001000040107      *---------------------------------------------------------------*
001100180306      * Distribuzione di febbraio 2018
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001801180306     ftitas38c  if   e           k Disk
001900180306     fwfdst00f  o    e             disk
002200090828     ffnlbl01l  if   e           k disk
002800040112      *
002900040107      *---------------------------------------------------------------*
003000040107
003100040107      *
003200040107      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
003300040107      *
003400040108     d K_tit1          c                   const('  IMMISSIONE  ')
003500040107      *
003600040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003700040107      *
003800180306     d sccm            S              7  0 DIM(10000) inz
003900180306     d stsp            S              8  0 DIM(10000) inz
004000180306     d stspA           S              8  0 DIM(10000) inz
004100180306     d stspB           S              8  0 DIM(10000) inz
004300100615     d
004800040107      *
004900040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005000040107      *
005100090922     d Tibs34ds      e ds                  inz
005200090922     d §azute        e ds                  Extname(Azute00f)
005300090922     d                                     dtaara
005400090922     d §Datiute      e ds                  EXTname(ddatiute)
005500090922     d                                     dtaara
007500090828     d tibs10ds      e ds
007700090828     d TIBS69DS      E DS                  INZ
007800180306     d DS_cnaco      E DS                  extname(CNACO00F)
007900180306     d DS_cnind      E DS                  extname(CNIND00F)
008000180306     d DS_cnclp      E DS                  extname(CNCLP00F)
008100180306     d DS_fncls      E DS                  extname(FNCLS00F)
008200040123      *
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009500040107      *
010700040213      * - Indici di schiera / Contatori
010800180306     d ind             s              5  0 inz
011000040109      *
011100040109      * - Campi di comodo
011200180306     d wccm            s                   like(tasccm)
018700090828      *
018800090828      * - Flags booleani
018900090828     d $Finerec        s              1    inz(*off)
019000081023
019100090402      // ? PROTOTIPI ?
019200090402      /copy gaitrasrc/srcprotopr,tibs34r
019400090828      /copy gaitrasrc/srcprotopr,tibs10r
019500090828      /copy gaitrasrc/srcprotopr,tibs69r
019700040107
019800040107      *===============================================================*
019900040107      *  RIEPILOGO INDICATORI                                         *
020000040107      *---------------------------------------------------------------*
020100090827      *  XX    -
020200040107      *===============================================================*
020300040107
020600090827     c
020800040123      *
020801180306        setll (2018:0201) titas38c;
020802180306        reade (2018) titas38c;
020803180306        dow not %eof(titas38c) and tasmgs<=0228;
020804180306        // scarto se non è distribuzione
020805180306         if tastsp<>'D' ;
020806180306            reade 2018 titas38c;
020807180306            iter;
020808180306         endif;
020809180306        // solo F1 o A2
020810180306         if tastbl<> 'F1' and tastbl <> 'A2';
020811180306            reade 2018 titas38c;
020812180306            iter;
020813180306         endif;
020814180306        // Scarto bolle di reso
020815180306         if tasfbr<>' ';
020816180306            reade 2018 titas38c;
020817180306            iter;
020818180306         endif;
020819180306        // scarto se bolla figlia
020820180306           setll (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l;
020821180306           if %equal(fnlbl01l);
020822180306              reade 2018 titas38c;
020823180306              iter;
020824180306           endif;
020825180306        // determino codice cliente unificante
020826180306           exsr sr_tibs10;
020827180306     c                   move      d10cop        wccm
020828180306           ind= %lookup(wccm:sccm);
020829180306           if ind=0;
020830180306              ind= %lookup(0000000:sccm);
020831180306           endif;
020832180306           sccm(ind)=wccm;
020833180306           stsp(ind)+=1;
020834180306           if tasduc=tasdbr;
020835180306              stspA(ind)+=1;
020836180306           else;
020837180306              stspB(ind)+=1;
020838180306           endif;
020839180306           reade 2018 titas38c;
020840180306        enddo;
020841180306        // finito il ciclo scarico le schiere in file di work
020842180306        for ind=1 to %elem(sccm);
020843180306           if sccm(ind)=0;
020844180306              leave;
020845180306           endif;
020846180306        // reperisco decodifica
020847180306           clear DS_cnaco;
020848180306           clear DS_cnind;
020849180306           clear DS_cnclp;
020850180306           clear DS_fncls;
020851180306           I69kac=sccm(ind);
020852180306           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
020853180306              dstccm=sccm(ind);
020854180306              dstrag=acorag;
020855180306              dsttsp=stsp(ind);
020856180306              dsttspA=stspa(ind);
020857180306              dsttspB=stspb(ind);
020858180306              write wfdst000;
020859180306        endfor;
021100090610
021101180306        *inlr = *on;
026400040108
026401180306     C*------------------------------------------------------------------------*
026402180306     C* Ricerca codice padre del legame "ST"
026403180306     C*------------------------------------------------------------------------*
026404180306     C     sr_tibs10     BEGSR
026406180306     c* verifico se codice presente come Padre
026407180306     c                   clear                   tibs10ds
026408180306     c                   eval      d10tle='ST'
026409180306     c                   eval      d10paf='F'
026410180306     c                   eval      d10cod=tasccm
026411180306     c                   call      'TIBS10R'
026412180306     c                   parm                    tibs10ds
026413180306     c                   if        d10cop>0
026414180306     c                   leavesr
026415180306     c                   endif
026416180306     c* verifico se codice presente come Figlio
026417180306     c                   eval      d10paf='P'
026418180306     c                   call      'TIBS10R'
026419180306     c                   parm                    tibs10ds
026420180306     c                   if        d10cop>0
026421180306     c                   leavesr
026422180306     c                   endif
026423180306     c* Se non è nè padre nè figlio è Singolo
026424180306     c                   eval      d10cop=d10cod
026426180306     c
026427180306     c                   endsr
026500090402       //-------------------------------------------------------------*
026600090402       //Routine Iniziale                                             *
026700090402       //-------------------------------------------------------------*
026800090414       begsr *inzsr;
026900090402
027000090402       //Reperisco dati job
027100090917         exsr datijob;
028400090827       //vtcpgm=SDSpgm;
029900090917
030500090402
030600090402       EndSr;
030700040108
035000090402       //-------------------------------------------------------------*
035100090402       //Reperimento Dati del job (Utente/Operativi)                  *
035200090402       //-------------------------------------------------------------*
035300090918       Begsr DatiJob;
035400090403
035500090917          in(e) §azute;
035600090917          if not %error;
035700090917           in(e) §datiute;
035800090917          endif;
035900090917          if %error or rsut = *blanks;
036000090917           tibs34r(tibs34ds);
036100090917           in §azute;
036200090917           in §datiute;
036300090917          endif;
036600090917         endsr;
