000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090310     ffiapd01l  if   e           k disk
000300090420     faitra05l  if   e           k disk    prefix(£)
000400090316     fansog08j  if   e           k disk
000500090311     Ftrmz96p   O    e             PRINTER OFLIND(*IN99)
000600090313     f                                     usropn
000700070731     d* Parametri x Controllo profilo utenti
000800070731     d TIBS34ds      e ds
000900070731     d AZUTEds       e ds                  extname(AZUTE00F)
001000090311     d DDATIUTE      e ds
001100090311     d ansogds       e ds                  extname(Ansog00f)
001200090311     d aitrads       e ds                  extname(Aitra00f)
001300090420     d fiattds       e ds                  extname(fiatt00f)
001400090729     d fiadtds       e ds                  extname(fiadt00f)
001500090316     d kpjba         e ds
001600090325     d minmean         s              4
001700090325     d maxmean         s              4
001800090325     d mincafi         s              1
001900090325     d maxcafi         s              1
002000090316     d minatp          s                   like(traatp)
002100090316     d maxatp          s                   like(traatp)
002200090316     d mindp4          s                   like(tradp4)
002300090316     d maxdp4          s                   like(tradp4)
002400090316     d mincor          s                   like(tracor)
002500090316     d maxcor          s                   like(tracor)
002600160113     d*** mindec          s                   like(tradec)
002700160113     d*** maxdec          s                   like(tradec)
002800160113     d*** mindfc          s                   like(tradfc)
002900160113     d*** maxdfc          s                   like(tradfc)
003000090724     d mindfi          s                   like(tradfi)
003100090727     d nrg             s              7  0
003200090727     d savcar          s              3  0
003300090312     d dtaiso          s               d
003400090312     d dtaeur          s               d   datfmt(*eur)
003500090313     c                   open      trmz96p
003600090720     c                   exsr      sraitra7
003700090311     c                   exsr      sraitra10
003800090311     c                   exsr      srproj
003900090313     c                   exsr      sraitra5
004000090310     c                   seton                                        lr
004100090311     C**********************************************************************
004200090311     C     sraitra5      BEGSR
004300090311     C**********************************************************************
004400090311     c                   seton                                        99
004500090310     c* se valorizza e non esiste in AITRA
004600090310     C/EXEC SQL
004700090420     C+ DECLARE A5 CURSOR FOR SELECT attpdr, max(attddc) FROM fiatt00f
004800090420     C+ exception join aitra00f on trabpt = attpdr WHERE FIATT00F.ATTDCO
004900121004     C+ <> 0 and attddc > 20090100 and attsoc <> '219'
005000121004     C+ GROUP BY attpdr ORDER BY attpdr
005100090310     C/END-EXEC
005200090311     C/EXEC SQL
005300090311     C+ OPEN A5
005400090311     C/END-EXEC
005500090311
005600090311     C                   do        *hival
005700090311
005800090311     C/EXEC SQL
005900090420     C+ FETCH NEXT FROM A5 INTO :attpdr, :attddc
006000090311     C/END-EXEC
006100090311
006200090311     C                   select
006300090311     c* fine file
006400090311     C                   when      sqlcod = 100
006500090311     c                   leave
006600090311     c* dettaglio
006700090311     C                   when      sqlcod = 0
006800090420     c                   eval      comkau = attpdr
006900090403     c                   exsr      srapd
007000090403     c                   if        apdatb <> ' '
007100090403     c                   iter
007200090403     c                   end
007300090420     c                   movel     attpdr        comfil            3 0
007400090312     c                   if        savfil <> comfil
007500090313     c                   if        %open(trmz96p)
007600090420     c                   close     trmz96p
007700090313     c                   end
007800090420     c                   if        not %open(trmz96p)
007900090724     c                   z-add     1             page
008000090420     c                   open      trmz96p
008100090313     c                   end
008200090312     c                   seton                                        99
008300090312     c                   move      comfil        savfil            3 0
008400090312     c                   end
008500090420     c                   if        attddc > 0
008600090420     c                   move      attddc        dtaiso
008700090312     c                   move      dtaiso        dtaeur
008800090312     c                   move      dtaeur        stpddc
008900090312     c                   else
009000090312     c                   clear                   stpddc
009100090312     c                   end
009200090311     c                   seton                                        05
009300090311     c                   exsr      srstampa
009400090311     C                   other
009500090311     C                   exsr      sqlerror
009600090311     C                   endsl
009700090311     C*
009800090311     C                   enddo
009900090311
010000090311     C/EXEC SQL
010100090311     C+ CLOSE A5
010200090311     C/END-EXEC
010300090311     c                   endsr
010400090720     C**********************************************************************
010500090720     C     sraitra7      BEGSR
010600090720     C**********************************************************************
010700090720     c                   seton                                        99
010800090720     c* se valorizza ed è disaccreditato
010900090720     C/EXEC SQL
011000090724     C+ DECLARE A7 CURSOR FOR SELECT trabpt, traaut, max(tradfi),
011100090724     C+ max(attddc), min(tradfi) FROM aitrs00f join aitra00f on tranrc=
011200090724     C+ trsnrc join fiatt00f on trabpt = attpdr join anfrn00f on
011300090724     C+ attsoc=frnsocieta and attcdf = frnksc join ansog00f on frnsogg=
011400090724     C+ sogsogg WHERE TRASCO = ' ' and traann =' ' and trabpt <> 0 and
011500121004     C+ attdco<> 0 and attsoc = trssocg and sogpartiva=trsiva and attsoc
011600121004     C+ <> '219' group by
011700090724     C+ trabpt, traaut HAVING min(tradfi) <> 0 and max(tradfi)<
011800090724     C+ max(attddc) ORDER BY trabpt, traaut
011900090720     C/END-EXEC
012000090720     C/EXEC SQL
012100090720     C+ OPEN A7
012200090720     C/END-EXEC
012300090720
012400090720     C                   do        *hival
012500090720
012600090720     C/EXEC SQL
012700090724     C+ FETCH NEXT FROM A7 INTO :trakau, :traaut, :tradfi, :attddc, :mindfi
012800090720     C/END-EXEC
012900090720
013000090720     C                   select
013100090720     c* fine file
013200090720     C                   when      sqlcod = 100
013300090720     c                   leave
013400090720     c* dettaglio
013500090720     C                   when      sqlcod = 0
013600090720     C                   eval      comkau = trakau
013700090720     c                   exsr      srapd
013800090720     c                   if        apdatb <> ' '
013900090720     c                   iter
014000090720     c                   end
014100090720     c                   clear                   ok                1
014200090720     c* verifico di non avere lo stesso codice anche accreditato
014300090724     c     trakau        setll     aitra05l
014400090720     c                   do        *hival
014500090724     c     trakau        reade     aitra05l
014600090724     c                   if        %eof(aitra05l)
014700090720     c                   leave
014800090720     c                   end
014900090720     c                   if        £tradfi = 0
015000090720     c                   eval      ok = '1'
015100090720     c                   leave
015200090720     c                   end
015300090720     c                   enddo
015400090720     c                   if        ok = '1'
015500090720     c                   iter
015600090720     c                   end
015700090720     c                   seton                                        07
015800090724     c                   if        tradfi > 0
015900090724     c                   move      tradfi        dtaiso
016000090720     c                   move      dtaiso        dtaeur
016100090724     c                   move      dtaeur        stpdfi
016200090720     c                   else
016300090724     c                   clear                   stpdfi
016400090720     c                   end
016500090724     c                   if        attddc > 0
016600090724     c                   move      attddc        dtaiso
016700090724     c                   move      dtaiso        dtaeur
016800090724     c                   move      dtaeur        stpddc
016900090724     c                   else
017000090724     c                   clear                   stpddc
017100090724     c                   end
017200090720     c                   exsr      srstampa
017300090720     C                   other
017400090720     C                   exsr      sqlerror
017500090720     C                   endsl
017600090720     C*
017700090720     C                   enddo
017800090720
017900090720     C/EXEC SQL
018000090720     C+ CLOSE A7
018100090720     C/END-EXEC
018200090720     c                   endsr
018300090311     C**********************************************************************
018400090311     C     sraitra10     BEGSR
018500090311     C**********************************************************************
018600090311     c                   seton                                        99
018700090310     c* controllo che i codici aut accreditati valorizzino
018800090310     C/EXEC SQL
018900090729     C+ DECLARE A10 CURSOR FOR SELECT trabpt, tradin, adtdst, adtdcn,
019000090729     C+ adtdrc FROM aitra00f exception join fiatt00f on trabpt = attpdr
019100090729     C+ left outer join fiadt00f on adtpdr = trabpt WHERE TRASCO = ' '
019200090729     C+ and traann =' ' and trabpt <> 0 and tradin > 0 and tradfi = 0 and
019300121004     C+ adtddt >= tradin and attsoc <> '219'
019400121004     C+ GROUP BY trabpt, tradin, adtdst, adtdcn, adtdrc
019500090729     C+ ORDER BY trabpt, tradin
019600090310     C/END-EXEC
019700090310
019800090310     C/EXEC SQL
019900090311     C+ OPEN A10
020000090310     C/END-EXEC
020100090310
020200090310     C                   do        *hival
020300090310
020400090310     C/EXEC SQL
020500090727     C+ FETCH NEXT FROM A10 INTO :trabpt, :tradin
020600090310     C/END-EXEC
020700090310
020800090310     C                   select
020900090310     c* fine file
021000090310     C                   when      sqlcod = 100
021100090310     c                   leave
021200090310     c* dettaglio
021300090310     C                   when      sqlcod = 0
021400090727     c                   if        tradin > 0
021500090727     c                   move      tradin        dtaiso
021600090727     c                   move      dtaiso        dtaeur
021700090727     c                   move      dtaeur        stpdac
021800090727     c                   else
021900090727     c                   clear                   stpdac
022000090727     c                   end
022100090729     c                   if        adtdst > 0
022200090729     c                   move      adtdst        dtaiso
022300090729     c                   move      dtaiso        dtaeur
022400090729     c                   move      dtaeur        stpdst
022500090729     c                   else
022600090729     c                   clear                   stpdst
022700090729     c                   end
022800090729     c                   if        adtdcn > 0
022900090729     c                   move      adtdcn        dtaiso
023000090729     c                   move      dtaiso        dtaeur
023100090729     c                   move      dtaeur        stpdcn
023200090729     c                   else
023300090729     c                   clear                   stpdcn
023400090729     c                   end
023500090729     c                   if        adtdrc > 0
023600090729     c                   move      adtdrc        dtaiso
023700090729     c                   move      dtaiso        dtaeur
023800090729     c                   move      dtaeur        stpdrc
023900090729     c                   else
024000090729     c                   clear                   stpdrc
024100090729     c                   end
024200090311     c                   seton                                        10
024300090420     C                   eval      comkau = trabpt
024400090311     c                   exsr      srapd
024500090310     c                   exsr      srstampa
024600090310     C                   other
024700090310     C                   exsr      sqlerror
024800090310     C                   endsl
024900090310     C*
025000090310     C                   enddo
025100090310
025200090310     C/EXEC SQL
025300090311     C+ CLOSE A10
025400090310     C/END-EXEC
025500090310     c                   endsr
025600020709     C**********************************************************************
025700090311     C     srproj        BEGSR
025800020709     C**********************************************************************
025900090311     c                   seton                                        99
026000090311     c* se valorizza reperisco il codice fornitore aggancio proj e verifico
026100090720     c* se partita iva coincide con quella presente su AITRS
026200090311     C/EXEC SQL
026300090727     C+ DECLARE P1 CURSOR FOR SELECT attsoc, attcdf, SOGPARTIVA, SOGDES,
026400090727     C+ max(attddc), count(*) FROM fiatt00f join fiapd00f on apdpdr =
026500090727     C+ attpdr join anfrn00f on frnsocieta = apdcsf and substr(frnksc, 2,
026600090727     C+ 7) = digits(apdksc) join ansog00f on frnsogg= sogsogg exception
026700090727     C+ join aitrs00f on attsoc = trssocg and trsiva = sogpartiva WHERE
026800090727     C+ attdco <> 0 and apdcsf = attsoc and apdksc = attcdf and apdtip =
026900121004     C+ 'D' and apdatb = ' ' and attsoc <> '219'
027000121004     C+ GROUP BY attsoc, attcdf, SOGPARTIVA, SOGDES
027100090727     C+ ORDER BY attsoc, attcdf, SOGPARTIVA, SOGDES
027200090311     C/END-EXEC
027300090311     C/EXEC SQL
027400090311     C+ OPEN p1
027500090311     C/END-EXEC
027600090311
027700090311     C                   do        *hival
027800090311
027900090311     C/EXEC SQL
028000090727     C+ FETCH NEXT FROM p1 INTO :attsoc, :attcdf, :SOGPARTIVA, :SOGDES,
028100090727     C+ :attddc, :nrg
028200090311     C/END-EXEC
028300090311
028400090311     C                   select
028500090311     c* fine file
028600090311     C                   when      sqlcod = 100
028700090311     c                   leave
028800090311     c* dettaglio
028900090311     C                   when      sqlcod = 0
029000090727     c                   eval      stpnrg = nrg
029100090727     c                   if        attddc > 0
029200090727     c                   move      attddc        dtaiso
029300090727     c                   move      dtaiso        dtaeur
029400090727     c                   move      dtaeur        stpddc
029500090727     c                   else
029600090727     c                   clear                   stpddc
029700090727     c                   end
029800090724     c                   eval      fttsoc = attsoc
029900090724     c                   eval      fttcdf = attcdf
030000090311     c                   seton                                        11
030100090311     c                   exsr      srstampa
030200090311     C                   other
030300090311     C                   exsr      sqlerror
030400090311     C                   endsl
030500090311     C*
030600090311     C                   enddo
030700090311
030800090311     C/EXEC SQL
030900090311     C+ CLOSE p1
031000090311     C/END-EXEC
031100090311     c                   endsr
031200090311     C**********************************************************************
031300090311     C     srstampa      BEGSR
031400090311     C**********************************************************************
031500070921     C*
031600090311     c                   if        *in99
031700090428     c                   write     testaa
031800090311     c   01              write     testa1
031900090311     c   02              write     testa2
032000090311     c   03              write     testa3
032100090311     c   04              write     testa4
032200090311     c   05              write     testa5
032300090311     c   06              write     testa6
032400090311     c   07              write     testa7
032500090311     c   08              write     testa8
032600090311     c   09              write     testa9
032700090311     c   10              write     testa10
032800090311     c   11              write     testa11
032900090316     c   12              write     testa12
033000090311     c                   setoff                                       99
033100090310     c                   end
033200090311     c*
033300090311     c*
033400090311     c   01              write     riga1
033500090311     c   02              write     riga2
033600090311     c   03              write     riga3
033700090311     c   04              write     riga4
033800090312     c   05              write     riga5
033900090311     c   06              write     riga1
034000090312     c   07              write     riga7
034100090311     c   08              write     riga1
034200090311     c   09              write     riga1
034300090727     c   10              write     riga10
034400090311     c   11              write     riga11
034500090316     c   12              write     riga12
034600090311     c   20              write     riga20
034700090311     c*
034800090317     c                   movea     '000000000000'*in
034900090311     c                   setoff                                       20
035000070921     C*
035100070921     C                   ENDSR
035200090311     C**********************************************************************
035300090311     C     srapd         begsr
035400090311     C**********************************************************************
035500090311     c     kapd          chain     fiapd01l
035600090311     c                   if        %found(fiapd01l)
035700090311     c                   eval      stprsf = apdrsf
035800090311     c                   if        apdatb <>' '
035900090311     c                   eval      stpann = 'ANNULLATO'
036000090311     c                   else
036100090311     c                   clear                   stpann
036200090311     c                   end
036300090311     c                   else
036400090311     c                   clear                   stpann
036500090311     c                   eval       stprsf = *all'?'
036600090311     c                   end
036700090311     C                   ENDSR
036800090311     C**********************************************************************
036900090311     C     sqlerror      begsr
037000090311     C**********************************************************************
037100081105     C*
037200081105     C                   call      'X66CHGJOB'
037300081105     C                   seton                                        lr
037400081105     C                   ENDSR
037500070921     C**********************************************************************
037600070921     C     *inzsr        BEGSR
037700070921     C**********************************************************************
037800070921     c     *entry        plist
037900070730     c                   parm                    kpjba
038000090311     c                   eval      nompgm = 'TRMZ96R'
038100090311     c                   eval      cmp132 = *all'-'
038200090311     C                   TIME                    TIMES             6 0
038300090311     C                   TIME                    WHDAT            12 0
038400090311     C                   MOVE      WHDAT         WOGGI             6 0
038500070731      * Reperisco dati job
038600070731     c                   exsr      DatiJob
038700090311     c*
038800090310     C     Kapd          KLIST                                                  *
038900090310     C                   KFLD                    apdtip                         *
039000090311     C                   KFLD                    comkau            7 0          *
039100090420     c                   eval      apdtip = 'D'
039200090311     c                   seton                                        99
039300020709     C                   ENDSR
039400070731      *---------------------------------------------------------------*
039500070731      * Reperimento Dati del job (Utente/Operativi)                   *
039600070731      *---------------------------------------------------------------*
039700070731     c     DatiJob       BEGSR
039800070731      *
039900070731     c     *dtaara       define    §azute        azuteds
040000070731     c     *dtaara       define    §datiute      ddatiute
040100070731      *
040200070731     c                   in(E)     *dtaara
040300070731     c                   IF        %ERROR or RSUT = *blanks
040400070731     c                   clear                   Tibs34Ds
040500070731     c                   call      'TIBS34R'
040600070731     c                   parm                    Tibs34Ds
040700070731     c                   in        *dtaara
040800070731     c                   ENDIF
040900070731      *
041000070731     c                   ENDSR
