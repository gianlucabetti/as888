000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090310     ffiapd01l  if   e           k disk
000300090318     faitra03l  if   e           k disk    prefix(£)
000400090316     fansog08j  if   e           k disk
000500090311     Ftrmz96p   O    e             PRINTER OFLIND(*IN99)
000600090313     f                                     usropn
000700070731     d* Parametri x Controllo profilo utenti
000800070731     d TIBS34ds      e ds
000900070731     d AZUTEds       e ds                  extname(AZUTE00F)
001000090311     d DDATIUTE      e ds
001100090311     d ansogds       e ds                  extname(Ansog00f)
001200090311     d aitrads       e ds                  extname(Aitra00f)
001300090311     d fifttds       e ds                  extname(fiftt00f)
001400090729     d fitgtds       e ds                  extname(fitgt00f)
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
003600090724     c                   exsr      sraitra2
003700090311     c                   exsr      sraitra7
003800090311     c                   exsr      sraitra10
003900090318     c* automezzi
004000090403     c*                  exsr      sraitra12
004100090318     c* qualità
004200090423     c*                  exsr      sraitra13
004300090311     c                   exsr      srproj
004400090313     c                   exsr      sraitra5
004500090310     c                   seton                                        lr
004600090720     C**********************************************************************
004700090724     C     sraitra2      BEGSR
004800090720     C**********************************************************************
004900090720     c                   seton                                        99
005000090723     c* se tutti gli aut disaccreditati segnalo se contratto non ancora
005100090723     c* chiuso
005200090720     C/EXEC SQL
005300090724     C+ DECLARE A2 CURSOR FOR SELECT tranrc, AITRA00F.TRASOC, min(tradfi)
005400090724     C+ FROM aitra00f join aitrs00f on tranrc =trsnrc WHERE trsdfc = 0
005500090724     C+ GROUP BY tranrc, trasoc HAVING min(tradfi) >0
005600090720     C/END-EXEC
005700090720     C/EXEC SQL
005800090724     C+ OPEN A2
005900090720     C/END-EXEC
006000090720
006100090720     C                   do        *hival
006200090720
006300090720     C/EXEC SQL
006400090724     C+ FETCH NEXT FROM A2 INTO :tranrc, :trasoc, :tradfi
006500090720     C/END-EXEC
006600090720
006700090720     C                   select
006800090720     c* fine file
006900090720     C                   when      sqlcod = 100
007000090720     c                   leave
007100090720     c* dettaglio
007200090720     C                   when      sqlcod = 0
007300090724     c                   seton                                        02
007400090720     c                   exsr      srstampa
007500090720     C                   other
007600090720     C                   exsr      sqlerror
007700090720     C                   endsl
007800090720     C*
007900090720     C                   enddo
008000090720
008100090720     C/EXEC SQL
008200090724     C+ CLOSE A2
008300090720     C/END-EXEC
008400090720     c                   endsr
008500090723     C**********************************************************************
008600090723     C     sraitra5      BEGSR
008700090723     C**********************************************************************
008800090723     c                   seton                                        99
008900090723     c* se valorizza e non esiste in AITRA
009000090723     C/EXEC SQL
009100090723     C+ DECLARE A5 CURSOR FOR SELECT fttpdr, max(fttddc) FROM fiftt00f
009200090723     C+ exception join aitra00f on trakau = fttpdr WHERE ftttsr = ' ' and
009300090723     C+ fttfvl = 'C' GROUP BY fttpdr ORDER BY fttpdr
009400090723     C/END-EXEC
009500090723     C/EXEC SQL
009600090723     C+ OPEN A5
009700090723     C/END-EXEC
009800090723
009900090723     C                   do        *hival
010000090723
010100090723     C/EXEC SQL
010200090723     C+ FETCH NEXT FROM A5 INTO :fttpdr, :fttddc
010300090723     C/END-EXEC
010400090723
010500090723     C                   select
010600090723     c* fine file
010700090723     C                   when      sqlcod = 100
010800090723     c                   leave
010900090723     c* dettaglio
011000090723     C                   when      sqlcod = 0
011100090723     c                   eval      comkau = fttpdr
011200090723     c                   exsr      srapd
011300090723     c                   if        apdatb <> ' '
011400090723     c                   iter
011500090723     c                   end
011600090723     c                   movel     fttpdr        comfil            3 0
011700090723     c                   if        savfil <> comfil
011800090723     c                   if        %open(trmz96p)
011900090723     c                   close     trmz96p
012000090723     c                   end
012100090723     c                   if        not %open(trmz96p)
012200090724     c                   z-add     1             page
012300090723     c                   open      trmz96p
012400090723     c                   end
012500090723     c                   seton                                        99
012600090723     c                   move      comfil        savfil            3 0
012700090723     c                   end
012800090723     c                   if        fttddc > 0
012900090723     c                   move      fttddc        dtaiso
013000090723     c                   move      dtaiso        dtaeur
013100090723     c                   move      dtaeur        stpddc
013200090723     c                   else
013300090723     c                   clear                   stpddc
013400090723     c                   end
013500090723     c                   seton                                        05
013600090723     c                   exsr      srstampa
013700090723     C                   other
013800090723     C                   exsr      sqlerror
013900090723     C                   endsl
014000090723     C*
014100090723     C                   enddo
014200090723
014300090723     C/EXEC SQL
014400090723     C+ CLOSE A5
014500090723     C/END-EXEC
014600090723     c                   endsr
014700090311     C**********************************************************************
014800090311     C     sraitra7      BEGSR
014900090311     C**********************************************************************
015000090311     c                   seton                                        99
015100090310     c* se valorizza ed è disaccreditato
015200090310     C/EXEC SQL
015300090724     C+ DECLARE A7 CURSOR FOR SELECT trakau, traaut, max(tradfi),
015400090724     C+ max(fttddc), min(tradfi) FROM aitrs00f join aitra00f on tranrc=
015500090724     C+ trsnrc join fiftt00f on trakau = fttpdr join anfrn00f on
015600090724     C+ fttsoc=frnsocieta and fttcdf = frnksc join ansog00f on frnsogg=
015700090724     C+ sogsogg WHERE TRASCO = ' ' and traann =' ' and trakau <> 0 and
015800090724     C+ ftttsr = ' ' and fttfvl='C' and fttsoc = trssocg and
015900090724     C+ sogpartiva=trsiva GROUP BY trakau, traaut HAVING min(tradfi) <> 0
016000090724     C+ and max(tradfi)< max(fttddc) ORDER BY trakau, traaut
016100090310     C/END-EXEC
016200090311     C/EXEC SQL
016300090311     C+ OPEN A7
016400090311     C/END-EXEC
016500090311
016600090311     C                   do        *hival
016700090311
016800090311     C/EXEC SQL
016900090724     C+ FETCH NEXT FROM A7 INTO :trakau, :traaut, :tradfi, :fttddc, :mindfi
017000090311     C/END-EXEC
017100090311
017200090311     C                   select
017300090311     c* fine file
017400090311     C                   when      sqlcod = 100
017500090311     c                   leave
017600090311     c* dettaglio
017700090311     C                   when      sqlcod = 0
017800090403     C                   eval      comkau = trakau
017900090403     c                   exsr      srapd
018000090403     c                   if        apdatb <> ' '
018100090403     c                   iter
018200090403     c                   end
018300090318     c                   clear                   ok                1
018400090318     c* verifico di non avere lo stesso codice anche accreditato
018500090318     c     trakau        setll     aitra03l
018600090318     c                   do        *hival
018700090318     c     trakau        reade     aitra03l
018800090318     c                   if        %eof(aitra03l)
018900090318     c                   leave
019000090318     c                   end
019100090318     c                   if        £tradfi = 0
019200090318     c                   eval      ok = '1'
019300090318     c                   leave
019400090318     c                   end
019500090318     c                   enddo
019600090318     c                   if        ok = '1'
019700090318     c                   iter
019800090318     c                   end
019900090318     c                   seton                                        07
020000090724     c                   if        tradfi > 0
020100090724     c                   move      tradfi        dtaiso
020200090312     c                   move      dtaiso        dtaeur
020300090724     c                   move      dtaeur        stpdfi
020400090312     c                   else
020500090724     c                   clear                   stpdfi
020600090312     c                   end
020700090724     c                   if        fttddc > 0
020800090724     c                   move      fttddc        dtaiso
020900090724     c                   move      dtaiso        dtaeur
021000090724     c                   move      dtaeur        stpddc
021100090724     c                   else
021200090724     c                   clear                   stpddc
021300090724     c                   end
021400090311     c                   exsr      srstampa
021500090311     C                   other
021600090311     C                   exsr      sqlerror
021700090311     C                   endsl
021800090311     C*
021900090311     C                   enddo
022000090311
022100090311     C/EXEC SQL
022200090311     C+ CLOSE A7
022300090311     C/END-EXEC
022400090311     c                   endsr
022500090311     C**********************************************************************
022600090311     C     sraitra10     BEGSR
022700090311     C**********************************************************************
022800090311     c                   seton                                        99
022900090729     c* controllo che i codici aut accreditati valorizzino e abbiano tariffe
023000090310     C/EXEC SQL
023100090727     C+ DECLARE A10 CURSOR FOR SELECT trakau, AITRA00F.TRADIN FROM
023200090729     C+ aitra00f exception join fiftt00f on trakau = fttpdr LEFT OUTER
023300090729     C+ JOIN FITGT00F on trakau = TGTpdr and tgtsml = ' ' WHERE TGTDDT >=
023400090729     C+ TRADIN AND TRASCO = ' ' and traann =' ' and trakau <> 0 and
023500090729     C+ tradin > 0 and tradfi = 0 GROUP BY trakau, tradin, tgtdst,
023600090729     C+ tgtdcn, tgtdrc ORDER BY trakau, tradin
023700090310     C/END-EXEC
023800090310
023900090310     C/EXEC SQL
024000090311     C+ OPEN A10
024100090310     C/END-EXEC
024200090310
024300090310     C                   do        *hival
024400090310
024500090310     C/EXEC SQL
024600090729     C+ FETCH NEXT FROM A10 INTO :trakau, :tradin, :tgtdst, :tgtdcn, :tgtdrc
024700090310     C/END-EXEC
024800090310
024900090310     C                   select
025000090310     c* fine file
025100090310     C                   when      sqlcod = 100
025200090310     c                   leave
025300090310     c* dettaglio
025400090310     C                   when      sqlcod = 0
025500090727     c                   if        tradin > 0
025600090727     c                   move      tradin        dtaiso
025700090727     c                   move      dtaiso        dtaeur
025800090727     c                   move      dtaeur        stpdac
025900090727     c                   else
026000090727     c                   clear                   stpdac
026100090727     c                   end
026200090729     c                   if        tgtdst > 0
026300090729     c                   move      tgtdst        dtaiso
026400090729     c                   move      dtaiso        dtaeur
026500090729     c                   move      dtaeur        stpdst
026600090729     c                   else
026700090729     c                   clear                   stpdst
026800090729     c                   end
026900090729     c                   if        tgtdcn > 0
027000090729     c                   move      tgtdcn        dtaiso
027100090729     c                   move      dtaiso        dtaeur
027200090729     c                   move      dtaeur        stpdcn
027300090729     c                   else
027400090729     c                   clear                   stpdcn
027500090729     c                   end
027600090729     c                   if        tgtdrc > 0
027700090729     c                   move      tgtdrc        dtaiso
027800090729     c                   move      dtaiso        dtaeur
027900090729     c                   move      dtaeur        stpdrc
028000090729     c                   else
028100090729     c                   clear                   stpdrc
028200090729     c                   end
028300090311     c                   seton                                        10
028400090311     C                   eval      comkau = trakau
028500090311     c                   exsr      srapd
028600090310     c                   exsr      srstampa
028700090310     C                   other
028800090310     C                   exsr      sqlerror
028900090310     C                   endsl
029000090310     C*
029100090310     C                   enddo
029200090310
029300090310     C/EXEC SQL
029400090311     C+ CLOSE A10
029500090310     C/END-EXEC
029600090310     c                   endsr
029700020709     C**********************************************************************
029800090311     C     srproj        BEGSR
029900020709     C**********************************************************************
030000090311     c                   seton                                        99
030100090311     c* se valorizza reperisco il codice fornitore aggancio proj e verifico
030200090720     c* se partita iva coincide con quella presente su AITRS
030300090311     C/EXEC SQL
030400090727     C+ DECLARE P1 CURSOR FOR SELECT fttsoc, fttcdf, SOGPARTIVA, SOGDES,
030500090727     C+ max(fttddc), count(*) FROM fiftt00f join fiapd00f on apdpdr =
030600090727     C+ fttpdr join anfrn00f on frnsocieta = apdcsf and substr(frnksc, 2,
030700090727     C+ 7) = digits(apdksc) join ansog00f on frnsogg= sogsogg exception
030800090727     C+ join aitrs00f on fttsoc = trssocg and trsiva = sogpartiva WHERE
030900090727     C+ ftttsr = ' ' and fttfvl = 'C' and apdcsf = fttsoc and apdksc =
031000090727     C+ fttcdf and apdtip = 'A' and apdatb = ' ' GROUP BY fttsoc, fttcdf,
031100090727     C+ SOGPARTIVA, SOGDES ORDER BY fttsoc, fttcdf, SOGPARTIVA, SOGDES
031200090311     C/END-EXEC
031300090311     C/EXEC SQL
031400090311     C+ OPEN p1
031500090311     C/END-EXEC
031600090311
031700090311     C                   do        *hival
031800090311
031900090311     C/EXEC SQL
032000090727     C+ FETCH NEXT FROM p1 INTO :fttsoc, :fttcdf, :SOGPARTIVA, :SOGDES,
032100090727     C+ :fttddc, :nrg
032200090311     C/END-EXEC
032300090311
032400090311     C                   select
032500090311     c* fine file
032600090311     C                   when      sqlcod = 100
032700090311     c                   leave
032800090311     c* dettaglio
032900090311     C                   when      sqlcod = 0
033000090727     c                   eval      stpnrg =nrg
033100090727     c                   if        fttddc > 0
033200090727     c                   move      fttddc        dtaiso
033300090727     c                   move      dtaiso        dtaeur
033400090727     c                   move      dtaeur        stpddc
033500090727     c                   else
033600090727     c                   clear                   stpddc
033700090727     c                   end
033800090311     c                   seton                                        11
033900090311     c                   exsr      srstampa
034000090311     C                   other
034100090311     C                   exsr      sqlerror
034200090311     C                   endsl
034300090311     C*
034400090311     C                   enddo
034500090311
034600090311     C/EXEC SQL
034700090311     C+ CLOSE p1
034800090311     C/END-EXEC
034900090311     c                   endsr
035000090311     C**********************************************************************
035100090311     C     srstampa      BEGSR
035200090311     C**********************************************************************
035300070921     C*
035400090311     c                   if        *in99
035500090311     c                   write     testa
035600090311     c   01              write     testa1
035700090311     c   02              write     testa2
035800090311     c   03              write     testa3
035900090311     c   04              write     testa4
036000090311     c   05              write     testa5
036100090311     c   06              write     testa6
036200090311     c   07              write     testa7
036300090311     c   08              write     testa8
036400090311     c   09              write     testa9
036500090311     c   10              write     testa10
036600090311     c   11              write     testa11
036700090316     c   12              write     testa12
036800090311     c                   setoff                                       99
036900090310     c                   end
037000090311     c*
037100090311     c*
037200090311     c   01              write     riga1
037300090311     c   02              write     riga2
037400090311     c   03              write     riga3
037500090311     c   04              write     riga4
037600090312     c   05              write     riga5
037700090311     c   06              write     riga1
037800090312     c   07              write     riga7
037900090311     c   08              write     riga1
038000090311     c   09              write     riga1
038100090727     c   10              write     riga10
038200090311     c   11              write     riga11
038300090316     c   12              write     riga12
038400090311     c   20              write     riga20
038500090311     c*
038600090317     c                   movea     '000000000000'*in
038700090311     c                   setoff                                       20
038800070921     C*
038900070921     C                   ENDSR
039000090311     C**********************************************************************
039100090311     C     srapd         begsr
039200090311     C**********************************************************************
039300090311     c     kapd          chain     fiapd01l
039400090311     c                   if        %found(fiapd01l)
039500090311     c                   eval      stprsf = apdrsf
039600090311     c                   if        apdatb <>' '
039700090311     c                   eval      stpann = 'ANNULLATO'
039800090311     c                   else
039900090311     c                   clear                   stpann
040000090311     c                   end
040100090311     c                   else
040200090311     c                   clear                   stpann
040300090311     c                   eval       stprsf = *all'?'
040400090311     c                   end
040500090311     C                   ENDSR
040600090311     C**********************************************************************
040700090311     C     sqlerror      begsr
040800090311     C**********************************************************************
040900081105     C*
041000081105     C                   call      'X66CHGJOB'
041100081105     C                   seton                                        lr
041200081105     C                   ENDSR
041300070921     C**********************************************************************
041400070921     C     *inzsr        BEGSR
041500070921     C**********************************************************************
041600070921     c     *entry        plist
041700070730     c                   parm                    kpjba
041800090311     c                   eval      nompgm = 'TRMZ96R'
041900090311     c                   eval      cmp132 = *all'-'
042000090311     C                   TIME                    TIMES             6 0
042100090311     C                   TIME                    WHDAT            12 0
042200090311     C                   MOVE      WHDAT         WOGGI             6 0
042300070731      * Reperisco dati job
042400070731     c                   exsr      DatiJob
042500090311     c*
042600090310     C     Kapd          KLIST                                                  *
042700090310     C                   KFLD                    apdtip                         *
042800090311     C                   KFLD                    comkau            7 0          *
042900090311     c                   eval      apdtip = 'A'
043000090311     c                   seton                                        99
043100020709     C                   ENDSR
043200070731      *---------------------------------------------------------------*
043300070731      * Reperimento Dati del job (Utente/Operativi)                   *
043400070731      *---------------------------------------------------------------*
043500070731     c     DatiJob       BEGSR
043600070731      *
043700070731     c     *dtaara       define    §azute        azuteds
043800070731     c     *dtaara       define    §datiute      ddatiute
043900070731      *
044000070731     c                   in(E)     *dtaara
044100070731     c                   IF        %ERROR or RSUT = *blanks
044200070731     c                   clear                   Tibs34Ds
044300070731     c                   call      'TIBS34R'
044400070731     c                   parm                    Tibs34Ds
044500070731     c                   in        *dtaara
044600070731     c                   ENDIF
044700070731      *
044800070731     c                   ENDSR
