000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000102090310     ffiapd01l  if   e           k disk
000103090318     faitra03l  if   e           k disk    prefix(£)
000104090316     fansog08j  if   e           k disk
000105090311     Ftrmz96p   O    e             PRINTER OFLIND(*IN99)
000106090313     f                                     usropn
000107070731     d* Parametri x Controllo profilo utenti
000108070731     d TIBS34ds      e ds
000109070731     d AZUTEds       e ds                  extname(AZUTE00F)
000110090311     d DDATIUTE      e ds
000111090311     d ansogds       e ds                  extname(Ansog00f)
000112090311     d aitrads       e ds                  extname(Aitra00f)
000113090311     d fifttds       e ds                  extname(fiftt00f)
000114090729     d fitgtds       e ds                  extname(fitgt00f)
000813090316     d kpjba         e ds
000814090325     d minmean         s              4
000815090325     d maxmean         s              4
000816090325     d mincafi         s              1
000817090325     d maxcafi         s              1
000818090316     d minatp          s                   like(traatp)
000819090316     d maxatp          s                   like(traatp)
000820090316     d mindp4          s                   like(tradp4)
000821090316     d maxdp4          s                   like(tradp4)
000822090316     d mincor          s                   like(tracor)
000823090316     d maxcor          s                   like(tracor)
000824090316     d mindec          s                   like(tradec)
000825090316     d maxdec          s                   like(tradec)
000826090311     d mindfc          s                   like(tradfc)
000827090311     d maxdfc          s                   like(tradfc)
000828090724     d mindfi          s                   like(tradfi)
000829090727     d nrg             s              7  0
000830090727     d savcar          s              3  0
000831090312     d dtaiso          s               d
000832090312     d dtaeur          s               d   datfmt(*eur)
000833090313     c                   open      trmz96p
000834090724     c                   exsr      sraitra2
000837090311     c                   exsr      sraitra7
000840090311     c                   exsr      sraitra10
000841090318     c* automezzi
000842090403     c*                  exsr      sraitra12
000843090318     c* qualità
000844090423     c*                  exsr      sraitra13
000845090311     c                   exsr      srproj
000846090313     c                   exsr      sraitra5
000847090310     c                   seton                                        lr
006720090720     C**********************************************************************
006721090724     C     sraitra2      BEGSR
006722090720     C**********************************************************************
006723090720     c                   seton                                        99
006724090723     c* se tutti gli aut disaccreditati segnalo se contratto non ancora
006725090723     c* chiuso
006726090720     C/EXEC SQL
006727090724     C+ DECLARE A2 CURSOR FOR SELECT tranrc, AITRA00F.TRASOC, min(tradfi)
006728090724     C+ FROM aitra00f join aitrs00f on tranrc =trsnrc WHERE trsdfc = 0
006729090724     C+ GROUP BY tranrc, trasoc HAVING min(tradfi) >0
006739090720     C/END-EXEC
006740090720     C/EXEC SQL
006741090724     C+ OPEN A2
006742090720     C/END-EXEC
006743090720
006744090720     C                   do        *hival
006745090720
006746090720     C/EXEC SQL
006747090724     C+ FETCH NEXT FROM A2 INTO :tranrc, :trasoc, :tradfi
006748090720     C/END-EXEC
006749090720
006750090720     C                   select
006751090720     c* fine file
006752090720     C                   when      sqlcod = 100
006753090720     c                   leave
006754090720     c* dettaglio
006755090720     C                   when      sqlcod = 0
006779090724     c                   seton                                        02
006780090720     c                   exsr      srstampa
006781090720     C                   other
006782090720     C                   exsr      sqlerror
006783090720     C                   endsl
006784090720     C*
006785090720     C                   enddo
006786090720
006787090720     C/EXEC SQL
006788090724     C+ CLOSE A2
006789090720     C/END-EXEC
006790090720     c                   endsr
006832090723     C**********************************************************************
006833090723     C     sraitra5      BEGSR
006834090723     C**********************************************************************
006835090723     c                   seton                                        99
006836090723     c* se valorizza e non esiste in AITRA
006837090723     C/EXEC SQL
006838090723     C+ DECLARE A5 CURSOR FOR SELECT fttpdr, max(fttddc) FROM fiftt00f
006839090723     C+ exception join aitra00f on trakau = fttpdr WHERE ftttsr = ' ' and
006840090723     C+ fttfvl = 'C' GROUP BY fttpdr ORDER BY fttpdr
006841090723     C/END-EXEC
006842090723     C/EXEC SQL
006843090723     C+ OPEN A5
006844090723     C/END-EXEC
006845090723
006846090723     C                   do        *hival
006847090723
006848090723     C/EXEC SQL
006849090723     C+ FETCH NEXT FROM A5 INTO :fttpdr, :fttddc
006850090723     C/END-EXEC
006851090723
006852090723     C                   select
006853090723     c* fine file
006854090723     C                   when      sqlcod = 100
006855090723     c                   leave
006856090723     c* dettaglio
006857090723     C                   when      sqlcod = 0
006858090723     c                   eval      comkau = fttpdr
006859090723     c                   exsr      srapd
006860090723     c                   if        apdatb <> ' '
006861090723     c                   iter
006862090723     c                   end
006863090723     c                   movel     fttpdr        comfil            3 0
006864090723     c                   if        savfil <> comfil
006865090723     c                   if        %open(trmz96p)
006866090723     c                   close     trmz96p
006867090723     c                   end
006868090723     c                   if        not %open(trmz96p)
006869090724     c                   z-add     1             page
006870090723     c                   open      trmz96p
006871090723     c                   end
006872090723     c                   seton                                        99
006873090723     c                   move      comfil        savfil            3 0
006874090723     c                   end
006875090723     c                   if        fttddc > 0
006876090723     c                   move      fttddc        dtaiso
006877090723     c                   move      dtaiso        dtaeur
006878090723     c                   move      dtaeur        stpddc
006879090723     c                   else
006880090723     c                   clear                   stpddc
006881090723     c                   end
006882090723     c                   seton                                        05
006883090723     c                   exsr      srstampa
006884090723     C                   other
006885090723     C                   exsr      sqlerror
006886090723     C                   endsl
006887090723     C*
006888090723     C                   enddo
006889090723
006890090723     C/EXEC SQL
006891090723     C+ CLOSE A5
006892090723     C/END-EXEC
006893090723     c                   endsr
006894090311     C**********************************************************************
006895090311     C     sraitra7      BEGSR
006896090311     C**********************************************************************
006897090311     c                   seton                                        99
006898090310     c* se valorizza ed è disaccreditato
006899090310     C/EXEC SQL
006900090724     C+ DECLARE A7 CURSOR FOR SELECT trakau, traaut, max(tradfi),
006901090724     C+ max(fttddc), min(tradfi) FROM aitrs00f join aitra00f on tranrc=
006902090724     C+ trsnrc join fiftt00f on trakau = fttpdr join anfrn00f on
006903090724     C+ fttsoc=frnsocieta and fttcdf = frnksc join ansog00f on frnsogg=
006904090724     C+ sogsogg WHERE TRASCO = ' ' and traann =' ' and trakau <> 0 and
006905090724     C+ ftttsr = ' ' and fttfvl='C' and fttsoc = trssocg and
006906090724     C+ sogpartiva=trsiva GROUP BY trakau, traaut HAVING min(tradfi) <> 0
006907090724     C+ and max(tradfi)< max(fttddc) ORDER BY trakau, traaut
006908090310     C/END-EXEC
006909090311     C/EXEC SQL
006910090311     C+ OPEN A7
006911090311     C/END-EXEC
006912090311
006913090311     C                   do        *hival
006914090311
006915090311     C/EXEC SQL
006916090724     C+ FETCH NEXT FROM A7 INTO :trakau, :traaut, :tradfi, :fttddc, :mindfi
006917090311     C/END-EXEC
006918090311
006919090311     C                   select
006920090311     c* fine file
006921090311     C                   when      sqlcod = 100
006922090311     c                   leave
006923090311     c* dettaglio
006924090311     C                   when      sqlcod = 0
006925090403     C                   eval      comkau = trakau
006926090403     c                   exsr      srapd
006927090403     c                   if        apdatb <> ' '
006928090403     c                   iter
006929090403     c                   end
006930090318     c                   clear                   ok                1
006931090318     c* verifico di non avere lo stesso codice anche accreditato
006932090318     c     trakau        setll     aitra03l
006933090318     c                   do        *hival
006934090318     c     trakau        reade     aitra03l
006935090318     c                   if        %eof(aitra03l)
006936090318     c                   leave
006937090318     c                   end
006938090318     c                   if        £tradfi = 0
006939090318     c                   eval      ok = '1'
006940090318     c                   leave
006941090318     c                   end
006942090318     c                   enddo
006943090318     c                   if        ok = '1'
006944090318     c                   iter
006945090318     c                   end
006946090318     c                   seton                                        07
006947090724     c                   if        tradfi > 0
006948090724     c                   move      tradfi        dtaiso
006949090312     c                   move      dtaiso        dtaeur
006950090724     c                   move      dtaeur        stpdfi
006951090312     c                   else
006952090724     c                   clear                   stpdfi
006953090312     c                   end
006954090724     c                   if        fttddc > 0
006955090724     c                   move      fttddc        dtaiso
006956090724     c                   move      dtaiso        dtaeur
006957090724     c                   move      dtaeur        stpddc
006958090724     c                   else
006959090724     c                   clear                   stpddc
006960090724     c                   end
006961090311     c                   exsr      srstampa
006962090311     C                   other
006963090311     C                   exsr      sqlerror
006964090311     C                   endsl
006965090311     C*
006966090311     C                   enddo
006967090311
006968090311     C/EXEC SQL
006969090311     C+ CLOSE A7
006970090311     C/END-EXEC
006971090311     c                   endsr
006972090311     C**********************************************************************
006973090311     C     sraitra10     BEGSR
006974090311     C**********************************************************************
006975090311     c                   seton                                        99
006976090729     c* controllo che i codici aut accreditati valorizzino e abbiano tariffe
006977090310     C/EXEC SQL
006978090727     C+ DECLARE A10 CURSOR FOR SELECT trakau, AITRA00F.TRADIN FROM
006979090729     C+ aitra00f exception join fiftt00f on trakau = fttpdr LEFT OUTER
006980090729     C+ JOIN FITGT00F on trakau = TGTpdr and tgtsml = ' ' WHERE TGTDDT >=
006981090729     C+ TRADIN AND TRASCO = ' ' and traann =' ' and trakau <> 0 and
006982090729     C+ tradin > 0 and tradfi = 0 GROUP BY trakau, tradin, tgtdst,
006983090729     C+ tgtdcn, tgtdrc ORDER BY trakau, tradin
006984090310     C/END-EXEC
006985090310
006986090310     C/EXEC SQL
006987090311     C+ OPEN A10
006988090310     C/END-EXEC
006989090310
006990090310     C                   do        *hival
006991090310
006992090310     C/EXEC SQL
006993090729     C+ FETCH NEXT FROM A10 INTO :trakau, :tradin, :tgtdst, :tgtdcn, :tgtdrc
006994090310     C/END-EXEC
006995090310
006996090310     C                   select
006997090310     c* fine file
006998090310     C                   when      sqlcod = 100
006999090310     c                   leave
007000090310     c* dettaglio
007001090310     C                   when      sqlcod = 0
007002090727     c                   if        tradin > 0
007003090727     c                   move      tradin        dtaiso
007004090727     c                   move      dtaiso        dtaeur
007005090727     c                   move      dtaeur        stpdac
007006090727     c                   else
007007090727     c                   clear                   stpdac
007008090727     c                   end
007009090729     c                   if        tgtdst > 0
007010090729     c                   move      tgtdst        dtaiso
007011090729     c                   move      dtaiso        dtaeur
007012090729     c                   move      dtaeur        stpdst
007013090729     c                   else
007014090729     c                   clear                   stpdst
007015090729     c                   end
007016090729     c                   if        tgtdcn > 0
007017090729     c                   move      tgtdcn        dtaiso
007018090729     c                   move      dtaiso        dtaeur
007019090729     c                   move      dtaeur        stpdcn
007020090729     c                   else
007021090729     c                   clear                   stpdcn
007022090729     c                   end
007023090729     c                   if        tgtdrc > 0
007024090729     c                   move      tgtdrc        dtaiso
007025090729     c                   move      dtaiso        dtaeur
007026090729     c                   move      dtaeur        stpdrc
007027090729     c                   else
007028090729     c                   clear                   stpdrc
007029090729     c                   end
007030090311     c                   seton                                        10
007031090311     C                   eval      comkau = trakau
007032090311     c                   exsr      srapd
007033090310     c                   exsr      srstampa
007034090310     C                   other
007035090310     C                   exsr      sqlerror
007036090310     C                   endsl
007037090310     C*
007038090310     C                   enddo
007039090310
007040090310     C/EXEC SQL
007041090311     C+ CLOSE A10
007042090310     C/END-EXEC
007043090310     c                   endsr
018000020709     C**********************************************************************
018100090311     C     srproj        BEGSR
018200020709     C**********************************************************************
018201090311     c                   seton                                        99
018202090311     c* se valorizza reperisco il codice fornitore aggancio proj e verifico
018203090720     c* se partita iva coincide con quella presente su AITRS
018204090311     C/EXEC SQL
018205090727     C+ DECLARE P1 CURSOR FOR SELECT fttsoc, fttcdf, SOGPARTIVA, SOGDES,
018206090727     C+ max(fttddc), count(*) FROM fiftt00f join fiapd00f on apdpdr =
018207090727     C+ fttpdr join anfrn00f on frnsocieta = apdcsf and substr(frnksc, 2,
018208090727     C+ 7) = digits(apdksc) join ansog00f on frnsogg= sogsogg exception
018209090727     C+ join aitrs00f on fttsoc = trssocg and trsiva = sogpartiva WHERE
018210090727     C+ ftttsr = ' ' and fttfvl = 'C' and apdcsf = fttsoc and apdksc =
018211090727     C+ fttcdf and apdtip = 'A' and apdatb = ' ' GROUP BY fttsoc, fttcdf,
018212090727     C+ SOGPARTIVA, SOGDES ORDER BY fttsoc, fttcdf, SOGPARTIVA, SOGDES
018213090311     C/END-EXEC
018214090311     C/EXEC SQL
018215090311     C+ OPEN p1
018216090311     C/END-EXEC
018217090311
018218090311     C                   do        *hival
018219090311
018220090311     C/EXEC SQL
018221090727     C+ FETCH NEXT FROM p1 INTO :fttsoc, :fttcdf, :SOGPARTIVA, :SOGDES,
018222090727     C+ :fttddc, :nrg
018223090311     C/END-EXEC
018224090311
018225090311     C                   select
018226090311     c* fine file
018227090311     C                   when      sqlcod = 100
018228090311     c                   leave
018229090311     c* dettaglio
018230090311     C                   when      sqlcod = 0
018231090727     c                   eval      stpnrg =nrg
018232090727     c                   if        fttddc > 0
018233090727     c                   move      fttddc        dtaiso
018234090727     c                   move      dtaiso        dtaeur
018235090727     c                   move      dtaeur        stpddc
018236090727     c                   else
018237090727     c                   clear                   stpddc
018238090727     c                   end
018268090311     c                   seton                                        11
018271090311     c                   exsr      srstampa
018276090311     C                   other
018277090311     C                   exsr      sqlerror
018278090311     C                   endsl
018279090311     C*
018280090311     C                   enddo
018281090311
018282090311     C/EXEC SQL
018283090311     C+ CLOSE p1
018284090311     C/END-EXEC
018285090311     c                   endsr
018333090311     C**********************************************************************
018334090311     C     srstampa      BEGSR
018335090311     C**********************************************************************
018336070921     C*
018337090311     c                   if        *in99
018338090311     c                   write     testa
018339090311     c   01              write     testa1
018340090311     c   02              write     testa2
018341090311     c   03              write     testa3
018342090311     c   04              write     testa4
018343090311     c   05              write     testa5
018344090311     c   06              write     testa6
018345090311     c   07              write     testa7
018346090311     c   08              write     testa8
018347090311     c   09              write     testa9
018348090311     c   10              write     testa10
018349090311     c   11              write     testa11
018350090316     c   12              write     testa12
018351090311     c                   setoff                                       99
018352090310     c                   end
018363090311     c*
018372090311     c*
018373090311     c   01              write     riga1
018374090311     c   02              write     riga2
018375090311     c   03              write     riga3
018376090311     c   04              write     riga4
018377090312     c   05              write     riga5
018378090311     c   06              write     riga1
018379090312     c   07              write     riga7
018380090311     c   08              write     riga1
018381090311     c   09              write     riga1
018382090727     c   10              write     riga10
018383090311     c   11              write     riga11
018384090316     c   12              write     riga12
018385090311     c   20              write     riga20
018386090311     c*
018387090317     c                   movea     '000000000000'*in
018388090311     c                   setoff                                       20
018389070921     C*
018390070921     C                   ENDSR
018400090311     C**********************************************************************
018401090311     C     srapd         begsr
018402090311     C**********************************************************************
018405090311     c     kapd          chain     fiapd01l
018406090311     c                   if        %found(fiapd01l)
018407090311     c                   eval      stprsf = apdrsf
018408090311     c                   if        apdatb <>' '
018409090311     c                   eval      stpann = 'ANNULLATO'
018410090311     c                   else
018411090311     c                   clear                   stpann
018412090311     c                   end
018413090311     c                   else
018414090311     c                   clear                   stpann
018415090311     c                   eval       stprsf = *all'?'
018416090311     c                   end
018417090311     C                   ENDSR
018418090311     C**********************************************************************
018419090311     C     sqlerror      begsr
018420090311     C**********************************************************************
018421081105     C*
018422081105     C                   call      'X66CHGJOB'
018423081105     C                   seton                                        lr
018424081105     C                   ENDSR
018425070921     C**********************************************************************
018426070921     C     *inzsr        BEGSR
018427070921     C**********************************************************************
018428070921     c     *entry        plist
018429070730     c                   parm                    kpjba
018430090311     c                   eval      nompgm = 'TRMZ96R'
018431090311     c                   eval      cmp132 = *all'-'
018432090311     C                   TIME                    TIMES             6 0
018433090311     C                   TIME                    WHDAT            12 0
018434090311     C                   MOVE      WHDAT         WOGGI             6 0
018435070731      * Reperisco dati job
018436070731     c                   exsr      DatiJob
018437090311     c*
019001090310     C     Kapd          KLIST                                                  *
019002090310     C                   KFLD                    apdtip                         *
019003090311     C                   KFLD                    comkau            7 0          *
019004090311     c                   eval      apdtip = 'A'
019007090311     c                   seton                                        99
019400020709     C                   ENDSR
019401070731      *---------------------------------------------------------------*
019402070731      * Reperimento Dati del job (Utente/Operativi)                   *
019403070731      *---------------------------------------------------------------*
019404070731     c     DatiJob       BEGSR
019405070731      *
019406070731     c     *dtaara       define    §azute        azuteds
019407070731     c     *dtaara       define    §datiute      ddatiute
019408070731      *
019409070731     c                   in(E)     *dtaara
019410070731     c                   IF        %ERROR or RSUT = *blanks
019411070731     c                   clear                   Tibs34Ds
019412070731     c                   call      'TIBS34R'
019413070731     c                   parm                    Tibs34Ds
019414070731     c                   in        *dtaara
019415070731     c                   ENDIF
019416070731      *
019417070731     c                   ENDSR
