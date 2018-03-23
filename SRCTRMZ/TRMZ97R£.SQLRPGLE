000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000102090310     ffiapd01l  if   e           k disk
000103090420     faitra05l  if   e           k disk    prefix(£)
000104090316     fansog08j  if   e           k disk
000105090311     Ftrmz96p   O    e             PRINTER OFLIND(*IN99)
000106090313     f                                     usropn
000107070731     d* Parametri x Controllo profilo utenti
000108070731     d TIBS34ds      e ds
000109070731     d AZUTEds       e ds                  extname(AZUTE00F)
000110090311     d DDATIUTE      e ds
000111090311     d ansogds       e ds                  extname(Ansog00f)
000112090311     d aitrads       e ds                  extname(Aitra00f)
000113090420     d fiattds       e ds                  extname(fiatt00f)
000114090729     d fiadtds       e ds                  extname(fiadt00f)
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
000834090720     c                   exsr      sraitra7
000840090311     c                   exsr      sraitra10
000845090311     c                   exsr      srproj
000846090313     c                   exsr      sraitra5
000847090310     c                   seton                                        lr
006659090311     C**********************************************************************
006660090311     C     sraitra5      BEGSR
006661090311     C**********************************************************************
006662090311     c                   seton                                        99
006663090310     c* se valorizza e non esiste in AITRA
006664090310     C/EXEC SQL
006665090420     C+ DECLARE A5 CURSOR FOR SELECT attpdr, max(attddc) FROM fiatt00f
006666090420     C+ exception join aitra00f on trabpt = attpdr WHERE FIATT00F.ATTDCO
006667121004     C+ <> 0 and attddc > 20090100 and attsoc <> '219'
006668121004     C+ GROUP BY attpdr ORDER BY attpdr
006669090310     C/END-EXEC
006670090311     C/EXEC SQL
006671090311     C+ OPEN A5
006672090311     C/END-EXEC
006673090311
006674090311     C                   do        *hival
006675090311
006676090311     C/EXEC SQL
006677090420     C+ FETCH NEXT FROM A5 INTO :attpdr, :attddc
006678090311     C/END-EXEC
006679090311
006680090311     C                   select
006681090311     c* fine file
006682090311     C                   when      sqlcod = 100
006683090311     c                   leave
006684090311     c* dettaglio
006685090311     C                   when      sqlcod = 0
006686090420     c                   eval      comkau = attpdr
006687090403     c                   exsr      srapd
006688090403     c                   if        apdatb <> ' '
006689090403     c                   iter
006690090403     c                   end
006691090420     c                   movel     attpdr        comfil            3 0
006692090312     c                   if        savfil <> comfil
006693090313     c                   if        %open(trmz96p)
006694090420     c                   close     trmz96p
006695090313     c                   end
006696090420     c                   if        not %open(trmz96p)
006697090724     c                   z-add     1             page
006698090420     c                   open      trmz96p
006699090313     c                   end
006700090312     c                   seton                                        99
006701090312     c                   move      comfil        savfil            3 0
006702090312     c                   end
006703090420     c                   if        attddc > 0
006704090420     c                   move      attddc        dtaiso
006705090312     c                   move      dtaiso        dtaeur
006706090312     c                   move      dtaeur        stpddc
006707090312     c                   else
006708090312     c                   clear                   stpddc
006709090312     c                   end
006710090311     c                   seton                                        05
006711090311     c                   exsr      srstampa
006712090311     C                   other
006713090311     C                   exsr      sqlerror
006714090311     C                   endsl
006715090311     C*
006716090311     C                   enddo
006717090311
006718090311     C/EXEC SQL
006719090311     C+ CLOSE A5
006720090311     C/END-EXEC
006721090311     c                   endsr
006722090720     C**********************************************************************
006723090720     C     sraitra7      BEGSR
006724090720     C**********************************************************************
006725090720     c                   seton                                        99
006726090720     c* se valorizza ed è disaccreditato
006727090720     C/EXEC SQL
006728090724     C+ DECLARE A7 CURSOR FOR SELECT trabpt, traaut, max(tradfi),
006729090724     C+ max(attddc), min(tradfi) FROM aitrs00f join aitra00f on tranrc=
006730090724     C+ trsnrc join fiatt00f on trabpt = attpdr join anfrn00f on
006731090724     C+ attsoc=frnsocieta and attcdf = frnksc join ansog00f on frnsogg=
006732090724     C+ sogsogg WHERE TRASCO = ' ' and traann =' ' and trabpt <> 0 and
006733121004     C+ attdco<> 0 and attsoc = trssocg and sogpartiva=trsiva and attsoc
006734121004     C+ <> '219' group by
006735090724     C+ trabpt, traaut HAVING min(tradfi) <> 0 and max(tradfi)<
006736090724     C+ max(attddc) ORDER BY trabpt, traaut
006737090720     C/END-EXEC
006738090720     C/EXEC SQL
006739090720     C+ OPEN A7
006740090720     C/END-EXEC
006741090720
006742090720     C                   do        *hival
006743090720
006744090720     C/EXEC SQL
006745090724     C+ FETCH NEXT FROM A7 INTO :trakau, :traaut, :tradfi, :attddc, :mindfi
006746090720     C/END-EXEC
006747090720
006748090720     C                   select
006749090720     c* fine file
006750090720     C                   when      sqlcod = 100
006751090720     c                   leave
006752090720     c* dettaglio
006753090720     C                   when      sqlcod = 0
006754090720     C                   eval      comkau = trakau
006755090720     c                   exsr      srapd
006756090720     c                   if        apdatb <> ' '
006757090720     c                   iter
006758090720     c                   end
006759090720     c                   clear                   ok                1
006760090720     c* verifico di non avere lo stesso codice anche accreditato
006761090724     c     trakau        setll     aitra05l
006762090720     c                   do        *hival
006763090724     c     trakau        reade     aitra05l
006764090724     c                   if        %eof(aitra05l)
006765090720     c                   leave
006766090720     c                   end
006767090720     c                   if        £tradfi = 0
006768090720     c                   eval      ok = '1'
006769090720     c                   leave
006770090720     c                   end
006771090720     c                   enddo
006772090720     c                   if        ok = '1'
006773090720     c                   iter
006774090720     c                   end
006775090720     c                   seton                                        07
006776090724     c                   if        tradfi > 0
006777090724     c                   move      tradfi        dtaiso
006778090720     c                   move      dtaiso        dtaeur
006779090724     c                   move      dtaeur        stpdfi
006780090720     c                   else
006781090724     c                   clear                   stpdfi
006782090720     c                   end
006783090724     c                   if        attddc > 0
006784090724     c                   move      attddc        dtaiso
006785090724     c                   move      dtaiso        dtaeur
006786090724     c                   move      dtaeur        stpddc
006787090724     c                   else
006788090724     c                   clear                   stpddc
006789090724     c                   end
006790090720     c                   exsr      srstampa
006791090720     C                   other
006792090720     C                   exsr      sqlerror
006793090720     C                   endsl
006794090720     C*
006795090720     C                   enddo
006796090720
006797090720     C/EXEC SQL
006798090720     C+ CLOSE A7
006799090720     C/END-EXEC
006800090720     c                   endsr
006913090311     C**********************************************************************
006914090311     C     sraitra10     BEGSR
006915090311     C**********************************************************************
006916090311     c                   seton                                        99
006917090310     c* controllo che i codici aut accreditati valorizzino
006918090310     C/EXEC SQL
006919090729     C+ DECLARE A10 CURSOR FOR SELECT trabpt, tradin, adtdst, adtdcn,
006920090729     C+ adtdrc FROM aitra00f exception join fiatt00f on trabpt = attpdr
006921090729     C+ left outer join fiadt00f on adtpdr = trabpt WHERE TRASCO = ' '
006922090729     C+ and traann =' ' and trabpt <> 0 and tradin > 0 and tradfi = 0 and
006923121004     C+ adtddt >= tradin and attsoc <> '219'
006924121004     C+ GROUP BY trabpt, tradin, adtdst, adtdcn, adtdrc
006925090729     C+ ORDER BY trabpt, tradin
006926090310     C/END-EXEC
006927090310
006928090310     C/EXEC SQL
006929090311     C+ OPEN A10
006930090310     C/END-EXEC
006931090310
006932090310     C                   do        *hival
006933090310
006934090310     C/EXEC SQL
006935090727     C+ FETCH NEXT FROM A10 INTO :trabpt, :tradin
006936090310     C/END-EXEC
006937090310
006938090310     C                   select
006939090310     c* fine file
006940090310     C                   when      sqlcod = 100
006941090310     c                   leave
006942090310     c* dettaglio
006943090310     C                   when      sqlcod = 0
006944090727     c                   if        tradin > 0
006945090727     c                   move      tradin        dtaiso
006946090727     c                   move      dtaiso        dtaeur
006947090727     c                   move      dtaeur        stpdac
006948090727     c                   else
006949090727     c                   clear                   stpdac
006950090727     c                   end
006951090729     c                   if        adtdst > 0
006952090729     c                   move      adtdst        dtaiso
006953090729     c                   move      dtaiso        dtaeur
006954090729     c                   move      dtaeur        stpdst
006955090729     c                   else
006956090729     c                   clear                   stpdst
006957090729     c                   end
006958090729     c                   if        adtdcn > 0
006959090729     c                   move      adtdcn        dtaiso
006960090729     c                   move      dtaiso        dtaeur
006961090729     c                   move      dtaeur        stpdcn
006962090729     c                   else
006963090729     c                   clear                   stpdcn
006964090729     c                   end
006965090729     c                   if        adtdrc > 0
006966090729     c                   move      adtdrc        dtaiso
006967090729     c                   move      dtaiso        dtaeur
006968090729     c                   move      dtaeur        stpdrc
006969090729     c                   else
006970090729     c                   clear                   stpdrc
006971090729     c                   end
006972090311     c                   seton                                        10
006973090420     C                   eval      comkau = trabpt
006974090311     c                   exsr      srapd
006975090310     c                   exsr      srstampa
006976090310     C                   other
006977090310     C                   exsr      sqlerror
006978090310     C                   endsl
006979090310     C*
006980090310     C                   enddo
006981090310
006982090310     C/EXEC SQL
006983090311     C+ CLOSE A10
006984090310     C/END-EXEC
006985090310     c                   endsr
018000020709     C**********************************************************************
018100090311     C     srproj        BEGSR
018200020709     C**********************************************************************
018201090311     c                   seton                                        99
018202090311     c* se valorizza reperisco il codice fornitore aggancio proj e verifico
018203090720     c* se partita iva coincide con quella presente su AITRS
018204090311     C/EXEC SQL
018205090727     C+ DECLARE P1 CURSOR FOR SELECT attsoc, attcdf, SOGPARTIVA, SOGDES,
018206090727     C+ max(attddc), count(*) FROM fiatt00f join fiapd00f on apdpdr =
018207090727     C+ attpdr join anfrn00f on frnsocieta = apdcsf and substr(frnksc, 2,
018208090727     C+ 7) = digits(apdksc) join ansog00f on frnsogg= sogsogg exception
018209090727     C+ join aitrs00f on attsoc = trssocg and trsiva = sogpartiva WHERE
018210090727     C+ attdco <> 0 and apdcsf = attsoc and apdksc = attcdf and apdtip =
018211121004     C+ 'D' and apdatb = ' ' and attsoc <> '219'
018212121004     C+ GROUP BY attsoc, attcdf, SOGPARTIVA, SOGDES
018213090727     C+ ORDER BY attsoc, attcdf, SOGPARTIVA, SOGDES
018214090311     C/END-EXEC
018215090311     C/EXEC SQL
018216090311     C+ OPEN p1
018217090311     C/END-EXEC
018218090311
018219090311     C                   do        *hival
018220090311
018221090311     C/EXEC SQL
018222090727     C+ FETCH NEXT FROM p1 INTO :attsoc, :attcdf, :SOGPARTIVA, :SOGDES,
018223090727     C+ :attddc, :nrg
018224090311     C/END-EXEC
018225090311
018226090311     C                   select
018227090311     c* fine file
018228090311     C                   when      sqlcod = 100
018229090311     c                   leave
018230090311     c* dettaglio
018231090311     C                   when      sqlcod = 0
018232090727     c                   eval      stpnrg = nrg
018233090727     c                   if        attddc > 0
018234090727     c                   move      attddc        dtaiso
018235090727     c                   move      dtaiso        dtaeur
018236090727     c                   move      dtaeur        stpddc
018237090727     c                   else
018238090727     c                   clear                   stpddc
018239090727     c                   end
018240090724     c                   eval      fttsoc = attsoc
018241090724     c                   eval      fttcdf = attcdf
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
018338090428     c                   write     testaa
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
019004090420     c                   eval      apdtip = 'D'
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
