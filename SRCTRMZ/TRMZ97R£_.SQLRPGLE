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
000828090318     d savcar          s              3  0
000829090312     d dtaiso          s               d
000830090312     d dtaeur          s               d   datfmt(*eur)
000831090313     c                   open      trmz96p
000836090311     c                   exsr      sraitra6
000837090311     c                   exsr      sraitra7
000838090311     c                   exsr      sraitra8
000839090311     c                   exsr      sraitra9
000840090311     c                   exsr      sraitra10
000841090318     c* automezzi
000842090403     c*                  exsr      sraitra12
000843090318     c* qualità
000844090415     c*                  exsr      sraitra13
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
006667090420     C+ <> 0 and attddc > 20090100 GROUP BY attpdr ORDER BY attpdr
006668090310     C/END-EXEC
006669090311     C/EXEC SQL
006670090311     C+ OPEN A5
006671090311     C/END-EXEC
006672090311
006673090311     C                   do        *hival
006674090311
006675090311     C/EXEC SQL
006676090420     C+ FETCH NEXT FROM A5 INTO :attpdr, :attddc
006677090311     C/END-EXEC
006678090311
006679090311     C                   select
006680090311     c* fine file
006681090311     C                   when      sqlcod = 100
006682090311     c                   leave
006683090311     c* dettaglio
006684090311     C                   when      sqlcod = 0
006685090420     c                   eval      comkau = attpdr
006686090403     c                   exsr      srapd
006687090403     c                   if        apdatb <> ' '
006688090403     c                   iter
006689090403     c                   end
006690090420     c                   movel     attpdr        comfil            3 0
006691090312     c                   if        savfil <> comfil
006692090313     c                   if        %open(trmz96p)
006693090420     c                   close     trmz96p
006694090313     c                   end
006695090420     c                   if        not %open(trmz96p)
006696090420     c                   open      trmz96p
006697090313     c                   end
006698090312     c                   seton                                        99
006699090312     c                   move      comfil        savfil            3 0
006700090312     c                   end
006701090420     c                   if        attddc > 0
006702090420     c                   move      attddc        dtaiso
006703090312     c                   move      dtaiso        dtaeur
006704090312     c                   move      dtaeur        stpddc
006705090312     c                   else
006706090312     c                   clear                   stpddc
006707090312     c                   end
006708090311     c                   seton                                        05
006709090311     c                   exsr      srstampa
006710090311     C                   other
006711090311     C                   exsr      sqlerror
006712090311     C                   endsl
006713090311     C*
006714090311     C                   enddo
006715090311
006716090311     C/EXEC SQL
006717090311     C+ CLOSE A5
006718090311     C/END-EXEC
006719090311     c                   endsr
006720090311     C**********************************************************************
006721090311     C     sraitra6      BEGSR
006722090311     C**********************************************************************
006723090311     c                   seton                                        99
006724090310     c* se valorizza e non è accreditato
006725090310     C/EXEC SQL
006726090420     C+ DECLARE A6 CURSOR FOR SELECT trabpt FROM aitra00f join fiatt00f
006727090420     C+ on trabpt = attpdr WHERE TRASCO = ' ' and traann =' ' and TRADIN
006728090420     C+ = 0 and trabpt <> 0 and tradfi =0 and attdco <> 0 and attddc >
006729090420     C+ 20090100 GROUP BY trabpt ORDER BY trabpt
006730090310     C/END-EXEC
006731090311     C/EXEC SQL
006732090311     C+ OPEN A6
006733090311     C/END-EXEC
006734090311
006735090311     C                   do        *hival
006736090311
006737090311     C/EXEC SQL
006738090420     C+ FETCH NEXT FROM A6 INTO :trabpt
006739090311     C/END-EXEC
006740090311
006741090311     C                   select
006742090311     c* fine file
006743090311     C                   when      sqlcod = 100
006744090311     c                   leave
006745090311     c* dettaglio
006746090311     C                   when      sqlcod = 0
006747090311     c                   seton                                        06
006748090420     C                   eval      comkau = trabpt
006749090311     c                   exsr      srapd
006750090311     c                   exsr      srstampa
006751090311     C                   other
006752090311     C                   exsr      sqlerror
006753090311     C                   endsl
006754090311     C*
006755090311     C                   enddo
006756090311
006757090311     C/EXEC SQL
006758090311     C+ CLOSE A6
006759090311     C/END-EXEC
006760090311     c                   endsr
006761090311     C**********************************************************************
006762090311     C     sraitra7      BEGSR
006763090311     C**********************************************************************
006764090311     c                   seton                                        99
006765090310     c* se valorizza ed è disaccreditato
006766090310     C/EXEC SQL
006767090420     C+ DECLARE A7 CURSOR FOR SELECT trabpt, traaut, max(attddc) FROM
006768090420     C+ aitra00f join fiatt00f on trabpt = attpdr join anfrn00f on
006769090420     C+ attsoc=frnsocieta and attcdf = frnksc join ansog00f on frnsogg=
006770090415     C+ sogsogg WHERE                 TRASCO = ' ' and traann =' ' and
006771090420     C+ trabpt <> 0 and tradfi < attddc and tradfi <> 0 and attdco<> 0
006772090420     C+                and attsoc = trasocg and sogpartiva=traiva GROUP
006773090420     C+ BY trabpt, traaut ORDER BY trabpt, traaut
006774090310     C/END-EXEC
006775090311     C/EXEC SQL
006776090311     C+ OPEN A7
006777090311     C/END-EXEC
006778090311
006779090311     C                   do        *hival
006780090311
006781090311     C/EXEC SQL
006782090420     C+ FETCH NEXT FROM A7 INTO :trabpt, :traaut, :attddc
006783090311     C/END-EXEC
006784090311
006785090311     C                   select
006786090311     c* fine file
006787090311     C                   when      sqlcod = 100
006788090311     c                   leave
006789090311     c* dettaglio
006790090311     C                   when      sqlcod = 0
006791090420     C                   eval      comkau = trabpt
006792090403     c                   exsr      srapd
006793090403     c                   if        apdatb <> ' '
006794090403     c                   iter
006795090403     c                   end
006796090318     c                   clear                   ok                1
006797090318     c* verifico di non avere lo stesso codice anche accreditato
006798090420     c     trabpt        setll     aitra05l
006799090420     c                   do        *hival
006800090420     c     trabpt        reade     aitra05l
006801090420     c                   if        %eof(aitra05l)
006802090318     c                   leave
006803090318     c                   end
006804090318     c                   if        £tradfi = 0
006805090318     c                   eval      ok = '1'
006806090318     c                   leave
006807090318     c                   end
006808090318     c                   enddo
006809090318     c                   if        ok = '1'
006810090318     c                   iter
006811090318     c                   end
006812090318     c                   seton                                        07
006813090420     c                   if        attddc > 0
006814090420     c                   move      attddc        dtaiso
006815090312     c                   move      dtaiso        dtaeur
006816090312     c                   move      dtaeur        stpddc
006817090312     c                   else
006818090312     c                   clear                   stpddc
006819090312     c                   end
006820090311     c                   exsr      srstampa
006821090311     C                   other
006822090311     C                   exsr      sqlerror
006823090311     C                   endsl
006824090311     C*
006825090311     C                   enddo
006826090311
006827090311     C/EXEC SQL
006828090311     C+ CLOSE A7
006829090311     C/END-EXEC
006830090311     c                   endsr
006831090311     C**********************************************************************
006832090311     C     sraitra8      BEGSR
006833090311     C**********************************************************************
006834090311     c                   seton                                        99
006835090310     c* controllo che i codici aut esistino in apd
006836090310     C/EXEC SQL
006837090420     C+ DECLARE A8 CURSOR FOR SELECT trabpt FROM aitra00f exception join
006838090420     C+ fiapd00f on trabpt = apdpdr and apdtip ='D' WHERE TRASCO = ' '
006839090420     C+ and traann =' ' and trabpt <> 0 and tradin > 0 and tradfi = 0 and
006840090420     C+ apdatb = ' ' GROUP BY trabpt ORDER BY trabpt
006841090310     C/END-EXEC
006842090311     C/EXEC SQL
006843090311     C+ OPEN A8
006844090311     C/END-EXEC
006845090311
006846090311     C                   do        *hival
006847090311
006848090311     C/EXEC SQL
006849090420     C+ FETCH NEXT FROM A8 INTO :trabpt
006850090311     C/END-EXEC
006851090311
006852090311     C                   select
006853090311     c* fine file
006854090311     C                   when      sqlcod = 100
006855090311     c                   leave
006856090311     c* dettaglio
006857090311     C                   when      sqlcod = 0
006858090311     c                   seton                                        08
006859090420     C                   eval      comkau = trabpt
006860090311     c                   exsr      srapd
006861090311     c                   exsr      srstampa
006862090311     C                   other
006863090311     C                   exsr      sqlerror
006864090311     C                   endsl
006865090311     C*
006866090311     C                   enddo
006867090311
006868090311     C/EXEC SQL
006869090311     C+ CLOSE A8
006870090311     C/END-EXEC
006871090311     c                   endsr
006872090311     C**********************************************************************
006873090311     C     sraitra9      BEGSR
006874090311     C**********************************************************************
006875090311     c                   seton                                        99
006876090310     c* controllo che i codici aut esistino in apd e siano in autofat.
006877090310     C/EXEC SQL
006878090420     C+ DECLARE A9 CURSOR FOR SELECT trabpt FROM aitra00f join fiapd00f
006879090420     C+ on trabpt = apdpdr and apdtip = 'D' WHERE TRASCO = ' ' and traann
006880090420     C+ =' ' and trabpt <> 0 and tradin > 0 and tradfi = 0 and apdatb =
006881090420     C+ ' ' and apdpdd = 'S' GROUP BY trabpt ORDER BY trabpt
006882090310     C/END-EXEC
006883090311     C/EXEC SQL
006884090311     C+ OPEN A9
006885090311     C/END-EXEC
006886090311
006887090311     C                   do        *hival
006888090311
006889090311     C/EXEC SQL
006890090420     C+ FETCH NEXT FROM A9 INTO :trabpt
006891090311     C/END-EXEC
006892090311
006893090311     C                   select
006894090311     c* fine file
006895090311     C                   when      sqlcod = 100
006896090311     c                   leave
006897090311     c* dettaglio
006898090311     C                   when      sqlcod = 0
006899090311     c                   seton                                        09
006900090420     C                   eval      comkau = trabpt
006901090311     c                   exsr      srapd
006902090311     c                   exsr      srstampa
006903090311     C                   other
006904090311     C                   exsr      sqlerror
006905090311     C                   endsl
006906090311     C*
006907090311     C                   enddo
006908090311
006909090311     C/EXEC SQL
006910090311     C+ CLOSE A9
006911090311     C/END-EXEC
006912090311     c                   endsr
006913090311     C**********************************************************************
006914090311     C     sraitra10     BEGSR
006915090311     C**********************************************************************
006916090311     c                   seton                                        99
006917090310     c* controllo che i codici aut accreditati valorizzino
006918090310     C/EXEC SQL
006919090420     C+ DECLARE A10 CURSOR FOR SELECT trabpt FROM aitra00f exception join
006920090420     C+ fiatt00f on trabpt = attpdr WHERE TRASCO = ' ' and traann =' '
006921090420     C+ and trabpt <> 0 and tradin > 0 and tradfi = 0 GROUP BY trabpt
006922090420     C+ ORDER BY trabpt
006923090310     C/END-EXEC
006924090310
006925090310     C/EXEC SQL
006926090311     C+ OPEN A10
006927090310     C/END-EXEC
006928090310
006929090310     C                   do        *hival
006930090310
006931090310     C/EXEC SQL
006932090420     C+ FETCH NEXT FROM A10 INTO :trabpt
006933090310     C/END-EXEC
006934090310
006935090310     C                   select
006936090310     c* fine file
006937090310     C                   when      sqlcod = 100
006938090310     c                   leave
006939090310     c* dettaglio
006940090310     C                   when      sqlcod = 0
006941090311     c                   seton                                        10
006942090420     C                   eval      comkau = trabpt
006943090311     c                   exsr      srapd
006948090310     c                   exsr      srstampa
006949090310     C                   other
006950090310     C                   exsr      sqlerror
006951090310     C                   endsl
006952090310     C*
006953090310     C                   enddo
006954090310
006955090310     C/EXEC SQL
006956090311     C+ CLOSE A10
006957090310     C/END-EXEC
006958090310     c                   endsr
018000020709     C**********************************************************************
018100090311     C     srproj        BEGSR
018200020709     C**********************************************************************
018201090311     c                   seton                                        99
018202090311     c* se valorizza reperisco il codice fornitore aggancio proj e verifico
018203090311     c* se partita iva coincide con quella presente su AITRA
018204090311     C/EXEC SQL
018205090420     C+ DECLARE P1 CURSOR FOR SELECT attsoc, attcdf, SOGPARTIVA, SOGDES
018206090420     C+ FROM aiftt00f join fiapd00f on apdpdr = attpdr join anfrn00f on
018207090311     C+ frnsocieta = apdcsf and substr(frnksc, 2, 7) = digits(apdksc)
018208090311     C+ join ansog00f on frnsogg= sogsogg exception join aitra00f on
018209090420     C+ attsoc = trasocg and traiva = sogpartiva WHERE attdco <> 0
018210090420     C+ and apdcsf = attsoc and apdksc = attcdf and apdtip =
018211090420     C+ 'D' and apdatb = ' ' GROUP BY attsoc, attcdf, SOGPARTIVA, SOGDES
018212090420     C+ ORDER BY attsoc, attcdf, SOGPARTIVA, SOGDES
018213090311     C/END-EXEC
018214090311     C/EXEC SQL
018215090311     C+ OPEN p1
018216090311     C/END-EXEC
018217090311
018218090311     C                   do        *hival
018219090311
018220090311     C/EXEC SQL
018221090420     C+ FETCH NEXT FROM p1 INTO :attsoc, :attcdf, :SOGPARTIVA, :SOGDES
018222090311     C/END-EXEC
018223090311
018224090311     C                   select
018225090311     c* fine file
018226090311     C                   when      sqlcod = 100
018227090311     c                   leave
018228090311     c* dettaglio
018229090311     C                   when      sqlcod = 0
018268090311     c                   seton                                        11
018271090311     c                   exsr      srstampa
018272090311     c* verifico se codice autista presente con altra p.iva
018273090311     c                   exsr      sraitra20
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
018286090311     C**********************************************************************
018287090311     C     sraitra20     BEGSR
018288090311     C**********************************************************************
018289090311     c* controllo che il codice autista non sia inserito nell'aitra con
018290090311     c* partita iva differente
018291090311     C/EXEC SQL
018292090312     C+ DECLARE A20 CURSOR FOR SELECT apdpdr, ifnull (trasocg, ' '),
018293090312     C+ ifnull (traiva, ' ') FROM fiapd00f left outer join aitra00f on
018294090420     C+ trabpt = apdpdr WHERE ((TRARIS= ' ' and TRASCO = ' ' and traann
018295090312     C+ =' ' and tradfi = 0 ) or (TRARIS is null and TRASCO is null and
018296090420     C+ traann is null and tradfi is null )) and apdtip = 'D' and apdatb
018297090420     C+ = ' ' and apdcsf = :attsoc and digits(apdksc) = substr(:attcdf,
018298090312     C+ 2, 7) GROUP BY apdpdr, trasocg, traiva ORDER BY apdpdr, trasocg,
018299090312     C+ traiva
018300090311     C/END-EXEC
018301090311
018302090311     C/EXEC SQL
018303090311     C+ OPEN A20
018304090311     C/END-EXEC
018305090311
018306090311     C                   do        *hival
018307090311
018308090311     C/EXEC SQL
018309090312     C+ FETCH NEXT FROM A20 INTO :apdpdr, :trasocg, :traiva
018310090311     C/END-EXEC
018311090311
018312090311     C                   select
018313090311     c* fine file
018314090311     C                   when      sqlcod = 100
018315090311     c                   leave
018316090311     c* dettaglio
018317090311     C                   when      sqlcod = 0
018318090311     c                   seton                                        20
018319090312     C                   EVAL      COMKAU = apdpdr
018320090311     c                   exsr      srapd
018322090311     c                   exsr      srstampa
018323090311     C                   other
018324090311     C                   exsr      sqlerror
018325090311     C                   endsl
018326090311     C*
018327090311     C                   enddo
018328090311
018329090311     C/EXEC SQL
018330090311     C+ CLOSE A20
018331090311     C/END-EXEC
018332090311     c                   endsr
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
018382090311     c   10              write     riga1
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
