000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000103090311     ffiapd01l  if   e           k disk
000104090428     ffiftt02l  if   e           k disk
000105090428     ffiatt02l  if   e           k disk
000106090724     Ftrmz98p   O    e             PRINTER OFLIND(*IN99)
000107070731     d* Parametri x Controllo profilo utenti
000108070731     d TIBS34ds      e ds
000109070731     d AZUTEds       e ds                  extname(AZUTE00F)
000812070730     d kpjba         e ds
000813090311     d DDATIUTE      e ds
000815090311     d aitrads       e ds                  extname(Aitra00f)
000816090428     d ok              s              1  0
000817090724     d oggim2          s              8  0 inz(20090501)
000818090724     d maxddc          s              8  0
000819090721     c                   exsr      srcitta
000820090724     c                   seton                                        98
000821090721     c                   exsr      sraff
000822090721     c                   seton                                        lr
000823090721     C**********************************************************************
000824090721     C     srcitta       BEGSR
000825090721     C**********************************************************************
003701090428     c* tariffe con data decorrenza tariffa incongruente con accreditamento
003702090428     c     'A'           setll     fiapd01l
003703090428     c                   do        *hival
003704090428     c     'A'           reade     fiapd01l
003705090428     c                   if        %found(fiapd01l)
003706090428     c                   leave
003707090428     c                   end
003708090721     c                   if        apdatb <> ' '
003709090721     c                   iter
003710090721     c                   end
003711090724     c                   if        apdpdd = 'S'
003712090724     c                   iter
003713090724     c                   end
003714090724     c                   clear                   maxddc
003715090724     C/EXEC SQL
003716090724     C+ SELECT max(fttddc) INTO :maxddc FROM fiftt00f WHERE fttpdr =
003717090724     C+ :apdpdr
003718090724     C/END-EXEC
003719090724     c                   if        fttddc > oggim2
003720090428     c                   clear                   ok
003721090311     C/EXEC SQL
003722090311     C+ DECLARE A1 CURSOR FOR SELECT trakau FROM aitra00f join fitgt00f
003723090424     C+ on trakau = tgtpdr WHERE TRASCO = ' ' and traann =' ' and trakau
003724090428     C+ = :apdpdr and tradfi = 0 and tgtatb = ' ' and tradin < tgtdst and
003725090428     C+ tradin > tgtddt GROUP BY trakau ORDER BY trakau
003726090311     C/END-EXEC
003800031106     C/EXEC SQL
003900031106     C+ OPEN A1
004000031106     C/END-EXEC
004100031028
004200070731     C                   do        *hival
004300031028
004400031028     C/EXEC SQL
004500090311     C+ FETCH NEXT FROM A1 INTO :trakau
004600031028     C/END-EXEC
004700031028
004800031028     C                   select
004801070926     c* fine file
004900031028     C                   when      sqlcod = 100
004901090428     c                   if        ok = 0
004902090428     c                   seton                                        01
004903090428     c                   exsr      srapd
004904090428     c                   exsr      srstampa
004905090428     c                   end
005001070731     c                   leave
005002070926     c* dettaglio
005400031028     C                   when      sqlcod = 0
005401090428     c                   eval      ok = 1
005700031028     C                   other
005800031028     C                   exsr      sqlerror
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006501090428     c                   end
006502090428     C                   enddo
006503090721     c                   endsr
006504090721     C**********************************************************************
006505090721     C     sraff         BEGSR
006506090721     C**********************************************************************
006507090721     c* tariffe con data decorrenza tariffa incongruente con accreditamento
006508090721     c     'D'           setll     fiapd01l
006509090721     c                   do        *hival
006510090721     c     'D'           reade     fiapd01l
006511090721     c                   if        %found(fiapd01l)
006512090721     c                   leave
006513090721     c                   end
006514090721     c                   if        apdatb <> ' '
006515090721     c                   iter
006516090721     c                   end
006517090724     c                   if        apdpdd = 'S'
006518090724     c                   iter
006519090724     c                   end
006520121004     c                   if        apdcsf = '219'
006521121004     c                   iter
006522121004     c                   end
006523090724     c                   clear                   maxddc
006524090724     C/EXEC SQL
006525090724     C+ SELECT max(attddc) INTO :maxddc FROM fiatt00f WHERE attpdr =
006526090724     C+ :apdpdr
006527090724     C/END-EXEC
006528090724     c                   if        maxddc > oggim2
006529090721     c                   clear                   ok
006530090721     C/EXEC SQL
006531090724     C+ DECLARE A2 CURSOR FOR SELECT trabpt FROM aitra00f join fiadt00f
006532090721     C+ on trabpt = adtpdr WHERE TRASCO = ' ' and traann =' ' and trabpt
006533090721     C+ = :apdpdr and tradfi = 0 and adtatb = ' ' and tradin < adtdst and
006534090721     C+ tradin > adtddt GROUP BY trabpt ORDER BY trabpt
006535090721     C/END-EXEC
006536090721     C/EXEC SQL
006537090724     C+ OPEN A2
006538090721     C/END-EXEC
006539090721
006540090721     C                   do        *hival
006541090721
006542090721     C/EXEC SQL
006543090724     C+ FETCH NEXT FROM A2 INTO :trakau
006544090721     C/END-EXEC
006545090721
006546090721     C                   select
006547090721     c* fine file
006548090721     C                   when      sqlcod = 100
006549090721     c                   if        ok = 0
006550090721     c                   seton                                        01
006551090721     c                   exsr      srapd
006552090721     c                   exsr      srstampa
006553090721     c                   end
006554090721     c                   leave
006555090721     c* dettaglio
006556090721     C                   when      sqlcod = 0
006557090721     c                   eval      ok = 1
006558090721     C                   other
006559090721     C                   exsr      sqlerror
006560090721     C                   endsl
006561090721     C*
006562090721     C                   enddo
006563090721
006564090721     C/EXEC SQL
006565090724     C+ CLOSE A2
006566090721     C/END-EXEC
006567090721     c                   end
006568090721     C                   enddo
006569090721     c                   endsr
006570090311     C**********************************************************************
006571090311     C     srstampa      BEGSR
006572090311     C**********************************************************************
006573090311     C*
006574090724     c                   if        *in99
006575090428     c  n98              write     testa
006576090428     c   98              write     testaa
006577090311     c   01              write     testa1
006578090311     c                   setoff                                       99
006579090311     c                   end
006580090311     c*
006581090311     c   01              write     riga1
006582090311     c*
006583090311     c                   setoff                                       01
006584090311     C*
006585090311     C                   ENDSR
006600090311     C**********************************************************************
006700090311     C     srapd         begsr
006800090311     C**********************************************************************
006900090311     c     kapd          chain     fiapd01l
007000090311     c                   if        %found(fiapd01l)
007100090311     c                   eval      stprsf = apdrsf
007200090311     c                   if        apdatb <>' '
007300090311     c                   eval      stpann = 'ANNULLATO'
007400090311     c                   else
007500090311     c                   clear                   stpann
007600090311     c                   end
007700090311     c                   else
007800090311     c                   clear                   stpann
007900090311     c                   eval       stprsf = *all'?'
008000090311     c                   end
008100090311     C                   ENDSR
008200090311     C**********************************************************************
008300090311     C     sqlerror      begsr
008400090311     C**********************************************************************
008500090311     C*
008600090311     C                   call      'X66CHGJOB'
008700090311     C                   seton                                        lr
008800090311     C                   ENDSR
008900090311     C**********************************************************************
009000090311     C     *inzsr        BEGSR
009100090311     C**********************************************************************
009200090311     c     *entry        plist
009300090311     c                   parm                    kpjba
009400090311     c                   eval      nompgm = 'TRMZ98R'
009500090311     c                   eval      cmp132 = *all'-'
009600090311     C                   TIME                    TIMES             6 0
009700090311     C                   TIME                    WHDAT            12 0
009800090311     C                   MOVE      WHDAT         WOGGI             6 0
009900090311      * Reperisco dati job
010000090311     c                   exsr      DatiJob
010100090311     c*
010200090311     C     Kapd          KLIST                                                  *
010300090311     C                   KFLD                    apdtip                         *
010400090428     C                   KFLD                    trakau            7 0          *
010600090311     c                   seton                                        99
010700090311     C                   ENDSR
010800090311      *---------------------------------------------------------------*
010900090311      * Reperimento Dati del job (Utente/Operativi)                   *
011000090311      *---------------------------------------------------------------*
011100090311     c     DatiJob       BEGSR
011200090311      *
011300090311     c     *dtaara       define    §azute        azuteds
011400090311     c     *dtaara       define    §datiute      ddatiute
011500090311      *
011600090311     c                   in(E)     *dtaara
011700090311     c                   IF        %ERROR or RSUT = *blanks
011800090311     c                   clear                   Tibs34Ds
011900090311     c                   call      'TIBS34R'
012000090311     c                   parm                    Tibs34Ds
012100090311     c                   in        *dtaara
012200090311     c                   ENDIF
012300090311      *
012400090311     c                   ENDSR
