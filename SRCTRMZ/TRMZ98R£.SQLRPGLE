000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000103090311     ffiapd01l  if   e           k disk
000104090311     Ftrmz98p   O    e             PRINTER OFLIND(*IN99)
000106070731     d* Parametri x Controllo profilo utenti
000107070731     d TIBS34ds      e ds
000108070731     d AZUTEds       e ds                  extname(AZUTE00F)
000812070730     d kpjba         e ds
000813090311     d DDATIUTE      e ds
000815090311     d aitrads       e ds                  extname(Aitra00f)
003700090427     c                   eval      tipov = 'AUT di Città'
003701090311     c* tariffe con data decorrenza minore di data accreditamento
003702090311     C/EXEC SQL
003703090311     C+ DECLARE A1 CURSOR FOR SELECT trakau FROM aitra00f join fitgt00f
003704090424     C+ on trakau = tgtpdr WHERE TRASCO = ' ' and traann =' ' and trakau
003705090424     C+ <> 0 and tradin < tgtdst and tradfi = 0 and tradin > tgtddt and
003706090424     C+ tgtatb = ' ' GROUP BY trakau ORDER BY trakau
003707090311     C/END-EXEC
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
005001070731     c                   leave
005002070926     c* dettaglio
005400031028     C                   when      sqlcod = 0
005401090310     c                   seton                                        01
005600090311     c                   exsr      srapd
005601090311     c                   exsr      srstampa
005700031028     C                   other
005800031028     C                   exsr      sqlerror
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006501090427     c                   seton                                        99
006502090427     c                   eval      tipov = 'AFFL/DEFL'
006503090427     c* tariffe con data decorrenza minore di data accreditamento
006504090427     C/EXEC SQL
006505090427     C+ DECLARE A2 CURSOR FOR SELECT trakau FROM aitra00f join fitgt00f
006506090427     C+ on trakau = tgtpdr WHERE TRASCO = ' ' and traann =' ' and trakau
006507090427     C+ <> 0 and tradin < tgtdst and tradfi = 0 and tradin > tgtddt and
006508090427     C+ tgtatb = ' ' GROUP BY trakau ORDER BY trakau
006509090427     C/END-EXEC
006510090427     C/EXEC SQL
006511090427     C+ OPEN A2
006512090427     C/END-EXEC
006513090427
006514090427     C                   do        *hival
006515090427
006516090427     C/EXEC SQL
006517090427     C+ FETCH NEXT FROM A2 INTO :trakau
006518090427     C/END-EXEC
006519090427
006520090427     C                   select
006521090427     c* fine file
006522090427     C                   when      sqlcod = 100
006523090427     c                   leave
006524090427     c* dettaglio
006525090427     C                   when      sqlcod = 0
006526090427     c                   seton                                        01
006527090427     c                   exsr      srapd
006528090427     c                   exsr      srstampa
006529090427     C                   other
006530090427     C                   exsr      sqlerror
006531090427     C                   endsl
006532090427     C*
006533090427     C                   enddo
006534090427
006535090427     C/EXEC SQL
006536090427     C+ CLOSE A2
006537090427     C/END-EXEC
006538090311     c                   seton                                        lr
006539090311     C**********************************************************************
006540090311     C     srstampa      BEGSR
006541090311     C**********************************************************************
006542090311     C*
006543090311     c                   if        *in99
006544090311     c                   write     testa
006545090311     c   01              write     testa1
006546090311     c                   setoff                                       99
006547090311     c                   end
006548090311     c*
006549090311     c   01              write     riga1
006550090311     c*
006551090311     c                   setoff                                       01
006552090311     C*
006553090311     C                   ENDSR
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
010400090311     C                   KFLD                    trakau            7 0          *
010500090311     c                   eval      apdtip = 'A'
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
