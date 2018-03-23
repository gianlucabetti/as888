000100071228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200071228
000300160629     ftabel00f  if   e           k disk
000400160629     fazorg01l  if   e           k disk
000500160721     ftikun01l  if   e           k disk
000600160721     fwfrtma1l  uf a e           k disk
000700071228
000800071228      //-----------------------------------------------------------------------
000900071228      //? VARIABILI
001000071228      //-----------------------------------------------------------------------
001100071228     d codut           s              1  0 inz(1)
001200160628     d kcod            s                   like(tblcod)
001300160628     d kkey            s                   like(tblkey)
001400160721     d datadal         s              8  0
001500160721     d dataal          s              8  0
001600160721     d valore          s             13  3 inz
001700160721     d w70             s              7  0
001800160721     d x               s              4  0 inz
001900160722     d dataiso         s               d
002000160721     d wdate           s              8  0
002100071228     d wtime           s             14  0
002200071228      //-----------------------------------------------------------------------
002300071228      //? SCHIERE
002400071228      //-----------------------------------------------------------------------
002500071228
002600071228      //-----------------------------------------------------------------------
002700071228      //? STRUTTURE DATI
002800071228      //-----------------------------------------------------------------------
002900071228     d param           ds
003000160721     d  anno                   1      4s 0
003100160721     d  mese                   5      6s 0
003200160721     d  wanmes                 1      6s 0
003300071228
003400071228     d ds_cnaco      e ds                  inz extname(cnaco00f)
003500071228     d ds_cnind      e ds                  inz extname(cnind00f)
003600071228     d ds_cnclp      e ds                  inz extname(cnclp00f)
003700071228     d ds_fncls      e ds                  inz extname(fncls00f)
003800071228     d kpjba         e ds
003900071228     d tibs10ds      e ds
004000160721     d  sk_fig                21   5520  0 inz  dim(500)
004100071228     d tibs69ds      e ds
004200160721      *
004300160722     d azmgb           ds
004400160722     D  cliente                       8
004500160722     D  codice                       15
004600160722     D  quantita                     13  3
004700071228
004800071228      //-----------------------------------------------------------------------
004900160701     c                   eval      *inlr = *on
005000160721     c*
005100160630     C/EXEC SQL
005200160721     C+ DECLARE B1 CURSOR FOR SELECT mgbsog, mgbcod, sum(mgbqta)
005300160722     C+ from azmgb00f where mgbcau = 'RTMA' and mgbdat between :datadal
005400160722     C+ and :dataal
005500160721     C+ group by mgbsog, mgbcod ORDER BY mgbsog
005600160630     C/END-EXEC
005700160630
005800160630     C/EXEC SQL
005900160722     C+ OPEN B1
006000160630     C/END-EXEC
006100160630     C                   DO        *HIVAL
006200160630     C/EXEC SQL
006300160722     C+ FETCH NEXT FROM B1 INTO :azmgb
006400160630     C/END-EXEC
006500160630     C                   IF        SQLCod = 100 or sqlcod < 0
006600160630     c                   clear                   sqlcod
006700160630     C                   LEAVE
006800160630     C                   ENDIF
006900160628     c                   exsr      sr_scrivi
007000160628     C                   ENDDO
007100160628
007200160628     C/EXEC SQL
007300160722     C+ CLOSE B1
007400160628     C/END-EXEC
007500071228      //-----------------------------------------------------------------------
007600071228      //? Scrivo il file di work
007700071228      //-----------------------------------------------------------------------
007800071228     c     sr_scrivi     begsr
007900160630     c*
008000160721     c                   move      cliente       w70
008100160721     c     w70           chain     wfrtma1l
008200160721     c                   if        not %found(wfrtma1l)
008300160721     c                   clear                   wfrtma00
008400160721     c                   move      cliente       rtmakun
008500160721     c                   z-add     datadal       rtmadal
008600160721     c                   z-add     dataal        rtmaal
008700160721     c                   select
008800160721     c                   when      codice = 'B01'
008900160722     c                   add       quantita      rtmaqta1
009000160721     c                   when      codice = 'B02'
009100160722     c                   add       quantita      rtmaqta2
009200160721     c                   when      codice = 'B03'
009300160722     c                   add       quantita      rtmaqta3
009400160721     c                   endsl
009500160721      *  recupero i dati necessari da memorizzare sul file
009600160721     c                   exsr      sr_daticom
009700160721      *  recupero i valori sui codici figli
009800160721     c                   exsr      sr_figli
009900160722     c                   write     wfrtma00
010000160630     c                   else
010100160721     c                   select
010200160721     c                   when      codice = 'B01'
010300160722     c                   add       quantita      rtmaqta1
010400160721     c                   when      codice = 'B02'
010500160722     c                   add       quantita      rtmaqta2
010600160721     c                   when      codice = 'B03'
010700160722     c                   add       quantita      rtmaqta3
010800160721     c                   endsl
010900160721     c                   update    wfrtma00
011000160630     c                   end
011100071228
011200071228     c                   endsr
011300160721      //-----------------------------------------------------------------------
011400160721      //? Recupero i valori della movimentazione sui codici figli
011500160721      //-----------------------------------------------------------------------
011600160721     c     sr_figli      begsr
011700160721      *?recupero i figli
011800160721      * --> controllo se è padre
011900160721     c                   clear                   tibs10ds
012000160721     c                   move      dataal        d10drf
012100160721     c                   eval      d10tle = 'ST'
012200160721     c                   eval      d10paf = 'F'
012300160722     c                   eval      d10cod = rtmakun
012400160721     c                   call      'TIBS10R'
012500160721     c                   parm                    tibs10ds
012600160721     c                   if        d10err <>*blanks
012700160721     c                   eval      sk_fig(1) =   rtmakun
012800160721     c                   endif
012900160721      *?recupero i valori su ogni codice figlio da riportare sul record
013000160721     c     1             do        5000          x
013100160726     c                   clear                   valore
013200160721     c                   if        sk_fig(x) = 0
013300160721     c                   leave
013400160721     c                   endif
013500160721     c                   eval      w70 = sk_fig(x)
013600160721     C/EXEC SQL
013700160722     C+ DECLARE v1 CURSOR FOR SELECT  sum(sdprcp)
013800160722     C+ from sisdp00f where sdpann = :anno  and sdpmes = :mese and sdpsvr = '='
013900160722     C+ and sdpksc = :w70
014000160721     C/END-EXEC
014100160721
014200160721     C/EXEC SQL
014300160721     C+ OPEN v1
014400160721     C/END-EXEC
014500160721     C                   DO        *HIVAL
014600160721     C/EXEC SQL
014700160721     C+ FETCH NEXT FROM v1 INTO :valore
014800160721     C/END-EXEC
014900160721     C                   IF        SQLCod = 100 or sqlcod < 0
015000160721     c                   add       valore        rtmaimp
015100160721     C                   LEAVE
015200160721     C                   ENDIF
015300160721     C                   ENDDO
015400160721
015500160721     C/EXEC SQL
015600160721     C+ CLOSE v1
015700160721     C/END-EXEC
015800160721     c                   enddo
015900160721     c                   endsr
016000160630      //-----------------------------------------------------------------------
016100160630      //? Recupero i dati comuni da memorizzare sul file
016200160630      //-----------------------------------------------------------------------
016300160630     c     sr_daticom    begsr
016400160630
016500160630      *?recupero la ragione sociale del cliente
016600160630     c                   clear                   tibs69ds
016700160630     c                   clear                   ds_cnaco
016800160630     c                   clear                   ds_cnind
016900160630     c                   clear                   ds_cnclp
017000160630     c                   clear                   ds_fncls
017100160721     c                   eval      i69kac = rtmakun
017200160630     c                   call      'TIBS69R'
017300160630     c                   parm                    tibs69ds
017400160630     c                   parm                    ds_cnaco
017500160630     c                   parm                    ds_cnind
017600160630     c                   parm                    ds_cnclp
017700160630     c                   parm                    ds_fncls
017800160721     c                   eval      rtmakund = acorag
017900160630
018000160630      *?recupero la filiale dell'unificante
018100160721     c                   movel     rtmakun       rtmafil
018200160721     c     rtmafil       chain     azorg01l
018300160630     c                   if        %found(azorg01l)
018400160721     c                   eval      rtmafild = orgdes
018500160630
018600160630      *?recupero l'area dell'unificante
018700160721     c                   eval      rtmacar = orgcar
018800160630     c                   eval      kcod = '05'
018900160721     c                   eval      kkey = %editc(rtmacar:'X')
019000160630     c     ktabel        chain     tabel00f
019100160630     c                   if        %found(tabel00f)
019200160721     c                   eval      rtmacard = tbluni
019300160630     c                   endif
019400160630
019500160630      *?recupero il distretto dell'unificante
019600160721     c                   eval      rtmadit = orgfl3
019700160630     c                   eval      kcod = '17'
019800160721     c                   eval      kkey = rtmadit
019900160630     c     ktabel        chain     tabel00f
020000160630     c                   if        %found(tabel00f)
020100160721     c                   eval      rtmaditd = tbluni
020200160630     c                   endif
020300160630
020400160630     c                   endif
020500160630     c                   endsr
020600071228      //-----------------------------------------------------------------------
020700071228      //? Routine iniziale
020800071228      //-----------------------------------------------------------------------
020900071228     c     *inzsr        begsr
021000071228
021100071228     c     *entry        plist
021200071228     c                   parm                    kpjba
021300071228     c                   eval      param = kpjbu
021400160721     c                   z-add     1             datadal
021500160721     c                   movel     wanmes        datadal
021600160721     c                   move      datadal       dataiso
021700160721     c     dataiso       adddur    1:*m          dataiso
021800160721     c     dataiso       subdur    1:*d          dataiso
021900160721     c                   move      dataiso       dataal
022000071228
022100160721     c     ktabel        klist
022200160721     c                   kfld                    codut
022300160721     c                   kfld                    kcod
022400160721     c                   kfld                    kkey
022500071228      * data del giorno
022600071228     c                   time                    wtime
022700071228     c                   move      wtime         wdate
022800071228     c                   endsr
