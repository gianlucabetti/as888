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
001800160728     d w701            s              7  0
001900160728     d w8a             s              8
002000160727     d alfa8           s              8
002100160727     d xx              s              4  0 inz
002200160727     d x               s              4  0 inz
002300160722     d dataiso         s               d
002400160721     d wdate           s              8  0
002500071228     d wtime           s             14  0
002600071228      //-----------------------------------------------------------------------
002700071228      //? SCHIERE
002800071228      //-----------------------------------------------------------------------
002900071228
003000071228      //-----------------------------------------------------------------------
003100071228      //? STRUTTURE DATI
003200071228      //-----------------------------------------------------------------------
003300071228     d param           ds
003400160721     d  anno                   1      4s 0
003500160721     d  mese                   5      6s 0
003600160721     d  wanmes                 1      6s 0
003700071228
003800071228     d ds_cnaco      e ds                  inz extname(cnaco00f)
003900071228     d ds_cnind      e ds                  inz extname(cnind00f)
004000071228     d ds_cnclp      e ds                  inz extname(cnclp00f)
004100071228     d ds_fncls      e ds                  inz extname(fncls00f)
004200071228     d kpjba         e ds
004300071228     d tibs10ds      e ds
004400160721     d  sk_fig                21   5520  0 inz  dim(500)
004500071228     d tibs69ds      e ds
004600160721      *
004700160722     d azmgb           ds
004800160722     D  cliente                       8
004900160722     D  codice                       15
005000160722     D  quantita                     13  3
005100071228
005200071228      //-----------------------------------------------------------------------
005300160701     c                   eval      *inlr = *on
005400160721     c*
005500160630     C/EXEC SQL
005600160728     C+ DECLARE B1 CURSOR FOR SELECT mgbsog FROM azmgb00f
005700160728     C+ where mgbcau = 'RTMA' and mgbdat between :datadal and :dataal
005800160728     C+ group by mgbsog ORDER BY mgbsog
005900160630     C/END-EXEC
006000160630
006100160630     C/EXEC SQL
006200160722     C+ OPEN B1
006300160630     C/END-EXEC
006400160630     C                   DO        *HIVAL
006500160630     C/EXEC SQL
006600160727     C+ FETCH NEXT FROM B1 INTO :alfa8
006700160630     C/END-EXEC
006800160630     C                   IF        SQLCod = 100 or sqlcod < 0
006900160630     C                   LEAVE
007000160630     C                   ENDIF
007100160727      * cerca il codice del padre dai movimenti trovati
007200160727     c                   move      alfa8         w70
007300160727     c                   exsr      cercapadre
007400160728     c                   exsr      elabora
007500160728     c                   clear                   sqlcod
007600160727      *
007700160628     C                   ENDDO
007800160728     c/exec sql
007900160728     C+ close B1
008000160728     C/END-EXEC
008100160728     c/exec sql
008200160728     C+ close B2
008300160728     C/END-EXEC
008400160628
008500160728     c*________________________________________________________________
008600160728     c     elabora       begsr
008700160728     c*________________________________________________________________
008800160728      * trovato il padre o codice singolo verifico se ho già scritto il rekord se si lo ignoro
008900160728      * altrimenti vado a riesplodere i movimenti con il padre e tutti i figli
009000160728     c                   eval      w701 = sk_fig(1)
009100160728     c     w701          chain     wfrtma1l
009200160728     c                   if        not %found(wfrtma1l)
009300160728     c     1             do        5000          xx
009400160728     c                   clear                   valore
009500160728     c                   if        sk_fig(xx) = 0
009600160728     c                   leave
009700160728     c                   endif
009800160728     c                   move      sk_fig(xx)    w8a
009900160728     C/EXEC SQL
010000160728     C+ DECLARE B2 CURSOR FOR SELECT mgbsog, mgbcod, sum(mgbqta) FROM
010100160728     C+ azmgb00f WHERE mgbcau = 'RTMA' and mgbdat between :datadal and
010200160728     C+ :dataal and mgbsog = :w8a GROUP BY mgbsog, mgbcod
010300160728     C/END-EXEC
010400160728
010500160728     C/EXEC SQL
010600160728     C+ OPEN B2
010700160728     C/END-EXEC
010800160728     C                   DO        *HIVAL
010900160728     C/EXEC SQL
011000160728     C+ FETCH NEXT FROM B2 INTO :azmgb
011100160728     C/END-EXEC
011200160728     C                   IF        SQLCod = 100 or sqlcod < 0
011300160728     c/exec sql
011400160728     C+ close B2
011500160728     C/END-EXEC
011600160728     c                   clear                   sqlcod
011700160728     C                   LEAVE
011800160728     C                   ENDIF
011900160728     c                   exsr      sr_scrivi
012000160728     c                   enddo
012100160728     c                   enddo
012200160728     c                   endif
012300160728     c                   endsr
012400071228      //-----------------------------------------------------------------------
012500071228      //? Scrivo il file di work
012600071228      //-----------------------------------------------------------------------
012700071228     c     sr_scrivi     begsr
012800160630     c*
012900160801     c     quantita      mult      -1            quantita
013000160728     c     w701          chain     wfrtma1l
013100160721     c                   if        not %found(wfrtma1l)
013200160721     c                   clear                   wfrtma00
013300160727     c                   move      sk_fig(1)     rtmakun
013400160721     c                   z-add     datadal       rtmadal
013500160721     c                   z-add     dataal        rtmaal
013600160721     c                   select
013700160721     c                   when      codice = 'B01'
013800160722     c                   add       quantita      rtmaqta1
013900160721     c                   when      codice = 'B02'
014000160722     c                   add       quantita      rtmaqta2
014100160721     c                   when      codice = 'B03'
014200160722     c                   add       quantita      rtmaqta3
014300160721     c                   endsl
014400160721      *  recupero i dati necessari da memorizzare sul file
014500160721     c                   exsr      sr_daticom
014600160721      *  recupero i valori sui codici figli
014700160721     c                   exsr      sr_figli
014800160722     c                   write     wfrtma00
014900160630     c                   else
015000160721     c                   select
015100160721     c                   when      codice = 'B01'
015200160722     c                   add       quantita      rtmaqta1
015300160721     c                   when      codice = 'B02'
015400160722     c                   add       quantita      rtmaqta2
015500160721     c                   when      codice = 'B03'
015600160722     c                   add       quantita      rtmaqta3
015700160721     c                   endsl
015800160721     c                   update    wfrtma00
015900160630     c                   end
016000071228
016100071228     c                   endsr
016200160727      //-----------------------------------------------------------------------
016300160727      //? Recupero i  codici figli
016400160727      //-----------------------------------------------------------------------
016500160727     c     cercapadre    begsr
016600160727      *?recupero i figli
016700160727      * --> controllo se è padre
016800160727     c                   clear                   tibs10ds
016900160727     c                   move      dataal        d10drf
017000160727     c                   eval      d10tle = 'ST'
017100160727     c                   eval      d10paf = 'F'
017200160727     c                   eval      d10cod = w70
017300160727     c                   call      'TIBS10R'
017400160727     c                   parm                    tibs10ds
017500160727     c                   if        d10err <>*blanks
017600160727     c                   eval      sk_fig(1) =   w70
017700160727     c                   endif
017800160727     c                   endsr
017900160721      //-----------------------------------------------------------------------
018000160721      //? Recupero i valori della movimentazione sui codici figli
018100160721      //-----------------------------------------------------------------------
018200160721     c     sr_figli      begsr
018300160727     c                   clear                   valore
018400160721      *?recupero i valori su ogni codice figlio da riportare sul record
018500160721     c     1             do        5000          x
018600160726     c                   clear                   valore
018700160721     c                   if        sk_fig(x) = 0
018800160721     c                   leave
018900160721     c                   endif
019000160721     c                   eval      w70 = sk_fig(x)
019100160721     C/EXEC SQL
019200160727     C+ select sum(sdprcp) into :valore
019300160728     C+ from sisdp00f where sdpann = :anno  and sdpmes = :mese
019400160728     C+ and sdpsvr = '='
019500160722     C+ and sdpksc = :w70
019600160721     C/END-EXEC
019700160721
019800160727     C                   IF        SQLCod = 0
019900160721     c                   add       valore        rtmaimp
020000160721     C                   ENDIF
020100160721
020200160721     c                   enddo
020300160721     c                   endsr
020400160630      //-----------------------------------------------------------------------
020500160630      //? Recupero i dati comuni da memorizzare sul file
020600160630      //-----------------------------------------------------------------------
020700160630     c     sr_daticom    begsr
020800160630
020900160630      *?recupero la ragione sociale del cliente
021000160630     c                   clear                   tibs69ds
021100160630     c                   clear                   ds_cnaco
021200160630     c                   clear                   ds_cnind
021300160630     c                   clear                   ds_cnclp
021400160630     c                   clear                   ds_fncls
021500160721     c                   eval      i69kac = rtmakun
021600160630     c                   call      'TIBS69R'
021700160630     c                   parm                    tibs69ds
021800160630     c                   parm                    ds_cnaco
021900160630     c                   parm                    ds_cnind
022000160630     c                   parm                    ds_cnclp
022100160630     c                   parm                    ds_fncls
022200160721     c                   eval      rtmakund = acorag
022300160630
022400160630      *?recupero la filiale dell'unificante
022500160721     c                   movel     rtmakun       rtmafil
022600160721     c     rtmafil       chain     azorg01l
022700160630     c                   if        %found(azorg01l)
022800160721     c                   eval      rtmafild = orgdes
022900160630
023000160630      *?recupero l'area dell'unificante
023100160721     c                   eval      rtmacar = orgcar
023200160630     c                   eval      kcod = '05'
023300160721     c                   eval      kkey = %editc(rtmacar:'X')
023400160630     c     ktabel        chain     tabel00f
023500160630     c                   if        %found(tabel00f)
023600160721     c                   eval      rtmacard = tbluni
023700160630     c                   endif
023800160630
023900160630      *?recupero il distretto dell'unificante
024000160721     c                   eval      rtmadit = orgfl3
024100160630     c                   eval      kcod = '17'
024200160721     c                   eval      kkey = rtmadit
024300160630     c     ktabel        chain     tabel00f
024400160630     c                   if        %found(tabel00f)
024500160721     c                   eval      rtmaditd = tbluni
024600160630     c                   endif
024700160630
024800160630     c                   endif
024900160630     c                   endsr
025000071228      //-----------------------------------------------------------------------
025100071228      //? Routine iniziale
025200071228      //-----------------------------------------------------------------------
025300071228     c     *inzsr        begsr
025400071228
025500071228     c     *entry        plist
025600071228     c                   parm                    kpjba
025700071228     c                   eval      param = kpjbu
025800160721     c                   z-add     1             datadal
025900160721     c                   movel     wanmes        datadal
026000160721     c                   move      datadal       dataiso
026100160721     c     dataiso       adddur    1:*m          dataiso
026200160721     c     dataiso       subdur    1:*d          dataiso
026300160721     c                   move      dataiso       dataal
026400071228
026500160721     c     ktabel        klist
026600160721     c                   kfld                    codut
026700160721     c                   kfld                    kcod
026800160721     c                   kfld                    kkey
026900071228      * data del giorno
027000071228     c                   time                    wtime
027100071228     c                   move      wtime         wdate
027200071228     c                   endsr
