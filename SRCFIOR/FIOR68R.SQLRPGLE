000100070725     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070725     h dftactgrp(*no) actgrp(*caller)
000300070725      * FIOR68R *----------------------------------------------------*
000400070725      *         assegnazione automatica distinte Ritiri   x giro
000500070725      *--------------------------------------------------------------*
000600070913     fFior63p   o    e             Printer
000700070725     fFidst09l  uf   e           k disk    commit infds(dstinfds)
000800070725      *--------------------------------------------------------------*
000900070725      *   SCHIERE
001000070725      *--------------------------------------------------------------*
001100070725     d Errmsg          s             78    dim(1) ctdata perrcd(1)              MSG DI ERRORE
001200070725      *--------------------------------------------------------------*
001300070725      *   DS
001400070725      *--------------------------------------------------------------*
001500070725     d trul82ds      e ds
001600070725     d fidst5ds      e ds                  extname(fidst50f)
001700070725     d fidstds       e ds                  extname(fidst00f)
001800070725     d ddatiute      e ds
001900070725     d azuteds       e ds                  extname(azute00f)
002000070725     d tibs34ds      e ds                  inz
002100070725     d Kpjba         e ds
002200070725     d Fior68ds      e ds
002300070725     d Fior69ds      e ds
002301080414     d Fior90ds      e ds
002400070725     d distinta        ds
002500070725     d  dfv                                like(dst5dfv)
002600070725     d  nfv                                like(dst5nfv)
002700070725     d dstinfds        ds
002800070725     d  dstnrr               397    400i 0
002900070725      *--------------------------------------------------------------*
003000070725      *   VARIABILI
003100070725      *--------------------------------------------------------------*
003200070725     ddtaiso           s               d
003300070725     ddtaeur           s               d   datfmt(*Eur)
003400070725     d compkl          s                   INZ like(dstrpkl)
003500070725     d comvlu          s                   INZ like(dstrvlu)
003600070725     d comnftl         s                   INZ like(dstrnftl)
003700070725     d savdist         s                   like(distinta)
003800070725      *--------------------------------------------------------------*
003900070725      *  Ciclo principale
004000070725      *--------------------------------------------------------------*
004100070725     C                   do        99            xx                3 0
004200070725      * leggo in Fidst50F ordinato per data, priorità, distinta e sequenza
004300070725      * giro
004400070725     C/EXEC SQL
004500070725     C+ DECLARE B1 CURSOR FOR SELECT fidst50f.* FROM fidst50f WHERE
004600070725     C+ dst5fgs =:or68fgsi and dst5npg=4 and dst5dfv = :or68dfvi and dst5atb
004700070725     C+  = ' '  and dst5prgg = :xx
004800070725     C+ ORDER BY DST5FGS, DST5NPG, DST5DFV, DST5NFV, DST5SQL
004900070725     C/END-EXEC
005000070725
005100070725     C/EXEC SQL
005200070725     C+ OPEN b1
005300070725     C/END-EXEC
005400070725
005500070725     C                   do        *hival
005600070725
005700070725     C/EXEC SQL
005800070725     C+ FETCH NEXT FROM b1 INTO :fidst5ds
005900070725     C/END-EXEC
006000070725
006100070725     c                   select
006200070725     c                   when      sqlcod = 100
006300070725     c                   leave
006400070725     c                   WHEN      SqlCod < 0
006500070725      * record allocato
006600070725     c                   if        sqlcod <> -913
006700070725     c                   seton                                        H1
006800070725     c                   end
006900070725     c                   seton                                        81
007000070725     c                   exsr      srstampa
007100070725     c                   exsr      srfine
007200070725     c                   other
007300070725     c                   eval      dfv = dst5dfv
007400070725     c                   eval      nfv = dst5nfv
007500070725     c                   if        distinta <> savdist and or68cmti='S'
007600070725     c                   commit
007700070725     c                   eval      savdist = distinta
007800070725     c                   end
007900070725     c                   exsr      gesdis
008000070725     c                   endsl
008100070725
008200070725     c                   enddo
008300070725
008400070725     C/EXEC SQL
008500070725     C+ CLOSE b1
008600070725     C/END-EXEC
008700070725
008800070725     c                   if        xx = 99
008900070725     c                   leave
009000070725     c                   end
009100070725     c                   enddo                                                  ---- 01 ----
009200070725
009300070725     c                   if        or68cmti= 'S'
009400070725     c                   commit
009500070206     c                   end
009600070725     c                   eval      or68pag = pages
009700070725     c                   eval      or68riga= riga
009800070725
009900070725     c                   eval      kpjbu = fior68ds
010000070725     c                   seton                                        lr
010100070725      *--------------------------------------------------------------*
010200070725      *  Gestione singola distinta
010300070725      *--------------------------------------------------------------*
010400070725     c     GESDIS        BEGSR
010500070725
010600070725     c     kdst          chain(e)  FIDST09l
010700070725     c                   if        %error
010800070725      * reperisco il RRN del file
010900070725     c     kdst          chain(n)  FIDST09l
011000070725     c                   if        %found(FIDST09l)
011100070725     c                   exsr      geslck
011200070725     c                   if        ul82§sts = 'A'
011300070725     c                   seton                                        82
011400070725     c                   exsr      srstampa
011500070725     c                   leavesr
011600070725     c                   end
011700070725     c                   end
011800070725     c                   end
011900070725
012000070725     c     kdst          chain(e)  FIDST09l
012100070725     c                   if        not %found(FIDST09l) or %error
012200070725     c                   seton                                        82
012300070725     c                   exsr      srstampa
012400070725     c                   else
012500070725      * controllo
012600070725     c                   exsr      srctrdis
012700070725      * se non ci sono errori sulla distinta proseguo
012800070725     c                   if        not *in28
012900070725     c                   seton                                        87
013000070725     c                   exsr      srstampa
013100070725     c                   clear                   fior69ds
013200070725     c                   eval      or69fgsi = dstfgs
013300070725     c                   eval      or69npgi = dstnpg
013400070725     c                   eval      or69dfvi = dstdfv
013500070725     c                   eval      or69nfvi = dstnfv
013600070725     c                   eval      or69pdri = dstpdr
013700070725     c                   eval      or69fgsgi= dst5fgsg
013800070725     c                   eval      or69cgii = dst5cgi
013900070725     c                   eval      or69rpkli = compkl
014000070725     c                   eval      or69rvlui = comvlu
014100070725     c                   eval      or69rnftli= comnftl
014200071010     c                   eval      or69allega= or68allega
014201161108     c                   eval      or69finoa = or68finoa
014300070725     c                   eval      or69pag= pages
014400070725     c                   eval      or69riga= RIGA
014500070725     c                   movel(p)  Fior69ds      Kpjbu
014600070725     c                   call      'FIOR69R'
014700070725     c                   parm                    kpjba
014800070725     c                   parm                    fidstds
014900070725     c                   eval      fior69ds=kpjbu
015000070725     c                   eval      pages = or69pag
015100070725     c                   eval      riga = or69riga
015200070725      * se ritorno senza errore aggiorno la distinta con il peso/volume/
015300070725      * fermate attribuite
015400070725     c                   if        or69erro= ' '
015500070725     c                   eval      dstrpkla = or69rpklo +  dstrpkla
015600070725     c                   eval      dstrvlua = or69rvluo +  dstrvlua
015700070725     c                   eval      dstrstpa = or69rnftlo + dstrstpa
015800070725     c                   eval      dstrnbla = or69rnblo +  dstrnbla
015900070725     c                   eval      dstrnbaa = or69rnbao +  dstrnbaa
016000070725     c                   update    fidst000
016100070725     c                   end
016200070725     c                   end
016300070725     c                   end                                                    Trovate spunte
016400070725
016500070725     c                   endsr
016600070725      *-----------------------------------------------
016700070725     c     srctrdis      begsr
016800070725      *-----------------------------------------------
016900070725     c                   setoff                                       28
017000070725      * distinta annullata
017100070725     c                   if        dstatb <> ' '
017200070223     c                   seton                                        28
017300070725     c                   leavesr
017400070725     c                   end
017500070725      * distinta chiusa
017600070725     c                   if        dstfcf <> ' '
017700070725     c                   seton                                        28
017800070725     c                   leavesr
017900070725     c                   end
018000070725      * prestazione mattina/pomeriggio
018100070725     c                   if        dstfpp <> or68fppi and or68fppi <> ' '
018200070725     c                   seton                                        28
018300070725     c                   leavesr
018400070725     c                   end
018500070725      * parcel/messaggerie
018600070725     c                   if        (or68tpmi= 'P' and dsttpm <>' ') or
018700070725     c                             (or68tpmi= 'M' and dsttpm <>'M')
018800070725     c                   seton                                        28
018900070725     c                   leavesr
019000070725     c                   end
019001080414      * controllo le fasi della distinta se già quadrata non assegno
019002080414     c                   exsr      sr_ctrfase
019003080414      * se fase  trovata
019004080414     c                   if        or90ndcok = '1'
019005080414     c                   seton                                        28
019006080414     c                   leavesr
019008080414     c                   endif
019100070725      * capienza
019200070725     c                   eval      compkl = dstrpkl
019300070725     c                   eval      comvlu = dstrvlu
019400070725     c                   eval      comnftl= dstrnftl
019500070725
019600070725     c                   if        dstrpkla >= compkl or
019700070725     c                             dstrvlua >= comvlu or
019800070725     c                             dstrstpa >= comnftl
019900070725     c                   seton                                        28
020000070725     c                   leavesr
020100070725     c                   else
020200070725     c                   eval      compkl = compkl - dstrpkla
020300070725     c                   eval      comvlu = comvlu - dstrvlua
020400070725     c                   eval      comnftl= comnftl- dstrstpa
020500070725     c                   end
020600070725
020700070725     c                   ENDSR
020701080414      *---------------------------------------------------------------*
020702080414      *?Controllo fase RCH/RQE                                       ?*
020703080414      *---------------------------------------------------------------*
020704080414     c     sr_CtrFase    BEGSR
020705080414      *
020706080414     c                   clear                   fior90ds
020707080414
020708080414     c                   eval      or90tla = 'C'
020709080414     c                   eval      or90fas = 'RCH'
020710080414     c                   z-add     dstfgs        or90fgs
020711080414     c                   z-add     dstnfv        or90ndc
020712080414
020713080414     c                   call      'FIOR90R'
020714080414     c                   parm                    kpjba
020715080414     c                   parm                    fior90ds
020716080414
020717080414      * se fase non trovata  cerco  la fase RQE in alternativa alla RCH
020718080414     c                   if        or90ndcok = ' '
020719080414     c                   clear                   fior90ds
020720080414
020721080414     c                   eval      or90tla = 'C'
020722080414     c                   eval      or90fas = 'RQE'
020723080414     c                   z-add     dstfgs        or90fgs
020724080414     c                   z-add     dstnfv        or90ndc
020725080414
020726080414     c                   call      'FIOR90R'
020727080414     c                   parm                    kpjba
020728080414     c                   parm                    fior90ds
020729080414     c                   endif
020730080414
020731080414     c                   ENDSR
020800070725      **************************************************************************
020900070725      * esco con codice d'errore
021000070725      **************************************************************************
021100070725     c     srfine        BEGSR
021200070725
021300070725     c                   if        or68cmti= 'S'
021400070725     c                   rolbk
021500070725     c                   end
021600070725
021700070725     c                   eval      or68erro= '1'
021800070725     c                   eval      kpjbu = fior68ds
021900070725     c                   seton                                        lr
022000070725     c                   return
022100070725
022200070725     c                   endsr
022300070725      *------------------------------------------------------------------------*
022400070725      * stampa
022500070725      *------------------------------------------------------------------------*
022600070725     c     srstampa      BEGSR
022700070725     c                   move      or68dfvi      dtaiso
022800070725     c                   move      dtaiso        dtaeur
022900070222     c                   move      dtaeur        prtdfv
023000070725     c                   eval      prtnfv=dst5nfv
023100070725     c                   eval      prtprgg=dst5prgg
023200070725     c                   eval      prtsql=dst5sql
023300070725     c                   eval      prtfgsg=dst5fgsg
023400070725     c                   eval      prtcgi=dst5cgi
023500070725      * PAGINA IN STAMPA
023600070725     c                   IF        *IN80 OR RIGA >=60
023700070725     c                   SETON                                        80
023800070725     c                   ADD       1             PAGES
023900070725     c                   Z-ADD     3             RIGA              3 0
024000070725     c                   END
024100070725
024200070222     c   80              write     testa
024300070222     c                   setoff                                       80
024400070222     c   81              write     riga1
024500070725     c   81              ADD       1             RIGA
024600070222     c   82              write     riga2
024700070725     c   82              ADD       1             RIGA
024800070402     c   87              write     riga7
024900070725     c   87              ADD       1             RIGA
025000070725     c                   setoff                                       818287
025100070725
025200070725     c                   endsr
025300070725      *--------------------------------------------------------------*
025400070725      *  Operazioni iniziali
025500070725      *--------------------------------------------------------------*
025600070725     c     *INZSR        begsr
025700070725
025800070725     c     *Entry        plist
025900070725     c                   parm                    kpjba
026000070725     c                   movel(p)  kpjbu         Fior68ds
026100070725     c                   clear                   or68erro
026200070725     c                   eval      pages = or68pag
026300070725     c                   eval      riga = or68riga
026400070725
026500070725     c     *dtaara       define    §azute        azuteds
026600070725     c     *dtaara       define    §datiute      ddatiute
026700070725     c                   in(E)     *dtaara
026800070725     c                   IF        %Error  or  RSUT = *blanks
026900070725     c                   call      'TIBS34R'
027000070725     c                   parm                    Tibs34Ds
027100070725     c                   in        *dtaara
027200070725     c                   ENDIF
027300070725      *  Definisco chiavi di accesso
027400070725     c     Kdst          KLIST
027500070725     c                   KFLD                    dst5fgs
027600070725     c                   KFLD                    dst5npg
027700070725     c                   KFLD                    dst5nfv
027800070725
027900070725     c                   TIME                    TIMES             6 0
028000070725     c                   TIME                    WHDAT            12 0
028100070725     c                   MOVE      WHDAT         WOGGI             6 0
028200070725     c                   ENDSR
028300070725      *----------------------------------------------------*
028400070725     c     geslck        begsr
028500070725      *----------------------------------------------------*
028600070725     c                   clear                   trul82ds
028700070725     c                   Eval      ul82§rrn = dstnrr
028800070725     c                   Eval      UL82§FIL = 'FIDST00F'
028900070518     c                   Eval      UL82§WIN = 'S'
029000070518     c                   Eval      UL82§num = 2
029100070518     c                   Eval      UL82§att = 2
029200070518     c                   eval      ul82§mss = ErrMsg(1)
029300070518     c                   Eval      UL82§msw = 'La Distinta '
029400070725     c                             + %editc(dst5nfv:'Z') +  ' è già utilizzata -
029500070518     c                             da un altro lavoro e non è manutenzionabile'
029600070518      * Effettuo la chiamata al *PGM d utilità
029700070518     c                   Call(e)   'TRUL82R'
029800070518     c                   Parm                    Trul82ds
029900070518
030000070518     C                   ENDSR
030100070518** ErrMsg
030200070518Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
