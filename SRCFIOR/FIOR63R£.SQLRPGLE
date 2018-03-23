000100070720     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070720     h dftactgrp(*no) actgrp(*caller)
000300070723      * FIOR63R *----------------------------------------------------*
000400070720      *         assegnazione automatica distinte ORM
000500070720      *--------------------------------------------------------------*
000600070913     fFior63p   o    e             Printer
000700070720     fAzorg01l  if   e           k disk
000800070720      *--------------------------------------------------------------*
000900070720     ddtaiso           s               d
001000070720     ddtaeur           s               d   datfmt(*Eur)
001100070720     d fidstds       e ds                  extname(fidst00f)
001200070720     d ddatiute      e ds
001300070720     d azuteds       e ds                  extname(AZUTE00F)
001400070720     d tibs34ds      e dS                  inz
001500070720     d KPJBA         e dS
001600070720     d Fior62ds      e dS
001700070727     d Fior68ds      e dS
001800070720     d Fior66ds      e dS
001900070731     d Fior71ds      e ds
002000070912     d Fior76ds      e ds
002001080328     d Fior90ds      e ds
002100070720     d og147         e ds                  inz
002200070720      *--------------------------------------------------------------*
002300070720      *  Ciclo principale
002400070720      *--------------------------------------------------------------*
002500070720      * pulisco i riferimenti di ORG per gli ORM  non in distinta
002600070912      * (ORGNDC=0) solo se P.O. partito
002700070912     c                   if        dateu8 >= comdda
002800070720     c                   clear                   fior66ds
002900070723     c                   eval      or66cmti= 'S'
003000070723     c                   eval      or66fgsi= or62fgsi
003100070723     c                   eval      or66pag = pages
003200070723     c                   eval      or66riga = riga
003300070723     c                   eval      kpjbu = fior66ds
003400070723     c                   call      'FIOR65R'
003500070723     c                   parm                    kpjba
003600070723     c                   eval      fior66ds = kpjbu
003700070723     c                   eval      pages = or66pag
003800070723     c                   eval      riga = or66riga
003900070723     c                   if        or66erro<> ' '
004000070723     c                   exsr      srfine
004100070723     c                   end
004200070912     c                   end
004300070723
004400070723      * in caso di richiesta pulizia dell'assegnazione precedente leggo tutte le distinte
004500070723      * ed ad ognuna chiamo il pgm FIOR66R
004600070723
004700070723     c                   if        or62cleari= 'S'
004800070723     C/EXEC SQL
004900070723     C+ DECLARE B1 CURSOR FOR SELECT fidst02l.* FROM fidst02l WHERE
005000070723     C+ dstfgs =:or62fgsi and dstnpg=4 and dstdfv = :or62dfvi and dstatb
005100070723     C+ = ' ' and dstfcf = ' '
005200070723     C/END-EXEC
005300070723
005400070723     C/EXEC SQL
005500070723     C+ OPEN b1
005600070723     C/END-EXEC
005700070723
005800070723     C                   do        *hival
005900070723
006000070723     C/EXEC SQL
006100070723     C+ FETCH NEXT FROM b1 INTO :fidstds
006200070723     C/END-EXEC
006300070723
006400070723     C                   select
006500070723     C                   when      sqlcod = 100
006600070723     c                   leave
006700070723     C                   WHEN      SqlCod < 0
006800070723     c* record allocato
006900070723     c                   if        sqlcod <> -913
007000070723     C                   seton                                        H1
007100070723     c                   end
007200070723     c                   seton                                        81
007300070723     c                   exsr      srstampa
007400070723     c                   exsr      srfine
007500070723     c                   other
007501080414      * verifico se fatta fase RCH o RQE passo alla distinta successiva
007502080328     c                   exsr      sr_CtrFase
007503080328      * se fase  trovata
007504080328     c                   if        or90ndcok = '1'
007505080328     c                   iter
007506080328     c                   endif
007600070723     c* mattina/pomeriggio
007700070723     c                   if        or62fppi <> ' ' and dstfpp <> or62fppi
007800070723     c                   iter
007900070723     c                   end
008000070723     c* parcel/messaggerie
008100070723     c                   if        (or62tpmi= 'P' and dsttpm <>' ') or
008200070723     c                             (or62tpmi= 'M' and dsttpm <>'M')
008300070723     c                   iter
008400070723     c                   end
008500070723     c* se devo pulire i riferimenti della distinta chiamo il pgm FIOR66R
008600070723     c                   if        dstrfasd = '   '
008700070723     c                   clear                   fior66ds
008800070723     c                   eval      or66cmti= 'S'
008900070723     c                   eval      or66fgsi= dstfgs
009000070723     c                   eval      or66nfvi= dstnfv
009100070723     c                   eval      or66dfvi= dstdfv
009200070723     c                   eval      or66fppi= dstfpp
009300070723     c                   eval      or66tpmi= dsttpm
009400070723     c                   eval      or66clrman=or62clrman
009500071009     c                   eval      or66allega=or62allega
009600070723     c                   eval      or66pag = pages
009700070723     c                   eval      or66riga = riga
009800070723     c                   eval      kpjbu = fior66ds
009900070731     c                   call      'FIOR66R'
010000070723     c                   parm                    kpjba
010100070723     c                   eval      fior66ds = kpjbu
010200070723     c                   eval      pages = or66pag
010300070723     c                   eval      riga = or66riga
010400070723     c                   if        or66erro<> ' '
010500070723     c                   exsr      srfine
010600070723     c                   end
010700070723     c                   end
010800070723     C                   endsl
010900070723
011000070723     C                   enddo
011100070723
011200070723     c                   endif
011300070723
011400070723     C/EXEC SQL
011500070723     C+ CLOSE b1
011600070723     C/END-EXEC
011700070723
011800070727     c                   clear                   fior68ds
011900070727     c                   eval      or68cmti= 'S'
012000070727     C                   eval      or68FGSi = or62FGSi
012100070727     C                   eval      or68Dfvi = or62Dfvi
012200070727     C                   eval      or68FPpi = or62fppi
012300070727     C                   eval      or68tpmi = or62tpmi
012400071010     C                   eval      or68allega = or62allega
012500070727     C                   EVAL      or68PAG = PAGES
012600070727     C                   EVAL      or68RIGA= RIGA
012700070727     c                   eval      kpjbu = fior68ds
012800070727     c                   call      'FIOR68R'
012900070205     c                   parm                    kpjba
013000070727     c                   eval      fior68ds = kpjbu
013100070727     c                   eval      pages = or68pag
013200070727     c                   eval      riga = or68riga
013300070727      * se ritorno senza codice d'errore commit tutto
013400070727     c                   if        or68erro= ' '
013500070727     c                   if        or62cmti= 'S'
013600070727     c                   commit
013700070727     c                   end
013800070727      * aggiorno ORM solo se p.o. partito
013900070313     c                   if        dateu8 >= comdda
014000070727     c                   clear                   fior71ds
014100070727     c                   eval      or71cmti= 'S'
014200070727     C                   eval      or71FGSi = or62FGSi
014300070727     C                   eval      or71Dfvi = or62Dfvi
014400070727     C                   eval      or71FPpi = or62fppi
014500070727     C                   eval      or71tpmi = or62tpmi
014600070727     C                   EVAL      or71PAG = PAGES
014700070727     C                   EVAL      or71RIGA= RIGA
014800070727     c                   eval      kpjbu = fior71ds
014900070912     c                   call      'FIOR71R'
015000070313     c                   parm                    kpjba
015100070727     c                   eval      fior71ds = kpjbu
015200070727     c                   eval      pages = or71pag
015300070727     c                   eval      riga = or71riga
015400070727     c                   if        or71erro<>' '
015500070313     c                   exsr      srfine
015600070313     c                   end
015700070313     c                   end
015800070727      * aggiorno FIDST con valori assegnato
015900070727     c                   eval      kpjbu = fior68ds
016000070727     c                   call      'FIOR72R'
016100070327     c                   parm                    kpjba
016200070727     c                   eval      fior68ds = kpjbu
016300070727     c                   eval      pages = or68pag
016400070727     c                   eval      riga = or68riga
016500070727     c                   if        or68erro <>' '
016600070327     c                   exsr      srfine
016700070327     c                   end
016800070727      * lancio la stampa
016900070912     c                   clear                   fior76ds
017000070912     C                   eval      or76fil = or62fgsi
017100070912     C                   eval      or76dat = or62dfvi
017200070912     C                   eval      or76orma= 'S'
017300070912     C                   eval      or76noas= 'S'
017400070912     C                   eval      or76pame= or62tpmi
017500070926     C                   eval      or76fppi= or62fppi
017600070912     c                   eval      kpjbu = fior76ds
017700070912     c                   call      'FIOR76R'
017800070912     c                   parm                    kpjba
017900070731     c                   else
018000070731     c                   exsr      srfine
018100070731     c                   end
018200070731      *
018300070731     c                   eval      kpjbu = fior62ds
018400070925     c                   eval      *in82 = *on
018500070925     c                   exsr      srstampa
018600070731     c                   SETON                                        LR
018601080328      *---------------------------------------------------------------*
018602080414      *?Controllo fase RCH / RQE                                     ?*
018603080328      *---------------------------------------------------------------*
018604080328     c     sr_CtrFase    BEGSR
018605080328      *
018606080328     c                   clear                   fior90ds
018607080328
018608080328     c                   eval      or90tla = 'C'
018609080328     c                   eval      or90fas = 'RCH'
018610080328     c                   z-add     dstfgs        or90fgs
018611080328     c                   z-add     dstnfv        or90ndc
018613080328
018614080328     c                   call      'FIOR90R'
018615080328     c                   parm                    kpjba
018616080328     c                   parm                    fior90ds
018617080328
018618080414      * se fase non trovata  cerco  la fase RQE in alternativa alla RCH
018619080414     c                   if        or90ndcok = ' '
018621080414     c                   clear                   fior90ds
018622080414
018623080414     c                   eval      or90tla = 'C'
018624080414     c                   eval      or90fas = 'RQE'
018625080414     c                   z-add     dstfgs        or90fgs
018626080414     c                   z-add     dstnfv        or90ndc
018627080414
018628080414     c                   call      'FIOR90R'
018629080414     c                   parm                    kpjba
018630080414     c                   parm                    fior90ds
018631080414     c                   endif
018632080414
018633080328     c                   ENDSR
018700070731      **************************************************************************
018800070731      * esco con codice d'errore
018900070731      **************************************************************************
019000070731     c     srfine        BEGSR
019100070731
019200070731     c                   if        Or62cmti= 'S'
019300070731     c                   rolbk
019400070731     c                   end
019500070731
019600070731     c                   write     finest
019700070731     c                   eval      or62erro= '1'
019800070731     c                   eval      kpjbu = fior62ds
019900070731     c                   seton                                        lr
020000070731     c                   return
020100070731
020200070731     c                   endsr
020300070731      *------------------------------------------------------------------------*
020400070731      * stampa
020500070731      *------------------------------------------------------------------------*
020600070731     c     srstampa      BEGSR
020700070731     c                   move      or62dfvi      dtaiso
020800070222     c                   move      dtaiso        dtaeur
020900070222     c                   move      dtaeur        prtdfv
021000070731      * PAGINA IN STAMPA
021100070731     c                   IF        *IN80 OR RIGA >=60
021200070731     c                   SETON                                        80
021300070731     c                   ADD       1             PAGES
021400070731     c                   Z-ADD     3             RIGA              3 0
021500070731     c                   END
021600070731      *
021700070731     c   80              write     testa
021800070731     c                   setoff                                       80
021900070731     c   81              write     riga1
022000070731     c   81              ADD       1             RIGA
022100070731     c                   setoff                                       81
022200070925     c   82              write     finest
022300070925     c   82              ADD       1             RIGA
022400070925     c                   setoff                                       82
022500070731
022600070731     c                   endsr
022700070731      *--------------------------------------------------------------*
022800070731      *  Operazioni iniziali
022900070731      *--------------------------------------------------------------*
023000070731     c     *INZSR        BEGSR
023100070731
023200070731     c     *ENTRY        PLIST
023300070731     c                   PARM                    KPJBA
023400070731     c                   MOVEL(P)  KPJBU         Fior62ds
023500070731     c                   clear                   or62erro
023600070731
023700070731     c     *dtaara       define    §azute        azuteds
023800070731     c     *dtaara       define    §datiute      ddatiute
023900070731     c                   in(E)     *dtaara
024000070731     c                   IF        %Error  or  RSUT = *blanks
024100070731     c                   call      'TIBS34R'
024200070731     c                   parm                    Tibs34Ds
024300070731     c                   in        *dtaara
024400070731     c                   ENDIF
024500070731
024600070731     c                   TIME                    WHHUDT           14 0
024700070731     c                   MOVEL     WHHUDT        ORA               6 0
024800070731     c                   MOVE      WHHUDT        UDATE8            8 0
024900070731     c                   move      udate8        dtaeur
025000070731     c                   move      dtaeur        dtaiso
025100070510      *
025200070228     c                   move      dtaiso        dateu8            8 0
025300070510      *
025400070731     C     or62fgsi      CHAIN     AZORG01l
025500070228    1C                   IF        %found(azorg01l)
025600070731     C                   MOVEL     ORGde7        og147
025700070731      * se data distinte automatiche < oggi p.o. partito
025800070731     c                   if        §ogddao = ' '
025900070228     c                   exsr      srfine
026000070228     c                   else
026100070731     c                   move      §ogddao       comdda            8 0
026200070228     c                   end
026300070228     c                   end
026400070802      *
026500070802     C                   TIME                    TIMES             6 0
026600070802     C                   TIME                    WHDAT            12 0
026700070802     C                   MOVE      WHDAT         WOGGI             6 0
026800070731      * stampo testa
026900070731     c                   seton                                        80
027000070731     c                   exsr      srstampa
027100070731     C                   ENDSR
