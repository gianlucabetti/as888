000100070723     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070723     h dftactgrp(*no) actgrp(*caller)
000300070723      * FIOR66R *----------------------------------------------------*
000400070723      *         Elimina riferimenti distinta da FNORG/FNORM
000500070723      *--------------------------------------------------------------*
000600070913     fFior63p   o    e             Printer
000700070723     fAzorg01l  if   e           k Disk
000800070723     fFnorg03L  if   e           k Disk    infds(orginfds)
000900070723     f                                     rename(fnorg000:fnorg00)
001000070723     fFnorg00f  uf   e             Disk    commit
001100070925     fFnorm01l  if   e           k Disk
001200070723      *--------------------------------------------------------------*
001300070723      *   DS
001400070723      *--------------------------------------------------------------*
001500070723     d orginfds        ds
001600070723     d  orgnrr               397    400i 0
001700070723     d ddatiute      e ds
001800070723     d azuteds       e ds                  extname(AZUTE00F)
001900070723     d tibs34ds      e ds                  inz
002000070723     d kpjba         e ds
002100070723     d Fior66ds      e ds
002200070723     d Fior61ds      e ds
002300070724     d Fior67ds      e ds
002400070723     d trul82ds      e ds
002500070723     d og147         e ds                  inz
002600071009     d dorm01        e ds
002700080314     d dorg01        e ds
002800070723     ddtaiso           s               d
002900070723     ddtaeur           s               d   datfmt(*eur)
003000070723     d Errmsg          s             78    dim(1) Ctdata perrcd(1)              Msg di errore
003100070723      *--------------------------------------------------------------*
003200070723      *  Ciclo principale
003300070723      *--------------------------------------------------------------*
003400070723
003500070723      *  aggiorno FNORG00F
003600070723
003700070723     c     korg          setll     fnorg03l
003800070723     c                   do        *hival
003900070723     c     korg          reade     fnorg03l
004000070723     c                   if        %eof(fnorg03l)
004100070723     c                   leave
004200070723     c                   end
004300070723     c     orgnrr        chain(e)  fnorg00f
004400070723     c                   if        %error
004500070723     c                   exsr      geslckorg
004600070723     c                   if        ul82§sts = 'A'
004700070723     c                   seton                                        83
004800070723     c                   exsr      srstampa
004900070723     c                   exsr      srfine
005000070723     c                   else
005100070723     c     orgnrr        chain     fnorg00f
005200070723     c                   end
005300070723     c                   end
005400070723
005500070723      * lascio le spedizioni inserite manualmente se non espressamente
005600070723      * richiesta la pulizia (solo per EDP)
005700070723
005800070723     c                   if        or66clrman <>'S' and
005900070723     c                             orgnftl = 99999
006000070223     c                   iter
006100070223     c                   end
006200111129
006300111129      * Lascio gli ORM RC in quanto legati alla Bolla
006400111129     c                   if        ORGnsr > 0
006500111129     c                   iter
006600111129     c                   endif
006700070208
006800070925      * verifico se l'ORM risulta comunicato  passo al successivo
006900080314      * o perchè stampato o perchè inviato al palmare
007000070926      * solo se sono già partita (non in simulazione)
007100071009     c     korm          chain     fnorm01l
007200071010     c                   if        not %found(fnorm01l)
007300071010     c                   iter
007400071010     c                   endif
007500071010
007600080314     c                   movel     orgflo        dorg01
007700080314
007800080314     c                   if        (dateu8 >= comdda  and
007900080314     c                             ormdst > 0) or
008000080314     c                             §orgpda = 'S'
008100070925     c                   iter
008200070925     c                   endif
008300071009      * verifico se richesta selezione sugli orm con allegati
008400071011     c*                  if        or66allega <> ' '
008500071011     c*                  movel     ormflo        dorm01
008600071011     c*                  if        or66allega = 'E' and (§orsrm = 'S' or
008700071011     c*                            ormtor = 'P')
008800071011     c*                  iter
008900071011     c*                  endif
009000071011     c*                  if        or66allega = 'I' and (§orsrm = ' ' and
009100071011     c*                            ormtor <> 'P')
009200071011     c*                  iter
009300071011     c*                  endif
009400071011     c*                  endif
009500070925
009600070723     c                   eval      orgndc = 0
009700070723     c                   eval      orgpdc = 0
009800070723     c                   eval      orgfgs = 0
009900070723     c                   eval      orgddc = 0
010000070723     c                   eval      orgnftl= 0
010100070723     c                   eval      orgslo = 0
010200070723     c                   eval      orgdtvdis= 0
010300070723     c                   eval      orghvdis= 0
010400070723     c                   update    fnorg000
010500070208     c                   enddo
010600070723     c
010700070723      * Richiamo il programma per togliere i riferimenti da FNORM e da FNORF
010800070723      * solo se sono già partita
010900070313     c                   if        dateu8 >= comdda
011000070723      * faccio la pulizia delle assegnazioni automatiche
011100070723     c                   clear                   fior61ds
011200070723     c                   eval      or61cmti = 'S'
011300070723     c                   eval      or61fgsi = or66fgsi
011400070723     c                   eval      or61dfvi = or66dfvi
011500070723     c                   eval      or61nfvi = or66nfvi
011600070723     c                   eval      or61nftl = *zeros
011700070723     c                   eval      or61mtdi = 'T'
011800070723     c                   eval      or61pag = pages
011900070723     c                   eval      or61riga= riga
012000070723
012100070731     c                   call      'FIOR61R'
012200070723     c                   parm                    kpjba
012300070723     c                   parm                    fior61ds
012400070723     c                   eval      pages = or61pag
012500070723     c                   eval      riga = or61riga
012600070723     c                   if        or61erro<> ' '
012700070723     c                   exsr      srfine
012800070723     c                   end
012900071010      * faccio la pulizia delle assegnazioni manuali se richiesto
013000070723     c                   If        or66clrman = 'S'
013100070723     c                   clear                   fior61ds
013200070723     c                   eval      or61cmti = 'S'
013300070723     c                   eval      or61fgsi = or66fgsi
013400070723     c                   eval      or61dfvi = or66dfvi
013500070723     c                   eval      or61nfvi = or66nfvi
013600070723     c                   eval      or61nftl = *all'9'
013700070723     c                   eval      or61mtdi = 'T'
013800070723     c                   eval      or61pag = pages
013900070723     c                   eval      or61riga= riga
014000070723
014100070731     c                   call      'FIOR61R'
014200070723     c                   parm                    kpjba
014300070723     c                   parm                    fior61ds
014400070723     c                   if        or61erro<> ' '
014500070723     c                   exsr      srfine
014600070723     c                   end
014700070723
014800070723     c                   endif
014900070723
015000070723     c                   endif
015100070223     c* FIDST00F
015200070723     c* chiamo il pgm che aggiorna valori assegnati nel caso ci siano ORM
015300070723     c* manuali
015400070723     c                   clear                   fior67ds
015500070723     c                   eval      or67fgsi = or66fgsi
015600070724     c                   eval      or67dfvi = or66dfvi
015700070724     c                   eval      or67nfvi = or66nfvi
015800070723     c                   eval      or67cmtI = 'N'
015900070723     c                   eval      kpjbu = fior67ds
016000070723     c                   call      'FIOR67R'
016100070723     c                   parm                    kpjba
016200070723     c                   eval      fior67ds = kpjbu
016300070723     c                   if        or67erro <> ' '
016400070723     c                   seton                                        81
016500070723     c                   exsr      srstampa
016600070723     c                   exsr      srfine
016700070723     c                   end
016800070723
016900070723     c                   if        or66cmti= 'S'
017000070723     c                   commit
017100070723     c                   end
017200070723
017300070723     c                   eval      or66pag = pages
017400070723     c                   eval      or66riga= riga
017500070723
017600070723     c                   eval      kpjbu = fior66ds
017700070723     c                   seton                                        lr
017800070723      *--------------------------------------------------------------*
017900070723      *  Operazioni finali in caso di errore
018000070723      *--------------------------------------------------------------*
018100070723     c     srfine        BEGSR
018200070723     c                   if        or66cmti= 'S'
018300070723     c                   rolbk
018400070723     c                   end
018500070723     c                   eval      or66erro= '1'
018600070723     c                   eval      kpjbu = fior66ds
018700070723     c                   seton                                        lr
018800070723     c                   return
018900070723     c                   endsr
019000070723      *------------------------------------------------------------------------*
019100070723      * stampa
019200070723      *------------------------------------------------------------------------*
019300070723     c     srstampa      BEGSR
019400070723     c                   move      or66dfvi      dtaiso
019500070723     c                   move      dtaiso        dtaeur
019600070723     c                   move      dtaeur        prtdfv
019700070723      * PAGINA IN STAMPA
019800070723     c                   IF        *IN80 OR RIGA >=60
019900070723     c                   SETON                                        80
020000070723     c                   ADD       1             PAGES
020100070723     c                   Z-ADD     3             RIGA              3 0
020200070723     c                   END
020300070723      *
020400070723     c   80              write     testa
020500070723     c                   setoff                                       80
020600070723     c   81              write     riga1
020700070723     c   81              ADD       1             RIGA
020800070723     c   83              write     riga3
020900070723     c   83              ADD       1             RIGA
021000070723     c                   setoff                                       8183
021100070723      *
021200070723     c                   endsr
021300070723      *--------------------------------------------------------------*
021400070723      *  Operazioni iniziali
021500070723      *--------------------------------------------------------------*
021600070723     c     *INZSR        Begsr
021700070723
021800070723     c     *ENTRY        Plist
021900070723     c                   parm                    kpjba
022000070723     c                   movel(p)  Kpjbu         Fior66ds
022100070723     c                   clear                   or66erro
022200070723     c                   eval      pages = or66pag
022300070723     c                   eval      riga = or66riga
022400070723
022500070723     c     *dtaara       define    §azute        azuteds
022600070723     c     *dtaara       define    §datiute      ddatiute
022700070723     c                   in(E)     *dtaara
022800070723     c                   IF        %Error  or  RSUT = *blanks
022900070723     c                   call      'TIBS34R'
023000070723     c                   parm                    Tibs34Ds
023100070723     c                   in        *dtaara
023200070723     c                   ENDIF
023300070723
023400070723     C                   TIME                    WHHUDT           14 0
023500070723     C                   MOVEL     WHHUDT        ORA               6 0
023600070723     C                   MOVE      WHHUDT        UDATE8            8 0
023700070723     c                   move      udate8        dtaeur
023800070723     c                   move      dtaeur        dtaiso
023900070723      *
024000070723     c                   move      dtaiso        dateu8            8 0
024100070723     c     or66fgsi      chain     AZORG01l
024200070723    1c                   IF        %found(azorg01l)
024300070723     c                   MOVEL     ORGde7        og147
024400070723      * se data distinte automatiche < oggi p.o. partito
024500070723     c                   if        §ogddao = '      '
024600070723     c                   exsr      srfine
024700070723     c                   else
024800070723     c                   move      §ogddao       comdda            8 0
024900070723     c                   end
025000070723     c                   end
025100070723      *  Definisco chiavi di accesso
025200070723     c     Korg          klist
025300070723     c                   kfld                    or66fgsi
025400070723     c                   kfld                    or66nfvi
025500070723     c                   kfld                    or66dfvi
025600070723
025700070925     c     Korm          klist
025800070925     c                   kfld                    orgpoe
025900070925     c                   kfld                    orgnsr
026000070925     c                   kfld                    orgnor
026100070925     c                   kfld                    orgnrv
026200070723     c                   ENDSR
026300070723      *----------------------------------------------------*
026400070723     c     geslckorg     begsr
026500070723      *----------------------------------------------------*
026600070723     c                   clear                   trul82ds
026700070723     c                   Eval      ul82§rrn = orgnrr
026800070723     c                   Eval      UL82§FIL = 'FNORG00F'
026900070723     c                   Eval      UL82§WIN = 'S'
027000070723     c                   Eval      UL82§num = 2
027100070723     c                   Eval      UL82§att = 2
027200070723     c                   eval      ul82§mss = ErrMsg(1)
027300070723     c                   Eval      UL82§msw = 'L''ORM '
027400070723     c                             + %editc(orgpoe:'Z') +  ' '
027500070723     c                             + %editc(orgnsr:'Z') +  ' '
027600070723     c                             + %editc(orgnor:'Z') +  ' '
027700070723     c                             + %editc(orgnrv:'Z') +  ' è già utilizzata -
027800070723     c                             da un altro lavoro e non è manutenzionabile'
027900070723      * Effettuo la chiamata al *PGM d utilità
028000070723     c                   Call(e)   'TRUL82R'
028100070723     c                   Parm                    Trul82ds
028200070723
028300070723     c                   ENDSR
028400070723** ErrMsg
028500070723Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
