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
002700070723     ddtaiso           s               d
002800070723     ddtaeur           s               d   datfmt(*eur)
002900070723     d Errmsg          s             78    dim(1) Ctdata perrcd(1)              Msg di errore
003000070723      *--------------------------------------------------------------*
003100070723      *  Ciclo principale
003200070723      *--------------------------------------------------------------*
003300070723
003400070723      *  aggiorno FNORG00F
003500070723
003600070723     c     korg          setll     fnorg03l
003700070723     c                   do        *hival
003800070723     c     korg          reade     fnorg03l
003900070723     c                   if        %eof(fnorg03l)
004000070723     c                   leave
004100070723     c                   end
004200070723     c     orgnrr        chain(e)  fnorg00f
004300070723     c                   if        %error
004400070723     c                   exsr      geslckorg
004500070723     c                   if        ul82§sts = 'A'
004600070723     c                   seton                                        83
004700070723     c                   exsr      srstampa
004800070723     c                   exsr      srfine
004900070723     c                   else
005000070723     c     orgnrr        chain     fnorg00f
005100070723     c                   end
005200070723     c                   end
005300070723
005400070723      * lascio le spedizioni inserite manualmente se non espressamente
005500070723      * richiesta la pulizia (solo per EDP)
005600070723
005700070723     c                   if        or66clrman <>'S' and
005800070723     c                             orgnftl = 99999
005900070223     c                   iter
006000070223     c                   end
006100070208
006200070925      * verifico se l'ORM risulta comunicato  passo al successivo
006300070926      * solo se sono già partita (non in simulazione)
006400071009     c     korm          chain     fnorm01l
006500071010     c                   if        not %found(fnorm01l)
006600071010     c                   iter
006700071010     c                   endif
006800071010
006900071009     c                   if        dateu8 >= comdda  and
007000071010     c                             ormdst > 0
007100070925     c                   iter
007200070925     c                   endif
007300071009      * verifico se richesta selezione sugli orm con allegati
007400071011     c*                  if        or66allega <> ' '
007500071011     c*                  movel     ormflo        dorm01
007600071011     c*                  if        or66allega = 'E' and (§orsrm = 'S' or
007700071011     c*                            ormtor = 'P')
007800071011     c*                  iter
007900071011     c*                  endif
008000071011     c*                  if        or66allega = 'I' and (§orsrm = ' ' and
008100071011     c*                            ormtor <> 'P')
008200071011     c*                  iter
008300071011     c*                  endif
008400071011     c*                  endif
008500070925
008600070723     c                   eval      orgndc = 0
008700070723     c                   eval      orgpdc = 0
008800070723     c                   eval      orgfgs = 0
008900070723     c                   eval      orgddc = 0
009000070723     c                   eval      orgnftl= 0
009100070723     c                   eval      orgslo = 0
009200070723     c                   eval      orgdtvdis= 0
009300070723     c                   eval      orghvdis= 0
009400070723     c                   update    fnorg000
009500070208     c                   enddo
009600070723     c
009700070723      * Richiamo il programma per togliere i riferimenti da FNORM e da FNORF
009800070723      * solo se sono già partita
009900070313     c                   if        dateu8 >= comdda
010000070723      * faccio la pulizia delle assegnazioni automatiche
010100070723     c                   clear                   fior61ds
010200070723     c                   eval      or61cmti = 'S'
010300070723     c                   eval      or61fgsi = or66fgsi
010400070723     c                   eval      or61dfvi = or66dfvi
010500070723     c                   eval      or61nfvi = or66nfvi
010600070723     c                   eval      or61nftl = *zeros
010700070723     c                   eval      or61mtdi = 'T'
010800070723     c                   eval      or61pag = pages
010900070723     c                   eval      or61riga= riga
011000070723
011100070731     c                   call      'FIOR61R'
011200070723     c                   parm                    kpjba
011300070723     c                   parm                    fior61ds
011400070723     c                   eval      pages = or61pag
011500070723     c                   eval      riga = or61riga
011600070723     c                   if        or61erro<> ' '
011700070723     c                   exsr      srfine
011800070723     c                   end
011900071010      * faccio la pulizia delle assegnazioni manuali se richiesto
012000070723     c                   If        or66clrman = 'S'
012100070723     c                   clear                   fior61ds
012200070723     c                   eval      or61cmti = 'S'
012300070723     c                   eval      or61fgsi = or66fgsi
012400070723     c                   eval      or61dfvi = or66dfvi
012500070723     c                   eval      or61nfvi = or66nfvi
012600070723     c                   eval      or61nftl = *all'9'
012700070723     c                   eval      or61mtdi = 'T'
012800070723     c                   eval      or61pag = pages
012900070723     c                   eval      or61riga= riga
013000070723
013100070731     c                   call      'FIOR61R'
013200070723     c                   parm                    kpjba
013300070723     c                   parm                    fior61ds
013400070723     c                   if        or61erro<> ' '
013500070723     c                   exsr      srfine
013600070723     c                   end
013700070723
013800070723     c                   endif
013900070723
014000070723     c                   endif
014100070223     c* FIDST00F
014200070723     c* chiamo il pgm che aggiorna valori assegnati nel caso ci siano ORM
014300070723     c* manuali
014400070723     c                   clear                   fior67ds
014500070723     c                   eval      or67fgsi = or66fgsi
014600070724     c                   eval      or67dfvi = or66dfvi
014700070724     c                   eval      or67nfvi = or66nfvi
014800070723     c                   eval      or67cmtI = 'N'
014900070723     c                   eval      kpjbu = fior67ds
015000070723     c                   call      'FIOR67R'
015100070723     c                   parm                    kpjba
015200070723     c                   eval      fior67ds = kpjbu
015300070723     c                   if        or67erro <> ' '
015400070723     c                   seton                                        81
015500070723     c                   exsr      srstampa
015600070723     c                   exsr      srfine
015700070723     c                   end
015800070723
015900070723     c                   if        or66cmti= 'S'
016000070723     c                   commit
016100070723     c                   end
016200070723
016300070723     c                   eval      or66pag = pages
016400070723     c                   eval      or66riga= riga
016500070723
016600070723     c                   eval      kpjbu = fior66ds
016700070723     c                   seton                                        lr
016800070723      *--------------------------------------------------------------*
016900070723      *  Operazioni finali in caso di errore
017000070723      *--------------------------------------------------------------*
017100070723     c     srfine        BEGSR
017200070723     c                   if        or66cmti= 'S'
017300070723     c                   rolbk
017400070723     c                   end
017500070723     c                   eval      or66erro= '1'
017600070723     c                   eval      kpjbu = fior66ds
017700070723     c                   seton                                        lr
017800070723     c                   return
017900070723     c                   endsr
018000070723      *------------------------------------------------------------------------*
018100070723      * stampa
018200070723      *------------------------------------------------------------------------*
018300070723     c     srstampa      BEGSR
018400070723     c                   move      or66dfvi      dtaiso
018500070723     c                   move      dtaiso        dtaeur
018600070723     c                   move      dtaeur        prtdfv
018700070723      * PAGINA IN STAMPA
018800070723     c                   IF        *IN80 OR RIGA >=60
018900070723     c                   SETON                                        80
019000070723     c                   ADD       1             PAGES
019100070723     c                   Z-ADD     3             RIGA              3 0
019200070723     c                   END
019300070723      *
019400070723     c   80              write     testa
019500070723     c                   setoff                                       80
019600070723     c   81              write     riga1
019700070723     c   81              ADD       1             RIGA
019800070723     c   83              write     riga3
019900070723     c   83              ADD       1             RIGA
020000070723     c                   setoff                                       8183
020100070723      *
020200070723     c                   endsr
020300070723      *--------------------------------------------------------------*
020400070723      *  Operazioni iniziali
020500070723      *--------------------------------------------------------------*
020600070723     c     *INZSR        Begsr
020700070723
020800070723     c     *ENTRY        Plist
020900070723     c                   parm                    kpjba
021000070723     c                   movel(p)  Kpjbu         Fior66ds
021100070723     c                   clear                   or66erro
021200070723     c                   eval      pages = or66pag
021300070723     c                   eval      riga = or66riga
021400070723
021500070723     c     *dtaara       define    §azute        azuteds
021600070723     c     *dtaara       define    §datiute      ddatiute
021700070723     c                   in(E)     *dtaara
021800070723     c                   IF        %Error  or  RSUT = *blanks
021900070723     c                   call      'TIBS34R'
022000070723     c                   parm                    Tibs34Ds
022100070723     c                   in        *dtaara
022200070723     c                   ENDIF
022300070723
022400070723     C                   TIME                    WHHUDT           14 0
022500070723     C                   MOVEL     WHHUDT        ORA               6 0
022600070723     C                   MOVE      WHHUDT        UDATE8            8 0
022700070723     c                   move      udate8        dtaeur
022800070723     c                   move      dtaeur        dtaiso
022900070723      *
023000070723     c                   move      dtaiso        dateu8            8 0
023100070723     c     or66fgsi      chain     AZORG01l
023200070723    1c                   IF        %found(azorg01l)
023300070723     c                   MOVEL     ORGde7        og147
023400070723      * se data distinte automatiche < oggi p.o. partito
023500070723     c                   if        §ogddao = '      '
023600070723     c                   exsr      srfine
023700070723     c                   else
023800070723     c                   move      §ogddao       comdda            8 0
023900070723     c                   end
024000070723     c                   end
024100070723      *  Definisco chiavi di accesso
024200070723     c     Korg          klist
024300070723     c                   kfld                    or66fgsi
024400070723     c                   kfld                    or66nfvi
024500070723     c                   kfld                    or66dfvi
024600070723
024700070925     c     Korm          klist
024800070925     c                   kfld                    orgpoe
024900070925     c                   kfld                    orgnsr
025000070925     c                   kfld                    orgnor
025100070925     c                   kfld                    orgnrv
025200070723     c                   ENDSR
025300070723      *----------------------------------------------------*
025400070723     c     geslckorg     begsr
025500070723      *----------------------------------------------------*
025600070723     c                   clear                   trul82ds
025700070723     c                   Eval      ul82§rrn = orgnrr
025800070723     c                   Eval      UL82§FIL = 'FNORG00F'
025900070723     c                   Eval      UL82§WIN = 'S'
026000070723     c                   Eval      UL82§num = 2
026100070723     c                   Eval      UL82§att = 2
026200070723     c                   eval      ul82§mss = ErrMsg(1)
026300070723     c                   Eval      UL82§msw = 'L''ORM '
026400070723     c                             + %editc(orgpoe:'Z') +  ' '
026500070723     c                             + %editc(orgnsr:'Z') +  ' '
026600070723     c                             + %editc(orgnor:'Z') +  ' '
026700070723     c                             + %editc(orgnrv:'Z') +  ' è già utilizzata -
026800070723     c                             da un altro lavoro e non è manutenzionabile'
026900070723      * Effettuo la chiamata al *PGM d utilità
027000070723     c                   Call(e)   'TRUL82R'
027100070723     c                   Parm                    Trul82ds
027200070723
027300070723     c                   ENDSR
027400070723** ErrMsg
027500070723Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
