000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200070205     h dftactgrp(*no) actgrp(*caller)
000300070322     H* FIDG20R *----------------------------------------------------*
000400070517     H*         Elimina riferimenti distinta da FIARG/FNARB
000500070322     H*--------------------------------------------------------------*
000501100514     Ftabel00f  if   E           k DISK
000600070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000700070205     FAZORG01L  IF   E           K DISK
000900070528     FFiarg03L  iF   E           K DISK    infds(arginfds)
000901070528     f                                     rename(fiarg000:fiarg00)
000902070528     FFiarg00f  uF   E             DISK    commit
001101070528     ffnarb70L  iF   E           K DISK    infds(arbinfds)
001102070528     f                                     rename(fnarb000:fnarb00)
001103070528     ffnarb00f  uF   E             DISK    commit
001200950524      *--------------------------------------------------------------*
001300950524      *   DS
001400950524      *--------------------------------------------------------------*
001401070528     d arbinfds        ds
001402070528     d  arbnrr               397    400i 0
001403070528     d arginfds        ds
001404070528     d  argnrr               397    400i 0
001500050520     D ddatiute      e ds
001600030910     D azuteds       e ds                  extname(AZUTE00F)
001700030910     D tibs34ds      E DS                  inz
001701100202     D dargflo       E DS                  inz
001702100514     D ds7r          E DS                  inz
001800950524     D KPJBA         E DS
001900070205     D Fidg20ds      E DS
002000070517     D Fidg37ds      E DS
002100070227     D Fnlr99ds      E DS
002101070528     D trul82ds      E DS
002200070205     D OG146         E DS                  INZ
002201071107     D OG147         E DS                  INZ
002202071107     D OG150         E DS                  INZ
002300070205     ddtaiso           s               d
002400070205     ddtaeur           s               d   datfmt(*eur)
002500070412     dARBIFPC          s                   LIKE(ARBIFP)
002501070528     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
002600950524     C*--------------------------------------------------------------*
002700950524     C*  Ciclo principale
002800950524     C*--------------------------------------------------------------*
002900070205     c*
003000070206      *  aggiorno FIARG00F
003100070205     c*
003200070208     c     karg          setll     fiarg03l
003300070208     c                   do        *hival
003400070528     c     karg          reade     fiarg03l
003401070528     C                   if        %eof(fiarg03l)
003402070528     c                   leave
003403070528     c                   end
003404070528     c     argnrr        chain(e)  fiarg00f
003500070208     c                   if        %error
003501070528     c                   exsr      geslckarg
003502070528     c                   if        ul82§sts = 'A'
003503070528     c                   seton                                        83
003504070528     c                   exsr      srstampa
003505070528     c                   exsr      srfine
003506070528     c                   else
003507070528     c     argnrr        chain     fiarg00f
003508070528     c                   end
003509070528     c                   end
004000061003
004400070326     c* lascio le spedizioni inserite manualmente se non espressamente
004500070326     c* richiesta la pulizia (solo per EDP)
004600070326     C                   if        d20clrman <>'S' and
004700070326     C                             argnftl = 99999
004800070223     c                   iter
004900070223     c                   end
005000070208
005001100202     c                   eval      dargflo = argflo
005002100202     c                   clear                   §argattesa
005003100202     c                   eval      argflo = dargflo
005100070208     c                   eval      argndc = 0
005200070208     c                   eval      argpdc = 0
005300070208     c                   eval      argfgs = 0
005400070208     c                   eval      argddc = 0
005500070208     c                   eval      argstp = 0
005600070208     c                   eval      argnftl= 0
005700070208     c                   eval      argslb = 0
005800070208     c                   eval      argdtvdis= 0
005900070208     c                   eval      arghvdis= 0
006000070209     c                   update    fiarg000
006100070208     c                   enddo
006200070313     c
006300070313     c* aggiorno ARB solo se p.o. partito
006400070313     c                   if        dateu8 >= comdda
006500070313     c     karb          setll     fnarb70l
006600070313     c                   do        *hival
006700070528     c     karb          reade     fnarb70l
006701070528     C                   if        %eof(fnarb70l)
006702070528     c                   leave
006703070528     c                   end
006704070528     c     arbnrr        chain(e)  fnarb00f
006800070313     c                   if        %error
006803070528     c                   exsr      geslckarb
006804070528     c                   if        ul82§sts = 'A'
006900070410     c                   seton                                        83
007000070313     c                   exsr      srstampa
007100070313     c                   exsr      srfine
007101070528     c                   else
007102070528     c     arbnrr        chain     fnarb00f
007201070528     c                   end
007202070528     c                   end
007600070214     c* spedizione manuale non cancello se non richiesto espressamente
007700070313     C                   if        d20clrman <>'S' and arbngd <> 0
007800070313     c                   iter
007900070313     c                   end
008000070313     c                   eval      arbndc = 0
008100070313     c                   eval      arbpdc = 0
008200070313     c                   eval      arbifp = 0
008300070313     c                   eval      arbddc = 0
008400070313     c                   eval      arbstp = 0
008500070313     c                   eval      arbngd = 0
008600070313     c                   eval      arbnss = 0
008700070313     c                   update    fnarb000
008800070227     c* ritiro contestuale
008900070313     c                   exsr      srorm
009000070313     c                   enddo
009100070313     c                   end
009200070223     c* FIDST00F
009300070517     c* chiamo il pgm che aggiorna valori assegnati nel caso ci siano sped.
009400070223     c* manuali
009500070517     c                   clear                   fidg37ds
009600070517     c                   eval      D37FGSI = D20FGSI
009700070517     c                   eval      D37DFVI = D20dFVI
009800070517     c                   eval      D37NFVI = D20nFVI
009801070824     c                   eval      D37dtaI = D20dtaI
009802070824     c                   eval      D37oraI = D20oraI
009804070517     c                   eval      D37cmtI = 'N'
009805070517     c                   eval      kpjbu = fidg37ds
009900070517     c                   call      'FIDG37R'
010000070517     c                   parm                    kpjba
010001070517     c                   eval      fidg37ds = kpjbu
010300070517     c                   if        d37erro <> ' '
010400070223     c                   seton                                        81
010500070223     c                   exsr      srstampa
010600070223     c                   exsr      srfine
010700070223     c                   end
012100950525     C*
012200070206     c                   if        d20cmti= 'S'
012300070205     c                   commit
012400070205     c                   end
012401070517     c*
012500070323     c                   eval      d20pag = pages
012600070323     c                   eval      d20riga= riga
012700070205     c*
012800070205     c                   eval      kpjbu = fidg20ds
012900950524     C                   SETON                                        LR
013000950524     C*
013100070227     C*-----------------------------------------------------*
013200070227     C*  toglie sull'orm x particolarità RC
013300070227     C*-----------------------------------------------------*
013400070313     C     srorm         BEGSR
013500100514     c                   if        arbgma <> ' '
013501100514     c                   clear                   ds7r
013503100514     C                   eval      tblcod = '7R'
013504100514     C                   eval      tblkey = arbgma
013505100514     C     ktab          chain     Tabel00f
013506100514     c                   if        %found(tabel00f)
013507100514     C                   MOVEL     TBLUNI        DS7R
013510100514     c                   end
013511100514     c                   if        §7RRC <>'S'
013512100514     c                   leavesr
013513100514     c                   end
013600070313     c                   clear                   fnlr99ds
013700070313     c                   eval      i99tla = 'L'
013800070313     c                   eval      i99aas = arbaas
013900070313     c                   eval      i99lnp = arblnp
014000070313     c                   eval      i99nrs = arbnrs
014100070313     c                   eval      i99nsp = arbnsp
014200071001     c                   eval      i99fgs = d20fgsi
014201071001     c                   eval      i99ndc = arbndc
014300070313     c                   eval      i99ddc = arbddc
014400071001     c                   eval      i99comit ='1'
014401071001     c                   eval      i99cmt ='N'
014402071001     c                   eval      i99fao = 390
014500070313     c                   call      'FNLR99R'
014600070313     c                   parm                    kpjba
014700070313     c                   parm                    fnlr99ds
014800070313     c                   end
014900070313     C                   ENDSR
015000950525     C*--------------------------------------------------------------*
015100070205     C*  Operazioni finali in caso di errore
015200950525     C*--------------------------------------------------------------*
015300070205     C     srfine        BEGSR
015400070206     c                   if        d20cmtI= 'S'
015500070205     c                   rolbk
015600070205     c                   end
015700070206     c                   eval      d20erro= '1'
015800070205     c                   eval      kpjbu = fidg20ds
015900070205     c                   seton                                        lr
016000070205     c                   return
016100070205     c                   endsr
016200070222     C*------------------------------------------------------------------------*
016300070222     C* stampa
016400070222     C*------------------------------------------------------------------------*
016500070222     C     srstampa      BEGSR
016501071107     c* stampo solo per assegnaz. automatica
016502071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
016600070222     c                   move      d20dfvi       dtaiso
016700070222     c                   move      dtaiso        dtaeur
016800070222     c                   move      dtaeur        prtdfv
016900070517     c                   eval      prtnfv=d20nfvi
016901071107     c* decodifico parametri di lancio
016902071210     c                   if        d20ldcri <> 0
016903071210     c                   move      d20ldcri      dtaiso
016904071210     c                   move      dtaiso        dtaeur
016905071210     c                   move      dtaeur        decldcr
016906071210     c                   else
016907071210     c                   clear                   decldcr
016908071210     c                   end
016909071107     c                   if        *in71
016910071107     c                   move      d20dtai       dtaiso
016911071107     c                   move      dtaiso        dtaeur
016912071107     c                   move      dtaeur        decdta
016913071107     c                   eval      decora = d20orai
016914071107     c                   end
016915071107     c                   if        d20cleari = 'S'
016916071107     c                   eval      decclear='SI'
016917071107     c                   else
016918071107     c                   eval      decclear='NO'
016919071107     c                   end
016927150515     c                   if        d20p88i= 'N'
016928150515     c                   eval      decp88r='Spediz. pistola 88 NO'
016929150515     c                   else
016930150515     c                   clear                   decp88r
016931150515     c                   end
016932150515     c                   if        *in70
016933071107     c                   select
016934071107     c                   when      d20tpmi ='M'
016935071107     c                   eval      dectpm ='MESSAGGERIE'
016936071107     c                   when      d20tpmi ='P'
016937071107     c                   eval      dectpm ='PARCEL'
016938071107     c                   when      d20tpmi =' '
016939071107     c                   eval      dectpm ='TUTTE'
016940071107     c                   endsl
016941071107     c                   end
016942160420     c                   if        *in70
016943160420     c                   eval      dectur1 = d20tur1
016944160420     c                   eval      dectur2 = d20tur2
016945160420     c                   end
016946071107     c                   select
016947071107     c                   when      d20fppi ='M'
016948071107     c                   eval      decfpp ='MAT'
016949071107     c                   when      d20fppi ='P'
016950071107     c                   eval      decfpp ='POM'
016951071107     c                   when      d20fppi =' '
016952071107     c                   eval      decfpp ='TUTTE'
016953071107     c                   endsl
017000070323     C* PAGINA IN STAMPA
017100070323     C                   IF        *IN80 OR RIGA >=60
017200070323     C                   SETON                                        80
017300070323     C                   ADD       1             PAGES
017400071107     C                   Z-ADD     5             RIGA              3 0
017500070323     C                   END
017600070222     c*
017700070222     c   80              write     testa
017701071107     c   80              write     testap
017800070222     c                   setoff                                       80
017900070222     c   81              write     riga1
018000070323     C   81              ADD       1             RIGA
018100070222     c   82              write     riga2
018200070323     C   82              ADD       1             RIGA
018300070222     c   83              write     riga3
018400070323     C   83              ADD       1             RIGA
018500070222     c                   setoff                                       818283
018501071107     c                   end
018600070222     c*
018700070222     c                   endsr
018800070205     C*--------------------------------------------------------------*
018900070205     C*  Operazioni iniziali
019000070205     C*--------------------------------------------------------------*
019100070205     C     *INZSR        BEGSR
019200950525     C*
019300950525     C     *ENTRY        PLIST
019400950525     C                   PARM                    KPJBA
019500070205     C                   MOVEL(P)  KPJBU         Fidg20ds
019600070206     c                   clear                   d20erro
019700070326     c                   eval      pages = d20pag
019800070326     c                   eval      riga = d20riga
019900030718     c*
020000030718     c     *dtaara       define    §azute        azuteds
020100030718     c     *dtaara       define    §datiute      ddatiute
020200030718     C                   in(E)     *dtaara
020300030718     C                   IF        %Error  or  RSUT = *blanks
020400030718     C                   call      'TIBS34R'
020500030718     C                   parm                    Tibs34Ds
020600030718     C                   in        *dtaara
020700030718     c                   ENDIF
020800070205     c*
020900070510     C                   TIME                    WHHUDT           14 0
021000070510     C                   MOVEL     WHHUDT        ORA               6 0
021100070510     C                   MOVE      WHHUDT        UDATE8            8 0
021200070510     c                   move      udate8        dtaeur
021300070510     c                   move      dtaeur        dtaiso
021400070510      *
021500070510     c********           move      *date         dtaiso
021600070206     c                   move      dtaiso        dateu8            8 0
021700070206     C     d20FGSi       CHAIN     AZORG01l
021800070205    1C                   IF        %found(azorg01l)
021901071107     c                   movel     orgdf0        og150
021902071107     C                   MOVEL     ORGde6        og146
021903071107     C                   MOVEL     ORGDE7        og147
021904071107     c     §OGPKM        comp      'S'                                    70
021905071107     c     §OGdoada      comp      'S'                                    71
022000070205     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
022100070205     c* se data distinte automatiche < oggi p.o. partito
022200070205     c                   if        §ogdda = ' '
022300070205     c                   exsr      srfine
022400070205     c                   else
022500070205     c                   move      §ogdda        comdda            8 0
022600070205     c                   end
022700070205     c                   end
022800950525     C*  Definisco chiavi di accesso
022802100514     C     Ktab          KLIST
022803100514     C                   KFLD                    TBLKUT
022804100514     C                   KFLD                    TBLCOD
022805100514     C                   KFLD                    TBLKEY
022806100514     c                   eval      tblkut = 1
023300070208     C     Karg          KLIST
023400070208     C                   KFLD                    d20fgsi
023500070208     C                   KFLD                    d20nfvi
023600070208     C                   KFLD                    d20dfvi
023700070313     C     Karb          KLIST
023800070412     C                   KFLD                    arbifpC
023900070313     C                   KFLD                    d20nfvi
024000070313     C                   KFLD                    d20dfvi
024300070412     c                   eval      arbifpC= d20fgsi
024400070223     C                   TIME                    TIMES             6 0
024500070223     C                   TIME                    WHDAT            12 0
024600070223     C                   MOVE      WHDAT         WOGGI             6 0
024700950525     C                   ENDSR
024800070528     C*----------------------------------------------------*
024900070528     C     geslckarg     begsr
025000070528     C*----------------------------------------------------*
025100070528     c                   clear                   trul82ds
025200070528     c                   Eval      ul82§rrn = argnrr
025300070528     c                   Eval      UL82§FIL = 'FIARG00F'
025400070528     c                   Eval      UL82§WIN = 'S'
025600070528     c                   Eval      UL82§num = 2
025700070528     c                   Eval      UL82§att = 2
025800070528     c                   eval      ul82§mss = ErrMsg(1)
025900070528     c                   Eval      UL82§msw = 'La Spedizione '
026000070528     c                             + %editc(arglnp:'Z') +  ' '
026100070528     c                             + %editc(argnrs:'Z') +  ' '
026200070528     c                             + %editc(argnsp:'Z') +  ' è già utilizza ta -
026300070528     c                             da un altro lavoro e non è manutenzionabile'
026400070528      * Effettuo la chiamata al *PGM d utilità
026500070528     c                   Call(e)   'TRUL82R'
026600070528     c                   Parm                    Trul82ds
026700070528
026800070528     C                   ENDSR
026801070528     C*----------------------------------------------------*
026802070528     C     geslckarb     begsr
026803070528     C*----------------------------------------------------*
026804070528     c                   clear                   trul82ds
026805070528     c                   Eval      ul82§rrn = arbnrr
026806070528     c                   Eval      UL82§FIL = 'FNARB00F'
026807070528     c                   Eval      UL82§WIN = 'S'
026809070528     c                   Eval      UL82§num = 2
026810070528     c                   Eval      UL82§att = 2
026811070528     c                   eval      ul82§mss = ErrMsg(1)
026812070528     c                   Eval      UL82§msw = 'La Spedizione '
026813070528     c                             + %editc(arblnp:'Z') +  ' '
026814070528     c                             + %editc(arbnrs:'Z') +  ' '
026815070528     c                             + %editc(arbnsp:'Z') +  ' è già utilizza ta -
026816070528     c                             da un altro lavoro e non è manutenzionabile'
026817070528      * Effettuo la chiamata al *PGM d utilità
026818070528     c                   Call(e)   'TRUL82R'
026819070528     c                   Parm                    Trul82ds
026820070528
026821070528     C                   ENDSR
026900070528** ErrMsg
027000070528Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
