000100041021      *PARMS CLOSQLCSR(*ENDMOD)
000200041021      *PARMS DYNUSRPRF(*OWNER)
000300041018     H*---------------------------------------------------------------*
000400041018     H*      Quadratura AUT: Scrittura bollettato FIQBT/FIQBD         *
000500041018     H*---------------------------------------------------------------*
000600041018     H* INDICATORI USATI                                              *
000700041018     H*---------------------------------------------------------------*
000800041022     H* 30 - lookup
000900041022     H* 99 - comodo
001000041018     F*---------------------------------------------------------------*
001100041018     F* DATA BASE                                                     *
001200041018     F*---------------------------------------------------------------*
001300041018     H DECEDIT('0,') DATEDIT(*DMY.)
001400041018     F*------------
001500041018     F* P A R T E N Z A
001600041018     F*------------
001700041021     FFNBLP00f  IF   E             DISK
001800041021     FFNBLT01L  IF   E           K DISK
001900041021     Ffibrv07l  IF   E           K DISK
002000041018     FTABEL00F  IF   E           K DISK
002100050414     fFiar501l  if   e           k Disk
002200041022     fTntbe01l  uf   e           k Disk
002300041021     ffiqbt00f  O  A E             disk
002400041021     ffiqbd00f  O  A E             disk
002500041018
002600041021     D KCOD            S                   LIKE(TBLCOD) inz
002700041021     D KKEY            S                   LIKE(TBLKEY) inz
002800041022     d kTbeCod         s                   Like(TbeCod)
002900041022     d kTbeKe1         s                   Like(TbeKe1)
003000050414     d KAr5Trd         s                   Like(Ar5Trd)
003100041021     D $CBLPR          S                   LIKE(BLPCBO) inz
003200041021     D savpdr          S                   LIKE(BLPpdr) inz
003300041022     D savdrt          S                   LIKE(BLPDRT) inz
003400041025     D savlnp          S                   LIKE(BLPlnp) inz
003500041021     D totncl          S                   LIKE(qbtnclBO) inz
003600050414     D totnclspu       S                   LIKE(qbtnclsp) inz
003700050414     D wnclspu         S                   LIKE(qbtnclsp) inz
003800041021     D totspe          S                   LIKE(qbtnspBo) inz
003900041021     D totpkf          S                   LIKE(qbtpkfBo) inz
004000041021     D totvlf          S                   LIKE(qbtvlfBo) inz
004100041022     D wncl            S                   LIKE(bltnsc) inz
004200050414     D wsquad          S                   LIKE(qbtfquad) inz
004300041020     d dataiso         s               d   datfmt(*iso)
004400041020     d dataiso2        s               d   datfmt(*iso)
004500041021     d StringaSql      s            500    Varying
004600041022     d datael          s              8  0
004700041022     d pardrd          s              8  0 inz
004800041022     d pardra          s              8  0 inz
004900041022     d parfgs          s              3  0 inz
005000041021     d wsputrov        s              1
005100041021     D NPS             S              2  0 DIM(101)
005200041020
005300041020     D WLBDAT          DS
005400041020     D  G02DAT                 1      8  0
005500041020     D  G02INV                 9     16  0
005600041020     D  G02ERR                17     17
005700041020     D  G02TGI                18     22  0
005800041020
005900041020     d Wdata8          DS
006000041020     d  dadata                 1      8  0
006100041020     d  adata                  9     16  0
006200041020     d  GioLav                17     21  0
006300041018
006400041018     D*------------------
006500041018     D* ARCHITETTURA
006600041018     D*------------------
006700041018     D KPJBA         E DS
006800041020     D
006900041020     D TIBS02DS      E DS
007000041018     D*------------------
007100041018     D* PARAMETRI LANCIO
007200041018     D*------------------
007300041018     D PARAM           DS
007400041022     D  ppardrd                1      8
007500041022     D  ppardra                9     16
007600041022     D  pparfgs               17     19
007700041021     d*
007800041021     D DCRA          E DS
007900041021     D DS7J          E DS
008000041022     D DS3A          E DS
008100050414     d Dar5Bnb       e Ds
008200041021     d FnblpxxDs     e ds                  ExtName(Fnblp00f)
008300041018     C*---------------------------------------------------------------*
008400041018     C* MAIN LINE                                                     *
008500041018     C*---------------------------------------------------------------*
008600041022     c* Lr è on quando giorno festivo
008700041022     c                   if        *inlr = '0'
008800041021     c* Eseguo sql per estrarre da fnblp solo i dati che mi servono
008900041021     c                   exsr      sr_SqlBlp
009000041021     c* Lettura ed elaborazione di fnblp
009100041021     c                   exsr      sr_LeggiBlp
009200041022     c* aggiorno data ultima elaborazione se pgm richiamato da schedulazione
009300041022     c     aggdta        ifeq      'S'
009400041022     c                   Eval      kTbeCod = 'CRA'
009500041022     c                   Movel(p)  'DFT'         kTbeKe1
009600041022     c     Ktbe          Chain     Tntbe01l
009700041022     c                   If        %Found(Tntbe01l)
009800041022     c                   Movel     TbeUni        dcra
009900041022     c                   Z-Add     pardra        d§cradt
010000041022     c                   Movel     dcra          TbeUni
010100041022     c                   Update    Tntbe000
010200041022     c                   EndIf
010300041022     c                   endif
010400041022     c                   endif
010500041022     c*
010600041022     c                   seton                                        lr
010700041018     c
010800041021     c**********************************************************************
010900041021     c     Sr_Sqlblp     begsr
011000041021     c* preparo la stringa sql
011100041022     c     parfgs        ifeq      *zeros
011200041021     c                   eval      StringaSql =
011300041021     c                             'Select * '                   +
011400041022     c                             'from fnblp00f WHERE blpdrt between ' +
011500041022     c                              %editc(pardrd:'4') +
011600041022     c                             ' and ' +
011700041022     c                              %editc(pardra:'4') +
011800041022     c                             ' ORDER BY blpdrt, blppdr for fetch ' +
011900041022     c                             'only'
012000041022     c                   else
012100041022     c                   eval      StringaSql =
012200041022     c                             'Select * '                   +
012300041022     c                             'from fnblp00f WHERE blpdrt between ' +
012400041022     c                              %editc(pardrd:'4') +
012500041022     c                             ' and ' +
012600041022     c                              %editc(pardra:'4') +
012700041025     c                             ' and blplnp = ' +
012800041025     c                              %editc(parfgs:'4') +
012900041022     c                             ' ORDER BY blpdrt, blppdr for fetch ' +
013000041022     c                             'only'
013100041022     c                   endif
013200041021     C/EXEC SQL
013300041021     C+ prepare S1 from :StringaSql
013400041021     C/END-EXEC
013500041021     C/EXEC SQL
013600041021     C+ declare C1 cursor for S1
013700041021     C/END-EXEC
013800041021     c                   endsr
013900041021     c**********************************************************************
014000041021     c     Sr_LeggiBlp   begsr
014100041021     c*
014200041021     c* apertura cursore
014300041021     c*
014400041021     C/EXEC SQL
014500041021     C+ Open C1
014600041021     C/END-EXEC
014700041021     c*
014800041021     c* lettura cursore
014900041021     c*
015000041021     C/EXEC SQL
015100041022     C+ Fetch C1 into :fnblpxxds
015200041021     C/END-EXEC
015300041021     c                   Dow       Sqlcod = 0                                   *record letto
015400041021     c* ignoro spedizione di recupero
015500041021    6C     BLPCBO        IFNE      $CBLPR
015600041021     C                   MOVE      '3A'          KCOD
015700041021     C                   MOVE      *BLANKS       KKEY
015800041021     C                   MOVEL     BLPCBO        KKEY
015900041021     C     KTAB1         CHAIN     TABEL00F                           99
016000041021     C  N99              MOVEL     TBLUNI        DS3A
016100041021     C  N99              MOVE      BLPCBO        $CBLPR
016200041021     C   99              CLEAR                   DS3A
016300041021     c                   endif
016400050208     c                   if        §3arbl <> 'R' and blppdr > 0
016500041022     c* a cambio di data/autotrasportatore memorizzo dati su file fiqbt
016600041021    2c     blppdr        ifne      savpdr
016700041022    2c     blpdrt        orne      savdrt
016800041021     c                   exsr      Sr_WrtFiqbt
016900041021     c                   eval      savpdr = blppdr
017000041022     c                   eval      savdrt = blpdrt
017100041025     c                   eval      savlnp = blplnp
017200041021    2c                   endif
017300050414     c* conteggio colli spuntati con pst. reale
017400050414     c                   exsr      Sr_Nospu
017500041021     c* conteggio colli /spedizioni / peso colli
017600041021     c                   add       1             totspe
017700041021     c                   add       blppkf        totpkf
017800041021     c                   add       blpvlf        totvlf
017900050414    2c                   if        %subst(blpgva:1:1)<>'O'
018000050414     c                   add       blpncl        totncl
018100050414     c                   add       wnclspu       totnclspu
018200050414   x2c                   else
018300050414     c* considerazioni particolari per sped. con particolarità varia "O"
018400050414     c                   Clear                   DAr5Bnb
018500050414     c                   Eval      Kar5Trd = 'BNB'
018600050414     c     KFiar5        Chain     Fiar501l
018700050414    3c                   If        %Found(Fiar501l)
018800050414     c                   Movel     Ar5Uni        DAr5Bnb
018900050414    3c                   EndIf
019000050414     c                   add       §AR5BCOR      totncl
019100050414    3c                   if        blpncl = wnclspu
019200050414     c                   add       §AR5BCOR      totnclspu
019300050414     c                   else
019400050420     c                   eval      wsquad = '§'
019500050414    3c                   endif
019600050414    2c                   endif
019700041021     c                   endif
019800041021     C/EXEC SQL
019900041022     C+ Fetch C1 into :fnblpxxds
020000041021     C/END-EXEC
020100041021
020200041021     c                   EndDo
020300041025     c*
020400041025     c                   exsr      Sr_WrtFiqbt
020500041021
020600041021     C/EXEC SQL
020700041021     C+ close C1
020800041021     C/END-EXEC
020900041021
021000041021     c                   endsr
021100041021     c**********************************************************************
021200041021     c     Sr_WrtFiqbt   begsr
021300041021     c* Memorizzo totali su FIQBT
021400041021     c     totspe        ifgt      *zeros
021500041021     c                   clear                   fiqbt000
021600041025     c                   z-add     savlnp        qbtfgs
021700041022     c                   z-add     savdrt        qbtddc
021800041022     c                   z-add     savpdr        qbtpdr
021900041021     c                   z-add     totncl        qbtnclbo
022000050414     c                   z-add     totnclspu     qbtnclsp
022100041022     c                   z-add     totspe        qbtnspbo
022200041021     c                   z-add     totpkf        qbtpkfbo
022300041021     c                   z-add     totvlf        qbtvlfbo
022400041022     c                   move      datael        qbtdaebo
022500041021     c                   movel     w0140         qbtorabo
022600050414     c                   movel     wsquad        qbtfquad
022700041021     c                   write     fiqbt000
022800041021     c                   endif
022900041021     c                   clear                   totspe
023000041021     c                   clear                   totncl
023100041021     c                   clear                   totvlf
023200041021     c                   clear                   totpkf
023300050414     c                   clear                   totnclspu
023400050414     c                   clear                   wsquad
023500041021     c
023600041021     c                   endsr
023700041021     c**********************************************************************
023800041021     c     Sr_NoSpu      begsr
023900050414     c                   clear                   wnclspu
024000041021     c* segnacolli     sequenziali
024100041021    1c     blpfns        ifeq      *blanks
024200041021     c                   z-add     blpncd        wncl
024300041021    2c     blpnca        ifeq      0
024400041021     c                   z-add     blpncd        blpnca
024500041021    2c                   endif
024600041021    2c     wncl          dowle     blpnca
024700050526     c* Devo testare anche caso che wncl > 0 perchè se segnacollo  al =
024800050526     c* 9999999 (ed è accaduto) il pgm va in loop in quanto wncl è
024900050526     c* definito 7,0 come bltnsc
025000050526     c     wncl          andgt     *zeros
025100041021     c                   exsr      Sr_NoSpu2
025200041021     c                   add       1             wncl
025300041021    2c                   enddo
025400041021   x1c                   else
025500041021     c* segnacolli non sequenziali
025600041021     c     kblt          setll     fnblt01l
025700041021    2c                   do        *hival
025800041021     c     kblt          reade     fnblt01l
025900041021     c                   if        %eof(fnblt01l)
026000041021     c                   leave
026100041021     c                   endif
026200041021     c                   z-add     bltnsc        wncl
026300041021     c                   exsr      Sr_NoSpu2
026400041021    2c                   enddo
026500041021    1c                   endif
026600041022     c                   endsr
026700041021     c**********************************************************************
026800041021     c     Sr_NoSpu2     begsr
026900041021     c                   clear                   wsputrov
027000041021     c     kbrv          setll     fibrv07l
027100041021     c                   do        *hival
027200041021     c     kbrv          reade     fibrv07l
027300041021     c* esco dal ciclo per fine file o per spunta trovata
027400041021     c                   if        %eof(fibrv07l) or wsputrov = 'S'
027500041021     c                   leave
027600041021     c                   endif
027700041021     c* ignoro spunta se è di pistola automatica
027800050415     c* do per scontato che la pistola 00 non sia automatica
027900041021     c     brvnps        lookup    nps                                    30
028000041021     c     *in30         ifeq      *off
028100050415     c     brvnps        oreq      00
028200041021     c                   movel     'S'           wsputrov
028300041021     c                   endif
028400041021     c                   enddo
028500041021     c* se non trovata spunte reale memorizzo in fiqbd
028600050414     c*    wsputrov      ifeq      *blanks
028700050414     c*                  clear                   fiqbd000
028800050414     c*                  z-add     blplnp        QBDFGS
028900050414     c*                  z-add     blpdrt        QBDDDC
029000050414     c*                  z-add     blppdr        QBDPDR
029100050414     c*                  z-add     blpaas        QBDAAS
029200050414     c*                  z-add     blplnp        QBDLNP
029300050414     c*                  z-add     blpnrs        QBDNRS
029400050414     c*                  z-add     blpnsp        QBDNSP
029500050414     c*    blpccm        ifgt      *zeros
029600050414     c*                  z-add     blpccm        QBDCCM
029700050414     c*                  else
029800050414     c*                  z-add     blpksc        qbdccm
029900050414     c*                  endif
030000050414     c*                  z-add     blpncl        QBDNCL
030100050414     c*                  z-add     blpfls        QBDFLS
030200050414     c*                  z-add     wncl          QBDNSC
030300050414     c*                  z-add     blplna        qbdlna
030400050414     c*                  write     fiqbd000
030500050414     c*                  endif
030600050414     c* se trovata spunta reale incremento nel tot.colli spuntati
030700050414     c     wsputrov      ifeq      'S'
030800050414     c                   add       1             Wnclspu
030900050414     c                   endif
031000041021     c                   endsr
031100041018     c**********************************************************************
031200041020     c     repdat        begsr
031300041020      *controllo se udate è festivo non lancio l'elaborazione
031400041020     c                   move      dataiso       dadata
031500041020     c                   move      dataiso       adata
031600041020     c                   CALL      'XSRLAV8'
031700041020     c                   PARM                    Wdata8
031800041020     c                   if        giolav = 0
031900041020     c                   seton                                        lr
032000041022     c                   goto      frepdat
032100041020     c                   endif
032200041020      * calcolo partendo da udate cercando i giorni lavorativi
032300041020      * all'indietro a seconda dei giorni impostati in tabella
032400041020     c     dataiso       subdur    1:*d          dataiso2
032500041020     c                   do        *hival
032600041020     c                   move      dataiso2      dadata
032700041020     c                   move      dataiso       adata
032800041020     c                   CALL      'XSRLAV8'
032900041020     c                   PARM                    Wdata8
033000041020      * se corrisponde ho trovato la data al
033100041020     c                   if        giolav = d§crabo
033200041020     c                   leave
033300041020     c                   else
033400041020      * altrimenti continuo a sottrarre i giorni da udate
033500041020     c     dataiso2      subdur    1:*d          dataiso2
033600041020     c                   iter
033700041020     c                   endif
033800041020     c                   enddo
033900041020      * ho stabilito la data al
034000041020     c                   move      dataiso2      pardra
034100041021     c* aggiungo 1 a data ultima elaborazione per impostare la data dal
034200050207     c* 07/02/2005 - Non più: da ora data dal = data ultima elab. - 1 giorno
034300050207     c*              lavorativo
034400041021     C                   move      d§cradt       dataiso
034500050207     c**** dataiso       adddur    1:*d          dataiso
034600050207     c****               move      dataiso       pardrd
034700050207     c     dataiso       subdur    1:*d          dataiso2
034800050207     c                   do        *hival
034900050207     c                   move      dataiso2      dadata
035000050207     c                   move      dataiso       adata
035100050207     c                   CALL      'XSRLAV8'
035200050207     c                   PARM                    Wdata8
035300050207     c                   if        giolav = 2
035400050207     c                   leave
035500050207     c                   else
035600050207      * altrimenti continuo a sottrarre 1 giorno
035700050207     c     dataiso2      subdur    1:*d          dataiso2
035800050207     c                   iter
035900050207     c                   endif
036000050207     c                   enddo
036100050207     c                   move      dataiso2      pardrd
036200041022     c     frepdat       endsr
036300041018     c**********************************************************************
036400041018     c     *inzsr        begsr
036500041022     c*
036600041022     c     *entry        plist
036700041022     c                   parm                    kpjba
036800041022     c                   movel     kpjbu         param
036900041022     c*
037000041022     c     ppardrd       ifgt      *zeros
037100041022     c                   move      ppardrd       pardrd
037200041022     c                   move      ppardra       pardra
037300041022     c                   move      pparfgs       parfgs
037400041022     c                   endif
037500041022     c*
037600041022      *reperisco limiti da tabella per calcolare il periodo dal/al da elabor
037700041022      *are e la data ultima elaborazione
037800041022     c                   Clear                   Tibs02Ds
037900041022     c                   Clear                   dcra
038000041022     c                   eval      t02mod = 'C'
038100041022     c                   Eval      T02Sif = Knsif
038200041022     c                   Eval      T02Cod = 'CRA'
038300041022     c                   eval      t02ke1 = 'DFT'
038400041022     c                   Call      'TIBS02R'
038500041022     c                   Parm                    Kpjba
038600041022     c                   Parm                    Tibs02Ds
038700041022     c                   If        T02Err = *Blanks
038800041022     c                   Movel     T02Uni        dcra
038900041022     c                   EndIf
039000041022     c* Se non passati parametri elaboro tutte le bolle aventi
039100050207     c*(data ritiro dal = data ultima elaborazione + 1) --> non più
039200050207     c* data ritiro dal = data ultima elaborazione - 1 giorno lavorativo
039300041022     c* data ritiro  al = data del job - i giorni lavorativi come da tabella
039400041022     c* richiamo quindi la routine per impostare le date
039500041022     c*                   "CRA"
039600041022     c                   clear                   aggdta            1
039700041025     C                   TIME                    W0140            14 0
039800041025     C                   move      W0140         G02DAT
039900041025     C                   MOVEL     *BLANK        G02ERR
040000041025     C                   CALL      'XSRDA8'
040100041025     C                   PARM                    WLBDAT
040200041025     c                   move      g02inv        datael
040300041025     C                   move      G02INV        dataiso
040400041025     c
040500041022     c                   if        pardrd = 0
040600041022     c                   exsr      repdat
040700041022     C                   if        *inlr
040800041022     C                   goto      endinz
040900041022     C                   endif
041000041022     c                   eval      aggdta = 'S'
041100041022     c                   endif
041200041022     c     pardra        ifeq      *zeros
041300041022     c                   z-add     pardrd        pardra
041400041022     c                   endif
041500041022     c* pulisco eventuali record se data già elaborata
041600041022     c     parfgs        ifeq      *zeros
041700041022     C/EXEC SQL
041800041022     C+ DELETE FROM FIQBT00F WHERE qbtddc between :pardrd and :pardra
041900041022     C/END-EXEC
042000041022     C/EXEC SQL
042100041022     C+ DELETE FROM FIQBD00F WHERE qbdddc between :pardrd and :pardra
042200041022     C/END-EXEC
042300041022     c                   else
042400041022     C/EXEC SQL
042500041022     C+ DELETE FROM FIQBT00F WHERE qbtddc between :pardrd and :pardra
042600041022     c+ and qbtfgs = :parfgs
042700041022     C/END-EXEC
042800041022     C/EXEC SQL
042900041022     C+ DELETE FROM FIQBD00F WHERE qbdddc between :pardrd and :pardra
043000041022     c+ and qbdfgs = :parfgs
043100041022     C/END-EXEC
043200041022     c                   endif
043300041018     c
043400041021     C* Chiave di accesso per il file tabelle
043500041021     C     KTAB1         KLIST
043600041021     C                   KFLD                    CODUT
043700041021     C                   KFLD                    KCOD
043800041021     C                   KFLD                    KKEY
043900041021     C     KTAB2         KLIST
044000041021     C                   KFLD                    CODUT
044100041021     C                   KFLD                    KCOD
044200041021     c     kbrv          klist
044300041025     c                   kfld                    blpfls
044400041021     c                   kfld                    blplna
044500041021     c                   kfld                    blpnrs
044600041021     c                   kfld                    wncl
044700041021     c     kblt          klist
044800041021     c                   kfld                    blpaas
044900041021     c                   kfld                    blplnp
045000041021     c                   kfld                    blpnrs
045100041021     c                   kfld                    blpnsp
045200041022     c     Ktbe          Klist
045300041022     c                   Kfld                    kTbeCod
045400041022     c                   Kfld                    kTbeKe1
045500050414     c     KFiar5        Klist
045600050414     c                   Kfld                    BlpAas
045700050414     c                   Kfld                    BlpLnp
045800050414     c                   Kfld                    BlpNrs
045900050414     c                   Kfld                    BlpNsp
046000050414     c                   Kfld                    KAr5Trd
046100041018     c
046200041021     c                   z-add     1             codut             1 0
046300041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
046400041021     C                   MOVEL     '7J'          KCOD
046500041021     C                   Z-ADD     1             X                 4 0
046600041021     C     KTAB2         SETLL     TABEL
046700041021     C     KTAB2         READE     TABEL                                  99
046800041021     C*
046900041021    2C     *IN99         DOWEQ     *OFF
047000041021    3C     TBLFLG        IFEQ      ' '
047100041021     C                   MOVEL     TBLUNI        DS7J
047200041021     c     §7jrps        ifeq      'A'
047300041021     C                   MOVEL     TBLKEY        NPS(X)
047400041021     C                   ADD       1             X
047500041021     c                   endif
047600041021    3C                   END
047700041021     C*
047800041021     C     KTAB2         READE     TABEL                                  99
047900041021    2C                   ENDDO
048000041022     c     endinz        endsr
