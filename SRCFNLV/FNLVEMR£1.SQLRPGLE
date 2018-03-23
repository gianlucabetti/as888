000100041021      *PARMS CLOSQLCSR(*ENDMOD)
000200041021      *PARMS DYNUSRPRF(*OWNER)
000300041018     H*---------------------------------------------------------------*
000400050418     H*      Elenco colli bollettati                                  *
000500041018     H*---------------------------------------------------------------*
000600041018     H* INDICATORI USATI                                              *
000700041018     H*---------------------------------------------------------------*
000800041022     H* 30 - lookup
000900041022     H* 99 - comodo
001000041018     F*---------------------------------------------------------------*
001100041018     F* DATA BASE                                                     *
001200041018     F*---------------------------------------------------------------*
001300041018     H DECEDIT('0,') DATEDIT(*DMY.)
001400050418     F*
001500041021     FFNBLP00f  IF   E             DISK
001600041021     FFNBLT01L  IF   E           K DISK
001700070117     Ffnbrv07l  IF   E           K DISK
001800041018     FTABEL00F  IF   E           K DISK
001900050414     fFiar501l  if   e           k Disk
002000050419     Ffiapd01l  if   e           k disk
002100050419     Ffnlrr9rp1 O    E             PRINTER oflind(*In99)
002200041018
002300041021     D KCOD            S                   LIKE(TBLCOD) inz
002400041021     D KKEY            S                   LIKE(TBLKEY) inz
002500050414     d KAr5Trd         s                   Like(Ar5Trd)
002600041021     D $CBLPR          S                   LIKE(BLPCBO) inz
002700041021     D savpdr          S                   LIKE(BLPpdr) inz
002800050418     D savrsm          S                   LIKE(BLPrsm) inz
002900050419     D totncl          S                   LIKE(blpncl) inz
003000050419     D totnclspu       S                   LIKE(blpncl) inz
003100050419     D T_totncl        S                   like(p01nclbot) inz
003200050419     D T_totnclspu     S                   like(p01nclspt) inz
003300050419     D wnclspu         S                   LIKE(blpncl) inz
003400041022     D wncl            S                   LIKE(bltnsc) inz
003500050419     D wsquad          S              1    inz
003600050420     D wsquadt         S              1    inz
003700041021     d wsputrov        s              1
003800041021     D NPS             S              2  0 DIM(101)
003900050419     d dataiso         s               d   datfmt(*iso)
004000050419     d dataeur         s               d   datfmt(*eur)
004100050419     d $righe          s              2  0
004200041018     D*------------------
004300041018     D* ARCHITETTURA
004400041018     D*------------------
004500041018     D KPJBA         E DS
004600041018     D*------------------
004700041018     D* PARAMETRI LANCIO
004800041018     D*------------------
004900041018     D PARAM           DS
005000050418     d  w01dd1                        8  0
005100050418     d  aut1                          7  0
005200050418     d  aut2                          7  0
005300050418     d  aut3                          7  0
005400050418     d  aut4                          7  0
005500050419     d  s_aut                  9     36  0 dim(004)
005600050420     d  s_auta                 9     36    dim(004)
005700041021     d*
005800041021     D DS7J          E DS
005900041022     D DS3A          E DS
006000050414     d Dar5Bnb       e Ds
006100041021     d FnblpxxDs     e ds                  ExtName(Fnblp00f)
006200050419     d Azuteds       e ds                  Extname(Azute00f)
006300050419     d dDatiute      e ds
006400050419     d Tibs34ds      e ds                  Inz
006500041018     C*---------------------------------------------------------------*
006600041018     C* MAIN LINE                                                     *
006700041018     C*---------------------------------------------------------------*
006800041021     c* Eseguo sql per estrarre da fnblp solo i dati che mi servono
006900041021     c                   exsr      sr_SqlBlp
007000041021     c* Lettura ed elaborazione di fnblp
007100041021     c                   exsr      sr_LeggiBlp
007200041022     c*
007300041022     c                   seton                                        lr
007400041018     c
007500041021     c**********************************************************************
007600041021     c     Sr_Sqlblp     begsr
007700041021     C/EXEC SQL
007800050419     C+ declare C1 cursor for
007900050419     c+ select * from fnblp00f where blpdrt = :w01dd1 and
008000050419     c+ (blppdr=:aut1 or blppdr=:aut2 or blppdr=:aut3 or blppdr=:aut4)
008100050419     c+ order by blppdr, blprsm for fetch only
008200041021     C/END-EXEC
008300041021     c                   endsr
008400041021     c**********************************************************************
008500041021     c     Sr_LeggiBlp   begsr
008600050419     c*
008700050419     c                   sorta     s_aut
008800050419     c* memorizzo l'indice del primo elemento > 0
008900050419     c                   z-add     1             xx                1 0
009000050420     c                   dow       xx <= 4 and %subst(s_auta(XX):4:4)=*zeros
009100050419     c                   add       1             xx
009200050419     c                   enddo
009300041021     c*
009400041021     c* apertura cursore
009500041021     c*
009600041021     C/EXEC SQL
009700041021     C+ Open C1
009800041021     C/END-EXEC
009900041021     c*
010000041021     c* lettura cursore
010100041021     c*
010200041021     C/EXEC SQL
010300041022     C+ Fetch C1 into :fnblpxxds
010400041021     C/END-EXEC
010500041021     c                   Dow       Sqlcod = 0
010600041021     c* ignoro spedizione di recupero
010700041021    6C     BLPCBO        IFNE      $CBLPR
010800041021     C                   MOVE      '3A'          KCOD
010900041021     C                   MOVE      *BLANKS       KKEY
011000041021     C                   MOVEL     BLPCBO        KKEY
011100041021     C     KTAB1         CHAIN     TABEL00F                           99
011200041021     C  N99              MOVEL     TBLUNI        DS3A
011300041021     C  N99              MOVE      BLPCBO        $CBLPR
011400041021     C   99              CLEAR                   DS3A
011500041021     c                   endif
011600050208     c                   if        §3arbl <> 'R' and blppdr > 0
011700050418     c* ROTTURA DI AUTOTRASPORTATORE
011800050419    3c     blppdr        ifne      savpdr
011900050418     c* stampo dati del precedente aut
012000050418     c     savpdr        ifgt      *zeros
012100050418     c                   exsr      Sr_stampa
012200050419     c* stampo totali
012300050419     c                   exsr      sr_totali
012400050418     c                   endif
012500050419     c* stampo autotasportatore e decodifica
012600050418     c                   eval      savpdr = blppdr
012700050419     c* se l'autotrasportatore letto è > di quello nella schiera stampo
012800050419     c* prima quelli nella schiera inferiori con la dicitura "Non presenti
012900050419     c* spedizioni" --> serve per elencare comunque sempre tutti gli aut
013000050419     c* richiesti al momento del lancio anche se per essi non sono presenti
013100050419     c* dati
013200050419    4c     blppdr        ifge      s_aut(xx)
013300050419     c     xx            dowle     4
013400050419    5c     s_aut(xx)     andle     blppdr
013500050419     c                   clear                   apdrsc
013600050419     c     kapdv         chain     fiapd000
013700050419    6c                   if        %found(fiapd01l)
013800050419     c                   eval      p01rsc = apdrsc
013900050419    6c                   endif
014000050421     c                   if        ($righe + 5) > 60
014100050419     c                   write     legenda
014200050419     c                   write     rcd001
014300050419     c                   z-add     3             $righe
014400050419     c                   endif
014500050419     c                   eval      p01aut = s_Aut(xx)
014600050419     c                   write     rcd003
014700050421     c                   add       5             $righe
014800050419    6c                   if        blppdr <> s_aut(xx)
014900050419     c                   write     nontro
015000050419     c                   add       3             $righe
015100050419    6c                   endif
015200050419     c                   add       1             xx
015300050419    5c                   enddo
015400050419    4c                   endif
015500050418     c                   eval      savrsm = blprsm
015600050418     c                   endif
015700050419     c* ROTTURA DI RAGIONE SOCIALE MITTENTE
015800050418    2c     blprsm        ifne      savrsm
015900050418     c                   exsr      Sr_stampa
016000050418     c                   eval      savrsm = blprsm
016100041021    2c                   endif
016200050418     c*
016300050414     c* conteggio colli spuntati con pst. reale
016400050414     c                   exsr      Sr_Nospu
016500050418     c* conteggio colli bollettati
016600050414    2c                   if        %subst(blpgva:1:1)<>'O'
016700050414     c                   add       blpncl        totncl
016800050414     c                   add       wnclspu       totnclspu
016900050414   x2c                   else
017000050414     c* considerazioni particolari per sped. con particolarità varia "O"
017100050414     c                   Clear                   DAr5Bnb
017200050414     c                   Eval      Kar5Trd = 'BNB'
017300050414     c     KFiar5        Chain     Fiar501l
017400050414    3c                   If        %Found(Fiar501l)
017500050414     c                   Movel     Ar5Uni        DAr5Bnb
017600050414    3c                   EndIf
017700050414     c                   add       §AR5BCOR      totncl
017800050414    3c                   if        blpncl = wnclspu
017900050414     c                   add       §AR5BCOR      totnclspu
018000050414     c                   else
018100050420     c                   eval      wsquad = '§'
018200050414    3c                   endif
018300050414    2c                   endif
018400041021     c                   endif
018500041021     C/EXEC SQL
018600041022     C+ Fetch C1 into :fnblpxxds
018700041021     C/END-EXEC
018800041021
018900041021     c                   EndDo
019000041025     c*
019100050418     c                   exsr      Sr_stampa
019200050419     c                   exsr      Sr_totali
019300050419
019400050419     c* stampo " *** fine stampa ***"
019500050419     c                   write     fines
019600050420     c                   write     legenda
019700041021
019800041021     C/EXEC SQL
019900041021     C+ close C1
020000041021     C/END-EXEC
020100041021
020200041021     c                   endsr
020300041021     c**********************************************************************
020400050418     c     Sr_stampa     begsr
020500050419
020600050419     c     totncl        ifgt      *zeros
020700050419     c                   eval      p01rsm = savrsm
020800050419     c                   z-add     totncl        p01nclbo
020900050419     c                   z-add     totnclspu     p01nclsp
021000050420     c                   eval      p01squad = wsquad
021100050419     c     totncl        sub       totnclspu     p01diff
021200050419     c* salto pagina
021300050419     c                   if        ($righe + 1) > 60
021400050419     c                   write     legenda
021500050419     c                   write     rcd001
021600050419     c                   write     rcd003
021700050421     c                   z-add     8             $righe
021800050419     c                   endif
021900050419     c                   write     rcd005
022000050419     c                   add       1             $righe
022100050419     c                   add       totncl        t_totncl
022200050419     c                   add       totnclspu     t_totnclspu
022300050421     c                   add       p01diff       p01difft
022400050420     c                   if        wsquad <> *blanks
022500050420     c                   eval      wsquadt = wsquad
022600050420     c                   endif
022700050419     c                   endif
022800050418     c*
022900041021     c                   clear                   totncl
023000050414     c                   clear                   totnclspu
023100050414     c                   clear                   wsquad
023200050421     c                   clear                   p01diff
023300041021     c
023400041021     c                   endsr
023500050419     c**********************************************************************
023600050419     c     Sr_totali     begsr
023700050419
023800050419     c     t_totncl      ifgt      *zeros
023900050419     c                   z-add     t_totncl      p01nclbot
024000050419     c                   z-add     T_totnclspu   p01nclspt
024100050420     c                   eval      p01squadt = wsquadt
024200050421     c                   if        ($righe + 5) > 60
024300050419     c                   write     legenda
024400050419     c                   write     rcd001
024500050419     c                   write     rcd003
024600050421     c                   z-add     8             $righe
024700050419     c                   endif
024800050419     c                   write     totaut
024900050421     c                   add       5             $righe
025000050419     c                   endif
025100050419     c*
025200050419     c                   clear                   t_totncl
025300050419     c                   clear                   t_totnclspu
025400050420     c                   clear                   wsquadt
025500050421     c                   clear                   p01difft
025600050419     c
025700050419     c                   endsr
025800041021     c**********************************************************************
025900041021     c     Sr_NoSpu      begsr
026000050414     c                   clear                   wnclspu
026100041021     c* segnacolli     sequenziali
026200041021    1c     blpfns        ifeq      *blanks
026300041021     c                   z-add     blpncd        wncl
026400041021    2c     blpnca        ifeq      0
026500041021     c                   z-add     blpncd        blpnca
026600041021    2c                   endif
026700041021    2c     wncl          dowle     blpnca
026800041021     c                   exsr      Sr_NoSpu2
026801120920     c                   if        wncl=9999999
026802120920     c                   leave
026803120920     c                   endif
026900041021     c                   add       1             wncl
027000041021    2c                   enddo
027100041021   x1c                   else
027200041021     c* segnacolli non sequenziali
027300041021     c     kblt          setll     fnblt01l
027400041021    2c                   do        *hival
027500041021     c     kblt          reade     fnblt01l
027600041021     c                   if        %eof(fnblt01l)
027700041021     c                   leave
027800041021     c                   endif
027900041021     c                   z-add     bltnsc        wncl
028000041021     c                   exsr      Sr_NoSpu2
028100041021    2c                   enddo
028200041021    1c                   endif
028300041022     c                   endsr
028400041021     c**********************************************************************
028500041021     c     Sr_NoSpu2     begsr
028600041021     c                   clear                   wsputrov
028700070117     c     kbrv          setll     fnbrv07l
028800041021     c                   do        *hival
028900070117     c     kbrv          reade     fnbrv07l
029000041021     c* esco dal ciclo per fine file o per spunta trovata
029100070117     c                   if        %eof(fnbrv07l) or wsputrov = 'S'
029200041021     c                   leave
029300041021     c                   endif
029400041021     c* ignoro spunta se è di pistola automatica
029500050415     c* do per scontato che la pistola 00 non sia automatica
029600041021     c     brvnps        lookup    nps                                    30
029700041021     c     *in30         ifeq      *off
029800050415     c     brvnps        oreq      00
029900041021     c                   movel     'S'           wsputrov
030000041021     c                   endif
030100041021     c                   enddo
030200050414     c* se trovata spunta reale incremento nel tot.colli spuntati
030300050414     c     wsputrov      ifeq      'S'
030400050414     c                   add       1             Wnclspu
030500050414     c                   endif
030600041021     c                   endsr
030700041018     c**********************************************************************
030800041018     c     *inzsr        begsr
030900041022     c*
031000041022     c     *entry        plist
031100041022     c                   parm                    kpjba
031200041022     c                   movel     kpjbu         param
031300050419     c*
031400050419     c     *dtaara       define    §azute        azuteds
031500050419     c     *dtaara       define    §datiute      ddatiute
031600050419     c                   in(E)     *dtaara
031700050419     c                   If        %error  or RSUT = *blanks
031800050419     c                   CLEAR                   tibs34ds
031900050419     c                   CALL      'TIBS34R'
032000050419     c                   PARM                    tibs34ds
032100050419     c                   in        *dtaara
032200050419     c                   EndIf
032300041018     c
032400041021     C* Chiave di accesso per il file tabelle
032500041021     C     KTAB1         KLIST
032600041021     C                   KFLD                    CODUT
032700041021     C                   KFLD                    KCOD
032800041021     C                   KFLD                    KKEY
032900041021     C     KTAB2         KLIST
033000041021     C                   KFLD                    CODUT
033100041021     C                   KFLD                    KCOD
033200041021     c     kbrv          klist
033300041025     c                   kfld                    blpfls
033400041021     c                   kfld                    blplna
033500041021     c                   kfld                    blpnrs
033600041021     c                   kfld                    wncl
033700041021     c     kblt          klist
033800041021     c                   kfld                    blpaas
033900041021     c                   kfld                    blplnp
034000041021     c                   kfld                    blpnrs
034100041021     c                   kfld                    blpnsp
034200050414     c     KFiar5        Klist
034300050414     c                   Kfld                    BlpAas
034400050414     c                   Kfld                    BlpLnp
034500050414     c                   Kfld                    BlpNrs
034600050414     c                   Kfld                    BlpNsp
034700050414     c                   Kfld                    KAr5Trd
034800050419     C     kapdv         klist
034900050419     C                   kfld                    apdtip
035000050419     C                   kfld                    s_aut(xx)
035100050419     C                   movel     'A'           apdtip
035200041018     c
035300041021     c                   z-add     1             codut             1 0
035400041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
035500041021     C                   MOVEL     '7J'          KCOD
035600041021     C                   Z-ADD     1             X                 4 0
035700041021     C     KTAB2         SETLL     TABEL
035800041021     C     KTAB2         READE     TABEL                                  99
035900041021     C*
036000041021    2C     *IN99         DOWEQ     *OFF
036100041021    3C     TBLFLG        IFEQ      ' '
036200041021     C                   MOVEL     TBLUNI        DS7J
036300041021     c     §7jrps        ifeq      'A'
036400041021     C                   MOVEL     TBLKEY        NPS(X)
036500041021     C                   ADD       1             X
036600041021     c                   endif
036700041021    3C                   END
036800041021     C*
036900041021     C     KTAB2         READE     TABEL                                  99
037000041021    2C                   ENDDO
037100050419     c*
037200050419     c* Stampo intestazione
037300050419     c                   move      w01dd1        dataiso
037400050419     c                   move      dataiso       dataeur
037500050419     c                   move      dataeur       p01drt
037600050419     C                   TIME                    WTIME            14 0
037700050419     C                   MOVE      WTIME         WDATE             8 0
037800050419     C                   MOVEL     WTIME         UTIME             6 0
037900050419     c                   write     rcd001
038000050419     c                   z-add     3             $righe
038100050419     c*
038200050418     c                   endsr
