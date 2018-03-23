000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040806     H* FNLRT9R *----------------------------------------------------*
000300000000     H*--------------------------------------------------------------*
000400040823     Ffnevb46f  uf   e             disk
000500040819     Ftitas30c  uf   e           k disk
000600040819     Fazorg01L  if   e           k disk
000700040819     Ftabel00f  if   e           k disk
000800990804     D*--------------------------------------------------------------*
000900040806     D kpjba         e ds
001000040806     D wlbda8          ds
001100040806     D  g08dat                 1      8  0
001200040817     D  g08inv                 9     16  0
001300040806     D  g08err                17     17
001400040806     D  g08tgi                18     22  0
001401081014     d oggim10         s               d
001402081014     d dataiso         s               d
001500030108     D og143         e ds
001600040819     D dtasflo       e ds
001700040819     d ds2a          e ds
001800040819
001900080903     d elav            s              3    dim(200)                             FLAG RIPRISTINO
002000021223     C*---------------------------------------------------------------*
002100040806     D digits          c                   const('0123456789')
002200040806      *---------------------------------------------------------------*
002300040806     i*  indicatori  tipo record titas
002400040806     ititas000      50
002500040806     ititas010      51
002600040806     ititasp00      52
002700040806      *---------------------------------------------------------------*
002800021223
002900040806     c* loop CARICAMENTO EVENTI
003000040823     c     *start        setll     fnevb46f
0031000408061    c                   DO        *hival
003200041008     c                   read      fnevb46f                             0201
003300040806     c   01              leave
003400041008     c   02              iter
003500040806     c* CONTROLLO SE E' GIA' STATA CARICATA LA BOLLA
003600040806     c                   z-add     evbaas        kaas
003700040806     c                   z-add     evblnp        klnp
003800040806     c                   z-add     evbnrs        knrs
003900040806     c                   z-add     evbnsp        knsp
004000040920     c                   setoff                                       505152
004100040329     C     KTAS          CHAIN     TITAS30C
0042000408062    c                   if        %found(titas30c)
004300030110      **
004400030110      **  Solo se ancora non ho ricevuto la data di consegna TASDCM
0045000408063    c                   if        TASDCM = 0
004600030110      **
004700021223      **  se l'evento è un MIC o un NIC deve aggiornare TITAS
004800021223      **   nella data e nr.Documento
004900021223      **    quando è un MIC deve impostare la EVBDEV nella DATA
005000021223      **    e EVBNOT nel Nr.Doc.
005100021223      **  se invece è un NIC deve azzerarer i 2 campi sempre che si tratti della
005200021223      **    stessa data nr.documento.
005300030108      **
005400030108      **  Se è un Export non deve aggiornare il Nr.Distinta in quanto nel nr.distinta
005500030108      **    c'è il numero del riferimento del Partner
005600030108     C                   MOVEL     *BLANKS       ORGestero         1
005700040806     C     tasLNA        CHAIN     azorg01L                           30
005800040806     C  N30              MOVEL     orgDE3        og143
0059000408064    C  N30§OgNTW        ifeq      'FED'
006000040806     C     §OgNTW        oreq      'EEX'
006100040806     C     §OgNTW        oreq      'EUP'
006200040806     C     §OgNTW        oreq      'DPD'
006300040806     C                   MOVEL     'E'           ORGestero         1
006400040806e4   C                   END
006500030108      **
0066000408064    c                   if        EVBCEV = 'MIC' or EVBCEV = 'NIC'
006700021223     c                   movel     evbnot        alfa7             7
006800021223     C     digits        check     alfa7                                  99
006900021223      *
0070000408065    c                   if        EVBCEV = 'MIC'
007100021223     c                   z-add     evbdev        tasddc
0072000408066    c     ORGestero     ifeq      *blank
007300021223     C  N99              MOVEL     alfa7         tasndc
007400040806e6   C                   End
007500040806e5   C                   End
007600040806      *
007700040806      * azzera solo se c'è coincidenza con il nr.distinta
0078000408065    c                   if        EVBCEV = 'NIC'
007900040806     C  N99              MOVEL     alfa7         numer7            7 0
008000040806      *
0081000408066    c                   If        numer7 = tasndc
008200040806     c                   z-add     0             tasddc
008300040806     c                   z-add     0             tasndc
008400040806e6   C                   EndIf
008500040806      *
008600040806e5   C                   End
008700040806e4   C                   EndIF
008800040819      * verifico se evento di lasciato avviso nel caso lo aggiungo al conto
008900040819      * nel campo del TASFLO
009000040823     c     evbcev        lookup    elav                                   90
009100040819     c                   if        *in90
009200040819     c                   movel     tasflo        dtasflo
009300040819     c                   if        §flonav <> ' '
009400040819     c                   move      §flonav       cont              1 0
009500040819     c                   else
009600040819     c                   z-add     0             cont
009700040819     c                   end
009800040819     c                   if        cont < 9
009900040819     c                   add       1             cont
010000040819     c                   endif
010100040819     c
010200040819     c                   move      cont          §flonav
010300040819     c                   movel(p)  dtasflo       tasflo
010400040819     c                   end
010500021223      * aggiorna TITAS
010600040806     C   50              update    titas000
010700040806     C   51              update    titas010
010800040806     C   52              update    titasp00
010900040806e3   C                   End
011000021223      **
011100040823     C                   delete    fnevb46f
011101081014     c                   else
011102081014     c* se non trova titas e l'evento è + vecchio di 10 gg cancello
011103081014     c                   movel     evbdev        dataiso
011104081014     c                   if        dataiso < oggim10
011105081014     C                   delete    fnevb46f
011106081014     c                   end
011200040908e2   C                   endif
011300040806e1   C                   enddo
011400040806      *
011500040806     C                   seton                                        lr
011600930422     C*---------------------------------------------------------------*
011700040806     C     *inzsr        begsr
011800930422     C*---------------------------------------------------------------*
011900040806     c     *entry        plist
012000040806     c                   parm                    kpjba
012100930422      *
012200040806     c                   time                    whhudt           14 0
012300040806     c                   movel     whhudt        ora               6 0
012400040806     c                   move      whhudt        g08dat
012500040806     c                   move      *zeros        g08inv
012600040806     c                   movel     *blank        g08err
012700040806     c                   call      'XSRDA8'
012800040806     c                   parm                    wlbda8
012900040806     c                   z-add     g08inv        amgudt            8 0
012901081014      * Data del Giorno
012902081014     c                   Movel     g08inv        Oggim10
012904081014      * Data max per cancellazione record già ricevuti
012905081014     c                   Subdur    10:*d         oggim10
013000930422     C*                                                    *
013100040806     C     ktas          klist
013200040806     C                   kfld                    KAAS
013300040806     C                   kfld                    KLNP
013400040806     C                   kfld                    KNRS
013500040806     C                   kfld                    KNSP
013600990804     C*
013700040819     C     keytab        klist
013800040819     C                   kfld                    tblkut
013900040819     C                   kfld                    tblcod
014000040819
014100990804     C* DEFINIZIONE VARIABILI
014200040806     C     *like         define    evbaas        kaas
014300040806     C     *like         define    evblnp        klnp
014400040819     C     *like         define    evbnrs        knrs
014500040806     C     *like         define    evbnsp        knsp
014600990804     C*
014700040819     C* CARICO I CODICI EVENTI solo i lasciati avviso per il conteggio
014800040819     C                   move      1             tblkut
014900040819     C                   movel     '2A'          tblcod
015000040819     C                   move      *zeros        w                 3 0
015100040819     C     keytab        setll     tabel00f
015200040819     C                   do        *hival
015300040819     C     keytab        reade     tabel00f                               31
015400040819     C   31              leave
015500040819     C                   movel     tbluni        ds2a
015600040819     C                   if        §2aftc = 'A'                                 TIPO MANCATA CONS.
015700040819     C                   add       1             w
015800040819     C                   movel     tblkey        elav(w)                        FLAG RIPRISTINO
015900040819     c                   endif
016000040819     C                   enddo
016100040806     C                   endsr
