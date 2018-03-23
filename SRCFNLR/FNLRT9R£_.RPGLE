000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040806     H* FNLRT9R *----------------------------------------------------*
000300000000     H*--------------------------------------------------------------*
000400040823     Ffnevb46f  uf   e             disk
000500040819     Ftitas30c  uf   e           k disk
000600040819     Fazorg01L  if   e           k disk
000700040819     Ftabel00f  if   e           k disk
000800990804     D*--------------------------------------------------------------*
000900040806     D kpjba         e ds
001000131104     D Devb01        e ds
001100040806     D wlbda8          ds
001200040806     D  g08dat                 1      8  0
001300040817     D  g08inv                 9     16  0
001400040806     D  g08err                17     17
001500040806     D  g08tgi                18     22  0
001600081014     d oggim10         s               d
001700081014     d dataiso         s               d
001800030108     D og143         e ds
001900040819     D dtasflo       e ds
002000040819     d ds2a          e ds
002100040819
002200080903     d elav            s              3    dim(200)                             FLAG RIPRISTINO
002300021223     C*---------------------------------------------------------------*
002400040806     D digits          c                   const('0123456789')
002500040806      *---------------------------------------------------------------*
002600040806     i*  indicatori  tipo record titas
002700040806     ititas000      50
002800040806     ititas010      51
002900040806     ititasp00      52
003000040806      *---------------------------------------------------------------*
003100021223
003200040806     c* loop CARICAMENTO EVENTI
003300040823     c     *start        setll     fnevb46f
0034000408061    c                   DO        *hival
003500041008     c                   read      fnevb46f                             0201
003600040806     c   01              leave
003700041008     c   02              iter
003800040806     c* CONTROLLO SE E' GIA' STATA CARICATA LA BOLLA
003900040806     c                   z-add     evbaas        kaas
004000040806     c                   z-add     evblnp        klnp
004100040806     c                   z-add     evbnrs        knrs
004200040806     c                   z-add     evbnsp        knsp
004300040920     c                   setoff                                       505152
004400040329     C     KTAS          CHAIN     TITAS30C
0045000408062    c                   if        %found(titas30c)
004600030110      **
004700030110      **  Solo se ancora non ho ricevuto la data di consegna TASDCM
0048000408063    c                   if        TASDCM = 0
004900030110      **
005000021223      **  se l'evento è un MIC o un NIC deve aggiornare TITAS
005100021223      **   nella data e nr.Documento
005200021223      **    quando è un MIC deve impostare la EVBDEV nella DATA
005300021223      **    e EVBNOT nel Nr.Doc.
005400021223      **  se invece è un NIC deve azzerarer i 2 campi sempre che si tratti della
005500021223      **    stessa data nr.documento.
005600030108      **
005700030108      **  Se è un Export non deve aggiornare il Nr.Distinta in quanto nel nr.distinta
005800030108      **    c'è il numero del riferimento del Partner
005900030108     C                   MOVEL     *BLANKS       ORGestero         1
006000040806     C     tasLNA        CHAIN     azorg01L                           30
006100040806     C  N30              MOVEL     orgDE3        og143
0062000408064    C  N30§OgNTW        ifeq      'FED'
006300040806     C     §OgNTW        oreq      'EEX'
006400040806     C     §OgNTW        oreq      'EUP'
006500040806     C     §OgNTW        oreq      'DPD'
006600040806     C                   MOVEL     'E'           ORGestero         1
006700040806e4   C                   END
006800030108      **
0069000408064    c                   if        EVBCEV = 'MIC' or EVBCEV = 'NIC'
007000131104     c                   movel     evbnot        Devb01
007100131104     C     digits        check     §notNDC                                99
007200021223      *
0073000408065    c                   if        EVBCEV = 'MIC'
007400021223     c                   z-add     evbdev        tasddc
0075000408066    c     ORGestero     ifeq      *blank
007600131104     C  N99              MOVEL     §notNDC       tasndc
007700040806e6   C                   End
007800040806e5   C                   End
007900040806      *
008000040806      * azzera solo se c'è coincidenza con il nr.distinta
0081000408065    c                   if        EVBCEV = 'NIC'
008200131104     C  N99              MOVEL     §notNDC       numer7            7 0
008300040806      *
0084000408066    c                   If        numer7 = tasndc
008500040806     c                   z-add     0             tasddc
008600040806     c                   z-add     0             tasndc
008700040806e6   C                   EndIf
008800040806      *
008900040806e5   C                   End
009000040806e4   C                   EndIF
009100040819      * verifico se evento di lasciato avviso nel caso lo aggiungo al conto
009200040819      * nel campo del TASFLO
009300040823     c     evbcev        lookup    elav                                   90
009400040819     c                   if        *in90
009500040819     c                   movel     tasflo        dtasflo
009600040819     c                   if        §flonav <> ' '
009700040819     c                   move      §flonav       cont              1 0
009800040819     c                   else
009900040819     c                   z-add     0             cont
010000040819     c                   end
010100040819     c                   if        cont < 9
010200040819     c                   add       1             cont
010300040819     c                   endif
010400040819     c
010500040819     c                   move      cont          §flonav
010600040819     c                   movel(p)  dtasflo       tasflo
010700040819     c                   end
010800021223      * aggiorna TITAS
010900040806     C   50              update    titas000
011000040806     C   51              update    titas010
011100040806     C   52              update    titasp00
011200040806e3   C                   End
011300021223      **
011400040823     C                   delete    fnevb46f
011500081014     c                   else
011600081014     c* se non trova titas e l'evento è + vecchio di 10 gg cancello
011700081014     c                   movel     evbdev        dataiso
011800081014     c                   if        dataiso < oggim10
011900081014     C                   delete    fnevb46f
012000081014     c                   end
012100040908e2   C                   endif
012200040806e1   C                   enddo
012300040806      *
012400040806     C                   seton                                        lr
012500930422     C*---------------------------------------------------------------*
012600040806     C     *inzsr        begsr
012700930422     C*---------------------------------------------------------------*
012800040806     c     *entry        plist
012900040806     c                   parm                    kpjba
013000930422      *
013100040806     c                   time                    whhudt           14 0
013200040806     c                   movel     whhudt        ora               6 0
013300040806     c                   move      whhudt        g08dat
013400040806     c                   move      *zeros        g08inv
013500040806     c                   movel     *blank        g08err
013600040806     c                   call      'XSRDA8'
013700040806     c                   parm                    wlbda8
013800040806     c                   z-add     g08inv        amgudt            8 0
013900081014      * Data del Giorno
014000081014     c                   Movel     g08inv        Oggim10
014100081014      * Data max per cancellazione record già ricevuti
014200081014     c                   Subdur    10:*d         oggim10
014300930422     C*                                                    *
014400040806     C     ktas          klist
014500040806     C                   kfld                    KAAS
014600040806     C                   kfld                    KLNP
014700040806     C                   kfld                    KNRS
014800040806     C                   kfld                    KNSP
014900990804     C*
015000040819     C     keytab        klist
015100040819     C                   kfld                    tblkut
015200040819     C                   kfld                    tblcod
015300040819
015400990804     C* DEFINIZIONE VARIABILI
015500040806     C     *like         define    evbaas        kaas
015600040806     C     *like         define    evblnp        klnp
015700040819     C     *like         define    evbnrs        knrs
015800040806     C     *like         define    evbnsp        knsp
015900990804     C*
016000040819     C* CARICO I CODICI EVENTI solo i lasciati avviso per il conteggio
016100040819     C                   move      1             tblkut
016200040819     C                   movel     '2A'          tblcod
016300040819     C                   move      *zeros        w                 3 0
016400040819     C     keytab        setll     tabel00f
016500040819     C                   do        *hival
016600040819     C     keytab        reade     tabel00f                               31
016700040819     C   31              leave
016800040819     C                   movel     tbluni        ds2a
016900040819     C                   if        §2aftc = 'A'                                 TIPO MANCATA CONS.
017000040819     C                   add       1             w
017100040819     C                   movel     tblkey        elav(w)                        FLAG RIPRISTINO
017200040819     c                   endif
017300040819     C                   enddo
017400040806     C                   endsr
