000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040805     H* FNLRT8R *----------------------------------------------------*
000300040805      *
000400000000     H*--------------------------------------------------------------*
000500040805     ffnevb02l  uf a e           k disk
000600040805     ffnblp01l  uf   e           k disk
000700040805     ftabel00f  if   e           k disk
000800060224     ffiar401l  if   e           k disk
000900040805     ffnlbl01l  if   e           k disk
001000040805     ffnevb46f  o    e             disk    rename(fnevb000:fnevb46)
001100040805     f
001200040805     d kpjba         e ds
001300040805     d utedse        e ds                  extname(ut§dse0f)
001400040805     d cncr80        e ds
001500040805     d ds3k          e ds
001600040805     d ds2a          e ds
001700131106     d devb01        e ds
001800081029     d* DS x controllo riempimento schiere
001900081029     d trul0sds      e ds
002000140128
002100140128     d fnlvv1ds      e ds
002200081029     d
002300161012     d clc             s              7  0 dim(9500)                            COD.CLI TR.D.CON.
002400161012     d d3k             s             89    dim(9500)                            DATI TABELLA 3K
002500040806     d cev             s              3    dim(300)                             FLAG RIPRISTINO
002600040805     d ftc             s              1    dim(300)                             FLAG TIPO CONS.
002700040806     d tre             s              1    dim(300)                             FLAG RIPRISTINO
002800040908     d fri             s              1    dim(300)                             FLAG RIPRISTINO
002900140128     d parvar          s            224
003000040805      *
003100040805     d wlbda8          ds
003200040805     d  g08dat                 1      8  0
003300040805     d  g08inv                 9     16  0
003400040805     d  g08err                17     17
003500040805     d  g08tgi                18     22  0
003600140128     D                SDS
003700140128     D  NOMPGM                 1     10
003800040805
003900040805      *
004000000000     C*---------------------------------------------------------------*
004100900615     C*
004200040805     C     *start        setll     fnevb02l
004300040805     C                   do        *hival
004400040927     C                   read      fnevb02l                             0201
004500040805     C                   setoff                                       32
004600040805     C   01              leave
004700051222     C   02              read(n)   fnevb02l
004800040927     C   02              iter
004900131106     c                   movel     evbnot        devb01
005000050331      * verifico se ho libero il rekord di blp per l'aggiornamento anche se
005100050331      * lo eseguo dopo l'evento altrimenti elaboro tutto il giro successivo
005200050331
005300050331      *verifico eventuali legami per riportare gli eventi
005400050331     c                   exsr      legami
005500050331      *verifico eventuali rekord da inviare ai clienti
005600050331     C     kblp          chain(e)  fnblp01l
005700050331     c                   if        %error
005800050331     c                   iter
005900050331     c                   endif
006000050331
006100040806      *scrivo rekord spia per titas se evento da rilevare
006200040806     c                   z-add     1             xy                3 0
006300040806     c     evbcev        lookup    cev(xy)                                30
006400040806     c                   if        *in30 and tre(xy) = 'S'
006500040805     c                   write     fnevb46
006600040806     c                   endif
006700040806      * aggiorno evento come trasmesso sempre
006800040805     c                   move      'T'           evbftr
006900040805     c                   move      amgudt        evbdtr
007000040805     c                   except    aggevb
007100040805      *
007200040908     **
007300131106     c                   if        %Found(fnblp01l)
007400040805     c                   exsr      invcli
007500040908     C                   Z-ADD     1             W
007600040908     C     EVBCEV        LOOKUP    CEV(W)                                 33
007700040908     C   33FRI(W)        IFEQ      'S'                                           RIPRISTINO
007800040908     C* SE RICEVO RIPRISTINO AZZERO DATI CONSEGNA
007900040908     C*  LA PULIZIA DATI DI CONSEGNA PER IL RIPRISTINO NON
008000040908     C* VIENE PIU' FATTO QUI MA DALLA RICEZIONE DATI CONSEGNA
008100040908      *
008200040908     C                   ELSE
008300131213     c                   if        §notprg <> 'P'
008400040908     C   33              MOVEL     FTC(W)        BLPTMC                          TIPO MANC.CONS.
008500131213     c                   endif
008600040908     C                   ENDIF
008700131106      * non aggiorna BLP se primo evento
008800040908     c                   except    aggblp
008900040908     c                   endif
009000140128     c* richiamno pgm Scrittura/aggior. file recupero variazioni post-fatturaz.
009100140128     c                   if        evbcev='RIC'
009200140128     c                   exsr      sr_tivrb
009300140128     c                   endif
009400040805     C                   enddo
009500040908     C*lancio pgm elaborazione eventi per aggiornamento TITAS
009600040910      * Imposto sistema informativo x sottomissione azione
009700040910      * ambiente di prova
009800040910     c                   If        %subst(knsif:7:1) = 'P'
009900040910     c                   Eval      knsif = 'GAITRAPSP '
010000040910     c                   Else
010100040910     c                   Eval      knsif = 'GAITRA201 '
010200040910     c                   EndIf
010300050411     c                   eval      KQIEB = 'KSKEDULA'
010400040908     C                   MOVEL     'LRT9'        KCOAZ
010500050504     C                   clear                   Kbuff
010600040908     C                   CALL      'BCH10'
010700040908     C                   PARM                    KPJBA
010800140128
010900140128     C                   MOVEL     'C'           iv1tla
011000140128     c                   movel     fnlvv1ds      kpjbu
011100140128     C                   CALL      'FNLVV1R'
011200140128     C                   PARM                    kpjba
011300040908     C*
011400040805     C                   seton                                        lr
011500040805      *-------------------------------------------------------------------
011600040805     c     invcli        begsr
011700040805      *-------------------------------------------------------------------
011800941027      *
011900040805     C                   setoff                                       32
012000040805     C                   z-add     1             w
012100040806     C     blpccm        ifne      0
012200040805     C     blpccm        lookup    clc(w)                                 32
012300040806     C                   endif
012400040806     C  N32blpksc        ifne      0
012500040805     C                   z-add     1             w
012600040805     C     blpksc        lookup    clc(w)                                 32
012700040806     C                   endif
012800060214     C*  ANCORA NON TROVATO CHAI SU ar4 PER VERIFICARE SE ESISTE IL
012900011002      *CODICE DEL CLIENTE NEL CAMPO NOTE
013000040806     C  N32              do
013100060224     C     kar4          chain     fiar401l                           66
013200040806     C     *in66         ifeq      '0'
013300060214     C                   movel     ar4not        kscj              7 0
013400040805     C                   z-add     1             w
013500040805     C     kscj          lookup    clc(w)                                 32
013600040806     C                   endif
013700040806     C                   enddo
013800941027      *
013900040805     C                   movel     d3k(w)        ds3k
014000980406      * SE LA BOLLA E' GIA' CONSEGNATA NON RISFLEGGO NULLA
014100040806     C     blpdcm        ifeq      0
014200040805     C     *in32         andeq     *on
014300040806     C     evbcev        ifne      'MIC'
014400040805     C     evbcev        andne     'NIC'
014500040805     C     §3kcq3        oreq      'S'
014600040805     C                   movel     *blank        blpft3
014700040805     C                   move      *zeros        blpdt3
014800040806     C                   endif
014900040806     C                   endif
015000040805
015100040805     C                   endsr
015200040805      *-----------------------------------------------------
015300040805     c     legami        begsr
015400040805      *-----------------------------------------------------
015500040805     C     kblp          chain     fnlbl01l
015600040805     c                   if        %found(fnlbl01l)
015700040805     C     lbllao        ifne      lbllan
015800040805     C     lbllao        orne      lbllap
015900040805     C                   move      *blank        evbftr
016000040805     C                   move      *zeros        evbdtr
016100040805     C                   move      lbllpo        evblnp
016200040805     C                   move      lblaao        evbaas
016300040805     C                   move      lblnro        evbnrs
016400040805     C                   move      lblnso        evbnsp
016500040805     C                   write     fnevb000
016600040805     C                   endif
016700040805     C                   endif
016800040426      **
016900040805     C                   endsr
017000140128     C**************************************************************************
017100140128     C* Richiamo pgm gestione file recupero variazioni post-fatturaz. (TIVRB)
017200140128     C**************************************************************************
017300140128     C     sr_tivrb      BEGSR
017400140128     c                   clear                   fnlvv1ds
017500140128     c                   eval      iv1cvb='RI'
017600140128     c                   eval      iv1pgm=nompgm
017700140128     c                   eval      iv1aas=blpaas
017800140128     c                   eval      iv1lnp=blplnp
017900140128     c                   eval      iv1nrs=blpnrs
018000140128     c                   eval      iv1nsp=blpnsp
018100140128     c                   eval      parvar='S'
018200140128     c                   movel     fnlvv1ds      kpjbu
018300140128     c                   call      'FNLVV1R'
018400140128     c                   parm                    kpjba
018500140128     c                   parm                    parvar
018600140128     c                   endsr
018700930422     C*---------------------------------------------------------------*
018800040805     C     *inzsr        begsr
018900930422     C*---------------------------------------------------------------*
019000040805     C     *entry        plist
019100040805     C                   parm                    kpjba
019200040805     C                   z-add     1             codut
019300040805     C                   call      'X§PARUT'
019400040805     C                   parm                    utedse
019500040805     C                   movel     rec80         cncr80
019600040805     C                   movel     ragut         rsut             20
019700930422      *
019800040805     C                   time                    whhudt           14 0
019900040805     C                   movel     whhudt        ora               6 0
020000040805     C                   move      whhudt        g08dat
020100040805     C                   move      *zeros        g08inv
020200040805     C                   movel     *blank        g08err
020300040805     C                   call      'XSRDA8'
020400040805     C                   parm                    wlbda8
020500040805     C                   z-add     g08inv        amgudt            8 0
020600040805     C                   z-add     g08inv        woggi             8 0
020700941027     C* CARICO I CODICI CLIENTI A CUI MANDARE I DATI DI CONSEGNA
020800040805     C                   move      1             tblkut
020900040805     C                   movel     '3K'          tblcod
021000040805     C                   move      *zeros        w                 4 0
021100040805     C                   move      *zeros        x                 4 0
021200040805     C     keytab        setll     tabel00f
021300040805     C                   do        *hival
021400040805     C     keytab        reade     tabel00f                               31
021500040805     C   31              leave
021600040805     C                   movel     tbluni        ds3k
021700040805     C                   add       1             x
021800040805     C                   movel     tblkey        clc(x)
021900040805     C                   movel     tbluni        d3k(x)
022000040805     C                   enddo
022100081029     c**
022200081029     c                   clear                   trul0sds
022300081029     c                   eval      i0sski='D3K'
022400081029     c                   eval      i0sele=%elem(D3K)
022500081029     c                   eval      i0spie=x
022600081029     c                   eval      i0scod='  3K '
022700081029     c                   eval      i0sfile='TABEL00F  '
022800081029     c                   eval      i0ssif=knsif
022900081029     c                   eval      i0spgm='FNLRT8R'
023000081029     c                   movel     trul0sds      kpjbu
023100081029     c                   call      'TRUL0SR'
023200081029     c                   parm                    kpjba
023300930422     C*                                                    *
023400930422     C* CARICO I CODICI EVENTI
023500040805     C                   move      1             tblkut
023600040805     C                   movel     '2A'          tblcod
023700040805     C                   move      *zeros        w
023800040805     C     keytab        setll     tabel00f
023900040805     C                   do        *hival
024000040805     C     keytab        reade     tabel00f                               31
024100040805     C   31              leave
024200040805     C                   movel     tbluni        ds2a
024300040805     C                   add       1             w
024400040805     C                   movel     §2aftc        ftc(w)                         TIPO MANCATA CONS.
024500040806     C                   movel     §2atre        tre(w)                         FLAG RIPRISTINO
024600040806     C                   movel     tblkey        cev(w)                         FLAG RIPRISTINO
024700040908     C                   movel     §2atip        fri(w)                         FLAG RIPRISTINO
024800040805     C                   enddo
024900081029     c**
025000081029     c                   clear                   trul0sds
025100081029     c                   eval      i0sski='CEV'
025200081029     c                   eval      i0sele=%elem(CEV)
025300081029     c                   eval      i0spie=W
025400081029     c                   eval      i0scod='  2A '
025500081029     c                   eval      i0sfile='TABEL00F  '
025600081029     c                   eval      i0ssif=knsif
025700081029     c                   eval      i0spgm='FNLRT8R'
025800081029     c                   movel     trul0sds      kpjbu
025900081029     c                   call      'TRUL0SR'
026000081029     c                   parm                    kpjba
026100930422     C*
026200930422     C* DEFINIZIONE CHIAVI
026300930422     C*                                                    *
026400040805     C     keytab        klist
026500040805     C                   kfld                    tblkut
026600040805     C                   kfld                    tblcod
026700930422     C*
026800040805     C     kblp          klist
026900040908     C                   kfld                    evbaas
027000040805     C                   kfld                    evblnp
027100040805     C                   kfld                    evbnrs
027200040805     C                   kfld                    evbnsp
027300040805
027400040805     C     kevb          klist
027500040805     C                   kfld                    evbaas
027600040805     C                   kfld                    evblnp
027700040805     C                   kfld                    evbnrs
027800040805     C                   kfld                    evbnsp
027900040805     C                   kfld                    evbcev
028000040805     C                   kfld                    evbdev
028100040805     C                   kfld                    evbhev
028200011002     C*
028300060214     C     kar4          klist
028400040908     C                   kfld                    evbaas
028500040805     C                   kfld                    evblnp
028600040805     C                   kfld                    evbnrs
028700040805     C                   kfld                    evbnsp
028800040805     C                   kfld                    tipor             1
028900040805     C                   move      'Z'           tipor
029000011002     C*
029100040805     C                   endsr
029200040805      *--------------------------------------------------------
029300040805     Ofnblp000  e            aggblp
029400040908     O                       blptmc
029500040805     O                       blpft3
029600040805     O                       blpdt3
029700040805     Ofnevb000  E            aggevb
029800040805     O                       evbftr
029900040805     O                       evbdtr
