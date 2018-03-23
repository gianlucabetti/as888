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
001800170314     d dsBL4Z        e ds                  qualified
001900170314     d                                     inz
002000081029     d* DS x controllo riempimento schiere
002100081029     d trul0sds      e ds
002200140128
002300140128     d fnlvv1ds      e ds
002400081029     d
002500170314     d*// clc             s              7  0 dim(9500)                            COD.CLI TR.D.CON.
002600170314     d*// d3k             s             89    dim(9500)                            DATI TABELLA 3K
002700040806     d cev             s              3    dim(300)                             FLAG RIPRISTINO
002800040805     d ftc             s              1    dim(300)                             FLAG TIPO CONS.
002900040806     d tre             s              1    dim(300)                             FLAG RIPRISTINO
003000040908     d fri             s              1    dim(300)                             FLAG RIPRISTINO
003100140128     d parvar          s            224
003200040805      *
003300040805     d wlbda8          ds
003400040805     d  g08dat                 1      8  0
003500040805     d  g08inv                 9     16  0
003600040805     d  g08err                17     17
003700040805     d  g08tgi                18     22  0
003800140128     D                SDS
003900140128     D  NOMPGM                 1     10
004000170314
004100170314     d w               s              4  0 inz
004200170314     d $Tab3K          s               n   inz
004300040805
004400040805      *
004500000000     C*---------------------------------------------------------------*
004600900615     C*
004700040805     C     *start        setll     fnevb02l
004800040805     C                   do        *hival
004900040927     C                   read      fnevb02l                             0201
005000170314     C*//                   setoff                                       32
005100040805     C   01              leave
005200051222     C   02              read(n)   fnevb02l
005300040927     C   02              iter
005400131106     c                   movel     evbnot        devb01
005500050331      * verifico se ho libero il rekord di blp per l'aggiornamento anche se
005600050331      * lo eseguo dopo l'evento altrimenti elaboro tutto il giro successivo
005700050331
005800050331      *verifico eventuali legami per riportare gli eventi
005900050331     c                   exsr      legami
006000050331      *verifico eventuali rekord da inviare ai clienti
006100050331     C     kblp          chain(e)  fnblp01l
006200050331     c                   if        %error
006300050331     c                   iter
006400050331     c                   endif
006500050331
006600040806      *scrivo rekord spia per titas se evento da rilevare
006700040806     c                   z-add     1             xy                3 0
006800040806     c     evbcev        lookup    cev(xy)                                30
006900040806     c                   if        *in30 and tre(xy) = 'S'
007000040805     c                   write     fnevb46
007100040806     c                   endif
007200040806      * aggiorno evento come trasmesso sempre
007300040805     c                   move      'T'           evbftr
007400040805     c                   move      amgudt        evbdtr
007500040805     c                   except    aggevb
007600040805      *
007700040908     **
007800131106     c                   if        %Found(fnblp01l)
007900040805     c                   exsr      invcli
008000040908     C                   Z-ADD     1             W
008100040908     C     EVBCEV        LOOKUP    CEV(W)                                 33
008200040908     C   33FRI(W)        IFEQ      'S'                                           RIPRISTINO
008300040908     C* SE RICEVO RIPRISTINO AZZERO DATI CONSEGNA
008400040908     C*  LA PULIZIA DATI DI CONSEGNA PER IL RIPRISTINO NON
008500040908     C* VIENE PIU' FATTO QUI MA DALLA RICEZIONE DATI CONSEGNA
008600040908      *
008700040908     C                   ELSE
008800131213     c                   if        §notprg <> 'P'
008900040908     C   33              MOVEL     FTC(W)        BLPTMC                          TIPO MANC.CONS.
009000131213     c                   endif
009100040908     C                   ENDIF
009200131106      * non aggiorna BLP se primo evento
009300040908     c                   except    aggblp
009400040908     c                   endif
009500140128     c* richiamno pgm Scrittura/aggior. file recupero variazioni post-fatturaz.
009600140128     c                   if        evbcev='RIC'
009700140128     c                   exsr      sr_tivrb
009800140128     c                   endif
009900040805     C                   enddo
010000040908     C*lancio pgm elaborazione eventi per aggiornamento TITAS
010100040910      * Imposto sistema informativo x sottomissione azione
010200040910      * ambiente di prova
010300040910     c                   If        %subst(knsif:7:1) = 'P'
010400040910     c                   Eval      knsif = 'GAITRAPSP '
010500040910     c                   Else
010600040910     c                   Eval      knsif = 'GAITRA201 '
010700040910     c                   EndIf
010800050411     c                   eval      KQIEB = 'KSKEDULA'
010900040908     C                   MOVEL     'LRT9'        KCOAZ
011000050504     C                   clear                   Kbuff
011100040908     C                   CALL      'BCH10'
011200040908     C                   PARM                    KPJBA
011300140128
011400140128     C                   MOVEL     'C'           iv1tla
011500140128     c                   movel     fnlvv1ds      kpjbu
011600140128     C                   CALL      'FNLVV1R'
011700140128     C                   PARM                    kpjba
011800040908     C*
011900040805     C                   seton                                        lr
012000040805      *-------------------------------------------------------------------
012100040805     c     invcli        begsr
012200040805      *-------------------------------------------------------------------
012300941027      *
012400170314     C*//                   setoff                                       32
012500170314     C*//                   z-add     1             w
012600170314     c                   clear                   ds3K
012700170314     c                   clear                   $Tab3K
012800170314     c                   eval      TBLkut = 1
012900170314     c                   eval      TBLcod = '3K'
013000040806     C     blpccm        ifne      0
013100170314     C*//     blpccm        lookup    clc(w)                                 32
013200170314     c                   movel(P)  BLPccm        TBLkey
013300170314     c     K03TAB00      chain     TABEL
013400170314     c                   eval      $Tab3K = %found(TABEL00F)
013500040806     C                   endif
013600170314     C*//  N32blpksc        ifne      0
013700170314     C*//                   z-add     1             w
013800170314     C*//     blpksc        lookup    clc(w)                                 32
013900170314     c     BLPksc        ifne      0
014000170314     c     $Tab3K        andeq     *off
014100170314     c                   movel(P)  BLPksc        TBLkey
014200170314     c     K03TAB00      chain     TABEL
014300170314     c                   eval      $Tab3K = %found(TABEL00F)
014400040806     C                   endif
014500060214     C*  ANCORA NON TROVATO CHAI SU ar4 PER VERIFICARE SE ESISTE IL
014600011002      *CODICE DEL CLIENTE NEL CAMPO NOTE
014700170314     C*//  N32              do
014800170314     c                   If        $Tab3K = *off
014900060224     C     kar4          chain     fiar401l                           66
015000040806     C     *in66         ifeq      '0'
015100170314     C*//                   movel     ar4not        kscj              7 0
015200170314     C*//                   z-add     1             w
015300170314     C*//     kscj          lookup    clc(w)                                 32
015400170314     c                   movel     AR4not        dsBL4Z
015500170314     c                   movel(P)  dsBL4Z.§B4okc TBLkey
015600170314     c     K03TAB00      chain     TABEL
015700170314     c                   eval      $Tab3K = %found(TABEL00F)
015800040806     C                   endif
015900170314     C                   EndIf
016000170314     C*//                   enddo
016100941027      *
016200170314     C*//                   movel     d3k(w)        ds3k
016300170314     c                   if        $Tab3K = *on
016400170314     c                   eval      ds3K = TBLuni
016500170314     c                   endif
016600170314      *
016700980406      * SE LA BOLLA E' GIA' CONSEGNATA NON RISFLEGGO NULLA
016800040806     C     blpdcm        ifeq      0
016900170314     C*//     *in32         andeq     *on
017000170314     c     $Tab3K        andeq     *on
017100040806     C     evbcev        ifne      'MIC'
017200040805     C     evbcev        andne     'NIC'
017300040805     C     §3kcq3        oreq      'S'
017400040805     C                   movel     *blank        blpft3
017500040805     C                   move      *zeros        blpdt3
017600040806     C                   endif
017700040806     C                   endif
017800040805
017900040805     C                   endsr
018000040805      *-----------------------------------------------------
018100040805     c     legami        begsr
018200040805      *-----------------------------------------------------
018300040805     C     kblp          chain     fnlbl01l
018400040805     c                   if        %found(fnlbl01l)
018500040805     C     lbllao        ifne      lbllan
018600040805     C     lbllao        orne      lbllap
018700040805     C                   move      *blank        evbftr
018800040805     C                   move      *zeros        evbdtr
018900040805     C                   move      lbllpo        evblnp
019000040805     C                   move      lblaao        evbaas
019100040805     C                   move      lblnro        evbnrs
019200040805     C                   move      lblnso        evbnsp
019300040805     C                   write     fnevb000
019400040805     C                   endif
019500040805     C                   endif
019600040426      **
019700040805     C                   endsr
019800140128     C**************************************************************************
019900140128     C* Richiamo pgm gestione file recupero variazioni post-fatturaz. (TIVRB)
020000140128     C**************************************************************************
020100140128     C     sr_tivrb      BEGSR
020200140128     c                   clear                   fnlvv1ds
020300140128     c                   eval      iv1cvb='RI'
020400140128     c                   eval      iv1pgm=nompgm
020500140128     c                   eval      iv1aas=blpaas
020600140128     c                   eval      iv1lnp=blplnp
020700140128     c                   eval      iv1nrs=blpnrs
020800140128     c                   eval      iv1nsp=blpnsp
020900140128     c                   eval      parvar='S'
021000140128     c                   movel     fnlvv1ds      kpjbu
021100140128     c                   call      'FNLVV1R'
021200140128     c                   parm                    kpjba
021300140128     c                   parm                    parvar
021400140128     c                   endsr
021500930422     C*---------------------------------------------------------------*
021600040805     C     *inzsr        begsr
021700930422     C*---------------------------------------------------------------*
021800040805     C     *entry        plist
021900040805     C                   parm                    kpjba
022000040805     C                   z-add     1             codut
022100040805     C                   call      'X§PARUT'
022200040805     C                   parm                    utedse
022300040805     C                   movel     rec80         cncr80
022400040805     C                   movel     ragut         rsut             20
022500930422      *
022600040805     C                   time                    whhudt           14 0
022700040805     C                   movel     whhudt        ora               6 0
022800040805     C                   move      whhudt        g08dat
022900040805     C                   move      *zeros        g08inv
023000040805     C                   movel     *blank        g08err
023100040805     C                   call      'XSRDA8'
023200040805     C                   parm                    wlbda8
023300040805     C                   z-add     g08inv        amgudt            8 0
023400040805     C                   z-add     g08inv        woggi             8 0
023500170314     C*//* CARICO I CODICI CLIENTI A CUI MANDARE I DATI DI CONSEGNA
023600170314     C*//                   move      1             tblkut
023700170314     C*//                   movel     '3K'          tblcod
023800170314     C*//                   move      *zeros        w                 4 0
023900170314     C*//                   move      *zeros        x                 4 0
024000170314     C*//     keytab        setll     tabel00f
024100170314     C*//                   do        *hival
024200170314     C*//     keytab        reade     tabel00f                               31
024300170314     C*//   31              leave
024400170314     C*//                   movel     tbluni        ds3k
024500170314     C*//                   add       1             x
024600170314     C*//                   movel     tblkey        clc(x)
024700170314     C*//                   movel     tbluni        d3k(x)
024800170314     C*//                   enddo
024900081029     c**
025000170314     c*//                   clear                   trul0sds
025100170314     c*//                   eval      i0sski='D3K'
025200170314     c*//                   eval      i0sele=%elem(D3K)
025300170314     c*//                   eval      i0spie=x
025400170314     c*//                   eval      i0scod='  3K '
025500170314     c*//                   eval      i0sfile='TABEL00F  '
025600170314     c*//                   eval      i0ssif=knsif
025700170314     c*//                   eval      i0spgm='FNLRT8R'
025800170314     c*//                   movel     trul0sds      kpjbu
025900170314     c*//                   call      'TRUL0SR'
026000170314     c*//                   parm                    kpjba
026100930422     C*                                                    *
026200930422     C* CARICO I CODICI EVENTI
026300040805     C                   move      1             tblkut
026400040805     C                   movel     '2A'          tblcod
026500040805     C                   move      *zeros        w
026600040805     C     keytab        setll     tabel00f
026700040805     C                   do        *hival
026800040805     C     keytab        reade     tabel00f                               31
026900040805     C   31              leave
027000040805     C                   movel     tbluni        ds2a
027100040805     C                   add       1             w
027200040805     C                   movel     §2aftc        ftc(w)                         TIPO MANCATA CONS.
027300040806     C                   movel     §2atre        tre(w)                         FLAG RIPRISTINO
027400040806     C                   movel     tblkey        cev(w)                         FLAG RIPRISTINO
027500040908     C                   movel     §2atip        fri(w)                         FLAG RIPRISTINO
027600040805     C                   enddo
027700081029     c**
027800081029     c                   clear                   trul0sds
027900081029     c                   eval      i0sski='CEV'
028000081029     c                   eval      i0sele=%elem(CEV)
028100081029     c                   eval      i0spie=W
028200081029     c                   eval      i0scod='  2A '
028300081029     c                   eval      i0sfile='TABEL00F  '
028400081029     c                   eval      i0ssif=knsif
028500081029     c                   eval      i0spgm='FNLRT8R'
028600081029     c                   movel     trul0sds      kpjbu
028700081029     c                   call      'TRUL0SR'
028800081029     c                   parm                    kpjba
028900930422     C*
029000930422     C* DEFINIZIONE CHIAVI
029100930422     C*                                                    *
029200040805     C     keytab        klist
029300040805     C                   kfld                    tblkut
029400040805     C                   kfld                    tblcod
029500170314      *
029600170314     C     K03TAB00      klist
029700170314     C                   kfld                    TBLkut
029800170314     C                   kfld                    TBLcod
029900170314     C                   kfld                    TBLkey
030000930422     C*
030100040805     C     kblp          klist
030200040908     C                   kfld                    evbaas
030300040805     C                   kfld                    evblnp
030400040805     C                   kfld                    evbnrs
030500040805     C                   kfld                    evbnsp
030600040805
030700040805     C     kevb          klist
030800040805     C                   kfld                    evbaas
030900040805     C                   kfld                    evblnp
031000040805     C                   kfld                    evbnrs
031100040805     C                   kfld                    evbnsp
031200040805     C                   kfld                    evbcev
031300040805     C                   kfld                    evbdev
031400040805     C                   kfld                    evbhev
031500011002     C*
031600060214     C     kar4          klist
031700040908     C                   kfld                    evbaas
031800040805     C                   kfld                    evblnp
031900040805     C                   kfld                    evbnrs
032000040805     C                   kfld                    evbnsp
032100040805     C                   kfld                    tipor             1
032200040805     C                   move      'Z'           tipor
032300011002     C*
032400040805     C                   endsr
032500040805      *--------------------------------------------------------
032600040805     Ofnblp000  e            aggblp
032700040908     O                       blptmc
032800040805     O                       blpft3
032900040805     O                       blpdt3
033000040805     Ofnevb000  E            aggevb
033100040805     O                       evbftr
033200040805     O                       evbdtr
