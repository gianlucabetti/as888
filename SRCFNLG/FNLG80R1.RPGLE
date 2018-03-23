000100021025      *--------------------------------------------------------------*
000200021025      *    aggiorna ARB dando la bolla per consegnata merce distrutta
000300021025      *    scrive record spia per promemoria
000400021025      *    riceve  i seguenti parametri per aggiornare
000500021025      *    riferimenti spedizione  LNA AAS NRS NSP +
000600021025      *    codice chiusura anomala CCA +
000700021025      *    codice particolarità    MGA
000800031223      *    Se si tratta di giacenza con particolarità GS chiude anche la
000900031223      *    giacenza se è disposizione di rientro
001000021025      *
001100020702      *--------------------------------------------------------------*
001200021024     Ffnlg80d1  cf   e             workstn
001300171215     ffnarb01l  uf   e           k disk    commit
001400171215     ftigcp21l  uf   e           k disk    commit
001500040109     ffiar601l  if   e           k disk
001600051115     ffiar901l  if   e           k disk
001700040109     ftabel00f  if   e           k disk
001800021024     ffnarbx0f  o    e             disk
001900020702
002000021024     D kpjba         e ds
002100021025     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002200021025     D CNCR80        E DS
002300040109     D DS3A          E DS
002400030826      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
002500030826     D fnlr99ds      E DS
002600090629     D trul90ds      E DS
002700090626     D DSARBD        E DS
002800090626     D DSARBG        E DS
002900090626     D DARBT         E DS
003000090626     D DSARBK        E DS
003100090626     D DS48          E DS                  EXTNAME(FNLR48DS)
003101100518     D DS7r          E DS
003102171003      *ds per richiamo monitor variazioni
003103171003     d tigcp         e ds                  extname(tigcp00f)
003104171003     d fnlg33ds      e ds                  inz
003105171003     d tigcpa        e ds                  extname(tigcp00f)
003106171003     d                                     prefix(a)
003107171003     d tigcpb        e ds                  extname(tigcp00f)
003108171003     d                                     prefix(b)
003200021024     d param           ds
003300021025     d  fldlnpa                       3
003400021025     d  fldaasa                       4
003500021025     d  fldnrsa                       2
003600021025     d  fldnspa                       7
003700021025     d  fldcca                        1
003800021029     d  fldgga                        2
003900040109     D D3A             S             71    DIM(100)
004000040109     D C3A             S              2    DIM(100)
004100021025     d dataiso         s               d   datfmt(*iso)
004200021025     d dataeur         s               d   datfmt(*eur)
004300021024      *--------------------------------------------------------------------
004400021024     c
004500021025     c                   move      fldlnpa       fld001
004600021025     c                   move      fldaasa       fld002
004700021025     c                   move      fldnrsa       fld003
004800021025     c                   move      fldnspa       fld004
004900021025
005000021025     c                   z-add     fld001        arblnp
005100021025     c                   z-add     fld002        arbaas
005200021025     c                   z-add     fld003        arbnrs
005300021025     c                   z-add     fld004        arbnsp
005400021025
005500020911     c                   do        *hival
005600021024     c                   exfmt     lg80d101
005700021024     c   kl              leave
005800021024     c                   if        *inkf
005900021025     c     karb          chain     fnarb01l                           99
006000021025     c                   if        not *in99
006100031223      * verifico se si tratta di una particolarita con distruzione merce
006200031223      * ma con giacenza
006300160105     c*                  if        arbgga = 'GS' or
006301160105     c*                            arbgga = 'GT'
006400031223     C                   EXSR      giacenza
006500031223      * se ha la giacenza e non è riuscito a chiuderla esco senza aggiornare
006600031223      * la bolla
006700160105     c*                  if        ok = *blank
006800160105     c*                  leave
006900160105     c*                  endif
007000160105     c*                  endif
007100031217      * do per consegnata la bolla
007200021025     c                   exsr      consegnata
007300021025      * scrittura record spia
007400021024     c                   clear                   fnarbx00
007500021024     c                   move      'C'           arbtpl
007600021024     c                   move      fld001        arblnp
007700021024     c                   move      fld002        arbaas
007800021024     c                   move      fld003        arbnrs
007900021024     c                   move      fld004        arbnsp
008000021025     C                   MOVE      *all'9'       arbndc
008100021025     C                   MOVE      dataiso       arbddc
008200021025     c                   move      simpou        arbfgs
008300021029     c                   move      fldgga        arbgma
008400021024     c                   write     fnarbx00
008500021024     c                   leave
008600021024     c                   endif
008700021025     c                   endif
008800020911     C                   ENDDO
008801171215      *
008802171215     c                   commit
008900020801
009000020702     C                   eval      *inlr = *on
009100020702      *
009200021025      *--------------------------------------------------
009300021025     c     consegnata    begsr
009400021025      *--------------------------------------------------
009500021025      *  DO PER CONSEGNATA LA BOLLA
009600051115     c     karb          chain     fiar901l                           90
009700090629     c                   if        not*in90
009800090629     c                   exsr      anncass
009900040108     c                   end
010000090629     C                   MOVEL     'Y'           ARBICC
010100021025     C                   MOVEL     'S'           ARBACC
010200040108     C                   MOVEL     'Y'           ARBICa
010300040109     C                   Z-ADD     1             X
010400040109     C                   CLEAR                   DS3A
010500040109     C     ARBCBO        LOOKUP    C3A(X)                                 95
010600040109     C     *IN95         IFEQ      '1'
010700040109     C                   MOVEL     D3A(X)        DS3A
010800040109     C                   END
010900040109     C                   MOVEL     §3ATB1        WPORTO            1
011000040109     C     WPORTO        IFEQ      'A'
011100040109     C                   MOVEL     '1'           KTRC
011200040109     C                   ELSE
011300040109     C                   MOVEL     '2'           KTRC
011400040109     C                   END
011500040109     c     kar6          chain     fiar601l                           91
011600040109     c                   if        not *in91 and ar6dft > 0
011700040108     C                   MOVEL     'R'           ARBICa
011800040108     c                   end
011900021025     C                   MOVEL     'S'           ARBACA
012000021025     C                   MOVE      dataiso       ARBDCM
012100021025     C                   MOVEL     whhudt        ARBHMC
012200021025     C                   MOVE      *BLANK        ARBFT1
012300021025     C                   MOVE      *ZEROS        ARBDT1
012400021025     C                   MOVEL     simpou        ARBPDC
012500021025     C                   MOVE      998           ARBPDC
012600021025     C                   z-add     simpou        ARBifp
012700021025     C                   z-add     simpou        ARBipp
012800021025     C                   MOVE      *ALL'9'       ARBNDC
012900021025     C                   MOVE      dataiso       ARBDDC
013000021025     C                   MOVEL     'S'           ARBFDC
013100021025     C                   MOVE      *ALL'9'       ARBNMI
013200021025     C                   MOVE      dataiso       ARBDMI
013300021025     C                   MOVE      *ALL'9'       ARBCUC
013400021025     C                   MOVEL     *BLANK        ARBFBC
013500021025     C                   MOVEL     *BLANK        ARBCMC
013600021025     c                   move      fldcca        arbcca
013700021025      *
013800021025     C                   UPDATE    FNARB000
013900030826     C* per bolle con particolarità RC (orm contestuale alla consegna)
014000030826     c* chiudo l'eventuale orm della bolla originale se particolarità "RC"
014100030826     C                   EXSR      AGGORM
014200021025     c
014300021025     c                   endsr
014400030826     C*-----------------------------------------------------------------*
014500030826     C* AGGORM x particolarità RC aggiorno anche l'orm della bolla orig.*
014600030826     C*-----------------------------------------------------------------*
014700030826     C     aggorm        BEGSR
014800100518     c                   if        arbgma <> *blank
014801100518     C                   MOVEL     '7R'          TBLCOD
014802100518     C                   MOVEL     *BLANK        TBLKEY
014803100518     C                   MOVEL     arbGMA        TBLKEY
014804100518     C     KTAB          CHAIN     TABEL                              21
014805100518     C     *IN21         IFEQ      '0'
014806100518     C                   MOVEL     TBLUNI        DS7R
014807100518     C                   if        §7Rrc ='S'
014900030826     c                   clear                   fnlr99ds
015000030826     c                   eval      i99tla = 'L'
015100030826     c                   eval      i99aas = arbaas
015200030826     c                   eval      i99lnp = arblnp
015300030826     c                   eval      i99nrs = arbnrs
015400030826     c                   eval      i99nsp = arbnsp
015500030826     c                   eval      i99fao = 999
015600030826     c                   eval      i99car = '91'
015700071002     c                   eval      i99fgs = simpou
015800030826     c                   call      'FNLR99R'
015900030826     c                   parm                    kpjba
016000030826     c                   parm                    fnlr99ds
016001100518     c                   end
016002100518     c                   end
016100030826     c                   end
016200030826     C                   ENDSR
016300031223     C*-----------------------------------------------------------------*
016400031223     C* giacenza particolarità GS aggiorno anche la giacnza x chiuderla.*
016500031223     C*-----------------------------------------------------------------*
016600031223     C     giacenza      BEGSR
016700160105     c*                  clear                   ok                1
016800050214     c     kgia          chain     tigcp21l                           98
016900031223     c                   if        not *in98 and gcpdis = 'R'
016901171003     c                   eval      tigcpb = tigcp
017000050214     c                   eval      gcpfas = 40
017100031223     c                   eval      gcpcfg = '011'
017200050214     c                   move      dataiso       gcpdeda
017300031223     c                   move      dataiso       gcpdcg
017400050214     c                   update    tigcp000
017401171003     c                   exsr      RegVariazioni
017500160105     c*                  eval      ok = 'X'
017600031223     c                   endif
017700031223     C                   ENDSR
017701171003     C*__________________________________________________________
017702171003     c     RegVariazioni begsr
017703171003     C*__________________________________________________________
017704171003     c                   eval      ILG33CTA = 'M'
017705171003     c                   eval      ILG33PRU = knmus
017706171003     c                   eval      ILG33NOJ = knmeb
017707171003     c                   eval      ILG33PGM = 'FNLG80R1'
017709171003     c                   eval      tigcpa = tigcp
017710171214     c                   call      'FNLG33R'
017711171003     c                   parm                    fnlg33ds
017712171003     c                   parm                    tigcpb
017713171003     c                   parm                    tigcpa
017714171003     c                   endsr
017800090626      *--------------------------------------------------
017900090626     c     anncass       begsr
018000090626      *--------------------------------------------------
018100090629      * disalloco ARB prima di chiamare fnlr48 lo vuole libero
018200090629     c                   unlock    fnarb01l
018300090629      *
018400090626     C                   CLEAR                   DS48
018500090626     C                   CLEAR                   DSARBK
018600090626     C                   MOVE      arbaas        D48AAS
018700090629     C                   MOVE      arblnp        D48LNP
018800090626     C                   MOVE      arbnrs        D48NRS
018900090626     C                   MOVE      arbnsp        D48NSP
019000090626     C                   MOVE      'KB'          D48CVB
019100090626     C                   MOVE      'E'           D48FFR
019200090626     C                   MOVE      'K'           D48TRC
019300090626     C                   MOVE      'A'           D48TBO
019400090626     C                   MOVE      *BLANK        §BKCBN
019500090626     C                   MOVE      *BLANK        §BKTIC
019600090626     C                   Z-ADD     0             §BKCAS
019700090626     C                   MOVEL     DS48          KPJBU
019800090626     C                   CALL      'FNLR48R'
019900090626     C                   PARM                    KPJBA
020000090626     C                   PARM                    DSARBD
020100090626     C                   PARM                    DSARBK
020200090626     C                   PARM                    DARBT
020300090626     C                   PARM                    DSARBG
020400090626     C                   PARM                    trul90ds
020500090626     C                   MOVEL     KPJBU         DS48
020600090626      * SE NON C'È ERRORE FORZO CHE NON C'È CONTRASSEGNO
020700090626     C     D48ERR        IFne      *BLANK
020800090626     C                   END
020900090629      * rialloco arb per aggiornamento
021000090629     c     karb          chain     fnarb01l
021100090626     C                   endsr
021200021025      *--------------------------------------------------
021300021025     c     *inzsr        begsr
021400021025      *--------------------------------------------------
021500021025     c     *entry        plist
021600021025     c                   parm                    kpjba
021700021025     c                   movel     kpjbu         param
021800021025      *
021900021025     C                   Z-ADD     1             CODUT
022000021025     C                   CALL      'X§PARUT'
022100021025     C                   PARM                    UTEDSE
022200021025     C                   MOVEL     REC80         CNCR80
022300021025      * UDATE
022400021025     C                   TIME                    WHHUDT           14 0
022500021025     C                   MOVE      WHHUDT        dataeur
022600021025     C                   MOVE      dataeur       dataiso
022700021025      *
022800021025     c     karb          klist
022900021025     c                   kfld                    arbaas
023000021025     c                   kfld                    arblnp
023100021025     c                   kfld                    arbnrs
023200021025     c                   kfld                    arbnsp
023300021025     c
023400040109     c     kar6          klist
023500040109     c                   kfld                    arbaas
023600040109     c                   kfld                    arblnp
023700040109     c                   kfld                    arbnrs
023800040109     c                   kfld                    arbnsp
023900040109     c                   kfld                    ktrc
024000040109
024100031216     c     kgia          klist
024200031223     c                   kfld                    arbaas
024300031223     c                   kfld                    arblnp
024400031223     c                   kfld                    arbnrs
024500031223     c                   kfld                    arbnsp
024600031223     c                   kfld                    prog
024700031223     c                   z-add     0             prog              2 0
024800040109     C*
024801100518     C     KTAB          KLIST
024802100518     C                   KFLD                    kkut
024803100518     C                   KFLD                    TBLCOD
024804100518     C                   KFLD                    TBLKEY
024900040109     C     KTAB1         KLIST
025000040109     C                   KFLD                    KKUT
025100040109     C                   KFLD                    KCOD
025200040109     C                   Z-ADD     1             KKUT
025300040109     C     *LIKE         DEFINE    TBLKUT        KKUT
025400040109     C     *LIKE         DEFINE    TBLCOD        KCOD
025500040109     C     *LIKE         DEFINE    AR6TRC        KTRC
025600040109     C*  Caricamento Tabella 3A
025700040109     C                   Z-ADD     0             X                 3 0
025800040109     C                   MOVEL     '3A'          KCOD
025900040109     C     KTAB1         CHAIN     TABEL00F                           96
026000040109     C     *IN96         DOWEQ     '0'
026100040109     C     X             ANDLT     100
026200040109     C     TBLFLG        IFEQ      ' '
026300040109     C                   ADD       1             X
026400040109     C                   MOVEL     TBLKEY        C3A(X)
026500040109     C                   MOVEL     TBLUNI        D3A(X)
026600040109     C                   END
026700040109     C     KTAB1         READE     TABEL00F                               96
026800040109     C                   END
026900031216     c
027000021025     c                   endsr
