000100001003     H DECEDIT('0,') DATEDIT(*YMD.)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500050516      *       Elenco clienti per sottoconto intestazione fattura      *
000600000927      *                                                               *
000700000927      *****************************************************************
000800050516     FAZORG01L  IF   E           K DISK
000900050516     FTNTA90D   CF   E             WORKSTN
001000000927
001100051130     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001200050516     d POG             S              3    DIM(250)
001300010118
001400041115     d codut           s              1  0 Inz(1)
001500050516     d wabi            s                   like(UteAut)
001600010208
001700000927     D KPJBA         E DS
001800000927
001900000927      * Parametri Azienda e Tabelle Ricerca Conti
002000000927
002100041115     d Azuteds       e ds                  Extname(Azute00f)
002200041115     d dDatiute      e ds
002300041115     d Tibs34ds      e ds                  Inz
002400050516     d dute01        e ds
002500050516     d trul31ds      e ds
002600050516     d tnta90ds        ds
002700050516     d  v1cpop
002800050516     d  v1cabl
002900051130     d  v1cfatt
003000051130     d  v1canom
003100061002     d* ordinamento K=cliente; C=commerciale
003200061002     d  v1cord                 7      7
003300050516      *                                                               *
003400000927      *    UTILIZZO DEGLI INDICATORI                                  *
003500000927      *                                                               *
003600000927      *---------------------------------------------------------------*
003700000927      *                                                               *
003800041116      *    02 -                                                       *
003900000927      *                                                               *
004000000927      *---------------------------------------------------------------*
004100001011
004200050516     c                   Clear                   wabi
004300050516      * Verifica errori e autorità profilo
004400050516s   1c                   Select
004500050516      * se ho errori nei dati utente esco dal pgm
004600050516w   1c                   When      DutErr = 'E'
004700050516     c                   GoTo      Fine
004800050516      * se non c'è l'abilitazione
004900050516      * --> se 1° livello, abilitazioni al terminal
005000050516      *     se 2° livello, abilitazioni al punto operativo
005100050516w   1c                   When      UteAut = *Blanks
005200050516if  2c                   If        DutLpo = '1'
005300050516     c                   Eval      wabi   = 'TP'
005400050516e   2c                   EndIf
005500050516if  2c                   If        DutLpo = '2'
005600050516     c                   Eval      wabi   = 'PO'
005700050516e   2c                   EndIf
005800050516if  2c                   If        DutLpo = 'S'
005900050516     c                   Eval      wabi   = 'AZ'
006000050516e   2c                   EndIf
006100050516      * carica le abilitazioni del profilo
006200050516x   1c                   Other
006300050516     c                   Movel     UteFaf        Dute01
006400050516     c                   If        §UteCli <> *Blanks
006500050516     c                   Eval      wabi = §UteCli
006600050516     c                   Else
006700050516     c                   Eval      wabi = UteAut
006800050516     c                   EndIf
006900050516e   1c                   EndSl
007000050516
007100050516      * Reperimento dei P.O. gestibili dall'utente
007200050516     c                   clear                   TRUL31DS
007300050516     c                   eval      I31abi = wabi
007400050516     c                   eval      I31cdi = DUTdis
007500050516     c                   eval      I31car = DUTare
007600050516     c                   eval      I31cpo = DUTpou
007700050516     c                   call      'TRUL31R'
007800050516     c                   parm                    KPJBA
007900050516     c                   parm                    TRUL31DS
008000050516if  1c                   if        O31pog > *zeros
008100050516     c                   movea     O31pog        pog
008200050516x   1c                   else
008300050516     c                   goto      fine
008400050516e   1c                   endif
008500050516     c*
008600041115     c                   exsr      sr_inz01
008700041115     c     emetti        tag
008800050516     c                   exfmt     ta90d01
008900041115     c                   setoff                                       9028
009000041115     c                   clear                   vidmsg
009100041115     c     *inkc         cabeq     *on           fine
009200041115     c                   exsr      ctrd01
009300041115     c                   if        *in90 = '1' or *inkf = '0'
009400041115     c                   goto      emetti
009500041115     c                   endif
009600041115     c* sottomissione pgm batch
009700050516     c****               clear                   dpog
009800050516     c****               if        v1cpop <> 999
009900050516     c****               movel     v1cpop        dpog
010000050516     c****               else
010100050516     c****               movea     pog           dpog
010200050516     c****               endif
010300050516     c****               eval      d90abl = v1cabl
010400050516     c                   movel     tnta90ds      kpjbu
010500050516     c                   movel     'TA91'        kcoaz
010600041115     c                   call      'BCH10'
010700041115     c                   parm                    kpjba
010800041115     c     fine          tag
010900000927     C                   eval      *inlr = *on
011000000927
011100000927
011200000927      *---------------------------------------------------------------*
011300000927      *    Inizializzazione Campi Video                               *
011400000927      *---------------------------------------------------------------*
011500000927
011600041115     C     SR_INZ01      begsr
011700000927
011800041115     c* se p.o di secondo livello proteggo il p.o. a video
011900041115     c                   select
012000041115     C     dutlpo        wheneq    '2'
012100041115     C     dutlpo        oreq      *blanks
012200041115     C                   move      dutpou        v1cpop
012300041115     C     dutlpo        wheneq    '1'
012400041115     C                   move      simfel        v1cpop
012500041115     c     dutlpo        wheneq    'S'
012600041115     c                   move      *zeros        v1cpop
012700041115     C                   endsl
012800041115     c*
012900041115      *    Decodifico il P.O.
013000041115     c                   clear                   v1dpop
013100041115     c     v1cpop        ifgt      *zeros
013200041115     C     v1cpop        chain     azorg01l
013300000927     C                   if        %found(azorg01l)
013400041115     C                   movel     orgdes        v1dpop
013500000927     C                   endif
013600041115     c                   endif
013700041209     c* clienti bloccati
013800041209     c                   eval      v1cabl = 'E'
013900061002     c*
014000061002     c                   eval      v1cord = 'K'
014100051130     c*
014200051130     c                   eval      v1cfatt = 'N'
014300051130     c                   eval      v1canom = 'N'
014400000927
014500000927     C                   endsr
014600041115      *****************************************************************
014700041115      * Controllo videata
014800041115      *****************************************************************
014900041115     C     CTRD01        BEGSR
015000041115     c* spengo indicatori di errore
015100051130     c                   setoff                                       0203
015200041115     c
015300041116     c* PUNTO OPERATIVO
015400050516    1c                   if        v1cpop = *zeros
015500050516     c                   eval      vidmsg = msg(1)
015600050516     c                   seton                                        029028
015700050516     c                   goto      endc01
015800050516    1c                   endif
015900050516    1c     v1cpop        ifgt      *zeros
016000050516     c     v1cpop        andne     999
016100050516     c* deocdifico il p.o.
016200050516     c     v1cpop        chain     azorg01l
016300050516    2c                   if        %found(azorg01l)
016400050516     c                   movel     orgdes        v1dpop
016500050516     c                   else
016600050516     c                   clear                   v1dpop
016700050516    2c                   endif
016800050516     c                   move      v1cpop        w003a             3
016900050516     c     w003a         lookup    pog                                    30
017000050516    2c                   if        *in30 = *off
017100050516     c                   eval      vidmsg = msg(2)
017200050516     c                   seton                                        029028
017300050516     c                   goto      endc01
017400050516    2c                   endif
017500050516    1c                   endif
017600051130     c                   if        v1canom = 'S' and v1cfatt = 'S'
017700051130     c                   eval      vidmsg = msg(3)
017800051130     c                   seton                                        039028
017900051130     c                   goto      endc01
018000051130     c                   endif
018100041115     c     endc01        tag
018200041115     c                   endsr
018300040413      *****************************************************************
018400040413      * ROUTINE INIZIALE
018500040413      *****************************************************************
018600040413     C     *INZSR        BEGSR
018700040413
018800040413     C     *ENTRY        PLIST
018900040413     C                   PARM                    KPJBA
019000040413
019100040413      *    Recupero Ragione Sociale Ditta da Emettere a Video
019200040413      *    e Punti Operativi Gestiti
019300040413
019400041115     c     *dtaara       define    §azute        azuteds
019500041115     c     *dtaara       define    §datiute      ddatiute
019600041115     c                   in(E)     *dtaara
019700041115     c                   If        %error  or RSUT = *blanks
019800041115     c                   CLEAR                   tibs34ds
019900041115     c                   CALL      'TIBS34R'
020000041115     c                   PARM                    tibs34ds
020100041115     c                   in        *dtaara
020200041115     c                   EndIf
020300050516
020400040413     C                   endsr
020500000929
020600041124** MSG                                                                       *
020700050516Immettere il Punto Operativo                                                  01
020800050516Utente non abilitato al P.O. immesso                                          02
020900051130Non è possibile richiedere contemporaneamente entrambe le scelte       n amme
