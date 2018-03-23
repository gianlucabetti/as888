000100001003     H DECEDIT('0,') DATEDIT(*YMD.)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500041115      *       Lista tipi invio anagrafica clienti                     *
000600000927      *                                                               *
000700000927      *****************************************************************
000800000927
000900010118     FTABEL00F  IF   E           K DISK
001000041115     FAZORG01L  IF   E           K DISK
001100041115     FTNTA71D   CF   E             WORKSTN
001200000927
001300041115     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
001400001025     D L1              S              3  0 DIM(30)
001500010118
001600041115     d codut           s              1  0 Inz(1)
001700041115     D codtab          s                   like(tblcod)
001800041115     D keytab          s                   like(tblkey)
001900010208
002000000927     D KPJBA         E DS
002100000927
002200000927      * Parametri Azienda e Tabelle Ricerca Conti
002300000927
002400041115     d Azuteds       e ds                  Extname(Azute00f)
002500041115     d dDatiute      e ds
002600041115     d Tibs34ds      e ds                  Inz
002700041115     d tnta71ds      e ds
002800000927
002900000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003000000927     D  LIN                    1     90  0
003100000927     D                                     DIM(30)
003200000927      *---------------------------------------------------------------*
003300000927      *                                                               *
003400000927      *    UTILIZZO DEGLI INDICATORI                                  *
003500000927      *                                                               *
003600000927      *---------------------------------------------------------------*
003700000927      *                                                               *
003800041116      *    02 -                                                       *
003900000927      *                                                               *
004000000927      *---------------------------------------------------------------*
004100001011
004200041115     c                   exsr      sr_inz01
004300041115     c     emetti        tag
004400041115     c                   exfmt     ta71d01
004500041115     c                   setoff                                       9028
004600041115     c                   clear                   vidmsg
004700041115     c     *inkc         cabeq     *on           fine
004800041115     c                   exsr      ctrd01
004900041115     c                   if        *in90 = '1' or *inkf = '0'
005000041115     c                   goto      emetti
005100041115     c                   endif
005200041115     c* sottomissione pgm batch
005300041115     c                   clear                   tnta71ds
005400041115     c                   eval      d71pop = v1cpop
005500041115     c                   eval      d71dpo = v1dpop
005600041115     c                   eval      d71sl0 = v1csl0
005700041115     c                   eval      d71tag = v1ctag
005800041115     c                   eval      d71ec0 = v1cec0
005900041115     c                   eval      d71an1 = v1can1
006000041115     c                   eval      d71sl1 = v1csl1
006100041115     c                   eval      d71tcg = v1ctcg
006200041115     c                   eval      d71ec1 = v1cec1
006300041115     c                   eval      d71an2 = v1can2
006400041115     c                   eval      d71sl2 = v1csl2
006500041115     c                   eval      d71tca = v1ctca
006600041115     c                   eval      d71ec2 = v1cec2
006700041115     c                   eval      d71sl3 = v1csl3
006800041115     c                   eval      d71tpd = v1ctpd
006900041115     c                   eval      d71ec3 = v1cec3
007000041116     c                   eval      d71sl4 = v1csl4
007100041115     c                   eval      d71ttx = v1cttx
007200041123     c                   movel(p)  tnta71ds      kpjbu
007300041115     c                   movel     'TA72'        kcoaz
007400041115     c                   call      'BCH10'
007500041115     c                   parm                    kpjba
007600041115     c     fine          tag
007700000927     C                   eval      *inlr = *on
007800000927
007900000927
008000000927      *---------------------------------------------------------------*
008100000927      *    Inizializzazione Campi Video                               *
008200000927      *---------------------------------------------------------------*
008300000927
008400041115     C     SR_INZ01      begsr
008500000927
008600041115     c* se p.o di secondo livello proteggo il p.o. a video
008700041115     c                   select
008800041115     C     dutlpo        wheneq    '2'
008900041115     C     dutlpo        oreq      *blanks
009000041115     C                   move      dutpou        v1cpop
009100041115     c                   seton                                        01
009200041115     C     dutlpo        wheneq    '1'
009300041115     C                   move      simfel        v1cpop
009400041115     C                   setoff                                       01
009500041115     c     dutlpo        wheneq    'S'
009600041115     c                   move      *zeros        v1cpop
009700041115     c                   setoff                                       01
009800041115     C                   endsl
009900041115     c*
010000041115      *    Decodifico il P.O.
010100041115     c                   clear                   v1dpop
010200041115     c     v1cpop        ifgt      *zeros
010300041115     C     v1cpop        chain     azorg01l
010400000927     C                   if        %found(azorg01l)
010500041115     C                   movel     orgdes        v1dpop
010600000927     C                   endif
010700041115     c                   endif
010800000927
010900000927      *    Inizializzo i Campi Restanti
011000041115     c                   clear                   v1csl0
011100041115     c                   clear                   v1csl1
011200041115     c                   clear                   v1csl2
011300041115     c                   clear                   v1csl3
011400041115     c                   clear                   v1csl4
011500041115     c                   clear                   v1ctag
011600041115     c                   clear                   v1ctcg
011700041115     c                   clear                   v1ctca
011800041115     c                   clear                   v1ctpd
011900041115     c                   clear                   v1cttx
012000041115     c                   movel     'N'           v1cec0
012100041115     c                   movel     'N'           v1cec1
012200041115     c                   movel     'N'           v1cec2
012300041115     c                   movel     'N'           v1cec3
012400041115     c                   movel     'N'           v1can1
012500041115     c                   movel     'N'           v1can2
012600041115     c                   setoff                                       020304
012700041116     c                   setoff                                       0507
012800041115     c                   setoff                                       080910
012900041115     c                   setoff                                       111213
013000041116     c                   setoff                                       1416
013100001012
013200000927
013300000927     C                   endsr
013400041115      *****************************************************************
013500041115      * Controllo videata
013600041115      *****************************************************************
013700041115     C     CTRD01        BEGSR
013800041115     c* spengo indicatori di errore
013900041115     c                   setoff                                       020304
014000041116     c                   setoff                                       0507
014100041115     c                   setoff                                       080910
014200041115     c                   setoff                                       111213
014300041116     c                   setoff                                       1416
014400041115     c
014500041116     c* PUNTO OPERATIVO
014600041115     c* controllo p.o se campo sprotetto (1° liv o sede)
014700041115    1c     *in01         ifeq      *off
014800041115     c     v1cpop        andgt     *zeros
014900041115     c     v1cpop        chain     azorg01l
015000041115    2c                   if        %found(azorg01l)
015100041115     c                   movel     orgdes        v1dpop
015200041115     c                   else
015300041115     c                   clear                   v1dpop
015400041115     c                   seton                                        289008
015500041115     c                   movel     msg(1)        vidmsg
015600041115     c                   goto      endc01
015700041115    2c                   endif
015800041115     c* se non sono sede controllo che il p.o. faccia parte della £1
015900041115    2c     dutlpo        ifne      'S'
016000041115     c     v1cpop        andne     simfel
016100041115     c     v1cpop        lookup    l1                                     30
016200041115    3c     *in30         ifeq      *off
016300041115     c                   seton                                        902808
016400041115     c                   movel     msg(2)        vidmsg
016500041115     c                   goto      endc01
016600041115    3c                   endif
016700041115    2c                   endif
016800041115    1c                   endif
016900041125     c                   If        v1cpop = *Zeros
017000041125     c                   eval      v1dpop = 'Tutti i p.o.'
017100041125     c                   EndIf
017200041116     c* TIPO COMUNICAZIONE: almeno uno deve essere stato scelto
017300041115    1c     v1csl0        ifeq      *blanks
017400041115     c     v1csl1        andeq     *blanks
017500041115     c     v1csl2        andeq     *blanks
017600041115     c     v1csl3        andeq     *blanks
017700041115     c     v1csl4        andeq     *blanks
017800041115     c                   seton                                        902809
017900041115     c                   movel     msg(3)        vidmsg
018000041115     c                   goto      endc01
018100041115    1c                   endif
018200041115     c* errore se scelto un tipo comunicazione o stampa solo eccez=S o stamp
018300041115     c* anomalie =S e non effettuata la scelta del tipo comunicazione
018400041125     c                   If        v1csl0 = *blanks and
018500041125     c                             (v1ctag <> *blanks or v1cec0 = 'S' or
018600041125     c                              v1can1 = 'S')
018700041115     c                   seton                                        902809
018800041115     c                   endif
018900041125     c                   If        v1csl1 = *blanks and
019000041125     c                             (v1ctcg <> *blanks or v1cec1 = 'S' or
019100041125     c                              V1can2 = 'S')
019200041115     c                   seton                                        902812
019300041115     c                   endif
019400041125     c                   If        v1csl2 = *blanks and
019500041125     c                             (v1ctca <> *blanks or v1cec2 = 'S')
019600041115     c                   seton                                        902813
019700041125     c                   endif
019800041125     c                   If        v1csl3 = *blanks and
019900041125     c                             (v1ctpd <> *blanks or v1cec3 = 'S')
020000041115     c                   seton                                        902814
020100041125     c                   endif
020200041125     c                   If        v1csl4 = *blanks and v1cttx <> *Blanks
020300041115     c                   seton                                        902816
020400041125     c                   endif
020500041115     c     *in90         ifeq      *on
020600041115     c                   movel     msg(7)        vidmsg
020700041115     c                   goto      endc01
020800041115     c                   endif
020900041122     c*  COMUNICAZIONE DI APERTURA GIACENZA
021000041122    1c     v1csl0        ifeq      'X'
021100041122     c     v1ctag        andeq     *blanks
021200041122     c                   seton                                        902802
021300041122     C                   movel     msg(6)        vidmsg
021400041122     C                   goto      endc01
021500041122     c                   endif
021600041124     c*  COMUNICAZIONE DI CESSATA GIACENZA
021700041122    1c     v1csl1        ifeq      'X'
021800041122     c     v1ctcg        andeq     *blanks
021900041122     c                   seton                                        902803
022000041122     C                   movel     msg(6)        vidmsg
022100041122     C                   goto      endc01
022200041122     c                   endif
022300041124     c*  AVVISO DI BONIFICO
022400041122    1c     v1csl2        ifeq      'X'
022500041122     c     v1ctca        andeq     *blanks
022600041122     c                   seton                                        902804
022700041122     C                   movel     msg(6)        vidmsg
022800041122     C                   goto      endc01
022900041122     c                   endif
023000041124    1c     v1csl2        ifeq      'X'
023100041124     c     v1ctca        andeq     'F'
023200041124     c                   seton                                        902804
023300041124     C                   movel     msg(4)        vidmsg
023400041124     C                   goto      endc01
023500041124     c                   endif
023600041124     c*  PREAVVISO DI DANNO
023700041122    1c     v1csl3        ifeq      'X'
023800041122     c     v1ctpd        andeq     *blanks
023900041122     c                   seton                                        902805
024000041122     C                   movel     msg(6)        vidmsg
024100041122     C                   goto      endc01
024200041122     c                   endif
024300041124     c*  TESTI LETTERA
024400041122    1c     v1csl4        ifeq      'X'
024500041123     c     v1cttx        andeq     *blanks
024600041122     c                   seton                                        902807
024700041122     C                   movel     msg(6)        vidmsg
024800041122     C                   goto      endc01
024900041122     c                   endif
025000041115     c     endc01        tag
025100041115     c                   endsr
025200040413      *****************************************************************
025300040413      * ROUTINE INIZIALE
025400040413      *****************************************************************
025500040413     C     *INZSR        BEGSR
025600040413
025700040413     C     *ENTRY        PLIST
025800040413     C                   PARM                    KPJBA
025900040413
026000040413
026100040413      *    Accesso a TABEL
026200040413
026300041115     C     Ktabel        klist
026400040413     C                   kfld                    codut
026500041115     C                   kfld                    codtab
026600041115     C                   kfld                    keytab
026700040413
026800040413      *    Recupero Ragione Sociale Ditta da Emettere a Video
026900040413      *    e Punti Operativi Gestiti
027000040413
027100041115     c     *dtaara       define    §azute        azuteds
027200041115     c     *dtaara       define    §datiute      ddatiute
027300041115     c                   in(E)     *dtaara
027400041115     c                   If        %error  or RSUT = *blanks
027500041115     c                   CLEAR                   tibs34ds
027600041115     c                   CALL      'TIBS34R'
027700041115     c                   PARM                    tibs34ds
027800041115     c                   in        *dtaara
027900041115     c                   EndIf
028000041115     c*
028100040413     C                   CLEAR                   DSUL06
028200040413     C                   MOVE      '£1'          D06COD
028300040413     C                   MOVEL     SIMFEL        D06KEY
028400040413     C                   MOVEL     DSUL06        KPJBU
028500040413     C                   CALL      'TRUL06R'
028600040413     C                   PARM                    KPJBA
028700040413     C                   MOVEL     KPJBU         DSUL06
028800040413     C                   MOVEA     LIN           L1
028900040413
029000040413     C                   endsr
029100000929
029200041124** MSG                                                                       *
029300041124Punto operativo inesistente                                                   01
029400041124Punto operativo in gestione ad altro p.o.                                     02
029500041124Scegliere almeno un tipo comunicazione                                        03
029600041124Per l'avviso di bonifico tipo invio "Fax" non previsto                        04
029700041124                                                                              05
029800041124Tipo invio obbligatorio                                                       06
029900041124Immettere 'X' per selezionare il tipo comunicazione richiesto                 07
