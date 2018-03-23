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
007200041209     c                   eval      d71abl = v1cabl
007300041123     c                   movel(p)  tnta71ds      kpjbu
007400041115     c                   movel     'TA72'        kcoaz
007500041115     c                   call      'BCH10'
007600041115     c                   parm                    kpjba
007700041115     c     fine          tag
007800000927     C                   eval      *inlr = *on
007900000927
008000000927
008100000927      *---------------------------------------------------------------*
008200000927      *    Inizializzazione Campi Video                               *
008300000927      *---------------------------------------------------------------*
008400000927
008500041115     C     SR_INZ01      begsr
008600000927
008700041115     c* se p.o di secondo livello proteggo il p.o. a video
008800041115     c                   select
008900041115     C     dutlpo        wheneq    '2'
009000041115     C     dutlpo        oreq      *blanks
009100041115     C                   move      dutpou        v1cpop
009200041115     c                   seton                                        01
009300041115     C     dutlpo        wheneq    '1'
009400041115     C                   move      simfel        v1cpop
009500041115     C                   setoff                                       01
009600041115     c     dutlpo        wheneq    'S'
009700041115     c                   move      *zeros        v1cpop
009800041115     c                   setoff                                       01
009900041115     C                   endsl
010000041115     c*
010100041115      *    Decodifico il P.O.
010200041115     c                   clear                   v1dpop
010300041115     c     v1cpop        ifgt      *zeros
010400041115     C     v1cpop        chain     azorg01l
010500000927     C                   if        %found(azorg01l)
010600041115     C                   movel     orgdes        v1dpop
010700000927     C                   endif
010800041115     c                   endif
010900041209     c* clienti bloccati
011000041209     c                   eval      v1cabl = 'E'
011100000927
011200000927      *    Inizializzo i Campi Restanti
011300041115     c                   clear                   v1csl0
011400041115     c                   clear                   v1csl1
011500041115     c                   clear                   v1csl2
011600041115     c                   clear                   v1csl3
011700041115     c                   clear                   v1csl4
011800041115     c                   clear                   v1ctag
011900041115     c                   clear                   v1ctcg
012000041115     c                   clear                   v1ctca
012100041115     c                   clear                   v1ctpd
012200041115     c                   clear                   v1cttx
012300041115     c                   movel     'N'           v1cec0
012400041115     c                   movel     'N'           v1cec1
012500041115     c                   movel     'N'           v1cec2
012600041115     c                   movel     'N'           v1cec3
012700041115     c                   movel     'N'           v1can1
012800041115     c                   movel     'N'           v1can2
012900041115     c                   setoff                                       020304
013000041116     c                   setoff                                       0507
013100041115     c                   setoff                                       080910
013200041115     c                   setoff                                       111213
013300041116     c                   setoff                                       1416
013400001012
013500000927
013600000927     C                   endsr
013700041115      *****************************************************************
013800041115      * Controllo videata
013900041115      *****************************************************************
014000041115     C     CTRD01        BEGSR
014100041115     c* spengo indicatori di errore
014200041115     c                   setoff                                       020304
014300041116     c                   setoff                                       0507
014400041115     c                   setoff                                       080910
014500041115     c                   setoff                                       111213
014600041116     c                   setoff                                       1416
014700041115     c
014800041116     c* PUNTO OPERATIVO
014900041115     c* controllo p.o se campo sprotetto (1° liv o sede)
015000041115    1c     *in01         ifeq      *off
015100041115     c     v1cpop        andgt     *zeros
015200041115     c     v1cpop        chain     azorg01l
015300041115    2c                   if        %found(azorg01l)
015400041115     c                   movel     orgdes        v1dpop
015500041115     c                   else
015600041115     c                   clear                   v1dpop
015700041115     c                   seton                                        289008
015800041115     c                   movel     msg(1)        vidmsg
015900041115     c                   goto      endc01
016000041115    2c                   endif
016100041115     c* se non sono sede controllo che il p.o. faccia parte della £1
016200041115    2c     dutlpo        ifne      'S'
016300041115     c     v1cpop        andne     simfel
016400041115     c     v1cpop        lookup    l1                                     30
016500041115    3c     *in30         ifeq      *off
016600041115     c                   seton                                        902808
016700041115     c                   movel     msg(2)        vidmsg
016800041115     c                   goto      endc01
016900041115    3c                   endif
017000041115    2c                   endif
017100041115    1c                   endif
017200041125     c                   If        v1cpop = *Zeros
017300041125     c                   eval      v1dpop = 'Tutti i p.o.'
017400041125     c                   EndIf
017500041116     c* TIPO COMUNICAZIONE: almeno uno deve essere stato scelto
017600041115    1c     v1csl0        ifeq      *blanks
017700041115     c     v1csl1        andeq     *blanks
017800041115     c     v1csl2        andeq     *blanks
017900041115     c     v1csl3        andeq     *blanks
018000041115     c     v1csl4        andeq     *blanks
018100041115     c                   seton                                        902809
018200041115     c                   movel     msg(3)        vidmsg
018300041115     c                   goto      endc01
018400041115    1c                   endif
018500041115     c* errore se scelto un tipo comunicazione o stampa solo eccez=S o stamp
018600041115     c* anomalie =S e non effettuata la scelta del tipo comunicazione
018700041125     c                   If        v1csl0 = *blanks and
018800041125     c                             (v1ctag <> *blanks or v1cec0 = 'S' or
018900041125     c                              v1can1 = 'S')
019000041115     c                   seton                                        902809
019100041115     c                   endif
019200041125     c                   If        v1csl1 = *blanks and
019300041125     c                             (v1ctcg <> *blanks or v1cec1 = 'S' or
019400041125     c                              V1can2 = 'S')
019500041115     c                   seton                                        902812
019600041115     c                   endif
019700041125     c                   If        v1csl2 = *blanks and
019800041125     c                             (v1ctca <> *blanks or v1cec2 = 'S')
019900041115     c                   seton                                        902813
020000041125     c                   endif
020100041125     c                   If        v1csl3 = *blanks and
020200041125     c                             (v1ctpd <> *blanks or v1cec3 = 'S')
020300041115     c                   seton                                        902814
020400041125     c                   endif
020500041125     c                   If        v1csl4 = *blanks and v1cttx <> *Blanks
020600041115     c                   seton                                        902816
020700041125     c                   endif
020800041115     c     *in90         ifeq      *on
020900041115     c                   movel     msg(7)        vidmsg
021000041115     c                   goto      endc01
021100041115     c                   endif
021200041122     c*  COMUNICAZIONE DI APERTURA GIACENZA
021300041122    1c     v1csl0        ifeq      'X'
021400041122     c     v1ctag        andeq     *blanks
021500041122     c                   seton                                        902802
021600041122     C                   movel     msg(6)        vidmsg
021700041122     C                   goto      endc01
021800041122     c                   endif
021900041124     c*  COMUNICAZIONE DI CESSATA GIACENZA
022000041122    1c     v1csl1        ifeq      'X'
022100041122     c     v1ctcg        andeq     *blanks
022200041122     c                   seton                                        902803
022300041122     C                   movel     msg(6)        vidmsg
022400041122     C                   goto      endc01
022500041122     c                   endif
022600041124     c*  AVVISO DI BONIFICO
022700041122    1c     v1csl2        ifeq      'X'
022800041122     c     v1ctca        andeq     *blanks
022900041122     c                   seton                                        902804
023000041122     C                   movel     msg(6)        vidmsg
023100041122     C                   goto      endc01
023200041122     c                   endif
023300041124    1c     v1csl2        ifeq      'X'
023400041124     c     v1ctca        andeq     'F'
023500041124     c                   seton                                        902804
023600041124     C                   movel     msg(4)        vidmsg
023700041124     C                   goto      endc01
023800041124     c                   endif
023900041124     c*  PREAVVISO DI DANNO
024000041122    1c     v1csl3        ifeq      'X'
024100041122     c     v1ctpd        andeq     *blanks
024200041122     c                   seton                                        902805
024300041122     C                   movel     msg(6)        vidmsg
024400041122     C                   goto      endc01
024500041122     c                   endif
024600041124     c*  TESTI LETTERA
024700041122    1c     v1csl4        ifeq      'X'
024800041123     c     v1cttx        andeq     *blanks
024900041122     c                   seton                                        902807
025000041122     C                   movel     msg(6)        vidmsg
025100041122     C                   goto      endc01
025200041122     c                   endif
025300041115     c     endc01        tag
025400041115     c                   endsr
025500040413      *****************************************************************
025600040413      * ROUTINE INIZIALE
025700040413      *****************************************************************
025800040413     C     *INZSR        BEGSR
025900040413
026000040413     C     *ENTRY        PLIST
026100040413     C                   PARM                    KPJBA
026200040413
026300040413
026400040413      *    Accesso a TABEL
026500040413
026600041115     C     Ktabel        klist
026700040413     C                   kfld                    codut
026800041115     C                   kfld                    codtab
026900041115     C                   kfld                    keytab
027000040413
027100040413      *    Recupero Ragione Sociale Ditta da Emettere a Video
027200040413      *    e Punti Operativi Gestiti
027300040413
027400041115     c     *dtaara       define    §azute        azuteds
027500041115     c     *dtaara       define    §datiute      ddatiute
027600041115     c                   in(E)     *dtaara
027700041115     c                   If        %error  or RSUT = *blanks
027800041115     c                   CLEAR                   tibs34ds
027900041115     c                   CALL      'TIBS34R'
028000041115     c                   PARM                    tibs34ds
028100041115     c                   in        *dtaara
028200041115     c                   EndIf
028300041115     c*
028400040413     C                   CLEAR                   DSUL06
028500040413     C                   MOVE      '£1'          D06COD
028600040413     C                   MOVEL     SIMFEL        D06KEY
028700040413     C                   MOVEL     DSUL06        KPJBU
028800040413     C                   CALL      'TRUL06R'
028900040413     C                   PARM                    KPJBA
029000040413     C                   MOVEL     KPJBU         DSUL06
029100040413     C                   MOVEA     LIN           L1
029200040413
029300040413     C                   endsr
029400000929
029500041124** MSG                                                                       *
029600041124Punto operativo inesistente                                                   01
029700041124Punto operativo in gestione ad altro p.o.                                     02
029800041124Scegliere almeno un tipo comunicazione                                        03
029900041124Per l'avviso di bonifico tipo invio "Fax" non previsto                        04
030000041124                                                                              05
030100041124Tipo invio obbligatorio                                                       06
030200041124Immettere 'X' per selezionare il tipo comunicazione richiesto                 07
