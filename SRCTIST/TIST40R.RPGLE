000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200101115      * TIST40R *----------------------------------------------------*
000300101115      *       CONTROLLO E CANCELLAZIONE CAO OBSOLETI
000400940915      *--------------------------------------------------------------*
000500101115     FTIst40D   CF   E             WORKSTN
000600101115     Fazcpr01l  IF   E           K DISK
000700121217     FTIala01l  IF   E           K DISK
000800020805      *
000900020805      * DEFINIZIONE SCHIERE
001000960302     D*
001100050103     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001200040917     D KPJBA         E DS
001300040917     D CNCR80        E DS
001400040917     D UT§DSE0F      E DS
001500101115     D tisi00ds      E DS
001600050103     d Dataiso         s               d   datfmt(*iso)
001700050103     d Dataeur         s               d   datfmt(*eur)
001800101103     d Datasys         s               d   inz(*sys) datfmt(*iso)
001900041220     d
002000101115     d
002100101115     d TIST41DS        ds
002200101115     d  ST41VERO               1      5
002300101115     d  ST41VERR               6     10
002400101115     d  ST41VERA              11     15
002500101115     d  ST41max               16     20
002600101115     d  ST41aggio             21     21
002700101115
002800050103     C**********************************************************************
002900040917     C     *ENTRY        PLIST
003000040917     C                   PARM                    KPJBA
003100101115     c
003200050103     c*
003300040917     C                   Z-ADD     1             CODUT
003400040917     C                   CALL      'X§PARUT'
003500040917     C                   PARM                    UT§DSE0F
003600040917     C                   MOVEL     REC80         CNCR80
003700040917     C                   MOVEL     RAGUT         VIDRSU
003800101115     C                   CLEAR                   V1Cvero
003900101115     C                   CLEAR                   V1Dvero
004000101115     C                   CLEAR                   V1CverR
004100101115     C                   CLEAR                   V1DverR
004200101115     C                   CLEAR                   V1CverA
004300101115     C                   CLEAR                   V1DverA
004400101115     C                   CLEAR                   V1Cmax
004500101115     C                   CLEAR                   V1Caggio
004600121217     c* Lettura di un record di TIALA00F
004700121217     c     *loval        setll     TIala01l
004800121217     c                   read      TIala01l
004900121217     c                   if        %eof(TIala01l)
005000101115     c                   seton                                        2890
005100101115     c                   eval      v1cmsg=msg(05)
005200101115     c
005300101115     c                   else
005400101115     c                   eval      dataiso=%date(aladai)
005500101115     c                   eval      dataeur=dataiso
005600101115     c                   eval      v1dalad=%dec(dataeur)
005700101115
005800101115     c                   eval      dataiso=%date(aladaf)
005900101115     c                   eval      dataeur=dataiso
006000101115     c                   eval      v1dalaa=%dec(dataeur)
006100101115     c                   endif
006200040917     c
006300040917     C     FOR01         TAG
006400000828      *
006500101115     C                   EXFMT     st40D01
006600101115     c
006700101115     c* Se trovato errore, esco dal pgm
006800101115     c                   if        v1cmsg=msg(05)
006900101115     C                   GOTO      FINE
007000101115     c                   endif
007100101115     c
007200101115     C                   SETOFF                                       289040
007300101115     C                   SETOFF                                       4142
007400040917     C* F3 - FINE
007500040917     C   KC              GOTO      FINE
007600041220      *  CONTROLLI
007700040917     C                   EXSR      CTRD01
007800040917     c   90              goto      for01
007900000825      *
008000040917     c*
008100101115     c                   if        not *inkf
008200041220     c                   goto      for01
008300041220     c                   endif
008400040917     C*
008500040917      * F6  - SOTTOMETTO ELABORAZIONE
008600050412    1C     *INKF         IFEQ      *ON
008700101115     c                   clear                   tist41ds
008800101115     c                   if        v1cverO>*zeros
008900101115     c                   movel     v1cverO       st41verO
009000101115     c                   endif
009100101115     c                   if        v1cverR>*zeros
009200101115     c                   movel     v1cverR       st41verR
009300101115     c                   endif
009400101115     c                   if        v1cverA>*zeros
009500101115     c                   movel     v1cverA       st41verA
009600101115     c                   endif
009700101115     c                   movel     v1caggio      st41aggio
009800101115     c                   movel     v1cmax        st41max
009900101115     c                   movel     tist41ds      kpjbu
010000101115     c                   movel     'ST41'        kcoaz
010100101115     c                   call      'BCH10'
010200101115     c                   parm                    kpjba
010300101115     c
010400101115     c                   endif
010500101115     c
010600050103     c
010700000000     C     FINE          TAG
010800960129     C*
010900000000     C                   SETON                                        LR
011000900515      *-----------------------------------------------------***********
011100040917      * CONTROLLI SFL                                       *  CTRD01 *
011200900515      *-----------------------------------------------------***********
011300040917     C     CTRD01        BEGSR
011400101115     c                   setoff                                       404142
011500101115     c
011600101115     c* Versione CAP obsoleti obbligatorio
011700101115    1c                   if        v1cvero=*blanks or v1cvero=*zeros
011800101115     c                   seton                                        402890
011900101115     c                   eval      v1cmsg=msg(01)
012000101115     c                   leavesr
012100101115    1c                   endif
012200101115     c
012300101115     c                   clear                   v1dvero
012400101115    1c                   if        %scan('?':v1cvero)>0
012500101115     c                   exsr      SceltaVer
012600101115    2c                   if        d00ver>0
012700101115     c                   movel     d00ver        v1cvero
012800101115     c     d00ver        chain     azcpr01l
012900101115    3c                   if        %found(azcpr01l)
013000101115     c                   movel     cprdes        v1dvero
013100101115    3c                   endif
013200101115    2c                   endif
013300101115     c                   seton                                        9040
013400101115     c                   leavesr
013500101115    1c                   endif
013600101103     c
013700101115      *  controllo versione
013800101115     c                   if        v1cvero<>*zeros and v1cvero<>*blanks
013900101115     c                   testn                   v1cvero              30
014000101115     c                   if        not *in30 or %subst(v1cvero:5:1)<*zeros
014100101115     c                   seton                                        402890
014200101115     c                   eval      v1cmsg=msg(02)
014300101115     c                   leavesr
014400101115     c                   endif
014500101115     c                   movel     v1cvero       kver              5 0
014600101115     c     kver          chain     azcpr01l
014700101115    3c                   if        not %found(azcpr01l)
014800101115     c                   seton                                        402890
014900101115     c                   eval      v1cmsg=msg(02)
015000101115     c                   leavesr
015100101115     c                   else
015200101115     c                   movel     cprdes        v1dvero
015300101115     c                   endif
015400101115     c
015500101115     c                   endif
015600101115     c* Versione CAP di rfierimento non obbligatorio: sse non immessa verrà presa
015700101115     c*  la versione corrente
015800101115     c
015900101115     c                   clear                   v1dverR
016000101115    1c                   if        %scan('?':v1cverr)>0
016100101115     c                   exsr      SceltaVer
016200101115    2c                   if        d00ver>0
016300101115     c                   movel     d00ver        v1cverR
016400101115     c     d00ver        chain     azcpr01l
016500101115    3c                   if        %found(azcpr01l)
016600101115     c                   movel     cprdes        v1dverR
016700101115    3c                   endif
016800101115    2c                   endif
016900101115     c                   seton                                        9041
017000101115     c                   leavesr
017100101115    1c                   endif
017200101115     c
017300101115      *  controllo versione
017400101115     c                   clear                   v1dverR
017500101115     c                   if        v1cverR<>*zeros and v1cverR<>*blanks
017600101115     c                   testn                   v1cverR              30
017700101115     c                   if        not *in30 or %subst(v1cverR:5:1)<*zeros
017800101115     c                   seton                                        412890
017900101115     c                   eval      v1cmsg=msg(02)
018000101115     c                   leavesr
018100101115     c                   endif
018200101115     c                   movel     v1cverR       kver              5 0
018300101115     c     kver          chain     azcpr01l
018400101115    3c                   if        not %found(azcpr01l)
018500101115     c                   seton                                        412890
018600101115     c                   eval      v1cmsg=msg(02)
018700101115     c                   leavesr
018800101115     c                   else
018900101115     c                   movel     cprdes        v1dverR
019000101115     c                   endif
019100101115     c
019200101115     c                   endif
019300101115     c
019400101115     c* Se no aggiornamento, non imposto versione da aggiornare e spedizioni, per non
019500101115     c*  sbagliarmi
019600101115     c                   if        v1caggio='  ' and (v1cmax>0 or(
019700101115     c                             v1cverA<>*zeros  and v1cverA<>*blanks))
019800101115     c                   seton                                        422890
019900101115     c                   eval      v1cmsg=msg(03)
020000101115     c                   leavesr
020100101115     c                   endif
020200101115     c* viceversa
020300101115     c                   if        v1caggio='SI' and (v1cverA=*zeros or
020400101115     c                             v1cvera=*blanks)
020500101115     c                   seton                                        422890
020600101115     c                   eval      v1cmsg=msg(04)
020700101115     c                   leavesr
020800101115     c                   endif
020900101115     c
021000101115     c                   clear                   v1dverA
021100101115    1c                   if        %scan('?':v1cverA)>0
021200101115     c                   exsr      SceltaVer
021300101115    2c                   if        d00ver>0
021400101115     c                   movel     d00ver        v1cverA
021500101115     c     d00ver        chain     azcpr01l
021600101115    3c                   if        %found(azcpr01l)
021700101115     c                   movel     cprdes        v1dverA
021800101115    3c                   endif
021900101115    2c                   endif
022000101115     c                   seton                                        9042
022100101115     c                   leavesr
022200101115    1c                   endif
022300101115     c
022400101115      *  controllo versione
022500101115     C                   clear                   v1dverA
022600101115     c                   if        v1cverA<>*zeros and v1cverA<>*blanks
022700101115     c                   testn                   v1cverA              30
022800101115     c                   if        not *in30 or %subst(v1cverA:5:1)<*zeros
022900101115     c                   seton                                        422890
023000101115     c                   eval      v1cmsg=msg(02)
023100101115     c                   leavesr
023200101115     c                   endif
023300101115     c                   movel     v1cverA       kver              5 0
023400101115     c     kver          chain     azcpr01l
023500101115    3c                   if        not %found(azcpr01l)
023600101115     c                   seton                                        422890
023700101115     c                   eval      v1cmsg=msg(02)
023800101115     c                   leavesr
023900101115     c                   else
024000101115     c                   movel     cprdes        v1dverA
024100101115     c                   endif
024200101115     c
024300101115     c                   endif
024400101115     C                   ENDSR
024500951025     C**************************************************************************
024600101115     C*  videATA PER scelta versione cappario
024700951025     C**************************************************************************
024800101115     C     SceltaVer     BEGSR
024900101115     C* lancia il programma di gestione versioni in ricerca
025000101115     c                   CLEAR                   tisi00ds                       *pulisce ds
025100101115     c                   MOVEL     'R01'         d00op0                         *livello procedura
025200101115     c                   MOVEL     *blanks       d00op1                         *livello programma
025300101115     c                   MOVEL     '0'           d00f03                         *premuto F03
025400101115     c                   MOVEL     '0'           d00f12                         *premuto F12
025500101115     c                   MOVEL     '0'           d00err                         *errore
025600101115     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
025700101115     c                   CALL      'TISI46R'                                    *gestione versioni
025800101115     c                   PARM                    kpjba
025900101115     c                   PARM                    tisi00ds
026000101115     c
026100101115     C                   ENDSR
026200020213
026300020805      **************************************************************************
026400960302** SCHIERA MSG - MESSAGGI DI ERRORE
026500101115Versione cappario obbligatoria                                                01
026600101115Versione cappario errata                                                      02
026700101115Se NO aggiornamento,non impostare versione da aggiornare  e/o num.spedizioni  03
026800101115Impostare la versione cappario da aggiornare                                  04
026900121217File TIALA00F vuoto --> impossible elaborare il controllo e la cancellazione  05
