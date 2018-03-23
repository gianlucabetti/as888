000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200101115      * TIST40R *----------------------------------------------------*
000300101115      *       CONTROLLO E CANCELLAZIONE CAO OBSOLETI
000400940915      *--------------------------------------------------------------*
000500101115     FTIst40D   CF   E             WORKSTN
000600101115     Fazcpr01l  IF   E           K DISK
000700101115     Fwfala01l  IF   E           K DISK    USROPN extfile(Wwfala)
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
002800101115     d Wwfala          s             21    inz('GAITRAAZM /WFALA01L')
002900050103     C**********************************************************************
003000040917     C     *ENTRY        PLIST
003100040917     C                   PARM                    KPJBA
003200101115     c
003300101115     c                   open(e)   wfala01l
003400101115     c                   if        not %open(wfala01l)
003500101115     c                   eval      %subst(Wwfala:7:4)='AZP '
003600101115     C                   OPEN      wfala01l
003700101115     c                   endif
003800101115     c
003900050103     c*
004000040917     C                   Z-ADD     1             CODUT
004100040917     C                   CALL      'X§PARUT'
004200040917     C                   PARM                    UT§DSE0F
004300040917     C                   MOVEL     REC80         CNCR80
004400040917     C                   MOVEL     RAGUT         VIDRSU
004500101115     C                   CLEAR                   V1Cvero
004600101115     C                   CLEAR                   V1Dvero
004700101115     C                   CLEAR                   V1CverR
004800101115     C                   CLEAR                   V1DverR
004900101115     C                   CLEAR                   V1CverA
005000101115     C                   CLEAR                   V1DverA
005100101115     C                   CLEAR                   V1Cmax
005200101115     C                   CLEAR                   V1Caggio
005300101115     c* Lettura di un record di WFALA00F
005400101115     c     *loval        setll     wfala01l
005500101115     c                   read      wfala01l
005600101115     c                   if        %eof(wfala01l)
005700101115     c                   seton                                        2890
005800101115     c                   eval      v1cmsg=msg(05)
005900101115     c
006000101115     c                   else
006100101115     c                   eval      dataiso=%date(aladai)
006200101115     c                   eval      dataeur=dataiso
006300101115     c                   eval      v1dalad=%dec(dataeur)
006400101115
006500101115     c                   eval      dataiso=%date(aladaf)
006600101115     c                   eval      dataeur=dataiso
006700101115     c                   eval      v1dalaa=%dec(dataeur)
006800101115     c                   endif
006900040917     c
007000040917     C     FOR01         TAG
007100000828      *
007200101115     C                   EXFMT     st40D01
007300101115     c
007400101115     c* Se trovato errore, esco dal pgm
007500101115     c                   if        v1cmsg=msg(05)
007600101115     C                   GOTO      FINE
007700101115     c                   endif
007800101115     c
007900101115     C                   SETOFF                                       289040
008000101115     C                   SETOFF                                       4142
008100040917     C* F3 - FINE
008200040917     C   KC              GOTO      FINE
008300041220      *  CONTROLLI
008400040917     C                   EXSR      CTRD01
008500040917     c   90              goto      for01
008600000825      *
008700040917     c*
008800101115     c                   if        not *inkf
008900041220     c                   goto      for01
009000041220     c                   endif
009100040917     C*
009200040917      * F6  - SOTTOMETTO ELABORAZIONE
009300050412    1C     *INKF         IFEQ      *ON
009400101115     c                   clear                   tist41ds
009500101115     c                   if        v1cverO>*zeros
009600101115     c                   movel     v1cverO       st41verO
009700101115     c                   endif
009800101115     c                   if        v1cverR>*zeros
009900101115     c                   movel     v1cverR       st41verR
010000101115     c                   endif
010100101115     c                   if        v1cverA>*zeros
010200101115     c                   movel     v1cverA       st41verA
010300101115     c                   endif
010400101115     c                   movel     v1caggio      st41aggio
010500101115     c                   movel     v1cmax        st41max
010600101115     c                   movel     tist41ds      kpjbu
010700101115     c                   movel     'ST41'        kcoaz
010800101115     c                   call      'BCH10'
010900101115     c                   parm                    kpjba
011000101115     c
011100101115     c                   endif
011200101115     c
011300050103     c
011400000000     C     FINE          TAG
011500960129     C*
011600000000     C                   SETON                                        LR
011700900515      *-----------------------------------------------------***********
011800040917      * CONTROLLI SFL                                       *  CTRD01 *
011900900515      *-----------------------------------------------------***********
012000040917     C     CTRD01        BEGSR
012100101115     c                   setoff                                       404142
012200101115     c
012300101115     c* Versione CAP obsoleti obbligatorio
012400101115    1c                   if        v1cvero=*blanks or v1cvero=*zeros
012500101115     c                   seton                                        402890
012600101115     c                   eval      v1cmsg=msg(01)
012700101115     c                   leavesr
012800101115    1c                   endif
012900101115     c
013000101115     c                   clear                   v1dvero
013100101115    1c                   if        %scan('?':v1cvero)>0
013200101115     c                   exsr      SceltaVer
013300101115    2c                   if        d00ver>0
013400101115     c                   movel     d00ver        v1cvero
013500101115     c     d00ver        chain     azcpr01l
013600101115    3c                   if        %found(azcpr01l)
013700101115     c                   movel     cprdes        v1dvero
013800101115    3c                   endif
013900101115    2c                   endif
014000101115     c                   seton                                        9040
014100101115     c                   leavesr
014200101115    1c                   endif
014300101103     c
014400101115      *  controllo versione
014500101115     c                   if        v1cvero<>*zeros and v1cvero<>*blanks
014600101115     c                   testn                   v1cvero              30
014700101115     c                   if        not *in30 or %subst(v1cvero:5:1)<*zeros
014800101115     c                   seton                                        402890
014900101115     c                   eval      v1cmsg=msg(02)
015000101115     c                   leavesr
015100101115     c                   endif
015200101115     c                   movel     v1cvero       kver              5 0
015300101115     c     kver          chain     azcpr01l
015400101115    3c                   if        not %found(azcpr01l)
015500101115     c                   seton                                        402890
015600101115     c                   eval      v1cmsg=msg(02)
015700101115     c                   leavesr
015800101115     c                   else
015900101115     c                   movel     cprdes        v1dvero
016000101115     c                   endif
016100101115     c
016200101115     c                   endif
016300101115     c* Versione CAP di rfierimento non obbligatorio: sse non immessa verrà presa
016400101115     c*  la versione corrente
016500101115     c
016600101115     c                   clear                   v1dverR
016700101115    1c                   if        %scan('?':v1cverr)>0
016800101115     c                   exsr      SceltaVer
016900101115    2c                   if        d00ver>0
017000101115     c                   movel     d00ver        v1cverR
017100101115     c     d00ver        chain     azcpr01l
017200101115    3c                   if        %found(azcpr01l)
017300101115     c                   movel     cprdes        v1dverR
017400101115    3c                   endif
017500101115    2c                   endif
017600101115     c                   seton                                        9041
017700101115     c                   leavesr
017800101115    1c                   endif
017900101115     c
018000101115      *  controllo versione
018100101115     c                   clear                   v1dverR
018200101115     c                   if        v1cverR<>*zeros and v1cverR<>*blanks
018300101115     c                   testn                   v1cverR              30
018400101115     c                   if        not *in30 or %subst(v1cverR:5:1)<*zeros
018500101115     c                   seton                                        412890
018600101115     c                   eval      v1cmsg=msg(02)
018700101115     c                   leavesr
018800101115     c                   endif
018900101115     c                   movel     v1cverR       kver              5 0
019000101115     c     kver          chain     azcpr01l
019100101115    3c                   if        not %found(azcpr01l)
019200101115     c                   seton                                        412890
019300101115     c                   eval      v1cmsg=msg(02)
019400101115     c                   leavesr
019500101115     c                   else
019600101115     c                   movel     cprdes        v1dverR
019700101115     c                   endif
019800101115     c
019900101115     c                   endif
020000101115     c
020100101115     c* Se no aggiornamento, non imposto versione da aggiornare e spedizioni, per non
020200101115     c*  sbagliarmi
020300101115     c                   if        v1caggio='  ' and (v1cmax>0 or(
020400101115     c                             v1cverA<>*zeros  and v1cverA<>*blanks))
020500101115     c                   seton                                        422890
020600101115     c                   eval      v1cmsg=msg(03)
020700101115     c                   leavesr
020800101115     c                   endif
020900101115     c* viceversa
021000101115     c                   if        v1caggio='SI' and (v1cverA=*zeros or
021100101115     c                             v1cvera=*blanks)
021200101115     c                   seton                                        422890
021300101115     c                   eval      v1cmsg=msg(04)
021400101115     c                   leavesr
021500101115     c                   endif
021600101115     c
021700101115     c                   clear                   v1dverA
021800101115    1c                   if        %scan('?':v1cverA)>0
021900101115     c                   exsr      SceltaVer
022000101115    2c                   if        d00ver>0
022100101115     c                   movel     d00ver        v1cverA
022200101115     c     d00ver        chain     azcpr01l
022300101115    3c                   if        %found(azcpr01l)
022400101115     c                   movel     cprdes        v1dverA
022500101115    3c                   endif
022600101115    2c                   endif
022700101115     c                   seton                                        9042
022800101115     c                   leavesr
022900101115    1c                   endif
023000101115     c
023100101115      *  controllo versione
023200101115     C                   clear                   v1dverA
023300101115     c                   if        v1cverA<>*zeros and v1cverA<>*blanks
023400101115     c                   testn                   v1cverA              30
023500101115     c                   if        not *in30 or %subst(v1cverA:5:1)<*zeros
023600101115     c                   seton                                        422890
023700101115     c                   eval      v1cmsg=msg(02)
023800101115     c                   leavesr
023900101115     c                   endif
024000101115     c                   movel     v1cverA       kver              5 0
024100101115     c     kver          chain     azcpr01l
024200101115    3c                   if        not %found(azcpr01l)
024300101115     c                   seton                                        422890
024400101115     c                   eval      v1cmsg=msg(02)
024500101115     c                   leavesr
024600101115     c                   else
024700101115     c                   movel     cprdes        v1dverA
024800101115     c                   endif
024900101115     c
025000101115     c                   endif
025100101115     C                   ENDSR
025200951025     C**************************************************************************
025300101115     C*  videATA PER scelta versione cappario
025400951025     C**************************************************************************
025500101115     C     SceltaVer     BEGSR
025600101115     C* lancia il programma di gestione versioni in ricerca
025700101115     c                   CLEAR                   tisi00ds                       *pulisce ds
025800101115     c                   MOVEL     'R01'         d00op0                         *livello procedura
025900101115     c                   MOVEL     *blanks       d00op1                         *livello programma
026000101115     c                   MOVEL     '0'           d00f03                         *premuto F03
026100101115     c                   MOVEL     '0'           d00f12                         *premuto F12
026200101115     c                   MOVEL     '0'           d00err                         *errore
026300101115     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
026400101115     c                   CALL      'TISI46R'                                    *gestione versioni
026500101115     c                   PARM                    kpjba
026600101115     c                   PARM                    tisi00ds
026700101115     c
026800101115     C                   ENDSR
026900020213
027000020805      **************************************************************************
027100960302** SCHIERA MSG - MESSAGGI DI ERRORE
027200101115Versione cappario obbligatoria                                                01
027300101115Versione cappario errata                                                      02
027400101115Se NO aggiornamento,non impostare versione da aggiornare  e/o num.spedizioni  03
027500101115Impostare la versione cappario da aggiornare                                  04
027600101115File WFALA00F vuoto --> impossible elaborare il controllo e la cancellazione  05
