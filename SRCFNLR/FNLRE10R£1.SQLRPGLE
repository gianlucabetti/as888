000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200120913     H* FNLRE10R*----------------------------------------------------*
000300110704     H*         - STAMPA RIEPILOGO eventi con RS
000400000000     H*--------------------------------------------------------------*
000500110704     FFnlre10d  CF   E             WORKSTN
000600110704     FAZORG01L  IF   E           K DISK
000700120913     fFITGD02L  if   e           k disk
000800121016     fFNARB01L  if   e           k disk
000900110704     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000950125     D* SCHIERE PER CARICAMENTO L1 E L6
001100950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001200950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
001300121016     d wL6             s              3  0 dim(30)
001400920810     D KPJBA         E DS
001500091112     D ddatiute      e ds
001600110704     D FNEVBDS       e ds                  extname(FNEVB00F)
001700110704     D FiAR4DS       e ds                  extname(FiAR400F)
001800110704     D azuteds       e ds                  extname(AZUTE00F)
001900091112     D tibs34ds      E DS                  inz
002000950125     D* DS PER TRUL06R - CARICAMENTO £1
002100950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002200950125     D  LIN                    1     90  0
002300950125     D                                     DIM(30)                              SKI COMODO
002400910423     D WLBDAT          DS
002500950125     D  G02DAT                 1      8  0
002600950125     D  G02INV                 9     16  0
002700950125     D  G02ERR                17     17
002800950125     D  G02TGI                18     22  0
002900110704     d ok              s              1    inz
003000120913
003100120913       //?Campi comodo data
003200120913     d wData_EUR       s               d   datfmt(*eur)
003300120913     d wData_ISO       s               d   datfmt(*iso)
003400120913     d wTGDdcd         s                   like(TGDdcd)
003500120919
003600120919       //?Campi comodo
003700120919     d wTrat           s            132    inz(*all'-')
003800121016
003900121016       //?Flag boolenai
004000121016     d wStampa         s              1n   inz(*off)
004100120913
004200930426     C*
004300091112     C                   do        *hival
004400120913     C                   EXFMT     LRE10D01
004500110704     C* FINE LAVORO
004600091112     C   KC              leave
004700931015     C* CONTROLLI FORMATO1
004800900528     C                   EXSR      CONTR1
004900110704     c                   if        *in90 or not *inkf
005000110704     C                   iter
005100110704     c                   end
005200931116     C*
005300110704     c                   exsr      srsql
005400110704     c*
005500110704     c                   exsr      inizio
005600091112     C                   iter
005700091112     C                   enddo
005800091112     C*
005900931027     C                   SETON                                        LR
006000110704     C*****************************************************************
006100110704     c     *inzsr        begsr
006200110704     C*****************************************************************
006300110704     C     *ENTRY        PLIST
006400110704     C                   PARM                    KPJBA
006500110704     c     *dtaara       define    §azute        azuteds
006600110704     c     *dtaara       define    §datiute      ddatiute
006700110704     C                   in(E)     *dtaara
006800110704     C                   IF        %Error  or  RSUT = *blanks
006900110704     C                   call      'TIBS34R'
007000110704     C                   parm                    Tibs34Ds
007100110704     C                   in        *dtaara
007200110704     c                   ENDIF
007300931126     C* CARICO L1
007400950125     C                   CLEAR                   DSUL06
007500950125     C                   MOVE      '£1'          D06COD
007600950125     C                   MOVEL     SIMFEL        D06KEY
007700950125     C                   MOVEL     DSUL06        KPJBU
007800950125     C                   CALL      'TRUL06R'
007900950125     C                   PARM                    KPJBA
008000950125     C                   MOVEL     KPJBU         DSUL06
008100950125     C                   MOVEA     LIN           L1
008200950125     C* CARICO TABELLA FILIALI GESTITE £6
008300110704     C                   IF        dutlpo = '2' OR dutlpo = *BLANK
008400110704     C                   SETON                                        89
008500950125     C                   CLEAR                   DSUL06
008600950125     C                   MOVE      '£6'          D06COD
008700091112     C                   MOVEL     dutPOU        D06KEY
008800950125     C                   MOVEL     DSUL06        KPJBU
008900950125     C                   CALL      'TRUL06R'
009000950125     C                   PARM                    KPJBA
009100950125     C                   MOVEL     KPJBU         DSUL06
009200950125     C                   MOVEA     LIN           L6
009300110704     C                   ENDIF
009400000203     C* UDATE IN GGMMAAAA
009500110704     C                   TIME                    W0140            14 0
009600000203     C                   MOVE      W0140         WDTGIO            8 0
009700110704     C                   MOVE      Wdtgio        G02DAT
009800110704     C                   MOVEL     *BLANK        G02ERR
009900110704     C                   CALL      'XSRDA8'
010000110704     C                   PARM                    WLBDAT
010100110704     C                   Z-ADD     G02INV        oggi              8 0
010200950125     C*
010300110704     c                   exsr      inizio
010400931111     C                   ENDSR
010500110704     C**********************************************************************
010600110704     C* inizio videata
010700110704     C**********************************************************************
010800110704     C     inizio        BEGSR
010900110704     C                   SETON                                        OF
011000110704     C                   Z-ADD     wdtgio        VI1DD1
011100110704     c                   eval      vi1pf1 = dutpou
011200110704     c     vi1pf1        chain     azorg01l
011300110704     c                   if        not %found(azorg01l)
011400110704     c                   clear                   VI1DP1
011500110704     c                   else
011600110704     c                   eval      VI1DP1=orgdes
011700110704     c                   end
011800110704     c                   clear                   ok
011900121011     c                   eval      *in10 = *off
012000110704     C                   ENDSR
012100110704     C**********************************************************************
012200110704     C* CONTROLLI FORMATO1
012300110704     C**********************************************************************
012400110704     C     CONTR1        BEGSR
012500110704     C                   SETOFF                                       904353
012600110704     c                   clear                   vi1msg
012700110704     C* data inserimento evento
012800931126     C                   MOVE      VI1DD1        G02DAT
012900931126     C                   MOVEL     *BLANK        G02ERR
013000950125     C                   CALL      'XSRDA8'
013100110704     C                   PARM                    WLBDAT
013200110704     C     G02ERR        IFEQ      '1'
013300931126     C                   SETON                                        43  90
013400110704     c                   eval      vi1msg = 'Data errata'
013500091112     C                   leavesr
013600110704     C                   END
013700110704     C                   Z-ADD     G02INV        parDD1            8 0
013800950125     C                   Z-ADD     G02DAT        VI1DD1
013900110704     C     G02inv        IFgt      oggi
014000110704     C                   SETON                                        43  90
014100110704     c                   eval      vi1msg = 'Data errata'
014200110704     C                   leavesr
014300110704     C                   END
014400110704     c                   clear                   vi1dp1
014500031023     C* Se non è un utente EDPxxx controlla la filiale inserita
014600110704     c                   if        %subst(knmus: 1: 3) = 'EDP'
014700110704     c                   else
014800950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
014900110704     C     *IN89         IFEQ      *OFF
015000950125     C     VI1PF1        IFNE      SIMFEL
015100080109     C* non può essere (0) la filiale
015200080109     c                   if        VI1PF1 = 0
015300080109     c                   z-add     simfel        VI1PF1
015400080109     c                   end
015500950125     C     VI1PF1        LOOKUP    L1                                     35
015600110704     c                   if        not *in35
015700110704     C                   SETON                                        5390
015800110704     c                   eval      vi1msg = 'Filiale errata'
015900110704     C                   leavesr
016000110704     c                   end
016100950125     C                   ENDIF
016200950125     C*
016300110704     C                   ELSE
016400020701     C                   MOVEL     VI1PF1        WFIL              3 0
016500091112     C     WFIL          IFNE      dutPOU
016600950125     C     VI1PF1        LOOKUP    L6                                     35
016700950125     C  N35              SETON                                        5390
016800110704     c  n35              eval      vi1msg = 'Filiale errata'
016900091112     C  N35              leavesr
017000950125     C                   ENDIF
017100110704     C                   ENDIF
017200110704     C                   ENDIF
017300110704     c     vi1pf1        chain     azorg01l
017400110704     c                   if        not %found(azorg01l)
017500110704     c                   clear                   VI1DP1
017600110704     c                   seton                                        9053
017700110704     c                   eval      vi1msg = 'Filiale errata'
017800110704     c                   else
017900110704     c                   eval      VI1DP1=orgdes
018000110704     c                   end
018100910320     C*
018200091112     C                   ENDSR
018300110704     C**********************************************************************
018400110704     c* leggo tutti gli eventi RS ed eventualmente le note inserite
018500110704     c* dall'aut
018600110704     C**********************************************************************
018700110704     c     srsql         begsr
018800110704     C/EXEC SQL
018900110704     C+ Declare A1 Cursor for
019000110704     C+ SELECT EVBCEV, EVBAAS, EVBLNP, EVBNRS, EVBNSP, EVBFLE,
019100110704     C+ ifnull(AR4NOT, ' ') FROM fnevb00f left outer join fiar400f
019200110704     C+ on evbaas = ar4aas and evblnp = ar4lnp and evbnrs = ar4nrs and
019300110704     C+ evbnsp = ar4nsp and ar4trc = '0' WHERE
019400110704     C+ EVBCEV = 'RS' and EVBDTV = :PARDD1 AND EVBFLE = :VI1PF1
019500110704     C/END-EXEC
019600110704      *          apertura cursore
019700110704     C/EXEC SQL
019800110704     C+ OPEN A1
019900110704     C/END-EXEC
020000110704     C                   DO        *hival
020100110704      *          lettura cursore
020200110704     C/EXEC SQL
020300110704     C+ Fetch Next From A1 Into
020400110704     C+ :EVBCEV, :EVBAAS, :EVBLNP, :EVBNRS, :EVBNSP, :EVBFLE, :AR4NOT
020500110704     C/END-EXEC
020600110704     C                   SELECT
020700110704     C                   WHEN      SqlCod = 100
020800110704     C                   LEAVE
020900110704     C                   WHEN      SqlCod >= 0
021000121016     c                   exsr      Ctr_LNA
021100121016     c                   IF        wStampa
021200110704     c                   eval      ok ='1'
021300120913     c                   exsr      Aut_RS
021400110704     C                   Exsr      STAMPA
021500121016     c                   ENDIF
021600110704     C                   OTHER
021700110704     C                   ENDSL
021800110704      *
021900110704     C                   ENDDO
022000110704     C/EXEC SQL
022100110704     C+ Close A1
022200110704     C/END-EXEC
022300110704     c                   if        ok = ' '
022400110704     C                   Exsr      STAMPA
022500110704     c                   end
022600110704     C                   ENDSR
022700120913
022800120913      /free
022900121016       //---------------------------------------------------------------
023000121016       //?Controllo la LNA.
023100121016       //---------------------------------------------------------------
023200121016       BEGSR Ctr_LNA;
023300121016
023400121016         wstampa = *on;
023500121016
023600121016       //?Cerco la LNA della bolla
023700121016         chain (EVBaas:EVBlnp:EVBnrs:EVBnsp) FNARB01L;
023800121016         IF  %found(FNARB01L);
023900121016           IF  VI1pf1 = ARBlna;
024000121016             leavesr;
024100121016           ENDIF;
024200121016         ELSE;
024300121016           clear ARBlna;
024400121016         ENDIF;
024500121016
024600121016       //?Carico la £6 della filiale richiesta a video
024700121016      /end-free
024800121016     c                   clear                   DSUL06
024900121016     c                   move      '£6'          D06COD
025000121016     c                   movel     VI1pf1        D06KEY
025100121016     c                   eval      kpjbu = DSUL06
025200121016     c                   call      'TRUL06R'
025300121016     c                   parm                    KPJBA
025400121016     c                   eval      DSUL06 = kpjbu
025500121016     c                   movea     LIN           wL6
025600121016      /free
025700121016         IF  ARBlna > 0 and %lookup(ARBlna : wL6) > 0;
025800121016           leavesr;
025900121016         ENDIF;
026000121016
026100121016       //?se arrivo qua vuol dire che il rcd NON si può stampare
026200121016         wstampa = *off;
026300121016
026400121016       ENDSR;
026500121016
026600120913       //---------------------------------------------------------------
026700120913       //?Controllo se RS autorizzata
026800120913       //---------------------------------------------------------------
026900120913       BEGSR Aut_RS;
027000120913
027100120913         *in10 = *off;
027200120913         clear wTGDdcd;
027300120913
027400120913       //?Leggo il file delle R.A.
027500120913         TGDtor = 'S';
027600120913         TGDogg = %editc(EVBlnp:'X') + %editc(EVBnrs:'X') +
027700120913                  %editc(EVBnsp:'X') + %editc(EVBaas:'X');
027800120913         setll (TGDtor : TGDogg) FITGD02L;
027900121011         reade (TGDtor : TGDogg) FITGD02L;
028000120913         DOW not %eof(FITGD02L);
028100120913           IF  TGDmad = ' 50';
028200120913             *in10 = *on;
028300120913             wData_ISO = %date(TGDdcd:*ISO);
028400120913             wData_EUR = wData_ISO;
028500120913             wTGDdcd   = %dec(wData_EUR);
028600120913             leavesr;
028700120913           ENDIF;
028800121011           reade (TGDtor : TGDogg) FITGD02L;
028900120913         ENDDO;
029000120913
029100120913       ENDSR;
029200120913      /end-free
029300120913
029400110704     C**********************************************************************
029500110704     c* STAMPO
029600110704     C**********************************************************************
029700110704     c     STAMPA        begsr
029800110704     C                   IF        *INOF
029900110704     C                   EXCEPT    testata
030000110704     C                   SETOFF                                       OF
030100110704     C                   END
030200110704     c                   if        ok = '1'
030300110704     C                   EXCEPT    detta
030400110704     c                   else
030500110704     C                   EXCEPT    noncene
030600110704     c                   end
030700110704     C                   ENDSR
030800110704     C**********************************************************************
030900110704     Oqsysprt   E            testata        1 02
031000120913     o                       rsut                20
031100110704     o                                        +   3 'LISTA SPEDIZIONI CON RISER-
031200110708     o                                              VA SPECIFICA INSERITE IL'
031300110704     o                       vi1dd1        y  +   1
031400120917     o                                        +  10 'FNLRE10R'
031500120917     o                       udate         y  +  15
031600110704     O                       page          z  +   1
031700120913     O          E            testata        1
031800120913     o                       knmus            +   0
031900120917     o                                        +  30 'In arrivo a'
032000120917     o                       vi1pf1           +   1
032100110704     O          E            testata     1  1
032200110704     o                                        +   0 'Spedizione'
032300120919     o                                           24 'Note'
032400120919     o                                           79 'Autorizzazione con R.A.'
032500120919     O          E            testata        1
032600120919     o                       wtrat              132
032700110704     O          E            detta       1
032800110704     o                       evbaas           +   0
032900110704     o                       evblnp           +   1
033000110704     o                       evbnrs        z  +   1
033100110704     o                       evbnsp        z  +   1
033200110704     o                       ar4not           +   1
033300120919     o               10                       +   1 'da'
033400120913     o               10      TGDutc           +   1
033500120913     o               10                       +   1 'il'
033600120913     o               10      wTGDdcd          +   1 '  /  /    '
033700110704     O          E            noncene     3
033800110704     o                                        +  10 'N O N   E S I S T O N O   -
033900110704     o                                              S P E D I Z I O N I   -
034000110704     o                                              C O N   -
034100110704     o                                              R I S E R V A   S P E C I -
034200110704     o                                              F I C A'
