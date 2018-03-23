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
001300151014     D LJ              S              3  0 DIM(30)
001400121016     d wL6             s              3  0 dim(30)
001500920810     D KPJBA         E DS
001600091112     D ddatiute      e ds
001700110704     D FNEVBDS       e ds                  extname(FNEVB00F)
001800110704     D FiAR4DS       e ds                  extname(FiAR400F)
001900110704     D azuteds       e ds                  extname(AZUTE00F)
002000091112     D tibs34ds      E DS                  inz
002100950125     D* DS PER TRUL06R - CARICAMENTO £1
002200950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002300950125     D  LIN                    1     90  0
002400950125     D                                     DIM(30)                              SKI COMODO
002500910423     D WLBDAT          DS
002600950125     D  G02DAT                 1      8  0
002700950125     D  G02INV                 9     16  0
002800950125     D  G02ERR                17     17
002900950125     D  G02TGI                18     22  0
003000110704     d ok              s              1    inz
003100120913
003200120913       //?Campi comodo data
003300120913     d wData_EUR       s               d   datfmt(*eur)
003400120913     d wData_ISO       s               d   datfmt(*iso)
003500120913     d wTGDdcd         s                   like(TGDdcd)
003600120919
003700120919       //?Campi comodo
003800120919     d wTrat           s            132    inz(*all'-')
003900121016
004000121016       //?Flag boolenai
004100121016     d wStampa         s              1n   inz(*off)
004200120913
004300930426     C*
004400091112     C                   do        *hival
004500120913     C                   EXFMT     LRE10D01
004600110704     C* FINE LAVORO
004700091112     C   KC              leave
004800931015     C* CONTROLLI FORMATO1
004900900528     C                   EXSR      CONTR1
005000110704     c                   if        *in90 or not *inkf
005100110704     C                   iter
005200110704     c                   end
005300931116     C*
005400110704     c                   exsr      srsql
005500110704     c*
005600110704     c                   exsr      inizio
005700091112     C                   iter
005800091112     C                   enddo
005900091112     C*
006000931027     C                   SETON                                        LR
006100110704     C*****************************************************************
006200110704     c     *inzsr        begsr
006300110704     C*****************************************************************
006400110704     C     *ENTRY        PLIST
006500110704     C                   PARM                    KPJBA
006600110704     c     *dtaara       define    §azute        azuteds
006700110704     c     *dtaara       define    §datiute      ddatiute
006800110704     C                   in(E)     *dtaara
006900110704     C                   IF        %Error  or  RSUT = *blanks
007000110704     C                   call      'TIBS34R'
007100110704     C                   parm                    Tibs34Ds
007200110704     C                   in        *dtaara
007300110704     c                   ENDIF
007400931126     C* CARICO L1
007500950125     C                   CLEAR                   DSUL06
007600950125     C                   MOVE      '£1'          D06COD
007700950125     C                   MOVEL     SIMFEL        D06KEY
007800950125     C                   MOVEL     DSUL06        KPJBU
007900950125     C                   CALL      'TRUL06R'
008000950125     C                   PARM                    KPJBA
008100950125     C                   MOVEL     KPJBU         DSUL06
008200950125     C                   MOVEA     LIN           L1
008300151014     C* CARICO TABELLA FILIALI legate 'J'
008400151014     C                   CLEAR                   DSUL06
008500151014     C                   MOVE      '£J'          D06COD
008600151014     C                   MOVEL     dutPOU        D06KEY
008700151014     C                   MOVEL     DSUL06        KPJBU
008800151014     C                   CALL      'TRUL06R'
008900151014     C                   PARM                    KPJBA
009000151014     C                   MOVEL     KPJBU         DSUL06
009100151014     C                   MOVEA     LIN           LJ
009200950125     C* CARICO TABELLA FILIALI GESTITE £6
009300110704     C                   IF        dutlpo = '2' OR dutlpo = *BLANK
009400110704     C                   SETON                                        89
009500950125     C                   CLEAR                   DSUL06
009600950125     C                   MOVE      '£6'          D06COD
009700091112     C                   MOVEL     dutPOU        D06KEY
009800950125     C                   MOVEL     DSUL06        KPJBU
009900950125     C                   CALL      'TRUL06R'
010000950125     C                   PARM                    KPJBA
010100950125     C                   MOVEL     KPJBU         DSUL06
010200950125     C                   MOVEA     LIN           L6
010300110704     C                   ENDIF
010400000203     C* UDATE IN GGMMAAAA
010500110704     C                   TIME                    W0140            14 0
010600000203     C                   MOVE      W0140         WDTGIO            8 0
010700110704     C                   MOVE      Wdtgio        G02DAT
010800110704     C                   MOVEL     *BLANK        G02ERR
010900110704     C                   CALL      'XSRDA8'
011000110704     C                   PARM                    WLBDAT
011100110704     C                   Z-ADD     G02INV        oggi              8 0
011200950125     C*
011300110704     c                   exsr      inizio
011400931111     C                   ENDSR
011500110704     C**********************************************************************
011600110704     C* inizio videata
011700110704     C**********************************************************************
011800110704     C     inizio        BEGSR
011900110704     C                   SETON                                        OF
012000110704     C                   Z-ADD     wdtgio        VI1DD1
012100110704     c                   eval      vi1pf1 = dutpou
012200110704     c     vi1pf1        chain     azorg01l
012300110704     c                   if        not %found(azorg01l)
012400110704     c                   clear                   VI1DP1
012500110704     c                   else
012600110704     c                   eval      VI1DP1=orgdes
012700110704     c                   end
012800110704     c                   clear                   ok
012900121011     c                   eval      *in10 = *off
013000110704     C                   ENDSR
013100110704     C**********************************************************************
013200110704     C* CONTROLLI FORMATO1
013300110704     C**********************************************************************
013400110704     C     CONTR1        BEGSR
013500110704     C                   SETOFF                                       904353
013600110704     c                   clear                   vi1msg
013700110704     C* data inserimento evento
013800931126     C                   MOVE      VI1DD1        G02DAT
013900931126     C                   MOVEL     *BLANK        G02ERR
014000950125     C                   CALL      'XSRDA8'
014100110704     C                   PARM                    WLBDAT
014200110704     C     G02ERR        IFEQ      '1'
014300931126     C                   SETON                                        43  90
014400110704     c                   eval      vi1msg = 'Data errata'
014500091112     C                   leavesr
014600110704     C                   END
014700110704     C                   Z-ADD     G02INV        parDD1            8 0
014800950125     C                   Z-ADD     G02DAT        VI1DD1
014900110704     C     G02inv        IFgt      oggi
015000110704     C                   SETON                                        43  90
015100110704     c                   eval      vi1msg = 'Data errata'
015200110704     C                   leavesr
015300110704     C                   END
015400110704     c                   clear                   vi1dp1
015500031023     C* Se non è un utente EDPxxx controlla la filiale inserita
015600110704     c                   if        %subst(knmus: 1: 3) = 'EDP'
015700110704     c                   else
015800950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
015900110704     C     *IN89         IFEQ      *OFF
016000950125     C     VI1PF1        IFNE      SIMFEL
016100080109     C* non può essere (0) la filiale
016200080109     c                   if        VI1PF1 = 0
016300080109     c                   z-add     simfel        VI1PF1
016400080109     c                   end
016500950125     C     VI1PF1        LOOKUP    L1                                     35
016600110704     c                   if        not *in35
016700110704     C                   SETON                                        5390
016800110704     c                   eval      vi1msg = 'Filiale errata'
016900110704     C                   leavesr
017000110704     c                   end
017100950125     C                   ENDIF
017200950125     C*
017300110704     C                   ELSE
017400020701     C                   MOVEL     VI1PF1        WFIL              3 0
017500091112     C     WFIL          IFNE      dutPOU
017600950125     C     VI1PF1        LOOKUP    L6                                     35
017700950125     C  N35              SETON                                        5390
017800110704     c  n35              eval      vi1msg = 'Filiale errata'
017900091112     C  N35              leavesr
018000950125     C                   ENDIF
018100110704     C                   ENDIF
018200110704     C                   ENDIF
018300110704     c     vi1pf1        chain     azorg01l
018400110704     c                   if        not %found(azorg01l)
018500110704     c                   clear                   VI1DP1
018600110704     c                   seton                                        9053
018700110704     c                   eval      vi1msg = 'Filiale errata'
018800110704     c                   else
018900110704     c                   eval      VI1DP1=orgdes
019000110704     c                   end
019100910320     C*
019200091112     C                   ENDSR
019300110704     C**********************************************************************
019400110704     c* leggo tutti gli eventi RS ed eventualmente le note inserite
019500110704     c* dall'aut
019600110704     C**********************************************************************
019700110704     c     srsql         begsr
019800110704     C/EXEC SQL
019900110704     C+ Declare A1 Cursor for
020000110704     C+ SELECT EVBCEV, EVBAAS, EVBLNP, EVBNRS, EVBNSP, EVBFLE,
020100110704     C+ ifnull(AR4NOT, ' ') FROM fnevb00f left outer join fiar400f
020200110704     C+ on evbaas = ar4aas and evblnp = ar4lnp and evbnrs = ar4nrs and
020300110704     C+ evbnsp = ar4nsp and ar4trc = '0' WHERE
020400110704     C+ EVBCEV = 'RS' and EVBDTV = :PARDD1 AND EVBFLE = :VI1PF1
020500110704     C/END-EXEC
020600110704      *          apertura cursore
020700110704     C/EXEC SQL
020800110704     C+ OPEN A1
020900110704     C/END-EXEC
021000110704     C                   DO        *hival
021100110704      *          lettura cursore
021200110704     C/EXEC SQL
021300110704     C+ Fetch Next From A1 Into
021400110704     C+ :EVBCEV, :EVBAAS, :EVBLNP, :EVBNRS, :EVBNSP, :EVBFLE, :AR4NOT
021500110704     C/END-EXEC
021600110704     C                   SELECT
021700110704     C                   WHEN      SqlCod = 100
021800110704     C                   LEAVE
021900110704     C                   WHEN      SqlCod >= 0
022000121016     c                   exsr      Ctr_LNA
022100121016     c                   IF        wStampa
022200110704     c                   eval      ok ='1'
022300120913     c                   exsr      Aut_RS
022400110704     C                   Exsr      STAMPA
022500121016     c                   ENDIF
022600110704     C                   OTHER
022700110704     C                   ENDSL
022800110704      *
022900110704     C                   ENDDO
023000110704     C/EXEC SQL
023100110704     C+ Close A1
023200110704     C/END-EXEC
023300110704     c                   if        ok = ' '
023400110704     C                   Exsr      STAMPA
023500110704     c                   end
023600110704     C                   ENDSR
023700120913
023800120913      /free
023900121016       //---------------------------------------------------------------
024000121016       //?Controllo la LNA.
024100121016       //---------------------------------------------------------------
024200121016       BEGSR Ctr_LNA;
024300121016
024400121016         wstampa = *on;
024500121016
024600121016       //?Cerco la LNA della bolla
024700121016         chain (EVBaas:EVBlnp:EVBnrs:EVBnsp) FNARB01L;
024800121016         IF  %found(FNARB01L);
024900121016           IF  VI1pf1 = ARBlna;
025000121016             leavesr;
025100121016           ENDIF;
025200121016         ELSE;
025300121016           clear ARBlna;
025400121016         ENDIF;
025500121016
025600121016       //?Carico la £6 della filiale richiesta a video
025700121016      /end-free
025800121016     c                   clear                   DSUL06
025900121016     c                   move      '£6'          D06COD
026000121016     c                   movel     VI1pf1        D06KEY
026100121016     c                   eval      kpjbu = DSUL06
026200121016     c                   call      'TRUL06R'
026300121016     c                   parm                    KPJBA
026400121016     c                   eval      DSUL06 = kpjbu
026500121016     c                   movea     LIN           wL6
026600121016      /free
026700121016         IF  ARBlna > 0 and %lookup(ARBlna : wL6) > 0;
026800121016           leavesr;
026900121016         ENDIF;
027000121016
027100151014       //?controllo se la LNA della bolla è tra quelle in £J della filiale utente
027200151014         IF  ARBlna > 0 and %lookup(ARBlna : LJ) > 0;
027300151014           leavesr;
027400151014         ENDIF;
027500151014
027600121016       //?se arrivo qua vuol dire che il rcd NON si può stampare
027700121016         wstampa = *off;
027800121016
027900121016       ENDSR;
028000121016
028100120913       //---------------------------------------------------------------
028200120913       //?Controllo se RS autorizzata
028300120913       //---------------------------------------------------------------
028400120913       BEGSR Aut_RS;
028500120913
028600120913         *in10 = *off;
028700120913         clear wTGDdcd;
028800120913
028900120913       //?Leggo il file delle R.A.
029000120913         TGDtor = 'S';
029100120913         TGDogg = %editc(EVBlnp:'X') + %editc(EVBnrs:'X') +
029200120913                  %editc(EVBnsp:'X') + %editc(EVBaas:'X');
029300120913         setll (TGDtor : TGDogg) FITGD02L;
029400121011         reade (TGDtor : TGDogg) FITGD02L;
029500120913         DOW not %eof(FITGD02L);
029600120913           IF  TGDmad = ' 50';
029700120913             *in10 = *on;
029800120913             wData_ISO = %date(TGDdcd:*ISO);
029900120913             wData_EUR = wData_ISO;
030000120913             wTGDdcd   = %dec(wData_EUR);
030100120913             leavesr;
030200120913           ENDIF;
030300121011           reade (TGDtor : TGDogg) FITGD02L;
030400120913         ENDDO;
030500120913
030600120913       ENDSR;
030700120913      /end-free
030800120913
030900110704     C**********************************************************************
031000110704     c* STAMPO
031100110704     C**********************************************************************
031200110704     c     STAMPA        begsr
031300110704     C                   IF        *INOF
031400110704     C                   EXCEPT    testata
031500110704     C                   SETOFF                                       OF
031600110704     C                   END
031700110704     c                   if        ok = '1'
031800110704     C                   EXCEPT    detta
031900110704     c                   else
032000110704     C                   EXCEPT    noncene
032100110704     c                   end
032200110704     C                   ENDSR
032300110704     C**********************************************************************
032400110704     Oqsysprt   E            testata        1 02
032500120913     o                       rsut                20
032600110704     o                                        +   3 'LISTA SPEDIZIONI CON RISER-
032700110708     o                                              VA SPECIFICA INSERITE IL'
032800110704     o                       vi1dd1        y  +   1
032900120917     o                                        +  10 'FNLRE10R'
033000120917     o                       udate         y  +  15
033100110704     O                       page          z  +   1
033200120913     O          E            testata        1
033300120913     o                       knmus            +   0
033400120917     o                                        +  30 'In arrivo a'
033500120917     o                       vi1pf1           +   1
033600110704     O          E            testata     1  1
033700110704     o                                        +   0 'Spedizione'
033800120919     o                                           24 'Note'
033900120919     o                                           79 'Autorizzazione con R.A.'
034000120919     O          E            testata        1
034100120919     o                       wtrat              132
034200110704     O          E            detta       1
034300110704     o                       evbaas           +   0
034400110704     o                       evblnp           +   1
034500110704     o                       evbnrs        z  +   1
034600110704     o                       evbnsp        z  +   1
034700110704     o                       ar4not           +   1
034800120919     o               10                       +   1 'da'
034900120913     o               10      TGDutc           +   1
035000120913     o               10                       +   1 'il'
035100120913     o               10      wTGDdcd          +   1 '  /  /    '
035200110704     O          E            noncene     3
035300110704     o                                        +  10 'N O N   E S I S T O N O   -
035400110704     o                                              S P E D I Z I O N I   -
035500110704     o                                              C O N   -
035600110704     o                                              R I S E R V A   S P E C I -
035700110704     o                                              F I C A'
