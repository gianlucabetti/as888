000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200041230      *---------------------------------------------------------------*
000300041230      * Recupero codice cliente da aggiornare nei reclami             *
000400041230      *---------------------------------------------------------------*
000500041231     fFITGD00F  if   e             disk    rename(fitgd000:fitgd0) prefix(F_)
000600041231     fFITGD01L  uf   e           k disk
000700041230     fFNARB01L  if   e           K disk
000800041230     fFNBLP01L  if   e           K disk
000900041230     fFNORM01L  if   e           K disk
001000041230     fTITAS30C  if   e           K disk    usropn
001100041230      *
001200041231     d cmd             s             80    dim(1) ctdata perrcd(1)              comando
001300041231      *
001400041230     d Wrk_cchiA       s                   like(tgdksc)
001500041230     d Wrk_cchiB       s                   like(tgdksc)
001600041230     d Wrk_cchiC       s                   like(tgdksc)
001700041230     d Wrk_cchi        s                   like(tgdksc)
001800041231
001900041231     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
002000041231     d comman          s             80                                         valore cmd CLP
002100041231     d wlib            s             10
002200041230      *
002300041230      * - DS di comodo    Spedizione
002400041230     D                 DS
002500041230     D  W_lnp                         3  0
002600041230     D  W_nrs                         2  0
002700041230     D  W_nsp                         7  0
002800041230     D  W_aas                         4  0
002900041230     D  Wspevid                1     16  0 inz
003000041230      * - DS di comodo    ORM
003100041230     D                 DS
003200041230     D  W_poe                         3  0
003300041230     D  W_nsr                         2  0
003400041230     D  W_nor                         7  0
003500041230     D  W_nrv                         2  0
003600041230     D  Wormvid                1     14  0 inz
003700041231      * - Parametri x Controllo profilo utenti
003800041231     d TIBS34DS      e ds                  inz
003900041231      * - Ds di riferimento al file esterno AZUTE00F
004000041231     d AZUTEDS       e ds                  extname(AZUTE00F)
004100041231      * - Ds per dati organigramma
004200041231     d DDatiUte      e ds
004300041230      *
004400041231     d KPJBA         e ds
004500041230      *
004600041231     c                   do        *hival
004700041231     c                   read      fitgd00f
004800041231     c                   if        %eof(fitgd00f)
004900041231     c                   leave
005000041231     c                   endif
005100041231      * se cliente valorizzato leggo il successivo
005200041231     c                   If        F_tgdksc <> *zeros
005300041231     c                   iter
005400041231     c                   endif
005500041231
005600041230     c                   exsr      RECCLI
005700041230      *
005800041230     c                   select
005900041230     c                   when      wrk_cchiA > 0
006000041230     c                   eval      wrk_cchi = wrk_cchiA
006100041230     c                   when      wrk_cchiB > 0
006200041230     c                   eval      wrk_cchi = wrk_cchiB
006300041230     c                   other
006400041230     c                   eval      wrk_cchi = wrk_cchiC
006500041230     c                   endsl
006600041230      * valorizzo il codice nel file
006700041231     c                   If        wrk_cchi > 0
006800041230     c                   exsr      REGCLI
006900041231     c                   endif
007000041231
007100041231     c                   enddo
007200041231      *
007300041231     c                   seton                                        lr
007400041230      *
007500041230      *---------------------------------------------------------------*
007600041230      * Routine recupero codice chiamante                             *
007700041230      *---------------------------------------------------------------*
007800041230     c     RECCLI        Begsr
007900041230
008000041230     c                   clear                   Wrk_cchi
008100041230     c                   clear                   Wrk_cchiA
008200041230     c                   clear                   Wrk_cchiB
008300041230     c                   clear                   Wrk_cchiC
008400041230      *
008500041230      * rileggo il dettaglio
008600041230      *
008700041230
008800041230     c     ktgdan        setll     Fitgd01l
008900041230      *
009000041230     c                   Do        *hival
009100041230     c     ktgdan        reade(N)  fitgd01l
009200041230
009300041230     c                   if        %eof(fitgd01l)
009400041230     c                   leave
009500041230     c                   endif
009600041230
009700041230      * ---------- SPEDIZIONE -----------
009800041230
009900041230    2c                   if        tgdtor = 'S'
010000041230      *
010100041230     c                   movel     tgdogg        Wspevid
010200041230      * Aggancio bolle arrivi
010300041230     c     KSPEDw        chain     Fnarb01l
010400041230    3c                   If        %found(Fnarb01l)
010500041230      * mittente se valorizzato
010600041230     c                   move      arbccm        w0040             4 0
010700041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
010800041230     c                             w0040 <> 9999
010900041230     c                   z-add     arbccm        Wrk_cchiA
011000041230      * se trovato codice mittente prendo quello e scappo
011100041230     c                   leave
011200041230    4c                   endif
011300041230      *
011400041230     c                   z-add     arbksc        W0040
011500041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
011600041230     c                             w0040 <> 9999
011700041230      * in caso di CCM uguale a zeros valorizzo con KSC
011800041230     c                   z-add     arbksc        Wrk_cchiA
011900041230     c                   leave
012000041230    4c                   endif
012100041230      *
012200041230    3c                   endif
012300041230      * Aggancio bolle partenze se non ho trovato ancora nulla
012400041230     c     KSPEDw        chain     Fnblp01l
012500041230    3c                   If        %found(Fnblp01l)
012600041230      * mittente se valorizzato
012700041230     c                   move      blpccm        w0040
012800041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
012900041230     c                             w0040 <> 9999
013000041230     c                   z-add     blpccm        Wrk_cchiA
013100041230      * se trovato codice mittente prendo quello e scappo
013200041230     c                   leave
013300041230    4c                   endif
013400041230      *
013500041230     c                   z-add     blpksc        W0040
013600041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
013700041230     c                             w0040 <> 9999
013800041230      * in caso di CCM uguale a zeros valorizzo con KSC
013900041230     c                   z-add     blpksc        Wrk_cchiA
014000041230     c                   leave
014100041230    4c                   endif
014200041230      *
014300041230    3c                   endif
014400041230      * Aggancio bolle di sede  se non ho trovato ancora nulla
014500041230     c     KSPEDw        chain     titas30c
014600041230    3c                   If        %found(Titas30c)
014700041230      * mittente se valorizzato
014800041230     c                   move      tasccm        w0040
014900041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
015000041230     c                             w0040 <> 9999
015100041230     c                   z-add     tasccm        Wrk_cchiA
015200041230      * se trovato codice mittente prendo quello e scappo
015300041230     c                   leave
015400041230    4c                   endif
015500041230      *
015600041230     c                   z-add     tasksc        W0040
015700041230    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
015800041230     c                             w0040 <> 9999
015900041230      * in caso di CCM uguale a zeros valorizzo con KSC
016000041230     c                   z-add     tasksc        Wrk_cchiA
016100041230     c                   leave
016200041230    4c                   endif
016300041230      *
016400041230    3c                   endif
016500041230      *
016600041230    2c                   endif
016700041230
016800041230      * ----------    ORM     -----------
016900041230
017000041230    2c                   if        tgdtor = 'O'
017100041230     c                   movel     tgdogg        Wormvid
017200041230      *
017300041230     c     KORMw         chain     Fnorm01l
017400041230    3c                   If        %found(Fnorm01l)
017500041230     c
017600041230      * ordinanante
017700041231     c                   movel     ormcor        w0070             7 0
017800041231     c                   move      w0070         w0040             4 0
017900041230     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
018000041230     c                             w0040 <> 9999  and Wrk_cchiA = 0
018100041230     c                   z-add     w0070         Wrk_cchiA
018200041230     c                   endif
018300041230
018400041230      * ritiro
018500041230     c                   movel     ormcra        w0070
018600041230     c                   move      w0070         w0040
018700041230     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
018800041230     c                             w0040 <> 9999 and Wrk_cchiB = 0
018900041230     c                   z-add     w0070         Wrk_cchiB
019000041230     c                   endif
019100041230
019200041230      * destinatario
019300041230     c                   movel     ormcrc        w0070
019400041230     c                   move      w0070         w0040
019500041230     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
019600041230     c                             w0040 <> 9999 and Wrk_cchiC = 0
019700041230     c                   z-add     w0070         Wrk_cchiC
019800041230     c                   endif
019900041230    3c                   endif
020000041230    2c                   endif
020100041230
020200041230      * ----------  GENERICO  -----------
020300041230      * leggo il successivo
020400041230    2c                   if        tgdtor = 'G'
020500041230     c                   iter
020600041230    2c                   endif
020700041230
020800041230     c                   enddo
020900041230
021000041230     c                   endsr
021100041231      *---------------------------------------------------------------*
021200041231      * Routine aggiornamento codice cliente                          *
021300041231      *---------------------------------------------------------------*
021400041231     c     REGcli        BegSR
021500041231
021600041231      * per ogni record di dettaglio reclamo aggiorno i dati della parte alta
021700041231      * del video
021800041231
021900041231     c     Ktgdan        setll     Fitgd01l
022000041231      *
022100041231     c                   Do        *hival
022200041231     c     Ktgdan        reade     fitgd01l
022300041231
022400041231     c                   if        %eof(fitgd01l)
022500041231     c                   leave
022600041231     c                   endif
022700041231
022800041231     c                   eval      tgdksc = wrk_cchi
022900041231
023000041231     c                   update    fitgd000
023100041231
023200041231     c                   enddo
023300041231
023400041231     c                   endsr
023500041231
023600041231      *---------------------------------------------------------------*
023700041231      * Routine Iniziale                                              *
023800041231      *---------------------------------------------------------------*
023900041231     c     *InzSR        BegSR
024000041231      *
024100041231     c     *Entry        plist
024200041231     c                   parm                    KPJBA
024300041231      *
024400041231      * Reperisco dati job
024500041231     c                   exsr      DatiJob
024600041231
024700041231
024800041231      * controllo il sistema informativo dove sono per reperire
024900041231      * la libreria
025000041231      *   ---> GAITRAGRU
025100041231     c                   If        %subst(knsif:7:1) <> 'P'
025200041231     c                   Eval      wlib = 'GAITRAGRU '
025300041231     c                   EndIf
025400041231      *   ---> GAITRAP/FILTRAP/UNITRAGRU
025500041231     c                   If        %subst(knsif:7:1) = 'P'
025600041231     c                   Eval      wlib = 'GAITRAGRPS'
025700041231     c                   EndIf
025800041231
025900041231      * apro il file titas
026000041231     c                   Clear                   comman
026100041231     c                   Movel(p)  cmd(1)        comman
026200041231     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
026300041231     c                   Eval      %Subst(comman:30:10) = wlib
026400041231     c                   Eval      comman =
026500041231     c                             %trim(comman) + '/TITAS30C)'
026600041231     c                   Call      'QCMDEXC'                            99
026700041231     c                   Parm                    comman
026800041231     c                   Parm                    lenght
026900041231      * apro il file
027000041231     c                   open      titas30c
027100041231      *
027200041231      * - file FITGD01L
027300041231     c     ktgdan        klist
027400041231     c                   kfld                    F_tgdant
027500041231     c                   kfld                    F_tgdnut
027600041231
027700041231      * - file FNARB01L - FNBLP01L
027800041231     c     kspedw        klist
027900041231     c                   kfld                    W_aas
028000041231     c                   kfld                    W_lnp
028100041231     c                   kfld                    W_nrs
028200050110     c                   kfld                    W_nsp
028300041231      * - file FNORM01L
028400041231     c     kormw         klist
028500041231     c                   kfld                    W_poe
028600041231     c                   kfld                    W_nsr
028700041231     c                   kfld                    W_nor
028800041231     c                   kfld                    W_nrv
028900041231     C                   ENDSR
029000041231      *
029100041231      *---------------------------------------------------------------*
029200041231      * Reperimento Dati del job (Utente/Operativi)                   *
029300041231      *---------------------------------------------------------------*
029400041231     c     DatiJob       BegSr
029500041231      *
029600041231     c     *dtaara       define    §azute        azuteds
029700041231     c     *dtaara       define    §datiute      ddatiute
029800041231      *
029900041231     c                   in(E)     *dtaara
030000041231     c                   IF        %ERROR or RSUT = *blanks
030100041231     c                   clear                   Tibs34Ds
030200041231     c                   call      'TIBS34R'
030300041231     c                   parm                    Tibs34Ds
030400041231     c                   in        *dtaara
030500041231     c                   ENDIF
030600041231     C                   ENDSR
030700041231**   cmd - comando
030800041231OVRDBF FILE(        ) TOFILE(
