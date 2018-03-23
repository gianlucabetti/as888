000100130805      *===============================================================*
000200130805      * TNSB31R    * Rigenerazione immagine LdV per AUTISTA           *
000300130805      *===============================================================*
000400130805
000500130805     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600130805
000700130805      *===============================================================*
000800130805
000900130805     fTILAC00F  o    e             disk
001000131004     ftabel00f  if   e           k disk
001100040721
001200130805      *===============================================================*
001300130805
001400130805      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
001500130805      *
001600130805     d KPJBA         e ds
001700130805      * - Parametri x Controllo profilo utenti
001800130805     d TIBS34DS      e ds                  inz
001900130805      * - Ds di riferimento al file esterno AZUTE00F
002000130805     d AZUTEDS       e ds                  extname(AZUTE00F)
002100130805      * - Ds per dati organigramma
002200130805     d DDatiUte      e ds
002300130805
002400131004     d titasds       e ds                  extname(titas00F)
002500130805
002600131004     d dtasflo       e ds                  inz
002700131004     d TNSB30ds      e ds                  inz
002800130805
002900130805     d DS_Time1        ds            14    inz
003000130805     d   dsT1_hms                     6  0 inz
003100130805     d   dsT1_dmy                     8  0 inz
003200130805     d DS_Time2        ds            14    inz
003300130805     d   dsT2_ymd                     8  0 inz
003400130805     d   dsT2_hms                     6  0 inz
003500130805      *
003600131004     d comdtd          s                   like(isb30dtd)
003700131004     d comdta          s                   like(isb30dta)
003800131004     D WrkSqlCmd       S           1024
003900131004     D file            S             10
004000130805      *
004100130805      * - Campi di comodo
004200130805     d WdateISO        s               d   datfmt(*ISO) inz
004300130805     d Wtime           s             14  0              inz
004400130805     d wEnD            s               n   inz(*off)
004500130805     d Wftdpdr         s              7    inz
004600130805     d Wftdddc         s              8    inz
004700130805     d Wftdaas         s              4    inz
004800130805     d Wftdlnp         s              3    inz
004900130805     d Wftdnrs         s              2    inz
005000130805     d Wftdnsp         s              7    inz
005100130805
005200130805      *===============================================================*
005300130805      *  RIEPILOGO INDICATORI                                         *
005400130805      *---------------------------------------------------------------*
005500130805      *                                                               *
005600130805      *===============================================================*
005700130805
005800130805     c     *Entry        plist
005900130805     c                   parm                    KPJBA
006000130805     c                   movel     KPJBU         TNSB30ds
006100131004     c                   eval      comdta = isb30dta
006200131004     c                   eval      comdtd = isb30dtd
006300130805      /free
006400130805        exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006500130805      /end-free
006600130805
006700130805      * Operazioni iniziali
006800130805     c                   exsr      OperazIniz
006900131004     c                   do        *hival
007000131004     c*
007100131004     c* Lettura titas00f/titas10f/titasp0f
007200131004     c                   exsr      readtas
007300131004     c* verifico se ho finito la lettura oppure devo leggere anche gli altri
007400131004     c* TAS
007500131004     c                   select
007600131008     C                   when      file = 'TASP'
007700131008     C                   EVAL      file = 'TAS10'
007800131004     c                   exsr      istruz
007900131004     C                   iter
008000131008     C                   when      file = 'TAS10'
008100131008     C                   EVAL      file = 'TAS0'
008200131004     c                   exsr      istruz
008300131004     C                   iter
008400131004     c                   other
008500131004     c                   leave
008600131004     C                   endsl
008700131004     c                   enddo
008800130805      * Fine
008900130805     c                   eval      *inLR = *on
009000130805
009100130805      *---------------------------------------------------------------*
009200131004      * lettura titas
009300130805      *---------------------------------------------------------------*
009400131004     c     readtas       BEGSR
009500131004     C/EXEC SQL
009600131004     C+ PREPARE S1 FROM :WrkSqlCmd
009700131004     C/END-EXEC
009800131004
009900131004     C/EXEC SQL
010000131004     C+ DECLARE A1 CURSOR FOR S1
010100131004     C/END-EXEC
010200131004
010300131004     C/EXEC SQL
010400131004     C+ OPEN A1
010500131004     C/END-EXEC
010600131004
010700131004     C                   DOU       SqlCod <> 0
010800131004     C/EXEC SQL
010900131004     C+ FETCH NEXT FROM A1 INTO :titasds
011000131004     C/END-EXEC
011100131004     c*
011200131004     C                   SELECT
011300131004     C                   WHEN      SqlCod = 0
011400131004     c                   exsr      Wrt_TILAC
011500131004     C                   WHEN      SqlCod = 100
011600131004     C                   WHEN      SqlCod <> 0
011700131004      * Forzo la stampa del JOBLOG.
011800131004     C                   CALL      'X66CHGJOB'
011900131004     C                   ENDSL
012000131004     C                   ENDDO
012100131004     C/EXEC SQL
012200131004     C+ CLOSE A1
012300131004     C/END-EXEC
012400131004     c                   endsr
012500131004      *---------------------------------------------------------------*
012600131004      * Operazioni iniziali                                           *
012700131004      *---------------------------------------------------------------*
012800131004     c     OperazIniz    BEGSR
012900130805
013000130805      * Reperisco dati job
013100130805     c                   exsr      DatiJob
013200130805
013300130805      * Impostazione del campo LAcTIM = aaaa/mm/gg+hh:mm:ss
013400130805     c                   time                    Wtime
013500130805     c                   movel     Wtime         DS_Time1
013600130805     c                   eval      dsT2_hms   = dsT1_hms
013700130805     c     *eur          movel     dsT1_dmy      WdateISO
013800130805     c                   move      WdateISO      dsT2_ymd
013900131004     c*
014000131008     C                   EVAL      file = 'TASP'
014100131004     c                   exsr      istruz
014200130805     c                   ENDSR
014300130805
014400130805      *---------------------------------------------------------------*
014500130805      * Reperimento Dati del job (Utente/Operativi)                   *
014600130805      *---------------------------------------------------------------*
014700130805     c     DatiJob       BEGSR
014800130805
014900130805     c     *dtaara       define    §azute        azuteds
015000130805     c     *dtaara       define    §datiute      ddatiute
015100130805
015200130805     c                   in(E)     *dtaara
015300130805     c                   IF        %ERROR or RSUT = *blanks
015400130805     c                   clear                   Tibs34Ds
015500130805     c                   call      'TIBS34R'
015600130805     c                   parm                    Tibs34Ds
015700130805     c                   in        *dtaara
015800130805     c                   ENDIF
015900130805
016000130805     c                   ENDSR
016100130805
016200040721      *---------------------------------------------------------------*
016300050317      * Registrazione dati nel file TILAC00F x cliente                *
016400040721      *---------------------------------------------------------------*
016500050317     c     Wrt_TILAC     BEGSR
016600130805
016700131004     c                   movel     taspdc        Wftdpdr
016800131004     c                   movel     tasddc        Wftdddc
016900131004     c                   movel     tasaas        Wftdaas
017000131004     c                   movel     taslnp        Wftdlnp
017100131004     c                   movel     tasnrs        Wftdnrs
017200131004     c                   movel     tasnsp        Wftdnsp
017300040721      *
017400050317     c                   clear                   TILAC000
017500050317     c                   move      TASaas        LACaas
017600050317     c                   move      TASlnp        LAClnp
017700050317     c                   move      TASnrs        LACnrs
017800050317     c                   move      TASnsp        LACnsp
017900130805     c                   movel     'CLI'         LACcop
018000040721      *
018100040721      * Impostazione nuovi dati
018200050317     c                   eval      LACtim  = DS_Time2
018300050317     c                   eval      LACela  = *zeros
018400130805     c                   eval      LACdir  = 'A/' + %subst(ISB30idjob:1:8) +
018500130805     c                             '/' + wftdpdr + '/' + wftdddc
018600130625     c                   eval      LACtpi  ='PDF'
018700130625     c                   eval      LACnim  = wftdaas+wftdlnp+wftdnrs+wftdnsp
018800130625     c                   eval      lactad = 'N'
018900090310      * imposto i nuovi campi di TILAC
019000090310     c                   eval      lactela = 'S'
019100130625     c                   eval      lacksc = 1020001
019200130625     c                   eval      lacksu = 1020001
019300130805     c                   eval      lacidjob = ISB30idjob
019400040721      *
019500050317     c                   WRITE     TILAC000
019600040721      *
019700040721     c                   ENDSR
019800131004?     *--------------------------------------------------------------*
019900131004?     *  istruz: Operazioni di inizializzazione dati                 *
020000131004?     *--------------------------------------------------------------*
020100131004     C     istruz        BEGSR
020200131004     C                   EVAL      WrkSqlCmd
020300131004     C                             =
020400131004     C                             'SELECT * from '
020500131004     c                   select
020600131008     C                   when      file = 'TASP'
020700131004     C                   EVAL      WrkSqlCmd
020800131004     C                             =
020900131004     C                             %TRIMR(WrkSqlCmd)
021000131004     C                             +
021100131004     C                             ' titasp0f where '
021200131008     C                   when      file = 'TAS10'
021300131004     C                   EVAL      WrkSqlCmd
021400131004     C                             =
021500131004     C                             %TRIMR(WrkSqlCmd)
021600131004     C                             +
021700131004     C                             ' titas10f where '
021800131004     c                   other
021900131004     C                   EVAL      WrkSqlCmd
022000131004     C                             =
022100131004     C                             %TRIMR(WrkSqlCmd)
022200131004     C                             +
022300131004     C                             ' titas00f where '
022400131004     c                   endsl
022500131004     C                   EVAL      WrkSqlCmd
022600131004     C                             =
022700131004     C                             %TRIMR(WrkSqlCmd)
022800131004     C                             +
022900131004     C                             ' substr(tasflo, 5, 1) <> '' '''
023000131004     C                             +
023100131004     C                             ' AND '
023200131004     C                             +
023300131004     C                             ' tasddc BETWEEN '
023400131004     C                             + %editc(isb30dtd:'X')
023500131004     C                             +
023600131004     C                             ' AND ' + %editc(isb30dta:'X')
023700131004     C                             +
023800131004     C                             ' AND '
023900131004     C                             +
024000131004     C                             ' taspdc = '
024100131004     C                             + %editc(isb30pdr:'X')
024200131004     C                             +
024300131004     C                             ' AND '
024400131004     C                             +
024500131004     C                             ' tasddc = tasdcm '
024600131004     C                   ENDSR
