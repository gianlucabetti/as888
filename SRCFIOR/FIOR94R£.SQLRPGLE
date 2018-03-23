000100031017      *PARMS DYNUSRPRF(*OWNER)
000200030404     H DECEDIT('0,') DATEDIT(*DMY.)
000300031022      * FIOR94R *-----------------------------------------------------*
000400031022      *  - STAMPA ANAGRAFICA RITIRI FISSI NON UTILIZZATI              *
000500020411      *---------------------------------------------------------------*
000600020604
000700040818     FFnors01L  iF   E           k DISK
000800031020     FFnacr01L  IF   E           k DISK
000900031014
001000031014     FSysprt    O    F  132        Printer OfLind(*inof)
001100031002      *
001200030521     D tibs34ds      E DS                  inz
001300030521     D ddatiute      e ds
001400030521     D azuteds       e ds                  extname(AZUTE00F)
001500030424     D Kpjba         e ds
001600031022      *
001700031022      * Ds per Trul06r - caricamento £1
001800031022     D Dsul06        e ds                  EXTNAME(trul06ds)
001900031022     D LIA                     1     90  0
002000031022     D                                     DIM(30)
002100031022      *
002200031022      * Schiera per caricamento L6
002300031022     D L6              s              3  0 DIM(30)
002400031022      *
002500031022      * Parametri di selezione
002600031022     D Param           ds
002700031022     D $fil                           3  0
002800031017      *
002900031017     D DSfissi         ds
003000031017     D  poe_num                      10
003100030527      *
003200031020     D WRK_nors        s             10
003300031014      *
003400031014     D W0140           S             14  0
003500031014     D Wdtgio          S              8  0
003600031014     D dateu           S              8  0
003700031014     D wdata           S               D   DATFMT(*iso)
003800031014     D wrkdta          s              8  0
003900031017      * Schiera per caricamento codici ordini ritiri fissi
004000031020     D CROF            s             10    DIM(9000)
004100031017      *
004200031017     D XX              s              6  0 inz
004300031014      *
004400031014     D WLBDAT          DS                  INZ
004500031014     D  G02DAT                 1      8  0
004600031014     D  G02INV                 9     16  0
004700031014     D  G02ERR                17     17
004800031014     D  G02TGI                18     22  0
004900030521      *
005000031020     D CONT_ELI        s              6  0 inz
005100031015      *
005200030416      *---------------------------------------------------------------*
005300030418      * INDICATORI UILIZZATI
005400030424      *---------------------------------------------------------------*
005500030418      *---------------------------------------------------------------*
005600031002      *
005700031017     C*
005800031017     C/EXEC SQL
005900031020     C+ DECLARE C1 CURSOR FOR SELECT DISTINCT
006000031020     C+ concat (digits(ormpos),digits(ormors))
006100031020     C+ FROM Fnorm00f WHERE ormpos > 0
006200031020     C+ and ormors > 0 and ormdao between :wrkdta and :dateu
006300031017     C/END-EXEC
006400031017     C
006500031017     C/EXEC SQL
006600031017     C+ open C1
006700031017     C/END-EXEC
006800031017     C
006900031017     C/EXEC SQL
007000031017     C+ Fetch C1 into :dsfissi
007100031017     C/END-EXEC
007200031017     C*
007300031017     C                   dow       sqlcod = *zeros
007400031017      * carico la schiera degli ordini fissi
007500031017     C*
007600031017     C                   add       1             xx
007700031017     C                   movea     poe_num       crof(xx)
007800031017     C*
007900031017     C/EXEC SQL
008000031017     C+ Fetch C1 into :dsfissi
008100031017     C/END-EXEC
008200031017     C*
008300031017     C                   enddo
008400031017     C*
008500031017     C/EXEC SQL
008600031017     C+ close C1
008700031017     C/END-EXEC
008800031017     C*
008900031017      * Verifico se esiste
009000031017      *
009100031017     c                   do        *hival
009200031017      *
009300031020     c                   read      fnors01l
009400031017      *
009500031020     c                   if        %eof(fnors01l)
009600031017     c                   leave
009700031017     c                   endif
009800031017      *
009900031017     c                   if        ORSatb = 'A'
010000031017     c                   iter
010100031017     c                   endif
010200031022      * verifico se P.O. emissione esiste nella £6 solo se la filaile passata a programma
010300031022      * è diversa da zero
010400031022     c                   if        $fil > 0
010500031022     c     orspoe        lookup    l6                                     30
010600031022     c                   if        not %found
010700031022     c                   iter
010800031022     c                   endif
010900031022     c                   endif
011000031002      * verifico se esiste nella schiera caricata all'inzio del programma
011100031002      *
011200031020     c                   movel(p)  orspoe        wrk_nors
011300031020     c                   move      orsnor        wrk_nors
011400031020     c     wrk_nors      lookup    crof                                   90
011500031020      * se lo trovo leggo il successivo
011600031020     c                   if        not %found
011700031020      * aggancio Anargrafica ritiri
011800031020     c     orscra        chain     fnacr01l
011900031020      *
012000031014     c                   if        not *in99 or *inof
012100031014     c                   except    testa
012200031014     c                   setoff                                       of
012300031014     c                   seton                                        99
012400031014     c                   endif
012500031020     c                   except    detta
012600031020     c                   add       1             cont_eli
012700031020      *
012800031020      * se lo trovo leggo il successivo
012900031020      *
013000031002     c                   iter
013100031002     c                   endif
013200031002     c                   enddo
013300031002      *
013400031127     c                   if        not *in99 or *inof
013500031127     c                   except    testa
013600031127     c                   setoff                                       of
013700031127     c                   seton                                        99
013800031127     c                   endif
013900031015     c                   except    totali
014000031002     c                   seton                                        lr
014100030526      *
014200030416      *-----------------------------------------------------------------*
014300030404      * SUBROUTINE INIZIALE
014400030404      *-----------------------------------------------------------------*
014500030521     C     *INZSR        BEGSR
014600030404      *
014700030404     C     *ENTRY        PLIST
014800030410     C                   PARM                    kpjba
014900031022     c                   movel     kpjbu         param
015000030521      *
015100030521     c     *dtaara       define    §azute        azuteds
015200030521     c     *dtaara       define    §datiute      ddatiute
015300030521     C                   in(E)     *dtaara
015400030521     C                   IF        %error  or RSUT = *blanks
015500030521     C                   CLEAR                   tibs34ds
015600030521     C                   CALL      'TIBS34R'
015700030521     C                   PARM                    tibs34ds
015800030521     C                   in        *dtaara
015900030521     c                   endif
016000030527      *
016100031014      * reperisco data e ora
016200031014     c                   Time                    W0140
016300031014      * udate in GGMMAAAA
016400031014     c                   Move      W0140         Wdtgio
016500031014      * udate IN AAAAMMGG
016600031014     c                   Z-Add     Wdtgio        G02dat
016700031014     c                   Movel     *Blank        G02err
016800031014     c                   Call      'XSRDA8'
016900031014     c                   Parm                    Wlbdat
017000031014     c                   Movel     G02inv        Dateu
017100031014     c* calcolo la data per il controllo della data immissione ORM
017200031014     c     *iso          move      dateu         wdata
017300031020     c                   subdur    3:*m          wdata
017400031014     c     *iso          move      wdata         wrkdta
017500031022      *
017600031022      * Carico £6 se la filiale passata non è uguale a zero che indica tutte le filiali
017700031022     c                   IF        $fil > 0
017800031022     C                   clear                   Dsul06
017900031022     C                   move      '£6'          D06Cod
018000031022     C                   movel     $Fil          D06Key
018100031022     C                   movel     Dsul06        kpjbu
018200031022     C                   call      'TRUL06R'
018300031022     C                   parm                    kpjba
018400031022     C                   movel     kpjbu         Dsul06
018500031022     C                   movea     LIA           L6
018600031022     C                   clear                   kpjbu
018700031022     c                   endif
018800030521      *
018900030404     C                   ENDSR
019000031014     Osysprt    E            TESTA          2  2
019100031014     O                       RSUT                21
019200031127     O                                         + 18 'Elenco Anagr.Ritiri Fissi '
019300031127     O                                              'non utilizzati'
019400031014     O                       UDATE         Y   + 12
019500031127     O                                         +  3 'FIOR94R'
019600031014     O                                         +  3 'Pag.'
019700031014     O                       PAGE          Z   +  1
019800031014      *
019900031014     O          E            TESTA       1  1
020000070419     O                                         +  3 'Fil. emiss.'
020100031020     O                                         +  2 'Num.Ord.Fisso'
020200031020     O                                         +  5 'M/P'
020300031020     O                                         +  1 'Mittente   '
020400031020     O                                         + 45 'Frequenza ritiro'
020500031014      *---------------------------------------------------------------
020600031020     O          E            DETTA       1  1
020700031020     O                       ORSPOE            +  7
020800031020     O                       ORSNOR        Z   +  6
020900031020     O                       ORSRMP            + 12
021000031020     O                       ORSCRA            +  2 '   .    .   '
021100031020     O                       ACRRSR            +  1
021200031020     O                       ACRTCR            +  1
021300031017     O          E            totali      1  1
021400031017     O                                         +  1 'Totale annullabili    '
021500031020     O                       CONT_eli      2   +  4
