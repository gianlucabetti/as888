000100940325     H DECEDIT('0,') DATEDIT(*YMD.)
000200021003     H OPTION(*NODEBUGIO)
000300941129      * FNLR94R *----------------------------------------------------*
000400940325      *-----------------*                                            *
000500940325      * ARRIVI:STAMPA SPEDIZIONI RIPRISTINATE DOPO CONTEGGI          *
000600940325      *--------------------------------------------------------------*
000700990803     FFNEVB03L  IF   E           K DISK
000800060505     Fazorg01L  IF   E           K DISK
000900940329      *------------
001000940325     FTABEL00F  UF A E           K DISK
001100940325      *------------
001200940325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300940325     F                                     USROPN
001400940325      *--------------------------------------------------------------*
001500940325      *  SCHIERE
001600940325      *--------------------------------------------------------------*
001700080903     D C2A             S              3    DIM(200)                             COD.CAUS.EVENTI
001800080903     D D2A             S             30    DIM(200)                             DES.CAUS.EVENTI
001900940325     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
002000940325      *--------------------------------------------------------------*
002100940325      *  DS
002200940325      *--------------------------------------------------------------*
002300940325     D KPJBA         E DS
002400060529     D param           DS
002500060529     D  parmpo                 1      3s 0
002600940325      *
002700050203     D ddatiute      e ds
002800050203     D azuteds       e ds                  extname(AZUTE00F)
002900050203     D tibs34ds      E DS                  inz
003000060411     D TRul06ds      E DS
003100060411     D  £1                     1     90  0 dim(30)
003200021119     d Tibs36Ds      e ds
003300960927     D*
003400960927     D                 DS
003500960927     D  WGG                    1      2  0
003600960927     D  WMM                    3      4  0
003700960927     D  WAA                    5      8  0
003800960927     D  WDATA                  1      8  0
003900960927     D*
004000940325     D DS2A          E DS
004100940325      *
004200940325     D DS5D          E DS
004300940325     D                SDS
004400940325     D  NOMPGM                 1     10
004500940325      *--------------------------------------------------------------*
004600940325      *  CICLO PRINCIPALE
004700940325      *--------------------------------------------------------------*
004800021120      *   Loop filiali: per ogni filiale caricata in £1 vado a vedere
004900940325      *   se ci sono eventi da stampare
005000940325     C                   Z-ADD     1             X                 2 0
005100060411     C     X             DOWLE     30
005200021120     C     £1(X)         ANDNE     0
005300021120     C                   Z-ADD     £1(X)         EVWFLE
005400021120     C                   Z-ADD     £1(X)         STPFLE
005500940325      *   Loop causali: per ogni causale carica in C2A procedo alla
005600940325      *   stampa e alla decodifica
005700940325     C                   Z-ADD     1             Y                 2 0
005800080903     C     Y             DOWLE     200
005900940325     C     C2A(Y)        ANDNE     *BLANKS
006000940325     C*  Posizionamento iniziale  x causale
006100940325     C                   EXSR      SETCEV
006200940325     C*  Loop fino a fine causale
006300940325     C     *IN80         DOWEQ     '0'
006400940325     C                   EXSR      REDCEV
006500940325     C                   END
006600940325      *  Causale successiva
006700940325     C                   ADD       1             Y
006800940325     C                   END
006900940325      *  Filiale Successiva
007000940325     C                   ADD       1             X
007100940325     C                   END
007200940325      *  Fine pgm: Aggiorno data/ora stampa eventi
007300940325     C                   EXSR      AGGTAB
007400940325     C                   SETON                                        LR
007500940325      *-----------------------------------------------------*
007600940325      *  SETCEV: Posizionamento iniziale ad inizio causale
007700940325      *-----------------------------------------------------*
007800940325     C     SETCEV        BEGSR
007900940325      *
008000940325     C                   MOVEL     C2A(Y)        EVWCEV
008100940325     C                   MOVEL     C2A(Y)        STPCEV
008200940325     C                   MOVEL     D2A(Y)        DESCEV           30
008300990803     C     KEYEV1        SETGT     FNEVB03L
008400990803     C     KEYEVB        READE     FNEVB03L                               80
008500940325      *
008600940325     C                   ENDSR
008700940325      *-----------------------------------------------------*
008800940325      *  REDCEV: Lettura e stampa eventi
008900940325      *-----------------------------------------------------*
009000940325     C     REDCEV        BEGSR
009100940325      *
009200940325     C     EVBDEV        IFNE      STPDEV
009300940325      * Giro la data
009400940325     C                   Z-ADD     EVBDEV        STPDEV
009500990803     C                   MOVEL     EVBDEV        AA4               4 0
009600990803     C                   MOVE      AA4           AA                2 0
009700940325     C                   MOVE      EVBDEV        GG                2 0
009800990803     C                   MOVE      AA            STPDEV            6 0
009900940325     C                   MOVEL     GG            STPDEV
010000940325     C                   END
010100940325      *
010200940325     C                   Z-ADD     EVBAAS        STPAAS
010300940325     C                   Z-ADD     EVBLNP        STPLNP
010400940325     C                   Z-ADD     EVBNRS        STPNRS
010500940325     C                   Z-ADD     EVBNSP        STPNSP
010600940325     C                   Z-ADD     EVBHEV        STPHEV
010700940325     C                   Z-ADD     EVBCDU        STPCDU
010800021003
010900021119     C                   CLEAR                   Tibs36Ds
011000021119     C                   EVAL      I36POU = EVBFLE
011100021119     C                   EVAL      I36CoU = EVBCDU
011200021119     C                   CALL      'TIBS36R'
011300021119     C                   PARM                    Tibs36Ds
011400021003
011500021119     C                   IF        O36Err = *BLANK
011600021119     C                   MOVEL     O36UTE        STPDDU           10
011700021003     C                   ELSE
011800021003     C                   MOVEL     *ALL'?'       STPDDU
011900021003     C                   ENDIF
012000021003
012100940325      *  Stampa dati evento
012200940325     C   OF              EXCEPT    TESTA
012300940325     C                   EXCEPT    DETTA
012400991118     C                   MOVEL     'S'           WSTP              1
012500940325      *  Lettura successiva
012600990803     C     KEYEVB        READE     FNEVB03L                               80
012700940325      *
012800940325     C                   ENDSR
012900940325      *-----------------------------------------------------*
013000940325      *  AGGTAB: Aggiornamanto tabella 5d
013100940325      *-----------------------------------------------------*
013200940325     C     AGGTAB        BEGSR
013300940325      *
013400940325      *   Caricamento dati da tabelle 5D
013500940325     C                   MOVEL     '5D'          TBWCOD
013600060529     c                   movel(p)  parmpo        tbwkey
013700940325     C     KEYTAB        CHAIN     TABEL00F                           70
013800940325     C   70              CLEAR                   TABEL
013900960927     C                   Z-ADD     DATEU8        §5DDTE
014000940325     C                   TIME                    §5DORE
014100060427     C                   MOVEL(p)  DS5D          TBLUNI
014200020902     c* se non lo trova lo crea con nuova key di tabel in GRU
014300940325     C     *IN70         IFEQ      '1'
014400940325     C                   Z-ADD     1             TBLKUT
014500940325     C                   MOVEL     '5D'          TBLCOD
014600060529     c                   movel(p)  parmpo        tblkey
014700940325     C                   WRITE     TABEL
014800940325     C                   ELSE
014900940325     C                   UPDATE    TABEL
015000940325     C                   END
015100940325      *  Fine Stampa
015200991118     C     WSTP          IFEQ      'S'
015300940325     C   OF              EXCEPT    TESTA
015400940325     C                   EXCEPT    FINE1
015500991118     C                   END
015600940325     C                   CLOSE     QSYSPRT
015700940325      *
015800940325     C                   ENDSR
015900940325      *-----------------------------------------------------*
016000940325      *  *INZSR: Operazioni iniziali
016100940325      *-----------------------------------------------------*
016200940325     C     *INZSR        BEGSR
016300940325      *-----------------------------------------------------*
016400940325     C     *ENTRY        PLIST
016500940325     C                   PARM                    KPJBA
016600060529     c                   movel     kpjbu         param
016700940325      *
016800940325     C                   MOVEL     *ALL'-'       TRAT            132
016900050203     C                   Z-ADD     1             CODUT             1 0
017000050203     c* dati utente
017100050203     c     *dtaara       define    §azute        azuteds
017200050203     c     *dtaara       define    §datiute      ddatiute
017300050203     C                   in(E)     *dtaara
017400050203     C                   IF        %Error  or  RSUT = *blanks
017500050203     C                   call      'TIBS34R'
017600050203     C                   parm                    Tibs34Ds
017700050203     C                   in        *dtaara
017800050203     c                   ENDIF
017900940325      *-----------------------------------------------------*
018000940325      *- Definizione delle chiavi                           *
018100940325     C     KEYTAB        KLIST
018200940325     C                   KFLD                    TBWKUT
018300940325     C                   KFLD                    TBWCOD
018400940325     C                   KFLD                    TBWKEY
018500940325      *
018600940325     C     KEYTB1        KLIST
018700940325     C                   KFLD                    TBWKUT
018800940325     C                   KFLD                    TBWCOD
018900940325      *
019000940325     C     KEYEV1        KLIST
019100940325     C                   KFLD                    EVWFLE
019200940325     C                   KFLD                    EVWCEV
019300940325     C                   KFLD                    EVWDEV
019400940325     C                   KFLD                    EVWHEV
019500940325      *
019600940325     C     KEYEVB        KLIST
019700940325     C                   KFLD                    EVWFLE
019800940325     C                   KFLD                    EVWCEV
019900940325      *-----------------------------------------------------*
020000940325      *   Definizione delle variabili
020100940325     C     *LIKE         DEFINE    TBLKUT        TBWKUT
020200940325     C     *LIKE         DEFINE    TBLCOD        TBWCOD
020300940325     C     *LIKE         DEFINE    TBLKEY        TBWKEY
020400940325     C     *LIKE         DEFINE    EVBFLE        EVWFLE
020500940325     C     *LIKE         DEFINE    EVBCEV        EVWCEV
020600940325     C     *LIKE         DEFINE    EVBDEV        EVWDEV
020700940325     C     *LIKE         DEFINE    EVBHEV        EVWHEV
020800940325     C     *LIKE         DEFINE    EVBAAS        STPAAS
020900940325     C     *LIKE         DEFINE    EVBCEV        STPCEV
021000940325     C     *LIKE         DEFINE    EVBFLE        STPFLE
021100940325     C     *LIKE         DEFINE    EVBLNP        STPLNP
021200940325     C     *LIKE         DEFINE    EVBNRS        STPNRS
021300940325     C     *LIKE         DEFINE    EVBNSP        STPNSP
021400940325     C     *LIKE         DEFINE    EVBHEV        STPHEV
021500940325     C     *LIKE         DEFINE    EVBCDU        STPCDU
021600940325      *-----------------------------------------------------*
021700060411      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
021800060411     c                   clear                   trul06ds
021900060411     C                   MOVEL     '£6'          D06COD
022000060529     C                   MOVEL     parmpo        D06KEY
022100060411     C                   MOVEL     TRul06ds      KPJBU
022200060411     C                   CALL      'TRUL06R'
022300060411     C                   PARM                    KPJBA
022400060411     C                   MOVEL     KPJBU         TRul06ds
022500020724     C                   Z-ADD     1             TBWKUT
022600940325      *----------------------------------------------------*
022700940325      *   Caricamento dati da tabelle 5D
022800940325     C                   MOVEL     '5D'          TBWCOD
022900060529     c                   movel(p)  parmpo        tbwkey
023000020903     C                   CLEAR                   DS5D
023100060505     c*
023200081007     C     KEYTAB        CHAIN(n)  TABEL00F                           70
023300020903     c                   if        not %found(tabel00f)
023400060529     C     parmpo        CHAIN     azorg01l
023500060505     c                   if        %found(azorg01l)
023600060505     c* se non lo trova imposta key = po di contabilità
023700060505     C                   MOVEL(p)  orgfc0        TBWKEY
023800081007     C     KEYTAB        CHAIN(n)  TABEL00F
023900020903     c                   end
024000060505     c                   end
024100060505     c                   if        %found(tabel00f)
024200940325     C                   MOVEL     TBLUNI        DS5D
024300940325     C                   END
024400940325     C                   Z-ADD     §5DDTE        EVWDEV
024500021003     C     §5DORE        DIV       100           EVWHEV
024600990831     C* CONTROLLO DATA EVENTO E CI METTO SECOLO
024700990831     C     §5DDTE        IFLT      400000
024800990901     C                   MOVEL     20            EVWDEV
024900990831     C                   ELSE
025000990901     C                   MOVEL     19            EVWDEV
025100990831     C                   END
025200940325      *-----------------------------------------------------*
025300940325      *   Caricamento dati da tabelle 2A
025400940325     C                   MOVEA     *BLANKS       C2A
025500940325     C                   MOVEA     *BLANKS       D2A
025600940325     C                   MOVEL     '2A'          TBWCOD
025700940325     C                   Z-ADD     1             X
025800081007     C     KEYTB1        CHAIN(n)  TABEL00F                           70
025900940325     C     *IN70         DOWEQ     '0'
026000080903     C     X             ANDLE     200
026100940325     C                   MOVEL     TBLUNI        DS2A
026200940325     C     §2ASTP        IFEQ      'S'
026300940325     C                   Z-ADD     1             X
026400940325     C                   MOVEL     TBLKEY        C2A(X)
026500940325     C                   MOVEL     §2ADES        D2A(X)
026600940325     C                   END
026700081007     C     KEYTB1        READE(n)  TABEL00F                               70
026800940325     C                   END
026900940325      *-----------------------------------------------------*
027000940325      *  Inizializzazione variabili
027100940325     C                   TIME                    ORA               6 0
027200960927     C                   TIME                    W0140            14 0
027300960927     C                   MOVE      W0140         UDATE8            8 0          GG/MM/AAAA
027400960927     C                   Z-ADD     UDATE8        WDATA
027500960927     C                   Z-ADD     WGG           WMMGG             4 0
027600960927     C                   MOVEL     WMM           WMMGG
027700960927     C                   Z-ADD     WMMGG         DATEU8            8 0          AAAA/MM/GG
027800960927     C                   MOVEL     WAA           DATEU8
027900940325      *-----------------------------------------------------*
028000940325      *- Apro file di stampa
028100940325     C                   Z-ADD     60            LUNCMD           15 5
028200940325     C                   CALL      'QCMDEXC'
028300940325     C                   PARM                    CMDP
028400940325     C                   PARM                    LUNCMD
028500940325     C                   OPEN      QSYSPRT
028600990804     C                   SETON                                        OF
028700940325      *-----------------------------------------------------*
028800940325     C                   ENDSR
028900940325      *-----------------------------------------------------*
029000940325     OQSYSPRT   E            TESTA          1 01
029100940325     O                       RSUT                20
029200940325     O                                           56 '***------------------'
029300940325     O                                              '------------------'
029400960927     O                                              '---------'
029500960927     O                                              '-----------------***'
029600960927     O                       ORA                120 '  :  :  '
029700960927     O                       UDATE8             132 '  /  /    '
029800940325     O          E            TESTA          1
029900940325     O                       KNMUS               10
030000940325     O                                           56 '** ELENCO SPEDIZIONI '
030100940325     O                                              'RIPRISTINATE DOPO '
030200960927     O                                              'CONTEGGI '
030300960927     O                                              'AUTOTRASPORTATORI **'
030400960927     O                       NOMPGM             122
030500940325     O          E            TESTA          2
030600960927     O                                           17 'PUNTO OPERATIVO :'
030700060529     O                       parmpo              21
030800940325     O                                           56 '***------------------'
030900940325     O                                              '------------------'
031000960927     O                                              '---------'
031100960927     O                                              '-----------------***'
031200940325     O          E            TESTA          1
031300940325     O                       TRAT               132
031400940325     O          E            TESTA          1
031500940329     O                                            8 'CAUS.'
031600940329     O                                           21 'DESCRIZIONE'
031700940329     O                                           58 'SPEDIZIONE'
031800940329     O                                           80 'UTENTE'
031900940329     O                                          101 'DATA RIPRI.'
032000940329     O                                          113 'ORA RIPRI.'
032100940329     O                                          123 'EFFET.DA'
032200940325     O          E            TESTA          1
032300940325     O                       TRAT               132
032400940325     O          E            DETTA          1
032500940329     O                       STPCEV               7
032600940329     O                       DESCEV              40
032700940329     O                       STPAAS        Z     48
032800940329     O                       STPLNP        Z     52
032900940329     O                       STPNRS        Z     55
033000940329     O                       STPNSP        Z     63
033100940329     O                       STPCDU        Z     74
033200940329     O                       STPDDU              85
033300990803     O                       STPDEV              98 '  /  /  '
033400940329     O                       STPHEV             111 '  :  :  '
033500940329     O                       STPFLE        Z    121
033600940325     O          E            FINE1       1
033700960927     O                                           46 'FINE STAMPA'
033800940325     O                                         +  1 'SPEDIZIONI RIPRISTINATE'
033900960927     O                                         +  1 'DOPO CONTEGGI'
034000960927     O                                         +  1 'AUTOTRASPORTATORI'
034100950118**  CMDP
034200950118OVRPRTF FILE(QSYSPRT) USRDTA(ELEN_RIPRI) SAVE(*YES)
