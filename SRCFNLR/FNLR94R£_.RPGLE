000100940325     H DECEDIT('0,') DATEDIT(*YMD.)
000200021003     H OPTION(*NODEBUGIO)
000300941129      * FNLR94R *----------------------------------------------------*
000400940325      *-----------------*                                            *
000500940325      * ARRIVI:STAMPA SPEDIZIONI RIPRISTINATE DOPO CONTEGGI          *
000600940325      *--------------------------------------------------------------*
000700990803     FFNEVB03L  IF   E           K DISK
000800940329      *------------
000900940325     FTABEL00F  UF A E           K DISK
001000940325      *------------
001100940325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200940325     F                                     USROPN
001300940325      *--------------------------------------------------------------*
001400940325      *  SCHIERE
001500940325      *--------------------------------------------------------------*
001600940325     D C2A             S              3    DIM(20)                              COD.CAUS.EVENTI
001700940325     D D2A             S             30    DIM(20)                              DES.CAUS.EVENTI
001800940325     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
001900940325      *--------------------------------------------------------------*
002000940325      *  DS
002100940325      *--------------------------------------------------------------*
002200940325     D KPJBA         E DS
002300940325      *
002400050203     D ddatiute      e ds
002500050203     D azuteds       e ds                  extname(AZUTE00F)
002600050203     D tibs34ds      E DS                  inz
002700040922     D TRUL72DS      E DS
002800040922     D  £1                            3  0 DESCEND
002900040922     D                                     DIM(60)
003000040922     D                                     OVERLAY(O72Lia)
003100021119     d Tibs36Ds      e ds
003200960927     D*
003300960927     D                 DS
003400960927     D  WGG                    1      2  0
003500960927     D  WMM                    3      4  0
003600960927     D  WAA                    5      8  0
003700960927     D  WDATA                  1      8  0
003800960927     D*
003900940325     D DS2A          E DS
004000940325      *
004100940325     D DS5D          E DS
004200940325     D                SDS
004300940325     D  NOMPGM                 1     10
004400940325      *--------------------------------------------------------------*
004500940325      *  CICLO PRINCIPALE
004600940325      *--------------------------------------------------------------*
004700021120      *   Loop filiali: per ogni filiale caricata in £1 vado a vedere
004800940325      *   se ci sono eventi da stampare
004900940325     C                   Z-ADD     1             X                 2 0
005000040922     C     X             DOWLE     60
005100021120     C     £1(X)         ANDNE     0
005200021120     C                   Z-ADD     £1(X)         EVWFLE
005300021120     C                   Z-ADD     £1(X)         STPFLE
005400940325      *   Loop causali: per ogni causale carica in C2A procedo alla
005500940325      *   stampa e alla decodifica
005600940325     C                   Z-ADD     1             Y                 2 0
005700940325     C     Y             DOWLE     20
005800940325     C     C2A(Y)        ANDNE     *BLANKS
005900940325     C*  Posizionamento iniziale  x causale
006000940325     C                   EXSR      SETCEV
006100940325     C*  Loop fino a fine causale
006200940325     C     *IN80         DOWEQ     '0'
006300940325     C                   EXSR      REDCEV
006400940325     C                   END
006500940325      *  Causale successiva
006600940325     C                   ADD       1             Y
006700940325     C                   END
006800940325      *  Filiale Successiva
006900940325     C                   ADD       1             X
007000940325     C                   END
007100940325      *  Fine pgm: Aggiorno data/ora stampa eventi
007200940325     C                   EXSR      AGGTAB
007300940325     C                   SETON                                        LR
007400940325      *-----------------------------------------------------*
007500940325      *  SETCEV: Posizionamento iniziale ad inizio causale
007600940325      *-----------------------------------------------------*
007700940325     C     SETCEV        BEGSR
007800940325      *
007900940325     C                   MOVEL     C2A(Y)        EVWCEV
008000940325     C                   MOVEL     C2A(Y)        STPCEV
008100940325     C                   MOVEL     D2A(Y)        DESCEV           30
008200990803     C     KEYEV1        SETGT     FNEVB03L
008300990803     C     KEYEVB        READE     FNEVB03L                               80
008400940325      *
008500940325     C                   ENDSR
008600940325      *-----------------------------------------------------*
008700940325      *  REDCEV: Lettura e stampa eventi
008800940325      *-----------------------------------------------------*
008900940325     C     REDCEV        BEGSR
009000940325      *
009100940325     C     EVBDEV        IFNE      STPDEV
009200940325      * Giro la data
009300940325     C                   Z-ADD     EVBDEV        STPDEV
009400990803     C                   MOVEL     EVBDEV        AA4               4 0
009500990803     C                   MOVE      AA4           AA                2 0
009600940325     C                   MOVE      EVBDEV        GG                2 0
009700990803     C                   MOVE      AA            STPDEV            6 0
009800940325     C                   MOVEL     GG            STPDEV
009900940325     C                   END
010000940325      *
010100940325     C                   Z-ADD     EVBAAS        STPAAS
010200940325     C                   Z-ADD     EVBLNP        STPLNP
010300940325     C                   Z-ADD     EVBNRS        STPNRS
010400940325     C                   Z-ADD     EVBNSP        STPNSP
010500940325     C                   Z-ADD     EVBHEV        STPHEV
010600940325     C                   Z-ADD     EVBCDU        STPCDU
010700021003
010800021119     C                   CLEAR                   Tibs36Ds
010900021119     C                   EVAL      I36POU = EVBFLE
011000021119     C                   EVAL      I36CoU = EVBCDU
011100021119     C                   CALL      'TIBS36R'
011200021119     C                   PARM                    Tibs36Ds
011300021003
011400021119     C                   IF        O36Err = *BLANK
011500021119     C                   MOVEL     O36UTE        STPDDU           10
011600021003     C                   ELSE
011700021003     C                   MOVEL     *ALL'?'       STPDDU
011800021003     C                   ENDIF
011900021003
012000940325      *  Stampa dati evento
012100940325     C   OF              EXCEPT    TESTA
012200940325     C                   EXCEPT    DETTA
012300991118     C                   MOVEL     'S'           WSTP              1
012400940325      *  Lettura successiva
012500990803     C     KEYEVB        READE     FNEVB03L                               80
012600940325      *
012700940325     C                   ENDSR
012800940325      *-----------------------------------------------------*
012900940325      *  AGGTAB: Aggiornamanto tabella 5d
013000940325      *-----------------------------------------------------*
013100940325     C     AGGTAB        BEGSR
013200940325      *
013300940325      *   Caricamento dati da tabelle 5D
013400940325     C                   MOVEL     '5D'          TBWCOD
013500050203     c                   movel(p)  dutpou        tbwkey
013600020902     c* prova con key = al po, lo trova solo se TABEL è già in GRU
013700940325     C     KEYTAB        CHAIN     TABEL00F                           70
013800020902     c                   if        not %found(tabel00f)
013900020902     c* se non lo trova imposta key = 1
014000020902     C                   MOVEL(p)  1             TBWKEY
014100020902     C     KEYTAB        CHAIN     TABEL00F                           70
014200020902     c                   end
014300940325     C   70              CLEAR                   TABEL
014400960927     C                   Z-ADD     DATEU8        §5DDTE
014500940325     C                   TIME                    §5DORE
014600940325     C                   MOVEL     DS5D          TBLUNI
014700020902     c* se non lo trova lo crea con nuova key di tabel in GRU
014800940325     C     *IN70         IFEQ      '1'
014900940325     C                   Z-ADD     1             TBLKUT
015000940325     C                   MOVEL     '5D'          TBLCOD
015100050203     c                   movel(p)  dutpou        tblkey
015200940325     C                   WRITE     TABEL
015300940325     C                   ELSE
015400940325     C                   UPDATE    TABEL
015500940325     C                   END
015600940325      *  Fine Stampa
015700991118     C     WSTP          IFEQ      'S'
015800940325     C   OF              EXCEPT    TESTA
015900940325     C                   EXCEPT    FINE1
016000991118     C                   END
016100940325     C                   CLOSE     QSYSPRT
016200940325      *
016300940325     C                   ENDSR
016400940325      *-----------------------------------------------------*
016500940325      *  *INZSR: Operazioni iniziali
016600940325      *-----------------------------------------------------*
016700940325     C     *INZSR        BEGSR
016800940325      *-----------------------------------------------------*
016900940325     C     *ENTRY        PLIST
017000940325     C                   PARM                    KPJBA
017100940325      *
017200940325     C                   MOVEL     *ALL'-'       TRAT            132
017300050203     C                   Z-ADD     1             CODUT             1 0
017400050203     c* dati utente
017500050203     c     *dtaara       define    §azute        azuteds
017600050203     c     *dtaara       define    §datiute      ddatiute
017700050203     C                   in(E)     *dtaara
017800050203     C                   IF        %Error  or  RSUT = *blanks
017900050203     C                   call      'TIBS34R'
018000050203     C                   parm                    Tibs34Ds
018100050203     C                   in        *dtaara
018200050203     c                   ENDIF
018300940325      *-----------------------------------------------------*
018400940325      *- Definizione delle chiavi                           *
018500940325     C     KEYTAB        KLIST
018600940325     C                   KFLD                    TBWKUT
018700940325     C                   KFLD                    TBWCOD
018800940325     C                   KFLD                    TBWKEY
018900940325      *
019000940325     C     KEYTB1        KLIST
019100940325     C                   KFLD                    TBWKUT
019200940325     C                   KFLD                    TBWCOD
019300940325      *
019400940325     C     KEYEV1        KLIST
019500940325     C                   KFLD                    EVWFLE
019600940325     C                   KFLD                    EVWCEV
019700940325     C                   KFLD                    EVWDEV
019800940325     C                   KFLD                    EVWHEV
019900940325      *
020000940325     C     KEYEVB        KLIST
020100940325     C                   KFLD                    EVWFLE
020200940325     C                   KFLD                    EVWCEV
020300940325      *-----------------------------------------------------*
020400940325      *   Definizione delle variabili
020500940325     C     *LIKE         DEFINE    TBLKUT        TBWKUT
020600940325     C     *LIKE         DEFINE    TBLCOD        TBWCOD
020700940325     C     *LIKE         DEFINE    TBLKEY        TBWKEY
020800940325     C     *LIKE         DEFINE    EVBFLE        EVWFLE
020900940325     C     *LIKE         DEFINE    EVBCEV        EVWCEV
021000940325     C     *LIKE         DEFINE    EVBDEV        EVWDEV
021100940325     C     *LIKE         DEFINE    EVBHEV        EVWHEV
021200940325     C     *LIKE         DEFINE    EVBAAS        STPAAS
021300940325     C     *LIKE         DEFINE    EVBCEV        STPCEV
021400940325     C     *LIKE         DEFINE    EVBFLE        STPFLE
021500940325     C     *LIKE         DEFINE    EVBLNP        STPLNP
021600940325     C     *LIKE         DEFINE    EVBNRS        STPNRS
021700940325     C     *LIKE         DEFINE    EVBNSP        STPNSP
021800940325     C     *LIKE         DEFINE    EVBHEV        STPHEV
021900940325     C     *LIKE         DEFINE    EVBCDU        STPCDU
022000940325      *-----------------------------------------------------*
022100040922     c* carico I PO DI CONTABILITÀ
022200040922     C                   CLEAR                   TRUL72DS
022300050203     C                   MOVEL     dutpou        I72POU
022400040922     C                   EVAL      I72TLa = 'L'
022500040922     C                   EVAL      KPJBU = TRUL72DS
022600040922     C                   CALL      'TRUL72R'
022700040922     C                   PARM                    KPJBA
022800040922     C                   EVAL      TRUL72DS = KPJBU
022900040922     c                   sorta     £1
023000020724     C                   Z-ADD     1             TBWKUT
023100940325      *----------------------------------------------------*
023200940325      *   Caricamento dati da tabelle 5D
023300940325     C                   MOVEL     '5D'          TBWCOD
023400050203     c                   movel(p)  dutpou        tbwkey
023500020903     C                   CLEAR                   DS5D
023600020903     c* prova con key = al po, lo trova solo se TABEL è già in GRU
023700020903     C     KEYTAB        CHAIN     TABEL00F                           70
023800020903     c                   if        not %found(tabel00f)
023900020903     C                   MOVEL(p)  1             TBWKEY
024000940325     C     KEYTAB        CHAIN     TABEL00F                           70
024100020903     c                   end
024200940325     C     *IN70         IFEQ      '0'
024300940325     C                   MOVEL     TBLUNI        DS5D
024400940325     C                   END
024500940325     C                   Z-ADD     §5DDTE        EVWDEV
024600021003     C     §5DORE        DIV       100           EVWHEV
024700990831     C* CONTROLLO DATA EVENTO E CI METTO SECOLO
024800990831     C     §5DDTE        IFLT      400000
024900990901     C                   MOVEL     20            EVWDEV
025000990831     C                   ELSE
025100990901     C                   MOVEL     19            EVWDEV
025200990831     C                   END
025300940325      *-----------------------------------------------------*
025400940325      *   Caricamento dati da tabelle 2A
025500940325     C                   MOVEA     *BLANKS       C2A
025600940325     C                   MOVEA     *BLANKS       D2A
025700940325     C                   MOVEL     '2A'          TBWCOD
025800940325     C                   Z-ADD     1             X
025900940325     C     KEYTB1        CHAIN     TABEL00F                           70
026000940325     C     *IN70         DOWEQ     '0'
026100940325     C     X             ANDLE     20
026200940325     C                   MOVEL     TBLUNI        DS2A
026300940325     C     §2ASTP        IFEQ      'S'
026400940325     C                   Z-ADD     1             X
026500940325     C                   MOVEL     TBLKEY        C2A(X)
026600940325     C                   MOVEL     §2ADES        D2A(X)
026700940325     C                   END
026800940325     C     KEYTB1        READE     TABEL00F                               70
026900940325     C                   END
027000940325      *-----------------------------------------------------*
027100940325      *  Inizializzazione variabili
027200940325     C                   TIME                    ORA               6 0
027300960927     C                   TIME                    W0140            14 0
027400960927     C                   MOVE      W0140         UDATE8            8 0          GG/MM/AAAA
027500960927     C                   Z-ADD     UDATE8        WDATA
027600960927     C                   Z-ADD     WGG           WMMGG             4 0
027700960927     C                   MOVEL     WMM           WMMGG
027800960927     C                   Z-ADD     WMMGG         DATEU8            8 0          AAAA/MM/GG
027900960927     C                   MOVEL     WAA           DATEU8
028000940325      *-----------------------------------------------------*
028100940325      *- Apro file di stampa
028200940325     C                   Z-ADD     60            LUNCMD           15 5
028300940325     C                   CALL      'QCMDEXC'
028400940325     C                   PARM                    CMDP
028500940325     C                   PARM                    LUNCMD
028600940325     C                   OPEN      QSYSPRT
028700990804     C                   SETON                                        OF
028800940325      *-----------------------------------------------------*
028900940325     C                   ENDSR
029000940325      *-----------------------------------------------------*
029100940325     OQSYSPRT   E            TESTA          1 01
029200940325     O                       RSUT                20
029300940325     O                                           56 '***------------------'
029400940325     O                                              '------------------'
029500960927     O                                              '---------'
029600960927     O                                              '-----------------***'
029700960927     O                       ORA                120 '  :  :  '
029800960927     O                       UDATE8             132 '  /  /    '
029900940325     O          E            TESTA          1
030000940325     O                       KNMUS               10
030100940325     O                                           56 '** ELENCO SPEDIZIONI '
030200940325     O                                              'RIPRISTINATE DOPO '
030300960927     O                                              'CONTEGGI '
030400960927     O                                              'AUTOTRASPORTATORI **'
030500960927     O                       NOMPGM             122
030600940325     O          E            TESTA          2
030700960927     O                                           17 'PUNTO OPERATIVO :'
030800050203     O                       dutpou              21
030900940325     O                                           56 '***------------------'
031000940325     O                                              '------------------'
031100960927     O                                              '---------'
031200960927     O                                              '-----------------***'
031300940325     O          E            TESTA          1
031400940325     O                       TRAT               132
031500940325     O          E            TESTA          1
031600940329     O                                            8 'CAUS.'
031700940329     O                                           21 'DESCRIZIONE'
031800940329     O                                           58 'SPEDIZIONE'
031900940329     O                                           80 'UTENTE'
032000940329     O                                          101 'DATA RIPRI.'
032100940329     O                                          113 'ORA RIPRI.'
032200940329     O                                          123 'EFFET.DA'
032300940325     O          E            TESTA          1
032400940325     O                       TRAT               132
032500940325     O          E            DETTA          1
032600940329     O                       STPCEV               7
032700940329     O                       DESCEV              40
032800940329     O                       STPAAS        Z     48
032900940329     O                       STPLNP        Z     52
033000940329     O                       STPNRS        Z     55
033100940329     O                       STPNSP        Z     63
033200940329     O                       STPCDU        Z     74
033300940329     O                       STPDDU              85
033400990803     O                       STPDEV              98 '  /  /  '
033500940329     O                       STPHEV             111 '  :  :  '
033600940329     O                       STPFLE        Z    121
033700940325     O          E            FINE1       1
033800960927     O                                           46 'FINE STAMPA'
033900940325     O                                         +  1 'SPEDIZIONI RIPRISTINATE'
034000960927     O                                         +  1 'DOPO CONTEGGI'
034100960927     O                                         +  1 'AUTOTRASPORTATORI'
034200950118**  CMDP
034300950118OVRPRTF FILE(QSYSPRT) USRDTA(ELEN_RIPRI) SAVE(*YES)
