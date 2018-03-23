000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940926     H* FNLG72R *----------------------------------------------------*
000300900418     H*                                                              *
000400920522     H*   - STAMPA ELENCO GIACENZE APERTE PER CLIENTE -   -FILTRO-   *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700120928     FFNLG56D   CF   E             WORKSTN
000800920522     FCNACO00F  IF   E           K DISK
000900940926     H*--------------------------------------------------------------*
001000940926     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001100940926     D*---------------------------------------------------------------*
001200940926     D WLBDA8          DS
001300940926     D  G02DAT                 1      8  0
001400940926     D  G02INV                 9     16  0
001500940926     D  G02ERR                17     17
001600940926     D  G02TGI                18     22  0
001700120928     D tntaa1ds      E DS
001800900423     D TCUDS           DS
001900940926     D  F34                    3      4
002000900423     D  F56                    5      6
002100900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002200900423     D  TCU                  398    697
002300900418     D                                     DIM(50)
002400900423     D  KCU                  698    847P 0
002500900418     D                                     DIM(50)
002600900418     D                                     PACKEVEN
002700900418     D KPJBA         E DS
002800900423     C*---------------------------------------------------------------*
002900940926     C                   Z-ADD     0             V1CDA1
003000940926     C                   Z-ADD     0             V1CDA2
003100940926     C                   MOVEL     *BLANKS       V1CKSC
003200940926     C                   MOVEL     *BLANKS       V1DKSC
003300920522     C*
003400920522     C     *INKC         DOWEQ     '0'
003500940926     C                   EXSR      GESD01
003600920522     C                   END
003700920522     C*
003800920522     C                   SETON                                        LR
003900940926     C*-----------------------------------------------------*
004000940926     C*  GESTIONE PRIMA VIDEATA
004100940926     C*------------------------------------------------------*
004200940926     C     GESD01        BEGSR
004300940926     C*
004400120928     C                   EXFMT     LG56D01
004500940926     C                   SETOFF                                       404142
004600940926     C                   SETOFF                                       28
004700940926     C* Fine lavoro
004800940926     C     *INKC         IFEQ      '1'
004900940926     C                   GOTO      FINVD1
005000940926     C                   END
005100940926     C* Controlli
005200940926     C                   EXSR      CTR01
005300940926     C*  Se non ci sono errori sottometto la stampa
005400940926     C  N28
005500940926     CAN KF              DO
006300120928     C                   exsr      elabora
006700940926     C                   END
006800940926     C*
006900940926     C     FINVD1        ENDSR
007000940926     C*-----------------------------------------------------*
007100940926     C*  Controlli prima videata
007200940926     C*------------------------------------------------------*
007300940926     C     CTR01         BEGSR
007400940926     C* Controllo codice cliente
007500940926     C     '?'           SCAN      V1CKSC                                 21
007600940926     C   21              DO
007700940926     C                   SETON                                        99
007800940926     C                   Z-ADD     CODUT         CODUT§            1 0
007900940926     C                   Z-ADD     KCI           KCI§              4 0
008000940926     C                   MOVEL     *BLANKS       RICRAG           48
008100940926     C                   MOVEL     V1DKSC        RICRAG
008200940926     C                   Z-ADD     0             RICKSC            7 0
008300940926     C                   Z-ADD     0             PARSTA            1 0
008400940926     C                   CALL      'XALFA1'
008500940926     C                   PARM                    PARDUT
008600940926     C                   PARM                    CODUT§
008700940926     C                   PARM                    RICRAG
008800940926     C                   PARM                    KCI§
008900940926     C                   PARM                    RICKSC
009000940926     C                   PARM                    PARSTA
009100961016     C                   PARM                    PARFLR           90
009200940926     C     PARSTA        IFNE      -1
009300940926     C                   MOVE      RICKSC        V1CKSC
009400940926     C                   MOVEL     RICRAG        V1DKSC
009500940926     C                   ELSE
009600940926     C                   MOVE      *ZEROS        V1CKSC
009700940926     C                   MOVE      *BLANKS       V1DKSC
009800940926     C                   END
009900940926     C                   END
010000940926     C*
010100940926     C     V1CKSC        IFEQ      *ZEROS
010200940926     C     V1CKSC        OREQ      *BLANKS
010300940926     C                   SETON                                        4028
010400940926     C                   MOVEL     ERR(4)        $MSG
010500940926     C                   GOTO      FINCT1
010600940926     C                   END
010700940926     C*
010800940926     C                   MOVE      V1CKSC        CODCLI
010900940926     C                   MOVEL     *BLANKS       V1DKSC
011000940926     C     KACO          CHAIN     CNACO000                           40
011100940926     C     *IN40         IFEQ      '1'
011200940926     C     ACOFLG        ORNE      ' '
011300940926     C                   SETON                                        4028
011400940926     C                   MOVEL     ERR(1)        $MSG
011500940926     C                   GOTO      FINCT1
011600940926     C                   ELSE
011700940926     C                   MOVEL     ACORAG        V1DKSC
011800940926     C                   END
011801120928     c                   eval      itaa1caut = '§UTEGTC'
011802120928     c                   eval      itaa1ksc = v1cksc
011803120928     c                   call      'TNTAA1C'
011804120928     c                   parm                    tntaa1ds
011805120928     c                   if        otaa1fabi = 'N'
011806120928     C                   SETON                                        4028
011807120928     C                   MOVEL     ERR(5)        $MSG
011808120928     C                   GOTO      FINCT1
011809120928     c                   endif
011900940926     C* Controllo data iniziale
012000940926     C                   Z-ADD     V1CDA1        G02DAT
012100940926     C                   Z-ADD     0             G02INV
012200940926     C                   MOVEL     *BLANKS       G02ERR
012300940926     C                   CALL      'XSRDA8'
012400940926     C                   PARM                    WLBDA8
012500940926     C     G02ERR        IFEQ      '1'
012600940926     C                   SETON                                        4128
012700940926     C                   MOVEL     ERR(2)        $MSG
012800940926     C                   GOTO      FINCT1
012900940926     C                   ELSE
013000940926     C                   Z-ADD     G02INV        DATAD             8 0
013100940926     C                   Z-ADD     G02DAT        V1CDA1
013200940926     C                   END
013300940926     C* Controllo data finale
013400940926     C                   Z-ADD     V1CDA2        G02DAT
013500940926     C                   Z-ADD     0             G02INV
013600940926     C                   MOVEL     *BLANKS       G02ERR
013700940926     C                   CALL      'XSRDA8'
013800940926     C                   PARM                    WLBDA8
013900940926     C     G02ERR        IFEQ      '1'
014000940926     C                   SETON                                        4228
014100940926     C                   MOVEL     ERR(2)        $MSG
014200940926     C                   GOTO      FINCT1
014300940926     C                   ELSE
014400940926     C                   Z-ADD     G02INV        DATAA             8 0
014500940926     C                   Z-ADD     G02DAT        V1CDA2
014600940926     C                   END
014700940926     C* Controllo limiti
014800940926     C     DATAD         IFGT      DATAA
014900940926     C                   SETON                                        284142
015000940926     C                   MOVEL     ERR(3)        $MSG
015100940926     C                   END
015200940926     C*
015300940926     C     FINCT1        ENDSR
015301120928     c
015302120928     C*-----------------------------------------------------*
015303120928     c     elabora       begsr
015304120928     C*-----------------------------------------------------*
015305120928     c     v1cksc        setll     tigcp53l
015306120928     c                   do        *hival
015307120928     c     v1cksc        reade     tigcp53l
015308120928     c                   if        %eof(tigcp53l)
015309120928     c                   leave
015310120928     c                   endif
015311120928     c                   if        gcpfrg > 0    or
015312120928     c                             gcpdcg = 0    or
015313120928     c                             gcpdcg < datad or
015314120928     c                             gcpdcg > dataa or
015315120928     c                             gcpdis = 'D'
015316120928     c                   iter
015317120928     c                   endif
015318120928     c     kbol          chain     titas30c                           99
015319120928     c                   if        not *in99
015320120928     c                   movel     tasprd        tblkey
015321120928     c     ktab          chain     tabel00f
015322120928     c                   if        %found(tabel00f)
015323120928     c                   endif
015324120928     c                   endif
015325120928     c                   enddo
015326120928     c                   endsr
015400940926     C*-----------------------------------------------------*
015500940926     C*  Operazioni iniziali
015600940926     C*------------------------------------------------------*
015700940926     C     *INZSR        BEGSR
015800940926     C*
015900940926     C     *ENTRY        PLIST
016000940926     C                   PARM                    KPJBA
016100940926     C                   Z-ADD     1             CODUT
016200940926     C                   CALL      'X§PARUT'
016300940926     C                   PARM                    UTEDSE
016400940926     C*          CODUT     CABEQ-1        FINE
016500940926     C                   MOVEL     RAGUT         RSUT             20
016600940926     C*--- RICERCA CAPOCONTI------------------------------------------*
016700940926     C                   DO        50            X                 2 0
016800940926     C                   MOVE      TCU(X)        TCUDS
016900940926     C     F56           IFEQ      'CG'
017000940926     C     F34           ANDEQ     '01'
017100940926     C                   Z-ADD     KCU(X)        KCI               4 0
017200940926     C                   END
017300940926     C                   END
017400940926     C*---------------------------------------------------------------*
017500940926     C                   MOVEL     RAGUT         V1CRSU
017600940926     C                   MOVEL     RAGUT         PARDUT           30
017700940926     C                   CLEAR                   FNLG73
017800940926     C* CNACO
017900940926     C     KACO          KLIST
018000940926     C                   KFLD                    CODUT
018100940926     C                   KFLD                    KCI
018200940926     C                   KFLD                    CODCLI            7 0
018300940926     C*----------------------------------------------------*
018400940926     C                   ENDSR
018500940926**
018600940926Codice Cliente errato
018700940926Data errata
018800940926Limiti incongruenti: data limite iniziale maggiore finale
018900940926Immettere codice cliente
019000120928Utente non abilitato al lancio per questo codice cliente
