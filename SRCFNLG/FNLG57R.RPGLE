000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200121218     H* FNLG57R *----------------------------------------------------*
000300900418     H*                                                              *
000400121218     H*   -       Work file giacenze      PER CLIENTE -   -FILTRO-   *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700121218     FFNLG57D   CF   E             WORKSTN
000800920522     FCNACO00F  IF   E           K DISK
000900121005     Ftabel00f  IF   E           K DISK
001000121218     Fazorg01l  IF   E           K DISK
001100940926     H*--------------------------------------------------------------*
001200121218     D ERR             S             70    DIM(8) CTDATA PERRCD(1)
001300940926     D*---------------------------------------------------------------*
001400121001     D KPJBA         E DS
001500120928     D tntaa1ds      E DS
001600121218     D fnlg57ds      E DS
001700121005     D ds$3          E DS
001800900423     D TCUDS           DS
001900940926     D  F34                    3      4
002000900423     D  F56                    5      6
002100900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002200900423     D  TCU                  398    697
002300900418     D                                     DIM(50)
002400900423     D  KCU                  698    847P 0
002500900418     D                                     DIM(50)
002600900418     D                                     PACKEVEN
002700121001     D WLBDA8          DS
002800121001     D  G02DAT                 1      8  0
002900121001     D  G02INV                 9     16  0
003000121001     D  G02ERR                17     17
003100121001     D  G02TGI                18     22  0
003200121001     d dataisod        s               d
003300121001     d dataisoa        s               d
003400121001     d diffe           s              5  0
003500900423     C*---------------------------------------------------------------*
003600940926     C                   Z-ADD     0             V1CDA1
003700940926     C                   Z-ADD     0             V1CDA2
003800121218     C                   Z-ADD     0             V1Cfil
003900940926     C                   MOVEL     *BLANKS       V1CKSC
004000940926     C                   MOVEL     *BLANKS       V1DKSC
004100121219     C                   Z-ADD     99            V1Cnrs
004200920522     C*
004300920522     C     *INKC         DOWEQ     '0'
004400940926     C                   EXSR      GESD01
004500920522     C                   END
004600920522     C*
004700920522     C                   SETON                                        LR
004800940926     C*-----------------------------------------------------*
004900940926     C*  GESTIONE PRIMA VIDEATA
005000940926     C*------------------------------------------------------*
005100940926     C     GESD01        BEGSR
005200940926     C*
005300121218     C                   EXFMT     lg57D01
005400940926     C                   SETOFF                                       404142
005500121218     C                   SETOFF                                       2843
005600940926     C* Fine lavoro
005700940926     C     *INKC         IFEQ      '1'
005800940926     C                   GOTO      FINVD1
005900940926     C                   END
006000940926     C* Controlli
006100940926     C                   EXSR      CTR01
006200940926     C*  Se non ci sono errori sottometto la stampa
006300940926     C  N28
006400940926     CAN KF              DO
006500121218     c                   movel     v1cksc        d57ksc
006600121219     c                   z-add     V1Cnrs        D57nrs
006700121218     c                   z-add     datad         d57datad
006800121218     c                   z-add     dataa         d57dataa
006900121218     c                   z-add     v1cfil        d57fil
007000121218     c                   movel(p)  fnlg57ds      kpjbu
007100121218     c                   call      'FNLG57R1'
007200121003     c                   parm                    kpjba
007300121218     c
007400121218     c                   eval      *inkc='1'
007500940926     C                   END
007600940926     C*
007700940926     C     FINVD1        ENDSR
007800940926     C*-----------------------------------------------------*
007900940926     C*  Controlli prima videata
008000940926     C*------------------------------------------------------*
008100940926     C     CTR01         BEGSR
008200940926     C* Controllo codice cliente
008300940926     C     '?'           SCAN      V1CKSC                                 21
008400940926     C   21              DO
008500940926     C                   SETON                                        99
008600940926     C                   Z-ADD     CODUT         CODUT§            1 0
008700940926     C                   Z-ADD     KCI           KCI§              4 0
008800940926     C                   MOVEL     *BLANKS       RICRAG           48
008900940926     C                   MOVEL     V1DKSC        RICRAG
009000940926     C                   Z-ADD     0             RICKSC            7 0
009100940926     C                   Z-ADD     0             PARSTA            1 0
009200940926     C                   CALL      'XALFA1'
009300940926     C                   PARM                    PARDUT
009400940926     C                   PARM                    CODUT§
009500940926     C                   PARM                    RICRAG
009600940926     C                   PARM                    KCI§
009700940926     C                   PARM                    RICKSC
009800940926     C                   PARM                    PARSTA
009900961016     C                   PARM                    PARFLR           90
010000940926     C     PARSTA        IFNE      -1
010100940926     C                   MOVE      RICKSC        V1CKSC
010200940926     C                   MOVEL     RICRAG        V1DKSC
010300940926     C                   ELSE
010400940926     C                   MOVE      *ZEROS        V1CKSC
010500940926     C                   MOVE      *BLANKS       V1DKSC
010600940926     C                   END
010700940926     C                   END
010800940926     C*
010900940926     C     V1CKSC        IFEQ      *ZEROS
011000121219     c     V1Cnrs        andeq     *zeros
011100121219     C     V1CKSC        OREQ      *ZEROS
011200121219     c     V1Cnrs        andeq     99
011300940926     C     V1CKSC        OREQ      *BLANKS
011400121219     c     V1Cnrs        andeq     *zeros
011500121219     C     V1CKSC        OREQ      *BLANKS
011600121219     c     V1Cnrs        andeq     99
011700940926     C                   SETON                                        4028
011800940926     C                   MOVEL     ERR(4)        $MSG
011900940926     C                   GOTO      FINCT1
012000940926     C                   END
012100940926     C*
012200121219     c                   clear                   V1Dksc
012300121219     C     V1Cksc        ifgt      *zeros
012400940926     C                   MOVE      V1CKSC        CODCLI
012500940926     C                   MOVEL     *BLANKS       V1DKSC
012600940926     C     KACO          CHAIN     CNACO000                           40
012700940926     C     *IN40         IFEQ      '1'
012800940926     C     ACOFLG        ORNE      ' '
012900940926     C                   SETON                                        4028
013000940926     C                   MOVEL     ERR(1)        $MSG
013100940926     C                   GOTO      FINCT1
013200940926     C                   ELSE
013300940926     C                   MOVEL     ACORAG        V1DKSC
013400940926     C                   END
013500121219     c                   endif
013600121219      *
013700121001     c                   clear                   tntaa1ds
013800120928     c                   eval      itaa1caut = '§UTEGTC'
013900121001     c                   movel     v1cksc        itaa1ksc
014000121005     c                   movel(p)  tntaa1ds      kpjbu
014100120928     c                   call      'TNTAA1C'
014200121001     c                   parm                    kpjba
014300121005     c                   movel(p)  kpjbu         tntaa1ds
014400120928     c                   if        otaa1fabi = 'N'
014500120928     C                   SETON                                        4028
014600120928     C                   MOVEL     ERR(5)        $MSG
014700120928     C                   GOTO      FINCT1
014800120928     c                   endif
014900940926     C* Controllo data iniziale
015000940926     C                   Z-ADD     V1CDA1        G02DAT
015100940926     C                   Z-ADD     0             G02INV
015200940926     C                   MOVEL     *BLANKS       G02ERR
015300940926     C                   CALL      'XSRDA8'
015400940926     C                   PARM                    WLBDA8
015500940926     C     G02ERR        IFEQ      '1'
015600940926     C                   SETON                                        4128
015700940926     C                   MOVEL     ERR(2)        $MSG
015800940926     C                   GOTO      FINCT1
015900940926     C                   ELSE
016000940926     C                   Z-ADD     G02INV        DATAD             8 0
016100940926     C                   Z-ADD     G02DAT        V1CDA1
016200940926     C                   END
016300940926     C* Controllo data finale
016400940926     C                   Z-ADD     V1CDA2        G02DAT
016500940926     C                   Z-ADD     0             G02INV
016600940926     C                   MOVEL     *BLANKS       G02ERR
016700940926     C                   CALL      'XSRDA8'
016800940926     C                   PARM                    WLBDA8
016900940926     C     G02ERR        IFEQ      '1'
017000940926     C                   SETON                                        4228
017100940926     C                   MOVEL     ERR(2)        $MSG
017200940926     C                   GOTO      FINCT1
017300940926     C                   ELSE
017400940926     C                   Z-ADD     G02INV        DATAA             8 0
017500940926     C                   Z-ADD     G02DAT        V1CDA2
017600940926     C                   END
017700121005     C     DATAD         IFlt      §$3D10
017800121005     C                   SETON                                        284142
017900121005     C                   MOVEL     ERR(7)        $MSG
018000121005     C                   GOTO      FINCT1
018100121005     c                   endif
018200940926     C* Controllo limiti
018300940926     C     DATAD         IFGT      DATAA
018400940926     C                   SETON                                        284142
018500940926     C                   MOVEL     ERR(3)        $MSG
018600121001     c                   else
018700121001     c                   movel     datad         dataisod
018800121001     c                   movel     dataa         dataisoa
018900121001     c     dataisoa      subdur    dataisod      diffe:*d
019000121218     c**                 if        diffe > 93
019100121218     C**                 seton                                        284142
019200121218     C**                 movel     ERR(6)        $MSG
019300121218     c**                 endif
019400940926     C                   END
019500121001     c
019600121218     c                   if        v1cfil>0
019700121218     c     v1cfil        chain     azorg01l
019800121218     c                   if        not %found(azorg01l) or orgfva<>' '
019900121218     c                             or (orgfag<>'F' and orgfag<>'A')
020000121218     C                   seton                                        2843
020100121218     C                   movel     ERR(6)        $MSG
020200121218     c                   endif
020300121218     c                   endif
020400940926     C*
020500940926     C     FINCT1        ENDSR
020600120928     c
020700940926     C*-----------------------------------------------------*
020800940926     C*  Operazioni iniziali
020900940926     C*------------------------------------------------------*
021000940926     C     *INZSR        BEGSR
021100940926     C*
021200940926     C     *ENTRY        PLIST
021300940926     C                   PARM                    KPJBA
021400940926     C                   Z-ADD     1             CODUT
021500940926     C                   CALL      'X§PARUT'
021600940926     C                   PARM                    UTEDSE
021700940926     C*          CODUT     CABEQ-1        FINE
021800940926     C                   MOVEL     RAGUT         RSUT             20
021900940926     C*--- RICERCA CAPOCONTI------------------------------------------*
022000940926     C                   DO        50            X                 2 0
022100940926     C                   MOVE      TCU(X)        TCUDS
022200940926     C     F56           IFEQ      'CG'
022300940926     C     F34           ANDEQ     '01'
022400940926     C                   Z-ADD     KCU(X)        KCI               4 0
022500940926     C                   END
022600940926     C                   END
022700940926     C*---------------------------------------------------------------*
022800940926     C                   MOVEL     RAGUT         V1CRSU
022900940926     C                   MOVEL     RAGUT         PARDUT           30
023000940926     C* CNACO
023100940926     C     KACO          KLIST
023200940926     C                   KFLD                    CODUT
023300940926     C                   KFLD                    KCI
023400940926     C                   KFLD                    CODCLI            7 0
023500121005     c     ktab          klist
023600121005     c                   kfld                    TBLKUT
023700121005     c                   kfld                    TBLCOD
023800121005     c                   clear                   ds$3
023900121005     c                   z-add     1             tblkut
024000121005     c                   move      '$3'          tblcod
024100121005     c     ktab          chain     tabel00f
024200121005     c                   if        %found(tabel00f)
024300121005     c                   movel     tbluni        ds$3
024400121005     c                   endif
024500121005     C*
024600940926     C                   ENDSR
024700940926**
024800940926Codice Cliente errato
024900940926Data errata
025000940926Limiti incongruenti: data limite iniziale maggiore finale
025100121219Immettere codice cliente  o  numero serie
025200120928Utente non abilitato al lancio per questo codice cliente
025300121001Limiti incongruenti: date distanti oltre un mese
025400121005Data iniziale non congruente antecedente alla pulizia dei dati
025500121218filiale errata
