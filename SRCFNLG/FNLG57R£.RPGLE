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
004100920522     C*
004200920522     C     *INKC         DOWEQ     '0'
004300940926     C                   EXSR      GESD01
004400920522     C                   END
004500920522     C*
004600920522     C                   SETON                                        LR
004700940926     C*-----------------------------------------------------*
004800940926     C*  GESTIONE PRIMA VIDEATA
004900940926     C*------------------------------------------------------*
005000940926     C     GESD01        BEGSR
005100940926     C*
005200121218     C                   EXFMT     lg57D01
005300940926     C                   SETOFF                                       404142
005400121218     C                   SETOFF                                       2843
005500940926     C* Fine lavoro
005600940926     C     *INKC         IFEQ      '1'
005700940926     C                   GOTO      FINVD1
005800940926     C                   END
005900940926     C* Controlli
006000940926     C                   EXSR      CTR01
006100940926     C*  Se non ci sono errori sottometto la stampa
006200940926     C  N28
006300940926     CAN KF              DO
006400121218     c                   movel     v1cksc        d57ksc
006500121218     c                   z-add     datad         d57datad
006600121218     c                   z-add     dataa         d57dataa
006700121218     c                   z-add     v1cfil        d57fil
006800121218     c                   movel(p)  fnlg57ds      kpjbu
006900121218     c                   call      'FNLG57R1'
007000121003     c                   parm                    kpjba
007100121218     c
007200121218     c                   eval      *inkc='1'
007300940926     C                   END
007400940926     C*
007500940926     C     FINVD1        ENDSR
007600940926     C*-----------------------------------------------------*
007700940926     C*  Controlli prima videata
007800940926     C*------------------------------------------------------*
007900940926     C     CTR01         BEGSR
008000940926     C* Controllo codice cliente
008100940926     C     '?'           SCAN      V1CKSC                                 21
008200940926     C   21              DO
008300940926     C                   SETON                                        99
008400940926     C                   Z-ADD     CODUT         CODUT§            1 0
008500940926     C                   Z-ADD     KCI           KCI§              4 0
008600940926     C                   MOVEL     *BLANKS       RICRAG           48
008700940926     C                   MOVEL     V1DKSC        RICRAG
008800940926     C                   Z-ADD     0             RICKSC            7 0
008900940926     C                   Z-ADD     0             PARSTA            1 0
009000940926     C                   CALL      'XALFA1'
009100940926     C                   PARM                    PARDUT
009200940926     C                   PARM                    CODUT§
009300940926     C                   PARM                    RICRAG
009400940926     C                   PARM                    KCI§
009500940926     C                   PARM                    RICKSC
009600940926     C                   PARM                    PARSTA
009700961016     C                   PARM                    PARFLR           90
009800940926     C     PARSTA        IFNE      -1
009900940926     C                   MOVE      RICKSC        V1CKSC
010000940926     C                   MOVEL     RICRAG        V1DKSC
010100940926     C                   ELSE
010200940926     C                   MOVE      *ZEROS        V1CKSC
010300940926     C                   MOVE      *BLANKS       V1DKSC
010400940926     C                   END
010500940926     C                   END
010600940926     C*
010700940926     C     V1CKSC        IFEQ      *ZEROS
010800940926     C     V1CKSC        OREQ      *BLANKS
010900940926     C                   SETON                                        4028
011000940926     C                   MOVEL     ERR(4)        $MSG
011100940926     C                   GOTO      FINCT1
011200940926     C                   END
011300940926     C*
011400940926     C                   MOVE      V1CKSC        CODCLI
011500940926     C                   MOVEL     *BLANKS       V1DKSC
011600940926     C     KACO          CHAIN     CNACO000                           40
011700940926     C     *IN40         IFEQ      '1'
011800940926     C     ACOFLG        ORNE      ' '
011900940926     C                   SETON                                        4028
012000940926     C                   MOVEL     ERR(1)        $MSG
012100940926     C                   GOTO      FINCT1
012200940926     C                   ELSE
012300940926     C                   MOVEL     ACORAG        V1DKSC
012400940926     C                   END
012500121001     c                   clear                   tntaa1ds
012600120928     c                   eval      itaa1caut = '§UTEGTC'
012700121001     c                   movel     v1cksc        itaa1ksc
012800121005     c                   movel(p)  tntaa1ds      kpjbu
012900120928     c                   call      'TNTAA1C'
013000121001     c                   parm                    kpjba
013100121005     c                   movel(p)  kpjbu         tntaa1ds
013200120928     c                   if        otaa1fabi = 'N'
013300120928     C                   SETON                                        4028
013400120928     C                   MOVEL     ERR(5)        $MSG
013500120928     C                   GOTO      FINCT1
013600120928     c                   endif
013700940926     C* Controllo data iniziale
013800940926     C                   Z-ADD     V1CDA1        G02DAT
013900940926     C                   Z-ADD     0             G02INV
014000940926     C                   MOVEL     *BLANKS       G02ERR
014100940926     C                   CALL      'XSRDA8'
014200940926     C                   PARM                    WLBDA8
014300940926     C     G02ERR        IFEQ      '1'
014400940926     C                   SETON                                        4128
014500940926     C                   MOVEL     ERR(2)        $MSG
014600940926     C                   GOTO      FINCT1
014700940926     C                   ELSE
014800940926     C                   Z-ADD     G02INV        DATAD             8 0
014900940926     C                   Z-ADD     G02DAT        V1CDA1
015000940926     C                   END
015100940926     C* Controllo data finale
015200940926     C                   Z-ADD     V1CDA2        G02DAT
015300940926     C                   Z-ADD     0             G02INV
015400940926     C                   MOVEL     *BLANKS       G02ERR
015500940926     C                   CALL      'XSRDA8'
015600940926     C                   PARM                    WLBDA8
015700940926     C     G02ERR        IFEQ      '1'
015800940926     C                   SETON                                        4228
015900940926     C                   MOVEL     ERR(2)        $MSG
016000940926     C                   GOTO      FINCT1
016100940926     C                   ELSE
016200940926     C                   Z-ADD     G02INV        DATAA             8 0
016300940926     C                   Z-ADD     G02DAT        V1CDA2
016400940926     C                   END
016500121005     C     DATAD         IFlt      §$3D10
016600121005     C                   SETON                                        284142
016700121005     C                   MOVEL     ERR(7)        $MSG
016800121005     C                   GOTO      FINCT1
016900121005     c                   endif
017000940926     C* Controllo limiti
017100940926     C     DATAD         IFGT      DATAA
017200940926     C                   SETON                                        284142
017300940926     C                   MOVEL     ERR(3)        $MSG
017400121001     c                   else
017500121001     c                   movel     datad         dataisod
017600121001     c                   movel     dataa         dataisoa
017700121001     c     dataisoa      subdur    dataisod      diffe:*d
017800121218     c**                 if        diffe > 93
017900121218     C**                 seton                                        284142
018000121218     C**                 movel     ERR(6)        $MSG
018100121218     c**                 endif
018200940926     C                   END
018300121001     c
018400121218     c                   if        v1cfil>0
018500121218     c     v1cfil        chain     azorg01l
018600121218     c                   if        not %found(azorg01l) or orgfva<>' '
018700121218     c                             or (orgfag<>'F' and orgfag<>'A')
018800121218     C                   seton                                        2843
018900121218     C                   movel     ERR(6)        $MSG
019000121218     c                   endif
019100121218     c                   endif
019200940926     C*
019300940926     C     FINCT1        ENDSR
019400120928     c
019500940926     C*-----------------------------------------------------*
019600940926     C*  Operazioni iniziali
019700940926     C*------------------------------------------------------*
019800940926     C     *INZSR        BEGSR
019900940926     C*
020000940926     C     *ENTRY        PLIST
020100940926     C                   PARM                    KPJBA
020200940926     C                   Z-ADD     1             CODUT
020300940926     C                   CALL      'X§PARUT'
020400940926     C                   PARM                    UTEDSE
020500940926     C*          CODUT     CABEQ-1        FINE
020600940926     C                   MOVEL     RAGUT         RSUT             20
020700940926     C*--- RICERCA CAPOCONTI------------------------------------------*
020800940926     C                   DO        50            X                 2 0
020900940926     C                   MOVE      TCU(X)        TCUDS
021000940926     C     F56           IFEQ      'CG'
021100940926     C     F34           ANDEQ     '01'
021200940926     C                   Z-ADD     KCU(X)        KCI               4 0
021300940926     C                   END
021400940926     C                   END
021500940926     C*---------------------------------------------------------------*
021600940926     C                   MOVEL     RAGUT         V1CRSU
021700940926     C                   MOVEL     RAGUT         PARDUT           30
021800940926     C* CNACO
021900940926     C     KACO          KLIST
022000940926     C                   KFLD                    CODUT
022100940926     C                   KFLD                    KCI
022200940926     C                   KFLD                    CODCLI            7 0
022300121005     c     ktab          klist
022400121005     c                   kfld                    TBLKUT
022500121005     c                   kfld                    TBLCOD
022600121005     c                   clear                   ds$3
022700121005     c                   z-add     1             tblkut
022800121005     c                   move      '$3'          tblcod
022900121005     c     ktab          chain     tabel00f
023000121005     c                   if        %found(tabel00f)
023100121005     c                   movel     tbluni        ds$3
023200121005     c                   endif
023300121005     C*
023400940926     C                   ENDSR
023500940926**
023600940926Codice Cliente errato
023700940926Data errata
023800940926Limiti incongruenti: data limite iniziale maggiore finale
023900940926Immettere codice cliente
024000120928Utente non abilitato al lancio per questo codice cliente
024100121001Limiti incongruenti: date distanti oltre un mese
024200121005Data iniziale non congruente antecedente alla pulizia dei dati
024300121218filiale errata
