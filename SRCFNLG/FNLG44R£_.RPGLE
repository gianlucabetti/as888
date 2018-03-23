000100930812     H DECEDIT('0,') DATEDIT(*DMY/)
000200930812     H*---------*
000300930812     H* TRST40R *-----------------------------------------------------*
000400930812     H*---------*                                                     *
000500930812     H*              - STATISTICA CONTROLLO UFF. GIACENZE             *
000600930812     F*---------------------------------------------------------------*
000700930812     F* DATA BASE                                                     *
000800930812     F*---------------------------------------------------------------*
000900930812     FTABEL00F  IF   E           K DISK
001000010921     FFLFG765L  IF   E           K DISK
001100941013     FFNLG44D   CF   E             WORKSTN
001200930812     D*---------------------------------------------------------------*
001300930812     D* SCHIERA
001400930812     D*---------------------------------------------------------------*
001500930812     D IN              S              1    DIM(99)
001600020430     D*LIN             S              3    DIM(29)
001700020430     D L3              S              3  0 DIM(30)
001800930812     D*---------------------------------------------------------------*
001900930812     D* DS
002000930812     D*---------------------------------------------------------------*
002100930812     D KPJBA         E DS
002200930812     D  LIBSYS                92    100
002300930812     D*------------------
002400930812     D* DS LANCIO PGM BATCH
002500930812     D*------------------
002600941013     D FNLG44        E DS                  EXTNAME(FNLG44DS)
002700941013     D FNLG45        E DS                  EXTNAME(FNLG45DS)
002701020430     d trul06ds      e ds
002702020430     d  lin                    1     90  0 dim(30)
002800930812     D                 DS
002900941013     D  AA2                    1      2  0
003000931125     D  PERIO                  3      4  0
003100931125     D  DSPER                  1      4  0
003200930812     D*------------------
003300930812     D* DS PER CONVERSIONE DATE
003400930812     D*------------------
003500941013     D WLBDA8          DS
003600941013     D  G02DAT                 1      8  0
003700941013     D  G02INV                 9     16  0
003800941013     D  G02ERR                17     17
003900941013     D  G02TGI                18     22  0
004000930812     D*------------------
004100930812     D* DS RICERCA RAG. SOCIALE
004200930812     D*------------------
004300930812     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400930812     D CNCR80        E DS
004500930812     D*------------------
004600930812     D* DS PERIODO DI ELABORAZIONE
004700930812     D*------------------
004800930812     D DS2I          E DS
004900941013     C*------------------------------------------------------------------
005000930812     C** IMPOSTAZIONE CAMPI VIDEO
005100941013     C                   TIME                    WHHDAT           14 0
005200941013     C                   MOVE      WHHDAT        VIDAAS
005300931125     C                   MOVE      ' ?'          VIDPER
005400931117     C                   MOVEL     '999'         VIDFIL
005500930812     C*
005600930812     C     *INKC         DOWEQ     *OFF
005700930812     C*
005800930812     C                   MOVEA     *IN           IN
005900930812     C                   MOVEA     *ZEROS        *IN
006000941013     C                   WRITE     LG44D01
006100930812     C                   MOVEA     IN            *IN
006200930812     C*
006300941013     C                   EXFMT     LG44D01
006400930812     C*
006500930812     C  NKC              DO
006600930812     C** CONTROLLO DATI FORMATO VIDEO
006700930812     C                   EXSR      CONTR1
006800930812     C** SE NON CI SONO ERRORI ED HANNO PREMUTO F1 SOTTOMETTO LA STAMPA
006900931221     C  N90*INKF         IFEQ      *ON
007000941013     C                   MOVE      VIDAAS        D44AA
007100941013     C                   MOVE      VIDPER        D44PER
007200941013     C                   MOVE      VIDFIL        D44FIL
007300930812     C     VIDFIL        IFEQ      '999'
007400941013     C                   MOVE      *ZEROS        D44FIL
007500930812     C                   ENDIF
007600941013     C                   Z-ADD     DATDAL        D44DT1
007700941013     C                   Z-ADD     DATAL         D44DT2
007800930812     C*
007900941013     C                   MOVEL     FNLG44        KPJBU
008000941013     C                   MOVEL     'LG46'        KCOAZ
008100930812     C                   CALL      'BCH10'
008200930812     C                   PARM                    KPJBA
008300930812     C                   ENDIF
008400930702     C*
008500930812     C                   ENDDO
008600930702     C*
008700930812     C                   ENDDO
008800930812     C*
008900930812     C                   SETON                                        LR
009000930812     C*========================================================================*
009100930812     C*-----------
009200930812     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO
009300930812     C*-----------
009400930812     C     CONTR1        BEGSR
009500930812     C*
009600930812     C                   SETOFF                                       902122
009700930812     C                   SETOFF                                       2310
009800931125     C*
009900931125     C* RICERCA FILIALI SE IMMESSO PUNTO INTERROGATIVO
010000931125     C     '?'           SCAN      VIDPER                                 21
010100931125     C     *IN21         IFEQ      *ON
010200941013     C                   MOVE      *ZERO         D45PAA
010300941013     C                   MOVE      *ZERO         D45PRD
010400941013     C                   MOVEL     FNLG45        KPJBU
010500941013     C                   CALL      'FNLG45R'
010600931125     C                   PARM                    KPJBA
010700941013     C                   MOVEL     KPJBU         FNLG45
010800931125     C*
010900931125     C*  CONTROLLO CHE SIA STATA FATTA UNA SELEZIONE
011000941013     C     D45CMD        IFNE      '3'
011100941013     C                   MOVE      D45PAA        VIDAAS
011200941013     C                   MOVE      D45PRD        VIDPER
011300931125     C                   END
011400931125     C                   END
011500931125     C     VIDPER        COMP      '000'                              22  22
011600931125     C  N22              SETON                                        4490
011700930812     C** ANNO OBBLIGATORIO
011800931125     C     *IN90         IFEQ      '0'
011900931117     C     VIDAAS        IFEQ      *ZEROS
012000930812     C                   SETON                                        4090
012100930812     C                   ENDIF
012200930812     C** CONTROLLO SE PERIODO RICHIESTO E' GIA' STATO ELABORATO
012300941013     C                   MOVE      VIDAAS        AA2
012400010921     C                   MOVE      VIDAAS        KAA4
012500931125     C                   MOVE      VIDPER        PERIO
012600010921     C     KFG7          SETLL     FLFG765L                               15
012700930812     C  N15              SETON                                        5090
012800931117     C     *IN15         IFEQ      *ON
012900930812     C                   MOVEL     *BLANKS       KEY
013000930812     C                   MOVEL     DSPER         KEY
013100930812     C     KTAB          CHAIN     TABEL00F                           18
013200931117     C  N18              DO
013300930812     C                   MOVEL     TBLUNI        DS2I
013400930812     C                   Z-ADD     §2IDAL        G02INV
013500930812     C                   MOVEL     '3'           G02ERR
013600941013     C                   CALL      'XSRDA8'
013700941013     C                   PARM                    WLBDA8
013800930812     C                   Z-ADD     G02DAT        VIDDAL
013900941013     C                   Z-ADD     G02INV        DATDAL            8 0
014000941013     C*
014100930812     C                   Z-ADD     §2IAL         G02INV
014200930812     C                   MOVEL     '3'           G02ERR
014300941013     C                   CALL      'XSRDA8'
014400941013     C                   PARM                    WLBDA8
014500930812     C                   Z-ADD     G02DAT        VIDAL
014600941013     C                   Z-ADD     G02INV        DATAL             8 0
014700930812     C                   SETON                                        10
014800931117     C                   ENDDO
014900930812     C                   ELSE
015000930812     C                   Z-ADD     0             VIDDAL
015100930812     C                   Z-ADD     0             VIDAL
015200931125     C                   ENDIF
015300931125     C                   ENDIF
015400930812     C*
015500941013     C  N90VIDFIL        IFNE      '999'
015600930812     C* RICERCA FILIALI SE IMMESSO PUNTO INTERROGATIVO
015700930812     C     '?'           SCAN      VIDFIL                                 21
015800941013     C     *IN21         IFEQ      *ON
015900941013     C     SIMFEL        IFEQ      0
016000930812     C                   MOVEL     *BLANKS       §COD
016100930812     C                   MOVEL     *BLANKS       §TIP
016200930812     C                   MOVEL     *ZEROS        VIDFIL
016300930812     C                   CALL      'TRSE19R'
016400930812     C                   PARM                    §COD              3
016500930812     C                   PARM                    §TIP              2
016600930812     C                   PARM                    §DES             25
016700930812     C                   MOVEL     §COD          VIDFIL
016800930812     C                   SETON                                          90
016900931117     C                   ELSE
017000931117     C                   SETON                                        4390
017100941013     C                   ENDIF
017200941013     C                   ENDIF
017300931117     C*
017400931117     C** FILIALE DA CONTROLLARE OBBLIGATORIA
017500941013     C  N90VIDFIL        IFEQ      *ZEROS
017600931117     C     VIDFIL        OREQ      *BLANKS
017700931117     C                   SETON                                        4190
017800941013     C                   ELSE
017900930812     C*
018000931125     C     VIDFIL        COMP      '000'                              22  22
018100941013     C     *IN22         IFEQ      *OFF
018200930812     C                   SETON                                        9042
018300941013     C                   ELSE
018400930705     C*
018500930705     C* FILIALE INESISTENTE
018501020430     c                   move      vidfil        vidfiln           3 0
018600020430     C     VIDFILn       LOOKUP    L3                                     12
018700931117     C  N12              SETON                                        9043
018800930705     C*
018900941013     C                   ENDIF
019000941013     C                   ENDIF
019100941013     C                   ENDIF
019200930705     C*
019300930812     C                   ENDSR
019400930812     C*-----------
019500930812     C* ROUTINE INIZIALE
019600930812     C*-----------
019700930812     C     *INZSR        BEGSR
019800930701     C*
019900930701     C     *ENTRY        PLIST
020000930701     C                   PARM                    KPJBA
020100930701     C*
020200930701     C                   Z-ADD     1             CODUT             1 0
020300930701     C                   CALL      'X§PARUT'
020400930701     C                   PARM                    UT§DSE
020500930701     C                   MOVEL     REC80         CNCR80
020600930701     C                   MOVEL     RAGUT         RSUT             20
020700930701     C*
020800930812     C** CHIAVE DI TABEL
020900930812     C     KTAB          KLIST
021000930812     C                   KFLD                    CODUT
021100930812     C                   KFLD                    COD               2
021200930812     C                   KFLD                    KEY               8
021300931117     C*
021400931117     C     KTAB1         KLIST
021500931117     C                   KFLD                    CODUT
021600931117     C                   KFLD                    COD               2
021700010921     C** CHIAVE DI FLFG765L
021800930812     C     KFG7          KLIST
021900010921     C                   KFLD                    KAA4              4 0
022000931125     C                   KFLD                    PERIO
022100931117     C*
022200931117     C** CONTROLLO SE SONO IN SEDE O IN FILIALE
022300931117     C     SIMFEL        COMP      0                                      01
022400930812     C*
022401020430     c                   clear                   trul06ds
022402020430     c                   eval      d06cod = '£1'
022403020430     c                   movel     simfel        d06key
022404020430     c                   movel     trul06ds      kpjbu
022405020430     c                   call      'TRUL06R'
022406020430     c                   parm                    kpjba
022407020430     c                   movel     kpjbu         trul06ds
022408020430     c                   movea     lin           l3
022500020430     C*                  MOVEL     '£3'          COD
022600020430     C*    KTAB1         SETLL     TABEL
022700020430     C*    *IN21         DOUEQ     '1'
022800020430     C*    KTAB1         READE     TABEL                                  21
022900020430     C* N21              MOVEA     TBLUNI        LIN
023000020430     C* N21              DO        29            §B                2 0
023100020430     C*    LIN(§B)       IFGT      *ZEROS
023200020430     C*                  ADD       1             §C                3 0
023300020430     C*                  MOVEL     LIN(§B)       L3(§C)
023400020430     C*                  END
023500020430     C*                  END
023600020430     C*                  END
023700931117     C*
023800931117     C                   MOVEL     '2I'          COD
023900931117     C*
024000930701     C                   ENDSR
