000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941025     H* FNLV19R  *---------------------------------------------------*
000300060223     H*     SCRITTURA FIAR4
000400921021     H*--------------------------------------------------------------*
000500941024     FTABEL00F  IF   E           K DISK
000600060214     FFiAR401L  UF A E           K DISK
000700941025     D K35             S              1    DIM(35)
000800941025     D K35B            S              1    DIM(35)
000900940218      *
001000060214     D* DS PER FNLV19R - DS DEI CAMPI PER SCITTURA FiAR4
001100941025     D DSLV19        E DS                  EXTNAME(FNLV19DS)
001200941025     D DS7K          E DS
001300060215     d Dataiso         s               d   datfmt(*iso)
001400941021     I*
001500921021     C*---------------------------------------------------------------*
001600921021     C     *ENTRY        PLIST
001700941025     C                   PARM                    DSLV19
001800000000     C*---------------------------------------------------------------*
001900941025     C* TIPO LANCIO, D19TLA "C" -           CHIUSO CON LR
002000941025     C* TIPO LANCIO, D19TLA "L" - ELABORO E CHIUDO CON LR
002100941025     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
002200941025    1C     D19TLA        IFNE      'C'
002300941025     C                   CLEAR                   D19ERR
002400941025     C**
002500941025     C* DESCRIZIONI NON IMMESSE
002600941025     C**
002700941025    2C     D19NT1        IFEQ      *BLANKS
002800941025    2C     D19NT2        ANDEQ     *BLANKS
002900941025     C*
003000941025     C* SE C'ERA DELETO RECORD
003100060215     C     KBOL          DELETE    Fiar4000                           3033
003200941025     C*
003300941025     C* ERRORE: RECORD NON DISPONIBILE
003400941025     C     *IN33         IFEQ      *ON
003500941025     C                   MOVEL     '2'           D19ERR
003600941025     C                   ENDIF
003700941025     C*
003800941025   X2C                   ELSE
003900941025     C**
004000941025     C* DESCRIZIONE     IMMESSA
004100941025     C**
004200941025     C* CONTROLLO
004300941025     C                   EXSR      CTRDES
004400941025     C*
004500941025     C* NON C'E' ERRORE MEMORIZZO LE NOTE
004600941025    3C     D19ERR        IFEQ      *BLANKS
004700941025     C*
004800941025     C* CHAIN SU TABEL CON TIPO RECORD
004900941025     C                   MOVEL     '7K'          COD
005000941025     C                   MOVEL(P)  D19TRC        KEY
005100941025     C     KTAB          CHAIN     TABEL                              30
005200941025     C  N30              MOVEL     TBLUNI        DS7K
005300941025     C   30              CLEAR                   DS7K
005400941025     C*
005500941025     C* VEDO PER QUANTI CARATTERI LA DEVO TENERE
005600941025     C                   MOVEA     D19NT1        K35
005700941025     C                   Z-ADD     35            X                 2 0
005800941025    4C     K35(X)        DOWEQ     *BLANKS
005900941025     C                   SUB       1             X
006000941025    4C                   ENDDO
006100941025     C*
006200941025     C* LA DESCRIZIONE SUPERE IL MINIMO DI CARATTERI --> POSSO SCRIVERL
006300941025    4C     X             IFGE      §7KMNC
006400060214     C*
006500060214     C     KBOL          CHAIN     FiAR4000                           3033
006600941025     C*
006700941025     C* ERRORE: RECORD NON DISPONIBILE
006800060215    5C     *IN33         IFEQ      *ON
006900941025     C                   MOVEL     '2'           D19ERR
007000060215   X5C                   ELSE
007100941025     C*
007200060223     c* In update , solo se cambiato
007300060223   5ac                   if        *in30 or (not *in30 and d19nt1<>ar4not)
007400060223     c
007500060215    6C     *IN30         IFEQ      *ON
007600060214     C                   CLEAR                   FiAR4000
007700941025     C                   MOVEL     D19AAS        AR4AAS
007800941025     C                   MOVEL     D19LNP        AR4LNP
007900941025     C                   MOVEL     D19NRS        AR4NRS
008000941025     C                   MOVEL     D19NSP        AR4NSP
008100941025     C                   MOVEL     D19TRC        AR4TRC
008200060215    6c                   endif
008300060215     c
008400941025     C                   MOVEL     D19NT1        AR4NOT
008500060215     C*
008600060215     C                   TIME                    W0140            14 0
008700060215     C                   MOVE      W0140         WDTGIO            8 0
008800060215     c     *eur          move      wdtgio        dataiso
008900060215     c                   movel     dataiso       ar4duv
009000060215     c
009100060215    6c                   if        d19ftr<>*blanks
009200060215     c                   movel     d19ftr        ar4ftr
009300060215     c                   movel     dataiso       ar4dtr
009400060223     c                   else
009500060223     c                   clear                   ar4ftr
009600060223     c                   clear                   ar4dtr
009700060215    6c                   endif
009800060215     c
009900060215     C   30              WRITE     FiAR4000
010000060215     c
010100060215     C  n30              UPDATE    FiAR4000
010200061114  x5ac                   else
010300061114     c                   unlock    fiar401l
010400060223   5aC                   ENDIF
010500060223    5C                   ENDIF
010600941025     C*
010700941025   X4C                   ELSE
010800941025     C*
010900941025     C* NON SUPERA I CARATTERI MINIMI PER SCRIVERE IL RECORD --> DELETO
011000060215     C     KBOL          DELETE    FiAR4000                           3033
011100941025     C*
011200941025     C* ERRORE: RECORD NON DISPONIBILE
011300941025    5C     *IN33         IFEQ      *ON
011400941025     C                   MOVEL     '2'           D19ERR
011500941025    5C                   ENDIF
011600970520     C*
011700970520     C* PULISCO IL CAMPO PASSATO PER FAR SAPERE AL CHIAMANTE CHE
011800970520     C*  LA 2 DESCRIZIONE NON E' STATA MEMORIZZATA
011900970520     C                   CLEAR                   D19NT1
012000941025    4C                   ENDIF
012100941025     C*
012200941025    3C                   ENDIF
012300941025    2C                   ENDIF
012400941025    1C                   ENDIF
012500941025     C*
012600941025     C* CHIUSURA PGM
012700941025    1C     D19TLA        IFEQ      ' '
012800941025     C                   RETURN
012900941025     C                   ELSE
013000941025     C                   SETON                                        LR
013100941025    1C                   ENDIF
013200941021     C*
013300941024     C* OPERAZIONI INIZIALI ------------------------------------------*
013400941024     C     *INZSR        BEGSR
013500941025     C     KBOL          KLIST
013600941025     C                   KFLD                    D19AAS
013700941025     C                   KFLD                    D19LNP
013800941025     C                   KFLD                    D19NRS
013900941025     C                   KFLD                    D19NSP
014000941025     C                   KFLD                    D19TRC
014100941025     C     KTAB          KLIST
014200941025     C                   KFLD                    CODUT             1 0
014300941025     C                   KFLD                    COD               2
014400941025     C                   KFLD                    KEY               8
014500941025     C*
014600941025     C                   Z-ADD     1             CODUT
014700941024     C                   ENDSR
014800941025     C*
014900941025     C* CONTROLLO LE DESCRIZIONI IMMESSE -----------------------------*
015000941025     C     CTRDES        BEGSR
015100941025     C*
015200941025    1C     D19NT1        IFNE      *BLANKS
015300941025     C     D19NT2        ANDNE     *BLANKS
015400941025     C*
015500941025     C                   MOVEA     D19NT1        K35B
015600941025     C                   Z-ADD     35            X
015700941025    2C     K35B(X)       DOWEQ     *BLANKS
015800941025     C                   SUB       1             X
015900941025    2C                   ENDDO
016000941025     C*
016100941025     C                   MOVEA     D19NT2        K35
016200941025     C                   Z-ADD     35            Y                 3 0
016300941025    2C     K35(Y)        DOWEQ     *BLANKS
016400941025     C                   SUB       1             Y
016500941025    2C                   ENDDO
016600941025     C*
016700941025     C                   ADD       Y             X
016800941025    2C     X             IFLT      35
016900941025     C* SOTTRAGGO DA 36 PERCHE' DEVE RIMANARE UNO SPAZIO VUOTO TRA LE
017000941025     C*  3 DESCRIZIONI
017100941025     C     36            SUB       Y             X
017200960118     C                   MOVEA     K35           K35B(X)
017300941025     C                   MOVEA     K35B          D19NT1
017400941025     C                   CLEAR                   D19NT2
017500941025     C                   ELSE
017600941025     C                   MOVEL     '1'           D19ERR
017700941025    2C                   ENDIF
017800941025    1C                   ENDIF
017900941025     C*
018000941025     C* SE PIENA SOLO LA 2 SCRIVO LA 1
018100941025    1C     D19NT1        IFEQ      *BLANKS
018200941025     C     D19NT2        ANDNE     *BLANKS
018300941025     C                   MOVEL     D19NT2        D19NT1
018400941025     C                   CLEAR                   D19NT2
018500941025    1C                   ENDIF
018600941025     C*
018700941025     C                   ENDSR
