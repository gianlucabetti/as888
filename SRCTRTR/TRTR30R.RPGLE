000100950209     H* TRTR30R *-----------------------------------------------------*
000200930216     H*            - STAMPA VARIAZIONI NON EFFETTUATE -               *
000300930215     H*---------------------------------------------------------------*
000400930215     H DECEDIT('0,') DATEDIT(*DMY.)
000500950209     F* ERRORI   PER FEV+DAT+DER
000600991103     FFIARBE1C  IF   E           K DISK
000700991103     FFIARBE2C  IF   E           K DISK
000800950209     F                                     RENAME(FNARBD00:FNARBD02)
000900950209     F                                     RENAME(FNARBK00:FNARBK02)
001000991103     F                                     RENAME(FIARBT00:FIARBT02)
001100950209     F                                     RENAME(FNARBM00:FNARBM02)
001200950209     F                                     RENAME(FNARBV00:FNARBV02)
001300950209     F                                     RENAME(FNARBG00:FNARBG02)
001400990916     F                                     RENAME(FNARBP00:FNARBP02)
001500930216     F*
001600920218     FAZORG01L  IF   E           K DISK
001700950209     FTRTR30D   CF   E             WORKSTN
001800930215     F*
001900930216     D*
002000920218     D PARAM           DS
002100950209     D  VIDDAL                 1      8  0
002200950209     D  VIDAL                  9     16  0
002300950209     D  COMDAL                17     24  0
002400950209     D  COMAL                 25     32  0
002500950209     D  VIDFEV                33     35  0
002600950220     D  VIDAAS                36     39  0
002700950220     D  VIDLNP                40     42  0
002800950220     D  VIDNRS                43     44  0
002900950220     D  VIDNSP                45     51  0
003000950220     D  NUMSPE                36     51  0
003100991202     D* PASSAGGIO PUNTO OPERATIVO DA MENU'
003200991202     D PARAM1          DS
003300991202     D  PARPO                 61     63
003400930216     D*
003500920218     D WLBDAT          DS
003600950209     D  G02DAT                 1      8  0
003700950209     D  G02INV                 9     16  0
003800950209     D  G02ERR                17     17
003900950209     D  G02TGI                18     22  0
004000930216     D*
004100900511     D KPJBA         E DS
004200900511     D  LIBSYS                92    100
004300900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400930216     C*---------------------------------------------------------------*
004500930216     C* INDICATORI USATI                                              *
004600930216     C*---------------------------------------------------------------*
004700991103     C* 30 - AGGANCIO DI AZ0RG01L E LETTURA DI FIARBE1C
004800930216     C* 32 - LETTURA DI FLBLPM1L
004900980109     C* 33 - DI COMODO
005000930216     C* 40 - FILIALE ERRATA
005100930216     C* 41 - DATA STAMPA DAL OBBLIGATORIA
005200930216     C* 42 - DATA STAMPA DAL ERRATA
005300930216     C* 43 - DATA STAMPA AL  ERRATA
005400930216     C* 44 - DATA STAMPA DAL SUPERIORE DATA AL
005500930216     C* 45 - NON ESISTONO VARIAZIONI PER FILIALE IMMESSA
005600930216     C* 46 - NON ESISTONO VARIAZIONI DA RISTAMPARE
005700930216     C* 47 - RISTAMPA PER DATA OPPURE PER NUMERO SPEDIZIONE
005800930216     C* 48 - VARIAZIONI IN P/A/E OBBLIGATORIO
005900980109     C* 49 - ERRORE LINEA DI PARTENZA SPEDIZIONE
006000930217     C* 50 - STAMPA PER SPEDIZIONE SOLO PER VARIAZIONI IN 'P'
006100930219     C* 55 - NON ESISTONO VARIAZIONI PER LA SPEDIZIONE
006200930216     C* 90 - GENERICO DI ERRORE
006300930217     C*---------------------------------------------------------------*
006400930217     C*                    MAIN                                       *
006500930217     C* PROGRAMMA PRINCIPALE STAMPA VARIAZIONI NON EFFETTUATE         *
006600930216     C*---------------------------------------------------------------*
006700930216     C*
006800930216     C* DEFINIZIONE VARIABILI E KLIST
006900930216     C                   EXSR      DEFVAR
007000930216     C*
007100900509     C     FOR01         TAG
007200920218     C                   EXFMT     TR85D01
007300930216     C*
007400950209     C* F3 - FINE LAVORO
007500900509     C   KC              GOTO      FINE
007600930215     C*
007700900509     C* CONTROLLI FORMATO1
007800900509     C                   EXSR      CONTR
007900950209     C  NKF
008000900522     COR 90              GOTO      FOR01
008100930215     C*
008200950209     C* F6 - ELABORAZIONE
008300950209     C*
008400900523     C                   MOVEL     PARAM         KPJBU
008500950209     C                   MOVEL     'TR31'        KCOAZ
008600900523     C                   CALL      'BCH10'
008700900523     C                   PARM                    KPJBA
008800931231     C*
008900950222     C* SE NON E' RISTAMPA SOTTOMETTO PULIZIA (PGM TNSD90R)
009000950228    1C     NUMSPE        IFEQ      0
009100950228     C     VIDDAL        ANDEQ     0
009200950228     C                   MOVEL     'SD90'        KCOAZ
009300950228     C                   CALL      'BCH10'
009400950228     C                   PARM                    KPJBA
009500950228E   1C                   ENDIF
009600930216     C*
009700000000     C     FINE          TAG
009800000000     C                   SETON                                        LR
009900930215     C*---------------------------------------------------------------*
010000930216     C*                    CONTR                                      *
010100930216     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
010200930215     C*---------------------------------------------------------------*
010300900509     C     CONTR         BEGSR
010400930215     C*
010500991103     C                   SETOFF                                       90
010600930219     C                   Z-ADD     *ZEROS        COMDAL
010700930219     C                   Z-ADD     *ZEROS        COMAL
010800930216     C*
010900911108     C* CONTROLLO CODICE LINEA ARRIVO SE IMMESSO
011000930216IF  1C     VIDFEV        IFNE      0
011100920218     C     VIDFEV        CHAIN     AZORG                              30
011200930216     C   30              SETON                                        4090
011300930216     C   30              GOTO      ENDCTR
011400980109     C* P.O. non appartenente al ramo aziendale
011500020704     C**         ORGDIT    IFNE COMPRA
011600020704     C**                   SETON                     4890
011700020704     C**                   GOTO ENDCTR
011800020704     C**                   ENDIF
011900980109E   1C                   ENDIF
012000930216     C*
012100930216     C* RISTAMPA PER DATA O PER NUMERO
012200930216IF  1C     VIDDAL        IFNE      *ZEROS
012300930216     C     VIDAL         ORNE      *ZEROS
012400930216     C*
012500930216IF  2C     NUMSPE        IFNE      *ZEROS
012600930216     C                   SETON                                        4790
012700930216     C                   GOTO      ENDCTR
012800930216E   2C                   ENDIF
012900930216E   1C                   ENDIF
013000930216     C*
013100911108     C* DATA STAMPA DAL OBBLIGATORIA SE IMMESSA DATA AL
013200930216IF  1C     VIDAL         IFNE      *ZEROS
013300930216     C     VIDDAL        ANDEQ     *ZEROS
013400920218     C                   SETON                                        4190
013500911114     C                   GOTO      ENDCTR
013600930216E   1C                   ENDIF
013700900522     C*
013800911114     C* CONTROLLO DATA STAMPA DAL SE IMMESSA
013900930216IF  1C     VIDDAL        IFNE      *ZEROS
014000930216     C*
014100950209     C                   Z-ADD     VIDDAL        G02DAT
014200920218     C                   MOVEL     *BLANK        G02ERR
014300950209     C                   CALL      'XSRDA8'
014400920218     C                   PARM                    WLBDAT
014500930216     C*
014600930216IF  2C     G02ERR        IFEQ      '1'
014700920218     C                   SETON                                        4290
014800920218     C                   GOTO      ENDCTR
014900930216E   2C                   ENDIF
015000950209     C                   Z-ADD     G02INV        COMDAL
015100950217     C                   MOVE      G02DAT        VIDDAL
015200911114     C*
015300911114     C* CONTROLLO DATA STAMPA AL SE IMMESSA
015400930216IF  2C     VIDAL         IFNE      *ZEROS
015500950209     C                   Z-ADD     VIDAL         G02DAT
015600920218     C                   MOVEL     *BLANK        G02ERR
015700930216     C*
015800950209     C                   CALL      'XSRDA8'
015900920218     C                   PARM                    WLBDAT
016000930216IF  3C     G02ERR        IFEQ      '1'
016100920218     C                   SETON                                        4390
016200920218     C                   GOTO      ENDCTR
016300930216E   3C                   ENDIF
016400950209     C                   Z-ADD     G02INV        COMAL
016500950217     C                   MOVE      G02DAT        VIDAL
016600930216     C*
016700900523     C* DATA DAL NO MAGGIORE DATA AL
016800930216IF  3C     COMDAL        IFGT      COMAL
016900920218     C                   SETON                                        4490
017000900523     C                   GOTO      ENDCTR
017100930216E   3C                   ENDIF
017200930219     C*
017300930219X   2C                   ELSE
017400930219     C*
017500930219     C                   Z-ADD     COMDAL        COMAL
017600900523     C*
017700930216E   2C                   ENDIF
017800930216E   1C                   ENDIF
017900930219     C*
018000930219IF  1C     NUMSPE        IFEQ      0
018100930216     C* SCELTA UNA FILIALE
018200950209IF  2C     VIDFEV        IFNE      *ZEROS
018300991103     C     KERR          SETLL     FIARBE1C                               30
018400930216     C*
018500930216     C* NO RISTAMPA E VARIAZIONI INESISTENTI: MESSAGGIO
018600950209IF  3C     COMDAL        IFEQ      *ZEROS
018700950209IF  4C     *IN30         IFEQ      '0'
018800920218     C                   SETON                                        4590
018900920218     C                   GOTO      ENDCTR
019000950209E   4C                   ENDIF
019100930216     C* RISTAMPA
019200950209X   3C                   ELSE
019300920218     C*
019400991103     C     VIDFEV        READE     FIARBE1C                               30
019500930216     C*
019600930216     C* VARIAZIONI INESISTENTI
019700950209IF  4C     *IN30         IFEQ      '1'
019800930216     C                   SETON                                        4690
019900930216     C                   GOTO      ENDCTR
020000950209E   4C                   ENDIF
020100920218     C*
020200930216     C* VARIAZIONI ESISTENTI: DATA 'DAL' PERO SUPERIORE
020300991103IF  4C     ARBDAT        IFGT      COMAL
020400920218     C                   SETON                                        4690
020500920218     C                   GOTO      ENDCTR
020600950209E   4C                   ENDIF
020700930216     C*
020800950209E   3C                   ENDIF
020900950209E   2C                   ENDIF
021000930219     C*
021100930219X   1C                   ELSE
021200930219     C*
021300930219     C* STAMPA PER N U M E R O   S P E D I Z I O N E
021400950220     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
021500950220     C     VIDAAS        IFLT      100
021600950220     C     VIDAAS        IFGT      60
021700950220     C                   MOVEL     19            VIDAAS
021800950220     C                   ELSE
021900950220     C                   MOVEL     20            VIDAAS
022000950220     C                   END
022100950220     C                   END
022200980109     C* CONTROLLA CHE LINEA DI PARTENZA BOLLA APPARTENGA A RAMO AZIEND
022300020704     C**         VIDLNP    CHAINAZORG                30
022400020704     C**N30      ORGDIT    IFNE COMPRA
022500020704     C**                   SETON                     4990
022600020704     C**                   GOTO ENDCTR
022700020704     C**                   END
022800930217     C*
022900930217     C* VARIAZIONI INESISTENTI: ERRORE
023000991103     C     KARB2         CHAIN     FIARBE2C                           33
023100980109IF  2C     *IN33         IFEQ      '1'
023200930219     C                   SETON                                        5590
023300930217     C                   GOTO      ENDCTR
023400930219E   2C                   ENDIF
023500930217     C*
023600930219E   1C                   ENDIF
023700930216     C*
023800930219     C     ENDCTR        ENDSR
023900930216     C*---------------------------------------------------------------*
024000930216     C*                    DEFVAR                                     *
024100930216     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
024200930216     C*---------------------------------------------------------------*
024300930216     C     DEFVAR        BEGSR
024400930216     C*
024500930216     C     *ENTRY        PLIST
024600930216     C                   PARM                    KPJBA
024700991202     C                   MOVEL     KPJBU         PARAM1
024800930216     C*
024900930216     C                   Z-ADD     1             CODUT
025000930216     C                   CALL      'X§PARUT'
025100930216     C                   PARM                    UT§DSE
025200930216     C                   MOVEL     RAGUT         RSUT             20
025300930216     C*
025400930216     C                   Z-ADD     *ZEROS        VIDFEV
025500930216     C                   Z-ADD     *ZEROS        VIDDAL
025600930216     C                   Z-ADD     *ZEROS        VIDAL
025700930219     C                   MOVEL     *ZEROS        NUMSPE
025800991202     C* SE C'E' P.O. LO IMPORTO E LO PROTEGGO
025900991202     C     PARPO         IFGT      *ZEROS
026000991202     C                   MOVEL     PARPO         VIDFEV
026100991202     C                   SETON                                        20
026200991202     C                   ENDIF
026300930216     C*
026400991103     C* LETTURA DI FIARBE1C
026500930216     C     KERR          KLIST
026600930216     C                   KFLD                    VIDFEV
026700930216     C                   KFLD                    COMDAL
026800930216     C*
026900991103     C* LETTURA DI FIARBE2C
027000950209     C     KARB2         KLIST
027100930216     C                   KFLD                    VIDAAS
027200930216     C                   KFLD                    VIDLNP
027300930216     C                   KFLD                    VIDNRS
027400930216     C                   KFLD                    VIDNSP
027500930216     C*
027600930216     C                   ENDSR
