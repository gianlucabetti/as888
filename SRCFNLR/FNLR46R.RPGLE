000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200911125      * FLFR46R *----------------------------------------------------*
000300911125      *-----------------*                                            *
000400911125      *           RISTAMPA FATTURE ASSEGNATI        FILTRO           *
000500911125      *--------------------------------------------------------------*
000600911125     FCNNUM00F  IF   E           K DISK
000700941227     FFNLR46D   CF   E             WORKSTN
000800911125     D                 DS
000900911125     D  AAC                    1      2  0
001000911125     D  RGS                    3      3  0
001100911125     D  LIB                    4      6  0
001200911211     D PARAM           DS
001300941227     D  VIDDFD                 1      8  0
001400941227     D  VIDDFA                 9     16  0
001500941227     D  COMDFD                17     24  0
001600941227     D  COMDFA                25     32  0
001700941227     D  VIDNFD                33     38  0
001800941227     D  VIDNFA                39     44  0
001900941227     D  VIDFIV                45     47  0
002000030619     d* - parametri da TRUL90R:
002100030619     d* � pgm.   per stampa LDV
002200030619     d  wD90psl              187    196
002300030619     d* � stampante per LDV in formato "A4" (laser)
002400030619     d  wD90prb4             197    206
002500030619     d* � stampante per LDV in formato "A5" (laser)
002600030619     d  wD90prb5             207    216
002700030619     d* � modulo per stampa LDV su "A4"     (laser)
002800030619     d  wD90mdb4             217    226
002900030619     d* � modulo per stampa LDV su "A5"     (laser)
003000030619     d  wD90mdb5             227    236
003100911126     D WLBDAT          DS
003200941227     D  G02DAT                 1      8  0
003300941227     D  G02INV                 9     16  0
003400941227     D  G02ERR                17     17
003500941227     D  G02TGI                18     22  0
003600941227      *
003700030619     D* DS PER TRUL90R - RICHIESTA STAMPANTI
003800030619     D TRUL90DS      E DS                  INZ
003900030619     d   D90rsb      e                     inz('S')
004000030619     d   D90mdb      e                     inz('FATTURE999')
004100030619     d   D90mdb4     e                     inz('FATT_999A4')
004200030619     d   D90mdb5     e                     inz('FATT_999A5')
004300941227      *
004400900517     D KPJBA         E DS
004500030619     D UT쬎SE0F      E DS
004600000000     I/SPACE 3
004700911125      *---------------------------------------------------------------*
004800000000     C     *ENTRY        PLIST
004900000000     C                   PARM                    KPJBA
005000030619     C***                Z-ADD     1             CODUT
005100030619     C                   CALL      'X쬝ARUT'
005200030619     C                   PARM                    UT쬎SE0F
005300000000     C                   MOVEL     RAGUT         RSUT             20
005400911125      *---------------------------------------------------------------*
005500911125      * CHIAVE X POSIZIONAMENTO SU CNNUM00F
005600911125      *
005700911125     C     KEY           KLIST                                                  *
005800911125     C                   KFLD                    AAC                            *
005900911125     C                   KFLD                    RGS                            *
006000911125     C                   KFLD                    LIB                            *
006100911126      *
006200911125      *---------------------------------------------------------------*
006300941227     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
006400030619     C                   reset                   TRUL90DS
006500030619     C                   call      'TRUL90R'
006600030619     C                   parm                    KPJBA
006700030619     c                   parm                    TRUL90DS
006800941227     C**
006900941227     C* F3 - FINE
007000030619     C     D90F3         IFEQ      '1'
007100941227     C                   GOTO      FINE
007200941227     C                   ENDIF
007300030619      * OVRPRTF
007400030619     c                   movel     D90psl        wD90psl
007500030619     c                   movel     D90prb4       wD90prb4
007600030619     c                   movel     D90prb5       wD90prb5
007700030619     c                   movel     D90mdb4       wD90mdb4
007800030619     c                   movel     D90mdb5       wD90mdb5
007900941227     C*
008000911212     C                   Z-ADD     0             VIDDFD
008100911212     C                   Z-ADD     0             VIDDFA
008200911212     C                   Z-ADD     0             COMDFD
008300911212     C                   Z-ADD     0             VIDNFD
008400911212     C                   Z-ADD     0             VIDNFA
008500911212     C                   Z-ADD     0             VIDFIV
008600911126      *
008700900509     C     FOR01         TAG
008800941227     C                   EXFMT     LR46D01
008900911125      ** CMD03 - FINE LAVORO
009000900509     C   KC              GOTO      FINE
009100911125      * CONTROLLO
009200900524     C                   EXSR      CONTR
009300941227     C  NKF
009400900524     COR 90              GOTO      FOR01
009500911126      *
009600941227      ** F6 - ELABORAZIONE
009700911210     C                   MOVEL     PARAM         KPJBU
009800941227     C                   MOVEL     'LR47'        KCOAZ
009900030619     C***                CALL      'BCH10'
010000030619     c***                movel(p)  'FNLR47C'     Wpgm             10
010100030619     c                   movel(p)  'BCH10'       Wpgm             10
010200030619     c                   call      Wpgm
010300900523     C                   PARM                    KPJBA
010400911125      *
010500000000     C     FINE          TAG
010600000000     C                   SETON                                        LR
010700911126      *---------------------------------------------------------------*
010800911126      * CONTROLLI FORMATO1
010900900524     C     CONTR         BEGSR
011000900524     C                   SETOFF                                       90
011100911125      *
011200911212      * CODICE FILIALE IVA OBBLIGATORIO;SE IMMESSO,CONTROLLO SE ESISTE
011300911125     C     VIDFIV        IFEQ      0
011400911126     C                   SETON                                        4090
011500911125     C                   GOTO      ENDCTR
011600911125     C                   ELSE
011700911125     C                   MOVE      UDATE         AAC
011800911125     C                   Z-ADD     2             RGS
011900911125     C                   Z-ADD     VIDFIV        LIB
012000911125     C     KEY           SETLL     CNNUM00F                               30
012100911125     C     *IN30         IFEQ      '0'
012200911126     C                   SETON                                        4190
012300911125     C                   GOTO      ENDCTR
012400911125     C                   END
012500911125     C                   END
012600911125      *
012700911125      * DATA FATTURA DAL OBBLIGATORIA
012800911125     C     VIDDFD        IFEQ      0
012900911126     C                   SETON                                        4290
013000911125     C                   GOTO      ENDCTR
013100911211     C                   ELSE
013200911126      *
013300911126      * CONTROLLO DATA FATTURA DAL
013400911126     C                   MOVE      VIDDFD        G02DAT
013500911126     C                   MOVEL     *BLANK        G02ERR
013600941227     C                   CALL      'XSRDA8'
013700911126     C                   PARM                    WLBDAT
013800911126     C     G02ERR        IFEQ      '1'
013900911126     C                   SETON                                        4390
014000911126     C                   GOTO      ENDCTR
014100911211     C                   END
014200941227     C  N90              Z-ADD     G02INV        COMDFD
014300941227     C  N90              Z-ADD     G02DAT        VIDDFD
014400911126     C                   END
014500911125      *
014600911125      * CONTROLLO DATA FATTURA AL
014700911216     C                   Z-ADD     0             COMDFA
014800911125     C     VIDDFA        IFNE      0
014900911126     C                   MOVE      VIDDFA        G02DAT
015000911126     C                   MOVEL     *BLANK        G02ERR
015100941227     C                   CALL      'XSRDA8'
015200911126     C                   PARM                    WLBDAT
015300911126     C     G02ERR        IFEQ      '1'
015400911126     C                   SETON                                        4490
015500911126     C                   GOTO      ENDCTR
015600911211     C                   END
015700941227     C  N90              Z-ADD     G02INV        COMDFA
015800941227     C  N90              Z-ADD     G02DAT        VIDDFA
015900900524     C                   END
016000911125      *
016100911125      * DATA DAL NO MAGGIORE DATA AL
016200911125     C     VIDDFA        IFNE      0
016300911125     C     COMDFD        ANDGT     COMDFA
016400911126     C                   SETON                                        4590
016500900524     C                   GOTO      ENDCTR
016600900524     C                   END
016700911125      *
016800911125      * DATA DAL/AL NO MAGGIORE UDATE
016900941227     C                   TIME                    W0140            14 0
017000941227     C                   MOVE      W0140         G02DAT
017100911126     C                   MOVEL     *BLANK        G02ERR
017200941227     C                   CALL      'XSRDA8'
017300911126     C                   PARM                    WLBDAT
017400911126     C     G02ERR        IFEQ      '1'
017500911126     C                   SETON                                          90
017600911126     C                   GOTO      ENDCTR
017700911126     C                   END
017800941227     C  N90              Z-ADD     G02INV        UDAT              8 0
017900911126      *
018000911125     C     VIDDFD        IFNE      0
018100911126     C     COMDFD        ANDGT     UDAT
018200911126     C                   SETON                                        4690
018300911125     C                   GOTO      ENDCTR
018400911125     C                   END
018500911125      *
018600911125     C     VIDDFA        IFNE      0
018700911126     C     COMDFA        ANDGT     UDAT
018800911126     C                   SETON                                        4790
018900911125     C                   GOTO      ENDCTR
019000911125     C                   END
019100911125      *
019200911125      * NON POSSIBILE LA RISTAMPA DI FATTURE DI ANNI DIVERSI
019300911125     C     VIDDFD        IFNE      0
019400911125     C     VIDDFA        ANDNE     0
019500911125     C                   MOVE      VIDDFD        ANNO              2 0
019600911125     C                   MOVE      VIDDFA        ANNO1             2 0
019700911125     C     ANNO          IFNE      ANNO1
019800911126     C                   SETON                                        4890
019900911125     C                   GOTO      ENDCTR
020000911125     C                   END
020100911125     C                   END
020200911125      *
020300911125      * N캟ATTURA DAL NO MAGGIORE N캟ATTURA AL
020400911125     C     VIDNFD        IFNE      0
020500911125     C     VIDNFA        ANDNE     0
020600911125     C     VIDNFD        ANDGT     VIDNFA
020700911126     C                   SETON                                        4990
020800911125     C                   GOTO      ENDCTR
020900911125     C                   END
021000911125      *
021100911125      * N캟ATTURA DAL NO MAGGIORE ULTIMO NUMERO PRELEVATO
021200911125     C     VIDNFD        IFNE      0
021300911126     C                   MOVE      VIDDFD        AAC
021400911125     C                   Z-ADD     2             RGS
021500911125     C                   Z-ADD     VIDFIV        LIB
021600911125     C     KEY           CHAIN     CNNUM00F                           30
021700911125     C  N30VIDNFD        IFGT      NUMNUM
021800911126     C                   SETON                                        5090
021900911125     C                   GOTO      ENDCTR
022000911125     C                   END
022100911125     C                   END
022200911125      *
022300911125      * N캟ATTURA AL NO MAGGIORE ULTIMO NUMERO PRELEVATO
022400911125     C     VIDNFA        IFNE      0
022500911126     C                   MOVE      VIDDFD        AAC
022600911125     C                   Z-ADD     2             RGS
022700911125     C                   Z-ADD     VIDFIV        LIB
022800911125     C     KEY           CHAIN     CNNUM00F                           30
022900911125     C  N30VIDNFA        IFGT      NUMNUM
023000911126     C                   SETON                                        5190
023100911125     C                   GOTO      ENDCTR
023200911125     C                   END
023300911125     C                   END
023400911125      *
023500900524     C     ENDCTR        ENDSR
