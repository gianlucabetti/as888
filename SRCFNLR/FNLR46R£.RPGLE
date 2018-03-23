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
002100030619     d* · pgm.   per stampa LDV
002200030619     d  wD90psl              187    196
002300030619     d* · stampante per LDV in formato "A4" (laser)
002400030619     d  wD90prb4             197    206
002500030619     d* · stampante per LDV in formato "A5" (laser)
002600030619     d  wD90prb5             207    216
002700030619     d* · modulo per stampa LDV su "A4"     (laser)
002800030619     d  wD90mdb4             217    226
002900030619     d* · modulo per stampa LDV su "A5"     (laser)
003000030619     d  wD90mdb5             227    236
003100030619     d* · modulo per stampa bolle/fatture   (ad aghi)
003200030619     d  wD90mdb              237    246
003300030619     d* · stampante per bolle/fatture       (ad aghi)
003400030619     d  wD90prb              247    256
003500911126     D WLBDAT          DS
003600941227     D  G02DAT                 1      8  0
003700941227     D  G02INV                 9     16  0
003800941227     D  G02ERR                17     17
003900941227     D  G02TGI                18     22  0
004000941227      *
004100030619     D* DS PER TRUL90R - RICHIESTA STAMPANTI
004200030619     D TRUL90DS      E DS                  INZ
004300030619     d   D90rsb      e                     inz('S')
004400030619     d   D90mdb      e                     inz('FATTURE999')
004500030619     d   D90mdb4     e                     inz('FATT_999A4')
004600030619     d   D90mdb5     e                     inz('FATT_999A5')
004700941227      *
004800900517     D KPJBA         E DS
004900030619     D UT§DSE0F      E DS
005000000000     I/SPACE 3
005100911125      *---------------------------------------------------------------*
005200000000     C     *ENTRY        PLIST
005300000000     C                   PARM                    KPJBA
005400030619     C***                Z-ADD     1             CODUT
005500030619     C                   CALL      'X§PARUT'
005600030619     C                   PARM                    UT§DSE0F
005700000000     C                   MOVEL     RAGUT         RSUT             20
005800911125      *---------------------------------------------------------------*
005900911125      * CHIAVE X POSIZIONAMENTO SU CNNUM00F
006000911125      *
006100911125     C     KEY           KLIST                                                  *
006200911125     C                   KFLD                    AAC                            *
006300911125     C                   KFLD                    RGS                            *
006400911125     C                   KFLD                    LIB                            *
006500911126      *
006600911125      *---------------------------------------------------------------*
006700941227     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
006800030619     C                   reset                   TRUL90DS
006900030619     C                   call      'TRUL90R'
007000030619     C                   parm                    KPJBA
007100030619     c                   parm                    TRUL90DS
007200941227     C**
007300941227     C* F3 - FINE
007400030619     C     D90F3         IFEQ      '1'
007500941227     C                   GOTO      FINE
007600941227     C                   ENDIF
007700030619      * OVRPRTF
007800030619     c                   movel     D90psl        wD90psl
007900030619     c                   movel     D90prb4       wD90prb4
008000030619     c                   movel     D90prb5       wD90prb5
008100030619     c                   movel     D90mdb4       wD90mdb4
008200030619     c                   movel     D90mdb5       wD90mdb5
008300030619     c                   movel     D90prb        wD90prb
008400030619     c                   movel     D90mdb        wD90mdb
008500941227     C*
008600911212     C                   Z-ADD     0             VIDDFD
008700911212     C                   Z-ADD     0             VIDDFA
008800911212     C                   Z-ADD     0             COMDFD
008900911212     C                   Z-ADD     0             VIDNFD
009000911212     C                   Z-ADD     0             VIDNFA
009100911212     C                   Z-ADD     0             VIDFIV
009200911126      *
009300900509     C     FOR01         TAG
009400941227     C                   EXFMT     LR46D01
009500911125      ** CMD03 - FINE LAVORO
009600900509     C   KC              GOTO      FINE
009700911125      * CONTROLLO
009800900524     C                   EXSR      CONTR
009900941227     C  NKF
010000900524     COR 90              GOTO      FOR01
010100911126      *
010200941227      ** F6 - ELABORAZIONE
010300911210     C                   MOVEL     PARAM         KPJBU
010400941227     C                   MOVEL     'LR47'        KCOAZ
010500030619     C***                CALL      'BCH10'
010600030619     c***                movel(p)  'FNLR47C'     Wpgm             10
010700030619     c                   movel(p)  'BCH10'       Wpgm             10
010800030619     c                   call      Wpgm
010900900523     C                   PARM                    KPJBA
011000911125      *
011100000000     C     FINE          TAG
011200000000     C                   SETON                                        LR
011300911126      *---------------------------------------------------------------*
011400911126      * CONTROLLI FORMATO1
011500900524     C     CONTR         BEGSR
011600900524     C                   SETOFF                                       90
011700911125      *
011800911212      * CODICE FILIALE IVA OBBLIGATORIO;SE IMMESSO,CONTROLLO SE ESISTE
011900911125     C     VIDFIV        IFEQ      0
012000911126     C                   SETON                                        4090
012100911125     C                   GOTO      ENDCTR
012200911125     C                   ELSE
012300911125     C                   MOVE      UDATE         AAC
012400911125     C                   Z-ADD     2             RGS
012500911125     C                   Z-ADD     VIDFIV        LIB
012600911125     C     KEY           SETLL     CNNUM00F                               30
012700911125     C     *IN30         IFEQ      '0'
012800911126     C                   SETON                                        4190
012900911125     C                   GOTO      ENDCTR
013000911125     C                   END
013100911125     C                   END
013200911125      *
013300911125      * DATA FATTURA DAL OBBLIGATORIA
013400911125     C     VIDDFD        IFEQ      0
013500911126     C                   SETON                                        4290
013600911125     C                   GOTO      ENDCTR
013700911211     C                   ELSE
013800911126      *
013900911126      * CONTROLLO DATA FATTURA DAL
014000911126     C                   MOVE      VIDDFD        G02DAT
014100911126     C                   MOVEL     *BLANK        G02ERR
014200941227     C                   CALL      'XSRDA8'
014300911126     C                   PARM                    WLBDAT
014400911126     C     G02ERR        IFEQ      '1'
014500911126     C                   SETON                                        4390
014600911126     C                   GOTO      ENDCTR
014700911211     C                   END
014800941227     C  N90              Z-ADD     G02INV        COMDFD
014900941227     C  N90              Z-ADD     G02DAT        VIDDFD
015000911126     C                   END
015100911125      *
015200911125      * CONTROLLO DATA FATTURA AL
015300911216     C                   Z-ADD     0             COMDFA
015400911125     C     VIDDFA        IFNE      0
015500911126     C                   MOVE      VIDDFA        G02DAT
015600911126     C                   MOVEL     *BLANK        G02ERR
015700941227     C                   CALL      'XSRDA8'
015800911126     C                   PARM                    WLBDAT
015900911126     C     G02ERR        IFEQ      '1'
016000911126     C                   SETON                                        4490
016100911126     C                   GOTO      ENDCTR
016200911211     C                   END
016300941227     C  N90              Z-ADD     G02INV        COMDFA
016400941227     C  N90              Z-ADD     G02DAT        VIDDFA
016500900524     C                   END
016600911125      *
016700911125      * DATA DAL NO MAGGIORE DATA AL
016800911125     C     VIDDFA        IFNE      0
016900911125     C     COMDFD        ANDGT     COMDFA
017000911126     C                   SETON                                        4590
017100900524     C                   GOTO      ENDCTR
017200900524     C                   END
017300911125      *
017400911125      * DATA DAL/AL NO MAGGIORE UDATE
017500941227     C                   TIME                    W0140            14 0
017600941227     C                   MOVE      W0140         G02DAT
017700911126     C                   MOVEL     *BLANK        G02ERR
017800941227     C                   CALL      'XSRDA8'
017900911126     C                   PARM                    WLBDAT
018000911126     C     G02ERR        IFEQ      '1'
018100911126     C                   SETON                                          90
018200911126     C                   GOTO      ENDCTR
018300911126     C                   END
018400941227     C  N90              Z-ADD     G02INV        UDAT              8 0
018500911126      *
018600911125     C     VIDDFD        IFNE      0
018700911126     C     COMDFD        ANDGT     UDAT
018800911126     C                   SETON                                        4690
018900911125     C                   GOTO      ENDCTR
019000911125     C                   END
019100911125      *
019200911125     C     VIDDFA        IFNE      0
019300911126     C     COMDFA        ANDGT     UDAT
019400911126     C                   SETON                                        4790
019500911125     C                   GOTO      ENDCTR
019600911125     C                   END
019700911125      *
019800911125      * NON POSSIBILE LA RISTAMPA DI FATTURE DI ANNI DIVERSI
019900911125     C     VIDDFD        IFNE      0
020000911125     C     VIDDFA        ANDNE     0
020100911125     C                   MOVE      VIDDFD        ANNO              2 0
020200911125     C                   MOVE      VIDDFA        ANNO1             2 0
020300911125     C     ANNO          IFNE      ANNO1
020400911126     C                   SETON                                        4890
020500911125     C                   GOTO      ENDCTR
020600911125     C                   END
020700911125     C                   END
020800911125      *
020900911125      * N°FATTURA DAL NO MAGGIORE N°FATTURA AL
021000911125     C     VIDNFD        IFNE      0
021100911125     C     VIDNFA        ANDNE     0
021200911125     C     VIDNFD        ANDGT     VIDNFA
021300911126     C                   SETON                                        4990
021400911125     C                   GOTO      ENDCTR
021500911125     C                   END
021600911125      *
021700911125      * N°FATTURA DAL NO MAGGIORE ULTIMO NUMERO PRELEVATO
021800911125     C     VIDNFD        IFNE      0
021900911126     C                   MOVE      VIDDFD        AAC
022000911125     C                   Z-ADD     2             RGS
022100911125     C                   Z-ADD     VIDFIV        LIB
022200911125     C     KEY           CHAIN     CNNUM00F                           30
022300911125     C  N30VIDNFD        IFGT      NUMNUM
022400911126     C                   SETON                                        5090
022500911125     C                   GOTO      ENDCTR
022600911125     C                   END
022700911125     C                   END
022800911125      *
022900911125      * N°FATTURA AL NO MAGGIORE ULTIMO NUMERO PRELEVATO
023000911125     C     VIDNFA        IFNE      0
023100911126     C                   MOVE      VIDDFD        AAC
023200911125     C                   Z-ADD     2             RGS
023300911125     C                   Z-ADD     VIDFIV        LIB
023400911125     C     KEY           CHAIN     CNNUM00F                           30
023500911125     C  N30VIDNFA        IFGT      NUMNUM
023600911126     C                   SETON                                        5190
023700911125     C                   GOTO      ENDCTR
023800911125     C                   END
023900911125     C                   END
024000911125      *
024100900524     C     ENDCTR        ENDSR
