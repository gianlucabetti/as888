000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200161005     H* fnlvt2r *----------------------------------------------------*
000300161005     H* statistica reclami/bolle
000400000000     H*--------------------------------------------------------------*
000500110704     Fazorg01l  IF   E           K DISK
000600161005     Ffnlvt2D   CF   E             WORKSTN
000700161005     D TES             S             34    DIM(1) CTDATA PERRCD(1)
000800941223     D*
000900110704
001000920925     D WLBDAT          DS
001100941223     D  G02DAT                 1      8  0
001200941223     D  G02INV                 9     16  0
001300941223     D  G02ERR                17     17
001400941223     D  G02TGI                18     22  0
001500900517     D KPJBA         E DS
001600920925     D CNCR80        E DS
001700161005     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800161005     D
001900161005     D param           DS
002000161005     D  datada                 1      8  0
002100161005     D  dataa                  9     16  0
002200161005     D  fil                   17     19  0
002300161005     D  priv                  20     20
002400161005     D  alert                 21     21
002500000000     C*---------------------------------------------------------------*
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800000000     C                   Z-ADD     1             CODUT
002900920925     C                   CALL      'X§PARUT'
003000920925     C                   PARM                    UT§DSE
003100000000     C                   MOVEL     RAGUT         RSUT             20
003200161005     C                   MOVEL     REC80         CNCR80
003300900511     C*---------------------------------------------------------------*
003400941223     C* TESTATA PGM
003500161005     C                   MOVE      TES(1)        VIDTES
003600941223     C*
003700900524     C                   Z-ADD     0             VIDDSD
003800900524     C                   Z-ADD     0             VIDDSA
003900161005     C                   CLEAR                   PARAM
004000000615     C*
004100900509     C     FOR01         TAG
004200161005     C                   EXFMT     LVT2D01
004300941223     C*
004400161005     C** F3 - FINE LAVORO
004500900509     C   KC              GOTO      FINE
004600900524     C* CONTROLLO
004700900524     C                   EXSR      CONTR
004800941223     C  NKF
004900900524     COR 90              GOTO      FOR01
005000161005     C** F6 - ELABORAZIONE
005100161005     C                   Z-ADD     VIDLNP        FIL
005200000606     C                   CLEAR                   KPJBU
005300161005     C                   MOVEL     PARAM         KPJBU
005400161005     C                   MOVEL     'LVT3'        KCOAZ
005500900523     C                   CALL      'BCH10'
005600900523     C                   PARM                    KPJBA
005700920925     C*
005800920925     C     FINE          TAG
005900000000     C                   SETON                                        LR
006000900524     C*
006100900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
006200900524     C     CONTR         BEGSR
006300900524     C                   SETOFF                                       90
006400941223     C* DATA SPEDIZIONE DAL OBBLIGATORIA
006500920925     C                   MOVE      VIDDSD        G02DAT
006600920925     C                   MOVEL     *BLANK        G02ERR
006700941223     C                   CALL      'XSRDA8'
006800920925     C                   PARM                    WLBDAT
006900920925     C     G02ERR        IFEQ      '1'
007000920925     C                   SETON                                        40  90
007100920925     C                   GOTO      ENDCTR
007200920925     C                   END
007300161005     C                   Z-ADD     G02INV        DATADA
007400941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
007500941223     C                   Z-ADD     G02DAT        VIDDSD
007600900524     C*
007700941223     C* DATA SPEDIZIONE AL
007800920925     C                   MOVE      VIDDSA        G02DAT
007900920925     C                   MOVEL     *BLANK        G02ERR
008000941223     C                   CALL      'XSRDA8'
008100920925     C                   PARM                    WLBDAT
008200920925     C     G02ERR        IFEQ      '1'
008300920925     C                   SETON                                        41  90
008400920925     C                   GOTO      ENDCTR
008500920925     C                   END
008600161005     C                   Z-ADD     G02INV        DATAA
008700941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
008800941223     C                   Z-ADD     G02DAT        VIDDSA
008900941223     C*
009000900524     C* DATA DAL NO MAGGIORE DATA AL
009100161005     C     dataDA        IFGT      dataa
009200900524     C                   SETON                                        4290
009300900524     C                   GOTO      ENDCTR
009400900524     C                   END
009500920925     C*
009600941223     C* LINEA DI PARTENZA
009700161005     c                   if        vidlnp>0
009800161005     C     VIDLNP        CHAIN     AZORG01L
009900161005     C                   IF        NOT %FOUND(AZORG01L)
010000161005     C                             OR (ORGFAG<>'A' AND ORGFAG<>'F')
010100161005     C                   SETON                                        4390
010200000606     C                   GOTO      ENDCTR
010300161005     C                   ENDIF
010400161005     C                   ENDIF
010500941223     C*
010600900524     C     ENDCTR        ENDSR
010700920925     C*
010800040621**
010900161005 **  STATISTICA RECLAMI / ALERT **
