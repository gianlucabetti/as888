000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200051110     H* TISTO1R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400051110     H* STATISTICA SPEDIZIONI OVER 20 COLLI                          *
000500000000     H*--------------------------------------------------------------*
000600051110     FTISTO1D   CF   E             WORKSTN
000700941223     D*
000800920925     D WLBDAT          DS
000900941223     D  G02DAT                 1      8  0
001000941223     D  G02INV                 9     16  0
001100941223     D  G02ERR                17     17
001200941223     D  G02TGI                18     22  0
001300900517     D KPJBA         E DS
001400051114     d Azuteds       e ds                  Extname(Azute00f)
001500051114     d dDatiute      e ds
001600051114     d Tibs34ds      e ds                  Inz
001700051114     D*-------------------
001800051114     D* DS PARAMETRI DI LANCIO PGM
001900051114     D*-------------------
002000051114     D PARAM           DS
002100051114     D  PARdda                 1      8  0
002200051114     D  PARdal                 9     16  0
002300051114     d
002400051114     D comdsd          S                   LIKE(viddsd) inz
002500051114     D comdsa          S                   LIKE(viddsa) inz
002600000000     C*---------------------------------------------------------------*
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900051114     c     *dtaara       define    §azute        azuteds
003000051114     c     *dtaara       define    §datiute      ddatiute
003100051114     c                   in(E)     *dtaara
003200051114     c                   If        %error  or RSUT = *blanks
003300051114     c                   CLEAR                   tibs34ds
003400051114     c                   CALL      'TIBS34R'
003500051114     c                   PARM                    tibs34ds
003600051114     c                   in        *dtaara
003700051114     c                   EndIf
003800941223     C*
003900900511     C*---------------------------------------------------------------*
004000920925     C*
004100900524     C                   Z-ADD     0             VIDDSD
004200900524     C                   Z-ADD     0             VIDDSA
004300000615     C*
004400900509     C     FOR01         TAG
004500051114     C                   EXFMT     sto1D01
004600941223     C*
004700941223     C** CMD3 - FINE LAVORO
004800900509     C   KC              GOTO      FINE
004900900524     C* CONTROLLO
005000900524     C                   EXSR      CONTR
005100941223     C  NKF
005200900524     COR 90              GOTO      FOR01
005300941223     C** CMD6 - ELABORAZIONE
005400051114     C                   Z-ADD     comDSD        pardda
005500051114     C                   Z-ADD     comdsa        pardal
005600000606     C                   CLEAR                   KPJBU
005700051114     C                   MOVEL     param         KPJBU
005800051117     C                   MOVEL     'STO2'        KCOAZ
005900900523     C                   CALL      'BCH10'
006000900523     C                   PARM                    KPJBA
006100920925     C*
006200920925     C     FINE          TAG
006300000000     C                   SETON                                        LR
006400900524     C*
006500900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
006600900524     C     CONTR         BEGSR
006700900524     C                   SETOFF                                       90
006800941223     C* DATA SPEDIZIONE DAL OBBLIGATORIA
006900920925     C                   MOVE      VIDDSD        G02DAT
007000920925     C                   MOVEL     *BLANK        G02ERR
007100941223     C                   CALL      'XSRDA8'
007200920925     C                   PARM                    WLBDAT
007300920925     C     G02ERR        IFEQ      '1'
007400920925     C                   SETON                                        40  90
007500920925     C                   GOTO      ENDCTR
007600920925     C                   END
007700051114     C                   Z-ADD     G02INV        comdsd
007800941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
007900941223     C                   Z-ADD     G02DAT        VIDDSD
008000900524     C*
008100051114     C* DATA SPEDIZIONE AL OBBLIGATORIA
008200051114     C                   MOVE      VIDDSA        G02DAT
008300051114     C                   MOVEL     *BLANK        G02ERR
008400051114     C                   CALL      'XSRDA8'
008500051114     C                   PARM                    WLBDAT
008600051114     C     G02ERR        IFEQ      '1'
008700051114     C                   SETON                                        41  90
008800051114     C                   GOTO      ENDCTR
008900051114     C                   END
009000051114     C                   Z-ADD     G02INV        comdsa
009100051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
009200051114     C                   Z-ADD     G02DAT        VIDDSa
009300941223     C*
009400900524     C* DATA DAL NO MAGGIORE DATA AL
009500051114     C     comdsd        IFGT      comdsa
009600900524     C                   SETON                                        4290
009700900524     C                   GOTO      ENDCTR
009800900524     C                   END
009900941223     C*
010000900524     C     ENDCTR        ENDSR
010100920925     C*
