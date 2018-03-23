000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200081014     H* TIST95R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400081014     H* STATISTICA SPEDIZIONI UNIFICANTI                             *
000500000000     H*--------------------------------------------------------------*
000600081014     FTIST95D   CF   E             WORKSTN
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
002100081014     D  PAR1da                 1      8  0
002200081014     D  PAR1al                 9     16  0
002201081014     D  PAR2da                17     24  0
002202081014     D  PAR2al                25     32  0
002300051114     d
002400081014     D comdsd1         S                   LIKE(vi1dsd) inz
002500081014     D comdsa1         S                   LIKE(vi1dsa) inz
002501081014     D comdsd2         S                   LIKE(vi2dsd) inz
002502081014     D comdsa2         S                   LIKE(vi2dsa) inz
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
004100081014     C                   Z-ADD     0             VI1DSD
004200081014     C                   Z-ADD     0             VI1DSA
004201081014     C                   Z-ADD     0             VI2DSD
004202081014     C                   Z-ADD     0             VI2DSA
004300000615     C*
004400900509     C     FOR01         TAG
004500081014     C                   EXFMT     st95D01
004600941223     C*
004700941223     C** CMD3 - FINE LAVORO
004800900509     C   KC              GOTO      FINE
004900900524     C* CONTROLLO
005000900524     C                   EXSR      CONTR
005100941223     C  NKF
005200900524     COR 90              GOTO      FOR01
005300941223     C** CMD6 - ELABORAZIONE
005400081014     C                   Z-ADD     comDSD1       par1da
005500081014     C                   Z-ADD     comDsa1       par1al
005501081014     C                   Z-ADD     comDSD2       par2da
005502081014     C                   Z-ADD     comDsa2       par2al
005600000606     C                   CLEAR                   KPJBU
005700051114     C                   MOVEL     param         KPJBU
005800081014     C                   MOVEL     'ST96'        KCOAZ
005900081014     C                   CALL      'BCH10'
005901081014     C*****              CALL      'TIST96R'
006000900523     C                   PARM                    KPJBA
006100920925     C*
006200920925     C     FINE          TAG
006300000000     C                   SETON                                        LR
006400900524     C*
006500900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
006600900524     C     CONTR         BEGSR
006700900524     C                   SETOFF                                       90
006800081014     C* DATA SPEDIZIONE DAL OBBLIGATORIA
006900081014     C                   MOVE      VI1DSD        G02DAT
007000920925     C                   MOVEL     *BLANK        G02ERR
007100941223     C                   CALL      'XSRDA8'
007200920925     C                   PARM                    WLBDAT
007300920925     C     G02ERR        IFEQ      '1'
007400920925     C                   SETON                                        40  90
007500920925     C                   GOTO      ENDCTR
007600920925     C                   END
007700081014     C                   Z-ADD     G02INV        comDsd1
007800941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
007900081014     C                   Z-ADD     G02DAT        VI1DSD
008000900524     C*
008100051114     C* DATA SPEDIZIONE AL OBBLIGATORIA
008200081014     C                   MOVE      VI1DSA        G02DAT
008300051114     C                   MOVEL     *BLANK        G02ERR
008400051114     C                   CALL      'XSRDA8'
008500051114     C                   PARM                    WLBDAT
008600051114     C     G02ERR        IFEQ      '1'
008700051114     C                   SETON                                        41  90
008800051114     C                   GOTO      ENDCTR
008900051114     C                   END
009000081014     C                   Z-ADD     G02INV        comDsa1
009100051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
009200081014     C                   Z-ADD     G02DAT        VI1DSa
009300941223     C*
009400900524     C* DATA DAL NO MAGGIORE DATA AL
009500081014     C     comdsd1       IFGT      comdsa1
009600900524     C                   SETON                                        4290
009700900524     C                   GOTO      ENDCTR
009800900524     C                   END
009900941223     C*
009901081014     C* DATA SPEDIZIONE DAL OBBLIGATORIA
009902081014     C                   MOVE      VI2DSD        G02DAT
009903081014     C                   MOVEL     *BLANK        G02ERR
009904081014     C                   CALL      'XSRDA8'
009905081014     C                   PARM                    WLBDAT
009906081014     C     G02ERR        IFEQ      '1'
009907081014     C                   SETON                                        50  90
009908081014     C                   GOTO      ENDCTR
009909081014     C                   END
009910081014     C                   Z-ADD     G02INV        comDsd2
009911081014     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
009912081014     C                   Z-ADD     G02DAT        VI2DSD
009913081014     C*
009914081014     C* DATA SPEDIZIONE AL OBBLIGATORIA
009915081014     C                   MOVE      VI2DSA        G02DAT
009916081014     C                   MOVEL     *BLANK        G02ERR
009917081014     C                   CALL      'XSRDA8'
009918081014     C                   PARM                    WLBDAT
009919081014     C     G02ERR        IFEQ      '1'
009920081014     C                   SETON                                        51  90
009921081014     C                   GOTO      ENDCTR
009922081014     C                   END
009923081014     C                   Z-ADD     G02INV        comDsa2
009924081014     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
009925081014     C                   Z-ADD     G02DAT        VI2DSa
009926081014     C*
009927081014     C* DATA DAL NO MAGGIORE DATA AL
009928081014     C     comdsd2       IFGT      comdsa2
009929081014     C                   SETON                                        5290
009930081014     C                   GOTO      ENDCTR
009931081014     C                   END
009932081014     C*
010000900524     C     ENDCTR        ENDSR
010100920925     C*
