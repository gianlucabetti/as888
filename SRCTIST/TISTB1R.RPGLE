000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200110221     H* TISTB1R *----------------------------------------------------*
000300110221     H* STATISTICA bancali per lnp/lna                               *
000400000000     H*--------------------------------------------------------------*
000500110221     FTISTB1D   CF   E             WORKSTN
000600941223     D*
000700920925     D WLBDAT          DS
000800941223     D  G02DAT                 1      8  0
000900941223     D  G02INV                 9     16  0
001000941223     D  G02ERR                17     17
001100941223     D  G02TGI                18     22  0
001200900517     D KPJBA         E DS
001300051114     d Azuteds       e ds                  Extname(Azute00f)
001400051114     d dDatiute      e ds
001500051114     d Tibs34ds      e ds                  Inz
001600051114     D*-------------------
001700051114     D* DS PARAMETRI DI LANCIO PGM
001800051114     D*-------------------
001900051114     D PARAM           DS
002000051114     D  PARdda                 1      8  0
002100051114     D  PARdal                 9     16  0
002200051114     d
002300051114     D comdsd          S                   LIKE(viddsd) inz
002400051114     D comdsa          S                   LIKE(viddsa) inz
002500000000     C*---------------------------------------------------------------*
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800051114     c     *dtaara       define    §azute        azuteds
002900051114     c     *dtaara       define    §datiute      ddatiute
003000051114     c                   in(E)     *dtaara
003100051114     c                   If        %error  or RSUT = *blanks
003200051114     c                   CLEAR                   tibs34ds
003300051114     c                   CALL      'TIBS34R'
003400051114     c                   PARM                    tibs34ds
003500051114     c                   in        *dtaara
003600051114     c                   EndIf
003700100922     C*
003800900511     C*---------------------------------------------------------------*
003900920925     C*
004000900524     C                   Z-ADD     0             VIDDSD
004100900524     C                   Z-ADD     0             VIDDSA
004200000615     C*
004300900509     C     FOR01         TAG
004400110221     C                   EXFMT     stB1D01
004500941223     C*
004600941223     C** CMD3 - FINE LAVORO
004700900509     C   KC              GOTO      FINE
004800900524     C* CONTROLLO
004900900524     C                   EXSR      CONTR
005000941223     C  NKF
005100900524     COR 90              GOTO      FOR01
005200110221     C** F6 - ELABORAZIONE
005300051114     C                   Z-ADD     comDSD        pardda
005400051114     C                   Z-ADD     comdsa        pardal
005500000606     C                   CLEAR                   KPJBU
005600051114     C                   MOVEL     param         KPJBU
005700110221     C                   MOVEL     'STB2'        KCOAZ
005800100923     C                   CALL      'BCH10'
005900110221     C****               CALL      'TISTB2R'
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
