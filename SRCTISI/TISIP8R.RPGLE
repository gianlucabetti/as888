000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200140114     H* TISIP8R *----------------------------------------------------*
000300140114     H*            Elabora CAl.Eventi particolari x scrivere EVB
000400000000     H*--------------------------------------------------------------*
000500110704     Fazorg01l  IF   E           K DISK
000600140114     FTISIP8D   CF   E             WORKSTN
000700941223     D*
000800110704
000900920925     D WLBDAT          DS
001000941223     D  G02DAT                 1      8  0
001100941223     D  G02INV                 9     16  0
001200941223     D  G02ERR                17     17
001300941223     D  G02TGI                18     22  0
001400900517     D KPJBA         E DS
001500920925     D CNCR80        E DS
001600920925     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700000000     C*---------------------------------------------------------------*
001800000000     C     *ENTRY        PLIST
001900000000     C                   PARM                    KPJBA
002000000000     C                   Z-ADD     1             CODUT
002100920925     C                   CALL      'X§PARUT'
002200920925     C                   PARM                    UT§DSE
002300000000     C                   MOVEL     RAGUT         RSUT             20
002400920925     C                   MOVEL     REC80         CNCR80
002500941223     C*
002600900511     C*---------------------------------------------------------------*
002700941223     C*
002800900524     C                   Z-ADD     0             VIDDSD
002900000615     C*
003000900509     C     FOR01         TAG
003100140114     C                   EXFMT     SIP8D01
003200941223     C*
003300140114     C** F3 - FINE LAVORO
003400900509     C   KC              GOTO      FINE
003500900524     C* CONTROLLO
003600900524     C                   EXSR      CONTR
003700941223     C  NKF
003800900524     COR 90              GOTO      FOR01
003900140114     C** F6 - ELABORAZIONE
004000140114     C                   MOVEL(P)  amgdsd        KPJBU
004100170119     c                   if        simfel>0
004200170119     C                   MOVEL     'SIP9'        KCOAZ
004300170119     C                   CALL      'BCH10'
004400170119     C                   PARM                    KPJBA
004500170119     c
004600170119     c                   else
004700170119     c
004800170119     C                   MOVEL     'SIPB'        KCOAZ
004900170119     C                   CALL      'BCH10'
005000170119     C                   PARM                    KPJBA
005100170119     c                   endif
005200920925     C*
005300920925     C     FINE          TAG
005400000000     C                   SETON                                        LR
005500900524     C*
005600900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
005700900524     C     CONTR         BEGSR
005800900524     C                   SETOFF                                       90
005900941223     C* DATA SPEDIZIONE DAL OBBLIGATORIA
006000920925     C                   MOVE      VIDDSD        G02DAT
006100920925     C                   MOVEL     *BLANK        G02ERR
006200941223     C                   CALL      'XSRDA8'
006300920925     C                   PARM                    WLBDAT
006400920925     C     G02ERR        IFEQ      '1'
006500920925     C                   SETON                                        40  90
006600920925     C                   GOTO      ENDCTR
006700920925     C                   END
006800140114     C                   Z-ADD     G02INV        amgdsd            8 0
006900941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
007000941223     C                   Z-ADD     G02DAT        VIDDSD
007100900524     C*
007200900524     C     ENDCTR        ENDSR
