000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS09R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400941223     H* ELENCO SPEDIZ: 8888 / CONVENUTI / OLTRE 35 Q.LI /            *
000500941223     H*                MC 5 E KG 500 / CON CAP GENERICO /            *
000600040621     H*                OVER 20
000700000000     H*--------------------------------------------------------------*
000701110704     Fazorg01l  IF   E           K DISK
000800941228     FFNLS09D   CF   E             WORKSTN
000900040621     D TES             S             34    DIM(6) CTDATA PERRCD(1)
001000920925     D L1              S              3  0 DIM(30)
001100941223     D*
001101110704     d wcar            s                   like(orgcar)
001102110704     d kfil            s                   like(orgfil)
001103110704     d pou_car         s                   like(orgcar)
001104110704
001200941223     D* PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - FNLS10R -
001300950309     D DSLS01        E DS                  INZ
001400920925     D WLBDAT          DS
001500941223     D  G02DAT                 1      8  0
001600941223     D  G02INV                 9     16  0
001700941223     D  G02ERR                17     17
001800941223     D  G02TGI                18     22  0
001900941223     D* DS PER TRUL06R - CARICAMENTO £1
002000941223     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002100950314     D  T06                    1     90  0
002200950314     D                                     DIM(30)
002300900517     D KPJBA         E DS
002400020429     D***                                     1   3 REM
002500020429     D***                                     4   6 REMFIL
002600920925     D  FLAG2                371    371
002700920925     D CNCR80        E DS
002800920925     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900000608     D* DS PER FNLSC4R1 - CONTROLLO ELABORAZIONE PER POSTE - PARAMETRI:
003000000608      * IC4LIN = LINEA
003100000608      * IC4FLG = I=INTERATTIVA (1 RICHIESTA) .. NON UTILIZZATO B=BATCH
003200000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
003300000000     C*---------------------------------------------------------------*
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600000000     C                   Z-ADD     1             CODUT
003700920925     C                   CALL      'X§PARUT'
003800920925     C                   PARM                    UT§DSE
003900000000     C                   MOVEL     RAGUT         RSUT             20
004000920925     C                   MOVEL     REC80         CNCR80
004100941223     C*
004200900511     C*---------------------------------------------------------------*
004300941223     C* CARICAMENTO TABELLE
004400920925     C                   EXSR      CARTAB
004500920925     C*
004600941223     C* TESTATA PGM
004700920925     C   01              MOVE      TES(1)        VIDTES
004800920925     C   02              MOVE      TES(2)        VIDTES
004900920925     C   03              MOVE      TES(3)        VIDTES
005000921001     C   04              MOVE      TES(4)        VIDTES
005100921002     C   06              MOVE      TES(5)        VIDTES
005200040621     C   07              MOVE      TES(6)        VIDTES
005300941223     C*
005400900524     C                   Z-ADD     0             VIDDSD
005500900524     C                   Z-ADD     0             VIDDSA
005600950309     C                   Z-ADD     0             DS1AAL
005700000615     C*
005800000615     C                   MOVEL     FLAG2         DS1FPG
005900900509     C     FOR01         TAG
006000941228     C                   EXFMT     LS09D01
006100941223     C*
006200941223     C** CMD3 - FINE LAVORO
006300900509     C   KC              GOTO      FINE
006400900524     C* CONTROLLO
006500900524     C                   EXSR      CONTR
006600941223     C  NKF
006700900524     COR 90              GOTO      FOR01
006800941223     C** CMD6 - ELABORAZIONE
006900950309     C                   Z-ADD     VIDLNP        DS1LNP
007000950309     C                   Z-ADD     VIDDSD        DS1GDA
007100950309     C                   Z-ADD     VIDDSA        DS1GAL
007200000606     C                   CLEAR                   KPJBU
007300950309     C                   MOVEL     DSLS01        KPJBU
007400941223     C                   MOVEL     'LS10'        KCOAZ
007500900523     C                   CALL      'BCH10'
007600900523     C                   PARM                    KPJBA
007700920925     C*
007800920925     C     FINE          TAG
007900000000     C                   SETON                                        LR
008000900524     C*
008100900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
008200900524     C     CONTR         BEGSR
008300900524     C                   SETOFF                                       90
008400941223     C* DATA SPEDIZIONE DAL OBBLIGATORIA
008500920925     C                   MOVE      VIDDSD        G02DAT
008600920925     C                   MOVEL     *BLANK        G02ERR
008700941223     C                   CALL      'XSRDA8'
008800920925     C                   PARM                    WLBDAT
008900920925     C     G02ERR        IFEQ      '1'
009000920925     C                   SETON                                        40  90
009100920925     C                   GOTO      ENDCTR
009200920925     C                   END
009300950309     C                   Z-ADD     G02INV        DS1ADA
009400941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
009500941223     C                   Z-ADD     G02DAT        VIDDSD
009600900524     C*
009700941223     C* DATA SPEDIZIONE AL
009800900524     C     VIDDSA        IFNE      0
009900920925     C                   MOVE      VIDDSA        G02DAT
010000920925     C                   MOVEL     *BLANK        G02ERR
010100941223     C                   CALL      'XSRDA8'
010200920925     C                   PARM                    WLBDAT
010300920925     C     G02ERR        IFEQ      '1'
010400920925     C                   SETON                                        41  90
010500920925     C                   GOTO      ENDCTR
010600920925     C                   END
010700950309     C                   Z-ADD     G02INV        DS1AAL
010800941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010900941223     C                   Z-ADD     G02DAT        VIDDSA
011000941223     C*
011100900524     C* DATA DAL NO MAGGIORE DATA AL
011200950309     C     DS1ADA        IFGT      DS1AAL
011300900524     C                   SETON                                        4290
011400900524     C                   GOTO      ENDCTR
011500900524     C                   END
011600900524     C                   END
011700920925     C*
011800941223     C* LINEA DI PARTENZA
011900920925     C     VIDLNP        IFNE      0
012000000606      * .. CONTROLLO CHE RISPETTI LE REGOLE DI ELABORAZIONE (PER POSTE)
012100000608     C                   CLEAR                   FNLSC4
012200000608     C                   Z-ADD     VIDLNP        IC4LIN
012300000608     C                   MOVE      'I'           IC4FLG
012400000608     C                   MOVEL     FNLSC4        KPJBU
012500000608     C                   CALL      'FNLSC4R1'
012600000606     C                   PARM                    KPJBA
012700000608     C                   MOVEL     KPJBU         FNLSC4
012800000606      *
012900000608     C     OC4ESE        IFNE      'S'
012901110704     c* se sono nel lancio elenco sped.oltre kg500 e la filiale richiesta
012902110704     c* appartiene all'area del p.o. utente non è errore
012903110704     c                   if        *in04
012904110704     c                   z-add     vidlnp        kfil
012905110704     c                   exsr      rep_car
012910110704     c                   endif
012911110704     c                   if        not *in04 or wcar<>pou_car
013000000606     C                   SETON                                        4390
013100000606     C                   GOTO      ENDCTR
013101110704     c                   endif
013200000606     C                   ENDIF
013300000606     C                   ENDIF
013400941223     C*
013500900524     C     ENDCTR        ENDSR
013600920925     C*
013700941223     C*--- CARICAMENTO TABELLE ---------------------------------------*
013800920925     C     CARTAB        BEGSR
013900920925     C* CARICO TABELLA FILIALI GESTITE £1
014000941223     C                   CLEAR                   DSUL06
014100941223     C                   MOVE      '£1'          D06COD
014200941223     C                   MOVEL     SIMFEL        D06KEY
014300941223     C                   MOVEL     'L'           D06TLA
014400941223     C                   MOVEL     DSUL06        KPJBU
014500941223     C*
014600941223     C                   CALL      'TRUL06R'
014700941223     C                   PARM                    KPJBA
014800941223     C                   MOVEL     KPJBU         DSUL06
014900950314     C                   MOVEA     T06           L1
015000941223     C*
015100020429     C                   Z-ADD     SIMPOU        VIDLNP
015200020429     C                   Z-ADD     SIMPOU        DS1FGS
015300020429     C     SIMTPP        IFEQ      '2'
015400020429     C     SIMTPP        OREQ      *BLANKS
015500020429     C                   SETON                                        05
015600020429     C                   ENDIF
015700920925     C*
015800920925     C     FLAG2         COMP      '8'                                    01
015900920925     C     FLAG2         COMP      'C'                                    02
016000920925     C     FLAG2         COMP      'Q'                                    03
016100921001     C     FLAG2         COMP      '5'                                    04
016200921002     C     FLAG2         COMP      'P'                                    06
016300040621     C     FLAG2         COMP      '2'                                    07
016301110704      *
016302110704     c* chaino azorg del p.o. utente per reperire area
016303110704     c                   clear                   pou_car
016304110704     c                   z-add     simpou        kfil
016305110704     c                   exsr      rep_car
016306110704     c                   z-add     wcar          pou_car
016400920925     C*
016500920925     C                   ENDSR
016501110704     C*---------------------------------------------------------------*
016502110704     C     rep_car       BEGSR
016503110704     c                   clear                   wcar
016504110704     c     kfil          chain     azorg01l
016505110704     c                   if        %found(azorg01l)
016506110704     c                   z-add     orgcar        wcar
016507110704     c                   endif
016508110704     C                   ENDSR
016600941223     C*---------------------------------------------------------------*
016700040621**
016800920925 ***  ELENCO SPEDIZIONI  8888  ***
016900920925 ** ELENCO SPEDIZ.CON CONVENUTI **
017000920925 * ELENCO SPEDIZIONI OLTRE 35 Q. *
017100921002 * ELENCO SPED.OLTRE KG 500/MC 5 *
017200921002 * ELENCO SPED. CON CAP GENERICO *
017300040621 * ELENCO SPEDIZ. OLTRE 20 COLLI *
