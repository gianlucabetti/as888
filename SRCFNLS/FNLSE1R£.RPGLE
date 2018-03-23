000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010326      * FNLSE1R *-----------------------------------------------------*
000300010312      *            - LISTA PREPAGATI PARTITI -        FILTRO          *
000400010312      *---------------------------------------------------------------*
000500010327      *___________________________________________________________________*
000600010327      *                                                                   *
000700010329      * ATTENZIONE!!!!!! NELL'ULTIMA POSIZIONE DELLA KPJBA (502)          *
000800010329      *                  PASSO UN FLAG PER LA STAMPA DEI PREPAGATI        *
000900010329      *                  ANCORA DA CONFERMARE                             *
001000010327      *___________________________________________________________________*
001100010327      *
001200010326     FFNLSE1D   CF   E             WORKSTN
001300010312      *
001400010312     D L1              S              3  0 DIM(30)
001500010312     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001600010312      *
001700950308     D DSLS01        E DS                  INZ
001800930913     D KPJBA         E DS
001900020429     D***                                     1   3 REM
002000020429     D***                                     4   6 REMFIL
002100010329     D  SINO                 502    502
002200010312      * DS PER TRUL06R - CARICAMENTO £X
002300970901     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002400970901     D  LIN                    1     90  0
002500970901     D                                     DIM(30)
002600010312      *----------------------------------
002700010312      * PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
002800010312      *----------------------------------
002900921002     D WLBDAT          DS
003000941227     D  G02DAT                 1      8  0
003100941227     D  G02INV                 9     16  0
003200941227     D  G02ERR                17     17
003300941227     D  G02TGI                18     22  0
003400010312      *----------------------------------
003500010312      * PARAMETRI RICERCA DATI UTENTI                       - X§PARUT -
003600010312      *----------------------------------
003700921002     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800930913     D CNCR80        E DS
003900010312      *---------------------------------------------------------------*
004000010312      *                    MAIN                                       *
004100010312      * PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
004200010312      *---------------------------------------------------------------*
004300010312      *
004400010312      * DEFINIZIONE VARIABILI, KLIST E PLIST
004500930913     C                   EXSR      DEFVAR
004600010312      *
004700010312      * CARICAMENTO FILIALI MECCANIZZATE
004800921002     C                   EXSR      CARTAB
004900010312      *
005000010312     C                   Z-ADD     0             V1DTAL
005100010312      *
005200900509     C     FOR01         TAG
005300010312      *
005400010326     C                   EXFMT     LSE1D01
005500010312     C                   SETOFF                                       28
005600010312      *
005700010312      * F03 - FINE LAVORO
005800900509     C   KC              GOTO      FINE
005900010312      *
006000010312      * CONTROLLO
006100900524     C                   EXSR      CONTR
006200941227     C  NKF
006300010312     COR 28              GOTO      FOR01
006400010312      *
006500010312      * F06 - ELABORAZIONE
006600950308     C                   MOVEL     *BLANKS       DS1FPG
006700010329     C                   Z-ADD     V1DLNP        DS1LNP
006800010312     C                   Z-ADD     V1DTDA        DS1GDA
006900010312     C                   Z-ADD     V1DTAL        DS1GAL
007000010312     C                   Z-ADD     VDTDA         DS1ADA
007100010312     C                   Z-ADD     VDTAL         DS1AAL
007200950308     C                   MOVEL     DSLS01        KPJBU
007300010327     C                   MOVEL     V1CONF        SINO
007400010329     C                   MOVEL     'LSE2'        KCOAZ
007500010329     C                   CALL      'BCH10'
007600010329     C******!!!!!!!!!!     CALL 'FNLSE2R'
007700900523     C                   PARM                    KPJBA
007800010312      *
007900000000     C     FINE          TAG
008000010312      *
008100000000     C                   SETON                                        LR
008200010312      *---------------------------------------------------------------*
008300010312      *                    CONTR                                      *
008400010312      * ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
008500010312      *---------------------------------------------------------------*
008600900524     C     CONTR         BEGSR
008700010312      *
008800010312     C                   SETOFF                                       4142
008900010312      *
009000010312      * DATA DAL OBBLIGATORIA
009100010312     C     V1DTDA        IFEQ      *ZEROS
009200010312     C                   SETON                                        41  28
009300010312     C                   MOVEL     MSG(1)        V1CMSG
009400010312     C                   GOTO      ENDCTR
009500010312     C                   ENDIF
009600010312      *
009700010312      * CONTROLLO DATA DAL
009800010312     C                   CLEAR                   WLBDAT
009900010312     C                   MOVE      V1DTDA        G02DAT
010000921002     C                   MOVEL     *BLANK        G02ERR
010100941227     C                   CALL      'XSRDA8'
010200921002     C                   PARM                    WLBDAT
010300010312     C     G02ERR        IFEQ      '1'
010400010312     C                   SETON                                        41  28
010500010312     C                   MOVEL     MSG(2)        V1CMSG
010600921002     C                   GOTO      ENDCTR
010700010312     C                   ENDIF
010800010312     C                   Z-ADD     G02INV        VDTDA
010900010312     C                   Z-ADD     G02DAT        V1DTDA
011000010312      *
011100010312      * CONTROLLO DATA AL
011200010312     C     V1DTAL        IFNE      0
011300010312     C                   CLEAR                   WLBDAT
011400010312     C                   MOVE      V1DTAL        G02DAT
011500921002     C                   MOVEL     *BLANK        G02ERR
011600941227     C                   CALL      'XSRDA8'
011700921002     C                   PARM                    WLBDAT
011800010312     C     G02ERR        IFEQ      '1'
011900010312     C                   SETON                                        42  28
012000010312     C                   MOVEL     MSG(2)        V1CMSG
012100921002     C                   GOTO      ENDCTR
012200010312     C                   ENDIF
012300010312     C                   Z-ADD     G02INV        VDTAL             8 0
012400010312     C                   Z-ADD     G02DAT        V1DTAL
012500010312     C                   ENDIF
012600010312      *
012700010312      * DATA DAL MINORE DATA AL
012800010312     C     VDTDA         IFGT      VDTAL
012900010312     C     VDTAL         ANDNE     *ZEROS
013000010312     C                   SETON                                        4128
013100010312     C                   MOVEL     MSG(4)        V1CMSG
013200930913     C                   GOTO      ENDCTR
013300010312     C                   ENDIF
013400010312      *
013500900524     C     ENDCTR        ENDSR
013600010312      *---------------------------------------------------------------*
013700010312      *                    CARTAB                                     *
013800010312      * ROUTINE DI CARICAMENTO FILIALI MECCANIZZATE                   *
013900010312      *---------------------------------------------------------------*
014000921002     C     CARTAB        BEGSR
014100010312      *
014200970901     C                   CLEAR                   DSUL06
014300970901     C                   MOVE      '£1'          D06COD
014400970901     C                   MOVEL     SIMFEL        D06KEY
014500970901     C                   MOVEL     'L'           D06TLA
014600000103     C                   MOVEL     DSUL06        KPJBU
014700010312      *
014800970901     C                   CALL      'TRUL06R'
014900970901     C                   PARM                    KPJBA
015000970901     C                   MOVEL     KPJBU         DSUL06
015100010312     C                   MOVEA     LIN           L1
015200010312      *
015300020429      * VEDO SE SONO PRIMO O SECONDO LIVELLO
015400020429     C                   Z-ADD     SIMPOU        V1DLNP
015500020429     C                   Z-ADD     SIMPOU        DS1FGS
015600020429     C     SIMTPP        IFEQ      '2'
015700020429     C     SIMTPP        OREQ      *BLANKS
015800020429     C                   SETON                                        05
015900020429     C                   ENDIF
016000020429     C***        REM       IFEQ 'REM'
016100020429     C***        REMFIL    ANDGT*ZEROS
016200020429     C***                  MOVELREMFIL    V1DLNP
016300020429     C***                  MOVELREMFIL    DS1FGS
016400020429     C***                  SETON                     05
016500020429     C***                  ELSE
016600020429     C***                  MOVELSIMFEL    V1DLNP
016700020429     C***                  MOVELSIMFEL    DS1FGS
016800020429     C***                  END
016900010312      *
017000930913     C                   ENDSR
017100010312      *---------------------------------------------------------------*
017200010312      *                    DEFVAR                                     *
017300010312      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
017400010312      *---------------------------------------------------------------*
017500930913     C     DEFVAR        BEGSR
017600010312      *
017700930913     C     *ENTRY        PLIST
017800930913     C                   PARM                    KPJBA
017900010312      *
018000930913     C                   Z-ADD     1             CODUT
018100930913     C                   CALL      'X§PARUT'
018200930913     C                   PARM                    UT§DSE
018300930913     C                   MOVEL     RAGUT         RSUT             20
018400930913     C                   MOVEL     REC80         CNCR80
018500010312      *
018600010312      * Reperisce data e ora
018700010312     C                   TIME                    W0140            14 0
018800010312      * UDATE IN GGMMAAAA
018900010312     C                   MOVE      W0140         WDTGIO            8 0
019000010312      * UDATE IN AAAAMMGG
019100010312     C                   CLEAR                   WLBDAT
019200010312     C                   Z-ADD     WDTGIO        G02DAT
019300010312     C                   MOVEL     *BLANK        G02ERR
019400010312     C                   CALL      'XSRDA8'
019500010312     C                   PARM                    WLBDAT
019600010312     C                   Z-ADD     G02INV        VDTDA             8 0
019700010312     C                   MOVEL     G02DAT        V1DTDA
019800010312      *
019900930913     C                   ENDSR
020000010312      *-------------------------------------------------------------------------
020100010312** MSG  Lungh. 78                                                            *
020200010312Inserire la data
020300010312Data errata
020400010312P.O. gestione errato
020500010312Range data errato
