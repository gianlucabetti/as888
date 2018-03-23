000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060406      * FNLSE7R *-----------------------------------------------------*
000300060406      *            - LISTA PREPAGATI NON INCASSATI -  FILTRO          *
000400010312      *---------------------------------------------------------------*
000500010327      *___________________________________________________________________*
000600010327      *                                                                   *
000700010329      * ATTENZIONE!!!!!! NELL'ULTIMA POSIZIONE DELLA KPJBA (502)          *
000800010329      *                  PASSO UN FLAG PER LA STAMPA DEI PREPAGATI        *
000900010329      *                  ANCORA DA CONFERMARE                             *
001000010327      *___________________________________________________________________*
001100010327      *
001200060406     FFNLSE7D   CF   E             WORKSTN
001300010312      *
001400010312     D L1              S              3  0 DIM(30)
001500010312     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001600010312      *
001700950308     D DSLS01        E DS                  INZ
001800060406     d Azuteds       e ds                  Extname(Azute00f)
001900060406     d dDatiute      e ds
002000060406     d Tibs34ds      e ds                  Inz
002100930913     D KPJBA         E DS
002200010329     D  SINO                 502    502
002300010312      * DS PER TRUL06R - CARICAMENTO £X
002400060406     D trul06ds      E DS
002500970901     D  LIN                    1     90  0
002600970901     D                                     DIM(30)
002700010312      *---------------------------------------------------------------*
002800010312      *                    MAIN                                       *
002900010312      * PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
003000010312      *---------------------------------------------------------------*
003100010312      *
003200010312      * DEFINIZIONE VARIABILI, KLIST E PLIST
003300930913     C                   EXSR      DEFVAR
003400010312      *
003500010312      * CARICAMENTO FILIALI MECCANIZZATE
003600921002     C                   EXSR      CARTAB
003700010312      *
003800900509     C     FOR01         TAG
003900010312      *
004000060406     C                   EXFMT     LSE7D01
004100010312     C                   SETOFF                                       28
004200010312      *
004300010312      * F03 - FINE LAVORO
004400900509     C   KC              GOTO      FINE
004500010312      *
004600941227     C  NKF
004700010312     COR 28              GOTO      FOR01
004800010312      *
004900010312      * F06 - ELABORAZIONE
005000950308     C                   MOVEL     *BLANKS       DS1FPG
005100010329     C                   Z-ADD     V1DLNP        DS1LNP
005200010312     C                   Z-ADD     V1DTDA        DS1GDA
005300010312     C                   Z-ADD     V1DTAL        DS1GAL
005400010312     C                   Z-ADD     VDTDA         DS1ADA
005500010312     C                   Z-ADD     VDTAL         DS1AAL
005600950308     C                   MOVEL     DSLS01        KPJBU
005700010327     C                   MOVEL     V1CONF        SINO
005800010329     C                   MOVEL     'LSE2'        KCOAZ
005900010329     C                   CALL      'BCH10'
006000010329     C******!!!!!!!!!!     CALL 'FNLSE2R'
006100900523     C                   PARM                    KPJBA
006200010312      *
006300000000     C     FINE          TAG
006400010312      *
006500000000     C                   SETON                                        LR
006600010312      *---------------------------------------------------------------*
006700010312      *                    CARTAB                                     *
006800060406      * ROUTINE DI CARICAMENTO tabelle                                *
006900010312      *---------------------------------------------------------------*
007000921002     C     CARTAB        BEGSR
007100010312      *
007200060406     C                   CLEAR                   trul06ds
007300970901     C                   MOVE      '£1'          D06COD
007400970901     C                   MOVEL     SIMFEL        D06KEY
007500970901     C                   MOVEL     'L'           D06TLA
007600060406     C                   MOVEL     trul06ds      KPJBU
007700010312      *
007800970901     C                   CALL      'TRUL06R'
007900970901     C                   PARM                    KPJBA
008000060406     C                   MOVEL     KPJBU         trul06ds
008100010312     C                   MOVEA     LIN           L1
008200010312      *
008300020429      * VEDO SE SONO PRIMO O SECONDO LIVELLO
008400060406     C                   Z-ADD     dutpou        V1DLNP
008500060406     C                   Z-ADD     dutpou        DS1FGS
008600060406     C     dutlpo        IFEQ      '2'
008700060406     C     dutlpo        OREQ      *BLANKS
008800020429     C                   SETON                                        05
008900020429     C                   ENDIF
009000010312      *
009100930913     C                   ENDSR
009200010312      *---------------------------------------------------------------*
009300010312      *                    DEFVAR                                     *
009400010312      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
009500010312      *---------------------------------------------------------------*
009600930913     C     DEFVAR        BEGSR
009700010312      *
009800930913     C     *ENTRY        PLIST
009900930913     C                   PARM                    KPJBA
010000010312      *
010100060406     c     *dtaara       define    §azute        azuteds
010200060406     c     *dtaara       define    §datiute      ddatiute
010300060406     c                   in(E)     *dtaara
010400060406     c                   If        %error  or RSUT = *blanks
010500060406     c                   CLEAR                   tibs34ds
010600060406     c                   CALL      'TIBS34R'
010700060406     c                   PARM                    tibs34ds
010800060406     c                   in        *dtaara
010900060406     c                   EndIf
011000010312      *
011100010312      *
011200930913     C                   ENDSR
011300010312      *-------------------------------------------------------------------------
011400010312** MSG  Lungh. 78                                                            *
011500010312P.O. gestione errato
