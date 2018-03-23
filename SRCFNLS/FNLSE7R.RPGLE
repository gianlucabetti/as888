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
002300060407     d fnlsc4ds      E DS
002400010312      * DS PER TRUL06R - CARICAMENTO £X
002500060406     D trul06ds      E DS
002600970901     D  LIN                    1     90  0
002700970901     D                                     DIM(30)
002800010312      *---------------------------------------------------------------*
002900010312      *                    MAIN                                       *
003000010312      * PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
003100010312      *---------------------------------------------------------------*
003200010312      *
003300010312      * DEFINIZIONE VARIABILI, KLIST E PLIST
003400930913     C                   EXSR      DEFVAR
003500010312      *
003600010312      * CARICAMENTO FILIALI MECCANIZZATE
003700921002     C                   EXSR      CARTAB
003800010312      *
003900900509     C     FOR01         TAG
004000010312      *
004100060406     C                   EXFMT     LSE7D01
004200010312     C                   SETOFF                                       28
004300010312      *
004400010312      * F03 - FINE LAVORO
004500900509     C   KC              GOTO      FINE
004600060407     c* controllo il p.o. immesso
004700060407     C* LINEA DI PARTENZA
004800060407     C                   if        v1dlnp <> 0
004900060407     C                   CLEAR                   FNLSC4ds
005000060407     C                   Z-ADD     V1DLNP        IC4LIN
005100060407     C                   MOVE      'I'           IC4FLG
005200060407     C                   MOVEL     FNLSC4ds      KPJBU
005300060407     C                   CALL      'FNLSC4R1'
005400060407     C                   PARM                    KPJBA
005500060407     C                   MOVEL     KPJBU         FNLSC4ds
005600060407      *
005700060407     C                   if        oc4ese <> 'S'
005800060407     C                   SETON                                        4328
005900060407     C                   ENDIF
006000060407     C                   ENDIF
006100060407     c
006200010312      *
006300941227     C  NKF
006400010312     COR 28              GOTO      FOR01
006500010312      *
006600010312      * F06 - ELABORAZIONE
006700950308     C                   MOVEL     *BLANKS       DS1FPG
006800010329     C                   Z-ADD     V1DLNP        DS1LNP
006900950308     C                   MOVEL     DSLS01        KPJBU
007000010327     C                   MOVEL     V1CONF        SINO
007100060407     C                   MOVEL     'LSE8'        KCOAZ
007200010329     C                   CALL      'BCH10'
007300900523     C                   PARM                    KPJBA
007400010312      *
007500000000     C     FINE          TAG
007600010312      *
007700000000     C                   SETON                                        LR
007800010312      *---------------------------------------------------------------*
007900010312      *                    CARTAB                                     *
008000060406      * ROUTINE DI CARICAMENTO tabelle                                *
008100010312      *---------------------------------------------------------------*
008200921002     C     CARTAB        BEGSR
008300010312      *
008400060406     C                   CLEAR                   trul06ds
008500970901     C                   MOVE      '£1'          D06COD
008600970901     C                   MOVEL     SIMFEL        D06KEY
008700970901     C                   MOVEL     'L'           D06TLA
008800060406     C                   MOVEL     trul06ds      KPJBU
008900010312      *
009000970901     C                   CALL      'TRUL06R'
009100970901     C                   PARM                    KPJBA
009200060406     C                   MOVEL     KPJBU         trul06ds
009300010312     C                   MOVEA     LIN           L1
009400010312      *
009500020429      * VEDO SE SONO PRIMO O SECONDO LIVELLO
009600060406     C                   Z-ADD     dutpou        V1DLNP
009700060406     C                   Z-ADD     dutpou        DS1FGS
009800060406     C     dutlpo        IFEQ      '2'
009900060406     C     dutlpo        OREQ      *BLANKS
010000020429     C                   SETON                                        05
010100020429     C                   ENDIF
010200010312      *
010300930913     C                   ENDSR
010400010312      *---------------------------------------------------------------*
010500010312      *                    DEFVAR                                     *
010600010312      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
010700010312      *---------------------------------------------------------------*
010800930913     C     DEFVAR        BEGSR
010900010312      *
011000930913     C     *ENTRY        PLIST
011100930913     C                   PARM                    KPJBA
011200010312      *
011300060406     c     *dtaara       define    §azute        azuteds
011400060406     c     *dtaara       define    §datiute      ddatiute
011500060406     c                   in(E)     *dtaara
011600060406     c                   If        %error  or RSUT = *blanks
011700060406     c                   CLEAR                   tibs34ds
011800060406     c                   CALL      'TIBS34R'
011900060406     c                   PARM                    tibs34ds
012000060406     c                   in        *dtaara
012100060406     c                   EndIf
012200010312      *
012300010312      *
012400930913     C                   ENDSR
012500010312      *-------------------------------------------------------------------------
012600010312** MSG  Lungh. 78                                                            *
012700010312P.O. gestione errato
