000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200951011     H* FNLR17R *----------------------------------------------------*
000300951011     H*     - CALCOLO DATA DI ARRIVO PER COLLI LOCALI
000400000000     H*--------------------------------------------------------------*
000500020906     F**!!!FNFVV02L  IF   E           K DISK    USROPN
000600020906     fFnfvv08l  if   e           k Disk    USROPN
000700951011     D*
000800951011     D* PARAMETRI PASSATI  --> DATA BORDERO'
000900950919     D PARAM           DS
001000951011     D* INPUT
001100951011     D  PARDBR                 1      8  0
001200951011     D* OUTPUT
001300951011     D  PARDAM                 9     16  0
001400951011     D***
001500951011     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
001600951011     D***
001700951011     D WLBDAT          DS                  INZ
001800951011     D  G02DAT                 1      8  0
001900951011     D  G02INV                 9     16  0
002000951011     D  G02ERR                17     17
002100951011     D  G02TGI                18     22  0
002200951011     D***
002300951011     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
002400951011     D***
002500951011     D WGIDAT          DS                  INZ
002600951011     D  GIODAT                 1      8  0
002700951011     D  GIOINV                 9     16  0
002800951011     D  GIOTGI                17     21  0
002900951011     D                 DS
003000951011     D  ORA                    1      2  0
003100951011     D  UTIME                  1      6  0
003200951011     D KPJBA         E DS
003300951011     D CNCR80        E DS
003400951011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500911003     C*****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800950919     C                   MOVEL     KPJBU         PARAM
003900951011     C*
004000950119     C*---------------------------------------------------------------*
004100951011     C** CONTROLLO LA DATA BORDERO' DELLA SPEDIZIONE
004200951011     C*
004300951011     C                   CLEAR                   WDAM
004400951011     C* IMPOSTO DATA ARRIVO UGUALE A:
004500951011     C*       1)UDATE SE RICEVO LE BOLLE NEL GIORNO DALLE 00 ALLE 12
004600951011     C     PARDBR        IFEQ      DATEU
004700951011     C     ORA           ANDGE     00
004800951011     C     ORA           ANDLE     12
004900951011     C                   MOVEL     DATEU         WDAM
005000951011     C*
005100951011     C                   ELSE
005200951011     C*
005300951011     C*         SE   NON RICEVO LE BOLLE NEL GIORNO DALLE 00 ALLE 12
005400951011     C*       2)DATA FOGLIO ARRIVI SE > DATA BORDERO'
005500951011     C*       3)UDATE + 1 SE DATA FOGLIO ARRIVI <= DATA BORDERO'
005600951011     C*
005700951011     C     SAVDFV        IFLE      PARDBR
005800951011     C*
005900951011     C                   MOVE      PARDBR        G02INV
006000951011     C                   MOVEL     '3'           G02ERR
006100951011     C                   CALL      'XSRDA8'
006200951011     C                   PARM                    WLBDAT
006300951011     C*
006400951011     C*  DATA BORDERO' + 1
006500951011     C     G02TGI        ADD       1             GIOTGI
006600951011     C                   CALL      'XSRGI8'
006700951011     C                   PARM                    WGIDAT
006800951011     C*
006900951011     C                   Z-ADD     GIOINV        WDAM
007000951011     C*
007100951011     C                   ELSE
007200951011     C                   Z-ADD     SAVDFV        WDAM
007300951011     C                   ENDIF
007400951011     C                   ENDIF
007500951011     C*
007600951011     C                   Z-ADD     WDAM          PARDAM
007700951011     C                   MOVEL     PARAM         KPJBU
007800951011     C                   SETON                                        RT
007900910322     C*
008000950918     C* OPERAZIONI INIZIALI ------------------------------------------*
008100951011     C     *INZSR        BEGSR
008200951011     C                   Z-ADD     1             CODUT
008300951011     C                   CALL      'X§PARUT'
008400951011     C                   PARM                    UT§DSE
008500951011     C                   MOVEL     REC80         CNCR80
008600951011     C* GIRO DATA DEL GIORNO
008700951011     C                   TIME                    W0140            14 0
008800951011     C                   MOVE      W0140         G02DAT
008900951011     C                   MOVEL     *BLANK        G02ERR
009000951011     C                   CALL      'XSRDA8'
009100951011     C                   PARM                    WLBDAT
009200951011     C                   Z-ADD     G02INV        DATEU             8 0
009300951011     C* ORA
009400951011     C                   MOVEL     W0140         UTIME             6 0
009500020906
009600020906     c     Kfvv          Klist
009700020906     c                   Kfld                    Simfel
009800020906     c                   Kfld                    Wnpg
009900950918     C*
010000950918     C* CERCO L'ULTIMO FOGLI ARRIVI PER VEDERE CHE DATA HA
010100950918     C*  SE DATA < = ALLA DATA BORDERO' METTO DATA DEL GIORNO DOPO
010200020906     C**!!!              OPEN      FNFVV02L
010300020906     c                   Open      Fnfvv08L
010400950918     C                   Z-ADD     2             WNPG
010500020906     C**!!!WNPG          SETGT     FNFVV000
010600020906     c     Kfvv          SetGt     Fnfvv08l
010700950918     C*
010800950918     C     LEGGI         TAG
010900020906     C**!!!WNPG          READPE    FNFVV000                               30
011000020917     c     Kfvv          ReadPe    Fnfvv08l                               30
011100950918     C*
011200950918     C* SE CHIUSO  COME SE NON CI FOSSE
011300950918     C  N30FVVFCF        IFNE      ' '
011400950918     C                   SETON                                        30
011500950918     C                   ENDIF
011600950918     C*
011700950918     C* SE ANNULLATO NE LEGGO UN ALTRO
011800950918     C  N30FVVATB        IFNE      ' '
011900020906     C**!!!FVVFLE        ORNE      SIMFEL
012000950918     C                   GOTO      LEGGI
012100950918     C                   ENDIF
012200950918     C*
012300950918     C* SAVDFV : DATA FOGLIO ARRIVI LETTA
012400950918     C  N30              MOVEL     FVVDFV        SAVDFV
012500950918     C*
012600950918     C   30              Z-ADD     0             SAVDFV
012700020906     C**!!!              CLOSE     FNFVV02L
012800020906     c                   Close     Fnfvv08l
012900950918     C**
013000950918     C* DEFINIZIONE CAMPI
013100950918     C**
013200951011     C     *LIKE         DEFINE    FVVNPG        WNPG
013300950918     C     *LIKE         DEFINE    FVVDFV        WDAM
013400950918     C     *LIKE         DEFINE    FVVDFV        SAVDFV
013500911107     C                   ENDSR
