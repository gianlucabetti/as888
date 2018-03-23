000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200040607     H* FNLRV2R *-----------------------------------------------------*
000300040607     H*   Controllo colli di valore al 2 livello in arrivo
000400930126     H*---------------------------------------------------------------*
000500040607     FFNLRv2d   CF   E             WORKSTN
000600921109     D*
000700040607     D* passaggio dati a batch - FNLRV3R -
000800951114     D*
000900040607     D PARA02          DS
001000040607     D  P02dam                 1      8  0
001100040607     D  P02fgs                 9     11  0
001200040607     D  P02invdam             12     19  0
001300951115     D*
001400911010     D WLBDAT          DS
001500941221     D  G02DAT                 1      8  0
001600941221     D  G02INV                 9     16  0
001700941221     D  G02ERR                17     17
001800941221     D  G02TGI                18     22  0
001900040607     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MESSAGGI ERRORE
002000040604     D CNCR80        E DS
002100040604     D KPJBA         E DS
002200030722     D UT§DSE0F      E DS
002300000000     C*---------------------------------------------------------------*
002400911010     C* INDICATORI
002500911010     C*---------------------------------------------------------------*
002600951115     C*  28    - EMETTE CAMPO MESSAGGI
002700040607     C*  40    - ERRORE
002800911010     C*  90    - ERRORE GENERICO
002900911010     C*---------------------------------------------------------------*
003000000000     C     *ENTRY        PLIST
003100000000     C                   PARM                    KPJBA
003200941221     C*
003300000000     C                   Z-ADD     1             CODUT
003400911010     C                   CALL      'X§PARUT'
003500030722     C                   PARM                    UT§DSE0F
003600920901     C                   MOVEL     REC80         CNCR80
003700941221     C                   MOVEL     RAGUT         RSUT
003800900511     C*---------------------------------------------------------------*
003900911008     C                   EXSR      PULIZ
004000971223     C*
004100921123     C     FOR01         TAG
004200911008     C*
004300040607     C                   EXFMT     LRv2D01
004400040607     C*
004500040607     C                   SETOFF                                       409028
004600030722     C** F3 - FINE LAVORO
004700040604     C   KC              GOTO      FINE
004800921117     C*
004900040528     C* CONTROLLO FOGLIO ARRIVI
005000911008     C                   EXSR      CONTR1
005100930127     C* SE ERRORE
005200040607     c  nkf
005300040607     Cor 90              GOTO      FOR01
005400040528     c
005500030722     C* F6  - ELABORAZIONE
005600040607     c                   if        *inkf
005700040607     C                   eval      p02dam=v1cdam
005800040607     C                   eval      p02invdam=g02inv
005900040607     C                   MOVEL     para02        KPJBU
006000040607     C                   MOVEL     'LRV3'        KCOAZ
006100040607     C                   CALL      'BCH10'
006200040607     C                   PARM                    KPJBA
006300040604     c                   endif
006400040604     c*
006500000000     C     FINE          TAG
006600960202     C*
006700000000     C                   SETON                                        LR
006800941216     C**************************************************************************
006900941216     C*    PULIZIA CAMPI
007000941216     C**************************************************************************
007100911008     C     PULIZ         BEGSR
007200941216     C*
007300920709     C* PRIMA VIDEATA
007400040607     C                   Z-ADD     SIMPOU        p02FGS
007500040607     C                   TIME                    w0140            14 0          ORA E DATA
007600040607     C                   MOVE      w0140         v1cdam                         GG/MM/AAAA
007700040528     c
007800040604     C                   TIME                    UTIME             6 0          ORA E DATA
007900040607     c*
008000911008     C                   ENDSR
008100960205     C**************************************************************************
008200040607     C*    CONTROLLI data arrivo  colli
008300960205     C**************************************************************************
008400960205     C     CONTR1        BEGSR
008500040607     C* data arrivo
008600040607     C*
008700040607     C                   Z-ADD     V1cdam        G02DAT
008800040607     C                   CLEAR                   G02ERR
008900040607     C                   CALL      'XSRDA8'
009000040607     C                   PARM                    WLBDAT
009100040607     C*
009200040607     C* Data errata
009300040607    1C     G02ERR        IFEQ      '1'
009400040607     C                   MOVEL     msg(1)        VIDMSG
009500040607     C                   SETON                                        409028
009600040607     c                   else
009700040607     c                   z-add     g02dat        v1cdam
009800040607    1C                   ENDIF
009900040607     C                   ENDSR
010000000606**  MSG
010100040607Data arrivo colli errata                                                      1
