000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200040607     H* FNLRV2R *-----------------------------------------------------*
000300040607     H*   Controllo colli di valore al 2 livello in arrivo
000400930126     H*---------------------------------------------------------------*
000500040611     FFNLRv4d   CF   E             WORKSTN
000600040611     FAZORG01L  IF   E           K DISK
000700921109     D*
000800040611     D* passaggio dati a batch - FNLR35R -
000900951114     D*
001000951115     D*
001100911010     D WLBDAT          DS
001200941221     D  G02DAT                 1      8  0
001300941221     D  G02INV                 9     16  0
001400941221     D  G02ERR                17     17
001500941221     D  G02TGI                18     22  0
001600040611     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MESSAGGI ERRORE
001700040604     D CNCR80        E DS
001800040604     D KPJBA         E DS
001900030722     D UT§DSE0F      E DS
002000040611      * Parametri per richiamo a TRUL09R
002100040611     d Trul09ds      e ds
002200040611     d  po2liv                19    108  0 dim(30)
002300040611     d fnlv55DS      e ds
002400040611     d fnlr34ds      e ds
002500040614     D  spo                  140    229  0
002600040614     D                                     DIM(30)                              ANOMALIE VIDEO
002700040614     D*
002800040614     D* ds per pgm trul06r - caricamento £x
002900040614     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003000040614     D  LIN                    1     90  0
003100040614     D                                     DIM(30)                              COMODO
003200000000     C*---------------------------------------------------------------*
003300911010     C* INDICATORI
003400911010     C*---------------------------------------------------------------*
003500951115     C*  28    - EMETTE CAMPO MESSAGGI
003600040607     C*  40    - ERRORE
003700911010     C*  90    - ERRORE GENERICO
003800911010     C*---------------------------------------------------------------*
003900000000     C     *ENTRY        PLIST
004000000000     C                   PARM                    KPJBA
004100941221     C*
004200000000     C                   Z-ADD     1             CODUT
004300911010     C                   CALL      'X§PARUT'
004400030722     C                   PARM                    UT§DSE0F
004500920901     C                   MOVEL     REC80         CNCR80
004600941221     C                   MOVEL     RAGUT         RSUT
004700900511     C*---------------------------------------------------------------*
004800911008     C                   EXSR      PULIZ
004900971223     C*
005000921123     C     FOR01         TAG
005100911008     C*
005200040611     C                   EXFMT     LRV4D01
005300040607     C*
005400040607     C                   SETOFF                                       409028
005500030722     C** F3 - FINE LAVORO
005600040604     C   KC              GOTO      FINE
005700921117     C*
005800040611     C* CONTROLLO
005900911008     C                   EXSR      CONTR1
006000930127     C* SE ERRORE
006100040607     c  nkf
006200040607     Cor 90              GOTO      FOR01
006300040528     c
006400030722     C* F6  - ELABORAZIONE
006500040607     c                   if        *inkf
006600040611     c* imposto la DS
006700040611     c                   clear                   fnlr34ds
006800040611     c                   eval      d34ges='S'
006900040611     c                   eval      d34bva='V'
007000040611     c                   eval      d34par=vidtbo
007100040611     c                   eval      d34ie='D'
007200040611     c                   eval      d34dlg=viddam
007300040611     c                   eval      d34dla=g02inv
007400040611     c                   eval      d34apr='S'
007500040611     c                   eval      d34inc='I'
007600040611     c                   movel     '005015'      d34anm
007700040615     c                   EVAL      d34stv='S'
007800040616     c                   EVAL      d34sl0='S'
007900040611     c
008000040611     c                   if        vidfil<>0
008100040617     c   10              z-add     vidfil        d34fgs
008200040617     C  N10              Z-ADD     VIDfgs        d34fgs
008300040617     c  n10              movel     vidfil        d34spo
008400040614     c                   else
008500040615     c                   z-add     vidfgs        d34fgs
008600040614     c*
008700040614     c                   if        d34par='P' or d34par=' '
008800040614     C                   CLEAR                   DSUL06
008900040614     c                   movel     'L'           d06tla
009000040614     C                   MOVE      '£1'          D06COD
009100040614     C                   MOVEL     *BLANKS       D06TLA
009200040614     C                   MOVEL     SIMFEL        D06KEY
009300040614     c                   z-add     g02inv        d06drf
009400040614     C                   MOVEL     DSUL06        KPJBU
009500040614     C                   CALL      'TRUL06R'
009600040614     C                   PARM                    KPJBA
009700040614     C                   MOVEL     KPJBU         DSUL06
009800040614     C                   MOVEA     LIN           spo
009900040614     c                   else
010000040614     c* terminal di arrivo
010100040614     c* carico tutti i p.o. 2 livello in arrivo del terminal
010200040614     c                   clear                   trul09ds
010300040614     c                   eval      i09mod='TFA'
010400040614     c                   eval      i09tfa=vidfgs
010500040614     c                   eval      i09tla='L'
010600040614     c                   eval      i09dta=g02inv
010700040614     c                   call      'TRUL09R'
010800040614     c                   parm                    trul09ds
010900040614     C                   MOVEA     po2liv        spo
011000040611     c                   endif
011100040614     c                   endif
011200040611     c
011300040611     c                   movel     fnlr34ds      kpjbu
011400040611     C                   MOVEL     'LR35'        KCOAZ
011500040607     C                   CALL      'BCH10'
011600040607     C                   PARM                    KPJBA
011700040604     c                   endif
011800040604     c*
011900000000     C     FINE          TAG
012000040611     c                   clear                   fnlv55ds
012100040611     c                   eval      d55tla='C'
012200040611     C                   CALL      'FNLV55R'
012300040611     C                   PARM                    fnlv55ds
012400960202     C*
012500000000     C                   SETON                                        LR
012600941216     C**************************************************************************
012700941216     C*    PULIZIA CAMPI
012800941216     C**************************************************************************
012900911008     C     PULIZ         BEGSR
013000040611     c                   setoff                                       10
013100941216     C*
013200920709     C* PRIMA VIDEATA
013300040611     C                   Z-ADD     SIMPOU        vidfgs
013400040611     c     simpou        chain     azorg01l
013500040611     c                   movel     orgdes        desfgs
013600040611     c*
013700040611     c                   eval      vidtbo='P'
013800040607     C                   TIME                    w0140            14 0          ORA E DATA
013900040611     C                   MOVE      w0140         viddam                         GG/MM/AAAA
014000040528     c
014100040604     C                   TIME                    UTIME             6 0          ORA E DATA
014200040611     c
014300040611     c* Se 2 livello propongo p.o. non modificabile
014400040611     c     simtpp        ifeq      '2'
014500040611     c     simtpp        oreq      *blanks
014600040611     c                   eval      vidfil=simpou
014700040611     c                   movel     orgdes        desfgs
014800040611     c                   seton                                        10
014900040611     c                   endif
015000040607     c*
015100911008     C                   ENDSR
015200960205     C**************************************************************************
015300040607     C*    CONTROLLI data arrivo  colli
015400960205     C**************************************************************************
015500960205     C     CONTR1        BEGSR
015600040607     C*
015700040611     C* Data arrivo
015800040611     C                   Z-ADD     Viddam        G02DAT
015900040607     C                   CLEAR                   G02ERR
016000040607     C                   CALL      'XSRDA8'
016100040607     C                   PARM                    WLBDAT
016200040607     C*
016300040607     C* Data errata
016400040607    1C     G02ERR        IFEQ      '1'
016500040607     C                   MOVEL     msg(1)        VIDMSG
016600040611     C                   SETON                                        419028
016700040607     c                   else
016800040611     c                   z-add     g02dat        viddam
016900040607    1C                   ENDIF
017000040611     c* controllo p.o.
017100040611     c  n10              if        vidfil<>0
017200040611     c                   if        vidtbo='P' or vidtbo=' '
017300040611     c* Deve essere un p.o. del terminal di partenza del p.o. gestione
017400040611     c                   clear                   fnlv55ds
017500040611     C                   MOVEL     'P'           D55TPT
017600040611     C                   MOVEL     vidfil        D55LIN
017700040611     C                   MOVEL     g02inv        D55DRF
017800040611     C                   CALL      'FNLV55R'
017900040611     C                   PARM                    fnlv55ds
018000040611     c*
018100040611     c                   if        d55err<>*blanks or d55tfp<>vidfgs
018200040611     C                   MOVEL     msg(2)        VIDMSG
018300040611     C                   SETON                                        409028
018400040611     c                   endif
018500040611     c*
018600040611   x2c                   else
018700040614     c* carico tutti i p.o. 2 livello in arrivo del terminal
018800040611     c                   clear                   trul09ds
018900040611     c                   eval      i09mod='TFA'
019000040611     c                   eval      i09tfa=vidfgs
019100040611     c                   eval      i09tla='L'
019200040611     c                   eval      i09dta=g02inv
019300040611     c                   call      'TRUL09R'
019400040611     c                   parm                    trul09ds
019500040611     c
019600040611     c     vidfil        lookup    po2liv                                 30
019700040611    3c                   if        not *in30
019800040611     C                   MOVEL     msg(3)        VIDMSG
019900040611     C                   SETON                                        409028
020000040611    3c                   endif
020100040611    2c                   endif
020200040611    1c                   endif
020300040611     c
020400040611     C     endct1        ENDSR
020500000606**  MSG
020600040607Data arrivo colli errata                                                      1
020700040611Il Punto operativo non è un 2 livello in partenza del P.O. in gestione
020800040611Il Punto operativo non è un 2 livello in arrivo   del P.O. in gestione
