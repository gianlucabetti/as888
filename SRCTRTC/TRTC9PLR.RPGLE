000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200111222     FEDTAB01L  IF   E           K DISK
000300141022     Fazorg01L  iF   E           K DISK
000400940224      *
000500141014     FTRTC9PLD  CF   E             WORKSTN
000600940128     F*----------------------------------------------------*
000700940211     D* Passaggio Parametri
000800940211     D KPJBA         E DS
000900111220     D UTEDSE0F      E DS
001000141014     D EDIDSPT       E DS
001100030113      *-------------
001200940325     D* Parametri in ricezione
001300141017     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
001400141021      *
001500141021     D WLBDA8          DS
001600141021     D  G02DAT                 1      8  0
001700141021     D  G02INV                 9     16  0
001800141021     D  G02ERR                17     17
001900141021     D  G02TGI                18     22  0
002000141022     D*-------------
002100141022     D OG143         E DS
002200141022     D*-------------
002300141022     D COD_PT          s             35    DIM(100)
002400141022     D LNA_PT          s              3S 0 DIM(100)
002500141022     D UNI_pt          s             90    DIM(100)
002600141021      *
002700131011     C*----------------------------------------------------*
002800141017     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002900940127     C*----------------------------------------------------*
003000940127     C*                MAIN LINE PROGRAM
003100940127     C*----------------------------------------------------*
003200111222      * Definizione chiavi
003300111222     C     KTAB          KLIST
003400111222     C                   KFLD                    KCOD
003500111222     C                   KFLD                    KKEY
003600111222      *
003700111222     C     *LIKE         DEFINE    TABCOD        KCOD
003800111222     C     *LIKE         DEFINE    TABKEY        KKEY
003900111222      *
004000111221     C* Reperimento parametri
004100111221     C     *ENTRY        PLIST
004200111221     C                   PARM                    KPJBA
004300141022     C*
004400111221     c                   write     z0
004500141022     C*
004600141022     c     video         tag
004700111221     c                   exfmt     d0
004800141022     C*
004900111221     c                   if        not *INKC
005000111221     C*
005100141022     c                   if        D0LNA = *zeros
005200141017     C                   CLEAR                   TRTC91
005300111221     C                   MOVEL     'PT'          D91COD
005400141017     C                   MOVEL     TRTC91        KPJBU
005500111221     C                   CALL      'TRTC9PTR'
005600111221     C                   PARM                    KPJBA
005700111221     C                   MOVEL     KPJBU         TRTC91
005800111222     C*  decodifica quello che ha il cliente configurato sulla sua tabella PARTNER
005900111222      *
006000141022     C*  Aggancio TABELLA PT
006100141014     C                   MOVEL     'PT'          KCOD
006200111222     C                   MOVEL     D91KEY        KKEY
006300111222     C     KTAB          CHAIN     EDTAB01L
006400111222     c                   if        %Found(EDTAB01L)
006500141014     C                   MOVEL     TABUNI        EDIDSPT
006600141017     C                   if        §PTLNP   <> *zeros
006700141022     C                   move      *zeros        D0LNA
006800141022     C                   move      §PTlnp        D0LNA
006900111222     c                   end
007000111222     c                   end
007100111222     C*
007200111221     c                   end
007300111221     C*
007400141022     c                   if        D0LNA >0
007500141022     C*
007600141022     C* Controlla che sia una LINEA di PARTNER EEX x Export Manifest
007700141022      * LINEA EEX
007800141022     C                   Z-ADD     1             XX                3 0
007900141022     c                   setoff                                       9956
008000141022     C     d0LNA         LOOKUP    LNA_PT(XX)                             99
008100141022     c                   if        not *in99
008200141022     c                   seton                                        56
008300141022     c                   goto      video
008400141022     c                   end
008500141022     C*
008600111221     C                   RESET                   tabds
008700141021     c                   move      *zero         xtaopz
008800111221     C                   MOVE      *ZERO         xtaret
008900111221     C                   MOVE      *ZERO         xtaopr
009000141022     C                   MOVEl     D0LNA         XTAKLNA
009100141017     C                   MOVEl     *blank        XTAKLUO
009200141017     C                   MOVEl     *zeros        XTAKDAT
009300141017     C                   MOVEl     *zeros        XTANREC
009400141021      *
009500141021      *  ultimi 2 parametri x il test
009600141021     c                   write     z0
009700141021     c     video1        tag
009800141021     c                   exfmt     d1
009900141021     c                   if        *INKC
010000141021     c                   goto      fine
010100141021     c                   end
010200141021     c                   if        d1opz <> *blank
010300141021     C                   MOVE      d1opz         xtaopz
010400141021     c                   end
010500141021      *
010600141021      *  controllo della data
010700141021     c                   if        d1data > 0
010800141021     C                   MOVE      d1data        G02DAT
010900141021     C                   MOVEL     *BLANK        G02ERR
011000141021     C                   CALL      'XSRDA8'
011100141021     C                   PARM                    WLBDA8
011200141021     C     G02ERR        IFEQ      '1'
011300141021     C                   SETON                                          21
011400141021     C                   goto      video1
011500141021     C                   END
011600141021     c                   move      G02INV        XTAKDAT
011700141021     c                   end
011800141021      *
011900111221     C                   MOVEL(p)  tabds         KPJBU
012000141017$004 C                   CALL      'TRTC9PLR1'
012100111221     C                   PARM                    KPJBA
012200141017     C                   MOVEL     KPJBU         tabds
012300141017     c                   else
012400141017     c                   goto      video
012500141017     c                   end
012600111221      *
012700111221     c                   end
012800940325     C* fine programma
012900141021     c     fine          tag
013000940325     C                   SETON                                            LR
013100141022     C************************************************************
013200141022     C* OPERAZIONI INIZIALI
013300141022     C************************************************************
013400141022     C     *INZSR        BEGSR
013500141022     C*
013600141022      *------------
013700141022     C* Caricamento Tabella Partner esteri
013800141022     C                   Z-ADD     0             X                 3 0
013900141022     C                   MOVEL     'PT'          TABCOD
014000141022     C     TABCOD        CHAIN     EDTAB01L                           30
014100141022     C     *IN30         DOWEQ     '0'
014200141022      *
014300141022     C     TABFLG        IFEQ      *BLANKS
014400141022     c                   movel     tabUNI        edidsPT
014500141022      *
014600141022     c     §ptLNP        chain     azorg01l
014700141022     c                   if        %Found(azorg01l)
014800141022     c                   movel     orgde3        og143
014900141022      *  Solo x PARTNER EEX
015000141022     c                   if        §OGNTW = 'EEX'
015100141022     C                   ADD       1             X
015200141022     C                   MOVEL     TABKEY        COD_PT(X)
015300141022     C                   MOVEL     TABUNI        UNI_pt(X)
015400141022     C                   MOVEL     TABUNI        edidsPT
015500141022     C                   MOVEL     §ptLNP        LNA_pt(X)
015600141022     c                   end
015700141022     c                   end
015800141022      *
015900141022     C                   END
016000141022     C     TABCOD        READE     EDTAB01L                               30
016100141022     C                   END
016200141022      *------------
016300141022     C                   ENDSR
016400030113     C************************************************************
