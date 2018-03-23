000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200940211     H DECEDIT('0,') DATEDIT(*DMY.)
000300111222     FEDTAB01L  IF   E           K DISK
000400141022     Fazorg01L  iF   E           K DISK
000500940224      *
000600141014     FTRTC9PLD  CF   E             WORKSTN
000700940128     F*----------------------------------------------------*
000800940211     D* Passaggio Parametri
000900940211     D KPJBA         E DS
001000111220     D UTEDSE0F      E DS
001100141014     D EDIDSPT       E DS
001200030113      *-------------
001300940325     D* Parametri in ricezione
001400141017     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
001500141021      *
001600141021     D WLBDA8          DS
001700141021     D  G02DAT                 1      8  0
001800141021     D  G02INV                 9     16  0
001900141021     D  G02ERR                17     17
002000141021     D  G02TGI                18     22  0
002100141022     D*-------------
002200141022     D OG143         E DS
002300141022     D*-------------
002400141022     D COD_PT          s             35    DIM(100)
002500141022     D LNA_PT          s              3S 0 DIM(100)
002600141022     D UNI_pt          s             90    DIM(100)
002700141021      *
002800131011     C*----------------------------------------------------*
002900141017     D TRTC91        E DS                  EXTNAME(TRTC91DS)
003000940127     C*----------------------------------------------------*
003100940127     C*                MAIN LINE PROGRAM
003200940127     C*----------------------------------------------------*
003300111222      * Definizione chiavi
003400111222     C     KTAB          KLIST
003500111222     C                   KFLD                    KCOD
003600111222     C                   KFLD                    KKEY
003700111222      *
003800111222     C     *LIKE         DEFINE    TABCOD        KCOD
003900111222     C     *LIKE         DEFINE    TABKEY        KKEY
004000111222      *
004100111221     C* Reperimento parametri
004200111221     C     *ENTRY        PLIST
004300111221     C                   PARM                    KPJBA
004400141022     C*
004500111221     c                   write     z0
004600141022     C*
004700141022     c     video         tag
004800111221     c                   exfmt     d0
004900141022     C*
005000111221     c                   if        not *INKC
005100111221     C*
005200141022     c                   if        D0LNA = *zeros
005300141017     C                   CLEAR                   TRTC91
005400111221     C                   MOVEL     'PT'          D91COD
005500141017     C                   MOVEL     TRTC91        KPJBU
005600111221     C                   CALL      'TRTC9PTR'
005700111221     C                   PARM                    KPJBA
005800111221     C                   MOVEL     KPJBU         TRTC91
005900111222     C*  decodifica quello che ha il cliente configurato sulla sua tabella PARTNER
006000111222      *
006100141022     C*  Aggancio TABELLA PT
006200141014     C                   MOVEL     'PT'          KCOD
006300111222     C                   MOVEL     D91KEY        KKEY
006400111222     C     KTAB          CHAIN     EDTAB01L
006500111222     c                   if        %Found(EDTAB01L)
006600141014     C                   MOVEL     TABUNI        EDIDSPT
006700141017     C                   if        §PTLNP   <> *zeros
006800141022     C                   move      *zeros        D0LNA
006900141022     C                   move      §PTlnp        D0LNA
007000111222     c                   end
007100111222     c                   end
007200111222     C*
007300111221     c                   end
007400111221     C*
007500141022     c                   if        D0LNA >0
007600141022     C*
007700141022     C* Controlla che sia una LINEA di PARTNER EEX x Export Manifest
007800141022      * LINEA EEX
007900141022     C                   Z-ADD     1             XX                3 0
008000141022     c                   setoff                                       9956
008100141022     C     d0LNA         LOOKUP    LNA_PT(XX)                             99
008200141022     c                   if        not *in99
008300141022     c                   seton                                        56
008400141022     c                   goto      video
008500141022     c                   end
008600141022     C*
008700111221     C                   RESET                   tabds
008800141021     c                   move      *zero         xtaopz
008900111221     C                   MOVE      *ZERO         xtaret
009000111221     C                   MOVE      *ZERO         xtaopr
009100141022     C                   MOVEl     D0LNA         XTAKLNA
009200141017     C                   MOVEl     *blank        XTAKLUO
009300141017     C                   MOVEl     *zeros        XTAKDAT
009400141017     C                   MOVEl     *zeros        XTANREC
009500141021      *
009600141021      *  ultimi 2 parametri x il test
009700141021     c                   write     z0
009800141021     c     video1        tag
009900141021     c                   exfmt     d1
010000141021     c                   if        *INKC
010100141021     c                   goto      fine
010200141021     c                   end
010300141021     c                   if        d1opz <> *blank
010400141021     C                   MOVE      d1opz         xtaopz
010500141021     c                   end
010600141021      *
010700141021      *  controllo della data
010800141021     c                   if        d1data > 0
010900141021     C                   MOVE      d1data        G02DAT
011000141021     C                   MOVEL     *BLANK        G02ERR
011100141021     C                   CALL      'XSRDA8'
011200141021     C                   PARM                    WLBDA8
011300141021     C     G02ERR        IFEQ      '1'
011400141021     C                   SETON                                          21
011500141021     C                   goto      video1
011600141021     C                   END
011700141021     c                   move      G02INV        XTAKDAT
011800141021     c                   end
011900141021      *
012000111221     C                   MOVEL(p)  tabds         KPJBU
012100141017$004 C                   CALL      'TRTC9PLR1'
012200111221     C                   PARM                    KPJBA
012300141017     C                   MOVEL     KPJBU         tabds
012400141017     c                   else
012500141017     c                   goto      video
012600141017     c                   end
012700111221      *
012800111221     c                   end
012900940325     C* fine programma
013000141021     c     fine          tag
013100940325     C                   SETON                                            LR
013200141022     C************************************************************
013300141022     C* OPERAZIONI INIZIALI
013400141022     C************************************************************
013500141022     C     *INZSR        BEGSR
013600141022     C*
013700141022      *------------
013800141022     C* Caricamento Tabella Partner esteri
013900141022     C                   Z-ADD     0             X                 3 0
014000141022     C                   MOVEL     'PT'          TABCOD
014100141022     C     TABCOD        CHAIN     EDTAB01L                           30
014200141022     C     *IN30         DOWEQ     '0'
014300141022      *
014400141022     C     TABFLG        IFEQ      *BLANKS
014500141022     c                   movel     tabUNI        edidsPT
014600141022      *
014700141022     c     §ptLNP        chain     azorg01l
014800141022     c                   if        %Found(azorg01l)
014900141022     c                   movel     orgde3        og143
015000141022      *  Solo x PARTNER EEX
015100141022     c                   if        §OGNTW = 'EEX'
015200141022     C                   ADD       1             X
015300141022     C                   MOVEL     TABKEY        COD_PT(X)
015400141022     C                   MOVEL     TABUNI        UNI_pt(X)
015500141022     C                   MOVEL     TABUNI        edidsPT
015600141022     C                   MOVEL     §ptLNP        LNA_pt(X)
015700141022     c                   end
015800141022     c                   end
015900141022      *
016000141022     C                   END
016100141022     C     TABCOD        READE     EDTAB01L                               30
016200141022     C                   END
016300141022      *------------
016400141022     C                   ENDSR
016500030113     C************************************************************
