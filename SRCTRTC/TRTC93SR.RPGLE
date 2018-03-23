000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200111222     FEDTAB01L  IF   E           K DISK
000300940224      *
000400111221     FTRTC93SD  CF   E             WORKSTN
000500940128     F*----------------------------------------------------*
000600940211     D* Passaggio Parametri
000700940211     D KPJBA         E DS
000800111220     D UTEDSE0F      E DS
000900111222     D EDIDSPV       E DS
001000030113      *-------------
001100940325     D* Parametri in ricezione
001200030113     D  TABDS          DS
001300030113     D  XTAOPZ                 1      2
001400030113     D  XTARET                 3      3
001500030113     D  XTAOPR                 4      4
001600030113     D  XTAERR                 5      5
001700111220     D  XTAKEY1                6      8
001800111220     D  XTAKEY2                9     11
001900111220     D  XTACOD1               12     14
002000111220     D  XTACOD2               15     17
002100111220     D  XTAUNBcli             18     52
002200111221     D  XTARAGs               53     84
002300131011     D  XTANREC               85     93  0
002400131011     C*----------------------------------------------------*
002500111221     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002600940127     C*----------------------------------------------------*
002700940127     C*                MAIN LINE PROGRAM
002800940127     C*----------------------------------------------------*
002900111222      * Definizione chiavi
003000111222     C     KTAB          KLIST
003100111222     C                   KFLD                    KCOD
003200111222     C                   KFLD                    KKEY
003300111222      *
003400111222     C     *LIKE         DEFINE    TABCOD        KCOD
003500111222     C     *LIKE         DEFINE    TABKEY        KKEY
003600111222      *
003700111221     C* Reperimento parametri
003800111221     C     *ENTRY        PLIST
003900111221     C                   PARM                    KPJBA
004000111221     C*
004100111221     c                   write     z0
004200111221     c                   exfmt     d0
004300111221     c                   if        not *INKC
004400111221     C*
004500111221     c                   if        d0cod = *blank
004600111221     C                   CLEAR                   TRTC91
004700111221     C                   MOVEL     'PT'          D91COD
004800111221     C                   MOVEL     TRTC91        KPJBU
004900111221     C                   CALL      'TRTC9PTR'
005000111221     C                   PARM                    KPJBA
005100111221     C                   MOVEL     KPJBU         TRTC91
005200111222     C*  decodifica quello che ha il cliente configurato sulla sua tabella PARTNER
005300111222      *
005400111222     C*  Aggancio TABELLA PV
005500111222     C                   MOVEL     'PV'          KCOD
005600111222     C                   MOVEL     D91KEY        KKEY
005700111222     C     KTAB          CHAIN     EDTAB01L
005800111222     c                   if        %Found(EDTAB01L)
005900111222     C                   MOVEL     TABUNI        EDIDSPV
006000111222     C                   if        §PVIDENT <> *blank
006100111222     C                   eval      D91KEY =  §PVIDENT
006200111222     c                   end
006300111222     c                   end
006400111222     C*
006500111221     c                   else
006600111221     C*
006700111221     C                   eval      D91KEY = d0cod
006800111221     c                   end
006900111221     C*
007000111221     C                   RESET                   tabds
007100111221     C                   MOVEL     '01'          xtaopz
007200111221     C                   MOVE      *ZERO         xtaret
007300111221     C                   MOVE      *ZERO         xtaopr
007400111221     C                   MOVEl     *blank        xtakey1
007500111221     C                   MOVEl     *blank        xtakey2
007600111221     C                   MOVEl     *blank        xtacod1
007700111221     C                   MOVEl     *blank        xtacod2
007800111221     C                   MOVEl     D91KEY        xtaUNBcli
007900111221     C                   eval       xtaRAGs = %subst(D91UNI:22:35)
008000111221     C                   MOVEL(p)  tabds         KPJBU
008100111221$004 C                   CALL      'TRTC93SR1'
008200111221     C                   PARM                    KPJBA
008300111221      *
008400111221     C                   MOVEL     KPJBU         tabds
008500111221      *
008600111221     c                   end
008700111221     C*
008800940325     C* fine programma
008900940325     C                   SETON                                            LR
009000030113     C************************************************************
