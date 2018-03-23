000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600050829-->  FTISE85D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200050829-->  D DsPassaggio   e DS                  EXTNAME(TISE85ds)
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900030205      *
002000050510     D*-------------
002100050510     D* Reperimento nome PGM
002200050510     D STATUS         SDS           333
002300050510     D  DSPGM            *PROC
002400050510     D*-------------
002500050928-->  D CLLE_Called     C                   CONST('TISE86C')
002600050829-->  D Pgm_Batch       C                   CONST('TISE86R')
002700050829-->  D WRK_File        C                   CONST('WFFMA00F')
002800050829-->  D PRT_File        C                   CONST('TISE85P')
002900050510     D*-------------
003000050510     D digits          C                   CONST('0123456789')
003100030116      *****************************************************************
003200000000     C     *ENTRY        PLIST
003300000000     C                   PARM                    KPJBA
003400030120      *
003500030825     C* Imposta la UDATE  da TIME
003600030825     C                   TIME                    W0140            14 0
003700030825     C                   MOVE      W0140         UDATE8            8 0
003800030825     C*
003900030120      *  In base all'utente prende il P.O.
004000030120     C                   CLEAR                   Tibs36Ds
004100030120     c                   EVAL      I36ute = Knmus
004200030120     c                   EVAL      I36Tla = 'L'
004300030120     C                   CALL      'TIBS36R'
004400030120     C                   PARM                    Tibs36Ds
004500030827     c                   movel     KNMUS         ALFA3             3
004600030829     c                   setoff                                       0102
004700030829      * solo per EDP....:
004800030829     c                   if        ALFA3 ='EDP'
004900030829     c                   seton                                        01
005000030829     c                   end
005100030829      * per utenti di filiale:
005200030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005300030829     c                   seton                                        02
005400030829     c                   end
005500000000      *--------------------------------------------------------------*
005600000000      *             M A I N      L I N E
005700000000      *--------------------------------------------------------------*
005800000000     C                   Z-ADD     1             CODUT
005900000000     C                   CALL      'X§PARUT'
006000000000     C                   PARM                    UT§DSE
006100000000     C                   MOVEL     RAGUT         RSUT             20
006200000000     C                   MOVEL     RAGUT         RSUT2            20
006300000000     C                   MOVEL     REC80         CNCR80
006400050510     C                   MOVEL     DSPGM         NOMPGM
006500040819      *
006600040819     C     INIZIO        tag
006700030825      *
006800030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006900030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
007000050511     c                   movel     'N'           D1LIST                         si vuole stampa?
007100030915      *
007200000000      * Emetto il formato video
007300030116     C     Video1        TAG
007400020308     C                   SETOFF                                       90
007500050510     C                   EXFMT     d01
007600000000      *
007700000000     C   KC              GOTO      FINE                                         F3=USCITA
007800000000      *
007900000000      * Eseguo i  controlli sui campi immessi a video
008000000000     C                   EXSR      CONTR
008100040830      *
008200040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008300040830     C   90
008400040830     CorNkf              GOTO      Video1
008500030905      *
008600050510     c                   clear                   DsPassaggio
008700050510      * parametri
008800050510     c                   movel     Pgm_Batch     paspgm
008900050510     c                   movel     WRK_File      pasfil
009000050511     c                   movel     PRT_File      pasptf
009100050511     c                   movel     D1list        paslst
009200050510     c                   z-add     DalAmg        pasdti
009300050510     c                   z-add     AlAmg         pasdtf
009400040819      *
009500050510      *  richiama il pgm che scrive il workFile
009600050510     c                   eval      kpjbu = DsPassaggio
009700050829     c                   movel     'SE86'        kcoaz
009800040826     c     kcoaz         chain     kfazn11l
009900040826     c                   if        not %Found(kfazn11l)
010000050510     c                   call      CLLE_Called
010100040826     c                   parm                    kpjba
010200040826     c                   else
010300041103     c                   eval      KJOST ='H'
010400041103     c                   if        ALFA3 ='EDP'
010500040819     c                   call      'BCH09'
010600040819     c                   parm                    kpjba
010700041103     c                   end
010800040819     c                   call      'BCH10'
010900040819     c                   parm                    kpjba
011000040826     c                   end
011100000000      *
011200030116     C                   GOTO      INIZIO
011300030116      *
011400000000     C     FINE          TAG
011500000000     C                   SETON                                        LR
011600000000      *--------------------------------------------------------------*
011700000000      * Routine per i controlli sui campi del formato video          *
011800000000      *--------------------------------------------------------------*
011900000000     C     CONTR         BEGSR
012000000000      *
012100030116     c                   movea     '0000000000'  *in(41)
012200040819     c                   movea     '0000000000'  *in(51)
012300030116      * --------------
012400020624      * controllo immissione delle date
012500030116     C                   Z-ADD     d1DDAL        G02DAT
012600000000     C                   MOVEL     *BLANK        G02ERR
012700940926     C                   CALL      'XSRDA8'
012800000000     C                   PARM                    WLBDAT
012900000000     C     G02ERR        IFEQ      '1'
013000020308     C                   SETON                                        44  90
013100030116     C                   GOTO      endct1
013200000000     C                   ENDIF
013300030116     C                   Z-ADD     G02INV        DalAmg            8 0
013400030116     C                   Z-ADD     G02dat        D1ddal
013500030825      *
013600020624      * Controllo data finale
013700030116     C     d1Dal         ifeq      0
013800030127     C                   Z-ADD     d1DDal        d1Dal
013900030127     C                   End
014000030825      *
014100030116     C                   Z-ADD     d1Dal         G02DAT
014200020624     C                   MOVEL     *BLANK        G02ERR
014300020624     C                   CALL      'XSRDA8'
014400020624     C                   PARM                    WLBDAT
014500020624     C     G02ERR        IFEQ      '1'
014600020624     C                   SETON                                        42  90
014700030116     C                   GOTO      endct1
014800020624     C                   ENDIF
014900030825     c*
015000030116     C                   Z-ADD     G02INV        AlAmG             8 0
015100030116     C                   Z-ADD     G02dat        d1dal
015200030127     c*
015300030116     C     DalAmg        ifgt      AlAmg
015400020624     C                   SETON                                        4590
015500030116     C                   GOTO      endct1
015600020624     C                   END
015700030129      *
015800040819      *
015900040819      * --------------
016000040819      *
016100030116     C     endct1        ENDSR
016200030116      *--------------------------------------------------------------*
