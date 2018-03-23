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
001100110511     D CNCR42        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300050829-->  D DsPassaggio   e DS                  EXTNAME(TISE85ds)
001400940926     D*
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000030205      *
002100050510     D*-------------
002200050510     D* Reperimento nome PGM
002300050510     D STATUS         SDS           333
002400050510     D  DSPGM            *PROC
002500050510     D*-------------
002600050928-->  D CLLE_Called     C                   CONST('TISE86C')
002700050829-->  D Pgm_Batch       C                   CONST('TISE86R')
002800050829-->  D WRK_File        C                   CONST('WFFMA00F')
002900050829-->  D PRT_File        C                   CONST('TISE85P')
003000050510     D*-------------
003100050510     D digits          C                   CONST('0123456789')
003200030116      *****************************************************************
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500030120      *
003600030825     C* Imposta la UDATE  da TIME
003700030825     C                   TIME                    W0140            14 0
003800030825     C                   MOVE      W0140         UDATE8            8 0
003900030825     C*
004000030120      *  In base all'utente prende il P.O.
004100030120     C                   CLEAR                   Tibs36Ds
004200030120     c                   EVAL      I36ute = Knmus
004300030120     c                   EVAL      I36Tla = 'L'
004400030120     C                   CALL      'TIBS36R'
004500030120     C                   PARM                    Tibs36Ds
004600030827     c                   movel     KNMUS         ALFA3             3
004700030829     c                   setoff                                       0102
004800030829      * solo per EDP....:
004900030829     c                   if        ALFA3 ='EDP'
005000030829     c                   seton                                        01
005100030829     c                   end
005200030829      * per utenti di filiale:
005300030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005400030829     c                   seton                                        02
005500030829     c                   end
005600000000      *--------------------------------------------------------------*
005700000000      *             M A I N      L I N E
005800000000      *--------------------------------------------------------------*
005900000000     C                   Z-ADD     1             CODUT
006000000000     C                   CALL      'X§PARUT'
006100000000     C                   PARM                    UT§DSE
006200110511     C********           MOVEL     RAGUT         RSUT             20
006300110511     C********           MOVEL     RAGUT         RSUT2            20
006400000000     C                   MOVEL     REC80         CNCR80
006500110511     C                   MOVEL     REC42         CNCR42
006600110511     C                   MOVEL     simRGS        RSUT             20
006700110511     C                   MOVEL     simRGS        RSUT2            20
006800050510     C                   MOVEL     DSPGM         NOMPGM
006900040819      *
007000040819     C     INIZIO        tag
007100030825      *
007200030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
007300030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
007400050511     c                   movel     'N'           D1LIST                         si vuole stampa?
007500030915      *
007600000000      * Emetto il formato video
007700030116     C     Video1        TAG
007800020308     C                   SETOFF                                       90
007900050510     C                   EXFMT     d01
008000000000      *
008100000000     C   KC              GOTO      FINE                                         F3=USCITA
008200000000      *
008300000000      * Eseguo i  controlli sui campi immessi a video
008400000000     C                   EXSR      CONTR
008500040830      *
008600040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008700040830     C   90
008800040830     CorNkf              GOTO      Video1
008900030905      *
009000050510     c                   clear                   DsPassaggio
009100050510      * parametri
009200050510     c                   movel     Pgm_Batch     paspgm
009300050510     c                   movel     WRK_File      pasfil
009400050511     c                   movel     PRT_File      pasptf
009500050511     c                   movel     D1list        paslst
009600050510     c                   z-add     DalAmg        pasdti
009700050510     c                   z-add     AlAmg         pasdtf
009800040819      *
009900050510      *  richiama il pgm che scrive il workFile
010000050510     c                   eval      kpjbu = DsPassaggio
010100050829     c                   movel     'SE86'        kcoaz
010200040826     c     kcoaz         chain     kfazn11l
010300040826     c                   if        not %Found(kfazn11l)
010400050510     c                   call      CLLE_Called
010500040826     c                   parm                    kpjba
010600040826     c                   else
010700041103     c                   eval      KJOST ='H'
010800041103     c                   if        ALFA3 ='EDP'
010900040819     c                   call      'BCH09'
011000040819     c                   parm                    kpjba
011100041103     c                   end
011200040819     c                   call      'BCH10'
011300040819     c                   parm                    kpjba
011400040826     c                   end
011500000000      *
011600030116     C                   GOTO      INIZIO
011700030116      *
011800000000     C     FINE          TAG
011900000000     C                   SETON                                        LR
012000000000      *--------------------------------------------------------------*
012100000000      * Routine per i controlli sui campi del formato video          *
012200000000      *--------------------------------------------------------------*
012300000000     C     CONTR         BEGSR
012400000000      *
012500030116     c                   movea     '0000000000'  *in(41)
012600040819     c                   movea     '0000000000'  *in(51)
012700030116      * --------------
012800020624      * controllo immissione delle date
012900030116     C                   Z-ADD     d1DDAL        G02DAT
013000000000     C                   MOVEL     *BLANK        G02ERR
013100940926     C                   CALL      'XSRDA8'
013200000000     C                   PARM                    WLBDAT
013300000000     C     G02ERR        IFEQ      '1'
013400020308     C                   SETON                                        44  90
013500030116     C                   GOTO      endct1
013600000000     C                   ENDIF
013700030116     C                   Z-ADD     G02INV        DalAmg            8 0
013800030116     C                   Z-ADD     G02dat        D1ddal
013900030825      *
014000020624      * Controllo data finale
014100030116     C     d1Dal         ifeq      0
014200030127     C                   Z-ADD     d1DDal        d1Dal
014300030127     C                   End
014400030825      *
014500030116     C                   Z-ADD     d1Dal         G02DAT
014600020624     C                   MOVEL     *BLANK        G02ERR
014700020624     C                   CALL      'XSRDA8'
014800020624     C                   PARM                    WLBDAT
014900020624     C     G02ERR        IFEQ      '1'
015000020624     C                   SETON                                        42  90
015100030116     C                   GOTO      endct1
015200020624     C                   ENDIF
015300030825     c*
015400030116     C                   Z-ADD     G02INV        AlAmG             8 0
015500030116     C                   Z-ADD     G02dat        d1dal
015600030127     c*
015700030116     C     DalAmg        ifgt      AlAmg
015800020624     C                   SETON                                        4590
015900030116     C                   GOTO      endct1
016000020624     C                   END
016100030129      *
016200040819      *
016300040819      * --------------
016400040819      *
016500030116     C     endct1        ENDSR
016600030116      *--------------------------------------------------------------*
