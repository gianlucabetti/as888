000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600061013-->  FTISE88D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200061013-->  D DsPassaggio   e DS                  EXTNAME(TISE88ds)
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
002500061016-->  D CLLE_Called     C                   CONST('TISE87C')
002600061013-->  D Pgm_Batch       C                   CONST('TISE87R')
002700061013-->  D WRK_File        C                   CONST('WFFM200F')
002800061013-->  D PRT_File        C                   CONST('       ')
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
007000030915      *
007100000000      * Emetto il formato video
007200030116     C     Video1        TAG
007300020308     C                   SETOFF                                       90
007400050510     C                   EXFMT     d01
007500000000      *
007600000000     C   KC              GOTO      FINE                                         F3=USCITA
007700000000      *
007800000000      * Eseguo i  controlli sui campi immessi a video
007900000000     C                   EXSR      CONTR
008000040830      *
008100040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008200040830     C   90
008300040830     CorNkf              GOTO      Video1
008400030905      *
008500050510     c                   clear                   DsPassaggio
008600050510      * parametri
008700050510     c                   movel     Pgm_Batch     paspgm
008800050510     c                   movel     WRK_File      pasfil
008900050511     c                   movel     PRT_File      pasptf
009000050510     c                   z-add     DalAmg        pasdti
009100050510     c                   z-add     AlAmg         pasdtf
009200061127     c                   move      autdop        pasdop
009300040819      *
009400050510      *  richiama il pgm che scrive il workFile
009500050510     c                   eval      kpjbu = DsPassaggio
009600061016     c                   movel     'SE87'        kcoaz
009700040826     c     kcoaz         chain     kfazn11l
009800040826     c                   if        not %Found(kfazn11l)
009900050510     c                   call      CLLE_Called
010000040826     c                   parm                    kpjba
010100040826     c                   else
010200041103     c                   eval      KJOST ='H'
010300041103     c                   if        ALFA3 ='EDP'
010400040819     c                   call      'BCH09'
010500040819     c                   parm                    kpjba
010600041103     c                   end
010700040819     c                   call      'BCH10'
010800040819     c                   parm                    kpjba
010900040826     c                   end
011000000000      *
011100030116     C                   GOTO      INIZIO
011200030116      *
011300000000     C     FINE          TAG
011400000000     C                   SETON                                        LR
011500000000      *--------------------------------------------------------------*
011600000000      * Routine per i controlli sui campi del formato video          *
011700000000      *--------------------------------------------------------------*
011800000000     C     CONTR         BEGSR
011900000000      *
012000030116     c                   movea     '0000000000'  *in(41)
012100040819     c                   movea     '0000000000'  *in(51)
012200030116      * --------------
012300020624      * controllo immissione delle date
012400030116     C                   Z-ADD     d1DDAL        G02DAT
012500000000     C                   MOVEL     *BLANK        G02ERR
012600940926     C                   CALL      'XSRDA8'
012700000000     C                   PARM                    WLBDAT
012800000000     C     G02ERR        IFEQ      '1'
012900020308     C                   SETON                                        44  90
013000030116     C                   GOTO      endct1
013100000000     C                   ENDIF
013200030116     C                   Z-ADD     G02INV        DalAmg            8 0
013300030116     C                   Z-ADD     G02dat        D1ddal
013400030825      *
013500020624      * Controllo data finale
013600030116     C     d1Dal         ifeq      0
013700030127     C                   Z-ADD     d1DDal        d1Dal
013800030127     C                   End
013900030825      *
014000030116     C                   Z-ADD     d1Dal         G02DAT
014100020624     C                   MOVEL     *BLANK        G02ERR
014200020624     C                   CALL      'XSRDA8'
014300020624     C                   PARM                    WLBDAT
014400020624     C     G02ERR        IFEQ      '1'
014500020624     C                   SETON                                        42  90
014600030116     C                   GOTO      endct1
014700020624     C                   ENDIF
014800030825     c*
014900030116     C                   Z-ADD     G02INV        AlAmG             8 0
015000030116     C                   Z-ADD     G02dat        d1dal
015100030127     c*
015200030116     C     DalAmg        ifgt      AlAmg
015300020624     C                   SETON                                        4590
015400030116     C                   GOTO      endct1
015500020624     C                   END
015600030129      *
015700040819      *
015800040819      * --------------
015900040819      *
016000030116     C     endct1        ENDSR
016100030116      *--------------------------------------------------------------*
