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
001100110511     D CNCR42        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300061013-->  D DsPassaggio   e DS                  EXTNAME(TISE88ds)
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
002600061016-->  D CLLE_Called     C                   CONST('TISE87C')
002700061013-->  D Pgm_Batch       C                   CONST('TISE87R')
002800061013-->  D WRK_File        C                   CONST('WFFM200F')
002900061013-->  D PRT_File        C                   CONST('       ')
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
007400030915      *
007500000000      * Emetto il formato video
007600030116     C     Video1        TAG
007700020308     C                   SETOFF                                       90
007800050510     C                   EXFMT     d01
007900000000      *
008000000000     C   KC              GOTO      FINE                                         F3=USCITA
008100000000      *
008200000000      * Eseguo i  controlli sui campi immessi a video
008300000000     C                   EXSR      CONTR
008400040830      *
008500040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008600040830     C   90
008700040830     CorNkf              GOTO      Video1
008800030905      *
008900050510     c                   clear                   DsPassaggio
009000050510      * parametri
009100050510     c                   movel     Pgm_Batch     paspgm
009200050510     c                   movel     WRK_File      pasfil
009300050511     c                   movel     PRT_File      pasptf
009400050510     c                   z-add     DalAmg        pasdti
009500050510     c                   z-add     AlAmg         pasdtf
009600061127     c                   move      autdop        pasdop
009700040819      *
009800050510      *  richiama il pgm che scrive il workFile
009900050510     c                   eval      kpjbu = DsPassaggio
010000061016     c                   movel     'SE87'        kcoaz
010100040826     c     kcoaz         chain     kfazn11l
010200040826     c                   if        not %Found(kfazn11l)
010300050510     c                   call      CLLE_Called
010400040826     c                   parm                    kpjba
010500040826     c                   else
010600041103     c                   eval      KJOST ='H'
010700041103     c                   if        ALFA3 ='EDP'
010800040819     c                   call      'BCH09'
010900040819     c                   parm                    kpjba
011000041103     c                   end
011100040819     c                   call      'BCH10'
011200040819     c                   parm                    kpjba
011300040826     c                   end
011400000000      *
011500030116     C                   GOTO      INIZIO
011600030116      *
011700000000     C     FINE          TAG
011800000000     C                   SETON                                        LR
011900000000      *--------------------------------------------------------------*
012000000000      * Routine per i controlli sui campi del formato video          *
012100000000      *--------------------------------------------------------------*
012200000000     C     CONTR         BEGSR
012300000000      *
012400030116     c                   movea     '0000000000'  *in(41)
012500040819     c                   movea     '0000000000'  *in(51)
012600030116      * --------------
012700020624      * controllo immissione delle date
012800030116     C                   Z-ADD     d1DDAL        G02DAT
012900000000     C                   MOVEL     *BLANK        G02ERR
013000940926     C                   CALL      'XSRDA8'
013100000000     C                   PARM                    WLBDAT
013200000000     C     G02ERR        IFEQ      '1'
013300020308     C                   SETON                                        44  90
013400030116     C                   GOTO      endct1
013500000000     C                   ENDIF
013600030116     C                   Z-ADD     G02INV        DalAmg            8 0
013700030116     C                   Z-ADD     G02dat        D1ddal
013800030825      *
013900020624      * Controllo data finale
014000030116     C     d1Dal         ifeq      0
014100030127     C                   Z-ADD     d1DDal        d1Dal
014200030127     C                   End
014300030825      *
014400030116     C                   Z-ADD     d1Dal         G02DAT
014500020624     C                   MOVEL     *BLANK        G02ERR
014600020624     C                   CALL      'XSRDA8'
014700020624     C                   PARM                    WLBDAT
014800020624     C     G02ERR        IFEQ      '1'
014900020624     C                   SETON                                        42  90
015000030116     C                   GOTO      endct1
015100020624     C                   ENDIF
015200030825     c*
015300030116     C                   Z-ADD     G02INV        AlAmG             8 0
015400030116     C                   Z-ADD     G02dat        d1dal
015500030127     c*
015600030116     C     DalAmg        ifgt      AlAmg
015700020624     C                   SETON                                        4590
015800030116     C                   GOTO      endct1
015900020624     C                   END
016000030129      *
016100040819      *
016200040819      * --------------
016300040819      *
016400030116     C     endct1        ENDSR
016500030116      *--------------------------------------------------------------*
