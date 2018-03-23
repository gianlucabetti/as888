000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300140916      *   prompt di lancio Statistica Export EEX in Porto franco
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600140916-->  FTISE8BD   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100110511     D CNCR42        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300140916-->  D DsPassaggio   e DS                  EXTNAME(TISE8BDS)
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
002600140916-->  D CLLE_Called     C                   CONST('TISE8CC')
002700140916-->  D Pgm_Batch       C                   CONST('TISE8CR')
002800140916-->  D PRT_File        C                   CONST('TISE8CP')
002900140916-->  D WRK_File        C                   CONST('WFTISE8CF')
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
006200000000     C                   MOVEL     REC80         CNCR80
006300110511     C                   MOVEL     REC42         CNCR42
006400110511     C                   MOVEL     simRGS        RSUT             20
006500110511     C                   MOVEL     simRGS        RSUT2            20
006600050510     C                   MOVEL     DSPGM         NOMPGM
006700040819      *
006800040819     C     INIZIO        tag
006900030825      *
007000030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
007100030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
007200050511     c                   movel     'N'           D1LIST                         si vuole stampa?
007300030915      *
007400000000      * Emetto il formato video
007500030116     C     Video1        TAG
007600020308     C                   SETOFF                                       90
007700050510     C                   EXFMT     d01
007800000000      *
007900000000     C   KC              GOTO      FINE                                         F3=USCITA
008000000000      *
008100000000      * Eseguo i  controlli sui campi immessi a video
008200000000     C                   EXSR      CONTR
008300040830      *
008400040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008500040830     C   90
008600040830     CorNkf              GOTO      Video1
008700030905      *
008800050510     c                   clear                   DsPassaggio
008900050510      * parametri
009000050510     c                   movel     Pgm_Batch     paspgm
009100050510     c                   movel     WRK_File      pasfil
009200050511     c                   movel     PRT_File      pasptf
009300050511     c                   movel     D1list        paslst
009400050510     c                   z-add     DalAmg        pasdti
009500050510     c                   z-add     AlAmg         pasdtf
009600040819      *
009700050510      *  richiama il pgm che scrive il workFile
009800050510     c                   eval      kpjbu = DsPassaggio
009900140916     c                   movel     'SE8C'        kcoaz
010000040826     c     kcoaz         chain     kfazn11l
010100040826     c                   if        not %Found(kfazn11l)
010200050510     c                   call      CLLE_Called
010300040826     c                   parm                    kpjba
010400040826     c                   else
010500041103     c                   eval      KJOST ='H'
010600041103     c                   if        ALFA3 ='EDP'
010700040819     c                   call      'BCH09'
010800040819     c                   parm                    kpjba
010900041103     c                   end
011000040819     c                   call      'BCH10'
011100040819     c                   parm                    kpjba
011200040826     c                   end
011300000000      *
011400030116     C                   GOTO      INIZIO
011500030116      *
011600000000     C     FINE          TAG
011700000000     C                   SETON                                        LR
011800000000      *--------------------------------------------------------------*
011900000000      * Routine per i controlli sui campi del formato video          *
012000000000      *--------------------------------------------------------------*
012100000000     C     CONTR         BEGSR
012200000000      *
012300030116     c                   movea     '0000000000'  *in(41)
012400040819     c                   movea     '0000000000'  *in(51)
012500030116      * --------------
012600020624      * controllo immissione delle date
012700030116     C                   Z-ADD     d1DDAL        G02DAT
012800000000     C                   MOVEL     *BLANK        G02ERR
012900940926     C                   CALL      'XSRDA8'
013000000000     C                   PARM                    WLBDAT
013100000000     C     G02ERR        IFEQ      '1'
013200020308     C                   SETON                                        44  90
013300030116     C                   GOTO      endct1
013400000000     C                   ENDIF
013500030116     C                   Z-ADD     G02INV        DalAmg            8 0
013600030116     C                   Z-ADD     G02dat        D1ddal
013700030825      *
013800020624      * Controllo data finale
013900030116     C     d1Dal         ifeq      0
014000030127     C                   Z-ADD     d1DDal        d1Dal
014100030127     C                   End
014200030825      *
014300030116     C                   Z-ADD     d1Dal         G02DAT
014400020624     C                   MOVEL     *BLANK        G02ERR
014500020624     C                   CALL      'XSRDA8'
014600020624     C                   PARM                    WLBDAT
014700020624     C     G02ERR        IFEQ      '1'
014800020624     C                   SETON                                        42  90
014900030116     C                   GOTO      endct1
015000020624     C                   ENDIF
015100030825     c*
015200030116     C                   Z-ADD     G02INV        AlAmG             8 0
015300030116     C                   Z-ADD     G02dat        d1dal
015400030127     c*
015500030116     C     DalAmg        ifgt      AlAmg
015600020624     C                   SETON                                        4590
015700030116     C                   GOTO      endct1
015800020624     C                   END
015900030129      *
016000040819      * --------------
016100040819      *
016200030116     C     endct1        ENDSR
016300030116      *--------------------------------------------------------------*
