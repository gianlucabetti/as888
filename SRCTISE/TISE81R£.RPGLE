000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600050513     Ftise81d   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200050513     D DsPassaggio   e DS                  EXTNAME(TISE81ds)
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
002500050513     D CLLE_Called     C                   CONST('TISE81C')
002600050517     D Pgm_Batch       C                   CONST('TISE82R')
002700050517     D WRK_File        C                   CONST('WFTISE82F')
002800050513     D PRT_File        C                   CONST('TISE81P')
002900050517     D   A_File        C                   CONST('WFDCM00F')
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
006200000000     C                   MOVEL     RAGUT         RSUT             20
006300000000     C                   MOVEL     RAGUT         RSUT2            20
006400000000     C                   MOVEL     REC80         CNCR80
006500050510     C                   MOVEL     DSPGM         NOMPGM
006600040819      *
006700040819     C     INIZIO        tag
006800030825      *
006900050513     C                   Z-ADD     *year         D1anno                         Fino all'anno
007000050513     C                   Z-ADD     *month        D1mese                         Fino al mese
007100050511     c                   movel     'N'           D1LIST                         si vuole stampa?
007200030915      *
007300000000      * Emetto il formato video
007400030116     C     Video1        TAG
007500020308     C                   SETOFF                                       90
007600050510     C                   EXFMT     d01
007700000000      *
007800000000     C   KC              GOTO      FINE                                         F3=USCITA
007900000000      *
008000000000      * Eseguo i  controlli sui campi immessi a video
008100000000     C                   EXSR      CONTR
008200040830      *
008300040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008400040830     C   90
008500040830     CorNkf              GOTO      Video1
008600030905      *
008700050510     c                   clear                   DsPassaggio
008800050510      * parametri
008900050513     c                   movel     Pgm_Batch     se81pgm
009000050513     c                   movel     WRK_File      se81fil
009100050513     c                   movel     PRT_File      se81ptf
009200050517     c                   movel     A_File        se81awf
009300050513     c                   movel     D1list        se81lst
009400050513     c                   z-add     D1Anno        se81ann
009500050513     c                   z-add     D1Mese        se81mes
009600040819      *
009700050510      *  richiama il pgm che scrive il workFile
009800050510     c                   eval      kpjbu = DsPassaggio
009900050513     c                   movel     'SE82'        kcoaz
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
011400050517     C********           GOTO      INIZIO
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
012600050513      * controllo immissione Anno
012700050513     C     d1anno        IFEQ      *zero
012800050513     C                   SETON                                        44  90
012900050513     C                   GOTO      endct1
013000050513     C                   ENDIF
013100030825      *
013200050513      * controllo immissione Anno
013300050513     C     d1mese        IFEQ      *zero
013400050513     C                   SETON                                        42  90
013500050513     C                   GOTO      endct1
013600050513     C                   ENDIF
013700040819      *
013800040819      * --------------
013900040819      *
014000030116     C     endct1        ENDSR
014100030116      *--------------------------------------------------------------*
