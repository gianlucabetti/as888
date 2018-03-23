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
001100110511     D CNCR42        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300050513     D DsPassaggio   e DS                  EXTNAME(TISE81ds)
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
002600050513     D CLLE_Called     C                   CONST('TISE81C')
002700050517     D Pgm_Batch       C                   CONST('TISE82R')
002800050517     D WRK_File        C                   CONST('WFTISE82F')
002900050513     D PRT_File        C                   CONST('TISE81P')
003000050517     D   A_File        C                   CONST('WFDCM00F')
003100050510     D*-------------
003200050510     D digits          C                   CONST('0123456789')
003300030116      *****************************************************************
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600030120      *
003700030825     C* Imposta la UDATE  da TIME
003800030825     C                   TIME                    W0140            14 0
003900030825     C                   MOVE      W0140         UDATE8            8 0
004000030825     C*
004100030120      *  In base all'utente prende il P.O.
004200030120     C                   CLEAR                   Tibs36Ds
004300030120     c                   EVAL      I36ute = Knmus
004400030120     c                   EVAL      I36Tla = 'L'
004500030120     C                   CALL      'TIBS36R'
004600030120     C                   PARM                    Tibs36Ds
004700030827     c                   movel     KNMUS         ALFA3             3
004800030829     c                   setoff                                       0102
004900030829      * solo per EDP....:
005000030829     c                   if        ALFA3 ='EDP'
005100030829     c                   seton                                        01
005200030829     c                   end
005300030829      * per utenti di filiale:
005400030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005500030829     c                   seton                                        02
005600030829     c                   end
005700000000      *--------------------------------------------------------------*
005800000000      *             M A I N      L I N E
005900000000      *--------------------------------------------------------------*
006000000000     C                   Z-ADD     1             CODUT
006100000000     C                   CALL      'X§PARUT'
006200000000     C                   PARM                    UT§DSE
006300110511     C*****              MOVEL     RAGUT         RSUT             20
006400110511     C*****              MOVEL     RAGUT         RSUT2            20
006500000000     C                   MOVEL     REC80         CNCR80
006600110511     C                   MOVEL     REC42         CNCR42
006700110511     C                   MOVEL     simRGS        RSUT             20
006800110511     C                   MOVEL     simRGS        RSUT2            20
006900050510     C                   MOVEL     DSPGM         NOMPGM
007000040819      *
007100040819     C     INIZIO        tag
007200030825      *
007300050513     C                   Z-ADD     *year         D1anno                         Fino all'anno
007400050513     C                   Z-ADD     *month        D1mese                         Fino al mese
007500050511     c                   movel     'N'           D1LIST                         si vuole stampa?
007600030915      *
007700000000      * Emetto il formato video
007800030116     C     Video1        TAG
007900020308     C                   SETOFF                                       90
008000050510     C                   EXFMT     d01
008100000000      *
008200000000     C   KC              GOTO      FINE                                         F3=USCITA
008300000000      *
008400000000      * Eseguo i  controlli sui campi immessi a video
008500000000     C                   EXSR      CONTR
008600040830      *
008700040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008800040830     C   90
008900040830     CorNkf              GOTO      Video1
009000030905      *
009100050510     c                   clear                   DsPassaggio
009200050510      * parametri
009300050513     c                   movel     Pgm_Batch     se81pgm
009400050513     c                   movel     WRK_File      se81fil
009500050513     c                   movel     PRT_File      se81ptf
009600050517     c                   movel     A_File        se81awf
009700050513     c                   movel     D1list        se81lst
009800050513     c                   z-add     D1Anno        se81ann
009900050513     c                   z-add     D1Mese        se81mes
010000040819      *
010100050510      *  richiama il pgm che scrive il workFile
010200050510     c                   eval      kpjbu = DsPassaggio
010300050513     c                   movel     'SE82'        kcoaz
010400040826     c     kcoaz         chain     kfazn11l
010500040826     c                   if        not %Found(kfazn11l)
010600050510     c                   call      CLLE_Called
010700040826     c                   parm                    kpjba
010800040826     c                   else
010900041103     c                   eval      KJOST ='H'
011000041103     c                   if        ALFA3 ='EDP'
011100040819     c                   call      'BCH09'
011200040819     c                   parm                    kpjba
011300041103     c                   end
011400040819     c                   call      'BCH10'
011500040819     c                   parm                    kpjba
011600040826     c                   end
011700000000      *
011800050517     C********           GOTO      INIZIO
011900030116      *
012000000000     C     FINE          TAG
012100000000     C                   SETON                                        LR
012200000000      *--------------------------------------------------------------*
012300000000      * Routine per i controlli sui campi del formato video          *
012400000000      *--------------------------------------------------------------*
012500000000     C     CONTR         BEGSR
012600000000      *
012700030116     c                   movea     '0000000000'  *in(41)
012800040819     c                   movea     '0000000000'  *in(51)
012900030116      * --------------
013000050513      * controllo immissione Anno
013100050513     C     d1anno        IFEQ      *zero
013200050513     C                   SETON                                        44  90
013300050513     C                   GOTO      endct1
013400050513     C                   ENDIF
013500030825      *
013600050513      * controllo immissione Anno
013700050513     C     d1mese        IFEQ      *zero
013800050513     C                   SETON                                        42  90
013900050513     C                   GOTO      endct1
014000050513     C                   ENDIF
014100040819      *
014200040819      * --------------
014300040819      *
014400030116     C     endct1        ENDSR
014500030116      *--------------------------------------------------------------*
