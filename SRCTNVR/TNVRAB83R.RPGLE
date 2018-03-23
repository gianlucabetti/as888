000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600051027     Ftnvrab83d CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200051027     D DsPassaggio   e DS                  EXTNAME(TNVRAB83ds)
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900050704     D*-------------------
002000050704     D* DS DATI VARI STATISTICHE DI GRUPPO
002100050704     D*-------------------
002200050704     D DS5S          E DS
002300050704     D*------------------
002400050704     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002500050704     D*------------------
002600050704     D                 DS
002700050704     D  AGGAA                  1      4  0
002800050704     D  AGGMM                  5      6  0
002900050704     D  AGGAM                  1      6  0
003000050704     D  AGGGG                  7      8  0
003100050704     D  WDAC                   1      8  0
003200030205      *
003300050510     D*-------------
003400050510     D* Reperimento nome PGM
003500050510     D STATUS         SDS           333
003600050510     D  DSPGM            *PROC
003700050510     D*-------------
003800051027     D CLLE_Called     C                   CONST('TNVRAB84C')
003900051027     D Pgm_Batch       C                   CONST('TNVRAB84R')
004000050712     D PRT_File        C                   CONST('       ')
004100051027     D WRK_File        C                   CONST('        ')
004200050712     D   A_File        C                   CONST('        ')
004300050510     D*-------------
004400050510     D digits          C                   CONST('0123456789')
004500030116      *****************************************************************
004600000000     C     *ENTRY        PLIST
004700000000     C                   PARM                    KPJBA
004800030120      *
004900030825     C* Imposta la UDATE  da TIME
005000030825     C                   TIME                    W0140            14 0
005100030825     C                   MOVE      W0140         UDATE8            8 0
005200030825     C*
005300030120      *  In base all'utente prende il P.O.
005400030120     C                   CLEAR                   Tibs36Ds
005500030120     c                   EVAL      I36ute = Knmus
005600030120     c                   EVAL      I36Tla = 'L'
005700030120     C                   CALL      'TIBS36R'
005800030120     C                   PARM                    Tibs36Ds
005900030827     c                   movel     KNMUS         ALFA3             3
006000030829     c                   setoff                                       0102
006100030829      * solo per EDP....:
006200030829     c                   if        ALFA3 ='EDP'
006300030829     c                   seton                                        01
006400030829     c                   end
006500030829      * per utenti di filiale:
006600030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
006700030829     c                   seton                                        02
006800030829     c                   end
006900000000      *--------------------------------------------------------------*
007000000000      *             M A I N      L I N E
007100000000      *--------------------------------------------------------------*
007200000000     C                   Z-ADD     1             CODUT
007300000000     C                   CALL      'X§PARUT'
007400000000     C                   PARM                    UT§DSE
007500000000     C                   MOVEL     RAGUT         RSUT             20
007600000000     C                   MOVEL     RAGUT         RSUT2            20
007700000000     C                   MOVEL     REC80         CNCR80
007800050510     C                   MOVEL     DSPGM         NOMPGM
007900040819      *
008000040819     C     INIZIO        tag
008100030825      *
008200051027     c                   movel     'S'           D1LIST                         si vuole stampa?
008300030915      *
008400000000      * Emetto il formato video
008500030116     C     Video1        TAG
008600020308     C                   SETOFF                                       90
008700050510     C                   EXFMT     d01
008800000000      *
008900000000     C   KC              GOTO      FINE                                         F3=USCITA
009000000000      *
009100000000      * Eseguo i  controlli sui campi immessi a video
009200000000     C                   EXSR      CONTR
009300040830      *
009400040830      * Se non vi sono errori ma non e' premuto F6 emetto video
009500040830     C   90
009600040830     CorNkf              GOTO      Video1
009700030905      *
009800050510     c                   clear                   DsPassaggio
009900050510      * parametri
010000050704     c                   movel     Pgm_Batch     se83pgm
010100050704     c                   movel     WRK_File      se83fil
010200050704     c                   movel     PRT_File      se83ptf
010300050704     c                   movel     A_File        se83awf
010400050704     c                   movel     D1list        se83lst
010500040819      *
010600050510      *  richiama il pgm che scrive il workFile
010700050510     c                   eval      kpjbu = DsPassaggio
010800051027     c                   movel     '    '        kcoaz
010900040826     c     kcoaz         chain     kfazn11l
011000051027     c******             if        not %Found(kfazn11l)
011100050510     c                   call      CLLE_Called
011200040826     c                   parm                    kpjba
011300051027     c******             else
011400051027     c******             eval      KJOST ='H'
011500051027     c******             if        ALFA3 ='EDP'
011600051027     c******             call      'BCH09'
011700051027     c******             parm                    kpjba
011800051027     c******             end
011900051027     c********           call      'BCH10'
012000051027     c********           parm                    kpjba
012100051027     c********           end
012200051027      ********
012300050517     C********           GOTO      INIZIO
012400030116      *
012500000000     C     FINE          TAG
012600000000     C                   SETON                                        LR
012700000000      *--------------------------------------------------------------*
012800000000      * Routine per i controlli sui campi del formato video          *
012900000000      *--------------------------------------------------------------*
013000000000     C     CONTR         BEGSR
013100000000      *
013200030116     c                   movea     '0000000000'  *in(41)
013300040819     c                   movea     '0000000000'  *in(51)
013400030116      * --------------
013500040819      * --------------
013600040819      *
013700030116     C     endct1        ENDSR
013800030116      *--------------------------------------------------------------*
013900050704      *   x decodifiche iniziali
014000050704      *--------------------------------------------------------------*
014100050704     C     *InzSR        begSR
014200050704     C*
014300050704     C*---
014400050704     C                   ENDSR
014500050704      *--------------------------------------------------------------*
