000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500050704     FTABEL00F  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700050704     Ftise83d   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200110511     D CNCR42        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400050704     D DsPassaggio   e DS                  EXTNAME(TISE83ds)
001500940926     D*
001600000000     D WLBDAT          DS
001700940926     D  G02DAT                 1      8  0
001800940926     D  G02INV                 9     16  0
001900940926     D  G02ERR                17     17
002000940926     D  G02TGI                18     22  0
002100050704     D*-------------------
002200050704     D* DS DATI VARI STATISTICHE DI GRUPPO
002300050704     D*-------------------
002400050704     D DS5S          E DS
002500050704     D*------------------
002600050704     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002700050704     D*------------------
002800050704     D                 DS
002900050704     D  AGGAA                  1      4  0
003000050704     D  AGGMM                  5      6  0
003100050704     D  AGGAM                  1      6  0
003200050704     D  AGGGG                  7      8  0
003300050704     D  WDAC                   1      8  0
003400030205      *
003500050510     D*-------------
003600050510     D* Reperimento nome PGM
003700050510     D STATUS         SDS           333
003800050510     D  DSPGM            *PROC
003900050510     D*-------------
004000050704     D CLLE_Called     C                   CONST('TISE84C')
004100050704     D Pgm_Batch       C                   CONST('TISE84R')
004200050712     D PRT_File        C                   CONST('       ')
004300050712     D WRK_File        C                   CONST('WFCEX00F')
004400050712     D   A_File        C                   CONST('        ')
004500050510     D*-------------
004600050510     D digits          C                   CONST('0123456789')
004700030116      *****************************************************************
004800000000     C     *ENTRY        PLIST
004900000000     C                   PARM                    KPJBA
005000030120      *
005100030825     C* Imposta la UDATE  da TIME
005200030825     C                   TIME                    W0140            14 0
005300030825     C                   MOVE      W0140         UDATE8            8 0
005400030825     C*
005500030120      *  In base all'utente prende il P.O.
005600030120     C                   CLEAR                   Tibs36Ds
005700030120     c                   EVAL      I36ute = Knmus
005800030120     c                   EVAL      I36Tla = 'L'
005900030120     C                   CALL      'TIBS36R'
006000030120     C                   PARM                    Tibs36Ds
006100030827     c                   movel     KNMUS         ALFA3             3
006200030829     c                   setoff                                       0102
006300030829      * solo per EDP....:
006400030829     c                   if        ALFA3 ='EDP'
006500030829     c                   seton                                        01
006600030829     c                   end
006700030829      * per utenti di filiale:
006800030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
006900030829     c                   seton                                        02
007000030829     c                   end
007100000000      *--------------------------------------------------------------*
007200000000      *             M A I N      L I N E
007300000000      *--------------------------------------------------------------*
007400000000     C                   Z-ADD     1             CODUT
007500000000     C                   CALL      'X§PARUT'
007600000000     C                   PARM                    UT§DSE
007700110511     C********           MOVEL     RAGUT         RSUT             20
007800110511     C********           MOVEL     RAGUT         RSUT2            20
007900000000     C                   MOVEL     REC80         CNCR80
008000110511     C                   MOVEL     REC42         CNCR42
008100110511     C                   MOVEL     simRGS        RSUT             20
008200110511     C                   MOVEL     simRGS        RSUT2            20
008300050510     C                   MOVEL     DSPGM         NOMPGM
008400040819      *
008500040819     C     INIZIO        tag
008600030825      *
008700091125     C                   Z-ADD     *year         d1AANN                         Fino all'anno
008800091125     C                   Z-ADD     *month        d1AMES                         Fino al mese
008900091125     C                   Z-ADD     0             d1AANN                         Fino all'anno
009000091125     C                   Z-ADD     0             d1AMES                         Fino al mese
009100050714     C                   Z-ADD     *year         D1anno                         Fino all'anno
009200050714     C                   Z-ADD     *month        D1mese                         Fino al mese
009300050511     c                   movel     'N'           D1LIST                         si vuole stampa?
009400050704     c                   movel     'S'           D1DPD
009500050704     c                   movel     'S'           D1EEX
009600050704     c                   movel     'S'           D1FED
009700030915      *
009800000000      * Emetto il formato video
009900030116     C     Video1        TAG
010000020308     C                   SETOFF                                       90
010100050510     C                   EXFMT     d01
010200000000      *
010300000000     C   KC              GOTO      FINE                                         F3=USCITA
010400000000      *
010500000000      * Eseguo i  controlli sui campi immessi a video
010600000000     C                   EXSR      CONTR
010700040830      *
010800040830      * Se non vi sono errori ma non e' premuto F6 emetto video
010900040830     C   90
011000040830     CorNkf              GOTO      Video1
011100030905      *
011200050510     c                   clear                   DsPassaggio
011300050510      * parametri
011400050704     c                   movel     Pgm_Batch     se83pgm
011500050704     c                   movel     WRK_File      se83fil
011600050704     c                   movel     PRT_File      se83ptf
011700050704     c                   movel     A_File        se83awf
011800050704     c                   movel     D1list        se83lst
011900091125     c                   z-add     D1Anno        se83dan
012000091125     c                   z-add     D1Mese        se83dme
012100110107     c                   z-add     d1Anno        se83ann
012200110107     c                   z-add     d1Mese        se83Mes
012300091125     c                   if        d1Aann = 0
012400050706     c     D1Anno        sub       1             se83Dan
012500050706     c                   if        d1mese < 12
012600050706     c     D1Mese        add       1             se83DMe
012700050706     c                   else
012800050706     c                   z-add     D1Anno        se83Dan
012900050706     c                   z-add     1             se83DMe
013000050706     c                   end
013100050707     c                   end
013200050704     c                   eval      %subst(se83ntw:1:1)= D1DPD
013300050704     c                   eval      %subst(se83ntw:2:1)= D1EEX
013400050704     c                   eval      %subst(se83ntw:3:1)= D1FED
013500040819      *
013600050510      *  richiama il pgm che scrive il workFile
013700060111      *
013800060111     c                   exfmt     w01
013900060111     C   KC              GOTO      FINE                                         F3=USCITA
014000060111      *
014100050510     c                   eval      kpjbu = DsPassaggio
014200050704     c                   movel     'SE84'        kcoaz
014300040826     c     kcoaz         chain     kfazn11l
014400040826     c                   if        not %Found(kfazn11l)
014500050510     c                   call      CLLE_Called
014600040826     c                   parm                    kpjba
014700040826     c                   else
014800041103     c                   eval      KJOST ='H'
014900041103     c                   if        ALFA3 ='EDP'
015000040819     c                   call      'BCH09'
015100040819     c                   parm                    kpjba
015200041103     c                   end
015300040819     c                   call      'BCH10'
015400040819     c                   parm                    kpjba
015500040826     c                   end
015600000000      *
015700050517     C********           GOTO      INIZIO
015800030116      *
015900000000     C     FINE          TAG
016000000000     C                   SETON                                        LR
016100000000      *--------------------------------------------------------------*
016200000000      * Routine per i controlli sui campi del formato video          *
016300000000      *--------------------------------------------------------------*
016400000000     C     CONTR         BEGSR
016500000000      *
016600030116     c                   movea     '0000000000'  *in(41)
016700040819     c                   movea     '0000000000'  *in(51)
016800030116      * --------------
016900091125     C     d1Aann        ifgt      d1anno
017000091125     C     d1Aann        orEQ      d1anno
017100091125     C     d1Ames        andgt     d1mese
017200050707     C                   SETON                                        42  90
017300050707     C                   GOTO      endct1
017400050707     C                   ENDIF
017500030825      *
017600091125     C     d1Aann        IFEQ      *zero
017700091125     C     d1Ames        andgt     *zero
017800091125     C     d1Ames        oreq      *zero
017900091125     C     d1Aann        andgt     *zero
018000050708     C                   SETON                                        44  90
018100050708     C                   GOTO      endct1
018200050708     C                   ENDIF
018300050708      *
018400050704      * controllo immissione Mese
018500050704     C     d1mese        IFge      AGGMM
018600050711     C     d1anno        andeq     AGGAA
018700050706     C     d1anno        orgt      AGGAA
018800050704     C                   SETON                                        43  90
018900050704     C                   GOTO      endct1
019000050704     C                   ENDIF
019100040819      * --------------
019200040819      *
019300030116     C     endct1        ENDSR
019400030116      *--------------------------------------------------------------*
019500050704      *   x decodifiche iniziali
019600050704      *--------------------------------------------------------------*
019700050704     C     *InzSR        begSR
019800050704     C*
019900050704     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
020000050704     C     *LIKE         DEFINE    TBLCOD        KTBCOD
020100050704     C     *LIKE         DEFINE    TBLKEY        KTBKEY
020200050704     C*
020300050704     C* CHIAVE LETTURA TABEL00F - PARZIALE
020400050704     C     KEYTA2        KLIST
020500050704     C                   KFLD                    KTBKUT
020600050704     C                   KFLD                    KTBCOD
020700050704     C*
020800050704     C                   Z-ADD     1             KTBKUT
020900050704     C                   MOVEL     '5S'          KTBCOD
021000050704     C     KEYTA2        CHAIN     TABEL00F                           99
021100050704     C                   clear                   WDAC                           *ULTIMO AGGIORNAMENT
021200050704IF  1C     *IN99         Ifeq      *Off                                         *TABELLA NON TROVATA
021300050704     C                   MOVEL     TBLUNI        DS5S
021400050704     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
021500050704E   1C                   ENDIF
021600050704     C*---
021700050704     C                   ENDSR
021800050704      *--------------------------------------------------------------*
