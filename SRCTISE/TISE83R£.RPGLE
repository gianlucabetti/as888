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
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300050704     D DsPassaggio   e DS                  EXTNAME(TISE83ds)
001400940926     D*
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000050704     D*-------------------
002100050704     D* DS DATI VARI STATISTICHE DI GRUPPO
002200050704     D*-------------------
002300050704     D DS5S          E DS
002400050704     D*------------------
002500050704     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002600050704     D*------------------
002700050704     D                 DS
002800050704     D  AGGAA                  1      4  0
002900050704     D  AGGMM                  5      6  0
003000050704     D  AGGAM                  1      6  0
003100050704     D  AGGGG                  7      8  0
003200050704     D  WDAC                   1      8  0
003300030205      *
003400050510     D*-------------
003500050510     D* Reperimento nome PGM
003600050510     D STATUS         SDS           333
003700050510     D  DSPGM            *PROC
003800050510     D*-------------
003900050704     D CLLE_Called     C                   CONST('TISE84C')
004000050704     D Pgm_Batch       C                   CONST('TISE84R')
004100050712     D PRT_File        C                   CONST('       ')
004200050712     D WRK_File        C                   CONST('WFCEX00F')
004300050712     D   A_File        C                   CONST('        ')
004400050510     D*-------------
004500050510     D digits          C                   CONST('0123456789')
004600030116      *****************************************************************
004700000000     C     *ENTRY        PLIST
004800000000     C                   PARM                    KPJBA
004900030120      *
005000030825     C* Imposta la UDATE  da TIME
005100030825     C                   TIME                    W0140            14 0
005200030825     C                   MOVE      W0140         UDATE8            8 0
005300030825     C*
005400030120      *  In base all'utente prende il P.O.
005500030120     C                   CLEAR                   Tibs36Ds
005600030120     c                   EVAL      I36ute = Knmus
005700030120     c                   EVAL      I36Tla = 'L'
005800030120     C                   CALL      'TIBS36R'
005900030120     C                   PARM                    Tibs36Ds
006000030827     c                   movel     KNMUS         ALFA3             3
006100030829     c                   setoff                                       0102
006200030829      * solo per EDP....:
006300030829     c                   if        ALFA3 ='EDP'
006400030829     c                   seton                                        01
006500030829     c                   end
006600030829      * per utenti di filiale:
006700030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
006800030829     c                   seton                                        02
006900030829     c                   end
007000000000      *--------------------------------------------------------------*
007100000000      *             M A I N      L I N E
007200000000      *--------------------------------------------------------------*
007300000000     C                   Z-ADD     1             CODUT
007400000000     C                   CALL      'X§PARUT'
007500000000     C                   PARM                    UT§DSE
007600000000     C                   MOVEL     RAGUT         RSUT             20
007700000000     C                   MOVEL     RAGUT         RSUT2            20
007800000000     C                   MOVEL     REC80         CNCR80
007900050510     C                   MOVEL     DSPGM         NOMPGM
008000040819      *
008100040819     C     INIZIO        tag
008200030825      *
008300091125     C                   Z-ADD     *year         d1AANN                         Fino all'anno
008400091125     C                   Z-ADD     *month        d1AMES                         Fino al mese
008500091125     C                   Z-ADD     0             d1AANN                         Fino all'anno
008600091125     C                   Z-ADD     0             d1AMES                         Fino al mese
008700050714     C                   Z-ADD     *year         D1anno                         Fino all'anno
008800050714     C                   Z-ADD     *month        D1mese                         Fino al mese
008900050511     c                   movel     'N'           D1LIST                         si vuole stampa?
009000050704     c                   movel     'S'           D1DPD
009100050704     c                   movel     'S'           D1EEX
009200050704     c                   movel     'S'           D1FED
009300030915      *
009400000000      * Emetto il formato video
009500030116     C     Video1        TAG
009600020308     C                   SETOFF                                       90
009700050510     C                   EXFMT     d01
009800000000      *
009900000000     C   KC              GOTO      FINE                                         F3=USCITA
010000000000      *
010100000000      * Eseguo i  controlli sui campi immessi a video
010200000000     C                   EXSR      CONTR
010300040830      *
010400040830      * Se non vi sono errori ma non e' premuto F6 emetto video
010500040830     C   90
010600040830     CorNkf              GOTO      Video1
010700030905      *
010800050510     c                   clear                   DsPassaggio
010900050510      * parametri
011000050704     c                   movel     Pgm_Batch     se83pgm
011100050704     c                   movel     WRK_File      se83fil
011200050704     c                   movel     PRT_File      se83ptf
011300050704     c                   movel     A_File        se83awf
011400050704     c                   movel     D1list        se83lst
011500091125     c                   z-add     D1Anno        se83dan
011600091125     c                   z-add     D1Mese        se83dme
011700110107     c                   z-add     d1Anno        se83ann
011800110107     c                   z-add     d1Mese        se83Mes
011900091125     c                   if        d1Aann = 0
012000050706     c     D1Anno        sub       1             se83Dan
012100050706     c                   if        d1mese < 12
012200050706     c     D1Mese        add       1             se83DMe
012300050706     c                   else
012400050706     c                   z-add     D1Anno        se83Dan
012500050706     c                   z-add     1             se83DMe
012600050706     c                   end
012700050707     c                   end
012800050704     c                   eval      %subst(se83ntw:1:1)= D1DPD
012900050704     c                   eval      %subst(se83ntw:2:1)= D1EEX
013000050704     c                   eval      %subst(se83ntw:3:1)= D1FED
013100040819      *
013200050510      *  richiama il pgm che scrive il workFile
013300060111      *
013400060111     c                   exfmt     w01
013500060111     C   KC              GOTO      FINE                                         F3=USCITA
013600060111      *
013700050510     c                   eval      kpjbu = DsPassaggio
013800050704     c                   movel     'SE84'        kcoaz
013900040826     c     kcoaz         chain     kfazn11l
014000040826     c                   if        not %Found(kfazn11l)
014100050510     c                   call      CLLE_Called
014200040826     c                   parm                    kpjba
014300040826     c                   else
014400041103     c                   eval      KJOST ='H'
014500041103     c                   if        ALFA3 ='EDP'
014600040819     c                   call      'BCH09'
014700040819     c                   parm                    kpjba
014800041103     c                   end
014900040819     c                   call      'BCH10'
015000040819     c                   parm                    kpjba
015100040826     c                   end
015200000000      *
015300050517     C********           GOTO      INIZIO
015400030116      *
015500000000     C     FINE          TAG
015600000000     C                   SETON                                        LR
015700000000      *--------------------------------------------------------------*
015800000000      * Routine per i controlli sui campi del formato video          *
015900000000      *--------------------------------------------------------------*
016000000000     C     CONTR         BEGSR
016100000000      *
016200030116     c                   movea     '0000000000'  *in(41)
016300040819     c                   movea     '0000000000'  *in(51)
016400030116      * --------------
016500091125     C     d1Aann        ifgt      d1anno
016600091125     C     d1Aann        orEQ      d1anno
016700091125     C     d1Ames        andgt     d1mese
016800050707     C                   SETON                                        42  90
016900050707     C                   GOTO      endct1
017000050707     C                   ENDIF
017100030825      *
017200091125     C     d1Aann        IFEQ      *zero
017300091125     C     d1Ames        andgt     *zero
017400091125     C     d1Ames        oreq      *zero
017500091125     C     d1Aann        andgt     *zero
017600050708     C                   SETON                                        44  90
017700050708     C                   GOTO      endct1
017800050708     C                   ENDIF
017900050708      *
018000050704      * controllo immissione Mese
018100050704     C     d1mese        IFge      AGGMM
018200050711     C     d1anno        andeq     AGGAA
018300050706     C     d1anno        orgt      AGGAA
018400050704     C                   SETON                                        43  90
018500050704     C                   GOTO      endct1
018600050704     C                   ENDIF
018700040819      * --------------
018800040819      *
018900030116     C     endct1        ENDSR
019000030116      *--------------------------------------------------------------*
019100050704      *   x decodifiche iniziali
019200050704      *--------------------------------------------------------------*
019300050704     C     *InzSR        begSR
019400050704     C*
019500050704     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
019600050704     C     *LIKE         DEFINE    TBLCOD        KTBCOD
019700050704     C     *LIKE         DEFINE    TBLKEY        KTBKEY
019800050704     C*
019900050704     C* CHIAVE LETTURA TABEL00F - PARZIALE
020000050704     C     KEYTA2        KLIST
020100050704     C                   KFLD                    KTBKUT
020200050704     C                   KFLD                    KTBCOD
020300050704     C*
020400050704     C                   Z-ADD     1             KTBKUT
020500050704     C                   MOVEL     '5S'          KTBCOD
020600050704     C     KEYTA2        CHAIN     TABEL00F                           99
020700050704     C                   clear                   WDAC                           *ULTIMO AGGIORNAMENT
020800050704IF  1C     *IN99         Ifeq      *Off                                         *TABELLA NON TROVATA
020900050704     C                   MOVEL     TBLUNI        DS5S
021000050704     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
021100050704E   1C                   ENDIF
021200050704     C*---
021300050704     C                   ENDSR
021400050704      *--------------------------------------------------------------*
