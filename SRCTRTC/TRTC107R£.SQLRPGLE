000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300050726      *     Visualizza QRY per tipo msg EDI
000400000000      *****************************************************************
000500050726     Fsndifcsum1iF   E           k disk    usropn rename(SNDDATI:SUMDATI)
000600050726     Fsndiftsta1iF   E           k disk    usropn rename(SNDDATI:STADATI)
000700050726     Ftrtc107d  CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900050704     D*------------------
002000050704     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002100050704     D*------------------
002200050704     D                 DS
002300050704     D  AGGAA                  1      4  0
002400050704     D  AGGMM                  5      6  0
002500050704     D  AGGAM                  1      6  0
002600050704     D  AGGGG                  7      8  0
002700050704     D  WDAC                   1      8  0
002800030205      *
002900050510     D*-------------
003000050510     D* Reperimento nome PGM
003100050510     D STATUS         SDS           333
003200050510     D  DSPGM            *PROC
003300050510     D*-------------
003400050510     D digits          C                   CONST('0123456789')
003500030116      *****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800030120      *
003900030825     C* Imposta la UDATE  da TIME
004000030825     C                   TIME                    W0140            14 0
004100030825     C                   MOVE      W0140         UDATE8            8 0
004200030825     C*
004300030120      *  In base all'utente prende il P.O.
004400030120     C                   CLEAR                   Tibs36Ds
004500030120     c                   EVAL      I36ute = Knmus
004600030120     c                   EVAL      I36Tla = 'L'
004700030120     C                   CALL      'TIBS36R'
004800030120     C                   PARM                    Tibs36Ds
004900030827     c                   movel     KNMUS         ALFA3             3
005000030829     c                   setoff                                       0102
005100030829      * solo per EDP....:
005200030829     c                   if        ALFA3 ='EDP'
005300030829     c                   seton                                        01
005400030829     c                   end
005500030829      * per utenti di filiale:
005600030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005700030829     c                   seton                                        02
005800030829     c                   end
005900000000      *--------------------------------------------------------------*
006000000000      *             M A I N      L I N E
006100000000      *--------------------------------------------------------------*
006200000000     C                   Z-ADD     1             CODUT
006300000000     C                   CALL      'X§PARUT'
006400000000     C                   PARM                    UT§DSE
006500000000     C                   MOVEL     RAGUT         RSUT             20
006600000000     C                   MOVEL     RAGUT         RSUT2            20
006700000000     C                   MOVEL     REC80         CNCR80
006800050510     C                   MOVEL     DSPGM         NOMPGM
006900040819      *
007000040819     C     INIZIO        tag
007100050726     C                   move      *blank        D1Tip
007200050726     C                   Z-ADD     0             D1Num
007300050726     C                   Z-ADD     0             D1Lnp
007400030825      *
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
008600050726     C   99
008700040830     CorNkf              GOTO      Video1
008800030905      *
008900050726      *  Sflagga i records da re-inviare
009000050726     C                   EXSR      SFLAG
009100040819      *
009200050726      *  richiama il pgm per sottomettere l'invio
009300050726     c                   clear                   kpjbu
009400050726     c                   call      'TRTC104R'
009500040826     c                   parm                    kpjba
009600000000      *
009700050726     C                   GOTO      INIZIO
009800030116      *
009900000000     C     FINE          TAG
010000000000     C                   SETON                                        LR
010100000000      *--------------------------------------------------------------*
010200000000      * Routine per i controlli sui campi del formato video          *
010300000000      *--------------------------------------------------------------*
010400000000     C     CONTR         BEGSR
010500000000      *
010600030116     c                   movea     '0000000000'  *in(41)
010700040819     c                   movea     '0000000000'  *in(51)
010800030116      * --------------
010900050726     c                   if        d1tip ='STA'
011000050726     c                   open      sndiftsta1
011100050726     c                   move      'RX'          sselabora
011200050726     c                   z-add     d1lnp         sslinea
011300050726     c                   z-add     d1num         ssnumero
011400050726     c     key_chk       chain     sndiftsta1
011500050726     c                   if        not %Found(sndiftsta1)
011600050726     c                   seton                                        4199
011700050726     c                   end
011800050726     c                   close     sndiftsta1
011900050726     c                   end
012000050726      * --------------
012100050726     c                   if        d1tip ='SUM'
012200050726     c                   open      sndifcsum1
012300050726     c                   move      'RX'          sselabora
012400050726     c                   z-add     d1lnp         sslinea
012500050726     c                   z-add     d1num         ssnumero
012600050726     c     key_chk       chain     sndifcsum1
012700050726     c                   if        not %Found(sndifcsum1)
012800050726     c                   seton                                        4199
012900050726     c                   end
013000050726     c                   close     sndifcsum1
013100050726     c                   end
013200040819      * --------------
013300040819      *
013400030116     C     endct1        ENDSR
013500050726      *--------------------------------------------------------------*
013600050726      * Esegue l'operazione di sflag per re-inviare i records        *
013700050726      *--------------------------------------------------------------*
013800050726     C     SFLAG         BEGSR
013900050726      *
014000050726      * --------------
014100050726     c                   if        d1tip ='STA'
014200050726     C/EXEC SQL
014300050726     C+ UPDATE SNDIFTSTA SET SSELABORA = 'OK' WHERE SSNUMERO = :d1num AND
014400050726     C+ SSLINEA = :d1lnp AND SSELABORA ='RX'
014500050726     C/END-EXEC
014600050726     c                   end
014700050726      * --------------
014800050726     c                   if        d1tip ='SUM'
014900050726     C/EXEC SQL
015000050726     C+ UPDATE SNDIFCSUM SET SSELABORA = 'OK' WHERE SSNUMERO = :d1num AND
015100050726     C+ SSLINEA = :d1lnp AND SSELABORA ='RX'
015200050726     C/END-EXEC
015300050726     c                   end
015400050726      * --------------
015500050726      *
015600050726     C                   ENDSR
015700030116      *--------------------------------------------------------------*
015800050704      *   x decodifiche iniziali
015900050704      *--------------------------------------------------------------*
016000050704     C     *InzSR        begSR
016100050704     C*
016200050726     c     key_Chk       klist
016300050726     c                   kfld                    sselabora
016400050726     c                   kfld                    sslinea
016500050726     c                   kfld                    ssnumero
016600050704     C*---
016700050704     C                   ENDSR
016800050704      *--------------------------------------------------------------*
