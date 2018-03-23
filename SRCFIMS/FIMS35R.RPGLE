000100000920     H DECEDIT('0,') DATEDIT(*DMY.)
000200040512      *---------------------------------------------------------------*
000300040512      * CANCELLAZIONE BOLLA TEST X PICKING + SPUNTE
000400040512      *---------------------------------------------------------------*
000500040512     fFIMS35D   cf   e             workstn
000600080908     FTNBLA00F  O  A E           K DISK    USROPN extfile(WTNBLA)
000700040512     FFNBLP01L  IF   E           K DISK
000800040519     FFNARB01L  IF   E           K DISK
000900040512     FFNBLT01L  IF   E           K DISK
001000070123     FFNBRV09L  UF   E           K DISK
001100080908     Ftabel00f  UF   E           K DISK
001200070123     FFNBRVE2L  UF   E           K DISK    RENAME(FNBRV000:FNBRVE)
001300040512
001400900511     D PARAM           DS
001500941221     D  PARAAS               108    111  0
001600941221     D  PARLNP               112    114  0
001700941221     D  PARNRS               115    116  0
001800941221     D  PARNSP               117    123  0
001900941221     D  PARLNA               124    126  0
002000040512
002100940819     D WLBDAT          DS                  INZ
002200940819     D  G02DAT                 1      8  0
002300940819     D  G02INV                 9     16  0
002400940819     D  G02ERR                17     17
002500940819     D  G02TGI                18     22  0
002600040512
002700040512     D FNLS63DS      E DS
002800040512     D KPJBA         E DS
002900080908     D ds3a          E DS
003000080908     d
003100080908     d WTNBLA          s             21    inz('GAITRAGRU /TNBLA00F')
003200040512
003300040512      *------------------------------------------------
003400040512     c     VIDEO         tag
003500040512      *
003600040512     c                   write     MS35T01
003700040512     c                   write     MS35Z01
003800040512     c                   exfmt     MS35D01
003900040512     c                   eval      *in28 = *off
004000040512     c                   clear                   V1Dmsg
004100040512      *
004200040512    1c                   select
004300040512      * F3=Fine
004400040512    1c                   when      *inKC = *on
004500040512     c                   goto      FINE
004600040512      * F6=Elaboro
004700040512    1c                   when      *inKF = *on
004800040512     C                   z-add     v1caas        BLPAAS
004900040512     C                   z-add     v1clnp        BLPlnp
005000040512     C                   z-add     v1cnrs        BLPnrs
005100040512     C                   z-add     v1cnsp        BLPnsp
005200040512     C     KBLP          CHAIN     FNBLP000
005300040519     C     KBLP          SETLL     FNARB000                               99
005400040512      *
005500040512    2C                   SELECT
005600040512    2C                   WHEN      NOT %found(fnblp01l)
005700040512     c                   eval      *in28 = *on
005800040512     c                   eval      V1Dmsg = 'Spedizione non trovata'
005900040519    2C                   WHEN      *IN99 = *ON
006000040519     c                   eval      *in28 = *on
006100040519     c                   eval      V1Dmsg = 'Spedizione non valida'
006200040512    2C                   WHEN      %subst(BLPRSM:1:5) <> 'PROVA' or
006300040512     C                             %subst(BLPRSD:1:5) <> 'PROVA'
006400040512     c                   eval      *in28 = *on
006500040512     c                   eval      V1Dmsg = 'Spedizione non valida, verificare'
006600040512    2c                   OTHER
006700040512     c                   exsr      DELETE
006800040512     c                   eval      *in28 = *on
006900040512     c                   eval      V1Dmsg = 'Spedizione cancellata'
007000040512    2c                   ENDSL
007100040512     c                   goto      VIDEO
007200040512
007300040512    1c                   other
007400040512     c                   goto      VIDEO
007500040512    1C                   endsl
007600040512      *
007700040512     c     FINE          tag
007800040512      *
007900040512     c                   eval      *INLR = *ON
008000040512      *------------------------------------------------
008100040512     C     *INZSR        BEGSR
008200040512      *
008300040512     C     *ENTRY        PLIST
008400040512     C                   PARM                    kpjba
008500040512     C*
008600040512     C     KBLP          KLIST
008700040512     C                   KFLD                    BLPAAS
008800040512     C                   KFLD                    BLPLNP
008900040512     C                   KFLD                    BLPNRS
009000040512     C                   KFLD                    BLPNSP
009100040512      *
009200040512     C     KBRV          KLIST
009300040512     C                   KFLD                    KNPG              1 0
009400040512     C                   KFLD                    BLTFLS
009500040512     C                   KFLD                    BLTLNA
009600040512     C                   KFLD                    BLTNRS
009700040512     C                   KFLD                    BLTNSC
009800080908     C* ACCESSO   TABEL
009900080908     C     KTAB          KLIST
010000080908     C                   KFLD                    CODUT             1 0
010100080908     C                   KFLD                    COD               2
010200080908     C                   KFLD                    KEY               8
010300080908     c
010400080908     c                   z-add     1             codut
010500040512      *
010600040512      * UDATE IN AAAAMMGG
010700040512     C                   TIME                    W0140            14 0
010800040512     C                   MOVE      W0140         G02DAT
010900040512     C                   MOVEL     *BLANK        G02ERR
011000040512     C                   CALL      'XSRDA8'
011100040512     C                   PARM                    WLBDAT
011200040512     C                   MOVEL     G02INV        DATEU             8 0
011300080908     c
011400080908     C                   OPEN(e)   TNBLA00F
011500080908     c
011600080908     c                   if        not %open(tnbla00f)
011700080908     c                   eval      %subst(WTNBLA:7:4)='GRPS'
011800080908     C                   OPEN      TNBLA00F
011900080908     c                   endif
012000040512      *
012100040512     C                   ENDSR
012200040512      *--- CANCELLAZIONE FISICA BOLLA IN TUTTI I SUOI FILES ----------*
012300040512     C     DELETE        BEGSR
012400040512      *
012500040512      * SCRIVO RECORD IN FLBLA
012600080908     C                   MOVEL     blpfle        BLAfle
012700080908     C                   MOVEL     BLPLNP        BLALNP
012800900511     C                   MOVEL     BLPAAS        BLAAAS
012900900511     C                   MOVEL     BLPMGS        BLAMGS
013000900511     C                   MOVEL     BLPNRS        BLANRS
013100900511     C                   MOVEL     BLPNSP        BLANSP
013200900511     C                   MOVEL     BLPLNA        BLALNA
013300080908     C                   MOVEL     '3A'          COD
013400080908     c                   movel(P)  blpcbo        key
013500080908     c     ktab          chain     tabel
013600080908     c                   if        %found(tabel00f)
013700080908     c                   movel     tbluni        ds3a
013800080908     c                   movel     §3atb1        blatbl
013900080908     c                   else
014000080908     c                   clear                   blatbl
014100080908     c                   endif
014200900511     C                   MOVEL     DATEU         BLADAN
014201110412     C                   MOVEL     blpcbo        BLAcbo
014202110412     c                   time                    blaoan
014203110412     c                   movel     knmus         blapru
014204110412     c                   movel     knmtd         blamtd
014205110412     c                   if        blpccm>0
014206110412     c                   z-add     blpccm        blaccm
014207110412     c                   else
014208110412     c                   if        %subst(blatbl:1:1)='F'
014209110412     c                   z-add     blpksc        blaccm
014210110412     c                   endif
014211110412     c                   endif
014212110412     c                   z-add     blprmn        blarmn
014213110412     c                   movel     blprma        blarma
014214110412     c                   movel     blprsm        blarsm
014215110412     c                   movel     blpinm        blainm
014216110412     c                   movel     blpcam        blacam
014217110412     c                   movel     blplom        blalom
014218110412     c                   movel     blpprm        blaprm
014219110412     c                   movel     blpnzm        blanzm
014220110412     c                   movel     blprsd        blarsd
014221110412     c                   movel     blpcad        blacad
014222110412     c                   movel     blplod        blalod
014223110412     c                   movel     blpprd        blaprd
014224110412     c                   movel     blpnzd        blanzd
014225110412     c                   movel     blpctm        blactm
014226110412     c                   z-add     blpncl        blancl
014227110412     c                   z-add     blppkb        blapkb
014228110412     c                   z-add     blpfls        blafls
014229110412     c                   z-add     blpncd        blancd
014230110412     c                   z-add     blpnca        blanca
014300080908     C                   WRITE     TNBLA000
014400040512      *
014500040512      * SPUNTE
014600040512     C     KBLP          SETLL     FNBLT000
014700040512     C     KBLP          READE     FNBLT000
014800040512    1C                   DOW       NOT %EOF(FNBLT01L)
014900040512      *
015000040512      * ciclo x tutti i tipi foglio per cancellare spunte e spunte doppie
015100040512    2C                   DO        9             WNPG              2 0
015200040512     c                   z-add     Wnpg          KNPG
015300040512      *
015400070123     C     KBRV          CHAIN     FNBRV000                           31
015500040512    3C     *IN31         DOWEQ     *OFF
015600070123     C                   DELETE    FNBRV000
015700070123     C     KBRV          READE     FNBRV000                               31
015800040512    3C                   ENDDO
015900040512      *
016000070123     C     KBRV          CHAIN     FNBRVE                             31
016100040512    3C     *IN31         DOWEQ     *OFF
016200070123     C                   DELETE    FNBRVE
016300070123     C     KBRV          READE     FNBRVE                                 31
016400040512    3C                   ENDDO
016500040512      *
016600040512    2C                   ENDDO
016700040512      *
016800040512     C     KBLP          READE     FNBLT000
016900040512    1C                   ENDDO
017000040512      *
017100040512      * RICHIAMO PROGRAMMA DI CANCELLAZIONE BOLLE
017200001130      *
017300040512     C                   CLEAR                   FNLS63DS
017400001130     C                   MOVE      BLPAAS        I63AAS
017500001130     C                   MOVE      BLPLNP        I63LNP
017600001130     C                   MOVE      BLPNRS        I63NRS
017700001130     C                   MOVE      BLPNSP        I63NSP
017800040512     C                   MOVEL     FNLS63DS      KPJBU
017900001130     C                   CALL      'FNLS63R'
018000001130     C                   PARM                    KPJBA
018100910807     C*
018200900511     C                   ENDSR
