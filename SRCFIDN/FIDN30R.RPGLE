000100981012      *----------------------------------------------------*
000200990408      *  Visualizza Mandato
000300981012      *----------------------------------------------------*
000400980623
000500980521     H DECEDIT('0,') DATEDIT(*DMY.)
000600980623
000700990408     FFIDN30D   CF   E             WORKSTN
000800990408     FTABEL00F  IF   E           K DISK
000900980623
001000980521      *------------------------------------------------------------------------*
001100990409     D NOMAND          C                   CONST('CLIENTE SENZA MANDATO')
001200990409
001300990408     D Kkut            S                   LIKE(TBLkut)
001400990408     D Kcod            S                   LIKE(TBLcod)
001500990408     D Kkey            S                   LIKE(TBLkey)
001600990408     D W0010           S              1  0
001700980521      *
001800980521      *   D S   I N T E R N E / E S T E R N E
001900980525      *
002000990408     D FIDN30DS      E DS
002100990408     d TIBS69DS      E DS
002200990408     d DS_cnaco      E DS                  extname(CNACO00F)
002300990408     d DS_cnind      E DS                  extname(CNIND00F)
002400990408     d DS_cnclp      E DS                  extname(CNCLP00F)
002500990408     d DS_fncls      E DS                  extname(FNCLS00F)
002600990408     D DSTR          E DS
002700990408     D DS1P          E DS
002800980521      *
002900980521     D KPJBA         E DS
003000980623
003100980623      *------------------------------------------------------------------------*
003200980521      *---------------  C A L C O L O  ----------------------------------------*
003300980521      *------------------------------------------------------------------------*
003400990408
003500990408      * Decodifico campi per video
003600990409
003700990409      *   TARIFFA PARTICOLARE
003800990409     c                   IF        I30ftc = *blanks
003900990409     C                   movel     NOMAND        W1Dftc
004000990409     c                   ELSE
004100990409     C                   movel     '1P'          KCOD
004200990409     C                   movel(p)  I30ftc        KKEY
004300990409     C     KTAB          CHAIN     TABEL                              30
004400990409    1C                   IF        *IN30 = *OFF
004500990409     C                   movel     TBLUNI        DS1P
004600990409     C                   movel     §1PDES        W1Dftc
004700990409    1C                   ENDIF
004800990409    1C                   ENDIF
004900990408
005000990408      *   CODICE CLIENTE
005100990408     c                   z-add     I30ksc        W1Cksc
005200990408     C                   clear                   TIBS69DS
005300990408     c                   z-add     I30ksc        I69kac
005400990408      *
005500990408     C                   CALL      'TIBS69R'
005600990408     C                   PARM                    tibs69DS
005700990408     C                   PARM                    DS_cnaco
005800990408     C                   PARM                    DS_cnind
005900990408     C                   PARM                    DS_cnclp
006000990408     C                   PARM                    DS_fncls
006100990408      *
006200990408     C                   IF        O69ERR <> '1'
006300990408     c                   movel     ACOrag        W1Dksc
006400990408     c                   ENDIF
006500990408
006600990408      *   CODICE TARIFFA
006700990408     c                   z-add     I30ctr        W1Cctr
006800990408     C                   movel     I30ctr        w0010
006900990408     C                   movel     'TR'          KCOD
007000990408     C                   movel(p)  W0010         KKEY
007100990408     C     KTAB          CHAIN     TABEL                              30
007200990408    1C                   IF        *IN30 = *OFF
007300990408     C                   movel     TBLUNI        DSTR
007400990408     C                   movel     §TRDES        W1Dctr
007500990408    1C                   ENDIF
007600990408
007700990408      *   VALORE MERCE
007800990408     c                   z-add     I30vlm        W1Cvlm
007900990408     c                   movel     I30vvm        W1Cvvm
008000990408
008100990408      *   TIPO VALORE
008200990408     C                   movel     'TR'          KCOD
008300990408     C                   movel(p)  I30fvm        KKEY
008400990408     C     KTAB          CHAIN     TABEL                              30
008500990408    1C                   IF        *IN30 = *OFF
008600990408     C                   movel     TBLUNI        DSTR
008700990409     C                   movel     §TRDE3        W1Dfvm
008800990408    1C                   ENDIF
008900990408
009000990408      * Emissione VIDEO
009100990409     C                   write     FI30D01
009200990409     C                   exfmt     FI30W01
009300990408
009400990408     C                   EVAL      I69TLA  = 'C'
009500990408     C                   CALL      'TIBS69R'
009600990408     C                   PARM                    TIBS69DS
009700990408
009800980907     c                   EVAL      *INLR = *ON
009900980910      *****************************************************************
010000980527      * ROUTINE INIZIALE
010100980527      *****************************************************************
010200980527     C     *INZSR        BEGSR
010300980615      *
010400980521     C     *ENTRY        PLIST
010500980521     C                   PARM                    KPJBA
010600990408     C                   PARM                    FIDN30DS
010700990408      *
010800990408     C     KTAB          KLIST
010900990408     C                   KFLD                    KKUT
011000990408     C                   KFLD                    KCOD
011100990408     C                   KFLD                    KKEY
011200990408      *
011300990408     C                   Z-ADD     1             KKUT
011400990408      *
011500990408     c                   ENDSR
