000100120216     ***************************************************************************
000200120216     **
000300120217     ** Tester di TIBSSOFR.
000400120216     **
000500120216     ***************************************************************************
000600120217     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TIBS')
000700120217     H EXTBININT(*YES)
000800120216
000900120216     ***************************************************************************
001000120216     **
001100120216     ** Costanti.
001200120216     **
001300120216     ***************************************************************************
001400120216      /COPY GAITRASRC/SRCCONST,TIBSSOFR
001500120216
001600120216     ***************************************************************************
001700120216     **
001800120216     ** Prototipi.
001900120216     **
002000120216     ***************************************************************************
002100120216      /DEFINE DFTACTGRP_NO
002200120217      /DEFINE DFTACTGRP_YES
002300120216      /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
002400120216      /UNDEFINE DFTACTGRP_NO
002500120217      /UNDEFINE DFTACTGRP_YES
002600120216
002700120216     ***************************************************************************
002800120216     **
002900120216     ** Strutture dati.
003000120216     **
003100120216     ***************************************************************************
003200120217     D azsof00f      E DS                  QUALIFIED TEMPLATE
003300120217     D azorg00f      E DS                  QUALIFIED
003400120217     D tibsSof0i     E DS                  QUALIFIED INZ(*EXTDFT)
003500120217     D tibsSof1i     E DS                  QUALIFIED INZ(*EXTDFT)
003600120217     D tibsSof0o     E DS                  QUALIFIED INZ(*EXTDFT)
003700120217     D tibsSof1o     E DS                  QUALIFIED INZ(*EXTDFT)
003800120217     D  idFilialiAry                       DIM(999) OVERLAY(idFiliali)
003900120217     D                                     LIKE(azsof00f.idFiliale)
004000120217
004100120216     ***************************************************************************
004200120216     **
004300120216     ** Campi.
004400120216     **
004500120216     ***************************************************************************
004600120220     D TibsSof_esito...
004700120217     D                 S             10I 0 IMPORT
004800120217     D idSocieta...
004900120217     D                 S                   LIKE(azsof00f.idSocieta)
005000120217     D idFiliali...
005100120217     D                 S                   DIM(999)
005200120217     D                                     LIKE(azsof00f.idFiliale)
005300120217     D rqsOpCode...
005400120220     D                 S             10I 0
005500120217     D rpyOpCode...
005600120220     D                 S             10I 0
005700120217     D rpyIdMsg...
005800120217     D                 S             10I 0
005900120217     D rqsFormato...
006000120217     D                 S             10A
006100120217     D rqsDataSize...
006200120217     D                 S             10I 0
006300120217     D rpyFormato...
006400120217     D                 S             10A
006500120217     D rpyDataSize...
006600120217     D                 S             10I 0
006700120217
006800120217     ***************************************************************************
006900120217     **
007000120217     ** Main.
007100120217     **
007200120217     ***************************************************************************
007300120216
007400120217      /FREE
007500120217
007600120217       *INLR = *ON;
007700120217
007800120220       TibsSof_Init();
007900120217
008000120220       idSocieta = TibsSof_GetIdSocietaByIdFiliale(1);
008100120217
008200120220       IF TibsSof_esito < TIBSSOF_ESITO_OK;
008300120217         DUMP(A);
008400120217       ENDIF;
008500120217
008600120220       idFiliali = TibsSof_GetIdFilialiByIdSocieta(idSocieta);
008700120217
008800120220       IF TibsSof_esito < TIBSSOF_ESITO_OK;
008900120217         DUMP(A);
009000120217       ENDIF;
009100120217
009200120220       azorg00f = TibsSof_GetOrganigrammaByIdFiliale(1);
009300120217
009400120220       IF TibsSof_esito < TIBSSOF_ESITO_OK;
009500120217         DUMP(A);
009600120217       ENDIF;
009700120217
009800120220       TibsSof_Finalize();
009900120217
010000120217      /END-FREE
010100120217
010200120220     C                   EVAL      rqsOpCode = TIBSSOF_RQSOPCODE_INIT
010300120217     C                   CALL      'TIBSSOFR'
010400120217     C                   PARM                    rqsOpCode
010500120220     C                   PARM      *ZERO         rpyOpCode
010600120217     C                   PARM      *ZERO         rpyIdMsg
010700120217
010800120217     C                   RESET                   tibsSof0i
010900120217     C                   EVAL      tibsSof0i.idFiliale = 1
011000120217     C                   RESET                   tibsSof0o
011100120217     C                   EVAL      rqsOpCode =
011200120220     C                             TIBSSOF_RQSOPCODE_GET_IDSOCIETA_BY_IDFILIALE
011300120217     C                   EVAL      rqsFormato = tibsSof0i.formato
011400120217     C                   EVAL      rqsDataSize = %SIZE(tibsSof0i)
011500120217     C                   EVAL      rpyFormato = tibsSof0o.formato
011600120217     C                   EVAL      rpyDataSize = %SIZE(tibsSof0o)
011700120217     C                   CALL      'TIBSSOFR'
011800120217     C                   PARM                    rqsOpCode
011900120220     C                   PARM      *ZERO         rpyOpCode
012000120217     C                   PARM      *ZERO         rpyIdMsg
012100120217     C                   PARM                    rqsFormato
012200120217     C                   PARM                    tibsSof0i
012300120217     C                   PARM                    rqsDataSize
012400120217     C                   PARM                    rpyFormato
012500120217     C                   PARM                    tibsSof0o
012600120217     C                   PARM                    rpyDataSize
012700120217
012800120217     C                   RESET                   tibsSof1i
012900120217     C                   EVAL      tibsSof1i.idSocieta = tibsSof0o.idSocieta
013000120217     C                   RESET                   tibsSof1o
013100120217     C                   EVAL      rqsOpCode =
013200120220     C                             TIBSSOF_RQSOPCODE_GET_IDFILIALI_BY_IDSOCIETA
013300120217     C                   EVAL      rqsFormato = tibsSof1i.formato
013400120217     C                   EVAL      rqsDataSize = %SIZE(tibsSof1i)
013500120217     C                   EVAL      rpyFormato = tibsSof1o.formato
013600120217     C                   EVAL      rpyDataSize = %SIZE(tibsSof1o)
013700120217     C                   CALL      'TIBSSOFR'
013800120217     C                   PARM                    rqsOpCode
013900120220     C                   PARM      *ZERO         rpyOpCode
014000120217     C                   PARM      *ZERO         rpyIdMsg
014100120217     C                   PARM                    rqsFormato
014200120217     C                   PARM                    tibsSof1i
014300120217     C                   PARM                    rqsDataSize
014400120217     C                   PARM                    rpyFormato
014500120217     C                   PARM                    tibsSof1o
014600120217     C                   PARM                    rpyDataSize
014700120217
014800120220     C                   EVAL      rqsOpCode = TIBSSOF_RQSOPCODE_FINALIZE
014900120217     C                   CALL      'TIBSSOFR'
015000120217     C                   PARM                    rqsOpCode
015100120220     C                   PARM      *ZERO         rpyOpCode
015200120217     C                   PARM      *ZERO         rpyIdMsg
