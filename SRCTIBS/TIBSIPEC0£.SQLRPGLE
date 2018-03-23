000100080930     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080930
000300080930      // ----------------------------------------------------------------------
000400080930      //
000600080930      //
000700080930      // ----------------------------------------------------------------------
000900080930      // ----------------------------------------------------------------------
001500080930
001700080930     d conta           s              5i 0
002300080930
002500080930     d §azute        e ds                  extname(azute00f)
002600080930     d                                     dtaara
002700080930     d §datiute      e ds                  extname(ddatiute)
003500080930     d kpjba         e ds
004200080930
004500080930     c     *entry        plist
004600150519     c                   parm                    requestCode
004601150519     c                   parm                    replyCode
004602150519     c                   parm                    inputFormat
004603150519     c                   parm                    inputData
004604150519     c                   parm                    inputSize
004605150519     c                   parm                    outputDormat
004606150519     c                   parm                    outputData
004607150519     c                   parm                    outputSize
004700080930
004800080930      /free
004900080930
005000080930         exec sql
005100080930          set option dynusrprf = *owner, closqlcsr = *endmod;
005200080930
005300080930       //?operazioni iniziali
005400080930         exsr routinz;
005500080930
005600080930         dow not $fine;
005700080930          exsr gesvid;
005800080930         enddo;
005900080930
006000080930         *inlr = *on;
006100080930
