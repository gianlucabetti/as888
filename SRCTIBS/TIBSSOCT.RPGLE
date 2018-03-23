000100090313     ***************************************************************************
000200090313     **
000300090313     ** Anagrafico società.
000400090313     ** Tester di TIBSSOCR.
000500090313     **
000600090313     ***************************************************************************
000700090313
000800090313     ***************************************************************************
000900090313     **
001000090313     ** Definizione costanti.
001100090313     **
001200090313     ***************************************************************************
001300090313     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
001400090313     D ESITO_OK...
001500090313     D                 C                   0
001600090316
001700090316     ***************************************************************************
001800090316     **
001900090316     ** Dichiarazione prototipi procedure esterne.
002000090316     **
002100090316     ***************************************************************************
002200090316      /DEFINE DFTACTGRP_YES
002300090316     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
002400090316      /UNDEFINE DFTACTGRP_YES
002500090313
002600090313     ***************************************************************************
002700090313     **
002800090313     ** Definizione strutture dati.
002900090313     **
003000090313     ***************************************************************************
003100090313     D tibsSocI0...
003200090313     D               E DS                  QUALIFIED
003300090313     D                                     INZ
003400090313     D tibsSocO0...
003500090313     D               E DS                  QUALIFIED
003600090313     D                                     INZ
003700090313
003800090313     ***************************************************************************
003900090313     **
004000090313     ** Definizione variabili modulo/programma.
004100090313     **
004200090313     ***************************************************************************
004300090313     D prmRqsOpCode...
004400090313     D                 S             10A
004500090313     D prmRpyOpCode...
004600090313     D                 S             10A
004700090313     D prmRpyIdMsg...
004800090313     D                 S             10I 0
004900090313     D prmRqsFormato...
005000090313     D                 S             10A
005100090313     D prmRqsDataSize...
005200090313     D                 S             10I 0
005300090313     D prmRpyFormato...
005400090313     D                 S             10A
005500090313     D prmRpyDataSize...
005600090313     D                 S             10I 0
005700090313
005800090313     ***************************************************************************
005900090313     **
006000090313     ** Main.
006100090313     **
006200090313     ***************************************************************************
006300090313     C                   EVAL      *INLR = *ON
006400090313
006500090316     ** Inizializzo il programma.
006600090313     C                   CALL      'TIBSSOCR'
006700090313     C                   PARM      'INIT'        prmRqsOpCode
006800090313     C                   PARM                    prmRpyOpCode
006900090313     C                   PARM                    prmRpyIdMsg
007000090313
007100090313     ** Chiamo senza passare l'id società.
007200090313     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
007300090313     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
007400090313     C                   RESET                   tibsSocI0
007500090313     C
007600090313     C                   CALL      'TIBSSOCR'
007700090313     C                   PARM      'GETANAGRAF'  prmRqsOpCode
007800090313     C                   PARM      *BLANK        prmRpyOpCode
007900090313     C                   PARM      *ZERO         prmRpyIdMsg
008000090313     C                   PARM      'TIBSSOCI0'   prmRqsFormato
008100090313     C                   PARM                    tibsSocI0
008200090313     C                   PARM                    prmRqsDataSize
008300090313     C                   PARM      'TIBSSOCO0'   prmRpyFormato
008400090313     C                   PARM                    tibsSocO0
008500090313     C                   PARM                    prmRpyDataSize
008600090313
008700090313     ** Chiamo passando un id società che esiste.
008800090313     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
008900090313     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
009000090313     C                   RESET                   tibsSocI0
009100090313     C                   EVAL      tibsSocI0.idSocieta = '201'
009200090313     C
009300090313     C                   CALL      'TIBSSOCR'
009400090313     C                   PARM      'GETANAGRAF'  prmRqsOpCode
009500090313     C                   PARM      *BLANK        prmRpyOpCode
009600090313     C                   PARM      *ZERO         prmRpyIdMsg
009700090313     C                   PARM      'TIBSSOCI0'   prmRqsFormato
009800090313     C                   PARM                    tibsSocI0
009900090313     C                   PARM                    prmRqsDataSize
010000090313     C                   PARM      'TIBSSOCO0'   prmRpyFormato
010100090313     C                   PARM                    tibsSocO0
010200090313     C                   PARM                    prmRpyDataSize
010300090313
010400090313     ** Chiamo passando un id società che esiste.
010500090313     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
010600090313     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
010700090313     C                   RESET                   tibsSocI0
010800091119     C                   EVAL      tibsSocI0.idSocieta = '250'
010900090313     C
011000090313     C                   CALL      'TIBSSOCR'
011100090313     C                   PARM      'GETANAGRAF'  prmRqsOpCode
011200090313     C                   PARM      *BLANK        prmRpyOpCode
011300090313     C                   PARM      *ZERO         prmRpyIdMsg
011400090313     C                   PARM      'TIBSSOCI0'   prmRqsFormato
011500090313     C                   PARM                    tibsSocI0
011600090313     C                   PARM                    prmRqsDataSize
011700090313     C                   PARM      'TIBSSOCO0'   prmRpyFormato
011800090313     C                   PARM                    tibsSocO0
011900090313     C                   PARM                    prmRpyDataSize
012000090313
012100090313     ** Chiamo passando un id società "ad minchiam".
012200090313     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
012300090313     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
012400090313     C                   RESET                   tibsSocI0
012500090313     C                   EVAL      tibsSocI0.idSocieta = 'XXX'
012600090313     C
012700090313     C                   CALL      'TIBSSOCR'
012800090313     C                   PARM      'GETANAGRAF'  prmRqsOpCode
012900090313     C                   PARM      *BLANK        prmRpyOpCode
013000090313     C                   PARM      *ZERO         prmRpyIdMsg
013100090313     C                   PARM      'TIBSSOCI0'   prmRqsFormato
013200090313     C                   PARM                    tibsSocI0
013300090313     C                   PARM                    prmRqsDataSize
013400090313     C                   PARM      'TIBSSOCO0'   prmRpyFormato
013500090313     C                   PARM                    tibsSocO0
013600090313     C                   PARM                    prmRpyDataSize
013700090313
013800090316     ** Chiudo il programma.
013900090313     C                   CALL      'TIBSSOCR'
014000090313     C                   PARM      'FINALIZE'    prmRqsOpCode
014100090313     C                   PARM                    prmRpyOpCode
014200090313     C                   PARM                    prmRpyIdMsg
