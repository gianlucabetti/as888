000100111202     H DFTACTGRP(*NO) ACTGRP(*NEW)
000200111202
000300090323     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
000400111202
000500090323     D trmz0100i...
000600090323     D               E DS                  QUALIFIED
000700090323     D                                     INZ
000800090323
000900090323     D prmRpyOpCode...
001000090323     D                 S             10A
001100090323     D prmRpyIdMsg...
001200090323     D                 S             10I 0
001300090323
001400090323      /FREE
001500090323
001600090323       *INLR = *ON;
001700090323
001800090323       trmz0100r( 'INIT'
001900090323                : prmRpyOpCode
002000090323                : prmRpyIdMsg
002100090323                );
002200090323
002300090325       IF prmRpyIdMsg < 0;
002400090325         RETURN;
002500090325       ENDIF;
002600111202
002700090323       RESET trmz0100i;
002800111221       trmz0100i.societa = '201';
002900111221       trmz0100i.contratNum = 9999999;
003000111221       trmz0100i.filiale = 1;
003100111221       trmz0100i.vetRagSoc = *ALL'_';
003200111221       trmz0100i.vetSlCom = *ALL'_';
003300111221       trmz0100i.vetSlInd = *ALL'_';
003400111221       trmz0100i.vetSlCap = *ALL'_';
003500111221       trmz0100i.vetSlPrv = *ALL'_';
003600111221       trmz0100i.vetSlNaz = 'IT';
003700111221       trmz0100i.vetCdFisca = *ALL'_';
003800111221       trmz0100i.vetCdIva = *ALL'_';
003900111221       trmz0100i.vetRimLuo = *ALL'_';
004000111221       trmz0100i.vetRimNum = *ALL'_';
004100111221       trmz0100i.vetReaLuo = *ALL'_';
004200090420       trmz0100i.vetReaNum = '';
004300111221       trmz0100i.vetAnaPrv = *ALL'_';
004400111221       trmz0100i.vetAnaNum = *ALL'_';
004500111202
004600111202       // Prima stampa.
004700111202
004800111202       trmz0100i.dataCertaP = %DATE('2011-11-28');
004900111202       trmz0100i.contratDat = %DATE('2012-01-01');
005000111202       trmz0100i.dataStampa = %DATE('2011-11-28');
005100111202       trmz0100i.dataFirmaV = %DATE('2012-11-29');
005200090323
005300090323       trmz0100r( 'PRTCONTRAT'
005400090323                : prmRpyOpCode
005500090323                : prmRpyIdMsg
005600090323                : 'TRMZ0100I'
005700090323                : trmz0100i
005800090323                : %SIZE(trmz0100i)
005900090323                );
006000090325
006100090325       IF prmRpyIdMsg < 0;
006200090325       ENDIF;
006300111202
006400111202       // Ristampa il 15 dicembre.
006500111202
006600111202       trmz0100i.dataStampa = %DATE('2011-12-15');
006700111202
006800111202       trmz0100r( 'PRTCONTRAT'
006900111202                : prmRpyOpCode
007000111202                : prmRpyIdMsg
007100111202                : 'TRMZ0100I'
007200111202                : trmz0100i
007300111202                : %SIZE(trmz0100i)
007400111202                );
007500111202
007600111202       // Ristampa il 15 gennaio.
007700111202
007800111202       trmz0100i.dataStampa = %DATE('2012-01-15');
007900111202
008000111202       trmz0100r( 'PRTCONTRAT'
008100111202                : prmRpyOpCode
008200111202                : prmRpyIdMsg
008300111202                : 'TRMZ0100I'
008400111202                : trmz0100i
008500111202                : %SIZE(trmz0100i)
008600111202                );
008700090323
008800090323       trmz0100r( 'FINALIZE'
008900090323                : prmRpyOpCode
009000090323                : prmRpyIdMsg
009100090323                );
009200090323
009300090323      /END-FREE
