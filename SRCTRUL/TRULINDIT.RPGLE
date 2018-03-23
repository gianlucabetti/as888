000100070131     H DFTACTGRP(*NO)
000200070131
000300070131     Fanind01l  IF   E           K DISK
000400070131
000500070131     D trulIndiR       PR                  EXTPGM('TRULINDIR')
000600070131     D  trulIndiS                          LIKEDS(trulIndiS)
000700070131     D trulIndiS     E DS                  INZ
000800070131     D  titolo       E                     INZ(' Spettabile')
000900070201     D  cvtUCase     E                     INZ(*OFF)
001000070131
001100070131     C     *ENTRY        PLIST
001200070131     C                   PARM                    indSogg
001300070131     C                   PARM                    indTpInd
001400070131     C                   PARM                    indCdInd
001500070131      /FREE
001600070131       *INLR = *ON;
001700070131       CHAIN (indSogg : indTpInd : indCdInd) anind01l;
001800070131       IF NOT %FOUND;
001900070131         RETURN;
002000070131       ENDIF;
002100070131       RESET trulIndiS;
002200070131       destinatar = indDesFil;
002300070131       destInfAgg = indDesAgg;
002400070131       via = indIndriz;
002500070131       cap = indCap;
002600070131       localita = indLocalit;
002700070131       provincia = indProv;
002800070131       statoEster = indStato;
002900070131       trulIndiR(trulIndiS);
003000070131       DSPLY indiRiga1;
003100070131       DSPLY indiRiga2;
003200070131       DSPLY indiRiga3;
003300070131       DSPLY indiRiga4;
003400070131       DSPLY indiRiga5;
003500070131       DSPLY indiRiga6;
003600070131       RETURN;
003700070131      /END-FREE
