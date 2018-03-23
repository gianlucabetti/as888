000100090107     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
000200090107
000300090107     D prmEsito        S              1A
000400090107     D prmFir          S             10A
000500090107     D prmLit          S             10A
000600090107     D tivlr00f      E DS                  QUALIFIED
000700090107     D                                     INZ
000800090107     D  vlrPpt       E                     INZ(' ; ,')
000801151112     D  vlrmsg       E                     INZ('IDPROVA')
000900090107     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
001100171201     D fidg64r         PR                  EXTPGM('FIDG64C')
001200090107     D  tivlr00f                           LIKEDS(tivlr00f)
001300090107     D  prmEsito                      1A
001400090107     D  prmFir                       10A
001500090107     D  prmLit                       10A
001600090107
001700090107      /FREE
001800090107
001900090107       *INLR = *ON;
002000090107
002100171201       ExecuteCommand('OVRDBF FILE(TIVIN00R) TOFILE(TIVIN00R) MBR(TL) +
002200090107                       OVRSCOPE(*CALLLVL)');
002300090107
002400171201       fidg64r( tivlr00f : prmEsito : prmFir : prmLit );
002500090107
002600090107       ExecuteCommand('DLTOVR FILE(TIVIN00R) LVL(*)');
002700090107
002800090107      /END-FREE
