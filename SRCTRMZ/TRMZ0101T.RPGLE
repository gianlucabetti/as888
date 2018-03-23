000100111128     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0101R
000200090323
000300111128     D trmz0101i...
000400090323     D               E DS                  QUALIFIED
000500090323     D                                     INZ
000600090422     D  dataStampa...
000700111130     D               E                     INZ(D'2011-12-07')
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
001800111128       trmz0101r( 'INIT'
001900090323                : prmRpyOpCode
002000090323                : prmRpyIdMsg
002100090323                );
002200090323
002300090325       IF prmRpyIdMsg < 0;
002400090325         RETURN;
002500090325       ENDIF;
002600090325
002700111128       RESET trmz0101i;
002800111128       trmz0101i.societa = '212';
002900111128       trmz0101i.contratDat = %DATE('2009-05-01');
003000111128       trmz0101i.contratNum = 159488;
003100111128       trmz0101i.filiale = 1;
003200111128       trmz0101i.vetRagSoc = 'STEFANI ALESSIO';
003300111128       trmz0101i.vetSlCom = 'SAN GIORGIO DI PIANO';
003400111128       trmz0101i.vetSlInd = 'VIA TORRICELLA 16';
003500111128       trmz0101i.vetSlCap = '40016';
003600111128       trmz0101i.vetSlPrv = 'BO';
003700111128       trmz0101i.vetSlNaz = 'IT';
003800111128       trmz0101i.vetCdFisca = 'STFLSS69S25C469S';
003900111128       trmz0101i.vetCdIva = '02018521209';
004000111128       trmz0101i.vetRimLuo = 'BOLOGNA';
004100111128       trmz0101i.vetRimNum = 'STFLSS69S25C469S';
004200111128       trmz0101i.vetReaLuo = 'BOLOGNA';
004300111128       trmz0101i.vetReaNum = '';
004400111128       trmz0101i.vetAnaPrv = 'BO';
004500111128       trmz0101i.vetAnaNum = '1234567A';
004600111130       trmz0101i.DATACERTAP = %DATE('2012-01-01');
004700111130       trmz0101i.DATAFIRMAV = %DATE('2012-01-02');
004800111206       trmz0101i.DATADECAPP = trmz0101i.dataStampa;
004900090323
005000111128       trmz0101r( 'PRTCONTRAT'
005100090323                : prmRpyOpCode
005200090323                : prmRpyIdMsg
005300111128                : 'TRMZ0101I'
005400111128                : trmz0101i
005500111128                : %SIZE(trmz0101i)
005600090323                );
005700090325
005800090325       IF prmRpyIdMsg < 0;
005900090325         // Stampa non riuscita.
006000090325       ENDIF;
006100090323
006200090323
006300111128       trmz0101r( 'FINALIZE'
006400090323                : prmRpyOpCode
006500090323                : prmRpyIdMsg
006600090323                );
006700090323
006800090323      /END-FREE
