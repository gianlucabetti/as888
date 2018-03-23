000100090323     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
000200090323
000300090323     D trmz0100i...
000400090323     D               E DS                  QUALIFIED
000500090323     D                                     INZ
000600090422     D  dataStampa...
000700090423     D               E                     INZ(D'2009-05-08')
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
002600090325
002700090323       RESET trmz0100i;
002800090423       trmz0100i.societa = '212';
002900090420       trmz0100i.contratDat = %DATE('2009-05-01');
003000090420       trmz0100i.contratNum = 159488;
003100090323       trmz0100i.filiale = 1;
003200090420       trmz0100i.vetRagSoc = 'STEFANI ALESSIO';
003300090420       trmz0100i.vetSlCom = 'SAN GIORGIO DI PIANO';
003400090420       trmz0100i.vetSlInd = 'VIA TORRICELLA 16';
003500090420       trmz0100i.vetSlCap = '40016';
003600090323       trmz0100i.vetSlPrv = 'BO';
003700090323       trmz0100i.vetSlNaz = 'IT';
003800090420       trmz0100i.vetCdFisca = 'STFLSS69S25C469S';
003900090420       trmz0100i.vetCdIva = '02018521209';
004000090324       trmz0100i.vetRimLuo = 'BOLOGNA';
004100090420       trmz0100i.vetRimNum = 'STFLSS69S25C469S';
004200090324       trmz0100i.vetReaLuo = 'BOLOGNA';
004300090420       trmz0100i.vetReaNum = '';
004400090324       trmz0100i.vetAnaPrv = 'BO';
004500090420       trmz0100i.vetAnaNum = '1234567A';
004600090323
004700090323       trmz0100r( 'PRTCONTRAT'
004800090323                : prmRpyOpCode
004900090323                : prmRpyIdMsg
005000090323                : 'TRMZ0100I'
005100090323                : trmz0100i
005200090323                : %SIZE(trmz0100i)
005300090323                );
005400090325
005500090325       IF prmRpyIdMsg < 0;
005600090325         // Stampa non riuscita.
005700090325       ENDIF;
005800090323
005900090323       RESET trmz0100i;
006000090617       trmz0100i.societa = '137';
006100090323       trmz0100i.contratDat = %DATE();
006200090323       trmz0100i.contratNum = 2;
006300090323       trmz0100i.filiale = 7;
006400090407       trmz0100i.vetRagSoc = 'AMBROSIONI F.LLI SNC AUTOTRASPORTI DI AMBROSIONI +
006500090407                              GIULIO RICCARDO MASSIMILIANO & C.';
006600090323       trmz0100i.vetSlCom = 'ALTAMURA';
006700090323       trmz0100i.vetSlInd = 'VIA N PICCINNI 37';
006800090323       trmz0100i.vetSlCap = '70022';
006900090323       trmz0100i.vetSlPrv = 'BA';
007000090323       trmz0100i.vetSlNaz = 'IT';
007100090323       trmz0100i.vetCdFisca = 'SGRMHL61S20A225N';
007200090323       trmz0100i.vetCdIva = '02938820723';
007300090324       trmz0100i.vetRimLuo = 'BARI';
007400090324       trmz0100i.vetRimNum = 'SGRMHL61S20A225N';
007500090324       trmz0100i.vetReaLuo = 'BARI';
007600090323       trmz0100i.vetReaNum = 'REA456';
007700090324       trmz0100i.vetAnaPrv = 'BA';
007800090324       trmz0100i.vetAnaNum = 'ALBO2';
007900090323
008000090323       trmz0100r( 'PRTCONTRAT'
008100090323                : prmRpyOpCode
008200090323                : prmRpyIdMsg
008300090323                : 'TRMZ0100I'
008400090323                : trmz0100i
008500090323                : %SIZE(trmz0100i)
008600090323                );
008700090325
008800090325       IF prmRpyIdMsg < 0;
008900090325         // Stampa non riuscita.
009000090325       ENDIF;
009100090323
009200090323       trmz0100r( 'FINALIZE'
009300090323                : prmRpyOpCode
009400090323                : prmRpyIdMsg
009500090323                );
009600090323
009700090323      /END-FREE
