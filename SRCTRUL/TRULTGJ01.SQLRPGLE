000100120914     ***************************************************************************
000200120914     **
000300120914     ** Questo programma reperisce dalla KPJBU il nome del programma da chiamare
000400120914     ** e lo chiama eseguendo un Trasferimento a lavoro gruppo.
000500120914     **
000600120914     ***************************************************************************
000700120914     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000800120914
000900120914     ***************************************************************************
001000120914     **
001100120914     ** Prototipi.
001200120914     **
001300120914     ***************************************************************************
001400120914      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
001500121029      /COPY GAITRASRC/SRCPROTOPR,QUILNGTX
001600121003      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
001700120914
001800120914     ***************************************************************************
001900120914     **
002000120914     ** Strutture dati.
002100120914     **
002200120914     ***************************************************************************
002300121029      /COPY QSYSINC/QRPGLESRC,QUSEC
002400121003     D psds           SDS                  QUALIFIED
002500121003     D  jobNumber            264    269A
002600121003     D kpjba         E DS                  QUALIFIED
002700120914     D tfrGrpJob       DS                  QUALIFIED
002800120914     D  kpjba                              LIKEDS(kpjba)
002900120914     D  societa                       3A   INZ('201')
003000120914     D  esitoProj                     1A
003100120914     D  programma                    10A
003200121029
003300121029     ***************************************************************************
003400121029     **
003500121029     ** Campi.
003600121029     **
003700121029     ***************************************************************************
003800121029     D isUtenteInProj...
003900121029     D                 S               N
004000121029     D msg             S            256A
004100121029     D usrPrf          S             10A
004200120914
004300120914     ***************************************************************************
004400120914     **
004500120914     ** Parametri.
004600120914     **
004700120914     ***************************************************************************
004800120914     C     *ENTRY        PLIST
004900120914     C                   PARM                    kpjba
005000120914
005100120914      /FREE
005200120914
005300120914       *INLR = *ON;
005400120914
005500121029       usrPrf = kpjba.knmus;
005600121029
005700121029       EXEC SQL
005800121030         SELECT CHAR('1', 1)
005900121029           INTO :isUtenteInProj
006000121029           FROM anutn00f
006100121029           WHERE anutn00f.utnPrf = :usrPrf
006200121030             AND anutn00f.utnAnn = ''
006300121029       ;
006400121029
006500121029       SELECT;
006600121029         WHEN sqlCode < *ZERO;
006700121029           DUMP(A);
006800121029           RETURN;
006900121029       ENDSL;
007000121029
007100121029       IF NOT isUtenteInProj;
007200121029         msg = 'L''utente ' + %TRIMR(usrPrf) + ' non è iscritto in Proj. +
007300121029                Impossibile eseguire l''azione. Premere Invio per tornare a +
007400121029                menù.';
007500121029         CLEAR qusec;
007600121029         qusbprv = %SIZE(qusec);
007700121029         DisplayLongText( msg : %SIZE(msg) : *BLANK : *BLANK : qusec );
007800121029         RETURN;
007900121029       ENDIF;
008000121029
008100120914       tfrGrpJob.programma = kpjba.kpjbu;
008200120914       tfrGrpJob.kpjba = kpjba;
008300120914       CLEAR tfrGrpJob.kpjba.kpjbu;
008400120914
008500120914       SELECT;
008600120914         WHEN %SUBST(kpjba.knsif : 1 : 6) = 'FILTRA';
008700120914           %SUBST(tfrGrpJob.kpjba.knsif : 1 : 6) = 'GAITRA';
008800120914         WHEN %SUBST(kpjba.knsif : 1 : 6) = 'GAITRA';
008900120914           %SUBST(tfrGrpJob.kpjba.knsif : 1 : 6) = 'FILTRA';
009000120914       ENDSL;
009100120914
009200120914       Proj_TrasferimentoLavoro_setKpjba( tfrGrpJob.societa : tfrGrpJob.kpjba );
009300120914
009400120914       Proj_TrasferimentoLavoro_call( 'TRULTGJ02'
009500120914                                    : tfrGrpJob.societa
009600120914                                    : tfrGrpJob.programma
009700120914                                    : %SIZE(tfrGrpJob.programma)
009800120914                                    : tfrGrpJob.kpjba
009900120914                                    : %SIZE(tfrGrpJob.kpjba)
010000120914                                    );
010100120914
010200121003       ExecuteCommand('DLTUSRSPC USRSPC(QGPL/XJ' + psds.jobNumber + '*)');
010300121003
010400120914      /END-FREE
