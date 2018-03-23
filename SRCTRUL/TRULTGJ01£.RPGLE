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
001500121003      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
001600120914
001700120914     ***************************************************************************
001800120914     **
001900120914     ** Strutture dati.
002000120914     **
002100120914     ***************************************************************************
002200121003     D psds           SDS                  QUALIFIED
002300121003     D  jobNumber            264    269A
002400121003     D kpjba         E DS                  QUALIFIED
002500120914     D tfrGrpJob       DS                  QUALIFIED
002600120914     D  kpjba                              LIKEDS(kpjba)
002700120914     D  societa                       3A   INZ('201')
002800120914     D  esitoProj                     1A
002900120914     D  programma                    10A
003000120914
003100120914     ***************************************************************************
003200120914     **
003300120914     ** Parametri.
003400120914     **
003500120914     ***************************************************************************
003600120914     C     *ENTRY        PLIST
003700120914     C                   PARM                    kpjba
003800120914
003900120914      /FREE
004000120914
004100120914       *INLR = *ON;
004200120914
004300120914       tfrGrpJob.programma = kpjba.kpjbu;
004400120914       tfrGrpJob.kpjba = kpjba;
004500120914       CLEAR tfrGrpJob.kpjba.kpjbu;
004600120914
004700120914       SELECT;
004800120914         WHEN %SUBST(kpjba.knsif : 1 : 6) = 'FILTRA';
004900120914           %SUBST(tfrGrpJob.kpjba.knsif : 1 : 6) = 'GAITRA';
005000120914         WHEN %SUBST(kpjba.knsif : 1 : 6) = 'GAITRA';
005100120914           %SUBST(tfrGrpJob.kpjba.knsif : 1 : 6) = 'FILTRA';
005200120914       ENDSL;
005300120914
005400120914       Proj_TrasferimentoLavoro_setKpjba( tfrGrpJob.societa : tfrGrpJob.kpjba );
005500120914
005600120914       Proj_TrasferimentoLavoro_call( 'TRULTGJ02'
005700120914                                    : tfrGrpJob.societa
005800120914                                    : tfrGrpJob.programma
005900120914                                    : %SIZE(tfrGrpJob.programma)
006000120914                                    : tfrGrpJob.kpjba
006100120914                                    : %SIZE(tfrGrpJob.kpjba)
006200120914                                    );
006300120914
006400121003       ExecuteCommand('DLTUSRSPC USRSPC(QGPL/XJ' + psds.jobNumber + '*)');
006500121003
006600120914      /END-FREE
