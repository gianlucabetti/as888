000100100716     ***************************************************************************
000200100716     **
000300100716     ** Questo programma esegue delle elaborazioni prima della pulizia dati
000400100716     ** remote banking.
000500100716     **
000600100716     ***************************************************************************
000700100719     H DFTACTGRP(*NO) BNDDIR('TRUL')
000800100716
000900100716     ***************************************************************************
001000100716     **
001100100716     ** Costanti.
001200100716     **
001300100716     ***************************************************************************
001400100728      /COPY GAITRASRC/SRCCONST,X24FPCR
001500100716     D ESITO_OK...
001600100716     D                 C                   0
001700100719     D NUOVA_RIGA...
001800100719     D                 C                   ':/N'
001900100719     D NUOVO_PARAGRAFO...
002000100719     D                 C                   ':/P'
002100100719     D REPLACE...
002200100719     D                 C                   'R'
002300100728     D SOCIETA_CAPOFILA_ANAGRAFICA...
002400100728     D                 C                   '888'
002500100716     D SQLSTATE_ESISTE_GIA...
002600100716     D                 C                   '42710'
002700100728     D WRKQUERY...
002800100728     D                 C                   'WRKQUERY'
002900100716
003000100716     ***************************************************************************
003100100716     **
003200100716     ** Prototipi.
003300100716     **
003400100716     ***************************************************************************
003500100716      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
003600100719      /COPY GAITRASRC/SRCPROTOPR,TRULEMAIL
003700100728      /COPY GAITRASRC/SRCPROTOPR,X24FPCR
003800100716     D Proj_ndck61r...
003900100716     D                 PR                  EXTPGM('PJCI1O3/NDCK61R')
004000100716     D  kpjba                              LIKEDS(kpjba)
004100100716
004200100716     ***************************************************************************
004300100716     **
004400100716     ** Strutture dati.
004500100716     **
004600100716     ***************************************************************************
004700100716     D kpjba         E DS                  QUALIFIED
004800100716     D ndck60ds      E DS                  QUALIFIED
004900100728     D programStatus  SDS                  QUALIFIED
005000100728     D  procName         *PROC
005100100728     D x24fpcds      E DS                  QUALIFIED
005200100728     D                                     INZ
005300100728     D  x24Libre     E                     INZ(WRKQUERY)
005400100728     D  x24FunCre    E                     INZ('YPULHB')
005500100728     D  x24FileM     E                     INZ('NDRI100F')
005600100719
005700100719     ***************************************************************************
005800100719     **
005900100719     ** Campi.
006000100719     **
006100100719     ***************************************************************************
006200100719     D emailOggetto...
006300100719     D                 S             44A   VARYING
006400100719     D                                     INZ('Pulizie periodiche home +
006500100719     D                                     banking.')
006600100716
006700100716     D*--------------------------------------------------
006800100716     D* Procedure name: PuliziaMovimentiEC_prima
006900100716     D* Purpose:        Elaborazioni da eseguire prima della pulizia dei mo...
007000100716     D*                          vimenti E/C
007100100716     D* Returns:
007200100716     D*--------------------------------------------------
007300100716     D PuliziaMovimentiEC_prima...
007400100716     D                 PR            10I 0
007500100716
007600100716
007700100716     ***************************************************************************
007800100716     **
007900100716     ** Parametri.
008000100716     **
008100100716     ***************************************************************************
008200100716     C     *ENTRY        PLIST
008300100716     C                   PARM                    kpjba
008400100716
008500100716      /FREE
008600100716
008700100716       *INLR = *ON;
008800100716
008900100716       ndck60ds = kpjba.kpjbu;
009000100716
009100100716       IF ndck60ds.mecK60 = *ON;
009200100716         IF PuliziaMovimentiEC_prima() < ESITO_OK;
009300100719           ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
009400100716           RETURN;
009500100716         ENDIF;
009600100716       ENDIF;
009700100716
009800100716       Proj_ndck61r( kpjba );
009900100716
010000100728       //***********************************************************************
010100100728       //
010200100728       // Inizializzazioni.
010300100728       //
010400100728       //***********************************************************************
010500100728
010600100728       BEGSR *INZSR;
010700100728
010800100728         x24fpcds.x24Pgm = programStatus.procName;
010900100728         x24fpcds.x24Utente = kpjba.knmus;
011000100728
011100100728       ENDSR;
011200100728
011300100716      /END-FREE
011400100716
011500100716     P*--------------------------------------------------
011600100716     P* Procedure name: PuliziaMovimentiEC_prima
011700100716     P* Purpose:        Elaborazioni da eseguire prima della pulizia dei mo...
011800100716     P*                          vimenti E/C
011900100716     P* Returns:
012000100716     P*--------------------------------------------------
012100100716     P PuliziaMovimentiEC_prima...
012200100716     P                 B
012300100716     D PuliziaMovimentiEC_prima...
012400100716     D                 PI            10I 0
012500100716
012600100719     D retField...
012700100716     D                 S             10I 0
012800100719     D righeInserite...
012900100719     D                 S             10I 0
013000100716     D stm...
013100100716     D                 S            255A   VARYING
013200100716     D schemaName...
013300100716     D                 S             10A   VARYING
013400100728     D                                     INZ(WRKQUERY)
013500100716     D tableName...
013600100716     D                 S             20A   VARYING
013700100716     D tableSystemName...
013800100716     D                 S             10A   VARYING
013900100716
014000100716      /FREE
014100100716
014200100716       tableName = 'NDRI100F_'
014300100716                 + %CHAR(ndck60ds.dtLimK60 : *ISO0);
014400100716
014500100716       tableSystemName = 'NDRI1_'
014600100720                       + %SUBST(%CHAR(ndck60ds.dtLimK60) : 1 : 4);
014700100716
014800100716       stm = 'CREATE TABLE ' + schemaName + '/' + tableSystemName
014900100716           + ' LIKE NDRI100F';
015000100716
015100100716       EXEC SQL
015200100716         EXECUTE IMMEDIATE :stm
015300100716       ;
015400100716
015500100716       IF sqlState > '03';
015600100716         IF sqlState <> SQLSTATE_ESISTE_GIA;
015700100716           DUMP(A);
015800100716           RETURN sqlCode;
015900100716         ENDIF;
016000100716       ELSE;
016100100716         stm = 'LABEL ON TABLE ' + schemaName + '/' + tableSystemName
016200100719             + ' IS ''NDRI100F pulito al ' + %CHAR(ndck60ds.dtLimK60) + '.''';
016300100716         EXEC SQL
016400100716           EXECUTE IMMEDIATE :stm
016500100716         ;
016600100716         IF sqlState > '03';
016700100716           DUMP(A);
016800100716         ENDIF;
016900100716       ENDIF;
017000100719
017100100720       stm = 'CREATE ALIAS QTEMP/WRKQUERY_NDRI1_AAAA FOR '
017200100719           + schemaName + '/' + tableSystemName;
017300100719
017400100719       EXEC SQL
017500100719         EXECUTE IMMEDIATE :stm
017600100719       ;
017700100719
017800100719       ExecuteCommand('STRJRNPF FILE('
017900100719                      + schemaName + '/' + tableSystemName
018000100719                      + ') JRN(PJJRN)');
018100100716
018200100716       EXEC SQL
018300100720         INSERT INTO QTEMP/WRKQUERY_NDRI1_AAAA
018400100719           SELECT *
018500100719           FROM NDRI100F
018600100719           WHERE RI1SOCIETA = :ndck60ds.societaK60
018700100719             AND RI1DOP <= :ndck60ds.dtLimK60
018800100719             AND RI1STATUS = '1'
018900100719           WITH UR
019000100719       ;
019100100716
019200100719       email_setAddressFromUser( kpjba.knmus : REPLACE );
019300100719
019400100716       IF sqlState > '03';
019500100728
019600100716         DUMP(A);
019700100716         retField = sqlCode;
019800100728
019900100716         EXEC SQL
020000100716           ROLLBACK
020100100716         ;
020200100728
020300100719         IF email_send( emailOggetto
020400100719                      : 'La copia delle righe per la società '
020500100719                      + ndck60ds.societaK60
020600100719                      + ' dalla tabella NDRI100F alla tabella '
020700100719                      + schemaName + '/' + tableSystemName
020800100719                      + ' è terminata in modo anomalo.'
020900100719                      + NUOVO_PARAGRAFO
021000100728                      + 'Elaborazione interrotta.'
021100100719                      + NUOVO_PARAGRAFO
021200100719                      + 'Inoltrare questo messaggio +
021300100719                        al responsabile della applicazione.'
021400100719                      ) < ESITO_OK;
021500100719           email_sndBrkMsg( emailOggetto
021600100719                          + ' La copia delle righe per la società '
021700100719                          + ndck60ds.societaK60
021800100719                          + ' dalla tabella NDRI100F alla tabella '
021900100719                          + schemaName + '/' + tableSystemName
022000100719                          + ' è terminata in modo anomalo. Elaborazione +
022100100728                            interrotta. Inoltrare questo messaggio +
022200100719                            al responsabile della applicazione.'
022300100719                          );
022400100719         ENDIF;
022500100728
022600100719       ELSE;
022700100728
022800100719         righeInserite = sqlErrD(3);
022900100728
023000100719         EXEC SQL
023100100719           COMMIT
023200100719         ;
023300100728
023400100728         x24fpcds.x24Opz = PROJ_CATALOGOFILESPC_AGGIORNAMENTO_CREAZIONEFILENO;
023500100728         x24fpcds.x24Societa = SOCIETA_CAPOFILA_ANAGRAFICA;
023600100728         x24fpcds.x24File = tableSystemName;
023700100728         x24fpcds.x24Descriz = 'NDRI100F pulito al '
023800100728                             + %CHAR(ndck60ds.dtLimK60) + '.';
023900100728
024000100728         Proj_CatalogoFilesPC( x24fpcds );
024100100728
024200100728         IF email_send( emailOggetto
024300100719                      : 'Copiate ' + %CHAR(righeInserite)
024400100719                      + ' righe per la società '
024500100719                      + ndck60ds.societaK60
024600100719                      + ' dalla tabella NDRI100F alla tabella '
024700100719                      +  schemaName + '/' + tableSystemName + '.'
024800100719                      ) < ESITO_OK;
024900100719           email_sndBrkMsg( emailOggetto
025000100719                          + ' Copiate ' + %CHAR(righeInserite)
025100100719                          + ' righe per la società ' + ndck60ds.societaK60
025200100719                          + ' dalla tabella NDRI100F alla tabella '
025300100719                          +  schemaName + '/' + tableSystemName + '.'
025400100719                          );
025500100719         ENDIF;
025600100728
025700100716       ENDIF;
025800100719
025900100719       EXEC SQL
026000100720         DROP ALIAS QTEMP/WRKQUERY_NDRI1_AAAA
026100100719       ;
026200100716
026300100716       ExecuteCommand('ENDJRNPF FILE('
026400100716                      + schemaName + '/' + tableSystemName
026500100716                      + ')');
026600100716
026700100716       RETURN retField;
026800100716
026900100716      /END-FREE
027000100716     P PuliziaMovimentiEC_prima...
027100100716     P                 E
027200100716
