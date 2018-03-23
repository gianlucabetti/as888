000100080204      //************************************************************************
000200080204      //
000300080204      // Questo programma calcola l'IBAN dal BBAN per i clienti.
000400080204      //
000500080204      //************************************************************************
000600080213     H DFTACTGRP(*NO) ACTGRP('BBAN2IBAN') BNDDIR('QC2LE')
000700080204
000800080204      //************************************************************************
000900080204      //
001000080204      // Dichiarazione files.
001100080204      //
001200080204      //************************************************************************
001300080213     Fprtf198   O    F  198        PRINTER OFLIND(*INOA) USROPN
001400080204
001500080204      //************************************************************************
001600080204      //
001700080204      // Definizioni costanti.
001800080204      //
001900080204      //************************************************************************
002000080207     D SQL_RIGA_NONTROVATA...
002100080207     D                 C                   100
002200080207     D PROJ_IBAN_CONTROLLO...
002300080205     D                 C                   *BLANK
002400080207     D PROJ_IBAN_CALCOLO...
002500080204     D                 C                   '01'
002600080204     D PROJ_IBAN_VALIDO...
002700080204     D                 C                   '0'
002800080204     D PROJ_IBAN_NONVALIDO...
002900080204     D                 C                   '1'
003000080207     D PROJ_SPORTELLOBANCARIO_TROVATO...
003100080206     D                 C                   '0'
003200080207     D PROJ_SPORTELLOBANCARIO_NONTROVATO...
003300080206     D                 C                   '1'
003400080204     D ITALIA...
003500080204     D                 C                   'IT'
003600080206     D SANMARINO...
003700080206     D                 C                   'SM'
003800080204
003900080204      //************************************************************************
004000080204      //
004100080204      // Dichiarazione procedure.
004200080204      //
004300080204      //************************************************************************
004400080204     D/COPY GAITRASRC/SRCPROTOPR,XCHKIBAN
004500080206     D/COPY GAITRASRC/SRCPROTOPR,X59ABI
004600080213     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
004700080213     D/COPY GAITRASRC/SRCPROTOPR,CEETREC
004800080204
004900080204      //************************************************************************
005000080204      //
005100080204      // Dichiarazione strutture dati.
005200080204      //
005300080204      //************************************************************************
005400080204     D cnDizion      E DS                  BASED(nullPtr) QUALIFIED
005500080204     D cbaIbanDs     E DS                  BASED(nullPtr) QUALIFIED
005600080206     D x59AbiDs      E DS                  INZ QUALIFIED
005700080204
005800080204      //************************************************************************
005900080204      //
006000080204      // Dichiarazione variabili.
006100080204      //
006200080204      //************************************************************************
006300080204     D aggiornare      S               N
006400080204     D nullPtr         S               *
006500080204     D clpKut          S                   LIKE(cnDizion.$Kut)
006600080204     D clpKcc          S                   LIKE(cnDizion.$Kcc)
006700080204     D clpKsc          S                   LIKE(cnDizion.$Ksc)
006800080204     D clpBab          S                   LIKE(cnDizion.$Bab)
006900080204     D clpCcb          S                   LIKE(cnDizion.$Ccb)
007000080204     D clpAbi          S                   LIKE(cnDizion.$Abi)
007100080204     D clpCab          S                   LIKE(cnDizion.$Cab)
007200080204     D statoIso        S                   LIKE(cbaIbanDs.cbaStato)
007300080204     D checkDigitIban...
007400080204     D                 S                   LIKE(cbaIbanDs.cbaCheck)
007500080204     D bban            S                   LIKE(cbaIbanDs.cbaCoori)
007600080205     D esito           S              1A
007700080204     D opzione         S              2A
007800080204     D elaborati       S             10U 0
007900080204     D validi          S             10U 0
008000080204     D calcolati       S             10U 0
008100080204     D nonCalcolati    S             10U 0
008200080204     D aggiornati      S             10U 0
008300080205     D puliti          S             10U 0
008400080213
008500080213     D*--------------------------------------------------
008600080213     D* Procedure name: aggiornamento
008700080213     D* Purpose:
008800080213     D* Returns:
008900080213     D*--------------------------------------------------
009000080213     D aggiornamento   PR
009100080213
009200080213
009300080204      //************************************************************************
009400080204      //
009500080204      // Dichiarazione parametri.
009600080204      //
009700080204      //************************************************************************
009800080204     C     *ENTRY        PLIST
009900080204     C                   PARM                    aggiornare
010000080204
010100080205      /FREE
010200080205
010300080205       //***********************************************************************
010400080205       //
010500080205       // Main.
010600080205       //
010700080205       //***********************************************************************
010800080204
010900080204       *INLR = *ON;
011000080204
011100080204       // Non ho ricevuto parametri.
011200080204       IF %PARMS() < 1;
011300080204         DUMP(A);
011400080213         CEE_NormalEnd();
011500080204       ENDIF;
011600080204
011700080213
011800080213       //***********************************************************************
011900080213       //
012000080213       // Aggiorno anagrafico clienti..
012100080213       //
012200080213       //***********************************************************************
012300080213
012400080213       EXEC SQL CREATE ALIAS QTEMP/CNIND_TEMP FOR UNITRAGRU/CNIND00F;
012500080213
012600080213       IF sqlCode < 0;
012700080213         DUMP(A);
012800080213         CEE_NormalEnd();
012900080213       ENDIF;
013000080213
013100080213       EXEC SQL CREATE ALIAS QTEMP/CNCLP_TEMP FOR UNITRAGRU/CNCLP00F;
013200080213
013300080213       IF sqlCode < 0;
013400080213         DUMP(A);
013500080213         CEE_NormalEnd();
013600080213       ENDIF;
013700080213
013800080213       EXEC SQL CREATE ALIAS QTEMP/CNACO_TEMP FOR UNITRAGRU/CNACO00F;
013900080213
014000080213       IF sqlCode < 0;
014100080213         DUMP(A);
014200080213         CEE_NormalEnd();
014300080213       ENDIF;
014400080213
014500080213       ExecuteCommand('OVRPRTF FILE(PRTF198) USRDTA(BBAN2IBANC)');
014600080213       OPEN prtf198;
014700080213
014800080213       aggiornamento();
014900080213
015000080213       CLOSE prtf198;
015100080213       ExecuteCommand('DLTOVR FILE(PRTF198)');
015200080213       EXEC SQL DROP ALIAS QTEMP/CNIND_TEMP;
015300080213       EXEC SQL DROP ALIAS QTEMP/CNCLP_TEMP;
015400080213       EXEC SQL DROP ALIAS QTEMP/CNACO_TEMP;
015500080213
015600080213       //***********************************************************************
015700080213       //
015800080213       // Aggiorno anagrafico clienti per tariffe offerte.
015900080213       //
016000080213       //***********************************************************************
016100080213
016200080213       EXEC SQL CREATE ALIAS QTEMP/CNIND_TEMP FOR FILTRAGRU/TFIND00F;
016300080213
016400080213       IF sqlCode < 0;
016500080213         DUMP(A);
016600080213         CEE_NormalEnd();
016700080213       ENDIF;
016800080213
016900080213       EXEC SQL CREATE ALIAS QTEMP/CNCLP_TEMP FOR FILTRAGRU/TFCLP00F;
017000080213
017100080213       IF sqlCode < 0;
017200080213         DUMP(A);
017300080213         CEE_NormalEnd();
017400080213       ENDIF;
017500080213
017600080213       EXEC SQL CREATE ALIAS QTEMP/CNACO_TEMP FOR FILTRAGRU/TFACO00F;
017700080213
017800080213       IF sqlCode < 0;
017900080213         DUMP(A);
018000080213         CEE_NormalEnd();
018100080213       ENDIF;
018200080213
018300080213       ExecuteCommand('OVRPRTF FILE(PRTF198) USRDTA(BBAN2IBANT)');
018400080213       OPEN prtf198;
018500080213
018600080213       aggiornamento();
018700080213
018800080213       CLOSE prtf198;
018900080213       ExecuteCommand('DLTOVR FILE(PRTF198)');
019000080213       EXEC SQL DROP ALIAS QTEMP/CNIND_TEMP;
019100080213       EXEC SQL DROP ALIAS QTEMP/CNCLP_TEMP;
019200080213       EXEC SQL DROP ALIAS QTEMP/CNACO_TEMP;
019300080204
019400080213       CEE_NormalEnd();
019500080213
019600080204      /END-FREE
019700080204
019800080213      //************************************************************************
019900080213      //
020000080213      // Stampa log.
020100080213      //
020200080213      //************************************************************************
020300080204     Oprtf198   E            ibanValido  1
020400080204     O                       clpKsc
020500080204     O                       clpBab              +1
020600080205     O                       clpCcb              +1
020700080205     O                       clpAbi              +1
020800080205     O                       clpCab              +1
020900080205     O                                           +1 'Valido'
021000080205     O          E            ibanPulito  1
021100080205     O                       clpKsc
021200080205     O                       clpBab              +1
021300080205     O                       clpCcb              +1
021400080205     O                       clpAbi              +1
021500080205     O                       clpCab              +1
021600080205     O                                           +1 'Pulito'
021700080204     O          E            ibanCalcol  1
021800080204     O                       clpKsc
021900080204     O                       clpBab              +1
022000080204     O                       clpCcb              +1
022100080204     O                       clpAbi              +1
022200080204     O                       clpCab              +1
022300080205     O                                           +1 'Calcolato'
022400080205     O          E            ibanNonCal  1
022500080205     O                       clpKsc
022600080205     O                       clpBab              +1
022700080205     O                       clpCcb              +1
022800080205     O                       clpAbi              +1
022900080205     O                       clpCab              +1
023000080205     O                                           +1 'Non calcolabile'
023100080204     O          E            totali      2
023200080204     O                                              'Elaborati:'
023300080204     O                       elaborati     Z
023400080204     O                                           +1 'Validi:'
023500080204     O                       validi        Z
023600080205     O                                           +1 'Puliti:'
023700080205     O                       puliti        Z
023800080204     O                                           +1 'Calcolati:'
023900080204     O                       calcolati     Z
024000080205     O                                           +1 'Non calcolabili:'
024100080204     O                       nonCalcolati  Z
024200080204     O                                           +1 'Aggiornati:'
024300080204     O                       aggiornati    Z
024400080213
024500080213     P*--------------------------------------------------
024600080213     P* Procedure name: aggiornamento
024700080213     P* Purpose:
024800080213     P* Returns:
024900080213     P*--------------------------------------------------
025000080213     P aggiornamento   B
025100080213     D aggiornamento   PI
025200080213
025300080213      /FREE
025400080213
025500080213       CLEAR elaborati;
025600080213       CLEAR validi;
025700080213       CLEAR calcolati;
025800080213       CLEAR nonCalcolati;
025900080213       CLEAR aggiornati;
026000080213       CLEAR puliti;
026100080213
026200080213       //***********************************************************************
026300080213       //
026400080213       // Pulizia dei campi numero conto e descrizione sportello su CNIND00F.
026500080213       //
026600080213       //***********************************************************************
026700080213
026800080213       IF aggiornare;
026900080213         EXEC SQL UPDATE QTEMP/CNIND_TEMP
027000080213                  SET INDBNA = ''
027100080213                    , INDCCB = ''
027200080213                  WHERE INDKUT = 1
027300080213                    AND INDKCC = 151
027400080213                    AND (INDBNA <> '' OR INDCCB <> '')
027500080213         ;
027600080213         IF sqlCode < 0;
027700080213           DUMP(A);
027800080213         ENDIF;
027900080213       ENDIF;
028000080213
028100080213       //***********************************************************************
028200080213       //
028300080213       // Calcolo IBAN da BBAN o pulizia descrizione sportello su CNCLP00F.
028400080213       //
028500080213       //***********************************************************************
028600080213
028700080213       IF aggiornare;
028800080213         EXEC SQL SET TRANSACTION ISOLATION LEVEL
028900080213                  READ UNCOMMITTED, READ WRITE
029000080213         ;
029100080213         EXEC SQL DECLARE CLIENTI_AGGIORNARE CURSOR FOR
029200080213                  SELECT CLPKUT, CLPKCC, CLPKSC, CLPBAB, CLPCCB, CLPABI, CLPCAB
029300080213                  FROM QTEMP/CNCLP_TEMP
029400080213                  WHERE CLPKUT = 1
029500080213                    AND CLPKCC = 151
029600080213                    AND (CLPBAB <> ''
029700080213                         OR (CLPCCB <> ''
029800080213                             AND CLPABI > 0
029900080213                             AND CLPCAB > 0
030000080213                            )
030100080213                        )
030200080213                  ORDER BY CLPKUT, CLPKCC, CLPKSC
030300080213                  FOR UPDATE OF CLPBAB
030400080213         ;
030500080213       ELSE;
030600080213         EXEC SQL DECLARE CLIENTI_CONTROLLO CURSOR FOR
030700080213                  SELECT CLPKUT, CLPKCC, CLPKSC, CLPBAB, CLPCCB, CLPABI, CLPCAB
030800080213                  FROM QTEMP/CNCLP_TEMP
030900080213                  WHERE CLPKUT = 1
031000080213                    AND CLPKCC = 151
031100080213                    AND (CLPBAB <> ''
031200080213                         OR (CLPCCB <> ''
031300080213                             AND CLPABI > 0
031400080213                             AND CLPCAB > 0
031500080213                            )
031600080213                        )
031700080213                  ORDER BY CLPKUT, CLPKCC, CLPKSC
031800080213                  FOR FETCH ONLY
031900080213         ;
032000080213       ENDIF;
032100080213
032200080213       IF sqlCode < 0;
032300080213         DUMP(A);
032400080213         RETURN;
032500080213       ENDIF;
032600080213
032700080213       IF aggiornare;
032800080213         EXEC SQL OPEN CLIENTI_AGGIORNARE;
032900080213       ELSE;
033000080213         EXEC SQL OPEN CLIENTI_CONTROLLO;
033100080213       ENDIF;
033200080213
033300080213       IF sqlCode < 0;
033400080213         DUMP(A);
033500080213         RETURN;
033600080213       ENDIF;
033700080213
033800080213       DOU sqlCode = SQL_RIGA_NONTROVATA;
033900080213
034000080213         IF aggiornare;
034100080213           EXEC SQL FETCH NEXT FROM CLIENTI_AGGIORNARE
034200080213                    INTO :clpKut, :clpKcc, :clpKsc, :clpBab, :clpCcb, :clpAbi
034300080213                       , :clpCab
034400080213           ;
034500080213         ELSE;
034600080213           EXEC SQL FETCH NEXT FROM CLIENTI_CONTROLLO
034700080213                    INTO :clpKut, :clpKcc, :clpKsc, :clpBab, :clpCcb, :clpAbi
034800080213                       , :clpCab
034900080213           ;
035000080213         ENDIF;
035100080213
035200080213         IF sqlCode < 0;
035300080213           DUMP(A);
035400080213           LEAVE;
035500080213         ELSEIF sqlCode = SQL_RIGA_NONTROVATA;
035600080213           LEAVE;
035700080213         ENDIF;
035800080213
035900080213         elaborati += 1;
036000080213
036100080213         // Verifico se la colonna CLPBAB contiene già un IBAN.
036200080213         IF clpBab = *BLANK;
036300080213           esito = PROJ_IBAN_NONVALIDO;
036400080213         ELSE;
036500080213           statoIso = %SUBST(clpBab : 1);
036600080213           checkDigitIban = %SUBST(clpBab : 3);
036700080213           bban = %SUBST(clpBab : 5);
036800080213           CLEAR esito;
036900080213           opzione = PROJ_IBAN_CONTROLLO;
037000080213           Proj_IBAN( statoIso : checkDigitIban : bban : esito : opzione );
037100080213         ENDIF;
037200080213
037300080213         IF esito = PROJ_IBAN_VALIDO;
037400080213
037500080213           validi += 1;
037600080213           EXCEPT ibanValido;
037700080213
037800080213         ELSE;
037900080213
038000080213           CLEAR clpBab;
038100080213
038200080213           // Se l'IBAN non è valido (o non c'è) provo a desumerlo dal BBAN
038300080213           // italiano ma solo se ho tutti i dati.
038400080213           IF clpAbi > 0 AND clpCab > 0 AND %SUBST(clpCcb : 16 : 1) <> ''
038500080213           AND %LEN(%TRIMR(%SUBST(clpCcb : 1 : 15))) = 12;
038600080213             RESET x59AbiDs;
038700080213             x59AbiDs.x59Abi = %EDITC(clpAbi : 'X');
038800080213             x59AbiDs.x59Cab = %EDITC(clpCab : 'X');
038900080213             Proj_sportelloBancario(x59AbiDs);
039000080213             IF x59AbiDs.x59Err = PROJ_SPORTELLOBANCARIO_TROVATO;
039100080213               statoIso = x59AbiDs.x59Stato;
039200080213               IF x59AbiDs.x59Stato = ITALIA AND x59AbiDs.x59Prov = SANMARINO;
039300080213                 statoIso = SANMARINO;
039400080213               ENDIF;
039500080213             ELSE;
039600080213               statoIso = ITALIA;
039700080213             ENDIF;
039800080213             IF statoIso = ITALIA OR statoIso = SANMARINO;
039900080213               CLEAR checkDigitIban;
040000080213               bban = %SUBST(clpCcb : 16 : 1) + %EDITC(clpAbi : 'X')
040100080213               + %EDITC(clpCab : 'X') + %SUBST(clpCcb : 1 : 15);
040200080213               CLEAR esito;
040300080213               opzione = PROJ_IBAN_CALCOLO;
040400080213               Proj_IBAN( statoIso : checkDigitIban : bban : esito : opzione );
040500080213             ELSE;
040600080213               esito = PROJ_IBAN_NONVALIDO;
040700080213             ENDIF;
040800080213             IF esito = PROJ_IBAN_VALIDO;
040900080213               clpBab = statoIso + checkDigitIban + bban;
041000080213               calcolati += 1;
041100080213               EXCEPT ibanCalcol;
041200080213             ELSE;
041300080213               nonCalcolati += 1;
041400080213               EXCEPT ibanNonCal;
041500080213             ENDIF;
041600080213           ELSE;
041700080213             puliti += 1;
041800080213             EXCEPT ibanPulito;
041900080213           ENDIF;
042000080213
042100080213           IF aggiornare;
042200080213             // Aggiorno sempre l'anagrafica del cliente con l'IBAN calcolato o pulito.
042300080213             EXEC SQL UPDATE QTEMP/CNCLP_TEMP
042400080213                      SET CLPBAB = :clpBab
042500080213                      WHERE CURRENT OF CLIENTI_AGGIORNARE
042600080213             ;
042700080213             IF sqlCode < 0;
042800080213               DUMP(A);
042900080213             ELSE;
043000080213               // Metto in trasmissione l'anagrafica del cliente
043100080213               // solo se l'ho aggiornata con l'IBAN.
043200080213               IF clpBab <> *BLANK;
043300080213                 EXEC SQL UPDATE QTEMP/CNACO_TEMP
043400080213                          SET ACOFTR = ''
043500080213                            , ACODTR = 0
043600080213                          WHERE ACOKUT = :clpKut
043700080213                            AND ACOKCC = :clpKcc
043800080213                            AND ACOKSC = :clpKsc
043900080213                 ;
044000080213                 IF sqlCode < 0;
044100080213                   DUMP(A);
044200080213                 ENDIF;
044300080213               ENDIF;
044400080213             ENDIF;
044500080213             IF sqlCode < 0;
044600080213               EXEC SQL ROLLBACK HOLD;
044700080213             ELSE;
044800080213               EXEC SQL COMMIT HOLD;
044900080213               aggiornati += 1;
045000080213             ENDIF;
045100080213           ENDIF;
045200080213
045300080213         ENDIF;
045400080213
045500080213       ENDDO;
045600080213
045700080213       EXCEPT totali;
045800080213
045900080213       IF aggiornare;
046000080213         EXEC SQL CLOSE CLIENTI_AGGIORNARE;
046100080213         EXEC SQL SET TRANSACTION ISOLATION LEVEL NO COMMIT;
046200080213         EXEC SQL LABEL ON COLUMN QTEMP/CNCLP_TEMP
046300080213                  (CLPBAB TEXT IS 'IBAN bonifico contrassegno.')
046400080213         ;
046500080213         EXEC SQL LABEL ON COLUMN QTEMP/CNCLP_TEMP
046600080213                  (CLPBAB IS 'IBAN bonifico       Contrassegno')
046700080213         ;
046800080213       ELSE;
046900080213         EXEC SQL CLOSE CLIENTI_CONTROLLO;
047000080213       ENDIF;
047100080213
047200080213      /END-FREE
047300080213     P aggiornamento   E
047400080213
