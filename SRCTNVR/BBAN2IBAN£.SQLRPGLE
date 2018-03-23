000100080204      //************************************************************************
000200080204      //
000300080204      // Questo programma calcola l'IBAN dal BBAN per i clienti.
000400080204      //
000500080204      //************************************************************************
000600080205     H DFTACTGRP(*NO) ACTGRP('BBAN2IBAN')
000700080204
000800080204      //************************************************************************
000900080204      //
001000080204      // Dichiarazione files.
001100080204      //
001200080204      //************************************************************************
001300080204     Fprtf198   O    F  198        PRINTER OFLIND(*INOA)
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
004600080204
004700080204      //************************************************************************
004800080204      //
004900080204      // Dichiarazione strutture dati.
005000080204      //
005100080204      //************************************************************************
005200080204     D cnDizion      E DS                  BASED(nullPtr) QUALIFIED
005300080204     D cbaIbanDs     E DS                  BASED(nullPtr) QUALIFIED
005400080206     D x59AbiDs      E DS                  INZ QUALIFIED
005500080204
005600080204      //************************************************************************
005700080204      //
005800080204      // Dichiarazione variabili.
005900080204      //
006000080204      //************************************************************************
006100080204     D aggiornare      S               N
006200080204     D nullPtr         S               *
006300080204     D clpKut          S                   LIKE(cnDizion.$Kut)
006400080204     D clpKcc          S                   LIKE(cnDizion.$Kcc)
006500080204     D clpKsc          S                   LIKE(cnDizion.$Ksc)
006600080204     D clpBab          S                   LIKE(cnDizion.$Bab)
006700080204     D clpCcb          S                   LIKE(cnDizion.$Ccb)
006800080204     D clpAbi          S                   LIKE(cnDizion.$Abi)
006900080204     D clpCab          S                   LIKE(cnDizion.$Cab)
007000080204     D statoIso        S                   LIKE(cbaIbanDs.cbaStato)
007100080204     D checkDigitIban...
007200080204     D                 S                   LIKE(cbaIbanDs.cbaCheck)
007300080204     D bban            S                   LIKE(cbaIbanDs.cbaCoori)
007400080205     D esito           S              1A
007500080204     D opzione         S              2A
007600080204     D elaborati       S             10U 0
007700080204     D validi          S             10U 0
007800080204     D calcolati       S             10U 0
007900080204     D nonCalcolati    S             10U 0
008000080204     D aggiornati      S             10U 0
008100080205     D puliti          S             10U 0
008200080204
008300080204      //************************************************************************
008400080204      //
008500080204      // Dichiarazione parametri.
008600080204      //
008700080204      //************************************************************************
008800080204     C     *ENTRY        PLIST
008900080204     C                   PARM                    aggiornare
009000080204
009100080205      /FREE
009200080205
009300080205       //***********************************************************************
009400080205       //
009500080205       // Main.
009600080205       //
009700080205       //***********************************************************************
009800080204
009900080204       *INLR = *ON;
010000080204
010100080204       // Non ho ricevuto parametri.
010200080204       IF %PARMS() < 1;
010300080204         DUMP(A);
010400080204         RETURN;
010500080204       ENDIF;
010600080204
010700080205       //***********************************************************************
010800080205       //
010900080205       // Pulizia dei campi numero conto e descrizione sportello su CNIND00F.
011000080205       //
011100080205       //***********************************************************************
011200080205
011300080204       IF aggiornare;
011400080204         EXEC SQL UPDATE CNIND00F
011500080204                  SET INDBNA = ''
011600080204                    , INDCCB = ''
011700080204                  WHERE INDKUT = 1
011800080204                    AND INDKCC = 151
011900080204                    AND (INDBNA <> '' OR INDCCB <> '')
012000080204         ;
012100080204         IF sqlCode < 0;
012200080204           DUMP(A);
012300080204         ENDIF;
012400080204       ENDIF;
012500080204
012600080205       //***********************************************************************
012700080205       //
012800080205       // Calcolo IBAN da BBAN o pulizia descrizione sportello su CNCLP00F.
012900080205       //
013000080205       //***********************************************************************
013100080205
013200080204       IF aggiornare;
013300080205         EXEC SQL SET TRANSACTION ISOLATION LEVEL
013400080205                  READ UNCOMMITTED, READ WRITE
013500080205         ;
013600080205         EXEC SQL DECLARE CLIENTI_AGGIORNARE CURSOR FOR
013700080204                  SELECT CLPKUT, CLPKCC, CLPKSC, CLPBAB, CLPCCB, CLPABI, CLPCAB
013800080204                  FROM CNCLP00F
013900080205                  WHERE CLPKUT = 1
014000080205                    AND CLPKCC = 151
014100080205                    AND (CLPBAB <> ''
014200080205                         OR (CLPCCB <> ''
014300080205                             AND CLPABI > 0
014400080205                             AND CLPCAB > 0
014500080205                            )
014600080205                        )
014700080204                  ORDER BY CLPKUT, CLPKCC, CLPKSC
014800080204                  FOR UPDATE OF CLPBAB
014900080204         ;
015000080204       ELSE;
015100080204         EXEC SQL DECLARE CLIENTI_CONTROLLO CURSOR FOR
015200080204                  SELECT CLPKUT, CLPKCC, CLPKSC, CLPBAB, CLPCCB, CLPABI, CLPCAB
015300080204                  FROM CNCLP00F
015400080205                  WHERE CLPKUT = 1
015500080205                    AND CLPKCC = 151
015600080205                    AND (CLPBAB <> ''
015700080205                         OR (CLPCCB <> ''
015800080205                             AND CLPABI > 0
015900080205                             AND CLPCAB > 0
016000080205                            )
016100080205                        )
016200080204                  ORDER BY CLPKUT, CLPKCC, CLPKSC
016300080204                  FOR FETCH ONLY
016400080204         ;
016500080204       ENDIF;
016600080204
016700080204       IF sqlCode < 0;
016800080204         DUMP(A);
016900080204         RETURN;
017000080204       ENDIF;
017100080204
017200080204       IF aggiornare;
017300080204         EXEC SQL OPEN CLIENTI_AGGIORNARE;
017400080204       ELSE;
017500080204         EXEC SQL OPEN CLIENTI_CONTROLLO;
017600080204       ENDIF;
017700080204
017800080204       IF sqlCode < 0;
017900080204         DUMP(A);
018000080204         RETURN;
018100080204       ENDIF;
018200080204
018300080207       DOU sqlCode = SQL_RIGA_NONTROVATA;
018400080204
018500080204         IF aggiornare;
018600080205           EXEC SQL FETCH NEXT FROM CLIENTI_AGGIORNARE
018700080204                    INTO :clpKut, :clpKcc, :clpKsc, :clpBab, :clpCcb, :clpAbi
018800080204                       , :clpCab
018900080204           ;
019000080204         ELSE;
019100080204           EXEC SQL FETCH NEXT FROM CLIENTI_CONTROLLO
019200080204                    INTO :clpKut, :clpKcc, :clpKsc, :clpBab, :clpCcb, :clpAbi
019300080204                       , :clpCab
019400080204           ;
019500080204         ENDIF;
019600080204
019700080204         IF sqlCode < 0;
019800080204           DUMP(A);
019900080204           LEAVE;
020000080207         ELSEIF sqlCode = SQL_RIGA_NONTROVATA;
020100080204           LEAVE;
020200080204         ENDIF;
020300080204
020400080204         elaborati += 1;
020500080204
020600080204         // Verifico se la colonna CLPBAB contiene già un IBAN.
020700080205         IF clpBab = *BLANK;
020800080205           esito = PROJ_IBAN_NONVALIDO;
020900080205         ELSE;
021000080205           statoIso = %SUBST(clpBab : 1);
021100080205           checkDigitIban = %SUBST(clpBab : 3);
021200080205           bban = %SUBST(clpBab : 5);
021300080205           CLEAR esito;
021400080207           opzione = PROJ_IBAN_CONTROLLO;
021500080205           Proj_IBAN( statoIso : checkDigitIban : bban : esito : opzione );
021600080205         ENDIF;
021700080204
021800080205         IF esito = PROJ_IBAN_VALIDO;
021900080205
022000080204           validi += 1;
022100080204           EXCEPT ibanValido;
022200080205
022300080205         ELSE;
022400080205
022500080205           CLEAR clpBab;
022600080205
022700080205           // Se l'IBAN non è valido (o non c'è) provo a desumerlo dal BBAN
022800080205           // italiano ma solo se ho tutti i dati.
022900080205           IF clpAbi > 0 AND clpCab > 0 AND %SUBST(clpCcb : 16 : 1) <> ''
023000080205           AND %LEN(%TRIMR(%SUBST(clpCcb : 1 : 15))) = 12;
023100080206             RESET x59AbiDs;
023200080206             x59AbiDs.x59Abi = %EDITC(clpAbi : 'X');
023300080206             x59AbiDs.x59Cab = %EDITC(clpCab : 'X');
023400080206             Proj_sportelloBancario(x59AbiDs);
023500080207             IF x59AbiDs.x59Err = PROJ_SPORTELLOBANCARIO_TROVATO;
023600080206               statoIso = x59AbiDs.x59Stato;
023700080206               IF x59AbiDs.x59Stato = ITALIA AND x59AbiDs.x59Prov = SANMARINO;
023800080206                 statoIso = SANMARINO;
023900080206               ENDIF;
024000080206             ELSE;
024100080206               statoIso = ITALIA;
024200080206             ENDIF;
024300080206             IF statoIso = ITALIA OR statoIso = SANMARINO;
024400080206               CLEAR checkDigitIban;
024500080206               bban = %SUBST(clpCcb : 16 : 1) + %EDITC(clpAbi : 'X')
024600080206               + %EDITC(clpCab : 'X') + %SUBST(clpCcb : 1 : 15);
024700080206               CLEAR esito;
024800080207               opzione = PROJ_IBAN_CALCOLO;
024900080206               Proj_IBAN( statoIso : checkDigitIban : bban : esito : opzione );
025000080206             ELSE;
025100080206               esito = PROJ_IBAN_NONVALIDO;
025200080206             ENDIF;
025300080205             IF esito = PROJ_IBAN_VALIDO;
025400080205               clpBab = statoIso + checkDigitIban + bban;
025500080205               calcolati += 1;
025600080205               EXCEPT ibanCalcol;
025700080205             ELSE;
025800080205               nonCalcolati += 1;
025900080205               EXCEPT ibanNonCal;
026000080205             ENDIF;
026100080205           ELSE;
026200080205             puliti += 1;
026300080205             EXCEPT ibanPulito;
026400080205           ENDIF;
026500080205
026600080205           IF aggiornare;
026700080205             // Aggiorno sempre l'anagrafica del cliente con l'IBAN calcolato o pulito.
026800080205             EXEC SQL UPDATE CNCLP00F
026900080205                      SET CLPBAB = :clpBab
027000080205                      WHERE CURRENT OF CLIENTI_AGGIORNARE
027100080205             ;
027200080205             IF sqlCode < 0;
027300080205               DUMP(A);
027400080205             ELSE;
027500080205               // Metto in trasmissione l'anagrafica del cliente
027600080205               // solo se l'ho aggiornata con l'IBAN.
027700080205               IF clpBab <> *BLANK;
027800080205                 EXEC SQL UPDATE CNACO00F
027900080205                          SET ACOFTR = ''
028000080205                            , ACODTR = 0
028100080205                          WHERE ACOKUT = :clpKut
028200080205                            AND ACOKCC = :clpKcc
028300080205                            AND ACOKSC = :clpKsc
028400080205                 ;
028500080205                 IF sqlCode < 0;
028600080205                   DUMP(A);
028700080205                 ENDIF;
028800080205               ENDIF;
028900080205             ENDIF;
029000080205             IF sqlCode < 0;
029100080205               EXEC SQL ROLLBACK HOLD;
029200080205             ELSE;
029300080205               EXEC SQL COMMIT HOLD;
029400080205               aggiornati += 1;
029500080205             ENDIF;
029600080205           ENDIF;
029700080205
029800080204         ENDIF;
029900080204
030000080204       ENDDO;
030100080204
030200080204       EXCEPT totali;
030300080204
030400080204       IF aggiornare;
030500080204         EXEC SQL CLOSE CLIENTI_AGGIORNARE;
030600080207         EXEC SQL SET TRANSACTION ISOLATION LEVEL NO COMMIT;
030700080207         EXEC SQL LABEL ON COLUMN CNCLP00F
030800080207                  (CLPBAB TEXT IS 'IBAN bonifico contrassegno.')
030900080207         ;
031000080207         EXEC SQL LABEL ON COLUMN CNCLP00F
031100080207                  (CLPBAB IS 'IBAN bonifico       Contrassegno')
031200080207         ;
031300080207       ELSE;
031400080204         EXEC SQL CLOSE CLIENTI_CONTROLLO;
031500080204       ENDIF;
031600080204
031700080204      /END-FREE
031800080204
031900080204     Oprtf198   E            ibanValido  1
032000080204     O                       clpKsc
032100080204     O                       clpBab              +1
032200080205     O                       clpCcb              +1
032300080205     O                       clpAbi              +1
032400080205     O                       clpCab              +1
032500080205     O                                           +1 'Valido'
032600080205     O          E            ibanPulito  1
032700080205     O                       clpKsc
032800080205     O                       clpBab              +1
032900080205     O                       clpCcb              +1
033000080205     O                       clpAbi              +1
033100080205     O                       clpCab              +1
033200080205     O                                           +1 'Pulito'
033300080204     O          E            ibanCalcol  1
033400080204     O                       clpKsc
033500080204     O                       clpBab              +1
033600080204     O                       clpCcb              +1
033700080204     O                       clpAbi              +1
033800080204     O                       clpCab              +1
033900080205     O                                           +1 'Calcolato'
034000080205     O          E            ibanNonCal  1
034100080205     O                       clpKsc
034200080205     O                       clpBab              +1
034300080205     O                       clpCcb              +1
034400080205     O                       clpAbi              +1
034500080205     O                       clpCab              +1
034600080205     O                                           +1 'Non calcolabile'
034700080204     O          E            totali      2
034800080204     O                                              'Elaborati:'
034900080204     O                       elaborati     Z
035000080204     O                                           +1 'Validi:'
035100080204     O                       validi        Z
035200080205     O                                           +1 'Puliti:'
035300080205     O                       puliti        Z
035400080204     O                                           +1 'Calcolati:'
035500080204     O                       calcolati     Z
035600080205     O                                           +1 'Non calcolabili:'
035700080204     O                       nonCalcolati  Z
035800080204     O                                           +1 'Aggiornati:'
035900080204     O                       aggiornati    Z
