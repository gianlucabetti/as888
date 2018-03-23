000100070131       //********************************************************************************************
000200070131       // http://www.poste.it/postali/cap/indirizzo.shtml
000300070131       //
000400070131       // L'indirizzo deve contenere da tre a cinque righe.
000500070131       // Solo se si riferisce a uno Stato estero può essere composto da sei righe.
000600070131       //
000700070131       // Composizione dell'indirizzo
000800070131       //
000900070131       // RI OB
001000070131       // GA BL ELEMENTI DELL'INDIRIZZO
001100070131       //  1 Sì Destinatario.
001200070131       //  2 No Informazioni aggiuntive sul destinatario.
001300070131       //  3 No Informazioni aggiuntive sull'edifico (scala, piano, interno).
001400070131       //  4 Sì Via, nome della via, numero civico (in alternativa casella postale).
001500070131       //  5 Sì CAP, località e sigla provincia.
001600070201       //  6 Sì Stato estero (con esclusione di San Marino e Città del Vaticano).
001700070131       //
001800070201       // Quando scrivi l'indirizzo ricorda di:
001900070201       //
002000070201       // 1. riportare sempre le righe obbligatorie e rispettare l'ordine delle righe
002100070201       // 2. scrivere in stampatello e in caratteri maiuscoli
002200070201       // 3. evitare punteggiatura, sottolineature e caratteri speciali
002300070201       // 4. indicare il numero civico dopo il nome della via, mai prima
002400070201       // 5. controllare che CAP, Località e sigla della Provincia siano sempre coerenti
002500070201       // 6. per le città suddivise in zone postali non usare il CAP generico:
002600070201       //    scrivi sempre il CAP specifico associato alla via e al numero civico del destinatario
002700070201       //
002800070131       //********************************************************************************************
002900070131
003000070201     H DFTACTGRP(*NO) ACTGRP(*CALLER)
003100070201
003200070201       //********************************************************************************************
003300070201       //
003400070201       // Definizione files.
003500070201       //
003600070201       //********************************************************************************************
003700070201     Ftabel00f  IF   E           K DISK    USROPN
003800070131
003900070131       //********************************************************************************************
004000070131       //
004100070131       // Definizione costanti.
004200070131       //
004300070131       //********************************************************************************************
004400070131     D ITALIA...
004500070131     D                 C                   'IT'
004600070131     D SAN_MARINO...
004700070131     D                 C                   'SM'
004800070131     D CITTA_DEL_VATICANO...
004900070131     D                 C                   'VA'
005000070131     D CCSID_FORMAT...
005100070131     D                 C                   1
005200070131     D CCSID_OF_THE_JOB...
005300070131     D                 C                   0
005400070131     D CONVERT_TO_UPPERCASE...
005500070131     D                 C                   0
005600070131
005700070131       //********************************************************************************************
005800070131       //
005900070131       // Definizione procedure.
006000070131       //
006100070131       //********************************************************************************************
006200070131     D convertCase...
006300070201     D                 PR                  EXTPGM('QLGCNVCS')
006400070131     D  requestControlBlock...
006500070131     D                                     LIKEDS(requestControlBlock)
006600070131     D  inputData...
006700070201     D                               50A
006800070131     D  outputData...
006900070131     D                               50A
007000070131     D  lengthOfData...
007100070201     D                               10I 0
007200070131     D  errorCode...
007300070131     D                                     LIKE(qusec)
007400070131
007500070131       //********************************************************************************************
007600070131       //
007700070131       // Definizione strutture dati.
007800070131       //
007900070131       //********************************************************************************************
008000070131     D trulIndiS     E DS                  QUALIFIED
008100070201     D ds15          E DS                  QUALIFIED
008200070131     D requestControlBlock...
008300070201     D                 DS                  QUALIFIED
008400070131     D  typeOfRequest...
008500070131     D                               10I 0 INZ(CCSID_FORMAT)
008600070131     D  CCSIDOfInputData...
008700070131     D                               10I 0 INZ(CCSID_OF_THE_JOB)
008800070131     D  caseRequest...
008900070131     D                               10I 0 INZ(CONVERT_TO_UPPERCASE)
009000070131     D  reserved...
009100070131     D                               10A   INZ(*ALLX'00')
009200070131
009300070131       //********************************************************************************************
009400070131       //
009500070131       // Definizione variabili.
009600070131       //
009700070131       //********************************************************************************************
009800070131      /COPY QSYSINC/QRPGLESRC,QUSEC
009900070131     D i               S              3U 0
010000070131     D iLast           S              3U 0
010100070131     D indi            S                   DIM(6) LIKE(trulIndiS.indiRiga1)
010200070201     D lengthOfData...
010300070201     D                 S             10I 0 INZ(%SIZE(indi))
010400070131
010500070131       //********************************************************************************************
010600070131       //
010700070131       // Definizione parametri.
010800070131       //
010900070131       //********************************************************************************************
011000070131     C     *ENTRY        PLIST
011100070131     C                   PARM                    trulIndiS
011200070131      /FREE
011300070131
011400070131       EXSR chkParm;
011500070131       EXSR setIndirizzo;
011600070131       EXSR uscita;
011700070131
011800070131       //********************************************************************************************
011900070131       //
012000070131       // Inizializzazione programma.
012100070131       //
012200070131       //********************************************************************************************
012300070131       BEGSR *INZSR;
012400070131
012500070131         CLEAR qusec;
012600070131         qusbprv = %SIZE(qusec);
012700070131
012800070131       ENDSR;
012900070131
013000070131       //********************************************************************************************
013100070131       //
013200070131       // Controllo parametri ricevuti.
013300070131       //
013400070131       //********************************************************************************************
013500070131       BEGSR chkParm;
013600070131
013700070131         CLEAR trulIndiS.rpyEsito;
013800070131         CLEAR trulIndiS.rpyOpCode;
013900070131         CLEAR trulIndiS.rpyStatus;
014000070131         CLEAR trulIndiS.indiRiga1;
014100070131         CLEAR trulIndiS.indiRiga2;
014200070131         CLEAR trulIndiS.indiRiga3;
014300070131         CLEAR trulIndiS.indiRiga4;
014400070131         CLEAR trulIndiS.indiRiga5;
014500070131         CLEAR trulIndiS.indiRiga6;
014600070131
014700070131       ENDSR;
014800070131
014900070131       //********************************************************************************************
015000070131       //
015100070131       // Impostazione indirizzo.
015200070131       //
015300070131       //********************************************************************************************
015400070131       BEGSR setIndirizzo;
015500070131
015600070131         RESET i;
015700070131         RESET indi;
015800070131
015900070131         // Titolo.
016000070131         IF trulIndiS.titolo <> *BLANK;
016100070131           i += 1;
016200070131           indi(i) = %TRIML(trulIndiS.titolo);
016300070131         ENDIF;
016400070131
016500070131         // Destinatario.
016600070131         i += 1;
016700070201         indi(i) = %TRIML(trulIndiS.destinatar);
016800070201         EXSR cvtToUpperCase;
016900070131
017000070131         // Informazioni aggiuntive destinatario.
017100070131         IF trulIndiS.destInfAgg <> *BLANK;
017200070131           i += 1;
017300070201           indi(i) = %TRIML(trulIndiS.destInfAgg);
017400070201           EXSR cvtToUpperCase;
017500070131         ENDIF;
017600070131
017700070131         // Informazioni aggiuntive edificio.
017800070131         IF trulIndiS.edificio <> *BLANK;
017900070131           i += 1;
018000070201           indi(i) = %TRIML(trulIndiS.edificio);
018100070201           EXSR cvtToUpperCase;
018200070131         ENDIF;
018300070131
018400070131         // Via, nome via e numero civico.
018500070131         i += 1;
018600070201         indi(i) = %TRIML(trulIndiS.via);
018700070201         EXSR cvtToUpperCase;
018800070131
018900070131         // CAP, località e provincia.
019000070131         i += 1;
019100070201         indi(i) = %TRIM(trulIndiS.cap) + ' ' + %TRIM(trulIndiS.localita) +
019200070201         ' ' + trulIndiS.provincia;
019300070201         EXSR cvtToUpperCase;
019400070131
019500070131         // Stato estero.
019600070131         IF trulIndiS.statoEster <> *BLANK AND trulIndiS.statoEster <> ITALIA
019700070131         AND trulIndiS.statoEster <> SAN_MARINO
019800070131         AND trulIndiS.statoEster <> CITTA_DEL_VATICANO;
019900070201           IF NOT %OPEN(tabel00f);
020000070201             OPEN tabel00f;
020100070201           ENDIF;
020200070201           CHAIN (1 : '15' : trulIndiS.statoEster) tabel00f;
020300070201           IF %FOUND;
020400070201             ds15 = tblUni;
020500070131             i += 1;
020600070201             indi(i) = %TRIML(ds15.§15Des);
020700070201             EXSR cvtToUpperCase;
020800070131           ENDIF;
020900070131         ENDIF;
021000070131
021100070131         trulIndiS.indiRiga1 = indi(1);
021200070131         trulIndiS.indiRiga2 = indi(2);
021300070131         trulIndiS.indiRiga3 = indi(3);
021400070131         trulIndiS.indiRiga4 = indi(4);
021500070131         trulIndiS.indiRiga5 = indi(5);
021600070131         trulIndiS.indiRiga6 = indi(6);
021700070131
021800070131       ENDSR;
021900070201
022000070201       //********************************************************************************************
022100070201       //
022200070201       // Conversione in maiuscolo (se richiesto).
022300070201       //
022400070201       //********************************************************************************************
022500070201       BEGSR cvtToUpperCase;
022600070201
022700070201         IF trulIndiS.cvtUCase = *ON;
022800070201           RESET qusec;
022900070201           convertCase( requestControlBlock
023000070201                      : indi(i)
023100070201                      : indi(i)
023200070201                      : lengthOfData
023300070201                      : qusec);
023400070201         ENDIF;
023500070201
023600070201       ENDSR;
023700070131
023800070131       //********************************************************************************************
023900070131       //
024000070131       // Inizializzazione programma.
024100070131       //
024200070131       //********************************************************************************************
024300070131       BEGSR uscita;
024400070131
024500070131         IF trulIndiS.rpyOpCode = *BLANK;
024600070131           trulIndiS.rpyOpCode = 'DONE';
024700070131         ENDIF;
024800070131
024900070131         RETURN;
025000070131
025100070131       ENDSR;
025200070131      /END-FREE
