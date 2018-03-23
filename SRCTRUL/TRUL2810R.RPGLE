000100061027     H NOMAIN
000200061027
000300061027      //********************************************************************************************
000400061027      //  MODULO 10
000500061027      //
000600061027      // > - ABI (p.e. 6160-6)
000700061027      // > - CAB (p.e. 02400-0)
000800061027      //
000900061027      // ABI e CAB utilizzano lo stesso algoritmo: il codice ABI è di cinque
001000061027      // cifre come il CAB ed eventualmente bisogna inserire uno zero davanti
001100061027      // se il codice presentato è a quattro cifre. L'algoritmo è il seguente:
001200061027      //
001300061027      // - somma fra loro del doppio delle cifre dispari con eventuale riduzione
001400061027      // - somma fra loro delle cifre pari
001500061027      // - somma delle due somme
001600061027      // - resto della divisione intera per 10 della somma delle somme (modulo)
001700061027      // - la sottrazione da 10 del resto è il check digit
001800061027      //
001900061027      // Esempio ABI 06160-6:
002000061027      //
002100061027      // 0 * 2 + 1 * 2 + 0 * 2 = 2 (somma del doppio delle cifre dispari)
002200061027      // 6 + 6 = 12 (somma delle cifre pari)
002300061027      // 2 + 12 = 14 (somma delle somme)
002400061027      // 14 / 10 = 1 e resto 4 (modulo 10)
002500061027      // 10 - 4 = 6 (10 - resto = check digit)
002600061027      //
002700061027      // Per "eventuale riduzione" nella moltiplicazione delle cifre dispari
002800061027      // intendo ad esempio una cifra 9 che raddoppiata dà 18, numero che va
002900061027      // ridotto a una cifra sola sommando tra loro le due cifre che compongono
003000061027      // il numero stesso (1 + 8) e si può ottenere anche semplicemente
003100061027      // sottraendo 9 al numero:
003200061027      //
003300061027      // 9 * 2 = 18 --> 1 + 8 = 9 oppure 18 - 9 = 9
003400061027      // 8 * 2 = 16 --> 1 + 6 = 7 oppure 16 - 9 = 7
003500061027      // ...
003600061027      // 5 * 2 = 10 --> 1 + 0 = 1 oppure 10 - 9 = 1
003700061027      //********************************************************************************************
003800061027      // MODULO 13
003900061027      //
004000061027      // > - Numero assegno (p.e. 0336663219-09)
004100061027      //
004200061027      // Questo è semplicemente il resto della divisione intera per 13 :-)
004300061027      //
004400061027      // 336663219 / 13 = 25897170 e resto 9
004500061027      //********************************************************************************************
004600061027
004700061027     D*--------------------------------------------------
004800061027     D* Procedure name: getCheckDigitModulo10
004900061027     D* Purpose:        Calcola il check digit modulo 10
005000061027     D* Returns:        Check digit
005100061027     D* Parameter:      rqsCodice => Codice senza check digit.
005200061027     D* Parameter:      rqsLunghezzaCodice => Lunghezza del codice.
005300061027     D* Parameter:      rpyEsito => Esito
005400061027     D* Parameter:      rpyOpCode => Codice operativo
005500061027     D* Parameter:      rpyStatus
005600061027     D* Parameter:      rpyCheckDigit => Check digit
005700061027     D*--------------------------------------------------
005800061027     D getCheckDigitModulo10...
005900061027     D                 PR             1P 0
006000061027     D  rqsCodice                   255A   CONST
006100061027     D                                     OPTIONS(*VARSIZE)
006200061027     D  rqsLunghezzaCodice...
006300061027     D                                3U 0 CONST
006400061027     D  rpyEsito                     10I 0
006500061027     D  rpyOpCode                    10A
006600061027     D  rpyStatus                    10I 0
006700061027     D  rpyCheckDigit                 1P 0 OPTIONS(*NOPASS:*OMIT)
006800061027
006900061027     D*--------------------------------------------------
007000061027     D* Procedure name: getCheckDigitModulo13
007100061027     D* Purpose:        Calcola il check digit modulo 13.
007200061027     D* Returns:        Check digit
007300061027     D* Parameter:      rqsCodice => Codice senza check digit
007400061027     D* Parameter:      rpyEsito => Esito
007500061027     D* Parameter:      rpyOpCode => Codice operativo
007600061027     D* Parameter:      rpyStatus => Stato
007700061027     D* Parameter:      rpyCheckDigit => Check digit
007800061027     D*--------------------------------------------------
007900061027     D getCheckDigitModulo13...
008000061027     D                 PR             2P 0
008100061027     D  rqsCodice                    63P 0 VALUE
008200061027     D  rpyEsito                     10I 0
008300061027     D  rpyOpCode                    10A
008400061027     D  rpyStatus                    10I 0
008500061027     D  rpyCheckDigit                 2P 0 OPTIONS(*OMIT:*NOPASS)
008600061027
008700061027
008800061027     P*--------------------------------------------------
008900061027     P* Procedure name: getCheckDigitModulo10
009000061027     P* Purpose:        Calcola il check digit modulo 10
009100061027     P* Returns:        Check digit
009200061027     P* Parameter:      rqsCodice => Codice senza check digit.
009300061027     P* Parameter:      rqsLunghezzaCodice => Lunghezza del codice.
009400061027     P* Parameter:      rpyEsito => Esito
009500061027     P* Parameter:      rpyOpCode => Codice operativo
009600061027     P* Parameter:      rpyStatus
009700061027     P* Parameter:      rpyCheckDigit => Check digit
009800061027     P*--------------------------------------------------
009900061027     P getCheckDigitModulo10...
010000061027     P                 B                   EXPORT
010100061027     D getCheckDigitModulo10...
010200061027     D                 PI             1P 0
010300061027     D  rqsCodice                   255A   CONST
010400061027     D                                     OPTIONS(*VARSIZE)
010500061027     D  rqsLunghezzaCodice...
010600061027     D                                3U 0 CONST
010700061027     D  rpyEsito                     10I 0
010800061027     D  rpyOpCode                    10A
010900061027     D  rpyStatus                    10I 0
011000061027     D  rpyCheckDigit                 1P 0 OPTIONS(*NOPASS:*OMIT)
011100061027
011200061027     D p               S              5U 0
011300061027     D checkDigit      S              1P 0
011400061027     D sommaDispari    S             20U 0
011500061027     D sommaPari       S             20U 0
011600061027     D risultato       S             20U 0
011700061027
011800061027      /FREE
011900061027
012000061027       MONITOR;
012100061027
012200061027         CLEAR rpyEsito;
012300061027         CLEAR rpyOpCode;
012400061027         CLEAR rpyStatus;
012500061027
012600061027         IF rqsLunghezzaCodice = 0 OR
012700061027         %SUBST(rqsCodice:1:rqsLunghezzaCodice) = *BLANK;
012800061027           rpyEsito = -1;
012900061027           rpyOpCode = 'ERRORE';
013000061027           rpyStatus = 1;
013100061027           IF %PARMS > 5 AND %ADDR(rpyCheckDigit) <> *NULL;
013200061027             CLEAR rpyCheckDigit;
013300061027           ENDIF;
013400061027           RETURN 0;
013500061027         ENDIF;
013600061027
013700061027         // - somma fra loro del doppio delle cifre dispari con eventuale riduzione
013800061027         FOR p = 1 BY 2 TO rqsLunghezzaCodice;
013900061027           risultato = %DEC(%SUBST(rqsCodice:p:1):1:0) * 2;
014000061027           IF risultato > 9;
014100061027             risultato -= 9;
014200061027           ENDIF;
014300061027           sommaDispari += risultato;
014400061027         ENDFOR;
014500061027
014600061027         // - somma fra loro delle cifre pari
014700061027         FOR p = 2 BY 2 TO rqsLunghezzaCodice;
014800061027           sommaPari += %DEC(%SUBST(rqsCodice:p:1):1:0);
014900061027         ENDFOR;
015000061027
015100061027         // - somma delle due somme
015200061027         // - resto della divisione intera per 10 della somma delle somme (modulo)
015300061027         // - la sottrazione da 10 del resto è il check digit
015400061027         checkDigit = 10 - (%REM((sommaDispari + sommaPari) : 10));
015500061027
015600061027         IF %PARMS > 5 AND %ADDR(rpyCheckDigit) <> *NULL;
015700061027           rpyCheckDigit = checkDigit;
015800061027         ENDIF;
015900061027
016000061027         rpyOpCode = 'DONE';
016100061027         RETURN checkDigit;
016200061027
016300061027         ON-ERROR;
016400061027
016500061027           rpyEsito = -1;
016600061027           rpyOpCode = 'STATUS';
016700061027           rpyStatus = %STATUS;
016800061027           IF %PARMS > 5 AND %ADDR(rpyCheckDigit) <> *NULL;
016900061027             CLEAR rpyCheckDigit;
017000061027           ENDIF;
017100061027           RETURN 0;
017200061027
017300061027       ENDMON;
017400061027
017500061027      /END-FREE
017600061027     P getCheckDigitModulo10...
017700061027     P                 E
017800061027
017900061027     P*--------------------------------------------------
018000061027     P* Procedure name: getCheckDigitModulo13
018100061027     P* Purpose:        Calcola il check digit modulo 13.
018200061027     P* Returns:        Check digit
018300061027     P* Parameter:      rqsCodice => Codice senza check digit
018400061027     P* Parameter:      rpyEsito => Esito
018500061027     P* Parameter:      rpyOpCode => Codice operativo
018600061027     P* Parameter:      rpyStatus => Stato
018700061027     P* Parameter:      rpyCheckDigit => Check digit
018800061027     P*--------------------------------------------------
018900061027     P getCheckDigitModulo13...
019000061027     P                 B                   EXPORT
019100061027     D getCheckDigitModulo13...
019200061027     D                 PI             2P 0
019300061027     D  rqsCodice                    63P 0 VALUE
019400061027     D  rpyEsito                     10I 0
019500061027     D  rpyOpCode                    10A
019600061027     D  rpyStatus                    10I 0
019700061027     D  rpyCheckDigit                 2P 0 OPTIONS(*OMIT:*NOPASS)
019800061027
019900061027     D checkDigit      S              2P 0
020000061027
020100061027      /FREE
020200061027
020300061027       CLEAR rpyEsito;
020400061027       CLEAR rpyOpCode;
020500061027       CLEAR rpyStatus;
020600061027
020700061027       checkDigit = %REM(%ABS(rqsCodice):13);
020800061027
020900061027       IF %PARMS > 5 AND %ADDR(rpyCheckDigit) <> *NULL;
021000061027         rpyCheckDigit = checkDigit;
021100061027       ENDIF;
021200061027
021300061027       rpyOpCode = 'DONE';
021400061027       RETURN checkDigit;
021500061027
021600061027      /END-FREE
021700061027     P getCheckDigitModulo13...
021800061027     P                 E
