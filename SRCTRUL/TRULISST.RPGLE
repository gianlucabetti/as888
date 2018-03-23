000100080627     ***************************************************************************
000200080627     **
000300081205     **  Questo modulo divide una stringa in n sottostringhe ?
000400081205     **  senza spezzare le parole.                           ?
000500080627     **
000600080627     ***************************************************************************
000700080627     H NOMAIN
000800080627
000900080627     ***************************************************************************
001000080627     **
001100080704     **? Definizione prototipi procedure. ?
001200080627     **
001300080627     ***************************************************************************
001400080627     D/COPY GAITRASRC/SRCPROTOPR,TRULISST
001500080627     ***************************************************************************
001600080627     **
001700080704     **? Definizione costanti. ?
001800080627     **
001900080627     ***************************************************************************
002000080627     D ESITO_OK...
002100080627     D                 C                   0
002200080627     D ESITO_WARNING...
002300080627     D                 C                   1
002400080627     D ESITO_ERROR...
002500080627     D                 C                   -1
002600080627     ***************************************************************************
002700080627     **
002800080704     **? Definizione variabili globali. ?
002900080627     **
003000080627     ***************************************************************************
003100080627     D stringa         S          65535A   VARYING
003200080627     D stringaLen      S             10I 0
003300080714     D PosizioneIniziale...
003400080627     D                 S             10I 0
003500080708     D  UltimoByte     s              5U 0
003600080627     P*--------------------------------------------------
003700080704     P*
003800080704     P*?Procedure name: ISST_init?
003900080627     P* Purpose:        Inizializza il modulo.
004000080627     P* Returns:        Esito.
004100080627     P*--------------------------------------------------
004200080627     P ISST_init       B                   EXPORT
004300080627     D ISST_init       PI            10I 0
004400080627
004500080627      /FREE
004600080627
004700080627       RETURN ESITO_OK;
004800080627
004900080627      /END-FREE
005000080627     P ISST_init       E
005100080627
005200080627     P*--------------------------------------------------
005300080704     P*?Procedure name: ISST_finalize?
005400080627     P* Purpose:        Chiude il modulo.
005500080627     P* Returns:
005600080627     P*--------------------------------------------------
005700080627     P ISST_finalize   B                   EXPORT
005800080627     D ISST_finalize   PI
005900080627
006000080627      /FREE
006100080627
006200080627      /END-FREE
006300080627     P ISST_finalize   E
006400080627
006500080627     P*--------------------------------------------------
006600080704     P*?Procedure name: ISST_newStringa?
006700080627     P* Purpose:        Riceve la stringa da sottostringare.
006800080627     P* Returns:        Esito.
006900080627     P* Parameter:      piStringa => Stringa da sottostringare
007000080627     P* Parameter:      piStringaLen => Lunghezza stringa.
007100080627     P*--------------------------------------------------
007200080627     PISST_newStringa  B                   EXPORT
007300080627     DISST_newStringa  PI            10I 0
007400080627     D  piStringa                 65535A   OPTIONS(*VARSIZE)
007500080627     D  piStringaLen                 10I 0 CONST
007600080709
007700080627      /FREE
007800080627
007900080704       IF piStringaLen < 1;   // se la lunghezza è 0 o minore NON deve esserlo
008000080704         RETURN ESITO_ERROR;  // restituisce l'errore
008100080627       ENDIF;
008200080627
008300080707       stringa = %SUBST(piStringa : 1 : piStringaLen); // imposta la stringa x la sua lunghezza
008400080627
008500080707       IF piStringaLen > %SIZE(stringa);  //?se la lunghezza fosse superiore della stringa stessa
008600080707            stringaLen = %SIZE(stringa);  //?imposta la lunghezza calcolata
008700080707       ELSE;
008800080707            stringaLen = piStringaLen;    //?altrimenti va bene la lunghezza passata
008900080627       ENDIF;
009000080627
009100080704       IF %SUBST(piStringa : 1 : piStringaLen) = *BLANK; // se la stringa è vuota avverte il chiamante
009200080627         RETURN ESITO_WARNING;
009300080627       ENDIF;
009400080627
009500080708         //?resetta il contatore x inziare un nuovo ciclo di spezzettatura.
009600080714       RESET PosizioneIniziale;
009700080708       RESET UltimoByte;
009800080708
009900080627       RETURN ESITO_OK;
010000080627
010100080627      /END-FREE
010200080627     PISST_newStringa  E
010300080627
010400080627
010500080627     P*--------------------------------------------------
010600080704     P*?Procedure name: ISST_getSottoStringa?
010700080627     P* Purpose:        Restituisce la sottostringa.
010800080627     P* Returns:        Esito.
010900080627     P* Parameter:      piSottoStringaLen => Lunghezza sottostringa.
011000080627     P* Parameter:      piPosizioneProgressiva => Posizione progressiva all...
011100080627     P*                          'interno della stringa.
011200080627     P*--------------------------------------------------
011300080627     P ISST_getSottoStringa...
011400080627     P                 B                   EXPORT
011500080627     D ISST_getSottoStringa...
011600080627     D                 PI         65535A
011700080627     D  piSottoStringaLen...
011800080627     D                               10I 0 CONST
011900080714     D  piPosizioneProgressiva...
012000080714     D                               10I 0
012100080627
012200080709     D  StringaBlankFinale...
012300080709     D                 s              1A
012301081209     D i               S              5U 0
012400080709
012500080627      /FREE
012600080704        //?bisogna calcolare la parte di sottostringa che restituisca fino  ?
012700080707        //?all'ultima parola intera senza spezzarla partendo sempre con  ?
012800080704        //?il primo carattere non blank da cui iniziare a calcolare una  ?
012900080704        //?nuova porzione di stringa.  ?
013000080709
013100080709        clear StringaBlankFinale;
013200080704
013300080708        //?parte dal byte successivo all'ultimo passato se non era già arrivato in Fondo
013400080714       IF  UltimoByte < stringaLen;
013500080708
013600080714          PosizioneIniziale = UltimoByte + 1;
013700080708       Else;
013800080708        //?altrimenti esce passando la sottostringa a blank
013900080709            //?imposta la stringa vuota per chiudere la spezzettatura
014000080714          piPosizioneProgressiva = stringaLen;
014100080709          RETURN StringaBlankFinale;
014200080708       EndIF;
014300080707
014400080708
014500080708        //?Se il primo byte è blank deve cercare il successivo NON blank da cui partire
014600080714       DoW  %subst(stringa : PosizioneIniziale : 1) = *blank;
014700080709
014800080709        //?Arrivato al limite estremo della stringa
014900080709        //?esce passando la sottostringa a blank
015000080714          IF PosizioneIniziale >= StringaLen;
015100080709
015200080709            //?imposta la stringa vuota per chiudere la spezzettatura
015300080714            piPosizioneProgressiva = stringaLen;
015400080709            RETURN StringaBlankFinale;
015500080709
015600080709          Else;
015700080714            PosizioneIniziale = PosizioneIniziale + 1; //?Aggiunge 1 byte e riprova.
015800080709          EndIF;
015900080709
016000080707       endDo;
016100080708
016200080708        //?A questo punto, abbiamo la posizione di partenza in PosizioneProgressiva
016300080708        //? quindi si deve calcolare la sottostringa da restituire.
016400080707
016500080708
016600080708        //?calcola l'Ultimo Byte della sottostringa per verificare
016700080708        //? se l'ultimo carattere dell'ultima parola della sottostringa è valido
016800080714       If (PosizioneIniziale + piSottoStringaLen - 1) >  stringaLen;
016900080714
017000080714           UltimoByte = stringaLen;
017100080708       else;
017200080714           UltimoByte = PosizioneIniziale + piSottoStringaLen - 1;
017300080714
017400080708       endIF;
017500080708
017600080704
017700080708        //?La regola è che NON si devono SPEZZARE le parole.
017800080708        //?quindi se l'ultimo Byte della sottostringa è <> da Blank deve verificare
017900080708        //?se la parola continua oppure NO.
018000080708        //?Deve andare alla parola precedente escludendo quella spezzata da impostare
018100080708        //?nella sottostringa successiva.
018200080714       If UltimoByte =     stringaLen                or
018300080708          %subst(stringa : UltimoByte : 1) = *blank  or
018400080708          %subst(stringa : UltimoByte + 1 : 1) = *blank;
018500080708
018600080714
018700080714        //?Solo se era blank l'ultimo carattere della sottostringa.
018800080714        //?poi andando a ritroso arriva al primo carattere valido dell'ultima
018900080714        //?parola intera.
019000080714          DoW  %Subst(stringa : UltimoByte : 1) = ' ';
019100080714               UltimoByte = UltimoByte - 1;
019200080714          endDO;
019300080714
019400080714
019500080714           piPosizioneProgressiva = UltimoByte;
019600080714           RETURN %subst(stringa : PosizioneIniziale :
019700080714                     UltimoByte - PosizioneIniziale + 1);
019800080708       Endif;
019900080708
020000080708        //?se non è uscito, allora deve cercare dove finisce la parola precedente
020100080708        //?l'ultima parola spezzata e uscire impostando l'UltimoByte valido trovato.
020200081209       //UltimoByte = UltimoByte - 1;
020300080708
020400081209       //?ha eliminato l'ultima parola non completa.
020500081209       //DoW  UltimoByte > posizioneIniziale
020600081209       //AND %Subst(stringa : UltimoByte : 1) <> *blank;
020700081209       //    UltimoByte = UltimoByte - 1;
020800081209       //endDO;
020900080708
021000081209       FOR i = ultimoByte DOWNTO posizioneIniziale;
021100081209         IF %SUBST(stringa : i : 1) = *BLANK;
021200081209           ultimoByte = i;
021300081209           LEAVE;
021400081209         ENDIF;
021500081209       ENDFOR;
021600080714
021700080714        //?poi andando a ritroso arriva al primo carattere valido dell'ultima
021800080714        //?parola intera.
021900081209       //DoW  UltimoByte > posizioneIniziale
022000081209       //AND %Subst(stringa : UltimoByte : 1) = ' ';
022100081209       //    UltimoByte = UltimoByte - 1;
022200081209       //endDO;
022300081209
022400081209       FOR i = ultimoByte DOWNTO posizioneIniziale;
022500081209         IF %SUBST(stringa : i : 1) <> *BLANK;
022600081209           ultimoByte = i;
022700081209           LEAVE;
022800081209         ENDIF;
022900081209       ENDFOR;
023000080714
023100080721        //?Bisogna controllare che non stia estraendo una sottostringa andata
023200080721        //?in negativo poichè la parola è + grande della grandezza della
023300080721        //?sottostringa da restituire.
023400080721        //?In questo caso si dovrà ricorrere alla spezzettatura della parola stessa.
023500080721       if UltimoByte <= PosizioneIniziale;
023600080721            UltimoByte = PosizioneIniziale + piSottoStringaLen - 1;
023700081205            IF ultimoByte > stringaLen - posizioneIniziale;
023800081205             ultimoByte = stringaLen - posizioneIniziale;
023900081205            ENDIF;
024000080721            piPosizioneProgressiva = UltimoByte;
024100080721            RETURN %subst(stringa : PosizioneIniziale : UltimoByte);
024200080721       endIf;
024300080714
024400080714       piPosizioneProgressiva = UltimoByte;
024500080714       RETURN %subst(stringa : PosizioneIniziale :
024600080714                     UltimoByte - PosizioneIniziale + 1);
024700080704
024800080627      /END-FREE
024900080627     P ISST_getSottoStringa...
025000080627     P                 E
025100080704     P*--------------------------------------------------
