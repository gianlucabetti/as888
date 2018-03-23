000100130328     ***************************************************************************
000200130328     **
000300130328     ** Questo modulo contiene i controlli da eseguire in fase di inserimento,
000400130328     ** aggiornamento e cancellazione di una pratica di contenzioso cliente.
000500130328     **
000600130328     ***************************************************************************
000700130328     H NOMAIN EXTBININT(*YES)
000800130328
000900130328     ***************************************************************************
001000130328     **
001100130328     ** Strutture dati.
001200130328     **
001300130328     ***************************************************************************
001400130328     D andiz00f      E DS                  QUALIFIED TEMPLATE
001500130328     D done            DS                  QUALIFIED INZ
001600130328     D  init                           N
001700130328     D kpjba         E DS                  QUALIFIED INZ
001800130328
001900130328     ***************************************************************************
002000130328     **
002100130328     ** Campi.
002200130328     **
002300130328     ***************************************************************************
002400130328     D esito           S             10I 0
002500130328
002600130328     ***************************************************************************
002700130328     **
002800130328     ** Costanti.
002900130328     **
003000130328     ***************************************************************************
003100130328      /COPY GAITRASRC/SRCCONST,YCOCTZ
003200130328     D PROJ_ESITO_OK...
003300130328     D                 C                   '0'
003400130328     D PROJ_RICHIESTA_CHAIN...
003500130328     D                 C                   '1'
003600130328     D PROJ_SOTTONATURA_CLIENTE...
003700130328     D                 C                   'C'
003800130328     D SQLCODE_ROW_IN_USE...
003900130328     D                 C                   -913
004000130328     D SQLCODE_ROW_NOT_FOUND...
004100130328     D                 C                   100
004200130328     D SQLCODE_SUCCESSFUL...
004300130328     D                 C                   0
004400130328
004500130328     ***************************************************************************
004600130328     **
004700130328     ** Prototipi.
004800130328     **
004900130328     ***************************************************************************
005000130328      /COPY GAITRASRC/SRCPROTOPR,YCOCTZCHK
005100130328
005200130328
005300130328     P*--------------------------------------------------
005400130328     P* Procedure name: YCOCTZCHK_Init
005500130328     P* Purpose:        Inizializza il modulo.
005600130328     P* Returns:        Esito.
005700130328     P* Parameter:      priKpjba
005800130328     P*--------------------------------------------------
005900130328     P YCOCTZCHK_Init  B                   EXPORT
006000130328     D YCOCTZCHK_Init  PI            10I 0
006100130328     D  priKpjba                           LIKEDS(kpjba)
006200130328
006300130328     D retField        S             10I 0 STATIC
006400130328
006500130328      /FREE
006600130328
006700130328       IF done.init;
006800130328         RETURN YCOCTZ_ESITO_OK;
006900130328       ENDIF;
007000130328
007100130328       kpjba = priKpjba;
007200130328
007300130328       done.init = *ON;
007400130328
007500130328       RETURN YCOCTZ_ESITO_OK;
007600130328
007700130328      /END-FREE
007800130328     P YCOCTZCHK_Init  E
007900130328
008000130328
008100130328     P*--------------------------------------------------
008200130328     P* Procedure name: YCOCTZCHK_Finalize
008300130328     P* Purpose:        Chiude il modulo.
008400130328     P* Returns:
008500130328     P*--------------------------------------------------
008600130328     P YCOCTZCHK_Finalize...
008700130328     P                 B                   EXPORT
008800130328     D YCOCTZCHK_Finalize...
008900130328     D                 PI
009000130328
009100130328      /FREE
009200130328
009300130328       RESET done;
009400130328
009500130328      /END-FREE
009600130328     P YCOCTZCHK_Finalize...
009700130328     P                 E
009800130328
009900130328
010000130328     P*--------------------------------------------------
010100130328     P* Procedure name: YCOCTZCHK_IsClienteInContenzioso
010200130328     P* Purpose:        Restituisce *ON se il cliente ha un contenzioso aperto.
010300130328     P* Returns:        *ON = cliente con contenzioso aperto.
010400130328     P* Parameter:      priIdCliente
010500130328     P* Parameter:      priPartitaIva
010600130328     P* Parameter:      priCodiceFiscale
010700130328     P*--------------------------------------------------
010800130328     P YCOCTZCHK_IsClienteInContenzioso...
010900130328     P                 B                   EXPORT
011000130328     D YCOCTZCHK_IsClienteInContenzioso...
011100130328     D                 PI              N
011200130328     D  priIdSocieta                  3A   CONST
011300130328     D  priIdCliente                  8A   CONST
011400130328     D  priPartitaIva                20A   CONST
011500130328     D  priCodiceFiscale...
011600130328     D                               20A   CONST
011700130328
011800130328     D retField        S               N   STATIC
011900130328
012000130328      /FREE
012100130328
012200130328       RESET retField;
012300130328
012400130328       // Ci provo col ID cliente.
012500130328
012600130328       IF priIdCliente <> *BLANK;
012700130328
012800130328         EXEC SQL
012900130328           SELECT CHAR('1', 1)
013000130328             INTO :retField
013100130328             FROM yCtzPra0f
013200130328            WHERE yCtzPra0f.id_cliente_societa = :priIdSocieta
013300130328              AND yCtzPra0f.id_cliente_ksc = :priIdCliente
013400130328              AND yCtzPra0f.data_fine_contenzioso IS NULL
013500130328              AND yCtzPra0f.data_delete IS NULL
013600130328         ;
013700130328
013800130328         SELECT;
013900130328           WHEN sqlCode < SQLCODE_SUCCESSFUL;
014000130328             DUMP(A);
014100130328           WHEN retField;
014200130328             RETURN retField;
014300130328         ENDSL;
014400130328
014500130328       ENDIF;
014600130328
014700130328       // Ci provo con la partita IVA.
014800130328
014900130328       IF priPartitaIva <> *BLANK;
015000130328
015100130328         EXEC SQL
015200130328           SELECT CHAR('1', 1)
015300130328             INTO :retField
015400130328             FROM yCtzPra0f
015500130328            WHERE yCtzPra0f.id_cliente_societa = :priIdSocieta
015600130328              AND yCtzPra0f.partita_iva = :priPartitaIva
015700130328              AND yCtzPra0f.data_fine_contenzioso IS NULL
015800130328              AND yCtzPra0f.data_delete IS NULL
015900130328         ;
016000130328
016100130328         SELECT;
016200130328           WHEN sqlCode < SQLCODE_SUCCESSFUL;
016300130328             DUMP(A);
016400130328           WHEN retField;
016500130328             RETURN retField;
016600130328         ENDSL;
016700130328
016800130328       ENDIF;
016900130328
017000130328       // Ci provo col codice fiscale.
017100130328
017200130328       IF priCodiceFiscale <> *BLANK;
017300130328
017400130328         EXEC SQL
017500130328           SELECT CHAR('1', 1)
017600130328             INTO :retField
017700130328             FROM yCtzPra0f
017800130328            WHERE yCtzPra0f.id_cliente_societa = :priIdSocieta
017900130328              AND yCtzPra0f.codice_fiscale = :priCodiceFiscale
018000130328              AND yCtzPra0f.data_fine_contenzioso IS NULL
018100130328              AND yCtzPra0f.data_delete IS NULL
018200130328         ;
018300130328
018400130328         SELECT;
018500130328           WHEN sqlCode < SQLCODE_SUCCESSFUL;
018600130328             DUMP(A);
018700130328           WHEN retField;
018800130328             RETURN retField;
018900130328         ENDSL;
019000130328
019100130328       ENDIF;
019200130328
019300130328       // A questo punto retField può essere solo *OFF.
019400130328
019500130328       RETURN retField;
019600130328
019700130328      /END-FREE
019800130328     P YCOCTZCHK_IsClienteInContenzioso...
019900130328     P                 E
020000130328
