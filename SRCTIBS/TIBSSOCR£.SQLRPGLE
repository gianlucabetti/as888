000100090312     ***************************************************************************
000200090312     **
000300090312     ** Anagrafico società.
000400090312     ** Lettura.
000500090313     **
000600090313     ** Questo modulo è anche un programma. Dopo aver creato il modulo creare il
000700090313     ** programma col comando CRTPGM PGM(GAITRAFIL/TIBSSOCR)
000800090313     **
000900090312     ***************************************************************************
001000090313
001100090312     ***************************************************************************
001200090312     **
001300090312     ** Definizione costanti.
001400090312     **
001500090312     ***************************************************************************
001600090313     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
001700090312     D ESITO_OK...
001800090312     D                 C                   0
001900090313
002000090313     ***************************************************************************
002100090313     **
002200090313     ** Dichiarazione procedure esterne.
002300090313     **
002400090313     ***************************************************************************
002500090406      /DEFINE DFTACTGRP_NO
002600090406      /DEFINE DFTACTGRP_YES
002700090313     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
002800090406      /UNDEFINE DFTACTGRP_NO
002900090406      /UNDEFINE DFTACTGRP_YES
003000090312
003100090312     ***************************************************************************
003200090312     **
003300090312     ** Definizione strutture dati.
003400090312     **
003500090312     ***************************************************************************
003600090313     D azSoc00f...
003700090313     D               E DS                  QUALIFIED
003800090313     D                                     INZ
003900090313     D tibsSocI0...
004000090313     D               E DS                  QUALIFIED
004100090313     D                                     INZ
004200090313     D tibsSocO0...
004300090313     D               E DS                  QUALIFIED
004400090313     D                                     INZ
004500090312
004600090312     ***************************************************************************
004700090312     **
004800090312     ** Definizione variabili modulo/programma.
004900090312     **
005000090312     ***************************************************************************
005100090313     D esito...
005200090313     D                 S             10I 0
005300090313     D initDone...
005400090313     D                 S               N
005500090313     D newSocietaDone...
005600090313     D                 S               N
005700090313
005800090313     ***************************************************************************
005900090313     **
006000090313     ** Dichiarazione parametri programma.
006100090313     **
006200090313     ***************************************************************************
006300090313     D Societa_Lettura...
006400090313     D                 PI
006500090313     D prmRqsOpCode...
006600090313     D                               10A
006700090313     D prmRpyOpCode...
006800090313     D                               10A
006900090313     D prmRpyIdMsg...
007000090313     D                               10I 0
007100090313     D prmRqsFormato...
007200090313     D                               10A
007300090313     D prmRqsData...
007400090313     D                            32767A   OPTIONS(*VARSIZE)
007500090313     D prmRqsDataSize...
007600090313     D                               10I 0
007700090313     D prmRpyFormato...
007800090313     D                               10A
007900090313     D prmRpyData...
008000090313     D                            32767A   OPTIONS(*VARSIZE)
008100090313     D prmRpyDataSize...
008200090313     D                               10I 0
008300090312
008400090312     ***************************************************************************
008500090312     **
008600090312     ** Main.
008700090312     **
008800090312     ***************************************************************************
008900090312
009000090312      /FREE
009100090312
009200090312       prmRpyOpCode = 'DONE';
009300090313       CLEAR prmRpyIdMsg;
009400090312
009500090313       IF prmRqsOpCode = 'GETANAGRAF';
009600090312
009700090313         %SUBST(prmRpyData : 1 : prmRpyDataSize) = *BLANK;
009800090312
009900090313         IF prmRqsFormato = 'TIBSSOCI0';
010000090313           tibsSocI0 = %SUBST(prmRqsData : 1 : prmRqsDataSize);
010100090313           esito = Societa_NewSocieta( tibsSocI0.idSocieta
010200090313                                     : tibsSocI0.dtValidita
010300090312                                     );
010400090312           IF esito < ESITO_OK;
010500090313             prmRpyOpCode = 'ERRORE';
010600090313             prmRpyIdMsg = esito;
010700090312             RETURN;
010800090313           ELSEIF esito = SOCIETA_INFO_ANAGRAFICA_DA_PROJ;
010900090313             prmRpyOpCode = 'INFO';
011000090313             prmRpyIdMsg = esito;
011100090312           ELSEIF esito > ESITO_OK;
011200090313             prmRpyOpCode = 'INFO';
011300090313             prmRpyIdMsg = esito;
011400090312             RETURN;
011500090312           ENDIF;
011600090312         ENDIF;
011700090312
011800090313         IF prmRpyFormato = 'TIBSSOCO0';
011900090313           tibsSocO0 = Societa_GetAnagrafica( prmRpyFormato );
012000090313           %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSocO0;
012100090312         ELSE;
012200090313           prmRpyOpCode = 'ERRORE';
012300090313           prmRpyIdMsg = SOCIETA_ERRORE_FORMATO_RPY_SCONOSCIUTO;
012400090312         ENDIF;
012500090312
012600090312       ELSEIF prmRqsOpCode = 'INIT';
012700090312
012800090313         esito = Societa_Init();
012900090313
013000090313         IF esito < ESITO_OK;
013100090313           prmRpyOpCode = 'ERRORE';
013200090313           prmRpyIdMsg = esito;
013300090313         ENDIF;
013400090312
013500090312       ELSEIF prmRqsOpCode = 'FINALIZE';
013600090312
013700090313         Societa_Finalize();
013800090313         *INLR = *ON;
013900090312
014000090312       ELSE;
014100090312
014200090313         prmRpyOpCode = 'ERRORE';
014300090313         prmRpyIdMsg = SOCIETA_ERRORE_OPCODE_SCONOSCIUTO;
014400090312
014500090312       ENDIF;
014600090312
014700090312       RETURN;
014800090312
014900090312      /END-FREE
015000090313
015100090313
015200090313     P*--------------------------------------------------
015300090313     P* Procedure name: Societa_Init
015400090313     P* Purpose:        Inizializza il modulo.
015500090313     P* Returns:        Esito.
015600090313     P*--------------------------------------------------
015700090313     P Societa_Init    B                   EXPORT
015800090313     D Societa_Init    PI            10I 0
015900090313
016000090313      /FREE
016100090313
016200090313       IF initDone;
016300090313         RETURN SOCIETA_INFO_MODULO_GIA_INIZIALIZZATO;
016400090313       ENDIF;
016500090313
016600090313       initDone = *ON;
016700090313
016800090313       RETURN ESITO_OK;
016900090313
017000090313      /END-FREE
017100090313     P Societa_Init    E
017200090313
017300090313     P*--------------------------------------------------
017400090313     P* Procedure name: Societa_Finalize
017500090313     P* Purpose:        Chiude il modulo.
017600090313     P* Returns:
017700090313     P*--------------------------------------------------
017800090313     P Societa_Finalize...
017900090313     P                 B                   EXPORT
018000090313     D Societa_Finalize...
018100090313     D                 PI
018200090313
018300090313      /FREE
018400090313
018500090313       RESET initDone;
018600090313
018700090313      /END-FREE
018800090313     P Societa_Finalize...
018900090313     P                 E
019000090313
019100090313     P*--------------------------------------------------
019200090313     P* Procedure name: Societa_NewSocieta
019300090313     P* Purpose:        Crea l'oggetto società.
019400090313     P* Returns:        Esito.
019500090313     P* Parameter:      piIdSocieta => Id società.
019600090313     P* Parameter:      piDataValidita => Data validità anagrafica.
019700090313     P*--------------------------------------------------
019800090313     P Societa_NewSocieta...
019900090313     P                 B                   EXPORT
020000090313     D Societa_NewSocieta...
020100090313     D                 PI            10I 0
020200090313     D  piIdSocieta                   3A   CONST
020300090313     D piDataValidita                  D   DATFMT(*ISO)
020400090313     D                                     CONST
020500090313     D                                     OPTIONS(*NOPASS)
020600090313
020700090313     D count...
020800090313     D                 S             10I 0
020900090313     D dataValidita...
021000090313     D                 S               D
021100090313
021200090313      /FREE
021300090313
021400090313       RESET newSocietaDone;
021500090313       CLEAR azSoc00f;
021600090313
021700090313       // Il modulo non è stato inizializzato.
021800090313
021900090313       IF NOT initDone;
022000090313         RETURN SOCIETA_ERRORE_MODULO_NON_INIZIALIZZATO;
022100090313       ENDIF;
022200090313
022300090313       // Parametri obbligatori.
022400090313
022500090313       IF piIdSocieta = *BLANK;
022600090313         RETURN SOCIETA_ERRORE_IDSOCIETA_OBBLIGATORIO;
022700090313       ENDIF;
022800090313
022900090313       IF %PARMS() > 1 AND piDataValidita > *LOVAL;
023000090313         dataValidita = piDataValidita;
023100090313       ELSE;
023200090313         dataValidita = %DATE();
023300090313       ENDIF;
023400090313
023500090313       // Reperisco l'anagrafica valida alla data richiesta.
023600090313
023700090313       EXEC SQL
023800090313         SELECT ID_RIGA
023900090313              , ID_SOCIETA
024000090313              , VALIDITA_DATA_INIZIO
024100090313              , VALIDITA_DATA_FINE
024200090313              , RAGIONE_SOCIALE
024300090313              , RAGIONE_SOCIALE_BREVE
024400090313              , ATTIVITA_DATA_INIZIO
024500090313              , IFNULL( ATTIVITA_DATA_FINE , '9999-12-31' )
024600090313              , ATTIVITA_ATECO2007
024700090313              , CODICE_FISCALE
024800090313              , IFNULL( PARTITA_IVA , '' )
024900090313              , NATURA_GIURIDICA
025000090313              , SOCIO_UNICO
025100090313              , CAPITALE_SOCIALE_IMPORTO
025200090313              , CAPITALE_SOCIALE_TIPO
025300090313              , CAPITALE_SOCIALE_IV
025400090313              , REA_LUOGO
025500090313              , REA_NUMERO
025600090313              , REGISTRO_IMPRESE_LUOGO
025700090313              , REGISTRO_IMPRESE_DATA
025800090313              , SEDE_LEGALE_NAZIONE
025900090313              , SEDE_LEGALE_PROVINCIA
026000090313              , SEDE_LEGALE_COMUNE
026100090313              , SEDE_LEGALE_CAP
026200090313              , SEDE_LEGALE_INDIRIZZO
026300090313              , SEDE_LEGALE_TELEFONO
026400090313              , SEDE_LEGALE_TELEFAX
026500090313              , SEDE_LEGALE_COMUNE_CATASTO
026600090313              , SEDE_AMMIN_NAZIONE
026700090313              , SEDE_AMMIN_PROVINCIA
026800090313              , SEDE_AMMIN_COMUNE
026900090313              , SEDE_AMMIN_CAP
027000090313              , SEDE_AMMIN_INDIRIZZO
027100090313              , SEDE_AMMIN_TELEFONO
027200090313              , SEDE_AMMIN_TELEFAX
027300090313              , SEDE_AMMIN_COMUNE_CATASTO
027400090313              , DIVISA
027500090313              , IFNULL( SOGGETTO , '' )
027600090313           INTO :azSoc00f
027700090313           FROM AZSOC00F
027800090406           WHERE ID_SOCIETA = :piIdSocieta
027900090406             AND :dataValidita BETWEEN VALIDITA_DATA_INIZIO
028000090406                                   AND VALIDITA_DATA_FINE
028100090406             AND :dataValidita BETWEEN ATTIVITA_DATA_INIZIO
028200090406                                   AND IFNULL( ATTIVITA_DATA_FINE
028300090406                                             , '9999-12-31' )
028400090313       ;
028500090313
028600090313       IF sqlCode < *ZERO;
028700090313         DUMP(A);
028800090313         RETURN sqlCode;
028900090313       ELSEIF sqlCode <> 100;
029000090313         newSocietaDone = *ON;
029100090313         RETURN ESITO_OK;
029200090313       ENDIF;
029300090313
029400090313       // Non ho trovato un'anagrafica valida, controllo l'esistenza della
029500090313       // società in Proj.
029600090313
029700090313       EXEC SQL
029800090313         SELECT 0
029900090313              , SIFSOCIETA
030000090313              , '0001-01-01'
030100090313              , CASE SIFDTFIVL
030200090313                WHEN '0001-01-01' THEN '9999-12-31'
030300090313                ELSE SIFDTFIVL
030400090313                END
030500090313              , RTRIM(SIFRGS)
030600090313              , RTRIM(SIFDESBREV)
030700090313              , '0001-01-01'
030800090313              , CASE SIFDTFIVL
030900090313                WHEN '0001-01-01' THEN '9999-12-31'
031000090313                ELSE SIFDTFIVL
031100090313                END
031200090429              , RTRIM(SIFATCCIAA)
031300090429              , RTRIM(SIFCDFISC)
031400090313              , CASE SIFPARTIVA
031500090313                WHEN '' THEN ''
031600090313                ELSE CONCAT( 'IT' , SIFPARTIVA )
031700090313                END
031800090313              , SIFNATGIUR
031900090313              , ''
032000090313              , 0
032100090313              , ''
032200090313              , ''
032300090429              , RTRIM(SIFCTI)
032400090429              , RTRIM(SIFISCCIAA)
032500090429              , RTRIM(SIFCTI)
032600090313              , SIFDTISCC
032700090313              , SIFSTATO
032800090313              , SIFPROV
032900090313              , RTRIM(SIFLOCALIT)
033000090313              , RTRIM(SIFCAP)
033100090313              , RTRIM(SIFVIA)
033200090313              , RTRIM(SIFTELEFON)
033300090313              , RTRIM(SIFTELEFAX)
033400090313              , ''
033500090313              , SIFSTATO
033600090313              , SIFPRS
033700090313              , RTRIM(SIFCSC)
033800090313              , RTRIM(SIFCAS)
033900090313              , RTRIM(SIFVSC)
034000090313              , RTRIM(SIFTLS)
034100090313              , RTRIM(SIFTELEFAX)
034200090313              , ''
034300090313              , SIFDIVISA
034400090313              , ''
034500090313           INTO :azSoc00f
034600090313           FROM ANSIF00F
034700090313           WHERE ANSIF00F.SIFSOCIETA = :piIdSocieta
034800090313       ;
034900090313
035000090313       IF sqlCode < *ZERO;
035100090313         DUMP(A);
035200090313         RETURN sqlCode;
035300090313       ELSEIF sqlCode = 100;
035400090313         RETURN SOCIETA_INFO_SOCIETA_INESISTENTE;
035500090313       ELSE;
035600090313         newSocietaDone = *ON;
035700090313         RETURN SOCIETA_INFO_ANAGRAFICA_DA_PROJ;
035800090313       ENDIF;
035900090313
036000090313       RETURN ESITO_OK;
036100090313
036200090313      /END-FREE
036300090313     P Societa_NewSocieta...
036400090313     P                 E
036500090313
036600090313
036700090313     P*--------------------------------------------------
036800090313     P* Procedure name: Societa_GetAnagrafica
036900090313     P* Purpose:        Restituisce l'anagrafica nel formato richiesto.
037000090313     P* Returns:        Anagrafica società.
037100090313     P* Parameter:      piNomeFormato => Nome formato dati output.
037200090313     P*--------------------------------------------------
037300090313     P Societa_GetAnagrafica...
037400090313     P                 B                   EXPORT
037500090313     D Societa_GetAnagrafica...
037600090313     D                 PI         32767A
037700090313     D  piNomeFormato                10A   CONST
037800090313
037900090313      /FREE
038000090313
038100090313       IF piNomeFormato = 'TIBSSOCO0';
038200090313         IF NOT newSocietaDone;
038300090313           CLEAR tibsSocO0;
038400090313           RETURN tibsSocO0;
038500090313         ENDIF;
038600090313         EVAL-CORR tibsSocO0 = azSoc00f;
038700090313         RETURN tibsSocO0;
038800090313       ELSE;
038900090313         RETURN *BLANK;
039000090313       ENDIF;
039100090313
039200090313      /END-FREE
039300090313     P Societa_GetAnagrafica...
039400090313     P                 E
039500090313
