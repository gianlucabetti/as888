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
001900110509     D SQLCODE_ROW_NOT_FOUND...
002000110509     D                 C                   100
002100090313
002200090313     ***************************************************************************
002300090313     **
002400090313     ** Dichiarazione procedure esterne.
002500090313     **
002600090313     ***************************************************************************
002700090406      /DEFINE DFTACTGRP_NO
002800090406      /DEFINE DFTACTGRP_YES
002900090313     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
003000090406      /UNDEFINE DFTACTGRP_NO
003100090406      /UNDEFINE DFTACTGRP_YES
003200090312
003300090312     ***************************************************************************
003400090312     **
003500090312     ** Definizione strutture dati.
003600090312     **
003700090312     ***************************************************************************
003800091119     D azSoc00f      E DS                  QUALIFIED
003900090313     D                                     INZ
004000091119     D tibsSocI0     E DS                  QUALIFIED
004100090313     D                                     INZ
004200091119     D tibsSocO0     E DS                  QUALIFIED
004300090313     D                                     INZ
004400090312
004500090312     ***************************************************************************
004600090312     **
004700090312     ** Definizione variabili modulo/programma.
004800090312     **
004900090312     ***************************************************************************
005000110509     D dataValidita...
005100110509     D                 S               D
005200090313     D esito...
005300090313     D                 S             10I 0
005400090313     D initDone...
005500090313     D                 S               N
005600090313     D newSocietaDone...
005700090313     D                 S               N
005800090313
005900090313     ***************************************************************************
006000090313     **
006100090313     ** Dichiarazione parametri programma.
006200090313     **
006300090313     ***************************************************************************
006400090313     D Societa_Lettura...
006500090313     D                 PI
006600090313     D prmRqsOpCode...
006700090313     D                               10A
006800090313     D prmRpyOpCode...
006900090313     D                               10A
007000090313     D prmRpyIdMsg...
007100090313     D                               10I 0
007200090313     D prmRqsFormato...
007300090313     D                               10A
007400090313     D prmRqsData...
007500090313     D                            32767A   OPTIONS(*VARSIZE)
007600090313     D prmRqsDataSize...
007700090313     D                               10I 0
007800090313     D prmRpyFormato...
007900090313     D                               10A
008000090313     D prmRpyData...
008100090313     D                            32767A   OPTIONS(*VARSIZE)
008200090313     D prmRpyDataSize...
008300090313     D                               10I 0
008400090312
008500090312     ***************************************************************************
008600090312     **
008700090312     ** Main.
008800090312     **
008900090312     ***************************************************************************
009000090312
009100090312      /FREE
009200090312
009300090312       prmRpyOpCode = 'DONE';
009400090313       CLEAR prmRpyIdMsg;
009500090312
009600090313       IF prmRqsOpCode = 'GETANAGRAF';
009700090312
009800090313         %SUBST(prmRpyData : 1 : prmRpyDataSize) = *BLANK;
009900090312
010000090313         IF prmRqsFormato = 'TIBSSOCI0';
010100090313           tibsSocI0 = %SUBST(prmRqsData : 1 : prmRqsDataSize);
010200090313           esito = Societa_NewSocieta( tibsSocI0.idSocieta
010300090313                                     : tibsSocI0.dtValidita
010400090312                                     );
010500090312           IF esito < ESITO_OK;
010600090313             prmRpyOpCode = 'ERRORE';
010700090313             prmRpyIdMsg = esito;
010800090312             RETURN;
010900090313           ELSEIF esito = SOCIETA_INFO_ANAGRAFICA_DA_PROJ;
011000090313             prmRpyOpCode = 'INFO';
011100090313             prmRpyIdMsg = esito;
011200090312           ELSEIF esito > ESITO_OK;
011300090313             prmRpyOpCode = 'INFO';
011400090313             prmRpyIdMsg = esito;
011500090312             RETURN;
011600090312           ENDIF;
011700090312         ENDIF;
011800090312
011900090313         IF prmRpyFormato = 'TIBSSOCO0';
012000090313           tibsSocO0 = Societa_GetAnagrafica( prmRpyFormato );
012100090313           %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSocO0;
012200090312         ELSE;
012300090313           prmRpyOpCode = 'ERRORE';
012400090313           prmRpyIdMsg = SOCIETA_ERRORE_FORMATO_RPY_SCONOSCIUTO;
012500090312         ENDIF;
012600090312
012700090312       ELSEIF prmRqsOpCode = 'INIT';
012800090312
012900090313         esito = Societa_Init();
013000090313
013100090313         IF esito < ESITO_OK;
013200090313           prmRpyOpCode = 'ERRORE';
013300090313           prmRpyIdMsg = esito;
013400090313         ENDIF;
013500090312
013600090312       ELSEIF prmRqsOpCode = 'FINALIZE';
013700090312
013800090313         Societa_Finalize();
013900090313         *INLR = *ON;
014000090312
014100090312       ELSE;
014200090312
014300090313         prmRpyOpCode = 'ERRORE';
014400090313         prmRpyIdMsg = SOCIETA_ERRORE_OPCODE_SCONOSCIUTO;
014500090312
014600090312       ENDIF;
014700090312
014800090312       RETURN;
014900090312
015000090312      /END-FREE
015100090313
015200090313
015300090313     P*--------------------------------------------------
015400090313     P* Procedure name: Societa_Init
015500090313     P* Purpose:        Inizializza il modulo.
015600090313     P* Returns:        Esito.
015700090313     P*--------------------------------------------------
015800090313     P Societa_Init    B                   EXPORT
015900090313     D Societa_Init    PI            10I 0
016000090313
016100090313      /FREE
016200090313
016300090313       IF initDone;
016400090313         RETURN SOCIETA_INFO_MODULO_GIA_INIZIALIZZATO;
016500090313       ENDIF;
016600090313
016700090313       initDone = *ON;
016800090313
016900090313       RETURN ESITO_OK;
017000090313
017100090313      /END-FREE
017200090313     P Societa_Init    E
017300090313
017400090313     P*--------------------------------------------------
017500090313     P* Procedure name: Societa_Finalize
017600090313     P* Purpose:        Chiude il modulo.
017700090313     P* Returns:
017800090313     P*--------------------------------------------------
017900090313     P Societa_Finalize...
018000090313     P                 B                   EXPORT
018100090313     D Societa_Finalize...
018200090313     D                 PI
018300090313
018400090313      /FREE
018500090313
018600090313       RESET initDone;
018800110509       RESET newSocietaDone;
018900090313
019000090313      /END-FREE
019100090313     P Societa_Finalize...
019200090313     P                 E
019300090313
019400090313     P*--------------------------------------------------
019500090313     P* Procedure name: Societa_NewSocieta
019600090313     P* Purpose:        Crea l'oggetto società.
019700090313     P* Returns:        Esito.
019800090313     P* Parameter:      piIdSocieta => Id società.
019900090313     P* Parameter:      piDataValidita => Data validità anagrafica.
020000090313     P*--------------------------------------------------
020100090313     P Societa_NewSocieta...
020200090313     P                 B                   EXPORT
020300090313     D Societa_NewSocieta...
020400090313     D                 PI            10I 0
020500090313     D  piIdSocieta                   3A   CONST
020600090313     D piDataValidita                  D   DATFMT(*ISO)
020700090313     D                                     CONST
020800090313     D                                     OPTIONS(*NOPASS)
020900090313
021000110509     D retField...
021100110509     D                 S             10I 0
021200090313     D count...
021300090313     D                 S             10I 0
021400090313
021500090313      /FREE
021600090313
021700090313       RESET newSocietaDone;
021800090313       CLEAR azSoc00f;
021900090313
022000090313       // Il modulo non è stato inizializzato.
022100090313
022200090313       IF NOT initDone;
022300090313         RETURN SOCIETA_ERRORE_MODULO_NON_INIZIALIZZATO;
022400090313       ENDIF;
022500090313
022600090313       // Parametri obbligatori.
022700090313
022800090313       IF piIdSocieta = *BLANK;
022900090313         RETURN SOCIETA_ERRORE_IDSOCIETA_OBBLIGATORIO;
023000090313       ENDIF;
023100090313
023200090313       IF %PARMS() > 1 AND piDataValidita > *LOVAL;
023300090313         dataValidita = piDataValidita;
023400090313       ELSE;
023500090313         dataValidita = %DATE();
023600090313       ENDIF;
023700090313
023800090313       // Reperisco l'anagrafica valida alla data richiesta.
023900090313
024000090313       EXEC SQL
024100090313         SELECT ID_RIGA
024200090313              , ID_SOCIETA
024300090313              , VALIDITA_DATA_INIZIO
024400090313              , VALIDITA_DATA_FINE
024500090313              , RAGIONE_SOCIALE
024600090313              , RAGIONE_SOCIALE_BREVE
024700090313              , ATTIVITA_DATA_INIZIO
024800090313              , IFNULL( ATTIVITA_DATA_FINE , '9999-12-31' )
024900090313              , ATTIVITA_ATECO2007
025000090313              , CODICE_FISCALE
025100090313              , IFNULL( PARTITA_IVA , '' )
025200090313              , NATURA_GIURIDICA
025300090313              , SOCIO_UNICO
025400090313              , CAPITALE_SOCIALE_IMPORTO
025500090313              , CAPITALE_SOCIALE_TIPO
025600090313              , CAPITALE_SOCIALE_IV
025700090313              , REA_LUOGO
025800090313              , REA_NUMERO
025900090313              , REGISTRO_IMPRESE_LUOGO
026000090313              , REGISTRO_IMPRESE_DATA
026100090313              , SEDE_LEGALE_NAZIONE
026200090313              , SEDE_LEGALE_PROVINCIA
026300090313              , SEDE_LEGALE_COMUNE
026400090313              , SEDE_LEGALE_CAP
026500090313              , SEDE_LEGALE_INDIRIZZO
026600090313              , SEDE_LEGALE_TELEFONO
026700090313              , SEDE_LEGALE_TELEFAX
026800090313              , SEDE_LEGALE_COMUNE_CATASTO
026900090313              , SEDE_AMMIN_NAZIONE
027000090313              , SEDE_AMMIN_PROVINCIA
027100090313              , SEDE_AMMIN_COMUNE
027200090313              , SEDE_AMMIN_CAP
027300090313              , SEDE_AMMIN_INDIRIZZO
027400090313              , SEDE_AMMIN_TELEFONO
027500090313              , SEDE_AMMIN_TELEFAX
027600090313              , SEDE_AMMIN_COMUNE_CATASTO
027700090313              , DIVISA
027800090313              , IFNULL( SOGGETTO , '' )
027900091119              , IFNULL( CODICE_FISCALE_INCORPORANTE , '' )
028000091119              , IFNULL( CASELLA_POSTALE , '' )
028100090313           INTO :azSoc00f
028200090313           FROM AZSOC00F
028300090406           WHERE ID_SOCIETA = :piIdSocieta
028400090406             AND :dataValidita BETWEEN VALIDITA_DATA_INIZIO
028500090406                                   AND VALIDITA_DATA_FINE
028600090406             AND :dataValidita BETWEEN ATTIVITA_DATA_INIZIO
028700090406                                   AND IFNULL( ATTIVITA_DATA_FINE
028800090406                                             , '9999-12-31' )
028900090313       ;
029000090313
029100090313       IF sqlCode < *ZERO;
029200090313         DUMP(A);
029300090313         RETURN sqlCode;
029400090313       ELSEIF sqlCode <> 100;
029500090313         newSocietaDone = *ON;
029600090313         RETURN ESITO_OK;
029700090313       ENDIF;
029800090313
029900090313       // Non ho trovato un'anagrafica valida, controllo l'esistenza della
030000090313       // società in Proj.
030100090313
030200090313       EXEC SQL
030300090313         SELECT 0
030400090313              , SIFSOCIETA
030500090313              , '0001-01-01'
030600090313              , CASE SIFDTFIVL
030700090313                WHEN '0001-01-01' THEN '9999-12-31'
030800090313                ELSE SIFDTFIVL
030900090313                END
031000090313              , RTRIM(SIFRGS)
031100090313              , RTRIM(SIFDESBREV)
031200090313              , '0001-01-01'
031300090313              , CASE SIFDTFIVL
031400090313                WHEN '0001-01-01' THEN '9999-12-31'
031500090313                ELSE SIFDTFIVL
031600090313                END
031700090429              , RTRIM(SIFATCCIAA)
031800090429              , RTRIM(SIFCDFISC)
031900090313              , CASE SIFPARTIVA
032000090313                WHEN '' THEN ''
032100090313                ELSE CONCAT( 'IT' , SIFPARTIVA )
032200090313                END
032300090313              , SIFNATGIUR
032400090313              , ''
032500090313              , 0
032600090313              , ''
032700090313              , ''
032800090429              , RTRIM(SIFCTI)
032900090429              , RTRIM(SIFISCCIAA)
033000090429              , RTRIM(SIFCTI)
033100090313              , SIFDTISCC
033200090313              , SIFSTATO
033300090313              , SIFPROV
033400090313              , RTRIM(SIFLOCALIT)
033500090313              , RTRIM(SIFCAP)
033600090313              , RTRIM(SIFVIA)
033700090313              , RTRIM(SIFTELEFON)
033800090313              , RTRIM(SIFTELEFAX)
033900090313              , ''
034000090313              , SIFSTATO
034100090313              , SIFPRS
034200090313              , RTRIM(SIFCSC)
034300090313              , RTRIM(SIFCAS)
034400090313              , RTRIM(SIFVSC)
034500090313              , RTRIM(SIFTLS)
034600090313              , RTRIM(SIFTELEFAX)
034700090313              , ''
034800090313              , SIFDIVISA
034900090313              , ''
035000091119              , ''
035100091119              , ''
035200090313           INTO :azSoc00f
035300090313           FROM ANSIF00F
035400090313           WHERE ANSIF00F.SIFSOCIETA = :piIdSocieta
035500090313       ;
035600090313
035700090313       IF sqlCode < *ZERO;
035800090313         DUMP(A);
035900090313         RETURN sqlCode;
036000090313       ELSEIF sqlCode = 100;
036100090313         RETURN SOCIETA_INFO_SOCIETA_INESISTENTE;
036200090313       ELSE;
036300090313         newSocietaDone = *ON;
036400110509         retField = SOCIETA_INFO_ANAGRAFICA_DA_PROJ;
036500090313       ENDIF;
036600110509
036700090313       RETURN ESITO_OK;
036800090313
036900090313      /END-FREE
037000090313     P Societa_NewSocieta...
037100090313     P                 E
037200090313
037300090313
037400090313     P*--------------------------------------------------
037500090313     P* Procedure name: Societa_GetAnagrafica
037600090313     P* Purpose:        Restituisce l'anagrafica nel formato richiesto.
037700090313     P* Returns:        Anagrafica società.
037800090313     P* Parameter:      piNomeFormato => Nome formato dati output.
037900090313     P*--------------------------------------------------
038000090313     P Societa_GetAnagrafica...
038100090313     P                 B                   EXPORT
038200090313     D Societa_GetAnagrafica...
038300090313     D                 PI         32767A
038400090313     D  piNomeFormato                10A   CONST
038500090313
038600090313      /FREE
038700090313
038800090313       IF piNomeFormato = 'TIBSSOCO0';
038900090313         IF NOT newSocietaDone;
039000090313           CLEAR tibsSocO0;
039100090313           RETURN tibsSocO0;
039200090313         ENDIF;
039300090313         EVAL-CORR tibsSocO0 = azSoc00f;
039400090313         RETURN tibsSocO0;
039500090313       ELSE;
039600090313         RETURN *BLANK;
039700090313       ENDIF;
039800090313
039900090313      /END-FREE
040000090313     P Societa_GetAnagrafica...
040100090313     P                 E
040200110509
040300110509
040400110509     P*--------------------------------------------------
040500110509     P* Procedure name: Societa_IsBRT
040600110509     P* Purpose:        Restituisce *ON se la società è BRT spa.
040700110509     P* Returns:        *ON = BRT spa; *OFF = Bartolini spa.
040800110509     P*--------------------------------------------------
040900110509     P Societa_IsBRT   B                   EXPORT
041000110509     D Societa_IsBRT   PI              N
041100110509
041200110509     D retField...
041300110509     D                 S               N   STATIC
041400110509     D dataFineBartolini...
041500110509     D                 S              8A   STATIC
041600110509
041700110509      /FREE
041800110509
041900110509       IF NOT newSocietaDone;
042000110509         RETURN *OFF;
042100110509       ENDIF;
042200110509
042300110509       // Dal 6 giugno 2011 la società Bartolini spa cambia ragione sociale in BRT spa.
042400110509
042500110509       RESET retField;
042600110509       RESET dataFineBartolini;
042700110509
042800110509       IF azSoc00f.idSocieta = SOCIETA_BARTOLINI;
042900110509         retField = *ON;
043000110509         EXEC SQL
043100110509           SELECT LEFT(tbeUni, 8) AS DATA_FINE_BARTOLINI
043200110509             INTO :dataFineBartolini
043300110509             FROM tntbe00f
043400110509             WHERE tbeCod = 'VPO' AND tbeKe1 = 'BRT' AND tbeKe2 = ''
043500110509               AND tbeLin = '' AND tbeSif = '' AND tbeAtb = ''
043600110509         ;
043700110509         SELECT;
043800110509           WHEN sqlCode < *ZERO;
043900110509             DUMP(A);
044000110509           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
044100110509           OTHER;
044200110509             retField = (dataValidita > %DATE(dataFineBartolini : *ISO0));
044300110509         ENDSL;
044400110509       ENDIF;
044500110509
044600110509       RETURN retField;
044700110509
044800110509      /END-FREE
044900110509     P Societa_IsBRT   E
045000110509
