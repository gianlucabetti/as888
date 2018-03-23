000100080430     ***************************************************************************
000200080430     **
000300080430     ** Modulo di lettura veloce dell'organigramma AZORG00F.
000400080430     **
000500080509     ** Creare il programma di servizio col comando
000600080509     ** CRTSRVPGM SRVPGM(GAITRAFIL/TIBSORG00) SRCFILE(GAITRASRC/SRCSRVPGM)
000700141212     ** TEXT('Organigramma.') ACTGRP(*CALLER)
000800080509     **
000900080430     ** Può essere creata anche la versione programma col comando
001000141212     ** CRTPGM PGM(GAITRAFIL/TIBSORG00) ACTGRP(*CALLER)
001100080430     **
001200080430     ***************************************************************************
001300080430
001400080430     ***************************************************************************
001500080430     **
001600080430     ** Definizione prototipi procedure.
001700080430     **
001800080430     ***************************************************************************
001900140326     D/COPY GAITRASRC/SRCPROTOPR,FNLV55R
002000080430     D/COPY GAITRASRC/SRCPROTOPR,TIBSORG00
002100091028     D/COPY GAITRASRC/SRCPROTOPR,TIBS02R
002200080515     D/COPY GAITRASRC/SRCPROTOPR,CEETREC
002300080430
002400080430     ***************************************************************************
002500080430     **
002600080430     ** Definizione interfaccia procedura.
002700080430     **
002800080430     ***************************************************************************
002900080430     D Organigramma...
003000080430     D                 PI
003100080430     D  rqsOpCode                    10A   CONST
003200080430     D  rpyEsito                     10I 0
003300080430     D  rqsData                       2A   OPTIONS(*NOPASS:*VARSIZE)
003400080430     D  rqsDataLen                   10I 0 CONST OPTIONS(*NOPASS)
003500080515     D  rpyData                    4282A   OPTIONS(*NOPASS:*VARSIZE)
003600080515     D  rpyDataLen                   10I 0 CONST OPTIONS(*NOPASS)
003700080430
003800080430     ***************************************************************************
003900080430     **
004000080430     ** Definizione costanti.
004100080430     **
004200080430     ***************************************************************************
004300091028     D DOMINIO...
004400120302     D                 C                   '@brt.it'
004500091028     D NETWORK_ESTERO...
004600091028     D                 C                   'E'
004700091028     D NETWORK_ITALIA...
004800091028     D                 C                   ' '
004900080430     D ORGANIGRAMMA_ESITO_OK...
005000080430     D                 C                   0
005100080430     D ORGANIGRAMMA_ESITO_ERRORE_GENERICO...
005200080430     D                 C                   1
005300080430     D ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO...
005400080430     D                 C                   2
005500080430     D ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE...
005600080430     D                 C                   3
005700080430     D ORGANIGRAMMA_ESITO_ERRORE_OPCODE_SCONOSCIUTO...
005800080430     D                 C                   4
005900080430     D ORGANIGRAMMA_OPCODE_GETANAGRAFICAFILIALE...
006000080515     D                 C                   'GETAZORG00'
006100080509     D ORGANIGRAMMA_OPCODE_GETDESCRIZIONEFILIALE...
006200080509     D                 C                   'GETDESFIL'
006300080430     D ORGANIGRAMMA_OPCODE_INIT...
006400080430     D                 C                   'INIT'
006500080430     D ORGANIGRAMMA_OPCODE_ISINIT...
006600080430     D                 C                   'ISINIT'
006700080430     D ORGANIGRAMMA_OPCODE_FINALIZE...
006800080430     D                 C                   'FINALIZE'
006900140326     D TERMINAL_PARTENZA...
007000140326     D                 C                   'P'
007100080430
007200080430     ***************************************************************************
007300080430     **
007400080430     ** Definizione strutture dati.
007500080430     **
007600080430     ***************************************************************************
007700080430     D azorg00f      E DS                  QUALIFIED
007800080509     D                                     DIM(999)
007900080430     D                                     BASED(azorg00fPtr)
008000091028     D dNtw          E DS                  QUALIFIED
008100091028     D                                     INZ
008200140326     D fnlv55ds      E DS                  QUALIFIED INZ
008300091028     D kpjba         E DS                  QUALIFIED
008400091028     D                                     INZ
008500091028     D og143         E DS                  QUALIFIED
008600091028     D                                     INZ
008700080430     D rpyAzorg      E DS                  QUALIFIED
008800080430     D                                     EXTNAME(azorg00f)
008900080430     D tibsorg01     E DS                  QUALIFIED
009000091028     D tibs02ds      E DS                  QUALIFIED
009100091028     D                                     INZ
009200091028     D  t02Mod       E                     INZ('C')
009300080430
009400080430     ***************************************************************************
009500080430     **
009600080430     ** Definizione variabili globali.
009700080430     **
009800080430     ***************************************************************************
009900080430     D guida           S              5I 0 DIM(999)
010000080430     D elem            S             10I 0
010100080430     D i               S             10I 0
010200080509     D g               S             10I 0
010300080430     D initDone        S               N   INZ(*OFF)
010400080430
010500080430      /FREE
010600080430
010700080430       CLEAR rpyEsito;
010800080430
010900080509       IF rqsOpCode = ORGANIGRAMMA_OPCODE_GETDESCRIZIONEFILIALE;
011000080430         tibsorg01 = %SUBST(rqsData : 1 : rqsDataLen);
011100080509         rpyEsito = Organigramma_getDescrizioneFiliale( tibsorg01.fil
011200080509                                                      : rpyAzorg.orgDes
011300080509                                                      );
011400080509         %SUBST(rpyData : 1 : rpyDataLen) = rpyAzorg.orgDes;
011500080509       ELSEIF rqsOpCode = ORGANIGRAMMA_OPCODE_GETANAGRAFICAFILIALE;
011600080509         tibsorg01 = %SUBST(rqsData : 1 : rqsDataLen);
011700080509         rpyEsito = Organigramma_getAnagraficaFiliale( tibsorg01.fil
011800080509                                                     : rpyAzorg
011900080509                                                     );
012000080509         %SUBST(rpyData : 1 : rpyDataLen) = rpyAzorg;
012100080430       ELSEIF rqsOpCode = ORGANIGRAMMA_OPCODE_ISINIT;
012200080430         %SUBST(rpyData : 1 : rpyDataLen) = Organigramma_isInit();
012300080430       ELSEIF rqsOpCode = ORGANIGRAMMA_OPCODE_INIT;
012400080430         rpyEsito = Organigramma_init();
012500080430       ELSEIF rqsOpCode = ORGANIGRAMMA_OPCODE_FINALIZE;
012600080430         *INLR = *ON;
012700080515         Organigramma_finalize();
012800080430       ELSE;
012900080430         rpyEsito = ORGANIGRAMMA_ESITO_ERRORE_OPCODE_SCONOSCIUTO;
013000080430       ENDIF;
013100080430
013200080430       RETURN;
013300080430
013400080430      /END-FREE
013500080430
013600080430     P*--------------------------------------------------
013700080430     P* Procedure name: Organigramma_init
013800080430     P* Purpose:        Inizializza il modulo.
013900080430     P* Returns:        Esito.
014000080430     P*--------------------------------------------------
014100080430     P Organigramma_init...
014200080430     P                 B                   EXPORT
014300080430     D Organigramma_init...
014400080430     D                 PI            10I 0
014500091028     D  piKpjba                            LIKEDS(kpjba)
014600091028     D                                     OPTIONS(*NOPASS)
014700091028
014800080430     D esito           S             10I 0
014900080430
015000080430      /FREE
015100080430
015200080515       // IL modulo è già stato inizializzato.
015300080515
015400080430       IF Organigramma_isInit();
015500080515         DEALLOC(N) azorg00fPtr;
015600080515         CLEAR guida;
015700080515         RESET initDone;
015800080430       ENDIF;
015900080430
016000091028       IF %PARMS() > 0;
016100091028         kpjba = piKpjba;
016200091028       ENDIF;
016300091028
016400080430       // Conto quante filiali ci sono nell'organigramma.
016500080430
016600080430       CLEAR elem;
016700080430
016800080430       EXEC SQL SELECT COUNT(*)
016900080430                INTO :elem
017000080430                FROM AZORG00F
017100080430       ;
017200080430
017300080430       IF sqlCode < 0 OR elem = 0;
017400080430         DUMP(A);
017500080430         RETURN sqlCode;
017600080430       ENDIF;
017700080430
017800080430       // Alloco la memoria necessaria per memorizzare tutto l'organigramma.
017900080430
018000080430       azorg00fPtr = %ALLOC(elem * %SIZE(azorg00f));
018100080430
018200080430       // Memorizzo tutto l'organigramma in una DS multipla.
018300080430
018400080430       EXEC SQL DECLARE ORGANIGRAMMA CURSOR FOR
018500080430                SELECT *
018600080430                FROM AZORG00F
018700080430                ORDER BY ORGFIL
018800080430                FOR READ ONLY
018900080430       ;
019000080430
019100080430       EXEC SQL OPEN ORGANIGRAMMA;
019200080430
019300080430       IF sqlCode < 0;
019400080430         DUMP(A);
019500080430         RETURN sqlCode;
019600080430       ENDIF;
019700080430
019800080430       EXEC SQL FETCH NEXT
019900080430                FROM ORGANIGRAMMA
020000080430                FOR :elem ROWS
020100080430                INTO :azorg00f
020200080430       ;
020300080430
020400080430       IF sqlCode < 0;
020500080430         DUMP(A);
020600080430         esito = sqlCode;
020700080430       ENDIF;
020800080430
020900080430       EXEC SQL CLOSE ORGANIGRAMMA;
021000080430
021100080430       IF esito < 0;
021200080430         RETURN esito;
021300080430       ENDIF;
021400080430
021500080509       // Carico la schiera delle filiali che mi farà da guida per accedere alla
021600080509       // schiera dell'organigramma.
021700080430
021800080430       FOR i = 1 TO elem;
021900080509         guida(azorg00f(i).orgFil) = i;
022000080430       ENDFOR;
022100080430
022200080430       CLEAR i;
022300080430       initDone = *ON;
022400080430
022500080430       RETURN esito;
022600080430
022700080430      /END-FREE
022800080430     P Organigramma_init...
022900080430     P                 E
023000080430
023100080430     P*--------------------------------------------------
023200080430     P* Procedure name: Organigramma_getAnagraficaFiliale
023300080430     P* Purpose:        Restituisce l'anagrafica di una filiale.
023400080430     P* Returns:        Esito
023500080430     P* Parameter:      piFiliale => Filiale.
023600080430     P* Parameter:      piAzorg00f => Record AZORG00F.
023700080430     P*--------------------------------------------------
023800080430     P Organigramma_getAnagraficaFiliale...
023900080430     P                 B                   EXPORT
024000080430     D Organigramma_getAnagraficaFiliale...
024100080430     D                 PI            10I 0
024200080430     D  piFiliale                          LIKE(azorg00f.orgFil)
024300080430     D                                     CONST
024400080430     D  piAzorg00f                         LIKEDS(azorg00f)
024500080430
024600080430      /FREE
024700080430
024800080430       // Pulisco i parametri di output.
024900080430
025000080430       CLEAR piAzorg00f;
025100080430
025200080430       // Non è stata eseguita l'inizializzazione del modulo.
025300080430
025400080430       IF NOT Organigramma_isInit();
025500080430         Organigramma_init();
025600080430       ENDIF;
025700080430
025800080430       // Il codice filiale non è valido.
025900080430
026000080430       IF piFiliale <= 0;
026100080430         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO;
026200080430       ENDIF;
026300080430
026400080430       // La filiale non esiste.
026500080430
026600080430       IF guida(piFiliale) = 0;
026700080430         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE;
026800080430       ENDIF;
026900080430
027000080509       piAzorg00f = azorg00f(guida(piFiliale));
027100080430
027200080430       RETURN ORGANIGRAMMA_ESITO_OK;
027300080430
027400080430      /END-FREE
027500080430     P Organigramma_getAnagraficaFiliale...
027600080430     P                 E
027700080509
027800080509     P*--------------------------------------------------
027900080509     P* Procedure name: Organigramma_getDescrizioneFiliale
028000080509     P* Purpose:        Restituisce la descrizione della filiale.
028100080509     P* Returns:        Esito.
028200080509     P* Parameter:      piFiliale => Filiale.
028300080509     P* Parameter:      piDescrizioneFiliale => Descrizione filiale.
028400080509     P*--------------------------------------------------
028500080509     P Organigramma_getDescrizioneFiliale...
028600080509     P                 B                   EXPORT
028700080509     D Organigramma_getDescrizioneFiliale...
028800080509     D                 PI            10I 0
028900080509     D  piFiliale                          LIKE(azorg00f.orgFil)
029000080509     D                                     CONST
029100080509     D  piDescrizioneFiliale...
029200080509     D                                     LIKE(azorg00f.orgDes)
029300080509
029400080509      /FREE
029500080509
029600080509       // Pulisco i parametri di output.
029700080509
029800080509       CLEAR piDescrizioneFiliale;
029900080509
030000080509       // Non è stata eseguita l'inizializzazione del modulo.
030100080509
030200080509       IF NOT Organigramma_isInit();
030300080509         Organigramma_init();
030400080509       ENDIF;
030500080509
030600080509       // Il codice filiale non è valido.
030700080509
030800080509       IF piFiliale <= 0;
030900080509         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO;
031000080509       ENDIF;
031100080509
031200080509       // La filiale non esiste.
031300080509
031400080509       IF guida(piFiliale) = 0;
031500080509         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE;
031600080509       ENDIF;
031700080509
031800080509       piDescrizioneFiliale = azorg00f(guida(piFiliale)).orgDes;
031900080509
032000080509       RETURN ORGANIGRAMMA_ESITO_OK;
032100080509
032200080509      /END-FREE
032300080509     P Organigramma_getDescrizioneFiliale...
032400080509     P                 E
032500091028
032600091028     P*--------------------------------------------------
032700091028     P* Procedure name: Organigramma_getIndirizzoFiliale
032800091028     P* Purpose:        Restituisce l'indirizzo di una filiale.
032900091028     P* Returns:        Esito.
033000091028     P* Parameter:      piFiliale => ID filiale.
033100091028     P* Parameter:      piIndirizzo => Indirizzo
033200091028     P* Parameter:      piCap => Codice avviamento postale.
033300091028     P* Parameter:      piLocalita => Località.
033400091028     P* Parameter:      piSiglaProvincia => Sigla provincia.
033500091028     P*--------------------------------------------------
033600091028     P Organigramma_getIndirizzoFiliale...
033700091028     P                 B                   EXPORT
033800091028     D Organigramma_getIndirizzoFiliale...
033900091028     D                 PI            10I 0
034000091028     D  piFiliale                          LIKE(azorg00f.orgFil)
034100091028     D                                     CONST
034200091028     D  piIndirizzo                        LIKE(azorg00f.orgInd)
034300091028     D  piCap                              LIKE(azorg00f.orgCpf)
034400091028     D  piLocalita                         LIKE(azorg00f.orgLoc)
034500091028     D  piSiglaProvincia...
034600091028     D                                     LIKE(azorg00f.orgPro)
034700091028
034800091028      /FREE
034900091028
035000091028       // Pulisco i parametri di output.
035100091028
035200091028       CLEAR piIndirizzo;
035300091028       CLEAR piCap;
035400091028       CLEAR piLocalita;
035500091028       CLEAR piSiglaProvincia;
035600091028
035700091028       // Non è stata eseguita l'inizializzazione del modulo.
035800091028
035900091028       IF NOT Organigramma_isInit();
036000091028         Organigramma_init();
036100091028       ENDIF;
036200091028
036300091028       // Il codice filiale non è valido.
036400091028
036500091028       IF piFiliale <= 0;
036600091028         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO;
036700091028       ENDIF;
036800091028
036900091028       // La filiale non esiste.
037000091028
037100091028       IF guida(piFiliale) = 0;
037200091028         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE;
037300091028       ENDIF;
037400091028
037500091028       piIndirizzo = azorg00f(guida(piFiliale)).orgInd;
037600091028       piCap = azorg00f(guida(piFiliale)).orgCpf;
037700091028       piLocalita = azorg00f(guida(piFiliale)).orgLoc;
037800091028       piSiglaProvincia = azorg00f(guida(piFiliale)).orgPro;
037900091028
038000091028       RETURN ORGANIGRAMMA_ESITO_OK;
038100091028
038200091028      /END-FREE
038300091028     P Organigramma_getIndirizzoFiliale...
038400091028     P                 E
038500091028
038600091028     P*--------------------------------------------------
038700091028     P* Procedure name: Organigramma_getRecapitiFiliale
038800091028     P* Purpose:        Restituisce telefono, fax, telex, indirizzo e-mail ...
038900091028     P*                          generico.
039000091028     P* Returns:        Esito
039100091028     P* Parameter:      piFiliale => ID filiale
039200091028     P* Parameter:      piTelefono => Telefono.
039300091028     P* Parameter:      piTelefax => Telefax.
039400091028     P* Parameter:      piTelex => Telex.
039500091028     P* Parameter:      piIndirizzoEmailInfo => Indirizzo e-mail info.
039600091028     P*--------------------------------------------------
039700091028     P Organigramma_getRecapitiFiliale...
039800091028     P                 B                   EXPORT
039900091028     D Organigramma_getRecapitiFiliale...
040000091028     D                 PI            10I 0
040100091028     D  piFiliale                          LIKE(azorg00f.orgFil)
040200091028     D                                     CONST
040300091028     D  piTelefono                         LIKE(azorg00f.orgTel)
040400091029     D                                     OPTIONS(*OMIT)
040500091028     D  piTelefax                          LIKE(azorg00f.orgFax)
040600091029     D                                     OPTIONS(*OMIT)
040700091028     D  piTelex                            LIKE(azorg00f.orgTlx)
040800091029     D                                     OPTIONS(*OMIT)
040900091028     D  piIndirizzoEmailInfo...
041000091028     D                              255A   VARYING
041100091029     D                                     OPTIONS(*OMIT)
041101150415     D  piIndirizzoEmailAmministrazione...
041102150415     D                              255A   VARYING
041103150415     D                                     OPTIONS(*NOPASS)
041200140326
041300091028      /FREE
041400091028
041500091029       IF %ADDR(piTelefono) <> *NULL;
041600091029         CLEAR piTelefono;
041700091029       ENDIF;
041800091029
041900091029       IF %ADDR(piTelefax) <> *NULL;
042000091029         CLEAR piTelefax;
042100091029       ENDIF;
042200091029
042300091029       IF %ADDR(piTelex) <> *NULL;
042400091029         CLEAR piTelex;
042500091029       ENDIF;
042600091029
042700091029       IF %ADDR(piIndirizzoEmailInfo) <> *NULL;
042800091029         CLEAR piIndirizzoEmailInfo;
042900091029       ENDIF;
042901150415
042902150415       IF %PARMS() > 5;
042903150415         CLEAR piIndirizzoEmailAmministrazione;
042904150415       ENDIF;
043000091028
043100091028       // Non è stata eseguita l'inizializzazione del modulo.
043200091028
043300091028       IF NOT Organigramma_isInit();
043400091028         Organigramma_init();
043500091028       ENDIF;
043600091028
043700091028       // Il codice filiale non è valido.
043800091028
043900091028       IF piFiliale <= 0;
044000091028         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO;
044100091028       ENDIF;
044200091028
044300091028       // La filiale non esiste.
044400091028
044500091028       IF guida(piFiliale) = 0;
044600091028         RETURN ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE;
044700091028       ENDIF;
044800091028
044900091029
045000091029       IF %ADDR(piTelefono) <> *NULL;
045100091029         piTelefono = azorg00f(guida(piFiliale)).orgTel;
045200091029       ENDIF;
045300091029
045400091029       IF %ADDR(piTelefax) <> *NULL;
045500091029         piTelefax = azorg00f(guida(piFiliale)).orgFax;
045600091029       ENDIF;
045700091029
045800091029       IF %ADDR(piTelex) <> *NULL;
045900091029         piTelex = azorg00f(guida(piFiliale)).orgTlx;
046000091029       ENDIF;
046100091028
046200091029       IF %ADDR(piIndirizzoEmailInfo) <> *NULL;
046300091029
046400140326         // L'indirizzo e-mail generico è infoXXX@brt.it
046500091028
046600091029         og143 = azorg00f(guida(piFiliale)).orgDe3;
046700091029         RESET tibs02ds;
046800091029         tibs02ds.t02Cod = 'NTW';
046900091029         tibs02ds.t02Ke1 = og143.§ogNtw;
047000091029         TNTBE_RicercaControllo(kpjba : tibs02ds);
047100091029         dNtw = tibs02ds.t02Uni;
047200091028
047300140326         piIndirizzoEmailInfo = 'info';
047400140326
047500091029         IF dNtw.§ntwFie = NETWORK_ESTERO;
047600140326           RESET fnlv55ds;
047700140326           fnlv55ds.d55tpt = TERMINAL_PARTENZA;
047800140326           fnlv55ds.d55lin = piFiliale;
047900140326           fnlv55ds.d55drf = %DEC(%DATE() : *ISO);
048000140326           fnlv55r(fnlv55ds);
048100140326           piIndirizzoEmailInfo += %EDITC(fnlv55ds.d55tfp : 'X');
048200140326         ELSE;
048300140326           piIndirizzoEmailInfo += %EDITC(piFiliale : 'X');
048400091029         ENDIF;
048500091028
048600140326         piIndirizzoEmailInfo += DOMINIO;
048700091029
048800091029       ENDIF;
048801150415
048802150415       IF %PARMS() > 5 AND azorg00f(guida(piFiliale)).orgDc1 <> *BLANK;
048803150415         piIndirizzoEmailAmministrazione =
048804150415                            %TRIMR(azorg00f(guida(piFiliale)).orgDc1) + DOMINIO;
048805150415       ENDIF;
048900091028
049000091028       RETURN ORGANIGRAMMA_ESITO_OK;
049100091028
049200091028      /END-FREE
049300091028     P Organigramma_getRecapitiFiliale...
049400091028     P                 E
049500091028
049600080430
049700080430     P*--------------------------------------------------
049800080430     P* Procedure name: Organigramma_finalize
049900080430     P* Purpose:        Chiude il modulo
050000080430     P* Returns:        Esito.
050100080430     P*--------------------------------------------------
050200080430     P Organigramma_finalize...
050300080430     P                 B                   EXPORT
050400080430     D Organigramma_finalize...
050500080515     D                 PI
050600080430
050700080430      /FREE
050800080430
050900080515       // Chiudo il gruppo di attivazione AZORG.
051000080515
051100080515       CEE_NormalEnd();
051200080515
051300080430      /END-FREE
051400080430     P Organigramma_finalize...
051500080430     P                 E
051600080430
051700080430     P*--------------------------------------------------
051800080430     P* Procedure name: Organigramma_isInit
051900080430     P* Purpose:        Restituisce *ON se il modulo è già stato inizializzato.
052000080430     P* Returns:        *ON = modulo già inizializzato, *OFF = modulo non i...
052100080430     P*                          nizializzato.
052200080430     P*--------------------------------------------------
052300080430     P Organigramma_isInit...
052400080430     P                 B                   EXPORT
052500080430     D Organigramma_isInit...
052600080430     D                 PI              N
052700080430
052800080430      /FREE
052900080430
053000080430       RETURN initDone;
053100080430
053200080430      /END-FREE
053300080430     P Organigramma_isInit...
053400080430     P                 E
053500091028
