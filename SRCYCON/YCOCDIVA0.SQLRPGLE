000100091222     ***************************************************************************
000200091222     **
000300091223     ** Questo programma calcola il codice di esenzione IVA di un cliente.
000400091222     **
000500091222     ***************************************************************************
000600091222     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000700091222
000800091222     ***************************************************************************
000900091222     **
001000091222     ** Costanti.
001100091222     **
001200091222     ***************************************************************************
001300100104     D CAP_CAMPIONE_ITALIA...
001400100104     D                 C                   '22060'
001500100104     D CAP_LIVIGNO...
001600100104     D                 C                   '23030'
001700091222     D CAP_SANMARINO...
001800091222     D                 C                   '4789'
001900091222     D CAP_VATICANO...
002000091222     D                 C                   '00120'
002100091222     D CODICE_IVA_EXTRA_UE...
002200091222     D                 C                   '740'
002300091222     D CODICE_IVA_UE...
002400091222     D                 C                   '730'
002500091222     D TERRITORIALITA_EXTRA_UE...
002600091222     D                 C                   '3'
002700091222     D TERRITORIALITA_ITALIA...
002800091222     D                 C                   '1'
002900091222     D TERRITORIALITA_UE...
003000091222     D                 C                   '2'
003100091222     D TERRITORIALITA_SCONOSCIUTA...
003200091222     D                 C                   '0'
003300091222     D ESITO_ERROR...
003400091222     D                 C                   -1
003500091222     D ESITO_OK...
003600091222     D                 C                   0
003700091222     D ISO2_ITALIA...
003800091222     D                 C                   'IT'
003900091222     D ISO2_SANMARINO...
004000091222     D                 C                   'SM'
004100091222     D ISO2_VATICANO...
004200091222     D                 C                   'VA'
004300100104     D SIGLA_COMO...
004400100104     D                 C                   'CO'
004500100104     D SIGLA_SONDRIO...
004600100104     D                 C                   'SO'
004700091222     D SIGLA_ITALIA...
004800091222     D                 C                   'IT'
004900091222     D PROJ_ESITO_OK...
005000091222     D                 C                   '0'
005100091222     D PROJ_XATB_LETTURA_SINGOLA_CHIAVE...
005200091222     D                 C                   '1'
005300091222
005400091222     ***************************************************************************
005500091222     **
005600091222     ** Prototipi.
005700091222     **
005800091222     ***************************************************************************
005900091222      /COPY GAITRASRC/SRCPROTOPR,XATB
006000091222
006100091222     ***************************************************************************
006200091222     **
006300091222     ** Strutture dati.
006400091222     **
006500091222     ***************************************************************************
006600091222     D andiz00f      E DS                  QUALIFIED
006700091222     D                                     BASED(nullPtr)
006800091222     D angG12Ds      E DS                  QUALIFIED
006900091222     D                                     INZ
007000091222     D xAtbDs        E DS                  QUALIFIED
007100091222     D                                     INZ
007200091222     D  xtbRic       E                     INZ(PROJ_XATB_LETTURA_SINGOLA_CHIAVE)
007300091222     D  xtbAlc       E                     INZ(*OFF)
007400091222     D  xtbDsc       E                     INZ(*SYS)
007500091222     D  xtbGab       E                     INZ(*OFF)
007600091222     D  xtbNoa       E                     INZ(*OFF)
007700091222     D ycoCdIvaI     E DS                  QUALIFIED
007800091222     D                                     INZ
007900091222     D ycoCdIvaO     E DS                  QUALIFIED
008000091222     D                                     INZ
008100091222
008200091222     ***************************************************************************
008300091222     **
008400091222     ** Campi.
008500091222     **
008600091222     ***************************************************************************
008700091222     D rpyData         S                   LIKE(ycoCdIvaO)
008800091222     D rpyDataSize     S             10I 0
008900091222     D rpyEsito        S             10I 0
009000091222     D rqsData         S                   LIKE(ycoCdIvaI)
009100091222     D rqsDataSize     S             10I 0
009200091222     D rqsOpCode       S             10A
009300091222
009400091222     D*--------------------------------------------------
009500091222     D* Procedure name: GetTerritorialitaCliente
009600091222     D* Purpose:        Ritorna la territorialità del cliente
009700091222     D* Returns:        Codice territorialità.
009800091222     D* Parameter:      piNazionePartitaIva => Nazione partita IVA cliente.
009900091222     D* Parameter:      piNazioneCliente => Sigla nazione cliente.
010000091222     D*--------------------------------------------------
010100091222     D GetTerritorialitaCliente...
010200091222     D                 PR             1A
010300091222     D  piNazionePartitaIva...
010400091222     D                                2A   CONST
010500091222     D  piNazioneCliente...
010600091222     D                                     LIKE(andiz00f.stato)
010700091222     D                                     VALUE
010800091222     D  piCapCliente...
010900091222     D                                     LIKE(andiz00f.cap)
011000091222     D                                     CONST
011100100104     D  piProvinciaCliente...
011200100104     D                                     LIKE(andiz00f.prov)
011300100104     D                                     CONST
011400100104     D  piLocalitaCliente...
011500100104     D                                     LIKE(andiz00f.localit)
011600100104     D                                     CONST
011700091222
011800091222     D*--------------------------------------------------
011900091222     D* Procedure name: GetCodiceIva
012000091222     D* Purpose:        Ritorna il codice esenzione IVA.
012100091222     D* Returns:        Nuovo codice esenzione IVA.
012200091222     D* Parameter:      piTerritorialitaCliente => Territorialità cliente.
012300091222     D* Parameter:      piCodiceIvaAttuale => Codice IVA attuale.
012400091222     D*--------------------------------------------------
012500091222     D GetCodiceIva    PR                  LIKE(andiz00f.cdIva)
012600091222     D  piTerritorialitaCliente...
012700091222     D                                1A   CONST
012800091222     D  piCodiceIvaAttuale...
012900091222     D                                     LIKE(andiz00f.cdIva)
013000091222     D                                     CONST
013100091222
013200091222     D*--------------------------------------------------
013300091222     D* Procedure name: IsNazioneUE
013400091222     D* Purpose:        Restituisce *ON se la nazione appartiene all' Union...
013500091222     D*                          e Europea.
013600091222     D* Returns:        *ON = appartiene a U.E.
013700091222     D* Parameter:      piIso2Nazione => Sigla ISO2 nazione.
013800091222     D*--------------------------------------------------
013900091222     D IsNazioneUE     PR              N
014000091222     D  piIso2Nazione                 2A   CONST
014100091222
014200091222
014300091222     ***************************************************************************
014400091222     **
014500091222     ** Parametri.
014600091222     **
014700091222     ***************************************************************************
014800091222     C     *ENTRY        PLIST
014900091222     C                   PARM                    rqsOpCode
015000091222     C                   PARM                    rpyEsito
015100091222     C                   PARM                    rqsData
015200091222     C                   PARM                    rqsDataSize
015300091222     C                   PARM                    rpyData
015400091222     C                   PARM                    rpyDataSize
015500091222
015600091222     ***************************************************************************
015700091222     **
015800091222     ** Main.
015900091222     **
016000091222     ***************************************************************************
016100091222
016200091222      /FREE
016300091222
016400091222       CLEAR rpyEsito;
016500091222
016600091222       SELECT;
016700091222         WHEN rqsOpCode = 'GETCDIVA';
016800100104           RESET ycoCdIvaI;
016900100104           ycoCdIvaI = %SUBST(rqsData : 1 : rqsDataSize);
017000091222           RESET ycoCdIvaO;
017100091222           ycoCdIvaO.territoCli = GetTerritorialitaCliente(
017200091222                                          %SUBST(ycoCdIvaI.partitaIva : 1 : 2)
017300091222                                          : ycoCdIvaI.nazione
017400091222                                          : ycoCdIvaI.cap
017500100104                                          : ycoCdIvaI.provincia
017600100104                                          : ycoCdIvaI.localita
017700091222                                                          );
017800091222           ycoCdIvaO.codiceIva = GetCodiceIva( ycoCdIvaO.territoCli
017900091222                                             : ycoCdIvaI.codiceIva
018000091222                                             );
018100091222           %SUBST(rpyData : 1 : rpyDataSize) = ycoCdIvaO;
018200091222         WHEN rqsOpCode = 'SETONLR';
018300091222           *INLR = *ON;
018400091222         OTHER;
018500091222           rpyEsito = ESITO_ERROR;
018600091222       ENDSL;
018700091222
018800091222       RETURN;
018900091222
019000091222      /END-FREE
019100091222
019200091222     P*--------------------------------------------------
019300091222     P* Procedure name: GetTerritorialitaCliente
019400091222     P* Purpose:        Ritorna la territorialità del cliente
019500091222     P* Returns:        Codice territorialità.
019600091222     P* Parameter:      piNazionePartitaIva => Nazione partita IVA cliente.
019700091222     P* Parameter:      piNazioneCliente => Sigla nazione cliente.
019800091222     P*--------------------------------------------------
019900091222     P GetTerritorialitaCliente...
020000091222     P                 B
020100091222     D GetTerritorialitaCliente...
020200091222     D                 PI             1A
020300091222     D  piNazionePartitaIva...
020400091222     D                                2A   CONST
020500091222     D  piNazioneCliente...
020600091222     D                                     LIKE(andiz00f.stato)
020700091222     D                                     VALUE
020800091222     D  piCapCliente...
020900091222     D                                     LIKE(andiz00f.cap)
021000091222     D                                     CONST
021100100104     D  piProvinciaCliente...
021200100104     D                                     LIKE(andiz00f.prov)
021300100104     D                                     CONST
021400100104     D  piLocalitaCliente...
021500100104     D                                     LIKE(andiz00f.localit)
021600100104     D                                     CONST
021700091222
021800100104     D isCampioneItalia...
021900100104     D                 S               N
022000100104     D isLivigno...
022100100104     D                 S               N
022200100104
022300100104      /FREE
022400100104
022500100104       // Partita IVA sicuramente italiana.
022600091222
022700100104       IF piNazionePartitaIva = ISO2_ITALIA;
022800091222         RETURN TERRITORIALITA_ITALIA;
022900091222       ENDIF;
023000091222
023100091222       // Partita IVA San Marino o Vaticano.
023200091222
023300091222       IF piNazionePartitaIva = ISO2_SANMARINO
023400091222       OR piNazionePartitaIva = ISO2_VATICANO;
023500091222         RETURN TERRITORIALITA_EXTRA_UE;
023600091222       ENDIF;
023700091222
023800100104       // Partita IVA Unione Europea
023900091222
024000091222       IF IsNazioneUE(piNazionePartitaIva);
024100091222         RETURN TERRITORIALITA_UE;
024200091222       ENDIF;
024300091222
024400091222       // Reperisco la sigla ISO2 della nazione del cliente.
024500091222
024600091222       IF piNazioneCliente = *BLANK;
024700091222         piNazioneCliente = SIGLA_ITALIA;
024800091222       ENDIF;
024900091222
025000100104       // Gestisco le eccezioni dei clienti italiani.
025100100104
025200100104       IF piNazioneCliente = SIGLA_ITALIA;
025300100104
025400100104         // San Marino o Vaticano.
025500091222
025600100104         IF %SUBST(piCapCliente : 1 : 4) = CAP_SANMARINO
025700100104         OR piCapCliente = CAP_VATICANO;
025800100104           RETURN TERRITORIALITA_EXTRA_UE;
025900100104         ENDIF;
026000100104
026100100104         // Livigno.
026200100104
026300100104         IF piProvinciaCliente = SIGLA_SONDRIO AND piCapCliente = CAP_LIVIGNO;
026400100104           EXEC SQL
026500100104             SET :isLivigno = CASE
026600100104                              WHEN :piLocalitaCliente LIKE '%LIVIGNO%' THEN '1'
026700100104                              WHEN :piLocalitaCliente LIKE '%TREPALLE%' THEN '1'
026800100104                              ELSE '0'
026900100104                              END
027000100104           ;
027100100104           IF isLivigno;
027200100104             RETURN TERRITORIALITA_EXTRA_UE;
027300100104           ENDIF;
027400100104         ENDIF;
027500100104
027600100104         // Campione d'Italia.
027700100104
027800100104         IF piProvinciaCliente = SIGLA_COMO
027900100104         AND piCapCliente = CAP_CAMPIONE_ITALIA;
028000100104           EXEC SQL
028100100104             SET :isCampioneItalia = CASE
028200100104                                     WHEN :piLocalitaCliente LIKE '%CAMPIONE%'
028300100104                                     THEN '1'
028400100104                                     ELSE '0'
028500100104                                     END
028600100104           ;
028700100104           IF isCampioneItalia;
028800100104             RETURN TERRITORIALITA_EXTRA_UE;
028900100104           ENDIF;
029000100104         ENDIF;
029100091222
029200100104       ENDIF;
029300100104
029400091222       RESET xAtbDs;
029500091222       xAtbDs.xtbCod = 'G12';
029600091222       xAtbDs.xtbAzi = ycoCdIvaI.societa;
029700091222       xAtbDs.xtbKey = '00000000000' + piNazioneCliente;
029800091222
029900091222       Proj_Tabella(xAtbDs);
030000091222
030100091222       IF xAtbDs.xtbErr <> PROJ_ESITO_OK;
030200091222         RETURN TERRITORIALITA_ITALIA;
030300091222       ENDIF;
030400091222
030500091222       angG12Ds = xAtbDs.xtbUni;
030600091222
030700091222       // Nazione Italia.
030800091222
030900091222       IF angG12Ds.nazG12 = ISO2_ITALIA;
031000091222         IF piNazionePartitaIva = '$$' OR piNazionePartitaIva = *BLANK;
031100091222           RETURN TERRITORIALITA_SCONOSCIUTA;
031200091222         ENDIF;
031300091222         RETURN TERRITORIALITA_ITALIA;
031400091222       ENDIF;
031500091222
031600091222       // Nazione San Marino o Vaticano.
031700091222
031800091222       IF angG12Ds.nazG12 = ISO2_SANMARINO
031900091222       OR angG12Ds.nazG12 = ISO2_VATICANO;
032000091222         RETURN TERRITORIALITA_EXTRA_UE;
032100091222       ENDIF;
032200091222
032300091222       // Controllo se la nazione del cliente è U.E.
032400091222
032500091222       IF IsNazioneUE(angG12Ds.nazG12);
032600091222         IF piNazionePartitaIva = '$$' OR piNazionePartitaIva = *BLANK;
032700091222           RETURN TERRITORIALITA_SCONOSCIUTA;
032800091222         ENDIF;
032900091222         RETURN TERRITORIALITA_UE;
033000091222       ENDIF;
033100091222
033200091222       RETURN TERRITORIALITA_EXTRA_UE;
033300091222
033400091222      /END-FREE
033500091222     P GetTerritorialitaCliente...
033600091222     P                 E
033700091222
033800091222
033900091222     P*--------------------------------------------------
034000091222     P* Procedure name: GetCodiceIva
034100091222     P* Purpose:        Ritorna il codice esenzione IVA.
034200091222     P* Returns:        Nuovo codice esenzione IVA.
034300091222     P* Parameter:      piTerritorialitaCliente => Territorialità cliente.
034400091222     P* Parameter:      piCodiceIvaAttuale => Codice IVA attuale.
034500091222     P*--------------------------------------------------
034600091222     P GetCodiceIva    B
034700091222     D GetCodiceIva    PI                  LIKE(andiz00f.cdIva)
034800091222     D  piTerritorialitaCliente...
034900091222     D                                1A   CONST
035000091222     D  piCodiceIvaAttuale...
035100091222     D                                     LIKE(andiz00f.cdIva)
035200091222     D                                     CONST
035300091222
035400091222      /FREE
035500091222
035600091222       SELECT;
035700091222         WHEN piTerritorialitaCliente = TERRITORIALITA_ITALIA;
035800091222           SELECT;
035900091222             WHEN piCodiceIvaAttuale = *BLANK;
036000091222               RETURN piCodiceIvaAttuale;
036100091222             WHEN piCodiceIvaAttuale = '405';
036200091222               RETURN *BLANK;
036300091222             WHEN piCodiceIvaAttuale = '711';
036400091222               RETURN '911';
036500091222             WHEN piCodiceIvaAttuale = '911';
036600091222               RETURN piCodiceIvaAttuale;
036700091222           ENDSL;
036800091222         WHEN piTerritorialitaCliente = TERRITORIALITA_UE;
036900091222           RETURN CODICE_IVA_UE;
037000091222         WHEN piTerritorialitaCliente = TERRITORIALITA_EXTRA_UE;
037100091222           RETURN CODICE_IVA_EXTRA_UE;
037200091222         WHEN piTerritorialitaCliente = TERRITORIALITA_SCONOSCIUTA;
037300091222           RETURN *BLANK;
037400091222       ENDSL;
037500091222
037600091222       RETURN piCodiceIvaAttuale;
037700091222
037800091222      /END-FREE
037900091222     P GetCodiceIva    E
038000091222
038100091222
038200091222     P*--------------------------------------------------
038300091222     P* Procedure name: IsNazioneUE
038400091222     P* Purpose:        Restituisce *ON se la nazione appartiene all' Union...
038500091222     P*                          e Europea.
038600091222     P* Returns:        *ON = appartiene a U.E.
038700091222     P* Parameter:      piIso2Nazione => Sigla ISO2 nazione.
038800091222     P*--------------------------------------------------
038900091222     P IsNazioneUE     B
039000091222     D IsNazioneUE     PI              N
039100091222     D  piIso2Nazione                 2A   CONST
039200091222
039300091222      /FREE
039400091222
039500091222       IF piIso2Nazione = *BLANK
039600091222       OR piIso2Nazione = ISO2_SANMARINO
039700091222       OR piIso2Nazione = ISO2_VATICANO;
039800091222         RETURN *OFF;
039900091222       ENDIF;
040000091222
040100091222       RESET xAtbDs;
040200091222       xAtbDs.xtbCod = 'W06';
040300091222       xAtbDs.xtbAzi = ycoCdIvaI.societa;
040400091222       xAtbDs.xtbKey = '0000000000000' + piIso2Nazione;
040500091222
040600091222       Proj_Tabella(xAtbDs);
040700091222
040800091222       RETURN (xAtbDs.xtbErr = PROJ_ESITO_OK);
040900091222
041000091222      /END-FREE
041100091222     P IsNazioneUE     E
041200091222
