000100110729     ***************************************************************************
000200110729     **
000300110729     ** Questo programma riceve una data e un numero di giorni lavorativi e
000400110729     ** restituisce un elenco di date dove è possibile chiedere ritiri.
000500110729     **
000600110729     ***************************************************************************
000700110801     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000800110729
000900110729     ***************************************************************************
001000110729     **
001100110729     ** Costanti.
001200110729     **
001300110729     ***************************************************************************
001400110802     D GIORNI_LAVORATIVI_DFT...
001500110802     D                 C                   10
001600110801     D ESITO_OK...
001700110801     D                 C                   0
001800110729     D ESITO_ERRORE...
001900110729     D                 C                   -1
002000110729     D ESITO_ERRORE_GIORNI_LAVORATIVI...
002100110729     D                 C                   -2
002200110729     D ESITO_ERRORE_DATA_DECORRENZA...
002300110729     D                 C                   -3
002400110801     D FESTA...
002500110801     D                 C                   'F'
002600110729     D LINGUA_FRANCESE...
002700110729     D                 C                   'fr'
002800110729     D LINGUA_INGLESE...
002900110729     D                 C                   'en'
003000110729     D LINGUA_ITALIANO...
003100110729     D                 C                   'it'
003200110729     D LINGUA_TEDESCO...
003300110729     D                 C                   'de'
003400110802     D MITTENTE...
003500110802     D                 C                   'M'
003600110803     D MSGID_OGGI...
003700110803     D                 C                   'TIS0698'
003800110803     D MSGID_LUNEDI...
003900110803     D                 C                   'TIS0351'
004000110803     D MSGID_MARTEDI...
004100110803     D                 C                   'TIS0354'
004200110803     D MSGID_MERCOLEDI...
004300110803     D                 C                   'TIS0356'
004400110803     D MSGID_GIOVEDI...
004500110803     D                 C                   'TIS0203'
004600110803     D MSGID_VENERDI...
004700110803     D                 C                   'TIS0607'
004800110803     D MSGID_SABATO...
004900110803     D                 C                   'TIS0533'
005000110803     D MSGID_DOMENICA...
005100110803     D                 C                   'TIS0159'
005200110801     D SQLCODE_ROW_NOT_FOUND...
005300110801     D                 C                   100
005400110729
005500110729     ***************************************************************************
005600110729     **
005700110729     ** Strutture dati.
005800110729     **
005900110729     ***************************************************************************
006000110801     D dataRitiro...
006100110801     D                 DS                  QUALIFIED
006200110801     D  data                           D   DIM(99) DATFMT(*EUR)
006300110801     D  descrizione                  30A   DIM(99)
006400110801     D  i                             3U 0
006500110801     D dayOfWeek       DS                  QUALIFIED
006600110803     D                                7A   INZ(MSGID_LUNEDI)
006700110803     D                                7A   INZ(MSGID_MARTEDI)
006800110803     D                                7A   INZ(MSGID_MERCOLEDI)
006900110803     D                                7A   INZ(MSGID_GIOVEDI)
007000110803     D                                7A   INZ(MSGID_VENERDI)
007100110803     D                                7A   INZ(MSGID_SABATO)
007200110803     D                                7A   INZ(MSGID_DOMENICA)
007300110801     D  msgId                         7A   DIM(7) OVERLAY(dayOfWeek)
007400110729     D lingue          DS
007500110801     D                                2A   INZ(LINGUA_TEDESCO)
007600110801     D                                2A   INZ(LINGUA_INGLESE)
007700110801     D                                2A   INZ(LINGUA_FRANCESE)
007800110801     D                                2A   INZ(LINGUA_ITALIANO)
007900110729     D  lingua                        2A   DIM(4) ASCEND OVERLAY(lingue)
008000110729     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT)
008100110729     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT)
008200110801     D parametri       DS                  QUALIFIED INZ
008300110801     D  lingua...
008400110802     D                                2A   INZ(LINGUA_ITALIANO)
008500110803     D  pagamentoMittente...
008600110803     D                                 N
008700110803     D  anonimo...
008800110803     D                                 N
008900110801     D  giorniLavorativi...
009000110802     D                                3U 0 INZ(GIORNI_LAVORATIVI_DFT)
009100110801     D  dataDecorrenza...
009200110802     D                                 D
009300110803     D  oggi...
009400110803     D                                 D   INZ(*SYS)
009500110824     D  indirizzoRitiroNonConvenzionale...
009600110824     D                                 N
009700110802     D parametri_prev  DS                  LIKEDS(parametri) INZ
009800110729
009900110729     ***************************************************************************
010000110729     **
010100110729     ** Campi.
010200110729     **
010300110729     ***************************************************************************
010400110801     D rqsDataFormat   S             10A
010500110729     D rqsDataSize     S             10I 0
010600110729     D rqsData         S                   LIKE(tio10800i)
010700110729     D rpyEsito        S             10I 0
010800110729     D rpyDataFormat   S             10A
010900110729     D rpyDataSize     S             10I 0
011000110729     D rpyData         S                   LIKE(tio10800o)
011100110729
011200110729     ***************************************************************************
011300110729     **
011400110729     ** Prototipi.
011500110729     **
011600110729     ***************************************************************************
011700110801
011800110801     D*--------------------------------------------------
011900110801     D* Procedure name: rtvMsgLang
012000110801     D* Purpose:        Reperisce la descrizione in lingus di un messaggio.
012100110801     D* Returns:        Testo messaggio secondo livello
012200110801     D* Parameter:      linguaISO2 => Lingua ISO2
012300110801     D* Parameter:      msgId => Identificativo messaggio
012400110801     D* Parameter:      msgDta => Valori campi dati messaggi
012500110801     D* Parameter:      msg => Testo messaggio primo livello
012600110801     D* Parameter:      secLvl => Testo messaggio secondo livello
012700110801     D* Parameter:      rtnCode => Codice di ritorno
012800110801     D* Parameter:      esito => Esito
012900110801     D*--------------------------------------------------
013000110801     D rtvMsgLang      PR          3512A
013100110803     D  msgId                         7A   CONST
013200110801     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
013300110801     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
013400110801     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
013500110801     D                                     VARYING
013600110801     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
013700110801     D                                     VARYING
013800110801     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
013900110801     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
014000110801
014100110729     D*--------------------------------------------------
014200110729     D* Procedure name: Main
014300110729     D* Purpose:
014400110729     D* Returns:        Esito.
014500110729     D*--------------------------------------------------
014600110729     D Main            PR            10I 0
014700110729
014800110729     D*--------------------------------------------------
014900110729     D* Procedure name: SetLingua
015000110729     D* Purpose:        Imposta la lingua.
015100110729     D* Returns:        Esito.
015200110729     D* Parameter:      priLingua
015300110729     D*--------------------------------------------------
015400110729     D SetLingua       PR            10I 0
015500110729     D  priLingua                     2A
015600110729
015700110729     D*--------------------------------------------------
015800110729     D* Procedure name: SetGiorniLavorativi
015900110729     D* Purpose:        Imposta i giorni lavorativi.
016000110729     D* Returns:        Esito.
016100110729     D* Parameter:      priGiorniLavorativi
016200110729     D*--------------------------------------------------
016300110729     D SetGiorniLavorativi...
016400110729     D                 PR            10I 0
016500110729     D  priGiorniLavorativi...
016600110729     D                               10I 0 CONST
016700110729
016800110729     D*--------------------------------------------------
016900110729     D* Procedure name: SetDataDecorrenza
017000110729     D* Purpose:        Imposta la data decorrenza.
017100110729     D* Returns:        Esito.
017200110729     D* Parameter:      priDataDecorrenza => Data decorrenza formato *EUR.
017300110729     D*--------------------------------------------------
017400110729     D SetDataDecorrenza...
017500110729     D                 PR            10I 0
017600110729     D  priDataDecorrenza...
017700110802     D                               10A   CONST
017800110801
017900110801     D*--------------------------------------------------
018000110801     D* Procedure name: IsFesta
018100110801     D* Purpose:        Restituisce *ON se il giorno è festivo.
018200110801     D* Returns:        *ON = festa.
018300110801     D* Parameter:      priData
018400110801     D*--------------------------------------------------
018500110801     D IsFesta         PR              N
018600110801     D  priData                        D
018700110801
018800110801     D*--------------------------------------------------
018900110801     D* Procedure name: GetDescrizioneGiorno
019000110801     D* Purpose:        Restituisce la descrizione del giorno.
019100110801     D* Returns:        Descrizione del giorno.
019200110801     D* Parameter:      priData
019300110801     D*--------------------------------------------------
019400110801     D GetDescrizioneGiorno...
019500110801     D                 PR           255A   VARYING
019600110801     D  priData                        D
019700110802
019800110802     D*--------------------------------------------------
019900110802     D* Procedure name: SetStrategiUserNumber
020000110802     D* Purpose:
020100110802     D* Returns:        Esito.
020200110802     D* Parameter:      priSun => Strategi User Number
020300110802     D*--------------------------------------------------
020400110802     D SetStrategiUserNumber...
020500110802     D                 PR            10I 0
020600110802     D  priSun                        9S 0
020700110802
020800110802     D*--------------------------------------------------
020900110802     D* Procedure name: SetPagamento
021000110802     D* Purpose:        Imposta il pagamento delle spese di trasporto.
021100110802     D* Returns:        Esito.
021200110802     D* Parameter:      priPagamento
021300110802     D*--------------------------------------------------
021400110802     D SetPagamento    PR            10I 0
021500110802     D  priPagamento                  1A
021600110802
021700110802     D*--------------------------------------------------
021800110802     D* Procedure name: SetDataRitiroOggiAmmessa
021900110802     D* Purpose:        Imposta l'ammissibilità della data ritiro oggi.
022000110802     D* Returns:        Esito.
022100110802     D* Parameter:      priSun => Strategi User Number
022200110802     D* Parameter:      priPagamento => Pagamento spese.
022300110802     D*--------------------------------------------------
022400110802     D SetDataRitiroOggiAmmessa...
022500110802     D                 PR            10I 0
022600110802     D  priSun                        9S 0
022700110802     D  priPagamento                  1A
022800110824     D  priCro                       10A
022900110824
023000110824     D*--------------------------------------------------
023100110824     D* Procedure name: SetIndirizzoRitiroNonConvenzionale
023200110824     D* Purpose:        Imposta il flag di ritiro ad un indirizzo non conve...
023300110824     D*                          nzionale.
023400110824     D* Returns:        Esito.
023500110824     D* Parameter:      priCro => Codice cliente ritiro.
023600110824     D*--------------------------------------------------
023700110824     D SetIndirizzoRitiroNonConvenzionale...
023800110824     D                 PR            10I 0
023900110824     D  priCro                       10A
024000110824
024100110824
024200110802
024300110729
024400110729     ***************************************************************************
024500110729     **
024600110729     ** Parametri.
024700110729     **
024800110729     ***************************************************************************
024900110729     C     *ENTRY        PLIST
025000110729     C                   PARM                    rqsDataFormat
025100110729     C                   PARM                    rqsDataSize
025200110729     C                   PARM                    rqsData
025300110729     C                   PARM                    rpyEsito
025400110729     C                   PARM                    rpyDataFormat
025500110729     C                   PARM                    rpyDataSize
025600110729     C                   PARM                    rpyData
025700110729
025800110729     ***************************************************************************
025900110729     **
026000110729     ** Main.
026100110729     **
026200110729     ***************************************************************************
026300110729
026400110729      /FREE
026500110729
026600110729       rpyEsito = Main();
026700110801       RETURN;
026800110801
026900110729      /END-FREE
027000110729
027100110729     P*--------------------------------------------------
027200110729     P* Procedure name: Main
027300110729     P* Purpose:
027400110729     P* Returns:        Esito.
027500110729     P*--------------------------------------------------
027600110729     P Main            B
027700110729     D Main            PI            10I 0
027800110729
027900110729     D retField        S             10I 0 STATIC
028000110801     D this            DS                  QUALIFIED STATIC
028100110801     D  dataRitiro                     D
028200110801     D  i                             3U 0
028300110729
028400110729      /FREE
028500110729
028600110729       CLEAR retField;
028700110801       CLEAR this;
028800110729       RESET tio10800i;
028900110729       RESET tio10800o;
029000110801       RESET parametri;
029100110729
029200110729       // Ricevo e controllo i parametri di input.
029300110729
029400110729       SELECT;
029500110729
029600110729         WHEN rqsDataFormat = tio10800i.rcdFmt;
029700110729
029800110729           tio10800i = %SUBST(rqsData : 1 : rqsDataSize);
029900110801
030000110729           retField = SetLingua(tio10800i.lingua);
030100110729
030200110729           IF retField < ESITO_OK;
030300110729             RETURN retField;
030400110729           ENDIF;
030500110729
030600110729           retField = SetGiorniLavorativi(tio10800i.ggLavorat);
030700110729
030800110729           IF retField < ESITO_OK;
030900110729             RETURN retField;
031000110729           ENDIF;
031100110729
031200110729           retField = SetDataDecorrenza(tio10800i.dtDecorr);
031300110729
031400110729           IF retField < ESITO_OK;
031500110729             RETURN retField;
031600110729           ENDIF;
031700110802
031800110802           retField = SetStrategiUserNumber(tio10800i.sun);
031900110802
032000110802           IF retField < ESITO_OK;
032100110802             RETURN retField;
032200110802           ENDIF;
032300110802
032400110802           retField = SetPagamento(tio10800i.pagamento);
032500110802
032600110802           IF retField < ESITO_OK;
032700110802             RETURN retField;
032800110802           ENDIF;
032900110802
033000110802           retField = SetDataRitiroOggiAmmessa( tio10800i.sun
033100110802                                              : tio10800i.pagamento
033200110824                                              : tio10800i.cro
033300110802                                              );
033400110802
033500110802           IF retField < ESITO_OK;
033600110802             RETURN retField;
033700110802           ENDIF;
033800110729
033900110729         OTHER;
034000110729
034100110729           RETURN ESITO_ERRORE;
034200110729
034300110729       ENDSL;
034400110801
034500110801       // Calcolo le date solo se è cambiato almeno 1 parametro di input.
034600110801
034700110803       IF parametri <> parametri_prev;
034800110729
034900110803         CLEAR dataRitiro;
035000110801
035100110801         DOU dataRitiro.i = parametri.giorniLavorativi;
035200110801           this.dataRitiro = parametri.dataDecorrenza + %DAYS(this.i);
035300110802           IF NOT IsFesta(this.dataRitiro);
035400110801             dataRitiro.i += 1;
035500110801             dataRitiro.data(dataRitiro.i) = this.dataRitiro;
035600110801             dataRitiro.descrizione(dataRitiro.i) =
035700110801                                          GetDescrizioneGiorno(this.dataRitiro);
035800110801             IF dataRitiro.i = parametri.giorniLavorativi;
035900110801               LEAVE;
036000110801             ENDIF;
036100110801           ENDIF;
036200110801           this.i += 1;
036300110801         ENDDO;
036400110729
036500110801         parametri_prev = parametri;
036600110801
036700110801       ENDIF;
036800110801
036900110801       // Imposto i parametri di output.
037000110801
037100110801       SELECT;
037200110801
037300110801         WHEN rpyDataFormat = tio10800o.rcdFmt;
037400110801
037500110801           FOR tio10800o.nrElem = 1 TO dataRitiro.i;
037600110802             %SUBST(tio10800o.dtRitiro : (tio10800o.nrElem * 8) - 7 : 8)
037700110802                             = %CHAR(dataRitiro.data(tio10800o.nrElem) : *EUR0);
037800110801             %SUBST(tio10800o.dzRitiro : (tio10800o.nrElem * 30) - 29 : 30)
037900110801                                     = dataRitiro.descrizione(tio10800o.nrElem);
038000110801           ENDFOR;
038100110801
038200110801           tio10800o.nrElem = dataRitiro.i;
038300110801           %SUBST(rpyData : 1 : rpyDataSize) = tio10800o;
038400110801
038500110801         OTHER;
038600110801
038700110801           RETURN ESITO_ERRORE;
038800110801
038900110801       ENDSL;
039000110801
039100110729       RETURN retField;
039200110729
039300110729      /END-FREE
039400110729     P Main            E
039500110729
039600110729
039700110729     P*--------------------------------------------------
039800110729     P* Procedure name: SetLingua
039900110729     P* Purpose:        Imposta la lingua.
040000110729     P* Returns:        Esito.
040100110729     P* Parameter:      priLingua
040200110729     P*--------------------------------------------------
040300110729     P SetLingua       B
040400110729     D SetLingua       PI            10I 0
040500110729     D  priLingua                     2A
040600110729
040700110729      /FREE
040800110729
040900110802       RESET parametri.lingua;
041000110729
041100110802       IF %LOOKUP(priLingua : lingue.lingua) > *ZERO;
041200110801         parametri.lingua = priLingua;
041300110729       ENDIF;
041400110729
041500110802       RETURN ESITO_OK;
041600110729
041700110729      /END-FREE
041800110729     P SetLingua       E
041900110729
042000110729
042100110729     P*--------------------------------------------------
042200110729     P* Procedure name: SetGiorniLavorativi
042300110729     P* Purpose:        Imposta i giorni lavorativi.
042400110729     P* Returns:        Esito.
042500110729     P* Parameter:      priGiorniLavorativi
042600110729     P*--------------------------------------------------
042700110729     P SetGiorniLavorativi...
042800110729     P                 B
042900110729     D SetGiorniLavorativi...
043000110729     D                 PI            10I 0
043100110729     D  priGiorniLavorativi...
043200110729     D                               10I 0 CONST
043300110729
043400110729      /FREE
043500110729
043600110802       RESET parametri.giorniLavorativi;
043700110729
043800110801       SELECT;
043900110801         WHEN priGiorniLavorativi < *ZERO;
044000110801           RETURN ESITO_ERRORE_GIORNI_LAVORATIVI;
044100110802         WHEN priGiorniLavorativi > *ZERO;
044200110801           parametri.giorniLavorativi = priGiorniLavorativi;
044300110801       ENDSL;
044400110729
044500110802       RETURN ESITO_OK;
044600110729
044700110729      /END-FREE
044800110729     P SetGiorniLavorativi...
044900110729     P                 E
045000110729
045100110729
045200110729     P*--------------------------------------------------
045300110729     P* Procedure name: SetDataDecorrenza
045400110729     P* Purpose:        Imposta la data decorrenza.
045500110729     P* Returns:        Esito.
045600110729     P* Parameter:      priDataDecorrenza => Data decorrenza formato *EUR.
045700110729     P*--------------------------------------------------
045800110729     P SetDataDecorrenza...
045900110729     P                 B
046000110729     D SetDataDecorrenza...
046100110729     D                 PI            10I 0
046200110729     D  priDataDecorrenza...
046300110802     D                               10A   CONST
046400110729
046500110729      /FREE
046600110729
046700110803       parametri.dataDecorrenza = parametri.oggi;
046800110729
046900110802       // Data corrente.
047000110729
047100110729       IF priDataDecorrenza = *BLANK;
047200110802         RETURN ESITO_OK;
047300110729       ENDIF;
047400110729
047500110802       // La data dovrebbe essere in formato *EUR,
047600110802       // ma voglio essere magnanimo e provo anche con gli altri formati.
047700110729
047800110729       MONITOR;
047900111005         parametri.dataDecorrenza = %DATE(priDataDecorrenza : *ISO);
048000110729       ON-ERROR;
048100110729         MONITOR;
048200111005           parametri.dataDecorrenza = %DATE(priDataDecorrenza : *EUR);
048300110729         ON-ERROR;
048400110729           MONITOR;
048500110801             parametri.dataDecorrenza = %DATE(priDataDecorrenza : *USA);
048600110729           ON-ERROR;
048700110729             RETURN ESITO_ERRORE_DATA_DECORRENZA;
048800110729           ENDMON;
048900110729         ENDMON;
049000110729       ENDMON;
049100110729
049200110802       // La data decorrenza non può essere passata.
049300110802
049400110803       IF parametri.dataDecorrenza < parametri.oggi;
049500110803         parametri.dataDecorrenza = parametri.oggi;
049600110802       ENDIF;
049700110802
049800110802       RETURN ESITO_OK;
049900110729
050000110729      /END-FREE
050100110729     P SetDataDecorrenza...
050200110729     P                 E
050300110729
050400110801
050500110801     P*--------------------------------------------------
050600110801     P* Procedure name: IsFesta
050700110801     P* Purpose:        Restituisce *ON se il giorno è festivo.
050800110801     P* Returns:        *ON = festa.
050900110801     P* Parameter:      priData
051000110801     P*--------------------------------------------------
051100110801     P IsFesta         B
051200110801     D IsFesta         PI              N
051300110801     D  priData                        D
051400110801
051500110801     D anno            S             10U 0 STATIC
051600110801     D mese            S             10U 0 STATIC
051700110801     D feste           S             31A   STATIC
051800110801
051900110801      /FREE
052000110801
052100110801       anno = %SUBDT(priData : *YEARS);
052200110801       mese = %SUBDT(priData : *MONTHS);
052300110801
052400110801       EXEC SQL
052500110801         SELECT clnPom
052600110801           INTO :feste
052700110801           FROM azcln00f
052800110801           WHERE clnTfp = 0 AND clnTfa = 0
052900110801             AND clnAnn = :anno AND clnMes = :mese
053000110801             AND clnAtb = ''
053100110801       ;
053200110801
053300110801       SELECT;
053400110801         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
053500110801           DUMP(A);
053600110801           RETURN *OFF;
053700110801         WHEN sqlCode < *ZERO;
053800110801           DUMP(A);
053900110801           RETURN *OFF;
054000110801       ENDSL;
054100110801
054200110801       RETURN (%SUBST(feste : %SUBDT(priData : *DAYS) : 1) = FESTA);
054300110801
054400110801      /END-FREE
054500110801     P IsFesta         E
054600110801
054700110801
054800110801     P*--------------------------------------------------
054900110801     P* Procedure name: GetDescrizioneGiorno
055000110801     P* Purpose:        Restituisce la descrizione del giorno.
055100110801     P* Returns:        Descrizione del giorno.
055200110801     P* Parameter:      priData
055300110801     P*--------------------------------------------------
055400110801     P GetDescrizioneGiorno...
055500110801     P                 B
055600110801     D GetDescrizioneGiorno...
055700110801     D                 PI           255A   VARYING
055800110801     D  priData                        D
055900110801
056000110801     D dw              S             10I 0 STATIC
056100110801
056200110801      /FREE
056300110801
056400110803       IF priData = parametri.oggi;
056500110803         RETURN %TRIMR(rtvMsgLang(MSGID_OGGI : parametri.lingua));
056600110803       ENDIF;
056700110803
056800110803       EXEC SQL SET :dw = DAYOFWEEK_ISO(:priData);
056900110803
057000110801       RETURN %CHAR(priData : *EUR) + ' '
057100110801              + %TRIMR(rtvMsgLang(dayOfWeek.msgId(dw) : parametri.lingua));
057200110801
057300110801      /END-FREE
057400110801     P GetDescrizioneGiorno...
057500110801     P                 E
057600110801
057700110802
057800110802     P*--------------------------------------------------
057900110802     P* Procedure name: SetStrategiUserNumber
058000110802     P* Purpose:
058100110802     P* Returns:        Esito.
058200110802     P* Parameter:      priSun => Strategi User Number
058300110802     P*--------------------------------------------------
058400110802     P SetStrategiUserNumber...
058500110802     P                 B
058600110802     D SetStrategiUserNumber...
058700110802     D                 PI            10I 0
058800110802     D  priSun                        9S 0
058900110802
059000110802      /FREE
059100110802
059200110803       parametri.anonimo = (priSun = *ZERO);
059300110802
059400110802       RETURN ESITO_OK;
059500110802
059600110802      /END-FREE
059700110802     P SetStrategiUserNumber...
059800110802     P                 E
059900110802
060000110802
060100110802     P*--------------------------------------------------
060200110802     P* Procedure name: SetPagamento
060300110802     P* Purpose:        Imposta il pagamento delle spese di trasporto.
060400110802     P* Returns:        Esito.
060500110802     P* Parameter:      priPagamento
060600110802     P*--------------------------------------------------
060700110802     P SetPagamento    B
060800110802     D SetPagamento    PI            10I 0
060900110802     D  priPagamento                  1A
061000110802
061100110802      /FREE
061200110802
061300110803       parametri.pagamentoMittente = (priPagamento = MITTENTE);
061400110802
061500110802       RETURN ESITO_OK;
061600110802
061700110802      /END-FREE
061800110802     P SetPagamento    E
061900110802
062000110802
062100110802     P*--------------------------------------------------
062200110802     P* Procedure name: SetDataRitiroOggiAmmessa
062300110802     P* Purpose:        Imposta l'ammissibilità della data ritiro oggi.
062400110802     P* Returns:        Esito.
062500110802     P* Parameter:      priSun => Strategi User Number
062600110802     P* Parameter:      priPagamento => Pagamento spese.
062700110802     P*--------------------------------------------------
062800110802     P SetDataRitiroOggiAmmessa...
062900110802     P                 B
063000110802     D SetDataRitiroOggiAmmessa...
063100110802     D                 PI            10I 0
063200110802     D  priSun                        9S 0
063300110802     D  priPagamento                  1A
063400110824     D  priCro                       10A
063500110802
063600110802      /FREE
063700110802
063800110825       // Non è possibile eseguire il ritiro in giornata quando ...
063900110825       // - sono passate le 12.30;
064000110825       // - sono passate le 11.00 e l'utente è anonimo;
064100110825       // - sono passate le 11.00 e il ritiro è in altra località;
064200110825       // - quando l'utente è anonimo e il pagamento è a carico del mittente.
064300110802
064400110802       IF %TIME() > T'12.30.00'
064500110825       OR %TIME() > T'11.00.00' AND (priSun = *ZERO OR priCro = *ZERO)
064600110825       OR priSun = *ZERO AND priPagamento = MITTENTE;
064700110824         SetDataDecorrenza(%CHAR(parametri.dataDecorrenza + %DAYS(1)));
064800110802         SetGiorniLavorativi(parametri.giorniLavorativi - 1);
064900110802       ENDIF;
065000110802
065100110802       RETURN ESITO_OK;
065200110802
065300110802      /END-FREE
065400110802     P SetDataRitiroOggiAmmessa...
065500110802     P                 E
065600110802
065700110824
065800110824     P*--------------------------------------------------
065900110824     P* Procedure name: SetIndirizzoRitiroNonConvenzionale
066000110824     P* Purpose:        Imposta il flag di ritiro ad un indirizzo non conve...
066100110824     P*                          nzionale.
066200110824     P* Returns:        Esito.
066300110824     P* Parameter:      priCro => Codice cliente ritiro.
066400110824     P*--------------------------------------------------
066500110824     P SetIndirizzoRitiroNonConvenzionale...
066600110824     P                 B
066700110824     D SetIndirizzoRitiroNonConvenzionale...
066800110824     D                 PI            10I 0
066900110824     D  priCro                       10A
067000110824
067100110824      /FREE
067200110824
067300110825       parametri.indirizzoRitiroNonConvenzionale = (priCro = *ZERO);
067400110824
067500110824       RETURN ESITO_OK;
067600110824
067700110824      /END-FREE
067800110824     P SetIndirizzoRitiroNonConvenzionale...
067900110824     P                 E
068000110824
