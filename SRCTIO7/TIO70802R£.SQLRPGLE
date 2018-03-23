000100110729     ***************************************************************************
000200110729     **
000201170616     ** Autore: Danilo Cussini
000202170616     **
000300160118     ** Questo programma riceve una data e
000400110729     ** restituisce un elenco di date dove è possibile chiedere ritiri.
000500110729     **
000600110729     ***************************************************************************
000601160118
000700140408     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIBS':'TIS')
000701160526     H/COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000800110729
000900110729     ***************************************************************************
001000110729     **
001100110729     ** Costanti.
001200110729     **
001300110729     ***************************************************************************
001301160118
001400110802     D GIORNI_LAVORATIVI_DFT...
001500160118     D                 C                   '10'
001600110801     D ESITO_OK...
001700110801     D                 C                   0
001800110729     D ESITO_ERRORE...
001900110729     D                 C                   -1
002000110729     D ESITO_ERRORE_GIORNI_LAVORATIVI...
002100110729     D                 C                   -2
002200110729     D ESITO_ERRORE_DATA_DECORRENZA...
002300110729     D                 C                   -3
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
005101140407     D ORM_PREPAGATO...
005102140407     D                 C                   'P'
005103140407     D SI...
005104140407     D                 C                   'S'
005200110801     D SQLCODE_ROW_NOT_FOUND...
005300110801     D                 C                   100
005301140407     D VERSIONE_A...
005302140407     D                 C                   'A'
005303140407     D VERSIONE_B...
005304140407     D                 C                   'B'
005400110729
005500110729     ***************************************************************************
005600110729     **
005700110729     ** Strutture dati.
005800110729     **
005900110729     ***************************************************************************
005901160118
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
007400160118     D dDft          E DS                  QUALIFIED INZ
007401160118     D  tbeUni                      256A   OVERLAY(dDft)
007402170616     D lingue          DS                  QUALIFIED
007500110801     D                                2A   INZ(LINGUA_TEDESCO)
007600110801     D                                2A   INZ(LINGUA_INGLESE)
007700110801     D                                2A   INZ(LINGUA_FRANCESE)
007800110801     D                                2A   INZ(LINGUA_ITALIANO)
007900110729     D  lingua                        2A   DIM(4) ASCEND OVERLAY(lingue)
008004140407     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT)
008100110729     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT)
008200110801     D parametri       DS                  QUALIFIED INZ
008300110801     D  lingua...
008400110802     D                                2A   INZ(LINGUA_ITALIANO)
008500110803     D  pagamentoMittente...
008600110803     D                                 N
008700110803     D  anonimo...
008800110803     D                                 N
008900110801     D  giorniLavorativi...
009000160118     D                                3U 0
009100110801     D  dataDecorrenza...
009200110802     D                                 D
009201140508     D  dataRitiroFocus...
009202140508     D                                 D
009300110803     D  oggi...
009400110803     D                                 D   INZ(*SYS)
009500110824     D  indirizzoRitiroNonConvenzionale...
009600110824     D                                 N
009601140407     D  capRitiro...
009602140407     D                                9A
009603140407     D  localitaRitiro...
009604140407     D                               35A
009605140407     D  ormCommissionato...
009606140407     D                                 N
009700110802     D parametri_prev  DS                  LIKEDS(parametri) INZ
009701160531     D wDat8           DS                  INZ QUALIFIED
009702160531     D  dataDa                 1      8  0
009703160531     D  dataA                  9     16  0
009704160531     D  ggl                   17     21  0
009705160531     D tfptfa          DS                  INZ QUALIFIED
009706160531     D  p_tfp                  1      3  0
009707160531     D  p_tfa                  4      6  0
009800110729
009900110729     ***************************************************************************
010000110729     **
010100110729     ** Campi.
010200110729     **
010300110729     ***************************************************************************
010301160118
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
011601160118
011700140408      /COPY GAITRASRC/SRCPROTOPR,RTVMSGLANG
011701140408      /COPY GAITRASRC/SRCPROTOPR,TIBS0800R
011702160531      /COPY GAITRASRC/SRCPROTOPR,XSRLAV8
011703160526
011704160526     ***************************************************************************
011705160526     **
011706160526     ** Opzioni SQL.
011707160526     **
011708160526     ***************************************************************************
011709160526
011710160526       EXEC SQL SET OPTION SQLCA = *NO;
024312140407
024400110729     ***************************************************************************
024500110729     **
024600110729     ** Parametri.
024700110729     **
024800110729     ***************************************************************************
024801160118
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
027001160118      //************************************************************************
027002160118      //
027003160118      // Inizializzazioni.
027004160118      //
027005160118      //************************************************************************
027006160118
027007160118       BEGSR *INZSR;
027008160118
027009160118         EXEC SQL
027010160118           SELECT tbeUni
027011160118             INTO :dDft.tbeUni
027012160118             FROM tntbe00f
027013160118            WHERE tbeCod = 'DFT'
027014160118              AND tbeKe1 = '999'
027015160118              AND tbeKe2 = ''
027016160118              AND tbeLin = ''
027017160118              AND tbeSif = ''
027018160118         ;
027019160118
027020160118         SELECT;
027021160118           WHEN sqlCode < *ZERO;
027022160118             DUMP(A);
027023160118             dDft.d§dftGg1 = GIORNI_LAVORATIVI_DFT;
027024160118           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
027025160118             dDft.d§dftGg1 = GIORNI_LAVORATIVI_DFT;
027026160118         ENDSL;
027027160118
027028160118       ENDSR;
027029160118
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
030600160118           retField = SetGiorniLavorativi(%INT(dDft.d§dftGg1));
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
032901140407           IF tio10800i.versione > VERSIONE_A;
032902140407
032903140407             retField = SetLuogoRitiro(tio10800i.car : tio10800i.lor);
032904140407
032905140407             IF retField < ESITO_OK;
032906140407               RETURN retField;
032907140407             ENDIF;
032908140407
032909140407           ENDIF;
032910140407
032911140407           IF tio10800i.versione > VERSIONE_A;
032912140407
032913140407             retField = SetOrmCommissionato(tio10800i.cttTlf);
032914140407
032915140407             IF retField < ESITO_OK;
032916140407               RETURN retField;
032917140407             ENDIF;
032918140407
032919140407           ENDIF;
032920140407
032921140407           SELECT;
032922140407             WHEN tio10800i.versione = VERSIONE_A;
032923140407               retField = SetDataRitiroOggiAmmessa( tio10800i.sun
032924140407                                                  : tio10800i.pagamento
032925140407                                                  : tio10800i.cro
032926140407                                                  );
032927140407             WHEN tio10800i.versione = VERSIONE_B;
033000140407               retField = SetDataRitiroOggiAmmessa( tio10800i.sun
033100140407                                                  : tio10800i.pagamento
033200140407                                                  : tio10800i.cro
033201140407                                                  : tio10800i.poe
033202140407                                                  : tio10800i.por
033203140407                                                  : tio10800i.car
033204140407                                                  : tio10800i.lor
033205140407                                                  : tio10800i.cttTlf
033206140407                                                  : tio10800i.lingua
033207140508                                                  : tio10800i.avrEma
033208140508                                                  : tio10800i.avrSms
033209160526                                                  : tio10800i.nar
033300140407                                                  );
033314140407           ENDSL;
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
038201140508           tio10800o.dtRitFocus = %CHAR(parametri.dataRitiroFocus : *EUR0);
038202140508
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
051500160531     D*anno            S             10U 0 STATIC
051600160531     D*mese            S             10U 0 STATIC
051700160531     D*feste           S             31A   STATIC
051702160531     D*FESTA           C                   'F'
051807160531
051900110801      /FREE
052002160531
052003160531       RESET wDat8;
052004160531       wDat8.dataDa = %DEC(priData : *ISO);
052005160531       wDat8.dataA = wDat8.dataDa;
052006160531       RESET tfptfa;
052007160531       tfptfa.p_tfa = tio10800i.por;
052008160531       XsrLav8(wDat8 : tfptfa);
052009160531       RETURN (wDat8.ggl = *ZERO);
052010160531
052100160531       //anno = %SUBDT(priData : *YEARS);
052200160531       //mese = %SUBDT(priData : *MONTHS);
052300160531       //
052400160531       //EXEC SQL
052500160531       //  SELECT clnPom
052600160531       //    INTO :feste
052700160531       //    FROM azcln00f
052800160531       //    WHERE clnTfp = 0 AND clnTfa = :tio10800i.por
052900160531       //      AND clnAnn = :anno AND clnMes = :mese
053000160531       //      AND clnAtb = ''
053100160531       //;
053200160531       //
053300160531       //SELECT;
053400160531       //  WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
053500160531       //    DUMP(A);
053600160531       //    RETURN *OFF;
053700160531       //  WHEN sqlCode < *ZERO;
053800160531       //    DUMP(A);
053900160531       //    RETURN *OFF;
054000160531       //ENDSL;
054100160531       //
054200160531       //RETURN (%SUBST(feste : %SUBDT(priData : *DAYS) : 1) = FESTA);
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
063401140407     D  priPoe                        3S 0 OPTIONS(*NOPASS)
063402140407     D  priPor                        3S 0 OPTIONS(*NOPASS)
063404140407     D  priCar                        9A   OPTIONS(*NOPASS)
063405140407     D  priLor                       35A   OPTIONS(*NOPASS)
063406140407     D  priCttTlf                     1A   OPTIONS(*NOPASS)
063407140407     D  priLingua                     2A   OPTIONS(*NOPASS)
063408140508     D  priAvrEma                     1A   OPTIONS(*NOPASS)
063409140508     D  priAvrSms                     1A   OPTIONS(*NOPASS)
063410160526     D  priNar                        3A   OPTIONS(*NOPASS)
063500110802
063501140407     D fior97ds      E DS                  QUALIFIED INZ STATIC
063502140407     D  ior97web     E                     INZ(SI)
063503170616     D fior971ds     E DS                  QUALIFIED INZ STATIC
063504140407     D kpjba         E DS                  QUALIFIED INZ STATIC
063505140407     D  knsif        E                     INZ('GAITRA201')
063506140407     D local           DS                  QUALIFIED INZ STATIC
063507140407     D  curTimeStamp                   Z
063508140407     D Fior97r         PR                  EXTPGM('FIOR97R')
063509140407     D  kpjba                              LIKEDS(kpjba)
063510140407     D  fior97ds                           LIKEDS(fior97ds)
063511170616     D  fior971ds                          LIKEDS(fior971ds)
063512140407
063600110802      /FREE
063700110802
063713140407       IF %PARMS() < 4;
063714140407
063800140407         // Non è possibile eseguire il ritiro in giornata quando ...
063900140407         // - sono passate le 12.30;
064000140407         // - sono passate le 11.00 e l'utente è anonimo;
064100140407         // - sono passate le 11.00 e il ritiro è in altra località;
064200140407         // - quando l'utente è anonimo e il pagamento è a carico del mittente.
064300110802
064400140407         IF %TIME() > T'12.30.00'
064500140407         OR %TIME() > T'11.00.00' AND (priSun = *ZERO OR priCro = *ZERO)
064600140407         OR priSun = *ZERO AND priPagamento = MITTENTE;
064700140407           SetDataDecorrenza(%CHAR(parametri.dataDecorrenza + %DAYS(1)));
064800160118           SetGiorniLavorativi(%INT(dDft.d§dftGg1) - 1);
064900140407         ENDIF;
065000110802
065001140407         RETURN ESITO_OK;
065002140407
065003140407       ENDIF;
065005140407
065006140407       // ORM 2.0 Nuovo controllo data ritiro corrente.
065007140407
065008140407       RESET kpjba;
065009140407       RESET fior97ds;
065010170616       RESET fior971ds;
065011140407
065012140407       fior97ds.ior97poe = priPoe;
065013140407       fior97ds.ior97por = priPor;
065014140407       fior97ds.ior97cap = priCar;
065015140407       fior97ds.ior97loc = priLor;
065016160526       fior97ds.ior97naz = priNar;
065017140407       local.curTimeStamp = %TIMESTAMP();
065018140407       fior97ds.ior97dao = %DEC(%DATE(local.curTimeStamp) : *ISO);
065019140407       fior97ds.ior97oao = %DEC(%TIME(local.curTimeStamp) : *ISO);
065020140408       fior97ds.ior97lin = CvtLinguaISO2ToTntbe(priLingua);
065021140408
065022160314       IF priCttTlf = SI;
065023140407         fior97ds.ior97com = SI;
065024140407       ENDIF;
065025140407
065026140408       IF priPagamento = MITTENTE AND priSun = *ZERO;
065027140407         fior97ds.ior97tor = ORM_PREPAGATO;
065028140407       ENDIF;
065029140407
065030140408       IF priCro <> *BLANK AND priCro <> *ZERO;
065031140407         fior97ds.ior97mCod = SI;
065032140407       ENDIF;
065033140508
065034140508       IF priAvrEma = SI OR priAvrSms = SI;
065035140508         fior97ds.ior97alert = SI;
065036140508       ENDIF;
065037140407
065038170616       IF tio10800i.sun <> *ZERO;
065039170616         fior971ds.ior97ocod = SI;
065040170616       ENDIF;
065041170616
065042170616       // Data pronta merce: per ora ci metto la data corrente,
065043170616       // poi la imposterò con il valore ricevuto dalla pagina web (progetto 867 punto B).
065044170616
065045170616       fior971ds.ior97dpm = %DEC(%DATE:*ISO);
065046170616
065047140407       MONITOR;
065048170616         Fior97r( kpjba : fior97ds : fior971ds );
065049140407       ON-ERROR;
065050140407         fior97ds.oor97err = *ON;
065051140407       ENDMON;
065052140407
065053140407       IF fior97ds.oor97err = *ON;
065054140407         RETURN ESITO_ERRORE;
065055140407       ENDIF;
065056140407
065057140407       // Oggi non posso ritirare.
065058140407
065059140407       IF fior97ds.oor97dMin > %DEC(%DATE(local.curTimeStamp) : *ISO);
065060140407         SetDataDecorrenza(%CHAR(parametri.dataDecorrenza + %DAYS(1)));
065061160118         SetGiorniLavorativi(%INT(dDft.d§dftGg1) - 1);
065062140407       ENDIF;
065063140508
065064140508       // Data ritiro su cui mettere il focus.
065065140508
065066140508       parametri.dataRitiroFocus = %DATE(fior97ds.oor97dar : *ISO);
065067140407
065100140508       RETURN ESITO_OK;
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
068001140407
068002140407     P*--------------------------------------------------
068003140407     P* Procedure name: SetLuogoRitiro
068004140407     P* Purpose:        Imposta il luogo di ritiro.
068005140407     P* Returns:        Esito.
068006140407     P* Parameter:      priCap
068007140407     P* Parameter:      priLocalita
068008140407     P*--------------------------------------------------
068009140407     P SetLuogoRitiro  B
068010140407     D SetLuogoRitiro  PI            10I 0
068011140407     D  priCapRitiro                  9A
068012140407     D  priLocalitaRitiro...
068013140407     D                               35A
068017140407
068018140407      /FREE
068019140407
068020140407       parametri.capRitiro = priCapRitiro;
068021140407       parametri.localitaRitiro = priLocalitaRitiro;
068022140407
068023140407       RETURN ESITO_OK;
068025140407
068026140407      /END-FREE
068027140407     P SetLuogoRitiro  E
068028140407
068029140407
068030140407     P*--------------------------------------------------
068031140407     P* Procedure name: SetOrmCommissionato
068032140407     P* Purpose:        Imposta il flag di ORM commissionato.
068033140407     P* Returns:        Esito.
068034140407     P* Parameter:      priContattoTelefonicoPreventivo
068035140407     P*--------------------------------------------------
068036140407     P SetOrmCommissionato...
068037140407     P                 B
068038140407     D SetOrmCommissionato...
068039140407     D                 PI            10I 0
068040140407     D  priContattoTelefonicoPreventivo...
068041140407     D                                1A
068046140407
068047140407      /FREE
068048140407
068049140407       parametri.ormCommissionato = (priContattoTelefonicoPreventivo = SI);
068050140407
068051140407       RETURN ESITO_OK;
068055140407
068056140407      /END-FREE
068057140407     P SetOrmCommissionato...
068058140407     P                 E
068059140407
