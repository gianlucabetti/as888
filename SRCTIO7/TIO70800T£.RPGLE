000100110729     ***************************************************************************
000200110729     **
000300110729     ** Questo programma riceve una data e un numero di giorni lavorativi e
000400110729     ** restituisce un elenco di date dove è possibile chiedere ritiri.
000500110729     **
000600110729     ***************************************************************************
000700110729     H DFTACTGRP(*NO)
000800110729
000900110729     ***************************************************************************
001000110729     **
001100110729     ** Costanti.
001200110729     **
001300110729     ***************************************************************************
001400110801     D ESITO_OK...
001500110801     D                 C                   0
001600110729     D ESITO_ERRORE...
001700110729     D                 C                   -1
001800110729     D ESITO_ERRORE_GIORNI_LAVORATIVI...
001900110729     D                 C                   -2
002000110729     D ESITO_ERRORE_DATA_DECORRENZA...
002100110729     D                 C                   -3
002200110729     D LINGUA_FRANCESE...
002300110729     D                 C                   'fr'
002400110729     D LINGUA_INGLESE...
002500110729     D                 C                   'en'
002600110729     D LINGUA_ITALIANO...
002700110729     D                 C                   'it'
002800110729     D LINGUA_TEDESCO...
002900110729     D                 C                   'de'
003000110729
003100110729     ***************************************************************************
003200110729     **
003300110729     ** Strutture dati.
003400110729     **
003500110729     ***************************************************************************
003600110801     D dataRitiro...
003700110801     D                 DS                  QUALIFIED
003800110801     D  data                           D   DIM(99) DATFMT(*EUR)
003900110801     D  i                             3U 0
004000110729     D lingue          DS
004100110729     D  de                            2A   INZ(LINGUA_TEDESCO)
004200110729     D  en                            2A   INZ(LINGUA_INGLESE)
004300110729     D  fr                            2A   INZ(LINGUA_FRANCESE)
004400110729     D  it                            2A   INZ(LINGUA_ITALIANO)
004500110729     D  lingua                        2A   DIM(4) ASCEND OVERLAY(lingue)
004600110729     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT)
004700110729     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT)
004800110801     D parametri       DS                  QUALIFIED INZ
004900110801     D  lingua...
005000110801     D                                2A   INZ(LINGUA_ITALIANO)
005100110801     D  giorniLavorativi...
005200110801     D                                3U 0
005300110801     D  dataDecorrenza...
005400110801     D                                 D   INZ(*SYS)
005500110801     D parametri_prev  DS                  LIKEDS(parametri)
005600110801     D xsrLav8ds       DS                  QUALIFIED INZ
005700110801     D  daData                        8S 0
005800110801     D  aData                         8S 0
005900110801     D  giorniLavorativi...
006000110801     D                                5S 0
006100110729
006200110729     ***************************************************************************
006300110729     **
006400110729     ** Campi.
006500110729     **
006600110729     ***************************************************************************
006700110801     D i               S             10I 0
006800110729     D rqsDataFormat   S             10A
006900110729     D rqsDataSize     S             10I 0
007000110729     D rqsData         S                   LIKE(tio10800i)
007100110729     D rpyEsito        S             10I 0
007200110729     D rpyDataFormat   S             10A
007300110729     D rpyDataSize     S             10I 0
007400110729     D rpyData         S                   LIKE(tio10800o)
007500110729
007600110729     ***************************************************************************
007700110729     **
007800110729     ** Prototipi.
007900110729     **
008000110729     ***************************************************************************
008100110729     D GetGiorniLavorativiTra2Date...
008200110729     D                 PR                  EXTPGM('XSRLAV8')
008300110729     D  xsrLav8ds                          LIKEDS(xsrLav8ds)
008400110729
008500110729     D*--------------------------------------------------
008600110729     D* Procedure name: Main
008700110729     D* Purpose:
008800110729     D* Returns:        Esito.
008900110729     D*--------------------------------------------------
009000110729     D Main            PR            10I 0
009100110729
009200110729     D*--------------------------------------------------
009300110729     D* Procedure name: SetLingua
009400110729     D* Purpose:        Imposta la lingua.
009500110729     D* Returns:        Esito.
009600110729     D* Parameter:      priLingua
009700110729     D*--------------------------------------------------
009800110729     D SetLingua       PR            10I 0
009900110729     D  priLingua                     2A
010000110729
010100110729     D*--------------------------------------------------
010200110729     D* Procedure name: SetGiorniLavorativi
010300110729     D* Purpose:        Imposta i giorni lavorativi.
010400110729     D* Returns:        Esito.
010500110729     D* Parameter:      priGiorniLavorativi
010600110729     D*--------------------------------------------------
010700110729     D SetGiorniLavorativi...
010800110729     D                 PR            10I 0
010900110729     D  priGiorniLavorativi...
011000110729     D                               10I 0 CONST
011100110729
011200110729     D*--------------------------------------------------
011300110729     D* Procedure name: SetDataDecorrenza
011400110729     D* Purpose:        Imposta la data decorrenza.
011500110729     D* Returns:        Esito.
011600110729     D* Parameter:      priDataDecorrenza => Data decorrenza formato *EUR.
011700110729     D*--------------------------------------------------
011800110729     D SetDataDecorrenza...
011900110729     D                 PR            10I 0
012000110729     D  priDataDecorrenza...
012100110729     D                               10A
012200110729
012300110729
012400110729     ***************************************************************************
012500110729     **
012600110729     ** Parametri.
012700110729     **
012800110729     ***************************************************************************
012900110729     C     *ENTRY        PLIST
013000110729     C                   PARM                    rqsDataFormat
013100110729     C                   PARM                    rqsDataSize
013200110729     C                   PARM                    rqsData
013300110729     C                   PARM                    rpyEsito
013400110729     C                   PARM                    rpyDataFormat
013500110729     C                   PARM                    rpyDataSize
013600110729     C                   PARM                    rpyData
013700110729
013800110729     ***************************************************************************
013900110729     **
014000110729     ** Main.
014100110729     **
014200110729     ***************************************************************************
014300110729
014400110729      /FREE
014500110729
014600110729       rpyEsito = Main();
014700110801       RETURN;
014800110801
014900110729      /END-FREE
015000110729
015100110729     P*--------------------------------------------------
015200110729     P* Procedure name: Main
015300110729     P* Purpose:
015400110729     P* Returns:        Esito.
015500110729     P*--------------------------------------------------
015600110729     P Main            B
015700110729     D Main            PI            10I 0
015800110729
015900110729     D retField        S             10I 0 STATIC
016000110729
016100110729      /FREE
016200110729
016300110729       CLEAR retField;
016400110729       RESET tio10800i;
016500110729       RESET tio10800o;
016600110801       RESET parametri;
016700110729
016800110729       // Ricevo e controllo i parametri di input.
016900110729
017000110729       SELECT;
017100110729
017200110729         WHEN rqsDataFormat = tio10800i.rcdFmt;
017300110729
017400110729           tio10800i = %SUBST(rqsData : 1 : rqsDataSize);
017500110801
017600110729           retField = SetLingua(tio10800i.lingua);
017700110729
017800110729           IF retField < ESITO_OK;
017900110729             RETURN retField;
018000110729           ENDIF;
018100110729
018200110729           retField = SetGiorniLavorativi(tio10800i.ggLavorat);
018300110729
018400110729           IF retField < ESITO_OK;
018500110729             RETURN retField;
018600110729           ENDIF;
018700110729
018800110729           retField = SetDataDecorrenza(tio10800i.dtDecorr);
018900110729
019000110729           IF retField < ESITO_OK;
019100110729             RETURN retField;
019200110729           ENDIF;
019300110729
019400110729         OTHER;
019500110729
019600110729           RETURN ESITO_ERRORE;
019700110729
019800110729       ENDSL;
019900110801
020000110801       // Calcolo le date solo se è cambiato almeno 1 parametro di input.
020100110801
020200110801       IF parametri_prev.lingua <> parametri.lingua
020300110801       OR parametri_prev.giorniLavorativi <> parametri.giorniLavorativi
020400110801       OR parametri_prev.dataDecorrenza <> parametri.dataDecorrenza;
020500110729
020600110801         CLEAR dataRitiro;
020700110801
020800110801         FOR dataRitiro.i = 1 TO 99;
020900110801           CLEAR xsrLav8ds;
021000110801           xsrLav8ds.daData = %DEC(parametri.dataDecorrenza);
021100110801           xsrLav8ds.aData = %DEC(parametri.dataDecorrenza + %DAYS(i - 1));
021200110801           GetGiorniLavorativiTra2Date(xsrLav8ds);
021300110801           IF xsrLav8ds.giorniLavorativi > parametri.giorniLavorativi;
021400110801             LEAVE;
021500110801           ENDIF;
021600110801           dataRitiro.data(dataRitiro.i) = %DATE(xsrLav8ds.aData : *ISO);
021700110801         ENDFOR;
021800110729
021900110801       ENDIF;
022000110801
022100110801       // Imposto i parametri di output.
022200110801
022300110801       SELECT;
022400110801
022500110801         WHEN rpyDataFormat = tio10800o.rcdFmt;
022600110801
022700110801           FOR tio10800o.nrElem = 1 TO dataRitiro.i;
022800110801             %SUBST(tio10800o.dtRitiro : (tio10800o.nrElem * 10) - 9 : 10)
022900110801                                     = %CHAR(dataRitiro.data(tio10800o.nrElem));
023000110801           ENDFOR;
023100110801
023200110801           tio10800o.nrElem = dataRitiro.i;
023300110801           %SUBST(rpyData : 1 : rpyDataSize) = tio10800o;
023400110801
023500110801         OTHER;
023600110801
023700110801           RETURN ESITO_ERRORE;
023800110801
023900110801       ENDSL;
024000110801
024100110729       RETURN retField;
024200110729
024300110729      /END-FREE
024400110729     P Main            E
024500110729
024600110729
024700110729     P*--------------------------------------------------
024800110729     P* Procedure name: SetLingua
024900110729     P* Purpose:        Imposta la lingua.
025000110729     P* Returns:        Esito.
025100110729     P* Parameter:      priLingua
025200110729     P*--------------------------------------------------
025300110729     P SetLingua       B
025400110729     D SetLingua       PI            10I 0
025500110729     D  priLingua                     2A
025600110729
025700110729     D retField        S             10I 0 STATIC
025800110729
025900110729      /FREE
026000110729
026100110729       RESET retField;
026200110729
026300110801       IF %LOOKUP(priLingua : lingue.lingua) > *ZERO;
026400110801         parametri.lingua = priLingua;
026500110729       ENDIF;
026600110729
026700110729       RETURN retField;
026800110729
026900110729      /END-FREE
027000110729     P SetLingua       E
027100110729
027200110729
027300110729     P*--------------------------------------------------
027400110729     P* Procedure name: SetGiorniLavorativi
027500110729     P* Purpose:        Imposta i giorni lavorativi.
027600110729     P* Returns:        Esito.
027700110729     P* Parameter:      priGiorniLavorativi
027800110729     P*--------------------------------------------------
027900110729     P SetGiorniLavorativi...
028000110729     P                 B
028100110729     D SetGiorniLavorativi...
028200110729     D                 PI            10I 0
028300110729     D  priGiorniLavorativi...
028400110729     D                               10I 0 CONST
028500110729
028600110729     D retField        S             10I 0 STATIC
028700110729
028800110729      /FREE
028900110729
029000110729       RESET retField;
029100110729
029200110729       IF priGiorniLavorativi < *ZERO;
029300110729         RETURN ESITO_ERRORE_GIORNI_LAVORATIVI;
029400110729       ENDIF;
029500110729
029600110801       parametri.giorniLavorativi = priGiorniLavorativi;
029700110729
029800110729       RETURN retField;
029900110729
030000110729      /END-FREE
030100110729     P SetGiorniLavorativi...
030200110729     P                 E
030300110729
030400110729
030500110729     P*--------------------------------------------------
030600110729     P* Procedure name: SetDataDecorrenza
030700110729     P* Purpose:        Imposta la data decorrenza.
030800110729     P* Returns:        Esito.
030900110729     P* Parameter:      priDataDecorrenza => Data decorrenza formato *EUR.
031000110729     P*--------------------------------------------------
031100110729     P SetDataDecorrenza...
031200110729     P                 B
031300110729     D SetDataDecorrenza...
031400110729     D                 PI            10I 0
031500110729     D  priDataDecorrenza...
031600110729     D                               10A
031700110729
031800110729     D retField        S             10I 0 STATIC
031900110729
032000110729      /FREE
032100110729
032200110729       RESET retField;
032300110729
032400110729       // Data corrente.
032500110729
032600110729       IF priDataDecorrenza = *BLANK;
032700110729         RETURN retField;
032800110729       ENDIF;
032900110729
033000110729       // La data dovrebbe essere in formato *EUR, ma voglio essere magnanimo
033100110729       // e provo anche con gli altri formati.
033200110729
033300110729       MONITOR;
033400110801         parametri.dataDecorrenza = %DATE(priDataDecorrenza : *EUR);
033500110729       ON-ERROR;
033600110729         MONITOR;
033700110801           parametri.dataDecorrenza = %DATE(priDataDecorrenza : *ISO);
033800110729         ON-ERROR;
033900110729           MONITOR;
034000110801             parametri.dataDecorrenza = %DATE(priDataDecorrenza : *USA);
034100110729           ON-ERROR;
034200110729             RETURN ESITO_ERRORE_DATA_DECORRENZA;
034300110729           ENDMON;
034400110729         ENDMON;
034500110729       ENDMON;
034600110729
034700110729       RETURN retField;
034800110729
034900110729      /END-FREE
035000110729     P SetDataDecorrenza...
035100110729     P                 E
035200110729
