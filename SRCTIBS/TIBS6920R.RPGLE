000100070215      //********************************************************************************************
000200070215      //
000300070216      // Questo programma restituisce l'indirizzo a cui inviare la fattura (nota di accredito).
000400070216      // Funziona in tutte le società, sia per i clienti che per i fornitori.
000500070216      // Per i clienti della società Bartolini l'indirizzo è reperito da FNSPE00F luogo '001';
000600070216      // in tutti gli altri casi dall'anagrafica di Proj tipo indirizzo 'IF'.
000700070215      // Se l'indirizzo di spedizione della fattura non esiste, restituisce la sede legale.
000800070215      //
000900070216      // PARAMETRI DI INPUT
001000070216      // ------------------------------ ------------------------------------------------------------
001100070216      // ID società                     ' ' = '201'
001200070216      //
001300070216      // Sotto natura                   'C' = Cliente; 'F' = Fornitore; ' ' = 'C'.
001400070216      //
001500070216      // ID cliente/fornitore           Obbligatorio.
001600070216      //                                Il parametro è di 8 caratteri in formato Proj '0nnnnnnn',
001700070216      //                                ma può essere impostato anche nel formato 'nnnnnnn '
001800070216      //                                oppure ' nnnnnnn'.
001900070216      //
002000070216      // Data di riferimento            E' la data con cui viene effettuato l'eventuale controllo
002100070216      //                                di inizio e fine validità dei records.
002200070216      //                                '0001-01-01' = data corrente.
002300070216      //
002400070216      // ID partita contabile           Data, numero e serie partita contabile da cui reperire
002500070216      //                                l'anagrafica dei clienti Bartolini non codificati.
002600070216      //
002700070216      // PARAMETRI DI OUTPUT
002800070216      // ------------------------------ ------------------------------------------------------------
002900070216      // Esito                          0 = Eseguito; Negativo = Errore.
003000070216      //
003100070216      // Codice operativo               'DONE      ' Elaborazione eseguita con successo.
003200070216      //                                'INFO      ' Elaborazione eseguita con successo.
003300070216      //                                             Il parametro status contiene l'ID
003400070216      //                                             informazione aggiuntiva.
003500070216      //                                'ERRORE    ' Si è verificato un errore applicativo.
003600070216      //                                             Il parametro status contiene l'ID errore.
003700070216      //                                'STATUS    ' Si è verificato un errore tecnico.
003800070216      //                                             Il parametro status contiene lo stato del
003900070216      //                                             programma o del file.
004000070216      //
004100070216      // Status                         Assume diversi significati a seconda del codice operativo.
004200070216      // Ragione sociale
004300070216      // Descrizione aggiuntiva
004400070216      // Indirizzo
004500070216      // CAP
004600070216      // Località
004700070216      // Sigla provincia
004800070216      // Sigla nazione
004900070216      //
005000070215      //********************************************************************************************
005100070215     H DFTACTGRP(*NO) ACTGRP(*CALLER)
005200070214
005300070214      //********************************************************************************************
005400070214      //
005500070214      // Definizione file database.
005600070214      //
005700070214      //********************************************************************************************
005800070214     Ffnspe03l  IF   E           K DISK    USROPN
005900070214
006000070214      //********************************************************************************************
006100070214      //
006200070214      // Definizione costanti.
006300070214      //
006400070214      //********************************************************************************************
006500070215     D BARTOLINI...
006600070215     D                 C                   '201'
006700070215     D CLIENTE...
006800070215     D                 C                   'C'
006900070215     D FORNITORE...
007000070215     D                 C                   'F'
007100070214     D LUOGO...
007200070214     D                 C                   'L'
007300070214     D LUOGO_SPEDIZIONE_FATTURA...
007400070214     D                 C                   '001'
007500070215     D RECORD_ATTIVO...
007600070215     D                 C                   *BLANK
007700070215     D COGE...
007800070215     D                 C                   'CG'
007900070215     D VOGLIO_CNACO00F...
008000070215     D                 C                   *ON
008100070215     D VOGLIO_CNIND00F...
008200070215     D                 C                   *ON
008300070215     D NON_VOGLIO_CNCLP00F...
008400070215     D                 C                   *OFF
008500070215     D NON_VOGLIO_FNCLS00F...
008600070215     D                 C                   *OFF
008700070215     D CHAIN...
008800070215     D                 C                   '1'
008900070214     D TIPO_INDIRIZZO_FATTURA...
009000070214     D                 C                   'IF'
009100070214     D CODICE_INDIRIZZO_FATTURA...
009200070214     D                 C                   '000'
009300070215     D TIPO_INDIRIZZO_SEDELEGALE...
009400070215     D                 C                   *BLANK
009500070215     D CODICE_INDIRIZZO_SEDELEGALE...
009600070215     D                 C                   *BLANK
009700070215     D ALLINEAMENTO_DESTRA_RIEMPIMENTO_ZERI...
009800070215     D                 C                   '1'
009900070215     D ESITO_ERRORE...
010000070215     D                 C                   -1
010100070216     D CODICE_OPERATIVO_ELABORAZIONE_ESEGUITA...
010200070216     D                 C                   'DONE'
010300070216     D CODICE_OPERATIVO_INFORMAZIONE...
010400070216     D                 C                   'INFO'
010500070216     D CODICE_OPERATIVO_ERRORE...
010600070216     D                 C                   'ERRORE'
010700070216     D CODICE_OPERATIVO_STATUS...
010800070216     D                 C                   'STATUS'
010900070215     D ERRORE_PARAMETRO_SOCIETA...
011000070215     D                 C                   1
011100070215     D ERRORE_PARAMETRO_SOTTONATURA...
011200070215     D                 C                   2
011300070215     D ERRORE_PARAMETRO_SOTTOCONTO...
011400070215     D                 C                   3
011500070215     D ERRORE_SOTTOCONTO_INESISTENTE...
011600070215     D                 C                   4
011700070215     D INFO_INDIRIZZO_SPEDIZIONE_FATTURA...
011800070215     D                 C                   1
011900070215     D INFO_INDIRIZZO_SEDE_LEGALE...
012000070215     D                 C                   2
012100070215
012200070215      //********************************************************************************************
012300070215      //
012400070215      // Definizione interfaccia procedura.
012500070215      //
012600070215      //********************************************************************************************
012700070215     D tibs6920r       PR                  EXTPGM('TIBS6920R')
012800070215     D  rqsSocieta                         LIKE(pjzSocieta)
012900070215     D  rqsSNatura                         LIKE(pjzSNatura)
013000070215     D  rqsKsc                             LIKE(pjzKsc)
013100070215     D  rqsDtRif                           LIKE(pjzData)
013200070215     D  rqsDtPar                           LIKE(pjzData)
013300070215     D  rqsNrPar                           LIKE(pjzNr)
013400070215     D  rqsSerPar                          LIKE(pjzSerieNm)
013500070215     D  rpyEsito                     10I 0
013600070215     D  rpyOpCode                    10A
013700070215     D  rpyStatus                    10I 0
013800070215     D  rpyRagioneSociale...
013900070215     D                                     LIKE(pjzDescr)
014000070215     D  rpyDescrizioneAggiuntiva...
014100070215     D                                     LIKE(pjzDescr)
014200070215     D  rpyIndirizzo...
014300070215     D                                     LIKE(pjzIndiriz)
014400070215     D  rpyCap...
014500070215     D                                     LIKE(pjzCap)
014600070215     D  rpyLocalita...
014700070215     D                                     LIKE(diz$Loc)
014800070215     D  rpySiglaProvincia...
014900070215     D                                     LIKE(pjzProv)
015000070215     D  rpySiglaNazione...
015100070215     D                                     LIKE(diz$Naz)
015200070215     D tibs6920r       PI
015300070215     D  rqsSocieta                         LIKE(pjzSocieta)
015400070215     D  rqsSNatura                         LIKE(pjzSNatura)
015500070215     D  rqsKsc                             LIKE(pjzKsc)
015600070215     D  rqsDtRif                           LIKE(pjzData)
015700070215     D  rqsDtPar                           LIKE(pjzData)
015800070215     D  rqsNrPar                           LIKE(pjzNr)
015900070215     D  rqsSerPar                          LIKE(pjzSerieNm)
016000070215     D  rpyEsito                     10I 0
016100070215     D  rpyOpCode                    10A
016200070215     D  rpyStatus                    10I 0
016300070215     D  rpyRagioneSociale...
016400070215     D                                     LIKE(pjzDescr)
016500070215     D  rpyDescrizioneAggiuntiva...
016600070215     D                                     LIKE(pjzDescr)
016700070215     D  rpyIndirizzo...
016800070215     D                                     LIKE(pjzIndiriz)
016900070215     D  rpyCap...
017000070215     D                                     LIKE(pjzCap)
017100070215     D  rpyLocalita...
017200070215     D                                     LIKE(diz$Loc)
017300070215     D  rpySiglaProvincia...
017400070215     D                                     LIKE(pjzProv)
017500070215     D  rpySiglaNazione...
017600070215     D                                     LIKE(diz$Naz)
017700070215
017800070215      //********************************************************************************************
017900070215      //
018000070215      // Definizione procedure usate.
018100070215      //
018200070215      //********************************************************************************************
018300070215     D xAllinea        PR                  EXTPGM('XALLINEA')
018400070215     D  codice                       31A   OPTIONS(*VARSIZE)
018500070215     D  len                           5P 0 CONST
018600070215     D  tipo                          1A   CONST
018700070215     D tibs6910r       PR                  EXTPGM('TIBS6910R')
018800070215     D  rqsSocieta                         LIKE(pjzSocieta)
018900070215     D  rqsCtb                             LIKE(pjzCtb) CONST
019000070215     D  rqsKcc                             LIKE(pjzKcc)
019100070215     D  rqsKsc                             LIKE(pjzKsc)
019200070215     D  rqsDtPar                           LIKE(pjzData)
019300070215     D  rqsNrPar                           LIKE(pjzNr)
019400070215     D  rqsSerPar                          LIKE(pjzSerieNm)
019500070215     D  rqsCnaco00f                    N   CONST
019600070215     D  rqsCnind00f                    N   CONST
019700070215     D  rqsCnclp00f                    N   CONST
019800070215     D  rqsFncls00f                    N   CONST
019900070215     D  rpyEsito                     10I 0
020000070215     D  rpyOpCode                    10A
020100070215     D  rpyStatus                    10I 0
020200070215     D  rpyCnaco00f                        LIKE(cnaco00f)
020300070215     D  rpyCnind00f                        LIKE(cnind00f)
020400070215     D  rpyCnclp00f                        LIKE(cnclp00f)
020500070215     D  rpyFncls00f                        LIKE(fncls00f)
020600070215     D ndDvaSog        PR                  EXTPGM('NDDVASOG')
020700070215     D  dvaRic                        1A   CONST
020800070215     D  dvaSocieta                         LIKE(pjzSocieta)
020900070215     D  dvaUnita                           LIKE(pjzUnita) CONST
021000070215     D  dvaStrutt                    10A   CONST
021100070215     D  dvaDtRif                           LIKE(pjzData)
021200070215     D  dvaLenOut                     9B 0 CONST
021300070215     D  dvaSNatura                         LIKE(pjzSNatura)
021400070215     D  dvaCodice                          LIKE(pjzKsc)
021500070215     D  dvaLineaV                          LIKE(pjzLineaV) CONST
021600070215     D  dvaFiliale                         LIKE(pjzFiliale) CONST
021700070215     D  dvaSogg                            LIKE(pjzSogg) OPTIONS(*OMIT)
021800070215     D  dvaTpInd                           LIKE(pjzTpInd) CONST
021900070215     D  dvaCdInd                           LIKE(pjzCdInd) CONST
022000070215     D  dvaErrore                     1A
022100070215     D  dvaOutput                  4000A   OPTIONS(*VARSIZE)
022200070215
022300070215      //********************************************************************************************
022400070215      //
022500070215      // Definizione strutture dati.
022600070215      //
022700070215      //********************************************************************************************
022800070215     D andiz00f      E DS                  BASED(nullPtr) PREFIX(pjz)
022900070215     D cndizion      E DS                  BASED(nullPtr) PREFIX(diz)
023000070215     D cnaco00f      E DS                  INZ QUALIFIED
023100070215     D cnind00f      E DS                  INZ QUALIFIED
023200070215     D cnclp00f      E DS                  INZ QUALIFIED
023300070215     D fncls00f      E DS                  INZ QUALIFIED
023400070215     D dvaSog        E DS                  INZ QUALIFIED
023500070215     D keyFnspe03l     DS                  INZ LIKEREC(fnspe000:*KEY)
023600070215
023700070215      //********************************************************************************************
023800070215      //
023900070215      // Definizione variabili.
024000070215      //
024100070215      //********************************************************************************************
024200070215     D nullPtr         S               *
024300070215     D rqsKcc          S                   LIKE(pjzKcc)
024400070215     D dvaDtRif        S                   LIKE(pjzData) INZ(*SYS)
024500070215     D dvaErrore       S              1A
024600070215
024700070221      /FREE
024800070221       //*******************************************************************************************
024900070221       //
025000070221       // Main.
025100070221       //
025200070221       //*******************************************************************************************
025300070215
025400070215       EXSR chkParametri;
025500070215       EXSR setIndirizzoSpedizioneFatturaDaGestionale;
025600070215
025700070215       // A questo punto, se la ragione sociale è ancora vuota significa che non sono riuscito
025800070215       // a reperire uno straccio di indirizzo a cui spedire 'sta fottutissima fattura,
025900070215       // quindi provo in Proj.
026000070215       IF rpyRagioneSociale = *BLANK;
026100070215         EXSR setIndirizzoSpedizioneFatturaDaProj;
026200070215       ENDIF;
026300070215
026400070215       EXSR uscita;
026500070215
026600070215       //*******************************************************************************************
026700070215       //
026800070216       // Inizializzazione programma.
026900070215       //
027000070215       //*******************************************************************************************
027100070215       BEGSR *INZSR;
027200070215
027300070215         keyFnspe03l.speFls = LUOGO;
027400070215         keyFnspe03l.speCod = LUOGO_SPEDIZIONE_FATTURA;
027500070215
027600070215       ENDSR;
027700070215
027800070215       //*******************************************************************************************
027900070215       //
028000070215       // Controllo validità dei parametri ricevuti.
028100070215       //
028200070215       //*******************************************************************************************
028300070215       BEGSR chkParametri;
028400070215
028500070215         CLEAR rpyEsito;
028600070215         CLEAR rpyOpCode;
028700070215         CLEAR rpyStatus;
028800070215         CLEAR rpyRagioneSociale;
028900070215         CLEAR rpyDescrizioneAggiuntiva;
029000070215         CLEAR rpyIndirizzo;
029100070215         CLEAR rpyCap;
029200070215         CLEAR rpyLocalita;
029300070215         CLEAR rpySiglaProvincia;
029400070215         CLEAR rpySiglaNazione;
029500070215
029600070215         IF rqsSocieta = *BLANK;
029700070215           rqsSocieta = BARTOLINI;
029800070215         ENDIF;
029900070215
030000070215         SELECT;
030100070215           WHEN rqsSNatura = *BLANK;
030200070215             rqsSNatura = CLIENTE;
030300070215           WHEN rqsSNatura <> CLIENTE AND rqsSNatura <> FORNITORE;
030400070215             rpyEsito = ESITO_ERRORE;
030500070216             rpyOpCode = CODICE_OPERATIVO_ERRORE;
030600070215             rpyStatus = ERRORE_PARAMETRO_SOTTONATURA;
030700070215             EXSR uscita;
030800070215         ENDSL;
030900070215
031000070215         IF rqsKsc = *BLANK;
031100070215           rpyEsito = ESITO_ERRORE;
031200070216           rpyOpCode = CODICE_OPERATIVO_ERRORE;
031300070215           rpyStatus = ERRORE_PARAMETRO_SOTTOCONTO;
031400070215           EXSR uscita;
031500070215         ENDIF;
031600070215
031700070215         rqsKsc = %TRIML(rqsKsc);
031800070215         xAllinea( rqsKsc : %SIZE(rqsKsc)
031900070215         : ALLINEAMENTO_DESTRA_RIEMPIMENTO_ZERI);
032000070215
032100070215       ENDSR;
032200070215
032300070215       //*******************************************************************************************
032400070215       //
032500070215       // Imposto l'indirizzo di spedizione della fattura prendendolo dai luoghi.
032600070215       //
032700070215       //*******************************************************************************************
032800070215       BEGSR setIndirizzoSpedizioneFatturaDaGestionale;
032900070215
033000070215         IF rqsSocieta <> BARTOLINI OR rqsSNatura <> CLIENTE;
033100070215           LEAVESR;
033200070215         ENDIF;
033300070215
033400070215         MONITOR;
033500070216           keyFnspe03l.speCli = %INT(rqsKsc);
033600070215           ON-ERROR;
033700070215             rpyEsito = ESITO_ERRORE;
033800070216             rpyOpCode = CODICE_OPERATIVO_ERRORE;
033900070216             rpyStatus = ERRORE_PARAMETRO_SOTTOCONTO;
034000070215             EXSR uscita;
034100070215         ENDMON;
034200070214
034300070215         IF NOT %OPEN(fnspe03l);
034400070215           OPEN(E) fnspe03l;
034500070215           IF %ERROR();
034600070215             rpyEsito = ESITO_ERRORE;
034700070216             rpyOpCode = CODICE_OPERATIVO_STATUS;
034800070215             rpyStatus = %STATUS();
034900070215             EXSR uscita;
035000070215           ENDIF;
035100070215         ENDIF;
035200070215
035300070215         CHAIN %KDS(keyFnspe03l) fnspe03l;
035400070214
035500070215         IF %FOUND AND speFlg = RECORD_ATTIVO;
035600070215           rpyRagioneSociale = speRag;
035700070215           rpyDescrizioneAggiuntiva = spePer;
035800070215           rpyIndirizzo = speInd;
035900070215           rpyCap = speCap;
036000070215           rpyLocalita = speLoc;
036100070215           rpySiglaProvincia = spePro;
036200070215           rpySiglaNazione = speNaz;
036300070216           rpyOpCode = CODICE_OPERATIVO_INFORMAZIONE;
036400070215           rpyStatus = INFO_INDIRIZZO_SPEDIZIONE_FATTURA;
036500070215           LEAVESR;
036600070215         ENDIF;
036700070215
036800070215         // Se l'indirizzo di spedizione della fattura non esiste oppure è annullato,
036900070215         // reperisco la sede legale. La sede legale c'è sempre.
037000070215         RESET rqsKcc;
037100070215         CLEAR rpyEsito;
037200070215         CLEAR rpyOpCode;
037300070215         CLEAR rpyStatus;
037400070215         RESET cnaco00f;
037500070215         RESET cnind00f;
037600070215         RESET cnclp00f;
037700070215         RESET fncls00f;
037800070215
037900070215         tibs6910r( rqsSocieta : COGE : rqsKcc : rqsKsc : rqsDtPar : rqsNrPar
038000070215         : rqsSerPar : VOGLIO_CNACO00F : VOGLIO_CNIND00F : NON_VOGLIO_CNCLP00F
038100070215         : NON_VOGLIO_FNCLS00F : rpyEsito : rpyOpCode : rpyStatus
038200070215         : cnaco00f : cnind00f : cnclp00f : fncls00f );
038300070215
038400070215         IF rpyEsito < 0;
038500070216           rpyOpCode = CODICE_OPERATIVO_ERRORE;
038600070215           rpyStatus = ERRORE_SOTTOCONTO_INESISTENTE;
038700070215           EXSR uscita;
038800070215         ENDIF;
038900070215
039000070215         rpyRagioneSociale = cnaco00f.acoRag;
039100070215         rpyIndirizzo = cnind00f.indVia;
039200070215         rpyCap = cnind00f.indCae;
039300070215         rpyLocalita = cnind00f.indCit;
039400070215         rpySiglaProvincia = cnind00f.indPrv;
039500070215         rpySiglaNazione = cnind00f.indSta;
039600070215         CLEAR rpyEsito;
039700070216         rpyOpCode = CODICE_OPERATIVO_INFORMAZIONE;
039800070215         rpyStatus = INFO_INDIRIZZO_SEDE_LEGALE;
039900070215
040000070215       ENDSR;
040100070215
040200070215       //*******************************************************************************************
040300070215       //
040400070215       // Prendo l'indirizzo spedizione fattura da Proj.
040500070215       //
040600070215       //*******************************************************************************************
040700070215       BEGSR setIndirizzoSpedizioneFatturaDaProj;
040800070215
040900070215         SELECT;
041000070215           WHEN rqsDtRif > *LOVAL;
041100070215             dvaDtRif = rqsDtRif;
041200070215           WHEN rqsDtPar > *LOVAL;
041300070215             dvaDtRif = rqsDtPar;
041400070215           OTHER;
041500070215             RESET dvaDtRif;
041600070215         ENDSL;
041700070215
041800070215         RESET dvaErrore;
041900070215         RESET dvaSog;
042000070215
042100070215         ndDvaSog( CHAIN : rqsSocieta : *BLANK : 'DVASOG' : dvaDtRif
042200070215         : %SIZE(dvaSog) : rqsSNatura : rqsKsc : *BLANK : *ZERO : *OMIT
042300070215         : TIPO_INDIRIZZO_FATTURA : CODICE_INDIRIZZO_FATTURA
042400070215         : dvaErrore : dvaSog );
042500070215
042600070215         // Se l'indirizzo di spedizione della fattura non esiste oppure è scaduto,
042700070215         // reperisco la sede legale. La sede legale c'è sempre.
042800070215         IF dvaErrore <> *OFF
042900070215         OR (dvaSog.dvsDtFiVlI > *LOVAL AND dvaDtRif > dvaSog.dvsDtFiVlI);
043000070215           RESET dvaErrore;
043100070215           RESET dvaSog;
043200070215           ndDvaSog( CHAIN : rqsSocieta : *BLANK : 'DVASOG' : dvaDtRif
043300070215           : %SIZE(dvaSog) : rqsSNatura : rqsKsc : *BLANK : *ZERO : *OMIT
043400070215           : TIPO_INDIRIZZO_SEDELEGALE : CODICE_INDIRIZZO_SEDELEGALE
043500070215           : dvaErrore : dvaSog );
043600070216           rpyOpCode = CODICE_OPERATIVO_INFORMAZIONE;
043700070215           rpyStatus = INFO_INDIRIZZO_SEDE_LEGALE;
043800070215         ELSE;
043900070216           rpyOpCode = CODICE_OPERATIVO_INFORMAZIONE;
044000070215           rpyStatus = INFO_INDIRIZZO_SPEDIZIONE_FATTURA;
044100070215         ENDIF;
044200070215
044300070215         IF dvaErrore = *OFF;
044400070215           rpyRagioneSociale = dvaSog.dvsDesFil;
044500070215           rpyDescrizioneAggiuntiva = dvaSog.dvsDesAggI;
044600070215           rpyIndirizzo = dvaSog.dvsIndriz;
044700070215           rpyCap = dvaSog.dvsCap;
044800070215           rpyLocalita = dvaSog.dvsLocalit;
044900070215           rpySiglaProvincia = dvaSog.dvsProv;
045000070215           rpySiglaNazione = dvaSog.dvsStato;
045100070215         ELSE;
045200070215           rpyEsito = ESITO_ERRORE;
045300070216           rpyOpCode = CODICE_OPERATIVO_ERRORE;
045400070215           rpyStatus = ERRORE_SOTTOCONTO_INESISTENTE;
045500070215           EXSR uscita;
045600070215         ENDIF;
045700070215
045800070215       ENDSR;
045900070215
046000070215       //*******************************************************************************************
046100070215       //
046200070215       // Uscita dal programma.
046300070215       //
046400070215       //*******************************************************************************************
046500070215       BEGSR uscita;
046600070215
046700070215         IF rpyOpCode = *BLANK;
046800070216           rpyOpCode = CODICE_OPERATIVO_ELABORAZIONE_ESEGUITA;
046801180313           IF %SUBST(rpyCap:1:4) = '4789'
046802180313           AND (rpySiglaNazione = 'IT' OR rpySiglaNazione = *BLANK);
046803180313             rpySiglaNazione = 'SM';
046804180313             CLEAR rpySiglaProvincia;
046805180313           ENDIF;
046900070215         ENDIF;
047000070215
047100070215         RETURN;
047200070215
047300070215       ENDSR;
047400070214
047500070214      /END-FREE
