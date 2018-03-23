000100120223     ***************************************************************************
000200120223     **
000300120301     ** Questo programma restituisce la lista delle nazioni.
000400120223     **
000500120223     ***************************************************************************
000600120302     H DFTACTGRP(*NO) BNDDIR('QC2LE':'TIBS')
000700120223
000800120223     ***************************************************************************
000900120223     **
001000120223     ** Costanti.
001100120223     **
001200120223     ***************************************************************************
001300120223     D ESITO_ERROR...
001400120223     D                 C                   -1
001500120223     D ESITO_OK...
001600120223     D                 C                   0
001700120312     D IO_SONO_IL_MITTENTE...
001800120312     D                 C                   'M'
001900120312     D NAZIONI_DESTINAZIONE...
002000120312     D                 C                   'D'
002100120312     D NAZIONI_MITTENTE...
002200120312     D                 C                   'M'
002300120312     D NETWORK_DPD...
002400120302     D                 C                   'DPD'
002500120302     D NETWORK_FEDEX...
002600120302     D                 C                   'FED'
002700120302     D NETWORK_EUROEXPRESS...
002800120302     D                 C                   'EEX'
002900120302     D NETWORK_ITALIA...
003000120302     D                 C                   '   '
003100120301     D ORDERBY_ID...
003200120301     D                 C                   2
003300120224     D QTEMP...
003400120224     D                 C                   'QTEMP'
003500120223     D RPYOPCODE_DONE...
003600120223     D                 C                   'DONE'
003700120223     D RPYOPCODE_ERROR...
003800120223     D                 C                   'ERROR'
003900120223     D RQSOPCODE_GETKSUFAMI...
004000120301     D                 C                   'GETNAZLIST'
004100120223
004200120223     ***************************************************************************
004300120223     **
004400120223     ** Strutture dati.
004500120223     **
004600120223     ***************************************************************************
004700120224      /COPY QSYSINC/QRPGLESRC,QUSEC
004800120302     D ds15          E DS                  INZ QUALIFIED
004900120302     D nazioniAry      DS                  QUALIFIED
005000120301     D                                     DIM(999) BASED(nazioniPtr)
005100120301     D  id                            3A
005200120301     D  descrizione                  29A
005300120302     D  networkItalia...
005400120302     D                                 N
005500120302     D  networkEuroExpress...
005600120302     D                                 N
005700120302     D  networkDPD...
005800120302     D                                 N
005900120302     D  networkFedEx...
006000120302     D                                 N
006100120302     D nazioniElem     DS
006200120302     D  nazioniElemC           1      3A
006300120302     D  nazioniElemN           1      3S 0 INZ
006400120301     D tis7st30i     E DS                  INZ(*EXTDFT) QUALIFIED
006500120301     D tis7st30o     E DS                  INZ(*EXTDFT) QUALIFIED
006600120301     D  idNazioni_ary                 3A   DIM(99) OVERLAY(idNazioni)
006700120301     D  dzNazioni_ary                29A   DIM(99) OVERLAY(dzNazioni)
006800120301     D lingue          DS                  QUALIFIED TEMPLATE
006900120302     D  usrSpcName                   10A
007000120302     D orderBy         DS                  QUALIFIED DIM(9)
007100120301     D  lingue                             LIKEDS(lingue) DIM(9)
007200120223
007300120223     ***************************************************************************
007400120223     **
007500120223     ** Campi.
007600120223     **
007700120223     ***************************************************************************
007800120301     D l               S              3U 0
007900120301     D o               S              3U 0
008000120302     D nazioniFld      S          35964A
008100120301     D rqsOpCode       S             10A
008200120223     D rqsData         S           9999A
008300120223     D rqsDataLen      S              4P 0
008400120223     D rpyOpCode       S             10A
008500120223     D rpyData         S           9999A
008600120223     D rpyDataLen      S              4P 0
008700120223
008800120223     ***************************************************************************
008900120223     **
009000120223     ** Prototipi.
009100120223     **
009200120223     ***************************************************************************
009300120224      /COPY GAITRASRC/SRCPROTOPR,QUSCHGUS
009400120224      /COPY GAITRASRC/SRCPROTOPR,QUSCRTUS
009500120224      /COPY GAITRASRC/SRCPROTOPR,QUSDLTUS
009600120224      /COPY GAITRASRC/SRCPROTOPR,QUSRTVUS
009700120301      /COPY GAITRASRC/SRCPROTOPR,TIBS0800R
009800120301      /COPY GAITRASRC/SRCPROTOPR,TMPNAM
009900120223
010000120223     D*--------------------------------------------------
010100120223     D* Procedure name: ChkParametriRqs
010200120223     D* Purpose:        Controlla i parametri della richiesta.
010300120223     D* Returns:        Esito.
010400120223     D*--------------------------------------------------
010500120223     DChkParametriRqs  PR            10I 0
010600120223
010700120223     D*--------------------------------------------------
010800120223     D* Procedure name: Run
010900120223     D* Purpose:
011000120223     D* Returns:        Esito.
011100120223     D*--------------------------------------------------
011200120223     D Run             PR            10I 0
011300120301
011400120301     D*--------------------------------------------------
011500120301     D* Procedure name: SetNazioni
011600120301     D* Purpose:        Imposta nazioni
011700120301     D* Returns:        Esito.
011800120301     D*--------------------------------------------------
011900120301     D SetNazioni      PR            10I 0
012000120312
012100120312     D*--------------------------------------------------
012200120312     D* Procedure name: IsNazioneValida
012300120312     D* Purpose:        Restituisce *ON se la nazione è valida.
012400120312     D* Returns:        *ON = Nazione valida.
012500120312     D* Parameter:      priNtwItalia => Network Italia.
012600120312     D* Parameter:      priNtwEuroExpress => Network EuroExpress.
012700120312     D* Parameter:      priNtwDPD => Network DPD.
012800120312     D* Parameter:      priNtwFedEx => Network FedEx.
012900120312     D* Parameter:      priIdNetwork
013000120312     D* Parameter:      priNazioneMittenteDestinazione => Mittente/Destinaz...
013100120312     D*                          ione.
013200120312     D* Parameter:      priMittenteDestinatarioOrdinante => Mittente/Destin...
013300120312     D*                          atario/Ordinante.
013400120312     D*--------------------------------------------------
013500120312     DIsNazioneValida  PR              N
013600120312     D  priNtwItalia                   N
013700120312     D  priNtwEuroExpress...
013800120312     D                                 N
013900120312     D  priNtwDPD                      N
014000120312     D  priNtwFedEx                    N
014100120312     D  priIdNetwork                  3A
014200120312     D  priNazioneMittenteDestinazione...
014300120312     D                                1A
014400120312     D  priMittenteDestinatarioOrdinante...
014500120312     D                                1A
014600120312
014700120312
014800120301
014900120223
015000120223     ***************************************************************************
015100120223     **
015200120223     ** Parametri.
015300120223     **
015400120223     ***************************************************************************
015500120223     C     *ENTRY        PLIST
015600120223     C                   PARM                    rqsOpCode
015700120223     C                   PARM                    rqsData
015800120223     C                   PARM                    rqsDataLen
015900120223     C                   PARM                    rpyOpCode
016000120223     C                   PARM                    rpyData
016100120223     C                   PARM                    rpyDataLen
016200120223
016300120223     ***************************************************************************
016400120223     **
016500120223     ** Main.
016600120223     **
016700120223     ***************************************************************************
016800120223
016900120223      /FREE
017000120223
017100120224       rpyOpCode = RPYOPCODE_ERROR;
017200120224       CLEAR rpyData;
017300120224       CLEAR rpyDataLen;
017400120224
017500120223       IF ChkParametriRqs() < ESITO_OK;
017600120223         RETURN;
017700120223       ENDIF;
017800120224
017900120224       IF Run() < ESITO_OK;
018000120224         RETURN;
018100120224       ENDIF;
018200120223
018300120224       rpyOpCode = RPYOPCODE_DONE;
018400120223       RETURN;
018500120223
018600120223      /END-FREE
018700120223
018800120223     P*--------------------------------------------------
018900120223     P* Procedure name: ChkParametriRqs
019000120223     P* Purpose:        Controlla i parametri della richiesta.
019100120223     P* Returns:        Esito.
019200120223     P*--------------------------------------------------
019300120223     PChkParametriRqs  B
019400120223     DChkParametriRqs  PI            10I 0
019500120223
019600120223      /FREE
019700120223
019800120223       IF rqsOpCode <> RQSOPCODE_GETKSUFAMI;
019900120223         RETURN ESITO_ERROR;
020000120223       ENDIF;
020100120223
020200120301       RESET tis7st30i;
020300120223
020400120223       SELECT;
020500120301         WHEN %SUBST(rqsData : 1 : 10) = tis7st30i.formato;
020600120301           tis7st30i = %SUBST(rqsData : 1 : rqsDataLen);
020700120223         OTHER;
020800120223           RETURN ESITO_ERROR;
020900120223       ENDSL;
021000120223
021100120301       l = cvtLinguaISO2ToTabel(tis7st30i.linIso2);
021200120301       o = tis7st30i.orderBy;
021300120301
021400120224       RETURN ESITO_OK;
021500120223
021600120223      /END-FREE
021700120223     PChkParametriRqs  E
021800120223
021900120223
022000120223     P*--------------------------------------------------
022100120223     P* Procedure name: Run
022200120223     P* Purpose:
022300120223     P* Returns:        Esito.
022400120223     P*--------------------------------------------------
022500120223     P Run             B
022600120223     D Run             PI            10I 0
022700120223
022800120223     D retField        S             10I 0 STATIC
022900120223     D i               S             10I 0 STATIC
023000120223     D p               S             10I 0 STATIC
023100120223
023200120224      /FREE
023300120223
023400120223       CLEAR retField;
023500120301       RESET tis7st30o;
023600120223
023700120301       IF SetNazioni() < ESITO_OK;
023800120224         RETURN ESITO_ERROR;
023900120224       ENDIF;
024000120223
024100120223       // Restituisco 99 elementi per volta con la seguente regola:
024200120301       // tis7st30i.prgCall = 0 da   1 a  99;
024300120301       // tis7st30i.prgCall = 1 da 100 a 198;
024400120301       // tis7st30i.prgCall = 2 da 199 a 297;
024500120223
024600120302       CLEAR p;
024700120312
024800120308       FOR i = (tis7st30i.prNazioni + 1) TO nazioniElemN;
024900120312         IF IsNazioneValida( nazioniAry(i).networkItalia
025000120312                           : nazioniAry(i).networkEuroExpress
025100120312                           : nazioniAry(i).networkDPD
025200120312                           : nazioniAry(i).networkFedEx
025300120312                           : tis7st30i.idNetwork
025400120312                           : tis7st30i.nazMitDes
025500120312                           : tis7st30i.mitDesOrd
025600120312                           );
025700120302           p += 1;
025800120302           tis7st30o.idNazioni_ary(p) = nazioniAry(i).id;
025900120302           tis7st30o.dzNazioni_ary(p) = nazioniAry(i).descrizione;
026000120302         ENDIF;
026100120301         IF i = nazioniElemN;
026200120301           tis7st30o.datiFiniti = *ON;
026300120224           LEAVE;
026400120223         ENDIF;
026500120301         IF p = %ELEM(tis7st30o.idNazioni_ary);
026600120223           LEAVE;
026700120223         ENDIF;
026800120223       ENDFOR;
026900120223
027000120301       tis7st30o.nrNazioni = p;
027100120308       tis7st30o.prNazioni = i;
027200120301       rpyData = tis7st30o;
027300120301       rpyDataLen = %SIZE(tis7st30o);
027400120224
027500120223       RETURN retField;
027600120223
027700120223      /END-FREE
027800120223     P Run             E
027900120301
028000120301     P*--------------------------------------------------
028100120301     P* Procedure name: SetNazioni
028200120301     P* Purpose:        Imposta nazioni
028300120301     P* Returns:        Esito.
028400120301     P*--------------------------------------------------
028500120301     P SetNazioni      B
028600120301     D SetNazioni      PI            10I 0
028700120301
028800120301     D retField        S             10I 0 STATIC
028900120301     D db2NumberRows   S             31P 0 STATIC
029000120301     D stm             S          32740A   VARYING STATIC
029100120302     D stmSelect       S            512A   VARYING STATIC
029200120301     D stmFrom         S            255A   VARYING STATIC
029300120301     D stmWhere        S            255A   VARYING STATIC
029400120301     D stmOrderBy      S            255A   VARYING STATIC
029500120301     D qualifiedUserSpaceName...
029600120301     D                 S             16A   BASED(qualifiedUserSpaceNamePtr)
029700120301
029800120301      /FREE
029900120301
030000120301       CLEAR retField;
030100120301
030200120301       // Prima chiamata: recupero tutte le nazioni dalla tabella 15 e le
030300120301       // memorizzo in uno user space in QTEMP.
030400120301       // Nelle chiamate successive ricevo il nome dello user space da cui
030500120301       // reperire il contenuto della struttura dati nazioni.
030600120301
030700120309       IF tis7st30i.prNazioni = 0 AND orderBy(o).lingue(l).usrSpcName = *BLANK;
030800120302
030900120308         stmSelect = 'SELECT CASE WHEN LEFT(tblKey, 3) = '''' +
031000120308                      THEN SUBSTR(tblUni, 46, 3) +
031100120302                      ELSE LEFT(tblKey, 3) END AS ID_NAZIONE';
031200120302
031300120302         SELECT;
031400120302           WHEN tis7st30i.orderBy = ORDERBY_ID;
031500120308             stmSelect += ', SUBSTR(tblUni, 46, 3) CONCAT '' '' CONCAT +
031600120302                             LEFT(tblUni, 25) AS DZ_NAZIONE';
031700120302           OTHER;
031800120302             stmSelect += ', RTRIM(LEFT(tblUni, 25)) CONCAT '' '' CONCAT +
031900120308                             SUBSTR(tblUni, 46, 3) AS DZ_NAZIONE';
032000120302         ENDSL;
032100120301
032200120302         stmSelect += ', CASE WHEN SUBSTR(tblUni, 44, 1) = ''I'' +
032300120302                       THEN ''1'' ELSE ''0'' END AS NTW_ITALIA';
032400120302
032500120302         stmSelect += ', CASE WHEN SUBSTR(tblUni, 72, 1) = ''S'' +
032600120302                       THEN ''1'' ELSE ''0'' END AS NTW_EUROEXPRESS';
032700120302
032800120302         stmSelect += ', CASE WHEN SUBSTR(tblUni, 56, 3) > ''000'' +
032900120302                       THEN ''1'' ELSE ''0'' END AS NTW_DPD';
033000120302
033100120302         stmSelect += ', CASE WHEN SUBSTR(tblUni, 62, 3) > ''000'' +
033200120302                       THEN ''1'' ELSE ''0'' END AS NTW_FEDEX';
033300120301
033400120301         stmFrom = 'FROM tabel00f';
033500120301
033600120302         stmWhere = 'WHERE tblKut = ' + %CHAR(l)
033700120302                  + ' AND tblCod = ''15'' AND tblFlg = '' ''';
033800120301
033900120301         stmOrderBy = 'ORDER BY tblKut, tblCod';
034000120301
034100120301         SELECT;
034200120301           WHEN tis7st30i.orderBy = ORDERBY_ID;
034300120308             stmOrderBy += ', SUBSTR(tblUni, 46, 3)';
034400120301           OTHER;
034500120301             stmOrderBy += ', LEFT(tblUni, 25)';
034600120301         ENDSL;
034700120301
034800120301         stm = stmSelect + ' ' + stmFrom + ' ' + stmWhere + ' ' + stmOrderBy
034900120301             + ' FOR FETCH ONLY';
035000120301
035100120301         EXEC SQL PREPARE nazioni FROM :stm;
035200120301
035300120301         EXEC SQL DECLARE nazioni INSENSITIVE NO SCROLL CURSOR FOR nazioni;
035400120301
035500120301         EXEC SQL OPEN nazioni;
035600120301
035700120301         IF sqlCode < *ZERO;
035800120301           DUMP(A);
035900120301           RETURN sqlCode;
036000120301         ENDIF;
036100120301
036200120301         EXEC SQL GET DIAGNOSTICS :db2NumberRows = DB2_NUMBER_ROWS;
036300120301
036400120302         nazioniPtr = %ADDR(nazioniFld);
036500120301
036600120301         EXEC SQL FETCH NEXT FROM nazioni FOR :db2NumberRows ROWS
036700120302           INTO :nazioniAry
036800120301         ;
036900120301
037000120301         IF sqlCode < *ZERO;
037100120301           DUMP(A);
037200120301           retField = sqlCode;
037300120301         ENDIF;
037400120301
037500120301         EXEC SQL CLOSE nazioni;
037600120301
037700120301         IF retField < ESITO_OK;
037800120301           RETURN retField;
037900120301         ENDIF;
038000120301
038100120301         nazioniElemN = db2NumberRows;
038200120301
038300120301         // Produco un nome temporaneo.
038400120301
038500120301         qualifiedUserSpaceNamePtr = ProduceTemporaryFileName(*OMIT);
038600120301
038700120301         IF qualifiedUserSpaceNamePtr = *NULL;
038800120301           DUMP(A);
038900120301           RETURN ESITO_ERROR;
039000120301         ENDIF;
039100120301
039200120301         // A questo punto nel campo qualifiedUserSpaceName c'è un nome
039300120301         // qualificato tipo 'QTEMP/QACXxxxxxx'.
039400120301
039500120301         CLEAR QUSEC;
039600120301         QUSBPRV = %SIZE(QUSEC);
039700120301
039800120301         CreateUserSpace( %SUBST(qualifiedUserSpaceName : 7 : 10) + QTEMP
039900120301                        : 'NAZIONI'
040000120302                        : %SIZE(nazioniFld) + 3
040100120301                        : X'00'
040200120301                        : '*ALL'
040300120302                        : 'Lingua ' + tis7st30i.linIso2
040400120302                        + ' OrderBy ' + %CHAR(tis7st30i.orderBy) + '.'
040500120301                        : '*NO'
040600120301                        : QUSEC
040700120301                        );
040800120301
040900120301         IF QUSEI <> *BLANK;
041000120301           DUMP(A);
041100120301           RETURN ESITO_ERROR;
041200120301         ENDIF;
041300120301
041400120301         CLEAR QUSEC;
041500120301         QUSBPRV = %SIZE(QUSEC);
041600120301
041700120301         ChangeUserSpace( %SUBST(qualifiedUserSpaceName : 7 : 10) + QTEMP
041800120301                        : 1
041900120301                        : 3
042000120301                        : nazioniElemC
042100120301                        : '0'
042200120301                        : QUSEC
042300120301                        );
042400120301
042500120301         IF QUSEI <> *BLANK;
042600120301           DUMP(A);
042700120301           RETURN ESITO_ERROR;
042800120301         ENDIF;
042900120301
043000120301         CLEAR QUSEC;
043100120301         QUSBPRV = %SIZE(QUSEC);
043200120301
043300120301         ChangeUserSpace( %SUBST(qualifiedUserSpaceName : 7 : 10) + QTEMP
043400120301                        : 4
043500120302                        : %SIZE(nazioniFld)
043600120302                        : nazioniFld
043700120301                        : '0'
043800120301                        : QUSEC
043900120301                        );
044000120301
044100120301         IF QUSEI <> *BLANK;
044200120301           DUMP(A);
044300120301           RETURN ESITO_ERROR;
044400120301         ENDIF;
044500120301
044600120302         orderBy(o).lingue(l).usrSpcName =
044700120301                                        %SUBST(qualifiedUserSpaceName : 7 : 10);
044800120301
044900120301       ELSE; // Chiamate successive.
045000120301
045100120301         CLEAR QUSEC;
045200120301         QUSBPRV = %SIZE(QUSEC);
045300120301
045400120302         RetrieveUserSpace( orderBy(o).lingue(l).usrSpcName + QTEMP
045500120301                          : 1
045600120301                          : 3
045700120301                          : nazioniElemC
045800120301                          : QUSEC
045900120301                          );
046000120301
046100120301         IF QUSEI <> *BLANK;
046200120301           DUMP(A);
046300120301           RETURN ESITO_ERROR;
046400120301         ENDIF;
046500120301
046600120301         CLEAR QUSEC;
046700120301         QUSBPRV = %SIZE(QUSEC);
046800120301
046900120302         RetrieveUserSpace( orderBy(o).lingue(l).usrSpcName + QTEMP
047000120301                          : 4
047100120302                          : %SIZE(nazioniAry) * nazioniElemN
047200120302                          : nazioniFld
047300120301                          : QUSEC
047400120301                          );
047500120301
047600120301         IF QUSEI <> *BLANK;
047700120301           DUMP(A);
047800120301           RETURN ESITO_ERROR;
047900120301         ENDIF;
048000120301
048100120301       ENDIF;
048200120301
048300120301       RETURN ESITO_OK;
048400120301
048500120301      /END-FREE
048600120301     P SetNazioni      E
048700120301
048800120312
048900120312     P*--------------------------------------------------
049000120312     P* Procedure name: IsNazioneValida
049100120312     P* Purpose:        Restituisce *ON se la nazione è valida.
049200120312     P* Returns:        *ON = Nazione valida.
049300120312     P* Parameter:      priNtwItalia => Network Italia.
049400120312     P* Parameter:      priNtwEuroExpress => Network EuroExpress.
049500120312     P* Parameter:      priNtwDPD => Network DPD.
049600120312     P* Parameter:      priNtwFedEx => Network FedEx.
049700120312     P* Parameter:      priIdNetwork
049800120312     P* Parameter:      priNazioneMittenteDestinazione => Mittente/Destinaz...
049900120312     P*                          ione.
050000120312     P* Parameter:      priMittenteDestinatarioOrdinante => Mittente/Destin...
050100120312     P*                          atario/Ordinante.
050200120312     P*--------------------------------------------------
050300120312     PIsNazioneValida  B
050400120312     DIsNazioneValida  PI              N
050500120312     D  priNtwItalia                   N
050600120312     D  priNtwEuroExpress...
050700120312     D                                 N
050800120312     D  priNtwDPD                      N
050900120312     D  priNtwFedEx                    N
051000120312     D  priIdNetwork                  3A
051100120312     D  priNazioneMittenteDestinazione...
051200120312     D                                1A
051300120312     D  priMittenteDestinatarioOrdinante...
051400120312     D                                1A
051500120312
051600120312      /FREE
051700120312
051800120312       IF priIdNetwork = NETWORK_ITALIA AND
051900120312          priNtwItalia = *ON;
052000120312         RETURN *ON;
052100120312       ENDIF;
052200120312
052300120312       IF priIdNetwork <> NETWORK_ITALIA AND
052400120312          priMittenteDestinatarioOrdinante = IO_SONO_IL_MITTENTE AND
052500120312          priNazioneMittenteDestinazione = NAZIONI_DESTINAZIONE AND (
052600120312          (priNtwDPD = *ON AND priIdNetwork = NETWORK_DPD) OR
052700120312          (priNtwEuroExpress = *ON AND priIdNetwork = NETWORK_EUROEXPRESS) OR
052800120312          (priNtwFedEx = *ON AND priIdNetwork = NETWORK_FEDEX));
052900120312         RETURN *ON;
053000120312       ENDIF;
053100120312
053200120312       IF priIdNetwork <> NETWORK_ITALIA AND
053300120312          priMittenteDestinatarioOrdinante <> IO_SONO_IL_MITTENTE AND
053400120312          priNazioneMittenteDestinazione = NAZIONI_MITTENTE AND (
053500120312          (priNtwDPD = *ON AND priIdNetwork = NETWORK_DPD) OR
053600120312          (priNtwEuroExpress = *ON AND priIdNetwork = NETWORK_EUROEXPRESS) OR
053700120312          (priNtwFedEx = *ON AND priIdNetwork = NETWORK_FEDEX));
053800120312         RETURN *ON;
053900120312       ENDIF;
054000120312
054100120312       IF priIdNetwork <> NETWORK_ITALIA AND
054200120312          priMittenteDestinatarioOrdinante <> IO_SONO_IL_MITTENTE AND
054300120312          priNazioneMittenteDestinazione = NAZIONI_DESTINAZIONE AND
054400120312          priNtwItalia = *ON;
054500120312         RETURN *ON;
054600120312       ENDIF;
054700120312
054800120312       RETURN *OFF;
054900120312
055000120312      /END-FREE
055100120312     PIsNazioneValida  E
055200120312
