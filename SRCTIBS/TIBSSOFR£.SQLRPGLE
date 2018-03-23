000100120216     ***************************************************************************
000200120216     **
000300120216     ** Questo programma di servizio restituisce la società di una filiale.
000400120216     **
000500120216     ** Questo modulo è anche un programma. Dopo aver creato il modulo creare il
000600120216     ** programma col comando CRTPGM PGM(GAITRAFIL/TIBSFISR)
000700120216     **
000800120216     ***************************************************************************
000900120216     H EXTBININT(*YES)
001000120216
001100120216     ***************************************************************************
001200120216     **
001300120216     ** Costanti.
001400120216     **
001500120216     ***************************************************************************
001600120216      /COPY GAITRASRC/SRCCONST,TIBSSOFR
001700120217
001800120216     ***************************************************************************
001900120216     **
002000120216     ** Prototipi.
002100120216     **
002200120216     ***************************************************************************
002300120216      /DEFINE DFTACTGRP_NO
002400120216      /DEFINE DFTACTGRP_YES
002500120216      /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
002600120216      /UNDEFINE DFTACTGRP_NO
002700120216      /UNDEFINE DFTACTGRP_YES
002800120216
002900120216     ***************************************************************************
003000120216     **
003100120216     ** Strutture dati.
003200120216     **
003300120216     ***************************************************************************
003400120217     D azsof00f      E DS                  QUALIFIED TEMPLATE
003500120216     D done            DS                  QUALIFIED INZ
003600120216     D  init                           N
003700120217     D filialiBySocieta...
003800120217     D                 DS                  DIM(999) QUALIFIED INZ
003900120217     D  idFiliale                          LIKE(azsof00f.idFiliale)
004000120217     D tibsSof0i     E DS                  QUALIFIED INZ(*EXTDFT)
004100120217     D tibsSof1i     E DS                  QUALIFIED INZ(*EXTDFT)
004200120217     D tibsSof0o     E DS                  QUALIFIED INZ(*EXTDFT)
004300120217     D tibsSof1o     E DS                  QUALIFIED INZ(*EXTDFT)
004400120216
004500120216     ***************************************************************************
004600120216     **
004700120216     ** Campi.
004800120216     **
004900120216     ***************************************************************************
005000120220     D tibssof_esito...
005100120217     D                 S             10I 0 EXPORT
005200120217     D dataRiferimento...
005300120217     D                 S               D   INZ(*SYS)
005400120217     D idFilialiAry...
005500120217     D                 S                   LIKE(azsof00f.idFiliale)
005600120217     D                                     DIM(999)
005700120217     D idFilialiAryPtr...
005800120217     D                 S               *   INZ(%ADDR(idFilialiAry))
005900120217     D idFilialiChr...
006000120217     D                 S           1998A   BASED(idFilialiAryPtr)
006100120217     D idSocieta...
006200120217     D                 S                   LIKE(azsof00f.idSocieta)
006300120217     D tipoSocieta...
006400120217     D                 S                   LIKE(azsof00f.tpSocieta)
006500120220     D                                     INZ(TIBSSOF_TIPO_SOCIETA_OPERATIVA)
006600120217     D i               S             10I 0
006700120217
006800120217     ***************************************************************************
006900120217     **
007000120217     ** Parametri programma.
007100120217     **
007200120217     ***************************************************************************
007300120220     D TibsSof_Lettura...
007400120217     D                 PI
007500120217     D prmRqsOpCode...
007600120220     D                               10I 0
007700120217     D prmRpyOpCode...
007800120220     D                               10I 0
007900120217     D prmRpyIdMsg...
008000120217     D                               10I 0
008100120217     D prmRqsFormato...
008200120217     D                               10A
008300120217     D prmRqsData...
008400120217     D                            32767A   OPTIONS(*VARSIZE)
008500120217     D prmRqsDataSize...
008600120217     D                               10I 0
008700120217     D prmRpyFormato...
008800120217     D                               10A
008900120217     D prmRpyData...
009000120217     D                            32767A   OPTIONS(*VARSIZE)
009100120217     D prmRpyDataSize...
009200120217     D                               10I 0
009300120217
009400120217     ***************************************************************************
009500120217     **
009600120217     ** Main.
009700120217     **
009800120217     ***************************************************************************
009900120216
010000120217      /FREE
010100120217
010200120220       prmRpyOpCode = TIBSSOF_RPYOPCODE_DONE;
010300120217       CLEAR prmRpyIdMsg;
010400120217
010500120217       SELECT;
010600120217
010700120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_GET_IDSOCIETA_BY_IDFILIALE;
010800120217
010900120217           RESET tibsSof0i;
011000120217           RESET tibsSof0o;
011100120217
011200120217           SELECT;
011300120217             WHEN prmRqsFormato = tibsSof0i.formato;
011400120217               tibsSof0i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
011500120220               IF tibsSof0i.formato = prmRqsFormato;
011600120220                 idSocieta = TibsSof_GetIdSocietaByIdFiliale(
011700120220                                                             tibsSof0i.idFiliale
011800120220                                                          : tibsSof0i.dtValidita
011900120220                                                          : tibsSof0i.tpSocieta
012000120220                                                          );
012100120220                 prmRpyIdMsg = tibssof_esito;
012200120220                 IF tibssof_esito < TIBSSOF_ESITO_OK;
012300120220                   prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
012400120220                 ENDIF;
012500120220               ELSE;
012600120220                 prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
012700120220                 prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
012800120220               ENDIF;
012900120217             OTHER;
013000120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
013100120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
013200120217           ENDSL;
013300120217
013400120217           SELECT;
013500120217             WHEN prmRpyFormato = tibsSof0o.formato;
013600120217               tibsSof0o.idSocieta = idSocieta;
013700120217               %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSof0o;
013800120217             OTHER;
013900120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
014000120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
014100120217           ENDSL;
014200120217
014300120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_GET_IDFILIALI_BY_IDSOCIETA;
014400120217
014500120217           RESET tibsSof1i;
014600120217           RESET tibsSof1o;
014700120217
014800120217           SELECT;
014900120217             WHEN prmRqsFormato = tibsSof1i.formato;
015000120217               tibsSof1i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
015100120220               IF prmRqsFormato = tibsSof1i.formato;
015200120220                idFilialiAry = TibsSof_GetIdFilialiByIdSocieta(
015300120220                                                            tibsSof1i.idSocieta
015400120220                                                          : tibsSof1i.dtValidita
015500120220                                                          : tibsSof1i.tpSocieta
015600120220                                                            );
015700120220                prmRpyIdMsg = tibssof_esito;
015800120220                IF tibssof_esito < TIBSSOF_ESITO_OK;
015900120220                  prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
016000120220                ENDIF;
016100120220               ELSE;
016200120220                 prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
016300120220                 prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
016400120220               ENDIF;
016500120217             OTHER;
016600120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
016700120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
016800120217           ENDSL;
016900120217
017000120217           SELECT;
017100120217             WHEN prmRpyFormato = tibsSof1o.formato;
017200120217               RESET tibsSof1o;
017300120217               tibsSof1o.idFiliali = idFilialiChr;
017400120217               %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSof1o;
017500120217             OTHER;
017600120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
017700120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
017800120217           ENDSL;
017900120217
018000120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_INIT;
018100120217
018200120220           IF TibsSof_Init() < TIBSSOF_ESITO_OK;
018300120220             prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
018400120220             prmRpyIdMsg = tibssof_esito;
018500120217           ENDIF;
018600120217
018700120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_FINALIZE;
018800120217
018900120220           TibsSof_Finalize();
019000120217           *INLR = *ON;
019100120217
019200120217         OTHER;
019300120217
019400120220           prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
019500120220           prmRpyIdMsg = TIBSSOF_ESITO_RQSOPCODE_SCONOSCIUTO;
019600120217
019700120217       ENDSL;
019800120217
019900120217       RETURN;
020000120217
020100120217      /END-FREE
020200120216
020300120216     P*--------------------------------------------------
020400120220     P* Procedure name: TibsSof_Init
020500120216     P* Purpose:        Inizializza il modulo.
020600120216     P* Returns:        Esito.
020700120216     P*--------------------------------------------------
020800120220     P TibsSof_Init    B                   EXPORT
020900120220     D TibsSof_Init    PI            10I 0
021000120216
021100120216      /FREE
021200120216
021300120216       IF done.init;
021400120220         RETURN TIBSSOF_ESITO_MODULO_GIA_INIZIALIZZATO;
021500120216       ENDIF;
021600120216
021700120216       done.init = *ON;
021800120216
021900120220       RETURN TIBSSOF_ESITO_OK;
022000120216
022100120216      /END-FREE
022200120220     P TibsSof_Init    E
022300120216
022400120216
022500120216     P*--------------------------------------------------
022600120220     P* Procedure name: TibsSof_Finalize
022700120216     P* Purpose:        Chiude il modulo.
022800120216     P* Returns:
022900120216     P*--------------------------------------------------
023000120220     P TibsSof_Finalize...
023100120220     P                 B                   EXPORT
023200120220     D TibsSof_Finalize...
023300120220     D                 PI
023400120216
023500120216      /FREE
023600120216
023700120216       RESET done;
023800120216
023900120216      /END-FREE
024000120220     P TibsSof_Finalize...
024100120220     P                 E
024200120216
024300120216
024400120216     P*--------------------------------------------------
024500120220     P* Procedure name: TibsSof_GetIdSocietaByIdFiliale
024600120216     P* Purpose:        Restituisce la società di una filiale alla data.
024700120216     P* Returns:        ID società (*BLANK = errore).
024800120216     P* Parameter:      priIdFiliale => ID filiale.
024900120216     P* Parameter:      priDataRiferimento => Data riferimento per ricerca ...
025000120216     P*                          (*NOPASS = data corrente).
025100120216     P* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società operativa).
025200120216     P*--------------------------------------------------
025300120220     P TibsSof_GetIdSocietaByIdFiliale...
025400120216     P                 B                   EXPORT
025500120220     D TibsSof_GetIdSocietaByIdFiliale...
025600120216     D                 PI             3A
025700120216     D  priIdFiliale                  3P 0 CONST
025800120216     D  priDataRiferimento...
025900120216     D                                 D   DATFMT(*ISO)
026000120216     D                                     CONST
026100120216     D                                     OPTIONS(*NOPASS:*OMIT)
026200120216     D priTipoSocieta                 1A   CONST
026300120216     D                                     OPTIONS(*NOPASS)
026400120216
026500120216     D retField        S              3A   STATIC
026600120216
026700120216      /FREE
026800120216
026900120216       IF NOT done.init;
027000120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
027100120217         RETURN *BLANK;
027200120216       ENDIF;
027300120216
027400120216       IF priIdFiliale < 1;
027500120220         tibssof_esito = TIBSSOF_ESITO_ID_FILIALE_OBBLIGATORIO;
027600120216         RETURN *BLANK;
027700120216       ENDIF;
027800120216
027900120216       IF %PARMS() > 1 AND %ADDR(priDataRiferimento) <> *NULL
028000120216       AND priDataRiferimento > *LOVAL;
028100120216         dataRiferimento = priDataRiferimento;
028200120216       ELSE;
028300120216         RESET dataRiferimento;
028400120216       ENDIF;
028500120216
028600120216       IF %PARMS() > 2 AND priTipoSocieta > *BLANK;
028700120216         tipoSocieta = priTipoSocieta;
028800120216       ELSE;
028900120216         RESET tipoSocieta;
029000120216       ENDIF;
029100120216
029200120216       CLEAR retField;
029300120216
029400120216       EXEC SQL
029500120216         SELECT id_societa
029600120216           INTO :retField
029700120216           FROM azsof00f
029800120216           WHERE id_filiale = :priIdFiliale
029900120216             AND tipo_societa = :tipoSocieta
030000120216             AND :dataRiferimento BETWEEN validita_data_inizio
030100120216                                      AND validita_data_fine
030200120216       ;
030300120216
030400120217       SELECT;
030500120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
030600120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
030700120217         WHEN sqlCode < *ZERO;
030800120217           DUMP(A);
030900120220           tibssof_esito = sqlCode;
031000120217       ENDSL;
031100120216
031200120216       RETURN retField;
031300120216
031400120216      /END-FREE
031500120220     P TibsSof_GetIdSocietaByIdFiliale...
031600120216     P                 E
031700120217
031800120217
031900120217     P*--------------------------------------------------
032000120220     P* Procedure name: TibsSof_GetIdFilialiByIdSocieta
032100120217     P* Purpose:        Restituisce tutte le filiali di una società alla data.
032200120217     P* Returns:
032300120217     P* Parameter:      priIdSocieta => ID società.
032400120217     P* Parameter:      priDataRiferimento => Data riferimento (*NOPASS = d...
032500120217     P*                          ata corrente).
032600120217     P* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società o...
032700120217     P*                          perativa).
032800120217     P*--------------------------------------------------
032900120220     P TibsSof_GetIdFilialiByIdSocieta...
033000120217     P                 B                   EXPORT
033100120220     D TibsSof_GetIdFilialiByIdSocieta...
033200120217     D                 PI             5I 0 DIM(999)
033300120217     D  priIdSocieta                  3A   CONST
033400120217     D  priDataRiferimento...
033500120217     D                                 D   DATFMT(*ISO)
033600120217     D                                     CONST
033700120217     D                                     OPTIONS(*NOPASS:*OMIT)
033800120217     D priTipoSocieta                 1A   CONST
033900120217     D                                     OPTIONS(*NOPASS)
034000120217
034100120217     D retField        S              5I 0 DIM(999) STATIC
034200120217
034300120217      /FREE
034400120217
034500120217       CLEAR retField;
034600120220       CLEAR tibssof_esito;
034700120217
034800120217       IF NOT done.init;
034900120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
035000120217         RETURN retField;
035100120217       ENDIF;
035200120217
035300120217       IF priIdSocieta = *BLANK;
035400120220         tibssof_esito = TIBSSOF_ESITO_ID_SOCIETA_OBBLIGATORIO;
035500120217         RETURN retField;
035600120217       ENDIF;
035700120217
035800120217       IF %PARMS() > 1 AND %ADDR(priDataRiferimento) <> *NULL
035900120217       AND priDataRiferimento > *LOVAL;
036000120217         dataRiferimento = priDataRiferimento;
036100120217       ELSE;
036200120217         RESET dataRiferimento;
036300120217       ENDIF;
036400120217
036500120217       IF %PARMS() > 2 AND priTipoSocieta > *BLANK;
036600120217         tipoSocieta = priTipoSocieta;
036700120217       ELSE;
036800120217         RESET tipoSocieta;
036900120217       ENDIF;
037000120217
037100120217       EXEC SQL
037200120217         DECLARE filialiBySocieta NO SCROLL CURSOR FOR
037300120217           WITH azsof_filialiBySocieta (id_filiale) AS
037400120217           (
037500120217             SELECT id_filiale
037600120217             FROM azsof00f
037700120217             WHERE id_societa = :priIdSocieta
037800120217               AND tipo_societa = :tipoSocieta
037900120217               AND :dataRiferimento BETWEEN validita_data_inizio
038000120217                                        AND validita_data_fine
038100120217           )
038200120217           SELECT azsof_filialiBySocieta.id_filiale
038300120217           FROM azsof_filialiBySocieta
038400120217           ORDER BY azsof_filialiBySocieta.id_filiale
038500120217           FOR FETCH ONLY
038600120217       ;
038700120217
038800120217       EXEC SQL
038900120217         OPEN filialiBySocieta
039000120217       ;
039100120217
039200120217       IF sqlCode < *ZERO;
039300120217         DUMP(A);
039400120220         tibssof_esito = sqlCode;
039500120217         RETURN retField;
039600120217       ENDIF;
039700120217
039800120217       CLEAR filialiBySocieta;
039900120217
040000120217       EXEC SQL
040100120217         FETCH NEXT FROM filialiBySocieta
040200120217           FOR 999 ROWS
040300120217           INTO :filialiBySocieta
040400120217       ;
040500120217
040600120217       SELECT;
040700120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
040800120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
040900120217         WHEN sqlCode < *ZERO;
041000120217           DUMP(A);
041100120220           tibssof_esito = sqlCode;
041200120217       ENDSL;
041300120217
041400120217       EXEC SQL
041500120217         CLOSE filialiBySocieta
041600120217       ;
041700120217
041800120217       FOR i = 1 TO 999;
041900120217         IF filialiBySocieta(i).idFiliale = *ZERO;
042000120217           LEAVE;
042100120217         ENDIF;
042200120217         retField(i) = filialiBySocieta(i).idFiliale;
042300120217       ENDFOR;
042400120217
042500120217       RETURN retField;
042600120217
042700120217      /END-FREE
042800120220     P TibsSof_GetIdFilialiByIdSocieta...
042900120217     P                 E
043000120217
043100120217
043200120217     P*--------------------------------------------------
043300120220     P* Procedure name: TibsSof_GetOrganigrammaByIdFiliale
043400120217     P* Purpose:        Restituisce l'organigramma di una filiale.
043500120217     P* Returns:        AZORG00F
043600120217     P* Parameter:      priIdFiliale => ID filiale.
043700120217     D* Parameter:      priNomeFormato => Nome formato (*NOPASS = 'AZORG00F').
043800120217     D* Parameter:      priVersioneFormato => Versione formato (*NOPASS = 'A').
043900120217     P*--------------------------------------------------
044000120220     P TibsSof_GetOrganigrammaByIdFiliale...
044100120217     P                 B                   EXPORT
044200120220     D TibsSof_GetOrganigrammaByIdFiliale...
044300120217     D                 PI          4282A
044400120217     D  priIdFiliale                  3P 0 CONST
044500120217     D  priNomeFormato...
044600120220     D                               10I 0 CONST
044700120217     D                                     OPTIONS(*NOPASS)
044800120217     D  priVersioneFormato...
044900120217     D                                1A   CONST
045000120217     D                                     OPTIONS(*NOPASS)
045100120217
045200120217     D retField        S           4282A   STATIC
045300120217     D azorg00f      E DS                  BASED(azorg00fPtr) QUALIFIED
045400120217     D nomeFormato...
045500120220     D                 S             10I 0 STATIC
045600120220     D                                     INZ(TIBSSOF_NOME_FORMATO_AZORG00F)
045700120217     D versioneFormato...
045800120217     D                 S              1A   STATIC
045900120220     D                                     INZ(TIBSSOF_VERSIONE_FORMATO_A)
046000120217
046100120217      /FREE
046200120217
046300120217       IF NOT done.init;
046400120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
046500120217         RETURN *BLANK;
046600120217       ENDIF;
046700120217
046800120217       IF priIdFiliale < 1;
046900120220         tibssof_esito = TIBSSOF_ESITO_ID_FILIALE_OBBLIGATORIO;
047000120217         RETURN *BLANK;
047100120217       ENDIF;
047200120217
047300120223       CLEAR tibssof_esito;
047400120223
047500120220       IF %PARMS() > 1 AND priNomeFormato <> *ZERO;
047600120217         nomeFormato = priNomeFormato;
047700120217       ELSE;
047800120217         RESET nomeFormato;
047900120217       ENDIF;
048000120217
048100120217       IF %PARMS() > 2 AND priVersioneFormato <> *BLANK;
048200120217         versioneFormato = priVersioneFormato;
048300120217       ELSE;
048400120217         RESET versioneFormato;
048500120217       ENDIF;
048600120217
048700120220       IF nomeFormato = TIBSSOF_NOME_FORMATO_AZORG00F;
048800120217         azorg00fPtr = %ADDR(retField);
048900120217         CLEAR azorg00f;
049000120217         EXEC SQL
049100120217           SELECT *
049200120217             INTO :azorg00f
049300120217             FROM azorg00f
049400120217             WHERE orgFil = :priIdFiliale
049500120217         ;
049600120217       ELSE;
049700120220         tibssof_esito = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
049800120217         RETURN *BLANK;
049900120217       ENDIF;
050000120217
050100120217       SELECT;
050200120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
050300120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
050400120217         WHEN sqlCode < *ZERO;
050500120217           DUMP(A);
050600120220           tibssof_esito = sqlCode;
050700120217       ENDSL;
050800120217
050900120217       RETURN retField;
051000120217
051100120217      /END-FREE
051200120220     P TibsSof_GetOrganigrammaByIdFiliale...
051300120217     P                 E
051400120217
