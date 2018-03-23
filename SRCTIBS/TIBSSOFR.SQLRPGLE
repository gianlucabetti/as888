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
006700120517     D valDatIniz...
006800120517     D                 S               D
006900120517     D valDatFine...
007000120517     D                 S               D
007100120217
007200120217     ***************************************************************************
007300120217     **
007400120217     ** Parametri programma.
007500120217     **
007600120217     ***************************************************************************
007700120220     D TibsSof_Lettura...
007800120217     D                 PI
007900120217     D prmRqsOpCode...
008000120220     D                               10I 0
008100120217     D prmRpyOpCode...
008200120220     D                               10I 0
008300120217     D prmRpyIdMsg...
008400120217     D                               10I 0
008500120217     D prmRqsFormato...
008600120217     D                               10A
008700120217     D prmRqsData...
008800120217     D                            32767A   OPTIONS(*VARSIZE)
008900120217     D prmRqsDataSize...
009000120217     D                               10I 0
009100120217     D prmRpyFormato...
009200120217     D                               10A
009300120217     D prmRpyData...
009400120217     D                            32767A   OPTIONS(*VARSIZE)
009500120217     D prmRpyDataSize...
009600120217     D                               10I 0
009700120217
009800120217     ***************************************************************************
009900120217     **
010000120217     ** Main.
010100120217     **
010200120217     ***************************************************************************
010300120216
010400120217      /FREE
010500120217
010600120220       prmRpyOpCode = TIBSSOF_RPYOPCODE_DONE;
010700120217       CLEAR prmRpyIdMsg;
010800120217
010900120217       SELECT;
011000120217
011100120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_GET_IDSOCIETA_BY_IDFILIALE;
011200120217
011300120217           RESET tibsSof0i;
011400120217           RESET tibsSof0o;
011500120217
011600120217           SELECT;
011700120217             WHEN prmRqsFormato = tibsSof0i.formato;
011800120217               tibsSof0i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
011900120220               IF tibsSof0i.formato = prmRqsFormato;
012000120517                 idSocieta = TibsSof_GetIdSocietaByIdFiliale
012100120517                             ( tibsSof0i.idFiliale : tibsSof0i.dtValidita
012200120517                             : tibsSof0i.tpSocieta
012300120517                             : valDatIniz : valDatFine );
012400120220                 prmRpyIdMsg = tibssof_esito;
012500120220                 IF tibssof_esito < TIBSSOF_ESITO_OK;
012600120220                   prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
012700120220                 ENDIF;
012800120220               ELSE;
012900120220                 prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
013000120220                 prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
013100120220               ENDIF;
013200120217             OTHER;
013300120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
013400120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
013500120217           ENDSL;
013600120217
013700120217           SELECT;
013800120217             WHEN prmRpyFormato = tibsSof0o.formato;
013900120217               tibsSof0o.idSocieta = idSocieta;
014000120517               IF tibssof0o.versione = 'B';
014100120517                 tibssof0o.valDatIniz = valDatIniz;
014200120517                 tibssof0o.valDatFine = valDatFine;
014300120517               ENDIF;
014400120217               %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSof0o;
014500120217             OTHER;
014600120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
014700120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
014800120217           ENDSL;
014900120217
015000120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_GET_IDFILIALI_BY_IDSOCIETA;
015100120217
015200120217           RESET tibsSof1i;
015300120217           RESET tibsSof1o;
015400120217
015500120217           SELECT;
015600120217             WHEN prmRqsFormato = tibsSof1i.formato;
015700120217               tibsSof1i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
015800120220               IF prmRqsFormato = tibsSof1i.formato;
015900120220                idFilialiAry = TibsSof_GetIdFilialiByIdSocieta(
016000120220                                                            tibsSof1i.idSocieta
016100120220                                                          : tibsSof1i.dtValidita
016200120220                                                          : tibsSof1i.tpSocieta
016300120220                                                            );
016400120220                prmRpyIdMsg = tibssof_esito;
016500120220                IF tibssof_esito < TIBSSOF_ESITO_OK;
016600120220                  prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
016700120220                ENDIF;
016800120220               ELSE;
016900120220                 prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
017000120220                 prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
017100120220               ENDIF;
017200120217             OTHER;
017300120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
017400120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
017500120217           ENDSL;
017600120217
017700120217           SELECT;
017800120217             WHEN prmRpyFormato = tibsSof1o.formato;
017900120217               RESET tibsSof1o;
018000120217               tibsSof1o.idFiliali = idFilialiChr;
018100120217               %SUBST(prmRpyData : 1 : prmRpyDataSize) = tibsSof1o;
018200120217             OTHER;
018300120220               prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
018400120220               prmRpyIdMsg = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
018500120217           ENDSL;
018600120217
018700120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_INIT;
018800120217
018900120220           IF TibsSof_Init() < TIBSSOF_ESITO_OK;
019000120220             prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
019100120220             prmRpyIdMsg = tibssof_esito;
019200120217           ENDIF;
019300120217
019400120220         WHEN prmRqsOpCode = TIBSSOF_RQSOPCODE_FINALIZE;
019500120217
019600120220           TibsSof_Finalize();
019700120217           *INLR = *ON;
019800120217
019900120217         OTHER;
020000120217
020100120220           prmRpyOpCode = TIBSSOF_RPYOPCODE_ERROR;
020200120220           prmRpyIdMsg = TIBSSOF_ESITO_RQSOPCODE_SCONOSCIUTO;
020300120217
020400120217       ENDSL;
020500120217
020600120217       RETURN;
020700120217
020800120217      /END-FREE
020900120216
021000120216     P*--------------------------------------------------
021100120220     P* Procedure name: TibsSof_Init
021200120216     P* Purpose:        Inizializza il modulo.
021300120216     P* Returns:        Esito.
021400120216     P*--------------------------------------------------
021500120220     P TibsSof_Init    B                   EXPORT
021600120220     D TibsSof_Init    PI            10I 0
021700120216
021800120216      /FREE
021900120829
022000120829       CLEAR tibssof_esito;
022100120829
022200120216       IF done.init;
022300120829         tibssof_esito = TIBSSOF_ESITO_MODULO_GIA_INIZIALIZZATO;
022400120829         RETURN tibssof_esito;
022500120216       ENDIF;
022600120216
022700120216       done.init = *ON;
022800120216
022900120220       RETURN TIBSSOF_ESITO_OK;
023000120216
023100120216      /END-FREE
023200120220     P TibsSof_Init    E
023300120216
023400120216
023500120216     P*--------------------------------------------------
023600120220     P* Procedure name: TibsSof_Finalize
023700120216     P* Purpose:        Chiude il modulo.
023800120216     P* Returns:
023900120216     P*--------------------------------------------------
024000120220     P TibsSof_Finalize...
024100120220     P                 B                   EXPORT
024200120220     D TibsSof_Finalize...
024300120220     D                 PI
024400120216
024500120216      /FREE
024600120216
024700120216       RESET done;
024800120829       CLEAR tibssof_esito;
024900120829
025000120216      /END-FREE
025100120220     P TibsSof_Finalize...
025200120220     P                 E
025300120216
025400120216
025500120216     P*--------------------------------------------------
025600120220     P* Procedure name: TibsSof_GetIdSocietaByIdFiliale
025700120216     P* Purpose:        Restituisce la società di una filiale alla data.
025800120216     P* Returns:        ID società (*BLANK = errore).
025900120216     P* Parameter:      priIdFiliale => ID filiale.
026000120216     P* Parameter:      priDataRiferimento => Data riferimento per ricerca ...
026100120216     P*                          (*NOPASS = data corrente).
026200120216     P* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società operativa).
026300120216     P*--------------------------------------------------
026400120220     P TibsSof_GetIdSocietaByIdFiliale...
026500120216     P                 B                   EXPORT
026600120220     D TibsSof_GetIdSocietaByIdFiliale...
026700120216     D                 PI             3A
026800120517     D  priIdFiliale                  3P 0 CONST                                1
026900120517     D  priDataRiferimento...                                                   2
027000120216     D                                 D   DATFMT(*ISO)
027100120216     D                                     CONST
027200120216     D                                     OPTIONS(*NOPASS:*OMIT)
027300120517     D priTipoSocieta                 1A   CONST                                3
027400120517     D                                     OPTIONS(*NOPASS:*OMIT)
027500120517     D priDataValiditaInizio...                                                 4
027600120517     D                                 D   OPTIONS(*NOPASS)
027700120517     D priDataValiditaFine...                                                   5
027800120517     D                                 D   OPTIONS(*NOPASS)
027900120517
028000120216     D retField        S              3A   STATIC
028100120517     D local           DS                  STATIC QUALIFIED INZ
028200120517     D  valDatIniz                     D
028300120517     D  valDatFine                     D
028400120216
028500120216      /FREE
028600120829
028700120829       CLEAR tibssof_esito;
028800120829
028900120216       IF NOT done.init;
029000120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
029100120217         RETURN *BLANK;
029200120216       ENDIF;
029300120216
029400120216       IF priIdFiliale < 1;
029500120220         tibssof_esito = TIBSSOF_ESITO_ID_FILIALE_OBBLIGATORIO;
029600120216         RETURN *BLANK;
029700120216       ENDIF;
029800120216
029900120216       IF %PARMS() > 1 AND %ADDR(priDataRiferimento) <> *NULL
030000120216       AND priDataRiferimento > *LOVAL;
030100120216         dataRiferimento = priDataRiferimento;
030200120216       ELSE;
030300120216         RESET dataRiferimento;
030400120216       ENDIF;
030500120216
030600120517       IF %PARMS() > 2 AND %ADDR(priTipoSocieta) <> *NULL
030700120517       AND priTipoSocieta > *BLANK;
030800120216         tipoSocieta = priTipoSocieta;
030900120216       ELSE;
031000120216         RESET tipoSocieta;
031100120216       ENDIF;
031200120216
031300120517       RESET retField;
031400120517       RESET local;
031500120216
031600120216       EXEC SQL
031700120517         SELECT id_societa, validita_data_inizio, validita_data_fine
031800120517           INTO :retField, :local.valDatIniz, :local.valDatFine
031900120216           FROM azsof00f
032000120216           WHERE id_filiale = :priIdFiliale
032100120216             AND tipo_societa = :tipoSocieta
032200120216             AND :dataRiferimento BETWEEN validita_data_inizio
032300120216                                      AND validita_data_fine
032400120216       ;
032500120216
032600120217       SELECT;
032700120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
032800120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
032900120217         WHEN sqlCode < *ZERO;
033000120217           DUMP(A);
033100120220           tibssof_esito = sqlCode;
033200120217       ENDSL;
033300120216
033400120517       IF %PARMS() > 3;
033500120517         priDataValiditaInizio = local.valDatIniz;
033600120517         priDataValiditaFine = local.valDatFine;
033700120517       ENDIF;
033800120517
033900120216       RETURN retField;
034000120216
034100120216      /END-FREE
034200120220     P TibsSof_GetIdSocietaByIdFiliale...
034300120216     P                 E
034400120217
034500120217
034600120217     P*--------------------------------------------------
034700120220     P* Procedure name: TibsSof_GetIdFilialiByIdSocieta
034800120217     P* Purpose:        Restituisce tutte le filiali di una società alla data.
034900120217     P* Returns:
035000120217     P* Parameter:      priIdSocieta => ID società.
035100120217     P* Parameter:      priDataRiferimento => Data riferimento (*NOPASS = d...
035200120217     P*                          ata corrente).
035300120217     P* Parameter:      priTipoSocieta => Tipo società (*NOPASS = società o...
035400120217     P*                          perativa).
035500120217     P*--------------------------------------------------
035600120220     P TibsSof_GetIdFilialiByIdSocieta...
035700120217     P                 B                   EXPORT
035800120220     D TibsSof_GetIdFilialiByIdSocieta...
035900120217     D                 PI             5I 0 DIM(999)
036000120217     D  priIdSocieta                  3A   CONST
036100120217     D  priDataRiferimento...
036200120217     D                                 D   DATFMT(*ISO)
036300120217     D                                     CONST
036400120217     D                                     OPTIONS(*NOPASS:*OMIT)
036500120217     D priTipoSocieta                 1A   CONST
036600120217     D                                     OPTIONS(*NOPASS)
036700120217
036800120217     D retField        S              5I 0 DIM(999) STATIC
036900120217
037000120217      /FREE
037100120217
037200120217       CLEAR retField;
037300120220       CLEAR tibssof_esito;
037400120217
037500120217       IF NOT done.init;
037600120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
037700120217         RETURN retField;
037800120217       ENDIF;
037900120217
038000120217       IF priIdSocieta = *BLANK;
038100120220         tibssof_esito = TIBSSOF_ESITO_ID_SOCIETA_OBBLIGATORIO;
038200120217         RETURN retField;
038300120217       ENDIF;
038400120217
038500120217       IF %PARMS() > 1 AND %ADDR(priDataRiferimento) <> *NULL
038600120217       AND priDataRiferimento > *LOVAL;
038700120217         dataRiferimento = priDataRiferimento;
038800120217       ELSE;
038900120217         RESET dataRiferimento;
039000120217       ENDIF;
039100120217
039200120217       IF %PARMS() > 2 AND priTipoSocieta > *BLANK;
039300120217         tipoSocieta = priTipoSocieta;
039400120217       ELSE;
039500120217         RESET tipoSocieta;
039600120217       ENDIF;
039700120217
039800120217       EXEC SQL
039900120217         DECLARE filialiBySocieta NO SCROLL CURSOR FOR
040000120217           WITH azsof_filialiBySocieta (id_filiale) AS
040100120217           (
040200120217             SELECT id_filiale
040300120217             FROM azsof00f
040400120217             WHERE id_societa = :priIdSocieta
040500120217               AND tipo_societa = :tipoSocieta
040600120217               AND :dataRiferimento BETWEEN validita_data_inizio
040700120217                                        AND validita_data_fine
040800120217           )
040900120217           SELECT azsof_filialiBySocieta.id_filiale
041000120217           FROM azsof_filialiBySocieta
041100120217           ORDER BY azsof_filialiBySocieta.id_filiale
041200120217           FOR FETCH ONLY
041300120217       ;
041400120217
041500120217       EXEC SQL
041600120217         OPEN filialiBySocieta
041700120217       ;
041800120217
041900120217       IF sqlCode < *ZERO;
042000120217         DUMP(A);
042100120220         tibssof_esito = sqlCode;
042200120217         RETURN retField;
042300120217       ENDIF;
042400120217
042500120217       CLEAR filialiBySocieta;
042600120217
042700120217       EXEC SQL
042800120217         FETCH NEXT FROM filialiBySocieta
042900120217           FOR 999 ROWS
043000120217           INTO :filialiBySocieta
043100120217       ;
043200120217
043300120217       SELECT;
043400120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
043500120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
043600120217         WHEN sqlCode < *ZERO;
043700120217           DUMP(A);
043800120220           tibssof_esito = sqlCode;
043900120217       ENDSL;
044000120217
044100120217       EXEC SQL
044200120217         CLOSE filialiBySocieta
044300120217       ;
044400120217
044500120217       FOR i = 1 TO 999;
044600120217         IF filialiBySocieta(i).idFiliale = *ZERO;
044700120217           LEAVE;
044800120217         ENDIF;
044900120217         retField(i) = filialiBySocieta(i).idFiliale;
045000120217       ENDFOR;
045100120217
045200120217       RETURN retField;
045300120217
045400120217      /END-FREE
045500120220     P TibsSof_GetIdFilialiByIdSocieta...
045600120217     P                 E
045700120217
045800120217
045900120217     P*--------------------------------------------------
046000120220     P* Procedure name: TibsSof_GetOrganigrammaByIdFiliale
046100120217     P* Purpose:        Restituisce l'organigramma di una filiale.
046200120217     P* Returns:        AZORG00F
046300120217     P* Parameter:      priIdFiliale => ID filiale.
046400120217     D* Parameter:      priNomeFormato => Nome formato (*NOPASS = 'AZORG00F').
046500120217     D* Parameter:      priVersioneFormato => Versione formato (*NOPASS = 'A').
046600120217     P*--------------------------------------------------
046700120220     P TibsSof_GetOrganigrammaByIdFiliale...
046800120217     P                 B                   EXPORT
046900120220     D TibsSof_GetOrganigrammaByIdFiliale...
047000120217     D                 PI          4282A
047100120217     D  priIdFiliale                  3P 0 CONST
047200120217     D  priNomeFormato...
047300120220     D                               10I 0 CONST
047400120217     D                                     OPTIONS(*NOPASS)
047500120217     D  priVersioneFormato...
047600120217     D                                1A   CONST
047700120217     D                                     OPTIONS(*NOPASS)
047800120217
047900120217     D retField        S           4282A   STATIC
048000120217     D azorg00f      E DS                  BASED(azorg00fPtr) QUALIFIED
048100120217     D nomeFormato...
048200120220     D                 S             10I 0 STATIC
048300120220     D                                     INZ(TIBSSOF_NOME_FORMATO_AZORG00F)
048400120217     D versioneFormato...
048500120217     D                 S              1A   STATIC
048600120220     D                                     INZ(TIBSSOF_VERSIONE_FORMATO_A)
048700120217
048800120217      /FREE
048900120217
049000120217       IF NOT done.init;
049100120220         tibssof_esito = TIBSSOF_ESITO_MODULO_NON_INIZIALIZZATO;
049200120217         RETURN *BLANK;
049300120217       ENDIF;
049400120217
049500120217       IF priIdFiliale < 1;
049600120220         tibssof_esito = TIBSSOF_ESITO_ID_FILIALE_OBBLIGATORIO;
049700120217         RETURN *BLANK;
049800120217       ENDIF;
049900120217
050000120223       CLEAR tibssof_esito;
050100120223
050200120220       IF %PARMS() > 1 AND priNomeFormato <> *ZERO;
050300120217         nomeFormato = priNomeFormato;
050400120217       ELSE;
050500120217         RESET nomeFormato;
050600120217       ENDIF;
050700120217
050800120217       IF %PARMS() > 2 AND priVersioneFormato <> *BLANK;
050900120217         versioneFormato = priVersioneFormato;
051000120217       ELSE;
051100120217         RESET versioneFormato;
051200120217       ENDIF;
051300120217
051400120220       IF nomeFormato = TIBSSOF_NOME_FORMATO_AZORG00F;
051500120217         azorg00fPtr = %ADDR(retField);
051600120217         CLEAR azorg00f;
051700120217         EXEC SQL
051800120217           SELECT *
051900120217             INTO :azorg00f
052000120217             FROM azorg00f
052100120217             WHERE orgFil = :priIdFiliale
052200120217         ;
052300120217       ELSE;
052400120220         tibssof_esito = TIBSSOF_ESITO_NOME_FORMATO_SCONOSCIUTO;
052500120217         RETURN *BLANK;
052600120217       ENDIF;
052700120217
052800120217       SELECT;
052900120220         WHEN sqlCode = TIBSSOF_ROW_NOT_FOUND;
053000120220           tibssof_esito = TIBSSOF_ROW_NOT_FOUND;
053100120217         WHEN sqlCode < *ZERO;
053200120217           DUMP(A);
053300120220           tibssof_esito = sqlCode;
053400120217       ENDSL;
053500120217
053600120217       RETURN retField;
053700120217
053800120217      /END-FREE
053900120220     P TibsSof_GetOrganigrammaByIdFiliale...
054000120217     P                 E
054100120217
