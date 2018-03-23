000100120223     ***************************************************************************
000200120223     **
000300120223     ** Questo programma riceve il codice unificante di un cliente, compone la
000400120223     ** sua famiglia e la restituisce 99 elementi per volta.
000500120223     **
000600120223     ***************************************************************************
000700120224     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
000800120223
000900120223     ***************************************************************************
001000120223     **
001100120223     ** Costanti.
001200120223     **
001300120223     ***************************************************************************
001400120223     D ESITO_ERROR...
001500120223     D                 C                   -1
001600120223     D ESITO_OK...
001700120223     D                 C                   0
001800120224     D QTEMP...
001900120224     D                 C                   'QTEMP'
002000120223     D RPYOPCODE_DONE...
002100120223     D                 C                   'DONE'
002200120223     D RPYOPCODE_ERROR...
002300120223     D                 C                   'ERROR'
002400120223     D RQSOPCODE_GETKSUFAMI...
002500120223     D                 C                   'GETKSUFAMI'
002600120223     D TIBS10_CERCARE_FIGLI...
002700120223     D                 C                   'F'
002800120223
002900120223     ***************************************************************************
003000120223     **
003100120223     ** Strutture dati.
003200120223     **
003300120223     ***************************************************************************
003400120224      /COPY QSYSINC/QRPGLESRC,QUSEC
003500120223     D tibs10ds      E DS                  INZ QUALIFIED
003600120223     D  d10Paf       E                     INZ(TIBS10_CERCARE_FIGLI)
003700120223     D  d10Skc_ary                   11A   DIM(500) OVERLAY(d10Skc)
003800120224     D  d10Skc_aryAsc                11A   DIM(500) OVERLAY(d10Skc) ASCEND
003900120224     D ds_cnaco      E DS                  INZ QUALIFIED EXTNAME(cnaco00f)
004000120224     D ds_cnind      E DS                  INZ QUALIFIED EXTNAME(cnind00f)
004100120224     D ds_cnclp      E DS                  INZ QUALIFIED EXTNAME(cnclp00f)
004200120224     D ds_fncls      E DS                  INZ QUALIFIED EXTNAME(fncls00f)
004300120224     D tibs69ds      E DS                  INZ QUALIFIED
004400120223     D tis7st20i     E DS                  INZ(*EXTDFT) QUALIFIED
004500120223     D tis7st20o     E DS                  INZ(*EXTDFT) QUALIFIED
004600120223     D  idCliente_ary                 7A   DIM(99) OVERLAY(idCliente)
004700120224     D  dzCliente_ary                79A   DIM(99) OVERLAY(dzCliente)
004800120223
004900120223     ***************************************************************************
005000120223     **
005100120223     ** Campi.
005200120223     **
005300120223     ***************************************************************************
005400120224     D f               S             10I 0
005500120223     D rqsOpCode       S             10A
005600120223     D rqsData         S           9999A
005700120223     D rqsDataLen      S              4P 0
005800120223     D rpyOpCode       S             10A
005900120223     D rpyData         S           9999A
006000120223     D rpyDataLen      S              4P 0
006100120223
006200120223     ***************************************************************************
006300120223     **
006400120223     ** Prototipi.
006500120223     **
006600120223     ***************************************************************************
006700120224      /COPY GAITRASRC/SRCPROTOPR,QUSCHGUS
006800120224      /COPY GAITRASRC/SRCPROTOPR,QUSCRTUS
006900120224      /COPY GAITRASRC/SRCPROTOPR,QUSDLTUS
007000120224      /COPY GAITRASRC/SRCPROTOPR,QUSRTVUS
007100120224      /COPY GAITRASRC/SRCPROTOPR,TIBS10R
007200120224      /COPY GAITRASRC/SRCPROTOPR,TIBS69R
007300120224      /COPY GAITRASRC/SRCPROTOPR,TMPNAM
007400120223
007500120223     D*--------------------------------------------------
007600120223     D* Procedure name: ChkParametriRqs
007700120223     D* Purpose:        Controlla i parametri della richiesta.
007800120223     D* Returns:        Esito.
007900120223     D*--------------------------------------------------
008000120223     DChkParametriRqs  PR            10I 0
008100120223
008200120223     D*--------------------------------------------------
008300120223     D* Procedure name: Run
008400120223     D* Purpose:
008500120223     D* Returns:        Esito.
008600120223     D*--------------------------------------------------
008700120223     D Run             PR            10I 0
008800120223
008900120223     D*--------------------------------------------------
009000120223     D* Procedure name: GetTipoLegameBySun
009100120223     D* Purpose:        Restituisce il tipo legame dello Strategi User Number.
009200120223     D* Returns:        Tipo legame.
009300120223     D* Parameter:      priSun => Strategi User Number.
009400120223     D*--------------------------------------------------
009500120223     D GetTipoLegameBySun...
009600120223     D                 PR             2A
009700120223     D  priSun                        9A   CONST
009800120224
009900120224     D*--------------------------------------------------
010000120224     D* Procedure name: SetTibs10ds
010100120224     D* Purpose:        Imposta la struttura dati TIBS10DS.
010200120224     D* Returns:        Esito.
010300120224     D*--------------------------------------------------
010400120224     D SetTibs10ds     PR            10I 0
010500120224
010600120224     D*--------------------------------------------------
010700120224     D* Procedure name: GetDescrizioneCliente
010800120224     D* Purpose:        Restituisce la descrizione del cliente.
010900120224     D* Returns:        Descrizione cliente.
011000120224     D* Parameter:      priIdCliente => ID cliente
011100120224     D*--------------------------------------------------
011200120224     D GetDescrizioneCliente...
011300120224     D                 PR            79A
011400120224     D  priIdCliente                  7P 0 CONST
011500120224
011600120224
011700120223
011800120223     ***************************************************************************
011900120223     **
012000120223     ** Parametri.
012100120223     **
012200120223     ***************************************************************************
012300120223     C     *ENTRY        PLIST
012400120223     C                   PARM                    rqsOpCode
012500120223     C                   PARM                    rqsData
012600120223     C                   PARM                    rqsDataLen
012700120223     C                   PARM                    rpyOpCode
012800120223     C                   PARM                    rpyData
012900120223     C                   PARM                    rpyDataLen
013000120223
013100120223     ***************************************************************************
013200120223     **
013300120223     ** Main.
013400120223     **
013500120223     ***************************************************************************
013600120223
013700120223      /FREE
013800120223
013900120224       rpyOpCode = RPYOPCODE_ERROR;
014000120224       CLEAR rpyData;
014100120224       CLEAR rpyDataLen;
014200120224
014300120223       IF ChkParametriRqs() < ESITO_OK;
014400120223         RETURN;
014500120223       ENDIF;
014600120224
014700120224       IF Run() < ESITO_OK;
014800120224         RETURN;
014900120224       ENDIF;
015000120223
015100120224       rpyOpCode = RPYOPCODE_DONE;
015200120223       RETURN;
015300120223
015400120223      /END-FREE
015500120223
015600120223     P*--------------------------------------------------
015700120223     P* Procedure name: ChkParametriRqs
015800120223     P* Purpose:        Controlla i parametri della richiesta.
015900120223     P* Returns:        Esito.
016000120223     P*--------------------------------------------------
016100120223     PChkParametriRqs  B
016200120223     DChkParametriRqs  PI            10I 0
016300120223
016400120223      /FREE
016500120223
016600120223       IF rqsOpCode <> RQSOPCODE_GETKSUFAMI;
016700120223         RETURN ESITO_ERROR;
016800120223       ENDIF;
016900120223
017000120223       RESET tis7st20i;
017100120223
017200120223       SELECT;
017300120223         WHEN %SUBST(rqsData : 1 : 10) = tis7st20i.formato;
017400120223           tis7st20i = %SUBST(rqsData : 1 : rqsDataLen);
017500120223         OTHER;
017600120223           RETURN ESITO_ERROR;
017700120223       ENDSL;
017800120223
017900120223       IF tis7st20i.ksu = *BLANK OR tis7st20i.ksu = *ZERO;
018000120223         RETURN ESITO_ERROR;
018100120223       ENDIF;
018200120224
018300120224       IF tis7st20i.sun = *ZERO;
018400120224         RETURN ESITO_ERROR;
018500120224       ENDIF;
018600120224
018700120224       IF tis7st20i.prgCall > 5;
018800120224         RETURN ESITO_ERROR;
018900120224       ENDIF;
019000120224
019100120224       IF tis7st20i.prgCall > 0 AND tis7st20i.usrSpcName = *BLANK;
019200120224         RETURN ESITO_ERROR;
019300120224       ENDIF;
019400120223
019500120224       RETURN ESITO_OK;
019600120223
019700120223      /END-FREE
019800120223     PChkParametriRqs  E
019900120223
020000120223
020100120223     P*--------------------------------------------------
020200120223     P* Procedure name: Run
020300120223     P* Purpose:
020400120223     P* Returns:        Esito.
020500120223     P*--------------------------------------------------
020600120223     P Run             B
020700120223     D Run             PI            10I 0
020800120223
020900120223     D retField        S             10I 0 STATIC
021000120223     D i               S             10I 0 STATIC
021100120223     D p               S             10I 0 STATIC
021200120223
021300120224      /FREE
021400120223
021500120223       CLEAR retField;
021600120224       RESET tis7st20o;
021700120223
021800120224       IF SetTibs10ds() < ESITO_OK;
021900120224         RETURN ESITO_ERROR;
022000120224       ENDIF;
022100120224
022200120223       // Cerco l'ultimo elemento pieno.
022300120223
022400120224       f = %LOOKUP(*ZERO : tibs10ds.d10Skc_aryAsc);
022500120223
022600120223       IF f = *ZERO;
022700120223         RETURN ESITO_ERROR;
022800120223       ENDIF;
022900120223
023000120223       f -= 1;
023100120223       RESET p;
023200120223
023300120223       // Restituisco 99 elementi per volta con la seguente regola:
023400120224       // tis7st20i.prgCall = 0 da   1 a  99;
023500120224       // tis7st20i.prgCall = 1 da 100 a 198;
023600120224       // tis7st20i.prgCall = 2 da 199 a 297;
023700120224       // tis7st20i.prgCall = 3 da 298 a 396;
023800120224       // tis7st20i.prgCall = 4 da 397 a 495;
023900120224       // tis7st20i.prgCall = 5 da 496 a 500.
024000120223
024100120223       FOR i = (tis7st20i.prgCall * %ELEM(tis7st20o.idCliente_ary) + 1) TO f;
024200120223         p += 1;
024300120224         tis7st20o.idCliente_ary(p) = %SUBST(tibs10ds.d10Skc_aryAsc(i) : 5);
024400120224         tis7st20o.dzCliente_ary(p) = GetDescrizioneCliente(
024500120224                                          %DEC(tis7st20o.idCliente_ary(p):7:0));
024600120223         IF i = f;
024700120223           tis7st20o.datiFiniti = *ON;
024800120224           LEAVE;
024900120223         ENDIF;
025000120223         IF p = %ELEM(tis7st20o.idCliente_ary);
025100120223           LEAVE;
025200120223         ENDIF;
025300120223       ENDFOR;
025400120223
025500120223       tis7st20o.nrElementi = p;
025600120223
025700120227       IF tis7st20o.datiFiniti = *ON AND tis7st20o.usrSpcName <> *BLANK;
025800120224         CLEAR QUSEC;
025900120224         QUSBPRV = %SIZE(QUSEC);
026000120224         DeleteUserSpace(tis7st20o.usrSpcName + QTEMP : QUSEC);
026100120224       ENDIF;
026200120224
026300120224       rpyData = tis7st20o;
026400120224       rpyDataLen = %SIZE(tis7st20o);
026500120224
026600120223       RETURN retField;
026700120223
026800120223      /END-FREE
026900120223     P Run             E
027000120223
027100120223
027200120223     P*--------------------------------------------------
027300120223     P* Procedure name: GetTipoLegameBySun
027400120223     P* Purpose:        Restituisce il tipo legame dello Strategi User Number.
027500120223     P* Returns:        Tipo legame.
027600120223     P* Parameter:      priSun => Strategi User Number.
027700120223     P*--------------------------------------------------
027800120223     P GetTipoLegameBySun...
027900120223     P                 B
028000120223     D GetTipoLegameBySun...
028100120223     D                 PI             2A
028200120223     D  priSun                        9A   CONST
028300120223
028400120227     D retField        S              2A   STATIC
028500120223     D dataCorrenteDec...
028600120223     D                 S              8P 0
028700120223
028800120223      /FREE
028900120223
029000120223       RESET retField;
029100120223       dataCorrenteDec = %DEC(%DATE() : *ISO);
029200120223
029300120223       EXEC SQL
029400120223         SELECT vssTle
029500120223           INTO :retField
029600120223           FROM tivss00f
029700120223           WHERE vssSun = :priSun
029800120223             AND vssIsv = 'TT'
029900120223             AND :dataCorrenteDec BETWEEN vssDde AND vssDsc
030000120223       ;
030100120223
030200120223       IF sqlCode < *ZERO;
030300120223         DUMP(A);
030400120223       ENDIF;
030500120223
030600120223       RETURN retField;
030700120223
030800120223      /END-FREE
030900120223     P GetTipoLegameBySun...
031000120223     P                 E
031100120223
031200120224
031300120224     P*--------------------------------------------------
031400120224     P* Procedure name: SetTibs10ds
031500120224     P* Purpose:        Imposta la struttura dati TIBS10DS.
031600120224     P* Returns:        Esito.
031700120224     P*--------------------------------------------------
031800120224     P SetTibs10ds     B
031900120224     D SetTibs10ds     PI            10I 0
032000120224
032100120224     D qualifiedUserSpaceName...
032200120224     D                 S             16A   BASED(qualifiedUserSpaceNamePtr)
032300120224
032400120224      /FREE
032500120224
032600120224       // Prima chiamata:
032700120227       // - recupero tutti i figli del padre e se sono più di 99
032800120227       //   - memorizzo la famiglia in uno user space in QTEMP.
032900120227       //   - restituisco il nome dello user space creato.
033000120224       // Nelle chiamate successive ricevo il nome dello user space da cui
033100120224       // reperire il contenuto della struttura dati TIBS10DS.
033200120224
033300120224       IF tis7st20i.prgCall = 0; // Prima chiamata.
033400120224
033500120224         RESET tibs10ds;
033600120224         tibs10ds.d10Tle = GetTipoLegameBySun(%EDITC(tis7st20i.sun : 'X'));
033700120227
033800120227         IF tibs10ds.d10Tle = *BLANK;
033900120227           tibs10ds.d10Skc_ary(*) = *ZERO;
034000120227           EVALR tibs10ds.d10Skc_ary(1) = tis7st20i.ksu;
034100120227         ELSE;
034200120227           MONITOR;
034300120227             tibs10ds.d10Cod = %DEC(tis7st20i.ksu : 8 : 0);
034400120227           ON-ERROR;
034500120227             DUMP(A);
034600120227             RETURN ESITO_ERROR;
034700120227           ENDMON;
034800120227           tibs10r(tibs10ds);
034900120227           IF tibs10ds.d10Err <> *BLANK;
035000120227             EVALR tibs10ds.d10Skc_ary(1) = tis7st20i.ksu;
035100120227           ENDIF;
035200120227         ENDIF;
035300120227
035400120224         // Poichè nella posizione 1 c'è l'ID del padre e poi i figli,
035500120224         // per avere tutti gli ID in sequenza devo eseguire un SORTA.
035600120224
035700120224         f = %LOOKUP(*ZERO : tibs10ds.d10Skc_ary);
035800120224
035900120224         IF f = *ZERO;
036000120224           RETURN ESITO_ERROR;
036100120224         ENDIF;
036200120224
036300120224         f -= 1;
036400120224         SORTA %SUBARR(tibs10ds.d10Skc_ary : 1 : f);
036500120224
036600120227         IF f > %ELEM(tis7st20o.idCliente_ary);
036700120227
036800120227           // Produco un nome temporaneo.
036900120224
037000120227           qualifiedUserSpaceNamePtr = ProduceTemporaryFileName(*OMIT);
037100120224
037200120227           IF qualifiedUserSpaceNamePtr = *NULL;
037300120227             DUMP(A);
037400120227             RETURN ESITO_ERROR;
037500120227           ENDIF;
037600120224
037700120227           // A questo punto nel campo qualifiedUserSpaceName c'è un nome
037800120227           // qualificato tipo 'QTEMP/QACXxxxxxx'.
037900120224
038000120227           CLEAR QUSEC;
038100120227           QUSBPRV = %SIZE(QUSEC);
038200120224
038300120227           CreateUserSpace( %SUBST(qualifiedUserSpaceName : 7 : 10) + QTEMP
038400120227                          : 'TIBS10DS'
038500120227                          : %SIZE(tibs10ds)
038600120227                          : X'00'
038700120227                          : '*ALL'
038800120227                          : %SUBST(rqsData : 1 : 50)
038900120227                          : '*NO'
039000120227                          : QUSEC
039100120227                          );
039200120224
039300120227           IF QUSEI <> *BLANK;
039400120227             DUMP(A);
039500120227             RETURN ESITO_ERROR;
039600120227           ENDIF;
039700120224
039800120227           CLEAR QUSEC;
039900120227           QUSBPRV = %SIZE(QUSEC);
040000120224
040100120227           ChangeUserSpace( %SUBST(qualifiedUserSpaceName : 7 : 10) + QTEMP
040200120227                          : 1
040300120227                          : %SIZE(tibs10ds)
040400120227                          : tibs10ds
040500120227                          : '0'
040600120227                          : QUSEC
040700120227                          );
040800120224
040900120227           IF QUSEI <> *BLANK;
041000120227             DUMP(A);
041100120227             RETURN ESITO_ERROR;
041200120227           ENDIF;
041300120224
041400120227           tis7st20o.usrSpcName = %SUBST(qualifiedUserSpaceName : 7 : 10);
041500120224
041600120227         ENDIF;
041700120227
041800120224       ELSE; // Chiamate successive.
041900120224
042000120224         tis7st20o.usrSpcName = tis7st20i.usrSpcName;
042100120224
042200120224         CLEAR QUSEC;
042300120224         QUSBPRV = %SIZE(QUSEC);
042400120224
042500120224         RetrieveUserSpace( tis7st20i.usrSpcName + QTEMP
042600120224                          : 1
042700120224                          : %SIZE(tibs10ds)
042800120224                          : tibs10ds
042900120224                          : QUSEC
043000120224                          );
043100120224
043200120224         IF QUSEI <> *BLANK;
043300120224           DUMP(A);
043400120224           RETURN ESITO_ERROR;
043500120224         ENDIF;
043600120224
043700120224       ENDIF;
043800120224
043900120224       RETURN ESITO_OK;
044000120224
044100120224      /END-FREE
044200120224     P SetTibs10ds     E
044300120224
044400120224
044500120224     P*--------------------------------------------------
044600120224     P* Procedure name: GetDescrizioneCliente
044700120224     P* Purpose:        Restituisce la descrizione del cliente.
044800120224     P* Returns:        Descrizione cliente.
044900120224     P* Parameter:      priIdCliente => ID cliente
045000120224     P*--------------------------------------------------
045100120224     P GetDescrizioneCliente...
045200120224     P                 B
045300120224     D GetDescrizioneCliente...
045400120224     D                 PI            79A
045500120224     D  priIdCliente                  7P 0 CONST
045600120224
045700120224      /FREE
045800120224
045900120224       RESET tibs69ds;
046000120224       tibs69ds.i69Kac = priIdCliente;
046100120224       tibs69ds.i69Kin = priIdCliente;
046200120224       Tibs69r( tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
046300120224
046400120224       IF tibs69ds.o69Err <> *BLANK;
046500120224         RETURN *BLANK;
046600120224       ENDIF;
046700120224
046800120224       RETURN %TRIMR(ds_cnaco.acoRag) + ' ' + ds_cnind.indCit;
046900120224
047000120224      /END-FREE
047100120224     P GetDescrizioneCliente...
047200120224     P                 E
047300120224
