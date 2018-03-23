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
001800130128     D GAITRATMP...
001900130128     D                 C                   'GAITRATMP'
002000120224     D QTEMP...
002100120224     D                 C                   'QTEMP'
002200120223     D RPYOPCODE_DONE...
002300120223     D                 C                   'DONE'
002400120223     D RPYOPCODE_ERROR...
002500120223     D                 C                   'ERROR'
002600120223     D RQSOPCODE_GETKSUFAMI...
002700120223     D                 C                   'GETKSUFAMI'
002800120223     D TIBS10_CERCARE_FIGLI...
002900120223     D                 C                   'F'
003000120223
003100120223     ***************************************************************************
003200120223     **
003300120223     ** Strutture dati.
003400120223     **
003500120223     ***************************************************************************
003600120224      /COPY QSYSINC/QRPGLESRC,QUSEC
003700120223     D tibs10ds      E DS                  INZ QUALIFIED
003800120223     D  d10Paf       E                     INZ(TIBS10_CERCARE_FIGLI)
003900120223     D  d10Skc_ary                   11A   DIM(500) OVERLAY(d10Skc)
004000120224     D  d10Skc_aryAsc                11A   DIM(500) OVERLAY(d10Skc) ASCEND
004100120224     D ds_cnaco      E DS                  INZ QUALIFIED EXTNAME(cnaco00f)
004200120224     D ds_cnind      E DS                  INZ QUALIFIED EXTNAME(cnind00f)
004300120224     D ds_cnclp      E DS                  INZ QUALIFIED EXTNAME(cnclp00f)
004400120224     D ds_fncls      E DS                  INZ QUALIFIED EXTNAME(fncls00f)
004500120224     D tibs69ds      E DS                  INZ QUALIFIED
004600120223     D tis7st20i     E DS                  INZ(*EXTDFT) QUALIFIED
004700120223     D tis7st20o     E DS                  INZ(*EXTDFT) QUALIFIED
004800120309     D  idClienti_ary                 7A   DIM(99) OVERLAY(idClienti)
004900120309     D  dzClienti_ary                79A   DIM(99) OVERLAY(dzClienti)
005000130128     D usrSpc          DS                  QUALIFIED
005100130128     D  name                         10A
005200130128     D  library                      10A   INZ(GAITRATMP)
005300120223
005400120223     ***************************************************************************
005500120223     **
005600120223     ** Campi.
005700120223     **
005800120223     ***************************************************************************
005900120224     D f               S             10I 0
006000120223     D rqsOpCode       S             10A
006100120223     D rqsData         S           9999A
006200120223     D rqsDataLen      S              4P 0
006300120223     D rpyOpCode       S             10A
006400120223     D rpyData         S           9999A
006500120223     D rpyDataLen      S              4P 0
006600130128
006700120223     ***************************************************************************
006800120223     **
006900120223     ** Prototipi.
007000120223     **
007100120223     ***************************************************************************
007200120224      /COPY GAITRASRC/SRCPROTOPR,QUSCHGUS
007300120224      /COPY GAITRASRC/SRCPROTOPR,QUSCRTUS
007400120224      /COPY GAITRASRC/SRCPROTOPR,QUSDLTUS
007500120224      /COPY GAITRASRC/SRCPROTOPR,QUSRTVUS
007600120224      /COPY GAITRASRC/SRCPROTOPR,TIBS10R
007700120224      /COPY GAITRASRC/SRCPROTOPR,TIBS69R
007800120224      /COPY GAITRASRC/SRCPROTOPR,TMPNAM
007900130128      /COPY GAITRASRC/SRCPROTOPR,TRULTMPN
008000120223
008100120223     D*--------------------------------------------------
008200120223     D* Procedure name: ChkParametriRqs
008300120223     D* Purpose:        Controlla i parametri della richiesta.
008400120223     D* Returns:        Esito.
008500120223     D*--------------------------------------------------
008600120223     DChkParametriRqs  PR            10I 0
008700120223
008800120223     D*--------------------------------------------------
008900120223     D* Procedure name: Run
009000120223     D* Purpose:
009100120223     D* Returns:        Esito.
009200120223     D*--------------------------------------------------
009300120223     D Run             PR            10I 0
009400120223
009500120223     D*--------------------------------------------------
009600120223     D* Procedure name: GetTipoLegameBySun
009700120223     D* Purpose:        Restituisce il tipo legame dello Strategi User Number.
009800120223     D* Returns:        Tipo legame.
009900120223     D* Parameter:      priSun => Strategi User Number.
010000120223     D*--------------------------------------------------
010100120223     D GetTipoLegameBySun...
010200120223     D                 PR             2A
010300120223     D  priSun                        9A   CONST
010400120224
010500120224     D*--------------------------------------------------
010600120224     D* Procedure name: SetTibs10ds
010700120224     D* Purpose:        Imposta la struttura dati TIBS10DS.
010800120224     D* Returns:        Esito.
010900120224     D*--------------------------------------------------
011000120224     D SetTibs10ds     PR            10I 0
011100120224
011200120224     D*--------------------------------------------------
011300120224     D* Procedure name: GetDescrizioneCliente
011400120224     D* Purpose:        Restituisce la descrizione del cliente.
011500120224     D* Returns:        Descrizione cliente.
011600120224     D* Parameter:      priIdCliente => ID cliente
011700120224     D*--------------------------------------------------
011800120224     D GetDescrizioneCliente...
011900120224     D                 PR            79A
012000120224     D  priIdCliente                  7P 0 CONST
012100120224
012200120224
012300120223
012400120223     ***************************************************************************
012500120223     **
012600120223     ** Parametri.
012700120223     **
012800120223     ***************************************************************************
012900120223     C     *ENTRY        PLIST
013000120223     C                   PARM                    rqsOpCode
013100120223     C                   PARM                    rqsData
013200120223     C                   PARM                    rqsDataLen
013300120223     C                   PARM                    rpyOpCode
013400120223     C                   PARM                    rpyData
013500120223     C                   PARM                    rpyDataLen
013600120223
013700120223     ***************************************************************************
013800120223     **
013900120223     ** Main.
014000120223     **
014100120223     ***************************************************************************
014200120223
014300120223      /FREE
014400120223
014500120224       rpyOpCode = RPYOPCODE_ERROR;
014600120224       CLEAR rpyData;
014700120224       CLEAR rpyDataLen;
014800120224
014900120223       IF ChkParametriRqs() < ESITO_OK;
015000120223         RETURN;
015100120223       ENDIF;
015200120224
015300120224       IF Run() < ESITO_OK;
015400120224         RETURN;
015500120224       ENDIF;
015600120223
015700120224       rpyOpCode = RPYOPCODE_DONE;
015800120223       RETURN;
015900120223
016000120223      /END-FREE
016100120223
016200120223     P*--------------------------------------------------
016300120223     P* Procedure name: ChkParametriRqs
016400120223     P* Purpose:        Controlla i parametri della richiesta.
016500120223     P* Returns:        Esito.
016600120223     P*--------------------------------------------------
016700120223     PChkParametriRqs  B
016800120223     DChkParametriRqs  PI            10I 0
016900120223
017000120223      /FREE
017100120223
017200120223       IF rqsOpCode <> RQSOPCODE_GETKSUFAMI;
017300120223         RETURN ESITO_ERROR;
017400120223       ENDIF;
017500120223
017600120223       RESET tis7st20i;
017700120223
017800120223       SELECT;
017900120223         WHEN %SUBST(rqsData : 1 : 10) = tis7st20i.formato;
018000120223           tis7st20i = %SUBST(rqsData : 1 : rqsDataLen);
018100120223         OTHER;
018200120223           RETURN ESITO_ERROR;
018300120223       ENDSL;
018400120223
018500120223       IF tis7st20i.ksu = *BLANK OR tis7st20i.ksu = *ZERO;
018600120223         RETURN ESITO_ERROR;
018700120223       ENDIF;
018800120224
018900120224       IF tis7st20i.sun = *ZERO;
019000120224         RETURN ESITO_ERROR;
019100120224       ENDIF;
019200120224
019300120309       IF tis7st20i.prClienti > %ELEM(tibs10ds.d10Skc_ary);
019400120309         RETURN ESITO_ERROR;
019500120224       ENDIF;
019600120224
019700120309       IF tis7st20i.prClienti > 0 AND tis7st20i.usrSpcName = *BLANK;
019800120224         RETURN ESITO_ERROR;
019900120224       ENDIF;
020000120223
020100120224       RETURN ESITO_OK;
020200120223
020300120223      /END-FREE
020400120223     PChkParametriRqs  E
020500120223
020600120223
020700120223     P*--------------------------------------------------
020800120223     P* Procedure name: Run
020900120223     P* Purpose:
021000120223     P* Returns:        Esito.
021100120223     P*--------------------------------------------------
021200120223     P Run             B
021300120223     D Run             PI            10I 0
021400120223
021500120223     D retField        S             10I 0 STATIC
021600120223     D i               S             10I 0 STATIC
021700120223     D p               S             10I 0 STATIC
021800120223
021900120224      /FREE
022000120223
022100120223       CLEAR retField;
022200120224       RESET tis7st20o;
022300120223
022400120224       IF SetTibs10ds() < ESITO_OK;
022500120224         RETURN ESITO_ERROR;
022600120224       ENDIF;
022700120224
022800120223       // Cerco l'ultimo elemento pieno.
022900120223
023000130220       f = %LOOKUP(*ZERO : tibs10ds.d10Skc_ary);
023100120223
023200120223       IF f = *ZERO;
023300120223         RETURN ESITO_ERROR;
023400120223       ENDIF;
023500120223
023600120223       f -= 1;
023700120223       RESET p;
023800120223
023900120309       // Restituisco 99 elementi per volta.
024000120223
024100120309       FOR i = (tis7st20i.prClienti + 1) TO f;
024200120223         p += 1;
024300120309         tis7st20o.idClienti_ary(p) = %SUBST(tibs10ds.d10Skc_aryAsc(i) : 5);
024400120309         tis7st20o.dzClienti_ary(p) = GetDescrizioneCliente(
024500120309                                          %DEC(tis7st20o.idClienti_ary(p):7:0));
024600120309         IF tis7st20o.dzClienti_ary(p) = *BLANK;
024700120309           CLEAR tis7st20o.idClienti_ary(p);
024800120309           p -= 1;
024900120306         ENDIF;
025000120223         IF i = f;
025100120223           tis7st20o.datiFiniti = *ON;
025200120224           LEAVE;
025300120223         ENDIF;
025400120309         IF p = %ELEM(tis7st20o.idClienti_ary);
025500120223           LEAVE;
025600120223         ENDIF;
025700120223       ENDFOR;
025800120223
025900120309       tis7st20o.nrClienti = p;
026000120309       tis7st20o.prClienti = i;
026100120223
026200120227       IF tis7st20o.datiFiniti = *ON AND tis7st20o.usrSpcName <> *BLANK;
026300120224         CLEAR QUSEC;
026400120224         QUSBPRV = %SIZE(QUSEC);
026500130128         DeleteUserSpace(tis7st20o.usrSpcName + usrSpc.library : QUSEC);
026600120224       ENDIF;
026700120224
026800120224       rpyData = tis7st20o;
026900120224       rpyDataLen = %SIZE(tis7st20o);
027000120224
027100120223       RETURN retField;
027200120223
027300120223      /END-FREE
027400120223     P Run             E
027500120223
027600120223
027700120223     P*--------------------------------------------------
027800120223     P* Procedure name: GetTipoLegameBySun
027900120223     P* Purpose:        Restituisce il tipo legame dello Strategi User Number.
028000120223     P* Returns:        Tipo legame.
028100120223     P* Parameter:      priSun => Strategi User Number.
028200120223     P*--------------------------------------------------
028300120223     P GetTipoLegameBySun...
028400120223     P                 B
028500120223     D GetTipoLegameBySun...
028600120223     D                 PI             2A
028700120223     D  priSun                        9A   CONST
028800120223
028900120227     D retField        S              2A   STATIC
029000120223     D dataCorrenteDec...
029100120223     D                 S              8P 0
029200120223
029300120223      /FREE
029400120223
029500120223       RESET retField;
029600120223       dataCorrenteDec = %DEC(%DATE() : *ISO);
029700120223
029800120223       EXEC SQL
029900120223         SELECT vssTle
030000120223           INTO :retField
030100120223           FROM tivss00f
030200120223           WHERE vssSun = :priSun
030300120223             AND vssIsv = 'TT'
030400120223             AND :dataCorrenteDec BETWEEN vssDde AND vssDsc
030500120223       ;
030600120223
030700120223       IF sqlCode < *ZERO;
030800120223         DUMP(A);
030900120223       ENDIF;
031000120223
031100120223       RETURN retField;
031200120223
031300120223      /END-FREE
031400120223     P GetTipoLegameBySun...
031500120223     P                 E
031600120223
031700120224
031800120224     P*--------------------------------------------------
031900120224     P* Procedure name: SetTibs10ds
032000120224     P* Purpose:        Imposta la struttura dati TIBS10DS.
032100120224     P* Returns:        Esito.
032200120224     P*--------------------------------------------------
032300120224     P SetTibs10ds     B
032400120224     D SetTibs10ds     PI            10I 0
032500120224
032600120224     D qualifiedUserSpaceName...
032700120224     D                 S             16A   BASED(qualifiedUserSpaceNamePtr)
032800120224
032900120224      /FREE
033000120224
033100120224       // Prima chiamata:
033200120227       // - recupero tutti i figli del padre e se sono più di 99
033300120227       //   - memorizzo la famiglia in uno user space in QTEMP.
033400120227       //   - restituisco il nome dello user space creato.
033500120224       // Nelle chiamate successive ricevo il nome dello user space da cui
033600120224       // reperire il contenuto della struttura dati TIBS10DS.
033700120224
033800120309       IF tis7st20i.prClienti = 0; // Prima chiamata.
033900120224
034000120224         RESET tibs10ds;
034100120224         tibs10ds.d10Tle = GetTipoLegameBySun(%EDITC(tis7st20i.sun : 'X'));
034200120227
034300120227         IF tibs10ds.d10Tle = *BLANK;
034400120227           tibs10ds.d10Skc_ary(*) = *ZERO;
034500120227           EVALR tibs10ds.d10Skc_ary(1) = tis7st20i.ksu;
034600120227         ELSE;
034700120227           MONITOR;
034800120227             tibs10ds.d10Cod = %DEC(tis7st20i.ksu : 8 : 0);
034900120227           ON-ERROR;
035000120227             DUMP(A);
035100120227             RETURN ESITO_ERROR;
035200120227           ENDMON;
035300120227           tibs10r(tibs10ds);
035400120227           IF tibs10ds.d10Err <> *BLANK;
035500120227             EVALR tibs10ds.d10Skc_ary(1) = tis7st20i.ksu;
035600120227           ENDIF;
035700120227         ENDIF;
035800120227
035900120224         // Poichè nella posizione 1 c'è l'ID del padre e poi i figli,
036000120224         // per avere tutti gli ID in sequenza devo eseguire un SORTA.
036100120224
036200120224         f = %LOOKUP(*ZERO : tibs10ds.d10Skc_ary);
036300120224
036400120224         IF f = *ZERO;
036500120224           RETURN ESITO_ERROR;
036600120224         ENDIF;
036700120224
036800120224         f -= 1;
036900120224         SORTA %SUBARR(tibs10ds.d10Skc_ary : 1 : f);
037000120224
037100120309         IF f > %ELEM(tis7st20o.idClienti_ary);
037200120227
037300120227           // Produco un nome temporaneo.
037400120224
037500130128           //qualifiedUserSpaceNamePtr = ProduceTemporaryFileName(*OMIT);
037600120224
037700130128           //IF qualifiedUserSpaceNamePtr = *NULL;
037800130128           //  DUMP(A);
037900130128           //  RETURN ESITO_ERROR;
038000130128           //ENDIF;
038100120224
038200130128           RESET usrSpc;
038300130128           TrulTmpN( '*USRSPC' : *BLANK : usrSpc.library : usrSpc.name );
038400130128
038500130128           IF %SUBST(usrSpc.name : 1 : 1) = '*';
038600130128             DUMP(A);
038700130128             RETURN ESITO_ERROR;
038800130128           ENDIF;
038900130128
039000120227           // A questo punto nel campo qualifiedUserSpaceName c'è un nome
039100120227           // qualificato tipo 'QTEMP/QACXxxxxxx'.
039200120224
039300120227           CLEAR QUSEC;
039400120227           QUSBPRV = %SIZE(QUSEC);
039500120224
039600130128           CreateUserSpace( usrSpc
039700120227                          : 'TIBS10DS'
039800120227                          : %SIZE(tibs10ds)
039900120227                          : X'00'
040000120227                          : '*ALL'
040100120227                          : %SUBST(rqsData : 1 : 50)
040200120227                          : '*NO'
040300120227                          : QUSEC
040400120227                          );
040500120224
040600120227           IF QUSEI <> *BLANK;
040700120227             DUMP(A);
040800120227             RETURN ESITO_ERROR;
040900120227           ENDIF;
041000120224
041100120227           CLEAR QUSEC;
041200120227           QUSBPRV = %SIZE(QUSEC);
041300120224
041400130128           ChangeUserSpace( usrSpc
041500120227                          : 1
041600120227                          : %SIZE(tibs10ds)
041700120227                          : tibs10ds
041800120227                          : '0'
041900120227                          : QUSEC
042000120227                          );
042100120224
042200120227           IF QUSEI <> *BLANK;
042300120227             DUMP(A);
042400120227             RETURN ESITO_ERROR;
042500120227           ENDIF;
042600120224
042700130128           tis7st20o.usrSpcName = usrSpc.name;
042800120224
042900120227         ENDIF;
043000120227
043100120224       ELSE; // Chiamate successive.
043200120224
043300120224         tis7st20o.usrSpcName = tis7st20i.usrSpcName;
043400120224
043500120224         CLEAR QUSEC;
043600120224         QUSBPRV = %SIZE(QUSEC);
043700120224
043800130128         RetrieveUserSpace( tis7st20i.usrSpcName + usrSpc.library
043900120224                          : 1
044000120224                          : %SIZE(tibs10ds)
044100120224                          : tibs10ds
044200120224                          : QUSEC
044300120224                          );
044400120224
044500120224         IF QUSEI <> *BLANK;
044600120224           DUMP(A);
044700120224           RETURN ESITO_ERROR;
044800120224         ENDIF;
044900120224
045000120224       ENDIF;
045100120224
045200120224       RETURN ESITO_OK;
045300120224
045400120224      /END-FREE
045500120224     P SetTibs10ds     E
045600120224
045700120224
045800120224     P*--------------------------------------------------
045900120224     P* Procedure name: GetDescrizioneCliente
046000120224     P* Purpose:        Restituisce la descrizione del cliente.
046100120224     P* Returns:        Descrizione cliente.
046200120224     P* Parameter:      priIdCliente => ID cliente
046300120224     P*--------------------------------------------------
046400120224     P GetDescrizioneCliente...
046500120224     P                 B
046600120224     D GetDescrizioneCliente...
046700120224     D                 PI            79A
046800120224     D  priIdCliente                  7P 0 CONST
046900120224
047000120224      /FREE
047100120224
047200120224       RESET tibs69ds;
047300120224       tibs69ds.i69Kac = priIdCliente;
047400120224       tibs69ds.i69Kin = priIdCliente;
047500120224       Tibs69r( tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
047600120224
047700130321       IF tibs69ds.o69Err <> *BLANK OR ds_cnaco.acoAbl <> *BLANK;
047800120224         RETURN *BLANK;
047900120224       ENDIF;
048000120224
048100120224       RETURN %TRIMR(ds_cnaco.acoRag) + ' ' + ds_cnind.indCit;
048200120224
048300120224      /END-FREE
048400120224     P GetDescrizioneCliente...
048500120224     P                 E
048600120224
