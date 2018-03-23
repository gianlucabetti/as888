000100090407      /DEFINE DFTACTGRP_NO
000200090407
000300090318      //************************************************************************
000400090318      //
000500090318      // Stampa contratto.
000600090318      //
000700090318      //************************************************************************
000800090323     H DFTACTGRP(*NO) BNDDIR('QC2LE':'TIBS':'TRUL')
000900090318
001000090318      //************************************************************************
001100090318      //
001200090318      // Dichiarazione file.
001300090318      //
001400090318      //************************************************************************
001500090323     Ftrmz0100p O    E             PRINTER OFLIND(overFlow)
001600090318     F                                     PREFIX(prt_)
001700090319     F                                     INDDS(indPrtf)
001800090422     F                                     INFDS(infPrtf)
001900090323     F                                     USROPN
002000090407
002100090318      //************************************************************************
002200090318      //
002300090318      // Definizione costanti.
002400090318      //
002500090318      //************************************************************************
002600090318     D/COPY GAITRASRC/SRCCONST,ERRNO
002700090323     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
002800090326     D CCSID_ITALIAN...
002900090318     D                 C                   1144
003000090318     D CCSID_LATIN1_PCDATA...
003100090318     D                 C                   850
003200090326     D CCSID_MSWIN_LATIN1...
003300090326     D                 C                   1252
003400090318     D ESITO_ERROR...
003500090318     D                 C                   -1
003600090318     D ESITO_OK...
003700090318     D                 C                   0
003800090318     D O_RDONLY...
003900090319     D                 C                   1                                    Apertura in lettura
004000090318
004100090318      //************************************************************************
004200090318      //
004300090318      // Dichiarazione procedure esterne
004400090318      //
004500090318      //************************************************************************
004600090318     D/COPY GAITRASRC/SRCPROTOPR,ERRNO
004700090318     D/COPY GAITRASRC/SRCPROTOPR,ICONV
004800111125     D/COPY GAITRASRC/SRCPROTOPR,IFS
004900090323     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
005000090323     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
005100090318     D/COPY GAITRASRC/SRCPROTOPR,TRULISST
005200111128     D/COPY GAITRASRC/SRCPROTOPR,TRUL13R
005300090324     D Giustifica...
005400090319     D                 PR                  EXTPGM('XGIUST')
005500090319     D  txtInput...
005600090319     D                              200A   VARYING
005700090319     D  txtInputLen...
005800090319     D                                5I 0
005900090319     D  txtGiust...
006000090319     D                              200A   VARYING
006100090319     D  txtResto...
006200090319     D                              200A   VARYING
006300090318
006400090318      //************************************************************************
006500090318      //
006600090318      // Definizione strutture dati.
006700090318      //
006800090318      //************************************************************************
006900090319     D/COPY QSYSINC/QRPGLESRC,QTQICONV
007000090318     D fromCode...
007100090318     D                 DS                  LIKEDS(QTQCODE)
007200090318     D                                     INZ
007300090318     D toCode...
007400090318     D                 DS                  LIKEDS(QTQCODE)
007500090318     D                                     INZ
007600090319     D indPrtf...
007700090319     D                 DS                  QUALIFIED
007800090319     D                                     INZ
007900090319     D  grassetto...
008000090319     D                         1      1N
008100090422     D  stampareVettore...
008200090422     D                         2      2N
008300090422     D infPrtf...
008400090422     D                 DS                  QUALIFIED
008500090422     D  currentPage...
008600090422     D                       369    372I 0
008700090323     D tibsSocI0...
008800090323     D               E DS                  QUALIFIED
008900090323     D                                     INZ
009000090323     D tibsSocO0...
009100090323     D               E DS                  QUALIFIED
009200090323     D                                     INZ
009300090323     D trmz0100i...
009400090323     D               E DS                  QUALIFIED
009500090323     D                                     INZ
009600090323     D*trmz0100o...
009700090323     D*              E DS                  QUALIFIED
009800090323     D*                                    INZ
009900111128     D trul13ds...
010000111128     D               E DS                  QUALIFIED
010100111128     D                                     INZ
010200090318
010300090318      //************************************************************************
010400090318      //
010500090318      // Definizione variabili.
010600090318      //
010700090318      //************************************************************************
010800090319     D/COPY GAITRASRC/SRCPROTOPI,ICONV
010900090318     D/COPY GAITRASRC/SRCPROTOPI,ERRNO
011000111125     D a...
011100111125     D                 S              2S 0
011200111125     D cartellaTesti...
011300111125     D                 S            255A   VARYING
011400111128     D dataAccettazione...
011500111128     D                 S               D
011600090318     D esito...
011700090318     D                 S             10I 0
011800090318     D fd...
011900090318     D                 S             10I 0                                      File descriptor
012000090318     D inBuf...
012100090318     D                 S          32766
012200090318     D inBytesLeft...
012300090318     D                 S             10I 0
012400090318     D inBufPtr...
012500090318     D                 S               *   INZ(%ADDR(inBuf))
012600090324     D initDone...
012700090324     D                 S               N
012800090318     D outBuf...
012900090318     D                 S                   LIKE(inBuf)
013000090318     D outBytesLeft...
013100090318     D                 S             10I 0
013200090318     D outBufPtr...
013300090318     D                 S               *   INZ(%ADDR(outBuf))
013400090320     D outBufVar...
013500090320     D                 S          32740A   VARYING
013600090318     D p...
013700090318     D                 S              5U 0
013800090324     D copiaPerVettore...
013900090324     D                 S               N
014000090324     D testoIntroduzione...
014100090324     D                 S          32767A   VARYING
014200090324     D testoPartiMittente...
014300090324     D                 S          32767A   VARYING
014400090324     D testoPartiVettore...
014500090324     D                 S          32767A   VARYING
014600090324     D testoPremesso1...
014700090324     D                 S          32767A   VARYING
014800090324     D testoPremesso1a...
014900090324     D                 S          32767A   VARYING
015000090324     D testoPremesso1b...
015100090324     D                 S          32767A   VARYING
015200090324     D testoPremesso2...
015300090324     D                 S          32767A   VARYING
015400090324     D testoPremesso3...
015500090324     D                 S          32767A   VARYING
015600090324     D testoPremesso4...
015700090324     D                 S          32767A   VARYING
015800111125     D testoArticolo...
015900111125     D                 S          32767A   VARYING DIM(23)
016000111125     D titoloArticolo...
016100111125     D                 S            255A   VARYING DIM(23)
016200090324     D testoDichiarazioneVisione...
016300090324     D                 S          32767A   VARYING
016400090319     D txtInput...
016500090319     D                 S            200A   VARYING
016600090319     D txtInputLen...
016700090319     D                 S              5I 0
016800090319     D txtGiust...
016900090319     D                 S            200A   VARYING
017000090319     D txtResto...
017100090319     D                 S            200A   VARYING
017200111125     D versioneContratto...
017300111125     D                 S              1A
017400090318
017500090323     ***************************************************************************
017600090323     **
017700090323     ** Dichiarazione parametri programma.
017800090323     **
017900090323     ***************************************************************************
018000090323     D trmz0100r...
018100090323     D                 PI
018200090323     D  prmRqsOpCode...
018300090323     D                               10A   CONST
018400090323     D  prmRpyOpCode...
018500090323     D                               10A
018600090323     D  prmRpyIdMsg...
018700090323     D                               10I 0
018800090323     D  prmRqsFormato...
018900090323     D                               10A   CONST
019000090323     D                                     OPTIONS(*NOPASS)
019100090323     D  prmRqsData...
019200090324     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
019300090323     D  prmRqsDataSize...
019400090323     D                               10I 0 CONST
019500090323     D                                     OPTIONS(*NOPASS)
019600090324     D* prmRpyFormato...
019700090324     D*                              10A   CONST
019800090324     D*                                    OPTIONS(*NOPASS)
019900090324     D* prmRpyData...
020000090324     D*                           32767A   OPTIONS(*VARSIZE:*NOPASS)
020100090324     D* prmRpyDataSize...
020200090324     D*                              10I 0 CONST
020300090324     D*                                    OPTIONS(*NOPASS)
020400090323
020500090323     D*--------------------------------------------------
020600090323     D* Procedure name: PrtContratto
020700090323     D* Purpose:        Stampa un contratto.
020800090323     D* Returns:        Esito.
020900090323     D*--------------------------------------------------
021000090323     D PrtContratto    PR            10I 0
021100090323
021200090318     D*--------------------------------------------------
021300090319     D* Procedure name: RunRigaTesto
021400090319     D* Purpose:        Prepara e scrive una riga di testo.
021500090318     D* Returns:        Esito.
021600090318     D*--------------------------------------------------
021700090319     D RunRigaTesto    PR            10I 0
021800090324     D  piTestoSezione...
021900090324     D                            32740A   VARYING
022000090324     D                                     VALUE
022100090319
022200090319     D*--------------------------------------------------
022300090319     D* Procedure name: RunOverFlow
022400090319     D* Purpose:        Gestisce l'overflow.
022500090319     D* Returns:        Esito.
022600090319     D*--------------------------------------------------
022700090319     D RunOverFlow     PR            10I 0
022800090324
022900090324     D*--------------------------------------------------
023000090324     D* Procedure name: Init
023100090324     D* Purpose:        Inizializza il modulo.
023200090324     D* Returns:        Esito.
023300090324     D*--------------------------------------------------
023400090324     D Init            PR            10I 0
023500090324
023600090324     D*--------------------------------------------------
023700090324     D* Procedure name: GetTestoSezione
023800090324     D* Purpose:        Restituisce il testo di una sezione.
023900090324     D* Returns:        Testo della sezione.
024000090324     D* Parameter:      piNomeFile => Nome file che contiene il testo della...
024100090324     D*                           sezione.
024200090324     D*--------------------------------------------------
024300090324     DGetTestoSezione  PR         32767A   VARYING
024400090324     D  piNomeFile                  255A   VARYING
024500090324     D                                     VALUE
024600111124
024700111124     D*--------------------------------------------------
024800111124     D* Procedure name: GetVersioneContrattoByData
024900111124     D* Purpose:        Restituisce la versione del contratto in base alla ...
025000111124     D*                          data.
025100111124     D* Returns:        Versione.
025200111124     D* Parameter:      priData => Data per calcolo versione.
025300111124     D*--------------------------------------------------
025400111124     D GetVersioneContrattoByData...
025500111124     D                 PR             1A
025600111124     D  priData                        D   CONST
025700111125
025800111125     D*--------------------------------------------------
025900111125     D* Procedure name: GetCartellaTestiByVersioneContratto
026000111125     D* Purpose:        Restituisce la cartella dei testi della versione di...
026100111125     D*                           contratto.
026200111125     D* Returns:        Cartella testi.
026300111125     D* Parameter:      priVersioneContratto => Versione contratto
026400111125     D*--------------------------------------------------
026500111125     D GetCartellaTestiByVersioneContratto...
026600111125     D                 PR           255A   VARYING
026700111125     D  priVersioneContratto...
026800111125     D                                1A   CONST
026900111125
027000111125
027100111125
027200090319      /FREE
027300090318
027400090323       // Inizializzo i parametri di output.
027500090323
027600090323       prmRpyOpCode = 'DONE';
027700090323       CLEAR prmRpyIdMsg;
027800090323
027900090324       IF prmRqsOpCode = 'PRTCONTRAT' AND initDone;
028000090324
028100090324         // Controllo parametri ricevuti.
028200090324
028300090324         IF prmRqsFormato = 'TRMZ0100I';
028400090324           trmz0100i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
028500090324         ELSE;
028600090324           prmRpyOpCode = 'ERRORE';
028700090324           prmRpyIdMsg = ESITO_ERROR;
028800090324           RETURN;
028900090324         ENDIF;
029000090324
029100090324         // Parametri obbligatori.
029200090324
029300090324         IF trmz0100i.contratDat = *LOVAL OR trmz0100i.contratNum < 1
029400090324         OR trmz0100i.societa = *BLANK OR trmz0100i.filiale < 1
029500090324         OR %LEN(trmz0100i.vetRagSoc) = *ZERO OR trmz0100i.vetRagSoc = *BLANK
029600090324         OR %LEN(trmz0100i.vetSlCom) = *ZERO OR trmz0100i.vetSlCom = *BLANK
029700090324         OR %LEN(trmz0100i.vetSlInd) = *ZERO OR trmz0100i.vetSlInd = *BLANK
029800090324         OR %LEN(trmz0100i.vetCdFisca) = *ZERO OR trmz0100i.vetCdFisca = *BLANK
029900090324         OR %LEN(trmz0100i.vetCdIva) = *ZERO OR trmz0100i.vetCdIva = *BLANK
030000090324         OR %LEN(trmz0100i.vetRimLuo) = *ZERO OR trmz0100i.vetRimLuo = *BLANK
030100090324         OR %LEN(trmz0100i.vetRimNum) = *ZERO OR trmz0100i.vetRimNum = *BLANK
030200090422         ;
030300090422           prmRpyOpCode = 'ERRORE';
030400090324           prmRpyIdMsg = ESITO_ERROR;
030500090324           RETURN;
030600090324         ENDIF;
030700090324
030800090324         // Reperisco l'anagrafica della società mittente.
030900090324
031000090429         esito = Societa_NewSocieta( trmz0100i.societa
031100090429                                   : trmz0100i.contratDat
031200090429                                   );
031300090429
031400090429         IF esito = SOCIETA_INFO_SOCIETA_INESISTENTE
031500090429         OR esito < ESITO_OK;
031600090429           prmRpyOpCode = 'ERRORE';
031700090324           prmRpyIdMsg = ESITO_ERROR;
031800090324           RETURN;
031900090324         ENDIF;
032000090324
032100090324         tibsSocO0 = Societa_GetAnagrafica( 'TIBSSOCO0' );
032200090324
032300090324         // Imposto variabili fisse.
032400090324
032500090422         IF trmz0100i.dataStampa > *LOVAL;
032600090422           prt_dataStampa = trmz0100i.dataStampa;
032700090422         ELSE;
032800090422           prt_dataStampa = %DATE();
032900090422         ENDIF;
033000090422
033100090423         prt_luogoData = %TRIMR(tibsSocO0.sedLegLoc) + ', '
033200090423                       + %CHAR(trmz0100i.contratDat : *EUR);
033300090324
033400090324         p = ((%SIZE(prt_firmaSoc)
033500090324           - %LEN(%TRIMR(tibsSocO0.ragSociale))) / 2) + 1;
033600090617         CLEAR prt_firmaSoc;
033700090617         %SUBST( prt_firmaSoc : p ) = tibsSocO0.ragSociale;
033800090324
033900090324         prt_vetRagSoc = trmz0100i.vetRagSoc;
034000090324         prt_vetSlInd = trmz0100i.vetSlInd;
034100090324
034200090324         prt_vetSlCom = trmz0100i.vetSlCap
034300090324                      + ' ' + trmz0100i.vetSlCom
034400090324                      + ' ' + trmz0100i.vetSlPrv
034500090324                      ;
034600090324
034700090324         prt_contratNum = %EDITC(trmz0100i.contratNum : 'X')
034800090324                        + '/'
034900090324                        + %EDITC(trmz0100i.filiale : 'X')
035000090324                        ;
035100090323
035200090324         // Stampo la copia con la firma.
035300090324
035400090324         copiaPerVettore = *ON;
035500090324
035600090323         IF PrtContratto() < ESITO_OK;
035700090323           FEOD trmz0100p;
035800090323           prmRpyOpCode = 'ERRORE';
035900090323           prmRpyIdMsg = ESITO_ERROR;
036000090323           RETURN;
036100090323         ENDIF;
036200090324
036300090324         // Stampo la copia senza la firma.
036400090324
036500090324         copiaPerVettore = *OFF;
036600090324
036700090324         IF PrtContratto() < ESITO_OK;
036800090324           FEOD trmz0100p;
036900090324           prmRpyOpCode = 'ERRORE';
037000090324           prmRpyIdMsg = ESITO_ERROR;
037100090324           RETURN;
037200090324         ENDIF;
037300090323
037400090324       ELSEIF prmRqsOpCode = 'INIT' AND NOT initDone;
037500090324
037600090324         IF Init() < ESITO_OK;
037700090324           prmRpyOpCode = 'ERRORE';
037800090324           prmRpyIdMsg = ESITO_ERROR;
037900090324           RETURN;
038000090324         ENDIF;
038100090323
038200090324       ELSEIF prmRqsOpCode = 'FINALIZE' AND initDone;
038300090323
038400090323         CLOSE trmz0100p;
038500090323         ISST_finalize();
038600090323         Societa_finalize();
038700090323         CodeConversionDeallocation ( iconv_t );
038800090324         RESET initDone;
038900090323         *INLR = *ON;
039000090323
039100090323       ELSE;
039200090323
039300090323         prmRpyOpCode = 'ERRORE';
039400090323         prmRpyIdMsg = ESITO_ERROR;
039500090323
039600090323       ENDIF;
039700090318
039800090323       RETURN;
039900090323
040000090318      /END-FREE
040100090323
040200090323     P*--------------------------------------------------
040300090323     P* Procedure name: PrtContratto
040400090323     P* Purpose:        Stampa un contratto.
040500090323     P* Returns:        Esito.
040600090323     P*--------------------------------------------------
040700090323     P PrtContratto    B
040800090323     D PrtContratto    PI            10I 0
040900090323
041000090323      /FREE
041100090323
041200111125       // Recupero la versione del contratto in base alla prima data di stampa
041300111125       // del contratto.
041400111125
041500111125       versioneContratto = GetVersioneContrattoByData(trmz0100i.contratDat);
041600111125
041700111125       IF versioneContratto = *BLANK;
041800111125         RETURN ESITO_ERROR;
041900111125       ENDIF;
042000111125
042100111125       // Recupero la cartella IFS dei testi in base alla versione del contratto.
042200111125
042300111125       cartellaTesti = GetCartellaTestiByVersioneContratto(versioneContratto);
042400111125
042500111125       IF cartellaTesti = *BLANK;
042600111125         RETURN ESITO_ERROR;
042700111125       ENDIF;
042800111125
042900111125       // Carico le sezioni di testo.
043000111125
043100111125       testoIntroduzione = GetTestoSezione('contratto_introduzione.txt');
043200111125       testoPartiMittente = GetTestoSezione('contratto_parti_mittente.txt');
043300111125       testoPartiVettore = GetTestoSezione('contratto_parti_vettore.txt');
043400111125       testoPremesso1 = GetTestoSezione('contratto_premesso_1.txt');
043500111125       testoPremesso1a = GetTestoSezione('contratto_premesso_1a.txt');
043600111125       testoPremesso1b = GetTestoSezione('contratto_premesso_1b.txt');
043700111125       testoPremesso2 = GetTestoSezione('contratto_premesso_2.txt');
043800111125       testoPremesso3 = GetTestoSezione('contratto_premesso_3.txt');
043900111125       testoPremesso4 = GetTestoSezione('contratto_premesso_4.txt');
044000111125
044100111125       FOR a = 1 TO %ELEM(titoloArticolo);
044200111125         titoloArticolo(a) = GetTestoSezione('contratto_articolo_'
044300111125                           + %EDITC(a : 'X') + '_titolo.txt');
044400111125         testoArticolo(a)  = GetTestoSezione('contratto_articolo_'
044500111125                           + %EDITC(a : 'X') + '.txt');
044600111125       ENDFOR;
044700111125
044800111125       testoDichiarazioneVisione =
044900111125                         GetTestoSezione('contratto_dichiarazione_visione.txt');
045000111125
045100111125       // C'è stato un errore nel caricamento dei testi del contratto.
045200111125
045300111125       IF prmRpyIdMsg < ESITO_OK;
045400111125         RETURN ESITO_ERROR;
045500111125       ENDIF;
045600111125
045700090323       //***********************************************************************
045800090323       //
045900090323       // Stampo l'introduzione.
046000090323       //
046100090323       //***********************************************************************
046200090323
046300090422       RESET overFlow;
046400090323
046500090323       WRITE pagina;
046600090422       WRITE dtCertaBox;
046700090422       indPrtf.stampareVettore = *ON;
046800090422       WRITE dtCertaTxt;
046900090422       indPrtf.stampareVettore = *OFF;
047000090323       WRITE inizioDoc;
047100090323
047200090324       IF RunRigaTesto(testoIntroduzione) < ESITO_OK;
047300090323         RETURN ESITO_ERROR;
047400090323       ENDIF;
047500090323
047600090323       //***********************************************************************
047700090323       //
047800090323       // Stampo la sezione "Contratto di trasporto tra le parti ... e ...".
047900090323       //
048000090323       //***********************************************************************
048100090323
048200090323       RunOverFlow();
048300090323
048400090323       CLEAR prt_testo;
048500090323       %SUBST(prt_testo : 55) = 'CONTRATTO DI TRASPORTO';
048600090323       indPrtf.grassetto = *ON;
048700090323       WRITE riga;
048800090323       RESET indPrtf.grassetto;
048900090323
049000090323       RunOverFlow();
049100090323
049200090323       CLEAR prt_testo;
049300090323       %SUBST(prt_testo : 59) = 'Tra le parti:';
049400090323       WRITE riga;
049500090323
049600090324       IF RunRigaTesto(testoPartiMittente) < ESITO_OK;
049700090323         RETURN ESITO_ERROR;
049800090323       ENDIF;
049900090323
050000090323       RunOverFlow();
050100090323
050200090323       CLEAR prt_testo;
050300090323       %SUBST(prt_testo : 65) = 'e';
050400090323       WRITE riga;
050500090323
050600090324       IF RunRigaTesto(testoPartiVettore) < ESITO_OK;
050700090323         RETURN ESITO_ERROR;
050800090323       ENDIF;
050900090323
051000090323       //***********************************************************************
051100090323       //
051200090323       // Stampo la sezione "Premesso che".
051300090323       //
051400090323       //***********************************************************************
051500090323
051600090323       RunOverFlow();
051700090323
051800090323       CLEAR prt_testo;
051900090323       %SUBST(prt_testo : 59) = 'PREMESSO CHE:';
052000090323       WRITE riga;
052100090323
052200090324       IF RunRigaTesto(testoPremesso1) < ESITO_OK;
052300090323         RETURN ESITO_ERROR;
052400090323       ENDIF;
052500090323
052600090324       IF RunRigaTesto(testoPremesso1a) < ESITO_OK;
052700090323         RETURN ESITO_ERROR;
052800090323       ENDIF;
052900090323
053000090324       IF RunRigaTesto(testoPremesso1b) < ESITO_OK;
053100090323         RETURN ESITO_ERROR;
053200090323       ENDIF;
053300090323
053400090324       IF RunRigaTesto(testoPremesso2) < ESITO_OK;
053500090323         RETURN ESITO_ERROR;
053600090323       ENDIF;
053700090323
053800090324       IF RunRigaTesto(testoPremesso3) < ESITO_OK;
053900090323         RETURN ESITO_ERROR;
054000090323       ENDIF;
054100090323
054200090324       IF RunRigaTesto(testoPremesso4) < ESITO_OK;
054300090323         RETURN ESITO_ERROR;
054400090323       ENDIF;
054500090323
054600090323       //***********************************************************************
054700090323       //
054800090323       // Stampo gli articoli del contratto.
054900090323       //
055000090323       //***********************************************************************
055100090323
055200090323       RunOverFlow();
055300090323       CLEAR prt_testo;
055400090323       %SUBST(prt_testo : 54) = 'SI CONVIENE QUANTO SEGUE';
055500090323       WRITE riga;
055600111125
055700111125       FOR a = 1 TO %ELEM(titoloArticolo);
055800111125
055900111125         indPrtf.grassetto = *ON;
056000111125
056100111125         IF RunRigaTesto(titoloArticolo(a)) < ESITO_OK;
056200111125           RETURN ESITO_ERROR;
056300111125         ENDIF;
056400111125
056500111125         RESET indPrtf.grassetto;
056600111125
056700111125         IF RunRigaTesto(testoArticolo(a)) < ESITO_OK;
056800111125           RETURN ESITO_ERROR;
056900111125         ENDIF;
057000111125
057100111125       ENDFOR;
057200111125
057300090323       //***********************************************************************
057400090323       //
057500090324       // Stampo chiusura contratto e spazi per le firma.
057600090323       //
057700090323       //***********************************************************************
057800111128
057900111128       RESET trul13ds;
058000111128       trul13ds.I13FIL = trmz0100i.filiale;
058100111128       trul13ds.I13DIN = %DEC(trmz0100i.contratDat);
058200111128       trul13ds.I13GIO = 1;
058300111128       Trul13r(trul13ds);
058400111128
058500111128       IF trul13ds.O13ERR = *BLANK;
058600111128         dataAccettazione = %DATE(trul13ds.O13DFI : *ISO);
058700111128       ELSE;
058800111128         dataAccettazione = trmz0100i.contratDat + %DAYS(1);
058900111128       ENDIF;
059000111128
059100111128       RunOverFlow();
059200111128       CLEAR prt_testo;
059300111128       WRITE riga;
059400090324
059500090324       RunOverFlow();
059600090323       prt_testo = '(La mittente) ' + tibsSocO0.ragSociale;
059700090323       WRITE riga;
059800111128
059900111128       RunOverFlow();
060000111128       CLEAR prt_testo;
060100111128       WRITE riga;
060200111128
060300111128       RunOverFlow();
060400111128       //prt_testo = %TRIMR(tibsSocO0.sedLegLoc) + ', '
060500111128       //          + %CHAR(trmz0100i.contratDat : *EUR);
060600111128       prt_testo = trmz0100i.vetSlCom + ', '
060700111128                 + %CHAR(dataAccettazione : *EUR);
060800111128       WRITE riga;
060900090323
061000111027       RunOverFlow();
061100111128       prt_testo = '(Per accettazione)(Il vettore) ' +
061200090324                   '_________________________________________________________'
061300090324                 ;
061400090324       WRITE riga;
061500090324
061600090324       RunOverFlow();
061700090324       CLEAR prt_testo;
061800090324       WRITE riga;
061900090323
062000090324       IF RunRigaTesto(testoDichiarazioneVisione) < ESITO_OK;
062100090324         RETURN ESITO_ERROR;
062200090324       ENDIF;
062300090323
062400090324       RunOverFlow();
062500111128       CLEAR prt_testo;
062600111128       WRITE riga;
062700111128
062800111128       RunOverFlow();
062900111128       CLEAR prt_testo;
063000111128       WRITE riga;
063100090323
063200111128       RunOverFlow();
063300111128       prt_testo = trmz0100i.vetSlCom + ', '
063400111128                 + %CHAR(dataAccettazione : *EUR);
063500111128       WRITE riga;
063600090323
063700111125       RunOverFlow();
063800090324       prt_testo = '(Per accettazione)(Il vettore) ' +
063900090324                   '_________________________________________________________'
064000090324                 ;
064100090324       WRITE riga;
064200090324
064300090324       IF copiaPerVettore;
064400090420         WRITE firmaTxt;
064500090324         WRITE firmaImg;
064600090324       ENDIF;
064700090324
064800090324       FEOD trmz0100p;
064900090323
065000090323       RETURN ESITO_OK;
065100090323
065200090323      /END-FREE
065300090323     P PrtContratto    E
065400090323
065500090318
065600090318     P*--------------------------------------------------
065700090319     P* Procedure name: RunRigaTesto
065800090319     P* Purpose:        Prepara e scrive una riga di testo.
065900090318     P* Returns:        Esito.
066000090318     P*--------------------------------------------------
066100090319     P RunRigaTesto    B
066200090319     D RunRigaTesto    PI            10I 0
066300090324     D  piTestoSezione...
066400090324     D                            32740A   VARYING
066500090324     D                                     VALUE
066600090318
066700090318     D posizioneProgressiva...
066800090318     D                 S             10I 0
066900090318
067000090318      /FREE
067100090318
067200090318       // Ricerca e sostituzione delle variabili.
067300090318
067400090320       outBufVar = %TRIMR(outBuf);
067500090320
067600090318       EXEC SQL
067700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
067800090324                                      , '&MITTENTE_RAGIONE_SOCIALE'
067900090324                                      , :tibsSocO0.ragSociale
068000090324                                      )
068100090323       ;
068200090318
068300090318       EXEC SQL
068400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
068500090324                                      , '&MITTENTE_COMUNE'
068600090324                                      , :tibsSocO0.sedLegLoc
068700090324                                      )
068800090324       ;
068900090318
069000090318       EXEC SQL
069100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
069200090324                                      , '&MITTENTE_INDIRIZZO'
069300090324                                      , :tibsSocO0.sedLegInd
069400090324                                      )
069500090324       ;
069600090324
069700090324       EXEC SQL
069800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
069900090324                                      , '&MITTENTE_REGISTRO_IMPRESE_LUOGO'
070000090324                                      , :tibsSocO0.rimLuogo
070100090324                                      )
070200090324       ;
070300090324
070400090324       EXEC SQL
070500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
070600090324                                      , '&MITTENTE_REGISTRO_IMPRESE_NUMERO'
070700090324                                      , :tibsSocO0.codFiscale
070800090324                                      )
070900090324       ;
071000090324
071100090324       EXEC SQL
071200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
071300090324                                      , '&MITTENTE_REA_LUOGO'
071400090324                                      , :tibsSocO0.reaLuogo
071500090324                                      )
071600090324       ;
071700090324
071800090324       EXEC SQL
071900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
072000090324                                      , '&MITTENTE_REA_NUMERO'
072100090324                                      , :tibsSocO0.reaNumero
072200090324                                      )
072300090324       ;
072400090324
072500090324       EXEC SQL
072600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
072700090324                                      , '&MITTENTE_CODICE_FISCALE'
072800090324                                      , :tibsSocO0.codFiscale
072900090324                                      )
073000090324       ;
073100090324
073200090324       EXEC SQL
073300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
073400090324                                      , '&MITTENTE_PARTITA_IVA'
073500090324                                      , :tibsSocO0.partitaIva
073600090324                                      )
073700090324       ;
073800090319
073900090324       IF tibsSocO0.socioUnico = *ON;
074000090324         EXEC SQL
074100090324           SET :piTestoSezione = REPLACE( :piTestoSezione
074200090324                                        , '&MITTENTE_SOCIO_UNICO'
074300090424                                        , 'a socio unico'
074400090324                                        )
074500090324         ;
074600090324       ELSE;
074700090324         EXEC SQL
074800090324           SET :piTestoSezione = REPLACE( :piTestoSezione
074900090324                                        , '&MITTENTE_SOCIO_UNICO'
075000090324                                        , ''
075100090324                                        )
075200090324         ;
075300090324       ENDIF;
075400090324
075500090324       EXEC SQL
075600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
075700090324                                      , '&VETTORE_RAGIONE_SOCIALE'
075800090324                                      , :trmz0100i.vetRagSoc
075900090324                                      )
076000090324       ;
076100090324
076200090324       EXEC SQL
076300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
076400090324                                      , '&VETTORE_COMUNE'
076500090324                                      , :trmz0100i.vetSlCom
076600090324                                      )
076700090324       ;
076800090324
076900090324       EXEC SQL
077000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
077100090324                                      , '&VETTORE_INDIRIZZO'
077200090324                                      , :trmz0100i.vetSlInd
077300090324                                      )
077400090324       ;
077500090324
077600090324       EXEC SQL
077700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
077800090324                                      , '&VETTORE_REGISTRO_IMPRESE_LUOGO'
077900090324                                      , :trmz0100i.vetRimLuo
078000090324                                      )
078100090324       ;
078200090324
078300090324       EXEC SQL
078400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
078500090324                                      , '&VETTORE_REGISTRO_IMPRESE_NUMERO'
078600090324                                      , :trmz0100i.vetRimNum
078700090324                                      )
078800090324       ;
078900090324
079000090324       EXEC SQL
079100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
079200090324                                      , '&VETTORE_PARTITA_IVA'
079300090324                                      , :trmz0100i.vetCdIva
079400090324                                      )
079500090324       ;
079600090324
079700090324       EXEC SQL
079800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
079900090324                                      , '&VETTORE_REA_LUOGO'
080000090324                                      , :trmz0100i.vetReaLuo
080100090324                                      )
080200090324       ;
080300090324
080400090324       EXEC SQL
080500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
080600090324                                      , '&VETTORE_REA_NUMERO'
080700090324                                      , :trmz0100i.vetReaNum
080800090324                                      )
080900090324       ;
081000090324
081100090324       EXEC SQL
081200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
081300090324                                      , '&VETTORE_LEGALE_RAPPRESENTANTE'
081400090324                                      , :trmz0100i.vetLegRap
081500090324                                      )
081600090324       ;
081700090324
081800090324       EXEC SQL
081900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
082000090324                                      , '&VETTORE_ALBO_PROVINCIA'
082100090324                                      , :trmz0100i.vetAnaPrv
082200090324                                      )
082300090324       ;
082400090324
082500090324       EXEC SQL
082600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
082700090324                                      , '&VETTORE_ALBO_NUMERO'
082800090324                                      , :trmz0100i.vetAnaNum
082900090324                                      )
083000090324       ;
083100090318
083200090318       // Sottostringo.
083300090320
083400090324       outBuf = piTestoSezione;
083500090324       outBytesLeft = %LEN(piTestoSezione);
083600090318
083700090318       IF ISST_newStringa( outBuf
083800090318                         : outBytesLeft
083900090318                         ) < ESITO_OK;
084000090318         DUMP(A);
084100090318         RETURN ESITO_ERROR;
084200090318       ENDIF;
084300090318
084400090318       // Stampo le sottostrighe.
084500090318
084600090318       DOU posizioneProgressiva >= outBytesLeft;
084700090318         prt_testo = ISST_getSottoStringa( %SIZE(prt_testo)
084800090318                                         : posizioneProgressiva
084900090318                                         );
085000090319         RunOverFlow();
085100090319         txtInput = %TRIMR(prt_testo);
085200090319         txtInputLen = %SIZE(prt_testo);
085300090324         Giustifica( txtInput : txtInputLen : txtGiust : txtResto );
085400090319         prt_testo = txtGiust;
085500090319         WRITE riga;
085600090318       ENDDO;
085700090318
085800090318       RETURN ESITO_OK;
085900090318
086000090318      /END-FREE
086100090319     P RunRigaTesto    E
086200090318
086300090319
086400090319     P*--------------------------------------------------
086500090319     P* Procedure name: RunOverFlow
086600090319     P* Purpose:        Gestisce l'overflow.
086700090319     P* Returns:        Esito.
086800090319     P*--------------------------------------------------
086900090319     P RunOverFlow     B
087000090319     D RunOverFlow     PI            10I 0
087100090319
087200090319      /FREE
087300090319
087400090319       IF NOT overFlow;
087500090319         RETURN ESITO_OK;
087600090319       ENDIF;
087700090324
087800090324       IF copiaPerVettore;
087900090324         WRITE firmaTxt;
088000090324         WRITE firmaImg;
088100090324       ENDIF;
088200090324
088300090319       WRITE pagina;
088400090319       RESET overFlow;
088500090319
088600090422       IF infPrtf.currentPage = 3;
088700090422         WRITE dtCertaBox;
088800090422         WRITE dtCertaTxt;
088900090422       ENDIF;
089000090422
089100090319       RETURN ESITO_OK;
089200090319
089300090319      /END-FREE
089400090319     P RunOverFlow     E
089500090319
089600090324
089700090324     P*--------------------------------------------------
089800090324     P* Procedure name: Init
089900090324     P* Purpose:        Inizializza il modulo.
090000090324     P* Returns:        Esito.
090100090324     P*--------------------------------------------------
090200090324     P Init            B
090300090324     D Init            PI            10I 0
090400090324
090500090324      /FREE
090600090324
090700090324       // Inizializzo una conversione dati da ASCII a EBCDIC.
090800090324
090900111125       fromCode.QTQCCSID = CCSID_MSWIN_LATIN1;
091000090326       toCode.QTQCCSID = CCSID_ITALIAN;
091100090324       fromCode.QTQERVED02 = *ALLX'00';
091200090324       toCode.QTQERVED02 = *ALLX'00';
091300090324
091400090324       iconv_t = CodeConversionAllocation( %ADDR(toCode)
091500090324                                         : %ADDR(fromCode)
091600090324                                         );
091700090324
091800090324       // Inizializzo il modulo sottostringa intelligente.
091900090324
092000090324       IF ISST_init() < ESITO_OK;
092100090324         prmRpyOpCode = 'ERRORE';
092200090324         prmRpyIdMsg = ESITO_ERROR;
092300090324         RETURN ESITO_ERROR;
092400090324       ENDIF;
092500090324
092600090324       // Inizializzo il modulo anagrafico società.
092700090324
092800090324       IF Societa_Init() < ESITO_OK;
092900090324         prmRpyOpCode = 'ERRORE';
093000090324         prmRpyIdMsg = ESITO_ERROR;
093100090324         RETURN ESITO_ERROR;
093200090324       ENDIF;
093300090325
093400090324       // Posso finalmente aprire il printer file.
093500090324
093600090324       OPEN trmz0100p;
093700090324
093800090324       // Inizializzazione del programma completata.
093900090324
094000090324       initDone = *ON;
094100090324
094200090324       RETURN ESITO_OK;
094300090324
094400090324      /END-FREE
094500090324     P Init            E
094600090324
094700090324
094800090324     P*--------------------------------------------------
094900090324     P* Procedure name: GetTestoSezione
095000090324     P* Purpose:        Restituisce il testo di una sezione.
095100090324     P* Returns:        Testo della sezione.
095200090324     P* Parameter:      piNomeFile => Nome file che contiene il testo della...
095300090324     P*                           sezione.
095400090324     P*--------------------------------------------------
095500090324     PGetTestoSezione  B
095600090324     DGetTestoSezione  PI         32767A   VARYING
095700090324     D  piNomeFile                  255A   VARYING
095800090324     D                                     VALUE
095900090324
096000090324      /FREE
096100090324
096200111125       fd = IFS_OpenFile( cartellaTesti + '/' + piNomeFile
096300090324                        : O_RDONLY);
096400090324
096500090324       IF fd < 0;
096600090324         errnoPtr = getErrno();
096700090326         prmRpyIdMsg = errno * -1;
096800090326         prmRpyOpCode = 'ERRORE';
096900090324         DUMP(A);
097000090324         RETURN *BLANK;
097100090324       ENDIF;
097200090324
097300090324       RESET inBuf;
097400090324       RESET inBufPtr;
097500090324
097600090324       IFS_ReadFromDescriptor( fd : inBufPtr : %SIZE(inBuf) );
097700090324       IFS_CloseFile( fd );
097800090324
097900090324       // Conversione ASCII -> EBCDIC.
098000090324
098100090324       inBytesLeft = %LEN(%TRIMR(inBuf));
098200090324
098300090324       IF inBytesLeft = *ZERO;
098400090324         RETURN *BLANK;
098500090324       ENDIF;
098600090324
098700090324       outBytesLeft = inBytesLeft;
098800090324       CLEAR outBuf;
098900090324       RESET outBufPtr;
099000090324
099100090324       IF CodeConversion( iconv_t
099200090324                        : inBufPtr
099300090324                        : inBytesLeft
099400090324                        : outBufPtr
099500090324                        : outBytesLeft
099600090324                        ) < 0;
099700090324         errnoPtr = getErrno();
099800090326         prmRpyIdMsg = errno * -1;
099900090326         prmRpyOpCode = 'ERRORE';
100000090324         DUMP(A);
100100090324         RETURN *BLANK;
100200090324       ENDIF;
100300090324
100400090324       RETURN %TRIMR(outBuf);
100500090324
100600090324      /END-FREE
100700090324     PGetTestoSezione  E
100800090324
100900111124
101000111124     P*--------------------------------------------------
101100111124     P* Procedure name: GetVersioneContrattoByData
101200111124     P* Purpose:        Restituisce la versione del contratto in base alla ...
101300111124     P*                          data.
101400111124     P* Returns:        Versione.
101500111124     P* Parameter:      priData => Data per calcolo versione.
101600111124     P*--------------------------------------------------
101700111124     P GetVersioneContrattoByData...
101800111124     P                 B
101900111124     D GetVersioneContrattoByData...
102000111124     D                 PI             1A
102100111124     D  priData                        D   CONST
102200111124
102300111124     D retField        S              1A   STATIC
102400111124
102500111124      /FREE
102600111124
102700111124       CLEAR retField;
102800111124
102900111124       EXEC SQL
103000111124         SELECT LEFT(tbeUni, 1)
103100111124           INTO :retField
103200111124           FROM tntbe00f
103300111124           WHERE tbeCod = '007'
103400111124             AND tbeKe1 = '*VERSIONE'
103500111124             AND tbeKe2 <= :priData
103600111124             AND tbeLin = ''
103700111124             AND tbeSif = ''
103800111124             AND tbeAtb = ''
103900111124           ORDER BY tbeCod, tbeKe1, tbeKe2 DESC
104000111124           FETCH FIRST 1 ROW ONLY
104100111124       ;
104200111124
104300111124       SELECT;
104400111124         WHEN sqlCode < *ZERO;
104500111124           DUMP(A);
104600111124         WHEN sqlCode = 100;
104700111124           DUMP(A);
104800111124       ENDSL;
104900111124
105000111124       RETURN retField;
105100111124
105200111124      /END-FREE
105300111124     P GetVersioneContrattoByData...
105400111124     P                 E
105500111124
105600111125
105700111125     P*--------------------------------------------------
105800111125     P* Procedure name: GetCartellaTestiByVersioneContratto
105900111125     P* Purpose:        Restituisce la cartella dei testi della versione di...
106000111125     P*                           contratto.
106100111125     P* Returns:        Cartella testi.
106200111125     P* Parameter:      priVersioneContratto => Versione contratto
106300111125     P*--------------------------------------------------
106400111125     P GetCartellaTestiByVersioneContratto...
106500111125     P                 B
106600111125     D GetCartellaTestiByVersioneContratto...
106700111125     D                 PI           255A   VARYING
106800111125     D  priVersioneContratto...
106900111125     D                                1A   CONST
107000111125
107100111125     D retField        S            255A   VARYING STATIC
107200111125
107300111125      /FREE
107400111125
107500111125       CLEAR retField;
107600111125
107700111125       EXEC SQL
107800111125         SELECT RTRIM(tbeUni)
107900111125           INTO :retField
108000111125           FROM tntbe00f
108100111125           WHERE tbeCod = '007'
108200111125             AND tbeKe1 = '*IFSDIRTXT'
108300111125             AND tbeKe2 = :priVersioneContratto
108400111125             AND tbeLin = ''
108500111125             AND tbeSif = ''
108600111125             AND tbeAtb = ''
108700111125       ;
108800111125
108900111125       SELECT;
109000111125         WHEN sqlCode < *ZERO;
109100111125           DUMP(A);
109200111125         WHEN sqlCode = 100;
109300111125           DUMP(A);
109400111125       ENDSL;
109500111125
109600111125       RETURN retField;
109700111125
109800111125      /END-FREE
109900111125     P GetCartellaTestiByVersioneContratto...
110000111125     P                 E
110100111125
