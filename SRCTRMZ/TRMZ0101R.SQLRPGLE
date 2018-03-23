000100111201      /DEfINE DFTACTGRP_NO
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
001500111128     Ftrmz0101p O    E             PRINTER OFLIND(overFlow)
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
004000111128     D SUBDIR_APPENDICE...
004100111128     D                 C                   '/appendice_a'
004200090318
004300090318      //************************************************************************
004400090318      //
004500090318      // Dichiarazione procedure esterne
004600090318      //
004700090318      //************************************************************************
004800090318     D/COPY GAITRASRC/SRCPROTOPR,ERRNO
004900090318     D/COPY GAITRASRC/SRCPROTOPR,ICONV
005000111125     D/COPY GAITRASRC/SRCPROTOPR,IFS
005100090323     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
005200111128     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0101R
005300090318     D/COPY GAITRASRC/SRCPROTOPR,TRULISST
005400111128     D/COPY GAITRASRC/SRCPROTOPR,TRUL13R
005500090324     D Giustifica...
005600090319     D                 PR                  EXTPGM('XGIUST')
005700090319     D  txtInput...
005800090319     D                              200A   VARYING
005900090319     D  txtInputLen...
006000090319     D                                5I 0
006100090319     D  txtGiust...
006200090319     D                              200A   VARYING
006300090319     D  txtResto...
006400090319     D                              200A   VARYING
006500090318
006600090318      //************************************************************************
006700090318      //
006800090318      // Definizione strutture dati.
006900090318      //
007000090318      //************************************************************************
007100090319     D/COPY QSYSINC/QRPGLESRC,QTQICONV
007200090318     D fromCode...
007300090318     D                 DS                  LIKEDS(QTQCODE)
007400090318     D                                     INZ
007500090318     D toCode...
007600090318     D                 DS                  LIKEDS(QTQCODE)
007700090318     D                                     INZ
007800090319     D indPrtf...
007900090319     D                 DS                  QUALIFIED
008000090319     D                                     INZ
008100090319     D  grassetto...
008200090319     D                         1      1N
008300090422     D  stampareVettore...
008400090422     D                         2      2N
008500090422     D infPrtf...
008600090422     D                 DS                  QUALIFIED
008700090422     D  currentPage...
008800090422     D                       369    372I 0
008900090323     D tibsSocI0...
009000090323     D               E DS                  QUALIFIED
009100090323     D                                     INZ
009200090323     D tibsSocO0...
009300090323     D               E DS                  QUALIFIED
009400090323     D                                     INZ
009500111128     D trmz0101i...
009600090323     D               E DS                  QUALIFIED
009700090323     D                                     INZ
009800111128     D*trmz0101o...
009900090323     D*              E DS                  QUALIFIED
010000090323     D*                                    INZ
010100111128     D trul13ds...
010200111128     D               E DS                  QUALIFIED
010300111128     D                                     INZ
010400090318
010500090318      //************************************************************************
010600090318      //
010700090318      // Definizione variabili.
010800090318      //
010900090318      //************************************************************************
011000090319     D/COPY GAITRASRC/SRCPROTOPI,ICONV
011100090318     D/COPY GAITRASRC/SRCPROTOPI,ERRNO
011200111125     D a...
011300111125     D                 S              2S 0
011400111125     D cartellaTesti...
011500111125     D                 S            255A   VARYING
011600111128     D dataAccettazione...
011700111128     D                 S               D
011800090318     D esito...
011900090318     D                 S             10I 0
012000090318     D fd...
012100090318     D                 S             10I 0                                      File descriptor
012200090318     D inBuf...
012300090318     D                 S          32766
012400090318     D inBytesLeft...
012500090318     D                 S             10I 0
012600090318     D inBufPtr...
012700090318     D                 S               *   INZ(%ADDR(inBuf))
012800090324     D initDone...
012900090324     D                 S               N
013000090318     D outBuf...
013100090318     D                 S                   LIKE(inBuf)
013200090318     D outBytesLeft...
013300090318     D                 S             10I 0
013400090318     D outBufPtr...
013500090318     D                 S               *   INZ(%ADDR(outBuf))
013600090320     D outBufVar...
013700090320     D                 S          32740A   VARYING
013800090318     D p...
013900090318     D                 S              5U 0
014000090324     D copiaPerVettore...
014100090324     D                 S               N
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
015900111128     D                 S          32767A   VARYING DIM(02)
016000111125     D titoloArticolo...
016100111128     D                 S            255A   VARYING DIM(02)
016200090319     D txtInput...
016300090319     D                 S            200A   VARYING
016400090319     D txtInputLen...
016500090319     D                 S              5I 0
016600090319     D txtGiust...
016700090319     D                 S            200A   VARYING
016800090319     D txtResto...
016900090319     D                 S            200A   VARYING
017000090318
017100090323     ***************************************************************************
017200090323     **
017300090323     ** Dichiarazione parametri programma.
017400090323     **
017500090323     ***************************************************************************
017600111128     D trmz0101r...
017700090323     D                 PI
017800090323     D  prmRqsOpCode...
017900090323     D                               10A   CONST
018000090323     D  prmRpyOpCode...
018100090323     D                               10A
018200090323     D  prmRpyIdMsg...
018300090323     D                               10I 0
018400090323     D  prmRqsFormato...
018500090323     D                               10A   CONST
018600090323     D                                     OPTIONS(*NOPASS)
018700090323     D  prmRqsData...
018800090324     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
018900090323     D  prmRqsDataSize...
019000090323     D                               10I 0 CONST
019100090323     D                                     OPTIONS(*NOPASS)
019200090324     D* prmRpyFormato...
019300090324     D*                              10A   CONST
019400090324     D*                                    OPTIONS(*NOPASS)
019500090324     D* prmRpyData...
019600090324     D*                           32767A   OPTIONS(*VARSIZE:*NOPASS)
019700090324     D* prmRpyDataSize...
019800090324     D*                              10I 0 CONST
019900090324     D*                                    OPTIONS(*NOPASS)
020000090323
020100090323     D*--------------------------------------------------
020200090323     D* Procedure name: PrtContratto
020300090323     D* Purpose:        Stampa un contratto.
020400090323     D* Returns:        Esito.
020500090323     D*--------------------------------------------------
020600090323     D PrtContratto    PR            10I 0
020700090323
020800090318     D*--------------------------------------------------
020900090319     D* Procedure name: RunRigaTesto
021000090319     D* Purpose:        Prepara e scrive una riga di testo.
021100090318     D* Returns:        Esito.
021200090318     D*--------------------------------------------------
021300090319     D RunRigaTesto    PR            10I 0
021400090324     D  piTestoSezione...
021500090324     D                            32740A   VARYING
021600090324     D                                     VALUE
021700090319
021800090319     D*--------------------------------------------------
021900090319     D* Procedure name: RunOverFlow
022000090319     D* Purpose:        Gestisce l'overflow.
022100090319     D* Returns:        Esito.
022200090319     D*--------------------------------------------------
022300090319     D RunOverFlow     PR            10I 0
022400090324
022500090324     D*--------------------------------------------------
022600090324     D* Procedure name: Init
022700090324     D* Purpose:        Inizializza il modulo.
022800090324     D* Returns:        Esito.
022900090324     D*--------------------------------------------------
023000090324     D Init            PR            10I 0
023100090324
023200090324     D*--------------------------------------------------
023300090324     D* Procedure name: GetTestoSezione
023400090324     D* Purpose:        Restituisce il testo di una sezione.
023500090324     D* Returns:        Testo della sezione.
023600090324     D* Parameter:      piNomeFile => Nome file che contiene il testo della...
023700090324     D*                           sezione.
023800090324     D*--------------------------------------------------
023900090324     DGetTestoSezione  PR         32767A   VARYING
024000090324     D  piNomeFile                  255A   VARYING
024100090324     D                                     VALUE
024200111124
024300111124     D*--------------------------------------------------
024400111124     D* Procedure name: GetVersioneContrattoByData
024500111124     D* Purpose:        Restituisce la versione del contratto in base alla ...
024600111124     D*                          data.
024700111124     D* Returns:        Versione.
024800111124     D* Parameter:      priData => Data per calcolo versione.
024900111124     D*--------------------------------------------------
025000111124     D GetVersioneContrattoByData...
025100111124     D                 PR             1A
025200111124     D  priData                        D   CONST
025300111125
025400111125     D*--------------------------------------------------
025500111125     D* Procedure name: GetCartellaTestiByVersioneContratto
025600111125     D* Purpose:        Restituisce la cartella dei testi della versione di...
025700111125     D*                           contratto.
025800111125     D* Returns:        Cartella testi.
025900111125     D* Parameter:      priVersioneContratto => Versione contratto
026000111125     D*--------------------------------------------------
026100111125     D GetCartellaTestiByVersioneContratto...
026200111125     D                 PR           255A   VARYING
026300111125     D  priVersioneContratto...
026400111125     D                                1A   CONST
026500111125
026600111125
026700111125
026800090319      /FREE
026900090318
027000090323       // Inizializzo i parametri di output.
027100090323
027200090323       prmRpyOpCode = 'DONE';
027300090323       CLEAR prmRpyIdMsg;
027400090323
027500090324       IF prmRqsOpCode = 'PRTCONTRAT' AND initDone;
027600090324
027700090324         // Controllo parametri ricevuti.
027800090324
027900111128         IF prmRqsFormato = 'TRMZ0101I';
028000111128           trmz0101i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
028100090324         ELSE;
028200090324           prmRpyOpCode = 'ERRORE';
028300090324           prmRpyIdMsg = ESITO_ERROR;
028400090324           RETURN;
028500090324         ENDIF;
028600090324
028700090324         // Parametri obbligatori.
028800090324
028900111128         IF trmz0101i.contratDat = *LOVAL OR trmz0101i.contratNum < 1
029000111128         OR trmz0101i.societa = *BLANK OR trmz0101i.filiale < 1
029100111128         OR %LEN(trmz0101i.vetRagSoc) = *ZERO OR trmz0101i.vetRagSoc = *BLANK
029200111128         OR %LEN(trmz0101i.vetSlCom) = *ZERO OR trmz0101i.vetSlCom = *BLANK
029300111128         OR %LEN(trmz0101i.vetSlInd) = *ZERO OR trmz0101i.vetSlInd = *BLANK
029400111128         OR %LEN(trmz0101i.vetCdFisca) = *ZERO OR trmz0101i.vetCdFisca = *BLANK
029500111128         OR %LEN(trmz0101i.vetCdIva) = *ZERO OR trmz0101i.vetCdIva = *BLANK
029600111128         OR %LEN(trmz0101i.vetRimLuo) = *ZERO OR trmz0101i.vetRimLuo = *BLANK
029700111128         OR %LEN(trmz0101i.vetRimNum) = *ZERO OR trmz0101i.vetRimNum = *BLANK
029800090422         ;
029900090422           prmRpyOpCode = 'ERRORE';
030000090324           prmRpyIdMsg = ESITO_ERROR;
030100090324           RETURN;
030200090324         ENDIF;
030300090324
030400090324         // Reperisco l'anagrafica della società mittente.
030500090324
030600111128         esito = Societa_NewSocieta( trmz0101i.societa
030700111128                                   : trmz0101i.contratDat
030800090429                                   );
030900090429
031000090429         IF esito = SOCIETA_INFO_SOCIETA_INESISTENTE
031100090429         OR esito < ESITO_OK;
031200090429           prmRpyOpCode = 'ERRORE';
031300090324           prmRpyIdMsg = ESITO_ERROR;
031400090324           RETURN;
031500090324         ENDIF;
031600090324
031700090324         tibsSocO0 = Societa_GetAnagrafica( 'TIBSSOCO0' );
031800090324
031900090324         // Imposto variabili fisse.
032000090324
032100111128         IF trmz0101i.dataStampa > *LOVAL;
032200111128           prt_dataStampa = trmz0101i.dataStampa;
032300090422         ELSE;
032400090422           prt_dataStampa = %DATE();
032500090422         ENDIF;
032600090422
032700090423         prt_luogoData = %TRIMR(tibsSocO0.sedLegLoc) + ', '
032800111130                       + %CHAR(trmz0101i.dataDecApp : *EUR);
032900090324
033000090324         p = ((%SIZE(prt_firmaSoc)
033100090324           - %LEN(%TRIMR(tibsSocO0.ragSociale))) / 2) + 1;
033200090617         CLEAR prt_firmaSoc;
033300090617         %SUBST( prt_firmaSoc : p ) = tibsSocO0.ragSociale;
033400090324
033500111128         prt_vetRagSoc = trmz0101i.vetRagSoc;
033600111128         prt_vetSlInd = trmz0101i.vetSlInd;
033700090324
033800111128         prt_vetSlCom = trmz0101i.vetSlCap
033900111128                      + ' ' + trmz0101i.vetSlCom
034000111128                      + ' ' + trmz0101i.vetSlPrv
034100090324                      ;
034200090324
034300111128         prt_contratNum = %EDITC(trmz0101i.contratNum : 'X')
034400090324                        + '/'
034500111128                        + %EDITC(trmz0101i.filiale : 'X')
034600090324                        ;
034700090323
034800090324         // Stampo la copia con la firma.
034900090324
035000090324         copiaPerVettore = *ON;
035100090324
035200090323         IF PrtContratto() < ESITO_OK;
035300111128           FEOD trmz0101p;
035400090323           prmRpyOpCode = 'ERRORE';
035500090323           prmRpyIdMsg = ESITO_ERROR;
035600090323           RETURN;
035700090323         ENDIF;
035800090324
035900090324         // Stampo la copia senza la firma.
036000090324
036100090324         copiaPerVettore = *OFF;
036200090324
036300090324         IF PrtContratto() < ESITO_OK;
036400111128           FEOD trmz0101p;
036500090324           prmRpyOpCode = 'ERRORE';
036600090324           prmRpyIdMsg = ESITO_ERROR;
036700090324           RETURN;
036800090324         ENDIF;
036900090323
037000090324       ELSEIF prmRqsOpCode = 'INIT' AND NOT initDone;
037100090324
037200090324         IF Init() < ESITO_OK;
037300090324           prmRpyOpCode = 'ERRORE';
037400090324           prmRpyIdMsg = ESITO_ERROR;
037500090324           RETURN;
037600090324         ENDIF;
037700090323
037800090324       ELSEIF prmRqsOpCode = 'FINALIZE' AND initDone;
037900090323
038000111128         CLOSE trmz0101p;
038100090323         ISST_finalize();
038200090323         Societa_finalize();
038300090323         CodeConversionDeallocation ( iconv_t );
038400090324         RESET initDone;
038500090323         *INLR = *ON;
038600090323
038700090323       ELSE;
038800090323
038900090323         prmRpyOpCode = 'ERRORE';
039000090323         prmRpyIdMsg = ESITO_ERROR;
039100090323
039200090323       ENDIF;
039300090318
039400090323       RETURN;
039500090323
039600090318      /END-FREE
039700090323
039800090323     P*--------------------------------------------------
039900090323     P* Procedure name: PrtContratto
040000090323     P* Purpose:        Stampa un contratto.
040100090323     P* Returns:        Esito.
040200090323     P*--------------------------------------------------
040300090323     P PrtContratto    B
040400090323     D PrtContratto    PI            10I 0
040500090323
040600090323      /FREE
040700090323
040800111125       // Recupero la cartella IFS dei testi in base alla versione del contratto.
040900111125
041000111128       cartellaTesti = GetCartellaTestiByVersioneContratto('A');
041100111125
041200111125       IF cartellaTesti = *BLANK;
041300111125         RETURN ESITO_ERROR;
041400111125       ENDIF;
041500111128       cartellaTesti += SUBDIR_APPENDICE;
041600111125       // Carico le sezioni di testo.
041700111125
041800111128       testoPartiMittente = GetTestoSezione('app-a-mittente.txt');
041900111128       testoPartiVettore = GetTestoSezione('app-a-vettore.txt');
042000111125
042100111125       FOR a = 1 TO %ELEM(titoloArticolo);
042200111128         titoloArticolo(a) = GetTestoSezione('app-a-articolo-'
042300111128                           + %EDITC(a : 'X') + '-titolo.txt');
042400111128         testoArticolo(a)  = GetTestoSezione('app-a-articolo-'
042500111125                           + %EDITC(a : 'X') + '.txt');
042600111125       ENDFOR;
042700111125
042800111125       // C'è stato un errore nel caricamento dei testi del contratto.
042900111125
043000111125       IF prmRpyIdMsg < ESITO_OK;
043100111125         RETURN ESITO_ERROR;
043200111125       ENDIF;
043300111125
043400090323       //***********************************************************************
043500090323       //
043600090323       // Stampo l'introduzione.
043700090323       //
043800090323       //***********************************************************************
043900090323
044000090422       RESET overFlow;
044100090323
044200090323       WRITE pagina;
044300090422       WRITE dtCertaBox;
044400090422       indPrtf.stampareVettore = *ON;
044500090422       WRITE dtCertaTxt;
044600090422       indPrtf.stampareVettore = *OFF;
044700090323       WRITE inizioDoc;
044800090323
044900090323       //***********************************************************************
045000090323       //
045100111130       // Stampo la sezione "scrittura privata integrativa............ ...".
045200090323       //
045300090323       //***********************************************************************
045400090323
045500090323       RunOverFlow();
045600090323
045700090323       CLEAR prt_testo;
045800111130       %SUBST(prt_testo : 25) = 'SCRITTURA PRIVATA INTEGRATIVA AL CONTRATTO +
045900111130       DI TRASPORTO DEL ' +  %CHAR(trmz0101i.ContratDat : *EUR)
046000111130                            + ' NR. '
046100111130                          +  %EDITC(trmz0101i.contratNum : 'X')
046200111130                            + '/'+ %EDITC(trmz0101i.filiale : 'X');
046300090323       indPrtf.grassetto = *ON;
046400090323       WRITE riga;
046500090323       RESET indPrtf.grassetto;
046600090323
046700090323       RunOverFlow();
046800090323
046900090323       CLEAR prt_testo;
047000090323       %SUBST(prt_testo : 59) = 'Tra le parti:';
047100090323       WRITE riga;
047200090323
047300090324       IF RunRigaTesto(testoPartiMittente) < ESITO_OK;
047400090323         RETURN ESITO_ERROR;
047500090323       ENDIF;
047600090323
047700090323       RunOverFlow();
047800090323
047900090323       CLEAR prt_testo;
048000090323       %SUBST(prt_testo : 65) = 'e';
048100090323       WRITE riga;
048200090323
048300090324       IF RunRigaTesto(testoPartiVettore) < ESITO_OK;
048400090323         RETURN ESITO_ERROR;
048500090323       ENDIF;
048600090323
048700090323       //***********************************************************************
048800090323       //
048900090323       // Stampo gli articoli del contratto.
049000090323       //
049100090323       //***********************************************************************
049200090323
049300090323       RunOverFlow();
049400090323       CLEAR prt_testo;
049500090323       %SUBST(prt_testo : 54) = 'SI CONVIENE QUANTO SEGUE';
049600090323       WRITE riga;
049700111125
049800111125       FOR a = 1 TO %ELEM(titoloArticolo);
049900111125
050000111125         indPrtf.grassetto = *ON;
050100111125
050200111125         IF RunRigaTesto(titoloArticolo(a)) < ESITO_OK;
050300111125           RETURN ESITO_ERROR;
050400111125         ENDIF;
050500111125
050600111125         RESET indPrtf.grassetto;
050700111125
050800111125         IF RunRigaTesto(testoArticolo(a)) < ESITO_OK;
050900111125           RETURN ESITO_ERROR;
051000111125         ENDIF;
051100111125
051200111125       ENDFOR;
051300111125
051400090323       //***********************************************************************
051500090323       //
051600090324       // Stampo chiusura contratto e spazi per le firma.
051700090323       //
051800090323       //***********************************************************************
051900111128
052000111130       //  RESET trul13ds;
052100111130       //  trul13ds.I13FIL = trmz0101i.filiale;
052200111130       //  trul13ds.I13DIN = %DEC(trmz0101i.contratDat);
052300111130       //  trul13ds.I13GIO = 1;
052400111130       //  Trul13r(trul13ds);
052500111128
052600111130       //  IF trul13ds.O13ERR = *BLANK;
052700111130       //  dataAccettazione = %DATE(trul13ds.O13DFI : *ISO);
052800111130       // ELSE;
052900111130         dataAccettazione = trmz0101i.datafirmav ;
053000111130       // ENDIF;
053100111128
053200111128       RunOverFlow();
053300111128       CLEAR prt_testo;
053400111128       WRITE riga;
053500090324
053600090324       RunOverFlow();
053700090323       prt_testo = '(La mittente) ' + tibsSocO0.ragSociale;
053800090323       WRITE riga;
053900111128
054000111128       RunOverFlow();
054100111128       CLEAR prt_testo;
054200111128       WRITE riga;
054300111128
054400111128       RunOverFlow();
054500111128       //prt_testo = %TRIMR(tibsSocO0.sedLegLoc) + ', '
054600111130       //          + %CHAR(trmz0101i.contratdat : *EUR);
054700111128       prt_testo = trmz0101i.vetSlCom + ', '
054800111128                 + %CHAR(dataAccettazione : *EUR);
054900111128       WRITE riga;
055000090323
055100111027       RunOverFlow();
055200111128       prt_testo = '(Per accettazione)(Il vettore) ' +
055300090324                   '_________________________________________________________'
055400090324                 ;
055500090324       WRITE riga;
055600090324
055700090324       IF copiaPerVettore;
055800090420         WRITE firmaTxt;
055900090324         WRITE firmaImg;
056000111201          else ;
056100111201         prt_bcd128 = trmz0101i.codbar12;
056200111201         WRITE barcode ;
056300090324       ENDIF;
056400090324
056500111206       // Stampo 2a pagina vuota.
056600111206
056700111206       CLEAR prt_testo;
056800111206       DOU infPrtf.currentPage > 1;
056900111206         WRITE riga;
057000111206         IF infPrtf.currentPage > 1;
057100111206           prt_testo = 'Pagina lasciata intenzionalmente vuota.' ;
057200111206           WRITE riga;
057300111206           LEAVE;
057400111206         ENDIF;
057500111206       ENDDO;
057600111206
057700111128       FEOD trmz0101p;
057800090323
057900090323       RETURN ESITO_OK;
058000090323
058100090323      /END-FREE
058200090323     P PrtContratto    E
058300090323
058400090318
058500090318     P*--------------------------------------------------
058600090319     P* Procedure name: RunRigaTesto
058700090319     P* Purpose:        Prepara e scrive una riga di testo.
058800090318     P* Returns:        Esito.
058900090318     P*--------------------------------------------------
059000090319     P RunRigaTesto    B
059100090319     D RunRigaTesto    PI            10I 0
059200090324     D  piTestoSezione...
059300090324     D                            32740A   VARYING
059400090324     D                                     VALUE
059500090318
059600090318     D posizioneProgressiva...
059700090318     D                 S             10I 0
059800090318
059900090318      /FREE
060000090318
060100090318       // Ricerca e sostituzione delle variabili.
060200090318
060300090320       outBufVar = %TRIMR(outBuf);
060400090320
060500090318       EXEC SQL
060600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
060700090324                                      , '&MITTENTE_RAGIONE_SOCIALE'
060800090324                                      , :tibsSocO0.ragSociale
060900090324                                      )
061000090323       ;
061100090318
061200090318       EXEC SQL
061300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
061400090324                                      , '&MITTENTE_COMUNE'
061500090324                                      , :tibsSocO0.sedLegLoc
061600090324                                      )
061700090324       ;
061800090318
061900090318       EXEC SQL
062000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
062100090324                                      , '&MITTENTE_INDIRIZZO'
062200090324                                      , :tibsSocO0.sedLegInd
062300090324                                      )
062400090324       ;
062500090324
062600090324       EXEC SQL
062700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
062800090324                                      , '&MITTENTE_REGISTRO_IMPRESE_LUOGO'
062900090324                                      , :tibsSocO0.rimLuogo
063000090324                                      )
063100090324       ;
063200090324
063300090324       EXEC SQL
063400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
063500090324                                      , '&MITTENTE_REGISTRO_IMPRESE_NUMERO'
063600090324                                      , :tibsSocO0.codFiscale
063700090324                                      )
063800090324       ;
063900090324
064000090324       EXEC SQL
064100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
064200090324                                      , '&MITTENTE_REA_LUOGO'
064300090324                                      , :tibsSocO0.reaLuogo
064400090324                                      )
064500090324       ;
064600090324
064700090324       EXEC SQL
064800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
064900090324                                      , '&MITTENTE_REA_NUMERO'
065000090324                                      , :tibsSocO0.reaNumero
065100090324                                      )
065200090324       ;
065300090324
065400090324       EXEC SQL
065500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
065600090324                                      , '&MITTENTE_CODICE_FISCALE'
065700090324                                      , :tibsSocO0.codFiscale
065800090324                                      )
065900090324       ;
066000090324
066100090324       EXEC SQL
066200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
066300090324                                      , '&MITTENTE_PARTITA_IVA'
066400090324                                      , :tibsSocO0.partitaIva
066500090324                                      )
066600090324       ;
066700090319
066800090324       IF tibsSocO0.socioUnico = *ON;
066900090324         EXEC SQL
067000090324           SET :piTestoSezione = REPLACE( :piTestoSezione
067100090324                                        , '&MITTENTE_SOCIO_UNICO'
067200090424                                        , 'a socio unico'
067300090324                                        )
067400090324         ;
067500090324       ELSE;
067600090324         EXEC SQL
067700090324           SET :piTestoSezione = REPLACE( :piTestoSezione
067800090324                                        , '&MITTENTE_SOCIO_UNICO'
067900090324                                        , ''
068000090324                                        )
068100090324         ;
068200090324       ENDIF;
068300090324
068400090324       EXEC SQL
068500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
068600090324                                      , '&VETTORE_RAGIONE_SOCIALE'
068700111128                                      , :trmz0101i.vetRagSoc
068800090324                                      )
068900090324       ;
069000090324
069100090324       EXEC SQL
069200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
069300090324                                      , '&VETTORE_COMUNE'
069400111128                                      , :trmz0101i.vetSlCom
069500090324                                      )
069600090324       ;
069700090324
069800090324       EXEC SQL
069900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
070000090324                                      , '&VETTORE_INDIRIZZO'
070100111128                                      , :trmz0101i.vetSlInd
070200090324                                      )
070300090324       ;
070400090324
070500090324       EXEC SQL
070600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
070700090324                                      , '&VETTORE_REGISTRO_IMPRESE_LUOGO'
070800111128                                      , :trmz0101i.vetRimLuo
070900090324                                      )
071000090324       ;
071100090324
071200090324       EXEC SQL
071300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
071400090324                                      , '&VETTORE_REGISTRO_IMPRESE_NUMERO'
071500111128                                      , :trmz0101i.vetRimNum
071600090324                                      )
071700090324       ;
071800090324
071900090324       EXEC SQL
072000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
072100090324                                      , '&VETTORE_PARTITA_IVA'
072200111128                                      , :trmz0101i.vetCdIva
072300090324                                      )
072400090324       ;
072500090324
072600090324       EXEC SQL
072700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
072800090324                                      , '&VETTORE_REA_LUOGO'
072900111128                                      , :trmz0101i.vetReaLuo
073000090324                                      )
073100090324       ;
073200090324
073300090324       EXEC SQL
073400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
073500090324                                      , '&VETTORE_REA_NUMERO'
073600111128                                      , :trmz0101i.vetReaNum
073700090324                                      )
073800090324       ;
073900090324
074000090324       EXEC SQL
074100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
074200090324                                      , '&VETTORE_LEGALE_RAPPRESENTANTE'
074300111128                                      , :trmz0101i.vetLegRap
074400090324                                      )
074500090324       ;
074600090324
074700090324       EXEC SQL
074800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
074900090324                                      , '&VETTORE_ALBO_PROVINCIA'
075000111128                                      , :trmz0101i.vetAnaPrv
075100090324                                      )
075200090324       ;
075300090324
075400090324       EXEC SQL
075500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
075600090324                                      , '&VETTORE_ALBO_NUMERO'
075700111128                                      , :trmz0101i.vetAnaNum
075800090324                                      )
075900090324       ;
076000090318
076100090318       // Sottostringo.
076200090320
076300090324       outBuf = piTestoSezione;
076400090324       outBytesLeft = %LEN(piTestoSezione);
076500090318
076600090318       IF ISST_newStringa( outBuf
076700090318                         : outBytesLeft
076800090318                         ) < ESITO_OK;
076900090318         DUMP(A);
077000090318         RETURN ESITO_ERROR;
077100090318       ENDIF;
077200090318
077300090318       // Stampo le sottostrighe.
077400090318
077500090318       DOU posizioneProgressiva >= outBytesLeft;
077600090318         prt_testo = ISST_getSottoStringa( %SIZE(prt_testo)
077700090318                                         : posizioneProgressiva
077800090318                                         );
077900090319         RunOverFlow();
078000111129         IF posizioneProgressiva < outBytesLeft;
078100090319         txtInput = %TRIMR(prt_testo);
078200090319         txtInputLen = %SIZE(prt_testo);
078300090324         Giustifica( txtInput : txtInputLen : txtGiust : txtResto );
078400090319         prt_testo = txtGiust;
078500111129         ENDIF;
078600090319         WRITE riga;
078700090318       ENDDO;
078800090318
078900090318       RETURN ESITO_OK;
079000090318
079100090318      /END-FREE
079200090319     P RunRigaTesto    E
079300090318
079400090319
079500090319     P*--------------------------------------------------
079600090319     P* Procedure name: RunOverFlow
079700090319     P* Purpose:        Gestisce l'overflow.
079800090319     P* Returns:        Esito.
079900090319     P*--------------------------------------------------
080000090319     P RunOverFlow     B
080100090319     D RunOverFlow     PI            10I 0
080200090319
080300090319      /FREE
080400090319
080500090319       IF NOT overFlow;
080600090319         RETURN ESITO_OK;
080700090319       ENDIF;
080800090324
080900090324       IF copiaPerVettore;
081000090324         WRITE firmaTxt;
081100090324         WRITE firmaImg;
081200111201          else ;
081300111201         prt_bcd128 = trmz0101i.codbar12;
081400111201         WRITE barcode ;
081500090324       ENDIF;
081600090324
081700090319       WRITE pagina;
081800090319       RESET overFlow;
081900090319
082000090422       IF infPrtf.currentPage = 3;
082100090422         WRITE dtCertaBox;
082200090422         WRITE dtCertaTxt;
082300090422       ENDIF;
082400090422
082500090319       RETURN ESITO_OK;
082600090319
082700090319      /END-FREE
082800090319     P RunOverFlow     E
082900090319
083000090324
083100090324     P*--------------------------------------------------
083200090324     P* Procedure name: Init
083300090324     P* Purpose:        Inizializza il modulo.
083400090324     P* Returns:        Esito.
083500090324     P*--------------------------------------------------
083600090324     P Init            B
083700090324     D Init            PI            10I 0
083800090324
083900090324      /FREE
084000090324
084100090324       // Inizializzo una conversione dati da ASCII a EBCDIC.
084200090324
084300111125       fromCode.QTQCCSID = CCSID_MSWIN_LATIN1;
084400090326       toCode.QTQCCSID = CCSID_ITALIAN;
084500090324       fromCode.QTQERVED02 = *ALLX'00';
084600090324       toCode.QTQERVED02 = *ALLX'00';
084700090324
084800090324       iconv_t = CodeConversionAllocation( %ADDR(toCode)
084900090324                                         : %ADDR(fromCode)
085000090324                                         );
085100090324
085200090324       // Inizializzo il modulo sottostringa intelligente.
085300090324
085400090324       IF ISST_init() < ESITO_OK;
085500090324         prmRpyOpCode = 'ERRORE';
085600090324         prmRpyIdMsg = ESITO_ERROR;
085700090324         RETURN ESITO_ERROR;
085800090324       ENDIF;
085900090324
086000090324       // Inizializzo il modulo anagrafico società.
086100090324
086200090324       IF Societa_Init() < ESITO_OK;
086300090324         prmRpyOpCode = 'ERRORE';
086400090324         prmRpyIdMsg = ESITO_ERROR;
086500090324         RETURN ESITO_ERROR;
086600090324       ENDIF;
086700090325
086800090324       // Posso finalmente aprire il printer file.
086900090324
087000111128       OPEN trmz0101p;
087100090324
087200090324       // Inizializzazione del programma completata.
087300090324
087400090324       initDone = *ON;
087500090324
087600090324       RETURN ESITO_OK;
087700090324
087800090324      /END-FREE
087900090324     P Init            E
088000090324
088100090324
088200090324     P*--------------------------------------------------
088300090324     P* Procedure name: GetTestoSezione
088400090324     P* Purpose:        Restituisce il testo di una sezione.
088500090324     P* Returns:        Testo della sezione.
088600090324     P* Parameter:      piNomeFile => Nome file che contiene il testo della...
088700090324     P*                           sezione.
088800090324     P*--------------------------------------------------
088900090324     PGetTestoSezione  B
089000090324     DGetTestoSezione  PI         32767A   VARYING
089100090324     D  piNomeFile                  255A   VARYING
089200090324     D                                     VALUE
089300090324
089400090324      /FREE
089500090324
089600111125       fd = IFS_OpenFile( cartellaTesti + '/' + piNomeFile
089700090324                        : O_RDONLY);
089800090324
089900090324       IF fd < 0;
090000090324         errnoPtr = getErrno();
090100090326         prmRpyIdMsg = errno * -1;
090200090326         prmRpyOpCode = 'ERRORE';
090300090324         DUMP(A);
090400090324         RETURN *BLANK;
090500090324       ENDIF;
090600090324
090700090324       RESET inBuf;
090800090324       RESET inBufPtr;
090900090324
091000090324       IFS_ReadFromDescriptor( fd : inBufPtr : %SIZE(inBuf) );
091100090324       IFS_CloseFile( fd );
091200090324
091300090324       // Conversione ASCII -> EBCDIC.
091400090324
091500090324       inBytesLeft = %LEN(%TRIMR(inBuf));
091600090324
091700090324       IF inBytesLeft = *ZERO;
091800090324         RETURN *BLANK;
091900090324       ENDIF;
092000090324
092100090324       outBytesLeft = inBytesLeft;
092200090324       CLEAR outBuf;
092300090324       RESET outBufPtr;
092400090324
092500090324       IF CodeConversion( iconv_t
092600090324                        : inBufPtr
092700090324                        : inBytesLeft
092800090324                        : outBufPtr
092900090324                        : outBytesLeft
093000090324                        ) < 0;
093100090324         errnoPtr = getErrno();
093200090326         prmRpyIdMsg = errno * -1;
093300090326         prmRpyOpCode = 'ERRORE';
093400090324         DUMP(A);
093500090324         RETURN *BLANK;
093600090324       ENDIF;
093700090324
093800090324       RETURN %TRIMR(outBuf);
093900090324
094000090324      /END-FREE
094100090324     PGetTestoSezione  E
094200090324
094300111124
094400111124     P*--------------------------------------------------
094500111124     P* Procedure name: GetVersioneContrattoByData
094600111124     P* Purpose:        Restituisce la versione del contratto in base alla ...
094700111124     P*                          data.
094800111124     P* Returns:        Versione.
094900111124     P* Parameter:      priData => Data per calcolo versione.
095000111124     P*--------------------------------------------------
095100111124     P GetVersioneContrattoByData...
095200111124     P                 B
095300111124     D GetVersioneContrattoByData...
095400111124     D                 PI             1A
095500111124     D  priData                        D   CONST
095600111124
095700111124     D retField        S              1A   STATIC
095800111124
095900111124      /FREE
096000111124
096100111124       CLEAR retField;
096200111124
096300111124       EXEC SQL
096400111124         SELECT LEFT(tbeUni, 1)
096500111124           INTO :retField
096600111124           FROM tntbe00f
096700111124           WHERE tbeCod = '007'
096800111124             AND tbeKe1 = '*VERSIONE'
096900111124             AND tbeKe2 <= :priData
097000111124             AND tbeLin = ''
097100111124             AND tbeSif = ''
097200111124             AND tbeAtb = ''
097300111124           ORDER BY tbeCod, tbeKe1, tbeKe2 DESC
097400111124           FETCH FIRST 1 ROW ONLY
097500111124       ;
097600111124
097700111124       SELECT;
097800111124         WHEN sqlCode < *ZERO;
097900111124           DUMP(A);
098000111128         WHEN sqlCode = 100;
098100111124           DUMP(A);
098200111124       ENDSL;
098300111124
098400111124       RETURN retField;
098500111124
098600111124      /END-FREE
098700111124     P GetVersioneContrattoByData...
098800111124     P                 E
098900111124
099000111125
099100111125     P*--------------------------------------------------
099200111125     P* Procedure name: GetCartellaTestiByVersioneContratto
099300111125     P* Purpose:        Restituisce la cartella dei testi della versione di...
099400111125     P*                           contratto.
099500111125     P* Returns:        Cartella testi.
099600111125     P* Parameter:      priVersioneContratto => Versione contratto
099700111125     P*--------------------------------------------------
099800111125     P GetCartellaTestiByVersioneContratto...
099900111125     P                 B
100000111125     D GetCartellaTestiByVersioneContratto...
100100111125     D                 PI           255A   VARYING
100200111125     D  priVersioneContratto...
100300111125     D                                1A   CONST
100400111125
100500111125     D retField        S            255A   VARYING STATIC
100600111125
100700111125      /FREE
100800111125
100900111125       CLEAR retField;
101000111125
101100111125       EXEC SQL
101200111125         SELECT RTRIM(tbeUni)
101300111125           INTO :retField
101400111125           FROM tntbe00f
101500111125           WHERE tbeCod = '007'
101600111125             AND tbeKe1 = '*IFSDIRTXT'
101700111125             AND tbeKe2 = :priVersioneContratto
101800111125             AND tbeLin = ''
101900111125             AND tbeSif = ''
102000111125             AND tbeAtb = ''
102100111125       ;
102200111125
102300111125       SELECT;
102400111125         WHEN sqlCode < *ZERO;
102500111125           DUMP(A);
102600111125         WHEN sqlCode = 100;
102700111125           DUMP(A);
102800111125       ENDSL;
102900111125
103000111125       RETURN retField;
103100111125
103200111125      /END-FREE
103300111125     P GetCartellaTestiByVersioneContratto...
103400111125     P                 E
103500111125
