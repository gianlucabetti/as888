000100090407      /DEFINE DFTACTGRP_NO
000200120326
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
010200160317     D trtcm1ds      e ds                  inz
010300160318     D trul28dsx     e ds                  inz
010400120314      *
010500090318
010600090318      //************************************************************************
010700090318      //
010800090318      // Definizione variabili.
010900090318      //
011000090318      //************************************************************************
011100090319     D/COPY GAITRASRC/SRCPROTOPI,ICONV
011200090318     D/COPY GAITRASRC/SRCPROTOPI,ERRNO
011300111125     D a...
011400111125     D                 S              2S 0
011401161207     D nrart...
011402161207     D                 S              2S 0
011403161207     D numeroart...
011404161207     D                 S              2
011500111125     D cartellaTesti...
011600111125     D                 S            255A   VARYING
011700111128     D dataAccettazione...
011800111128     D                 S               D
011900090318     D esito...
012000090318     D                 S             10I 0
012100090318     D fd...
012200090318     D                 S             10I 0                                      File descriptor
012300090318     D inBuf...
012400090318     D                 S          32766
012500090318     D inBytesLeft...
012600090318     D                 S             10I 0
012700090318     D inBufPtr...
012800090318     D                 S               *   INZ(%ADDR(inBuf))
012900090324     D initDone...
013000090324     D                 S               N
013100090318     D outBuf...
013200090318     D                 S                   LIKE(inBuf)
013203161207     D datapdfc...
013204161207     D                 S             10
013205161207     D versionec...
013206161207     D                 S                   LIKE(versioneContratto)
013300090318     D outBytesLeft...
013400090318     D                 S             10I 0
013500090318     D outBufPtr...
013600090318     D                 S               *   INZ(%ADDR(outBuf))
013700090320     D outBufVar...
013800090320     D                 S          32740A   VARYING
013900090318     D p...
014000090318     D                 S              5U 0
014100090324     D copiaPerVettore...
014200090324     D                 S               N
014300090324     D testoIntroduzione...
014400090324     D                 S          32767A   VARYING
014500090324     D testoPartiMittente...
014600090324     D                 S          32767A   VARYING
014700090324     D testoPartiVettore...
014800090324     D                 S          32767A   VARYING
014900090324     D testoPremesso1...
015000090324     D                 S          32767A   VARYING
015100090324     D testoPremesso1a...
015200090324     D                 S          32767A   VARYING
015300090324     D testoPremesso1b...
015400090324     D                 S          32767A   VARYING
015500090324     D testoPremesso2...
015600090324     D                 S          32767A   VARYING
015700090324     D testoPremesso3...
015800090324     D                 S          32767A   VARYING
015900090324     D testoPremesso4...
016000090324     D                 S          32767A   VARYING
016100111125     D testoArticolo...
016200161207     D                 S          32767A   VARYING DIM(99)
016300111125     D titoloArticolo...
016400161207     D                 S            255A   VARYING DIM(99)
016500090324     D testoDichiarazioneVisione...
016600090324     D                 S          32767A   VARYING
016700111207     D testoFirmaVettore...
016800111207     D                 S            130A   VARYING
016900090319     D txtInput...
017000090319     D                 S            200A   VARYING
017100090319     D txtInputLen...
017200090319     D                 S              5I 0
017300090319     D txtGiust...
017400090319     D                 S            200A   VARYING
017500090319     D txtResto...
017600090319     D                 S            200A   VARYING
017700111125     D versioneContratto...
017800111125     D                 S              1A
017900130205     D Ovrprtfversione...
018000130205     D                 S            256A
018100160502     D exOvrprtfvers   s            256a
018200130205     d comman          s            256
018300130205     d lenght          s             15  5 inz(256)
018400160317
018500160317     d DATACONTR       s              8  0
018600160318     d cbarre28        s             28    inz
018700090318
018800090323     ***************************************************************************
018900090323     **
019000090323     ** Dichiarazione parametri programma.
019100090323     **
019200090323     ***************************************************************************
019300090323     D trmz0100r...
019400090323     D                 PI
019500090323     D  prmRqsOpCode...
019600090323     D                               10A   CONST
019700090323     D  prmRpyOpCode...
019800090323     D                               10A
019900090323     D  prmRpyIdMsg...
020000090323     D                               10I 0
020100090323     D  prmRqsFormato...
020200090323     D                               10A   CONST
020300090323     D                                     OPTIONS(*NOPASS)
020400090323     D  prmRqsData...
020500090324     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
020600090323     D  prmRqsDataSize...
020700090323     D                               10I 0 CONST
020800090323     D                                     OPTIONS(*NOPASS)
020900090324     D* prmRpyFormato...
021000090324     D*                              10A   CONST
021100090324     D*                                    OPTIONS(*NOPASS)
021200090324     D* prmRpyData...
021300090324     D*                           32767A   OPTIONS(*VARSIZE:*NOPASS)
021400090324     D* prmRpyDataSize...
021500090324     D*                              10I 0 CONST
021600090324     D*                                    OPTIONS(*NOPASS)
021700090323
021800090323     D*--------------------------------------------------
021900090323     D* Procedure name: PrtContratto
022000090323     D* Purpose:        Stampa un contratto.
022100090323     D* Returns:        Esito.
022200090323     D*--------------------------------------------------
022300090323     D PrtContratto    PR            10I 0
022400090323
022500090318     D*--------------------------------------------------
022600090319     D* Procedure name: RunRigaTesto
022700090319     D* Purpose:        Prepara e scrive una riga di testo.
022800090318     D* Returns:        Esito.
022900090318     D*--------------------------------------------------
023000090319     D RunRigaTesto    PR            10I 0
023100090324     D  piTestoSezione...
023200090324     D                            32740A   VARYING
023300090324     D                                     VALUE
023400090319
023500090319     D*--------------------------------------------------
023600090319     D* Procedure name: RunOverFlow
023700090319     D* Purpose:        Gestisce l'overflow.
023800090319     D* Returns:        Esito.
023900090319     D*--------------------------------------------------
024000090319     D RunOverFlow     PR            10I 0
024100090324
024200090324     D*--------------------------------------------------
024300090324     D* Procedure name: Init
024400090324     D* Purpose:        Inizializza il modulo.
024500090324     D* Returns:        Esito.
024600090324     D*--------------------------------------------------
024700090324     D Init            PR            10I 0
024800090324
024900090324     D*--------------------------------------------------
025000090324     D* Procedure name: GetTestoSezione
025100090324     D* Purpose:        Restituisce il testo di una sezione.
025200090324     D* Returns:        Testo della sezione.
025300090324     D* Parameter:      piNomeFile => Nome file che contiene il testo della...
025400090324     D*                           sezione.
025500090324     D*--------------------------------------------------
025600090324     DGetTestoSezione  PR         32767A   VARYING
025700090324     D  piNomeFile                  255A   VARYING
025800090324     D                                     VALUE
025900111124
026000111124     D*--------------------------------------------------
026100111124     D* Procedure name: GetVersioneContrattoByData
026200111124     D* Purpose:        Restituisce la versione del contratto in base alla ...
026300111124     D*                          data.
026400111124     D* Returns:        Versione.
026500111124     D* Parameter:      priData => Data per calcolo versione.
026600111124     D*--------------------------------------------------
026700111124     D GetVersioneContrattoByData...
026800111124     D                 PR             1A
026900111124     D  priData                        D   CONST
027000111125
027100111125     D*--------------------------------------------------
027200111125     D* Procedure name: GetCartellaTestiByVersioneContratto
027300111125     D* Purpose:        Restituisce la cartella dei testi della versione di...
027400111125     D*                           contratto.
027500111125     D* Returns:        Cartella testi.
027600111125     D* Parameter:      priVersioneContratto => Versione contratto
027700111125     D*--------------------------------------------------
027800111125     D GetCartellaTestiByVersioneContratto...
027900111125     D                 PR           255A   VARYING
028000111125     D  priVersioneContratto...
028100111125     D                                1A   CONST
028200130205     D*--------------------------------------------------
028300130205     D* Procedure name: GetCartellaTestiByVersioneContratto
028400130205     D* Purpose:        Restituisce la cartella dei testi della versione di...
028500130205     D*                           contratto.
028600130205     D* Returns:        Cartella testi.
028700130205     D* Parameter:      priVersioneContratto => Versione contratto
028800130205     D*--------------------------------------------------
028900130205     D GetOvrPrtfByVersioneContratto...
029000130205     D                 PR           256A
029100130205     D  priVersioneContratto...
029200130205     D                                1A   CONST
029300111125
029400111125
029500111125
029600090319      /FREE
029700090318
029800090323       // Inizializzo i parametri di output.
029900090323
030000090323       prmRpyOpCode = 'DONE';
030100090323       CLEAR prmRpyIdMsg;
030200090323
030300090324       IF prmRqsOpCode = 'PRTCONTRAT' AND initDone;
030400090324
030500090324         // Controllo parametri ricevuti.
030600090324
030700090324         IF prmRqsFormato = 'TRMZ0100I';
030800090324           trmz0100i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
030900111130           IF prmRqsDataSize = 505; // Vecchia versione del parametro.
031000111130             trmz0100i.dataCertaP = trmz0100i.contratDat;
031100160317             RESET trtcm1ds;
031200111130             RESET trul13ds;
031300111130             trul13ds.i13fil = trmz0100i.filiale;
031400111130             trul13ds.i13din = %DEC(trmz0100i.contratDat);
031500111130             trul13ds.i13gio = 1;
031600111130             Trul13r(trul13ds);
031700111130             IF trul13ds.o13err = *BLANK;
031800111130               trmz0100i.dataFirmaV = %DATE(trul13ds.o13dfi : *ISO);
031900111130             ELSE;
032000111130               trmz0100i.dataFirmaV = trmz0100i.contratDat + %DAYS(1);
032100111130             ENDIF;
032200111130           ENDIF;
032300111130         ELSE;
032400090324           prmRpyOpCode = 'ERRORE';
032500090324           prmRpyIdMsg = ESITO_ERROR;
032600090324           RETURN;
032700090324         ENDIF;
032800090324
032900090324         // Parametri obbligatori.
033000090324
033100160317            DATACONTR = %DEC(trmz0100i.contratDat) ;
033200090324         IF trmz0100i.contratDat = *LOVAL OR trmz0100i.contratNum < 1
033300111130         OR trmz0100i.dataStampa = *LOVAL OR trmz0100i.dataCertaP = *LOVAL
033400111130         OR trmz0100i.dataFirmaV = *LOVAL
033500090324         OR trmz0100i.societa = *BLANK OR trmz0100i.filiale < 1
033600090324         OR %LEN(trmz0100i.vetRagSoc) = *ZERO OR trmz0100i.vetRagSoc = *BLANK
033700090324         OR %LEN(trmz0100i.vetSlCom) = *ZERO OR trmz0100i.vetSlCom = *BLANK
033800090324         OR %LEN(trmz0100i.vetSlInd) = *ZERO OR trmz0100i.vetSlInd = *BLANK
033900090324         OR %LEN(trmz0100i.vetCdFisca) = *ZERO OR trmz0100i.vetCdFisca = *BLANK
034000090324         OR %LEN(trmz0100i.vetCdIva) = *ZERO OR trmz0100i.vetCdIva = *BLANK
034100090324         OR %LEN(trmz0100i.vetRimLuo) = *ZERO OR trmz0100i.vetRimLuo = *BLANK
034200090324         OR %LEN(trmz0100i.vetRimNum) = *ZERO OR trmz0100i.vetRimNum = *BLANK
034300090422         ;
034400090422           prmRpyOpCode = 'ERRORE';
034500090324           prmRpyIdMsg = ESITO_ERROR;
034600090324           RETURN;
034700090324         ENDIF;
034800090324
034900090324         // Reperisco l'anagrafica della società mittente.
035000090324
035100090429         esito = Societa_NewSocieta( trmz0100i.societa
035200111212                                   : trmz0100i.contratDat
035300090429                                   );
035400090429
035500090429         IF esito = SOCIETA_INFO_SOCIETA_INESISTENTE
035600090429         OR esito < ESITO_OK;
035700090429           prmRpyOpCode = 'ERRORE';
035800090324           prmRpyIdMsg = ESITO_ERROR;
035900090324           RETURN;
036000090324         ENDIF;
036100090324
036200090324         tibsSocO0 = Societa_GetAnagrafica( 'TIBSSOCO0' );
036300090324
036400090324         // Imposto variabili fisse.
036500090422
036600090423         prt_luogoData = %TRIMR(tibsSocO0.sedLegLoc) + ', '
036700111130                       + %CHAR(trmz0100i.dataCertaP : *EUR);
036800090324
036900090324         p = ((%SIZE(prt_firmaSoc)
037000090324           - %LEN(%TRIMR(tibsSocO0.ragSociale))) / 2) + 1;
037100090617         CLEAR prt_firmaSoc;
037200090617         %SUBST( prt_firmaSoc : p ) = tibsSocO0.ragSociale;
037300090324
037400090324         prt_vetRagSoc = trmz0100i.vetRagSoc;
037500090324         prt_vetSlInd = trmz0100i.vetSlInd;
037600090324
037700090324         prt_vetSlCom = trmz0100i.vetSlCap
037800090324                      + ' ' + trmz0100i.vetSlCom
037900090324                      + ' ' + trmz0100i.vetSlPrv
038000090324                      ;
038100090324
038200090324         prt_contratNum = %EDITC(trmz0100i.contratNum : 'X')
038300090324                        + '/'
038400090324                        + %EDITC(trmz0100i.filiale : 'X')
038500090324                        ;
038600090323
038700090324         // Stampo la copia con la firma.
038800090324
038900090324         copiaPerVettore = *ON;
039000090324
039100090323         IF PrtContratto() < ESITO_OK;
039200090323           FEOD trmz0100p;
039300090323           prmRpyOpCode = 'ERRORE';
039400090323           prmRpyIdMsg = ESITO_ERROR;
039500090323           RETURN;
039600090323         ENDIF;
039700090324
039800090324         // Stampo la copia senza la firma.
039900090324
040000090324         copiaPerVettore = *OFF;
040100090324
040200090324         IF PrtContratto() < ESITO_OK;
040300090324           FEOD trmz0100p;
040400090324           prmRpyOpCode = 'ERRORE';
040500090324           prmRpyIdMsg = ESITO_ERROR;
040600090324           RETURN;
040700090324         ENDIF;
040800090323
040900090324       ELSEIF prmRqsOpCode = 'INIT' AND NOT initDone;
041000090324
041100090324         IF Init() < ESITO_OK;
041200090324           prmRpyOpCode = 'ERRORE';
041300090324           prmRpyIdMsg = ESITO_ERROR;
041400090324           RETURN;
041500090324         ENDIF;
041600090323
041700090324       ELSEIF prmRqsOpCode = 'FINALIZE' AND initDone;
041800090323
041900090323         CLOSE trmz0100p;
042000160318
042100090323         ISST_finalize();
042200090323         Societa_finalize();
042300090323         CodeConversionDeallocation ( iconv_t );
042400090324         RESET initDone;
042500090323         *INLR = *ON;
042600090323
042700090323       ELSE;
042800090323
042900090323         prmRpyOpCode = 'ERRORE';
043000090323         prmRpyIdMsg = ESITO_ERROR;
043100090323
043200090323       ENDIF;
043300090318
043400090323       RETURN;
043500090323
043600090318      /END-FREE
043700090323
043800090323     P*--------------------------------------------------
043900090323     P* Procedure name: PrtContratto
044000090323     P* Purpose:        Stampa un contratto.
044100090323     P* Returns:        Esito.
044200090323     P*--------------------------------------------------
044300090323     P PrtContratto    B
044400090323     D PrtContratto    PI            10I 0
044500120315      *
044600120315       // - Stampa O.R.M.
044700120315     d trul33r         pr                  extpgm('TRUL33R')
044800120315     d   kpjba                             likeds(KPJBA)
044900120315
045000120315     D trul33ds      E DS                  QUALIFIED
045100120315     D kpjba         E DS                  QUALIFIED
045200120322     d savkpjbu        s                   like(kpjba.kpjbu)
045300120315      *
045400090323
045500090323      /FREE
045600090323
045700111125       // Recupero la versione del contratto in base alla prima data di stampa
045800111125       // del contratto.
045900111125
046000111207       versioneContratto = GetVersioneContrattoByData(trmz0100i.dataCertaP);
046100111125
046200111125       IF versioneContratto = *BLANK;
046300111125         RETURN ESITO_ERROR;
046400111125       ENDIF;
046500130205          close trmz0100p;
046501161207
046502161207       // Recupero la data di inizio *PDF
046503161207       CLEAR datapdfc;
046504161207
046505161207       EXEC SQL
046506161207         SELECT tbeke2
046507161207           INTO :datapdfc
046508161207           FROM tntbe00f
046509161207           WHERE tbeCod = '007'
046510161207             AND tbeKe1 = '*PDF'
046512161207             AND tbeLin = ''
046513161207             AND tbeSif = ''
046514161207             AND tbeAtb = ''
046516161207           FETCH FIRST 1 ROW ONLY
046517161207       ;
046518161207
046519161207       SELECT;
046520161207         WHEN sqlCode < *ZERO;
046521161207           DUMP(A);
046522161207         WHEN sqlCode = 100;
046523161207           DUMP(A);
046524161207       ENDSL;
046525161207
046528161207       IF datapdfc > %CHAR(trmz0100i.DATACERTAP);
046529161207         versionec='E';
046530161207       else;
046531161207         versionec=versioneContratto;
046532161207       ENDIF;
046600111125
046700161207       OvrPrtFversione = GetOvrPrtfByVersioneContratto(versioneC);
046800130205      /END-FREE
046900160317     c                   exsr      inviapdf
047000130211     c                   IF        OvrPrtFversione <> *BLANK and
047100160502     c                             OvrPrtFversione <> exOvrPrtFvers
047200130205     c                   eval      comman = ovrprtfversione
047300160317     c                   eval      commanl = %trim(Comman)+ trtcm1ds + ''')'
047400160317     c                   eval      lenght = %len(%trim(commanl))
047500130211     c                   Call(e)   'QCMDEXC'
047600160317     c                   Parm                    commanl         500
047700130205     c                   Parm                    lenght
047800160502     c                   eval      exovrprtfvers = ovrprtfversione
047900130205     c                   endif
048000130205      /FREE
048100130205
048200130205        //APRE PRINTER FILE
048300130205          OPEN trmz0100p;
048400130205
048500111125       // Recupero la cartella IFS dei testi in base alla versione del contratto.
048600111125
048700111125       cartellaTesti = GetCartellaTestiByVersioneContratto(versioneContratto);
048800111125
048900111125       IF cartellaTesti = *BLANK;
049000111125         RETURN ESITO_ERROR;
049100111125       ENDIF;
049200111125
049300111125       // Carico le sezioni di testo.
049400111125
049500111125       testoIntroduzione = GetTestoSezione('contratto_introduzione.txt');
049600111125       testoPartiMittente = GetTestoSezione('contratto_parti_mittente.txt');
049700111125       testoPartiVettore = GetTestoSezione('contratto_parti_vettore.txt');
049800111125       testoPremesso1 = GetTestoSezione('contratto_premesso_1.txt');
049900111125       testoPremesso1a = GetTestoSezione('contratto_premesso_1a.txt');
050000111125       testoPremesso1b = GetTestoSezione('contratto_premesso_1b.txt');
050100111125       testoPremesso2 = GetTestoSezione('contratto_premesso_2.txt');
050200111125       testoPremesso3 = GetTestoSezione('contratto_premesso_3.txt');
050300111125       testoPremesso4 = GetTestoSezione('contratto_premesso_4.txt');
050400111125
050401161207       nrart = %dec(numeroart: 2: 0);
050500161207       FOR a = 1 TO nrart;
050600111125         titoloArticolo(a) = GetTestoSezione('contratto_articolo_'
050700111125                           + %EDITC(a : 'X') + '_titolo.txt');
050800111125         testoArticolo(a)  = GetTestoSezione('contratto_articolo_'
050900111125                           + %EDITC(a : 'X') + '.txt');
051000111125       ENDFOR;
051100111125
051200111125       testoDichiarazioneVisione =
051300111125                         GetTestoSezione('contratto_dichiarazione_visione.txt');
051400111207
051500111207       testoFirmaVettore = GetTestoSezione('contratto_firma_vettore.txt');
051600111125
051700111125       // C'è stato un errore nel caricamento dei testi del contratto.
051800111125
051900111125       IF prmRpyIdMsg < ESITO_OK;
052000111125         RETURN ESITO_ERROR;
052100111125       ENDIF;
052200111130
052300111130       dataAccettazione = trmz0100i.dataFirmaV;
052400111125
052500090323       //***********************************************************************
052600090323       //
052700090323       // Stampo l'introduzione.
052800090323       //
052900090323       //***********************************************************************
053000090323
053100090422       RESET overFlow;
053200090323
053300120315        exsr repNUM;
053400090323       WRITE pagina;
053500160412       // box bollo asteriscata la scrittura  WRITE dtCertaBox;
053600090422       indPrtf.stampareVettore = *ON;
053700090422       WRITE dtCertaTxt;
053800090422       indPrtf.stampareVettore = *OFF;
053900090323       WRITE inizioDoc;
054000090323
054100090324       IF RunRigaTesto(testoIntroduzione) < ESITO_OK;
054200090323         RETURN ESITO_ERROR;
054300090323       ENDIF;
054400090323
054500090323       //***********************************************************************
054600090323       //
054700090323       // Stampo la sezione "Contratto di trasporto tra le parti ... e ...".
054800090323       //
054900090323       //***********************************************************************
055000090323
055100090323       RunOverFlow();
055200090323
055300090323       CLEAR prt_testo;
055400090323       %SUBST(prt_testo : 55) = 'CONTRATTO DI TRASPORTO';
055500090323       indPrtf.grassetto = *ON;
055600090323       WRITE riga;
055700090323       RESET indPrtf.grassetto;
055800090323
055900090323       RunOverFlow();
056000090323
056100090323       CLEAR prt_testo;
056200090323       %SUBST(prt_testo : 59) = 'Tra le parti:';
056300090323       WRITE riga;
056400090323
056500090324       IF RunRigaTesto(testoPartiMittente) < ESITO_OK;
056600090323         RETURN ESITO_ERROR;
056700090323       ENDIF;
056800090323
056900090323       RunOverFlow();
057000090323
057100090323       CLEAR prt_testo;
057200090323       %SUBST(prt_testo : 65) = 'e';
057300090323       WRITE riga;
057400090323
057500090324       IF RunRigaTesto(testoPartiVettore) < ESITO_OK;
057600090323         RETURN ESITO_ERROR;
057700090323       ENDIF;
057800090323
057900090323       //***********************************************************************
058000090323       //
058100090323       // Stampo la sezione "Premesso che".
058200090323       //
058300090323       //***********************************************************************
058400090323
058500090323       RunOverFlow();
058600090323
058700090323       CLEAR prt_testo;
058800090323       %SUBST(prt_testo : 59) = 'PREMESSO CHE:';
058900090323       WRITE riga;
059000090323
059100090324       IF RunRigaTesto(testoPremesso1) < ESITO_OK;
059200090323         RETURN ESITO_ERROR;
059300090323       ENDIF;
059400090323
059500090324       IF RunRigaTesto(testoPremesso1a) < ESITO_OK;
059600090323         RETURN ESITO_ERROR;
059700090323       ENDIF;
059800090323
059900090324       IF RunRigaTesto(testoPremesso1b) < ESITO_OK;
060000090323         RETURN ESITO_ERROR;
060100090323       ENDIF;
060200090323
060300090324       IF RunRigaTesto(testoPremesso2) < ESITO_OK;
060400090323         RETURN ESITO_ERROR;
060500090323       ENDIF;
060600090323
060700090324       IF RunRigaTesto(testoPremesso3) < ESITO_OK;
060800090323         RETURN ESITO_ERROR;
060900090323       ENDIF;
061000090323
061100090324       IF RunRigaTesto(testoPremesso4) < ESITO_OK;
061200090323         RETURN ESITO_ERROR;
061300090323       ENDIF;
061400090323
061500090323       //***********************************************************************
061600090323       //
061700090323       // Stampo gli articoli del contratto.
061800090323       //
061900090323       //***********************************************************************
062000090323
062100090323       RunOverFlow();
062200090323       CLEAR prt_testo;
062300090323       %SUBST(prt_testo : 54) = 'SI CONVIENE QUANTO SEGUE';
062400090323       WRITE riga;
062500111125
062600161207       FOR a = 1 TO nrart;
062700111125
062800111125         indPrtf.grassetto = *ON;
062900111125
063000111125         IF RunRigaTesto(titoloArticolo(a)) < ESITO_OK;
063100111125           RETURN ESITO_ERROR;
063200111125         ENDIF;
063300111125
063400111125         RESET indPrtf.grassetto;
063500111125
063600111125         IF RunRigaTesto(testoArticolo(a)) < ESITO_OK;
063700111125           RETURN ESITO_ERROR;
063800111125         ENDIF;
063900111125
064000111125       ENDFOR;
064100111125
064200090323       //***********************************************************************
064300090323       //
064400090324       // Stampo chiusura contratto e spazi per le firma.
064500090323       //
064600090323       //***********************************************************************
064700111128
064800111128       RunOverFlow();
064900111128       CLEAR prt_testo;
065000111128       WRITE riga;
065100090324
065200090324       RunOverFlow();
065300090323       prt_testo = '(La mittente) ' + tibsSocO0.ragSociale;
065400090323       WRITE riga;
065500111128
065600111128       RunOverFlow();
065700111128       CLEAR prt_testo;
065800111128       WRITE riga;
065900111128
066000111207       RunRigaTesto(testoFirmaVettore);
066100090324
066200090324       RunOverFlow();
066300090324       CLEAR prt_testo;
066400090324       WRITE riga;
066500090323
066600090324       IF RunRigaTesto(testoDichiarazioneVisione) < ESITO_OK;
066700090324         RETURN ESITO_ERROR;
066800090324       ENDIF;
066900090323
067000090324       RunOverFlow();
067100111128       CLEAR prt_testo;
067200111128       WRITE riga;
067300111128
067400111128       RunOverFlow();
067500111128       CLEAR prt_testo;
067600111128       WRITE riga;
067700111207
067800111207       RunRigaTesto(testoFirmaVettore);
067900090324
068000090324       IF copiaPerVettore;
068100130403       IF versionecontratto <> 'A' and versionecontratto <> 'B';
068200130205         WRITE firmaTxt;
068300130205         else;
068400130205         write firmaTxto;
068500130205         endif;
068600090324         WRITE firmaImg;
068700160428      /END-FREE
068800160428     C*
068900160428     C                   CALL      'TIS7P2SR4'
069000160428     C                   PARM                    pin_tla           1
069100160428     C                   PARM                    pin_tips          3
069200160428     C                   PARM                    pin_NomeFile     60
069300160428     C                   PARM                    pin_Extfile      11
069400160428     C                   PARM                    pOut_esito        1
069500160428     C*
069600160428      /FREE
069700160317         else;
069800160317         prt_bcd128 = trmz0100i.societa + %EDITC(trmz0100i.filiale : 'X')
069900160317                        + %EDITC(trmz0100i.contratnum : 'X') + '0000000'
070000160317                        + %EDITC(DATACONTR : 'X')
070100160317                        ;
070200160318            cbarre28 = prt_bcd128;
070300160318              exsr srchkdgt;
070400160318            prt_bcd128 = cbarre29;
070500160317         write codbarre;
070600160428      /END-FREE
070700160428     C                   CALL      'TIS7P2SR4'
070800160428     C                   PARM                    pin_tla2          1
070900160428     C                   PARM                    pin_tips2         3
071000160428     C                   PARM                    pin_NomeFile2    60
071100160428     C                   PARM                    pin_Extfile2     11
071200160428     C                   PARM                    pOut_esito2       1
071300160428      /FREE
071400090324       ENDIF;
071500090324
071600090324       FEOD trmz0100p;
071700090323
071800090323       RETURN ESITO_OK;
071900120315
072000120315       //***********************************************************************
072100120315       //
072200120315       // Estrae numero progressivo pagina
072300120315       //
072400120315       //***********************************************************************
072500120315          begsr repnum;
072600120315
072700120322           eval  savkpjbu = kpjba.kpjbu ;
072800120315           clear  PRT_numprogr;
072900120315           clear  trul33ds;
073000120315           eval  trul33ds.i33tla = ' ';
073100120315           eval  trul33ds.i33cnu = 80;
073200120315           eval  trul33ds.i33num = 1;
073300120315           eval  kpjba.kpjbu  = trul33ds;
073400120315
073500120315             TRUL33R(kpjba);
073600120315
073700120315           eval trul33ds = kpjba.kpjbu;
073800120315
073900120315           if  trul33ds.o33err <> *zeros;
074000120315               dump;
074100120315           else;
074200120315               eval  PRT_numPROGR = trul33ds.o33nrf;
074300120315           endif;
074400120315
074500120322           eval   kpjba.kpjbu = savkpjbu ;
074600120315          endsr;
074700120315
074800120315       //***********************************************************************
074900120315      /END-FREE
075000160318     C*-----------------------------------------------------*
075100160318     C* controllo check digits
075200160318     C*-----------------------------------------------------*
075300160318     C     srchkdgt      BEGSR
075400160318      *
075500160318     c                   movel(p)  cbarre28      cbarre29         29
075600160318     C                   clear                   trul28dsx
075700160318     c                   eval      i284std = 'E13'
075800160318     c                   eval      i284cod = cbarre28
075900160318     c                   call      'TRUL28R4'
076000160318     c                   parm                    trul28dsx
076100160318     c* se reperito barcode imposto i campi in stampa
076200160318     c                   if        o284err = *blanks
076300160318     c                   move      O284CKD       cbarre29
076400160318     c                   end
076500160318      *
076600160318     C                   ENDSR
076700160317     C*------------------------------------------------------*
076800160317     C*  reperisce dati necessari preparazione PDF
076900160317     C*------------------------------------------------------*
077000160317     C     inviaPDF      begsr
077100160317      * verifico se l'invio è tramite PDF
077200160317     c                   clear                   trtcm1ds
077300160317     c                   move      046           §cm1po
077400160317     c                   move      '0'           §cm1sts
077500160317     c                   eval      §cm1idp = '*'
077600160317     c                   eval      §cm1pcl = 'S'
077700160502     c                   if        copiaPerVettore = *on
077800160502     c                   clear                   exOvrprtFvers
077900160318     c                   eval      §cm1tips= 'ACC'
078000160317     c                   eval      §cm1var = '*FOUT*' + trmz0100i.societa + '_'
078100160317     c                             +  %editc(trmz0100i.filiale :'X') + '_'
078200160329     c                             +  %editc(trmz0100i.contratnum :'X') + '_'
078300160329     c                             +  '0000000' + '_'
078400160421     c                             +  %editc(datacontr :'X')
078500160317     C* Imposto i parametri x il primo pdf con firma
078600160317     C                   EVAL      pin_tla    = *blanks
078700160317     C                   EVAL      pin_tips   = §cm1tips
078800160317     C                   EVAL      pin_NomeFile = %subst(§cm1var:7)
078900160317     C                   EVAL      pin_Extfile  = '.PDF'
079000160317     C                   EVAL      pOut_esito  = ' '
079100160318      *resetto ovr
079200160318      *
079300160502     c                   else
079400160502     c                   clear                   exOvrprtFvers
079500160318     c                   eval      §cm1tips = 'ACS'
079600160317     c                   eval      §cm1var = '*FOUT*' + trmz0100i.societa + '_'
079700160317     c                             +  %editc(trmz0100i.filiale :'X') + '_'
079800160329     c                             +  %editc(trmz0100i.contratnum :'X') + '_'
079900160329     c                             +  '0000000' + '_'
080000160421     c                             +  %editc(datacontr :'X')
080100160317     C                   EVAL      pin_tla2   = *blanks
080200160317     C                   EVAL      pin_tips2  = §cm1tips
080300160317     C                   EVAL      pin_NomeFile2 = %subst(§cm1var:7)
080400160317     C                   EVAL      pin_Extfile2  = '.PDF'
080500160317     C                   EVAL      pOut_esito2  = ' '
080600160502     c                   endif
080700160317     C*
080800160317     C                   endsr
080900090323     P PrtContratto    E
081000090323
081100090318
081200090318     P*--------------------------------------------------
081300090319     P* Procedure name: RunRigaTesto
081400090319     P* Purpose:        Prepara e scrive una riga di testo.
081500090318     P* Returns:        Esito.
081600090318     P*--------------------------------------------------
081700090319     P RunRigaTesto    B
081800090319     D RunRigaTesto    PI            10I 0
081900090324     D  piTestoSezione...
082000090324     D                            32740A   VARYING
082100090324     D                                     VALUE
082200090318
082300090318     D posizioneProgressiva...
082400090318     D                 S             10I 0
082500090318
082600090318      /FREE
082700090318
082800090318       // Ricerca e sostituzione delle variabili.
082900090318
083000090320       outBufVar = %TRIMR(outBuf);
083100090320
083200090318       EXEC SQL
083300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
083400090324                                      , '&MITTENTE_RAGIONE_SOCIALE'
083500090324                                      , :tibsSocO0.ragSociale
083600090324                                      )
083700090323       ;
083800090318
083900090318       EXEC SQL
084000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
084100090324                                      , '&MITTENTE_COMUNE'
084200090324                                      , :tibsSocO0.sedLegLoc
084300090324                                      )
084400090324       ;
084500090318
084600090318       EXEC SQL
084700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
084800090324                                      , '&MITTENTE_INDIRIZZO'
084900090324                                      , :tibsSocO0.sedLegInd
085000090324                                      )
085100090324       ;
085200090324
085300090324       EXEC SQL
085400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
085500090324                                      , '&MITTENTE_REGISTRO_IMPRESE_LUOGO'
085600090324                                      , :tibsSocO0.rimLuogo
085700090324                                      )
085800090324       ;
085900090324
086000090324       EXEC SQL
086100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
086200090324                                      , '&MITTENTE_REGISTRO_IMPRESE_NUMERO'
086300090324                                      , :tibsSocO0.codFiscale
086400090324                                      )
086500090324       ;
086600090324
086700090324       EXEC SQL
086800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
086900090324                                      , '&MITTENTE_REA_LUOGO'
087000090324                                      , :tibsSocO0.reaLuogo
087100090324                                      )
087200090324       ;
087300090324
087400090324       EXEC SQL
087500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
087600090324                                      , '&MITTENTE_REA_NUMERO'
087700090324                                      , :tibsSocO0.reaNumero
087800090324                                      )
087900090324       ;
088000090324
088100090324       EXEC SQL
088200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
088300090324                                      , '&MITTENTE_CODICE_FISCALE'
088400090324                                      , :tibsSocO0.codFiscale
088500090324                                      )
088600090324       ;
088700090324
088800090324       EXEC SQL
088900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
089000090324                                      , '&MITTENTE_PARTITA_IVA'
089100090324                                      , :tibsSocO0.partitaIva
089200090324                                      )
089300090324       ;
089400090319
089500090324       IF tibsSocO0.socioUnico = *ON;
089600090324         EXEC SQL
089700090324           SET :piTestoSezione = REPLACE( :piTestoSezione
089800090324                                        , '&MITTENTE_SOCIO_UNICO'
089900090424                                        , 'a socio unico'
090000090324                                        )
090100090324         ;
090200090324       ELSE;
090300090324         EXEC SQL
090400090324           SET :piTestoSezione = REPLACE( :piTestoSezione
090500090324                                        , '&MITTENTE_SOCIO_UNICO'
090600090324                                        , ''
090700090324                                        )
090800090324         ;
090900090324       ENDIF;
091000090324
091100090324       EXEC SQL
091200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
091300090324                                      , '&VETTORE_RAGIONE_SOCIALE'
091400090324                                      , :trmz0100i.vetRagSoc
091500090324                                      )
091600090324       ;
091700090324
091800090324       EXEC SQL
091900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
092000090324                                      , '&VETTORE_COMUNE'
092100090324                                      , :trmz0100i.vetSlCom
092200090324                                      )
092300090324       ;
092400090324
092500090324       EXEC SQL
092600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
092700090324                                      , '&VETTORE_INDIRIZZO'
092800090324                                      , :trmz0100i.vetSlInd
092900090324                                      )
093000090324       ;
093100090324
093200090324       EXEC SQL
093300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
093400090324                                      , '&VETTORE_REGISTRO_IMPRESE_LUOGO'
093500090324                                      , :trmz0100i.vetRimLuo
093600090324                                      )
093700090324       ;
093800090324
093900090324       EXEC SQL
094000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
094100090324                                      , '&VETTORE_REGISTRO_IMPRESE_NUMERO'
094200090324                                      , :trmz0100i.vetRimNum
094300090324                                      )
094400090324       ;
094500090324
094600090324       EXEC SQL
094700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
094800090324                                      , '&VETTORE_PARTITA_IVA'
094900090324                                      , :trmz0100i.vetCdIva
095000090324                                      )
095100090324       ;
095200090324
095300090324       EXEC SQL
095400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
095500090324                                      , '&VETTORE_REA_LUOGO'
095600090324                                      , :trmz0100i.vetReaLuo
095700090324                                      )
095800090324       ;
095900090324
096000090324       EXEC SQL
096100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
096200090324                                      , '&VETTORE_REA_NUMERO'
096300090324                                      , :trmz0100i.vetReaNum
096400090324                                      )
096500090324       ;
096600090324
096700090324       EXEC SQL
096800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
096900090324                                      , '&VETTORE_LEGALE_RAPPRESENTANTE'
097000090324                                      , :trmz0100i.vetLegRap
097100090324                                      )
097200090324       ;
097300090324
097400090324       EXEC SQL
097500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
097600090324                                      , '&VETTORE_ALBO_PROVINCIA'
097700090324                                      , :trmz0100i.vetAnaPrv
097800090324                                      )
097900090324       ;
098000090324
098100090324       EXEC SQL
098200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
098300090324                                      , '&VETTORE_ALBO_NUMERO'
098400090324                                      , :trmz0100i.vetAnaNum
098500090324                                      )
098600090324       ;
098700111130
098800111130       EXEC SQL
098900111130         SET :piTestoSezione = REPLACE( :piTestoSezione
099000111130                                      , '&DATA_CONTRATTO'
099100111130                                      , CHAR(:trmz0100i.contratDat, EUR)
099200111130                                      )
099300111130       ;
099400111207
099500111207       EXEC SQL
099600111207         SET :piTestoSezione = REPLACE( :piTestoSezione
099700111207                                      , '&DATA_FIRMA_ACCETTAZIONE'
099800111207                                      , CHAR(:dataAccettazione, EUR)
099900111207                                      )
100000111207       ;
100100090318
100200090318       // Sottostringo.
100300090320
100400090324       outBuf = piTestoSezione;
100500090324       outBytesLeft = %LEN(piTestoSezione);
100600090318
100700090318       IF ISST_newStringa( outBuf
100800090318                         : outBytesLeft
100900090318                         ) < ESITO_OK;
101000090318         DUMP(A);
101100090318         RETURN ESITO_ERROR;
101200090318       ENDIF;
101300090318
101400090318       // Stampo le sottostrighe.
101500090318
101600090318       DOU posizioneProgressiva >= outBytesLeft;
101700111128         RunOverFlow();
101800090318         prt_testo = ISST_getSottoStringa( %SIZE(prt_testo)
101900090318                                         : posizioneProgressiva
102000090318                                         );
102100111128         IF posizioneProgressiva < outBytesLeft;
102200111128           txtInput = %TRIMR(prt_testo);
102300111128           txtInputLen = %SIZE(prt_testo);
102400111128           Giustifica( txtInput : txtInputLen : txtGiust : txtResto );
102500111128           prt_testo = txtGiust;
102600111128         ENDIF;
102700090319         WRITE riga;
102800090318       ENDDO;
102900090318
103000090318       RETURN ESITO_OK;
103100090318
103200090318      /END-FREE
103300090319     P RunRigaTesto    E
103400090318
103500090319
103600090319     P*--------------------------------------------------
103700090319     P* Procedure name: RunOverFlow
103800090319     P* Purpose:        Gestisce l'overflow.
103900090319     P* Returns:        Esito.
104000090319     P*--------------------------------------------------
104100090319     P RunOverFlow     B
104200090319     D RunOverFlow     PI            10I 0
104300120315      *
104400120315       // - Stampa O.R.M.
104500120315     d trul33r         pr                  extpgm('TRUL33R')
104600120315     d   kpjba                             likeds(KPJBA)
104700120315
104800120315     D trul33ds      E DS                  QUALIFIED
104900120315     D kpjba         E DS                  QUALIFIED
105000120322     D savkpjbu        s                   like(kpjba.kpjbu)
105100120315      *
105200120315
105300090319      /FREE
105400090319
105500090319       IF NOT overFlow;
105600090319         RETURN ESITO_OK;
105700090319       ENDIF;
105800090324
105900090324       IF copiaPerVettore;
106000130403       IF versionecontratto <> 'A' and versionecontratto <> 'B';
106100090324         WRITE firmaTxt;
106200130205         else;
106300130205         write firmaTxto;
106400130205         endif;
106500090324         WRITE firmaImg;
106600090324       ENDIF;
106700090324
106800120315         clear  PRT_numprogr;
106900120315       IF infPrtf.currentPage = 2 or infPrtf.currentPage = 4;
107000120315         exsr repNUM;
107100120315       ENDIF;
107200120315
107300090319       WRITE pagina;
107400090319       RESET overFlow;
107500090319
107501161207       // CON LA VERSIONE F NON LASCIAMO PIÙ LO SPAZIO PER IL TIMPO
107502161207       // NELLA 3 PAGINA
107600161207       IF infPrtf.currentPage = 3 AND versionecontratto < 'F';
107700160412       // box bollo asteriscata la scrittura  WRITE dtCertaBox;
107800090422         WRITE dtCertaTxt;
107900090422       ENDIF;
108000090422
108100090319       RETURN ESITO_OK;
108200120315
108300120315       //***********************************************************************
108400120315       //
108500120315       // Estrae numero progressivo pagina
108600120315       //
108700120315       //***********************************************************************
108800120315          begsr repnum;
108900120315
109000120322           eval  savkpjbu = kpjba.kpjbu ;
109100120315           clear  PRT_numprogr;
109200120315           clear  trul33ds;
109300120315           eval  trul33ds.i33tla = ' ';
109400120315           eval  trul33ds.i33cnu = 80;
109500120315           eval  trul33ds.i33num = 1;
109600120315           eval  kpjba.kpjbu  = trul33ds;
109700120315
109800120315             TRUL33R(kpjba);
109900120315
110000120315           eval trul33ds = kpjba.kpjbu;
110100120315
110200120315           if  trul33ds.o33err <> *zeros;
110300120315               dump;
110400120315           else;
110500120315               eval  PRT_numPROGR = trul33ds.o33nrf;
110600120315           endif;
110700120315
110800120322           eval   kpjba.kpjbu = savkpjbu ;
110900120315          endsr;
111000120315
111100120315       //***********************************************************************
111200120315
111300090319      /END-FREE
111400090319     P RunOverFlow     E
111500090319
111600090324
111700090324     P*--------------------------------------------------
111800090324     P* Procedure name: Init
111900090324     P* Purpose:        Inizializza il modulo.
112000090324     P* Returns:        Esito.
112100090324     P*--------------------------------------------------
112200090324     P Init            B
112300090324     D Init            PI            10I 0
112400090324
112500090324      /FREE
112600090324
112700090324       // Inizializzo una conversione dati da ASCII a EBCDIC.
112800090324
112900111125       fromCode.QTQCCSID = CCSID_MSWIN_LATIN1;
113000090326       toCode.QTQCCSID = CCSID_ITALIAN;
113100090324       fromCode.QTQERVED02 = *ALLX'00';
113200090324       toCode.QTQERVED02 = *ALLX'00';
113300090324
113400090324       iconv_t = CodeConversionAllocation( %ADDR(toCode)
113500090324                                         : %ADDR(fromCode)
113600090324                                         );
113700090324
113800090324       // Inizializzo il modulo sottostringa intelligente.
113900090324
114000090324       IF ISST_init() < ESITO_OK;
114100090324         prmRpyOpCode = 'ERRORE';
114200090324         prmRpyIdMsg = ESITO_ERROR;
114300090324         RETURN ESITO_ERROR;
114400090324       ENDIF;
114500090324
114600090324       // Inizializzo il modulo anagrafico società.
114700090324
114800090324       IF Societa_Init() < ESITO_OK;
114900090324         prmRpyOpCode = 'ERRORE';
115000090324         prmRpyIdMsg = ESITO_ERROR;
115100090324         RETURN ESITO_ERROR;
115200090324       ENDIF;
115300090325
115400090324       // Posso finalmente aprire il printer file.
115500130205       // spostato dopo reperimento versione ed OVRPRTF eventuale
115600130205       // OPEN trmz0100p;
115700090324
115800090324       // Inizializzazione del programma completata.
115900090324
116000090324       initDone = *ON;
116100090324
116200090324       RETURN ESITO_OK;
116300090324
116400090324      /END-FREE
116500090324     P Init            E
116600090324
116700090324
116800090324     P*--------------------------------------------------
116900090324     P* Procedure name: GetTestoSezione
117000090324     P* Purpose:        Restituisce il testo di una sezione.
117100090324     P* Returns:        Testo della sezione.
117200090324     P* Parameter:      piNomeFile => Nome file che contiene il testo della...
117300090324     P*                           sezione.
117400090324     P*--------------------------------------------------
117500090324     PGetTestoSezione  B
117600090324     DGetTestoSezione  PI         32767A   VARYING
117700090324     D  piNomeFile                  255A   VARYING
117800090324     D                                     VALUE
117900090324
118000090324      /FREE
118100090324
118200111125       fd = IFS_OpenFile( cartellaTesti + '/' + piNomeFile
118300090324                        : O_RDONLY);
118400090324
118500090324       IF fd < 0;
118600090324         errnoPtr = getErrno();
118700090326         prmRpyIdMsg = errno * -1;
118800090326         prmRpyOpCode = 'ERRORE';
118900090324         DUMP(A);
119000090324         RETURN *BLANK;
119100090324       ENDIF;
119200090324
119300090324       RESET inBuf;
119400090324       RESET inBufPtr;
119500090324
119600090324       IFS_ReadFromDescriptor( fd : inBufPtr : %SIZE(inBuf) );
119700090324       IFS_CloseFile( fd );
119800090324
119900090324       // Conversione ASCII -> EBCDIC.
120000090324
120100090324       inBytesLeft = %LEN(%TRIMR(inBuf));
120200090324
120300090324       IF inBytesLeft = *ZERO;
120400090324         RETURN *BLANK;
120500090324       ENDIF;
120600090324
120700090324       outBytesLeft = inBytesLeft;
120800090324       CLEAR outBuf;
120900090324       RESET outBufPtr;
121000090324
121100090324       IF CodeConversion( iconv_t
121200090324                        : inBufPtr
121300090324                        : inBytesLeft
121400090324                        : outBufPtr
121500090324                        : outBytesLeft
121600090324                        ) < 0;
121700090324         errnoPtr = getErrno();
121800090326         prmRpyIdMsg = errno * -1;
121900090326         prmRpyOpCode = 'ERRORE';
122000090324         DUMP(A);
122100090324         RETURN *BLANK;
122200090324       ENDIF;
122300090324
122400090324       RETURN %TRIMR(outBuf);
122500090324
122600090324      /END-FREE
122700090324     PGetTestoSezione  E
122800090324
122900111124
123000111124     P*--------------------------------------------------
123100111124     P* Procedure name: GetVersioneContrattoByData
123200111124     P* Purpose:        Restituisce la versione del contratto in base alla ...
123300111124     P*                          data.
123400111124     P* Returns:        Versione.
123500111124     P* Parameter:      priData => Data per calcolo versione.
123600111124     P*--------------------------------------------------
123700111124     P GetVersioneContrattoByData...
123800111124     P                 B
123900111124     D GetVersioneContrattoByData...
124000111124     D                 PI             1A
124100111124     D  priData                        D   CONST
124200111124
124300111124     D retField        S              1A   STATIC
124400111124
124500111124      /FREE
124600111124
124700111124       CLEAR retField;
124701161207       CLEAR numeroart;
124800111124
124900111124       EXEC SQL
125000161207         SELECT LEFT(tbeUni, 1), substr(tbeuni, 2, 2)
125100161207           INTO :retField, :numeroart
125200111124           FROM tntbe00f
125300111124           WHERE tbeCod = '007'
125400111124             AND tbeKe1 = '*VERSIONE'
125500111124             AND tbeKe2 <= :priData
125600111124             AND tbeLin = ''
125700111124             AND tbeSif = ''
125800111124             AND tbeAtb = ''
125900111124           ORDER BY tbeCod, tbeKe1, tbeKe2 DESC
126000111124           FETCH FIRST 1 ROW ONLY
126100111124       ;
126200111124
126300111124       SELECT;
126400111124         WHEN sqlCode < *ZERO;
126500111124           DUMP(A);
126600111124         WHEN sqlCode = 100;
126700111124           DUMP(A);
126800111124       ENDSL;
126900111124
127000111124       RETURN retField;
127100111124
127200111124      /END-FREE
127300111124     P GetVersioneContrattoByData...
127400111124     P                 E
127500111124
127600111125
127700111125     P*--------------------------------------------------
127800111125     P* Procedure name: GetCartellaTestiByVersioneContratto
127900111125     P* Purpose:        Restituisce la cartella dei testi della versione di...
128000111125     P*                           contratto.
128100111125     P* Returns:        Cartella testi.
128200111125     P* Parameter:      priVersioneContratto => Versione contratto
128300111125     P*--------------------------------------------------
128400111125     P GetCartellaTestiByVersioneContratto...
128500111125     P                 B
128600111125     D GetCartellaTestiByVersioneContratto...
128700111125     D                 PI           255A   VARYING
128800111125     D  priVersioneContratto...
128900111125     D                                1A   CONST
129000111125
129100111125     D retField        S            255A   VARYING STATIC
129200111125
129300111125      /FREE
129400111125
129500111125       CLEAR retField;
129600111125
129700111125       EXEC SQL
129800111125         SELECT RTRIM(tbeUni)
129900111125           INTO :retField
130000111125           FROM tntbe00f
130100111125           WHERE tbeCod = '007'
130200111125             AND tbeKe1 = '*IFSDIRTXT'
130300111125             AND tbeKe2 = :priVersioneContratto
130400111125             AND tbeLin = ''
130500111125             AND tbeSif = ''
130600111125             AND tbeAtb = ''
130700111125       ;
130800111125
130900111125       SELECT;
131000111125         WHEN sqlCode < *ZERO;
131100111125           DUMP(A);
131200111125         WHEN sqlCode = 100;
131300111125           DUMP(A);
131400111125       ENDSL;
131500111125
131600111125       RETURN retField;
131700111125
131800111125      /END-FREE
131900111125     P GetCartellaTestiByVersioneContratto...
132000111125     P                 E
132100111125
132200130205     P*--------------------------------------------------
132300130205     P* Procedure name: GetOvrPrtfByVersioneContratto
132400130205     P* Purpose:        Restituisce eventuale ovrprtf se presente
132500130205     P* Returns:        OVERRIDE
132600130205     P* Parameter:      priVersioneContratto => Versione contratto
132700130205     P*--------------------------------------------------
132800130205     P GetOvrPrtfByVersioneContratto...
132900130205     P                 B
133000130205     D GetOvrPrtfByVersioneContratto...
133100130205     D                 PI           256A
133200130205     D  VersioneContratto...
133300130205     D                                1A   CONST
133400130205
133500130205     D retField        S            256A   STATIC
133600130205
133700130205      /FREE
133800130205
133900130205       CLEAR retField;
134000130205
134100130205       EXEC SQL
134200130205         SELECT RTRIM(tbeUni)
134300130205           INTO :retField
134400130205           FROM tntbe00f
134500130205           WHERE tbeCod = '007'
134600130205             AND tbeKe1 = '*OVRPRTF'
134700161207             AND tbeKe2 = :VersioneContratto
134800130205             AND tbeLin = ''
134900130205             AND tbeSif = ''
135000130205             AND tbeAtb = ''
135100130205       ;
135200130205
135300130205       SELECT;
135400130205         WHEN sqlCode < *ZERO;
135500130205           DUMP(A);
135600130205         WHEN sqlCode = 100;
135700130205           DUMP(A);
135800130205       ENDSL;
135900130205
136000130205       RETURN retField;
136100130205
136200130205      /END-FREE
136300130205     P GetOvrPrtfByVersioneContratto...
136400130205     P                 E
136500130205
