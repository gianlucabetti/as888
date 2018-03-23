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
016200130204     D                 S          32767A   VARYING DIM(24)
016300111125     D titoloArticolo...
016400130204     D                 S            255A   VARYING DIM(24)
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
018100130211     D exOvrprtfversione...
018200130211     D                 S            256A
018300130205     d comman          s            256
018400130205     d lenght          s             15  5 inz(256)
018500160317
018600160317     d DATACONTR       s              8  0
018700160317     d primo           s              1    inz
018800160318     d cbarre28        s             28    inz
018900090318
019000090323     ***************************************************************************
019100090323     **
019200090323     ** Dichiarazione parametri programma.
019300090323     **
019400090323     ***************************************************************************
019500090323     D trmz0100r...
019600090323     D                 PI
019700090323     D  prmRqsOpCode...
019800090323     D                               10A   CONST
019900090323     D  prmRpyOpCode...
020000090323     D                               10A
020100090323     D  prmRpyIdMsg...
020200090323     D                               10I 0
020300090323     D  prmRqsFormato...
020400090323     D                               10A   CONST
020500090323     D                                     OPTIONS(*NOPASS)
020600090323     D  prmRqsData...
020700090324     D                            32767A   OPTIONS(*VARSIZE:*NOPASS)
020800090323     D  prmRqsDataSize...
020900090323     D                               10I 0 CONST
021000090323     D                                     OPTIONS(*NOPASS)
021100090324     D* prmRpyFormato...
021200090324     D*                              10A   CONST
021300090324     D*                                    OPTIONS(*NOPASS)
021400090324     D* prmRpyData...
021500090324     D*                           32767A   OPTIONS(*VARSIZE:*NOPASS)
021600090324     D* prmRpyDataSize...
021700090324     D*                              10I 0 CONST
021800090324     D*                                    OPTIONS(*NOPASS)
021900090323
022000090323     D*--------------------------------------------------
022100090323     D* Procedure name: PrtContratto
022200090323     D* Purpose:        Stampa un contratto.
022300090323     D* Returns:        Esito.
022400090323     D*--------------------------------------------------
022500090323     D PrtContratto    PR            10I 0
022600090323
022700090318     D*--------------------------------------------------
022800090319     D* Procedure name: RunRigaTesto
022900090319     D* Purpose:        Prepara e scrive una riga di testo.
023000090318     D* Returns:        Esito.
023100090318     D*--------------------------------------------------
023200090319     D RunRigaTesto    PR            10I 0
023300090324     D  piTestoSezione...
023400090324     D                            32740A   VARYING
023500090324     D                                     VALUE
023600090319
023700090319     D*--------------------------------------------------
023800090319     D* Procedure name: RunOverFlow
023900090319     D* Purpose:        Gestisce l'overflow.
024000090319     D* Returns:        Esito.
024100090319     D*--------------------------------------------------
024200090319     D RunOverFlow     PR            10I 0
024300090324
024400090324     D*--------------------------------------------------
024500090324     D* Procedure name: Init
024600090324     D* Purpose:        Inizializza il modulo.
024700090324     D* Returns:        Esito.
024800090324     D*--------------------------------------------------
024900090324     D Init            PR            10I 0
025000090324
025100090324     D*--------------------------------------------------
025200090324     D* Procedure name: GetTestoSezione
025300090324     D* Purpose:        Restituisce il testo di una sezione.
025400090324     D* Returns:        Testo della sezione.
025500090324     D* Parameter:      piNomeFile => Nome file che contiene il testo della...
025600090324     D*                           sezione.
025700090324     D*--------------------------------------------------
025800090324     DGetTestoSezione  PR         32767A   VARYING
025900090324     D  piNomeFile                  255A   VARYING
026000090324     D                                     VALUE
026100111124
026200111124     D*--------------------------------------------------
026300111124     D* Procedure name: GetVersioneContrattoByData
026400111124     D* Purpose:        Restituisce la versione del contratto in base alla ...
026500111124     D*                          data.
026600111124     D* Returns:        Versione.
026700111124     D* Parameter:      priData => Data per calcolo versione.
026800111124     D*--------------------------------------------------
026900111124     D GetVersioneContrattoByData...
027000111124     D                 PR             1A
027100111124     D  priData                        D   CONST
027200111125
027300111125     D*--------------------------------------------------
027400111125     D* Procedure name: GetCartellaTestiByVersioneContratto
027500111125     D* Purpose:        Restituisce la cartella dei testi della versione di...
027600111125     D*                           contratto.
027700111125     D* Returns:        Cartella testi.
027800111125     D* Parameter:      priVersioneContratto => Versione contratto
027900111125     D*--------------------------------------------------
028000111125     D GetCartellaTestiByVersioneContratto...
028100111125     D                 PR           255A   VARYING
028200111125     D  priVersioneContratto...
028300111125     D                                1A   CONST
028400130205     D*--------------------------------------------------
028500130205     D* Procedure name: GetCartellaTestiByVersioneContratto
028600130205     D* Purpose:        Restituisce la cartella dei testi della versione di...
028700130205     D*                           contratto.
028800130205     D* Returns:        Cartella testi.
028900130205     D* Parameter:      priVersioneContratto => Versione contratto
029000130205     D*--------------------------------------------------
029100130205     D GetOvrPrtfByVersioneContratto...
029200130205     D                 PR           256A
029300130205     D  priVersioneContratto...
029400130205     D                                1A   CONST
029500111125
029600111125
029700111125
029800090319      /FREE
029900090318
030000090323       // Inizializzo i parametri di output.
030100090323
030200090323       prmRpyOpCode = 'DONE';
030300090323       CLEAR prmRpyIdMsg;
030400090323
030500090324       IF prmRqsOpCode = 'PRTCONTRAT' AND initDone;
030600090324
030700090324         // Controllo parametri ricevuti.
030800090324
030900090324         IF prmRqsFormato = 'TRMZ0100I';
031000090324           trmz0100i = %SUBST(prmRqsData : 1 : prmRqsDataSize);
031100111130           IF prmRqsDataSize = 505; // Vecchia versione del parametro.
031200111130             trmz0100i.dataCertaP = trmz0100i.contratDat;
031300160317             RESET primo ;
031400160317             RESET trtcm1ds;
031500111130             RESET trul13ds;
031600111130             trul13ds.i13fil = trmz0100i.filiale;
031700111130             trul13ds.i13din = %DEC(trmz0100i.contratDat);
031800111130             trul13ds.i13gio = 1;
031900111130             Trul13r(trul13ds);
032000111130             IF trul13ds.o13err = *BLANK;
032100111130               trmz0100i.dataFirmaV = %DATE(trul13ds.o13dfi : *ISO);
032200111130             ELSE;
032300111130               trmz0100i.dataFirmaV = trmz0100i.contratDat + %DAYS(1);
032400111130             ENDIF;
032500111130           ENDIF;
032600111130         ELSE;
032700090324           prmRpyOpCode = 'ERRORE';
032800090324           prmRpyIdMsg = ESITO_ERROR;
032900090324           RETURN;
033000090324         ENDIF;
033100090324
033200090324         // Parametri obbligatori.
033300090324
033400160317            DATACONTR = %DEC(trmz0100i.contratDat) ;
033500090324         IF trmz0100i.contratDat = *LOVAL OR trmz0100i.contratNum < 1
033600111130         OR trmz0100i.dataStampa = *LOVAL OR trmz0100i.dataCertaP = *LOVAL
033700111130         OR trmz0100i.dataFirmaV = *LOVAL
033800090324         OR trmz0100i.societa = *BLANK OR trmz0100i.filiale < 1
033900090324         OR %LEN(trmz0100i.vetRagSoc) = *ZERO OR trmz0100i.vetRagSoc = *BLANK
034000090324         OR %LEN(trmz0100i.vetSlCom) = *ZERO OR trmz0100i.vetSlCom = *BLANK
034100090324         OR %LEN(trmz0100i.vetSlInd) = *ZERO OR trmz0100i.vetSlInd = *BLANK
034200090324         OR %LEN(trmz0100i.vetCdFisca) = *ZERO OR trmz0100i.vetCdFisca = *BLANK
034300090324         OR %LEN(trmz0100i.vetCdIva) = *ZERO OR trmz0100i.vetCdIva = *BLANK
034400090324         OR %LEN(trmz0100i.vetRimLuo) = *ZERO OR trmz0100i.vetRimLuo = *BLANK
034500090324         OR %LEN(trmz0100i.vetRimNum) = *ZERO OR trmz0100i.vetRimNum = *BLANK
034600090422         ;
034700090422           prmRpyOpCode = 'ERRORE';
034800090324           prmRpyIdMsg = ESITO_ERROR;
034900090324           RETURN;
035000090324         ENDIF;
035100090324
035200090324         // Reperisco l'anagrafica della società mittente.
035300090324
035400090429         esito = Societa_NewSocieta( trmz0100i.societa
035500111212                                   : trmz0100i.contratDat
035600090429                                   );
035700090429
035800090429         IF esito = SOCIETA_INFO_SOCIETA_INESISTENTE
035900090429         OR esito < ESITO_OK;
036000090429           prmRpyOpCode = 'ERRORE';
036100090324           prmRpyIdMsg = ESITO_ERROR;
036200090324           RETURN;
036300090324         ENDIF;
036400090324
036500090324         tibsSocO0 = Societa_GetAnagrafica( 'TIBSSOCO0' );
036600090324
036700090324         // Imposto variabili fisse.
036800090422
036900090423         prt_luogoData = %TRIMR(tibsSocO0.sedLegLoc) + ', '
037000111130                       + %CHAR(trmz0100i.dataCertaP : *EUR);
037100090324
037200090324         p = ((%SIZE(prt_firmaSoc)
037300090324           - %LEN(%TRIMR(tibsSocO0.ragSociale))) / 2) + 1;
037400090617         CLEAR prt_firmaSoc;
037500090617         %SUBST( prt_firmaSoc : p ) = tibsSocO0.ragSociale;
037600090324
037700090324         prt_vetRagSoc = trmz0100i.vetRagSoc;
037800090324         prt_vetSlInd = trmz0100i.vetSlInd;
037900090324
038000090324         prt_vetSlCom = trmz0100i.vetSlCap
038100090324                      + ' ' + trmz0100i.vetSlCom
038200090324                      + ' ' + trmz0100i.vetSlPrv
038300090324                      ;
038400090324
038500090324         prt_contratNum = %EDITC(trmz0100i.contratNum : 'X')
038600090324                        + '/'
038700090324                        + %EDITC(trmz0100i.filiale : 'X')
038800090324                        ;
038900090323
039000090324         // Stampo la copia con la firma.
039100090324
039200090324         copiaPerVettore = *ON;
039300090324
039400090323         IF PrtContratto() < ESITO_OK;
039500090323           FEOD trmz0100p;
039600090323           prmRpyOpCode = 'ERRORE';
039700090323           prmRpyIdMsg = ESITO_ERROR;
039800090323           RETURN;
039900090323         ENDIF;
040000090324
040100090324         // Stampo la copia senza la firma.
040200090324
040300090324         copiaPerVettore = *OFF;
040400090324
040500090324         IF PrtContratto() < ESITO_OK;
040600090324           FEOD trmz0100p;
040700090324           prmRpyOpCode = 'ERRORE';
040800090324           prmRpyIdMsg = ESITO_ERROR;
040900090324           RETURN;
041000090324         ENDIF;
041100090323
041200090324       ELSEIF prmRqsOpCode = 'INIT' AND NOT initDone;
041300090324
041400090324         IF Init() < ESITO_OK;
041500090324           prmRpyOpCode = 'ERRORE';
041600090324           prmRpyIdMsg = ESITO_ERROR;
041700090324           RETURN;
041800090324         ENDIF;
041900090323
042000090324       ELSEIF prmRqsOpCode = 'FINALIZE' AND initDone;
042100090323
042200090323         CLOSE trmz0100p;
042300160318       if  primo <> *blank;
042400160318          exsr generaPDF;
042500160318          reset primo;
042600160318       endif;
042700160318
042800090323         ISST_finalize();
042900090323         Societa_finalize();
043000090323         CodeConversionDeallocation ( iconv_t );
043100090324         RESET initDone;
043200090323         *INLR = *ON;
043300090323
043400090323       ELSE;
043500090323
043600090323         prmRpyOpCode = 'ERRORE';
043700090323         prmRpyIdMsg = ESITO_ERROR;
043800090323
043900090323       ENDIF;
044000090318
044100090323       RETURN;
044200090323
044300090318      /END-FREE
044400160317     c*______________________________________________________________
044500160317     c     GeneraPDF     begsr
044600160317     c*______________________________________________________________
044700160317     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
044800160317     C*
044900160317     C                   CALL      'TIS7P2SR4'
045000160317     C                   PARM                    pin_tla           1
045100160317     C                   PARM                    pin_tips          3
045200160317     C                   PARM                    pin_NomeFile     60
045300160317     C                   PARM                    pin_Extfile      11
045400160317     C                   PARM                    pOut_esito        1
045500160317     C*
045600160317     C                   CALL      'TIS7P2SR4'
045700160317     C                   PARM                    pin_tla2          1
045800160317     C                   PARM                    pin_tips2         3
045900160317     C                   PARM                    pin_NomeFile2    60
046000160317     C                   PARM                    pin_Extfile2     11
046100160317     C                   PARM                    pOut_esito2       1
046200160317      *non trovato indirizzo mail destinatario invio la mail allo stesso
046300160317      * utente che sta inviando
046400160317     c
046500160317     C                   endsr
046600090323
046700090323     P*--------------------------------------------------
046800090323     P* Procedure name: PrtContratto
046900090323     P* Purpose:        Stampa un contratto.
047000090323     P* Returns:        Esito.
047100090323     P*--------------------------------------------------
047200090323     P PrtContratto    B
047300090323     D PrtContratto    PI            10I 0
047400120315      *
047500120315       // - Stampa O.R.M.
047600120315     d trul33r         pr                  extpgm('TRUL33R')
047700120315     d   kpjba                             likeds(KPJBA)
047800120315
047900120315     D trul33ds      E DS                  QUALIFIED
048000120315     D kpjba         E DS                  QUALIFIED
048100120322     d savkpjbu        s                   like(kpjba.kpjbu)
048200120315      *
048300090323
048400090323      /FREE
048500090323
048600111125       // Recupero la versione del contratto in base alla prima data di stampa
048700111125       // del contratto.
048800111125
048900111207       versioneContratto = GetVersioneContrattoByData(trmz0100i.dataCertaP);
049000111125
049100111125       IF versioneContratto = *BLANK;
049200111125         RETURN ESITO_ERROR;
049300111125       ENDIF;
049400130205          close trmz0100p;
049500111125
049600130205       OvrPrtFversione = GetOvrPrtfByVersioneContratto(versioneContratto);
049700130205      /END-FREE
049800160317     c                   exsr      inviapdf
049900160317     c                   move      '1'           primo
050000130211     c                   IF        OvrPrtFversione <> *BLANK and
050100130211     c                             OvrPrtFversione <> exOvrPrtFversione
050200130205     c                   eval      comman = ovrprtfversione
050300160317     c                   eval      commanl = %trim(Comman)+ trtcm1ds + ''')'
050400160317     c                   eval      lenght = %len(%trim(commanl))
050500130211     c                   Call(e)   'QCMDEXC'
050600160317     c                   Parm                    commanl         500
050700130205     c                   Parm                    lenght
050800130211     c                   eval      exovrprtfversione = ovrprtfversione
050900130205     c                   endif
051000130205      /FREE
051100130205
051200130205        //APRE PRINTER FILE
051300130205          OPEN trmz0100p;
051400130205
051500111125       // Recupero la cartella IFS dei testi in base alla versione del contratto.
051600111125
051700111125       cartellaTesti = GetCartellaTestiByVersioneContratto(versioneContratto);
051800111125
051900111125       IF cartellaTesti = *BLANK;
052000111125         RETURN ESITO_ERROR;
052100111125       ENDIF;
052200111125
052300111125       // Carico le sezioni di testo.
052400111125
052500111125       testoIntroduzione = GetTestoSezione('contratto_introduzione.txt');
052600111125       testoPartiMittente = GetTestoSezione('contratto_parti_mittente.txt');
052700111125       testoPartiVettore = GetTestoSezione('contratto_parti_vettore.txt');
052800111125       testoPremesso1 = GetTestoSezione('contratto_premesso_1.txt');
052900111125       testoPremesso1a = GetTestoSezione('contratto_premesso_1a.txt');
053000111125       testoPremesso1b = GetTestoSezione('contratto_premesso_1b.txt');
053100111125       testoPremesso2 = GetTestoSezione('contratto_premesso_2.txt');
053200111125       testoPremesso3 = GetTestoSezione('contratto_premesso_3.txt');
053300111125       testoPremesso4 = GetTestoSezione('contratto_premesso_4.txt');
053400111125
053500111125       FOR a = 1 TO %ELEM(titoloArticolo);
053600111125         titoloArticolo(a) = GetTestoSezione('contratto_articolo_'
053700111125                           + %EDITC(a : 'X') + '_titolo.txt');
053800111125         testoArticolo(a)  = GetTestoSezione('contratto_articolo_'
053900111125                           + %EDITC(a : 'X') + '.txt');
054000111125       ENDFOR;
054100111125
054200111125       testoDichiarazioneVisione =
054300111125                         GetTestoSezione('contratto_dichiarazione_visione.txt');
054400111207
054500111207       testoFirmaVettore = GetTestoSezione('contratto_firma_vettore.txt');
054600111125
054700111125       // C'è stato un errore nel caricamento dei testi del contratto.
054800111125
054900111125       IF prmRpyIdMsg < ESITO_OK;
055000111125         RETURN ESITO_ERROR;
055100111125       ENDIF;
055200111130
055300111130       dataAccettazione = trmz0100i.dataFirmaV;
055400111125
055500090323       //***********************************************************************
055600090323       //
055700090323       // Stampo l'introduzione.
055800090323       //
055900090323       //***********************************************************************
056000090323
056100090422       RESET overFlow;
056200090323
056300120315        exsr repNUM;
056400090323       WRITE pagina;
056500160412       // box bollo asteriscata la scrittura  WRITE dtCertaBox;
056600090422       indPrtf.stampareVettore = *ON;
056700090422       WRITE dtCertaTxt;
056800090422       indPrtf.stampareVettore = *OFF;
056900090323       WRITE inizioDoc;
057000090323
057100090324       IF RunRigaTesto(testoIntroduzione) < ESITO_OK;
057200090323         RETURN ESITO_ERROR;
057300090323       ENDIF;
057400090323
057500090323       //***********************************************************************
057600090323       //
057700090323       // Stampo la sezione "Contratto di trasporto tra le parti ... e ...".
057800090323       //
057900090323       //***********************************************************************
058000090323
058100090323       RunOverFlow();
058200090323
058300090323       CLEAR prt_testo;
058400090323       %SUBST(prt_testo : 55) = 'CONTRATTO DI TRASPORTO';
058500090323       indPrtf.grassetto = *ON;
058600090323       WRITE riga;
058700090323       RESET indPrtf.grassetto;
058800090323
058900090323       RunOverFlow();
059000090323
059100090323       CLEAR prt_testo;
059200090323       %SUBST(prt_testo : 59) = 'Tra le parti:';
059300090323       WRITE riga;
059400090323
059500090324       IF RunRigaTesto(testoPartiMittente) < ESITO_OK;
059600090323         RETURN ESITO_ERROR;
059700090323       ENDIF;
059800090323
059900090323       RunOverFlow();
060000090323
060100090323       CLEAR prt_testo;
060200090323       %SUBST(prt_testo : 65) = 'e';
060300090323       WRITE riga;
060400090323
060500090324       IF RunRigaTesto(testoPartiVettore) < ESITO_OK;
060600090323         RETURN ESITO_ERROR;
060700090323       ENDIF;
060800090323
060900090323       //***********************************************************************
061000090323       //
061100090323       // Stampo la sezione "Premesso che".
061200090323       //
061300090323       //***********************************************************************
061400090323
061500090323       RunOverFlow();
061600090323
061700090323       CLEAR prt_testo;
061800090323       %SUBST(prt_testo : 59) = 'PREMESSO CHE:';
061900090323       WRITE riga;
062000090323
062100090324       IF RunRigaTesto(testoPremesso1) < ESITO_OK;
062200090323         RETURN ESITO_ERROR;
062300090323       ENDIF;
062400090323
062500090324       IF RunRigaTesto(testoPremesso1a) < ESITO_OK;
062600090323         RETURN ESITO_ERROR;
062700090323       ENDIF;
062800090323
062900090324       IF RunRigaTesto(testoPremesso1b) < ESITO_OK;
063000090323         RETURN ESITO_ERROR;
063100090323       ENDIF;
063200090323
063300090324       IF RunRigaTesto(testoPremesso2) < ESITO_OK;
063400090323         RETURN ESITO_ERROR;
063500090323       ENDIF;
063600090323
063700090324       IF RunRigaTesto(testoPremesso3) < ESITO_OK;
063800090323         RETURN ESITO_ERROR;
063900090323       ENDIF;
064000090323
064100090324       IF RunRigaTesto(testoPremesso4) < ESITO_OK;
064200090323         RETURN ESITO_ERROR;
064300090323       ENDIF;
064400090323
064500090323       //***********************************************************************
064600090323       //
064700090323       // Stampo gli articoli del contratto.
064800090323       //
064900090323       //***********************************************************************
065000090323
065100090323       RunOverFlow();
065200090323       CLEAR prt_testo;
065300090323       %SUBST(prt_testo : 54) = 'SI CONVIENE QUANTO SEGUE';
065400090323       WRITE riga;
065500111125
065600111125       FOR a = 1 TO %ELEM(titoloArticolo);
065700111125
065800111125         indPrtf.grassetto = *ON;
065900111125
066000111125         IF RunRigaTesto(titoloArticolo(a)) < ESITO_OK;
066100111125           RETURN ESITO_ERROR;
066200111125         ENDIF;
066300111125
066400111125         RESET indPrtf.grassetto;
066500111125
066600111125         IF RunRigaTesto(testoArticolo(a)) < ESITO_OK;
066700111125           RETURN ESITO_ERROR;
066800111125         ENDIF;
066900111125
067000111125       ENDFOR;
067100111125
067200090323       //***********************************************************************
067300090323       //
067400090324       // Stampo chiusura contratto e spazi per le firma.
067500090323       //
067600090323       //***********************************************************************
067700111128
067800111128       RunOverFlow();
067900111128       CLEAR prt_testo;
068000111128       WRITE riga;
068100090324
068200090324       RunOverFlow();
068300090323       prt_testo = '(La mittente) ' + tibsSocO0.ragSociale;
068400090323       WRITE riga;
068500111128
068600111128       RunOverFlow();
068700111128       CLEAR prt_testo;
068800111128       WRITE riga;
068900111128
069000111207       RunRigaTesto(testoFirmaVettore);
069100090324
069200090324       RunOverFlow();
069300090324       CLEAR prt_testo;
069400090324       WRITE riga;
069500090323
069600090324       IF RunRigaTesto(testoDichiarazioneVisione) < ESITO_OK;
069700090324         RETURN ESITO_ERROR;
069800090324       ENDIF;
069900090323
070000090324       RunOverFlow();
070100111128       CLEAR prt_testo;
070200111128       WRITE riga;
070300111128
070400111128       RunOverFlow();
070500111128       CLEAR prt_testo;
070600111128       WRITE riga;
070700111207
070800111207       RunRigaTesto(testoFirmaVettore);
070900090324
071000090324       IF copiaPerVettore;
071100130403       IF versionecontratto <> 'A' and versionecontratto <> 'B';
071200130205         WRITE firmaTxt;
071300130205         else;
071400130205         write firmaTxto;
071500130205         endif;
071600090324         WRITE firmaImg;
071700160317         else;
071800160317         prt_bcd128 = trmz0100i.societa + %EDITC(trmz0100i.filiale : 'X')
071900160317                        + %EDITC(trmz0100i.contratnum : 'X') + '0000000'
072000160317                        + %EDITC(DATACONTR : 'X')
072100160317                        ;
072200160318            cbarre28 = prt_bcd128;
072300160318              exsr srchkdgt;
072400160318            prt_bcd128 = cbarre29;
072500160317         write codbarre;
072600090324       ENDIF;
072700090324
072800090324       FEOD trmz0100p;
072900090323
073000090323       RETURN ESITO_OK;
073100120315
073200120315       //***********************************************************************
073300120315       //
073400120315       // Estrae numero progressivo pagina
073500120315       //
073600120315       //***********************************************************************
073700120315          begsr repnum;
073800120315
073900120322           eval  savkpjbu = kpjba.kpjbu ;
074000120315           clear  PRT_numprogr;
074100120315           clear  trul33ds;
074200120315           eval  trul33ds.i33tla = ' ';
074300120315           eval  trul33ds.i33cnu = 80;
074400120315           eval  trul33ds.i33num = 1;
074500120315           eval  kpjba.kpjbu  = trul33ds;
074600120315
074700120315             TRUL33R(kpjba);
074800120315
074900120315           eval trul33ds = kpjba.kpjbu;
075000120315
075100120315           if  trul33ds.o33err <> *zeros;
075200120315               dump;
075300120315           else;
075400120315               eval  PRT_numPROGR = trul33ds.o33nrf;
075500120315           endif;
075600120315
075700120322           eval   kpjba.kpjbu = savkpjbu ;
075800120315          endsr;
075900120315
076000120315       //***********************************************************************
076100120315      /END-FREE
076200160318     C*-----------------------------------------------------*
076300160318     C* controllo check digits
076400160318     C*-----------------------------------------------------*
076500160318     C     srchkdgt      BEGSR
076600160318      *
076700160318     c                   movel(p)  cbarre28      cbarre29         29
076800160318     C                   clear                   trul28dsx
076900160318     c                   eval      i284std = 'E13'
077000160318     c                   eval      i284cod = cbarre28
077100160318     c                   call      'TRUL28R4'
077200160318     c                   parm                    trul28dsx
077300160318     c* se reperito barcode imposto i campi in stampa
077400160318     c                   if        o284err = *blanks
077500160318     c                   move      O284CKD       cbarre29
077600160318     c                   end
077700160318      *
077800160318     C                   ENDSR
077900160317     C*------------------------------------------------------*
078000160317     C*  reperisce dati necessari preparazione PDF
078100160317     C*------------------------------------------------------*
078200160317     C     inviaPDF      begsr
078300160317      * verifico se l'invio è tramite PDF
078400160317     c                   clear                   trtcm1ds
078500160317     c                   move      046           §cm1po
078600160317     c                   move      '0'           §cm1sts
078700160317     c                   eval      §cm1idp = '*'
078800160317     c                   eval      §cm1pcl = 'S'
078900160317     c                   if        primo = *blank
079000160318     c                   eval      §cm1tips= 'ACC'
079100160317     c                   eval      §cm1var = '*FOUT*' + trmz0100i.societa + '_'
079200160317     c                             +  %editc(trmz0100i.filiale :'X') + '_'
079300160329     c                             +  %editc(trmz0100i.contratnum :'X') + '_'
079400160329     c                             +  '0000000' + '_'
079500160421     c                             +  %editc(datacontr :'X')
079600160317     C* Imposto i parametri x il primo pdf con firma
079700160317     C                   EVAL      pin_tla    = *blanks
079800160317     C                   EVAL      pin_tips   = §cm1tips
079900160317     C                   EVAL      pin_NomeFile = %subst(§cm1var:7)
080000160317     C                   EVAL      pin_Extfile  = '.PDF'
080100160317     C                   EVAL      pOut_esito  = ' '
080200160317     c                   else
080300160318      *resetto ovr
080400160318     c                   eval      exOvrPrtFversione = *blank
080500160318      *
080600160318     c                   eval      §cm1tips = 'ACS'
080700160317     c                   eval      §cm1var = '*FOUT*' + trmz0100i.societa + '_'
080800160317     c                             +  %editc(trmz0100i.filiale :'X') + '_'
080900160329     c                             +  %editc(trmz0100i.contratnum :'X') + '_'
081000160329     c                             +  '0000000' + '_'
081100160421     c                             +  %editc(datacontr :'X')
081200160317     C                   EVAL      pin_tla2   = *blanks
081300160317     C                   EVAL      pin_tips2  = §cm1tips
081400160317     C                   EVAL      pin_NomeFile2 = %subst(§cm1var:7)
081500160317     C                   EVAL      pin_Extfile2  = '.PDF'
081600160317     C                   EVAL      pOut_esito2  = ' '
081700160317     c                   endif
081800160317     C*
081900160317     C                   endsr
082000090323     P PrtContratto    E
082100090323
082200090318
082300090318     P*--------------------------------------------------
082400090319     P* Procedure name: RunRigaTesto
082500090319     P* Purpose:        Prepara e scrive una riga di testo.
082600090318     P* Returns:        Esito.
082700090318     P*--------------------------------------------------
082800090319     P RunRigaTesto    B
082900090319     D RunRigaTesto    PI            10I 0
083000090324     D  piTestoSezione...
083100090324     D                            32740A   VARYING
083200090324     D                                     VALUE
083300090318
083400090318     D posizioneProgressiva...
083500090318     D                 S             10I 0
083600090318
083700090318      /FREE
083800090318
083900090318       // Ricerca e sostituzione delle variabili.
084000090318
084100090320       outBufVar = %TRIMR(outBuf);
084200090320
084300090318       EXEC SQL
084400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
084500090324                                      , '&MITTENTE_RAGIONE_SOCIALE'
084600090324                                      , :tibsSocO0.ragSociale
084700090324                                      )
084800090323       ;
084900090318
085000090318       EXEC SQL
085100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
085200090324                                      , '&MITTENTE_COMUNE'
085300090324                                      , :tibsSocO0.sedLegLoc
085400090324                                      )
085500090324       ;
085600090318
085700090318       EXEC SQL
085800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
085900090324                                      , '&MITTENTE_INDIRIZZO'
086000090324                                      , :tibsSocO0.sedLegInd
086100090324                                      )
086200090324       ;
086300090324
086400090324       EXEC SQL
086500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
086600090324                                      , '&MITTENTE_REGISTRO_IMPRESE_LUOGO'
086700090324                                      , :tibsSocO0.rimLuogo
086800090324                                      )
086900090324       ;
087000090324
087100090324       EXEC SQL
087200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
087300090324                                      , '&MITTENTE_REGISTRO_IMPRESE_NUMERO'
087400090324                                      , :tibsSocO0.codFiscale
087500090324                                      )
087600090324       ;
087700090324
087800090324       EXEC SQL
087900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
088000090324                                      , '&MITTENTE_REA_LUOGO'
088100090324                                      , :tibsSocO0.reaLuogo
088200090324                                      )
088300090324       ;
088400090324
088500090324       EXEC SQL
088600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
088700090324                                      , '&MITTENTE_REA_NUMERO'
088800090324                                      , :tibsSocO0.reaNumero
088900090324                                      )
089000090324       ;
089100090324
089200090324       EXEC SQL
089300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
089400090324                                      , '&MITTENTE_CODICE_FISCALE'
089500090324                                      , :tibsSocO0.codFiscale
089600090324                                      )
089700090324       ;
089800090324
089900090324       EXEC SQL
090000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
090100090324                                      , '&MITTENTE_PARTITA_IVA'
090200090324                                      , :tibsSocO0.partitaIva
090300090324                                      )
090400090324       ;
090500090319
090600090324       IF tibsSocO0.socioUnico = *ON;
090700090324         EXEC SQL
090800090324           SET :piTestoSezione = REPLACE( :piTestoSezione
090900090324                                        , '&MITTENTE_SOCIO_UNICO'
091000090424                                        , 'a socio unico'
091100090324                                        )
091200090324         ;
091300090324       ELSE;
091400090324         EXEC SQL
091500090324           SET :piTestoSezione = REPLACE( :piTestoSezione
091600090324                                        , '&MITTENTE_SOCIO_UNICO'
091700090324                                        , ''
091800090324                                        )
091900090324         ;
092000090324       ENDIF;
092100090324
092200090324       EXEC SQL
092300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
092400090324                                      , '&VETTORE_RAGIONE_SOCIALE'
092500090324                                      , :trmz0100i.vetRagSoc
092600090324                                      )
092700090324       ;
092800090324
092900090324       EXEC SQL
093000090324         SET :piTestoSezione = REPLACE( :piTestoSezione
093100090324                                      , '&VETTORE_COMUNE'
093200090324                                      , :trmz0100i.vetSlCom
093300090324                                      )
093400090324       ;
093500090324
093600090324       EXEC SQL
093700090324         SET :piTestoSezione = REPLACE( :piTestoSezione
093800090324                                      , '&VETTORE_INDIRIZZO'
093900090324                                      , :trmz0100i.vetSlInd
094000090324                                      )
094100090324       ;
094200090324
094300090324       EXEC SQL
094400090324         SET :piTestoSezione = REPLACE( :piTestoSezione
094500090324                                      , '&VETTORE_REGISTRO_IMPRESE_LUOGO'
094600090324                                      , :trmz0100i.vetRimLuo
094700090324                                      )
094800090324       ;
094900090324
095000090324       EXEC SQL
095100090324         SET :piTestoSezione = REPLACE( :piTestoSezione
095200090324                                      , '&VETTORE_REGISTRO_IMPRESE_NUMERO'
095300090324                                      , :trmz0100i.vetRimNum
095400090324                                      )
095500090324       ;
095600090324
095700090324       EXEC SQL
095800090324         SET :piTestoSezione = REPLACE( :piTestoSezione
095900090324                                      , '&VETTORE_PARTITA_IVA'
096000090324                                      , :trmz0100i.vetCdIva
096100090324                                      )
096200090324       ;
096300090324
096400090324       EXEC SQL
096500090324         SET :piTestoSezione = REPLACE( :piTestoSezione
096600090324                                      , '&VETTORE_REA_LUOGO'
096700090324                                      , :trmz0100i.vetReaLuo
096800090324                                      )
096900090324       ;
097000090324
097100090324       EXEC SQL
097200090324         SET :piTestoSezione = REPLACE( :piTestoSezione
097300090324                                      , '&VETTORE_REA_NUMERO'
097400090324                                      , :trmz0100i.vetReaNum
097500090324                                      )
097600090324       ;
097700090324
097800090324       EXEC SQL
097900090324         SET :piTestoSezione = REPLACE( :piTestoSezione
098000090324                                      , '&VETTORE_LEGALE_RAPPRESENTANTE'
098100090324                                      , :trmz0100i.vetLegRap
098200090324                                      )
098300090324       ;
098400090324
098500090324       EXEC SQL
098600090324         SET :piTestoSezione = REPLACE( :piTestoSezione
098700090324                                      , '&VETTORE_ALBO_PROVINCIA'
098800090324                                      , :trmz0100i.vetAnaPrv
098900090324                                      )
099000090324       ;
099100090324
099200090324       EXEC SQL
099300090324         SET :piTestoSezione = REPLACE( :piTestoSezione
099400090324                                      , '&VETTORE_ALBO_NUMERO'
099500090324                                      , :trmz0100i.vetAnaNum
099600090324                                      )
099700090324       ;
099800111130
099900111130       EXEC SQL
100000111130         SET :piTestoSezione = REPLACE( :piTestoSezione
100100111130                                      , '&DATA_CONTRATTO'
100200111130                                      , CHAR(:trmz0100i.contratDat, EUR)
100300111130                                      )
100400111130       ;
100500111207
100600111207       EXEC SQL
100700111207         SET :piTestoSezione = REPLACE( :piTestoSezione
100800111207                                      , '&DATA_FIRMA_ACCETTAZIONE'
100900111207                                      , CHAR(:dataAccettazione, EUR)
101000111207                                      )
101100111207       ;
101200090318
101300090318       // Sottostringo.
101400090320
101500090324       outBuf = piTestoSezione;
101600090324       outBytesLeft = %LEN(piTestoSezione);
101700090318
101800090318       IF ISST_newStringa( outBuf
101900090318                         : outBytesLeft
102000090318                         ) < ESITO_OK;
102100090318         DUMP(A);
102200090318         RETURN ESITO_ERROR;
102300090318       ENDIF;
102400090318
102500090318       // Stampo le sottostrighe.
102600090318
102700090318       DOU posizioneProgressiva >= outBytesLeft;
102800111128         RunOverFlow();
102900090318         prt_testo = ISST_getSottoStringa( %SIZE(prt_testo)
103000090318                                         : posizioneProgressiva
103100090318                                         );
103200111128         IF posizioneProgressiva < outBytesLeft;
103300111128           txtInput = %TRIMR(prt_testo);
103400111128           txtInputLen = %SIZE(prt_testo);
103500111128           Giustifica( txtInput : txtInputLen : txtGiust : txtResto );
103600111128           prt_testo = txtGiust;
103700111128         ENDIF;
103800090319         WRITE riga;
103900090318       ENDDO;
104000090318
104100090318       RETURN ESITO_OK;
104200090318
104300090318      /END-FREE
104400090319     P RunRigaTesto    E
104500090318
104600090319
104700090319     P*--------------------------------------------------
104800090319     P* Procedure name: RunOverFlow
104900090319     P* Purpose:        Gestisce l'overflow.
105000090319     P* Returns:        Esito.
105100090319     P*--------------------------------------------------
105200090319     P RunOverFlow     B
105300090319     D RunOverFlow     PI            10I 0
105400120315      *
105500120315       // - Stampa O.R.M.
105600120315     d trul33r         pr                  extpgm('TRUL33R')
105700120315     d   kpjba                             likeds(KPJBA)
105800120315
105900120315     D trul33ds      E DS                  QUALIFIED
106000120315     D kpjba         E DS                  QUALIFIED
106100120322     D savkpjbu        s                   like(kpjba.kpjbu)
106200120315      *
106300120315
106400090319      /FREE
106500090319
106600090319       IF NOT overFlow;
106700090319         RETURN ESITO_OK;
106800090319       ENDIF;
106900090324
107000090324       IF copiaPerVettore;
107100130403       IF versionecontratto <> 'A' and versionecontratto <> 'B';
107200090324         WRITE firmaTxt;
107300130205         else;
107400130205         write firmaTxto;
107500130205         endif;
107600090324         WRITE firmaImg;
107700090324       ENDIF;
107800090324
107900120315         clear  PRT_numprogr;
108000120315       IF infPrtf.currentPage = 2 or infPrtf.currentPage = 4;
108100120315         exsr repNUM;
108200120315       ENDIF;
108300120315
108400090319       WRITE pagina;
108500090319       RESET overFlow;
108600090319
108700090422       IF infPrtf.currentPage = 3;
108800160412       // box bollo asteriscata la scrittura  WRITE dtCertaBox;
108900090422         WRITE dtCertaTxt;
109000090422       ENDIF;
109100090422
109200090319       RETURN ESITO_OK;
109300120315
109400120315       //***********************************************************************
109500120315       //
109600120315       // Estrae numero progressivo pagina
109700120315       //
109800120315       //***********************************************************************
109900120315          begsr repnum;
110000120315
110100120322           eval  savkpjbu = kpjba.kpjbu ;
110200120315           clear  PRT_numprogr;
110300120315           clear  trul33ds;
110400120315           eval  trul33ds.i33tla = ' ';
110500120315           eval  trul33ds.i33cnu = 80;
110600120315           eval  trul33ds.i33num = 1;
110700120315           eval  kpjba.kpjbu  = trul33ds;
110800120315
110900120315             TRUL33R(kpjba);
111000120315
111100120315           eval trul33ds = kpjba.kpjbu;
111200120315
111300120315           if  trul33ds.o33err <> *zeros;
111400120315               dump;
111500120315           else;
111600120315               eval  PRT_numPROGR = trul33ds.o33nrf;
111700120315           endif;
111800120315
111900120322           eval   kpjba.kpjbu = savkpjbu ;
112000120315          endsr;
112100120315
112200120315       //***********************************************************************
112300120315
112400090319      /END-FREE
112500090319     P RunOverFlow     E
112600090319
112700090324
112800090324     P*--------------------------------------------------
112900090324     P* Procedure name: Init
113000090324     P* Purpose:        Inizializza il modulo.
113100090324     P* Returns:        Esito.
113200090324     P*--------------------------------------------------
113300090324     P Init            B
113400090324     D Init            PI            10I 0
113500090324
113600090324      /FREE
113700090324
113800090324       // Inizializzo una conversione dati da ASCII a EBCDIC.
113900090324
114000111125       fromCode.QTQCCSID = CCSID_MSWIN_LATIN1;
114100090326       toCode.QTQCCSID = CCSID_ITALIAN;
114200090324       fromCode.QTQERVED02 = *ALLX'00';
114300090324       toCode.QTQERVED02 = *ALLX'00';
114400090324
114500090324       iconv_t = CodeConversionAllocation( %ADDR(toCode)
114600090324                                         : %ADDR(fromCode)
114700090324                                         );
114800090324
114900090324       // Inizializzo il modulo sottostringa intelligente.
115000090324
115100090324       IF ISST_init() < ESITO_OK;
115200090324         prmRpyOpCode = 'ERRORE';
115300090324         prmRpyIdMsg = ESITO_ERROR;
115400090324         RETURN ESITO_ERROR;
115500090324       ENDIF;
115600090324
115700090324       // Inizializzo il modulo anagrafico società.
115800090324
115900090324       IF Societa_Init() < ESITO_OK;
116000090324         prmRpyOpCode = 'ERRORE';
116100090324         prmRpyIdMsg = ESITO_ERROR;
116200090324         RETURN ESITO_ERROR;
116300090324       ENDIF;
116400090325
116500090324       // Posso finalmente aprire il printer file.
116600130205       // spostato dopo reperimento versione ed OVRPRTF eventuale
116700130205       // OPEN trmz0100p;
116800090324
116900090324       // Inizializzazione del programma completata.
117000090324
117100090324       initDone = *ON;
117200090324
117300090324       RETURN ESITO_OK;
117400090324
117500090324      /END-FREE
117600090324     P Init            E
117700090324
117800090324
117900090324     P*--------------------------------------------------
118000090324     P* Procedure name: GetTestoSezione
118100090324     P* Purpose:        Restituisce il testo di una sezione.
118200090324     P* Returns:        Testo della sezione.
118300090324     P* Parameter:      piNomeFile => Nome file che contiene il testo della...
118400090324     P*                           sezione.
118500090324     P*--------------------------------------------------
118600090324     PGetTestoSezione  B
118700090324     DGetTestoSezione  PI         32767A   VARYING
118800090324     D  piNomeFile                  255A   VARYING
118900090324     D                                     VALUE
119000090324
119100090324      /FREE
119200090324
119300111125       fd = IFS_OpenFile( cartellaTesti + '/' + piNomeFile
119400090324                        : O_RDONLY);
119500090324
119600090324       IF fd < 0;
119700090324         errnoPtr = getErrno();
119800090326         prmRpyIdMsg = errno * -1;
119900090326         prmRpyOpCode = 'ERRORE';
120000090324         DUMP(A);
120100090324         RETURN *BLANK;
120200090324       ENDIF;
120300090324
120400090324       RESET inBuf;
120500090324       RESET inBufPtr;
120600090324
120700090324       IFS_ReadFromDescriptor( fd : inBufPtr : %SIZE(inBuf) );
120800090324       IFS_CloseFile( fd );
120900090324
121000090324       // Conversione ASCII -> EBCDIC.
121100090324
121200090324       inBytesLeft = %LEN(%TRIMR(inBuf));
121300090324
121400090324       IF inBytesLeft = *ZERO;
121500090324         RETURN *BLANK;
121600090324       ENDIF;
121700090324
121800090324       outBytesLeft = inBytesLeft;
121900090324       CLEAR outBuf;
122000090324       RESET outBufPtr;
122100090324
122200090324       IF CodeConversion( iconv_t
122300090324                        : inBufPtr
122400090324                        : inBytesLeft
122500090324                        : outBufPtr
122600090324                        : outBytesLeft
122700090324                        ) < 0;
122800090324         errnoPtr = getErrno();
122900090326         prmRpyIdMsg = errno * -1;
123000090326         prmRpyOpCode = 'ERRORE';
123100090324         DUMP(A);
123200090324         RETURN *BLANK;
123300090324       ENDIF;
123400090324
123500090324       RETURN %TRIMR(outBuf);
123600090324
123700090324      /END-FREE
123800090324     PGetTestoSezione  E
123900090324
124000111124
124100111124     P*--------------------------------------------------
124200111124     P* Procedure name: GetVersioneContrattoByData
124300111124     P* Purpose:        Restituisce la versione del contratto in base alla ...
124400111124     P*                          data.
124500111124     P* Returns:        Versione.
124600111124     P* Parameter:      priData => Data per calcolo versione.
124700111124     P*--------------------------------------------------
124800111124     P GetVersioneContrattoByData...
124900111124     P                 B
125000111124     D GetVersioneContrattoByData...
125100111124     D                 PI             1A
125200111124     D  priData                        D   CONST
125300111124
125400111124     D retField        S              1A   STATIC
125500111124
125600111124      /FREE
125700111124
125800111124       CLEAR retField;
125900111124
126000111124       EXEC SQL
126100111124         SELECT LEFT(tbeUni, 1)
126200111124           INTO :retField
126300111124           FROM tntbe00f
126400111124           WHERE tbeCod = '007'
126500111124             AND tbeKe1 = '*VERSIONE'
126600111124             AND tbeKe2 <= :priData
126700111124             AND tbeLin = ''
126800111124             AND tbeSif = ''
126900111124             AND tbeAtb = ''
127000111124           ORDER BY tbeCod, tbeKe1, tbeKe2 DESC
127100111124           FETCH FIRST 1 ROW ONLY
127200111124       ;
127300111124
127400111124       SELECT;
127500111124         WHEN sqlCode < *ZERO;
127600111124           DUMP(A);
127700111124         WHEN sqlCode = 100;
127800111124           DUMP(A);
127900111124       ENDSL;
128000111124
128100111124       RETURN retField;
128200111124
128300111124      /END-FREE
128400111124     P GetVersioneContrattoByData...
128500111124     P                 E
128600111124
128700111125
128800111125     P*--------------------------------------------------
128900111125     P* Procedure name: GetCartellaTestiByVersioneContratto
129000111125     P* Purpose:        Restituisce la cartella dei testi della versione di...
129100111125     P*                           contratto.
129200111125     P* Returns:        Cartella testi.
129300111125     P* Parameter:      priVersioneContratto => Versione contratto
129400111125     P*--------------------------------------------------
129500111125     P GetCartellaTestiByVersioneContratto...
129600111125     P                 B
129700111125     D GetCartellaTestiByVersioneContratto...
129800111125     D                 PI           255A   VARYING
129900111125     D  priVersioneContratto...
130000111125     D                                1A   CONST
130100111125
130200111125     D retField        S            255A   VARYING STATIC
130300111125
130400111125      /FREE
130500111125
130600111125       CLEAR retField;
130700111125
130800111125       EXEC SQL
130900111125         SELECT RTRIM(tbeUni)
131000111125           INTO :retField
131100111125           FROM tntbe00f
131200111125           WHERE tbeCod = '007'
131300111125             AND tbeKe1 = '*IFSDIRTXT'
131400111125             AND tbeKe2 = :priVersioneContratto
131500111125             AND tbeLin = ''
131600111125             AND tbeSif = ''
131700111125             AND tbeAtb = ''
131800111125       ;
131900111125
132000111125       SELECT;
132100111125         WHEN sqlCode < *ZERO;
132200111125           DUMP(A);
132300111125         WHEN sqlCode = 100;
132400111125           DUMP(A);
132500111125       ENDSL;
132600111125
132700111125       RETURN retField;
132800111125
132900111125      /END-FREE
133000111125     P GetCartellaTestiByVersioneContratto...
133100111125     P                 E
133200111125
133300130205     P*--------------------------------------------------
133400130205     P* Procedure name: GetOvrPrtfByVersioneContratto
133500130205     P* Purpose:        Restituisce eventuale ovrprtf se presente
133600130205     P* Returns:        OVERRIDE
133700130205     P* Parameter:      priVersioneContratto => Versione contratto
133800130205     P*--------------------------------------------------
133900130205     P GetOvrPrtfByVersioneContratto...
134000130205     P                 B
134100130205     D GetOvrPrtfByVersioneContratto...
134200130205     D                 PI           256A
134300130205     D  VersioneContratto...
134400130205     D                                1A   CONST
134500130205
134600130205     D retField        S            256A   STATIC
134700130205
134800130205      /FREE
134900130205
135000130205       CLEAR retField;
135100130205
135200130205       EXEC SQL
135300130205         SELECT RTRIM(tbeUni)
135400130205           INTO :retField
135500130205           FROM tntbe00f
135600130205           WHERE tbeCod = '007'
135700130205             AND tbeKe1 = '*OVRPRTF'
135800130205             AND tbeKe2 = :VersioneContratto
135900130205             AND tbeLin = ''
136000130205             AND tbeSif = ''
136100130205             AND tbeAtb = ''
136200130205       ;
136300130205
136400130205       SELECT;
136500130205         WHEN sqlCode < *ZERO;
136600130205           DUMP(A);
136700130205         WHEN sqlCode = 100;
136800130205           DUMP(A);
136900130205       ENDSL;
137000130205
137100130205       RETURN retField;
137200130205
137300130205      /END-FREE
137400130205     P GetOvrPrtfByVersioneContratto...
137500130205     P                 E
137600130205
