000100140917      * ---------------------------------------------------------------------
000200140917      * CRTRPGMOD MODULE(GAITRAOBJ/UBTL05R02)
000300140917      * SRCFILE(GAITRASRC/SRCUB) SRCMBR(UBTL05R02)
000400140917      * TRUNCNBR(*NO) DBGVIEW(*LIST)
000500140917
000600140917      * CRTPGM PGM(GAITRAOBJ/UBTL05R02)
000700140917      * MODULE(GAITRAOBJ/UBTL05R02)
000800140917      * BNDSRVPGM(GAITRAOBJ/UBTL05R) ACTGRP(*NEW) BNDDIR(QC2LE)
000900140917      * TEXT('Web Service: NormalizeOne() - DRIVER TELLUS -')
001000140917
001100000000      * =====================================================================
001200000000     H DEBUG
001300000000     H OPTION(*SRCSTMT : *NODEBUGIO)
001400000000     H EXTBININT(*YES)
001500140917      *
001600140917      *
001700140917      *  Inclusioni esterne
001800140917      *
001900140917      /DEFINE PROTOTYPE_UBTL05R01
002000140917 CPY  /COPY GAITRASRC/SRCUB,UBTL05R01
002100140917      /UNDEFINE PROTOTYPE_UBTL05R01
002200140917
002300140917
002400140917      * =====================================================================
002500140917      *  Program entry point - PROTOTIPO
002600140917      * =====================================================================
002700140917     D UBTL05R02...
002800140917     D                 PR
002900140917     D   pInNazione                   3A
003000140917     D   pInProvincia                 4A
003100140917     D   pInComune                  128A
003200140917     D   pInLocalita                128A
003300140917     D   pInCAP                       9A
003400140917     D   pInVia                     128A
003500140917     D   pInCivico                   10A
003600140917     D   pOutEsatto                  10A
003700140917     D   pOutRank                    10A
003800140917     D   pOutLivNorm                 10A
003900140917     D   pOutScore                   10A
004000140917     D   pOutRegione                128A
004100140917     D   pOutProv                     4A
004200140917     D   pOutCAP                      9A
004300140917     D   pOutComune                 128A
004400140917     D   pOutLocalit                128A
004500140917     D   pOutVia                    128A
004600140917     D   pOutCivico                  10A
004700140917     D   pOutXY_X                    32A
004800140917     D   pOutXY_Y                    32A
004900140917     D   pOutEsito                    1A
005000140917     D   pOutEsitoDes               128A
005100140917
005200140917
005300140917      * =====================================================================
005400140917      *  Program entry point - PROCEDURE INTERFACE
005500140917      * =====================================================================
005600140917     D UBTL05R02...
005700140917     D                 PI
005800140917     D   pInNazione                   3A
005900140917     D   pInProvincia                 4A
006000140917     D   pInComune                  128A
006100140917     D   pInLocalita                128A
006200140917     D   pInCAP                       9A
006300140917     D   pInVia                     128A
006400140917     D   pInCivico                   10A
006500140917     D   pOutEsatto                  10A
006600140917     D   pOutRank                    10A
006700140917     D   pOutLivNorm                 10A
006800140917     D   pOutScore                   10A
006900140917     D   pOutRegione                128A
007000140917     D   pOutProv                     4A
007100140917     D   pOutCAP                      9A
007200140917     D   pOutComune                 128A
007300140917     D   pOutLocalit                128A
007400140917     D   pOutVia                    128A
007500140917     D   pOutCivico                  10A
007600140917     D   pOutXY_X                    32A
007700140917     D   pOutXY_Y                    32A
007800140917     D   pOutEsito                    1A
007900140917     D   pOutEsitoDes               128A
008000000000      *
008100000000      *  Web service error message text
008200000000     D errText         S                   like(wsdl_errText_t ) inz
008300000000      *
008400000000      * Request SOAP headers and message
008500000000      * These fields needs to be filled with reasonable values
008600000000      * right before calling the web service.
008700000000     D parameters      DS                  likeds(tns_NormalizeOne_t)
008800000000     D                                     inz
008900000000      * Response SOAP headers and message
009000000000      * This structure contains the result values returned by
009100000000      * the web service.
009200000000     D NormalizeOneResponse...
009300000000     D                 DS                  likeds(tns_NormalizeOneResponse_t)
009400000000     D                                     inz
009500000000      *
009600000000      *  Text of messages sent to the command line
009700000000     D text            S            128A   varying inz
009800000000      *
009900000000      *  Memory Manager UUID, used to allocate and free
010000000000      *  the memory blocks of the dynamic arrays
010100000000     D uuid            S                   like(wsdl_uuid_t ) inz
010200000000      *
010300000000      *  Dynamic array index fields
010400000000      *
010500000000      *  Dynamic array item fields
010600000000      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
010700140917
010800140917      *
010900140917      * Variabili di wrk
011000140917      *
011100140917     D wInNazione      S                   like(pInNazione)    inz
011200140917     D wXMLstrIn       S           2048A   varying             inz
011300140917     D wXMLstrOut      S           2048A   varying             inz
011400140917     D wStartPos       S              4S 0                     inz
011500140917     D wFindTag        S            512A   varying             inz
011600140917     D wTagPos         S              4S 0                     inz
011700140917     D wTagLen         S              4S 0                     inz
011800140917
011900000000      /free
012000140917
012100140917       // inizializzazione parametri di output
012200140917       clear pOutEsatto;
012300140917       clear pOutRank;
012400140917       clear pOutLivNorm;
012500140917       clear pOutScore;
012600140917       clear pOutRegione;
012700140917       clear pOutProv;
012800140917       clear pOutCAP;
012900140917       clear pOutComune;
013000140917       clear pOutLocalit;
013100140917       clear pOutVia;
013200140917       clear pOutCivico;
013300140917       clear pOutXY_X;
013400140917       clear pOutXY_Y;
013500140917       clear pOutEsito;
013600140917       clear pOutEsitoDes;
013700140917       clear wXMLstrOut;
013800140917
013900140917       // Inizializzo esito a Errore
014000140917       pOutEsito = 'E';
014100140917
014200140917
014300140917       // Se lancio non "a vuoto" => procedo
014400140917       if pInNazione    <> *blanks OR
014500140917          pInProvincia  <> *blanks OR
014600140917          pInComune     <> *blanks OR
014700140917          pInLocalita   <> *blanks OR
014800140917          pInCAP        <> *blanks OR
014900140917          pInVia        <> *blanks OR
015000140917          pInCivico     <> *blanks;
015100140917
015200140917          // Se nazione a *blanks => si intende di default Italia
015300140917          wInNazione = pInNazione;
015400140917          if wInNazione = *blanks;
015500140917             wInNazione = 'it';
015600140917          endif;
015700140917
015800140917
015900140917          // Inizializzo la stringa di input da passare al WS
016000140917          // wXMLstrIn = '<tem:Xml><![CDATA[<Data><Param type="0" /> ' +
016100140917          // wXMLstrIn = '<![CDATA[<Data><Param type="0" /> ' +
016200140917          wXMLstrIn = '<Data><Param type="0" /> ' +
016300140917                 '<AuthenticationData username="brt" password="xxx$$$" /> '+
016400140917                 '<A id="1" pr="§prv§" comune="§comune§" loc="§localita§" '+
016500140917                 'pv="" via="§via§" n="§civico§" cap="§cap§" nType="C" '+
016600140917                 'naz="§nazione§" /></Data>';
016700140917          //     'naz="§nazione§" /></Data>]]></tem:Xml>';
016800140917          //     'naz="§nazione§" /></Data>]]>';
016900140917
017000140917          // Personalizzo con gli input ricevuti in input
017100140917          wXMLstrIn = %scanrpl('§nazione§'  : %trim(wInNazione)   : wXMLstrIn);
017200140917          wXMLstrIn = %scanrpl('§prv§'      : %trim(pInProvincia) : wXMLstrIn);
017300140917          wXMLstrIn = %scanrpl('§comune§'   : %trim(pInComune)    : wXMLstrIn);
017400140917          wXMLstrIn = %scanrpl('§localita§' : %trim(pInLocalita)  : wXMLstrIn);
017500140917          wXMLstrIn = %scanrpl('§cap§'      : %trim(pInCAP)       : wXMLstrIn);
017600140917          wXMLstrIn = %scanrpl('§via§'      : %trim(pInVia)       : wXMLstrIn);
017700140917          wXMLstrIn = %scanrpl('§civico§'   : %trim(pInCivico)    : wXMLstrIn);
017800140917
017900000000
018000000000         // Enable/disable http api debug log
018100000000         // (Enable the http api debug log in case of problems with the web service)
018200000000         // TLAddressFinderWSClassSoap_Port_setHttpDebug(
018300000000         //    *ON: '/tmp/httpapi_debug.txt');   // Default path of http api
018400000000
018500000000         // Specify a proxy server if your network requires it.
018600000000         // TLAddressFinderWSClassSoap_Port_setHttpProxy(
018700000000         //    'your.proxy.server': 8080);
018800000000
018900000000         // Initialize the Memory Manager
019000000000         // (Use 'uuid' to create dynamic arrays within the same heap.)
019100000000         uuid =
019200000000            TLAddressFinderWSClassSoap_initializeMemoryManager();
019300000000
019400000000         // Set the parameter values of the request message
019500000000         // of the web service.
019600000000         //    assign values here
019700140917         // parameters.Xml = 'string';
019800140917         parameters.Xml = %trim(wXMLstrIn);
019900000000
020000000000         // Now let's call the web service.
020100000000         NormalizeOneResponse =
020200000000            TLAddressFinderWSClassSoap_NormalizeOne(
020300000000                parameters:
020400000000                errText);
020500140917
020600140917         // Intercetto l'esito del web service
020700140917         pOutEsitoDes = %trim(errText);
020800000000
020900000000         // If the Web service finished successfully ...
021000140917         if (not TLAddressFinderWSClassSoap_Port_isError());
021100140917
021200140917            // Se tutto ok => reperisco i dati di output del web service
021300140917            wXMLstrOut = %trim(NormalizeOneResponse.NormalizeOneResult);
021400140917
021500140917            // Quindi estraggo i valori per i partametri di output
021600140917            wFindTag  = ' Esatto="';
021700140917            wStartPos = %scan(wFindTag:wXMLstrOut);
021800140917            if wStartPos > *zeros;
021900140917               wTagPos = wStartPos+%len(wFindTag);
022000140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
022100140917               pOutEsatto  = %subst(wXMLstrOut:wTagPos:wTagLen);
022200140917            endif;
022300140917
022400140917            wFindTag  = ' Rank="';
022500140917            wStartPos = %scan(wFindTag:wXMLstrOut);
022600140917            if wStartPos > *zeros;
022700140917               wTagPos = wStartPos+%len(wFindTag);
022800140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
022900140917               pOutRank    = %subst(wXMLstrOut:wTagPos:wTagLen);
023000140917            endif;
023100140917
023200140917            wFindTag  = ' nl="';
023300140917            wStartPos = %scan(wFindTag:wXMLstrOut);
023400140917            if wStartPos > *zeros;
023500140917               wTagPos = wStartPos+%len(wFindTag);
023600140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
023700140917               pOutLivNorm = %subst(wXMLstrOut:wTagPos:wTagLen);
023800140917            endif;
023900140917
024000140917            wFindTag  = ' s="';
024100140917            wStartPos = %scan(wFindTag:wXMLstrOut);
024200140917            if wStartPos > *zeros;
024300140917               wTagPos = wStartPos+%len(wFindTag);
024400140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
024500140917               pOutScore   = %subst(wXMLstrOut:wTagPos:wTagLen);
024600140917            endif;
024700140917
024800140917            wFindTag  = ' reg="';
024900140917            wStartPos = %scan(wFindTag:wXMLstrOut);
025000140917            if wStartPos > *zeros;
025100140917               wTagPos = wStartPos+%len(wFindTag);
025200140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
025300140917               pOutRegione = %subst(wXMLstrOut:wTagPos:wTagLen);
025400140917            endif;
025500140917
025600140917            wFindTag  = ' spr="';
025700140917            wStartPos = %scan(wFindTag:wXMLstrOut);
025800140917            if wStartPos > *zeros;
025900140917               wTagPos = wStartPos+%len(wFindTag);
026000140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
026100140917               pOutProv    = %subst(wXMLstrOut:wTagPos:wTagLen);
026200140917            endif;
026300140917
026400140917            wFindTag  = ' cap="';
026500140917            wStartPos = %scan(wFindTag:wXMLstrOut);
026600140917            if wStartPos > *zeros;
026700140917               wTagPos = wStartPos+%len(wFindTag);
026800140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
026900140917               pOutCAP     = %subst(wXMLstrOut:wTagPos:wTagLen);
027000140917            endif;
027100140917
027200140917            wFindTag  = ' comune="';
027300140917            wStartPos = %scan(wFindTag:wXMLstrOut);
027400140917            if wStartPos > *zeros;
027500140917               wTagPos = wStartPos+%len(wFindTag);
027600140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
027700140917               pOutComune  = %subst(wXMLstrOut:wTagPos:wTagLen);
027800140917            endif;
027900140917
028000140917            wFindTag  = ' loc="';
028100140917            wStartPos = %scan(wFindTag:wXMLstrOut);
028200140917            if wStartPos > *zeros;
028300140917               wTagPos = wStartPos+%len(wFindTag);
028400140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
028500140917               pOutLocalit = %subst(wXMLstrOut:wTagPos:wTagLen);
028600140917            endif;
028700140917
028800140917            wFindTag  = ' via="';
028900140917            wStartPos = %scan(wFindTag:wXMLstrOut);
029000140917            if wStartPos > *zeros;
029100140917               wTagPos = wStartPos+%len(wFindTag);
029200140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
029300140917               pOutVia     = %subst(wXMLstrOut:wTagPos:wTagLen);
029400140917            endif;
029500140917
029600140917            wFindTag  = ' n="';
029700140917            wStartPos = %scan(wFindTag:wXMLstrOut);
029800140917            if wStartPos > *zeros;
029900140917               wTagPos = wStartPos+%len(wFindTag);
030000140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
030100140917               pOutCivico  = %subst(wXMLstrOut:wTagPos:wTagLen);
030200140917            endif;
030300140917
030400140917            wFindTag  = ' x="';
030500140917            wStartPos = %scan(wFindTag:wXMLstrOut);
030600140917            if wStartPos > *zeros;
030700140917               wTagPos = wStartPos+%len(wFindTag);
030800140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
030900140917               pOutXY_X    = %subst(wXMLstrOut:wTagPos:wTagLen);
031000140917            endif;
031100140917
031200140917            wFindTag  = ' y="';
031300140917            wStartPos = %scan(wFindTag:wXMLstrOut);
031400140917            if wStartPos > *zeros;
031500140917               wTagPos = wStartPos+%len(wFindTag);
031600140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
031700140917               pOutXY_Y    = %subst(wXMLstrOut:wTagPos:wTagLen);
031800140917            endif;
031900140917
032000140917            // A fine chiamata imposto esito a OK
032100140917            pOutEsito = '0';
032200000000
032300140917
032400140917         else;
032500000000            // ... else show error messages ordered by priority:
032600000000            // 1. HTTP error information
032700140917            pOutEsitoDes = %char(
032800000000                      TLAddressFinderWSClassSoap_HttpError_getCode()
032900000000                      ) + ': ' +
033000000000                   TLAddressFinderWSClassSoap_HttpError_getText()
033100000000                   ;
033200140917
033300000000            // 2. SOAP error information
033400140917            if (TLAddressFinderWSClassSoap_SoapError_getCode()
033500000000                  <> '');
033600000000               text = TLAddressFinderWSClassSoap_SoapError_getCode()
033700000000                      + ': ' +
033800000000                      TLAddressFinderWSClassSoap_SoapError_getText()
033900000000                      ;
034000140917            endif;
034100140917
034200000000            // 3. XML parser error information
034300140917            if (TLAddressFinderWSClassSoap_XmlError_getCode()
034400000000                  <> 0);
034500000000               text = %char(TLAddressFinderWSClassSoap_XmlError_getCode())
034600000000                      + ': ' +
034700000000                      TLAddressFinderWSClassSoap_XmlError_getText()
034800000000                      ;
034900140917            endif;
035000140917         endif;
035100140917
035200140917
035300140917       else;
035400140917          pOutEsitoDes = 'Parametri di input vuoti o insufficienti';
035500140917       endif;
035600140917
035700000000
035800140917       // Terminate Memory Manager and free resources
035900140917       // (Release all memory blocks that have been allocated within
036000140917       //  the heap that is assigned to 'uuid'.)
036100140917       TLAddressFinderWSClassSoap_terminateMemoryManager(uuid);
036200000000
036300000000
036400140917       *inlr = *on;
036500000000
036600000000      /end-free
036700140917
036800140917      *
036900140917      *
