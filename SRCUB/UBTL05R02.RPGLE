000100140917      * ---------------------------------------------------------------------
000200140917      * CRTRPGMOD MODULE(GAITRAOBJ/UBTL05R02)
000300140917      * SRCFILE(GAITRASRC/SRCUB) SRCMBR(UBTL05R02)
000400140917      * TRUNCNBR(*NO) DBGVIEW(*LIST)
000500140917
000600140917      * CRTPGM PGM(GAITRAOBJ/UBTL05R02) MODULE(GAITRAOBJ/UBTL05R02)
000700140917      * ACTGRP(*NEW) BNDDIR(QC2LE) BNDSRVPGM(GAITRAOBJ/UBTL05R
000800140917      * *LIBL/BASICS1 *LIBL/WSDL2RPGRT)
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
003600140918     D   pInScore                    10A
003700140917     D   pOutEsito                    1A
003800140917     D   pOutEsitoDes               128A
003900140917     D   pOutTotFound                 4S 0
004000140917     D   pOutResults                       dim(10) likeds(dsOutUBTL05R02)       10 * 630 della DS
004100140917
004200140917
004300140917      * =====================================================================
004400140917      *  Program entry point - PROCEDURE INTERFACE
004500140917      * =====================================================================
004600140917     D UBTL05R02...
004700140917     D                 PI
004800140917     D   pInNazione                   3A
004900140917     D   pInProvincia                 4A
005000140917     D   pInComune                  128A
005100140917     D   pInLocalita                128A
005200140917     D   pInCAP                       9A
005300140917     D   pInVia                     128A
005400140917     D   pInCivico                   10A
005500140918     D   pInScore                    10A
005600140917     D   pOutEsito                    1A
005700140917     D   pOutEsitoDes               128A
005800140917     D   pOutTotFound                 4S 0
005900140917     D   pOutResults                       dim(10) likeds(dsOutUBTL05R02)       10 * 630 della DS
006000000000      *
006100000000      *  Web service error message text
006200000000     D errText         S                   like(wsdl_errText_t ) inz
006300000000      *
006400000000      * Request SOAP headers and message
006500000000      * These fields needs to be filled with reasonable values
006600000000      * right before calling the web service.
006700000000     D parameters      DS                  likeds(tns_NormalizeOne_t)
006800000000     D                                     inz
006900000000      * Response SOAP headers and message
007000000000      * This structure contains the result values returned by
007100000000      * the web service.
007200000000     D NormalizeOneResponse...
007300000000     D                 DS                  likeds(tns_NormalizeOneResponse_t)
007400000000     D                                     inz
007500000000      *
007600000000      *  Text of messages sent to the command line
007700000000     D text            S            128A   varying inz
007800000000      *
007900000000      *  Memory Manager UUID, used to allocate and free
008000000000      *  the memory blocks of the dynamic arrays
008100000000     D uuid            S                   like(wsdl_uuid_t ) inz
008200000000      *
008300000000      *  Dynamic array index fields
008400000000      *
008500000000      *  Dynamic array item fields
008600000000      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
008700140917
008800140917      *
008900140917      * Variabili di wrk
009000140917      *
009100140917 CPY  /COPY GAITRASRC/SRCUB,UBTL05D02
009200140917      *
009300140917     D wDsOut          DS                  dim(10) likeds(dsOutUBTL05R02) inz
009400140917     D wInNazione      S                   like(pInNazione)    inz
009500140917     D wXMLstrIn       S           8192A   varying             inz
009600140917     D wXMLstrOut      S           8192A   varying             inz
009700140917     D wStartPos       S              4S 0                     inz
009800140917     D wFindTag        S            512A   varying             inz
009900140917     D wTagPos         S              4S 0                     inz
010000140917     D wTagLen         S              4S 0                     inz
010100140917
010200140917
010300140917     D*------------------
010400140917     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
010500140917     D*------------------
010600140917     D InStringa       S          65535A   VARYING                              (stringa input)
010700140917     D InSepara        S             10A                                        (separatore)
010800140917     D InTypeOut       S              1A                                        (tipo output)
010900140917     D wSkParam        S          65535A                                        (output)
011000140917     D OutErrore       S              1A                                        (flag errore)
011100140917     D OutMsg          S             80A                                        (messaggio errore)
011200140917     D SkSplitCSV_8    S           2048    DIM(32)
011300140917
011400140917
011500140917
011600000000      /free
011700140917
011800140917       // inizializzazione parametri di output
011900140917       clear wDsOut;
012000140917       clear pOutEsito;
012100140917       clear pOutEsitoDes;
012200140917       clear pOutTotFound;
012300140917       clear pOutResults;
012400140917       clear wXMLstrOut;
012500140917
012600140917
012700140917       // Inizializzo esito a Errore
012800140917       pOutEsito = 'E';
012900140917
013000140917
013100140917       // Se lancio non "a vuoto" => procedo
013200140917       if pInNazione    <> *blanks OR
013300140917          pInProvincia  <> *blanks OR
013400140917          pInComune     <> *blanks OR
013500140917          pInLocalita   <> *blanks OR
013600140917          pInCAP        <> *blanks OR
013700140917          pInVia        <> *blanks OR
013800140917          pInCivico     <> *blanks;
013900140918
014000140918          // Se non indicato imposto default per score di normalizzazione
014100140918          if pInScore = *blanks;
014200140918             pInScore = '9499';
014300140918          endif;
014400140917
014500140918
014600140917          // Se nazione a *blanks => si intende di default Italia
014700140917          wInNazione = pInNazione;
014800140917          if wInNazione = *blanks;
014900140917             wInNazione = 'it';
015000140917          endif;
015100140917
015200140917
015300140917          // Inizializzo la stringa di input da passare al WS
015400140918          // wXMLstrIn = '<![CDATA[<Data><Param type="0" /> ' +
015500140917          wXMLstrIn = '<Data><Param type="0" /> ' +
015600140917                 '<AuthenticationData username="brt" password="xxx$$$" /> '+
015700140917                 '<A id="1" pr="§prv§" comune="§comune§" loc="§localita§" '+
015800140917                 'pv="" via="§via§" n="§civico§" cap="§cap§" nType="C" '+
015900140917                 'naz="§nazione§" /></Data>';
016000140918          //     'naz="§nazione§" /></Data>]]>';
016100140917
016200140917          // Personalizzo con gli input ricevuti in input
016300140917          wXMLstrIn = %scanrpl('§nazione§'  : %trim(wInNazione)   : wXMLstrIn);
016400140917          wXMLstrIn = %scanrpl('§prv§'      : %trim(pInProvincia) : wXMLstrIn);
016500140917          wXMLstrIn = %scanrpl('§comune§'   : %trim(pInComune)    : wXMLstrIn);
016600140917          wXMLstrIn = %scanrpl('§localita§' : %trim(pInLocalita)  : wXMLstrIn);
016700140917          wXMLstrIn = %scanrpl('§cap§'      : %trim(pInCAP)       : wXMLstrIn);
016800140917          wXMLstrIn = %scanrpl('§via§'      : %trim(pInVia)       : wXMLstrIn);
016900140917          wXMLstrIn = %scanrpl('§civico§'   : %trim(pInCivico)    : wXMLstrIn);
017000140917
017100000000
017200000000         // Enable/disable http api debug log
017300000000         // (Enable the http api debug log in case of problems with the web service)
017400000000         // TLAddressFinderWSClassSoap_Port_setHttpDebug(
017500000000         //    *ON: '/tmp/httpapi_debug.txt');   // Default path of http api
017600000000
017700000000         // Specify a proxy server if your network requires it.
017800000000         // TLAddressFinderWSClassSoap_Port_setHttpProxy(
017900000000         //    'your.proxy.server': 8080);
018000000000
018100000000         // Initialize the Memory Manager
018200000000         // (Use 'uuid' to create dynamic arrays within the same heap.)
018300000000         uuid =
018400000000            TLAddressFinderWSClassSoap_initializeMemoryManager();
018500000000
018600000000         // Set the parameter values of the request message
018700000000         // of the web service.
018800000000         //    assign values here
018900140917         // parameters.Xml = 'string';
019000140917         parameters.Xml = %trim(wXMLstrIn);
019100000000
019200000000         // Now let's call the web service.
019300000000         NormalizeOneResponse =
019400000000            TLAddressFinderWSClassSoap_NormalizeOne(
019500000000                parameters:
019600000000                errText);
019700140917
019800140917         // Intercetto l'esito del web service
019900140917         pOutEsitoDes = %trim(errText);
020000000000
020100000000         // If the Web service finished successfully ...
020200140917         if (not TLAddressFinderWSClassSoap_Port_isError());
020300140917
020400140917            // Se tutto ok => reperisco i dati di output del web service
020500140917            wXMLstrOut = %trim(NormalizeOneResponse.NormalizeOneResult);
020600140917
020700140917            // Verifico se tornato almeno un risultato valido
020800140917            if %scan(' Esatto="':wXMLstrOut) > *zeros;
020900140917
021000140917               exsr sr_Splitta;
021100140918               if i = *zeros;
021200140917               else;
021300140917                  // Imposto numero risultati ritornati da TELLUS
021400140918                  pOutTotFound = i;
021500140917
021600140917                  // Imposto l'output
021700140917                  pOutResults = wDsOut;
021800140917
021900140917                  // Imposto esito a OK
022000140917                  pOutEsito = '0';
022100140917               endif;
022200140917            endif;
022300000000
022400140917
022500140917         else;
022600000000            // ... else show error messages ordered by priority:
022700000000            // 1. HTTP error information
022800140917            pOutEsitoDes = %char(
022900000000                      TLAddressFinderWSClassSoap_HttpError_getCode()
023000000000                      ) + ': ' +
023100000000                   TLAddressFinderWSClassSoap_HttpError_getText()
023200000000                   ;
023300140917
023400000000            // 2. SOAP error information
023500140917            if (TLAddressFinderWSClassSoap_SoapError_getCode()
023600000000                  <> '');
023700000000               text = TLAddressFinderWSClassSoap_SoapError_getCode()
023800000000                      + ': ' +
023900000000                      TLAddressFinderWSClassSoap_SoapError_getText()
024000000000                      ;
024100140917            endif;
024200140917
024300000000            // 3. XML parser error information
024400140917            if (TLAddressFinderWSClassSoap_XmlError_getCode()
024500000000                  <> 0);
024600000000               text = %char(TLAddressFinderWSClassSoap_XmlError_getCode())
024700000000                      + ': ' +
024800000000                      TLAddressFinderWSClassSoap_XmlError_getText()
024900000000                      ;
025000140917            endif;
025100140917         endif;
025200140917
025300140917
025400140917       else;
025500140917          pOutEsitoDes = 'Parametri di input vuoti o insufficienti';
025600140917       endif;
025700140917
025800000000
025900140917       // Terminate Memory Manager and free resources
026000140917       // (Release all memory blocks that have been allocated within
026100140917       //  the heap that is assigned to 'uuid'.)
026200140917       TLAddressFinderWSClassSoap_terminateMemoryManager(uuid);
026300000000
026400000000
026500140917       *inlr = *on;
026600000000
026700000000      /end-free
026800140917
026900140917
027000140917     C*----------------------------------------------------*
027100140917     C     sr_Splitta    BEGSR
027200140917     C*
027300140917     C                   eval      InStringa = %trim(wXMLstrOut)
027400140917     C*
027500140917     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
027600140917     C                   CALL      'XSPLIT2'
027700140917     C                   PARM                    InStringa
027800140917     C                   PARM      '><R'         InSepara
027900140917     C                   PARM      '8'           InTypeOut
028000140917     C                   PARM      ''            wSkParam
028100140917     C                   PARM                    OutErrore
028200140917     C                   PARM                    OutMsg
028300140917     C                   MOVEA     wSkParam      SkSplitCSV_8
028400140917     C*
028500140917     C* Valorizzo la schiera delle intestazioni colonne (elementi)
028600140918     C                   z-add     *zeros        i                 4 0
028700140917     C                   z-add     2             x                 4 0
028800140917     C                   dow       x <= %elem(SkSplitCSV_8)
028900140917     C                   if        SkSplitCSV_8(x) = *blanks
029000140917     C                   leave
029100140917     C                   else
029200140917     C                   eval      wXMLstrOut = ' '+SkSplitCSV_8(x)
029300140917     C                   exsr      sr_ValOut
029400140917     C                   endif
029500140917     C                   add       1             x
029600140917     C                   enddo
029700140917     C*
029800140917     C                   ENDSR
029900140917
030000140917
030100140917     C*----------------------------------------------------*
030200140917     C     sr_ValOut     BEGSR
030300140917     C*
030400140917      /free
030500140917
030600140917            // Quindi estraggo i valori per i partametri di output
030700140917            wFindTag  = ' Esatto="';
030800140917            wStartPos = %scan(wFindTag:wXMLstrOut);
030900140917            if wStartPos > *zeros;
031000140918               i = i + 1;
031100140917               wTagPos = wStartPos+%len(wFindTag);
031200140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
031300140918               wDsOut(i).Esatto  = %subst(wXMLstrOut:wTagPos:wTagLen);
031400140917
031500140917            wFindTag  = ' Rank="';
031600140917            wStartPos = %scan(wFindTag:wXMLstrOut);
031700140917            if wStartPos > *zeros;
031800140917               wTagPos = wStartPos+%len(wFindTag);
031900140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
032000140918               wDsOut(i).Rank    = %subst(wXMLstrOut:wTagPos:wTagLen);
032100140917            endif;
032200140917
032300140917            wFindTag  = ' nl="';
032400140917            wStartPos = %scan(wFindTag:wXMLstrOut);
032500140917            if wStartPos > *zeros;
032600140917               wTagPos = wStartPos+%len(wFindTag);
032700140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
032800140918               wDsOut(i).LivNorm = %subst(wXMLstrOut:wTagPos:wTagLen);
032900140917            endif;
033000140917
033100140917            wFindTag  = ' s="';
033200140917            wStartPos = %scan(wFindTag:wXMLstrOut);
033300140917            if wStartPos > *zeros;
033400140917               wTagPos = wStartPos+%len(wFindTag);
033500140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
033600140918               wDsOut(i).Score   = %subst(wXMLstrOut:wTagPos:wTagLen);
033700140917            endif;
033800140917
033900140917            wFindTag  = ' reg="';
034000140917            wStartPos = %scan(wFindTag:wXMLstrOut);
034100140917            if wStartPos > *zeros;
034200140917               wTagPos = wStartPos+%len(wFindTag);
034300140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
034400140918               wDsOut(i).Regione = %subst(wXMLstrOut:wTagPos:wTagLen);
034500140917            endif;
034600140917
034700140917            wFindTag  = ' spr="';
034800140917            wStartPos = %scan(wFindTag:wXMLstrOut);
034900140917            if wStartPos > *zeros;
035000140917               wTagPos = wStartPos+%len(wFindTag);
035100140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
035200140918               wDsOut(i).Prov    = %subst(wXMLstrOut:wTagPos:wTagLen);
035300140917            endif;
035400140917
035500140917            wFindTag  = ' cap="';
035600140917            wStartPos = %scan(wFindTag:wXMLstrOut);
035700140917            if wStartPos > *zeros;
035800140917               wTagPos = wStartPos+%len(wFindTag);
035900140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
036000140918               wDsOut(i).CAP     = %subst(wXMLstrOut:wTagPos:wTagLen);
036100140917            endif;
036200140917
036300140917            wFindTag  = ' comune="';
036400140917            wStartPos = %scan(wFindTag:wXMLstrOut);
036500140917            if wStartPos > *zeros;
036600140917               wTagPos = wStartPos+%len(wFindTag);
036700140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
036800140918               wDsOut(i).Comune  = %subst(wXMLstrOut:wTagPos:wTagLen);
036900140917            endif;
037000140917
037100140917            wFindTag  = ' loc="';
037200140917            wStartPos = %scan(wFindTag:wXMLstrOut);
037300140917            if wStartPos > *zeros;
037400140917               wTagPos = wStartPos+%len(wFindTag);
037500140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
037600140918               wDsOut(i).Localit = %subst(wXMLstrOut:wTagPos:wTagLen);
037700140917            endif;
037800140917
037900140917            wFindTag  = ' via="';
038000140917            wStartPos = %scan(wFindTag:wXMLstrOut);
038100140917            if wStartPos > *zeros;
038200140917               wTagPos = wStartPos+%len(wFindTag);
038300140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
038400140918               wDsOut(i).Via     = %subst(wXMLstrOut:wTagPos:wTagLen);
038500140917            endif;
038600140917
038700140917            wFindTag  = ' n="';
038800140917            wStartPos = %scan(wFindTag:wXMLstrOut);
038900140917            if wStartPos > *zeros;
039000140917               wTagPos = wStartPos+%len(wFindTag);
039100140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
039200140918               wDsOut(i).Civico  = %subst(wXMLstrOut:wTagPos:wTagLen);
039300140917            endif;
039400140917
039500140917            wFindTag  = ' x="';
039600140917            wStartPos = %scan(wFindTag:wXMLstrOut);
039700140917            if wStartPos > *zeros;
039800140917               wTagPos = wStartPos+%len(wFindTag);
039900140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
040000140918               wDsOut(i).XY_X    = %subst(wXMLstrOut:wTagPos:wTagLen);
040100140917            endif;
040200140917
040300140917            wFindTag  = ' y="';
040400140917            wStartPos = %scan(wFindTag:wXMLstrOut);
040500140917            if wStartPos > *zeros;
040600140917               wTagPos = wStartPos+%len(wFindTag);
040700140917               wTagLen = %scan('"':wXMLstrOut:wTagPos)-wTagPos;
040800140918               wDsOut(i).XY_Y    = %subst(wXMLstrOut:wTagPos:wTagLen);
040900140917            endif;
041000140918
041100140918            endif;
041200140918
041300140918            // Filtro in base allo score richiesto/definito
041400140918            if %trim(wDsOut(i).Score) > pInScore;
041500140918               clear wDsOut(i);
041600140918               i = i - 1;
041700140918            endif;
041800140917
041900140917      /end-free
042000140917     C*
042100140917     C                   ENDSR
042200140917      *
