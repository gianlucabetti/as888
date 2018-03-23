000100170727     H DECEDIT('0,') DATEDIT(*DMY.)
000200170911     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300091223
000400091223       //--------------------------------------------------------------
000500170905       // Dichiarazione file
000600091223       //--------------------------------------------------------------
000700170905     Ftivpi00f  o    e             disk
000800170508
000900170508
001000170508       //--------------------------------------------------------------
001100170911       // Definizione prototipi
001200170508       //--------------------------------------------------------------
001300170911      /COPY GAITRASRC/SRCPROTOPR,UBISNUM
001400170913      /COPY GAITRASRC/SRCPROTOPR,TISWRTLOG
001500091223
001600160802
001700091223       //--------------------------------------------------------------
001800170905       // Definizione variabili
001900091223       //--------------------------------------------------------------
002000170911      /COPY GAITRASRC/SRCPROTOPI,UBISNUM
002100170914      /COPY GAITRASRC/SRCPROTOPI,TIS713R
002200121106
002300170905     D cTIP            s              2a   inz('VB')
002400170905     D cISV            s              2a   inz('IW')
002500170911     D cSUN            s              9a   inz('901663932')
002600170911     D cKSU            s              8a   inz( '01663932')
002700170727
002800170911     D wMsgOrig        s           1000    inz varying
002900170911     D wMsg            s           1000    inz varying
003000170911     D wDati           s           2048    inz varying
003100170911     D wDest           s             70    inz
003200170911     D wEmail          s             70    inz
003300170911     D wCount          s              6s 0 inz
003400170905     D TIS7VASDS     e ds                  inz
003500170905     D DVPIXXX1      e ds                  inz qualified
003600170905     D sDVPIXXX1       s                   inz like(DVPIXXX1)
003700170911     D EDIVAB0F      e ds                  inz
003800170911     D VABNOT_I        s             35a   inz
003900170911     D VABNOT_J        s             35a   inz
004000170911     D VATNOT_A        s             35a   inz
004100170911     D VATNOT_B        s             35a   inz
004200170911     D VATNOT_I        s             35a   inz
004300170911     D VATNOT_J        s             35a   inz
004400170911     D VATNOT_S        s             35a   inz
004500170911     D TISI95DS      e ds
004600170911     D datcor          s              8s 0 inz
004700170911     D wStsNorm        s              2a   inz
004800170914     D wStsNorm_KO     s              2a   inz('ZB')
004900170914     D Ambiente        s              1a   inz('T')
005000170913     D richiestaTimestamp...
005100170913     D                 S               Z   inz
005200170913     D RqsType         S            256A   inz
005300170913     D RspLog          S            256A   inz
005400170911
005500170911     D xmlHeader       s           5000a   inz varying
005600170911     D xmlBody         s           5000a   inz varying
005700170911     D dimBody         s              5a   inz
005800170911     D xmlNsSoapEnv    s            256a   inz varying
005900170911     D xmlNsSoap       s            256a   inz varying
006000170911     D xmlNsNs         s            256a   inz varying
006100170911     D xmlNsNs0        s            256a   inz varying
006200170911     D xmlNsNs0ProcD   s            256a   inz varying
006300170911     D xmlNsNs0Activ   s            256a   inz varying
006400170911     D xmlNsNs1        s            256a   inz varying
006500170911     D xmlDOC          s            256a   inz varying
006600170911     D xmlRetColumns   s           5000a   inz varying
006700170911     D tim_URL         s            256a   inz varying
006800170911     D destEnvCol      s             48a   inz('cedvetcol')
006900170911     D destEnvEse      s             48a   inz('cedvetese')
007000170911     D destEnv         s             48a   inz
007100170911     D partnerCode     s             20a   inz('269207')
007200170911     D businessID      s             24a   inz
007300170911     D SepDec          s              1a   inz(',')
007400170911     D Api             s              1a   inz('''')
007500170911     D wSQL            s          10000a   inz varying
007600170911
007700170928     D K_NumDoc        s             10a   inz
007800170928     D DS_DATI         ds                  inz likeds(DS_DATI_ZJ)
007900170928     D DS_DATI_ZJ      ds                  inz
008000170928     D returnCode                    10a
008100170928     D returnDescr                   80a
008200170928     D Stato                          4a
008300170928     D NumDoc                        10a
008400170928     D CodiceCliente                 10a
008500170928     D CognomeDest                   40a
008600170928     D NomeDest                      40a
008700170928     D NomeReferente                 40a
008800170928     D IndirizzoDest                 80a
008900170928     D CapDest                       10a
009000170928     D Citta                         40a
009100170928     D Country                        3a
009200170928     D Provincia                      3a
009300170928     D Telefono                      30a
009400170928     D TelAltern                     30a
009500170928     D EmailDest                     70a
009600170928     D RichApp                        1a
009700170928     D ImpContras                    15a
009800170911
009900170911
010000170911     D iAmbiente       s              1a
010100170911     D iSAPnumDOC      s             10a
010200170911     D iStatus         s              4a
010300170911     D iTipoStato      s              2a
010400170911     D iStatusDate     s              8a
010500170911     D iStatusTime     s              6a
010600170911     D oReturnCode     s                   like(returnCode)
010700170911     D oReturnDescr    s                   like(returnDescr)
010800170911
010900170727
011000091223
011100130307      /free
011200091223
011300091223       //--------------------------------------------------------------
011400121106       // M A I N - L I N E
011500091223       //--------------------------------------------------------------
011600140806
011700170911       EXEC SQL SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
011800170911
011900170911
012000160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
012100160805       Monitor;
012200160805
012300130307       // Operazioni iniziali?
012400170508       exsr RoutInz;
012500170905
012600170905       // Stacco progressivo univoco transazione
012700170905       exsr CalProg;
012800130307
012900170911       // Eseguo query web service Telecom per reperimento dati spedizione
013000170508       exsr RtvSped;
013100170905
013200170905       // Al termine rilascio i dati reperiti per avvio processo di traduzione
013300170905       exsr RlsDati;
013400160804
013500170905       // Gestione errore imprevisto
013600160805       On-Error;
013700170911
013800170911          // Accensione indicatore di errore
013900170911          *in31 = *on;
014000170905
014100170905       // Fine monitoring
014200160805       Endmon;
014300170727
014400160920       // Uscita
014500160921       exsr exeUscita;
014600170911
014700091223
014800160802
014900091223       //--------------------------------------------------------------
015000121107       // Operazioni iniziali.
015100091223       //--------------------------------------------------------------
015200170726       BEGSR  RoutInz;
015300160601
015400170911         // Inizializzo indicatori di procedura
015500170905         *in31 = *off;
015600170911         *in55 = *off;
015700170911
015800170911         // Inizializzazione campi di work
015900170911         clear EDIVAB0F;
016000170911         clear DVPIXXX1;
016100170911         clear wSQL;
016200170911         clear wDati;
016300170911         clear wCount;
016400170911
016500170911         // Reperisco data corrente
016600170911         datcor = %dec(%date() : *iso);
016700170911
016800170911         // Stabilisco l'ambiente PROD/TEST richiesto
016900170911         select;
017000170914           when Ambiente = 'P';
017100170911              destEnv = destEnvEse;
017200170914           when Ambiente = 'T';
017300170911              destEnv = destEnvCol;
017400170911           other;
017500170911              exsr exeUscita;
017600170911         endsl;
017700170905
017800170905         // Inizializzo il messaggio di errore per alert email
017900170911         wMsgOrig  = 'ATTENZIONE problemi nel acquisizione dati Telecom' +
018000170914                     ' via Web Service. '      + ':/N' ;
018100160202
018200160202       ENDSR;
018300170905
018400170905
018500170905
018600170911       //--------------------------------------------------------------
018700170911       // Stacco progressivo univoco transazione
018800170911       //--------------------------------------------------------------
018900170905       BEGSR CalProg;
019000170905     C*
019100170905     C* Stacco progressivo univoco download
019200170905     C                   clear                   TIS7VASDS
019300170905     C                   eval      i§7VASOPZ = 'PRG'
019400170905     C                   call(e)   'TIS7VASR1'
019500170905     C                   parm                    TIS7VASDS
019600170905     C*
019700170905     C* Se OK => proseguo
019800170905     C                   if        not %error AND
019900170905     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
020000170905     C                   else
020100170905     C                   seton                                        31
020200170905     C                   exsr      exeUscita
020300170905     C                   endif
020400170905     C*
020500170905       ENDSR;
020600170905
020700170905
020800170905
020900170905       //--------------------------------------------------------------
021000170911       // Eseguo query web service Telecom per reperimento dati spedizione
021100170905       //--------------------------------------------------------------
021200170911       BEGSR  RtvSped;
021300170928
021400170928       *in53 = *off;
021500170928       *in54 = *off;
021600170928       clear K_NumDoc;
021700170911
021800170914       businessID  = '990'+%subst(o§7VASPRG:3);
021900170911
022000170911       tim_URL = 'http://' + %trim(destEnv) + '/VectorOrderMgmtQuery';
022100170911
022200170911       xmlNsSoapEnv  = 'http://schemas.xmlsoap.org/soap/envelope/';
022300170911
022400170911       xmlNsNs       = 'http://telecomitalia.it/SOA/' +
022500170911                          'VectorDeliveryActivityQuery/2016-05-06';
022600170911       xmlNsNs1      = 'http://telecomitalia.it/SOA/' +
022700170911                          'VectorDeliveryActivityQueryCustomTypes/2016-05-06';
022800170911       xmlNsSoap     = 'http://telecomitalia.it/SOA/SOAP/SOAPHeader';
022900170911
023000170911       xmlNsNs0      = xmlNsNs;
023100170911
023200170915       xmlNsNs0ProcD = '../ns0:ProcessData/ns0:';
023300170911
023400170915       xmlNsNs0Activ = 'ns1:Header/ns1:';
023500170911
023600170911
023700170911       xmlDOC = Api+'$doc/SOAP-ENV:Envelope/SOAP-ENV:Body/' +
023800170915       'ns0:getListDeliveryDetailsByPartnerResponse/ns0:activity'+Api;
023900170911
024000170911
024100170911       xmlBody = ' CAST (' + Api +
024200170911       ' <soapenv:Envelope ' +
024300170911       ' xmlns:soapenv="' + xmlNsSoapEnv + '"' +
024400170911       ' xmlns:ns="'      + xmlNsNs      + '"' +
024500170911       ' xmlns:ns1="'     + xmlNsNs1     + '"' +
024600170911       ' xmlns:soap="'    + xmlNsSoap    + '">' +
024700170911       ' <soapenv:Header/>' +
024800170911       ' <soapenv:Body>' +
024900170911       ' <ns:getListDeliveryDetailsByPartnerRequest>' +
025000170911       ' <ns:activity>' +
025100170911       ' <ns1:partnerCode>' + %trim(partnerCode) + '</ns1:partnerCode>' +
025200170911       ' </ns:activity>' +
025300170911       ' </ns:getListDeliveryDetailsByPartnerRequest>' +
025400170911       ' <soap:Header>' +
025500170911       ' <soap:sourceSystem>BRT</soap:sourceSystem>' +
025600170911       ' <soap:businessID>' + %trim(businessID) + '</soap:businessID>' +
025700170911       ' </soap:Header>' +
025800170911       ' </soapenv:Body>' +
025900170911       ' </soapenv:Envelope>' + Api + ' AS CLOB(10000)) ';
026000170911
026100170911       dimBody = %trim(%char(%len(%trim(xmlBody))));
026200170911
026300170911       xmlHeader = ' CAST (' + Api +
026400170911       ' <httpHeader connectionTimeout="600000" followRedirects="true"> ' +
026500170911       ' <header name="Content-Type" value="text/xml;charset=utf-8"/> ' +
026600170911       ' <header name="SOAPAction" value="getListDeliveryDetailsByPartner"/> ' +
026700170911       ' </httpHeader>' + Api + ' AS CLOB(10000)) ';
026800170911
026900170911       xmlRetColumns =
027000170911       ' returnCode           Char(10) PATH '''+xmlNsNs0ProcD+
027100170911                                                 'returnCode'' , ' +
027200170911
027300170911       ' returnDescription    Char(80) PATH '''+xmlNsNs0ProcD+
027400170911                                                 'returnDescription'' , ' +
027500170927
027600170927       ' Stato                Char(4)  PATH '''+xmlNsNs0Activ+
027700170927       'CaratterizzazioneConsegna/ns1:Stato'' , ' +
027800170911
027900170911       ' NumDoc               Char(10) PATH '''+xmlNsNs0Activ+
028000170911       'CaratterizzazioneConsegna/ns1:DettaglioDocumento/ns1:NumDoc'' , ' +
028100170911
028200170911       ' CodiceCliente        Char(10) PATH '''+xmlNsNs0Activ+
028300170911                                                 'CodiceCliente'' , ' +
028400170911
028500170911       ' CognomeDest          Char(40) PATH '''+xmlNsNs0Activ+
028600170911                                   'Destinatario/ns1:CognomeDest'' , ' +
028700170911
028800170911       ' NomeDest             Char(40) PATH '''+xmlNsNs0Activ+
028900170911                                   'Destinatario/ns1:NomeDest'' , ' +
029000170911
029100170911       ' NomeReferente        Char(40) PATH '''+xmlNsNs0Activ+
029200170911                                   'Destinatario/ns1:NomeReferente'' , ' +
029300170911
029400170911       ' IndirizzoDest        Char(80) PATH '''+xmlNsNs0Activ+
029500170911                                   'Destinatario/ns1:IndirizzoDest'' , ' +
029600170911
029700170911       ' CapDest              Char(10) PATH '''+xmlNsNs0Activ+
029800170911                                   'Destinatario/ns1:CapDest'' , ' +
029900170911
030000170911       ' Citta                Char(40) PATH '''+xmlNsNs0Activ+
030100170911                                   'Destinatario/ns1:Citta'' , ' +
030200170911
030300170911       ' Country              Char(3)  PATH '''+xmlNsNs0Activ+
030400170911                                   'Destinatario/ns1:Country'' , ' +
030500170911
030600170911       ' Provincia            Char(3)  PATH '''+xmlNsNs0Activ+
030700170911                                   'Destinatario/ns1:Provincia'' , ' +
030800170911
030900170911       ' Telefono             Char(30) PATH '''+xmlNsNs0Activ+
031000170911                                   'Destinatario/ns1:Telefono'' , ' +
031100170911
031200170911       ' TelAltern            Char(30) PATH '''+xmlNsNs0Activ+
031300170911                        'Destinatario/ns1:TelefonoAlternativo'' , ' +
031400170911
031500170911       ' EmailDest            Char(70) PATH '''+xmlNsNs0Activ+
031600170911                                   'Destinatario/ns1:EmailDest'' , ' +
031700170911
031800170911       ' RichApp              Char(1)  PATH '''+xmlNsNs0Activ+
031900170911                                   'Vettore/ns1:RichApp'' , ' +
032000170911
032100170911       ' ImportoContrassegno  Char(15) PATH '''+xmlNsNs0Activ+
032200170911                                   'ImportoContrassegno''' ;
032300170911
032400170911
032500170911       wSQL = 'SELECT TelecomGetBolle.* ' +
032600170911              'FROM ' +
032700170911              'XMLTABLE( ' +
032800170911              'XMLNAMESPACES( ' +
032900170911              Api + xmlNsSoapenv + Api + ' as "SOAP-ENV", ' +
033000170911              Api + xmlNsNs0     + Api + ' as "ns0", ' +
033100170911              Api + xmlNsNs1     + Api + ' as "ns1"), ' +
033200170911              xmlDOC + ' ' +
033300170911              'PASSING ' +
033400170911              'XMLPARSE(document ' +
033500170911              ' SYSTOOLS.HTTPPOSTCLOB(' +
033600170911              Api + tim_URL + Api + ', ' + xmlHeader +', ' + xmlBody + '))' +
033700170911              ' as "doc" ' +
033800170911              'COLUMNS ' +
033900170911              xmlRetColumns +
034000170911              ') as TelecomGetBolle ';
034100170911
034200170911
034300170911       // Dichiarazione cursore
034400170911       exec sql prepare S0 from :wSQL;
034500170911       exec sql declare C0 cursor for S0;
034600170911
034700170911       // Apro il cursore
034800170911       exec sql open C0;
034900170911
035000170911       // Leggo i dati
035100170911       dow sqlcode = *zeros;
035200170911           exec sql
035300170928                fetch next from C0 into
035400170928                                   :DS_DATI;
035500170911
035600170913          // In funzione del esito chiamata procedo di conseguenza
035700170914          select;
035800170914
035900170914          when sqlcode < *zeros;
036000170914
036100170914             *in31 = *on;
036200170914             wMsg  = %trim(wMsgOrig)                + ':/N' +
036300170928                     'Metodo: OrderQuery '          + ':/N' +
036400170914                     'SQLcode: ' + %char(sqlcode)   + ':/N' +
036500170914                     ' '                            + ':/N' +
036600170914                     ' Verificare connettività'             +
036700170914                     ' verso endpoint Telecom: '            +
036800170914                     %trim(destEnv)                 + ':/N' +
036900170914                     '           :/N' ;
037000170914             exsr exeUscita;
037100170914
037200170914          when sqlcode <> 100;
037300170913
037400170913           // Registro nel log esito chiamata
037500171005           RqsType = %trim(tim_URL) +
037600171005                  ' =>  SAP DocNum: ' + DS_DATI.NumDoc +
037700171005                  ' =>  Status ricevuto: ' + %trim(DS_DATI.Stato);
037800171005
037900171005           RspLog  = %trim(DS_DATI.returnCode) + ' - ' +
038000171005                     %trim(DS_DATI.returnDescr);
038100171005
038200171005           // Scrittura log "internet"
038300171005           exsr WriLog;
038400170911
038500170927           // Se esito invocazione servizio OK
038600170928           if %trim(DS_DATI.returnCode) <> *blanks AND
038700170928              %dec(DS_DATI.returnCode:10:0) = 0;
038800170928
038900170928            // Verifico rottura di codice per ID SAP "NumDoc"
039000170928            if DS_DATI.NumDoc <> K_NumDoc;
039100170928
039200170928               // Se ok "ricevo" la spedizione
039300170928               if K_NumDoc <> *blanks;
039400170928                  exsr RcvSped;
039500170928               endif;
039600170928
039700170928            endif;
039800170928
039900170928
040000170928            // In funzione dello Stato reperito effettuo operazioni differenti
040100170928            select;
040200170928
040300170928              // Considero unicamente le spedizioni in Stato 'ZJ'
040400170928              when %trim(DS_DATI.Stato) <> 'ZJ';
040500170928
040600170928                 // Pulizia dati da scaricare
040700170928                 clear DS_DATI_ZJ;
040800170928                 *in54 = *off;
040900170928
041000170928              // Considero unicamente le spedizioni in Stato 'ZJ'
041100170928              when %trim(DS_DATI.Stato) = 'ZJ';
041200170928
041300170928                 // Se Stato da considerare salvo i dati in DS apposita
041400170928                 DS_DATI_ZJ = DS_DATI;
041500170928                 *in54 = *on;
041600170928
041700170928                 // Salvo la nuova chiave di rottura
041800170928                 K_NumDoc = DS_DATI.NumDoc;
041900170927
042000170928            endsl;
042100170911
042200170911           else;
042300170911             *in31 = *on;
042400170928             wMsg  = %trim(wMsgOrig)                   + ':/N' +
042500170928                     'Metodo: OrderQuery '             + ':/N' +
042600170928                     'Esito: '                         + ':/N' +
042700170928                     %trim(DS_DATI.returnCode) + '   ' +
042800170928                     %trim(DS_DATI.returnDescr)        +
042900170913                     '           :/N' ;
043000170911             exsr exeUscita;
043100170911
043200170911           endif;
043300170911
043400170914          endsl;
043500170911       enddo;
043600170928
043700170928       // Scaricamento eventuali dati in sospeso dalla rottura di codice
043800170928       exsr RcvSped;
043900170911
044000170911       // Chiusura cursore
044100170911       exec sql close C0;
044200170905
044300170905       ENDSR;
044400170928
044500170928
044600170928
044700170928       //--------------------------------------------------------------
044800170928       // "Ricezione" spedizione
044900170928       //--------------------------------------------------------------
045000170928       BEGSR RcvSped;
045100170928
045200170928         // Procedo solamente se OK e se ID SAP "NumDoc" è valorizzato
045300170928         if *in54 = *on AND NumDoc <> *blanks;
045400170928
045500170928         // Incremento contatore di spedizioni estratte
045600170928         wCount = wCount + 1;
045700170928
045800170928         // Valorizzazioe formato bolla BRT
045900170928         exsr ValBol;
046000170928
046100170928         // Eseguo verifica indirizzo tramite TISI95R
046200170928         exsr ChkInd;
046300170928
046400170928         // Se indirizzo non "KO" => importo spedizione corrente in BRT
046500170928         if wStsNorm <> wStsNorm_KO;
046600170928            exsr ExeOutDati;
046700170928         endif;
046800170928
046900170928         // Ritorno esito verifica indirizzo
047000170928         exsr SndSts;
047100170928
047200170928         endif;
047300170928
047400170928       ENDSR;
047500170911
047600170911
047700170911
047800170911       //--------------------------------------------------------------
047900170911       // Valorizzazioe formato bolla BRT
048000170911       //--------------------------------------------------------------
048100170911       BEGSR ValBol;
048200170911
048300170911         // Valorizzo il formato bolla BRT
048400170911         clear EDIVAB0F;
048500170911
048600170911         // Valorizzo i campi di default
048700170911         vabCBO = '1';
048800170911
048900170911         // Stacco "per sicurezza" in progressivo univoco per ogni spedizione
049000170911         exsr RepNsp;
049100170911
049200170911         // Valorizzo i campi provenienti dal cliente
049300170911         vabRMA   = %trim(NumDoc);
049400170911         if %trim(NumDoc) <> *blanks AND
049500170911            %trim(NumDoc) <> *all'0';
049600170911            PiStr = %trim(NumDoc);
049700170911            exsr chkNum;
049800170911            if PiInt = *on;
049900170911               vabRMN = PiVal;
050000170911            endif;
050100170911         endif;
050200170911         vabNAS   = %trim(CodiceCliente);
050300170911         wDest    = %trim(NomeDest) + ' ' +
050400170911                         %trim(CognomeDest);
050500170911         vabRSD   = %subst(wDest:1:35);
050600170911         vabRD2   = %subst(wDest:36:35);
050700170911         vatNOT_A = %trim(NomeReferente);
050800170911         vabIND   = %trim(IndirizzoDest);
050900170911         vabCAD   = %trim(CapDest);
051000170911         vabLOD   = %trim(Citta);
051100170911         //vabNZD   = %trim(Country);
051200170911         vabPRD   = %trim(Provincia);
051300170911         vatNOT_B = %trim(Telefono);
051400170911         vatNOT_S = %trim(TelAltern);
051500170911         wEmail   = %trim(EmailDest);
051600170911         vabNOT_I = %subst(wEmail:1:35);
051700170911         vabNOT_J = %subst(wEmail:36:35);
051800170911         if %trim(RichApp) = 'X';
051900170911            vabTC1 = 'A';
052000170911         endif;
052100170911         if %trim(ImpContras) <> *blanks AND
052200170911            %trim(ImpContras) <> *all'0';
052300170911            vabCBO = '4';
052400170911            vabVCA = 'EUR';
052500170911            PiStr = %trim(ImpContras);
052600170911            exsr chkNum;
052700170911            if PiNum = *on;
052800170911               vabCAS = PiVal;
052900170911            else;
053000170911               vabCAS = *hival;
053100170911            endif;
053200170911         endif;
053300170911
053400170911       ENDSR;
053500170911
053600170911
053700170911
053800170911       //--------------------------------------------------------------
053900170911       // Compongo buffer output per traduzione UPLOAD
054000170911       //--------------------------------------------------------------
054100170911       BEGSR ExeOutDati;
054200170911
054300170911         // Se prima scrittura => riga di intestazione colonne
054400170911         if not *in55;
054500170911            exsr ExeOutHeader;
054600170911            *in55 = *on;
054700170911         endif;
054800170911
054900170911         wDati =
055000170911                 %editc(vabNSP:'X')             + ';' +
055100170911                  %trim(vabCBO)                 + ';' +
055200170911                  %trim(vabRMA)                 + ';' +
055300170911                 %editc(vabRMN:'X')             + ';' +
055400170911                  %trim(vabRSD)                 + ';' +
055500170911                  %trim(vabRD2)                 + ';' +
055600170911                  %trim(vabIND)                 + ';' +
055700170911                  %trim(vabCAD)                 + ';' +
055800170911                  %trim(vabLOD)                 + ';' +
055900170911                  %trim(vabPRD)                 + ';' +
056000170911                  %trim(vabNZD)                 + ';' +
056100170911                  %trim(vabNAS)                 + ';' +
056200170911                  %trim(vabTC1)                 + ';' +
056300170911                 %editc(vabCAS:'X')             + ';' +
056400170911                  %trim(vabVCA)                 + ';' +
056500170911                  %trim(vatNOT_A)               + ';' +
056600170911                  %trim(vatNOT_B)               + ';' +
056700170911                  %trim(vatNOT_S)               + ';' +
056800170911                  %trim(vatNOT_I)               + ';' +
056900170911                  %trim(vatNOT_J)               + ';' ;
057000170911
057100170911         // Scrittura dati
057200170911         exsr WriVPI;
057300170911
057400170911       ENDSR;
057500170911
057600170911
057700170911
057800170911       //--------------------------------------------------------------
057900170911       // Compongo buffer output per riga di intestazione colonne
058000170911       //--------------------------------------------------------------
058100170911       BEGSR ExeOutHeader;
058200170911
058300170911         wDati =
058400170911                       'VABNSP'                 + ';' +
058500170911                       'VABCBO'                 + ';' +
058600170911                       'VABRMA'                 + ';' +
058700170911                       'VABRMN'                 + ';' +
058800170911                       'VABRSD'                 + ';' +
058900170911                       'VABRD2'                 + ';' +
059000170911                       'VABIND'                 + ';' +
059100170911                       'VABCAD'                 + ';' +
059200170911                       'VABLOD'                 + ';' +
059300170911                       'VABPRD'                 + ';' +
059400170911                       'VABNZD'                 + ';' +
059500170911                       'VABNAS'                 + ';' +
059600170911                       'VABTC1'                 + ';' +
059700170911                       'VABCAS'                 + ';' +
059800170911                       'VABVCA'                 + ';' +
059900170911                       'VATNOT_A'               + ';' +
060000170911                       'VATNOT_B'               + ';' +
060100170911                       'VATNOT_S'               + ';' +
060200170911                       'VATNOT_I'               + ';' +
060300170911                       'VATNOT_J'               + ';' ;
060400170911
060500170911         // Scrittura dati
060600170911         exsr WriVPI;
060700170911
060800170911       ENDSR;
060900170911
061000170911
061100170911
061200170911       //--------------------------------------------------------------
061300170911       // Eseguo verifica indirizzo
061400170911       //--------------------------------------------------------------
061500170911       BEGSR ChkInd;
061600170911     C*
061700170911     C                   CLEAR                   TISI95DS
061800170911     C                   EVAL      I95TCN = '4'
061900170911     C                   EVAL      I95DAT = datcor
062000170911     C                   EVAL      I95NAR = vabNZD
062100170911     C                   EVAL      I95PRV = vabPRD
062200170911     C                   EVAL      I95CAP = vabCAD
062300170911     C                   EVAL      I95LOC = vabLOD
062400170911     C                   CALL      'TISI95R'
062500170911     C                   PARM                    TISI95DS
062600170911     C*
062700170911     C* Se esito KO => indirizzo destinatario ERRATO
062800170911     C                   IF        O95ERR <> *blanks
062900170911     C                   EVAL      wStsNorm = 'ZB'
063000170911     C                   ELSE
063100170911     C                   IF        O95PRV <> vabPRD OR
063200170911     C                             O95CAP <> vabCAD OR
063300170911     C                             O95LOC <> vabLOD
063400170911     C                   EVAL      wStsNorm = 'ZY'
063500170911     C                   ELSE
063600170911     C                   EVAL      wStsNorm = 'ZX'
063700170911     C                   ENDIF
063800170911     C                   EVAL      vabPRD = O95PRV
063900170911     C                   EVAL      vabCAD = O95CAP
064000170911     C                   EVAL      vabLOD = O95LOC
064100170911     C                   ENDIF
064200170911     C*
064300170911       ENDSR;
064400170905
064500170905
064600170905
064700170911       //--------------------------------------------------------------
064800170911       // Scrittura dati di output per UPLOAD
064900170911       //--------------------------------------------------------------
065000170911       BEGSR WriVPI;
065100170905
065200170905         // Valorizzo i flag Operativi del TIVPI in modo da "sincronizzare" la transazione
065300170905         DVPIXXX1.DVPISTS = '1';
065400170905         DVPIXXX1.DVPIPRG = %subst(o§7VASPRG:3:8);
065500170905
065600170905         // Inserisco il dato ricevuto
065700170905         VPITIP = cTIP;
065800170905         VPIISV = cISV;
065900170905         VPISUN = cSUN;
066000170905         VPIKSU = cKSU;
066100170911         VPIDTA = wDati;
066200170905         VPIXXX = DVPIXXX1;
066300170905
066400170905         // Scarico il buffer di output corrente
066500170905         write(e) tivpi000;
066600170928         *in53 = *on;
066700170905
066800170905         // Gestisco eventuale errore
066900170905         if %error;
067000170905         endif;
067100170905
067200170905       ENDSR;
067300170911
067400170911
067500170911
067600170911       //--------------------------------------------------------------
067700170911       // Ritorno esito verifica indirizzo
067800170911       //--------------------------------------------------------------
067900170911       BEGSR SndSts;
068000170911     C*
068100170914     C                   eval      iAmbiente   = Ambiente
068200170911     C                   eval      iSAPnumDOC  = %trim(NumDoc)
068300170911     C                   eval      iStatus     = wStsNorm
068400170913     C                   eval      iTipoStato  = 'SC'
068500170911     C                   eval      iStatusDate = %editc(datcor:'X')
068600170911     C                   eval      iStatusTime = %editc(oracor:'X')
068700170911     C                   call      'TITV3A6C3'
068800170911     C                   parm                    iAmbiente
068900170911     C                   parm                    iSAPnumDOC
069000170911     C                   parm                    iStatus
069100170911     C                   parm                    iTipoStato
069200170911     C                   parm                    iStatusDate
069300170911     C                   parm                    iStatusTime
069400170911     C                   parm                    oReturnCode
069500170911     C                   parm                    oReturnDescr
069600170911     C*
069700170911     C*  Se esito invocazione servizio non OK
069800170911     C                   if         %trim(oReturnCode) = '0'
069900170911     C                   else
070000170914     C                   eval      wMsg  =
070100170914     C                             %trim(wMsgOrig)                + ':/N' +
070200170914     C                             'Metodo: DeliveryNotification' + ':/N' +
070300170928     C                             'Esito: '                      + ':/N' +
070400170914     C                             %trim(oReturnCode) + '   '             +
070500170914     C                             %trim(oReturnDescr)                    +
070600170914     C                             '           :/N'
070700170911     C                   exsr      sndeml
070800170911     C                   endif
070900170911     C*
071000170911       ENDSR;
071100170905
071200170905
071300170905
071400170911       //--------------------------------------------------------------
071500170911       // Rilascio i dati reperiti per avvio processo di traduzione
071600170911       //--------------------------------------------------------------
071700170905       BEGSR RlsDati;
071800170911     C*
071900170928     C                   if        *in53 = *on
072000170905     C
072100170905     C                   eval      sDVPIXXX1 = DVPIXXX1
072200170905     C
072300170905     C/EXEC SQL
072400170905     C+ UPDATE TIVPI00F SET VPIXXX = ' '
072500170905     C+ WHERE VPIISV = :cISV   AND VPISUN = :cSUN   AND
072600170905     C+       VPIKSU = :cKSU   AND VPIXXX = :sDVPIXXX1
072700170905     C/END-EXEC
072800170905     C
072900170905     C                   if        sqlcod < *zeros
073000170905     C                   seton                                        31
073100170905     C                   endif
073200170928     C
073300170928     C                   endif
073400170911     C*
073500170905       ENDSR;
073600170911
073700170911
073800170911
073900170911       //--------------------------------------------------------------
074000170911       // Controllo numericità dato
074100170911       //--------------------------------------------------------------
074200170911       BEGSR chkNum;
074300170911
074400170911         // Imposto default separatore decimale
074500170911         PiDecChr = SepDec;
074600170911
074700170911         // Verifica numericità
074800170911         callp UBISNUM_Check(PiStr
074900170911                            :PiDecChr
075000170911                            :PiVal
075100170911                            :PiNum
075200170911                            :PiInt);
075300170911
075400170911       ENDSR;
075500170913
075600170913
075700170913
075800170913       //--------------------------------------------------------------
075900170913       // Registro nel log esito chiamata
076000170913       //--------------------------------------------------------------
076100170913       BEGSR WriLog;
076200170913
076300170913         richiestaTimestamp = %timestamp();
076400170913
076500170913         TisWrtLog( cKSU
076600170913                  : *zeros
076700170913                  : *blanks
076800170913                  : 'IW'
076900170913                  : *blanks
077000170913                  : '*NONE'
077100170913                  : richiestaTimestamp
077200170913                  : 'TELECOM'
077300170913                  : RqsType
077400170913                  : %timestamp()
077500170913                  : %trim(returnCode)
077600170913                  : RspLog
077700170913                  );
077800170913
077900170913       ENDSR;
078000170911
078100170727
078200170727
078300170727       //--------------------------------------------------------------
078400170727       // Uscita
078500170727       //--------------------------------------------------------------
078600170905       BEGSR exeUscita;
078700170911
078800170911         // Chiusura cursore
078900170911         exec sql close C0;
079000170727
079100170905         // Se errore => invio email di alert ed interrompo l'elaborazione
079200170905         if *in31 = *on;
079300170905            exsr sndeml;
079400170905            dump(a);
079500170727         endif;
079600170727
079700170905         *inlr = *on;
079800170727         return;
079900170727
080000170911       ENDSR;
080100170911
080200170911
080300170911
080400170911       //--------------------------------------------------------------
080500170911       // Stacco numeratore per numero spedizione
080600170911       //--------------------------------------------------------------
080700170911       BEGSR RepNsp;
080800170911     C*
080900170911     C* CALCOLA LA DATA CORRENTE
081000170911     C                   time                    wn14             14 0
081100170911     C                   movel     wn14          oracor            6 0
081200170911     C*
081300170911     C                   eval      vabNSP = oracor + wCount
081400170911     C*
081500170911       ENDSR;
081600170905
081700170905
081800170905
081900170905      /TITLE Compongo il testo e spedisco una e-m@ail
082000170905     C     sndeml        begsr
082100170905     C*
082200170905     C* Inizializzo variabili
082300170905     C                   movel     *blanks       wrkEmlTo        253
082400170905     C                   movel     *blanks       wrkEmlCc        253
082500170905     C                   movel     *blanks       wrkMsg         5000
082600170905     C                   movel     *blanks       wrkOgg           44
082700170905     C                   movel     *blanks       wrkEsi            1
082800170905     C*
082900170905     C* Valorizzo i campi della e-m@ail
083000170905     C                   eval      wrkEmlTo = 'cedalert@brt.it'
083100170914     C                   eval      wrkOgg   = 'VAS - WARNING!!!' +
083200170914     C                             ' (BID ' + %trim(businessID) + ')'
083300170905     C                   eval      wrkMsg   = wMsg
083400170905     C*
083500170905     C                   call(e)   'TIS701C1'
083600170905     C                   parm                    wrkEmlTo
083700170905     C                   parm                    wrkEmlCc
083800170905     C                   parm                    wrkOgg
083900170905     C                   parm                    wrkMsg
084000170905     C                   parm                    wrkEsi
084100170905     C*
084200170905     C                   endsr
084300170914
084400170914
084500170914
084600170914     C*
084700170914     C     CarTab        begsr
084800170914     C*
084900170914     C* Reperimento Ambiente COLLAUDO(TEST) / ESERCIZIO(PROD) da utilizzare
085000170914     C                   eval      iS713TBECOD = 'VPR'
085100170914     C                   eval      iS713TBEKE1 = '*VAS-CLI'
085200170914     C                   eval      iS713TBEKE2 = 'TELECOM'
085300170914     C                   eval      iS713TBELIN = *blanks
085400170914     C                   eval      iS713TBESIF = *blanks
085500170914     C                   call      'TIS713R'
085600170914     C                   parm                    iS713TBECOD
085700170914     C                   parm                    iS713TBEKE1
085800170914     C                   parm                    iS713TBEKE2
085900170914     C                   parm                    iS713TBELIN
086000170914     C                   parm                    iS713TBESIF
086100170914     C                   parm                    oS713TBEUNI
086200170914     C                   parm                    oS713ESITO
086300170914     C*
086400170914     C                   if        oS713ESITO = '1'
086500170914     C                   eval      Ambiente = %trim(oS713TBEUNI)
086600170914     C                   endif
086700170914     C*
086800170914     C                   endsr
086900170914     C*
087000170731
087100170727
087200170727
087300170727       //--------------------------------------------------------------
087400170727       // *INZSR
087500170727       //--------------------------------------------------------------
087600170727       BEGSR  *inzsr;
087700170727
087800170914         exsr CarTab;
087900170727
088000170727       ENDSR;
088100091223
088200160802
088300091223      /end-free
