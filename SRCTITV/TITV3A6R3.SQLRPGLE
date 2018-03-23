000100170913
000200170913
000300170913       //--------------------------------------------------------------
000400170913       // Definizione prototipi
000500170913       //--------------------------------------------------------------
000600170913      /COPY GAITRASRC/SRCPROTOPR,TISWRTLOG
000700170911
000800170913
000900170913       //--------------------------------------------------------------
001000170913       // Definizione variabili
001100170913       //--------------------------------------------------------------
001200170911     D xmlHeader       s           1000a   inz varying
001300170911     D xmlBody         s           5000a   inz varying
001400170907     D dimBody         s              5a   inz
001500170907     D xmlNsSoapEnv    s            256a   inz varying
001600170907     D xmlNsSoap       s            256a   inz varying
001700170907     D xmlNsNs         s            256a   inz varying
001800170907     D xmlNsNs0        s            256a   inz varying
001900170907     D xmlNsNs0ProcD   s            256a   inz varying
002000170907     D xmlNsNs1        s            256a   inz varying
002100170907     D xmlDOC          s            256a   inz varying
002200170911     D xmlRetColumns   s           1000a   inz varying
002300170907     D tim_URL         s            256a   inz varying
002400170911     D destEnvCol      s             48a   inz('cedvetcol')
002500170911     D destEnvEse      s             48a   inz('cedvetese')
002600170907     D destEnv         s             48a   inz
002700170911     D partnerCode     s             20a   inz('269207')
002800170907     D Api             s              1a   inz('''')
002900170911     D wSQL            s           5000a   inz varying
003000170907
003100170907     D returnCode      s             10a   inz
003200170907     D returnDescr     s             80a   inz
003300170913     D richiestaTimestamp...
003400170913     D                 S               Z   inz
003500170913     D RqsType         S            256A   inz
003600170913     D RspLog          S            256A   inz
003700170913     D cKSU            s              8a   inz( '01663932')
003800170911
003900170911     D iAmbiente       s              1a
004000170911     D iSAPnumDOC      s             10a
004100170911     D iStatus         s              4a
004200170911     D iTipoStato      s              2a
004300170911     D iStatusDate     s              8a
004400170911     D iStatusTime     s              6a
004500170911     D oReturnCode     s                   like(returnCode)
004600170911     D oReturnDescr    s                   like(returnDescr)
004700170907
004800170907
004900170911
005000170911     C     *Entry        plist
005100170911     C                   parm                    iAmbiente
005200170911     C                   parm                    iSAPnumDOC
005300170911     C                   parm                    iStatus
005400170911     C                   parm                    iTipoStato
005500170911     C                   parm                    iStatusDate
005600170911     C                   parm                    iStatusTime
005700170911     C                   parm                    oReturnCode
005800170911     C                   parm                    oReturnDescr
005900170911
006000170907
006100170907       EXEC SQL SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
006200170907
006300170911       clear wSQL;
006400170911       clear oReturnCode;
006500170911       clear oReturnDescr;
006600170907
006700170907       // Monitoraggio intero flusso applicativo
006800170907       Monitor;
006900170907
007000170911       // In base al Ambiente indicato in input determino l'endpoint da utilizzare
007100170911       select;
007200170911         when iAmbiente = 'P';
007300170911            destEnv = destEnvEse;
007400170911         when iAmbiente = 'T';
007500170911            destEnv = destEnvCol;
007600170911         other;
007700170911            *inlr = *on;
007800170911            return;
007900170911       endsl;
008000170911
008100170907
008200170911       tim_URL = 'http://' + %trim(destEnv) +
008300170911                             '/VectorDeliveryActivityNotification';
008400170907
008500170908       xmlNsSoapEnv  = 'http://schemas.xmlsoap.org/soap/envelope/';
008600170907
008700170911       xmlNsNs       = 'http://telecomitalia.it/SOA/' +
008800170911                        'VectorDeliveryActivityNotification/2016-05-06';
008900170908       xmlNsNs1      = 'http://telecomitalia.it/SOA/' +
009000170911                        'VectorDeliveryActivityNotificationEntities/2016-05-06';
009100170911       xmlNsSoap     = 'http://telecomitalia.it/SOA/SOAP/SOAPHeader';
009200170907
009300170908       xmlNsNs0      = xmlNsNs;
009400170907
009500170911       xmlNsNs0ProcD = 'ns0:ProcessData/ns0:';
009600170911
009700170907
009800170907       xmlDOC = Api+'$doc/SOAP-ENV:Envelope/SOAP-ENV:Body/' +
009900170911                       'ns0:VectorDeliveryActivityNotificationResponse'+Api;
010000170911
010100170907
010200170908       xmlBody = ' CAST (' + Api +
010300170908       ' <soapenv:Envelope ' +
010400170908       ' xmlns:soapenv="' + xmlNsSoapEnv + '"' +
010500170908       ' xmlns:ns="'      + xmlNsNs      + '"' +
010600170908       ' xmlns:ns1="'     + xmlNsNs1     + '"' +
010700170908       ' xmlns:soap="'    + xmlNsSoap    + '">' +
010800170907       ' <soapenv:Header/>' +
010900170907       ' <soapenv:Body>' +
011000170911       ' <ns:VectorDeliveryActivityNotificationRequest>' +
011100170911       ' <ns:Detail>' +
011200170911       ' <ns1:DeliveryNotifyStatusCaracteristic>' +
011300170911       ' <ns1:Header>' +
011400170911       '   <ns1:SistemaMitt>BRT</ns1:SistemaMitt>' +
011500170911       '   <ns1:NumDoc>'        + %trim(iSAPnumDOC)  + '</ns1:NumDoc>' +
011600170915       '   <ns1:StatoCon>'      + %trim(iStatus)     + '</ns1:StatoCon>' +
011700170915       '   <ns1:TipoStato>'     + %trim(iTipoStato)  + '</ns1:TipoStato>' +
011800170911       '   <ns1:DataEvento>'    + iStatusDate        + '</ns1:DataEvento>' +
011900170911       '   <ns1:OraEvento>'     + iStatusTime        + '</ns1:OraEvento>' +
012000170911       '   <ns1:CodiceVettore>' + %trim(partnerCode) + '</ns1:CodiceVettore>' +
012100170911       ' </ns1:Header>' +
012200170911       ' </ns1:DeliveryNotifyStatusCaracteristic>' +
012300170911       ' </ns:Detail>' +
012400170911       ' </ns:VectorDeliveryActivityNotificationRequest>' +
012500170911       ' <soap:Header></soap:Header>' +
012600170911       ' </soapenv:Body>' +
012700170908       ' </soapenv:Envelope>' + Api + ' AS CLOB(10000)) ';
012800170907
012900170907       dimBody = %trim(%char(%len(%trim(xmlBody))));
013000170907
013100170908       xmlHeader = ' CAST (' + Api +
013200170908       ' <httpHeader connectionTimeout="600000" followRedirects="true"> ' +
013300170908       ' <header name="Content-Type" value="text/xml;charset=utf-8"/> ' +
013400170911       ' <header name="SOAPAction" ' +
013500170911               'value="VectorDeliveryActivityNotification"/> ' +
013600170908       ' </httpHeader>' + Api + ' AS CLOB(10000)) ';
013700170911
013800170907       xmlRetColumns =
013900170907       ' returnCode           Char(10) PATH '''+xmlNsNs0ProcD+
014000170911                                                 'returnCode'' , ' +
014100170907
014200170907       ' returnDescription    Char(80) PATH '''+xmlNsNs0ProcD+
014300170911                                                 'returnDescription''' ;
014400170907
014500170907
014600170911       wSQL = 'SELECT TelecomSetNotification.* ' +
014700170907              'FROM ' +
014800170907              'XMLTABLE( ' +
014900170907              'XMLNAMESPACES( ' +
015000170908              Api + xmlNsSoapenv + Api + ' as "SOAP-ENV", ' +
015100170911              Api + xmlNsNs0     + Api + ' as "ns0"), ' +
015200170907              xmlDOC + ' ' +
015300170907              'PASSING ' +
015400170907              'XMLPARSE(document ' +
015500170907              ' SYSTOOLS.HTTPPOSTCLOB(' +
015600170908              Api + tim_URL + Api + ', ' + xmlHeader +', ' + xmlBody + '))' +
015700170907              ' as "doc" ' +
015800170907              'COLUMNS ' +
015900170907              xmlRetColumns +
016000170911              ') as TelecomSetNotification ' ;
016100170907
016200170907
016300170907       // Dichiarazione cursore
016400170907       exec sql prepare S0 from :wSQL;
016500170907       exec sql declare C0 cursor for S0;
016600170907
016700170907       // Apro il cursore
016800170907       exec sql open C0;
016900170907
017000170907       // Leggo i dati
017100170911       if sqlcode = *zeros;
017200170911          exec sql
017300170911               fetch next from C0 into
017400170911                           :returnCode,
017500170911                           :returnDescr;
017600170913
017700170913          // In funzione del esito chiamata procedo di conseguenza
017800170907          if sqlcode <> 100;
017900170913
018000170913             // Registro nel log esito chiamata
018100170915             RqsType = %trim(tim_URL) +
018200170915                       ' =>  SAP DocNum: ' +
018300170915                             %trim(iSAPnumDOC) +
018400170915                       ' =>  Status inviato: ' +
018500170915                             %trim(iStatus) + ' / ' + %trim(iTipoStato) ;
018600170915
018700170913             RspLog  = %trim(returnCode) + ' - ' +
018800170913                       %trim(returnDescr);
018900170913
019000170913             richiestaTimestamp = %timestamp();
019100170913
019200170913             TisWrtLog( cKSU
019300170913                      : *zeros
019400170913                      : *blanks
019500170913                      : 'OW'
019600170913                      : *blanks
019700170913                      : '*NONE'
019800170913                      : richiestaTimestamp
019900170913                      : 'TELECOM'
020000170913                      : RqsType
020100170913                      : %timestamp()
020200170915                      : %trim(returnCode)
020300170913                      : RspLog
020400170913                      );
020500170913
020600170911             oReturnCode  = returnCode;
020700170911             oReturnDescr = returnDescr;
020800170911          endif;
020900170911       endif;
021000170908
021100170907       // Chiusura cursore
021200170907       exec sql close C0;
021300170907
021400170907       // Gestione errore imprevisto
021500170907       On-Error;
021600170907
021700170907          // Chiusura cursore
021800170907          exec sql close C0;
021900170907
022000170907       // Fine monitoraggio
022100170907       Endmon;
022200170907
022300170907
022400170907       *inlr = *on;
022500170907       return;
022600170907
