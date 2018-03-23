000100091228       //==============================================================
000200160802       // Verifica credenziali per accesso servizi VAS
000300091228       //==============================================================
000400091228
000500091228
000600160202     **
000700160202     ** ISTRUZIONI PER LA COMPILAZIONE
000800160202     **
000900160802     ** 1. Creare il modulo UBISVPWDR (Opz 15 PDM)
001000161017     **    indicando TGTRLS(*CURRENT) e CLOSQLCSR(*ENDACTGRP)
001100160802     ** 2. Creare/Aggiornare il programma di servizio UBISVPWDR (CRTSRVPGM / UPDSRVPGM)
001200160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001300160923     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBISVPWDR) EXPORT(*ALL)
001400160923     **                TEXT('Driver check credenziali accesso servizi VAS')
001500160802     ** 3. Creare/Aggiornare il programma UBISVPWDR (CRTPGM / UPDPGM)
001600160601     **    con ACTGRP(*caller), la BNDDIR non è importante tanto non chiama niente
001700160923     **      CRTPGM PGM(GAITRAOBJ/UBISVPWDR) ACTGRP(*CALLER)
001800161017     ** 4. Eventualmente si può cancellare il modulo.
001900160202     **
002000091228
002100091228       //--------------------------------------------------------------
002200121106       // Specifiche di controllo.
002300091228       //--------------------------------------------------------------
002400091223
002500091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600160601     h alwnull(*inputonly)
002700091223
002800091223       //--------------------------------------------------------------
002900121106       // Dichiarazione file.
003000091223       //--------------------------------------------------------------
003100160218
003200160531
003300091223       //--------------------------------------------------------------
003400121106       // Definizione costanti.
003500091223       //--------------------------------------------------------------
003600100302
003700091223
003800091223       //--------------------------------------------------------------
003900121106       // Definizione schiere.
004000091223       //--------------------------------------------------------------
004100091223
004200091223
004300091223       //--------------------------------------------------------------
004400121106       // Definizione aree dati.
004500091223       //--------------------------------------------------------------
004600091223
004700091223
004800140724       //--------------------------------------------------------------
004900140724       // Definizione strutture dati.
005000140724       //--------------------------------------------------------------
005100091223
005200160802
005300091223       //--------------------------------------------------------------
005400121106       // Definizione variabili.
005500091223       //--------------------------------------------------------------
005600160921      /COPY GAITRASRC/SRCPROTOPI,UBISVPWDR
005700121106
005800121106       // Campi di comodo
005900161017     D wISVPWDTIP      s                   inz like(iISVPWDTIP)
006000160802     D wTypeLogin      s              3a   inz
006100161017     D wSQL            s           1024a   inz varying
006200160802     D wCount          s              1s 0 inz
006300160805     D wApi            c                   const('''')
006400091223
006500160802
006600091223       //--------------------------------------------------------------
006700160603       // Definizione costanti.
006800091223       //--------------------------------------------------------------
006900130307
007000160531
007100160603       //--------------------------------------------------------------
007200160603       // Definizione prototipi procedure.
007300160603       //--------------------------------------------------------------
007400160603
007500160603
007600091223       //--------------------------------------------------------------
007700121106       // Definizione key-list.
007800091223       //--------------------------------------------------------------
007900091223
008000091223
008100091223       //--------------------------------------------------------------
008200121107       // Definizione parametri procedura.
008300091223       //--------------------------------------------------------------
008400091223
008500091223     c     *Entry        plist
008600160916     c                   parm                    iISVPWDUTE
008700160916     c                   parm                    iISVPWDSUN
008800160916     c                   parm                    iISVPWDISV
008900161017     c                   parm                    iISVPWDTIP
009000160916     c                   parm                    iISVPWDPWD
009100160916     c                   parm                    iISVPWDLANG
009200160923     c                   parm                    oISVPWDEsito
009300161108     c                   parm                    oISVPWDUTE
009400161108     c                   parm                    oISVPWDSUN
009500160923     c                   parm                    oISVPWDTLE
009600160923     c                   parm                    oISVPWDRSP
009700160923     c                   parm                    oISVPWDEML
009800160923     c                   parm                    oISVPWDTEL
009900160923     c                   parm                    oISVPWDMsg
010000091223
010100130307      /free
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // M A I N - L I N E
010500091223       //--------------------------------------------------------------
010600140806
010700160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
010800160805       Monitor;
010900160805
011000130307       // Operazioni iniziali?
011100130307       exsr sr_RoutInz;
011200130307
011300160802       // Eseguo verifica credenziali
011400160802       exsr ChkUsrPwd;
011500091223
011600160804       // Operazioni finali
011700160804       exsr sr_RoutEnd;
011800160804
011900160805       // Fine monitoring
012000160805       On-Error;
012100160920          oISVPWDEsito = *zeros;
012200160921          exsr exeUscita;
012300160805       Endmon;
012400160805
012500160920       // Uscita
012600160921       exsr exeUscita;
012700160802
012800160802
012900160601       //--------------------------------------------------------------
013000160601       //
013100160601       //--------------------------------------------------------------
013200160601       BEGSR  *inzsr;
013300160601
013400160601         // Inizializzazioni parametri esecuzione comandi sql
013500160802         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013600160601
013700160601       ENDSR;
013800091223
013900160802
014000091223       //--------------------------------------------------------------
014100121107       // Operazioni iniziali.
014200091223       //--------------------------------------------------------------
014300091223       BEGSR  sr_RoutInz;
014400160601
014500160920         // Inizializzo l'esito a errore generico
014600160920         oISVPWDEsito = *zeros;
014700160608
014800160608         // Controllo i parametri di input
014900160608         exsr sr_CtrlPIn;
015000160202
015100160202       ENDSR;
015200160802
015300160804
015400160202       //--------------------------------------------------------------
015500160202       // Controllo i parametri di input
015600160202       //--------------------------------------------------------------
015700160202       BEGSR  sr_CtrlPIn;
015800140724
015900160802         // Come utente di default considero l'UTENTE ma se passato prevale lo STRATEGI-USER-NBR
016000160916         if iISVPWDSUN <> *blanks;
016100160802            wTypeLogin = 'SGI';
016200160802         else;
016300160802            wTypeLogin = 'UTE';
016400160802         endif;
016500161017
016600161017         // Se non indicato espressamente il Sottotipo Servizio Interbet assumo 'PWD'
016700161017         if iISVPWDTIP = *blanks;
016800161017            wISVPWDTIP = 'PWD';
016900161017         else;
017000161017            wISVPWDTIP = iISVPWDTIP;
017100161017         endif;
017200160804
017300160804         // Maiuscolizzo il parametro relatovo alla lingua
017400160916         select;
017500160916           when iISVPWDLANG = *blanks OR
017600160916                iISVPWDLANG = 'it'    OR
017700160916                iISVPWDLANG = 'It'    OR
017800160916                iISVPWDLANG = 'iT';
017900160916                iISVPWDLANG = 'IT';
018000160916           when iISVPWDLANG = 'en'    OR
018100160916                iISVPWDLANG = 'En'    OR
018200160916                iISVPWDLANG = 'eN';
018300160916                iISVPWDLANG = 'EN';
018400160916           other;
018500160916                iISVPWDLANG = 'EN';
018600160804         endsl;
018700091223
018800091223       ENDSR;
018900121106
019000160802
019100091223       //--------------------------------------------------------------
019200160217       // Esecuzione
019300091223       //--------------------------------------------------------------
019400160802       BEGSR  ChkUsrPwd;
019500160531
019600160802         select;
019700160802          when wTypeLogin = 'SGI';
019800161108            wSQL = 'select count(*), +
019900161223                    min(ABLUTE), min(ABLSUN), min(VSSTLE), +
020000161223                    min(ABLRSP), min(ABLEML), min(ABLTEL)  +
020100160923                    from tiabl00f join tivss00f on +
020200160923                    ablsun = vsssun and ablisv = vssisv +
020300160923                    where +
020400160923                    ABLSUN = ' + wApi + %trim(iISVPWDSUN) + wApi + ' and +
020500160923                    ABLISV = ' + wApi + iISVPWDISV        + wApi + ' and +
020600161017                    ABLTIP = ' + wApi + wISVPWDTIP        + wApi + ' and +
020700160923                    ABLPWD = ' + wApi + %trim(iISVPWDPWD) + wApi + ' and +
020800160923                    VSSSUN = ' + wApi + %trim(iISVPWDSUN) + wApi + ' and +
020900160923                    VSSISV = ' + wApi + iISVPWDISV        + wApi + ' and +
021000161108                    VSSVAT = ' + wApi + ' '               + wApi + ' and +
021100160923                    (select DC_YYMD_DEC from DATECNV0F +
021200160923                     where DC_DATE = curdate()) between VSSDDE and VSSDSC +
021300160923                    for read only';
021400160916
021500160916          when wTypeLogin = 'UTE';
021600161108            wSQL = 'select count(*), +
021700161223                    min(ABLUTE), min(ABLSUN), min(VSSTLE), +
021800161223                    min(ABLRSP), min(ABLEML), min(ABLTEL)  +
021900160923                    from tiabl00f join tivss00f on +
022000160923                    ''0''!!ablute = vssksu and ablisv = vssisv +
022100160923                    where +
022200160923                    ABLUTE = ' + wApi + %trim(iISVPWDUTE) + wApi + ' and +
022300160916                    ABLISV = ' + wApi + iISVPWDISV        + wApi + ' and +
022400161017                    ABLTIP = ' + wApi + wISVPWDTIP        + wApi + ' and +
022500160916                    ABLPWD = ' + wApi + %trim(iISVPWDPWD) + wApi + ' and +
022600160923                    VSSKSU = ' + wApi + '0' + %trim(iISVPWDUTE) + wApi + ' and +
022700160923                    VSSISV = ' + wApi + iISVPWDISV              + wApi + ' and +
022800161108                    VSSVAT = ' + wApi + ' '               + wApi + ' and +
022900160809                    (select DC_YYMD_DEC from DATECNV0F +
023000160923                     where DC_DATE = curdate()) between VSSDDE and VSSDSC +
023100160809                    for read only';
023200160802         endsl;
023300160802
023400160531         // Dichiarazione cursore
023500160802         exec sql   prepare S0   from :wSQL;
023600160531         exec sql   declare C0   cursor   for S0;
023700160802
023800160531         // Apro il cursore
023900160531         exec sql open C0;
024000160802
024100160923         // Leggo i dati
024200160923         exec sql
024300161108             fetch next from C0 into :wCount,
024400161108                                     :oISVPWDUTE, :oISVPWDSUN,
024500161108                                     :oISVPWDTLE, :oISVPWDRSP,
024600160923                                     :oISVPWDEML, :oISVPWDTEL;
024700160923
024800160923         // Se 1 record trovato => login riuscito
024900160916         if sqlCode = 0;
025000160916            if wCount  = 1;
025100160916               oISVPWDEsito = 1;
025200160916            else;
025300160916               oISVPWDEsito = 2;
025400160916            endif;
025500160916         endif;
025600160916
025700160916         if oISVPWDEsito <> 1;
025800161108            clear oISVPWDUTE;
025900161108            clear oISVPWDSUN;
026000160923            clear oISVPWDTLE;
026100160923            clear oISVPWDRSP;
026200160923            clear oISVPWDEML;
026300160923            clear oISVPWDTEL;
026400160916         endif;
026500160916
026600160217
026700160217       ENDSR;
026800160921
026900160921
027000160921       //--------------------------------------------------------------
027100160921       // Operazioni finali.
027200160921       //--------------------------------------------------------------
027300160921       BEGSR  sr_RoutEnd;
027400160921
027500160921         // Imposto il messaggio di esito in lingua
027600160921         select;
027700160921            when iISVPWDLANG = 'IT';
027800160921                 select;
027900160921                    when oISVPWDEsito = *zeros;
028000160921                       oISVPWDMsg = 'Login fallito.';
028100160921                    when oISVPWDEsito = 1;
028200160921                       oISVPWDMsg = 'Login effettuato.';
028300160921                    when oISVPWDEsito = 2;
028400160921                       oISVPWDMsg = 'Utente/password non validi.';
028500160921                 endsl;
028600160921
028700160921            when iISVPWDLANG = 'EN';
028800160921                 select;
028900160921                    when oISVPWDEsito = *zeros;
029000160921                       oISVPWDMsg = 'Login failed.';
029100160921                    when oISVPWDEsito = 1;
029200160921                       oISVPWDMsg = 'Login successful.';
029300160921                    when oISVPWDEsito = 2;
029400160921                       oISVPWDMsg = 'User/password not valid.';
029500160921                 endsl;
029600160921         endsl;
029700160921
029800160921       ENDSR;
029900160921
030000160921
030100160921       //--------------------------------------------------------------
030200160921       // Uscita
030300160921       //--------------------------------------------------------------
030400160921       BEGSR exeUscita;
030500160921
030600160921         // Chiusura cursore
030700160921         exec sql close C0;
030800160921
030900160921         return;
031000160921
031100160921       ENDSR;
031200091223
031300160802
031400091223      /end-free
