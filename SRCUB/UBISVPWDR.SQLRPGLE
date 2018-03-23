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
001700170508     **      CRTPGM PGM(GAITRAOBJ/UBISVPWDR) ACTGRP(*CALLER) TGTRLS(*CURRENT)
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
006400170508
006500170508     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
006600170508     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
006700091223
006800160802
006900091223       //--------------------------------------------------------------
007000160603       // Definizione costanti.
007100091223       //--------------------------------------------------------------
007200130307
007300160531
007400160603       //--------------------------------------------------------------
007500160603       // Definizione prototipi procedure.
007600160603       //--------------------------------------------------------------
007700160603
007800160603
007900091223       //--------------------------------------------------------------
008000121106       // Definizione key-list.
008100091223       //--------------------------------------------------------------
008200091223
008300091223
008400091223       //--------------------------------------------------------------
008500121107       // Definizione parametri procedura.
008600091223       //--------------------------------------------------------------
008700091223
008800091223     c     *Entry        plist
008900160916     c                   parm                    iISVPWDUTE
009000160916     c                   parm                    iISVPWDSUN
009100160916     c                   parm                    iISVPWDISV
009200161017     c                   parm                    iISVPWDTIP
009300160916     c                   parm                    iISVPWDPWD
009400160916     c                   parm                    iISVPWDLANG
009500160923     c                   parm                    oISVPWDEsito
009600161108     c                   parm                    oISVPWDUTE
009700161108     c                   parm                    oISVPWDSUN
009800160923     c                   parm                    oISVPWDTLE
009900160923     c                   parm                    oISVPWDRSP
010000160923     c                   parm                    oISVPWDEML
010100160923     c                   parm                    oISVPWDTEL
010200160923     c                   parm                    oISVPWDMsg
010300091223
010400130307      /free
010500091223
010600091223       //--------------------------------------------------------------
010700121106       // M A I N - L I N E
010800091223       //--------------------------------------------------------------
010900140806
011000160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
011100160805       Monitor;
011200160805
011300130307       // Operazioni iniziali?
011400130307       exsr sr_RoutInz;
011500130307
011600160802       // Eseguo verifica credenziali
011700160802       exsr ChkUsrPwd;
011800091223
011900160804       // Operazioni finali
012000160804       exsr sr_RoutEnd;
012100160804
012200160805       // Fine monitoring
012300160805       On-Error;
012400160920          oISVPWDEsito = *zeros;
012500160921          exsr exeUscita;
012600160805       Endmon;
012700160805
012800160920       // Uscita
012900160921       exsr exeUscita;
013000160802
013100160802
013200160601       //--------------------------------------------------------------
013300160601       //
013400160601       //--------------------------------------------------------------
013500160601       BEGSR  *inzsr;
013600160601
013700160601         // Inizializzazioni parametri esecuzione comandi sql
013800160802         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013900160601
014000160601       ENDSR;
014100091223
014200160802
014300091223       //--------------------------------------------------------------
014400121107       // Operazioni iniziali.
014500091223       //--------------------------------------------------------------
014600091223       BEGSR  sr_RoutInz;
014700160601
014800160920         // Inizializzo l'esito a errore generico
014900160920         oISVPWDEsito = *zeros;
015000160608
015100160608         // Controllo i parametri di input
015200160608         exsr sr_CtrlPIn;
015300160202
015400160202       ENDSR;
015500160802
015600160804
015700160202       //--------------------------------------------------------------
015800160202       // Controllo i parametri di input
015900160202       //--------------------------------------------------------------
016000160202       BEGSR  sr_CtrlPIn;
016100140724
016200160802         // Come utente di default considero l'UTENTE ma se passato prevale lo STRATEGI-USER-NBR
016300160916         if iISVPWDSUN <> *blanks;
016400160802            wTypeLogin = 'SGI';
016500160802         else;
016600160802            wTypeLogin = 'UTE';
016700160802         endif;
016800161017
016900161017         // Se non indicato espressamente il Sottotipo Servizio Interbet assumo 'PWD'
017000161017         if iISVPWDTIP = *blanks;
017100161017            wISVPWDTIP = 'PWD';
017200161017         else;
017300161017            wISVPWDTIP = iISVPWDTIP;
017400161017         endif;
017500160804
017600160804         // Maiuscolizzo il parametro relatovo alla lingua
017700160916         select;
017800170508           when iISVPWDLANG = 'IT';
017900170508           when iISVPWDLANG = 'EN';
018000170508           when iISVPWDLANG = *blanks;
018100170508                iISVPWDLANG = 'IT';
018200170508           other;
018300170508                iISVPWDLANG = %xlate(Lower:Upper:iISVPWDLANG);
018400170508         endsl;
018500170508         if iISVPWDLANG = 'IT' OR iISVPWDLANG = 'EN';
018600170508         else;
018700170508            iISVPWDLANG = 'EN';
018800170508         endif;
018900091223
019000091223       ENDSR;
019100121106
019200160802
019300091223       //--------------------------------------------------------------
019400160217       // Esecuzione
019500091223       //--------------------------------------------------------------
019600160802       BEGSR  ChkUsrPwd;
019700160531
019800160802         select;
019900160802          when wTypeLogin = 'SGI';
020000161108            wSQL = 'select count(*), +
020100161223                    min(ABLUTE), min(ABLSUN), min(VSSTLE), +
020200161223                    min(ABLRSP), min(ABLEML), min(ABLTEL)  +
020300160923                    from tiabl00f join tivss00f on +
020400160923                    ablsun = vsssun and ablisv = vssisv +
020500160923                    where +
020600160923                    ABLSUN = ' + wApi + %trim(iISVPWDSUN) + wApi + ' and +
020700160923                    ABLISV = ' + wApi + iISVPWDISV        + wApi + ' and +
020800161017                    ABLTIP = ' + wApi + wISVPWDTIP        + wApi + ' and +
020900160923                    ABLPWD = ' + wApi + %trim(iISVPWDPWD) + wApi + ' and +
021000160923                    VSSSUN = ' + wApi + %trim(iISVPWDSUN) + wApi + ' and +
021100160923                    VSSISV = ' + wApi + iISVPWDISV        + wApi + ' and +
021200161108                    VSSVAT = ' + wApi + ' '               + wApi + ' and +
021300160923                    (select DC_YYMD_DEC from DATECNV0F +
021400160923                     where DC_DATE = curdate()) between VSSDDE and VSSDSC +
021500160923                    for read only';
021600160916
021700160916          when wTypeLogin = 'UTE';
021800161108            wSQL = 'select count(*), +
021900161223                    min(ABLUTE), min(ABLSUN), min(VSSTLE), +
022000161223                    min(ABLRSP), min(ABLEML), min(ABLTEL)  +
022100160923                    from tiabl00f join tivss00f on +
022200160923                    ''0''!!ablute = vssksu and ablisv = vssisv +
022300160923                    where +
022400160923                    ABLUTE = ' + wApi + %trim(iISVPWDUTE) + wApi + ' and +
022500160916                    ABLISV = ' + wApi + iISVPWDISV        + wApi + ' and +
022600161017                    ABLTIP = ' + wApi + wISVPWDTIP        + wApi + ' and +
022700160916                    ABLPWD = ' + wApi + %trim(iISVPWDPWD) + wApi + ' and +
022800160923                    VSSKSU = ' + wApi + '0' + %trim(iISVPWDUTE) + wApi + ' and +
022900160923                    VSSISV = ' + wApi + iISVPWDISV              + wApi + ' and +
023000161108                    VSSVAT = ' + wApi + ' '               + wApi + ' and +
023100160809                    (select DC_YYMD_DEC from DATECNV0F +
023200160923                     where DC_DATE = curdate()) between VSSDDE and VSSDSC +
023300160809                    for read only';
023400160802         endsl;
023500160802
023600160531         // Dichiarazione cursore
023700160802         exec sql   prepare S0   from :wSQL;
023800160531         exec sql   declare C0   cursor   for S0;
023900160802
024000160531         // Apro il cursore
024100160531         exec sql open C0;
024200160802
024300160923         // Leggo i dati
024400160923         exec sql
024500161108             fetch next from C0 into :wCount,
024600161108                                     :oISVPWDUTE, :oISVPWDSUN,
024700161108                                     :oISVPWDTLE, :oISVPWDRSP,
024800160923                                     :oISVPWDEML, :oISVPWDTEL;
024900160923
025000160923         // Se 1 record trovato => login riuscito
025100160916         if sqlCode = 0;
025200160916            if wCount  = 1;
025300160916               oISVPWDEsito = 1;
025400160916            else;
025500160916               oISVPWDEsito = 2;
025600160916            endif;
025700160916         endif;
025800160916
025900160916         if oISVPWDEsito <> 1;
026000161108            clear oISVPWDUTE;
026100161108            clear oISVPWDSUN;
026200160923            clear oISVPWDTLE;
026300160923            clear oISVPWDRSP;
026400160923            clear oISVPWDEML;
026500160923            clear oISVPWDTEL;
026600160916         endif;
026700160916
026800160217
026900160217       ENDSR;
027000160921
027100160921
027200160921       //--------------------------------------------------------------
027300160921       // Operazioni finali.
027400160921       //--------------------------------------------------------------
027500160921       BEGSR  sr_RoutEnd;
027600160921
027700160921         // Imposto il messaggio di esito in lingua
027800160921         select;
027900160921            when iISVPWDLANG = 'IT';
028000160921                 select;
028100160921                    when oISVPWDEsito = *zeros;
028200160921                       oISVPWDMsg = 'Login fallito.';
028300160921                    when oISVPWDEsito = 1;
028400160921                       oISVPWDMsg = 'Login effettuato.';
028500160921                    when oISVPWDEsito = 2;
028600160921                       oISVPWDMsg = 'Utente/password non validi.';
028700160921                 endsl;
028800160921
028900160921            when iISVPWDLANG = 'EN';
029000160921                 select;
029100160921                    when oISVPWDEsito = *zeros;
029200160921                       oISVPWDMsg = 'Login failed.';
029300160921                    when oISVPWDEsito = 1;
029400160921                       oISVPWDMsg = 'Login successful.';
029500160921                    when oISVPWDEsito = 2;
029600160921                       oISVPWDMsg = 'User/password not valid.';
029700160921                 endsl;
029800160921         endsl;
029900160921
030000160921       ENDSR;
030100160921
030200160921
030300160921       //--------------------------------------------------------------
030400160921       // Uscita
030500160921       //--------------------------------------------------------------
030600160921       BEGSR exeUscita;
030700160921
030800160921         // Chiusura cursore
030900160921         exec sql close C0;
031000160921
031100160921         return;
031200160921
031300160921       ENDSR;
031400091223
031500160802
031600091223      /end-free
