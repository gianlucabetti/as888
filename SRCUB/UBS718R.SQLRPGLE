000100170906     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170906     H DECEDIT('0,') OPTION(*NODEBUGIO)
000300091223
000400091223       //--------------------------------------------------------------
000500121106       // Dichiarazione file.
000600091223       //--------------------------------------------------------------
000700170508
000800170508
000900170508       //--------------------------------------------------------------
001000170508       // Definizione prototipi
001100170508       //--------------------------------------------------------------
001200170508      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
001300170509      /COPY GAITRASRC/SRCPROTOPR,UBISNUM
001400160218
001500160531
001600091223       //--------------------------------------------------------------
001700121106       // Definizione costanti.
001800091223       //--------------------------------------------------------------
001900170906     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
002000170906     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
002100100302
002200091223
002300091223       //--------------------------------------------------------------
002400121106       // Definizione schiere.
002500091223       //--------------------------------------------------------------
002600091223
002700091223
002800091223       //--------------------------------------------------------------
002900121106       // Definizione aree dati.
003000091223       //--------------------------------------------------------------
003100091223
003200091223
003300140724       //--------------------------------------------------------------
003400140724       // Definizione strutture dati.
003500140724       //--------------------------------------------------------------
003600170906     D  DVPC         e ds                  inz
003700170906     D  EDIVABDS     e ds                  inz extname(EDIVAB0F)
003800170906     D  UBSGU1DSA    e ds                  inz qualified
003900170906     D  UBBRTETIDS   e ds                  inz qualified
004000091223
004100160802
004200091223       //--------------------------------------------------------------
004300121106       // Definizione variabili.
004400091223       //--------------------------------------------------------------
004500170906      /COPY GAITRASRC/SRCPROTOPI,UBS718R
004600170508      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004700170509      /COPY GAITRASRC/SRCPROTOPI,UBISNUM
004800170906      /COPY GAITRASRC/SRCPROTOPI,TIS713R
004900170906      /COPY GAITRASRC/SRCPROTOPI,UBSGU1R
005000121106
005100121106       // Campi di comodo
005200170906     D Lingua          s              2a   inz
005300170508     D wLIB§           s             11a   inz
005400161017     D wSQL            s           1024a   inz varying
005500170511     D wWHERE_1        s            256a   inz varying
005600160802     D wCount          s              1s 0 inz
005700160805     D wApi            c                   const('''')
005800170906     D dftOUTQ         s             10A
005900170906     D dftTIPF_ETI     s              2A
006000170906     D dftTIP_TRASF    s              2A
006100170928     D timestamp01     s             26A   inz
006200170928     D timestamp02     s             26A   inz
006300170928     D timestampDIFF   s              7S 0 inz
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
008500170906     C     *Entry        plist
008600170906     C                   parm                    iUBS718LANG
008700170908     C                   parm                    iUBS718FGS
008800170906     C                   parm                    iUBS718CCM
008900170906     C                   parm                    iUBS718REF
009000170906     C                   parm                    iUBS718REFTYP
009100170906     C                   parm                    iUBS718LBLTYP
009200170906     C                   parm                    iUBS718VPC
009300170906     C                   parm                    oUBS718Esito
009400170906     C                   parm                    oUBS718MSG
009500170925     C                   parm                    oUBS718NCL
009600170925     C                   parm                    oUBS718LNP
009700170925     C                   parm                    oUBS718LNA
009800170925     C                   parm                    oUBS718NRS
009900170925     C                   parm                    oUBS718ZNC
010000170925     C                   parm                    oUBS718NCD
010100170925     C                   parm                    oUBS718NCA
010200170906     C                   parm                    oUBS718DLEN
010300170906     C                   parm                    oUBS718DATI
010400091223
010500130307      /free
010600091223
010700091223       //--------------------------------------------------------------
010800121106       // M A I N - L I N E
010900091223       //--------------------------------------------------------------
011000140806
011100160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
011200160805       Monitor;
011300160805
011400130307       // Operazioni iniziali?
011500170508       exsr RoutInz;
011600130307
011700170508       // Eseguo reperimento ID spedizione
011800170906       exsr RtvSped;
011900170906
012000160804
012100160805       // Fine monitoring
012200160805       On-Error;
012300170906          oUBS718Esito = -1;
012400160921          exsr exeUscita;
012500160805       Endmon;
012600160805
012700160920       // Uscita
012800170906       oUBS718Esito = 0;
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
013900170508
014000170508         // Verifico il sistema AS/400 corrente
014100170906         wLIB§ = 'FILTRAGRU/';
014200170508         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
014300170508            if %subst(PiOutSysNeta:1:6) <> 'SETRAS';
014400170906               wLIB§ = 'FILTRAGRPF/';
014500170508           endif;
014600170508         endif;
014700170928
014800170928         // Caricamento tabelle di procedura
014900170928         exsr CarTab;
015000160601
015100160601       ENDSR;
015200091223
015300160802
015400091223       //--------------------------------------------------------------
015500121107       // Operazioni iniziali.
015600091223       //--------------------------------------------------------------
015700170726       BEGSR  RoutInz;
015800160601
015900160920         // Inizializzo l'esito a errore generico
016000170906         oUBS718Esito = -1;
016100170511
016200170515         // Inizializzazione campi di work e di output
016300170906         clear wSQL;
016400170906         clear wWHERE_1;
016500170906         clear EDIVABDS;
016600170906         clear oUBS718MSG;
016700170925         clear oUBS718NCL;
016800170925         clear oUBS718LNP;
016900170925         clear oUBS718LNA;
017000170925         clear oUBS718NRS;
017100170925         clear oUBS718ZNC;
017200170925         clear oUBS718NCD;
017300170925         clear oUBS718NCA;
017400170906         clear oUBS718DLEN;
017500170906         clear oUBS718DATI;
017600160608
017700160608         // Controllo i parametri di input
017800170508         exsr CtrlPIn;
017900160202
018000160202       ENDSR;
018100160802
018200160804
018300160202       //--------------------------------------------------------------
018400160202       // Controllo i parametri di input
018500160202       //--------------------------------------------------------------
018600170508       BEGSR  CtrlPIn;
018700170508
018800170906         // Verifica lingua indicata
018900170906         Lingua = %xlate(Lower:Upper:iUBS718LANG);
019000170906         select;
019100170906           when Lingua = 'IT';
019200170906           when Lingua = 'EN';
019300170906           other;
019400170906             Lingua = 'EN';
019500170906         endsl;
019600170908
019700170908         // I parametri fondamentali devono essere valorizzati
019800170908         if iUBS718FGS = *zeros;
019900170908            oUBS718Esito = -2;
020000170908            exsr exeUscita;
020100170908         endif;
020200170906
020300170508         // I parametri fondamentali devono essere valorizzati
020400170906         if iUBS718CCM = *zeros;
020500170906            oUBS718Esito = -2;
020600170906            exsr exeUscita;
020700170906         endif;
020800170906
020900170906         if iUBS718REF = *blanks OR %trim(iUBS718REF) = *all'0';
021000170906            oUBS718Esito = -2;
021100170508            exsr exeUscita;
021200160802         endif;
021300170511
021400170906         // Verifica il tipo di riferimento indicato
021500170906         if iUBS718REFTYP = 'N' OR
021600170906            iUBS718REFTYP = 'A';
021700170511         else;
021800170906            oUBS718Esito = -2;
021900170511            exsr exeUscita;
022000170511         endif;
022100170906
022200170906         // Verifica il tipo di output etichetta richiesto
022300170906         if iUBS718LBLTYP = 'Z' OR
022400170906            iUBS718LBLTYP = 'B' OR
022500170906            iUBS718LBLTYP = 'P' OR
022600170906            iUBS718LBLTYP = 'T';
022700170906         else;
022800170906            oUBS718Esito = -2;
022900170906            exsr exeUscita;
023000170906         endif;
023100170906
023200170906         // Se richiesto tipo numerico => verifico numericità riferimento
023300170906         if iUBS718REFTYP = 'N';
023400170906            PiStr = iUBS718REF;
023500170511            exsr chkNum;
023600170511            if PiInt = *on;
023700170511            else;
023800170906               oUBS718Esito = -2;
023900170906               exsr exeUscita;
024000170511            endif;
024100170906         endif;
024200170906
024300170906         if iUBS718VPC = *blanks;
024400170906            oUBS718Esito = -2;
024500170906            exsr exeUscita;
024600170906         endif;
024700091223
024800091223       ENDSR;
024900121106
025000160802
025100091223       //--------------------------------------------------------------
025200160217       // Esecuzione
025300091223       //--------------------------------------------------------------
025400170508       BEGSR  RtvSped;
025500170906
025600170906         // Compondo l'istruzione SQL di "base"
025700170906         wSQL =
025800170906           'select * FROM ' + %trim(wLIB§) + 'EDIVAB0F ' +
025900170911           'where (VABFGS = 998 or VABFGS = ' + %char(iUBS718FGS) + ') ' +
026000170911           'and VABCCM = ' + %char(iUBS718CCM) + ' and ' + wWHERE_1;
026100170906
026200170906         // Compongo clausola WHERE in relazione al tipo ricerca richiesto
026300170906         exsr chkTip;
026400170906
026500170906         // Ricerca spedizione con parametri del tentativo corrente
026600170906         exsr srcSped;
026700170906
026800170906         // Verifico esito ricerca spedizione
026900170906         select;
027000170906           when wCount = 1;
027100170925              // Valorizzo i dati relativi ai segnacolli spedizione in output
027200170925              oUBS718NCL = VABNCL;
027300170925              oUBS718LNP = VABLNP;
027400170925              oUBS718LNA = VABLNA;
027500170925              oUBS718NRS = VABNRS;
027600170925              oUBS718ZNC = VABZNC;
027700170925              oUBS718NCD = VABNCD;
027800170925              oUBS718NCA = VABNCD+VABNCL-1;
027900170906              // Eseguo generazione etichette segnacollo
028000170906              exsr RtvLabel;
028100170906           when wCount = *zeros;
028200170906              oUBS718Esito = -3;
028300170906              exsr exeUscita;
028400170906           when wCount > *zeros;
028500170906              oUBS718Esito = -4;
028600170906              exsr exeUscita;
028700170906         endsl;
028800160217
028900160217       ENDSR;
029000170906
029100170906
029200170906       //--------------------------------------------------------------
029300170906       // Operazioni propedeutiche rispetto al tipo ricerca richiesto
029400170906       //--------------------------------------------------------------
029500170906       BEGSR chkTip;
029600170906
029700170906         clear wWHERE_1;
029800170906         select;
029900170906          when iUBS718REFTYP = 'N';
030000170906             wWHERE_1 = ' VABRMN = ' + %trim(iUBS718REF);
030100170906          when iUBS718REFTYP = 'A';
030200170906             wWHERE_1 = ' VABRMA = ' + wApi + %trim(iUBS718REF) + wApi;
030300170906         endsl;
030400170906         wSQL = wSQL + wWHERE_1;
030500170906
030600170906       ENDSR;
030700170509
030800170509
030900170509       //--------------------------------------------------------------
031000170509       // Cerca spedizione
031100170509       //--------------------------------------------------------------
031200170509       BEGSR  srcSped;
031300170509
031400170906         clear wCount;
031500170928
031600170928         // Se istruzione SQL formata => la eseguo
031700170928         if wSQL <> *blanks;
031800170928
031900170928            // Salvo il timestamp "prima"
032000170928            //exec sql   set :timestamp01 = CHAR(CURRENT TIMESTAMP);
032100170515
032200170906            // Dichiarazione cursore
032300170906            exec sql   prepare S0   from :wSQL;
032400170906            exec sql   declare C0   cursor   for S0;
032500170906
032600170906            // Apro il cursore
032700170906            exec sql open C0;
032800170906
032900170906            // Leggo i dati
033000170906            dow sqlcode = *zeros;
033100170906               exec sql
033200170906                    fetch next from C0 into :EDIVABDS;
033300170906
033400170906               if sqlcode <> 100;
033500170906                  wCount = wCount + 1;
033600170906               endif;
033700170906            enddo;
033800170906
033900170906            // Chiusura cursore
034000170906            exec sql close C0;
034100170928
034200170928            // Salvo il timestamp "dopo"
034300170928            //exec sql   set :timestamp02 = CHAR(CURRENT TIMESTAMP);
034400170928
034500170928            // Calcolo la "performance"
034600170928            //exec sql   set :timestampDIFF = TIMESTAMPDIFF (1,
034700170928            //  CHAR(TIMESTAMP(:timestamp02) - TIMESTAMP(:timestamp01)));
034800170928
034900170515         endif;
035000170509
035100170509       ENDSR;
035200170906
035300170906
035400170906       //--------------------------------------------------------------
035500170906       // Caricamento tabelle di procedura
035600170906       //--------------------------------------------------------------
035700170906     C     CarTab        begsr
035800170906     C*
035900170906     C* Reperimento *OUT di DEFAULT "VASWS"
036000170906     C                   eval      iS713TBECOD = 'VPR'
036100170906     C                   eval      iS713TBEKE1 = '*VASWS'
036200170906     C                   eval      iS713TBEKE2 = '*DFTOUTQ'
036300170906     C                   eval      iS713TBELIN = *blanks
036400170906     C                   eval      iS713TBESIF = *blanks
036500170906     C                   call      'TIS713R'
036600170906     C                   parm                    iS713TBECOD
036700170906     C                   parm                    iS713TBEKE1
036800170906     C                   parm                    iS713TBEKE2
036900170906     C                   parm                    iS713TBELIN
037000170906     C                   parm                    iS713TBESIF
037100170906     C                   parm                    oS713TBEUNI
037200170906     C                   parm                    oS713ESITO
037300170906     C*
037400170906     C                   if        oS713ESITO = '1'
037500170906     C                   eval      dftOUTQ = %trim(oS713TBEUNI)
037600170906     C                   else
037700170906     C                   eval      dftOUTQ = 'VASSPOOL'
037800170906     C                   endif
037900170906     C*
038000170906     C* Reperimento TIPO FILE ETICHETTE SEGNACOLLO di DEFAULT "VASWS"
038100170906     C                   eval      iS713TBECOD = 'VPR'
038200170906     C                   eval      iS713TBEKE1 = '*VASWS'
038300170906     C                   eval      iS713TBEKE2 = '*TIPF-ETI'
038400170906     C                   eval      iS713TBELIN = *blanks
038500170906     C                   eval      iS713TBESIF = *blanks
038600170906     C                   call      'TIS713R'
038700170906     C                   parm                    iS713TBECOD
038800170906     C                   parm                    iS713TBEKE1
038900170906     C                   parm                    iS713TBEKE2
039000170906     C                   parm                    iS713TBELIN
039100170906     C                   parm                    iS713TBESIF
039200170906     C                   parm                    oS713TBEUNI
039300170906     C                   parm                    oS713ESITO
039400170906     C*
039500170906     C                   if        oS713ESITO = '1'
039600170906     C                   eval      dftTIPF_ETI = %trim(oS713TBEUNI)
039700170906     C                   else
039800170906     C                   eval      dftTIPF_ETI = 'SG'
039900170906     C                   endif
040000170906     C*
040100170906     C* Reperimento TIPO TRASFERIMENTO OUTPUT di DEFAULT "VASWS"
040200170906     C                   eval      iS713TBECOD = 'VPR'
040300170906     C                   eval      iS713TBEKE1 = '*VASWS'
040400170906     C                   eval      iS713TBEKE2 = '*TIP-TRASF'
040500170906     C                   eval      iS713TBELIN = *blanks
040600170906     C                   eval      iS713TBESIF = *blanks
040700170906     C                   call      'TIS713R'
040800170906     C                   parm                    iS713TBECOD
040900170906     C                   parm                    iS713TBEKE1
041000170906     C                   parm                    iS713TBEKE2
041100170906     C                   parm                    iS713TBELIN
041200170906     C                   parm                    iS713TBESIF
041300170906     C                   parm                    oS713TBEUNI
041400170906     C                   parm                    oS713ESITO
041500170906     C*
041600170906     C                   if        oS713ESITO = '1'
041700170906     C                   eval      dftTIP_TRASF = %trim(oS713TBEUNI)
041800170906     C                   else
041900170906     C                   eval      dftTIP_TRASF = 'BV'
042000170906     C                   endif
042100170906     C*
042200170906     C                   endsr
042300170906     C*
042400170906
042500170906
042600170906       //--------------------------------------------------------------
042700170906       // Etichetta SEGNACOLLO
042800170906       //--------------------------------------------------------------
042900170906     C     RtvLabel      begsr
043000170907     C*
043100170907     C* Verifico che la bolla abbia tutti gli elementi necessari al Disk B
043200170907     C                   if        VABAAS <> *zeros AND
043300170907     C                             VABLNP <> *zeros AND
043400170907     C                             VABNRS <> *zeros AND
043500170907     C                             VABNSP <> *zeros AND
043600170907     C                             VABLNA <> *zeros AND
043700170907     C                             (VABNCD <> *zeros OR VABNCA <> *zeros)
043800170907     C                   else
043900170907     C                   eval      oUBS718Esito = -5
044000170907     C                   exsr      exeUscita
044100170907     C                   endif
044200170906     C*
044300170906     C* Inizializzazioni DS di procedure
044400170906     C                   clear                   UBBRTETIDS
044500170906     C                   clear                   UBSGU1IDS
044600170906     C                   clear                   UBSGU1DSA
044700170912     C*
044800170912     C* Se FGS bolla = FGS richeisto in input
044900170912     C                   if        VABFGS = iUBS718FGS
045000170912     C                   seton                                        55
045100170912     C                   else
045200170912     C                   setoff                                       55
045300170912     C                   endif
045400170906     C*
045500170906     C* Ridefinisco in DS le opzioni di esecuzione richieste in input
045600170906     C                   eval      DVPC = iUBS718VPC
045700170906     C*
045800170906     C* Valorizzo parametri per stampa segnacollo
045900170906     C                   select
046000170906     C                   when      §VPCSGF = 'P'
046100170906     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
046200170906     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
046300170906     C                   when      §VPCSGF = 'T'
046400170906     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
046500170906     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
046600170906     C                   when      §VPCSGF = 'Z'
046700170906     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
046800170906     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
046900171023     C                   when      §VPCSGF = 'L'
047000171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
047100171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
047200170906     C                   other
047300170906     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
047400170906     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
047500170906     C                   endsl
047600170906     C*
047700170906     C                   select
047800170906     C                   when      iUBS718LBLTYP = 'Z'                          * ASCII
047900170906     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'DATI'
048000170906     C                   when      iUBS718LBLTYP = 'B'                          * BASE64
048100170906     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
048200170906     C                   when      iUBS718LBLTYP = 'P'                          * BASE64
048300170906     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
048400170906     C                   when      iUBS718LBLTYP = 'T'                          * BASE64
048500170906     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
048600170906     C                   other
048700170906     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
048800170906     C                   endsl
048900170906     C*
049000170906     C                   eval      UBBRTETIDS.PIN_OUTQ  = dftOUTQ
049100170906     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
049200170906     C                   eval      UBBRTETIDS.PIN_OFSX  = %dec(§VPCOFX:8:3)
049300170906     C                   eval      UBBRTETIDS.PIN_OFSY  = %dec(§VPCOFY:8:3)
049400170906     C                   eval      UBBRTETIDS.PIN_BORDO = §VPCBRD
049500170906     C                   eval      UBBRTETIDS.PIN_LOGO  = §VPCPDL
049600170906     C                   eval      UBBRTETIDS.PIN_ROW   = §VPCROW
049700170906     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
049800170912     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
049900170912     C   55              eval      UBBRTETIDS.PIN_RST   = 'R'
050000170906     C*
050100170906     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
050200170906     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
050300170906     C                   eval      UBSGU1DSA.ISGU1AKSU  = %editc(iUBS718CCM:'X')
050400170906     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS718VPC
050500170906     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
050600170906     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
050700170906     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
050800170906     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
050900170906     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
051000170906     C*
051100170906     C* Valorizzo parametri per driver utilità segnacolli 1
051200170906     C                   eval      UBSGU1IDS.ISGU1ILIN  = Lingua
051300170906     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
051400170906     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
051500170906     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
051600170906     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
051700170906     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
051800170928
051900170928            // Salvo il timestamp "prima"
052000170928            //exec sql   set :timestamp01 = CHAR(CURRENT TIMESTAMP);
052100170928
052200170906     C*
052300170906     C* Chiamata al driver stampa e reperimento etichetta segnacolli
052400170906     C                   call      'UBSGU1R'
052500170906     C                   parm                    UBSGU1IDS
052600170906     C                   parm                    UBSGU1OLEN
052700170906     C                   parm                    UBSGU1OUT
052800170928
052900170928            // Salvo il timestamp "dopo"
053000170928            //exec sql   set :timestamp02 = CHAR(CURRENT TIMESTAMP);
053100170928
053200170928            // Calcolo la "performance"
053300170928            //exec sql   set :timestampDIFF = TIMESTAMPDIFF (1,
053400170928            //  CHAR(TIMESTAMP(:timestamp02) - TIMESTAMP(:timestamp01)));
053500170928
053600170906     C*
053700170906     C* Valorizzo sempre il messaggio di ritorno
053800170906     C                   eval      oUBS718MSG = UBSGU1IDS.OSGU1IMSG
053900170906     C*
054000170906     C* Se errore => DUMP e uscita con errore
054100170906     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
054200170906     C                   eval      oUBS718Esito = -5
054300170906     C                   exsr      exeUscita
054400170906     C*
054500170908     C* Se tutto OK
054600170906     C                   else
054700170908     C*
054800170908     C* Se etichetta segnacollo generata correttamente => "abilito" i dati spedizione
054900170908     C                   exsr      AbilBolla
055000170908     C*
055100170908     C* Infine recepisco output
055200170906     C                   eval      oUBS718DLEN = UBSGU1OLEN
055300170906     C                   eval      oUBS718DATI = UBSGU1OUT
055400170906     C                   endif
055500170906     C*
055600170906     C                   endsr
055700170908
055800170908
055900170908       //--------------------------------------------------------------
056000170908       // "abilita" Bolla in conferma
056100170908       //--------------------------------------------------------------
056200170908       BEGSR  AbilBolla;
056300170928
056400170928         // Salvo il timestamp "prima"
056500170928         //exec sql   set :timestamp01 = CHAR(CURRENT TIMESTAMP);
056600170908
056700170908         // Prima abilito estensioni VAT
056800170908         wSQL =
056900170908             ' update ' + %trim(wLIB§) + 'EDIVAT0F ' +
057000170908             ' set VATFGS = ' + %char(iUBS718FGS) + ' where ' +
057100170908             ' VATFGS = 998 and ' +
057200170908             ' VATCCM = ' + %char(VABCCM) + ' and ' +
057300170908             ' VATAAS = ' + %char(VABAAS) + ' and ' +
057400170908             ' VATLNP = ' + %char(VABLNP) + ' and ' +
057500170908             ' VATNRS = ' + %char(VABNRS) + ' and ' +
057600170908             ' VATNSP = ' + %char(VABNSP) ;
057700170908
057800170908         exec sql   execute immediate :wSQL;
057900170908
058000170908         // Verifico esito
058100170908         if sqlcode < *zeros;
058200170908            oUBS718Esito = -5;
058300170908            exsr exeUscita;
058400170908         endif;
058500170908
058600170908
058700170908         // Quindi abilito testata VAB
058800170908         wSQL =
058900170908             ' update ' + %trim(wLIB§) + 'EDIVAB0F ' +
059000170908             ' set VABFGS = ' + %char(iUBS718FGS) + ' where ' +
059100170908             ' VABFGS = 998 and ' +
059200170908             ' VABCCM = ' + %char(VABCCM) + ' and ' +
059300170908             ' VABAAS = ' + %char(VABAAS) + ' and ' +
059400170908             ' VABLNP = ' + %char(VABLNP) + ' and ' +
059500170908             ' VABNRS = ' + %char(VABNRS) + ' and ' +
059600170908             ' VABNSP = ' + %char(VABNSP) ;
059700170908
059800170908         exec sql   execute immediate :wSQL;
059900170908
060000170908         // Verifico esito
060100170908         if sqlcode < *zeros;
060200170908            oUBS718Esito = -5;
060300170908            exsr exeUscita;
060400170908         endif;
060500170928
060600170928         // Salvo il timestamp "dopo"
060700170928         //exec sql   set :timestamp02 = CHAR(CURRENT TIMESTAMP);
060800170928
060900170928         // Calcolo la "performance"
061000170928         //exec sql   set :timestampDIFF = TIMESTAMPDIFF (1,
061100170928         //  CHAR(TIMESTAMP(:timestamp02) - TIMESTAMP(:timestamp01)));
061200170908
061300170908       ENDSR;
061400170509
061500160921
061600160921       //--------------------------------------------------------------
061700160921       // Operazioni finali.
061800160921       //--------------------------------------------------------------
061900170906       BEGSR  RoutEnd;
062000160921
062100160921         // Imposto il messaggio di esito in lingua
062200160921         select;
062300170906            when Lingua = 'IT';
062400160921                 select;
062500170906                    when oUBS718Esito = *zeros;
062600170906                       oUBS718MSG = 'OK';
062700170906                    when oUBS718Esito = -1;
062800170906                       oUBS718MSG = 'Errore generico';
062900170906                    when oUBS718Esito = -2;
063000170906                       oUBS718MSG = 'Parametri input non validi';
063100170906                    when oUBS718Esito = -3;
063200170906                       oUBS718MSG = 'Spedizione non trovata';
063300170906                    when oUBS718Esito = -4;
063400170906                       oUBS718MSG = 'Spedizione non univoca';
063500170906                    when oUBS718Esito = -5;
063600170906                       oUBS718MSG = 'Errore generazione etichetta';
063700160921                 endsl;
063800160921
063900170906            when Lingua = 'EN';
064000160921                 select;
064100170906                    when oUBS718Esito = *zeros;
064200170906                       oUBS718MSG = 'OK';
064300170906                    when oUBS718Esito = -1;
064400170906                       oUBS718MSG = 'Generic Error';
064500170906                    when oUBS718Esito = -2;
064600170906                       oUBS718MSG = 'Input Parameters not valid';
064700170906                    when oUBS718Esito = -3;
064800170906                       oUBS718MSG = 'Shipment not Found';
064900170906                    when oUBS718Esito = -4;
065000170906                       oUBS718MSG = 'Shipment not Unique';
065100170906                    when oUBS718Esito = -5;
065200170906                       oUBS718MSG = 'Label genaration error';
065300160921                 endsl;
065400160921         endsl;
065500160921
065600160921       ENDSR;
065700170509
065800170509
065900170509       //--------------------------------------------------------------
066000170509       // Controllo numericità dato
066100170509       //--------------------------------------------------------------
066200170509       BEGSR chkNum;
066300170509
066400170509         // Imposto default separatore decimale
066500170509         PiDecChr = ',';
066600170509
066700170509         // Verifica numericità
066800170509         callp UBISNUM_Check(PiStr
066900170509                            :PiDecChr
067000170509                            :PiVal
067100170509                            :PiNum
067200170509                            :PiInt);
067300170509
067400170509       ENDSR;
067500170726
067600160921
067700160921
067800160921       //--------------------------------------------------------------
067900160921       // Uscita
068000160921       //--------------------------------------------------------------
068100160921       BEGSR exeUscita;
068200170508
068300170508         // Operazioni finali
068400170508         exsr RoutEnd;
068500160921
068600160921         // Chiusura cursore
068700160921         exec sql close C0;
068800160921
068900170906         *inlr = *on;
069000160921         return;
069100160921
069200160921       ENDSR;
069300091223
069400160802
069500091223      /end-free
