000100170727     H DECEDIT('0,') DATEDIT(*DMY.)
000200170727     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'TIO7') ACTGRP('BARTVAS')
000300091223
000400091223       //--------------------------------------------------------------
000500121106       // Dichiarazione file.
000600091223       //--------------------------------------------------------------
000700170727     Ftitas30c  if   e           k disk    extfile(wLibFile1)  usropn
000800170727     Ffiar531c  if   e           k disk    extfile(wLibFile2)  usropn
000900170728     Ffnspe01l  if   e           k disk    extfile(wLibFile3)  usropn
001000170731     Ffnacr01l  if   e           k disk    extfile(wLibFile4)  usropn
001100170731     Fcnaco00f  if   e           k disk    extfile(wLibFile5)  usropn
001200170731     Fcnind01l  if   e           k disk    extfile(wLibFile6)  usropn
001300170728     Ftivpi00f  uf a e             disk
001400170508
001500170508
001600170508       //--------------------------------------------------------------
001700170508       // Definizione prototipi
001800170508       //--------------------------------------------------------------
001900170508      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002000170509      /COPY GAITRASRC/SRCPROTOPR,UBISNUM
002100170731      /COPY GAITRASRC/SRCPROTOPR,UBBS10R
002200170727
002300170727       // -?Recupero nuovo Numero Prenotazione Ritiro
002400170727     D GetNPR          PR            10
002500170727     D  ParametroNPR                 10
002600091223
002700160802
002800091223       //--------------------------------------------------------------
002900121106       // Definizione variabili.
003000091223       //--------------------------------------------------------------
003100170508      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003200170509      /COPY GAITRASRC/SRCPROTOPI,UBISNUM
003300170731      /COPY GAITRASRC/SRCPROTOPI,UBBS10R
003400170728      /COPY GAITRASRC/SRCPROTOPI,UBB2ORMR
003500121106
003600170728     D FNVAOEIIR     e ds                  inz
003700170727     D dar5gen       e ds                  qualified inz
003800170727     D dar5emd       e ds                  qualified inz
003900170727
004000170727     D DS_SPED_BRT     ds                  qualified
004100170727     D   lnp                          3s 0
004200170727     D   nrs                          2s 0
004300170727     D   nsp                          7s 0
004400170728     D   sped                  1     12s 0
004500170727
004600170727     D wLibFile1       s             21a   inz
004700170727     D wLibFile2       s             21a   inz
004800170728     D wLibFile3       s             21a   inz
004900170731     D wLibFile4       s             21a   inz
005000170731     D wLibFile5       s             21a   inz
005100170731     D wLibFile6       s             21a   inz
005200170727     D parametroNPR    s             10a   inz
005300170727     D wKSU            s              7s 0 inz
005400170731     D wUnifKSU        s              7s 0 inz
005500170731     D wUnifBolla      s              7s 0 inz
005600170727
005700091223
005800091223     c     *Entry        plist
005900170727     c                   parm                    iB2ORMSUN
006000170727     c                   parm                    iB2ORMKSU
006100170727     c                   parm                    iB2ORMISV
006200170727     c                   parm                    iB2ORMAAS
006300170727     c                   parm                    iB2ORMSPE
006400170727     c                   parm                    iB2ORMCRC
006500170727     c                   parm                    iB2ORMDPM
006600170727     c                   parm                    iB2ORMORARD
006700170727     c                   parm                    iB2ORMORARA
006800170727     c                   parm                    iB2ORMRFA
006900170727     c                   parm                    iB2ORMEMLR
007000170727     c                   parm                    iB2ORMSMSR
007100170727     c                   parm                    iB2ORMNOTE
007200170727     c                   parm                    iB2ORMIP
007300170727     c                   parm                    oB2ORMEsito
007400170727     c                   parm                    oB2ORMNPR
007500091223
007600130307      /free
007700091223
007800091223       //--------------------------------------------------------------
007900121106       // M A I N - L I N E
008000091223       //--------------------------------------------------------------
008100140806
008200160805       // Monitorizzo tutto (per necessità massima solidità stack chiamate)
008300160805       Monitor;
008400160805
008500130307       // Operazioni iniziali?
008600170508       exsr RoutInz;
008700130307
008800170727       // Eseguo reperimento dati spedizione
008900170508       exsr RtvSped;
009000160804
009100160805       // Fine monitoring
009200160805       On-Error;
009300170727          oB2ORMEsito = -1;
009400160921          exsr exeUscita;
009500160805       Endmon;
009600160805
009700170727       // Esito OK
009800170727       oB2ORMEsito = 0;
009900170727
010000160920       // Uscita
010100160921       exsr exeUscita;
010200091223
010300160802
010400091223       //--------------------------------------------------------------
010500121107       // Operazioni iniziali.
010600091223       //--------------------------------------------------------------
010700170726       BEGSR  RoutInz;
010800160601
010900160920         // Inizializzo l'esito a errore generico
011000170727         oB2ORMEsito = -1;
011100170511
011200170515         // Inizializzazione campi di work e di output
011300170727         clear FNVAOEIIR;
011400170727         clear oB2ORMNPR;
011500160608
011600160608         // Controllo i parametri di input
011700170508         exsr CtrlPIn;
011800160202
011900160202       ENDSR;
012000160802
012100160804
012200160202       //--------------------------------------------------------------
012300160202       // Controllo i parametri di input
012400160202       //--------------------------------------------------------------
012500170727       BEGSR  CtrlPIn;
012600170508
012700170727         // Se non valorizzati alcuni parametri di input => imposto default
012800170727         if iB2ORMAAS = *zeros;
012900170727            iB2ORMAAS = %subdt(%date():*YEARS);
013000170727         endif;
013100170727         if iB2ORMISV = *blanks;
013200170727            iB2ORMISV = 'IW';
013300170727         endif;
013400170804         if iB2ORMDPM = *zeros;
013500170804            iB2ORMDPM = %dec(%date() + %days(1) : *ISO);
013600170804         endif;
013700170727
013800170727         // I parametri fondamentali devono essere valorizzati
013900170727         if iB2ORMSUN = *blanks OR iB2ORMSUN = *zeros;
014000170727            oB2ORMEsito = -2;
014100170727            exsr exeUscita;
014200170727         endif;
014300170727         if iB2ORMKSU = *blanks OR iB2ORMKSU = *zeros;
014400170727            oB2ORMEsito = -2;
014500170727            exsr exeUscita;
014600170727         endif;
014700170727
014800170727         // Verifico numericità parametri necessariamente numerici
014900170727         PiStr = iB2ORMKSU;
015000170727         exsr chkNum;
015100170727         if PiInt = *on;
015200170727            wKSU = PiVal;
015300170731
015400170731            // Reperisco padre (legame 'WW') del codice cliente richiesta
015500170731            iBS10CLI = wKSU;
015600170731            exsr rtvUNI;
015700170731            if oBS10COP > *zeros;
015800170731               wUnifKSU = oBS10COP;
015900170731            else;
016000170731               wUnifKSU = wKSU;
016100170731            endif;
016200170727         else;
016300170727            oB2ORMEsito = -2;
016400170727            exsr exeUscita;
016500170727         endif;
016600170804
016700170804         // Controllo validità valori "anagrafici"
016800170804         if iB2ORMCRC > *zeros;
016900170804            chain (iB2ORMCRC) fnacr01l;
017000170804            if not %found(fnacr01l);
017100170804               oB2ORMEsito = -4;
017200170804               exsr exeUscita;
017300170804            endif;
017400170804         endif;
017500091223
017600091223       ENDSR;
017700121106
017800160802
017900091223       //--------------------------------------------------------------
018000160217       // Esecuzione
018100091223       //--------------------------------------------------------------
018200170508       BEGSR  RtvSped;
018300170727
018400170728         DS_SPED_BRT.sped = iB2ORMSPE;
018500170727         chain (iB2ORMAAS:
018600170727                DS_SPED_BRT.lnp:
018700170727                DS_SPED_BRT.nrs:
018800170727                DS_SPED_BRT.nsp) titas30c;
018900170727
019000170804         if %found(titas30c)    AND
019100170804            tasDCM >= *zeros    AND
019200170804            (tasCCA = *blanks OR tasCCA = '1');
019300170731
019400170731            // Reperisco padre (legame 'WW') del codice cliente mittente bolla
019500170731            iBS10CLI = tasCCM;
019600170731            exsr rtvUNI;
019700170731            if oBS10COP > *zeros;
019800170731               wUnifBolla = oBS10COP;
019900170731            else;
020000170731               wUnifBolla = tasCCM;
020100170731            endif;
020200170731
020300170731            // Procedo solo se i codici padri "cliente richiesta" e "mittente bolla" coincidono
020400170731            if wUnifKSU = wUnifBolla;
020500170731            else;
020600170731               oB2ORMEsito = -3;
020700170731               exsr exeUscita;
020800170731            endif;
020900170731
021000170727            clear dar5gen;
021100170727            chain (tasAAS:tasLNP:tasNRS:tasNSP:'GEN') fiar531c;
021200170727            if %found(fiar531c);
021300170727               dar5gen = ar5UNI;
021400170727            endif;
021500170727            clear dar5emd;
021600170727            chain (tasAAS:tasLNP:tasNRS:tasNSP:'EMD') fiar531c;
021700170727            if %found(fiar531c);
021800170727               dar5emd = ar5UNI;
021900170727            endif;
022000170727
022100170727            // Valorizzzione DS ORM
022200170728            exsr valORM_Inz;
022300170727            exsr valORM_Bolla;
022400170727            exsr valORM_Param;
022500170728            exsr valORM_Check;
022600170727
022700170727         else;
022800170727            oB2ORMEsito = -3;
022900170727            exsr exeUscita;
023000170727         endif;
023100160217
023200160217       ENDSR;
023300170728
023400170728
023500170728       //--------------------------------------------------------------
023600170728       // Valorizzaione dati ORM "iniziali"
023700170728       //--------------------------------------------------------------
023800170728       BEGSR valORM_Inz;
023900170728
024000170728         vaoPOE   = *zeros;
024100170728         vaoNSR   = *zeros;
024200170728         vaoNOR   = *zeros;
024300170728         vaoNRV   = *zeros;
024400170728         vaoDAO   = *zeros;
024500170728         vaoOAO   = *zeros;
024600170728         vaoCOR   = *zeros;
024700170728         vaoCRA   = *zeros;
024800170728         vaoCRC   = *zeros;
024900170728         vaoDAR   = *zeros;
025000170728         vaoORR   = *zeros;
025100170728         vaoNCL   = *zeros;
025200170728         vaoPKG   = *zeros;
025300170728         vaoVLM   = *zeros;
025400170728         vaoBNC   = *zeros;
025500170728         vaoBLC   = *zeros;
025600170728         vaoATT   = *zeros;
025700170728         vaoMTC   = *zeros;
025800170728         vaoKSC   = *zeros;
025900170728         vaoPOR   = *zeros;
026000170728         vaoPOC   = *zeros;
026100170728         vaoSTO   = *zeros;
026200170728         vaoORDA1 = *zeros;
026300170728         vaoORAA1 = *zeros;
026400170728         vaoORDA2 = *zeros;
026500170728         vaoORAA2 = *zeros;
026600170728         vaoORARD = *zeros;
026700170728         vaoORARA = *zeros;
026800170728         vaoDDPM  = *zeros;
026900170728         vaoDDAR  = *zeros;
027000170728
027100170728       ENDSR;
027200170727
027300170727
027400170727       //--------------------------------------------------------------
027500170727       // Valorizzaione dati ORM da bolla
027600170727       //--------------------------------------------------------------
027700170727       BEGSR valORM_Bolla;
027800170727
027900170727         vaoPOE  = ' '+%subst(%editc(wKSU:'X'):1:3);
028000170727         vaoTOR  = 'S';
028100170727         vaoTCO  = 'F';
028200170727         vaoDAO  = '  '+%editc(%dec(%date() : *ISO):'X');
028300170727         vaoOAO  = '  '+%editc(%dec(%time() : *ISO):'X');
028400170728         vaoCOR  = '  '+%editc(wKSU:'X')+'000';
028500170727         vaoRSR  = tasRSD;
028600170727         vaoINR  = tasIND;
028700170727         vaoCAR  = tasCAD;
028800170727         vaoLOR  = tasLOD;
028900170727         vaoPRR  = tasPRD;
029000170727         vaoNAR  = tasNZD;
029100170727         vaoRER  = dar5gen.§AR5REF;
029200170727         vaoTER  = dar5gen.§AR5TELD;
029300170727         vaoNCL  = ' '+%editc(tasNCL:'X');
029400170728         vaoPKG  = ' '+%editc(tasPKB:'4');
029500170728         vaoPKG  = %scanrpl(' ':'0':vaoPKG:2);
029600170728         vaoVLM  = ' '+%editc(tasVLB:'4');
029700170728         vaoVLM  = %scanrpl(' ':'0':vaoVLM:2);
029800170727         vaoPAG  = 'O';
029900170727         vaoRFA  = %editc(tasRMN:'4');
030000170727         vaoEMLR = dar5emd.§AR5EML;
030100170727         vaoSMSR = dar5emd.§AR5TEL;
030200170727         vaoDOIO = %char(%timestamp);
030300170727
030400170727       ENDSR;
030500170727
030600170727
030700170727       //--------------------------------------------------------------
030800170727       // Valorizzaione dati ORM da parametri
030900170727       //--------------------------------------------------------------
031000170727       BEGSR valORM_Param;
031100170727
031200170727         if iB2ORMCRC    > *zeros;
031300170727            vaoCRC   = '  '+%editc(iB2ORMCRC:'X');
031400170727         endif;
031500170727
031600170727         if iB2ORMDPM    > *zeros;
031700170727            vaoDDPM  = %editc(iB2ORMDPM:'X');
031800170727         endif;
031900170727
032000170728         vaoORARD = ' '+%editc(iB2ORMORARD:'X');
032100170728         vaoORARA = ' '+%editc(iB2ORMORARA:'X');
032200170727
032300170727         if iB2ORMRFA   <> *blanks;
032400170727            vaoRFA   = iB2ORMRFA;
032500170727         endif;
032600170727
032700170727         if iB2ORMNOTE  <> *blanks;
032800170727            vaoNO1 = %subst(iB2ORMNOTE:01:35);
032900170727            vaoNO2 = %subst(iB2ORMNOTE:36:35);
033000170727         endif;
033100170727
033200170727         if iB2ORMEMLR  <> *blanks;
033300170727            vaoEMLR  = iB2ORMEMLR;
033400170727         endif;
033500170727
033600170727         if iB2ORMSMSR  <> *blanks;
033700170727            vaoSMSR  = iB2ORMSMSR;
033800170727         endif;
033900170727
034000170727         vaoORIP = iB2ORMIP;
034100170727
034200170727       ENDSR;
034300170728
034400170728
034500170728       //--------------------------------------------------------------
034600170728       // Valorizzaione dati ORM da controlli "particolari"
034700170728       //--------------------------------------------------------------
034800170728       BEGSR valORM_Check;
034900170728
035000170728         // Logiche di valorizzazione Codice Cliene Consegna
035100170728         // Se codice cliente consegna non indicato espressamente nei parametri di input
035200170728         if vaoCRC = *blanks;
035300170728
035400170728            // Tentativo considerazioe luogo RESO (300)
035500170728            chain ('L':tasCCM:'300') fnspe01l;
035600170728            if %found(fnspe01l);
035700170728               vaoRSC = speRAG;
035800170728               vaoINC = speIND;
035900170728               vaoCAC = speCAP;
036000170728               vaoLOC = speLOC;
036100170728               vaoPRC = spePRO;
036200170728               vaoNAC = speNAZ;
036300170728            else;
036400170728
036500170728               // In ultima istanza assumo default CCM+444 (come per Particolarità RC)
036600170731               chain (tasCCM*1000+444) fnacr01l;
036700170731               if %found(fnacr01l);
036800170731                  vaoCRC  = '  '+%editc(acrCRO:'X');
036900170731               else;
037000170731                  chain (1:151:tasCCM) cnaco00f;
037100170731                  if %found(cnaco00f);
037200170731                  chain (1:151:tasCCM) cnind01l;
037300170731                  if %found(cnind01l);
037400170731                     vaoRSC = acoRAG;
037500170731                     vaoINC = indVIA;
037600170731                     vaoCAC = indCAE;
037700170731                     vaoLOC = indCIT;
037800170731                     vaoPRC = indPRV;
037900170731                     vaoNAC = indSTA;
038000170731                     vaoFFD = 'S';
038100170731                  endif;
038200170731                  endif;
038300170731               endif;
038400170728            endif;
038500170728
038600170728         endif;
038700170728
038800170728       ENDSR;
038900170727
039000170727
039100170727       //--------------------------------------------------------------
039200170727       // Scrittuta buffer dati
039300170727       //--------------------------------------------------------------
039400170727       BEGSR wriORM;
039500170727
039600170728         clear tivpi000;
039700170727         vpiTIP = 'TO';
039800170727         vpiISV = iB2ORMISV;
039900170727         vpiSUN = iB2ORMSUN;
040000170727         vpiKSU = '0'+%editc(wKSU:'X');
040100170727         vpiDTA = FNVAOEIIR;
040200170728         write tivpi000;
040300170727
040400170727       ENDSR;
040500170509
040600170509
040700170509       //--------------------------------------------------------------
040800170509       // Controllo numericità dato
040900170509       //--------------------------------------------------------------
041000170509       BEGSR chkNum;
041100170509
041200170509         // Imposto default separatore decimale
041300170509         PiDecChr = ',';
041400170727
041500170727         // Sostituisco evebtuale separatore decimale '.' in quello provisto
041600170728         PiStr = %scanrpl('.' : PiDecChr : PiStr);
041700170509
041800170509         // Verifica numericità
041900170509         callp UBISNUM_Check(PiStr
042000170509                            :PiDecChr
042100170509                            :PiVal
042200170509                            :PiNum
042300170509                            :PiInt);
042400170509
042500170509       ENDSR;
042600170727
042700170727
042800170727       //--------------------------------------------------------------
042900170727       // Uscita
043000170727       //--------------------------------------------------------------
043100170727       BEGSR exeUscita;
043200170727
043300170728         // Se tutto OK
043400170728         if oB2ORMEsito = *zeros;
043500170728            // Stacco Numero Prenotazionen Ritiro
043600170728            vaoNPR    = GetNPR(parametroNPR);
043700170728            oB2ORMNPR = vaoNPR;
043800170728
043900170728            // Scrittura dati ORM
044000170728            exsr wriORM;
044100170727         endif;
044200170727
044300170727         return;
044400170727
044500170727       ENDSR;
044600170731
044700170731
044800170731       //--------------------------------------------------------------
044900170731       // Reperimento Codice Padre
045000170731       //--------------------------------------------------------------
045100170731       BEGSR rtvUni;
045200170731
045300170731         iBS10DRF  = *zeros;
045400170731         iBS10TLE  = 'WW';
045500170731
045600170731         // Reperimento codice padre
045700170731         callp UBBS10R (iBS10DRF
045800170731                       :iBS10TLE
045900170731                       :iBS10CLI
046000170731                       :oBS10Esito
046100170731                       :oBS10COP
046200170731                       :oBS10TIP);
046300170731
046400170731       ENDSR;
046500170727
046600170727
046700170727       //--------------------------------------------------------------
046800170727       // *INZSR
046900170727       //--------------------------------------------------------------
047000170727       BEGSR  *inzsr;
047100170727
047200170727         // Verifico il sistema AS/400 corrente
047300170727         wLibFile1 = 'GAITRAGRPS/TITAS30C';
047400170727         wLibFile2 = 'GAITRAGRPS/FIAR531C';
047500170728         wLibFile3 = 'UNITRAGRP/FNSPE01L';
047600170731         wLibFile4 = 'UNITRAGRP/FNACR01L';
047700170731         wLibFile5 = 'UNITRAGRP/CNACO00F';
047800170731         wLibFile6 = 'UNITRAGRP/CNIND01L';
047900170727         if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
048000170728            if %subst(PiOutSysNeta:1:6) = 'SETRAS';
048100170727               wLibFile1 = 'GAITRAGRU/TITAS30C';
048200170727               wLibFile2 = 'GAITRAGRU/FIAR531C';
048300170728               wLibFile3 = 'UNITRAGRU/FNSPE01L';
048400170731               wLibFile4 = 'UNITRAGRU/FNACR01L';
048500170731               wLibFile5 = 'UNITRAGRU/CNACO00F';
048600170731               wLibFile6 = 'UNITRAGRU/CNIND01L';
048700170727           endif;
048800170727         endif;
048900170727
049000170727         if  not %open(titas30c);
049100170727           open  titas30c;
049200170727         endif;
049300170727         if  not %open(fiar531c);
049400170727           open  fiar531c;
049500170727         endif;
049600170728         if  not %open(fnspe01l);
049700170728           open  fnspe01l;
049800170728         endif;
049900170731         if  not %open(fnacr01l);
050000170731           open  fnacr01l;
050100170731         endif;
050200170731         if  not %open(cnaco00f);
050300170731           open  cnaco00f;
050400170731         endif;
050500170731         if  not %open(cnind01l);
050600170731           open  cnind01l;
050700170731         endif;
050800170727
050900170727       ENDSR;
051000091223
051100160802
051200091223      /end-free
