000100110617      *---------------------------------------------------------------*
000200110627      * Avviso al destinatario di affidamento spedizione              *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600131017     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000130626     ffiar504l  uf   e           k disk
001100170126     ftivgd00f  o    e             disk
001200161205     ftidp300f  uf a e             disk    commit
001300130701     ffiar901l  if   e           k disk
001400130829     ftabel00f  if   e           k disk
001500140805     ffnevb04l  if   e           k disk
001600150929     fazorg01l  if   e           k disk
001700150929     ftntbe01l  if   e           k disk
001800130701
001900130701
002000130701     d                sds
002100130701     d  nompgm                 1     10
002200130701     d  SDSusr               254    263
002300110617
002400130701
002500130626      * - DS procedure esterne
002600110617     d dar5emd       e ds
002700130626     d kpjba         e ds
002800130701     d trul33ds      e ds
002900120619     d tnsd99ds      e ds
003000161205     d trul28ds0     e ds                  qualified inz
003100140324     d trulORsds     e ds                  inz
003200140324     d trulOR2ds     e ds                  inz
003300130701     d trul21ds      e ds
003400140414     d fidna6ds      e ds                  inz
003500130625     d fnlvemds      e ds
003600130829     d ds3a          e ds
003700141128     d ds7r          e ds
003800130626     d fnblpds       e ds                  extname('FNBLP00F')
003900150730     d diore01       e ds
004000150929     d og143         e ds                  inz
004100150929     d dntw          e ds                  inz
004200170126     d tis7vasds     e ds                  inz
004300130626
004400130626
004500110617      * - Campi di comodo
004600130829     d y               s              3  0 inz
004700130829     d cbf             S              2    DIM(100) inz
004800140805     d bolok           s              1    inz
004900140805     d bologgi         s              1    inz
005000130626     d kdos            s                   like(ar5dos) inz
005100130703     d wSmsText        s           1600    inz
005200131104     d wTracking       s            100    inz
005300130626     d wProcedi        s              1    inz
005400130701     d wcliper         s              1    inz
005500141128     d wPINcode        s              1    inz
005600130701     d Dataeur         s               d   datfmt(*eur)
005700131004     d currDate        s              8  0 inz
005800130701     d wdcr            s                   like(blpdcr)
005900140327     d wdcrRev         s                   like(blpdcr)
006000130704     d wSep            s              1    inz('|')
006100130704     d wPos            s              2  0 inz
006200130719     d wEsito          s              1    inz
006300141202     d wMaxRecFile     s              4  0 inz(200)
006400130920     d wCurRecFile     s              4  0 inz
006500141201     d wMaxCivetta     s              4  0 inz(5000)
006600141201     d wCurCivetta     s              4  0 inz
006700161205     d wBRTcodeLong    s             25                   inz
006800140623     d wSpedConsOrari  s             64A                  inz
006900140324     d wSpedConsOraDa  s              5A                  inz
007000140324     d wSpedConsOraA   s              5A                  inz
007100140610     d wSpedNum        s             16A                  inz
007200140423     d wConsegna       s           1000A   varying        inz
007300140725     d wRSM_Sms        s                   like(pOut_RNT) inz
007400141128     d pIn_Opz         s              1a   inz
007500141128     d pIn_APL         s              1a   inz
007600141128     d pIn_NK1         s             11a   inz
007700141128     d pIn_NK2         s              4a   inz
007800141128     d pIn_TNT         s              2a   inz
007900141128     d pOut_RNT        s             60a   inz
008000141128     d pOut_NTR        s              6s 0 inz
008100141128     d pOut_Esito      s              1a   inz
008200141128     d sk7R_PINcode    s              2    dim(100) inz
008300141128     d ix7R            s              3  0 inz
008400150929     D skFilExt        s                   like(orgFIL) dim(500) inz
008500160506     D skFilExtDPD     s                   like(orgFIL) dim(500) inz
008600150929     D ixFilExt        s              3  0 inz
008700170126     D wVPOdeploy      s              6A   inz
008800170126     D wVASPRG         s                   like(O§7VASPRG) inz
008900141016
009000141016
009100141016     D BRTcode         ds                  qualified
009200141016     D   aas4                         4  0
009300141016     D   lnp                          3  0
009400141016     D   nrs                          2  0
009500141016     D   nsp                          7  0
009600141016     D   chkdgt1                      1  0
009700141016     D   code                         3  0
009800141016     D   chkdgt2                      1  0
009900141016     D   BRTcode               3     21
010000130704
010100130703
010200130703     d* DS di output
010300130703     d  DS_Output      ds                  inz
010400130704     d   o_MittSMS                   30
010500130704     d   o_IdSMS                     36
010600130703     d   o_cell                      20
010700130703     d   o_message                 1600
010800170126
010900170126
011000170126      // DS per filiali abilitate al deploy
011100170126     D TRULVPODS     e ds
011200170126     D  skFilvpo              16    765    dim(250)
011300170126     D skFil_New       s                   like(skFilvpo) dim(250) inz
011400130406
011500130625
011600130701     d fnlvemr         PR
011700130626     d                                     extpgm('FNLVEMR')
011800130701     d  kpjba                              likeds(kpjba)
011900130701     d  fnblpds                            likeds(fnblpds)
012000140414
012100140414
012200140414     d FIDNA6R         PR
012300140414     d                                     extpgm('FIDNA6R')
012400140414     d fidna6ds                            likeds(fidna6ds)
012500140414
012600140414
012700130701     d trul21r         PR
012800130701     d                                     EXTPGM('TRUL21R')
012900130701     d  trul21ds                           likeds(trul21ds)
013000141016
013100141016
013200141016     D TRUL28R0        PR
013300141016     D                                     extpgm('TRUL28R0')
013400141016     D trul28ds0                           likeds(trul28ds0)
013500130701
013600130701
013700130626     d trul33r         PR
013800130626     d                                     extpgm('TRUL33R')
013900130626     d  kpjba                              likeds(kpjba)
014000140324
014100140324
014200140324     D TRULORSR        PR
014300140324     D                                     extpgm('TRULORSR')
014400140324     D kpjba                               likeds(kpjba)
014500140324     D trulORsds                           likeds(trulORsds)
014600140324     D trulOR2ds                           likeds(trulOR2ds)
014700130719
014800161205
014900170126     D tis7vasr1       PR
015000170126     D                                     extpgm('TIS7VASR1')
015100170126     D  tis7vasds                          like(tis7vasds)
015200170126
015300170126
015400170126     D TRULVPOR        PR
015500170126     D                                     extpgm('TRULVPOR')
015600170126     D trulvpods                           likeds(TRULVPODS)
015700140725
015800140725
015900140725     D UBREPNTCR       PR
016000140725     D                                     extpgm('UBREPNTCR')
016100140725     D pIn_Opz                             like(pIn_Opz)
016200140725     D pIn_APL                             like(pIn_APL)
016300140725     D pIn_NK1                             like(pIn_NK1)
016400140725     D pIn_NK2                             like(pIn_NK2)
016500140725     D pIn_TNT                             like(pIn_TNT)
016600140725     D pOut_RNT                            like(pOut_RNT)
016700140725     D pOut_NTR                            like(pOut_NTR)
016800140725     D pOut_Esito                          like(pOut_Esito)
016900130719
017000110617
017100110617      // ? PROTOTIPI ?
017200120619      /copy gaitrasrc/srcprotopr,tnsd99r
017300131017      /copy gaitrasrc/srcprotopr,ubchkcel
017400131017      /copy gaitrasrc/srcprotopi,ubchkcel
017500110617
017600130406
017700110627     C     *ENTRY        PLIST
017800110627     C                   PARM                    chiudi            1
017900130406
018000110617      /free
018100130626
018200140731       // Inizializzozioni
018300140731       wProcedi = 'S';
018400140805       wCurRecFile = *zeros;
018500131015
018600130829       // Carico le tabelle occorrenti
018700130829       exsr sr_cartab;
018800170126
018900170126       // carico le filiali per rilasci pilotati
019000170126       exsr sr_VPOR;
019100131004
019200131004       // Inizializzo variabili di procedura
019300131004       currDate = %dec(%date() : *ISO);
019400131004
019500161205       // Avvio Inizio transazione TIVGD x file SMS
019600170126       clear wVASPRG;
019700161205       exsr strVGD;
019800131004
019900130701       // Se consentita elaborazione => procedo
020000140731       if wProcedi <> 'N';
020100130701          // Leggo fiar5 rk "EMD" x SMS da inviare
020200130719          *in34 = *off;
020300130626          exsr sr_Elabora;
020400131004
020500170126          // Fine transazione TIVGD x file SMS
020600170126          exsr endVGD;
020700170126       endif;
020800130406
020900130626       // Gestisco chiusura controllata
021000130626       if chiudi = 'S' or %shtdn;
021100130625          clear fnlvemds;
021200130625          iemTLA = 'C';
021300130625          kpjbu = fnlvemds;
021400130625          callP(e) FNLVEMR (kpjba : fnblpds);
021500140725          pIn_OPZ = 'C';
021600140725          callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
021700140725                              pIn_TNT : pOut_RNT : pOut_NTR : pOut_Esito);
021800161205          *inlr = *on;
021900140623       else;
022000161205          *inrt = *on;
022100110627       endif;
022200140617
022300140623       unlock(e) fiar504l;
022400130626
022500161205       return;
022600161205
022700130626
022800130626
022900130626       //-------------------------------------------------------------*
023000161205       // Avvio Inizio transazione TIVGD x file SMS
023100130626       //-------------------------------------------------------------*
023200161205        Begsr strVGD;
023300161205
023400170126          clear tis7vasds;
023500170127          i§7VASOPZ  = 'PRG';
023600170126          callP(e) tis7vasr1 (tis7vasds);
023700130626
023800170126          if %error OR o§7VASOK = *off;
023900161205             wProcedi = 'N';
024000170126          else;
024100170126             wVASPRG = o§7VASPRG;
024200161205          endif;
024300130626
024400130626        endsr;
024500161205
024600161205
024700161205
024800161205       //-------------------------------------------------------------*
024900161205       // Fine transazione TIVGD x file SMS
025000161205       //-------------------------------------------------------------*
025100161205        Begsr endVGD;
025200170126
025300170126          clear tis7vasds;
025400170127          I§7VASOPZ  = 'RLS';
025500170126          I§7VASTIP  = 'SM';
025600170126          I§7VASKSU  = '0BART001';
025700170126          I§7VASTSC  = 'WW';
025800170127          I§7VASPRG  = wVASPRG;
025900170126          callP(e) tis7vasr1 (tis7vasds);
026000170126
026100170126          if %error OR o§7VASOK = *off;
026200170126             dump(A);
026300170126             rolbk;
026400170126             *inlr = *on;
026500170126             return;
026600170126          endif;
026700161205
026800161205        endsr;
026900130626
027000130626
027100130626
027200130626       //-------------------------------------------------------------*
027300130626       //Reperimento progressivo                                      *
027400130626       //-------------------------------------------------------------*
027500130626        Begsr sr_rtvPrg;
027600130626
027700130626          clear trul33ds;
027800130626          i33cnu = 24;
027900130626          i33num = 1;
028000130626          kpjbu = trul33ds;
028100130626          trul33r (kpjba);
028200130626          trul33ds = kpjbu;
028300130626
028400130626        endsr;
028500130626
028600130406
028700130406
028800110617       //-------------------------------------------------------------*
028900110617       //Lettura fiar5 record "EMD" da inviare                         *
029000110617       //-------------------------------------------------------------*
029100110617       Begsr SR_Elabora;
029200130626
029300130626        kdos = *blanks;
029400130626        setll (kdos) fiar504l;
029500130626        reade (kdos) fiar504l;
029600110617
029700130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
029800130406
029900130626           // controllo se è stata richiesta la chiusura del sottosistema
030000110627           if %shtdn;
030100130626               chiudi = 'S';
030200110627               leavesr;
030300110627           endif;
030400141201
030500141201           // Gestione "civette"
030600141201           if wCurCivetta >= wMaxCivetta;
030700141201              exsr exeVGDcivetta;
030800141201              wCurCivetta = *zeros;
030900141201           endif;
031000141201
031100130406
031200130626           // controllo lo stato della bolla
031300110617           exsr sr_chkstblp;
031400140805           if bolok <> *blanks;
031500130719
031600130703              // inizializzo DS di work
031700130703              clear DS_Output;
031800130701              clear fiar9000;
031900130703
032000130703              // provo a reperire il contrassegno
032100130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
032200130703
032300130703              // compongo l'output per l'invio SMS
032400130703              dar5emd = ar5uni;
032500130719
032600130719              // normalizzo e verifico il numero di cellulare
032700130719              *in33 = *off;
032800130719              exsr sr_normCELL;
032900130719              if not *in33;
033000130719                 exsr sr_inviaSMS;
033100130719              endif;
033200130703
033300130703              // aggiorno il record corrente come SMS inviato
033400150930              exsr sr_UpdFIAR5;
033500150930
033600110617           endif;
033700130406
033800130701           reade (kdos) fiar504l;
033900110617        enddo;
034000130406
034100130626       endsr;
034200150930
034300150930
034400150930
034500150930       //-------------------------------------------------------------*
034600150930       // Gestione tentativi esecuzioni comandi                       *
034700150930       //-------------------------------------------------------------*
034800150930        Begsr sr_UpdFIAR5;
034900150930
035000150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
035100150930          §ar5DPC  = %editc(wdcrRev:'X');
035200150930          §ar5OPCD = %editc(OOR2STIS:'X');
035300150930          §ar5OPCA = %editc(OOR2STFS:'X');
035400150930          // Verifico che i valori sopra attribuiti siano diversi da zero
035500150930          if §AR5DPC  = *all'0';
035600150930             §AR5DPC  = *blanks;
035700160330             §AR5OPCD = *zeros;
035800160330             §AR5OPCA = *zeros;
035900150930          endif;
036000150930          if §AR5OPCD = *all'0';
036100150930             §AR5OPCD = *blanks;
036200150930          endif;
036300150930          if §AR5OPCA = *all'0';
036400150930             §AR5OPCA = *blanks;
036500150930          endif;
036600150930          ar5uni=dar5emd;
036700150930          if chiudi = *blanks;
036800150930             update fiar5000;
036900150930          endif;
037000150930
037100150930        endsr;
037200130406
037300130626
037400130406
037500110617       //-------------------------------------------------------------*
037600110617       //Verifica stato della bolla                                   *
037700110617       //-------------------------------------------------------------*
037800130625        Begsr sr_chkstblp;
037900130406
038000130626           bolok=' ';
038100131104           bologgi=' ';
038200130626
038300130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
038400130626           // delle comunicazioni ai destinatari
038500130626           clear fnlvemds;
038600130626           clear fnblpds;
038700140725           clear wRSM_Sms;
038800130626           iemAAS = ar5aas;
038900130626           iemLNP = ar5lnp;
039000130626           iemNRS = ar5nrs;
039100130626           iemNSP = ar5nsp;
039200130626           kpjbu = fnlvemds;
039300130626           callP(e) FNLVEMR (kpjba : fnblpds);
039400130626           fnlvemds = kpjbu;
039500140725
039600130626
039700130626           // Verifico se esito favorevole al invio
039800130626           if oemESITO = '1';
039900150929
040000150929           // Verifico che NON trattasi di spedizione "export"
040100150929           if  %lookup(blpLNA:skFilExt) > 0;
040200150929               bolok = 'E';
040300150929           endif;
040400150929
040500150929           // Se tutto OK => procedo
040600150929           if bolok <> 'E';
040700140725
040800140805              // Verifico se per cliente corrente presente nota specifica
040900140725              // per Rag.Soc. invio alerts
041000140725              if blpCCM > *zeros;
041100140725                 pIn_NK1 = '0151' + %editc(blpCCM:'X');
041200140725              else;
041300140725                 pIn_NK1 = '0151' + %editc(blpKSC:'X');
041400140725              endif;
041500140725              // --- SMS ---
041600140725              pIn_OPZ = *blanks;
041700140725              pIn_APL = 'C';
041800140725              pIn_NK2 = *blanks;
041900140725              pIn_TNT = 'AS';
042000140725              callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
042100140725                                  pIn_TNT : wRSM_Sms : pOut_NTR : pOut_Esito);
042200140725              if pOut_Esito = '1';
042300140725              else;
042400140725                 wRSM_Sms = %trim(blpRSM);
042500140725              endif;
042600141016
042700141016
042800141016              // Calcolo il BRTcode (tramite apposito driver)
042900141016              clear BRTcode;
043000141016              BRTcode.aas4    = blpAAS;
043100141016              BRTcode.lnp     = blpLNP;
043200141016              BRTcode.nrs     = blpNRS;
043300141016              BRTcode.nsp     = blpNSP;
043400141016
043500141016              clear trul28ds0;
043600141016              trul28ds0.i280COD = BRTcode.BRTcode;
043700141016              callP(e) TRUL28R0 (trul28ds0);
043800141016
043900141016              // Se BRTcode errato => esito KO
044000141016              clear wBRTcodeLong;
044100141016              if %error OR trul28ds0.o280err = '1';
044200141016              else;
044300170127                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
044400170127                                %subst(trul28ds0.o280cod:6:5) +
044500170127                                %subst(trul28ds0.o280cod:11:5)+
044600141016                                %subst(trul28ds0.o280cod:16:4);
044700141016              endif;
044800131104
044900141016
045000131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
045100131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
045200131104                 bolok='S';
045300131104              endif;
045400131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
045500131104                 %dec(%time():*ISO)  >= 150000;
045600131104                 bolok='S';
045700131104                 bologgi='S';
045800131104              endif;
045900140805
046000140805              if bolok='S';
046100140805                 // Verifico per sicurezza se già presente evento 'MIC'
046200140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
046300140805                 if %found(fnevb04l);
046400140805                    bolok='X';
046500140805                 endif;
046600140805              endif;
046700150929
046800150929           endif;
046900131104
047000130626           endif;
047100130406
047200110617        endsr;
047300130719
047400130719
047500130719
047600130719       //-------------------------------------------------------------*
047700130719       //Normalizza numero ci cellulare                               *
047800130719       //-------------------------------------------------------------*
047900130719        Begsr sr_normCELL;
048000130719
048100131017           // Chiamo il driver preposto
048200131017           pInCell = %trim(§ar5TEL);
048300131017           pOutErr = *blanks;
048400131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
048500131017              §ar5SME = 'E';
048600131017              *in33 = *on;
048700131017           else;
048800131017              o_cell = pOutCell;
048900131017           endif;
049000130719
049100130719        endsr;
049200130406
049300130626
049400130406
049500110617       //-------------------------------------------------------------*
049600130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
049700110617       //-------------------------------------------------------------*
049800130626        Begsr sr_inviaSMS;
049900130719
050000130719          // Stacco un progressivo per il file SMS corrente
050100130719          if not *in34;
050200130719             exsr sr_rtvPrg;
050300130719             *in34 = *on;
050400130719          endif;
050500130406
050600130626          // determino data consegnabilità
050700130626          exsr sr_calcons;
050800130626
050900170127          // Verifoco se per linea arrivo corrente previsto nuovo deploy
051000170127          exsr sr_ChkDeploy;
051100170127
051200170126          // Compongo il testo del SMS e gestisco modo vecchio/nuovo
051300170126          if not *in40;
051400170126             exsr valTextSMS;
051500170126          else;
051600170126             exsr valTextSMSnew;
051700170126          endif;
051800130626
051900130626          // Aggiungo l'SMS corrente
052000130626          exsr exeVGD;
052100130406
052200110617        endsr;
052300130626
052400130626
052500130626
052600130626       //-------------------------------------------------------------*
052700130626       //Verifica eventuali personalizzazioni per il cliente          *
052800130626       //-------------------------------------------------------------*
052900130701        Begsr sr_calcons;
053000130626
053100130701        clear wcliper;
053200130701        clear wdcr;
053300140327        clear wdcrRev;
053400141128        clear wPINcode;
053500140331        clear tnsd99ds;
053600140324        clear trulORsds;
053700140324        clear trulOR2ds;
053800150730        clear diore01;
053900141128
054000141128        // Verifica se spedizione con particolarità consegna "PINcode"
054100141128        if blpGMA <> *blanks;
054200141128           if %lookup(blpGMA:sk7R_PINcode)>0;
054300141128              wPINcode = 'S';
054400141128            endif;
054500141128        endif;
054600130701
054700130701        // Se presente il contrassegno verifica se richiesto il contatto
054800130701        if ar9cas > 0;
054900130701           clear trul21ds;
055000130701           i21tla = 'L';
055100130701           i21cbo = blpcbo;
055200130701           i21tsp = blptsp;
055300130701           i21lnp = blplnp;
055400130701           i21nzm = blpnzm;
055500130701           i21lna = blplna;
055600130701           i21nzd = blpnzd;
055700130701           i21tic = ar9tic;
055800130701           i21imp = ar9cas;
055900130701           i21div = ar9vca;
056000130701           i21ute = sdsusr;
056100130701           i21pgm = nompgm;
056200130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
056300130829              if %lookup(blpcbo:cbf)>0;
056400130829                 i21ksc = blpksc;
056500130829              else;
056600130829                 i21ksc = blpccm;
056700130829                 if i21ksc=0;
056800130829                    i21ksc=(blplnp*10000) + 8888;
056900130829                 endif;
057000130829              endif;
057100130701           trul21r(trul21ds);
057200130701        endif;
057300130701
057400130701        // L'indicazione della consegna prevista viene fatta solo se la
057500130701        // spedizione non prevede contatti col destinatario:
057600130701
057700130701           // No Fermo deposito
057800130701        if blpffd = *blanks and (
057900130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
058000130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
058100130701        (blpdcr  > *zeros and blptcr = *blanks))
058200130701           // No contrassegno oppure importo che non richiede
058300130701           // l'abilitazione "manuale"
058400130701        and (ar9cas = *zeros or o21fca = *blanks);
058500130701           wcliper = 'S';
058600130701        endif;
058700130701
058800130701
058900130701        // Se non è indicata la data consegna richiesta
059000130701        // viene calcolata mediante pgm di calcolo delivery
059100130701        if wcliper = 'S' ;
059200160502         //  if blpdcr = *zeros;
059300130701              d98tbo = 'P';
059400130701              d98aas = blpaas;
059500130701              d98lnp = blplnp;
059600130701              d98nrs = blpnrs;
059700130701              d98nsp = blpnsp;
059800130701              if blptsp = 'D';
059900130701                 I98TSP_FOR = 'C';
060000130701              endif;
060100130701              tnsd99r (tnsd99ds);
060200130701              if d98err = '0';
060300151111                 wdcr = d98dee;
060400160502              else  ;
060500160502                 if blpdcr>0 ;
060600160502                  wdcr = blpdcr;
060700160502                 endif   ;
060800130701              endif;
060900160502          //        else;
061000160502          //           wdcr = blpdcr;
061100160502          //        endif;
061200160502
061300140327           wdcrRev = wdcr;
061400130701           if wdcr > *zeros;
061500130701              dataeur = %date(wdcr : *ISO);
061600130701              wdcr = %dec(dataeur);
061700130701           endif;
061800130701        endif;
061900140324
062000140324        wSpedConsOrari = '.';
062100141016        clear wSpedConsOraDa;
062200141016        clear wSpedConsOraA;
062300140324
062400140327        IOREDTA  = wdcrRev;
062500140324        IOREFIL  = blpLNA;
062600140324        IORECAP  = blpCAD;
062700140324        IORELOC  = blpLOD;
062800140324        IORENAR  = blpNZD;
062900140324        IORETSER = 'C';
063000140324        IOREDDC  = blpDDC;
063100140324        IORENDC  = blpNDC;
063200140324        IORETFP  = blpTFP;
063300140324        IORETFA  = blpTFA;
063400140324        IOREDTI  = blpDTI;
063500140324        IOREHTI  = blpHTI;
063600140324        IOREDCR  = blpDCR;
063700140324        IOREHCR  = blpHCR;
063800140324        IORETCR  = blpTCR;
063900140324        IOREDEI  = d98DEI;
064000140324        IOREOEI  = d98OEI;
064100140324        IORETSP  = blpTSP;
064200150730        §IORETRAZC = %editc(D98TTC:'X');
064300150730        §IORECONSC = %editc(D98TCC:'X');
064400150730        IOREFLO  = diore01;
064500140324
064600140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
064700140324        if not %error AND OOREERR = *blanks;
064800141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
064900141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
065000141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
065100141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
065200170202           wSpedConsOrari = '(' + wSpedConsOraDa + '-' +
065300150530                                  wSpedConsOraA  + ')';
065400140324        endif;
065500130626
065600130626        endsr;
065700130701
065800130701
065900130701
066000130701       //-------------------------------------------------------------*
066100130701       //Compongo il testo del SMS                                    *
066200130701       //-------------------------------------------------------------*
066300130701        Begsr valTextSMS;
066400130701
066500130703          wSmsText = *blanks;
066600131104          wTracking = *blanks;
066700140423          clear wConsegna;
066800131104
066900131104          if bologgi='S';
067000150530             wTracking = ' Verifichi da domani su www.brt.it';
067100131104          else;
067200150530             wTracking = ' Verifichi su www.brt.it';
067300131104          endif;
067400131104
067500130703
067600130701          if blpffd='S';
067700150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
067800140423
067900141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
068000150530                 //wConsegna = 'Potrà ritirare dal ' +
068100141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
068200141014                 wConsegna = '';
068300140423
068400150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
068500150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068600130701          else;
068700130701            if wcliper = 'S'and wdcr > *zeros ;
068800130701               select;
068900130701               when blptcr=*blanks;
069000150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
069100140423
069200150530                       wConsegna = ' Consegna prevista il ' +
069300140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
069400140423                       %trim(wSpedConsOrari);
069500140423
069600150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
069700150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069800150530
069900130701               when blptcr='D';
070000150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
070100140423
070200150530                       wConsegna = ' Consegna prevista dopo il ' +
070300140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
070400140423                       %trim(wSpedConsOrari);
070500140423
070600150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
070700150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
070800130701               when blptcr='P';
070900150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
071000140423
071100150530                       wConsegna = ' Consegna prevista prima del ' +
071200140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
071300140423                       %trim(wSpedConsOrari);
071400140423
071500150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
071600150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
071700130701               endsl;
071800130701
071900130701              else;
072000150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
072100150530                   wSmsText = %trim(wSmsText) + ' ' +
072200150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
072300130701              endif;
072400130701          endif;
072500141128
072600141128
072700141128          // Se consegna con "PINcode" aggiungo reminder
072800141128          if wPINcode = 'S';
072900141128             wSmsText = %trim(wSmsText) + ' ' +
073000141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
073100141128                      ' l''inserimento del PIN di sicurezza.';
073200141128          endif;
073300141128
073400130701
073500130701        endsr;
073600170126
073700170126
073800170126
073900170126       //-------------------------------------------------------------*
074000170126       //Compongo il testo del SMS - NEW                              *
074100170202 ccc   //-------------------------------------------------------------*
074200170202 xxx    Begsr valTextSMSnew;
074300170126
074400170126          wSmsText = *blanks;
074500170202          wTracking = *blanks;
074600170126          clear wConsegna;
074700170126
074800170126
074900170126          if blpffd='S';
075000170202             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23));
075100170126
075200170126             // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
075300170126             //wConsegna = 'Potrà ritirare dal ' +
075400170126             //%trim(%editw(wdcr:'0  /  /    '));
075500170126             wConsegna = '';
075600170202             wTracking = 'Info più precise su www.brt.it';
075700170126
075800170202             wSmsText = %trim(wSmsText) + ' ' + %trim(wConsegna) + ' ' +
075900170202                        %trim(wTracking) + '  BRTcode ' + wBRTcodeLong;
076000170126          else;
076100170126
076200170126           // Verifico condizioni particolari della bolla
076300170126           select;
076400170126             // Appuntamento
076500170126             when wcliper <> 'S' and (blptc1 = 'A' or blptc2 = 'A');
076600170126               wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23)) +
076700170202                        ' affidata il ' +
076800170126               %subst(%editc(blpaas*10000+blpmgs:'X'):7:2) + '/' +
076900170202               %subst(%editc(blpaas*10000+blpmgs:'X'):5:2) +
077000170202                      ' prevede consegna su appuntamento che puoi' +
077100170202                      ' programmare subito su www.brt.it';
077200170126
077300170202               wSmsText = %trim(wSmsText) + '  BRTcode ' + wBRTcodeLong;
077400170126
077500170126             // Altre
077600170126             other;
077700170126
077800170126              // Abilitazioni alla consegna
077900170126              if wcliper = 'S'and wdcr > *zeros ;
078000170126                 select;
078100170126                 when blptcr=*blanks;
078200170202                   wSmsText='Sped. da '+%trim(%subst(wRSM_Sms:1:23))+' ';
078300170126
078400170202                   wConsegna = ' Prevista consegna ' +
078500170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
078600170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
078700170202                      %trim(wSpedConsOrari) +
078800170215                      ' Puoi riprogrammare la consegna su www.brt.it';
078900170126
079000170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
079100170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
079200170126
079300170126                 when blptcr='D';
079400170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
079500170126
079600170126                   wConsegna = ' Prevista consegna dopo il ' +
079700170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
079800170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
079900170202                      %trim(wSpedConsOrari) +
080000170215                      ' Puoi riprogrammare la consegna su www.brt.it';
080100170126
080200170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
080300170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
080400170126
080500170126                 when blptcr='P';
080600170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
080700170126
080800170126                   wConsegna = ' Prevista consegna prima del ' +
080900170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
081000170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
081100170202                      %trim(wSpedConsOrari) +
081200170215                      ' Puoi riprogrammare la consegna su www.brt.it';
081300170126
081400170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
081500170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
081600170126                 endsl;
081700170126
081800170126              else;
081900170127                 wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
082000170202                 wTracking = 'Info più precise su www.brt.it';
082100170126                 wSmsText = %trim(wSmsText) + ' ' +
082200170126                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
082300170126              endif;
082400170126           endsl;
082500170126          endif;
082600170126
082700170126
082800170126          // Se consegna con "PINcode" aggiungo reminder
082900170126          if wPINcode = 'S';
083000170126             wSmsText = %trim(wSmsText) + ' ' +
083100170126                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
083200170126                      ' l''inserimento del PIN di sicurezza.';
083300170126          endif;
083400170126
083500170126        endsr;
083600130703
083700130703
083800130703
083900130703       //-------------------------------------------------------------*
084000130703       //Scrittura dato di output per creazione file massivo SMS      *
084100130703       //-------------------------------------------------------------*
084200130703        Begsr exeVGD;
084300131015
084400130704           o_MittSMS = 'BRT SpA';
084500130704           o_message = wSmsText;
084600130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
084700130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
084800130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
084900130704                       'SAAS';
085000130704
085100140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
085200140805           if bolok = 'S';
085300170126
085400141016              select;
085500141016                when chiudi = 'T';
085600170127                     o_cell = '393473441969';
085700141016              endsl;
085800160706
085900160706              o_message = %scanrpl(wSep:'':o_message);
086000170126
086100170126              clear tivgd000;
086200140805
086300161205              vgdDTA = %trim(o_MittSMS) + wSep +
086400140805                       %trim(o_IdSMS)   + wSep +
086500140805                       %trim(o_cell)    + wSep +
086600140805                       %trim(o_message);
086700161205
086800161205              // Scrittura TIVGD x file SMS
086900170126              vgdtip = 'SM';
087000170126              vgdksu = '0BART001';
087100170126              vgdtsc = 'WW';
087200170126              vgddat = currDate;
087300170126              vgdprg = wVASPRG;
087400170126              vgdpgm = 'FNLSSMR';
087500170127              vgdsto = '?';
087600170126
087700170126              write tivgd000;
087800140805
087900140805              // Incremento il conteggio degli SMS inseriti nel file corrente
088000140805              wCurRecFile = wCurRecFile + 1;
088100141201              wCurCivetta = wCurCivetta + 1;
088200140414
088300150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
088400150930              exsr sr_InsRA;
088500150930
088600140805           endif;
088700140414
088800130703        endsr;
088900170126
089000140414
089100141201
089200141201       //-------------------------------------------------------------*
089300141201       //Scrittura dato di output per SMS "civetta"                   *
089400141201       //-------------------------------------------------------------*
089500141201        Begsr exeVGDcivetta;
089600141201
089700141201
089800141201           o_MittSMS = 'BRT SpA';
089900141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
090000141201                                                %char(%time());
090100141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
090200141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
090300141201           o_cell = '393471487911';
090400170126
090500170126           o_message = %scanrpl(wSep:'':o_message);
090600170126
090700170126           clear tivgd000;
090800141201
090900161205           vgdDTA = %trim(o_MittSMS) + wSep +
091000141201                    %trim(o_IdSMS)   + wSep +
091100141201                    %trim(o_cell)    + wSep +
091200141201                    %trim(o_message);
091300170126
091400170126           // Scrittura TIVGD x file SMS
091500170126           vgdtip = 'SM';
091600170126           vgdksu = '0BART001';
091700170126           vgdtsc = 'WW';
091800170126           vgddat = currDate;
091900170126           vgdprg = wVASPRG;
092000170126           vgdpgm = 'FNLSSMR';
092100170127           vgdsto = '?';
092200170126
092300170126           write tivgd000;
092400141201
092500141201        endsr;
092600170126
092700141201
092800140414
092900140414       //-------------------------------------------------------------*
093000140414       //inserimento Richiesta Assistenza                             *
093100140414       //-------------------------------------------------------------*
093200140414        Begsr sr_InsRA;
093300141016
093400141016         if chiudi = *blanks;
093500140414
093600140805           // Inserimento R.A. via driver
093700140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
093800140414                        %trim(%editc(blpNRS:'X')) +
093900140414                        %trim(%editc(blpNSP:'X')) +
094000140414                        %trim(%editc(blpAAS:'X'));
094100140414
094200140414           clear fidna6ds;
094300140414
094400140423           iDA06MAD = ' 82';
094500140908
094600140908           iDA06RSC = 'SMS';
094700140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
094800140609                      '  -  ' + wConsegna;
094900140414           iDA06TOR = 'S';
095000140414           iDA06OGG = %trim(wSpedNum);
095100140414           iDA06UTE = 'QPGMR';
095200140610           iDA06FIL = blpLNP;
095300140414
095400140414           callP(e) FIDNA6R (fidna6ds);
095500160506
095600160506           // Inserimento spia per Predict DPD
095700160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
095800160506               exsr insPredict;
095900160506           endif;
096000141016
096100141016         endif;
096200140414
096300140414        endsr;
096400170126
096500160506
096600160506
096700160506       //-------------------------------------------------------------*
096800160506       //inserimento Predict DPD                                      *
096900160506       //-------------------------------------------------------------*
097000160506        Begsr insPredict;
097100160506
097200160506           clear tidp3000;
097300160506           dp3AAS   = blpAAS;
097400160506           dp3LNP   = blpLNP;
097500160506           dp3NRS   = blpNRS;
097600160506           dp3NSP   = blpNSP;
097700160506           dp3EVD   = '18';
097800160506           dp3CEV   = 'S25';
097900160506           dp3FLE   = blpLNA;
098000160518           dp3DEV   = 99991231;
098100160512           dp3HEV   = %dec(%time() : *HMS)/100;
098200160512           dp3DTSPE = blpAAS*10000+blpMGS;
098300160506           write tidp3000;
098400160506
098500160506        endsr;
098600170126
098700140414
098800140414
098900130829       //-------------------------------------------------------------*
099000130829       //caricamento codici bolla Franco                               *
099100130829       //-------------------------------------------------------------*
099200130829       Begsr SR_cartab;
099300130829
099400130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
099500130829       y=0;
099600130829       clear cbf;
099700130829       setll (1:'3A') tabel00f  ;
099800130829       reade (1:'3A') tabel00f  ;
099900130829
100000130829  1    dow    not %eof(tabel00f)  ;
100100130829          ds3a=tbluni           ;
100200130829          if %subst(§3atb1:1:1)='F';
100300130829             y=y+1;
100400130829             cbf(y)=tblkey;
100500130829          endif;
100600130829          reade (1:'3A') tabel00f  ;
100700130829  1    enddo     ;
100800141128
100900141128
101000141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
101100141128       clear sk7R_PINcode;
101200141128       clear ix7R;
101300141128       setll (1:'7R') tabel00f;
101400141128       reade (1:'7R') tabel00f;
101500141128
101600141128       dow    not %eof(tabel00f);
101700141128          ds7r=tbluni;
101800141128          if §7RPINCODE = 'S';
101900141128             ix7R = ix7R + 1;
102000141128             sk7R_PINcode(ix7r)=tblkey;
102100141128          endif;
102200141128          reade (1:'7R') tabel00f;
102300141128       enddo;
102400150929
102500150929
102600150929       // Carico la lista di filiali "estere" (secondo il network)
102700150929       clear skFilExt;
102800150929       clear ixFilExt;
102900150929       setll (*zeros) azorg01l;
103000150929       read azorg01l;
103100150929
103200150929       dow not %eof(azorg01l);
103300150929           if ORGFVA = *blanks;
103400150929              og143 = orgDE3;
103500150929              chain ('NTW':§OGNTW) tntbe01l;
103600150929              if %found(tntbe01l);
103700150929                 dntw = tbeuni;
103800150929                 if §NTWFIE = 'E';
103900150929                    ixFilExt = ixFilExt + 1;
104000150929                    skFilExt(ixFilExt) = orgFIL;
104100160506                    if §OGNTW = 'DPD';
104200160506                       skFilExtDPD(ixFilExt) = orgFIL;
104300160506                    endif;
104400150929                 endif;
104500150929              endif;
104600150929           endif;
104700150929           read azorg01l;
104800150929       enddo;
104900130703
105000130829       endsr;
105100170126
105200170126
105300170126
105400170126       //-------------------------------------------------------------*
105500170126       //caricamento filiali abilitate al deploy                      *
105600170126       //-------------------------------------------------------------*
105700170126       Begsr sr_VPOR;
105800170126
105900170127          clear wVPOdeploy;
106000170126
106100170126          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
106200170126          clear TRULVPODS;
106300170127          IVPOke1 = 'DECOFIIDC';
106400170126          TRULVPOR (trulvpods);
106500170126          skFil_New = skFilvpo;
106600170126
106700170126          select;
106800170126            // se al primo elemento c'è 999 => tutto a modo nuovo
106900170126            when skFil_New(1) = '999';
107000170127                 wVPOdeploy = 'NUOVO';
107100170126
107200170126            // se al primo elemento c'è *blanks => tutto a modo vecchio
107300170127            when skFil_New(1) = *zeros;
107400170127                 wVPOdeploy = 'VECCHIO';
107500170126
107600170126            other;
107700170127                 wVPOdeploy = 'MISTO';
107800170126          endsl;
107900170126
108000170126       endsr;
108100170126
108200170126
108300170126
108400170126       //-------------------------------------------------------------*
108500170126       //Effettuo controlli pre invio 2                               *
108600170126       //-------------------------------------------------------------*
108700170126       Begsr sr_ChkDeploy;
108800170126
108900170126          *in40 = *off;
109000170126
109100170126          select;
109200170127            when wVPOdeploy = 'VECCHIO';
109300170126              *in40  = *off;
109400170126
109500170127            when wVPOdeploy = 'NUOVO';
109600170126              *in40  = *on;
109700170126
109800170127            when wVPOdeploy = 'MISTO';
109900170126              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
110000170126                *in40 = *on;
110100170126              endif;
110200170126          endsl;
110300170126
110400170126       endsr;
110500170126
