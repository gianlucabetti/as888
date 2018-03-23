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
004300170221     D UBRSMALDS     E DS                  QUALIFIED
004400130626
004500130626
004600110617      * - Campi di comodo
004700130829     d y               s              3  0 inz
004800130829     d cbf             S              2    DIM(100) inz
004900140805     d bolok           s              1    inz
005000140805     d bologgi         s              1    inz
005100130626     d kdos            s                   like(ar5dos) inz
005200130703     d wSmsText        s           1600    inz
005300131104     d wTracking       s            100    inz
005400130626     d wProcedi        s              1    inz
005500130701     d wcliper         s              1    inz
005600141128     d wPINcode        s              1    inz
005700130701     d Dataeur         s               d   datfmt(*eur)
005800131004     d currDate        s              8  0 inz
005900130701     d wdcr            s                   like(blpdcr)
006000140327     d wdcrRev         s                   like(blpdcr)
006100130704     d wSep            s              1    inz('|')
006200130704     d wPos            s              2  0 inz
006300130719     d wEsito          s              1    inz
006400141202     d wMaxRecFile     s              4  0 inz(200)
006500130920     d wCurRecFile     s              4  0 inz
006600141201     d wMaxCivetta     s              4  0 inz(5000)
006700141201     d wCurCivetta     s              4  0 inz
006800170221     d wBRTcodeLong    s             25    inz
006900170221     d wSpedConsOrari  s             64A   inz
007000170221     d wSpedConsOraDa  s              5A   inz
007100170221     d wSpedConsOraA   s              5A   inz
007200170221     d wSpedNum        s             16A   inz
007300170221     d wConsegna       s           1000A   varying inz
007400170221     d wRSM_Sms        s             60A   inz
007500141128     d sk7R_PINcode    s              2    dim(100) inz
007600141128     d ix7R            s              3  0 inz
007700150929     D skFilExt        s                   like(orgFIL) dim(500) inz
007800160506     D skFilExtDPD     s                   like(orgFIL) dim(500) inz
007900150929     D ixFilExt        s              3  0 inz
008000170126     D wVPOdeploy      s              6A   inz
008100170126     D wVASPRG         s                   like(O§7VASPRG) inz
008200141016
008300141016
008400141016     D BRTcode         ds                  qualified
008500141016     D   aas4                         4  0
008600141016     D   lnp                          3  0
008700141016     D   nrs                          2  0
008800141016     D   nsp                          7  0
008900141016     D   chkdgt1                      1  0
009000141016     D   code                         3  0
009100141016     D   chkdgt2                      1  0
009200141016     D   BRTcode               3     21
009300130704
009400130703
009500130703     d* DS di output
009600130703     d  DS_Output      ds                  inz
009700130704     d   o_MittSMS                   30
009800130704     d   o_IdSMS                     36
009900130703     d   o_cell                      20
010000130703     d   o_message                 1600
010100170126
010200170126
010300170126      // DS per filiali abilitate al deploy
010400170126     D TRULVPODS     e ds
010500170126     D  skFilvpo              16    765    dim(250)
010600170126     D skFil_New       s                   like(skFilvpo) dim(250) inz
010700130406
010800130625
010900130701     d fnlvemr         PR
011000130626     d                                     extpgm('FNLVEMR')
011100130701     d  kpjba                              likeds(kpjba)
011200130701     d  fnblpds                            likeds(fnblpds)
011300140414
011400140414
011500140414     d FIDNA6R         PR
011600140414     d                                     extpgm('FIDNA6R')
011700140414     d fidna6ds                            likeds(fidna6ds)
011800140414
011900140414
012000130701     d trul21r         PR
012100130701     d                                     EXTPGM('TRUL21R')
012200130701     d  trul21ds                           likeds(trul21ds)
012300141016
012400141016
012500141016     D TRUL28R0        PR
012600141016     D                                     extpgm('TRUL28R0')
012700141016     D trul28ds0                           likeds(trul28ds0)
012800130701
012900130701
013000130626     d trul33r         PR
013100130626     d                                     extpgm('TRUL33R')
013200130626     d  kpjba                              likeds(kpjba)
013300140324
013400140324
013500140324     D TRULORSR        PR
013600140324     D                                     extpgm('TRULORSR')
013700140324     D kpjba                               likeds(kpjba)
013800140324     D trulORsds                           likeds(trulORsds)
013900140324     D trulOR2ds                           likeds(trulOR2ds)
014000130719
014100161205
014200170126     D tis7vasr1       PR
014300170126     D                                     extpgm('TIS7VASR1')
014400170126     D  tis7vasds                          like(tis7vasds)
014500170126
014600170126
014700170126     D TRULVPOR        PR
014800170126     D                                     extpgm('TRULVPOR')
014900170126     D trulvpods                           likeds(TRULVPODS)
015000130719
015100110617
015200110617      // ? PROTOTIPI ?
015300120619      /copy gaitrasrc/srcprotopr,tnsd99r
015400131017      /copy gaitrasrc/srcprotopr,ubchkcel
015500131017      /copy gaitrasrc/srcprotopi,ubchkcel
015600170221      /copy gaitrasrc/srcProtoPr,UBRSMALR
015700110617
015800130406
015900110627     C     *ENTRY        PLIST
016000110627     C                   PARM                    chiudi            1
016100130406
016200110617      /free
016300130626
016400140731       // Inizializzozioni
016500140731       wProcedi = 'S';
016600140805       wCurRecFile = *zeros;
016700131015
016800130829       // Carico le tabelle occorrenti
016900130829       exsr sr_cartab;
017000170126
017100170126       // carico le filiali per rilasci pilotati
017200170126       exsr sr_VPOR;
017300131004
017400131004       // Inizializzo variabili di procedura
017500131004       currDate = %dec(%date() : *ISO);
017600131004
017700161205       // Avvio Inizio transazione TIVGD x file SMS
017800170126       clear wVASPRG;
017900161205       exsr strVGD;
018000131004
018100130701       // Se consentita elaborazione => procedo
018200140731       if wProcedi <> 'N';
018300130701          // Leggo fiar5 rk "EMD" x SMS da inviare
018400130719          *in34 = *off;
018500130626          exsr sr_Elabora;
018600131004
018700170126          // Fine transazione TIVGD x file SMS
018800170126          exsr endVGD;
018900170126       endif;
019000130406
019100130626       // Gestisco chiusura controllata
019200130626       if chiudi = 'S' or %shtdn;
019300130625          clear fnlvemds;
019400130625          iemTLA = 'C';
019500130625          kpjbu = fnlvemds;
019600130625          callP(e) FNLVEMR (kpjba : fnblpds);
019700161205          *inlr = *on;
019800140623       else;
019900161205          *inrt = *on;
020000110627       endif;
020100140617
020200140623       unlock(e) fiar504l;
020300130626
020400161205       return;
020500161205
020600130626
020700130626
020800130626       //-------------------------------------------------------------*
020900161205       // Avvio Inizio transazione TIVGD x file SMS
021000130626       //-------------------------------------------------------------*
021100161205        Begsr strVGD;
021200161205
021300170329          wVASPRG = 'XX00000000';
021400130626
021500130626        endsr;
021600161205
021700161205
021800161205
021900161205       //-------------------------------------------------------------*
022000161205       // Fine transazione TIVGD x file SMS
022100161205       //-------------------------------------------------------------*
022200161205        Begsr endVGD;
022300170126
022400170126          clear tis7vasds;
022500170127          I§7VASOPZ  = 'RLS';
022600170126          I§7VASTIP  = 'SM';
022700170126          I§7VASKSU  = '0BART001';
022800170126          I§7VASTSC  = 'WW';
022900170127          I§7VASPRG  = wVASPRG;
023000170126          callP(e) tis7vasr1 (tis7vasds);
023100170126
023200170126          if %error OR o§7VASOK = *off;
023300170126             dump(A);
023400170322             rolbk(e);
023500170126             *inlr = *on;
023600170126             return;
023700170126          endif;
023800161205
023900161205        endsr;
024000130626
024100130626
024200130626
024300130626       //-------------------------------------------------------------*
024400130626       //Reperimento progressivo                                      *
024500130626       //-------------------------------------------------------------*
024600130626        Begsr sr_rtvPrg;
024700130626
024800130626          clear trul33ds;
024900130626          i33cnu = 24;
025000130626          i33num = 1;
025100130626          kpjbu = trul33ds;
025200130626          trul33r (kpjba);
025300130626          trul33ds = kpjbu;
025400130626
025500130626        endsr;
025600130626
025700130406
025800130406
025900110617       //-------------------------------------------------------------*
026000110617       //Lettura fiar5 record "EMD" da inviare                         *
026100110617       //-------------------------------------------------------------*
026200110617       Begsr SR_Elabora;
026300130626
026400130626        kdos = *blanks;
026500130626        setll (kdos) fiar504l;
026600130626        reade (kdos) fiar504l;
026700110617
026800130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
026900130406
027000130626           // controllo se è stata richiesta la chiusura del sottosistema
027100110627           if %shtdn;
027200130626               chiudi = 'S';
027300110627               leavesr;
027400110627           endif;
027500141201
027600141201           // Gestione "civette"
027700141201           if wCurCivetta >= wMaxCivetta;
027800141201              exsr exeVGDcivetta;
027900141201              wCurCivetta = *zeros;
028000141201           endif;
028100141201
028200130406
028300130626           // controllo lo stato della bolla
028400110617           exsr sr_chkstblp;
028500140805           if bolok <> *blanks;
028600130719
028700130703              // inizializzo DS di work
028800130703              clear DS_Output;
028900130701              clear fiar9000;
029000130703
029100130703              // provo a reperire il contrassegno
029200130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
029300130703
029400130703              // compongo l'output per l'invio SMS
029500130703              dar5emd = ar5uni;
029600130719
029700130719              // normalizzo e verifico il numero di cellulare
029800130719              *in33 = *off;
029900130719              exsr sr_normCELL;
030000130719              if not *in33;
030100130719                 exsr sr_inviaSMS;
030200130719              endif;
030300130703
030400130703              // aggiorno il record corrente come SMS inviato
030500150930              exsr sr_UpdFIAR5;
030600150930
030700110617           endif;
030800130406
030900130701           reade (kdos) fiar504l;
031000110617        enddo;
031100130406
031200130626       endsr;
031300150930
031400150930
031500150930
031600150930       //-------------------------------------------------------------*
031700150930       // Gestione tentativi esecuzioni comandi                       *
031800150930       //-------------------------------------------------------------*
031900150930        Begsr sr_UpdFIAR5;
032000150930
032100150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
032200150930          §ar5DPC  = %editc(wdcrRev:'X');
032300150930          §ar5OPCD = %editc(OOR2STIS:'X');
032400150930          §ar5OPCA = %editc(OOR2STFS:'X');
032500150930          // Verifico che i valori sopra attribuiti siano diversi da zero
032600150930          if §AR5DPC  = *all'0';
032700150930             §AR5DPC  = *blanks;
032800160330             §AR5OPCD = *zeros;
032900160330             §AR5OPCA = *zeros;
033000150930          endif;
033100150930          if §AR5OPCD = *all'0';
033200150930             §AR5OPCD = *blanks;
033300150930          endif;
033400150930          if §AR5OPCA = *all'0';
033500150930             §AR5OPCA = *blanks;
033600150930          endif;
033700150930          ar5uni=dar5emd;
033800150930          if chiudi = *blanks;
033900150930             update fiar5000;
034000150930          endif;
034100150930
034200150930        endsr;
034300130406
034400130626
034500130406
034600110617       //-------------------------------------------------------------*
034700110617       //Verifica stato della bolla                                   *
034800110617       //-------------------------------------------------------------*
034900130625        Begsr sr_chkstblp;
035000130406
035100130626           bolok=' ';
035200131104           bologgi=' ';
035300130626
035400130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
035500130626           // delle comunicazioni ai destinatari
035600130626           clear fnlvemds;
035700130626           clear fnblpds;
035800140725           clear wRSM_Sms;
035900130626           iemAAS = ar5aas;
036000130626           iemLNP = ar5lnp;
036100130626           iemNRS = ar5nrs;
036200130626           iemNSP = ar5nsp;
036300130626           kpjbu = fnlvemds;
036400130626           callP(e) FNLVEMR (kpjba : fnblpds);
036500130626           fnlvemds = kpjbu;
036600140725
036700130626
036800130626           // Verifico se esito favorevole al invio
036900130626           if oemESITO = '1';
037000150929
037100150929           // Verifico che NON trattasi di spedizione "export"
037200150929           if  %lookup(blpLNA:skFilExt) > 0;
037300150929               bolok = 'E';
037400150929           endif;
037500150929
037600150929           // Se tutto OK => procedo
037700150929           if bolok <> 'E';
037800140725
037900170221              // Verifico se per cliente corrente presente forzatura
038000140725              // per Rag.Soc. invio alerts
038100170221              UBRSMALDS.iTpRic   = 'S';
038200170221              UBRSMALDS.iOrigine = 'P';
038300170221              UBRSMALDS.iAAS     = blpAAS;
038400170221              UBRSMALDS.iLNP     = blpLNP;
038500170221              UBRSMALDS.iNRS     = blpNRS;
038600170221              UBRSMALDS.iNSP     = blpNSP;
038700170221              UBRSMALR_S ( UBRSMALDS );
038800170221              if UBRSMALDS.oEsito = *zeros;
038900170221                 wRSM_Sms = %trim(UBRSMALDS.oRagSoc);
039000170221              else;
039100170221                 wRSM_Sms = %trim(blpRSM);
039200170221              endif;
039300141016
039400141016              // Calcolo il BRTcode (tramite apposito driver)
039500141016              clear BRTcode;
039600141016              BRTcode.aas4    = blpAAS;
039700141016              BRTcode.lnp     = blpLNP;
039800141016              BRTcode.nrs     = blpNRS;
039900141016              BRTcode.nsp     = blpNSP;
040000141016
040100141016              clear trul28ds0;
040200141016              trul28ds0.i280COD = BRTcode.BRTcode;
040300141016              callP(e) TRUL28R0 (trul28ds0);
040400141016
040500141016              // Se BRTcode errato => esito KO
040600141016              clear wBRTcodeLong;
040700141016              if %error OR trul28ds0.o280err = '1';
040800141016              else;
040900170127                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
041000170127                                %subst(trul28ds0.o280cod:6:5) +
041100170127                                %subst(trul28ds0.o280cod:11:5)+
041200141016                                %subst(trul28ds0.o280cod:16:4);
041300141016              endif;
041400131104
041500141016
041600131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
041700131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
041800131104                 bolok='S';
041900131104              endif;
042000131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
042100131104                 %dec(%time():*ISO)  >= 150000;
042200131104                 bolok='S';
042300131104                 bologgi='S';
042400131104              endif;
042500140805
042600140805              if bolok='S';
042700140805                 // Verifico per sicurezza se già presente evento 'MIC'
042800140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
042900140805                 if %found(fnevb04l);
043000140805                    bolok='X';
043100140805                 endif;
043200140805              endif;
043300150929
043400150929           endif;
043500131104
043600130626           endif;
043700130406
043800110617        endsr;
043900130719
044000130719
044100130719
044200130719       //-------------------------------------------------------------*
044300130719       //Normalizza numero ci cellulare                               *
044400130719       //-------------------------------------------------------------*
044500130719        Begsr sr_normCELL;
044600130719
044700131017           // Chiamo il driver preposto
044800131017           pInCell = %trim(§ar5TEL);
044900131017           pOutErr = *blanks;
045000131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
045100131017              §ar5SME = 'E';
045200131017              *in33 = *on;
045300131017           else;
045400131017              o_cell = pOutCell;
045500131017           endif;
045600130719
045700130719        endsr;
045800130406
045900130626
046000130406
046100110617       //-------------------------------------------------------------*
046200130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
046300110617       //-------------------------------------------------------------*
046400130626        Begsr sr_inviaSMS;
046500130719
046600130719          // Stacco un progressivo per il file SMS corrente
046700130719          if not *in34;
046800130719             exsr sr_rtvPrg;
046900130719             *in34 = *on;
047000130719          endif;
047100130406
047200130626          // determino data consegnabilità
047300130626          exsr sr_calcons;
047400130626
047500170127          // Verifoco se per linea arrivo corrente previsto nuovo deploy
047600170127          exsr sr_ChkDeploy;
047700170127
047800170126          // Compongo il testo del SMS e gestisco modo vecchio/nuovo
047900170126          if not *in40;
048000170126             exsr valTextSMS;
048100170126          else;
048200170126             exsr valTextSMSnew;
048300170126          endif;
048400130626
048500130626          // Aggiungo l'SMS corrente
048600130626          exsr exeVGD;
048700130406
048800110617        endsr;
048900130626
049000130626
049100130626
049200130626       //-------------------------------------------------------------*
049300130626       //Verifica eventuali personalizzazioni per il cliente          *
049400130626       //-------------------------------------------------------------*
049500130701        Begsr sr_calcons;
049600130626
049700130701        clear wcliper;
049800130701        clear wdcr;
049900140327        clear wdcrRev;
050000141128        clear wPINcode;
050100140331        clear tnsd99ds;
050200140324        clear trulORsds;
050300140324        clear trulOR2ds;
050400150730        clear diore01;
050500141128
050600141128        // Verifica se spedizione con particolarità consegna "PINcode"
050700141128        if blpGMA <> *blanks;
050800141128           if %lookup(blpGMA:sk7R_PINcode)>0;
050900141128              wPINcode = 'S';
051000141128            endif;
051100141128        endif;
051200130701
051300130701        // Se presente il contrassegno verifica se richiesto il contatto
051400130701        if ar9cas > 0;
051500130701           clear trul21ds;
051600130701           i21tla = 'L';
051700130701           i21cbo = blpcbo;
051800130701           i21tsp = blptsp;
051900130701           i21lnp = blplnp;
052000130701           i21nzm = blpnzm;
052100130701           i21lna = blplna;
052200130701           i21nzd = blpnzd;
052300130701           i21tic = ar9tic;
052400130701           i21imp = ar9cas;
052500130701           i21div = ar9vca;
052600130701           i21ute = sdsusr;
052700130701           i21pgm = nompgm;
052800130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
052900130829              if %lookup(blpcbo:cbf)>0;
053000130829                 i21ksc = blpksc;
053100130829              else;
053200130829                 i21ksc = blpccm;
053300130829                 if i21ksc=0;
053400130829                    i21ksc=(blplnp*10000) + 8888;
053500130829                 endif;
053600130829              endif;
053700130701           trul21r(trul21ds);
053800130701        endif;
053900130701
054000130701        // L'indicazione della consegna prevista viene fatta solo se la
054100130701        // spedizione non prevede contatti col destinatario:
054200130701
054300130701           // No Fermo deposito
054400130701        if blpffd = *blanks and (
054500130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
054600130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
054700130701        (blpdcr  > *zeros and blptcr = *blanks))
054800130701           // No contrassegno oppure importo che non richiede
054900130701           // l'abilitazione "manuale"
055000130701        and (ar9cas = *zeros or o21fca = *blanks);
055100130701           wcliper = 'S';
055200130701        endif;
055300130701
055400130701
055500130701        // Se non è indicata la data consegna richiesta
055600130701        // viene calcolata mediante pgm di calcolo delivery
055700130701        if wcliper = 'S' ;
055800160502         //  if blpdcr = *zeros;
055900130701              d98tbo = 'P';
056000130701              d98aas = blpaas;
056100130701              d98lnp = blplnp;
056200130701              d98nrs = blpnrs;
056300130701              d98nsp = blpnsp;
056400130701              if blptsp = 'D';
056500130701                 I98TSP_FOR = 'C';
056600130701              endif;
056700130701              tnsd99r (tnsd99ds);
056800130701              if d98err = '0';
056900151111                 wdcr = d98dee;
057000160502              else  ;
057100160502                 if blpdcr>0 ;
057200160502                  wdcr = blpdcr;
057300160502                 endif   ;
057400130701              endif;
057500160502          //        else;
057600160502          //           wdcr = blpdcr;
057700160502          //        endif;
057800160502
057900140327           wdcrRev = wdcr;
058000130701           if wdcr > *zeros;
058100130701              dataeur = %date(wdcr : *ISO);
058200130701              wdcr = %dec(dataeur);
058300130701           endif;
058400130701        endif;
058500140324
058600140324        wSpedConsOrari = '.';
058700141016        clear wSpedConsOraDa;
058800141016        clear wSpedConsOraA;
058900140324
059000140327        IOREDTA  = wdcrRev;
059100140324        IOREFIL  = blpLNA;
059200140324        IORECAP  = blpCAD;
059300140324        IORELOC  = blpLOD;
059400140324        IORENAR  = blpNZD;
059500140324        IORETSER = 'C';
059600140324        IOREDDC  = blpDDC;
059700140324        IORENDC  = blpNDC;
059800140324        IORETFP  = blpTFP;
059900140324        IORETFA  = blpTFA;
060000140324        IOREDTI  = blpDTI;
060100140324        IOREHTI  = blpHTI;
060200140324        IOREDCR  = blpDCR;
060300140324        IOREHCR  = blpHCR;
060400140324        IORETCR  = blpTCR;
060500140324        IOREDEI  = d98DEI;
060600140324        IOREOEI  = d98OEI;
060700140324        IORETSP  = blpTSP;
060800150730        §IORETRAZC = %editc(D98TTC:'X');
060900150730        §IORECONSC = %editc(D98TCC:'X');
061000150730        IOREFLO  = diore01;
061100140324
061200140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
061300140324        if not %error AND OOREERR = *blanks;
061400141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
061500141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
061600141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
061700141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
061800170202           wSpedConsOrari = '(' + wSpedConsOraDa + '-' +
061900150530                                  wSpedConsOraA  + ')';
062000140324        endif;
062100130626
062200130626        endsr;
062300130701
062400130701
062500130701
062600130701       //-------------------------------------------------------------*
062700130701       //Compongo il testo del SMS                                    *
062800130701       //-------------------------------------------------------------*
062900130701        Begsr valTextSMS;
063000130701
063100130703          wSmsText = *blanks;
063200131104          wTracking = *blanks;
063300140423          clear wConsegna;
063400131104
063500131104          if bologgi='S';
063600150530             wTracking = ' Verifichi da domani su www.brt.it';
063700131104          else;
063800150530             wTracking = ' Verifichi su www.brt.it';
063900131104          endif;
064000131104
064100130703
064200130701          if blpffd='S';
064300150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
064400140423
064500141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
064600150530                 //wConsegna = 'Potrà ritirare dal ' +
064700141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
064800141014                 wConsegna = '';
064900140423
065000150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
065100150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
065200130701          else;
065300130701            if wcliper = 'S'and wdcr > *zeros ;
065400130701               select;
065500130701               when blptcr=*blanks;
065600150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
065700140423
065800150530                       wConsegna = ' Consegna prevista il ' +
065900140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
066000140423                       %trim(wSpedConsOrari);
066100140423
066200150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
066300150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
066400150530
066500130701               when blptcr='D';
066600150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
066700140423
066800150530                       wConsegna = ' Consegna prevista dopo il ' +
066900140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
067000140423                       %trim(wSpedConsOrari);
067100140423
067200150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
067300150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
067400130701               when blptcr='P';
067500150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
067600140423
067700150530                       wConsegna = ' Consegna prevista prima del ' +
067800140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
067900140423                       %trim(wSpedConsOrari);
068000140423
068100150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
068200150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068300130701               endsl;
068400130701
068500130701              else;
068600150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
068700150530                   wSmsText = %trim(wSmsText) + ' ' +
068800150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068900130701              endif;
069000130701          endif;
069100141128
069200141128
069300141128          // Se consegna con "PINcode" aggiungo reminder
069400141128          if wPINcode = 'S';
069500141128             wSmsText = %trim(wSmsText) + ' ' +
069600141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
069700141128                      ' l''inserimento del PIN di sicurezza.';
069800141128          endif;
069900141128
070000130701
070100130701        endsr;
070200170126
070300170126
070400170126
070500170126       //-------------------------------------------------------------*
070600170126       //Compongo il testo del SMS - NEW                              *
070700170202 ccc   //-------------------------------------------------------------*
070800170202 xxx    Begsr valTextSMSnew;
070900170126
071000170126          wSmsText = *blanks;
071100170202          wTracking = *blanks;
071200170126          clear wConsegna;
071300170126
071400170126
071500170126          if blpffd='S';
071600170202             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23));
071700170126
071800170126             // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
071900170126             //wConsegna = 'Potrà ritirare dal ' +
072000170126             //%trim(%editw(wdcr:'0  /  /    '));
072100170126             wConsegna = '';
072200170202             wTracking = 'Info più precise su www.brt.it';
072300170126
072400170202             wSmsText = %trim(wSmsText) + ' ' + %trim(wConsegna) + ' ' +
072500170202                        %trim(wTracking) + '  BRTcode ' + wBRTcodeLong;
072600170126          else;
072700170126
072800170126           // Verifico condizioni particolari della bolla
072900170126           select;
073000170126             // Appuntamento
073100170126             when wcliper <> 'S' and (blptc1 = 'A' or blptc2 = 'A');
073200170126               wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23)) +
073300170202                        ' affidata il ' +
073400170126               %subst(%editc(blpaas*10000+blpmgs:'X'):7:2) + '/' +
073500170202               %subst(%editc(blpaas*10000+blpmgs:'X'):5:2) +
073600170202                      ' prevede consegna su appuntamento che puoi' +
073700170202                      ' programmare subito su www.brt.it';
073800170126
073900170202               wSmsText = %trim(wSmsText) + '  BRTcode ' + wBRTcodeLong;
074000170126
074100170126             // Altre
074200170126             other;
074300170126
074400170126              // Abilitazioni alla consegna
074500170126              if wcliper = 'S'and wdcr > *zeros ;
074600170126                 select;
074700170126                 when blptcr=*blanks;
074800170202                   wSmsText='Sped. da '+%trim(%subst(wRSM_Sms:1:23))+' ';
074900170126
075000170202                   wConsegna = ' Prevista consegna ' +
075100170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
075200170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
075300170202                      %trim(wSpedConsOrari) +
075400170215                      ' Puoi riprogrammare la consegna su www.brt.it';
075500170126
075600170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
075700170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
075800170126
075900170126                 when blptcr='D';
076000170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
076100170126
076200170126                   wConsegna = ' Prevista consegna dopo il ' +
076300170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
076400170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
076500170202                      %trim(wSpedConsOrari) +
076600170215                      ' Puoi riprogrammare la consegna su www.brt.it';
076700170126
076800170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
076900170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
077000170126
077100170126                 when blptcr='P';
077200170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
077300170126
077400170126                   wConsegna = ' Prevista consegna prima del ' +
077500170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
077600170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
077700170202                      %trim(wSpedConsOrari) +
077800170215                      ' Puoi riprogrammare la consegna su www.brt.it';
077900170126
078000170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
078100170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
078200170126                 endsl;
078300170126
078400170126              else;
078500170127                 wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
078600170202                 wTracking = 'Info più precise su www.brt.it';
078700170126                 wSmsText = %trim(wSmsText) + ' ' +
078800170126                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
078900170126              endif;
079000170126           endsl;
079100170126          endif;
079200170126
079300170126
079400170126          // Se consegna con "PINcode" aggiungo reminder
079500170126          if wPINcode = 'S';
079600170126             wSmsText = %trim(wSmsText) + ' ' +
079700170126                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
079800170126                      ' l''inserimento del PIN di sicurezza.';
079900170126          endif;
080000170126
080100170126        endsr;
080200130703
080300130703
080400130703
080500130703       //-------------------------------------------------------------*
080600130703       //Scrittura dato di output per creazione file massivo SMS      *
080700130703       //-------------------------------------------------------------*
080800130703        Begsr exeVGD;
080900131015
081000130704           o_MittSMS = 'BRT SpA';
081100130704           o_message = wSmsText;
081200130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
081300130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
081400130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
081500130704                       'SAAS';
081600130704
081700140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
081800140805           if bolok = 'S';
081900170126
082000141016              select;
082100141016                when chiudi = 'T';
082200170127                     o_cell = '393473441969';
082300141016              endsl;
082400160706
082500160706              o_message = %scanrpl(wSep:'':o_message);
082600170126
082700170126              clear tivgd000;
082800140805
082900161205              vgdDTA = %trim(o_MittSMS) + wSep +
083000140805                       %trim(o_IdSMS)   + wSep +
083100140805                       %trim(o_cell)    + wSep +
083200140805                       %trim(o_message);
083300161205
083400161205              // Scrittura TIVGD x file SMS
083500170126              vgdtip = 'SM';
083600170126              vgdksu = '0BART001';
083700170126              vgdtsc = 'WW';
083800170126              vgddat = currDate;
083900170126              vgdprg = wVASPRG;
084000170126              vgdpgm = 'FNLSSMR';
084100170127              vgdsto = '?';
084200170126
084300170126              write tivgd000;
084400140805
084500140805              // Incremento il conteggio degli SMS inseriti nel file corrente
084600140805              wCurRecFile = wCurRecFile + 1;
084700141201              wCurCivetta = wCurCivetta + 1;
084800140414
084900150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
085000150930              exsr sr_InsRA;
085100150930
085200140805           endif;
085300140414
085400130703        endsr;
085500170126
085600140414
085700141201
085800141201       //-------------------------------------------------------------*
085900141201       //Scrittura dato di output per SMS "civetta"                   *
086000141201       //-------------------------------------------------------------*
086100141201        Begsr exeVGDcivetta;
086200141201
086300141201
086400141201           o_MittSMS = 'BRT SpA';
086500141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
086600141201                                                %char(%time());
086700141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
086800141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
086900141201           o_cell = '393471487911';
087000170126
087100170126           o_message = %scanrpl(wSep:'':o_message);
087200170126
087300170126           clear tivgd000;
087400141201
087500161205           vgdDTA = %trim(o_MittSMS) + wSep +
087600141201                    %trim(o_IdSMS)   + wSep +
087700141201                    %trim(o_cell)    + wSep +
087800141201                    %trim(o_message);
087900170126
088000170126           // Scrittura TIVGD x file SMS
088100170126           vgdtip = 'SM';
088200170126           vgdksu = '0BART001';
088300170126           vgdtsc = 'WW';
088400170126           vgddat = currDate;
088500170126           vgdprg = wVASPRG;
088600170126           vgdpgm = 'FNLSSMR';
088700170127           vgdsto = '?';
088800170126
088900170126           write tivgd000;
089000141201
089100141201        endsr;
089200170126
089300141201
089400140414
089500140414       //-------------------------------------------------------------*
089600140414       //inserimento Richiesta Assistenza                             *
089700140414       //-------------------------------------------------------------*
089800140414        Begsr sr_InsRA;
089900141016
090000141016         if chiudi = *blanks;
090100140414
090200140805           // Inserimento R.A. via driver
090300140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
090400140414                        %trim(%editc(blpNRS:'X')) +
090500140414                        %trim(%editc(blpNSP:'X')) +
090600140414                        %trim(%editc(blpAAS:'X'));
090700140414
090800140414           clear fidna6ds;
090900140414
091000140423           iDA06MAD = ' 82';
091100140908
091200140908           iDA06RSC = 'SMS';
091300140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
091400140609                      '  -  ' + wConsegna;
091500140414           iDA06TOR = 'S';
091600140414           iDA06OGG = %trim(wSpedNum);
091700140414           iDA06UTE = 'QPGMR';
091800140610           iDA06FIL = blpLNP;
091900140414
092000140414           callP(e) FIDNA6R (fidna6ds);
092100160506
092200160506           // Inserimento spia per Predict DPD
092300160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
092400160506               exsr insPredict;
092500160506           endif;
092600141016
092700141016         endif;
092800140414
092900140414        endsr;
093000170126
093100160506
093200160506
093300160506       //-------------------------------------------------------------*
093400160506       //inserimento Predict DPD                                      *
093500160506       //-------------------------------------------------------------*
093600160506        Begsr insPredict;
093700160506
093800160506           clear tidp3000;
093900160506           dp3AAS   = blpAAS;
094000160506           dp3LNP   = blpLNP;
094100160506           dp3NRS   = blpNRS;
094200160506           dp3NSP   = blpNSP;
094300160506           dp3EVD   = '18';
094400160506           dp3CEV   = 'S25';
094500160506           dp3FLE   = blpLNA;
094600160518           dp3DEV   = 99991231;
094700160512           dp3HEV   = %dec(%time() : *HMS)/100;
094800160512           dp3DTSPE = blpAAS*10000+blpMGS;
094900160506           write tidp3000;
095000160506
095100160506        endsr;
095200170126
095300140414
095400140414
095500130829       //-------------------------------------------------------------*
095600130829       //caricamento codici bolla Franco                               *
095700130829       //-------------------------------------------------------------*
095800130829       Begsr SR_cartab;
095900130829
096000130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
096100130829       y=0;
096200130829       clear cbf;
096300130829       setll (1:'3A') tabel00f  ;
096400130829       reade (1:'3A') tabel00f  ;
096500130829
096600130829  1    dow    not %eof(tabel00f)  ;
096700130829          ds3a=tbluni           ;
096800130829          if %subst(§3atb1:1:1)='F';
096900130829             y=y+1;
097000130829             cbf(y)=tblkey;
097100130829          endif;
097200130829          reade (1:'3A') tabel00f  ;
097300130829  1    enddo     ;
097400141128
097500141128
097600141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
097700141128       clear sk7R_PINcode;
097800141128       clear ix7R;
097900141128       setll (1:'7R') tabel00f;
098000141128       reade (1:'7R') tabel00f;
098100141128
098200141128       dow    not %eof(tabel00f);
098300141128          ds7r=tbluni;
098400141128          if §7RPINCODE = 'S';
098500141128             ix7R = ix7R + 1;
098600141128             sk7R_PINcode(ix7r)=tblkey;
098700141128          endif;
098800141128          reade (1:'7R') tabel00f;
098900141128       enddo;
099000150929
099100150929
099200150929       // Carico la lista di filiali "estere" (secondo il network)
099300150929       clear skFilExt;
099400150929       clear ixFilExt;
099500150929       setll (*zeros) azorg01l;
099600150929       read azorg01l;
099700150929
099800150929       dow not %eof(azorg01l);
099900150929           if ORGFVA = *blanks;
100000150929              og143 = orgDE3;
100100150929              chain ('NTW':§OGNTW) tntbe01l;
100200150929              if %found(tntbe01l);
100300150929                 dntw = tbeuni;
100400150929                 if §NTWFIE = 'E';
100500150929                    ixFilExt = ixFilExt + 1;
100600150929                    skFilExt(ixFilExt) = orgFIL;
100700160506                    if §OGNTW = 'DPD';
100800160506                       skFilExtDPD(ixFilExt) = orgFIL;
100900160506                    endif;
101000150929                 endif;
101100150929              endif;
101200150929           endif;
101300150929           read azorg01l;
101400150929       enddo;
101500130703
101600130829       endsr;
101700170126
101800170126
101900170126
102000170126       //-------------------------------------------------------------*
102100170126       //caricamento filiali abilitate al deploy                      *
102200170126       //-------------------------------------------------------------*
102300170126       Begsr sr_VPOR;
102400170126
102500170127          clear wVPOdeploy;
102600170126
102700170126          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
102800170126          clear TRULVPODS;
102900170127          IVPOke1 = 'DECOFIIDC';
103000170126          TRULVPOR (trulvpods);
103100170126          skFil_New = skFilvpo;
103200170126
103300170126          select;
103400170126            // se al primo elemento c'è 999 => tutto a modo nuovo
103500170126            when skFil_New(1) = '999';
103600170127                 wVPOdeploy = 'NUOVO';
103700170126
103800170126            // se al primo elemento c'è *blanks => tutto a modo vecchio
103900170127            when skFil_New(1) = *zeros;
104000170127                 wVPOdeploy = 'VECCHIO';
104100170126
104200170126            other;
104300170127                 wVPOdeploy = 'MISTO';
104400170126          endsl;
104500170126
104600170126       endsr;
104700170126
104800170126
104900170126
105000170126       //-------------------------------------------------------------*
105100170126       //Effettuo controlli pre invio 2                               *
105200170126       //-------------------------------------------------------------*
105300170126       Begsr sr_ChkDeploy;
105400170126
105500170126          *in40 = *off;
105600170126
105700170126          select;
105800170127            when wVPOdeploy = 'VECCHIO';
105900170126              *in40  = *off;
106000170126
106100170127            when wVPOdeploy = 'NUOVO';
106200170126              *in40  = *on;
106300170126
106400170127            when wVPOdeploy = 'MISTO';
106500170126              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
106600170126                *in40 = *on;
106700170126              endif;
106800170126          endsl;
106900170126
107000170126       endsr;
107100170126
