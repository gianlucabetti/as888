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
021300170126          clear tis7vasds;
021400170127          i§7VASOPZ  = 'PRG';
021500170126          callP(e) tis7vasr1 (tis7vasds);
021600130626
021700170126          if %error OR o§7VASOK = *off;
021800161205             wProcedi = 'N';
021900170126          else;
022000170126             wVASPRG = o§7VASPRG;
022100161205          endif;
022200130626
022300130626        endsr;
022400161205
022500161205
022600161205
022700161205       //-------------------------------------------------------------*
022800161205       // Fine transazione TIVGD x file SMS
022900161205       //-------------------------------------------------------------*
023000161205        Begsr endVGD;
023100170126
023200170126          clear tis7vasds;
023300170127          I§7VASOPZ  = 'RLS';
023400170126          I§7VASTIP  = 'SM';
023500170126          I§7VASKSU  = '0BART001';
023600170126          I§7VASTSC  = 'WW';
023700170127          I§7VASPRG  = wVASPRG;
023800170126          callP(e) tis7vasr1 (tis7vasds);
023900170126
024000170126          if %error OR o§7VASOK = *off;
024100170126             dump(A);
024200170126             rolbk;
024300170126             *inlr = *on;
024400170126             return;
024500170126          endif;
024600161205
024700161205        endsr;
024800130626
024900130626
025000130626
025100130626       //-------------------------------------------------------------*
025200130626       //Reperimento progressivo                                      *
025300130626       //-------------------------------------------------------------*
025400130626        Begsr sr_rtvPrg;
025500130626
025600130626          clear trul33ds;
025700130626          i33cnu = 24;
025800130626          i33num = 1;
025900130626          kpjbu = trul33ds;
026000130626          trul33r (kpjba);
026100130626          trul33ds = kpjbu;
026200130626
026300130626        endsr;
026400130626
026500130406
026600130406
026700110617       //-------------------------------------------------------------*
026800110617       //Lettura fiar5 record "EMD" da inviare                         *
026900110617       //-------------------------------------------------------------*
027000110617       Begsr SR_Elabora;
027100130626
027200130626        kdos = *blanks;
027300130626        setll (kdos) fiar504l;
027400130626        reade (kdos) fiar504l;
027500110617
027600130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
027700130406
027800130626           // controllo se è stata richiesta la chiusura del sottosistema
027900110627           if %shtdn;
028000130626               chiudi = 'S';
028100110627               leavesr;
028200110627           endif;
028300141201
028400141201           // Gestione "civette"
028500141201           if wCurCivetta >= wMaxCivetta;
028600141201              exsr exeVGDcivetta;
028700141201              wCurCivetta = *zeros;
028800141201           endif;
028900141201
029000130406
029100130626           // controllo lo stato della bolla
029200110617           exsr sr_chkstblp;
029300140805           if bolok <> *blanks;
029400130719
029500130703              // inizializzo DS di work
029600130703              clear DS_Output;
029700130701              clear fiar9000;
029800130703
029900130703              // provo a reperire il contrassegno
030000130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
030100130703
030200130703              // compongo l'output per l'invio SMS
030300130703              dar5emd = ar5uni;
030400130719
030500130719              // normalizzo e verifico il numero di cellulare
030600130719              *in33 = *off;
030700130719              exsr sr_normCELL;
030800130719              if not *in33;
030900130719                 exsr sr_inviaSMS;
031000130719              endif;
031100130703
031200130703              // aggiorno il record corrente come SMS inviato
031300150930              exsr sr_UpdFIAR5;
031400150930
031500110617           endif;
031600130406
031700130701           reade (kdos) fiar504l;
031800110617        enddo;
031900130406
032000130626       endsr;
032100150930
032200150930
032300150930
032400150930       //-------------------------------------------------------------*
032500150930       // Gestione tentativi esecuzioni comandi                       *
032600150930       //-------------------------------------------------------------*
032700150930        Begsr sr_UpdFIAR5;
032800150930
032900150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
033000150930          §ar5DPC  = %editc(wdcrRev:'X');
033100150930          §ar5OPCD = %editc(OOR2STIS:'X');
033200150930          §ar5OPCA = %editc(OOR2STFS:'X');
033300150930          // Verifico che i valori sopra attribuiti siano diversi da zero
033400150930          if §AR5DPC  = *all'0';
033500150930             §AR5DPC  = *blanks;
033600160330             §AR5OPCD = *zeros;
033700160330             §AR5OPCA = *zeros;
033800150930          endif;
033900150930          if §AR5OPCD = *all'0';
034000150930             §AR5OPCD = *blanks;
034100150930          endif;
034200150930          if §AR5OPCA = *all'0';
034300150930             §AR5OPCA = *blanks;
034400150930          endif;
034500150930          ar5uni=dar5emd;
034600150930          if chiudi = *blanks;
034700150930             update fiar5000;
034800150930          endif;
034900150930
035000150930        endsr;
035100130406
035200130626
035300130406
035400110617       //-------------------------------------------------------------*
035500110617       //Verifica stato della bolla                                   *
035600110617       //-------------------------------------------------------------*
035700130625        Begsr sr_chkstblp;
035800130406
035900130626           bolok=' ';
036000131104           bologgi=' ';
036100130626
036200130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
036300130626           // delle comunicazioni ai destinatari
036400130626           clear fnlvemds;
036500130626           clear fnblpds;
036600140725           clear wRSM_Sms;
036700130626           iemAAS = ar5aas;
036800130626           iemLNP = ar5lnp;
036900130626           iemNRS = ar5nrs;
037000130626           iemNSP = ar5nsp;
037100130626           kpjbu = fnlvemds;
037200130626           callP(e) FNLVEMR (kpjba : fnblpds);
037300130626           fnlvemds = kpjbu;
037400140725
037500130626
037600130626           // Verifico se esito favorevole al invio
037700130626           if oemESITO = '1';
037800150929
037900150929           // Verifico che NON trattasi di spedizione "export"
038000150929           if  %lookup(blpLNA:skFilExt) > 0;
038100150929               bolok = 'E';
038200150929           endif;
038300150929
038400150929           // Se tutto OK => procedo
038500150929           if bolok <> 'E';
038600140725
038700170221              // Verifico se per cliente corrente presente forzatura
038800140725              // per Rag.Soc. invio alerts
038900170221              UBRSMALDS.iTpRic   = 'S';
039000170221              UBRSMALDS.iOrigine = 'P';
039100170221              UBRSMALDS.iAAS     = blpAAS;
039200170221              UBRSMALDS.iLNP     = blpLNP;
039300170221              UBRSMALDS.iNRS     = blpNRS;
039400170221              UBRSMALDS.iNSP     = blpNSP;
039500170221              UBRSMALR_S ( UBRSMALDS );
039600170221              if UBRSMALDS.oEsito = *zeros;
039700170221                 wRSM_Sms = %trim(UBRSMALDS.oRagSoc);
039800170221              else;
039900170221                 wRSM_Sms = %trim(blpRSM);
040000170221              endif;
040100141016
040200141016              // Calcolo il BRTcode (tramite apposito driver)
040300141016              clear BRTcode;
040400141016              BRTcode.aas4    = blpAAS;
040500141016              BRTcode.lnp     = blpLNP;
040600141016              BRTcode.nrs     = blpNRS;
040700141016              BRTcode.nsp     = blpNSP;
040800141016
040900141016              clear trul28ds0;
041000141016              trul28ds0.i280COD = BRTcode.BRTcode;
041100141016              callP(e) TRUL28R0 (trul28ds0);
041200141016
041300141016              // Se BRTcode errato => esito KO
041400141016              clear wBRTcodeLong;
041500141016              if %error OR trul28ds0.o280err = '1';
041600141016              else;
041700170127                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
041800170127                                %subst(trul28ds0.o280cod:6:5) +
041900170127                                %subst(trul28ds0.o280cod:11:5)+
042000141016                                %subst(trul28ds0.o280cod:16:4);
042100141016              endif;
042200131104
042300141016
042400131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
042500131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
042600131104                 bolok='S';
042700131104              endif;
042800131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
042900131104                 %dec(%time():*ISO)  >= 150000;
043000131104                 bolok='S';
043100131104                 bologgi='S';
043200131104              endif;
043300140805
043400140805              if bolok='S';
043500140805                 // Verifico per sicurezza se già presente evento 'MIC'
043600140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
043700140805                 if %found(fnevb04l);
043800140805                    bolok='X';
043900140805                 endif;
044000140805              endif;
044100150929
044200150929           endif;
044300131104
044400130626           endif;
044500130406
044600110617        endsr;
044700130719
044800130719
044900130719
045000130719       //-------------------------------------------------------------*
045100130719       //Normalizza numero ci cellulare                               *
045200130719       //-------------------------------------------------------------*
045300130719        Begsr sr_normCELL;
045400130719
045500131017           // Chiamo il driver preposto
045600131017           pInCell = %trim(§ar5TEL);
045700131017           pOutErr = *blanks;
045800131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
045900131017              §ar5SME = 'E';
046000131017              *in33 = *on;
046100131017           else;
046200131017              o_cell = pOutCell;
046300131017           endif;
046400130719
046500130719        endsr;
046600130406
046700130626
046800130406
046900110617       //-------------------------------------------------------------*
047000130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
047100110617       //-------------------------------------------------------------*
047200130626        Begsr sr_inviaSMS;
047300130719
047400130719          // Stacco un progressivo per il file SMS corrente
047500130719          if not *in34;
047600130719             exsr sr_rtvPrg;
047700130719             *in34 = *on;
047800130719          endif;
047900130406
048000130626          // determino data consegnabilità
048100130626          exsr sr_calcons;
048200130626
048300170127          // Verifoco se per linea arrivo corrente previsto nuovo deploy
048400170127          exsr sr_ChkDeploy;
048500170127
048600170126          // Compongo il testo del SMS e gestisco modo vecchio/nuovo
048700170126          if not *in40;
048800170126             exsr valTextSMS;
048900170126          else;
049000170126             exsr valTextSMSnew;
049100170126          endif;
049200130626
049300130626          // Aggiungo l'SMS corrente
049400130626          exsr exeVGD;
049500130406
049600110617        endsr;
049700130626
049800130626
049900130626
050000130626       //-------------------------------------------------------------*
050100130626       //Verifica eventuali personalizzazioni per il cliente          *
050200130626       //-------------------------------------------------------------*
050300130701        Begsr sr_calcons;
050400130626
050500130701        clear wcliper;
050600130701        clear wdcr;
050700140327        clear wdcrRev;
050800141128        clear wPINcode;
050900140331        clear tnsd99ds;
051000140324        clear trulORsds;
051100140324        clear trulOR2ds;
051200150730        clear diore01;
051300141128
051400141128        // Verifica se spedizione con particolarità consegna "PINcode"
051500141128        if blpGMA <> *blanks;
051600141128           if %lookup(blpGMA:sk7R_PINcode)>0;
051700141128              wPINcode = 'S';
051800141128            endif;
051900141128        endif;
052000130701
052100130701        // Se presente il contrassegno verifica se richiesto il contatto
052200130701        if ar9cas > 0;
052300130701           clear trul21ds;
052400130701           i21tla = 'L';
052500130701           i21cbo = blpcbo;
052600130701           i21tsp = blptsp;
052700130701           i21lnp = blplnp;
052800130701           i21nzm = blpnzm;
052900130701           i21lna = blplna;
053000130701           i21nzd = blpnzd;
053100130701           i21tic = ar9tic;
053200130701           i21imp = ar9cas;
053300130701           i21div = ar9vca;
053400130701           i21ute = sdsusr;
053500130701           i21pgm = nompgm;
053600130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
053700130829              if %lookup(blpcbo:cbf)>0;
053800130829                 i21ksc = blpksc;
053900130829              else;
054000130829                 i21ksc = blpccm;
054100130829                 if i21ksc=0;
054200130829                    i21ksc=(blplnp*10000) + 8888;
054300130829                 endif;
054400130829              endif;
054500130701           trul21r(trul21ds);
054600130701        endif;
054700130701
054800130701        // L'indicazione della consegna prevista viene fatta solo se la
054900130701        // spedizione non prevede contatti col destinatario:
055000130701
055100130701           // No Fermo deposito
055200130701        if blpffd = *blanks and (
055300130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
055400130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
055500130701        (blpdcr  > *zeros and blptcr = *blanks))
055600130701           // No contrassegno oppure importo che non richiede
055700130701           // l'abilitazione "manuale"
055800130701        and (ar9cas = *zeros or o21fca = *blanks);
055900130701           wcliper = 'S';
056000130701        endif;
056100130701
056200130701
056300130701        // Se non è indicata la data consegna richiesta
056400130701        // viene calcolata mediante pgm di calcolo delivery
056500130701        if wcliper = 'S' ;
056600160502         //  if blpdcr = *zeros;
056700130701              d98tbo = 'P';
056800130701              d98aas = blpaas;
056900130701              d98lnp = blplnp;
057000130701              d98nrs = blpnrs;
057100130701              d98nsp = blpnsp;
057200130701              if blptsp = 'D';
057300130701                 I98TSP_FOR = 'C';
057400130701              endif;
057500130701              tnsd99r (tnsd99ds);
057600130701              if d98err = '0';
057700151111                 wdcr = d98dee;
057800160502              else  ;
057900160502                 if blpdcr>0 ;
058000160502                  wdcr = blpdcr;
058100160502                 endif   ;
058200130701              endif;
058300160502          //        else;
058400160502          //           wdcr = blpdcr;
058500160502          //        endif;
058600160502
058700140327           wdcrRev = wdcr;
058800130701           if wdcr > *zeros;
058900130701              dataeur = %date(wdcr : *ISO);
059000130701              wdcr = %dec(dataeur);
059100130701           endif;
059200130701        endif;
059300140324
059400140324        wSpedConsOrari = '.';
059500141016        clear wSpedConsOraDa;
059600141016        clear wSpedConsOraA;
059700140324
059800140327        IOREDTA  = wdcrRev;
059900140324        IOREFIL  = blpLNA;
060000140324        IORECAP  = blpCAD;
060100140324        IORELOC  = blpLOD;
060200140324        IORENAR  = blpNZD;
060300140324        IORETSER = 'C';
060400140324        IOREDDC  = blpDDC;
060500140324        IORENDC  = blpNDC;
060600140324        IORETFP  = blpTFP;
060700140324        IORETFA  = blpTFA;
060800140324        IOREDTI  = blpDTI;
060900140324        IOREHTI  = blpHTI;
061000140324        IOREDCR  = blpDCR;
061100140324        IOREHCR  = blpHCR;
061200140324        IORETCR  = blpTCR;
061300140324        IOREDEI  = d98DEI;
061400140324        IOREOEI  = d98OEI;
061500140324        IORETSP  = blpTSP;
061600150730        §IORETRAZC = %editc(D98TTC:'X');
061700150730        §IORECONSC = %editc(D98TCC:'X');
061800150730        IOREFLO  = diore01;
061900140324
062000140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
062100140324        if not %error AND OOREERR = *blanks;
062200141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
062300141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
062400141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
062500141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
062600170202           wSpedConsOrari = '(' + wSpedConsOraDa + '-' +
062700150530                                  wSpedConsOraA  + ')';
062800140324        endif;
062900130626
063000130626        endsr;
063100130701
063200130701
063300130701
063400130701       //-------------------------------------------------------------*
063500130701       //Compongo il testo del SMS                                    *
063600130701       //-------------------------------------------------------------*
063700130701        Begsr valTextSMS;
063800130701
063900130703          wSmsText = *blanks;
064000131104          wTracking = *blanks;
064100140423          clear wConsegna;
064200131104
064300131104          if bologgi='S';
064400150530             wTracking = ' Verifichi da domani su www.brt.it';
064500131104          else;
064600150530             wTracking = ' Verifichi su www.brt.it';
064700131104          endif;
064800131104
064900130703
065000130701          if blpffd='S';
065100150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
065200140423
065300141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
065400150530                 //wConsegna = 'Potrà ritirare dal ' +
065500141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
065600141014                 wConsegna = '';
065700140423
065800150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
065900150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
066000130701          else;
066100130701            if wcliper = 'S'and wdcr > *zeros ;
066200130701               select;
066300130701               when blptcr=*blanks;
066400150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
066500140423
066600150530                       wConsegna = ' Consegna prevista il ' +
066700140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
066800140423                       %trim(wSpedConsOrari);
066900140423
067000150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
067100150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
067200150530
067300130701               when blptcr='D';
067400150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
067500140423
067600150530                       wConsegna = ' Consegna prevista dopo il ' +
067700140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
067800140423                       %trim(wSpedConsOrari);
067900140423
068000150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
068100150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068200130701               when blptcr='P';
068300150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
068400140423
068500150530                       wConsegna = ' Consegna prevista prima del ' +
068600140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
068700140423                       %trim(wSpedConsOrari);
068800140423
068900150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
069000150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069100130701               endsl;
069200130701
069300130701              else;
069400150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
069500150530                   wSmsText = %trim(wSmsText) + ' ' +
069600150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069700130701              endif;
069800130701          endif;
069900141128
070000141128
070100141128          // Se consegna con "PINcode" aggiungo reminder
070200141128          if wPINcode = 'S';
070300141128             wSmsText = %trim(wSmsText) + ' ' +
070400141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
070500141128                      ' l''inserimento del PIN di sicurezza.';
070600141128          endif;
070700141128
070800130701
070900130701        endsr;
071000170126
071100170126
071200170126
071300170126       //-------------------------------------------------------------*
071400170126       //Compongo il testo del SMS - NEW                              *
071500170202 ccc   //-------------------------------------------------------------*
071600170202 xxx    Begsr valTextSMSnew;
071700170126
071800170126          wSmsText = *blanks;
071900170202          wTracking = *blanks;
072000170126          clear wConsegna;
072100170126
072200170126
072300170126          if blpffd='S';
072400170202             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23));
072500170126
072600170126             // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
072700170126             //wConsegna = 'Potrà ritirare dal ' +
072800170126             //%trim(%editw(wdcr:'0  /  /    '));
072900170126             wConsegna = '';
073000170202             wTracking = 'Info più precise su www.brt.it';
073100170126
073200170202             wSmsText = %trim(wSmsText) + ' ' + %trim(wConsegna) + ' ' +
073300170202                        %trim(wTracking) + '  BRTcode ' + wBRTcodeLong;
073400170126          else;
073500170126
073600170126           // Verifico condizioni particolari della bolla
073700170126           select;
073800170126             // Appuntamento
073900170126             when wcliper <> 'S' and (blptc1 = 'A' or blptc2 = 'A');
074000170126               wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23)) +
074100170202                        ' affidata il ' +
074200170126               %subst(%editc(blpaas*10000+blpmgs:'X'):7:2) + '/' +
074300170202               %subst(%editc(blpaas*10000+blpmgs:'X'):5:2) +
074400170202                      ' prevede consegna su appuntamento che puoi' +
074500170202                      ' programmare subito su www.brt.it';
074600170126
074700170202               wSmsText = %trim(wSmsText) + '  BRTcode ' + wBRTcodeLong;
074800170126
074900170126             // Altre
075000170126             other;
075100170126
075200170126              // Abilitazioni alla consegna
075300170126              if wcliper = 'S'and wdcr > *zeros ;
075400170126                 select;
075500170126                 when blptcr=*blanks;
075600170202                   wSmsText='Sped. da '+%trim(%subst(wRSM_Sms:1:23))+' ';
075700170126
075800170202                   wConsegna = ' Prevista consegna ' +
075900170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
076000170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
076100170202                      %trim(wSpedConsOrari) +
076200170215                      ' Puoi riprogrammare la consegna su www.brt.it';
076300170126
076400170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
076500170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
076600170126
076700170126                 when blptcr='D';
076800170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
076900170126
077000170126                   wConsegna = ' Prevista consegna dopo il ' +
077100170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
077200170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
077300170202                      %trim(wSpedConsOrari) +
077400170215                      ' Puoi riprogrammare la consegna su www.brt.it';
077500170126
077600170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
077700170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
077800170126
077900170126                 when blptcr='P';
078000170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
078100170126
078200170126                   wConsegna = ' Prevista consegna prima del ' +
078300170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
078400170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
078500170202                      %trim(wSpedConsOrari) +
078600170215                      ' Puoi riprogrammare la consegna su www.brt.it';
078700170126
078800170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
078900170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
079000170126                 endsl;
079100170126
079200170126              else;
079300170127                 wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
079400170202                 wTracking = 'Info più precise su www.brt.it';
079500170126                 wSmsText = %trim(wSmsText) + ' ' +
079600170126                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
079700170126              endif;
079800170126           endsl;
079900170126          endif;
080000170126
080100170126
080200170126          // Se consegna con "PINcode" aggiungo reminder
080300170126          if wPINcode = 'S';
080400170126             wSmsText = %trim(wSmsText) + ' ' +
080500170126                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
080600170126                      ' l''inserimento del PIN di sicurezza.';
080700170126          endif;
080800170126
080900170126        endsr;
081000130703
081100130703
081200130703
081300130703       //-------------------------------------------------------------*
081400130703       //Scrittura dato di output per creazione file massivo SMS      *
081500130703       //-------------------------------------------------------------*
081600130703        Begsr exeVGD;
081700131015
081800130704           o_MittSMS = 'BRT SpA';
081900130704           o_message = wSmsText;
082000130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
082100130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
082200130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
082300130704                       'SAAS';
082400130704
082500140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
082600140805           if bolok = 'S';
082700170126
082800141016              select;
082900141016                when chiudi = 'T';
083000170127                     o_cell = '393473441969';
083100141016              endsl;
083200160706
083300160706              o_message = %scanrpl(wSep:'':o_message);
083400170126
083500170126              clear tivgd000;
083600140805
083700161205              vgdDTA = %trim(o_MittSMS) + wSep +
083800140805                       %trim(o_IdSMS)   + wSep +
083900140805                       %trim(o_cell)    + wSep +
084000140805                       %trim(o_message);
084100161205
084200161205              // Scrittura TIVGD x file SMS
084300170126              vgdtip = 'SM';
084400170126              vgdksu = '0BART001';
084500170126              vgdtsc = 'WW';
084600170126              vgddat = currDate;
084700170126              vgdprg = wVASPRG;
084800170126              vgdpgm = 'FNLSSMR';
084900170127              vgdsto = '?';
085000170126
085100170126              write tivgd000;
085200140805
085300140805              // Incremento il conteggio degli SMS inseriti nel file corrente
085400140805              wCurRecFile = wCurRecFile + 1;
085500141201              wCurCivetta = wCurCivetta + 1;
085600140414
085700150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
085800150930              exsr sr_InsRA;
085900150930
086000140805           endif;
086100140414
086200130703        endsr;
086300170126
086400140414
086500141201
086600141201       //-------------------------------------------------------------*
086700141201       //Scrittura dato di output per SMS "civetta"                   *
086800141201       //-------------------------------------------------------------*
086900141201        Begsr exeVGDcivetta;
087000141201
087100141201
087200141201           o_MittSMS = 'BRT SpA';
087300141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
087400141201                                                %char(%time());
087500141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
087600141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
087700141201           o_cell = '393471487911';
087800170126
087900170126           o_message = %scanrpl(wSep:'':o_message);
088000170126
088100170126           clear tivgd000;
088200141201
088300161205           vgdDTA = %trim(o_MittSMS) + wSep +
088400141201                    %trim(o_IdSMS)   + wSep +
088500141201                    %trim(o_cell)    + wSep +
088600141201                    %trim(o_message);
088700170126
088800170126           // Scrittura TIVGD x file SMS
088900170126           vgdtip = 'SM';
089000170126           vgdksu = '0BART001';
089100170126           vgdtsc = 'WW';
089200170126           vgddat = currDate;
089300170126           vgdprg = wVASPRG;
089400170126           vgdpgm = 'FNLSSMR';
089500170127           vgdsto = '?';
089600170126
089700170126           write tivgd000;
089800141201
089900141201        endsr;
090000170126
090100141201
090200140414
090300140414       //-------------------------------------------------------------*
090400140414       //inserimento Richiesta Assistenza                             *
090500140414       //-------------------------------------------------------------*
090600140414        Begsr sr_InsRA;
090700141016
090800141016         if chiudi = *blanks;
090900140414
091000140805           // Inserimento R.A. via driver
091100140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
091200140414                        %trim(%editc(blpNRS:'X')) +
091300140414                        %trim(%editc(blpNSP:'X')) +
091400140414                        %trim(%editc(blpAAS:'X'));
091500140414
091600140414           clear fidna6ds;
091700140414
091800140423           iDA06MAD = ' 82';
091900140908
092000140908           iDA06RSC = 'SMS';
092100140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
092200140609                      '  -  ' + wConsegna;
092300140414           iDA06TOR = 'S';
092400140414           iDA06OGG = %trim(wSpedNum);
092500140414           iDA06UTE = 'QPGMR';
092600140610           iDA06FIL = blpLNP;
092700140414
092800140414           callP(e) FIDNA6R (fidna6ds);
092900160506
093000160506           // Inserimento spia per Predict DPD
093100160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
093200160506               exsr insPredict;
093300160506           endif;
093400141016
093500141016         endif;
093600140414
093700140414        endsr;
093800170126
093900160506
094000160506
094100160506       //-------------------------------------------------------------*
094200160506       //inserimento Predict DPD                                      *
094300160506       //-------------------------------------------------------------*
094400160506        Begsr insPredict;
094500160506
094600160506           clear tidp3000;
094700160506           dp3AAS   = blpAAS;
094800160506           dp3LNP   = blpLNP;
094900160506           dp3NRS   = blpNRS;
095000160506           dp3NSP   = blpNSP;
095100160506           dp3EVD   = '18';
095200160506           dp3CEV   = 'S25';
095300160506           dp3FLE   = blpLNA;
095400160518           dp3DEV   = 99991231;
095500160512           dp3HEV   = %dec(%time() : *HMS)/100;
095600160512           dp3DTSPE = blpAAS*10000+blpMGS;
095700160506           write tidp3000;
095800160506
095900160506        endsr;
096000170126
096100140414
096200140414
096300130829       //-------------------------------------------------------------*
096400130829       //caricamento codici bolla Franco                               *
096500130829       //-------------------------------------------------------------*
096600130829       Begsr SR_cartab;
096700130829
096800130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
096900130829       y=0;
097000130829       clear cbf;
097100130829       setll (1:'3A') tabel00f  ;
097200130829       reade (1:'3A') tabel00f  ;
097300130829
097400130829  1    dow    not %eof(tabel00f)  ;
097500130829          ds3a=tbluni           ;
097600130829          if %subst(§3atb1:1:1)='F';
097700130829             y=y+1;
097800130829             cbf(y)=tblkey;
097900130829          endif;
098000130829          reade (1:'3A') tabel00f  ;
098100130829  1    enddo     ;
098200141128
098300141128
098400141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
098500141128       clear sk7R_PINcode;
098600141128       clear ix7R;
098700141128       setll (1:'7R') tabel00f;
098800141128       reade (1:'7R') tabel00f;
098900141128
099000141128       dow    not %eof(tabel00f);
099100141128          ds7r=tbluni;
099200141128          if §7RPINCODE = 'S';
099300141128             ix7R = ix7R + 1;
099400141128             sk7R_PINcode(ix7r)=tblkey;
099500141128          endif;
099600141128          reade (1:'7R') tabel00f;
099700141128       enddo;
099800150929
099900150929
100000150929       // Carico la lista di filiali "estere" (secondo il network)
100100150929       clear skFilExt;
100200150929       clear ixFilExt;
100300150929       setll (*zeros) azorg01l;
100400150929       read azorg01l;
100500150929
100600150929       dow not %eof(azorg01l);
100700150929           if ORGFVA = *blanks;
100800150929              og143 = orgDE3;
100900150929              chain ('NTW':§OGNTW) tntbe01l;
101000150929              if %found(tntbe01l);
101100150929                 dntw = tbeuni;
101200150929                 if §NTWFIE = 'E';
101300150929                    ixFilExt = ixFilExt + 1;
101400150929                    skFilExt(ixFilExt) = orgFIL;
101500160506                    if §OGNTW = 'DPD';
101600160506                       skFilExtDPD(ixFilExt) = orgFIL;
101700160506                    endif;
101800150929                 endif;
101900150929              endif;
102000150929           endif;
102100150929           read azorg01l;
102200150929       enddo;
102300130703
102400130829       endsr;
102500170126
102600170126
102700170126
102800170126       //-------------------------------------------------------------*
102900170126       //caricamento filiali abilitate al deploy                      *
103000170126       //-------------------------------------------------------------*
103100170126       Begsr sr_VPOR;
103200170126
103300170127          clear wVPOdeploy;
103400170126
103500170126          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
103600170126          clear TRULVPODS;
103700170127          IVPOke1 = 'DECOFIIDC';
103800170126          TRULVPOR (trulvpods);
103900170126          skFil_New = skFilvpo;
104000170126
104100170126          select;
104200170126            // se al primo elemento c'è 999 => tutto a modo nuovo
104300170126            when skFil_New(1) = '999';
104400170127                 wVPOdeploy = 'NUOVO';
104500170126
104600170126            // se al primo elemento c'è *blanks => tutto a modo vecchio
104700170127            when skFil_New(1) = *zeros;
104800170127                 wVPOdeploy = 'VECCHIO';
104900170126
105000170126            other;
105100170127                 wVPOdeploy = 'MISTO';
105200170126          endsl;
105300170126
105400170126       endsr;
105500170126
105600170126
105700170126
105800170126       //-------------------------------------------------------------*
105900170126       //Effettuo controlli pre invio 2                               *
106000170126       //-------------------------------------------------------------*
106100170126       Begsr sr_ChkDeploy;
106200170126
106300170126          *in40 = *off;
106400170126
106500170126          select;
106600170127            when wVPOdeploy = 'VECCHIO';
106700170126              *in40  = *off;
106800170126
106900170127            when wVPOdeploy = 'NUOVO';
107000170126              *in40  = *on;
107100170126
107200170127            when wVPOdeploy = 'MISTO';
107300170126              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
107400170126                *in40 = *on;
107500170126              endif;
107600170126          endsl;
107700170126
107800170126       endsr;
107900170126
