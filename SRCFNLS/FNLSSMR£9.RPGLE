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
022100170329             %subst(wVASPRG:1:2) = 'XX';
022200161205          endif;
022300130626
022400130626        endsr;
022500161205
022600161205
022700161205
022800161205       //-------------------------------------------------------------*
022900161205       // Fine transazione TIVGD x file SMS
023000161205       //-------------------------------------------------------------*
023100161205        Begsr endVGD;
023200170126
023300170126          clear tis7vasds;
023400170127          I§7VASOPZ  = 'RLS';
023500170126          I§7VASTIP  = 'SM';
023600170126          I§7VASKSU  = '0BART001';
023700170126          I§7VASTSC  = 'WW';
023800170127          I§7VASPRG  = wVASPRG;
023900170126          callP(e) tis7vasr1 (tis7vasds);
024000170126
024100170126          if %error OR o§7VASOK = *off;
024200170126             dump(A);
024300170322             rolbk(e);
024400170126             *inlr = *on;
024500170126             return;
024600170126          endif;
024700161205
024800161205        endsr;
024900130626
025000130626
025100130626
025200130626       //-------------------------------------------------------------*
025300130626       //Reperimento progressivo                                      *
025400130626       //-------------------------------------------------------------*
025500130626        Begsr sr_rtvPrg;
025600130626
025700130626          clear trul33ds;
025800130626          i33cnu = 24;
025900130626          i33num = 1;
026000130626          kpjbu = trul33ds;
026100130626          trul33r (kpjba);
026200130626          trul33ds = kpjbu;
026300130626
026400130626        endsr;
026500130626
026600130406
026700130406
026800110617       //-------------------------------------------------------------*
026900110617       //Lettura fiar5 record "EMD" da inviare                         *
027000110617       //-------------------------------------------------------------*
027100110617       Begsr SR_Elabora;
027200130626
027300130626        kdos = *blanks;
027400130626        setll (kdos) fiar504l;
027500130626        reade (kdos) fiar504l;
027600110617
027700130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
027800130406
027900130626           // controllo se è stata richiesta la chiusura del sottosistema
028000110627           if %shtdn;
028100130626               chiudi = 'S';
028200110627               leavesr;
028300110627           endif;
028400141201
028500141201           // Gestione "civette"
028600141201           if wCurCivetta >= wMaxCivetta;
028700141201              exsr exeVGDcivetta;
028800141201              wCurCivetta = *zeros;
028900141201           endif;
029000141201
029100130406
029200130626           // controllo lo stato della bolla
029300110617           exsr sr_chkstblp;
029400140805           if bolok <> *blanks;
029500130719
029600130703              // inizializzo DS di work
029700130703              clear DS_Output;
029800130701              clear fiar9000;
029900130703
030000130703              // provo a reperire il contrassegno
030100130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
030200130703
030300130703              // compongo l'output per l'invio SMS
030400130703              dar5emd = ar5uni;
030500130719
030600130719              // normalizzo e verifico il numero di cellulare
030700130719              *in33 = *off;
030800130719              exsr sr_normCELL;
030900130719              if not *in33;
031000130719                 exsr sr_inviaSMS;
031100130719              endif;
031200130703
031300130703              // aggiorno il record corrente come SMS inviato
031400150930              exsr sr_UpdFIAR5;
031500150930
031600110617           endif;
031700130406
031800130701           reade (kdos) fiar504l;
031900110617        enddo;
032000130406
032100130626       endsr;
032200150930
032300150930
032400150930
032500150930       //-------------------------------------------------------------*
032600150930       // Gestione tentativi esecuzioni comandi                       *
032700150930       //-------------------------------------------------------------*
032800150930        Begsr sr_UpdFIAR5;
032900150930
033000150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
033100150930          §ar5DPC  = %editc(wdcrRev:'X');
033200150930          §ar5OPCD = %editc(OOR2STIS:'X');
033300150930          §ar5OPCA = %editc(OOR2STFS:'X');
033400150930          // Verifico che i valori sopra attribuiti siano diversi da zero
033500150930          if §AR5DPC  = *all'0';
033600150930             §AR5DPC  = *blanks;
033700160330             §AR5OPCD = *zeros;
033800160330             §AR5OPCA = *zeros;
033900150930          endif;
034000150930          if §AR5OPCD = *all'0';
034100150930             §AR5OPCD = *blanks;
034200150930          endif;
034300150930          if §AR5OPCA = *all'0';
034400150930             §AR5OPCA = *blanks;
034500150930          endif;
034600150930          ar5uni=dar5emd;
034700150930          if chiudi = *blanks;
034800150930             update fiar5000;
034900150930          endif;
035000150930
035100150930        endsr;
035200130406
035300130626
035400130406
035500110617       //-------------------------------------------------------------*
035600110617       //Verifica stato della bolla                                   *
035700110617       //-------------------------------------------------------------*
035800130625        Begsr sr_chkstblp;
035900130406
036000130626           bolok=' ';
036100131104           bologgi=' ';
036200130626
036300130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
036400130626           // delle comunicazioni ai destinatari
036500130626           clear fnlvemds;
036600130626           clear fnblpds;
036700140725           clear wRSM_Sms;
036800130626           iemAAS = ar5aas;
036900130626           iemLNP = ar5lnp;
037000130626           iemNRS = ar5nrs;
037100130626           iemNSP = ar5nsp;
037200130626           kpjbu = fnlvemds;
037300130626           callP(e) FNLVEMR (kpjba : fnblpds);
037400130626           fnlvemds = kpjbu;
037500140725
037600130626
037700130626           // Verifico se esito favorevole al invio
037800130626           if oemESITO = '1';
037900150929
038000150929           // Verifico che NON trattasi di spedizione "export"
038100150929           if  %lookup(blpLNA:skFilExt) > 0;
038200150929               bolok = 'E';
038300150929           endif;
038400150929
038500150929           // Se tutto OK => procedo
038600150929           if bolok <> 'E';
038700140725
038800170221              // Verifico se per cliente corrente presente forzatura
038900140725              // per Rag.Soc. invio alerts
039000170221              UBRSMALDS.iTpRic   = 'S';
039100170221              UBRSMALDS.iOrigine = 'P';
039200170221              UBRSMALDS.iAAS     = blpAAS;
039300170221              UBRSMALDS.iLNP     = blpLNP;
039400170221              UBRSMALDS.iNRS     = blpNRS;
039500170221              UBRSMALDS.iNSP     = blpNSP;
039600170221              UBRSMALR_S ( UBRSMALDS );
039700170221              if UBRSMALDS.oEsito = *zeros;
039800170221                 wRSM_Sms = %trim(UBRSMALDS.oRagSoc);
039900170221              else;
040000170221                 wRSM_Sms = %trim(blpRSM);
040100170221              endif;
040200141016
040300141016              // Calcolo il BRTcode (tramite apposito driver)
040400141016              clear BRTcode;
040500141016              BRTcode.aas4    = blpAAS;
040600141016              BRTcode.lnp     = blpLNP;
040700141016              BRTcode.nrs     = blpNRS;
040800141016              BRTcode.nsp     = blpNSP;
040900141016
041000141016              clear trul28ds0;
041100141016              trul28ds0.i280COD = BRTcode.BRTcode;
041200141016              callP(e) TRUL28R0 (trul28ds0);
041300141016
041400141016              // Se BRTcode errato => esito KO
041500141016              clear wBRTcodeLong;
041600141016              if %error OR trul28ds0.o280err = '1';
041700141016              else;
041800170127                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
041900170127                                %subst(trul28ds0.o280cod:6:5) +
042000170127                                %subst(trul28ds0.o280cod:11:5)+
042100141016                                %subst(trul28ds0.o280cod:16:4);
042200141016              endif;
042300131104
042400141016
042500131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
042600131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
042700131104                 bolok='S';
042800131104              endif;
042900131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
043000131104                 %dec(%time():*ISO)  >= 150000;
043100131104                 bolok='S';
043200131104                 bologgi='S';
043300131104              endif;
043400140805
043500140805              if bolok='S';
043600140805                 // Verifico per sicurezza se già presente evento 'MIC'
043700140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
043800140805                 if %found(fnevb04l);
043900140805                    bolok='X';
044000140805                 endif;
044100140805              endif;
044200150929
044300150929           endif;
044400131104
044500130626           endif;
044600130406
044700110617        endsr;
044800130719
044900130719
045000130719
045100130719       //-------------------------------------------------------------*
045200130719       //Normalizza numero ci cellulare                               *
045300130719       //-------------------------------------------------------------*
045400130719        Begsr sr_normCELL;
045500130719
045600131017           // Chiamo il driver preposto
045700131017           pInCell = %trim(§ar5TEL);
045800131017           pOutErr = *blanks;
045900131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
046000131017              §ar5SME = 'E';
046100131017              *in33 = *on;
046200131017           else;
046300131017              o_cell = pOutCell;
046400131017           endif;
046500130719
046600130719        endsr;
046700130406
046800130626
046900130406
047000110617       //-------------------------------------------------------------*
047100130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
047200110617       //-------------------------------------------------------------*
047300130626        Begsr sr_inviaSMS;
047400130719
047500130719          // Stacco un progressivo per il file SMS corrente
047600130719          if not *in34;
047700130719             exsr sr_rtvPrg;
047800130719             *in34 = *on;
047900130719          endif;
048000130406
048100130626          // determino data consegnabilità
048200130626          exsr sr_calcons;
048300130626
048400170127          // Verifoco se per linea arrivo corrente previsto nuovo deploy
048500170127          exsr sr_ChkDeploy;
048600170127
048700170126          // Compongo il testo del SMS e gestisco modo vecchio/nuovo
048800170126          if not *in40;
048900170126             exsr valTextSMS;
049000170126          else;
049100170126             exsr valTextSMSnew;
049200170126          endif;
049300130626
049400130626          // Aggiungo l'SMS corrente
049500130626          exsr exeVGD;
049600130406
049700110617        endsr;
049800130626
049900130626
050000130626
050100130626       //-------------------------------------------------------------*
050200130626       //Verifica eventuali personalizzazioni per il cliente          *
050300130626       //-------------------------------------------------------------*
050400130701        Begsr sr_calcons;
050500130626
050600130701        clear wcliper;
050700130701        clear wdcr;
050800140327        clear wdcrRev;
050900141128        clear wPINcode;
051000140331        clear tnsd99ds;
051100140324        clear trulORsds;
051200140324        clear trulOR2ds;
051300150730        clear diore01;
051400141128
051500141128        // Verifica se spedizione con particolarità consegna "PINcode"
051600141128        if blpGMA <> *blanks;
051700141128           if %lookup(blpGMA:sk7R_PINcode)>0;
051800141128              wPINcode = 'S';
051900141128            endif;
052000141128        endif;
052100130701
052200130701        // Se presente il contrassegno verifica se richiesto il contatto
052300130701        if ar9cas > 0;
052400130701           clear trul21ds;
052500130701           i21tla = 'L';
052600130701           i21cbo = blpcbo;
052700130701           i21tsp = blptsp;
052800130701           i21lnp = blplnp;
052900130701           i21nzm = blpnzm;
053000130701           i21lna = blplna;
053100130701           i21nzd = blpnzd;
053200130701           i21tic = ar9tic;
053300130701           i21imp = ar9cas;
053400130701           i21div = ar9vca;
053500130701           i21ute = sdsusr;
053600130701           i21pgm = nompgm;
053700130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
053800130829              if %lookup(blpcbo:cbf)>0;
053900130829                 i21ksc = blpksc;
054000130829              else;
054100130829                 i21ksc = blpccm;
054200130829                 if i21ksc=0;
054300130829                    i21ksc=(blplnp*10000) + 8888;
054400130829                 endif;
054500130829              endif;
054600130701           trul21r(trul21ds);
054700130701        endif;
054800130701
054900130701        // L'indicazione della consegna prevista viene fatta solo se la
055000130701        // spedizione non prevede contatti col destinatario:
055100130701
055200130701           // No Fermo deposito
055300130701        if blpffd = *blanks and (
055400130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
055500130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
055600130701        (blpdcr  > *zeros and blptcr = *blanks))
055700130701           // No contrassegno oppure importo che non richiede
055800130701           // l'abilitazione "manuale"
055900130701        and (ar9cas = *zeros or o21fca = *blanks);
056000130701           wcliper = 'S';
056100130701        endif;
056200130701
056300130701
056400130701        // Se non è indicata la data consegna richiesta
056500130701        // viene calcolata mediante pgm di calcolo delivery
056600130701        if wcliper = 'S' ;
056700160502         //  if blpdcr = *zeros;
056800130701              d98tbo = 'P';
056900130701              d98aas = blpaas;
057000130701              d98lnp = blplnp;
057100130701              d98nrs = blpnrs;
057200130701              d98nsp = blpnsp;
057300130701              if blptsp = 'D';
057400130701                 I98TSP_FOR = 'C';
057500130701              endif;
057600130701              tnsd99r (tnsd99ds);
057700130701              if d98err = '0';
057800151111                 wdcr = d98dee;
057900160502              else  ;
058000160502                 if blpdcr>0 ;
058100160502                  wdcr = blpdcr;
058200160502                 endif   ;
058300130701              endif;
058400160502          //        else;
058500160502          //           wdcr = blpdcr;
058600160502          //        endif;
058700160502
058800140327           wdcrRev = wdcr;
058900130701           if wdcr > *zeros;
059000130701              dataeur = %date(wdcr : *ISO);
059100130701              wdcr = %dec(dataeur);
059200130701           endif;
059300130701        endif;
059400140324
059500140324        wSpedConsOrari = '.';
059600141016        clear wSpedConsOraDa;
059700141016        clear wSpedConsOraA;
059800140324
059900140327        IOREDTA  = wdcrRev;
060000140324        IOREFIL  = blpLNA;
060100140324        IORECAP  = blpCAD;
060200140324        IORELOC  = blpLOD;
060300140324        IORENAR  = blpNZD;
060400140324        IORETSER = 'C';
060500140324        IOREDDC  = blpDDC;
060600140324        IORENDC  = blpNDC;
060700140324        IORETFP  = blpTFP;
060800140324        IORETFA  = blpTFA;
060900140324        IOREDTI  = blpDTI;
061000140324        IOREHTI  = blpHTI;
061100140324        IOREDCR  = blpDCR;
061200140324        IOREHCR  = blpHCR;
061300140324        IORETCR  = blpTCR;
061400140324        IOREDEI  = d98DEI;
061500140324        IOREOEI  = d98OEI;
061600140324        IORETSP  = blpTSP;
061700150730        §IORETRAZC = %editc(D98TTC:'X');
061800150730        §IORECONSC = %editc(D98TCC:'X');
061900150730        IOREFLO  = diore01;
062000140324
062100140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
062200140324        if not %error AND OOREERR = *blanks;
062300141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
062400141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
062500141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
062600141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
062700170202           wSpedConsOrari = '(' + wSpedConsOraDa + '-' +
062800150530                                  wSpedConsOraA  + ')';
062900140324        endif;
063000130626
063100130626        endsr;
063200130701
063300130701
063400130701
063500130701       //-------------------------------------------------------------*
063600130701       //Compongo il testo del SMS                                    *
063700130701       //-------------------------------------------------------------*
063800130701        Begsr valTextSMS;
063900130701
064000130703          wSmsText = *blanks;
064100131104          wTracking = *blanks;
064200140423          clear wConsegna;
064300131104
064400131104          if bologgi='S';
064500150530             wTracking = ' Verifichi da domani su www.brt.it';
064600131104          else;
064700150530             wTracking = ' Verifichi su www.brt.it';
064800131104          endif;
064900131104
065000130703
065100130701          if blpffd='S';
065200150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
065300140423
065400141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
065500150530                 //wConsegna = 'Potrà ritirare dal ' +
065600141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
065700141014                 wConsegna = '';
065800140423
065900150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
066000150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
066100130701          else;
066200130701            if wcliper = 'S'and wdcr > *zeros ;
066300130701               select;
066400130701               when blptcr=*blanks;
066500150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
066600140423
066700150530                       wConsegna = ' Consegna prevista il ' +
066800140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
066900140423                       %trim(wSpedConsOrari);
067000140423
067100150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
067200150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
067300150530
067400130701               when blptcr='D';
067500150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
067600140423
067700150530                       wConsegna = ' Consegna prevista dopo il ' +
067800140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
067900140423                       %trim(wSpedConsOrari);
068000140423
068100150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
068200150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068300130701               when blptcr='P';
068400150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
068500140423
068600150530                       wConsegna = ' Consegna prevista prima del ' +
068700140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
068800140423                       %trim(wSpedConsOrari);
068900140423
069000150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
069100150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069200130701               endsl;
069300130701
069400130701              else;
069500150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
069600150530                   wSmsText = %trim(wSmsText) + ' ' +
069700150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069800130701              endif;
069900130701          endif;
070000141128
070100141128
070200141128          // Se consegna con "PINcode" aggiungo reminder
070300141128          if wPINcode = 'S';
070400141128             wSmsText = %trim(wSmsText) + ' ' +
070500141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
070600141128                      ' l''inserimento del PIN di sicurezza.';
070700141128          endif;
070800141128
070900130701
071000130701        endsr;
071100170126
071200170126
071300170126
071400170126       //-------------------------------------------------------------*
071500170126       //Compongo il testo del SMS - NEW                              *
071600170202 ccc   //-------------------------------------------------------------*
071700170202 xxx    Begsr valTextSMSnew;
071800170126
071900170126          wSmsText = *blanks;
072000170202          wTracking = *blanks;
072100170126          clear wConsegna;
072200170126
072300170126
072400170126          if blpffd='S';
072500170202             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23));
072600170126
072700170126             // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
072800170126             //wConsegna = 'Potrà ritirare dal ' +
072900170126             //%trim(%editw(wdcr:'0  /  /    '));
073000170126             wConsegna = '';
073100170202             wTracking = 'Info più precise su www.brt.it';
073200170126
073300170202             wSmsText = %trim(wSmsText) + ' ' + %trim(wConsegna) + ' ' +
073400170202                        %trim(wTracking) + '  BRTcode ' + wBRTcodeLong;
073500170126          else;
073600170126
073700170126           // Verifico condizioni particolari della bolla
073800170126           select;
073900170126             // Appuntamento
074000170126             when wcliper <> 'S' and (blptc1 = 'A' or blptc2 = 'A');
074100170126               wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23)) +
074200170202                        ' affidata il ' +
074300170126               %subst(%editc(blpaas*10000+blpmgs:'X'):7:2) + '/' +
074400170202               %subst(%editc(blpaas*10000+blpmgs:'X'):5:2) +
074500170202                      ' prevede consegna su appuntamento che puoi' +
074600170202                      ' programmare subito su www.brt.it';
074700170126
074800170202               wSmsText = %trim(wSmsText) + '  BRTcode ' + wBRTcodeLong;
074900170126
075000170126             // Altre
075100170126             other;
075200170126
075300170126              // Abilitazioni alla consegna
075400170126              if wcliper = 'S'and wdcr > *zeros ;
075500170126                 select;
075600170126                 when blptcr=*blanks;
075700170202                   wSmsText='Sped. da '+%trim(%subst(wRSM_Sms:1:23))+' ';
075800170126
075900170202                   wConsegna = ' Prevista consegna ' +
076000170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
076100170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
076200170202                      %trim(wSpedConsOrari) +
076300170215                      ' Puoi riprogrammare la consegna su www.brt.it';
076400170126
076500170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
076600170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
076700170126
076800170126                 when blptcr='D';
076900170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
077000170126
077100170126                   wConsegna = ' Prevista consegna dopo il ' +
077200170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
077300170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
077400170202                      %trim(wSpedConsOrari) +
077500170215                      ' Puoi riprogrammare la consegna su www.brt.it';
077600170126
077700170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
077800170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
077900170126
078000170126                 when blptcr='P';
078100170127                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
078200170126
078300170126                   wConsegna = ' Prevista consegna prima del ' +
078400170202                      %subst(%editc(wdcr:'X'):1:2) + '/' +
078500170202                      %subst(%editc(wdcr:'X'):3:2) + ' ' +
078600170202                      %trim(wSpedConsOrari) +
078700170215                      ' Puoi riprogrammare la consegna su www.brt.it';
078800170126
078900170126                   wSmsText = %trim(wSmsText) + wConsegna + ' ' +
079000170126                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
079100170126                 endsl;
079200170126
079300170126              else;
079400170127                 wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+' ';
079500170202                 wTracking = 'Info più precise su www.brt.it';
079600170126                 wSmsText = %trim(wSmsText) + ' ' +
079700170126                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
079800170126              endif;
079900170126           endsl;
080000170126          endif;
080100170126
080200170126
080300170126          // Se consegna con "PINcode" aggiungo reminder
080400170126          if wPINcode = 'S';
080500170126             wSmsText = %trim(wSmsText) + ' ' +
080600170126                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
080700170126                      ' l''inserimento del PIN di sicurezza.';
080800170126          endif;
080900170126
081000170126        endsr;
081100130703
081200130703
081300130703
081400130703       //-------------------------------------------------------------*
081500130703       //Scrittura dato di output per creazione file massivo SMS      *
081600130703       //-------------------------------------------------------------*
081700130703        Begsr exeVGD;
081800131015
081900130704           o_MittSMS = 'BRT SpA';
082000130704           o_message = wSmsText;
082100130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
082200130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
082300130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
082400130704                       'SAAS';
082500130704
082600140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
082700140805           if bolok = 'S';
082800170126
082900141016              select;
083000141016                when chiudi = 'T';
083100170127                     o_cell = '393473441969';
083200141016              endsl;
083300160706
083400160706              o_message = %scanrpl(wSep:'':o_message);
083500170126
083600170126              clear tivgd000;
083700140805
083800161205              vgdDTA = %trim(o_MittSMS) + wSep +
083900140805                       %trim(o_IdSMS)   + wSep +
084000140805                       %trim(o_cell)    + wSep +
084100140805                       %trim(o_message);
084200161205
084300161205              // Scrittura TIVGD x file SMS
084400170126              vgdtip = 'SM';
084500170126              vgdksu = '0BART001';
084600170126              vgdtsc = 'WW';
084700170126              vgddat = currDate;
084800170126              vgdprg = wVASPRG;
084900170126              vgdpgm = 'FNLSSMR';
085000170127              vgdsto = '?';
085100170126
085200170126              write tivgd000;
085300140805
085400140805              // Incremento il conteggio degli SMS inseriti nel file corrente
085500140805              wCurRecFile = wCurRecFile + 1;
085600141201              wCurCivetta = wCurCivetta + 1;
085700140414
085800150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
085900150930              exsr sr_InsRA;
086000150930
086100140805           endif;
086200140414
086300130703        endsr;
086400170126
086500140414
086600141201
086700141201       //-------------------------------------------------------------*
086800141201       //Scrittura dato di output per SMS "civetta"                   *
086900141201       //-------------------------------------------------------------*
087000141201        Begsr exeVGDcivetta;
087100141201
087200141201
087300141201           o_MittSMS = 'BRT SpA';
087400141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
087500141201                                                %char(%time());
087600141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
087700141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
087800141201           o_cell = '393471487911';
087900170126
088000170126           o_message = %scanrpl(wSep:'':o_message);
088100170126
088200170126           clear tivgd000;
088300141201
088400161205           vgdDTA = %trim(o_MittSMS) + wSep +
088500141201                    %trim(o_IdSMS)   + wSep +
088600141201                    %trim(o_cell)    + wSep +
088700141201                    %trim(o_message);
088800170126
088900170126           // Scrittura TIVGD x file SMS
089000170126           vgdtip = 'SM';
089100170126           vgdksu = '0BART001';
089200170126           vgdtsc = 'WW';
089300170126           vgddat = currDate;
089400170126           vgdprg = wVASPRG;
089500170126           vgdpgm = 'FNLSSMR';
089600170127           vgdsto = '?';
089700170126
089800170126           write tivgd000;
089900141201
090000141201        endsr;
090100170126
090200141201
090300140414
090400140414       //-------------------------------------------------------------*
090500140414       //inserimento Richiesta Assistenza                             *
090600140414       //-------------------------------------------------------------*
090700140414        Begsr sr_InsRA;
090800141016
090900141016         if chiudi = *blanks;
091000140414
091100140805           // Inserimento R.A. via driver
091200140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
091300140414                        %trim(%editc(blpNRS:'X')) +
091400140414                        %trim(%editc(blpNSP:'X')) +
091500140414                        %trim(%editc(blpAAS:'X'));
091600140414
091700140414           clear fidna6ds;
091800140414
091900140423           iDA06MAD = ' 82';
092000140908
092100140908           iDA06RSC = 'SMS';
092200140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
092300140609                      '  -  ' + wConsegna;
092400140414           iDA06TOR = 'S';
092500140414           iDA06OGG = %trim(wSpedNum);
092600140414           iDA06UTE = 'QPGMR';
092700140610           iDA06FIL = blpLNP;
092800140414
092900140414           callP(e) FIDNA6R (fidna6ds);
093000160506
093100160506           // Inserimento spia per Predict DPD
093200160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
093300160506               exsr insPredict;
093400160506           endif;
093500141016
093600141016         endif;
093700140414
093800140414        endsr;
093900170126
094000160506
094100160506
094200160506       //-------------------------------------------------------------*
094300160506       //inserimento Predict DPD                                      *
094400160506       //-------------------------------------------------------------*
094500160506        Begsr insPredict;
094600160506
094700160506           clear tidp3000;
094800160506           dp3AAS   = blpAAS;
094900160506           dp3LNP   = blpLNP;
095000160506           dp3NRS   = blpNRS;
095100160506           dp3NSP   = blpNSP;
095200160506           dp3EVD   = '18';
095300160506           dp3CEV   = 'S25';
095400160506           dp3FLE   = blpLNA;
095500160518           dp3DEV   = 99991231;
095600160512           dp3HEV   = %dec(%time() : *HMS)/100;
095700160512           dp3DTSPE = blpAAS*10000+blpMGS;
095800160506           write tidp3000;
095900160506
096000160506        endsr;
096100170126
096200140414
096300140414
096400130829       //-------------------------------------------------------------*
096500130829       //caricamento codici bolla Franco                               *
096600130829       //-------------------------------------------------------------*
096700130829       Begsr SR_cartab;
096800130829
096900130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
097000130829       y=0;
097100130829       clear cbf;
097200130829       setll (1:'3A') tabel00f  ;
097300130829       reade (1:'3A') tabel00f  ;
097400130829
097500130829  1    dow    not %eof(tabel00f)  ;
097600130829          ds3a=tbluni           ;
097700130829          if %subst(§3atb1:1:1)='F';
097800130829             y=y+1;
097900130829             cbf(y)=tblkey;
098000130829          endif;
098100130829          reade (1:'3A') tabel00f  ;
098200130829  1    enddo     ;
098300141128
098400141128
098500141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
098600141128       clear sk7R_PINcode;
098700141128       clear ix7R;
098800141128       setll (1:'7R') tabel00f;
098900141128       reade (1:'7R') tabel00f;
099000141128
099100141128       dow    not %eof(tabel00f);
099200141128          ds7r=tbluni;
099300141128          if §7RPINCODE = 'S';
099400141128             ix7R = ix7R + 1;
099500141128             sk7R_PINcode(ix7r)=tblkey;
099600141128          endif;
099700141128          reade (1:'7R') tabel00f;
099800141128       enddo;
099900150929
100000150929
100100150929       // Carico la lista di filiali "estere" (secondo il network)
100200150929       clear skFilExt;
100300150929       clear ixFilExt;
100400150929       setll (*zeros) azorg01l;
100500150929       read azorg01l;
100600150929
100700150929       dow not %eof(azorg01l);
100800150929           if ORGFVA = *blanks;
100900150929              og143 = orgDE3;
101000150929              chain ('NTW':§OGNTW) tntbe01l;
101100150929              if %found(tntbe01l);
101200150929                 dntw = tbeuni;
101300150929                 if §NTWFIE = 'E';
101400150929                    ixFilExt = ixFilExt + 1;
101500150929                    skFilExt(ixFilExt) = orgFIL;
101600160506                    if §OGNTW = 'DPD';
101700160506                       skFilExtDPD(ixFilExt) = orgFIL;
101800160506                    endif;
101900150929                 endif;
102000150929              endif;
102100150929           endif;
102200150929           read azorg01l;
102300150929       enddo;
102400130703
102500130829       endsr;
102600170126
102700170126
102800170126
102900170126       //-------------------------------------------------------------*
103000170126       //caricamento filiali abilitate al deploy                      *
103100170126       //-------------------------------------------------------------*
103200170126       Begsr sr_VPOR;
103300170126
103400170127          clear wVPOdeploy;
103500170126
103600170126          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
103700170126          clear TRULVPODS;
103800170127          IVPOke1 = 'DECOFIIDC';
103900170126          TRULVPOR (trulvpods);
104000170126          skFil_New = skFilvpo;
104100170126
104200170126          select;
104300170126            // se al primo elemento c'è 999 => tutto a modo nuovo
104400170126            when skFil_New(1) = '999';
104500170127                 wVPOdeploy = 'NUOVO';
104600170126
104700170126            // se al primo elemento c'è *blanks => tutto a modo vecchio
104800170127            when skFil_New(1) = *zeros;
104900170127                 wVPOdeploy = 'VECCHIO';
105000170126
105100170126            other;
105200170127                 wVPOdeploy = 'MISTO';
105300170126          endsl;
105400170126
105500170126       endsr;
105600170126
105700170126
105800170126
105900170126       //-------------------------------------------------------------*
106000170126       //Effettuo controlli pre invio 2                               *
106100170126       //-------------------------------------------------------------*
106200170126       Begsr sr_ChkDeploy;
106300170126
106400170126          *in40 = *off;
106500170126
106600170126          select;
106700170127            when wVPOdeploy = 'VECCHIO';
106800170126              *in40  = *off;
106900170126
107000170127            when wVPOdeploy = 'NUOVO';
107100170126              *in40  = *on;
107200170126
107300170127            when wVPOdeploy = 'MISTO';
107400170126              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
107500170126                *in40 = *on;
107600170126              endif;
107700170126          endsl;
107800170126
107900170126       endsr;
108000170126
