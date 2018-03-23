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
001100161205     ftidp300f  uf a e             disk    commit
001200130701     ffiar901l  if   e           k disk
001300130829     ftabel00f  if   e           k disk
001400140805     ffnevb04l  if   e           k disk
001500150929     fazorg01l  if   e           k disk
001600150929     ftntbe01l  if   e           k disk
001700130701
001800130701
001900130701     d                sds
002000130701     d  nompgm                 1     10
002100130701     d  SDSusr               254    263
002200110617
002300130701
002400130626      * - DS procedure esterne
002500110617     d dar5emd       e ds
002600130626     d kpjba         e ds
002700130701     d trul33ds      e ds
002800120619     d tnsd99ds      e ds
002900161205     d trul28ds0     e ds                  qualified inz
003000140324     d trulORsds     e ds                  inz
003100140324     d trulOR2ds     e ds                  inz
003200130701     d trul21ds      e ds
003300140414     d fidna6ds      e ds                  inz
003400130625     d fnlvemds      e ds
003500161205     d iDSVGD        e ds                  extname('TIVGD00F')
003600130829     d ds3a          e ds
003700141128     d ds7r          e ds
003800130626     d fnblpds       e ds                  extname('FNBLP00F')
003900150730     d diore01       e ds
004000150929     d og143         e ds                  inz
004100150929     d dntw          e ds                  inz
004200130626
004300130626
004400110617      * - Campi di comodo
004500130829     d y               s              3  0 inz
004600130829     d cbf             S              2    DIM(100) inz
004700140805     d bolok           s              1    inz
004800140805     d bologgi         s              1    inz
004900130626     d kdos            s                   like(ar5dos) inz
005000130703     d wSmsText        s           1600    inz
005100131104     d wTracking       s            100    inz
005200130626     d wProcedi        s              1    inz
005300130701     d wcliper         s              1    inz
005400141128     d wPINcode        s              1    inz
005500130701     d Dataeur         s               d   datfmt(*eur)
005600131004     d currDate        s              8  0 inz
005700130701     d wdcr            s                   like(blpdcr)
005800140327     d wdcrRev         s                   like(blpdcr)
005900130704     d wSep            s              1    inz('|')
006000130704     d wPos            s              2  0 inz
006100130719     d wEsito          s              1    inz
006200141202     d wMaxRecFile     s              4  0 inz(200)
006300130920     d wCurRecFile     s              4  0 inz
006400141201     d wMaxCivetta     s              4  0 inz(5000)
006500141201     d wCurCivetta     s              4  0 inz
006600161205     d wBRTcodeLong    s             25                   inz
006700140623     d wSpedConsOrari  s             64A                  inz
006800140324     d wSpedConsOraDa  s              5A                  inz
006900140324     d wSpedConsOraA   s              5A                  inz
007000140610     d wSpedNum        s             16A                  inz
007100140423     d wConsegna       s           1000A   varying        inz
007200140725     d wRSM_Sms        s                   like(pOut_RNT) inz
007300141128     d pIn_Opz         s              1a   inz
007400141128     d pIn_APL         s              1a   inz
007500141128     d pIn_NK1         s             11a   inz
007600141128     d pIn_NK2         s              4a   inz
007700141128     d pIn_TNT         s              2a   inz
007800141128     d pOut_RNT        s             60a   inz
007900141128     d pOut_NTR        s              6s 0 inz
008000141128     d pOut_Esito      s              1a   inz
008100141128     d sk7R_PINcode    s              2    dim(100) inz
008200141128     d ix7R            s              3  0 inz
008300150929     D skFilExt        s                   like(orgFIL) dim(500) inz
008400160506     D skFilExtDPD     s                   like(orgFIL) dim(500) inz
008500150929     D ixFilExt        s              3  0 inz
008600161205     d iOPZ            s              1a   inz
008700161205     d oPRG            s             10a   inz
008800161205     d oOK             s              1a   inz
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
010000140725
010100130703
010200130704
010300130704      * Costanti
010400130704     d numeri          c                   const('1234567890')
010500130704
010600130703
010700130703     d* DS di output
010800130703     d  DS_Output      ds                  inz
010900130704     d   o_MittSMS                   30
011000130704     d   o_IdSMS                     36
011100130703     d   o_cell                      20
011200130703     d   o_message                 1600
011300130406
011400130625
011500130701     d fnlvemr         PR
011600130626     d                                     extpgm('FNLVEMR')
011700130701     d  kpjba                              likeds(kpjba)
011800130701     d  fnblpds                            likeds(fnblpds)
011900140414
012000140414
012100140414     d FIDNA6R         PR
012200140414     d                                     extpgm('FIDNA6R')
012300140414     d fidna6ds                            likeds(fidna6ds)
012400140414
012500140414
012600130701     d trul21r         PR
012700130701     d                                     EXTPGM('TRUL21R')
012800130701     d  trul21ds                           likeds(trul21ds)
012900141016
013000141016
013100141016     D TRUL28R0        PR
013200141016     D                                     extpgm('TRUL28R0')
013300141016     D trul28ds0                           likeds(trul28ds0)
013400130701
013500130701
013600130626     d trul33r         PR
013700130626     d                                     extpgm('TRUL33R')
013800130626     d  kpjba                              likeds(kpjba)
013900140324
014000140324
014100140324     D TRULORSR        PR
014200140324     D                                     extpgm('TRULORSR')
014300140324     D kpjba                               likeds(kpjba)
014400140324     D trulORsds                           likeds(trulORsds)
014500140324     D trulOR2ds                           likeds(trulOR2ds)
014600130719
014700161205
014800161205     d tis7vasr1       PR
014900161205     d                                     extpgm('TIS7VASR1')
015000161205     d  iOPZ                               like(iOPZ)
015100161205     d  iDSVGD                             likeds(iDSVGD)
015200161205     d  oPRG                               like(oPRG)
015300161205     d  oOK                                like(oOK)
015400140725
015500140725
015600140725     D UBREPNTCR       PR
015700140725     D                                     extpgm('UBREPNTCR')
015800140725     D pIn_Opz                             like(pIn_Opz)
015900140725     D pIn_APL                             like(pIn_APL)
016000140725     D pIn_NK1                             like(pIn_NK1)
016100140725     D pIn_NK2                             like(pIn_NK2)
016200140725     D pIn_TNT                             like(pIn_TNT)
016300140725     D pOut_RNT                            like(pOut_RNT)
016400140725     D pOut_NTR                            like(pOut_NTR)
016500140725     D pOut_Esito                          like(pOut_Esito)
016600130719
016700110617
016800110617      // ? PROTOTIPI ?
016900120619      /copy gaitrasrc/srcprotopr,tnsd99r
017000131017      /copy gaitrasrc/srcprotopr,ubchkcel
017100131017      /copy gaitrasrc/srcprotopi,ubchkcel
017200110617
017300130406
017400110627     C     *ENTRY        PLIST
017500110627     C                   PARM                    chiudi            1
017600130406
017700110617      /free
017800130626
017900140731       // Inizializzozioni
018000140731       wProcedi = 'S';
018100140805       wCurRecFile = *zeros;
018200131015
018300130829       // Carico le tabelle occorrenti
018400130829       exsr sr_cartab;
018500131004
018600131004       // Inizializzo variabili di procedura
018700131004       currDate = %dec(%date() : *ISO);
018800131004
018900161205       // Avvio Inizio transazione TIVGD x file SMS
019000161205       exsr strVGD;
019100131004
019200130701       // Se consentita elaborazione => procedo
019300140731       if wProcedi <> 'N';
019400130701          // Leggo fiar5 rk "EMD" x SMS da inviare
019500130719          *in34 = *off;
019600130626          exsr sr_Elabora;
019700130626       endif;
019800131004
019900161205       // Fine transazione TIVGD x file SMS
020000161205       exsr endVGD;
020100130406
020200130626       // Gestisco chiusura controllata
020300130626       if chiudi = 'S' or %shtdn;
020400130625          clear fnlvemds;
020500130625          iemTLA = 'C';
020600130625          kpjbu = fnlvemds;
020700130625          callP(e) FNLVEMR (kpjba : fnblpds);
020800140725          pIn_OPZ = 'C';
020900140725          callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
021000140725                              pIn_TNT : pOut_RNT : pOut_NTR : pOut_Esito);
021100161205          *inlr = *on;
021200140623       else;
021300161205          *inrt = *on;
021400110627       endif;
021500140617
021600140623       unlock(e) fiar504l;
021700130626
021800161205       return;
021900161205
022000130626
022100130626
022200130626       //-------------------------------------------------------------*
022300161205       // Avvio Inizio transazione TIVGD x file SMS
022400130626       //-------------------------------------------------------------*
022500161205        Begsr strVGD;
022600161205
022700161205          iOPZ   = '1';
022800161205          clear iDSVGD;
022900161205          clear oPRG;
023000161205          clear oOK;
023100161205          callP(e) tis7vasr1 (iOPZ
023200161205                             :iDSVGD
023300161205                             :oPRG
023400161205                             :oOK);
023500130626
023600161205          if %error OR oOK = *off;
023700161205             wProcedi = 'N';
023800161205          endif;
023900130626
024000130626        endsr;
024100161205
024200161205
024300161205
024400161205       //-------------------------------------------------------------*
024500161205       // Fine transazione TIVGD x file SMS
024600161205       //-------------------------------------------------------------*
024700161205        Begsr endVGD;
024800161205
024900161205          iOPZ   = '3';
025000161205          callP(e) tis7vasr1 (iOPZ
025100161205                             :iDSVGD
025200161205                             :oPRG
025300161205                             :oOK);
025400161205
025500161205          if %error OR oOK = *off;
025600161205             dump(A);
025700161205             rolbk;
025800161205             *inlr = *on;
025900161205             return;
026000161205          endif;
026100161205
026200161205        endsr;
026300130626
026400130626
026500130626
026600130626       //-------------------------------------------------------------*
026700130626       //Reperimento progressivo                                      *
026800130626       //-------------------------------------------------------------*
026900130626        Begsr sr_rtvPrg;
027000130626
027100130626          clear trul33ds;
027200130626          i33cnu = 24;
027300130626          i33num = 1;
027400130626          kpjbu = trul33ds;
027500130626          trul33r (kpjba);
027600130626          trul33ds = kpjbu;
027700130626
027800130626        endsr;
027900130626
028000130406
028100130406
028200110617       //-------------------------------------------------------------*
028300110617       //Lettura fiar5 record "EMD" da inviare                         *
028400110617       //-------------------------------------------------------------*
028500110617       Begsr SR_Elabora;
028600130626
028700130626        kdos = *blanks;
028800130626        setll (kdos) fiar504l;
028900130626        reade (kdos) fiar504l;
029000110617
029100130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
029200130406
029300130626           // controllo se è stata richiesta la chiusura del sottosistema
029400110627           if %shtdn;
029500130626               chiudi = 'S';
029600110627               leavesr;
029700110627           endif;
029800141201
029900141201           // Gestione "civette"
030000141201           if wCurCivetta >= wMaxCivetta;
030100141201              exsr exeVGDcivetta;
030200141201              wCurCivetta = *zeros;
030300141201           endif;
030400141201
030500130406
030600130626           // controllo lo stato della bolla
030700110617           exsr sr_chkstblp;
030800140805           if bolok <> *blanks;
030900130719
031000130703              // inizializzo DS di work
031100130703              clear DS_Output;
031200130701              clear fiar9000;
031300130703
031400130703              // provo a reperire il contrassegno
031500130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
031600130703
031700130703              // compongo l'output per l'invio SMS
031800130703              dar5emd = ar5uni;
031900130719
032000130719              // normalizzo e verifico il numero di cellulare
032100130719              *in33 = *off;
032200130719              exsr sr_normCELL;
032300130719              if not *in33;
032400130719                 exsr sr_inviaSMS;
032500130719              endif;
032600130703
032700130703              // aggiorno il record corrente come SMS inviato
032800150930              exsr sr_UpdFIAR5;
032900150930
033000110617           endif;
033100130406
033200130701           reade (kdos) fiar504l;
033300110617        enddo;
033400130406
033500130626       endsr;
033600150930
033700150930
033800150930
033900150930       //-------------------------------------------------------------*
034000150930       // Gestione tentativi esecuzioni comandi                       *
034100150930       //-------------------------------------------------------------*
034200150930        Begsr sr_UpdFIAR5;
034300150930
034400150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
034500150930          §ar5DPC  = %editc(wdcrRev:'X');
034600150930          §ar5OPCD = %editc(OOR2STIS:'X');
034700150930          §ar5OPCA = %editc(OOR2STFS:'X');
034800150930          // Verifico che i valori sopra attribuiti siano diversi da zero
034900150930          if §AR5DPC  = *all'0';
035000150930             §AR5DPC  = *blanks;
035100160330             §AR5OPCD = *zeros;
035200160330             §AR5OPCA = *zeros;
035300150930          endif;
035400150930          if §AR5OPCD = *all'0';
035500150930             §AR5OPCD = *blanks;
035600150930          endif;
035700150930          if §AR5OPCA = *all'0';
035800150930             §AR5OPCA = *blanks;
035900150930          endif;
036000150930          ar5uni=dar5emd;
036100150930          if chiudi = *blanks;
036200150930             update fiar5000;
036300150930          endif;
036400150930
036500150930        endsr;
036600130406
036700130626
036800130406
036900110617       //-------------------------------------------------------------*
037000110617       //Verifica stato della bolla                                   *
037100110617       //-------------------------------------------------------------*
037200130625        Begsr sr_chkstblp;
037300130406
037400130626           bolok=' ';
037500131104           bologgi=' ';
037600130626
037700130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
037800130626           // delle comunicazioni ai destinatari
037900130626           clear fnlvemds;
038000130626           clear fnblpds;
038100140725           clear wRSM_Sms;
038200130626           iemAAS = ar5aas;
038300130626           iemLNP = ar5lnp;
038400130626           iemNRS = ar5nrs;
038500130626           iemNSP = ar5nsp;
038600130626           kpjbu = fnlvemds;
038700130626           callP(e) FNLVEMR (kpjba : fnblpds);
038800130626           fnlvemds = kpjbu;
038900140725
039000130626
039100130626           // Verifico se esito favorevole al invio
039200130626           if oemESITO = '1';
039300150929
039400150929           // Verifico che NON trattasi di spedizione "export"
039500150929           if  %lookup(blpLNA:skFilExt) > 0;
039600150929               bolok = 'E';
039700150929           endif;
039800150929
039900150929           // Se tutto OK => procedo
040000150929           if bolok <> 'E';
040100140725
040200140805              // Verifico se per cliente corrente presente nota specifica
040300140725              // per Rag.Soc. invio alerts
040400140725              if blpCCM > *zeros;
040500140725                 pIn_NK1 = '0151' + %editc(blpCCM:'X');
040600140725              else;
040700140725                 pIn_NK1 = '0151' + %editc(blpKSC:'X');
040800140725              endif;
040900140725              // --- SMS ---
041000140725              pIn_OPZ = *blanks;
041100140725              pIn_APL = 'C';
041200140725              pIn_NK2 = *blanks;
041300140725              pIn_TNT = 'AS';
041400140725              callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
041500140725                                  pIn_TNT : wRSM_Sms : pOut_NTR : pOut_Esito);
041600140725              if pOut_Esito = '1';
041700140725              else;
041800140725                 wRSM_Sms = %trim(blpRSM);
041900140725              endif;
042000141016
042100141016
042200141016              // Calcolo il BRTcode (tramite apposito driver)
042300141016              clear BRTcode;
042400141016              BRTcode.aas4    = blpAAS;
042500141016              BRTcode.lnp     = blpLNP;
042600141016              BRTcode.nrs     = blpNRS;
042700141016              BRTcode.nsp     = blpNSP;
042800141016
042900141016              clear trul28ds0;
043000141016              trul28ds0.i280COD = BRTcode.BRTcode;
043100141016              callP(e) TRUL28R0 (trul28ds0);
043200141016
043300141016              // Se BRTcode errato => esito KO
043400141016              clear wBRTcodeLong;
043500141016              if %error OR trul28ds0.o280err = '1';
043600141016              else;
043700150530                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
043800150530                                %subst(trul28ds0.o280cod:6:5) +
043900150530                                %subst(trul28ds0.o280cod:11:5)+
044000141016                                %subst(trul28ds0.o280cod:16:4);
044100141016              endif;
044200131104
044300141016
044400131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
044500131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
044600131104                 bolok='S';
044700131104              endif;
044800131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
044900131104                 %dec(%time():*ISO)  >= 150000;
045000131104                 bolok='S';
045100131104                 bologgi='S';
045200131104              endif;
045300140805
045400140805              if bolok='S';
045500140805                 // Verifico per sicurezza se già presente evento 'MIC'
045600140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
045700140805                 if %found(fnevb04l);
045800140805                    bolok='X';
045900140805                 endif;
046000140805              endif;
046100150929
046200150929           endif;
046300131104
046400130626           endif;
046500130406
046600110617        endsr;
046700130719
046800130719
046900130719
047000130719       //-------------------------------------------------------------*
047100130719       //Normalizza numero ci cellulare                               *
047200130719       //-------------------------------------------------------------*
047300130719        Begsr sr_normCELL;
047400130719
047500131017           // Chiamo il driver preposto
047600131017           pInCell = %trim(§ar5TEL);
047700131017           pOutErr = *blanks;
047800131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
047900131017              §ar5SME = 'E';
048000131017              *in33 = *on;
048100131017           else;
048200131017              o_cell = pOutCell;
048300131017           endif;
048400130719
048500130719        endsr;
048600130406
048700130626
048800130406
048900110617       //-------------------------------------------------------------*
049000130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
049100110617       //-------------------------------------------------------------*
049200130626        Begsr sr_inviaSMS;
049300130719
049400130719          // Stacco un progressivo per il file SMS corrente
049500130719          if not *in34;
049600130719             exsr sr_rtvPrg;
049700130719             *in34 = *on;
049800130719          endif;
049900130406
050000130626          // determino data consegnabilità
050100130626          exsr sr_calcons;
050200130626
050300130626          // Compongo il testo del SMS
050400130701          exsr valTextSMS;
050500130626
050600130626          // Aggiungo l'SMS corrente
050700130626          exsr exeVGD;
050800130406
050900110617        endsr;
051000130626
051100130626
051200130626
051300130626       //-------------------------------------------------------------*
051400130626       //Verifica eventuali personalizzazioni per il cliente          *
051500130626       //-------------------------------------------------------------*
051600130701        Begsr sr_calcons;
051700130626
051800130701        clear wcliper;
051900130701        clear wdcr;
052000140327        clear wdcrRev;
052100141128        clear wPINcode;
052200140331        clear tnsd99ds;
052300140324        clear trulORsds;
052400140324        clear trulOR2ds;
052500150730        clear diore01;
052600141128
052700141128        // Verifica se spedizione con particolarità consegna "PINcode"
052800141128        if blpGMA <> *blanks;
052900141128           if %lookup(blpGMA:sk7R_PINcode)>0;
053000141128              wPINcode = 'S';
053100141128            endif;
053200141128        endif;
053300130701
053400130701        // Se presente il contrassegno verifica se richiesto il contatto
053500130701        if ar9cas > 0;
053600130701           clear trul21ds;
053700130701           i21tla = 'L';
053800130701           i21cbo = blpcbo;
053900130701           i21tsp = blptsp;
054000130701           i21lnp = blplnp;
054100130701           i21nzm = blpnzm;
054200130701           i21lna = blplna;
054300130701           i21nzd = blpnzd;
054400130701           i21tic = ar9tic;
054500130701           i21imp = ar9cas;
054600130701           i21div = ar9vca;
054700130701           i21ute = sdsusr;
054800130701           i21pgm = nompgm;
054900130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
055000130829              if %lookup(blpcbo:cbf)>0;
055100130829                 i21ksc = blpksc;
055200130829              else;
055300130829                 i21ksc = blpccm;
055400130829                 if i21ksc=0;
055500130829                    i21ksc=(blplnp*10000) + 8888;
055600130829                 endif;
055700130829              endif;
055800130701           trul21r(trul21ds);
055900130701        endif;
056000130701
056100130701        // L'indicazione della consegna prevista viene fatta solo se la
056200130701        // spedizione non prevede contatti col destinatario:
056300130701
056400130701           // No Fermo deposito
056500130701        if blpffd = *blanks and (
056600130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
056700130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
056800130701        (blpdcr  > *zeros and blptcr = *blanks))
056900130701           // No contrassegno oppure importo che non richiede
057000130701           // l'abilitazione "manuale"
057100130701        and (ar9cas = *zeros or o21fca = *blanks);
057200130701           wcliper = 'S';
057300130701        endif;
057400130701
057500130701
057600130701        // Se non è indicata la data consegna richiesta
057700130701        // viene calcolata mediante pgm di calcolo delivery
057800130701        if wcliper = 'S' ;
057900160502         //  if blpdcr = *zeros;
058000130701              d98tbo = 'P';
058100130701              d98aas = blpaas;
058200130701              d98lnp = blplnp;
058300130701              d98nrs = blpnrs;
058400130701              d98nsp = blpnsp;
058500130701              if blptsp = 'D';
058600130701                 I98TSP_FOR = 'C';
058700130701              endif;
058800130701              tnsd99r (tnsd99ds);
058900130701              if d98err = '0';
059000151111                 wdcr = d98dee;
059100160502              else  ;
059200160502                 if blpdcr>0 ;
059300160502                  wdcr = blpdcr;
059400160502                 endif   ;
059500130701              endif;
059600160502          //        else;
059700160502          //           wdcr = blpdcr;
059800160502          //        endif;
059900160502
060000140327           wdcrRev = wdcr;
060100130701           if wdcr > *zeros;
060200130701              dataeur = %date(wdcr : *ISO);
060300130701              wdcr = %dec(dataeur);
060400130701           endif;
060500130701        endif;
060600140324
060700140324        wSpedConsOrari = '.';
060800141016        clear wSpedConsOraDa;
060900141016        clear wSpedConsOraA;
061000140324
061100140327        IOREDTA  = wdcrRev;
061200140324        IOREFIL  = blpLNA;
061300140324        IORECAP  = blpCAD;
061400140324        IORELOC  = blpLOD;
061500140324        IORENAR  = blpNZD;
061600140324        IORETSER = 'C';
061700140324        IOREDDC  = blpDDC;
061800140324        IORENDC  = blpNDC;
061900140324        IORETFP  = blpTFP;
062000140324        IORETFA  = blpTFA;
062100140324        IOREDTI  = blpDTI;
062200140324        IOREHTI  = blpHTI;
062300140324        IOREDCR  = blpDCR;
062400140324        IOREHCR  = blpHCR;
062500140324        IORETCR  = blpTCR;
062600140324        IOREDEI  = d98DEI;
062700140324        IOREOEI  = d98OEI;
062800140324        IORETSP  = blpTSP;
062900150730        §IORETRAZC = %editc(D98TTC:'X');
063000150730        §IORECONSC = %editc(D98TCC:'X');
063100150730        IOREFLO  = diore01;
063200140324
063300140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
063400140324        if not %error AND OOREERR = *blanks;
063500141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
063600141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
063700141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
063800141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
063900150530           wSpedConsOrari = '(' + wSpedConsOraDa + '  ' +
064000150530                                  wSpedConsOraA  + ')';
064100140324        endif;
064200130626
064300130626        endsr;
064400130701
064500130701
064600130701
064700130701       //-------------------------------------------------------------*
064800130701       //Compongo il testo del SMS                                    *
064900130701       //-------------------------------------------------------------*
065000130701        Begsr valTextSMS;
065100130701
065200130703          wSmsText = *blanks;
065300131104          wTracking = *blanks;
065400140423          clear wConsegna;
065500131104
065600131104          if bologgi='S';
065700150530             wTracking = ' Verifichi da domani su www.brt.it';
065800131104          else;
065900150530             wTracking = ' Verifichi su www.brt.it';
066000131104          endif;
066100131104
066200130703
066300130701          if blpffd='S';
066400150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
066500140423
066600141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
066700150530                 //wConsegna = 'Potrà ritirare dal ' +
066800141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
066900141014                 wConsegna = '';
067000140423
067100150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
067200150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
067300130701          else;
067400130701            if wcliper = 'S'and wdcr > *zeros ;
067500130701               select;
067600130701               when blptcr=*blanks;
067700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
067800140423
067900150530                       wConsegna = ' Consegna prevista il ' +
068000140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
068100140423                       %trim(wSpedConsOrari);
068200140423
068300150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
068400150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
068500150530
068600130701               when blptcr='D';
068700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
068800140423
068900150530                       wConsegna = ' Consegna prevista dopo il ' +
069000140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
069100140423                       %trim(wSpedConsOrari);
069200140423
069300150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
069400150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
069500130701               when blptcr='P';
069600150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
069700140423
069800150530                       wConsegna = ' Consegna prevista prima del ' +
069900140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
070000140423                       %trim(wSpedConsOrari);
070100140423
070200150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
070300150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
070400130701               endsl;
070500130701
070600130701              else;
070700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
070800150530                   wSmsText = %trim(wSmsText) + ' ' +
070900150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
071000130701              endif;
071100130701          endif;
071200141128
071300141128
071400141128          // Se consegna con "PINcode" aggiungo reminder
071500141128          if wPINcode = 'S';
071600141128             wSmsText = %trim(wSmsText) + ' ' +
071700141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
071800141128                      ' l''inserimento del PIN di sicurezza.';
071900141128          endif;
072000141128
072100130701
072200130701        endsr;
072300130703
072400130703
072500130703
072600130703       //-------------------------------------------------------------*
072700130703       //Scrittura dato di output per creazione file massivo SMS      *
072800130703       //-------------------------------------------------------------*
072900130703        Begsr exeVGD;
073000131015
073100130704           o_MittSMS = 'BRT SpA';
073200130704           o_message = wSmsText;
073300130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
073400130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
073500130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
073600130704                       'SAAS';
073700130704
073800140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
073900140805           if bolok = 'S';
074000141016
074100141016              select;
074200141016                when chiudi = 'T';
074300141016                     o_cell = '3473441969';
074400141016              endsl;
074500160706
074600160706              o_message = %scanrpl(wSep:'':o_message);
074700161205
074800161205              clear iDSVGD;
074900140805
075000161205              vgdDTA = %trim(o_MittSMS) + wSep +
075100140805                       %trim(o_IdSMS)   + wSep +
075200140805                       %trim(o_cell)    + wSep +
075300140805                       %trim(o_message);
075400161205
075500161205              // Scrittura TIVGD x file SMS
075600161205              exsr wriVGD;
075700140805
075800140805              // Incremento il conteggio degli SMS inseriti nel file corrente
075900140805              wCurRecFile = wCurRecFile + 1;
076000141201              wCurCivetta = wCurCivetta + 1;
076100140414
076200150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
076300150930              exsr sr_InsRA;
076400150930
076500140805           endif;
076600140414
076700130703        endsr;
076800140414
076900141201
077000141201       //-------------------------------------------------------------*
077100141201       //Scrittura dato di output per SMS "civetta"                   *
077200141201       //-------------------------------------------------------------*
077300141201        Begsr exeVGDcivetta;
077400141201
077500141201
077600141201           o_MittSMS = 'BRT SpA';
077700141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
077800141201                                                %char(%time());
077900141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
078000141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
078100141201           o_cell = '393471487911';
078200141201
078300161205           clear iDSVGD;
078400141201
078500161205           vgdDTA = %trim(o_MittSMS) + wSep +
078600141201                    %trim(o_IdSMS)   + wSep +
078700141201                    %trim(o_cell)    + wSep +
078800141201                    %trim(o_message);
078900161205
079000161205           // Scrittura TIVGD x file SMS
079100161205           exsr wriVGD;
079200141201
079300141201        endsr;
079400161205
079500161205
079600161205
079700161205       //-------------------------------------------------------------*
079800161205       // Scrittura TIVGD x file SMS
079900161205       //-------------------------------------------------------------*
080000161205        Begsr wriVGD;
080100161205
080200161205          iOPZ   = '2';
080300161205          vgdTIP = 'SM';
080400161205          vgdKSU = '0BART001';
080500161205          vgdTSC = 'WW';
080600161205          vgdDAT = currDate;
080700161205          vgdPRG = oPRG;
080800161205          vgdPGM = nompgm;
080900161205
081000161205          callP(e) tis7vasr1 (iOPZ
081100161205                             :iDSVGD
081200161205                             :oPRG
081300161205                             :oOK);
081400161205
081500161205          if %error OR oOK = *off;
081600161205             dump(A);
081700161205             rolbk;
081800161205             *inlr = *on;
081900161205             return;
082000161205          endif;
082100161205
082200161205        endsr;
082300141201
082400140414
082500140414       //-------------------------------------------------------------*
082600140414       //inserimento Richiesta Assistenza                             *
082700140414       //-------------------------------------------------------------*
082800140414        Begsr sr_InsRA;
082900141016
083000141016         if chiudi = *blanks;
083100140414
083200140805           // Inserimento R.A. via driver
083300140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
083400140414                        %trim(%editc(blpNRS:'X')) +
083500140414                        %trim(%editc(blpNSP:'X')) +
083600140414                        %trim(%editc(blpAAS:'X'));
083700140414
083800140414           clear fidna6ds;
083900140414
084000140423           iDA06MAD = ' 82';
084100140908
084200140908           iDA06RSC = 'SMS';
084300140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
084400140609                      '  -  ' + wConsegna;
084500140414           iDA06TOR = 'S';
084600140414           iDA06OGG = %trim(wSpedNum);
084700140414           iDA06UTE = 'QPGMR';
084800140610           iDA06FIL = blpLNP;
084900140414
085000140414           callP(e) FIDNA6R (fidna6ds);
085100160506
085200160506           // Inserimento spia per Predict DPD
085300160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
085400160506               exsr insPredict;
085500160506           endif;
085600141016
085700141016         endif;
085800140414
085900140414        endsr;
086000160506
086100160506
086200160506       //-------------------------------------------------------------*
086300160506       //inserimento Predict DPD                                      *
086400160506       //-------------------------------------------------------------*
086500160506        Begsr insPredict;
086600160506
086700160506           clear tidp3000;
086800160506           dp3AAS   = blpAAS;
086900160506           dp3LNP   = blpLNP;
087000160506           dp3NRS   = blpNRS;
087100160506           dp3NSP   = blpNSP;
087200160506           dp3EVD   = '18';
087300160506           dp3CEV   = 'S25';
087400160506           dp3FLE   = blpLNA;
087500160518           dp3DEV   = 99991231;
087600160512           dp3HEV   = %dec(%time() : *HMS)/100;
087700160512           dp3DTSPE = blpAAS*10000+blpMGS;
087800160506           write tidp3000;
087900160506
088000160506        endsr;
088100140414
088200140414
088300130829       //-------------------------------------------------------------*
088400130829       //caricamento codici bolla Franco                               *
088500130829       //-------------------------------------------------------------*
088600130829       Begsr SR_cartab;
088700130829
088800130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
088900130829       y=0;
089000130829       clear cbf;
089100130829       setll (1:'3A') tabel00f  ;
089200130829       reade (1:'3A') tabel00f  ;
089300130829
089400130829  1    dow    not %eof(tabel00f)  ;
089500130829          ds3a=tbluni           ;
089600130829          if %subst(§3atb1:1:1)='F';
089700130829             y=y+1;
089800130829             cbf(y)=tblkey;
089900130829          endif;
090000130829          reade (1:'3A') tabel00f  ;
090100130829  1    enddo     ;
090200141128
090300141128
090400141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
090500141128       clear sk7R_PINcode;
090600141128       clear ix7R;
090700141128       setll (1:'7R') tabel00f;
090800141128       reade (1:'7R') tabel00f;
090900141128
091000141128       dow    not %eof(tabel00f);
091100141128          ds7r=tbluni;
091200141128          if §7RPINCODE = 'S';
091300141128             ix7R = ix7R + 1;
091400141128             sk7R_PINcode(ix7r)=tblkey;
091500141128          endif;
091600141128          reade (1:'7R') tabel00f;
091700141128       enddo;
091800150929
091900150929
092000150929       // Carico la lista di filiali "estere" (secondo il network)
092100150929       clear skFilExt;
092200150929       clear ixFilExt;
092300150929       setll (*zeros) azorg01l;
092400150929       read azorg01l;
092500150929
092600150929       dow not %eof(azorg01l);
092700150929           if ORGFVA = *blanks;
092800150929              og143 = orgDE3;
092900150929              chain ('NTW':§OGNTW) tntbe01l;
093000150929              if %found(tntbe01l);
093100150929                 dntw = tbeuni;
093200150929                 if §NTWFIE = 'E';
093300150929                    ixFilExt = ixFilExt + 1;
093400150929                    skFilExt(ixFilExt) = orgFIL;
093500160506                    if §OGNTW = 'DPD';
093600160506                       skFilExtDPD(ixFilExt) = orgFIL;
093700160506                    endif;
093800150929                 endif;
093900150929              endif;
094000150929           endif;
094100150929           read azorg01l;
094200150929       enddo;
094300130703
094400130829       endsr;
