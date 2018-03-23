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
001100130626     ftivgd00f  uf a e             disk
001200160506     ftidp300f  uf a e             disk
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
002900130626     d trul47ds      e ds
003000120619     d tnsd99ds      e ds
003100141016     D trul28ds0     e ds                  qualified inz
003200140324     d trulORsds     e ds                  inz
003300140324     d trulOR2ds     e ds                  inz
003400130701     d trul21ds      e ds
003500140414     d fidna6ds      e ds                  inz
003600130625     d fnlvemds      e ds
003700130719     d tis781ds      e ds
003800130719     d tis781dflo    e ds
003900130719     d dvgdflo       e ds
004000130829     d ds3a          e ds
004100141128     d ds7r          e ds
004200130626     d fnblpds       e ds                  extname('FNBLP00F')
004300150730     d diore01       e ds
004400150929     d og143         e ds                  inz
004500150929     d dntw          e ds                  inz
004600130626
004700130626
004800110617      * - Campi di comodo
004900130829     d y               s              3  0 inz
005000130829     d cbf             S              2    DIM(100) inz
005100140805     d bolok           s              1    inz
005200140805     d bologgi         s              1    inz
005300130626     d kdos            s                   like(ar5dos) inz
005400130703     d wSmsText        s           1600    inz
005500131104     d wTracking       s            100    inz
005600130626     d wProcedi        s              1    inz
005700130701     d wcliper         s              1    inz
005800141128     d wPINcode        s              1    inz
005900130701     d Dataeur         s               d   datfmt(*eur)
006000131004     d currDate        s              8  0 inz
006100130701     d wdcr            s                   like(blpdcr)
006200140327     d wdcrRev         s                   like(blpdcr)
006300130704     d wSep            s              1    inz('|')
006400130704     d wPos            s              2  0 inz
006500130719     d wEsito          s              1    inz
006600141202     d wMaxRecFile     s              4  0 inz(200)
006700130920     d wCurRecFile     s              4  0 inz
006800141201     d wMaxCivetta     s              4  0 inz(5000)
006900141201     d wCurCivetta     s              4  0 inz
007000131015     d wForzaDWL       s               n
007100141016     D wBRTcodeLong    s             25                   inz
007200140623     d wSpedConsOrari  s             64A                  inz
007300140324     d wSpedConsOraDa  s              5A                  inz
007400140324     d wSpedConsOraA   s              5A                  inz
007500140610     d wSpedNum        s             16A                  inz
007600140423     d wConsegna       s           1000A   varying        inz
007700140725     d wRSM_Sms        s                   like(pOut_RNT) inz
007800141128     d pIn_Opz         s              1a   inz
007900141128     d pIn_APL         s              1a   inz
008000141128     d pIn_NK1         s             11a   inz
008100141128     d pIn_NK2         s              4a   inz
008200141128     d pIn_TNT         s              2a   inz
008300141128     d pOut_RNT        s             60a   inz
008400141128     d pOut_NTR        s              6s 0 inz
008500141128     d pOut_Esito      s              1a   inz
008600141128     d sk7R_PINcode    s              2    dim(100) inz
008700141128     d ix7R            s              3  0 inz
008800150929     D skFilExt        s                   like(orgFIL) dim(500) inz
008900160506     D skFilExtDPD     s                   like(orgFIL) dim(500) inz
009000150929     D ixFilExt        s              3  0 inz
009100141016
009200141016
009300141016     D BRTcode         ds                  qualified
009400141016     D   aas4                         4  0
009500141016     D   lnp                          3  0
009600141016     D   nrs                          2  0
009700141016     D   nsp                          7  0
009800141016     D   chkdgt1                      1  0
009900141016     D   code                         3  0
010000141016     D   chkdgt2                      1  0
010100141016     D   BRTcode               3     21
010200140725
010300130703
010400130704
010500130704      * Costanti
010600130704     d numeri          c                   const('1234567890')
010700130704
010800130703
010900130703     d* DS di output
011000130703     d  DS_Output      ds                  inz
011100130704     d   o_MittSMS                   30
011200130704     d   o_IdSMS                     36
011300130703     d   o_cell                      20
011400130703     d   o_message                 1600
011500130406
011600130625
011700130701     d fnlvemr         PR
011800130626     d                                     extpgm('FNLVEMR')
011900130701     d  kpjba                              likeds(kpjba)
012000130701     d  fnblpds                            likeds(fnblpds)
012100140414
012200140414
012300140414     d FIDNA6R         PR
012400140414     d                                     extpgm('FIDNA6R')
012500140414     d fidna6ds                            likeds(fidna6ds)
012600140414
012700140414
012800130701     d trul21r         PR
012900130701     d                                     EXTPGM('TRUL21R')
013000130701     d  trul21ds                           likeds(trul21ds)
013100130626
013200130626
013300130701     d trul47r         PR
013400130701     d                                     EXTPGM('TRUL47R')
013500130701     d  trul47ds                           likeds(trul47ds)
013600141016
013700141016
013800141016     D TRUL28R0        PR
013900141016     D                                     extpgm('TRUL28R0')
014000141016     D trul28ds0                           likeds(trul28ds0)
014100130701
014200130701
014300130626     d trul33r         PR
014400130626     d                                     extpgm('TRUL33R')
014500130626     d  kpjba                              likeds(kpjba)
014600140324
014700140324
014800140324     D TRULORSR        PR
014900140324     D                                     extpgm('TRULORSR')
015000140324     D kpjba                               likeds(kpjba)
015100140324     D trulORsds                           likeds(trulORsds)
015200140324     D trulOR2ds                           likeds(trulOR2ds)
015300130719
015400130719
015500130719     d tis781c1        PR
015600130719     d                                     extpgm('TIS781C1')
015700130719     d  esito                         1
015800130719     d  tis781ds                           likeds(tis781ds)
015900130719     d  vgdPRG                             like(vgdPRG)
016000140725
016100140725
016200140725     D UBREPNTCR       PR
016300140725     D                                     extpgm('UBREPNTCR')
016400140725     D pIn_Opz                             like(pIn_Opz)
016500140725     D pIn_APL                             like(pIn_APL)
016600140725     D pIn_NK1                             like(pIn_NK1)
016700140725     D pIn_NK2                             like(pIn_NK2)
016800140725     D pIn_TNT                             like(pIn_TNT)
016900140725     D pOut_RNT                            like(pOut_RNT)
017000140725     D pOut_NTR                            like(pOut_NTR)
017100140725     D pOut_Esito                          like(pOut_Esito)
017200130719
017300110617
017400110617      // ? PROTOTIPI ?
017500120619      /copy gaitrasrc/srcprotopr,tnsd99r
017600131017      /copy gaitrasrc/srcprotopr,ubchkcel
017700131017      /copy gaitrasrc/srcprotopi,ubchkcel
017800110617
017900130406
018000110627     C     *ENTRY        PLIST
018100110627     C                   PARM                    chiudi            1
018200130406
018300110617      /free
018400130626
018500140731       // Inizializzozioni
018600131015       wForzaDWL = *off;
018700140731       wProcedi = 'S';
018800140805       wCurRecFile = *zeros;
018900131015
019000130829       // Carico le tabelle occorrenti
019100130829       exsr sr_cartab;
019200131004
019300131004       // Inizializzo variabili di procedura
019400131004       currDate = %dec(%date() : *ISO);
019500131004
019600130626       // Avvvio blocco elaborazione TIVGD x file SMS
019700140731       //exsr strTRUL47;
019800130626
019900131004
020000130701       // Se consentita elaborazione => procedo
020100140731       if wProcedi <> 'N';
020200130701          // Leggo fiar5 rk "EMD" x SMS da inviare
020300130719          *in34 = *off;
020400130626          exsr sr_Elabora;
020500130626       endif;
020600130626
020700131004
020800130626       // Termino blocco elaborazione TIVGD x file SMS
020900140731       //exsr endTRUL47;
021000130719
021100131015
021200130719       // Forzo l'elaborazione del download
021300140731       //if wForzaDWL;
021400140731       //   exsr forzaDWL;
021500140731       //endif;
021600131015
021700130406
021800130626       // Gestisco chiusura controllata
021900130626       if chiudi = 'S' or %shtdn;
022000140623          eval *inlr = *on;
022100130625          clear fnlvemds;
022200130625          iemTLA = 'C';
022300130625          kpjbu = fnlvemds;
022400130625          callP(e) FNLVEMR (kpjba : fnblpds);
022500140725          pIn_OPZ = 'C';
022600140725          callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
022700140725                              pIn_TNT : pOut_RNT : pOut_NTR : pOut_Esito);
022800140623       else;
022900140623          eval *inrt = *on;
023000110627       endif;
023100140617
023200140623       unlock(e) fiar504l;
023300140623       unlock(e) tivgd00f;
023400130626
023500130626
023600130626
023700130626       //-------------------------------------------------------------*
023800130626       //Avvio il blocco elaborazione TIVGD x tipo file in questione  *
023900130626       //-------------------------------------------------------------*
024000130626        Begsr strTRUL47;
024100130626
024200130626          clear trul47ds;
024300130626          d47opz  = 'I';
024400130626          d47tip  = 'SM';
024500130626          d47lck  = 'N';
024600130626          d47chkj = 'S';
024700130626          d47pgm  = 'FNLSSMR';
024800130626          callP(e) TRUL47R (trul47ds);
024900130626
025000130626          if d47sts <> 'A';
025100140731          else;
025200140731             wProcedi = 'N';
025300130626          endif;
025400130626
025500130626        endsr;
025600130626
025700130626
025800130626
025900130626       //-------------------------------------------------------------*
026000130626       //Elimino il blocco elaborazione TIVGD x tipo file in questione*
026100130626       //-------------------------------------------------------------*
026200130626        Begsr endTRUL47;
026300130626
026400130626          if wProcedi = 'S';
026500130626
026600130626             clear trul47ds;
026700130626             d47opz  = 'F';
026800130626             d47tip  = 'SM';
026900130626             callP(e) TRUL47R (trul47ds);
027000130626
027100130626          endif;
027200130626
027300130626        endsr;
027400130719
027500130719
027600130719
027700130719       //-------------------------------------------------------------*
027800130719       //Forzo l'esecuzione del download                              *
027900130719       //-------------------------------------------------------------*
028000130719        Begsr forzaDWL;
028100130719
028200130719
028300130719          // Forzo lo scarico del buffer d output del TIVGD
028400130719          feod tivgd00f;
028500130719
028600130719          clear tis781ds;
028700131004          §781tip = 'SM';
028800131004          §781ksu = '0BART001';
028900131004          §781tsc = 'WW';
029000131004          §781dat = currDate;
029100131004          §781pgm = 'FNLSSMR';
029200130719
029300131004          clear tis781dflo;
029400130719          §781flocsi = 'S';
029500131004          §781floela = 'P';
029600130719          §781flo    = tis781dflo;
029700130719
029800130719          wEsito = *blanks;
029900130719          vgdPRG = *blanks;
030000130719          callP(e) TIS781C1  (wEsito : tis781ds : vgdPRG);
030100130719
030200130719        endsr;
030300130626
030400130626
030500130626
030600130626       //-------------------------------------------------------------*
030700130626       //Reperimento progressivo                                      *
030800130626       //-------------------------------------------------------------*
030900130626        Begsr sr_rtvPrg;
031000130626
031100130626          clear trul33ds;
031200130626          i33cnu = 24;
031300130626          i33num = 1;
031400130626          kpjbu = trul33ds;
031500130626          trul33r (kpjba);
031600130626          trul33ds = kpjbu;
031700130626
031800130626        endsr;
031900130626
032000130406
032100130406
032200110617       //-------------------------------------------------------------*
032300110617       //Lettura fiar5 record "EMD" da inviare                         *
032400110617       //-------------------------------------------------------------*
032500110617       Begsr SR_Elabora;
032600130626
032700130626        kdos = *blanks;
032800130626        setll (kdos) fiar504l;
032900130626        reade (kdos) fiar504l;
033000110617
033100130920        dow not %eof(fiar504l) and wCurRecFile <= wMaxRecFile;
033200130406
033300130626           // controllo se è stata richiesta la chiusura del sottosistema
033400110627           if %shtdn;
033500130626               chiudi = 'S';
033600110627               leavesr;
033700110627           endif;
033800141201
033900141201           // Gestione "civette"
034000141201           if wCurCivetta >= wMaxCivetta;
034100141201              exsr exeVGDcivetta;
034200141201              wCurCivetta = *zeros;
034300141201           endif;
034400141201
034500130406
034600130626           // controllo lo stato della bolla
034700110617           exsr sr_chkstblp;
034800140805           if bolok <> *blanks;
034900130719
035000130703              // inizializzo DS di work
035100130703              clear DS_Output;
035200130701              clear fiar9000;
035300130703
035400130703              // provo a reperire il contrassegno
035500130701              chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
035600130703
035700130703              // compongo l'output per l'invio SMS
035800130703              dar5emd = ar5uni;
035900130719
036000130719              // normalizzo e verifico il numero di cellulare
036100130719              *in33 = *off;
036200130719              exsr sr_normCELL;
036300130719              if not *in33;
036400130719                 exsr sr_inviaSMS;
036500130719              endif;
036600130703
036700130703              // aggiorno il record corrente come SMS inviato
036800150930              exsr sr_UpdFIAR5;
036900150930
037000110617           endif;
037100130406
037200130701           reade (kdos) fiar504l;
037300110617        enddo;
037400130406
037500130626       endsr;
037600150930
037700150930
037800150930
037900150930       //-------------------------------------------------------------*
038000150930       // Gestione tentativi esecuzioni comandi                       *
038100150930       //-------------------------------------------------------------*
038200150930        Begsr sr_UpdFIAR5;
038300150930
038400150930          eval §ar5dos=%char(%dec(%timestamp:*ISO));
038500150930          §ar5DPC  = %editc(wdcrRev:'X');
038600150930          §ar5OPCD = %editc(OOR2STIS:'X');
038700150930          §ar5OPCA = %editc(OOR2STFS:'X');
038800150930          // Verifico che i valori sopra attribuiti siano diversi da zero
038900150930          if §AR5DPC  = *all'0';
039000150930             §AR5DPC  = *blanks;
039100160330             §AR5OPCD = *zeros;
039200160330             §AR5OPCA = *zeros;
039300150930          endif;
039400150930          if §AR5OPCD = *all'0';
039500150930             §AR5OPCD = *blanks;
039600150930          endif;
039700150930          if §AR5OPCA = *all'0';
039800150930             §AR5OPCA = *blanks;
039900150930          endif;
040000150930          ar5uni=dar5emd;
040100150930          if chiudi = *blanks;
040200150930             update fiar5000;
040300150930          endif;
040400150930
040500150930        endsr;
040600130406
040700130626
040800130406
040900110617       //-------------------------------------------------------------*
041000110617       //Verifica stato della bolla                                   *
041100110617       //-------------------------------------------------------------*
041200130625        Begsr sr_chkstblp;
041300130406
041400130626           bolok=' ';
041500131104           bologgi=' ';
041600130626
041700130626           // Effettuo controlli tramite driver che detrmina "inviabilità"
041800130626           // delle comunicazioni ai destinatari
041900130626           clear fnlvemds;
042000130626           clear fnblpds;
042100140725           clear wRSM_Sms;
042200130626           iemAAS = ar5aas;
042300130626           iemLNP = ar5lnp;
042400130626           iemNRS = ar5nrs;
042500130626           iemNSP = ar5nsp;
042600130626           kpjbu = fnlvemds;
042700130626           callP(e) FNLVEMR (kpjba : fnblpds);
042800130626           fnlvemds = kpjbu;
042900140725
043000130626
043100130626           // Verifico se esito favorevole al invio
043200130626           if oemESITO = '1';
043300150929
043400150929           // Verifico che NON trattasi di spedizione "export"
043500150929           if  %lookup(blpLNA:skFilExt) > 0;
043600150929               bolok = 'E';
043700150929           endif;
043800150929
043900150929           // Se tutto OK => procedo
044000150929           if bolok <> 'E';
044100140725
044200140805              // Verifico se per cliente corrente presente nota specifica
044300140725              // per Rag.Soc. invio alerts
044400140725              if blpCCM > *zeros;
044500140725                 pIn_NK1 = '0151' + %editc(blpCCM:'X');
044600140725              else;
044700140725                 pIn_NK1 = '0151' + %editc(blpKSC:'X');
044800140725              endif;
044900140725              // --- SMS ---
045000140725              pIn_OPZ = *blanks;
045100140725              pIn_APL = 'C';
045200140725              pIn_NK2 = *blanks;
045300140725              pIn_TNT = 'AS';
045400140725              callP(e) UBREPNTCR (pIn_OPZ : pIn_APL : pIn_NK1 : pIn_NK2 :
045500140725                                  pIn_TNT : wRSM_Sms : pOut_NTR : pOut_Esito);
045600140725              if pOut_Esito = '1';
045700140725              else;
045800140725                 wRSM_Sms = %trim(blpRSM);
045900140725              endif;
046000141016
046100141016
046200141016              // Calcolo il BRTcode (tramite apposito driver)
046300141016              clear BRTcode;
046400141016              BRTcode.aas4    = blpAAS;
046500141016              BRTcode.lnp     = blpLNP;
046600141016              BRTcode.nrs     = blpNRS;
046700141016              BRTcode.nsp     = blpNSP;
046800141016
046900141016              clear trul28ds0;
047000141016              trul28ds0.i280COD = BRTcode.BRTcode;
047100141016              callP(e) TRUL28R0 (trul28ds0);
047200141016
047300141016              // Se BRTcode errato => esito KO
047400141016              clear wBRTcodeLong;
047500141016              if %error OR trul28ds0.o280err = '1';
047600141016              else;
047700150530                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +
047800150530                                %subst(trul28ds0.o280cod:6:5) +
047900150530                                %subst(trul28ds0.o280cod:11:5)+
048000141016                                %subst(trul28ds0.o280cod:16:4);
048100141016              endif;
048200131104
048300141016
048400131104              // Effettuo verifica anche su data/ora invio rispetto alla bolla
048500131104              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
048600131104                 bolok='S';
048700131104              endif;
048800131104              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
048900131104                 %dec(%time():*ISO)  >= 150000;
049000131104                 bolok='S';
049100131104                 bologgi='S';
049200131104              endif;
049300140805
049400140805              if bolok='S';
049500140805                 // Verifico per sicurezza se già presente evento 'MIC'
049600140805                 chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'MIC') fnevb04l;
049700140805                 if %found(fnevb04l);
049800140805                    bolok='X';
049900140805                 endif;
050000140805              endif;
050100150929
050200150929           endif;
050300131104
050400130626           endif;
050500130406
050600110617        endsr;
050700130719
050800130719
050900130719
051000130719       //-------------------------------------------------------------*
051100130719       //Normalizza numero ci cellulare                               *
051200130719       //-------------------------------------------------------------*
051300130719        Begsr sr_normCELL;
051400130719
051500131017           // Chiamo il driver preposto
051600131017           pInCell = %trim(§ar5TEL);
051700131017           pOutErr = *blanks;
051800131017           if UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
051900131017              §ar5SME = 'E';
052000131017              *in33 = *on;
052100131017           else;
052200131017              o_cell = pOutCell;
052300131017           endif;
052400130719
052500130719        endsr;
052600130406
052700130626
052800130406
052900110617       //-------------------------------------------------------------*
053000130626       //Richiamo driver per invio SMS e aggiorno dos su fiar5        *
053100110617       //-------------------------------------------------------------*
053200130626        Begsr sr_inviaSMS;
053300130719
053400130719          // Stacco un progressivo per il file SMS corrente
053500130719          if not *in34;
053600130719             exsr sr_rtvPrg;
053700130719             *in34 = *on;
053800130719          endif;
053900130406
054000130626          // determino data consegnabilità
054100130626          exsr sr_calcons;
054200130626
054300130626          // Compongo il testo del SMS
054400130701          exsr valTextSMS;
054500130626
054600130626          // Aggiungo l'SMS corrente
054700130626          exsr exeVGD;
054800130406
054900110617        endsr;
055000130626
055100130626
055200130626
055300130626       //-------------------------------------------------------------*
055400130626       //Verifica eventuali personalizzazioni per il cliente          *
055500130626       //-------------------------------------------------------------*
055600130701        Begsr sr_calcons;
055700130626
055800130701        clear wcliper;
055900130701        clear wdcr;
056000140327        clear wdcrRev;
056100141128        clear wPINcode;
056200140331        clear tnsd99ds;
056300140324        clear trulORsds;
056400140324        clear trulOR2ds;
056500150730        clear diore01;
056600141128
056700141128        // Verifica se spedizione con particolarità consegna "PINcode"
056800141128        if blpGMA <> *blanks;
056900141128           if %lookup(blpGMA:sk7R_PINcode)>0;
057000141128              wPINcode = 'S';
057100141128            endif;
057200141128        endif;
057300130701
057400130701        // Se presente il contrassegno verifica se richiesto il contatto
057500130701        if ar9cas > 0;
057600130701           clear trul21ds;
057700130701           i21tla = 'L';
057800130701           i21cbo = blpcbo;
057900130701           i21tsp = blptsp;
058000130701           i21lnp = blplnp;
058100130701           i21nzm = blpnzm;
058200130701           i21lna = blplna;
058300130701           i21nzd = blpnzd;
058400130701           i21tic = ar9tic;
058500130701           i21imp = ar9cas;
058600130701           i21div = ar9vca;
058700130701           i21ute = sdsusr;
058800130701           i21pgm = nompgm;
058900130829           //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
059000130829              if %lookup(blpcbo:cbf)>0;
059100130829                 i21ksc = blpksc;
059200130829              else;
059300130829                 i21ksc = blpccm;
059400130829                 if i21ksc=0;
059500130829                    i21ksc=(blplnp*10000) + 8888;
059600130829                 endif;
059700130829              endif;
059800130701           trul21r(trul21ds);
059900130701        endif;
060000130701
060100130701        // L'indicazione della consegna prevista viene fatta solo se la
060200130701        // spedizione non prevede contatti col destinatario:
060300130701
060400130701           // No Fermo deposito
060500130701        if blpffd = *blanks and (
060600130701           // No appuntamento o supermercato OPPURE indicata DCR tassativa
060700130701        (blptc1 <> 'A' and blptc1 <> 'S' and blptc2 <> 'A' and blptc2 <> 'S') or
060800130701        (blpdcr  > *zeros and blptcr = *blanks))
060900130701           // No contrassegno oppure importo che non richiede
061000130701           // l'abilitazione "manuale"
061100130701        and (ar9cas = *zeros or o21fca = *blanks);
061200130701           wcliper = 'S';
061300130701        endif;
061400130701
061500130701
061600130701        // Se non è indicata la data consegna richiesta
061700130701        // viene calcolata mediante pgm di calcolo delivery
061800130701        if wcliper = 'S' ;
061900160502         //  if blpdcr = *zeros;
062000130701              d98tbo = 'P';
062100130701              d98aas = blpaas;
062200130701              d98lnp = blplnp;
062300130701              d98nrs = blpnrs;
062400130701              d98nsp = blpnsp;
062500130701              if blptsp = 'D';
062600130701                 I98TSP_FOR = 'C';
062700130701              endif;
062800130701              tnsd99r (tnsd99ds);
062900130701              if d98err = '0';
063000151111                 wdcr = d98dee;
063100160502              else  ;
063200160502                 if blpdcr>0 ;
063300160502                  wdcr = blpdcr;
063400160502                 endif   ;
063500130701              endif;
063600160502          //        else;
063700160502          //           wdcr = blpdcr;
063800160502          //        endif;
063900160502
064000140327           wdcrRev = wdcr;
064100130701           if wdcr > *zeros;
064200130701              dataeur = %date(wdcr : *ISO);
064300130701              wdcr = %dec(dataeur);
064400130701           endif;
064500130701        endif;
064600140324
064700140324        wSpedConsOrari = '.';
064800141016        clear wSpedConsOraDa;
064900141016        clear wSpedConsOraA;
065000140324
065100140327        IOREDTA  = wdcrRev;
065200140324        IOREFIL  = blpLNA;
065300140324        IORECAP  = blpCAD;
065400140324        IORELOC  = blpLOD;
065500140324        IORENAR  = blpNZD;
065600140324        IORETSER = 'C';
065700140324        IOREDDC  = blpDDC;
065800140324        IORENDC  = blpNDC;
065900140324        IORETFP  = blpTFP;
066000140324        IORETFA  = blpTFA;
066100140324        IOREDTI  = blpDTI;
066200140324        IOREHTI  = blpHTI;
066300140324        IOREDCR  = blpDCR;
066400140324        IOREHCR  = blpHCR;
066500140324        IORETCR  = blpTCR;
066600140324        IOREDEI  = d98DEI;
066700140324        IOREOEI  = d98OEI;
066800140324        IORETSP  = blpTSP;
066900150730        §IORETRAZC = %editc(D98TTC:'X');
067000150730        §IORECONSC = %editc(D98TCC:'X');
067100150730        IOREFLO  = diore01;
067200140324
067300140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
067400140324        if not %error AND OOREERR = *blanks;
067500141016           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
067600141016           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
067700141016           //wSpedConsOraDa = %trim(%editc(OOR2STIS:'X'));
067800141016           //wSpedConsOraA  = %trim(%editc(OOR2STFS:'X'));
067900150530           wSpedConsOrari = '(' + wSpedConsOraDa + '  ' +
068000150530                                  wSpedConsOraA  + ')';
068100140324        endif;
068200130626
068300130626        endsr;
068400130701
068500130701
068600130701
068700130701       //-------------------------------------------------------------*
068800130701       //Compongo il testo del SMS                                    *
068900130701       //-------------------------------------------------------------*
069000130701        Begsr valTextSMS;
069100130701
069200130703          wSmsText = *blanks;
069300131104          wTracking = *blanks;
069400140423          clear wConsegna;
069500131104
069600131104          if bologgi='S';
069700150530             wTracking = ' Verifichi da domani su www.brt.it';
069800131104          else;
069900150530             wTracking = ' Verifichi su www.brt.it';
070000131104          endif;
070100131104
070200130703
070300130701          if blpffd='S';
070400150530             wSmsText = 'Spedizione da ' + %trim(%subst(wRSM_Sms:1:23)) +'.';
070500140423
070600141014               // In attesa di data per Fermo Deposito asteriscare 2 spec. sotto
070700150530                 //wConsegna = 'Potrà ritirare dal ' +
070800141014                 //%subst(%trim(%editw(wdcr:'0  /  /    ')):1:5);
070900141014                 wConsegna = '';
071000140423
071100150530                 wSmsText = %trim(wSmsText) + wConsegna + ' ' +
071200150530                            %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
071300130701          else;
071400130701            if wcliper = 'S'and wdcr > *zeros ;
071500130701               select;
071600130701               when blptcr=*blanks;
071700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
071800140423
071900150530                       wConsegna = ' Consegna prevista il ' +
072000140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
072100140423                       %trim(wSpedConsOrari);
072200140423
072300150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
072400150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
072500150530
072600130701               when blptcr='D';
072700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
072800140423
072900150530                       wConsegna = ' Consegna prevista dopo il ' +
073000140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
073100140423                       %trim(wSpedConsOrari);
073200140423
073300150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
073400150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
073500130701               when blptcr='P';
073600150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
073700140423
073800150530                       wConsegna = ' Consegna prevista prima del ' +
073900140714                       %subst(%trim(%editw(wdcr:'0  /  /    ')):1:5) + ' ' +
074000140423                       %trim(wSpedConsOrari);
074100140423
074200150530                       wSmsText = %trim(wSmsText) + wConsegna + ' ' +
074300150530                                  %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
074400130701               endsl;
074500130701
074600130701              else;
074700150530                   wSmsText='Spedizione da '+%trim(%subst(wRSM_Sms:1:23))+'.';
074800150530                   wSmsText = %trim(wSmsText) + ' ' +
074900150530                              %trim(wTracking) + ' BRTcode ' + wBRTcodeLong;
075000130701              endif;
075100130701          endif;
075200141128
075300141128
075400141128          // Se consegna con "PINcode" aggiungo reminder
075500141128          if wPINcode = 'S';
075600141128             wSmsText = %trim(wSmsText) + ' ' +
075700141128                      'NB: Ricordiamo che in fase di consegna verrà richiesto' +
075800141128                      ' l''inserimento del PIN di sicurezza.';
075900141128          endif;
076000141128
076100130701
076200130701        endsr;
076300130703
076400130703
076500130703
076600130703       //-------------------------------------------------------------*
076700130703       //Scrittura dato di output per creazione file massivo SMS      *
076800130703       //-------------------------------------------------------------*
076900130703        Begsr exeVGD;
077000130703
077100131015           wForzaDWL = *on;
077200131015
077300130704           o_MittSMS = 'BRT SpA';
077400130704           o_message = wSmsText;
077500130704           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
077600130704                       %editc(blpAAS:'X')+%editc(blpLNP:'X')    +
077700130704                       %editc(blpNRS:'X')+%editc(blpNSP:'X')    +'_'+
077800130704                       'SAAS';
077900130704
078000140805           // Invio fisicamente l'SMS solo se bolla perfettamente OK (bolok='S')
078100140805           if bolok = 'S';
078200140805
078300140805              clear tivgd000;
078400141016
078500141016              select;
078600141016                when chiudi = 'T';
078700141016                     o_cell = '3473441969';
078800141016              endsl;
078900160706
079000160706              o_message = %scanrpl(wSep:'':o_message);
079100140805
079200140805              vgddta = %trim(o_MittSMS) + wSep +
079300140805                       %trim(o_IdSMS)   + wSep +
079400140805                       %trim(o_cell)    + wSep +
079500140805                       %trim(o_message);
079600130704
079700140805              vgdtip = 'SM';
079800140805              vgdksu = '0BART001';
079900140805              vgdtsc = 'WW';
080000140805              vgddat = currDate;
080100140805              // vgdprg = %subst(%editc(o33nri:'X'):6:10);
080200140805              vgdpgm = 'FNLSSMR';
080300140805
080400140805              clear dvgdflo;
080500140805              //§vgdFELA = 'P';
080600140805              §vgdFELA = *blanks;
080700140805              vgdFLO   = dvgdflo;
080800140805
080900140805              write tivgd000;
081000140805
081100140805              // Incremento il conteggio degli SMS inseriti nel file corrente
081200140805              wCurRecFile = wCurRecFile + 1;
081300141201              wCurCivetta = wCurCivetta + 1;
081400140414
081500150930              // Per ogni SMS inserito per l'invio => scrittura R.A.
081600150930              exsr sr_InsRA;
081700150930
081800140805           endif;
081900140414
082000130703        endsr;
082100140414
082200141201
082300141201       //-------------------------------------------------------------*
082400141201       //Scrittura dato di output per SMS "civetta"                   *
082500141201       //-------------------------------------------------------------*
082600141201        Begsr exeVGDcivetta;
082700141201
082800141201
082900141201           o_MittSMS = 'BRT SpA';
083000141201           o_message = 'SMS civetta inviato ' + %char(%date()) + ' - ' +
083100141201                                                %char(%time());
083200141201           o_IdSMS   = %subst(%char(%dec(%timestamp:*ISO)):1:14)+'_'+
083300141201                       'SMS_CIVETTA' +'_'+ 'SAAS';
083400141201           o_cell = '393471487911';
083500141201
083600141201           clear tivgd000;
083700141201
083800141201           vgddta = %trim(o_MittSMS) + wSep +
083900141201                    %trim(o_IdSMS)   + wSep +
084000141201                    %trim(o_cell)    + wSep +
084100141201                    %trim(o_message);
084200141201
084300141201           vgdtip = 'SM';
084400141201           vgdksu = '0BART001';
084500141201           vgdtsc = 'WW';
084600141201           vgddat = currDate;
084700141201           vgdpgm = 'FNLSSMR';
084800141201
084900141201           clear dvgdflo;
085000141201           §vgdFELA = *blanks;
085100141201           vgdFLO   = dvgdflo;
085200141201
085300141201           write tivgd000;
085400141201
085500141201        endsr;
085600141201
085700140414
085800140414       //-------------------------------------------------------------*
085900140414       //inserimento Richiesta Assistenza                             *
086000140414       //-------------------------------------------------------------*
086100140414        Begsr sr_InsRA;
086200141016
086300141016         if chiudi = *blanks;
086400140414
086500140805           // Inserimento R.A. via driver
086600140414           wSpedNum   = %trim(%editc(blpLNP:'X')) +
086700140414                        %trim(%editc(blpNRS:'X')) +
086800140414                        %trim(%editc(blpNSP:'X')) +
086900140414                        %trim(%editc(blpAAS:'X'));
087000140414
087100140414           clear fidna6ds;
087200140414
087300140423           iDA06MAD = ' 82';
087400140908
087500140908           iDA06RSC = 'SMS';
087600140908           iDA06NO1 = 'SMS Affidamento a: ' + %trim(o_cell) +
087700140609                      '  -  ' + wConsegna;
087800140414           iDA06TOR = 'S';
087900140414           iDA06OGG = %trim(wSpedNum);
088000140414           iDA06UTE = 'QPGMR';
088100140610           iDA06FIL = blpLNP;
088200140414
088300140414           callP(e) FIDNA6R (fidna6ds);
088400160506
088500160506           // Inserimento spia per Predict DPD
088600160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
088700160506               exsr insPredict;
088800160506           endif;
088900141016
089000141016         endif;
089100140414
089200140414        endsr;
089300160506
089400160506
089500160506       //-------------------------------------------------------------*
089600160506       //inserimento Predict DPD                                      *
089700160506       //-------------------------------------------------------------*
089800160506        Begsr insPredict;
089900160506
090000160506           clear tidp3000;
090100160506           dp3AAS   = blpAAS;
090200160506           dp3LNP   = blpLNP;
090300160506           dp3NRS   = blpNRS;
090400160506           dp3NSP   = blpNSP;
090500160506           dp3EVD   = '18';
090600160506           dp3CEV   = 'S25';
090700160506           dp3FLE   = blpLNA;
090800160518           dp3DEV   = 99991231;
090900160512           dp3HEV   = %dec(%time() : *HMS)/100;
091000160512           dp3DTSPE = blpAAS*10000+blpMGS;
091100160506           write tidp3000;
091200160506
091300160506        endsr;
091400140414
091500140414
091600130829       //-------------------------------------------------------------*
091700130829       //caricamento codici bolla Franco                               *
091800130829       //-------------------------------------------------------------*
091900130829       Begsr SR_cartab;
092000130829
092100130829       // Leggo tabella "3A" per caricare schiera dei codici bolla franco
092200130829       y=0;
092300130829       clear cbf;
092400130829       setll (1:'3A') tabel00f  ;
092500130829       reade (1:'3A') tabel00f  ;
092600130829
092700130829  1    dow    not %eof(tabel00f)  ;
092800130829          ds3a=tbluni           ;
092900130829          if %subst(§3atb1:1:1)='F';
093000130829             y=y+1;
093100130829             cbf(y)=tblkey;
093200130829          endif;
093300130829          reade (1:'3A') tabel00f  ;
093400130829  1    enddo     ;
093500141128
093600141128
093700141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
093800141128       clear sk7R_PINcode;
093900141128       clear ix7R;
094000141128       setll (1:'7R') tabel00f;
094100141128       reade (1:'7R') tabel00f;
094200141128
094300141128       dow    not %eof(tabel00f);
094400141128          ds7r=tbluni;
094500141128          if §7RPINCODE = 'S';
094600141128             ix7R = ix7R + 1;
094700141128             sk7R_PINcode(ix7r)=tblkey;
094800141128          endif;
094900141128          reade (1:'7R') tabel00f;
095000141128       enddo;
095100150929
095200150929
095300150929       // Carico la lista di filiali "estere" (secondo il network)
095400150929       clear skFilExt;
095500150929       clear ixFilExt;
095600150929       setll (*zeros) azorg01l;
095700150929       read azorg01l;
095800150929
095900150929       dow not %eof(azorg01l);
096000150929           if ORGFVA = *blanks;
096100150929              og143 = orgDE3;
096200150929              chain ('NTW':§OGNTW) tntbe01l;
096300150929              if %found(tntbe01l);
096400150929                 dntw = tbeuni;
096500150929                 if §NTWFIE = 'E';
096600150929                    ixFilExt = ixFilExt + 1;
096700150929                    skFilExt(ixFilExt) = orgFIL;
096800160506                    if §OGNTW = 'DPD';
096900160506                       skFilExtDPD(ixFilExt) = orgFIL;
097000160506                    endif;
097100150929                 endif;
097200150929              endif;
097300150929           endif;
097400150929           read azorg01l;
097500150929       enddo;
097600130703
097700130829       endsr;
