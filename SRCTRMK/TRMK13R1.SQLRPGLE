000100110525      //--------------------------------------------------------------
000200110525      //?TRMK13R1 - Creo file di WORK per stampa ANGRAFICA POTENZIALI
000300110525      //--------------------------------------------------------------
000400110525
000500110525     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600110525     h dftactgrp(*no) actgrp(*caller)
000700110526
000800110526      //--------------------------------------------------------------
000900110526      //?Dichiarazione file.
001000110526      //---------------------------------------------------------------
001100110526
001200110526      // - Organigramma filiale/aziendale
001300110526     fAZORG01L  if   e           k disk
001400110530
001500130916      // - Anagrafica Commerciali
001600130916     fAZCMM01L  if   e           k disk
001700110530
001800110530      // - Attività
001900110531     fTIATC05L  if   e           k disk
002000110530
002100110530      // - Trattative
002200110530     fTIVIS01L  if   e           k disk
002300110526
002400110526      // - Anagrafica Potenziale
002500110530     fTNCPO01L  if   e           k disk
002600110526     fTNCPO03L  if   e           k disk    usropn
002700110526     f                                     rename(TNCPO000:TNCPO03)
002800110530     fTNCPO04L  if   e           k disk    usropn
002900110530     f                                     rename(TNCPO000:TNCPO04)
003000110530     fTNCPO08L  if   e           k disk    usropn
003100110530     f                                     rename(TNCPO000:TNCPO08)
003200110526
003300110526      // - Work File per la stampa
003400110530     fWFCPO00F  o    e             disk    usropn
003500110526
003600110526      //---------------------------------------------------------------
003700110526      //?Definizione costanti.
003800110526      //---------------------------------------------------------------
003900110526
004000110526
004100110526      //---------------------------------------------------------------
004200110526      //?Definizione schiere.
004300110526      //---------------------------------------------------------------
004400110526
004500110526      // - Filiali abilitate
004600110526     d POG             s              3    DIM(250)
004700110530
004800110530      // - Agenti (appoggio)
004900110530     d $Age            s              7  0 dim(3500)
005000110526
005100110530      // - Agente Unificante (figli)
005200110530     d $Cmmu           s              7  0 dim(3500)
005300110530
005400110530      // - Agente Unificante prossima attività (figli)
005500110530     d $Cmm            s              7  0 dim(3500)
005600110526
005700110526      //---------------------------------------------------------------
005800110526      //?Definizione aree dati.
005900110526      //---------------------------------------------------------------
006000110526
006100110526      // - Dati utente
006200110526     d §AzUte        e ds                  extname(AZUTE00F)
006300110526     d                                     dtaara
006400110526     d §DatiUte      e ds                  extname(dDatiUte)
006500110526     d                                     dtaara
006600110526
006700110526      //---------------------------------------------------------------
006800110526      //?Definizione strutture dati.
006900110526      //---------------------------------------------------------------
007000110526
007100110526      // - Status
007200110526     d Psds           sds
007300110526     d   SDSpgm          *proc
007400110526
007500110526      // - Parametri ricevuti
007600110526     d KPJBA         e ds
007700140925     d TRMK13ds      e ds
007800110531     d  $cic                  68     72    dim(5)
007900110531     d  $fls                  73     77    dim(5)
008000110531     d  $cco                  88     97    dim(5)
008100110531
008200110531      // - Ds x campo CPORST
008300110531     d dCPO01        e ds                  inz
008400140925
008500140925      // - Dati in CPOTEL
008600140925     d dCPO02        e ds                  inz
008700110530
008800110530      // - File TNCPO
008900110530     d TNCPOds       e ds                  extname(TNCPO00F)
009000110526
009100110526      //---------------------------------------------------------------
009200110526      //?Definizione variabili globali.
009300110526      //---------------------------------------------------------------
009400110526
009500110526      // - Indici di schiera
009600110526     d xx              s              4  0 inz
009700110526     d yy              s              4  0 inz
009800110526     d zz              s              4  0 inz
009900110526
010000110526       // - Stringa SQL da eseguire
010100110526     d StringaSql      s           2048    Varying        inz
010200110526
010300110526       // - Flags booleani
010400110526     d $End            s               n   inz(*off)
010500110526     d $EndAge         s               n   inz(*off)
010600110531     d $EndAtc         s               n   inz(*off)
010700110531     d $EndVis         s               n   inz(*off)
010800110531     d $InfoAbi        s               n
010900110526     d $RcdOk          s               n   inz(*off)
011000110526
011100110526       // - Campi per richiamo QCMDEXC
011200110526     d Qcmd            s            128    inz
011300110526
011400110526       // - Campi di comodo
011500110526     d wAge            s              7  0 inz
011600110530     d wAtt            s              3  0 inz
011700110608     d wCmm            s              7  0 inz
011800110608     d wDate           s              8  0 inz
011900110530     d wPos            s              3  0 inz
012000110530     d wTtr            s              3  0 inz
012100110526
012200110526      //---------------------------------------------------------------
012300110526      //?Definizione procedure usate.
012400110526      //---------------------------------------------------------------
012500110526
012600110526
012700110526      //---------------------------------------------------------------
012800110526      //?Prototipi.
012900110526      //---------------------------------------------------------------
013000110526
013100110526      /copy gaitrasrc/srcprotopr,QCMDEXC
013200110526
013300110526      //---------------------------------------------------------------
013400110526      //?Definizione key-list.
013500110526      //---------------------------------------------------------------
013600110526
013700110526
013800110526      //---------------------------------------------------------------
013900110526      //?M A I N - L I N E
014000110526      //---------------------------------------------------------------
014100110526
014200110526     c     *Entry        plist
014300110526     c                   parm                    KPJBA
014400110526     c                   parm                    pog
014500110526     c                   parm                    $InfoAbi
014600140925     c                   parm                    TRMK13DS
014700110530
014800110530      /free
014900110526
015000110526       //?Operazioni iniziali
015100110526       exsr RoutInz;
015200110526
015300110530       //?Elaborazione
015400110530       exsr Elabora;
015500110526
015600110526       //?Operazioni finali
015700110526       exsr RoutEnd;
015800110526
015900110526       //--------------------------------------------------------------
016000110526       //?Operazioni iniziali.
016100110526       //--------------------------------------------------------------
016200110526       BEGSR RoutInz;
016300110526
016400110526       // -?Duplico il file in qtemp?
016500110526         Qcmd = 'CRTDUPOBJ OBJ(WFCPO00F) FROMLIB(';
016600110526         IF  %subst(knsif : 7 : 1) = 'P';
016700110720           Qcmd = %trimr(Qcmd) + 'GAITRAAZP)';
016800110526         ELSE;
016900110720           Qcmd = %trimr(Qcmd) + 'GAITRAAZM)';
017000110526         ENDIF;
017100110607         Qcmd = %trimr(Qcmd) + ' OBJTYPE(*FILE) TOLIB(QTEMP)';
017200110526         ExecuteCommand (Qcmd : %size(Qcmd));
017300110530
017400110530         open WFCPO00F;
017500110607
017600140925         //trmk13ds = kpjbu;
017700110526
017800110531       //?Se richiesto Commerciale Unificante oppure
017900110531       //?Commerciale Unificante prossima attività
018000110531       //?carico la sk con i figli
018100110530         IF  D13cmmu <> 0;
018200110530           wAge = D13cmmu;
018300110530           exsr CarAge;
018400110530           $Cmmu = $Age;
018500110530         ENDIF;
018600110530         IF  D13cmm <> 0;
018700110530           wAge = D13cmm;
018800110526           exsr CarAge;
018900110530           $Cmm = $Age;
019000110526         ENDIF;
019100110526
019200110526       ENDSR;
019300110526
019400110526       //--------------------------------------------------------------
019500110526       //?Carico Sk Agenti.
019600110526       //--------------------------------------------------------------
019700110526       BEGSR CarAGE;
019800110526
019900110526         clear xx;
020000110526         clear $Age;
020100110526         $EndAge = *off;
020200110526
020300110526         exec sql
020400110526          declare AGE cursor for
020500130916          select CMMcod, CMMuni
020600130916          from AZCMM00F
020700130916          order by CMMcod;
020800110526
020900110526         exec sql
021000110526           open AGE;
021100110526           IF sqlcode < 0;
021200110526             $EndAge = *on;
021300110526           ENDIF;
021400110526
021500110526         DOW not $EndAge;
021600110526           exec sql
021700130916             fetch next from AGE into :CMMcod, :CMMuni;
021800110526             IF sqlcod = 100 or sqlcod < 0;
021900110526               $EndAge = *on;
022000110526               leave;
022100110526             ENDIF;
022200130916             IF  CMMuni = wAge;
022300110526               xx += 1;
022400130916               $Age(xx) = CMMcod;
022500110526             ENDIF;
022600110526         ENDDO;
022700110526
022800110526         exec sql close AGE;
022900110526
023000110531       //?Se non ho cricato niente memorizzo almeno il codice richiesto
023100110526         IF $Age(01) = 0;
023200110526           $Age(01) = wAge;
023300110526         ENDIF;
023400110526
023500110526       ENDSR;
023600110530
023700110530       //--------------------------------------------------------------
023800110530       //?Elaborazione.
023900110530       //--------------------------------------------------------------
024000110530       BEGSR Elabora;
024100110530
024200110530         SELECT;
024300110531       //?Ciclo per sk agenti (figli Unificante richiesto)
024400110531       //?e leggo per commerciale
024500110530           WHEN  D13cmmu > 0;
024600110530             sorta $Cmmu;
024700110530             exsr ReadCpo8;
024800110531       //?Leggo per provincia
024900110530           WHEN  D13prv > *blanks;
025000110530             exsr ReadCpo4;
025100110531       //?Leggo per categoria mercologica
025200110531           WHEN  D13sct > *zeros;
025300110530             exsr ReadCpo3;
025400110530           OTHER;
025500110531       //?leggo con SQL
025600110530             exsr ReadSql;
025700110530         ENDSL;
025800110530
025900110530       ENDSR;
026000110530
026100110530       //--------------------------------------------------------------
026200110530       //?Ciclo per sk Agenti.
026300110530       //--------------------------------------------------------------
026400110530       BEGSR ReadCpo8;
026500110530
026600110530         open TNCPO08L;
026700110526
026800110530         zz = 1;
026900110530         FOR zz by 1 to %elem($CmmU);
027000110530           IF  $CmmU(zz) <> *zeros;
027100110530
027200110530             setll ($Cmmu(zz)) TNCPO08L;
027300110530             reade ($Cmmu(zz)) TNCPO08L;
027400110530             DOW  not %eof(TNCPO08L);
027500110530           //?Controllo se il rcd letto è OK
027600110531               exsr CtrRcd;
027700110530               IF  $RcdOk;
027800110531                 exsr Scrivi;
027900110530               ENDIF;
028000110530               reade ($Cmmu(zz)) TNCPO08L;
028100110530             ENDDO;
028200110530           ENDIF;
028300110530
028400110530         ENDFOR;
028500110530
028600110530       ENDSR;
028700110530
028800110530       //--------------------------------------------------------------
028900110530       //?Leggo per Provincia.
029000110530       //--------------------------------------------------------------
029100110530       BEGSR ReadCpo4;
029200110530
029300110530         open TNCPO04L;
029400110530
029500110530         setll (D13prv) TNCPO04L;
029600110530         reade (D13prv) TNCPO04L;
029700110530         DOW  not %eof(TNCPO04L);
029800110531         //?Controllo se il rcd letto è OK
029900110531           exsr CtrRcd;
030000110530           IF  $RcdOk;
030100110531             exsr Scrivi;
030200110530           ENDIF;
030300110530           reade (D13prv) TNCPO04L;
030400110530         ENDDO;
030500110530
030600110530       ENDSR;
030700110530
030800110530       //--------------------------------------------------------------
030900110530       //?Leggo per Categoria Merceologica.
031000110530       //--------------------------------------------------------------
031100110530       BEGSR ReadCpo3;
031200110530
031300110530         open TNCPO03L;
031400110530
031500110530         setll (D13sct) TNCPO03L;
031600110530         reade (D13sct) TNCPO03L;
031700110530         DOW  not %eof(TNCPO03L);
031800110531         //?Controllo se il rcd letto è OK
031900110531           exsr CtrRcd;
032000110530           IF  $RcdOk;
032100110531             exsr Scrivi;
032200110530           ENDIF;
032300110530           reade (D13sct) TNCPO03L;
032400110530         ENDDO;
032500110530
032600110530       ENDSR;
032700110530
032800110530       //--------------------------------------------------------------
032900110530       //?Leggo con SQL.
033000110530       //--------------------------------------------------------------
033100110530       BEGSR ReadSql;
033200110530
033300110530       //?Preparo SQL
033400110530         exsr PrepSql;
033500110530
033600110530       //?Dichiarazione cursore
033700110530         exec sql
033800110530           prepare S1   from :StringaSql;
033900110530         exec sql
034000110530           declare CPO  cursor for S1;
034100110530
034200110530       //?Apertura del cursore
034300110530         exec sql
034400110530           open CPO;
034500110530
034600110530         IF  sqlcode < 0;
034700110530           $End = *on;
034800110530         ENDIF;
034900110530
035000110530         DOW  not $End;
035100110530           exec sql
035200110530             fetch next from CPO into :TNCPOds;
035300110530           IF  sqlcod = 100 or sqlcod < 0;
035400110530             $End = *on;
035500110530             leave;
035600110530           ENDIF;
035700110530
035800110530         //?Controllo se il rcd letto è OK
035900110531           exsr CtrRcd;
036000110530           IF  $RcdOk;
036100110531             exsr Scrivi;
036200110530           ENDIF;
036300110530
036400110530         ENDDO;
036500110530
036600110530         exec sql
036700110530           close CPO;
036800110530
036900110530       ENDSR;
037000110530
037100110530       //--------------------------------------------------------------
037200110530       //?Controllo se RECORD OK.
037300110530       //--------------------------------------------------------------
037400110531       BEGSR CtrRcd;
037500110530
037600110531         $EndAtc = *off;
037700110531         $EndVis = *off;
037800110531         $RcdOk  = *off;
037900110530
038000110530         dCPO01 = CPOrst;
038100140925         dCPO02 = CPOtel;
038200110530
038300110530       //?Filiale potenziale OK su Organigramma
038400110530         chain CPOflt AZORG01L;
038500110530         IF  not %found(AZORG01L);
038600110530           leavesr;
038700110530         ENDIF;
038800110530         IF  ORGfva <> *blanks;
038900110530           leavesr;
039000110530         ENDIF;
039100110530
039200110530       //?Controllo se stessa provincia
039300110530         IF  D13prv <> *blanks and CPOprv <> D13prv;
039400110530           leavesr;
039500110530         ENDIF;
039600110530
039700110530       //?Controllo se stessa località
039800110530         IF  D13loc <> *blanks;
039900110530           wPos = %scan('  ':D13loc);
040000110530           wPos -= 1;
040100120606           IF  %subst(D13loc:1:wPos) <> %subst(CPOcit:1:wPos);
040200110530             leavesr;
040300110530           ENDIF;
040400110530         ENDIF;
040500110530
040600110530       //?Controllo se stesso cap
040700110530         IF  D13cap <> *blanks and (CPOcap <> D13cap or
040800110530                                    CPOnaz <> D13naz);
040900110530           leavesr;
041000110530         ENDIF;
041100110530
041200110530       //?Controllo se stessa nazione
041300110530         IF  D13naz <> *blanks and CPOnaz <> D13naz;
041400110530           leavesr;
041500110530         ENDIF;
041600110530       //?Controllo se stessa area
041700110531         IF  D13car > *zeros and ORGcar <> D13car;
041800110530           leavesr;
041900110530         ENDIF;
042000110530
042100110530       //?Controllo se stessa filiale
042200110530         IF  D13fil <> 0 and CPOflt <> D13fil;
042300110530           leavesr;
042400110530         ENDIF;
042500110530
042600110530       //?Controllo se stessa categoria merceologica
042700110530         IF  D13sct <> 0 and CPOsct <> D13sct;
042800110530           leavesr;
042900110530         ENDIF;
043000110530
043100110530       //?Controllo se stesso codice importanza
043200110530         IF  D13cic <> *blanks;
043300110531           IF  CPOftr = *blanks;
043400110530             leavesr;
043500110530           ENDIF;
043600110531           IF  %lookup(CPOftr:$cic) = 0;
043700110530             leavesr;
043800110530           ENDIF;
043900110530         ENDIF;
044000110530
044100110530       //?Controllo se stessa categoria potenziale
044200110530         IF  D13fls <> *blanks;
044300110530           IF  CPOfls = *blanks;
044400110530             leavesr;
044500110530           ENDIF;
044600110531           IF  %lookup(CPOfls:$fls) = 0;
044700110530             leavesr;
044800110530           ENDIF;
044900110530         ENDIF;
045000110530
045100110530       //?Controllo se stessa area utente
045200110530         IF  %lookup(%editc(CPOflt:'X'):POG) = 0;
045300110530           leavesr;
045400110530         ENDIF;
045500110530
045600110530       //?Contattati
045700110530         IF  D13pst <> *blanks;
045800120914           IF  (D13pst = 'N' and §CPOdtpcon <> *blanks and
045900120914                                 §CPOdtpcon <> *zeros) or
046000120914               (D13pst = 'S' and (§CPOdtpcon = *blanks or
046100120914                                  §CPOdtpcon = *zeros));
046200120914             leavesr;
046300110530           ENDIF;
046400110530         ENDIF;
046500110530
046600110530       //?Controllo se sede/filiale
046700110530         IF  D13fsf <> *blanks and CPOfsf <> D13fsf;
046800110530           leavesr;
046900110530         ENDIF;
047000140925
047100140925       //?% Fatturato Export
047200140930         IF  D13pexpa > *zeros and
047300140925            (CPOpexp < D13pexpda or CPOpexp > D13pexpa);
047400140925           leavesr;
047500140925         ENDIF;
047600140925
047700140925       //?Anno inizio attività
047800140930         IF  D13annoa > *zeros and
047900140925            (§CPOannoia < %editc(D13annoda:'X') or
048000140925             §CPOannoia > %editc(D13annoa:'X'));
048100140925           leavesr;
048200140925         ENDIF;
048300110530
048400110530       //?Controllo trattative in corso
048500110608         wTtr = 0;
048600110608         setll (CPOcpo) TIVIS01L;
048700110608         DOW  not $EndVis;
048800110608           reade (CPOcpo) TIVIS01L;
048900110608           IF  %eof(TIVIS01L);
049000110608             $EndVis = *on;
049100110608             leave;
049200110608           ENDIF;
049300110608           IF  VISffz = *blanks and VISdch = 0;
049400110608             wTtr += 1;
049500110608             $EndVis = *on;
049600110608           ENDIF;
049700110608         ENDDO;
049800110608       //?Controllo trattative in corso
049900110608         IF  D13ttr <> *blanks;
050000110608           IF (D13ttr = 'S' and wTtr = 0) or
050100110608              (D13ttr = 'N' and wTtr > 0);
050200110608             leavesr;
050300110608           ENDIF;
050400110608         ENDIF;
050500110530
050600110530       //?Controllo prossima attività
050700110530         IF  D13cmm > *zeros or D13cco <> *blanks or
050800110530             D13dadda > *zeros;
050900110608           clear wDate;
051000110608           clear wAtt;
051100110608           setll (CPOcpo:wDate) TIATC05L;
051200110531           DOW  not $EndAtc;
051300110608             reade (CPOcpo:wDate) TIATC05L;
051400110531             IF  %eof(TIATC05L);
051500110531               $EndAtc = *on;
051600110531               leave;
051700110531             ENDIF;
051800110608             IF  D13cmm > 0 and %lookup(ATCcmm:$Cmm) = 0;
051900110608               iter;
052000110608             ENDIF;
052100110608             IF  D13cco <> *blanks and %lookup(ATCcad:$cco) = 0;
052200110608               iter;
052300110608             ENDIF;
052400110608             IF  D13dadda <> *zeros and ATCdad < D13dadda;
052500110608               iter;
052600110608             ENDIF;
052700110608             IF  D13dada <> *zeros and ATCdad > D13dada;
052800110530               iter;
052900110530             ENDIF;
053000110530             wAtt += 1;
053100110608             wCmm = ATCcmm;
053200110531             $EndAtc = *on;
053300110530           ENDDO;
053400110608           IF  wAtt = 0;
053500110530             leavesr;
053600110530           ENDIF;
053700110530         ENDIF;
053800110530
053900110530       //?Se arrivo qua il rcd è OK
054000110530         $RcdOk = *on;
054100110530
054200110530       //?Cerco Commerciale Unificante potenziale
054300110530         IF  D13cmmu = 0;
054400130916           chain  (CPOcmm)  AZCMM000;
054500130916           IF  Not %found(AZCMM01L);
054600130916             clear  CMMuni;
054700110530           ENDIF;
054800110530         ENDIF;
054900110530
055000110530       ENDSR;
055100110530
055200110530       //--------------------------------------------------------------
055300110530       //?Scrivo file di WORK.
055400110530       //--------------------------------------------------------------
055500110530       BEGSR Scrivi;
055600110530
055700110530         clear WFCPO000;
055800110530
055900110530         WFCdis = ORGfl3;
056000110530         WFCare = ORGcar;
056100110608
056200110530         IF  D13cmmu = 0;
056300130916           WFCcmmu = CMMuni;
056400110530         ELSE;
056500110531           WFCcmmu = D13cmmu;
056600110530         ENDIF;
056700110608
056800110608         IF  D13cmm = 0;
056900130916           chain  (wCmm)  AZCMM000;
057000130916           IF  %found(AZCMM01L);
057100130916             WFCcmm = CMMuni;
057200110608           ENDIF;
057300110608         ELSE;
057400110608            WFCcmm = D13cmm;
057500110530         ENDIF;
057600110608
057700110530         WFCcpo = CPOcpo;
057800110530         WFCrag = CPOrag;
057900110530         WFCspt = §CPOsptp;
058000110531         IF  §CPOifodul <> *blanks;
058100110531           WFCdtifo = %dec(§CPOifodul:8:0);
058200110531         ENDIF;
058300110530       //?Se utente non abilitato alle INFO pulisco la spesa trasporti
058400110531         IF  $InfoAbi = 'N';
058500110530           clear WFCspt;
058600110530         ENDIF;
058700110607         WFCflt = CPOflt;
058800110530
058900110530         write WFCPO000;
059000110530
059100110530       ENDSR;
059200110530
059300110530       //--------------------------------------------------------------
059400110530       //?Preparazione stringa SQL.
059500110530       //--------------------------------------------------------------
059600110530       BEGSR PrepSQL;
059700110530
059800110530         StringaSql = 'select * from TNCPO00F where CPOflt ';
059900110530
060000110530         SELECT;
060100110530           WHEN  D13car > 0;
060200110530             StringaSql += 'in (';
060300110530             yy = 0;
060400110530             xx = 1;
060500110530             FOR xx by 1 to %elem(POG);
060600110530               IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
060700110530                 IF  yy > 0;
060800110530                   StringaSql += ', ';
060900110530                 ELSE;
061000110530                   yy = 1;
061100110530                 ENDIF;
061200110530                 StringaSql += '''';
061300110530                 StringaSql += POG(xx);
061400110530                 StringaSql += '''';
061500110530               ENDIF;
061600110530             ENDFOR;
061700110607             StringaSql += ')';
061800110530
061900110530           WHEN  D13fil > 0;
062000110530             StringaSql += '= ' + %editc(D13fil:'X');
062100110530         ENDSL;
062200110530
062300110530         StringaSql += ' order by CPOcpo';
062400110530
062500110531       ENDSR;
062600110530
062700110526       //--------------------------------------------------------------
062800110526       //?Operazioni finali.
062900110526       //--------------------------------------------------------------
063000110526       BEGSR RoutEnd;
063100110526
063200110526         *inLR = *on;
063300110526         return;
063400110526
063500110526       ENDSR;
063600110526
063700110526      /end-free
