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
007700110526     d TRMK13ds      e ds                  inz
007800110531     d  $cic                  68     72    dim(5)
007900110531     d  $fls                  73     77    dim(5)
008000110531     d  $cco                  88     97    dim(5)
008100110531
008200110531      // - Ds x campo CPORST
008300110531     d dCPO01        e ds                  inz
008400110530
008500110530      // - File TNCPO
008600110530     d TNCPOds       e ds                  extname(TNCPO00F)
008700110526
008800110526      //---------------------------------------------------------------
008900110526      //?Definizione variabili globali.
009000110526      //---------------------------------------------------------------
009100110526
009200110526      // - Indici di schiera
009300110526     d xx              s              4  0 inz
009400110526     d yy              s              4  0 inz
009500110526     d zz              s              4  0 inz
009600110526
009700110526       // - Stringa SQL da eseguire
009800110526     d StringaSql      s           2048    Varying        inz
009900110526
010000110526       // - Flags booleani
010100110526     d $End            s               n   inz(*off)
010200110526     d $EndAge         s               n   inz(*off)
010300110531     d $EndAtc         s               n   inz(*off)
010400110531     d $EndVis         s               n   inz(*off)
010500110531     d $InfoAbi        s               n
010600110526     d $RcdOk          s               n   inz(*off)
010700110526
010800110526       // - Campi per richiamo QCMDEXC
010900110526     d Qcmd            s            128    inz
011000110526
011100110526       // - Campi di comodo
011200110526     d wAge            s              7  0 inz
011300110530     d wAtt            s              3  0 inz
011400110608     d wCmm            s              7  0 inz
011500110608     d wDate           s              8  0 inz
011600110530     d wPos            s              3  0 inz
011700110530     d wTtr            s              3  0 inz
011800110526
011900110526      //---------------------------------------------------------------
012000110526      //?Definizione procedure usate.
012100110526      //---------------------------------------------------------------
012200110526
012300110526
012400110526      //---------------------------------------------------------------
012500110526      //?Prototipi.
012600110526      //---------------------------------------------------------------
012700110526
012800110526      /copy gaitrasrc/srcprotopr,QCMDEXC
012900110526
013000110526      //---------------------------------------------------------------
013100110526      //?Definizione key-list.
013200110526      //---------------------------------------------------------------
013300110526
013400110526
013500110526      //---------------------------------------------------------------
013600110526      //?M A I N - L I N E
013700110526      //---------------------------------------------------------------
013800110526
013900110526     c     *Entry        plist
014000110526     c                   parm                    KPJBA
014100110526     c                   parm                    pog
014200110526     c                   parm                    $InfoAbi
014300110530
014400110530      /free
014500110526
014600110526       //?Operazioni iniziali
014700110526       exsr RoutInz;
014800110526
014900110530       //?Elaborazione
015000110530       exsr Elabora;
015100110526
015200110526       //?Operazioni finali
015300110526       exsr RoutEnd;
015400110526
015500110526       //--------------------------------------------------------------
015600110526       //?Operazioni iniziali.
015700110526       //--------------------------------------------------------------
015800110526       BEGSR RoutInz;
015900110526
016000110526       // -?Duplico il file in qtemp?
016100110526         Qcmd = 'CRTDUPOBJ OBJ(WFCPO00F) FROMLIB(';
016200110526         IF  %subst(knsif : 7 : 1) = 'P';
016300110720           Qcmd = %trimr(Qcmd) + 'GAITRAAZP)';
016400110526         ELSE;
016500110720           Qcmd = %trimr(Qcmd) + 'GAITRAAZM)';
016600110526         ENDIF;
016700110607         Qcmd = %trimr(Qcmd) + ' OBJTYPE(*FILE) TOLIB(QTEMP)';
016800110526         ExecuteCommand (Qcmd : %size(Qcmd));
016900110530
017000110530         open WFCPO00F;
017100110607
017200110607         trmk13ds = kpjbu;
017300110526
017400110531       //?Se richiesto Commerciale Unificante oppure
017500110531       //?Commerciale Unificante prossima attività
017600110531       //?carico la sk con i figli
017700110530         IF  D13cmmu <> 0;
017800110530           wAge = D13cmmu;
017900110530           exsr CarAge;
018000110530           $Cmmu = $Age;
018100110530         ENDIF;
018200110530         IF  D13cmm <> 0;
018300110530           wAge = D13cmm;
018400110526           exsr CarAge;
018500110530           $Cmm = $Age;
018600110526         ENDIF;
018700110526
018800110526       ENDSR;
018900110526
019000110526       //--------------------------------------------------------------
019100110526       //?Carico Sk Agenti.
019200110526       //--------------------------------------------------------------
019300110526       BEGSR CarAGE;
019400110526
019500110526         clear xx;
019600110526         clear $Age;
019700110526         $EndAge = *off;
019800110526
019900110526         exec sql
020000110526          declare AGE cursor for
020100130916          select CMMcod, CMMuni
020200130916          from AZCMM00F
020300130916          order by CMMcod;
020400110526
020500110526         exec sql
020600110526           open AGE;
020700110526           IF sqlcode < 0;
020800110526             $EndAge = *on;
020900110526           ENDIF;
021000110526
021100110526         DOW not $EndAge;
021200110526           exec sql
021300130916             fetch next from AGE into :CMMcod, :CMMuni;
021400110526             IF sqlcod = 100 or sqlcod < 0;
021500110526               $EndAge = *on;
021600110526               leave;
021700110526             ENDIF;
021800130916             IF  CMMuni = wAge;
021900110526               xx += 1;
022000130916               $Age(xx) = CMMcod;
022100110526             ENDIF;
022200110526         ENDDO;
022300110526
022400110526         exec sql close AGE;
022500110526
022600110531       //?Se non ho cricato niente memorizzo almeno il codice richiesto
022700110526         IF $Age(01) = 0;
022800110526           $Age(01) = wAge;
022900110526         ENDIF;
023000110526
023100110526       ENDSR;
023200110530
023300110530       //--------------------------------------------------------------
023400110530       //?Elaborazione.
023500110530       //--------------------------------------------------------------
023600110530       BEGSR Elabora;
023700110530
023800110530         SELECT;
023900110531       //?Ciclo per sk agenti (figli Unificante richiesto)
024000110531       //?e leggo per commerciale
024100110530           WHEN  D13cmmu > 0;
024200110530             sorta $Cmmu;
024300110530             exsr ReadCpo8;
024400110531       //?Leggo per provincia
024500110530           WHEN  D13prv > *blanks;
024600110530             exsr ReadCpo4;
024700110531       //?Leggo per categoria mercologica
024800110531           WHEN  D13sct > *zeros;
024900110530             exsr ReadCpo3;
025000110530           OTHER;
025100110531       //?leggo con SQL
025200110530             exsr ReadSql;
025300110530         ENDSL;
025400110530
025500110530       ENDSR;
025600110530
025700110530       //--------------------------------------------------------------
025800110530       //?Ciclo per sk Agenti.
025900110530       //--------------------------------------------------------------
026000110530       BEGSR ReadCpo8;
026100110530
026200110530         open TNCPO08L;
026300110526
026400110530         zz = 1;
026500110530         FOR zz by 1 to %elem($CmmU);
026600110530           IF  $CmmU(zz) <> *zeros;
026700110530
026800110530             setll ($Cmmu(zz)) TNCPO08L;
026900110530             reade ($Cmmu(zz)) TNCPO08L;
027000110530             DOW  not %eof(TNCPO08L);
027100110530           //?Controllo se il rcd letto è OK
027200110531               exsr CtrRcd;
027300110530               IF  $RcdOk;
027400110531                 exsr Scrivi;
027500110530               ENDIF;
027600110530               reade ($Cmmu(zz)) TNCPO08L;
027700110530             ENDDO;
027800110530           ENDIF;
027900110530
028000110530         ENDFOR;
028100110530
028200110530       ENDSR;
028300110530
028400110530       //--------------------------------------------------------------
028500110530       //?Leggo per Provincia.
028600110530       //--------------------------------------------------------------
028700110530       BEGSR ReadCpo4;
028800110530
028900110530         open TNCPO04L;
029000110530
029100110530         setll (D13prv) TNCPO04L;
029200110530         reade (D13prv) TNCPO04L;
029300110530         DOW  not %eof(TNCPO04L);
029400110531         //?Controllo se il rcd letto è OK
029500110531           exsr CtrRcd;
029600110530           IF  $RcdOk;
029700110531             exsr Scrivi;
029800110530           ENDIF;
029900110530           reade (D13prv) TNCPO04L;
030000110530         ENDDO;
030100110530
030200110530       ENDSR;
030300110530
030400110530       //--------------------------------------------------------------
030500110530       //?Leggo per Categoria Merceologica.
030600110530       //--------------------------------------------------------------
030700110530       BEGSR ReadCpo3;
030800110530
030900110530         open TNCPO03L;
031000110530
031100110530         setll (D13sct) TNCPO03L;
031200110530         reade (D13sct) TNCPO03L;
031300110530         DOW  not %eof(TNCPO03L);
031400110531         //?Controllo se il rcd letto è OK
031500110531           exsr CtrRcd;
031600110530           IF  $RcdOk;
031700110531             exsr Scrivi;
031800110530           ENDIF;
031900110530           reade (D13sct) TNCPO03L;
032000110530         ENDDO;
032100110530
032200110530       ENDSR;
032300110530
032400110530       //--------------------------------------------------------------
032500110530       //?Leggo con SQL.
032600110530       //--------------------------------------------------------------
032700110530       BEGSR ReadSql;
032800110530
032900110530       //?Preparo SQL
033000110530         exsr PrepSql;
033100110530
033200110530       //?Dichiarazione cursore
033300110530         exec sql
033400110530           prepare S1   from :StringaSql;
033500110530         exec sql
033600110530           declare CPO  cursor for S1;
033700110530
033800110530       //?Apertura del cursore
033900110530         exec sql
034000110530           open CPO;
034100110530
034200110530         IF  sqlcode < 0;
034300110530           $End = *on;
034400110530         ENDIF;
034500110530
034600110530         DOW  not $End;
034700110530           exec sql
034800110530             fetch next from CPO into :TNCPOds;
034900110530           IF  sqlcod = 100 or sqlcod < 0;
035000110530             $End = *on;
035100110530             leave;
035200110530           ENDIF;
035300110530
035400110530         //?Controllo se il rcd letto è OK
035500110531           exsr CtrRcd;
035600110530           IF  $RcdOk;
035700110531             exsr Scrivi;
035800110530           ENDIF;
035900110530
036000110530         ENDDO;
036100110530
036200110530         exec sql
036300110530           close CPO;
036400110530
036500110530       ENDSR;
036600110530
036700110530       //--------------------------------------------------------------
036800110530       //?Controllo se RECORD OK.
036900110530       //--------------------------------------------------------------
037000110531       BEGSR CtrRcd;
037100110530
037200110531         $EndAtc = *off;
037300110531         $EndVis = *off;
037400110531         $RcdOk  = *off;
037500110530
037600110530         dCPO01 = CPOrst;
037700110530
037800110530       //?Filiale potenziale OK su Organigramma
037900110530         chain CPOflt AZORG01L;
038000110530         IF  not %found(AZORG01L);
038100110530           leavesr;
038200110530         ENDIF;
038300110530         IF  ORGfva <> *blanks;
038400110530           leavesr;
038500110530         ENDIF;
038600110530
038700110530       //?Controllo se stessa provincia
038800110530         IF  D13prv <> *blanks and CPOprv <> D13prv;
038900110530           leavesr;
039000110530         ENDIF;
039100110530
039200110530       //?Controllo se stessa località
039300110530         IF  D13loc <> *blanks;
039400110530           wPos = %scan('  ':D13loc);
039500110530           wPos -= 1;
039600120606           IF  %subst(D13loc:1:wPos) <> %subst(CPOcit:1:wPos);
039700110530             leavesr;
039800110530           ENDIF;
039900110530         ENDIF;
040000110530
040100110530       //?Controllo se stesso cap
040200110530         IF  D13cap <> *blanks and (CPOcap <> D13cap or
040300110530                                    CPOnaz <> D13naz);
040400110530           leavesr;
040500110530         ENDIF;
040600110530
040700110530       //?Controllo se stessa nazione
040800110530         IF  D13naz <> *blanks and CPOnaz <> D13naz;
040900110530           leavesr;
041000110530         ENDIF;
041100110530       //?Controllo se stessa area
041200110531         IF  D13car > *zeros and ORGcar <> D13car;
041300110530           leavesr;
041400110530         ENDIF;
041500110530
041600110530       //?Controllo se stessa filiale
041700110530         IF  D13fil <> 0 and CPOflt <> D13fil;
041800110530           leavesr;
041900110530         ENDIF;
042000110530
042100110530       //?Controllo se stessa categoria merceologica
042200110530         IF  D13sct <> 0 and CPOsct <> D13sct;
042300110530           leavesr;
042400110530         ENDIF;
042500110530
042600110530       //?Controllo se stesso codice importanza
042700110530         IF  D13cic <> *blanks;
042800110531           IF  CPOftr = *blanks;
042900110530             leavesr;
043000110530           ENDIF;
043100110531           IF  %lookup(CPOftr:$cic) = 0;
043200110530             leavesr;
043300110530           ENDIF;
043400110530         ENDIF;
043500110530
043600110530       //?Controllo se stessa categoria potenziale
043700110530         IF  D13fls <> *blanks;
043800110530           IF  CPOfls = *blanks;
043900110530             leavesr;
044000110530           ENDIF;
044100110531           IF  %lookup(CPOfls:$fls) = 0;
044200110530             leavesr;
044300110530           ENDIF;
044400110530         ENDIF;
044500110530
044600110530       //?Controllo se stessa area utente
044700110530         IF  %lookup(%editc(CPOflt:'X'):POG) = 0;
044800110530           leavesr;
044900110530         ENDIF;
045000110530
045100110530       //?Contattati
045200110530         IF  D13pst <> *blanks;
045300120914           IF  (D13pst = 'N' and §CPOdtpcon <> *blanks and
045400120914                                 §CPOdtpcon <> *zeros) or
045500120914               (D13pst = 'S' and (§CPOdtpcon = *blanks or
045600120914                                  §CPOdtpcon = *zeros));
045700120914             leavesr;
045800110530           ENDIF;
045900110530         ENDIF;
046000110530
046100110530       //?Controllo se sede/filiale
046200110530         IF  D13fsf <> *blanks and CPOfsf <> D13fsf;
046300110530           leavesr;
046400110530         ENDIF;
046500110530
046600110530       //?Controllo trattative in corso
046700110608         wTtr = 0;
046800110608         setll (CPOcpo) TIVIS01L;
046900110608         DOW  not $EndVis;
047000110608           reade (CPOcpo) TIVIS01L;
047100110608           IF  %eof(TIVIS01L);
047200110608             $EndVis = *on;
047300110608             leave;
047400110608           ENDIF;
047500110608           IF  VISffz = *blanks and VISdch = 0;
047600110608             wTtr += 1;
047700110608             $EndVis = *on;
047800110608           ENDIF;
047900110608         ENDDO;
048000110608       //?Controllo trattative in corso
048100110608         IF  D13ttr <> *blanks;
048200110608           IF (D13ttr = 'S' and wTtr = 0) or
048300110608              (D13ttr = 'N' and wTtr > 0);
048400110608             leavesr;
048500110608           ENDIF;
048600110608         ENDIF;
048700110530
048800110530       //?Controllo prossima attività
048900110530         IF  D13cmm > *zeros or D13cco <> *blanks or
049000110530             D13dadda > *zeros;
049100110608           clear wDate;
049200110608           clear wAtt;
049300110608           setll (CPOcpo:wDate) TIATC05L;
049400110531           DOW  not $EndAtc;
049500110608             reade (CPOcpo:wDate) TIATC05L;
049600110531             IF  %eof(TIATC05L);
049700110531               $EndAtc = *on;
049800110531               leave;
049900110531             ENDIF;
050000110608             IF  D13cmm > 0 and %lookup(ATCcmm:$Cmm) = 0;
050100110608               iter;
050200110608             ENDIF;
050300110608             IF  D13cco <> *blanks and %lookup(ATCcad:$cco) = 0;
050400110608               iter;
050500110608             ENDIF;
050600110608             IF  D13dadda <> *zeros and ATCdad < D13dadda;
050700110608               iter;
050800110608             ENDIF;
050900110608             IF  D13dada <> *zeros and ATCdad > D13dada;
051000110530               iter;
051100110530             ENDIF;
051200110530             wAtt += 1;
051300110608             wCmm = ATCcmm;
051400110531             $EndAtc = *on;
051500110530           ENDDO;
051600110608           IF  wAtt = 0;
051700110530             leavesr;
051800110530           ENDIF;
051900110530         ENDIF;
052000110530
052100110530       //?Se arrivo qua il rcd è OK
052200110530         $RcdOk = *on;
052300110530
052400110530       //?Cerco Commerciale Unificante potenziale
052500110530         IF  D13cmmu = 0;
052600130916           chain  (CPOcmm)  AZCMM000;
052700130916           IF  Not %found(AZCMM01L);
052800130916             clear  CMMuni;
052900110530           ENDIF;
053000110530         ENDIF;
053100110530
053200110530       ENDSR;
053300110530
053400110530       //--------------------------------------------------------------
053500110530       //?Scrivo file di WORK.
053600110530       //--------------------------------------------------------------
053700110530       BEGSR Scrivi;
053800110530
053900110530         clear WFCPO000;
054000110530
054100110530         WFCdis = ORGfl3;
054200110530         WFCare = ORGcar;
054300110608
054400110530         IF  D13cmmu = 0;
054500130916           WFCcmmu = CMMuni;
054600110530         ELSE;
054700110531           WFCcmmu = D13cmmu;
054800110530         ENDIF;
054900110608
055000110608         IF  D13cmm = 0;
055100130916           chain  (wCmm)  AZCMM000;
055200130916           IF  %found(AZCMM01L);
055300130916             WFCcmm = CMMuni;
055400110608           ENDIF;
055500110608         ELSE;
055600110608            WFCcmm = D13cmm;
055700110530         ENDIF;
055800110608
055900110530         WFCcpo = CPOcpo;
056000110530         WFCrag = CPOrag;
056100110530         WFCspt = §CPOsptp;
056200110531         IF  §CPOifodul <> *blanks;
056300110531           WFCdtifo = %dec(§CPOifodul:8:0);
056400110531         ENDIF;
056500110530       //?Se utente non abilitato alle INFO pulisco la spesa trasporti
056600110531         IF  $InfoAbi = 'N';
056700110530           clear WFCspt;
056800110530         ENDIF;
056900110607         WFCflt = CPOflt;
057000110530
057100110530         write WFCPO000;
057200110530
057300110530       ENDSR;
057400110530
057500110530       //--------------------------------------------------------------
057600110530       //?Preparazione stringa SQL.
057700110530       //--------------------------------------------------------------
057800110530       BEGSR PrepSQL;
057900110530
058000110530         StringaSql = 'select * from TNCPO00F where CPOflt ';
058100110530
058200110530         SELECT;
058300110530           WHEN  D13car > 0;
058400110530             StringaSql += 'in (';
058500110530             yy = 0;
058600110530             xx = 1;
058700110530             FOR xx by 1 to %elem(POG);
058800110530               IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
058900110530                 IF  yy > 0;
059000110530                   StringaSql += ', ';
059100110530                 ELSE;
059200110530                   yy = 1;
059300110530                 ENDIF;
059400110530                 StringaSql += '''';
059500110530                 StringaSql += POG(xx);
059600110530                 StringaSql += '''';
059700110530               ENDIF;
059800110530             ENDFOR;
059900110607             StringaSql += ')';
060000110530
060100110530           WHEN  D13fil > 0;
060200110530             StringaSql += '= ' + %editc(D13fil:'X');
060300110530         ENDSL;
060400110530
060500110530         StringaSql += ' order by CPOcpo';
060600110530
060700110531       ENDSR;
060800110530
060900110526       //--------------------------------------------------------------
061000110526       //?Operazioni finali.
061100110526       //--------------------------------------------------------------
061200110526       BEGSR RoutEnd;
061300110526
061400110526         *inLR = *on;
061500110526         return;
061600110526
061700110526       ENDSR;
061800110526
061900110526      /end-free
