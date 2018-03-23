000100100413      //---------------------------------------------------------------
000200110513      //?TRMK36R1 - Crea attività 73/74 su potenziali persi    SPOT
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000110513      // - File Organigramma
001100110513     fAZORG01L  if   e           k disk
001200110202      // - File Anagrafica clienti
001300110202     fCNACO16L  if   e           k disk
001400110202     fCNCLP00F  if   e           k disk
001500110201      // - File Variazioni anagrafiche
001600110201     fCNACVT1L  if   e           k disk
001700110323      // - File Tabelle
001800110323     fTABEL00F  if   e           k disk
001900110323      // - File Attività
002000110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
002100110428      // - File Trattative
002200110502     fTIVIS01L  if   e           k disk    usropn
002300110502     f                                     extfile(wFLib)
002400110516      // - File di controllo
002500110516     fwfcad73t  o    e             disk    rename(wfcad73t:wfcad7300)
002600100413
002700100413      //---------------------------------------------------------------
002800100413      //?Definizione costanti.
002900100413      //---------------------------------------------------------------
003000100413
003100100413      //---------------------------------------------------------------
003200100413      //?Definizione schiere.
003300100413      //---------------------------------------------------------------
003400110323
003500110323      // - Causali che generano attività
003600110323     d $BIatc          s              3    dim(20)
003700110323
003800110323      // - Causali + cod. attività
003900110323     d $BIatccau       s              5    dim(20)
004000110323
004100110323      // - Causali che non generano attività
004200110323     d $BInoatc        s              3    dim(20)
004300110513
004400110516      // - Sk area x attività 73 con 3 tranche + ultima tranche fatta
004500110516     d sk73            s              3  0 dim(9999)
004600110516     d sk73u           s              3  0 dim(9999)
004700110513
004800110516      // - Sk area x attività 74 con 6 tranche + ultima tranche fatta
004900110516     d sk74            s              3  0 dim(9999)
005000110516     d sk74u           s              3  0 dim(9999)
005100110513
005200090715
005300100413      //---------------------------------------------------------------
005400100413      //?Definizione aree dati.
005500100413      //---------------------------------------------------------------
005600100413
005700100413      //---------------------------------------------------------------
005800100413      //?Definizione strutture dati.
005900100413      //---------------------------------------------------------------
006000100415
006100110131      // - Parametri ricevuti
006200100415     d KPJBA         e ds
006300110211
006400110211      // ds x ATCflo
006500110211     d dATC01        e ds
006600100726
006700100726      // File potenziali
006800100726     d TNCPOds       e ds                  extname(TNCPO00F)
006900110131
007000110131      // - Aggiunge/Toglie gg/mm dalla data
007100110131     d XGIOLAVds     e ds                  inz
007200110323
007300110323      // - Tabella BI - Blocco Causali Clienti
007400110323     d dsBI          e ds                  inz
007500100415
007600100413      //---------------------------------------------------------------
007700100413      //?Definizione variabili globali.
007800100413      //---------------------------------------------------------------
007900100413
008000100413      // - Flags booleani
008100100413     d $End            s               n   inz(*off)
008200110201     d $SiAcv          s               n   inz(*off)
008300110323
008400110323      // - Indici di schiera
008500110323     d xx              s              3  0
008600110323     d yy              s              3  0
008700110513     d zz              s              4  0
008800110131
008900110131      // - Campi di comodo
009000110513     d Conta           s              2  0 inz
009100110201     d NrAtt           s              5i 0
009200110201     d NrVis           s              5i 0
009300110201     d wCad            s                   like(ATCcad)
009400110201     d wCauBlc         s                   like(CLPnar)
009500110131     d wDad            s                   like(ATCdad)
009600110513     d wDataOggi       s              8  0
009700110513     d wDataISO1       s               d   datfmt(*iso)
009800110513     d wDataISO2       s               d   datfmt(*iso)
009900110201     d wDatBlc         s                   like(ACVdav)
010000110201     d wOraBlc         s                   like(ACVorv)
010100110516     d wgg             s             10  0
010200110218     d wgg73           s                   like(IXGLgg) inz(90)
010300110218     d wgg74           s                   like(IXGLgg) inz(365)
010400110502
010500110502       // - Nome libreria del file TIVIS
010600110502     d wFLib           s             21    inz
010700100413
010800100413      //---------------------------------------------------------------
010900100413      //?Definizione procedure esterne.
011000100413      //---------------------------------------------------------------
011100110131
011200110131      // - Aggiunge/Toglie gg/mm dalla data
011300110131     d Xgiolav         pr                  extpgm('XGIOLAV')
011400110131     d  xgiolavds                          likeds(xgiolavds)
011500100413
011600100413      //---------------------------------------------------------------
011700110131      //?Prototipi
011800100413      //---------------------------------------------------------------
011900100414
012000100413
012100100413      //---------------------------------------------------------------
012200100413      //?Definizione key-list.
012300100413      //---------------------------------------------------------------
012400110201
012500110201      // - Dettaglio variazioi anagrafiche
012600110201     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
012700110201     d                                     prefix(k_)
012800110201     d                                     inz
012900100413
013000100413      //---------------------------------------------------------------
013100100413      //?Riepilogo indicatori.
013200100413      //---------------------------------------------------------------
013300100413
013400100413      //---------------------------------------------------------------
013500100413      //?M A I N - L I N E
013600100413      //---------------------------------------------------------------
013700100413
013800100413     c     *Entry        plist
013900110131     c                   parm                    kpjba
014000100413
014100100413      /free
014200100413
014300100413       //?Operazioni iniziali
014400100513       exsr RoutInz;
014500100413
014600100726       //?Elaboro
014700100726       exsr Elabora;
014800100413
014900100413       //?Operazioni finali
015000100513       exsr RoutEnd;
015100100413
015200100413       //--------------------------------------------------------------
015300100413       //?Operazioni iniziali.
015400100413       //--------------------------------------------------------------
015500100513       BEGSR RoutInz;
015600100726
015700100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015800110323
015900110323         clear xx;
016000110323         clear yy;
016100110323
016200110323       //?Carico tabelle causali
016300110323         TBLkut = 1;
016400110323         TBLcod = 'BI';
016500110323         setll (TBLkut:TBLcod) TABEL00F;
016600110323         reade (TBLkut:TBLcod) TABEL00F;
016700110323         DOW  not %eof(TABEL00F);
016800110323           dsBI = TBLuni;
016900110323           IF  §BIatc <> *blanks;
017000110323             xx += 1;
017100110323         //?$BIATC dove memorizzo le causali che generano attività
017200110323         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
017300110323             $BIatc(xx) = %subst(TBLkey:1:3);
017400110323             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
017500110323             %subst($BIatccau(xx):4:2) = §BIatc;
017600110323           ELSE;
017700110323             yy += 1;
017800110323         //?$BINOATC dove memorizzo le causali che NON generano attività
017900110323             $BInoatc(yy) = %subst(TBLkey:1:3);
018000110323           ENDIF;
018100110323           reade (TBLkut:TBLcod) TABEL00F;
018200110323         ENDDO;
018300110502
018400110502         //?Apro il file delle trattative
018500110502         IF  %subst(knsif : 7 : 1) = 'P';
018600110502           wFLib = 'FILTRAGRPF/TIVIS01L';
018700110502         ELSE;
018800110502           wFLib = 'FILTRAGRU/TIVIS01L';
018900110502         ENDIF;
019000110502         open  TIVIS01L;
019100110513
019200110513         //?Data del giorno
019300110513         wDataOggi = %dec(%date());
019400100413
019500100413       ENDSR;
019600100413
019700100413       //--------------------------------------------------------------
019800110131       //?Elabora.
019900100413       //--------------------------------------------------------------
020000100513       BEGSR Elabora;
020100100413
020200100413         $End = *off;
020300100413
020400110201       //?Estraggo solo i potenziali persi
020500100413         exec sql
020600100726         DECLARE CPO cursor for
020700110516         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt, CPOftr
020800110428         FROM TNCPO01L
020900110131         WHERE CPOfls = 'P'
021000100726         ORDER by CPOcpo;
021100100413
021200100726         exec sql OPEN CPO;
021300100413
021400100413         DOW  not $End;
021500110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
021600110516                                             :CPOFLS, :CPOFLT,
021700110516                                             :CPOFTR;
021800100413           IF  sqlcod = 100 or sqlcod < 0;
021900100413             $End = *on;
022000100413             leave;
022100100413           ENDIF;
022200100422
022300110201         //?Controllo se potenziale ha azioni o trattative aperte
022400110201           exsr CtrAtt;
022500110201
022600110513         //?Scrivo nuova attività solo se NON trovo attività da eseguire o trattative aperte
022700110203           IF  NrAtt = 0 and NrVis = 0;
022800110201
022900110203           //?Controllo l'ultimo blocco dei clienti collegati
023000110203             exsr CtrBlc;
023100110315
023200110323           //?Controllo se la causale prevede la creazione dell'attività
023300110323           //?Se non la prevede leggo altro codice
023400110323             IF  %lookup(wCauBlc:$BInoatc) > 0;
023500110323               iter;
023600110323             ENDIF;
023700110201
023800110203           //?Genero nuova attività 73 o 74
023900110513           //?Ma solo se c'è il codice del commerciale
024000110218             IF  CPOcmm > 0;
024100110218               exsr Scrivi;
024200110218             ENDIF;
024300110203           ENDIF;
024400110201
024500100413         ENDDO;
024600100413
024700100726         exec sql CLOSE CPO;
024800100413
024900100413       ENDSR;
025000110201
025100110201       //--------------------------------------------------------------
025200110201       //?Controllo attività/trattative.
025300110201       //--------------------------------------------------------------
025400110201       BEGSR CtrAtt;
025500110201
025600110201         clear NrAtt;
025700110201         clear NrVis;
025800110201
025900110428       //?Controllo se ci sono attività da eseguire
026000110428         chain (CPOcpo:0:0) TIATC05L;
026100110428         IF  %found(TIATC05L);
026200110428           NrAtt = 1;
026300110428           leavesr;
026400110428         ENDIF;
026500110428
026600110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
026700110428         setll (CPOcpo:0) TIVIS01L;
026800110428         reade (CPOcpo:0) TIVIS01L;
026900110428         DOW not %eof(TIVIS01L);
027000110428           IF  VISffz = ' ';
027100110428             NrVis = 1;
027200110428             leavesr;
027300110428           ENDIF;
027400110428           reade (CPOcpo:0) TIVIS01L;
027500110428         ENDDO;
027600110201
027700110201       ENDSR;
027800110201
027900110201       //--------------------------------------------------------------
028000110201       //?Controllo l'ultimo blocco sui clienti.
028100110201       //--------------------------------------------------------------
028200110201       BEGSR CtrBlc;
028300110202
028400110202         clear wCauBlc;
028500110202         clear wDatBlc;
028600110202         clear wOraBlc;
028700110201
028800110201       //?Leggo tutte le anagrafiche legate al codice potenziale
028900110202         setll CPOcpo CNACO16L;
029000110202         reade CPOcpo CNACO16L;
029100110202         DOW  not %eof(CNACO16L);
029200110202           IF  %eof(CNACO16L);
029300110202             leave;
029400110202           ENDIF;
029500110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
029600110202           IF  not %found(CNCLP00F);
029700110202             clear CLPnar;
029800110202           ENDIF;
029900110202
030000110202         //?Cerco la data della variazione
030100110202           $SiAcv = *off;
030200110202           setll (ACOkcc:ACOksc) CNACVT1L;
030300110202           reade (ACOkcc:ACOksc) CNACVT1L;
030400110202           DOW  not %eof(CNACVT1L);
030500110202             IF  %eof(CNACVT1L);
030600110202               leave;
030700110202             ENDIF;
030800110202             IF  ACVcta = 'M' and
030900110202                 %scan('B':ACVflo) > 0;
031000110202               $siAcv = *on;
031100110202               leave;
031200110202             ENDIF;
031300110202             reade (ACOkcc:ACOksc) CNACVT1L;
031400110202           ENDDO;
031500110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
031600110202         //?e memorizzo la causale blocco
031700110202           IF not $SiAcv and wDatBlc = 0;
031800110202             clear ACVdav;
031900110202             clear ACVorv;
032000110202             wDatBlc = 00010101;
032100110202             wOraBlc = 000001;
032200110202             wCauBlc = CLPnar;
032300110202           ENDIF;
032400110201
032500110202         //?se data più alta memorizzo la causale e data/ora variazione
032600110202           IF  ACVdav > wDatBlc or
032700110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
032800110202             wDatBlc = ACVdav;
032900110202             wOraBlc = ACVorv;
033000110202             wCauBlc = CLPnar;
033100110202           ENDIF;
033200110201
033300110202           reade CPOcpo CNACO16L;
033400110201         ENDDO;
033500100413
033600110201       ENDSR;
033700110201
033800100413       //--------------------------------------------------------------
033900100726       //?Scrivo nuova attività.
034000100413       //--------------------------------------------------------------
034100100413       BEGSR Scrivi;
034200100422
034300100422         clear wDad;
034400110201         clear wCad;
034500110323
034600110323       //?Recupero l'attività da creare
034700110323         clear xx;
034800110323         xx = %lookup(wCauBlc:$BIatc);
034900110323         IF  xx > 0;
035000110323           wCad = %subst($BIatccau(xx):4:2);
035100110323         ENDIF;
035200110513
035300110513       //?Cerco l'area del potenziale
035400110513         chain CPOflt AZORG01L;
035500110513         IF  not %found(AZORG01L);
035600110513           clear ORGcar;
035700110513         ENDIF;
035800110513
035900110513       //?Imposto i gg
036000110513         exsr Calcolagg;
036100110513
036200110131       //?Calcolo la data attività
036300110131         clear XGIOLAVDS;
036400110131         IXGLdata = %dec(%date());
036500110131         IXGLadd  = 'S';
036600110218         IF  wCad = '73';
036700110218           IXGLgg   = wgg73;
036800110218         ENDIF;
036900110218         IF  wCad = '74';
037000110218           IXGLgg   = wgg74;
037100110218         ENDIF;
037200110131         IXGLpa   = 'P';
037300110218         //IXGLlav  = 'S';
037400110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
037500110131         XGIOLAV (xgiolavds);
037600110131         IF  OXGLerr = *blanks;
037700110131           wDad = OXGLdata;
037800110131         ELSE;
037900110131           wDad = %dec(%date());
038000110131         ENDIF;
038100110201
038200110131       //?Scrivo la nuova attività
038300110131         exsr Scrivi_TIATC;
038400100413
038500100413       ENDSR;
038600110513
038700110513       //--------------------------------------------------------------
038800110513       //?Calcolo i gg.
038900110513       //--------------------------------------------------------------
039000110513       BEGSR Calcolagg;
039100110513
039200110513         wgg73 = 1;
039300110513         wgg74 = 1;
039400110513
039500110513       //?Calcolo da quanto tempo il cliente è bloccato
039600110513         wDataISO1 = %date(wDataOggi:*iso);
039700110513         wDataISO2 = %date(wDatBlc:*iso);
039800110513         wgg = %diff(wDataISO1 : wDataISO2 : *days);
039900110513
040000110513       //?Per attività 73 calcolo i gg in base al codice importanza cliente
040100110513         IF  wCad = '73';
040200110513         //?se cliente DTAB creo l'attività a 30 gg.
040300110513           IF  CPOftr <> 'C' and CPOftr <> *blanks;
040400110513             wgg73 = 30;
040500110513           ELSE;
040600110513         //?se cliente C o senza codice importanza cliente divido in 3 tranche
040700110513         //?60 - 90 - 120 gg.
040800110513             exsr Tranche73;
040900110513           ENDIF;
041000110516         ENDIF;
041100110513
041200110513       //?Per attività 74 calcolo i gg in base alla data blocco
041300110513         IF  wCad = '74';
041400110513         //?calcolo i gg in base alla data blocco
041500110513           SELECT;
041600110513           //?cliente bloccato entro 1 anno creo l'attività a 240 gg.
041700110513             WHEN  wgg <= 365;
041800110513               wgg74 = 240;
041900110513           //?cliente bloccato da oltre 1 anno fino a 1 anno e mezzo creo l'attività a 210 gg.
042000110513             WHEN  wgg <= 540;
042100110513               wgg74 = 210;
042200110513           //?cliente bloccato da oltre 1 anno e mezzo e fino a 2 anni creo l'attività a 180 gg.
042300110513             WHEN  wgg <= 730;
042400110513               wgg74 = 180;
042500110513           //?cliente bloccato da oltre 2 anni divido in 6 tranche
042600110513           //?1 - 30 - 60 - 90 - 120 - 150 gg.
042700110513             OTHER;
042800110513               exsr Tranche74;
042900110516           ENDSL;
043000110513         ENDIF;
043100110513
043200110513       ENDSR;
043300110513
043400110513       //--------------------------------------------------------------
043500110513       //?Tranche 73 per clienti C.
043600110513       //--------------------------------------------------------------
043700110513       BEGSR Tranche73;
043800110513
043900110516       //?Cerco l'area nella sk
044000110516         clear zz;
044100110516         zz = %lookup(Orgcar:sk73);
044200110516       //?Se l'area non è ancora stata memorizzata metto nella prima tranche
044300110516         IF  zz = 0;
044400110516           conta = 1;
044500110516           clear zz;
044600110516           zz = %lookup(0:sk73);
044700110516           IF  zz > 0;
044800110516             sk73(zz)  = ORGcar;
044900110516             sk73u(zz) = conta;
045000110516             wgg73 = 60;
045100110516           ENDIF;
045200110516       //?Se l'aera è già presente metto alla tranche successiva all'ultima memorizzata
045300110516       //?la memorizzo nella seconda
045400110516         ELSE;
045500110516           SELECT;
045600110516             WHEN sk73u(zz) = 1;
045700110516              conta = sk73u(zz) + 1;
045800110516              sk73u(zz) = conta;
045900110516              wgg73 = 90;
046000110516             WHEN sk73u(zz) = 2;
046100110516              conta = sk73u(zz) + 1;
046200110516              sk73u(zz) = conta;
046300110516              wgg73 = 120;
046400110516             WHEN sk73u(zz) = 3;
046500110516              conta = 1;
046600110516              sk73u(zz) = conta;
046700110516              wgg73 = 60;
046800110516           ENDSL;
046900110513         ENDIF;
047000110513
047100110513       ENDSR;
047200110516
047300110516       //--------------------------------------------------------------
047400110516       //?Tranche 74 per blocco cliente oltre 2 anni.
047500110516       //--------------------------------------------------------------
047600110516       BEGSR Tranche74;
047700110516
047800110516       //?Cerco l'area nella sk
047900110516         clear zz;
048000110516         zz = %lookup(Orgcar:sk74);
048100110516       //?Se l'area non è ancora stata memorizzata metto nella prima tranche
048200110516         IF  zz = 0;
048300110516           conta = 1;
048400110516           clear zz;
048500110516           zz = %lookup(0:sk74);
048600110516           IF  zz > 0;
048700110516             sk74(zz)  = ORGcar;
048800110516             sk74u(zz) = conta;
048900110516             wgg74 = 1;
049000110516           ENDIF;
049100110516       //?Se l'aera è già presente metto alla tranche successiva all'ultima memorizzata
049200110516       //?la memorizzo nella seconda
049300110516         ELSE;
049400110516           SELECT;
049500110516             WHEN sk74u(zz) = 1;
049600110516              conta = sk74u(zz) + 1;
049700110516              sk74u(zz) = conta;
049800110516              wgg74 = 30;
049900110516             WHEN sk74u(zz) = 2;
050000110516              conta = sk74u(zz) + 1;
050100110516              sk74u(zz) = conta;
050200110516              wgg74 = 60;
050300110516             WHEN sk74u(zz) = 3;
050400110516              conta = sk74u(zz) + 1;
050500110516              sk74u(zz) = conta;
050600110516              wgg74 = 90;
050700110516             WHEN sk74u(zz) = 4;
050800110516              conta = sk74u(zz) + 1;
050900110516              sk74u(zz) = conta;
051000110516              wgg74 = 120;
051100110516             WHEN sk74u(zz) = 5;
051200110516              conta = sk74u(zz) + 1;
051300110516              sk74u(zz) = conta;
051400110516              wgg74 = 150;
051500110516             WHEN sk74u(zz) = 6;
051600110516              conta = 1;
051700110516              sk74u(zz) = conta;
051800110516              wgg74 = 1;
051900110516           ENDSL;
052000110516         ENDIF;
052100110516
052200110516       ENDSR;
052300100415
052400100415       //--------------------------------------------------------------
052500100415       //?Scrivo file TIATC.
052600100415       //--------------------------------------------------------------
052700100415       BEGSR Scrivi_TIATC;
052800100415
052900110211         clear dATC01;
053000110516         clear wfcad7300;
053100110131         ATCcpo  = CPOcpo;
053200110201         ATCcad  = wCad;
053300110131         ATCdad  = wDad;
053400100422         ATChda  = 103000;
053500110131         ATCcmm  = CPOcmm;
053600100415         ATCdim  = %dec(%date());
053700100415         ATChim  = %dec(%time());
053800100415         ATCpri  = 'BATCH';
053900110516         write WFCAD7300;
054000100415
054100100415       ENDSR;
054200100413
054300100413       //--------------------------------------------------------------
054400100413       //?Operazioni finali.
054500100413       //--------------------------------------------------------------
054600100513       BEGSR RoutEnd;
054700100413
054800100413         *inLR = *on;
054900100413         return;
055000100413
055100100413       ENDSR;
055200100413
055300100413      /end-free
