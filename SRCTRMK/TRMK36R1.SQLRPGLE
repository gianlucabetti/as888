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
002000100415     fTIATC00F  o    e             disk
002100110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
002200110428      // - File Trattative
002300110502     fTIVIS01L  if   e           k disk    usropn
002400110502     f                                     extfile(wFLib)
002500100413
002600100413      //---------------------------------------------------------------
002700100413      //?Definizione costanti.
002800100413      //---------------------------------------------------------------
002900100413
003000100413      //---------------------------------------------------------------
003100100413      //?Definizione schiere.
003200100413      //---------------------------------------------------------------
003300110323
003400110323      // - Causali che generano attività
003500110323     d $BIatc          s              3    dim(20)
003600110323
003700110323      // - Causali + cod. attività
003800110323     d $BIatccau       s              5    dim(20)
003900110323
004000110323      // - Causali che non generano attività
004100110323     d $BInoatc        s              3    dim(20)
004200110513
004300110516      // - Sk area x attività 73 con 3 tranche + ultima tranche fatta
004400110516     d sk73            s              3  0 dim(9999)
004500110516     d sk73u           s              3  0 dim(9999)
004600110513
004700110516      // - Sk area x attività 74 con 6 tranche + ultima tranche fatta
004800110516     d sk74            s              3  0 dim(9999)
004900110516     d sk74u           s              3  0 dim(9999)
005000110513
005100090715
005200100413      //---------------------------------------------------------------
005300100413      //?Definizione aree dati.
005400100413      //---------------------------------------------------------------
005500100413
005600100413      //---------------------------------------------------------------
005700100413      //?Definizione strutture dati.
005800100413      //---------------------------------------------------------------
005900100415
006000110131      // - Parametri ricevuti
006100100415     d KPJBA         e ds
006200110211
006300110211      // ds x ATCflo
006400110211     d dATC01        e ds
006500100726
006600100726      // File potenziali
006700100726     d TNCPOds       e ds                  extname(TNCPO00F)
006800100415
006900100415      // - Ricerca ultimo numero attività
007000100415     d TRUL33ds      e ds                  inz
007100110131
007200110131      // - Aggiunge/Toglie gg/mm dalla data
007300110131     d XGIOLAVds     e ds                  inz
007400110323
007500110323      // - Tabella BI - Blocco Causali Clienti
007600110323     d dsBI          e ds                  inz
007700100415
007800100413      //---------------------------------------------------------------
007900100413      //?Definizione variabili globali.
008000100413      //---------------------------------------------------------------
008100100413
008200100413      // - Flags booleani
008300100413     d $End            s               n   inz(*off)
008400110201     d $SiAcv          s               n   inz(*off)
008500110323
008600110323      // - Indici di schiera
008700110323     d xx              s              3  0
008800110323     d yy              s              3  0
008900110513     d zz              s              4  0
009000110131
009100110131      // - Campi di comodo
009200110513     d Conta           s              2  0 inz
009300110201     d NrAtt           s              5i 0
009400110201     d NrVis           s              5i 0
009500110201     d wCad            s                   like(ATCcad)
009600110201     d wCauBlc         s                   like(CLPnar)
009700110131     d wDad            s                   like(ATCdad)
009800110513     d wDataOggi       s              8  0
009900110513     d wDataISO1       s               d   datfmt(*iso)
010000110513     d wDataISO2       s               d   datfmt(*iso)
010100110201     d wDatBlc         s                   like(ACVdav)
010200110201     d wOraBlc         s                   like(ACVorv)
010300110516     d wgg             s             10  0
010400110218     d wgg73           s                   like(IXGLgg) inz(90)
010500110218     d wgg74           s                   like(IXGLgg) inz(365)
010600110502
010700110502       // - Nome libreria del file TIVIS
010800110502     d wFLib           s             21    inz
010900100413
011000100413      //---------------------------------------------------------------
011100100413      //?Definizione procedure esterne.
011200100413      //---------------------------------------------------------------
011300110131
011400110131      // - Aggiunge/Toglie gg/mm dalla data
011500110131     d Xgiolav         pr                  extpgm('XGIOLAV')
011600110131     d  xgiolavds                          likeds(xgiolavds)
011700100413
011800100413      //---------------------------------------------------------------
011900110131      //?Prototipi
012000100413      //---------------------------------------------------------------
012100100414
012200100415      /copy gaitrasrc/srcprotopr,trul33r
012300100413
012400100413      //---------------------------------------------------------------
012500100413      //?Definizione key-list.
012600100413      //---------------------------------------------------------------
012700110201
012800110201      // - Dettaglio variazioi anagrafiche
012900110201     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
013000110201     d                                     prefix(k_)
013100110201     d                                     inz
013200100413
013300100413      //---------------------------------------------------------------
013400100413      //?Riepilogo indicatori.
013500100413      //---------------------------------------------------------------
013600100413
013700100413      //---------------------------------------------------------------
013800100413      //?M A I N - L I N E
013900100413      //---------------------------------------------------------------
014000100413
014100100413     c     *Entry        plist
014200110131     c                   parm                    kpjba
014300100413
014400100413      /free
014500100413
014600100413       //?Operazioni iniziali
014700100513       exsr RoutInz;
014800100413
014900100726       //?Elaboro
015000100726       exsr Elabora;
015100100413
015200100413       //?Operazioni finali
015300100513       exsr RoutEnd;
015400100413
015500100413       //--------------------------------------------------------------
015600100413       //?Operazioni iniziali.
015700100413       //--------------------------------------------------------------
015800100513       BEGSR RoutInz;
015900100726
016000100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016100110323
016200110323         clear xx;
016300110323         clear yy;
016400110323
016500110323       //?Carico tabelle causali
016600110323         TBLkut = 1;
016700110323         TBLcod = 'BI';
016800110323         setll (TBLkut:TBLcod) TABEL00F;
016900110323         reade (TBLkut:TBLcod) TABEL00F;
017000110323         DOW  not %eof(TABEL00F);
017100110323           dsBI = TBLuni;
017200110323           IF  §BIatc <> *blanks;
017300110323             xx += 1;
017400110323         //?$BIATC dove memorizzo le causali che generano attività
017500110323         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
017600110323             $BIatc(xx) = %subst(TBLkey:1:3);
017700110323             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
017800110323             %subst($BIatccau(xx):4:2) = §BIatc;
017900110323           ELSE;
018000110323             yy += 1;
018100110323         //?$BINOATC dove memorizzo le causali che NON generano attività
018200110323             $BInoatc(yy) = %subst(TBLkey:1:3);
018300110323           ENDIF;
018400110323           reade (TBLkut:TBLcod) TABEL00F;
018500110323         ENDDO;
018600110502
018700110502         //?Apro il file delle trattative
018800110502         IF  %subst(knsif : 7 : 1) = 'P';
018900110502           wFLib = 'FILTRAGRPF/TIVIS01L';
019000110502         ELSE;
019100110502           wFLib = 'FILTRAGRU/TIVIS01L';
019200110502         ENDIF;
019300110502         open  TIVIS01L;
019400110513
019500110513         //?Data del giorno
019600110513         wDataOggi = %dec(%date());
019700100413
019800100413       ENDSR;
019900100413
020000100413       //--------------------------------------------------------------
020100110131       //?Elabora.
020200100413       //--------------------------------------------------------------
020300100513       BEGSR Elabora;
020400100413
020500100413         $End = *off;
020600100413
020700110201       //?Estraggo solo i potenziali persi
020800100413         exec sql
020900100726         DECLARE CPO cursor for
021000110516         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt, CPOftr
021100110428         FROM TNCPO01L
021200110131         WHERE CPOfls = 'P'
021300100726         ORDER by CPOcpo;
021400100413
021500100726         exec sql OPEN CPO;
021600100413
021700100413         DOW  not $End;
021800110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
021900110516                                             :CPOFLS, :CPOFLT,
022000110516                                             :CPOFTR;
022100100413           IF  sqlcod = 100 or sqlcod < 0;
022200100413             $End = *on;
022300100413             leave;
022400100413           ENDIF;
022500100422
022600110201         //?Controllo se potenziale ha azioni o trattative aperte
022700110201           exsr CtrAtt;
022800110201
022900110513         //?Scrivo nuova attività solo se NON trovo attività da eseguire o trattative aperte
023000110203           IF  NrAtt = 0 and NrVis = 0;
023100110201
023200110203           //?Controllo l'ultimo blocco dei clienti collegati
023300110203             exsr CtrBlc;
023400110315
023500110323           //?Controllo se la causale prevede la creazione dell'attività
023600110323           //?Se non la prevede leggo altro codice
023700110323             IF  %lookup(wCauBlc:$BInoatc) > 0;
023800110323               iter;
023900110323             ENDIF;
024000110201
024100110203           //?Genero nuova attività 73 o 74
024200110513           //?Ma solo se c'è il codice del commerciale
024300110218             IF  CPOcmm > 0;
024400110218               exsr Scrivi;
024500110218             ENDIF;
024600110203           ENDIF;
024700110201
024800100413         ENDDO;
024900100413
025000100726         exec sql CLOSE CPO;
025100100413
025200100413       ENDSR;
025300110201
025400110201       //--------------------------------------------------------------
025500110201       //?Controllo attività/trattative.
025600110201       //--------------------------------------------------------------
025700110201       BEGSR CtrAtt;
025800110201
025900110201         clear NrAtt;
026000110201         clear NrVis;
026100110201
026200110428       //?Controllo se ci sono attività da eseguire
026300110428         chain (CPOcpo:0:0) TIATC05L;
026400110428         IF  %found(TIATC05L);
026500110428           NrAtt = 1;
026600110428           leavesr;
026700110428         ENDIF;
026800110428
026900110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
027000110428         setll (CPOcpo:0) TIVIS01L;
027100110428         reade (CPOcpo:0) TIVIS01L;
027200110428         DOW not %eof(TIVIS01L);
027300110428           IF  VISffz = ' ';
027400110428             NrVis = 1;
027500110428             leavesr;
027600110428           ENDIF;
027700110428           reade (CPOcpo:0) TIVIS01L;
027800110428         ENDDO;
027900110201
028000110201       ENDSR;
028100110201
028200110201       //--------------------------------------------------------------
028300110201       //?Controllo l'ultimo blocco sui clienti.
028400110201       //--------------------------------------------------------------
028500110201       BEGSR CtrBlc;
028600110202
028700110202         clear wCauBlc;
028800110202         clear wDatBlc;
028900110202         clear wOraBlc;
029000110201
029100110201       //?Leggo tutte le anagrafiche legate al codice potenziale
029200110202         setll CPOcpo CNACO16L;
029300110202         reade CPOcpo CNACO16L;
029400110202         DOW  not %eof(CNACO16L);
029500110202           IF  %eof(CNACO16L);
029600110202             leave;
029700110202           ENDIF;
029800110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
029900110202           IF  not %found(CNCLP00F);
030000110202             clear CLPnar;
030100110202           ENDIF;
030200110202
030300110202         //?Cerco la data della variazione
030400110202           $SiAcv = *off;
030500110202           setll (ACOkcc:ACOksc) CNACVT1L;
030600110202           reade (ACOkcc:ACOksc) CNACVT1L;
030700110202           DOW  not %eof(CNACVT1L);
030800110202             IF  %eof(CNACVT1L);
030900110202               leave;
031000110202             ENDIF;
031100110202             IF  ACVcta = 'M' and
031200110202                 %scan('B':ACVflo) > 0;
031300110202               $siAcv = *on;
031400110202               leave;
031500110202             ENDIF;
031600110202             reade (ACOkcc:ACOksc) CNACVT1L;
031700110202           ENDDO;
031800110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
031900110202         //?e memorizzo la causale blocco
032000110202           IF not $SiAcv and wDatBlc = 0;
032100110202             clear ACVdav;
032200110202             clear ACVorv;
032300110202             wDatBlc = 00010101;
032400110202             wOraBlc = 000001;
032500110202             wCauBlc = CLPnar;
032600110202           ENDIF;
032700110201
032800110202         //?se data più alta memorizzo la causale e data/ora variazione
032900110202           IF  ACVdav > wDatBlc or
033000110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
033100110202             wDatBlc = ACVdav;
033200110202             wOraBlc = ACVorv;
033300110202             wCauBlc = CLPnar;
033400110202           ENDIF;
033500110201
033600110202           reade CPOcpo CNACO16L;
033700110201         ENDDO;
033800100413
033900110201       ENDSR;
034000110201
034100100413       //--------------------------------------------------------------
034200100726       //?Scrivo nuova attività.
034300100413       //--------------------------------------------------------------
034400100413       BEGSR Scrivi;
034500100422
034600100422         clear wDad;
034700110201         clear wCad;
034800110323
034900110323       //?Recupero l'attività da creare
035000110323         clear xx;
035100110323         xx = %lookup(wCauBlc:$BIatc);
035200110323         IF  xx > 0;
035300110323           wCad = %subst($BIatccau(xx):4:2);
035400110323         ENDIF;
035500110513
035600110513       //?Cerco l'area del potenziale
035700110513         chain CPOflt AZORG01L;
035800110513         IF  not %found(AZORG01L);
035900110513           clear ORGcar;
036000110513         ENDIF;
036100110513
036200110513       //?Imposto i gg
036300110513         exsr Calcolagg;
036400100415
036500100726       //?Stacco nuovo numero attività
036600110131         exsr Stacca_Numero;
036700110513
036800110131       //?Calcolo la data attività
036900110131         clear XGIOLAVDS;
037000110131         IXGLdata = %dec(%date());
037100110131         IXGLadd  = 'S';
037200110218         IF  wCad = '73';
037300110218           IXGLgg   = wgg73;
037400110218         ENDIF;
037500110218         IF  wCad = '74';
037600110218           IXGLgg   = wgg74;
037700110218         ENDIF;
037800110131         IXGLpa   = 'P';
037900110218         //IXGLlav  = 'S';
038000110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
038100110131         XGIOLAV (xgiolavds);
038200110131         IF  OXGLerr = *blanks;
038300110131           wDad = OXGLdata;
038400110131         ELSE;
038500110131           wDad = %dec(%date());
038600110131         ENDIF;
038700110201
038800110131       //?Scrivo la nuova attività
038900110131         exsr Scrivi_TIATC;
039000100413
039100100413       ENDSR;
039200110513
039300110513       //--------------------------------------------------------------
039400110513       //?Calcolo i gg.
039500110513       //--------------------------------------------------------------
039600110513       BEGSR Calcolagg;
039700110513
039800110513         wgg73 = 1;
039900110513         wgg74 = 1;
040000110513
040100110513       //?Calcolo da quanto tempo il cliente è bloccato
040200110513         wDataISO1 = %date(wDataOggi:*iso);
040300110513         wDataISO2 = %date(wDatBlc:*iso);
040400110513         wgg = %diff(wDataISO1 : wDataISO2 : *days);
040500110513
040600110513       //?Per attività 73 calcolo i gg in base al codice importanza cliente
040700110513         IF  wCad = '73';
040800110513         //?se cliente DTAB creo l'attività a 30 gg.
040900160926           //*·IF  CPOftr <> 'C' and CPOftr <> *blanks;
041000160926           IF  CPOftr <> 'C' and CPOftr <> 'E' and
041100160926               CPOftr <> *blanks;
041200110513             wgg73 = 30;
041300110513           ELSE;
041400110513         //?se cliente C o senza codice importanza cliente divido in 3 tranche
041500110513         //?60 - 90 - 120 gg.
041600110513             exsr Tranche73;
041700110513           ENDIF;
041800110516         ENDIF;
041900110513
042000110513       //?Per attività 74 calcolo i gg in base alla data blocco
042100110513         IF  wCad = '74';
042200110513         //?calcolo i gg in base alla data blocco
042300110513           SELECT;
042400110513           //?cliente bloccato entro 1 anno creo l'attività a 240 gg.
042500110513             WHEN  wgg <= 365;
042600110513               wgg74 = 240;
042700110513           //?cliente bloccato da oltre 1 anno fino a 1 anno e mezzo creo l'attività a 210 gg.
042800110513             WHEN  wgg <= 540;
042900110513               wgg74 = 210;
043000110513           //?cliente bloccato da oltre 1 anno e mezzo e fino a 2 anni creo l'attività a 180 gg.
043100110513             WHEN  wgg <= 730;
043200110513               wgg74 = 180;
043300110513           //?cliente bloccato da oltre 2 anni divido in 6 tranche
043400110513           //?1 - 30 - 60 - 90 - 120 - 150 gg.
043500110516             OTHER;
043600110513               exsr Tranche74;
043700110516           ENDSL;
043800110513         ENDIF;
043900110513
044000110513       ENDSR;
044100110513
044200110513       //--------------------------------------------------------------
044300110513       //?Tranche 73 per clienti C.
044400110513       //--------------------------------------------------------------
044500110513       BEGSR Tranche73;
044600110513
044700110516       //?Cerco l'area nella sk
044800110516         clear zz;
044900110516         zz = %lookup(Orgcar:sk73);
045000110516       //?Se l'area non è ancora stata memorizzata metto nella prima tranche
045100110516         IF  zz = 0;
045200110516           conta = 1;
045300110516           clear zz;
045400110516           zz = %lookup(0:sk73);
045500110516           IF  zz > 0;
045600110516             sk73(zz)  = ORGcar;
045700110516             sk73u(zz) = conta;
045800110516             wgg73 = 60;
045900110516           ENDIF;
046000110516       //?Se l'aera è già presente metto alla tranche successiva all'ultima memorizzata
046100110516       //?la memorizzo nella seconda
046200110516         ELSE;
046300110516           SELECT;
046400110516             WHEN sk73u(zz) = 1;
046500110516              conta = sk73u(zz) + 1;
046600110516              sk73u(zz) = conta;
046700110516              wgg73 = 90;
046800110516             WHEN sk73u(zz) = 2;
046900110516              conta = sk73u(zz) + 1;
047000110516              sk73u(zz) = conta;
047100110516              wgg73 = 120;
047200110516             WHEN sk73u(zz) = 3;
047300110516              conta = 1;
047400110516              sk73u(zz) = conta;
047500110516              wgg73 = 60;
047600110516           ENDSL;
047700110513         ENDIF;
047800110513
047900110513       ENDSR;
048000110516
048100110516       //--------------------------------------------------------------
048200110516       //?Tranche 74 per blocco cliente oltre 2 anni.
048300110516       //--------------------------------------------------------------
048400110516       BEGSR Tranche74;
048500110516
048600110516       //?Cerco l'area nella sk
048700110516         clear zz;
048800110516         zz = %lookup(Orgcar:sk74);
048900110516       //?Se l'area non è ancora stata memorizzata metto nella prima tranche
049000110516         IF  zz = 0;
049100110516           conta = 1;
049200110516           clear zz;
049300110516           zz = %lookup(0:sk74);
049400110516           IF  zz > 0;
049500110516             sk74(zz)  = ORGcar;
049600110516             sk74u(zz) = conta;
049700110516             wgg74 = 1;
049800110516           ENDIF;
049900110516       //?Se l'aera è già presente metto alla tranche successiva all'ultima memorizzata
050000110516       //?la memorizzo nella seconda
050100110516         ELSE;
050200110516           SELECT;
050300110516             WHEN sk74u(zz) = 1;
050400110516              conta = sk74u(zz) + 1;
050500110516              sk74u(zz) = conta;
050600110516              wgg74 = 30;
050700110516             WHEN sk74u(zz) = 2;
050800110516              conta = sk74u(zz) + 1;
050900110516              sk74u(zz) = conta;
051000110516              wgg74 = 60;
051100110516             WHEN sk74u(zz) = 3;
051200110516              conta = sk74u(zz) + 1;
051300110516              sk74u(zz) = conta;
051400110516              wgg74 = 90;
051500110516             WHEN sk74u(zz) = 4;
051600110516              conta = sk74u(zz) + 1;
051700110516              sk74u(zz) = conta;
051800110516              wgg74 = 120;
051900110516             WHEN sk74u(zz) = 5;
052000110516              conta = sk74u(zz) + 1;
052100110516              sk74u(zz) = conta;
052200110516              wgg74 = 150;
052300110516             WHEN sk74u(zz) = 6;
052400110516              conta = 1;
052500110516              sk74u(zz) = conta;
052600110516              wgg74 = 1;
052700110516           ENDSL;
052800110516         ENDIF;
052900110516
053000110516       ENDSR;
053100100415
053200100415       //--------------------------------------------------------------
053300100415       //?Stacco nuovo numero attività.
053400100415       //--------------------------------------------------------------
053500100415       BEGSR Stacca_Numero;
053600100415
053700100415         clear TRUL33ds;
053800100415         I33tla = 'L';
053900100415         I33ope = 0;
054000100415         I33cnu = 070;
054100100415         I33num = 1;
054200100415         I33aaa = *year;
054300100415         kpjbu  = TRUL33ds;
054400100415         trul33r (kpjba);
054500100415         TRUL33ds = kpjbu;
054600100415
054700100415       ENDSR;
054800100415
054900100415       //--------------------------------------------------------------
055000100415       //?Scrivo file TIATC.
055100100415       //--------------------------------------------------------------
055200100415       BEGSR Scrivi_TIATC;
055300100415
055400110211         clear dATC01;
055500100415         clear TIATC000;
055600110131         ATCtat  = 'T';
055700100415         ATCatn  = O33Nrf;
055800100415         ATCatnp = 1;
055900110131         ATCcpo  = CPOcpo;
056000110201         ATCcad  = wCad;
056100110131         ATCdad  = wDad;
056200100422         ATChda  = 103000;
056300110131         ATCcmm  = CPOcmm;
056400110131         ATCco3  = CPOcmm;
056500110131         ATCcnw = 'C';
056600100415         ATCdim  = %dec(%date());
056700100415         ATChim  = %dec(%time());
056800100415         ATCpri  = 'BATCH';
056900110211         §ATCcapo1 = CPOfls;
057000110211         ATCflo = dATC01;
057100100415         write TIATC000;
057200100415
057300100415       ENDSR;
057400100413
057500100413       //--------------------------------------------------------------
057600100413       //?Operazioni finali.
057700100413       //--------------------------------------------------------------
057800100513       BEGSR RoutEnd;
057900100413
058000100413         *inLR = *on;
058100100413         return;
058200100413
058300100413       ENDSR;
058400100413
058500100413      /end-free
