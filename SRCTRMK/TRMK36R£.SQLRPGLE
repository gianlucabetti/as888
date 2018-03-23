000100100413      //---------------------------------------------------------------
000200110201      //?TRMK36R - Crea attività 73/74 su potenziali persi
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000110202      // - File Anagrafica clienti
001100110202     fCNACO16L  if   e           k disk
001200110202     fCNCLP00F  if   e           k disk
001300110201      // - File Variazioni anagrafiche
001400110201     fCNACVT1L  if   e           k disk
001500110323      // - File Tabelle
001600110323     fTABEL00F  if   e           k disk
001700110323      // - File Attività
001800100415     fTIATC00F  o    e             disk
001900110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
002000110428      // - File Trattative
002100110502     fTIVIS01L  if   e           k disk    usropn
002200110502     f                                     extfile(wFLib)
002300100413
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione costanti.
002600100413      //---------------------------------------------------------------
002700100413
002800100413      //---------------------------------------------------------------
002900100413      //?Definizione schiere.
003000100413      //---------------------------------------------------------------
003100110323
003200110323      // - Causali che generano attività
003300110323     d $BIatc          s              3    dim(20)
003400110323
003500110323      // - Causali + cod. attività
003600110323     d $BIatccau       s              5    dim(20)
003700110323
003800110323      // - Causali che non generano attività
003900110323     d $BInoatc        s              3    dim(20)
004000090715
004100100413      //---------------------------------------------------------------
004200100413      //?Definizione aree dati.
004300100413      //---------------------------------------------------------------
004400100413
004500100413      //---------------------------------------------------------------
004600100413      //?Definizione strutture dati.
004700100413      //---------------------------------------------------------------
004800100415
004900110131      // - Parametri ricevuti
005000100415     d KPJBA         e ds
005100110211
005200110211      // ds x ATCflo
005300110211     d dATC01        e ds
005400100726
005500100726      // File potenziali
005600100726     d TNCPOds       e ds                  extname(TNCPO00F)
005700100415
005800100415      // - Ricerca ultimo numero attività
005900100415     d TRUL33ds      e ds                  inz
006000110131
006100110131      // - Aggiunge/Toglie gg/mm dalla data
006200110131     d XGIOLAVds     e ds                  inz
006300110323
006400110323      // - Tabella BI - Blocco Causali Clienti
006500110323     d dsBI          e ds                  inz
006600100415
006700100413      //---------------------------------------------------------------
006800100413      //?Definizione variabili globali.
006900100413      //---------------------------------------------------------------
007000100413
007100100413      // - Flags booleani
007200100413     d $End            s               n   inz(*off)
007300110201     d $SiAcv          s               n   inz(*off)
007400110323
007500110323      // - Indici di schiera
007600110323     d xx              s              3  0
007700110323     d yy              s              3  0
007800110131
007900110131      // - Campi di comodo
008000110201     d NrAtt           s              5i 0
008100110201     d NrVis           s              5i 0
008200110201     d wCad            s                   like(ATCcad)
008300110201     d wCauBlc         s                   like(CLPnar)
008400110131     d wDad            s                   like(ATCdad)
008500110201     d wDatBlc         s                   like(ACVdav)
008600110201     d wOraBlc         s                   like(ACVorv)
008700110218     d wgg73           s                   like(IXGLgg) inz(90)
008800110218     d wgg74           s                   like(IXGLgg) inz(365)
008900110502
009000110502       // - Nome libreria del file TIVIS
009100110502     d wFLib           s             21    inz
009200100413
009300100413      //---------------------------------------------------------------
009400100413      //?Definizione procedure esterne.
009500100413      //---------------------------------------------------------------
009600110131
009700110131      // - Aggiunge/Toglie gg/mm dalla data
009800110131     d Xgiolav         pr                  extpgm('XGIOLAV')
009900110131     d  xgiolavds                          likeds(xgiolavds)
010000100413
010100100413      //---------------------------------------------------------------
010200110131      //?Prototipi
010300100413      //---------------------------------------------------------------
010400100414
010500100415      /copy gaitrasrc/srcprotopr,trul33r
010600100413
010700100413      //---------------------------------------------------------------
010800100413      //?Definizione key-list.
010900100413      //---------------------------------------------------------------
011000110201
011100110201      // - Dettaglio variazioi anagrafiche
011200110201     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
011300110201     d                                     prefix(k_)
011400110201     d                                     inz
011500100413
011600100413      //---------------------------------------------------------------
011700100413      //?Riepilogo indicatori.
011800100413      //---------------------------------------------------------------
011900100413
012000100413      //---------------------------------------------------------------
012100100413      //?M A I N - L I N E
012200100413      //---------------------------------------------------------------
012300100413
012400100413     c     *Entry        plist
012500110131     c                   parm                    kpjba
012600100413
012700100413      /free
012800100413
012900100413       //?Operazioni iniziali
013000100513       exsr RoutInz;
013100100413
013200100726       //?Elaboro
013300100726       exsr Elabora;
013400100413
013500100413       //?Operazioni finali
013600100513       exsr RoutEnd;
013700100413
013800100413       //--------------------------------------------------------------
013900100413       //?Operazioni iniziali.
014000100413       //--------------------------------------------------------------
014100100513       BEGSR RoutInz;
014200100726
014300100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014400110323
014500110323         clear xx;
014600110323         clear yy;
014700110323
014800110323       //?Carico tabelle causali
014900110323         TBLkut = 1;
015000110323         TBLcod = 'BI';
015100110323         setll (TBLkut:TBLcod) TABEL00F;
015200110323         reade (TBLkut:TBLcod) TABEL00F;
015300110323         DOW  not %eof(TABEL00F);
015400110323           dsBI = TBLuni;
015500110323           IF  §BIatc <> *blanks;
015600110323             xx += 1;
015700110323         //?$BIATC dove memorizzo le causali che generano attività
015800110323         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
015900110323             $BIatc(xx) = %subst(TBLkey:1:3);
016000110323             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
016100110323             %subst($BIatccau(xx):4:2) = §BIatc;
016200110323           ELSE;
016300110323             yy += 1;
016400110323         //?$BINOATC dove memorizzo le causali che NON generano attività
016500110323             $BInoatc(yy) = %subst(TBLkey:1:3);
016600110323           ENDIF;
016700110323           reade (TBLkut:TBLcod) TABEL00F;
016800110323         ENDDO;
016900110502
017000110502         //?Apro il file delle trattative
017100110502         IF  %subst(knsif : 7 : 1) = 'P';
017200110502           wFLib = 'FILTRAGRPF/TIVIS01L';
017300110502         ELSE;
017400110502           wFLib = 'FILTRAGRU/TIVIS01L';
017500110502         ENDIF;
017600110502         open  TIVIS01L;
017700100413
017800100413       ENDSR;
017900100413
018000100413       //--------------------------------------------------------------
018100110131       //?Elabora.
018200100413       //--------------------------------------------------------------
018300100513       BEGSR Elabora;
018400100413
018500100413         $End = *off;
018600100413
018700110201       //?Estraggo solo i potenziali persi
018800100413         exec sql
018900100726         DECLARE CPO cursor for
019000110211         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt
019100110428         FROM TNCPO01L
019200110131         WHERE CPOfls = 'P'
019300100726         ORDER by CPOcpo;
019400100413
019500100726         exec sql OPEN CPO;
019600100413
019700100413         DOW  not $End;
019800110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
019900110211                                             :CPOFLS, :CPOFLT;
020000100413           IF  sqlcod = 100 or sqlcod < 0;
020100100413             $End = *on;
020200100413             leave;
020300100413           ENDIF;
020400100422
020500110201         //?Controllo se potenziale ha azioni o trattative aperte
020600110201           exsr CtrAtt;
020700110201
020800110203         //?Scrivo nova attività solo se NON trovo attività da eseguire o trattative aperte
020900110203           IF  NrAtt = 0 and NrVis = 0;
021000110201
021100110203           //?Controllo l'ultimo blocco dei clienti collegati
021200110203             exsr CtrBlc;
021300110315
021400110323           //?Controllo se la causale prevede la creazione dell'attività
021500110323           //?Se non la prevede leggo altro codice
021600110323             IF  %lookup(wCauBlc:$BInoatc) > 0;
021700110323               iter;
021800110323             ENDIF;
021900110201
022000110203           //?Genero nuova attività 73 o 74
022100110218             IF  CPOcmm > 0;
022200110218               exsr Scrivi;
022300110218             ENDIF;
022400110203           ENDIF;
022500110201
022600100413         ENDDO;
022700100413
022800100726         exec sql CLOSE CPO;
022900100413
023000100413       ENDSR;
023100110201
023200110201       //--------------------------------------------------------------
023300110201       //?Controllo attività/trattative.
023400110201       //--------------------------------------------------------------
023500110201       BEGSR CtrAtt;
023600110201
023700110201         clear NrAtt;
023800110201         clear NrVis;
023900110201
024000110428       //?Controllo se ci sono attività da eseguire
024100110428         chain (CPOcpo:0:0) TIATC05L;
024200110428         IF  %found(TIATC05L);
024300110428           NrAtt = 1;
024400110428           leavesr;
024500110428         ENDIF;
024600110428
024700110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
024800110428         setll (CPOcpo:0) TIVIS01L;
024900110428         reade (CPOcpo:0) TIVIS01L;
025000110428         DOW not %eof(TIVIS01L);
025100110428           IF  VISffz = ' ';
025200110428             NrVis = 1;
025300110428             leavesr;
025400110428           ENDIF;
025500110428           reade (CPOcpo:0) TIVIS01L;
025600110428         ENDDO;
025700110201
025800110201       ENDSR;
025900110201
026000110201       //--------------------------------------------------------------
026100110201       //?Controllo l'ultimo blocco sui clienti.
026200110201       //--------------------------------------------------------------
026300110201       BEGSR CtrBlc;
026400110202
026500110202         clear wCauBlc;
026600110202         clear wDatBlc;
026700110202         clear wOraBlc;
026800110201
026900110201       //?Leggo tutte le anagrafiche legate al codice potenziale
027000110202         setll CPOcpo CNACO16L;
027100110202         reade CPOcpo CNACO16L;
027200110202         DOW  not %eof(CNACO16L);
027300110202           IF  %eof(CNACO16L);
027400110202             leave;
027500110202           ENDIF;
027600110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
027700110202           IF  not %found(CNCLP00F);
027800110202             clear CLPnar;
027900110202           ENDIF;
028000110202
028100110202         //?Cerco la data della variazione
028200110202           $SiAcv = *off;
028300110202           setll (ACOkcc:ACOksc) CNACVT1L;
028400110202           reade (ACOkcc:ACOksc) CNACVT1L;
028500110202           DOW  not %eof(CNACVT1L);
028600110202             IF  %eof(CNACVT1L);
028700110202               leave;
028800110202             ENDIF;
028900110202             IF  ACVcta = 'M' and
029000110202                 %scan('B':ACVflo) > 0;
029100110202               $siAcv = *on;
029200110202               leave;
029300110202             ENDIF;
029400110202             reade (ACOkcc:ACOksc) CNACVT1L;
029500110202           ENDDO;
029600110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
029700110202         //?e memorizzo la causale blocco
029800110202           IF not $SiAcv and wDatBlc = 0;
029900110202             clear ACVdav;
030000110202             clear ACVorv;
030100110202             wDatBlc = 00010101;
030200110202             wOraBlc = 000001;
030300110202             wCauBlc = CLPnar;
030400110202           ENDIF;
030500110201
030600110202         //?se data più alta memorizzo la causale e data/ora variazione
030700110202           IF  ACVdav > wDatBlc or
030800110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
030900110202             wDatBlc = ACVdav;
031000110202             wOraBlc = ACVorv;
031100110202             wCauBlc = CLPnar;
031200110202           ENDIF;
031300110201
031400110202           reade CPOcpo CNACO16L;
031500110201         ENDDO;
031600100413
031700110201       ENDSR;
031800110201
031900100413       //--------------------------------------------------------------
032000100726       //?Scrivo nuova attività.
032100100413       //--------------------------------------------------------------
032200100413       BEGSR Scrivi;
032300100422
032400100422         clear wDad;
032500110201         clear wCad;
032600110323
032700110323       //?Recupero l'attività da creare
032800110323         clear xx;
032900110323         xx = %lookup(wCauBlc:$BIatc);
033000110323         IF  xx > 0;
033100110323           wCad = %subst($BIatccau(xx):4:2);
033200110323         ENDIF;
033300100415
033400100726       //?Stacco nuovo numero attività
033500110131         exsr Stacca_Numero;
033600110131       //?Calcolo la data attività
033700110131         clear XGIOLAVDS;
033800110131         IXGLdata = %dec(%date());
033900110131         IXGLadd  = 'S';
034000110218         IF  wCad = '73';
034100110218           IXGLgg   = wgg73;
034200110218         ENDIF;
034300110218         IF  wCad = '74';
034400110218           IXGLgg   = wgg74;
034500110218         ENDIF;
034600110131         IXGLpa   = 'P';
034700110218         //IXGLlav  = 'S';
034800110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
034900110131         XGIOLAV (xgiolavds);
035000110131         IF  OXGLerr = *blanks;
035100110131           wDad = OXGLdata;
035200110131         ELSE;
035300110131           wDad = %dec(%date());
035400110131         ENDIF;
035500110201
035600110131       //?Scrivo la nuova attività
035700110131         exsr Scrivi_TIATC;
035800100413
035900100413       ENDSR;
036000100415
036100100415       //--------------------------------------------------------------
036200100415       //?Stacco nuovo numero attività.
036300100415       //--------------------------------------------------------------
036400100415       BEGSR Stacca_Numero;
036500100415
036600100415         clear TRUL33ds;
036700100415         I33tla = 'L';
036800100415         I33ope = 0;
036900100415         I33cnu = 070;
037000100415         I33num = 1;
037100100415         I33aaa = *year;
037200100415         kpjbu  = TRUL33ds;
037300100415         trul33r (kpjba);
037400100415         TRUL33ds = kpjbu;
037500100415
037600100415       ENDSR;
037700100415
037800100415       //--------------------------------------------------------------
037900100415       //?Scrivo file TIATC.
038000100415       //--------------------------------------------------------------
038100100415       BEGSR Scrivi_TIATC;
038200100415
038300110211         clear dATC01;
038400100415         clear TIATC000;
038500110131         ATCtat  = 'T';
038600100415         ATCatn  = O33Nrf;
038700100415         ATCatnp = 1;
038800110131         ATCcpo  = CPOcpo;
038900110201         ATCcad  = wCad;
039000110131         ATCdad  = wDad;
039100100422         ATChda  = 103000;
039200110131         ATCcmm  = CPOcmm;
039300110131         ATCco3  = CPOcmm;
039400110131         ATCcnw = 'C';
039500100415         ATCdim  = %dec(%date());
039600100415         ATChim  = %dec(%time());
039700100415         ATCpri  = 'BATCH';
039800110211         §ATCcapo1 = CPOfls;
039900110211         ATCflo = dATC01;
040000100415         write TIATC000;
040100100415
040200100415       ENDSR;
040300100413
040400100413       //--------------------------------------------------------------
040500100413       //?Operazioni finali.
040600100413       //--------------------------------------------------------------
040700100513       BEGSR RoutEnd;
040800100413
040900100413         *inLR = *on;
041000100413         return;
041100100413
041200100413       ENDSR;
041300100413
041400100413      /end-free
