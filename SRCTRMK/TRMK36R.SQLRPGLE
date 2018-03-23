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
002100110803     fTIVIS01L  if   e           k disk
002200100413
002300100413      //---------------------------------------------------------------
002400100413      //?Definizione costanti.
002500100413      //---------------------------------------------------------------
002600100413
002700100413      //---------------------------------------------------------------
002800100413      //?Definizione schiere.
002900100413      //---------------------------------------------------------------
003000110323
003100110323      // - Causali che generano attività
003200110323     d $BIatc          s              3    dim(20)
003300110323
003400110323      // - Causali + cod. attività
003500110323     d $BIatccau       s              5    dim(20)
003600110323
003700110323      // - Causali che non generano attività
003800110323     d $BInoatc        s              3    dim(20)
003900090715
004000100413      //---------------------------------------------------------------
004100100413      //?Definizione aree dati.
004200100413      //---------------------------------------------------------------
004300100413
004400100413      //---------------------------------------------------------------
004500100413      //?Definizione strutture dati.
004600100413      //---------------------------------------------------------------
004700100415
004800110131      // - Parametri ricevuti
004900100415     d KPJBA         e ds
005000110211
005100110211      // ds x ATCflo
005200110211     d dATC01        e ds
005300100726
005400100726      // File potenziali
005500100726     d TNCPOds       e ds                  extname(TNCPO00F)
005600100415
005700100415      // - Ricerca ultimo numero attività
005800100415     d TRUL33ds      e ds                  inz
005900110131
006000110131      // - Aggiunge/Toglie gg/mm dalla data
006100110131     d XGIOLAVds     e ds                  inz
006200110323
006300110323      // - Tabella BI - Blocco Causali Clienti
006400110323     d dsBI          e ds                  inz
006500100415
006600100413      //---------------------------------------------------------------
006700100413      //?Definizione variabili globali.
006800100413      //---------------------------------------------------------------
006900100413
007000100413      // - Flags booleani
007100100413     d $End            s               n   inz(*off)
007200110201     d $SiAcv          s               n   inz(*off)
007300110323
007400110323      // - Indici di schiera
007500110323     d xx              s              3  0
007600110323     d yy              s              3  0
007700110131
007800110131      // - Campi di comodo
007900110201     d NrAtt           s              5i 0
008000110201     d NrVis           s              5i 0
008100110201     d wCad            s                   like(ATCcad)
008200110201     d wCauBlc         s                   like(CLPnar)
008300110131     d wDad            s                   like(ATCdad)
008400110201     d wDatBlc         s                   like(ACVdav)
008500110201     d wOraBlc         s                   like(ACVorv)
008600110218     d wgg73           s                   like(IXGLgg) inz(90)
008700110218     d wgg74           s                   like(IXGLgg) inz(365)
008800110502
008900100413
009000100413      //---------------------------------------------------------------
009100100413      //?Definizione procedure esterne.
009200100413      //---------------------------------------------------------------
009300110131
009400110131      // - Aggiunge/Toglie gg/mm dalla data
009500110131     d Xgiolav         pr                  extpgm('XGIOLAV')
009600110131     d  xgiolavds                          likeds(xgiolavds)
009700100413
009800100413      //---------------------------------------------------------------
009900110131      //?Prototipi
010000100413      //---------------------------------------------------------------
010100100414
010200100415      /copy gaitrasrc/srcprotopr,trul33r
010300100413
010400100413      //---------------------------------------------------------------
010500100413      //?Definizione key-list.
010600100413      //---------------------------------------------------------------
010700110201
010800110201      // - Dettaglio variazioi anagrafiche
010900110201     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
011000110201     d                                     prefix(k_)
011100110201     d                                     inz
011200100413
011300100413      //---------------------------------------------------------------
011400100413      //?Riepilogo indicatori.
011500100413      //---------------------------------------------------------------
011600100413
011700100413      //---------------------------------------------------------------
011800100413      //?M A I N - L I N E
011900100413      //---------------------------------------------------------------
012000100413
012100100413     c     *Entry        plist
012200110131     c                   parm                    kpjba
012300100413
012400100413      /free
012500100413
012600100413       //?Operazioni iniziali
012700100513       exsr RoutInz;
012800100413
012900100726       //?Elaboro
013000100726       exsr Elabora;
013100100413
013200100413       //?Operazioni finali
013300100513       exsr RoutEnd;
013400100413
013500100413       //--------------------------------------------------------------
013600100413       //?Operazioni iniziali.
013700100413       //--------------------------------------------------------------
013800100513       BEGSR RoutInz;
013900100726
014000100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014100110323
014200110323         clear xx;
014300110323         clear yy;
014400110323
014500110323       //?Carico tabelle causali
014600110323         TBLkut = 1;
014700110323         TBLcod = 'BI';
014800110323         setll (TBLkut:TBLcod) TABEL00F;
014900110323         reade (TBLkut:TBLcod) TABEL00F;
015000110323         DOW  not %eof(TABEL00F);
015100110323           dsBI = TBLuni;
015200110323           IF  §BIatc <> *blanks;
015300110323             xx += 1;
015400110323         //?$BIATC dove memorizzo le causali che generano attività
015500110323         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
015600110323             $BIatc(xx) = %subst(TBLkey:1:3);
015700110323             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
015800110323             %subst($BIatccau(xx):4:2) = §BIatc;
015900110323           ELSE;
016000110323             yy += 1;
016100110323         //?$BINOATC dove memorizzo le causali che NON generano attività
016200110323             $BInoatc(yy) = %subst(TBLkey:1:3);
016300110323           ENDIF;
016400110323           reade (TBLkut:TBLcod) TABEL00F;
016500110323         ENDDO;
016600100413
016700100413       ENDSR;
016800100413
016900100413       //--------------------------------------------------------------
017000110131       //?Elabora.
017100100413       //--------------------------------------------------------------
017200100513       BEGSR Elabora;
017300100413
017400100413         $End = *off;
017500100413
017600110201       //?Estraggo solo i potenziali persi
017700100413         exec sql
017800100726         DECLARE CPO cursor for
017900110211         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt
018000110428         FROM TNCPO01L
018100110131         WHERE CPOfls = 'P'
018200100726         ORDER by CPOcpo;
018300100413
018400100726         exec sql OPEN CPO;
018500100413
018600100413         DOW  not $End;
018700110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
018800110211                                             :CPOFLS, :CPOFLT;
018900100413           IF  sqlcod = 100 or sqlcod < 0;
019000100413             $End = *on;
019100100413             leave;
019200100413           ENDIF;
019300100422
019400110201         //?Controllo se potenziale ha azioni o trattative aperte
019500110201           exsr CtrAtt;
019600110201
019700110203         //?Scrivo nova attività solo se NON trovo attività da eseguire o trattative aperte
019800110203           IF  NrAtt = 0 and NrVis = 0;
019900110201
020000110203           //?Controllo l'ultimo blocco dei clienti collegati
020100110203             exsr CtrBlc;
020200110315
020300110323           //?Controllo se la causale prevede la creazione dell'attività
020400110323           //?Se non la prevede leggo altro codice
020500110323             IF  %lookup(wCauBlc:$BInoatc) > 0;
020600110323               iter;
020700110323             ENDIF;
020800110201
020900110203           //?Genero nuova attività 73 o 74
021000110218             IF  CPOcmm > 0;
021100110218               exsr Scrivi;
021200110218             ENDIF;
021300110203           ENDIF;
021400110201
021500100413         ENDDO;
021600100413
021700100726         exec sql CLOSE CPO;
021800100413
021900100413       ENDSR;
022000110201
022100110201       //--------------------------------------------------------------
022200110201       //?Controllo attività/trattative.
022300110201       //--------------------------------------------------------------
022400110201       BEGSR CtrAtt;
022500110201
022600110201         clear NrAtt;
022700110201         clear NrVis;
022800110201
022900110428       //?Controllo se ci sono attività da eseguire
023000110428         chain (CPOcpo:0:0) TIATC05L;
023100110428         IF  %found(TIATC05L);
023200110428           NrAtt = 1;
023300110428           leavesr;
023400110428         ENDIF;
023500110428
023600110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
023700110428         setll (CPOcpo:0) TIVIS01L;
023800110428         reade (CPOcpo:0) TIVIS01L;
023900110428         DOW not %eof(TIVIS01L);
024000110428           IF  VISffz = ' ';
024100110428             NrVis = 1;
024200110428             leavesr;
024300110428           ENDIF;
024400110428           reade (CPOcpo:0) TIVIS01L;
024500110428         ENDDO;
024600110201
024700110201       ENDSR;
024800110201
024900110201       //--------------------------------------------------------------
025000110201       //?Controllo l'ultimo blocco sui clienti.
025100110201       //--------------------------------------------------------------
025200110201       BEGSR CtrBlc;
025300110202
025400110202         clear wCauBlc;
025500110202         clear wDatBlc;
025600110202         clear wOraBlc;
025700110201
025800110201       //?Leggo tutte le anagrafiche legate al codice potenziale
025900110202         setll CPOcpo CNACO16L;
026000110202         reade CPOcpo CNACO16L;
026100110202         DOW  not %eof(CNACO16L);
026200110202           IF  %eof(CNACO16L);
026300110202             leave;
026400110202           ENDIF;
026500110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
026600110202           IF  not %found(CNCLP00F);
026700110202             clear CLPnar;
026800110202           ENDIF;
026900110202
027000110202         //?Cerco la data della variazione
027100110202           $SiAcv = *off;
027200110202           setll (ACOkcc:ACOksc) CNACVT1L;
027300110202           reade (ACOkcc:ACOksc) CNACVT1L;
027400110202           DOW  not %eof(CNACVT1L);
027500110202             IF  %eof(CNACVT1L);
027600110202               leave;
027700110202             ENDIF;
027800110202             IF  ACVcta = 'M' and
027900110202                 %scan('B':ACVflo) > 0;
028000110202               $siAcv = *on;
028100110202               leave;
028200110202             ENDIF;
028300110202             reade (ACOkcc:ACOksc) CNACVT1L;
028400110202           ENDDO;
028500110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
028600110202         //?e memorizzo la causale blocco
028700110202           IF not $SiAcv and wDatBlc = 0;
028800110202             clear ACVdav;
028900110202             clear ACVorv;
029000110202             wDatBlc = 00010101;
029100110202             wOraBlc = 000001;
029200110202             wCauBlc = CLPnar;
029300110202           ENDIF;
029400110201
029500110202         //?se data più alta memorizzo la causale e data/ora variazione
029600110202           IF  ACVdav > wDatBlc or
029700110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
029800110202             wDatBlc = ACVdav;
029900110202             wOraBlc = ACVorv;
030000110202             wCauBlc = CLPnar;
030100110202           ENDIF;
030200110201
030300110202           reade CPOcpo CNACO16L;
030400110201         ENDDO;
030500100413
030600110201       ENDSR;
030700110201
030800100413       //--------------------------------------------------------------
030900100726       //?Scrivo nuova attività.
031000100413       //--------------------------------------------------------------
031100100413       BEGSR Scrivi;
031200100422
031300100422         clear wDad;
031400110201         clear wCad;
031500110323
031600110323       //?Recupero l'attività da creare
031700110323         clear xx;
031800110323         xx = %lookup(wCauBlc:$BIatc);
031900110323         IF  xx > 0;
032000110323           wCad = %subst($BIatccau(xx):4:2);
032100110323         ENDIF;
032200100415
032300100726       //?Stacco nuovo numero attività
032400110131         exsr Stacca_Numero;
032500110131       //?Calcolo la data attività
032600110131         clear XGIOLAVDS;
032700110131         IXGLdata = %dec(%date());
032800110131         IXGLadd  = 'S';
032900110218         IF  wCad = '73';
033000110218           IXGLgg   = wgg73;
033100110218         ENDIF;
033200110218         IF  wCad = '74';
033300110218           IXGLgg   = wgg74;
033400110218         ENDIF;
033500110131         IXGLpa   = 'P';
033600110218         //IXGLlav  = 'S';
033700110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
033800110131         XGIOLAV (xgiolavds);
033900110131         IF  OXGLerr = *blanks;
034000110131           wDad = OXGLdata;
034100110131         ELSE;
034200110131           wDad = %dec(%date());
034300110131         ENDIF;
034400110201
034500110131       //?Scrivo la nuova attività
034600110131         exsr Scrivi_TIATC;
034700100413
034800100413       ENDSR;
034900100415
035000100415       //--------------------------------------------------------------
035100100415       //?Stacco nuovo numero attività.
035200100415       //--------------------------------------------------------------
035300100415       BEGSR Stacca_Numero;
035400100415
035500100415         clear TRUL33ds;
035600100415         I33tla = 'L';
035700100415         I33ope = 0;
035800100415         I33cnu = 070;
035900100415         I33num = 1;
036000100415         I33aaa = *year;
036100100415         kpjbu  = TRUL33ds;
036200100415         trul33r (kpjba);
036300100415         TRUL33ds = kpjbu;
036400100415
036500100415       ENDSR;
036600100415
036700100415       //--------------------------------------------------------------
036800100415       //?Scrivo file TIATC.
036900100415       //--------------------------------------------------------------
037000100415       BEGSR Scrivi_TIATC;
037100100415
037200110211         clear dATC01;
037300100415         clear TIATC000;
037400110131         ATCtat  = 'T';
037500100415         ATCatn  = O33Nrf;
037600100415         ATCatnp = 1;
037700110131         ATCcpo  = CPOcpo;
037800110201         ATCcad  = wCad;
037900110131         ATCdad  = wDad;
038000100422         ATChda  = 103000;
038100110131         ATCcmm  = CPOcmm;
038200110131         ATCco3  = CPOcmm;
038300110131         ATCcnw = 'C';
038400100415         ATCdim  = %dec(%date());
038500100415         ATChim  = %dec(%time());
038600100415         ATCpri  = 'BATCH';
038700110211         §ATCcapo1 = CPOfls;
038800110211         ATCflo = dATC01;
038900100415         write TIATC000;
039000100415
039100100415       ENDSR;
039200100413
039300100413       //--------------------------------------------------------------
039400100413       //?Operazioni finali.
039500100413       //--------------------------------------------------------------
039600100513       BEGSR RoutEnd;
039700100413
039800100413         *inLR = *on;
039900100413         return;
040000100413
040100100413       ENDSR;
040200100413
040300100413      /end-free
