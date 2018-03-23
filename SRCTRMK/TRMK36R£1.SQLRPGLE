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
001900100413
002000100413      //---------------------------------------------------------------
002100100413      //?Definizione costanti.
002200100413      //---------------------------------------------------------------
002300100413
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione schiere.
002600100413      //---------------------------------------------------------------
002700110323
002800110323      // - Causali che generano attività
002900110323     d $BIatc          s              3    dim(20)
003000110323
003100110323      // - Causali + cod. attività
003200110323     d $BIatccau       s              5    dim(20)
003300110323
003400110323      // - Causali che non generano attività
003500110323     d $BInoatc        s              3    dim(20)
003600090715
003700100413      //---------------------------------------------------------------
003800100413      //?Definizione aree dati.
003900100413      //---------------------------------------------------------------
004000100413
004100100413      //---------------------------------------------------------------
004200100413      //?Definizione strutture dati.
004300100413      //---------------------------------------------------------------
004400100415
004500110131      // - Parametri ricevuti
004600100415     d KPJBA         e ds
004700110211
004800110211      // ds x ATCflo
004900110211     d dATC01        e ds
005000100726
005100100726      // File potenziali
005200100726     d TNCPOds       e ds                  extname(TNCPO00F)
005300100415
005400100415      // - Ricerca ultimo numero attività
005500100415     d TRUL33ds      e ds                  inz
005600110131
005700110131      // - Aggiunge/Toglie gg/mm dalla data
005800110131     d XGIOLAVds     e ds                  inz
005900110323
006000110323      // - Tabella BI - Blocco Causali Clienti
006100110323     d dsBI          e ds                  inz
006200100415
006300100413      //---------------------------------------------------------------
006400100413      //?Definizione variabili globali.
006500100413      //---------------------------------------------------------------
006600100413
006700100413      // - Flags booleani
006800100413     d $End            s               n   inz(*off)
006900110201     d $SiAcv          s               n   inz(*off)
007000110323
007100110323      // - Indici di schiera
007200110323     d xx              s              3  0
007300110323     d yy              s              3  0
007400110131
007500110131      // - Campi di comodo
007600110201     d NrAtt           s              5i 0
007700110201     d NrVis           s              5i 0
007800110201     d wCad            s                   like(ATCcad)
007900110201     d wCauBlc         s                   like(CLPnar)
008000110131     d wDad            s                   like(ATCdad)
008100110201     d wDatBlc         s                   like(ACVdav)
008200110201     d wOraBlc         s                   like(ACVorv)
008300110218     d wgg73           s                   like(IXGLgg) inz(90)
008400110218     d wgg74           s                   like(IXGLgg) inz(365)
008500100413
008600100413      //---------------------------------------------------------------
008700100413      //?Definizione procedure esterne.
008800100413      //---------------------------------------------------------------
008900110131
009000110131      // - Aggiunge/Toglie gg/mm dalla data
009100110131     d Xgiolav         pr                  extpgm('XGIOLAV')
009200110131     d  xgiolavds                          likeds(xgiolavds)
009300100413
009400100413      //---------------------------------------------------------------
009500110131      //?Prototipi
009600100413      //---------------------------------------------------------------
009700100414
009800100415      /copy gaitrasrc/srcprotopr,trul33r
009900100413
010000100413      //---------------------------------------------------------------
010100100413      //?Definizione key-list.
010200100413      //---------------------------------------------------------------
010300110201
010400110201      // - Dettaglio variazioi anagrafiche
010500110201     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
010600110201     d                                     prefix(k_)
010700110201     d                                     inz
010800100413
010900100413      //---------------------------------------------------------------
011000100413      //?Riepilogo indicatori.
011100100413      //---------------------------------------------------------------
011200100413
011300100413      //---------------------------------------------------------------
011400100413      //?M A I N - L I N E
011500100413      //---------------------------------------------------------------
011600100413
011700100413     c     *Entry        plist
011800110131     c                   parm                    kpjba
011900100413
012000100413      /free
012100100413
012200100413       //?Operazioni iniziali
012300100513       exsr RoutInz;
012400100413
012500100726       //?Elaboro
012600100726       exsr Elabora;
012700100413
012800100413       //?Operazioni finali
012900100513       exsr RoutEnd;
013000100413
013100100413       //--------------------------------------------------------------
013200100413       //?Operazioni iniziali.
013300100413       //--------------------------------------------------------------
013400100513       BEGSR RoutInz;
013500100726
013600100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013700110323
013800110323         clear xx;
013900110323         clear yy;
014000110323
014100110323       //?Carico tabelle causali
014200110323         TBLkut = 1;
014300110323         TBLcod = 'BI';
014400110323         setll (TBLkut:TBLcod) TABEL00F;
014500110323         reade (TBLkut:TBLcod) TABEL00F;
014600110323         DOW  not %eof(TABEL00F);
014700110323           dsBI = TBLuni;
014800110323           IF  §BIatc <> *blanks;
014900110323             xx += 1;
015000110323         //?$BIATC dove memorizzo le causali che generano attività
015100110323         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
015200110323             $BIatc(xx) = %subst(TBLkey:1:3);
015300110323             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
015400110323             %subst($BIatccau(xx):4:2) = §BIatc;
015500110323           ELSE;
015600110323             yy += 1;
015700110323         //?$BINOATC dove memorizzo le causali che NON generano attività
015800110323             $BInoatc(yy) = %subst(TBLkey:1:3);
015900110323           ENDIF;
016000110323           reade (TBLkut:TBLcod) TABEL00F;
016100110323         ENDDO;
016200100413
016300100413       ENDSR;
016400100413
016500100413       //--------------------------------------------------------------
016600110131       //?Elabora.
016700100413       //--------------------------------------------------------------
016800100513       BEGSR Elabora;
016900100413
017000100413         $End = *off;
017100100413
017200110201       //?Estraggo solo i potenziali persi
017300100413         exec sql
017400100726         DECLARE CPO cursor for
017500110211         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt
017600110131         FROM TNCPO00F
017700110131         WHERE CPOfls = 'P'
017800100726         ORDER by CPOcpo;
017900100413
018000100726         exec sql OPEN CPO;
018100100413
018200100413         DOW  not $End;
018300110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
018400110211                                             :CPOFLS, :CPOFLT;
018500100413           IF  sqlcod = 100 or sqlcod < 0;
018600100413             $End = *on;
018700100413             leave;
018800100413           ENDIF;
018900100422
019000110201         //?Controllo se potenziale ha azioni o trattative aperte
019100110201           exsr CtrAtt;
019200110201
019300110203         //?Scrivo nova attività solo se NON trovo attività da eseguire o trattative aperte
019400110203           IF  NrAtt = 0 and NrVis = 0;
019500110201
019600110203           //?Controllo l'ultimo blocco dei clienti collegati
019700110203             exsr CtrBlc;
019800110315
019900110323           //?Controllo se la causale prevede la creazione dell'attività
020000110323           //?Se non la prevede leggo altro codice
020100110323             IF  %lookup(wCauBlc:$BInoatc) > 0;
020200110323               iter;
020300110323             ENDIF;
020400110201
020500110203           //?Genero nuova attività 73 o 74
020600110218             IF  CPOcmm > 0;
020700110218               exsr Scrivi;
020800110218             ENDIF;
020900110203           ENDIF;
021000110201
021100100413         ENDDO;
021200100413
021300100726         exec sql CLOSE CPO;
021400100413
021500100413       ENDSR;
021600110201
021700110201       //--------------------------------------------------------------
021800110201       //?Controllo attività/trattative.
021900110201       //--------------------------------------------------------------
022000110201       BEGSR CtrAtt;
022100110201
022200110201         clear NrAtt;
022300110201         clear NrVis;
022400110201
022500110201         exec sql
022600110201         select count(*) into :NrAtt from tiatc00f
022700110201         where atccpo = :CPOcpo and atcdco = 0;
022800110201
022900110201       //?Se ho trovato attività esco
023000110201         IF  NrAtt > 0;
023100110201           leavesr;
023200110201         ENDIF;
023300110201
023400110201       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
023500110201         exec sql
023600110201         select count(*) into :NrVis from tivis00f
023700110201         where viscpo = :CPOcpo and visdch = 0 and visffz = ' ';
023800110201
023900110201       ENDSR;
024000110201
024100110201       //--------------------------------------------------------------
024200110201       //?Controllo l'ultimo blocco sui clienti.
024300110201       //--------------------------------------------------------------
024400110201       BEGSR CtrBlc;
024500110202
024600110202         clear wCauBlc;
024700110202         clear wDatBlc;
024800110202         clear wOraBlc;
024900110201
025000110201       //?Leggo tutte le anagrafiche legate al codice potenziale
025100110202         setll CPOcpo CNACO16L;
025200110202         reade CPOcpo CNACO16L;
025300110202         DOW  not %eof(CNACO16L);
025400110202           IF  %eof(CNACO16L);
025500110202             leave;
025600110202           ENDIF;
025700110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
025800110202           IF  not %found(CNCLP00F);
025900110202             clear CLPnar;
026000110202           ENDIF;
026100110202
026200110202         //?Cerco la data della variazione
026300110202           $SiAcv = *off;
026400110202           setll (ACOkcc:ACOksc) CNACVT1L;
026500110202           reade (ACOkcc:ACOksc) CNACVT1L;
026600110202           DOW  not %eof(CNACVT1L);
026700110202             IF  %eof(CNACVT1L);
026800110202               leave;
026900110202             ENDIF;
027000110202             IF  ACVcta = 'M' and
027100110202                 %scan('B':ACVflo) > 0;
027200110202               $siAcv = *on;
027300110202               leave;
027400110202             ENDIF;
027500110202             reade (ACOkcc:ACOksc) CNACVT1L;
027600110202           ENDDO;
027700110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
027800110202         //?e memorizzo la causale blocco
027900110202           IF not $SiAcv and wDatBlc = 0;
028000110202             clear ACVdav;
028100110202             clear ACVorv;
028200110202             wDatBlc = 00010101;
028300110202             wOraBlc = 000001;
028400110202             wCauBlc = CLPnar;
028500110202           ENDIF;
028600110201
028700110202         //?se data più alta memorizzo la causale e data/ora variazione
028800110202           IF  ACVdav > wDatBlc or
028900110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
029000110202             wDatBlc = ACVdav;
029100110202             wOraBlc = ACVorv;
029200110202             wCauBlc = CLPnar;
029300110202           ENDIF;
029400110201
029500110202           reade CPOcpo CNACO16L;
029600110201         ENDDO;
029700100413
029800110201       ENDSR;
029900110201
030000100413       //--------------------------------------------------------------
030100100726       //?Scrivo nuova attività.
030200100413       //--------------------------------------------------------------
030300100413       BEGSR Scrivi;
030400100422
030500100422         clear wDad;
030600110201         clear wCad;
030700110323
030800110323       //?Recupero l'attività da creare
030900110323         clear xx;
031000110323         xx = %lookup(wCauBlc:$BIatc);
031100110323         IF  xx > 0;
031200110323           wCad = %subst($BIatccau(xx):4:2);
031300110323         ENDIF;
031400100415
031500100726       //?Stacco nuovo numero attività
031600110131         exsr Stacca_Numero;
031700110131       //?Calcolo la data attività
031800110131         clear XGIOLAVDS;
031900110131         IXGLdata = %dec(%date());
032000110131         IXGLadd  = 'S';
032100110218         IF  wCad = '73';
032200110218           IXGLgg   = wgg73;
032300110218         ENDIF;
032400110218         IF  wCad = '74';
032500110218           IXGLgg   = wgg74;
032600110218         ENDIF;
032700110131         IXGLpa   = 'P';
032800110218         //IXGLlav  = 'S';
032900110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
033000110131         XGIOLAV (xgiolavds);
033100110131         IF  OXGLerr = *blanks;
033200110131           wDad = OXGLdata;
033300110131         ELSE;
033400110131           wDad = %dec(%date());
033500110131         ENDIF;
033600110201
033700110131       //?Scrivo la nuova attività
033800110131         exsr Scrivi_TIATC;
033900100413
034000100413       ENDSR;
034100100415
034200100415       //--------------------------------------------------------------
034300100415       //?Stacco nuovo numero attività.
034400100415       //--------------------------------------------------------------
034500100415       BEGSR Stacca_Numero;
034600100415
034700100415         clear TRUL33ds;
034800100415         I33tla = 'L';
034900100415         I33ope = 0;
035000100415         I33cnu = 070;
035100100415         I33num = 1;
035200100415         I33aaa = *year;
035300100415         kpjbu  = TRUL33ds;
035400100415         trul33r (kpjba);
035500100415         TRUL33ds = kpjbu;
035600100415
035700100415       ENDSR;
035800100415
035900100415       //--------------------------------------------------------------
036000100415       //?Scrivo file TIATC.
036100100415       //--------------------------------------------------------------
036200100415       BEGSR Scrivi_TIATC;
036300100415
036400110211         clear dATC01;
036500100415         clear TIATC000;
036600110131         ATCtat  = 'T';
036700100415         ATCatn  = O33Nrf;
036800100415         ATCatnp = 1;
036900110131         ATCcpo  = CPOcpo;
037000110201         ATCcad  = wCad;
037100110131         ATCdad  = wDad;
037200100422         ATChda  = 103000;
037300110131         ATCcmm  = CPOcmm;
037400110131         ATCco3  = CPOcmm;
037500110131         ATCcnw = 'C';
037600100415         ATCdim  = %dec(%date());
037700100415         ATChim  = %dec(%time());
037800100415         ATCpri  = 'BATCH';
037900110211         §ATCcapo1 = CPOfls;
038000110211         ATCflo = dATC01;
038100100415         write TIATC000;
038200100415
038300100415       ENDSR;
038400100413
038500100413       //--------------------------------------------------------------
038600100413       //?Operazioni finali.
038700100413       //--------------------------------------------------------------
038800100513       BEGSR RoutEnd;
038900100413
039000100413         *inLR = *on;
039100100413         return;
039200100413
039300100413       ENDSR;
039400100413
039500100413      /end-free
