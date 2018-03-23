000100100413      //---------------------------------------------------------------
000200110513      //?TRMK36R1 - Crea attività 73/74 su potenziali persi    SPOT
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
001800110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
001900110428      // - File Trattative
002000111221     fTIVIS01L  if   e           k disk
002100110516      // - File di controllo
002200110516     fwfcad73t  o    e             disk    rename(wfcad73t:wfcad7300)
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
005700110131
005800110131      // - Aggiunge/Toglie gg/mm dalla data
005900110131     d XGIOLAVds     e ds                  inz
006000110323
006100110323      // - Tabella BI - Blocco Causali Clienti
006200110323     d dsBI          e ds                  inz
006300100415
006400100413      //---------------------------------------------------------------
006500100413      //?Definizione variabili globali.
006600100413      //---------------------------------------------------------------
006700100413
006800100413      // - Flags booleani
006900100413     d $End            s               n   inz(*off)
007000110201     d $SiAcv          s               n   inz(*off)
007100110323
007200110323      // - Indici di schiera
007300110323     d xx              s              3  0
007400110323     d yy              s              3  0
007500110131
007600110131      // - Campi di comodo
007700110201     d NrAtt           s              5i 0
007800110201     d NrVis           s              5i 0
007900110201     d wCad            s                   like(ATCcad)
008000110201     d wCauBlc         s                   like(CLPnar)
008100110131     d wDad            s                   like(ATCdad)
008200110201     d wDatBlc         s                   like(ACVdav)
008300110201     d wOraBlc         s                   like(ACVorv)
008400110218     d wgg73           s                   like(IXGLgg) inz(90)
008500110218     d wgg74           s                   like(IXGLgg) inz(365)
008600100413
008700100413      //---------------------------------------------------------------
008800100413      //?Definizione procedure esterne.
008900100413      //---------------------------------------------------------------
009000110131
009100110131      // - Aggiunge/Toglie gg/mm dalla data
009200110131     d Xgiolav         pr                  extpgm('XGIOLAV')
009300110131     d  xgiolavds                          likeds(xgiolavds)
009400100413
009500100413      //---------------------------------------------------------------
009600110131      //?Prototipi
009700100413      //---------------------------------------------------------------
009800100414
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
017500110516         SELECT  CPOcpo, CPOcmm, CPOfls, CPOflt, CPOftr
017600110428         FROM TNCPO01L
017700110131         WHERE CPOfls = 'P'
017800100726         ORDER by CPOcpo;
017900100413
018000100726         exec sql OPEN CPO;
018100100413
018200100413         DOW  not $End;
018300110211           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM,
018400111221                                             :CPOFLS, :CPOFLT;
018500100413           IF  sqlcod = 100 or sqlcod < 0;
018600100413             $End = *on;
018700100413             leave;
018800100413           ENDIF;
018900100422
019000110201         //?Controllo se potenziale ha azioni o trattative aperte
019100110201           exsr CtrAtt;
019200110201
019300110513         //?Scrivo nuova attività solo se NON trovo attività da eseguire o trattative aperte
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
020600110513           //?Ma solo se c'è il codice del commerciale
020700110218             IF  CPOcmm > 0;
020800110218               exsr Scrivi;
020900110218             ENDIF;
021000110203           ENDIF;
021100110201
021200100413         ENDDO;
021300100413
021400100726         exec sql CLOSE CPO;
021500100413
021600100413       ENDSR;
021700110201
021800110201       //--------------------------------------------------------------
021900110201       //?Controllo attività/trattative.
022000110201       //--------------------------------------------------------------
022100110201       BEGSR CtrAtt;
022200110201
022300110201         clear NrAtt;
022400110201         clear NrVis;
022500110201
022600110428       //?Controllo se ci sono attività da eseguire
022700110428         chain (CPOcpo:0:0) TIATC05L;
022800110428         IF  %found(TIATC05L);
022900110428           NrAtt = 1;
023000110428           leavesr;
023100110428         ENDIF;
023200110428
023300110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
023400110428         setll (CPOcpo:0) TIVIS01L;
023500110428         reade (CPOcpo:0) TIVIS01L;
023600110428         DOW not %eof(TIVIS01L);
023700110428           IF  VISffz = ' ';
023800110428             NrVis = 1;
023900110428             leavesr;
024000110428           ENDIF;
024100110428           reade (CPOcpo:0) TIVIS01L;
024200110428         ENDDO;
024300110201
024400110201       ENDSR;
024500110201
024600110201       //--------------------------------------------------------------
024700110201       //?Controllo l'ultimo blocco sui clienti.
024800110201       //--------------------------------------------------------------
024900110201       BEGSR CtrBlc;
025000110202
025100110202         clear wCauBlc;
025200110202         clear wDatBlc;
025300110202         clear wOraBlc;
025400110201
025500110201       //?Leggo tutte le anagrafiche legate al codice potenziale
025600110202         setll CPOcpo CNACO16L;
025700110202         reade CPOcpo CNACO16L;
025800110202         DOW  not %eof(CNACO16L);
025900110202           IF  %eof(CNACO16L);
026000110202             leave;
026100110202           ENDIF;
026200110202           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
026300110202           IF  not %found(CNCLP00F);
026400110202             clear CLPnar;
026500110202           ENDIF;
026600110202
026700110202         //?Cerco la data della variazione
026800110202           $SiAcv = *off;
026900110202           setll (ACOkcc:ACOksc) CNACVT1L;
027000110202           reade (ACOkcc:ACOksc) CNACVT1L;
027100110202           DOW  not %eof(CNACVT1L);
027200110202             IF  %eof(CNACVT1L);
027300110202               leave;
027400110202             ENDIF;
027500110202             IF  ACVcta = 'M' and
027600110202                 %scan('B':ACVflo) > 0;
027700110202               $siAcv = *on;
027800110202               leave;
027900110202             ENDIF;
028000110202             reade (ACOkcc:ACOksc) CNACVT1L;
028100110202           ENDDO;
028200110202         //?se non trovo variazioni pulisco i campi della data e ora variazione
028300110202         //?e memorizzo la causale blocco
028400110202           IF not $SiAcv and wDatBlc = 0;
028500110202             clear ACVdav;
028600110202             clear ACVorv;
028700110202             wDatBlc = 00010101;
028800110202             wOraBlc = 000001;
028900110202             wCauBlc = CLPnar;
029000110202           ENDIF;
029100110201
029200110202         //?se data più alta memorizzo la causale e data/ora variazione
029300110202           IF  ACVdav > wDatBlc or
029400110202               (ACVdav = wDatBlc and ACVorv > wOraBlc);
029500110202             wDatBlc = ACVdav;
029600110202             wOraBlc = ACVorv;
029700110202             wCauBlc = CLPnar;
029800110202           ENDIF;
029900110201
030000110202           reade CPOcpo CNACO16L;
030100110201         ENDDO;
030200100413
030300110201       ENDSR;
030400110201
030500100413       //--------------------------------------------------------------
030600100726       //?Scrivo nuova attività.
030700100413       //--------------------------------------------------------------
030800100413       BEGSR Scrivi;
030900100422
031000100422         clear wDad;
031100110201         clear wCad;
031200110323
031300110323       //?Recupero l'attività da creare
031400110323         clear xx;
031500110323         xx = %lookup(wCauBlc:$BIatc);
031600110323         IF  xx > 0;
031700110323           wCad = %subst($BIatccau(xx):4:2);
031800110323         ENDIF;
031900110513
032000110131       //?Calcolo la data attività
032100110131         clear XGIOLAVDS;
032200110131         IXGLdata = %dec(%date());
032300110131         IXGLadd  = 'S';
032400110218         IF  wCad = '73';
032500110218           IXGLgg   = wgg73;
032600110218         ENDIF;
032700110218         IF  wCad = '74';
032800110218           IXGLgg   = wgg74;
032900110218         ENDIF;
033000110131         IXGLpa   = 'P';
033100110218         //IXGLlav  = 'S';
033200110218         IXGLfil  = %dec(%subst(%editc(CPOcmm:'X'):1:3):3:0);
033300110131         XGIOLAV (xgiolavds);
033400110131         IF  OXGLerr = *blanks;
033500110131           wDad = OXGLdata;
033600110131         ELSE;
033700110131           wDad = %dec(%date());
033800110131         ENDIF;
033900110201
034000110131       //?Scrivo la nuova attività
034100110131         exsr Scrivi_TIATC;
034200100413
034300100413       ENDSR;
034400100415
034500100415       //--------------------------------------------------------------
034600100415       //?Scrivo file TIATC.
034700100415       //--------------------------------------------------------------
034800100415       BEGSR Scrivi_TIATC;
034900100415
035000110211         clear dATC01;
035100110516         clear wfcad7300;
035200110131         ATCcpo  = CPOcpo;
035300110201         ATCcad  = wCad;
035400110131         ATCdad  = wDad;
035500100422         ATChda  = 103000;
035600110131         ATCcmm  = CPOcmm;
035700100415         ATCdim  = %dec(%date());
035800100415         ATChim  = %dec(%time());
035900100415         ATCpri  = 'BATCH';
036000110516         write WFCAD7300;
036100100415
036200100415       ENDSR;
036300100413
036400100413       //--------------------------------------------------------------
036500100413       //?Operazioni finali.
036600100413       //--------------------------------------------------------------
036700100513       BEGSR RoutEnd;
036800100413
036900100413         *inLR = *on;
037000100413         return;
037100100413
037200100413       ENDSR;
037300100413
037400100413      /end-free
