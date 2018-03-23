000100100413      //---------------------------------------------------------------
000200110407      //?TNVRCRM81 - Genero attività 73/74
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000110207      // - File Anagrafica clienti
001100110207     fCNACO16L  if   e           k disk
001200110207     fCNCLP00F  if   e           k disk
001300110407      // - File Variazioni anagrafiche
001400110407     fCNACVT1L  if   e           k disk
001500110407      // - File Tabelle
001600110407     fTABEL00F  if   e           k disk
001700110204      // - File di controllo
001800110407     fwfcad73   o    e             disk    rename(wfcad73:wfcad7300)
001900110204     f                                     prefix(w_)
002000100413
002100100413      //---------------------------------------------------------------
002200100413      //?Definizione costanti.
002300100413      //---------------------------------------------------------------
002400100413
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione schiere.
002700100413      //---------------------------------------------------------------
002800110407
002900110407      // - Causali che generano attività
003000110407     d $BIatc          s              3    dim(20)
003100110407
003200110407      // - Causali + cod. attività
003300110407     d $BIatccau       s              5    dim(20)
003400110407
003500110407      // - Causali che non generano attività
003600110407     d $BInoatc        s              3    dim(20)
003700090715
003800100413      //---------------------------------------------------------------
003900100413      //?Definizione aree dati.
004000100413      //---------------------------------------------------------------
004100100413
004200100413      //---------------------------------------------------------------
004300100413      //?Definizione strutture dati.
004400100413      //---------------------------------------------------------------
004500110204
004600110204     d KPJBA         e ds
004700100726
004800100726      // File potenziali
004900110204     d TNCPO00F      e ds                  extname(TNCPO00F)
005000110204
005100110204      // - Controllo se potenziale codificato
005200110204     d TRMK05ds      e ds                  inz
005300110407
005400110407      // - Tabella BI - Blocco Causali Clienti
005500110407     d dsBI          e ds                  inz
005600100415
005700100413      //---------------------------------------------------------------
005800100413      //?Definizione variabili globali.
005900100413      //---------------------------------------------------------------
006000100413
006100100413      // - Flags booleani
006200100413     d $End            s               n   inz(*off)
006300110204     d $Fine           s               n   inz(*off)
006400110407     d $SiAcv          s               n   inz(*off)
006500110204
006600110204      // - Indici di schiera
006700110204     d xx              s              4  0 inz
006800110407     d yy              s              3  0
006900100413
007000100413      // - Campi di comodo
007100110407     d NrAtt           s              5i 0
007200110407     d NrVis           s              5i 0
007300110407     d wCauBlc         s                   like(CLPnar)
007400110407     d wDatBlc         s                   like(ACVdav)
007500110407     d wOraBlc         s                   like(ACVorv)
007600100413
007700100413      //---------------------------------------------------------------
007800100413      //?Definizione procedure esterne.
007900100413      //---------------------------------------------------------------
008000110204
008100110204      // - Controllo categoria potenziale
008200110204     d Trmk05R         pr                  extpgm('TRMK05R')
008300110204     d  KPJBA                              likeds(KPJBA)
008400110204     d  TRMK05ds                           likeds(TRMK05ds)
008500100413
008600100413      //---------------------------------------------------------------
008700110204      //?Prototipi.
008800100413      //---------------------------------------------------------------
008900100413
009000100413      //---------------------------------------------------------------
009100100413      //?Definizione key-list.
009200100413      //---------------------------------------------------------------
009300110407
009400110407      // - Dettaglio variazioi anagrafiche
009500110407     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
009600110407     d                                     prefix(k_)
009700110407     d                                     inz
009800100413
009900100413      //---------------------------------------------------------------
010000100413      //?Riepilogo indicatori.
010100100413      //---------------------------------------------------------------
010200100413
010300100413      //---------------------------------------------------------------
010400100413      //?M A I N - L I N E
010500100413      //---------------------------------------------------------------
010600100413
010700100413     c     *Entry        plist
010800110204     c                   parm                    kpjba
010900100413
011000100413      /free
011100100413
011200100413       //?Operazioni iniziali
011300100513       exsr RoutInz;
011400110204
011500110204       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011600100413
011700100726       //?Elaboro
011800100726       exsr Elabora;
011900100413
012000100413       //?Operazioni finali
012100100513       exsr RoutEnd;
012200100413
012300100413       //--------------------------------------------------------------
012400100413       //?Operazioni iniziali.
012500100413       //--------------------------------------------------------------
012600100513       BEGSR RoutInz;
012700110407
012800110407         clear xx;
012900110407         clear yy;
013000110407
013100110407       //?Carico tabelle causali
013200110407         TBLkut = 1;
013300110407         TBLcod = 'BI';
013400110407         setll (TBLkut:TBLcod) TABEL00F;
013500110407         reade (TBLkut:TBLcod) TABEL00F;
013600110407         DOW  not %eof(TABEL00F);
013700110407           dsBI = TBLuni;
013800110407           IF  §BIatc <> *blanks;
013900110407             xx += 1;
014000110407         //?$BIATC dove memorizzo le causali che generano attività
014100110407         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
014200110407             $BIatc(xx) = %subst(TBLkey:1:3);
014300110407             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
014400110407             %subst($BIatccau(xx):4:2) = §BIatc;
014500110407           ELSE;
014600110407             yy += 1;
014700110407         //?$BINOATC dove memorizzo le causali che NON generano attività
014800110407             $BInoatc(yy) = %subst(TBLkey:1:3);
014900110407           ENDIF;
015000110407           reade (TBLkut:TBLcod) TABEL00F;
015100110407         ENDDO;
015200100413
015300100413       ENDSR;
015400100413
015500100413       //--------------------------------------------------------------
015600100422       //?Elaboro.
015700100413       //--------------------------------------------------------------
015800100513       BEGSR Elabora;
015900100413
016000110204         $Fine = *off;
016100100413
016200110204       //?Leggo i potenziali
016300100413         exec sql
016400100726         DECLARE CPO cursor for
016500110204         SELECT  CPOcpo, CPOrag, CPOcmm, CPOftr
016600110204         FROM TNCPO00F
016700100726         ORDER by CPOcpo;
016800100413
016900100726         exec sql OPEN CPO;
017000100413
017100110204         DOW  not $Fine;
017200110204           exec sql FETCH next from CPO into :CPOCPO, :CPORAG,
017300110204                                             :CPOCMM, :CPOFTR;
017400100413           IF  sqlcod = 100 or sqlcod < 0;
017500110204             $Fine = *on;
017600100413             leave;
017700100413           ENDIF;
017800100422
017900110204         //?Controllo la categoria del potenziale
018000110204           exsr Categoria;
018100110204
018200110407           //?Per potenziale persi controllo se devo fare l'attività
018300110407           IF  OMK05cat = 'P';
018400110407             exsr Controlla;
018500110204           ENDIF;
018600100726
018700100413         ENDDO;
018800100413
018900100726         exec sql CLOSE CPO;
019000100413
019100100413       ENDSR;
019200100726
019300100726       //--------------------------------------------------------------
019400110204       //?Controllo la categoria del potenziale.
019500100726       //--------------------------------------------------------------
019600110204       BEGSR Categoria;
019700100726
019800110204         clear TRMK05DS;
019900110204         IMK05cpo = CPOcpo;
020000110204         trmk05r (kpjba: TRMK05DS);
020100110204         IF  OMK05err <> *blanks;
020200110204           clear OMK05cat;
020300110204         ENDIF;
020400100726
020500100726       ENDSR;
020600100413
020700100413       //--------------------------------------------------------------
020800110407       //?Controllo se l'attività è da scrivere.
020900100413       //--------------------------------------------------------------
021000110407       BEGSR Controlla;
021100110207
021200110407         //?Controllo se potenziale ha azioni o trattative aperte
021300110407           exsr CtrAtt;
021400110407
021500110407         //?Scrivo nova attività solo se NON trovo attività da eseguire o trattative aperte
021600110407           IF  NrAtt = 0 and NrVis = 0;
021700110407
021800110407           //?Controllo l'ultimo blocco dei clienti collegati
021900110407             exsr CtrBlc;
022000110407
022100110407           //?Controllo se la causale prevede la creazione dell'attività
022200110407           //?Se non la prevede leggo altro codice
022300110407             IF  %lookup(wCauBlc:$BInoatc) > 0;
022400110407               leavesr;
022500110407             ENDIF;
022600110407
022700110407           //?Genero nuova attività 73 o 74
022800110407             IF  CPOcmm > 0;
022900110407               exsr Scrivi;
023000110407             ENDIF;
023100110407           ENDIF;
023200110204
023300110204       ENDSR;
023400110407
023500110407       //--------------------------------------------------------------
023600110407       //?Controllo attività/trattative.
023700110407       //--------------------------------------------------------------
023800110407       BEGSR CtrAtt;
023900110407
024000110407         clear NrAtt;
024100110407         clear NrVis;
024200110407
024300110407         exec sql
024400110407         select count(*) into :NrAtt from tiatc00f
024500110407         where atccpo = :CPOcpo and atcdco = 0;
024600110407
024700110407       //?Se ho trovato attività esco
024800110407         IF  NrAtt > 0;
024900110407           leavesr;
025000110407         ENDIF;
025100110407
025200110407       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
025300110407         exec sql
025400110407         select count(*) into :NrVis from tivis00f
025500110407         where viscpo = :CPOcpo and visdch = 0 and visffz = ' ';
025600110407
025700110407       ENDSR;
025800110407
025900110407       //--------------------------------------------------------------
026000110407       //?Controllo l'ultimo blocco sui clienti.
026100110407       //--------------------------------------------------------------
026200110407       BEGSR CtrBlc;
026300110407
026400110407         clear wCauBlc;
026500110407         clear wDatBlc;
026600110407         clear wOraBlc;
026700110407
026800110407       //?Leggo tutte le anagrafiche legate al codice potenziale
026900110407         setll CPOcpo CNACO16L;
027000110407         reade CPOcpo CNACO16L;
027100110407         DOW  not %eof(CNACO16L);
027200110407           IF  %eof(CNACO16L);
027300110407             leave;
027400110407           ENDIF;
027500110407           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
027600110407           IF  not %found(CNCLP00F);
027700110407             clear CLPnar;
027800110407           ENDIF;
027900110407
028000110407         //?Cerco la data della variazione
028100110407           $SiAcv = *off;
028200110407           setll (ACOkcc:ACOksc) CNACVT1L;
028300110407           reade (ACOkcc:ACOksc) CNACVT1L;
028400110407           DOW  not %eof(CNACVT1L);
028500110407             IF  %eof(CNACVT1L);
028600110407               leave;
028700110407             ENDIF;
028800110407             IF  ACVcta = 'M' and
028900110407                 %scan('B':ACVflo) > 0;
029000110407               $siAcv = *on;
029100110407               leave;
029200110407             ENDIF;
029300110407             reade (ACOkcc:ACOksc) CNACVT1L;
029400110407           ENDDO;
029500110407         //?se non trovo variazioni pulisco i campi della data e ora variazione
029600110407         //?e memorizzo la causale blocco
029700110407           IF not $SiAcv and wDatBlc = 0;
029800110407             clear ACVdav;
029900110407             clear ACVorv;
030000110407             wDatBlc = 00010101;
030100110407             wOraBlc = 000001;
030200110407             wCauBlc = CLPnar;
030300110407           ENDIF;
030400110407
030500110407         //?se data più alta memorizzo la causale e data/ora variazione
030600110407           IF  ACVdav > wDatBlc or
030700110407               (ACVdav = wDatBlc and ACVorv > wOraBlc);
030800110407             wDatBlc = ACVdav;
030900110407             wOraBlc = ACVorv;
031000110407             wCauBlc = CLPnar;
031100110407           ENDIF;
031200110407
031300110407           reade CPOcpo CNACO16L;
031400110407         ENDDO;
031500110407
031600110407       ENDSR;
031700100415
031800100415       //--------------------------------------------------------------
031900110204       //?Scrivo il file.
032000100415       //--------------------------------------------------------------
032100110204       BEGSR Scrivi;
032200100415
032300110407         clear wfcad7300;
032400110204         w_cpocpo = cpocpo;
032500110204         w_cporag = cporag;
032600110204         w_cpocmm = cpocmm;
032700110204         w_cpoftr = cpoftr;
032800110407       //?Recupero l'attività da creare
032900110407         clear xx;
033000110407         xx = %lookup(wCauBlc:$BIatc);
033100110407         IF  xx > 0;
033200110407           w_cau = %subst($BIatccau(xx):4:2);
033300110407         ENDIF;
033400110207
033500110407         write wfcad7300;
033600100415
033700100415       ENDSR;
033800100413
033900100413       //--------------------------------------------------------------
034000100413       //?Operazioni finali.
034100100413       //--------------------------------------------------------------
034200100513       BEGSR RoutEnd;
034300100413
034400100413         *inLR = *on;
034500100413         return;
034600100413
034700100413       ENDSR;
034800100413
034900100413      /end-free
