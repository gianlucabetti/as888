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
001700110428      // - File Attività
001800110428     fTIATC05L  if   e           k disk
001900110428      // - File Trattative
002000110502     fTIVIS01L  if   e           k disk    usropn
002100110502     f                                     extfile(wFLib)
002200110204      // - File di controllo
002300110407     fwfcad73   o    e             disk    rename(wfcad73:wfcad7300)
002400110204     f                                     prefix(w_)
002500100413
002600100413      //---------------------------------------------------------------
002700100413      //?Definizione costanti.
002800100413      //---------------------------------------------------------------
002900100413
003000100413      //---------------------------------------------------------------
003100100413      //?Definizione schiere.
003200100413      //---------------------------------------------------------------
003300110407
003400110407      // - Causali che generano attività
003500110407     d $BIatc          s              3    dim(20)
003600110407
003700110407      // - Causali + cod. attività
003800110407     d $BIatccau       s              5    dim(20)
003900110407
004000110407      // - Causali che non generano attività
004100110407     d $BInoatc        s              3    dim(20)
004200090715
004300100413      //---------------------------------------------------------------
004400100413      //?Definizione aree dati.
004500100413      //---------------------------------------------------------------
004600100413
004700100413      //---------------------------------------------------------------
004800100413      //?Definizione strutture dati.
004900100413      //---------------------------------------------------------------
005000110204
005100110204     d KPJBA         e ds
005200100726
005300100726      // File potenziali
005400110204     d TNCPO00F      e ds                  extname(TNCPO00F)
005500110204
005600110204      // - Controllo se potenziale codificato
005700110204     d TRMK05ds      e ds                  inz
005800110407
005900110407      // - Tabella BI - Blocco Causali Clienti
006000110407     d dsBI          e ds                  inz
006100100415
006200100413      //---------------------------------------------------------------
006300100413      //?Definizione variabili globali.
006400100413      //---------------------------------------------------------------
006500100413
006600100413      // - Flags booleani
006700100413     d $End            s               n   inz(*off)
006800110204     d $Fine           s               n   inz(*off)
006900110407     d $SiAcv          s               n   inz(*off)
007000110204
007100110204      // - Indici di schiera
007200110204     d xx              s              4  0 inz
007300110407     d yy              s              3  0
007400100413
007500100413      // - Campi di comodo
007600110407     d NrAtt           s              5i 0
007700110407     d NrVis           s              5i 0
007800110407     d wCauBlc         s                   like(CLPnar)
007900110502     d wDataOggi       s              8  0
008000110502     d wDataISO1       s               d   datfmt(*iso)
008100110502     d wDataISO2       s               d   datfmt(*iso)
008200110407     d wDatBlc         s                   like(ACVdav)
008300110407     d wOraBlc         s                   like(ACVorv)
008400110502
008500110502       // - Nome libreria del file TIVIS
008600110502     d wFLib           s             21    inz
008700100413
008800100413      //---------------------------------------------------------------
008900100413      //?Definizione procedure esterne.
009000100413      //---------------------------------------------------------------
009100110204
009200110204      // - Controllo categoria potenziale
009300110204     d Trmk05R         pr                  extpgm('TRMK05R')
009400110204     d  KPJBA                              likeds(KPJBA)
009500110204     d  TRMK05ds                           likeds(TRMK05ds)
009600100413
009700100413      //---------------------------------------------------------------
009800110204      //?Prototipi.
009900100413      //---------------------------------------------------------------
010000100413
010100100413      //---------------------------------------------------------------
010200100413      //?Definizione key-list.
010300100413      //---------------------------------------------------------------
010400110407
010500110407      // - Dettaglio variazioi anagrafiche
010600110407     d k06cnacvd     e ds                  extname(CNACVD0F:*key)
010700110407     d                                     prefix(k_)
010800110407     d                                     inz
010900100413
011000100413      //---------------------------------------------------------------
011100100413      //?Riepilogo indicatori.
011200100413      //---------------------------------------------------------------
011300100413
011400100413      //---------------------------------------------------------------
011500100413      //?M A I N - L I N E
011600100413      //---------------------------------------------------------------
011700100413
011800100413     c     *Entry        plist
011900110204     c                   parm                    kpjba
012000100413
012100100413      /free
012200100413
012300100413       //?Operazioni iniziali
012400100513       exsr RoutInz;
012500110204
012600110204       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012700100413
012800100726       //?Elaboro
012900100726       exsr Elabora;
013000100413
013100100413       //?Operazioni finali
013200100513       exsr RoutEnd;
013300100413
013400100413       //--------------------------------------------------------------
013500100413       //?Operazioni iniziali.
013600100413       //--------------------------------------------------------------
013700100513       BEGSR RoutInz;
013800110407
013900110407         clear xx;
014000110407         clear yy;
014100110407
014200110407       //?Carico tabelle causali
014300110407         TBLkut = 1;
014400110407         TBLcod = 'BI';
014500110407         setll (TBLkut:TBLcod) TABEL00F;
014600110407         reade (TBLkut:TBLcod) TABEL00F;
014700110407         DOW  not %eof(TABEL00F);
014800110407           dsBI = TBLuni;
014900110407           IF  §BIatc <> *blanks;
015000110407             xx += 1;
015100110407         //?$BIATC dove memorizzo le causali che generano attività
015200110407         //?$BIATCCAU dove memorizzo le causali + codice attività da generare
015300110407             $BIatc(xx) = %subst(TBLkey:1:3);
015400110407             %subst($BIatccau(xx):1:3) = %subst(TBLkey:1:3);
015500110407             %subst($BIatccau(xx):4:2) = §BIatc;
015600110407           ELSE;
015700110407             yy += 1;
015800110407         //?$BINOATC dove memorizzo le causali che NON generano attività
015900110407             $BInoatc(yy) = %subst(TBLkey:1:3);
016000110407           ENDIF;
016100110407           reade (TBLkut:TBLcod) TABEL00F;
016200110407         ENDDO;
016300110502
016400110502         //?Data del giorno
016500110502         wDataOggi = %dec(%date());
016600110502
016700110502         //?Apro il file delle trattative
016800110502         IF  %subst(knsif : 7 : 1) = 'P';
016900110502           wFLib = 'FILTRAGRPF/TIVIS01L';
017000110502         ELSE;
017100110502           wFLib = 'FILTRAGRU/TIVIS01L';
017200110502         ENDIF;
017300110502         open  TIVIS01L;
017400100413
017500100413       ENDSR;
017600100413
017700100413       //--------------------------------------------------------------
017800100422       //?Elaboro.
017900100413       //--------------------------------------------------------------
018000100513       BEGSR Elabora;
018100100413
018200110204         $Fine = *off;
018300100413
018400110204       //?Leggo i potenziali
018500100413         exec sql
018600100726         DECLARE CPO cursor for
018700110204         SELECT  CPOcpo, CPOrag, CPOcmm, CPOftr
018800110428         FROM TNCPO01L
018900100726         ORDER by CPOcpo;
019000100413
019100100726         exec sql OPEN CPO;
019200100413
019300110204         DOW  not $Fine;
019400110204           exec sql FETCH next from CPO into :CPOCPO, :CPORAG,
019500110204                                             :CPOCMM, :CPOFTR;
019600100413           IF  sqlcod = 100 or sqlcod < 0;
019700110204             $Fine = *on;
019800100413             leave;
019900100413           ENDIF;
020000100422
020100110204         //?Controllo la categoria del potenziale
020200110204           exsr Categoria;
020300110204
020400110407           //?Per potenziale persi controllo se devo fare l'attività
020500110407           IF  OMK05cat = 'P';
020600110407             exsr Controlla;
020700110204           ENDIF;
020800100726
020900100413         ENDDO;
021000100413
021100100726         exec sql CLOSE CPO;
021200100413
021300100413       ENDSR;
021400100726
021500100726       //--------------------------------------------------------------
021600110204       //?Controllo la categoria del potenziale.
021700100726       //--------------------------------------------------------------
021800110204       BEGSR Categoria;
021900100726
022000110204         clear TRMK05DS;
022100110204         IMK05cpo = CPOcpo;
022200110204         trmk05r (kpjba: TRMK05DS);
022300110204         IF  OMK05err <> *blanks;
022400110204           clear OMK05cat;
022500110204         ENDIF;
022600100726
022700100726       ENDSR;
022800100413
022900100413       //--------------------------------------------------------------
023000110407       //?Controllo se l'attività è da scrivere.
023100100413       //--------------------------------------------------------------
023200110407       BEGSR Controlla;
023300110207
023400110407         //?Controllo se potenziale ha azioni o trattative aperte
023500110407           exsr CtrAtt;
023600110407
023700110407         //?Scrivo nova attività solo se NON trovo attività da eseguire o trattative aperte
023800110407           IF  NrAtt = 0 and NrVis = 0;
023900110407
024000110407           //?Controllo l'ultimo blocco dei clienti collegati
024100110407             exsr CtrBlc;
024200110407
024300110407           //?Controllo se la causale prevede la creazione dell'attività
024400110407           //?Se non la prevede leggo altro codice
024500110407             IF  %lookup(wCauBlc:$BInoatc) > 0;
024600110407               leavesr;
024700110407             ENDIF;
024800110407
024900110407           //?Genero nuova attività 73 o 74
025000110407             IF  CPOcmm > 0;
025100110407               exsr Scrivi;
025200110407             ENDIF;
025300110407           ENDIF;
025400110204
025500110204       ENDSR;
025600110407
025700110407       //--------------------------------------------------------------
025800110407       //?Controllo attività/trattative.
025900110407       //--------------------------------------------------------------
026000110407       BEGSR CtrAtt;
026100110407
026200110407         clear NrAtt;
026300110407         clear NrVis;
026400110407
026500110428       //?Controllo se ci sono attività da eseguire
026600110428         chain (CPOcpo:0:0) TIATC05L;
026700110428         IF  %found(TIATC05L);
026800110428           NrAtt = 1;
026900110428           leavesr;
027000110428         ENDIF;
027100110428
027200110428       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
027300110428         setll (CPOcpo:0) TIVIS01L;
027400110428         reade (CPOcpo:0) TIVIS01L;
027500110428         DOW not %eof(TIVIS01L);
027600110428           IF  VISffz = ' ';
027700110428             NrVis = 1;
027800110428             leavesr;
027900110428           ENDIF;
028000110428           reade (CPOcpo:0) TIVIS01L;
028100110428         ENDDO;
028200110407
028300110407       ENDSR;
028400110407
028500110407       //--------------------------------------------------------------
028600110407       //?Controllo l'ultimo blocco sui clienti.
028700110407       //--------------------------------------------------------------
028800110407       BEGSR CtrBlc;
028900110407
029000110407         clear wCauBlc;
029100110407         clear wDatBlc;
029200110407         clear wOraBlc;
029300110407
029400110407       //?Leggo tutte le anagrafiche legate al codice potenziale
029500110407         setll CPOcpo CNACO16L;
029600110407         reade CPOcpo CNACO16L;
029700110407         DOW  not %eof(CNACO16L);
029800110407           IF  %eof(CNACO16L);
029900110407             leave;
030000110407           ENDIF;
030100110407           chain (ACOkut: ACOkcc : ACOksc) CNCLP00F;
030200110407           IF  not %found(CNCLP00F);
030300110407             clear CLPnar;
030400110407           ENDIF;
030500110407
030600110407         //?Cerco la data della variazione
030700110407           $SiAcv = *off;
030800110407           setll (ACOkcc:ACOksc) CNACVT1L;
030900110407           reade (ACOkcc:ACOksc) CNACVT1L;
031000110407           DOW  not %eof(CNACVT1L);
031100110407             IF  %eof(CNACVT1L);
031200110407               leave;
031300110407             ENDIF;
031400110407             IF  ACVcta = 'M' and
031500110407                 %scan('B':ACVflo) > 0;
031600110407               $siAcv = *on;
031700110407               leave;
031800110407             ENDIF;
031900110407             reade (ACOkcc:ACOksc) CNACVT1L;
032000110407           ENDDO;
032100110407         //?se non trovo variazioni pulisco i campi della data e ora variazione
032200110407         //?e memorizzo la causale blocco
032300110407           IF not $SiAcv and wDatBlc = 0;
032400110407             clear ACVdav;
032500110407             clear ACVorv;
032600110407             wDatBlc = 00010101;
032700110407             wOraBlc = 000001;
032800110407             wCauBlc = CLPnar;
032900110407           ENDIF;
033000110407
033100110407         //?se data più alta memorizzo la causale e data/ora variazione
033200110407           IF  ACVdav > wDatBlc or
033300110407               (ACVdav = wDatBlc and ACVorv > wOraBlc);
033400110407             wDatBlc = ACVdav;
033500110407             wOraBlc = ACVorv;
033600110407             wCauBlc = CLPnar;
033700110407           ENDIF;
033800110407
033900110407           reade CPOcpo CNACO16L;
034000110407         ENDDO;
034100110407
034200110407       ENDSR;
034300100415
034400100415       //--------------------------------------------------------------
034500110204       //?Scrivo il file.
034600100415       //--------------------------------------------------------------
034700110204       BEGSR Scrivi;
034800100415
034900110407         clear wfcad7300;
035000110204         w_cpocpo = cpocpo;
035100110204         w_cporag = cporag;
035200110204         w_cpocmm = cpocmm;
035300110204         w_cpoftr = cpoftr;
035400110407       //?Recupero l'attività da creare
035500110407         clear xx;
035600110407         xx = %lookup(wCauBlc:$BIatc);
035700110407         IF  xx > 0;
035800110407           w_cau = %subst($BIatccau(xx):4:2);
035900110407         ENDIF;
036000110502
036100110502       //?metto le date in formato ISO
036200110502         wDataISO1 = %date(wDataOggi:*iso);
036300110502         wDataISO2 = %date(wDatBlc:*iso);
036400110502         w_giorni = %editc(%diff(wDataISO1 : wDataISO2 : *days):'X');
036500110207
036600110407         write wfcad7300;
036700100415
036800100415       ENDSR;
036900100413
037000100413       //--------------------------------------------------------------
037100100413       //?Operazioni finali.
037200100413       //--------------------------------------------------------------
037300100513       BEGSR RoutEnd;
037400100413
037500100413         *inLR = *on;
037600100413         return;
037700100413
037800100413       ENDSR;
037900100413
038000100413      /end-free
