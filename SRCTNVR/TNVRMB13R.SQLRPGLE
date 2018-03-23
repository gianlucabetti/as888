000100120423      //---------------------------------------------------------------
000200120423      //
000300131209      //?      Sistema dati non impostati su ORG
000400120423      //
000500120423      //---------------------------------------------------------------
000600120423
000700120423     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800120423
000900120423      //---------------------------------------------------------------
001000120423      //?Dichiarazione file.
001100120423      //---------------------------------------------------------------
001200130314
001300131209     fFNORG01L  uf   e           k disk
001400131209     fFNACR13L  if   e           k disk
001500131209     fFNORS11L  if   e           k disk
001600120423
001700120423      //---------------------------------------------------------------
001800120423      //?Definizione costanti.
001900120423      //---------------------------------------------------------------
002000120423
002100120423      //---------------------------------------------------------------
002200120423      //?Definizione schiere.
002300120423      //---------------------------------------------------------------
002400120423
002500120423      //---------------------------------------------------------------
002600120423      //?Definizione aree dati.
002700120423      //---------------------------------------------------------------
002800120423
002900120423      //---------------------------------------------------------------
003000120423      //?Definizione strutture dati.
003100120423      //---------------------------------------------------------------
003200130214
003300130214       // -?Parametri ricevuti?
003400130214     d kpjba         e ds
003500131209
003600131209      // -?File FNORG00F
003700131209     d FNORG00F      e ds                  extname(FNORG00F)
003800131209     d                                     prefix(w)
003900131209      // -?File FNORM00F
004000131209     d FNORM00F      e ds                  extname(FNORM00F)
004100131209
004200131209      // -?Tabelle
004300131209     d TIBS02DS      e ds
004400131209
004500131209      // -?Tabella AGR
004600131209     d dAGR          e ds
004700131209
004800131209      // -?Tabella DFT
004900131209     d dDFT          e ds
005000120423
005100120423      //---------------------------------------------------------------
005200120423      //?Definizione variabili globali.
005300120423      //---------------------------------------------------------------
005400120423      // - Flags booleani
005500120423     d wEoF            s               n   inz(*off)
005600130430
005700130430      // - Campi di comodo
005800131209     d wACR1ain        s                   like(ACR1ain)
005900131209     d wcgi            s                   like(ORGcgi)
006000131209     d wdata           s              8  0
006100131209     d wpkg            s             10s 1
006200131209     d wvlm            s             10s 3
006300131209     d wvlmaut         s             10s 3
006400131209     d wpkgaut         s             10s 1
006500131209     d wvlmmtc         s             10s 3
006600131209     d wpkgmtc         s             10s 1
006700131209     d wvlmblc         s             10s 3
006800131209     d wpkgblc         s             10s 1
006900120423
007000120423      //---------------------------------------------------------------
007100120423      //?Definizione procedure esterne.
007200120423      //---------------------------------------------------------------
007300120423
007400120423      //---------------------------------------------------------------
007500120423      //?Definizione prototipi.
007600120423      //---------------------------------------------------------------
007700131209      /copy gaitrasrc/srcprotopr,tibs02r
007800120423
007900120423      //---------------------------------------------------------------
008000120423      //?Definizione key-list.
008100120423      //---------------------------------------------------------------
008200120423
008300120423      //---------------------------------------------------------------
008400120423      //?Riepilogo indicatori.
008500120423      //---------------------------------------------------------------
008600120423
008700120423      //---------------------------------------------------------------
008800120423      //?M A I N - L I N E
008900120423      //---------------------------------------------------------------
009000130214
009100130214     c     *Entry        plist
009200130214     c                   parm                    kpjba
009300120423
009400120423      /free
009500120423
009600120423       //?Operazioni iniziali
009700120423       exsr RoutInz;
009800120423
009900120430       //?Elabora file
010000120430       exsr Elabora;
010100120423
010200120423       //?Operazioni finali
010300120423       exsr RoutEnd;
010400120423
010500120423       //--------------------------------------------------------------
010600120423       //?Operazioni iniziali.
010700120423       //--------------------------------------------------------------
010800120423       BEGSR RoutInz;
010900120706
011000130214       //?Impostazione opzioni per SQL?
011100130214         exec sql   set  option  DynUsrPrf = *Owner,
011200130214                                 CloSqlCsr = *EndMod;
011300131209
011400131209       //?Imposto le date
011500131209         wdata = 20131201;
011600120423
011700131209       //?Carico tabella DFT
011800131209           clear TIBS02ds;
011900131209           clear dDFT;
012000131209           T02mod = 'C';
012100131209           T02cod = 'DFT';
012200131209           T02ke1 = '999';
012300131209           T02sif = KNSIF;
012400131209           TNTBE_RicercaControllo (kpjba : tibs02ds);
012500131209           IF  T02err = *blanks;
012600131209             dDFT = T02uni;
012700131209           ENDIF;
012800131209
012900131209           IF  d§DFTpkg = 0;
013000131209             d§DFTpkg = 200;
013100131209           ENDIF;
013200131209
013300131209           IF  d§DFTbnc = 0;
013400131209             d§DFTbnc = 1;
013500131209           ENDIF;
013600131209
013700120423       ENDSR;
013800130204
013900130204       //--------------------------------------------------------------
014000130204       //?Elabora
014100130204       //--------------------------------------------------------------
014200130204       BEGSR elabora;
014300130304
014400130304         wEoF = *off;
014500130304
014600130430       //?Leggo tutte le distinte delle ultime 2 settimane con ORM
014700130304         exec sql
014800131209         DECLARE ORM cursor FOR
014900130314         SELECT *
015000131209         from FNORM00F join FNORG00F
015100131209         ON  ORMpoe = ORGpoe and ORMnsr = ORGnsr and
015200131209             ORMnor = ORGnor and ORMnrv = ORGnrv
015300131209         WHERE  ORMdao >= :wdata and ORMfao < 600
015400131209         ORDER BY ORMpoe, ORMnsr, ORMnor, ORMnrv;
015500130430
015600130304         exec sql
015700131209         open ORM;
015800130304         IF sqlcode < 0;
015900130304           wEoF = *on;
016000130304         ENDIF;
016100130304
016200130304         DOW not wEoF;
016300130304           exec sql
016400131209           FETCH NEXT from ORM into :FNORM00F, :FNORG00F;
016500130304           IF sqlcod = 100 or sqlcod < 0;
016600130304             wEOF = *on;
016700130304             leave;
016800130304           ENDIF;
016900131209
017000131209           exsr pesvol;
017100130430
017200131209           exsr carcgi;
017300131209
017400131209           IF  wpkg <> 0 or wvlm <> 0 or wcgi <> *blanks;
017500131209             chain(e) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
017600131209             IF  %error;
017700131209               iter;
017800131209             ENDIF;
017900131209             IF  %found(FNORG01L);
018000131209               IF  ORGpkg = 0;
018100131209                 ORGpkg = wpkg;
018200131209                 ORGvlm = wvlm;
018300131209               ENDIF;
018400131209               IF  ORGcgi = *blanks;
018500131209                 IF  wcgi <> *blanks;
018600131209                   ORGcgi = wcgi;
018700131209                   ORGpocgi = ORMpor;
018800131209                   ORGtgi = 'A';
018900131209                   ORGdtvtel = 20131209;
019000131209                   ORGhvtel = 120000;
019100131209                 ENDIF;
019200131209               ENDIF;
019300131209               update fnorg000;
019400131209             ENDIF;
019500131209           ENDIF;
019600130304
019700130304         ENDDO;
019800130304
019900130304       //?Chiudo il cursore
020000130304         exec sql
020100131209           close ORM;
020200130204
020300130204       ENDSR;
020400131209
020500131209       //--------------------------------------------------------------
020600131209       //?Calcolo peso e volume
020700131209       //--------------------------------------------------------------
020800131209       BEGSR pesvol;
020900131209
021000131209         clear wvlmaut;
021100131209         clear wpkgaut;
021200131209         clear wvlmmtc;
021300131209         clear wpkgmtc;
021400131209         clear wvlmblc;
021500131209         clear wpkgblc;
021600131209         clear wpkg;
021700131209         clear wvlm;
021800131209
021900131209         IF  wORGpkg > 0 or wORGvlm > 0;
022000131209           leavesr;
022100131209         ENDIF;
022200131209
022300131209         wpkg = ORMpkg;
022400131209         wvlm = ORMvlm;
022500131209
022600131209       //?se peso non presente su ORG lo calcolo
022700131209         IF  wpkg = 0;
022800131209           SELECT;
022900131209             WHEN  ORMbnc <> 0;
023000131209               wpkg = ORMbnc / d§DFTbnc * d§DFTpkg;
023100131209             WHEN  ORMvlm <> 0;
023200131209               wpkg = ORMvlm * d§DFTpkg;
023300131209           ENDSL;
023400131209           IF  wpkg > 999999,9;
023500131209             wpkg = 999999,9;
023600131209           ENDIF;
023700131209         ENDIF;
023800131209       //?se volume non presente su ORG lo calcolo
023900131209         IF  wvlm = 0;
024000131209           SELECT;
024100131209             WHEN  ORMbnc <> 0;
024200131209               eval(h) wvlm = ORMbnc / d§DFTbnc;
024300131209             WHEN  ORMpkg <> 0;
024400131209               eval(h) wvlm = ORMpkg / d§DFTpkg;
024500131209           ENDSL;
024600131209           IF  wvlm > 99,999;
024700131209             wvlm = 99,990;
024800131209           ENDIF;
024900131209         ENDIF;
025000131209
025100131209       //?se inseriti gli automezzi devo convertirli in m³ per poi memorizzare
025200131209       //?il dato più alto su FNORG
025300131209
025400131209       //?AUTOTRENO
025500131209         IF  ORMatt > 0;
025600131209           wvlmaut = ORMatt * d§DFTaut;
025700131209           wpkgaut = wvlmaut * d§DFTpkg;
025800131209           IF  wvlmaut > 99,999;
025900131209             wvlmaut = 99,999;
026000131209           ENDIF;
026100131209           IF  wpkgaut > 999999,9;
026200131209             wpkgaut = 999999,9;
026300131209           ENDIF;
026400131209         ENDIF;
026500131209
026600131209       //?MOTRICE
026700131209         IF  ORMmtc > 0;
026800131209           wvlmmtc = ORMmtc * d§DFTmtc;
026900131209           wpkgmtc = wvlmmtc * d§DFTpkg;
027000131209           IF  wvlmmtc > 99,999;
027100131209             wvlmmtc = 99,999;
027200131209           ENDIF;
027300131209           IF  wpkgmtc > 999999,9;
027400131209             wpkgmtc = 999999,9;
027500131209           ENDIF;
027600131209         ENDIF;
027700131209
027800131209       //?BILICO
027900131209         IF  ORMblc > 0;
028000131209           wvlmblc = ORMblc * d§DFTblc;
028100131209           wpkgblc = wvlmblc * d§DFTpkg;
028200131209           IF  wvlmblc > 99,999;
028300131209             wvlmblc = 99,999;
028400131209           ENDIF;
028500131209           IF  wpkgblc > 999999,9;
028600131209             wpkgblc = 999999,9;
028700131209           ENDIF;
028800131209         ENDIF;
028900131209
029000131209       //?cerco il volume più alto
029100131209         IF  wvlmaut > wvlm;
029200131209           wvlm = wvlmaut;
029300131209         ENDIF;
029400131209         IF  wvlmmtc > wvlm;
029500131209           wvlm = wvlmmtc;
029600131209         ENDIF;
029700131209         IF  wvlmblc > wvlm;
029800131209           wvlm = wvlmblc;
029900131209         ENDIF;
030000131209
030100131209       //?cerco il peso più alto
030200131209         IF  wpkgaut > wpkg;
030300131209           wpkg = wpkgaut;
030400131209         ENDIF;
030500131209         IF  wpkgmtc > wpkg;
030600131209           wpkg = wpkgmtc;
030700131209         ENDIF;
030800131209         IF  wpkgblc > wpkg;
030900131209           wpkg = wpkgblc;
031000131209         ENDIF;
031100131209
031200131209       ENDSR;
031300131209
031400131209       //--------------------------------------------------------------
031500131209       //?Carico giro
031600131209       //--------------------------------------------------------------
031700131209       BEGSR carcgi;
031800131209
031900131209         clear wcgi;
032000131209
032100131209       //?Se presente il mittente aggancio l'anagrafica giri del mittente
032200131209         IF  ORMcra > 0;
032300131209           wACR1ain = '=';
032400131209         //?Carico tabella AGR
032500131209           clear TIBS02ds;
032600131209           clear dAGR;
032700131209           T02mod = 'C';
032800131209           T02cod = 'AGR';
032900131209           T02ke1 = %editc(ORMpor:'X');
033000131209           T02sif = KNSIF;
033100131209           TNTBE_RicercaControllo (kpjba : tibs02ds);
033200131209           IF  T02err = *blanks;
033300131209             dAGR = T02uni;
033400131209             IF  wpkg > §AGRpkgo or wvlm > §AGRvlmo or ORMbnc > §AGRbnco;
033500131209               wACR1ain = '>';
033600131209             ENDIF;
033700131209             IF  wpkg < §AGRpkgs and wvlm < §AGRvlms and ORMbnc < §AGRbncs;
033800131209               wACR1ain = '<';
033900131209             ENDIF;
034000131209           ENDIF;
034100131209         //?Aggancio Anagrafica
034200131209           chain (ORMcra:ORMpor:wACR1ain) FNACR13L;
034300131209           IF  %found(FNACR13L) and ACR1cgi <> *blanks;
034400131209             wcgi = ACR1cgi;
034500131209           ELSE;
034600131209             IF  wACR1ain <> '=';
034700131209               wACR1ain = '=';
034800131209               chain (ORMcra:ORMpor:wACR1ain) FNACR13L;
034900131209               IF  %found(FNACR13L) and ACR1cgi <> *blanks;
035000131209                 wcgi = ACR1cgi;
035100131209               ENDIF;
035200131209             ENDIF;
035300131209           ENDIF;
035400131209
035500131209       //?Se ritiro fisso da giri ritiri fissi
035600131209           IF  ORMtco = 'S';
035700131209             chain  (ORMpos:ORMors) FNORS11L;
035800131209             IF  %found(FNORS11L) and ORS1cgi <> *blanks;
035900131209               wcgi = ORS1cgi;
036000131209             ENDIF;
036100131209           ENDIF;
036200131209
036300131209         ENDIF;
036400131209
036500131209       ENDSR;
036600130125
036700120423       //--------------------------------------------------------------
036800120423       //?Operazioni finali.
036900120423       //--------------------------------------------------------------
037000120423       BEGSR RoutEnd;
037100120423
037200120423         *inLR = *on;
037300120423         return;
037400120423
037500120423       ENDSR;
037600051221
037700051221      /end-free
037800051221
