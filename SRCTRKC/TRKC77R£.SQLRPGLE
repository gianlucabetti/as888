000100141106      //--------------------------------------------------------------
000200150211      //?TRKC75R - MODIFICA CAMPAGNA PER CODICE UNIFICANTE
000300141106      //--------------------------------------------------------------
000400141106
000500141106     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141106
000700141106      //---------------------------------------------------------------
000800141106      //?Dichiarazione file.
000900141106      //---------------------------------------------------------------
001000141114
001100141114      // - Anagrafica Campagna
001200141114     fTICMP01L  if   e           k disk
001300150211
001400150211      // - Anagrafica Potenziali
001500150211     fTNCPO01L  if   e           k disk
001600141106
001700141106      //---------------------------------------------------------------
001800141106      //?Definizione costanti.
001900141106      //---------------------------------------------------------------
002000141106
002100141106      //---------------------------------------------------------------
002200141106      //?Definizione schiere.
002300141106      //---------------------------------------------------------------
002400141106
002500141106      // - Messaggi di errore
002600141106     d Msg             s             78    dim(20) ctdata perrcd(1)
002700141106
002800141106      //---------------------------------------------------------------
002900141106      //?Definizione aree dati.
003000141106      //---------------------------------------------------------------
003100141106
003200141106      // - Dati utente
003300141106     d §AzUte        e ds                  extname(AZUTE00F)
003400141106     d                                     dtaara
003500141106     d §DatiUte      e ds                  extname(dDatiUte)
003600141106     d                                     dtaara
003700141106
003800141106      //---------------------------------------------------------------
003900141106      //?Definizione strutture dati.
004000141106      //---------------------------------------------------------------
004100141106
004200141106      // - Status
004300141106     d Psds           sds
004400141106     d   SDSpgm          *proc
004500141106
004600141106      // - Parametri ricevuti
004700141106     d KPJBA         e ds
004800150211     d TRKC75DS      e ds
004900141106
005000141106      // - Reperimento dati utente
005100141106     d TIBS34DS      e ds
005200150211
005300150211      // - Reperimento dati Anagrafica Clienti
005400150211      /copy gaitrasrc/srcprotopi,TIBS69R
005500150211
005600150211      // - File Clienti in Campagna Commerciale
005700150211     d TICMC00F      e ds                  extname(TICMC00F)
005800150211
005900150211      // - File INFO Clienti in Campagna Commerciale
006000150211     d TICMI00F      e ds                  extname(TICMI00F)
006100150211
006200150211      // - File Fasi Clienti in Campagna Commerciale
006300150211     d TICMF00F      e ds                  extname(TICMF00F)
006400150211
006500150211      // - File Note Clienti in Campagna Commerciale
006600150211     d TICMN00F      e ds                  extname(TICMN00F)
006700150211
006800141106      //---------------------------------------------------------------
006900141106      //?Definizione variabili globali.
007000141106      //---------------------------------------------------------------
007100141106
007200141106      // - Campi di comodo data
007300141106     d Data_EUR        s               d   datfmt(*eur)
007400141106     d Data_ISO        s               d   datfmt(*iso)
007500141106
007600141106      // - Campi di comodo
007700141106     d Oggi            s              8s 0 inz
007800141106
007900141106      //---------------------------------------------------------------
008000141106      //?Definizione procedure usate.
008100141106      //---------------------------------------------------------------
008200141106
008300141106      //---------------------------------------------------------------
008400141106      //?Definizione Prototipi.
008500141106      //---------------------------------------------------------------
008600141106      /copy gaitrasrc/srcprotopr,TIBS34R
008700150211      /copy gaitrasrc/srcprotopr,TIBS69R
008800141106
008900141106      //---------------------------------------------------------------
009000141106      //?Definizione key-list.
009100141106      //---------------------------------------------------------------
009200141106
009300141106      //---------------------------------------------------------------
009400141106      //?M A I N - L I N E
009500141106      //---------------------------------------------------------------
009600141106
009700141106     c     *Entry        plist
009800141106     c                   parm                    kpjba
009900150211     c                   parm                    TRKC75DS
010000141106
010100141106      /free
010200141106
010300141106       //?Operazioni iniziali
010400141106       exsr RoutInz;
010500141106
010600141106       //?Controllo dati passati
010700141106       exsr Controlla;
010800141106
010900150211       //?Aggiorna la campagna sui vari archivi
011000150211       exsr Aggiorna;
011100141106
011200141106       //?Operazioni finali
011300141106       exsr RoutEnd;
011400141106
011500141106       //--------------------------------------------------------------
011600141106       //?Operazioni iniziali.
011700141106       //--------------------------------------------------------------
011800141106       BEGSR RoutInz;
011900141106
012000141106       //?Imposto oggi
012100141106         Oggi = %dec(%date());
012200141106
012300141106       //?Reperimento dati job
012400141106         exsr DatiJob;
012500141106
012600150211         clear OKC75err;
012700150211         clear OKC75msg;
012800141106
012900141106       ENDSR;
013000141106
013100141106       //--------------------------------------------------------------
013200141106       //?Reperimento Dati del job (Utente/Operativi).
013300141106       //--------------------------------------------------------------
013400141106       BEGSR DatiJob;
013500141106
013600141106         in(E) §AzUte;
013700141106         IF  NOT %error;
013800141106           in(E) §DatiUte;
013900141106         ENDIF;
014000141106         IF  %error or RSut = *blanks;
014100141106           clear TIBS34ds;
014200141106           tibs34r(tibs34ds);
014300141106           in §AzUte;
014400141106           in §DatiUte;
014500141106         ENDIF;
014600141106
014700141106       ENDSR;
014800141106
014900141106       //--------------------------------------------------------------
015000141106       //?Controllo i dati passati.
015100141106       //--------------------------------------------------------------
015200141106       BEGSR Controlla;
015300141106
015400141106       //?Numero campagna obbligatorio
015500150211         IF  IKC75ncm = 0;
015600150211           OKC75err = 'E';
015700150211           OKC75msg = Msg(01);
015800141106           exsr RoutEnd;
015900141106         ENDIF;
016000141114       //?Deve essere una campagna valida
016100150211         chain (IKC75ncm) TICMP01L;
016200141114         IF  not %found(TICMP01L) or CMPdfc < Oggi;
016300150211           OKC75err = 'E';
016400150211           OKC75msg = Msg(01);
016500141114           exsr RoutEnd;
016600141114         ENDIF;
016700141106
016800141106       //?Almeno 1 tra KSU/KSC/CPO deve esserci
016900150211         IF  IKC75ksu = 0 and IKC75ksc = 0 and IKC75cpo = 0;
017000150211           OKC75err = 'E';
017100150211           OKC75msg = Msg(02);
017200141106           exsr RoutEnd;
017300141106         ENDIF;
017400141114       //?Deve essere una codice valido
017500150211         IF  IKC75ksu > 0;
017600141114           clear TIBS69DS;
017700150211           I69kac = %dec(IKC75ksu:7:0);
017800141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017900141114           IF  O69err <> *blanks;
018000150211             OKC75err = 'E';
018100150211             OKC75msg = Msg(02);
018200141114             exsr RoutEnd;
018300141114           ENDIF;
018400141114         ENDIF;
018500150211         IF  IKC75ksc > 0;
018600141114           clear TIBS69DS;
018700150211           I69kac = %dec(IKC75ksc:7:0);
018800141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
018900141114           IF  O69err <> *blanks;
019000150211             OKC75err = 'E';
019100150211             OKC75msg = Msg(02);
019200141114             exsr RoutEnd;
019300141114           ENDIF;
019400141114         ENDIF;
019500150211         IF  IKC75cpo > 0;
019600150211           chain (IKC75cpo) TNCPO01L;
019700141114           IF  not %found(TNCPO01L);
019800150211             OKC75err = 'E';
019900150211             OKC75msg = Msg(02);
020000141114             exsr RoutEnd;
020100141114           ENDIF;
020200141114         ENDIF;
020300141106
020400150211       //?Deve essere indicata la nuova Campagna
020500150211         IF  IKC75nncm = 0;
020600150211           OKC75err = 'E';
020700150211           OKC75msg = Msg(01);
020800150211           exsr RoutEnd;
020900150211         ENDIF;
021000150211       //?Deve essere una campagna valida
021100150211         chain (IKC75nncm) TICMP01L;
021200150211         IF  not %found(TICMP01L) or CMPdfc < Oggi;
021300150211           OKC75err = 'E';
021400150211           OKC75msg = Msg(01);
021500150211           exsr RoutEnd;
021600150211         ENDIF;
021700141106
021800141106       ENDSR;
021900141106
022000141106       //--------------------------------------------------------------
022100150211       //?Aggiorno la campagna sui vari archivi.
022200141106       //--------------------------------------------------------------
022300150211       BEGSR Aggiorna;
022400141106
022500150211         exec sql
022600150211         UPDATE TICMC00F set CMCncm = :IKC75nncm
022700150211         WHERE  CMCncm = :IKC75ncm and CMCksu = :IKC75ksu and
022800150211                CMCksc = :IKC75ksc and CMCcpo = :IKC75cpo;
022900150211
023000150211         exec sql
023100150211         UPDATE TICMI00F set CMIncm = :IKC75nncm
023200150211         WHERE  CMIncm = :IKC75ncm and CMIksu = :IKC75ksu and
023300150211                CMIksc = :IKC75ksc and CMIcpo = :IKC75cpo;
023400150211
023500150211         exec sql
023600150211         UPDATE TICMF00F set CMFncm = :IKC75nncm
023700150211         WHERE  CMFncm = :IKC75ncm and CMFksu = :IKC75ksu and
023800150211                CMFksc = :IKC75ksc and CMFcpo = :IKC75cpo;
023900150211
024000150211         exec sql
024100150211         UPDATE TICMN00F set CMNncm = :IKC75nncm
024200150211         WHERE  CMNncm = :IKC75ncm and CMNksu = :IKC75ksu and
024300150211                CMNksc = :IKC75ksc and CMNcpo = :IKC75cpo;
024400141106
024500141106       ENDSR;
024600141106
024700141106       //--------------------------------------------------------------
024800141106       //?Operazioni finali.
024900141106       //--------------------------------------------------------------
025000141106       BEGSR RoutEnd;
025100141106
025200141106         *inLR = *on;
025300141106         return;
025400141106
025500141106       ENDSR;
025600141106
025700141106      /end-free
025800141106
025900141106       //--------------------------------------------------------------
026000141106       //?Schiere a tempo di compilazione.
026100141106       //--------------------------------------------------------------
026200141106
026300141106** -- MSG -------------------------------------------------------------------*
026400141114Campagna commerciale errata                                                    1
026500141114Cliente/Potenziale errato                                                      2
