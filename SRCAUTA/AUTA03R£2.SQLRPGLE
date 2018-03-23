000100150326      //---------------------------------------------------------------
000200150326      //?AUTA03R - Crea WFAUC00F x Campagna Aumento Tariffe da Lettera
000300150326      //---------------------------------------------------------------
000400150326
000500150326     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150326     h dftactgrp(*no) actgrp(*caller)
000700150326
000800150326      //---------------------------------------------------------------
000900150326      //?Dichiarazione file.
001000150326      //---------------------------------------------------------------
001100150326
001200150331     fTICMC02L  if   e           k DISK
001300150326     fTICMF01L  if   e           k DISK
001400150326
001500150326     fTNTAM00L  if   e           k DISK
001600150326
001700150326     fWFAUC00F  o    e             disk    usropn extfile('EDPAUMENTI/WFAUC00F')
001800150331     fWFAUC01L  if   e           k disk    usropn extfile('EDPAUMENTI/WFAUC01L')
001900150331     f                                     rename(WFAUC000:WFAUC01)
002000150326
002100150326      //---------------------------------------------------------------
002200150326      //?Definizione costanti.
002300150326      //---------------------------------------------------------------
002400150326
002500150326     d FaseObjFine     c                   const(' 30')
002600150327     d FaseObjTf       c                   const(' TF')
002700150327     d FaseObjTtr      c                   const(' TR')
002800150326
002900150326      //---------------------------------------------------------------
003000150326      //?Definizione schiere.
003100150326      //---------------------------------------------------------------
003200150326
003300150326      //---------------------------------------------------------------
003400150326      //?Definizione aree dati.
003500150326      //---------------------------------------------------------------
003600150326
003700150326      // - Dati utente
003800150326     d §AzUte        e ds                  extname(AZUTE00F)
003900150326     d                                     dtaara
004000150326     d §DatiUte      e ds                  extname(dDatiUte)
004100150326     d                                     dtaara
004200150326
004300150326      //---------------------------------------------------------------
004400150326      //?Definizione strutture dati.
004500150326      //---------------------------------------------------------------
004600150326
004700150326      // - Parametri ricevuti
004800150326     d KPJBA         e ds
004900150326
005000150326      // - Ricerca Unificante Padre?
005100150326     d TIBS10DS      e ds                  inz
005200150326     d  sk_D10skc             21   5520  0 inz  dim(500)
005300150326
005400150326      // - Reperimento dati utente
005500150326     d TIBS34DS      e ds
005600150326
005700150326      // - Reperimento dati Anagrafica Clienti
005800150326      /copy gaitrasrc/srcprotopi,TIBS69R
005900150326
006000150326      // - File TICMC00F
006100150326     d TICMC00F      e ds                  extname(TICMC00F)
006200150331
006300150331      // - File WFAUC00F
006400150331     d WFAUCDS       e ds                  extname(WFAUC00F)
006500150326
006600150326      //---------------------------------------------------------------
006700150326      //?Definizione variabili globali.
006800150326      //---------------------------------------------------------------
006900150326
007000150326      // - Flags booleani
007100150326     d wEoF            s               n   inz(*off)
007200150326
007300150326      // - Indice di schiera
007400150326     d xx              s              3s 0 inz
007500150326
007600150326      // - Campi di comodo
007700150326     d Comando         s            512a   varying
007800150326     d DataUni         s              8s 0 inz(20141231)
007900150326     d Oggi            s              8s 0 inz
008000150331     d w0070           s              7s 0 inz
008100150326
008200150326      //---------------------------------------------------------------
008300150326      //?Definizione Procedure usate.
008400150326      //---------------------------------------------------------------
008500150326
008600150326      //---------------------------------------------------------------
008700150326      //?Definizione Prototipi.
008800150326      //---------------------------------------------------------------
008900150326      /copy gaitrasrc/srcprotopr,TIBS10R
009000150326      /copy gaitrasrc/srcprotopr,TIBS34R
009100150326      /copy gaitrasrc/srcprotopr,TIBS69R
009200150326      /copy gaitrasrc/srcprotopr,SYSTEM
009300150326
009400150326      //---------------------------------------------------------------
009500150326      //?Definizione key-list.
009600150326      //---------------------------------------------------------------
009700150326
009800150326      //---------------------------------------------------------------
009900150326      //?M A I N - L I N E
010000150326      //---------------------------------------------------------------
010100150326
010200150326     c     *Entry        plist
010300150326     c                   parm                    kpjba
010400150326
010500150326      /free
010600150326
010700150326       //?Operazioni iniziali
010800150326       exsr RoutInz;
010900150326
011000150326       //?Elaborazione
011100150326       exsr Elabora;
011200150326
011300150326       //?Operazioni finali
011400150326       exsr RoutEnd;
011500150326
011600150326       //--------------------------------------------------------------
011700150326       //?Operazioni iniziali.
011800150326       //--------------------------------------------------------------
011900150326       BEGSR RoutInz;
012000150326
012100150326         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012200150326
012300150326       //?Imposto oggi
012400150326         Oggi = %dec(%date());
012500150326
012600150326       //?Reperimento dati job
012700150326         exsr DatiJob;
012800150326
012900150326       //?Pulisco il file di work
013000150326         comando = 'CLRPFM FILE(EDPAUMENTI/WFAUC00F)';
013100150326         IF  ExecuteCommand (comando) > 0 ;
013200150326           exsr RoutEnd;
013300150326         ENDIF;
013400150326
013500150326       //?Apro il file di work
013600150326         open WFAUC00F;
013700150331         open WFAUC01L;
013800150326
013900150326       ENDSR;
014000150326
014100150326       //--------------------------------------------------------------
014200150326       //?Reperimento Dati del job (Utente/Operativi).
014300150326       //--------------------------------------------------------------
014400150326       BEGSR DatiJob;
014500150326
014600150326         in(E) §AzUte;
014700150326         IF  not %error;
014800150326           in(E) §DatiUte;
014900150326         ENDIF;
015000150326         IF  %error or RSut = *blanks;
015100150326           clear TIBS34ds;
015200150326           tibs34r(tibs34ds);
015300150326           in §AzUte;
015400150326           in §DatiUte;
015500150326         ENDIF;
015600150326
015700150326       ENDSR;
015800150326
015900150326       //--------------------------------------------------------------
016000150326       //?Elaborazione.
016100150326       //--------------------------------------------------------------
016200150326       BEGSR Elabora;
016300150326
016400150326       //?Dichiarazione cursore
016500150326         exec sql
016600150326         DECLARE CMC cursor for
016700150326         SELECT * from TICMC00F
016800150401         WHERE CMCncm in(2, 4)
016900150401           and CMCufe <> ' 90';
017000150326
017100150326         //?Apertura del cursore
017200150326         exec sql open CMC;
017300150326
017400150326         IF sqlcode < 0;
017500150326           exec sql close CMC;
017600150326           leavesr;
017700150326         ENDIF;
017800150326
017900150326         DOW  not wEoF;
018000150326           exec sql
018100150326           fetch next from CMC into :TICMC00F;
018200150326           IF  sqlcod = 100 or sqlcod < 0;
018300150326             wEoF = *on;
018400150326             leave;
018500150326           ENDIF;
018600150326
018700150326       //?Cerco i figli del codice in elaborazione
018800150326           clear TIBS10DS;
018900150326           D10tle = 'ST';
019000150326           D10paf = 'F';
019100150326           D10cod = CMCksu;
019200150327           D10drf = %dec(%date());
019300150326           GetKSCfamily ( TIBS10ds : 'A' );
019400150326       //?Se ritorna errore vuol dire che non è un codice figlio
019500150326       //?quindi imposto lui come primo elemento della schiera
019600150326           IF  D10err <> *blanks or D10skc = *zeros;
019700150326             sk_D10skc(01) = CMCksu;
019800150326           ENDIF;
019900150326
020000150326       //?Scrivo il file
020100150326           exsr Scrivi;
020200150326
020300150326         ENDDO;
020400150326
020500150326         exec sql close CMC;
020600150326
020700150326       ENDSR;
020800150326
020900150326       //--------------------------------------------------------------
021000150326       //?Scrivo il file.
021100150326       //--------------------------------------------------------------
021200150326       BEGSR Scrivi;
021300150326
021400150326       //?Scrivo il file per TCL = 'C'
021500150326         FOR  xx = 1 to %elem(sk_D10skc);
021600150326
021700150326           IF  sk_D10skc(xx) = *zeros;
021800150326             leave;
021900150326           ENDIF;
022000150331
022100150331         //?Prima di scrivere un nuovo figlio controllo che non esista già
022200150331         //?può capitare perchè gli unificanti cambiano costantemente
022300150331         //?e un cliente unificante presente in campagna nel tempo può diventare
022400150331         //?figlio di un altro cliente unificante presente in campagna
022500150331           w0070 = sk_D10skc(xx);
022600150401         //  chain ('C':w0070) WFAUC01L;
022700150401         //  IF  not %found(WFAUC01L);
022800150331             exsr NewRcdC;
022900150401         //  ENDIF;
023000150326         ENDFOR;
023100150326
023200150326       //?Scrivo il file per TCL = 'U'
023300150331         exsr NewRcdU;
023400150326
023500150326       ENDSR;
023600150331
023700150331       //--------------------------------------------------------------
023800150331       //?Scrive nuovo rcd C su WFAUC
023900150331       //--------------------------------------------------------------
024000150331       BEGSR NewRcdC;
024100150331
024200150331       //?Aggancio CNACO
024300150331         clear  TIBS69ds;
024400150331         i69sif = knsif;
024500150331         i69kcc = DUTkci;
024600150331         i69kac = sk_D10skc(xx);
024700150331         tibs69r( TIBS69ds :
024800150331                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
024900150331
025000150331       //?Imposto i campi del file per figlio
025100150331         clear WFAUC000;
025200150331         WFAncm = CMCncm;
025300150331         WFAtcl = 'C';
025400150331         WFAksu = CMCksu;
025500150331         WFAksc = ACOksc;
025600150331         WFArag = ACOrag;
025700150331       //?Cerco ultima fase 30
025800150331         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjFine) TICMF01L;
025900150331         IF  %found(TICMF01L);
026000150331           IF  CMFncm = 2;
026100150331             WFAper = 4;
026200150331           ENDIF;
026300150331           IF  CMFncm = 4;
026400150331             WFAper = 1,90;
026500150331           ENDIF;
026600150331         ENDIF;
026700150331
026800150331       //?Imposto se anullato
026900150331         IF  ACOflg <> *blanks;
027000150331           WFAflg = 'S';
027100150331         ENDIF;
027200150331
027300150331       //?Imposto se bloccato
027400150331         IF  ACOabl <> *blanks;
027500150401           //WFAabl = 'S';
027600150331         ENDIF;
027700150331
027800150331       //?Imposto se NON ha tariffe
027900150331         chain  WFAksc TNTAM00L;
028000150331         IF  not %found(TNTAM00L);
028100150331           WFAtar = 'S';
028200150331         ENDIF;
028300150331
028400150331       //?Imposto se è stato trattato
028500150331       //?Aggancio la fase TR
028600150331         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
028700150331       //?se la trovo imposto il flag
028800150331         IF  %found(TICMF01L);
028900150331           WFAttr = 'S';
029000150331         ELSE;
029100150331       //?se non la trovo provo con la fase TF
029200150331           chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
029300150331       //?se la trovo imposto il flag
029400150331           IF  %found(TICMF01L);
029500150331             WFAttr = 'S';
029600150331           ENDIF;
029700150331         ENDIF;
029800150331
029900150331         write WFAUC000;
030000150331         feod WFAUC00F;
030100150331
030200150331       ENDSR;
030300150331
030400150331       //--------------------------------------------------------------
030500150331       //?Scrive nuovo rcd U su WFAUC
030600150331       //--------------------------------------------------------------
030700150331       BEGSR NewRcdU;
030800150331
030900150331       //?Aggancio CNACO
031000150331         clear  TIBS69ds;
031100150331         i69sif = knsif;
031200150331         i69kcc = DUTkci;
031300150331         i69kac = CMCksu;
031400150331         tibs69r( TIBS69ds :
031500150331                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
031600150331
031700150331       //?Imposto i campi del file per padre
031800150331         clear WFAUC000;
031900150331         WFAncm = CMCncm;
032000150331         WFAtcl = 'U';
032100150331         WFAksu = CMCksu;
032200150331         WFArag = ACOrag;
032300150331       //?Cerco ultima fase 30
032400150331         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjFine) TICMF01L;
032500150331         IF  %found(TICMF01L);
032600150331           IF  CMFncm = 2;
032700150331             WFAper = 4;
032800150331           ENDIF;
032900150331           IF  CMFncm = 4;
033000150331             WFAper = 1,90;
033100150331           ENDIF;
033200150331         ENDIF;
033300150331
033400150331       //?Imposto se anullato
033500150331         IF  ACOflg <> *blanks;
033600150331           WFAflg = 'S';
033700150331         ENDIF;
033800150331
033900150331       //?Imposto se bloccato
034000150331         IF  ACOabl <> *blanks;
034100150401           //WFAabl = 'S';
034200150331         ENDIF;
034300150331
034400150331       //?Imposto se NON ha tariffe
034500150331         chain  WFAksu TNTAM00L;
034600150331         IF  not %found(TNTAM00L);
034700150331           WFAtar = 'S';
034800150331         ENDIF;
034900150331
035000150331       //?Imposto se è stato trattato
035100150331       //?Aggancio la fase TR
035200150331         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
035300150331       //?se la trovo imposto il flag
035400150331         IF  %found(TICMF01L);
035500150331           WFAttr = 'S';
035600150331         ELSE;
035700150331       //?se non la trovo provo con la fase TF
035800150331           chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
035900150331       //?se la trovo imposto il flag
036000150331           IF  %found(TICMF01L);
036100150331             WFAttr = 'S';
036200150331           ENDIF;
036300150331         ENDIF;
036400150331
036500150331         write WFAUC000;
036600150331         feod WFAUC00F;
036700150331
036800150331       ENDSR;
036900150326
037000150326       //--------------------------------------------------------------
037100150326       //?Operazioni finali.
037200150326       //--------------------------------------------------------------
037300150326       BEGSR RoutEnd;
037400150326
037500150326         *inLR = *on;
037600150326         return;
037700150326
037800150326       ENDSR;
037900150326
038000150326      /end-free
