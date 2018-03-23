000100150130      //---------------------------------------------------------------
000200150130      //?AUTA03R - Crea WFAUC00F x Campagna Aumento Tariffe da Lettera
000300150130      //---------------------------------------------------------------
000400150129
000500150129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150130     h dftactgrp(*no) actgrp(*caller)
000700150129
000800150129      //---------------------------------------------------------------
000900150129      //?Dichiarazione file.
001000150129      //---------------------------------------------------------------
001100150129
001200150130     fWFAUC00F  o    e             disk    usropn extfile('EDPAUMENTI/WFAUC00F')
001300150129
001400150129     fTNTAM00L  if   e           k DISK
001500150129
001600150129      //---------------------------------------------------------------
001700150129      //?Definizione costanti.
001800150129      //---------------------------------------------------------------
001900150129
002000150129      //---------------------------------------------------------------
002100150129      //?Definizione schiere.
002200150129      //---------------------------------------------------------------
002300150129
002400150129      //---------------------------------------------------------------
002500150129      //?Definizione aree dati.
002600150129      //---------------------------------------------------------------
002700150129
002800150129      // - Dati utente
002900150129     d §AzUte        e ds                  extname(AZUTE00F)
003000150129     d                                     dtaara
003100150129     d §DatiUte      e ds                  extname(dDatiUte)
003200150129     d                                     dtaara
003300150129
003400150129      //---------------------------------------------------------------
003500150129      //?Definizione strutture dati.
003600150129      //---------------------------------------------------------------
003700150129
003800150129      // - Parametri ricevuti
003900150129     d KPJBA         e ds
004000150129
004100150129      // - Ricerca Unificante Padre?
004200150129     d TIBS10DS      e ds                  inz
004300150129     d  sk_D10skc             21   5520  0 inz  dim(500)
004400150129
004500150129      // - Reperimento dati utente
004600150129     d TIBS34DS      e ds
004700150129
004800150129      // - Reperimento dati Anagrafica Clienti
004900150129      /copy gaitrasrc/srcprotopi,TIBS69R
005000150130
005100150130      // - File WADELAUM
005200150130     d WADELAUM      e ds                  extname(WADELAUM)
005300150129
005400150129      //---------------------------------------------------------------
005500150129      //?Definizione variabili globali.
005600150129      //---------------------------------------------------------------
005700150129
005800150130      // - Flags booleani
005900150130     d wEoF            s               n   inz(*off)
006000150129
006100150129      // - Indice di schiera
006200150129     d xx              s              3s 0 inz
006300150129
006400150129      // - Campi di comodo
006500150129     d Comando         s            512a   varying
006600150129     d DataUni         s              8s 0 inz(20141231)
006700150129     d Oggi            s              8s 0 inz
006800150129
006900150129      //---------------------------------------------------------------
007000150129      //?Definizione Procedure usate.
007100150129      //---------------------------------------------------------------
007200150129
007300150129      //---------------------------------------------------------------
007400150129      //?Definizione Prototipi.
007500150129      //---------------------------------------------------------------
007600150129      /copy gaitrasrc/srcprotopr,TIBS10R
007700150129      /copy gaitrasrc/srcprotopr,TIBS34R
007800150129      /copy gaitrasrc/srcprotopr,TIBS69R
007900150129      /copy gaitrasrc/srcprotopr,SYSTEM
008000150129
008100150129      //---------------------------------------------------------------
008200150129      //?Definizione key-list.
008300150129      //---------------------------------------------------------------
008400150129
008500150129      //---------------------------------------------------------------
008600150129      //?M A I N - L I N E
008700150129      //---------------------------------------------------------------
008800150129
008900150129     c     *Entry        plist
009000150129     c                   parm                    kpjba
009100150129
009200150129
009300150129      /free
009400150129
009500150129       //?Operazioni iniziali
009600150129       exsr RoutInz;
009700150129
009800150129       //?Elaborazione
009900150129       exsr Elabora;
010000150129
010100150129       //?Operazioni finali
010200150129       exsr RoutEnd;
010300150129
010400150129       //--------------------------------------------------------------
010500150129       //?Operazioni iniziali.
010600150129       //--------------------------------------------------------------
010700150129       BEGSR RoutInz;
010800150130
010900150130         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011000150129
011100150129       //?Imposto oggi
011200150129         Oggi = %dec(%date());
011300150129
011400150129       //?Reperimento dati job
011500150129         exsr DatiJob;
011600150129
011700150129       //?Pulisco il file di work
011800150130         comando = 'CLRPFM FILE(EDPAUMENTI/WFAUC00F)';
011900150129         IF  ExecuteCommand (comando) > 0 ;
012000150129           exsr RoutEnd;
012100150129         ENDIF;
012200150129
012300150129       //?Apro il file di work
012400150130         open WFAUC00F;
012500150129
012600150129       ENDSR;
012700150129
012800150129       //--------------------------------------------------------------
012900150129       //?Reperimento Dati del job (Utente/Operativi).
013000150129       //--------------------------------------------------------------
013100150129       BEGSR DatiJob;
013200150129
013300150129         in(E) §AzUte;
013400150129         IF  not %error;
013500150129           in(E) §DatiUte;
013600150129         ENDIF;
013700150129         IF  %error or RSut = *blanks;
013800150129           clear TIBS34ds;
013900150129           tibs34r(tibs34ds);
014000150129           in §AzUte;
014100150129           in §DatiUte;
014200150129         ENDIF;
014300150129
014400150129       ENDSR;
014500150129
014600150129       //--------------------------------------------------------------
014700150129       //?Elaborazione.
014800150129       //--------------------------------------------------------------
014900150129       BEGSR Elabora;
015000150130
015100150130       //?Dichiarazione cursore
015200150130         exec sql
015300150130         DECLARE WAUM cursor for
015400150130         SELECT * from WADELAUM
015500150130         WHERE WAPtau = 'L';
015600150130
015700150130         //?Apertura del cursore
015800150130         exec sql open WAUM;
015900150130
016000150130         IF sqlcode < 0;
016100150130           exec sql close WAUM;
016200150130           leavesr;
016300150130         ENDIF;
016400150130
016500150130         DOW  not wEoF;
016600150130           exec sql
016700150130           fetch next from WAUM into :WADELAUM;
016800150130           IF  sqlcod = 100 or sqlcod < 0;
016900150130             wEoF = *on;
017000150130             leave;
017100150130           ENDIF;
017200150129
017300150129       //?Cerco i figli del codice in elaborazione
017400150129           clear TIBS10DS;
017500150129           D10tle = 'ST';
017600150129           D10paf = 'F';
017700150129           D10cod = WAPcli;
017800150130           D10drf = DataUni;
017900150129           GetKSCfamily ( TIBS10ds : 'A' );
018000150129       //?Se ritorna errore vuol dire che non è un codice figlio
018100150129       //?quindi imposto lui come primo elemento della schiera
018200150129           IF  D10err <> *blanks or D10skc = *zeros;
018300150129             sk_D10skc(01) = WAPcli;
018400150129           ENDIF;
018500150129
018600150130       //?Scrivo il file
018700150130           exsr Scrivi;
018800150129
018900150129         ENDDO;
019000150130
019100150130         exec sql close WAUM;
019200150129
019300150129       ENDSR;
019400150129
019500150129       //--------------------------------------------------------------
019600150130       //?Scrivo il file.
019700150129       //--------------------------------------------------------------
019800150130       BEGSR Scrivi;
019900150129
020000150130       //?Scrivo il file per TCL = 'C'
020100150129         FOR  xx = 1 to %elem(sk_D10skc);
020200150129
020300150129           IF  sk_D10skc(xx) = *zeros;
020400150129             leave;
020500150129           ENDIF;
020600150129
020700150129         //?Aggancio CNACO
020800150129           clear  TIBS69ds;
020900150129           i69sif = knsif;
021000150129           i69kcc = DUTkci;
021100150129           i69kac = sk_D10skc(xx);
021200150129           tibs69r( TIBS69ds :
021300150129                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
021400150129
021500150129         //?Imposto i campi del file per figlio
021600150130           clear WFAUC000;
021700150129           WFAtcl = 'C';
021800150130           WFAksu = WAPcli;
021900150130           WFAksc = ACOksc;
022000150129           WFArag = ACOrag;
022100150130           WFAper = WAPadt;
022200150130
022300150130         //?Imposto se anullato
022400150130           IF  ACOflg <> *blanks;
022500150130             WFAflg = 'S';
022600150130           ENDIF;
022700150130
022800150130         //?Imposto se bloccato
022900150130           IF  ACOabl <> *blanks;
023000150130             WFAabl = 'S';
023100150130           ENDIF;
023200150129
023300150130         //?Imposto se NON ha tariffe
023400150130           chain  WFAksc TNTAM00L;
023500150130           IF  not %found(TNTAM00L);
023600150130             WFAtar = 'S';
023700150129           ENDIF;
023800150129
023900150130           write WFAUC000;
024000150130         ENDFOR;
024100150130
024200150130       //?Scrivo il file per TCL = 'U'
024300150130
024400150130       //?Aggancio CNACO
024500150130         clear  TIBS69ds;
024600150130         i69sif = knsif;
024700150130         i69kcc = DUTkci;
024800150130         i69kac = WAPcli;
024900150130         tibs69r( TIBS69ds :
025000150130                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
025100150130
025200150130       //?Imposto i campi del file per padre
025300150130         clear WFAUC000;
025400150130         WFAtcl = 'U';
025500150130         WFAksu = WAPcli;
025600150130         WFArag = ACOrag;
025700150130         WFAper = WAPadt;
025800150130
025900150130       //?Imposto se anullato
026000150130         IF  ACOflg <> *blanks;
026100150130           WFAflg = 'S';
026200150130         ENDIF;
026300150130
026400150130       //?Imposto se bloccato
026500150130         IF  ACOabl <> *blanks;
026600150130           WFAabl = 'S';
026700150130         ENDIF;
026800150130
026900150130       //?Imposto se NON ha tariffe
027000150130         chain  WFAksu TNTAM00L;
027100150130         IF  not %found(TNTAM00L);
027200150130           WFAtar = 'S';
027300150130         ENDIF;
027400150130
027500150130         write WFAUC000;
027600150129
027700150129       ENDSR;
027800150129
027900150129       //--------------------------------------------------------------
028000150129       //?Operazioni finali.
028100150129       //--------------------------------------------------------------
028200150129       BEGSR RoutEnd;
028300150129
028400150129         *inLR = *on;
028500150129         return;
028600150129
028700150129       ENDSR;
028800150129
028900150129      /end-free
