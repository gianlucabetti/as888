000100150326      //---------------------------------------------------------------
000200150401      //?AUTA03R1- Crea WFAUC10F
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
001700150401     fWFAUC10F  o    e             disk    usropn extfile('EDPAUMENTI/WFAUC10F')
001800150401     fWFAUC12L  uf   e           k disk    usropn extfile('EDPAUMENTI/WFAUC12L')
001900150401     f                                     rename(WFAUC100:WFAUC12)
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
006300150401      // - File WFAUC10F
006400150401     d WFAUCDS       e ds                  extname(WFAUC10F)
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
011200150331
011300150331       //?Sistema gli Unificanti che sono figli di altri
011400150331       exsr Sistema;
011500150326
011600150326       //?Operazioni finali
011700150326       exsr RoutEnd;
011800150326
011900150326       //--------------------------------------------------------------
012000150326       //?Operazioni iniziali.
012100150326       //--------------------------------------------------------------
012200150326       BEGSR RoutInz;
012300150326
012400150326         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500150326
012600150326       //?Imposto oggi
012700150326         Oggi = %dec(%date());
012800150326
012900150326       //?Reperimento dati job
013000150326         exsr DatiJob;
013100150326
013200150326       //?Pulisco il file di work
013300150401         comando = 'CLRPFM FILE(EDPAUMENTI/WFAUC10F)';
013400150326         IF  ExecuteCommand (comando) > 0 ;
013500150326           exsr RoutEnd;
013600150326         ENDIF;
013700150326
013800150326       //?Apro il file di work
013900150401         open WFAUC10F;
014000150401         open WFAUC12L;
014100150326
014200150326       ENDSR;
014300150326
014400150326       //--------------------------------------------------------------
014500150326       //?Reperimento Dati del job (Utente/Operativi).
014600150326       //--------------------------------------------------------------
014700150326       BEGSR DatiJob;
014800150326
014900150326         in(E) §AzUte;
015000150326         IF  not %error;
015100150326           in(E) §DatiUte;
015200150326         ENDIF;
015300150326         IF  %error or RSut = *blanks;
015400150326           clear TIBS34ds;
015500150326           tibs34r(tibs34ds);
015600150326           in §AzUte;
015700150326           in §DatiUte;
015800150326         ENDIF;
015900150326
016000150326       ENDSR;
016100150326
016200150326       //--------------------------------------------------------------
016300150326       //?Elaborazione.
016400150326       //--------------------------------------------------------------
016500150326       BEGSR Elabora;
016600150326
016700150326       //?Dichiarazione cursore
016800150326         exec sql
016900150326         DECLARE CMC cursor for
017000150401         SELECT * from TICMC00F
017100150401         WHERE CMCufe <> ' 90';
017200150326
017300150326         //?Apertura del cursore
017400150326         exec sql open CMC;
017500150326
017600150326         IF sqlcode < 0;
017700150326           exec sql close CMC;
017800150326           leavesr;
017900150326         ENDIF;
018000150326
018100150326         DOW  not wEoF;
018200150326           exec sql
018300150326           fetch next from CMC into :TICMC00F;
018400150326           IF  sqlcod = 100 or sqlcod < 0;
018500150326             wEoF = *on;
018600150326             leave;
018700150326           ENDIF;
018800150326
018900150326       //?Scrivo il file
019000150326           exsr Scrivi;
019100150326
019200150326         ENDDO;
019300150326
019400150326         exec sql close CMC;
019500150326
019600150326       ENDSR;
019700150326
019800150326       //--------------------------------------------------------------
019900150326       //?Scrivo il file.
020000150326       //--------------------------------------------------------------
020100150326       BEGSR Scrivi;
020200150326
020300150326       //?Scrivo il file per TCL = 'U'
020400150331         exsr NewRcdU;
020500150326
020600150326       ENDSR;
020700150331
020800150331       //--------------------------------------------------------------
020900150331       //?Scrive nuovo rcd U su WFAUC
021000150331       //--------------------------------------------------------------
021100150331       BEGSR NewRcdU;
021200150331
021300150331       //?Aggancio CNACO
021400150331         clear  TIBS69ds;
021500150331         i69sif = knsif;
021600150331         i69kcc = DUTkci;
021700150331         i69kac = CMCksu;
021800150331         tibs69r( TIBS69ds :
021900150331                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
022000150331
022100150331       //?Imposto i campi del file per padre
022200150401         clear WFAUC100;
022300150331         WFAncm = CMCncm;
022400150331         WFAtcl = 'U';
022500150331         WFAksu = CMCksu;
022600150331         WFArag = ACOrag;
022700150331
022800150331       //?Imposto se anullato
022900150331         IF  ACOflg <> *blanks;
023000150331           WFAflg = 'S';
023100150331         ENDIF;
023200150331
023300150331       //?Imposto se bloccato
023400150331         IF  ACOabl <> *blanks;
023500150401           //WFAabl = 'S';
023600150331         ENDIF;
023700150331
023800150331       //?Imposto se NON ha tariffe
023900150331         chain  WFAksu TNTAM00L;
024000150331         IF  not %found(TNTAM00L);
024100150331           WFAtar = 'S';
024200150331         ENDIF;
024300150331
024400150331       //?Imposto se è stato trattato
024500150331       //?Aggancio la fase TR
024600150331         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
024700150331       //?se la trovo imposto il flag
024800150331         IF  %found(TICMF01L);
024900150331           WFAttr = 'S';
025000150331         ELSE;
025100150331       //?se non la trovo provo con la fase TF
025200150331           chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjTtr) TICMF01L;
025300150331       //?se la trovo imposto il flag
025400150331           IF  %found(TICMF01L);
025500150331             WFAttr = 'S';
025600150331           ENDIF;
025700150331         ENDIF;
025800150331
025900150401         write WFAUC100;
026000150401         feod WFAUC10F;
026100150331
026200150331       ENDSR;
026300150331
026400150331       //--------------------------------------------------------------
026500150331       //?Sistema.
026600150331       //--------------------------------------------------------------
026700150331       BEGSR Sistema;
026800150331
026900150331         wEoF = *off;
027000150331
027100150331       //?Leggo tutti gli Unificanti
027200150331         exec sql
027300150331         DECLARE AUC cursor for
027400150401         SELECT * from WFAUC10F
027500150331         WHERE WFAtcl = 'U'
027600150331         ORDER BY WFAksu;
027700150331
027800150331         //?Apertura del cursore
027900150331         exec sql open AUC;
028000150331
028100150331         IF sqlcode < 0;
028200150331           exec sql close AUC;
028300150331           leavesr;
028400150331         ENDIF;
028500150331
028600150331         DOW  not wEoF;
028700150331           exec sql
028800150331           fetch next from AUC into :WFAUCDS;
028900150331           IF  sqlcod = 100 or sqlcod < 0;
029000150331             wEoF = *on;
029100150331             leave;
029200150331           ENDIF;
029300150331
029400150331       //?Cerco il papa del codice in elaborazione
029500150331           clear TIBS10DS;
029600150331           D10tle = 'ST';
029700150331           D10paf = 'P';
029800150331           D10cod = WFAksu;
029900150331           D10drf = %dec(%date());
030000150331           GetKSCfamily ( TIBS10ds : 'A' );
030100150331       //?Se torna errore leggo nuovo KSU
030200150331           IF  D10err <> *blanks;
030300150331             iter;
030400150331           ENDIF;
030500150331       //?Se Papa uguale al KSU leggo nuovo KSU
030600150331           IF  D10cop = WFAksu;
030700150331             iter;
030800150331           ENDIF;
030900150331       //?Se arrivo qua vol dire che
031000150331       //?è un Unificante presente in campagna
031100150331       //?ma che è diventato figlio di un altro Unificante
031200150331       //?quindi controllo se l'unificante nuovo è in campagna
031300150331           w0070 = D10cop;
031400150331           chain (w0070) TICMC02L;
031500150401       //?memorizzo in che campagna è il nuovo papà
031600150401           IF  not %found(TICMC02L);
031700150401             clear CMCncm;
031800150331           ENDIF;
031900150331         //?se lo trovo aggiorno memorizzo nuovo KSU su WFAUC
032000150401           chain ('U':WFAksu) WFAUC12L;
032100150401           IF  %found(WFAUC12L);
032200150401             WFAnncm = CMCncm;
032300150331             WFAnksu = D10cop;
032400150401             update WFAUC12;
032500150331           ENDIF;
032600150331
032700150331         ENDDO;
032800150331
032900150331         exec sql close AUC;
033000150331
033100150331       ENDSR;
033200150326
033300150326       //--------------------------------------------------------------
033400150326       //?Operazioni finali.
033500150326       //--------------------------------------------------------------
033600150326       BEGSR RoutEnd;
033700150326
033800150326         *inLR = *on;
033900150326         return;
034000150326
034100150326       ENDSR;
034200150326
034300150326      /end-free
