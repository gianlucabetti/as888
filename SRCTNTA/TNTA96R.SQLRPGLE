000100150225      //--------------------------------------------------------------
000200150225      //?TNTA96R - Verifica Tab. VMA con Tar.Particolari Fuel
000300150225      //--------------------------------------------------------------
000400150225
000500150225     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150225
000700150225      //---------------------------------------------------------------
000800150225      //?Dichiarazione file.
000900150225      //---------------------------------------------------------------
001000150225
001100150225      // - Printer File
001200150225     fTNTA96P   o    e             printer oflind(*in99)
001300150225
001400150225      //---------------------------------------------------------------
001500150225      //?Definizione costanti.
001600150225      //---------------------------------------------------------------
001700150225
001800150225     d Digits          c                   const('0123456789')
001900150225
002000150225      //---------------------------------------------------------------
002100150225      //?Definizione schiere.
002200150225      //---------------------------------------------------------------
002300150225
002400150225      //---------------------------------------------------------------
002500150225      //?Definizione aree dati.
002600150225      //---------------------------------------------------------------
002700150225
002800150225      // - Dati utente
002900150225     d §AzUte        e ds                  extname(AZUTE00F)
003000150225     d                                     dtaara
003100150225     d §DatiUte      e ds                  extname(dDatiUte)
003200150225     d                                     dtaara
003300150225
003400150225      //---------------------------------------------------------------
003500150225      //?Definizione strutture dati.
003600150225      //---------------------------------------------------------------
003700150225
003800150225      // - Status
003900150225     d Psds           sds
004000150225     d   SDSpgm          *proc
004100150225
004200150225      // - Parametri ricevuti
004300150225     d KPJBA         e ds
004400150225
004500150225      // - Ricerca/Controllo tabelle
004600150225     d TIBS02DS      e ds                  inz
004700150225
004800150225      // - Reperimento dati utente
004900150225     d TIBS34DS      e ds
005000150225
005100150225      // - Reperimento dati Anagrafica Clienti
005200150225      /copy gaitrasrc/srcprotopi,TIBS69R
005300150225
005400150225      // - File Tariffe particolari
005500150225     d TITPT00F      e ds                  extname(TITPT00F)
005600150225
005700150225      // - File Tariffe
005800150225     d TNTAM00F      e ds                  extname(TNTAM00F)
005900150225
006000150225      // - Campo TPTflo file TITPT00F
006100150225     d dTPT01        e ds
006200150225
006300150225      // - Tabella VMA - VMA Negato
006400150225     d dVMA          e ds
006500150225
006600150225      //---------------------------------------------------------------
006700150225      //?Definizione variabili globali.
006800150225      //---------------------------------------------------------------
006900150225
007000150225      // - Flags booleani
007100150225     d Fine            s               n   inz(*off)
007200150225     d OkStampa        s               n   inz(*off)
007300150225
007400150225      // - Indici di schiera
007500150225     d xx              s              4s 0 inz
007600150225
007700150225      // - Campi di comodo data
007800150225     d Data_EUR        s               d   datfmt(*eur)
007900150225     d Data_ISO        s               d   datfmt(*iso)
008000150225     d Wdata           s              8s 0 inz
008100150225     d Oggi            s              8s 0 inz
008200150225
008300150225      // - Campi di comodo
008400150225     d sav_TAMksc      s                   like(TAMksc) inz
008500150225
008600150225      //---------------------------------------------------------------
008700150225      //?Definizione procedure usate.
008800150225      //---------------------------------------------------------------
008900150225
009000150225      //---------------------------------------------------------------
009100150225      //?Definizione Prototipi.
009200150225      //---------------------------------------------------------------
009300150225      /copy gaitrasrc/srcprotopr,TIBS02R
009400150225      /copy gaitrasrc/srcprotopr,TIBS34R
009500150225      /copy gaitrasrc/srcprotopr,TIBS69R
009600150225
009700150225      //---------------------------------------------------------------
009800150225      //?Definizione key-list.
009900150225      //---------------------------------------------------------------
010000150225
010100150225      //---------------------------------------------------------------
010200150225      //?M A I N - L I N E
010300150225      //---------------------------------------------------------------
010400150225
010500150225     c     *Entry        plist
010600150225     c                   parm                    kpjba
010700150225
010800150225      /free
010900150225
011000150225       //?Operazioni iniziali
011100150225       exsr RoutInz;
011200150225
011300150225       //?Elaborazione
011400150225       exsr Elabora;
011500150225
011600150225       //?Operazioni finali
011700150225       exsr RoutEnd;
011800150225
011900150225       //--------------------------------------------------------------
012000150225       //?Operazioni iniziali.
012100150225       //--------------------------------------------------------------
012200150225       BEGSR RoutInz;
012300150225
012400150225         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500150225
012600150225       //?Imposto nome programma
012700150225         NomePgm = SDSpgm;
012800150225
012900150225       //?Imposto oggi + data e ora elaborazione
013000150225         Oggi = %dec(%date());
013100150225         Data_ISO = %date(Oggi:*ISO);
013200150225         Data_EUR = Data_ISO;
013300150225         DataEla   = %dec(Data_EUR);
013400150225         OraEla    = %dec(%time());
013500150225
013600150225       //?Reperimento dati job
013700150225         exsr DatiJob;
013800150225
013900150225       ENDSR;
014000150225
014100150225       //--------------------------------------------------------------
014200150225       //?Reperimento Dati del job (Utente/Operativi).
014300150225       //--------------------------------------------------------------
014400150225       BEGSR DatiJob;
014500150225
014600150225         in(E) §AzUte;
014700150225         IF  NOT %error;
014800150225           in(E) §DatiUte;
014900150225         ENDIF;
015000150225         IF  %error or RSut = *blanks;
015100150225           clear TIBS34ds;
015200150225           tibs34r(tibs34ds);
015300150225           in §AzUte;
015400150225           in §DatiUte;
015500150225         ENDIF;
015600150225
015700150225       ENDSR;
015800150225
015900150225       //--------------------------------------------------------------
016000150225       //?Elaborazione.
016100150225       //--------------------------------------------------------------
016200150225       BEGSR Elabora;
016300150225
016400150225       //?Leggo file Tariffe particolari
016500150225         exec sql
016600150225         declare TPT cursor for
016700150225         select * from TITPT00F join TNTAM00F
016800150225         on TPTksc = TAMksc and TPTctr = TAMctr and
016900150225         TPTprg = TAMprg
017000150225         where TPTftc = 'f ' and TPTatb = ' '
017100150225         order by TPTksc, TPTctr, TPTprg;
017200150225
017300150225         exec sql
017400150225         open TPT;
017500150225         IF  sqlcode < 0;
017600150225           Fine = *on;
017700150225         ENDIF;
017800150225
017900150225         DOW not Fine;
018000150225           exec sql
018100150225           fetch next from TPT into :TITPT00F, :TNTAM00F;
018200150225           IF  sqlcod = 100 or sqlcod < 0;
018300150225             Fine = *on;
018400150225             leave;
018500150225           ENDIF;
018600150225
018700150225           dTPT01 = TPTflo;
018800150225           exsr Controlla;
018900150225
019000150225         ENDDO;
019100150225
019200150225         exec sql close TPT;
019300150225
019400150225       ENDSR;
019500150225
019600150225       //--------------------------------------------------------------
019700150225       //?Controllo il Cliente Tariffa con tabella VMA.
019800150225       //--------------------------------------------------------------
019900150225       BEGSR Controlla;
020000150225
020100150225         clear nota;
020200150225
020300150225       //?Cerco cliente in tabella VMA
020400150225         clear TIBS02DS;
020500150225         clear dVMA;
020600150225         T02mod = 'C';
020700150225         T02cod = 'VMA';
020800150225         T02ke1 = %editc(TPTksc:'X');
020900150225         T02sif = KNSIF;
021000150225         TNTBE_RicercaControllo (kpjba : tibs02ds);
021100150225
021200150225       //?Se non trovo il cliente in tabella VMA
021300150225       //?e in tariffa c'è il flag di VMA negato
021400150225       //?stampo
021500150225         IF  T02err <> *blanks and  §TPTvma = 'N';
021600150225           nota = 'No Cliente in Tab. VMA';
021700150225           exsr stampa;
021800150225           leavesr;
021900150225         ENDIF;
022000150225
022100150225       //?Se non trovo vado via
022200150225         IF  T02err <> *blanks;
022300150225           leavesr;
022400150225         ENDIF;
022500150225
022600150225       //?Se trovo il cliente in tabella VMA
022700150225         dVMA = T02uni;
022800150225
022900150225       //?in tariffa c'è il flag di VMA negato
023000150225       //?e la data decorrenza tariffa non è nel range
023100150225       //?impostato in tabella
023200150225       //?stampo
023300150225         IF  §TPTvma = 'N' and
023400150225             (TAMddt < §VMAddt or TAMddt > §VMAdst);
023500150225           nota = 'Decorrenza Tariffa fuori range tab. VMA';
023600150225           exsr stampa;
023700150225           leavesr;
023800150225         ENDIF;
023900150225
024000150225       //?in tariffa NON c'è il flag di VMA negato
024100150225       //?e la data decorrenza tariffa è nel range
024200150225       //?impostato in tabella
024300150225       //?stampo
024400150225         IF  §TPTvma <> 'N' and
024500150225             TAMddt >= §VMAddt and TAMddt <= §VMAdst;
024600150225           nota = 'Tariffa senza VMA negato';
024700150225           exsr stampa;
024800150225           leavesr;
024900150225         ENDIF;
025000150225
025100150225       ENDSR;
025200150225
025300150206       //--------------------------------------------------------------
025400150225       //?Stampa anomalie.
025500150206       //--------------------------------------------------------------
025600150206       BEGSR Stampa;
025700150225
025800150225         IF  not Okstampa;
025900150225           write TA96TESTA;
026000150225           OkStampa = *on;
026100150225         ENDIF;
026200150206
026300150225         IF  *in99;
026400150225           write TA96TESTA;
026500150225           *in99 = *off;
026600150225         ENDIF;
026700150206
026800150225         clear TIBS69DS;
026900150225         clear ACOrag;
027000150225         I69kac = TPTksc;
027100150225         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
027200170123
027300170123         Data_ISO = %date(TAMddt:*iso);
027400170123         Data_EUR = Data_ISO;
027500170123         dtdeco   = %dec(Data_EUR);
027600170123         Data_ISO = %date(TAMdst:*iso);
027700170123         Data_EUR = Data_ISO;
027800170123         dtscad   = %dec(Data_EUR);
027900150206
028000150225         write TA96RIGA;
028100150206
028200150206       ENDSR;
028300141028
028400141028       //--------------------------------------------------------------
028500141028       //?Operazioni finali.
028600141028       //--------------------------------------------------------------
028700141028       BEGSR RoutEnd;
028800150225
028900150225       //?Stampa fine
029000150225         IF  OkStampa;
029100150225           write TA96FINE;
029200161230
029300161230         //?Se ho stampato mando mail e me e Antonella per controllare le stampe
029400170109           peml = 'antonella.gallucci@brt.it';
029500170109           pcceml = 'monica.beghelli@brt.it';
029600161230           pogg = 'ANOMALIE IN TABELLA VMA';
029700161230           pmsg = 'Controllare la stampa su Setras con profilo EDPMB TNTA96P';
029800161230      /end-free
029900161230     C                   call      'TIS701C1'
030000161230     C                   parm                    peml            253
030100161230     C                   parm                    pcceml          253
030200161230     C                   parm                    pogg             44
030300161230     C                   parm                    pmsg           5000
030400161230     C                   parm                    pesito            1
030500161230      /free
030600161230
030700150225         ENDIF;
030800150206
030900141028         *inLR = *on;
031000141028         return;
031100141028
031200141028       ENDSR;
031300141028
031400141028      /end-free
