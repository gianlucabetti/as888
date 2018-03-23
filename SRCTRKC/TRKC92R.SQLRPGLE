000100141028      //--------------------------------------------------------------
000200150206      //?TRKC92R - AGGIORNA COMM.LE UNIFICANTE + RAMO AZIENDALE
000300141028      //--------------------------------------------------------------
000400141028
000500141028     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141028
000700141028      //---------------------------------------------------------------
000800141028      //?Dichiarazione file.
000900141028      //---------------------------------------------------------------
001000141024
001100141024      // - Organigramma
001200141024     fAZORG01L  if   e           k disk
001300141029     fAZORG02L  if   e           k disk    rename(AZORG:AZORG02)
001400141029
001500141029      // - Anagrafica Commerciali
001600141029     fAZCMM01L  if   e           k disk
001700141024
001800150206      // - File Info Clienti in Campagna
001900150206     fTICMI01L  uf   e           k disk
002000150206
002100150206      // - Printer File
002200150206     fTRKC92P   o    e             printer oflind(*in99)
002300150206
002400141028      //---------------------------------------------------------------
002500141028      //?Definizione costanti.
002600141028      //---------------------------------------------------------------
002700141028
002800141028     d Digits          c                   const('0123456789')
002900141030
003000141028      //---------------------------------------------------------------
003100141028      //?Definizione schiere.
003200141028      //---------------------------------------------------------------
003300150206     d skCmmO          s                   like(CMMcod) dim(999) inz
003400150206     d skCmmN          s                   like(CMMcod) dim(999) inz
003500141028
003600141028      //---------------------------------------------------------------
003700141028      //?Definizione aree dati.
003800141028      //---------------------------------------------------------------
003900141028
004000141028      // - Dati utente
004100141028     d §AzUte        e ds                  extname(AZUTE00F)
004200141028     d                                     dtaara
004300141028     d §DatiUte      e ds                  extname(dDatiUte)
004400141028     d                                     dtaara
004500141028
004600141028      //---------------------------------------------------------------
004700141028      //?Definizione strutture dati.
004800141028      //---------------------------------------------------------------
004900141028
005000141028      // - Status
005100141028     d Psds           sds
005200141028     d   SDSpgm          *proc
005300141028
005400141028      // - Parametri ricevuti
005500141028     d KPJBA         e ds
005600150206
005700150206      // - File Anagrafica Campagne
005800150206     d TICMP00F      e ds                  extname(TICMP00F)
005900141028
006000141028      // - Reperimento dati utente
006100141027     d TIBS34DS      e ds
006200141027
006300141027      // - Reperimento dati Anagrafica Clienti
006400141027      /copy gaitrasrc/srcprotopi,TIBS69R
006500141028
006600141028      //---------------------------------------------------------------
006700141028      //?Definizione variabili globali.
006800141028      //---------------------------------------------------------------
006900141028
007000141028      // - Flags booleani
007100150206     d AggCMI          s               n   inz(*off)
007200141024     d Fine            s               n   inz(*off)
007300150206     d OkStampa        s               n   inz(*off)
007400150206
007500150206      // - Indici di schiera
007600150206     d xx              s              4s 0 inz
007700141028
007800141028      // - Campi di comodo data
007900141024     d Data_EUR        s               d   datfmt(*eur)
008000141024     d Data_ISO        s               d   datfmt(*iso)
008100150203     d wData           s              8s 0 inz
008200150206     d Oggi            s              8s 0 inz
008300141028
008400141028      //---------------------------------------------------------------
008500141028      //?Definizione procedure usate.
008600141028      //---------------------------------------------------------------
008700141028
008800141028      //---------------------------------------------------------------
008900141024      //?Definizione Prototipi.
009000141028      //---------------------------------------------------------------
009100141024      /copy gaitrasrc/srcprotopr,TIBS34R
009200141027      /copy gaitrasrc/srcprotopr,TIBS69R
009300141028
009400141028      //---------------------------------------------------------------
009500141028      //?Definizione key-list.
009600141028      //---------------------------------------------------------------
009700141028
009800141028      //---------------------------------------------------------------
009900141028      //?M A I N - L I N E
010000141028      //---------------------------------------------------------------
010100141028
010200141028     c     *Entry        plist
010300141028     c                   parm                    kpjba
010400141028
010500141028      /free
010600141028
010700141028       //?Operazioni iniziali
010800141028       exsr RoutInz;
010900141028
011000150206       //?Elaborazione
011100150206       exsr Elabora;
011200150206
011300150206       //?Stampo o commerciali variati
011400150206       exsr Stampa;
011500141028
011600141028       //?Operazioni finali
011700141028       exsr RoutEnd;
011800141028
011900141028       //--------------------------------------------------------------
012000141028       //?Operazioni iniziali.
012100141028       //--------------------------------------------------------------
012200141028       BEGSR RoutInz;
012300141028
012400141028         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500141028
012600150206       //?Imposto nome programma
012700150206         NomePgm = SDSpgm;
012800141029
012900150206       //?Imposto oggi + data e ora elaborazione
013000141029         Oggi = %dec(%date());
013100150206         Data_ISO = %date(Oggi:*ISO);
013200150206         Data_EUR = Data_ISO;
013300150206         DataEla   = %dec(Data_EUR);
013400150206         OraEla    = %dec(%time());
013500141028
013600141028       //?Reperimento dati job
013700141028         exsr DatiJob;
013800150206
013900150206       //?Imposto se devo aggiornare il file TICMC o solo stampare
014000150206         IF  %subst(kpjbu:1:3) = 'AGG';
014100150206           AggCMI = *on;
014200150206         ELSE;
014300150206           AggCMI = *off;
014400150206         ENDIF;
014500141028
014600141028       ENDSR;
014700141028
014800141028       //--------------------------------------------------------------
014900141028       //?Reperimento Dati del job (Utente/Operativi).
015000141028       //--------------------------------------------------------------
015100141028       BEGSR DatiJob;
015200141028
015300141028         in(E) §AzUte;
015400141024         IF  NOT %error;
015500141028           in(E) §DatiUte;
015600141024         ENDIF;
015700141024         IF  %error or RSut = *blanks;
015800141028           clear TIBS34ds;
015900141028           tibs34r(tibs34ds);
016000141028           in §AzUte;
016100141028           in §DatiUte;
016200141024         ENDIF;
016300141028
016400141028       ENDSR;
016500141024
016600141024       //--------------------------------------------------------------
016700150206       //?Elaborazione.
016800141024       //--------------------------------------------------------------
016900150206       BEGSR Elabora;
017000141024
017100150206       //?Leggo file Campagne - solo le attive
017200150206         exec sql
017300150206         declare CMP cursor for
017400150206         select CMPncm from TICMP00F
017500150507         where CMPdic <= :Oggi and CMPdfc >= :Oggi
017600150206         order by CMPncm;
017700150206
017800150206         exec sql
017900150206         open CMP;
018000150206         IF  sqlcode < 0;
018100150206           Fine = *on;
018200150206         ENDIF;
018300150206
018400150206         DOW not Fine;
018500150206           exec sql
018600150206           fetch next from CMP into :CMPncm;
018700150206           IF  sqlcod = 100 or sqlcod < 0;
018800150206             Fine = *on;
018900150206             leave;
019000150206           ENDIF;
019100150206
019200150206         //?Per ogni campagna ancora attiva leggo i clienti in campagna
019300150206           exsr LeggiCMI;
019400150206
019500150206         ENDDO;
019600150206
019700150206         exec sql close CMP;
019800150203
019900150203       ENDSR;
020000150206
020100150206       //--------------------------------------------------------------
020200150206       //?Leggo i clienti in campagna.
020300150206       //--------------------------------------------------------------
020400150206       BEGSR LeggiCMI;
020500150206
020600150206         setll CMPncm TICMI01L;
020700150206         reade CMPncm TICMI01L;
020800150206         DOW  not %eof(TICMI01L);
020900150206
021000150206       //?Aggancio l'anagrafica clienti
021100150206       //?per verificare se il commerciale è cambiato
021200150206           clear TIBS69DS;
021300150206           I69kcp = CMIksu;
021400150206           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
021500150206           chain CLPage AZCMM01L;
021600150206           IF  %found(AZCMM01L) and CMMuni <> CMIcmm;
021700150206             exsr Aggiorna;
021800150206           ELSE;
021900150206             unlock TICMI01L;
022000150206           ENDIF;
022100150206
022200150206           reade CMIncm TICMI01L;
022300150206         ENDDO;
022400150206
022500150206       ENDSR;
022600150206
022700150206       //--------------------------------------------------------------
022800150206       //?Aggiorno il comm.le unificante + ramo aziendale.
022900150206       //--------------------------------------------------------------
023000150206       BEGSR Aggiorna;
023100150206
023200150206         xx = %lookup(CMIcmm : skCMMo);
023300150206         IF  xx = 0;
023400150206           xx = %lookup(*zeros:skCMMo);
023500150206           skCMMo(xx) = CMIcmm;
023600150206           skCMMn(xx) = CMMuni;
023700150206         ENDIF;
023800150206
023900150206         IF  not AggCMI;
024000150206           unlock TICMI01L;
024100150206           leavesr;
024200150206         ENDIF;
024300150206
024400150206         CMIcmm = CMMuni;
024500150206         CMIfcm = CMIcmm / 10000;
024600150206         chain  CMIfcm AZORG01L;
024700150206         IF  %found(AZORG01L);
024800150206           CMIdcm = ORGfl3;
024900150206           CMIacm = ORGcar;
025000150206         ENDIF;
025100150206
025200150206         UPDATE TICMI000;
025300150206
025400150206       ENDSR;
025500150206
025600150206       //--------------------------------------------------------------
025700150206       //?Stampa commerciali variati.
025800150206       //--------------------------------------------------------------
025900150206       BEGSR Stampa;
026000150206
026100150206         *in99 = *on;
026200150206
026300150206         xx = 1;
026400150206         FOR xx by 1 to %elem(skCMMo);
026500150206           IF  skCMMo(xx) = 0;
026600150206             iter;
026700150206           ENDIF;
026800150206           IF  *in99;
026900150206             write KC92TESTA;
027000150206             *in99 = *off;
027100150206           ENDIF;
027200150206
027300150206           OkStampa = *on;
027400150206
027500150206           pCMMold = skCMMo(xx);
027600150206           pCMMnew = skCMMn(xx);
027700150206
027800150206           pFILold = skCMMo(xx) / 10000;
027900150206           pFILnew = skCMMn(xx) / 10000;
028000150206
028100150206           chain pFILold AZORG01L;
028200150206           IF  %found(AZORG01L);
028300150206             pDISold = ORGfl3;
028400150206             pCARold = ORGcar;
028500150206           ENDIF;
028600150206           chain pFILnew AZORG01L;
028700150206           IF  %found(AZORG01L);
028800150206             pDISnew = ORGfl3;
028900150206             pCARnew = ORGcar;
029000150206           ENDIF;
029100150206
029200150206           write KC92RIGA;
029300150206
029400150206         ENDFOR;
029500150206
029600150206       //?Stampa fine
029700150206         IF  OkStampa;
029800150206           write KC92FINE;
029900150206         ENDIF;
030000150206
030100150206       ENDSR;
030200141028
030300141028       //--------------------------------------------------------------
030400141028       //?Operazioni finali.
030500141028       //--------------------------------------------------------------
030600141028       BEGSR RoutEnd;
030700150206
030800141028         *inLR = *on;
030900141028         return;
031000141028
031100141028       ENDSR;
031200141028
031300141028      /end-free
