000100091029      //---------------------------------------------------------------
000200110131      //?TRMK04R - Controlla se potenziale lavora
000300091029      //---------------------------------------------------------------
000400091029
000500091029     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600091029     h dftactgrp(*no) actgrp(*caller)
000700091029
000800091029      //---------------------------------------------------------------
000900091029      //?Dichiarazione file.
001000091029      //---------------------------------------------------------------
001100101217      // - File Anagrafica clienti
001200101217     fCNACO16L  if   e           k disk
001300101217     fCNCLP00F  if   e           k disk
001400101217      // - File Variazioni anagrafiche
001500101217     fCNACVT1L  if   e           k disk
001600101217      // - File Bolle sede
001700101217     fTITAS02L  if   e           k disk    usropn
001800101217     f                                     extfile(wLibTas)
001900091029
002000091029      //---------------------------------------------------------------
002100091029      //?Definizione costanti.
002200091029      //---------------------------------------------------------------
002300091029
002400091029     d Digits          c                   const('0123456789')
002500091029
002600091029      //---------------------------------------------------------------
002700091029      //?Definizione schiere.
002800091029      //---------------------------------------------------------------
002900101216
003000101216      // - Codici importanza clienti
003100101216     d $ICcod          s              1    dim(10)
003200101216
003300110131      // - Giorni no spedizioni
003400101216     d $ICgg           s              3  0 dim(10)
003500091029
003600091029      //---------------------------------------------------------------
003700091029      //?Definizione aree dati.
003800091029      //---------------------------------------------------------------
003900091029
004000091029      //---------------------------------------------------------------
004100091029      //?Definizione strutture dati.
004200091029      //---------------------------------------------------------------
004300101216
004400101216      // - Controllo data
004500101216     d wlbdat          ds                  inz
004600101216     d  g02dat                 1      8  0
004700101216     d  g02inv                 9     16  0
004800101216     d  g02err                17     17
004900101216     d  g02tgi                18     22  0
005000091029
005100091029      // - Parametri ricevuti
005200091029     d KPJBA         e ds
005300101216     d TRMK04ds      e ds
005400110301     d TRMK85ds      e ds
005500091029
005600101216      // Tabella Importanza clienti
005700101216     d dsIC          e ds
005800101216
005900101217      // File Tabelle
006000101216     d TABEL00F      e ds                  extname(TABEL00F)
006100101217
006200101217      // File Trattative
006300101217     d TIVIS00F      e ds                  extname(TIVIS00F)
006400091029
006500091029      //---------------------------------------------------------------
006600091029      //?Definizione variabili globali.
006700091029      //---------------------------------------------------------------
006800091029
006900091029      // - Flags booleani
007000101217     d $AcoBlc         s               n   inz(*off)
007100091029     d $End            s               n   inz(*off)
007200101216     d $EndCvt         s               n   inz(*off)
007300091029     d $Fine           s               n   inz(*off)
007400110301     d $Mk85           s               n   inz(*off)
007500101216
007600101216      // - Indici di schiera
007700101216     d xx              s              4  0 inz
007800091029
007900091029      // - Campi di comodo
008000101216     d wData           s              8  0
008100101216     d wDataOggi       s              8  0
008200101216     d wDataISO1       s               d   datfmt(*iso)
008300101216     d wDataISO2       s               d   datfmt(*iso)
008400110131     d wggAPE          s              3  0
008500110131     d wggSPE          s              3  0
008600101217     d wNrAtc          s              5i 0
008700101217     d wNrVis          s              5i 0
008800101217
008900101217       // - Nome libreria del file TITAS
009000101217     d wLibTas         s             21    inz
009100101217
009200101217       // - Stringhe SQL da eseguire
009300101217     d wSQL            s           2048    Varying        inz
009400091029
009500091029      //---------------------------------------------------------------
009600091029      //?Definizione procedure esterne.
009700091029      //---------------------------------------------------------------
009800110301
009900110301      // Controllo se altre attività aperte
010000110301     d TRMK85R         pr                  extpgm('TRMK85R')
010100110301     d  trmk85ds                           likeds(trmk85ds)
010200101216
010300101216      //---------------------------------------------------------------
010400101216      //?Definizione prototipi.
010500101216      //---------------------------------------------------------------
010600101216
010700101216      /copy gaitrasrc/srcprotopr,xsrda8
010800091029
010900091029      //---------------------------------------------------------------
011000091029      //?Definizione key-list.
011100091029      //---------------------------------------------------------------
011200091029
011300091029      //---------------------------------------------------------------
011400091029      //?Riepilogo indicatori.
011500091029      //---------------------------------------------------------------
011600091029
011700091029      //---------------------------------------------------------------
011800091029      //?M A I N - L I N E
011900091029      //---------------------------------------------------------------
012000091029
012100091029     c     *Entry        plist
012200091029     c                   parm                    KPJBA
012300101216     c                   parm                    TRMK04ds
012400110301     c                   parm                    TRMK85ds
012500091029
012600091029      /free
012700091029
012800091029       //?Operazioni iniziali
012900101216       exsr RoutInz;
013000101217
013100101217       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013200101217
013300101217       //?Carico tabella IC
013400101217       exsr CarTabIc;
013500091029
013600101216       //?Elabora
013700091029       DOW  $Fine = *off;
013800091029
013900101216         exsr Elabora;
014000101217         //?se ho il flag $AcoBlc impostato vuol dire che tutti i KSC legati al
014100110131         //?potenziale sono bloccati --> categoria potenziale Perso
014200110131         //?non torno se il potenziale lavora o non lavora
014300101217         IF  $AcoBlc;
014400110131           OMK04cat = 'P';
014500101217         ENDIF;
014600091029
014700091029       ENDDO;
014800091029
014900091029       //?Operazioni finali
015000101216       exsr RoutEnd;
015100091029
015200091029       //--------------------------------------------------------------
015300091029       //?Operazioni iniziali.
015400091029       //--------------------------------------------------------------
015500101216       BEGSR RoutInz;
015600101216
015700110131         clear OMK04lav;
015800110204         clear OMK04ksc;
015900110218         clear OMK04cmm;
016000110204         clear OMK04dus;
016100110207         clear OMK04clv;
016200110131         clear OMK04cat;
016300101216         clear OMK04err;
016400101216         clear OMK04msg;
016500091029
016600101216         //?Se Non ho il potenziale esco dal pgm con errore
016700101216         IF  IMK04CPO = 0;
016800101216           OMK04err = '1';
016900101216           OMK04msg = 'Potenziale non passato';
017000091029           $Fine = *on;
017100091029           leavesr;
017200091029         ENDIF;
017300101217
017400101217
017500101217         //?Imposto libreria/file per TITAS00F
017600101217         IF  %subst(knsif : 7 : 1) = 'P';
017700101217           wLibTas = 'GAITRAGRPS/TITAS02L';
017800101217         ELSE;
017900101217           wLibTas = 'GAITRAGRU/TITAS02L';
018000101217         ENDIF;
018100101217         open TITAS02L;
018200110301
018300110301       //?Imposto flag che indica la ricezione della ds x controllo attività
018400110301       //?in essere sul cliente, con esclusione di quella in gestione (da TRMK19R)
018500110329        IF  %parms > 2;
018600110301          $Mk85 = *on;
018700110301        ENDIF;
018800101216
018900101216         //?Data del giorno
019000101216         wDataOggi = %dec(%date());
019100091029
019200091029       ENDSR;
019300101217
019400101217       //--------------------------------------------------------------
019500101217       //?Carico Tabella IC.
019600101217       //--------------------------------------------------------------
019700101217       BEGSR CarTabIC;
019800101217
019900101217         //?Carico tabella IC in sk
020000101217         clear $ICcod;
020100101217         clear $ICgg;
020200101217         clear xx;
020300101217         $End = *off;
020400101217
020500101217         exec sql
020600101217          declare TAB cursor for
020700101217          select tblkey, tbluni
020800101217          from tabel00f where tblkut = '1' and tblcod = 'IC' and
020900101217          tblflg = ' ' order by tblkut, tblcod, tblkey;
021000101217
021100101217         exec sql
021200101217           open TAB;
021300101217           IF sqlcode < 0;
021400101217             $End = *on;
021500101217           ENDIF;
021600101217
021700101217         DOW not $End;
021800101217           exec sql
021900101217             fetch next from TAB into :tblkey, :tbluni;
022000101217             IF sqlcod = 100 or sqlcod < 0;
022100101217               $End = *on;
022200101217               leave;
022300101217             ENDIF;
022400101217
022500101217             dsIC = Tbluni;
022600101217             xx += 1;
022700101217             $ICcod(xx) = TBLkey;
022800101217             $ICgg(xx)  = §SICgg;
022900101217
023000101217         ENDDO;
023100101217
023200101217         exec sql close TAB;
023300101217         $End = *off;
023400101217
023500101217       ENDSR;
023600101216
023700101216       //--------------------------------------------------------------
023800101216       //?Elabora.
023900101216       //--------------------------------------------------------------
024000101216       BEGSR Elabora;
024100110301
024200110301         //?Se il pgm è stato richiamo con la ds TRMK85 il controllo
024300110301         //?delle attività da eseguire lo faccio con il relativo PGM
024400110301         IF  $Mk85;
024500110530
024600110530         // aggiunto il controllo el potenziale passato
024700110530         // se non passato vuol dire che il programma deve verificare
024800110530         // solo se ha delle trattative aperte o se lavora
024900110530         // questo per adesso solo nel caso di richiamo dal trmk19r
025000110530         // con causale 70  richiesta da Cuffrao 05/2011
025100110530
025200110530           If imk85cpo <> *zeros ;
025300110530
025400110301           trmk85r (trmk85ds);
025500110301         //?se torna errore <> "9" ho trovato delle attività aperte
025600110301           IF  OMK85err <> *blanks and OMK85err <> '9';
025700110301         //?in questo caso vuol dire che il potenziale lavora
025800110301             OMK04lav = 'SI';
025900110301             $Fine = *on;
026000110301             leavesr;
026100110301           ENDIF;
026200110530           ENDIF;
026300110301
026400110301         ELSE;
026500110407         //?Controllo se ci sono attività 70/75 da eseguire sul potenziale
026600110131         //?se ne trovo vuol dire che il potenziale lavora
026700110301           clear wNrAtc;
026800110301           exec sql
026900110301           select count(*) into :wNrAtc from TIATC00F
027000110407           where ATCcpo = :IMK04cpo and ATCdco = 0 and
027100110922           ATCcad in('70', '75', '71');
027200110301           IF  wNrAtc > 0 ;
027300110301             OMK04lav = 'SI';
027400110301             $Fine = *on;
027500110301             leavesr;
027600110301           ENDIF;
027700110301         ENDIF;
027800101217
027900110131         //?Controllo se ci sono trattative aperte sul potenziale
028000110204         //?se ne trovo vuol dire che il potenziale lavora
028100101217         $End = *off;
028200101217         clear wNrVis;
028300110204           exec sql
028400110803           select count(*) into :wNrVis from tivis00f
028500110204           where viscpo = :IMK04cpo and visdch = 0 and visffz = ' ';
028600101217         IF  wNrVis > 0;
028700110131           OMK04lav = 'SI';
028800101217           $Fine = *on;
028900101217           leavesr;
029000101217         ENDIF;
029100101216
029200101216         //?Cerco tutti i clienti con il potenziale ricevuto
029300101217         $AcoBlc = *off;
029400101217         $End = *off;
029500101216         setll IMK04cpo CNACO16L;
029600110131         //?Nessun KSC associato il potenziale è un mai codificato, imposto la
029700110131         //?categoria del potenziale ed esco dal ciclo
029800110131         IF  not %Equal(CNACO16L);
029900110131           OMK04cat = 'M';
030000101216           $Fine = *on;
030100101216           leavesr;
030200101216         ENDIF;
030300101216
030400101216         DOW not $End;
030500101216           reade IMK04cpo CNACO16L;
030600101216           IF  %eof(CNACO16L);
030700101217             $Fine = *on;
030800110204             leavesr;
030900101216           ENDIF;
031000110204
031100110204           //?Cliente bloccato leggo altro codice
031200110204           IF  ACOabl <> *blanks;
031300110204             $AcoBlc = *on;
031400110204             iter;
031500110204           ENDIF;
031600101216
031700110204           $AcoBlc = *off;
031800110204           chain (ACOkut:ACOkcc:ACOksc) CNCLP00F;
031900110204           IF  not %found(CNCLP00F);
032000110204             OMK04err = '1';
032100110204             OMK04msg = 'Mancano dati anagrafici cliente ' +
032200110204                         %editc(ACOksc:'X');
032300110204             $Fine = *on;
032400110204             leavesr;
032500110204           ENDIF;
032600101216
032700110131           //?Cerco giorni no spedizioni
032800110204           clear wggSPE;
032900110131           //?O passati dal chiamante
033000110204           IF  IMK04ggspe > 0;
033100110204             wggSPE = IMK04ggspe;
033200110204           ELSE;
033300110204           //?O in base al codice importanza cliente
033400110204           //?Il codice importanza cliente può essere passata dal chiamante (da potenziale)
033500110204           //?o la recupero dal KSC
033600110204           //?Codice importanza cliente passato dal chiamante
033700110204             IF  IMK04clv <> *blanks;
033800110204               xx = %lookup(IMK04clv:$ICcod);
033900110204             //?Recupero dal cliente
034000110204             ELSE;
034100110204               xx = %lookup(CLPclv:$ICcod);
034200110204             ENDIF;
034300110204             IF xx > 0;
034400110204               wggSPE = $ICgg(xx);
034500110204             ENDIF;
034600110204           ENDIF;
034700110204
034800110204           clear wData;
034900110204           SELECT;
035000101216
035100110204             //?C'è data ultima spedizione fattura
035200110204             //?confronto con data limite
035300101216             WHEN  CLPdus > 0;
035400110204               //?da 6 la porto lunga 8
035500101216               clear wlbdat;
035600101216               g02inv = CLPdus;
035700101216               g02err = '3';
035800101216               xsrda8(wlbdat);
035900101216               wData  = g02inv;
036000110204               //?metto le date in formato ISO
036100101216               wDataISO1 = %date(wDataOggi:*iso);
036200101216               wDataISO2 = %date(wData:*iso);
036300110204               //?i giorni previsti in tabella sono maggiori/uguali
036400110204               //?il cliente lavora esco dal controllo
036500110204               IF  wggSPE >= %diff(wDataISO1 : wDataISO2 : *days);
036600110204                 $Fine = *on;
036700110204                 OMK04lav = 'SI';
036800110204                 clear OMK04ksc;
036900110218                 clear OMK04cmm;
037000110204                 clear OMK04dus;
037100110207                 clear OMK04clv;
037200110204                 leavesr;
037300110204               ENDIF;
037400110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
037500110204               //?Controllo se ci sono spedizioni ancora da fatturare
037600110204               chain ACOksc TITAS02L;
037700110204               //?ci sono spedizioni il cliente lavora
037800110204               //?esco dal controllo
037900110204               IF  %found(TITAS02L);
038000110204                 //?esco dal controllo
038100110204                 $Fine = *on;
038200110204                 OMK04lav = 'SI';
038300110204                 clear OMK04ksc;
038400110218                 clear OMK04cmm;
038500110204                 clear OMK04dus;
038600110207                 clear OMK04clv;
038700110204                 leavesr;
038800110204               ENDIF;
038900110204               //?Se arrivo qua vuol dire che questo cliente non lavora
039000110204               OMK04lav = 'NO';
039100110204               //?mi memorizzo il codice del cliente che ha la data
039200110204               //?ultima spedizione più alta
039300110204               IF  wData >= OMK04dus;
039400110204                 OMK04ksc = ACOksc;
039500110218                 OMK04cmm = CLPage;
039600110204                 OMK04dus = wData;
039700110207                 OMK04clv = CLPclv;
039800110204               ENDIF;
039900101216
040000110204             //?Non c'è data ultima spedizione fattura
040100110204             //?controllo quando è stato aperto il codice
040200101216             WHEN  CLPdus = 0;
040300110204               //?metto data minima ISO
040400110207               wDataISO1 = %date(wDataOggi:*iso);
040500101216               wData = 00010101;
040600101216               wDataISO2 = %date(wData:*iso);
040700101216               $EndCvt = *off;
040800110204               //?cerco il rcd di inserimento nel file delle variazioni
040900110204               //?per recuperare la data di apertura codice
041000101216               setll (ACOkcc:ACOksc) CNACVT1L;
041100101216               DOW  not $EndCvt;
041200101216                 reade (ACOkcc:ACOksc) CNACVT1L;
041300101216                 IF  %eof(CNACVT1L);
041400101216                   $EndCvt = *on;
041500101216                 ENDIF;
041600101216                 IF  not %eof(CNACVT1L) and ACVcta <> 'M';
041700101217                   wDataISO2 = %date(ACVdav:*iso);
041800110204                   wData = ACVdav;
041900101216                   $EndCvt = *on;
042000101216                 ENDIF;
042100101216               ENDDO;
042200110131
042300110131               //?Giorni apertura codice passati dal chiamante
042400110131               IF  IMK04ggape > 0;
042500110131                 wggAPE = IMK04ggape;
042600110131               //?Giorni apertura codice passati = a gg no spedizioni
042700110131               ELSE;
042800110131                 wggAPE = wggSPE;
042900110131               ENDIF;
043000101216
043100110204               //?I giorni apertura codice sono maggiori/uguali
043200110204               //?il cliente lavora esco dal controllo
043300110207               IF  wggAPE >= %diff(wDataISO1 : wDataISO2 : *days);
043400110204                 OMK04lav = 'SI';
043500110204                 clear OMK04ksc;
043600110218                 clear OMK04cmm;
043700110204                 clear OMK04dus;
043800110207                 clear OMK04clv;
043900110204                 $Fine = *on;
044000110204                 leavesr;
044100110204               ENDIF;
044200110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
044300110204               //?Controllo se ci sono spedizioni ancora da fatturare
044400110204               chain ACOksc TITAS02L;
044500110204               //?ci sono spedizioni il cliente lavora esco dal controllo
044600110204               IF  %found(TITAS02L);
044700110204                 OMK04lav = 'SI';
044800110204                 clear OMK04ksc;
044900110218                 clear OMK04cmm;
045000110204                 clear OMK04dus;
045100110207                 clear OMK04clv;
045200110204                 $Fine = *on;
045300110204                 leavesr;
045400101216               ENDIF;
045500110204               //?Se arrivo qua vuol dire che questo cliente non lavora
045600110204               OMK04lav = 'NO';
045700110204               //?mi memorizzo il codice del cliente che ha la data
045800110204               //?di apertura codice più alta
045900110204               IF  wData >= OMK04dus;
046000110204                 OMK04ksc = ACOksc;
046100110218                 OMK04cmm = CLPage;
046200110204                 OMK04dus = wData;
046300110207                 OMK04clv = CLPclv;
046400110204               ENDIF;
046500101217
046600110204           ENDSL;
046700101216
046800101216         ENDDO;
046900101216
047000101216       ENDSR;
047100091029
047200091029       //--------------------------------------------------------------
047300091029       //?Operazioni finali.
047400091029       //--------------------------------------------------------------
047500101216       BEGSR RoutEnd;
047600091029
047700091029         *inLR = *on;
047800091029         return;
047900091029
048000091029       ENDSR;
048100091029
048200091029      /end-free
