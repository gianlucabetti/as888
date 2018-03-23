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
008900101217       // - Nome libreria del file TIVIS
009000101217     d wLibVis         s             10    inz
009100101217
009200101217       // - Nome libreria del file TITAS
009300101217     d wLibTas         s             21    inz
009400101217
009500101217       // - Stringhe SQL da eseguire
009600101217     d wSQL            s           2048    Varying        inz
009700091029
009800091029      //---------------------------------------------------------------
009900091029      //?Definizione procedure esterne.
010000091029      //---------------------------------------------------------------
010100110301
010200110301      // Controllo se altre attività aperte
010300110301     d TRMK85R         pr                  extpgm('TRMK85R')
010400110301     d  trmk85ds                           likeds(trmk85ds)
010500101216
010600101216      //---------------------------------------------------------------
010700101216      //?Definizione prototipi.
010800101216      //---------------------------------------------------------------
010900101216
011000101216      /copy gaitrasrc/srcprotopr,xsrda8
011100091029
011200091029      //---------------------------------------------------------------
011300091029      //?Definizione key-list.
011400091029      //---------------------------------------------------------------
011500091029
011600091029      //---------------------------------------------------------------
011700091029      //?Riepilogo indicatori.
011800091029      //---------------------------------------------------------------
011900091029
012000091029      //---------------------------------------------------------------
012100091029      //?M A I N - L I N E
012200091029      //---------------------------------------------------------------
012300091029
012400091029     c     *Entry        plist
012500091029     c                   parm                    KPJBA
012600101216     c                   parm                    TRMK04ds
012700110301     c                   parm                    TRMK85ds
012800091029
012900091029      /free
013000091029
013100091029       //?Operazioni iniziali
013200101216       exsr RoutInz;
013300101217
013400101217       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013500101217
013600101217       //?Carico tabella IC
013700101217       exsr CarTabIc;
013800091029
013900101216       //?Elabora
014000091029       DOW  $Fine = *off;
014100091029
014200101216         exsr Elabora;
014300101217         //?se ho il flag $AcoBlc impostato vuol dire che tutti i KSC legati al
014400110131         //?potenziale sono bloccati --> categoria potenziale Perso
014500110131         //?non torno se il potenziale lavora o non lavora
014600101217         IF  $AcoBlc;
014700110131           OMK04cat = 'P';
014800101217         ENDIF;
014900091029
015000091029       ENDDO;
015100091029
015200091029       //?Operazioni finali
015300101216       exsr RoutEnd;
015400091029
015500091029       //--------------------------------------------------------------
015600091029       //?Operazioni iniziali.
015700091029       //--------------------------------------------------------------
015800101216       BEGSR RoutInz;
015900101216
016000110131         clear OMK04lav;
016100110204         clear OMK04ksc;
016200110218         clear OMK04cmm;
016300110204         clear OMK04dus;
016400110207         clear OMK04clv;
016500110131         clear OMK04cat;
016600101216         clear OMK04err;
016700101216         clear OMK04msg;
016800091029
016900101216         //?Se Non ho il potenziale esco dal pgm con errore
017000101216         IF  IMK04CPO = 0;
017100101216           OMK04err = '1';
017200101216           OMK04msg = 'Potenziale non passato';
017300091029           $Fine = *on;
017400091029           leavesr;
017500091029         ENDIF;
017600101217
017700101217         //?Imposto libreria per TIVIS00F
017800101217         IF  %subst(knsif : 7 : 1) = 'P';
017900101217           wLibVis = 'FILTRAGRPF';
018000101217         ELSE;
018100101217           wLibVis = 'FILTRAGRU ';
018200101217         ENDIF;
018300101217
018400101217         //?Imposto libreria/file per TITAS00F
018500101217         IF  %subst(knsif : 7 : 1) = 'P';
018600101217           wLibTas = 'GAITRAGRPS/TITAS02L';
018700101217         ELSE;
018800101217           wLibTas = 'GAITRAGRU/TITAS02L';
018900101217         ENDIF;
019000101217         open TITAS02L;
019100110301
019200110301       //?Imposto flag che indica la ricezione della ds x controllo attività
019300110301       //?in essere sul cliente, con esclusione di quella in gestione (da TRMK19R)
019400110329        IF  %parms > 2;
019500110301          $Mk85 = *on;
019600110301        ENDIF;
019700101216
019800101216         //?Data del giorno
019900101216         wDataOggi = %dec(%date());
020000091029
020100091029       ENDSR;
020200101217
020300101217       //--------------------------------------------------------------
020400101217       //?Carico Tabella IC.
020500101217       //--------------------------------------------------------------
020600101217       BEGSR CarTabIC;
020700101217
020800101217         //?Carico tabella IC in sk
020900101217         clear $ICcod;
021000101217         clear $ICgg;
021100101217         clear xx;
021200101217         $End = *off;
021300101217
021400101217         exec sql
021500101217          declare TAB cursor for
021600101217          select tblkey, tbluni
021700101217          from tabel00f where tblkut = '1' and tblcod = 'IC' and
021800101217          tblflg = ' ' order by tblkut, tblcod, tblkey;
021900101217
022000101217         exec sql
022100101217           open TAB;
022200101217           IF sqlcode < 0;
022300101217             $End = *on;
022400101217           ENDIF;
022500101217
022600101217         DOW not $End;
022700101217           exec sql
022800101217             fetch next from TAB into :tblkey, :tbluni;
022900101217             IF sqlcod = 100 or sqlcod < 0;
023000101217               $End = *on;
023100101217               leave;
023200101217             ENDIF;
023300101217
023400101217             dsIC = Tbluni;
023500101217             xx += 1;
023600101217             $ICcod(xx) = TBLkey;
023700101217             $ICgg(xx)  = §SICgg;
023800101217
023900101217         ENDDO;
024000101217
024100101217         exec sql close TAB;
024200101217         $End = *off;
024300101217
024400101217       ENDSR;
024500101216
024600101216       //--------------------------------------------------------------
024700101216       //?Elabora.
024800101216       //--------------------------------------------------------------
024900101216       BEGSR Elabora;
025000110301
025100110301         //?Se il pgm è stato richiamo con la ds TRMK85 il controllo
025200110301         //?delle attività da eseguire lo faccio con il relativo PGM
025300110301         IF  $Mk85;
025400110530
025500110530         // aggiunto il controllo el potenziale passato
025600110530         // se non passato vuol dire che il programma deve verificare
025700110530         // solo se ha delle trattative aperte o se lavora
025800110530         // questo per adesso solo nel caso di richiamo dal trmk19r
025900110530         // con causale 70  richiesta da Cuffrao 05/2011
026000110530
026100110530           If imk85cpo <> *zeros ;
026200110530
026300110301           trmk85r (trmk85ds);
026400110301         //?se torna errore <> "9" ho trovato delle attività aperte
026500110301           IF  OMK85err <> *blanks and OMK85err <> '9';
026600110301         //?in questo caso vuol dire che il potenziale lavora
026700110301             OMK04lav = 'SI';
026800110301             $Fine = *on;
026900110301             leavesr;
027000110301           ENDIF;
027100110530           ENDIF;
027200110301
027300110301         ELSE;
027400110407         //?Controllo se ci sono attività 70/75 da eseguire sul potenziale
027500110131         //?se ne trovo vuol dire che il potenziale lavora
027600110301           clear wNrAtc;
027700110301           exec sql
027800110301           select count(*) into :wNrAtc from TIATC00F
027900110407           where ATCcpo = :IMK04cpo and ATCdco = 0 and
028000110407           ATCcad in('70', '75');
028100110301           IF  wNrAtc > 0 ;
028200110301             OMK04lav = 'SI';
028300110301             $Fine = *on;
028400110301             leavesr;
028500110301           ENDIF;
028600110301         ENDIF;
028700101217
028800110131         //?Controllo se ci sono trattative aperte sul potenziale
028900110204         //?se ne trovo vuol dire che il potenziale lavora
029000101217         $End = *off;
029100101217         clear wNrVis;
029200101217         IF  %subst(knsif : 7 : 1) = 'P';
029300110204           exec sql
029400110204           select count(*) into :wNrVis from filtragrpf/tivis00f
029500110204           where viscpo = :IMK04cpo and visdch = 0 and visffz = ' ';
029600110204         ELSE;
029700110204           exec sql
029800110204           select count(*) into :wNrVis from filtragru/tivis00f
029900110204           where viscpo = :IMK04cpo and visdch = 0 and visffz = ' ';
030000110204         ENDIF;
030100101217         IF  wNrVis > 0;
030200110131           OMK04lav = 'SI';
030300101217           $Fine = *on;
030400101217           leavesr;
030500101217         ENDIF;
030600101216
030700101216         //?Cerco tutti i clienti con il potenziale ricevuto
030800101217         $AcoBlc = *off;
030900101217         $End = *off;
031000101216         setll IMK04cpo CNACO16L;
031100110131         //?Nessun KSC associato il potenziale è un mai codificato, imposto la
031200110131         //?categoria del potenziale ed esco dal ciclo
031300110131         IF  not %Equal(CNACO16L);
031400110131           OMK04cat = 'M';
031500101216           $Fine = *on;
031600101216           leavesr;
031700101216         ENDIF;
031800101216
031900101216         DOW not $End;
032000101216           reade IMK04cpo CNACO16L;
032100101216           IF  %eof(CNACO16L);
032200101217             $Fine = *on;
032300110204             leavesr;
032400101216           ENDIF;
032500110204
032600110204           //?Cliente bloccato leggo altro codice
032700110204           IF  ACOabl <> *blanks;
032800110204             $AcoBlc = *on;
032900110204             iter;
033000110204           ENDIF;
033100101216
033200110204           $AcoBlc = *off;
033300110204           chain (ACOkut:ACOkcc:ACOksc) CNCLP00F;
033400110204           IF  not %found(CNCLP00F);
033500110204             OMK04err = '1';
033600110204             OMK04msg = 'Mancano dati anagrafici cliente ' +
033700110204                         %editc(ACOksc:'X');
033800110204             $Fine = *on;
033900110204             leavesr;
034000110204           ENDIF;
034100101216
034200110131           //?Cerco giorni no spedizioni
034300110204           clear wggSPE;
034400110131           //?O passati dal chiamante
034500110204           IF  IMK04ggspe > 0;
034600110204             wggSPE = IMK04ggspe;
034700110204           ELSE;
034800110204           //?O in base al codice importanza cliente
034900110204           //?Il codice importanza cliente può essere passata dal chiamante (da potenziale)
035000110204           //?o la recupero dal KSC
035100110204           //?Codice importanza cliente passato dal chiamante
035200110204             IF  IMK04clv <> *blanks;
035300110204               xx = %lookup(IMK04clv:$ICcod);
035400110204             //?Recupero dal cliente
035500110204             ELSE;
035600110204               xx = %lookup(CLPclv:$ICcod);
035700110204             ENDIF;
035800110204             IF xx > 0;
035900110204               wggSPE = $ICgg(xx);
036000110204             ENDIF;
036100110204           ENDIF;
036200110204
036300110204           clear wData;
036400110204           SELECT;
036500101216
036600110204             //?C'è data ultima spedizione fattura
036700110204             //?confronto con data limite
036800101216             WHEN  CLPdus > 0;
036900110204               //?da 6 la porto lunga 8
037000101216               clear wlbdat;
037100101216               g02inv = CLPdus;
037200101216               g02err = '3';
037300101216               xsrda8(wlbdat);
037400101216               wData  = g02inv;
037500110204               //?metto le date in formato ISO
037600101216               wDataISO1 = %date(wDataOggi:*iso);
037700101216               wDataISO2 = %date(wData:*iso);
037800110204               //?i giorni previsti in tabella sono maggiori/uguali
037900110204               //?il cliente lavora esco dal controllo
038000110204               IF  wggSPE >= %diff(wDataISO1 : wDataISO2 : *days);
038100110204                 $Fine = *on;
038200110204                 OMK04lav = 'SI';
038300110204                 clear OMK04ksc;
038400110218                 clear OMK04cmm;
038500110204                 clear OMK04dus;
038600110207                 clear OMK04clv;
038700110204                 leavesr;
038800110204               ENDIF;
038900110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
039000110204               //?Controllo se ci sono spedizioni ancora da fatturare
039100110204               chain ACOksc TITAS02L;
039200110204               //?ci sono spedizioni il cliente lavora
039300110204               //?esco dal controllo
039400110204               IF  %found(TITAS02L);
039500110204                 //?esco dal controllo
039600110204                 $Fine = *on;
039700110204                 OMK04lav = 'SI';
039800110204                 clear OMK04ksc;
039900110218                 clear OMK04cmm;
040000110204                 clear OMK04dus;
040100110207                 clear OMK04clv;
040200110204                 leavesr;
040300110204               ENDIF;
040400110204               //?Se arrivo qua vuol dire che questo cliente non lavora
040500110204               OMK04lav = 'NO';
040600110204               //?mi memorizzo il codice del cliente che ha la data
040700110204               //?ultima spedizione più alta
040800110204               IF  wData >= OMK04dus;
040900110204                 OMK04ksc = ACOksc;
041000110218                 OMK04cmm = CLPage;
041100110204                 OMK04dus = wData;
041200110207                 OMK04clv = CLPclv;
041300110204               ENDIF;
041400101216
041500110204             //?Non c'è data ultima spedizione fattura
041600110204             //?controllo quando è stato aperto il codice
041700101216             WHEN  CLPdus = 0;
041800110204               //?metto data minima ISO
041900110207               wDataISO1 = %date(wDataOggi:*iso);
042000101216               wData = 00010101;
042100101216               wDataISO2 = %date(wData:*iso);
042200101216               $EndCvt = *off;
042300110204               //?cerco il rcd di inserimento nel file delle variazioni
042400110204               //?per recuperare la data di apertura codice
042500101216               setll (ACOkcc:ACOksc) CNACVT1L;
042600101216               DOW  not $EndCvt;
042700101216                 reade (ACOkcc:ACOksc) CNACVT1L;
042800101216                 IF  %eof(CNACVT1L);
042900101216                   $EndCvt = *on;
043000101216                 ENDIF;
043100101216                 IF  not %eof(CNACVT1L) and ACVcta <> 'M';
043200101217                   wDataISO2 = %date(ACVdav:*iso);
043300110204                   wData = ACVdav;
043400101216                   $EndCvt = *on;
043500101216                 ENDIF;
043600101216               ENDDO;
043700110131
043800110131               //?Giorni apertura codice passati dal chiamante
043900110131               IF  IMK04ggape > 0;
044000110131                 wggAPE = IMK04ggape;
044100110131               //?Giorni apertura codice passati = a gg no spedizioni
044200110131               ELSE;
044300110131                 wggAPE = wggSPE;
044400110131               ENDIF;
044500101216
044600110204               //?I giorni apertura codice sono maggiori/uguali
044700110204               //?il cliente lavora esco dal controllo
044800110207               IF  wggAPE >= %diff(wDataISO1 : wDataISO2 : *days);
044900110204                 OMK04lav = 'SI';
045000110204                 clear OMK04ksc;
045100110218                 clear OMK04cmm;
045200110204                 clear OMK04dus;
045300110207                 clear OMK04clv;
045400110204                 $Fine = *on;
045500110204                 leavesr;
045600110204               ENDIF;
045700110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
045800110204               //?Controllo se ci sono spedizioni ancora da fatturare
045900110204               chain ACOksc TITAS02L;
046000110204               //?ci sono spedizioni il cliente lavora esco dal controllo
046100110204               IF  %found(TITAS02L);
046200110204                 OMK04lav = 'SI';
046300110204                 clear OMK04ksc;
046400110218                 clear OMK04cmm;
046500110204                 clear OMK04dus;
046600110207                 clear OMK04clv;
046700110204                 $Fine = *on;
046800110204                 leavesr;
046900101216               ENDIF;
047000110204               //?Se arrivo qua vuol dire che questo cliente non lavora
047100110204               OMK04lav = 'NO';
047200110204               //?mi memorizzo il codice del cliente che ha la data
047300110204               //?di apertura codice più alta
047400110204               IF  wData >= OMK04dus;
047500110204                 OMK04ksc = ACOksc;
047600110218                 OMK04cmm = CLPage;
047700110204                 OMK04dus = wData;
047800110207                 OMK04clv = CLPclv;
047900110204               ENDIF;
048000101217
048100110204           ENDSL;
048200101216
048300101216         ENDDO;
048400101216
048500101216       ENDSR;
048600091029
048700091029       //--------------------------------------------------------------
048800091029       //?Operazioni finali.
048900091029       //--------------------------------------------------------------
049000101216       BEGSR RoutEnd;
049100091029
049200091029         *inLR = *on;
049300091029         return;
049400091029
049500091029       ENDSR;
049600091029
049700091029      /end-free
