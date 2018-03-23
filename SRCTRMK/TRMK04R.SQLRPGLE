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
001600140403      // - File Info commerciali
001700140403     fTICPI01L  if   e           k disk
001800101217      // - File Bolle sede
001900101217     fTITAS02L  if   e           k disk    usropn
002000101217     f                                     extfile(wLibTas)
002100091029
002200091029      //---------------------------------------------------------------
002300091029      //?Definizione costanti.
002400091029      //---------------------------------------------------------------
002500091029
002600091029     d Digits          c                   const('0123456789')
002700091029
002800091029      //---------------------------------------------------------------
002900091029      //?Definizione schiere.
003000091029      //---------------------------------------------------------------
003100101216
003200101216      // - Codici importanza clienti
003300101216     d $ICcod          s              1    dim(10)
003400101216
003500110131      // - Giorni no spedizioni
003600101216     d $ICgg           s              3  0 dim(10)
003700140403     d $ICgg1          s              3  0 dim(10)
003800140403
003900140403      // - Spesa trasporti F14 massima per calcolo GG oltre calola GG1
004000140403     d $ICmax          s             11  0 dim(10)
004100091029
004200091029      //---------------------------------------------------------------
004300091029      //?Definizione aree dati.
004400091029      //---------------------------------------------------------------
004500091029
004600091029      //---------------------------------------------------------------
004700091029      //?Definizione strutture dati.
004800091029      //---------------------------------------------------------------
004900101216
005000101216      // - Controllo data
005100101216     d wlbdat          ds                  inz
005200101216     d  g02dat                 1      8  0
005300101216     d  g02inv                 9     16  0
005400101216     d  g02err                17     17
005500101216     d  g02tgi                18     22  0
005600091029
005700091029      // - Parametri ricevuti
005800091029     d KPJBA         e ds
005900101216     d TRMK04ds      e ds
006000110301     d TRMK85ds      e ds
006100091029
006200101216      // Tabella Importanza clienti
006300101216     d dsIC          e ds
006400101216
006500101217      // File Tabelle
006600101216     d TABEL00F      e ds                  extname(TABEL00F)
006700101217
006800101217      // File Trattative
006900101217     d TIVIS00F      e ds                  extname(TIVIS00F)
007000091029
007100091029      //---------------------------------------------------------------
007200091029      //?Definizione variabili globali.
007300091029      //---------------------------------------------------------------
007400091029
007500091029      // - Flags booleani
007600101217     d $AcoBlc         s               n   inz(*off)
007700091029     d $End            s               n   inz(*off)
007800101216     d $EndCvt         s               n   inz(*off)
007900091029     d $Fine           s               n   inz(*off)
008000110301     d $Mk85           s               n   inz(*off)
008100101216
008200101216      // - Indici di schiera
008300101216     d xx              s              4  0 inz
008400091029
008500091029      // - Campi di comodo
008600101216     d wData           s              8  0
008700101216     d wDataOggi       s              8  0
008800101216     d wDataISO1       s               d   datfmt(*iso)
008900101216     d wDataISO2       s               d   datfmt(*iso)
009000110131     d wggAPE          s              3  0
009100110131     d wggSPE          s              3  0
009200101217     d wNrAtc          s              5i 0
009300101217     d wNrVis          s              5i 0
009400101217
009500101217       // - Nome libreria del file TITAS
009600101217     d wLibTas         s             21    inz
009700101217
009800101217       // - Stringhe SQL da eseguire
009900101217     d wSQL            s           2048    Varying        inz
010000091029
010100091029      //---------------------------------------------------------------
010200091029      //?Definizione procedure esterne.
010300091029      //---------------------------------------------------------------
010400110301
010500110301      // Controllo se altre attività aperte
010600110301     d TRMK85R         pr                  extpgm('TRMK85R')
010700110301     d  trmk85ds                           likeds(trmk85ds)
010800101216
010900101216      //---------------------------------------------------------------
011000101216      //?Definizione prototipi.
011100101216      //---------------------------------------------------------------
011200101216
011300101216      /copy gaitrasrc/srcprotopr,xsrda8
011400091029
011500091029      //---------------------------------------------------------------
011600091029      //?Definizione key-list.
011700091029      //---------------------------------------------------------------
011800091029
011900091029      //---------------------------------------------------------------
012000091029      //?Riepilogo indicatori.
012100091029      //---------------------------------------------------------------
012200091029
012300091029      //---------------------------------------------------------------
012400091029      //?M A I N - L I N E
012500091029      //---------------------------------------------------------------
012600091029
012700091029     c     *Entry        plist
012800091029     c                   parm                    KPJBA
012900101216     c                   parm                    TRMK04ds
013000110301     c                   parm                    TRMK85ds
013100091029
013200091029      /free
013300091029
013400091029       //?Operazioni iniziali
013500101216       exsr RoutInz;
013600101217
013700101217       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013800101217
013900101217       //?Carico tabella IC
014000101217       exsr CarTabIc;
014100140403
014200140403       //?Cerco Info commerciali (Spesa Trasporti)
014300140403       exsr InfoComm;
014400091029
014500101216       //?Elabora
014600091029       DOW  $Fine = *off;
014700091029
014800101216         exsr Elabora;
014900101217         //?se ho il flag $AcoBlc impostato vuol dire che tutti i KSC legati al
015000110131         //?potenziale sono bloccati --> categoria potenziale Perso
015100110131         //?non torno se il potenziale lavora o non lavora
015200101217         IF  $AcoBlc;
015300110131           OMK04cat = 'P';
015400101217         ENDIF;
015500091029
015600091029       ENDDO;
015700091029
015800091029       //?Operazioni finali
015900101216       exsr RoutEnd;
016000091029
016100091029       //--------------------------------------------------------------
016200091029       //?Operazioni iniziali.
016300091029       //--------------------------------------------------------------
016400101216       BEGSR RoutInz;
016500101216
016600110131         clear OMK04lav;
016700110204         clear OMK04ksc;
016800110218         clear OMK04cmm;
016900110204         clear OMK04dus;
017000110207         clear OMK04clv;
017100110131         clear OMK04cat;
017200101216         clear OMK04err;
017300101216         clear OMK04msg;
017400091029
017500101216         //?Se Non ho il potenziale esco dal pgm con errore
017600101216         IF  IMK04CPO = 0;
017700101216           OMK04err = '1';
017800101216           OMK04msg = 'Potenziale non passato';
017900091029           $Fine = *on;
018000091029           leavesr;
018100091029         ENDIF;
018200101217
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
023800140403             $ICgg1(xx) = §SICgg1;
023900140403             $ICmax(xx) = §SICmax;
024000101217
024100101217         ENDDO;
024200101217
024300101217         exec sql close TAB;
024400101217         $End = *off;
024500101217
024600101217       ENDSR;
024700140403
024800140403       //--------------------------------------------------------------
024900140403       //?Cerco Info Commerciali (Spesa Trasporti).
025000140403       //--------------------------------------------------------------
025100140403       BEGSR InfoComm;
025200140403
025300140407         CPItpf = 'SPT';
025400140403         chain (IMK04CPO:CPItpf) TICPI01L;
025500140403         IF  not %found(TICPI01L) or CPIatb <> *blanks;
025600140403           clear  CPIpft;
025700140403         ENDIF;
025800140403
025900140403       ENDSR;
026000101216
026100101216       //--------------------------------------------------------------
026200101216       //?Elabora.
026300101216       //--------------------------------------------------------------
026400101216       BEGSR Elabora;
026500110301
026600110301         //?Se il pgm è stato richiamo con la ds TRMK85 il controllo
026700110301         //?delle attività da eseguire lo faccio con il relativo PGM
026800110301         IF  $Mk85;
026900110530
027000110530         // aggiunto il controllo el potenziale passato
027100110530         // se non passato vuol dire che il programma deve verificare
027200110530         // solo se ha delle trattative aperte o se lavora
027300110530         // questo per adesso solo nel caso di richiamo dal trmk19r
027400110530         // con causale 70  richiesta da Cuffrao 05/2011
027500110530
027600110530           If imk85cpo <> *zeros ;
027700110530
027800110301           trmk85r (trmk85ds);
027900110301         //?se torna errore <> "9" ho trovato delle attività aperte
028000110301           IF  OMK85err <> *blanks and OMK85err <> '9';
028100110301         //?in questo caso vuol dire che il potenziale lavora
028200110301             OMK04lav = 'SI';
028300110301             $Fine = *on;
028400110301             leavesr;
028500110301           ENDIF;
028600110530           ENDIF;
028700110301
028800110301         ELSE;
028900110407         //?Controllo se ci sono attività 70/75 da eseguire sul potenziale
029000110131         //?se ne trovo vuol dire che il potenziale lavora
029100110301           clear wNrAtc;
029200110301           exec sql
029300110301           select count(*) into :wNrAtc from TIATC00F
029400110407           where ATCcpo = :IMK04cpo and ATCdco = 0 and
029500110922           ATCcad in('70', '75', '71');
029600110301           IF  wNrAtc > 0 ;
029700110301             OMK04lav = 'SI';
029800110301             $Fine = *on;
029900110301             leavesr;
030000110301           ENDIF;
030100110301         ENDIF;
030200101217
030300110131         //?Controllo se ci sono trattative aperte sul potenziale
030400110204         //?se ne trovo vuol dire che il potenziale lavora
030500101217         $End = *off;
030600101217         clear wNrVis;
030700110204           exec sql
030800110803           select count(*) into :wNrVis from tivis00f
030900110204           where viscpo = :IMK04cpo and visdch = 0 and visffz = ' ';
031000101217         IF  wNrVis > 0;
031100110131           OMK04lav = 'SI';
031200101217           $Fine = *on;
031300101217           leavesr;
031400101217         ENDIF;
031500101216
031600101216         //?Cerco tutti i clienti con il potenziale ricevuto
031700101217         $AcoBlc = *off;
031800101217         $End = *off;
031900101216         setll IMK04cpo CNACO16L;
032000110131         //?Nessun KSC associato il potenziale è un mai codificato, imposto la
032100110131         //?categoria del potenziale ed esco dal ciclo
032200110131         IF  not %Equal(CNACO16L);
032300110131           OMK04cat = 'M';
032400101216           $Fine = *on;
032500101216           leavesr;
032600101216         ENDIF;
032700101216
032800101216         DOW not $End;
032900101216           reade IMK04cpo CNACO16L;
033000101216           IF  %eof(CNACO16L);
033100101217             $Fine = *on;
033200110204             leavesr;
033300101216           ENDIF;
033400110204
033500110204           //?Cliente bloccato leggo altro codice
033600110204           IF  ACOabl <> *blanks;
033700110204             $AcoBlc = *on;
033800110204             iter;
033900110204           ENDIF;
034000101216
034100110204           $AcoBlc = *off;
034200110204           chain (ACOkut:ACOkcc:ACOksc) CNCLP00F;
034300110204           IF  not %found(CNCLP00F);
034400110204             OMK04err = '1';
034500110204             OMK04msg = 'Mancano dati anagrafici cliente ' +
034600110204                         %editc(ACOksc:'X');
034700110204             $Fine = *on;
034800110204             leavesr;
034900110204           ENDIF;
035000101216
035100110131           //?Cerco giorni no spedizioni
035200110204           clear wggSPE;
035300110131           //?O passati dal chiamante
035400110204           IF  IMK04ggspe > 0;
035500110204             wggSPE = IMK04ggspe;
035600110204           ELSE;
035700110204           //?O in base al codice importanza cliente
035800110204           //?Il codice importanza cliente può essere passata dal chiamante (da potenziale)
035900110204           //?o la recupero dal KSC
036000110204           //?Codice importanza cliente passato dal chiamante
036100110204             IF  IMK04clv <> *blanks;
036200110204               xx = %lookup(IMK04clv:$ICcod);
036300110204             //?Recupero dal cliente
036400110204             ELSE;
036500110204               xx = %lookup(CLPclv:$ICcod);
036600110204             ENDIF;
036700110204             IF xx > 0;
036800140403             //?Se importo Massimo impostato e Spesa trasporti > devo prendere GG1
036900140403             //?cioè n. di giorni inferiore rispetto allo standard
037000140403               IF  $ICmax(xx) > 0 and CPIpft > $ICmax(xx);
037100140403                 wggSPE = $ICgg1(xx);
037200140403               ELSE;
037300140403             //?Se importo Massimo impostato e Spesa trasporti <= o Importo max a 0
037400140403             //?devo prendere GG cioè lo standard
037500110204               wggSPE = $ICgg(xx);
037600140403               ENDIF;
037700110204             ENDIF;
037800110204           ENDIF;
037900110204
038000110204           clear wData;
038100110204           SELECT;
038200101216
038300110204             //?C'è data ultima spedizione fattura
038400110204             //?confronto con data limite
038500101216             WHEN  CLPdus > 0;
038600110204               //?da 6 la porto lunga 8
038700101216               clear wlbdat;
038800101216               g02inv = CLPdus;
038900101216               g02err = '3';
039000101216               xsrda8(wlbdat);
039100101216               wData  = g02inv;
039200110204               //?metto le date in formato ISO
039300101216               wDataISO1 = %date(wDataOggi:*iso);
039400101216               wDataISO2 = %date(wData:*iso);
039500110204               //?i giorni previsti in tabella sono maggiori/uguali
039600110204               //?il cliente lavora esco dal controllo
039700110204               IF  wggSPE >= %diff(wDataISO1 : wDataISO2 : *days);
039800110204                 $Fine = *on;
039900110204                 OMK04lav = 'SI';
040000110204                 clear OMK04ksc;
040100110218                 clear OMK04cmm;
040200110204                 clear OMK04dus;
040300110207                 clear OMK04clv;
040400110204                 leavesr;
040500110204               ENDIF;
040600110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
040700110204               //?Controllo se ci sono spedizioni ancora da fatturare
040800110204               chain ACOksc TITAS02L;
040900110204               //?ci sono spedizioni il cliente lavora
041000110204               //?esco dal controllo
041100110204               IF  %found(TITAS02L);
041200110204                 //?esco dal controllo
041300110204                 $Fine = *on;
041400110204                 OMK04lav = 'SI';
041500110204                 clear OMK04ksc;
041600110218                 clear OMK04cmm;
041700110204                 clear OMK04dus;
041800110207                 clear OMK04clv;
041900110204                 leavesr;
042000110204               ENDIF;
042100110204               //?Se arrivo qua vuol dire che questo cliente non lavora
042200110204               OMK04lav = 'NO';
042300110204               //?mi memorizzo il codice del cliente che ha la data
042400110204               //?ultima spedizione più alta
042500110204               IF  wData >= OMK04dus;
042600110204                 OMK04ksc = ACOksc;
042700110218                 OMK04cmm = CLPage;
042800110204                 OMK04dus = wData;
042900110207                 OMK04clv = CLPclv;
043000110204               ENDIF;
043100101216
043200110204             //?Non c'è data ultima spedizione fattura
043300110204             //?controllo quando è stato aperto il codice
043400101216             WHEN  CLPdus = 0;
043500110204               //?metto data minima ISO
043600110207               wDataISO1 = %date(wDataOggi:*iso);
043700101216               wData = 00010101;
043800101216               wDataISO2 = %date(wData:*iso);
043900101216               $EndCvt = *off;
044000110204               //?cerco il rcd di inserimento nel file delle variazioni
044100110204               //?per recuperare la data di apertura codice
044200101216               setll (ACOkcc:ACOksc) CNACVT1L;
044300101216               DOW  not $EndCvt;
044400101216                 reade (ACOkcc:ACOksc) CNACVT1L;
044500101216                 IF  %eof(CNACVT1L);
044600101216                   $EndCvt = *on;
044700101216                 ENDIF;
044800101216                 IF  not %eof(CNACVT1L) and ACVcta <> 'M';
044900101217                   wDataISO2 = %date(ACVdav:*iso);
045000110204                   wData = ACVdav;
045100101216                   $EndCvt = *on;
045200101216                 ENDIF;
045300101216               ENDDO;
045400110131
045500110131               //?Giorni apertura codice passati dal chiamante
045600110131               IF  IMK04ggape > 0;
045700110131                 wggAPE = IMK04ggape;
045800110131               //?Giorni apertura codice passati = a gg no spedizioni
045900110131               ELSE;
046000110131                 wggAPE = wggSPE;
046100110131               ENDIF;
046200101216
046300110204               //?I giorni apertura codice sono maggiori/uguali
046400110204               //?il cliente lavora esco dal controllo
046500110207               IF  wggAPE >= %diff(wDataISO1 : wDataISO2 : *days);
046600110204                 OMK04lav = 'SI';
046700110204                 clear OMK04ksc;
046800110218                 clear OMK04cmm;
046900110204                 clear OMK04dus;
047000110207                 clear OMK04clv;
047100110204                 $Fine = *on;
047200110204                 leavesr;
047300110204               ENDIF;
047400110426               OMK04gg = %diff(wDataISO1 : wDataISO2 : *days);
047500110204               //?Controllo se ci sono spedizioni ancora da fatturare
047600110204               chain ACOksc TITAS02L;
047700110204               //?ci sono spedizioni il cliente lavora esco dal controllo
047800110204               IF  %found(TITAS02L);
047900110204                 OMK04lav = 'SI';
048000110204                 clear OMK04ksc;
048100110218                 clear OMK04cmm;
048200110204                 clear OMK04dus;
048300110207                 clear OMK04clv;
048400110204                 $Fine = *on;
048500110204                 leavesr;
048600101216               ENDIF;
048700110204               //?Se arrivo qua vuol dire che questo cliente non lavora
048800110204               OMK04lav = 'NO';
048900110204               //?mi memorizzo il codice del cliente che ha la data
049000110204               //?di apertura codice più alta
049100110204               IF  wData >= OMK04dus;
049200110204                 OMK04ksc = ACOksc;
049300110218                 OMK04cmm = CLPage;
049400110204                 OMK04dus = wData;
049500110207                 OMK04clv = CLPclv;
049600110204               ENDIF;
049700101217
049800110204           ENDSL;
049900101216
050000101216         ENDDO;
050100101216
050200101216       ENDSR;
050300091029
050400091029       //--------------------------------------------------------------
050500091029       //?Operazioni finali.
050600091029       //--------------------------------------------------------------
050700101216       BEGSR RoutEnd;
050800091029
050900091029         *inLR = *on;
051000091029         return;
051100091029
051200091029       ENDSR;
051300091029
051400091029      /end-free
