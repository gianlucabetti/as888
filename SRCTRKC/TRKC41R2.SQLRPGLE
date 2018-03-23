000100141230      //--------------------------------------------------------------
000200150123      //?TRKC41R2 - Report Campagne Commerciali - Crea WF a TOTALE
000300141230      //--------------------------------------------------------------
000400141230
000500141230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141230     h dftactgrp(*no) actgrp(*caller)
000700141230
000800141230      //---------------------------------------------------------------
000900141230      //?Dichiarazione file.
001000141230      //---------------------------------------------------------------
001100150107
001200150107      // - File anagrafiche Campagne
001300150107     fTICMP01L  if   e           k disk
001400150123
001500150123      // - File report Campagna Commerciale tot.Azienda
001600150123     fWFCM101L  uf a e           k disk
001700150123
001800150123      // - File report Campagna Commerciale tot.Distretto
001900150123     fWFCM201L  uf a e           k disk
002000150123
002100150123      // - File report Campagna Commerciale tot.Area
002200150123     fWFCM301L  uf a e           k disk
002300150123
002400150123      // - File report Campagna Commerciale tot.Commerciale
002500150123     fWFCM401L  uf a e           k disk
002600141230
002700141230      //---------------------------------------------------------------
002800141230      //?Definizione costanti.
002900141230      //---------------------------------------------------------------
003000141230
003100141230     d FaseObj         c                   const(' 10')
003200141230     d FaseObjProp     c                   const(' 20')
003300141230     d FaseObjFine     c                   const(' 30')
003400141230     d FaseObjTtr      c                   const(' TR')
003500141230     d FaseObjCf       c                   const(' CF')
003600141230     d FaseChiudi      c                   const(' 90')
003700150121     d Blocco8         c                   const(' 8')
003800150108
003900150108     d Elementi_IC     c                   const(10)
004000141230
004100141230      //---------------------------------------------------------------
004200141230      //?Definizione schiere.
004300141230      //---------------------------------------------------------------
004400150109
004500141230      //---------------------------------------------------------------
004600141230      //?Definizione aree dati.
004700141230      //---------------------------------------------------------------
004800141230
004900141230      // - Dati utente
005000141230     d §AzUte        e ds                  extname(AZUTE00F)
005100141230     d                                     dtaara
005200141230     d §DatiUte      e ds                  extname(dDatiUte)
005300141230     d                                     dtaara
005400141230
005500141230      //---------------------------------------------------------------
005600141230      //?Definizione strutture dati.
005700141230      //---------------------------------------------------------------
005800150107
005900150107      // - Status
006000150107     d Psds           sds
006100150107     d   SDSpgm          *proc
006200141230
006300141230      // - Parametri ricevuti
006400141230     d KPJBA         e ds
006500150112     d TRKC41DS      e ds
006600141230
006700141230      // - Reperimento dati utente
006800141230     d TIBS34DS      e ds
006900150107
007000150107      // File di work
007100150107     d WFCMP00F      e ds                  extname(WFCMP00F)
007200141230
007300141230      //---------------------------------------------------------------
007400141230      //?Definizione variabili globali.
007500141230      //---------------------------------------------------------------
007600141230
007700141230      // - Flags booleani
007800141230     d Fine            s               n   inz(*off)
007900141230
008000141230      // - Indici di schiera
008100150112     d xx              s              4s 0 inz
008200141230
008300150108      // - Campi di comodo data
008400150107     d Data_EUR        s               d   datfmt(*eur)
008500150107     d Data_ISO        s               d   datfmt(*iso)
008600141230     d Oggi            s              8s 0 inz
008700150108
008800150108      // - Campi di comodo
008900150123     d claui           s              5s 0 inz
009000150123     d claup           s              5s 0 inz
009100150123     d clauf           s              5s 0 inz
009200150123     d clnavvio        s              5s 0 inz
009300150123     d clavvio         s              5s 0 inz
009400150123     d clavvtr         s              5s 0 inz
009500150123     d clchiuso        s              5s 0 inz
009600150123     d clconfr         s              5s 0 inz
009700150123     d clnocf          s              5s 0 inz
009800150123     d clperso         s              5s 0 inz
009900150123     d ftaui           s              9s 0 inz
010000150123     d ftaup           s              9s 0 inz
010100150123     d ftauf           s              9s 0 inz
010200150123     d ftnavvio        s              9s 0 inz
010300150123     d ftavvio         s              9s 0 inz
010400150123     d ftavvtr         s              9s 0 inz
010500150123     d ftchiuso        s              9s 0 inz
010600150123     d ftconfr         s              9s 0 inz
010700150123     d ftnocf          s              9s 0 inz
010800150123     d ftperso         s              9s 0 inz
010900150123     d valaui          s             10s 0 inz
011000150123     d valaup          s             10s 0 inz
011100150123     d valauf          s             10s 0 inz
011200150123     d valnavvio       s             10s 0 inz
011300150123     d valavvio        s             10s 0 inz
011400150123     d valavvtr        s             10s 0 inz
011500150123     d valchiuso       s             10s 0 inz
011600150123     d valconfr        s             10s 0 inz
011700150123     d valnocf         s             10s 0 inz
011800150123     d valperso        s             10s 0 inz
011900150123     d vaoavvio        s             10s 0 inz
012000150123     d vaoavvtr        s             10s 0 inz
012100150123     d vaochiuso       s             10s 0 inz
012200150123     d vaoconfr        s             10s 0 inz
012300150408     d vcfconfr        s             10s 0 inz
012400150109
012500150318     d wVALaui         s              9s 0 inz
012600150318     d wVALaup         s              9s 0 inz
012700150318     d wVALauf         s              9s 0 inz
012800150318     d wVALautr        s              9s 0 inz
012900150318     d wVALaucf        s              9s 0 inz
013000150123
013100150123     d savclvord       s                   like(WCMPclvord) inz
013200150123     d savclv          s                   like(WCMPclv) inz
013300150123     d savdis          s                   like(WCMPdis) inz
013400150123     d savcar          s                   like(WCMPcar) inz
013500150123     d savcmm          s                   like(WCMPcmm) inz
013600150109
013700141230      //---------------------------------------------------------------
013800141230      //?Definizione procedure esterne.
013900141230      //---------------------------------------------------------------
014000141230
014100141230      //---------------------------------------------------------------
014200141230      //?Definizione prototipi.
014300141230      //---------------------------------------------------------------
014400141230
014500141230      /copy gaitrasrc/srcprotopr,tibs34r
014600141230
014700141230      //---------------------------------------------------------------
014800141230      //?Definizione key-list.
014900141230      //---------------------------------------------------------------
015000150107
015100150107       // - File TABEL00F
015200150107     d k03tabel      e ds                  extname(TABEL00F:*key)
015300150107     d                                     prefix(k_)
015400150107     d                                     inz
015500141230
015600141230      //---------------------------------------------------------------
015700141230
015800141230      //---------------------------------------------------------------
015900141230      //?M A I N - L I N E
016000141230      //---------------------------------------------------------------
016100141230
016200141230     c     *Entry        plist
016300141230     c                   parm                    kpjba
016400141230
016500141230      /free
016600141230
016700141230       //?Operazioni iniziali
016800141230       exsr RoutInz;
016900141230
017000141230       //?Elaboro
017100141230       exsr Elabora;
017200141230
017300141230       //?Operazioni finali
017400141230       exsr RoutEnd;
017500141230
017600141230       //--------------------------------------------------------------
017700141230       //?Operazioni iniziali.
017800141230       //--------------------------------------------------------------
017900141230       BEGSR RoutInz;
018000141230
018100141230       //?Imposto la ds con i dati della KPJBU
018200150112         TRKC41ds = kpjbu;
018300141230
018400141230       //?Reperimento dati job
018500141230         exsr DatiJob;
018600141230
018700141230         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018800150429
018900150429       //?Aggancio l'anagrafica Campagna
019000150429         chain IKC41ncm TICMP01L;
019100150429         IF  not %found(TICMP01L);
019200150429           clear CMPtpc;
019300150429         ENDIF;
019400141230
019500141230       ENDSR;
019600141230
019700141230       //--------------------------------------------------------------
019800141230       //?Reperimento Dati del job (Utente/Operativi).
019900141230       //--------------------------------------------------------------
020000141230       BEGSR DatiJob;
020100141230
020200141230         in(E) §AzUte;
020300141230         if NOT %error;
020400141230           in(E) §DatiUte;
020500141230         endif;
020600141230         if %error or RSut = *blanks;
020700141230           clear TIBS34ds;
020800141230           tibs34r(tibs34ds);
020900141230           in §AzUte;
021000141230           in §DatiUte;
021100141230         endif;
021200141230
021300141230       ENDSR;
021400141230
021500141230       //--------------------------------------------------------------
021600141230       //?Elaboro.
021700141230       //--------------------------------------------------------------
021800141230       BEGSR Elabora;
021900150107
022000150107       //?Leggo il file di work
022100150107         exec sql
022200150107         declare WRK cursor for
022300150107         select * from WFCMP00F
022400150123         order by WCMPdis, WCMPcar, WCMPcmm, WCMPclvord desc;
022500150107
022600150107         exec sql
022700150107         open WRK;
022800150107         IF  sqlcode < 0;
022900150107           Fine = *on;
023000150107         ENDIF;
023100150107
023200150107         DOW not Fine;
023300150107           exec sql
023400150107           fetch next from WRK into :WFCMP00F;
023500150107           IF  sqlcod = 100 or sqlcod < 0;
023600150107             Fine = *on;
023700150107             leave;
023800150107           ENDIF;
023900150204
024000150204         //?A rottura di distretto scrivo/aggiorno i file di work
024100150204           IF  WCMPdis <> savdis;
024200150204             IF  savdis > *blanks;
024300150204               exsr scrivi;
024400150204             ENDIF;
024500150204             savclvord = WCMPclvord;
024600150204             savclv = WCMPclv;
024700150204             savcmm = WCMPcmm;
024800150204             savcar = WCMPcar;
024900150204             savdis = WCMPdis;
025000150204           ENDIF;
025100150204
025200150204         //?A rottura di area scrivo/aggiorno i file di work
025300150204           IF  WCMPcar <> savcar;
025400150204             IF  savcar > *zeros;
025500150204               exsr scrivi;
025600150204             ENDIF;
025700150204             savclvord = WCMPclvord;
025800150204             savclv = WCMPclv;
025900150204             savcmm = WCMPcmm;
026000150204             savcar = WCMPcar;
026100150204             savdis = WCMPdis;
026200150204           ENDIF;
026300150204
026400150204         //?A rottura di commerciale scrivo/aggiorno i file di work
026500150204           IF  WCMPcmm <> savcmm;
026600150204             IF  savcmm > *zeros;
026700150204               exsr scrivi;
026800150204             ENDIF;
026900150204             savclvord = WCMPclvord;
027000150204             savclv = WCMPclv;
027100150204             savcmm = WCMPcmm;
027200150204             savcar = WCMPcar;
027300150204             savdis = WCMPdis;
027400150204           ENDIF;
027500150123
027600150123         //?A rottura di categoria cliente scrivo/aggiorno i file di work
027700150123           IF  WCMPclvord <> savclvord;
027800150123             IF  savclvord > *zeros;
027900150123               exsr scrivi;
028000150123             ENDIF;
028100150123             savclvord = WCMPclvord;
028200150126             savclv = WCMPclv;
028300150123             savcmm = WCMPcmm;
028400150123             savcar = WCMPcar;
028500150123             savdis = WCMPdis;
028600150123           ENDIF;
028700150109
028800150109           wVALaui = WCMPfat * WCMPaui;
028900150109           wVALaup = WCMPfat * WCMPaup;
029000150109           wVALauf = WCMPfat * WCMPauf;
029100150109           wVALautr = WCMPfat * WCMPautr;
029200150109           wVALaucf = WCMPfat * WCMPaucf;
029300150107
029400150121         //?INIZIALE  tutti i rcd del file di work
029500150123           claui += 1;
029600150123           ftaui += WCMPfat;
029700150123           valaui += wVALaui;
029800150121
029900150121         //?Se NON è un PERSO
030000150121           IF  WCMPperso = *blanks;
030100150107
030200150120           //?PROPOSTO
030300150120             IF  WCMPufe = FaseObjProp;
030400150123               claup += 1;
030500150123               ftaup += WCMPfat;
030600150123               valaup += wVALaup;
030700150114             ENDIF;
030800150107
030900150120           //?FINALE
031000150120             IF  WCMPufe = FaseObjFine;
031100150123               clauf += 1;
031200150123               ftauf += WCMPfat;
031300150123               valauf += wVALauf;
031400150114             ENDIF;
031500150107
031600150120           //?NON AVVIATI
031700150123             IF  WCMPnavvio = 'S';
031800150123               clnavvio += 1;
031900150123               ftnavvio += WCMPfat;
032000150123               valnavvio += wVALauf;
032100150114             ENDIF;
032200150120
032300150120           //?AVVIATI
032400150120             IF  WCMPavvio = 'S';
032500150123               clavvio += 1;
032600150123               ftavvio += WCMPfat;
032700150123               valavvio += wVALauf;
032800150123               vaoavvio += wVALautr;
032900150120             ENDIF;
033000150107
033100150120           //?AVVIATI CON OFFERTA
033200150120             IF  WCMPavvtr = 'S';
033300150123               clavvtr += 1;
033400150123               ftavvtr += WCMPfat;
033500150123               valavvtr += wVALauf;
033600150123               vaoavvtr += wVALautr;
033700150114             ENDIF;
033800150107
033900150120           //?CHIUSI
034000150114             IF  WCMPchiuso = 'S';
034100150123               clchiuso += 1;
034200150123               ftchiuso += WCMPfat;
034300150123               valchiuso += wVALauf;
034400150123               vaochiuso += wVALautr;
034500150114             ENDIF;
034600150107
034700150120         //?CONFRONTO
034800150120             IF  WCMPconfr = 'S';
034900150123               clconfr += 1;
035000150123               ftconfr += WCMPfat;
035100150123               valconfr += wVALauf;
035200150429           //?se campagna di tipo 'A' imposto il valore ottenuto dalle trattative
035300150429               IF  CMPtpc = 'A';
035400150429                 vaoconfr += wVALautr;
035500150429               ENDIF;
035600150429           //?se campagna di tipo 'L' imposto il valore ottenuto dalla fase finale
035700150429               IF  CMPtpc = 'L';
035800150429                 vaoconfr += wVALauf;
035900150429               ENDIF;
036000150408               vcfconfr += wVALaucf;
036100150114             ENDIF;
036200150120
036300150120         //?NON CONFRONTATI
036400150120             IF  WCMPnocf = 'S';
036500150123               clnocf += 1;
036600150123               ftnocf += WCMPfat;
036700150123               valnocf += wVALauf;
036800150120             ENDIF;
036900150114           ENDIF;
037000150114
037100150114         //?Se è un PERSO
037200150114           IF  WCMPperso = 'S';
037300150123             clperso += 1;
037400150123             ftperso += WCMPfat;
037500150123             valperso += wVALaui;
037600150107           ENDIF;
037700150107
037800150107         ENDDO;
037900150107
038000150107         exec sql close WRK;
038100150108
038200150123         //?Scrivo ultima categoria cliente
038300150123         IF  savclvord > *zeros;
038400150123            exsr Scrivi;
038500150108          ENDIF;
038600141230
038700141230       ENDSR;
038800150107
038900150107       //--------------------------------------------------------------
039000150123       //?Scrivo/Aggiorno file di work.
039100150107       //--------------------------------------------------------------
039200150123       BEGSR Scrivi;
039300150123
039400150123       //?Totale per Azienda
039500150126         chain (WCMPncm:savclvord) WFCM101L;
039600150123         IF  %found(WFCM101L);
039700150123           exsr Aggiorna1;
039800150123         ELSE;
039900150123           exsr Scrivi1;
040000150123         ENDIF;
040100150123
040200150123       //?Totale per Distretto
040300150126         chain (WCMPncm:savdis:savclvord) WFCM201L;
040400150123         IF  %found(WFCM201L);
040500150123           exsr Aggiorna2;
040600150123         ELSE;
040700150123           exsr Scrivi2;
040800150123         ENDIF;
040900150123
041000150123       //?Totale per Area
041100150126         chain (WCMPncm:savdis:savcar:savclvord) WFCM301L;
041200150123         IF  %found(WFCM301L);
041300150123           exsr Aggiorna3;
041400150123         ELSE;
041500150123           exsr Scrivi3;
041600150123         ENDIF;
041700150123
041800150123       //?Totale per Commerciale
041900150126         chain (WCMPncm:savdis:savcar:savcmm:savclvord) WFCM401L;
042000150123         IF  %found(WFCM401L);
042100150123           exsr Aggiorna4;
042200150123         ELSE;
042300150123           exsr Scrivi4;
042400150123         ENDIF;
042500150123
042600150123         exsr Pulisci;
042700150107
042800150107       ENDSR;
042900150123
043000150123       //--------------------------------------------------------------
043100150123       //?Aggiorno file 1.
043200150123       //--------------------------------------------------------------
043300150123       BEGSR Aggiorna1;
043400150123
043500150123       //?riga 1 --> Clienti
043600150123         W1claui += claui;
043700150123         W1claup += claup;
043800150123         W1clauf += clauf;
043900150123         W1clnavvi += clnavvio;
044000150123         W1clavvio += clavvio;
044100150123         W1clavvtr += clavvtr;
044200150123         W1clchius += clchiuso;
044300150123         W1clconfr += clconfr;
044400150123         W1clnocf  += clnocf;
044500150123         W1clperso += clperso;
044600150123       //?riga 2 --> Fatturato
044700150123         W1ftaui += ftaui;
044800150123         W1ftaup += ftaup;
044900150123         W1ftauf += ftauf;
045000150123         W1ftnavvi += ftnavvio;
045100150123         W1ftavvio += ftavvio;
045200150123         W1ftavvtr += ftavvtr;
045300150123         W1ftchius += ftchiuso;
045400150123         W1ftconfr += ftconfr;
045500150123         W1ftnocf  += ftnocf;
045600150123         W1ftperso += ftperso;
045700150123       //?Valori per calcolo riga 3
045800150123         W1valaui += valaui;
045900150123         W1valaup += valaup;
046000150123         W1valauf += valauf;
046100150123         W1valnavvi += valnavvio;
046200150123         W1valavvio += valavvio;
046300150123         W1valavvtr += valavvtr;
046400150123         W1valchius += valchiuso;
046500150123         W1valconfr += valconfr;
046600150123         W1valnocf  += valnocf;
046700150123         W1valperso += valperso;
046800150123       //?Valori per calcolo riga 5
046900150123         W1vaoavvio += vaoavvio;
047000150123         W1vaoavvtr += vaoavvtr;
047100150123         W1vaochius += vaochiuso;
047200150123         W1vaoconfr += vaoconfr;
047300150408         W1vcfconfr += vcfconfr;
047400150123
047500150123         UPDATE WFCM1000;
047600150123
047700150123       ENDSR;
047800150123
047900150123       //--------------------------------------------------------------
048000150123       //?Scrivo file 1.
048100150123       //--------------------------------------------------------------
048200150123       BEGSR Scrivi1;
048300150123
048400150123        clear WFCM1000;
048500150123
048600150123       //?Dati comuni
048700150123         WCM1ncm = WCMPncm;
048800150123         WCM1clv = savclv;
048900150123         WCM1clvord = savclvord;
049000150123
049100150123       //?riga 1 --> Clienti
049200150123         W1claui = claui;
049300150123         W1claup = claup;
049400150123         W1clauf = clauf;
049500150123         W1clnavvi = clnavvio;
049600150123         W1clavvio = clavvio;
049700150123         W1clavvtr = clavvtr;
049800150123         W1clchius = clchiuso;
049900150123         W1clconfr = clconfr;
050000150123         W1clnocf  = clnocf;
050100150123         W1clperso = clperso;
050200150123       //?riga 2 --> Fatturato
050300150123         W1ftaui = ftaui;
050400150123         W1ftaup = ftaup;
050500150123         W1ftauf = ftauf;
050600150123         W1ftnavvi = ftnavvio;
050700150123         W1ftavvio = ftavvio;
050800150123         W1ftavvtr = ftavvtr;
050900150123         W1ftchius = ftchiuso;
051000150123         W1ftconfr = ftconfr;
051100150123         W1ftnocf  = ftnocf;
051200150123         W1ftperso = ftperso;
051300150123       //?Valori per calcolo riga 3
051400150123         W1valaui = valaui;
051500150123         W1valaup = valaup;
051600150123         W1valauf = valauf;
051700150123         W1valnavvi = valnavvio;
051800150123         W1valavvio = valavvio;
051900150123         W1valavvtr = valavvtr;
052000150123         W1valchius = valchiuso;
052100150123         W1valconfr = valconfr;
052200150123         W1valnocf  = valnocf;
052300150123         W1valperso = valperso;
052400150123       //?Valori per calcolo riga 5
052500150123         W1vaoavvio = vaoavvio;
052600150123         W1vaoavvtr = vaoavvtr;
052700150123         W1vaochius = vaochiuso;
052800150123         W1vaoconfr = vaoconfr;
052900150408         W1vcfconfr = vcfconfr;
053000150123
053100150123         WRITE WFCM1000;
053200150123
053300150123       ENDSR;
053400150123
053500150123       //--------------------------------------------------------------
053600150123       //?Aggiorno file 2.
053700150123       //--------------------------------------------------------------
053800150123       BEGSR Aggiorna2;
053900150123
054000150123       //?riga 1 --> Clienti
054100150123         W2claui += claui;
054200150123         W2claup += claup;
054300150123         W2clauf += clauf;
054400150123         W2clnavvi += clnavvio;
054500150123         W2clavvio += clavvio;
054600150123         W2clavvtr += clavvtr;
054700150123         W2clchius += clchiuso;
054800150123         W2clconfr += clconfr;
054900150123         W2clnocf  += clnocf;
055000150123         W2clperso += clperso;
055100150123       //?riga 2 --> Fatturato
055200150123         W2ftaui += ftaui;
055300150123         W2ftaup += ftaup;
055400150123         W2ftauf += ftauf;
055500150123         W2ftnavvi += ftnavvio;
055600150123         W2ftavvio += ftavvio;
055700150123         W2ftavvtr += ftavvtr;
055800150123         W2ftchius += ftchiuso;
055900150123         W2ftconfr += ftconfr;
056000150123         W2ftnocf  += ftnocf;
056100150123         W2ftperso += ftperso;
056200150123       //?Valori per calcolo riga 3
056300150123         W2valaui += valaui;
056400150123         W2valaup += valaup;
056500150123         W2valauf += valauf;
056600150123         W2valnavvi += valnavvio;
056700150123         W2valavvio += valavvio;
056800150123         W2valavvtr += valavvtr;
056900150123         W2valchius += valchiuso;
057000150123         W2valconfr += valconfr;
057100150123         W2valnocf  += valnocf;
057200150123         W2valperso += valperso;
057300150123       //?Valori per calcolo riga 5
057400150123         W2vaoavvio += vaoavvio;
057500150123         W2vaoavvtr += vaoavvtr;
057600150123         W2vaochius += vaochiuso;
057700150123         W2vaoconfr += vaoconfr;
057800150408         W2vcfconfr += vcfconfr;
057900150123
058000150123         UPDATE WFCM2000;
058100150123
058200150123       ENDSR;
058300150123
058400150123       //--------------------------------------------------------------
058500150123       //?Scrivo file 2.
058600150123       //--------------------------------------------------------------
058700150123       BEGSR Scrivi2;
058800150123
058900150123        clear WFCM2000;
059000150123
059100150123       //?Dati comuni
059200150123         WCM2ncm = WCMPncm;
059300150123         WCM2dis = savdis;
059400150123         WCM2clv = savclv;
059500150123         WCM2clvord = savclvord;
059600150123
059700150123       //?riga 1 --> Clienti
059800150123         W2claui = claui;
059900150123         W2claup = claup;
060000150123         W2clauf = clauf;
060100150123         W2clnavvi = clnavvio;
060200150123         W2clavvio = clavvio;
060300150123         W2clavvtr = clavvtr;
060400150123         W2clchius = clchiuso;
060500150123         W2clconfr = clconfr;
060600150123         W2clnocf  = clnocf;
060700150123         W2clperso = clperso;
060800150123       //?riga 2 --> Fatturato
060900150123         W2ftaui = ftaui;
061000150123         W2ftaup = ftaup;
061100150123         W2ftauf = ftauf;
061200150123         W2ftnavvi = ftnavvio;
061300150123         W2ftavvio = ftavvio;
061400150123         W2ftavvtr = ftavvtr;
061500150123         W2ftchius = ftchiuso;
061600150123         W2ftconfr = ftconfr;
061700150123         W2ftnocf  = ftnocf;
061800150123         W2ftperso = ftperso;
061900150123       //?Valori per calcolo riga 3
062000150123         W2valaui = valaui;
062100150123         W2valaup = valaup;
062200150123         W2valauf = valauf;
062300150123         W2valnavvi = valnavvio;
062400150123         W2valavvio = valavvio;
062500150123         W2valavvtr = valavvtr;
062600150123         W2valchius = valchiuso;
062700150123         W2valconfr = valconfr;
062800150123         W2valnocf  = valnocf;
062900150123         W2valperso = valperso;
063000150123       //?Valori per calcolo riga 5
063100150123         W2vaoavvio = vaoavvio;
063200150123         W2vaoavvtr = vaoavvtr;
063300150123         W2vaochius = vaochiuso;
063400150123         W2vaoconfr = vaoconfr;
063500150408         W2vcfconfr = vcfconfr;
063600150123
063700150123         WRITE WFCM2000;
063800150123
063900150123       ENDSR;
064000150123
064100150123       //--------------------------------------------------------------
064200150123       //?Aggiorno file 3.
064300150123       //--------------------------------------------------------------
064400150123       BEGSR Aggiorna3;
064500150123
064600150123       //?riga 1 --> Clienti
064700150123         W3claui += claui;
064800150123         W3claup += claup;
064900150123         W3clauf += clauf;
065000150123         W3clnavvi += clnavvio;
065100150123         W3clavvio += clavvio;
065200150123         W3clavvtr += clavvtr;
065300150123         W3clchius += clchiuso;
065400150123         W3clconfr += clconfr;
065500150123         W3clnocf  += clnocf;
065600150123         W3clperso += clperso;
065700150123       //?riga 2 --> Fatturato
065800150123         W3ftaui += ftaui;
065900150123         W3ftaup += ftaup;
066000150123         W3ftauf += ftauf;
066100150123         W3ftnavvi += ftnavvio;
066200150123         W3ftavvio += ftavvio;
066300150123         W3ftavvtr += ftavvtr;
066400150123         W3ftchius += ftchiuso;
066500150123         W3ftconfr += ftconfr;
066600150123         W3ftnocf  += ftnocf;
066700150123         W3ftperso += ftperso;
066800150123       //?Valori per calcolo riga 3
066900150123         W3valaui += valaui;
067000150123         W3valaup += valaup;
067100150123         W3valauf += valauf;
067200150123         W3valnavvi += valnavvio;
067300150123         W3valavvio += valavvio;
067400150123         W3valavvtr += valavvtr;
067500150123         W3valchius += valchiuso;
067600150123         W3valconfr += valconfr;
067700150123         W3valnocf  += valnocf;
067800150123         W3valperso += valperso;
067900150123       //?Valori per calcolo riga 5
068000150123         W3vaoavvio += vaoavvio;
068100150123         W3vaoavvtr += vaoavvtr;
068200150123         W3vaochius += vaochiuso;
068300150123         W3vaoconfr += vaoconfr;
068400150408         W3vcfconfr += vcfconfr;
068500150123
068600150123         UPDATE WFCM3000;
068700150123
068800150123       ENDSR;
068900150123
069000150123       //--------------------------------------------------------------
069100150123       //?Scrivo file 3.
069200150123       //--------------------------------------------------------------
069300150123       BEGSR Scrivi3;
069400150123
069500150123        clear WFCM3000;
069600150123
069700150123       //?Dati comuni
069800150123         WCM3ncm = WCMPncm;
069900150123         WCM3dis = savdis;
070000150123         WCM3car = savcar;
070100150123         WCM3clv = savclv;
070200150123         WCM3clvord = savclvord;
070300150123
070400150123       //?riga 1 --> Clienti
070500150123         W3claui = claui;
070600150123         W3claup = claup;
070700150123         W3clauf = clauf;
070800150123         W3clnavvi = clnavvio;
070900150123         W3clavvio = clavvio;
071000150123         W3clavvtr = clavvtr;
071100150123         W3clchius = clchiuso;
071200150123         W3clconfr = clconfr;
071300150123         W3clnocf  = clnocf;
071400150123         W3clperso = clperso;
071500150123       //?riga 2 --> Fatturato
071600150123         W3ftaui = ftaui;
071700150123         W3ftaup = ftaup;
071800150123         W3ftauf = ftauf;
071900150123         W3ftnavvi = ftnavvio;
072000150123         W3ftavvio = ftavvio;
072100150123         W3ftavvtr = ftavvtr;
072200150123         W3ftchius = ftchiuso;
072300150123         W3ftconfr = ftconfr;
072400150123         W3ftnocf  = ftnocf;
072500150123         W3ftperso = ftperso;
072600150123       //?Valori per calcolo riga 3
072700150123         W3valaui = valaui;
072800150123         W3valaup = valaup;
072900150123         W3valauf = valauf;
073000150123         W3valnavvi = valnavvio;
073100150123         W3valavvio = valavvio;
073200150123         W3valavvtr = valavvtr;
073300150123         W3valchius = valchiuso;
073400150123         W3valconfr = valconfr;
073500150123         W3valnocf  = valnocf;
073600150123         W3valperso = valperso;
073700150123       //?Valori per calcolo riga 5
073800150123         W3vaoavvio = vaoavvio;
073900150123         W3vaoavvtr = vaoavvtr;
074000150123         W3vaochius = vaochiuso;
074100150123         W3vaoconfr = vaoconfr;
074200150408         W3vcfconfr = vcfconfr;
074300150123
074400150123         WRITE WFCM3000;
074500150123
074600150123       ENDSR;
074700150123
074800150123       //--------------------------------------------------------------
074900150123       //?Aggiorno file 4.
075000150123       //--------------------------------------------------------------
075100150123       BEGSR Aggiorna4;
075200150123
075300150123       //?riga 1 --> Clienti
075400150123         W4claui += claui;
075500150123         W4claup += claup;
075600150123         W4clauf += clauf;
075700150123         W4clnavvi += clnavvio;
075800150123         W4clavvio += clavvio;
075900150123         W4clavvtr += clavvtr;
076000150123         W4clchius += clchiuso;
076100150123         W4clconfr += clconfr;
076200150123         W4clnocf  += clnocf;
076300150123         W4clperso += clperso;
076400150123       //?riga 2 --> Fatturato
076500150123         W4ftaui += ftaui;
076600150123         W4ftaup += ftaup;
076700150123         W4ftauf += ftauf;
076800150123         W4ftnavvi += ftnavvio;
076900150123         W4ftavvio += ftavvio;
077000150123         W4ftavvtr += ftavvtr;
077100150123         W4ftchius += ftchiuso;
077200150123         W4ftconfr += ftconfr;
077300150123         W4ftnocf  += ftnocf;
077400150123         W4ftperso += ftperso;
077500150123       //?Valori per calcolo riga 3
077600150123         W4valaui += valaui;
077700150123         W4valaup += valaup;
077800150123         W4valauf += valauf;
077900150123         W4valnavvi += valnavvio;
078000150123         W4valavvio += valavvio;
078100150123         W4valavvtr += valavvtr;
078200150123         W4valchius += valchiuso;
078300150123         W4valconfr += valconfr;
078400150123         W4valnocf  += valnocf;
078500150123         W4valperso += valperso;
078600150123       //?Valori per calcolo riga 5
078700150123         W4vaoavvio += vaoavvio;
078800150123         W4vaoavvtr += vaoavvtr;
078900150123         W4vaochius += vaochiuso;
079000150123         W4vaoconfr += vaoconfr;
079100150408         W4vcfconfr += vcfconfr;
079200150123
079300150123         UPDATE WFCM4000;
079400150123
079500150123       ENDSR;
079600150123
079700150123       //--------------------------------------------------------------
079800150123       //?Scrivo file 4.
079900150123       //--------------------------------------------------------------
080000150123       BEGSR Scrivi4;
080100150123
080200150123        clear WFCM4000;
080300150123
080400150123       //?Dati comuni
080500150123         WCM4ncm = WCMPncm;
080600150123         WCM4dis = savdis;
080700150123         WCM4car = savcar;
080800150123         WCM4cmm = savcmm;
080900150123         WCM4clv = savclv;
081000150123         WCM4clvord = savclvord;
081100150123
081200150123       //?riga 1 --> Clienti
081300150123         W4claui = claui;
081400150123         W4claup = claup;
081500150123         W4clauf = clauf;
081600150123         W4clnavvi = clnavvio;
081700150123         W4clavvio = clavvio;
081800150123         W4clavvtr = clavvtr;
081900150123         W4clchius = clchiuso;
082000150123         W4clconfr = clconfr;
082100150123         W4clnocf  = clnocf;
082200150123         W4clperso = clperso;
082300150123       //?riga 2 --> Fatturato
082400150123         W4ftaui = ftaui;
082500150123         W4ftaup = ftaup;
082600150123         W4ftauf = ftauf;
082700150123         W4ftnavvi = ftnavvio;
082800150123         W4ftavvio = ftavvio;
082900150123         W4ftavvtr = ftavvtr;
083000150123         W4ftchius = ftchiuso;
083100150123         W4ftconfr = ftconfr;
083200150123         W4ftnocf  = ftnocf;
083300150123         W4ftperso = ftperso;
083400150123       //?Valori per calcolo riga 3
083500150123         W4valaui = valaui;
083600150123         W4valaup = valaup;
083700150123         W4valauf = valauf;
083800150123         W4valnavvi = valnavvio;
083900150123         W4valavvio = valavvio;
084000150123         W4valavvtr = valavvtr;
084100150123         W4valchius = valchiuso;
084200150123         W4valconfr = valconfr;
084300150123         W4valnocf  = valnocf;
084400150123         W4valperso = valperso;
084500150123       //?Valori per calcolo riga 5
084600150123         W4vaoavvio = vaoavvio;
084700150123         W4vaoavvtr = vaoavvtr;
084800150123         W4vaochius = vaochiuso;
084900150123         W4vaoconfr = vaoconfr;
085000150408         W4vcfconfr = vcfconfr;
085100150123
085200150123         WRITE WFCM4000;
085300150123
085400150123       ENDSR;
085500150107
085600150107       //--------------------------------------------------------------
085700150123       //?Pulisco i campi di lavoro.
085800150107       //--------------------------------------------------------------
085900150107       BEGSR Pulisci;
086000150107
086100150123       //?riga 1 --> Clienti
086200150107         clear claui;
086300150107         clear claup;
086400150107         clear clauf;
086500150123         clear clnavvio;
086600150123         clear clavvio;
086700150123         clear clavvtr;
086800150123         clear clchiuso;
086900150123         clear clconfr;
087000150123         clear clnocf;
087100150123         clear clperso;
087200150107       //?riga 2 --> Fatturato
087300150108         clear ftaui;
087400150108         clear ftaup;
087500150108         clear ftauf;
087600150123         clear ftnavvio;
087700150123         clear ftavvio;
087800150123         clear ftavvtr;
087900150123         clear ftchiuso;
088000150123         clear ftconfr;
088100150123         clear ftnocf;
088200150123         clear ftperso;
088300150123       //?Valore per calcolo riga 3
088400150123         clear valaui;
088500150123         clear valaup;
088600150123         clear valauf;
088700150123         clear valnavvio;
088800150123         clear valavvio;
088900150123         clear valavvtr;
089000150123         clear valchiuso;
089100150123         clear valconfr;
089200150123         clear valnocf;
089300150123         clear valperso;
089400150123       //?Valore per calcolo riga 4
089500150123         clear vaoavvio;
089600150123         clear vaoavvtr;
089700150123         clear vaochiuso;
089800150123         clear vaoconfr;
089900150408         clear vcfconfr;
090000150108
090100150107       ENDSR;
090200150108
090300141230       //--------------------------------------------------------------
090400141230       //?Operazioni finali.
090500141230       //--------------------------------------------------------------
090600141230       BEGSR RoutEnd;
090700141230
090800141230         *inLR = *on;
090900141230         return;
091000141230
091100141230       ENDSR;
091200141230
091300141230      /end-free
