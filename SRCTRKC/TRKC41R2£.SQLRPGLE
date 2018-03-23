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
012300150109
012400150318     d wVALaui         s              9s 0 inz
012500150318     d wVALaup         s              9s 0 inz
012600150318     d wVALauf         s              9s 0 inz
012700150318     d wVALautr        s              9s 0 inz
012800150318     d wVALaucf        s              9s 0 inz
012900150123
013000150123     d savclvord       s                   like(WCMPclvord) inz
013100150123     d savclv          s                   like(WCMPclv) inz
013200150123     d savdis          s                   like(WCMPdis) inz
013300150123     d savcar          s                   like(WCMPcar) inz
013400150123     d savcmm          s                   like(WCMPcmm) inz
013500150109
013600141230      //---------------------------------------------------------------
013700141230      //?Definizione procedure esterne.
013800141230      //---------------------------------------------------------------
013900141230
014000141230      //---------------------------------------------------------------
014100141230      //?Definizione prototipi.
014200141230      //---------------------------------------------------------------
014300141230
014400141230      /copy gaitrasrc/srcprotopr,tibs34r
014500141230
014600141230      //---------------------------------------------------------------
014700141230      //?Definizione key-list.
014800141230      //---------------------------------------------------------------
014900150107
015000150107       // - File TABEL00F
015100150107     d k03tabel      e ds                  extname(TABEL00F:*key)
015200150107     d                                     prefix(k_)
015300150107     d                                     inz
015400141230
015500141230      //---------------------------------------------------------------
015600141230
015700141230      //---------------------------------------------------------------
015800141230      //?M A I N - L I N E
015900141230      //---------------------------------------------------------------
016000141230
016100141230     c     *Entry        plist
016200141230     c                   parm                    kpjba
016300141230
016400141230      /free
016500141230
016600141230       //?Operazioni iniziali
016700141230       exsr RoutInz;
016800141230
016900141230       //?Elaboro
017000141230       exsr Elabora;
017100141230
017200141230       //?Operazioni finali
017300141230       exsr RoutEnd;
017400141230
017500141230       //--------------------------------------------------------------
017600141230       //?Operazioni iniziali.
017700141230       //--------------------------------------------------------------
017800141230       BEGSR RoutInz;
017900141230
018000141230       //?Imposto la ds con i dati della KPJBU
018100150112         TRKC41ds = kpjbu;
018200141230
018300141230       //?Reperimento dati job
018400141230         exsr DatiJob;
018500141230
018600141230         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018700141230
018800141230       ENDSR;
018900141230
019000141230       //--------------------------------------------------------------
019100141230       //?Reperimento Dati del job (Utente/Operativi).
019200141230       //--------------------------------------------------------------
019300141230       BEGSR DatiJob;
019400141230
019500141230         in(E) §AzUte;
019600141230         if NOT %error;
019700141230           in(E) §DatiUte;
019800141230         endif;
019900141230         if %error or RSut = *blanks;
020000141230           clear TIBS34ds;
020100141230           tibs34r(tibs34ds);
020200141230           in §AzUte;
020300141230           in §DatiUte;
020400141230         endif;
020500141230
020600141230       ENDSR;
020700141230
020800141230       //--------------------------------------------------------------
020900141230       //?Elaboro.
021000141230       //--------------------------------------------------------------
021100141230       BEGSR Elabora;
021200150107
021300150107       //?Leggo il file di work
021400150107         exec sql
021500150107         declare WRK cursor for
021600150107         select * from WFCMP00F
021700150123         order by WCMPdis, WCMPcar, WCMPcmm, WCMPclvord desc;
021800150107
021900150107         exec sql
022000150107         open WRK;
022100150107         IF  sqlcode < 0;
022200150107           Fine = *on;
022300150107         ENDIF;
022400150107
022500150107         DOW not Fine;
022600150107           exec sql
022700150107           fetch next from WRK into :WFCMP00F;
022800150107           IF  sqlcod = 100 or sqlcod < 0;
022900150107             Fine = *on;
023000150107             leave;
023100150107           ENDIF;
023200150204
023300150204         //?A rottura di distretto scrivo/aggiorno i file di work
023400150204           IF  WCMPdis <> savdis;
023500150204             IF  savdis > *blanks;
023600150204               exsr scrivi;
023700150204             ENDIF;
023800150204             savclvord = WCMPclvord;
023900150204             savclv = WCMPclv;
024000150204             savcmm = WCMPcmm;
024100150204             savcar = WCMPcar;
024200150204             savdis = WCMPdis;
024300150204           ENDIF;
024400150204
024500150204         //?A rottura di area scrivo/aggiorno i file di work
024600150204           IF  WCMPcar <> savcar;
024700150204             IF  savcar > *zeros;
024800150204               exsr scrivi;
024900150204             ENDIF;
025000150204             savclvord = WCMPclvord;
025100150204             savclv = WCMPclv;
025200150204             savcmm = WCMPcmm;
025300150204             savcar = WCMPcar;
025400150204             savdis = WCMPdis;
025500150204           ENDIF;
025600150204
025700150204         //?A rottura di commerciale scrivo/aggiorno i file di work
025800150204           IF  WCMPcmm <> savcmm;
025900150204             IF  savcmm > *zeros;
026000150204               exsr scrivi;
026100150204             ENDIF;
026200150204             savclvord = WCMPclvord;
026300150204             savclv = WCMPclv;
026400150204             savcmm = WCMPcmm;
026500150204             savcar = WCMPcar;
026600150204             savdis = WCMPdis;
026700150204           ENDIF;
026800150123
026900150123         //?A rottura di categoria cliente scrivo/aggiorno i file di work
027000150123           IF  WCMPclvord <> savclvord;
027100150123             IF  savclvord > *zeros;
027200150123               exsr scrivi;
027300150123             ENDIF;
027400150123             savclvord = WCMPclvord;
027500150126             savclv = WCMPclv;
027600150123             savcmm = WCMPcmm;
027700150123             savcar = WCMPcar;
027800150123             savdis = WCMPdis;
027900150123           ENDIF;
028000150109
028100150109           wVALaui = WCMPfat * WCMPaui;
028200150109           wVALaup = WCMPfat * WCMPaup;
028300150109           wVALauf = WCMPfat * WCMPauf;
028400150109           wVALautr = WCMPfat * WCMPautr;
028500150109           wVALaucf = WCMPfat * WCMPaucf;
028600150107
028700150121         //?INIZIALE  tutti i rcd del file di work
028800150123           claui += 1;
028900150123           ftaui += WCMPfat;
029000150123           valaui += wVALaui;
029100150121
029200150121         //?Se NON è un PERSO
029300150121           IF  WCMPperso = *blanks;
029400150107
029500150120           //?PROPOSTO
029600150120             IF  WCMPufe = FaseObjProp;
029700150123               claup += 1;
029800150123               ftaup += WCMPfat;
029900150123               valaup += wVALaup;
030000150114             ENDIF;
030100150107
030200150120           //?FINALE
030300150120             IF  WCMPufe = FaseObjFine;
030400150123               clauf += 1;
030500150123               ftauf += WCMPfat;
030600150123               valauf += wVALauf;
030700150114             ENDIF;
030800150107
030900150120           //?NON AVVIATI
031000150123             IF  WCMPnavvio = 'S';
031100150123               clnavvio += 1;
031200150123               ftnavvio += WCMPfat;
031300150123               valnavvio += wVALauf;
031400150114             ENDIF;
031500150120
031600150120           //?AVVIATI
031700150120             IF  WCMPavvio = 'S';
031800150123               clavvio += 1;
031900150123               ftavvio += WCMPfat;
032000150123               valavvio += wVALauf;
032100150123               vaoavvio += wVALautr;
032200150120             ENDIF;
032300150107
032400150120           //?AVVIATI CON OFFERTA
032500150120             IF  WCMPavvtr = 'S';
032600150123               clavvtr += 1;
032700150123               ftavvtr += WCMPfat;
032800150123               valavvtr += wVALauf;
032900150123               vaoavvtr += wVALautr;
033000150114             ENDIF;
033100150107
033200150120           //?CHIUSI
033300150114             IF  WCMPchiuso = 'S';
033400150123               clchiuso += 1;
033500150123               ftchiuso += WCMPfat;
033600150123               valchiuso += wVALauf;
033700150123               vaochiuso += wVALautr;
033800150114             ENDIF;
033900150107
034000150120         //?CONFRONTO
034100150120             IF  WCMPconfr = 'S';
034200150123               clconfr += 1;
034300150123               ftconfr += WCMPfat;
034400150123               valconfr += wVALauf;
034500150123               vaoconfr += wVALaucf;
034600150114             ENDIF;
034700150120
034800150120         //?NON CONFRONTATI
034900150120             IF  WCMPnocf = 'S';
035000150123               clnocf += 1;
035100150123               ftnocf += WCMPfat;
035200150123               valnocf += wVALauf;
035300150120             ENDIF;
035400150114           ENDIF;
035500150114
035600150114         //?Se è un PERSO
035700150114           IF  WCMPperso = 'S';
035800150123             clperso += 1;
035900150123             ftperso += WCMPfat;
036000150123             valperso += wVALaui;
036100150107           ENDIF;
036200150107
036300150107         ENDDO;
036400150107
036500150107         exec sql close WRK;
036600150108
036700150123         //?Scrivo ultima categoria cliente
036800150123         IF  savclvord > *zeros;
036900150123            exsr Scrivi;
037000150108          ENDIF;
037100141230
037200141230       ENDSR;
037300150107
037400150107       //--------------------------------------------------------------
037500150123       //?Scrivo/Aggiorno file di work.
037600150107       //--------------------------------------------------------------
037700150123       BEGSR Scrivi;
037800150123
037900150123       //?Totale per Azienda
038000150126         chain (WCMPncm:savclvord) WFCM101L;
038100150123         IF  %found(WFCM101L);
038200150123           exsr Aggiorna1;
038300150123         ELSE;
038400150123           exsr Scrivi1;
038500150123         ENDIF;
038600150123
038700150123       //?Totale per Distretto
038800150126         chain (WCMPncm:savdis:savclvord) WFCM201L;
038900150123         IF  %found(WFCM201L);
039000150123           exsr Aggiorna2;
039100150123         ELSE;
039200150123           exsr Scrivi2;
039300150123         ENDIF;
039400150123
039500150123       //?Totale per Area
039600150126         chain (WCMPncm:savdis:savcar:savclvord) WFCM301L;
039700150123         IF  %found(WFCM301L);
039800150123           exsr Aggiorna3;
039900150123         ELSE;
040000150123           exsr Scrivi3;
040100150123         ENDIF;
040200150123
040300150123       //?Totale per Commerciale
040400150126         chain (WCMPncm:savdis:savcar:savcmm:savclvord) WFCM401L;
040500150123         IF  %found(WFCM401L);
040600150123           exsr Aggiorna4;
040700150123         ELSE;
040800150123           exsr Scrivi4;
040900150123         ENDIF;
041000150123
041100150123         exsr Pulisci;
041200150107
041300150107       ENDSR;
041400150123
041500150123       //--------------------------------------------------------------
041600150123       //?Aggiorno file 1.
041700150123       //--------------------------------------------------------------
041800150123       BEGSR Aggiorna1;
041900150123
042000150123       //?riga 1 --> Clienti
042100150123         W1claui += claui;
042200150123         W1claup += claup;
042300150123         W1clauf += clauf;
042400150123         W1clnavvi += clnavvio;
042500150123         W1clavvio += clavvio;
042600150123         W1clavvtr += clavvtr;
042700150123         W1clchius += clchiuso;
042800150123         W1clconfr += clconfr;
042900150123         W1clnocf  += clnocf;
043000150123         W1clperso += clperso;
043100150123       //?riga 2 --> Fatturato
043200150123         W1ftaui += ftaui;
043300150123         W1ftaup += ftaup;
043400150123         W1ftauf += ftauf;
043500150123         W1ftnavvi += ftnavvio;
043600150123         W1ftavvio += ftavvio;
043700150123         W1ftavvtr += ftavvtr;
043800150123         W1ftchius += ftchiuso;
043900150123         W1ftconfr += ftconfr;
044000150123         W1ftnocf  += ftnocf;
044100150123         W1ftperso += ftperso;
044200150123       //?Valori per calcolo riga 3
044300150123         W1valaui += valaui;
044400150123         W1valaup += valaup;
044500150123         W1valauf += valauf;
044600150123         W1valnavvi += valnavvio;
044700150123         W1valavvio += valavvio;
044800150123         W1valavvtr += valavvtr;
044900150123         W1valchius += valchiuso;
045000150123         W1valconfr += valconfr;
045100150123         W1valnocf  += valnocf;
045200150123         W1valperso += valperso;
045300150123       //?Valori per calcolo riga 5
045400150123         W1vaoavvio += vaoavvio;
045500150123         W1vaoavvtr += vaoavvtr;
045600150123         W1vaochius += vaochiuso;
045700150123         W1vaoconfr += vaoconfr;
045800150123
045900150123         UPDATE WFCM1000;
046000150123
046100150123       ENDSR;
046200150123
046300150123       //--------------------------------------------------------------
046400150123       //?Scrivo file 1.
046500150123       //--------------------------------------------------------------
046600150123       BEGSR Scrivi1;
046700150123
046800150123        clear WFCM1000;
046900150123
047000150123       //?Dati comuni
047100150123         WCM1ncm = WCMPncm;
047200150123         WCM1clv = savclv;
047300150123         WCM1clvord = savclvord;
047400150123
047500150123       //?riga 1 --> Clienti
047600150123         W1claui = claui;
047700150123         W1claup = claup;
047800150123         W1clauf = clauf;
047900150123         W1clnavvi = clnavvio;
048000150123         W1clavvio = clavvio;
048100150123         W1clavvtr = clavvtr;
048200150123         W1clchius = clchiuso;
048300150123         W1clconfr = clconfr;
048400150123         W1clnocf  = clnocf;
048500150123         W1clperso = clperso;
048600150123       //?riga 2 --> Fatturato
048700150123         W1ftaui = ftaui;
048800150123         W1ftaup = ftaup;
048900150123         W1ftauf = ftauf;
049000150123         W1ftnavvi = ftnavvio;
049100150123         W1ftavvio = ftavvio;
049200150123         W1ftavvtr = ftavvtr;
049300150123         W1ftchius = ftchiuso;
049400150123         W1ftconfr = ftconfr;
049500150123         W1ftnocf  = ftnocf;
049600150123         W1ftperso = ftperso;
049700150123       //?Valori per calcolo riga 3
049800150123         W1valaui = valaui;
049900150123         W1valaup = valaup;
050000150123         W1valauf = valauf;
050100150123         W1valnavvi = valnavvio;
050200150123         W1valavvio = valavvio;
050300150123         W1valavvtr = valavvtr;
050400150123         W1valchius = valchiuso;
050500150123         W1valconfr = valconfr;
050600150123         W1valnocf  = valnocf;
050700150123         W1valperso = valperso;
050800150123       //?Valori per calcolo riga 5
050900150123         W1vaoavvio = vaoavvio;
051000150123         W1vaoavvtr = vaoavvtr;
051100150123         W1vaochius = vaochiuso;
051200150123         W1vaoconfr = vaoconfr;
051300150123
051400150123         WRITE WFCM1000;
051500150123
051600150123       ENDSR;
051700150123
051800150123       //--------------------------------------------------------------
051900150123       //?Aggiorno file 2.
052000150123       //--------------------------------------------------------------
052100150123       BEGSR Aggiorna2;
052200150123
052300150123       //?riga 1 --> Clienti
052400150123         W2claui += claui;
052500150123         W2claup += claup;
052600150123         W2clauf += clauf;
052700150123         W2clnavvi += clnavvio;
052800150123         W2clavvio += clavvio;
052900150123         W2clavvtr += clavvtr;
053000150123         W2clchius += clchiuso;
053100150123         W2clconfr += clconfr;
053200150123         W2clnocf  += clnocf;
053300150123         W2clperso += clperso;
053400150123       //?riga 2 --> Fatturato
053500150123         W2ftaui += ftaui;
053600150123         W2ftaup += ftaup;
053700150123         W2ftauf += ftauf;
053800150123         W2ftnavvi += ftnavvio;
053900150123         W2ftavvio += ftavvio;
054000150123         W2ftavvtr += ftavvtr;
054100150123         W2ftchius += ftchiuso;
054200150123         W2ftconfr += ftconfr;
054300150123         W2ftnocf  += ftnocf;
054400150123         W2ftperso += ftperso;
054500150123       //?Valori per calcolo riga 3
054600150123         W2valaui += valaui;
054700150123         W2valaup += valaup;
054800150123         W2valauf += valauf;
054900150123         W2valnavvi += valnavvio;
055000150123         W2valavvio += valavvio;
055100150123         W2valavvtr += valavvtr;
055200150123         W2valchius += valchiuso;
055300150123         W2valconfr += valconfr;
055400150123         W2valnocf  += valnocf;
055500150123         W2valperso += valperso;
055600150123       //?Valori per calcolo riga 5
055700150123         W2vaoavvio += vaoavvio;
055800150123         W2vaoavvtr += vaoavvtr;
055900150123         W2vaochius += vaochiuso;
056000150123         W2vaoconfr += vaoconfr;
056100150123
056200150123         UPDATE WFCM2000;
056300150123
056400150123       ENDSR;
056500150123
056600150123       //--------------------------------------------------------------
056700150123       //?Scrivo file 2.
056800150123       //--------------------------------------------------------------
056900150123       BEGSR Scrivi2;
057000150123
057100150123        clear WFCM2000;
057200150123
057300150123       //?Dati comuni
057400150123         WCM2ncm = WCMPncm;
057500150123         WCM2dis = savdis;
057600150123         WCM2clv = savclv;
057700150123         WCM2clvord = savclvord;
057800150123
057900150123       //?riga 1 --> Clienti
058000150123         W2claui = claui;
058100150123         W2claup = claup;
058200150123         W2clauf = clauf;
058300150123         W2clnavvi = clnavvio;
058400150123         W2clavvio = clavvio;
058500150123         W2clavvtr = clavvtr;
058600150123         W2clchius = clchiuso;
058700150123         W2clconfr = clconfr;
058800150123         W2clnocf  = clnocf;
058900150123         W2clperso = clperso;
059000150123       //?riga 2 --> Fatturato
059100150123         W2ftaui = ftaui;
059200150123         W2ftaup = ftaup;
059300150123         W2ftauf = ftauf;
059400150123         W2ftnavvi = ftnavvio;
059500150123         W2ftavvio = ftavvio;
059600150123         W2ftavvtr = ftavvtr;
059700150123         W2ftchius = ftchiuso;
059800150123         W2ftconfr = ftconfr;
059900150123         W2ftnocf  = ftnocf;
060000150123         W2ftperso = ftperso;
060100150123       //?Valori per calcolo riga 3
060200150123         W2valaui = valaui;
060300150123         W2valaup = valaup;
060400150123         W2valauf = valauf;
060500150123         W2valnavvi = valnavvio;
060600150123         W2valavvio = valavvio;
060700150123         W2valavvtr = valavvtr;
060800150123         W2valchius = valchiuso;
060900150123         W2valconfr = valconfr;
061000150123         W2valnocf  = valnocf;
061100150123         W2valperso = valperso;
061200150123       //?Valori per calcolo riga 5
061300150123         W2vaoavvio = vaoavvio;
061400150123         W2vaoavvtr = vaoavvtr;
061500150123         W2vaochius = vaochiuso;
061600150123         W2vaoconfr = vaoconfr;
061700150123
061800150123         WRITE WFCM2000;
061900150123
062000150123       ENDSR;
062100150123
062200150123       //--------------------------------------------------------------
062300150123       //?Aggiorno file 3.
062400150123       //--------------------------------------------------------------
062500150123       BEGSR Aggiorna3;
062600150123
062700150123       //?riga 1 --> Clienti
062800150123         W3claui += claui;
062900150123         W3claup += claup;
063000150123         W3clauf += clauf;
063100150123         W3clnavvi += clnavvio;
063200150123         W3clavvio += clavvio;
063300150123         W3clavvtr += clavvtr;
063400150123         W3clchius += clchiuso;
063500150123         W3clconfr += clconfr;
063600150123         W3clnocf  += clnocf;
063700150123         W3clperso += clperso;
063800150123       //?riga 2 --> Fatturato
063900150123         W3ftaui += ftaui;
064000150123         W3ftaup += ftaup;
064100150123         W3ftauf += ftauf;
064200150123         W3ftnavvi += ftnavvio;
064300150123         W3ftavvio += ftavvio;
064400150123         W3ftavvtr += ftavvtr;
064500150123         W3ftchius += ftchiuso;
064600150123         W3ftconfr += ftconfr;
064700150123         W3ftnocf  += ftnocf;
064800150123         W3ftperso += ftperso;
064900150123       //?Valori per calcolo riga 3
065000150123         W3valaui += valaui;
065100150123         W3valaup += valaup;
065200150123         W3valauf += valauf;
065300150123         W3valnavvi += valnavvio;
065400150123         W3valavvio += valavvio;
065500150123         W3valavvtr += valavvtr;
065600150123         W3valchius += valchiuso;
065700150123         W3valconfr += valconfr;
065800150123         W3valnocf  += valnocf;
065900150123         W3valperso += valperso;
066000150123       //?Valori per calcolo riga 5
066100150123         W3vaoavvio += vaoavvio;
066200150123         W3vaoavvtr += vaoavvtr;
066300150123         W3vaochius += vaochiuso;
066400150123         W3vaoconfr += vaoconfr;
066500150123
066600150123         UPDATE WFCM3000;
066700150123
066800150123       ENDSR;
066900150123
067000150123       //--------------------------------------------------------------
067100150123       //?Scrivo file 3.
067200150123       //--------------------------------------------------------------
067300150123       BEGSR Scrivi3;
067400150123
067500150123        clear WFCM3000;
067600150123
067700150123       //?Dati comuni
067800150123         WCM3ncm = WCMPncm;
067900150123         WCM3dis = savdis;
068000150123         WCM3car = savcar;
068100150123         WCM3clv = savclv;
068200150123         WCM3clvord = savclvord;
068300150123
068400150123       //?riga 1 --> Clienti
068500150123         W3claui = claui;
068600150123         W3claup = claup;
068700150123         W3clauf = clauf;
068800150123         W3clnavvi = clnavvio;
068900150123         W3clavvio = clavvio;
069000150123         W3clavvtr = clavvtr;
069100150123         W3clchius = clchiuso;
069200150123         W3clconfr = clconfr;
069300150123         W3clnocf  = clnocf;
069400150123         W3clperso = clperso;
069500150123       //?riga 2 --> Fatturato
069600150123         W3ftaui = ftaui;
069700150123         W3ftaup = ftaup;
069800150123         W3ftauf = ftauf;
069900150123         W3ftnavvi = ftnavvio;
070000150123         W3ftavvio = ftavvio;
070100150123         W3ftavvtr = ftavvtr;
070200150123         W3ftchius = ftchiuso;
070300150123         W3ftconfr = ftconfr;
070400150123         W3ftnocf  = ftnocf;
070500150123         W3ftperso = ftperso;
070600150123       //?Valori per calcolo riga 3
070700150123         W3valaui = valaui;
070800150123         W3valaup = valaup;
070900150123         W3valauf = valauf;
071000150123         W3valnavvi = valnavvio;
071100150123         W3valavvio = valavvio;
071200150123         W3valavvtr = valavvtr;
071300150123         W3valchius = valchiuso;
071400150123         W3valconfr = valconfr;
071500150123         W3valnocf  = valnocf;
071600150123         W3valperso = valperso;
071700150123       //?Valori per calcolo riga 5
071800150123         W3vaoavvio = vaoavvio;
071900150123         W3vaoavvtr = vaoavvtr;
072000150123         W3vaochius = vaochiuso;
072100150123         W3vaoconfr = vaoconfr;
072200150123
072300150123         WRITE WFCM3000;
072400150123
072500150123       ENDSR;
072600150123
072700150123       //--------------------------------------------------------------
072800150123       //?Aggiorno file 4.
072900150123       //--------------------------------------------------------------
073000150123       BEGSR Aggiorna4;
073100150123
073200150123       //?riga 1 --> Clienti
073300150123         W4claui += claui;
073400150123         W4claup += claup;
073500150123         W4clauf += clauf;
073600150123         W4clnavvi += clnavvio;
073700150123         W4clavvio += clavvio;
073800150123         W4clavvtr += clavvtr;
073900150123         W4clchius += clchiuso;
074000150123         W4clconfr += clconfr;
074100150123         W4clnocf  += clnocf;
074200150123         W4clperso += clperso;
074300150123       //?riga 2 --> Fatturato
074400150123         W4ftaui += ftaui;
074500150123         W4ftaup += ftaup;
074600150123         W4ftauf += ftauf;
074700150123         W4ftnavvi += ftnavvio;
074800150123         W4ftavvio += ftavvio;
074900150123         W4ftavvtr += ftavvtr;
075000150123         W4ftchius += ftchiuso;
075100150123         W4ftconfr += ftconfr;
075200150123         W4ftnocf  += ftnocf;
075300150123         W4ftperso += ftperso;
075400150123       //?Valori per calcolo riga 3
075500150123         W4valaui += valaui;
075600150123         W4valaup += valaup;
075700150123         W4valauf += valauf;
075800150123         W4valnavvi += valnavvio;
075900150123         W4valavvio += valavvio;
076000150123         W4valavvtr += valavvtr;
076100150123         W4valchius += valchiuso;
076200150123         W4valconfr += valconfr;
076300150123         W4valnocf  += valnocf;
076400150123         W4valperso += valperso;
076500150123       //?Valori per calcolo riga 5
076600150123         W4vaoavvio += vaoavvio;
076700150123         W4vaoavvtr += vaoavvtr;
076800150123         W4vaochius += vaochiuso;
076900150123         W4vaoconfr += vaoconfr;
077000150123
077100150123         UPDATE WFCM4000;
077200150123
077300150123       ENDSR;
077400150123
077500150123       //--------------------------------------------------------------
077600150123       //?Scrivo file 4.
077700150123       //--------------------------------------------------------------
077800150123       BEGSR Scrivi4;
077900150123
078000150123        clear WFCM4000;
078100150123
078200150123       //?Dati comuni
078300150123         WCM4ncm = WCMPncm;
078400150123         WCM4dis = savdis;
078500150123         WCM4car = savcar;
078600150123         WCM4cmm = savcmm;
078700150123         WCM4clv = savclv;
078800150123         WCM4clvord = savclvord;
078900150123
079000150123       //?riga 1 --> Clienti
079100150123         W4claui = claui;
079200150123         W4claup = claup;
079300150123         W4clauf = clauf;
079400150123         W4clnavvi = clnavvio;
079500150123         W4clavvio = clavvio;
079600150123         W4clavvtr = clavvtr;
079700150123         W4clchius = clchiuso;
079800150123         W4clconfr = clconfr;
079900150123         W4clnocf  = clnocf;
080000150123         W4clperso = clperso;
080100150123       //?riga 2 --> Fatturato
080200150123         W4ftaui = ftaui;
080300150123         W4ftaup = ftaup;
080400150123         W4ftauf = ftauf;
080500150123         W4ftnavvi = ftnavvio;
080600150123         W4ftavvio = ftavvio;
080700150123         W4ftavvtr = ftavvtr;
080800150123         W4ftchius = ftchiuso;
080900150123         W4ftconfr = ftconfr;
081000150123         W4ftnocf  = ftnocf;
081100150123         W4ftperso = ftperso;
081200150123       //?Valori per calcolo riga 3
081300150123         W4valaui = valaui;
081400150123         W4valaup = valaup;
081500150123         W4valauf = valauf;
081600150123         W4valnavvi = valnavvio;
081700150123         W4valavvio = valavvio;
081800150123         W4valavvtr = valavvtr;
081900150123         W4valchius = valchiuso;
082000150123         W4valconfr = valconfr;
082100150123         W4valnocf  = valnocf;
082200150123         W4valperso = valperso;
082300150123       //?Valori per calcolo riga 5
082400150123         W4vaoavvio = vaoavvio;
082500150123         W4vaoavvtr = vaoavvtr;
082600150123         W4vaochius = vaochiuso;
082700150123         W4vaoconfr = vaoconfr;
082800150123
082900150123         WRITE WFCM4000;
083000150123
083100150123       ENDSR;
083200150107
083300150107       //--------------------------------------------------------------
083400150123       //?Pulisco i campi di lavoro.
083500150107       //--------------------------------------------------------------
083600150107       BEGSR Pulisci;
083700150107
083800150123       //?riga 1 --> Clienti
083900150107         clear claui;
084000150107         clear claup;
084100150107         clear clauf;
084200150123         clear clnavvio;
084300150123         clear clavvio;
084400150123         clear clavvtr;
084500150123         clear clchiuso;
084600150123         clear clconfr;
084700150123         clear clnocf;
084800150123         clear clperso;
084900150107       //?riga 2 --> Fatturato
085000150108         clear ftaui;
085100150108         clear ftaup;
085200150108         clear ftauf;
085300150123         clear ftnavvio;
085400150123         clear ftavvio;
085500150123         clear ftavvtr;
085600150123         clear ftchiuso;
085700150123         clear ftconfr;
085800150123         clear ftnocf;
085900150123         clear ftperso;
086000150123       //?Valore per calcolo riga 3
086100150123         clear valaui;
086200150123         clear valaup;
086300150123         clear valauf;
086400150123         clear valnavvio;
086500150123         clear valavvio;
086600150123         clear valavvtr;
086700150123         clear valchiuso;
086800150123         clear valconfr;
086900150123         clear valnocf;
087000150123         clear valperso;
087100150123       //?Valore per calcolo riga 4
087200150123         clear vaoavvio;
087300150123         clear vaoavvtr;
087400150123         clear vaochiuso;
087500150123         clear vaoconfr;
087600150108
087700150107       ENDSR;
087800150108
087900141230       //--------------------------------------------------------------
088000141230       //?Operazioni finali.
088100141230       //--------------------------------------------------------------
088200141230       BEGSR RoutEnd;
088300141230
088400141230         *inLR = *on;
088500141230         return;
088600141230
088700141230       ENDSR;
088800141230
088900141230      /end-free
