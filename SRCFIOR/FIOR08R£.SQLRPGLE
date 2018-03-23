000100150622      //--------------------------------------------------------------
000200150622      //?FIOR08R - CONFERMA AUTOMATICA ORM DA INTERNET/FILE
000300150622      //--------------------------------------------------------------
000400150622
000500150622     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150623     h dftactgrp(*no) actgrp(*caller)
000700150622
000800150622      //---------------------------------------------------------------
000900150622      //?Dichiarazione file.
001000150622      //---------------------------------------------------------------
001100150623      // - Anagrafica Clienti Ritiro
001200150623     fFNACR01L  if   e           k disk
001300150623      // - ORM DA VAS
001400150622     fFNVAO00F  uf   e             disk
001500150623     fFNVAOE1L  uf   e           k disk
001600150622     fTIORI01L  uf   e           k disk    usropn
001700150623
001800150623      // - TABELLE
001900150623     fTABEL00F  if   e           k disk
002000150713     fTNTBE01L  if   e           k disk
002100150622
002200150622      //---------------------------------------------------------------
002300150622      //?Definizione costanti.
002400150622      //---------------------------------------------------------------
002500150623     d CmdOvrDbF       c                   const('OVRDBF +
002600150623     d                                           file(TIORI01L) +
002700150623     d                                           tofile(')
002800150623
002900150623     d Digits          c                   const('0123456789')
003000151026
003100151026     d KeyCOR          c                   const('COR')
003200151026     d KeyCOR7         c                   const('COR7')
003300151026     d KeyPOE          c                   const('POE')
003400150622
003500150622      //---------------------------------------------------------------
003600150622      //?Definizione schiere.
003700150622      //---------------------------------------------------------------
003800150622
003900150622      //---------------------------------------------------------------
004000150622      //?Definizione aree dati.
004100150622      //---------------------------------------------------------------
004200150622
004300150622      //---------------------------------------------------------------
004400150622      //?Definizione strutture dati.
004500150622      //---------------------------------------------------------------
004600150622      // - Parametri ricevuti
004700150622     d KPJBA         e ds
004800150622     d Chiudi          s              1
004900150622     d Sistema         s             15
005000150622
005100150622      // - Gestione ORM
005200150623     d FIOR05DS      e ds                  inz
005300151026
005400151026      // - Ricerca/Controllo tabelle
005500151026     d TIBS02DS      e ds                  inz
005600150623
005700150623      // - Rcd "G" file FNVAOE0F
005800150623     d dOREGEN       e ds                  inz
005900150623
006000150623      // - Flag Operativi
006100150623     d dORM01        e ds                  inz
006200150622
006300150622      //---------------------------------------------------------------
006400150622      //?Definizione variabili globali.
006500150622      //---------------------------------------------------------------
006600150622      // - Flags booleani
006700150622     d Fine            s               n   inz(*off)
006800150622     d wEnd            s               n   inz(*off)
006900150623
007000150623      // - Indici di schiera
007100150623     d xx              s              4s 0 inz
007200150622
007300150622      // - Campi di comodo
007400150623     d cmd             s            512a   varying
007500150623     d wnrr            s              9s 0
007600151006     d wVAOcor         s                   like(VAOcor)
007700150713     d wVAOpoe         s                   like(VAOpoe)
007800150713     d wVAOtco         s                   like(VAOtco)
007900150623     d wtxt            S           2048
008000150622
008100150622      //---------------------------------------------------------------
008200150622      //?Definizione procedure usate.
008300150622      //---------------------------------------------------------------
008400150622       // - Gestione ORM
008500150622     d FIOR05R         pr                  extpgm('FIOR05R')
008600150622     d  kpjba                              likeds(kpjba)
008700150622     d  fior05ds                           likeds(fior05ds)
008800150622
008900150622      //---------------------------------------------------------------
009000150622      //?Definizione Prototipi.
009100150622      //---------------------------------------------------------------
009200151026       // - Controlla Tabelle
009300151026      /copy gaitrasrc/srcprotopr,TIBS02R
009400151026
009500150623       // - "Bonifica" stringa da caratteri indesiderati?
009600150623      /copy gaitrasrc/srcProtoPI,XCHKCHAR
009700150623      /copy gaitrasrc/srcProtoPR,XCHKCHAR
009800150623
009900150622       // - Esecuzione comando
010000150622      /copy gaitrasrc/srcprotopr,SYSTEM
010100150622
010200150622      //---------------------------------------------------------------
010300150622      //?Definizione key-list.
010400150622      //---------------------------------------------------------------
010500150623       // - File FNVAOE1L
010600150623     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
010700150623     d                                     prefix(k_)
010800150623     d                                     inz
010900150623
011000150623       // - File TABEL00F
011100150623     d k03tabel      e ds                  extname(TABEL00F:*key)
011200150623     d                                     prefix(k_)
011300150623     d                                     inz
011400150622
011500150622      //---------------------------------------------------------------
011600150622      //?M A I N - L I N E
011700150622      //---------------------------------------------------------------
011800150622
011900150622     c     *Entry        plist
012000150622     c                   parm                    Chiudi
012100150622     c                   parm                    Sistema
012200150622
012300150622      /free
012400150622
012500150622       //?Operazioni iniziali
012600150622       exsr RoutInz;
012700150622
012800150622       //?Elabora
012900150622       exsr Elabora;
013000150622
013100150622       //?Operazioni finali
013200150622       exsr RoutEnd;
013300150622
013400150622       //--------------------------------------------------------------
013500150622       //?Operazioni iniziali.
013600150622       //--------------------------------------------------------------
013700150622       BEGSR RoutInz;
013800150622
013900150622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014000150622
014100150622       //?Se chiusura del sistema devo chiudere il lavoro
014200150622         IF  %shtdn;
014300150622           Chiudi = 'S';
014400150622           exsr RoutEnd;
014500150622         ENDIF;
014600150622
014700150622       //?Apro il file TIORI
014800150623         IF  not %open(TIORI01L);
014900150623           cmd = CmdOvrDbF;
015000150623           IF  %subst(Sistema:1:5) = 'AS888';
015100150623             cmd = %trim(cmd) + 'GAITRAGRPS/TIORI01L)';
015200150623           ELSE;
015300150623             cmd = %trim(cmd) + 'GAITRAGRU/TIORI01L)';
015400150623           ENDIF;
015500150623           ExecuteCommand (cmd);
015600150623           open TIORI01L;
015700150622         ENDIF;
015800150622
015900150622       ENDSR;
016000150622
016100150622       //--------------------------------------------------------------
016200150622       //?Elabora.
016300150622       //--------------------------------------------------------------
016400150622       BEGSR Elabora;
016500150623
016600150623         wEnd = *off;
016700150623
016800150623       //?Leggo FNVAO
016900150623       //?non annullati
017000150623       //?solo quelli ancora da elaborare
017100150623       //?no prepagati
017200150623         exec sql
017300150623         DECLARE vao CURSOR for
017400151006         SELECT rrn(FNVAO00F), VAOpoe, VAOtco, VAOcor from FNVAO00F
017500150623         WHERE VAOatb = ' ' and substr(VAOflo, 11, 1) = ' ' and
017600150623               VAOtor <> 'P';
017700150623
017800150623       //?Apertura del cursore
017900150623         exec sql
018000150623         OPEN vao;
018100150623
018200150623         IF sqlcode < 0;
018300150623           wEnd = *on;
018400150623           exec sql
018500150623           CLOSE vao;
018600150623           leavesr;
018700150623         ENDIF;
018800150623
018900150623         DOW  not wEnd;
019000150623           exec sql
019100151006           FETCH NEXT from vao into :wnrr, :wVAOpoe, :wVAOtco, :wVAOcor;
019200150623           IF sqlcod = 100 or sqlcod < 0;
019300150623             wEnd = *on;
019400150623             leave;
019500150623           ENDIF;
019600151026
019700151026         //?Controllo se in tabella COV c'è la filiale emissione
019800151026         //?Se presente non confermo in automatico gli ORM emessi da questa filiale
019900151026         clear TIBS02DS;
020000151026         T02mod = 'C';
020100151026         T02sif = knsif;
020200151026         T02cod = 'COV';
020300151026         T02ke1 = KeyPOE;
020400151026         T02ke2 = %editc(wVAOpoe:'X');
020500151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
020600151026         IF  T02err = *blanks;
020700151026           iter;
020800151026         ENDIF;
020900151026
021000151026         //?Controllo se in tabella COV c'è il codice ordinante lungo 7 quindi tutti i luoghi
021100151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante lungo 7
021200151026         clear TIBS02DS;
021300151026         T02mod = 'C';
021400151026         T02sif = knsif;
021500151026         T02cod = 'COV';
021600151026         T02ke1 = KeyCOR7;
021700151026         T02ke2 = %subst(%editc(wVAOcor:'X'):1:7);
021800151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
021900151026         IF  T02err = *blanks;
022000151026           iter;
022100151026         ENDIF;
022200151026
022300151026         //?Controllo se in tabella COV c'è il codice ordinante
022400151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante
022500151026         clear TIBS02DS;
022600151026         T02mod = 'C';
022700151026         T02sif = knsif;
022800151026         T02cod = 'COV';
022900151026         T02ke1 = KeyCOR;
023000151026         T02ke2 = %editc(wVAOcor:'X');
023100151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
023200151026         IF  T02err = *blanks;
023300151026           iter;
023400151026         ENDIF;
023500150623
023600150623         //?Richiamo pgm conferma ORM
023700150623           exsr Conferma;
023800150623
023900150623         ENDDO;
024000150623
024100150623       //?Chiusura del cursore
024200150623         exec sql
024300150623         CLOSE vao;
024400150622
024500150622       ENDSR;
024600150623
024700150623       //--------------------------------------------------------------
024800150623       //?Confermo ORM.
024900150623       //--------------------------------------------------------------
025000150623       BEGSR Conferma;
025100150623
025200150623       //?Come prima cosa aggancio il rcd del VAO
025300150623         chain(e) wnrr FNVAO00F;
025400150623       //?se allocato vado via
025500150623         IF  %error;
025600150623           leavesr;
025700150623         ENDIF;
025800150623       //?se non trovo vado via
025900150623         IF  not %found(FNVAO00F);
026000150623           leavesr;
026100150623         ENDIF;
026200150623
026300150623         clear FIOR05DS;
026400150623       //?Conferma senza emissione video
026500150623         §RMtla = '0';
026600150623       //?Conferma da Internet/File Automatica (nel chiamato poi diventa 'C')
026700150623         §RMfpr = 'K';
026800150623       //?Numero ORM
026900150623         §RMpoe = VAOpoe;
027000150623         §RMnsr = VAOnsr;
027100150623         §RMnor = VAOnor;
027200150623         §RMnrv = VAOnrv;
027300150623       //?Tipo ORM
027400150623         §RMtor = VAOtor;
027500150623       //?Tipo Comunicazion ORM
027600150623         §RMtco = VAOtco;
027700150623       //?Data/Ora immissione
027800150623         §RMdao = VAOdao;
027900150623         §RMoao = VAOoao;
028000150623       //?Ordinante
028100150623         §RMcor = VAOcor;
028200150623         wtxt = VAOrso;
028300150623         exsr Bonifica;
028400150623         §RMrso = wtxt;
028500150623         wtxt = VAOino;
028600150623         exsr Bonifica;
028700150623         §RMino = wtxt;
028800150623         wtxt = VAOcao;
028900150623         exsr Bonifica;
029000150623         §RMcao = wtxt;
029100150623         wtxt = VAOloo;
029200150623         exsr Bonifica;
029300150623         §RMloo = wtxt;
029400150623         §RMpro = VAOpro;
029500150623         §RMnao = VAOnao;
029600150623       //?Mittente
029700150623         §RMcra = VAOcra;
029800150623         wtxt = VAOrsr;
029900150623         exsr Bonifica;
030000150623         §RMrsr = wtxt;
030100150623         wtxt = VAOinr;
030200150623         exsr Bonifica;
030300150623         §RMinr = wtxt;
030400150623         wtxt = VAOcar;
030500150623         exsr Bonifica;
030600150623         §RMcar = wtxt;
030700150623         wtxt = VAOlor;
030800150623         exsr Bonifica;
030900150623         §RMlor = wtxt;
031000150623         §RMprr = VAOprr;
031100150623         §RMnar = VAOnar;
031200150623       //?Data/Ora Ritiro
031300150623         §RMdar = VAOdar;
031400150623         §RMorr = VAOorr;
031500150623       //?Dati mittente da anagrafica clienti ritiro
031600150623         IF  VAOcra > *zeros;
031700150623           chain (VAOcra) FNACR01L;
031800150623           IF  %found(FNACR01L);
031900150623             §RMrsr = ACRrsr;
032000150623             §RMinr = ACRinr;
032100150623             §RMcar = ACRcar;
032200150623             §RMlor = ACRlor;
032300150623             §RMprr = ACRprr;
032400150623             §RMnar = ACRnar;
032500150623             IF  §RMorr = 0;
032600150623               §RMorr = ACRorr;
032700150623             ENDIF;
032800150623           ENDIF;
032900150623         ENDIF;
033000150623       //?Referente/Telefono
033100150623         wtxt = VAOrer;
033200150623         exsr Bonifica;
033300150623         §RMrer = wtxt;
033400150623         §RMter = VAOter;
033500150623       //?Destinatario
033600150623         §RMcrc = VAOcrc;
033700150623         wtxt = VAOrsc;
033800150623         exsr Bonifica;
033900150623         §RMrsc = wtxt;
034000150623         wtxt = VAOinc;
034100150623         exsr Bonifica;
034200150623         §RMinc = wtxt;
034300150623         wtxt = VAOcac;
034400150623         exsr Bonifica;
034500150623         §RMcac = wtxt;
034600150623         wtxt = VAOloc;
034700150623         exsr Bonifica;
034800150623         §RMloc = wtxt;
034900150623         §RMprc = VAOprc;
035000150623         §RMnac = VAOnac;
035100150623       //?Fermo Deposito
035200150623         §RMffd = VAOffd;
035300150623         IF  VAOffd = 'N';
035400150623           clear §RMffd;
035500150623         ENDIF;
035600150623       //?Mattino/Pomeriggio
035700150623         §RMrmp = VAOrmp;
035800150623       //?Natura merce
035900150623         wtxt = VAOnam;
036000150623         exsr Bonifica;
036100150623         §RMnam = wtxt;
036200150623       //?Quantità
036300150623         §RMncl = VAOncl;
036400150623         §RMpkg = VAOpkg;
036500150623         §RMvlm = VAOvlm;
036600150623         §RMbnc = VAObnc;
036700150623         §RMblc = VAOblc;
036800150623         §RMatt = VAOatt;
036900150623         §RMmtc = VAOmtc;
037000150623       //?Chi paga
037100150623         §RMpag = VAOpag;
037200150623         §RMksc = VAOksc;
037300150623         §RMctr = VAOctr;
037400150623       //?Filiale Ritiro/Consegna
037500150623         §RMpor = VAOpor;
037600150623         §RMpoc = VAOpoc;
037700150623       //?Note
037800150623         wtxt = VAOno1;
037900150623         exsr Bonifica;
038000150623         §RMno1 = wtxt;
038100150623         wtxt = VAOno2;
038200150623         exsr Bonifica;
038300150623         §RMno2 = wtxt;
038400150623       //?Flag DDT
038500150623         §RMddt = VAOddt;
038600150623       //?Riferimento
038700150623         wtxt = VAOrfa;
038800150623         exsr Bonifica;
038900150623         §RMrfa = wtxt;
039000150623       //?Priorità
039100150623         §RMsto = VAOsto;
039200150623       //?Sponda idraulica
039300150623         §RMspi = VAOspi;
039400150623       //?Flag Operativi
039500150623         §RMflo = VAOflo;
039600150623       //?Controllo che i dati di §RMflo siano giusti
039700150623         dORM01 = §RMflo;
039800150623         IF  §ORMcb <> 'S';
039900150623           clear §ORMcb;
040000150623         ENDIF;
040100150623         IF  §ORMfr <> 'S';
040200150623           clear §ORMfr;
040300150623         ENDIF;
040400150623         IF  §ORMks <> 'S';
040500150623           clear §ORMks;
040600150623         ENDIF;
040700150623         IF  §ORMfd <> 'S';
040800150623           clear §ORMfd;
040900150623         ENDIF;
041000150623         IF  §ORcom <> 'S';
041100150623           clear §ORcom;
041200150623         ENDIF;
041300150623         IF  §ORcomc <> 'S' and §ORcomc <> 'N';
041400150623           clear §ORcomc;
041500150623         ENDIF;
041600150623         IF  §ORsrm <> 'S';
041700150623           clear §ORsrm;
041800150623         ENDIF;
041900150623         §RMflo = dORM01;
042000150623
042100150623       //?Verifico se esiste il rcd 'G' su VAOE
042200150623       //?se esite controllo se OK il tipo servizio
042300150623       //?se NON è OK non devo memorizzarlo
042400150623         exsr VaoRcdG;
042500150623
042600150623       //?Richiamo FIOR05R per scrittura ORM
042700150623         clear kpjbu;
042800150623         fior05r (kpjba:FIOR05DS);
042900150623
043000150623       //?se torna errore flaggo il rcd come già elaborato automaticamente
043100150623         IF  §RMerr <> *blank;
043200150713           dORM01 = §RMflo;
043300150713           §ORMauto = 'X';
043400150714           VAOflo = dORM01;
043500150623           update FNVAO000;
043600150623           leavesr;
043700150623         ENDIF;
043800150623
043900150623       //?se OK cancello le estensioni e aggiorno TIORI
044000150623         exsr delVAOE;
044100150623         exsr aggTIORI;
044200150623         delete FNVAO000;
044300150623
044400150623       ENDSR;
044500150623
044600150623       //--------------------------------------------------------------
044700150623       //?Controllo caratteri.
044800150623       //--------------------------------------------------------------
044900150623       BEGSR  Bonifica;
045000150623
045100150623         clear TxtInOut;
045200150623         TxtInOut  = wtxt;
045300150623         UpperCase = '1';
045400150623         ChkNull   = '1';
045500150623
045600150623         xchkchar ( TxtInOut :
045700150623                    ElencoChar :
045800150623                    TipoElenco :
045900150623                    CharSost :
046000150623                    UpperCase :
046100150623                    ChkNull :
046200150623                    CharNull :
046300150623                    Esito );
046400150623
046500150623         // -?Modifica effettuata?
046600150623         IF  Esito = '1';
046700150623           wtxt = TxtInOut;
046800150623         ENDIF;
046900150623
047000150623       ENDSR;
047100150623
047200150623       //--------------------------------------------------------------------
047300150623       //?Controllo Tipo Servizio.
047400150623       //--------------------------------------------------------------------
047500150623       BEGSR VaoRcdG;
047600150623
047700150623         clear dOREGEN;
047800150623
047900150623       //?Imposto la chiave per il FNVAOE rcd 'G '
048000150623         k_VAOecor = VAOcor;
048100150623         k_VAOepoe = VAOpoe;
048200150623         k_VAOensr = VAOnsr;
048300150623         k_VAOenor = VAOnor;
048400150623         k_VAOenrv = VAOnrv;
048500150623         k_VAOetrc = 'G ';
048600150623         chain  %kds(k06fnvaoe) FNVAOE1L;
048700150623         IF  not %found(FNVAOE1L);
048800150623           leavesr;
048900150623         ENDIF;
049000150623         dOREGEN = VAOEdati;
049100150623         IF  §OREtsp <> *blanks;
049200150623       //?Imposto la chiave per TABELLA '5E'
049300150623           k_TBLkut = 1;
049400150623           k_TBLcod = '5E';
049500150623           k_TBLkey = §OREtsp;
049600150623           chain %kds(K03tabel) TABEL00F;
049700150623         //?Non esiste o annullato pulisco il campo
049800150623           IF  not %found(TABEL00F) or TBLflg <> *blanks;
049900150623             clear §OREtsp;
050000150623           ENDIF;
050100150623           VAOEdati = dOREGEN;
050200150623         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
050300150623           IF  VAOEdati = *blanks;
050400150623             delete FNVAOE00;
050500150623           ELSE;
050600150623             update FNVAOE00;
050700150623           ENDIF;
050800150623         ELSE;
050900150623           unlock FNVAOE1L;
051000150623         ENDIF;
051100150623
051200150623       ENDSR;
051300150623
051400150623       //--------------------------------------------------------------------
051500150623       //?Delete l'estensione del VAO.
051600150623       //--------------------------------------------------------------------
051700150623       BEGSR delVAOE;
051800150623
051900150623       //?Imposto la chiave per il FNVAOE
052000150623         k_VAOecor = VAOcor;
052100150623         k_VAOepoe = VAOpoe;
052200150623         k_VAOensr = VAOnsr;
052300150623         k_VAOenor = VAOnor;
052400150623         k_VAOenrv = VAOnrv;
052500150623       //?Cancello i rcd
052600150623         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
052700150623         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
052800150623         DOW not %eof(FNVAOE1L);
052900150623           delete fnvaoe00;
053000150623           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
053100150623         ENDDO;
053200150623
053300150623       ENDSR;
053400150623
053500150623       //--------------------------------------------------------------------
053600150623       //?Aggiorna TIORI00F.
053700150623       //--------------------------------------------------------------------
053800150623       BEGSR aggTIORI;
053900150623
054000150623       //?Il campo Progressivo di TIORI deve essere numerico
054100150928         // -?Se tutto vuoto: scarto?
054200150928         if  §ORMpg = *blank;
054300150928           leavesr;
054400150928         endif;
054500150928         // -?Se con caratteri alfabetici (non blank): scarto?
054600150623         xx = 1;
054700150623         FOR xx by 1 to %len(§ORMpg);
054800150623           IF  %subst(§ORMpg:xx:1) <> *blanks and
054900150623               %check(Digits:%subst(§ORMpg:xx:1)) > *zeros;
055000150623             leavesr;
055100150623           ENDIF;
055200150623         ENDFOR;
055300150623
055400150623       //?Aggiorno TIORI con il n. ORM
055500150623         ORIprg = %dec(§ORMpg:7:0);
055600150623         chain (ORIprg) TIORI01L;
055700150623         IF  %found(TIORI01L);
055800150623           ORIpoe = §RMpoeo;
055900150623           ORInsr = §RMnsro;
056000150623           ORInor = §RMnoro;
056100150623           ORInrv = §RMnrvo;
056200150623           update TIORI000;
056300150623         ENDIF;
056400150623
056500150623       ENDSR;
056600150622
056700150622       //--------------------------------------------------------------
056800150622       //?Operazioni finali.
056900150622       //--------------------------------------------------------------
057000150622       BEGSR RoutEnd;
057100150622
057200150622         *inLR = *on;
057300150622         return;
057400150622
057500150622       ENDSR;
