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
003800150713     d skFIL           s              3  0 dim(64)
003900150713     d skTCO           s              1    dim(64)
004000150622
004100150622      //---------------------------------------------------------------
004200150622      //?Definizione aree dati.
004300150622      //---------------------------------------------------------------
004400150622
004500150622      //---------------------------------------------------------------
004600150622      //?Definizione strutture dati.
004700150622      //---------------------------------------------------------------
004800150622      // - Parametri ricevuti
004900150622     d KPJBA         e ds
005000150622     d Chiudi          s              1
005100150622     d Sistema         s             15
005200150622
005300150622      // - Gestione ORM
005400150623     d FIOR05DS      e ds                  inz
005500151026
005600151026      // - Ricerca/Controllo tabelle
005700151026     d TIBS02DS      e ds                  inz
005800150623
005900150623      // - Rcd "G" file FNVAOE0F
006000150623     d dOREGEN       e ds                  inz
006100150623
006200150623      // - Flag Operativi
006300150623     d dORM01        e ds                  inz
006400150713
006500150713      // - Campo TBEuni
006600150713     d wTBEuni         ds           256
006700150713     d  SKfiltco               1    256    dim(64)
006800150622
006900150622      //---------------------------------------------------------------
007000150622      //?Definizione variabili globali.
007100150622      //---------------------------------------------------------------
007200150622      // - Flags booleani
007300150622     d Fine            s               n   inz(*off)
007400150713     d RecOK           s               n   inz(*off)
007500150622     d wEnd            s               n   inz(*off)
007600150623
007700150623      // - Indici di schiera
007800150623     d xx              s              4s 0 inz
007900150622
008000150622      // - Campi di comodo
008100150623     d cmd             s            512a   varying
008200150623     d wnrr            s              9s 0
008300151006     d wVAOcor         s                   like(VAOcor)
008400150713     d wVAOpoe         s                   like(VAOpoe)
008500150713     d wVAOtco         s                   like(VAOtco)
008600150623     d wtxt            S           2048
008700150622
008800150622      //---------------------------------------------------------------
008900150622      //?Definizione procedure usate.
009000150622      //---------------------------------------------------------------
009100150622       // - Gestione ORM
009200150622     d FIOR05R         pr                  extpgm('FIOR05R')
009300150622     d  kpjba                              likeds(kpjba)
009400150622     d  fior05ds                           likeds(fior05ds)
009500150622
009600150622      //---------------------------------------------------------------
009700150622      //?Definizione Prototipi.
009800150622      //---------------------------------------------------------------
009900151026       // - Controlla Tabelle
010000151026      /copy gaitrasrc/srcprotopr,TIBS02R
010100151026
010200150623       // - "Bonifica" stringa da caratteri indesiderati?
010300150623      /copy gaitrasrc/srcProtoPI,XCHKCHAR
010400150623      /copy gaitrasrc/srcProtoPR,XCHKCHAR
010500150623
010600150622       // - Esecuzione comando
010700150622      /copy gaitrasrc/srcprotopr,SYSTEM
010800150622
010900150622      //---------------------------------------------------------------
011000150622      //?Definizione key-list.
011100150622      //---------------------------------------------------------------
011200150623       // - File FNVAOE1L
011300150623     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
011400150623     d                                     prefix(k_)
011500150623     d                                     inz
011600150623
011700150623       // - File TABEL00F
011800150623     d k03tabel      e ds                  extname(TABEL00F:*key)
011900150623     d                                     prefix(k_)
012000150623     d                                     inz
012100150622
012200150622      //---------------------------------------------------------------
012300150622      //?M A I N - L I N E
012400150622      //---------------------------------------------------------------
012500150622
012600150622     c     *Entry        plist
012700150622     c                   parm                    Chiudi
012800150622     c                   parm                    Sistema
012900150622
013000150622      /free
013100150622
013200150622       //?Operazioni iniziali
013300150622       exsr RoutInz;
013400150622
013500150622       //?Elabora
013600150622       exsr Elabora;
013700150622
013800150622       //?Operazioni finali
013900150622       exsr RoutEnd;
014000150622
014100150622       //--------------------------------------------------------------
014200150622       //?Operazioni iniziali.
014300150622       //--------------------------------------------------------------
014400150622       BEGSR RoutInz;
014500150622
014600150622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014700150622
014800150622       //?Se chiusura del sistema devo chiudere il lavoro
014900150622         IF  %shtdn;
015000150622           Chiudi = 'S';
015100150622           exsr RoutEnd;
015200150622         ENDIF;
015300150622
015400150622       //?Apro il file TIORI
015500150623         IF  not %open(TIORI01L);
015600150623           cmd = CmdOvrDbF;
015700150623           IF  %subst(Sistema:1:5) = 'AS888';
015800150623             cmd = %trim(cmd) + 'GAITRAGRPS/TIORI01L)';
015900150623           ELSE;
016000150623             cmd = %trim(cmd) + 'GAITRAGRU/TIORI01L)';
016100150623           ENDIF;
016200150623           ExecuteCommand (cmd);
016300150623           open TIORI01L;
016400150622         ENDIF;
016500150713
016600150713       //?Carico in sk le filiali emissioni attive alla procedura
016700150713       //?con il relativo tipo comunicazione ORM da confermare
016800150713         TBEcod = 'VPO';
016900150713         TBEke1 = 'ORMVAS';
017000150713         chain (TBEcod:TBEke1) TNTBE01L;
017100150713         IF  not %found(TNTBE01L);
017200150713           clear SKfiltco;
017300150713           leavesr;
017400150713         ENDIF;
017500150713         wTBEuni = TBEuni;
017600150713         xx = 1;
017700150713         FOR xx by 1 to %elem(SKfiltco);
017800150713           IF  SKfiltco(xx) <> *zeros and SKfiltco(xx) <> *blanks;
017900150713             skFIL(xx) = %dec(%subst(SKfiltco(xx):1:3):3:0);
018000150713             skTCO(xx) = %subst(SKfiltco(xx):4:1);
018100150713           ENDIF;
018200150713         ENDFOR;
018300150622
018400150622       ENDSR;
018500150622
018600150622       //--------------------------------------------------------------
018700150622       //?Elabora.
018800150622       //--------------------------------------------------------------
018900150622       BEGSR Elabora;
019000150623
019100150623         wEnd = *off;
019200150623
019300150623       //?Leggo FNVAO
019400150623       //?non annullati
019500150623       //?solo quelli ancora da elaborare
019600150623       //?no prepagati
019700150623         exec sql
019800150623         DECLARE vao CURSOR for
019900151006         SELECT rrn(FNVAO00F), VAOpoe, VAOtco, VAOcor from FNVAO00F
020000150623         WHERE VAOatb = ' ' and substr(VAOflo, 11, 1) = ' ' and
020100150623               VAOtor <> 'P';
020200150623
020300150623       //?Apertura del cursore
020400150623         exec sql
020500150623         OPEN vao;
020600150623
020700150623         IF sqlcode < 0;
020800150623           wEnd = *on;
020900150623           exec sql
021000150623           CLOSE vao;
021100150623           leavesr;
021200150623         ENDIF;
021300150623
021400150623         DOW  not wEnd;
021500150623           exec sql
021600151006           FETCH NEXT from vao into :wnrr, :wVAOpoe, :wVAOtco, :wVAOcor;
021700150623           IF sqlcod = 100 or sqlcod < 0;
021800150623             wEnd = *on;
021900150623             leave;
022000150623           ENDIF;
022100151026
022200151026         //?Controllo se in tabella COV c'è la filiale emissione
022300151026         //?Se presente non confermo in automatico gli ORM emessi da questa filiale
022400151026         clear TIBS02DS;
022500151026         T02mod = 'C';
022600151026         T02sif = knsif;
022700151026         T02cod = 'COV';
022800151026         T02ke1 = KeyPOE;
022900151026         T02ke2 = %editc(wVAOpoe:'X');
023000151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
023100151026         IF  T02err = *blanks;
023200151026           iter;
023300151026         ENDIF;
023400151026
023500151026         //?Controllo se in tabella COV c'è il codice ordinante lungo 7 quindi tutti i luoghi
023600151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante lungo 7
023700151026         clear TIBS02DS;
023800151026         T02mod = 'C';
023900151026         T02sif = knsif;
024000151026         T02cod = 'COV';
024100151026         T02ke1 = KeyCOR7;
024200151026         T02ke2 = %subst(%editc(wVAOcor:'X'):1:7);
024300151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
024400151026         IF  T02err = *blanks;
024500151026           iter;
024600151026         ENDIF;
024700151026
024800151026         //?Controllo se in tabella COV c'è il codice ordinante
024900151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante
025000151026         clear TIBS02DS;
025100151026         T02mod = 'C';
025200151026         T02sif = knsif;
025300151026         T02cod = 'COV';
025400151026         T02ke1 = KeyCOR;
025500151026         T02ke2 = %editc(wVAOcor:'X');
025600151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
025700151026         IF  T02err = *blanks;
025800151026           iter;
025900151026         ENDIF;
026000151005
026100151005         //?Non devo confermare in automatico gli ORM con ordinante FedEx
026200151005         //?0891495000
026300151026          //IF  wVAOcor = 0891495000;
026400151026          //iter;
026500151026          //ENDIF;
026600150713
026700150714         //?Controlla se OK filiale + tipo comunicazione ORM
026800150713           IF  skFIL(01) <> 999 and skFIL(01) > 0;
026900150713             exsr ContrRec;
027000150713           ENDIF;
027100150714         //?Se il primo rcd della tabella è 999 solo il tipo comunicazione impostato
027200150714           IF  skFIL(01) = 999 and skTCO(01) <> *blanks;
027300150714             exsr ContrRec;
027400150714           ENDIF;
027500150714         //?Se il primo rcd della tabella è 999 e non c'è il tipo comunicazione faccio tutto
027600150714           IF  skFIL(01) = 999 and skTCO(01) = *blanks;
027700150714             RecOK = *on;
027800150714           ENDIF;
027900150713           IF  not RecOK;
028000150713             iter;
028100150713           ENDIF;
028200150623
028300150623         //?Richiamo pgm conferma ORM
028400150623           exsr Conferma;
028500150623
028600150623         ENDDO;
028700150623
028800150623       //?Chiusura del cursore
028900150623         exec sql
029000150623         CLOSE vao;
029100150622
029200150622       ENDSR;
029300150713
029400150713       //--------------------------------------------------------------
029500150713       //?Controllo se record OK.
029600150713       //--------------------------------------------------------------
029700150713       BEGSR ContrRec;
029800150713
029900150713         RecOK = *off;
030000150713
030100150714         IF  skFIL(01) <> 999;
030200150714           clear xx;
030300150714           IF  wVAOpoe = *zeros;
030400150714             leavesr;
030500150714           ENDIF;
030600150714           xx = %lookup(wVAOpoe:skFIL);
030700150714           IF  xx = 0;
030800150714             leavesr;
030900150714           ENDIF;
031000150713
031100150714           IF  wVAOtco <> skTCO(xx) and skTCO(xx) <> *blanks;
031200150714             leavesr;
031300150714           ENDIF;
031400150714         ELSE;
031500150714           IF  wVAOtco <> skTCO(01);
031600150714             leavesr;
031700150714           ENDIF;
031800150714         ENDIF;
031900150713
032000150713         RecOK = *on;
032100150713
032200150713       ENDSR;
032300150623
032400150623       //--------------------------------------------------------------
032500150623       //?Confermo ORM.
032600150623       //--------------------------------------------------------------
032700150623       BEGSR Conferma;
032800150623
032900150623       //?Come prima cosa aggancio il rcd del VAO
033000150623         chain(e) wnrr FNVAO00F;
033100150623       //?se allocato vado via
033200150623         IF  %error;
033300150623           leavesr;
033400150623         ENDIF;
033500150623       //?se non trovo vado via
033600150623         IF  not %found(FNVAO00F);
033700150623           leavesr;
033800150623         ENDIF;
033900150623
034000150623         clear FIOR05DS;
034100150623       //?Conferma senza emissione video
034200150623         §RMtla = '0';
034300150623       //?Conferma da Internet/File Automatica (nel chiamato poi diventa 'C')
034400150623         §RMfpr = 'K';
034500150623       //?Numero ORM
034600150623         §RMpoe = VAOpoe;
034700150623         §RMnsr = VAOnsr;
034800150623         §RMnor = VAOnor;
034900150623         §RMnrv = VAOnrv;
035000150623       //?Tipo ORM
035100150623         §RMtor = VAOtor;
035200150623       //?Tipo Comunicazion ORM
035300150623         §RMtco = VAOtco;
035400150623       //?Data/Ora immissione
035500150623         §RMdao = VAOdao;
035600150623         §RMoao = VAOoao;
035700150623       //?Ordinante
035800150623         §RMcor = VAOcor;
035900150623         wtxt = VAOrso;
036000150623         exsr Bonifica;
036100150623         §RMrso = wtxt;
036200150623         wtxt = VAOino;
036300150623         exsr Bonifica;
036400150623         §RMino = wtxt;
036500150623         wtxt = VAOcao;
036600150623         exsr Bonifica;
036700150623         §RMcao = wtxt;
036800150623         wtxt = VAOloo;
036900150623         exsr Bonifica;
037000150623         §RMloo = wtxt;
037100150623         §RMpro = VAOpro;
037200150623         §RMnao = VAOnao;
037300150623       //?Mittente
037400150623         §RMcra = VAOcra;
037500150623         wtxt = VAOrsr;
037600150623         exsr Bonifica;
037700150623         §RMrsr = wtxt;
037800150623         wtxt = VAOinr;
037900150623         exsr Bonifica;
038000150623         §RMinr = wtxt;
038100150623         wtxt = VAOcar;
038200150623         exsr Bonifica;
038300150623         §RMcar = wtxt;
038400150623         wtxt = VAOlor;
038500150623         exsr Bonifica;
038600150623         §RMlor = wtxt;
038700150623         §RMprr = VAOprr;
038800150623         §RMnar = VAOnar;
038900150623       //?Data/Ora Ritiro
039000150623         §RMdar = VAOdar;
039100150623         §RMorr = VAOorr;
039200150623       //?Dati mittente da anagrafica clienti ritiro
039300150623         IF  VAOcra > *zeros;
039400150623           chain (VAOcra) FNACR01L;
039500150623           IF  %found(FNACR01L);
039600150623             §RMrsr = ACRrsr;
039700150623             §RMinr = ACRinr;
039800150623             §RMcar = ACRcar;
039900150623             §RMlor = ACRlor;
040000150623             §RMprr = ACRprr;
040100150623             §RMnar = ACRnar;
040200150623             IF  §RMorr = 0;
040300150623               §RMorr = ACRorr;
040400150623             ENDIF;
040500150623           ENDIF;
040600150623         ENDIF;
040700150623       //?Referente/Telefono
040800150623         wtxt = VAOrer;
040900150623         exsr Bonifica;
041000150623         §RMrer = wtxt;
041100150623         §RMter = VAOter;
041200150623       //?Destinatario
041300150623         §RMcrc = VAOcrc;
041400150623         wtxt = VAOrsc;
041500150623         exsr Bonifica;
041600150623         §RMrsc = wtxt;
041700150623         wtxt = VAOinc;
041800150623         exsr Bonifica;
041900150623         §RMinc = wtxt;
042000150623         wtxt = VAOcac;
042100150623         exsr Bonifica;
042200150623         §RMcac = wtxt;
042300150623         wtxt = VAOloc;
042400150623         exsr Bonifica;
042500150623         §RMloc = wtxt;
042600150623         §RMprc = VAOprc;
042700150623         §RMnac = VAOnac;
042800150623       //?Fermo Deposito
042900150623         §RMffd = VAOffd;
043000150623         IF  VAOffd = 'N';
043100150623           clear §RMffd;
043200150623         ENDIF;
043300150623       //?Mattino/Pomeriggio
043400150623         §RMrmp = VAOrmp;
043500150623       //?Natura merce
043600150623         wtxt = VAOnam;
043700150623         exsr Bonifica;
043800150623         §RMnam = wtxt;
043900150623       //?Quantità
044000150623         §RMncl = VAOncl;
044100150623         §RMpkg = VAOpkg;
044200150623         §RMvlm = VAOvlm;
044300150623         §RMbnc = VAObnc;
044400150623         §RMblc = VAOblc;
044500150623         §RMatt = VAOatt;
044600150623         §RMmtc = VAOmtc;
044700150623       //?Chi paga
044800150623         §RMpag = VAOpag;
044900150623         §RMksc = VAOksc;
045000150623         §RMctr = VAOctr;
045100150623       //?Filiale Ritiro/Consegna
045200150623         §RMpor = VAOpor;
045300150623         §RMpoc = VAOpoc;
045400150623       //?Note
045500150623         wtxt = VAOno1;
045600150623         exsr Bonifica;
045700150623         §RMno1 = wtxt;
045800150623         wtxt = VAOno2;
045900150623         exsr Bonifica;
046000150623         §RMno2 = wtxt;
046100150623       //?Flag DDT
046200150623         §RMddt = VAOddt;
046300150623       //?Riferimento
046400150623         wtxt = VAOrfa;
046500150623         exsr Bonifica;
046600150623         §RMrfa = wtxt;
046700150623       //?Priorità
046800150623         §RMsto = VAOsto;
046900150623       //?Sponda idraulica
047000150623         §RMspi = VAOspi;
047100150623       //?Flag Operativi
047200150623         §RMflo = VAOflo;
047300150623       //?Controllo che i dati di §RMflo siano giusti
047400150623         dORM01 = §RMflo;
047500150623         IF  §ORMcb <> 'S';
047600150623           clear §ORMcb;
047700150623         ENDIF;
047800150623         IF  §ORMfr <> 'S';
047900150623           clear §ORMfr;
048000150623         ENDIF;
048100150623         IF  §ORMks <> 'S';
048200150623           clear §ORMks;
048300150623         ENDIF;
048400150623         IF  §ORMfd <> 'S';
048500150623           clear §ORMfd;
048600150623         ENDIF;
048700150623         IF  §ORcom <> 'S';
048800150623           clear §ORcom;
048900150623         ENDIF;
049000150623         IF  §ORcomc <> 'S' and §ORcomc <> 'N';
049100150623           clear §ORcomc;
049200150623         ENDIF;
049300150623         IF  §ORsrm <> 'S';
049400150623           clear §ORsrm;
049500150623         ENDIF;
049600150623         §RMflo = dORM01;
049700150623
049800150623       //?Verifico se esiste il rcd 'G' su VAOE
049900150623       //?se esite controllo se OK il tipo servizio
050000150623       //?se NON è OK non devo memorizzarlo
050100150623         exsr VaoRcdG;
050200150623
050300150623       //?Richiamo FIOR05R per scrittura ORM
050400150623         clear kpjbu;
050500150623         fior05r (kpjba:FIOR05DS);
050600150623
050700150623       //?se torna errore flaggo il rcd come già elaborato automaticamente
050800150623         IF  §RMerr <> *blank;
050900150713           dORM01 = §RMflo;
051000150713           §ORMauto = 'X';
051100150714           VAOflo = dORM01;
051200150623           update FNVAO000;
051300150623           leavesr;
051400150623         ENDIF;
051500150623
051600150623       //?se OK cancello le estensioni e aggiorno TIORI
051700150623         exsr delVAOE;
051800150623         exsr aggTIORI;
051900150623         delete FNVAO000;
052000150623
052100150623       ENDSR;
052200150623
052300150623       //--------------------------------------------------------------
052400150623       //?Controllo caratteri.
052500150623       //--------------------------------------------------------------
052600150623       BEGSR  Bonifica;
052700150623
052800150623         clear TxtInOut;
052900150623         TxtInOut  = wtxt;
053000150623         UpperCase = '1';
053100150623         ChkNull   = '1';
053200150623
053300150623         xchkchar ( TxtInOut :
053400150623                    ElencoChar :
053500150623                    TipoElenco :
053600150623                    CharSost :
053700150623                    UpperCase :
053800150623                    ChkNull :
053900150623                    CharNull :
054000150623                    Esito );
054100150623
054200150623         // -?Modifica effettuata?
054300150623         IF  Esito = '1';
054400150623           wtxt = TxtInOut;
054500150623         ENDIF;
054600150623
054700150623       ENDSR;
054800150623
054900150623       //--------------------------------------------------------------------
055000150623       //?Controllo Tipo Servizio.
055100150623       //--------------------------------------------------------------------
055200150623       BEGSR VaoRcdG;
055300150623
055400150623         clear dOREGEN;
055500150623
055600150623       //?Imposto la chiave per il FNVAOE rcd 'G '
055700150623         k_VAOecor = VAOcor;
055800150623         k_VAOepoe = VAOpoe;
055900150623         k_VAOensr = VAOnsr;
056000150623         k_VAOenor = VAOnor;
056100150623         k_VAOenrv = VAOnrv;
056200150623         k_VAOetrc = 'G ';
056300150623         chain  %kds(k06fnvaoe) FNVAOE1L;
056400150623         IF  not %found(FNVAOE1L);
056500150623           leavesr;
056600150623         ENDIF;
056700150623         dOREGEN = VAOEdati;
056800150623         IF  §OREtsp <> *blanks;
056900150623       //?Imposto la chiave per TABELLA '5E'
057000150623           k_TBLkut = 1;
057100150623           k_TBLcod = '5E';
057200150623           k_TBLkey = §OREtsp;
057300150623           chain %kds(K03tabel) TABEL00F;
057400150623         //?Non esiste o annullato pulisco il campo
057500150623           IF  not %found(TABEL00F) or TBLflg <> *blanks;
057600150623             clear §OREtsp;
057700150623           ENDIF;
057800150623           VAOEdati = dOREGEN;
057900150623         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
058000150623           IF  VAOEdati = *blanks;
058100150623             delete FNVAOE00;
058200150623           ELSE;
058300150623             update FNVAOE00;
058400150623           ENDIF;
058500150623         ELSE;
058600150623           unlock FNVAOE1L;
058700150623         ENDIF;
058800150623
058900150623       ENDSR;
059000150623
059100150623       //--------------------------------------------------------------------
059200150623       //?Delete l'estensione del VAO.
059300150623       //--------------------------------------------------------------------
059400150623       BEGSR delVAOE;
059500150623
059600150623       //?Imposto la chiave per il FNVAOE
059700150623         k_VAOecor = VAOcor;
059800150623         k_VAOepoe = VAOpoe;
059900150623         k_VAOensr = VAOnsr;
060000150623         k_VAOenor = VAOnor;
060100150623         k_VAOenrv = VAOnrv;
060200150623       //?Cancello i rcd
060300150623         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
060400150623         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
060500150623         DOW not %eof(FNVAOE1L);
060600150623           delete fnvaoe00;
060700150623           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
060800150623         ENDDO;
060900150623
061000150623       ENDSR;
061100150623
061200150623       //--------------------------------------------------------------------
061300150623       //?Aggiorna TIORI00F.
061400150623       //--------------------------------------------------------------------
061500150623       BEGSR aggTIORI;
061600150623
061700150623       //?Il campo Progressivo di TIORI deve essere numerico
061800150928         // -?Se tutto vuoto: scarto?
061900150928         if  §ORMpg = *blank;
062000150928           leavesr;
062100150928         endif;
062200150928         // -?Se con caratteri alfabetici (non blank): scarto?
062300150623         xx = 1;
062400150623         FOR xx by 1 to %len(§ORMpg);
062500150623           IF  %subst(§ORMpg:xx:1) <> *blanks and
062600150623               %check(Digits:%subst(§ORMpg:xx:1)) > *zeros;
062700150623             leavesr;
062800150623           ENDIF;
062900150623         ENDFOR;
063000150623
063100150623       //?Aggiorno TIORI con il n. ORM
063200150623         ORIprg = %dec(§ORMpg:7:0);
063300150623         chain (ORIprg) TIORI01L;
063400150623         IF  %found(TIORI01L);
063500150623           ORIpoe = §RMpoeo;
063600150623           ORInsr = §RMnsro;
063700150623           ORInor = §RMnoro;
063800150623           ORInrv = §RMnrvo;
063900150623           update TIORI000;
064000150623         ENDIF;
064100150623
064200150623       ENDSR;
064300150622
064400150622       //--------------------------------------------------------------
064500150622       //?Operazioni finali.
064600150622       //--------------------------------------------------------------
064700150622       BEGSR RoutEnd;
064800150622
064900150622         *inLR = *on;
065000150622         return;
065100150622
065200150622       ENDSR;
