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
001100160215      // - Organigramma
001200160215     fAZORG01L  if   e           k disk
001300160215
001400150623      // - Anagrafica Clienti Ritiro
001500150623     fFNACR01L  if   e           k disk
001600160215
001700150623      // - ORM DA VAS
001800150622     fFNVAO00F  uf   e             disk
001900150623     fFNVAOE1L  uf   e           k disk
002000150622     fTIORI01L  uf   e           k disk    usropn
002100150623
002200150623      // - TABELLE
002300150623     fTABEL00F  if   e           k disk
002400150713     fTNTBE01L  if   e           k disk
002500150622
002600150622      //---------------------------------------------------------------
002700150622      //?Definizione costanti.
002800150622      //---------------------------------------------------------------
002900150623     d CmdOvrDbF       c                   const('OVRDBF +
003000150623     d                                           file(TIORI01L) +
003100150623     d                                           tofile(')
003200150623
003300150623     d Digits          c                   const('0123456789')
003400151026
003500151026     d KeyCOR          c                   const('COR')
003600151026     d KeyCOR7         c                   const('COR7')
003700151026     d KeyPOE          c                   const('POE')
003800150622
003900150622      //---------------------------------------------------------------
004000150622      //?Definizione schiere.
004100150622      //---------------------------------------------------------------
004200150622
004300150622      //---------------------------------------------------------------
004400150622      //?Definizione aree dati.
004500150622      //---------------------------------------------------------------
004600150622
004700150622      //---------------------------------------------------------------
004800150622      //?Definizione strutture dati.
004900150622      //---------------------------------------------------------------
005000150622      // - Parametri ricevuti
005100150622     d KPJBA         e ds
005200150622     d Chiudi          s              1
005300150622     d Sistema         s             15
005400150622
005500150622      // - Gestione ORM
005600150623     d FIOR05DS      e ds                  inz
005700151026
005800151026      // - Ricerca/Controllo tabelle
005900151026     d TIBS02DS      e ds                  inz
006000150623
006100150623      // - Rcd "G" file FNVAOE0F
006200150623     d dOREGEN       e ds                  inz
006300150623
006400150623      // - Flag Operativi
006500150623     d dORM01        e ds                  inz
006600160215
006700160215      // - Campo ORGDE3 del fila AZORG00F
006800160215     d OG143         e ds                  inz
006900160215
007000160215      // - Tabella 15 - Nazioni
007100160215     d ds15          e ds                  inz
007200150622
007300150622      //---------------------------------------------------------------
007400150622      //?Definizione variabili globali.
007500150622      //---------------------------------------------------------------
007600150622      // - Flags booleani
007700150622     d Fine            s               n   inz(*off)
007800150622     d wEnd            s               n   inz(*off)
007900150623
008000150623      // - Indici di schiera
008100150623     d xx              s              4s 0 inz
008200150622
008300150622      // - Campi di comodo
008400150623     d cmd             s            512a   varying
008500150623     d wnrr            s              9s 0
008600151006     d wVAOcor         s                   like(VAOcor)
008700150713     d wVAOpoe         s                   like(VAOpoe)
008800150713     d wVAOtco         s                   like(VAOtco)
008900150623     d wtxt            S           2048
009000150622
009100150622      //---------------------------------------------------------------
009200150622      //?Definizione procedure usate.
009300150622      //---------------------------------------------------------------
009400150622       // - Gestione ORM
009500150622     d FIOR05R         pr                  extpgm('FIOR05R')
009600150622     d  kpjba                              likeds(kpjba)
009700150622     d  fior05ds                           likeds(fior05ds)
009800150622
009900150622      //---------------------------------------------------------------
010000150622      //?Definizione Prototipi.
010100150622      //---------------------------------------------------------------
010200151026       // - Controlla Tabelle
010300151026      /copy gaitrasrc/srcprotopr,TIBS02R
010400151026
010500150623       // - "Bonifica" stringa da caratteri indesiderati?
010600150623      /copy gaitrasrc/srcProtoPI,XCHKCHAR
010700150623      /copy gaitrasrc/srcProtoPR,XCHKCHAR
010800150623
010900150622       // - Esecuzione comando
011000150622      /copy gaitrasrc/srcprotopr,SYSTEM
011100150622
011200150622      //---------------------------------------------------------------
011300150622      //?Definizione key-list.
011400150622      //---------------------------------------------------------------
011500150623       // - File FNVAOE1L
011600150623     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
011700150623     d                                     prefix(k_)
011800150623     d                                     inz
011900150623
012000150623       // - File TABEL00F
012100150623     d k03tabel      e ds                  extname(TABEL00F:*key)
012200150623     d                                     prefix(k_)
012300150623     d                                     inz
012400150622
012500150622      //---------------------------------------------------------------
012600150622      //?M A I N - L I N E
012700150622      //---------------------------------------------------------------
012800150622
012900150622     c     *Entry        plist
013000150622     c                   parm                    Chiudi
013100150622     c                   parm                    Sistema
013200150622
013300150622      /free
013400150622
013500150622       //?Operazioni iniziali
013600150622       exsr RoutInz;
013700150622
013800150622       //?Elabora
013900150622       exsr Elabora;
014000150622
014100150622       //?Operazioni finali
014200150622       exsr RoutEnd;
014300150622
014400150622       //--------------------------------------------------------------
014500150622       //?Operazioni iniziali.
014600150622       //--------------------------------------------------------------
014700150622       BEGSR RoutInz;
014800150622
014900150622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015000150622
015100150622       //?Se chiusura del sistema devo chiudere il lavoro
015200150622         IF  %shtdn;
015300150622           Chiudi = 'S';
015400150622           exsr RoutEnd;
015500150622         ENDIF;
015600150622
015700150622       //?Apro il file TIORI
015800150623         IF  not %open(TIORI01L);
015900150623           cmd = CmdOvrDbF;
016000150623           IF  %subst(Sistema:1:5) = 'AS888';
016100150623             cmd = %trim(cmd) + 'GAITRAGRPS/TIORI01L)';
016200150623           ELSE;
016300150623             cmd = %trim(cmd) + 'GAITRAGRU/TIORI01L)';
016400150623           ENDIF;
016500150623           ExecuteCommand (cmd);
016600150623           open TIORI01L;
016700150622         ENDIF;
016800150622
016900150622       ENDSR;
017000150622
017100150622       //--------------------------------------------------------------
017200150622       //?Elabora.
017300150622       //--------------------------------------------------------------
017400150622       BEGSR Elabora;
017500150623
017600150623         wEnd = *off;
017700150623
017800150623       //?Leggo FNVAO
017900150623       //?non annullati
018000150623       //?solo quelli ancora da elaborare
018100150623       //?no prepagati
018200150623         exec sql
018300150623         DECLARE vao CURSOR for
018400151006         SELECT rrn(FNVAO00F), VAOpoe, VAOtco, VAOcor from FNVAO00F
018500150623         WHERE VAOatb = ' ' and substr(VAOflo, 11, 1) = ' ' and
018600150623               VAOtor <> 'P';
018700150623
018800150623       //?Apertura del cursore
018900150623         exec sql
019000150623         OPEN vao;
019100150623
019200150623         IF sqlcode < 0;
019300150623           wEnd = *on;
019400150623           exec sql
019500150623           CLOSE vao;
019600150623           leavesr;
019700150623         ENDIF;
019800150623
019900150623         DOW  not wEnd;
020000150623           exec sql
020100151006           FETCH NEXT from vao into :wnrr, :wVAOpoe, :wVAOtco, :wVAOcor;
020200150623           IF sqlcod = 100 or sqlcod < 0;
020300150623             wEnd = *on;
020400150623             leave;
020500150623           ENDIF;
020600151026
020700151026         //?Controllo se in tabella COV c'è la filiale emissione
020800151026         //?Se presente non confermo in automatico gli ORM emessi da questa filiale
020900151026         clear TIBS02DS;
021000151026         T02mod = 'C';
021100151026         T02sif = knsif;
021200151026         T02cod = 'COV';
021300151026         T02ke1 = KeyPOE;
021400151026         T02ke2 = %editc(wVAOpoe:'X');
021500151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
021600151026         IF  T02err = *blanks;
021700151026           iter;
021800151026         ENDIF;
021900151026
022000151026         //?Controllo se in tabella COV c'è il codice ordinante lungo 7 quindi tutti i luoghi
022100151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante lungo 7
022200151026         clear TIBS02DS;
022300151026         T02mod = 'C';
022400151026         T02sif = knsif;
022500151026         T02cod = 'COV';
022600151026         T02ke1 = KeyCOR7;
022700151026         T02ke2 = %subst(%editc(wVAOcor:'X'):1:7);
022800151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
022900151026         IF  T02err = *blanks;
023000151026           iter;
023100151026         ENDIF;
023200151026
023300151026         //?Controllo se in tabella COV c'è il codice ordinante
023400151026         //?Se presente non confermo in automatico gli ORM che hanno quel codice ordinante
023500151026         clear TIBS02DS;
023600151026         T02mod = 'C';
023700151026         T02sif = knsif;
023800151026         T02cod = 'COV';
023900151026         T02ke1 = KeyCOR;
024000151026         T02ke2 = %editc(wVAOcor:'X');
024100151026         TNTBE_RicercaControllo (kpjba:tibs02ds);
024200151026         IF  T02err = *blanks;
024300151026           iter;
024400151026         ENDIF;
024500150623
024600150623         //?Richiamo pgm conferma ORM
024700150623           exsr Conferma;
024800150623
024900150623         ENDDO;
025000150623
025100150623       //?Chiusura del cursore
025200150623         exec sql
025300150623         CLOSE vao;
025400150622
025500150622       ENDSR;
025600150623
025700150623       //--------------------------------------------------------------
025800150623       //?Confermo ORM.
025900150623       //--------------------------------------------------------------
026000150623       BEGSR Conferma;
026100150623
026200150623       //?Come prima cosa aggancio il rcd del VAO
026300150623         chain(e) wnrr FNVAO00F;
026400150623       //?se allocato vado via
026500150623         IF  %error;
026600150623           leavesr;
026700150623         ENDIF;
026800150623       //?se non trovo vado via
026900150623         IF  not %found(FNVAO00F);
027000150623           leavesr;
027100150623         ENDIF;
027200150623
027300150623         clear FIOR05DS;
027400150623       //?Conferma senza emissione video
027500150623         §RMtla = '0';
027600150623       //?Conferma da Internet/File Automatica (nel chiamato poi diventa 'C')
027700150623         §RMfpr = 'K';
027800150623       //?Numero ORM
027900150623         §RMpoe = VAOpoe;
028000150623         §RMnsr = VAOnsr;
028100150623         §RMnor = VAOnor;
028200150623         §RMnrv = VAOnrv;
028300150623       //?Tipo ORM
028400150623         §RMtor = VAOtor;
028500150623       //?Tipo Comunicazion ORM
028600150623         §RMtco = VAOtco;
028700150623       //?Data/Ora immissione
028800150623         §RMdao = VAOdao;
028900150623         §RMoao = VAOoao;
029000150623       //?Ordinante
029100150623         §RMcor = VAOcor;
029200150623         wtxt = VAOrso;
029300150623         exsr Bonifica;
029400150623         §RMrso = wtxt;
029500150623         wtxt = VAOino;
029600150623         exsr Bonifica;
029700150623         §RMino = wtxt;
029800150623         wtxt = VAOcao;
029900150623         exsr Bonifica;
030000150623         §RMcao = wtxt;
030100150623         wtxt = VAOloo;
030200150623         exsr Bonifica;
030300150623         §RMloo = wtxt;
030400150623         §RMpro = VAOpro;
030500150623         §RMnao = VAOnao;
030600150623       //?Mittente
030700150623         §RMcra = VAOcra;
030800150623         wtxt = VAOrsr;
030900150623         exsr Bonifica;
031000150623         §RMrsr = wtxt;
031100150623         wtxt = VAOinr;
031200150623         exsr Bonifica;
031300150623         §RMinr = wtxt;
031400150623         wtxt = VAOcar;
031500150623         exsr Bonifica;
031600150623         §RMcar = wtxt;
031700150623         wtxt = VAOlor;
031800150623         exsr Bonifica;
031900150623         §RMlor = wtxt;
032000150623         §RMprr = VAOprr;
032100150623         §RMnar = VAOnar;
032200150623       //?Data/Ora Ritiro
032300150623         §RMdar = VAOdar;
032400150623         §RMorr = VAOorr;
032500150623       //?Dati mittente da anagrafica clienti ritiro
032600150623         IF  VAOcra > *zeros;
032700150623           chain (VAOcra) FNACR01L;
032800150623           IF  %found(FNACR01L);
032900150623             §RMrsr = ACRrsr;
033000150623             §RMinr = ACRinr;
033100150623             §RMcar = ACRcar;
033200150623             §RMlor = ACRlor;
033300150623             §RMprr = ACRprr;
033400150623             §RMnar = ACRnar;
033500150623             IF  §RMorr = 0;
033600150623               §RMorr = ACRorr;
033700150623             ENDIF;
033800150623           ENDIF;
033900150623         ENDIF;
034000150623       //?Referente/Telefono
034100150623         wtxt = VAOrer;
034200150623         exsr Bonifica;
034300150623         §RMrer = wtxt;
034400150623         §RMter = VAOter;
034500150623       //?Destinatario
034600150623         §RMcrc = VAOcrc;
034700150623         wtxt = VAOrsc;
034800150623         exsr Bonifica;
034900150623         §RMrsc = wtxt;
035000150623         wtxt = VAOinc;
035100150623         exsr Bonifica;
035200150623         §RMinc = wtxt;
035300150623         wtxt = VAOcac;
035400150623         exsr Bonifica;
035500150623         §RMcac = wtxt;
035600150623         wtxt = VAOloc;
035700150623         exsr Bonifica;
035800150623         §RMloc = wtxt;
035900150623         §RMprc = VAOprc;
036000150623         §RMnac = VAOnac;
036100150623       //?Fermo Deposito
036200150623         §RMffd = VAOffd;
036300150623         IF  VAOffd = 'N';
036400150623           clear §RMffd;
036500150623         ENDIF;
036600150623       //?Mattino/Pomeriggio
036700150623         §RMrmp = VAOrmp;
036800150623       //?Natura merce
036900150623         wtxt = VAOnam;
037000150623         exsr Bonifica;
037100150623         §RMnam = wtxt;
037200150623       //?Quantità
037300150623         §RMncl = VAOncl;
037400150623         §RMpkg = VAOpkg;
037500150623         §RMvlm = VAOvlm;
037600150623         §RMbnc = VAObnc;
037700150623         §RMblc = VAOblc;
037800150623         §RMatt = VAOatt;
037900150623         §RMmtc = VAOmtc;
038000150623       //?Chi paga
038100150623         §RMpag = VAOpag;
038200150623         §RMksc = VAOksc;
038300150623         §RMctr = VAOctr;
038400150623       //?Filiale Ritiro/Consegna
038500150623         §RMpor = VAOpor;
038600150623         §RMpoc = VAOpoc;
038700150623       //?Note
038800150623         wtxt = VAOno1;
038900150623         exsr Bonifica;
039000150623         §RMno1 = wtxt;
039100150623         wtxt = VAOno2;
039200150623         exsr Bonifica;
039300150623         §RMno2 = wtxt;
039400150623       //?Flag DDT
039500150623         §RMddt = VAOddt;
039600150623       //?Riferimento
039700150623         wtxt = VAOrfa;
039800150623         exsr Bonifica;
039900150623         §RMrfa = wtxt;
040000150623       //?Priorità
040100150623         §RMsto = VAOsto;
040200150623       //?Sponda idraulica
040300150623         §RMspi = VAOspi;
040400150623       //?Flag Operativi
040500150623         §RMflo = VAOflo;
040600150623       //?Controllo che i dati di §RMflo siano giusti
040700150623         dORM01 = §RMflo;
040800150623         IF  §ORMcb <> 'S';
040900150623           clear §ORMcb;
041000150623         ENDIF;
041100150623         IF  §ORMfr <> 'S';
041200150623           clear §ORMfr;
041300150623         ENDIF;
041400150623         IF  §ORMks <> 'S';
041500150623           clear §ORMks;
041600150623         ENDIF;
041700150623         IF  §ORMfd <> 'S';
041800150623           clear §ORMfd;
041900150623         ENDIF;
042000150623         IF  §ORcom <> 'S';
042100150623           clear §ORcom;
042200150623         ENDIF;
042300150623         IF  §ORcomc <> 'S' and §ORcomc <> 'N';
042400150623           clear §ORcomc;
042500150623         ENDIF;
042600150623         IF  §ORsrm <> 'S';
042700150623           clear §ORsrm;
042800150623         ENDIF;
042900150623         §RMflo = dORM01;
043000160215
043100160215       //?Se destinatario estero e Filiale Consegna inserita è una fil.DPD
043200160215       //?devo cercare la filiale giusta
043300160215         clear OG143;
043400160215         clear ds15;
043500160215         IF  VAOpoc > 0 and VAOnac <> *blanks;
043600160215           chain (VAOpoc) AZORG01L;
043700160215           IF  %found(AZORG01L);
043800160215             OG143 = ORGde3;
043900160215           ENDIF;
044000160215           IF  §OGntw = 'DPD';
044100160215          //?cerco la lna sulla tabella nazioni
044200160215             k_TBLkut = 1;
044300160215             k_TBLcod = '15';
044400160215             k_TBLkey = VAOnac;
044500160215             chain %kds(K03tabel) TABEL00F;
044600160215             IF  %found(TABEL00F) and TBLflg = *blanks;
044700160215               ds15 = TBLuni;
044800160215               §RMpoc = §15lad;
044900160215             ENDIF;
045000160215           ENDIF;
045100160215         ENDIF;
045200150623
045300150623       //?Verifico se esiste il rcd 'G' su VAOE
045400150623       //?se esite controllo se OK il tipo servizio
045500150623       //?se NON è OK non devo memorizzarlo
045600150623         exsr VaoRcdG;
045700150623
045800150623       //?Richiamo FIOR05R per scrittura ORM
045900150623         clear kpjbu;
046000150623         fior05r (kpjba:FIOR05DS);
046100150623
046200150623       //?se torna errore flaggo il rcd come già elaborato automaticamente
046300150623         IF  §RMerr <> *blank;
046400150713           dORM01 = §RMflo;
046500150713           §ORMauto = 'X';
046600150714           VAOflo = dORM01;
046700150623           update FNVAO000;
046800150623           leavesr;
046900150623         ENDIF;
047000150623
047100150623       //?se OK cancello le estensioni e aggiorno TIORI
047200150623         exsr delVAOE;
047300150623         exsr aggTIORI;
047400150623         delete FNVAO000;
047500150623
047600150623       ENDSR;
047700150623
047800150623       //--------------------------------------------------------------
047900150623       //?Controllo caratteri.
048000150623       //--------------------------------------------------------------
048100150623       BEGSR  Bonifica;
048200150623
048300150623         clear TxtInOut;
048400150623         TxtInOut  = wtxt;
048500150623         UpperCase = '1';
048600150623         ChkNull   = '1';
048700150623
048800150623         xchkchar ( TxtInOut :
048900150623                    ElencoChar :
049000150623                    TipoElenco :
049100150623                    CharSost :
049200150623                    UpperCase :
049300150623                    ChkNull :
049400150623                    CharNull :
049500150623                    Esito );
049600150623
049700150623         // -?Modifica effettuata?
049800150623         IF  Esito = '1';
049900150623           wtxt = TxtInOut;
050000150623         ENDIF;
050100150623
050200150623       ENDSR;
050300150623
050400150623       //--------------------------------------------------------------------
050500150623       //?Controllo Tipo Servizio.
050600150623       //--------------------------------------------------------------------
050700150623       BEGSR VaoRcdG;
050800150623
050900150623         clear dOREGEN;
051000150623
051100150623       //?Imposto la chiave per il FNVAOE rcd 'G '
051200150623         k_VAOecor = VAOcor;
051300150623         k_VAOepoe = VAOpoe;
051400150623         k_VAOensr = VAOnsr;
051500150623         k_VAOenor = VAOnor;
051600150623         k_VAOenrv = VAOnrv;
051700150623         k_VAOetrc = 'G ';
051800150623         chain  %kds(k06fnvaoe) FNVAOE1L;
051900150623         IF  not %found(FNVAOE1L);
052000150623           leavesr;
052100150623         ENDIF;
052200150623         dOREGEN = VAOEdati;
052300150623         IF  §OREtsp <> *blanks;
052400150623       //?Imposto la chiave per TABELLA '5E'
052500150623           k_TBLkut = 1;
052600150623           k_TBLcod = '5E';
052700150623           k_TBLkey = §OREtsp;
052800150623           chain %kds(K03tabel) TABEL00F;
052900150623         //?Non esiste o annullato pulisco il campo
053000150623           IF  not %found(TABEL00F) or TBLflg <> *blanks;
053100150623             clear §OREtsp;
053200150623           ENDIF;
053300150623           VAOEdati = dOREGEN;
053400150623         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
053500150623           IF  VAOEdati = *blanks;
053600150623             delete FNVAOE00;
053700150623           ELSE;
053800150623             update FNVAOE00;
053900150623           ENDIF;
054000150623         ELSE;
054100150623           unlock FNVAOE1L;
054200150623         ENDIF;
054300150623
054400150623       ENDSR;
054500150623
054600150623       //--------------------------------------------------------------------
054700150623       //?Delete l'estensione del VAO.
054800150623       //--------------------------------------------------------------------
054900150623       BEGSR delVAOE;
055000150623
055100150623       //?Imposto la chiave per il FNVAOE
055200150623         k_VAOecor = VAOcor;
055300150623         k_VAOepoe = VAOpoe;
055400150623         k_VAOensr = VAOnsr;
055500150623         k_VAOenor = VAOnor;
055600150623         k_VAOenrv = VAOnrv;
055700150623       //?Cancello i rcd
055800150623         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
055900150623         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
056000150623         DOW not %eof(FNVAOE1L);
056100150623           delete fnvaoe00;
056200150623           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
056300150623         ENDDO;
056400150623
056500150623       ENDSR;
056600150623
056700150623       //--------------------------------------------------------------------
056800150623       //?Aggiorna TIORI00F.
056900150623       //--------------------------------------------------------------------
057000150623       BEGSR aggTIORI;
057100150623
057200150623       //?Il campo Progressivo di TIORI deve essere numerico
057300150928         // -?Se tutto vuoto: scarto?
057400150928         if  §ORMpg = *blank;
057500150928           leavesr;
057600150928         endif;
057700150928         // -?Se con caratteri alfabetici (non blank): scarto?
057800150623         xx = 1;
057900150623         FOR xx by 1 to %len(§ORMpg);
058000150623           IF  %subst(§ORMpg:xx:1) <> *blanks and
058100150623               %check(Digits:%subst(§ORMpg:xx:1)) > *zeros;
058200150623             leavesr;
058300150623           ENDIF;
058400150623         ENDFOR;
058500150623
058600150623       //?Aggiorno TIORI con il n. ORM
058700150623         ORIprg = %dec(§ORMpg:7:0);
058800150623         chain (ORIprg) TIORI01L;
058900150623         IF  %found(TIORI01L);
059000150623           ORIpoe = §RMpoeo;
059100150623           ORInsr = §RMnsro;
059200150623           ORInor = §RMnoro;
059300150623           ORInrv = §RMnrvo;
059400150623           update TIORI000;
059500150623         ENDIF;
059600150623
059700150623       ENDSR;
059800150622
059900150622       //--------------------------------------------------------------
060000150622       //?Operazioni finali.
060100150622       //--------------------------------------------------------------
060200150622       BEGSR RoutEnd;
060300150622
060400150622         *inLR = *on;
060500150622         return;
060600150622
060700150622       ENDSR;
