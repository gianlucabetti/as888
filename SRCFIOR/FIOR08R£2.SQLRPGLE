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
003000150622
003100150622      //---------------------------------------------------------------
003200150622      //?Definizione schiere.
003300150622      //---------------------------------------------------------------
003400150713     d skFIL           s              3  0 dim(64)
003500150713     d skTCO           s              1    dim(64)
003600150622
003700150622      //---------------------------------------------------------------
003800150622      //?Definizione aree dati.
003900150622      //---------------------------------------------------------------
004000150622
004100150622      //---------------------------------------------------------------
004200150622      //?Definizione strutture dati.
004300150622      //---------------------------------------------------------------
004400150622      // - Parametri ricevuti
004500150622     d KPJBA         e ds
004600150622     d Chiudi          s              1
004700150622     d Sistema         s             15
004800150622
004900150622      // - Gestione ORM
005000150623     d FIOR05DS      e ds                  inz
005100150623
005200150623      // - Rcd "G" file FNVAOE0F
005300150623     d dOREGEN       e ds                  inz
005400150623
005500150623      // - Flag Operativi
005600150623     d dORM01        e ds                  inz
005700150713
005800150713      // - Campo TBEuni
005900150713     d wTBEuni         ds           256
006000150713     d  SKfiltco               1    256    dim(64)
006100150622
006200150622      //---------------------------------------------------------------
006300150622      //?Definizione variabili globali.
006400150622      //---------------------------------------------------------------
006500150622      // - Flags booleani
006600150622     d Fine            s               n   inz(*off)
006700150713     d RecOK           s               n   inz(*off)
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
009200150623       // - "Bonifica" stringa da caratteri indesiderati?
009300150623      /copy gaitrasrc/srcProtoPI,XCHKCHAR
009400150623      /copy gaitrasrc/srcProtoPR,XCHKCHAR
009500150623
009600150622       // - Esecuzione comando
009700150622      /copy gaitrasrc/srcprotopr,SYSTEM
009800150622
009900150622      //---------------------------------------------------------------
010000150622      //?Definizione key-list.
010100150622      //---------------------------------------------------------------
010200150623       // - File FNVAOE1L
010300150623     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
010400150623     d                                     prefix(k_)
010500150623     d                                     inz
010600150623
010700150623       // - File TABEL00F
010800150623     d k03tabel      e ds                  extname(TABEL00F:*key)
010900150623     d                                     prefix(k_)
011000150623     d                                     inz
011100150622
011200150622      //---------------------------------------------------------------
011300150622      //?M A I N - L I N E
011400150622      //---------------------------------------------------------------
011500150622
011600150622     c     *Entry        plist
011700150622     c                   parm                    Chiudi
011800150622     c                   parm                    Sistema
011900150622
012000150622      /free
012100150622
012200150622       //?Operazioni iniziali
012300150622       exsr RoutInz;
012400150622
012500150622       //?Elabora
012600150622       exsr Elabora;
012700150622
012800150622       //?Operazioni finali
012900150622       exsr RoutEnd;
013000150622
013100150622       //--------------------------------------------------------------
013200150622       //?Operazioni iniziali.
013300150622       //--------------------------------------------------------------
013400150622       BEGSR RoutInz;
013500150622
013600150622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013700150622
013800150622       //?Se chiusura del sistema devo chiudere il lavoro
013900150622         IF  %shtdn;
014000150622           Chiudi = 'S';
014100150622           exsr RoutEnd;
014200150622         ENDIF;
014300150622
014400150622       //?Apro il file TIORI
014500150623         IF  not %open(TIORI01L);
014600150623           cmd = CmdOvrDbF;
014700150623           IF  %subst(Sistema:1:5) = 'AS888';
014800150623             cmd = %trim(cmd) + 'GAITRAGRPS/TIORI01L)';
014900150623           ELSE;
015000150623             cmd = %trim(cmd) + 'GAITRAGRU/TIORI01L)';
015100150623           ENDIF;
015200150623           ExecuteCommand (cmd);
015300150623           open TIORI01L;
015400150622         ENDIF;
015500150713
015600150713       //?Carico in sk le filiali emissioni attive alla procedura
015700150713       //?con il relativo tipo comunicazione ORM da confermare
015800150713         TBEcod = 'VPO';
015900150713         TBEke1 = 'ORMVAS';
016000150713         chain (TBEcod:TBEke1) TNTBE01L;
016100150713         IF  not %found(TNTBE01L);
016200150713           clear SKfiltco;
016300150713           leavesr;
016400150713         ENDIF;
016500150713         wTBEuni = TBEuni;
016600150713         xx = 1;
016700150713         FOR xx by 1 to %elem(SKfiltco);
016800150713           IF  SKfiltco(xx) <> *zeros and SKfiltco(xx) <> *blanks;
016900150713             skFIL(xx) = %dec(%subst(SKfiltco(xx):1:3):3:0);
017000150713             skTCO(xx) = %subst(SKfiltco(xx):4:1);
017100150713           ENDIF;
017200150713         ENDFOR;
017300150622
017400150622       ENDSR;
017500150622
017600150622       //--------------------------------------------------------------
017700150622       //?Elabora.
017800150622       //--------------------------------------------------------------
017900150622       BEGSR Elabora;
018000150623
018100150623         wEnd = *off;
018200150623
018300150623       //?Leggo FNVAO
018400150623       //?non annullati
018500150623       //?solo quelli ancora da elaborare
018600150623       //?no prepagati
018700150623         exec sql
018800150623         DECLARE vao CURSOR for
018900151006         SELECT rrn(FNVAO00F), VAOpoe, VAOtco, VAOcor from FNVAO00F
019000150623         WHERE VAOatb = ' ' and substr(VAOflo, 11, 1) = ' ' and
019100150623               VAOtor <> 'P';
019200150623
019300150623       //?Apertura del cursore
019400150623         exec sql
019500150623         OPEN vao;
019600150623
019700150623         IF sqlcode < 0;
019800150623           wEnd = *on;
019900150623           exec sql
020000150623           CLOSE vao;
020100150623           leavesr;
020200150623         ENDIF;
020300150623
020400150623         DOW  not wEnd;
020500150623           exec sql
020600151006           FETCH NEXT from vao into :wnrr, :wVAOpoe, :wVAOtco, :wVAOcor;
020700150623           IF sqlcod = 100 or sqlcod < 0;
020800150623             wEnd = *on;
020900150623             leave;
021000150623           ENDIF;
021100151005
021200151005         //?Non devo confermare in automatico gli ORM con ordinante FedEx
021300151005         //?0891495000
021400151006           IF  wVAOcor = 0891495000;
021500151005             iter;
021600151005           ENDIF;
021700150713
021800150714         //?Controlla se OK filiale + tipo comunicazione ORM
021900150713           IF  skFIL(01) <> 999 and skFIL(01) > 0;
022000150713             exsr ContrRec;
022100150713           ENDIF;
022200150714         //?Se il primo rcd della tabella è 999 solo il tipo comunicazione impostato
022300150714           IF  skFIL(01) = 999 and skTCO(01) <> *blanks;
022400150714             exsr ContrRec;
022500150714           ENDIF;
022600150714         //?Se il primo rcd della tabella è 999 e non c'è il tipo comunicazione faccio tutto
022700150714           IF  skFIL(01) = 999 and skTCO(01) = *blanks;
022800150714             RecOK = *on;
022900150714           ENDIF;
023000150713           IF  not RecOK;
023100150713             iter;
023200150713           ENDIF;
023300150623
023400150623         //?Richiamo pgm conferma ORM
023500150623           exsr Conferma;
023600150623
023700150623         ENDDO;
023800150623
023900150623       //?Chiusura del cursore
024000150623         exec sql
024100150623         CLOSE vao;
024200150622
024300150622       ENDSR;
024400150713
024500150713       //--------------------------------------------------------------
024600150713       //?Controllo se record OK.
024700150713       //--------------------------------------------------------------
024800150713       BEGSR ContrRec;
024900150713
025000150713         RecOK = *off;
025100150713
025200150714         IF  skFIL(01) <> 999;
025300150714           clear xx;
025400150714           IF  wVAOpoe = *zeros;
025500150714             leavesr;
025600150714           ENDIF;
025700150714           xx = %lookup(wVAOpoe:skFIL);
025800150714           IF  xx = 0;
025900150714             leavesr;
026000150714           ENDIF;
026100150713
026200150714           IF  wVAOtco <> skTCO(xx) and skTCO(xx) <> *blanks;
026300150714             leavesr;
026400150714           ENDIF;
026500150714         ELSE;
026600150714           IF  wVAOtco <> skTCO(01);
026700150714             leavesr;
026800150714           ENDIF;
026900150714         ENDIF;
027000150713
027100150713         RecOK = *on;
027200150713
027300150713       ENDSR;
027400150623
027500150623       //--------------------------------------------------------------
027600150623       //?Confermo ORM.
027700150623       //--------------------------------------------------------------
027800150623       BEGSR Conferma;
027900150623
028000150623       //?Come prima cosa aggancio il rcd del VAO
028100150623         chain(e) wnrr FNVAO00F;
028200150623       //?se allocato vado via
028300150623         IF  %error;
028400150623           leavesr;
028500150623         ENDIF;
028600150623       //?se non trovo vado via
028700150623         IF  not %found(FNVAO00F);
028800150623           leavesr;
028900150623         ENDIF;
029000150623
029100150623         clear FIOR05DS;
029200150623       //?Conferma senza emissione video
029300150623         §RMtla = '0';
029400150623       //?Conferma da Internet/File Automatica (nel chiamato poi diventa 'C')
029500150623         §RMfpr = 'K';
029600150623       //?Numero ORM
029700150623         §RMpoe = VAOpoe;
029800150623         §RMnsr = VAOnsr;
029900150623         §RMnor = VAOnor;
030000150623         §RMnrv = VAOnrv;
030100150623       //?Tipo ORM
030200150623         §RMtor = VAOtor;
030300150623       //?Tipo Comunicazion ORM
030400150623         §RMtco = VAOtco;
030500150623       //?Data/Ora immissione
030600150623         §RMdao = VAOdao;
030700150623         §RMoao = VAOoao;
030800150623       //?Ordinante
030900150623         §RMcor = VAOcor;
031000150623         wtxt = VAOrso;
031100150623         exsr Bonifica;
031200150623         §RMrso = wtxt;
031300150623         wtxt = VAOino;
031400150623         exsr Bonifica;
031500150623         §RMino = wtxt;
031600150623         wtxt = VAOcao;
031700150623         exsr Bonifica;
031800150623         §RMcao = wtxt;
031900150623         wtxt = VAOloo;
032000150623         exsr Bonifica;
032100150623         §RMloo = wtxt;
032200150623         §RMpro = VAOpro;
032300150623         §RMnao = VAOnao;
032400150623       //?Mittente
032500150623         §RMcra = VAOcra;
032600150623         wtxt = VAOrsr;
032700150623         exsr Bonifica;
032800150623         §RMrsr = wtxt;
032900150623         wtxt = VAOinr;
033000150623         exsr Bonifica;
033100150623         §RMinr = wtxt;
033200150623         wtxt = VAOcar;
033300150623         exsr Bonifica;
033400150623         §RMcar = wtxt;
033500150623         wtxt = VAOlor;
033600150623         exsr Bonifica;
033700150623         §RMlor = wtxt;
033800150623         §RMprr = VAOprr;
033900150623         §RMnar = VAOnar;
034000150623       //?Data/Ora Ritiro
034100150623         §RMdar = VAOdar;
034200150623         §RMorr = VAOorr;
034300150623       //?Dati mittente da anagrafica clienti ritiro
034400150623         IF  VAOcra > *zeros;
034500150623           chain (VAOcra) FNACR01L;
034600150623           IF  %found(FNACR01L);
034700150623             §RMrsr = ACRrsr;
034800150623             §RMinr = ACRinr;
034900150623             §RMcar = ACRcar;
035000150623             §RMlor = ACRlor;
035100150623             §RMprr = ACRprr;
035200150623             §RMnar = ACRnar;
035300150623             IF  §RMorr = 0;
035400150623               §RMorr = ACRorr;
035500150623             ENDIF;
035600150623           ENDIF;
035700150623         ENDIF;
035800150623       //?Referente/Telefono
035900150623         wtxt = VAOrer;
036000150623         exsr Bonifica;
036100150623         §RMrer = wtxt;
036200150623         §RMter = VAOter;
036300150623       //?Destinatario
036400150623         §RMcrc = VAOcrc;
036500150623         wtxt = VAOrsc;
036600150623         exsr Bonifica;
036700150623         §RMrsc = wtxt;
036800150623         wtxt = VAOinc;
036900150623         exsr Bonifica;
037000150623         §RMinc = wtxt;
037100150623         wtxt = VAOcac;
037200150623         exsr Bonifica;
037300150623         §RMcac = wtxt;
037400150623         wtxt = VAOloc;
037500150623         exsr Bonifica;
037600150623         §RMloc = wtxt;
037700150623         §RMprc = VAOprc;
037800150623         §RMnac = VAOnac;
037900150623       //?Fermo Deposito
038000150623         §RMffd = VAOffd;
038100150623         IF  VAOffd = 'N';
038200150623           clear §RMffd;
038300150623         ENDIF;
038400150623       //?Mattino/Pomeriggio
038500150623         §RMrmp = VAOrmp;
038600150623       //?Natura merce
038700150623         wtxt = VAOnam;
038800150623         exsr Bonifica;
038900150623         §RMnam = wtxt;
039000150623       //?Quantità
039100150623         §RMncl = VAOncl;
039200150623         §RMpkg = VAOpkg;
039300150623         §RMvlm = VAOvlm;
039400150623         §RMbnc = VAObnc;
039500150623         §RMblc = VAOblc;
039600150623         §RMatt = VAOatt;
039700150623         §RMmtc = VAOmtc;
039800150623       //?Chi paga
039900150623         §RMpag = VAOpag;
040000150623         §RMksc = VAOksc;
040100150623         §RMctr = VAOctr;
040200150623       //?Filiale Ritiro/Consegna
040300150623         §RMpor = VAOpor;
040400150623         §RMpoc = VAOpoc;
040500150623       //?Note
040600150623         wtxt = VAOno1;
040700150623         exsr Bonifica;
040800150623         §RMno1 = wtxt;
040900150623         wtxt = VAOno2;
041000150623         exsr Bonifica;
041100150623         §RMno2 = wtxt;
041200150623       //?Flag DDT
041300150623         §RMddt = VAOddt;
041400150623       //?Riferimento
041500150623         wtxt = VAOrfa;
041600150623         exsr Bonifica;
041700150623         §RMrfa = wtxt;
041800150623       //?Priorità
041900150623         §RMsto = VAOsto;
042000150623       //?Sponda idraulica
042100150623         §RMspi = VAOspi;
042200150623       //?Flag Operativi
042300150623         §RMflo = VAOflo;
042400150623       //?Controllo che i dati di §RMflo siano giusti
042500150623         dORM01 = §RMflo;
042600150623         IF  §ORMcb <> 'S';
042700150623           clear §ORMcb;
042800150623         ENDIF;
042900150623         IF  §ORMfr <> 'S';
043000150623           clear §ORMfr;
043100150623         ENDIF;
043200150623         IF  §ORMks <> 'S';
043300150623           clear §ORMks;
043400150623         ENDIF;
043500150623         IF  §ORMfd <> 'S';
043600150623           clear §ORMfd;
043700150623         ENDIF;
043800150623         IF  §ORcom <> 'S';
043900150623           clear §ORcom;
044000150623         ENDIF;
044100150623         IF  §ORcomc <> 'S' and §ORcomc <> 'N';
044200150623           clear §ORcomc;
044300150623         ENDIF;
044400150623         IF  §ORsrm <> 'S';
044500150623           clear §ORsrm;
044600150623         ENDIF;
044700150623         §RMflo = dORM01;
044800150623
044900150623       //?Verifico se esiste il rcd 'G' su VAOE
045000150623       //?se esite controllo se OK il tipo servizio
045100150623       //?se NON è OK non devo memorizzarlo
045200150623         exsr VaoRcdG;
045300150623
045400150623       //?Richiamo FIOR05R per scrittura ORM
045500150623         clear kpjbu;
045600150623         fior05r (kpjba:FIOR05DS);
045700150623
045800150623       //?se torna errore flaggo il rcd come già elaborato automaticamente
045900150623         IF  §RMerr <> *blank;
046000150713           dORM01 = §RMflo;
046100150713           §ORMauto = 'X';
046200150714           VAOflo = dORM01;
046300150623           update FNVAO000;
046400150623           leavesr;
046500150623         ENDIF;
046600150623
046700150623       //?se OK cancello le estensioni e aggiorno TIORI
046800150623         exsr delVAOE;
046900150623         exsr aggTIORI;
047000150623         delete FNVAO000;
047100150623
047200150623       ENDSR;
047300150623
047400150623       //--------------------------------------------------------------
047500150623       //?Controllo caratteri.
047600150623       //--------------------------------------------------------------
047700150623       BEGSR  Bonifica;
047800150623
047900150623         clear TxtInOut;
048000150623         TxtInOut  = wtxt;
048100150623         UpperCase = '1';
048200150623         ChkNull   = '1';
048300150623
048400150623         xchkchar ( TxtInOut :
048500150623                    ElencoChar :
048600150623                    TipoElenco :
048700150623                    CharSost :
048800150623                    UpperCase :
048900150623                    ChkNull :
049000150623                    CharNull :
049100150623                    Esito );
049200150623
049300150623         // -?Modifica effettuata?
049400150623         IF  Esito = '1';
049500150623           wtxt = TxtInOut;
049600150623         ENDIF;
049700150623
049800150623       ENDSR;
049900150623
050000150623       //--------------------------------------------------------------------
050100150623       //?Controllo Tipo Servizio.
050200150623       //--------------------------------------------------------------------
050300150623       BEGSR VaoRcdG;
050400150623
050500150623         clear dOREGEN;
050600150623
050700150623       //?Imposto la chiave per il FNVAOE rcd 'G '
050800150623         k_VAOecor = VAOcor;
050900150623         k_VAOepoe = VAOpoe;
051000150623         k_VAOensr = VAOnsr;
051100150623         k_VAOenor = VAOnor;
051200150623         k_VAOenrv = VAOnrv;
051300150623         k_VAOetrc = 'G ';
051400150623         chain  %kds(k06fnvaoe) FNVAOE1L;
051500150623         IF  not %found(FNVAOE1L);
051600150623           leavesr;
051700150623         ENDIF;
051800150623         dOREGEN = VAOEdati;
051900150623         IF  §OREtsp <> *blanks;
052000150623       //?Imposto la chiave per TABELLA '5E'
052100150623           k_TBLkut = 1;
052200150623           k_TBLcod = '5E';
052300150623           k_TBLkey = §OREtsp;
052400150623           chain %kds(K03tabel) TABEL00F;
052500150623         //?Non esiste o annullato pulisco il campo
052600150623           IF  not %found(TABEL00F) or TBLflg <> *blanks;
052700150623             clear §OREtsp;
052800150623           ENDIF;
052900150623           VAOEdati = dOREGEN;
053000150623         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
053100150623           IF  VAOEdati = *blanks;
053200150623             delete FNVAOE00;
053300150623           ELSE;
053400150623             update FNVAOE00;
053500150623           ENDIF;
053600150623         ELSE;
053700150623           unlock FNVAOE1L;
053800150623         ENDIF;
053900150623
054000150623       ENDSR;
054100150623
054200150623       //--------------------------------------------------------------------
054300150623       //?Delete l'estensione del VAO.
054400150623       //--------------------------------------------------------------------
054500150623       BEGSR delVAOE;
054600150623
054700150623       //?Imposto la chiave per il FNVAOE
054800150623         k_VAOecor = VAOcor;
054900150623         k_VAOepoe = VAOpoe;
055000150623         k_VAOensr = VAOnsr;
055100150623         k_VAOenor = VAOnor;
055200150623         k_VAOenrv = VAOnrv;
055300150623       //?Cancello i rcd
055400150623         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
055500150623         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
055600150623         DOW not %eof(FNVAOE1L);
055700150623           delete fnvaoe00;
055800150623           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
055900150623         ENDDO;
056000150623
056100150623       ENDSR;
056200150623
056300150623       //--------------------------------------------------------------------
056400150623       //?Aggiorna TIORI00F.
056500150623       //--------------------------------------------------------------------
056600150623       BEGSR aggTIORI;
056700150623
056800150623       //?Il campo Progressivo di TIORI deve essere numerico
056900150928         // -?Se tutto vuoto: scarto?
057000150928         if  §ORMpg = *blank;
057100150928           leavesr;
057200150928         endif;
057300150928         // -?Se con caratteri alfabetici (non blank): scarto?
057400150623         xx = 1;
057500150623         FOR xx by 1 to %len(§ORMpg);
057600150623           IF  %subst(§ORMpg:xx:1) <> *blanks and
057700150623               %check(Digits:%subst(§ORMpg:xx:1)) > *zeros;
057800150623             leavesr;
057900150623           ENDIF;
058000150623         ENDFOR;
058100150623
058200150623       //?Aggiorno TIORI con il n. ORM
058300150623         ORIprg = %dec(§ORMpg:7:0);
058400150623         chain (ORIprg) TIORI01L;
058500150623         IF  %found(TIORI01L);
058600150623           ORIpoe = §RMpoeo;
058700150623           ORInsr = §RMnsro;
058800150623           ORInor = §RMnoro;
058900150623           ORInrv = §RMnrvo;
059000150623           update TIORI000;
059100150623         ENDIF;
059200150623
059300150623       ENDSR;
059400150622
059500150622       //--------------------------------------------------------------
059600150622       //?Operazioni finali.
059700150622       //--------------------------------------------------------------
059800150622       BEGSR RoutEnd;
059900150622
060000150622         *inLR = *on;
060100150622         return;
060200150622
060300150622       ENDSR;
