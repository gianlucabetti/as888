000100150622      //--------------------------------------------------------------
000200150622      //?FIOR08R - CONFERMA AUTOMATICA ORM DA INTERNET/FILE
000300150622      //--------------------------------------------------------------
000400150928
000500150928       //--------------------------------------------------------------
000600150928       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150928       //--------------------------------------------------------------
000800150928
000900150928     /*CMD addlible lib(GAITRAGRPS) position(*last)
001000150928     /*PRM dbgview(*source)
001100150928     /*END rmvlible lib(GAITRAGRPS)
001200150928     /*END
001300150928
001400150928       //--------------------------------------------------------------
001500150928       //?Specifiche di controllo.                                     ?
001600150928       //--------------------------------------------------------------
001700150622
001800150622     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900150623     h dftactgrp(*no) actgrp(*caller)
002000150622
002100150622      //---------------------------------------------------------------
002200150622      //?Dichiarazione file.
002300150622      //---------------------------------------------------------------
002400150623      // - Anagrafica Clienti Ritiro
002500150623     fFNACR01L  if   e           k disk
002600150623      // - ORM DA VAS
002700150622     fFNVAO00F  uf   e             disk
002800150623     fFNVAOE1L  uf   e           k disk
002900150622     fTIORI01L  uf   e           k disk    usropn
003000150623
003100150623      // - TABELLE
003200150623     fTABEL00F  if   e           k disk
003300150713     fTNTBE01L  if   e           k disk
003400150622
003500150622      //---------------------------------------------------------------
003600150622      //?Definizione costanti.
003700150622      //---------------------------------------------------------------
003800150623     d CmdOvrDbF       c                   const('OVRDBF +
003900150623     d                                           file(TIORI01L) +
004000150623     d                                           tofile(')
004100150623
004200150623     d Digits          c                   const('0123456789')
004300150622
004400150622      //---------------------------------------------------------------
004500150622      //?Definizione schiere.
004600150622      //---------------------------------------------------------------
004700150713     d skFIL           s              3  0 dim(64)
004800150713     d skTCO           s              1    dim(64)
004900150622
005000150622      //---------------------------------------------------------------
005100150622      //?Definizione aree dati.
005200150622      //---------------------------------------------------------------
005300150622
005400150622      //---------------------------------------------------------------
005500150622      //?Definizione strutture dati.
005600150622      //---------------------------------------------------------------
005700150622      // - Parametri ricevuti
005800150622     d KPJBA         e ds
005900150622     d Chiudi          s              1
006000150622     d Sistema         s             15
006100150622
006200150622      // - Gestione ORM
006300150623     d FIOR05DS      e ds                  inz
006400150623
006500150623      // - Rcd "G" file FNVAOE0F
006600150623     d dOREGEN       e ds                  inz
006700150623
006800150623      // - Flag Operativi
006900150623     d dORM01        e ds                  inz
007000150713
007100150713      // - Campo TBEuni
007200150713     d wTBEuni         ds           256
007300150713     d  SKfiltco               1    256    dim(64)
007400150622
007500150622      //---------------------------------------------------------------
007600150622      //?Definizione variabili globali.
007700150622      //---------------------------------------------------------------
007800150622      // - Flags booleani
007900150622     d Fine            s               n   inz(*off)
008000150713     d RecOK           s               n   inz(*off)
008100150622     d wEnd            s               n   inz(*off)
008200150623
008300150623      // - Indici di schiera
008400150623     d xx              s              4s 0 inz
008500150622
008600150622      // - Campi di comodo
008700150623     d cmd             s            512a   varying
008800150623     d wnrr            s              9s 0
008900150713     d wVAOpoe         s                   like(VAOpoe)
009000150713     d wVAOtco         s                   like(VAOtco)
009100150623     d wtxt            S           2048
009200150622
009300150622      //---------------------------------------------------------------
009400150622      //?Definizione procedure usate.
009500150622      //---------------------------------------------------------------
009600150622       // - Gestione ORM
009700150622     d FIOR05R         pr                  extpgm('FIOR05R')
009800150622     d  kpjba                              likeds(kpjba)
009900150622     d  fior05ds                           likeds(fior05ds)
010000150622
010100150622      //---------------------------------------------------------------
010200150622      //?Definizione Prototipi.
010300150622      //---------------------------------------------------------------
010400150623       // - "Bonifica" stringa da caratteri indesiderati?
010500150623      /copy gaitrasrc/srcProtoPI,XCHKCHAR
010600150623      /copy gaitrasrc/srcProtoPR,XCHKCHAR
010700150623
010800150622       // - Esecuzione comando
010900150622      /copy gaitrasrc/srcprotopr,SYSTEM
011000150622
011100150622      //---------------------------------------------------------------
011200150622      //?Definizione key-list.
011300150622      //---------------------------------------------------------------
011400150623       // - File FNVAOE1L
011500150623     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
011600150623     d                                     prefix(k_)
011700150623     d                                     inz
011800150623
011900150623       // - File TABEL00F
012000150623     d k03tabel      e ds                  extname(TABEL00F:*key)
012100150623     d                                     prefix(k_)
012200150623     d                                     inz
012300150622
012400150622      //---------------------------------------------------------------
012500150622      //?M A I N - L I N E
012600150622      //---------------------------------------------------------------
012700150622
012800150622     c     *Entry        plist
012900150622     c                   parm                    Chiudi
013000150622     c                   parm                    Sistema
013100150622
013200150622      /free
013300150622
013400150622       //?Operazioni iniziali
013500150622       exsr RoutInz;
013600150622
013700150622       //?Elabora
013800150622       exsr Elabora;
013900150622
014000150622       //?Operazioni finali
014100150622       exsr RoutEnd;
014200150622
014300150622       //--------------------------------------------------------------
014400150622       //?Operazioni iniziali.
014500150622       //--------------------------------------------------------------
014600150622       BEGSR RoutInz;
014700150622
014800150622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014900150622
015000150622       //?Se chiusura del sistema devo chiudere il lavoro
015100150622         IF  %shtdn;
015200150622           Chiudi = 'S';
015300150622           exsr RoutEnd;
015400150622         ENDIF;
015500150622
015600150622       //?Apro il file TIORI
015700150623         IF  not %open(TIORI01L);
015800150623           cmd = CmdOvrDbF;
015900150623           IF  %subst(Sistema:1:5) = 'AS888';
016000150623             cmd = %trim(cmd) + 'GAITRAGRPS/TIORI01L)';
016100150623           ELSE;
016200150623             cmd = %trim(cmd) + 'GAITRAGRU/TIORI01L)';
016300150623           ENDIF;
016400150623           ExecuteCommand (cmd);
016500150623           open TIORI01L;
016600150622         ENDIF;
016700150713
016800150713       //?Carico in sk le filiali emissioni attive alla procedura
016900150713       //?con il relativo tipo comunicazione ORM da confermare
017000150713         TBEcod = 'VPO';
017100150713         TBEke1 = 'ORMVAS';
017200150713         chain (TBEcod:TBEke1) TNTBE01L;
017300150713         IF  not %found(TNTBE01L);
017400150713           clear SKfiltco;
017500150713           leavesr;
017600150713         ENDIF;
017700150713         wTBEuni = TBEuni;
017800150713         xx = 1;
017900150713         FOR xx by 1 to %elem(SKfiltco);
018000150713           IF  SKfiltco(xx) <> *zeros and SKfiltco(xx) <> *blanks;
018100150713             skFIL(xx) = %dec(%subst(SKfiltco(xx):1:3):3:0);
018200150713             skTCO(xx) = %subst(SKfiltco(xx):4:1);
018300150713           ENDIF;
018400150713         ENDFOR;
018500150622
018600150622       ENDSR;
018700150622
018800150622       //--------------------------------------------------------------
018900150622       //?Elabora.
019000150622       //--------------------------------------------------------------
019100150622       BEGSR Elabora;
019200150623
019300150623         wEnd = *off;
019400150623
019500150623       //?Leggo FNVAO
019600150623       //?non annullati
019700150623       //?solo quelli ancora da elaborare
019800150623       //?no prepagati
019900150623         exec sql
020000150623         DECLARE vao CURSOR for
020100150713         SELECT rrn(FNVAO00F), VAOpoe, VAOtco from FNVAO00F
020200150623         WHERE VAOatb = ' ' and substr(VAOflo, 11, 1) = ' ' and
020300150623               VAOtor <> 'P';
020400150623
020500150623       //?Apertura del cursore
020600150623         exec sql
020700150623         OPEN vao;
020800150623
020900150623         IF sqlcode < 0;
021000150623           wEnd = *on;
021100150623           exec sql
021200150623           CLOSE vao;
021300150623           leavesr;
021400150623         ENDIF;
021500150623
021600150623         DOW  not wEnd;
021700150623           exec sql
021800150713           FETCH NEXT from vao into :wnrr, :wVAOpoe, :wVAOtco;
021900150623           IF sqlcod = 100 or sqlcod < 0;
022000150623             wEnd = *on;
022100150623             leave;
022200150623           ENDIF;
022300150713
022400150714         //?Controlla se OK filiale + tipo comunicazione ORM
022500150713           IF  skFIL(01) <> 999 and skFIL(01) > 0;
022600150713             exsr ContrRec;
022700150713           ENDIF;
022800150714         //?Se il primo rcd della tabella è 999 solo il tipo comunicazione impostato
022900150714           IF  skFIL(01) = 999 and skTCO(01) <> *blanks;
023000150714             exsr ContrRec;
023100150714           ENDIF;
023200150714         //?Se il primo rcd della tabella è 999 e non c'è il tipo comunicazione faccio tutto
023300150714           IF  skFIL(01) = 999 and skTCO(01) = *blanks;
023400150714             RecOK = *on;
023500150714           ENDIF;
023600150713           IF  not RecOK;
023700150713             iter;
023800150713           ENDIF;
023900150623
024000150623         //?Richiamo pgm conferma ORM
024100150623           exsr Conferma;
024200150623
024300150623         ENDDO;
024400150623
024500150623       //?Chiusura del cursore
024600150623         exec sql
024700150623         CLOSE vao;
024800150622
024900150622       ENDSR;
025000150713
025100150713       //--------------------------------------------------------------
025200150713       //?Controllo se record OK.
025300150713       //--------------------------------------------------------------
025400150713       BEGSR ContrRec;
025500150713
025600150713         RecOK = *off;
025700150713
025800150714         IF  skFIL(01) <> 999;
025900150714           clear xx;
026000150714           IF  wVAOpoe = *zeros;
026100150714             leavesr;
026200150714           ENDIF;
026300150714           xx = %lookup(wVAOpoe:skFIL);
026400150714           IF  xx = 0;
026500150714             leavesr;
026600150714           ENDIF;
026700150713
026800150714           IF  wVAOtco <> skTCO(xx) and skTCO(xx) <> *blanks;
026900150714             leavesr;
027000150714           ENDIF;
027100150714         ELSE;
027200150714           IF  wVAOtco <> skTCO(01);
027300150714             leavesr;
027400150714           ENDIF;
027500150714         ENDIF;
027600150713
027700150713         RecOK = *on;
027800150713
027900150713       ENDSR;
028000150623
028100150623       //--------------------------------------------------------------
028200150623       //?Confermo ORM.
028300150623       //--------------------------------------------------------------
028400150623       BEGSR Conferma;
028500150623
028600150623       //?Come prima cosa aggancio il rcd del VAO
028700150623         chain(e) wnrr FNVAO00F;
028800150623       //?se allocato vado via
028900150623         IF  %error;
029000150623           leavesr;
029100150623         ENDIF;
029200150623       //?se non trovo vado via
029300150623         IF  not %found(FNVAO00F);
029400150623           leavesr;
029500150623         ENDIF;
029600150623
029700150623         clear FIOR05DS;
029800150623       //?Conferma senza emissione video
029900150623         §RMtla = '0';
030000150623       //?Conferma da Internet/File Automatica (nel chiamato poi diventa 'C')
030100150623         §RMfpr = 'K';
030200150623       //?Numero ORM
030300150623         §RMpoe = VAOpoe;
030400150623         §RMnsr = VAOnsr;
030500150623         §RMnor = VAOnor;
030600150623         §RMnrv = VAOnrv;
030700150623       //?Tipo ORM
030800150623         §RMtor = VAOtor;
030900150623       //?Tipo Comunicazion ORM
031000150623         §RMtco = VAOtco;
031100150623       //?Data/Ora immissione
031200150623         §RMdao = VAOdao;
031300150623         §RMoao = VAOoao;
031400150623       //?Ordinante
031500150623         §RMcor = VAOcor;
031600150623         wtxt = VAOrso;
031700150623         exsr Bonifica;
031800150623         §RMrso = wtxt;
031900150623         wtxt = VAOino;
032000150623         exsr Bonifica;
032100150623         §RMino = wtxt;
032200150623         wtxt = VAOcao;
032300150623         exsr Bonifica;
032400150623         §RMcao = wtxt;
032500150623         wtxt = VAOloo;
032600150623         exsr Bonifica;
032700150623         §RMloo = wtxt;
032800150623         §RMpro = VAOpro;
032900150623         §RMnao = VAOnao;
033000150623       //?Mittente
033100150623         §RMcra = VAOcra;
033200150623         wtxt = VAOrsr;
033300150623         exsr Bonifica;
033400150623         §RMrsr = wtxt;
033500150623         wtxt = VAOinr;
033600150623         exsr Bonifica;
033700150623         §RMinr = wtxt;
033800150623         wtxt = VAOcar;
033900150623         exsr Bonifica;
034000150623         §RMcar = wtxt;
034100150623         wtxt = VAOlor;
034200150623         exsr Bonifica;
034300150623         §RMlor = wtxt;
034400150623         §RMprr = VAOprr;
034500150623         §RMnar = VAOnar;
034600150623       //?Data/Ora Ritiro
034700150623         §RMdar = VAOdar;
034800150623         §RMorr = VAOorr;
034900150623       //?Dati mittente da anagrafica clienti ritiro
035000150623         IF  VAOcra > *zeros;
035100150623           chain (VAOcra) FNACR01L;
035200150623           IF  %found(FNACR01L);
035300150623             §RMrsr = ACRrsr;
035400150623             §RMinr = ACRinr;
035500150623             §RMcar = ACRcar;
035600150623             §RMlor = ACRlor;
035700150623             §RMprr = ACRprr;
035800150623             §RMnar = ACRnar;
035900150623             IF  §RMorr = 0;
036000150623               §RMorr = ACRorr;
036100150623             ENDIF;
036200150623           ENDIF;
036300150623         ENDIF;
036400150623       //?Referente/Telefono
036500150623         wtxt = VAOrer;
036600150623         exsr Bonifica;
036700150623         §RMrer = wtxt;
036800150623         §RMter = VAOter;
036900150623       //?Destinatario
037000150623         §RMcrc = VAOcrc;
037100150623         wtxt = VAOrsc;
037200150623         exsr Bonifica;
037300150623         §RMrsc = wtxt;
037400150623         wtxt = VAOinc;
037500150623         exsr Bonifica;
037600150623         §RMinc = wtxt;
037700150623         wtxt = VAOcac;
037800150623         exsr Bonifica;
037900150623         §RMcac = wtxt;
038000150623         wtxt = VAOloc;
038100150623         exsr Bonifica;
038200150623         §RMloc = wtxt;
038300150623         §RMprc = VAOprc;
038400150623         §RMnac = VAOnac;
038500150623       //?Fermo Deposito
038600150623         §RMffd = VAOffd;
038700150623         IF  VAOffd = 'N';
038800150623           clear §RMffd;
038900150623         ENDIF;
039000150623       //?Mattino/Pomeriggio
039100150623         §RMrmp = VAOrmp;
039200150623       //?Natura merce
039300150623         wtxt = VAOnam;
039400150623         exsr Bonifica;
039500150623         §RMnam = wtxt;
039600150623       //?Quantità
039700150623         §RMncl = VAOncl;
039800150623         §RMpkg = VAOpkg;
039900150623         §RMvlm = VAOvlm;
040000150623         §RMbnc = VAObnc;
040100150623         §RMblc = VAOblc;
040200150623         §RMatt = VAOatt;
040300150623         §RMmtc = VAOmtc;
040400150623       //?Chi paga
040500150623         §RMpag = VAOpag;
040600150623         §RMksc = VAOksc;
040700150623         §RMctr = VAOctr;
040800150623       //?Filiale Ritiro/Consegna
040900150623         §RMpor = VAOpor;
041000150623         §RMpoc = VAOpoc;
041100150623       //?Note
041200150623         wtxt = VAOno1;
041300150623         exsr Bonifica;
041400150623         §RMno1 = wtxt;
041500150623         wtxt = VAOno2;
041600150623         exsr Bonifica;
041700150623         §RMno2 = wtxt;
041800150623       //?Flag DDT
041900150623         §RMddt = VAOddt;
042000150623       //?Riferimento
042100150623         wtxt = VAOrfa;
042200150623         exsr Bonifica;
042300150623         §RMrfa = wtxt;
042400150623       //?Priorità
042500150623         §RMsto = VAOsto;
042600150623       //?Sponda idraulica
042700150623         §RMspi = VAOspi;
042800150623       //?Flag Operativi
042900150623         §RMflo = VAOflo;
043000150623       //?Controllo che i dati di §RMflo siano giusti
043100150623         dORM01 = §RMflo;
043200150623         IF  §ORMcb <> 'S';
043300150623           clear §ORMcb;
043400150623         ENDIF;
043500150623         IF  §ORMfr <> 'S';
043600150623           clear §ORMfr;
043700150623         ENDIF;
043800150623         IF  §ORMks <> 'S';
043900150623           clear §ORMks;
044000150623         ENDIF;
044100150623         IF  §ORMfd <> 'S';
044200150623           clear §ORMfd;
044300150623         ENDIF;
044400150623         IF  §ORcom <> 'S';
044500150623           clear §ORcom;
044600150623         ENDIF;
044700150623         IF  §ORcomc <> 'S' and §ORcomc <> 'N';
044800150623           clear §ORcomc;
044900150623         ENDIF;
045000150623         IF  §ORsrm <> 'S';
045100150623           clear §ORsrm;
045200150623         ENDIF;
045300150623         §RMflo = dORM01;
045400150623
045500150623       //?Verifico se esiste il rcd 'G' su VAOE
045600150623       //?se esite controllo se OK il tipo servizio
045700150623       //?se NON è OK non devo memorizzarlo
045800150623         exsr VaoRcdG;
045900150623
046000150623       //?Richiamo FIOR05R per scrittura ORM
046100150623         clear kpjbu;
046200150623         fior05r (kpjba:FIOR05DS);
046300150623
046400150623       //?se torna errore flaggo il rcd come già elaborato automaticamente
046500150623         IF  §RMerr <> *blank;
046600150713           dORM01 = §RMflo;
046700150713           §ORMauto = 'X';
046800150714           VAOflo = dORM01;
046900150623           update FNVAO000;
047000150623           leavesr;
047100150623         ENDIF;
047200150623
047300150623       //?se OK cancello le estensioni e aggiorno TIORI
047400150623         exsr delVAOE;
047500150623         exsr aggTIORI;
047600150623         delete FNVAO000;
047700150623
047800150623       ENDSR;
047900150623
048000150623       //--------------------------------------------------------------
048100150623       //?Controllo caratteri.
048200150623       //--------------------------------------------------------------
048300150623       BEGSR  Bonifica;
048400150623
048500150623         clear TxtInOut;
048600150623         TxtInOut  = wtxt;
048700150623         UpperCase = '1';
048800150623         ChkNull   = '1';
048900150623
049000150623         xchkchar ( TxtInOut :
049100150623                    ElencoChar :
049200150623                    TipoElenco :
049300150623                    CharSost :
049400150623                    UpperCase :
049500150623                    ChkNull :
049600150623                    CharNull :
049700150623                    Esito );
049800150623
049900150623         // -?Modifica effettuata?
050000150623         IF  Esito = '1';
050100150623           wtxt = TxtInOut;
050200150623         ENDIF;
050300150623
050400150623       ENDSR;
050500150623
050600150623       //--------------------------------------------------------------------
050700150623       //?Controllo Tipo Servizio.
050800150623       //--------------------------------------------------------------------
050900150623       BEGSR VaoRcdG;
051000150623
051100150623         clear dOREGEN;
051200150623
051300150623       //?Imposto la chiave per il FNVAOE rcd 'G '
051400150623         k_VAOecor = VAOcor;
051500150623         k_VAOepoe = VAOpoe;
051600150623         k_VAOensr = VAOnsr;
051700150623         k_VAOenor = VAOnor;
051800150623         k_VAOenrv = VAOnrv;
051900150623         k_VAOetrc = 'G ';
052000150623         chain  %kds(k06fnvaoe) FNVAOE1L;
052100150623         IF  not %found(FNVAOE1L);
052200150623           leavesr;
052300150623         ENDIF;
052400150623         dOREGEN = VAOEdati;
052500150623         IF  §OREtsp <> *blanks;
052600150623       //?Imposto la chiave per TABELLA '5E'
052700150623           k_TBLkut = 1;
052800150623           k_TBLcod = '5E';
052900150623           k_TBLkey = §OREtsp;
053000150623           chain %kds(K03tabel) TABEL00F;
053100150623         //?Non esiste o annullato pulisco il campo
053200150623           IF  not %found(TABEL00F) or TBLflg <> *blanks;
053300150623             clear §OREtsp;
053400150623           ENDIF;
053500150623           VAOEdati = dOREGEN;
053600150623         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
053700150623           IF  VAOEdati = *blanks;
053800150623             delete FNVAOE00;
053900150623           ELSE;
054000150623             update FNVAOE00;
054100150623           ENDIF;
054200150623         ELSE;
054300150623           unlock FNVAOE1L;
054400150623         ENDIF;
054500150623
054600150623       ENDSR;
054700150623
054800150623       //--------------------------------------------------------------------
054900150623       //?Delete l'estensione del VAO.
055000150623       //--------------------------------------------------------------------
055100150623       BEGSR delVAOE;
055200150623
055300150623       //?Imposto la chiave per il FNVAOE
055400150623         k_VAOecor = VAOcor;
055500150623         k_VAOepoe = VAOpoe;
055600150623         k_VAOensr = VAOnsr;
055700150623         k_VAOenor = VAOnor;
055800150623         k_VAOenrv = VAOnrv;
055900150623       //?Cancello i rcd
056000150623         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
056100150623         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
056200150623         DOW not %eof(FNVAOE1L);
056300150623           delete fnvaoe00;
056400150623           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
056500150623         ENDDO;
056600150623
056700150623       ENDSR;
056800150623
056900150623       //--------------------------------------------------------------------
057000150623       //?Aggiorna TIORI00F.
057100150623       //--------------------------------------------------------------------
057200150623       BEGSR aggTIORI;
057300150623
057400150623       //?Il campo Progressivo di TIORI deve essere numerico
057500150928         // -?Se tutto vuoto: scarto?
057600150928         if  §ORMpg = *blank;
057700150928           leavesr;
057800150928         endif;
057900150928         // -?Se con caratteri alfabetici (non blank): scarto?
058000150623         xx = 1;
058100150623         FOR xx by 1 to %len(§ORMpg);
058200150623           IF  %subst(§ORMpg:xx:1) <> *blanks and
058300150623               %check(Digits:%subst(§ORMpg:xx:1)) > *zeros;
058400150623             leavesr;
058500150623           ENDIF;
058600150623         ENDFOR;
058700150623
058800150623       //?Aggiorno TIORI con il n. ORM
058900150623         ORIprg = %dec(§ORMpg:7:0);
059000150623         chain (ORIprg) TIORI01L;
059100150623         IF  %found(TIORI01L);
059200150623           ORIpoe = §RMpoeo;
059300150623           ORInsr = §RMnsro;
059400150623           ORInor = §RMnoro;
059500150623           ORInrv = §RMnrvo;
059600150623           update TIORI000;
059700150623         ENDIF;
059800150623
059900150623       ENDSR;
060000150622
060100150622       //--------------------------------------------------------------
060200150622       //?Operazioni finali.
060300150622       //--------------------------------------------------------------
060400150622       BEGSR RoutEnd;
060500150622
060600150622         *inLR = *on;
060700150622         return;
060800150622
060900150622       ENDSR;
