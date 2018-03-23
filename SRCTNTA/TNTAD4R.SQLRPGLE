000100141003      //--------------------------------------------------------------
000200141003      //?TNTAD4R - Controllo anagrafiche clienti incomplete
000300141003      //--------------------------------------------------------------
000400141003
000500141003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141007     h dftactgrp(*no) actgrp(*caller)
000700141003
000800141003      //---------------------------------------------------------------
000900141003      //?Dichiarazione file.
001000141003      //---------------------------------------------------------------
001100141006      // - File Anagrafica Commerciali - Rubrica
001200141006     fAZNTC01L  if   e           k disk
001300141007
001400141007      // - Printer file per invio mail
001500141007     fPRTEMAIL  o    f  198        printer oflind(*inof) usropn
001600141003
001700141003      //---------------------------------------------------------------
001800141003      //?Definizione costanti.
001900141003      //---------------------------------------------------------------
002000141007      // - Comando di override al PrtF
002100141007     d CmdOvrPrtF      c                   const('OVRPRTF +
002200141007     d                                           file(PRTEMAIL) +
002300141007     d                                           pagesize(66 198) +
002400141007     d                                           lpi(6) cpi(10) +
002500141007     d                                           ovrscope(*actgrpdfn) +
002600141007     d                                           ')
002700141007     d CmdDltOvr       c                   const('DLTOVR +
002800141007     d                                            file(PRTEMAIL) +
002900141007     d                                            lvl(*actgrpdfn)')
003000141007
003100141007     d Dominio         c                   const('@brt.it')
003200141007
003300141007     d Sede            c                   const('046')
003400141006
003500141003      //---------------------------------------------------------------
003600141003      //?Definizione schiere.
003700141003      //---------------------------------------------------------------
003800141006      // - Agenti
003900141007     d sk_Age          s              7  0 dim(50) inz
004000141003
004100141003      //---------------------------------------------------------------
004200141003      //?Definizione aree dati.
004300141003      //---------------------------------------------------------------
004400141003      // - Dati utente
004500141003     d §AzUte        e ds                  extname(AZUTE00F)
004600141003     d                                     dtaara
004700141003     d §DatiUte      e ds                  extname(dDatiUte)
004800141003     d                                     dtaara
004900141003
005000141003      //---------------------------------------------------------------
005100141003      //?Definizione strutture dati.
005200141003      //---------------------------------------------------------------
005300141003
005400141003      // - Parametri ricevuti
005500141003     d KPJBA         e ds
005600141007
005700141007      // - Gestione tabelle
005800141007     d TIBS02ds      e ds                  inz
005900141007     d  T02tla       e                     inz('L')
006000141007     d  T02mod       e                     inz('C')
006100141007     d  T02cod       e                     inz('MRA')
006200141003
006300141003      // - Reperimento dati utente
006400141003     d TIBS34DS      e ds                  inz qualified
006500141003
006600141003      // - Recupera dati cliente
006700141003     d TIBS69ds      e ds                  inz
006800141003     d   I69tla      e                     inz('C')
006900141007
007000141007      // - Parametri x Ridefinizione dati utente estesi per mailing
007100141007     d TRTCM1ds      e ds                  inz
007200141007     d   §CM1mitt    e                     inz('ced@brt.it')
007300141007     d   §CM1dst     e                     inz('anotnella.gallucci@brt.it')
007400141007     d   §CM1tips    e                     inz('COI')
007500141007     d   §CM1po      e                     inz('046')
007600141007     d   §CM1var     e                     inz('*OBJM*+
007700141007     d                                     Anagrafica Clienti incompleta')
007800141007     d   §CM1sts     e                     inz(*off)
007900141007     d   §CM1idp     e                     inz('2')
008000141007
008100141007      // File Organigramma
008200141007     d AZORG00F      e ds                  extname(AZORG00F)
008300141006
008400141006      // File Anagrafiche
008500141006     d CNACO00F      e ds                  extname(CNACO00F)
008600141006     d CNCLP00F      e ds                  extname(CNCLP00F)
008700141007
008800141007      // - Tabella "MRA" = Bart-Mailing danni
008900141007     d dMRAdan       e ds                  inz
009000141007
009100141007      // - Campo ORGDE0 file AZORG00F
009200141007     d OG140         e ds                  inz
009300141006
009400141006      // - Ds multipla per invio mail al commerciale
009500141007     d Anagrafica      ds                  QUALIFIED OCCURS(100)
009600141006     d                                     inz
009700141006     d  ksc                                like(CLPksc)
009800141006     d  rag                                like(ACOrag)
009900141006     d  age                                like(CLPage)
010000141007     d  apo                                like(§OGapo)
010100141007
010200141007      // - Status
010300141007     d Psds           sds
010400141007     d   SDSpgm          *proc
010500141007     d   JobUser             254    263
010600141003
010700141003      //---------------------------------------------------------------
010800141003      //?Definizione variabili globali.
010900141003      //---------------------------------------------------------------
011000141003      // - Flags booleani
011100141003     d wEoF            s               n   inz(*off)
011200141003     d wFine           s               n   inz(*off)
011300141007
011400141007      // - Campi per QCMDEXC
011500141007     d cmd             s            512a   varying
011600141006
011700141006      // - Campi di comodo
011800141007     d testo           s            198    inz
011900141006     d indicex         s              5i 0 inz
012000141006     d indicey         s              5i 0 inz
012100141006     d mailcmm         s            255a   inz varying
012200141003
012300141003      //---------------------------------------------------------------
012400141003      //?Definizione procedure usate.
012500141003      //---------------------------------------------------------------
012600141003
012700141003      //---------------------------------------------------------------
012800141003      //?Definizione prototipi.
012900141003      //---------------------------------------------------------------
013000141007      /copy gaitrasrc/srcprotopr,SYSTEM
013100141007      /copy gaitrasrc/srcprotopr,TIBS02R
013200141003      /copy gaitrasrc/srcprotopr,TIBS34R
013300141003
013400141003      //---------------------------------------------------------------
013500141003      //?Definizione key-list.
013600141003      //---------------------------------------------------------------
013700141006       // - File AZNTC01L
013800141006     d k02azntc      e ds                  extname(AZNTC01L:*key)
013900141006     d                                     prefix(k_) inz
014000141003
014100141003      //---------------------------------------------------------------
014200141003      //?M A I N - L I N E
014300141003      //---------------------------------------------------------------
014400141003
014500141003     c     *entry        plist
014600141003     c                   parm                    KPJBA
014700141003
014800141003      /free
014900141003
015000141003       //?Operazioni iniziali
015100141003       exsr RoutInz;
015200141003
015300141003       //?Elaborazioni
015400141006       exsr Clienti_Parziali;
015500141006
015600141006       //?Manda mail ai commerciali
015700141007       IF  indicex > 0;
015800141007         exsr Invia_Mail;
015900141007       ENDIF;
016000141003
016100141003       //?Operazioni finali
016200141003       exsr RoutEnd;
016300141003
016400141003       //--------------------------------------------------------------
016500141003       //?Operazioni iniziali.
016600141003       //--------------------------------------------------------------
016700141003       BEGSR RoutInz;
016800141003
016900141003         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017000141003
017100141003       //?Reperimento dati job
017200141003         exsr DatiJob;
017300141006
017400141007       //?Recupero i dati da tabella MRA per invio mail
017500141007         reset TIBS02ds;
017600141007         T02ke1 = SDSpgm;
017700141007         TNTBE_RicercaControllo (kpjba:tibs02ds);
017800141007         IF  T02err = *blanks;
017900141007           dMRAdan = T02uni;
018000141007         ENDIF;
018100141003
018200141003       ENDSR;
018300141003
018400141003       //--------------------------------------------------------------
018500141003       //?Reperimento Dati del job.
018600141003       //--------------------------------------------------------------
018700141003       BEGSR DatiJob;
018800141003
018900141003         in(E) §AzUte;
019000141003         IF  not %error;
019100141003           in(E) §DatiUte;
019200141003         ENDIF;
019300141003         IF  %error or rsut = *blanks;
019400141003           clear TIBS34ds;
019500141003           tibs34r(tibs34ds);
019600141003           in §AzUte;
019700141003           in §DatiUte;
019800141003         ENDIF;
019900141003
020000141003       ENDSR;
020100141003
020200141003       //--------------------------------------------------------------
020300141006       //?Leggo i clienti con i dati incompleti.
020400141003       //--------------------------------------------------------------
020500141006       BEGSR Clienti_parziali;
020600141003
020700141003         wEoF = *off;
020800141006         clear indicex;
020900141003
021000141003         exec sql
021100141006         DECLARE Clienti cursor for
021200141007         WITH F1 AS
021300141007         (SELECT ORGfil, ORGde0 FROM AZORG00F WHERE substr(ORGde3, 11, 3)
021400141006         not in('LOG', 'XXX'))
021500141007         SELECT CLPksc, ACOrag, CLPage, ORGde0
021600141006         FROM F1, CNCLP00F, CNACO00F
021700141006         WHERE SUBSTR(digits(CLPksc), 1, 3) = digits(ORGfil) and
021800141006         CLPkcc = 151 and CLPksc = ACOksc and CLPscf = 0
021900141006         ORDER BY CLPksc;
022000141003
022100141003       //?Apertura del cursore
022200141003         exec sql
022300141006         OPEN Clienti;
022400141003
022500141003         IF  sqlcode < 0;
022600141003           wEoF = *on;
022700141003         ENDIF;
022800141003
022900141003         DOW  not wEoF;
023000141003           exec sql
023100141006           FETCH NEXT FROM Clienti into :CLPksc, :ACOrag,
023200141007                                        :CLPage, :ORGde0;
023300141003
023400141003           IF  sqlcod = 100 or sqlcod < 0;
023500141003             wEoF = *on;
023600141003             leave;
023700141003           ENDIF;
023800141003
023900141006         //?Carico i dati in sk
024000141006           exsr Carica_Schiera;
024100141003
024200141003         ENDDO;
024300141003
024400141003       //?Chiudo il cursore
024500141003         exec sql
024600141006         CLOSE Clienti;
024700141003
024800141003       ENDSR;
024900141003
025000141003       //--------------------------------------------------------------
025100141006       //?Carico i dati in schiera.
025200141003       //--------------------------------------------------------------
025300141006       BEGSR Carica_Schiera;
025400141006
025500141006         clear indicey;
025600141007
025700141007         OG140 = ORGde0;
025800141006
025900141006         indicex += 1;
026000141006         %occur(anagrafica) = indicex;
026100141006         anagrafica.ksc = CLPksc;
026200141006         anagrafica.rag = ACOrag;
026300141006         anagrafica.age = CLPage;
026400141007         anagrafica.apo = §OGapo;
026500141006
026600141006         IF  %lookup(CLPage:sk_Age) = 0;
026700141006           indicey = %lookup(*zeros:sk_Age);
026800141006           sk_Age(indicey) = CLPage;
026900141006         ENDIF;
027000141003
027100141003       ENDSR;
027200141003
027300141003       //--------------------------------------------------------------
027400141006       //?Mando la mail ai commerciali.
027500141003       //--------------------------------------------------------------
027600141006       BEGSR Invia_Mail;
027700141006
027800141006         clear indicex;
027900141006         clear indicey;
028000141007         clear mailcmm;
028100141006
028200141006       //?Sorto la sk dei commerciali
028300141006         sorta sk_Age;
028400141006
028500141006       //?Leggo la sk dei commerciali
028600141006         indicex = 1;
028700141006         FOR  indicex by 1 to %elem(sk_Age);
028800141007           IF  sk_Age(indicex) = 0;
028900141007             iter;
029000141007           ENDIF;
029100141007
029200141007         //?Aggancio l'anagrafica dei commerciali per recuperare
029300141006         //?l'indirizzo mail del destinario
029400141007           k_NTCtnt = '02';
029500141007           k_NTCcmm = sk_Age(indicex);
029600141007           chain %kds(k02azntc : 2) AZNTC01L;
029700141007           IF  %found(AZNTC01L);
029800141007             mailcmm = NTCrnt;
029900141007             mailcmm = %trim(mailcmm) + Dominio;
030000141007           ENDIF;
030100141006
030200141007       //?Leggo i dati caricati nella ds multipla
030300141007           FOR indicey = 1 TO %elem(anagrafica);
030400141007             %occur(anagrafica) = indicey;
030500141007
030600141007         //?Se non ancora fatto preparo la testata della mail
030700141007             IF  not %open(PRTEMAIL);
030800141007           //?aggiungo anche Poc di Area come destinatario
030900141007               mailcmm = %trim(mailcmm) + '; POC' + anagrafica.apo +
031000141007               Dominio;
031100141007           //?aggiungo anche Antonella come destinatario
031200141007               mailcmm = %trim(mailcmm) + '; antonella.gallucci@brt.it';
031300141007               exsr TestaMail;
031400141007             ENDIF;
031500141007
031600141007           //?Scrivo il corpo della mail
031700141007             IF  anagrafica.age = sk_Age(indicex) and
031800141007                 anagrafica.age > 0;
031900141007               exsr CorpoMail;
032000141007             ENDIF;
032100141007           ENDFOR;
032200141006
032300141007         //?mando la mail
032400141007           exsr FineMail;
032500141006
032600141006         ENDFOR;
032700141003
032800141003       ENDSR;
032900141007
033000141007       //--------------------------------------------------------------
033100141007       //?Testata mail.
033200141007       //--------------------------------------------------------------
033300141007       BEGSR TestaMail;
033400141007
033500141007       //?Override al file di stampa per impostarvi i dati per
033600141007       //?l'invio via e-mail
033700141007         exsr Override;
033800141007
033900141007         open PRTEMAIL;
034000141007
034100141007       //?Stampa una testata se NON è richiesta la e-mail
034200141007         IF  §MRAdreg =  *blank;
034300141007           testo = JobUser + ' - ' + SDSpgm
034400141007                 + ' - ' + %editc( *date : 'Y' )
034500141007                 + ' - *REM* ' + %subst(§CM1var : 7 : 70);
034600141007           except PrtDet;
034700141007           clear testo;
034800141007           except PrtDet;
034900141007           except PrtDet;
035000141007         ENDIF;
035100141007
035200141007       //?Stampa testo iniziale
035300141007         testo = 'Buongiorno,';
035400141007         except PrtDet;
035500141007         clear testo;
035600141007         except PrtDet;
035700141007         testo = 'Controllando le anagrafiche clienti abbiamo ' +
035800141007                 'notato che i seguenti codici sono incompleti, ';
035900141007         except PrtDet;
036000141007         testo = 'per esempio manca il codice intestazione '+
036100141007                 'fattura.';
036200141007         except PrtDet;
036300141007         clear testo;
036400141007         except PrtDet;
036500141007
036600141007       //?Stampa intestazione
036700141007         testo = 'Codice Cliente    Ragione Sociale ';
036800141007         except PrtDet;
036900141007         clear testo;
037000141007         except PrtDet;
037100141007
037200141007       ENDSR;
037300141007
037400141007       //--------------------------------------------------------------
037500141007       //?Override al file di stampa per impostarvi i dati per
037600141007       //?  l'invio via e-mail   +   stampa inizio e-mail
037700141007       //--------------------------------------------------------------
037800141007       BEGSR Override;
037900141007
038000141007         reset TRTCM1DS;
038100141007
038200141007         IF  §MRAdreg <> *blank;
038300141007           §CM1mitt = %trim(§MRAdmitt);
038400141007           //§CM1dst  = %trim(§MRAddest);
038500141007           §CM1dst  = %trim(mailcmm);
038600141007           §CM1tips = §MRAdreg;
038700141007           §CM1po   = Sede;
038800141007           §CM1var  = '*OBJM*' + §MRAddes;
038900141007           §CM1idp  = §MRAdidpro;
039000141007           cmd = CmdOvrPrtF
039100141007               + ' outq(' + %trim(§MRAdoutqi) + ')'
039200141007               + ' usrdfndta(''' + TRTCM1ds + ''')';
039300141007         ELSE;
039400141007           cmd = CmdOvrPrtF;
039500141007         ENDIF;
039600141007
039700141007         ExecuteCommand (cmd);
039800141007
039900141007       ENDSR;
040000141006
040100141006       //--------------------------------------------------------------
040200141007       //?Corpo della mail.
040300141006       //--------------------------------------------------------------
040400141007       BEGSR Corpomail;
040500141006
040600141007         clear testo;
040700141007         %subst(testo:04:7) = %editc(anagrafica.ksc:'X');
040800141007         %subst(testo:19:25) = anagrafica.rag;
040900141007         except PrtDet;
041000141006
041100141006       ENDSR;
041200141006
041300141006       //--------------------------------------------------------------
041400141006       //?Manda la mail.
041500141006       //--------------------------------------------------------------
041600141007       BEGSR FineMail;
041700141006
041800141007       //?Se file di stampa aperto
041900141007         IF   %open(PRTEMAIL);
042000141007
042100141007         //?fine del testo della mail
042200141007           clear testo;
042300141007           except PrtDet;
042400141007           testo = 'Vi invitiamo a completare le anagrafiche il ' +
042500141007                   'prima possibile.';
042600141007           except PrtDet;
042700141007           clear testo;
042800141007           except PrtDet;
042900141007           testo = 'A disposizione per ogni chiarimento.';
043000141007           except PrtDet;
043100141007           clear testo;
043200141007           except PrtDet;
043300141007           testo = 'ICT di Sede';
043400141007           except PrtDet;
043500141007           testo = 'BRT SPA';
043600141007           except PrtDet;
043700141007
043800141007           close PRTEMAIL;
043900141007
044000141007           cmd = CmdOvrPrtF;
044100141007           ExecuteCommand (cmd);
044200141007
044300141007         ENDIF;
044400141006
044500141006       ENDSR;
044600141003
044700141003       //--------------------------------------------------------------
044800141003       //?Operazioni finali.
044900141003       //--------------------------------------------------------------
045000141003       BEGSR RoutEnd;
045100141003
045200141003         *inLR = *on;
045300141003         return;
045400141003
045500141003       ENDSR;
045600141006
045700141006      /end-free
045800141007      //---------------------------------------------------------------
045900141007      //?Spool di stampa (per e-mail).
046000141007      //---------------------------------------------------------------
046100141007     oPRTEMAIL  e            prtdet      1
046200141007     o                       testo
046300141007
