000100131128       //---------------------------------------------------------------
000200131128       //?FNLRX3R - Scrive file TELEFONATE AUT da gestione NOTE AUT
000300131128       //---------------------------------------------------------------
000400131128
000500131128       //--------------------------------------------------------------
000600131128       //?Specifiche di controllo.                                     ?
000700131128       //--------------------------------------------------------------
000800131128     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000900131128
001000131128       //---------------------------------------------------------------
001100131128       //?Dichiarazione file.
001200131128       //---------------------------------------------------------------
001300131128
001400131128       //?File Telefonate AUT
001500131129     fFICAU00F  uf a e             disk
001600131128
001700131128       //---------------------------------------------------------------
001800131128       //?Definizione costanti.
001900131128       //---------------------------------------------------------------
002000131128
002100131128       //---------------------------------------------------------------
002200131128       //?Definizione schiere.
002300131128       //---------------------------------------------------------------
002400131128
002500131128       //---------------------------------------------------------------
002600131128       //?Definizione aree dati.
002700131128       //---------------------------------------------------------------
002800131128
002900131128       //---------------------------------------------------------------
003000131128       //?Definizione strutture dati.
003100131128       //---------------------------------------------------------------
003200131128
003300131128       //?kpjba?
003400131128     d KPJBA         e ds
003500131128
003600131128       //?Tabella CMD - Comando
003700131128     d dCMD          e ds                  inz
003800131128
003900131128       //?Tabella TCV - Tipologia variazione
004000131128     d dTCV          e ds                  inz
004100131128
004200131128       //?File telefonate
004300131128     d FICAUDS       e ds                  extname(FICAU00F)
004400131128     d                                     prefix(w)
004500131128
004600131128       //?Parametri da chiamante
004700131128     d FNLRX3DS      e ds
004800131128
004900131128       //?Tabelle
005000131128     d TIBS02DS      e ds                  inz
005100131128
005200131128       //---------------------------------------------------------------
005300131128       //?Definizione variabili globali.
005400131128       //---------------------------------------------------------------
005500131128
005600131128       //?Flags booleani
005700131128     d wEoFf1          s               n   inz(*off)
005800140117     d wAggiorna       s               n   inz(*off)
005900140116     d wDelete         s               n   inz(*off)
006000140116     d wNoficau        s               n   inz(*off)
006100131128     d wScrivi         s               n   inz(*off)
006200131128     d wVaria          s               n   inz(*off)
006300131128
006400131128       //?Campi di comodo
006500131129     d wFlgVar         s                   like(IX3var)
006600131128     d wRrn            s              5i 0 inz
006700131206     d xx              s              2  0
006800131206
006900131206       //?ds di comodo
007000131206     d FLGvar          ds            10    inz
007100131206     d  $var                          1    inz  dim(10)
007200131128
007300131128       //---------------------------------------------------------------
007400131128       //?Definizione procedure esterne.
007500131128       //---------------------------------------------------------------
007600131128
007700131128       //---------------------------------------------------------------
007800131128       //?Definizione prototipi.
007900131128       //---------------------------------------------------------------
008000131128
008100131128      /copy gaitrasrc/srcprotopr,tibs02r
008200131128
008300131128       //---------------------------------------------------------------
008400131128       //?Definizione key-list.
008500131128       //---------------------------------------------------------------
008600131128
008700131128       //---------------------------------------------------------------
008800131128       //?Main.
008900131128       //---------------------------------------------------------------
009000131128
009100131128     c     *Entry        plist
009200131128     c                   parm                    KPJBA
009300131128     c                   parm                    FNLRX3DS
009400131128
009500131128      /free
009600131128
009700131128       //?Operazioni iniziali?
009800131128       exsr RoutInz;
009900131128
010000131128       //?Elabora?
010100131128       exsr Elabora;
010200131128
010300131128       //?Operazioni finali?
010400131128       exsr RoutEnd;
010500131128
010600131128       //--------------------------------------------------------------
010700131128       //?Operazioni iniziali.                                         ?
010800131128       //--------------------------------------------------------------
010900131128       BEGSR  RoutInz;
011000131128
011100131128         *inLR = *on;
011200131128
011300131128         OX3err = *off;
011400131128         clear OX3msg;
011500131128
011600131128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011700131128
011800131128       ENDSR;
011900131128
012000131128       //--------------------------------------------------------------
012100131128       //?Elabora.
012200131128       //--------------------------------------------------------------
012300131128       BEGSR  Elabora;
012400131128
012500140117         wAggiorna = *off;
012600131128         wEoFf1 = *off;
012700140116         wDelete = *off;
012800140116         wNoficau = *off;
012900131128         wScrivi = *off;
013000131128
013100131128       //?Leggo ultimo rcd telefonata a parità di oggetto/distinta/aut
013200131128         exec sql
013300131128         DECLARE  f1 cursor FOR
013400131128         SELECT   rrn(ficau00f), ficau00f.*
013500131128         FROM     ficau00f
013600131128         WHERE    CAUtor = :IX3tpdoc and CAUogg = :IX3iddoc and
013700131128                  CAUfgs = :IX3fgs and CAUnfv = :IX3nfv and
013800131128                  CAUpdr = :IX3pdr
013900131128         ORDER BY CAUtor, CAUogg, CAUfgs, CAUnfv, CAUpdr, CAUdtoins desc;
014000131128
014100131128         exec sql OPEN f1;
014200131128
014300131128         IF sqlcode < 0;
014400131128           wEoFf1 = *on;
014500131128         ENDIF;
014600131128
014700131128         DOW not wEoFf1;
014800131128
014900131128           exec sql
015000131128           FETCH next from f1 into :wRrn, :ficauds;
015100131128
015200131128       //?fine file o errore sql esco
015300131128           IF  sqlcod = 100 or sqlcod < 0;
015400131128             wEoFf1 = *on;
015500131128             leave;
015600131128           ENDIF;
015700131128
015800140116       //?Tipo documento passato BOLLA
015900140117           IF  IX3tpdoc = 'C';
016000131128       //?Verifico se la telefonata è già stata fatta
016100131128           IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
016200131128             wScrivi = *on;
016300131128           ENDIF;
016400140116           ENDIF;
016500140116
016600140116       //?Tipo documento passato ORM
016700140117           IF  IX3tpdoc = 'R';
016800140116         //?Controllo il comando ricevuto
016900140116             exsr ctrCMD;
017000140116           ENDIF;
017100131128
017200131128           wEoFf1 = *on;
017300131128           leave;
017400131128
017500131128         ENDDO;
017600131128
017700131128       //?chiudo il cursore
017800131128         exec sql CLOSE f1;
017900131128
018000131128       //?Scrivo/Aggiorno file Telefonate
018100131128         exsr  Telefonate;
018200131128
018300131128       ENDSR;
018400140116
018500140116       //--------------------------------------------------------------
018600140116       //?Controllo comando ricevuto x ORM.                            ?
018700140116       //--------------------------------------------------------------
018800140116       BEGSR  ctrCMD;
018900140116
019000140116         SELECT;
019100140116
019200140117       //?==> INSERITO
019300140117           WHEN  IX3cmd = 'I';
019400140116         //?Se la telefonata che sto leggendo
019500140116         //?è INSERITO o VARIATO
019600140116         //?NON devo scrivere/aggiornare FICAU
019700140116             IF  wCAUcmd = 'I' or wCAUcmd = 'V';
019800140117               wNoficau = *on;
019900140116             ENDIF;
020000140116
020100140116         //?Se la telefonata che sto leggendo
020200140117         //?è ANNULLATO
020300140117             IF  wCAUcmd = 'A';
020400140117           //?se è stata effettuta
020500140117           //?devo scrivere FICAU
020600140117               IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
020700140117                 wScrivi = *on;
020800140117               ENDIF;
020900140117           //?se NON è stata effettuta
021000140117           //?cancello la telefonata ANNULLATO e NON devo scrivere/aggiornare FICAU
021100140121               IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
021200140117                 wDelete = *on;
021300140117               ENDIF;
021400140116             ENDIF;
021500140116
021600140117       //?==> VARIATO
021700140116           WHEN  IX3cmd = 'V';
021800140117         //?Se la telefonata che sto leggendo
021900140117         //?é ANNULLATO
022000140117         //?NON devo scrivere/aggiornare FICAU
022100140117             IF  wCAUcmd = 'A';
022200140117               wNoficau = *on;
022300140117             ENDIF;
022400140117         //?Se la telefonata che sto leggendo
022500140117         //?è stata effettuta
022600140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
022700140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
022800140117               wScrivi = *on;
022900140117             ENDIF;
023000140117         //?Se la telefonata che sto leggendo
023100140117         //?NON è stata effettuta
023200140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
023300140117           //?se é INSERITO
023400140130           //?devo aggiornare FICAU
023500140117               IF  wCAUcmd = 'I';
023600140130                 wAggiorna = *on;
023700140117               ENDIF;
023800140117           //?se é VARIATO
023900140117           //?devo aggiornare FICAU
024000140117               IF  wCAUcmd = 'V';
024100140117                 wAggiorna = *on;
024200140117               ENDIF;
024300140117             ENDIF;
024400140117
024500140117       //?==> ANNULLATO
024600140117           WHEN  IX3cmd = 'A';
024700140117         //?Se la telefonata che sto leggendo
024800140117         //?é ANNULLATO
024900140117         //?NON devo scrivere/aggiornare FICAU
025000140117             IF  wCAUcmd = 'A';
025100140117               wNoficau = *on;
025200140117             ENDIF;
025300140117         //?Se la telefonata che sto leggendo
025400140117         //?è stata effettuta
025500140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
025600140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
025700140117               wScrivi = *on;
025800140117             ENDIF;
025900140117         //?Se la telefonata che sto leggendo
026000140117         //?NON è stata effettuta
026100140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
026200140117           //?se é INSERITO
026300140117           //?cancello la telefonata INSERITO e NON devo scrivere/aggiornare FICAU
026400140117               IF  wCAUcmd = 'I';
026500140117                 wDelete = *on;
026600140117               ENDIF;
026700140117           //?se é VARIATO
026800140117           //?cancello la telefonata VARIATO e devo scrivere FICAU
026900140117               IF  wCAUcmd = 'V';
027000140117                 wDelete = *on;
027100140117                 wScrivi = *on;
027200140117               ENDIF;
027300140117             ENDIF;
027400140117
027500140117         ENDSL;
027600140116
027700140116       ENDSR;
027800131128
027900131128       //--------------------------------------------------------------
028000131128       //?Scrivo/Aggiorno file telefonate.                             ?
028100131128       //--------------------------------------------------------------
028200131128       BEGSR  Telefonate;
028300131129
028400131128         wVaria = *off;
028500140117
028600140117       //?Se richiamato da ORM e ANNULLATO o VARIATO e non ho trovato nessuna telefonata
028700140117       //?esco senza scrivere/aggiornare FICAU
028800140117         IF  wRrn = 0 and IX3tpdoc = 'R' and
028900140117            (IX3cmd = 'V' or IX3cmd = 'A');
029000140117           leavesr;
029100140117         ENDIF;
029200140117
029300140117       //?Se NON devo scrivere/aggiornare
029400140117       //?esco
029500140117         IF  wNoficau;
029600140117           leavesr;
029700140117         ENDIF;
029800140117
029900140117       //?Cancello la telefonata trovata
030000140117         IF  wDelete;
030100140117           chain wRrn FICAU00F;
030200140117           IF  %found(FICAU00F);
030300140117             delete  FICAU000;
030400140117           ENDIF;
030500140117         ENDIF;
030600140117
030700131129       //?Se devo scrivere un nuovo rcd perchè non ho trovato nessuna telefonata
030800131129       //?precedente imposto i campi dalla DS di passaggio dati
030900131129         IF  wRrn = 0;
031000131129           clear FICAU000;
031100131129           CAUtor = IX3tpdoc;
031200131129           CAUogg = IX3iddoc;
031300131129           CAUfgs = IX3fgs;
031400131129           CAUnfv = IX3nfv;
031500131129           CAUdfv = IX3dfv;
031600131129           CAUpdr = IX3pdr;
031700131129           CAUflt = 'T';
031800131129         //?Se passati dalla DS di passaggio dati
031900131129         //?imposto il comando
032000131129           IF  IX3cmd <> *blanks;
032100131129             CAUcmd = IX3cmd;
032200131129             exsr tab_CMD;
032300131129             CAUdescmd = §CMDdes;
032400131129           ENDIF;
032500131129         //?imposto la variazione
032600131129           IF  IX3var <> *blanks;
032700131206               FlgVar = IX3var;
032800131206               xx = 1;
032900131206               FOR xx by 1 to %elem($var);
033000131206                  IF $var(xx) <> *blanks;
033100131206                     eval wFlgVar= $var(xx) ;
033200131206                     exsr tab_TCV;
033300140122                     IF  CAUdesva = *blanks;
033400140122                       CAUdesva = §TCVdesb ;
033500140122                     ELSE;
033600140122                       CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
033700140122                    ENDIF;
033800131206                  ENDIF;
033900131206               ENDFOR;
034000131206
034100131129               CAUtva = IX3var;
034200131206           ENDIF;
034300131129         ENDIF;
034400131128
034500131129       //?Se devo scrivere un nuovo rcd perchè la telefonata trovata è già stata
034600131129       //?effettuata imposto i campi dal rcd trovato
034700131128         IF  wScrivi;
034800131128           clear FICAU000;
034900131129           CAUtor = wCAUtor;
035000131129           CAUogg = wCAUogg;
035100131129           CAUfgs = wCAUfgs;
035200131129           CAUnfv = wCAUnfv;
035300131129           CAUdfv = wCAUdfv;
035400131129           CAUpdr = wCAUpdr;
035500131129           CAUflt = wCAUflt;
035600131129         //?Se passato aggiorno il comando, solo se variato
035700131129           IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
035800131129             CAUcmd = IX3cmd;
035900131129             exsr tab_CMD;
036000131129             CAUdescmd = §CMDdes;
036100131129           ENDIF;
036200140117         //?Se passato il comando ed è lo stesso delle telefonata che sto leggendo
036300140117         //?ed è un richiamo da ORM
036400140117         //?imposto il comando della telefonata che sto leggendo
036500140117           IF  IX3cmd <> *blanks and IX3cmd = wCAUcmd and
036600140117               IX3tpdoc = 'R';
036700140117             CAUcmd = wCAUcmd;
036800140117             CAUdescmd = wCAUdescmd;
036900140117           ENDIF;
037000131129         //?Se non passato riporto il comando del rcd trovato
037100131129           IF  IX3cmd = *blanks;
037200131129             CAUcmd = wCAUcmd;
037300131129             CAUdescmd = wCAUdescmd;
037400131129           ENDIF;
037500131206         //?Se passata aggiorno la variazione, ma solo se non è già presente
037600131206           IF  IX3var <> *blanks;
037700131206               FlgVar = IX3var;
037800131206               xx = 1;
037900131206               FOR xx by 1 to %elem($var);
038000131206                  IF $var(xx) <> *blanks;
038100131206                   IF  %scan($var(xx):CAUtva) = 0;
038200131206                       eval wFlgVar= $var(xx) ;
038300131206                       exsr tab_TCV;
038400131206                       CAUtva = %trim(CAUtva) + wFlgVar;
038500140117                       if CAUdesva = *blanks ;
038600131211                          CAUdesva = §TCVdesb ;
038700131211                       Else ;
038800131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
038900131211                       ENDIF;
039000131206                   ENDIF;
039100131206                  ENDIF;
039200131206               ENDFOR;
039300131206           ENDIF;
039400131206         ENDIF;
039500131129
039600131129       //?Scrivo il rcd
039700131129         IF  wScrivi or wRrn = 0;
039800131129           CAUdtoins = %char(%timestamp:*iso0);
039900140122           CAUpruins = knmus;
040000131129           write FICAU000;
040100131129         ENDIF;
040200131128
040300131129       //?Se devo aggiornare un rcd già presente perchè la telefonata non è ancora
040400131129       //?stata fatta
040500140117       //?ed è un richiamo da BOLLE
040600140117         IF  (not wScrivi and wRrn > 0 and IX3tpdoc = 'C') or
040700140117       //?è un richiamo da ORM e devo aggiornare
040800140117             (wAggiorna and IX3tpdoc = 'R');
040900131129           chain wRrn FICAU00F;
041000131129           IF  %found(FICAU00F);
041100131129           //?Se passato aggiorno il comando, solo se variato
041200131129             IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
041300140130             //?Se il comando che ho ricevuto è 'VARIATO' e sto leggendo un
041400140130             //?rcd 'INSERITO' non devo aggiornare il comando
041500140130               IF  IX3cmd = 'V' and wCAUcmd = 'I';
041600140130               ELSE;
041700131129               CAUcmd = IX3cmd;
041800131129               exsr tab_CMD;
041900131129               CAUdescmd = §CMDdes;
042000131129               wVaria = *on;
042100140130               ENDIF;
042200131129             ENDIF;
042300131129           //?Se passata aggiorno la variazione, ma solo se non è già presente
042400131129             IF  IX3var <> *blanks;
042500131206               FlgVar = IX3var;
042600131206               xx = 1;
042700131206               FOR xx by 1 to %elem($var);
042800131206                IF $var(xx) <> *blanks;
042900131206                   IF  %scan($var(xx):CAUtva) = 0;
043000131206                       eval wFlgVar= $var(xx) ;
043100131206                       exsr tab_TCV;
043200131206                       CAUtva = %trim(CAUtva) + wFlgVar;
043300131211                       If CAUdesva = *blanks ;
043400131211                          CAUdesva = §TCVdesb;
043500131211                       Else ;
043600131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
043700131211                       ENDIF ;
043800131206                       wVaria = *on;
043900131206                   ENDIF;
044000131206                ENDIF;
044100131206               ENDFOR;
044200131129             ENDIF;
044300131129             IF  wVaria;
044400131129               CAUdtoins = %char(%timestamp:*iso0);
044500140122               CAUpruins = knmus;
044600131129             ENDIF;
044700131129             update FICAU000;
044800131129           ENDIF;
044900131129         ENDIF;
045000131128
045100131128       ENDSR;
045200131128
045300131128       //--------------------------------------------------------------
045400131128       //?Aggancio tabella CMD.                                        ?
045500131128       //--------------------------------------------------------------
045600131128       BEGSR  tab_CMD;
045700131128
045800131128         clear dCMD;
045900131128         clear TIBS02DS;
046000131128         T02mod = 'C';
046100131128         T02sif = knsif;
046200131128         T02cod = 'CMD';
046300131128         T02ke1 = CAUcmd;
046400131128         TNTBE_RicercaControllo (kpjba : tibs02ds);
046500131128         IF  T02err = *blanks;
046600131128           dCMD = T02uni;
046700131128         ENDIF;
046800131128
046900131128       ENDSR;
047000131129
047100131129       //--------------------------------------------------------------
047200131129       //?Aggancio tabella TCV.                                   ?
047300131129       //--------------------------------------------------------------
047400131129       BEGSR  tab_TCV;
047500131129
047600131129         clear dTCV;
047700131129         clear TIBS02DS;
047800131129         T02mod = 'C';
047900131129         T02sif = knsif;
048000131129         T02cod = 'TCV';
048100131129         T02ke1 = wFlgVar;
048200131129         TNTBE_RicercaControllo (kpjba : tibs02ds);
048300131129         IF  T02err = *blanks;
048400131129           dTCV = T02uni;
048500131129         ENDIF;
048600131129
048700131129       ENDSR;
048800131128
048900131128       //--------------------------------------------------------------
049000131128       //?Operazioni finali.                                           ?
049100131128       //--------------------------------------------------------------
049200131128       BEGSR  RoutEnd;
049300131128
049400131128         return;
049500131128
049600131128       ENDSR;
049700131128
049800131128      /end-free
