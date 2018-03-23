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
001600140423     fFIDST08L  if   e           k disk
001700131128
001800131128       //---------------------------------------------------------------
001900131128       //?Definizione costanti.
002000131128       //---------------------------------------------------------------
002100131128
002200131128       //---------------------------------------------------------------
002300131128       //?Definizione schiere.
002400131128       //---------------------------------------------------------------
002500131128
002600131128       //---------------------------------------------------------------
002700131128       //?Definizione aree dati.
002800131128       //---------------------------------------------------------------
002900131128
003000131128       //---------------------------------------------------------------
003100131128       //?Definizione strutture dati.
003200131128       //---------------------------------------------------------------
003300131128
003400131128       //?kpjba?
003500131128     d KPJBA         e ds
003600131128
003700131128       //?Tabella CMD - Comando
003800131128     d dCMD          e ds                  inz
003900140423
004000140423       //?DS DSTFLR Distinta
004100140423     d dDSTflr       e ds                  inz
004200131128
004300131128       //?Tabella TCV - Tipologia variazione
004400131128     d dTCV          e ds                  inz
004500131128
004600131128       //?File telefonate
004700131128     d FICAUDS       e ds                  extname(FICAU00F)
004800131128     d                                     prefix(w)
004900131128
005000131128       //?Parametri da chiamante
005100131128     d FNLRX3DS      e ds
005200131128
005300131128       //?Tabelle
005400131128     d TIBS02DS      e ds                  inz
005500131128
005600131128       //---------------------------------------------------------------
005700131128       //?Definizione variabili globali.
005800131128       //---------------------------------------------------------------
005900131128
006000131128       //?Flags booleani
006100131128     d wEoFf1          s               n   inz(*off)
006200140117     d wAggiorna       s               n   inz(*off)
006300140116     d wDelete         s               n   inz(*off)
006400140116     d wNoficau        s               n   inz(*off)
006500131128     d wScrivi         s               n   inz(*off)
006600131128     d wVaria          s               n   inz(*off)
006700131128
006800131128       //?Campi di comodo
006900131129     d wFlgVar         s                   like(IX3var)
007000151106     d wRrn            s             10s 0 inz
007100131206     d xx              s              2  0
007200140314
007300140314       // -?Causali Esito Telefonata?
007400140314     d c_Eseguita      c                   const('ES')
007500140317
007600140317       // -?Status ds?
007700140317     d Status         sds
007800140317     d   SDSjobName          244    253                                         Job name
007900140317     d   SDSjobUser          254    263                                         User name
008000140317
008100131206
008200131206       //?ds di comodo
008300131206     d FLGvar          ds            10    inz
008400131206     d  $var                          1    inz  dim(10)
008500131128
008600131128       //---------------------------------------------------------------
008700131128       //?Definizione procedure esterne.
008800131128       //---------------------------------------------------------------
008900131128
009000131128       //---------------------------------------------------------------
009100131128       //?Definizione prototipi.
009200131128       //---------------------------------------------------------------
009300140314
009400131128      /copy gaitrasrc/srcprotopr,tibs02r
009500140314
009600131128
009700131128       //---------------------------------------------------------------
009800131128       //?Definizione key-list.
009900131128       //---------------------------------------------------------------
010000131128
010100131128       //---------------------------------------------------------------
010200131128       //?Main.
010300131128       //---------------------------------------------------------------
010400131128
010500131128     c     *Entry        plist
010600131128     c                   parm                    KPJBA
010700131128     c                   parm                    FNLRX3DS
010800131128
010900131128      /free
011000131128
011100131128       //?Operazioni iniziali?
011200131128       exsr RoutInz;
011300131128
011400131128       //?Elabora?
011500131128       exsr Elabora;
011600131128
011700131128       //?Operazioni finali?
011800131128       exsr RoutEnd;
011900131128
012000131128       //--------------------------------------------------------------
012100131128       //?Operazioni iniziali.                                         ?
012200131128       //--------------------------------------------------------------
012300131128       BEGSR  RoutInz;
012400131128
012500131128         *inLR = *on;
012600131128
012700131128         OX3err = *off;
012800131128         clear OX3msg;
012900131128
013000131128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013100131128
013200131128       ENDSR;
013300131128
013400131128       //--------------------------------------------------------------
013500131128       //?Elabora.
013600131128       //--------------------------------------------------------------
013700131128       BEGSR  Elabora;
013800131128
013900140117         wAggiorna = *off;
014000131128         wEoFf1 = *off;
014100140116         wDelete = *off;
014200140116         wNoficau = *off;
014300131128         wScrivi = *off;
014400131128
014500131128       //?Leggo ultimo rcd telefonata a parità di oggetto/distinta/aut
014600131128         exec sql
014700131128         DECLARE  f1 cursor FOR
014800131128         SELECT   rrn(ficau00f), ficau00f.*
014900131128         FROM     ficau00f
015000131128         WHERE    CAUtor = :IX3tpdoc and CAUogg = :IX3iddoc and
015100131128                  CAUfgs = :IX3fgs and CAUnfv = :IX3nfv and
015200131128                  CAUpdr = :IX3pdr
015300131128         ORDER BY CAUtor, CAUogg, CAUfgs, CAUnfv, CAUpdr, CAUdtoins desc;
015400131128
015500131128         exec sql OPEN f1;
015600131128
015700131128         IF sqlcode < 0;
015800131128           wEoFf1 = *on;
015900131128         ENDIF;
016000131128
016100131128         DOW not wEoFf1;
016200131128
016300131128           exec sql
016400131128           FETCH next from f1 into :wRrn, :ficauds;
016500131128
016600131128       //?fine file o errore sql esco
016700131128           IF  sqlcod = 100 or sqlcod < 0;
016800131128             wEoFf1 = *on;
016900131128             leave;
017000131128           ENDIF;
017100131128
017200140116       //?Tipo documento passato BOLLA
017300140117           IF  IX3tpdoc = 'C';
017400131128       //?Verifico se la telefonata è già stata fatta
017500131128           IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
017600131128             wScrivi = *on;
017700131128           ENDIF;
017800140116           ENDIF;
017900140116
018000140116       //?Tipo documento passato ORM
018100140117           IF  IX3tpdoc = 'R';
018200140116         //?Controllo il comando ricevuto
018300140116             exsr ctrCMD;
018400140116           ENDIF;
018500131128
018600131128           wEoFf1 = *on;
018700131128           leave;
018800131128
018900131128         ENDDO;
019000131128
019100131128       //?chiudo il cursore
019200131128         exec sql CLOSE f1;
019300131128
019400131128       //?Scrivo/Aggiorno file Telefonate
019500131128         exsr  Telefonate;
019600131128
019700131128       ENDSR;
019800140116
019900140116       //--------------------------------------------------------------
020000140116       //?Controllo comando ricevuto x ORM.                            ?
020100140116       //--------------------------------------------------------------
020200140116       BEGSR  ctrCMD;
020300140116
020400140116         SELECT;
020500140116
020600140117       //?==> INSERITO
020700140117           WHEN  IX3cmd = 'I';
020800140116         //?Se la telefonata che sto leggendo
020900140116         //?è INSERITO o VARIATO
021000140116         //?NON devo scrivere/aggiornare FICAU
021100140116             IF  wCAUcmd = 'I' or wCAUcmd = 'V';
021200140117               wNoficau = *on;
021300140116             ENDIF;
021400140116
021500140116         //?Se la telefonata che sto leggendo
021600140117         //?è ANNULLATO
021700140117             IF  wCAUcmd = 'A';
021800140117           //?se è stata effettuta
021900140117           //?devo scrivere FICAU
022000140117               IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
022100140117                 wScrivi = *on;
022200140117               ENDIF;
022300140117           //?se NON è stata effettuta
022400140117           //?cancello la telefonata ANNULLATO e NON devo scrivere/aggiornare FICAU
022500140121               IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
022600140117                 wDelete = *on;
022700140117               ENDIF;
022800140116             ENDIF;
022900140116
023000140117       //?==> VARIATO
023100140116           WHEN  IX3cmd = 'V';
023200140117         //?Se la telefonata che sto leggendo
023300140117         //?é ANNULLATO
023400140117         //?NON devo scrivere/aggiornare FICAU
023500140117             IF  wCAUcmd = 'A';
023600140117               wNoficau = *on;
023700140117             ENDIF;
023800140117         //?Se la telefonata che sto leggendo
023900140117         //?è stata effettuta
024000140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
024100140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
024200140117               wScrivi = *on;
024300140117             ENDIF;
024400140117         //?Se la telefonata che sto leggendo
024500140117         //?NON è stata effettuta
024600140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
024700140117           //?se é INSERITO
024800140130           //?devo aggiornare FICAU
024900140117               IF  wCAUcmd = 'I';
025000140130                 wAggiorna = *on;
025100140117               ENDIF;
025200140117           //?se é VARIATO
025300140117           //?devo aggiornare FICAU
025400140117               IF  wCAUcmd = 'V';
025500140117                 wAggiorna = *on;
025600140117               ENDIF;
025700140117             ENDIF;
025800140117
025900140117       //?==> ANNULLATO
026000140117           WHEN  IX3cmd = 'A';
026100140117         //?Se la telefonata che sto leggendo
026200140117         //?é ANNULLATO
026300140117         //?NON devo scrivere/aggiornare FICAU
026400140117             IF  wCAUcmd = 'A';
026500140117               wNoficau = *on;
026600140117             ENDIF;
026700140117         //?Se la telefonata che sto leggendo
026800140117         //?è stata effettuta
026900140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
027000140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
027100140117               wScrivi = *on;
027200140117             ENDIF;
027300140117         //?Se la telefonata che sto leggendo
027400140117         //?NON è stata effettuta
027500140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
027600140117           //?se é INSERITO
027700140117           //?cancello la telefonata INSERITO e NON devo scrivere/aggiornare FICAU
027800140117               IF  wCAUcmd = 'I';
027900140117                 wDelete = *on;
028000140117               ENDIF;
028100140117           //?se é VARIATO
028200140117           //?cancello la telefonata VARIATO e devo scrivere FICAU
028300140117               IF  wCAUcmd = 'V';
028400140117                 wDelete = *on;
028500140117                 wScrivi = *on;
028600140117               ENDIF;
028700140117             ENDIF;
028800140117
028900140117         ENDSL;
029000140116
029100140116       ENDSR;
029200131128
029300131128       //--------------------------------------------------------------
029400131128       //?Scrivo/Aggiorno file telefonate.                             ?
029500131128       //--------------------------------------------------------------
029600131128       BEGSR  Telefonate;
029700131129
029800131128         wVaria = *off;
029900140117
030000140117       //?Se richiamato da ORM e ANNULLATO o VARIATO e non ho trovato nessuna telefonata
030100140117       //?esco senza scrivere/aggiornare FICAU
030200140117         IF  wRrn = 0 and IX3tpdoc = 'R' and
030300140117            (IX3cmd = 'V' or IX3cmd = 'A');
030400140117           leavesr;
030500140117         ENDIF;
030600140117
030700140117       //?Se NON devo scrivere/aggiornare
030800140117       //?esco
030900140117         IF  wNoficau;
031000140117           leavesr;
031100140117         ENDIF;
031200140117
031300140117       //?Cancello la telefonata trovata
031400140117         IF  wDelete;
031500140117           chain wRrn FICAU00F;
031600140117           IF  %found(FICAU00F);
031700140117             delete  FICAU000;
031800140117           ENDIF;
031900140117         ENDIF;
032000140117
032100131129       //?Se devo scrivere un nuovo rcd perchè non ho trovato nessuna telefonata
032200131129       //?precedente imposto i campi dalla DS di passaggio dati
032300131129         IF  wRrn = 0;
032400131129           clear FICAU000;
032500131129           CAUtor = IX3tpdoc;
032600131129           CAUogg = IX3iddoc;
032700131129           CAUfgs = IX3fgs;
032800131129           CAUnfv = IX3nfv;
032900131129           CAUdfv = IX3dfv;
033000131129           CAUpdr = IX3pdr;
033100131129           CAUflt = 'T';
033200131129         //?Se passati dalla DS di passaggio dati
033300131129         //?imposto il comando
033400131129           IF  IX3cmd <> *blanks;
033500131129             CAUcmd = IX3cmd;
033600131129             exsr tab_CMD;
033700131129             CAUdescmd = §CMDdes;
033800131129           ENDIF;
033900131129         //?imposto la variazione
034000131129           IF  IX3var <> *blanks;
034100131206               FlgVar = IX3var;
034200131206               xx = 1;
034300131206               FOR xx by 1 to %elem($var);
034400131206                  IF $var(xx) <> *blanks;
034500131206                     eval wFlgVar= $var(xx) ;
034600131206                     exsr tab_TCV;
034700140122                     IF  CAUdesva = *blanks;
034800140122                       CAUdesva = §TCVdesb ;
034900140122                     ELSE;
035000140122                       CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
035100140122                    ENDIF;
035200131206                  ENDIF;
035300131206               ENDFOR;
035400131206
035500131129               CAUtva = IX3var;
035600131206           ENDIF;
035700131129         ENDIF;
035800131128
035900131129       //?Se devo scrivere un nuovo rcd perchè la telefonata trovata è già stata
036000131129       //?effettuata imposto i campi dal rcd trovato
036100131128         IF  wScrivi;
036200131128           clear FICAU000;
036300131129           CAUtor = wCAUtor;
036400131129           CAUogg = wCAUogg;
036500131129           CAUfgs = wCAUfgs;
036600131129           CAUnfv = wCAUnfv;
036700131129           CAUdfv = wCAUdfv;
036800131129           CAUpdr = wCAUpdr;
036900131129           CAUflt = wCAUflt;
036901170926           CAUcmd = wCAUcmd;
036902170926           CAUdescmd = wCAUdescmd;
037000131129         //?Se passato aggiorno il comando, solo se variato
037100131129           IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
037200131129             CAUcmd = IX3cmd;
037300131129             exsr tab_CMD;
037400131129             CAUdescmd = §CMDdes;
037500131129           ENDIF;
037600140117         //?Se passato il comando ed è lo stesso delle telefonata che sto leggendo
037700140117         //?ed è un richiamo da ORM
037800140117         //?imposto il comando della telefonata che sto leggendo
037900140117           IF  IX3cmd <> *blanks and IX3cmd = wCAUcmd and
038000140117               IX3tpdoc = 'R';
038100140117             CAUcmd = wCAUcmd;
038200140117             CAUdescmd = wCAUdescmd;
038300140117           ENDIF;
038400131129         //?Se non passato riporto il comando del rcd trovato
038500131129           IF  IX3cmd = *blanks;
038600131129             CAUcmd = wCAUcmd;
038700131129             CAUdescmd = wCAUdescmd;
038800131129           ENDIF;
038900131206         //?Se passata aggiorno la variazione, ma solo se non è già presente
039000131206           IF  IX3var <> *blanks;
039100131206               FlgVar = IX3var;
039200131206               xx = 1;
039300131206               FOR xx by 1 to %elem($var);
039400131206                  IF $var(xx) <> *blanks;
039500131206                   IF  %scan($var(xx):CAUtva) = 0;
039600131206                       eval wFlgVar= $var(xx) ;
039700131206                       exsr tab_TCV;
039800131206                       CAUtva = %trim(CAUtva) + wFlgVar;
039900140117                       if CAUdesva = *blanks ;
040000131211                          CAUdesva = §TCVdesb ;
040100131211                       Else ;
040200131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
040300131211                       ENDIF;
040400131206                   ENDIF;
040500131206                  ENDIF;
040600131206               ENDFOR;
040700131206           ENDIF;
040800131206         ENDIF;
040900131129
041000131129       //?Scrivo il rcd
041100131129         IF  wScrivi or wRrn = 0;
041200131129           CAUdtoins = %char(%timestamp:*iso0);
041300140317           CAUpruins = SDSjobUser;
041400140523       //?Se sto scrivendo un record relativo alle spedizioni
041500140523       //?scrivo la telefonata come eseguita
041600140423       //?solo se in una distinta con PDA
041700140523           If IX3tpdoc = 'C';
041800140423               clear DDSTflr ;
041900140423               chain (IX3fgs:4:IX3dfv:IX3nfv) FIDST08L;
042000140423               IF  %found(FIDST08L);
042100140423                   dDSTflr = DSTflr;
042200140423               ENDIF;
042300140423       //?Verifico se a PDA se a PDA in TEST o NO PDA
042400140423               IF (DSTpda = 'C' or DSTpda = 'E') and
042500140423                  (§dsttstpda = ' ' or §dsttstpda = 'O');
042600140423                   CAUdtotel = %char(%timestamp:*iso0);
042700140423                   CAUprutel = SDSjobUser;
042800140423                   CAUterm   = SDSjobName;
042900140423                   CAUesito  = c_Eseguita;
043000140423                   CAUesec   = 'A';
043100140423               Endif;
043200140314           Endif;
043300140314
043400131129           write FICAU000;
043500131129         ENDIF;
043600131128
043700131129       //?Se devo aggiornare un rcd già presente perchè la telefonata non è ancora
043800131129       //?stata fatta
043900140117       //?ed è un richiamo da BOLLE
044000140117         IF  (not wScrivi and wRrn > 0 and IX3tpdoc = 'C') or
044100140117       //?è un richiamo da ORM e devo aggiornare
044200140117             (wAggiorna and IX3tpdoc = 'R');
044300131129           chain wRrn FICAU00F;
044400131129           IF  %found(FICAU00F);
044500131129           //?Se passato aggiorno il comando, solo se variato
044600131129             IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
044700140130             //?Se il comando che ho ricevuto è 'VARIATO' e sto leggendo un
044800140130             //?rcd 'INSERITO' non devo aggiornare il comando
044900140130               IF  IX3cmd = 'V' and wCAUcmd = 'I';
045000140130               ELSE;
045100131129               CAUcmd = IX3cmd;
045200131129               exsr tab_CMD;
045300131129               CAUdescmd = §CMDdes;
045400131129               wVaria = *on;
045500140130               ENDIF;
045600131129             ENDIF;
045700131129           //?Se passata aggiorno la variazione, ma solo se non è già presente
045800131129             IF  IX3var <> *blanks;
045900131206               FlgVar = IX3var;
046000131206               xx = 1;
046100131206               FOR xx by 1 to %elem($var);
046200131206                IF $var(xx) <> *blanks;
046300131206                   IF  %scan($var(xx):CAUtva) = 0;
046400131206                       eval wFlgVar= $var(xx) ;
046500131206                       exsr tab_TCV;
046600131206                       CAUtva = %trim(CAUtva) + wFlgVar;
046700131211                       If CAUdesva = *blanks ;
046800131211                          CAUdesva = §TCVdesb;
046900131211                       Else ;
047000131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
047100131211                       ENDIF ;
047200131206                       wVaria = *on;
047300131206                   ENDIF;
047400131206                ENDIF;
047500131206               ENDFOR;
047600131129             ENDIF;
047700131129             IF  wVaria;
047800131129               CAUdtoins = %char(%timestamp:*iso0);
047900140317               CAUpruins = SDSjobUser;
048000131129             ENDIF;
048100131129             update FICAU000;
048200131129           ENDIF;
048300131129         ENDIF;
048400131128
048500131128       ENDSR;
048600131128
048700131128       //--------------------------------------------------------------
048800131128       //?Aggancio tabella CMD.                                        ?
048900131128       //--------------------------------------------------------------
049000131128       BEGSR  tab_CMD;
049100131128
049200131128         clear dCMD;
049300131128         clear TIBS02DS;
049400131128         T02mod = 'C';
049500131128         T02sif = knsif;
049600131128         T02cod = 'CMD';
049700131128         T02ke1 = CAUcmd;
049800131128         TNTBE_RicercaControllo (kpjba : tibs02ds);
049900131128         IF  T02err = *blanks;
050000131128           dCMD = T02uni;
050100131128         ENDIF;
050200131128
050300131128       ENDSR;
050400131129
050500131129       //--------------------------------------------------------------
050600131129       //?Aggancio tabella TCV.                                   ?
050700131129       //--------------------------------------------------------------
050800131129       BEGSR  tab_TCV;
050900131129
051000131129         clear dTCV;
051100131129         clear TIBS02DS;
051200131129         T02mod = 'C';
051300131129         T02sif = knsif;
051400131129         T02cod = 'TCV';
051500131129         T02ke1 = wFlgVar;
051600131129         TNTBE_RicercaControllo (kpjba : tibs02ds);
051700131129         IF  T02err = *blanks;
051800131129           dTCV = T02uni;
051900131129         ENDIF;
052000131129
052100131129       ENDSR;
052200131128
052300131128       //--------------------------------------------------------------
052400131128       //?Operazioni finali.                                           ?
052500131128       //--------------------------------------------------------------
052600131128       BEGSR  RoutEnd;
052700131128
052800131128         return;
052900131128
053000131128       ENDSR;
053100131128
053200131128      /end-free
