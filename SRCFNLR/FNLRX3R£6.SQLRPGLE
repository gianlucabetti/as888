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
005500140314
005600140314       //?Parametri per recupero Filiali partite con PDA
005700140314     d TRULVPODS     e ds
005800140317     d   sk_PDA               16    765    inz   dim(250)
005900140314
006000131128
006100131128       //---------------------------------------------------------------
006200131128       //?Definizione variabili globali.
006300131128       //---------------------------------------------------------------
006400131128
006500131128       //?Flags booleani
006600131128     d wEoFf1          s               n   inz(*off)
006700140117     d wAggiorna       s               n   inz(*off)
006800140116     d wDelete         s               n   inz(*off)
006900140116     d wNoficau        s               n   inz(*off)
007000131128     d wScrivi         s               n   inz(*off)
007100131128     d wVaria          s               n   inz(*off)
007200131128
007300131128       //?Campi di comodo
007400131129     d wFlgVar         s                   like(IX3var)
007500131128     d wRrn            s              5i 0 inz
007600131206     d xx              s              2  0
007700140314
007800140314       // -?Causali Esito Telefonata?
007900140314     d c_Eseguita      c                   const('ES')
008000140317
008100140317       // -?Status ds?
008200140317     d Status         sds
008300140317     d   SDSjobName          244    253                                         Job name
008400140317     d   SDSjobUser          254    263                                         User name
008500140317
008600131206
008700131206       //?ds di comodo
008800131206     d FLGvar          ds            10    inz
008900131206     d  $var                          1    inz  dim(10)
009000131128
009100131128       //---------------------------------------------------------------
009200131128       //?Definizione procedure esterne.
009300131128       //---------------------------------------------------------------
009400131128
009500131128       //---------------------------------------------------------------
009600131128       //?Definizione prototipi.
009700131128       //---------------------------------------------------------------
009800131128
009900140314     D  TRULVPOR       PR                  EXTPGM('TRULVPOR')
010000140314     D  trulvpods                          LIKEDS(trulvpods)
010100140314
010200131128      /copy gaitrasrc/srcprotopr,tibs02r
010300140314
010400131128
010500131128       //---------------------------------------------------------------
010600131128       //?Definizione key-list.
010700131128       //---------------------------------------------------------------
010800131128
010900131128       //---------------------------------------------------------------
011000131128       //?Main.
011100131128       //---------------------------------------------------------------
011200131128
011300131128     c     *Entry        plist
011400131128     c                   parm                    KPJBA
011500131128     c                   parm                    FNLRX3DS
011600131128
011700131128      /free
011800131128
011900131128       //?Operazioni iniziali?
012000131128       exsr RoutInz;
012100131128
012200131128       //?Elabora?
012300131128       exsr Elabora;
012400131128
012500131128       //?Operazioni finali?
012600131128       exsr RoutEnd;
012700131128
012800131128       //--------------------------------------------------------------
012900131128       //?Operazioni iniziali.                                         ?
013000131128       //--------------------------------------------------------------
013100131128       BEGSR  RoutInz;
013200131128
013300131128         *inLR = *on;
013400131128
013500131128         OX3err = *off;
013600131128         clear OX3msg;
013700131128
013800131128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013900140314
014000140314         // -?Caricamento filiali partite con il nuovo PDA
014100140314         clear  TRULvpods ;
014200140314         IvpoKE1 = 'DECOFI816F17';
014300140317         trulvpor ( TRULvpods );
014400131128
014500131128       ENDSR;
014600131128
014700131128       //--------------------------------------------------------------
014800131128       //?Elabora.
014900131128       //--------------------------------------------------------------
015000131128       BEGSR  Elabora;
015100131128
015200140117         wAggiorna = *off;
015300131128         wEoFf1 = *off;
015400140116         wDelete = *off;
015500140116         wNoficau = *off;
015600131128         wScrivi = *off;
015700131128
015800131128       //?Leggo ultimo rcd telefonata a parità di oggetto/distinta/aut
015900131128         exec sql
016000131128         DECLARE  f1 cursor FOR
016100131128         SELECT   rrn(ficau00f), ficau00f.*
016200131128         FROM     ficau00f
016300131128         WHERE    CAUtor = :IX3tpdoc and CAUogg = :IX3iddoc and
016400131128                  CAUfgs = :IX3fgs and CAUnfv = :IX3nfv and
016500131128                  CAUpdr = :IX3pdr
016600131128         ORDER BY CAUtor, CAUogg, CAUfgs, CAUnfv, CAUpdr, CAUdtoins desc;
016700131128
016800131128         exec sql OPEN f1;
016900131128
017000131128         IF sqlcode < 0;
017100131128           wEoFf1 = *on;
017200131128         ENDIF;
017300131128
017400131128         DOW not wEoFf1;
017500131128
017600131128           exec sql
017700131128           FETCH next from f1 into :wRrn, :ficauds;
017800131128
017900131128       //?fine file o errore sql esco
018000131128           IF  sqlcod = 100 or sqlcod < 0;
018100131128             wEoFf1 = *on;
018200131128             leave;
018300131128           ENDIF;
018400131128
018500140116       //?Tipo documento passato BOLLA
018600140117           IF  IX3tpdoc = 'C';
018700131128       //?Verifico se la telefonata è già stata fatta
018800131128           IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
018900131128             wScrivi = *on;
019000131128           ENDIF;
019100140116           ENDIF;
019200140116
019300140116       //?Tipo documento passato ORM
019400140117           IF  IX3tpdoc = 'R';
019500140116         //?Controllo il comando ricevuto
019600140116             exsr ctrCMD;
019700140116           ENDIF;
019800131128
019900131128           wEoFf1 = *on;
020000131128           leave;
020100131128
020200131128         ENDDO;
020300131128
020400131128       //?chiudo il cursore
020500131128         exec sql CLOSE f1;
020600131128
020700131128       //?Scrivo/Aggiorno file Telefonate
020800131128         exsr  Telefonate;
020900131128
021000131128       ENDSR;
021100140116
021200140116       //--------------------------------------------------------------
021300140116       //?Controllo comando ricevuto x ORM.                            ?
021400140116       //--------------------------------------------------------------
021500140116       BEGSR  ctrCMD;
021600140116
021700140116         SELECT;
021800140116
021900140117       //?==> INSERITO
022000140117           WHEN  IX3cmd = 'I';
022100140116         //?Se la telefonata che sto leggendo
022200140116         //?è INSERITO o VARIATO
022300140116         //?NON devo scrivere/aggiornare FICAU
022400140116             IF  wCAUcmd = 'I' or wCAUcmd = 'V';
022500140117               wNoficau = *on;
022600140116             ENDIF;
022700140116
022800140116         //?Se la telefonata che sto leggendo
022900140117         //?è ANNULLATO
023000140117             IF  wCAUcmd = 'A';
023100140117           //?se è stata effettuta
023200140117           //?devo scrivere FICAU
023300140117               IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
023400140117                 wScrivi = *on;
023500140117               ENDIF;
023600140117           //?se NON è stata effettuta
023700140117           //?cancello la telefonata ANNULLATO e NON devo scrivere/aggiornare FICAU
023800140121               IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
023900140117                 wDelete = *on;
024000140117               ENDIF;
024100140116             ENDIF;
024200140116
024300140117       //?==> VARIATO
024400140116           WHEN  IX3cmd = 'V';
024500140117         //?Se la telefonata che sto leggendo
024600140117         //?é ANNULLATO
024700140117         //?NON devo scrivere/aggiornare FICAU
024800140117             IF  wCAUcmd = 'A';
024900140117               wNoficau = *on;
025000140117             ENDIF;
025100140117         //?Se la telefonata che sto leggendo
025200140117         //?è stata effettuta
025300140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
025400140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
025500140117               wScrivi = *on;
025600140117             ENDIF;
025700140117         //?Se la telefonata che sto leggendo
025800140117         //?NON è stata effettuta
025900140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
026000140117           //?se é INSERITO
026100140130           //?devo aggiornare FICAU
026200140117               IF  wCAUcmd = 'I';
026300140130                 wAggiorna = *on;
026400140117               ENDIF;
026500140117           //?se é VARIATO
026600140117           //?devo aggiornare FICAU
026700140117               IF  wCAUcmd = 'V';
026800140117                 wAggiorna = *on;
026900140117               ENDIF;
027000140117             ENDIF;
027100140117
027200140117       //?==> ANNULLATO
027300140117           WHEN  IX3cmd = 'A';
027400140117         //?Se la telefonata che sto leggendo
027500140117         //?é ANNULLATO
027600140117         //?NON devo scrivere/aggiornare FICAU
027700140117             IF  wCAUcmd = 'A';
027800140117               wNoficau = *on;
027900140117             ENDIF;
028000140117         //?Se la telefonata che sto leggendo
028100140117         //?è stata effettuta
028200140117         //?devo scrivere FICAU sia per INSERITO che per VARIATO
028300140117             IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
028400140117               wScrivi = *on;
028500140117             ENDIF;
028600140117         //?Se la telefonata che sto leggendo
028700140117         //?NON è stata effettuta
028800140121             IF  wCAUflt = 'T' and wCAUdtotel = *blanks;
028900140117           //?se é INSERITO
029000140117           //?cancello la telefonata INSERITO e NON devo scrivere/aggiornare FICAU
029100140117               IF  wCAUcmd = 'I';
029200140117                 wDelete = *on;
029300140117               ENDIF;
029400140117           //?se é VARIATO
029500140117           //?cancello la telefonata VARIATO e devo scrivere FICAU
029600140117               IF  wCAUcmd = 'V';
029700140117                 wDelete = *on;
029800140117                 wScrivi = *on;
029900140117               ENDIF;
030000140117             ENDIF;
030100140117
030200140117         ENDSL;
030300140116
030400140116       ENDSR;
030500131128
030600131128       //--------------------------------------------------------------
030700131128       //?Scrivo/Aggiorno file telefonate.                             ?
030800131128       //--------------------------------------------------------------
030900131128       BEGSR  Telefonate;
031000131129
031100131128         wVaria = *off;
031200140117
031300140117       //?Se richiamato da ORM e ANNULLATO o VARIATO e non ho trovato nessuna telefonata
031400140117       //?esco senza scrivere/aggiornare FICAU
031500140117         IF  wRrn = 0 and IX3tpdoc = 'R' and
031600140117            (IX3cmd = 'V' or IX3cmd = 'A');
031700140117           leavesr;
031800140117         ENDIF;
031900140117
032000140117       //?Se NON devo scrivere/aggiornare
032100140117       //?esco
032200140117         IF  wNoficau;
032300140117           leavesr;
032400140117         ENDIF;
032500140117
032600140117       //?Cancello la telefonata trovata
032700140117         IF  wDelete;
032800140117           chain wRrn FICAU00F;
032900140117           IF  %found(FICAU00F);
033000140117             delete  FICAU000;
033100140117           ENDIF;
033200140117         ENDIF;
033300140117
033400131129       //?Se devo scrivere un nuovo rcd perchè non ho trovato nessuna telefonata
033500131129       //?precedente imposto i campi dalla DS di passaggio dati
033600131129         IF  wRrn = 0;
033700131129           clear FICAU000;
033800131129           CAUtor = IX3tpdoc;
033900131129           CAUogg = IX3iddoc;
034000131129           CAUfgs = IX3fgs;
034100131129           CAUnfv = IX3nfv;
034200131129           CAUdfv = IX3dfv;
034300131129           CAUpdr = IX3pdr;
034400131129           CAUflt = 'T';
034500131129         //?Se passati dalla DS di passaggio dati
034600131129         //?imposto il comando
034700131129           IF  IX3cmd <> *blanks;
034800131129             CAUcmd = IX3cmd;
034900131129             exsr tab_CMD;
035000131129             CAUdescmd = §CMDdes;
035100131129           ENDIF;
035200131129         //?imposto la variazione
035300131129           IF  IX3var <> *blanks;
035400131206               FlgVar = IX3var;
035500131206               xx = 1;
035600131206               FOR xx by 1 to %elem($var);
035700131206                  IF $var(xx) <> *blanks;
035800131206                     eval wFlgVar= $var(xx) ;
035900131206                     exsr tab_TCV;
036000140122                     IF  CAUdesva = *blanks;
036100140122                       CAUdesva = §TCVdesb ;
036200140122                     ELSE;
036300140122                       CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
036400140122                    ENDIF;
036500131206                  ENDIF;
036600131206               ENDFOR;
036700131206
036800131129               CAUtva = IX3var;
036900131206           ENDIF;
037000131129         ENDIF;
037100131128
037200131129       //?Se devo scrivere un nuovo rcd perchè la telefonata trovata è già stata
037300131129       //?effettuata imposto i campi dal rcd trovato
037400131128         IF  wScrivi;
037500131128           clear FICAU000;
037600131129           CAUtor = wCAUtor;
037700131129           CAUogg = wCAUogg;
037800131129           CAUfgs = wCAUfgs;
037900131129           CAUnfv = wCAUnfv;
038000131129           CAUdfv = wCAUdfv;
038100131129           CAUpdr = wCAUpdr;
038200131129           CAUflt = wCAUflt;
038300131129         //?Se passato aggiorno il comando, solo se variato
038400131129           IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
038500131129             CAUcmd = IX3cmd;
038600131129             exsr tab_CMD;
038700131129             CAUdescmd = §CMDdes;
038800131129           ENDIF;
038900140117         //?Se passato il comando ed è lo stesso delle telefonata che sto leggendo
039000140117         //?ed è un richiamo da ORM
039100140117         //?imposto il comando della telefonata che sto leggendo
039200140117           IF  IX3cmd <> *blanks and IX3cmd = wCAUcmd and
039300140117               IX3tpdoc = 'R';
039400140117             CAUcmd = wCAUcmd;
039500140117             CAUdescmd = wCAUdescmd;
039600140117           ENDIF;
039700131129         //?Se non passato riporto il comando del rcd trovato
039800131129           IF  IX3cmd = *blanks;
039900131129             CAUcmd = wCAUcmd;
040000131129             CAUdescmd = wCAUdescmd;
040100131129           ENDIF;
040200131206         //?Se passata aggiorno la variazione, ma solo se non è già presente
040300131206           IF  IX3var <> *blanks;
040400131206               FlgVar = IX3var;
040500131206               xx = 1;
040600131206               FOR xx by 1 to %elem($var);
040700131206                  IF $var(xx) <> *blanks;
040800131206                   IF  %scan($var(xx):CAUtva) = 0;
040900131206                       eval wFlgVar= $var(xx) ;
041000131206                       exsr tab_TCV;
041100131206                       CAUtva = %trim(CAUtva) + wFlgVar;
041200140117                       if CAUdesva = *blanks ;
041300131211                          CAUdesva = §TCVdesb ;
041400131211                       Else ;
041500131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
041600131211                       ENDIF;
041700131206                   ENDIF;
041800131206                  ENDIF;
041900131206               ENDFOR;
042000131206           ENDIF;
042100131206         ENDIF;
042200131129
042300131129       //?Scrivo il rcd
042400131129         IF  wScrivi or wRrn = 0;
042500131129           CAUdtoins = %char(%timestamp:*iso0);
042600140317           CAUpruins = SDSjobUser;
042700140314       //?Se sto scrivendo un record relativo alle spedizioni e la filiale distinta è
042800140314       //?partita con il nuovo PDA DECOFI816F17 scrivo la telefonata come eseguita
042900140423       //?solo se in una distinta con PDA
043000140314           If IX3tpdoc = 'C' and
043100140516               (sk_pda(1) = '999' or
043200140516               %lookup(%editc(ix3fgs:'X'):sk_PDA)>0);
043300140423               clear DDSTflr ;
043400140423               chain (IX3fgs:4:IX3dfv:IX3nfv) FIDST08L;
043500140423               IF  %found(FIDST08L);
043600140423                   dDSTflr = DSTflr;
043700140423               ENDIF;
043800140423       //?Verifico se a PDA se a PDA in TEST o NO PDA
043900140423               IF (DSTpda = 'C' or DSTpda = 'E') and
044000140423                  (§dsttstpda = ' ' or §dsttstpda = 'O');
044100140423                   CAUdtotel = %char(%timestamp:*iso0);
044200140423                   CAUprutel = SDSjobUser;
044300140423                   CAUterm   = SDSjobName;
044400140423                   CAUesito  = c_Eseguita;
044500140423                   CAUesec   = 'A';
044600140423               Endif;
044700140314           Endif;
044800140314
044900131129           write FICAU000;
045000131129         ENDIF;
045100131128
045200131129       //?Se devo aggiornare un rcd già presente perchè la telefonata non è ancora
045300131129       //?stata fatta
045400140117       //?ed è un richiamo da BOLLE
045500140117         IF  (not wScrivi and wRrn > 0 and IX3tpdoc = 'C') or
045600140117       //?è un richiamo da ORM e devo aggiornare
045700140117             (wAggiorna and IX3tpdoc = 'R');
045800131129           chain wRrn FICAU00F;
045900131129           IF  %found(FICAU00F);
046000131129           //?Se passato aggiorno il comando, solo se variato
046100131129             IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
046200140130             //?Se il comando che ho ricevuto è 'VARIATO' e sto leggendo un
046300140130             //?rcd 'INSERITO' non devo aggiornare il comando
046400140130               IF  IX3cmd = 'V' and wCAUcmd = 'I';
046500140130               ELSE;
046600131129               CAUcmd = IX3cmd;
046700131129               exsr tab_CMD;
046800131129               CAUdescmd = §CMDdes;
046900131129               wVaria = *on;
047000140130               ENDIF;
047100131129             ENDIF;
047200131129           //?Se passata aggiorno la variazione, ma solo se non è già presente
047300131129             IF  IX3var <> *blanks;
047400131206               FlgVar = IX3var;
047500131206               xx = 1;
047600131206               FOR xx by 1 to %elem($var);
047700131206                IF $var(xx) <> *blanks;
047800131206                   IF  %scan($var(xx):CAUtva) = 0;
047900131206                       eval wFlgVar= $var(xx) ;
048000131206                       exsr tab_TCV;
048100131206                       CAUtva = %trim(CAUtva) + wFlgVar;
048200131211                       If CAUdesva = *blanks ;
048300131211                          CAUdesva = §TCVdesb;
048400131211                       Else ;
048500131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
048600131211                       ENDIF ;
048700131206                       wVaria = *on;
048800131206                   ENDIF;
048900131206                ENDIF;
049000131206               ENDFOR;
049100131129             ENDIF;
049200131129             IF  wVaria;
049300131129               CAUdtoins = %char(%timestamp:*iso0);
049400140317               CAUpruins = SDSjobUser;
049500131129             ENDIF;
049600131129             update FICAU000;
049700131129           ENDIF;
049800131129         ENDIF;
049900131128
050000131128       ENDSR;
050100131128
050200131128       //--------------------------------------------------------------
050300131128       //?Aggancio tabella CMD.                                        ?
050400131128       //--------------------------------------------------------------
050500131128       BEGSR  tab_CMD;
050600131128
050700131128         clear dCMD;
050800131128         clear TIBS02DS;
050900131128         T02mod = 'C';
051000131128         T02sif = knsif;
051100131128         T02cod = 'CMD';
051200131128         T02ke1 = CAUcmd;
051300131128         TNTBE_RicercaControllo (kpjba : tibs02ds);
051400131128         IF  T02err = *blanks;
051500131128           dCMD = T02uni;
051600131128         ENDIF;
051700131128
051800131128       ENDSR;
051900131129
052000131129       //--------------------------------------------------------------
052100131129       //?Aggancio tabella TCV.                                   ?
052200131129       //--------------------------------------------------------------
052300131129       BEGSR  tab_TCV;
052400131129
052500131129         clear dTCV;
052600131129         clear TIBS02DS;
052700131129         T02mod = 'C';
052800131129         T02sif = knsif;
052900131129         T02cod = 'TCV';
053000131129         T02ke1 = wFlgVar;
053100131129         TNTBE_RicercaControllo (kpjba : tibs02ds);
053200131129         IF  T02err = *blanks;
053300131129           dTCV = T02uni;
053400131129         ENDIF;
053500131129
053600131129       ENDSR;
053700131128
053800131128       //--------------------------------------------------------------
053900131128       //?Operazioni finali.                                           ?
054000131128       //--------------------------------------------------------------
054100131128       BEGSR  RoutEnd;
054200131128
054300131128         return;
054400131128
054500131128       ENDSR;
054600131128
054700131128      /end-free
