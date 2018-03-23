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
005800131128     d wScrivi         s               n   inz(*off)
005900131128     d wVaria          s               n   inz(*off)
006000131128
006100131128       //?Campi di comodo
006200131129     d wFlgVar         s                   like(IX3var)
006300131128     d wRrn            s              5i 0 inz
006400131128
006500131128       //---------------------------------------------------------------
006600131128       //?Definizione procedure esterne.
006700131128       //---------------------------------------------------------------
006800131128
006900131128       //---------------------------------------------------------------
007000131128       //?Definizione prototipi.
007100131128       //---------------------------------------------------------------
007200131128
007300131128      /copy gaitrasrc/srcprotopr,tibs02r
007400131128
007500131128       //---------------------------------------------------------------
007600131128       //?Definizione key-list.
007700131128       //---------------------------------------------------------------
007800131128
007900131128       //---------------------------------------------------------------
008000131128       //?Main.
008100131128       //---------------------------------------------------------------
008200131128
008300131128     c     *Entry        plist
008400131128     c                   parm                    KPJBA
008500131128     c                   parm                    FNLRX3DS
008600131128
008700131128      /free
008800131128
008900131128       //?Operazioni iniziali?
009000131128       exsr RoutInz;
009100131128
009200131128       //?Elabora?
009300131128       exsr Elabora;
009400131128
009500131128       //?Operazioni finali?
009600131128       exsr RoutEnd;
009700131128
009800131128       //--------------------------------------------------------------
009900131128       //?Operazioni iniziali.                                         ?
010000131128       //--------------------------------------------------------------
010100131128       BEGSR  RoutInz;
010200131128
010300131128         *inLR = *on;
010400131128
010500131128         OX3err = *off;
010600131128         clear OX3msg;
010700131128
010800131128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010900131128
011000131128       ENDSR;
011100131128
011200131128       //--------------------------------------------------------------
011300131128       //?Elabora.
011400131128       //--------------------------------------------------------------
011500131128       BEGSR  Elabora;
011600131128
011700131128         wEoFf1 = *off;
011800131128         wScrivi = *off;
011900131128
012000131128       //?Leggo ultimo rcd telefonata a parità di oggetto/distinta/aut
012100131128         exec sql
012200131128         DECLARE  f1 cursor FOR
012300131128         SELECT   rrn(ficau00f), ficau00f.*
012400131128         FROM     ficau00f
012500131128         WHERE    CAUtor = :IX3tpdoc and CAUogg = :IX3iddoc and
012600131128                  CAUfgs = :IX3fgs and CAUnfv = :IX3nfv and
012700131128                  CAUpdr = :IX3pdr
012800131128         ORDER BY CAUtor, CAUogg, CAUfgs, CAUnfv, CAUpdr, CAUdtoins desc;
012900131128
013000131128         exec sql OPEN f1;
013100131128
013200131128         IF sqlcode < 0;
013300131128           wEoFf1 = *on;
013400131128         ENDIF;
013500131128
013600131128         DOW not wEoFf1;
013700131128
013800131128           exec sql
013900131128           FETCH next from f1 into :wRrn, :ficauds;
014000131128
014100131128       //?fine file o errore sql esco
014200131128           IF  sqlcod = 100 or sqlcod < 0;
014300131128             wEoFf1 = *on;
014400131128             leave;
014500131128           ENDIF;
014600131128
014700131128       //?Verifico se la telefonata è già stata fatta
014800131128           IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
014900131128             wScrivi = *on;
015000131128           ENDIF;
015100131128
015200131128           wEoFf1 = *on;
015300131128           leave;
015400131128
015500131128         ENDDO;
015600131128
015700131128       //?chiudo il cursore
015800131128         exec sql CLOSE f1;
015900131128
016000131128       //?Scrivo/Aggiorno file Telefonate
016100131128         exsr  Telefonate;
016200131128
016300131128       ENDSR;
016400131128
016500131128       //--------------------------------------------------------------
016600131128       //?Scrivo/Aggiorno file telefonate.                             ?
016700131128       //--------------------------------------------------------------
016800131128       BEGSR  Telefonate;
016900131129
017000131128         wVaria = *off;
017100131129
017200131129       //?Se devo scrivere un nuovo rcd perchè non ho trovato nessuna telefonata
017300131129       //?precedente imposto i campi dalla DS di passaggio dati
017400131129         IF  wRrn = 0;
017500131129           clear FICAU000;
017600131129           CAUtor = IX3tpdoc;
017700131129           CAUogg = IX3iddoc;
017800131129           CAUfgs = IX3fgs;
017900131129           CAUnfv = IX3nfv;
018000131129           CAUdfv = IX3dfv;
018100131129           CAUpdr = IX3pdr;
018200131129           CAUflt = 'T';
018300131129         //?Se passati dalla DS di passaggio dati
018400131129         //?imposto il comando
018500131129           IF  IX3cmd <> *blanks;
018600131129             CAUcmd = IX3cmd;
018700131129             exsr tab_CMD;
018800131129             CAUdescmd = §CMDdes;
018900131129           ENDIF;
019000131129         //?imposto che sono state immesse/variate le note AUT
019100131129           IF  IX3note = 'S';
019200131129             wFlgVar = 'N';
019300131129             exsr tab_TCV;
019400131129             CAUtva = wFlgVar;
019500131129             CAUdesva = §TCVdesb;
019600131129           ENDIF;
019700131129         //?imposto la variazione
019800131129           IF  IX3var <> *blanks;
019900131129             wFlgVar = IX3var;
020000131129             exsr tab_TCV;
020100131129           //?se ho anche il flag delle note vado in aggiunta sul campo delle variazioni
020200131129             IF  IX3note = 'S';
020300131129               CAUtva = %trim(CAUtva) + wFlgVar;
020400131129               CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
020500131129             ELSE;
020600131129           //?se ho solo la variazione imposto solo questa
020700131129               CAUtva = IX3var;
020800131129               CAUdesva = §TCVdesb;
020900131129             ENDIF;
021000131129           ENDIF;
021100131129         ENDIF;
021200131128
021300131129       //?Se devo scrivere un nuovo rcd perchè la telefonata trovata è già stata
021400131129       //?effettuata imposto i campi dal rcd trovato
021500131128         IF  wScrivi;
021600131128           clear FICAU000;
021700131129           CAUtor = wCAUtor;
021800131129           CAUogg = wCAUogg;
021900131129           CAUfgs = wCAUfgs;
022000131129           CAUnfv = wCAUnfv;
022100131129           CAUdfv = wCAUdfv;
022200131129           CAUpdr = wCAUpdr;
022300131129           CAUflt = wCAUflt;
022400131129         //?Se passato aggiorno il comando, solo se variato
022500131129           IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
022600131129             CAUcmd = IX3cmd;
022700131129             exsr tab_CMD;
022800131129             CAUdescmd = §CMDdes;
022900131129           ENDIF;
023000131129         //?Se non passato riporto il comando del rcd trovato
023100131129           IF  IX3cmd = *blanks;
023200131129             CAUcmd = wCAUcmd;
023300131129             CAUdescmd = wCAUdescmd;
023400131129           ENDIF;
023500131129         //?Se passato aggiorno che sono state immesse/variate le note AUT
023600131129         //?ma solo se non è già presente
023700131129           IF  IX3note = 'S';
023800131129             wFlgVar = 'N';
023900131129             exsr tab_TCV;
024000131129             IF  %scan(wFlgVar:wCAUtva) = 0;
024100131129               CAUtva = %trim(wCAUtva) + wFlgVar;
024200131129               CAUdesva = %trim(wCAUdesva) + '+' + §TCVdesb;
024300131129             //?se le note sono già state registrate
024400131129             //?riporto le variazioni già presenti sul rcd
024500131129             ELSE;
024600131129               CAUtva = wCAUtva;
024700131129               CAUdesva = wCAUdesva;
024800131129             ENDIF;
024900131129           ENDIF;
025000131129         //?Se passata aggiorno la variazione, ma solo se non è già presente
025100131129           IF  IX3var <> *blanks;
025200131129             wFlgVar = IX3var;
025300131129             exsr tab_TCV;
025400131129             IF  %scan(wFlgVar:wCAUtva) = 0;
025500131129               CAUtva = %trim(wCAUtva) + wFlgVar;
025600131129               CAUdesva = %trim(wCAUdesva) + '+' + §TCVdesb;
025700131129             //?se la variazione è già stata fatta riporto quelle già presenti sul rcd
025800131129             ELSE;
025900131129               CAUtva = wCAUtva;
026000131129               CAUdesva = wCAUdesva;
026100131129             ENDIF;
026200131129           ENDIF;
026300131129         //?Se non è stata passata la variazione riporto quelle già presenti sul rcd
026400131129           IF  IX3var = *blanks;
026500131129             CAUtva = wCAUtva;
026600131129             CAUdesva = wCAUdesva;
026700131129           ENDIF;
026800131128         ENDIF;
026900131129
027000131129       //?Scrivo il rcd
027100131129         IF  wScrivi or wRrn = 0;
027200131129           CAUdtoins = %char(%timestamp:*iso0);
027300131129           CAUpruins = knmus;
027400131129           write FICAU000;
027500131129         ENDIF;
027600131128
027700131129       //?Se devo aggiornare un rcd già presente perchè la telefonata non è ancora
027800131129       //?stata fatta
027900131129         IF  not wScrivi and wRrn > 0;
028000131129           chain wRrn FICAU00F;
028100131129           IF  %found(FICAU00F);
028200131129           //?Se passato aggiorno il comando, solo se variato
028300131129             IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
028400131129               CAUcmd = IX3cmd;
028500131129               exsr tab_CMD;
028600131129               CAUdescmd = §CMDdes;
028700131129               wVaria = *on;
028800131129             ENDIF;
028900131129           //?Se passato aggiorno che sono state variate/immesse le note AUT
029000131129           //?ma solo se non è già presente
029100131129             IF  IX3note = 'S';
029200131129               wFlgVar = 'N';
029300131129               IF  %scan(wFlgVar:CAUtva) = 0;
029400131129                 exsr tab_TCV;
029500131129                 CAUtva = %trim(CAUtva) + wFlgVar;
029600131129                 CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
029700131129                 wVaria = *on;
029800131129               ENDIF;
029900131129             ENDIF;
030000131129           //?Se passata aggiorno la variazione, ma solo se non è già presente
030100131129             IF  IX3var <> *blanks;
030200131129               wFlgVar = IX3var;
030300131129               IF  %scan(IX3var:CAUtva) = 0;
030400131129                 exsr tab_TCV;
030500131129                 CAUtva = %trim(CAUtva) + wFlgVar;
030600131129                 CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
030700131129                 wVaria = *on;
030800131129               ENDIF;
030900131129             ENDIF;
031000131129             IF  wVaria;
031100131129               CAUdtoins = %char(%timestamp:*iso0);
031200131129               CAUpruins = knmus;
031300131129             ENDIF;
031400131129             update FICAU000;
031500131129           ENDIF;
031600131129         ENDIF;
031700131128
031800131128       ENDSR;
031900131128
032000131128       //--------------------------------------------------------------
032100131128       //?Aggancio tabella CMD.                                        ?
032200131128       //--------------------------------------------------------------
032300131128       BEGSR  tab_CMD;
032400131128
032500131128         clear dCMD;
032600131128         clear TIBS02DS;
032700131128         T02mod = 'C';
032800131128         T02sif = knsif;
032900131128         T02cod = 'CMD';
033000131128         T02ke1 = CAUcmd;
033100131128         TNTBE_RicercaControllo (kpjba : tibs02ds);
033200131128         IF  T02err = *blanks;
033300131128           dCMD = T02uni;
033400131128         ENDIF;
033500131128
033600131128       ENDSR;
033700131129
033800131129       //--------------------------------------------------------------
033900131129       //?Aggancio tabella TCV.                                   ?
034000131129       //--------------------------------------------------------------
034100131129       BEGSR  tab_TCV;
034200131129
034300131129         clear dTCV;
034400131129         clear TIBS02DS;
034500131129         T02mod = 'C';
034600131129         T02sif = knsif;
034700131129         T02cod = 'TCV';
034800131129         T02ke1 = wFlgVar;
034900131129         TNTBE_RicercaControllo (kpjba : tibs02ds);
035000131129         IF  T02err = *blanks;
035100131129           dTCV = T02uni;
035200131129         ENDIF;
035300131129
035400131129       ENDSR;
035500131128
035600131128       //--------------------------------------------------------------
035700131128       //?Operazioni finali.                                           ?
035800131128       //--------------------------------------------------------------
035900131128       BEGSR  RoutEnd;
036000131128
036100131128         return;
036200131128
036300131128       ENDSR;
036400131128
036500131128      /end-free
