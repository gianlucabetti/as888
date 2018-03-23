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
006400131206     d xx              s              2  0
006500131206
006600131206       //?ds di comodo
006700131206     d FLGvar          ds            10    inz
006800131206     d  $var                          1    inz  dim(10)
006900131128
007000131128       //---------------------------------------------------------------
007100131128       //?Definizione procedure esterne.
007200131128       //---------------------------------------------------------------
007300131128
007400131128       //---------------------------------------------------------------
007500131128       //?Definizione prototipi.
007600131128       //---------------------------------------------------------------
007700131128
007800131128      /copy gaitrasrc/srcprotopr,tibs02r
007900131128
008000131128       //---------------------------------------------------------------
008100131128       //?Definizione key-list.
008200131128       //---------------------------------------------------------------
008300131128
008400131128       //---------------------------------------------------------------
008500131128       //?Main.
008600131128       //---------------------------------------------------------------
008700131128
008800131128     c     *Entry        plist
008900131128     c                   parm                    KPJBA
009000131128     c                   parm                    FNLRX3DS
009100131128
009200131128      /free
009300131128
009400131128       //?Operazioni iniziali?
009500131128       exsr RoutInz;
009600131128
009700131128       //?Elabora?
009800131128       exsr Elabora;
009900131128
010000131128       //?Operazioni finali?
010100131128       exsr RoutEnd;
010200131128
010300131128       //--------------------------------------------------------------
010400131128       //?Operazioni iniziali.                                         ?
010500131128       //--------------------------------------------------------------
010600131128       BEGSR  RoutInz;
010700131128
010800131128         *inLR = *on;
010900131128
011000131128         OX3err = *off;
011100131128         clear OX3msg;
011200131128
011300131128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011400131128
011500131128       ENDSR;
011600131128
011700131128       //--------------------------------------------------------------
011800131128       //?Elabora.
011900131128       //--------------------------------------------------------------
012000131128       BEGSR  Elabora;
012100131128
012200131128         wEoFf1 = *off;
012300131128         wScrivi = *off;
012400131128
012500131128       //?Leggo ultimo rcd telefonata a parità di oggetto/distinta/aut
012600131128         exec sql
012700131128         DECLARE  f1 cursor FOR
012800131128         SELECT   rrn(ficau00f), ficau00f.*
012900131128         FROM     ficau00f
013000131128         WHERE    CAUtor = :IX3tpdoc and CAUogg = :IX3iddoc and
013100131128                  CAUfgs = :IX3fgs and CAUnfv = :IX3nfv and
013200131128                  CAUpdr = :IX3pdr
013300131128         ORDER BY CAUtor, CAUogg, CAUfgs, CAUnfv, CAUpdr, CAUdtoins desc;
013400131128
013500131128         exec sql OPEN f1;
013600131128
013700131128         IF sqlcode < 0;
013800131128           wEoFf1 = *on;
013900131128         ENDIF;
014000131128
014100131128         DOW not wEoFf1;
014200131128
014300131128           exec sql
014400131128           FETCH next from f1 into :wRrn, :ficauds;
014500131128
014600131128       //?fine file o errore sql esco
014700131128           IF  sqlcod = 100 or sqlcod < 0;
014800131128             wEoFf1 = *on;
014900131128             leave;
015000131128           ENDIF;
015100131128
015200131128       //?Verifico se la telefonata è già stata fatta
015300131128           IF  wCAUflt = 'T' and wCAUdtotel <> *blanks;
015400131128             wScrivi = *on;
015500131128           ENDIF;
015600131128
015700131128           wEoFf1 = *on;
015800131128           leave;
015900131128
016000131128         ENDDO;
016100131128
016200131128       //?chiudo il cursore
016300131128         exec sql CLOSE f1;
016400131128
016500131128       //?Scrivo/Aggiorno file Telefonate
016600131128         exsr  Telefonate;
016700131128
016800131128       ENDSR;
016900131128
017000131128       //--------------------------------------------------------------
017100131128       //?Scrivo/Aggiorno file telefonate.                             ?
017200131128       //--------------------------------------------------------------
017300131128       BEGSR  Telefonate;
017400131129
017500131128         wVaria = *off;
017600131129
017700131129       //?Se devo scrivere un nuovo rcd perchè non ho trovato nessuna telefonata
017800131129       //?precedente imposto i campi dalla DS di passaggio dati
017900131129         IF  wRrn = 0;
018000131129           clear FICAU000;
018100131129           CAUtor = IX3tpdoc;
018200131129           CAUogg = IX3iddoc;
018300131129           CAUfgs = IX3fgs;
018400131129           CAUnfv = IX3nfv;
018500131129           CAUdfv = IX3dfv;
018600131129           CAUpdr = IX3pdr;
018700131129           CAUflt = 'T';
018800131129         //?Se passati dalla DS di passaggio dati
018900131129         //?imposto il comando
019000131129           IF  IX3cmd <> *blanks;
019100131129             CAUcmd = IX3cmd;
019200131129             exsr tab_CMD;
019300131129             CAUdescmd = §CMDdes;
019400131129           ENDIF;
019500131129         //?imposto la variazione
019600131129           IF  IX3var <> *blanks;
019700131206               FlgVar = IX3var;
019800131206               xx = 1;
019900131206               FOR xx by 1 to %elem($var);
020000131206                  IF $var(xx) <> *blanks;
020100131206                     eval wFlgVar= $var(xx) ;
020200131206                     exsr tab_TCV;
020300131206                     CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
020400131206                  ENDIF;
020500131206               ENDFOR;
020600131206
020700131129               CAUtva = IX3var;
020800131206           ENDIF;
020900131129         ENDIF;
021000131128
021100131129       //?Se devo scrivere un nuovo rcd perchè la telefonata trovata è già stata
021200131129       //?effettuata imposto i campi dal rcd trovato
021300131128         IF  wScrivi;
021400131128           clear FICAU000;
021500131129           CAUtor = wCAUtor;
021600131129           CAUogg = wCAUogg;
021700131129           CAUfgs = wCAUfgs;
021800131129           CAUnfv = wCAUnfv;
021900131129           CAUdfv = wCAUdfv;
022000131129           CAUpdr = wCAUpdr;
022100131129           CAUflt = wCAUflt;
022200131129         //?Se passato aggiorno il comando, solo se variato
022300131129           IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
022400131129             CAUcmd = IX3cmd;
022500131129             exsr tab_CMD;
022600131129             CAUdescmd = §CMDdes;
022700131129           ENDIF;
022800131129         //?Se non passato riporto il comando del rcd trovato
022900131129           IF  IX3cmd = *blanks;
023000131129             CAUcmd = wCAUcmd;
023100131129             CAUdescmd = wCAUdescmd;
023200131129           ENDIF;
023300131206         //?Se passata aggiorno la variazione, ma solo se non è già presente
023400131206           IF  IX3var <> *blanks;
023500131206               FlgVar = IX3var;
023600131206               xx = 1;
023700131206               FOR xx by 1 to %elem($var);
023800131206                  IF $var(xx) <> *blanks;
023900131206                   IF  %scan($var(xx):CAUtva) = 0;
024000131206                       eval wFlgVar= $var(xx) ;
024100131206                       exsr tab_TCV;
024200131206                       CAUtva = %trim(CAUtva) + wFlgVar;
024300131211                       if CCAUdesva = *blanks ;
024400131211                          CAUdesva = §TCVdesb ;
024500131211                       Else ;
024600131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
024700131211                       ENDIF;
024800131206                   ENDIF;
024900131206                  ENDIF;
025000131206               ENDFOR;
025100131206           ENDIF;
025200131206         ENDIF;
025300131129
025400131129       //?Scrivo il rcd
025500131129         IF  wScrivi or wRrn = 0;
025600131129           CAUdtoins = %char(%timestamp:*iso0);
025700131129           CAUpruins = knmus;
025800131129           write FICAU000;
025900131129         ENDIF;
026000131128
026100131129       //?Se devo aggiornare un rcd già presente perchè la telefonata non è ancora
026200131129       //?stata fatta
026300131129         IF  not wScrivi and wRrn > 0;
026400131129           chain wRrn FICAU00F;
026500131129           IF  %found(FICAU00F);
026600131129           //?Se passato aggiorno il comando, solo se variato
026700131129             IF  IX3cmd <> *blanks and IX3cmd <> wCAUcmd;
026800131129               CAUcmd = IX3cmd;
026900131129               exsr tab_CMD;
027000131129               CAUdescmd = §CMDdes;
027100131129               wVaria = *on;
027200131129             ENDIF;
027300131129           //?Se passata aggiorno la variazione, ma solo se non è già presente
027400131129             IF  IX3var <> *blanks;
027500131206               FlgVar = IX3var;
027600131206               xx = 1;
027700131206               FOR xx by 1 to %elem($var);
027800131206                IF $var(xx) <> *blanks;
027900131206                   IF  %scan($var(xx):CAUtva) = 0;
028000131206                       eval wFlgVar= $var(xx) ;
028100131206                       exsr tab_TCV;
028200131206                       CAUtva = %trim(CAUtva) + wFlgVar;
028300131211                       If CAUdesva = *blanks ;
028400131211                          CAUdesva = §TCVdesb;
028500131211                       Else ;
028600131211                          CAUdesva = %trim(CAUdesva) + '+' + §TCVdesb;
028700131211                       ENDIF ;
028800131206                       wVaria = *on;
028900131206                   ENDIF;
029000131206                ENDIF;
029100131206               ENDFOR;
029200131129             ENDIF;
029300131129             IF  wVaria;
029400131129               CAUdtoins = %char(%timestamp:*iso0);
029500131129               CAUpruins = knmus;
029600131129             ENDIF;
029700131129             update FICAU000;
029800131129           ENDIF;
029900131129         ENDIF;
030000131128
030100131128       ENDSR;
030200131128
030300131128       //--------------------------------------------------------------
030400131128       //?Aggancio tabella CMD.                                        ?
030500131128       //--------------------------------------------------------------
030600131128       BEGSR  tab_CMD;
030700131128
030800131128         clear dCMD;
030900131128         clear TIBS02DS;
031000131128         T02mod = 'C';
031100131128         T02sif = knsif;
031200131128         T02cod = 'CMD';
031300131128         T02ke1 = CAUcmd;
031400131128         TNTBE_RicercaControllo (kpjba : tibs02ds);
031500131128         IF  T02err = *blanks;
031600131128           dCMD = T02uni;
031700131128         ENDIF;
031800131128
031900131128       ENDSR;
032000131129
032100131129       //--------------------------------------------------------------
032200131129       //?Aggancio tabella TCV.                                   ?
032300131129       //--------------------------------------------------------------
032400131129       BEGSR  tab_TCV;
032500131129
032600131129         clear dTCV;
032700131129         clear TIBS02DS;
032800131129         T02mod = 'C';
032900131129         T02sif = knsif;
033000131129         T02cod = 'TCV';
033100131129         T02ke1 = wFlgVar;
033200131129         TNTBE_RicercaControllo (kpjba : tibs02ds);
033300131129         IF  T02err = *blanks;
033400131129           dTCV = T02uni;
033500131129         ENDIF;
033600131129
033700131129       ENDSR;
033800131128
033900131128       //--------------------------------------------------------------
034000131128       //?Operazioni finali.                                           ?
034100131128       //--------------------------------------------------------------
034200131128       BEGSR  RoutEnd;
034300131128
034400131128         return;
034500131128
034600131128       ENDSR;
034700131128
034800131128      /end-free
