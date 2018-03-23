000100110221       //==============================================================
000200120511       // TIS785R // Cancellazione oggetti rispetto a filtri.
000300110221       //==============================================================
000400120511       //
000500120511       // i parametyri di ingresso devono essere:
000600120511       // pInLib: obbligatorio
000700120511       // pInObjType: obbligatorio
000800120511       // pInObj: obbligatorio
000900120511       // pInDate: obbligatorio
001000120511       // pInDateType: può valere
001100120511       //   1 = data di creazione
001200120511       //   2 = data ultimo uso
001300120511       //   se non valorizzato, allora impostato a 1
001400120511       // pInMode: può valere
001500120511       //   1 = gli oggetti con data prima della data di riferimente
001600120511       //   2 = solo gli oggetti con la data di riferimento
001700120511       //   3 = gli oggetti con data successiva alla data di riferimente
001800120511       //   se non valorizzato, allora impostato a 1
001900120518       //
002000120518       // i parametyri di uscita valgono:
002100120518       // pOutError = 1 => pOutErrorD = ERRORE: con i parametri indicati non sono stati trovati
002200120518       //                               oggetti
002300120518       // pOutError = 2 => pOutErrorD = ERRORE: il parametro Libreria è obbligatorio
002400120518       // pOutError = 3 => pOutErrorD = ERRORE: il parametro Tipo oggetto è obbligatorio
002500120518       // pOutError = 4 => pOutErrorD = ERRORE: il parametro Oggetto è obbligatorio
002600120518       // pOutError = 5 => pOutErrorD = ERRORE: il parametro Data è obbligatorio
002700120518       // pOutError = 6 => pOutErrorD = ERRORE: valore errato per il tipo data
002800120518       // pOutError = 7 => pOutErrorD = ERRORE: valore errato per il modo
002900120518       // pOutError = 8 => pOutErrorD = ERRORE: chiamata API <APIName> con errore <CPF>
003000120522       // pOutError = 9 => pOutErrorD = ERRORE: nessun oggetto cancellato ma alcuni oggetti aveva...
003100120522       // pOutError = A => pOutErrorD = ERRORE: eseguita cancellazione ma alcuni oggetti avevano ...
003200120522
003300120522       // file di log
003400120522       // CREATE TABLE GAITRAAZP/WFS7850F (S785FILE CHAR ( 10) NOT NULL WITH
003500120522       // DEFAULT, S785TYPE CHAR ( 7) NOT NULL WITH DEFAULT, S785DTC CHAR (
003600120522       // 8) NOT NULL WITH DEFAULT, S785DTU CHAR ( 8) NOT NULL WITH DEFAULT,
003700120522       // S785TMDLT TIMESTAMP NOT NULL WITH DEFAULT)
003800120522       // RCDFMT WFS78500
003900120522
004000110221       //--------------------------------------------------------------
004100120511       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004200110221       //--------------------------------------------------------------
004300110221
004400110221     /*PRM dbgview(*source)
004500110221     /*END
004600110221
004700110221       //--------------------------------------------------------------
004800120511       // Specifiche di controllo.                                     ?
004900110221       //--------------------------------------------------------------
005000110221
005100110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005200120521     h dftactgrp(*no) actgrp('QILE')
005300110221     h bnddir('UBBNDDIR')
005400110221
005500110221       //--------------------------------------------------------------
005600120511       // Dichiarazione file.                                          ?
005700110221       //--------------------------------------------------------------
005800110221
005900120522     Fwfs7850f  IF A E             DISK
006000110221
006100110221       //--------------------------------------------------------------
006200120511       // Definizione costanti.                                        ?
006300110221       //--------------------------------------------------------------
006400110221
006500110221
006600110221       //--------------------------------------------------------------
006700120511       // Definizione schiere.                                         ?
006800110221       //--------------------------------------------------------------
006900110221
007000110221
007100110221       //--------------------------------------------------------------
007200120511       // Definizione aree dati.                                       ?
007300110221       //--------------------------------------------------------------
007400110221
007500110221
007600110221       //--------------------------------------------------------------
007700120511       // Definizione strutture dati.                                  ?
007800110221       //--------------------------------------------------------------
007900110221
008000120511       // Status ds?
008100110221     d Status         sds
008200120511     d  SDSproc          *proc
008300120511     d  SDSparms         *parms
008400110224     d  SDSdta               191    198
008500110224     d  SDSjob               244    253
008600110221     d  SDSusr               254    263
008700110224     d  SDSjnr               264    269  0
008800170201
008900170201     D Curdate         DS
009000170201     D Curdat                         8S 0
009100170201     D CurTim                         9S 0
009200110221
009300110221       //--------------------------------------------------------------
009400120511       // Definizione variabili globali.                               ?
009500110221       //--------------------------------------------------------------
009600110221
009700120511       // Parametri ricevuti?
009800120511     d pInLib          s             10
009900120511     d pInObjType      s              7
010000120511     d pInObj          s             10
010100120511     d pInDate         s              8s 0
010200120511     d pInDateType     s              1
010300120511     d pInMode         s              1
010400120511     d pOutError       s              2
010500120511     d pOutErrorD      s             70
010600120511
010700120511       // variabili per QUSCRTUS
010800120511     d Crtus01         s             20    inz
010900120511     d Crtus02         s             10    inz
011000120511     d Crtus03         s              9b 0 inz
011100120511     d Crtus04         s              1    inz
011200120511     d Crtus05         s             10    inz
011300120511     d Crtus06         s             50    inz
011400120511     d Crtus07         s             10    inz
011500120511
011600120511       // variabili per QUSLOBJ
011700120511     d Lobj01          s             20    inz
011800120511     d Lobj02          s              8    inz
011900120511     d Lobj03          s             20    inz
012000120511     d Lobj04          s             10    inz
012100120511
012200120511       // variabili per QUSRTVUS
012300120511     d Rtvus01         s             20    inz
012400120511     d Rtvus02         s              9b 0 inz
012500120511     d Rtvus03         s              9b 0 inz
012600120514
012700120514       // appoggio
012800120514     d wAPIName        s             20
012900120514     d Data08A         s              8
013000120511
013100120511       // indici
013200120511     d wrkCount        s              9s 0
013300120511
013400120511       // booleani
013500120514     d wDeletable      s               n
013600120518     d wDeleted        s               n
013700120522     d wSkipped        s               n
013800120511
013900110221       //--------------------------------------------------------------
014000120511       // Definizione prototipi procedure.                             ?
014100110221       //--------------------------------------------------------------
014200110221
014300120511       // Reperimento NETA sistema AS/400 corrente?
014400110221     d currSysNeta     s              8a   inz
014500110221      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014600110224
014700120511       // strutture generiche per API
014800120511      /copy QSYSINC/QRPGLESRC,QUSGEN
014900120511
015000120511       // prototipi per QUS...
015100120511      /copy gaitrasrc/srcprotopr,QUS
015200120511
015300120511       // lista oggetti
015400120511      /copy QSYSINC/QRPGLESRC,QUSLOBJ
015500120511      /copy gaitrasrc/srcProtoPR,QUSLOBJ
015600110221
015700120511       // Parametri API QCAPCMD (Process Commands)?
015800110221     d Qcmd            s           1024    inz  varying
015900110221      /copy qSysInc/qRpgleSrc,QCAPCMD
016000110221      /copy gaitrasrc/srcProtoPR,QCAPCMD
016100120514
016200120514       // Parametri eliminazione oggetti?
016300120514      /copy gaitrasrc/srcProtoPR,QLIDLTO
016400110221
016500120511       // Parametri gestione errori API.?
016600110221      /copy qSysInc/qRpgleSrc,QUSEC
016700120514
016800120514       // Parametri per conversione data da un formato a un altro
016900120514      /copy gaitrasrc/srcprotopr,qwccvtdt
017000110221
017100110221       //--------------------------------------------------------------
017200120511       // Definizione key-list.                                        ?
017300110221       //--------------------------------------------------------------
017400110221
017500110221
017600110221       //==============================================================
017700120511       // M A I N - L I N E                                            ?
017800110221       //==============================================================
017900110221
018000120511       // Parametri
018100110221     c     *Entry        plist
018200120511     c                   parm                    pInLib
018300120511     c                   parm                    pInObjType
018400120511     c                   parm                    pInObj
018500120511     c                   parm                    pInDate
018600120511     c                   parm                    pInDateType
018700120511     c                   parm                    pInMode
018800120511     c                   parm                    pOutError
018900120511     c                   parm                    pOutErrorD
019000110221
019100110221      /free
019200110221
019300120511       // Operazioni iniziali?
019400110221       exsr  sr_RoutInz;
019500110221
019600120511       // Creazione lista oggetti
019700120511       exsr  CrtObjList;
019800110221
019900120511       // Cancella oggetti
020000120511       exsr  DelObj;
020100110221
020200110221       //--------------------------------------------------------------
020300120511       // Operazioni iniziali.                                         ?
020400110221       //--------------------------------------------------------------
020500110221       begsr  sr_RoutInz;
020600110221
020700110221         *inLR = *on;
020800110221
020900120511         // Verifica del sistema AS/400 corrente?(al momento non serve)
021000110221         select;
021100110221           when  UBRTVNETA_Rtv( currSysNeta ) <> *zero;
021200110221             return;
021300120511         //when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
021400120511         //  return;
021500110221         endsl;
021600110224
021700120518         pOutError = '0';
021800120518
021900120511         // ctrl parametri in ingresso
022000120511         exsr CtrlParmIn;
022100110221
022200110221       endsr;
022300110221
022400110221       //--------------------------------------------------------------
022500120511       // Controllo parametri in ingresso                             ?
022600110221       //--------------------------------------------------------------
022700120511       begsr  CtrlParmIn;
022800110221
022900120511         // la libreria è obbligatoria
023000120511         if pInLib = *blank;
023100120511           pOutError = '2';
023200120511           pOutErrorD = 'ERRORE: il parametro Libreria è obbligatorio';
023300120511           exsr sr_RoutEnd;
023400120511         endif;
023500120511
023600120511         // il tipo è obbligatorio
023700120511         if pInObjType = *blank;
023800120511           pOutError = '3';
023900120511           pOutErrorD = 'ERRORE: il parametro Tipo oggetto è obbligatorio';
024000120511           exsr sr_RoutEnd;
024100120511         endif;
024200120511
024300120511         // il nome oggetto (o iniziale o parola chiave *ALL) è obbligatorio
024400120511         if pInObj = *blank;
024500120511           pOutError = '4';
024600120511           pOutErrorD = 'ERRORE: il parametro Oggetto è obbligatorio';
024700120511           exsr sr_RoutEnd;
024800120511         endif;
024900120511
025000120511         // la data è obbligatoria
025100120511         if pInDate = 0;
025200120511           pOutError = '5';
025300120511           pOutErrorD = 'ERRORE: il parametro Data è obbligatorio';
025400120511           exsr sr_RoutEnd;
025500120511         endif;
025600120514
025700120514         // il tipo data se non importato è 1=CRT
025800120514         if pInDateType = *blank;
025900120514           pInDateType = '1';
026000120514         endif;
026100120511
026200120514         // il tipo data può essere o '1' o '2'
026300120514         if pInDateType <> '1' and
026400120514           pInDateType <> '2';
026500120514           pOutError = '6';
026600120514           pOutErrorD = 'ERRORE: valore errato per il tipo data';
026700120514           exsr sr_RoutEnd;
026800120514         endif;
026900120514
027000120514         // il modo di selezione sulla data se non impostato è 1=Prima
027100120514         if pInMode = *blank;
027200120514           pInMode = '1';
027300120514         endif;
027400120514
027500120514         // il modo può essere o '1' o '2' o '3'
027600120514         if pInMode <> '1' and
027700120514           pInMode <> '2' and
027800120514           pInMode <> '3';
027900120514           pOutError = '7';
028000120514           pOutErrorD = 'ERRORE: valore errato per il modo';
028100120514           exsr sr_RoutEnd;
028200120514         endif;
028300120511
028400120511       endsr;
028500120511
028600120511       //--------------------------------------------------------------
028700120511       // Crea la lista degli oggetti
028800120511       //--------------------------------------------------------------
028900120511       begsr  CrtObjList;
029000120511
029100120511         // Creazione dello user space?
029200120511         Crtus01 = SDSproc + 'QTEMP';
029300120511         clear qusec;
029400120511         Qusbprv = %size(Qusec);
029500120514         CreateUserSpace(CrtUs01: CrtUs02: 1024: CrtUs04: '*CHANGE': CrtUs06:
029600120511                   '*YES': Qusec);
029700120514         wAPIName= 'QUSCRTUS';
029800120514         exsr CtrlAPIErr;
029900120511
030000120511         // Riempimmento dello user space con la lista degli oggetti
030100120511         clear Qusec;
030200120511         Qusbprv = %size(Qusec);
030300120511         Lobj03 = pInObj + pInLib;
030400120514         ListObjects(CrtUs01: 'OBJL0600': Lobj03: pInObjType: QUsec);
030500120514         wAPIName= 'QUSLOBJ';
030600120514         exsr CtrlAPIErr;
030700120511
030800120511         // Reperimento del numero di elementi ?
030900120511         //       e della lunghezza di un elemento?
031000120511         clear Qusec;
031100120511         Qusbprv = %size(Qusec);
031200120511         Rtvus03 = %size(Qush0100);
031300120514         RetrieveUserSpace(CrtUs01: 1: RtvUs03: QUsh0100: Qusec);
031400120514         wAPIName= 'QUSRTVUS';
031500120514         exsr CtrlAPIErr;
031600120511
031700120511         // Se non trovo dati lo segnalo al chiamante
031800120511if  1    if Qusnbrle = 0;
031900120511           pOutError = '1';
032000120511           pOutErrorD = 'ERRORE: con i parametri indicati non sono stati +
032100120511                         trovati oggetti';
032200120511           exsr sr_RoutEnd;
032300120511e   1    endif;
032400120511
032500120511         // Impostazione della posizione iniziale del primo elemento?
032600120511         // e della lunghezza?di ogni elemento
032700120511         Rtvus02 = Qusold + 1;
032800120511         Rtvus03 = Qussee;
032900110221
033000110221       endsr;
033100110221
033200110221       //--------------------------------------------------------------
033300120511       // Ciclo di cancellazione degli oggetti
033400110221       //--------------------------------------------------------------
033500120511       begsr  DelObj;
033600120518
033700120518       wDeleted = *off;
033800120522       wSkipped = *off;
033900120511
034000120511       for WrkCount = 1 to Qusnbrle;
034100120511
034200120511         // Reperimento dell'elemento?
034300120511         clear Qusec;
034400120511         Qusbprv = %size(Qusec);
034500120514         RetrieveUserSpace(CrtUs01: RtvUs02: RtvUs03: QUSL0600: Qusec);
034600120514         wAPIName= 'QUSRTVUS';
034700120514         exsr CtrlAPIErr;
034800120511
034900120511         // controllo se l'oggetto ricade tra quelli da eliminare
035000120511         exsr CtrlDeletable;
035100120511
035200120511         // cancello l'oggetto se ricade tra quelli da eliminare
035300120514         if wDeletable = *on;
035400120511           // eseguo cancellazione
035500120514           clear qusec;
035600120514           Qusbprv = %size(Qusec);
035700120514           DltObj(QUSOBJNU04 + pInLib:pInObjType :'*' :'0' :Qusec);
035800120522           // siccome l'API è legata a un singolo oggetto, nel caso di errore non esco
035900120522           // ma passo al prossimo oggetto
036000120522           if QUSEI <> *blank;
036100120522             wSkipped = *on;
036200120522           else;
036300120522             wDeleted = *on;
036400120522             // scrivo il log
036500120522             S785FILE = QUSOBJNU04;
036600120522             S785TYPE = pInObjType;
036700120522             if pInDateType = '1';
036800120522               S785DTC  = Data08A;
036900120522               S785DTU  = *blank;
037000120522             endif;
037100120522             if pInDateType = '2';
037200120522               S785DTC  = *blank;
037300120522               S785DTU  = Data08A;
037400120522             endif;
037500120522             S785TMDLT= %timestamp();
037600120522             write wfs78500;
037700120522           endif;
037800120522         endif;
037900120511
038000120511         // Impostazione della posizione iniziale del prossimo elemento?
038100120511         Rtvus02 = Rtvus02 + Qussee;
038200120511       endfor;
038300120518
038400120518         // Cancellazione dello user space?
038500120511         clear Qusec;
038600120511         Qusbprv = %size(Qusec);
038700120514         DeleteUserSpace(CrtUs01: Qusec);
038800120514         wAPIName= 'QUSDLTUS';
038900120514         exsr CtrlAPIErr;
039000120518
039100120522         // imposto parametri di uscita a seconda dei casi Deleted e Skipped
039200120522         exsr SetUpParmOut;
039300120518
039400120522         // esco
039500120511         exsr sr_RoutEnd;
039600120511
039700120511       endsr;
039800120511
039900120511       //--------------------------------------------------------------
040000120511       // Controllo se oggetto cancellabile
040100120511       //--------------------------------------------------------------
040200120511       begsr  CtrlDeletable;
040300120511
040400120514         wDeletable = *off;
040500120511
040600120522         select;
040700120522         // ctrl su data creazione
040800120522         when pInDateType = '1';
040900120522           clear qusec;
041000120522           Qusbprv = %size(Qusec);
041100120522           // la data in DS è in formato system timestamp (*DTS) e va convertito
041200170201           ConvertDateAndTimeFormat('*DTS': QUSCDT06: '*YYMD': Curdate:
041300120522                                    Qusec);
041400120522           // siccome l'API è legata a un singolo oggetto, nel caso di errore non esco
041500120522           // ma passo al prossimo oggetto
041600120522           if QUSEI <> *blank;
041700120522             wSkipped = *on;
041800120522             leavesr;
041900120522           endif;
042000120523           // se la data ottenuta è < di 1/1/1995
042100120522           // o > di oggi
042200120522           // vuol dire che è accaduto qualcosa di strano all'oggetto
042300120522           // e quindi passo all'oggetto successivo, segnalando la stranezza
042400170201           Data08A = %char(CurDat);
042500120523           if %dec(Data08A:8:0) < 19950101
042600120522             or %dec(Data08A:8:0) > %dec(%date() : *ISO);
042700120522             wSkipped = *on;
042800120522             leavesr;
042900120522           endif;
043000120522
043100120522         // ctrl su data uso
043200120522         when pInDateType = '2';
043300120522           clear qusec;
043400120522           Qusbprv = %size(Qusec);
043500120522           // la data in DS è in formato system timestamp (*DTS) e va convertito
043600170201           ConvertDateAndTimeFormat('*DTS': QUSLUDT: '*YYMD': Curdate:
043700120522                                    Qusec);
043800120522           // siccome l'API è legata a un singolo oggetto, nel caso di errore non esco
043900120522           // ma passo al prossimo oggetto
044000120522           if QUSEI <> *blank;
044100120522             wSkipped = *on;
044200120522             leavesr;
044300120522           endif;
044400120523           // se la data ottenuta è < di 1/1/1995
044500120522           // o > di oggi
044600120522           // vuol dire che è accaduto qualcosa di strano all'oggetto
044700120522           // e quindi passo all'oggetto successivo, segnalando la stranezza
044800170201           Data08A = %char(CurDat);
044900120523           if %dec(Data08A:8:0) < 19950101
045000120522             or %dec(Data08A:8:0) > %dec(%date() : *ISO);
045100120522             wSkipped = *on;
045200120522             leavesr;
045300120522           endif;
045400120522         endsl;
045500120522
045600120522
045700120511         select;
045800120511         // modo ctrl "prima"
045900120511         when pInMode = '1';
046000120522           if Data08A < %char(pInDate);
046100120522             wDeletable = *on;
046200120522           endif;
046300120511
046400120511         // modo ctrl "solo quella"
046500120511         when pInMode = '2';
046600120522           if Data08A = %char(pInDate);
046700120522             wDeletable = *on;
046800120522           endif;
046900120511
047000120511         // modo ctrl "dopo"
047100120511         when pInMode = '3';
047200120522           if Data08A > %char(pInDate);
047300120522             wDeletable = *on;
047400120522           endif;
047500120511         endsl;
047600120511
047700120511       endsr;
047800110221
047900110221       //--------------------------------------------------------------
048000120514       // Controllo se chiamata API andata in errore                  ?
048100110221       //--------------------------------------------------------------
048200120514       begsr  CtrlAPIErr;
048300110224
048400120514         if QUSEI <> *blank;
048500120518           pOutError = '8';
048600120514           pOutErrorD = 'ERRORE: chiamata API ' + %trimr(wAPIName) +
048700120514                        ' con errore ' + QUSEI;
048800120514
048900120514           exsr sr_RoutEnd;
049000120514         endif;
049100120514
049200120514       endsr;
049300120522
049400120522       //--------------------------------------------------------------
049500120522       // Imposta parametri in uscita a seconda di Deleted e Skipped
049600120522       //--------------------------------------------------------------
049700120522       begsr  SetUpParmOut;
049800120522
049900120522         select;
050000120522         // se non ho avuto errori
050100120522         when pOutError = '0' and
050200120522         // ma non ho cancellato nulla
050300120522            wDeleted = *off and
050400120522         // e non ho saltato nulla
050500120522            wSkipped = *off;
050600120522            // è come se non avessi trovato nulla coi parametri indicati
050700120522           pOutError = '1';
050800120522           pOutErrorD = 'ERRORE: con i parametri indicati non sono stati +
050900120522                         trovati oggetti';
051000120522
051100120522         // se non ho avuto errori
051200120522         when pOutError = '0' and
051300120522         // non ho cancellato nulla
051400120522            wDeleted = *off and
051500120522         // ma ho saltato qualcosa
051600120522            wSkipped = *on;
051700120522            // segnalo l'anomalia
051800120522           pOutError = '9';
051900120522           pOutErrorD = 'ERRORE: nessun oggetto cancellato ma alcuni +
052000120522                         oggetti avevano anomalie';
052100120522
052200120522         // se non ho avuto errori
052300120522         when pOutError = '0' and
052400120522         // ho cancellato qualcosa
052500120522            wDeleted = *on and
052600120522         // ma ho saltato qualcosa
052700120522            wSkipped = *on;
052800120522            // segnalo l'anomalia
052900120522           pOutError = 'A';
053000120522           pOutErrorD = 'ERRORE: eseguita cancellazione ma alcuni oggetti +
053100120522                         avevano anomalie';
053200120522         endsl;
053300120522
053400120522       endsr;
053500120514
053600120514       //--------------------------------------------------------------
053700120514       // Operazioni finali.                                           ?
053800120514       //--------------------------------------------------------------
053900120514       begsr  sr_RoutEnd;
054000120514
054100120511         // Uscita dal *pgm?
054200110221         return;
054300110221
054400110221       endsr;
054500110221
054600110221       //--------------------------------------------------------------
054700120511       // Esecuzione del comando (già impostato).                      ?
054800110221       //--------------------------------------------------------------
054900110221       begsr  sr_ExecCmd;
055000110221
055100110221         clear Qcap0100;
055200110221         Qcabcsdh = *off;
055300110221         Qcapa    = *off;
055400110221         Qcacmdss = *off;
055500110221         Qcaerved = *allX'00';
055600110221
055700110221         clear Qusec;
055800110221         Qusbprv  = %size(Qusec);
055900110221
056000110221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
056100110221                           %size(Qcap0100) : 'CPOP0100' : *omit :
056200110221                           0 : 0 : Qusec);
056300110221
056400110221         //if  Qusei <> *blank;
056500110222         //  ...;
056600110221         //endif;
056700110221
056800110221       endsr;
056900110221
057000110221      /end-free
