000100110221       //==============================================================
000200170125       // UBLGOBR // Reperimento dati oggetto per logica richiesta
000300110221       //==============================================================
000400170125
000500170303         // Logiche speciali (da non valorizzare in tab.VPR)
000600170303         // 99: lasciare i dati nello US e non scrivere WAFOB00F
000700170303
000800170125       //--------------------------------------------------------------
000900170125       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001000170125       //--------------------------------------------------------------
001100170125
001200170125     /*PRM dbgview(*source)
001300170125     /*PRM commit(*none)
001400170125     /*END
001500170125     **
001600170125     ** ISTRUZIONI PER LA COMPILAZIONE
001700170125     **
001800170130     ** 1. Creare il modulo UBLGOBR (Opz 15 PDM)
001900170130     ** 2. Creare/Aggiornare il programma di servizio UBLGOBR (CRTSRVPGM / UPDSRVPGM)
002000170130     **    con ACTGRP(*caller) bnddir(UBBDDIR)
002100170130     **    aggiungere nella BNDDIR(UBBNDDIR)
002200170130     ** 3. Visto chi userà questo servizio, non serve l'oggetto programma
002300170130     ** 4. Cancellare il modulo.
002400170125     **
002500170125
002600120511       //
002700120511       // i parametyri di ingresso devono essere:
002800170125       // iLogica: obbligatorio
002900170125       //   può valere:
003000170125       //   01 = tutti gli oggetti di tutte le librerie mai usati
003100170125       //   02 =
003200170125       // iLib: obbligatorio
003300170125       // iObj: obbligatorio
003400170125       // iObjType: obbligatorio
003500170504       // iUsrCrt: facoltativo (blank = tutti)
003600120518       //
003700170125
003800170125       // oError = 1 => oErrorD = ERRORE: con i parametri indicati non sono stati trovati
003900170125       //                            oggetti
004000170125       // oError = 2 => oErrorD = ERRORE: il parametro Libreria è obbligatorio
004100170125       // oError = 3 => oErrorD = ERRORE: il parametro Tipo oggetto è obbligatorio
004200170125       // oError = 4 => oErrorD = ERRORE: il parametro Oggetto è obbligatorio
004300170125       // oError = 5 => oErrorD = ERRORE: il parametro Logica è obbligatorio
004400170125       // oError = 8 => oErrorD = ERRORE: chiamata API <APIName> con errore <CPF>
004500170125       // oError = 9 => oErrorD = ERRORE: nessun oggetto cancellato ma alcuni oggetti aveva...
004600170125       // oError = A => oErrorD = ERRORE: eseguita cancellazione ma alcuni oggetti avevano ...
004700120522
004800110221       //--------------------------------------------------------------
004900120511       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
005000110221       //--------------------------------------------------------------
005100110221
005200110221     /*PRM dbgview(*source)
005300110221     /*END
005400110221
005500110221       //--------------------------------------------------------------
005600120511       // Specifiche di controllo.                                     ?
005700110221       //--------------------------------------------------------------
005800110221
005900110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
006000170130     h alwnull(*inputonly)
006100110221
006200110221       //--------------------------------------------------------------
006300120511       // Dichiarazione file.                                          ?
006400110221       //--------------------------------------------------------------
006500110221
006600170303     FWAFOB00F  O    E             DISK    usropn
006700110221
006800110221       //--------------------------------------------------------------
006900120511       // Definizione costanti.                                        ?
007000110221       //--------------------------------------------------------------
007100110221
007200110221
007300110221       //--------------------------------------------------------------
007400120511       // Definizione schiere.                                         ?
007500110221       //--------------------------------------------------------------
007600110221
007700110221
007800110221       //--------------------------------------------------------------
007900120511       // Definizione aree dati.                                       ?
008000110221       //--------------------------------------------------------------
008100110221
008200110221
008300110221       //--------------------------------------------------------------
008400120511       // Definizione strutture dati.                                  ?
008500110221       //--------------------------------------------------------------
008600110221
008700120511       // Status ds?
008800110221     d Status         sds
008900120511     d  SDSproc          *proc
009000120511     d  SDSparms         *parms
009100110224     d  SDSdta               191    198
009200110224     d  SDSjob               244    253
009300110221     d  SDSusr               254    263
009400110224     d  SDSjnr               264    269  0
009500110221
009600110221       //--------------------------------------------------------------
009700120511       // Definizione variabili globali.                               ?
009800110221       //--------------------------------------------------------------
009900170125
010000170125       // Parametri Logica 01
010100170125     d iDati01         s            700
010200170125     d oDati01         s            100
010300170125     d oError01        s              2
010400170323
010500170323       // Parametri Logica 02
010600170323     d iDati02         s            700
010700170323     d oDati02         s            100
010800170323     d oError02        s              2
010900120511
011000120511       // variabili per QUSCRTUS
011100120511     d Crtus01         s             20    inz
011200120511     d Crtus02         s             10    inz
011300120511     d Crtus03         s              9b 0 inz
011400120511     d Crtus04         s              1    inz
011500120511     d Crtus05         s             10    inz
011600120511     d Crtus06         s             50    inz
011700120511     d Crtus07         s             10    inz
011800120511
011900120511       // variabili per QUSLOBJ
012000120511     d Lobj01          s             20    inz
012100120511     d Lobj02          s              8    inz
012200120511     d Lobj03          s             20    inz
012300120511     d Lobj04          s             10    inz
012400120511
012500120511       // variabili per QUSRTVUS
012600120511     d Rtvus01         s             20    inz
012700120511     d Rtvus02         s              9b 0 inz
012800120511     d Rtvus03         s              9b 0 inz
012900120514
013000120514       // appoggio
013100120514     d wAPIName        s             20
013200120514     d Data08A         s              8
013300120511
013400120511       // indici
013500120511     d wrkCount        s              9s 0
013600120511
013700120511       // booleani
013800170125     d wRientra        s               n
013900170125     d wWrited         s               n
014000120511
014100110221       //--------------------------------------------------------------
014200120511       // Definizione prototipi procedure.                             ?
014300110221       //--------------------------------------------------------------
014400110221
014500120511       // Reperimento NETA sistema AS/400 corrente?
014600110221     d currSysNeta     s              8a   inz
014700110221      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014800110224
014900120511       // strutture generiche per API
015000120511      /copy QSYSINC/QRPGLESRC,QUSGEN
015100120511
015200120511       // prototipi per QUS...
015300120511      /copy gaitrasrc/srcprotopr,QUS
015400120511
015500120511       // lista oggetti
015600120511      /copy QSYSINC/QRPGLESRC,QUSLOBJ
015700120511      /copy gaitrasrc/srcProtoPR,QUSLOBJ
015800110221
015900120511       // Parametri API QCAPCMD (Process Commands)?
016000110221     d Qcmd            s           1024    inz  varying
016100110221      /copy qSysInc/qRpgleSrc,QCAPCMD
016200110221      /copy gaitrasrc/srcProtoPR,QCAPCMD
016300110221
016400120511       // Parametri gestione errori API.?
016500110221      /copy qSysInc/qRpgleSrc,QUSEC
016600120514
016700120514       // Parametri per conversione data da un formato a un altro
016800120514      /copy gaitrasrc/srcprotopr,qwccvtdt
016900170130
017000170130      /copy gaitrasrc/srcProtoPR,UBLGOB01R
017100170323
017200170323      /copy gaitrasrc/srcProtoPR,UBLGOB02R
017300110221
017400170130      /copy gaitrasrc/srcProtoPI,UBLGOBR
017500170130
017600110221       //--------------------------------------------------------------
017700120511       // Definizione key-list.                                        ?
017800110221       //--------------------------------------------------------------
017900110221
018000110221
018100110221       //==============================================================
018200120511       // M A I N - L I N E                                            ?
018300110221       //==============================================================
018400110221
018500120511       // Operazioni iniziali?
018600110221       exsr  sr_RoutInz;
018700110221
018800120511       // Creazione lista oggetti
018900120511       exsr  CrtObjList;
019000170303
019100170303       // se la logica non è 99 = lasciare dati in US
019200170303       if iLogica <> '99';
019300170303         // Scrivi su WAFOB
019400170303         exsr  WrtFOB;
019500170303       endif;
019600170125
019700170125       // esco
019800170125       exsr sr_RoutEnd;
019900110221
020000110221       //--------------------------------------------------------------
020100120511       // Operazioni iniziali.                                         ?
020200110221       //--------------------------------------------------------------
020300110221       begsr  sr_RoutInz;
020400110221
020500170130         *inRT = *on;
020600110221
020700120511         // Verifica del sistema AS/400 corrente?(al momento non serve)
020800110221         select;
020900110221           when  UBRTVNETA_Rtv( currSysNeta ) <> *zero;
021000110221             return;
021100120511         //when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
021200120511         //  return;
021300110221         endsl;
021400110224
021500170125         oError = '0';
021600120518
021700120511         // ctrl parametri in ingresso
021800120511         exsr CtrlParmIn;
021900170303
022000170303         // se la logica non è 99 = lasciare dati in US
022100170303         if iLogica <> '99';
022200170303           // apro il file da scrivere, se non già aperto
022300170303           if not %open(WAFOB00F);
022400170303             open WAFOB00F;
022500170303           endif;
022600170303         endif;
022700110221
022800110221       endsr;
022900110221
023000110221       //--------------------------------------------------------------
023100120511       // Controllo parametri in ingresso                             ?
023200110221       //--------------------------------------------------------------
023300120511       begsr  CtrlParmIn;
023400110221
023500170125         // la logica è obbligatoria
023600170125         if iLogica = *blank;
023700170125           oError = '5';
023800170125           oErrorD = 'ERRORE: il parametro Logica è obbligatorio';
023900120511           exsr sr_RoutEnd;
024000120511         endif;
024100170125
024200170125         // la libreria è obbligatoria
024300170125         if iLib = *blank;
024400170125           oError = '2';
024500170125           oErrorD = 'ERRORE: il parametro Libreria è obbligatorio';
024600170125           exsr sr_RoutEnd;
024700170125         endif;
024800120511
024900120511         // il tipo è obbligatorio
025000170125         if iObjType = *blank;
025100170125           oError = '3';
025200170125           oErrorD = 'ERRORE: il parametro Tipo oggetto è obbligatorio';
025300120511           exsr sr_RoutEnd;
025400120511         endif;
025500120511
025600120511         // il nome oggetto (o iniziale o parola chiave *ALL) è obbligatorio
025700170125         if iObj = *blank;
025800170125           oError = '4';
025900170125           oErrorD = 'ERRORE: il parametro Oggetto è obbligatorio';
026000120511           exsr sr_RoutEnd;
026100120511         endif;
026200120511
026300120511       endsr;
026400120511
026500120511       //--------------------------------------------------------------
026600120511       // Crea la lista degli oggetti
026700120511       //--------------------------------------------------------------
026800120511       begsr  CrtObjList;
026900120511
027000120511         // Creazione dello user space?
027100120511         Crtus01 = SDSproc + 'QTEMP';
027200120511         clear qusec;
027300120511         Qusbprv = %size(Qusec);
027400120514         CreateUserSpace(CrtUs01: CrtUs02: 1024: CrtUs04: '*CHANGE': CrtUs06:
027500120511                   '*YES': Qusec);
027600120514         wAPIName= 'QUSCRTUS';
027700120514         exsr CtrlAPIErr;
027800120511
027900120511         // Riempimmento dello user space con la lista degli oggetti
028000120511         clear Qusec;
028100120511         Qusbprv = %size(Qusec);
028200170125         Lobj03 = iObj + iLib;
028300170125         ListObjects(CrtUs01: 'OBJL0600': Lobj03: iObjType: QUsec);
028400120514         wAPIName= 'QUSLOBJ';
028500120514         exsr CtrlAPIErr;
028600120511
028700120511         // Reperimento del numero di elementi ?
028800120511         //       e della lunghezza di un elemento?
028900120511         clear Qusec;
029000120511         Qusbprv = %size(Qusec);
029100120511         Rtvus03 = %size(Qush0100);
029200120514         RetrieveUserSpace(CrtUs01: 1: RtvUs03: QUsh0100: Qusec);
029300120514         wAPIName= 'QUSRTVUS';
029400120514         exsr CtrlAPIErr;
029500120511
029600120511         // Se non trovo dati lo segnalo al chiamante
029700120511if  1    if Qusnbrle = 0;
029800170125           oError = '1';
029900170125           oErrorD = 'ERRORE: con i parametri indicati non sono stati +
030000120511                         trovati oggetti';
030100120511           exsr sr_RoutEnd;
030200120511e   1    endif;
030300120511
030400120511         // Impostazione della posizione iniziale del primo elemento?
030500120511         // e della lunghezza?di ogni elemento
030600120511         Rtvus02 = Qusold + 1;
030700120511         Rtvus03 = Qussee;
030800110221
030900110221       endsr;
031000110221
031100110221       //--------------------------------------------------------------
031200170125       // Ciclo di scrittura dei dati recuperati
031300110221       //--------------------------------------------------------------
031400170125       begsr  WrtFOB;
031500120518
031600170125       wWrited = *off;
031700120511
031800120511       for WrkCount = 1 to Qusnbrle;
031900120511
032000120511         // Reperimento dell'elemento?
032100120511         clear Qusec;
032200120511         Qusbprv = %size(Qusec);
032300120514         RetrieveUserSpace(CrtUs01: RtvUs02: RtvUs03: QUSL0600: Qusec);
032400120514         wAPIName= 'QUSRTVUS';
032500120514         exsr CtrlAPIErr;
032600170125
032700170125         // chiamo pgm che analizza i dati a seconda della logica richiesta
032800170125         select;
032900170125          when iLogica = '01';
033000170125           exsr AnalisiLog01;
033100170125          when iLogica = '02';
033200170125           exsr AnalisiLog02;
033300170125         endsl;
033400170125
033500170125         // se il rcd rientra nella Logica richiesta
033600170125         if wRientra = *on;
033700170504           // controllo i dati rispetto a valori validi per qualsiasi logica
033800170504           exsr AnalisiCross;
033900170504         endif;
034000170504
034100170504         // se il rcd rientra nella Logica richiesta
034200170504         if wRientra = *on;
034300170125           // scrivo il file
034400170125           FOBLogica= iLogica;
034500170125           FOBDtEstr= %date();
034600170504           // dalla DS QUSLI600 caricata da /copy QSYSINC/QRPGLESRC,QUSLOBJ
034700170125           FOBOBJ   = QUSOBJNU04;
034800170125           FOBLIB   = QUSOLNU04;
034900170125           FOBOBJT  = QUSOBJTU04;
035000170131           FOBATTR  = QUSEOA03;
035100170504           FOBOBJD  = QUSTD10;
035200170125           // i dati li ho impostati nella analisi di logica
035300170125           write WAFOB000;
035400170125           wWrited = *on;
035500170125         endif;
035600120511
035700120511         // Impostazione della posizione iniziale del prossimo elemento?
035800120511         Rtvus02 = Rtvus02 + Qussee;
035900120511       endfor;
036000120518
036100120518         // Cancellazione dello user space?
036200120511         clear Qusec;
036300120511         Qusbprv = %size(Qusec);
036400120514         DeleteUserSpace(CrtUs01: Qusec);
036500120514         wAPIName= 'QUSDLTUS';
036600120514         exsr CtrlAPIErr;
036700120518
036800170125         // imposto parametri di uscita a seconda della scrittura
036900120522         exsr SetUpParmOut;
037000120511
037100120511       endsr;
037200120511
037300120511       //--------------------------------------------------------------
037400170125       // Analisi per logica 01
037500120511       //--------------------------------------------------------------
037600170125       begsr  AnalisiLog01;
037700120511
037800170125         wRientra = *off;
037900170126         clear oDati01;
038000170126         clear oError01;
038100170125         iDati01 = QUSL0600;
038200170130         UBLGOB01R_S(iDati01 : oDati01 : oError01);
038300170130     C*                  call      'UBLGOB01R'
038400170130     C*                  parm                    iDati01
038500170130     C*                  parm                    oDati01
038600170130     C*                  parm                    oError01
038700170125         if oError01 = '0';
038800170126           wRientra = *on;
038900170125           FOBDATI  = oDati01;
039000170125         endif;
039100120511
039200120511       endsr;
039300170125
039400170125       //--------------------------------------------------------------
039500170125       // Analisi per logica 02
039600170125       //--------------------------------------------------------------
039700170125       begsr  AnalisiLog02;
039800170125
039900170125         wRientra = *off;
040000170323         clear oDati02;
040100170323         clear oError02;
040200170323         iDati02 = QUSL0600;
040300170323         UBLGOB02R_S(iDati02 : oDati02 : oError02);
040400170323         if oError02 = '0';
040500170323           wRientra = *on;
040600170323           FOBDATI  = oDati02;
040700170323         endif;
040800170125
040900170125       endsr;
041000170504
041100170504       //--------------------------------------------------------------
041200170504       // Analisi cross ogni logica
041300170504       //--------------------------------------------------------------
041400170504       begsr  AnalisiCross;
041500170504
041600170504         // parto positivo
041700170504         wRientra = *on;
041800170504
041900170504         if iUsrCrt <> *blank;
042000170504           if iUsrCrt <> QUSCUP01;
042100170504             wRientra = *off;
042200170504             leavesr;
042300170504           endif;
042400170504         endif;
042500170504
042600170504       endsr;
042700110221
042800110221       //--------------------------------------------------------------
042900120514       // Controllo se chiamata API andata in errore                  ?
043000110221       //--------------------------------------------------------------
043100120514       begsr  CtrlAPIErr;
043200110224
043300120514         if QUSEI <> *blank;
043400170125           oError = '8';
043500170125           oErrorD = 'ERRORE: chiamata API ' + %trimr(wAPIName) +
043600120514                        ' con errore ' + QUSEI;
043700120514
043800120514           exsr sr_RoutEnd;
043900120514         endif;
044000120514
044100120514       endsr;
044200120522
044300120522       //--------------------------------------------------------------
044400170125       // Imposta parametri in uscita a seconda della scrittura
044500120522       //--------------------------------------------------------------
044600120522       begsr  SetUpParmOut;
044700120522
044800120522         select;
044900120522         // se non ho avuto errori
045000170125         when oError = '0' and
045100170125         // ma non ho scritto nulla
045200170125            wWrited = *off;
045300120522            // è come se non avessi trovato nulla coi parametri indicati
045400170125           oError = '1';
045500170125           oErrorD = 'ERRORE: con i parametri indicati non sono stati +
045600120522                         trovati oggetti';
045700120522         endsl;
045800120522
045900120522       endsr;
046000120514
046100120514       //--------------------------------------------------------------
046200120514       // Operazioni finali.                                           ?
046300120514       //--------------------------------------------------------------
046400120514       begsr  sr_RoutEnd;
046500120514
046600120511         // Uscita dal *pgm?
046700110221         return;
046800110221
046900110221       endsr;
047000110221
047100110221       //--------------------------------------------------------------
047200120511       // Esecuzione del comando (già impostato).                      ?
047300110221       //--------------------------------------------------------------
047400110221       begsr  sr_ExecCmd;
047500110221
047600110221         clear Qcap0100;
047700110221         Qcabcsdh = *off;
047800110221         Qcapa    = *off;
047900110221         Qcacmdss = *off;
048000110221         Qcaerved = *allX'00';
048100110221
048200110221         clear Qusec;
048300110221         Qusbprv  = %size(Qusec);
048400110221
048500110221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
048600110221                           %size(Qcap0100) : 'CPOP0100' : *omit :
048700110221                           0 : 0 : Qusec);
048800110221
048900110221         //if  Qusei <> *blank;
049000110222         //  ...;
049100110221         //endif;
049200110221
049300110221       endsr;
