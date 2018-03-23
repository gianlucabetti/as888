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
003500120518       //
003600170125
003700170125       // oError = 1 => oErrorD = ERRORE: con i parametri indicati non sono stati trovati
003800170125       //                            oggetti
003900170125       // oError = 2 => oErrorD = ERRORE: il parametro Libreria è obbligatorio
004000170125       // oError = 3 => oErrorD = ERRORE: il parametro Tipo oggetto è obbligatorio
004100170125       // oError = 4 => oErrorD = ERRORE: il parametro Oggetto è obbligatorio
004200170125       // oError = 5 => oErrorD = ERRORE: il parametro Logica è obbligatorio
004300170125       // oError = 8 => oErrorD = ERRORE: chiamata API <APIName> con errore <CPF>
004400170125       // oError = 9 => oErrorD = ERRORE: nessun oggetto cancellato ma alcuni oggetti aveva...
004500170125       // oError = A => oErrorD = ERRORE: eseguita cancellazione ma alcuni oggetti avevano ...
004600120522
004700110221       //--------------------------------------------------------------
004800120511       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004900110221       //--------------------------------------------------------------
005000110221
005100110221     /*PRM dbgview(*source)
005200110221     /*END
005300110221
005400110221       //--------------------------------------------------------------
005500120511       // Specifiche di controllo.                                     ?
005600110221       //--------------------------------------------------------------
005700110221
005800110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005900170130     h alwnull(*inputonly)
006000110221
006100110221       //--------------------------------------------------------------
006200120511       // Dichiarazione file.                                          ?
006300110221       //--------------------------------------------------------------
006400110221
006500170303     FWAFOB00F  O    E             DISK    usropn
006600110221
006700110221       //--------------------------------------------------------------
006800120511       // Definizione costanti.                                        ?
006900110221       //--------------------------------------------------------------
007000110221
007100110221
007200110221       //--------------------------------------------------------------
007300120511       // Definizione schiere.                                         ?
007400110221       //--------------------------------------------------------------
007500110221
007600110221
007700110221       //--------------------------------------------------------------
007800120511       // Definizione aree dati.                                       ?
007900110221       //--------------------------------------------------------------
008000110221
008100110221
008200110221       //--------------------------------------------------------------
008300120511       // Definizione strutture dati.                                  ?
008400110221       //--------------------------------------------------------------
008500110221
008600120511       // Status ds?
008700110221     d Status         sds
008800120511     d  SDSproc          *proc
008900120511     d  SDSparms         *parms
009000110224     d  SDSdta               191    198
009100110224     d  SDSjob               244    253
009200110221     d  SDSusr               254    263
009300110224     d  SDSjnr               264    269  0
009400110221
009500110221       //--------------------------------------------------------------
009600120511       // Definizione variabili globali.                               ?
009700110221       //--------------------------------------------------------------
009800170125
009900170125       // Parametri Logica 01
010000170125     d iDati01         s            700
010100170125     d oDati01         s            100
010200170125     d oError01        s              2
010300170323
010400170323       // Parametri Logica 02
010500170323     d iDati02         s            700
010600170323     d oDati02         s            100
010700170323     d oError02        s              2
010800120511
010900120511       // variabili per QUSCRTUS
011000120511     d Crtus01         s             20    inz
011100120511     d Crtus02         s             10    inz
011200120511     d Crtus03         s              9b 0 inz
011300120511     d Crtus04         s              1    inz
011400120511     d Crtus05         s             10    inz
011500120511     d Crtus06         s             50    inz
011600120511     d Crtus07         s             10    inz
011700120511
011800120511       // variabili per QUSLOBJ
011900120511     d Lobj01          s             20    inz
012000120511     d Lobj02          s              8    inz
012100120511     d Lobj03          s             20    inz
012200120511     d Lobj04          s             10    inz
012300120511
012400120511       // variabili per QUSRTVUS
012500120511     d Rtvus01         s             20    inz
012600120511     d Rtvus02         s              9b 0 inz
012700120511     d Rtvus03         s              9b 0 inz
012800120514
012900120514       // appoggio
013000120514     d wAPIName        s             20
013100120514     d Data08A         s              8
013200120511
013300120511       // indici
013400120511     d wrkCount        s              9s 0
013500120511
013600120511       // booleani
013700170125     d wRientra        s               n
013800170125     d wWrited         s               n
013900120511
014000110221       //--------------------------------------------------------------
014100120511       // Definizione prototipi procedure.                             ?
014200110221       //--------------------------------------------------------------
014300110221
014400120511       // Reperimento NETA sistema AS/400 corrente?
014500110221     d currSysNeta     s              8a   inz
014600110221      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014700110224
014800120511       // strutture generiche per API
014900120511      /copy QSYSINC/QRPGLESRC,QUSGEN
015000120511
015100120511       // prototipi per QUS...
015200120511      /copy gaitrasrc/srcprotopr,QUS
015300120511
015400120511       // lista oggetti
015500120511      /copy QSYSINC/QRPGLESRC,QUSLOBJ
015600120511      /copy gaitrasrc/srcProtoPR,QUSLOBJ
015700110221
015800120511       // Parametri API QCAPCMD (Process Commands)?
015900110221     d Qcmd            s           1024    inz  varying
016000110221      /copy qSysInc/qRpgleSrc,QCAPCMD
016100110221      /copy gaitrasrc/srcProtoPR,QCAPCMD
016200110221
016300120511       // Parametri gestione errori API.?
016400110221      /copy qSysInc/qRpgleSrc,QUSEC
016500120514
016600120514       // Parametri per conversione data da un formato a un altro
016700120514      /copy gaitrasrc/srcprotopr,qwccvtdt
016800170130
016900170130      /copy gaitrasrc/srcProtoPR,UBLGOB01R
017000170323
017100170323      /copy gaitrasrc/srcProtoPR,UBLGOB02R
017200110221
017300170130      /copy gaitrasrc/srcProtoPI,UBLGOBR
017400170130
017500110221       //--------------------------------------------------------------
017600120511       // Definizione key-list.                                        ?
017700110221       //--------------------------------------------------------------
017800110221
017900110221
018000110221       //==============================================================
018100120511       // M A I N - L I N E                                            ?
018200110221       //==============================================================
018300110221
018400120511       // Operazioni iniziali?
018500110221       exsr  sr_RoutInz;
018600110221
018700120511       // Creazione lista oggetti
018800120511       exsr  CrtObjList;
018900170303
019000170303       // se la logica non è 99 = lasciare dati in US
019100170303       if iLogica <> '99';
019200170303         // Scrivi su WAFOB
019300170303         exsr  WrtFOB;
019400170303       endif;
019500170125
019600170125       // esco
019700170125       exsr sr_RoutEnd;
019800110221
019900110221       //--------------------------------------------------------------
020000120511       // Operazioni iniziali.                                         ?
020100110221       //--------------------------------------------------------------
020200110221       begsr  sr_RoutInz;
020300110221
020400170130         *inRT = *on;
020500110221
020600120511         // Verifica del sistema AS/400 corrente?(al momento non serve)
020700110221         select;
020800110221           when  UBRTVNETA_Rtv( currSysNeta ) <> *zero;
020900110221             return;
021000120511         //when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
021100120511         //  return;
021200110221         endsl;
021300110224
021400170125         oError = '0';
021500120518
021600120511         // ctrl parametri in ingresso
021700120511         exsr CtrlParmIn;
021800170303
021900170303         // se la logica non è 99 = lasciare dati in US
022000170303         if iLogica <> '99';
022100170303           // apro il file da scrivere, se non già aperto
022200170303           if not %open(WAFOB00F);
022300170303             open WAFOB00F;
022400170303           endif;
022500170303         endif;
022600110221
022700110221       endsr;
022800110221
022900110221       //--------------------------------------------------------------
023000120511       // Controllo parametri in ingresso                             ?
023100110221       //--------------------------------------------------------------
023200120511       begsr  CtrlParmIn;
023300110221
023400170125         // la logica è obbligatoria
023500170125         if iLogica = *blank;
023600170125           oError = '5';
023700170125           oErrorD = 'ERRORE: il parametro Logica è obbligatorio';
023800120511           exsr sr_RoutEnd;
023900120511         endif;
024000170125
024100170125         // la libreria è obbligatoria
024200170125         if iLib = *blank;
024300170125           oError = '2';
024400170125           oErrorD = 'ERRORE: il parametro Libreria è obbligatorio';
024500170125           exsr sr_RoutEnd;
024600170125         endif;
024700120511
024800120511         // il tipo è obbligatorio
024900170125         if iObjType = *blank;
025000170125           oError = '3';
025100170125           oErrorD = 'ERRORE: il parametro Tipo oggetto è obbligatorio';
025200120511           exsr sr_RoutEnd;
025300120511         endif;
025400120511
025500120511         // il nome oggetto (o iniziale o parola chiave *ALL) è obbligatorio
025600170125         if iObj = *blank;
025700170125           oError = '4';
025800170125           oErrorD = 'ERRORE: il parametro Oggetto è obbligatorio';
025900120511           exsr sr_RoutEnd;
026000120511         endif;
026100120511
026200120511       endsr;
026300120511
026400120511       //--------------------------------------------------------------
026500120511       // Crea la lista degli oggetti
026600120511       //--------------------------------------------------------------
026700120511       begsr  CrtObjList;
026800120511
026900120511         // Creazione dello user space?
027000120511         Crtus01 = SDSproc + 'QTEMP';
027100120511         clear qusec;
027200120511         Qusbprv = %size(Qusec);
027300120514         CreateUserSpace(CrtUs01: CrtUs02: 1024: CrtUs04: '*CHANGE': CrtUs06:
027400120511                   '*YES': Qusec);
027500120514         wAPIName= 'QUSCRTUS';
027600120514         exsr CtrlAPIErr;
027700120511
027800120511         // Riempimmento dello user space con la lista degli oggetti
027900120511         clear Qusec;
028000120511         Qusbprv = %size(Qusec);
028100170125         Lobj03 = iObj + iLib;
028200170125         ListObjects(CrtUs01: 'OBJL0600': Lobj03: iObjType: QUsec);
028300120514         wAPIName= 'QUSLOBJ';
028400120514         exsr CtrlAPIErr;
028500120511
028600120511         // Reperimento del numero di elementi ?
028700120511         //       e della lunghezza di un elemento?
028800120511         clear Qusec;
028900120511         Qusbprv = %size(Qusec);
029000120511         Rtvus03 = %size(Qush0100);
029100120514         RetrieveUserSpace(CrtUs01: 1: RtvUs03: QUsh0100: Qusec);
029200120514         wAPIName= 'QUSRTVUS';
029300120514         exsr CtrlAPIErr;
029400120511
029500120511         // Se non trovo dati lo segnalo al chiamante
029600120511if  1    if Qusnbrle = 0;
029700170125           oError = '1';
029800170125           oErrorD = 'ERRORE: con i parametri indicati non sono stati +
029900120511                         trovati oggetti';
030000120511           exsr sr_RoutEnd;
030100120511e   1    endif;
030200120511
030300120511         // Impostazione della posizione iniziale del primo elemento?
030400120511         // e della lunghezza?di ogni elemento
030500120511         Rtvus02 = Qusold + 1;
030600120511         Rtvus03 = Qussee;
030700110221
030800110221       endsr;
030900110221
031000110221       //--------------------------------------------------------------
031100170125       // Ciclo di scrittura dei dati recuperati
031200110221       //--------------------------------------------------------------
031300170125       begsr  WrtFOB;
031400120518
031500170125       wWrited = *off;
031600120511
031700120511       for WrkCount = 1 to Qusnbrle;
031800120511
031900120511         // Reperimento dell'elemento?
032000120511         clear Qusec;
032100120511         Qusbprv = %size(Qusec);
032200120514         RetrieveUserSpace(CrtUs01: RtvUs02: RtvUs03: QUSL0600: Qusec);
032300120514         wAPIName= 'QUSRTVUS';
032400120514         exsr CtrlAPIErr;
032500170125
032600170125         // chiamo pgm che analizza i dati a seconda della logica richiesta
032700170125         select;
032800170125          when iLogica = '01';
032900170125           exsr AnalisiLog01;
033000170125          when iLogica = '02';
033100170125           exsr AnalisiLog02;
033200170125         endsl;
033300170125
033400170125         // se il rcd rientra nella Logica richiesta
033500170125         if wRientra = *on;
033600170125           // scrivo il file
033700170125           FOBLogica= iLogica;
033800170125           FOBDtEstr= %date();
033900170125           // dalla DS QUSLI00 caricata da /copy QSYSINC/QRPGLESRC,QUSLOBJ
034000170125           FOBOBJ   = QUSOBJNU04;
034100170125           FOBLIB   = QUSOLNU04;
034200170125           FOBOBJT  = QUSOBJTU04;
034300170131           FOBATTR  = QUSEOA03;
034400170125           // i dati li ho impostati nella analisi di logica
034500170125           write WAFOB000;
034600170125           wWrited = *on;
034700170125         endif;
034800120511
034900120511         // Impostazione della posizione iniziale del prossimo elemento?
035000120511         Rtvus02 = Rtvus02 + Qussee;
035100120511       endfor;
035200120518
035300120518         // Cancellazione dello user space?
035400120511         clear Qusec;
035500120511         Qusbprv = %size(Qusec);
035600120514         DeleteUserSpace(CrtUs01: Qusec);
035700120514         wAPIName= 'QUSDLTUS';
035800120514         exsr CtrlAPIErr;
035900120518
036000170125         // imposto parametri di uscita a seconda della scrittura
036100120522         exsr SetUpParmOut;
036200120511
036300120511       endsr;
036400120511
036500120511       //--------------------------------------------------------------
036600170125       // Analisi per logica 01
036700120511       //--------------------------------------------------------------
036800170125       begsr  AnalisiLog01;
036900120511
037000170125         wRientra = *off;
037100170126         clear oDati01;
037200170126         clear oError01;
037300170125         iDati01 = QUSL0600;
037400170130         UBLGOB01R_S(iDati01 : oDati01 : oError01);
037500170130     C*                  call      'UBLGOB01R'
037600170130     C*                  parm                    iDati01
037700170130     C*                  parm                    oDati01
037800170130     C*                  parm                    oError01
037900170125         if oError01 = '0';
038000170126           wRientra = *on;
038100170125           FOBDATI  = oDati01;
038200170125         endif;
038300120511
038400120511       endsr;
038500170125
038600170125       //--------------------------------------------------------------
038700170125       // Analisi per logica 02
038800170125       //--------------------------------------------------------------
038900170125       begsr  AnalisiLog02;
039000170125
039100170125         wRientra = *off;
039200170323         clear oDati02;
039300170323         clear oError02;
039400170323         iDati02 = QUSL0600;
039500170323         UBLGOB02R_S(iDati02 : oDati02 : oError02);
039600170323         if oError02 = '0';
039700170323           wRientra = *on;
039800170323           FOBDATI  = oDati02;
039900170323         endif;
040000170125
040100170125       endsr;
040200110221
040300110221       //--------------------------------------------------------------
040400120514       // Controllo se chiamata API andata in errore                  ?
040500110221       //--------------------------------------------------------------
040600120514       begsr  CtrlAPIErr;
040700110224
040800120514         if QUSEI <> *blank;
040900170125           oError = '8';
041000170125           oErrorD = 'ERRORE: chiamata API ' + %trimr(wAPIName) +
041100120514                        ' con errore ' + QUSEI;
041200120514
041300120514           exsr sr_RoutEnd;
041400120514         endif;
041500120514
041600120514       endsr;
041700120522
041800120522       //--------------------------------------------------------------
041900170125       // Imposta parametri in uscita a seconda della scrittura
042000120522       //--------------------------------------------------------------
042100120522       begsr  SetUpParmOut;
042200120522
042300120522         select;
042400120522         // se non ho avuto errori
042500170125         when oError = '0' and
042600170125         // ma non ho scritto nulla
042700170125            wWrited = *off;
042800120522            // è come se non avessi trovato nulla coi parametri indicati
042900170125           oError = '1';
043000170125           oErrorD = 'ERRORE: con i parametri indicati non sono stati +
043100120522                         trovati oggetti';
043200120522         endsl;
043300120522
043400120522       endsr;
043500120514
043600120514       //--------------------------------------------------------------
043700120514       // Operazioni finali.                                           ?
043800120514       //--------------------------------------------------------------
043900120514       begsr  sr_RoutEnd;
044000120514
044100120511         // Uscita dal *pgm?
044200110221         return;
044300110221
044400110221       endsr;
044500110221
044600110221       //--------------------------------------------------------------
044700120511       // Esecuzione del comando (già impostato).                      ?
044800110221       //--------------------------------------------------------------
044900110221       begsr  sr_ExecCmd;
045000110221
045100110221         clear Qcap0100;
045200110221         Qcabcsdh = *off;
045300110221         Qcapa    = *off;
045400110221         Qcacmdss = *off;
045500110221         Qcaerved = *allX'00';
045600110221
045700110221         clear Qusec;
045800110221         Qusbprv  = %size(Qusec);
045900110221
046000110221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
046100110221                           %size(Qcap0100) : 'CPOP0100' : *omit :
046200110221                           0 : 0 : Qusec);
046300110221
046400110221         //if  Qusei <> *blank;
046500110222         //  ...;
046600110221         //endif;
046700110221
046800110221       endsr;
