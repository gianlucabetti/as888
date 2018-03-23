000100091228       //==============================================================
000200160916       // Per spalmare dati su versione
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100160202     **
001200160202     ** ISTRUZIONI PER LA COMPILAZIONE
001300160916     **
001400161201     ** 1. Creare il pgm TIS7VMS4R (Opz 14 PDM)
001500160202     **
001600091228
001700091228       //--------------------------------------------------------------
001800121106       // Specifiche di controllo.
001900091228       //--------------------------------------------------------------
002000091223
002100091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200160601     h alwnull(*inputonly)
002300091223
002400091223       //--------------------------------------------------------------
002500121106       // Dichiarazione file.
002600091223       //--------------------------------------------------------------
002700160218
002800160915     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
002900160915     FTIVMS00F  IF A E           K DISK    prefix(F_)
003000160531
003100091223       //--------------------------------------------------------------
003200121106       // Definizione costanti.
003300091223       //--------------------------------------------------------------
003400100302
003500091223
003600091223       //--------------------------------------------------------------
003700121106       // Definizione schiere.
003800160915       //--------------------
003900091223
004000091223
004100091223       //--------------------------------------------------------------
004200121106       // Definizione aree dati.
004300091223       //--------------------------------------------------------------
004400091223
004500091223
004600140724       //--------------------------------------------------------------
004700140724       // Definizione strutture dati.
004800140724       //--------------------------------------------------------------
004900140724
005000121106       // - Status
005100110516     d Psds           sds
005200110516     d   SDSpgm          *proc
005300121106     d   JobName             244    253                                         Job name
005400121106     d   JobUser             254    263                                         User name
005500121106     d   JobNumber           264    269s 0                                      Job number
005600091223
005700091223       //--------------------------------------------------------------
005800121106       // Definizione variabili.
005900091223       //--------------------------------------------------------------
006000121106
006100160916       // - Parametri:
006200160916       // opz: blank=elabora  C=chiudi
006300160916     d pIn_Opz         s              1a
006400160916       // Procedura
006500160916     d pIn_Proc        s                   like(VMSProc)
006600160916       // Cliente unificante
006700160916     d pIn_KSU         s                   like(VMSKSU)
006800160916       // Data decorrenza
006900160916     d pIn_DDE         s                   like(VMSDDE)
007000160916       // Tipo causale
007100160916     d pIn_CauTip      s                   like(VMSCAUTIP)
007200160916       // Causale BRT
007300160916     d pIn_CauBRT      s                   like(VMSCAUBRT)
007400160916       // Data scadenza
007500160916     d pIn_DSC         s                   like(VMSDSC)
007600160916       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
007700160916       // esito: 5=rcd duplicato
007800160916     d pOut_Esito      s              1a
007900160916
008000121106       // Campi di comodo
008100160531     d wSQL            s           2048    inz  varying
008200160901     d DS_res          ds
008300160901     d  Proc                         10
008400160901     d  KSU                           8
008500160901     d  DDE                           8s 0
008600160915     d  DSC                           8s 0
008700091223
008800091223       //--------------------------------------------------------------
008900160603       // Definizione costanti.
009000091223       //--------------------------------------------------------------
009100130307
009200160531
009300160603       //--------------------------------------------------------------
009400160603       // Definizione prototipi procedure.
009500160603       //--------------------------------------------------------------
009600160603
009700160603
009800091223       //--------------------------------------------------------------
009900121106       // Definizione key-list.
010000091223       //--------------------------------------------------------------
010100091223
010200160915       // - TIVMS01I completa
010300160915     d k05VMS01      e ds                  extname(TIVMS01I : *key)
010400091223
010500091223       //--------------------------------------------------------------
010600121107       // Definizione parametri procedura.
010700091223       //--------------------------------------------------------------
010800091223
010900091223     c     *Entry        plist
011000160901     c                   parm                    pIn_Opz
011100160901     c                   parm                    pIn_Proc
011200160901     c                   parm                    pIn_KSU
011300160915     c                   parm                    pIn_DDE
011400160916     c                   parm                    pIn_CauTip
011500160916     c                   parm                    pIn_CauBRT
011600160915     c                   parm                    pIn_DSC
011700160901     c                   parm                    pOut_Esito
011800091223
011900130307      /free
012000091223
012100091223       //--------------------------------------------------------------
012200121106       // M A I N - L I N E
012300091223       //--------------------------------------------------------------
012400140806
012500130307       // Operazioni iniziali?
012600130307       exsr sr_RoutInz;
012700130307
012800140724       // attività richiesta dal chiamante se non chiamato per chiusura
012900160901       if pIn_Opz = *blank;
013000160217         exsr esegui;
013100140724       endif;
013200091223
013300121106       // Operazioni finali?
013400091223       exsr sr_RoutEnd;
013500160601
013600160601       //--------------------------------------------------------------
013700160601       //
013800160601       //--------------------------------------------------------------
013900160601       BEGSR  *inzsr;
014000160601
014100160601         // Inizializzazioni parametri esecuzione comandi sql
014200160601         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
014300160601
014400160601       ENDSR;
014500091223
014600091223       //--------------------------------------------------------------
014700121107       // Operazioni iniziali.
014800091223       //--------------------------------------------------------------
014900091223       BEGSR  sr_RoutInz;
015000160901
015100160901         // Inizializzazione parametri di output
015200160901
015300160901         // Inizializzo risultato a OK
015400160901         pOut_Esito = '1';
015500160608
015600160608         // Controllo i parametri di input
015700160608         exsr sr_CtrlPIn;
015800160202
015900160202       ENDSR;
016000160202
016100160202       //--------------------------------------------------------------
016200160202       // Controllo i parametri di input
016300160202       //--------------------------------------------------------------
016400160202       BEGSR  sr_CtrlPIn;
016500160901
016600160901         // Lancio deve essere blank o 'C'
016700160901         if pIn_Opz <> *blank and pIn_Opz <>'C';
016800160901           // esco con errore sui parametri
016900160901           pOut_Esito = 'I';
017000160901           exsr sr_RoutEnd;
017100160901         endif;
017200160901
017300160901         // Procedura deve essere <> blank
017400160901         if pIn_Proc = *blank;
017500160901           // esco con errore sui parametri
017600160901           pOut_Esito = 'I';
017700160901           exsr sr_RoutEnd;
017800160901         endif;
017900160915
018000160915         // leggo il rcd di cui mi è stata passata la chiave univoca
018100160915         VMSProc = pIn_Proc;
018200160915         VMSKSU  = pIn_Ksu;
018300160916         VMSDDE  = pIn_DDE;
018400160916         VMSCAUTIP = pIn_CauTip;
018500160916         VMSCAUBRT = pIn_CauBRT;
018600160916         chain  %kds(k05vms01)  TIVMS01;
018700160915         // - Non trovato => errore
018800160916         if  Not %found();
018900160915           // esco con errore specifico
019000160915           pOut_Esito = '1';
019100160915           exsr sr_RoutEnd;
019200160915         endif;
019300160915
019400091223       ENDSR;
019500121106
019600091223       //--------------------------------------------------------------
019700160217       // Esecuzione
019800091223       //--------------------------------------------------------------
019900160217       BEGSR  esegui;
020000160915
020100160916         // preparo SQL per selezionare tutte le procedure/clienti unificanti/versioni
020200160916         // raggruppate (uso la view 06V), evitando:
020300160916         // - la riga avente procedura/unificante/decorrenza uguali a quelli passati
020400160916         // - la righe già scadute a oggi
020500160915         wSQL = 'select vmsproc, vmsksu, vmsdde, vmsdsc +
020600160915                 from tivms06v +
020700160901                 where +
020800160919                 vmsdsc >= ' + %char(%dec(%date())) + ' and( +
020900160919                 vmsproc <> ''' + pIn_Proc + ''' or +
021000160919                 vmsksu  <> ''' + pIn_Ksu  + ''' or +
021100160926                 vmsdde  <> ' + %char(%dec(%date(pIn_DDE))) + ')';
021200160601
021300160531         // Dichiarazione cursore
021400160531         exec sql   prepare S0   from :wSQL;
021500160531         exec sql   declare C0   cursor   for S0;
021600160531         // Apro il cursore
021700160531         exec sql open C0;
021800160901         if sqlCode < 0;
021900160901           // esco con errore SQL
022000160901           pOut_Esito = '2';
022100160901           exsr sr_RoutEnd;
022200160901         endif;
022300160915
022400160915         // ciclo su TIVMS raggruppato per procedura/cliente unificante/decorrenza/scadenza
022500160916         // con le esclusioni di cui sopra
022600160915         DoU  SQLCode = 100 or SQLCode < 0;
022700160915           exec sql
022800160901             fetch next from C0 into :DS_res;
022900160901
023000160915           select;
023100160915            // fine rcd
023200160915            when sqlCode = 100;
023300160915              // non faccio niente e uscirò dal giro
023400160915            // errore SQL
023500160915            when sqlCode < 0;
023600160915              exec sql close C0;
023700160915              // esco con errore SQL
023800160915              pOut_Esito = '2';
023900160915              exsr sr_RoutEnd;
024000160915            // c'è il rcd, allora spalmo
024100160915            other;
024200160915
024300160915             // prima di scrivere devo controllare che non ci siano accavallamenmti (ACCANTONATA)
024400160915             // imposto i dati da scrivere che ho letto dalla versione
024500160915             F_VMSPROC   = Proc   ;
024600160915             F_VMSKSU    = KSU    ;
024700160915             F_VMSDDE    = DDE    ;
024800160915             F_VMSDSC    = DSC    ;
024900160915             // imposto i campi con i dati del rcd in ingresso
025000160915             F_VMSATB    = *blank;
025100160915             F_VMSCAUTIP = VMSCAUTIP  ;
025200160915             F_VMSCAUBRT = VMSCAUBRT  ;
025300160915             F_VMSSTSCLI = VMSSTSCLI  ;
025400161201             // se il campo è vuoto, lo imposto a 'NONINVIARE'
025500161201             if F_VMSSTSCLI = *blank;
025600161201               F_VMSSTSCLI = 'NONINVIARE'  ;
025700161201             endif;
025800160915             F_VMSSTSOPZ = VMSSTSOPZ  ;
025900160915             F_VMSCAUTT  = VMSCAUTT   ;
026000160915             F_VMSFLO    = VMSFLO     ;
026100160915             F_VMSFASOP  = VMSFASOP   ;
026200160915             F_VMSDESCRIZ= VMSDESCRIZ ;
026300160915             F_VMSDESCIT = VMSDESCIT  ;
026400160915             F_VMSDESCEN = VMSDESCEN  ;
026500160915             F_VMSDESCFR = VMSDESCFR  ;
026600160915             F_VMSDESCDE = VMSDESCDE  ;
026700160915             F_VMSDESCES = VMSDESCES  ;
026800160915
026900160916             write(e) TIVMS000;
027000160916             // - Non trovato => errore
027100160916             if   %error;
027200160923               // CONTINUO in modo da scrivere tutto ciò che posso
027300160923               //pOut_Esito = '5';
027400160923               //exsr sr_RoutEnd;
027500160916             endif;
027600160915
027700160915           endsl;
027800160915
027900160915         enddo;
028000160901
028100160901         // Quindi chiudo il cursore
028200160531         exec sql close C0;
028300160531
028400160531         // Se sono qui tutto OK
028500160531         pOut_Esito = '1';
028600160217
028700160217       ENDSR;
028800160601
028900160601       //--------------------------------------------------------------
029000160601       // Operazioni finali.
029100160601       //--------------------------------------------------------------
029200160601       BEGSR  sr_RoutEnd;
029300160601
029400160601         // Chiusura pgm
029500160601         if pIn_Opz = 'C';
029600160601           *inlr = *on;
029700160601         else;
029800160901
029900160901           // se c'è un errore
030000160901           if pOut_Esito <>'1';
030100160915             // demando al pgm chiamante
030200160901           endif;
030300160901
030400160601           *inrt = *on;
030500160601         endif;
030600160601         return;
030700160601
030800160601       ENDSR;
030900160901
