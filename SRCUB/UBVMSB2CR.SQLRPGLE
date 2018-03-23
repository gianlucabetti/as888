000100091228       //==============================================================
000200160913       // Driver per mappare stati BRT to Consumer
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
001300160202     **
001400160913     ** 1. Creare il modulo UBVMSB2CR (Opz 15 PDM)
001500160913     ** 2. Creare/Aggiornare il programma di servizio UBVMSB2CR (CRTSRVPGM / UPDSRVPGM)
001600160920     **    con ACTGRP(*caller) e BNDDIR=UBBMDDIR
001700160913     ** 3. Creare/Aggiornare il programma UBVMSB2CR (CRTPGM / UPDPGM)
001800160920     **    con ACTGRP(*caller) e BNDDIR=UBBMDDIR
001900160202     ** 4. Cancellare il modulo.
002000160202     **
002100091228
002200091228       //--------------------------------------------------------------
002300121106       // Specifiche di controllo.
002400091228       //--------------------------------------------------------------
002500091223
002600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002700160601     h alwnull(*inputonly)
002800091223
002900091223       //--------------------------------------------------------------
003000121106       // Dichiarazione file.
003100091223       //--------------------------------------------------------------
003200160218
003300160531
003400091223       //--------------------------------------------------------------
003500121106       // Definizione costanti.
003600091223       //--------------------------------------------------------------
003700100302
003800091223
003900091223       //--------------------------------------------------------------
004000121106       // Definizione schiere.
004100091223       //--------------------------------------------------------------
004200091223
004300091223
004400091223       //--------------------------------------------------------------
004500121106       // Definizione aree dati.
004600091223       //--------------------------------------------------------------
004700091223
004800091223
004900140724       //--------------------------------------------------------------
005000140724       // Definizione strutture dati.
005100140724       //--------------------------------------------------------------
005200140724
005300121106       // - Status
005400110516     d Psds           sds
005500110516     d   SDSpgm          *proc
005600121106     d   JobName             244    253                                         Job name
005700121106     d   JobUser             254    263                                         User name
005800121106     d   JobNumber           264    269s 0                                      Job number
005900091223
006000091223       //--------------------------------------------------------------
006100121106       // Definizione variabili.
006200091223       //--------------------------------------------------------------
006300121106
006400121106       // Campi di comodo
006500160531     d wSQL            s           2048    inz  varying
006600160901     d DS_res          ds
006700160920     d  CauTip                        3
006800160920     d  CauTT                        15
006900160920     d  StsCli                       15
007000160920     d  StsOpz                        5
007100160920     d  FasOp                        10
007200160920     d  Descriz                     200    varying
007300160920     d  DescIT                      200    varying
007400160920     d  DescEN                      200    varying
007500160920     d  DescDE                      200    varying
007600160920     d  DescFR                      200    varying
007700160920     d  DescES                      200    varying
007800160920     d  DDE                           8s 0
007900091223
008000091223       //--------------------------------------------------------------
008100160603       // Definizione costanti.
008200091223       //--------------------------------------------------------------
008300130307
008400160531
008500160603       //--------------------------------------------------------------
008600160603       // Definizione prototipi procedure.
008700160603       //--------------------------------------------------------------
008800160603
008900160920       /copy gaitrasrc/srcProtoPI,UBVMSB2CR
009000160920       /copy gaitrasrc/srcProtoPR,UBVMSB2CR
009100160920       /copy gaitrasrc/srcProtoPR,UBVMSDDER
009200160921       /copy gaitrasrc/srcProtoPI,UBVMSDDER
009300160603
009400091223       //--------------------------------------------------------------
009500121106       // Definizione key-list.
009600091223       //--------------------------------------------------------------
009700091223
009800091223
009900091223       //--------------------------------------------------------------
010000121107       // Definizione parametri procedura.
010100091223       //--------------------------------------------------------------
010200091223
010300091223     c     *Entry        plist
010400160921     c                   parm                    iVMSB2COpz
010500160921     c                   parm                    iVMSB2CDtRif
010600160921     c                   parm                    iVMSB2CLingua
010700160921     c                   parm                    iVMSB2CProc
010800160921     c                   parm                    iVMSB2CKSU
010900160921     c                   parm                    iVMSB2CCauBRT
011000160921     c                   parm                    oVMSB2CEsito
011100160921     c                   parm                    oVMSB2CCauTip
011200160921     c                   parm                    oVMSB2CCauTT
011300160921     c                   parm                    oVMSB2CStsCli
011400160921     c                   parm                    oVMSB2CStsOpz
011500160921     c                   parm                    oVMSB2CFasOp
011600160921     c                   parm                    oVMSB2CDDE
011700160921     c                   parm                    oVMSB2CDesc
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
012900160921       if iVMSB2COpz = *blank;
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
015200160921         clear oVMSB2CDDE     ;
015300160921         clear oVMSB2CCauTip  ;
015400160921         clear oVMSB2CCauTT   ;
015500160921         clear oVMSB2CStsCli  ;
015600160921         clear oVMSB2CStsOpz  ;
015700160921         clear oVMSB2CFasOp   ;
015800160921         clear oVMSB2CDesc    ;
015900160901
016000160901         // Inizializzo risultato a OK
016100160921         oVMSB2CEsito = '1';
016200160608
016300160608         // Controllo i parametri di input
016400160608         exsr sr_CtrlPIn;
016500160202
016600160202       ENDSR;
016700160202
016800160202       //--------------------------------------------------------------
016900160202       // Controllo i parametri di input
017000160202       //--------------------------------------------------------------
017100160202       BEGSR  sr_CtrlPIn;
017200160901
017300160901         // Lancio deve essere blank o 'C'
017400160921         if iVMSB2COpz <> *blank and iVMSB2COpz <>'C';
017500160901           // esco con errore sui parametri
017600160921           oVMSB2CEsito = 'I';
017700160901           exsr sr_RoutEnd;
017800160901         endif;
017900160901
018000160920         // la data di riferimento deve essere valorizzata
018100160914         // se non valorizzata, la imposto a oggi
018200160921         if iVMSB2CDtRif = 0;
018300160921           iVMSB2CDtRif = %dec(%date());
018400160914         endif;
018500160914
018600160914         // Procedura deve essere <> blank
018700160921         if iVMSB2CProc = *blank;
018800160901           // esco con errore sui parametri
018900160921           oVMSB2CEsito = 'I';
019000160901           exsr sr_RoutEnd;
019100160901         endif;
019200160920
019300160920         // Causale BRT deve essere <> blank
019400160921         if iVMSB2CCauBRT = *blank;
019500160920           // esco con errore sui parametri
019600160921           oVMSB2CEsito = 'I';
019700160920           exsr sr_RoutEnd;
019800160920         endif;
019900091223
020000091223       ENDSR;
020100121106
020200091223       //--------------------------------------------------------------
020300160217       // Esecuzione
020400091223       //--------------------------------------------------------------
020500160217       BEGSR  esegui;
020600160531
020700160920         // reperisco la data decorrenza rispetto alla data di riferimento
020800160921         iVMSDDEOpz = *blank;
020900160922         UBVMSDDER_S(iVMSDDEOpz:iVMSB2CProc:iVMSB2CKsu:iVMSB2CDtRif:
021000160921                   oVMSB2CDDE:oVMSDDEEsito);
021100160920
021200160920         // reperisco il rcd su TIVMS
021300160920         // potrebbero essercene più d'uno perché non ho la chiave univoca, comunque prendo il primo
021400160920         wSQL = 'select vmscautip, vmscautt, vmsstscli, vmsstsopz, vmsfasop,  +
021500160920                 vmsdescriz, vmsdescit, vmsdescen, vmsdescde, vmsdescfr, +
021600160920                 vmsdesces, vmsdde +
021700160901                 from tivms00f +
021800160901                 where +
021900160922                 vmsdde    = ' + %char(oVMSB2CDDE) + ' and +
022000160921                 vmsproc   = ''' + iVMSB2CProc + ''' and +
022100160921                 vmsksu    = ''' + iVMSB2CKSU  + ''' and +
022200160922                 vmscaubrt = ''' + iVMSB2CCauBRT + ''' and +
022300160922                 vmsatb    = '' '' +
022400160920                 fetch first 1 rows only';
022500160601
022600160531         // Dichiarazione cursore
022700160531         exec sql   prepare S0   from :wSQL;
022800160531         exec sql   declare C0   cursor   for S0;
022900160531         // Apro il cursore
023000160531         exec sql open C0;
023100160901         if sqlCode < 0;
023200160901           // esco con errore SQL
023300160921           oVMSB2CEsito = '2';
023400160901           exsr sr_RoutEnd;
023500160901         endif;
023600160531         // Leggo i dati
023700160531         exec sql
023800160920             fetch next from C0 into :DS_res;
023900160901
024000160531         select;
024100160531          // nessun rcd trovato
024200160531          when sqlCode = 100;
024300160901            // esco con errore tipo 3 (non trovato periodo)
024400160921            oVMSB2CEsito = '3';
024500160531            exec sql close C0;
024600160531            exsr sr_RoutEnd;
024700160531          // errore SQL
024800160531          when sqlCode < 0;
024900160531            exec sql close C0;
025000160901            // esco con errore SQL
025100160921            oVMSB2CEsito = '2';
025200160531            exsr sr_RoutEnd;
025300160920          // c'è il rcd, allora mi salvo i vari dati di output, decidendo che descrizione usare
025400160901          other;
025500160921            oVMSB2CCauTip = CauTip ;
025600160921            oVMSB2CCauTT  = CauTT  ;
025700160921            oVMSB2CStsCli = StsCli ;
025800160921            oVMSB2CStsOpz = StsOpz ;
025900160921            oVMSB2CFasOp  = FasOp  ;
026000160921            oVMSB2CDDE    = DDE    ;
026100160920            exsr DecideDesc;
026200160531         endsl;
026300160901
026400160901         // Quindi chiudo il cursore
026500160531         exec sql close C0;
026600160531
026700160531         // Se sono qui tutto OK
026800160921         oVMSB2CEsito = '1';
026900160217
027000160217       ENDSR;
027100160601
027200160601       //--------------------------------------------------------------
027300160920       // Descide che descrizione usare
027400160601       //--------------------------------------------------------------
027500160920       BEGSR  DecideDesc;
027600160601
027700160920         select;
027800160921          when iVMSB2CLingua = *blank;
027900160921           oVMSB2CDesc = Descriz;
028000160921          when iVMSB2CLingua = 'IT';
028100160921           oVMSB2CDesc = DescIT;
028200160921          when iVMSB2CLingua = 'EN';
028300160921           oVMSB2CDesc = DescEN;
028400160921          when iVMSB2CLingua = 'FR';
028500160921           oVMSB2CDesc = DescFR;
028600160921          when iVMSB2CLingua = 'DE';
028700160921           oVMSB2CDesc = DescDE;
028800160921          when iVMSB2CLingua = 'ES';
028900160921           oVMSB2CDesc = DescES;
029000160920          other                 ;
029100160921           oVMSB2CDesc = DescEN;
029200160920         endsl;
029300160920
029400160920       ENDSR;
029500160920
029600160920       //--------------------------------------------------------------
029700160920       // Operazioni finali.
029800160920       //--------------------------------------------------------------
029900160920       BEGSR  sr_RoutEnd;
030000160920
030100160601         // Chiusura pgm
030200160921         if iVMSB2COpz = 'C';
030300160601           *inlr = *on;
030400160601         else;
030500160901
030600160901           // se c'è un errore
030700160921           if oVMSB2CEsito <>'1';
030800160920             // azzero i parametri di output
030900160921             clear oVMSB2CCauTip  ;
031000160921             clear oVMSB2CCauTT   ;
031100160921             clear oVMSB2CStsCli  ;
031200160921             clear oVMSB2CStsOpz  ;
031300160921             clear oVMSB2CFasOp   ;
031400160921             clear oVMSB2CDesc    ;
031500160921             clear oVMSB2CDDE     ;
031600160901           endif;
031700160901
031800160601           *inrt = *on;
031900160601         endif;
032000160601         return;
032100160601
032200160601       ENDSR;
