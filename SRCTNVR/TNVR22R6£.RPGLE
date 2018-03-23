000100100507       //==============================================================
000200100510       //?TNVR22R6 * Visite di commerciali di filiale diversa da quella?
000300100510       //?           del relativo unificante                           ?
000400100507       //==============================================================
000500100507
000600100507       //--------------------------------------------------------------
000700100507       //?Specifiche di controllo.                                     ?
000800100507       //--------------------------------------------------------------
000900100507
001000100507     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100100507     h dftactgrp(*no)
001200100510     h bnddir('UBRTVNETA')
001300100507
001400100507       //--------------------------------------------------------------
001500100507       //?Dichiarazione file.                                          ?
001600100507       //--------------------------------------------------------------
001700100507
001800100507       // -?Trattative a clienti?
001900100510     fTNVIS02L  if   e           k disk    usropn
002000100510     f                                     extfile(wFLib)
002100100507
002200100507       // -?Tabelle?
002300100507     fTABEL00F  if   e           k disk
002400100507
002500100507       // -?Organigramma?
002600100507     fAZORG01L  if   e           k disk
002700100507
002800100507       // -?Stampa?
002900100507     fQSYSPRT   o    f  132        printer oflind(*inOF)
003000100507
003100100507       //--------------------------------------------------------------
003200100507       //?Definizione costanti.                                        ?
003300100507       //--------------------------------------------------------------
003400100507
003500100507     d c_DataMin       c                   const(20100401)
003600100507
003700100507       //--------------------------------------------------------------
003800100507       //?Definizione schiere.                                         ?
003900100507       //--------------------------------------------------------------
004000100507
004100100507
004200100507       //--------------------------------------------------------------
004300100507       //?Definizione aree dati.                                       ?
004400100507       //--------------------------------------------------------------
004500100507
004600100507       // -?Dati utente?
004700100507     d §AzUte        e ds                  extname(AZUTE00F)
004800100507     d                                     dtaara
004900100507     d §DatiUte      e ds                  extname(dDatiUte)
005000100507     d                                     dtaara
005100100507
005200100507       //--------------------------------------------------------------
005300100507       //?Definizione strutture dati.                                  ?
005400100507       //--------------------------------------------------------------
005500100507
005600100507       // -?Status ds?
005700100507     d Status         sds
005800100507     d  SDSpgm           *proc
005900100507     d  SDSusr               254    263
006000100507
006100100507       // -?Parametri per Reperimento dati utente?
006200100507     d TIBS34ds      e ds
006300100507
006400100507       // -?Tabella 01 = Commerciali?
006500100507     d ds01          e ds                  inz
006600100507
006700100507       // -?Tabella 05 = Codici Area?
006800100507     d ds05ccm       e ds                  extname(ds05)
006900100507     d                                     inz  prefix(ccm)
007000100507     d ds05uni       e ds                  extname(ds05)
007100100507     d                                     inz  prefix(uni)
007200100507
007300100507       // -?Data e Ora correnti?
007400100507     d ds_Time         ds            20    inz
007500100507     d   wDate                        8s 0 inz
007600100507     d   wTime                        6s 0 inz
007700100507     d   wMmSs                        6s 0 inz
007800100507
007900100507       //--------------------------------------------------------------
008000100507       //?Definizione variabili globali.                               ?
008100100507       //--------------------------------------------------------------
008200100510
008300100510       // -?Nome del sistema?
008400100510     d currSysNeta     s              8a   inz
008500100510
008600100510       // -?Nome libreria del file TNVIS02L?
008700100510     d wFLib           s             21    inz
008800100507
008900100507       // -?Codice filiale del commerciale?
009000100507     d ORGfil_ccm      s                   like(ORGfil)  inz
009100100507       // -?Codice area del commerciale?
009200100507     d ORGcar_ccm      s                   like(ORGcar)  inz
009300100507
009400100507       // -?Codice filiale dell'unificante?
009500100507     d ORGfil_uni      s                   like(ORGfil)  inz
009600100507       // -?Codice area dell'unificante?
009700100507     d ORGcar_uni      s                   like(ORGcar)  inz
009800100507
009900100507       // -?Campi di comodo?
010000100507     d wDate_Eur       s               d   inz  datfmt(*eur)
010100100507
010200100507       //--------------------------------------------------------------
010300100507       //?Definizione prototipi procedure e relativi parametri.        ?
010400100507       //--------------------------------------------------------------
010500100510
010600100510       // -?Reperimento NETA sistema AS/400 corrente?
010700100510      /copy gaitrasrc/srcProtoPr,UBRTVNETA
010800100507
010900100507       // -?Reperimento dati utente?
011000100507      /copy gaitrasrc/srcProtoPR,TIBS34R
011100100507
011200100507       //--------------------------------------------------------------
011300100507       //?Definizione key-list.                                        ?
011400100507       //--------------------------------------------------------------
011500100507
011600100507       // -?File TNVIS02L?
011700100507     d k04tnvis02    e ds                  extname(TNVIS02L : *key)
011800100507     d                                     inz  prefix(k_)
011900100507
012000100507       // -?File TABEL00F?
012100100507     d k03tabel00    e ds                  extname(TABEL00F : *key)
012200100507     d                                     inz  prefix(k_)
012300100507
012400100507       //--------------------------------------------------------------
012500100507       //?M A I N - L I N E                                            ?
012600100507       //--------------------------------------------------------------
012700100507
012800100507      /free
012900100507
013000100507       // -?Operazioni iniziali?
013100100507       exsr sr_RoutInz;
013200100507
013300100507       // -?Ciclo di lettura?
013400100507       clear  k04tnvis02;
013500100507       setll  %kds(k04tnvis02)  TNVIS000;
013600100507       read  TNVIS000;
013700100507       DoW  Not %eof(TNVIS02L);
013800100507         if  VISdvi < c_DataMin;
013900100507           k_VIScmm = VIScmm;
014000100507           k_VISdvi = c_DataMin;
014100100507           clear  k_VIShmv;
014200100507           clear  k_VISnrv;
014300100507           setll  %kds(k04tnvis02)  TNVIS000;
014400100507         else;
014500100507           exsr  sr_Elabora;
014600100507         endif;
014700100507         read  TNVIS000;
014800100507       EndDo;
014900100507
015000100507       // -?Operazioni finali?
015100100507       exsr sr_RoutEnd;
015200100507
015300100507       //--------------------------------------------------------------
015400100507       //?Operazioni iniziali.                                         ?
015500100507       //--------------------------------------------------------------
015600100507       BEGSR  sr_RoutInz;
015700100507
015800100507         *inLR = *on;
015900100510
016000100510         // -?Apertura file TNVIS02L?
016100100510         select;
016200100510           when  UBRTVNETA_Rtv(currSysNeta) <> *zero;
016300100510             exsr  sr_RoutEnd;
016400100510           when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
016500100510             wFLib = 'FILTRAGRU/TNVIS02L';
016600100510           other;
016700100510             wFLib = 'FILTRAGRPF/TNVIS02L';
016800100510         endsl;
016900100510         open  TNVIS02L ;
017000100507
017100100507         // -?Reperimento dati job?
017200100507         exsr  sr_DatiJob;
017300100507
017400100507         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
017500100507         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
017600100507         ds_Time   = %char( %dec( %timestamp() ) );
017700100507         wDate_Eur = %date( wDate : *iso );
017800100507         wDate     = %dec( wDate_Eur );
017900100507
018000100507         // -?Impostazione campi fissi?
018100100507         k_TBLkut = 1;
018200100507
018300100507         // -?Stampa testata?
018400100507         except  PrtTxt;
018500100507         *inOF = *off;
018600100507
018700100507       ENDSR;
018800100507
018900100507       //--------------------------------------------------------------
019000100507       //?Reperimento Dati del job (Utente/Operativi).                 ?
019100100507       //--------------------------------------------------------------
019200100507       BEGSR  sr_DatiJob;
019300100507
019400100507         in(E) §AzUte;
019500100507         if NOT %error;
019600100507           in(E) §DatiUte;
019700100507         endif;
019800100507         if %error or RSut = *blanks;
019900100507           clear TIBS34ds;
020000100507           tibs34r ( tibs34ds );
020100100507           in §AzUte;
020200100507           in §DatiUte;
020300100507         endif;
020400100507
020500100507       ENDSR;
020600100507
020700100507       //--------------------------------------------------------------
020800100507       //?Elaborazione singolo record del file TNVIS00F.               ?
020900100507       //--------------------------------------------------------------
021000100507       BEGSR  sr_Elabora;
021100100507
021200100507         // -?Reperimento dati commerciale?
021300100507         if  %editc( VIScmm : 'X' ) <> %trimr( k_TBLkey );
021400100507           clear  ds01;
021500100507           k_TBLcod = '01';
021600100507           k_TBLkey = %editc( VIScmm : 'X' );
021700100507           chain  %kds(k03tabel00)  TABEL;
021800100507           if  %found(TABEL00F)   and   TBLflg = *blank;
021900100507             ds01 = TBLuni;
022000100507           endif;
022100100507         endif;
022200100507
022300100507         // -?Stampa SE filiale CCM <> filiale RGF (unificante)?
022400100507         if  %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) <>
022500100507             %subst( %editc( §01rgf : 'X' ) : 1 : 3 );
022600100507           exsr  sr_Stampa;
022700100507         endif;
022800100507
022900100507       ENDSR;
023000100507
023100100507       //--------------------------------------------------------------
023200100507       //?Stampa singola visita cliente.                               ?
023300100507       //--------------------------------------------------------------
023400100507       BEGSR  sr_Stampa;
023500100507
023600100507         // -?Reperimento area commerciale?
023700100507         IF  ORGfil_ccm <>
023800100507                  %int( %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) );
023900100507           ORGfil_ccm =
024000100507                  %int( %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) );
024100100507           clear  ORGcar_ccm;
024200100507           clear  ds05ccm;
024300100507           chain  (ORGfil_ccm)  AZORG;
024400100507           If  %found(AZORG01L);
024500100507             ORGcar_ccm = ORGcar;
024600100507             k_TBLcod = '05';
024700100507             k_TBLkey = %editc( ORGcar : 'X' );
024800100507             chain  %kds(k03tabel00)  TABEL;
024900100507             if  %found(TABEL00F)   and   TBLflg = *blank;
025000100507               ds05ccm = TBLuni;
025100100507             endif;
025200100507           EndIf;
025300100507         ENDIF;
025400100507
025500100507         // -?Reperimento area unificante?
025600100507         IF  ORGfil_uni <>
025700100507                  %int( %subst( %editc( §01rgf : 'X' ) : 1 : 3 ) );
025800100507           ORGfil_uni =
025900100507                  %int( %subst( %editc( §01rgf : 'X' ) : 1 : 3 ) );
026000100507           clear  ORGcar_uni;
026100100507           clear  ds05uni;
026200100507           chain  (ORGfil_uni)  AZORG;
026300100507           If  %found(AZORG01L);
026400100507             ORGcar_uni = ORGcar;
026500100507             k_TBLcod = '05';
026600100507             k_TBLkey = %editc( ORGcar : 'X' );
026700100507             chain  %kds(k03tabel00)  TABEL;
026800100507             if  %found(TABEL00F)   and   TBLflg = *blank;
026900100507               ds05uni = TBLuni;
027000100507             endif;
027100100507           EndIf;
027200100507         ENDIF;
027300100507
027400100507         // -?Stampa solo se aree diverse?
027500100507         if  ORGcar_ccm = ORGcar_uni;
027600100507           leavesr;
027700100507         endif;
027800100507
027900100507         // -?Stampa?
028000100507         if  *inOF;
028100100507           except  PrtTxt;
028200100507           *inOF = *off;
028300100507         endif;
028400100507         except  PrtDet;
028500100507
028600100507       ENDSR;
028700100507
028800100507       //--------------------------------------------------------------
028900100507       //?Operazioni finali.                                           ?
029000100507       //--------------------------------------------------------------
029100100507       BEGSR  sr_RoutEnd;
029200100507
029300100507         // -?"Chiusura" spool di stampa?
029400100507         if  %open(Qsysprt);
029500100507           except  PrtEnd;
029600100507           close  Qsysprt;
029700100507         endif;
029800100507
029900100507         // -?"Chiusura" pgm?
030000100507         return;
030100100507
030200100507       ENDSR;
030300100507
030400100507      /end-free
030500100507
030600100507      //---------------------------------------------------------------
030700100507      //?Spool di stampa.                                              ?
030800100507      //---------------------------------------------------------------
030900100507
031000100507     oQSYSPRT   e            PrtTxt            2
031100100507     o                       RSut
031200100510     o                                        +   5 'VISITE DI COMMERC-
031300100510     o                                              IALI DI FILIALE DI-
031400100510     o                                              VERSA DA QUELLA DE-
031500100510     o                                              LL''UNIFICANTE'
031600100510     o                       SDSpgm           +   5
031700100510     o                       SDSusr           +   3
031800100510     o                       wDate         y  +   3
031900100507     o          e            PrtTxt      0
032000100510     o                                        +  25 'VISITE DI COMMERC-
032100100510     o                                              IALI DI FILIALE DI-
032200100510     o                                              VERSA DA QUELLA DE-
032300100510     o                                              LL''UNIFICANTE'
032400100507     o          e            PrtTxt      1
032500100507     o*//                    kNSif
032600100507     o                       wTime            + 122 '  :  :  '
032700100507     o          e            PrtTxt      2
032800100507     o                                              'Nr.Visita'
032900100507     o                                        +   2 'Commerciale               -
033000100507     o                                                       '
033100100507     o                                        +   2 'Area commerciale          -
033200100507     o                                                   '
033300100507     o                                        +   2 'Unificante'
033400100507     o                                        +   2 'Area unificante           -
033500100507     o                                                   '
033600100507     o          e            PrtTxt      1  1
033700100507     o                                              '---------'
033800100507     o                                        +   2 '---------------------------
033900100507     o                                              ---------'
034000100507     o                                        +   2 '---------------------------
034100100507     o                                              -----'
034200100507     o                                        +   2 '----------'
034300100507     o                                        +   2 '---------------------------
034400100507     o                                              -----'
034500100507     o          e            PrtDet      1
034600100507     o                       VISnrv        j
034700100507     o                       VIScmm           +   1
034800100507     o                                        +   1 '-'
034900100507     o                       §01age           +   1
035000100507     o                       ORGcar_ccm       +   2
035100100507     o                                        +   1 '-'
035200100507     o                       ccm§05des        +   1
035300100507     o                       §01rgf           +   2
035400100507     o                       ORGcar_uni       +   5
035500100507     o                                        +   1 '-'
035600100507     o                       uni§05des        +   1
035700100507     o          e            PrtEnd      2
035800100507     o                                        +  25 '***  Fine Lista  ***'
