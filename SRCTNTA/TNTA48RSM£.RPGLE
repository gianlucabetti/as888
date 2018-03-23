000100141008       //==============================================================
000200141008       //?TNTA48R - Controllo singola Tariffa/Offerta per Cliente.     ?
000300141008       //==============================================================
000400141008
000500141008       //--------------------------------------------------------------
000600141008       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141008       //--------------------------------------------------------------
000800141008
000900141008     /*PRM  dbgview(*source)
001000141008     /*END
001100141008
001200141008       //--------------------------------------------------------------
001300141008       //?Specifiche di controllo.                                     ?
001400141008       //--------------------------------------------------------------
001500141008
001600141008     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141008     h dftactgrp(*no)
001800141008     h bnddir('UBRTVNETA':'TRUL')
001900141008
002000141008       //--------------------------------------------------------------
002100141008       //?Dichiarazione file.                                          ?
002200141008       //--------------------------------------------------------------
002300141008
002400141008       // -?Tariffe Clienti: Testata?
002500141008     fTNTAM01L  if   e           k disk    usropn
002600141008       // -?Offerte Clienti: Testata?
002700141008     fTNOFM01L  if   e           k disk    usropn
002800141008     f                                     rename( TNTAM000 : TNOFM000 )
002900141008
003000141008       // -?Tariffe Clienti: Dettaglio?
003100141008     fTITAD04L  if   e           k disk    usropn
003200141008       // -?Offerte Clienti: Dettaglio?
003300141008     fTIOFD01L  if   e           k disk    usropn
003400141008     f                                     rename( TITAD000 : TIOFD000 )
003500141020
003600141020       // -?Tariffe Particolari: Testata?
003700141020     fTITPT01L  if   e           k disk    usropn
003800141020       // -?Offerte Particolari: Testata?
003900141020     fTIOPT01L  if   e           k disk    usropn
004000141020     f                                     rename( TITPT000 : TIOPT000 )
004100141020
004200141020       // -?Tariffe Particolari: Dettaglio?
004300141020     fTITAD04L  if   e           k disk    usropn
004400141020       // -?Offerte Particolari: Dettaglio?
004500141020     fTIOPD01L  if   e           k disk    usropn
004600141020     f                                     rename( TITPD000 : TIOPD000 )
004700141008
004800141008       // -?Anagrafica P.D.C. per Tariffe?
004900141008     fTFACO00F  if   e           k disk    usropn
005000141008     f                                     rename( CNACO000 : TFACO000 )
005100141008     fTFIND00F  if   e           k disk    usropn
005200141008     f                                     rename( CNIND000 : TFIND000 )
005300141008     fTFCLP00F  if   e           k disk    usropn
005400141008     f                                     rename( CNCLP000 : TFCLP000 )
005500141022
005600141022       // -?Tabelle?
005700141022     fTABEL00F  if   e           k disk
005800141008
005900141008       //--------------------------------------------------------------
006000141008       //?Definizione costanti.                                        ?
006100141008       //--------------------------------------------------------------
006200141008
006300141009       // -?n° Max degli Scaglioni?
006400141009     d c_MaxSgl        c                   const(18)
006500141008
006600141008       //--------------------------------------------------------------
006700141008       //?Definizione schiere.                                         ?
006800141008       //--------------------------------------------------------------
006900141008
007000141008       // -?Messaggi di errore?
007100141008     d sk_Msg          s             78    dim(20)  ctdata  perrcd(1)
007200141009
007300141009       // -?Codici Tassazione?
007400141009     d sk_CT           s                   like(TADcts)     dim(600)
007500141009     d                                     inz
007600141009       // -?Sigle Regioni di appartenenza?
007700141009     d sk_CTrap        s                   like(§CTrap)     dim(600)
007800141009     d                                     inz
007900141009       // -?Codici Regione (da ordinamento in tab. "CT")?
008000141009     d sk_CReg         s                   like(TADcts)     dim(200)
008100141009     d                                     inz
008200141009
008300141009       // -?Scaglioni Tariffe?
008400141010     d*// sk_Sgl          s                   like(TADsgl)     dim(c_MaxSgl)
008500141013     d sk_Sgl          s              7  2 dim(c_MaxSgl)    inz
008600141020     d sk_Ssl          s              7s 2 dim( 6)          inz
008700141020
008800141020       // -?Tariffe Inoltro?
008900141020     d sk_Pro          s              7  3 dim(c_MaxSgl)    inz
009000141013
009100141013       // -?Codici Tassazione?
009200141013     d*// sk_Tct          s                   like(TADitr)     dim( 6)
009300141017     d ds_sk_TCT       ds                  inz
009400141017     d   sk_Tct                       7s 3 dim( 6)          inz
009500141022
009600141022       // -?Province per Regione?
009700141022     d sk_Big          s             42    dim(15)          inz
009800141013
009900141022       // -?Ordinamento Province per Regione?
010000141013     d*// sk_Brg          s                   like(TADorp)     dim(15)
010100141013     d sk_Brg          s              1    dim(15)          inz
010200141022
010300141022       // -?Codici Consegna Particolare?
010400141022     d sk_1Pftc        s                   like(TPDftc)  dim(50)  inz
010500141022     d sk_1Ptst        s                   like(§1Ptst)  dim(50)  inz
010600141008
010700141008       //--------------------------------------------------------------
010800141008       //?Definizione aree dati.                                       ?
010900141008       //--------------------------------------------------------------
011000141008
011100141008       // -?Dati utente?
011200141008     d §AzUte        e ds                  extname(AZUTE00F)
011300141008     d                                     dtaara
011400141008     d §DatiUte      e ds                  extname(dDatiUte)
011500141008     d                                     dtaara
011600141008
011700141008       //--------------------------------------------------------------
011800141008       //?Definizione strutture dati.                                  ?
011900141008       //--------------------------------------------------------------
012000141008
012100141008       // -?Parametri ricevuti?
012200141008     d KPJBA         e ds                  qualified
012300141009     d TNTA48ds      e ds                  qualified     inz
012400141009
012500141009       // -?Tab. "CT" = Codici di Tassazione?
012600141009     d dsCT          e ds                  inz
012700141022
012800141022       // -?Tab. "1P" = Consegne Particolari Clienti?
012900141022     d ds1P          e ds                  inz
013000141008
013100141008       //--------------------------------------------------------------
013200141008       //?Definizione variabili globali.                               ?
013300141008       //--------------------------------------------------------------
013400141008
013500141009       // -?Indici di schiera?
013600141009     d xK              s              3  0 inz
013700141009     d xK1             s              3  0 inz
013800141017     d xK4             s              3  0 inz
013900141013     d xK5             s              3  0 inz
014000141017     d xP              s              3  0 inz
014100141009     d xY              s              3  0 inz
014200141010     d xZ              s              3  0 inz
014300141022     d YY              s              3  0 inz
014400141009
014500141009       // -?Flags Booleani?
014600141010     d $cap1scaglione  s               n   inz ........................
014700141009     d $CtrCAP         s               n   inz ........................
014800141021     d*// $Inoltro        s               n   inz
014900141021     d*// $StaLNP         s               n   inz
015000141013     d $TariffePartic  s               n   inz
015100141022     d $RecVal         s               n   inz
015200141009
015300141009       // -?Campi di comodo?
015400141009     d wSaveLNP        s                   like(TADlnp)  inz
015500141009     d wSaveCTS        s                   like(TADcts)  inz
015600141009     d wSaveCAP        s                   like(TADcap)  inz
015700141010     d wSavePRO        s              1    inz
015800141010     d wSaveREG        s              2    inz
015900141010     d wComPRO         s              1    inz
016000141010     d wComREG         s              2    inz
016100141020     d work_LNP        s                   like(TADlnp)  inz
016200141020     d work_CTS        s                   like(TADcts)  inz
016300141020     d work_CAP        s                   like(TADcap)  inz
016400141020     d work_PRO        s              1    inz
016500141020     d work_REG        s              2    inz
016600141009
016700141010     d wCodREG         s                   like(§CTrap)  inz
016800141010
016900141013     d wCodORP         s              1    inz
017000141009     d wNrScaglioni    s              3  0 inz
017100141010     d wNumDec         s              2  0 inz
017200141010     d w0072           s              7  2 inz
017300141013     d wNumPDE         s              3  0 inz
017400141022     d wRecVal         s              3  0 inz
017500141008
017600141008       //--------------------------------------------------------------
017700141008       //?Definizione prototipi procedure.                             ?
017800141008       //--------------------------------------------------------------
017900141008
018000141008       // -?Reperimento dati utente?
018100141008     d TIBS34ds      e ds
018200141008      /copy gaitrasrc/srcProtoPR,TIBS34R
018300141008
018400141008       // -?Controllo/Decodifica cliente?
018500141008      /copy gaitrasrc/srcProtoPI,TIBS69R
018600141008      /copy gaitrasrc/srcProtoPR,TIBS69R
018700141008
018800141008       //--------------------------------------------------------------
018900141008       //?Definizione key-list.                                        ?
019000141008       //--------------------------------------------------------------
019100141008
019200141008       // -?File TNTAM01L / TNOFM01L?
019300141008     d keyTNTAM01    e ds                  extname(TNTAM01L : *key)
019400141008     d                                     prefix(k_)   inz
019500141008
019600141008       // -?File TITAD04L / TIOFD01L?
019700141008     d keyTITAD01    e ds                  extname(TITAD04L : *key)
019800141008     d                                     prefix(k_)   inz
019900141022
020000141022       // -?File TFACO00F / TFIND00F / TFCLP00F?
020100141022     d keyTFACO00    e ds                  extname(TFACO00F : *key)
020200141022     d                                     prefix(k_)   inz
020300141022
020400141022       // -?File TABEL00F?
020500141022     d keyTABEL00    e ds                  extname(TABEL00F : *key)
020600141022     d                                     prefix(k_)   inz
020700141008
020800141008       //--------------------------------------------------------------
020900141008       //?M A I N - L I N E                                            ?
021000141008       //--------------------------------------------------------------
021100141008
021200141008     c     *Entry        plist
021300141008     c                   parm                    TNTA48ds
021400141008
021500141008      /free
021600141008
021700141008       // -?Operazioni iniziali?
021800141008       exsr  sr_RoutInz;
021900141008
022000141008       // -?Controlli sulla singola Tariffa/Offerta per Cliente?
022100141008       select;
022200141008         when  TNTA48ds.iTA48cto = 'T';
022300141008           exsr  sr_Ctrl_Tariffa;
022400141008         when  TNTA48ds.iTA48cto = 'O';
022500141008           exsr  sr_Ctrl_Offerta;
022600141008       endsl;
022700141022
022800141022       dsTA01 = TAMflo;
022900141022
023000141022       // -?Caricamento delle Tariffe Particolari?
023100141022       exsr  sr_CarConsegnePartic;
023200141010
023300141010       // -?Controlli sugli Scaglioni?
023400141010       exsr  sr_Scaglioni;
023500141010
023600141010       //// -?Controlli sull'Inoltro?
023700141010       //exsr  sr_Inoltro;
023800141010
023900141010       // -?Controlli sul Dettaglio Tariffario?
024000141017       $TariffePartic = *off;
024100141010       exsr  sr_Dettaglio;
024200141013
024300141013       // -?Controlli sulle Tariffe Particolari?
024400141013       $TariffePartic = *on;
024500141022       exsr  sr_TariffeParticolari;
024600141008
024700141008       // -?Operazioni finali?
024800141008       exsr  sr_RoutEnd;
024900141008
025000141008       //--------------------------------------------------------------
025100141008       //?Operazioni iniziali.                                         ?
025200141008       //--------------------------------------------------------------
025300141008       BEGSR  sr_RoutInz;
025400141008
025500141008         *inLR  = *on;
025600141008
025700141008         TNTA48ds.oTA48err = *off;
025800141008         clear  TNTA48ds.oTA48msg;
025900141008
026000141008         // -?Controllo parametri ricevuti?
026100141008         select;
026200141009           when  TNTA48ds.iTA48dsf = *blank  or
026300141009                 TNTA48ds.iTA48cto = *blank  or
026400141009                 TNTA48ds.iTA48ksc = *zero   or
026500141009                 TNTA48ds.iTA48ctr = *zero;
026600141008             TNTA48ds.oTA48err = *on;
026700141013             TNTA48ds.oTA48msg = sk_Msg(17);
026800141008             exsr  sr_RoutEnd;
026900141009           when  ( TNTA48ds.iTA48dsf <> 'S'  and
027000141009                   TNTA48ds.iTA48dsf <> 'F' )   OR
027100141009                 ( TNTA48ds.iTA48cto <> 'T'  and
027200141009                   TNTA48ds.iTA48cto <> 'O' )   OR
027300141008             TNTA48ds.oTA48err = *on;
027400141013             TNTA48ds.oTA48msg = sk_Msg(18);
027500141008             exsr  sr_RoutEnd;
027600141009         endsl;
027700141008
027800141008         // -?Reperimento dati job?
027900141008         exsr  sr_DatiJob;
028000141008
028100141008         // -?Apertura archivi?
028200141008         select;
028300141008           when  TNTA48ds.iTA48cto = 'T';
028400141008             open  TNTAM01L;
028500141008             open  TITAD04L;
028600141008           when  TNTA48ds.iTA48cto = 'O';
028700141008             open  TNOFM01L;
028800141008             open  TIOFD01L;
028900141008             open  TIVIS01L;
029000141008             open  TFACO00F;
029100141008             open  TFIND00F;
029200141008             open  TFCLP00F;
029300141008         endsl;
029400141009
029500141009         // -?Caricamento iniziale delle tabelle?
029600141009         exsr  sr_CarTab;
029700141008
029800141008       ENDSR;
029900141008
030000141008       //--------------------------------------------------------------
030100141008       //?Reperimento Dati del job (Utente/Operativi).                 ?
030200141008       //--------------------------------------------------------------
030300141008       BEGSR  sr_DatiJob;
030400141008
030500141008         in(E) §AzUte;
030600141008         if NOT %error;
030700141008           in(E) §DatiUte;
030800141008         endif;
030900141008         if %error or RSut = *blanks;
031000141008           clear TIBS34ds;
031100141008           tibs34r ( tibs34ds );
031200141008           in §AzUte;
031300141008           in §DatiUte;
031400141008         endif;
031500141008
031600141008       ENDSR;
031700141009
031800141009       //--------------------------------------------------------------
031900141009       //?Caricamento iniziale delle tabelle.                          ?
032000141009       //--------------------------------------------------------------
032100141009       BEGSR  sr_CarTab;
032200141009
032300141009         clear  keyTABEL00;
032400141009         k_TBLkut = 1;
032500141022         k_TBLcod = 'CT';
032600141009
032700141009         // -?Schiera di tutti i Codici Tassazione?
032800141009         clear  xK;
032900141009         clear  xK1;
033000141009         clear  sk_CT;         //?(Codici Tassazione)?
033100141009         clear  sk_CTrap;      //?(Sigle  Regioni di Appartenenza)?
033200141009         clear  sk_CReg;       //?(Codici Regione)?
033300141022
033400141009         setll  %kds( keyTABEL00 )  TABEL;
033500141009         reade  %kds( keyTABEL00 : 2 )  TABEL;
033600141009
033700141009         DoW  NOT %eof(TABEL00F);
033800141009
033900141009           If  TBLkey <> *blank;
034000141009
034100141009             dsCT = TBLuni;
034200141009             xK += 1;
034300141009             sk_CT(xK)    = TBLkey;
034400141009             sk_CTrap(xK) = §CTrap;
034500141009
034600141009             // -?SE è una Regione: caricamento schiera delle Regioni?
034700141009             if  %subst( §CTcor : %len(§CTcor) : 1 ) = '9';
034800141009               xK1 += 1;
034900141009               sk_CReg(xK1) = TBLkey;
035000141009             endif;
035100141009
035200141009           EndIf;
035300141009
035400141009           reade  %kds( keyTABEL00 : 2 )  TABEL;
035500141009
035600141009         EndDo;
035700141009
035800141009         // -?Schiera delle Nazioni (SOLO quella ITALIA)?
035900141009         clear  xK;
036000141009         clear  sk_15;         //?(Codici Nazione)?
036100141009         k_TBLcod = '15';
036200141009         setll  %kds( keyTABEL00 )  TABEL;
036300141009         reade  %kds( keyTABEL00 : 2 )  TABEL;
036400141009
036500141009         DoW  NOT %eof(TABEL00F);
036600141009
036700141009           If  TBLkey <> *blank;
036800141009
036900141009             dsCT = TBLuni;
037000141009             if  §15ita = 'I';
037100141009               xK += 1;
037200141009               sk_15(xK) = TBLkey;
037300141009             endif;
037400141009
037500141009           EndIf;
037600141009
037700141009           reade  %kds( keyTABEL00 : 2 )  TABEL;
037800141009
037900141009         EndDo;
038000141009
038100141009       ENDSR;
038200141008
038300141008       //--------------------------------------------------------------
038400141008       //?Controlli sulla singola TARIFFA per cliente.                 ?
038500141008       //--------------------------------------------------------------
038600141008       BEGSR  sr_Ctrl_Tariffa;
038700141008
038800141008         // -?Dati anagrafici del cliente?
038900141008         clear  TIBS69ds;
039000141008         I69kac = iTA48ksc;
039100141008         exsr  sr_AnagrCliente;
039200141017
039300141017         // -?Aggancio testata Tariffa?
039400141021         //  ?con Codice Cliente Tariffa = Numero Visita?
039500141017         clear  keyTNTAM01;
039600141017         k_TAMksc = iTA48ksc;
039700141017         k_TAMctr = iTA48ctr;
039800141017         k_TAMprg = iTA48prg;
039900141017         chain  %kds( keyTNTAM01 )  TNTAM000;
040000141008
040100141008       ENDSR;
040200141008
040300141008       //--------------------------------------------------------------
040400141008       //?Controlli sulla singola OFFERTA per cliente.                 ?
040500141008       //--------------------------------------------------------------
040600141008       BEGSR  sr_Ctrl_Offerta;
040700141008
040800141008         // -?Visita?
040900141008         chain  ( iTA48ksc )  TIVIS000;
041000141008
041100141009         If  NOT %found(TIVIS01L);
041200141008           TNTA48ds.oTA48err = *on;
041300141008           TNTA48ds.oTA48msg = sk_Msg(02);
041400141008           exsr  sr_RoutEnd;
041500141009         EndIf;
041600141008
041700141008         // -?Dati anagrafici del cliente - dal file delle Visite?
041800141008         k_ACOkut = 1;
041900141008         k_ACOkcc = DUTkci;
042000141008         k_ACOksc = VISnrv;
042100141008
042200141008         chain  %kds( keyTFACO00 )  TFACO000;
042300141008         chain  %kds( keyTFACO00 )  TFIND000;
042400141008         chain  %kds( keyTFACO00 )  TFCLP000;
042500141008
042600141008         // -?SE non trovato: dall'Anagrafica Clienti?
042700141008         If  NOT %found(TFACO00F)  or
042800141008             NOT %found(TFIND00F)  or
042900141008             NOT %found(TFCLP00F);
043000141008
043100141008           clear  TIBS69ds;
043200141009           I69kac = VISksc;
043300141008           exsr  sr_AnagrCliente;
043400141008
043500141008         EndIf;
043600141017
043700141017         // -?Aggancio testata Offerta?
043800141021         //  ?con Codice Cliente Tariffa?
043900141017         clear  keyTNTAM01;
044000141017         k_TAMksc = iTA48ksc;
044100141017         k_TAMctr = iTA48ctr;
044200141017         k_TAMprg = iTA48prg;
044300141017         chain  %kds( keyTNTAM01 )  TNOFM000;
044400141008
044500141008       ENDSR;
044600141008
044700141008       //--------------------------------------------------------------
044800141008       //?Controllo dati Anagrafici del Cliente.                       ?
044900141008       //--------------------------------------------------------------
045000141008       BEGSR  sr_AnagrCliente;
045100141008
045200141008         I69kcc = DUTkci;
045300141008         I69kin = I69kac;
045400141008         I69kcp = I69kac;
045500141008
045600141008         tibs69r( tibs69ds :
045700141008                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
045800141008
045900141009         If  O69err = *on;
046000141008           TNTA48ds.oTA48err = *on;
046100141008           TNTA48ds.oTA48msg = sk_Msg(03);
046200141008           exsr  sr_RoutEnd;
046300141009         EndIf;
046400141008
046500141008       ENDSR;
046600141022
046700141022       //--------------------------------------------------------------
046800141022       //?Caricamento delle Consegne Particolari che interessano la    ?
046900141022       //?  Tariffa che si sta elaborando.                             ?
047000141022       //--------------------------------------------------------------
047100141022       BEGSR  sr_CarConsegnePartic;
047200141022
047300141022         // -?Schiere delle Consegne Particolari?
047400141022         clear  xK;
047500141022         clear  sk_1Pftc;      //?(Codici Consegna Particolare)?
047600141022         clear  sk_1Ptst;      //?(Tipo Stampa)?
047700141022
047800141022         clear  keyTABEL00;
047900141022         k_TBLkut = 1;
048000141022         k_TBLcod = '1P';
048100141022
048200141022         setll  %kds( keyTABEL00 )  TABEL;
048300141022         reade  %kds( keyTABEL00 : 2 )  TABEL;
048400141022
048500141022         DoW  NOT %eof(TABEL00F);
048600141022
048700141022           ds1P = TBLuni;
048800141022
048900141022           Select;
049000141022
049100141022             // -?Tariffa Annullata?
049200141022             When  TBLkey <> *blank;
049300141022
049400141022             // -?Tariffa Particolare "C       " (Lasciato Avviso)?
049500141022             //  ?per le filiali NON abilitate: si accetta Cartello?
049600141022             //  ?(NON si elabora)?
049700141022             When  TBLkey =  'C       '  and  wComFil <> 888  and
049800141022                   %lookup( wComFil : sk_Lav ) = *zero        and
049900141022                   %lookup( 999     : sk_Lav ) = *zero;
050000141022
050100141022             // -?Per Tariffa con Servizio Poste: si escludono le?
050200141022             //  ?tariffe particolari con il flag §1PFPT = "N"?
050300141022             When  TAMtsp =  'P'  and  §1Pfpt = 'N';
050400141022
050500141022             // -?Per Tariffa con Network DPD: si escludono le?
050600141022             //  ?tariffe particolari con il flag §1PFDP = "N"?
050700141022             When  dsTA01.§TAdpd = 'P'  and  §1Pfdp = 'N';
050800141022
050900141022             // -?(Per Tariffa con Network FedEx: NON si fanno?
051000141022             //  ?controlli perchè NON vengono lette)?
051100141022
051200141022             // -?Per Tariffa EuroExpress: si escludono le?
051300141022             //  ?tariffe particolari con il flag §1PFIE = "N"?
051400141022             When  TAMfie =  'E'  and  §1Pfir = 'N';
051500141022
051600141022             // -?Per Tariffa Italia: si escludono le tariffe?
051700141022             //  ?particolari con il flag §1PTCO = "N" o Servizio Poste?
051800141022             When  TAMfie =  'I'  and  TAMfie <> 'P'  and  §1Ptco = 'N';
051900141022
052000141022             // -?Per Tariffa di Cartello: si verifica che NON sia?
052100141022             //  ?tipo servizio "P"?
052200141022             When  TAMfie =  *blank  and  TAMtsp <> 'P'  and  §1Ptco = 'N';
052300141022
052400141022             // -?Tariffa OK?
052500141022             Other;
052600141022               xK += 1;
052700141022               sk_1Pftc(xK) = TBLkey;
052800141022               sk_1Ptst(xK) = §1Ptst;
052900141022
053000141022           EndSl;
053100141022
053200141022           reade  %kds( keyTABEL00 : 2 )  TABEL;
053300141022
053400141022         EndDo;
053500141022
053600141022       ENDSR;
053700141008
053800141008       //--------------------------------------------------------------
053900141008       //?Controllo Scaglioni.                                         ?
054000141008       //--------------------------------------------------------------
054100141008       BEGSR  sr_Scaglioni;
054200141009
054300141009         clear  xK;
054400141009         clear  wSaveLNP;
054500141009         clear  wSaveCTS;
054600141009         clear  wSaveCAP;
054700141010         clear  wNumDec;
054800141022         $cap1scaglione = *off;
054900141022         $CtrCAP = *on;
055000141008
055100141021         // -?Impostazione chiave parziale per l'accesso agli scaglioni?
055200141021         clear  keyTITAD04;
055300141010         k_TADksc = iTA48ksc;
055400141010         k_TADctr = iTA48ctr;
055500141010         k_TADprg = iTA48prg;
055600141021         Select;
055700141021           // -?Se Tariffa: si recupera la filiale dal codice cliente?
055800141021           When  iTA48cto = 'T';
055900141021             k_TADlnp = iTA48ksc / 10000;
056000141021           // -?Se Visita ad un Cliente Nuovo: si recupera la filiale?
056100141021           //  ?dai primi 3 campi del codice Commerciale?
056200141021           When  VISksc = *zero;
056300141021             k_TADlnp = VIScmm / 10000;
056400141021           // -?Se Visita ad un Cliente esistente: si recupera la?
056500141021           //  ?filiale di appartenenza dal cliente?
056600141021           Other;
056700141021             k_TADlnp = VISksc / 10000;
056800141021         EndSl;
056900141010
057000141008         // -?Ricerca DETTAGLIO: prima con la LNP del cliente?
057100141008         if  iTA48cto = 'T';
057200141008           setll  %kds( keyTITAD04 : 4 )  TITAD000;
057300141008         else;
057400141008           setll  %kds( keyTITAD04 : 4 )  TIOFD000;
057500141008         endif;
057600141008
057700141008         // -?Se NON trovato:?
057800141008         IF  NOT %equal;
057900141008
058000141008           // -?Ricerca DETTAGLIO: senza la LNP del cliente?
058100141008           if  iTA48cto = 'T';
058200141008             setll  %kds( keyTITAD04 : 3 )  TITAD000;
058300141008           else;
058400141008             setll  %kds( keyTITAD04 : 3 )  TIOFD000;
058500141008           endif;
058600141008
058700141022           // -?Se NON trovato:?ERRORE?
058800141008           //  ?(manca dettaglio tariffario)?
058900141009           If  NOT %equal;
059000141022
059100141008             TNTA48ds.oTA48err = *on;
059200141008             TNTA48ds.oTA48msg = sk_Msg(04);
059300141008             exsr  sr_RoutEnd;
059400141022
059500141009           EndIf;
059600141008
059700141008         ENDIF;
059800141009
059900141009
060000141009         DOW  1 = 1;
060100141009
060200141009           if  iTA48cto = 'T';
060300141009             reade  %kds( keyTITAD04 : 3 )  TITAD000;
060400141009           else;
060500141009             reade  %kds( keyTITAD04 : 3 )  TIOFD000;
060600141009           endif;
060700141009
060800141009           // -?Se a fine lettura non risulta caricato nessuno scaglione?
060900141009           //  ?(xK = 0) ed è stato letti solo il dettaglio con CAP?
061000141009           //  ?valorizzato ($cap1scaglione = *on) => ci si riposiziona?
061100141009           //  ?con l'ultima chiave per caricare almeno gli scaglioni?
061200141009           If  %eof;
061300141009
061400141009             if  xK = 0  and  $cap1scaglione;
061500141009
061600141021               clear  keyTITAD04;
061700141009               k_TADksc = TADksc;
061800141009               k_TADctr = TADctr;
061900141009               k_TADprg = TADprg;
062000141009               k_TADlnp = TADlnp;
062100141009               k_TADorp = TADorp;
062200141009               k_TADnaz = TADnaz;
062300141009               k_TADcap = TADcap;
062400141009
062500141009               if  iTA48cto = 'T';
062600141009                 setll  %kds( keyTITAD04 )  TITAD000;
062700141009               else;
062800141009                 setll  %kds( keyTITAD04 )  TIOFD000;
062900141009               endif;
063000141009
063100141009               // -?Si valorizza il flag del NON controllo del CAP perché?
063200141009               //  ?altrimenti non si riescono a caricare gli scaglioni?
063300141009               $CtrCAP  = *off;
063400141009
063500141009               iter;
063600141009
063700141009             else;
063800141009
063900141009               // -?Altrimenti si esce dal ciclo di lettura?
064000141009               leave;
064100141009
064200141009             endif;
064300141009
064400141009           EndIf;
064500141009
064600141009
064700141009           // -?Si scartano le tariffe annullate?
064800141009           if  TADatb = 'A';
064900141009             iter;
065000141009           endif;
065100141009
065200141009           // -?Salvataggio dei campi per le rotture Regione/Provincia?
065300141009           if  wSaveLNP = *zero;
065400141009
065500141009             // -?Se controllo del CAP si deve fare ($CtrCAP = *on)?
065600141009             //  ?Se cap valorizzato nel 1° scaglione lo si segnala?
065700141009             //  ?per caricare schiera inoltro quando non c'è CAP e?
065800141009             //  ?si ritorna a leggere?
065900141009             if  TADcap  <> *blank  and
066000141009                 $CtrCAP  = *on;
066100141010               $cap1scaglione = *on;
066200141009               iter;
066300141009             endif;
066400141009
066500141009             wSaveLNP = TADlnp;
066600141009             wSaveCTS = TADcts;
066700141009             wSaveCAP = TADcap;
066800141009
066900141021             exsr  sr_CtrlRegione;
067000141021             exsr  sr_CtrlProvincia;
067100141009
067200141009           EndIf;
067300141009
067400141009           // -?Caricamento Scaglioni per LNP CTS e CAP uguali?
067500141009           If  TADlnp = wSaveLNP  and
067600141009               TADcts = wSaveCTS  and
067700141009               TADcap = wSaveCAP;
067800141009
067900141009             xK += 1;
068000141009
068100141022             // -?Troppi scaglioni:?ERRORE?
068200141009             If  xK > c_MaxSgl;
068300141022
068400141009               TNTA48ds.oTA48err = *on;
068500141009               TNTA48ds.oTA48msg = sk_Msg(05);
068600141009               exsr  sr_RoutEnd;
068700141022
068800141009             EndIf;
068900141009
069000141009             sk_sgl(xK) = TADsgl;
069100141009
069200141009             // -?Verifica se gli scaglioni hanno dei decimali?
069300141010             //  ?e quanti ne hanno (SE NON è Tariffa a Valore)?
069400141010             If  %subst( %editc( iTA48ctr : 'X' ) : 1 : 1 ) <> '4'  and
069500141010                 %int( TADsgl ) <> 99999;
069600141010
069700141010               select;
069800141010                 when  TADsgl - %int( TADsgl ) = *zero;
069900141010                   wNumDec = *zero;
070000141010                 when  ( TADsgl * 10 ) - %int( TADsgl * 10 ) = *zero;
070100141010                   wNumDec = 1;
070200141010                 when  ( TADsgl * 100 ) - %int( TADsgl * 100 ) = *zero;
070300141010                   wNumDec = 2;
070400141010                 other;
070500141010                   wNumDec = 3;
070600141010               endsl;
070700141009
070800141009             EndIf;
070900141009
071000141009             // -?CAP non valorizzato: si carica la schiera degli inoltri?
071100141009             if  TADcap = *blank;
071200141020               sk_Pro(xK) = TADino;
071300141010               clear  $cap1scaglione;
071400141009             endif;
071500141009
071600141009           Else;
071700141009
071800141009             // -?Rottura chiave?
071900141009             leave;
072000141009
072100141009           EndIf;
072200141009
072300141009         ENDDO;
072400141009
072500141009
072600141022         // -?Nessuno scaglione:?ERRORE?mancanza di dettaglio?
072700141009         If  xK = *zero;
072800141022
072900141009           TNTA48ds.oTA48err = *on;
073000141009           TNTA48ds.oTA48msg = sk_Msg(04);
073100141009           exsr  sr_RoutEnd;
073200141022
073300141009         EndIf;
073400141009
073500141010         // -?Memorizzazione del n° scaglioni trovati?
073600141009         wNrScaglioni = xK;
073700141009
073800141009         // -?Controllo se gli scaglioni sono stampabili?
073900141009         //  ?(max 4 interi)?
074000141009         For  xY = 1  By 1  To wNrScaglioni;
074100141009
074200141009           // -?Verifica quanti decimali ha lo scaglione?
074300141009           //  ?(se NON è Tariffa a Valore - senza decimali)?
074400141010           Select;
074500141010             //  ?· se scalgione 99999 non si controllano i decimali?
074600141010             When  %int( sk_Sgl(xY) ) = 99999;
074700141010             //  ?· scaglioni senza decimali: max 9999?
074800141010             When  wNumDec = *zero  and  %int( sk_Sgl(xY) ) > 9999;
074900141010               TNTA48ds.oTA48err = *on;
075000141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
075100141010                                   %editc( sk_Sgl(xY) : '3' ) +
075200141010                                   sk_Msg(11) : 1 );
075300141010               exsr  sr_RoutEnd;
075400141010             //  ?· scaglioni con 1 decimale: max 999,9?
075500141010             When  wNumDec = 1      and  %int( sk_Sgl(xY) ) > 999,9;
075600141010               TNTA48ds.oTA48err = *on;
075700141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
075800141010                                   %editc( sk_Sgl(xY) : '4' ) +
075900141010                                   sk_Msg(11) : 1 );
076000141010               exsr  sr_RoutEnd;
076100141010             //  ?· scaglioni con 2 decimali: max 99,99?
076200141010             When  wNumDec >= 2     and  %int( sk_Sgl(xY) ) > 99,99;
076300141010               TNTA48ds.oTA48err = *on;
076400141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
076500141010                                   %editc( sk_Sgl(xY) : '4' ) +
076600141010                                   sk_Msg(11) : 1 );
076700141010               exsr  sr_RoutEnd;
076800141010           EndSl;
076900141010
077000141010         EndFor;
077100141008
077200141008       ENDSR;
077300141010
077400141010       ////--------------------------------------------------------------
077500141022       ////?Controllo Inoltro. (ex subr. sr_STINO)                       ?
077600141010       ////--------------------------------------------------------------
077700141010       //BEGSR  sr_Inoltro;
077800141010       //
077900141010       //  $Inoltro = *off;
078000141010       //
078100141010       //  // -?Lettura del DETTAGLIO e verifica - per ogni scaglione?
078200141010       //  //  ?valido - se l'inoltro è uguale?
078300141010       //  clear  keyTITAD04;
078400141010       //  k_TADksc = iTA48ksc;
078500141010       //  k_TADctr = iTA48ctr;
078600141010       //  k_TADprg = iTA48prg;
078700141010       //
078800141010       //  if  iTA48cto = 'T';
078900141010       //    setll  %kds( keyTITAD04 : 3 )  TITAD000;
079000141010       //    reade  %kds( keyTITAD04 : 3 )  TITAD000;
079100141010       //  else;
079200141010       //    setll  %kds( keyTITAD04 : 3 )  TIOFD000;
079300141010       //    reade  %kds( keyTITAD04 : 3 )  TIOFD000;
079400141010       //  endif;
079500141010       //
079600141010       //  DoW  Not %eof;
079700141010       //
079800141010       //    If  TADatb <> 'A';
079900141010       //
080000141010       //      if  NOT $StaLNP;
080100141010       //
080200141010       //        chain  (TADlnp)  AZORG;
080300141010       //        $StaLNP = ( %found(AZORG01L) );
080400141020       //
080500141020       //        wSaveLNP = TADlnp;
080600141020       //        wSaveCTS = TADcts;
080700141020       //        wSaveCAP = TADcap;
080800141020       //        wSavePRO = %subst( TADorp : 5 : 1 );
080900141021       //        wSaveREG = %subst( TADorp : 3 : 2 );
081000141020       //        work_LNP = wSaveLNP;
081100141020       //        work_CTS = wSaveCTS;
081200141020       //        work_CAP = wSaveCAP;
081300141020       //        work_PRO = wSavePRO;
081400141021       //        work_REG = wSaveREG;
081500141010       //
081600141021       //        // -?Controlli a rottura per Regione e Provincia?
081700141010       //        exsr  sr_CtrlRegione;
081800141021       //        exsr  sr_CtrlProvincia;
081900141010       //
082000141010       //      endif;
082100141010       //
082200141010       //      // -?Se CAP valorizzato: lettura del record successivo?
082300141010       //      if  TADcap = *blank;
082400141010       //
082500141010       //        // -?Verifica se scaglione presente?
082600141010       //        w0072 = TADsgl;
082700141010       //        xZ = %lookup( w0072 : sk_Sgl );
082800141010       //        // -?Controllo inoltro nella schiera?
082900141010       //        if  xZ > *zero  and  TADino <> sk_Pro(xZ);
083000141010       //          $Inoltro = *on;
083100141010       //          leave;
083200141010       //        endif;
083300141010       //
083400141010       //      endif;
083500141010       //
083600141010       //    EndIf;
083700141010       //
083800141010       //    if  iTA48cto = 'T';
083900141010       //      reade  %kds( keyTITAD04 : 3 )  TITAD000;
084000141010       //    else;
084100141010       //      reade  %kds( keyTITAD04 : 3 )  TIOFD000;
084200141010       //    endif;
084300141010       //
084400141010       //  EndDo;
084500141010       //
084600141010       //ENDSR;
084700141010
084800141010       //--------------------------------------------------------------
084900141022       //?Controllo del Dettaglio Tariffario. (ex subr. sr_ELADET)     ?
085000141010       //--------------------------------------------------------------
085100141020       BEGSR  sr_Dettaglio;
085200141020
085300141020         xK4 = 1;
085400141020         DoW  xK4 <= %elem(sk_Sgl);
085500141020
085600141021           // -?Caricamento Scaglioni (6 alla volta) per elaborare?
085700141021           //  ?il Dettaglio?
085800141020           sk_Ssl = %subarr( sk_Sgl : xK4 : %elem(sk_Ssl) );
085900141020
086000141020           // -?Elaborazione Dettaglio (ex subr. sr_FULCRO)?
086100141020           exsr  sr_Dettaglio1;
086200141020
086300141020           xK4 += %elem(sk_Ssl);
086400141022           If  xK4 <= c_MaxSgl  and  sk_Sgl(xK4) <> *zero;
086500141022
086600141020             clear  sk_Ssl;
086700141020             wSaveLNP = work_LNP;
086800141020             wSaveCTS = work_CTS;
086900141020             wSaveCAP = work_CAP;
087000141020             wSavePRO = work_PRO;
087100141020             wSaveREG = work_REG;
087200141022             // -?Pulizia e Controllo Dettaglio per Regione?
087300141020             exsr  sr_CtrlRegione;
087400141022             // -?Pulizia e Controllo Dettaglio per Provincia?
087500141021             exsr  sr_CtrlProvincia;
087600141022
087700141022           Else;
087800141022
087900141022             xK4 = c_MaxSgl + 1;
088000141020             leave;
088100141022
088200141022           EndIf;
088300141020
088400141020         EndDo;
088500141020
088600141020       ENDSR;
088700141020
088800141020       //--------------------------------------------------------------
088900141022       //?                                    (ex subr. sr_FULCRO)     ?
089000141022       //--------------------------------------------------------------
089100141020       BEGSR  sr_Dettaglio1;
089200141010
089300141010         clear  wSaveLNP;
089400141010         clear  wSaveCTS;
089500141010         clear  wSaveCAP;
089600141010
089700141017         // -?Lettura del Dettaglio Tariffario dall'inizio?
089800141010         clear  keyTITAD04;
089900141010         k_TADksc = iTA48ksc;
090000141010         k_TADctr = iTA48ctr;
090100141010         k_TADprg = iTA48prg;
090200141010
090300141010         if  iTA48cto = 'T';
090400141010           setll  %kds( keyTITAD04 : 3 )  TITAD000;
090500141010           reade  %kds( keyTITAD04 : 3 )  TITAD000;
090600141010         else;
090700141010           setll  %kds( keyTITAD04 : 3 )  TIOFD000;
090800141010           reade  %kds( keyTITAD04 : 3 )  TIOFD000;
090900141010         endif;
091000141010
091100141010         DoW  Not %eof;
091200141010
091300141010           if  TADatb <> 'A';
091400141010             exsr  sr_Dettaglio2;
091500141010           endif;
091600141010
091700141010           if  iTA48cto = 'T';
091800141010             reade  %kds( keyTITAD04 : 3 )  TITAD000;
091900141010           else;
092000141010             reade  %kds( keyTITAD04 : 3 )  TIOFD000;
092100141010           endif;
092200141010
092300141010         EndDo;
092400141010
092500141010
092600141021         // -?Ultimi Totali (per Provincia/CAP)?
092700141010         if  wSaveCAP = *blank;
092800141010           // -?Se CAP uguale a *blank:?
092900141010           //  ?si controllano i totali per Provincia?
093000141010           exsr  sr_ProTot;
093100141010         else;
093200141010           // -?Se CAP valorizzato:?
093300141010           //  ?si controllano i totali per Paese di Arrivo (CAP)?
093400141010           exsr  sr_CapTot;
093500141010         endif;
093600141021
093700141010         // -?Calcoli per Regione?
093800141010         exsr  sr_CalReg;
093900141010
094000141010       ENDSR;
094100141010
094200141010       //--------------------------------------------------------------
094300141010
094400141010       BEGSR  sr_Dettaglio2;
094500141010
094600141010         // -?Verifica se è uno scaglione previsto?
094700141010         w0072 = TADsgl;
094800141017         If  %lookup( w0072 : sk_Ssl ) = *zero;
094900141010
095000141010           // -?Se non trovato nei 6 scaglioni da stampare:?
095100141010           //  ?verifica se scaglione corretto?
095200141010           If  %lookup( w0072 : sk_Sgl ) = *zero;
095300141010
095400141022             // -?Errore:?scaglione non previsto per la tariffa?
095500141010             TNTA48ds.oTA48err = *on;
095600141017             TNTA48ds.oTA48msg = sk_Msg(08);
095700141010             exsr  sr_RoutEnd;
095800141010
095900141010           EndIf;
096000141010
096100141010           leavesr;
096200141010
096300141010         EndIf;
096400141017
096500141017         // -?Preparazione campi per eventuali "rotture"?
096600141017         wComREG = %subst( TADorp : 3 : 2 );
096700141017         wComPRO = %subst( TADorp : 5 : 1 );
096800141010
096900141010         Select;
097000141010
097100141017           // -?Rottura per   L I N E A   D I   P A R T E N Z A   ----*?
097200141010           When  TADlnp <> wSaveLNP;
097300141010             wSaveLNP = TADlnp;
097400141017             wSaveREG = wComREG;
097500141017             wSavePRO = wComPRO;
097600141022             // -?Controllo Totali?
097700141017             exsr  sr_Dettaglio3;
097800141010             // -?Calcoli per Regione?
097900141010             exsr  sr_CalReg;
098000141022             // -?Pulizia e Controllo Dettaglio per Regione?
098100141010             exsr  sr_CtrlRegione;
098200141022             // -?Pulizia e Controllo Dettaglio per Provincia?
098300141021             exsr  sr_CtrlProvincia;
098400141010
098500141017           // -?Rottura per   R E G I O N E   ------------------------*?
098600141010           When  wComREG <> wSaveREG;
098700141010             wSaveREG = wComREG;
098800141010             wSavePRO = wComPRO;
098900141022             // -?Controllo Totali?
099000141017             exsr  sr_Dettaglio3;
099100141010             // -?Calcoli per Regione?
099200141010             exsr  sr_CalReg;
099300141022             // -?Pulizia e Controllo Dettaglio per Regione?
099400141010             exsr  sr_CtrlRegione;
099500141022             // -?Pulizia e Controllo Dettaglio per Provincia?
099600141021             exsr  sr_CtrlProvincia;
099700141010
099800141017           // -?Rottura per   P R O V I N C I A   --------------------*?
099900141010           When  wComPRO <> wSavePRO;
100000141010             wSavePRO = wComPRO;
100100141022             // -?Controllo Totali?
100200141017             exsr  sr_Dettaglio3;
100300141010             // -?Controlli per Provincia?
100400141022             exsr  sr_CtrlProvincia;
100500141010
100600141017           // -?Rottura per   P A E S E   D I   A R R I V O   (CAP)   *?
100700141010           When  TADcap <> wSaveCAP;
100800141017             exsr  sr_Dettaglio3;
100900141010
101000141010         EndSl;
101100141022
101200141022         // -?Elaborazione Scaglione?
101300141022         //w0072 = TADsgl;    ?(già fatto prima)?
101400141022         xK1 = %lookup( w0072 : sk_Ssl );
101500141022         if  xK1 = *zero;
101600141022           xK1 = 1;
101700141022         endif;
101800141022         // -?Valorizzazione schiera importi?
101900141022         sk_Tct(xK1) = TADitr;
102000141022
102100141022         //if  $Inoltro;
102200141022         //  .....
102300141022         //endif;
102400141010
102500141010       ENDSR;
102600141017
102700141017       //--------------------------------------------------------------
102800141017
102900141017       BEGSR  sr_Dettaglio3;
103000141017
103100141021         // -?Verifica se CAP valorizzato?
103200141017         if  wSaveCAP = *blank;
103300141017           // -?Se CAP uguale a *blank:?
103400141017           //  ?si controllano i totali per Provincia?
103500141017           exsr  sr_ProTot;
103600141017         else;
103700141017           // -?Se CAP valorizzato:?
103800141017           //  ?si controllano i totali per Paese di Arrivo (CAP)?
103900141017           exsr  sr_CapTot;
104000141017         endif;
104100141017
104200141017         wSaveCAP = TADcap;
104300141017
104400141017       ENDSR;
104500141010
104600141010       //--------------------------------------------------------------
104700141010       //?Totali per   P A E S E   D I   A R R I V O   (CAP).          ?
104800141010       //--------------------------------------------------------------
104900141010       BEGSR  sr_CapTOT;
105000141010
105100141013         If  wNumPDE >= 30;
105200141021
105300141022           // -?Errore:?troppi Codici Tassazione?
105400141013           TNTA48ds.oTA48err = *on;
105500141013           TNTA48ds.oTA48msg = sk_Msg(12);
105600141013           exsr  sr_RoutEnd;
105700141021
105800141013         EndIf;
105900141013
106000141013         wNumPDE += 1;
106100141013
106200141013         // -?Controllo se ok tutti gli scaglioni?
106300141013         exsr  sr_CtrSca;
106400141010
106500141010       ENDSR;
106600141010
106700141010       //--------------------------------------------------------------
106800141010       //?Totali per   P R O V I N C I A .                             ?
106900141010       //--------------------------------------------------------------
107000141010       BEGSR  sr_ProTOT;
107100141010
107200141022         // -?Salvataggio della schiera sk_TCT nel primo sk_BIG libero?
107300141022         xK = %lookup( *blank : sk_BIG );
107400141022         If  xK = *zero;
107500141022
107600141022           // -?Errore:?troppe Province per Regione?
107700141013           TNTA48ds.oTA48err = *on;
107800141013           TNTA48ds.oTA48msg = sk_Msg(09);
107900141013           exsr  sr_RoutEnd;
108000141022
108100141013         EndIf;
108200141010
108300141013         // -?Controllo se ok tutti gli scaglioni?
108400141013         exsr  sr_CtrSca;
108500141017
108600141022         // -?Caricamento schiera delle province 6 elementi alla volta?
108700141022         //  ?(città) - 6 elementi x 7 bytes = 42 bytes?
108800141022         //For  xB = 1  To %elem(sk_TCT);
108900141022         //  sk_Big(xK) = %trim( sk_BIG(xK) ) +
109000141022         //               %editc( sk_TCT(xB) : 'X' );
109100141017         //EndFor;
109200141017         sk_Big(xK) = ds_sk_TCT;
109300141017
109400141022         sk_Brg(xK) = wCodORP;
109500141010
109600141010       ENDSR;
109700141010
109800141010       //--------------------------------------------------------------
109900141010       //?Calcoli per   R E G I O N E .                                ?
110000141010       //--------------------------------------------------------------
110100141010       BEGSR  sr_CalREG;
110200141010
110300141013         // -?N O T E del sorgente originale (TNTA49R):?
110400141013         //  ?Leggo BIG (schiera per ogni provincia della regione)?
110500141013         //  ?Per ogni provincia letta (quindi per ogni elemento) dopo?
110600141013         //  ?averlo caricato controllo se ci sono altre province uguali?
110700141013         //  ?se sì accodo nella skiera dei COD/TASSAZ. che hanno la?
110800141013         //  ?stessa tariffa?
110900141022         //  ?leggo i soli elementi con BRG>0 (a mano mano che li raggruppo?
111000141013         //  ?li abblenco). Mi fermo quando leggo la regione che è sempre?
111100141013         //  ?l'ultimo e va da solo : K=1 la descrizione è  CAPOLUOGHI?
111200141013         //  ?                        K>1 "    "    "    è  ALTRI?
111300141013
111400141022         // -?Ricerca ultimo elemento occupato in "Paesi di Arrivo"?
111500141022         //xP = %lookup( *blank : sk_PDE ) - 1;
111600141013         xP = wNumPDE;
111700141010
111800141013         For  xK = 1  To %elem( sk_Brg );     //?(max 15)?
111900141013
112000141013           if  sk_Brg(xK) <> *blank;
112100141013             // -?9 = Regione?
112200141022             if  sk_Brg(xK) = '9';
112300141022
112400141022               xP += 1;
112500141022               If  xP > 30;
112600141022
112700141022                 // -?Errore:?troppi Cod. Tassazione per la Regione?
112800141022                 TNTA48ds.oTA48err = *on;
112900141022                 TNTA48ds.oTA48msg = sk_Msg(12);
113000141022                 exsr  sr_RoutEnd;
113100141022
113200141022               EndIf;
113300141022               clear  sk_Big(xK);
113400141022               clear  sk_Brg(xK);
113500141022
113600141022             endif;
113700141013           endif;
113800141013
113900141013         EndFor;
114000141010
114100141010       ENDSR;
114200141010
114300141010       //--------------------------------------------------------------
114400141010       //?Controllo se OK gli scaglioni elaborati.                     ?
114500141010       //--------------------------------------------------------------
114600141010       BEGSR  sr_CtrSCA;
114700141010
114800141014         Select;
114900141010
115000141014           When  wNrScaglioni > 12;
115100141014             // -?Si sta elaborando il 1° o il 2° giro di scaglioni:?
115200141014             //  ?vuol dire che ci sono di sicuro 6 importi?
115300141022             if  xK4 <= 12;
115400141014               yy = 6;
115500141014             // -?Si sta elaborando il 3° giro di scaglioni:?
115600141014             //  ?si contano quanti importi si devo elaborare?
115700141014             else;
115800141014               yy = wNrScaglioni - 12;
115900141014             endif;
116000141010
116100141014           When  wNrScaglioni > 6  and  wNrScaglioni < 12;
116200141014             // -?Si sta elaborando il 1° giro di scaglioni:?
116300141014             //  ?vuol dire che ci sono di sicuro 6 importi?
116400141014             if  xK4 = 1;
116500141014               yy = 6;
116600141014             // -?Si sta elaborando il 2° giro di scaglioni:?
116700141014             //  ?si contano quanti importi si devo elaborare?
116800141014             else;
116900141014               yy = wNrScaglioni - 6;
117000141014             endif;
117100141010
117200141014           When  wNrScaglioni = 6;
117300141014             yy = 6;
117400141010
117500141014           When  wNrScaglioni < 6;
117600141014             yy = wNrScaglioni;
117700141014
117800141014         EndSl;
117900141010
118000141014         // -?Controllo se ci sono degli importi a zero?
118100141014         For  xx = 1  To  yy;
118200141014
118300141014           If  sk_Tct(xx) = *zero;
118400141022
118500141022             // -?Errore:?Scaglione imprevisto per Codice Tassazione?
118600141014             TNTA48ds.oTA48err = *on;
118700141014             TNTA48ds.oTA48msg = sk_Msg(08);
118800141014             exsr  sr_RoutEnd;
118900141022
119000141014           EndIf;
119100141014
119200141014         EndFor;
119300141010
119400141010       ENDSR;
119500141009
119600141009       //--------------------------------------------------------------
119700141022       //?Controllo Regione. (ex subr. REGDET)                         ?
119800141009       //--------------------------------------------------------------
119900141009       BEGSR  sr_CtrlRegione;
120000141021
120100141021         // -?Controllo Codice Tassazione?
120200141021         exsr  sr_CtrlCodTassazione;
120300141009
120400141009         // -?Se tariffa DPD e linea partenza cliente non DPD:?
120500141009         //  ?regione IT diventa regione EE?
120600141022         //dsTA01 = TAMflo;   ?(già fatto prima)?
120700141009         if  wCodREG       = 'IT'  and
120800141009             dsTA01.§TAdpd = 'S'   and
120900141009             Og143.§OGntw <> 'DPD';
121000141009           wCodREG = 'EE';
121100141009         endif;
121200141009
121300141021         clear  xK5;
121400141013         wK5 = %lookup( wCodREG : sk_CReg );
121500141013
121600141013         If  wK5 = *zero;
121700141009
121800141009           TNTA48ds.oTA48err = *on;
121900141009           TNTA48ds.oTA48msg = sk_Msg(07);
122000141009           exsr  sr_RoutEnd;
122100141009
122200141009         EndIf;
122300141013
122400141013         clear  sk_Big;
122500141013         clear  sk_Brg;
122600141013
122700141013         clear  wNumPDE;
122800141009
122900141009       ENDSR;
123000141021
123100141021       //--------------------------------------------------------------
123200141022       //?Controllo Provincia. (ex subr. PRODET)                       ?
123300141021       //--------------------------------------------------------------
123400141021       BEGSR  sr_CtrlProvincia;
123500141021
123600141021         // -?Controllo Codice Tassazione?
123700141021         exsr  sr_CtrlCodTassazione;
123800141021
123900141021         if  NOT $TariffePartic;
124000141021           wCodORP = %subst( TADorp : %len(TADorp) - %len(wCodORP) + 1 );
124100141021         else;
124200141021           wCodORP = %subst( TPDorp : %len(TPDorp) - %len(wCodORP) + 1 );
124300141021         endif;
124400141021
124500141021         wCodREG = sk_CTrap(xK5);
124600141021
124700141021       ENDSR;
124800141021
124900141021       //--------------------------------------------------------------
125000141022       //?Controllo Codice Tassazione. (ex subr. REGDET e PRODET)      ?
125100141021       //--------------------------------------------------------------
125200141021       BEGSR  sr_CtrlCodTassazione;
125300141021
125400141021         clear  xK5;
125500141021         if  NOT $TariffePartic;
125600141021           // -?TITAD?
125700141021           xK5 = %lookup( TADcts : sk_CT );
125800141021         else;
125900141021           // -?TITPD?
126000141021           xK5 = %lookup( TPDcts : sk_CT );
126100141021         endif;
126200141021
126300141021         If  xK5 = *zero;
126400141021
126500141021           TNTA48ds.oTA48err = *on;
126600141021           TNTA48ds.oTA48msg = sk_Msg(06);
126700141021           exsr  sr_RoutEnd;
126800141021
126900141021         EndIf;
127000141021
127100141021       ENDSR;
127200141022
127300141022       //--------------------------------------------------------------
127400141022       //?Controllo delle Tariffe Particolari. (ex subr. sr_STATPT)    ?
127500141022       //--------------------------------------------------------------
127600141022       BEGSR  sr_TariffeParticolari;
127700141022
127800141022         // -?Lettura della schiera delle Tariffe Particolari?
127900141022         //  ?(NON ordinata...)?
128000141022         For  xY = 1  To  %elem(sk_1Pftc);
128100141022
128200141022           Select;
128300150108             // -?Tariffa Particolare da stampare in modo standard?
128400141022             When  sk_1Ptst(xY) = *blank;
128500141022               exsr  sr_TariffaParticolareSTD;
128600150108             // -?Tariffa Particolare da NON stampare in modo standard?
128700141022             When  sk_1Ptst(xY) = 'N';
128800141022               select;
128900141022                 // -?Elevazione Limite Risarcibile?
129000141022                 when  sk_1Pftc(xY) = 'R ';
129100141022                   exsr  sr_ElevazioneLimiteRisarcibile;
129200141022                 // -?Mandato Assicuratico AC Plus?
129300141022                 when  sk_1Pftc(xY) = 'C ';
129400150108                   exsr  sr_MandatoAssicACplus; .......................
129500141022                 // -?Mandato Assicuratico AC Base?
129600141022                 when  sk_1Pftc(xY) = 'd ';
129700150108                   exsr  sr_MandatoAssicACbase; .......................
129800141022                 // -?Fuel Surchange?
129900141022                 when  sk_1Pftc(xY) = 'f ';
130000150108                   exsr  sr_FuelSurchange; ............................
130100141022               endsl;
130200141022           EndSl;
130300141022
130400141022         EndFor;
130500141022
130600141022       ENDSR;
130700141022
130800141022       //--------------------------------------------------------------
130900141022       //?Controllo dati della Tariffa Particolare da stampare in modo ?
131000141022       //?  standard. (ex subr. SR_TAPARSTD)                           ?
131100141022       //--------------------------------------------------------------
131200141022       BEGSR  sr_TariffaParticolareSTD;
131300141022
131400141022         clear  keyTITPT01;
131500141022         k_TPTksc = iTA48ksc;
131600141022         k_TPTctr = iTA48ctr;
131700141022         k_TPTprg = iTA48prg;
131800141022         k_TPTftc = sk_1Pftc(xY);   // -?" "?
131900141022
132000141022         if  iTA48cto = 'T';
132100141022           chain  %kds( keyTITPT01 )  TITPT000;
132200141022         else;
132300141022           chain  %kds( keyTITPT01 )  TIOPT000;
132400141022         endif;
132500141022
132600141022         if  Not %found  or  TPTatb = 'A';
132700141022           // -?Se costo da stampare?
132800141022           if  sk_Edac(xY) = 'S';
132900141022             if  sk_Esud(xY) = 'S';
133000141022               leavesr;
133100141022             endif;
133200141022           else;
133300141022             leavesr;
133400141022           endif;
133500141022         endif;
133600141022
133700141022         // -?Ricerca record validi con Tariffa o Minimo > *zero?
133800141022         exsr  sr_RicercaTariffaParticolare;
133900141022
134000141022         Select;
134100141022           // -?Nessun addebito?
134200141022           When  Not $RecVal;
134300141022             leavesr;
134400141022           // -?Reperito un unico dettaglio tariffario?
134500141022           When  wRecVal = 1;
134600141022             exsr  sr_GesUnico; .......................................
134700141022           // -?Reperita tariffa valorizzata?
134800141022           Other;
134900141022             exsr  sr_GesTPD; .........................................
135000141022         EndSl;
135100141022
135200141022       ENDSR;
135300141022
135400141022       //--------------------------------------------------------------
135500141022       //?Controllo dati della Tariffa Particolare                     ?
135600141022       //?  Elevazione Limite Risarcibile. (ex subr. SR_ELLIRIS)       ?
135700141022       //--------------------------------------------------------------
135800141022       BEGSR  sr_ElevazioneLimiteRisarcibile;
135900141022
136000141022         clear  keyTITPT01;
136100141022         k_TPTksc = iTA48ksc;
136200141022         k_TPTctr = iTA48ctr;
136300141022         k_TPTprg = iTA48prg;
136400141022         k_TPTftc = sk_1Pftc(xY);   // -?"R"?
136500141022
136600141022         if  iTA48cto = 'T';
136700141022           chain  %kds( keyTITPT01 )  TITPT000;
136800141022         else;
136900141022           chain  %kds( keyTITPT01 )  TIOPT000;
137000141022         endif;
137100141022
137200141022         if  Not %found  or  TPTatb = 'A';
137300141022           leavesr;
137400141022         endif;
137500141022
137600141022         // -?Ricerca record validi con Tariffa o Minimo > *zero?
137700141022         exsr  sr_RicercaTariffaParticolare;
137800150108
137900150108         ..............................................................
138000141022
138100141022       ENDSR;
138200150108
138300150108       //--------------------------------------------------------------
138400150108       //?Controllo dati della Tariffa Particolare                     ?
138500150108       //?  Mandato Assicurativo AC Plus.  (ex subr. SR_MANDATO)       ?
138600150108       //--------------------------------------------------------------
138700150108       BEGSR  sr_MandatoAssicACplus;
138800150108
138900150108         clear  keyTITPT01;
139000150108         k_TPTksc = iTA48ksc;
139100150108         k_TPTctr = iTA48ctr;
139200150108         k_TPTprg = iTA48prg;
139300150108         k_TPTftc = sk_1Pftc(xY);   // -?"C"?
139400150108
139500150108         if  iTA48cto = 'T';
139600150108           chain  %kds( keyTITPT01 )  TITPT000;
139700150108         else;
139800150108           chain  %kds( keyTITPT01 )  TIOPT000;
139900150108         endif;
140000150108
140100150108         if  Not %found  or  TPTatb = 'A';
140200150108           leavesr;
140300150108         endif;
140400150108
140500150108         // -?Se NON è Mandato a Valore Variabile e NON è fittizio:?
140600150108         if  TPTvlm <> *zero  and  TPTtma <> 'F';
140700150108
140800150108           // -?Decodifica del flag Valore Merce?
140900150108           clear  dsTR;
141000150108           clear  keyTABEL00;
141100150108           k_TBLkut = 1;
141200150108           k_TBLcod = 'TR';
141300150108           k_TBLkey = TPTvfm;
141400150108
141500150108           chain  %kds( keyTABEL00 )  TABEL;
141600150108
141700150108           if  %found(TABEL00F);
141800150108             dsTR = TBLuni;
141900150108           endif;
142000150108
142100150108         endif;
142200150108
142300150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
142400150108         exsr  sr_RicercaTariffaParticolare;
142500150108
142600150108         ..............................................................
142700150108
142800150108       ENDSR;
142900150108
143000150108       //--------------------------------------------------------------
143100150108       //?Controllo dati della Tariffa Particolare                     ?
143200150108       //?  Mandato Assicurativo AC Base.  (ex subr. SR_ACBASE)        ?
143300150108       //--------------------------------------------------------------
143400150108       BEGSR  sr_MandatoAssicACbase;
143500150108
143600150108         clear  keyTITPT01;
143700150108         k_TPTksc = iTA48ksc;
143800150108         k_TPTctr = iTA48ctr;
143900150108         k_TPTprg = iTA48prg;
144000150108         k_TPTftc = sk_1Pftc(xY);   // -?"d"?
144100150108
144200150108         if  iTA48cto = 'T';
144300150108           chain  %kds( keyTITPT01 )  TITPT000;
144400150108         else;
144500150108           chain  %kds( keyTITPT01 )  TIOPT000;
144600150108         endif;
144700150108
144800150108         if  Not %found  or  TPTatb = 'A';
144900150108           leavesr;
145000150108         endif;
145100150108
145200150108         // -?Decodifica del flag Valore Merce?
145300150108         clear  dsTR;
145400150108         clear  keyTABEL00;
145500150108         k_TBLkut = 1;
145600150108         k_TBLcod = 'TR';
145700150108         k_TBLkey = TPTvfm;
145800150108
145900150108         chain  %kds( keyTABEL00 )  TABEL;
146000150108
146100150108         if  %found(TABEL00F);
146200150108           dsTR = TBLuni;
146300150108         endif;
146400150108
146500150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
146600150108         exsr  sr_RicercaTariffaParticolare;
146700150108
146800150108         ..............................................................
146900150108
147000150108       ENDSR;
147100150108
147200150108       //--------------------------------------------------------------
147300150108       //?Controllo dati della Tariffa Particolare                     ?
147400150108       //?  Fuel Surchange.                (ex subr. SR_FUEL)          ?
147500150108       //--------------------------------------------------------------
147600150108       BEGSR  sr_FuelSurchange;
147700150108
147800150108         clear  keyTITPT01;
147900150108         k_TPTksc = iTA48ksc;
148000150108         k_TPTctr = iTA48ctr;
148100150108         k_TPTprg = iTA48prg;
148200150108         k_TPTftc = sk_1Pftc(xY);   // -?"f"?
148300150108
148400150108         if  iTA48cto = 'T';
148500150108           chain  %kds( keyTITPT01 )  TITPT000;
148600150108         else;
148700150108           chain  %kds( keyTITPT01 )  TIOPT000;
148800150108         endif;
148900150108
149000150108         if  Not %found  or  TPTatb = 'A';
149100150108           leavesr;
149200150108         endif;
149300150108
149400150108         dTPT01 = TPTflo;
149500150108
149600150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
149700150108         exsr  sr_RicercaTariffaParticolare;
149800150108
149900150108         ..............................................................
150000150108
150100150108         // -?Recupero del Prezzo Medio del Gasolio?
150200150108         wData_Iso1 = *loval;
150300150108         wData_Iso2 = *hival;
150400150108         setll  ( TPTdpb )  TIPMG000;
150500150108         read  TIPMG000;
150600150108         if  Not %eof(TIPMG01L);
150700150108           if  PMGdtd > *zero;
150800150108             wData_Iso1 = %date( PMGdtd : *iso );
150900150108           endif;
151000150108           wData_Iso2 = %date( PMGdpb : *iso );
151100150108         endif;
151200150108
151300150108         If  %eof(TIPMG01L)  or  PMGpmg = *zero  or
151400150108             %diff( wData_Iso2 : wData_Iso1 : *d ) > 6;
151500150108           oTA48err = *on;
151600150108           oTA48msg = sk_Msg(01);
151700150108           leavesr;
151800150108         EndIf;
151900150108
152000150108         ..............................................................
152100150108
152200150108       ENDSR;
152300141022
152400141022       //--------------------------------------------------------------
152500141022       //?Controllo esistenza di un rec. valido e con                  ?
152600141022       //?  Tariffa + Minimo > *zero. (ex subr. RICREC)                ?
152700141022       //--------------------------------------------------------------
152800141022       BEGSR  sr_RicercaTariffaParticolare;
152900141022
153000141022         clear  $RecVal;
153100141022         clear  wRecVal;
153200141022         clear  work_FTC;
153300141022         clear  work_ORP;
153400141022         clear  work_SGL;
153500141022
153600141022         clear  keyTITPD01;
153700141022         k_TPDksc = iTA48ksc;
153800141022         k_TPDctr = iTA48ctr;
153900141022         k_TPDprg = iTA48prg;
154000141022         k_TPDftc = sk_1Pftc(xY);
154100141022
154200141022         if  iTA48cto = 'T';
154300141022           setll  %kds( keyTITPD01 )  TITPD000;
154400141022           reade  %kds( keyTITPD01 : 4 )  TITPD000;
154500141022         else;
154600141022           setll  %kds( keyTITPD01 )  TIOPD000;
154700141022           reade  %kds( keyTITPD01 : 4 )  TIOPD000;
154800141022         endif;
154900141022
155000141022         DoW  NOT %eof;
155100141022
155200141022           If  TPDatb <> 'A';
155300141022
155400141022             if  TPDitr = *zero  and  TPDmin = *zero;
155500141022
155600141022               // -?Memorizzazione chiave per il riaggancio del 1°?
155700141022               //  ?rec. valido?
155800141022               if  Not $RecVal;
155900141022                 $RecVal  = *on;
156000141022                 work_FTC = TPDftc;
156100141022                 work_ORP = TPDorp;
156200141022                 work_SGL = TPDsgl;
156300141022               endif;
156400141022
156500141022             else;
156600141022
156700141022               wRecVal += 1;
156800141022
156900141022               // -?Memorizzazione chiave per il riaggancio del 1°?
157000141022               //  ?rec. valido se ancora *blank?
157100141022               if  wRecVal = 1  and  work_FTC = *blank;
157200141022                 work_FTC = TPDftc;
157300141022                 work_ORP = TPDorp;
157400141022                 work_SGL = TPDsgl;
157500141022               endif;
157600141022
157700141022               // -?SE trovati più di 1 rec.: NON si prosegue nella?
157800141022               //  ?lettura?
157900141022               if  wRecVal > 1;
158000141022                 // -?Riposizionamento sul 1° rec. valido trovato?
158100141022                 k_TPDftc = work_FTC;
158200141022                 k_TPDorp = work_ORP;
158300141022                 k_TPDsgl = work_SGL;
158400141022                 if  iTA48cto = 'T';
158500141022                   chain  %kds( keyTITPD01 )  TITPD000;
158600141022                 else;
158700141022                   chain  %kds( keyTITPD01 )  TIOPD000;
158800141022                 endif;
158900141022                 leave;
159000141022               endif;
159100141022
159200141022             endif;
159300141022
159400141022           EndIf;
159500141022
159600141022           if  iTA48cto = 'T';
159700141022             reade  %kds( keyTITPD01 : 4 )  TITPD000;
159800141022           else;
159900141022             reade  %kds( keyTITPD01 : 4 )  TIOPD000;
160000141022           endif;
160100141022
160200141022         EndDo;
160300141022
160400141022         ..............................................................
160500141022      * se al primo record valido verifico che non ha lo stesso ordinamento
160600141022      * regione e provincia del record a zero forzo per la stampa in dettaglio
160700141022      *
160800141022     c                   if        recval = 1 and tpdorp <> savorp1 and
160900141022     c                             savorp1 <> *blanks
161000141022     c                   eval      recval = 9
161100141022      * mi riposiziono sul prima record valido trovato
161200141022     c  n98ktpdwrk       chain     titpd01l
161300141022     c   98ktpdwrk       chain     tiopd01l
161400141022     c                   clear                   savorp1
161500141022     c                   endif
161600141022      * Se ho trovato un solo record valido verifico lo scaglione trovato
161700141022     c                   if        recval = 1
161800141022     c                   z-add     recsgl        w0050
161900141022     c                   if        w0050 <> 99999 and tpttpg <> '2'
162000141022      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
162100141022      * e tipo tariffa non è a spedizione
162200141022     c                   eval      recval = 9
162300141022      * mi riposiziono sul prima record valido trovato
162400141022     c  n98ktpdwrk       chain     titpd01l
162500141022     c   98ktpdwrk       chain     tiopd01l
162600141022     c                   endif
162700141022     c                   endif
162800141022
162900141022       ENDSR;
163000141008
163100141008       //--------------------------------------------------------------
163200141008       //?Operazioni finali.                                           ?
163300141008       //--------------------------------------------------------------
163400141008       BEGSR  sr_RoutEnd;
163500141008
163600141008         // -?Chiusura applicazioni?
163700141021         ..............................................................
163800141008
163900141008         // -?Uscita dal *pgm?
164000141008         return;
164100141008
164200141008       ENDSR;
164300141008
164400141008      /end-free
164500141008
164600141008       //--------------------------------------------------------------
164700141008       //?Definizione schiere a tempo di compilazione                  ?
164800141008       //--------------------------------------------------------------
164900141008
165000141008** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
165100141008ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero!!    1
165200141009+ Numero visita errato                                                         2
165300141008CLIENTE NON CODIFICATO O ANNULLATO                                             3
165400141008NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                             4
165500141008PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE                                    5
165600141008CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                    6
165700141008CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                  7
165800141008UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO            8
165900141008PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T A D  NON ALLINEATO   9
166000141008PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
166100141010+ Scaglione &Scaglione____ non stampabile                                     11
166200141008TROPPI PAESI DI ARRIVO/CODICI DI TASSAZIONE PER LA REGIONE                    12
166300141008                                                                              13
166400141008UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   14
166500141008PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        15
166600141008UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     16
166700141021+ Mancano parametri obbligatori                                               17
166800141021+ Ricevuti parametri errati                                                   18
