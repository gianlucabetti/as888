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
002600150205       // -?   "       "   : Dettaglio?
002700141008     fTITAD04L  if   e           k disk    usropn
002800150205       // -?   "    Particolari: Testata?
002900150205     fTITPT01L  if   e           k disk    usropn
003000150205       // -?   "         "     : Dettaglio?
003100150205     fTITPD01L  if   e           k disk    usropn
003200150205
003300150205       // -?Offerte Clienti: Testata?
003400150205     fTNOFM01L  if   e           k disk    usropn
003500150205     f                                     rename( TNTAM000 : TNOFM000 )
003600150205       // -?   "       "   : Dettaglio?
003700141008     fTIOFD01L  if   e           k disk    usropn
003800141008     f                                     rename( TITAD000 : TIOFD000 )
003900150205       // -?   "    Particolari: Testata?
004000141020     fTIOPT01L  if   e           k disk    usropn
004100141020     f                                     rename( TITPT000 : TIOPT000 )
004200150205       // -?   "         "     : Dettaglio?
004300141020     fTIOPD01L  if   e           k disk    usropn
004400141020     f                                     rename( TITPD000 : TIOPD000 )
004500150205
004600150205       // -?Visite (se Offerta)?
004700150205     fTIVIS05L  if   e           k disk    usropn
004800141008
004900150205       // -?Anagrafica P.D.C. per Tariffe (se Offerta)?
005000141008     fTFACO00F  if   e           k disk    usropn
005100141008     f                                     rename( CNACO000 : TFACO000 )
005200141008     fTFIND00F  if   e           k disk    usropn
005300141008     f                                     rename( CNIND000 : TFIND000 )
005400141008     fTFCLP00F  if   e           k disk    usropn
005500141008     f                                     rename( CNCLP000 : TFCLP000 )
005600141022
005700141022       // -?Tabelle?
005800141022     fTABEL00F  if   e           k disk
005900141008
006000141008       //--------------------------------------------------------------
006100141008       //?Definizione costanti.                                        ?
006200141008       //--------------------------------------------------------------
006300141008
006400141009       // -?n° Max degli Scaglioni?
006500141009     d c_MaxSgl        c                   const(18)
006600141008
006700141008       //--------------------------------------------------------------
006800141008       //?Definizione schiere.                                         ?
006900141008       //--------------------------------------------------------------
007000141008
007100141008       // -?Messaggi di errore?
007200141008     d sk_Msg          s             78    dim(20)  ctdata  perrcd(1)
007300141009
007400141009       // -?Codici Tassazione?
007500141009     d sk_CT           s                   like(TADcts)     dim(600)
007600141009     d                                     inz
007700141009       // -?Sigle Regioni di appartenenza?
007800141009     d sk_CTrap        s                   like(§CTrap)     dim(600)
007900141009     d                                     inz
008000141009       // -?Codici Regione (da ordinamento in tab. "CT")?
008100141009     d sk_CReg         s                   like(TADcts)     dim(200)
008200141009     d                                     inz
008300141009
008400141009       // -?Scaglioni Tariffe?
008500141010     d*// sk_Sgl          s                   like(TADsgl)     dim(c_MaxSgl)
008600141013     d sk_Sgl          s              7  2 dim(c_MaxSgl)    inz
008700141020     d sk_Ssl          s              7s 2 dim( 6)          inz
008800141020
008900141020       // -?Tariffe Inoltro?
009000141020     d sk_Pro          s              7  3 dim(c_MaxSgl)    inz
009100141013
009200141013       // -?Codici Tassazione?
009300141013     d*// sk_Tct          s                   like(TADitr)     dim( 6)
009400141017     d ds_sk_TCT       ds                  inz
009500141017     d   sk_Tct                       7s 3 dim( 6)          inz
009600141022
009700141022       // -?Province per Regione?
009800141022     d sk_Big          s             42    dim(15)          inz
009900141013
010000141022       // -?Ordinamento Province per Regione?
010100141013     d*// sk_Brg          s                   like(TADorp)     dim(15)
010200141013     d sk_Brg          s              1    dim(15)          inz
010300141022
010400141022       // -?Codici Consegna Particolare?
010500141022     d sk_1Pftc        s                   like(TPDftc)  dim(50)  inz
010600141022     d sk_1Ptst        s                   like(§1Ptst)  dim(50)  inz
010700141008
010800141008       //--------------------------------------------------------------
010900141008       //?Definizione aree dati.                                       ?
011000141008       //--------------------------------------------------------------
011100141008
011200141008       // -?Dati utente?
011300141008     d §AzUte        e ds                  extname(AZUTE00F)
011400141008     d                                     dtaara
011500141008     d §DatiUte      e ds                  extname(dDatiUte)
011600141008     d                                     dtaara
011700141008
011800141008       //--------------------------------------------------------------
011900141008       //?Definizione strutture dati.                                  ?
012000141008       //--------------------------------------------------------------
012100141008
012200141008       // -?Parametri ricevuti?
012300141008     d KPJBA         e ds                  qualified
012400141009     d TNTA48ds      e ds                  qualified     inz
012500141009
012600141009       // -?Tab. "CT" = Codici di Tassazione?
012700141009     d dsCT          e ds                  inz
012800141022
012900141022       // -?Tab. "1P" = Consegne Particolari Clienti?
013000141022     d ds1P          e ds                  inz
013100141008
013200141008       //--------------------------------------------------------------
013300141008       //?Definizione variabili globali.                               ?
013400141008       //--------------------------------------------------------------
013500141008
013600141009       // -?Indici di schiera?
013700141009     d xK              s              3  0 inz
013800141009     d xK1             s              3  0 inz
013900141017     d xK4             s              3  0 inz
014000141013     d xK5             s              3  0 inz
014100141017     d xP              s              3  0 inz
014200141009     d xY              s              3  0 inz
014300141010     d xZ              s              3  0 inz
014400141022     d YY              s              3  0 inz
014500141009
014600141009       // -?Flags Booleani?
014700141010     d $cap1scaglione  s               n   inz ........................
014800141009     d $CtrCAP         s               n   inz ........................
014900141021     d*// $Inoltro        s               n   inz
015000141021     d*// $StaLNP         s               n   inz
015100141013     d $TariffePartic  s               n   inz
015200141022     d $RecVal         s               n   inz
015300141009
015400141009       // -?Campi di comodo?
015500141009     d wSaveLNP        s                   like(TADlnp)  inz
015600141009     d wSaveCTS        s                   like(TADcts)  inz
015700141009     d wSaveCAP        s                   like(TADcap)  inz
015800141010     d wSavePRO        s              1    inz
015900141010     d wSaveREG        s              2    inz
016000141010     d wComPRO         s              1    inz
016100141010     d wComREG         s              2    inz
016200141020     d work_LNP        s                   like(TADlnp)  inz
016300141020     d work_CTS        s                   like(TADcts)  inz
016400141020     d work_CAP        s                   like(TADcap)  inz
016500141020     d work_PRO        s              1    inz
016600141020     d work_REG        s              2    inz
016700141009
016800141010     d wCodREG         s                   like(§CTrap)  inz
016900141010
017000141013     d wCodORP         s              1    inz
017100141009     d wNrScaglioni    s              3  0 inz
017200141010     d wNumDec         s              2  0 inz
017300141010     d w0072           s              7  2 inz
017400141013     d wNumPDE         s              3  0 inz
017500141022     d wRecVal         s              3  0 inz
017600150108
017700150108     d wData_Iso1      s               d   datfmt(*iso)  inz(*loval)
017800150108     d wData_Iso2      s               d   datfmt(*iso)  inz(*hival)
017900141008
018000141008       //--------------------------------------------------------------
018100141008       //?Definizione prototipi procedure.                             ?
018200141008       //--------------------------------------------------------------
018300141008
018400141008       // -?Reperimento dati utente?
018500141008     d TIBS34ds      e ds
018600141008      /copy gaitrasrc/srcProtoPR,TIBS34R
018700141008
018800141008       // -?Controllo/Decodifica cliente?
018900141008      /copy gaitrasrc/srcProtoPI,TIBS69R
019000141008      /copy gaitrasrc/srcProtoPR,TIBS69R
019100141008
019200141008       //--------------------------------------------------------------
019300141008       //?Definizione key-list.                                        ?
019400141008       //--------------------------------------------------------------
019500141008
019600141008       // -?File TNTAM01L / TNOFM01L?
019700150205     d keyTNTAM01    e ds                  extname( TNTAM01L : *key )
019800150205     d                                     prefix( k_ )   inz
019900141008
020000141008       // -?File TITAD04L / TIOFD01L?
020100150205     d keyTITAD01    e ds                  extname( TITAD04L : *key )
020200150205     d                                     prefix( k_ )   inz
020300150205
020400150205       // -?File TITPT01L / TIOPT01L?
020500150205     d keyTITPT01    e ds                  extname( TITPT01L : *key )
020600150205     d                                     prefix( k_ )   inz
020700150205
020800150205       // -?File TITPD01L / TIOPD01L?
020900150205     d keyTITPD01    e ds                  extname( TITPD01L : *key )
021000150205     d                                     prefix( k_ )   inz
021100141022
021200141022       // -?File TFACO00F / TFIND00F / TFCLP00F?
021300150205     d keyTFACO00    e ds                  extname( TFACO00F : *key )
021400150205     d                                     prefix( k_ )   inz
021500141022
021600141022       // -?File TABEL00F?
021700150205     d keyTABEL00    e ds                  extname( TABEL00F : *key )
021800150205     d                                     prefix( k_ )   inz
021900141008
022000141008       //--------------------------------------------------------------
022100141008       //?M A I N - L I N E                                            ?
022200141008       //--------------------------------------------------------------
022300141008
022400141008     c     *Entry        plist
022500141008     c                   parm                    TNTA48ds
022600141008
022700141008      /free
022800141008
022900141008       // -?Operazioni iniziali?
023000141008       exsr  sr_RoutInz;
023100141008
023200141008       // -?Controlli sulla singola Tariffa/Offerta per Cliente?
023300141008       select;
023400141008         when  TNTA48ds.iTA48cto = 'T';
023500141008           exsr  sr_Ctrl_Tariffa;
023600141008         when  TNTA48ds.iTA48cto = 'O';
023700141008           exsr  sr_Ctrl_Offerta;
023800141008       endsl;
023900141022
024000141022       dsTA01 = TAMflo;
024100141022
024200141022       // -?Caricamento delle Tariffe Particolari?
024300141022       exsr  sr_CarConsegnePartic;
024400141010
024500141010       // -?Controlli sugli Scaglioni?
024600141010       exsr  sr_Scaglioni;
024700141010
024800141010       //// -?Controlli sull'Inoltro?
024900141010       //exsr  sr_Inoltro;
025000141010
025100141010       // -?Controlli sul Dettaglio Tariffario?
025200141017       $TariffePartic = *off;
025300141010       exsr  sr_Dettaglio;
025400141013
025500141013       // -?Controlli sulle Tariffe Particolari?
025600141013       $TariffePartic = *on;
025700141022       exsr  sr_TariffeParticolari;
025800141008
025900141008       // -?Operazioni finali?
026000141008       exsr  sr_RoutEnd;
026100141008
026200141008       //--------------------------------------------------------------
026300141008       //?Operazioni iniziali.                                         ?
026400141008       //--------------------------------------------------------------
026500141008       BEGSR  sr_RoutInz;
026600141008
026700141008         *inLR  = *on;
026800141008
026900141008         TNTA48ds.oTA48err = *off;
027000141008         clear  TNTA48ds.oTA48msg;
027100141008
027200141008         // -?Controllo parametri ricevuti?
027300141008         select;
027400141009           when  TNTA48ds.iTA48dsf = *blank  or
027500141009                 TNTA48ds.iTA48cto = *blank  or
027600141009                 TNTA48ds.iTA48ksc = *zero   or
027700141009                 TNTA48ds.iTA48ctr = *zero;
027800141008             TNTA48ds.oTA48err = *on;
027900141013             TNTA48ds.oTA48msg = sk_Msg(17);
028000141008             exsr  sr_RoutEnd;
028100141009           when  ( TNTA48ds.iTA48dsf <> 'S'  and
028200141009                   TNTA48ds.iTA48dsf <> 'F' )   OR
028300141009                 ( TNTA48ds.iTA48cto <> 'T'  and
028400141009                   TNTA48ds.iTA48cto <> 'O' )   OR
028500141008             TNTA48ds.oTA48err = *on;
028600141013             TNTA48ds.oTA48msg = sk_Msg(18);
028700141008             exsr  sr_RoutEnd;
028800141009         endsl;
028900141008
029000141008         // -?Reperimento dati job?
029100141008         exsr  sr_DatiJob;
029200141008
029300141008         // -?Apertura archivi?
029400141008         select;
029500141008           when  TNTA48ds.iTA48cto = 'T';
029600141008             open  TNTAM01L;
029700141008             open  TITAD04L;
029800141008           when  TNTA48ds.iTA48cto = 'O';
029900141008             open  TNOFM01L;
030000141008             open  TIOFD01L;
030100141008             open  TIVIS01L;
030200141008             open  TFACO00F;
030300141008             open  TFIND00F;
030400141008             open  TFCLP00F;
030500141008         endsl;
030600141009
030700141009         // -?Caricamento iniziale delle tabelle?
030800141009         exsr  sr_CarTab;
030900141008
031000141008       ENDSR;
031100141008
031200141008       //--------------------------------------------------------------
031300141008       //?Reperimento Dati del job (Utente/Operativi).                 ?
031400141008       //--------------------------------------------------------------
031500141008       BEGSR  sr_DatiJob;
031600141008
031700141008         in(E) §AzUte;
031800141008         if NOT %error;
031900141008           in(E) §DatiUte;
032000141008         endif;
032100141008         if %error or RSut = *blanks;
032200141008           clear TIBS34ds;
032300141008           tibs34r ( tibs34ds );
032400141008           in §AzUte;
032500141008           in §DatiUte;
032600141008         endif;
032700141008
032800141008       ENDSR;
032900141009
033000141009       //--------------------------------------------------------------
033100141009       //?Caricamento iniziale delle tabelle.                          ?
033200141009       //--------------------------------------------------------------
033300141009       BEGSR  sr_CarTab;
033400141009
033500141009         clear  keyTABEL00;
033600141009         k_TBLkut = 1;
033700141022         k_TBLcod = 'CT';
033800141009
033900141009         // -?Schiera di tutti i Codici Tassazione?
034000141009         clear  xK;
034100141009         clear  xK1;
034200141009         clear  sk_CT;         //?(Codici Tassazione)?
034300141009         clear  sk_CTrap;      //?(Sigle  Regioni di Appartenenza)?
034400141009         clear  sk_CReg;       //?(Codici Regione)?
034500141022
034600141009         setll  %kds( keyTABEL00 )  TABEL;
034700141009         reade  %kds( keyTABEL00 : 2 )  TABEL;
034800141009
034900141009         DoW  NOT %eof(TABEL00F);
035000141009
035100141009           If  TBLkey <> *blank;
035200141009
035300141009             dsCT = TBLuni;
035400141009             xK += 1;
035500141009             sk_CT(xK)    = TBLkey;
035600141009             sk_CTrap(xK) = §CTrap;
035700141009
035800141009             // -?SE è una Regione: caricamento schiera delle Regioni?
035900141009             if  %subst( §CTcor : %len(§CTcor) : 1 ) = '9';
036000141009               xK1 += 1;
036100141009               sk_CReg(xK1) = TBLkey;
036200141009             endif;
036300141009
036400141009           EndIf;
036500141009
036600141009           reade  %kds( keyTABEL00 : 2 )  TABEL;
036700141009
036800141009         EndDo;
036900141009
037000141009         // -?Schiera delle Nazioni (SOLO quella ITALIA)?
037100141009         clear  xK;
037200141009         clear  sk_15;         //?(Codici Nazione)?
037300141009         k_TBLcod = '15';
037400141009         setll  %kds( keyTABEL00 )  TABEL;
037500141009         reade  %kds( keyTABEL00 : 2 )  TABEL;
037600141009
037700141009         DoW  NOT %eof(TABEL00F);
037800141009
037900141009           If  TBLkey <> *blank;
038000141009
038100141009             dsCT = TBLuni;
038200141009             if  §15ita = 'I';
038300141009               xK += 1;
038400141009               sk_15(xK) = TBLkey;
038500141009             endif;
038600141009
038700141009           EndIf;
038800141009
038900141009           reade  %kds( keyTABEL00 : 2 )  TABEL;
039000141009
039100141009         EndDo;
039200141009
039300141009       ENDSR;
039400141008
039500141008       //--------------------------------------------------------------
039600141008       //?Controlli sulla singola TARIFFA per cliente.                 ?
039700141008       //--------------------------------------------------------------
039800141008       BEGSR  sr_Ctrl_Tariffa;
039900141008
040000141008         // -?Dati anagrafici del cliente?
040100141008         clear  TIBS69ds;
040200141008         I69kac = iTA48ksc;
040300141008         exsr  sr_AnagrCliente;
040400141017
040500141017         // -?Aggancio testata Tariffa?
040600141021         //  ?con Codice Cliente Tariffa = Numero Visita?
040700141017         clear  keyTNTAM01;
040800141017         k_TAMksc = iTA48ksc;
040900141017         k_TAMctr = iTA48ctr;
041000141017         k_TAMprg = iTA48prg;
041100141017         chain  %kds( keyTNTAM01 )  TNTAM000;
041200141008
041300141008       ENDSR;
041400141008
041500141008       //--------------------------------------------------------------
041600141008       //?Controlli sulla singola OFFERTA per cliente.                 ?
041700141008       //--------------------------------------------------------------
041800141008       BEGSR  sr_Ctrl_Offerta;
041900141008
042000141008         // -?Visita?
042100141008         chain  ( iTA48ksc )  TIVIS000;
042200141008
042300141009         If  NOT %found(TIVIS01L);
042400141008           TNTA48ds.oTA48err = *on;
042500141008           TNTA48ds.oTA48msg = sk_Msg(02);
042600141008           exsr  sr_RoutEnd;
042700141009         EndIf;
042800141008
042900141008         // -?Dati anagrafici del cliente - dal file delle Visite?
043000141008         k_ACOkut = 1;
043100141008         k_ACOkcc = DUTkci;
043200141008         k_ACOksc = VISnrv;
043300141008
043400141008         chain  %kds( keyTFACO00 )  TFACO000;
043500141008         chain  %kds( keyTFACO00 )  TFIND000;
043600141008         chain  %kds( keyTFACO00 )  TFCLP000;
043700141008
043800141008         // -?SE non trovato: dall'Anagrafica Clienti?
043900141008         If  NOT %found(TFACO00F)  or
044000141008             NOT %found(TFIND00F)  or
044100141008             NOT %found(TFCLP00F);
044200141008
044300141008           clear  TIBS69ds;
044400141009           I69kac = VISksc;
044500141008           exsr  sr_AnagrCliente;
044600141008
044700141008         EndIf;
044800141017
044900141017         // -?Aggancio testata Offerta?
045000141021         //  ?con Codice Cliente Tariffa?
045100141017         clear  keyTNTAM01;
045200141017         k_TAMksc = iTA48ksc;
045300141017         k_TAMctr = iTA48ctr;
045400141017         k_TAMprg = iTA48prg;
045500141017         chain  %kds( keyTNTAM01 )  TNOFM000;
045600141008
045700141008       ENDSR;
045800141008
045900141008       //--------------------------------------------------------------
046000141008       //?Controllo dati Anagrafici del Cliente.                       ?
046100141008       //--------------------------------------------------------------
046200141008       BEGSR  sr_AnagrCliente;
046300141008
046400141008         I69kcc = DUTkci;
046500141008         I69kin = I69kac;
046600141008         I69kcp = I69kac;
046700141008
046800141008         tibs69r( tibs69ds :
046900141008                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
047000141008
047100141009         If  O69err = *on;
047200141008           TNTA48ds.oTA48err = *on;
047300141008           TNTA48ds.oTA48msg = sk_Msg(03);
047400141008           exsr  sr_RoutEnd;
047500141009         EndIf;
047600141008
047700141008       ENDSR;
047800141022
047900141022       //--------------------------------------------------------------
048000141022       //?Caricamento delle Consegne Particolari che interessano la    ?
048100141022       //?  Tariffa che si sta elaborando.                             ?
048200141022       //--------------------------------------------------------------
048300141022       BEGSR  sr_CarConsegnePartic;
048400141022
048500141022         // -?Schiere delle Consegne Particolari?
048600141022         clear  xK;
048700141022         clear  sk_1Pftc;      //?(Codici Consegna Particolare)?
048800141022         clear  sk_1Ptst;      //?(Tipo Stampa)?
048900141022
049000141022         clear  keyTABEL00;
049100141022         k_TBLkut = 1;
049200141022         k_TBLcod = '1P';
049300141022
049400141022         setll  %kds( keyTABEL00 )  TABEL;
049500141022         reade  %kds( keyTABEL00 : 2 )  TABEL;
049600141022
049700141022         DoW  NOT %eof(TABEL00F);
049800141022
049900141022           ds1P = TBLuni;
050000141022
050100141022           Select;
050200141022
050300141022             // -?Tariffa Annullata?
050400141022             When  TBLkey <> *blank;
050500141022
050600141022             // -?Tariffa Particolare "C       " (Lasciato Avviso)?
050700141022             //  ?per le filiali NON abilitate: si accetta Cartello?
050800141022             //  ?(NON si elabora)?
050900141022             When  TBLkey =  'C       '  and  wComFil <> 888  and
051000141022                   %lookup( wComFil : sk_Lav ) = *zero        and
051100141022                   %lookup( 999     : sk_Lav ) = *zero;
051200141022
051300141022             // -?Per Tariffa con Servizio Poste: si escludono le?
051400141022             //  ?tariffe particolari con il flag §1PFPT = "N"?
051500141022             When  TAMtsp =  'P'  and  §1Pfpt = 'N';
051600141022
051700141022             // -?Per Tariffa con Network DPD: si escludono le?
051800141022             //  ?tariffe particolari con il flag §1PFDP = "N"?
051900141022             When  dsTA01.§TAdpd = 'P'  and  §1Pfdp = 'N';
052000141022
052100141022             // -?(Per Tariffa con Network FedEx: NON si fanno?
052200141022             //  ?controlli perchè NON vengono lette)?
052300141022
052400141022             // -?Per Tariffa EuroExpress: si escludono le?
052500141022             //  ?tariffe particolari con il flag §1PFIE = "N"?
052600141022             When  TAMfie =  'E'  and  §1Pfir = 'N';
052700141022
052800141022             // -?Per Tariffa Italia: si escludono le tariffe?
052900141022             //  ?particolari con il flag §1PTCO = "N" o Servizio Poste?
053000141022             When  TAMfie =  'I'  and  TAMfie <> 'P'  and  §1Ptco = 'N';
053100141022
053200141022             // -?Per Tariffa di Cartello: si verifica che NON sia?
053300141022             //  ?tipo servizio "P"?
053400141022             When  TAMfie =  *blank  and  TAMtsp <> 'P'  and  §1Ptco = 'N';
053500141022
053600141022             // -?Tariffa OK?
053700141022             Other;
053800141022               xK += 1;
053900141022               sk_1Pftc(xK) = TBLkey;
054000141022               sk_1Ptst(xK) = §1Ptst;
054100141022
054200141022           EndSl;
054300141022
054400141022           reade  %kds( keyTABEL00 : 2 )  TABEL;
054500141022
054600141022         EndDo;
054700141022
054800141022       ENDSR;
054900141008
055000141008       //--------------------------------------------------------------
055100141008       //?Controllo Scaglioni.                                         ?
055200141008       //--------------------------------------------------------------
055300141008       BEGSR  sr_Scaglioni;
055400141009
055500141009         clear  xK;
055600141009         clear  wSaveLNP;
055700141009         clear  wSaveCTS;
055800141009         clear  wSaveCAP;
055900141010         clear  wNumDec;
056000141022         $cap1scaglione = *off;
056100141022         $CtrCAP = *on;
056200141008
056300141021         // -?Impostazione chiave parziale per l'accesso agli scaglioni?
056400141021         clear  keyTITAD04;
056500141010         k_TADksc = iTA48ksc;
056600141010         k_TADctr = iTA48ctr;
056700141010         k_TADprg = iTA48prg;
056800141021         Select;
056900141021           // -?Se Tariffa: si recupera la filiale dal codice cliente?
057000141021           When  iTA48cto = 'T';
057100141021             k_TADlnp = iTA48ksc / 10000;
057200141021           // -?Se Visita ad un Cliente Nuovo: si recupera la filiale?
057300141021           //  ?dai primi 3 campi del codice Commerciale?
057400141021           When  VISksc = *zero;
057500141021             k_TADlnp = VIScmm / 10000;
057600141021           // -?Se Visita ad un Cliente esistente: si recupera la?
057700141021           //  ?filiale di appartenenza dal cliente?
057800141021           Other;
057900141021             k_TADlnp = VISksc / 10000;
058000141021         EndSl;
058100141010
058200141008         // -?Ricerca DETTAGLIO: prima con la LNP del cliente?
058300141008         if  iTA48cto = 'T';
058400141008           setll  %kds( keyTITAD04 : 4 )  TITAD000;
058500141008         else;
058600141008           setll  %kds( keyTITAD04 : 4 )  TIOFD000;
058700141008         endif;
058800141008
058900141008         // -?Se NON trovato:?
059000141008         IF  NOT %equal;
059100141008
059200141008           // -?Ricerca DETTAGLIO: senza la LNP del cliente?
059300141008           if  iTA48cto = 'T';
059400141008             setll  %kds( keyTITAD04 : 3 )  TITAD000;
059500141008           else;
059600141008             setll  %kds( keyTITAD04 : 3 )  TIOFD000;
059700141008           endif;
059800141008
059900141022           // -?Se NON trovato:?ERRORE?
060000141008           //  ?(manca dettaglio tariffario)?
060100141009           If  NOT %equal;
060200141022
060300141008             TNTA48ds.oTA48err = *on;
060400141008             TNTA48ds.oTA48msg = sk_Msg(04);
060500141008             exsr  sr_RoutEnd;
060600141022
060700141009           EndIf;
060800141008
060900141008         ENDIF;
061000141009
061100141009
061200141009         DOW  1 = 1;
061300141009
061400141009           if  iTA48cto = 'T';
061500141009             reade  %kds( keyTITAD04 : 3 )  TITAD000;
061600141009           else;
061700141009             reade  %kds( keyTITAD04 : 3 )  TIOFD000;
061800141009           endif;
061900141009
062000141009           // -?Se a fine lettura non risulta caricato nessuno scaglione?
062100141009           //  ?(xK = 0) ed è stato letti solo il dettaglio con CAP?
062200141009           //  ?valorizzato ($cap1scaglione = *on) => ci si riposiziona?
062300141009           //  ?con l'ultima chiave per caricare almeno gli scaglioni?
062400141009           If  %eof;
062500141009
062600141009             if  xK = 0  and  $cap1scaglione;
062700141009
062800141021               clear  keyTITAD04;
062900141009               k_TADksc = TADksc;
063000141009               k_TADctr = TADctr;
063100141009               k_TADprg = TADprg;
063200141009               k_TADlnp = TADlnp;
063300141009               k_TADorp = TADorp;
063400141009               k_TADnaz = TADnaz;
063500141009               k_TADcap = TADcap;
063600141009
063700141009               if  iTA48cto = 'T';
063800141009                 setll  %kds( keyTITAD04 )  TITAD000;
063900141009               else;
064000141009                 setll  %kds( keyTITAD04 )  TIOFD000;
064100141009               endif;
064200141009
064300141009               // -?Si valorizza il flag del NON controllo del CAP perché?
064400141009               //  ?altrimenti non si riescono a caricare gli scaglioni?
064500141009               $CtrCAP  = *off;
064600141009
064700141009               iter;
064800141009
064900141009             else;
065000141009
065100141009               // -?Altrimenti si esce dal ciclo di lettura?
065200141009               leave;
065300141009
065400141009             endif;
065500141009
065600141009           EndIf;
065700141009
065800141009
065900141009           // -?Si scartano le tariffe annullate?
066000141009           if  TADatb = 'A';
066100141009             iter;
066200141009           endif;
066300141009
066400141009           // -?Salvataggio dei campi per le rotture Regione/Provincia?
066500141009           if  wSaveLNP = *zero;
066600141009
066700141009             // -?Se controllo del CAP si deve fare ($CtrCAP = *on)?
066800141009             //  ?Se cap valorizzato nel 1° scaglione lo si segnala?
066900141009             //  ?per caricare schiera inoltro quando non c'è CAP e?
067000141009             //  ?si ritorna a leggere?
067100141009             if  TADcap  <> *blank  and
067200141009                 $CtrCAP  = *on;
067300141010               $cap1scaglione = *on;
067400141009               iter;
067500141009             endif;
067600141009
067700141009             wSaveLNP = TADlnp;
067800141009             wSaveCTS = TADcts;
067900141009             wSaveCAP = TADcap;
068000141009
068100141021             exsr  sr_CtrlRegione;
068200141021             exsr  sr_CtrlProvincia;
068300141009
068400141009           EndIf;
068500141009
068600141009           // -?Caricamento Scaglioni per LNP CTS e CAP uguali?
068700141009           If  TADlnp = wSaveLNP  and
068800141009               TADcts = wSaveCTS  and
068900141009               TADcap = wSaveCAP;
069000141009
069100141009             xK += 1;
069200141009
069300141022             // -?Troppi scaglioni:?ERRORE?
069400141009             If  xK > c_MaxSgl;
069500141022
069600141009               TNTA48ds.oTA48err = *on;
069700141009               TNTA48ds.oTA48msg = sk_Msg(05);
069800141009               exsr  sr_RoutEnd;
069900141022
070000141009             EndIf;
070100141009
070200141009             sk_sgl(xK) = TADsgl;
070300141009
070400141009             // -?Verifica se gli scaglioni hanno dei decimali?
070500141010             //  ?e quanti ne hanno (SE NON è Tariffa a Valore)?
070600141010             If  %subst( %editc( iTA48ctr : 'X' ) : 1 : 1 ) <> '4'  and
070700141010                 %int( TADsgl ) <> 99999;
070800141010
070900141010               select;
071000141010                 when  TADsgl - %int( TADsgl ) = *zero;
071100141010                   wNumDec = *zero;
071200141010                 when  ( TADsgl * 10 ) - %int( TADsgl * 10 ) = *zero;
071300141010                   wNumDec = 1;
071400141010                 when  ( TADsgl * 100 ) - %int( TADsgl * 100 ) = *zero;
071500141010                   wNumDec = 2;
071600141010                 other;
071700141010                   wNumDec = 3;
071800141010               endsl;
071900141009
072000141009             EndIf;
072100141009
072200141009             // -?CAP non valorizzato: si carica la schiera degli inoltri?
072300141009             if  TADcap = *blank;
072400141020               sk_Pro(xK) = TADino;
072500141010               clear  $cap1scaglione;
072600141009             endif;
072700141009
072800141009           Else;
072900141009
073000141009             // -?Rottura chiave?
073100141009             leave;
073200141009
073300141009           EndIf;
073400141009
073500141009         ENDDO;
073600141009
073700141009
073800141022         // -?Nessuno scaglione:?ERRORE?mancanza di dettaglio?
073900141009         If  xK = *zero;
074000141022
074100141009           TNTA48ds.oTA48err = *on;
074200141009           TNTA48ds.oTA48msg = sk_Msg(04);
074300141009           exsr  sr_RoutEnd;
074400141022
074500141009         EndIf;
074600141009
074700141010         // -?Memorizzazione del n° scaglioni trovati?
074800141009         wNrScaglioni = xK;
074900141009
075000141009         // -?Controllo se gli scaglioni sono stampabili?
075100141009         //  ?(max 4 interi)?
075200141009         For  xY = 1  By 1  To wNrScaglioni;
075300141009
075400141009           // -?Verifica quanti decimali ha lo scaglione?
075500141009           //  ?(se NON è Tariffa a Valore - senza decimali)?
075600141010           Select;
075700141010             //  ?· se scalgione 99999 non si controllano i decimali?
075800141010             When  %int( sk_Sgl(xY) ) = 99999;
075900141010             //  ?· scaglioni senza decimali: max 9999?
076000141010             When  wNumDec = *zero  and  %int( sk_Sgl(xY) ) > 9999;
076100141010               TNTA48ds.oTA48err = *on;
076200141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
076300141010                                   %editc( sk_Sgl(xY) : '3' ) +
076400141010                                   sk_Msg(11) : 1 );
076500141010               exsr  sr_RoutEnd;
076600141010             //  ?· scaglioni con 1 decimale: max 999,9?
076700141010             When  wNumDec = 1      and  %int( sk_Sgl(xY) ) > 999,9;
076800141010               TNTA48ds.oTA48err = *on;
076900141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
077000141010                                   %editc( sk_Sgl(xY) : '4' ) +
077100141010                                   sk_Msg(11) : 1 );
077200141010               exsr  sr_RoutEnd;
077300141010             //  ?· scaglioni con 2 decimali: max 99,99?
077400141010             When  wNumDec >= 2     and  %int( sk_Sgl(xY) ) > 99,99;
077500141010               TNTA48ds.oTA48err = *on;
077600141010               TNTA48ds.oTA48msg = %xlate( '&Scaglione____' :
077700141010                                   %editc( sk_Sgl(xY) : '4' ) +
077800141010                                   sk_Msg(11) : 1 );
077900141010               exsr  sr_RoutEnd;
078000141010           EndSl;
078100141010
078200141010         EndFor;
078300141008
078400141008       ENDSR;
078500141010
078600141010       ////--------------------------------------------------------------
078700141022       ////?Controllo Inoltro. (ex subr. sr_STINO)                       ?
078800141010       ////--------------------------------------------------------------
078900141010       //BEGSR  sr_Inoltro;
079000141010       //
079100141010       //  $Inoltro = *off;
079200141010       //
079300141010       //  // -?Lettura del DETTAGLIO e verifica - per ogni scaglione?
079400141010       //  //  ?valido - se l'inoltro è uguale?
079500141010       //  clear  keyTITAD04;
079600141010       //  k_TADksc = iTA48ksc;
079700141010       //  k_TADctr = iTA48ctr;
079800141010       //  k_TADprg = iTA48prg;
079900141010       //
080000141010       //  if  iTA48cto = 'T';
080100141010       //    setll  %kds( keyTITAD04 : 3 )  TITAD000;
080200141010       //    reade  %kds( keyTITAD04 : 3 )  TITAD000;
080300141010       //  else;
080400141010       //    setll  %kds( keyTITAD04 : 3 )  TIOFD000;
080500141010       //    reade  %kds( keyTITAD04 : 3 )  TIOFD000;
080600141010       //  endif;
080700141010       //
080800141010       //  DoW  Not %eof;
080900141010       //
081000141010       //    If  TADatb <> 'A';
081100141010       //
081200141010       //      if  NOT $StaLNP;
081300141010       //
081400141010       //        chain  (TADlnp)  AZORG;
081500141010       //        $StaLNP = ( %found(AZORG01L) );
081600141020       //
081700141020       //        wSaveLNP = TADlnp;
081800141020       //        wSaveCTS = TADcts;
081900141020       //        wSaveCAP = TADcap;
082000141020       //        wSavePRO = %subst( TADorp : 5 : 1 );
082100141021       //        wSaveREG = %subst( TADorp : 3 : 2 );
082200141020       //        work_LNP = wSaveLNP;
082300141020       //        work_CTS = wSaveCTS;
082400141020       //        work_CAP = wSaveCAP;
082500141020       //        work_PRO = wSavePRO;
082600141021       //        work_REG = wSaveREG;
082700141010       //
082800141021       //        // -?Controlli a rottura per Regione e Provincia?
082900141010       //        exsr  sr_CtrlRegione;
083000141021       //        exsr  sr_CtrlProvincia;
083100141010       //
083200141010       //      endif;
083300141010       //
083400141010       //      // -?Se CAP valorizzato: lettura del record successivo?
083500141010       //      if  TADcap = *blank;
083600141010       //
083700141010       //        // -?Verifica se scaglione presente?
083800141010       //        w0072 = TADsgl;
083900141010       //        xZ = %lookup( w0072 : sk_Sgl );
084000141010       //        // -?Controllo inoltro nella schiera?
084100141010       //        if  xZ > *zero  and  TADino <> sk_Pro(xZ);
084200141010       //          $Inoltro = *on;
084300141010       //          leave;
084400141010       //        endif;
084500141010       //
084600141010       //      endif;
084700141010       //
084800141010       //    EndIf;
084900141010       //
085000141010       //    if  iTA48cto = 'T';
085100141010       //      reade  %kds( keyTITAD04 : 3 )  TITAD000;
085200141010       //    else;
085300141010       //      reade  %kds( keyTITAD04 : 3 )  TIOFD000;
085400141010       //    endif;
085500141010       //
085600141010       //  EndDo;
085700141010       //
085800141010       //ENDSR;
085900141010
086000141010       //--------------------------------------------------------------
086100141022       //?Controllo del Dettaglio Tariffario. (ex subr. sr_ELADET)     ?
086200141010       //--------------------------------------------------------------
086300141020       BEGSR  sr_Dettaglio;
086400141020
086500141020         xK4 = 1;
086600141020         DoW  xK4 <= %elem(sk_Sgl);
086700141020
086800141021           // -?Caricamento Scaglioni (6 alla volta) per elaborare?
086900141021           //  ?il Dettaglio?
087000141020           sk_Ssl = %subarr( sk_Sgl : xK4 : %elem(sk_Ssl) );
087100141020
087200141020           // -?Elaborazione Dettaglio (ex subr. sr_FULCRO)?
087300141020           exsr  sr_Dettaglio1;
087400141020
087500141020           xK4 += %elem(sk_Ssl);
087600141022           If  xK4 <= c_MaxSgl  and  sk_Sgl(xK4) <> *zero;
087700141022
087800141020             clear  sk_Ssl;
087900141020             wSaveLNP = work_LNP;
088000141020             wSaveCTS = work_CTS;
088100141020             wSaveCAP = work_CAP;
088200141020             wSavePRO = work_PRO;
088300141020             wSaveREG = work_REG;
088400141022             // -?Pulizia e Controllo Dettaglio per Regione?
088500141020             exsr  sr_CtrlRegione;
088600141022             // -?Pulizia e Controllo Dettaglio per Provincia?
088700141021             exsr  sr_CtrlProvincia;
088800141022
088900141022           Else;
089000141022
089100141022             xK4 = c_MaxSgl + 1;
089200141020             leave;
089300141022
089400141022           EndIf;
089500141020
089600141020         EndDo;
089700141020
089800141020       ENDSR;
089900141020
090000141020       //--------------------------------------------------------------
090100141022       //?                                    (ex subr. sr_FULCRO)     ?
090200141022       //--------------------------------------------------------------
090300141020       BEGSR  sr_Dettaglio1;
090400141010
090500141010         clear  wSaveLNP;
090600141010         clear  wSaveCTS;
090700141010         clear  wSaveCAP;
090800141010
090900141017         // -?Lettura del Dettaglio Tariffario dall'inizio?
091000141010         clear  keyTITAD04;
091100141010         k_TADksc = iTA48ksc;
091200141010         k_TADctr = iTA48ctr;
091300141010         k_TADprg = iTA48prg;
091400141010
091500141010         if  iTA48cto = 'T';
091600141010           setll  %kds( keyTITAD04 : 3 )  TITAD000;
091700141010           reade  %kds( keyTITAD04 : 3 )  TITAD000;
091800141010         else;
091900141010           setll  %kds( keyTITAD04 : 3 )  TIOFD000;
092000141010           reade  %kds( keyTITAD04 : 3 )  TIOFD000;
092100141010         endif;
092200141010
092300141010         DoW  Not %eof;
092400141010
092500141010           if  TADatb <> 'A';
092600141010             exsr  sr_Dettaglio2;
092700141010           endif;
092800141010
092900141010           if  iTA48cto = 'T';
093000141010             reade  %kds( keyTITAD04 : 3 )  TITAD000;
093100141010           else;
093200141010             reade  %kds( keyTITAD04 : 3 )  TIOFD000;
093300141010           endif;
093400141010
093500141010         EndDo;
093600141010
093700141010
093800141021         // -?Ultimi Totali (per Provincia/CAP)?
093900141010         if  wSaveCAP = *blank;
094000141010           // -?Se CAP uguale a *blank:?
094100141010           //  ?si controllano i totali per Provincia?
094200141010           exsr  sr_ProTot;
094300141010         else;
094400141010           // -?Se CAP valorizzato:?
094500141010           //  ?si controllano i totali per Paese di Arrivo (CAP)?
094600141010           exsr  sr_CapTot;
094700141010         endif;
094800141021
094900141010         // -?Calcoli per Regione?
095000141010         exsr  sr_CalReg;
095100141010
095200141010       ENDSR;
095300141010
095400141010       //--------------------------------------------------------------
095500141010
095600141010       BEGSR  sr_Dettaglio2;
095700141010
095800141010         // -?Verifica se è uno scaglione previsto?
095900141010         w0072 = TADsgl;
096000141017         If  %lookup( w0072 : sk_Ssl ) = *zero;
096100141010
096200141010           // -?Se non trovato nei 6 scaglioni da stampare:?
096300141010           //  ?verifica se scaglione corretto?
096400141010           If  %lookup( w0072 : sk_Sgl ) = *zero;
096500141010
096600141022             // -?Errore:?scaglione non previsto per la tariffa?
096700141010             TNTA48ds.oTA48err = *on;
096800141017             TNTA48ds.oTA48msg = sk_Msg(08);
096900141010             exsr  sr_RoutEnd;
097000141010
097100141010           EndIf;
097200141010
097300141010           leavesr;
097400141010
097500141010         EndIf;
097600141017
097700141017         // -?Preparazione campi per eventuali "rotture"?
097800141017         wComREG = %subst( TADorp : 3 : 2 );
097900141017         wComPRO = %subst( TADorp : 5 : 1 );
098000141010
098100141010         Select;
098200141010
098300141017           // -?Rottura per   L I N E A   D I   P A R T E N Z A   ----*?
098400141010           When  TADlnp <> wSaveLNP;
098500141010             wSaveLNP = TADlnp;
098600141017             wSaveREG = wComREG;
098700141017             wSavePRO = wComPRO;
098800141022             // -?Controllo Totali?
098900141017             exsr  sr_Dettaglio3;
099000141010             // -?Calcoli per Regione?
099100141010             exsr  sr_CalReg;
099200141022             // -?Pulizia e Controllo Dettaglio per Regione?
099300141010             exsr  sr_CtrlRegione;
099400141022             // -?Pulizia e Controllo Dettaglio per Provincia?
099500141021             exsr  sr_CtrlProvincia;
099600141010
099700141017           // -?Rottura per   R E G I O N E   ------------------------*?
099800141010           When  wComREG <> wSaveREG;
099900141010             wSaveREG = wComREG;
100000141010             wSavePRO = wComPRO;
100100141022             // -?Controllo Totali?
100200141017             exsr  sr_Dettaglio3;
100300141010             // -?Calcoli per Regione?
100400141010             exsr  sr_CalReg;
100500141022             // -?Pulizia e Controllo Dettaglio per Regione?
100600141010             exsr  sr_CtrlRegione;
100700141022             // -?Pulizia e Controllo Dettaglio per Provincia?
100800141021             exsr  sr_CtrlProvincia;
100900141010
101000141017           // -?Rottura per   P R O V I N C I A   --------------------*?
101100141010           When  wComPRO <> wSavePRO;
101200141010             wSavePRO = wComPRO;
101300141022             // -?Controllo Totali?
101400141017             exsr  sr_Dettaglio3;
101500141010             // -?Controlli per Provincia?
101600141022             exsr  sr_CtrlProvincia;
101700141010
101800141017           // -?Rottura per   P A E S E   D I   A R R I V O   (CAP)   *?
101900141010           When  TADcap <> wSaveCAP;
102000141017             exsr  sr_Dettaglio3;
102100141010
102200141010         EndSl;
102300141022
102400141022         // -?Elaborazione Scaglione?
102500141022         //w0072 = TADsgl;    ?(già fatto prima)?
102600141022         xK1 = %lookup( w0072 : sk_Ssl );
102700141022         if  xK1 = *zero;
102800141022           xK1 = 1;
102900141022         endif;
103000141022         // -?Valorizzazione schiera importi?
103100141022         sk_Tct(xK1) = TADitr;
103200141022
103300141022         //if  $Inoltro;
103400141022         //  .....
103500141022         //endif;
103600141010
103700141010       ENDSR;
103800141017
103900141017       //--------------------------------------------------------------
104000141017
104100141017       BEGSR  sr_Dettaglio3;
104200141017
104300141021         // -?Verifica se CAP valorizzato?
104400141017         if  wSaveCAP = *blank;
104500141017           // -?Se CAP uguale a *blank:?
104600141017           //  ?si controllano i totali per Provincia?
104700141017           exsr  sr_ProTot;
104800141017         else;
104900141017           // -?Se CAP valorizzato:?
105000141017           //  ?si controllano i totali per Paese di Arrivo (CAP)?
105100141017           exsr  sr_CapTot;
105200141017         endif;
105300141017
105400141017         wSaveCAP = TADcap;
105500141017
105600141017       ENDSR;
105700141010
105800141010       //--------------------------------------------------------------
105900141010       //?Totali per   P A E S E   D I   A R R I V O   (CAP).          ?
106000141010       //--------------------------------------------------------------
106100141010       BEGSR  sr_CapTOT;
106200141010
106300141013         If  wNumPDE >= 30;
106400141021
106500141022           // -?Errore:?troppi Codici Tassazione?
106600141013           TNTA48ds.oTA48err = *on;
106700141013           TNTA48ds.oTA48msg = sk_Msg(12);
106800141013           exsr  sr_RoutEnd;
106900141021
107000141013         EndIf;
107100141013
107200141013         wNumPDE += 1;
107300141013
107400141013         // -?Controllo se ok tutti gli scaglioni?
107500141013         exsr  sr_CtrSca;
107600141010
107700141010       ENDSR;
107800141010
107900141010       //--------------------------------------------------------------
108000141010       //?Totali per   P R O V I N C I A .                             ?
108100141010       //--------------------------------------------------------------
108200141010       BEGSR  sr_ProTOT;
108300141010
108400141022         // -?Salvataggio della schiera sk_TCT nel primo sk_BIG libero?
108500141022         xK = %lookup( *blank : sk_BIG );
108600141022         If  xK = *zero;
108700141022
108800141022           // -?Errore:?troppe Province per Regione?
108900141013           TNTA48ds.oTA48err = *on;
109000141013           TNTA48ds.oTA48msg = sk_Msg(09);
109100141013           exsr  sr_RoutEnd;
109200141022
109300141013         EndIf;
109400141010
109500141013         // -?Controllo se ok tutti gli scaglioni?
109600141013         exsr  sr_CtrSca;
109700141017
109800141022         // -?Caricamento schiera delle province 6 elementi alla volta?
109900141022         //  ?(città) - 6 elementi x 7 bytes = 42 bytes?
110000141022         //For  xB = 1  To %elem(sk_TCT);
110100141022         //  sk_Big(xK) = %trim( sk_BIG(xK) ) +
110200141022         //               %editc( sk_TCT(xB) : 'X' );
110300141017         //EndFor;
110400141017         sk_Big(xK) = ds_sk_TCT;
110500141017
110600141022         sk_Brg(xK) = wCodORP;
110700141010
110800141010       ENDSR;
110900141010
111000141010       //--------------------------------------------------------------
111100141010       //?Calcoli per   R E G I O N E .                                ?
111200141010       //--------------------------------------------------------------
111300141010       BEGSR  sr_CalREG;
111400141010
111500141013         // -?N O T E del sorgente originale (TNTA49R):?
111600141013         //  ?Leggo BIG (schiera per ogni provincia della regione)?
111700141013         //  ?Per ogni provincia letta (quindi per ogni elemento) dopo?
111800141013         //  ?averlo caricato controllo se ci sono altre province uguali?
111900141013         //  ?se sì accodo nella skiera dei COD/TASSAZ. che hanno la?
112000141013         //  ?stessa tariffa?
112100141022         //  ?leggo i soli elementi con BRG>0 (a mano mano che li raggruppo?
112200141013         //  ?li abblenco). Mi fermo quando leggo la regione che è sempre?
112300141013         //  ?l'ultimo e va da solo : K=1 la descrizione è  CAPOLUOGHI?
112400141013         //  ?                        K>1 "    "    "    è  ALTRI?
112500141013
112600141022         // -?Ricerca ultimo elemento occupato in "Paesi di Arrivo"?
112700141022         //xP = %lookup( *blank : sk_PDE ) - 1;
112800141013         xP = wNumPDE;
112900141010
113000141013         For  xK = 1  To %elem( sk_Brg );     //?(max 15)?
113100141013
113200141013           if  sk_Brg(xK) <> *blank;
113300141013             // -?9 = Regione?
113400141022             if  sk_Brg(xK) = '9';
113500141022
113600141022               xP += 1;
113700141022               If  xP > 30;
113800141022
113900141022                 // -?Errore:?troppi Cod. Tassazione per la Regione?
114000141022                 TNTA48ds.oTA48err = *on;
114100141022                 TNTA48ds.oTA48msg = sk_Msg(12);
114200141022                 exsr  sr_RoutEnd;
114300141022
114400141022               EndIf;
114500141022               clear  sk_Big(xK);
114600141022               clear  sk_Brg(xK);
114700141022
114800141022             endif;
114900141013           endif;
115000141013
115100141013         EndFor;
115200141010
115300141010       ENDSR;
115400141010
115500141010       //--------------------------------------------------------------
115600141010       //?Controllo se OK gli scaglioni elaborati.                     ?
115700141010       //--------------------------------------------------------------
115800141010       BEGSR  sr_CtrSCA;
115900141010
116000141014         Select;
116100141010
116200141014           When  wNrScaglioni > 12;
116300141014             // -?Si sta elaborando il 1° o il 2° giro di scaglioni:?
116400141014             //  ?vuol dire che ci sono di sicuro 6 importi?
116500141022             if  xK4 <= 12;
116600141014               yy = 6;
116700141014             // -?Si sta elaborando il 3° giro di scaglioni:?
116800141014             //  ?si contano quanti importi si devo elaborare?
116900141014             else;
117000141014               yy = wNrScaglioni - 12;
117100141014             endif;
117200141010
117300141014           When  wNrScaglioni > 6  and  wNrScaglioni < 12;
117400141014             // -?Si sta elaborando il 1° giro di scaglioni:?
117500141014             //  ?vuol dire che ci sono di sicuro 6 importi?
117600141014             if  xK4 = 1;
117700141014               yy = 6;
117800141014             // -?Si sta elaborando il 2° giro di scaglioni:?
117900141014             //  ?si contano quanti importi si devo elaborare?
118000141014             else;
118100141014               yy = wNrScaglioni - 6;
118200141014             endif;
118300141010
118400141014           When  wNrScaglioni = 6;
118500141014             yy = 6;
118600141010
118700141014           When  wNrScaglioni < 6;
118800141014             yy = wNrScaglioni;
118900141014
119000141014         EndSl;
119100141010
119200141014         // -?Controllo se ci sono degli importi a zero?
119300141014         For  xx = 1  To  yy;
119400141014
119500141014           If  sk_Tct(xx) = *zero;
119600141022
119700141022             // -?Errore:?Scaglione imprevisto per Codice Tassazione?
119800141014             TNTA48ds.oTA48err = *on;
119900141014             TNTA48ds.oTA48msg = sk_Msg(08);
120000141014             exsr  sr_RoutEnd;
120100141022
120200141014           EndIf;
120300141014
120400141014         EndFor;
120500141010
120600141010       ENDSR;
120700141009
120800141009       //--------------------------------------------------------------
120900141022       //?Controllo Regione. (ex subr. REGDET)                         ?
121000141009       //--------------------------------------------------------------
121100141009       BEGSR  sr_CtrlRegione;
121200141021
121300141021         // -?Controllo Codice Tassazione?
121400141021         exsr  sr_CtrlCodTassazione;
121500141009
121600141009         // -?Se tariffa DPD e linea partenza cliente non DPD:?
121700141009         //  ?regione IT diventa regione EE?
121800141022         //dsTA01 = TAMflo;   ?(già fatto prima)?
121900141009         if  wCodREG       = 'IT'  and
122000141009             dsTA01.§TAdpd = 'S'   and
122100141009             Og143.§OGntw <> 'DPD';
122200141009           wCodREG = 'EE';
122300141009         endif;
122400141009
122500141021         clear  xK5;
122600141013         wK5 = %lookup( wCodREG : sk_CReg );
122700141013
122800141013         If  wK5 = *zero;
122900141009
123000141009           TNTA48ds.oTA48err = *on;
123100141009           TNTA48ds.oTA48msg = sk_Msg(07);
123200141009           exsr  sr_RoutEnd;
123300141009
123400141009         EndIf;
123500141013
123600141013         clear  sk_Big;
123700141013         clear  sk_Brg;
123800141013
123900141013         clear  wNumPDE;
124000141009
124100141009       ENDSR;
124200141021
124300141021       //--------------------------------------------------------------
124400141022       //?Controllo Provincia. (ex subr. PRODET)                       ?
124500141021       //--------------------------------------------------------------
124600141021       BEGSR  sr_CtrlProvincia;
124700141021
124800141021         // -?Controllo Codice Tassazione?
124900141021         exsr  sr_CtrlCodTassazione;
125000141021
125100141021         if  NOT $TariffePartic;
125200141021           wCodORP = %subst( TADorp : %len(TADorp) - %len(wCodORP) + 1 );
125300141021         else;
125400141021           wCodORP = %subst( TPDorp : %len(TPDorp) - %len(wCodORP) + 1 );
125500141021         endif;
125600141021
125700141021         wCodREG = sk_CTrap(xK5);
125800141021
125900141021       ENDSR;
126000141021
126100141021       //--------------------------------------------------------------
126200141022       //?Controllo Codice Tassazione. (ex subr. REGDET e PRODET)      ?
126300141021       //--------------------------------------------------------------
126400141021       BEGSR  sr_CtrlCodTassazione;
126500141021
126600141021         clear  xK5;
126700141021         if  NOT $TariffePartic;
126800141021           // -?TITAD?
126900141021           xK5 = %lookup( TADcts : sk_CT );
127000141021         else;
127100141021           // -?TITPD?
127200141021           xK5 = %lookup( TPDcts : sk_CT );
127300141021         endif;
127400141021
127500141021         If  xK5 = *zero;
127600141021
127700141021           TNTA48ds.oTA48err = *on;
127800141021           TNTA48ds.oTA48msg = sk_Msg(06);
127900141021           exsr  sr_RoutEnd;
128000141021
128100141021         EndIf;
128200141021
128300141021       ENDSR;
128400141022
128500141022       //--------------------------------------------------------------
128600141022       //?Controllo delle Tariffe Particolari. (ex subr. sr_STATPT)    ?
128700141022       //--------------------------------------------------------------
128800141022       BEGSR  sr_TariffeParticolari;
128900141022
129000141022         // -?Lettura della schiera delle Tariffe Particolari?
129100141022         //  ?(NON ordinata...)?
129200141022         For  xY = 1  To  %elem(sk_1Pftc);
129300141022
129400141022           Select;
129500150108             // -?Tariffa Particolare da stampare in modo standard?
129600141022             When  sk_1Ptst(xY) = *blank;
129700141022               exsr  sr_TariffaParticolareSTD;
129800150108             // -?Tariffa Particolare da NON stampare in modo standard?
129900141022             When  sk_1Ptst(xY) = 'N';
130000141022               select;
130100141022                 // -?Elevazione Limite Risarcibile?
130200141022                 when  sk_1Pftc(xY) = 'R ';
130300141022                   exsr  sr_ElevazioneLimiteRisarcibile;
130400141022                 // -?Mandato Assicuratico AC Plus?
130500141022                 when  sk_1Pftc(xY) = 'C ';
130600150108                   exsr  sr_MandatoAssicACplus; .......................
130700141022                 // -?Mandato Assicuratico AC Base?
130800141022                 when  sk_1Pftc(xY) = 'd ';
130900150108                   exsr  sr_MandatoAssicACbase; .......................
131000141022                 // -?Fuel Surchange?
131100141022                 when  sk_1Pftc(xY) = 'f ';
131200150108                   exsr  sr_FuelSurchange; ............................
131300141022               endsl;
131400141022           EndSl;
131500141022
131600141022         EndFor;
131700141022
131800141022       ENDSR;
131900141022
132000141022       //--------------------------------------------------------------
132100141022       //?Controllo dati della Tariffa Particolare da stampare in modo ?
132200141022       //?  standard. (ex subr. SR_TAPARSTD)                           ?
132300141022       //--------------------------------------------------------------
132400141022       BEGSR  sr_TariffaParticolareSTD;
132500141022
132600141022         clear  keyTITPT01;
132700141022         k_TPTksc = iTA48ksc;
132800141022         k_TPTctr = iTA48ctr;
132900141022         k_TPTprg = iTA48prg;
133000141022         k_TPTftc = sk_1Pftc(xY);   // -?" "?
133100141022
133200141022         if  iTA48cto = 'T';
133300141022           chain  %kds( keyTITPT01 )  TITPT000;
133400141022         else;
133500141022           chain  %kds( keyTITPT01 )  TIOPT000;
133600141022         endif;
133700141022
133800141022         if  Not %found  or  TPTatb = 'A';
133900141022           // -?Se costo da stampare?
134000141022           if  sk_Edac(xY) = 'S';
134100141022             if  sk_Esud(xY) = 'S';
134200141022               leavesr;
134300141022             endif;
134400141022           else;
134500141022             leavesr;
134600141022           endif;
134700141022         endif;
134800141022
134900141022         // -?Ricerca record validi con Tariffa o Minimo > *zero?
135000141022         exsr  sr_RicercaTariffaParticolare;
135100141022
135200141022         Select;
135300141022           // -?Nessun addebito?
135400141022           When  Not $RecVal;
135500141022             leavesr;
135600141022           // -?Reperito un unico dettaglio tariffario?
135700141022           When  wRecVal = 1;
135800141022             exsr  sr_GesUnico; .......................................
135900141022           // -?Reperita tariffa valorizzata?
136000141022           Other;
136100141022             exsr  sr_GesTPD; .........................................
136200141022         EndSl;
136300141022
136400141022       ENDSR;
136500141022
136600141022       //--------------------------------------------------------------
136700141022       //?Controllo dati della Tariffa Particolare                     ?
136800141022       //?  Elevazione Limite Risarcibile. (ex subr. SR_ELLIRIS)       ?
136900141022       //--------------------------------------------------------------
137000141022       BEGSR  sr_ElevazioneLimiteRisarcibile;
137100141022
137200141022         clear  keyTITPT01;
137300141022         k_TPTksc = iTA48ksc;
137400141022         k_TPTctr = iTA48ctr;
137500141022         k_TPTprg = iTA48prg;
137600141022         k_TPTftc = sk_1Pftc(xY);   // -?"R"?
137700141022
137800141022         if  iTA48cto = 'T';
137900141022           chain  %kds( keyTITPT01 )  TITPT000;
138000141022         else;
138100141022           chain  %kds( keyTITPT01 )  TIOPT000;
138200141022         endif;
138300141022
138400141022         if  Not %found  or  TPTatb = 'A';
138500141022           leavesr;
138600141022         endif;
138700141022
138800141022         // -?Ricerca record validi con Tariffa o Minimo > *zero?
138900141022         exsr  sr_RicercaTariffaParticolare;
139000150108
139100150108         ..............................................................
139200141022
139300141022       ENDSR;
139400150108
139500150108       //--------------------------------------------------------------
139600150108       //?Controllo dati della Tariffa Particolare                     ?
139700150108       //?  Mandato Assicurativo AC Plus.  (ex subr. SR_MANDATO)       ?
139800150108       //--------------------------------------------------------------
139900150108       BEGSR  sr_MandatoAssicACplus;
140000150108
140100150108         clear  keyTITPT01;
140200150108         k_TPTksc = iTA48ksc;
140300150108         k_TPTctr = iTA48ctr;
140400150108         k_TPTprg = iTA48prg;
140500150108         k_TPTftc = sk_1Pftc(xY);   // -?"C"?
140600150108
140700150108         if  iTA48cto = 'T';
140800150108           chain  %kds( keyTITPT01 )  TITPT000;
140900150108         else;
141000150108           chain  %kds( keyTITPT01 )  TIOPT000;
141100150108         endif;
141200150108
141300150108         if  Not %found  or  TPTatb = 'A';
141400150108           leavesr;
141500150108         endif;
141600150108
141700150108         // -?Se NON è Mandato a Valore Variabile e NON è fittizio:?
141800150108         if  TPTvlm <> *zero  and  TPTtma <> 'F';
141900150108
142000150108           // -?Decodifica del flag Valore Merce?
142100150108           clear  dsTR;
142200150108           clear  keyTABEL00;
142300150108           k_TBLkut = 1;
142400150108           k_TBLcod = 'TR';
142500150108           k_TBLkey = TPTvfm;
142600150108
142700150108           chain  %kds( keyTABEL00 )  TABEL;
142800150108
142900150108           if  %found(TABEL00F);
143000150108             dsTR = TBLuni;
143100150108           endif;
143200150108
143300150108         endif;
143400150108
143500150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
143600150108         exsr  sr_RicercaTariffaParticolare;
143700150108
143800150108         ..............................................................
143900150108
144000150108       ENDSR;
144100150108
144200150108       //--------------------------------------------------------------
144300150108       //?Controllo dati della Tariffa Particolare                     ?
144400150108       //?  Mandato Assicurativo AC Base.  (ex subr. SR_ACBASE)        ?
144500150108       //--------------------------------------------------------------
144600150108       BEGSR  sr_MandatoAssicACbase;
144700150108
144800150108         clear  keyTITPT01;
144900150108         k_TPTksc = iTA48ksc;
145000150108         k_TPTctr = iTA48ctr;
145100150108         k_TPTprg = iTA48prg;
145200150108         k_TPTftc = sk_1Pftc(xY);   // -?"d"?
145300150108
145400150108         if  iTA48cto = 'T';
145500150108           chain  %kds( keyTITPT01 )  TITPT000;
145600150108         else;
145700150108           chain  %kds( keyTITPT01 )  TIOPT000;
145800150108         endif;
145900150108
146000150108         if  Not %found  or  TPTatb = 'A';
146100150108           leavesr;
146200150108         endif;
146300150108
146400150108         // -?Decodifica del flag Valore Merce?
146500150108         clear  dsTR;
146600150108         clear  keyTABEL00;
146700150108         k_TBLkut = 1;
146800150108         k_TBLcod = 'TR';
146900150108         k_TBLkey = TPTvfm;
147000150108
147100150108         chain  %kds( keyTABEL00 )  TABEL;
147200150108
147300150108         if  %found(TABEL00F);
147400150108           dsTR = TBLuni;
147500150108         endif;
147600150108
147700150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
147800150108         exsr  sr_RicercaTariffaParticolare;
147900150108
148000150108         ..............................................................
148100150108
148200150108       ENDSR;
148300150108
148400150108       //--------------------------------------------------------------
148500150108       //?Controllo dati della Tariffa Particolare                     ?
148600150108       //?  Fuel Surchange.                (ex subr. SR_FUEL)          ?
148700150108       //--------------------------------------------------------------
148800150108       BEGSR  sr_FuelSurchange;
148900150108
149000150108         clear  keyTITPT01;
149100150108         k_TPTksc = iTA48ksc;
149200150108         k_TPTctr = iTA48ctr;
149300150108         k_TPTprg = iTA48prg;
149400150108         k_TPTftc = sk_1Pftc(xY);   // -?"f"?
149500150108
149600150108         if  iTA48cto = 'T';
149700150108           chain  %kds( keyTITPT01 )  TITPT000;
149800150108         else;
149900150108           chain  %kds( keyTITPT01 )  TIOPT000;
150000150108         endif;
150100150108
150200150108         if  Not %found  or  TPTatb = 'A';
150300150108           leavesr;
150400150108         endif;
150500150108
150600150108         dTPT01 = TPTflo;
150700150108
150800150108         // -?Ricerca record validi con Tariffa o Minimo > *zero?
150900150108         exsr  sr_RicercaTariffaParticolare;
151000150108
151100150108         ..............................................................
151200150108
151300150108         // -?Recupero del Prezzo Medio del Gasolio?
151400150108         wData_Iso1 = *loval;
151500150108         wData_Iso2 = *hival;
151600150108         setll  ( TPTdpb )  TIPMG000;
151700150108         read  TIPMG000;
151800150108         if  Not %eof(TIPMG01L);
151900150108           if  PMGdtd > *zero;
152000150108             wData_Iso1 = %date( PMGdtd : *iso );
152100150108           endif;
152200150108           wData_Iso2 = %date( PMGdpb : *iso );
152300150108         endif;
152400150108
152500150108         If  %eof(TIPMG01L)  or  PMGpmg = *zero  or
152600150108             %diff( wData_Iso2 : wData_Iso1 : *d ) > 6;
152700150108           oTA48err = *on;
152800150108           oTA48msg = sk_Msg(01);
152900150108           leavesr;
153000150108         EndIf;
153100150108
153200150108         ..............................................................
153300150108
153400150108       ENDSR;
153500141022
153600141022       //--------------------------------------------------------------
153700141022       //?Controllo esistenza di un rec. valido e con                  ?
153800141022       //?  Tariffa + Minimo > *zero. (ex subr. RICREC)                ?
153900141022       //--------------------------------------------------------------
154000141022       BEGSR  sr_RicercaTariffaParticolare;
154100141022
154200141022         clear  $RecVal;
154300141022         clear  wRecVal;
154400141022         clear  work_FTC;
154500141022         clear  work_ORP;
154600141022         clear  work_SGL;
154700141022
154800141022         clear  keyTITPD01;
154900141022         k_TPDksc = iTA48ksc;
155000141022         k_TPDctr = iTA48ctr;
155100141022         k_TPDprg = iTA48prg;
155200141022         k_TPDftc = sk_1Pftc(xY);
155300141022
155400141022         if  iTA48cto = 'T';
155500141022           setll  %kds( keyTITPD01 )  TITPD000;
155600141022           reade  %kds( keyTITPD01 : 4 )  TITPD000;
155700141022         else;
155800141022           setll  %kds( keyTITPD01 )  TIOPD000;
155900141022           reade  %kds( keyTITPD01 : 4 )  TIOPD000;
156000141022         endif;
156100141022
156200141022         DoW  NOT %eof;
156300141022
156400141022           If  TPDatb <> 'A';
156500141022
156600141022             if  TPDitr = *zero  and  TPDmin = *zero;
156700141022
156800141022               // -?Memorizzazione chiave per il riaggancio del 1°?
156900141022               //  ?rec. valido?
157000141022               if  Not $RecVal;
157100141022                 $RecVal  = *on;
157200141022                 work_FTC = TPDftc;
157300141022                 work_ORP = TPDorp;
157400141022                 work_SGL = TPDsgl;
157500141022               endif;
157600141022
157700141022             else;
157800141022
157900141022               wRecVal += 1;
158000141022
158100141022               // -?Memorizzazione chiave per il riaggancio del 1°?
158200141022               //  ?rec. valido se ancora *blank?
158300141022               if  wRecVal = 1  and  work_FTC = *blank;
158400141022                 work_FTC = TPDftc;
158500141022                 work_ORP = TPDorp;
158600141022                 work_SGL = TPDsgl;
158700141022               endif;
158800141022
158900141022               // -?SE trovati più di 1 rec.: NON si prosegue nella?
159000141022               //  ?lettura?
159100141022               if  wRecVal > 1;
159200141022                 // -?Riposizionamento sul 1° rec. valido trovato?
159300141022                 k_TPDftc = work_FTC;
159400141022                 k_TPDorp = work_ORP;
159500141022                 k_TPDsgl = work_SGL;
159600141022                 if  iTA48cto = 'T';
159700141022                   chain  %kds( keyTITPD01 )  TITPD000;
159800141022                 else;
159900141022                   chain  %kds( keyTITPD01 )  TIOPD000;
160000141022                 endif;
160100141022                 leave;
160200141022               endif;
160300141022
160400141022             endif;
160500141022
160600141022           EndIf;
160700141022
160800141022           if  iTA48cto = 'T';
160900141022             reade  %kds( keyTITPD01 : 4 )  TITPD000;
161000141022           else;
161100141022             reade  %kds( keyTITPD01 : 4 )  TIOPD000;
161200141022           endif;
161300141022
161400141022         EndDo;
161500141022
161600141022         ..............................................................
161700141022      * se al primo record valido verifico che non ha lo stesso ordinamento
161800141022      * regione e provincia del record a zero forzo per la stampa in dettaglio
161900141022      *
162000141022     c                   if        recval = 1 and tpdorp <> savorp1 and
162100141022     c                             savorp1 <> *blanks
162200141022     c                   eval      recval = 9
162300141022      * mi riposiziono sul prima record valido trovato
162400141022     c  n98ktpdwrk       chain     titpd01l
162500141022     c   98ktpdwrk       chain     tiopd01l
162600141022     c                   clear                   savorp1
162700141022     c                   endif
162800141022      * Se ho trovato un solo record valido verifico lo scaglione trovato
162900141022     c                   if        recval = 1
163000141022     c                   z-add     recsgl        w0050
163100141022     c                   if        w0050 <> 99999 and tpttpg <> '2'
163200141022      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
163300141022      * e tipo tariffa non è a spedizione
163400141022     c                   eval      recval = 9
163500141022      * mi riposiziono sul prima record valido trovato
163600141022     c  n98ktpdwrk       chain     titpd01l
163700141022     c   98ktpdwrk       chain     tiopd01l
163800141022     c                   endif
163900141022     c                   endif
164000141022
164100141022       ENDSR;
164200141008
164300141008       //--------------------------------------------------------------
164400141008       //?Operazioni finali.                                           ?
164500141008       //--------------------------------------------------------------
164600141008       BEGSR  sr_RoutEnd;
164700141008
164800141008         // -?Chiusura applicazioni?
164900141021         ..............................................................
165000141008
165100141008         // -?Uscita dal *pgm?
165200141008         return;
165300141008
165400141008       ENDSR;
165500141008
165600141008      /end-free
165700141008
165800141008       //--------------------------------------------------------------
165900141008       //?Definizione schiere a tempo di compilazione                  ?
166000141008       //--------------------------------------------------------------
166100141008
166200141008** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
166300141008ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero!!    1
166400141009+ Numero visita errato                                                         2
166500141008CLIENTE NON CODIFICATO O ANNULLATO                                             3
166600141008NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                             4
166700141008PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE                                    5
166800141008CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                    6
166900141008CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                  7
167000141008UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO            8
167100141008PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T A D  NON ALLINEATO   9
167200141008PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
167300141010+ Scaglione &Scaglione____ non stampabile                                     11
167400141008TROPPI PAESI DI ARRIVO/CODICI DI TASSAZIONE PER LA REGIONE                    12
167500141008                                                                              13
167600141008UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   14
167700141008PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        15
167800141008UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     16
167900141021+ Mancano parametri obbligatori                                               17
168000141021+ Ricevuti parametri errati                                                   18
