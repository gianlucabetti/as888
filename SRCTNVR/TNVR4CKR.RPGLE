000100091216       //--------------------------------------------------------------
000200091216       //?TNVR4CKR - Verifica cliente intestazione fattura in tab. "4C"?
000300091216       //--------------------------------------------------------------
000400091216
000500091216     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600091216     h dftactgrp(*no)
000700091216
000800091216       //--------------------------------------------------------------
000900091216       //?Dichiarazione file.                                          ?
001000091216       //--------------------------------------------------------------
001100091216
001200091216       // - Tabelle
001300091216     fTABEL00F  if   e           k disk
001400091216
001500091216       // - Output
001600091216     fWF4CK00F  o    e             disk    usropn
001700091216
001800091216       //--------------------------------------------------------------
001900091216       //?Definizione costanti.                                        ?
002000091216       //--------------------------------------------------------------
002100091216
002200091216     d c_Kut           c                   const(1)
002300091216     d c_Tab           c                   const('4C')
002400091216
002500091216       //--------------------------------------------------------------
002600091216       //?Definizione schiere.                                         ?
002700091216       //--------------------------------------------------------------
002800091216
002900091216
003000091216       //--------------------------------------------------------------
003100091216       //?Definizione aree dati.                                       ?
003200091216       //--------------------------------------------------------------
003300091216
003400091216       // - Dati utente
003500091216     d §AzUte        e ds                  extname(AZUTE00F)
003600091216     d                                     dtaara
003700091216     d §DatiUte      e ds                  extname(dDatiUte)
003800091216     d                                     dtaara
003900091216
004000091216       //--------------------------------------------------------------
004100091216       //?Definizione strutture dati.                                  ?
004200091216       //--------------------------------------------------------------
004300091216
004400091216       // - Status
004500091216     d Status         sds
004600091216     d   SDSpgm          *proc
004700091216
004800091216       // - Parametri ricevuti
004900091216     d KPJBA         e ds
005000091216
005100091216       // - Reperimento dati utente
005200091216     d TIBS34ds      e ds                  inz
005300091216
005400091216       // - Tabella "4C"
005500091216     d ds4C          e ds                  inz
005600091216
005700091216       // - Parametri per pgm. TIBS10R (reperimento legami)
005800091216     d TIBS10ds      e ds                  inz
005900091216     d  $skFigli              21   5520  0 dim(500)
006000091216
006100091216       // - Parametri per pgm. TIBS69R (decodifica cliente)
006200091216     d TIBS69ds      e ds                  inz
006300091216     d ds_CNACO      e ds                  extname(CNACO00F) inz
006400091216     d ds_CNIND      e ds                  extname(CNIND00F) inz
006500091216     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006600091216     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006700091216
006800091216       //--------------------------------------------------------------
006900091216       //?Definizione variabili globali.                               ?
007000091216       //--------------------------------------------------------------
007100091216
007200091216       // - Flags booleani
007300091216     d $Err            s               n   inz
007400091216
007500091216       // - Parametri API QCAPCMD (Process Commands)
007600091216      /copy qsysinc/qrpglesrc,QCAPCMD
007700091216     d Qcmd            s            512    inz  varying
007800091216
007900091216       // - Parametri gestione errori API.
008000091216      /copy qsysinc/qrpglesrc,QUSEC
008100091216
008200091216       //--------------------------------------------------------------
008300091216       //?Definizione procedure usate.                                 ?
008400091216       //--------------------------------------------------------------
008500091216
008600091216       // - API QCAPCMD (Process Commands)
008700091216      /copy gaitrasrc/srcProtoPR,QCAPCMD
008800091216
008900091216       // - Reperimento legami
009000091216      /copy gaitrasrc/srcProtoPR,TIBS10R
009100091216
009200091216       // - Reperimento dati utente
009300091216      /copy gaitrasrc/srcProtoPR,TIBS34R
009400091216
009500091216       // - Controllo/Decodifica cliente
009600091216      /copy gaitrasrc/srcprotopr,TIBS69R
009700091216
009800091216       //--------------------------------------------------------------
009900091216       //?Definizione key-list.                                        ?
010000091216       //--------------------------------------------------------------
010100091216
010200091216       // - file TABEL00F
010300091216     d k03tabel00    e ds                  extname(TABEL00F : *key)
010400091216     d                                     inz     prefix(k_)
010500091216
010600091216       //--------------------------------------------------------------
010700091216       //?M A I N - L I N E.                                           ?
010800091216       //--------------------------------------------------------------
010900091216
011000091216     c     *Entry        plist
011100091216     c                   parm                    KPJBA
011200091216
011300091216      /free
011400091216
011500091216       // -?Operazioni iniziali?
011600091216       exsr  sr_RoutInz;
011700091216
011800091216       // -?Elaborazione tab. "4C"?
011900091216       exsr  sr_ElabTab4C;
012000091216
012100091216       // -?Operazioni finali?
012200091216       exsr  sr_RoutEnd;
012300091216
012400091216       //--------------------------------------------------------------
012500091216       //?Operazioni iniziali.                                         ?
012600091216       //--------------------------------------------------------------
012700091216       BEGSR  sr_RoutInz;
012800091216
012900091216         *inLR = *on;
013000091216
013100091216         // - Reperimento dati job
013200091216         exsr  sr_DatiJob;
013300091216
013400091216         // - Pulizia ed apertura del WrkF
013500091216         Qcmd = 'CLRPFM file(WF4CK00F) mbr(*first)';
013600091216         exsr  sr_ExecCmd;
013700091216         if  Qusei <> *blank;
013800091216           return;
013900091216         endif;
014000091216
014100091216         open  WF4CK00F;
014200091216
014300091216       ENDSR;
014400091216
014500091216       //--------------------------------------------------------------
014600091216       //?Reperimento Dati del job (Utente/Operativi).                 ?
014700091216       //--------------------------------------------------------------
014800091216       BEGSR  sr_DatiJob;
014900091216
015000091216         in(E) §AzUte;
015100091216         if NOT %error;
015200091216           in(E) §DatiUte;
015300091216         endif;
015400091216         if %error or RSut = *blanks;
015500091216           clear TIBS34ds;
015600091216           tibs34r(tibs34ds);
015700091216           in §AzUte;
015800091216           in §DatiUte;
015900091216         endif;
016000091216
016100091216       ENDSR;
016200091216
016300091216       //--------------------------------------------------------------
016400091216       //?Esecuzione del comando (già impostato).                      ?
016500091216       //--------------------------------------------------------------
016600091216       BEGSR  sr_ExecCmd;
016700091216
016800091216         clear Qcap0100;
016900091216         Qcabcsdh = *off;
017000091216         Qcapa    = *off;
017100091216         Qcacmdss = *off;
017200091216         Qcaerved = *allX'00';
017300091216
017400091216         clear Qusec;
017500091216         Qusbprv  = %size(Qusec);
017600091216
017700091216         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
017800091216                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
017900091216                           0 : 0 : Qusec);
018000091216
018100091216         //if  Qusei <> *blank;
018200091216         //  ...;
018300091216         //endif;
018400091216
018500091216       ENDSR;
018600091216
018700091216       //--------------------------------------------------------------
018800091216       //?Elaborazione tabella "4C"                                    ?
018900091216       //--------------------------------------------------------------
019000091216       BEGSR  sr_ElabTab4C;
019100091216
019200091216         k_TBLkut = c_Kut;
019300091216         k_TBLcod = c_Tab;
019400091216         k_TBLkey = *loval;
019500091216         setll  %kds(k03tabel00)  TABEL;
019600091216         reade  %kds(k03tabel00 : 2)  TABEL;
019700091216
019800091216         DoW  not %eof(TABEL00F);
019900091216
020000091216           if  TBLflg = *blank;
020100091216
020200091216             ds4C = TBLuni;
020300091216             exsr  sr_Ctrl4Cfks;
020400091216
020500091216           endif;
020600091216
020700091216           reade  %kds(k03tabel00 : 2)  TABEL;
020800091216
020900091216         EndDo;
021000091216
021100091216       ENDSR;
021200091216
021300091216       //--------------------------------------------------------------
021400091216       //?Controllo cliente intestazione fattura in tab. "4C"          ?
021500091216       //--------------------------------------------------------------
021600091216       BEGSR  sr_Ctrl4Cfks;
021700091216
021800091216         reset  $Err;
021900091216         clear  WF4CK000;
022000091216
022100091216         // -?Decodifica cliente invio dati (§4Cfks)?
022200091216         clear  TIBS69ds;
022300091216         if  §4Cfks <> *zero;
022400091216           I69kcc = DUTkci;
022500091216           I69kac = §4Cfks;
022600091216           I69kcp = I69kac;
022700091216           I69sif = knsif;
022800091216           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
022900091216           if  O69err = *on;
023000091216             $Err   = *on;
023100091216             //ACOrag = 'Cliente invio dati errato';
023200091216             ACOrag = O69msg;
023300091216           endif;
023400091216           wfFKS  = §4Cfks;
023500091216           wfFRA  = ACOrag;
023600091216           wfFSC  = CLPscf;
023700091216         endif;
023800091216
023900091216         // -?Decodifica intestatario fattura cliente invio dati (CLPSCF)?
024000091216         clear  TIBS69ds;
024100091216         if  wfFSC <> *zero;
024200091216           I69kcc = DUTkci;
024300091216           I69kac = wfFSC;
024400091216           I69kin = I69kac;
024500091216           I69sif = knsif;
024600091216           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
024700091216           if  O69err = *on;
024800091216             $Err   = *on;
024900091216             //ACOrag = 'Codice intestatario fattura cliente invio dati errato';
025000091216             ACOrag = O69msg;
025100091216           endif;
025200091216           wfFRS  = ACOrag;
025300091216           wfFPI  = INDiva;
025400091216         endif;
025500091216
025600091216         // -?Reperimento dati del cliente in esame (da tab. "4C")?
025700091216         clear  TIBS69ds;
025800091216         if  %trimr(TBLkey) <> %editc(§4Cfks : 'X');
025900091216           I69kcc = DUTkci;
026000091216           I69kac = %int( %trimr(TBLkey) );
026100091216           I69kcp = I69kac;
026200091216           I69sif = knsif;
026300091216           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
026400091216         endif;
026500091216         if  O69err = *on;
026600091216           $Err   = *on;
026700091216           //ACOrag = 'Codice cliente errato';
026800091216           ACOrag = O69msg;
026900091216         endif;
027000091216         wfKSC  = %int( %trimr(TBLkey) );
027100091216         wfRAG  = ACOrag;
027200091216         wfSCF  = CLPscf;
027300091216
027400091216         // -?Decodifica intestatario fattura cliente invio dati (CLPSCF)?
027500091216         clear  TIBS69ds;
027600091216         if  wfSCF <> *zero;
027700091216           I69kcc = DUTkci;
027800091216           I69kac = wfSCF;
027900091216           I69kin = I69kac;
028000091216           I69sif = knsif;
028100091216           tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
028200091216           if  O69err = *on;
028300091216             $Err   = *on;
028400091216             //ACOrag = 'Codice cliente invio dati errato';
028500091216             ACOrag = O69msg;
028600091216           endif;
028700091216           wfRSF  = ACOrag;
028800091216           wfPIF  = INDiva;
028900091216         endif;
029000091216
029100091216
029200091216         select;
029300091216
029400091216           // -Cliente errato?
029500091216           when  $Err = *on;
029600091216
029700091216           // -Cliente di fatturazione NON inserito => ok?
029800091216           when  wfFKS = *zero;
029900091216             leavesr;
030000091216
030100091216           // -Cliente di fatturazione = Cliente tabella => ok?
030200091216           when  wfFKS = wfKSC;
030300091216             leavesr;
030400091216
030500091216           // -Cliente di fatturazione = Codice intestazione fattura?
030600091216           //  => ok?
030700091216           when  wfFKS = wfSCF;
030800091216             leavesr;
030900091216
031000091216           // -Stesso codice intestazione fattura => ok?
031100091216           when  wfFSC = wfSCF;
031200091216             leavesr;
031300091216
031400091216           // -Codice intestazione fattura diverso => ERRORE?
031500091216           other;
031600091216             $Err = *on;
031700091216
031800091216         endsl;
031900091216
032000091216
032100091216         // -Registrazione dati errati nel Work-File?
032200091216         If  $Err;
032300091216
032400091216           // -?Verifica legami tra i due clienti WFKSC e WFFKS?
032500091216           clear tibs10ds;
032600091216           D10drf = *date;
032700091216           D10tle = 'WW';
032800091216           D10paf = 'P';
032900091216           D10cod = wfKSC;
033000091216           tibs10r ( TIBS10ds );
033100091216           if  D10err = *blank  and (%lookup(wfFKS : $skFigli) > *zero
033200091216                                 or  wfFKS = D10cop);
033300091216             wfFLG = 'S';
033400091216           else;
033500091216             clear tibs10ds;
033600091216             D10drf = *date;
033700091216             D10tle = 'WW';
033800091216             D10paf = 'P';
033900091216             D10cod = wfFKS;
034000091216             tibs10r ( TIBS10ds );
034100091216             if  D10err = *blank  and (%lookup(wfKSC : $skFigli) > *zero
034200091216                                   or  wfKSC = D10cop);
034300091216               wfFLG = 'S';
034400091216             else;
034500091216               wfFLG = 'N';
034600091216             endif;
034700091216           endif;
034800091216
034900091216           // -?Verifica legami tra i due clienti intestatari fatture?
035000091216           //  ?WFSCF e WFFSC?
035100091216           clear tibs10ds;
035200091216           D10drf = *date;
035300091216           D10tle = 'WW';
035400091216           D10paf = 'P';
035500091216           D10cod = wfSCF;
035600091216           tibs10r ( TIBS10ds );
035700091216           if  D10err = *blank  and (%lookup(wfFSC : $skFigli) > *zero
035800091216                                 or  wfFSC = D10cop);
035900091216             wfFLI = 'S';
036000091216           else;
036100091216             clear tibs10ds;
036200091216             D10drf = *date;
036300091216             D10tle = 'WW';
036400091216             D10paf = 'P';
036500091216             D10cod = wfFSC;
036600091216             tibs10r ( TIBS10ds );
036700091216             if  D10err = *blank  and (%lookup(wfSCF : $skFigli) > *zero
036800091216                                   or  wfSCF = D10cop);
036900091216               wfFLI = 'S';
037000091216             else;
037100091216               wfFLI = 'N';
037200091216             endif;
037300091216           endif;
037400091216
037500091216           // -?Registrazione dati errati nel Work-File?
037600091216           //______________
037700091216           write  WF4CK000;
037800091216           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
037900091216
038000091216         EndIf;
038100091216
038200091216       ENDSR;
038300091216
038400091216       //--------------------------------------------------------------
038500091216       //?Operazioni finali.                                           ?
038600091216       //--------------------------------------------------------------
038700091216       BEGSR  sr_RoutEnd;
038800091216
038900091216         if  %open(WF4CK00F);
039000091216           close  WF4CK00F;
039100091216         endif;
039200091216
039300091216         return;
039400091216
039500091216       ENDSR;
039600091216
039700091216      /end-free
