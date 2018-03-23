000100100511     /*CMD ovrdbf file(TNVIS02L) tofile(FILTRAGRPF/TNVIS02L) +
000200100511     /*CMD        ovrscope(*calllvl)
000300100511     /*CMD ovrdbf file(WFACV00F) tofile(GAITRAAZP/WFACV00F) +
000400100511     /*CMD        ovrscope(*calllvl)
000500100511     /*END dltovr file(TNVIS02L) lvl(*)
000600100511     /*END dltovr file(WFACV00F) lvl(*)
000700100511     /*END
000800100507       //==============================================================
000900100511       //?TNVR22R6 * Visite di commerciali con raggruppamento area     ?
001000100511       //?           diversa da quella del relativo unificante         ?
001100100507       //==============================================================
001200100507
001300100507       //--------------------------------------------------------------
001400100507       //?Specifiche di controllo.                                     ?
001500100507       //--------------------------------------------------------------
001600100507
001700100510     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001800100507     h dftactgrp(*no)
001900100510     h bnddir('UBRTVNETA')
002000100507
002100100507       //--------------------------------------------------------------
002200100507       //?Dichiarazione file.                                          ?
002300100507       //--------------------------------------------------------------
002400100507
002500100507       // -?Trattative a clienti?
002600100510     fTNVIS02L  if   e           k disk    usropn
002700100511     f                                     extfile(wFLib1)
002800100507
002900100507       // -?Tabelle?
003000100507     fTABEL00F  if   e           k disk
003100100507
003200100507       // -?Organigramma?
003300100507     fAZORG01L  if   e           k disk
003400100507
003500100510       // -?Elenco visite di commerciali di area diversa da quella?
003600100510       //  ?dell'unificante?
003700100510     fWFACV00F  o    e             disk    usropn
003800100511     f                                     extfile(wFLib2)
003900100507
004000100507       //--------------------------------------------------------------
004100100507       //?Definizione costanti.                                        ?
004200100507       //--------------------------------------------------------------
004300100507
004400100507     d c_DataMin       c                   const(20100401)
004500100510
004600100510       // -?Comando per pulizia work-file?
004700100511     d c_CmdClrPfm1    c                   const('CLRPFM +
004800100511     d                                           file(GAITRAAZM/+
004900100511     d                                                WFACV00F) +
005000100510     d                                           ')
005100100511     d c_CmdClrPfm2    c                   const('CLRPFM +
005200100511     d                                           file(GAITRAAZP/+
005300100511     d                                                WFACV00F) +
005400100511     d                                           ')
005500100507
005600100507       //--------------------------------------------------------------
005700100507       //?Definizione schiere.                                         ?
005800100507       //--------------------------------------------------------------
005900100507
006000100507
006100100507       //--------------------------------------------------------------
006200100507       //?Definizione aree dati.                                       ?
006300100507       //--------------------------------------------------------------
006400100507
006500100507       // -?Dati utente?
006600100507     d §AzUte        e ds                  extname(AZUTE00F)
006700100507     d                                     dtaara
006800100507     d §DatiUte      e ds                  extname(dDatiUte)
006900100507     d                                     dtaara
007000100507
007100100507       //--------------------------------------------------------------
007200100507       //?Definizione strutture dati.                                  ?
007300100507       //--------------------------------------------------------------
007400100507
007500100507       // -?Status ds?
007600100507     d Status         sds
007700100507     d  SDSpgm           *proc
007800100507     d  SDSusr               254    263
007900100507
008000100507       // -?Parametri per Reperimento dati utente?
008100100507     d TIBS34ds      e ds
008200100507
008300100510       // -?Tabella "01" = Commerciali?
008400100510     d ds01cmm       e ds                  extname(ds01)
008500100510     d                                     inz  prefix(cmm)
008600100510     d ds01uni       e ds                  extname(ds01)
008700100510     d                                     inz  prefix(uni)
008800100507
008900100510       // -?Tabella "05" = Codici Area?
009000100510     d ds05cmm       e ds                  extname(ds05)
009100100510     d                                     inz  prefix(cmm)
009200100507     d ds05uni       e ds                  extname(ds05)
009300100507     d                                     inz  prefix(uni)
009400100510
009500100510       // -?140ª descrizione dell'organigramma?
009600100510     d Og140cmm      e ds                  extname(Og140)
009700100510     d                                     inz  prefix(cmm)
009800100510     d Og140uni      e ds                  extname(Og140)
009900100510     d                                     inz  prefix(uni)
010000100507
010100100507       // -?Data e Ora correnti?
010200100507     d ds_Time         ds            20    inz
010300100507     d   wDate                        8s 0 inz
010400100507     d   wTime                        6s 0 inz
010500100507     d   wMmSs                        6s 0 inz
010600100507
010700100507       //--------------------------------------------------------------
010800100507       //?Definizione variabili globali.                               ?
010900100507       //--------------------------------------------------------------
011000100510
011100100510       // -?Nome del sistema?
011200100510     d currSysNeta     s              8a   inz
011300100510
011400100510       // -?Nome libreria del file TNVIS02L?
011500100511     d wFLib1          s             21    inz
011600100511       // -?Nome libreria del work-file WFACV00F?
011700100511     d wFLib2          s             21    inz
011800100507
011900100507       // -?Codice filiale del commerciale?
012000100510     d ORGfil_cmm      s                   like(ORGfil)  inz
012100100510       // -?Codice raggruppamento area del commerciale?
012200100510     d TBLkey_Cmm_01   s                   like(TBLkey)  inz
012300100510     d TBLkey_Cmm_05   s                   like(TBLkey)  inz
012400100507
012500100507       // -?Codice filiale dell'unificante?
012600100507     d ORGfil_uni      s                   like(ORGfil)  inz
012700100510       // -?Codice raggruppamento area dell'unificante?
012800100510     d TBLkey_Uni_01   s                   like(TBLkey)  inz
012900100510     d TBLkey_Uni_05   s                   like(TBLkey)  inz
013000100510
013100100507
013200100507       // -?Campi di comodo?
013300100507     d wDate_Eur       s               d   inz  datfmt(*eur)
013400100507
013500100507       //--------------------------------------------------------------
013600100507       //?Definizione prototipi procedure e relativi parametri.        ?
013700100507       //--------------------------------------------------------------
013800100510
013900100510       // -?Reperimento NETA sistema AS/400 corrente?
014000100510      /copy gaitrasrc/srcProtoPr,UBRTVNETA
014100100507
014200100507       // -?Reperimento dati utente?
014300100507      /copy gaitrasrc/srcProtoPR,TIBS34R
014400100510
014500100510       // -?Parametri API QCAPCMD (Process Commands)?
014600100510     d Qcmd            s           2048    inz  varying
014700100510      /copy qSysInc/qRpgleSrc,QCAPCMD
014800100510       // -?API QCAPCMD (Process Commands)?
014900100510      /copy gaitrasrc/srcProtoPR,QCAPCMD
015000100510
015100100510       // -?Parametri gestione errori API.?
015200100510      /copy qsysinc/qrpglesrc,QUSEC
015300100507
015400100507       //--------------------------------------------------------------
015500100507       //?Definizione key-list.                                        ?
015600100507       //--------------------------------------------------------------
015700100507
015800100507       // -?File TNVIS02L?
015900100507     d k04tnvis02    e ds                  extname(TNVIS02L : *key)
016000100507     d                                     inz  prefix(k_)
016100100507
016200100507       // -?File TABEL00F?
016300100507     d k03tabel00    e ds                  extname(TABEL00F : *key)
016400100507     d                                     inz  prefix(k_)
016500100507
016600100507       //--------------------------------------------------------------
016700100507       //?M A I N - L I N E                                            ?
016800100507       //--------------------------------------------------------------
016900100507
017000100507      /free
017100100507
017200100507       // -?Operazioni iniziali?
017300100507       exsr sr_RoutInz;
017400100507
017500100507       // -?Ciclo di lettura?
017600100507       clear  k04tnvis02;
017700100507       setll  %kds(k04tnvis02)  TNVIS000;
017800100507       read  TNVIS000;
017900100507       DoW  Not %eof(TNVIS02L);
018000100507         if  VISdvi < c_DataMin;
018100100507           k_VIScmm = VIScmm;
018200100507           k_VISdvi = c_DataMin;
018300100507           clear  k_VIShmv;
018400100507           clear  k_VISnrv;
018500100507           setll  %kds(k04tnvis02)  TNVIS000;
018600100507         else;
018700100507           exsr  sr_Elabora;
018800100507         endif;
018900100507         read  TNVIS000;
019000100507       EndDo;
019100100507
019200100507       // -?Operazioni finali?
019300100507       exsr sr_RoutEnd;
019400100507
019500100507       //--------------------------------------------------------------
019600100507       //?Operazioni iniziali.                                         ?
019700100507       //--------------------------------------------------------------
019800100507       BEGSR  sr_RoutInz;
019900100507
020000100507         *inLR = *on;
020100100510
020200100510         // -?Apertura file TNVIS02L?
020300100510         select;
020400100510           when  UBRTVNETA_Rtv(currSysNeta) <> *zero;
020500100510             exsr  sr_RoutEnd;
020600100510           when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
020700100511             wFLib1 = 'FILTRAGRU/TNVIS02L';
020800100511             wFLib2 = 'GAITRAAZM/WFACV00F';
020900100510           other;
021000100511             wFLib1 = 'FILTRAGRPF/TNVIS02L';
021100100511             wFLib2 = 'GAITRAAZP/WFACV00F';
021200100510         endsl;
021300100510         open  TNVIS02L ;
021400100507
021500100507         // -?Reperimento dati job?
021600100507         exsr  sr_DatiJob;
021700100507
021800100507         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
021900100507         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
022000100507         ds_Time   = %char( %dec( %timestamp() ) );
022100100507         wDate_Eur = %date( wDate : *iso );
022200100507         wDate     = %dec( wDate_Eur );
022300100507
022400100507         // -?Impostazione campi fissi?
022500100507         k_TBLkut = 1;
022600100507
022700100510         // -?Pulizia ed apertura WrkF?
022800100511         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
022900100511           Qcmd = c_CmdClrPfm1;
023000100511         else;
023100100511           Qcmd = c_CmdClrPfm2;
023200100511         endif;
023300100510         exsr  sr_ExecCmd;
023400100510         open  WFACV00F;
023500100507
023600100507       ENDSR;
023700100507
023800100507       //--------------------------------------------------------------
023900100507       //?Reperimento Dati del job (Utente/Operativi).                 ?
024000100507       //--------------------------------------------------------------
024100100507       BEGSR  sr_DatiJob;
024200100507
024300100507         in(E) §AzUte;
024400100507         if NOT %error;
024500100507           in(E) §DatiUte;
024600100507         endif;
024700100507         if %error or RSut = *blanks;
024800100507           clear TIBS34ds;
024900100507           tibs34r ( tibs34ds );
025000100507           in §AzUte;
025100100507           in §DatiUte;
025200100507         endif;
025300100507
025400100507       ENDSR;
025500100510
025600100510       //--------------------------------------------------------------
025700100510       //?Esecuzione del comando (già impostato).                      ?
025800100510       //--------------------------------------------------------------
025900100510       BEGSR  sr_ExecCmd;
026000100510
026100100510         clear Qcap0100;
026200100510         Qcabcsdh = *off;
026300100510         Qcapa    = *off;
026400100510         Qcacmdss = *off;
026500100510         Qcaerved = *allX'00';
026600100510
026700100510         clear Qusec;
026800100510         Qusbprv  = %size(Qusec);
026900100510
027000100510         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
027100100510                           %size(Qcap0100) : 'CPOP0100' : *omit :
027200100510                           0 : 0 : Qusec);
027300100510
027400100510         if  Qusei <> *blank;
027500100510           exsr  sr_PrintErr;
027600100510           exsr  sr_RoutEnd;
027700100510         endif;
027800100510
027900100510       ENDSR;
028000100510
028100100510       //--------------------------------------------------------------
028200100510       //?Stampa segnalazioni dell'errore rilevato.                    ?
028300100510       //--------------------------------------------------------------
028400100510       BEGSR  sr_PrintErr;
028500100510
028600100510         // -?Stampa Dump?
028700100510         Dump(A);
028800100510
028900100510         // -?Stampa Job-Log?
029000100510         Qcmd = 'DSPJOBLOG job(*) output(*print)';
029100100510         clear Qcap0100;
029200100510         Qcabcsdh = *off;
029300100510         Qcapa    = *off;
029400100510         Qcacmdss = *off;
029500100510         Qcaerved = *allX'00';
029600100510         clear Qusec;
029700100510         Qusbprv  = %size(Qusec);
029800100510         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
029900100510                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
030000100510                           0 : 0 : Qusec);
030100100510
030200100510       ENDSR;
030300100507
030400100507       //--------------------------------------------------------------
030500100507       //?Elaborazione singolo record del file TNVIS00F.               ?
030600100507       //--------------------------------------------------------------
030700100507       BEGSR  sr_Elabora;
030800100510
030900100510         // -?ESCLUSIONE visite fittizie?
031000100510         if  VISffz <> *blank;
031100100510           leavesr;
031200100510         endif;
031300100507
031400100507         // -?Reperimento dati commerciale?
031500100510         if  TBLkey_Cmm_01 <> %editc( VIScmm : 'X' );
031600100510           TBLkey_Cmm_01 = %editc( VIScmm : 'X' );
031700100510           clear  ds01cmm;
031800100507           k_TBLcod = '01';
031900100510           k_TBLkey = TBLkey_Cmm_01;
032000100507           chain  %kds(k03tabel00)  TABEL;
032100100507           if  %found(TABEL00F)   and   TBLflg = *blank;
032200100510             ds01cmm = TBLuni;
032300100507           endif;
032400100507         endif;
032500100507
032600100510         // -?ESCLUSIONE filiale CMM = filiale RGF (unificante)?
032700100510         if  %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) =
032800100510             %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 );
032900100510           leavesr;
033000100507         endif;
033100100507
033200100510         // -?Reperimento raggruppamento area commerciale?
033300100510         IF  ORGfil_cmm <>
033400100507                  %int( %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) );
033500100510
033600100510           ORGfil_cmm =
033700100507                  %int( %subst( %editc( VIScmm : 'X' ) : 1 : 3 ) );
033800100510           clear  Og140cmm;
033900100510           chain  (ORGfil_cmm)  AZORG;
034000100510
034100100510           if  %found(AZORG01L);
034200100510             Og140cmm = ORGde0;
034300100510           endif;
034400100510
034500100507         ENDIF;
034600100507
034700100510         // -?Reperimento raggruppamento area unificante?
034800100507         IF  ORGfil_uni <>
034900100510                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
035000100510
035100100507           ORGfil_uni =
035200100510                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
035300100510           clear  Og140uni;
035400100507           chain  (ORGfil_uni)  AZORG;
035500100510
035600100510           if  %found(AZORG01L);
035700100510             Og140uni = ORGde0;
035800100510           endif;
035900100510
036000100507         ENDIF;
036100100507
036200100510         // -?Elabora solo se raggruppamenti aree diversi?
036300100510         if  cmm§OGara = uni§OGara;
036400100507           leavesr;
036500100507         endif;
036600100510
036700100510         // -?Decodifica unificante?
036800100510         If  TBLkey_Uni_01 <> %editc( cmm§01rgf : 'X' );
036900100510           TBLkey_Uni_01 = %editc( cmm§01rgf : 'X' );
037000100510           clear  ds01uni;
037100100510           k_TBLcod = '01';
037200100510           k_TBLkey = TBLkey_Uni_01;
037300100510           chain  %kds(k03tabel00)  TABEL;
037400100510           if  %found(TABEL00F)   and   TBLflg = *blank;
037500100510             ds01uni = TBLuni;
037600100510           endif;
037700100510         endif;
037800100510
037900100510         // -?Decodifica raggruppamento area commerciale?
038000100510         If  TBLkey_Cmm_05 <> cmm§OGara;
038100100510           TBLkey_Cmm_05 = cmm§OGara;
038200100510           clear  ds05cmm;
038300100510           k_TBLcod = '05';
038400100510           k_TBLkey = TBLkey_Cmm_05;
038500100510           chain  %kds(k03tabel00)  TABEL;
038600100510           if  %found(TABEL00F)   and   TBLflg = *blank;
038700100510             ds05cmm = TBLuni;
038800100510           endif;
038900100510         EndIf;
039000100510
039100100510         // -?Decodifica raggruppamento area unificante?
039200100510         If  TBLkey_Uni_05 <> uni§OGara;
039300100510           TBLkey_Uni_05 = uni§OGara;
039400100510           clear  ds05uni;
039500100510           k_TBLcod = '05';
039600100510           k_TBLkey = TBLkey_Uni_05;
039700100510           chain  %kds(k03tabel00)  TABEL;
039800100510           if  %found(TABEL00F)   and   TBLflg = *blank;
039900100510             ds05uni = TBLuni;
040000100510           endif;
040100100510         EndIf;
040200100507
040300100510         // -?Scrittura record nel Work-File?
040400100510         clear  WFACV000;
040500100510         WACute  = SDSusr;
040600100510         WACdta  = *date;
040700100510         WACnrv  = VISnrv;
040800100510         WACdvi  = VISdvi;
040900100510         WACcmm  = VIScmm;
041000100510         WACcmmD = cmm§01age;
041100100510         if  cmm§OGara > *blank;
041200100510           WACara  = %int( cmm§OGara );
041300100510         endif;
041400100510         WACaraD = cmm§05des;
041500100510         WACcmu  = cmm§01rgf;
041600100510         WACcmuD = uni§01age;
041700100510         if  uni§OGara > *blank;
041800100510           WACaru  = %int( uni§OGara );
041900100510         endif;
042000100510         WACaruD = uni§05des;
042100100510
042200100510         //______________
042300100510         write  WFACV000;
042400100510         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
042500100507
042600100507       ENDSR;
042700100507
042800100507       //--------------------------------------------------------------
042900100507       //?Operazioni finali.                                           ?
043000100507       //--------------------------------------------------------------
043100100507       BEGSR  sr_RoutEnd;
043200100507
043300100510         // -?Chiusura workfile WFACV00F?
043400100510         if  %open(WFACV00F);
043500100510           close  WFACV00F;
043600100507         endif;
043700100507
043800100510         // -?Uscita dal *pgm?
043900100507         return;
044000100507
044100100507       ENDSR;
044200100507
044300100507      /end-free
