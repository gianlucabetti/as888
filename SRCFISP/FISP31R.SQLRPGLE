000100130109       //==============================================================
000200130109       //?Inserimento "Nominativo FIRMA" su FIAR4/TITA4 (da FIPLG).    ?
000300130109       //==============================================================
000400130109
000500130109       //--------------------------------------------------------------
000600130109       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130109       //--------------------------------------------------------------
000800130109
000900130109     /*PRM  dbgview(*source)
001000130109     /*PRM  commit(*none)
001100130109     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001200130109     /*CMD         ovrscope(*calllvl)
001300130109     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
001400130109     /*CMD         ovrscope(*calllvl)
001500130109     /*CMD  ovrdbf file(FIAR401L) tofile(FILTRAPRD/FIAR401L) +
001600130109     /*CMD         ovrscope(*calllvl)
001700130109     /*END  dltovr file(TITAS30C TITA430C FIAR401L) lvl(*)
001800130109     /*END
001900130109
002000130109       //--------------------------------------------------------------
002100130109       //?Specifiche di controllo.                                     ?
002200130109       //--------------------------------------------------------------
002300130109
002400130109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500130109     h dftactgrp(*no)
002600130109     h alwnull(*inputonly)
002700130109
002800130109       //--------------------------------------------------------------
002900130109       //?Dichiarazione file.                                          ?
003000130109       //--------------------------------------------------------------
003100130109
003200130109       // -?Distinte delle cui LdV ri-generare le immagini?
003300130109     fFIIRD01L  if   e           k disk
003400130109
003500130109       // -?Dati per LdV (sede)?
003600130109     fTITAS30C  if   e           k disk    usropn
003700130109     f                                     extfile(wLibFileTITAS)
003800130109
003900130109       // -?Estensione descrizioni?
004000130109     fFIAR401L  if A e           k disk    usropn
004100130109     f                                     extfile(wLibFileFIAR4)
004200130109
004300130109       // -?Riferimenti bolla (sede)?
004400130109     fTITA430C  if A e           k disk    usropn
004500130109     f                                     extfile(wLibFileTITA4)
004600130109
004700130109       //--------------------------------------------------------------
004800130109       //?Definizione costanti.                                        ?
004900130109       //--------------------------------------------------------------
005000130109
005100130109
005200130109       //--------------------------------------------------------------
005300130109       //?Definizione schiere.                                         ?
005400130109       //--------------------------------------------------------------
005500130109
005600130109
005700130109       //--------------------------------------------------------------
005800130109       //?Definizione aree dati.                                       ?
005900130109       //--------------------------------------------------------------
006000130109
006100130109       // -?Dati utente?
006200130109     d §AzUte        e ds                  extname(AZUTE00F)
006300130109     d                                     dtaara
006400130109     d §DatiUte      e ds                  extname(dDatiUte)
006500130109     d                                     dtaara
006600130109
006700130109       //--------------------------------------------------------------
006800130109       //?Definizione strutture dati.                                  ?
006900130109       //--------------------------------------------------------------
007000130109
007100130109       // -?Parametri ricevuti?
007200130109     d KPJBA         e ds
007300130110
007400130110       // -?Flag operativi in TITAS30C?
007500130110     d dTASflo       e ds                  inz     qualified
007600130109
007700130109       // -?Dati estratti via SQL?
007800130109     d wSQL_ds         ds                  inz  qualified
007900130109     d   wFGS                         3s 0 inz
008000130109     d   wNDC                         6s 0 inz
008100130109     d   wSPED                       16    inz
008200130109     d   wNOME                       35    inz
008300130109
008400130109       // -?Parametro Numero Spedizione?
008500130109     d ds_Spedizione   ds            16    inz  qualified
008600130109     d   wAAS                         4s 0 inz
008700130109     d   wLNP                         3s 0 inz
008800130109     d   wNRS                         2s 0 inz
008900130109     d   wNSP                         7s 0 inz
009000130109
009100130109       //--------------------------------------------------------------
009200130109       //?Definizione variabili globali.                               ?
009300130109       //--------------------------------------------------------------
009400130109
009500130109       // -?Flags booleani?
009600130109     d $Fine           s               n   inz
009700130109
009800130109       // -?Nome esteso Libreria/File del file TITAS30C?
009900130109     d wLibFileTITAS   s             21a   inz
010000130109       // -?Nome esteso Libreria/File del file TITA430C?
010100130109     d wLibFileTITA4   s             21a   inz
010200130109       // -?Nome esteso Libreria/File del file FIAR401L?
010300130109     d wLibFileFIAR4   s             21a   inz
010400130109
010500130109       // -?Stringa SQL da eseguire?
010600130109     d wSQL            s           2048    inz  Varying
010700130109
010800130109       //--------------------------------------------------------------
010900130109       //?Definizione prototipi procedure.                             ?
011000130109       //--------------------------------------------------------------
011100130109
011200130109       // -?Reperimento dati utente?
011300130109     d TIBS34ds      e ds                  inz
011400130109      /copy gaitrasrc/srcProtoPR,TIBS34R
011500130109
011600130109       // -?Parametri API QCAPCMD (Process Commands)?
011700130109     d Qcmd            s            512    inz  varying
011800130109      /copy qSysInc/qRpgleSrc,QCAPCMD
011900130109       // -?API QCAPCMD (Process Commands)?
012000130109      /copy gaitrasrc/srcProtoPR,QCAPCMD
012100130109
012200130109       // -?Parametri gestione errori API.?
012300130109      /copy qsysinc/qrpglesrc,QUSEC
012400130109
012500130109       //--------------------------------------------------------------
012600130109       //?Definizione key-list.                                        ?
012700130109       //--------------------------------------------------------------
012800130109
012900130109       // -?File TITAS30C?
013000130109     d k05titas30    e ds                  extname(TITAS30C : *key)
013100130109     d                                     prefix(k_)
013200130109     d                                     inz
013300130109
013400130109       // -?File TITA430C?
013500130109     d k05tita430    e ds                  extname(TITA430C : *key)
013600130109     d                                     prefix(k_)
013700130109     d                                     inz
013800130109
013900130109       // -?File FIAR401L?
014000130109     d k05fiar401    e ds                  extname(FIAR401L : *key)
014100130109     d                                     prefix(k_)
014200130109     d                                     inz
014300130109
014400130109       //--------------------------------------------------------------
014500130109       //?M A I N - L I N E                                            ?
014600130109       //--------------------------------------------------------------
014700130109
014800130109     c     *Entry        plist
014900130109     c                   parm                    KPJBA
015000130109
015100130109      /free
015200130109
015300130109       // -?Operazioni iniziali?
015400130109       exsr sr_RoutInz;
015500130109
015600130109       // -?Ciclo di lettura spedizioni per distinta da FIIRD/FIPLG?
015700130109       DoU  $Fine;
015800130109         exsr  sr_ExecSql;
015900130109       EndDo;
016000130109
016100130109       // -?Operazioni finali?
016200130109       exsr sr_RoutEnd;
016300130109
016400130109       //--------------------------------------------------------------
016500130109       //?Operazioni iniziali.                                         ?
016600130109       //--------------------------------------------------------------
016700130109       BEGSR  sr_RoutInz;
016800130109
016900130109         *inLR = *on;
017000130109
017100130109         // -?Impostazione opzioni per SQL?
017200130109         exec sql  set  option  DynUsrPrf = *Owner,
017300130109                                CloSqlCsr = *EndMod;
017400130109
017500130109         // -?Reperimento dati job?
017600130109         exsr  sr_DatiJob;
017700130109
017800130109         // -?Apertura archivi in librerie specifiche?
017900130109         if  %subst( knsif : 7 : 1 ) = 'P';
018000130109           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
018100130109           wLibFileTITA4 = 'GAITRAGRPS/TITA430C';
018200130109           wLibFileFIAR4 = 'FILTRAPRD/FIAR401L';
018300130109         else;
018400130109           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
018500130109           wLibFileTITA4 = 'GAITRAGRU/TITA430C';
018600130109           wLibFileFIAR4 = 'FILTRA201/FIAR401L';
018700130109         endif;
018800130109
018900130109         open(e)  TITAS30C;
019000130109         if  %error;
019100130109           exsr  sr_PrintErr;
019200130109         endif;
019300130109
019400130109         open(e)  TITA430C;
019500130109         if  %error;
019600130109           exsr  sr_PrintErr;
019700130109         endif;
019800130109
019900130109         open(e)  FIAR401L;
020000130109         if  %error;
020100130109           exsr  sr_PrintErr;
020200130109         endif;
020300130109
020400130109         // -?Preparazione stringa SQL?
020500130109         wSQL = 'with FIPLG as (select plgFGS, plgNDC, plgIdDoc, +
020600130109                                       substr(plgDATI, 163, 35) +
020700130109                                  from FIPLG00F +
020800130109                                 where plgTIPORCD = ''CET'' +
020900130109                                   and substr(plgDATI, 163, 35) <> '' '') +
021000130109                 select FIPLG.* +
021100130109                   from FIPLG inner join FIIRD00F +
021200130109                     on digits(plgFGS) concat digits(plgNDC) = irdFGSNDC +
021300130109                    for fetch only';
021400130109
021500130109         // -?Dichiarazione del cursore?
021600130109         exec sql   prepare S1   from :wSQL;
021700130109         exec sql   declare C1   cursor for S1;
021800130109
021900130109         if  SQLcode < *zero;
022000130109           exsr  sr_PrintErr;
022100130109           //$Fine = *on;
022200130109           //leavesr;
022300130109         endif;
022400130109
022500130109         // -?Apertura del cursore?
022600130109         exec sql   open C1;
022700130109
022800130109         if  SQLcode < *zero;
022900130109           exsr  sr_PrintErr;
023000130109           //$Fine = *on;
023100130109           //leavesr;
023200130109         endif;
023300130109
023400130109       ENDSR;
023500130109
023600130109       //--------------------------------------------------------------
023700130109       //?Reperimento Dati del job (Utente/Operativi).                 ?
023800130109       //--------------------------------------------------------------
023900130109       BEGSR  sr_DatiJob;
024000130109
024100130109         in(e) §AzUte;
024200130109         if NOT %error;
024300130109           in(e) §DatiUte;
024400130109         endif;
024500130109         if %error or RSut = *blank;
024600130109           tibs34r ( tibs34ds );
024700130109           in §AzUte;
024800130109           in §DatiUte;
024900130109         endif;
025000130109
025100130109       ENDSR;
025200130109
025300130109       //--------------------------------------------------------------
025400130109       //?Lettura del cursore.                                         ?
025500130109       //--------------------------------------------------------------
025600130109       BEGSR  sr_ExecSQL;
025700130109
025800130109         clear  wSQL_ds;
025900130109         exec sql   fetch next   from C1   into :wSQL_ds;
026000130109
026100130109         select;
026200130109           when  SQLcode = 100;
026300130109             $Fine = *on;
026400130109           when  SQLcode < *zero;
026500130109             exsr  sr_PrintErr;
026600130109             //$Fine = *on;
026700130109             //leavesr;
026800130109           other;
026900130109             ds_Spedizione = wSQL_ds.wSPED;
027000130110             clear k05titas30;
027100130110             k_TASaas = ds_Spedizione.wAAS;
027200130110             k_TASlnp = ds_Spedizione.wLNP;
027300130110             k_TASnrs = ds_Spedizione.wNRS;
027400130110             k_TASnsp = ds_Spedizione.wNSP;
027500130110             chain  %kds(k05titas30 : 4)  TITAS30C;
027600130110             If  %found(TITAS30C);
027700130110               dTASflo = TASflo;
027800130110               if  dTASflo.§FLOiml = *blank;
027900130110                 exsr  sr_WrtFIAR4;
028000130110                 exsr  sr_WrtTITA4;
028100130110               endif;
028200130110             EndIf;
028300130109         endsl;
028400130109
028500130109       ENDSR;
028600130109
028700130109       //--------------------------------------------------------------
028800130109       //?Scrittura "Nominativo FIRMA LDV" in FIAR4.                   ?
028900130109       //--------------------------------------------------------------
029000130109       BEGSR  sr_WrtFIAR4;
029100130109
029200130109         clear  k05fiar401;
029300130109         k_AR4aas = ds_Spedizione.wAAS;
029400130109         k_AR4lnp = ds_Spedizione.wLNP;
029500130109         k_AR4nrs = ds_Spedizione.wNRS;
029600130109         k_AR4nsp = ds_Spedizione.wNSP;
029700130109         k_AR4trc = '1';
029800130109
029900130109         chain  %kds(k05fiar401)  FIAR4000;
030000130109
030100130109         If  NOT %found(FIAR401L);
030200130109
030300130109           clear  FIAR4000;
030400130109           AR4aas = k_AR4aas;
030500130109           AR4lnp = k_AR4lnp;
030600130109           AR4nrs = k_AR4nrs;
030700130109           AR4nsp = k_AR4nsp;
030800130109           AR4trc = k_AR4trc;
030900130109           AR4not = wSQL_ds.wNOME;
031000130109           AR4dtr = 20121231;
031100130109
031200130109           //______________
031300130109           write  FIAR4000;
031400130109           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
031500130109
031600130109         EndIf;
031700130109
031800130109       ENDSR;
031900130109
032000130109       //--------------------------------------------------------------
032100130109       //?Scrittura "Nominativo FIRMA LDV" in TITA4.                   ?
032200130109       //--------------------------------------------------------------
032300130109       BEGSR  sr_WrtTITA4;
032400130109
032500130109         clear  k05tita430;
032600130109         k_TA4aas = ds_Spedizione.wAAS;
032700130109         k_TA4lnp = ds_Spedizione.wLNP;
032800130109         k_TA4nrs = ds_Spedizione.wNRS;
032900130109         k_TA4nsp = ds_Spedizione.wNSP;
033000130109         k_TA4trc = '1';
033100130109
033200130109         chain  %kds(k05tita430)  TITA430C;
033300130109
033400130109         If  NOT %found(TITA430C);
033500130109
033600130109           clear  TITA4000;
033700130109           TA4aas = k_TA4aas;
033800130109           TA4lnp = k_TA4lnp;
033900130109           TA4nrs = k_TA4nrs;
034000130109           TA4nsp = k_TA4nsp;
034100130109           TA4trc = k_TA4trc;
034200130109           TA4not = wSQL_ds.wNOME;
034300130109
034400130109           //______________
034500130109           write  TITA4000;
034600130109           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
034700130109
034800130109         EndIf;
034900130109
035000130109       ENDSR;
035100130109
035200130109       //--------------------------------------------------------------
035300130109       //?Stampa segnalazione dell'errore rilevato.                    ?
035400130109       //--------------------------------------------------------------
035500130109       BEGSR  sr_PrintErr;
035600130109
035700130109         // -?Stampa del Dump?
035800130109         Dump(A);
035900130109
036000130109         // -?Stampa del Job-Log?
036100130109         Qcmd = 'DSPJOBLOG job(*) output(*print)';
036200130109         exsr  sr_ExecCmd;
036300130109
036400130109         // -?Chiusura del programma?
036500130109         exsr  sr_RoutEnd;
036600130109
036700130109       ENDSR;
036800130109
036900130109       //--------------------------------------------------------------
037000130109       //?Esecuzione del comando (già impostato).                      ?
037100130109       //--------------------------------------------------------------
037200130109       BEGSR  sr_ExecCmd;
037300130109
037400130109         clear Qcap0100;
037500130109         Qcabcsdh = *off;
037600130109         Qcapa    = *off;
037700130109         Qcacmdss = *off;
037800130109         Qcaerved = *allX'00';
037900130109
038000130109         clear Qusec;
038100130109         Qusbprv  = %size(Qusec);
038200130109
038300130109         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
038400130109                           %size(Qcap0100) : 'CPOP0100' : *omit :
038500130109                           0 : 0 : Qusec);
038600130109
038700130109         //if  Qusei <> *blank;
038800130109         //  ...;
038900130109         //endif;
039000130109
039100130109       ENDSR;
039200130109
039300130109       //--------------------------------------------------------------
039400130109       //?Operazioni finali.                                           ?
039500130109       //--------------------------------------------------------------
039600130109       BEGSR  sr_RoutEnd;
039700130109
039800130109         // -?Chiusura del cursore?
039900130109         exec sql   close C1;
040000130109
040100130109         // -?Uscita?
040200130109         return;
040300130109
040400130109       ENDSR;
