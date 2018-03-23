000100140821       //==============================================================
000200140821       //?Acquisizione Potenziali: lettura del file WFMDP00F per       ?
000300140821       //?                 SCRITTURA  dati nel file TNCPO00F.          ?
000400141002       //?-------------------------------------------------------------?
000500141002       //?SE lanciato per la verifica: stampa, ma NON inserisce.       ?
000600141002       //?SE lanciato per l'inserimento: NON stampa, ma inserisce.     ?
000700140821       //==============================================================
000800140821
000900140821       //--------------------------------------------------------------
001000140821       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001100140821       //--------------------------------------------------------------
001200140821
001300140821     /*PRM  dbgview(*source)
001400140821     /*CMD  ovrdbf file(WFDEMR1L) tofile(GAITRAAZP/WFDEMR1L) +
001500140821     /*CMD         ovrscope(*calllvl)
001600140821     /*END  dltovr file(WFDEMR1L) lvl(*)
001700140821     /*END
001800140821
001900140821       //--------------------------------------------------------------
002000140821       //?Specifiche di controllo.                                     ?
002100140821       //--------------------------------------------------------------
002200140821
002300140821     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002400140821     h dftactgrp(*no)
002500140821
002600140821       //--------------------------------------------------------------
002700140821       //?Dichiarazione file.                                          ?
002800140821       //--------------------------------------------------------------
002900140905
003000140905       // -?Organigramma?
003100140905     fAZORG01L  if   e           k disk
003200140821
003300140821       // -?WrkF Acquisizione Potenziali: main dati potenziali CRIF?
003400140821     fWFMDP00F  if   e             disk
003500140821     f                                     extfile(wLibFile1)
003600140821     f                                     usropn
003700140821       // -?WrkF Classificazione attività economiche ATECO 2007?
003800140821     fWATECO1L  if   e           k disk
003900140821     f                                     extfile(wLibFile2)
004000140821     f                                     usropn
004100140821       // -?WrkF per Mappatura Commerciale -?
004200140821       //  ?% di spesa Presunta trasporti sul fatturato?
004300140821     fWFDEMR1L  if   e           k disk
004400140821     f                                     extfile(wLibFile3)
004500140821     f                                     usropn
004600140821
004700140911       // -?Anagrafica Clienti Potenziali (ed estensione)?
004800140902     fTNCPO05L  if   e           k disk
004900140902     f                                     rename(TNCPO000 : TNCPO005)
005000140902     f                                     prefix(x)
005100140822     fTNCPO06L  if   e           k disk
005200140822     f                                     rename(TNCPO000 : TNCPO006)
005300140902     f                                     prefix(y)
005400140911     fTNCPO12L  if   e           k disk
005500140911     f                                     rename(TNCPO100 : TNCPO120)
005600140911     f                                     prefix(y)
005700140822
005800140821     fTNCPO01L  if A e           k disk
005900140821     fTNCPO11L  if A e           k disk
006000140905
006100140905       // -?Informazioni commerciali clienti Potenziali?
006200141002     f*//TICPI01L  if A e           k disk
006300141002     fTICPI00F  o    e             disk
006400140821
006500140821       // -?Stampa segnalazioni di errore?
006600140821     fQSYSPRT   o    f  132        printer
006700140821     f                                     usropn
006800140821     f                                     oflind(*inOF)
006900140821
007000141002       // -?Stampa dettaglio potenziali elaborati?
007100141002     fQSYSPRT1  o    f  132        printer
007200141002     f                                     usropn
007300141002     f                                     oflind(*inOA)
007400140821
007500140821       //--------------------------------------------------------------
007600140821       //?Definizione costanti.                                        ?
007700140821       //--------------------------------------------------------------
007800140821
007900140821       // -?Parte "iniziale" del nuovo Codice Potenziale?
008000140821     d c_CPOini        c                   const(04600000000)
008100140821
008200140821       // -?Limite fatturato per "piccole imprese"?
008300140821     d c_PiccoleAL     c                   const(10000000)
008400140821       // -?Limite fatturato per "medie imprese"?
008500140821     d c_MedieAL       c                   const(50000000)
008600140901
008700140901       // -?C.A.P. di San Marino (parziale: prime 4 cifre)?
008800140901     d c_CAP_SM_parz4  c                   const('4789')
008900140904
009000140904       // -?Data Fissa Aggiornamento: 15/08/2014?
009100140904     d c_Data          c                   const(20140815)
009200140821
009300140821       // -?Costanti in stampa (forse)?
009400140821     d c_SpesaTra      c                   const('Spesa Trasp. Presunta:')
009500140821
009600140821       //--------------------------------------------------------------
009700140821       //?Definizione schiere.                                         ?
009800140821       //--------------------------------------------------------------
009900140821
010000140821
010100140821       //--------------------------------------------------------------
010200140821       //?Definizione aree dati.                                       ?
010300140821       //--------------------------------------------------------------
010400140821
010500140821       // -?Dati utente?
010600140821     d §AzUte        e ds                  extname(AZUTE00F)
010700140821     d                                     dtaara
010800140821     d §DatiUte      e ds                  extname(dDatiUte)
010900140821     d                                     dtaara
011000140821
011100140821       //--------------------------------------------------------------
011200140821       //?Definizione strutture dati.                                  ?
011300140821       //--------------------------------------------------------------
011400140821
011500140821       // -?Status ds?
011600140821     d Status         sds
011700140821     d   SDSpgmName      *proc
011800140821     d*//SDSparms        *parms
011900140821     d   SDSjobName          244    253                                         Job name
012000140821     d   SDSjobUser          254    263                                         User name
012100140821     d*//SDSjobNumber        264    269s 0                                      Job number
012200140821
012300140821       // -?Parametri ricevuti?
012400140821     d KPJBA         e ds
012500140821
012600140821       // -?DS per campo CPORST di TNCPO00F
012700140821     d dCPO01        e ds                  inz  qualified
012800140910       // -?DS per campo CPOTEL di TNCPO00F
012900140910     d dCPO02        e ds                  inz  qualified
013000140821
013100140821       //--------------------------------------------------------------
013200140821       //?Definizione variabili globali.                               ?
013300140821       //--------------------------------------------------------------
013400140821
013500140821       // -?Nome esteso Libreria/File del file da elaborare?
013600140821     d wLibFile1       s             21a   inz
013700140821     d wLibFile2       s             21a   inz
013800140821     d wLibFile3       s             21a   inz
013900140821
014000140821       // -?Flags Booleani?
014100140821     d $WrtTNCPO       s               n   inz
014200140821
014300140821       // -?Campi per il calcolo del nuovo cod. Potenziale?
014400140821     d wNEWcpo8        s              8  0 inz(02999999)
014500140821
014600140821       // -?Campi di comodo?
014700140821     d wDate_Iso       s               d   inz  datfmt(*iso)
014800140821     d wDate_Eur       s               d   inz  datfmt(*eur)
014900140821     d wDate           s              8  0 inz
015000140821     d wTime           s              6  0 inz
015100140821     d wErr            s              3  0 inz
015200140821     d w255a           s            255    inz
015300140821
015400140821       // -?Campi per la subr. sr_CalcoloSpesaPresunta?
015500140821     d wDEMfil         s              3  0 inz
015600140821     d wDEMare         s              3  0 inz
015700140821     d wDEMtotFat      s             15  0 inz
015800140821     d wPerc           s              5  2 inz
015900140821     d wSpesaTra       s             11  0 inz
016000140821     d w0080           s              8  0 inz
016100140821     d wAlfaSCT        s              5    inz
016200140821
016300140821       // -?Campi di stampa?
016400140821     d wAggior         s             10    inz('INSERIBILI')
016500141002     d s_SpesaTra      s             40    inz
016600141002     d s_Testo         s            115    inz
016700141002     d s_Testo2        s             92    inz
016800141002     d s_CatMercERR    s             26    inz
016900141002     d s_CpLoPrERR     s              1    inz
017000141002     d s_Err           s             10    inz
017100141002     d wSaveWMPateco   s                   like(WMPateco)  inz(*hival)
017200140821
017300140821       //--------------------------------------------------------------
017400140821       //?Definizione prototipi procedure.                             ?
017500140821       //--------------------------------------------------------------
017600140821
017700140821       // -?Reperimento dati utente?
017800140821     d TIBS34ds      e ds                  inz
017900140821      /copy gaitrasrc/srcProtoPR,TIBS34R
018000140821
018100140821       // -?Controllo CAP ("Pretrullo TISI95R")?
018200140821     d FNLV13ds      e ds                  inz
018300140821     d TISI95ds      e ds                  inz
018400140821     d fnlv13r         pr                  extpgm('FNLV13R')
018500140821     d   kpjba                             likeds(KPJBA)
018600140821     d   fnlv13ds                          likeds(FNLV13ds)
018700140821     d   tisi95ds                          likeds(TISI95ds)
018800140904
018900140904       // -?Calcolo codice importanza cliente potenziale?
019000140904     d TRMK52ds      e ds                  inz
019100140904      /copy gaitrasrc/srcProtoPR,TRMK52R
019200140821
019300140821       // -?API QCAPCMD (Process Commands)?
019400140821     d Qcmd            s           2048    inz  varying
019500140821      /copy qSysInc/qRpgleSrc,QCAPCMD
019600140821      /copy gaitrasrc/srcProtoPR,QCAPCMD
019700140821
019800140821       // -?Parametri gestione errori API.?
019900140821      /copy qsysinc/qrpglesrc,QUSEC
020000140821
020100140821       //--------------------------------------------------------------
020200140821       //?Definizione key-list.                                        ?
020300140821       //--------------------------------------------------------------
020400140821
020500140821       // -?File TNCPO01L?
020600140821     d k01tncpo01    e ds                  extname(TNCPO01L : *key)
020700140821     d                                     prefix(k_)   inz
020800140905
020900141002      *// // -?File TICPI01L?
021000141002     d*// keyTICPI01    e ds                  extname(TICPI01L : *key)
021100141002     d*//                                     prefix(k_)   inz
021200140911
021300140911       // -?File TICPI12L?
021400140911     d keyTNCPO12    e ds                  extname(TNCPO12L : *key)
021500140911     d                                     prefix(k_)   inz
021600140821
021700140821       //--------------------------------------------------------------
021800140821       //?Riepilogo indicatori utilizzati.                             ?
021900140821       //--------------------------------------------------------------
022000140821
022100140821
022200140821       //--------------------------------------------------------------
022300140821       //?M A I N - L I N E                                            ?
022400140821       //--------------------------------------------------------------
022500140821
022600140821     c     *Entry        plist
022700140821     c                   parm                    KPJBA
022800140821
022900140821      /free
023000140821
023100140821       // -?Operazioni iniziali?
023200140821       exsr sr_RoutInz;
023300140821
023400140821       // -?Ciclo di lettura file WFMDP00F?
023500140821       read  WFMDP000;
023600140821
023700140821       DoW  Not %eof(WFMDP00F);
023800140821
023900140821         exsr  sr_ElabPot;
024000140821
024100140821         read  WFMDP000;
024200140821
024300140821       EndDo;
024400140821
024500140821       // -?Operazioni finali?
024600140821       exsr sr_RoutEnd;
024700140821
024800140821       //--------------------------------------------------------------
024900140821       //?Operazioni iniziali.                                         ?
025000140821       //--------------------------------------------------------------
025100140821
025200140821       BEGSR  sr_RoutInz;
025300140821
025400140821         // -?Impostazione chiusura?
025500140821         *inLR = *on;
025600140821
025700140821         // -?Ricezione parametri?
025800140821         $WrtTNCPO  = ( %subst( KPJBU : 1 : 3 ) = 'A  ' );
025900140821         if  $WrtTNCPO;
026000140821           wAggior = 'INSERITI  ';
026100140821         endif;
026200140821
026300140821         // -?Reperimento data odierna in formato aaaa/mm/gg?
026400140821         wDate_Iso = %date();
026500140821         // -?Conversione data odierna in formato gg/mm/aaaa?
026600140821         wDate_Eur = wDate_Iso;
026700140821         wDate = %dec( wDate_Eur );
026800140821
026900140821         // -?Reperimento orario (hh:mm:ss)?
027000140821         wTime = %dec( %time() );
027100140821
027200140821         // -?Reperimento dati job?
027300140821         exsr  sr_DatiJob;
027400140821
027500140821         // -?Impostazione librerie dei work-file di input:?
027600140821         //  ?1) WFMDP00F?
027700140821         //  ?2) WATECO1L?
027800140821         //  ?3) WFDEMR1L?
027900140821         if  %subst( knsif : 7 : 1 ) = 'P';
028000140821           wLibFile1 = '*LIBL';
028100140821           wLibFile3 = 'GAITRAAZP';
028200140821         else;
028300140821           wLibFile1 = 'EDPCRIBIS';
028400140821           wLibFile3 = 'GAITRAAZM';
028500140821         endif;
028600140821         wLibFile2 = wLibFile1;
028700140821
028800140821         // -?Apertura del work-file di input WFMDP00F?
028900140821         wLibFile1 = %trimR( wLibFile1 ) + '/WFMDP00F';
029000140821         open(e)  WFMDP00F;
029100140821         if  %error();
029200140821           wErr = 1;
029300140821           exsr  sr_PrintErr;
029400140821           exsr  sr_RoutEnd;
029500140821         endif;
029600140821
029700140821         // -?Apertura del work-file di input WATECO1L?
029800140821         wLibFile2 = %trimR( wLibFile2 ) + '/WATECO1L';
029900140821         open(e)  WATECO1L;
030000140821         if  %error();
030100140821           wErr = 2;
030200140821           exsr  sr_PrintErr;
030300140821           exsr  sr_RoutEnd;
030400140821         endif;
030500140821
030600140821         // -?Apertura del work-file di input WFDEMR1L?
030700140821         wLibFile3 = %trimR( wLibFile3 ) + '/WFDEMR1L';
030800140821         open(e)  WFDEMR1L;
030900140821         if  %error();
031000140821           wErr = 3;
031100140821           exsr  sr_PrintErr;
031200140821           exsr  sr_RoutEnd;
031300140821         endif;
031400140821
031500140821       ENDSR;
031600140821
031700140821       //--------------------------------------------------------------
031800140821       //?Reperimento Dati del job (Utente/Operativi).                 ?
031900140821       //--------------------------------------------------------------
032000140821       BEGSR  sr_DatiJob;
032100140821
032200140821         in(e) §AzUte;
032300140821         if NOT %error;
032400140821           in(e) §DatiUte;
032500140821         endif;
032600140821         if %error or RSut = *blank;
032700140821           tibs34r ( tibs34ds );
032800140821           in §AzUte;
032900140821           in §DatiUte;
033000140821         endif;
033100140821
033200140821       ENDSR;
033300140821
033400140821       //--------------------------------------------------------------
033500140821       //?Elaborazione singolo cliente Potenziale.                     ?
033600140821       //--------------------------------------------------------------
033700140821       BEGSR  sr_ElabPot;
033800140909
033900140911         // -?Verifica SE cliente da aggiungere?
034000140909         clear  wErr;
034100140911         exsr  sr_Verifica;
034200140911         if  wErr > *zero;
034300140911           leavesr;
034400140911         endif;
034500140821
034600140821         // -?Reperimento nuovo codice per Potenziale da aggiungere?
034700140821         exsr  sr_RicercaNuovoCodice;
034800140821
034900140821         // -?Valorizzazione campi in TNCPO (00F/10F)?
035000140821         //  ?(comodo comunque per la stampa)?
035100140821         exsr  sr_ImpostaTNCPO;
035200140821
035300141002         // -?Stampa (se NON richiesto inserimento)?
035400141002         if  Not  $WrtTNCPO;
035500141002           exsr  sr_PrintCliPot;
035600141002         endif;
035700140821
035800140821         // -?Scrittura TNCPO (se richiesto)?
035900140821         if  $WrtTNCPO;
036000140821           exsr  sr_WriteTNCPO;
036100140821         endif;
036200140821
036300140821       ENDSR;
036400140911
036500140911       //--------------------------------------------------------------
036600140911       //?Verifica SE cliente da aggiungere.                           ?
036700140911       //--------------------------------------------------------------
036800140911       BEGSR  sr_Verifica;
036900140911
037000140911         // -?Verifica esistenza di un altro cliente con stesso?
037100140911         //  ?Crif-Number: SE esiste => da scartare?
037200140911         clear  KeyTNCPO12;
037300140911         k_CPO1crifN = WMPcrif;
037400140911
037500140911         setll  %kds( KeyTNCPO12 )  TNCPO120;
037600140911
037700140911         if  %equal(TNCPO12L);
037800141002           wErr = 10;
037900141002           //exsr  sr_PrintErr;    ?(senza segnalazione di errore)?
038000140911           leavesr;
038100140911         endif;
038200140911
038300140911       ENDSR;
038400140821
038500140821       //--------------------------------------------------------------
038600140821       //?Reperimento nuovo codice per Potenziale da aggiungere.       ?
038700140821       //--------------------------------------------------------------
038800140821       BEGSR  sr_RicercaNuovoCodice;
038900140821
039000141002         reset  wSaveWMPateco;
039100140821
039200140821         // -?Reperimento codice Potenziale da inserire?
039300140821         wNEWcpo8 += 1;
039400140821         k_CPOcpo  = c_CPOini + wNEWcpo8;
039500140821
039600140821         setll  %kds( k01TNCPO01 )  TNCPO000;
039700140821
039800140821         // -?Codice Potenziale già esistente?
039900140821         DoW  %equal(TNCPO01L);
040000140821
040100140821           // -?Non è più calcolabile alcun nuovo codice Potenziale?
040200140821           if  wNEWcpo8 = *hival;
040300140821             wErr = 4;
040400140821             exsr  sr_PrintErr;
040500140821             exsr  sr_RoutEnd;
040600140821           endif;
040700140821
040800140821           // -?Verifica con progressivo successivo?
040900140821           wNEWcpo8 += 1;
041000140821           k_CPOcpo  = c_CPOini + wNEWcpo8;
041100140821           setll  %kds( k01TNCPO01 )  TNCPO000;
041200140821
041300140821         EndDo;
041400140821
041500140821
041600140821         // -?Reperimento categoria merceologica?
041700140821         chain  ( WMPateco )  WATECO00;
041800140821
041900140822         // -?SE Categoria Merceologica mancante =>?
042000140822         //  ?segnalazione in stampa  &?
042100140822         //  ?NO impostazione categoria merceologica?
042200140821         if  Not %found(WATECO1L);
042300141002           if  Not  $WrtTNCPO;
042400141002             wErr = 5;
042500141002             exsr  sr_PrintErr;
042600141002           endif;
042700141002           wSaveWMPateco = WMPateco;
042800140821           clear  WMPateco;
042900140821           clear  WATbCatM;
043000140821         endif;
043100140821
043200140821       ENDSR;
043300140821
043400140821       //--------------------------------------------------------------
043500140821       //?Impostazione nuovi dati nei campi di TNCPO00F e TNCPO10F.    ?
043600140821       //--------------------------------------------------------------
043700140821       BEGSR  sr_ImpostaTNCPO;
043800140821
043900140909         clear  TNCPO000;
044000140909         clear  TNCPO100;
044100140909
044200140821         // -?Impostazione dati nel file TNCPO00F?
044300140821
044400140822         // -?Codice Potenziale?
044500140822         CPOcpo  = k_CPOcpo;
044600140821         // -?Ragione Sociale?
044700140821         CPOrag  = WMPdeno;
044800140825         // -?Indirizzo / Località / CAP / Provincia?
044900140821         exsr  sr_AggioINDIRIZZO;
045000140821         // -?Telefono (ora in TNCPO10F)?
045100140821         //*CPOtel  = WMPtel;
045200140821         // -?Fax (ora in TNCPO10F)?
045300140821         //*CPOfax  = WMPfax;
045400140904         // -?Anno Inizio Attività?
045500140910         //%subst( CPOtel : 1 : 4 ) = %subst( %editc( WMPdtIni : 'X' ) : 5 : 4 );
045600140910         clear  CPOtel;
045700140910         clear  dCPO02;
045800140910         dCPO02.§CPOannoIA = %subst( %editc( WMPdtIni : 'X' ) : 5 : 4 );
045900140910         CPOtel = dCPO02;
046000140821         // -?Categoria Merceologica (categoria SEAT)?
046100140821         if  WMPateco <> *blank  and  WATbCatM > *zero;
046200140821           CPOsct  =  WATbCatM;
046300140821         endif;
046400140826         if  CPOsct = *zero;
046500140904           CPOsct  = 00021;
046600140826         endif;
046700140904         // -?Codice Importanza Cliente --- CPOCLV?
046800140822         // -?Codice Commerciale?
046900140822         // -?Categoria Potenziale?
047000140904         CPOfls  = 'M';
047100140821         // -?Azienda in Difficoltà Finanziarie?
047200140821         %subst( CPOclt : 1 : 1 ) = WMPfr4;
047300140904         // -?Flag E-Commerce?
047400140821         if  WMPeCom = 'Y';
047500140905           //%subst( CPOclt : 2 : 1 ) = WMPeCom;  //? va bene qui/così??? NO!?
047600140905           exsr  sr_TICPI_ECM;
047700140821         endif;
047800140822         // -?Data Acquisizione Potenziale?
047900140904         //CPOdaq  = %dec( wDate_Iso );
048000140904         CPOdaq  = c_Data;
048100140821         // -?Codice Fiscale?
048200140909         CPOcdf  = WMPcfi;
048300140821         // -?Partita Iva?
048400140909         CPOpiv  = WMPcpi;
048500140821         // -?Fatturato Azienda (migliaia di Euro)?
048600140821         eval(H)  CPOftAz = WMPfatt / 1000;
048700140821         // -?Anno Fatturato Azienda?
048800140821         CPOaFAz = WMPaaFt;
048900140822         // -?Flag Fatturato Azienda (S-Stimato, D-Dichiarato)?
049000140825         CPOfFAz = WMPpvnFt;
049100140821         // -?% Export?
049200140821         if  WMPpExp > *zero;
049300140821           CPOpExp = WMPpExp;
049400140821         else;
049500140821           CPOpExp = WMPpImpEx;
049600140821         endif;
049700140821
049800140822         // -?Flag Sede/Filiale  +  Filiale di Appartenenza?
049900140904         exsr  sr_Sede_FilialeAppartenenza;
050000140822
050100140822         // -?Spesa Trasporti Presunta?
050200140822         //  ?- richiede Filiale di Appartenenza -?
050300140822         //  ?(un nuovo Potenziale è impossibile esista già su TICPI00F:?
050400140822         //  ?quindi si esegue subito il Calcolo Spesa Presunta)?
050500140822         clear  dCPO01;
050600140822         exsr  sr_CalcoloSpesaPresunta;
050700140822         dCPO01.§CPOspTp = wSpesaTra;
050800140904         CPOrst  = dCPO01;
050900140822
051000140822         // -?Codice DUNS?
051100140904         // -?Codice Importanza Cliente (A-B-C-T-D) --- CPOFTR?
051200140904         exsr  sr_CodImportanzaCliente;
051300140904         CPOftr = O52cImp;
051400140822         // -?Data Trasmissione?
051500140904         CPOdtr = %dec( wDate_Iso );
051600140904         //CPOdtr  = c_Data;
051700140821
051800140821         // -?Impostazione dati nel file TNCPO10F?
051900140821
052000140909         // -?Codice Potenziale?
052100140909         CPO1cpo = CPOcpo;
052200140821         // -?Telefono?
052300140821         CPO1tel = WMPtel;
052400140821         // -?Fax?
052500140821         CPO1fax = WMPfax;
052600140909         // -?Data Ultima Conferma Info Commerciali?
052700140821         // -?Crif Number?
052800140821         CPO1crifN  = WMPcrif;
052900140821         // -?Crif Number Casa Madre?
053000140821         CPO1crifNM = WMPcmCrif;
053100140909         // -?Flag Operativi?
053200140821
053300140821       ENDSR;
053400140821
053500140821       //--------------------------------------------------------------
053600140825       //?Impostazione indirizzo.                                      ?
053700140821       //--------------------------------------------------------------
053800140821       BEGSR  sr_AGGIOindirizzo;
053900140821
054000141002         clear  s_CpLoPrERR;
054100140825
054200140821         CPOvia = WMPind;
054300140821         CPOcap = WMPcap;
054400140821         CPOprv = WMPprv;
054500140821
054600140821         IF  WMPfra = *blank  or  WMPfra = WMPloc;
054700140821
054800140821           CPOcit = WMPloc;
054900140821
055000140821         ELSE;
055100140821
055200140821           // -?Verifica SE frazione corretta --> al posto della località?
055300140821           clear  TISI95ds;
055400140821           clear  FNLV13ds;
055500140821           I95tcn = '7';
055600140910           //I95cap = WMPcap;         //?(con anche CAP)?
055700140821           I95loc = WMPfra;
055800140821           I95prv = WMPprv;
055900140901           ////if  (WMPreg = 'SAN MARINO'                and
056000140901           ////     WMPprv = *blank)                     OR
056100140901           ////    (WMPloc = 'REPUBBLICA DI SAN MARINO'  and
056200140901           ////     WMPprv = *blank);
056300140901           //if  ( %scan( 'SAN MARINO' : WMPreg ) > *zero  and
056400140901           //      WMPprv <> 'RN' )                        OR
056500140901           //    ( %scan( 'SAN MARINO' : WMPloc ) > *zero  and
056600140901           //      WMPprv <> 'RN' )                        OR
056700140901           //    ( %scan( 'RSM'        : WMPreg ) > *zero  and
056800140901           //      WMPprv <> 'RN' )                        OR
056900140901           //    ( %scan( 'RSM'        : WMPloc ) > *zero  and
057000140901           //      WMPprv <> 'RN' );
057100140901           if  %subst( WMPcap : 1 : 4 ) = c_CAP_SM_parz4;
057200140825             WMPprv = 'RN';
057300140827             CPOprv = WMPprv;
057400140827             I95prv = WMPprv;
057500140825           endif;
057600140821           I95dat = %dec( wDate_Iso );
057700140825           I13af0 = 'S';
057800140822           I13la3 = 'S';
057900140821           //I13cnv = *blank;
058000140821           //I13af1 = *blank;
058100140821
058200140821           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
058300140821
058400140910           ////If  O95err =  *blank  and
058500140910           ////    O95cap <> *blank  and
058600140910           ////    O95prv <> *blank  and
058700140910           ////    O95loc <> *blank;
058800140910           //If  O95err =  *blank  and  O13err <> '5';
058900140910           If  O95err = *blank;
059000140821
059100140821             CPOcit = O95loc;
059200140821             CPOcap = O95cap;
059300140821             CPOprv = O95prv;
059400140821
059500140821           Else;
059600140821
059700140821             clear  w255a;
059800140821             w255a = %trim(WMPind) + '-' + %trim(WMPfra);
059900140821             //if  %subst( w255a : 36 ) = *blank;
060000140821             if  %subst( w255a : %size( CPOvia ) + 1 ) = *blank;
060100140821               CPOvia = w255a;
060200140821               CPOcit = WMPloc;
060300140821             else;
060400140821               CPOcit = WMPloc;
060500140821             endif;
060600140821
060700140821           EndIf;
060800140821
060900140821         ENDIF;
061000140821
061100140821
061200140821         IF  CPOcap = *blank  or  CPOcap = '00000    ';
061300140821
061400140821           clear  TISI95ds;
061500140821           clear  FNLV13ds;
061600140821           I95tcn = '7';
061700140821           I95loc = CPOcit;
061800140821           I95prv = CPOprv;
061900140904           //if  (WMPreg = 'SAN MARINO'                and
062000140904           //     WMPprv = *blank)                     OR
062100140904           //    (WMPloc = 'REPUBBLICA DI SAN MARINO'  and
062200140904           //     WMPprv = *blank);
062300140904           if  ( %scan( 'SAN MARINO' : WMPreg ) > *zero  and
062400140904                 WMPprv <> 'RN' )                        OR
062500140904               ( %scan( 'SAN MARINO' : WMPloc ) > *zero  and
062600140904                 WMPprv <> 'RN' )                        OR
062700140904               ( %scan( 'RSM'        : WMPreg ) > *zero  and
062800140904                 WMPprv <> 'RN' )                        OR
062900140904               ( %scan( 'RSM'        : WMPloc ) > *zero  and
063000140904                 WMPprv <> 'RN' );
063100140826             WMPprv = 'RN';
063200140827             CPOprv = WMPprv;
063300140827             I95prv = WMPprv;
063400140825           endif;
063500140821           I95dat = %dec( wDate_Iso );
063600140821           I13af0 = 'S';
063700140821           I13la3 = 'S';
063800140821           //I13cnv = *blank;
063900140821           //I13af1 = *blank;
064000140821
064100140821           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
064200140821
064300140821           If  O95err = *blank;
064400140821
064500140821             CPOcap = O95cap;
064600140821             CPOprv = O95prv;
064700140821
064800141002           Else;
064900141002
065000141002             s_CpLoPrERR = '?';
065100141002             wErr = 6;
065200141002             //exsr  sr_PrintErr;
065300140821
065400140821           EndIf;
065500140821
065600140821         ENDIF;
065700140821
065800140821       ENDSR;
065900140905
066000140905       //--------------------------------------------------------------
066100140905       //?Impostaz. campi nel rec."ECM" da aggiungere al file TICPI00F.?
066200140905       //--------------------------------------------------------------
066300140905       BEGSR  sr_TICPI_ECM;
066400140905
066500140905         clear  TICPI000;
066600140905
066700140905         CPIcpo = CPOcpo;
066800140905         CPItpf = 'ECM';
066900140905         //CPIspf = *blank;
067000140905
067100140905         CPIval = 50;
067200140905         CPItva = '%  ';
067300140905
067400140905         //CPIdim = %dec( wDate_Iso );
067500140905         CPIdim = c_Data;
067600140905         CPIfil = 046;
067700140905         CPIpru = 'BATCH     ';
067800140905
067900140905       ENDSR;
068000140904
068100140904       //--------------------------------------------------------------
068200140904       //?Impostazione flag Sede/Filiale e campo Filiale Appartenenza. ?
068300140904       //--------------------------------------------------------------
068400140904       BEGSR  sr_Sede_FilialeAppartenenza;
068500140904
068600140904         // -?Flag Sede/Filiale?
068700140904         CPOfsf = 'S';
068800140904
068900140904         // -?Verifica esistenza Potenziale con:?
069000140904         //  ?· (stessa Partita Iva  o?
069100140904         //  ?·  stesso Codice Fiscale)  e?
069200140904         //  ?· CPOFSF = "S"?
069300140904         if  CPOpiv <> *blank;
069400140904           chain  ( CPOpiv )  TNCPO006;
069500140904         endif;
069600140904         if  CPOcdf <> *blank;
069700140904           chain  ( CPOcdf )  TNCPO005;
069800140904         endif;
069900140904
070000140904         Select;
070100140904
070200140904           // -?Sia P.I. che C.F. sono vuoti?
070300140904           When  CPOpiv = *blank  and  CPOcdf = *blank;
070400140911             CPOfsf = 'F';
070500140904             wErr = 8;
070600140911             //s_Testo2 = 'SENZA né Partita Iva né Codice Fiscale.';
070700140911             //exsr  sr_PrintErr;
070800140904
070900140904           // -?Sia P.I. che C.F. sono già presenti?
071000140904           When  ( CPOpiv <> *blank  and
071100140904                   %found(TNCPO06L)  and  yCPOfsf = 'S' )  and
071200140904                 ( CPOcdf <> *blank  and
071300140904                   %found(TNCPO05L)  and  xCPOfsf = 'S' );
071400140904             CPOfsf = 'F';
071500140911             wErr = 8;
071600140911             //s_Testo2 = 'assegnato a Filiale; di Sede trovato ' +
071700140911             //           %editc( yCPOcpo : '3' );
071800140911             //// ·?...entrambi sullo stesso potenziale?
071900140911             //if  yCPOcpo = xCPOcpo;
072000140911             //  s_Testo2 = %trimR( s_Testo2 ) + ' con stessi P.I. e C.F.';
072100140911             //// ·?...su potenziali diversi (P.I. su un potenziale,?
072200140911             ////  ?                          C.F. su un altro)?
072300140911             //else;
072400140911             //  s_Testo2 = %trimR( s_Testo2 ) + ' con stessa P.I. e ' +
072500140911             //             %editc( xCPOcpo : '3' ) +
072600140911             //             ' con stesso C.F.';
072700140911             //endif;
072800140911             //exsr  sr_PrintErr;
072900140904
073000140904           // -?Solo la P.I. è già presente su un altro potenziale?
073100140904           When  ( CPOpiv <> *blank  and
073200140904                   %found(TNCPO06L)  and  yCPOfsf = 'S' );
073300140904             CPOfsf = 'F';
073400140904             wErr = 8;
073500140911             //s_Testo2 = 'assegnato a Filiale; di Sede trovato ' +
073600140911             //           %editc( yCPOcpo : '3' ) +
073700140911             //           ' con stessa P.I.';
073800140911             //if  CPOcdf = *blank;
073900140911             //  s_Testo2 = %trimR( s_Testo2 ) + ' - ma manca il C.F.';
074000140911             //endif;
074100140911             //exsr  sr_PrintErr;
074200140904
074300140904           // -?Solo il C.F. è già presente su un altro potenziale?
074400140904           When  ( CPOcdf <> *blank  and
074500140904                   %found(TNCPO05L)  and  xCPOfsf = 'S' );
074600140904             CPOfsf = 'F';
074700140904             wErr = 8;
074800140911             //s_Testo2 = 'assegnato a Filiale; di Sede trovato ' +
074900140911             //           %editc( xCPOcpo : '3' ) +
075000140911             //           ' con stesso C.F.';
075100140911             //if  CPOpiv = *blank;
075200140911             //  s_Testo2 = %trimR( s_Testo2 ) + ' - ma manca la P.I.';
075300140911             //endif;
075400140911             //exsr  sr_PrintErr;
075500140904
075600140904         EndSl;
075700140904
075800140904
075900140904         // -?Filiale di Appartenenza?
076000140904         clear  TISI95ds;
076100140904         clear  FNLV13ds;
076200140911
076300140911         //I95tcn = '7';
076400140911         //I95cap = WMPcap;
076500140911         //if  WMPfra <> *blank;
076600140911         //  I95loc = WMPfra;
076700140911         //  clear  I95cap;
076800140911         //else;
076900140911         //  I95loc = WMPloc;
077000140911         //endif;
077100140911         //I95prv = WMPprv;
077200140911         //if  %subst( WMPcap : 1 : 4 ) = c_CAP_SM_parz4;
077300140911         //  WMPprv = 'RN';
077400140911         //  I95prv = 'RN';
077500140911         //endif;
077600140911         //I95dat = %dec( wDate_Iso );
077700140911         //I13af0 = 'S';
077800140911         //I13la3 = 'S';
077900140911         //
078000140911         //FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
078100140911         //
078200140911         ////CPOflt = O95lna;
078300140911         //// -?SE non reperita la filiale con la Frazione,?
078400140911         ////  ?ci si riprova con la Località?
078500140911         //if  O95lna = *zero  and  WMPfra <> *blank;
078600140911         //  I95loc = WMPloc;
078700140911         //  I95cap = WMPcap;
078800140911         //  FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
078900140911         //endif;
079000140911
079100140911         I95tcn = '7';
079200140911         I95cap = CPOcap;
079300140911         I95loc = CPOcit;
079400140911         I95prv = CPOprv;
079500140911         I95dat = %dec( wDate_Iso );
079600140911         I13af0 = 'S';
079700140911         I13la3 = 'S';
079800140911
079900140911         FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
080000140904
080100140904         // -?Segnalazione in stampa del NON riuscito reperimento della?
080200140904         //  ?Filiale di Appartenenza?
080300140904         if  O95lna = *zero;
080400140904           wErr = 9;
080500140911           //exsr  sr_PrintErr;
080600140904         endif;
080700140904
080800140904         // -?Impostazione Filiale di Appartenenza?
080900140904         CPOflt = O95lna;
081000140904
081100140904       ENDSR;
081200140821
081300140821       //--------------------------------------------------------------
081400140821       //?Calcolo Spesa Presunta.                                      ?
081500140821       //--------------------------------------------------------------
081600140821       BEGSR  sr_CalcoloSpesaPresunta;
081700140821
081800140821         clear  wDEMfil;
081900140821         clear  wDEMare;
082000140821         clear  wDEMtotFat;
082100140821         clear  wAlfaSCT;
082200140821         clear  wPerc;
082300140821         clear  wSpesaTra;
082400140821
082500140821         // -?SE non c'è un Fatturato Aziendale: NON si calcola nulla?
082600140821         if  CPOftAz <= *zero;
082700140821           leavesr;
082800140821         endif;
082900140821
083000140821         wDEMtotFat = CPOftAz * 1000;
083100140904         wAlfaSCT   = %editc( CPOsct : 'X' );
083200140821
083300140821
083400140904         // -?Imposto SEMPRE la filiale del codice?
083500140904         wDEMfil = CPOflt;
083600140821
083700140821
083800140821         // -?SE trovata filiale: impostazione area?
083900140821         if  wDEMfil > *zero;
084000140821           chain  ( wDEMfil )  AZORG;
084100140821           if  %found(AZORG01L);
084200140821             wDEMare = ORGcar;
084300140821           endif;
084400140821         endif;
084500140821
084600140821
084700140821         // -?Reperimento area di appartenenza dall'indirizzo?
084800140821         chain  ( wAlfaSCT : wDEMare )  WFDEMR00;
084900140821         if  %found(WFDEMR1L);
085000140821           exsr  sr_ScegliP;
085100140821         endif ;
085200140821
085300140821         if  wPerc = *zero;
085400140821           clear  wDEMare;
085500140821           chain  ( wAlfaSCT : 000 )  WFDEMR00;
085600140821           if  %found(WFDEMR1L);
085700140821             exsr  sr_ScegliP;
085800140821           endif;
085900140821         endif;
086000140821
086100140821         If  wPerc > *zero;
086200140821           eval(H)  wSpesaTra = ( wDEMtotFat * wPerc ) / 100;
086300140821           // -?Arrotondamento ai 1000 € superiori?
086400140821           w0080 = wSpesaTra / 1000;
086500140821           w0080 = w0080 + 1;
086600140821           wSpesaTra = w0080 * 1000;
086700140821         EndIf;
086800140821
086900140821       ENDSR;
087000140821
087100140821       //--------------------------------------------------------------
087200140821       //?Reperimento percentuale per calcolare la spesa presunta.     ?
087300140821       //--------------------------------------------------------------
087400140821       BEGSR  sr_ScegliP;
087500140821
087600140821         select;
087700140821
087800140821           when  wDEMtotFat <= c_PiccoleAL;
087900140821             if  DEMpCli > 9  or  wDEMare = *zero;
088000140821               wPerc = DEMpPerc;
088100140821             endif;
088200140821
088300140821           when  wDEMtotfat <= c_MedieAL;
088400140821             if  DEMmCli > 9  or  wDEMare = *zero;
088500140821               wPerc = DEMmPerc;
088600140821             endif;
088700140821
088800140821           other;
088900140821             if  DEMgCli > 9  or  wDEMare = *zero;
089000140821               wPerc = DEMgPerc;
089100140821             endif;
089200140821
089300140821         endsl;
089400140821
089500140821       ENDSR;
089600140904
089700140904       //--------------------------------------------------------------
089800140904       //?Calcolo Cod. Importanza Cli.Potenziale (A/B/C/T/D) - CPOFTR. ?
089900140904       //--------------------------------------------------------------
090000140904       BEGSR  sr_CodImportanzaCliente;
090100140904
090200140904         clear  TRMK52ds;
090300140904         I52cpo  = CPOcpo;
090400140904         I52spt  = wSpesaTra;
090500140904         I52tSpt = 'P';
090600140904
090700140904         trmk52r  ( TRMK52ds );
090800140904
090900140904       ENDSR;
091000140821
091100140821       //--------------------------------------------------------------
091200140821       //?Scrittura dati nei file TNCPIO00F e TNCPO10F.                ?
091300140821       //--------------------------------------------------------------
091400140821       BEGSR  sr_WriteTNCPO;
091500140821
091600140821         // -?Scrittura rec. nel file TNCPO00F?
091700140821         //______________
091800140821         write  TNCPO000;
091900140821         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
092000140821
092100140821         // -?Scrittura rec. nel file TNCPO10F?
092200140821         //______________
092300140821         write  TNCPO100;
092400140821         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
092500140905
092600140905         // -?Scrittura rec. "ECM" nel file TICPI00F?
092700140905         if  WMPeCom = 'Y';
092800140905           //______________
092900140905           write  TICPI000;
093000140905           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093100140905         endif;
093200140821
093300140821       ENDSR;
093400140821
093500141002       //--------------------------------------------------------------
093600141002       //?Stampa dati del cliente potenziale da inserire.              ?
093700141002       //--------------------------------------------------------------
093800141002       BEGSR  sr_PrintCliPot;
093900141002
094000141002         // -?Impostazione campi di stampa?
094100141002         //  ?(con lunghezza diversa da quella dei campi "originali")?
094200141002         // ·?Spesa Trasporti Presunta?
094300141002         //  ?(SE NON c'è la spesa reale delle Info-Commerciali,?
094400141002         //  ? SE c'è un Fatturato Aziendale  e?
094500141002         //  ? SE è stata calcolcolata)?
094600141002         clear  s_SpesaTra;
094700141002         if  CPOftAz > *zero  and  wSpesaTra > *zero;
094800141002           s_SpesaTra  = c_SpesaTra + ' ' + %editc( wSpesaTra : '1' );
094900141002         endif;
095000141002         // ·?Categoria Merceologica ERRATA?
095100141002         clear  s_CatMercERR;
095200141002         if  wSaveWMPateco <> *hival;
095300141002           s_CatMercERR = 'Cat.Merc.ERRATA: ' + wSaveWMPateco;
095400141002         endif;
095500141002         // ·?Cap/Località/Provincia ERRATI?
095600141002         //  ?(già impostato)?
095700141002         //clear  s_CpLoPrERR;
095800141002         //if  wErr = 7;
095900141002         //  s_CpLoPrERR = '?';
096000141002         //endif;
096100141002
096200141002         // ·?"Codice" errore (NON segnalato)?
096300141002         clear  s_Err;
096400141002         if  wErr > *zero;
096500141002           s_Err = '*ERR: ' + %trim( %editc( wErr : '3' ) );
096600141002         endif;
096700141002
096800141002         // -?Apertura del printer-file?
096900141002         if  Not %open(QSYSPRT1);
097000141002           Qcmd = 'OVRPRTF file(QSYSPRT1) +
097100141002                           usrdta(''Ins. TNCPO'') +
097200141002                           ovrscope(*actgrpdfn)';
097300141002           exsr  sr_ExecCmd;
097400141002           open  QSYSPRT1;
097500141002           except  PRT1txt;
097600141002         endif;
097700141002
097800141002         if  *inOA;
097900141002           except  PRT1txt;
098000141002           *inOA = *off;
098100141002         endif;
098200141002
098300141002         except  PRT1det;
098400141002
098500141002       ENDSR;
098600140821
098700140821       //--------------------------------------------------------------
098800140821       //?Stampa segnalazione dell'errore rilevato.                    ?
098900140821       //--------------------------------------------------------------
099000140821       BEGSR  sr_PrintErr;
099100140821
099200140821         // -?Apertura del printer-file?
099300140821         if  Not %open(QSYSPRT);
099400140821           Qcmd = 'OVRPRTF file(QSYSPRT) +
099500140821                           usrdta(''ACPO07*ERR'') +
099600140821                           ovrscope(*actgrpdfn)';
099700140821           exsr  sr_ExecCmd;
099800140821           open  QSYSPRT;
099900140821           except  PRTtxt;
100000140821         endif;
100100140821
100200140821         if  *inOF;
100300140821           except  PRTtxt;
100400140821           *inOF = *off;
100500140821         endif;
100600140821
100700140821         select;
100800140821           when  wErr = 1;
100900140821             except  PrtErr1;
101000140821           when  wErr = 2;
101100140821             except  PrtErr2;
101200140821           when  wErr = 3;
101300140821             except  PrtErr3;
101400140821           when  wErr = 4;
101500140821             except  PrtErr4;
101600140821           when  wErr = 5;
101700140821             except  PrtErr5;
101800141002           //when  wErr = 6;
101900141002           //  s_Testo = 'Cap: '      + %trimR( CPOcap ) +
102000141002           //            ', Comune: ' + %trimR( CPOcit ) +
102100141002           //            ', Prov.: '  + CPOprv;
102200141002           //  except  PrtErr6;
102300141002           //when  wErr = 7;
102400141002           //  except  PrtErr7;
102500141002           //when  wErr = 8;
102600141002           //  except  PrtErr8;
102700141002           //when  wErr = 9;
102800141002           //  s_Testo = 'Cap: '      + %trimR( CPOcap ) +
102900141002           //            ', Comune: ' + %trimR( CPOcit ) +
103000141002           //            ', Prov.: '  + CPOprv;
103100141002           //  except  PrtErr9;
103200140821         endsl;
103300140821
103400140821       ENDSR;
103500140821
103600140821       //--------------------------------------------------------------
103700140821       //?Operazioni finali.                                           ?
103800140821       //--------------------------------------------------------------
103900140821       BEGSR  sr_RoutEnd;
104000140821
104100140821         // -?Chiusura spool di stampa errori?
104200140821         if  %open(QSYSPRT);
104300140821           except  PrtEND;
104400140821           close  QSYSPRT;
104500140821         endif;
104600140821
104700141002         // -?Chiusura spool di stampa?
104800141002         if  %open(QSYSPRT1);
104900141002           except  Prt1END;
105000141002           close  QSYSPRT1;
105100141002         endif;
105200140821
105300140821         // -?Chiusura Work-File di input?
105400140821         if  %open(WFMDP00F);
105500140821           close  WFMDP00F;
105600140821         endif;
105700140821         if  %open(WATECO1L);
105800140821           close  WATECO1L;
105900140821         endif;
106000140821         if  %open(WFDEMR1L);
106100140821           close  WFDEMR1L;
106200140821         endif;
106300140821
106400140821         // -?Chiusura *pgm?
106500140821         return;
106600140821
106700140821       ENDSR;
106800140821
106900140821       //--------------------------------------------------------------
107000140821       //?Esecuzione del comando (già impostato).                      ?
107100140821       //--------------------------------------------------------------
107200140821       BEGSR  sr_ExecCmd;
107300140821
107400140821         clear Qcap0100;
107500140821         Qcabcsdh = *off;
107600140821         Qcapa    = *off;
107700140821         Qcacmdss = *off;
107800140821         Qcaerved = *allX'00';
107900140821
108000140821         clear Qusec;
108100140821         Qusbprv  = %size(Qusec);
108200140821
108300140821         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
108400140821                           %size(Qcap0100) : 'CPOP0100' : *omit :
108500140821                           0 : 0 : Qusec);
108600140821
108700140821         //if  Qusei <> *blank;
108800140821         //  ...;
108900140821         //endif;
109000140821
109100140821       ENDSR;
109200140821
109300140821      /end-free
109400140821
109500140821       //--------------------------------------------------------------
109600140821       //?S P O O L - F I L E S                                        ?
109700140821       //--------------------------------------------------------------
109800140821
109900140821       // -?Stampa segnalazioni di errore?
110000140821     oQSYSPRT   e            PRTtxt            2
110100140821     o                       RSUT
110200140821     o                                        +   4 'LISTA ERRORI RILE-
110300140821     o                                              VATI IN FASE DI EL-
110400140821     o                                              ABORAZIONE DATI NE-
110500140821     o                                              L FILE'
110600140821     o                       wLibFile1        +   1
110700140821     o                       SDSpgmName       +   4
110800140821     o                       *Date         y  +   2
110900140821     o          e            PRTtxt      0
111000140821     o                                          +24 'LISTA ERRORI RILE-
111100140821     o                                              VATI IN FASE DI EL-
111200140821     o                                              ABORAZIONE DATI NE-
111300140821     o                                              L FILE'
111400140821     o                       wLibFile1        +   1
111500140821     o          e            PRTtxt      1
111600140821     o                       KNSIF
111700140821     o                       KNMUS            +   1
111800140821     o                                        +   3 '------------------
111900140821     o                                              -------------------
112000140821     o                                              -------------------
112100140821     o                                              -------------------
112200140821     o                                              ----------'
112300140821     o                                        +   4 'Pag.'
112400140821     o                       Page          z  +   0
112500140821     o                       wTime            +   4 '  :  :  '
112600140821      *
112700140821     o          e            PRTtxt      2
112800140822     o                                              'Crif Number    '
112900140821     o                                        +   2 'Messaggio'
113000140821     o          e            PRTtxt      1
113100140822     o                                              '---------------'
113200140821     o                                        +   2 '---------'
113300140821      *
113400140821     o          e            PRTerr1     2
113500140821     o                                              'NON TROVATO IL +
113600140821     o                                               FILE'
113700140821     o                       wLibFile1        +   1
113800140821      *
113900140821     o          e            PRTerr2     2
114000140821     o                                              'NON TROVATO IL +
114100140821     o                                               FILE'
114200140821     o                       wLibFile2        +   1
114300140821      *
114400140821     o          e            PRTerr3     2
114500140821     o                                              'NON TROVATO IL +
114600140821     o                                               FILE'
114700140821     o                       wLibFile3        +   1
114800140821      *
114900140821     o          e            PRTerr4     2
115000140822     o                       WMPcrif
115100140822     o                                        +   2 'NON è più calcola-
115200140821     o                                              bile un nuovo codi-
115300140821     o                                              ce Potenziale nel -
115400140822     o                                              file TNCPO00F: esi-
115500140822     o                                              ste già'
115600140822     o                       k_CPOcpo         +   1
115700140821      *
115800140821     o          e            PRTerr5     2
115900140822     o                       WMPcrif
116000140821     o                                        +   2 'Categoria merceol-
116100140821     o                                              ogica'
116200140821     o                       WMPateco         +   1
116300140822     o                                        +   1 'NON reperita nell-
116400140821     o                                              a tabella WATECO0F-
116500140822     o                                               (per potenziale'
116600140822     o                       k_CPOcpo         +   1
116700140822     o                                        +   0 ').'
116800141002      *// *
116900141002      *//o          e            PRTerr6     2
117000141002      *//o                       WMPcrif
117100141002      *//o                                        +   2 'Fallito il contro-
117200141002      *//o                                              llo del CAP (per p-
117300141002      *//o                                              otenziale'
117400141002      *//o                       k_CPOcpo         +   1
117500141002      *//o                                        +   0 '):'
117600141002      *//o          e            PRTerr6     1
117700141002      *//o                       O95msg           +  17
117800141002      *//o          e            PRTerr6     1
117900141002      *//o                       s_Testo          +  17
118000141002      *// *
118100141002      *//o          e            PRTerr7     2
118200141002      *//o                       WMPcrif
118300141002      *//o                                        +   2 'Indirizzo errato:-
118400141002      *//o                                               sistemare anagraf-
118500141002      *//o                                              ica per il potenzi-
118600141002      *//o                                              ale'
118700141002      *//o                       k_CPOcpo         +   1
118800141002      *//o          e            PRTerr7     1
118900141002      *//o                       CPOcap           +  17
119000141002      *//o                       CPOcit           +   1
119100141002      *//o                       CPOprv           +   1
119200141002      *// *
119300141002      *//o          e            PRTerr8     2
119400141002      *//o                       WMPcrif
119500141002      *//o                                        +   2 'Potenziale'
119600141002      *//o                       k_CPOcpo      3  +   1
119700141002      *//o*//                                     +   1 'assegnato a Filia-
119800141002      *//o*//                                           le; di Sede trovat-
119900141002      *//o*//                                           o'
120000141002      *//o*//                    yCPOcpo          +   1
120100141002      *//o*//                                     +   1 'con stessi P.I. e-
120200141002      *//o*//                                            C.F.'
120300141002      *//o                       s_Testo2         +   1
120400141002      *// *
120500141002      *//o          e            PRTerr9     2
120600141002      *//o                       WMPcrif
120700141002      *//o                                        +   2 'Fallito il reperi-
120800141002      *//o                                              mento della Filial-
120900141002      *//o                                              e di Appartenenza -
121000141002      *//o                                              per il potenziale'
121100141002      *//o                       k_CPOcpo         +   1
121200141002      *//o                                        +   1 '- assegnata'
121300141002      *//o                       O95lna           +   1
121400141002      *//o                                        +   0 '.'
121500141002      *//o          e            PRTerr9     1
121600141002      *//o                       s_Testo          +  17
121700140821      *
121800140821     o          e            PRTend      2
121900140821     o                                        +  24 '***  Fine Lista  ***'
122000140821     o          e            PRTend      0
122100140821     o                                        +  24 '***  Fine Lista  ***'
122200140821
122300141002       // -?Stampa dettaglio potenziali elaborati?
122400141002     oQSYSPRT1  e            PRT1txt           2
122500141002     o                       RSUT
122600141002     o                                        +   4 'LISTA POTENZIALI'
122700141002     o                       wAggior          +   1
122800141002     o                       SDSpgmName       +   4
122900141002     o                       *Date         y  +   2
123000141002     o          e            PRT1txt     0
123100141002     o                                          +24 'LISTA POTENZIALI'
123200141002     o                       wAggior          +   1
123300141002     o          e            PRT1txt     1
123400141002     o                       KNSIF
123500141002     o                       KNMUS            +   1
123600141002     o                                        +   3 '------------------
123700141002     o                                              ----------'
123800141002     o                                        +   4 'Pag.'
123900141002     o                       Page2         z  +   0
124000141002     o                       wTime            +   4 '  :  :  '
124100141002      *
124200141002     o          e            PRT1txt     2
124300141002     o                                              'Crif Number    '
124400141002     o                                        +   1 'Potenziale '
124500141002     o          e            PRT1txt     1  1
124600141002     o                                              '---------------'
124700141002     o                                        +   1 '-----------'
124800141002      *
124900141002     o          e            PRT1det     1
125000141002     o                       WMPcrif
125100141002     o                       CPOcpo           +   1
125200141002     o                       CPOrag           +   1
125300141002     o                                        +   3 'Sede/Filiale:'
125400141002     o                       CPOfsf           +   1
125500141002     o                                        +   3 'Fil.Appartenenza:'
125600141002     o                       CPOflt           +   1
125700141002     o                       s_Err            +   2
125800141002     o          e            PRT1det     1
125900141002     o                       CPOcap           +  28
126000141002     o                       CPOcit           +   1
126100141002     o                       CPOprv           +   1
126200141002     o                       s_CpLoPrERR      +   1
126300141002     o                       s_SpesaTra       +   1
126400141002      *
126500141002     o          e            PRT1end     2
126600141002     o                                        +  24 '***  Fine Lista  ***'
126700141002     o          e            PRT1end     0
126800141002     o                                        +  24 '***  Fine Lista  ***'
