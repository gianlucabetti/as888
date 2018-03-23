000100140821       //==============================================================
000200140821       //?Acquisizione Potenziali: lettura del file WFMDP00F per       ?
000300140821       //?                 SCRITTURA  dati nel file TNCPO00F.          ?
000400140821       //==============================================================
000500140821
000600140821       //--------------------------------------------------------------
000700140821       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140821       //--------------------------------------------------------------
000900140821
001000140821     /*PRM  dbgview(*source)
001100140821     /*CMD  ovrdbf file(WFDEMR1L) tofile(GAITRAAZP/WFDEMR1L) +
001200140821     /*CMD         ovrscope(*calllvl)
001300140821     /*END  dltovr file(WFDEMR1L) lvl(*)
001400140821     /*END
001500140821
001600140821       //--------------------------------------------------------------
001700140821       //?Specifiche di controllo.                                     ?
001800140821       //--------------------------------------------------------------
001900140821
002000140821     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100140821     h dftactgrp(*no)
002200140821
002300140821       //--------------------------------------------------------------
002400140821       //?Dichiarazione file.                                          ?
002500140821       //--------------------------------------------------------------
002600140821
002700140821       // -?WrkF Acquisizione Potenziali: main dati potenziali CRIF?
002800140821     fWFMDP00F  if   e             disk
002900140821     f                                     extfile(wLibFile1)
003000140821     f                                     usropn
003100140821       // -?WrkF Classificazione attività economiche ATECO 2007?
003200140821     fWATECO1L  if   e           k disk
003300140821     f                                     extfile(wLibFile2)
003400140821     f                                     usropn
003500140821       // -?WrkF per Mappatura Commerciale -?
003600140821       //  ?% di spesa Presunta trasporti sul fatturato?
003700140821     fWFDEMR1L  if   e           k disk
003800140821     f                                     extfile(wLibFile3)
003900140821     f                                     usropn
004000140821
004100140821       // -?Informazioni commerciali clienti Potenziali?
004200140821     fTICPI01L  if   e           k disk
004300140821
004400140821       // -?Organigramma?
004500140821     fAZORG01L  if   e           k disk
004600140821
004700140821       // -?Anagrafica Clienti Potenziali?
004800140822     fTNCPO06L  if   e           k disk
004900140822     f                                     rename(TNCPO000 : TNCPO006)
005000140822     f                                     prefix(x)
005100140822
005200140821     fTNCPO01L  if A e           k disk
005300140821     fTNCPO11L  if A e           k disk
005400140821
005500140821       // -?Stampa segnalazioni di errore?
005600140821     fQSYSPRT   o    f  132        printer
005700140821     f                                     usropn
005800140821     f                                     oflind(*inOF)
005900140821
006000140821       // -?Stampa dettaglio potenziali elaborati?
006100140821     fQSYSPRT1  o    f  132        printer
006200140821     f                                     usropn
006300140821     f                                     oflind(*inOA)
006400140821
006500140821       //--------------------------------------------------------------
006600140821       //?Definizione costanti.                                        ?
006700140821       //--------------------------------------------------------------
006800140821
006900140821       // -?Parte "iniziale" del nuovo Codice Potenziale?
007000140821     d c_CPOini        c                   const(04600000000)
007100140821
007200140821       // -?Limite fatturato per "piccole imprese"?
007300140821     d c_PiccoleAL     c                   const(10000000)
007400140821       // -?Limite fatturato per "medie imprese"?
007500140821     d c_MedieAL       c                   const(50000000)
007600140821
007700140821       // -?Costanti in stampa (forse)?
007800140821     d c_SpesaTra      c                   const('Spesa Trasp. Presunta:')
007900140821
008000140821       //--------------------------------------------------------------
008100140821       //?Definizione schiere.                                         ?
008200140821       //--------------------------------------------------------------
008300140821
008400140821
008500140821       //--------------------------------------------------------------
008600140821       //?Definizione aree dati.                                       ?
008700140821       //--------------------------------------------------------------
008800140821
008900140821       // -?Dati utente?
009000140821     d §AzUte        e ds                  extname(AZUTE00F)
009100140821     d                                     dtaara
009200140821     d §DatiUte      e ds                  extname(dDatiUte)
009300140821     d                                     dtaara
009400140821
009500140821       //--------------------------------------------------------------
009600140821       //?Definizione strutture dati.                                  ?
009700140821       //--------------------------------------------------------------
009800140821
009900140821       // -?Status ds?
010000140821     d Status         sds
010100140821     d   SDSpgmName      *proc
010200140821     d*//SDSparms        *parms
010300140821     d   SDSjobName          244    253                                         Job name
010400140821     d   SDSjobUser          254    263                                         User name
010500140821     d*//SDSjobNumber        264    269s 0                                      Job number
010600140821
010700140821       // -?Parametri ricevuti?
010800140821     d KPJBA         e ds
010900140821
011000140821       // -?DS per campo CPORST di TNCPO00F
011100140821     d dCPO01        e ds                  inz  qualified
011200140821
011300140821       //--------------------------------------------------------------
011400140821       //?Definizione variabili globali.                               ?
011500140821       //--------------------------------------------------------------
011600140821
011700140821       // -?Nome esteso Libreria/File del file da elaborare?
011800140821     d wLibFile1       s             21a   inz
011900140821     d wLibFile2       s             21a   inz
012000140821     d wLibFile3       s             21a   inz
012100140821
012200140821       // -?Flags Booleani?
012300140821     d $WrtTNCPO       s               n   inz
012400140821
012500140821       // -?Campi per il calcolo del nuovo cod. Potenziale?
012600140821     d wNEWcpo8        s              8  0 inz(02999999)
012700140821
012800140821       // -?Campi di comodo?
012900140821     d wDate_Iso       s               d   inz  datfmt(*iso)
013000140821     d wDate_Eur       s               d   inz  datfmt(*eur)
013100140821     d wDate           s              8  0 inz
013200140821     d wTime           s              6  0 inz
013300140821     d wErr            s              3  0 inz
013400140821     d wSituazione     s              1  0 inz
013500140821     d w255a           s            255    inz
013600140825     d wCPOflt         s                   inz  like(CPOflt)
013700140821
013800140821       // -?Campi per la subr. sr_CalcoloSpesaPresunta?
013900140821     d wDEMfil         s              3  0 inz
014000140821     d wDEMare         s              3  0 inz
014100140821     d wDEMtotFat      s             15  0 inz
014200140821     d wPerc           s              5  2 inz
014300140821     d wSpesaTra       s             11  0 inz
014400140821     d w0080           s              8  0 inz
014500140821     d wAlfaSCT        s              5    inz
014600140821
014700140821       // -?Campi di stampa?
014800140821     d wAggior         s             10    inz('INSERIBILI')
014900140821     d s_SpesaTra      s             40    inz
015000140825     d s_Testo         s            115    inz
015100140821
015200140821       //--------------------------------------------------------------
015300140821       //?Definizione prototipi procedure.                             ?
015400140821       //--------------------------------------------------------------
015500140821
015600140821       // -?Reperimento dati utente?
015700140821     d TIBS34ds      e ds                  inz
015800140821      /copy gaitrasrc/srcProtoPR,TIBS34R
015900140821
016000140821       // -?Controllo CAP ("Pretrullo TISI95R")?
016100140821     d FNLV13ds      e ds                  inz
016200140821     d TISI95ds      e ds                  inz
016300140821     d fnlv13r         pr                  extpgm('FNLV13R')
016400140821     d   kpjba                             likeds(KPJBA)
016500140821     d   fnlv13ds                          likeds(FNLV13ds)
016600140821     d   tisi95ds                          likeds(TISI95ds)
016700140822     d tisi95r         pr                  extpgm('TISI95R')
016800140822     d   tisi95ds                          likeds(TISI95ds)
016900140821
017000140821       // -?API QCAPCMD (Process Commands)?
017100140821     d Qcmd            s           2048    inz  varying
017200140821      /copy qSysInc/qRpgleSrc,QCAPCMD
017300140821      /copy gaitrasrc/srcProtoPR,QCAPCMD
017400140821
017500140821       // -?Parametri gestione errori API.?
017600140821      /copy qsysinc/qrpglesrc,QUSEC
017700140821
017800140821       //--------------------------------------------------------------
017900140821       //?Definizione key-list.                                        ?
018000140821       //--------------------------------------------------------------
018100140821
018200140821       // -?File TNCPO01L?
018300140821     d k01tncpo01    e ds                  extname(TNCPO01L : *key)
018400140821     d                                     prefix(k_)   inz
018500140821
018600140821       //--------------------------------------------------------------
018700140821       //?Riepilogo indicatori utilizzati.                             ?
018800140821       //--------------------------------------------------------------
018900140821
019000140821
019100140821       //--------------------------------------------------------------
019200140821       //?M A I N - L I N E                                            ?
019300140821       //--------------------------------------------------------------
019400140821
019500140821     c     *Entry        plist
019600140821     c                   parm                    KPJBA
019700140821
019800140821      /free
019900140821
020000140821       // -?Operazioni iniziali?
020100140821       exsr sr_RoutInz;
020200140821
020300140821       // -?Ciclo di lettura file WFMDP00F?
020400140821       read  WFMDP000;
020500140821
020600140821       DoW  Not %eof(WFMDP00F);
020700140821
020800140821         exsr  sr_ElabPot;
020900140821
021000140821         read  WFMDP000;
021100140821
021200140821       EndDo;
021300140821
021400140821       // -?Operazioni finali?
021500140821       exsr sr_RoutEnd;
021600140821
021700140821       //--------------------------------------------------------------
021800140821       //?Operazioni iniziali.                                         ?
021900140821       //--------------------------------------------------------------
022000140821
022100140821       BEGSR  sr_RoutInz;
022200140821
022300140821         // -?Impostazione chiusura?
022400140821         *inLR = *on;
022500140821
022600140821         // -?Ricezione parametri?
022700140821         $WrtTNCPO  = ( %subst( KPJBU : 1 : 3 ) = 'A  ' );
022800140821         if  $WrtTNCPO;
022900140821           wAggior = 'INSERITI  ';
023000140821         endif;
023100140821
023200140821         // -?Reperimento data odierna in formato aaaa/mm/gg?
023300140821         wDate_Iso = %date();
023400140821         // -?Conversione data odierna in formato gg/mm/aaaa?
023500140821         wDate_Eur = wDate_Iso;
023600140821         wDate = %dec( wDate_Eur );
023700140821
023800140821         // -?Reperimento orario (hh:mm:ss)?
023900140821         wTime = %dec( %time() );
024000140821
024100140821         // -?Reperimento dati job?
024200140821         exsr  sr_DatiJob;
024300140821
024400140821         // -?Impostazione librerie dei work-file di input:?
024500140821         //  ?1) WFMDP00F?
024600140821         //  ?2) WATECO1L?
024700140821         //  ?3) WFDEMR1L?
024800140821         if  %subst( knsif : 7 : 1 ) = 'P';
024900140821           wLibFile1 = '*LIBL';
025000140821           wLibFile3 = 'GAITRAAZP';
025100140821         else;
025200140821           wLibFile1 = 'EDPCRIBIS';
025300140821           wLibFile3 = 'GAITRAAZM';
025400140821         endif;
025500140821         wLibFile2 = wLibFile1;
025600140821
025700140821         // -?Apertura del work-file di input WFMDP00F?
025800140821         wLibFile1 = %trimR( wLibFile1 ) + '/WFMDP00F';
025900140821         open(e)  WFMDP00F;
026000140821         if  %error();
026100140821           wErr = 1;
026200140821           exsr  sr_PrintErr;
026300140821           exsr  sr_RoutEnd;
026400140821         endif;
026500140821
026600140821         // -?Apertura del work-file di input WATECO1L?
026700140821         wLibFile2 = %trimR( wLibFile2 ) + '/WATECO1L';
026800140821         open(e)  WATECO1L;
026900140821         if  %error();
027000140821           wErr = 2;
027100140821           exsr  sr_PrintErr;
027200140821           exsr  sr_RoutEnd;
027300140821         endif;
027400140821
027500140821         // -?Apertura del work-file di input WFDEMR1L?
027600140821         wLibFile3 = %trimR( wLibFile3 ) + '/WFDEMR1L';
027700140821         open(e)  WFDEMR1L;
027800140821         if  %error();
027900140821           wErr = 3;
028000140821           exsr  sr_PrintErr;
028100140821           exsr  sr_RoutEnd;
028200140821         endif;
028300140821
028400140821       ENDSR;
028500140821
028600140821       //--------------------------------------------------------------
028700140821       //?Reperimento Dati del job (Utente/Operativi).                 ?
028800140821       //--------------------------------------------------------------
028900140821       BEGSR  sr_DatiJob;
029000140821
029100140821         in(e) §AzUte;
029200140821         if NOT %error;
029300140821           in(e) §DatiUte;
029400140821         endif;
029500140821         if %error or RSut = *blank;
029600140821           tibs34r ( tibs34ds );
029700140821           in §AzUte;
029800140821           in §DatiUte;
029900140821         endif;
030000140821
030100140821       ENDSR;
030200140821
030300140821       //--------------------------------------------------------------
030400140821       //?Elaborazione singolo cliente Potenziale.                     ?
030500140821       //--------------------------------------------------------------
030600140821       BEGSR  sr_ElabPot;
030700140821
030800140821         // -?Reperimento nuovo codice per Potenziale da aggiungere?
030900140821         exsr  sr_RicercaNuovoCodice;
031000140821
031100140821         // -?Valorizzazione campi in TNCPO (00F/10F)?
031200140821         //  ?(comodo comunque per la stampa)?
031300140821         exsr  sr_ImpostaTNCPO;
031400140821
031500140821         // -?Stampa?
031600140821         exsr  sr_PrintCliPot;
031700140821
031800140821         // -?Scrittura TNCPO (se richiesto)?
031900140821         if  $WrtTNCPO;
032000140821           exsr  sr_WriteTNCPO;
032100140821         endif;
032200140821
032300140821       ENDSR;
032400140821
032500140821       //--------------------------------------------------------------
032600140821       //?Reperimento nuovo codice per Potenziale da aggiungere.       ?
032700140821       //--------------------------------------------------------------
032800140821       BEGSR  sr_RicercaNuovoCodice;
032900140821
033000140821         clear  wSituazione;
033100140821
033200140821         // -?Reperimento codice Potenziale da inserire?
033300140821         wNEWcpo8 += 1;
033400140821         k_CPOcpo  = c_CPOini + wNEWcpo8;
033500140821
033600140821         setll  %kds( k01TNCPO01 )  TNCPO000;
033700140821
033800140821         // -?Codice Potenziale già esistente?
033900140821         DoW  %equal(TNCPO01L);
034000140821
034100140821           // -?Non è più calcolabile alcun nuovo codice Potenziale?
034200140821           if  wNEWcpo8 = *hival;
034300140821             wErr = 4;
034400140821             exsr  sr_PrintErr;
034500140821             exsr  sr_RoutEnd;
034600140821           endif;
034700140821
034800140821           // -?Verifica con progressivo successivo?
034900140821           wNEWcpo8 += 1;
035000140821           k_CPOcpo  = c_CPOini + wNEWcpo8;
035100140821           setll  %kds( k01TNCPO01 )  TNCPO000;
035200140821
035300140821         EndDo;
035400140821
035500140821
035600140821         // -?Reperimento categoria merceologica?
035700140821         chain  ( WMPateco )  WATECO00;
035800140821
035900140822         // -?SE Categoria Merceologica mancante =>?
036000140822         //  ?segnalazione in stampa  &?
036100140822         //  ?NO impostazione categoria merceologica?
036200140821         if  Not %found(WATECO1L);
036300140821           wErr = 5;
036400140821           exsr  sr_PrintErr;
036500140821           clear  WMPateco;
036600140821           clear  WATbCatM;
036700140821         endif;
036800140821
036900140821       ENDSR;
037000140821
037100140821       //--------------------------------------------------------------
037200140821       //?Impostazione nuovi dati nei campi di TNCPO00F e TNCPO10F.    ?
037300140821       //--------------------------------------------------------------
037400140821       BEGSR  sr_ImpostaTNCPO;
037500140821
037600140821         // -?Impostazione dati nel file TNCPO00F?
037700140821
037800140822         // -?Codice Potenziale?
037900140822         CPOcpo  = k_CPOcpo;
038000140821         // -?Ragione Sociale?
038100140821         CPOrag  = WMPdeno;
038200140825         // -?Indirizzo / Località / CAP / Provincia?
038300140821         exsr  sr_AggioINDIRIZZO;
038400140821         // -?Telefono (ora in TNCPO10F)?
038500140821         //*CPOtel  = WMPtel;
038600140821         // -?Fax (ora in TNCPO10F)?
038700140821         //*CPOfax  = WMPfax;
038800140821         // -?Anno Inizio Attività? ? va bene qui??? ?
038900140821         //CPOtel  = %int( %subst( %editc( WMPdtIni : 'X' ) : 5 : 4 ) );
039000140821         // -?Categoria Merceologica (categoria SEAT)?
039100140821         if  WMPateco <> *blank  and  WATbCatM > *zero;
039200140821           CPOsct  =  WATbCatM;
039300140821         endif;
039400140826         if  CPOsct = *zero;
039500140826           CPOsct = 00021;
039600140826         endif;
039700140822         // -?Codice Importanza Cliente?
039800140822         // -?Codice Commerciale?
039900140822         // -?Categoria Potenziale?
040000140821         // -?Azienda in Difficoltà Finanziarie?
040100140821         %subst( CPOclt : 1 : 1 ) = WMPfr4;
040200140822         // -?Flag E-Commerce?      ? va bene qui??? ?
040300140821         if  WMPeCom = 'Y';
040400140821           %subst( CPOclt : 2 : 1 ) = WMPeCom;
040500140821         endif;
040600140822         // -?Data Acquisizione Potenziale?
040700140822         CPOdaq = %dec( wDate_Iso );
040800140821         // -?Codice Fiscale?
040900140821         CPOpiv  =  WMPcfi;
041000140821         // -?Partita Iva?
041100140821         CPOpiv  =  WMPcpi;
041200140821         // -?Fatturato Azienda (migliaia di Euro)?
041300140821         eval(H)  CPOftAz = WMPfatt / 1000;
041400140821         // -?Anno Fatturato Azienda?
041500140821         CPOaFAz = WMPaaFt;
041600140822         // -?Flag Fatturato Azienda (S-Stimato, D-Dichiarato)?
041700140825         CPOfFAz = WMPpvnFt;
041800140821         // -?% Export?
041900140821         if  WMPpExp > *zero;
042000140821           CPOpExp = WMPpExp;
042100140821         else;
042200140821           CPOpExp = WMPpImpEx;
042300140821         endif;
042400140821
042500140822         // -?Flag Sede/Filiale  +  Filiale di Appartenenza?
042600140825         //if  wCPOflt = *zero;
042700140825           exsr  sr_Sede_FilialeAppartenenza;
042800140825         //endif;
042900140822
043000140822         // -?Spesa Trasporti Presunta?
043100140822         //  ?- richiede Filiale di Appartenenza -?
043200140822         //  ?(un nuovo Potenziale è impossibile esista già su TICPI00F:?
043300140822         //  ?quindi si esegue subito il Calcolo Spesa Presunta)?
043400140822         clear  dCPO01;
043500140822         exsr  sr_CalcoloSpesaPresunta;
043600140822         dCPO01.§CPOspTp = wSpesaTra;
043700140822         CPOrst = dCPO01;
043800140822
043900140822         // -?Codice DUNS?
044000140822         // -?Codice Importanza Cliente (A-B-C-T-D)?
044100140822         // -?Data Trasmissione?
044200140822         CPOdtr = %dec( wDate_Iso );
044300140821
044400140821         // -?Impostazione dati nel file TNCPO10F?
044500140821
044600140821         // -?Telefono?
044700140821         CPO1tel = WMPtel;
044800140821         // -?Fax?
044900140821         CPO1fax = WMPfax;
045000140821         // -?Crif Number?
045100140821         CPO1crifN  = WMPcrif;
045200140821         // -?Crif Number Casa Madre?
045300140821         CPO1crifNM = WMPcmCrif;
045400140821
045500140821       ENDSR;
045600140821
045700140821       //--------------------------------------------------------------
045800140825       //?Impostazione indirizzo.                                      ?
045900140821       //--------------------------------------------------------------
046000140821       BEGSR  sr_AGGIOindirizzo;
046100140821
046200140825         clear  wCPOflt;
046300140825
046400140821         CPOvia = WMPind;
046500140821         CPOcap = WMPcap;
046600140821         CPOprv = WMPprv;
046700140821
046800140821         IF  WMPfra = *blank  or  WMPfra = WMPloc;
046900140821
047000140821           CPOcit = WMPloc;
047100140821
047200140821         ELSE;
047300140821
047400140821           // -?Verifica SE frazione corretta --> al posto della località?
047500140821           clear  TISI95ds;
047600140821           clear  FNLV13ds;
047700140821           I95tcn = '7';
047800140821           I95loc = WMPfra;
047900140821           I95prv = WMPprv;
048000140825           if  (WMPreg = 'SAN MARINO'                and
048100140825                WMPprv = *blank)                     OR
048200140825               (WMPloc = 'REPUBBLICA DI SAN MARINO'  and
048300140825                WMPprv = *blank);
048400140825             WMPprv = 'RN';
048500140825             I95prv = 'RN';
048600140825           endif;
048700140821           I95dat = %dec( wDate_Iso );
048800140825           I13af0 = 'S';
048900140822           I13la3 = 'S';
049000140821           //I13cnv = *blank;
049100140821           //I13af1 = *blank;
049200140821
049300140821           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
049400140821
049500140821           If  O95err = *blank;
049600140821
049700140821             CPOcit = O95loc;
049800140821             CPOcap = O95cap;
049900140821             CPOprv = O95prv;
050000140821
050100140821           Else;
050200140821
050300140821             clear  w255a;
050400140821             w255a = %trim(WMPind) + '-' + %trim(WMPfra);
050500140821             //if  %subst( w255a : 36 ) = *blank;
050600140821             if  %subst( w255a : %size( CPOvia ) + 1 ) = *blank;
050700140821               CPOvia = w255a;
050800140821               CPOcit = WMPloc;
050900140821             else;
051000140821               CPOcit = WMPloc;
051100140821             endif;
051200140821
051300140821           EndIf;
051400140821
051500140821         ENDIF;
051600140821
051700140821
051800140821         IF  CPOcap = *blank  or  CPOcap = '00000    ';
051900140821
052000140821           clear  TISI95ds;
052100140821           clear  FNLV13ds;
052200140821           I95tcn = '7';
052300140821           I95loc = CPOcit;
052400140821           I95prv = CPOprv;
052500140825           if  (WMPreg = 'SAN MARINO'                and
052600140825                WMPprv = *blank)                     OR
052700140825               (WMPloc = 'REPUBBLICA DI SAN MARINO'  and
052800140825                WMPprv = *blank);
052900140825             WMPprv = 'RN';
053000140825             I95prv = 'RN';
053100140825           endif;
053200140821           I95dat = %dec( wDate_Iso );
053300140821           I13af0 = 'S';
053400140821           I13la3 = 'S';
053500140821           //I13cnv = *blank;
053600140821           //I13af1 = *blank;
053700140821
053800140821           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
053900140821
054000140821           If  O95err = *blank;
054100140821
054200140821             CPOcap = O95cap;
054300140821             CPOprv = O95prv;
054400140825             wCPOflt = O95lna;
054500140821
054600140821           Else;
054700140825
054800140825             wErr = 6;
054900140825             exsr  sr_PrintErr;
055000140821
055100140821           EndIf;
055200140821
055300140821         ENDIF;
055400140825
055500140825         // -?Memorizzazione Filiale di Appartenenza?
055600140821
055700140821       ENDSR;
055800140821
055900140821       //--------------------------------------------------------------
056000140821       //?Calcolo Spesa Presunta.                                      ?
056100140821       //--------------------------------------------------------------
056200140821       BEGSR  sr_CalcoloSpesaPresunta;
056300140821
056400140821         clear  wDEMfil;
056500140821         clear  wDEMare;
056600140821         clear  wDEMtotFat;
056700140821         clear  wAlfaSCT;
056800140821         clear  wPerc;
056900140821         clear  wSpesaTra;
057000140821
057100140821         // -?SE non c'è un Fatturato Aziendale: NON si calcola nulla?
057200140821         if  CPOftAz <= *zero;
057300140821           leavesr;
057400140821         endif;
057500140821
057600140821         wDEMtotFat = CPOftAz * 1000;
057700140821
057800140821         wAlfaSCT = %editc( CPOsct : 'X' );
057900140821
058000140821         // -?SE presente la nazione, imposto la filiale del codice?
058100140821         //  ?e NON da cappario?
058200140821         If  CPOnaz <> *blank;
058300140821
058400140821           wDEMfil = CPOflt;
058500140821
058600140821         Else;
058700140821
058800140821           clear  TISI95ds;
058900140821           clear  FNLV13ds;
059000140821           I95tcn = '7';
059100140821           I95cap = CPOcap;
059200140821           I95loc = CPOcit;
059300140821           I95prv = CPOprv;
059400140825           if  %subst( I95cap : 1 : 2 ) = '47'  and  I95prv = *blank;
059500140825             I95prv = 'RN';
059600140825           endif  ;
059700140821           I95nar = CPOnaz;
059800140821           I95dat = %dec( wDate_Iso );
059900140825           I13af0 = 'S';
060000140822           I13la3 = 'S';
060100140821           //I13cnv = *blank;
060200140821           //I13af1 = *blank;
060300140821
060400140821           FNLV13R  ( KPJBA : FNLV13ds : TISI95ds );
060500140821
060600140821           wDEMfil = O95lna;
060700140821
060800140821         EndIf;
060900140821
061000140821         // -?SE trovata filiale: impostazione area?
061100140821         if  wDEMfil > *zero;
061200140821           chain  ( wDEMfil )  AZORG;
061300140821           if  %found(AZORG01L);
061400140821             wDEMare = ORGcar;
061500140821           endif;
061600140821         endif;
061700140821
061800140821
061900140821         // -?Reperimento area di appartenenza dall'indirizzo?
062000140821         chain  ( wAlfaSCT : wDEMare )  WFDEMR00;
062100140821         if  %found(WFDEMR1L);
062200140821           exsr  sr_ScegliP;
062300140821         endif ;
062400140821
062500140821         if  wPerc = *zero;
062600140821           clear  wDEMare;
062700140821           chain  ( wAlfaSCT : 000 )  WFDEMR00;
062800140821           if  %found(WFDEMR1L);
062900140821             exsr  sr_ScegliP;
063000140821           endif;
063100140821         endif;
063200140821
063300140821         If  wPerc > *zero;
063400140821           eval(H)  wSpesaTra = ( wDEMtotFat * wPerc ) / 100;
063500140821           // -?Arrotondamento ai 1000 € superiori?
063600140821           w0080 = wSpesaTra / 1000;
063700140821           w0080 = w0080 + 1;
063800140821           wSpesaTra = w0080 * 1000;
063900140821         EndIf;
064000140821
064100140821       ENDSR;
064200140821
064300140821       //--------------------------------------------------------------
064400140821       //?Reperimento percentuale per calcolare la spesa presunta.     ?
064500140821       //--------------------------------------------------------------
064600140821       BEGSR  sr_ScegliP;
064700140821
064800140821         select;
064900140821
065000140821           when  wDEMtotFat <= c_PiccoleAL;
065100140821             if  DEMpCli > 9  or  wDEMare = *zero;
065200140821               wPerc = DEMpPerc;
065300140821             endif;
065400140821
065500140821           when  wDEMtotfat <= c_MedieAL;
065600140821             if  DEMmCli > 9  or  wDEMare = *zero;
065700140821               wPerc = DEMmPerc;
065800140821             endif;
065900140821
066000140821           other;
066100140821             if  DEMgCli > 9  or  wDEMare = *zero;
066200140821               wPerc = DEMgPerc;
066300140821             endif;
066400140821
066500140821         endsl;
066600140821
066700140821       ENDSR;
066800140822
066900140822       //--------------------------------------------------------------
067000140822       //?Impostazione flag Sede/Filiale e campo Filiale Appartenenza. ?
067100140822       //--------------------------------------------------------------
067200140822       BEGSR  sr_Sede_FilialeAppartenenza;
067300140822
067400140822         // -?Flag Sede/Filiale?
067500140822         CPOfsf = 'S';
067600140822         // -?Verifica esistenza Potenziale con:?
067700140822         //  ?· stessa Partita Iva  e?
067800140822         //  ?· stesso Codice Fiscale  e?
067900140822         //  ?· CPOFSF = "S"?
068000140822         chain  ( CPOpiv )  TNCPO006;
068100140822         if  %found(TNCPO06L)  and  xCPOcdf = CPOcdf
068200140822                               and  xCPOfsf = 'S';
068300140822           CPOfsf = 'F';
068400140825           wErr = 7;
068500140822           exsr  sr_PrintErr;
068600140822         endif;
068700140822
068800140822         // -?Filiale di Appartenenza?
068900140822         clear  TISI95ds;
069000140822         clear  FNLV13ds;
069100140822         I95tcn = '7';
069200140825         //I95cap = WMPcap;
069300140825         //if  WMPfra <> *blank;
069400140825         //  I95loc = WMPfra;
069500140825         //  clear  I95cap;
069600140825         //else;
069700140825         //  I95loc = WMPloc;
069800140825         //endif;
069900140825         //I95prv = WMPprv;
070000140825         //if  (WMPreg = 'SAN MARINO'                and
070100140825         //     WMPprv = *blank)                     OR
070200140825         //    (WMPloc = 'REPUBBLICA DI SAN MARINO'  and
070300140825         //     WMPprv = *blank);
070400140825         //  WMPprv = 'RN';
070500140825         //  I95prv = 'RN';
070600140825         //endif;
070700140825         //I95dat = %dec( wDate_Iso );
070800140825         //I13af0 = 'S';
070900140825         //I13la3 = 'S';
071000140825         ////I13cnv = *blank;
071100140825         ////I13af1 = *blank;
071200140825         //FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
071300140825         //// -?SE non reperita la filiale con la Frazione,?
071400140825         ////  ?ci si riprova con la Località?
071500140825         //If  O95lna = *zero  and  WMPfra <> *blank;
071600140825         //  I95loc = WMPloc;
071700140825         //  I95cap = WMPcap;
071800140825         //  FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
071900140825         //EndIf;
072000140825         // -?Località/CAP/Provincia già reperiti ed impostati...?
072100140825         I95cap = CPOcap;
072200140825         I95loc = CPOcit;
072300140825         I95prv = CPOprv;
072400140825         I95dat = %dec( wDate_Iso );
072500140825         I13af0 = 'S';
072600140825         I13la3 = 'S';
072700140825         FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
072800140822
072900140822         // -?Segnalazione in stampa del NON riuscito reperimento della?
073000140825         //  ?Filiale di Appartenenza (a questo punto ricavata dalla?
073100140825         //  ?Località o dal CAP)?
073200140822         if  O95lna = *zero;
073300140825           clear  TISI95ds;
073400140825           I95tcn = '4';
073500140825           //I95cap = CPOcap;
073600140825           I95loc = CPOcit;
073700140825           I95prv = CPOprv;
073800140825           TISI95R ( TISI95ds );
073900140825           if  O95lna = *zero;
074000140825             clear  TISI95ds;
074100140825             I95tcn = '3';
074200140825             I95cap = CPOcap;
074300140825             TISI95R ( TISI95ds );
074400140825           endif;
074500140825           wErr = 8;
074600140825           exsr  sr_PrintErr;
074700140822         endif;
074800140822
074900140822         // -?Impostazione Filiale di Appartenenza?
075000140825         CPOflt = O95lna;
075100140822
075200140822       ENDSR;
075300140821
075400140821       //--------------------------------------------------------------
075500140821       //?Scrittura dati nei file TNCPIO00F e TNCPO10F.                ?
075600140821       //--------------------------------------------------------------
075700140821       BEGSR  sr_WriteTNCPO;
075800140821
075900140821         // -?Scrittura rec. nel file TNCPO00F?
076000140821         //______________
076100140821         write  TNCPO000;
076200140821         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076300140821
076400140821         // -?Scrittura rec. nel file TNCPO10F?
076500140821         //______________
076600140821         write  TNCPO100;
076700140821         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076800140821
076900140821       ENDSR;
077000140821
077100140821       //--------------------------------------------------------------
077200140821       //?Stampa dati del cliente potenziale da inserire.              ?
077300140821       //--------------------------------------------------------------
077400140821       BEGSR  sr_PrintCliPot;
077500140821
077600140821         // -?Impostazione campi di stampa?
077700140821         //  ?(con lunghezza diversa da quella dei campi "originali")?
077800140821         // ·?Spesa Trasporti Presunta?
077900140821         //  ?(SE NON c'è la spesa reale delle Info-Commerciali,?
078000140821         //  ? SE c'è un Fatturato Aziendale  e?
078100140821         //  ? SE è stata calcolcolata)?
078200140821         clear  s_SpesaTra;
078300140821         if  Not %found(TICPI01L)  and  CPOftAz > *zero  and
078400140821             wSpesaTra > *zero;
078500140821           s_SpesaTra  = c_SpesaTra + ' ' + %editc( wSpesaTra : '1' );
078600140821         endif;
078700140821
078800140821         // -?Apertura del printer-file?
078900140821         if  Not %open(QSYSPRT1);
079000140821           Qcmd = 'OVRPRTF file(QSYSPRT1) +
079100140821                           usrdta(''Agg. TNCPO'') +
079200140821                           ovrscope(*actgrpdfn)';
079300140821           exsr  sr_ExecCmd;
079400140821           open  QSYSPRT1;
079500140821           except  PRT1txt;
079600140821         endif;
079700140821
079800140821         if  *inOA;
079900140821           except  PRT1txt;
080000140821           *inOA = *off;
080100140821         endif;
080200140821
080300140821         except  PRT1det;
080400140821
080500140821       ENDSR;
080600140821
080700140821       //--------------------------------------------------------------
080800140821       //?Stampa segnalazione dell'errore rilevato.                    ?
080900140821       //--------------------------------------------------------------
081000140821       BEGSR  sr_PrintErr;
081100140821
081200140821         // -?Apertura del printer-file?
081300140821         if  Not %open(QSYSPRT);
081400140821           Qcmd = 'OVRPRTF file(QSYSPRT) +
081500140821                           usrdta(''ACPO07*ERR'') +
081600140821                           ovrscope(*actgrpdfn)';
081700140821           exsr  sr_ExecCmd;
081800140821           open  QSYSPRT;
081900140821           except  PRTtxt;
082000140821         endif;
082100140821
082200140821         if  *inOF;
082300140821           except  PRTtxt;
082400140821           *inOF = *off;
082500140821         endif;
082600140821
082700140821         select;
082800140821           when  wErr = 1;
082900140821             except  PrtErr1;
083000140821           when  wErr = 2;
083100140821             except  PrtErr2;
083200140821           when  wErr = 3;
083300140821             except  PrtErr3;
083400140821           when  wErr = 4;
083500140821             except  PrtErr4;
083600140821           when  wErr = 5;
083700140821             except  PrtErr5;
083800140822           when  wErr = 6;
083900140825             s_Testo = 'Cap: '      + %trimR( CPOcap ) +
084000140825                       ', Comune: ' + %trimR( CPOcit ) +
084100140825                       ', Prov.: '  + CPOprv;
084200140822             except  PrtErr6;
084300140825           when  wErr = 7;
084400140825             except  PrtErr7;
084500140825           when  wErr = 8;
084600140825             //s_Testo = 'Cap: ' + WMPcap +
084700140825             //          ', Comune: ' + %trim( WMPloc );
084800140825             //if  WMPfra <> *blank;
084900140825             //  s_Testo = %trimR( s_Testo ) +
085000140825             //            ', Frazione: ' + %trim( WMPfra );
085100140825             //endif;
085200140825             //s_Testo = %trimR( s_Testo ) +
085300140825             //          ', Prov.: ' + WMPprv;
085400140825             s_Testo = 'Cap: '      + %trimR( CPOcap ) +
085500140825                       ', Comune: ' + %trimR( CPOcit ) +
085600140825                       ', Prov.: '  + CPOprv;
085700140825             except  PrtErr8;
085800140821         endsl;
085900140821
086000140821       ENDSR;
086100140821
086200140821       //--------------------------------------------------------------
086300140821       //?Operazioni finali.                                           ?
086400140821       //--------------------------------------------------------------
086500140821       BEGSR  sr_RoutEnd;
086600140821
086700140821         // -?Chiusura spool di stampa errori?
086800140821         if  %open(QSYSPRT);
086900140821           except  PrtEND;
087000140821           close  QSYSPRT;
087100140821         endif;
087200140821
087300140821         // -?Chiusura spool di stampa?
087400140821         if  %open(QSYSPRT1);
087500140821           except  Prt1END;
087600140821           close  QSYSPRT1;
087700140821         endif;
087800140821
087900140821         // -?Chiusura Work-File di input?
088000140821         if  %open(WFMDP00F);
088100140821           close  WFMDP00F;
088200140821         endif;
088300140821         if  %open(WATECO1L);
088400140821           close  WATECO1L;
088500140821         endif;
088600140821         if  %open(WFDEMR1L);
088700140821           close  WFDEMR1L;
088800140821         endif;
088900140821
089000140821         // -?Chiusura *pgm?
089100140821         return;
089200140821
089300140821       ENDSR;
089400140821
089500140821       //--------------------------------------------------------------
089600140821       //?Esecuzione del comando (già impostato).                      ?
089700140821       //--------------------------------------------------------------
089800140821       BEGSR  sr_ExecCmd;
089900140821
090000140821         clear Qcap0100;
090100140821         Qcabcsdh = *off;
090200140821         Qcapa    = *off;
090300140821         Qcacmdss = *off;
090400140821         Qcaerved = *allX'00';
090500140821
090600140821         clear Qusec;
090700140821         Qusbprv  = %size(Qusec);
090800140821
090900140821         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
091000140821                           %size(Qcap0100) : 'CPOP0100' : *omit :
091100140821                           0 : 0 : Qusec);
091200140821
091300140821         //if  Qusei <> *blank;
091400140821         //  ...;
091500140821         //endif;
091600140821
091700140821       ENDSR;
091800140821
091900140821      /end-free
092000140821
092100140821       //--------------------------------------------------------------
092200140821       //?S P O O L - F I L E S                                        ?
092300140821       //--------------------------------------------------------------
092400140821
092500140821       // -?Stampa segnalazioni di errore?
092600140821     oQSYSPRT   e            PRTtxt            2
092700140821     o                       RSUT
092800140821     o                                        +   4 'LISTA ERRORI RILE-
092900140821     o                                              VATI IN FASE DI EL-
093000140821     o                                              ABORAZIONE DATI NE-
093100140821     o                                              L FILE'
093200140821     o                       wLibFile1        +   1
093300140821     o                       SDSpgmName       +   4
093400140821     o                       *Date         y  +   2
093500140821     o          e            PRTtxt      0
093600140821     o                                          +24 'LISTA ERRORI RILE-
093700140821     o                                              VATI IN FASE DI EL-
093800140821     o                                              ABORAZIONE DATI NE-
093900140821     o                                              L FILE'
094000140821     o                       wLibFile1        +   1
094100140821     o          e            PRTtxt      1
094200140821     o                       KNSIF
094300140821     o                       KNMUS            +   1
094400140821     o                                        +   3 '------------------
094500140821     o                                              -------------------
094600140821     o                                              -------------------
094700140821     o                                              -------------------
094800140821     o                                              ----------'
094900140821     o                                        +   4 'Pag.'
095000140821     o                       Page          z  +   0
095100140821     o                       wTime            +   4 '  :  :  '
095200140821      *
095300140821     o          e            PRTtxt      2
095400140822     o                                              'Crif Number    '
095500140821     o                                        +   2 'Messaggio'
095600140821     o          e            PRTtxt      1
095700140822     o                                              '---------------'
095800140821     o                                        +   2 '---------'
095900140821      *
096000140821     o          e            PRTerr1     2
096100140821     o                                              'NON TROVATO IL +
096200140821     o                                               FILE'
096300140821     o                       wLibFile1        +   1
096400140821      *
096500140821     o          e            PRTerr2     2
096600140821     o                                              'NON TROVATO IL +
096700140821     o                                               FILE'
096800140821     o                       wLibFile2        +   1
096900140821      *
097000140821     o          e            PRTerr3     2
097100140821     o                                              'NON TROVATO IL +
097200140821     o                                               FILE'
097300140821     o                       wLibFile3        +   1
097400140821      *
097500140821     o          e            PRTerr4     2
097600140822     o                       WMPcrif
097700140822     o                                        +   2 'NON è più calcola-
097800140821     o                                              bile un nuovo codi-
097900140821     o                                              ce Potenziale nel -
098000140822     o                                              file TNCPO00F: esi-
098100140822     o                                              ste già'
098200140822     o                       k_CPOcpo         +   1
098300140821      *
098400140821     o          e            PRTerr5     2
098500140822     o                       WMPcrif
098600140821     o                                        +   2 'Categoria merceol-
098700140821     o                                              ogica'
098800140821     o                       WMPateco         +   1
098900140822     o                                        +   1 'NON reperita nell-
099000140821     o                                              a tabella WATECO0F-
099100140822     o                                               (per potenziale'
099200140822     o                       k_CPOcpo         +   1
099300140822     o                                        +   0 ').'
099400140825      *
099500140825     o          e            PRTerr6     2
099600140825     o                       WMPcrif
099700140825     o                                        +   2 'Fallito il contro-
099800140825     o                                              llo del CAP (per p-
099900140825     o                                              otenziale'
100000140825     o                       k_CPOcpo         +   1
100100140825     o                                        +   0 '):'
100200140825     o          e            PRTerr6     1
100300140825     o                       O95msg           +  17
100400140825     o          e            PRTerr6     1
100500140825     o                       s_Testo          +  17
100600140822      *
100700140825     o          e            PRTerr7     2
100800140822     o                       WMPcrif
100900140822     o                                        +   2 'Potenziale'
101000140822     o                       k_CPOcpo         +   1
101100140822     o                                        +   1 'assegnato a Filia-
101200140822     o                                              le; di Sede trovat-
101300140822     o                                              o'
101400140822     o                       xCPOcpo          +   1
101500140822     o                                        +   1 'con stessi P.I. e-
101600140822     o                                               C.F.'
101700140822      *
101800140825     o          e            PRTerr8     2
101900140822     o                       WMPcrif
102000140822     o                                        +   2 'Fallito il reperi-
102100140822     o                                              mento della Filial-
102200140822     o                                              e di Appartenenza -
102300140822     o                                              per il potenziale'
102400140822     o                       k_CPOcpo         +   1
102500140822     o                                        +   1 '- assegnata'
102600140822     o                       O95lna           +   1
102700140822     o                                        +   0 '.'
102800140825     o          e            PRTerr8     1
102900140825     o                       s_Testo          +  17
103000140821      *
103100140821     o          e            PRTend      2
103200140821     o                                        +  24 '***  Fine Lista  ***'
103300140821     o          e            PRTend      0
103400140821     o                                        +  24 '***  Fine Lista  ***'
103500140821
103600140821       // -?Stampa dettaglio potenziali elaborati?
103700140821     oQSYSPRT1  e            PRT1txt           2
103800140821     o                       RSUT
103900140821     o                                        +   4 'LISTA POTENZIALI'
104000140821     o                       wAggior          +   1
104100140821     o                       SDSpgmName       +   4
104200140821     o                       *Date         y  +   2
104300140821     o          e            PRT1txt     0
104400140821     o                                          +24 'LISTA POTENZIALI'
104500140821     o                       wAggior          +   1
104600140821     o          e            PRT1txt     1
104700140821     o                       KNSIF
104800140821     o                       KNMUS            +   1
104900140821     o                                        +   3 '------------------
105000140821     o                                              ----------'
105100140821     o                                        +   4 'Pag.'
105200140821     o                       Page2         z  +   0
105300140821     o                       wTime            +   4 '  :  :  '
105400140821      *
105500140821     o          e            PRT1txt     2
105600140821     o                                              'Crif Number    '
105700140821     o                                        +   1 'Potenziale '
105800140821     o          e            PRT1txt     1  1
105900140821     o                                              '---------------'
106000140821     o                                        +   1 '-----------'
106100140821      *
106200140821     o          e            PRT1det     1
106300140821     o                       WMPcrif
106400140822     o                       CPOcpo           +   1
106500140821     o                       CPOrag           +   1
106600140822     o                                        +   3 'Sede/Filiale:'
106700140822     o                       CPOfsf           +   1
106800140822     o                                        +   3 'Fil.Appartenenza:'
106900140822     o                       CPOflt           +   1
107000140821     o          e            PRT1det     1
107100140821     o                       CPOcap           +  28
107200140821     o                       CPOcit           +   1
107300140821     o                       CPOprv           +   1
107400140821     o                       s_SpesaTra       +   3
107500140821      *
107600140821     o          e            PRT1end     2
107700140821     o                                        +  24 '***  Fine Lista  ***'
107800140821     o          e            PRT1end     0
107900140821     o                                        +  24 '***  Fine Lista  ***'
