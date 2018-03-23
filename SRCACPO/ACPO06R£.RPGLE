000100140818       //==============================================================
000200140818       //?Acquisizione Potenziali: lettura del file WFACP00F per       ?
000300140818       //?             AGGIORNAMENTO  dati nel file TNCPO00F.          ?
000400140818       //==============================================================
000500140818
000600140818       //--------------------------------------------------------------
000700140818       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140818       //--------------------------------------------------------------
000900140818
001000140818     /*PRM  dbgview(*source)
001100140826     /*CMD  ovrdbf file(WFACP00F) tofile(*LIBL/WFACPV0F) +
001200140826     /*CMD         ovrscope(*calllvl)
001300140820     /*CMD  ovrdbf file(WFDEMR1L) tofile(GAITRAAZP/WFDEMR1L) +
001400140820     /*CMD         ovrscope(*calllvl)
001500140826     /*END  dltovr file(WFACP00F WFDEMR1L) lvl(*)
001600140818     /*END
001700140818
001800140818       //--------------------------------------------------------------
001900140818       //?Specifiche di controllo.                                     ?
002000140818       //--------------------------------------------------------------
002100140818
002200140818     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300140818     h dftactgrp(*no)
002400140818
002500140818       //--------------------------------------------------------------
002600140818       //?Dichiarazione file.                                          ?
002700140818       //--------------------------------------------------------------
002800140818
002900140818       // -?WrkF Acquisizione Potenziali con dati ricevuti da CRIBIS?
003000140826       //  ?(clienti da aggiornare: WFACPV0F)?
003100140826     fWFACP00F  if   e             disk
003200140819     f                                     extfile(wLibFile1)
003300140826     f                                     rename(WFACPV00 : WFACP000)
003400140818     f                                     usropn
003500140819       // -?WrkF Classificazione attività economiche ATECO 2007?
003600140820     fWATECO1L  if   e           k disk
003700140819     f                                     extfile(wLibFile2)
003800140819     f                                     usropn
003900140820       // -?WrkF per Mappatura Commerciale -?
004000140820       //  ?% di spesa Presunta trasporti sul fatturato?
004100140820     fWFDEMR1L  if   e           k disk
004200140820     f                                     extfile(wLibFile3)
004300140820     f                                     usropn
004400140820
004500140820       // -?Informazioni commerciali clienti Potenziali?
004600140820     fTICPI01L  if   e           k disk
004700140820
004800140820       // -?Organigramma?
004900140820     fAZORG01L  if   e           k disk
005000140818
005100140818       // -?Anagrafica Clienti Potenziali?
005200140820     fTNCPO01L  Uf   e           k disk
005300140820     fTNCPO11L  Uf A e           k disk
005400140818
005500140818       // -?Stampa segnalazioni di errore?
005600140818     fQSYSPRT   o    f  132        printer
005700140818     f                                     usropn
005800140818     f                                     oflind(*inOF)
005900140820
006000140820       // -?Stampa dettaglio potenziali elaborati?
006100140820     fQSYSPRT1  o    f  132        printer
006200140820     f                                     usropn
006300140820     f                                     oflind(*inOA)
006400140818
006500140818       //--------------------------------------------------------------
006600140818       //?Definizione costanti.                                        ?
006700140818       //--------------------------------------------------------------
006800140818
006900140820       // -?Limite fatturato per "piccole imprese"?
007000140820     d c_PiccoleAL     c                   const(10000000)
007100140820       // -?Limite fatturato per "medie imprese"?
007200140820     d c_MedieAL       c                   const(50000000)
007300140820
007400140820       // -?Costanti in stampa (forse)?
007500140820     d c_SpesaTra      c                   const('Spesa Trasp. Presunta:')
007600140818
007700140818       //--------------------------------------------------------------
007800140818       //?Definizione schiere.                                         ?
007900140818       //--------------------------------------------------------------
008000140818
008100140818
008200140818       //--------------------------------------------------------------
008300140818       //?Definizione aree dati.                                       ?
008400140818       //--------------------------------------------------------------
008500140818
008600140818       // -?Dati utente?
008700140818     d §AzUte        e ds                  extname(AZUTE00F)
008800140818     d                                     dtaara
008900140818     d §DatiUte      e ds                  extname(dDatiUte)
009000140818     d                                     dtaara
009100140818
009200140818       //--------------------------------------------------------------
009300140818       //?Definizione strutture dati.                                  ?
009400140818       //--------------------------------------------------------------
009500140818
009600140818       // -?Status ds?
009700140818     d Status         sds
009800140818     d   SDSpgmName      *proc
009900140818     d*//SDSparms        *parms
010000140818     d   SDSjobName          244    253                                         Job name
010100140818     d   SDSjobUser          254    263                                         User name
010200140818     d*//SDSjobNumber        264    269s 0                                      Job number
010300140818
010400140820       // -?Parametri ricevuti?
010500140820     d KPJBA         e ds
010600140819
010700140820       // -?DS per campo CPORST di TNCPO00F
010800140820     d dCPO01        e ds                  inz  qualified
010900140818
011000140818       //--------------------------------------------------------------
011100140818       //?Definizione variabili globali.                               ?
011200140818       //--------------------------------------------------------------
011300140818
011400140818       // -?Nome esteso Libreria/File del file da elaborare?
011500140819     d wLibFile1       s             21a   inz
011600140819     d wLibFile2       s             21a   inz
011700140820     d wLibFile3       s             21a   inz
011800140818
011900140818       // -?Flags Booleani?
012000140819     d $UpdTNCPO       s               n   inz
012100140818
012200140818       // -?Campi di comodo?
012300140818     d wDate_Iso       s               d   inz  datfmt(*iso)
012400140818     d wDate_Eur       s               d   inz  datfmt(*eur)
012500140818     d wDate           s              8  0 inz
012600140818     d wTime           s              6  0 inz
012700140818     d wErr            s              3  0 inz
012800140819     d wSituazione     s              1  0 inz
012900140820     d w255a           s            255    inz
013000140820
013100140820       // -?Campi per la subr. sr_AGGIOindirizzo?
013200140820     d wSAVcap         s                   like(CPOcap)  inz
013300140820     d wSAVprv         s                   like(CPOprv)  inz
013400140820
013500140820       // -?Campi per la subr. sr_CalcoloSpesaPresunta?
013600140820     d wDEMfil         s              3  0 inz
013700140820     d wDEMare         s              3  0 inz
013800140820     d wDEMtotFat      s             15  0 inz
013900140820     d wPerc           s              5  2 inz
014000140820     d wSpesaTra       s             11  0 inz
014100140820     d w0080           s              8  0 inz
014200140820     d wAlfaSCT        s              5    inz
014300140820
014400140820       // -?Campi di stampa?
014500140821     d s_Aggior        s             12    inz('AGGIORNABILI')
014600140820     d s_SpesaTra      s             40    inz
014700140826     d s_CatMercERR    s             26    inz
014800140818
014900140818       //--------------------------------------------------------------
015000140818       //?Definizione prototipi procedure.                             ?
015100140818       //--------------------------------------------------------------
015200140818
015300140818       // -?Reperimento dati utente?
015400140818     d TIBS34ds      e ds                  inz
015500140818      /copy gaitrasrc/srcProtoPR,TIBS34R
015600140820
015700140820       // -?Controllo CAP ("Pretrullo TISI95R")?
015800140820     d FNLV13ds      e ds                  inz
015900140820     d TISI95ds      e ds                  inz
016000140820     d fnlv13r         pr                  extpgm('FNLV13R')
016100140820     d   kpjba                             likeds(KPJBA)
016200140820     d   fnlv13ds                          likeds(FNLV13ds)
016300140820     d   tisi95ds                          likeds(TISI95ds)
016400140818
016500140819       // -?API QCAPCMD (Process Commands)?
016600140819     d Qcmd            s           2048    inz  varying
016700140818      /copy qSysInc/qRpgleSrc,QCAPCMD
016800140818      /copy gaitrasrc/srcProtoPR,QCAPCMD
016900140818
017000140818       // -?Parametri gestione errori API.?
017100140818      /copy qsysinc/qrpglesrc,QUSEC
017200140818
017300140818       //--------------------------------------------------------------
017400140818       //?Definizione key-list.                                        ?
017500140818       //--------------------------------------------------------------
017600140818
017700140818
017800140818       //--------------------------------------------------------------
017900140818       //?Riepilogo indicatori utilizzati.                             ?
018000140818       //--------------------------------------------------------------
018100140818
018200140818
018300140818       //--------------------------------------------------------------
018400140818       //?M A I N - L I N E                                            ?
018500140818       //--------------------------------------------------------------
018600140818
018700140818     c     *Entry        plist
018800140820     c                   parm                    KPJBA
018900140818
019000140818      /free
019100140818
019200140818       // -?Operazioni iniziali?
019300140818       exsr sr_RoutInz;
019400140818
019500140818       // -?Ciclo di lettura file WFACP00F?
019600140818       read  WFACP000;
019700140818
019800140818       DoW  Not %eof(WFACP00F);
019900140818
020000140818         exsr  sr_ElabPot;
020100140818
020200140818         read  WFACP000;
020300140818
020400140818       EndDo;
020500140818
020600140818       // -?Operazioni finali?
020700140818       exsr sr_RoutEnd;
020800140818
020900140818       //--------------------------------------------------------------
021000140818       //?Operazioni iniziali.                                         ?
021100140818       //--------------------------------------------------------------
021200140819
021300140818       BEGSR  sr_RoutInz;
021400140818
021500140818         // -?Impostazione chiusura?
021600140818         *inLR = *on;
021700140818
021800140820         // -?Ricezione parametri?
021900140820         $UpdTNCPO  = ( %subst( KPJBU : 1 : 3 ) = 'A  ' );
022000140820         if  $UpdTNCPO;
022100140821           s_Aggior = 'AGGIORNATI  ';
022200140820         endif;
022300140818
022400140820         // -?Reperimento data odierna in formato aaaa/mm/gg?
022500140818         wDate_Iso = %date();
022600140820         // -?Conversione data odierna in formato gg/mm/aaaa?
022700140818         wDate_Eur = wDate_Iso;
022800140818         wDate = %dec( wDate_Eur );
022900140818
023000140819         // -?Reperimento orario (hh:mm:ss)?
023100140818         wTime = %dec( %time() );
023200140818
023300140818         // -?Reperimento dati job?
023400140818         exsr  sr_DatiJob;
023500140818
023600140820         // -?Impostazione librerie dei work-file di input:?
023700140820         //  ?1) WFACP00F?
023800140820         //  ?2) WATECO1L?
023900140820         //  ?3) WFDEMR1L?
024000140818         if  %subst( knsif : 7 : 1 ) = 'P';
024100140819           wLibFile1 = '*LIBL';
024200140820           wLibFile3 = 'GAITRAAZP';
024300140818         else;
024400140819           wLibFile1 = 'EDPCRIBIS';
024500140820           wLibFile3 = 'GAITRAAZM';
024600140818         endif;
024700140819         wLibFile2 = wLibFile1;
024800140818
024900140820         // -?Apertura del work-file di input WFACP00F?
025000140826         //wLibFile1 = %trimR( wLibFile1 ) + '/WFACP00F';
025100140826         wLibFile1 = %trimR( wLibFile1 ) + '/WFACPV0F';
025200140818         open(e)  WFACP00F;
025300140818         if  %error();
025400140818           wErr = 1;
025500140818           exsr  sr_PrintErr;
025600140818           exsr  sr_RoutEnd;
025700140818         endif;
025800140820
025900140820         // -?Apertura del work-file di input WATECO1L?
026000140820         wLibFile2 = %trimR( wLibFile2 ) + '/WATECO1L';
026100140819         open(e)  WATECO1L;
026200140819         if  %error();
026300140819           wErr = 2;
026400140819           exsr  sr_PrintErr;
026500140819           exsr  sr_RoutEnd;
026600140819         endif;
026700140820
026800140820         // -?Apertura del work-file di input WFDEMR1L?
026900140820         wLibFile3 = %trimR( wLibFile3 ) + '/WFDEMR1L';
027000140820         open(e)  WFDEMR1L;
027100140820         if  %error();
027200140820           wErr = 3;
027300140820           exsr  sr_PrintErr;
027400140820           exsr  sr_RoutEnd;
027500140820         endif;
027600140818
027700140818       ENDSR;
027800140818
027900140818       //--------------------------------------------------------------
028000140818       //?Reperimento Dati del job (Utente/Operativi).                 ?
028100140818       //--------------------------------------------------------------
028200140818       BEGSR  sr_DatiJob;
028300140818
028400140818         in(e) §AzUte;
028500140818         if NOT %error;
028600140818           in(e) §DatiUte;
028700140818         endif;
028800140818         if %error or RSut = *blank;
028900140818           tibs34r ( tibs34ds );
029000140818           in §AzUte;
029100140818           in §DatiUte;
029200140818         endif;
029300140818
029400140818       ENDSR;
029500140818
029600140818       //--------------------------------------------------------------
029700140818       //?Elaborazione singolo cliente Potenziale.                     ?
029800140818       //--------------------------------------------------------------
029900140818       BEGSR  sr_ElabPot;
030000140819
030100140820         // -?Verifica SE cliente da aggiornare?
030200140819         exsr  sr_Verifica;
030300140820
030400140826         // -?I dati sono già stati filtrati in WFACPV0F:?
030500140826         //  ?questo test servirebbe SE si leggesse WFACP00F?
030600140826         //if  wSituazione = *zero;
030700140826         //  leavesr;
030800140826         //endif;
030900140820
031000140820         // -?Valorizzazione campi in TNCPO (00F/10F)?
031100140820         //  ?(comodo comunque per la stampa)?
031200140820         exsr  sr_ImpostaTNCPO;
031300140820
031400140820         // -?Stampa?
031500140820         exsr  sr_PrintCliPot;
031600140820
031700140820         // -?Aggiornamento TNCPO (se richiesto)?
031800140820         if  $UpdTNCPO;
031900140820           exsr  sr_UpdateTNCPO;
032000140820         endif;
032100140818
032200140818       ENDSR;
032300140819
032400140819       //--------------------------------------------------------------
032500140820       //?Verifica SE cliente da aggiornare.                           ?
032600140819       //--------------------------------------------------------------
032700140819       BEGSR  sr_Verifica;
032800140820
032900140820         clear  wSituazione;
033000140820
033100140820         Select;
033200140820
033300140820           // -?Situazione 1?
033400140820           //  ?(incrocio per Ragione Sociale)?
033500140820           When  WAPconf <> *blank  and  WAPconf >= '07'  and
033600140820                 WAPsimN >= 30      and  WAPsimA >= 30;
033700140820             wSituazione  = 1;
033800140820
033900140820           // -?Situazione 2?
034000140820           //  ?(incrocio per Partita Iva / Codice Fiscale)?
034100140820           When  WAPconf  = *blank  and
034200140820                 WAPmgrN >= 30      and  WAPmgrA >= 30;
034300140820             wSituazione  = 2;
034400140820
034500140820         EndSl;
034600140820
034700140820
034800140820         // -?SE cliente da NON aggiornare => uscita?
034900140826         //  ?(i dati sono già stati filtrati in WFACPV0F)?
035000140826         //if  wSituazione = *zero;
035100140826         //  leavesr;
035200140826         //endif;
035300140820
035400140819
035500140819         // -?Reperimento dati anagrafici da aggiornare?
035600140819         if  $UpdTNCPO;
035700140819           chain  ( WAPcpo )  TNCPO000;
035800140820           chain  ( WAPcpo )  TNCPO100;
035900140819         else;
036000140819           chain(N)  ( WAPcpo )  TNCPO000;
036100140820           chain(N)  ( WAPcpo )  TNCPO100;
036200140820         endif;
036300140819
036400140820         // -?SE cliente NON reperito in TNCPO00F => uscita?
036500140819         if  Not %found(TNCPO01L);
036600140820           wErr = 4;
036700140819           exsr  sr_PrintErr;
036800140819           clear  wSituazione;
036900140819           leavesr;
037000140819         endif;
037100140819
037200140820         // -?SE cliente NON reperito in TNCPO10F =>?
037300140820         //  ?rec. da aggiungere (WRITE)?
037400140820         if  Not %found(TNCPO11L)  and  $UpdTNCPO;
037500140819           clear  TNCPO100;
037600140819           CPO1cpo = CPOcpo;
037700140819         endif;
037800140819
037900140819         // -?Spesa Trasporti Presunta?
038000140819         dCPO01 = CPOrst;
038100140820         setll  ( WAPcpo : 'SPT' : *blank )  TICPI000;
038200140820         //if  Not %equal(TICPI01L);
038300140820         //  exsr  sr_CalcoloSpesaPresunta;  ? subr. eseguita DOPO ! ?
038400140819         //endif;
038500140819
038600140819         // -?Reperimento categoria merceologica?
038700140819         chain  ( WAPateco )  WATECO00;
038800140819
038900140820         // -?SE categoria merceologica mancante =>?
039000140820         //  ?segnalazione in stampa  &  NO aggiornamento?
039100140819         if  Not %found(WATECO1L);
039200140820           wErr = 5;
039300140819           exsr  sr_PrintErr;
039400140819           //clear  wSituazione;
039500140819           //leavesr;
039600140819           clear  WAPateco;
039700140819           clear  WATbCatM;
039800140819         endif;
039900140819
040000140819       ENDSR;
040100140820
040200140820       //--------------------------------------------------------------
040300140820       //?Impostazione nuovi dati nei campi di TNCPO00F e TNCPO10F.    ?
040400140820       //--------------------------------------------------------------
040500140820       BEGSR  sr_ImpostaTNCPO;
040600140820
040700140820         // -?Impostazione dati nel file TNCPO00F?
040800140820
040900140825         // -?Codice Potenziale?
041000140820         // -?Ragione Sociale?
041100140820         CPOrag  = WAPdeno;
041200140825         // -?Indirizzo / Località / CAP / Provincia?
041300140820         exsr  sr_AggioINDIRIZZO;
041400140820         // -?Telefono (ora in TNCPO10F)?
041500140820         //*CPOtel  = WAPtel2;
041600140820         // -?Fax (ora in TNCPO10F)?
041700140820         //*CPOfax  = WAPfax2;
041800140821         // -?Anno Inizio Attività?       ? dove??? va bene qui??? ?
041900140820         //CPOtel  = %int( %subst( %editc( WAPdtIni : 'X' ) : 5 : 4 ) );
042000140820         // -?Categoria Merceologica (categoria SEAT)?
042100140820         if  WAPateco <> *blank  and  WATbCatM > *zero;
042200140820           CPOsct  =  WATbCatM;
042300140820         endif;
042400140825         // -?Codice Importanza Cliente?
042500140821         // -?Codice Commerciale?
042600140821         // -?Categoria Potenziale?
042700140820         // -?Azienda in Difficoltà Finanziarie?
042800140820         %subst( CPOclt : 1 : 1 ) = WAPfr4;
042900140820         // -?Flag E-Commerce?
043000140820         if  WAPeCom = 'Y';
043100140820           %subst( CPOclt : 2 : 1 ) = WAPeCom;  //? va bene qui/così??? ?
043200140820         endif;
043300140821         // -?Data Acquisizione Potenziale?
043400140820         // -?Codice Fiscale?
043500140829         if  CPOcdf  =  *blank;
043600140829           CPOcdf  =  WAPcfi2;
043700140820         endif;
043800140820         // -?Partita Iva?
043900140820         if  CPOpiv  =  *blank;
044000140829           CPOpiv  =  WAPcpi2;
044100140820         endif;
044200140820         // -?Fatturato Azienda (migliaia di Euro)?
044300140820         eval(H)  CPOftAz = WAPfatt / 1000;
044400140820         // -?Anno Fatturato Azienda?
044500140820         CPOaFAz = WAPaaFt;
044600140825         // -?Flag Fatturato Azienda (S-Stimato, D-Dichiarato)?
044700140825         if  WAPpvnFt <> *blank;
044800140825           CPOfFAz = WAPpvnFt;
044900140825         endif;
045000140820         // -?% Export?
045100140820         if  WAPpExp > *zero;
045200140820           CPOpExp = WAPpExp;
045300140820         else;
045400140820           CPOpExp = WAPpImpEx;
045500140820         endif;
045600140820
045700140820         // -?Spesa Trasporti Presunta?
045800140820         //dCPO01 = CPOrst;   ? dati già reperiti nella subr. sr_Verifica ?
045900140820         //setll  ( WAPcpo : 'SPT' : *blank )  TICPI000;
046000140820         if  Not %equal(TICPI01L);
046100140820           exsr  sr_CalcoloSpesaPresunta;
046200140820           dCPO01.§CPOspTp = wSpesaTra;
046300140820         endif;
046400140820
046500140820         CPOrst = dCPO01;
046600140821
046700140821         // -?Flag Sede/Filiale?
046800140821         // -?Codice DUNS?
046900140825         // -?Filiale di Appartenenza (Trasmissione Dati)?
047000140821         // -?Codice Importanza Cliente (A-B-C-T-D)?
047100140821         // -?Data Trasmissione?
047200140825         CPOdtr = %dec( wDate_Iso );
047300140820
047400140820         // -?Impostazione dati nel file TNCPO10F?
047500140820
047600140820         // -?Telefono?
047700140820         CPO1tel = WAPtel2;
047800140820         // -?Fax?
047900140820         CPO1fax = WAPfax2;
048000140820         // -?Crif Number?
048100140820         CPO1crifN  = WAPcrif;
048200140820         // -?Crif Number Casa Madre?
048300140820         CPO1crifNM = WAPcmCrif;
048400140820
048500140820       ENDSR;
048600140819
048700140819       //--------------------------------------------------------------
048800140819       //?Aggiornamento indirizzo.                                     ?
048900140819       //--------------------------------------------------------------
049000140819       BEGSR  sr_AGGIOindirizzo;
049100140821
049200140821         clear  wSAVcap;
049300140821         clear  wSAVprv;
049400140819
049500140819         // -?SE manca il CAP: tengo quello presente SE stessa provincia?
049600140819         if  WAPcap2 = *blank  or  WAPcap2 = *zero;
049700140820           wSAVcap = CPOcap;
049800140820           wSAVprv = CPOprv;
049900140819         endif;
050000140819
050100140821         CPOvia = WAPind2;
050200140819         CPOcap = WAPcap2;
050300140819         CPOprv = WAPprv2;
050400140819
050500140819         IF  WAPfra2 = *blank  or  WAPfra2 = WAPloc2;
050600140819
050700140819           CPOcit = WAPloc2;
050800140819
050900140819         ELSE;
051000140819
051100140819           // -?Verifica SE frazione corretta --> al posto della località?
051200140819           clear  TISI95ds;
051300140819           clear  FNLV13ds;
051400140819           I95tcn = '7';
051500140819           I95loc = WAPfra2;
051600140819           I95prv = WAPprv2;
051700140825           if  (WAPreg2 = 'SAN MARINO'                and
051800140825                WAPprv2 = *blank)                     OR
051900140825               (WAPloc2 = 'REPUBBLICA DI SAN MARINO'  and
052000140825                WAPprv2 = *blank);
052100140825             WAPprv2 = 'RN';
052200140825             I95prv  = 'RN';
052300140825           endif;
052400140819           I95dat = %dec( wDate_Iso );
052500140822           I13la3 = 'S';
052600140819           I13af0 = 'S';
052700140819           //I13cnv = *blank;
052800140819           //I13af1 = *blank;
052900140819
053000140819           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
053100140819
053200140820           If  O95err = *blank;
053300140819
053400140819             CPOcit = O95loc;
053500140819             CPOcap = O95cap;
053600140819             CPOprv = O95prv;
053700140819
053800140819           Else;
053900140819
054000140820             clear  w255a;
054100140820             w255a = %trim(WAPind2) + '-' + %trim(WAPfra2);
054200140820             //if  %subst( w255a : 36 ) = *blank;
054300140820             if  %subst( w255a : %size( CPOvia ) + 1 ) = *blank;
054400140820               CPOvia = w255a;
054500140819               CPOcit = WAPloc2;
054600140819             else;
054700140819               CPOcit = WAPloc2;
054800140819             endif;
054900140819
055000140819           EndIf;
055100140819
055200140819         ENDIF;
055300140819
055400140819
055500140819         IF  CPOcap = *blank  or  CPOcap = '00000    ';
055600140819
055700140819           clear  TISI95ds;
055800140819           clear  FNLV13ds;
055900140819           I95tcn = '7';
056000140819           I95loc = CPOcit;
056100140819           I95prv = CPOprv;
056200140825           if  (WAPreg2 = 'SAN MARINO'                and
056300140825                WAPprv2 = *blank)                     OR
056400140825               (WAPloc2 = 'REPUBBLICA DI SAN MARINO'  and
056500140825                WAPprv2 = *blank);
056600140825             WAPprv2 = 'RN';
056700140825             I95prv  = 'RN';
056800140825           endif;
056900140819           I95dat = %dec( wDate_Iso );
057000140822           I13la3 = 'S';
057100140822           I13af0 = 'S';
057200140819           //I13cnv = *blank;
057300140819           //I13af1 = *blank;
057400140819
057500140819           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
057600140819
057700140819           If  O95err = *blank;
057800140819
057900140819             CPOcap = O95cap;
058000140819             CPOprv = O95prv;
058100140819
058200140819           Else;
058300140819
058400140820             if  wSAVprv = CPOprv;
058500140820               CPOcap = wSAVcap;
058600140819             endif;
058700140819
058800140819           EndIf;
058900140819
059000140819         ENDIF;
059100140819
059200140819       ENDSR;
059300140819
059400140819       //--------------------------------------------------------------
059500140819       //?Calcolo Spesa Presunta.                                      ?
059600140819       //--------------------------------------------------------------
059700140820       BEGSR  sr_CalcoloSpesaPresunta;
059800140820
059900140820         clear  wDEMfil;
060000140820         clear  wDEMare;
060100140820         clear  wDEMtotFat;
060200140820         clear  wAlfaSCT;
060300140820         clear  wPerc;
060400140820         clear  wSpesaTra;
060500140819
060600140819         // -?SE non c'è un Fatturato Aziendale: NON si calcola nulla?
060700140819         if  CPOftAz <= *zero;
060800140819           leavesr;
060900140819         endif;
061000140819
061100140820         wDEMtotFat = CPOftAz * 1000;
061200140820
061300140820         wAlfaSCT = %editc( CPOsct : 'X' );
061400140819
061500140819         // -?SE presente la nazione, imposto la filiale del codice?
061600140819         //  ?e NON da cappario?
061700140819         If  CPOnaz <> *blank;
061800140819
061900140820           wDEMfil = CPOflt;
062000140819
062100140819         Else;
062200140819
062300140819           clear  TISI95ds;
062400140819           clear  FNLV13ds;
062500140819           I95tcn = '7';
062600140819           I95cap = CPOcap;
062700140819           I95loc = CPOcit;
062800140819           I95prv = CPOprv;
062900140819           I95nar = CPOnaz;
063000140819           I95dat = %dec( wDate_Iso );
063100140822           I13la3 = 'S';
063200140819           I13af0 = 'S';
063300140819           //I13cnv = *blank;
063400140819           //I13af1 = *blank;
063500140819           if  %subst( I95cap : 1 : 2 ) = '47'  and  I95prv = *blank;
063600140819             I95prv = 'RN';
063700140819           endif  ;
063800140819
063900140819           FNLV13R  ( KPJBA : FNLV13ds : TISI95ds );
064000140819
064100140820           wDEMfil = O95lna;
064200140819
064300140819         EndIf;
064400140819
064500140820         // -?SE trovata filiale: impostazione area?
064600140820         if  wDEMfil > *zero;
064700140820           chain  ( wDEMfil )  AZORG;
064800140819           if  %found(AZORG01L);
064900140820             wDEMare = ORGcar;
065000140819           endif;
065100140820         endif;
065200140819
065300140819
065400140820         // -?Reperimento area di appartenenza dall'indirizzo?
065500140820         chain  ( wAlfaSCT : wDEMare )  WFDEMR00;
065600140820         if  %found(WFDEMR1L);
065700140819           exsr  sr_ScegliP;
065800140820         endif ;
065900140819
066000140820         if  wPerc = *zero;
066100140820           clear  wDEMare;
066200140820           chain  ( wAlfaSCT : 000 )  WFDEMR00;
066300140819           if  %found(WFDEMR1L);
066400140819             exsr  sr_ScegliP;
066500140819           endif;
066600140820         endif;
066700140819
066800140820         If  wPerc > *zero;
066900140820           eval(H)  wSpesaTra = ( wDEMtotFat * wPerc ) / 100;
067000140820           // -?Arrotondamento ai 1000 € superiori?
067100140820           w0080 = wSpesaTra / 1000;
067200140819           w0080 = w0080 + 1;
067300140820           wSpesaTra = w0080 * 1000;
067400140820         EndIf;
067500140819
067600140819       ENDSR;
067700140819
067800140819       //--------------------------------------------------------------
067900140820       //?Reperimento percentuale per calcolare la spesa presunta.     ?
068000140820       //--------------------------------------------------------------
068100140819       BEGSR  sr_ScegliP;
068200140820
068300140819         select;
068400140819
068500140820           when  wDEMtotFat <= c_PiccoleAL;
068600140820             if  DEMpCli > 9  or  wDEMare = *zero;
068700140820               wPerc = DEMpPerc;
068800140819             endif;
068900140819
069000140820           when  wDEMtotfat <= c_MedieAL;
069100140820             if  DEMmCli > 9  or  wDEMare = *zero;
069200140820               wPerc = DEMmPerc;
069300140819             endif;
069400140819
069500140819           other;
069600140820             if  DEMgCli > 9  or  wDEMare = *zero;
069700140820               wPerc = DEMgPerc;
069800140819             endif;
069900140819
070000140819         endsl;
070100140819
070200140819       ENDSR;
070300140820
070400140820       //--------------------------------------------------------------
070500140820       //?Aggiornamento dati nei file TNCPIO00F e TNCPO10F.            ?
070600140820       //--------------------------------------------------------------
070700140820       BEGSR  sr_UpdateTNCPO;
070800140820
070900140820         // -?Aggiornamento rec. nel file TNCPO00F?
071000140820         //_______________
071100140820         update  TNCPO000;
071200140820         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071300140820
071400140820         // -?Aggiornamento/Scrittura rec. nel file TNCPO10F?
071500140820         if  %found(TNCPO11L);
071600140820           //_______________
071700140820           update  TNCPO100;
071800140820           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071900140820         else;
072000140820           //______________
072100140820           write  TNCPO100;
072200140820           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
072300140820         endif;
072400140820
072500140820       ENDSR;
072600140820
072700140820       //--------------------------------------------------------------
072800140820       //?Stampa dati del cliente potenziale da aggiornare.            ?
072900140820       //--------------------------------------------------------------
073000140820       BEGSR  sr_PrintCliPot;
073100140820
073200140820         // -?Impostazione campi di stampa?
073300140820         //  ?(con lunghezza diversa da quella dei campi "originali")?
073400140820         // ·?Spesa Trasporti Presunta?
073500140820         //  ?(SE NON c'è la spesa reale delle Info-Commerciali,?
073600140820         //  ? SE c'è un Fatturato Aziendale  e?
073700140820         //  ? SE è stata calcolcolata)?
073800140820         clear  s_SpesaTra;
073900140821         if  Not %equal(TICPI01L)  and  CPOftAz > *zero  and
074000140820             wSpesaTra > *zero;
074100140820           s_SpesaTra  = c_SpesaTra + ' ' + %editc( wSpesaTra : '1' );
074200140820         endif;
074300140826         // ·?Categoria Merceologica ERRATA?
074400140826         clear  s_CatMercERR;
074500140826         if  wErr = 5;
074600140826           s_CatMercERR = 'Cat.Merc.ERRATA: ' + WAPateco;
074700140826         endif;
074800140820
074900140820         // -?Apertura del printer-file?
075000140820         if  Not %open(QSYSPRT1);
075100140820           Qcmd = 'OVRPRTF file(QSYSPRT1) +
075200140820                           usrdta(''Agg. TNCPO'') +
075300140820                           ovrscope(*actgrpdfn)';
075400140820           exsr  sr_ExecCmd;
075500140820           open  QSYSPRT1;
075600140820           except  PRT1txt;
075700140820         endif;
075800140820
075900140820         if  *inOA;
076000140820           except  PRT1txt;
076100140820           *inOA = *off;
076200140820         endif;
076300140820
076400140820         except  PRT1det;
076500140820
076600140820       ENDSR;
076700140818
076800140818       //--------------------------------------------------------------
076900140818       //?Stampa segnalazione dell'errore rilevato.                    ?
077000140818       //--------------------------------------------------------------
077100140818       BEGSR  sr_PrintErr;
077200140818
077300140818         // -?Apertura del printer-file?
077400140818         if  Not %open(QSYSPRT);
077500140818           Qcmd = 'OVRPRTF file(QSYSPRT) +
077600140818                           usrdta(''ACPO06*ERR'') +
077700140818                           ovrscope(*actgrpdfn)';
077800140818           exsr  sr_ExecCmd;
077900140818           open  QSYSPRT;
078000140818           except  PRTtxt;
078100140818         endif;
078200140818
078300140818         if  *inOF;
078400140818           except  PRTtxt;
078500140818           *inOF = *off;
078600140818         endif;
078700140818
078800140819         select;
078900140819           when  wErr = 1;
079000140818             except  PrtErr1;
079100140819           when  wErr = 2;
079200140818             except  PrtErr2;
079300140819           when  wErr = 3;
079400140818             except  PrtErr3;
079500140819           when  wErr = 4;
079600140819             except  PrtErr4;
079700140820           when  wErr = 5;
079800140820             except  PrtErr5;
079900140819         endsl;
080000140818
080100140818       ENDSR;
080200140818
080300140818       //--------------------------------------------------------------
080400140818       //?Operazioni finali.                                           ?
080500140818       //--------------------------------------------------------------
080600140818       BEGSR  sr_RoutEnd;
080700140818
080800140818         // -?Chiusura spool di stampa errori?
080900140818         if  %open(QSYSPRT);
081000140818           except  PrtEND;
081100140818           close  QSYSPRT;
081200140818         endif;
081300140820
081400140820         // -?Chiusura spool di stampa?
081500140820         if  %open(QSYSPRT1);
081600140820           except  Prt1END;
081700140820           close  QSYSPRT1;
081800140820         endif;
081900140818
082000140820         // -?Chiusura Work-File di input?
082100140818         if  %open(WFACP00F);
082200140818           close  WFACP00F;
082300140818         endif;
082400140820         if  %open(WATECO1L);
082500140820           close  WATECO1L;
082600140820         endif;
082700140820         if  %open(WFDEMR1L);
082800140820           close  WFDEMR1L;
082900140820         endif;
083000140818
083100140818         // -?Chiusura *pgm?
083200140818         return;
083300140818
083400140818       ENDSR;
083500140818
083600140818       //--------------------------------------------------------------
083700140818       //?Esecuzione del comando (già impostato).                      ?
083800140818       //--------------------------------------------------------------
083900140818       BEGSR  sr_ExecCmd;
084000140818
084100140818         clear Qcap0100;
084200140818         Qcabcsdh = *off;
084300140818         Qcapa    = *off;
084400140818         Qcacmdss = *off;
084500140818         Qcaerved = *allX'00';
084600140818
084700140818         clear Qusec;
084800140818         Qusbprv  = %size(Qusec);
084900140818
085000140818         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
085100140818                           %size(Qcap0100) : 'CPOP0100' : *omit :
085200140818                           0 : 0 : Qusec);
085300140818
085400140818         //if  Qusei <> *blank;
085500140818         //  ...;
085600140818         //endif;
085700140818
085800140818       ENDSR;
085900140818
086000140818      /end-free
086100140818
086200140818       //--------------------------------------------------------------
086300140818       //?S P O O L - F I L E S                                        ?
086400140818       //--------------------------------------------------------------
086500140818
086600140820       // -?Stampa segnalazioni di errore?
086700140820     oQSYSPRT   e            PRTtxt            2
086800140818     o                       RSUT
086900140818     o                                        +   4 'LISTA ERRORI RILE-
087000140818     o                                              VATI IN FASE DI EL-
087100140818     o                                              ABORAZIONE DATI NE-
087200140818     o                                              L FILE'
087300140820     o                       wLibFile1        +   1
087400140818     o                       SDSpgmName       +   4
087500140818     o                       *Date         y  +   2
087600140818     o          e            PRTtxt      0
087700140818     o                                          +24 'LISTA ERRORI RILE-
087800140818     o                                              VATI IN FASE DI EL-
087900140818     o                                              ABORAZIONE DATI NE-
088000140818     o                                              L FILE'
088100140820     o                       wLibFile1        +   1
088200140818     o          e            PRTtxt      1
088300140818     o                       KNSIF
088400140818     o                       KNMUS            +   1
088500140818     o                                        +   3 '------------------
088600140818     o                                              -------------------
088700140818     o                                              -------------------
088800140818     o                                              -------------------
088900140818     o                                              ----------'
089000140818     o                                        +   4 'Pag.'
089100140818     o                       Page          z  +   0
089200140818     o                       wTime            +   4 '  :  :  '
089300140818      *
089400140818     o          e            PRTtxt      2
089500140818     o                                              'Potenziale '
089600140818     o                                        +   2 'Messaggio'
089700140818     o          e            PRTtxt      1
089800140818     o                                              '-----------'
089900140818     o                                        +   2 '---------'
090000140818      *
090100140818     o          e            PRTerr1     2
090200140818     o                                              'NON TROVATO IL +
090300140818     o                                               FILE'
090400140819     o                       wLibFile1        +   1
090500140819      *
090600140819     o          e            PRTerr2     2
090700140819     o                                              'NON TROVATO IL +
090800140819     o                                               FILE'
090900140819     o                       wLibFile2        +   1
091000140820      *
091100140820     o          e            PRTerr3     2
091200140820     o                                              'NON TROVATO IL +
091300140820     o                                               FILE'
091400140820     o                       wLibFile3        +   1
091500140818      *
091600140820     o          e            PRTerr4     2
091700140819     o                       WAPcpo
091800140819     o                                        +   2 'Potenziale NON tr-
091900140819     o                                              ovato in anagrafic-
092000140819     o                                              a potenziali (TNCP-
092100140819     o                                              O00F).'
092200140819      *
092300140820     o          e            PRTerr5     2
092400140819     o                       WAPcpo
092500140819     o                                        +   2 'Categoria merceol-
092600140819     o                                              ogica'
092700140819     o                       WAPateco         +   1
092800140819     o                                        +   1 'NON reperita nell-
092900140819     o                                              a tabella WATECO0F-
093000140819     o                                              .'
093100140819      *
093200140819     o          e            PRTend      2
093300140819     o                                        +  24 '***  Fine Lista  ***'
093400140819     o          e            PRTend      0
093500140819     o                                        +  24 '***  Fine Lista  ***'
093600140820
093700140820       // -?Stampa dettaglio potenziali elaborati?
093800140820     oQSYSPRT1  e            PRT1txt           2
093900140820     o                       RSUT
094000140820     o                                        +   4 'LISTA POTENZIALI'
094100140821     o                       s_Aggior         +   1
094200140820     o                       SDSpgmName       +   4
094300140820     o                       *Date         y  +   2
094400140820     o          e            PRT1txt     0
094500140820     o                                          +24 'LISTA POTENZIALI'
094600140821     o                       s_Aggior         +   1
094700140820     o          e            PRT1txt     1
094800140820     o                       KNSIF
094900140820     o                       KNMUS            +   1
095000140820     o                                        +   3 '------------------
095100140820     o                                              ------------'
095200140820     o                                        +   4 'Pag.'
095300140820     o                       Page2         z  +   0
095400140820     o                       wTime            +   4 '  :  :  '
095500140820      *
095600140820     o          e            PRT1txt     2
095700140820     o                                              'Potenziale '
095800140820     o          e            PRT1txt     1  1
095900140820     o                                              '-----------'
096000140820      *
096100140820     o          e            PRT1det     1
096200140820     o                       WAPcpo
096300140820     o                       CPOrag           +   1
096400140820     o                                        +   3 'Situazione:'
096500140820     o                       wSituazione      +   1
096600140826     o                       s_CatMercERR     +   3
096700140820     o          e            PRT1det     1
096800140820     o                       CPOcap           +  12
096900140820     o                       CPOcit           +   1
097000140820     o                       CPOprv           +   1
097100140820     o                       s_SpesaTra       +   3
097200140820      *
097300140820     o          e            PRT1end     2
097400140820     o                                        +  24 '***  Fine Lista  ***'
097500140820     o          e            PRT1end     0
097600140820     o                                        +  24 '***  Fine Lista  ***'
