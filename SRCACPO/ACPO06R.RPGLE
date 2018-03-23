000100140818       //==============================================================
000200140818       //?Acquisizione Potenziali: lettura del file WFACP00F per       ?
000300140818       //?             AGGIORNAMENTO  dati nel file TNCPO00F.          ?
000400141002       //?-------------------------------------------------------------?
000500141002       //?SE lanciato per la verifica: stampa, ma NON aggiorna.        ?
000600141002       //?SE lanciato per l'aggiornamento: NON stampa, ma aggiorna.    ?
000700140818       //==============================================================
000800140818
000900140818       //--------------------------------------------------------------
001000140818       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001100140818       //--------------------------------------------------------------
001200140818
001300140818     /*PRM  dbgview(*source)
001400140826     /*CMD  ovrdbf file(WFACP00F) tofile(*LIBL/WFACPV0F) +
001500140826     /*CMD         ovrscope(*calllvl)
001600140820     /*CMD  ovrdbf file(WFDEMR1L) tofile(GAITRAAZP/WFDEMR1L) +
001700140820     /*CMD         ovrscope(*calllvl)
001800140826     /*END  dltovr file(WFACP00F WFDEMR1L) lvl(*)
001900140818     /*END
002000140818
002100140818       //--------------------------------------------------------------
002200140818       //?Specifiche di controllo.                                     ?
002300140818       //--------------------------------------------------------------
002400140818
002500140818     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600140818     h dftactgrp(*no)
002700140818
002800140818       //--------------------------------------------------------------
002900140818       //?Dichiarazione file.                                          ?
003000140818       //--------------------------------------------------------------
003100140905
003200140905       // -?Organigramma?
003300140905     fAZORG01L  if   e           k disk
003400140818
003500140818       // -?WrkF Acquisizione Potenziali con dati ricevuti da CRIBIS?
003600140826       //  ?(clienti da aggiornare: WFACPV0F)?
003700140826     fWFACP00F  if   e             disk
003800140819     f                                     extfile(wLibFile1)
003900140826     f                                     rename(WFACPV00 : WFACP000)
004000140818     f                                     usropn
004100140819       // -?WrkF Classificazione attività economiche ATECO 2007?
004200140820     fWATECO1L  if   e           k disk
004300140819     f                                     extfile(wLibFile2)
004400140819     f                                     usropn
004500140820       // -?WrkF per Mappatura Commerciale -?
004600140820       //  ?% di spesa Presunta trasporti sul fatturato?
004700140820     fWFDEMR1L  if   e           k disk
004800140820     f                                     extfile(wLibFile3)
004900140820     f                                     usropn
005000140820
005100140820       // -?Informazioni commerciali clienti Potenziali?
005200140905     fTICPI01L  if A e           k disk
005300140818
005400140818       // -?Anagrafica Clienti Potenziali?
005500140820     fTNCPO01L  Uf   e           k disk
005600140820     fTNCPO11L  Uf A e           k disk
005700140818
005800140818       // -?Stampa segnalazioni di errore?
005900140818     fQSYSPRT   o    f  132        printer
006000140818     f                                     usropn
006100140818     f                                     oflind(*inOF)
006200140820
006300141002       // -?Stampa dettaglio potenziali elaborati?
006400141002     fQSYSPRT1  o    f  132        printer
006500141002     f                                     usropn
006600141002     f                                     oflind(*inOA)
006700140818
006800140818       //--------------------------------------------------------------
006900140818       //?Definizione costanti.                                        ?
007000140818       //--------------------------------------------------------------
007100140818
007200140820       // -?Limite fatturato per "piccole imprese"?
007300140820     d c_PiccoleAL     c                   const(10000000)
007400140820       // -?Limite fatturato per "medie imprese"?
007500140820     d c_MedieAL       c                   const(50000000)
007600140901
007700140901       // -?C.A.P. di San Marino (parziale: prime 4 cifre)?
007800140901     d c_CAP_SM_parz4  c                   const('4789')
007900140820
008000140820       // -?Costanti in stampa (forse)?
008100140820     d c_SpesaTra      c                   const('Spesa Trasp. Presunta:')
008200140818
008300140818       //--------------------------------------------------------------
008400140818       //?Definizione schiere.                                         ?
008500140818       //--------------------------------------------------------------
008600140818
008700140818
008800140818       //--------------------------------------------------------------
008900140818       //?Definizione aree dati.                                       ?
009000140818       //--------------------------------------------------------------
009100140818
009200140818       // -?Dati utente?
009300140818     d §AzUte        e ds                  extname(AZUTE00F)
009400140818     d                                     dtaara
009500140818     d §DatiUte      e ds                  extname(dDatiUte)
009600140818     d                                     dtaara
009700140818
009800140818       //--------------------------------------------------------------
009900140818       //?Definizione strutture dati.                                  ?
010000140818       //--------------------------------------------------------------
010100140818
010200140818       // -?Status ds?
010300140818     d Status         sds
010400140818     d   SDSpgmName      *proc
010500140818     d*//SDSparms        *parms
010600140818     d   SDSjobName          244    253                                         Job name
010700140818     d   SDSjobUser          254    263                                         User name
010800140818     d*//SDSjobNumber        264    269s 0                                      Job number
010900140818
011000140820       // -?Parametri ricevuti?
011100140820     d KPJBA         e ds
011200140819
011300140820       // -?DS per campo CPORST di TNCPO00F
011400140820     d dCPO01        e ds                  inz  qualified
011500140910       // -?DS per campo CPOTEL di TNCPO00F
011600140910     d dCPO02        e ds                  inz  qualified
011700140818
011800140818       //--------------------------------------------------------------
011900140818       //?Definizione variabili globali.                               ?
012000140818       //--------------------------------------------------------------
012100140818
012200140818       // -?Nome esteso Libreria/File del file da elaborare?
012300140819     d wLibFile1       s             21a   inz
012400140819     d wLibFile2       s             21a   inz
012500140820     d wLibFile3       s             21a   inz
012600140818
012700140818       // -?Flags Booleani?
012800140819     d $UpdTNCPO       s               n   inz
012900140905     d $SPT            s               n   inz
013000140905     d $ECM            s               n   inz
013100140818
013200140818       // -?Campi di comodo?
013300140818     d wDate_Iso       s               d   inz  datfmt(*iso)
013400140818     d wDate_Eur       s               d   inz  datfmt(*eur)
013500140818     d wDate           s              8  0 inz
013600140818     d wTime           s              6  0 inz
013700140818     d wErr            s              3  0 inz
013800140819     d wSituazione     s              1  0 inz
013900140820     d w255a           s            255    inz
014000140820
014100140820       // -?Campi per la subr. sr_AGGIOindirizzo?
014200140820     d wSAVcap         s                   like(CPOcap)  inz
014300140820     d wSAVprv         s                   like(CPOprv)  inz
014400140820
014500140820       // -?Campi per la subr. sr_CalcoloSpesaPresunta?
014600140820     d wDEMfil         s              3  0 inz
014700140820     d wDEMare         s              3  0 inz
014800140820     d wDEMtotFat      s             15  0 inz
014900140820     d wPerc           s              5  2 inz
015000140820     d wSpesaTra       s             11  0 inz
015100140820     d w0080           s              8  0 inz
015200140820     d wAlfaSCT        s              5    inz
015300140820
015400140820       // -?Campi di stampa?
015500140901     d s_Aggior        s             12    inz('AGGIORNABILI')
015600141002     d s_SpesaTra      s             40    inz
015700141002     d s_CatMercERR    s             26    inz
015800141002     d s_CpLoPrERR     s              1    inz
015900141002     d wSaveWAPateco   s                   like(WAPateco)  inz(*hival)
016000140818
016100140818       //--------------------------------------------------------------
016200140818       //?Definizione prototipi procedure.                             ?
016300140818       //--------------------------------------------------------------
016400140818
016500140818       // -?Reperimento dati utente?
016600140818     d TIBS34ds      e ds                  inz
016700140818      /copy gaitrasrc/srcProtoPR,TIBS34R
016800140820
016900140820       // -?Controllo CAP ("Pretrullo TISI95R")?
017000140820     d FNLV13ds      e ds                  inz
017100140820     d TISI95ds      e ds                  inz
017200140820     d fnlv13r         pr                  extpgm('FNLV13R')
017300140820     d   kpjba                             likeds(KPJBA)
017400140820     d   fnlv13ds                          likeds(FNLV13ds)
017500140820     d   tisi95ds                          likeds(TISI95ds)
017600140818
017700140819       // -?API QCAPCMD (Process Commands)?
017800140819     d Qcmd            s           2048    inz  varying
017900140818      /copy qSysInc/qRpgleSrc,QCAPCMD
018000140818      /copy gaitrasrc/srcProtoPR,QCAPCMD
018100140818
018200140818       // -?Parametri gestione errori API.?
018300140818      /copy qsysinc/qrpglesrc,QUSEC
018400140818
018500140818       //--------------------------------------------------------------
018600140818       //?Definizione key-list.                                        ?
018700140818       //--------------------------------------------------------------
018800140818
018900140905       // -?File TICPI01L?
019000140905     d keyTICPI01    e ds                  extname(TICPI01L : *key)
019100140905     d                                     prefix(k_)   inz
019200140818
019300140818       //--------------------------------------------------------------
019400140818       //?Riepilogo indicatori utilizzati.                             ?
019500140818       //--------------------------------------------------------------
019600140818
019700140818
019800140818       //--------------------------------------------------------------
019900140818       //?M A I N - L I N E                                            ?
020000140818       //--------------------------------------------------------------
020100140818
020200140818     c     *Entry        plist
020300140820     c                   parm                    KPJBA
020400140818
020500140818      /free
020600140818
020700140818       // -?Operazioni iniziali?
020800140818       exsr sr_RoutInz;
020900140818
021000140818       // -?Ciclo di lettura file WFACP00F?
021100140818       read  WFACP000;
021200140818
021300140818       DoW  Not %eof(WFACP00F);
021400140818
021500140818         exsr  sr_ElabPot;
021600140818
021700140818         read  WFACP000;
021800140818
021900140818       EndDo;
022000140818
022100140818       // -?Operazioni finali?
022200140818       exsr sr_RoutEnd;
022300140818
022400140818       //--------------------------------------------------------------
022500140818       //?Operazioni iniziali.                                         ?
022600140818       //--------------------------------------------------------------
022700140819
022800140818       BEGSR  sr_RoutInz;
022900140818
023000140818         // -?Impostazione chiusura?
023100140818         *inLR = *on;
023200140818
023300140820         // -?Ricezione parametri?
023400140820         $UpdTNCPO  = ( %subst( KPJBU : 1 : 3 ) = 'A  ' );
023500140820         if  $UpdTNCPO;
023600140901           s_Aggior = 'AGGIORNATI  ';
023700140820         endif;
023800140818
023900140820         // -?Reperimento data odierna in formato aaaa/mm/gg?
024000140818         wDate_Iso = %date();
024100140820         // -?Conversione data odierna in formato gg/mm/aaaa?
024200140818         wDate_Eur = wDate_Iso;
024300140818         wDate = %dec( wDate_Eur );
024400140818
024500140819         // -?Reperimento orario (hh:mm:ss)?
024600140818         wTime = %dec( %time() );
024700140818
024800140818         // -?Reperimento dati job?
024900140818         exsr  sr_DatiJob;
025000140818
025100140820         // -?Impostazione librerie dei work-file di input:?
025200140820         //  ?1) WFACP00F?
025300140820         //  ?2) WATECO1L?
025400140820         //  ?3) WFDEMR1L?
025500140818         if  %subst( knsif : 7 : 1 ) = 'P';
025600140819           wLibFile1 = '*LIBL';
025700140820           wLibFile3 = 'GAITRAAZP';
025800140818         else;
025900140819           wLibFile1 = 'EDPCRIBIS';
026000140820           wLibFile3 = 'GAITRAAZM';
026100140818         endif;
026200140819         wLibFile2 = wLibFile1;
026300140818
026400140820         // -?Apertura del work-file di input WFACP00F?
026500140826         wLibFile1 = %trimR( wLibFile1 ) + '/WFACPV0F';
026600140818         open(e)  WFACP00F;
026700140818         if  %error();
026800140818           wErr = 1;
026900140818           exsr  sr_PrintErr;
027000140818           exsr  sr_RoutEnd;
027100140818         endif;
027200140820
027300140820         // -?Apertura del work-file di input WATECO1L?
027400140820         wLibFile2 = %trimR( wLibFile2 ) + '/WATECO1L';
027500140819         open(e)  WATECO1L;
027600140819         if  %error();
027700140819           wErr = 2;
027800140819           exsr  sr_PrintErr;
027900140819           exsr  sr_RoutEnd;
028000140819         endif;
028100140820
028200140820         // -?Apertura del work-file di input WFDEMR1L?
028300140820         wLibFile3 = %trimR( wLibFile3 ) + '/WFDEMR1L';
028400140820         open(e)  WFDEMR1L;
028500140820         if  %error();
028600140820           wErr = 3;
028700140820           exsr  sr_PrintErr;
028800140820           exsr  sr_RoutEnd;
028900140820         endif;
029000140818
029100140818       ENDSR;
029200140818
029300140818       //--------------------------------------------------------------
029400140818       //?Reperimento Dati del job (Utente/Operativi).                 ?
029500140818       //--------------------------------------------------------------
029600140818       BEGSR  sr_DatiJob;
029700140818
029800140818         in(e) §AzUte;
029900140818         if NOT %error;
030000140818           in(e) §DatiUte;
030100140818         endif;
030200140818         if %error or RSut = *blank;
030300140818           tibs34r ( tibs34ds );
030400140818           in §AzUte;
030500140818           in §DatiUte;
030600140818         endif;
030700140818
030800140818       ENDSR;
030900140818
031000140818       //--------------------------------------------------------------
031100140818       //?Elaborazione singolo cliente Potenziale.                     ?
031200140818       //--------------------------------------------------------------
031300140818       BEGSR  sr_ElabPot;
031400140819
031500140820         // -?Verifica SE cliente da aggiornare?
031600140909         clear  wErr;
031700140819         exsr  sr_Verifica;
031800140820
031900140826         // -?I dati sono già stati filtrati in WFACPV0F:?
032000140826         //  ?questo test servirebbe SE si leggesse WFACP00F?
032100140826         //if  wSituazione = *zero;
032200140826         //  leavesr;
032300140826         //endif;
032400140901
032500140901         // -?Potenziale NON più esistente in TNCPO00F?
032600140901         //  ?(perchè unificato)?
032700140901         if  wErr = 4;
032800140901           leavesr;
032900140901         endif;
033000140901
033100140820
033200140820         // -?Valorizzazione campi in TNCPO (00F/10F)?
033300140820         //  ?(comodo comunque per la stampa)?
033400140820         exsr  sr_ImpostaTNCPO;
033500140820
033600141002         // -?Stampa (se NON richiesto aggiornamento)?
033700141002         if  Not  $UpdTNCPO;
033800141002           exsr  sr_PrintCliPot;
033900141002         endif;
034000140820
034100140820         // -?Aggiornamento TNCPO (se richiesto)?
034200140820         if  $UpdTNCPO;
034300140820           exsr  sr_UpdateTNCPO;
034400140820         endif;
034500140818
034600140818       ENDSR;
034700140819
034800140819       //--------------------------------------------------------------
034900140820       //?Verifica SE cliente da aggiornare.                           ?
035000140819       //--------------------------------------------------------------
035100140819       BEGSR  sr_Verifica;
035200140820
035300140820         clear  wSituazione;
035400141002         reset  wSaveWAPateco;
035500140820
035600140820         Select;
035700140820
035800140820           // -?Situazione 1?
035900140820           //  ?(incrocio per Ragione Sociale)?
036000140820           When  WAPconf <> *blank  and  WAPconf >= '07'  and
036100140820                 WAPsimN >= 30      and  WAPsimA >= 30;
036200140820             wSituazione  = 1;
036300140820
036400140820           // -?Situazione 2?
036500140820           //  ?(incrocio per Partita Iva / Codice Fiscale)?
036600140820           When  WAPconf  = *blank  and
036700140820                 WAPmgrN >= 30      and  WAPmgrA >= 30;
036800140820             wSituazione  = 2;
036900140820
037000140820         EndSl;
037100140820
037200140820
037300140820         // -?SE cliente da NON aggiornare => uscita?
037400140826         //  ?(i dati sono già stati filtrati in WFACPV0F)?
037500140826         //if  wSituazione = *zero;
037600140826         //  leavesr;
037700140826         //endif;
037800140820
037900140819
038000140819         // -?Reperimento dati anagrafici da aggiornare?
038100140819         if  $UpdTNCPO;
038200140819           chain  ( WAPcpo )  TNCPO000;
038300140820           chain  ( WAPcpo )  TNCPO100;
038400140819         else;
038500140819           chain(N)  ( WAPcpo )  TNCPO000;
038600140820           chain(N)  ( WAPcpo )  TNCPO100;
038700140820         endif;
038800140819
038900140820         // -?SE cliente NON reperito in TNCPO00F => uscita?
039000140819         if  Not %found(TNCPO01L);
039100140901           wErr = 4;
039200141002           //exsr  sr_PrintErr;    ?(senza segnalazione di errore)?
039300141002           ////clear  wSituazione; ?(scarta comunque per wErr = 4)?
039400140819           leavesr;
039500140819         endif;
039600140819
039700140820         // -?SE cliente NON reperito in TNCPO10F =>?
039800140820         //  ?rec. da aggiungere (WRITE)?
039900140820         if  Not %found(TNCPO11L)  and  $UpdTNCPO;
040000140819           clear  TNCPO100;
040100140819           CPO1cpo = CPOcpo;
040200140819         endif;
040300140819
040400140905         // -?Verifica esistenza Spesa Trasporti Presunta?
040500140819         dCPO01 = CPOrst;
040600140905         clear  KeyTICPI01;
040700140905         k_CPIcpo = WAPcpo;
040800140905         k_CPItpf = 'SPT';
040900140905         setll  %kds( KeyTICPI01 )  TICPI000;
041000140905         $SPT = ( %equal(TICPI01L) );
041100140905         // -?Verifica esistenza rec. per Info E-Commerce?
041200140905         clear  KeyTICPI01;
041300140905         k_CPIcpo = WAPcpo;
041400140905         k_CPItpf = 'ECM';
041500140905         setll  %kds( KeyTICPI01 )  TICPI000;
041600140905         $ECM = ( %equal(TICPI01L) );
041700140819
041800140819         // -?Reperimento categoria merceologica?
041900140819         chain  ( WAPateco )  WATECO00;
042000140819
042100140820         // -?SE categoria merceologica mancante =>?
042200141002         //  ?segnalazione in stampa   &?
042300141002         //  ?NO aggiornamento della Categoria Merceologica?
042400140819         if  Not %found(WATECO1L);
042500141002           //if  Not  $UpdTNCPO;
042600141002           //  wErr = 5;           ?(NON segnala più in stampa)?
042700141002           //  exsr  sr_PrintErr;
042800141002           //endif;
042900141002           wSaveWAPateco = WAPateco;
043000140819           clear  WAPateco;
043100140819           clear  WATbCatM;
043200140819         endif;
043300140819
043400140819       ENDSR;
043500140820
043600140820       //--------------------------------------------------------------
043700140820       //?Impostazione nuovi dati nei campi di TNCPO00F e TNCPO10F.    ?
043800140820       //--------------------------------------------------------------
043900140820       BEGSR  sr_ImpostaTNCPO;
044000140820
044100140820         // -?Impostazione dati nel file TNCPO00F?
044200140820
044300140825         // -?Codice Potenziale?
044400140820         // -?Ragione Sociale?
044500140820         CPOrag  = WAPdeno;
044600140825         // -?Indirizzo / Località / CAP / Provincia?
044700140903         if  WAPind2 <> *blank  and  WAPloc2 <> *blank;
044800140903           exsr  sr_AggioINDIRIZZO;
044900140903         endif;
045000140820         // -?Telefono (ora in TNCPO10F)?
045100140820         //*CPOtel  = WAPtel2;
045200140820         // -?Fax (ora in TNCPO10F)?
045300140820         //*CPOfax  = WAPfax2;
045400140903         // -?Anno Inizio Attività?
045500140910         //%subst( CPOtel : 1 : 4 ) = %subst( %editc( WAPdtIni : 'X' ) : 5 : 4 );
045600140910         dCPO02 = CPOtel;
045700140910         dCPO02.§CPOannoIA = %subst( %editc( WAPdtIni : 'X' ) : 5 : 4 );
045800140910         CPOtel = dCPO02;
045900140820         // -?Categoria Merceologica (categoria SEAT)?
046000140820         if  WAPateco <> *blank  and  WATbCatM > *zero;
046100140820           CPOsct  =  WATbCatM;
046200140820         endif;
046300140904         // -?Codice Importanza Cliente --- CPOCLV?
046400140821         // -?Codice Commerciale?
046500140821         // -?Categoria Potenziale?
046600140820         // -?Azienda in Difficoltà Finanziarie?
046700140820         %subst( CPOclt : 1 : 1 ) = WAPfr4;
046800140820         // -?Flag E-Commerce?
046900140820         if  WAPeCom = 'Y';
047000140905           if  Not $ECM;
047100140905             exsr  sr_TICPI_ECM;
047200140905           endif;
047300140820         endif;
047400140821         // -?Data Acquisizione Potenziale?
047500140820         // -?Codice Fiscale?
047600140909         //if  CPOcdf  =  *blank;
047700140909         if  WAPcfi2 <> *blank;
047800140829           CPOcdf  =  WAPcfi2;
047900140820         endif;
048000140820         // -?Partita Iva?
048100140909         //if  CPOpiv  =  *blank;
048200140909         if  WAPcpi2 <> *blank;
048300140829           CPOpiv  =  WAPcpi2;
048400140820         endif;
048500140820         // -?% Export?
048600140820         if  WAPpExp > *zero;
048700140910           CPOpExp = WAPpExp;
048800140820         else;
048900140910           CPOpExp = WAPpImpEx;
049000140820         endif;
049100140903
049200140909         If  WAPfatt > *zero;
049300140903
049400140910           // -?Fatturato Azienda (migliaia di Euro)?
049500140910           eval(H)  CPOftAz = WAPfatt / 1000;
049600140910           // -?Anno Fatturato Azienda?
049700140910           CPOaFAz = WAPaaFt;
049800140910           // -?Flag Fatturato Azienda (S-Stimato, D-Dichiarato)?
049900140910           if  WAPpvnFt <> *blank;
050000140910             CPOfFAz = WAPpvnFt;
050100140910           endif;
050200140909
050300140910           // -?Spesa Trasporti Presunta?
050400140910           //dCPO01  = CPOrst; ? dati già reperiti nella subr. sr_Verifica ?
050500140910           if  Not $SPT;
050600140910             exsr  sr_CalcoloSpesaPresunta;
050700140910             dCPO01.§CPOspTp = wSpesaTra;
050800140910             CPOrst  = dCPO01;
050900140910           endif;
051000140909
051100140903         EndIf;
051200140820
051300140821         // -?Flag Sede/Filiale?
051400140821         // -?Codice DUNS?
051500140825         // -?Filiale di Appartenenza (Trasmissione Dati)?
051600140904         // -?Codice Importanza Cliente (A-B-C-T-D) --- CPOFTR?
051700140821         // -?Data Trasmissione?
051800140904         CPOdtr  = %dec( wDate_Iso );
051900141002         //CPOdtr  = 20140815;     ?(proposta bocciata da L. Bocchi)?
052000140820
052100140820         // -?Impostazione dati nel file TNCPO10F?
052200140820
052300140820         // -?Telefono?
052400140909         if  WAPtel2 <> *blank;
052500140909           CPO1tel = WAPtel2;
052600140909         endif;
052700140909         // -?Fax?
052800140909         if  WAPfax2 <> *blank;
052900140909           CPO1fax = WAPfax2;
053000140909         endif;
053100140820         // -?Crif Number?
053200140820         CPO1crifN  = WAPcrif;
053300140820         // -?Crif Number Casa Madre?
053400140820         CPO1crifNM = WAPcmCrif;
053500140820
053600140820       ENDSR;
053700140819
053800140819       //--------------------------------------------------------------
053900140819       //?Aggiornamento indirizzo.                                     ?
054000140819       //--------------------------------------------------------------
054100140819       BEGSR  sr_AGGIOindirizzo;
054200140821
054300141002         clear  s_CpLoPrERR;
054400140821         clear  wSAVcap;
054500140821         clear  wSAVprv;
054600140819
054700140819         // -?SE manca il CAP: tengo quello presente SE stessa provincia?
054800140819         if  WAPcap2 = *blank  or  WAPcap2 = *zero;
054900140820           wSAVcap = CPOcap;
055000140820           wSAVprv = CPOprv;
055100140819         endif;
055200140819
055300140821         CPOvia = WAPind2;
055400140819         CPOcap = WAPcap2;
055500140819         CPOprv = WAPprv2;
055600140819
055700140819         IF  WAPfra2 = *blank  or  WAPfra2 = WAPloc2;
055800140819
055900140819           CPOcit = WAPloc2;
056000140819
056100140819         ELSE;
056200140819
056300140819           // -?Verifica SE frazione corretta --> al posto della località?
056400140819           clear  TISI95ds;
056500140819           clear  FNLV13ds;
056600140819           I95tcn = '7';
056700141002           //I95cap = WAPcap2;     ?(senza CAP)?
056800140819           I95loc = WAPfra2;
056900140819           I95prv = WAPprv2;
057000140901           if  %subst( WAPcap2 : 1 : 4 ) = c_CAP_SM_parz4;
057100140825             WAPprv2 = 'RN';
057200140825             I95prv  = 'RN';
057300140825           endif;
057400140819           I95dat = %dec( wDate_Iso );
057500140822           I13la3 = 'S';
057600140819           I13af0 = 'S';
057700140819
057800140819           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
057900140819
058000140910           //If  O95err =  *blank  and  O13err <> '5';
058100140910           If  O95err = *blank;
058200140819
058300140819             CPOcit = O95loc;
058400140909             CPOcap = O95cap;
058500140819             CPOprv = O95prv;
058600140819
058700140819           Else;
058800140819
058900140820             clear  w255a;
059000140820             w255a = %trim(WAPind2) + '-' + %trim(WAPfra2);
059100140820             if  %subst( w255a : %size( CPOvia ) + 1 ) = *blank;
059200140820               CPOvia = w255a;
059300140819               CPOcit = WAPloc2;
059400140819             else;
059500140819               CPOcit = WAPloc2;
059600140819             endif;
059700140819
059800140819           EndIf;
059900140819
060000140819         ENDIF;
060100140819
060200140819
060300140819         IF  CPOcap = *blank  or  CPOcap = '00000    ';
060400140819
060500140819           clear  TISI95ds;
060600140819           clear  FNLV13ds;
060700140819           I95tcn = '7';
060800140819           I95loc = CPOcit;
060900140819           I95prv = CPOprv;
061000140903           if  ( %scan( 'SAN MARINO' : WAPreg2 ) > *zero  and
061100140903                 WAPprv2 <> 'RN' )                        OR
061200140903               ( %scan( 'SAN MARINO' : WAPloc2 ) > *zero  and
061300140903                 WAPprv2 <> 'RN' )                        OR
061400140903               ( %scan( 'RSM'        : WAPreg2 ) > *zero  and
061500140903                 WAPprv2 <> 'RN' )                        OR
061600140903               ( %scan( 'RSM'        : WAPloc2 ) > *zero  and
061700140903                 WAPprv2 <> 'RN' );
061800140901             WAPprv2 = 'RN';
061900140825             I95prv  = 'RN';
062000140825           endif;
062100140819           I95dat = %dec( wDate_Iso );
062200140822           I13la3 = 'S';
062300140822           I13af0 = 'S';
062400140819
062500140819           FNLV13R ( KPJBA : FNLV13ds : TISI95ds );
062600140819
062700140819           If  O95err = *blank;
062800140819
062900140819             CPOcap = O95cap;
063000140819             CPOprv = O95prv;
063100140819
063200140819           Else;
063300140819
063400140820             if  wSAVprv = CPOprv;
063500140820               CPOcap = wSAVcap;
063600140819             endif;
063700140819
063800140819           EndIf;
063900140819
064000140819         ENDIF;
064100140901
064200140819       ENDSR;
064300140819
064400140905       //--------------------------------------------------------------
064500140905       //?Impostaz. campi nel rec."ECM" da aggiungere al file TICPI00F.?
064600140905       //--------------------------------------------------------------
064700140905       BEGSR  sr_TICPI_ECM;
064800140905
064900140905         clear  TICPI000;
065000140905
065100140905         CPIcpo = k_CPIcpo;
065200140905         CPItpf = k_CPItpf;
065300140905         CPIspf = k_CPIspf;
065400140905
065500140905         CPIval = 50;
065600140905         CPItva = '%  ';
065700140905
065800140905         CPIdim = %dec( wDate_Iso );
065900140905         CPIfil = 046;
066000140905         CPIpru = 'BATCH     ';
066100140905
066200140905       ENDSR;
066300140905
066400140819       //--------------------------------------------------------------
066500140819       //?Calcolo Spesa Presunta.                                      ?
066600140819       //--------------------------------------------------------------
066700140820       BEGSR  sr_CalcoloSpesaPresunta;
066800140820
066900140820         clear  wDEMfil;
067000140820         clear  wDEMare;
067100140820         clear  wDEMtotFat;
067200140820         clear  wAlfaSCT;
067300140820         clear  wPerc;
067400140820         clear  wSpesaTra;
067500140819
067600140819         // -?SE non c'è un Fatturato Aziendale: NON si calcola nulla?
067700140819         if  CPOftAz <= *zero;
067800140819           leavesr;
067900140819         endif;
068000140819
068100140820         wDEMtotFat = CPOftAz * 1000;
068200140820
068300140820         wAlfaSCT = %editc( CPOsct : 'X' );
068400140819
068500140819         // -?SE presente la nazione, imposto la filiale del codice?
068600140819         //  ?e NON da cappario?
068700140819         If  CPOnaz <> *blank;
068800140819
068900140820           wDEMfil = CPOflt;
069000140819
069100140819         Else;
069200140819
069300140819           clear  TISI95ds;
069400140819           clear  FNLV13ds;
069500140819           I95tcn = '7';
069600140819           I95cap = CPOcap;
069700140819           I95loc = CPOcit;
069800140819           I95prv = CPOprv;
069900140819           I95nar = CPOnaz;
070000140819           I95dat = %dec( wDate_Iso );
070100140822           I13la3 = 'S';
070200140819           I13af0 = 'S';
070300140819           if  %subst( I95cap : 1 : 2 ) = '47'  and  I95prv = *blank;
070400140819             I95prv = 'RN';
070500140819           endif  ;
070600140819
070700140819           FNLV13R  ( KPJBA : FNLV13ds : TISI95ds );
070800140819
070900140820           wDEMfil = O95lna;
071000140819
071100140819         EndIf;
071200140903
071300140903         // -?SE  NON trovata filiale: forza Filiale Trasmissione Dati?
071400140903         if  wDEMfil = *zero;
071500140903           wDEMfil = CPOflt;
071600140903         endif;
071700140819
071800140820         // -?SE trovata filiale: impostazione area?
071900140820         if  wDEMfil > *zero;
072000140820           chain  ( wDEMfil )  AZORG;
072100140819           if  %found(AZORG01L);
072200140820             wDEMare = ORGcar;
072300140819           endif;
072400140820         endif;
072500140819
072600140819
072700140820         // -?Reperimento area di appartenenza dall'indirizzo?
072800140820         chain  ( wAlfaSCT : wDEMare )  WFDEMR00;
072900140820         if  %found(WFDEMR1L);
073000140819           exsr  sr_ScegliP;
073100140820         endif ;
073200140819
073300140820         if  wPerc = *zero;
073400140820           clear  wDEMare;
073500140820           chain  ( wAlfaSCT : 000 )  WFDEMR00;
073600140819           if  %found(WFDEMR1L);
073700140819             exsr  sr_ScegliP;
073800140819           endif;
073900140820         endif;
074000140819
074100140820         If  wPerc > *zero;
074200140820           eval(H)  wSpesaTra = ( wDEMtotFat * wPerc ) / 100;
074300140820           // -?Arrotondamento ai 1000 € superiori?
074400140820           w0080 = wSpesaTra / 1000;
074500140819           w0080 = w0080 + 1;
074600140820           wSpesaTra = w0080 * 1000;
074700140820         EndIf;
074800140819
074900140819       ENDSR;
075000140819
075100140819       //--------------------------------------------------------------
075200140820       //?Reperimento percentuale per calcolare la spesa presunta.     ?
075300140820       //--------------------------------------------------------------
075400140819       BEGSR  sr_ScegliP;
075500140820
075600140819         select;
075700140819
075800140820           when  wDEMtotFat <= c_PiccoleAL;
075900140820             if  DEMpCli > 9  or  wDEMare = *zero;
076000140820               wPerc = DEMpPerc;
076100140819             endif;
076200140819
076300140820           when  wDEMtotfat <= c_MedieAL;
076400140820             if  DEMmCli > 9  or  wDEMare = *zero;
076500140820               wPerc = DEMmPerc;
076600140819             endif;
076700140819
076800140819           other;
076900140820             if  DEMgCli > 9  or  wDEMare = *zero;
077000140820               wPerc = DEMgPerc;
077100140819             endif;
077200140819
077300140819         endsl;
077400140819
077500140819       ENDSR;
077600140820
077700140820       //--------------------------------------------------------------
077800140905       //?Aggiornamento dati nei file TNCPO00F, TNCPO10F e TICPI00F.   ?
077900140820       //--------------------------------------------------------------
078000140820       BEGSR  sr_UpdateTNCPO;
078100140820
078200140820         // -?Aggiornamento rec. nel file TNCPO00F?
078300140820         //_______________
078400140820         update  TNCPO000;
078500140820         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078600140820
078700140820         // -?Aggiornamento/Scrittura rec. nel file TNCPO10F?
078800140820         if  %found(TNCPO11L);
078900140820           //_______________
079000140820           update  TNCPO100;
079100140820           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079200140820         else;
079300140820           //______________
079400140820           write  TNCPO100;
079500140820           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079600140820         endif;
079700140905
079800140905         // -?Scrittura rec. "ECM" nel file TICPI00F?
079900140905         if  WAPeCom = 'Y'  and  Not $ECM;
080000140905           //______________
080100140905           write  TICPI000;
080200140905           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080300140905         endif;
080400140820
080500140820       ENDSR;
080600140820
080700140820       //--------------------------------------------------------------
080800140820       //?Stampa dati del cliente potenziale da aggiornare.            ?
080900140820       //--------------------------------------------------------------
081000141002       BEGSR  sr_PrintCliPot;
081100141002
081200141002         // -?Impostazione campi di stampa?
081300141002         //  ?(con lunghezza diversa da quella dei campi "originali")?
081400141002         // ·?Spesa Trasporti Presunta?
081500141002         //  ?(SE NON c'è la spesa reale delle Info-Commerciali,?
081600141002         //  ? SE c'è un Fatturato Aziendale  e?
081700141002         //  ? SE è stata calcolcolata)?
081800141002         clear  s_SpesaTra;
081900141002         if  Not $SPT  and  CPOftAz > *zero  and
082000141002             wSpesaTra > *zero;
082100141002           s_SpesaTra  = c_SpesaTra + ' ' + %editc( wSpesaTra : '1' );
082200141002         endif;
082300141002         // ·?Categoria Merceologica ERRATA?
082400141002         clear  s_CatMercERR;
082500141002         //if  wErr = 5;
082600141002         if  wSaveWAPateco <> *hival;
082700141002           s_CatMercERR = 'Cat.Merc.ERRATA: ' + wSaveWAPateco;
082800141002         endif;
082900141002
083000141002         // -?Apertura del printer-file?
083100141002         if  Not %open(QSYSPRT1);
083200141002           Qcmd = 'OVRPRTF file(QSYSPRT1) +
083300141002                           usrdta(''Agg. TNCPO'') +
083400141002                           ovrscope(*actgrpdfn)';
083500141002           exsr  sr_ExecCmd;
083600141002           open  QSYSPRT1;
083700141002           except  PRT1txt;
083800141002         endif;
083900141002
084000141002         if  *inOA;
084100141002           except  PRT1txt;
084200141002           *inOA = *off;
084300141002         endif;
084400141002
084500141002         except  PRT1det;
084600141002
084700141002       ENDSR;
084800140818
084900140818       //--------------------------------------------------------------
085000140818       //?Stampa segnalazione dell'errore rilevato.                    ?
085100140818       //--------------------------------------------------------------
085200140818       BEGSR  sr_PrintErr;
085300140818
085400140818         // -?Apertura del printer-file?
085500140818         if  Not %open(QSYSPRT);
085600140818           Qcmd = 'OVRPRTF file(QSYSPRT) +
085700140818                           usrdta(''ACPO06*ERR'') +
085800140818                           ovrscope(*actgrpdfn)';
085900140818           exsr  sr_ExecCmd;
086000140818           open  QSYSPRT;
086100140818           except  PRTtxt;
086200140818         endif;
086300140818
086400140818         if  *inOF;
086500140818           except  PRTtxt;
086600140818           *inOF = *off;
086700140818         endif;
086800140818
086900140819         select;
087000140819           when  wErr = 1;
087100140818             except  PrtErr1;
087200140819           when  wErr = 2;
087300140818             except  PrtErr2;
087400140819           when  wErr = 3;
087500140818             except  PrtErr3;
087600140819           when  wErr = 4;
087700140819             except  PrtErr4;
087800141002           when  wErr = 5;
087900141002             except  PrtErr5;
088000140819         endsl;
088100140818
088200140818       ENDSR;
088300140818
088400140818       //--------------------------------------------------------------
088500140818       //?Operazioni finali.                                           ?
088600140818       //--------------------------------------------------------------
088700140818       BEGSR  sr_RoutEnd;
088800140818
088900140818         // -?Chiusura spool di stampa errori?
089000140818         if  %open(QSYSPRT);
089100140818           except  PrtEND;
089200140818           close  QSYSPRT;
089300140818         endif;
089400140820
089500140820         // -?Chiusura spool di stampa?
089600141002         if  %open(QSYSPRT1);
089700141002           except  Prt1END;
089800141002           close  QSYSPRT1;
089900141002         endif;
090000140818
090100140820         // -?Chiusura Work-File di input?
090200140818         if  %open(WFACP00F);
090300140818           close  WFACP00F;
090400140818         endif;
090500140820         if  %open(WATECO1L);
090600140820           close  WATECO1L;
090700140820         endif;
090800140820         if  %open(WFDEMR1L);
090900140820           close  WFDEMR1L;
091000140820         endif;
091100140818
091200140818         // -?Chiusura *pgm?
091300140818         return;
091400140818
091500140818       ENDSR;
091600140818
091700140818       //--------------------------------------------------------------
091800140818       //?Esecuzione del comando (già impostato).                      ?
091900140818       //--------------------------------------------------------------
092000140818       BEGSR  sr_ExecCmd;
092100140818
092200140818         clear Qcap0100;
092300140818         Qcabcsdh = *off;
092400140818         Qcapa    = *off;
092500140818         Qcacmdss = *off;
092600140818         Qcaerved = *allX'00';
092700140818
092800140818         clear Qusec;
092900140818         Qusbprv  = %size(Qusec);
093000140818
093100140818         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
093200141002                           %size(Qcap0100) : 'CPOP0100' : *omit :
093300140818                           0 : 0 : Qusec);
093400140818
093500140818         //if  Qusei <> *blank;
093600140818         //  ...;
093700140818         //endif;
093800140818
093900140818       ENDSR;
094000140818
094100140818      /end-free
094200140818
094300140818       //--------------------------------------------------------------
094400140818       //?S P O O L - F I L E S                                        ?
094500140818       //--------------------------------------------------------------
094600140818
094700140820       // -?Stampa segnalazioni di errore?
094800140820     oQSYSPRT   e            PRTtxt            2
094900140818     o                       RSUT
095000140818     o                                        +   4 'LISTA ERRORI RILE-
095100140818     o                                              VATI IN FASE DI EL-
095200140818     o                                              ABORAZIONE DATI NE-
095300140818     o                                              L FILE'
095400140820     o                       wLibFile1        +   1
095500140818     o                       SDSpgmName       +   4
095600140818     o                       *Date         y  +   2
095700140818     o          e            PRTtxt      0
095800140818     o                                          +24 'LISTA ERRORI RILE-
095900140818     o                                              VATI IN FASE DI EL-
096000140818     o                                              ABORAZIONE DATI NE-
096100140818     o                                              L FILE'
096200140820     o                       wLibFile1        +   1
096300140818     o          e            PRTtxt      1
096400140818     o                       KNSIF
096500140818     o                       KNMUS            +   1
096600140818     o                                        +   3 '------------------
096700140818     o                                              -------------------
096800140818     o                                              -------------------
096900140818     o                                              -------------------
097000140818     o                                              ----------'
097100140818     o                                        +   4 'Pag.'
097200140818     o                       Page          z  +   0
097300140818     o                       wTime            +   4 '  :  :  '
097400140818      *
097500140818     o          e            PRTtxt      2
097600140818     o                                              'Potenziale '
097700140818     o                                        +   2 'Messaggio'
097800140818     o          e            PRTtxt      1
097900140818     o                                              '-----------'
098000140818     o                                        +   2 '---------'
098100140818      *
098200140818     o          e            PRTerr1     2
098300140818     o                                              'NON TROVATO IL +
098400140818     o                                               FILE'
098500140819     o                       wLibFile1        +   1
098600140819      *
098700140819     o          e            PRTerr2     2
098800140819     o                                              'NON TROVATO IL +
098900140819     o                                               FILE'
099000140819     o                       wLibFile2        +   1
099100140820      *
099200140820     o          e            PRTerr3     2
099300140820     o                                              'NON TROVATO IL +
099400140820     o                                               FILE'
099500140820     o                       wLibFile3        +   1
099600140818      *
099700140820     o          e            PRTerr4     2
099800140819     o                       WAPcpo
099900140819     o                                        +   2 'Potenziale NON tr-
100000140819     o                                              ovato in anagrafic-
100100140819     o                                              a potenziali (TNCP-
100200140819     o                                              O00F).'
100300141002      *
100400141002     o          e            PRTerr5     2
100500141002     o                       WAPcpo
100600141002     o                                        +   2 'Categoria merceol-
100700141002     o                                              ogica'
100800141002     o                       WAPateco         +   1
100900141002     o                                        +   1 'NON reperita nell-
101000141002     o                                              a tabella WATECO0F-
101100141002     o                                              : sistemare anagra-
101200141002     o                                              fica.'
101300140819      *
101400140819     o          e            PRTend      2
101500140819     o                                        +  24 '***  Fine Lista  ***'
101600140819     o          e            PRTend      0
101700140819     o                                        +  24 '***  Fine Lista  ***'
101800140820
101900141002       // -?Stampa dettaglio potenziali elaborati?
102000141002     oQSYSPRT1  e            PRT1txt           2
102100141002     o                       RSUT
102200141002     o                                        +   4 'LISTA POTENZIALI'
102300141002     o                       s_Aggior         +   1
102400141002     o                       SDSpgmName       +   4
102500141002     o                       *Date         y  +   2
102600141002     o          e            PRT1txt     0
102700141002     o                                          +24 'LISTA POTENZIALI'
102800141002     o                       s_Aggior         +   1
102900141002     o          e            PRT1txt     1
103000141002     o                       KNSIF
103100141002     o                       KNMUS            +   1
103200141002     o                                        +   3 '------------------
103300141002     o                                              ------------'
103400141002     o                                        +   4 'Pag.'
103500141002     o                       Page2         z  +   0
103600141002     o                       wTime            +   4 '  :  :  '
103700141002      *
103800141002     o          e            PRT1txt     2
103900141002     o                                              'Potenziale '
104000141002     o          e            PRT1txt     1  1
104100141002     o                                              '-----------'
104200141002      *
104300141002     o          e            PRT1det     1
104400141002     o                       WAPcpo
104500141002     o                       CPOrag           +   1
104600141002     o                                        +   3 'Situazione:'
104700141002     o                       wSituazione      +   1
104800141002     o                       s_CatMercERR     +   3
104900141002     o          e            PRT1det     1
105000141002     o                       CPOcap           +  12
105100141002     o                       CPOcit           +   1
105200141002     o                       CPOprv           +   1
105300141002     o                       s_CpLoPrERR      +   1
105400141002     o                       s_SpesaTra       +   1
105500141002      *
105600141002     o          e            PRT1end     2
105700141002     o                                        +  24 '***  Fine Lista  ***'
105800141002     o          e            PRT1end     0
105900141002     o                                        +  24 '***  Fine Lista  ***'
