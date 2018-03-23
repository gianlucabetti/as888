000100140115       //==============================================================
000200140115       //?Statistica Numero Pagine di Packing-List per Cliente - BATCH ?
000300140115       //==============================================================
000400140115
000500140115       //--------------------------------------------------------------
000600140115       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140115       //--------------------------------------------------------------
000800140115
000900140115     /*PRM dbgview(*source)
001000140115     /*CMD ovrdbf file(TIPDL00F) tofile(GAITRAAZP/TIPDL00F) +
001100140115     /*CMD        ovrscope(*calllvl)
001200140116     /*CMD ovrdbf file(WFPDL00F) tofile(GAITRAAZP/WFPDL00F) +
001300140116     /*CMD        ovrscope(*calllvl)
001400140115     /*END dltovr file(TIPDL00F) lvl(*)
001500140116     /*END dltovr file(WFPDL00F) lvl(*)
001600140115     /*END
001700140115
001800140115       //--------------------------------------------------------------
001900140115       //?Specifiche di controllo.                                     ?
002000140115       //--------------------------------------------------------------
002100140115
002200140115     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300140115     h alwnull(*inputonly)
002400140115     h dftactgrp(*no)
002500140115     h bnddir('TRUL')
002600140115
002700140115       //--------------------------------------------------------------
002800140115       //?Dichiarazione file.                                          ?
002900140115       //--------------------------------------------------------------
003000140115
003100140115       // -?Organigramma?
003200140115     fAZORG01L  if   e           k disk
003300140115
003400140115      // -?Anagrafica Commerciali?
003500140115     fAZCMM01L  if   e           k disk
003600140117
003700140117      // -?WorkFile - Statistica di Packing-List in LdV (libr. QTEMP)?
003800140117     fWFPDL00F  o    e             disk    usropn
003900140115
004000140115       // -?Printer File?
004100140116     fTNSS09P   o    e             printer
004200140115     f                                     oflind(*in25)
004300140116
004400140116       // -?Stampa segnalazioni di errore?
004500140116     fQSYSPRT   o    f  132        printer usropn
004600140116     f                                     oflind(*inOF)
004700140115
004800140115       //--------------------------------------------------------------
004900140115       //?Definizione costanti.                                        ?
005000140115       //--------------------------------------------------------------
005100140115
005200140115       // -?Codice Utente in TABEL00F?
005300140115     d c_Kut           c                   const(1)
005400140116
005500140116       // -?Costanti per la definizione delle schiere con i nomi?
005600140116       //  ?degli iSeries da elaborare e delle relative librerie?
005700140116     d c_NrSyst        c                   const(2)
005800140116     d c_NrLibr        c                   const(2)
005900140117
006000140117       // -?Comandi da eseguire?
006100140117     d c_CrtDupObj     c                   const('CRTDUPOBJ +
006200140117     d                                            obj(WFPDL00F) +
006300140117     d                                            fromlib(GAITRAAZM ) +
006400140117     d                                            objtype(*file) +
006500140117     d                                            tolib(QTEMP) +
006600140117     d                                            data(*NO)')
006700140117     d c_CpyF          c                   const('CPYF +
006800140117     d                                            fromfile(QTEMP/+
006900140117     d                                                     WFPDL00F) +
007000140117     d                                            tofile(GAITRAAZM/+
007100140117     d                                                     WFPDL00F) +
007200140117     d                                            mbropt(*replace) +
007300140117     d                                            crtfile(*no)')
007400140117     d c_OvrPrtF       c                   const('OVRPRTF +
007500140117     d                                            file(QSYSPRT) +
007600140117     d                                            usrdta(''FNLSE4+
007700140117     d                                                     *ERR'')')
007800140117     d c_DltOvr        c                   const('DLTOVR +
007900140117     d                                            file(QSYSPRT)')
008000140115
008100140115       //--------------------------------------------------------------
008200140115       //?Definizione schiere.                                         ?
008300140115       //--------------------------------------------------------------
008400140115
008500140116       // -?iSeries & Librerie in cui gestire i files?
008600140116     d $iSystem        s                   like(currSysNetA)
008700140116     d                                     dim(c_NrSyst)
008800140116     d                                     ctdata   perrcd( 1)
008900140116     d $Libraries      s                   like(ds_Libr)
009000140116     d                                     dim(c_NrSyst)
009100140116     d                                     alt($iSystem)
009200140115
009300140115       //--------------------------------------------------------------
009400140115       //?Definizione aree dati.                                       ?
009500140115       //--------------------------------------------------------------
009600140115
009700140115       // -?Dati utente?
009800140115     d §AzUte        e ds                  extname(AZUTE00F)
009900140115     d                                     dtaara
010000140115     d §DatiUte      e ds                  extname(dDatiUte)
010100140115     d                                     dtaara
010200140115
010300140115       //--------------------------------------------------------------
010400140115       //?Definizione strutture dati.                                  ?
010500140115       //--------------------------------------------------------------
010600140115
010700140115       // -?Status ds?
010800140115     d Status         sds
010900140115     d   SDSpgmName      *proc
011000140115
011100140115       // -?Parametri ricevuti?
011200140115     d KPJBA         e ds
011300140116     d TNSS09ds      e ds                  inz
011400140122
011500140122       // -?Tab. "05"  = Aree?
011600140122     d ds05          e ds                  inz         qualified
011700140116
011800140116       // -?Dati estratti via SQL?
011900140116     d TIPDLds       e ds                  inz   extname(TIPDL00F)
012000140116     d WFPDLds       e ds                  inz   extname(WFPDL00F)
012100140117     d WTPDLds         ds            81    inz
012200140117     d   UPDn1p                       9s 0 inz
012300140117     d   UPDn2p                       9s 0 inz
012400140117     d   UPDn3p                       9s 0 inz
012500140117     d   UPDn4p                       9s 0 inz
012600140117     d   UPDn5p                       9s 0 inz
012700140117     d   UPDnop                       9s 0 inz
012800140117     d   UPDpo5                       9s 0 inz
012900140117     d   UPDpdfSp                     9s 0 inz
013000140117     d   UPDpdfNr                     9s 0 inz
013100140116
013200140116       // -?Ridefinizione elenco librerie in elaborare le tabelle?
013300140116     d ds_Libr         ds                  inz
013400140116     d   $Libr                       10    dim(c_NrLibr) inz
013500140116
013600140116       // -?Schiera per "scomposizione" stringa SQL in stampa?
013700140116     d ds_Temp         ds          5000    inz
013800140116     d   $Temp                 1   5000    inz   dim(50)
013900140115
014000140115       //--------------------------------------------------------------
014100140115       //?Definizione variabili globali.                               ?
014200140115       //--------------------------------------------------------------
014300140115
014400140115       // -?Flags booleani?
014500140116     d $EoF            s               n   inz
014600140116     d $Ok             s               n   inz
014700140121     d $CmmU           s               n   inz
014800140219     d $Almeno1rec     s               n   inz
014900140117
015000140117       // -?Indici di schiera / Contatori?
015100140117     d xx              s              5  0 inz
015200140117     d wErr            s              3  0 inz
015300140115
015400140116       // -?Stringhe SQL da eseguire?
015500140116     d wSQL1           s           2000    inz   varying
015600140116     d wSQL2           s           2000    inz   varying
015700140116     d wSQL3           s           2000    inz   varying
015800140116
015900140116       // -?Campi per controllo "rotture di livello"?
016000140116     d SaveWPDksu      s                   inz(*loval)   like(WPDksu)
016100140116     d SaveWPDrsu      s                   inz(*loval)   like(WPDrsu)
016200140116
016300140116       // -?Campi di comodo?
016400140117     d wLibrAZx        s             10    inz
016500140116     d w_iSystem       s              1  0 inz
016600140116     d w_SisInf        s              3  0 inz
016700140116     d wTime           s              6  0 inz
016800140116     d wDate           s              8  0 inz
016900140117     d wDate_Iso       s               d   inz  datfmt(*iso)
017000140117     d wDate_Eur       s               d   inz  datfmt(*eur)
017100140115
017200140115       //--------------------------------------------------------------
017300140115       //?Definizione prototipi procedure.                             ?
017400140115       //--------------------------------------------------------------
017500140116
017600140116       // -?Reperimento NETA sistema AS/400 corrente?
017700140116     d currSysNeta     s              8a   inz
017800140116      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017900140115
018000140115       // -?Reperimento dati utente?
018100140115     d TIBS34ds      e ds                  inz
018200140115      /copy gaitrasrc/srcProtoPR,TIBS34R
018300140115
018400140115       // -?Controllo abilitazioni utente?
018500140115     d TNTAA1ds      e ds                  inz
018600140115     d   ITAA1caut   e                     inz('§utegtc')
018700140115      /copy gaitrasrc/srcProtoPR,TNTAA1C
018800140115
018900140115       // -?Caricamento Filiali/Aree gestite dall'utente?
019000140115     d TRUL31ds      e ds                  inz
019100140115     d   sk_POg               10    759    inz   dim(250)
019200140115     d TRUL31ds2     e ds                  inz
019300140115     d   sk_Car               22    171    inz   dim(50)
019400140115     d trul31r         pr                  extpgm('TRUL31R')
019500140115     d   kpjba                             likeds(KPJBA)
019600140115     d   trul31ds                          likeds(TRUL31ds)
019700140115     d   trul31ds2                         likeds(TRUL31ds2)
019800140116
019900140116       // -?Ricerca unificante padre?
020000140116      /copy gaitrasrc/srcProtoPI,TIBS10R
020100140116      /copy gaitrasrc/srcProtoPR,TIBS10R
020200140116
020300140116       // -?Controllo/Decodifica cliente?
020400140116      /copy gaitrasrc/srcProtoPI,TIBS69R
020500140116      /copy gaitrasrc/srcProtoPR,TIBS69R
020600140122
020700140122       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
020800140122      /copy gaitrasrc/srcProtoPI,TRULTAB
020900140122      /copy gaitrasrc/srcProtoPR,TRULTAB
021000140117
021100140117       // -?Parametri API QCAPCMD (Process Commands)?
021200140117     d Qcmd            s           2048    inz  varying
021300140117      /copy qSysInc/qRpgleSrc,QCAPCMD
021400140117       // -?API QCAPCMD (Process Commands)?
021500140117      /copy gaitrasrc/srcProtoPR,QCAPCMD
021600140117
021700140117       // -?Parametri gestione errori API.?
021800140117      /copy qsysinc/qrpglesrc,QUSEC
021900140115
022000140115       //--------------------------------------------------------------
022100140115       //?Definizione key-list.                                        ?
022200140115       //--------------------------------------------------------------
022300140115
022400140115       // -?File AZORG01L?
022500140115     d keyAZORG01    e ds                  extname(AZORG01L : *key)
022600140115     d                                     prefix(k_)   inz
022700140115
022800140115       // -?File AZCMM01L?
022900140115     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
023000140115     d                                     prefix(k_)   inz
023100140115
023200140115       //--------------------------------------------------------------
023300140115       //?Riepilogo indicatori utilizzati.                             ?
023400140115       //--------------------------------------------------------------
023500140115
023600140115
023700140115       //--------------------------------------------------------------
023800140115       //?M A I N - L I N E                                            ?
023900140115       //--------------------------------------------------------------
024000140115
024100140115     c     *Entry        plist
024200140115     c                   parm                    KPJBA
024300140115
024400140115      /free
024500140115
024600140115       // -?Operazioni iniziali?
024700140115       exsr  sr_RoutInz;
024800140115
024900140116       // -?Preparazione stringhe SQL?
025000140116       exsr  sr_PrepSQL1;
025100140116       exsr  sr_PrepSQL2;
025200140115
025300140117       // -?Ciclo di elaborazione TIPDL00F per creare QTEMP/WFPDL00F?
025400140116       reset  $EoF;
025500140116       exsr  sr_OpenCursor1;
025600140115
025700140117       DoW  $EoF = *off;
025800140116         exsr  sr_ReadCursor1;
025900140117       EndDo;
026000140115
026100140116       exsr  sr_CloseCursor1;
026200140116
026300140117       // -?Ciclo di lettura QTEMP/WFPDL00F per stampa?
026400140121       If  SS09out = 'S'  or  SS09out = 'E';
026500140121
026600140121         reset  $EoF;
026700140121         exsr  sr_OpenCursor2;
026800140121
026900140121         DoW  $EoF = *off;
027000140121           exsr  sr_ReadCursor2;
027100140121         EndDo;
027200140121
027300140121         exsr  sr_CloseCursor2;
027400140121
027500140121       EndIf;
027600140122
027700140122       // -?Copia del WrkF QTEMP/WFPDL00F in GAITRAAZM?
027800140122       //  ?SE estratti dati?
027900140219       //If    SS09out = 'E'  and
028000140219       //    ( TPDn1p + TPDn2p + TPDn3p + TPDn4p +
028100140219       //      TPDn5p + TPDnOp + TPDpdfNr ) > *zero  OR
028200140219       //      SS09out = 'F'  and
028300140219       //    ( WPDn1p + WPDn2p + WPDn3p + WPDn4p +
028400140219       //      WPDn5p + WPDnOp + WPDpdfNr ) > *zero;
028500140219       If  ( SS09out = 'F'  or  SS09out = 'E' )  and  $Almeno1rec;
028600140122
028700140122         if  w_iSystem = 1;       //?(SETRAS)?
028800140122           Qcmd = c_CpyF;
028900140122         else;                    //?(AS888)?
029000140122           Qcmd = %trim( %replace( %trimr(wLibrAZx) : c_CpyF :
029100140122                                   %scan( 'GAITRAAZM' : c_CpyF ) ) );
029200140122         endif;
029300140122         exsr  sr_ExecCmd;
029400140122
029500140122         if  Qusei <> *blank;
029600140122           wErr = 4;
029700140122           exsr  sr_PrintERR;
029800140122         endif;
029900140122
030000140122       EndIf;
030100140115
030200140115       // -?Operazioni finali?
030300140115       exsr sr_RoutEnd;
030400140115
030500140115       //--------------------------------------------------------------
030600140115       //?Operazioni iniziali.                                         ?
030700140115       //--------------------------------------------------------------
030800140115       BEGSR  sr_RoutInz;
030900140115
031000140115         // -?Impostazione opzioni per SQL?
031100140115         exec sql   set  option  DynUsrPrf = *Owner,
031200140115                                 CloSqlCsr = *EndMod;
031300140115
031400140115         // -?Impostazione chiusura?
031500140115         *inLR = *on;
031600140115
031700140115         // -?Reperimento dati job?
031800140115         exsr  sr_DatiJob;
031900140116
032000140116         // -?Reperimento data odierna in formato aaaammgg?
032100140117         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
032200140117         wDate_Iso = %date();
032300140117         wDate_Eur = wDate_Iso;
032400140117         wDate = %dec( wDate_Eur );
032500140116
032600140116         // -?Reperimento orario?
032700140116         wTime = %dec( %time() );
032800140116
032900140116         // -?Reperimento parametri?
033000140116         TNSS09ds = kpjbu;
033100140116
033200140116         // -?Verifica del sistema AS/400 corrente?
033300140116         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
033400140116           exsr  sr_RoutEnd;
033500140116         endif;
033600140116         // -?Reperimento librerie in stanno i files da gestire?
033700140116         //  ?(a seconda del sistema in cui si stà lavorando)?
033800140116         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
033900140116         if  w_iSystem = *zero;
034000140116           exsr  sr_RoutEnd;
034100140116         endif;
034200140117         ds_Libr  = $Libraries(w_iSystem);
034300140117         wLibrAZx = $Libr(1);
034400140115
034500140115         // -?Controllo se utente autorizzato alla funzione?
034600140115         reset  TNTAA1ds;
034700140117         //ITAA1caut = '§utegtc';     ?(già così)?
034800140115         kpjbu     = TNTAA1ds;
034900140115         TNTAA1C ( kpjba );
035000140115         TNTAA1ds = kpjbu;
035100140115         if  oTAA1fabi = 'N';
035200140115           exsr  sr_RoutEnd;
035300140115         endif;
035400140117
035500140117         // -?Copia del WrkF GAITRAAZM/WFPDL00F in QTEMP?
035600140117         if  w_iSystem = 1;         //?(SETRAS)?
035700140117           Qcmd = c_CrtDupObj;
035800140117         else;                      //?(AS888)?
035900140117           Qcmd = %trim( %replace( %trimr(wLibrAZx) : c_CrtDupObj :
036000140117                                   %scan( 'GAITRAAZM' : c_CrtDupObj ) ) );
036100140117         endif;
036200140117         exsr  sr_ExecCmd;
036300140117         if  Qusei <> *blank;
036400140117           $EoF = *on;
036500140117           wErr = 3;
036600140117           exsr  sr_PrintERR;
036700140117           exsr  sr_RoutEnd;
036800140117         endif;
036900140117
037000140117         // -?Apertura del WrkF QTEMP/WFPDL00F?
037100140117         open  WFPDL00F;
037200140115
037300140115         // -?Caricamento filiali gestite dall'utente?
037400140115         clear  TRUL31ds;
037500140115         clear  TRUL31ds2;
037600140115         I31abi = oTAA1cabi;
037700140115         I31cdi = DUTdis;
037800140115         I31car = DUTare;
037900140115         I31cpo = DUTpou;
038000140115         TRUL31R ( kpjba : trul31ds : trul31ds2);
038100140115         if  O31pog <= *zero;
038200140115           exsr  sr_RoutEnd;
038300140115         endif;
038400140116
038500140116         // -?Impostazione campi di stampa in testata?
038600140116         T1Cpgm = SDSpgmName;
038700140116         T1Cdta = wDate;
038800140116         T1Cora = wTime;
038900140121
039000140121         wDate_Eur = %date( SS09dsi : *iso );
039100140122         T1Cdsi    = %dec( wDate_Eur );
039200140121         wDate_Eur = %date( SS09dsf : *iso );
039300140122         T1Cdsf    = %dec( wDate_Eur );
039400140122
039500140122         Select;
039600140122           When  SS09cmu <> *zero;
039700140122             T1parz = 'Comm.le Unificante ' + %editc( SS09cmu : 'X' );
039800140122             clear  keyAZCMM01;
039900140122             k_CMMcod = SS09cmu;
040000140122             chain  %kds( keyAZCMM01 )  AZCMM000;
040100140122             if  NOT %found(AZCMM01L);
040200140122               CMMdes = *all'? ';
040300140122             endif;
040400140122             T1parz = %trimr( T1parz ) + ' ' + CMMdes;
040500140122           When  SS09fil <> *zero;
040600140122             T1parz = 'Comm.li della fil. ' + %editc( SS09fil : 'X' );
040700140122             clear  keyAZORG01;
040800140122             k_ORGfil = SS09fil;
040900140122             chain  %kds( keyAZORG01 )  AZORG;
041000140122             if  NOT %found(AZORG01L)  or  ORGfva <> *blank;
041100140122               ORGdes = *all'? ';
041200140122             endif;
041300140122             T1parz = %trimr( T1parz ) + '  ' + ORGdes;
041400140122           When  SS09car <> *zero;
041500140122             T1parz = 'Comm.li dell''area ' + %editc( SS09car : '1' );
041600140122             ds_TNTBE.TBEke1 = %editc( SS09car : 'X' );
041700140122             if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
041800140122                              *omit : *omit : *omit :
041900140122                              *omit : *omit :
042000140122                              *omit : *omit : *omit : *omit :
042100140122                              *omit : *omit : *omit : *omit :
042200140122                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
042300140122                            = *zero      AND
042400140122                 ds_TNTBE.TBEatb = *blank;
042500140122               ds05   = ds_TNTBE.TBEuni;
042600140122             else;
042700140122               ds05.§05des = *all'? ';
042800140122             endif;
042900140122             T1parz = %trimr( T1parz ) + '  ' + ds05.§05des;
043000140122         EndSl;
043100140121
043200140121         *in25  = *on;
043300140115
043400140115         // -?Pulizia / impostazione iniziale dei campi chiave?
043500140115         clear  keyAZORG01;
043600140115         clear  keyAZCMM01;
043700140115
043800140115       ENDSR;
043900140115
044000140115       //--------------------------------------------------------------
044100140115       //?Reperimento Dati del job (Utente/Operativi).                 ?
044200140115       //--------------------------------------------------------------
044300140115       BEGSR  sr_DatiJob;
044400140115
044500140115         in(e) §AzUte;
044600140115         if NOT %error;
044700140115           in(e) §DatiUte;
044800140115         endif;
044900140115         if %error or RSut = *blank;
045000140115           tibs34r ( tibs34ds );
045100140115           in §AzUte;
045200140115           in §DatiUte;
045300140115         endif;
045400140115
045500140115       ENDSR;
045600140115
045700140115       //--------------------------------------------------------------
045800140116       //?Preparazione 1ª stringa SQL (per l'estrazione da TIPDL).     ?
045900140115       //--------------------------------------------------------------
046000140116       BEGSR  sr_PrepSQL1;
046100140115
046200140116         wSQL1 = 'select '' '', 0, ' + %editc( SS09dsi : 'X' ) + ', '
046300140116                                     + %editc( SS09dsf : 'X' ) + ', +
046400140121                         '' '', 0, 0, PDLksc, PDLrsm, '' '', +
046500140116                         sum(PDLn1p), sum(PDLn2p), sum(PDLn3p), +
046600140116                         sum(PDLn4p), sum(PDLn5p), sum(PDLnop), +
046700140116                         sum(PDLpo5), sum(PDLpdfSp), sum(PDLpdfNr) +
046800140117                    from ' + %trimr(wLibrAZx) + '/TIPDL00F +
046900140120                   where PDLdti >= ' + %editc( SS09dsi : 'X' ) + ' +
047000140120                     and PDLdtf <= ' + %editc( SS09dsf : 'X' ) + ' +
047100140121                   group by PDLksc, PDLrsm +
047200140121                   order by PDLksc, PDLrsm +
047300140116                     for fetch only';
047400140115
047500140115       ENDSR;
047600140115
047700140115       //--------------------------------------------------------------
047800140116       //?Apertura cursore 1.                                          ?
047900140115       //--------------------------------------------------------------
048000140116       BEGSR  sr_OpenCursor1;
048100140115
048200140115         // -?Dichiarazione cursore?
048300140116         exec sql   prepare S1   from :wSQL1;
048400140115         exec sql   declare C1   cursor for S1;
048500140117
048600140117         if  SQLcode < *zero;
048700140117           $EoF = *on;
048800140117           wErr = 1;
048900140117           exsr  sr_PrintERR;
049000140117         endif;
049100140115
049200140115         // -?Apertura del cursore?
049300140115         exec sql   open C1;
049400140115
049500140115         if  SQLcode < *zero;
049600140115           $EoF = *on;
049700140117           wErr = 1;
049800140117           exsr  sr_PrintERR;
049900140115         endif;
050000140115
050100140115       ENDSR;
050200140115
050300140115       //--------------------------------------------------------------
050400140116       //?Chiusura cursore 1.                                          ?
050500140115       //--------------------------------------------------------------
050600140116       BEGSR  sr_CloseCursor1;
050700140115
050800140115         // -?Chiusura del cursore?
050900140115         exec sql   close C1;
051000140117
051100140117         // -?Chiusura del WrkF QTEMP/WFPDL00F?
051200140117         close  WFPDL00F;
051300140115
051400140115       ENDSR;
051500140115
051600140115       //--------------------------------------------------------------
051700140116       //?Lettura cursore 1.                                           ?
051800140115       //--------------------------------------------------------------
051900140116       BEGSR  sr_ReadCursor1;
052000140115
052100140116         clear  TIPDLds;
052200140115
052300140116         exec sql   fetch next   from C1   into :TIPDLds;
052400140115
052500140115         select;
052600140115           when  SQLcode = 100;
052700140115             $EoF = *on;
052800140115           when  SQLcode < *zero;
052900140116             $EoF = *on;
053000140117             wErr = 1;
053100140117             exsr  sr_PrintERR;
053200140115           other;
053300140116             exsr  sr_SelezioneRec;
053400140116             if  $Ok;
053500140116               exsr  sr_WriteWFPDL;
053600140116             endif;
053700140115         endsl;
053800140115
053900140115       ENDSR;
054000140116
054100140116       //--------------------------------------------------------------
054200140116       //?Selezione del singolo record in base alle parzializzazioni.  ?
054300140116       //--------------------------------------------------------------
054400140116       BEGSR  sr_SelezioneRec;
054500140116
054600140116         $Ok = *off;
054700140116
054800140116         // -?Pulizia dei campi nel record?
054900140116         clear  WFPDL000;
055000140116
055100140116
055200140121         // -?Reperimento Cliente Unificante?
055300140121         clear  TIBS10ds;
055400140121         TIBS10ds.D10drf = %dec( wDate_Iso );
055500140121         TIBS10ds.D10tle = 'ST';
055600140121         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
055700140121         TIBS10ds.D10cod = PDLksc;
055800140121         tibs10r ( TIBS10ds );
055900140121         if  TIBS10ds.D10err = *blank  and  TIBS10ds.D10cop > *zero;
056000140121           WPDksu = TIBS10ds.D10cop;
056100140121         else;
056200140121           WPDksu = TIBS10ds.D10cod;
056300140121         endif;
056400140121
056500140121
056600140122         // -?Reperimento Commerciale del Cliente Unificante?
056700140121         clear  TIBS69ds;
056800140121         I69sif = knsif;
056900140121         I69kcc = DUTkci;
057000140121         I69kac = %int( WPDksu );
057100140121         I69kcp = I69kac;
057200140121         tibs69r( TIBS69ds :
057300140121                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
057400140121         if  O69err <> *on;
057500140121           WPDrsu = ACOrag;
057600140121           WPDcmu = CLPage;
057700140121         endif;
057800140122
057900140122
058000140122         // -?Reperimento Commerciale Unificante del Cliente Unificante?
058100140122         k_CMMcod = WPDcmu;
058200140122         chain  %kds( keyAZCMM01 )  AZCMM000;
058300140122         if  %found(AZCMM01L);
058400140122           WPDcmu = CMMuni;
058500140122         endif;
058600140122
058700140122         // -?Decodifica e Reperimento Filiale del Comm.le Unificante?
058800140122         k_CMMcod = WPDcmu;
058900140122         chain  %kds( keyAZCMM01 )  AZCMM000;
059000140122         if  %found(AZCMM01L);
059100140122           WPDcmu = CMMuni;
059200140122           WPDfil = CMMfil;
059300140122         else;
059400140122           clear  CMMdes;
059500140122         endif;
059600140122
059700140122         if  WPDfil = *zero;
059800140122           WPDfil = WPDcmu / 10000;
059900140122         endif;
060000140121
060100140121
060200140122         // -?PARZIALIZZAZIONI:?
060300140122         select;
060400140122           //  ?Commerciale Unificante diverso da quello selezionato?
060500140122           when  SS09cmu <> *zero  and  SS09cmu <> WPDcmu;
060600140122             leavesr;
060700140122           // -?Filiale del commerciale unificante diversa da quella selezionata?
060800140122           when  SS09fil <> *zero  and  SS09fil <> WPDfil;
060900140122             leavesr;
061000140122           // -?Filiale del commerciale unificante non gestibile dall'utente?
061100140122           when  %lookup( %editc( WPDfil : 'X' ) : sk_POg ) = *zero;
061200140122             leavesr;
061300140122         endsl;
061400140122
061500140121
061600140121         // -?Controllo abilitazione dell'utente al commerciale unificante?
061700140121         reset  TNTAA1ds;
061800140121         //ITAA1caut = '§utegtc';   ?(già così)?
061900140121         ITAA1cmm  = WPDcmu;
062000140121         kpjbu     = TNTAA1ds;
062100140121         tntaa1c (kpjba);
062200140121         TNTAA1ds = kpjbu;
062300140121         if  oTAA1fabi = 'N';
062400140121           leavesr;
062500140121         endif;
062600140116
062700140116
062800140121         // -?Reperimento Distretto & Area del Commerciale Unificante?
062900140116         k_ORGfil = WPDfil;
063000140116         chain  %kds( keyAZORG01 )  AZORG;
063100140116         if  %found(AZORG01L)  and  ORGfva = *blank;
063200140116           WPDcar = ORGcar;
063300140121           WPDcdi = ORGfl3;
063400140116         endif;
063500140116
063600140121
063700140121         // -?PARZIALIZZAZIONI:?
063800140116         select;
063900140121           // -?Area del commerciale unificante diversa da quella selezionata?
064000140116           when  SS09car <> *zero  and  SS09car <> WPDcar;
064100140116             leavesr;
064200140121           // -?Area del commerciale unificante non gestibile dall'utente?
064300140116           when  %lookup( %editc( WPDcar : 'X' ) : sk_Car ) = *zero;
064400140116             leavesr;
064500140116         endsl;
064600140116
064700140116
064800140116         // -?Record elaborabile?
064900140116         $Ok = *on;
065000140116
065100140116       ENDSR;
065200140116
065300140116       //--------------------------------------------------------------
065400140116       //?Scrittura del singolo record nel WrkF WFPDL00F.              ?
065500140116       //--------------------------------------------------------------
065600140116       BEGSR  sr_WriteWFPDL;
065700140116
065800140121         // -?Impostazione dati ancora mancanti nel record?
065900140116         WPDeut   = DUTute;
066000140117         WPDedt   = %dec( wDate_Iso );
066100140117         WPDdti   = SS09dsi;
066200140117         WPDdtf   = SS09dsf;
066300140116
066400140121         //WPDcdi   = ......;       ?(già valorizzato)?
066500140121         //WPDcar   = ......;       ?(già valorizzato)?
066600140121         //WPDfil   = ......;       ?(già valorizzato)?
066700140121         //WPDcmu   = ......;       ?(già valorizzato)?
066800140121         //WPDksu   = ......;       ?(già valorizzato)?
066900140121         //WPDrsu   = ......;       ?(già valorizzato)?
067000140116
067100140116         WPDksc   = PDLksc;
067200140116         WPDrsm   = PDLrsm;
067300140121         //WPDtmd   = PDLtmd;
067400140116
067500140116         WPDn1p   = PDLn1p;
067600140116         WPDn2p   = PDLn2p;
067700140116         WPDn3p   = PDLn3p;
067800140116         WPDn4p   = PDLn4p;
067900140116         WPDn5p   = PDLn5p;
068000140116         WPDnop   = PDLnop;
068100140116         WPDpo5   = PDLpo5;
068200140116         WPDpdfSp = PDLpdfSp;
068300140116         WPDpdfNr = PDLpdfNr;
068400140116
068500140116         // -?Scrittura nuovo rec. nel WrkF WFPDL00F?
068600140116         write  WFPDL000;
068700140219
068800140219         $Almeno1rec = *on;
068900140116
069000140116       ENDSR;
069100140116
069200140116       //--------------------------------------------------------------
069300140116       //?Preparazione 2ª stringa SQL (per la stampa).                 ?
069400140116       //--------------------------------------------------------------
069500140116       BEGSR  sr_PrepSQL2;
069600140116
069700140121         wSQL2 = 'select * +
069800140121                    from QTEMP/WFPDL00F +
069900140121                   order by WPDcmu, WPDksu, WPDksc +
070000140121                     for fetch only';
070100140116
070200140116       ENDSR;
070300140116
070400140116       //--------------------------------------------------------------
070500140116       //?Apertura cursore 2.                                          ?
070600140116       //--------------------------------------------------------------
070700140116       BEGSR  sr_OpenCursor2;
070800140116
070900140116         // -?Dichiarazione cursore?
071000140116         exec sql   prepare S2   from :wSQL2;
071100140116         exec sql   declare C2   cursor for S2;
071200140117
071300140117         if  SQLcode < *zero;
071400140117           $EoF = *on;
071500140117           wErr = 1;
071600140117           exsr  sr_PrintERR;
071700140117         endif;
071800140116
071900140116         // -?Apertura del cursore?
072000140116         exec sql   open C2;
072100140116
072200140116         if  SQLcode < *zero;
072300140116           $EoF = *on;
072400140117           wErr = 1;
072500140117           exsr  sr_PrintERR;
072600140116         endif;
072700140116
072800140116       ENDSR;
072900140116
073000140116       //--------------------------------------------------------------
073100140116       //?Chiusura cursore 2.                                          ?
073200140116       //--------------------------------------------------------------
073300140116       BEGSR  sr_CloseCursor2;
073400140122
073500140122         // -?Stampa testata per Totali Generali?
073600140122         write  SS09TX1;
073700140122         write  SS09TX4;
073800140116
073900140116         // -?Stampa Totali Generali?
074000140116         write  SS09TOT;
074100140116
074200140116         // -?Chiusura del cursore?
074300140116         exec sql   close C2;
074400140116
074500140116       ENDSR;
074600140116
074700140116       //--------------------------------------------------------------
074800140116       //?Lettura cursore 2.                                           ?
074900140116       //--------------------------------------------------------------
075000140116       BEGSR  sr_ReadCursor2;
075100140116
075200140116         clear  WFPDLds;
075300140116
075400140117         exec sql   fetch next   from C2   into :WFPDLds;
075500140116
075600140116         select;
075700140116           when  SQLcode = 100;
075800140116             $EoF = *on;
075900140116           when  SQLcode < *zero;
076000140116             $EoF = *on;
076100140117             wErr = 1;
076200140117             exsr  sr_PrintERR;
076300140116           other;
076400140116             exsr  sr_PrintWFPDL;
076500140116         endsl;
076600140116
076700140116       ENDSR;
076800140116
076900140116       //--------------------------------------------------------------
077000140116       //?Stampa singolo cliente dal file WFPDL00F.                    ?
077100140116       //--------------------------------------------------------------
077200140116       BEGSR  sr_PrintWFPDL;
077300140116
077400140121         $CmmU = (WPDcmu <> pCMUcod);
077500140117
077600140121         // -?Rottura per Commerciale Unificante?
077700140121         IF  $CmmU;
077800140121
077900140121           *in25 = *on;
078000140121           exsr  sr_DecodCMU;
078100140121
078200140122           // -?Stampa testata?
078300140122           write  SS09TX1;
078400140121           write  SS09TX3;
078500140121           write  SS09TX4;
078600140121           *in25 = *off;
078700140121
078800140121         ENDIF;
078900140117
079000140117
079100140116         // -?Rottura per Cliente Unificante?
079200140116         If  WPDksu <> UPDksu  or  WPDrsu <> UPDrsu;
079300140116           exsr  sr_PrintCliUni;
079400140116         EndIf;
079500140116
079600140116
079700140116         // -?Incremento Totali per Totale Generale?
079800140116         TPDn1p   += WPDn1p;
079900140116         TPDn2p   += WPDn2p;
080000140116         TPDn3p   += WPDn3p;
080100140116         TPDn4p   += WPDn4p;
080200140116         TPDn5p   += WPDn5p;
080300140116         TPDnop   += WPDnop;
080400140116         TPDpo5   += WPDpo5;
080500140116         TPDpdfSp += WPDpdfSp;
080600140116         TPDpdfNr += WPDpdfNr;
080700140116
080800140116
080900140116         // -?Impostazione dati in stampa?
081000140116         //WPDksc   = WPDksc;
081100140116         //WPDrsm   = WPDrsm;
081200140116         //WPDtmd   = WPDtmd;
081300140116         CPDn1p   = WPDn1p;
081400140116         CPDn2p   = WPDn2p;
081500140116         CPDn3p   = WPDn3p;
081600140116         CPDn4p   = WPDn4p;
081700140116         CPDn5p   = WPDn5p;
081800140116         CPDnop   = WPDnop;
081900140116         CPDpo5   = WPDpo5;
082000140116         CPDpdfSp = WPDpdfSp;
082100140116         CPDpdfNr = WPDpdfNr;
082200140116
082300140116
082400140120         // -?Stampa del singolo cliente / tipo modulo?
082500140117         if  *in25;
082600140117           write  SS09TX1;
082700140117           write  SS09TX4;
082800140117           *in25 = *off;
082900140117         endif;
083000140116         write  SS09ksc;
083100140116
083200140116       ENDSR;
083300140116
083400140116       //--------------------------------------------------------------
083500140116       //?Decodifica Commerciale Unificante del commerciale.           ?
083600140116       //--------------------------------------------------------------
083700140116       BEGSR  sr_DecodCMU;
083800140116
083900140117         k_CMMcod = WPDcmu;
084000140117         chain  %kds( keyAZCMM01 )  AZCMM000;
084100140117
084200140117         if  %found(AZCMM01L);
084300140117           WPDcmu = CMMuni;
084400140117         else;
084500140117           clear  CMMdes;
084600140117         endif;
084700140117
084800140117         pCMUcod = WPDcmu;
084900140117         pCMUdes = CMMdes;
085000140116
085100140116       ENDSR;
085200140116
085300140116       //--------------------------------------------------------------
085400140116       //?Calcolo totali e Stampa x Cliente Unificante.                ?
085500140116       //--------------------------------------------------------------
085600140116       BEGSR  sr_PrintCliUni;
085700140116
085800140116         UPDksu    = WPDksu;
085900140116         UPDrsu    = WPDrsu;
086000140116
086100140116
086200140116         wSQL3 = 'select sum(WPDn1p), sum(WPDn2p), sum(WPDn3p), +
086300140116                         sum(WPDn4p), sum(WPDn5p), sum(WPDnop), +
086400140116                         sum(WPDpo5), sum(WPDpdfSp), sum(WPDpdfNr) +
086500140117                    from QTEMP/WFPDL00F +
086600140116                   where WPDksu = ' + %editc( WPDksu : 'X' ) + ' +
086700140116                   group by WPDksu +
086800140116                     for fetch only';
086900140116
087000140116         // -?Dichiarazione cursore?
087100140116         exec sql   prepare S3   from :wSQL3;
087200140116         exec sql   declare C3   cursor for S3;
087300140116
087400140116         // -?Apertura del cursore?
087500140116         exec sql   open C3;
087600140116
087700140116         if  SQLcode < *zero;
087800140116           $EoF = *on;
087900140117           wErr = 1;
088000140117           exsr  sr_PrintERR;
088100140116         endif;
088200140116
088300140116         // -?Lettura del cursore?
088400140116         clear  WTPDLds;
088500140116         exec sql   fetch next   from C3   into :WTPDLds;
088600140116
088700140116         if  SQLcode < *zero;
088800140116           $EoF = *on;
088900140117           wErr = 1;
089000140117           exsr  sr_PrintERR;
089100140116         endif;
089200140116
089300140116         // -?Chiusura del cursore?
089400140116         exec sql   close C3;
089500140116
089600140116
089700140116         // -?Stampa totali per cliente unificante?
089800140117         if  *in25;
089900140117           write  SS09TX1;
090000140117           write  SS09TX4;
090100140117           *in25 = *off;
090200140117         endif;
090300140116         write  SS09ksu;
090400140116
090500140116       ENDSR;
090600140115
090700140115       //--------------------------------------------------------------
090800140115       //?Stampa segnalazione dell'errore rilevato via SQL             ?
090900140115       //--------------------------------------------------------------
091000140117       BEGSR  sr_PrintERR;
091100140115
091200140115         // -?Stampa del Dump?
091300140115         Dump(A);
091400140115
091500140115         // -?Stampa del Job-Log?
091600140115         Qcmd = 'DSPJOBLOG job(*) output(*print)';
091700140117         exsr  sr_ExecCmd;
091800140115
091900140115         // -?Apertura del printer-file?
092000140115         if  Not %open(QSYSPRT);
092100140117           Qcmd = c_OvrPrtF;
092200140117           exsr  sr_ExecCmd;
092300140115           open  QSYSPRT;
092400140117           except  PRTtxt;
092500140115         endif;
092600140117
092700140117         SELECT;
092800140117
092900140117           // -?Stampa dell'errore in COPIA iniziale di WFPDL00F?
093000140117           //  ?da GAITRAAZM in QTEMP (e chiusura del *pgm)?
093100140117           //WHEN  SQlcode >= *zero  and  Not %open(WFPDL00F);
093200140117           WHEN  wErr = 3;
093300140117             if  *inOF;
093400140117               except  PRTtxt;
093500140117               *inOF = *off;
093600140117             endif;
093700140117             except  PRTerr3;
093800140117             *inOF = *off;
093900140117             // -?Chiusura del programma (DOPO)?
094000140117             exsr  sr_RoutEnd;
094100140117
094200140117           // -?Stampa dell'errore SQL (e chiusura del *pgm)?
094300140117           //WHEN  SQlcode <  *zero;
094400140117           WHEN  wErr = 1;
094500140117             clear  ds_Temp;
094600140117             ds_Temp = wSQL1;
094700140117             if  *inOF;
094800140117               except  PRTtxt;
094900140117               *inOF = *off;
095000140117             endif;
095100140117             except  PRTerr1;
095200140117             *inOF = *off;
095300140117             For  xx = 1  To  %elem($Temp);
095400140117               if  $Temp(xx) = *blank;
095500140117                 leave;
095600140117               endif;
095700140117               if  *inOF;
095800140117                 except  PRTtxt;
095900140117                 *inOF = *off;
096000140117               endif;
096100140117               except  PRTerr2;
096200140117             EndFor;
096300140117             // -?Chiusura del programma (DOPO)?
096400140117             exsr  sr_RoutEnd;
096500140117
096600140117           // -?Stampa dell'errore in COPIA finale di WFPDL00F?
096700140117           //  ?da QTEMP in GAITRAAZM (e proseguimento)?
096800140117           //WHEN  SQlcode >= *zero  and  %open(WFPDL00F);
096900140117           WHEN  wErr = 4;
097000140117             if  *inOF;
097100140117               except  PRTtxt;
097200140117               *inOF = *off;
097300140117             endif;
097400140117             except  PRTerr4;
097500140117             *inOF = *off;
097600140117             leavesr;
097700140117
097800140117         ENDSL;
097900140115
098000140115       ENDSR;
098100140115
098200140115       //--------------------------------------------------------------
098300140115       //?Operazioni finali.                                           ?
098400140115       //--------------------------------------------------------------
098500140115       BEGSR  sr_RoutEnd;
098600140115
098700140115         // -?Chiusura applicazioni?
098800140122         cloTabella();
098900140122
099000140116         reset  TIBS69ds;
099100140116         tibs69r( tibs69ds :
099200140116                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
099300140115
099400140115         // -?Chiusura printer-file?
099500140115         if  %open(QSYSPRT);
099600140115           close  QSYSPRT;
099700140117           Qcmd = c_DltOvr;
099800140117           exsr  sr_ExecCmd;
099900140115         endif;
100000140115
100100140115         // -?Uscita dal *pgm?
100200140115         return;
100300140115
100400140115       ENDSR;
100500140117
100600140117       //--------------------------------------------------------------
100700140117       //?Esecuzione del comando (già impostato).                      ?
100800140117       //--------------------------------------------------------------
100900140117       BEGSR  sr_ExecCmd;
101000140117
101100140117         clear Qcap0100;
101200140117         Qcabcsdh = *off;
101300140117         Qcapa    = *off;
101400140117         Qcacmdss = *off;
101500140117         Qcaerved = *allX'00';
101600140117
101700140117         clear Qusec;
101800140117         Qusbprv  = %size(Qusec);
101900140117
102000140117         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
102100140117                           %size(Qcap0100) : 'CPOP0100' : *omit :
102200140117                           0 : 0 : Qusec);
102300140117
102400140117         //if  Qusei <> *blank;
102500140117         //  ...;
102600140117         //endif;
102700140117
102800140117       ENDSR;
102900140115
103000140115      /end-free
103100140116
103200140116       //--------------------------------------------------------------
103300140116       //?S P O O L - F I L E S                                        ?
103400140116       //--------------------------------------------------------------
103500140116
103600140116     oQSYSPRT   e            PRTtxt            1
103700140116     o                       RSUT
103800140116     o                                        +   6 'LISTA ERRORI RILE-
103900140116     o                                              VATI IN FASE DI ST-
104000140116     o                                              AMPA N° PAGINE DI -
104100140116     o                                              PACKING-LIST IN LD-
104200140116     o                                              V'
104300140116     o                       SDSpgmName       +   6
104400140116     o                       wDate         y  +   3
104500140117     o          e            PRTtxt      0
104600140117     o                                          +26 'LISTA ERRORI RILE-
104700140117     o                                              VATI IN FASE DI ST-
104800140117     o                                              AMPA N° PAGINE DI -
104900140117     o                                              PACKING-LIST IN LD-
105000140117     o                                              V'
105100140116     o          e            PRTtxt      1
105200140116     o                       KNSIF
105300140116     o                       KNMUS            +   1
105400140117     o                                        +   5 '------------------
105500140116     o                                              -------------------
105600140116     o                                              -------------------
105700140116     o                                              -------------------
105800140117     o                                              -'
105900140116     o                                        +   6 'Pag.'
106000140116     o                       Page          z  +   0
106100140116     o                       wTime            +   5 '  :  :  '
106200140117      *
106300140116     o          e            PRTerr1     2
106400140116     o                                              'RILEVATO ERRORE: +
106500140116     o                                               SQLCODE'
106600140116     o                       SQLcode       k  +   1
106700140116     o                                        +   1 'SQLSTATE'
106800140116     o                       SQLstate         +   1
106900140116     o                                        +   1 'DURANTE L''ESECUZ-
107000140116     o                                              IONE DEL SEGUENTE -
107100140116     o                                              COMANDO SQL:'
107200140116     o          e            PRTerr1     0  1
107300140116     o                                              'RILEVATO ERRORE: +
107400140116     o                                               SQLCODE'
107500140116     o                       SQLcode       k  +   1
107600140116     o                                        +   1 'SQLSTATE'
107700140116     o                       SQLstate         +   1
107800140116     o          e            PRTerr2     1
107900140116     o                       $Temp(xx)        +   3
108000140117      *
108100140117     o          e            PRTerr3     2
108200140117     o                                              'NON RIUSCITA LA +
108300140117     o                                               COPIA DEL WORK-+
108400140117     o                                               FILE WFPDL00F +
108500140117     o                                               NELLA LIBR. QTEMP'
108600140117      *
108700140117     o          e            PRTerr4     2
108800140117     o                                              'NON RIUSCITA LA +
108900140117     o                                               COPIA DEL WORK-+
109000140117     o                                               FILE WFPDL00F +
109100140117     o                                               NELLA LIBR.'
109200140117     o                       wLibrAZx         +   1
109300140116
109400140116       //--------------------------------------------------------------
109500140116       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
109600140116       //--------------------------------------------------------------
109700140116
109800140117** -?$iSystem / $Libraries:?Sistema AS/400 & Libreria.?
109900140116SETRAS  GAITRAAZM
110000140116AS888   GAITRAAZP
