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
017100151102     d wDateIso1       s               d   inz  datfmt(*iso)
017200151102     d wDateIso2       s               d   inz  datfmt(*iso)
017300140115
017400140115       //--------------------------------------------------------------
017500140115       //?Definizione prototipi procedure.                             ?
017600140115       //--------------------------------------------------------------
017700140116
017800140116       // -?Reperimento NETA sistema AS/400 corrente?
017900140116     d currSysNeta     s              8a   inz
018000140116      /copy gaitrasrc/srcProtoPR,UBRTVNETA
018100140115
018200140115       // -?Reperimento dati utente?
018300140115     d TIBS34ds      e ds                  inz
018400140115      /copy gaitrasrc/srcProtoPR,TIBS34R
018500140115
018600140115       // -?Controllo abilitazioni utente?
018700140115     d TNTAA1ds      e ds                  inz
018800140115     d   ITAA1caut   e                     inz('§utegtc')
018900140115      /copy gaitrasrc/srcProtoPR,TNTAA1C
019000140115
019100140115       // -?Caricamento Filiali/Aree gestite dall'utente?
019200140115     d TRUL31ds      e ds                  inz
019300140115     d   sk_POg               10    759    inz   dim(250)
019400140115     d TRUL31ds2     e ds                  inz
019500140115     d   sk_Car               22    171    inz   dim(50)
019600140115     d trul31r         pr                  extpgm('TRUL31R')
019700140115     d   kpjba                             likeds(KPJBA)
019800140115     d   trul31ds                          likeds(TRUL31ds)
019900140115     d   trul31ds2                         likeds(TRUL31ds2)
020000151102
020100151104       // -?Calcolo giorni lavorativi tra due date?
020200151104     d xsrLav8ds       ds                  inz  qualified
020300151104     d   DataIni                      8s 0 inz
020400151104     d   DataFin                      8s 0 inz
020500151104     d   GiorniLav                    5s 0 inz
020600151104     d xsrLav8         pr                  extpgm('XSRLAV8')
020700151104     d   xsrLav8ds                         likeds(xsrLAV8ds)
020800140116
020900140116       // -?Ricerca unificante padre?
021000140116      /copy gaitrasrc/srcProtoPI,TIBS10R
021100140116      /copy gaitrasrc/srcProtoPR,TIBS10R
021200140116
021300140116       // -?Controllo/Decodifica cliente?
021400140116      /copy gaitrasrc/srcProtoPI,TIBS69R
021500140116      /copy gaitrasrc/srcProtoPR,TIBS69R
021600140122
021700140122       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
021800140122      /copy gaitrasrc/srcProtoPI,TRULTAB
021900140122      /copy gaitrasrc/srcProtoPR,TRULTAB
022000140117
022100140117       // -?Parametri API QCAPCMD (Process Commands)?
022200140117     d Qcmd            s           2048    inz  varying
022300140117      /copy qSysInc/qRpgleSrc,QCAPCMD
022400140117       // -?API QCAPCMD (Process Commands)?
022500140117      /copy gaitrasrc/srcProtoPR,QCAPCMD
022600140117
022700140117       // -?Parametri gestione errori API.?
022800140117      /copy qsysinc/qrpglesrc,QUSEC
022900140115
023000140115       //--------------------------------------------------------------
023100140115       //?Definizione key-list.                                        ?
023200140115       //--------------------------------------------------------------
023300140115
023400140115       // -?File AZORG01L?
023500140115     d keyAZORG01    e ds                  extname(AZORG01L : *key)
023600140115     d                                     prefix(k_)   inz
023700140115
023800140115       // -?File AZCMM01L?
023900140115     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
024000140115     d                                     prefix(k_)   inz
024100140115
024200140115       //--------------------------------------------------------------
024300140115       //?Riepilogo indicatori utilizzati.                             ?
024400140115       //--------------------------------------------------------------
024500140115
024600140115
024700140115       //--------------------------------------------------------------
024800140115       //?M A I N - L I N E                                            ?
024900140115       //--------------------------------------------------------------
025000140115
025100140115     c     *Entry        plist
025200140115     c                   parm                    KPJBA
025300140115
025400140115      /free
025500140115
025600140115       // -?Operazioni iniziali?
025700140115       exsr  sr_RoutInz;
025800140115
025900140116       // -?Preparazione stringhe SQL?
026000140116       exsr  sr_PrepSQL1;
026100140116       exsr  sr_PrepSQL2;
026200140115
026300140117       // -?Ciclo di elaborazione TIPDL00F per creare QTEMP/WFPDL00F?
026400140116       reset  $EoF;
026500140116       exsr  sr_OpenCursor1;
026600140115
026700140117       DoW  $EoF = *off;
026800140116         exsr  sr_ReadCursor1;
026900140117       EndDo;
027000140115
027100140116       exsr  sr_CloseCursor1;
027200140116
027300140117       // -?Ciclo di lettura QTEMP/WFPDL00F per stampa?
027400151022       If  SS09outS <> *blank;
027500140121
027600140121         reset  $EoF;
027700140121         exsr  sr_OpenCursor2;
027800140121
027900140121         DoW  $EoF = *off;
028000140121           exsr  sr_ReadCursor2;
028100140121         EndDo;
028200140121
028300140121         exsr  sr_CloseCursor2;
028400140121
028500140121       EndIf;
028600140122
028700140122       // -?Copia del WrkF QTEMP/WFPDL00F in GAITRAAZM?
028800140122       //  ?SE estratti dati?
028900151022       If  SS09outF = 'S'  and  $Almeno1rec;
029000140122
029100140122         if  w_iSystem = 1;       //?(SETRAS)?
029200140122           Qcmd = c_CpyF;
029300140122         else;                    //?(AS888)?
029400140122           Qcmd = %trim( %replace( %trimr(wLibrAZx) : c_CpyF :
029500140122                                   %scan( 'GAITRAAZM' : c_CpyF ) ) );
029600140122         endif;
029700140122         exsr  sr_ExecCmd;
029800140122
029900140122         if  Qusei <> *blank;
030000140122           wErr = 4;
030100140122           exsr  sr_PrintERR;
030200140122         endif;
030300140122
030400140122       EndIf;
030500140115
030600140115       // -?Operazioni finali?
030700140115       exsr sr_RoutEnd;
030800140115
030900140115       //--------------------------------------------------------------
031000140115       //?Operazioni iniziali.                                         ?
031100140115       //--------------------------------------------------------------
031200140115       BEGSR  sr_RoutInz;
031300140115
031400140115         // -?Impostazione opzioni per SQL?
031500140115         exec sql   set  option  DynUsrPrf = *Owner,
031600140115                                 CloSqlCsr = *EndMod;
031700140115
031800140115         // -?Impostazione chiusura?
031900140115         *inLR = *on;
032000140115
032100140115         // -?Reperimento dati job?
032200140115         exsr  sr_DatiJob;
032300140116
032400140116         // -?Reperimento data odierna in formato aaaammgg?
032500140117         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
032600140117         wDate_Iso = %date();
032700140117         wDate_Eur = wDate_Iso;
032800140117         wDate = %dec( wDate_Eur );
032900140116
033000140116         // -?Reperimento orario?
033100140116         wTime = %dec( %time() );
033200140116
033300140116         // -?Reperimento parametri?
033400140116         TNSS09ds = kpjbu;
033500140116
033600140116         // -?Verifica del sistema AS/400 corrente?
033700140116         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
033800140116           exsr  sr_RoutEnd;
033900140116         endif;
034000140116         // -?Reperimento librerie in stanno i files da gestire?
034100140116         //  ?(a seconda del sistema in cui si stà lavorando)?
034200140116         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
034300140116         if  w_iSystem = *zero;
034400140116           exsr  sr_RoutEnd;
034500140116         endif;
034600140117         ds_Libr  = $Libraries(w_iSystem);
034700140117         wLibrAZx = $Libr(1);
034800140115
034900140115         // -?Controllo se utente autorizzato alla funzione?
035000140115         reset  TNTAA1ds;
035100140117         //ITAA1caut = '§utegtc';     ?(già così)?
035200140115         kpjbu     = TNTAA1ds;
035300140115         TNTAA1C ( kpjba );
035400140115         TNTAA1ds = kpjbu;
035500140115         if  oTAA1fabi = 'N';
035600140115           exsr  sr_RoutEnd;
035700140115         endif;
035800140117
035900140117         // -?Copia del WrkF GAITRAAZM/WFPDL00F in QTEMP?
036000140117         if  w_iSystem = 1;         //?(SETRAS)?
036100140117           Qcmd = c_CrtDupObj;
036200140117         else;                      //?(AS888)?
036300140117           Qcmd = %trim( %replace( %trimr(wLibrAZx) : c_CrtDupObj :
036400140117                                   %scan( 'GAITRAAZM' : c_CrtDupObj ) ) );
036500140117         endif;
036600140117         exsr  sr_ExecCmd;
036700140117         if  Qusei <> *blank;
036800140117           $EoF = *on;
036900140117           wErr = 3;
037000140117           exsr  sr_PrintERR;
037100140117           exsr  sr_RoutEnd;
037200140117         endif;
037300151102
037400151102         // -?Calcolo del n° giorni lavorativi tra le 2 date (dal-al)?
037500151102         //   ·?calcolo della metà del n° giorni tra le 2 date (dal-al)?
037600151104         xsrLav8ds.DataIni = SS09dsi;
037700151104         xsrLav8ds.DataFin = SS09dsf;
037800151104         clear  xsrLav8ds.GiorniLav;
037900151104         xsrLav8 ( xsrLav8ds );
038000140117
038100140117         // -?Apertura del WrkF QTEMP/WFPDL00F?
038200140117         open  WFPDL00F;
038300140115
038400140115         // -?Caricamento filiali gestite dall'utente?
038500140115         clear  TRUL31ds;
038600140115         clear  TRUL31ds2;
038700140115         I31abi = oTAA1cabi;
038800140115         I31cdi = DUTdis;
038900140115         I31car = DUTare;
039000140115         I31cpo = DUTpou;
039100140115         TRUL31R ( kpjba : trul31ds : trul31ds2);
039200140115         if  O31pog <= *zero;
039300140115           exsr  sr_RoutEnd;
039400140115         endif;
039500140116
039600140116         // -?Impostazione campi di stampa in testata?
039700140116         T1Cpgm = SDSpgmName;
039800140116         T1Cdta = wDate;
039900140116         T1Cora = wTime;
040000140121
040100140121         wDate_Eur = %date( SS09dsi : *iso );
040200140122         T1Cdsi    = %dec( wDate_Eur );
040300140121         wDate_Eur = %date( SS09dsf : *iso );
040400140122         T1Cdsf    = %dec( wDate_Eur );
040500140122
040600140122         Select;
040700140122           When  SS09cmu <> *zero;
040800140122             T1parz = 'Comm.le Unificante ' + %editc( SS09cmu : 'X' );
040900140122             clear  keyAZCMM01;
041000140122             k_CMMcod = SS09cmu;
041100140122             chain  %kds( keyAZCMM01 )  AZCMM000;
041200140122             if  NOT %found(AZCMM01L);
041300140122               CMMdes = *all'? ';
041400140122             endif;
041500140122             T1parz = %trimr( T1parz ) + ' ' + CMMdes;
041600140122           When  SS09fil <> *zero;
041700140122             T1parz = 'Comm.li della fil. ' + %editc( SS09fil : 'X' );
041800140122             clear  keyAZORG01;
041900140122             k_ORGfil = SS09fil;
042000140122             chain  %kds( keyAZORG01 )  AZORG;
042100140122             if  NOT %found(AZORG01L)  or  ORGfva <> *blank;
042200140122               ORGdes = *all'? ';
042300140122             endif;
042400140122             T1parz = %trimr( T1parz ) + '  ' + ORGdes;
042500140122           When  SS09car <> *zero;
042600140122             T1parz = 'Comm.li dell''area ' + %editc( SS09car : '1' );
042700140122             ds_TNTBE.TBEke1 = %editc( SS09car : 'X' );
042800140122             if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
042900140122                              *omit : *omit : *omit :
043000140122                              *omit : *omit :
043100140122                              *omit : *omit : *omit : *omit :
043200140122                              *omit : *omit : *omit : *omit :
043300140122                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
043400140122                            = *zero      AND
043500140122                 ds_TNTBE.TBEatb = *blank;
043600140122               ds05   = ds_TNTBE.TBEuni;
043700140122             else;
043800140122               ds05.§05des = *all'? ';
043900140122             endif;
044000140122             T1parz = %trimr( T1parz ) + '  ' + ds05.§05des;
044100140122         EndSl;
044200140121
044300140121         *in25  = *on;
044400140115
044500140115         // -?Pulizia / impostazione iniziale dei campi chiave?
044600140115         clear  keyAZORG01;
044700140115         clear  keyAZCMM01;
044800140115
044900140115       ENDSR;
045000140115
045100140115       //--------------------------------------------------------------
045200140115       //?Reperimento Dati del job (Utente/Operativi).                 ?
045300140115       //--------------------------------------------------------------
045400140115       BEGSR  sr_DatiJob;
045500140115
045600140115         in(e) §AzUte;
045700140115         if NOT %error;
045800140115           in(e) §DatiUte;
045900140115         endif;
046000140115         if %error or RSut = *blank;
046100140115           tibs34r ( tibs34ds );
046200140115           in §AzUte;
046300140115           in §DatiUte;
046400140115         endif;
046500140115
046600140115       ENDSR;
046700140115
046800140115       //--------------------------------------------------------------
046900140116       //?Preparazione 1ª stringa SQL (per l'estrazione da TIPDL).     ?
047000140115       //--------------------------------------------------------------
047100140116       BEGSR  sr_PrepSQL1;
047200140115
047300140116         wSQL1 = 'select '' '', 0, ' + %editc( SS09dsi : 'X' ) + ', '
047400140116                                     + %editc( SS09dsf : 'X' ) + ', +
047500140121                         '' '', 0, 0, PDLksc, PDLrsm, '' '', +
047600140116                         sum(PDLn1p), sum(PDLn2p), sum(PDLn3p), +
047700140116                         sum(PDLn4p), sum(PDLn5p), sum(PDLnop), +
047800140116                         sum(PDLpo5), sum(PDLpdfSp), sum(PDLpdfNr) +
047900140117                    from ' + %trimr(wLibrAZx) + '/TIPDL00F +
048000140120                   where PDLdti >= ' + %editc( SS09dsi : 'X' ) + ' +
048100140120                     and PDLdtf <= ' + %editc( SS09dsf : 'X' ) + ' +
048200140121                   group by PDLksc, PDLrsm +
048300140121                   order by PDLksc, PDLrsm +
048400140116                     for fetch only';
048500140115
048600140115       ENDSR;
048700140115
048800140115       //--------------------------------------------------------------
048900140116       //?Apertura cursore 1.                                          ?
049000140115       //--------------------------------------------------------------
049100140116       BEGSR  sr_OpenCursor1;
049200140115
049300140115         // -?Dichiarazione cursore?
049400140116         exec sql   prepare S1   from :wSQL1;
049500140115         exec sql   declare C1   cursor for S1;
049600140117
049700140117         if  SQLcode < *zero;
049800140117           $EoF = *on;
049900140117           wErr = 1;
050000140117           exsr  sr_PrintERR;
050100140117         endif;
050200140115
050300140115         // -?Apertura del cursore?
050400140115         exec sql   open C1;
050500140115
050600140115         if  SQLcode < *zero;
050700140115           $EoF = *on;
050800140117           wErr = 1;
050900140117           exsr  sr_PrintERR;
051000140115         endif;
051100140115
051200140115       ENDSR;
051300140115
051400140115       //--------------------------------------------------------------
051500140116       //?Chiusura cursore 1.                                          ?
051600140115       //--------------------------------------------------------------
051700140116       BEGSR  sr_CloseCursor1;
051800140115
051900140115         // -?Chiusura del cursore?
052000140115         exec sql   close C1;
052100140117
052200140117         // -?Chiusura del WrkF QTEMP/WFPDL00F?
052300140117         close  WFPDL00F;
052400140115
052500140115       ENDSR;
052600140115
052700140115       //--------------------------------------------------------------
052800140116       //?Lettura cursore 1.                                           ?
052900140115       //--------------------------------------------------------------
053000140116       BEGSR  sr_ReadCursor1;
053100140115
053200140116         clear  TIPDLds;
053300140115
053400140116         exec sql   fetch next   from C1   into :TIPDLds;
053500140115
053600140115         select;
053700140115           when  SQLcode = 100;
053800140115             $EoF = *on;
053900140115           when  SQLcode < *zero;
054000140116             $EoF = *on;
054100140117             wErr = 1;
054200140117             exsr  sr_PrintERR;
054300140115           other;
054400140116             exsr  sr_SelezioneRec;
054500140116             if  $Ok;
054600140116               exsr  sr_WriteWFPDL;
054700140116             endif;
054800140115         endsl;
054900140115
055000140115       ENDSR;
055100140116
055200140116       //--------------------------------------------------------------
055300140116       //?Selezione del singolo record in base alle parzializzazioni.  ?
055400140116       //--------------------------------------------------------------
055500140116       BEGSR  sr_SelezioneRec;
055600140116
055700140116         $Ok = *off;
055800140116
055900140116         // -?Pulizia dei campi nel record?
056000140116         clear  WFPDL000;
056100140116
056200140116
056300140121         // -?Reperimento Cliente Unificante?
056400140121         clear  TIBS10ds;
056500140121         TIBS10ds.D10drf = %dec( wDate_Iso );
056600140121         TIBS10ds.D10tle = 'ST';
056700140121         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
056800140121         TIBS10ds.D10cod = PDLksc;
056900140121         tibs10r ( TIBS10ds );
057000140121         if  TIBS10ds.D10err = *blank  and  TIBS10ds.D10cop > *zero;
057100140121           WPDksu = TIBS10ds.D10cop;
057200140121         else;
057300140121           WPDksu = TIBS10ds.D10cod;
057400140121         endif;
057500140121
057600140121
057700140122         // -?Reperimento Commerciale del Cliente Unificante?
057800140121         clear  TIBS69ds;
057900140121         I69sif = knsif;
058000140121         I69kcc = DUTkci;
058100140121         I69kac = %int( WPDksu );
058200140121         I69kcp = I69kac;
058300140121         tibs69r( TIBS69ds :
058400140121                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
058500140121         if  O69err <> *on;
058600140121           WPDrsu = ACOrag;
058700140121           WPDcmu = CLPage;
058800140121         endif;
058900140122
059000140122
059100140122         // -?Reperimento Commerciale Unificante del Cliente Unificante?
059200140122         k_CMMcod = WPDcmu;
059300140122         chain  %kds( keyAZCMM01 )  AZCMM000;
059400140122         if  %found(AZCMM01L);
059500140122           WPDcmu = CMMuni;
059600140122         endif;
059700140122
059800140122         // -?Decodifica e Reperimento Filiale del Comm.le Unificante?
059900140122         k_CMMcod = WPDcmu;
060000140122         chain  %kds( keyAZCMM01 )  AZCMM000;
060100140122         if  %found(AZCMM01L);
060200140122           WPDcmu = CMMuni;
060300140122           WPDfil = CMMfil;
060400140122         else;
060500140122           clear  CMMdes;
060600140122         endif;
060700140122
060800140122         if  WPDfil = *zero;
060900140122           WPDfil = WPDcmu / 10000;
061000140122         endif;
061100140121
061200140121
061300140122         // -?PARZIALIZZAZIONI:?
061400140122         select;
061500140122           //  ?Commerciale Unificante diverso da quello selezionato?
061600140122           when  SS09cmu <> *zero  and  SS09cmu <> WPDcmu;
061700140122             leavesr;
061800140122           // -?Filiale del commerciale unificante diversa da quella selezionata?
061900140122           when  SS09fil <> *zero  and  SS09fil <> WPDfil;
062000140122             leavesr;
062100140122           // -?Filiale del commerciale unificante non gestibile dall'utente?
062200140122           when  %lookup( %editc( WPDfil : 'X' ) : sk_POg ) = *zero;
062300140122             leavesr;
062400140122         endsl;
062500140122
062600140121
062700140121         // -?Controllo abilitazione dell'utente al commerciale unificante?
062800140121         reset  TNTAA1ds;
062900140121         //ITAA1caut = '§utegtc';   ?(già così)?
063000140121         ITAA1cmm  = WPDcmu;
063100140121         kpjbu     = TNTAA1ds;
063200140121         tntaa1c (kpjba);
063300140121         TNTAA1ds = kpjbu;
063400140121         if  oTAA1fabi = 'N';
063500140121           leavesr;
063600140121         endif;
063700140116
063800140116
063900140121         // -?Reperimento Distretto & Area del Commerciale Unificante?
064000140116         k_ORGfil = WPDfil;
064100140116         chain  %kds( keyAZORG01 )  AZORG;
064200140116         if  %found(AZORG01L)  and  ORGfva = *blank;
064300140116           WPDcar = ORGcar;
064400140121           WPDcdi = ORGfl3;
064500140116         endif;
064600140116
064700140121
064800140121         // -?PARZIALIZZAZIONI:?
064900140116         select;
065000140121           // -?Area del commerciale unificante diversa da quella selezionata?
065100140116           when  SS09car <> *zero  and  SS09car <> WPDcar;
065200140116             leavesr;
065300140121           // -?Area del commerciale unificante non gestibile dall'utente?
065400140116           when  %lookup( %editc( WPDcar : 'X' ) : sk_Car ) = *zero;
065500140116             leavesr;
065600140116         endsl;
065700140116
065800140116
065900140116         // -?Record elaborabile?
066000140116         $Ok = *on;
066100140116
066200140116       ENDSR;
066300140116
066400140116       //--------------------------------------------------------------
066500140116       //?Scrittura del singolo record nel WrkF WFPDL00F.              ?
066600140116       //--------------------------------------------------------------
066700140116       BEGSR  sr_WriteWFPDL;
066800140116
066900140121         // -?Impostazione dati ancora mancanti nel record?
067000140116         WPDeut   = DUTute;
067100140117         WPDedt   = %dec( wDate_Iso );
067200140117         WPDdti   = SS09dsi;
067300140117         WPDdtf   = SS09dsf;
067400140116
067500140121         //WPDcdi   = ......;       ?(già valorizzato)?
067600140121         //WPDcar   = ......;       ?(già valorizzato)?
067700140121         //WPDfil   = ......;       ?(già valorizzato)?
067800140121         //WPDcmu   = ......;       ?(già valorizzato)?
067900140121         //WPDksu   = ......;       ?(già valorizzato)?
068000140121         //WPDrsu   = ......;       ?(già valorizzato)?
068100140116
068200140116         WPDksc   = PDLksc;
068300140116         WPDrsm   = PDLrsm;
068400140121         //WPDtmd   = PDLtmd;
068500140116
068600140116         WPDn1p   = PDLn1p;
068700140116         WPDn2p   = PDLn2p;
068800140116         WPDn3p   = PDLn3p;
068900140116         WPDn4p   = PDLn4p;
069000140116         WPDn5p   = PDLn5p;
069100140116         WPDnop   = PDLnop;
069200140116         WPDpo5   = PDLpo5;
069300140116         WPDpdfSp = PDLpdfSp;
069400140116         WPDpdfNr = PDLpdfNr;
069500140116
069600140116         // -?Scrittura nuovo rec. nel WrkF WFPDL00F?
069700140116         write  WFPDL000;
069800140219
069900140219         $Almeno1rec = *on;
070000140116
070100140116       ENDSR;
070200140116
070300140116       //--------------------------------------------------------------
070400140116       //?Preparazione 2ª stringa SQL (per la stampa).                 ?
070500140116       //--------------------------------------------------------------
070600140116       BEGSR  sr_PrepSQL2;
070700140116
070800140121         wSQL2 = 'select * +
070900140121                    from QTEMP/WFPDL00F +
071000140121                   order by WPDcmu, WPDksu, WPDksc +
071100140121                     for fetch only';
071200140116
071300140116       ENDSR;
071400140116
071500140116       //--------------------------------------------------------------
071600140116       //?Apertura cursore 2.                                          ?
071700140116       //--------------------------------------------------------------
071800140116       BEGSR  sr_OpenCursor2;
071900140116
072000140116         // -?Dichiarazione cursore?
072100140116         exec sql   prepare S2   from :wSQL2;
072200140116         exec sql   declare C2   cursor for S2;
072300140117
072400140117         if  SQLcode < *zero;
072500140117           $EoF = *on;
072600140117           wErr = 1;
072700140117           exsr  sr_PrintERR;
072800140117         endif;
072900140116
073000140116         // -?Apertura del cursore?
073100140116         exec sql   open C2;
073200140116
073300140116         if  SQLcode < *zero;
073400140116           $EoF = *on;
073500140117           wErr = 1;
073600140117           exsr  sr_PrintERR;
073700140116         endif;
073800140116
073900140116       ENDSR;
074000140116
074100140116       //--------------------------------------------------------------
074200140116       //?Chiusura cursore 2.                                          ?
074300140116       //--------------------------------------------------------------
074400140116       BEGSR  sr_CloseCursor2;
074500151104
074600151104         // -?Chiusura del cursore?
074700151104         exec sql   close C2;
074800140122
074900140122         // -?Stampa testata per Totali Generali?
075000140122         write  SS09TX1;
075100140122         write  SS09TX4;
075200140116
075300151104         // -?Calcolo Medie Giornaliere?
075400151104         eval(H) MPDn1P   = TPDn1p   / xsrLav8ds.GiorniLav;
075500151104         eval(H) MPDn2p   = TPDn2p   / xsrLav8ds.GiorniLav;
075600151104         eval(H) MPDn3p   = TPDn3p   / xsrLav8ds.GiorniLav;
075700151104         eval(H) MPDn4p   = TPDn4p   / xsrLav8ds.GiorniLav;
075800151104         eval(H) MPDn5p   = TPDn5p   / xsrLav8ds.GiorniLav;
075900151104         eval(H) MPDnop   = TPDnop   / xsrLav8ds.GiorniLav;
076000151104         eval(H) MPDpo5   = TPDpo5   / xsrLav8ds.GiorniLav;
076100151104         eval(H) MPDnTotP = TPDnTotP / xsrLav8ds.GiorniLav;
076200151104         eval(H) MPDpdfSp = TPDpdfSp / xsrLav8ds.GiorniLav;
076300151104         eval(H) MPDpdfNr = TPDpdfNr / xsrLav8ds.GiorniLav;
076400151104
076500151104         // -?Stampa Totali Generali?
076600140116         write  SS09TOT;
076700140116
076800140116       ENDSR;
076900140116
077000140116       //--------------------------------------------------------------
077100140116       //?Lettura cursore 2.                                           ?
077200140116       //--------------------------------------------------------------
077300140116       BEGSR  sr_ReadCursor2;
077400140116
077500140116         clear  WFPDLds;
077600140116
077700140117         exec sql   fetch next   from C2   into :WFPDLds;
077800140116
077900140116         select;
078000140116           when  SQLcode = 100;
078100140116             $EoF = *on;
078200140116           when  SQLcode < *zero;
078300140116             $EoF = *on;
078400140117             wErr = 1;
078500140117             exsr  sr_PrintERR;
078600140116           other;
078700151022             exsr  sr_IncremTot;
078800151022             if  SS09outS = 'D';
078900151022               exsr  sr_PrintWFPDL;
079000151022             endif;
079100140116         endsl;
079200140116
079300140116       ENDSR;
079400151022
079500151022       //--------------------------------------------------------------
079600151022       //?Incremento Totali per Totale Generale in stampa.             ?
079700151022       //--------------------------------------------------------------
079800151022       BEGSR  sr_IncremTot;
079900151022
080000151022         TPDn1p   += WPDn1p;
080100151022         TPDn2p   += WPDn2p;
080200151022         TPDn3p   += WPDn3p;
080300151022         TPDn4p   += WPDn4p;
080400151022         TPDn5p   += WPDn5p;
080500151022         TPDnop   += WPDnop;
080600151022         TPDpo5   += WPDpo5;
080700151022         TPDnTotP += WPDn1p + ( WPDn2p * 2 ) + ( WPDn3p * 3 ) +
080800151022                     ( WPDn4p * 4 ) + ( WPDn5p * 5 ) + WPDpo5;
080900151022         TPDpdfSp += WPDpdfSp;
081000151022         TPDpdfNr += WPDpdfNr;
081100151022
081200151022       ENDSR;
081300140116
081400140116       //--------------------------------------------------------------
081500140116       //?Stampa singolo cliente dal file WFPDL00F.                    ?
081600140116       //--------------------------------------------------------------
081700140116       BEGSR  sr_PrintWFPDL;
081800140116
081900140121         $CmmU = (WPDcmu <> pCMUcod);
082000140117
082100140121         // -?Rottura per Commerciale Unificante?
082200140121         IF  $CmmU;
082300140121
082400140121           *in25 = *on;
082500140121           exsr  sr_DecodCMU;
082600140121
082700140122           // -?Stampa testata?
082800140122           write  SS09TX1;
082900140121           write  SS09TX3;
083000140121           write  SS09TX4;
083100140121           *in25 = *off;
083200140121
083300140121         ENDIF;
083400140117
083500140117
083600140116         // -?Rottura per Cliente Unificante?
083700140116         If  WPDksu <> UPDksu  or  WPDrsu <> UPDrsu;
083800140116           exsr  sr_PrintCliUni;
083900140116         EndIf;
084000140116
084100140116
084200140116         // -?Impostazione dati in stampa?
084300140116         //WPDksc   = WPDksc;
084400140116         //WPDrsm   = WPDrsm;
084500140116         //WPDtmd   = WPDtmd;
084600140116         CPDn1p   = WPDn1p;
084700140116         CPDn2p   = WPDn2p;
084800140116         CPDn3p   = WPDn3p;
084900140116         CPDn4p   = WPDn4p;
085000140116         CPDn5p   = WPDn5p;
085100140116         CPDnop   = WPDnop;
085200140116         CPDpo5   = WPDpo5;
085300151022         CPDnTotP = WPDn1p + ( WPDn2p * 2 ) + ( WPDn3p * 3 ) +
085400151022                    ( WPDn4p * 4 ) + ( WPDn5p * 5 ) + WPDpo5;
085500140116         CPDpdfSp = WPDpdfSp;
085600140116         CPDpdfNr = WPDpdfNr;
085700140116
085800140116
085900140120         // -?Stampa del singolo cliente / tipo modulo?
086000140117         if  *in25;
086100140117           write  SS09TX1;
086200140117           write  SS09TX4;
086300140117           *in25 = *off;
086400140117         endif;
086500140116         write  SS09ksc;
086600140116
086700140116       ENDSR;
086800140116
086900140116       //--------------------------------------------------------------
087000140116       //?Decodifica Commerciale Unificante del commerciale.           ?
087100140116       //--------------------------------------------------------------
087200140116       BEGSR  sr_DecodCMU;
087300140116
087400140117         k_CMMcod = WPDcmu;
087500140117         chain  %kds( keyAZCMM01 )  AZCMM000;
087600140117
087700140117         if  %found(AZCMM01L);
087800140117           WPDcmu = CMMuni;
087900140117         else;
088000140117           clear  CMMdes;
088100140117         endif;
088200140117
088300140117         pCMUcod = WPDcmu;
088400140117         pCMUdes = CMMdes;
088500140116
088600140116       ENDSR;
088700140116
088800140116       //--------------------------------------------------------------
088900140116       //?Calcolo totali e Stampa x Cliente Unificante.                ?
089000140116       //--------------------------------------------------------------
089100140116       BEGSR  sr_PrintCliUni;
089200140116
089300140116         UPDksu    = WPDksu;
089400140116         UPDrsu    = WPDrsu;
089500140116
089600140116
089700140116         wSQL3 = 'select sum(WPDn1p), sum(WPDn2p), sum(WPDn3p), +
089800140116                         sum(WPDn4p), sum(WPDn5p), sum(WPDnop), +
089900140116                         sum(WPDpo5), sum(WPDpdfSp), sum(WPDpdfNr) +
090000140117                    from QTEMP/WFPDL00F +
090100140116                   where WPDksu = ' + %editc( WPDksu : 'X' ) + ' +
090200140116                   group by WPDksu +
090300140116                     for fetch only';
090400140116
090500140116         // -?Dichiarazione cursore?
090600140116         exec sql   prepare S3   from :wSQL3;
090700140116         exec sql   declare C3   cursor for S3;
090800140116
090900140116         // -?Apertura del cursore?
091000140116         exec sql   open C3;
091100140116
091200140116         if  SQLcode < *zero;
091300140116           $EoF = *on;
091400140117           wErr = 1;
091500140117           exsr  sr_PrintERR;
091600140116         endif;
091700140116
091800140116         // -?Lettura del cursore?
091900140116         clear  WTPDLds;
092000140116         exec sql   fetch next   from C3   into :WTPDLds;
092100140116
092200140116         if  SQLcode < *zero;
092300140116           $EoF = *on;
092400140117           wErr = 1;
092500140117           exsr  sr_PrintERR;
092600140116         endif;
092700140116
092800140116         // -?Chiusura del cursore?
092900140116         exec sql   close C3;
093000140116
093100151022
093200151022         // -?Calcolo Totale Pagine per Packing-List?
093300151022         UPDnTotP = UPDn1p + ( UPDn2p * 2 ) + ( UPDn3p * 3 ) +
093400151022                    ( UPDn4p * 4 ) + ( UPDn5p * 5 ) + UPDpo5;
093500140116
093600140116         // -?Stampa totali per cliente unificante?
093700140117         if  *in25;
093800140117           write  SS09TX1;
093900140117           write  SS09TX4;
094000140117           *in25 = *off;
094100140117         endif;
094200140116         write  SS09ksu;
094300140116
094400140116       ENDSR;
094500140115
094600140115       //--------------------------------------------------------------
094700140115       //?Stampa segnalazione dell'errore rilevato via SQL             ?
094800140115       //--------------------------------------------------------------
094900140117       BEGSR  sr_PrintERR;
095000140115
095100140115         // -?Stampa del Dump?
095200140115         Dump(A);
095300140115
095400140115         // -?Stampa del Job-Log?
095500140115         Qcmd = 'DSPJOBLOG job(*) output(*print)';
095600140117         exsr  sr_ExecCmd;
095700140115
095800140115         // -?Apertura del printer-file?
095900140115         if  Not %open(QSYSPRT);
096000140117           Qcmd = c_OvrPrtF;
096100140117           exsr  sr_ExecCmd;
096200140115           open  QSYSPRT;
096300140117           except  PRTtxt;
096400140115         endif;
096500140117
096600140117         SELECT;
096700140117
096800140117           // -?Stampa dell'errore in COPIA iniziale di WFPDL00F?
096900140117           //  ?da GAITRAAZM in QTEMP (e chiusura del *pgm)?
097000140117           //WHEN  SQlcode >= *zero  and  Not %open(WFPDL00F);
097100140117           WHEN  wErr = 3;
097200140117             if  *inOF;
097300140117               except  PRTtxt;
097400140117               *inOF = *off;
097500140117             endif;
097600140117             except  PRTerr3;
097700140117             *inOF = *off;
097800140117             // -?Chiusura del programma (DOPO)?
097900140117             exsr  sr_RoutEnd;
098000140117
098100140117           // -?Stampa dell'errore SQL (e chiusura del *pgm)?
098200140117           //WHEN  SQlcode <  *zero;
098300140117           WHEN  wErr = 1;
098400140117             clear  ds_Temp;
098500140117             ds_Temp = wSQL1;
098600140117             if  *inOF;
098700140117               except  PRTtxt;
098800140117               *inOF = *off;
098900140117             endif;
099000140117             except  PRTerr1;
099100140117             *inOF = *off;
099200140117             For  xx = 1  To  %elem($Temp);
099300140117               if  $Temp(xx) = *blank;
099400140117                 leave;
099500140117               endif;
099600140117               if  *inOF;
099700140117                 except  PRTtxt;
099800140117                 *inOF = *off;
099900140117               endif;
100000140117               except  PRTerr2;
100100140117             EndFor;
100200140117             // -?Chiusura del programma (DOPO)?
100300140117             exsr  sr_RoutEnd;
100400140117
100500140117           // -?Stampa dell'errore in COPIA finale di WFPDL00F?
100600140117           //  ?da QTEMP in GAITRAAZM (e proseguimento)?
100700140117           //WHEN  SQlcode >= *zero  and  %open(WFPDL00F);
100800140117           WHEN  wErr = 4;
100900140117             if  *inOF;
101000140117               except  PRTtxt;
101100140117               *inOF = *off;
101200140117             endif;
101300140117             except  PRTerr4;
101400140117             *inOF = *off;
101500140117             leavesr;
101600140117
101700140117         ENDSL;
101800140115
101900140115       ENDSR;
102000140115
102100140115       //--------------------------------------------------------------
102200140115       //?Operazioni finali.                                           ?
102300140115       //--------------------------------------------------------------
102400140115       BEGSR  sr_RoutEnd;
102500140115
102600140115         // -?Chiusura applicazioni?
102700140122         cloTabella();
102800140122
102900140116         reset  TIBS69ds;
103000140116         tibs69r( tibs69ds :
103100140116                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
103200140115
103300140115         // -?Chiusura printer-file?
103400140115         if  %open(QSYSPRT);
103500140115           close  QSYSPRT;
103600140117           Qcmd = c_DltOvr;
103700140117           exsr  sr_ExecCmd;
103800140115         endif;
103900140115
104000140115         // -?Uscita dal *pgm?
104100140115         return;
104200140115
104300140115       ENDSR;
104400140117
104500140117       //--------------------------------------------------------------
104600140117       //?Esecuzione del comando (già impostato).                      ?
104700140117       //--------------------------------------------------------------
104800140117       BEGSR  sr_ExecCmd;
104900140117
105000140117         clear Qcap0100;
105100140117         Qcabcsdh = *off;
105200140117         Qcapa    = *off;
105300140117         Qcacmdss = *off;
105400140117         Qcaerved = *allX'00';
105500140117
105600140117         clear Qusec;
105700140117         Qusbprv  = %size(Qusec);
105800140117
105900140117         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
106000140117                           %size(Qcap0100) : 'CPOP0100' : *omit :
106100140117                           0 : 0 : Qusec);
106200140117
106300140117         //if  Qusei <> *blank;
106400140117         //  ...;
106500140117         //endif;
106600140117
106700140117       ENDSR;
106800140115
106900140115      /end-free
107000140116
107100140116       //--------------------------------------------------------------
107200140116       //?S P O O L - F I L E S                                        ?
107300140116       //--------------------------------------------------------------
107400140116
107500140116     oQSYSPRT   e            PRTtxt            1
107600140116     o                       RSUT
107700140116     o                                        +   6 'LISTA ERRORI RILE-
107800140116     o                                              VATI IN FASE DI ST-
107900140116     o                                              AMPA N° PAGINE DI -
108000140116     o                                              PACKING-LIST IN LD-
108100140116     o                                              V'
108200140116     o                       SDSpgmName       +   6
108300140116     o                       wDate         y  +   3
108400140117     o          e            PRTtxt      0
108500140117     o                                          +26 'LISTA ERRORI RILE-
108600140117     o                                              VATI IN FASE DI ST-
108700140117     o                                              AMPA N° PAGINE DI -
108800140117     o                                              PACKING-LIST IN LD-
108900140117     o                                              V'
109000140116     o          e            PRTtxt      1
109100140116     o                       KNSIF
109200140116     o                       KNMUS            +   1
109300140117     o                                        +   5 '------------------
109400140116     o                                              -------------------
109500140116     o                                              -------------------
109600140116     o                                              -------------------
109700140117     o                                              -'
109800140116     o                                        +   6 'Pag.'
109900140116     o                       Page          z  +   0
110000140116     o                       wTime            +   5 '  :  :  '
110100140117      *
110200140116     o          e            PRTerr1     2
110300140116     o                                              'RILEVATO ERRORE: +
110400140116     o                                               SQLCODE'
110500140116     o                       SQLcode       k  +   1
110600140116     o                                        +   1 'SQLSTATE'
110700140116     o                       SQLstate         +   1
110800140116     o                                        +   1 'DURANTE L''ESECUZ-
110900140116     o                                              IONE DEL SEGUENTE -
111000140116     o                                              COMANDO SQL:'
111100140116     o          e            PRTerr1     0  1
111200140116     o                                              'RILEVATO ERRORE: +
111300140116     o                                               SQLCODE'
111400140116     o                       SQLcode       k  +   1
111500140116     o                                        +   1 'SQLSTATE'
111600140116     o                       SQLstate         +   1
111700140116     o          e            PRTerr2     1
111800140116     o                       $Temp(xx)        +   3
111900140117      *
112000140117     o          e            PRTerr3     2
112100140117     o                                              'NON RIUSCITA LA +
112200140117     o                                               COPIA DEL WORK-+
112300140117     o                                               FILE WFPDL00F +
112400140117     o                                               NELLA LIBR. QTEMP'
112500140117      *
112600140117     o          e            PRTerr4     2
112700140117     o                                              'NON RIUSCITA LA +
112800140117     o                                               COPIA DEL WORK-+
112900140117     o                                               FILE WFPDL00F +
113000140117     o                                               NELLA LIBR.'
113100140117     o                       wLibrAZx         +   1
113200140116
113300140116       //--------------------------------------------------------------
113400140116       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
113500140116       //--------------------------------------------------------------
113600140116
113700140117** -?$iSystem / $Libraries:?Sistema AS/400 & Libreria.?
113800140116SETRAS  GAITRAAZM
113900140116AS888   GAITRAAZP
