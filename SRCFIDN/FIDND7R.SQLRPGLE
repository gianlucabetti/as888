000100140417       //==============================================================
000200140417       //?Statistica RAC per motivi apertura "  8" e " 88"             ?
000300140417       //?DDT/PACKING LIST NON CONS. - NO PDA ("  8") o PDA (" 88")    ?
000400140417       //==============================================================
000500140417
000600140417       //--------------------------------------------------------------
000700140417       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140417       //--------------------------------------------------------------
000900140417
001000140417     /*PRM  dbgview(*source)
001100140417     /*END
001200140417
001300140417       //--------------------------------------------------------------
001400140417       //?Specifiche di controllo.                                     ?
001500140417       //--------------------------------------------------------------
001600140417
001700140417     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800140417     h dftactgrp(*no)
001900140417     h bnddir('TRUL')
002000140417
002100140417       //--------------------------------------------------------------
002200140417       //?Dichiarazione file.                                          ?
002300140417       //--------------------------------------------------------------
002400140417
002500140417       // -?Organigramma?
002600140417     fAZORG01L  if   e           k disk
002700140417
002800140417       // -?Anagrafica Autotrasportatori/Cooperative?
002900140417     fFIAPD01L  if   e           k disk
003000140417
003100140417       // -?Bolle Sede?
003200140417     fTITAS30C  if   e           k disk
003300140417       // -?Estensione testata bolle?
003400140417     fFIAR531C  if   e           k disk
003500140417       // -?Legami bolle?
003600140417     fFNLBL01L  if   e           k disk
003700140417
003800140417       // -?WrkF statistica R.A. motivi apertura "  8"/" 88"
003900140423     f*//WFRAP11L  if a e           k disk    usropn
004000140423     fWFRAP10F  o    e             disk    usropn
004100140417
004200140417       //--------------------------------------------------------------
004300140417       //?Definizione costanti.                                        ?
004400140417       //--------------------------------------------------------------
004500140417
004600140417
004700140417       //--------------------------------------------------------------
004800140417       //?Definizione schiere.                                         ?
004900140417       //--------------------------------------------------------------
005000140417
005100140417
005200140417       //--------------------------------------------------------------
005300140417       //?Definizione aree dati.                                       ?
005400140417       //--------------------------------------------------------------
005500140417
005600140417       // -?Dati utente?
005700140417     d §AzUte        e ds                  extname(AZUTE00F)
005800140417     d                                     dtaara
005900140417     d §DatiUte      e ds                  extname(dDatiUte)
006000140417     d                                     dtaara
006100140417
006200140417       //--------------------------------------------------------------
006300140417       //?Definizione strutture dati.                                  ?
006400140417       //--------------------------------------------------------------
006500140417
006600140417       // -?DS per Numero Spedizione?
006700140417     d ds_Spedizione   ds                  inz   qualified
006800140417     d   wLNP                         3s 0 inz
006900140417     d   wNRS                         2s 0 inz
007000140417     d   wNSP                         7s 0 inz
007100140417     d   wAAS                         4s 0 inz
007200140417
007300140417       // -?Parametri ricevuti?
007400140417     d KPJBA         e ds
007500140417     d FIDND7ds      e ds                  inz   qualified
007600140417
007700140417       // -?Dati estratti via SQL?
007800140417     d FITGDds       e ds                  extname(FITGD00F)
007900140417     d                                     inz   qualified
008000140417
008100140417       // -?DS Estensione FIAR5 tipo record "GEN"?
008200140417     d dAR5gen       e ds                  inz   qualified
008300140417
008400140417       // -?Tab. "05"  = Aree?
008500140417     d ds05          e ds
008600140417       // -?Tab. "17"  = Distretti?
008700140417     d ds17          e ds
008800140417
008900140417       // -?Stringa SQL da eseguire?
009000140417     d wSQL            s           1024    inz   varying
009100140417
009200140417       //--------------------------------------------------------------
009300140417       //?Definizione variabili globali.                               ?
009400140417       //--------------------------------------------------------------
009500140417
009600140417       // -?Flags booleani?
009700140417     d $EoF            s               n   inz
009800140417
009900140417       // -?Campi di comodo?
010000140417     d wDate           s              8  0 inz
010100140417
010200140417       //--------------------------------------------------------------
010300140417       //?Definizione prototipi procedure.                             ?
010400140417       //--------------------------------------------------------------
010500140417
010600140417       // -?Reperimento dati utente?
010700140417     d TIBS34ds      e ds                  inz
010800140417      /copy gaitrasrc/srcProtoPR,TIBS34R
010900140417
011000140417       // -?Ricerca unificante padre?
011100140417      /copy gaitrasrc/srcProtoPI,TIBS10R
011200140417      /copy gaitrasrc/srcProtoPR,TIBS10R
011300140417
011400140417       // -?Controllo/Decodifica cliente?
011500140417      /copy gaitrasrc/srcProtoPI,TIBS69R
011600140417      /copy gaitrasrc/srcProtoPR,TIBS69R
011700140417
011800140417       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
011900140417      /copy gaitrasrc/srcProtoPI,TRULTAB
012000140417      /copy gaitrasrc/srcProtoPR,TRULTAB
012100140417
012200140417       // -?Parametri API QCAPCMD (Process Commands)?
012300140417     d Qcmd            s           2048    inz  varying
012400140417      /copy qSysInc/qRpgleSrc,QCAPCMD
012500140417       // -?API QCAPCMD (Process Commands)?
012600140417      /copy gaitrasrc/srcProtoPR,QCAPCMD
012700140417
012800140417       // -?Parametri gestione errori API.?
012900140417      /copy qsysinc/qrpglesrc,QUSEC
013000140417
013100140417       //--------------------------------------------------------------
013200140417       //?Definizione key-list.                                        ?
013300140417       //--------------------------------------------------------------
013400140417
013500140417       // -?File AZORG01L?
013600140417     d keyAZORG01    e ds                  extname(AZORG01L : *key)
013700140417     d                                     prefix(k_)   inz
013800140417
013900140417       // -?File FIAPD01L?
014000140417     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
014100140417     d                                     prefix(k_)   inz
014200140417
014300140417       // -?File TITAS30C?
014400140417     d keyTITAS30    e ds                  extname(TITAS30C : *key)
014500140417     d                                     prefix(k_)   inz
014600140417       // -?File FIAR531C?
014700140417     d keyFIAR531    e ds                  extname(FIAR531C : *key)
014800140417     d                                     prefix(k_)   inz
014900140417       // -?File FNLBL01L?
015000140417     d keyFNLBL01    e ds                  extname(FNLBL01L : *key)
015100140417     d                                     prefix(k_)   inz
015200140417
015300140417       // -?WrkF WFRAP11L?
015400140423     d*// keyWFRAP11    e ds                  extname(WFRAP11L : *key)
015500140423     d*//                                     prefix(k_)   inz
015600140417
015700140417       //--------------------------------------------------------------
015800140417       //?Riepilogo indicatori utilizzati.                             ?
015900140417       //--------------------------------------------------------------
016000140417
016100140417
016200140417       //--------------------------------------------------------------
016300140417       //?M A I N - L I N E                                            ?
016400140417       //--------------------------------------------------------------
016500140417
016600140417     c     *Entry        plist
016700140417     c                   parm                    KPJBA
016800140417
016900140417      /free
017000140417
017100140417       // -?Operazioni iniziali?
017200140417       exsr  sr_RoutInz;
017300140417
017400140417       // -?Ciclo di elaborazione per creare WFRAP10F?
017500140417       reset  $EoF;
017600140417       exsr  sr_OpenCursor;
017700140417
017800140417       DoW  $EoF = *off;
017900140417         exsr  sr_ReadCursor;
018000140417       EndDo;
018100140417
018200140417       exsr  sr_CloseCursor;
018300140417
018400140417       // -?Operazioni finali?
018500140417       exsr sr_RoutEnd;
018600140417
018700140417       //--------------------------------------------------------------
018800140417       //?Operazioni iniziali.                                         ?
018900140417       //--------------------------------------------------------------
019000140417       BEGSR  sr_RoutInz;
019100140417
019200140417         // -?Impostazione opzioni per SQL?
019300140417         exec sql   set  option  DynUsrPrf = *Owner,
019400140417                                 CloSqlCsr = *EndMod;
019500140417
019600140417         // -?Impostazione chiusura?
019700140417         *inLR = *on;
019800140417
019900140417         // -?Reperimento dati job?
020000140417         exsr  sr_DatiJob;
020100140417
020200140417         // -?Reperimento data odierna in formato aaaammgg?
020300140417         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020400140417                               : 1 : 8 ) );
020500140417
020600140417         // -?Reperimento parametri?
020700140417         FIDND7ds = kpjbu;
020800140417
020900140417         // -?Pulizia del WrkF GAITRAAZM/WFRAP10F?
021000140417         Qcmd = 'CLRPFM file(';
021100140417         if  %subst( KNSIF : 7 : 1 ) = 'P'; //?(AS888)?
021200140417           Qcmd += 'GAITRAAZP';
021300140417         else;                              //?(SETRAS)?
021400140417           Qcmd += 'GAITRAAZM';
021500140417         endif;
021600140417         Qcmd += '/WFRAP10F)';
021700140417         exsr  sr_ExecCmd;
021800140417         if  Qusei <> *blank;
021900140417           $EoF = *on;
022000140417           exsr  sr_PrintERR;
022100140417           exsr  sr_RoutEnd;
022200140417         endif;
022300140417
022400140417         // -?Apertura del WrkF WFRAP10F?
022500140423         //open  WFRAP11L;
022600140423         open  WFRAP10F;
022700140417
022800140417         // -?Preparazione stringa SQL?
022900140417         exsr  sr_PrepSQL;
023000140417
023100140417       ENDSR;
023200140417
023300140417       //--------------------------------------------------------------
023400140417       //?Reperimento Dati del job (Utente/Operativi).                 ?
023500140417       //--------------------------------------------------------------
023600140417       BEGSR  sr_DatiJob;
023700140417
023800140417         in(E) §AzUte;
023900140417         if NOT %error;
024000140417           in(E) §DatiUte;
024100140417         endif;
024200140417         if %error or RSut = *blanks;
024300140417           clear TIBS34ds;
024400140417           tibs34r ( tibs34ds );
024500140417           in §AzUte;
024600140417           in §DatiUte;
024700140417         endif;
024800140417
024900140417       ENDSR;
025000140417
025100140417       //--------------------------------------------------------------
025200140417       //?Preparazione stringa SQL per l'estrazione da FITGD00F.       ?
025300140417       //--------------------------------------------------------------
025400140417       BEGSR  sr_PrepSQL;
025500140417
025600140417         wSQL = 'select * +
025700140417                   from FITGD00F +
025800140417                  where TGDdac between ' +
025900140417                        %editc( FIDND7ds.iD7dtI : '3' ) + ' and ' +
026000140417                        %editc( FIDND7ds.iD7dtF : '3' ) +
026100140417                  ' and TGDtor = ''S'' +
026200140417                    and TGDmad in (''  8'', '' 88'') +
026300140417                  order by TGDdac, TGDtor, TGDogg, TGDmad +
026400140417                    for fetch only';
026500140417
026600140417       ENDSR;
026700140417
026800140417       //--------------------------------------------------------------
026900140417       //?Apertura cursore.                                            ?
027000140417       //--------------------------------------------------------------
027100140417       BEGSR  sr_OpenCursor;
027200140417
027300140417         // -?Dichiarazione cursore?
027400140417         exec sql   prepare S1   from :wSQL;
027500140417         exec sql   declare C1   cursor for S1;
027600140417
027700140417         if  SQLcode < *zero;
027800140417           $EoF = *on;
027900140417           exsr  sr_PrintERR;
028000140417         endif;
028100140417
028200140417         // -?Apertura del cursore?
028300140417         exec sql   open C1;
028400140417
028500140417         if  SQLcode < *zero;
028600140417           $EoF = *on;
028700140417           exsr  sr_PrintERR;
028800140417         endif;
028900140417
029000140417       ENDSR;
029100140417
029200140417       //--------------------------------------------------------------
029300140417       //?Chiusura cursore.                                            ?
029400140417       //--------------------------------------------------------------
029500140417       BEGSR  sr_CloseCursor;
029600140417
029700140417         // -?Chiusura del cursore?
029800140417         exec sql   close C1;
029900140417
030000140417         // -?Chiusura del WrkF?
030100140423         //close  WFRAP11L;
030200140423         close  WFRAP10F;
030300140417
030400140417       ENDSR;
030500140417
030600140417       //--------------------------------------------------------------
030700140417       //?Lettura cursore.                                             ?
030800140417       //--------------------------------------------------------------
030900140417       BEGSR  sr_ReadCursor;
031000140417
031100140417         clear  FITGDds;
031200140417
031300140417         exec sql   fetch next   from C1   into :FITGDds;
031400140417
031500140417         select;
031600140417           when  SQLcode = 100;
031700140417             $EoF = *on;
031800140417           when  SQLcode < *zero;
031900140417             $EoF = *on;
032000140417             exsr  sr_PrintERR;
032100140417           other;
032200140417             exsr  sr_WriteWFRAP;
032300140417         endsl;
032400140417
032500140417       ENDSR;
032600140417
032700140417       //--------------------------------------------------------------
032800140417       //?Scrittura record nel WrkF WFRAP10F.                          ?
032900140417       //--------------------------------------------------------------
033000140417       BEGSR  sr_WriteWFRAP;
033100140417
033200140417         ds_Spedizione = FITGDds.TGDogg;
033300140417
033400140417         // -?Pulizia campi nel record?
033500140417         clear  WFRAP000;
033600140417
033700140417         // -?Dati della spedizione (...avesse mai un "mamma"!)?
033800140417         clear  keyTITAS30;
033900140417         k_TASaas = ds_Spedizione.wAAS;
034000140417         k_TASlnp = ds_Spedizione.wLNP;
034100140417         k_TASnrs = ds_Spedizione.wNRS;
034200140417         k_TASnsp = ds_Spedizione.wNSP;
034300140417         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
034400140417         if  %found(TITAS30C);
034500140417           RAPlna = TASlna;
034600140417           RAPpdr = TASpdr;
034700140417           RAPrsd = TASrsd;
034800140417         endif;
034900140417
035000140417         // -?Verifica SE c'è Mamma?
035100140417         clear  keyFNLBL01;
035200140417         k_LBLaan = ds_Spedizione.wAAS;
035300140417         k_LBLlpn = ds_Spedizione.wLNP;
035400140417         k_LBLnrn = ds_Spedizione.wNRS;
035500140417         k_LBLnsn = ds_Spedizione.wNSP;
035600140417         chain  %kds( keyFNLBL01 )  FNLBL000;
035700140417
035800140417         If  %found(FNLBL01L);
035900140417
036000140417           ds_Spedizione.wAAS = LBLaao;
036100140417           ds_Spedizione.wLNP = LBLlpo;
036200140417           ds_Spedizione.wNRS = LBLnro;
036300140417           ds_Spedizione.wNSP = LBLnso;
036400140417
036500140417           clear  keyTITAS30;
036600140417           k_TASaas = ds_Spedizione.wAAS;
036700140417           k_TASlnp = ds_Spedizione.wLNP;
036800140417           k_TASnrs = ds_Spedizione.wNRS;
036900140417           k_TASnsp = ds_Spedizione.wNSP;
037000140417           chain  %kds( keyTITAS30 : 4 )  TITAS30C;
037100140417
037200140417         EndIf;
037300140417
037400140417         // -?Spedizione NON trovata su TITAS30C?
037500140417         If  NOT %found(TITAS30C);
037600140417           leavesr;
037700140417         EndIf;
037800140417
037900140423         //// ·?Verifica SE già inserita questa spedizione nel WrkF?
038000140423         //clear  keyWFRAP11;
038100140423         //k_RAPaas = ds_Spedizione.wAAS;
038200140423         //k_RAPlnp = ds_Spedizione.wLNP;
038300140423         //k_RAPnrs = ds_Spedizione.wNRS;
038400140423         //k_RAPnsp = ds_Spedizione.wNSP;
038500140423         //setll  %kds( keyWFRAP11 )  WFRAP000;
038600140423         //if  %equal(WFRAP11L);
038700140423         //  leavesr;
038800140423         //endif;
038900140417
039000140417         // -?Impostazione dati nel record:?
039100140417
039200140417         // ·?Parametri di lancio?
039300140417         RAPute   = DUTute;
039400140417         RAPdta   = wDate;
039500140417         RAPddr   = FIDND7ds.iD7dti;
039600140417         RAPadr   = FIDND7ds.iD7dtf;
039700140417
039800140417         // ·?Numero Spedizione?
039900140417         RAPaas = ds_Spedizione.wAAS;
040000140417         RAPlnp = ds_Spedizione.wLNP;
040100140417         RAPnrs = ds_Spedizione.wNRS;
040200140417         RAPnsp = ds_Spedizione.wNSP;
040300140417
040400140417         // ·?Dati Reclamo?
040500140417         RAPant = FITGDds.TGDant;
040600140417         RAPnut = FITGDds.TGDnut;
040700140417         RAPpdt = FITGDds.TGDpdt;
040800140417         RAPdac = FITGDds.TGDdac;
040900140417         RAPcdt = FITGDds.TGDcdt;
041000140417
041100140417         // ·?Cod. Cliente Mittente?
041200140417         if  TASccm > *zero;
041300140417           RAPccm = TASccm;
041400140417         else;
041500140417           RAPccm = TASksc;
041600140417         endif;
041700140417
041800140417         clear  dAR5gen;
041900140417         // ·?DDT?
042000140417         If  TASll1 = 'S';
042100140417           RAPflg = 'S';
042200140417         // ·?Packing-List?
042300140417         Else;
042400140417           clear  keyFIAR531;
042500140417           k_AR5aas = TASaas;
042600140417           k_AR5lnp = TASlnp;
042700140417           k_AR5nrs = TASnrs;
042800140417           k_AR5nsp = TASnsp;
042900140417           k_AR5trd = 'GEN';
043000140417           chain  %kds( keyFIAR531 : 5 )  FIAR531C;
043100140417           if  %found(FIAR531C);
043200140417             dAR5gen = AR5uni;
043300140417             if  dAR5gen.§AR5als = 'S'  or  dAR5gen.§AR5alx = 'S';
043400140417               RAPflg = 'P';
043500140417             endif;
043600140417           endif;
043700140417         EndIf;
043800140417
043900140417         // ·?Data Consegna merce?
044000140423         RAPdcm  = TASdcm;
044100140423
044200140423         // ·?Filiale Responsabile?
044300140423         RAPfilR = FITGDds.TGDpor;
044400140423
044500140423         // ·?Motivo Apertura R.A.?
044600140423         RAPmad  = FITGDds.TGDmad;
044700140417
044800140417         // ·?Decodifiche?
044900140417         exsr  sr_Decod;
045000140417
045100140417         // -?Scrittura record?
045200140417         //______________
045300140417         write  WFRAP000;
045400140417         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
045500140417
045600140423         //feod  WFRAP11L;
045700140417
045800140417       ENDSR;
045900140417
046000140417       //--------------------------------------------------------------
046100140417       //?Decodifica dei dati.                                         ?
046200140417       //--------------------------------------------------------------
046300140417       BEGSR  sr_Decod;
046400140417
046500140417         // -?Decodifica cliente (CCM)?
046600140417         clear  TIBS69ds;
046700140417         I69sif = knsif;
046800140417         I69kcc = DUTkci;
046900140417         I69kac = RAPccm;
047000140417         tibs69r( TIBS69ds :
047100140417                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
047200140417         if  O69err = *blank;
047300140417           RAPccmD  = ACOrag;
047400140417         endif;
047500140417
047600140417         // -?Reperimento codice cliente Unificante?
047700140417         clear  TIBS10ds;
047800140417         TIBS10ds.D10drf = wDate;
047900140417         TIBS10ds.D10tle = 'ST';
048000140417         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
048100140417         TIBS10ds.D10cod = RAPccm;
048200140417         //tibs10r ( TIBS10ds );
048300140417         GetKSCfamily ( TIBS10ds : 'R' );
048400140417         if  TIBS10ds.D10err = *blank  and
048500140417             TIBS10ds.D10cop > *zero;
048600140417           RAPuni = TIBS10ds.D10cop;
048700140417         else;
048800140417           RAPuni = TIBS10ds.D10cod;
048900140417         endif;
049000140417
049100140417         // -?Decodifica cliente Unificante?
049200140417         If  RAPccm <> RAPuni;
049300140417           clear  TIBS69ds;
049400140417           I69sif = knsif;
049500140417           I69kcc = DUTkci;
049600140417           I69kac = RAPuni;
049700140417           tibs69r( TIBS69ds :
049800140417                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
049900140417           if  O69err = *blank;
050000140417             RAPuniD  = ACOrag;
050100140417           endif;
050200140417         Else;
050300140417           RAPuniD = RAPccmD;
050400140417         EndIf;
050500140417
050600140417         // -?Decodifica Linea di Arrivo?
050700140417         //  ?e reperimento Distretto e Area?
050800140417         clear  keyAZORG01;
050900140417         k_ORGfil = RAPlna;
051000140417         chain  %kds( keyAZORG01 )  AZORG;
051100140417         if  %found(AZORG01L)  and  ORGfva = *blank;
051200140417           RAPlnad = ORGdes;
051300140417           RAPdisA = ORGfl3;
051400140417           RAPareA = ORGcar;
051500140417         endif;
051600140417
051700140417         // -?Decodifica Distretto della LNA?
051800140417         clear  ds17;
051900140417         ds_TNTBE.TBEke1 = RAPdisA;
052000140417         if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
052100140417                          *omit : *omit : *omit :
052200140417                          *omit : *omit :
052300140417                          *omit : *omit : *omit : *omit :
052400140417                          *omit : *omit : *omit : *omit :
052500140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
052600140417                        = *zero      AND
052700140417             ds_TNTBE.TBEatb = *blank;
052800140417           ds17   = ds_TNTBE.TBEuni;
052900140417         endif;
053000140417         RAPdisAd = §17des;
053100140417
053200140417         // -?Decodifica Area della LNA?
053300140417         clear  ds05;
053400140417         ds_TNTBE.TBEke1 = %editc( RAPareA : 'X' );
053500140417         if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
053600140417                          *omit : *omit : *omit :
053700140417                          *omit : *omit :
053800140417                          *omit : *omit : *omit : *omit :
053900140417                          *omit : *omit : *omit : *omit :
054000140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
054100140417                        = *zero      AND
054200140417             ds_TNTBE.TBEatb = *blank;
054300140417           ds05   = ds_TNTBE.TBEuni;
054400140417         endif;
054500140417         RAPareAd = §05des;
054600140417
054700140417         // -?Decodifica Filiale Unificante?
054800140417         //  ?e reperimento Distretto e Area?
054900140417         RAPfilU = RAPuni / 10000;
055000140417         k_ORGfil = RAPfilU;
055100140417         chain  %kds( keyAZORG01 )  AZORG;
055200140417         if  %found(AZORG01L)  and  ORGfva = *blank;
055300140417           RAPfilUd = ORGdes;
055400140417           RAPdisU  = ORGfl3;
055500140417           RAPareU  = ORGcar;
055600140417         endif;
055700140417
055800140417         // -?Decodifica Distretto della Fil. Unificante?
055900140417         clear  ds17;
056000140417         ds_TNTBE.TBEke1 = RAPdisU;
056100140417         if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
056200140417                          *omit : *omit : *omit :
056300140417                          *omit : *omit :
056400140417                          *omit : *omit : *omit : *omit :
056500140417                          *omit : *omit : *omit : *omit :
056600140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
056700140417                        = *zero      AND
056800140417             ds_TNTBE.TBEatb = *blank;
056900140417           ds17   = ds_TNTBE.TBEuni;
057000140417         endif;
057100140417         RAPdisUd = §17des;
057200140417
057300140417         // -?Decodifica Area della Fil. Unificante?
057400140417         clear  ds05;
057500140417         ds_TNTBE.TBEke1 = %editc( RAPareU : 'X' );
057600140417         if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
057700140417                          *omit : *omit : *omit :
057800140417                          *omit : *omit :
057900140417                          *omit : *omit : *omit : *omit :
058000140417                          *omit : *omit : *omit : *omit :
058100140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
058200140417                        = *zero      AND
058300140417             ds_TNTBE.TBEatb = *blank;
058400140417           ds05   = ds_TNTBE.TBEuni;
058500140417         endif;
058600140417         RAPareUd = §05des;
058700140417
058800140417         // -?Decodifica Autotrasportatore?
058900140417         clear  keyFIAPD01;
059000140417         k_APDtip = 'A';
059100140417         k_APDpdr = RAPpdr;
059200140417         chain  %kds( keyFIAPD01 )  FIAPD000;
059300140417         if  %found(FIAPD01L);
059400140417           RAPpdrD = APDrsc;
059500140417         endif;
059600140417
059700140417       ENDSR;
059800140417
059900140417       //--------------------------------------------------------------
060000140417       //?Stampa segnalazione dell'errore rilevato via SQL             ?
060100140417       //--------------------------------------------------------------
060200140417       BEGSR  sr_PrintERR;
060300140417
060400140417         // -?Stampa del Dump?
060500140417         Dump(A);
060600140417
060700140417         // -?Stampa del Job-Log?
060800140417         Qcmd = 'DSPJOBLOG job(*) output(*print)';
060900140417         exsr  sr_ExecCmd;
061000140417
061100140417       ENDSR;
061200140417
061300140417       //--------------------------------------------------------------
061400140417       //?Esecuzione del comando (già impostato).                      ?
061500140417       //--------------------------------------------------------------
061600140417       BEGSR  sr_ExecCmd;
061700140417
061800140417         clear Qcap0100;
061900140417         Qcabcsdh = *off;
062000140417         Qcapa    = *off;
062100140417         Qcacmdss = *off;
062200140417         Qcaerved = *allX'00';
062300140417
062400140417         clear Qusec;
062500140417         Qusbprv  = %size(Qusec);
062600140417
062700140417         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
062800140417                           %size(Qcap0100) : 'CPOP0100' : *omit :
062900140417                           0 : 0 : Qusec);
063000140417
063100140417         //if  Qusei <> *blank;
063200140417         //  ...;
063300140417         //endif;
063400140417
063500140417       ENDSR;
063600140417
063700140417       //--------------------------------------------------------------
063800140417       //?Operazioni finali.                                           ?
063900140417       //--------------------------------------------------------------
064000140417       BEGSR  sr_RoutEnd;
064100140417
064200140417         // -?Chiusura applicazioni?
064300140417         cloTabella();
064400140417
064500140417         reset  TIBS69ds;
064600140417         tibs69r( TIBS69ds :
064700140417                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
064800140417
064900140417         clear  TIBS10ds;
065000140417         GetKSCfamily ( TIBS10ds : 'C' );
065100140417
065200140417         // -?Uscita dal *pgm?
065300140417         return;
065400140417
065500140417       ENDSR;
065600140417
065700140417      /end-free
