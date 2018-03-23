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
003900140417     fWFRAP11L  if a e           k disk    usropn
004000140417
004100140417       //--------------------------------------------------------------
004200140417       //?Definizione costanti.                                        ?
004300140417       //--------------------------------------------------------------
004400140417
004500140417
004600140417       //--------------------------------------------------------------
004700140417       //?Definizione schiere.                                         ?
004800140417       //--------------------------------------------------------------
004900140417
005000140417
005100140417       //--------------------------------------------------------------
005200140417       //?Definizione aree dati.                                       ?
005300140417       //--------------------------------------------------------------
005400140417
005500140417       // -?Dati utente?
005600140417     d §AzUte        e ds                  extname(AZUTE00F)
005700140417     d                                     dtaara
005800140417     d §DatiUte      e ds                  extname(dDatiUte)
005900140417     d                                     dtaara
006000140417
006100140417       //--------------------------------------------------------------
006200140417       //?Definizione strutture dati.                                  ?
006300140417       //--------------------------------------------------------------
006400140417
006500140417       // -?DS per Numero Spedizione?
006600140417     d ds_Spedizione   ds                  inz   qualified
006700140417     d   wLNP                         3s 0 inz
006800140417     d   wNRS                         2s 0 inz
006900140417     d   wNSP                         7s 0 inz
007000140417     d   wAAS                         4s 0 inz
007100140417
007200140417       // -?Parametri ricevuti?
007300140417     d KPJBA         e ds
007400140417     d FIDND7ds      e ds                  inz   qualified
007500140417
007600140417       // -?Dati estratti via SQL?
007700140417     d FITGDds       e ds                  extname(FITGD00F)
007800140417     d                                     inz   qualified
007900140417
008000140417       // -?DS Estensione FIAR5 tipo record "GEN"?
008100140417     d dAR5gen       e ds                  inz   qualified
008200140417
008300140417       // -?Tab. "05"  = Aree?
008400140417     d ds05          e ds
008500140417       // -?Tab. "17"  = Distretti?
008600140417     d ds17          e ds
008700140417
008800140417       // -?Stringa SQL da eseguire?
008900140417     d wSQL            s           1024    inz   varying
009000140417
009100140417       //--------------------------------------------------------------
009200140417       //?Definizione variabili globali.                               ?
009300140417       //--------------------------------------------------------------
009400140417
009500140417       // -?Flags booleani?
009600140417     d $EoF            s               n   inz
009700140417
009800140417       // -?Campi di comodo?
009900140417     d wDate           s              8  0 inz
010000140417
010100140417       //--------------------------------------------------------------
010200140417       //?Definizione prototipi procedure.                             ?
010300140417       //--------------------------------------------------------------
010400140417
010500140417       // -?Reperimento dati utente?
010600140417     d TIBS34ds      e ds                  inz
010700140417      /copy gaitrasrc/srcProtoPR,TIBS34R
010800140417
010900140417       // -?Ricerca unificante padre?
011000140417      /copy gaitrasrc/srcProtoPI,TIBS10R
011100140417      /copy gaitrasrc/srcProtoPR,TIBS10R
011200140417
011300140417       // -?Controllo/Decodifica cliente?
011400140417      /copy gaitrasrc/srcProtoPI,TIBS69R
011500140417      /copy gaitrasrc/srcProtoPR,TIBS69R
011600140417
011700140417       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
011800140417      /copy gaitrasrc/srcProtoPI,TRULTAB
011900140417      /copy gaitrasrc/srcProtoPR,TRULTAB
012000140417
012100140417       // -?Parametri API QCAPCMD (Process Commands)?
012200140417     d Qcmd            s           2048    inz  varying
012300140417      /copy qSysInc/qRpgleSrc,QCAPCMD
012400140417       // -?API QCAPCMD (Process Commands)?
012500140417      /copy gaitrasrc/srcProtoPR,QCAPCMD
012600140417
012700140417       // -?Parametri gestione errori API.?
012800140417      /copy qsysinc/qrpglesrc,QUSEC
012900140417
013000140417       //--------------------------------------------------------------
013100140417       //?Definizione key-list.                                        ?
013200140417       //--------------------------------------------------------------
013300140417
013400140417       // -?File AZORG01L?
013500140417     d keyAZORG01    e ds                  extname(AZORG01L : *key)
013600140417     d                                     prefix(k_)   inz
013700140417
013800140417       // -?File FIAPD01L?
013900140417     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
014000140417     d                                     prefix(k_)   inz
014100140417
014200140417       // -?File TITAS30C?
014300140417     d keyTITAS30    e ds                  extname(TITAS30C : *key)
014400140417     d                                     prefix(k_)   inz
014500140417       // -?File FIAR531C?
014600140417     d keyFIAR531    e ds                  extname(FIAR531C : *key)
014700140417     d                                     prefix(k_)   inz
014800140417       // -?File FNLBL01L?
014900140417     d keyFNLBL01    e ds                  extname(FNLBL01L : *key)
015000140417     d                                     prefix(k_)   inz
015100140417
015200140417       // -?WrkF WFRAP11L?
015300140417     d keyWFRAP11    e ds                  extname(WFRAP11L : *key)
015400140417     d                                     prefix(k_)   inz
015500140417
015600140417       //--------------------------------------------------------------
015700140417       //?Riepilogo indicatori utilizzati.                             ?
015800140417       //--------------------------------------------------------------
015900140417
016000140417
016100140417       //--------------------------------------------------------------
016200140417       //?M A I N - L I N E                                            ?
016300140417       //--------------------------------------------------------------
016400140417
016500140417     c     *Entry        plist
016600140417     c                   parm                    KPJBA
016700140417
016800140417      /free
016900140417
017000140417       // -?Operazioni iniziali?
017100140417       exsr  sr_RoutInz;
017200140417
017300140417       // -?Ciclo di elaborazione per creare WFRAP10F?
017400140417       reset  $EoF;
017500140417       exsr  sr_OpenCursor;
017600140417
017700140417       DoW  $EoF = *off;
017800140417         exsr  sr_ReadCursor;
017900140417       EndDo;
018000140417
018100140417       exsr  sr_CloseCursor;
018200140417
018300140417       // -?Operazioni finali?
018400140417       exsr sr_RoutEnd;
018500140417
018600140417       //--------------------------------------------------------------
018700140417       //?Operazioni iniziali.                                         ?
018800140417       //--------------------------------------------------------------
018900140417       BEGSR  sr_RoutInz;
019000140417
019100140417         // -?Impostazione opzioni per SQL?
019200140417         exec sql   set  option  DynUsrPrf = *Owner,
019300140417                                 CloSqlCsr = *EndMod;
019400140417
019500140417         // -?Impostazione chiusura?
019600140417         *inLR = *on;
019700140417
019800140417         // -?Reperimento dati job?
019900140417         exsr  sr_DatiJob;
020000140417
020100140417         // -?Reperimento data odierna in formato aaaammgg?
020200140417         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020300140417                               : 1 : 8 ) );
020400140417
020500140417         // -?Reperimento parametri?
020600140417         FIDND7ds = kpjbu;
020700140417
020800140417         // -?Pulizia del WrkF GAITRAAZM/WFRAP10F?
020900140417         Qcmd = 'CLRPFM file(';
021000140417         if  %subst( KNSIF : 7 : 1 ) = 'P'; //?(AS888)?
021100140417           Qcmd += 'GAITRAAZP';
021200140417         else;                              //?(SETRAS)?
021300140417           Qcmd += 'GAITRAAZM';
021400140417         endif;
021500140417         Qcmd += '/WFRAP10F)';
021600140417         exsr  sr_ExecCmd;
021700140417         if  Qusei <> *blank;
021800140417           $EoF = *on;
021900140417           exsr  sr_PrintERR;
022000140417           exsr  sr_RoutEnd;
022100140417         endif;
022200140417
022300140417         // -?Apertura del WrkF WFRAP10F?
022400140417         open  WFRAP11L;
022500140417
022600140417         // -?Preparazione stringa SQL?
022700140417         exsr  sr_PrepSQL;
022800140417
022900140417       ENDSR;
023000140417
023100140417       //--------------------------------------------------------------
023200140417       //?Reperimento Dati del job (Utente/Operativi).                 ?
023300140417       //--------------------------------------------------------------
023400140417       BEGSR  sr_DatiJob;
023500140417
023600140417         in(E) §AzUte;
023700140417         if NOT %error;
023800140417           in(E) §DatiUte;
023900140417         endif;
024000140417         if %error or RSut = *blanks;
024100140417           clear TIBS34ds;
024200140417           tibs34r ( tibs34ds );
024300140417           in §AzUte;
024400140417           in §DatiUte;
024500140417         endif;
024600140417
024700140417       ENDSR;
024800140417
024900140417       //--------------------------------------------------------------
025000140417       //?Preparazione stringa SQL per l'estrazione da FITGD00F.       ?
025100140417       //--------------------------------------------------------------
025200140417       BEGSR  sr_PrepSQL;
025300140417
025400140417         wSQL = 'select * +
025500140417                   from FITGD00F +
025600140417                  where TGDdac between ' +
025700140417                        %editc( FIDND7ds.iD7dtI : '3' ) + ' and ' +
025800140417                        %editc( FIDND7ds.iD7dtF : '3' ) +
025900140417                  ' and TGDtor = ''S'' +
026000140417                    and TGDmad in (''  8'', '' 88'') +
026100140417                  order by TGDdac, TGDtor, TGDogg, TGDmad +
026200140417                    for fetch only';
026300140417
026400140417       ENDSR;
026500140417
026600140417       //--------------------------------------------------------------
026700140417       //?Apertura cursore.                                            ?
026800140417       //--------------------------------------------------------------
026900140417       BEGSR  sr_OpenCursor;
027000140417
027100140417         // -?Dichiarazione cursore?
027200140417         exec sql   prepare S1   from :wSQL;
027300140417         exec sql   declare C1   cursor for S1;
027400140417
027500140417         if  SQLcode < *zero;
027600140417           $EoF = *on;
027700140417           exsr  sr_PrintERR;
027800140417         endif;
027900140417
028000140417         // -?Apertura del cursore?
028100140417         exec sql   open C1;
028200140417
028300140417         if  SQLcode < *zero;
028400140417           $EoF = *on;
028500140417           exsr  sr_PrintERR;
028600140417         endif;
028700140417
028800140417       ENDSR;
028900140417
029000140417       //--------------------------------------------------------------
029100140417       //?Chiusura cursore.                                            ?
029200140417       //--------------------------------------------------------------
029300140417       BEGSR  sr_CloseCursor;
029400140417
029500140417         // -?Chiusura del cursore?
029600140417         exec sql   close C1;
029700140417
029800140417         // -?Chiusura del WrkF?
029900140417         close  WFRAP11L;
030000140417
030100140417       ENDSR;
030200140417
030300140417       //--------------------------------------------------------------
030400140417       //?Lettura cursore.                                             ?
030500140417       //--------------------------------------------------------------
030600140417       BEGSR  sr_ReadCursor;
030700140417
030800140417         clear  FITGDds;
030900140417
031000140417         exec sql   fetch next   from C1   into :FITGDds;
031100140417
031200140417         select;
031300140417           when  SQLcode = 100;
031400140417             $EoF = *on;
031500140417           when  SQLcode < *zero;
031600140417             $EoF = *on;
031700140417             exsr  sr_PrintERR;
031800140417           other;
031900140417             exsr  sr_WriteWFRAP;
032000140417         endsl;
032100140417
032200140417       ENDSR;
032300140417
032400140417       //--------------------------------------------------------------
032500140417       //?Scrittura record nel WrkF WFRAP10F.                          ?
032600140417       //--------------------------------------------------------------
032700140417       BEGSR  sr_WriteWFRAP;
032800140417
032900140417         ds_Spedizione = FITGDds.TGDogg;
033000140417
033100140417         // -?Pulizia campi nel record?
033200140417         clear  WFRAP000;
033300140417
033400140417         // -?Dati della spedizione (...avesse mai un "mamma"!)?
033500140417         clear  keyTITAS30;
033600140417         k_TASaas = ds_Spedizione.wAAS;
033700140417         k_TASlnp = ds_Spedizione.wLNP;
033800140417         k_TASnrs = ds_Spedizione.wNRS;
033900140417         k_TASnsp = ds_Spedizione.wNSP;
034000140417         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
034100140417         if  %found(TITAS30C);
034200140417           RAPlna = TASlna;
034300140417           RAPpdr = TASpdr;
034400140417           RAPrsd = TASrsd;
034500140417         endif;
034600140417
034700140417         // -?Verifica SE c'è Mamma?
034800140417         clear  keyFNLBL01;
034900140417         k_LBLaan = ds_Spedizione.wAAS;
035000140417         k_LBLlpn = ds_Spedizione.wLNP;
035100140417         k_LBLnrn = ds_Spedizione.wNRS;
035200140417         k_LBLnsn = ds_Spedizione.wNSP;
035300140417         chain  %kds( keyFNLBL01 )  FNLBL000;
035400140417
035500140417         If  %found(FNLBL01L);
035600140417
035700140417           ds_Spedizione.wAAS = LBLaao;
035800140417           ds_Spedizione.wLNP = LBLlpo;
035900140417           ds_Spedizione.wNRS = LBLnro;
036000140417           ds_Spedizione.wNSP = LBLnso;
036100140417
036200140417           clear  keyTITAS30;
036300140417           k_TASaas = ds_Spedizione.wAAS;
036400140417           k_TASlnp = ds_Spedizione.wLNP;
036500140417           k_TASnrs = ds_Spedizione.wNRS;
036600140417           k_TASnsp = ds_Spedizione.wNSP;
036700140417           chain  %kds( keyTITAS30 : 4 )  TITAS30C;
036800140417
036900140417         EndIf;
037000140417
037100140417         // -?Spedizione NON trovata su TITAS30C?
037200140417         If  NOT %found(TITAS30C);
037300140417           leavesr;
037400140417         EndIf;
037500140417
037600140417         // ·?Verifica SE già inserita questa spedizione nel WrkF?
037700140417         clear  keyWFRAP11;
037800140417         k_RAPaas = ds_Spedizione.wAAS;
037900140417         k_RAPlnp = ds_Spedizione.wLNP;
038000140417         k_RAPnrs = ds_Spedizione.wNRS;
038100140417         k_RAPnsp = ds_Spedizione.wNSP;
038200140417         setll  %kds( keyWFRAP11 )  WFRAP000;
038300140417         if  %equal(WFRAP11L);
038400140417           leavesr;
038500140417         endif;
038600140417
038700140417         // -?Impostazione dati nel record:?
038800140417
038900140417         // ·?Parametri di lancio?
039000140417         RAPute   = DUTute;
039100140417         RAPdta   = wDate;
039200140417         RAPddr   = FIDND7ds.iD7dti;
039300140417         RAPadr   = FIDND7ds.iD7dtf;
039400140417
039500140417         // ·?Numero Spedizione?
039600140417         RAPaas = ds_Spedizione.wAAS;
039700140417         RAPlnp = ds_Spedizione.wLNP;
039800140417         RAPnrs = ds_Spedizione.wNRS;
039900140417         RAPnsp = ds_Spedizione.wNSP;
040000140417
040100140417         // ·?Dati Reclamo?
040200140417         RAPant = FITGDds.TGDant;
040300140417         RAPnut = FITGDds.TGDnut;
040400140417         RAPpdt = FITGDds.TGDpdt;
040500140417         RAPdac = FITGDds.TGDdac;
040600140417         RAPcdt = FITGDds.TGDcdt;
040700140417
040800140417         // ·?Cod. Cliente Mittente?
040900140417         if  TASccm > *zero;
041000140417           RAPccm = TASccm;
041100140417         else;
041200140417           RAPccm = TASksc;
041300140417         endif;
041400140417
041500140417         clear  dAR5gen;
041600140417         // ·?DDT?
041700140417         If  TASll1 = 'S';
041800140417           RAPflg = 'S';
041900140417         // ·?Packing-List?
042000140417         Else;
042100140417           clear  keyFIAR531;
042200140417           k_AR5aas = TASaas;
042300140417           k_AR5lnp = TASlnp;
042400140417           k_AR5nrs = TASnrs;
042500140417           k_AR5nsp = TASnsp;
042600140417           k_AR5trd = 'GEN';
042700140417           chain  %kds( keyFIAR531 : 5 )  FIAR531C;
042800140417           if  %found(FIAR531C);
042900140417             dAR5gen = AR5uni;
043000140417             if  dAR5gen.§AR5als = 'S'  or  dAR5gen.§AR5alx = 'S';
043100140417               RAPflg = 'P';
043200140417             endif;
043300140417           endif;
043400140417         EndIf;
043500140417
043600140417         // ·?Data Consegna merce?
043700140417         RAPdcm = TASdcm;
043800140417
043900140417         // ·?Decodifiche?
044000140417         exsr  sr_Decod;
044100140417
044200140417         // -?Scrittura record?
044300140417         //______________
044400140417         write  WFRAP000;
044500140417         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
044600140417
044700140417         feod  WFRAP11L;
044800140417
044900140417       ENDSR;
045000140417
045100140417       //--------------------------------------------------------------
045200140417       //?Decodifica dei dati.                                         ?
045300140417       //--------------------------------------------------------------
045400140417       BEGSR  sr_Decod;
045500140417
045600140417         // -?Decodifica cliente (CCM)?
045700140417         clear  TIBS69ds;
045800140417         I69sif = knsif;
045900140417         I69kcc = DUTkci;
046000140417         I69kac = RAPccm;
046100140417         tibs69r( TIBS69ds :
046200140417                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
046300140417         if  O69err = *blank;
046400140417           RAPccmD  = ACOrag;
046500140417         endif;
046600140417
046700140417         // -?Reperimento codice cliente Unificante?
046800140417         clear  TIBS10ds;
046900140417         TIBS10ds.D10drf = wDate;
047000140417         TIBS10ds.D10tle = 'ST';
047100140417         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
047200140417         TIBS10ds.D10cod = RAPccm;
047300140417         //tibs10r ( TIBS10ds );
047400140417         GetKSCfamily ( TIBS10ds : 'R' );
047500140417         if  TIBS10ds.D10err = *blank  and
047600140417             TIBS10ds.D10cop > *zero;
047700140417           RAPuni = TIBS10ds.D10cop;
047800140417         else;
047900140417           RAPuni = TIBS10ds.D10cod;
048000140417         endif;
048100140417
048200140417         // -?Decodifica cliente Unificante?
048300140417         If  RAPccm <> RAPuni;
048400140417           clear  TIBS69ds;
048500140417           I69sif = knsif;
048600140417           I69kcc = DUTkci;
048700140417           I69kac = RAPuni;
048800140417           tibs69r( TIBS69ds :
048900140417                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
049000140417           if  O69err = *blank;
049100140417             RAPuniD  = ACOrag;
049200140417           endif;
049300140417         Else;
049400140417           RAPuniD = RAPccmD;
049500140417         EndIf;
049600140417
049700140417         // -?Decodifica Linea di Arrivo?
049800140417         //  ?e reperimento Distretto e Area?
049900140417         clear  keyAZORG01;
050000140417         k_ORGfil = RAPlna;
050100140417         chain  %kds( keyAZORG01 )  AZORG;
050200140417         if  %found(AZORG01L)  and  ORGfva = *blank;
050300140417           RAPlnad = ORGdes;
050400140417           RAPdisA = ORGfl3;
050500140417           RAPareA = ORGcar;
050600140417         endif;
050700140417
050800140417         // -?Decodifica Distretto della LNA?
050900140417         clear  ds17;
051000140417         ds_TNTBE.TBEke1 = RAPdisA;
051100140417         if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
051200140417                          *omit : *omit : *omit :
051300140417                          *omit : *omit :
051400140417                          *omit : *omit : *omit : *omit :
051500140417                          *omit : *omit : *omit : *omit :
051600140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
051700140417                        = *zero      AND
051800140417             ds_TNTBE.TBEatb = *blank;
051900140417           ds17   = ds_TNTBE.TBEuni;
052000140417         endif;
052100140417         RAPdisAd = §17des;
052200140417
052300140417         // -?Decodifica Area della LNA?
052400140417         clear  ds05;
052500140417         ds_TNTBE.TBEke1 = %editc( RAPareA : 'X' );
052600140417         if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
052700140417                          *omit : *omit : *omit :
052800140417                          *omit : *omit :
052900140417                          *omit : *omit : *omit : *omit :
053000140417                          *omit : *omit : *omit : *omit :
053100140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
053200140417                        = *zero      AND
053300140417             ds_TNTBE.TBEatb = *blank;
053400140417           ds05   = ds_TNTBE.TBEuni;
053500140417         endif;
053600140417         RAPareAd = §05des;
053700140417
053800140417         // -?Decodifica Filiale Unificante?
053900140417         //  ?e reperimento Distretto e Area?
054000140417         RAPfilU = RAPuni / 10000;
054100140417         k_ORGfil = RAPfilU;
054200140417         chain  %kds( keyAZORG01 )  AZORG;
054300140417         if  %found(AZORG01L)  and  ORGfva = *blank;
054400140417           RAPfilUd = ORGdes;
054500140417           RAPdisU  = ORGfl3;
054600140417           RAPareU  = ORGcar;
054700140417         endif;
054800140417
054900140417         // -?Decodifica Distretto della Fil. Unificante?
055000140417         clear  ds17;
055100140417         ds_TNTBE.TBEke1 = RAPdisU;
055200140417         if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
055300140417                          *omit : *omit : *omit :
055400140417                          *omit : *omit :
055500140417                          *omit : *omit : *omit : *omit :
055600140417                          *omit : *omit : *omit : *omit :
055700140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
055800140417                        = *zero      AND
055900140417             ds_TNTBE.TBEatb = *blank;
056000140417           ds17   = ds_TNTBE.TBEuni;
056100140417         endif;
056200140417         RAPdisUd = §17des;
056300140417
056400140417         // -?Decodifica Area della Fil. Unificante?
056500140417         clear  ds05;
056600140417         ds_TNTBE.TBEke1 = %editc( RAPareU : 'X' );
056700140417         if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
056800140417                          *omit : *omit : *omit :
056900140417                          *omit : *omit :
057000140417                          *omit : *omit : *omit : *omit :
057100140417                          *omit : *omit : *omit : *omit :
057200140417                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
057300140417                        = *zero      AND
057400140417             ds_TNTBE.TBEatb = *blank;
057500140417           ds05   = ds_TNTBE.TBEuni;
057600140417         endif;
057700140417         RAPareUd = §05des;
057800140417
057900140417         // -?Decodifica Autotrasportatore?
058000140417         clear  keyFIAPD01;
058100140417         k_APDtip = 'A';
058200140417         k_APDpdr = RAPpdr;
058300140417         chain  %kds( keyFIAPD01 )  FIAPD000;
058400140417         if  %found(FIAPD01L);
058500140417           RAPpdrD = APDrsc;
058600140417         endif;
058700140417
058800140417         // -?Filiale Responsabile?
058900140417         RAPfilR = FITGDds.TGDpor;
059000140417
059100140417         // -?Motivo Apertura R.A.?
059200140417         RAPmad  = FITGDds.TGDmad;
059300140417
059400140417       ENDSR;
059500140417
059600140417       //--------------------------------------------------------------
059700140417       //?Stampa segnalazione dell'errore rilevato via SQL             ?
059800140417       //--------------------------------------------------------------
059900140417       BEGSR  sr_PrintERR;
060000140417
060100140417         // -?Stampa del Dump?
060200140417         Dump(A);
060300140417
060400140417         // -?Stampa del Job-Log?
060500140417         Qcmd = 'DSPJOBLOG job(*) output(*print)';
060600140417         exsr  sr_ExecCmd;
060700140417
060800140417       ENDSR;
060900140417
061000140417       //--------------------------------------------------------------
061100140417       //?Esecuzione del comando (già impostato).                      ?
061200140417       //--------------------------------------------------------------
061300140417       BEGSR  sr_ExecCmd;
061400140417
061500140417         clear Qcap0100;
061600140417         Qcabcsdh = *off;
061700140417         Qcapa    = *off;
061800140417         Qcacmdss = *off;
061900140417         Qcaerved = *allX'00';
062000140417
062100140417         clear Qusec;
062200140417         Qusbprv  = %size(Qusec);
062300140417
062400140417         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
062500140417                           %size(Qcap0100) : 'CPOP0100' : *omit :
062600140417                           0 : 0 : Qusec);
062700140417
062800140417         //if  Qusei <> *blank;
062900140417         //  ...;
063000140417         //endif;
063100140417
063200140417       ENDSR;
063300140417
063400140417       //--------------------------------------------------------------
063500140417       //?Operazioni finali.                                           ?
063600140417       //--------------------------------------------------------------
063700140417       BEGSR  sr_RoutEnd;
063800140417
063900140417         // -?Chiusura applicazioni?
064000140417         cloTabella();
064100140417
064200140417         reset  TIBS69ds;
064300140417         tibs69r( TIBS69ds :
064400140417                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
064500140417
064600140417         clear  TIBS10ds;
064700140417         GetKSCfamily ( TIBS10ds : 'C' );
064800140417
064900140417         // -?Uscita dal *pgm?
065000140417         return;
065100140417
065200140417       ENDSR;
065300140417
065400140417      /end-free
