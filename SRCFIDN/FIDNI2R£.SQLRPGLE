000100150724       //==============================================================
000200150724       //?Elenco Spediz. Giornaliere con Imp.d'Assicurare eccessivo BCH?
000300150724       //==============================================================
000400150724
000500150724       //--------------------------------------------------------------
000600150724       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150724       //--------------------------------------------------------------
000800150724
000900150724     /*PRM dbgview(*source)
001000150724     /*//CMD ovrdbf file(WFIAE00F) tofile(GAITRAAZP/WFIAE00F) +
001100150724     /*//CMD        ovrscope(*calllvl)
001200150724     /*//END dltovr file(WFIAE00F) lvl(*)
001300150724     /*END
001400150724
001500150724       //--------------------------------------------------------------
001600150724       //?Specifiche di controllo.                                     ?
001700150724       //--------------------------------------------------------------
001800150724
001900150724     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000150724     h alwnull(*inputonly)
002100150724     h dftactgrp(*no)
002200150724
002300150724       //--------------------------------------------------------------
002400150724       //?Dichiarazione file.                                          ?
002500150724       //--------------------------------------------------------------
002600150724
002700150724      // -?WorkFile - Spedizioni Giornaliere con Imp.Assicurare eccessivo?
002800150724     fWFIAE00F  o    e             disk    usropn
002900150724
003000150724       // -?Printer File?
003100150724     fTNSB59P   o    e             printer
003200150724     f                                     oflind(*in25)
003300150724
003400150724       // -?Stampa segnalazioni di errore?
003500150724     fQSYSPRT   o    f  132        printer usropn
003600150724     f                                     oflind(*inOF)
003700150724
003800150724       //--------------------------------------------------------------
003900150724       //?Definizione costanti.                                        ?
004000150724       //--------------------------------------------------------------
004100150724
004200150724       // -?Comandi da eseguire?
004300150724     d c_CrtDupObj     c                   const('CRTDUPOBJ +
004400150724     d                                            obj(WFIAE00F) +
004500150724     d                                            fromlib(GAITRAAZM ) +
004600150724     d                                            objtype(*file) +
004700150724     d                                            tolib(QTEMP) +
004800150724     d                                            data(*NO)')
004900150724     d c_CpyF          c                   const('CPYF +
005000150724     d                                            fromfile(QTEMP/+
005100150724     d                                                     WFIAE00F) +
005200150724     d                                            tofile(GAITRAAZM/+
005300150724     d                                                     WFIAE00F) +
005400150724     d                                            mbropt(*replace) +
005500150724     d                                            crtfile(*no)')
005600150724     d c_OvrPrtF       c                   const('OVRPRTF +
005700150724     d                                            file(QSYSPRT) +
005800150724     d                                            usrdta(''TNSB59+
005900150724     d                                                     *ERR'')')
006000150724     d c_DltOvr        c                   const('DLTOVR +
006100150724     d                                            file(QSYSPRT)')
006200150724
006300150724       //--------------------------------------------------------------
006400150724       //?Definizione schiere.                                         ?
006500150724       //--------------------------------------------------------------
006600150724
006700150724
006800150724       //--------------------------------------------------------------
006900150724       //?Definizione aree dati.                                       ?
007000150724       //--------------------------------------------------------------
007100150724
007200150724       // -?Dati utente?
007300150724     d §AzUte        e ds                  extname(AZUTE00F)
007400150724     d                                     dtaara
007500150724     d §DatiUte      e ds                  extname(dDatiUte)
007600150724     d                                     dtaara
007700150724
007800150724       //--------------------------------------------------------------
007900150724       //?Definizione strutture dati.                                  ?
008000150724       //--------------------------------------------------------------
008100150724
008200150724       // -?Status ds?
008300150724     d Status         sds
008400150724     d   SDSpgmName      *proc
008500150724
008600150724       // -?Parametri ricevuti?
008700150724     d KPJBA         e ds
008800150724     d TNSB59ds      e ds                  inz
008900150724
009000150724       // -?Dati estratti via SQL?
009100150724     d WFIAEds       e ds                  inz   qualified   extname(WFIAE00F)
009200150724     d WTIAEds         ds            81    inz   qualified
009300150724     d   TOTksc                            inz   like(TASksc)
009400150724     d   TOTrsm                            inz   like(ACOrag)
009500150724     d   TOTrsd                            inz   like(TASrsd)
009600150724     d   TOTcad                            inz   like(TAScad)
009700150724     d   TOTdts                            inz   like(TASdrt)
009800150724     d   TOTias                            inz   like(TASias)
009900150724     d   TOTnum                       7  0 inz
010000150724
010100150724       //--------------------------------------------------------------
010200150724       //?Definizione variabili globali.                               ?
010300150724       //--------------------------------------------------------------
010400150724
010500150724       // -?Flags booleani?
010600150724     d $EoF            s               n   inz
010700150724
010800150724       // -?Indici di schiera / Contatori?
010900150724     d wErr            s              3  0 inz
011000150724
011100150724       // -?Stringhe SQL da eseguire?
011200150724     d wSQL1           s          32740    inz   varying
011300150724     d wSQL2           s          32740    inz   varying
011400150724
011500150724       // -?Campi di comodo?
011600150724     d wTime           s              6  0 inz
011700150724     d wDate           s              8  0 inz
011800150724     d wDate_Iso       s               d   inz  datfmt(*iso)
011900150724     d wDate_Eur       s               d   inz  datfmt(*eur)
012000150724
012100150724       //--------------------------------------------------------------
012200150724       //?Definizione prototipi procedure.                             ?
012300150724       //--------------------------------------------------------------
012400150724
012500150724       // -?Reperimento NETA sistema AS/400 corrente?
012600150724     d currSysNeta     s              8a   inz
012700150724      /copy gaitrasrc/srcProtoPR,UBRTVNETA
012800150724
012900150724       // -?Reperimento dati utente?
013000150724     d TIBS34ds      e ds                  inz
013100150724      /copy gaitrasrc/srcProtoPR,TIBS34R
013200150724
013300150724       // -?Controllo/Decodifica cliente?
013400150724      /copy gaitrasrc/srcProtoPI,TIBS69R
013500150724      /copy gaitrasrc/srcProtoPR,TIBS69R
013600150724
013700150724       // -?Parametri API QCAPCMD (Process Commands)?
013800150724     d Qcmd            s           2048    inz  varying
013900150724      /copy qSysInc/qRpgleSrc,QCAPCMD
014000150724       // -?API QCAPCMD (Process Commands)?
014100150724      /copy gaitrasrc/srcProtoPR,QCAPCMD
014200150724
014300150724       // -?Parametri gestione errori API.?
014400150724      /copy qsysinc/qrpglesrc,QUSEC
014500150724
014600150724       //--------------------------------------------------------------
014700150724       //?Definizione key-list.                                        ?
014800150724       //--------------------------------------------------------------
014900150724
015000150724
015100150724       //--------------------------------------------------------------
015200150724       //?Riepilogo indicatori utilizzati.                             ?
015300150724       //--------------------------------------------------------------
015400150724
015500150724
015600150724       //--------------------------------------------------------------
015700150724       //?M A I N - L I N E                                            ?
015800150724       //--------------------------------------------------------------
015900150724
016000150724     c     *Entry        plist
016100150724     c                   parm                    KPJBA
016200150724
016300150724      /free
016400150724
016500150724       // -?Operazioni iniziali?
016600150724       exsr  sr_RoutInz;
016700150724
016800150724       // -?Preparazione stringhe SQL?
016900150724       exsr  sr_PrepSQL;
017000150724
017100150724       // -?Ciclo di elaborazione TITAS30C per creare QTEMP/WFIAE00F?
017200150724       reset  $EoF;
017300150724       exsr  sr_OpenCursor1;
017400150724
017500150724       DoW  $EoF = *off;
017600150724         exsr  sr_ReadCursor1;
017700150724       EndDo;
017800150724
017900150724       exsr  sr_CloseCursor1;
018000150724
018100150724       // -?Ciclo di lettura QTEMP/WFIAE00F per stampa?
018200150724       //If  SB59out = 'S'  or  SB59out = 'E';
018300150724
018400150724         reset  $EoF;
018500150724         exsr  sr_OpenCursor2;
018600150724
018700150724         DoW  $EoF = *off;
018800150724           exsr  sr_ReadCursor2;
018900150724         EndDo;
019000150724
019100150724         exsr  sr_CloseCursor2;
019200150724
019300150724       //EndIf;
019400150724
019500150724       // -?Copia del WrkF QTEMP/WFIAE00F in GAITRAAZM?
019600150724       //  ?SE estratti dati?
019700150724       If  ( SB59out = 'F'  or  SB59out = 'E' )  and  $Almeno1rec;
019800150724
019900150724         if  %subst( currSysNeta : 1 : 6 ) = 'SETRAS';
020000150724           Qcmd = c_CpyF;
020100150724         else;
020200150724           Qcmd = %trim( %replace( 'GAITRAAZP' : c_CpyF :
020300150724                            %scan( 'GAITRAAZM' : c_CpyF ) ) );
020400150724         endif;
020500150724         exsr  sr_ExecCmd;
020600150724
020700150724         if  Qusei <> *blank;
020800150724           wErr = 4;
020900150724           exsr  sr_PrintERR;
021000150724         endif;
021100150724
021200150724       EndIf;
021300150724
021400150724       // -?Operazioni finali?
021500150724       exsr sr_RoutEnd;
021600150724
021700150724       //--------------------------------------------------------------
021800150724       //?Operazioni iniziali.                                         ?
021900150724       //--------------------------------------------------------------
022000150724       BEGSR  sr_RoutInz;
022100150724
022200150724         // -?Impostazione opzioni per SQL?
022300150724         exec sql   set  option  DynUsrPrf = *Owner,
022400150724                                 CloSqlCsr = *EndMod;
022500150724
022600150724         // -?Impostazione chiusura?
022700150724         *inLR = *on;
022800150724
022900150724         // -?Reperimento dati job?
023000150724         exsr  sr_DatiJob;
023100150724
023200150724         // -?Reperimento data odierna in formato aaaammgg?
023300150724         wDate_Iso = %date();
023400150724         wDate_Eur = wDate_Iso;
023500150724         wDate = %dec( wDate_Eur );
023600150724
023700150724         // -?Reperimento orario?
023800150724         wTime = %dec( %time() );
023900150724
024000150724         // -?Reperimento parametri?
024100150724         TNSB59ds = kpjbu;
024200150724
024300150724         // -?Verifica del sistema AS/400 corrente?
024400150724         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
024500150724           exsr  sr_RoutEnd;
024600150724         endif;
024700150724
024800150724         // -?Copia del WrkF GAITRAAZM/WFPDL00F in QTEMP?
024900150724         if  %subst( currSysNeta : 1 : 6 ) = 'SETRAS';
025000150724           Qcmd = c_CrtDupObj;
025100150724         else;
025200150724           Qcmd = %trim( %replace( 'GAITRAAZP' : c_CrtDupObj :
025300150724                            %scan( 'GAITRAAZM' : c_CrtDupObj ) ) );
025400150724         endif;
025500150724         exsr  sr_ExecCmd;
025600150724         if  Qusei <> *blank;
025700150724           $EoF = *on;
025800150724           wErr = 3;
025900150724           exsr  sr_PrintERR;
026000150724           exsr  sr_RoutEnd;
026100150724         endif;
026200150724
026300150724         // -?Apertura del WrkF QTEMP/WFIAE00F?
026400150724         open  WFIAE00F;
026500150724
026600150724         // -?Impostazione campi di stampa in testata?
026700150724         T1Cpgm = SDSpgmName;
026800150724         T1Cdta = wDate;
026900150724         T1Cora = wTime;
027000150724
027100150724         wDate_Eur = %date( SB59dsi : *iso );
027200150724         T1Cdsi    = %dec( wDate_Eur );
027300150724         wDate_Eur = %date( SB59dsf : *iso );
027400150724         T1Cdsf    = %dec( wDate_Eur );
027500150724
027600150724       ENDSR;
027700150724
027800150724       //--------------------------------------------------------------
027900150724       //?Reperimento Dati del job (Utente/Operativi).                 ?
028000150724       //--------------------------------------------------------------
028100150724       BEGSR  sr_DatiJob;
028200150724
028300150724         in(e) §AzUte;
028400150724         if NOT %error;
028500150724           in(e) §DatiUte;
028600150724         endif;
028700150724         if %error or RSut = *blank;
028800150724           tibs34r ( tibs34ds );
028900150724           in §AzUte;
029000150724           in §DatiUte;
029100150724         endif;
029200150724
029300150724       ENDSR;
029400150724
029500150724       //--------------------------------------------------------------
029600150724       //?Preparazione 1ª stringa SQL (per l'estrazione da TIPDL).     ?
029700150724       //--------------------------------------------------------------
029800150724       BEGSR  sr_PrepSQL1;
029900150724
030000150724         wSQL1 = 'with +
030100150724
030200150724                    TITASP as ( select TASksc, TASrsd, TAScad, TASias, +
030300150724                                       TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
030400150724                                       TASdcm +
030500150724                                  from TITASP0F +
030600150724                                 where TAStbl = ''F1'' +
030700150724                                   and TASias > 0 +
030800150724                                   and TASfcm = '' '' +
030900150727                                   and ( (TASaas * 10000) + TASmgs ) between ' +
031000150727                                       %trimL( %editc( SB59dsi : '3' ) ) +
031100150727                                       ' and ' +
031200150727                                       %trimL( %editc( SB59dsf : '3' ) ) +
031300150727                               ' order by TASaas, TASmgs, TASksc, TAScad ), +
031400150724
031500150724                    TITAS1 as ( select TASksc, TASrsd, TAScad, TASias, +
031600150724                                       TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
031700150724                                       TASdcm +
031800150724                                  from TITAS10F +
031900150724                                 where TAStbl = ''F1'' +
032000150724                                   and TASias > 0 +
032100150724                                   and TASfcm = '' '' +
032200150724                                 order by TASaas, TASmgs, TASksc, TAScad ), +
032300150724
032400150724                    TITAS0 as ( select TASksc, TASrsd, TAScad, TASias, +
032500150724                                       TASaas, TASmgs, TASlnp, TASnrs, TASnsp, +
032600150724                                       TASdcm +
032700150724                                  from TITAS00F +
032800150724                                 where TAStbl = ''F1'' +
032900150724                                   and TASias > 0 +
033000150724                                   and TASfcm = '' '' +
033100150724                                 order by TASaas, TASmgs, TASksc, TAScad ), +
033200150724
033300150724                    TITAS  as ( select * from TITASP union +
033400150724                                select * from TITAS1 union +
033500150724                                select * from TITAS0 +
033600150724                                 order by TASaas, TASmgs, TASksc, TAScad ) +
033700150724
033800150724                  select TASksc, ACOrag, TASrsd, TAScad, +
033900150724                         int( TASaas * 10000 + TASmgs ) as Data, +
034000150724                         sum(TASias), count(*) +
034100150724                    from TITAS left outer join CNACO00F +
034200150727                      on TASksc = ACOksc and +
034300150727                         ACOkcc = ' + %trimL( %editc( DUTkci : '3' ) ) +
034400150724
034500150727                 ' group by TASksc, ACOrag, TASrsd, TAScad, TASaas, TASmgs +
034600150724                  having sum(TASias) > 100000 +
034700150724                   order by TASksc, ACOrag, TASrsd, TAScad, TASaas, TASmgs +
034800150724
034900150724                     for fetch only';
035000150724
035100150724       ENDSR;
035200150724
035300150724       //--------------------------------------------------------------
035400150724       //?Apertura cursore 1.                                          ?
035500150724       //--------------------------------------------------------------
035600150724       BEGSR  sr_OpenCursor1;
035700150724
035800150724         // -?Dichiarazione cursore?
035900150724         exec sql   prepare S1   from :wSQL1;
036000150724         exec sql   declare C1   cursor for S1;
036100150724
036200150724         if  SQLcode < *zero;
036300150724           $EoF = *on;
036400150724           wErr = 1;
036500150724           exsr  sr_PrintERR;
036600150724         endif;
036700150724
036800150724         // -?Apertura del cursore?
036900150724         exec sql   open C1;
037000150724
037100150724         if  SQLcode < *zero;
037200150724           $EoF = *on;
037300150724           wErr = 1;
037400150724           exsr  sr_PrintERR;
037500150724         endif;
037600150724
037700150724       ENDSR;
037800150724
037900150724       //--------------------------------------------------------------
038000150724       //?Chiusura cursore 1.                                          ?
038100150724       //--------------------------------------------------------------
038200150724       BEGSR  sr_CloseCursor1;
038300150724
038400150724         // -?Chiusura del cursore?
038500150724         exec sql   close C1;
038600150724
038700150724         // -?Chiusura del WrkF QTEMP/WFIAE00F?
038800150724         close  WFIAE00F;
038900150724
039000150724       ENDSR;
039100150724
039200150724       //--------------------------------------------------------------
039300150724       //?Lettura cursore 1.                                           ?
039400150724       //--------------------------------------------------------------
039500150724       BEGSR  sr_ReadCursor1;
039600150724
039700150724         clear  WFIAEds;
039800150724
039900150724         exec sql   fetch next   from C1   into :WFIAEds;
040000150724
040100150724         select;
040200150724           when  SQLcode = 100;
040300150724             $EoF = *on;
040400150724           when  SQLcode < *zero;
040500150724             $EoF = *on;
040600150724             wErr = 1;
040700150724             exsr  sr_PrintERR;
040800150724           other;
040900150724             exsr  sr_SelezioneRec;
041000150724             if  $Ok;
041100150724               exsr  sr_WriteWFIAE;
041200150724             endif;
041300150724         endsl;
041400150724
041500150724       ENDSR;
041600150724
041700150724       //--------------------------------------------------------------
041800150724       //?Selezione del singolo record in base alle parzializzazioni.  ?
041900150724       //--------------------------------------------------------------
042000150724       BEGSR  sr_SelezioneRec;
042100150724
042200150724         $Ok = *off;
042300150724
042400150724         // -?Pulizia dei campi nel record?
042500150724         clear  WFPDL000;
042600150724
042700150724
042800150724         // -?Reperimento Cliente Unificante?
042900150724         clear  TIBS10ds;
043000150724         TIBS10ds.D10drf = %dec( wDate_Iso );
043100150724         TIBS10ds.D10tle = 'ST';
043200150724         TIBS10ds.D10paf = 'P';   //?Verifica se è figlio e ne reperisce il padre?
043300150724         TIBS10ds.D10cod = PDLksc;
043400150724         tibs10r ( TIBS10ds );
043500150724         if  TIBS10ds.D10err = *blank  and  TIBS10ds.D10cop > *zero;
043600150724           WPDksu = TIBS10ds.D10cop;
043700150724         else;
043800150724           WPDksu = TIBS10ds.D10cod;
043900150724         endif;
044000150724
044100150724
044200150724         // -?Reperimento Commerciale del Cliente Unificante?
044300150724         clear  TIBS69ds;
044400150724         I69sif = knsif;
044500150724         I69kcc = DUTkci;
044600150724         I69kac = %int( WPDksu );
044700150724         I69kcp = I69kac;
044800150724         tibs69r( TIBS69ds :
044900150724                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
045000150724         if  O69err <> *on;
045100150724           WPDrsu = ACOrag;
045200150724           WPDcmu = CLPage;
045300150724         endif;
045400150724
045500150724
045600150724         // -?Reperimento Commerciale Unificante del Cliente Unificante?
045700150724         k_CMMcod = WPDcmu;
045800150724         chain  %kds( keyAZCMM01 )  AZCMM000;
045900150724         if  %found(AZCMM01L);
046000150724           WPDcmu = CMMuni;
046100150724         endif;
046200150724
046300150724         // -?Decodifica e Reperimento Filiale del Comm.le Unificante?
046400150724         k_CMMcod = WPDcmu;
046500150724         chain  %kds( keyAZCMM01 )  AZCMM000;
046600150724         if  %found(AZCMM01L);
046700150724           WPDcmu = CMMuni;
046800150724           WPDfil = CMMfil;
046900150724         else;
047000150724           clear  CMMdes;
047100150724         endif;
047200150724
047300150724         if  WPDfil = *zero;
047400150724           WPDfil = WPDcmu / 10000;
047500150724         endif;
047600150724
047700150724
047800150724         // -?PARZIALIZZAZIONI:?
047900150724         select;
048000150724           //  ?Commerciale Unificante diverso da quello selezionato?
048100150724           when  SS09cmu <> *zero  and  SS09cmu <> WPDcmu;
048200150724             leavesr;
048300150724           // -?Filiale del commerciale unificante diversa da quella selezionata?
048400150724           when  SS09fil <> *zero  and  SS09fil <> WPDfil;
048500150724             leavesr;
048600150724           // -?Filiale del commerciale unificante non gestibile dall'utente?
048700150724           when  %lookup( %editc( WPDfil : 'X' ) : sk_POg ) = *zero;
048800150724             leavesr;
048900150724         endsl;
049000150724
049100150724
049200150724         // -?Controllo abilitazione dell'utente al commerciale unificante?
049300150724         reset  TNTAA1ds;
049400150724         //ITAA1caut = '§utegtc';   ?(già così)?
049500150724         ITAA1cmm  = WPDcmu;
049600150724         kpjbu     = TNTAA1ds;
049700150724         tntaa1c (kpjba);
049800150724         TNTAA1ds = kpjbu;
049900150724         if  oTAA1fabi = 'N';
050000150724           leavesr;
050100150724         endif;
050200150724
050300150724
050400150724         // -?Reperimento Distretto & Area del Commerciale Unificante?
050500150724         k_ORGfil = WPDfil;
050600150724         chain  %kds( keyAZORG01 )  AZORG;
050700150724         if  %found(AZORG01L)  and  ORGfva = *blank;
050800150724           WPDcar = ORGcar;
050900150724           WPDcdi = ORGfl3;
051000150724         endif;
051100150724
051200150724
051300150724         // -?PARZIALIZZAZIONI:?
051400150724         select;
051500150724           // -?Area del commerciale unificante diversa da quella selezionata?
051600150724           when  SS09car <> *zero  and  SS09car <> WPDcar;
051700150724             leavesr;
051800150724           // -?Area del commerciale unificante non gestibile dall'utente?
051900150724           when  %lookup( %editc( WPDcar : 'X' ) : sk_Car ) = *zero;
052000150724             leavesr;
052100150724         endsl;
052200150724
052300150724
052400150724         // -?Record elaborabile?
052500150724         $Ok = *on;
052600150724
052700150724       ENDSR;
052800150724
052900150724       //--------------------------------------------------------------
053000150724       //?Scrittura del singolo record nel WrkF WFIAE00F.              ?
053100150724       //--------------------------------------------------------------
053200150724       BEGSR  sr_WriteWFIAE;
053300150724
053400150724         // -?Impostazione dati ancora mancanti nel record?
053500150724         WIAEeut   = DUTute;
053600150724         WIAEedt   = %dec( wDate_Iso );
053700150724         WIAEdti   = SB59dsi;
053800150724         WIAEdtf   = SB59dsf;
053900150724
054000150724         //WIAEcdi   = ......;       ?(già valorizzato)?
054100150724         //WIAEcar   = ......;       ?(già valorizzato)?
054200150724         //WIAEfil   = ......;       ?(già valorizzato)?
054300150724         //WIAEcmu   = ......;       ?(già valorizzato)?
054400150724         //WIAEksu   = ......;       ?(già valorizzato)?
054500150724         //WIAErsu   = ......;       ?(già valorizzato)?
054600150724
054700150724         WIAEksc   = PDLksc;
054800150724         WIAErsm   = PDLrsm;
054900150724         //WIAEtmd   = PDLtmd;
055000150724
055100150724         WPDn1p   = PDLn1p;
055200150724         WPDn2p   = PDLn2p;
055300150724         WPDn3p   = PDLn3p;
055400150724         WPDn4p   = PDLn4p;
055500150724         WPDn5p   = PDLn5p;
055600150724         WPDnop   = PDLnop;
055700150724         WPDpo5   = PDLpo5;
055800150724         WPDpdfSp = PDLpdfSp;
055900150724         WPDpdfNr = PDLpdfNr;
056000150724
056100150724         // -?Scrittura nuovo rec. nel WrkF WFPDL00F?
056200150724         write  WFPDL000;
056300150724
056400150724         $Almeno1rec = *on;
056500150724
056600150724       ENDSR;
056700150724
056800150724       //--------------------------------------------------------------
056900150724       //?Preparazione 2ª stringa SQL (per la stampa).                 ?
057000150724       //--------------------------------------------------------------
057100150724       BEGSR  sr_PrepSQL2;
057200150724
057300150724         wSQL2 = 'select * +
057400150724                    from QTEMP/WFPDL00F +
057500150724                   order by WPDcmu, WPDksu, WPDksc +
057600150724                     for fetch only';
057700150724
057800150724       ENDSR;
057900150724
058000150724       //--------------------------------------------------------------
058100150724       //?Apertura cursore 2.                                          ?
058200150724       //--------------------------------------------------------------
058300150724       BEGSR  sr_OpenCursor2;
058400150724
058500150724         // -?Dichiarazione cursore?
058600150724         exec sql   prepare S2   from :wSQL2;
058700150724         exec sql   declare C2   cursor for S2;
058800150724
058900150724         if  SQLcode < *zero;
059000150724           $EoF = *on;
059100150724           wErr = 1;
059200150724           exsr  sr_PrintERR;
059300150724         endif;
059400150724
059500150724         // -?Apertura del cursore?
059600150724         exec sql   open C2;
059700150724
059800150724         if  SQLcode < *zero;
059900150724           $EoF = *on;
060000150724           wErr = 1;
060100150724           exsr  sr_PrintERR;
060200150724         endif;
060300150724
060400150724       ENDSR;
060500150724
060600150724       //--------------------------------------------------------------
060700150724       //?Chiusura cursore 2.                                          ?
060800150724       //--------------------------------------------------------------
060900150724       BEGSR  sr_CloseCursor2;
061000150724
061100150724         // -?Stampa testata per Totali Generali?
061200150724         write  SS09TX1;
061300150724         write  SS09TX4;
061400150724
061500150724         // -?Stampa Totali Generali?
061600150724         write  SS09TOT;
061700150724
061800150724         // -?Chiusura del cursore?
061900150724         exec sql   close C2;
062000150724
062100150724       ENDSR;
062200150724
062300150724       //--------------------------------------------------------------
062400150724       //?Lettura cursore 2.                                           ?
062500150724       //--------------------------------------------------------------
062600150724       BEGSR  sr_ReadCursor2;
062700150724
062800150724         clear  WFPDLds;
062900150724
063000150724         exec sql   fetch next   from C2   into :WFPDLds;
063100150724
063200150724         select;
063300150724           when  SQLcode = 100;
063400150724             $EoF = *on;
063500150724           when  SQLcode < *zero;
063600150724             $EoF = *on;
063700150724             wErr = 1;
063800150724             exsr  sr_PrintERR;
063900150724           other;
064000150724             exsr  sr_PrintWFPDL;
064100150724         endsl;
064200150724
064300150724       ENDSR;
064400150724
064500150724       //--------------------------------------------------------------
064600150724       //?Stampa singolo cliente dal file WFPDL00F.                    ?
064700150724       //--------------------------------------------------------------
064800150724       BEGSR  sr_PrintWFPDL;
064900150724
065000150724         $CmmU = (WPDcmu <> pCMUcod);
065100150724
065200150724         // -?Rottura per Commerciale Unificante?
065300150724         IF  $CmmU;
065400150724
065500150724           *in25 = *on;
065600150724           exsr  sr_DecodCMU;
065700150724
065800150724           // -?Stampa testata?
065900150724           write  SS09TX1;
066000150724           write  SS09TX3;
066100150724           write  SS09TX4;
066200150724           *in25 = *off;
066300150724
066400150724         ENDIF;
066500150724
066600150724
066700150724         // -?Rottura per Cliente Unificante?
066800150724         If  WPDksu <> UPDksu  or  WPDrsu <> UPDrsu;
066900150724           exsr  sr_PrintCliUni;
067000150724         EndIf;
067100150724
067200150724
067300150724         // -?Incremento Totali per Totale Generale?
067400150724         TPDn1p   += WPDn1p;
067500150724         TPDn2p   += WPDn2p;
067600150724         TPDn3p   += WPDn3p;
067700150724         TPDn4p   += WPDn4p;
067800150724         TPDn5p   += WPDn5p;
067900150724         TPDnop   += WPDnop;
068000150724         TPDpo5   += WPDpo5;
068100150724         TPDpdfSp += WPDpdfSp;
068200150724         TPDpdfNr += WPDpdfNr;
068300150724
068400150724
068500150724         // -?Impostazione dati in stampa?
068600150724         //WPDksc   = WPDksc;
068700150724         //WPDrsm   = WPDrsm;
068800150724         //WPDtmd   = WPDtmd;
068900150724         CPDn1p   = WPDn1p;
069000150724         CPDn2p   = WPDn2p;
069100150724         CPDn3p   = WPDn3p;
069200150724         CPDn4p   = WPDn4p;
069300150724         CPDn5p   = WPDn5p;
069400150724         CPDnop   = WPDnop;
069500150724         CPDpo5   = WPDpo5;
069600150724         CPDpdfSp = WPDpdfSp;
069700150724         CPDpdfNr = WPDpdfNr;
069800150724
069900150724
070000150724         // -?Stampa del singolo cliente / tipo modulo?
070100150724         if  *in25;
070200150724           write  SS09TX1;
070300150724           write  SS09TX4;
070400150724           *in25 = *off;
070500150724         endif;
070600150724         write  SS09ksc;
070700150724
070800150724       ENDSR;
070900150724
071000150724       //--------------------------------------------------------------
071100150724       //?Decodifica Commerciale Unificante del commerciale.           ?
071200150724       //--------------------------------------------------------------
071300150724       BEGSR  sr_DecodCMU;
071400150724
071500150724         k_CMMcod = WPDcmu;
071600150724         chain  %kds( keyAZCMM01 )  AZCMM000;
071700150724
071800150724         if  %found(AZCMM01L);
071900150724           WPDcmu = CMMuni;
072000150724         else;
072100150724           clear  CMMdes;
072200150724         endif;
072300150724
072400150724         pCMUcod = WPDcmu;
072500150724         pCMUdes = CMMdes;
072600150724
072700150724       ENDSR;
072800150724
072900150724       //--------------------------------------------------------------
073000150724       //?Calcolo totali e Stampa x Cliente Unificante.                ?
073100150724       //--------------------------------------------------------------
073200150724       BEGSR  sr_PrintCliUni;
073300150724
073400150724         UPDksu    = WPDksu;
073500150724         UPDrsu    = WPDrsu;
073600150724
073700150724
073800150724         wSQL3 = 'select sum(WPDn1p), sum(WPDn2p), sum(WPDn3p), +
073900150724                         sum(WPDn4p), sum(WPDn5p), sum(WPDnop), +
074000150724                         sum(WPDpo5), sum(WPDpdfSp), sum(WPDpdfNr) +
074100150724                    from QTEMP/WFPDL00F +
074200150724                   where WPDksu = ' + %editc( WPDksu : 'X' ) + ' +
074300150724                   group by WPDksu +
074400150724                     for fetch only';
074500150724
074600150724         // -?Dichiarazione cursore?
074700150724         exec sql   prepare S3   from :wSQL3;
074800150724         exec sql   declare C3   cursor for S3;
074900150724
075000150724         // -?Apertura del cursore?
075100150724         exec sql   open C3;
075200150724
075300150724         if  SQLcode < *zero;
075400150724           $EoF = *on;
075500150724           wErr = 1;
075600150724           exsr  sr_PrintERR;
075700150724         endif;
075800150724
075900150724         // -?Lettura del cursore?
076000150724         clear  WTPDLds;
076100150724         exec sql   fetch next   from C3   into :WTPDLds;
076200150724
076300150724         if  SQLcode < *zero;
076400150724           $EoF = *on;
076500150724           wErr = 1;
076600150724           exsr  sr_PrintERR;
076700150724         endif;
076800150724
076900150724         // -?Chiusura del cursore?
077000150724         exec sql   close C3;
077100150724
077200150724
077300150724         // -?Stampa totali per cliente unificante?
077400150724         if  *in25;
077500150724           write  SS09TX1;
077600150724           write  SS09TX4;
077700150724           *in25 = *off;
077800150724         endif;
077900150724         write  SS09ksu;
078000150724
078100150724       ENDSR;
078200150724
078300150724       //--------------------------------------------------------------
078400150724       //?Stampa segnalazione dell'errore rilevato via SQL             ?
078500150724       //--------------------------------------------------------------
078600150724       BEGSR  sr_PrintERR;
078700150724
078800150724         // -?Stampa del Dump?
078900150724         Dump(A);
079000150724
079100150724         // -?Stampa del Job-Log?
079200150724         Qcmd = 'DSPJOBLOG job(*) output(*print)';
079300150724         exsr  sr_ExecCmd;
079400150724
079500150724         // -?Apertura del printer-file?
079600150724         if  Not %open(QSYSPRT);
079700150724           Qcmd = c_OvrPrtF;
079800150724           exsr  sr_ExecCmd;
079900150724           open  QSYSPRT;
080000150724           except  PRTtxt;
080100150724         endif;
080200150724
080300150724         SELECT;
080400150724
080500150724           // -?Stampa dell'errore in COPIA iniziale di WFPDL00F?
080600150724           //  ?da GAITRAAZM in QTEMP (e chiusura del *pgm)?
080700150724           //WHEN  SQlcode >= *zero  and  Not %open(WFPDL00F);
080800150724           WHEN  wErr = 3;
080900150724             if  *inOF;
081000150724               except  PRTtxt;
081100150724               *inOF = *off;
081200150724             endif;
081300150724             except  PRTerr3;
081400150724             *inOF = *off;
081500150724             // -?Chiusura del programma (DOPO)?
081600150724             exsr  sr_RoutEnd;
081700150724
081800150724           // -?Stampa dell'errore SQL (e chiusura del *pgm)?
081900150724           //WHEN  SQlcode <  *zero;
082000150724           WHEN  wErr = 1;
082100150724             clear  ds_Temp;
082200150724             ds_Temp = wSQL1;
082300150724             if  *inOF;
082400150724               except  PRTtxt;
082500150724               *inOF = *off;
082600150724             endif;
082700150724             except  PRTerr1;
082800150724             *inOF = *off;
082900150724             For  xx = 1  To  %elem($Temp);
083000150724               if  $Temp(xx) = *blank;
083100150724                 leave;
083200150724               endif;
083300150724               if  *inOF;
083400150724                 except  PRTtxt;
083500150724                 *inOF = *off;
083600150724               endif;
083700150724               except  PRTerr2;
083800150724             EndFor;
083900150724             // -?Chiusura del programma (DOPO)?
084000150724             exsr  sr_RoutEnd;
084100150724
084200150724           // -?Stampa dell'errore in COPIA finale di WFPDL00F?
084300150724           //  ?da QTEMP in GAITRAAZM (e proseguimento)?
084400150724           //WHEN  SQlcode >= *zero  and  %open(WFPDL00F);
084500150724           WHEN  wErr = 4;
084600150724             if  *inOF;
084700150724               except  PRTtxt;
084800150724               *inOF = *off;
084900150724             endif;
085000150724             except  PRTerr4;
085100150724             *inOF = *off;
085200150724             leavesr;
085300150724
085400150724         ENDSL;
085500150724
085600150724       ENDSR;
085700150724
085800150724       //--------------------------------------------------------------
085900150724       //?Operazioni finali.                                           ?
086000150724       //--------------------------------------------------------------
086100150724       BEGSR  sr_RoutEnd;
086200150724
086300150724         // -?Chiusura applicazioni?
086400150724         cloTabella();
086500150724
086600150724         reset  TIBS69ds;
086700150724         tibs69r( tibs69ds :
086800150724                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
086900150724
087000150724         // -?Chiusura printer-file?
087100150724         if  %open(QSYSPRT);
087200150724           close  QSYSPRT;
087300150724           Qcmd = c_DltOvr;
087400150724           exsr  sr_ExecCmd;
087500150724         endif;
087600150724
087700150724         // -?Uscita dal *pgm?
087800150724         return;
087900150724
088000150724       ENDSR;
088100150724
088200150724       //--------------------------------------------------------------
088300150724       //?Esecuzione del comando (già impostato).                      ?
088400150724       //--------------------------------------------------------------
088500150724       BEGSR  sr_ExecCmd;
088600150724
088700150724         clear Qcap0100;
088800150724         Qcabcsdh = *off;
088900150724         Qcapa    = *off;
089000150724         Qcacmdss = *off;
089100150724         Qcaerved = *allX'00';
089200150724
089300150724         clear Qusec;
089400150724         Qusbprv  = %size(Qusec);
089500150724
089600150724         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
089700150724                           %size(Qcap0100) : 'CPOP0100' : *omit :
089800150724                           0 : 0 : Qusec);
089900150724
090000150724         //if  Qusei <> *blank;
090100150724         //  ...;
090200150724         //endif;
090300150724
090400150724       ENDSR;
090500150724
090600150724      /end-free
090700150724
090800150724       //--------------------------------------------------------------
090900150724       //?S P O O L - F I L E S                                        ?
091000150724       //--------------------------------------------------------------
091100150724
091200150724     oQSYSPRT   e            PRTtxt            1
091300150724     o                       RSUT
091400150724     o                                        +   6 'LISTA ERRORI RILE-
091500150724     o                                              VATI IN FASE DI ST-
091600150724     o                                              AMPA N° PAGINE DI -
091700150724     o                                              PACKING-LIST IN LD-
091800150724     o                                              V'
091900150724     o                       SDSpgmName       +   6
092000150724     o                       wDate         y  +   3
092100150724     o          e            PRTtxt      0
092200150724     o                                          +26 'LISTA ERRORI RILE-
092300150724     o                                              VATI IN FASE DI ST-
092400150724     o                                              AMPA N° PAGINE DI -
092500150724     o                                              PACKING-LIST IN LD-
092600150724     o                                              V'
092700150724     o          e            PRTtxt      1
092800150724     o                       KNSIF
092900150724     o                       KNMUS            +   1
093000150724     o                                        +   5 '------------------
093100150724     o                                              -------------------
093200150724     o                                              -------------------
093300150724     o                                              -------------------
093400150724     o                                              -'
093500150724     o                                        +   6 'Pag.'
093600150724     o                       Page          z  +   0
093700150724     o                       wTime            +   5 '  :  :  '
093800150724      *
093900150724     o          e            PRTerr1     2
094000150724     o                                              'RILEVATO ERRORE: +
094100150724     o                                               SQLCODE'
094200150724     o                       SQLcode       k  +   1
094300150724     o                                        +   1 'SQLSTATE'
094400150724     o                       SQLstate         +   1
094500150724     o                                        +   1 'DURANTE L''ESECUZ-
094600150724     o                                              IONE DEL SEGUENTE -
094700150724     o                                              COMANDO SQL:'
094800150724     o          e            PRTerr1     0  1
094900150724     o                                              'RILEVATO ERRORE: +
095000150724     o                                               SQLCODE'
095100150724     o                       SQLcode       k  +   1
095200150724     o                                        +   1 'SQLSTATE'
095300150724     o                       SQLstate         +   1
095400150724     o          e            PRTerr2     1
095500150724     o                       $Temp(xx)        +   3
095600150724      *
095700150724     o          e            PRTerr3     2
095800150724     o                                              'NON RIUSCITA LA +
095900150724     o                                               COPIA DEL WORK-+
096000150724     o                                               FILE WFPDL00F +
096100150724     o                                               NELLA LIBR. QTEMP'
096200150724      *
096300150724     o          e            PRTerr4     2
096400150724     o                                              'NON RIUSCITA LA +
096500150724     o                                               COPIA DEL WORK-+
096600150724     o                                               FILE WFPDL00F +
096700150724     o                                               NELLA LIBR.'
096800150724     o                       wLibrAZx         +   1
096900150724
097000150724       //--------------------------------------------------------------
097100150724       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
097200150724       //--------------------------------------------------------------
097300150724
097400150724** -?$iSystem / $Libraries:?Sistema AS/400 & Libreria.?
097500150724SETRAS  GAITRAAZM
097600150724AS888   GAITRAAZP
