000100100304       //==============================================================
000200100204       //?TNTB79R1 * Selezione tab. "3EW" = Dati EasyWeb x "postazione"?
000300100304       //==============================================================
000400100304
000500100304       //--------------------------------------------------------------
000600100304       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100304       //--------------------------------------------------------------
000800100304
000900100330     /*PRM dbgview(*source)
001000100304     /*END
001100100304
001200100304       //--------------------------------------------------------------
001300100304       //?Specifiche di controllo.                                     ?
001400100304       //--------------------------------------------------------------
001500100204
001600100204     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100204     h dftactgrp(*no) actgrp(*caller)
001800140418     h alwnull(*inputonly)
001900100204
002000100204       //--------------------------------------------------------------
002100100204       //?Dichiarazione file.                                          ?
002200100204       //--------------------------------------------------------------
002300100204
002400100204       // -?Tabelle?
002500140418     fTNTBE00F  if   e             disk
002600140418     f                                     extfile(wLibTNTBE)  usropn
002700100204
002800100204       // -?Video?
002900100204     fTNTB79D1  cf   e             workstn
003000100204     f                                     sfile(TB79S01 : S01nrr)
003100100204     f                                     indds(IndDspF)
003200100204     f                                     infds(InfDspF)
003300100204
003400100204       //--------------------------------------------------------------
003500100204       //?Definizione costanti.                                        ?
003600100204       //--------------------------------------------------------------
003700100204
003800100204       // -?Codice tabella in gestione?
003900100204     d c_Tab           c                   const('3EW')
004000100204
004100100204       // -?Numero di record in ciascuna videata di subfile?
004200100204     d c_SflPag        c                   const(15)
004300100204
004400100204       // -?Tasti funzionali a video?
004500100204     d c_F01           c                   const(x'31')
004600100204     d c_F02           c                   const(x'32')
004700100204     d c_F03           c                   const(x'33')
004800100204     d c_F04           c                   const(x'34')
004900100204     d c_F05           c                   const(x'35')
005000100204     d c_F06           c                   const(x'36')
005100100204     d c_F07           c                   const(x'37')
005200100204     d c_F08           c                   const(x'38')
005300100204     d c_F09           c                   const(x'39')
005400100204     d c_F10           c                   const(x'3A')
005500100204     d c_F11           c                   const(x'3B')
005600100204     d c_F12           c                   const(x'3C')
005700100204     d c_F13           c                   const(x'B1')
005800100204     d c_F14           c                   const(x'B2')
005900100204     d c_F15           c                   const(x'B3')
006000100204     d c_F16           c                   const(x'B4')
006100100204     d c_F17           c                   const(x'B5')
006200100204     d c_F18           c                   const(x'B6')
006300100204     d c_F19           c                   const(x'B7')
006400100204     d c_F20           c                   const(x'B8')
006500100204     d c_F21           c                   const(x'B9')
006600100204     d c_F22           c                   const(x'BA')
006700100204     d c_F23           c                   const(x'BB')
006800100204     d c_F24           c                   const(x'BC')
006900100204     d c_Enter         c                   const(x'F1')
007000100204     d c_RollDown      c                   const(x'F4')
007100100204     d c_RollUp        c                   const(x'F5')
007200100204
007300100204       //--------------------------------------------------------------
007400100204       //?Definizione schiere.                                         ?
007500100204       //--------------------------------------------------------------
007600100204
007700100204       // -?Messaggi di errore?
007800100204     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
007900140418
008000140418       // -?RRN di TNTBE con cui si scriveranno i rec. della pag.?
008100140418     d $FetchRow       ds                  inz  occurs(c_SflPag)
008200140418     d   wTBErrn                     10i 0 inz
008300140418
008400140418       // -?RRN dei record di TNTBE che compongono la pagina?
008500140418     d dsRowPag        ds                  inz
008600140418     d   $Row                              like(wTBErrn)  dim(c_SflPag)
008700140418
008800140418     d $Pag            s                   like(dsRowPag)
008900140418     d                                     dim(32767)  based(pPag)
009000100204
009100100204       //--------------------------------------------------------------
009200100204       //?Definizione aree dati.                                       ?
009300100204       //--------------------------------------------------------------
009400100204
009500100204       // -?Dati utente?
009600100204     d §AzUte        e ds                  extname(AZUTE00F)
009700100204     d                                     dtaara
009800100204     d §DatiUte      e ds                  extname(dDatiUte)
009900100204     d                                     dtaara
010000100204
010100100204       //--------------------------------------------------------------
010200100204       //?Definizione strutture dati.                                  ?
010300100204       //--------------------------------------------------------------
010400100204
010500100204       // -?Status ds?
010600100204     d Status         sds
010700100204     d   SDSpgm          *proc
010800100204     d*//SDSprm          *parms
010900100204
011000100204       // -?InfDS?
011100100204     d InfDspF         ds
011200100204     d   dsp_aid             369    369a
011300100204     d   sfl_rrn             376    377i 0
011400100204     d   min_nrr             378    379i 0
011500100204     d   num_rcds            380    381i 0
011600100204
011700100204       // -?Indicatori su DspF?
011800100204     d IndDspF         ds                  inz
011900100204         // -?Abilitazione tasti funzionali?
012000100204     d   F3Attivo                      n   overlay(IndDspF : 03)
012100140418     d   F8Attivo                      n   overlay(IndDspF : 08)
012200100204     d   F12Attivo                     n   overlay(IndDspF : 12)
012300100204         // -?Emissione messaggio di errore?
012400100204     d   ErrMessage                    n   overlay(IndDspF : 28)
012500100204         // -?Indicatori di gestione del subfile?
012600100204     d   SflDsp_N                      n   overlay(IndDspF : 30)
012700100204     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012800100204     d   SflNxtChg                     n   overlay(IndDspF : 32)
012900100204     d   SflEnd                        n   overlay(IndDspF : 33)
013000140418     d   SflStart                      n   overlay(IndDspF : 34)
013100100204         // -?Indicatori per Attibuti di visualizzazione?
013200100204         //  ?o   Condizionamento visualizzazione campi?
013300100312     d   No_Posiz                      n   overlay(IndDspF : 40)
013400140418     d   PosizKSU                      n   overlay(IndDspF : 41)
013500140418     d   PosizRAG                      n   overlay(IndDspF : 42)
013600140418     d   PosizSUN                      n   overlay(IndDspF : 43)
013700140418     d   PosizAEW                      n   overlay(IndDspF : 44)
013800140418     d   PosizLNP                      n   overlay(IndDspF : 45)
013900140418     d   PosizFSR                      n   overlay(IndDspF : 46)
014000140418     d   AnnullApplic                  n   overlay(IndDspF : 49)
014100100204         // -?Posizionamento cursore & segnalazione errore?
014200100204     d   PosCurOPZ                     n   overlay(IndDspF : 50)
014300140418         // -?Errore di paginazione?
014400140418     d   ErrRoll                       n   overlay(IndDspF : 98)
014500140418         // -?Riemissione videata?
014600100204     d   ErrGenerico                   n   overlay(IndDspF : 99)
014700100204
014800100204       // -?Parametri ricevuti?
014900100204     d KPJBA         e ds
015000100204     d TNTB79ds      e ds                  inz
015100100204
015200100204       // -?Tab. 3EW = Dati assegnati alla "postazione" in abilitazione?
015300100204       //             ?ad EasyWeb?
015400100204     d d3EW          e ds                  inz
015500100204
015600100204       //--------------------------------------------------------------
015700100204       //?Definizione variabili globali.                               ?
015800100204       //--------------------------------------------------------------
015900100204
016000100204       // -?Flags booleani?
016100100204     d $Fine           s               n   inz
016200100205     d $EoF            s               n   inz
016300140418     d $Err            s               n   inz
016400100204     d $InzS01         s               n   inz(*on)
016500100204     d $RecOK          s               n   inz
016600100204
016700100204       // -?Parametri di input facoltativi?
016800100204     d p_TNTB79ds      s                   like(TNTB79ds)
016900100204
017000100204       // -?Variabili per la gestione del video?
017100100204     d $Video          s              2    inz('S1')
017200140418     d w_CurrPag       s              5  0 inz
017300140418     d w_LastPag       s              5  0 inz
017400140422     d w_FetchPag      s              5  0 inz
017500100204     d wPag            s              4  0 inz
017600100204     d wRig            s              3  0 inz
017700100204     d S01nrr          s                   like(C1RcdNbr) inz
017800100204     d SavS01csr       s                   like(C1RcdNbr) inz
017900140418     d SaveKSU         s                   inz  like(C1Cksu)
018000140418     d SaveRAG         s                   inz  like(C1Dksu)
018100140418     d SaveSUN         s                   inz  like(C1Csun)
018200140418     d SaveAEW         s                   inz  like(C1Caew)
018300140418     d SaveLNP         s                   inz  like(C1Clnp)
018400140418     d SaveFLS         s                   inz  like(C1Cfls)
018500140418     d SaveNRS         s                   inz  like(C1Cnrs)
018600140418     d wOrdSfl         s              1  0 inz
018700140418
018800140418       // -?Variabili per la gestione del subfile con oltre 9999 righe?
018900140418     d xR              s                   inz  like(S01nrr)
019000140418     d nSize           s             10i 0 inz
019100140418     d pPag            s               *   inz
019200140418
019300140418       // -?Stringa SQL da eseguire?
019400140418     d wSQL            s           2048    inz  Varying
019500140418
019600140418       // -?Nome esteso Libreria/File del file tabelle?
019700140418     d wLibTABsede     s             10    inz
019800140418     d wLibTNTBE       s             21a   inz
019900100204
020000100204       //--------------------------------------------------------------
020100100204       //?Definizione prototipi procedure.                             ?
020200100204       //--------------------------------------------------------------
020300100204
020400100204       // -?Reperimento dati utente?
020500140418     d TIBS34ds      e ds                  inz
020600100204      /copy gaitrasrc/srcProtoPR,TIBS34R
020700100204
020800100204       // -?Controllo/Decodifica cliente?
020900100217      /copy gaitrasrc/srcProtoPI,TIBS69R
021000100204      /copy gaitrasrc/srcProtoPR,TIBS69R
021100140418
021200140418       // -?Parametri API QCAPCMD (Process Commands)?
021300140418     d Qcmd            s           2048    inz  varying
021400140418      /copy qSysInc/qRpgleSrc,QCAPCMD
021500140418       // -?API QCAPCMD (Process Commands)?
021600140418      /copy gaitrasrc/srcProtoPR,QCAPCMD
021700140418
021800140418       // -?Parametri gestione errori API.?
021900140418      /copy qsysinc/qRpgleSrc,QUSEC
022000100204
022100100204       //--------------------------------------------------------------
022200100204       //?Definizione key-list.                                        ?
022300100204       //--------------------------------------------------------------
022400100204
022500100204
022600100204       //--------------------------------------------------------------
022700100204       //?Riepilogo indicatori utilizzati.                             ?
022800100204       //--------------------------------------------------------------
022900100204
023000100204
023100100204       //--------------------------------------------------------------
023200100204       //?M A I N - L I N E                                            ?
023300100204       //--------------------------------------------------------------
023400100204
023500100204     c     *Entry        plist
023600100204     c                   parm                    KPJBA
023700100204     c                   parm                    p_TNTB79ds
023800100204
023900100204      /free
024000100204
024100100204       // -?Operazioni iniziali?
024200100204       exsr sr_RoutInz;
024300100204
024400100204       // -?Ciclo di gestione del file video?
024500100204       DoW  $Fine = *off;
024600140418
024700100204         select;
024800100204           // -?Gestione videata S1 (subfile)?
024900100204           when $Video = 'S1';
025000100204             exsr sr_GesS01;
025100100204           other;
025200100204             $Fine = *on;
025300100204         endsl;
025400140418
025500140418       EndDo;
025600100204
025700100204       // -?Operazioni finali?
025800100204       exsr sr_RoutEnd;
025900100204
026000100204       //--------------------------------------------------------------
026100100204       //?*InzSR.                                                      ?
026200100204       //--------------------------------------------------------------
026300100204       BEGSR  *InzSr;
026400140418
026500140418         // -?Impostazione opzioni per SQL?
026600140418         exec sql   set  option  DynUsrPrf = *Owner,
026700140418                                 CloSqlCsr = *EndMod;
026800100204
026900100204         // -?Reperimento dati job?
027000100204         exsr  sr_DatiJob;
027100100204
027200100204         // -?Impostazione nome programma a video?
027300100204         V1Tpgm = SDSpgm;
027400100204
027500100204       ENDSR;
027600100204
027700100204       //--------------------------------------------------------------
027800100204       //?Reperimento Dati del job (Utente/Operativi).                 ?
027900100204       //--------------------------------------------------------------
028000100204       BEGSR  sr_DatiJob;
028100100204
028200100204         in(e) §AzUte;
028300100204         if NOT %error;
028400100204           in(e) §DatiUte;
028500100204         endif;
028600100204         if %error or RSut = *blank;
028700100204           tibs34r ( tibs34ds );
028800100204           in §AzUte;
028900100204           in §DatiUte;
029000100204         endif;
029100100204
029200100204       ENDSR;
029300100204
029400100204       //--------------------------------------------------------------
029500100204       //?Operazioni iniziali.                                         ?
029600100204       //--------------------------------------------------------------
029700100204       BEGSR  sr_RoutInz;
029800100204
029900100204         // -?Verifica se ricevuti parametri?
030000100204         if  %parms() > 1;
030100100204           TNTB79ds = p_TNTB79ds;
030200100204         else;
030300100204           clear  TNTB79ds;
030400100204         endif;
030500100204
030600100204         // -?Impostazione iniziale parametri di output?
030700100211         clear  o3EWksu;
030800100211         clear  o3EWsun;
030900100204         clear  o3EWuni;
031000100204         clear  o3EWfxx;
031100100204         o3EWerr = *off;
031200100204         clear  o3EWmsg;
031300100204
031400100204         // -?Impostazione chiusura?
031500100204         select;
031600100204           when  %parms() > 1   and   i3EWtla = *blank;
031700100204             *inRT = *on;
031800100204           when  %parms() > 1   and   i3EWtla = 'L';
031900100204             *inLR = *on;
032000100204           when  %parms() > 1   and   i3EWtla = 'C';
032100100204             *inLR = *on;
032200100204             $Fine = *on;
032300100204             leavesr;
032400100204           other;
032500100204             *inLR = *on;
032600100204         endsl;
032700140418
032800140418         // -?Apertura file TNTBE00F del S.I. di sede?
032900140418         if  Not  %open(TNTBE00F);
033000140418           if  %subst( knsif : 7 : 1 ) = 'P';
033100140418             wLibTABsede = 'GAITRAGRPS';
033200140418           else;
033300140418             wLibTABsede = 'GAITRAGRU';
033400140418           endif;
033500140418           wLibTNTBE = %trimr(wLibTABsede) + '/TNTBE00F';
033600140418           open  TNTBE00F;
033700140418         endif;
033800100204
033900100204         // -?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
034000100204         reset  $Fine;
034100140418         reset  $EoF;
034200140418         reset  $Err;
034300140418         reset  $InzS01;
034400100204         reset  $Video;
034500100204
034600100204       ENDSR;
034700140418
034800140418       //--------------------------------------------------------------
034900140418       //?Preparazione stringa SQL.                                    ?
035000140418       //--------------------------------------------------------------
035100140418       BEGSR  sr_PrepSQL;
035200140418
035300140418         wSQL =   'with +
035400140418                  TNTBE as ( select rrn(TNTBE00F) as Nrr, +
035500140418                                    TBEke1, TBEke2, TBEuni +
035600140418                               from ' + %trimr(wLibTABsede) + '/TNTBE00F +
035700140422                              where TBEcod = ''3EW'' +
035800140422                                and TBEatb = '' ''';
035900140418
036000140418         // -?Ricevuti parametri dal chiamante?
036100140418         select;
036200140418           when  %parms() > 1  and  i3EWksu <> *blank
036300140418                               and  i3EWsun =  *blank;
036400140418             wSQL += ' and substr(TBEke1, 1, ' +
036500140418                           %trim( %editc( %size(i3EWksu) : '3' ) ) +
036600140418                             ') = ''' + i3EWksu + '''';
036700140418           when  %parms() > 1  and  i3EWksu =  *blank
036800140418                               and  i3EWsun <> *blank;
036900140418             wSQL += ' and substr(TBEke2, 1, ' +
037000140418                           %trim( %editc( %size(i3EWsun) : '3' ) ) +
037100140418                             ') = ''' + i3EWsun + '''';
037200140418           when  %parms() > 1  and  i3EWksu <> *blank
037300140418                               and  i3EWsun <> *blank;
037400140418             wSQL += ' and substr(TBEke1, 1, ' +
037500140418                           %trim( %editc( %size(i3EWksu) : '3' ) ) +
037600140418                           ') = ''' + i3EWksu + ''' +
037700140418                       and substr(TBEke2, 1, ' +
037800140418                           %trim( %editc( %size(i3EWsun) : '3' ) ) +
037900140418                           ') = ''' + i3EWsun + '''';
038000140418         endsl;
038100140422         wSQL += ' )';
038200140418
038300140422         If  wOrdSfl = 1;
038400140422           wSQL += ', +
038500140418                  CNACO as ( select ACOksc, ACOrag +
038600140418                               from CNACO00F +
038700140418                              where ACOkut = 1 and ACOkcc = ' +
038800140422                                    %trim( %editc( DUTkci : '3' ) ) + ' ) +
038900140422
039000140418                 select TNTBE.Nrr +
039100140418                   from TNTBE left outer join CNACO +
039200140418                     on substr(TNTBE.TBEke1, 2, 7) = digits(CNACO.ACOksc)';
039300140422         Else;
039400140422           wSQL += ' +
039500140422                 select TNTBE.Nrr +
039600140422                   from TNTBE';
039700140422         EndIf;
039800140418
039900140418         // -?Posizionamento / Ordinamento?
040000140418         Select;
040100140418
040200140418           // -?Per Codice Unificante?
040300140418           When  wOrdSfl = 0;
040400140418             if  SaveKSU <> *blank;
040500140418               wSQL += ' where TNTBE.TBEke1 >= ''' +
040600140418                       SaveKSU + '''';
040700140418             endif;
040800140418             wSQL += ' order by TNTBE.TBEke1, TNTBE.TBEke2';
040900140418
041000140418           // -?Per Ragione Sociale unificante?
041100140418           When  wOrdSfl = 1;
041200140418             if  SaveRAG <> *blank;
041300140418               wSQL += ' where substr(CNACO.ACOrag, 1, ' +
041400140418                       %trim( %editc( %size( C1Dksu ) : '3' ) ) + ') >= ''' +
041500140418                       SaveRAG + '''';
041600140418             endif;
041700140418             wSQL += ' order by CNACO.ACOrag, TNTBE.TBEke1, TNTBE.TBEke2';
041800140418
041900140418           // -?Per Strategi User Number?
042000140418           When  wOrdSfl = 2;
042100140418             if  SaveSUN <> *blank;
042200140418               wSQL += ' where TNTBE.TBEke2 >= ''' +
042300140418                       SaveSUN + '''';
042400140418             endif;
042500140418             wSQL += ' order by TNTBE.TBEke2, TNTBE.TBEke1';
042600140418
042700140418           // -?Per Abilitazione a creare utenti "ESWEB" bollettisti?
042800140418           When  wOrdSfl = 3;
042900140418             if  SaveAEW <> *blank;
043000140418               wSQL += ' where substr(TNTBE.TBEuni, 31, 1) >= ''' +
043100140418                       SaveAEW + '''';
043200140418             endif;
043300140418             wSQL += ' order by substr(TNTBE.TBEuni, 31, 1), +
043400140418                                TNTBE.TBEke1, TNTBE.TBEke2';
043500140418
043600140418           // -?Per Linea di Partenza?
043700140418           When  wOrdSfl = 4;
043800140418             if  SaveLNP <> *zero;
043900140418               wSQL += ' where substr(TNTBE.TBEuni, 1, 3) >= ''' +
044000140418                       %editc( SaveLNP : 'X' ) + '''';
044100140418             endif;
044200140418             wSQL += ' order by substr(TNTBE.TBEuni, 1, 3), +
044300140418                                TNTBE.TBEke1, TNTBE.TBEke2';
044400140418
044500140418           // -?Per supporto Filiale/Serie Segnacollo?
044600140418           When  wOrdSfl = 5;
044700140418             if  SaveFLS <> *zero  or  SaveNRS <> *zero;
044800140418               wSQL += ' where substr(TNTBE.TBEuni, 4, 3) >= ''' +
044900140418                               %editc( SaveFLS : 'X' ) + ''' +
045000140418                           and substr(TNTBE.TBEuni, 7, 2) >= ''' +
045100140418                               %editc( SaveNRS : 'X' ) + '''';
045200140418             endif;
045300140418             wSQL += ' order by substr(TNTBE.TBEuni, 4, 19), +
045400140418                                TNTBE.TBEke1, TNTBE.TBEke2';
045500140418
045600140418         EndSl;
045700140418
045800140418         wSQL += ' for fetch only +
045900140418                 optimize for ' + %editc( c_SflPag : '3' ) + ' rows';
046000140418
046100140418       ENDSR;
046200140418
046300140418       //--------------------------------------------------------------
046400140418       //?Apertura cursore C1.                                         ?
046500140418       //--------------------------------------------------------------
046600140418       BEGSR  sr_OpenCursor;
046700140418
046800140418         // -?Dichiarazione cursore?
046900140418         exec sql   prepare S1   from :wSQL;
047000140418         exec sql   declare C1   cursor for S1;
047100140418
047200140418         // -?Apertura del cursore?
047300140418         exec sql   open C1;
047400140418
047500140418         if  SQLcode < *zero;
047600140418           $Err = *on;
047700140418           $EoF = *on;
047800140418           exsr  sr_PrintErrSQL;
047900140418         endif;
048000140418
048100140418       ENDSR;
048200140418
048300140418       //--------------------------------------------------------------
048400140418       //?Chiusura cursore C1.                                         ?
048500140418       //--------------------------------------------------------------
048600140418       BEGSR  sr_CloseCursor;
048700140418
048800140418         // -?Chiusura del cursore?
048900140418         exec sql   close C1;
049000140418
049100140418       ENDSR;
049200140418
049300140418       //--------------------------------------------------------------
049400140418       //?Lettura cursore C1.                                          ?
049500140418       //--------------------------------------------------------------
049600140418       BEGSR  sr_ReadCursor;
049700140418
049800140418         clear  $FetchRow;
049900140422         w_FetchPag += 1;
050000140418
050100140422         // -?Reperimento dei 15 (c_SflPag) Rrn che comporranno la pag.?
050200140418         exec sql   fetch next   from C1   for 15 rows
050300140418                    into  :$FetchRow;
050400140418
050500140418         select;
050600140418           when  SQLcode = 100;
050700140418             $EoF = *on;
050800140418           when  SQLcode < *zero;
050900140418             $Err = *on;
051000140418             $EoF = *on;
051100140418             exsr  sr_PrintErrSQL;
051200140418         endsl;
051300140418
051400140418       ENDSR;
051500100204
051600100204       //--------------------------------------------------------------
051700140418       //?Gestione subfile S01 - Subfile con elenco tab. "3EW".        ?
051800100204       //--------------------------------------------------------------
051900100204       BEGSR  sr_GesS01;
052000100204
052100100204         // -?Inizializzazione videata?
052200140418         if  $InzS01  and  Not ErrGenerico;
052300100204           exsr  sr_InzS01;
052400100204           $InzS01 = *off;
052500100204         endif;
052600100204
052700140418         // -?Emissione Testata & Piede con tasti funzionali abilitati?
052800100204         write  TB79T01;
052900100204         write  TB79P01;
053000100204
053100100204         // -?Posizionamento cursore?
053200100204         if  C1CsrRrn > *zero;
053300100204           C1RcdNbr = C1CsrRrn;
053400100204         else;
053500100204           C1RcdNbr = 1;
053600100204           write  TB79S00;             //?(no rec.)?
053700100204         endif;
053800100204
053900100204         // -?Emissione videata?
054000100204         exfmt  TB79C01;
054100100204
054200100204         reset  ErrMessage;
054300100204         reset  ErrGenerico;
054400100204         clear  V1Dmsg;
054500100204
054600100211         clear  o3EWksu;
054700100211         clear  o3EWsun;
054800100204         clear  o3EWuni;
054900100204
055000140418         Select;
055100100204
055200100204           // -?F3=Fine?
055300140418           When  dsp_aid = c_F03;
055400140418             exsr  sr_CloseCursor;
055500100204             o3EWfxx = '1';
055600100204             $Fine = *on;
055700100204
055800100204           // -?F5=Rivisualizza?
055900140418           When  dsp_aid = c_F05;
056000140418             exsr  sr_CloseCursor;
056100100204             $InzS01 = *on;
056200100331
056300121008           // -?F8=Ordinamento?
056400140418           When  dsp_aid = c_F08;
056500121008             exsr  sr_F08S01;
056600100204
056700100204           // -?F12=Ritorno?
056800140418           When  dsp_aid = c_F12;
056900140418             exsr  sr_CloseCursor;
057000100204             o3EWfxx = '2';
057100100204             $Fine = *on;
057200140418
057300140418           // -?Roll-Up?
057400140418           When  dsp_aid = c_RollUp;
057500140418             exsr sr_RollUpS01;
057600140418
057700140418           // -?Roll-Down?
057800140418           When  dsp_aid = c_RollDown;
057900140418             exsr sr_RollDownS01;
058000100204
058100100204           // -?SubFile vuoto?
058200140418           When  S01nrr = *zero;
058300100204
058400100204           // -?Invio?
058500140418           Other;
058600100204             // -?Gestione opzioni?
058700100331             exsr  sr_OpzS01;
058800100331             if  ErrGenerico;
058900100331               leavesr;
059000100331             endif;
059100100204             // -?Richiesto posizionamento?
059200140418             if  C1Cksu <> SaveKSU  or
059300140418                 C1Dksu <> SaveRAG  or
059400140418                 C1Csun <> SaveSUN  or
059500140418                 C1Caew <> SaveAEW  or
059600140418                 C1Clnp <> SaveLNP  or
059700140418                (C1Cfls <> SaveFLS  or  C1Cnrs <> SaveNRS);
059800140418               exsr  sr_CloseCursor;
059900100204               $InzS01 = *on;
060000140418               SaveKSU = C1Cksu;
060100140418               SaveRAG = C1Dksu;
060200140418               SaveSUN = C1Csun;
060300140418               SaveAEW = C1Caew;
060400140418               SaveLNP = C1Clnp;
060500140418               SaveFLS = C1Cfls;
060600140418               SaveNRS = C1Cnrs;
060700100204             endif;
060800100204
060900140418         EndSl;
061000100204
061100100204       ENDSR;
061200100204
061300100204       //--------------------------------------------------------------
061400100204       //?Inizializzazione subfile S01.                                ?
061500100204       //--------------------------------------------------------------
061600100204       BEGSR  sr_InzS01;
061700140418
061800140418         // -?La pulizia del subfile verrà eseguita nella subr.?
061900140418         //  ?"sr_WrtS01", eseguita nella subr. "sr_RollUpS01"?
062000140418
062100140418         // -?Pulizia subfile-control?
062200140418         clear  TB79C01;
062300140418
062400140418         C1Cksu  = SaveKSU;
062500140418         C1Dksu  = SaveRAG;
062600140418         C1Csun  = SaveSUN;
062700140418         C1Caew  = SaveAEW;
062800140418         C1Clnp  = SaveLNP;
062900140418         C1Cfls  = SaveFLS;
063000140418         C1Cnrs  = SaveNRS;
063100140418
063200140418         $EoF = *off;
063300140418         $Err = *off;
063400140418         clear  w_CurrPag;
063500140418         clear  w_LastPag;
063600140422         clear  w_FetchPag;
063700140418
063800140418         // -?Preparazione stringa SQL?
063900140418         exsr  sr_PrepSQL;
064000140418
064100140418         // -?Caricamento della 1ª pagina di dati del subfile?
064200140418         exsr  sr_OpenCursor;
064300140422         exsr  sr_ReadCursor;
064400140418         exsr  sr_RollUpS01;
064500100204
064600100204         // -?Impaginazione subfile?
064700100204         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
064800100204         if S01nrr > *zero;
064900100204           C1RcdNbr  = 1;
065000100204           C1CsrRrn  = 1;
065100100204         else;
065200100204           clear C1RcdNbr;
065300100204         endif;
065400140418
065500140418         // -?Impostazione indicatore x la gestione del posizionamento?
065600140418         //  ?nel subfile?
065700140418         No_Posiz = (%parms() > 1   and
065800140418                    (i3EWksu <> *blank  or  i3EWsun <> *blank));
065900140418
066000140418         // -?Settaggio indicatori dei tasti funzionali?
066100140418         exsr  sr_SetIndP01;
066200140418
066300140418         // -?Visualizzazione richiesta di posizionamento?
066400140418         //  ?& Impostazione F8 nel piede del subfile?
066500140418         PosizKSU = (S01nrr > *zero  and  wOrdSfl = 0);
066600140418         PosizRAG = (S01nrr > *zero  and  wOrdSfl = 1);
066700140418         PosizSUN = (S01nrr > *zero  and  wOrdSfl = 2);
066800140418         PosizAEW = (S01nrr > *zero  and  wOrdSfl = 3);
066900140418         PosizLNP = (S01nrr > *zero  and  wOrdSfl = 4);
067000140418         PosizFSR = (S01nrr > *zero  and  wOrdSfl = 5);
067100140418         select;
067200140418           // -?SubFile vuoto...?
067300140418           when  S01nrr = *zero;
067400140418             clear  V1PF08;
067500140418           // -?Già ordinato per Codice Cliente Unificante?
067600140418           when  wOrdSfl = 0;
067700140418             V1PF08   = 'F8=Ord. x Ragione Sociale';
067800140418           // -?Già ordinato per Ragione Sociale Unificante?
067900140418           when  wOrdSfl = 1;
068000140418             V1PF08   = 'F8=Ord. x StrategiUserNbr';
068100140418           // -?Già ordinato per Strategi User Number?
068200140418           when  wOrdSfl = 2;
068300140418             V1PF08   = 'F8=Ord. x Abil.Cr.Ut.Bol.';
068400140418           // -?Già ordinato per Abilitaz. a Creare Utenti EW Bollettisti?
068500140418           when  wOrdSfl = 3;
068600140418             V1PF08   = 'F8=Ord. x Linea Partenza ';
068700140418           // -?Già ordinato per Linea di Partenza?
068800140418           when  wOrdSfl = 4;
068900140418             V1PF08   = 'F8=Ord. x Fil/Serie/SgnCl';
069000140418           // -?Già ordinato per Filiale/Serie/Range Segnacolli?
069100140418           when  wOrdSfl = 5;
069200140418             V1PF08   = 'F8=Ord. x Codice Cliente ';
069300140418         endsl;
069400100204
069500100204       ENDSR;
069600140418
069700140418       //--------------------------------------------------------------
069800140418       //?Pagina del SubFile in avanti.                                ?
069900140418       //--------------------------------------------------------------
070000140418       BEGSR  sr_RollUpS01;
070100140418
070200140418         if  w_CurrPag = w_LastPag;
070300140418           w_CurrPag += 1;
070400140418           exsr  sr_NuovaPagina;
070500140418         else;
070600140418           w_CurrPag += 1;
070700140418         endif;
070800140418
070900140418         exsr  sr_WrtS01;
071000140418
071100140418       ENDSR;
071200140418
071300140418       //--------------------------------------------------------------
071400140418       //?Pagina del SubFile indietro.                                 ?
071500140418       //--------------------------------------------------------------
071600140418       BEGSR  sr_RollDownS01;
071700140418
071800140418         if  w_CurrPag > 1;
071900140418           w_CurrPag -= 1;
072000140418           exsr  sr_WrtS01;
072100140418         else;
072200140418           ErrRoll = *on;
072300140418         endif;
072400140418
072500140418
072600140418       ENDSR;
072700140418
072800140418       //--------------------------------------------------------------
072900140418       //?Caricamento singola pagina in memoria.                       ?
073000140418       //--------------------------------------------------------------
073100140418       BEGSR  sr_NuovaPagina;
073200140418
073300140418         // -?Memorizzazione n° dell'ultima pagina già letta?
073400140422         w_LastPag = w_CurrPag;
073500140418
073600140418         // -?Memorizzazione, nella schiera $Row, dei RRN del file?
073700140418         //  ?con cui verranno scritte le righe della pagina?
073800140418         clear  $Row;
073900140418         For  xR = 1  To  SqlEr3;
074000140418           %occur( $FetchRow ) = xR;
074100140418           $Row(xR) = wTBErrn;
074200140418         EndFor;
074300140418
074400140418         // -?Allocazione della memoria necessaria per memorizzare?
074500140418         //  ?il nuovo elemento?
074600140418         nSize = %size($Pag) * w_CurrPag;
074700140418         if  w_CurrPag = 1;
074800140418           pPag = %alloc( nSize );
074900140418         else;
075000140418           pPag = %realloc( pPag : nSize );
075100140418         endif;
075200140418
075300140418         // -?Memorizzazione della pagina?
075400140418         $Pag(w_CurrPag) = dsRowPag;
075500140418
075600140418         // -?Fermo il caricamento di nuove pagine:?
075700140418         //  ?se i records sono finiti, oppure?
075800140418         //  ?se ho raggiunto la capienza massima?
075900140418         if  SqlEr5 = 100  or  w_CurrPag = %elem($Pag);
076000140422           $EoF = *on;
076100140418           leavesr;
076200140418         endif;
076300140418
076400140418       ENDSR;
076500140418
076600140418       //--------------------------------------------------------------
076700140418       //?Caricamento singola pagina nel subfile S01.                  ?
076800140418       //--------------------------------------------------------------
076900140418       BEGSR  sr_WrtS01;
077000140418
077100140418         // -?Spegnimento degli indicatori di errore?
077200140418         %subst(IndDspF : 50) = *off;
077300140418
077400140418         // -?Pulizia subfile?
077500140418         SflDsp_N    = *on;
077600140418         SflDspCtl_N = *on;
077700140418         write  TB79C01;
077800140418         SflDspCtl_N = *off;
077900140418         SflEnd      = *off;
078000140418         SflStart    = *off;
078100140418
078200140418         clear  C1RcdNbr;
078300140418         clear  C1CsrRrn;
078400140418         clear  S01nrr;
078500140418         clear  V1Dmsg;
078600140418         ErrMessage  = *off;
078700140418         ErrGenerico = *off;
078800140418
078900140418         SflNxtChg = *off;
079000140418
079100140418         // -?Reperimento dei Rrn che compongono la pagina?
079200140418         dsRowPag = $Pag(w_CurrPag);
079300140418
079400140418         // -?Ciclo di caricamento dati nel subfile (singola pagina)?
079500140418         xR = 1;
079600140418         DoW  xR      <= c_SflPag  and
079700140418              $Row(xR) > *zero;
079800140418
079900140418           clear  TB79S01;
080000140418
080100140418           // -?Caricamento del singolo record nel subfile S01?
080200140418           chain  $Row(xR)  TNTBE000;
080300140418
080400140418           if  %found(TNTBE00F);
080500140418
080600140418             d3EW   = TBEuni;
080700140418             clear  TB79S01;
080800140418             // - -?Campi di Output?
080900140418             S1Cksu = TBEke1;
081000140418             S1Csun = TBEke2;
081100140418             S1Caew = §3EWaew;
081200140418             S1Clnp = §3EWlnp;
081300140418             S1Cfls = §3EWfls;
081400140418             S1Cnrs = §3EWnrs;
081500140418             S1Cnsi = §3EWnsi;
081600140418             S1Cnsf = §3EWnsf;
081700140418             S1Cctm = §3EWctm;
081800140418             // - -?Campi Hidden?
081900140418             H1Cfaa = §3EWfaa;
082000140418             H1Cnrr = $Row(xR);
082100140418
082200140418             // ·?Decodifica Cliente Unificante?
082300140418             clear  TIBS69ds;
082400140418             I69kcc = DUTkci;
082500140418             I69kac = %int( %subst( S1Cksu : 2 ) );
082600140418             I69sif = knsif;
082700140418             tibs69r ( TIBS69ds :
082800140418                       ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
082900140418             if  O69err = *blank;
083000140418               S1Dksu = %subst(ACOrag : 1 : %len(S1Dksu));
083100140418             endif;
083200140418
083300140418           endif;
083400140418
083500140418           // - -?Attributi di visualizzazione?
083600140418           PosizKSU = (wOrdSfl = 0);
083700140418           PosizRAG = (wOrdSfl = 1);
083800140418           PosizSUN = (wOrdSfl = 2);
083900140418           PosizAEW = (wOrdSfl = 3);
084000140418           PosizLNP = (wOrdSfl = 4);
084100140418           PosizFSR = (wOrdSfl = 5);
084200140418           AnnullApplic = (H1Cfaa = 'A');
084300140418
084400140418           // -?Scrittura record nel subfile S01?
084500140418           S01nrr = xR;
084600140418           write  TB79S01;
084700140418
084800140418           xR += 1;
084900140418
085000140418         EndDo;
085100140418
085200140418         // -?Visualizzazione del SFL (se ci sono dati)?
085300140418         SflDsp_N = (S01nrr <= *zero);
085400140418
085500140418         // -?Attivazione del SFLEND?
085600140418         SflEnd   = ( ($EoF  and  w_CurrPag = w_LastPag)  or
085700140418                       $Err  or  $Row(c_SflPag) = *zero );
085800140418         SflStart = (w_CurrPag <= 1);
085900140418
086000140418         // -?Posizionamento cursore al 1° rcd della pagina?
086100140418         if  S01nrr > *zero;
086200140418           C1RcdNbr = 1;
086300140418         else;
086400140418           clear  C1RcdNbr;
086500140418           clear  w_CurrPag;
086600140418         endif;
086700140418
086800140418         C1CsrRrn = C1RcdNbr;
086900140418
087000140418         // -?Lettura pagina di records successivi?
087100140418         //  ?ed eventuale attivazione del SflEnd?
087200140418         //  ?(se non ce ne fossero più...)?
087300140418         //  ?- SE NON già chiesto un RollDown -?
087400140418         //  ?(nel qual caso i dati sarebbero già stati estratti)?
087500140422         if  Not SflEnd  and  w_CurrPag = w_LastPag  and
087600140422            (w_FetchPag = w_LastPag);
087700140422           exsr  sr_ReadCursor;
087800140422           %occur( $FetchRow ) = 1;
087900140422           SflEnd   = ( $EoF  or  $Err  or  wTBErrn = *zero );
088000140422         endif;
088100140418
088200140418       ENDSR;
088300100204
088400100204       //--------------------------------------------------------------
088500100204       //?Settaggio indicatori utilizzati in C01/P01.                  ?
088600100204       //--------------------------------------------------------------
088700100204       BEGSR  sr_SetIndP01;
088800100204
088900100204         // -?F3=Fine?
089000100204         F3Attivo  = (%parms() = 1);
089100140418
089200140418         // -?F8=Ordinamento x ...?
089300140418         F8Attivo  = (S01nrr > 1);
089400100204
089500100204         // -?F12=Ritorno?
089600100204         F12Attivo = (%parms() > 1);
089700100204
089800100204       ENDSR;
089900100204
090000100204       //--------------------------------------------------------------
090100121008       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
090200121008       //?F8-Cambia ordinamento sfl                                    ?
090300100204       //--------------------------------------------------------------
090400121008       BEGSR  sr_F08S01;
090500100204
090600140418         exsr  sr_CloseCursor;
090700140418
090800140418         reset  $InzS01;
090900140418         clear  SaveKSU;
091000140418         clear  SaveRAG;
091100140418         clear  SaveSUN;
091200140418         clear  SaveAEW;
091300140418         clear  SaveLNP;
091400140418         clear  SaveFLS;
091500140418         clear  SaveNRS;
091600100204
091700140418         if  wOrdSfl < 5;
091800140418           wOrdSfl += 1;
091900140418         else;
092000140418           clear  wOrdSfl;
092100140418         endif;
092200100204
092300100204       ENDSR;
092400100204
092500100204       //--------------------------------------------------------------
092600100204       //?Gestione opzioni subfile S01.                                ?
092700100204       //--------------------------------------------------------------
092800100204       BEGSR  sr_OpzS01;
092900100204
093000100204         clear  SavS01csr;
093100100204
093200100204         // -?Ciclo di lettura subfile?
093300100204         readc  TB79S01;
093400100204
093500100205         DoW  Not %eof(TNTB79D1);
093600100204
093700100204           SflNxtChg = *off;
093800100204           %subst(IndDspF : 50) = *off;
093900100204           C1RcdNbr = S01nrr;
094000100204
094100100204           select;
094200100204
094300100204             // -?Opz. 1=Selezione?
094400140418             when  S1Copz = '1'   and   Not $Fine;
094500100211               o3EWksu = S1Cksu;
094600100211               o3EWsun = S1Csun;
094700140418               chain  H1Cnrr  TNTBE000;
094800140418               if  %found(TNTBE00F);
094900100204                 o3EWuni = TBEuni;
095000100204               endif;
095100100204               $Fine   = *on;
095200100204
095300100204             // -?Opz. 1=Selezione già reperita?
095400100204             //  ?(ammessa una sola selezione)?
095500100205             when  S1Copz = '1'   and   $Fine;
095600100204               ErrMessage  = *on;
095700100204               ErrGenerico = *on;
095800100204               PosCurOPZ   = *on;
095900100204               V1Dmsg = $Msg(02);
096000100204               $Fine  = *off;
096100100204
096200100204             // -?Nessuna opzione?
096300100205             when  S1Copz = *blank;
096400100204
096500100204             // -?Opzione errata?
096600100204             other;
096700100204               ErrMessage  = *on;
096800100204               ErrGenerico = *on;
096900100204               PosCurOPZ   = *on;
097000100204               V1Dmsg = $Msg(01);
097100100204               $Fine  = *off;
097200100204
097300100204           endsl;
097400100204
097500100204           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
097600100204           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
097700100205           if  S1Copz  <> *blank   and
097800100204              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
097900100204             SavS01csr   = C1RcdNbr;
098000100204           endif;
098100100204
098200100204           // -?Aggiornamento sfl?
098300100204           select;
098400100204             when  ErrMessage;
098500100204               SflNxtChg = *on;
098600100204               C1CsrRrn  = C1RcdNbr;
098700100204             when  ErrGenerico;
098800100204               C1CsrRrn  = C1RcdNbr;
098900100205               if  S1Copz <> '1';
099000100205                 clear  S1Copz;
099100100204               endif;
099200100204             other;
099300100205               if  S1Copz <> '1';
099400100205                 clear  S1Copz;
099500100204               endif;
099600100204           endsl;
099700100204
099800100205           if  S1Copz <> *blank;
099900100204             SflNxtChg = *on;
100000100204           endif;
100100121122
100200121122           AnnullApplic = (H1Cfaa = 'A');
100300100204
100400100204           UPDATE  TB79S01;
100500100204
100600100204           if  ErrGenerico   or   ErrMessage;
100700100204             leave;
100800100204           endif;
100900100204
101000100204           readc  TB79S01;
101100100204
101200100204         ENDDO;
101300100204
101400100204         // -?Riposizionam. cursore sul record contenente la prima opz.?
101500100204         //  ?(se non sono stati rilevati errori)?
101600100204         if  NOT ErrMessage   and   NOT ErrGenerico   and
101700100204             SavS01csr > *zero;
101800100204           C1CsrRrn = SavS01csr;
101900100204         endif;
102000100204
102100100204       ENDSR;
102200140418
102300140418       //--------------------------------------------------------------
102400140418       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
102500140418       //--------------------------------------------------------------
102600140418       BEGSR  sr_PrintErrSQL;
102700140418
102800140418         // -?Stampa del Dump?
102900140418         Dump(A);
103000140418
103100140418         // -?Stampa del Job-Log?
103200140418         Qcmd = 'DSPJOBLOG job(*) output(*print)';
103300140418         exsr  sr_ExecCmd;
103400140418
103500140418         // -?Chiusura del programma?
103600140418         exsr  sr_RoutEnd;
103700140418
103800140418       ENDSR;
103900140418
104000140418       //--------------------------------------------------------------
104100140418       //?Esecuzione del comando (già impostato).                      ?
104200140418       //--------------------------------------------------------------
104300140418       BEGSR  sr_ExecCmd;
104400140418
104500140418         clear Qcap0100;
104600140418         Qcabcsdh = *off;
104700140418         Qcapa    = *off;
104800140418         Qcacmdss = *off;
104900140418         Qcaerved = *allX'00';
105000140418
105100140418         clear Qusec;
105200140418         Qusbprv  = %size(Qusec);
105300140418
105400140418         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
105500140418                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
105600140418                           0 : 0 : Qusec);
105700140418
105800140418         // -?Invio *msg per segnalazione errore?
105900140418         //if  Qusei <> *blank;
106000140418         //  ...;
106100140418         //endif;
106200140418
106300140418       ENDSR;
106400100204
106500100204       //--------------------------------------------------------------
106600100204       //?Operazioni finali.                                           ?
106700100204       //--------------------------------------------------------------
106800100204       BEGSR  sr_RoutEnd;
106900100217
107000100217         // -?Chiusura funzioni precedentemente aperte?
107100100217         reset  TIBS69ds;
107200100217         //I69tla = 'C';         ?(già così)?
107300100217         tibs69r( tibs69ds :
107400100217                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
107500100204
107600100204         // -?Ritorno parametri?
107700100204         if  %parms() > 1;
107800100204           p_TNTB79ds = TNTB79ds;
107900100204         endif;
108000100204
108100100204         // -?"Chiusura" *pgm?
108200100204         return;
108300100204
108400100204       ENDSR;
108500100204
108600100204      /end-free
108700100204
108800100204       //--------------------------------------------------------------
108900100204       //?Schiere a tempo di compilazione.                             ?
109000100204       //--------------------------------------------------------------
109100100204
109200140418** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
109300100204Opzione errata                                                                 1
109400100331Ammessa la selezione di un solo cliente                                        2
