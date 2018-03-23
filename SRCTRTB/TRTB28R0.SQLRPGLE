000100110210       //==============================================================
000200111108       //?TRTB28R0 * Interrogazione invio dati - bollettazione -       ?
000300110210       //==============================================================
000400110210
000500110210       //--------------------------------------------------------------
000600110210       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110210       //--------------------------------------------------------------
000800110210
000900110210     /*PRM dbgview(*source)
001000110210     /*END
001100110210
001200110210       //--------------------------------------------------------------
001300110210       //?Specifiche di controllo.                                     ?
001400110210       //--------------------------------------------------------------
001500110210
001600110210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110210     h dftactgrp(*no) bnddir('TRUL')
001800110210     h alwnull(*inputonly)
001900110210
002000110210       //--------------------------------------------------------------
002100110210       //?Dichiarazione file.                                          ?
002200110210       //--------------------------------------------------------------
002300110210
002400110211       // -?Tabella "3C"?
002500110210     fTABEL00F  if   e             disk
002600130531     f                                     extfile(wLibTABEL)  usropn
002700110211       // -?Tabelle "3CE" e "3CP"?
002800110211     fTNTBE01L  if   e           k disk
002900130531     f                                     extfile(wLibTNTBE)  usropn
003000110210
003100110210       // -?Video Gestione?
003200110210     fTRTB28D0  cf   e             workstn
003300110210     f                                     sfile(TB28S01 : S01nrr)
003400110210     f                                     indds(IndDspF)
003500110210     f                                     infds(InfDspF)
003600110210
003700110210       //--------------------------------------------------------------
003800110210       //?Definizione costanti.                                        ?
003900110210       //--------------------------------------------------------------
004000110211
004100110211       // -?Costante Supporto "EasyWEB"?
004200110211     d c_EasyWEB       c                   const('ESWEB')
004300110210
004400110211       // -?Numero di record in ciascuna pagina di subfile?
004500110211     d c_SflPag        c                   const(16)
004600110211
004700110211       // -?Costante per controllo "caratteri solo numerici"?
004800110211     d c_Digits        c                   const('0123456789')
004900110210
005000110210       // -?Tasti funzionali a video?
005100110210     d c_F01           c                   const(x'31')
005200110210     d c_F02           c                   const(x'32')
005300110210     d c_F03           c                   const(x'33')
005400110210     d c_F04           c                   const(x'34')
005500110210     d c_F05           c                   const(x'35')
005600110210     d c_F06           c                   const(x'36')
005700110210     d c_F07           c                   const(x'37')
005800110210     d c_F08           c                   const(x'38')
005900110210     d c_F09           c                   const(x'39')
006000110210     d c_F10           c                   const(x'3A')
006100110210     d c_F11           c                   const(x'3B')
006200110210     d c_F12           c                   const(x'3C')
006300110210     d c_F13           c                   const(x'B1')
006400110210     d c_F14           c                   const(x'B2')
006500110210     d c_F15           c                   const(x'B3')
006600110210     d c_F16           c                   const(x'B4')
006700110210     d c_F17           c                   const(x'B5')
006800110210     d c_F18           c                   const(x'B6')
006900110210     d c_F19           c                   const(x'B7')
007000110210     d c_F20           c                   const(x'B8')
007100110210     d c_F21           c                   const(x'B9')
007200110210     d c_F22           c                   const(x'BA')
007300110210     d c_F23           c                   const(x'BB')
007400110210     d c_F24           c                   const(x'BC')
007500110210     d c_Enter         c                   const(x'F1')
007600110210     d c_RollDown      c                   const(x'F4')
007700110210     d c_RollUp        c                   const(x'F5')
007800110210
007900110210       // -?Attributi di Visualizzazione?
008000110210     d c_ColorNorm     c                   const(x'A0')
008100110210     d c_ColorRed      c                   const(x'A8')
008200110210
008300110210       //--------------------------------------------------------------
008400110210       //?Definizione schiere.                                         ?
008500110210       //--------------------------------------------------------------
008600110210
008700110210       // -?Messaggi di errore?
008800110210     d $Msg            s             78    dim( 1) ctdata perrcd(1)
008900110210
009000110210       // -?RRN di TABEL con cui si scriveranno i rec. della pag.?
009100110211     d $FetchRow       ds                  inz  occurs(c_SflPag)
009200110210     d   wTBLrrn                     10i 0 inz
009300110210
009400110210       // -?RRN dei record di TABEL che compongono la pagina?
009500120216     d*// dsRowPag        ds            80    inz
009600120216     d*//  $Row                         10i 0 dim(c_SflPag)
009700120216     d dsRowPag        ds                  inz
009800140422     d   $Row                              like(wTBLrrn)  dim(c_SflPag)
009900110210
010000120216     d*// $Pag            s             80    dim(32767)  based(pPag)
010100120216     d $Pag            s                   like(dsRowPag)
010200120216     d                                     dim(32767)  based(pPag)
010300110210
010400110210       //--------------------------------------------------------------
010500110210       //?Definizione aree dati.                                       ?
010600110210       //--------------------------------------------------------------
010700110210
010800110210       // -?Dati utente?
010900110210     d §AzUte        e ds                  extname(AZUTE00F)
011000110210     d                                     dtaara
011100110210     d §DatiUte      e ds                  extname(dDatiUte)
011200110210     d                                     dtaara
011300110210
011400110210       //--------------------------------------------------------------
011500110210       //?Definizione strutture dati.                                  ?
011600110210       //--------------------------------------------------------------
011700110211
011800110211       // -?Status ds?
011900110211     d Status         sds
012000110211     d   SDSpgm          *proc
012100110210
012200110210       // -?InfDS?
012300110210     d InfDspF         ds
012400110210     d   dsp_aid             369    369a
012500110210     d   sfl_rrn             376    377i 0
012600110210     d   min_nrr             378    379i 0
012700110210     d   num_rcds            380    381i 0
012800110210
012900110210       // -?Indicatori su DspF?
013000110210     d IndDspF         ds                  inz
013100110210         // -?Abilitazione tasti funzionali?
013200110211     d   F8Attivo                      n   overlay(IndDspF : 08)
013300110210         // -?Emissione messaggio di errore?
013400110210     d   ErrMessage                    n   overlay(IndDspF : 28)
013500110210         // -?Indicatori di gestione del subfile?
013600110210     d   SflDsp_N                      n   overlay(IndDspF : 30)
013700110210     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
013800110210     d   SflNxtChg                     n   overlay(IndDspF : 32)
013900110210     d   SflEnd                        n   overlay(IndDspF : 33)
014000110211     d   SflStart                      n   overlay(IndDspF : 34)
014100110210         // -?Indicatori per attibuti di visualizzazione?
014200110211     d   PosizKEY                      n   overlay(IndDspF : 40)
014300110211     d   PosizRAG                      n   overlay(IndDspF : 41)
014400120517     d   PosizCKS                      n   overlay(IndDspF : 42)
014500120517     d   PosizCBA                      n   overlay(IndDspF : 43)
014600120517     d   PosizFLS                      n   overlay(IndDspF : 44)
014700120517     d   PosizNRS                      n   overlay(IndDspF : 45)
014800120220     d   ProtectFLS                    n   overlay(IndDspF : 49)
014900110210         // -?Posizionamento cursore & Segnalazione errore?
015000110210     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015100110211         // -?Errore di paginazione?
015200110211     d   ErrRoll                       n   overlay(IndDspF : 98)
015300110211         // -?Riemissione videata?
015400110210     d   ErrGenerico                   n   overlay(IndDspF : 99)
015500110210
015600110210       // -?Parametri ricevuti?
015700110210     d KPJBA         e ds
015800110210
015900110211       // -?Tabella "3C"  = Invio dati -Bollettazione-?
016000110210     d ds3C          e ds                  inz
016100110211       // -?Tabella "3CP" = Serie parziali assegnate ai clienti?
016200110211     d d3CP          e ds                  inz
016300110211       // -?Tabella "3EW" = Dati assegnati alla postazione EasyWeb?
016400110211     d d3EW          e ds                  inz
016500120816     d d3EW_Temp     e ds                  inz  extname(d3EW)
016600120816     d                                          qualified
016700110210
016800110210       //--------------------------------------------------------------
016900110210       //?Definizione variabili globali.                               ?
017000110210       //--------------------------------------------------------------
017100110210
017200110210       // -?Flags booleani?
017300110210     d $Fine           s               n   inz
017400110210     d $EoF            s               n   inz
017500110210     d $Err            s               n   inz
017600110210     d $InzS01         s               n   inz(*on)
017700120206     d $FLS            s               n   inz
017800110210
017900110210       // -?Variabili per la gestione del video?
018000110210     d $Video          s              2    inz('S1')
018100110210     d w_CurrPag       s              5  0 inz
018200110210     d w_LastPag       s              5  0 inz
018300140422     d w_FetchPag      s              5  0 inz
018400110210     d wPag            s              5  0 inz
018500110210     d wRig            s              3  0 inz
018600110210     d S01nrr          s              5i 0 inz
018700110210     d SavS01csr       s                   inz  like(C1RcdNbr)
018800110211     d SaveKEY         s                   inz  like(C01key)
018900110211     d SaveRAG         s                   inz  like(C01rag)
019000120517     d SaveCKS         s                   inz  like(C01cks)
019100110211     d SaveCBA         s                   inz  like(C01cba)
019200110211     d SaveFLS         s                   inz  like(C01fls)
019300110211     d SaveNRS         s                   inz  like(C01nrs)
019400111108     d SaveNRS2        s                   inz  like(C01nrs2)
019500111108     d SaveFLS2        s                   inz  like(C01fls2)
019600110210     d wOrdSfl         s              1  0 inz
019700110210
019800110210       // -?Variabili per la gestione del subfile con oltre 9999 righe?
019900110210     d xR              s                   inz  like(S01nrr)
020000110210     d nSize           s             10i 0 inz
020100110210     d pPag            s               *   inz
020200110210
020300110210       // -?Stringa SQL da eseguire?
020400110210     d wSQL            s           2048    inz  Varying
020500130531     d wLibTABsede     s             10    inz
020600130531
020700130531       // -?Nome esteso Libreria/File dei 2 file tabella?
020800130531     d wLibTABEL       s             21a   inz
020900130531     d wLibTNTBE       s             21a   inz
021000110210
021100110210       // -?Campi di comodo?
021200110210     d wDate_Iso       s               d   inz  datfmt(*iso)
021300110211     d w_Fil           s              3    inz
021400110211     d w3CPnsi         s              7  0 inz
021500110210
021600110210       //--------------------------------------------------------------
021700110210       //?Definizione prototipi procedure.                             ?
021800110210       //--------------------------------------------------------------
021900110210
022000110210       // -?Reperimento dati utente?
022100110210     d TIBS34ds      e ds                  inz
022200110210      /copy gaitrasrc/srcProtoPR,TIBS34R
022300110210
022400110210       // -?Caricamento filiali gestite dall'utente?
022500110210     d TRUL31ds      e ds                  inz
022600110210     d   $POg                 10    759    inz   dim(250)
022700110210      /copy gaitrasrc/srcProtoPR,TRUL31R
022800110211
022900110211       // -?Verifica abilitazione STRATEGI?
023000110211     d TIVSSds       e ds                  extname(TIVSS00F) qualified
023100110211      /copy gaitrasrc/srcProtoPI,TIS7701R
023200110211      /copy gaitrasrc/srcProtoPR,TIS7701R
023300110210
023400110210       // -?Controllo ed inversione date?
023500110210     d WLBdat          ds                  inz
023600110210     d  G08dat                        8  0 inz
023700110210     d  G08inv                        8  0 inz
023800110210     d  G08err                        1    inz('3')
023900110210     d  G08tgi                        5  0 inz
024000110210      /copy gaitrasrc/srcProtoPr,XSRDA8
024100110210
024200110210       // -?Parametri gestione errori API.?
024300110210      /copy qsysinc/qRpgleSrc,QUSEC
024400110210       // -?Parametri API QCAPCMD (Process Commands)?
024500110210     d Qcmd            s           2048    inz  varying
024600110210      /copy qSysInc/qRpgleSrc,QCAPCMD
024700110210       // -?API QCAPCMD (Process Commands)?
024800110210      /copy gaitrasrc/srcProtoPR,QCAPCMD
024900110210
025000110210       //--------------------------------------------------------------
025100110210       //?Definizione key-list.                                        ?
025200110210       //--------------------------------------------------------------
025300110210
025400110211       // -?File TNTBE01L?
025500110211     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025600110211     d                                     prefix(k_)   inz
025700110210
025800110210       //--------------------------------------------------------------
025900110210       //?M A I N - L I N E                                            ?
026000110210       //--------------------------------------------------------------
026100110210
026200110210     c     *Entry        plist
026300110210     c                   parm                    KPJBA
026400110210
026500110210      /free
026600110210
026700110210       // -?Operazioni iniziali?
026800110210       exsr sr_RoutInz;
026900110210
027000110210       // -?Gestione videate?
027100110210       DoW  $Fine = *off;
027200110210
027300110210         select;
027400110210           // -?Gestione subfile S1?
027500110210           when $Video = 'S1';
027600110210             exsr sr_GesS01;
027700110210           // -?? ? ? ? ??
027800110210           other;
027900110210             $Fine = *on;
028000110210         endsl;
028100110210       enddo;
028200110210
028300110210       // -?Operazioni finali?
028400110210       exsr sr_RoutEnd;
028500110210
028600110210       //--------------------------------------------------------------
028700110210       //?Operazioni iniziali.                                         ?
028800110210       //--------------------------------------------------------------
028900110210       BEGSR  sr_RoutInz;
029000110210
029100110210         *inLR = *on;
029200110210
029300110210         // -?Impostazione opzioni per SQL?
029400110210         exec sql   set  option  DynUsrPrf = *Owner,
029500110210                                 CloSqlCsr = *EndMod;
029600110210
029700110210         // -?Reperimento dati job?
029800110210         exsr  sr_DatiJob;
029900110210
030000110210         // -?Impostazione nome programma a video?
030100110210         V1Tpgm = SDSpgm;
030200130531
030300130531         // -?Apertura file TABEL00F e TNTBE01L del S.I. di sede?
030400130603         if  %subst( knsif : 7 : 1 ) = 'P';
030500130531           wLibTABsede = 'GAITRAGRPS';
030600130531         else;
030700130531           wLibTABsede = 'GAITRAGRU';
030800130531         endif;
030900130531         wLibTABEL = %trimr(wLibTABsede) + '/TABEL00F';
031000130531         wLibTNTBE = %trimr(wLibTABsede) + '/TNTBE01L';
031100130531         open  TABEL00F;
031200130531         open  TNTBE01L;
031300110211
031400110211         // -?Verifica se *pgm richiamato con filiale:?
031500120215         //  ?test delle prime 3+3 posizioni della KPJBA per sapere se?
031600120215         //  ?il *pgm è stato richiamato dal *pgm TRTB28R o dal *pgm?
031700120215         //  ?TNTB91R.?
031800120206         reset  $FLS;
031900110211         if  %subst( KPJBU : 1 : 3 ) <> *blank  and
032000110211             %check( c_Digits : %subst( KPJBU : 1 : 3 ) ) = *zero;
032100110211           w_Fil = %subst( KPJBU : 1 : 3 );
032200120206           $FLS  = ( %subst( KPJBU : 4 : 3 ) = 'FLS' );
032300120220           // -?Per supporto Filiale/Serie Segnacollo?
032400120220           if  $FLS;
032500120220             wOrdSfl  = 3;
032600120220             C01fls   = %int( w_Fil );
032700120220             SaveFLS  = %int( w_Fil );
032800120220             C01fls2  = %int( w_Fil );
032900120220             SaveFLS2 = %int( w_Fil );
033000120220           endif;
033100110211         endif;
033200110210
033300110210       ENDSR;
033400110210
033500110210       //--------------------------------------------------------------
033600110210       //?Reperimento Dati del job (Utente/Operativi).                 ?
033700110210       //--------------------------------------------------------------
033800110210       BEGSR  sr_DatiJob;
033900110210
034000110210         in(e) §AzUte;
034100110210         if NOT %error;
034200110210           in(e) §DatiUte;
034300110210         endif;
034400110210         if %error or RSut = *blank;
034500110210           tibs34r ( tibs34ds );
034600110210           in §AzUte;
034700110210           in §DatiUte;
034800110210         endif;
034900110210
035000110210       ENDSR;
035100110210
035200110210       //--------------------------------------------------------------
035300110210       //?Preparazione stringa SQL.                                    ?
035400110210       //--------------------------------------------------------------
035500110210       BEGSR  sr_PrepSQL;
035600110210
035700111108         wSQL = 'With TABEL as +
035800111108                 ( select rrn(TABEL00F) as Nrr, TABEL00F.*, +
035900111108                          substr(TBLuni, 36, 3) as Fls +
036000130531                     from ' + %trimr(wLibTABsede) + '/TABEL00F +
036100161128                    where TBLkut = 1 +
036200161128                      and TBLcod = ''3C'' +
036300111108                      and substr(TBLuni, 44, 5) <> ''ESWEB'' +
036400111108                   UNION +
036500111108                   select rrn(TABEL00F) as Nrr, TABEL00F.*, +
036600111108                          substr(TBEuni, 4, 3) as Fls +
036700130531                     from ' + %trimr(wLibTABsede) + '/TABEL00F +
036800130531                          inner join ' +
036900130531                              %trimr(wLibTABsede) + '/TNTBE00F +
037000161128                       on TBLkut = 1 +
037100161128                      and TBLcod = ''3C'' and TBEcod = ''3EW'' +
037200121119                      and substr(TBLuni, 79, 7) = substr(TBEke1, 2, 7) +
037300161128                    where TBLkut = 1 +
037400161128                      and substr(TBLuni, 44, 5) = ''ESWEB'' +
037500120816                      and substr(TBEuni, 33, 1) = '' '' ) +
037600111108                 select Nrr from TABEL';
037700110211
037800110211         // -?Ricevuta filiale dal chiamante?
037900110211         if  w_Fil > *zero;
038000120206           if  Not $FLS;
038100120206             wSQL += ' where substr( TBLkey, 1, 3 ) = ''' + w_Fil + '''';
038200120206           else;
038300120206             wSQL += ' where Fls = ''' + w_Fil + '''';
038400120206           endif;
038500110211         endif;
038600110210
038700110211         // -?Posizionamento / Ordinamento?
038800110211         Select;
038900110211
039000110211           // -?Per codice cliente?
039100110211           When  wOrdSfl = 0;
039200110211             if  SaveKEY <> *zero;
039300111108               if  w_Fil > *zero;
039400111108                 wSQL += ' and';
039500111108               else;
039600111108                 wSQL += ' where';
039700111108               endif;
039800111108               wSQL += ' TBLkey >= ''' + %editc( SaveKEY : 'X' ) + '''';
039900110211             endif;
040000110211             wSQL += ' order by TBLkey';
040100110211
040200110211           // -?Per ragione sociale cliente?
040300110211           When  wOrdSfl = 1;
040400110211             if  SaveRAG <> *blank;
040500111108               if  w_Fil > *zero;
040600111108                 wSQL += ' and';
040700111108               else;
040800111108                 wSQL += ' where';
040900111108               endif;
041000111108               wSQL += ' substr(TBLuni, 4, 20) >= ''' + SaveRAG + '''';
041100110211             endif;
041200120213             wSQL += ' order by substr(TBLuni, 4, 20), TBLkey';
041300120517
041400120517           // -?Per codice unificante?
041500120517           When  wOrdSfl = 2;
041600120517             if  SaveCKS <> *zero;
041700120517               if  w_Fil > *zero;
041800120517                 wSQL += ' and';
041900120517               else;
042000120517                 wSQL += ' where';
042100120517               endif;
042200120517               wSQL += ' substr(TBLuni, 79, 7) >= ''' +
042300120517                       %editc( SaveCKS : 'X' ) + '''';
042400120517             endif;
042500120517             wSQL += ' order by substr(TBLuni, 79, 7), TBLkey';
042600110211
042700120517           // -?Per supporto Cliente->BRT?
042800120517           When  wOrdSfl = 3;
042900110211             if  SaveCBA <> *blank;
043000111108               if  w_Fil > *zero;
043100111108                 wSQL += ' and';
043200111108               else;
043300111108                 wSQL += ' where';
043400111108               endif;
043500111108               wSQL += ' substr(TBLuni, 44, 5) >= ''' + SaveCBA + '''';
043600110211             endif;
043700120213             wSQL += ' order by substr(TBLuni, 44, 5), TBLkey';
043800110211
043900110211           // -?Per supporto Filiale/Serie Segnacollo?
044000120517           When  wOrdSfl = 4;
044100110211             if  SaveFLS <> *zero  or  SaveNRS <> *zero;
044200111108               if  w_Fil > *zero;
044300111108                 wSQL += ' and';
044400111108               else;
044500111108                 wSQL += ' where';
044600111108               endif;
044700120213               wSQL +=     ' Fls >= ''' +
044800111108                             %editc( SaveFLS : 'X' ) + ''' +
044900110211                         and substr(TBLuni, 1, 2) >= ''' +
045000110211                             %editc( SaveNRS : 'X' ) + '''';
045100110211             endif;
045200120213             wSQL += ' order by Fls, substr(TBLuni, 1, 2), TBLkey';
045300111108
045400111108           // -?Per supporto Serie/Filiale Segnacollo?
045500120517           When  wOrdSfl = 5;
045600111108             if  SaveNRS2 <> *zero  or  SaveFLS2 <> *zero;
045700111108               if  w_Fil > *zero;
045800111108                 wSQL += ' and';
045900111108               else;
046000111108                 wSQL += ' where';
046100111108               endif;
046200111108               wSQL +=     ' substr(TBLuni, 1, 2) >= ''' +
046300111108                             %editc( SaveNRS2 : 'X' ) + ''' +
046400120213                         and Fls >= ''' +
046500111108                             %editc( SaveFLS2 : 'X' ) + '''';
046600111108             endif;
046700120213             wSQL += ' order by substr(TBLuni, 1, 2), Fls, TBLkey';
046800110211
046900110211         EndSl;
047000110210
047100110210         wSQL += ' for fetch only +
047200110211                  optimize for ' + %editc( c_SflPag : 'X' ) + ' rows';
047300110210
047400110210       ENDSR;
047500110210
047600110210       //--------------------------------------------------------------
047700110210       //?Apertura cursore C1.                                         ?
047800110210       //--------------------------------------------------------------
047900110210       BEGSR  sr_OpenCursor;
048000110210
048100110210         // -?Dichiarazione cursore?
048200110210         exec sql   prepare S1   from :wSQL;
048300110210         exec sql   declare C1   cursor for S1;
048400110210
048500110210         // -?Apertura del cursore?
048600110210         exec sql   open C1;
048700110210
048800110210         if  SQLcode < *zero;
048900110210           $Err = *on;
049000110210           $EoF = *on;
049100110210           exsr  sr_PrintErrSQL;
049200110210         endif;
049300110210
049400110210       ENDSR;
049500110210
049600110210       //--------------------------------------------------------------
049700110210       //?Chiusura cursore C1.                                         ?
049800110210       //--------------------------------------------------------------
049900110210       BEGSR  sr_CloseCursor;
050000110210
050100110210         // -?Chiusura del cursore?
050200110210         exec sql   close C1;
050300110210
050400110210       ENDSR;
050500110210
050600110210       //--------------------------------------------------------------
050700110210       //?Lettura cursore C1.                                          ?
050800110210       //--------------------------------------------------------------
050900110210       BEGSR  sr_ReadCursor;
051000110210
051100110211         clear  $FetchRow;
051200140422         w_FetchPag += 1;
051300110210
051400110211         // -?Reperimento dei 16 (c_SflPag) Rrn che comporranno la pag.?
051500110211         exec sql   fetch next   from C1   for 16 rows
051600110210                    into  :$FetchRow;
051700110210
051800110210         select;
051900110210           when  SQLcode = 100;
052000110210             $EoF = *on;
052100110210           when  SQLcode < *zero;
052200110210             $Err = *on;
052300110211             $EoF = *on;
052400110210             exsr  sr_PrintErrSQL;
052500110210         endsl;
052600110210
052700110210       ENDSR;
052800110210
052900110210       //--------------------------------------------------------------
053000110210       //?Gestione subfile S01 - Subfile con elenco tab. "3C".         ?
053100110210       //--------------------------------------------------------------
053200110210       BEGSR  sr_GesS01;
053300110210
053400110210         // -?Inizializzazione subfile?
053500110210         if  $InzS01  and  Not ErrGenerico;
053600110210           exsr  sr_InzS01;
053700110210           $InzS01 = *off;
053800110210         endif;
053900110210
054000110210         // -?Emissione Testata e Piede con tasti funzionali abilitati?
054100110210         write  TB28T01;
054200110210         write  TB28P01;
054300110210
054400110210         // -?Posizionamento cursore?
054500110210         if  C1CsrRrn > *zero;
054600110210           C1RcdNbr = C1CsrRrn;
054700110210         else;
054800110210           C1RcdNbr = 1;
054900110211           //write  TB28S00;             //(no rec.)?
055000110210         endif;
055100110210
055200110210         // -?Emissione videata?
055300110210         exfmt  TB28C01;
055400110210
055500110210         reset  ErrMessage;
055600110210         reset  ErrGenerico;
055700110210         clear  V1Dmsg;
055800110210
055900110210         SELECT;
056000110210
056100110210           // -?F3=Fine?
056200110210           WHEN  dsp_aid = c_F03;
056300110210             exsr  sr_CloseCursor;
056400110210             $Fine = *on;
056500110210
056600110210           // -?F5=Aggiornamento?
056700110210           WHEN  dsp_aid = c_F05;
056800110210             exsr  sr_CloseCursor;
056900110210             reset  $InzS01;
057000110210
057100110210           // -?F8=Ordinamento Sfl?
057200110210           WHEN  dsp_aid = c_F08;
057300110210             exsr  sr_F08S01;
057400110210
057500110210           // -?F12=Ritorno?
057600110210           WHEN  dsp_aid = c_F12;
057700110210             exsr  sr_CloseCursor;
057800110211             clear  kpjbu;
057900110210             $Fine = *on;
058000110210
058100110210           // -?Roll-Up?
058200110210           WHEN  dsp_aid = c_RollUp;
058300110210             exsr sr_RollUpS01;
058400110210
058500110210           // -?Roll-Down?
058600110210           WHEN  dsp_aid = c_RollDown;
058700110210             exsr sr_RollDownS01;
058800110210
058900110210           // -?SubFile vuoto?
059000110210           WHEN  S01nrr = *zero;
059100110210
059200110210           // -?Invio?
059300110210           OTHER;
059400110210             // -?Gestione opzioni?
059500110210             exsr  sr_OpzS01;
059600110210             if  ErrGenerico;
059700110210               leavesr;
059800110210             endif;
059900110211             // -?Nuovo posizionamento?
060000120517             if  C01key  <> SaveKEY   or
060100120517                 C01rag  <> SaveRAG   or
060200120517                 C01cks  <> SaveCKS   or
060300120517                 C01cba  <> SaveCBA   or
060400120517                (C01fls  <> SaveFLS   or  C01nrs <> SaveNRS)  or
060500111108                (C01nrs2 <> SaveNRS2  or  C01fls2 <> SaveFLS2);
060600110211               exsr  sr_CloseCursor;
060700110211               reset  $InzS01;
060800120517               SaveKEY  = C01key;
060900120517               SaveRAG  = C01rag;
061000120517               SaveCKS  = C01cks;
061100120517               SaveCBA  = C01cba;
061200120517               SaveFLS  = C01fls;
061300120517               SaveNRS  = C01nrs;
061400111108               SaveNRS2 = C01nrs2;
061500111108               SaveFLS2 = C01fls2;
061600110211             endif;
061700110210
061800110210         ENDSL;
061900110210
062000110210       ENDSR;
062100110210
062200110210       //--------------------------------------------------------------
062300110210       //?Inizializzazione subfile S01.                                ?
062400110210       //--------------------------------------------------------------
062500110210       BEGSR  sr_InzS01;
062600110210
062700110211         // -?La pulizia del subfile verrà eseguita nella subr.?
062800110211         //  ?"sr_WrtS01", eseguita nella subr. "sr_RollUpS01"?
062900110211
063000110211         // -?Pulizia subfile-control?
063100110211         clear  TB28C01;
063200110211
063300120517         C01key  = SaveKEY;
063400120517         C01rag  = SaveRAG;
063500120517         C01cks  = SaveCKS;
063600120517         C01cba  = SaveCBA;
063700120517         C01fls  = SaveFLS;
063800120517         C01nrs  = SaveNRS;
063900111108         C01nrs2 = SaveNRS2;
064000111108         C01fls2 = SaveFLS2;
064100110210
064200110211         $EoF = *off;
064300110210         $Err = *off;
064400110210         clear  w_CurrPag;
064500110211         clear  w_LastPag;
064600140422         clear  w_FetchPag;
064700110210
064800110210         // -?Preparazione stringa SQL?
064900110210         exsr  sr_PrepSQL;
065000110210
065100110210         // -?Caricamento della 1ª pagina di dati del subfile?
065200110210         exsr  sr_OpenCursor;
065300110211         exsr  sr_ReadCursor;
065400110210         exsr  sr_RollUpS01;
065500110210
065600110210         // -?Impaginazione subfile?
065700110210         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
065800110210         if S01nrr > *zero;
065900110210           C1RcdNbr  = 1;
066000110210           C1CsrRrn  = 1;
066100110210         else;
066200110210           clear C1RcdNbr;
066300110210         endif;
066400110210
066500110210         // -?Abilitazione CONDIZIONATE tasti funzionali in P01 (per C01)?
066600110210         F8Attivo  = (S01nrr > *zero);
066700110210
066800110211         // -?Visualizzazione richiesta di posizionamento?
066900110211         //  ?& Impostazione F8 nel piede del subfile?
067000120220         ProtectFLS = *off;
067100110211         select;
067200110211           // -?SubFile vuoto...?
067300110211           when  S01nrr = *zero;
067400110211             PosizKEY = *off;
067500110211             PosizRAG = *off;
067600120517             PosizCKS = *off;
067700110211             PosizCBA = *off;
067800111108             PosizFLS = *off;
067900110211             PosizNRS = *off;
068000110211             clear  V1PF08;
068100110211           // -?Già ordinato per codice cliente?
068200110210           when  wOrdSfl = 0;
068300110211             PosizKEY = *on;
068400110211             PosizRAG = *off;
068500120517             PosizCKS = *off;
068600110211             PosizCBA = *off;
068700111108             PosizFLS = *off;
068800110211             PosizNRS = *off;
068900110211             V1PF08   = 'F8=Ord. x Ragione Sociale';
069000110211           // -?Già ordinato per ragione sociale?
069100110210           when  wOrdSfl = 1;
069200110211             PosizKEY = *off;
069300110211             PosizRAG = *on;
069400120517             PosizCKS = *off;
069500110211             PosizCBA = *off;
069600111108             PosizFLS = *off;
069700110211             PosizNRS = *off;
069800120517             V1PF08   = 'F8=Ord. x Cod. Unificante';
069900120517           // -?Già ordinato per codice unificante?
070000120517           when  wOrdSfl = 2;
070100120517             PosizKEY = *off;
070200120517             PosizRAG = *off;
070300120517             PosizCKS = *on;
070400120517             PosizCBA = *off;
070500120517             PosizFLS = *off;
070600120517             PosizNRS = *off;
070700120517             V1PF08   = 'F8=Ord. x Supp. Cli->BRT ';
070800120517           // -?Già ordinato per supporto cliente => BRT?
070900120517           when  wOrdSfl = 3;
071000110211             PosizKEY = *off;
071100110211             PosizRAG = *off;
071200120517             PosizCKS = *off;
071300110211             PosizCBA = *on;
071400111108             PosizFLS = *off;
071500110211             PosizNRS = *off;
071600110211             V1PF08   = 'F8=Ord. x Filiale/Serie  ';
071700110211           // -?Già ordinato per filiale/serie segnacollo?
071800120517           when  wOrdSfl = 4;
071900110211             PosizKEY = *off;
072000110211             PosizRAG = *off;
072100120517             PosizCKS = *off;
072200110211             PosizCBA = *off;
072300111108             PosizFLS = *on;
072400111108             PosizNRS = *off;
072500111108             V1PF08   = 'F8=Ord. x Serie/Filiale  ';
072600120220             ProtectFLS = $FLS;
072700111108           // -?Già ordinato per serie/filiale segnacollo?
072800120517           when  wOrdSfl = 5;
072900111108             PosizKEY = *off;
073000111108             PosizRAG = *off;
073100120517             PosizCKS = *off;
073200111108             PosizCBA = *off;
073300111108             PosizFLS = *off;
073400111108             PosizNRS = *on;
073500111108             V1PF08   = 'F8=Ord. x Codice Cliente ';
073600120220             ProtectFLS = $FLS;
073700110210         endsl;
073800110210
073900110210       ENDSR;
074000110211
074100110211       //--------------------------------------------------------------
074200110211       //?Pagina del SubFile in avanti.                                ?
074300110211       //--------------------------------------------------------------
074400110211       BEGSR  sr_RollUpS01;
074500110211
074600110211         if  w_CurrPag = w_LastPag;
074700110211           w_CurrPag += 1;
074800110211           exsr  sr_NuovaPagina;
074900110211         else;
075000110211           w_CurrPag += 1;
075100110211         endif;
075200110211
075300110211         exsr  sr_WrtS01;
075400110211
075500110211       ENDSR;
075600110211
075700110211       //--------------------------------------------------------------
075800110211       //?Pagina del SubFile indietro.                                 ?
075900110211       //--------------------------------------------------------------
076000110211       BEGSR  sr_RollDownS01;
076100110211
076200110211         if  w_CurrPag > 1;
076300110211           w_CurrPag -= 1;
076400110211           exsr  sr_WrtS01;
076500110211         else;
076600110211           ErrRoll = *on;
076700110211         endif;
076800110211
076900110211
077000110211       ENDSR;
077100110210
077200110210       //--------------------------------------------------------------
077300110210       //?Caricamento singola pagina in memoria.                       ?
077400110210       //--------------------------------------------------------------
077500110210       BEGSR  sr_NuovaPagina;
077600110210
077700110211         // -?Memorizzazione n° dell'ultima pagina già letta?
077800110210         w_LastPag = w_CurrPag;
077900110210
078000110211         // -?Memorizzazione, nella schiera $Row, dei RRN del file?
078100110211         //  ?con cui verranno scritte le righe della pagina?
078200110210         clear  $Row;
078300110210         For  xR = 1  To  SqlEr3;
078400110210           %occur( $FetchRow ) = xR;
078500110210           $Row(xR) = wTBLrrn;
078600110210         EndFor;
078700110210
078800110210         // -?Allocazione della memoria necessaria per memorizzare?
078900110210         //  ?il nuovo elemento?
079000110210         nSize = %size($Pag) * w_CurrPag;
079100110210         if  w_CurrPag = 1;
079200110210           pPag = %alloc( nSize );
079300110210         else;
079400110210           pPag = %realloc( pPag : nSize );
079500110210         endif;
079600110210
079700110210         // -?Memorizzazione della pagina?
079800110210         $Pag(w_CurrPag) = dsRowPag;
079900110210
080000110210         // -?Fermo il caricamento di nuove pagine:?
080100110210         //  ?se i records sono finiti, oppure?
080200110210         //  ?se ho raggiunto la capienza massima?
080300110210         if  SqlEr5 = 100  or  w_CurrPag = %elem($Pag);
080400140422           $EoF = *on;
080500110210           leavesr;
080600110210         endif;
080700110210
080800110210       ENDSR;
080900110210
081000110210       //--------------------------------------------------------------
081100110210       //?Caricamento singola pagina nel subfile S01.                  ?
081200110210       //--------------------------------------------------------------
081300110211       BEGSR  sr_WrtS01;
081400110210
081500110210         // -?Spegnimento degli indicatori di errore?
081600110210         %subst(IndDspF : 50) = *off;
081700110210
081800110210         // -?Pulizia subfile?
081900110210         SflDsp_N    = *on;
082000110210         SflDspCtl_N = *on;
082100110210         write  TB28C01;
082200110210         SflDspCtl_N = *off;
082300110210         SflEnd      = *off;
082400110211         SflStart    = *off;
082500110210
082600110210         clear  C1RcdNbr;
082700110210         clear  C1CsrRrn;
082800110210         clear  S01nrr;
082900110210         clear  V1Dmsg;
083000110210         ErrMessage  = *off;
083100110210         ErrGenerico = *off;
083200110210
083300110210         SflNxtChg = *off;
083400110210
083500110211         // -?Reperimento dei Rrn che compongono la pagina?
083600110210         dsRowPag = $Pag(w_CurrPag);
083700110210
083800110210         // -?Ciclo di caricamento dati nel subfile?
083900110211         xR = 1;
084000110211         DoW  xR      <= c_SflPag  and
084100110211              $Row(xR) > *zero;
084200110210
084300110210           clear  TB28S01;
084400110210
084500110210           // -?Caricamento del singolo record nel subfile S01?
084600110211           chain  $Row(xR)  TABEL;
084700110210           // - -?Campi di Output?
084800110211           if  %found(TABEL00F);
084900111104             S01flg = TBLflg;
085000110211             S01cod = TBLkey;
085100110211             ds3C   = TBLuni;
085200110211             S01des = §3Crag;
085300110211             S01cks = §3Ccks;
085400110211             S01cba = §3Ccba;
085500110211             S01fls = §3Cfls;
085600110211             S01nrs = §3Cnrs;
085700110211           endif;
085800110211
085900110211           if  §3Cnrs > *zero;
086000110211             exsr  sr_Tab3CP3EW;
086100110211           endif;
086200120206
086300120517           // - -?Campi Hidden?
086400120206           S1Hds3C = ds3C;
086500120517
086600120517           // - -?Attributi di visualizzazione?
086700120517           PosizKEY = (wOrdSfl = 0);
086800120517           PosizRAG = (wOrdSfl = 1);
086900120517           PosizCKS = (wOrdSfl = 2);
087000120517           PosizCBA = (wOrdSfl = 3);
087100120517           PosizFLS = (wOrdSfl = 4);
087200120517           PosizNRS = (wOrdSfl = 5);
087300110210
087400110210           // -?Scrittura record nel subfile S01?
087500110211           S01nrr = xR;
087600110210           write  TB28S01;
087700110211
087800110211           xR += 1;
087900110210
088000110210         EndDo;
088100110210
088200110210         // -?Visualizzazione del SFL (se ci sono dati)?
088300110210         SflDsp_N = (S01nrr <= *zero);
088400110210
088500110210         // -?Attivazione del SFLEND?
088600110211         SflEnd   = ( ($EoF  and  w_CurrPag = w_LastPag)  or
088700110211                       $Err  or  $Row(c_SflPag) = *zero );
088800110211         SflStart = (w_CurrPag <= 1);
088900110210
089000110210         // -?Posizionamento cursore al 1° rcd della pagina?
089100110210         if  S01nrr > *zero;
089200110210           C1RcdNbr = 1;
089300110210         else;
089400110210           clear  C1RcdNbr;
089500110210           clear  w_CurrPag;
089600110210         endif;
089700110210
089800110210         C1CsrRrn = C1RcdNbr;
089900110211
090000110211         // -?Lettura pagina di records successivi?
090100110211         //  ?ed eventuale attivazione del SflEnd?
090200110211         //  ?(se non ce ne fossero più...)?
090300110211         //  ?- SE NON già chiesto un RollDown -?
090400110211         //  ?(nel qual caso i dati sarebbero già stati estratti)?
090500140422         if  Not SflEnd  and  w_CurrPag = w_LastPag  and
090600140422             w_FetchPag = w_LastPag;
090700110211           exsr  sr_ReadCursor;
090800140422           %occur( $FetchRow ) = 1;
090900140422           SflEnd   = ( $EoF  or  $Err  or  wTBLrrn = *zero );
091000110211         endif;
091100110210
091200110210       ENDSR;
091300110211
091400110211       //--------------------------------------------------------------
091500110211       //?Recupero range segnacolli per clienti da tab. "3CP" o "3EW". ?
091600110211       //--------------------------------------------------------------
091700110211       BEGSR  sr_Tab3CP3EW;
091800110211
091900110211         // -?Cliente con EasyWEB?
092000110211         If  §3Ccba = c_EasyWeb;
092100110211
092200110211           clear  d3EW;
092300110211
092400110211           // -?Reperimento codice completo del padre?
092500110211           clear  TIVSSds;
092600110211           clear  rqsSUN;
092700110211           rqsKSU = '0' + %editc( §3Ccks : 'X' );
092800110211           clear  rpyEsito;
092900110211           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
093000110211                                    rqsSUN : *zero : TIVSSds :
093100110211                                    rpyEsito );
093200110211
093300110211           if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
093400110211
093500110211             // -?Reperimento segnacolli da tab. "3EW"?
093600110211             k_TBEcod = '3EW';
093700110211             k_TBEke1 = '0' + %editc( §3Ccks : 'X' );
093800110211             setll  %kds( k05tntbe01 : 2 )  TNTBE000;
093900110211             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
094000110211             dow  Not %eof(TNTBE01L);
094100120816               d3EW_Temp = TBEuni;
094200120816               if  (TBEatb = *blank   and   d3EW_Temp.§3EWfaa = *blank)
094300120816                   or   TBLflg <> *blank;
094400110211                 if  TBEke1 = TIVSSds.VSSksu   and
094500110211                     TBEke2 = TIVSSds.VSSsun;
094600110211                   d3EW = TBEuni;
094700120816                   if  §3EWfaa = *blank;
094800120816                     leave;
094900120816                   endif;
095000110211                 endif;
095100110211               endif;
095200110211               reade  %kds( k05tntbe01 : 2 )  TNTBE000;
095300110211             enddo;
095400110211
095500110211           endif;
095600110211
095700120816           //if  §3EWfls <> *zero  and  §3EWfaa = *blank;
095800120816             §3Cfls = §3EWfls;
095900120816             S01fls = §3EWfls;
096000120816             S01nsi = §3EWnsi;
096100120816             S01nsf = §3EWnsf;
096200120816           //endif;
096300110211
096400110211         // -?Cliente senza EasyWEB?
096500110211         Else;
096600110211
096700110211           clear  w3CPnsi;
096800110211           clear  d3CP;
096900110211
097000110211           // -?Reperimento segnacolli da tab. "3CP"?
097100110211           k_TBEcod = '3CP';
097200110211           k_TBEke1 = TBLkey;
097300110211           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
097400110211           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
097500110211           dow  Not %eof(TNTBE01L);
097600110211             if  %subst( TBEke2 : 1 : 2 ) = %editc( §3Cnrs : 'X' ) AND
097700110211               ( (TBLflg = '*'      and   TBEatb = 'A')   or
097800110211                 (TBLflg = *blank   and   TBEatb = *blank) );
097900110211               d3CP = TBEuni;
098000110211               w3CPnsi = %int( %subst( TBEke2 : 3 : 7 ) );
098100110211               leave;
098200110211             endif;
098300110211             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
098400110211           enddo;
098500110211
098600110211           S01nsi = w3CPnsi;
098700110211           S01nsf = §3CPal;
098800110211
098900110211         EndIf;
099000110211
099100110211         if  S01nsi = *zero  and  S01nsf = *zero;
099200110211           S01nsi = 1;
099300110211           S01nsf = *hival;
099400110211         endif;
099500110211
099600110211       ENDSR;
099700110211
099800110211       //--------------------------------------------------------------
099900110211       //?Gestione tasto funzionale F8 = Ordinamento subfile.          ?
100000110211       //--------------------------------------------------------------
100100110211       BEGSR  sr_F08S01;
100200110211
100300110211         exsr  sr_CloseCursor;
100400110211
100500110211         reset  $InzS01;
100600110211         clear  SaveKEY;
100700110211         clear  SaveRAG;
100800110211         clear  SaveCBA;
100900120220         if  Not  $FLS;
101000120220           clear  SaveFLS;
101100120220         endif;
101200110211         clear  SaveNRS;
101300111108         clear  SaveNRS2;
101400120220         if  Not  $FLS;
101500120220           clear  SaveFLS2;
101600120220         endif;
101700110211         clear  C01key;
101800110211         clear  C01rag;
101900110211         clear  C01cba;
102000120220         if  Not  $FLS;
102100120220           clear  C01fls;
102200120220         endif;
102300110211         clear  C01nrs;
102400111108         clear  C01nrs2;
102500120220         if  Not  $FLS;
102600120220           clear  C01fls2;
102700120220         endif;
102800110211
102900120517         if  wOrdSfl < 5;
103000110211           wOrdSfl += 1;
103100110211         else;
103200110211           clear  wOrdSfl;
103300110211         endif;
103400110211
103500110211       ENDSR;
103600110210
103700110210       //--------------------------------------------------------------
103800110210       //?Gestione opzioni del subfile S01.                            ?
103900110210       //--------------------------------------------------------------
104000110210       BEGSR  sr_OpzS01;
104100110210
104200110210         clear  SavS01csr;
104300110210
104400110210         // -?Ciclo di lettura subfile?
104500110211         readc  TB28S01;
104600110210
104700110211         DoW  Not %eof(TRTB28D0);
104800110210
104900110210           SflNxtChg = *off;
105000110210           %subst(IndDspF : 50) = *off;
105100110210           C1RcdNbr = S01nrr;
105200110210
105300110210           select;
105400110210
105500110210             // -?1=Selezione?
105600110211             when  S01opz = '1';
105700110210               exsr  sr_Opz01_S01;
105800110210
105900110210             // -?Nessuna opzione?
106000110211             when  S01opz = *blank;
106100110210
106200110210             // -?Opzione errata?
106300110210             other;
106400110210               ErrMessage  = *on;
106500110210               ErrGenerico = *on;
106600110210               PosCurOPZ   = *on;
106700110210               V1Dmsg = $Msg(01);
106800110210
106900110210           endsl;
107000110210
107100110210           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
107200110210           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
107300110211           if  S01opz  <> *blank   and
107400110210              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
107500110210             SavS01csr   = C1RcdNbr;
107600110210           endif;
107700110210
107800110210           // -?Aggiornamento sfl?
107900110210           select;
108000110210             when  ErrMessage;
108100110210               SflNxtChg = *on;
108200110210               C1CsrRrn  = C1RcdNbr;
108300110210             when  ErrGenerico;
108400110210               C1CsrRrn  = C1RcdNbr;
108500110211               clear  S01opz;
108600110210             other;
108700110211               clear  S01opz;
108800110210           endsl;
108900110211           UPDATE  TB28S01;
109000110210           if  ErrGenerico   or   ErrMessage;
109100110210             leave;
109200110210           endif;
109300110210
109400110211           readc  TB28S01;
109500110210
109600110210         ENDDO;
109700110210
109800110210         // -?Riposiziono il cursore sul record contenente la prima opz.?
109900110210         //  ?(se non sono stati rilevati errori)?
110000110210         if  NOT ErrMessage   and   NOT ErrGenerico   and
110100110210             SavS01csr > *zero;
110200110210           C1CsrRrn = SavS01csr;
110300110210         endif;
110400110210
110500110210       ENDSR;
110600110210
110700110210       //--------------------------------------------------------------
110800110210       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
110900110210       //--------------------------------------------------------------
111000110210       BEGSR  sr_PrintErrSQL;
111100110210
111200110210         // -?Stampa del Dump?
111300110210         Dump(A);
111400110210
111500110210         // -?Stampa del Job-Log?
111600110210         Qcmd = 'DSPJOBLOG job(*) output(*print)';
111700110210         exsr  sr_ExecCmd;
111800110210
111900110210         // -?Chiusura del programma?
112000110210         exsr  sr_RoutEnd;
112100110210
112200110210       ENDSR;
112300110210
112400110210       //--------------------------------------------------------------
112500110210       //?Esecuzione del comando (già impostato).                      ?
112600110210       //--------------------------------------------------------------
112700110210       BEGSR  sr_ExecCmd;
112800110210
112900110210         clear Qcap0100;
113000110210         Qcabcsdh = *off;
113100110210         Qcapa    = *off;
113200110210         Qcacmdss = *off;
113300110210         Qcaerved = *allX'00';
113400110210
113500110210         clear Qusec;
113600110210         Qusbprv  = %size(Qusec);
113700110210
113800110210         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
113900110210                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
114000110210                           0 : 0 : Qusec);
114100110210
114200110210         // -?Invio *msg per segnalazione errore?
114300110210         //if  Qusei <> *blank;
114400110210         //  ...;
114500110210         //endif;
114600110210
114700110210       ENDSR;
114800110210
114900110210       //--------------------------------------------------------------
115000110210       //?Selezione singolo record.                                    ?
115100110210       //--------------------------------------------------------------
115200110210       BEGSR  sr_Opz01_S01;
115300110210
115400110210         ErrGenerico = *on;
115500110211         kpjbu = S01cod + S1HDS3C;
115600110210         $Fine = *on;
115700110210
115800110210       ENDSR;
115900110210
116000110210       //--------------------------------------------------------------
116100110210       //?Operazioni finali.                                           ?
116200110210       //--------------------------------------------------------------
116300110210       BEGSR  sr_RoutEnd;
116400110210
116500110210         // -?Uscita dal *pgm?
116600110210         return;
116700110210
116800110210       ENDSR;
116900110210
117000110210      /end-free
117100110210
117200110210       //--------------------------------------------------------------
117300110210       //?Schiere a tempo di compilazione.                             ?
117400110210       //--------------------------------------------------------------
117500110210
117600110210** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
117700110210Opzione errata                                                                 1
