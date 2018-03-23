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
036100111108                    where TBLcod = ''3C'' +
036200111108                      and substr(TBLuni, 44, 5) <> ''ESWEB'' +
036300111108                   UNION +
036400111108                   select rrn(TABEL00F) as Nrr, TABEL00F.*, +
036500111108                          substr(TBEuni, 4, 3) as Fls +
036600130531                     from ' + %trimr(wLibTABsede) + '/TABEL00F +
036700130531                          inner join ' +
036800130531                              %trimr(wLibTABsede) + '/TNTBE00F +
036900111108                       on TBLcod = ''3C'' and TBEcod = ''3EW'' +
037000121119                      and substr(TBLuni, 79, 7) = substr(TBEke1, 2, 7) +
037100120816                    where substr(TBLuni, 44, 5) = ''ESWEB'' +
037200120816                      and substr(TBEuni, 33, 1) = '' '' ) +
037300111108                 select Nrr from TABEL';
037400110211
037500110211         // -?Ricevuta filiale dal chiamante?
037600110211         if  w_Fil > *zero;
037700120206           if  Not $FLS;
037800120206             wSQL += ' where substr( TBLkey, 1, 3 ) = ''' + w_Fil + '''';
037900120206           else;
038000120206             wSQL += ' where Fls = ''' + w_Fil + '''';
038100120206           endif;
038200110211         endif;
038300110210
038400110211         // -?Posizionamento / Ordinamento?
038500110211         Select;
038600110211
038700110211           // -?Per codice cliente?
038800110211           When  wOrdSfl = 0;
038900110211             if  SaveKEY <> *zero;
039000111108               if  w_Fil > *zero;
039100111108                 wSQL += ' and';
039200111108               else;
039300111108                 wSQL += ' where';
039400111108               endif;
039500111108               wSQL += ' TBLkey >= ''' + %editc( SaveKEY : 'X' ) + '''';
039600110211             endif;
039700110211             wSQL += ' order by TBLkey';
039800110211
039900110211           // -?Per ragione sociale cliente?
040000110211           When  wOrdSfl = 1;
040100110211             if  SaveRAG <> *blank;
040200111108               if  w_Fil > *zero;
040300111108                 wSQL += ' and';
040400111108               else;
040500111108                 wSQL += ' where';
040600111108               endif;
040700111108               wSQL += ' substr(TBLuni, 4, 20) >= ''' + SaveRAG + '''';
040800110211             endif;
040900120213             wSQL += ' order by substr(TBLuni, 4, 20), TBLkey';
041000120517
041100120517           // -?Per codice unificante?
041200120517           When  wOrdSfl = 2;
041300120517             if  SaveCKS <> *zero;
041400120517               if  w_Fil > *zero;
041500120517                 wSQL += ' and';
041600120517               else;
041700120517                 wSQL += ' where';
041800120517               endif;
041900120517               wSQL += ' substr(TBLuni, 79, 7) >= ''' +
042000120517                       %editc( SaveCKS : 'X' ) + '''';
042100120517             endif;
042200120517             wSQL += ' order by substr(TBLuni, 79, 7), TBLkey';
042300110211
042400120517           // -?Per supporto Cliente->BRT?
042500120517           When  wOrdSfl = 3;
042600110211             if  SaveCBA <> *blank;
042700111108               if  w_Fil > *zero;
042800111108                 wSQL += ' and';
042900111108               else;
043000111108                 wSQL += ' where';
043100111108               endif;
043200111108               wSQL += ' substr(TBLuni, 44, 5) >= ''' + SaveCBA + '''';
043300110211             endif;
043400120213             wSQL += ' order by substr(TBLuni, 44, 5), TBLkey';
043500110211
043600110211           // -?Per supporto Filiale/Serie Segnacollo?
043700120517           When  wOrdSfl = 4;
043800110211             if  SaveFLS <> *zero  or  SaveNRS <> *zero;
043900111108               if  w_Fil > *zero;
044000111108                 wSQL += ' and';
044100111108               else;
044200111108                 wSQL += ' where';
044300111108               endif;
044400120213               wSQL +=     ' Fls >= ''' +
044500111108                             %editc( SaveFLS : 'X' ) + ''' +
044600110211                         and substr(TBLuni, 1, 2) >= ''' +
044700110211                             %editc( SaveNRS : 'X' ) + '''';
044800110211             endif;
044900120213             wSQL += ' order by Fls, substr(TBLuni, 1, 2), TBLkey';
045000111108
045100111108           // -?Per supporto Serie/Filiale Segnacollo?
045200120517           When  wOrdSfl = 5;
045300111108             if  SaveNRS2 <> *zero  or  SaveFLS2 <> *zero;
045400111108               if  w_Fil > *zero;
045500111108                 wSQL += ' and';
045600111108               else;
045700111108                 wSQL += ' where';
045800111108               endif;
045900111108               wSQL +=     ' substr(TBLuni, 1, 2) >= ''' +
046000111108                             %editc( SaveNRS2 : 'X' ) + ''' +
046100120213                         and Fls >= ''' +
046200111108                             %editc( SaveFLS2 : 'X' ) + '''';
046300111108             endif;
046400120213             wSQL += ' order by substr(TBLuni, 1, 2), Fls, TBLkey';
046500110211
046600110211         EndSl;
046700110210
046800110210         wSQL += ' for fetch only +
046900110211                  optimize for ' + %editc( c_SflPag : 'X' ) + ' rows';
047000110210
047100110210       ENDSR;
047200110210
047300110210       //--------------------------------------------------------------
047400110210       //?Apertura cursore C1.                                         ?
047500110210       //--------------------------------------------------------------
047600110210       BEGSR  sr_OpenCursor;
047700110210
047800110210         // -?Dichiarazione cursore?
047900110210         exec sql   prepare S1   from :wSQL;
048000110210         exec sql   declare C1   cursor for S1;
048100110210
048200110210         // -?Apertura del cursore?
048300110210         exec sql   open C1;
048400110210
048500110210         if  SQLcode < *zero;
048600110210           $Err = *on;
048700110210           $EoF = *on;
048800110210           exsr  sr_PrintErrSQL;
048900110210         endif;
049000110210
049100110210       ENDSR;
049200110210
049300110210       //--------------------------------------------------------------
049400110210       //?Chiusura cursore C1.                                         ?
049500110210       //--------------------------------------------------------------
049600110210       BEGSR  sr_CloseCursor;
049700110210
049800110210         // -?Chiusura del cursore?
049900110210         exec sql   close C1;
050000110210
050100110210       ENDSR;
050200110210
050300110210       //--------------------------------------------------------------
050400110210       //?Lettura cursore C1.                                          ?
050500110210       //--------------------------------------------------------------
050600110210       BEGSR  sr_ReadCursor;
050700110210
050800110211         clear  $FetchRow;
050900140422         w_FetchPag += 1;
051000110210
051100110211         // -?Reperimento dei 16 (c_SflPag) Rrn che comporranno la pag.?
051200110211         exec sql   fetch next   from C1   for 16 rows
051300110210                    into  :$FetchRow;
051400110210
051500110210         select;
051600110210           when  SQLcode = 100;
051700110210             $EoF = *on;
051800110210           when  SQLcode < *zero;
051900110210             $Err = *on;
052000110211             $EoF = *on;
052100110210             exsr  sr_PrintErrSQL;
052200110210         endsl;
052300110210
052400110210       ENDSR;
052500110210
052600110210       //--------------------------------------------------------------
052700110210       //?Gestione subfile S01 - Subfile con elenco tab. "3C".         ?
052800110210       //--------------------------------------------------------------
052900110210       BEGSR  sr_GesS01;
053000110210
053100110210         // -?Inizializzazione subfile?
053200110210         if  $InzS01  and  Not ErrGenerico;
053300110210           exsr  sr_InzS01;
053400110210           $InzS01 = *off;
053500110210         endif;
053600110210
053700110210         // -?Emissione Testata e Piede con tasti funzionali abilitati?
053800110210         write  TB28T01;
053900110210         write  TB28P01;
054000110210
054100110210         // -?Posizionamento cursore?
054200110210         if  C1CsrRrn > *zero;
054300110210           C1RcdNbr = C1CsrRrn;
054400110210         else;
054500110210           C1RcdNbr = 1;
054600110211           //write  TB28S00;             //(no rec.)?
054700110210         endif;
054800110210
054900110210         // -?Emissione videata?
055000110210         exfmt  TB28C01;
055100110210
055200110210         reset  ErrMessage;
055300110210         reset  ErrGenerico;
055400110210         clear  V1Dmsg;
055500110210
055600110210         SELECT;
055700110210
055800110210           // -?F3=Fine?
055900110210           WHEN  dsp_aid = c_F03;
056000110210             exsr  sr_CloseCursor;
056100110210             $Fine = *on;
056200110210
056300110210           // -?F5=Aggiornamento?
056400110210           WHEN  dsp_aid = c_F05;
056500110210             exsr  sr_CloseCursor;
056600110210             reset  $InzS01;
056700110210
056800110210           // -?F8=Ordinamento Sfl?
056900110210           WHEN  dsp_aid = c_F08;
057000110210             exsr  sr_F08S01;
057100110210
057200110210           // -?F12=Ritorno?
057300110210           WHEN  dsp_aid = c_F12;
057400110210             exsr  sr_CloseCursor;
057500110211             clear  kpjbu;
057600110210             $Fine = *on;
057700110210
057800110210           // -?Roll-Up?
057900110210           WHEN  dsp_aid = c_RollUp;
058000110210             exsr sr_RollUpS01;
058100110210
058200110210           // -?Roll-Down?
058300110210           WHEN  dsp_aid = c_RollDown;
058400110210             exsr sr_RollDownS01;
058500110210
058600110210           // -?SubFile vuoto?
058700110210           WHEN  S01nrr = *zero;
058800110210
058900110210           // -?Invio?
059000110210           OTHER;
059100110210             // -?Gestione opzioni?
059200110210             exsr  sr_OpzS01;
059300110210             if  ErrGenerico;
059400110210               leavesr;
059500110210             endif;
059600110211             // -?Nuovo posizionamento?
059700120517             if  C01key  <> SaveKEY   or
059800120517                 C01rag  <> SaveRAG   or
059900120517                 C01cks  <> SaveCKS   or
060000120517                 C01cba  <> SaveCBA   or
060100120517                (C01fls  <> SaveFLS   or  C01nrs <> SaveNRS)  or
060200111108                (C01nrs2 <> SaveNRS2  or  C01fls2 <> SaveFLS2);
060300110211               exsr  sr_CloseCursor;
060400110211               reset  $InzS01;
060500120517               SaveKEY  = C01key;
060600120517               SaveRAG  = C01rag;
060700120517               SaveCKS  = C01cks;
060800120517               SaveCBA  = C01cba;
060900120517               SaveFLS  = C01fls;
061000120517               SaveNRS  = C01nrs;
061100111108               SaveNRS2 = C01nrs2;
061200111108               SaveFLS2 = C01fls2;
061300110211             endif;
061400110210
061500110210         ENDSL;
061600110210
061700110210       ENDSR;
061800110210
061900110210       //--------------------------------------------------------------
062000110210       //?Inizializzazione subfile S01.                                ?
062100110210       //--------------------------------------------------------------
062200110210       BEGSR  sr_InzS01;
062300110210
062400110211         // -?La pulizia del subfile verrà eseguita nella subr.?
062500110211         //  ?"sr_WrtS01", eseguita nella subr. "sr_RollUpS01"?
062600110211
062700110211         // -?Pulizia subfile-control?
062800110211         clear  TB28C01;
062900110211
063000120517         C01key  = SaveKEY;
063100120517         C01rag  = SaveRAG;
063200120517         C01cks  = SaveCKS;
063300120517         C01cba  = SaveCBA;
063400120517         C01fls  = SaveFLS;
063500120517         C01nrs  = SaveNRS;
063600111108         C01nrs2 = SaveNRS2;
063700111108         C01fls2 = SaveFLS2;
063800110210
063900110211         $EoF = *off;
064000110210         $Err = *off;
064100110210         clear  w_CurrPag;
064200110211         clear  w_LastPag;
064300140422         clear  w_FetchPag;
064400110210
064500110210         // -?Preparazione stringa SQL?
064600110210         exsr  sr_PrepSQL;
064700110210
064800110210         // -?Caricamento della 1ª pagina di dati del subfile?
064900110210         exsr  sr_OpenCursor;
065000110211         exsr  sr_ReadCursor;
065100110210         exsr  sr_RollUpS01;
065200110210
065300110210         // -?Impaginazione subfile?
065400110210         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
065500110210         if S01nrr > *zero;
065600110210           C1RcdNbr  = 1;
065700110210           C1CsrRrn  = 1;
065800110210         else;
065900110210           clear C1RcdNbr;
066000110210         endif;
066100110210
066200110210         // -?Abilitazione CONDIZIONATE tasti funzionali in P01 (per C01)?
066300110210         F8Attivo  = (S01nrr > *zero);
066400110210
066500110211         // -?Visualizzazione richiesta di posizionamento?
066600110211         //  ?& Impostazione F8 nel piede del subfile?
066700120220         ProtectFLS = *off;
066800110211         select;
066900110211           // -?SubFile vuoto...?
067000110211           when  S01nrr = *zero;
067100110211             PosizKEY = *off;
067200110211             PosizRAG = *off;
067300120517             PosizCKS = *off;
067400110211             PosizCBA = *off;
067500111108             PosizFLS = *off;
067600110211             PosizNRS = *off;
067700110211             clear  V1PF08;
067800110211           // -?Già ordinato per codice cliente?
067900110210           when  wOrdSfl = 0;
068000110211             PosizKEY = *on;
068100110211             PosizRAG = *off;
068200120517             PosizCKS = *off;
068300110211             PosizCBA = *off;
068400111108             PosizFLS = *off;
068500110211             PosizNRS = *off;
068600110211             V1PF08   = 'F8=Ord. x Ragione Sociale';
068700110211           // -?Già ordinato per ragione sociale?
068800110210           when  wOrdSfl = 1;
068900110211             PosizKEY = *off;
069000110211             PosizRAG = *on;
069100120517             PosizCKS = *off;
069200110211             PosizCBA = *off;
069300111108             PosizFLS = *off;
069400110211             PosizNRS = *off;
069500120517             V1PF08   = 'F8=Ord. x Cod. Unificante';
069600120517           // -?Già ordinato per codice unificante?
069700120517           when  wOrdSfl = 2;
069800120517             PosizKEY = *off;
069900120517             PosizRAG = *off;
070000120517             PosizCKS = *on;
070100120517             PosizCBA = *off;
070200120517             PosizFLS = *off;
070300120517             PosizNRS = *off;
070400120517             V1PF08   = 'F8=Ord. x Supp. Cli->BRT ';
070500120517           // -?Già ordinato per supporto cliente => BRT?
070600120517           when  wOrdSfl = 3;
070700110211             PosizKEY = *off;
070800110211             PosizRAG = *off;
070900120517             PosizCKS = *off;
071000110211             PosizCBA = *on;
071100111108             PosizFLS = *off;
071200110211             PosizNRS = *off;
071300110211             V1PF08   = 'F8=Ord. x Filiale/Serie  ';
071400110211           // -?Già ordinato per filiale/serie segnacollo?
071500120517           when  wOrdSfl = 4;
071600110211             PosizKEY = *off;
071700110211             PosizRAG = *off;
071800120517             PosizCKS = *off;
071900110211             PosizCBA = *off;
072000111108             PosizFLS = *on;
072100111108             PosizNRS = *off;
072200111108             V1PF08   = 'F8=Ord. x Serie/Filiale  ';
072300120220             ProtectFLS = $FLS;
072400111108           // -?Già ordinato per serie/filiale segnacollo?
072500120517           when  wOrdSfl = 5;
072600111108             PosizKEY = *off;
072700111108             PosizRAG = *off;
072800120517             PosizCKS = *off;
072900111108             PosizCBA = *off;
073000111108             PosizFLS = *off;
073100111108             PosizNRS = *on;
073200111108             V1PF08   = 'F8=Ord. x Codice Cliente ';
073300120220             ProtectFLS = $FLS;
073400110210         endsl;
073500110210
073600110210       ENDSR;
073700110211
073800110211       //--------------------------------------------------------------
073900110211       //?Pagina del SubFile in avanti.                                ?
074000110211       //--------------------------------------------------------------
074100110211       BEGSR  sr_RollUpS01;
074200110211
074300110211         if  w_CurrPag = w_LastPag;
074400110211           w_CurrPag += 1;
074500110211           exsr  sr_NuovaPagina;
074600110211         else;
074700110211           w_CurrPag += 1;
074800110211         endif;
074900110211
075000110211         exsr  sr_WrtS01;
075100110211
075200110211       ENDSR;
075300110211
075400110211       //--------------------------------------------------------------
075500110211       //?Pagina del SubFile indietro.                                 ?
075600110211       //--------------------------------------------------------------
075700110211       BEGSR  sr_RollDownS01;
075800110211
075900110211         if  w_CurrPag > 1;
076000110211           w_CurrPag -= 1;
076100110211           exsr  sr_WrtS01;
076200110211         else;
076300110211           ErrRoll = *on;
076400110211         endif;
076500110211
076600110211
076700110211       ENDSR;
076800110210
076900110210       //--------------------------------------------------------------
077000110210       //?Caricamento singola pagina in memoria.                       ?
077100110210       //--------------------------------------------------------------
077200110210       BEGSR  sr_NuovaPagina;
077300110210
077400110211         // -?Memorizzazione n° dell'ultima pagina già letta?
077500110210         w_LastPag = w_CurrPag;
077600110210
077700110211         // -?Memorizzazione, nella schiera $Row, dei RRN del file?
077800110211         //  ?con cui verranno scritte le righe della pagina?
077900110210         clear  $Row;
078000110210         For  xR = 1  To  SqlEr3;
078100110210           %occur( $FetchRow ) = xR;
078200110210           $Row(xR) = wTBLrrn;
078300110210         EndFor;
078400110210
078500110210         // -?Allocazione della memoria necessaria per memorizzare?
078600110210         //  ?il nuovo elemento?
078700110210         nSize = %size($Pag) * w_CurrPag;
078800110210         if  w_CurrPag = 1;
078900110210           pPag = %alloc( nSize );
079000110210         else;
079100110210           pPag = %realloc( pPag : nSize );
079200110210         endif;
079300110210
079400110210         // -?Memorizzazione della pagina?
079500110210         $Pag(w_CurrPag) = dsRowPag;
079600110210
079700110210         // -?Fermo il caricamento di nuove pagine:?
079800110210         //  ?se i records sono finiti, oppure?
079900110210         //  ?se ho raggiunto la capienza massima?
080000110210         if  SqlEr5 = 100  or  w_CurrPag = %elem($Pag);
080100140422           $EoF = *on;
080200110210           leavesr;
080300110210         endif;
080400110210
080500110210       ENDSR;
080600110210
080700110210       //--------------------------------------------------------------
080800110210       //?Caricamento singola pagina nel subfile S01.                  ?
080900110210       //--------------------------------------------------------------
081000110211       BEGSR  sr_WrtS01;
081100110210
081200110210         // -?Spegnimento degli indicatori di errore?
081300110210         %subst(IndDspF : 50) = *off;
081400110210
081500110210         // -?Pulizia subfile?
081600110210         SflDsp_N    = *on;
081700110210         SflDspCtl_N = *on;
081800110210         write  TB28C01;
081900110210         SflDspCtl_N = *off;
082000110210         SflEnd      = *off;
082100110211         SflStart    = *off;
082200110210
082300110210         clear  C1RcdNbr;
082400110210         clear  C1CsrRrn;
082500110210         clear  S01nrr;
082600110210         clear  V1Dmsg;
082700110210         ErrMessage  = *off;
082800110210         ErrGenerico = *off;
082900110210
083000110210         SflNxtChg = *off;
083100110210
083200110211         // -?Reperimento dei Rrn che compongono la pagina?
083300110210         dsRowPag = $Pag(w_CurrPag);
083400110210
083500110210         // -?Ciclo di caricamento dati nel subfile?
083600110211         xR = 1;
083700110211         DoW  xR      <= c_SflPag  and
083800110211              $Row(xR) > *zero;
083900110210
084000110210           clear  TB28S01;
084100110210
084200110210           // -?Caricamento del singolo record nel subfile S01?
084300110211           chain  $Row(xR)  TABEL;
084400110210           // - -?Campi di Output?
084500110211           if  %found(TABEL00F);
084600111104             S01flg = TBLflg;
084700110211             S01cod = TBLkey;
084800110211             ds3C   = TBLuni;
084900110211             S01des = §3Crag;
085000110211             S01cks = §3Ccks;
085100110211             S01cba = §3Ccba;
085200110211             S01fls = §3Cfls;
085300110211             S01nrs = §3Cnrs;
085400110211           endif;
085500110211
085600110211           if  §3Cnrs > *zero;
085700110211             exsr  sr_Tab3CP3EW;
085800110211           endif;
085900120206
086000120517           // - -?Campi Hidden?
086100120206           S1Hds3C = ds3C;
086200120517
086300120517           // - -?Attributi di visualizzazione?
086400120517           PosizKEY = (wOrdSfl = 0);
086500120517           PosizRAG = (wOrdSfl = 1);
086600120517           PosizCKS = (wOrdSfl = 2);
086700120517           PosizCBA = (wOrdSfl = 3);
086800120517           PosizFLS = (wOrdSfl = 4);
086900120517           PosizNRS = (wOrdSfl = 5);
087000110210
087100110210           // -?Scrittura record nel subfile S01?
087200110211           S01nrr = xR;
087300110210           write  TB28S01;
087400110211
087500110211           xR += 1;
087600110210
087700110210         EndDo;
087800110210
087900110210         // -?Visualizzazione del SFL (se ci sono dati)?
088000110210         SflDsp_N = (S01nrr <= *zero);
088100110210
088200110210         // -?Attivazione del SFLEND?
088300110211         SflEnd   = ( ($EoF  and  w_CurrPag = w_LastPag)  or
088400110211                       $Err  or  $Row(c_SflPag) = *zero );
088500110211         SflStart = (w_CurrPag <= 1);
088600110210
088700110210         // -?Posizionamento cursore al 1° rcd della pagina?
088800110210         if  S01nrr > *zero;
088900110210           C1RcdNbr = 1;
089000110210         else;
089100110210           clear  C1RcdNbr;
089200110210           clear  w_CurrPag;
089300110210         endif;
089400110210
089500110210         C1CsrRrn = C1RcdNbr;
089600110211
089700110211         // -?Lettura pagina di records successivi?
089800110211         //  ?ed eventuale attivazione del SflEnd?
089900110211         //  ?(se non ce ne fossero più...)?
090000110211         //  ?- SE NON già chiesto un RollDown -?
090100110211         //  ?(nel qual caso i dati sarebbero già stati estratti)?
090200140422         if  Not SflEnd  and  w_CurrPag = w_LastPag  and
090300140422             w_FetchPag = w_LastPag;
090400110211           exsr  sr_ReadCursor;
090500140422           %occur( $FetchRow ) = 1;
090600140422           SflEnd   = ( $EoF  or  $Err  or  wTBLrrn = *zero );
090700110211         endif;
090800110210
090900110210       ENDSR;
091000110211
091100110211       //--------------------------------------------------------------
091200110211       //?Recupero range segnacolli per clienti da tab. "3CP" o "3EW". ?
091300110211       //--------------------------------------------------------------
091400110211       BEGSR  sr_Tab3CP3EW;
091500110211
091600110211         // -?Cliente con EasyWEB?
091700110211         If  §3Ccba = c_EasyWeb;
091800110211
091900110211           clear  d3EW;
092000110211
092100110211           // -?Reperimento codice completo del padre?
092200110211           clear  TIVSSds;
092300110211           clear  rqsSUN;
092400110211           rqsKSU = '0' + %editc( §3Ccks : 'X' );
092500110211           clear  rpyEsito;
092600110211           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
092700110211                                    rqsSUN : *zero : TIVSSds :
092800110211                                    rpyEsito );
092900110211
093000110211           if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
093100110211
093200110211             // -?Reperimento segnacolli da tab. "3EW"?
093300110211             k_TBEcod = '3EW';
093400110211             k_TBEke1 = '0' + %editc( §3Ccks : 'X' );
093500110211             setll  %kds( k05tntbe01 : 2 )  TNTBE000;
093600110211             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
093700110211             dow  Not %eof(TNTBE01L);
093800120816               d3EW_Temp = TBEuni;
093900120816               if  (TBEatb = *blank   and   d3EW_Temp.§3EWfaa = *blank)
094000120816                   or   TBLflg <> *blank;
094100110211                 if  TBEke1 = TIVSSds.VSSksu   and
094200110211                     TBEke2 = TIVSSds.VSSsun;
094300110211                   d3EW = TBEuni;
094400120816                   if  §3EWfaa = *blank;
094500120816                     leave;
094600120816                   endif;
094700110211                 endif;
094800110211               endif;
094900110211               reade  %kds( k05tntbe01 : 2 )  TNTBE000;
095000110211             enddo;
095100110211
095200110211           endif;
095300110211
095400120816           //if  §3EWfls <> *zero  and  §3EWfaa = *blank;
095500120816             §3Cfls = §3EWfls;
095600120816             S01fls = §3EWfls;
095700120816             S01nsi = §3EWnsi;
095800120816             S01nsf = §3EWnsf;
095900120816           //endif;
096000110211
096100110211         // -?Cliente senza EasyWEB?
096200110211         Else;
096300110211
096400110211           clear  w3CPnsi;
096500110211           clear  d3CP;
096600110211
096700110211           // -?Reperimento segnacolli da tab. "3CP"?
096800110211           k_TBEcod = '3CP';
096900110211           k_TBEke1 = TBLkey;
097000110211           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
097100110211           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
097200110211           dow  Not %eof(TNTBE01L);
097300110211             if  %subst( TBEke2 : 1 : 2 ) = %editc( §3Cnrs : 'X' ) AND
097400110211               ( (TBLflg = '*'      and   TBEatb = 'A')   or
097500110211                 (TBLflg = *blank   and   TBEatb = *blank) );
097600110211               d3CP = TBEuni;
097700110211               w3CPnsi = %int( %subst( TBEke2 : 3 : 7 ) );
097800110211               leave;
097900110211             endif;
098000110211             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
098100110211           enddo;
098200110211
098300110211           S01nsi = w3CPnsi;
098400110211           S01nsf = §3CPal;
098500110211
098600110211         EndIf;
098700110211
098800110211         if  S01nsi = *zero  and  S01nsf = *zero;
098900110211           S01nsi = 1;
099000110211           S01nsf = *hival;
099100110211         endif;
099200110211
099300110211       ENDSR;
099400110211
099500110211       //--------------------------------------------------------------
099600110211       //?Gestione tasto funzionale F8 = Ordinamento subfile.          ?
099700110211       //--------------------------------------------------------------
099800110211       BEGSR  sr_F08S01;
099900110211
100000110211         exsr  sr_CloseCursor;
100100110211
100200110211         reset  $InzS01;
100300110211         clear  SaveKEY;
100400110211         clear  SaveRAG;
100500110211         clear  SaveCBA;
100600120220         if  Not  $FLS;
100700120220           clear  SaveFLS;
100800120220         endif;
100900110211         clear  SaveNRS;
101000111108         clear  SaveNRS2;
101100120220         if  Not  $FLS;
101200120220           clear  SaveFLS2;
101300120220         endif;
101400110211         clear  C01key;
101500110211         clear  C01rag;
101600110211         clear  C01cba;
101700120220         if  Not  $FLS;
101800120220           clear  C01fls;
101900120220         endif;
102000110211         clear  C01nrs;
102100111108         clear  C01nrs2;
102200120220         if  Not  $FLS;
102300120220           clear  C01fls2;
102400120220         endif;
102500110211
102600120517         if  wOrdSfl < 5;
102700110211           wOrdSfl += 1;
102800110211         else;
102900110211           clear  wOrdSfl;
103000110211         endif;
103100110211
103200110211       ENDSR;
103300110210
103400110210       //--------------------------------------------------------------
103500110210       //?Gestione opzioni del subfile S01.                            ?
103600110210       //--------------------------------------------------------------
103700110210       BEGSR  sr_OpzS01;
103800110210
103900110210         clear  SavS01csr;
104000110210
104100110210         // -?Ciclo di lettura subfile?
104200110211         readc  TB28S01;
104300110210
104400110211         DoW  Not %eof(TRTB28D0);
104500110210
104600110210           SflNxtChg = *off;
104700110210           %subst(IndDspF : 50) = *off;
104800110210           C1RcdNbr = S01nrr;
104900110210
105000110210           select;
105100110210
105200110210             // -?1=Selezione?
105300110211             when  S01opz = '1';
105400110210               exsr  sr_Opz01_S01;
105500110210
105600110210             // -?Nessuna opzione?
105700110211             when  S01opz = *blank;
105800110210
105900110210             // -?Opzione errata?
106000110210             other;
106100110210               ErrMessage  = *on;
106200110210               ErrGenerico = *on;
106300110210               PosCurOPZ   = *on;
106400110210               V1Dmsg = $Msg(01);
106500110210
106600110210           endsl;
106700110210
106800110210           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
106900110210           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
107000110211           if  S01opz  <> *blank   and
107100110210              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
107200110210             SavS01csr   = C1RcdNbr;
107300110210           endif;
107400110210
107500110210           // -?Aggiornamento sfl?
107600110210           select;
107700110210             when  ErrMessage;
107800110210               SflNxtChg = *on;
107900110210               C1CsrRrn  = C1RcdNbr;
108000110210             when  ErrGenerico;
108100110210               C1CsrRrn  = C1RcdNbr;
108200110211               clear  S01opz;
108300110210             other;
108400110211               clear  S01opz;
108500110210           endsl;
108600110211           UPDATE  TB28S01;
108700110210           if  ErrGenerico   or   ErrMessage;
108800110210             leave;
108900110210           endif;
109000110210
109100110211           readc  TB28S01;
109200110210
109300110210         ENDDO;
109400110210
109500110210         // -?Riposiziono il cursore sul record contenente la prima opz.?
109600110210         //  ?(se non sono stati rilevati errori)?
109700110210         if  NOT ErrMessage   and   NOT ErrGenerico   and
109800110210             SavS01csr > *zero;
109900110210           C1CsrRrn = SavS01csr;
110000110210         endif;
110100110210
110200110210       ENDSR;
110300110210
110400110210       //--------------------------------------------------------------
110500110210       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
110600110210       //--------------------------------------------------------------
110700110210       BEGSR  sr_PrintErrSQL;
110800110210
110900110210         // -?Stampa del Dump?
111000110210         Dump(A);
111100110210
111200110210         // -?Stampa del Job-Log?
111300110210         Qcmd = 'DSPJOBLOG job(*) output(*print)';
111400110210         exsr  sr_ExecCmd;
111500110210
111600110210         // -?Chiusura del programma?
111700110210         exsr  sr_RoutEnd;
111800110210
111900110210       ENDSR;
112000110210
112100110210       //--------------------------------------------------------------
112200110210       //?Esecuzione del comando (già impostato).                      ?
112300110210       //--------------------------------------------------------------
112400110210       BEGSR  sr_ExecCmd;
112500110210
112600110210         clear Qcap0100;
112700110210         Qcabcsdh = *off;
112800110210         Qcapa    = *off;
112900110210         Qcacmdss = *off;
113000110210         Qcaerved = *allX'00';
113100110210
113200110210         clear Qusec;
113300110210         Qusbprv  = %size(Qusec);
113400110210
113500110210         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
113600110210                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
113700110210                           0 : 0 : Qusec);
113800110210
113900110210         // -?Invio *msg per segnalazione errore?
114000110210         //if  Qusei <> *blank;
114100110210         //  ...;
114200110210         //endif;
114300110210
114400110210       ENDSR;
114500110210
114600110210       //--------------------------------------------------------------
114700110210       //?Selezione singolo record.                                    ?
114800110210       //--------------------------------------------------------------
114900110210       BEGSR  sr_Opz01_S01;
115000110210
115100110210         ErrGenerico = *on;
115200110211         kpjbu = S01cod + S1HDS3C;
115300110210         $Fine = *on;
115400110210
115500110210       ENDSR;
115600110210
115700110210       //--------------------------------------------------------------
115800110210       //?Operazioni finali.                                           ?
115900110210       //--------------------------------------------------------------
116000110210       BEGSR  sr_RoutEnd;
116100110210
116200110210         // -?Uscita dal *pgm?
116300110210         return;
116400110210
116500110210       ENDSR;
116600110210
116700110210      /end-free
116800110210
116900110210       //--------------------------------------------------------------
117000110210       //?Schiere a tempo di compilazione.                             ?
117100110210       //--------------------------------------------------------------
117200110210
117300110210** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
117400110210Opzione errata                                                                 1
