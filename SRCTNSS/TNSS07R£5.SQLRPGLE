000100110628       //==============================================================
000200110628       //?Statistica spedizioni EasySped/EasySpedWeb - batch           ?
000300110628       //==============================================================
000400110628
000500110628       //--------------------------------------------------------------
000600110628       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110628       //--------------------------------------------------------------
000800110628
000900110628     /*PRM dbgview(*source)
001000110628     /*END
001100110628
001200110628       //--------------------------------------------------------------
001300110628       //?Specifiche di controllo.                                     ?
001400110628       //--------------------------------------------------------------
001500110628
001600110628     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110628     h dftactgrp(*no)
001800110628     h alwnull(*inputonly)
001900110628
002000110628       //--------------------------------------------------------------
002100110628       //?Dichiarazione file.                                          ?
002200110628       //--------------------------------------------------------------
002300110628
002400110628       // -?Organigramma filiale/aziendale?
002500110628     fAZORG01L  if   e           k disk
002600110628
002700110628       // -?Tabelle?
002800110628     fTABEL00F  if   e           k disk
002900140416
003000140416       // -?Contratti Comodato
003100140416     fUncmd01l  if   e           k disk
003200110628
003300110628       // -?Work-File di dettaglio?
003400110628     fWFES210F  Uf A e             disk    usropn
003500110628
003600110628       // -?Work-File di totale?
003700110628     fWFES220F  O    e             disk    usropn
003800110628
003900110628       //--------------------------------------------------------------
004000110628       //?Definizione costanti.                                        ?
004100110628       //--------------------------------------------------------------
004200110628
004300110628       // -?Comandi per pulizia Work-Files?
004400110628     d c_Cmd_ClrWF1    c                   const('clrpfm file(*libl/+
004500110628     d                                                   WFES210F)')
004600110628     d c_Cmd_ClrWF2    c                   const('clrpfm file(*libl/+
004700110628     d                                                   WFES220F)')
004800110628
004900110628       // -?Supporti Cliente => BRT gestiti?
005000110628     d c_SuppCB_ESYSP  c                   const('ESYSP')
005100110628     d c_SuppCB_ESVAL  c                   const('ESVAL')
005200110628     d c_SuppCB_ESWEB  c                   const('ESWEB')
005300110628
005400110628       //--------------------------------------------------------------
005500110628       //?Definizione schiere.                                         ?
005600110628       //--------------------------------------------------------------
005700110628
005800130110     d c_MaxElem       c                   const(25000)
005900110628       // -?Clienti in "3C" con famiglia?
006000140416     d*$Cli            s              7a   dim(c_MaxElem)    inz
006100110628       // -?Padri   in "3C" con famiglia?
006200140416     d*$Padre          s              7a   dim(c_MaxElem)    inz
006300110628       // -?Stampanti in comodato (da tab. "3R") x ogni cliente in "3C"?
006400140416     d*$StpComodato    s                   like(ds3R.§3Rnot)
006500140416     d*                                    dim(c_MaxElem)  inz
006600140416       // -?Codici padre legame ST dei clienti esaminati
006700140416     d $padst          s                   like(d10cod)
006800140416     d                                     dim(c_MaxElem)  inz
006900140416       // -?Flag di presenza materiale in comodato per famiglia ST
007000140416     d $com            s              1
007100140416     d                                     dim(c_MaxElem)  inz
007200110628
007300110628       //--------------------------------------------------------------
007400110628       //?Definizione aree dati.                                       ?
007500110628       //--------------------------------------------------------------
007600110628
007700110628       // -?Dati utente?
007800110628     d §AzUte        e ds                  extname(AZUTE00F)
007900110628     d                                     dtaara
008000110628     d §DatiUte      e ds                  extname(dDatiUte)
008100110628     d                                     dtaara
008200110628
008300110628       //--------------------------------------------------------------
008400110628       //?Definizione strutture dati.                                  ?
008500110628       //--------------------------------------------------------------
008600110628
008700110628       // -?Status ds?
008800110628     d Status         sds
008900110628     d  SDSpgm           *proc
009000110628     d  SDSprm           *parms
009100110628
009200110628       // -?Parametri ricevuti?
009300110628     d KPJBA         e ds
009400110628     d TNSS07ds      e ds                  inz
009500110628     d   D07dti      e                     inz(*loval)
009600110628     d   D07dtf      e                     inz(*hival)
009700110628
009800110628       // -?Tab. "3CE" = Versioni EasySped per cliente?
009900110628     d d3CE          e ds                  inz         qualified
010000110628       // -?Tab. "3CP" = Serie parziali assegnate ai clienti NON ESWEB?
010100110628     d d3CP          e ds                  inz         qualified
010200110628     d   §3CPal      e                     inz(*hival)
010300110628       // -?Tab. "3EW" = Dati assegnati alla postazione EasySpedWeb?
010400110628     d d3EW          e ds                  inz         qualified
010500110628
010600110628       // -?Tab. "3C"  = Invio dati -bollettazione-?
010700110628     d ds3C          e ds                  inz         qualified
010800110628       // -?Tab. "3R"  = Scheda clienti per scambio dati?
010900110628     d ds3R          e ds                  inz         qualified
011000110628       // -?Tab. "05"  = Aree?
011100110628     d ds05          e ds                  inz         qualified
011200110628       // -?Tab. "17"  = Distretti?
011300110628     d ds17          e ds                  inz         qualified
011400110628
011500110628       // -?Strutture dati "di comodo"?
011600110628       //  ?(per la definizione dei dati da estrarre)?
011700110628     d TIVSSds       e ds                  extname(TIVSS00F)
011800110628     d                                     inz         qualified
011900110628     d TNTBEds       e ds                  extname(TNTBE00F)
012000110628     d                                     inz         qualified
012100110628     d TITASds       e ds                  extname(TITAS00F)
012200110628     d                                     inz         qualified
012300110628     d CNACOds_Uni   e ds                  extname(CNACO00F)
012400110628     d                                     inz         qualified
012500110628
012600110628       // -?Dati estratti via SQL:?
012700110628       // - -?dal file TABEL00F per tab. "3C"?
012800110628     d wSQL_ds_3C      ds                  inz         qualified
012900110628     d   sqlTBLkey                         inz  like(TBLkey)
013000110628     d   sqlTBLuni                         inz  like(TBLuni)
013100110630       // - -?dal file TNTBE00F per tab. "3CP"?
013200110630     d wSQL_ds_3CP     ds                  inz         qualified
013300110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
013400110630     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
013500110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
013600110630       // - -?dal file TNTBE00F per tab. "3CE"?
013700110630     d wSQL_ds_3CE     ds                  inz         qualified
013800110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
013900110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
014000110628       // - -?dal file TNTBE00F per tab. "3EW"?
014100110628     d wSQL_ds_3EW     ds                  inz         qualified
014200110628     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
014300110628     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
014400110628     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
014500110630       // - -?dal file TIVSS00F se VSSISV = "EW"?
014600110630     d wSQL_ds_TIVSS   ds                  inz         qualified
014700110630     d   sqlVSSksu                         inz    like(TIVSSds.VSSksu)
014800110630     d   sqlVSSsun                         inz    like(TIVSSds.VSSsun)
014900110630     d   sqlVSSisv                         inz    like(TIVSSds.VSSisv)
015000110630     d*//sqlVSSdsc                         inz    like(TIVSSds.VSSdsc)
015100110628
015200110628       // -?Dati estratti via SQL:?
015300110628       // - -?dal file TITAS33C x CCM/AAS/MGS?
015400110628     d wSQL_ds_TITAS   ds                  inz         qualified
015500110628     d   sqlTASccm                         inz    like(TITASds.TASccm)
015600110628     d*//sqlTASaas                         inz    like(TITASds.TASaas)
015700110628     d*//sqlTASmgs                         inz    like(TITASds.TASmgs)
015800110628     d*//sqlTASnrs                         inz    like(TITASds.TASnrs)
015900110628     d   sqlTASdts                    8  0 inz
016000110628     d   sqlTOTspe                    9  0 inz
016100110628     d   sqlTOTcol                    9  0 inz
016200110628     d   sqlMAXncd                    9  0 inz
016300110628     d   sqlMAXnca                    9  0 inz
016400110628
016500110628       // -?Dati estratti via SQL:?
016600110628       // - -?dal file WFES210F x aggiornamento info EasySped?
016700110628     d wSQL_ds_WFES21  ds                  inz         qualified
016800110628     d  sqlWESccu                          inz    like(WE21ccu)
016900110628     d  sqlWESflgu                         inz    like(WE21flgu)
017000110628     d  sqlWESedat                         inz    like(WE21edat)
017100110628     d  sqlWESever                         inz    like(WE21ever)
017200110628     d  sqlWEScver                         inz    like(WE21cver)
017300110628     d  sqlWEScdsc                         inz    like(WE21cdsc)
017400110628     d  sqlWEStypw                         inz    like(WE21typw)
017500110628     d  sqlWEScverMax                      inz    like(WE21cverMx)
017600110628     d  sqlWEScdscMax                      inz    like(WE21cdscMx)
017700110628     d  sqlWESesweb                        inz    like(WE21esweb)
017800110628     d  sqlWEScverAgg                      inz    like(WE21cverag)
017900110628     d  SQLrrn                        8  0 inz
018000110628
018100110628       // -?Salvataggio dati (del padre) estratti in wSQL_WFES21?
018200110628     d wEsySp_ds       ds                  inz
018300110628     d  ESYwesccu                          like(wSQL_ds_WFES21.sqlWESccu)
018400110628     d                                     inz
018500110628     d  ESYwesflgu                         like(wSQL_ds_WFES21.sqlWESflgu)
018600110628     d                                     inz
018700110628     d  ESYwesedat                         like(wSQL_ds_WFES21.sqlWESedat)
018800110628     d                                     inz
018900110628     d  ESYwesever                         like(wSQL_ds_WFES21.sqlWESever)
019000110628     d                                     inz
019100110628     d  ESYwescver                         like(wSQL_ds_WFES21.sqlWEScver)
019200110628     d                                     inz
019300110628     d  ESYwescdsc                         like(wSQL_ds_WFES21.sqlWEScdsc)
019400110628     d                                     inz
019500110628     d  ESYwestypw                         like(wSQL_ds_WFES21.sqlWEStypw)
019600110628     d                                     inz
019700110628     d  ESYwescverMax                      like(wSQL_ds_WFES21.sqlWEScverMax)
019800110628     d                                     inz
019900110628     d  ESYwescdscMax                      like(wSQL_ds_WFES21.sqlWEScdscMax)
020000110628     d                                     inz
020100110628     d  ESYwesesweb                        like(wSQL_ds_WFES21.sqlWESesweb)
020200110628     d                                     inz
020300110628     d  ESYwescverAgg                      like(wSQL_ds_WFES21.sqlWEScverAgg)
020400110628     d                                     inz
020500110628
020600110628       // -?Dati estratti via SQL:?
020700110628       // - -?dal file WFES210F x totalizzazione in WFES220F?
020800110628     d WFES210ds     e ds                  extname(WFES210F)
020900110628     d                                     inz         qualified
021000110628
021100140416     d tibs10ds      e ds
021200140416     d skc                           11    DIM(500) overlay(d10skc)
021300110628       //--------------------------------------------------------------
021400110628       //?Definizione variabili globali.                               ?
021500110628       //--------------------------------------------------------------
021600110628
021700110628       // -?Flags booleani?
021800110628     d $EoF_1          s               n   inz
021900110628     d $EoF_2          s               n   inz
022000110628     d $EoF_3          s               n   inz
022100110628     d $EoF_4          s               n   inz
022200110628     d $EoF_5          s               n   inz
022300110628     d $EoF_6          s               n   inz
022400110628
022500110628       // -?Stringhe SQL da eseguire?
022600110628     d wSql            s           5000    inz  varying
022700110628     d wSql_4          s           5000    inz  varying
022800110628
022900110628       // -?Indici di schiera?
023000140416     d*xx              s              5  0 inz
023100110628     d yy              s              5  0 inz
023200140416     d i               s              5  0 inz
023300110628
023400110628       // -?Campi di comodo?
023500110628     d wDate           s              8  0 inz
023600110628     d wNTS            s              9  0 inz
023700110630     d SAVcdi          s                   inz(*loval) like(ORGfl3)
023800110628     d SAVcar          s                   inz(*loval) like(WE21car)
023900110628     d SAVccu          s                   inz(*loval) like(WE21ccu)
024000110628     d wSgnClI         s                   inz(1)      like(WE21nsi)
024100110630     d wSupporto       s                   inz         like(WE21scb)
024200140416     d wcom            s              1    inz
024300140416     d $COMODATO       s             21    inz
024400140416     d kcli            s                   like(cmdksc)
024500110628
024600110628       //--------------------------------------------------------------
024700110628       //?Definizione prototipi procedure.                             ?
024800110628       //--------------------------------------------------------------
024900110628
025000110628       // -?Reperimento dati utente?
025100110628     d TIBS34ds      e ds                  inz
025200110628      /copy gaitrasrc/srcProtoPr,TIBS34R
025300110628
025400110628       // -?Controllo/Decodifica cliente?
025500110628      /copy gaitrasrc/srcProtoPI,TIBS69R
025600110628      /copy gaitrasrc/srcProtoPR,TIBS69R
025700140416
025800140416       // -?ricerca cliente unificante
025900140416      /copy gaitrasrc/srcProtoPR,TIBS10R
026000110628
026100110628       // -?Parametri API QCAPCMD (Process Commands)?
026200110628     d Qcmd            s           2048    inz  varying
026300110628      /copy qSysInc/qRpgleSrc,QCAPCMD
026400110628       // -?API QCAPCMD (Process Commands)?
026500110628      /copy gaitrasrc/srcProtoPR,QCAPCMD
026600110628
026700110628       // -?Parametri gestione errori API.?
026800110628      /copy qsysinc/qrpglesrc,QUSEC
026900110628
027000110628       //--------------------------------------------------------------
027100110628       //?Definizione key-list.                                        ?
027200110628       //--------------------------------------------------------------
027300110628
027400110628       // -?File TABEL00F?
027500110628     d k03tabel00    e ds                  extname(TABEL00F : *key)
027600110628     d                                     prefix(k_)   inz
027700110628       // -?File AZORG01L?
027800110628     d k01azorg01    e ds                  extname(AZORG01L : *key)
027900110628     d                                     prefix(k_)   inz
028000110628
028100110628       //--------------------------------------------------------------
028200110628       //?M A I N - L I N E                                            ?
028300110628       //--------------------------------------------------------------
028400110628
028500110628     c     *Entry        plist
028600110628     c                   parm                    KPJBA
028700110628
028800110628      /free
028900110628
029000110628       // -?Operazioni iniziali?
029100110628       exsr  sr_RoutInz;
029200110628
029300110628       // -?1° ciclo di elaborazione: creazione WFES210F (dettaglio)?
029400110628       //                            ?per clienti EasySped?
029500110628       exsr  sr_OpenCursor_1;
029600110628
029700110628       DoU  $Eof_1 = *on;
029800110628         exsr  sr_ReadCursor_1;
029900110628       EndDo;
030000110628
030100110628       exsr  sr_CloseCursor_1;
030200110628
030300110628       // -?2° ciclo di elaborazione: creazione WFES210F (dettaglio)?
030400110628       //                            ?per clienti EasySpedWeb?
030500110628       exsr  sr_OpenCursor_2;
030600110628
030700110628       DoU  $Eof_2 = *on;
030800110628         exsr  sr_ReadCursor_2;
030900110628       EndDo;
031000110628
031100110628       exsr  sr_CloseCursor_2;
031200110628
031300110628       // -?Aggiornamento dati EasySped/EasySpedWeb mancanti...?
031400110628       exsr  sr_Upd_EsySp;
031500110628
031600110628       // -?3° ciclo di elaborazione: creazione WFES220F (totale area)?
031700110628       exsr  sr_OpenCursor_3;
031800110628
031900110628       dou  $EoF_3 = *on;
032000110628         exsr  sr_ReadCursor_3;
032100110628       enddo;
032200110628
032300110628       exsr  sr_CloseCursor_3;
032400110628
032500110628       // -?Fine programma?
032600110628       exsr  sr_RoutEnd;
032700110628
032800110628       //--------------------------------------------------------------
032900110628       //?Operazioni iniziali.                                         ?
033000110628       //--------------------------------------------------------------
033100110628       BEGSR  sr_RoutInz;
033200110628
033300110628         *inLR = *on;
033400110628
033500110628         // -?Impostazione opzioni per SQL?
033600110628         exec sql  set  option  DynUsrPrf = *Owner,
033700110628                                CloSqlCsr = *EndMod;
033800110628
033900110628         // -?Reperimento dati job?
034000110628         exsr  sr_DatiJob;
034100110628
034200110628         // -?Impostazione campi "fissi"?
034300110628         k_TBLkut = 1;
034400110628
034500110628         // -?Reperimento data odierna in formato aaaammgg?
034600110628         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
034700110628         //wData_Iso = %date(wDate : *iso);
034800110628
034900110628         // -?Impostazione eventuali parametri ricevuti?
035000110628         if  SDSprm > *zero  and
035100110628             KPJBU <> *blank;
035200110628           TNSS07ds = KPJBU;
035300110628         else;
035400110628           reset  TNSS07ds;
035500110628         endif;
035600110628
035700110628         // -?Pulizia work-files del lavoro sottomesso e loro apertura?
035800110628         Qcmd = c_Cmd_ClrWF1;
035900110628         exsr  sr_ExecCmd;
036000110628         if  Qusei <> *blank;
036100110628           exsr  sr_PrintErr;
036200110628         endif;
036300110628         open  WFES210F;
036400110628         Qcmd = c_Cmd_ClrWF2;
036500110628         exsr  sr_ExecCmd;
036600110628         if  Qusei <> *blank;
036700110628           exsr  sr_PrintErr;
036800110628         endif;
036900110628         open  WFES220F;
037000110628
037100140416         //clear  xx;
037200140416         //clear  $Cli;
037300140416         //clear  $Padre;
037400140416         //clear  $StpComodato;
037500110628
037600110628         // -?Intabellamento clienti e relativa famiglia da "3C"?
037700140416         //k_TBLcod = '3C';
037800140416         //setll  %kds(k03tabel00 : 2)  TABEL;
037900140416         //reade  %kds(k03tabel00 : 2)  TABEL;
038000140416         //DoW  Not  %eof(TABEL00F);
038100140416         //  if  TBLflg = *blank;
038200140416         //    xx += 1;
038300140416         //    ds3C       = TBLuni;
038400140416         //    $Cli(xx)   = %trimr( TBLkey );
038500140416         //    $Padre(xx) = %editc( ds3C.§3Ccks : 'X' );
038600140416         //  endif;
038700140416         //reade  %kds(k03tabel00 : 2)  TABEL;
038800140416         //EndDo;
038900110628
039000110628         // -?Intabellamento stampanti in comodato per cliente da "3R"?
039100140416         //k_TBLcod = '3R';
039200140416         //For  yy = 1  To  xx;
039300140416         //  k_TBLkey = $Cli(yy);
039400140416         //  chain  %kds(k03tabel00)  TABEL;
039500140416         //  if  %found(TABEL00F)   and   TBLflg = *blank;
039600140416         //    ds3R = TBLuni;
039700140416         //    $StpComodato(yy) = ds3R.§3Rnot;
039800140416         //  endif;
039900140416         //EndFor;
040000110628
040100110628       ENDSR;
040200110628
040300110628       //--------------------------------------------------------------
040400110628       //?Reperimento Dati del job (Utente/Operativi).                 ?
040500110628       //--------------------------------------------------------------
040600110628       BEGSR  sr_DatiJob;
040700110628
040800110628         in(E) §AzUte;
040900110628         if NOT %error;
041000110628           in(E) §DatiUte;
041100110628         endif;
041200110628         if  %error  or  RSut = *blank;
041300110628           clear TIBS34ds;
041400110628           tibs34r ( tibs34ds );
041500110628           in §AzUte;
041600110628           in §DatiUte;
041700110628         endif;
041800110628
041900110628       ENDSR;
042000110628
042100110628       //--------------------------------------------------------------
042200110628       //?Apertura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
042300110628       //?  estrazione clienti EasySped.                               ?
042400110628       //--------------------------------------------------------------
042500110628       BEGSR  sr_OpenCursor_1;
042600110628
042700110628         // -?Preparazione stringa SQL?
042800110628         wSql = 'with +
042900110628
043000110628                 Tab_3C as +
043100110628                     ( select TBLkey, TBLuni +
043200110628                         from TABEL00F +
043300110628                        where TBLflg = '' '' +
043400110628                          and TBLcod = ''3C'' +
043500110628                          and substr(TBLuni,  1, 2) > ''00'' +
043600110628                          and substr(TBLuni, 44, 5) in +
043700110628                              (''' + c_SuppCB_ESYSP + ''', +
043800110628                               ''' + c_SuppCB_ESVAL + ''') ), +
043900110628
044000110628                 Tab_3CP as +
044100110628                     ( select TBEke1, TBEke2, TBEuni +
044200110628                         from TNTBE00F +
044300110628                        where TBEatb = '' '' +
044400110628                          and TBEcod = ''3CP'' +
044500110628                          and TBEsif = '' '' +
044600110629                          and TBElin = '' '' ), +
044700110628
044800110628                 Tab_3CE as +
044900110628                     ( select TBEke1, TBEuni +
045000110628                         from TNTBE00F +
045100110628                        where TBEatb = '' '' +
045200110628                          and TBEcod = ''3CE'' +
045300110628                          and TBEke2 = '' '' +
045400110628                          and TBEsif = '' '' +
045500110628                          and TBElin = '' '' ) +
045600110628
045700110630                 select Tab_3C.*, +
045800110630                        case when Tab_3CP.TBEke1 > '' '' +
045900110630                             then Tab_3CP.TBEke1 else '' '' end, +
046000110630                        case when Tab_3CP.TBEke2 > '' '' +
046100110630                             then Tab_3CP.TBEke2 else '' '' end, +
046200110630                        case when Tab_3CP.TBEuni > '' '' +
046300110630                             then Tab_3CP.TBEuni else '' '' end, +
046400110630                        case when Tab_3CE.TBEke1 > '' '' +
046500110630                             then Tab_3CE.TBEke1 else '' '' end, +
046600110630                        case when Tab_3CE.TBEuni > '' '' +
046700110630                             then Tab_3CE.TBEuni else '' '' end  +
046800110628
046900110628                   from Tab_3C left outer join Tab_3CP +
047000110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
047100110628                        substr(Tab_3CP.TBEke1, 1, 7) and +
047200110628                        substr(Tab_3C.TBLuni, 1, 2) = +
047300110628                        substr(Tab_3CP.TBEke2, 1, 2) +
047400110628
047500110628                               left outer join Tab_3CE +
047600110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
047700110630                        substr(Tab_3CE.TBEke1, 1, 7)';
047800110628
047900110628         // -?Dichiarazione cursore?
048000110628         exec sql  prepare S1  from :wSql;
048100110628         exec sql  declare C1  cursor for S1;
048200110628
048300110628         // -?Apertura del cursore?
048400110628         exec sql  open C1;
048500110628
048600110628         if  SQLcode < *zero;
048700110628           $EoF_1 = *on;
048800110628           exsr  sr_PrintErr;
048900110628         endif;
049000110628
049100110628       ENDSR;
049200110628
049300110628       //--------------------------------------------------------------
049400110628       //?Lettura cursore C1 (tab. "3C", "3CP" e "3CE") per            ?
049500110628       //?  estrazione clienti EasySped.                               ?
049600110628       //--------------------------------------------------------------
049700110628       BEGSR  sr_ReadCursor_1;
049800110628
049900110628         clear  ds3C;
050000110630         reset  d3CP;
050100110628         clear  d3CE;
050200110630         reset  wSgnClI;
050300110628         clear  wSQL_ds_3C;
050400110628         clear  wSQL_ds_3CP;
050500110628         clear  wSQL_ds_3CE;
050600110628
050700110628         // -?Lettura cursore?
050800110628         exec sql  fetch next  from C1  into :wSQL_ds_3C,
050900110628                                             :wSQL_ds_3CP,
051000110628                                             :wSQL_ds_3CE;
051100110628
051200110628         select;
051300110628           // -?Fine flusso?
051400110628           when  SQLcode = 100;
051500110628             $EoF_1 = *on;
051600110628           // -?Errore?
051700110628           when  SQLcode < *zero;
051800110628             exsr  sr_PrintErr;
051900110628             $EoF_1 = *on;
052000110628           // -?Elaborazione dati estratti?
052100110628           other;
052200110628             exsr  sr_Elab_1;
052300110628         endsl;
052400110628
052500110628       ENDSR;
052600110628
052700110628       //--------------------------------------------------------------
052800110628       //?Chiusura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
052900110628       //?  estrazione clienti EasySped.                               ?
053000110628       //--------------------------------------------------------------
053100110628       BEGSR  sr_CloseCursor_1;
053200110628
053300110628         // -?Chiusura del cursore?
053400110628         exec sql  close C1;
053500110628
053600110628       ENDSR;
053700110628
053800110628       //--------------------------------------------------------------
053900110628       //?Elaborazione singolo cliente estratto (EasySped).            ?
054000110628       //--------------------------------------------------------------
054100110628       BEGSR  sr_Elab_1;
054200110628
054300110628         // -?Valorizzazione tab. "3C"?
054400110628         ds3C = wSQL_ds_3C.sqlTBLuni;
054500110628
054600110628         // -?Valorizzazione tab. "3CP"?
054700110628         if  wSQL_ds_3CP.sqlTBEke1 > *zero  and
054800110628             wSQL_ds_3CP.sqlTBEke2 > *zero;
054900110628           Monitor;
055000110628             wSgnClI = %dec( %subst( wSQL_ds_3CP.sqlTBEke2 : 3 : 7 )
055100110628                             : 7 : 0 );
055200110628             On-Error  105;
055300110628               reset  wSgnClI;
055400110628               reset  d3CP;
055500110628           EndMon;
055600110628           if  wSQL_ds_3CP.sqlTBEuni > *zero;
055700110628             d3CP = wSQL_ds_3CP.sqlTBEuni;
055800110628           endif;
055900110628         endif;
056000110628
056100110628         // -?Valorizzazione tab. "3CE"?
056200110628         if  wSQL_ds_3CE.sqlTBEke1 > *blank;
056300110628           d3CE = wSQL_ds_3CE.sqlTBEuni;
056400110628         endif;
056500110628
056600110628         // -?Pulizia iniziale del record da registrare?
056700110628         clear  WFES2100;
056800110628
056900110628
057000110628         // -?Reperimento Distretto ed Area?
057100110628         //  ?(a "rottura" di filiale del cliente unificante)?
057200110628         k_ORGfil = %int( %subst( %editc( ds3C.§3Ccks : 'X' ) : 1 : 3 ) );
057300110628         if  k_ORGfil <> ORGfil;
057400110628           chain  %kds(k01azorg01)  AZORG;
057500110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
057600110628             clear  ORGfl3;
057700110628             clear  ORGcar;
057800110628             clear  ds17;
057900110628             clear  ds05;
058000110628           endif;
058100110628         endif;
058200110628
058300110628         // -?Decodifica distretto?
058400110628         if  ORGfl3 <> SAVcdi;
058500110628           SAVcdi = ORGfl3;
058600110628           clear  ds17;
058700110628           k_TBLkut = 1;
058800110628           k_TBLcod = '17';
058900110628           k_TBLkey = ORGfl3;
059000110628           chain  %kds(k03tabel00)  TABEL;
059100110628           if  %found(TABEL00F)  and  TBLflg = *blank;
059200110628             ds17 = TBLuni;
059300110628           endif;
059400110628         endif;
059500110628
059600110628         // -?Decodifica area?
059700110628         if  ORGcar <> SAVcar;
059800110628           SAVcar = ORGcar;
059900110628           clear  ds05;
060000110628           k_TBLkut = 1;
060100110628           k_TBLcod = '05';
060200110628           k_TBLkey = %editc( ORGcar : 'X' );
060300110628           chain  %kds(k03tabel00)  TABEL;
060400110628           if  %found(TABEL00F)  and  TBLflg = *blank;
060500110628             ds05 = TBLuni;
060600110628           endif;
060700110628         endif;
060800110628
060900110628         // -?Decodifica unificante?
061000110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
061100110628           clear  TIBS69ds;
061200110628           I69sif = knsif;
061300110628           I69kcc = DUTkci;
061400110628           I69kac = ds3C.§3Ccks;
061500110628           tibs69r( tibs69ds :
061600110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
061700110628           if  O69err = *on;
061800110628             clear  CNACOds_Uni;
061900110628           else;
062000110628             CNACOds_Uni = ds_CNACO;
062100110628           endif;
062200110628         EndIf;
062300110628
062400110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
062500110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
062600110628           clear  TIBS69ds;
062700110628           I69sif = knsif;
062800110628           I69kcc = DUTkci;
062900110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
063000110628           tibs69r( tibs69ds :
063100110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
063200110628           if  O69err = *on;
063300110628             clear  ds_CNACO;
063400110628           endif;
063500110628         EndIf;
063600110628
063700140416         // -?Reperimento materiale in comodato file UNCMD00F ?
063800140416         exsr  sr_StampCom;
063900110628
064000110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
064100110628         // -?(reperimento dati da spedizioni associate)?
064200110628         $Eof_4 = *off;
064300110628         exsr  sr_OpenCursor_4;
064400110628
064500110628         DoU  $Eof_4;
064600110628           exsr  sr_ReadCursor_4;
064700110628         EndDo;
064800110628
064900110628         exsr  sr_CloseCursor_4;
065000110628
065100110628         // -?Scrittura del record a totale per cliente?
065200110629         wSupporto = ds3C.§3Ccba;
065300110628         exsr  sr_Wrt_WFES210;
065400110628
065500110628       ENDSR;
065600110628
065700110628       //--------------------------------------------------------------
065800110628       //?Apertura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
065900110628       //?  estrazione clienti EasySpedWeb.                            ?
066000110628       //--------------------------------------------------------------
066100110628       BEGSR  sr_OpenCursor_2;
066200110628
066300110628         // -?Preparazione stringa SQL?
066400110628         wSql = 'with +
066500110628
066600110628                 TIVSS as +
066700110628                     ( select VSSksu, VSSsun, VSSisv +
066800110628                         from TIVSS00F +
066900110628                        where VSSisv = ''EW'' and +
067000110628                             (VSSdsc = 0 or VSSdsc > ' +
067100110628                              %char(wDate) + ') ), +
067200110628
067300110628                 Tab_3EW as +
067400110628                     ( select TBEke1, TBEke2, TBEuni +
067500110628                         from TNTBE00F +
067600110628                        where TBEatb = '' '' and +
067700120814                              TBEcod = ''3EW'' and +
067800120814                              substr(TBEuni, 33, 1) = '' '' ), +
067900110628
068000110628                 Tab_3C as +
068100110628                     ( select TBLkey, TBLuni +
068200110628                         from TABEL00F +
068300110628                        where TBLflg = '' '' and +
068400110628                              TBLcod = ''3C'' and +
068500110628                              substr(TBLuni, 44, 5) = +
068600110630                              ''' + c_SuppCB_ESWEB + ''') +
068700110628
068800110628                 select * +
068900110628
069000110628                   from TIVSS inner join Tab_3EW +
069100110628                     on TIVSS.VSSksu = Tab_3EW.TBEke1  and +
069200110628                        TIVSS.VSSsun = Tab_3EW.TBEke2 +
069300110628
069400110628                              inner join Tab_3C +
069500110628                     on substr(TIVSS.VSSksu, 2, 7) = +
069600110628                        substr(Tab_3C.TBLuni, 79, 7)';
069700110628
069800110628         // -?Dichiarazione cursore?
069900110628         exec sql  prepare S2  from :wSql;
070000110628         exec sql  declare C2  cursor for S2;
070100110628
070200110628         // -?Apertura del cursore?
070300110628         exec sql  open C2;
070400110628
070500110628         if  SQLcode < *zero;
070600110628           $EoF_2 = *on;
070700110628           exsr  sr_PrintErr;
070800110628         endif;
070900110628
071000110628       ENDSR;
071100110628
071200110628       //--------------------------------------------------------------
071300110628       //?Lettura cursore C2 (file TIVSS, tab. "3EW" e tab. "3C") per  ?
071400110628       //?  estrazione clienti EasySpedWeb.                            ?
071500110628       //--------------------------------------------------------------
071600110628       BEGSR  sr_ReadCursor_2;
071700110628
071800110628         clear  d3EW;
071900110628         clear  ds3C;
072000110628         clear  wSQL_ds_TIVSS;
072100110628         clear  wSQL_ds_3EW;
072200110628         clear  wSQL_ds_3C;
072300110628
072400110628         // -?Lettura cursore?
072500110628         exec sql  fetch next  from C2  into :wSQL_ds_TIVSS,
072600110628                                             :wSQL_ds_3EW,
072700110628                                             :wSQL_ds_3C;
072800110628
072900110628         select;
073000110628           // -?Fine flusso?
073100110628           when  SQLcode = 100;
073200110628             $EoF_2 = *on;
073300110628           // -?Errore?
073400110628           when  SQLcode < *zero;
073500110628             exsr  sr_PrintErr;
073600110628             $EoF_2 = *on;
073700110628           // -?Elaborazione dati estratti?
073800110628           other;
073900110628             exsr  sr_Elab_2;
074000110628         endsl;
074100110628
074200110628       ENDSR;
074300110628
074400110628       //--------------------------------------------------------------
074500110628       //?Chiusura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
074600110628       //?  estrazione clienti EasySpedWeb.                            ?
074700110628       //--------------------------------------------------------------
074800110628       BEGSR  sr_CloseCursor_2;
074900110628
075000110628         // -?Chiusura del cursore?
075100110628         exec sql  close C2;
075200110628
075300110628       ENDSR;
075400110628
075500110628       //--------------------------------------------------------------
075600110629       //?Elaborazione singolo cliente estratto (EasySpedWeb).         ?
075700110628       //--------------------------------------------------------------
075800110628       BEGSR  sr_Elab_2;
075900110628
076000110628         d3EW = wSQL_ds_3EW.sqlTBEuni;
076100110628         ds3C = wSQL_ds_3C.sqlTBLuni;
076200110628
076300110630         // -?Pulizia iniziale del record da registrare?
076400110628         clear  WFES2100;
076500110628
076600110628         // -?Reperimento Area?
076700110628         //  ?(a "rottura" di filiale del cliente unificante)?
076800110628         k_ORGfil = %int( %subst( wSQL_ds_TIVSS.sqlVSSksu : 2 : 3 ) );
076900110628         if  k_ORGfil <> ORGfil;
077000110628           chain  %kds(k01azorg01)  AZORG;
077100110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
077200110629             clear  ORGfl3;
077300110628             clear  ORGcar;
077400110628           endif;
077500110628         endif;
077600110629
077700110629         // -?Decodifica distretto?
077800110629         if  ORGfl3 <> SAVcdi;
077900110629           SAVcdi = ORGfl3;
078000110629           clear  ds17;
078100110629           k_TBLkut = 1;
078200110629           k_TBLcod = '17';
078300110629           k_TBLkey = ORGfl3;
078400110629           chain  %kds(k03tabel00)  TABEL;
078500110629           if  %found(TABEL00F)  and  TBLflg = *blank;
078600110629             ds17 = TBLuni;
078700110629           endif;
078800110629         endif;
078900110628
079000110628         // -?Decodifica area?
079100110628         if  ORGcar <> SAVcar;
079200110628           SAVcar = ORGcar;
079300110628           clear  ds05;
079400110628           k_TBLkut = 1;
079500110628           k_TBLcod = '05';
079600110628           k_TBLkey = %editc( ORGcar : 'X' );
079700110628           chain  %kds(k03tabel00)  TABEL;
079800110628           if  %found(TABEL00F)  and  TBLflg = *blank;
079900110628             ds05 = TBLuni;
080000110628           endif;
080100110628         endif;
080200110628
080300110628         // -?Decodifica unificante?
080400110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
080500110628           clear  TIBS69ds;
080600110628           I69sif = knsif;
080700110628           I69kcc = DUTkci;
080800110628           I69kac = ds3C.§3Ccks;
080900110628           tibs69r( tibs69ds :
081000110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
081100110628           if  O69err = *on;
081200110628             clear  CNACOds_Uni;
081300110628           else;
081400110628             CNACOds_Uni = ds_CNACO;
081500110628           endif;
081600110628         EndIf;
081700110628
081800110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
081900110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
082000110628           clear  TIBS69ds;
082100110628           I69sif = knsif;
082200110628           I69kcc = DUTkci;
082300110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
082400110628           tibs69r( tibs69ds :
082500110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
082600110628           if  O69err = *on;
082700110628             clear  ds_CNACO;
082800110628           endif;
082900110628         EndIf;
083000110628
083100140416         // -?Reperimento materiale in comodato file UNCMD00F ?
083200140416         exsr  sr_StampCom ;
083300110628
083400110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
083500110628         // -?(reperimento dati da spedizioni associate)?
083600110628         $Eof_4 = *off;
083700110628         exsr  sr_OpenCursor_4;
083800110628
083900110628         DoU  $Eof_4;
084000110628           exsr  sr_ReadCursor_4;
084100110628         EndDo;
084200110628
084300110628         exsr  sr_CloseCursor_4;
084400110628
084500110628         // -?Scrittura del record a totale per cliente?
084600110629         wSupporto = ds3C.§3Ccba;
084700110628         exsr  sr_Wrt_WFES210;
084800110628
084900110628       ENDSR;
085000110628
085100110628       //--------------------------------------------------------------
085200110628       //?Apertura cursore C4 (totale spedizioni nel periodo x cliente)?
085300110628       //--------------------------------------------------------------
085400110628       BEGSR  sr_OpenCursor_4;
085500110628
085600110628         // -?Preparazione stringa SQL?
085700110628         //  ? N.B. ?- Seleziona le LdV del periodo richiesto?
085800110630         //  ?· Seleziona le LdV del cliente in esame da tab. "3C"?
085900110628         //  ?· Seleziona le LdV con la serie del cliente in esame?
086000110628         //  ?· Seleziona le LdV porto franco?
086100110628
086200110630         wSql_4 = 'select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
086300110630                          count(*), sum( TASncl ) +
086400110628                     from TITASP0F +
086500110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
086600110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
086700110628                          %editc( D07dti : 'X' ) + ' and ' +
086800110628                          %editc( D07dtf : 'X' ) +
086900110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
087000110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
087100110628                  ' group by TASccm +
087200110628
087300110628                   UNION +
087400110628
087500110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
087600110630                          count(*), sum( TASncl ) +
087700110628                     from TITAS10F +
087800110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
087900110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
088000110628                          %editc( D07dti : 'X' ) + ' and ' +
088100110628                          %editc( D07dtf : 'X' ) +
088200110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
088300110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
088400110628                  ' group by TASccm +
088500110628
088600110628                   UNION +
088700110628
088800110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
088900110630                          count(*), sum( TASncl ) +
089000110628                     from TITAS00F +
089100110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
089200110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
089300110628                          %editc( D07dti : 'X' ) + ' and ' +
089400110628                          %editc( D07dtf : 'X' ) +
089500110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
089600110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
089700110628                  ' group by TASccm +
089800110628
089900110628                      FOR FETCH ONLY';
090000110628
090100110628         // -?Dichiarazione cursore?
090200110628         exec sql  prepare S4  from :wSql_4;
090300110628         exec sql  declare C4  cursor for S4;
090400110628
090500110628         // -?Apertura del cursore?
090600110628         exec sql  open C4;
090700110628
090800110628         if  SQLcode < *zero;
090900110628           $EoF_4 = *on;
091000110628           exsr  sr_PrintErr;
091100110628         endif;
091200110628
091300110628       ENDSR;
091400110628
091500110628       //--------------------------------------------------------------
091600110628       //?Lettura cursore C4 (totale spedizioni nel periodo x cliente) ?
091700110628       //--------------------------------------------------------------
091800110628       BEGSR  sr_ReadCursor_4;
091900110628
092000110628         clear  wSQL_ds_TITAS;
092100110628
092200110628         // -?Lettura cursore?
092300110628         exec sql  fetch next  from C4  into :wSQL_ds_TITAS;
092400110628
092500110628         select;
092600110628           // -?Fine flusso?
092700110628           when  SQLcode = 100;
092800110628             $EoF_4 = *on;
092900110628           // -?Errore?
093000110628           when  SQLcode < *zero;
093100110628             $EoF_4 = *on;
093200110628             exsr  sr_PrintErr;
093300110628           // -?Elaborazione dati estratti?
093400110628           other;
093500110628             exsr  sr_Elab_4;
093600110628         endsl;
093700110628
093800110628       ENDSR;
093900110628
094000110628       //--------------------------------------------------------------
094100110628       //?Chiusura cursore C4 (totale spedizioni nel periodo x cliente)?
094200110628       //--------------------------------------------------------------
094300110628       BEGSR  sr_CloseCursor_4;
094400110628
094500110628         // -?Chiusura del cursore?
094600110628         exec sql  close C4;
094700110628
094800110628       ENDSR;
094900110628
095000110628       //--------------------------------------------------------------
095100110628       //?Elaborazione totale spedizioni nel periodo x CCM da TITAS33C ?
095200110628       //--------------------------------------------------------------
095300110628       BEGSR  sr_Elab_4;
095400110628
095500110628         // -?Conteggio numero spedizioni?
095600110628         WE21nts += wSQL_ds_TITAS.sqlTOTspe;
095700110628
095800110628         // -?Conteggio numero colli?
095900110628         WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
096000110628
096100110628         // -?Memorizzazione data ULTIMA spedizione?
096200110628         if  wSQL_ds_TITAS.sqlTASdts > WE21dus;
096300110628           WE21dus = wSQL_ds_TITAS.sqlTASdts;
096400110628         endif;
096500110628
096600110628       ENDSR;
096700110628
096800110628       //--------------------------------------------------------------
096900110628       //?Scrittura record in work-file WFES210F.                      ?
097000110628       //--------------------------------------------------------------
097100110628       BEGSR  sr_Wrt_WFES210;
097200110628
097300110628         // -?Dati nel trk del work-file WFES210F?
097400110628         // -?Parametri di lancio?
097500110628         WE21pdti  = D07dti;
097600110628         WE21pdtf  = D07dtf;
097700110628         WE21pcver = D07cver;
097800110628         WE21pcdsc = D07cdsc;
097900110628         // -?Distretto?
098000110628         WE21cdi = ORGfl3;
098100110628         WE21ddi = ds17.§17des;
098200110628         // -?Area?
098300110628         WE21car = ORGcar;
098400110628         WE21dar = ds05.§05des;
098500110628         // -?Cliente unificante?
098600110628         WE21ccu = ds3C.§3Ccks;
098700110628         WE21rsu = CNACOds_Uni.ACOrag;
098800110628         // -?Cliente mittente tab. "3C"?
098900110628         WE21ccm = %int( %triml( wSQL_ds_3C.sqlTBLkey ) );
099000140911         //WE21rsm = ds3C.§3Crag;
099100140911         WE21rsm = ACORag;
099200110628         WE21blo = ACOabl;
099300110628         // -?Supporto Cliente => BRT?
099400110628         WE21scb = wSupporto;
099500110628         if  wSupporto = c_SuppCB_ESWEB;
099600110628           // -?Dati dalla tab. "3EW"?
099700110628           WE21EWlnp = d3EW.§3EWlnp;
099800120321           WE21fls   = d3EW.§3EWfls;
099900110628           WE21nrs   = d3EW.§3EWnrs;
100000110628           WE21nsi   = d3EW.§3EWnsi;
100100110628           WE21nsf   = d3EW.§3EWnsf;
100200110628           WE21EWctm = d3EW.§3EWctm;
100300110628           WE21EWmad = d3EW.§3EWmad;
100400110628           WE21EWaew = d3EW.§3EWaew;
100500110628         else;
100600110628           // -?Dati dalla tab. "3C"?
100700120321           WE21fls   = ds3C.§3Cfls;
100800110628           WE21nrs   = ds3C.§3Cnrs;
100900110628           WE21nsi   = wSgnClI;
101000110628           WE21nsf   = d3CP.§3CPal;
101100110628         endif;
101200110630         // -?Totali (già calcolati in subr. "sr_Elab_4")?
101300110628         //WE21nts += wSQL_ds_TITAS.sqlTOTspe;
101400110628         //WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
101500110628         //WE21dus  = wSQL_ds_TITAS.sqlTASdts;
101600110706         // -?Dati EasySped (NON per "EasySped-Validate")?
101700110706         if  wSupporto <> c_SuppCB_ESVAL;
101800110706           WE21edat = d3CE.§3CEedat;
101900110706           WE21ever = d3CE.§3CEever;
102000110706           WE21cver = d3CE.§3CEcver;
102100110706           WE21cdsc = d3CE.§3CEcdsc;
102200110706           WE21typw = d3CE.§3CEtypweb;
102300110706           WE21cvermx = d3CE.§3CEcvermx;
102400110706           WE21cdscmx = d3CE.§3CEcdscmx;
102500110706         endif;
102600110630         // -?EasySpedWeb?
102700110706         select;
102800110706           when  wSupporto = c_SuppCB_ESWEB  or
102900110706                 wSupporto = c_SuppCB_ESVAL;
103000110706             WE21esweb = 'SI';
103100110706           when  WE21ever >= '4.0.3 ';
103200110706             WE21esweb = 'SI';
103300110706           other;
103400110706             WE21esweb = 'NO';
103500110706         endsl;
103600110630         // -?Cappario aggiornato?
103700110630         select;
103800110706           when  wSupporto = c_SuppCB_ESWEB;
103900110706             WE21cverag = 'SI';
104000110706           when  wSupporto = c_SuppCB_ESVAL;
104100110706             WE21cverag = 'NO';
104200110630           when  WE21cver   = *zero;
104300110630             WE21cverag = 'NO';
104400110630           when  WE21cver   = D07cver;
104500110630             WE21cverag = 'SI';
104600110630           when  WE21cverMx = D07cver;
104700110630             WE21cverag = 'SI';
104800120601           when  WE21cver   = (D07cver - 1)  and  WE21cdsc = D07cdsc;
104900120601             WE21cverag = 'SI';
105000110630           other;
105100110630             WE21cverag = 'NO';
105200110630         endsl;
105300140416         // -?Materiale in comodato?
105400140416         WE21stamco = $comodato  ;
105500110628
105600110628         //______________
105700110628         WRITE  WFES2100;
105800110628         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
105900110628
106000110628       ENDSR;
106100110628
106200110628       //--------------------------------------------------------------
106300110628       //?Reperimento stampanti in concordato da tab. "3R"             ?
106400110628       //--------------------------------------------------------------
106500140416       //BEGSR  sr_StampConc;
106600110628
106700140416       //  clear  ds3R;
106800110628
106900110628         // -?1° tentativo con il cliente in esame?
107000140416       //  clear  yy;
107100140416       //  yy = %lookup( %trimr( wSQL_ds_3C.sqlTBLkey ) : $Cli );
107200110628
107300110628         // -?2° tentativo con il padre del cliente in esame?
107400140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
107500140416       //    clear  yy;
107600140416       //    yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Cli );
107700140416       //  EndIf;
107800110628
107900110628         // -?3° tentativo con gli altri fratelli?
108000140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
108100140416       //    clear  yy;
108200140416       //    DoU  yy = *zero   or   $StpComodato(yy) <> *blank;
108300140416       //      yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Padre : yy + 1 );
108400140416       //    EndDo;
108500140416       //  EndIf;
108600110628
108700110628         // -?Imposta la stampante in ds3R (se reperita)?
108800140416       //  If  yy > *zero;
108900140416       //    ds3R.§3Rnot = $StpComodato(yy);
109000140416       //  EndIf;
109100140416       //--------------------------------------------------------------
109200140416       //?Verifica presenza comodati            ?
109300140416       //--------------------------------------------------------------
109400140416       BEGSR  sr_StampCom;
109500140415         clear $comodato;
109600140414         // -?Con il cliente in esame verifico legame "ST"
109700140414         //   Verifico se codice presente come padre
109800140414         clear tibs10ds;
109900140414         d10tle='ST';
110000140414         d10paf='F';
110100140415         d10cod=%dec(%trimr( wSQL_ds_3C.sqlTBLkey ):11:0);
110200140415         tibs10r(tibs10ds);
110300140415         if d10cop<=0;
110400140415         //   Se non è padre verifico se è figlio
110500140415            d10paf='P';
110600140415            tibs10r(tibs10ds);
110700140415            if d10cop<=0;
110800140415         //  Se non è nè padre nè figlio è cliente singolo
110900140415               d10cop=d10cod;
110901141016               skc(1)=%editc(d10cod:'X');
111000140415            endif;
111100140415         endif;
111200140416         // -?Verifico se il padre del legame ST è già stato analizzato per
111300140416         // -?i comodati, altrimenti lo analizzo e lo memorizzo in schiera
111400140416         // -?Schiera $padst (clienti padri legame ST)
111500140416         // -?Schiera $com   (indica se presente comodato S=Comodato)
111600140415         yy=%lookup(d10cop:$padst);
111700140415         if yy=0;
111800140415            yy=%lookup(0:$padst);
111900140415            $padst(yy)=d10cop      ;
112000140415         // Verifico se presente materiale in comodato nella famiglia e
112100140415         // memorizzo in schiera
112200140416            clear wcom;
112300140416            for i=1 to %elem(sKC);
112400140416               if skc(i)=*blanks or skc(i)=*zeros or wcom='S';
112500140415                  leave;
112600140415               endif;
112700140416               kcli=%dec(%subst(skc(I):5:7):7:0);
112800140415               exsr sr_comodato;
112900140416               if wcom = 'S';
113000140416                  $com(yy)='S';
113100140416               endif ;
113200140415            endfor;
113300140415         endif;
113400140415         if $com(yy)='S';
113500140415            $comodato='Materiale in comodato';
113600140415         endif;
113700110628       ENDSR;
113800110628
113900140416       //--------------------------------------------------------------
114000140416       //?Ricerca comodato per cliente                                 ?
114100140416       //--------------------------------------------------------------
114200140416       BEGSR  sr_Comodato;
114300140416       setll kcli uncmd01l;
114400140416       reade kcli uncmd01l;
114500140416       dow not %eof(uncmd01l) ;
114600140416          if cmdcfg<>'A' and cmdcfg<>'C';
114700140416             wcom='S';
114800140416             leave;
114900140416          endif;
115000140416          reade kcli uncmd01l;
115100140416       enddo;
115200140416       endsr;
115300110628       //--------------------------------------------------------------
115400110628       //?Apertura cursore C3 (creazione WFES220F con totali x area).  ?
115500110628       //--------------------------------------------------------------
115600110628       BEGSR  sr_OpenCursor_3;
115700110628
115800110628         // -?Dichiarazione cursore?
115900110628         exec sql  declare C3  cursor for
116000110628                   select *
116100110628                     from WFES210F
116200110628                    order by WE21car, WE21scb, WE21ccu, WE21ccm
116300110628                      for read only;
116400110628
116500110628         // -?Apertura cursore?
116600110628         exec sql  open C3;
116700110628
116800110628         clear  WFES2200;
116900110628
117000110628       ENDSR;
117100110628
117200110628       //--------------------------------------------------------------
117300110628       //?Lettura cursore C3 (creazione WFES220F con totali x area).
117400110628       //--------------------------------------------------------------
117500110628       BEGSR  sr_ReadCursor_3;
117600110628
117700110628         // -?Lettura cursore?
117800110628         exec sql  fetch  NEXT  from C3  into :WFES210ds;
117900110628
118000110628         select;
118100110628           when  SQLcode = 100;
118200110628             $EoF_3 = *on;
118300110628           when  SQLcode < *zero;
118400110628             $EoF_3 = *on;
118500110628             exsr  sr_PrintErr;
118600110628           other;
118700110628             exsr  sr_Elab_3;
118800110628         endsl;
118900110628
119000110628       ENDSR;
119100110628
119200110628       //--------------------------------------------------------------
119300110628       //?Creazione work-file WFES220F con totali per area.            ?
119400110628       //--------------------------------------------------------------
119500110628       BEGSR  sr_Elab_3;
119600110628
119700110628         // -?Rottura Area?
119800111122         IF  WFES210ds.WE21car <> WE22car;
119900110628
120000110628           // -?Scrittura record con i totali dell'area precedente?
120100110628           if  WE22car <> *zero;
120200111123             exsr  sr_Wrt_WFES220;
120300110628           endif;
120400110628
120500110628           // -?Impostazione dati nuova area?
120600110628           clear  WFES2200;
120700110628           WE22pdti  = WFES210ds.WE21pdti;
120800110628           WE22pdtf  = WFES210ds.WE21pdtf;
120900110628           WE22pcver = WFES210ds.WE21pcver;
121000110628           WE22pcdsc = WFES210ds.WE21pcdsc;
121100110628           WE22car   = WFES210ds.WE21car;
121200110628           WE22dar   = WFES210ds.WE21dar;
121300110628
121400110628         ENDIF;
121500110628
121600110628
121700110628         // -?Incremento totali dell'area in elaborazione?
121800111122         Select;
121900111122
122000111122           // -? EasySpeb ?
122100111123           When  WFES210ds.WE21scb = c_SuppCB_ESYSP;
122200111122             // -?Num. totale spedizioni nel periodo?
122300111122             WE22esnts  += WFES210ds.WE21nts;
122400111122             // -?Num. totale colli nel periodo?
122500111122             WE22esntc  += WFES210ds.WE21ntc;
122600111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
122700111122             if  WFES210ds.WE21stamco = *blank;
122800111122               WE22esntcn += WFES210ds.WE21ntc;
122900111122             endif;
123000111122             // -?Num. totale clienti padre?
123100111122             if  WFES210ds.WE21ccu <> SAVccu;
123200111122               SAVccu = WFES210ds.WE21ccu;
123300111122               WE22esnksu += 1;
123400111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
123500111122               exsr  sr_Elab_5;
123600111122               if  wNTS = *zero;
123700111122                 WE22esnuns += 1;
123800111122               endif;
123900111122             endif;
124000111122             // -?Num. totale clienti in tab. "3C"?
124100111122             WE22esncli += 1;
124200111123             //  ?(scartati quelli bloccati)?
124300111122             IF  WFES210ds.WE21blo = *blank;
124400111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
124500111122               If  WFES210ds.WE21nts > *zero;
124600111122                 WE22esncls += 1;
124700111122                 // ·?...ma SENZA cappario aggiornato?
124800111122                 if  WFES210ds.WE21cverag = 'NO';
124900111122                   WE22esncca += 1;
125000111122                 endif;
125100111122                 // ·?...ma SENZA EasySpedWeb?
125200111122                 if  WFES210ds.WE21esweb  = 'NO';
125300111122                   WE22esnclw += 1;
125400111122                 endif;
125500111122               EndIf;
125600111122             ENDIF;
125700111122
125800111122           // -? EasySpeb Validate ?
125900111123           When  WFES210ds.WE21scb = c_SuppCB_ESVAL;
126000111122             // -?Num. totale spedizioni nel periodo?
126100111122             WE22evnts  += WFES210ds.WE21nts;
126200111122             // -?Num. totale colli nel periodo?
126300111122             WE22evntc  += WFES210ds.WE21ntc;
126400111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
126500111122             if  WFES210ds.WE21stamco = *blank;
126600111122               WE22evntcn += WFES210ds.WE21ntc;
126700111122             endif;
126800111122             // -?Num. totale clienti padre?
126900111122             if  WFES210ds.WE21ccu <> SAVccu;
127000111122               SAVccu = WFES210ds.WE21ccu;
127100111122               WE22evnksu += 1;
127200111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
127300111122               exsr  sr_Elab_5;
127400111122               if  wNTS = *zero;
127500111122                 WE22evnuns += 1;
127600111122               endif;
127700111122             endif;
127800111122             // -?Num. totale clienti in tab. "3C"?
127900111122             WE22evncli += 1;
128000111123             //  ?(scartati quelli bloccati)?
128100111122             IF  WFES210ds.WE21blo = *blank;
128200111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
128300111122               If  WFES210ds.WE21nts > *zero;
128400111122                 WE22evncls += 1;
128500111122               EndIf;
128600111122             ENDIF;
128700111123
128800111123           // -? EasySpeb WEB ?
128900111123           When  WFES210ds.WE21scb = c_SuppCB_ESWEB;
129000111123             // -?Num. totale spedizioni nel periodo?
129100111123             WE22ewnts  += WFES210ds.WE21nts;
129200111123             // -?Num. totale colli nel periodo?
129300111123             WE22ewntc  += WFES210ds.WE21ntc;
129400111123             // -?Num. totale spedizioni e colli senza stampanti in comodato?
129500111123             if  WFES210ds.WE21stamco = *blank;
129600111123               WE22ewntcn += WFES210ds.WE21ntc;
129700111123             endif;
129800111123             // -?Num. totale clienti padre?
129900111123             if  WFES210ds.WE21ccu <> SAVccu;
130000111123               SAVccu = WFES210ds.WE21ccu;
130100111123               WE22ewnksu += 1;
130200111123               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
130300111123               exsr  sr_Elab_5;
130400111123               if  wNTS = *zero;
130500111123                 WE22ewnuns += 1;
130600111123               endif;
130700111123             endif;
130800111123             // -?Num. totale clienti in tab. "3C"?
130900111123             WE22ewncli += 1;
131000111123             //  ?(scartati quelli bloccati)?
131100111123             IF  WFES210ds.WE21blo = *blank;
131200111123               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
131300111123               If  WFES210ds.WE21nts > *zero;
131400111123                 WE22ewncls += 1;
131500111123               EndIf;
131600111123             ENDIF;
131700111122
131800111122         EndSl;
131900110628
132000110628       ENDSR;
132100110628
132200110628       //--------------------------------------------------------------
132300110630       //?Chiusura cursore C3 (creazione WFES220F con totali x area).
132400110628       //--------------------------------------------------------------
132500110628       BEGSR  sr_CloseCursor_3;
132600110628
132700110628         // -?Scrittura ultimo record?
132800110628         if  WE22car <> *zero;
132900111123           exsr  sr_Wrt_WFES220;
133000110628         endif;
133100110628
133200110628         // -?Chiusura cursore?
133300110628         exec sql  close C3;
133400110628
133500110628       ENDSR;
133600111123
133700111123       //--------------------------------------------------------------
133800111123       //?Scrittura record in work-file WFES220F.                      ?
133900111123       //--------------------------------------------------------------
134000111123       BEGSR  sr_Wrt_WFES220;
134100111123
134200111123         // -? Totali AREA ?
134300111123         // ·?Num. totale spedizioni nel periodo?
134400111123         WE22tants  = WE22esnts + WE22evnts + WE22ewnts;
134500111123         // ·?Num. totale colli nel periodo?
134600111123         WE22tantc  = WE22esntc + WE22evntc + WE22ewntc;
134700111123         // ·?Num. totale spedizioni e colli senza stampanti in comodato?
134800111123         WE22tantcn = WE22esntcn + WE22evntcn + WE22ewntcn;
134900111123         // ·?Num. totale clienti padre?
135000111123         WE22tanksu = WE22esnksu + WE22evnksu + WE22ewnksu;
135100111123         // ·?"Raggruppamento" clienti SENZA spedizioni nel periodo?
135200111123         WE22tanuns = WE22esnuns + WE22evnuns + WE22ewnuns;
135300111123         // ·?Num. totale clienti in tab. "3C"?
135400111123         WE22tancli = WE22esncli + WE22evncli + WE22ewncli;
135500111123         // ·?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
135600111123         //  ?(scartati quelli bloccati)?
135700111123         WE22tancls = WE22esncls + WE22evncls + WE22ewncls;
135800111123
135900111123         //______________
136000111123         WRITE  WFES2200;
136100111123         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
136200111123
136300111123       ENDSR;
136400110628
136500110628       //--------------------------------------------------------------
136600110628       //?Verifica se padre & figli senza spedizioni nel periodo.      ?
136700110628       //--------------------------------------------------------------
136800110628       BEGSR  sr_Elab_5;
136900110628
137000110628         // -?Dichiarazione cursore?
137100110628         exec sql  declare C5  cursor for
137200110628                    select sum(WE21nts)
137300110628                      from WFES210F
137400110628                     where WE21ccu = :WFES210ds.WE21ccu
137500110628                       for read only;
137600110628
137700110628         // -?Apertura cursore?
137800110628         exec sql  open C5;
137900110628
138000110628         // -?Lettura cursore?
138100110628         exec sql  fetch  NEXT  from C5  into :wNTS;
138200110628
138300110628         select;
138400110628           when  SQLcode = 100;
138500110628             $EoF_5 = *on;
138600110628           when  SQLcode < *zero;
138700110628             $EoF_5 = *on;
138800110628             exsr  sr_PrintErr;
138900110628           other;
139000111122             //if  wNTS = *zero;
139100111122             //  WE22nksuns += 1;
139200111122             //endif;
139300110628         endsl;
139400110628
139500110628         // -?Chiusura cursore?
139600110628         exec sql  close C5;
139700110628
139800110628       ENDSR;
139900110628
140000110628       //--------------------------------------------------------------
140100110628       //?Ultimo aggiornamento (dati di EasySped & relativo flag).     ?
140200110628       //--------------------------------------------------------------
140300110628       BEGSR  sr_Upd_EsySp;
140400110628
140500110628         exsr  sr_OpenCursor_6;
140600110628
140700110628         clear wEsySp_ds;
140800110628         dou  $Eof_6 = *on;
140900110628           exsr  sr_ReadCursor_6;
141000110628         enddo;
141100110628
141200110628         exsr  sr_CloseCursor_6;
141300110628
141400110628       ENDSR;
141500110628
141600110628       //--------------------------------------------------------------
141700110628       //?Apertura cursore C6.                                         ?
141800110628       //--------------------------------------------------------------
141900110628       BEGSR  sr_OpenCursor_6;
142000110628
142100110628         // -?Dichiarazione cursore?
142200110628         exec sql  declare C6 cursor for
142300110628
142400110628                    select WE21ccu, WE21flgu,
142500110628                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
142600110628                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg,
142700110628                           rrn(WFES210F)
142800110628                      from WFES210F
142900110628                     order by WE21ccu, WE21cver desc
143000110628
143100110628                       for fetch only;
143200110628
143300110628         // -?Apertura cursore?
143400110628         exec sql  open C6;
143500110628
143600110628       ENDSR;
143700110628
143800110628       //--------------------------------------------------------------
143900110628       //?Chiusura cursore C6.                                         ?
144000110628       //--------------------------------------------------------------
144100110628       BEGSR  sr_CloseCursor_6;
144200110628
144300110628         // -?Chiusura cursore?
144400110628         exec sql  close C6;
144500110628
144600110628       ENDSR;
144700110628
144800110628       //--------------------------------------------------------------
144900110628       //?Lettura cursore C6 (rec. WFES210F senza info EasySped).      ?
145000110628       //--------------------------------------------------------------
145100110628       BEGSR  sr_ReadCursor_6;
145200110628
145300110628         // -?Lettura cursore?
145400110628         exec sql  fetch  NEXT  from C6  into :wSQL_ds_WFES21;
145500110628
145600110628         select;
145700110628           when  SQLcode = 100;
145800110628             $Eof_6 = *on;
145900110628           when  SQLcode < *zero;
146000110628             $Eof_6 = *on;
146100110628           other;
146200110628             exsr  sr_Elab_6;
146300110628         endsl;
146400110628
146500110628       ENDSR;
146600110628
146700110628       //--------------------------------------------------------------
146800110630       //?Aggiornamento rec. WFES210F per info EasySped.               ?
146900110628       //--------------------------------------------------------------
147000110628       BEGSR  sr_Elab_6;
147100110628
147200110628         select;
147300110628
147400110628           // -?Record con la versione: salvo cod. cliente + dati della?
147500110628           //  ?versione?
147600111117           //  ?NO: la versione (anche se meno recente) del 2° cliente?
147700111117           //      ?della famiglia si sovrapporrebbe a quella del 1°...?
147800111117           //when  wSQL_ds_WFES21.sqlWESever <> *blank;
147900111117
148000111117           // -?1° Record con la versione: salvo cod. cliente + dati?
148100111117           //  ?della versione?
148200111117           //  ?Meglio tenere comunque la versione più recente (quella?
148300111117           //  ?nel 1° record letto via vedi SQL)!?
148400111117           when  wSQL_ds_WFES21.sqlWESccu <> ESYWESccu;
148500110628             wEsySp_ds = wSQL_ds_WFES21;
148600110628
148700111117           // -?Record senza la versione: se unificante = al precedente?
148800110628           //  ?aggiorno il record con la versione salvata?
148900110628           when  wSQL_ds_WFES21.sqlWESever = *blank  and
149000110628                 wSQL_ds_WFES21.sqlWESccu  = ESYWESccu;
149100110628             chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
149200110628             if  %found(WFES210F);
149300110628               WE21flgu = 'U';
149400110628               WE21edat = ESYWESedat;
149500110628               WE21ever = ESYWESever;
149600110628               WE21cver = ESYWEScver;
149700110628               WE21typw = ESYWEStypw;
149800110628               WE21cdsc = ESYWEScdsc;
149900110628               WE21cvermx = ESYWEScvermax;
150000110628               WE21cdscmx = ESYWEScdscmax;
150100110628               WE21esweb  = ESYWESesweb;
150200110628               WE21cverag = ESYWEScveragg;
150300110628               //______________
150400110628               update WFES2100;
150500110628               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
150600110628             endif;
150700110628
150800110628         endsl;
150900110628
151000110628       ENDSR;
151100110628
151200110628       //--------------------------------------------------------------
151300110628       //?Stampa segnalazione dell'errore rilevato.                    ?
151400110628       //--------------------------------------------------------------
151500110628       BEGSR  sr_PrintErr;
151600110628
151700110628         // -?Stampa del Dump?
151800110628         Dump(A);
151900110628
152000110628         // -?Stampa del Job-Log?
152100110628         Qcmd = 'DSPJOBLOG job(*) output(*print)';
152200110628         exsr  sr_ExecCmd;
152300110628
152400110628         // -?Chiusura del programma?
152500110628         exsr  sr_RoutEnd;
152600110628
152700110628       ENDSR;
152800110628
152900110628       //--------------------------------------------------------------
153000110628       //?Esecuzione del comando (già impostato).                      ?
153100110628       //--------------------------------------------------------------
153200110628       BEGSR  sr_ExecCmd;
153300110628
153400110628         clear Qcap0100;
153500110628         Qcabcsdh = *off;
153600110628         Qcapa    = *off;
153700110628         Qcacmdss = *off;
153800110628         Qcaerved = *allX'00';
153900110628
154000110628         clear Qusec;
154100110628         Qusbprv  = %size(Qusec);
154200110628
154300110628         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
154400110628                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
154500110628                           0 : 0 : Qusec);
154600110628
154700110628         //if  Qusei <> *blank;
154800110628         //  ...
154900110628         //endif;
155000110628
155100110628       ENDSR;
155200110628
155300110628       //--------------------------------------------------------------
155400110628       //?Operazioni finali.                                           ?
155500110628       //--------------------------------------------------------------
155600110628       BEGSR  sr_RoutEnd;
155700110628
155800110628         // -?Chiusura work-file?
155900110628         close(e)  WFES220F;
156000110628         close(e)  WFES210F;
156100110628
156200110628         // -?Chiusura applicazioni?
156300110628         reset  TIBS69ds;
156400110628         tibs69r( tibs69ds :
156500110628                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
156600110628
156700110628         // -?Uscita del *pgm?
156800110628         return;
156900110628
157000110628       ENDSR;
157100110628
157200110628      /end-free
