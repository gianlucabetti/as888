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
002400141024       // -?Anagrafica commerciali
002500141024     fAZCMM01L  if   e           k disk
002600141024
002700141024       // -?Organigramma filiale/aziendale?
002800141024     fAZORG01L  if   e           k disk
002900110628
003000110628       // -?Tabelle?
003100110628     fTABEL00F  if   e           k disk
003200140416
003300140416       // -?Contratti Comodato
003400140416     fUncmd01l  if   e           k disk
003500110628
003600110628       // -?Work-File di dettaglio?
003700110628     fWFES210F  Uf A e             disk    usropn
003800110628
003900110628       // -?Work-File di totale?
004000110628     fWFES220F  O    e             disk    usropn
004100110628
004200110628       //--------------------------------------------------------------
004300110628       //?Definizione costanti.                                        ?
004400110628       //--------------------------------------------------------------
004500110628
004600110628       // -?Comandi per pulizia Work-Files?
004700110628     d c_Cmd_ClrWF1    c                   const('clrpfm file(*libl/+
004800110628     d                                                   WFES210F)')
004900110628     d c_Cmd_ClrWF2    c                   const('clrpfm file(*libl/+
005000110628     d                                                   WFES220F)')
005100110628
005200110628       // -?Supporti Cliente => BRT gestiti?
005300110628     d c_SuppCB_ESYSP  c                   const('ESYSP')
005400110628     d c_SuppCB_ESVAL  c                   const('ESVAL')
005500110628     d c_SuppCB_ESWEB  c                   const('ESWEB')
005600110628
005700110628       //--------------------------------------------------------------
005800110628       //?Definizione schiere.                                         ?
005900110628       //--------------------------------------------------------------
006000110628
006100130110     d c_MaxElem       c                   const(25000)
006200110628       // -?Clienti in "3C" con famiglia?
006300140416     d*$Cli            s              7a   dim(c_MaxElem)    inz
006400110628       // -?Padri   in "3C" con famiglia?
006500140416     d*$Padre          s              7a   dim(c_MaxElem)    inz
006600110628       // -?Stampanti in comodato (da tab. "3R") x ogni cliente in "3C"?
006700140416     d*$StpComodato    s                   like(ds3R.§3Rnot)
006800140416     d*                                    dim(c_MaxElem)  inz
006900140416       // -?Codici padre legame ST dei clienti esaminati
007000140416     d $padst          s                   like(d10cod)
007100140416     d                                     dim(c_MaxElem)  inz
007200140416       // -?Flag di presenza materiale in comodato per famiglia ST
007300140416     d $com            s              1
007400140416     d                                     dim(c_MaxElem)  inz
007500110628
007600110628       //--------------------------------------------------------------
007700110628       //?Definizione aree dati.                                       ?
007800110628       //--------------------------------------------------------------
007900110628
008000110628       // -?Dati utente?
008100110628     d §AzUte        e ds                  extname(AZUTE00F)
008200110628     d                                     dtaara
008300110628     d §DatiUte      e ds                  extname(dDatiUte)
008400110628     d                                     dtaara
008500110628
008600110628       //--------------------------------------------------------------
008700110628       //?Definizione strutture dati.                                  ?
008800110628       //--------------------------------------------------------------
008900110628
009000110628       // -?Status ds?
009100110628     d Status         sds
009200110628     d  SDSpgm           *proc
009300110628     d  SDSprm           *parms
009400110628
009500110628       // -?Parametri ricevuti?
009600110628     d KPJBA         e ds
009700110628     d TNSS07ds      e ds                  inz
009800110628     d   D07dti      e                     inz(*loval)
009900110628     d   D07dtf      e                     inz(*hival)
010000110628
010100110628       // -?Tab. "3CE" = Versioni EasySped per cliente?
010200110628     d d3CE          e ds                  inz         qualified
010300110628       // -?Tab. "3CP" = Serie parziali assegnate ai clienti NON ESWEB?
010400110628     d d3CP          e ds                  inz         qualified
010500110628     d   §3CPal      e                     inz(*hival)
010600110628       // -?Tab. "3EW" = Dati assegnati alla postazione EasySpedWeb?
010700110628     d d3EW          e ds                  inz         qualified
010800110628
010900110628       // -?Tab. "3C"  = Invio dati -bollettazione-?
011000110628     d ds3C          e ds                  inz         qualified
011100110628       // -?Tab. "3R"  = Scheda clienti per scambio dati?
011200110628     d ds3R          e ds                  inz         qualified
011300110628       // -?Tab. "05"  = Aree?
011400110628     d ds05          e ds                  inz         qualified
011500110628       // -?Tab. "17"  = Distretti?
011600110628     d ds17          e ds                  inz         qualified
011700110628
011800110628       // -?Strutture dati "di comodo"?
011900110628       //  ?(per la definizione dei dati da estrarre)?
012000110628     d TIVSSds       e ds                  extname(TIVSS00F)
012100110628     d                                     inz         qualified
012200110628     d TNTBEds       e ds                  extname(TNTBE00F)
012300110628     d                                     inz         qualified
012400110628     d TITASds       e ds                  extname(TITAS00F)
012500110628     d                                     inz         qualified
012600110628     d CNACOds_Uni   e ds                  extname(CNACO00F)
012700110628     d                                     inz         qualified
012800141024     d CNCLPds_Uni   e ds                  extname(CNCLP00F)
012900141024     d                                     inz         qualified
013000110628
013100110628       // -?Dati estratti via SQL:?
013200110628       // - -?dal file TABEL00F per tab. "3C"?
013300110628     d wSQL_ds_3C      ds                  inz         qualified
013400110628     d   sqlTBLkey                         inz  like(TBLkey)
013500110628     d   sqlTBLuni                         inz  like(TBLuni)
013600110630       // - -?dal file TNTBE00F per tab. "3CP"?
013700110630     d wSQL_ds_3CP     ds                  inz         qualified
013800110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
013900110630     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
014000110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
014100110630       // - -?dal file TNTBE00F per tab. "3CE"?
014200110630     d wSQL_ds_3CE     ds                  inz         qualified
014300110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
014400110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
014500110628       // - -?dal file TNTBE00F per tab. "3EW"?
014600110628     d wSQL_ds_3EW     ds                  inz         qualified
014700110628     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
014800110628     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
014900110628     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
015000110630       // - -?dal file TIVSS00F se VSSISV = "EW"?
015100110630     d wSQL_ds_TIVSS   ds                  inz         qualified
015200110630     d   sqlVSSksu                         inz    like(TIVSSds.VSSksu)
015300110630     d   sqlVSSsun                         inz    like(TIVSSds.VSSsun)
015400110630     d   sqlVSSisv                         inz    like(TIVSSds.VSSisv)
015500110630     d*//sqlVSSdsc                         inz    like(TIVSSds.VSSdsc)
015600110628
015700110628       // -?Dati estratti via SQL:?
015800110628       // - -?dal file TITAS33C x CCM/AAS/MGS?
015900110628     d wSQL_ds_TITAS   ds                  inz         qualified
016000110628     d   sqlTASccm                         inz    like(TITASds.TASccm)
016100110628     d*//sqlTASaas                         inz    like(TITASds.TASaas)
016200110628     d*//sqlTASmgs                         inz    like(TITASds.TASmgs)
016300110628     d*//sqlTASnrs                         inz    like(TITASds.TASnrs)
016400110628     d   sqlTASdts                    8  0 inz
016500110628     d   sqlTOTspe                    9  0 inz
016600110628     d   sqlTOTcol                    9  0 inz
016700110628     d   sqlMAXncd                    9  0 inz
016800110628     d   sqlMAXnca                    9  0 inz
016900110628
017000110628       // -?Dati estratti via SQL:?
017100110628       // - -?dal file WFES210F x aggiornamento info EasySped?
017200110628     d wSQL_ds_WFES21  ds                  inz         qualified
017300110628     d  sqlWESccu                          inz    like(WE21ccu)
017400151023     d  sqlWESccm                          inz    like(WE21ccm)
017500151023     d  sqlWESscb                          inz    like(WE21scb )
017600110628     d  sqlWESflgu                         inz    like(WE21flgu)
017700110628     d  sqlWESedat                         inz    like(WE21edat)
017800110628     d  sqlWESever                         inz    like(WE21ever)
017900110628     d  sqlWEScver                         inz    like(WE21cver)
018000110628     d  sqlWEScdsc                         inz    like(WE21cdsc)
018100110628     d  sqlWEStypw                         inz    like(WE21typw)
018200110628     d  sqlWEScverMax                      inz    like(WE21cverMx)
018300110628     d  sqlWEScdscMax                      inz    like(WE21cdscMx)
018400110628     d  sqlWESesweb                        inz    like(WE21esweb)
018500110628     d  sqlWEScverAgg                      inz    like(WE21cverag)
018600110628     d  SQLrrn                        8  0 inz
018700110628
018800110628       // -?Salvataggio dati (del padre) estratti in wSQL_WFES21?
018900151023     d wEsySp_ds       ds                  inz
019000110628     d  ESYwesccu                          like(wSQL_ds_WFES21.sqlWESccu)
019100110628     d                                     inz
019200151023     d  ESYwesccm                          like(wSQL_ds_WFES21.sqlWESccm)
019300151023     d                                     inz
019400151023     d  ESYwesscb                          like(wSQL_ds_WFES21.sqlWESscb )
019500151023     d                                     inz
019600110628     d  ESYwesflgu                         like(wSQL_ds_WFES21.sqlWESflgu)
019700110628     d                                     inz
019800110628     d  ESYwesedat                         like(wSQL_ds_WFES21.sqlWESedat)
019900110628     d                                     inz
020000110628     d  ESYwesever                         like(wSQL_ds_WFES21.sqlWESever)
020100110628     d                                     inz
020200110628     d  ESYwescver                         like(wSQL_ds_WFES21.sqlWEScver)
020300110628     d                                     inz
020400110628     d  ESYwescdsc                         like(wSQL_ds_WFES21.sqlWEScdsc)
020500110628     d                                     inz
020600110628     d  ESYwestypw                         like(wSQL_ds_WFES21.sqlWEStypw)
020700110628     d                                     inz
020800110628     d  ESYwescverMax                      like(wSQL_ds_WFES21.sqlWEScverMax)
020900110628     d                                     inz
021000110628     d  ESYwescdscMax                      like(wSQL_ds_WFES21.sqlWEScdscMax)
021100110628     d                                     inz
021200110628     d  ESYwesesweb                        like(wSQL_ds_WFES21.sqlWESesweb)
021300110628     d                                     inz
021400110628     d  ESYwescverAgg                      like(wSQL_ds_WFES21.sqlWEScverAgg)
021500110628     d                                     inz
021600110628
021700110628       // -?Dati estratti via SQL:?
021800110628       // - -?dal file WFES210F x totalizzazione in WFES220F?
021900110628     d WFES210ds     e ds                  extname(WFES210F)
022000110628     d                                     inz         qualified
022100110628
022200140416     d tibs10ds      e ds
022300140416     d skc                           11    DIM(500) overlay(d10skc)
022400110628       //--------------------------------------------------------------
022500110628       //?Definizione variabili globali.                               ?
022600110628       //--------------------------------------------------------------
022700110628
022800110628       // -?Flags booleani?
022900110628     d $EoF_1          s               n   inz
023000110628     d $EoF_2          s               n   inz
023100110628     d $EoF_3          s               n   inz
023200110628     d $EoF_4          s               n   inz
023300110628     d $EoF_5          s               n   inz
023400110628     d $EoF_6          s               n   inz
023500151023     d $EoF_7          s               n   inz
023600110628
023700110628       // -?Stringhe SQL da eseguire?
023800110628     d wSql            s           5000    inz  varying
023900110628     d wSql_4          s           5000    inz  varying
024000110628
024100110628       // -?Indici di schiera?
024200140416     d*xx              s              5  0 inz
024300110628     d yy              s              5  0 inz
024400140416     d i               s              5  0 inz
024500110628
024600110628       // -?Campi di comodo?
024700110628     d wDate           s              8  0 inz
024800110628     d wNTS            s              9  0 inz
024900110630     d SAVcdi          s                   inz(*loval) like(ORGfl3)
025000110628     d SAVcar          s                   inz(*loval) like(WE21car)
025100110628     d SAVccu          s                   inz(*loval) like(WE21ccu)
025200110628     d wSgnClI         s                   inz(1)      like(WE21nsi)
025300110630     d wSupporto       s                   inz         like(WE21scb)
025400140416     d wcom            s              1    inz
025500140416     d $COMODATO       s             21    inz
025600140416     d kcli            s                   like(cmdksc)
025700151023     d wCCM            s                   like(WE21CCM)
025800110628
025900110628       //--------------------------------------------------------------
026000110628       //?Definizione prototipi procedure.                             ?
026100110628       //--------------------------------------------------------------
026200110628
026300110628       // -?Reperimento dati utente?
026400110628     d TIBS34ds      e ds                  inz
026500110628      /copy gaitrasrc/srcProtoPr,TIBS34R
026600110628
026700110628       // -?Controllo/Decodifica cliente?
026800110628      /copy gaitrasrc/srcProtoPI,TIBS69R
026900110628      /copy gaitrasrc/srcProtoPR,TIBS69R
027000140416
027100140416       // -?ricerca cliente unificante
027200140416      /copy gaitrasrc/srcProtoPR,TIBS10R
027300110628
027400110628       // -?Parametri API QCAPCMD (Process Commands)?
027500110628     d Qcmd            s           2048    inz  varying
027600110628      /copy qSysInc/qRpgleSrc,QCAPCMD
027700110628       // -?API QCAPCMD (Process Commands)?
027800110628      /copy gaitrasrc/srcProtoPR,QCAPCMD
027900110628
028000110628       // -?Parametri gestione errori API.?
028100110628      /copy qsysinc/qrpglesrc,QUSEC
028200110628
028300110628       //--------------------------------------------------------------
028400110628       //?Definizione key-list.                                        ?
028500110628       //--------------------------------------------------------------
028600110628
028700110628       // -?File TABEL00F?
028800110628     d k03tabel00    e ds                  extname(TABEL00F : *key)
028900110628     d                                     prefix(k_)   inz
029000110628       // -?File AZORG01L?
029100110628     d k01azorg01    e ds                  extname(AZORG01L : *key)
029200110628     d                                     prefix(k_)   inz
029300141027       // -?File AZCMM01L?
029400141027     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
029500141027     d                                     prefix(k_)   inz
029600110628
029700110628       //--------------------------------------------------------------
029800110628       //?M A I N - L I N E                                            ?
029900110628       //--------------------------------------------------------------
030000110628
030100110628     c     *Entry        plist
030200110628     c                   parm                    KPJBA
030300110628
030400110628      /free
030500110628
030600110628       // -?Operazioni iniziali?
030700151023       //exsr  sr_RoutInz;
030800110628
030900110628
031000110628       // -?Aggiornamento dati EasySped/EasySpedWeb mancanti...?
031100110628       exsr  sr_Upd_EsySp;
031200110628
031300110628
031400110628       // -?Fine programma?
031500110628       exsr  sr_RoutEnd;
031600110628
031700110628       //--------------------------------------------------------------
031800110628       //?Operazioni iniziali.                                         ?
031900110628       //--------------------------------------------------------------
032000110628       BEGSR  sr_RoutInz;
032100110628
032200110628         *inLR = *on;
032300110628
032400110628         // -?Impostazione opzioni per SQL?
032500110628         exec sql  set  option  DynUsrPrf = *Owner,
032600110628                                CloSqlCsr = *EndMod;
032700110628
032800110628         // -?Reperimento dati job?
032900110628         exsr  sr_DatiJob;
033000110628
033100110628         // -?Impostazione campi "fissi"?
033200110628         k_TBLkut = 1;
033300110628
033400110628         // -?Reperimento data odierna in formato aaaammgg?
033500110628         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
033600110628         //wData_Iso = %date(wDate : *iso);
033700110628
033800110628         // -?Impostazione eventuali parametri ricevuti?
033900110628         if  SDSprm > *zero  and
034000110628             KPJBU <> *blank;
034100110628           TNSS07ds = KPJBU;
034200110628         else;
034300110628           reset  TNSS07ds;
034400110628         endif;
034500110628
034600110628         // -?Pulizia work-files del lavoro sottomesso e loro apertura?
034700110628         Qcmd = c_Cmd_ClrWF1;
034800110628         exsr  sr_ExecCmd;
034900110628         if  Qusei <> *blank;
035000110628           exsr  sr_PrintErr;
035100110628         endif;
035200110628         open  WFES210F;
035300110628         Qcmd = c_Cmd_ClrWF2;
035400110628         exsr  sr_ExecCmd;
035500110628         if  Qusei <> *blank;
035600110628           exsr  sr_PrintErr;
035700110628         endif;
035800110628         open  WFES220F;
035900110628
036000140416         //clear  xx;
036100140416         //clear  $Cli;
036200140416         //clear  $Padre;
036300140416         //clear  $StpComodato;
036400110628
036500110628         // -?Intabellamento clienti e relativa famiglia da "3C"?
036600140416         //k_TBLcod = '3C';
036700140416         //setll  %kds(k03tabel00 : 2)  TABEL;
036800140416         //reade  %kds(k03tabel00 : 2)  TABEL;
036900140416         //DoW  Not  %eof(TABEL00F);
037000140416         //  if  TBLflg = *blank;
037100140416         //    xx += 1;
037200140416         //    ds3C       = TBLuni;
037300140416         //    $Cli(xx)   = %trimr( TBLkey );
037400140416         //    $Padre(xx) = %editc( ds3C.§3Ccks : 'X' );
037500140416         //  endif;
037600140416         //reade  %kds(k03tabel00 : 2)  TABEL;
037700140416         //EndDo;
037800110628
037900110628         // -?Intabellamento stampanti in comodato per cliente da "3R"?
038000140416         //k_TBLcod = '3R';
038100140416         //For  yy = 1  To  xx;
038200140416         //  k_TBLkey = $Cli(yy);
038300140416         //  chain  %kds(k03tabel00)  TABEL;
038400140416         //  if  %found(TABEL00F)   and   TBLflg = *blank;
038500140416         //    ds3R = TBLuni;
038600140416         //    $StpComodato(yy) = ds3R.§3Rnot;
038700140416         //  endif;
038800140416         //EndFor;
038900110628
039000110628       ENDSR;
039100110628
039200110628       //--------------------------------------------------------------
039300110628       //?Reperimento Dati del job (Utente/Operativi).                 ?
039400110628       //--------------------------------------------------------------
039500110628       BEGSR  sr_DatiJob;
039600110628
039700110628         in(E) §AzUte;
039800110628         if NOT %error;
039900110628           in(E) §DatiUte;
040000110628         endif;
040100110628         if  %error  or  RSut = *blank;
040200110628           clear TIBS34ds;
040300110628           tibs34r ( tibs34ds );
040400110628           in §AzUte;
040500110628           in §DatiUte;
040600110628         endif;
040700110628
040800110628       ENDSR;
040900110628
041000110628       //--------------------------------------------------------------
041100110628       //?Apertura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
041200110628       //?  estrazione clienti EasySped.                               ?
041300110628       //--------------------------------------------------------------
041400110628       BEGSR  sr_OpenCursor_1;
041500110628
041600110628         // -?Preparazione stringa SQL?
041700110628         wSql = 'with +
041800110628
041900110628                 Tab_3C as +
042000110628                     ( select TBLkey, TBLuni +
042100110628                         from TABEL00F +
042200110628                        where TBLflg = '' '' +
042300110628                          and TBLcod = ''3C'' +
042400110628                          and substr(TBLuni,  1, 2) > ''00'' +
042500110628                          and substr(TBLuni, 44, 5) in +
042600110628                              (''' + c_SuppCB_ESYSP + ''', +
042700110628                               ''' + c_SuppCB_ESVAL + ''') ), +
042800110628
042900110628                 Tab_3CP as +
043000110628                     ( select TBEke1, TBEke2, TBEuni +
043100110628                         from TNTBE00F +
043200110628                        where TBEatb = '' '' +
043300110628                          and TBEcod = ''3CP'' +
043400110628                          and TBEsif = '' '' +
043500110629                          and TBElin = '' '' ), +
043600110628
043700110628                 Tab_3CE as +
043800110628                     ( select TBEke1, TBEuni +
043900110628                         from TNTBE00F +
044000110628                        where TBEatb = '' '' +
044100110628                          and TBEcod = ''3CE'' +
044200110628                          and TBEke2 = '' '' +
044300110628                          and TBEsif = '' '' +
044400110628                          and TBElin = '' '' ) +
044500110628
044600110630                 select Tab_3C.*, +
044700110630                        case when Tab_3CP.TBEke1 > '' '' +
044800110630                             then Tab_3CP.TBEke1 else '' '' end, +
044900110630                        case when Tab_3CP.TBEke2 > '' '' +
045000110630                             then Tab_3CP.TBEke2 else '' '' end, +
045100110630                        case when Tab_3CP.TBEuni > '' '' +
045200110630                             then Tab_3CP.TBEuni else '' '' end, +
045300110630                        case when Tab_3CE.TBEke1 > '' '' +
045400110630                             then Tab_3CE.TBEke1 else '' '' end, +
045500110630                        case when Tab_3CE.TBEuni > '' '' +
045600110630                             then Tab_3CE.TBEuni else '' '' end  +
045700110628
045800110628                   from Tab_3C left outer join Tab_3CP +
045900110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
046000110628                        substr(Tab_3CP.TBEke1, 1, 7) and +
046100110628                        substr(Tab_3C.TBLuni, 1, 2) = +
046200110628                        substr(Tab_3CP.TBEke2, 1, 2) +
046300110628
046400110628                               left outer join Tab_3CE +
046500110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
046600110630                        substr(Tab_3CE.TBEke1, 1, 7)';
046700110628
046800110628         // -?Dichiarazione cursore?
046900110628         exec sql  prepare S1  from :wSql;
047000110628         exec sql  declare C1  cursor for S1;
047100110628
047200110628         // -?Apertura del cursore?
047300110628         exec sql  open C1;
047400110628
047500110628         if  SQLcode < *zero;
047600110628           $EoF_1 = *on;
047700110628           exsr  sr_PrintErr;
047800110628         endif;
047900110628
048000110628       ENDSR;
048100110628
048200110628       //--------------------------------------------------------------
048300110628       //?Lettura cursore C1 (tab. "3C", "3CP" e "3CE") per            ?
048400110628       //?  estrazione clienti EasySped.                               ?
048500110628       //--------------------------------------------------------------
048600110628       BEGSR  sr_ReadCursor_1;
048700110628
048800110628         clear  ds3C;
048900110630         reset  d3CP;
049000110628         clear  d3CE;
049100110630         reset  wSgnClI;
049200110628         clear  wSQL_ds_3C;
049300110628         clear  wSQL_ds_3CP;
049400110628         clear  wSQL_ds_3CE;
049500110628
049600110628         // -?Lettura cursore?
049700110628         exec sql  fetch next  from C1  into :wSQL_ds_3C,
049800110628                                             :wSQL_ds_3CP,
049900110628                                             :wSQL_ds_3CE;
050000110628
050100110628         select;
050200110628           // -?Fine flusso?
050300110628           when  SQLcode = 100;
050400110628             $EoF_1 = *on;
050500110628           // -?Errore?
050600110628           when  SQLcode < *zero;
050700110628             exsr  sr_PrintErr;
050800110628             $EoF_1 = *on;
050900110628           // -?Elaborazione dati estratti?
051000110628           other;
051100110628             exsr  sr_Elab_1;
051200110628         endsl;
051300110628
051400110628       ENDSR;
051500110628
051600110628       //--------------------------------------------------------------
051700110628       //?Chiusura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
051800110628       //?  estrazione clienti EasySped.                               ?
051900110628       //--------------------------------------------------------------
052000110628       BEGSR  sr_CloseCursor_1;
052100110628
052200110628         // -?Chiusura del cursore?
052300110628         exec sql  close C1;
052400110628
052500110628       ENDSR;
052600110628
052700110628       //--------------------------------------------------------------
052800110628       //?Elaborazione singolo cliente estratto (EasySped).            ?
052900110628       //--------------------------------------------------------------
053000110628       BEGSR  sr_Elab_1;
053100110628
053200110628         // -?Valorizzazione tab. "3C"?
053300110628         ds3C = wSQL_ds_3C.sqlTBLuni;
053400110628
053500110628         // -?Valorizzazione tab. "3CP"?
053600110628         if  wSQL_ds_3CP.sqlTBEke1 > *zero  and
053700110628             wSQL_ds_3CP.sqlTBEke2 > *zero;
053800110628           Monitor;
053900110628             wSgnClI = %dec( %subst( wSQL_ds_3CP.sqlTBEke2 : 3 : 7 )
054000110628                             : 7 : 0 );
054100110628             On-Error  105;
054200110628               reset  wSgnClI;
054300110628               reset  d3CP;
054400110628           EndMon;
054500110628           if  wSQL_ds_3CP.sqlTBEuni > *zero;
054600110628             d3CP = wSQL_ds_3CP.sqlTBEuni;
054700110628           endif;
054800110628         endif;
054900110628
055000110628         // -?Valorizzazione tab. "3CE"?
055100110628         if  wSQL_ds_3CE.sqlTBEke1 > *blank;
055200110628           d3CE = wSQL_ds_3CE.sqlTBEuni;
055300110628         endif;
055400110628
055500110628         // -?Pulizia iniziale del record da registrare?
055600110628         clear  WFES2100;
055700110628
055800110628
055900110628         // -?Reperimento Distretto ed Area?
056000110628         //  ?(a "rottura" di filiale del cliente unificante)?
056100110628         k_ORGfil = %int( %subst( %editc( ds3C.§3Ccks : 'X' ) : 1 : 3 ) );
056200110628         if  k_ORGfil <> ORGfil;
056300110628           chain  %kds(k01azorg01)  AZORG;
056400110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
056500110628             clear  ORGfl3;
056600110628             clear  ORGcar;
056700110628             clear  ds17;
056800110628             clear  ds05;
056900110628           endif;
057000110628         endif;
057100110628
057200110628         // -?Decodifica distretto?
057300110628         if  ORGfl3 <> SAVcdi;
057400110628           SAVcdi = ORGfl3;
057500110628           clear  ds17;
057600110628           k_TBLkut = 1;
057700110628           k_TBLcod = '17';
057800110628           k_TBLkey = ORGfl3;
057900110628           chain  %kds(k03tabel00)  TABEL;
058000110628           if  %found(TABEL00F)  and  TBLflg = *blank;
058100110628             ds17 = TBLuni;
058200110628           endif;
058300110628         endif;
058400110628
058500110628         // -?Decodifica area?
058600110628         if  ORGcar <> SAVcar;
058700110628           SAVcar = ORGcar;
058800110628           clear  ds05;
058900110628           k_TBLkut = 1;
059000110628           k_TBLcod = '05';
059100110628           k_TBLkey = %editc( ORGcar : 'X' );
059200110628           chain  %kds(k03tabel00)  TABEL;
059300110628           if  %found(TABEL00F)  and  TBLflg = *blank;
059400110628             ds05 = TBLuni;
059500110628           endif;
059600110628         endif;
059700110628
059800110628         // -?Decodifica unificante?
059900110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
060000110628           clear  TIBS69ds;
060100110628           I69sif = knsif;
060200110628           I69kcc = DUTkci;
060300110628           I69kac = ds3C.§3Ccks;
060400110628           tibs69r( tibs69ds :
060500110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
060600110628           if  O69err = *on;
060700110628             clear  CNACOds_Uni;
060800110628           else;
060900110628             CNACOds_Uni = ds_CNACO;
061000141027           endif;
061100110628         EndIf;
061200110628
061300110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
061400110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
061500110628           clear  TIBS69ds;
061600110628           I69sif = knsif;
061700110628           I69kcc = DUTkci;
061800110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
061900110628           tibs69r( tibs69ds :
062000110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062100110628           if  O69err = *on;
062200110628             clear  ds_CNACO;
062300110628           endif;
062400110628         EndIf;
062500110628
062600140416         // -?Reperimento materiale in comodato file UNCMD00F ?
062700140416         exsr  sr_StampCom;
062800110628
062900110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
063000110628         // -?(reperimento dati da spedizioni associate)?
063100110628         $Eof_4 = *off;
063200110628         exsr  sr_OpenCursor_4;
063300110628
063400110628         DoU  $Eof_4;
063500110628           exsr  sr_ReadCursor_4;
063600110628         EndDo;
063700110628
063800110628         exsr  sr_CloseCursor_4;
063900110628
064000110628         // -?Scrittura del record a totale per cliente?
064100110629         wSupporto = ds3C.§3Ccba;
064200110628         exsr  sr_Wrt_WFES210;
064300110628
064400110628       ENDSR;
064500110628
064600110628       //--------------------------------------------------------------
064700110628       //?Apertura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
064800110628       //?  estrazione clienti EasySpedWeb.                            ?
064900110628       //--------------------------------------------------------------
065000110628       BEGSR  sr_OpenCursor_2;
065100110628
065200110628         // -?Preparazione stringa SQL?
065300110628         wSql = 'with +
065400110628
065500110628                 TIVSS as +
065600110628                     ( select VSSksu, VSSsun, VSSisv +
065700110628                         from TIVSS00F +
065800110628                        where VSSisv = ''EW'' and +
065900110628                             (VSSdsc = 0 or VSSdsc > ' +
066000110628                              %char(wDate) + ') ), +
066100110628
066200110628                 Tab_3EW as +
066300110628                     ( select TBEke1, TBEke2, TBEuni +
066400110628                         from TNTBE00F +
066500110628                        where TBEatb = '' '' and +
066600120814                              TBEcod = ''3EW'' and +
066700120814                              substr(TBEuni, 33, 1) = '' '' ), +
066800110628
066900110628                 Tab_3C as +
067000110628                     ( select TBLkey, TBLuni +
067100110628                         from TABEL00F +
067200110628                        where TBLflg = '' '' and +
067300110628                              TBLcod = ''3C'' and +
067400110628                              substr(TBLuni, 44, 5) = +
067500110630                              ''' + c_SuppCB_ESWEB + ''') +
067600110628
067700110628                 select * +
067800110628
067900110628                   from TIVSS inner join Tab_3EW +
068000110628                     on TIVSS.VSSksu = Tab_3EW.TBEke1  and +
068100110628                        TIVSS.VSSsun = Tab_3EW.TBEke2 +
068200110628
068300110628                              inner join Tab_3C +
068400110628                     on substr(TIVSS.VSSksu, 2, 7) = +
068500110628                        substr(Tab_3C.TBLuni, 79, 7)';
068600110628
068700110628         // -?Dichiarazione cursore?
068800110628         exec sql  prepare S2  from :wSql;
068900110628         exec sql  declare C2  cursor for S2;
069000110628
069100110628         // -?Apertura del cursore?
069200110628         exec sql  open C2;
069300110628
069400110628         if  SQLcode < *zero;
069500110628           $EoF_2 = *on;
069600110628           exsr  sr_PrintErr;
069700110628         endif;
069800110628
069900110628       ENDSR;
070000110628
070100110628       //--------------------------------------------------------------
070200110628       //?Lettura cursore C2 (file TIVSS, tab. "3EW" e tab. "3C") per  ?
070300110628       //?  estrazione clienti EasySpedWeb.                            ?
070400110628       //--------------------------------------------------------------
070500110628       BEGSR  sr_ReadCursor_2;
070600110628
070700110628         clear  d3EW;
070800110628         clear  ds3C;
070900110628         clear  wSQL_ds_TIVSS;
071000110628         clear  wSQL_ds_3EW;
071100110628         clear  wSQL_ds_3C;
071200110628
071300110628         // -?Lettura cursore?
071400110628         exec sql  fetch next  from C2  into :wSQL_ds_TIVSS,
071500110628                                             :wSQL_ds_3EW,
071600110628                                             :wSQL_ds_3C;
071700110628
071800110628         select;
071900110628           // -?Fine flusso?
072000110628           when  SQLcode = 100;
072100110628             $EoF_2 = *on;
072200110628           // -?Errore?
072300110628           when  SQLcode < *zero;
072400110628             exsr  sr_PrintErr;
072500110628             $EoF_2 = *on;
072600110628           // -?Elaborazione dati estratti?
072700110628           other;
072800110628             exsr  sr_Elab_2;
072900110628         endsl;
073000110628
073100110628       ENDSR;
073200110628
073300110628       //--------------------------------------------------------------
073400110628       //?Chiusura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
073500110628       //?  estrazione clienti EasySpedWeb.                            ?
073600110628       //--------------------------------------------------------------
073700110628       BEGSR  sr_CloseCursor_2;
073800110628
073900110628         // -?Chiusura del cursore?
074000110628         exec sql  close C2;
074100110628
074200110628       ENDSR;
074300110628
074400110628       //--------------------------------------------------------------
074500110629       //?Elaborazione singolo cliente estratto (EasySpedWeb).         ?
074600110628       //--------------------------------------------------------------
074700110628       BEGSR  sr_Elab_2;
074800110628
074900110628         d3EW = wSQL_ds_3EW.sqlTBEuni;
075000110628         ds3C = wSQL_ds_3C.sqlTBLuni;
075100110628
075200110630         // -?Pulizia iniziale del record da registrare?
075300110628         clear  WFES2100;
075400110628
075500110628         // -?Reperimento Area?
075600110628         //  ?(a "rottura" di filiale del cliente unificante)?
075700110628         k_ORGfil = %int( %subst( wSQL_ds_TIVSS.sqlVSSksu : 2 : 3 ) );
075800110628         if  k_ORGfil <> ORGfil;
075900110628           chain  %kds(k01azorg01)  AZORG;
076000110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
076100110629             clear  ORGfl3;
076200110628             clear  ORGcar;
076300110628           endif;
076400110628         endif;
076500110629
076600110629         // -?Decodifica distretto?
076700110629         if  ORGfl3 <> SAVcdi;
076800110629           SAVcdi = ORGfl3;
076900110629           clear  ds17;
077000110629           k_TBLkut = 1;
077100110629           k_TBLcod = '17';
077200110629           k_TBLkey = ORGfl3;
077300110629           chain  %kds(k03tabel00)  TABEL;
077400110629           if  %found(TABEL00F)  and  TBLflg = *blank;
077500110629             ds17 = TBLuni;
077600110629           endif;
077700110629         endif;
077800110628
077900110628         // -?Decodifica area?
078000110628         if  ORGcar <> SAVcar;
078100110628           SAVcar = ORGcar;
078200110628           clear  ds05;
078300110628           k_TBLkut = 1;
078400110628           k_TBLcod = '05';
078500110628           k_TBLkey = %editc( ORGcar : 'X' );
078600110628           chain  %kds(k03tabel00)  TABEL;
078700110628           if  %found(TABEL00F)  and  TBLflg = *blank;
078800110628             ds05 = TBLuni;
078900110628           endif;
079000110628         endif;
079100110628
079200110628         // -?Decodifica unificante?
079300110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
079400110628           clear  TIBS69ds;
079500110628           I69sif = knsif;
079600110628           I69kcc = DUTkci;
079700110628           I69kac = ds3C.§3Ccks;
079800110628           tibs69r( tibs69ds :
079900110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
080000110628           if  O69err = *on;
080100110628             clear  CNACOds_Uni;
080200110628           else;
080300110628             CNACOds_Uni = ds_CNACO;
080400110628           endif;
080500110628         EndIf;
080600110628
080700110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
080800110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
080900110628           clear  TIBS69ds;
081000110628           I69sif = knsif;
081100110628           I69kcc = DUTkci;
081200110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
081300110628           tibs69r( tibs69ds :
081400110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
081500110628           if  O69err = *on;
081600110628             clear  ds_CNACO;
081700110628           endif;
081800110628         EndIf;
081900110628
082000140416         // -?Reperimento materiale in comodato file UNCMD00F ?
082100140416         exsr  sr_StampCom ;
082200110628
082300110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
082400110628         // -?(reperimento dati da spedizioni associate)?
082500110628         $Eof_4 = *off;
082600110628         exsr  sr_OpenCursor_4;
082700110628
082800110628         DoU  $Eof_4;
082900110628           exsr  sr_ReadCursor_4;
083000110628         EndDo;
083100110628
083200110628         exsr  sr_CloseCursor_4;
083300110628
083400110628         // -?Scrittura del record a totale per cliente?
083500110629         wSupporto = ds3C.§3Ccba;
083600110628         exsr  sr_Wrt_WFES210;
083700110628
083800110628       ENDSR;
083900110628
084000110628       //--------------------------------------------------------------
084100110628       //?Apertura cursore C4 (totale spedizioni nel periodo x cliente)?
084200110628       //--------------------------------------------------------------
084300110628       BEGSR  sr_OpenCursor_4;
084400110628
084500110628         // -?Preparazione stringa SQL?
084600110628         //  ? N.B. ?- Seleziona le LdV del periodo richiesto?
084700110630         //  ?· Seleziona le LdV del cliente in esame da tab. "3C"?
084800110628         //  ?· Seleziona le LdV con la serie del cliente in esame?
084900110628         //  ?· Seleziona le LdV porto franco?
085000110628
085100110630         wSql_4 = 'select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
085200110630                          count(*), sum( TASncl ) +
085300110628                     from TITASP0F +
085400110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
085500110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
085600110628                          %editc( D07dti : 'X' ) + ' and ' +
085700110628                          %editc( D07dtf : 'X' ) +
085800110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
085900110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
086000110628                  ' group by TASccm +
086100110628
086200110628                   UNION +
086300110628
086400110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
086500110630                          count(*), sum( TASncl ) +
086600110628                     from TITAS10F +
086700110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
086800110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
086900110628                          %editc( D07dti : 'X' ) + ' and ' +
087000110628                          %editc( D07dtf : 'X' ) +
087100110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
087200110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
087300110628                  ' group by TASccm +
087400110628
087500110628                   UNION +
087600110628
087700110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
087800110630                          count(*), sum( TASncl ) +
087900110628                     from TITAS00F +
088000110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
088100110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
088200110628                          %editc( D07dti : 'X' ) + ' and ' +
088300110628                          %editc( D07dtf : 'X' ) +
088400110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
088500110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
088600110628                  ' group by TASccm +
088700110628
088800110628                      FOR FETCH ONLY';
088900110628
089000110628         // -?Dichiarazione cursore?
089100110628         exec sql  prepare S4  from :wSql_4;
089200110628         exec sql  declare C4  cursor for S4;
089300110628
089400110628         // -?Apertura del cursore?
089500110628         exec sql  open C4;
089600110628
089700110628         if  SQLcode < *zero;
089800110628           $EoF_4 = *on;
089900110628           exsr  sr_PrintErr;
090000110628         endif;
090100110628
090200110628       ENDSR;
090300110628
090400110628       //--------------------------------------------------------------
090500110628       //?Lettura cursore C4 (totale spedizioni nel periodo x cliente) ?
090600110628       //--------------------------------------------------------------
090700110628       BEGSR  sr_ReadCursor_4;
090800110628
090900110628         clear  wSQL_ds_TITAS;
091000110628
091100110628         // -?Lettura cursore?
091200110628         exec sql  fetch next  from C4  into :wSQL_ds_TITAS;
091300110628
091400110628         select;
091500110628           // -?Fine flusso?
091600110628           when  SQLcode = 100;
091700110628             $EoF_4 = *on;
091800110628           // -?Errore?
091900110628           when  SQLcode < *zero;
092000110628             $EoF_4 = *on;
092100110628             exsr  sr_PrintErr;
092200110628           // -?Elaborazione dati estratti?
092300110628           other;
092400110628             exsr  sr_Elab_4;
092500110628         endsl;
092600110628
092700110628       ENDSR;
092800110628
092900110628       //--------------------------------------------------------------
093000110628       //?Chiusura cursore C4 (totale spedizioni nel periodo x cliente)?
093100110628       //--------------------------------------------------------------
093200110628       BEGSR  sr_CloseCursor_4;
093300110628
093400110628         // -?Chiusura del cursore?
093500110628         exec sql  close C4;
093600110628
093700110628       ENDSR;
093800110628
093900110628       //--------------------------------------------------------------
094000110628       //?Elaborazione totale spedizioni nel periodo x CCM da TITAS33C ?
094100110628       //--------------------------------------------------------------
094200110628       BEGSR  sr_Elab_4;
094300110628
094400110628         // -?Conteggio numero spedizioni?
094500110628         WE21nts += wSQL_ds_TITAS.sqlTOTspe;
094600110628
094700110628         // -?Conteggio numero colli?
094800110628         WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
094900110628
095000110628         // -?Memorizzazione data ULTIMA spedizione?
095100110628         if  wSQL_ds_TITAS.sqlTASdts > WE21dus;
095200110628           WE21dus = wSQL_ds_TITAS.sqlTASdts;
095300110628         endif;
095400110628
095500110628       ENDSR;
095600110628
095700110628       //--------------------------------------------------------------
095800110628       //?Scrittura record in work-file WFES210F.                      ?
095900110628       //--------------------------------------------------------------
096000110628       BEGSR  sr_Wrt_WFES210;
096100110628
096200110628         // -?Dati nel trk del work-file WFES210F?
096300110628         // -?Parametri di lancio?
096400110628         WE21pdti  = D07dti;
096500110628         WE21pdtf  = D07dtf;
096600110628         WE21pcver = D07cver;
096700110628         WE21pcdsc = D07cdsc;
096800110628         // -?Distretto?
096900110628         WE21cdi = ORGfl3;
097000110628         WE21ddi = ds17.§17des;
097100110628         // -?Area?
097200110628         WE21car = ORGcar;
097300110628         WE21dar = ds05.§05des;
097400110628         // -?Cliente unificante?
097500110628         WE21ccu = ds3C.§3Ccks;
097600110628         WE21rsu = CNACOds_Uni.ACOrag;
097700110628         // -?Cliente mittente tab. "3C"?
097800110628         WE21ccm = %int( %triml( wSQL_ds_3C.sqlTBLkey ) );
097900140911         //WE21rsm = ds3C.§3Crag;
098000140911         WE21rsm = ACORag;
098100110628         WE21blo = ACOabl;
098200110628         // -?Supporto Cliente => BRT?
098300110628         WE21scb = wSupporto;
098400110628         if  wSupporto = c_SuppCB_ESWEB;
098500110628           // -?Dati dalla tab. "3EW"?
098600110628           WE21EWlnp = d3EW.§3EWlnp;
098700120321           WE21fls   = d3EW.§3EWfls;
098800110628           WE21nrs   = d3EW.§3EWnrs;
098900110628           WE21nsi   = d3EW.§3EWnsi;
099000110628           WE21nsf   = d3EW.§3EWnsf;
099100110628           WE21EWctm = d3EW.§3EWctm;
099200110628           WE21EWmad = d3EW.§3EWmad;
099300110628           WE21EWaew = d3EW.§3EWaew;
099400110628         else;
099500110628           // -?Dati dalla tab. "3C"?
099600120321           WE21fls   = ds3C.§3Cfls;
099700110628           WE21nrs   = ds3C.§3Cnrs;
099800110628           WE21nsi   = wSgnClI;
099900110628           WE21nsf   = d3CP.§3CPal;
100000110628         endif;
100100110630         // -?Totali (già calcolati in subr. "sr_Elab_4")?
100200110628         //WE21nts += wSQL_ds_TITAS.sqlTOTspe;
100300110628         //WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
100400110628         //WE21dus  = wSQL_ds_TITAS.sqlTASdts;
100500110706         // -?Dati EasySped (NON per "EasySped-Validate")?
100600110706         if  wSupporto <> c_SuppCB_ESVAL;
100700110706           WE21edat = d3CE.§3CEedat;
100800110706           WE21ever = d3CE.§3CEever;
100900110706           WE21cver = d3CE.§3CEcver;
101000110706           WE21cdsc = d3CE.§3CEcdsc;
101100110706           WE21typw = d3CE.§3CEtypweb;
101200110706           WE21cvermx = d3CE.§3CEcvermx;
101300110706           WE21cdscmx = d3CE.§3CEcdscmx;
101400110706         endif;
101500110630         // -?EasySpedWeb?
101600110706         select;
101700110706           when  wSupporto = c_SuppCB_ESWEB  or
101800110706                 wSupporto = c_SuppCB_ESVAL;
101900110706             WE21esweb = 'SI';
102000110706           when  WE21ever >= '4.0.3 ';
102100110706             WE21esweb = 'SI';
102200110706           other;
102300110706             WE21esweb = 'NO';
102400110706         endsl;
102500110630         // -?Cappario aggiornato?
102600110630         select;
102700110706           when  wSupporto = c_SuppCB_ESWEB;
102800110706             WE21cverag = 'SI';
102900110706           when  wSupporto = c_SuppCB_ESVAL;
103000110706             WE21cverag = 'NO';
103100110630           when  WE21cver   = *zero;
103200110630             WE21cverag = 'NO';
103300110630           when  WE21cver   = D07cver;
103400110630             WE21cverag = 'SI';
103500110630           when  WE21cverMx = D07cver;
103600110630             WE21cverag = 'SI';
103700120601           when  WE21cver   = (D07cver - 1)  and  WE21cdsc = D07cdsc;
103800120601             WE21cverag = 'SI';
103900110630           other;
104000110630             WE21cverag = 'NO';
104100110630         endsl;
104200140416         // -?Materiale in comodato?
104300140416         WE21stamco = $comodato  ;
104400141024         // -?Cod. e Nominativo commerciale
104500141027         WE21CMU = *zeros;
104600141027         WE21CMD = *blank;
104700141027         // se esiste un cod. unificante
104800141027         if WE21CCU <> *zeros;
104900141027           // reperisco il suo cod.agente
105000141024           clear  TIBS69ds;
105100141024           I69sif = knsif;
105200141024           I69kcc = DUTkci;
105300141024           I69kcp = WE21CCU;
105400141024           tibs69r( tibs69ds :
105500141024                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
105600141024           if  O69err = *on;
105700141024             clear  CNCLPds_Uni;
105800141024           else;
105900141024             CNCLPds_Uni = ds_CNCLP;
106000141024           endif;
106100141027           // reperisco il cod. commerciale unificante mediante l'agente del cliente
106200141027           k_CMMCOD = CLPAGE;
106300141027           chain  %kds(k01azcmm01)  AZCMM000;
106400141027           if  %found();
106500141027             WE21CMU = CMMUNI;
106600141027             k_CMMCOD = WE21CMU;
106700141027             chain  %kds(k01azcmm01)  AZCMM000;
106800141027             // reperisco la descrizione del cod. commerciale unificante
106900141027             if  %found();
107000141027               WE21CMD = CMMDES;
107100141027             endif;
107200141027           endif;
107300141024         EndIf;
107400110628
107500110628         //______________
107600110628         WRITE  WFES2100;
107700110628         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
107800110628
107900110628       ENDSR;
108000110628
108100110628       //--------------------------------------------------------------
108200110628       //?Reperimento stampanti in concordato da tab. "3R"             ?
108300110628       //--------------------------------------------------------------
108400140416       //BEGSR  sr_StampConc;
108500110628
108600140416       //  clear  ds3R;
108700110628
108800110628         // -?1° tentativo con il cliente in esame?
108900140416       //  clear  yy;
109000140416       //  yy = %lookup( %trimr( wSQL_ds_3C.sqlTBLkey ) : $Cli );
109100110628
109200110628         // -?2° tentativo con il padre del cliente in esame?
109300140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
109400140416       //    clear  yy;
109500140416       //    yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Cli );
109600140416       //  EndIf;
109700110628
109800110628         // -?3° tentativo con gli altri fratelli?
109900140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
110000140416       //    clear  yy;
110100140416       //    DoU  yy = *zero   or   $StpComodato(yy) <> *blank;
110200140416       //      yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Padre : yy + 1 );
110300140416       //    EndDo;
110400140416       //  EndIf;
110500110628
110600110628         // -?Imposta la stampante in ds3R (se reperita)?
110700140416       //  If  yy > *zero;
110800140416       //    ds3R.§3Rnot = $StpComodato(yy);
110900140416       //  EndIf;
111000140416       //--------------------------------------------------------------
111100140416       //?Verifica presenza comodati            ?
111200140416       //--------------------------------------------------------------
111300140416       BEGSR  sr_StampCom;
111400140415         clear $comodato;
111500140414         // -?Con il cliente in esame verifico legame "ST"
111600140414         //   Verifico se codice presente come padre
111700140414         clear tibs10ds;
111800140414         d10tle='ST';
111900140414         d10paf='F';
112000140415         d10cod=%dec(%trimr( wSQL_ds_3C.sqlTBLkey ):11:0);
112100140415         tibs10r(tibs10ds);
112200140415         if d10cop<=0;
112300140415         //   Se non è padre verifico se è figlio
112400140415            d10paf='P';
112500140415            tibs10r(tibs10ds);
112600140415            if d10cop<=0;
112700140415         //  Se non è nè padre nè figlio è cliente singolo
112800140415               d10cop=d10cod;
112900141016               skc(1)=%editc(d10cod:'X');
113000140415            endif;
113100140415         endif;
113200140416         // -?Verifico se il padre del legame ST è già stato analizzato per
113300140416         // -?i comodati, altrimenti lo analizzo e lo memorizzo in schiera
113400140416         // -?Schiera $padst (clienti padri legame ST)
113500140416         // -?Schiera $com   (indica se presente comodato S=Comodato)
113600140415         yy=%lookup(d10cop:$padst);
113700140415         if yy=0;
113800140415            yy=%lookup(0:$padst);
113900140415            $padst(yy)=d10cop      ;
114000140415         // Verifico se presente materiale in comodato nella famiglia e
114100140415         // memorizzo in schiera
114200140416            clear wcom;
114300140416            for i=1 to %elem(sKC);
114400140416               if skc(i)=*blanks or skc(i)=*zeros or wcom='S';
114500140415                  leave;
114600140415               endif;
114700140416               kcli=%dec(%subst(skc(I):5:7):7:0);
114800140415               exsr sr_comodato;
114900140416               if wcom = 'S';
115000140416                  $com(yy)='S';
115100140416               endif ;
115200140415            endfor;
115300140415         endif;
115400140415         if $com(yy)='S';
115500140415            $comodato='Materiale in comodato';
115600140415         endif;
115700110628       ENDSR;
115800110628
115900140416       //--------------------------------------------------------------
116000140416       //?Ricerca comodato per cliente                                 ?
116100140416       //--------------------------------------------------------------
116200140416       BEGSR  sr_Comodato;
116300140416       setll kcli uncmd01l;
116400140416       reade kcli uncmd01l;
116500140416       dow not %eof(uncmd01l) ;
116600140416          if cmdcfg<>'A' and cmdcfg<>'C';
116700140416             wcom='S';
116800140416             leave;
116900140416          endif;
117000140416          reade kcli uncmd01l;
117100140416       enddo;
117200140416       endsr;
117300110628       //--------------------------------------------------------------
117400110628       //?Apertura cursore C3 (creazione WFES220F con totali x area).  ?
117500110628       //--------------------------------------------------------------
117600110628       BEGSR  sr_OpenCursor_3;
117700110628
117800110628         // -?Dichiarazione cursore?
117900110628         exec sql  declare C3  cursor for
118000110628                   select *
118100110628                     from WFES210F
118200110628                    order by WE21car, WE21scb, WE21ccu, WE21ccm
118300110628                      for read only;
118400110628
118500110628         // -?Apertura cursore?
118600110628         exec sql  open C3;
118700110628
118800110628         clear  WFES2200;
118900110628
119000110628       ENDSR;
119100110628
119200110628       //--------------------------------------------------------------
119300110628       //?Lettura cursore C3 (creazione WFES220F con totali x area).
119400110628       //--------------------------------------------------------------
119500110628       BEGSR  sr_ReadCursor_3;
119600110628
119700110628         // -?Lettura cursore?
119800110628         exec sql  fetch  NEXT  from C3  into :WFES210ds;
119900110628
120000110628         select;
120100110628           when  SQLcode = 100;
120200110628             $EoF_3 = *on;
120300110628           when  SQLcode < *zero;
120400110628             $EoF_3 = *on;
120500110628             exsr  sr_PrintErr;
120600110628           other;
120700110628             exsr  sr_Elab_3;
120800110628         endsl;
120900110628
121000110628       ENDSR;
121100110628
121200110628       //--------------------------------------------------------------
121300110628       //?Creazione work-file WFES220F con totali per area.            ?
121400110628       //--------------------------------------------------------------
121500110628       BEGSR  sr_Elab_3;
121600110628
121700110628         // -?Rottura Area?
121800111122         IF  WFES210ds.WE21car <> WE22car;
121900110628
122000110628           // -?Scrittura record con i totali dell'area precedente?
122100110628           if  WE22car <> *zero;
122200111123             exsr  sr_Wrt_WFES220;
122300110628           endif;
122400110628
122500110628           // -?Impostazione dati nuova area?
122600110628           clear  WFES2200;
122700110628           WE22pdti  = WFES210ds.WE21pdti;
122800110628           WE22pdtf  = WFES210ds.WE21pdtf;
122900110628           WE22pcver = WFES210ds.WE21pcver;
123000110628           WE22pcdsc = WFES210ds.WE21pcdsc;
123100110628           WE22car   = WFES210ds.WE21car;
123200110628           WE22dar   = WFES210ds.WE21dar;
123300110628
123400110628         ENDIF;
123500110628
123600110628
123700110628         // -?Incremento totali dell'area in elaborazione?
123800111122         Select;
123900111122
124000111122           // -? EasySpeb ?
124100111123           When  WFES210ds.WE21scb = c_SuppCB_ESYSP;
124200111122             // -?Num. totale spedizioni nel periodo?
124300111122             WE22esnts  += WFES210ds.WE21nts;
124400111122             // -?Num. totale colli nel periodo?
124500111122             WE22esntc  += WFES210ds.WE21ntc;
124600111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
124700111122             if  WFES210ds.WE21stamco = *blank;
124800111122               WE22esntcn += WFES210ds.WE21ntc;
124900111122             endif;
125000111122             // -?Num. totale clienti padre?
125100111122             if  WFES210ds.WE21ccu <> SAVccu;
125200111122               SAVccu = WFES210ds.WE21ccu;
125300111122               WE22esnksu += 1;
125400111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
125500111122               exsr  sr_Elab_5;
125600111122               if  wNTS = *zero;
125700111122                 WE22esnuns += 1;
125800111122               endif;
125900111122             endif;
126000111122             // -?Num. totale clienti in tab. "3C"?
126100111122             WE22esncli += 1;
126200111123             //  ?(scartati quelli bloccati)?
126300111122             IF  WFES210ds.WE21blo = *blank;
126400111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
126500111122               If  WFES210ds.WE21nts > *zero;
126600111122                 WE22esncls += 1;
126700111122                 // ·?...ma SENZA cappario aggiornato?
126800111122                 if  WFES210ds.WE21cverag = 'NO';
126900111122                   WE22esncca += 1;
127000111122                 endif;
127100111122                 // ·?...ma SENZA EasySpedWeb?
127200111122                 if  WFES210ds.WE21esweb  = 'NO';
127300111122                   WE22esnclw += 1;
127400111122                 endif;
127500111122               EndIf;
127600111122             ENDIF;
127700111122
127800111122           // -? EasySpeb Validate ?
127900111123           When  WFES210ds.WE21scb = c_SuppCB_ESVAL;
128000111122             // -?Num. totale spedizioni nel periodo?
128100111122             WE22evnts  += WFES210ds.WE21nts;
128200111122             // -?Num. totale colli nel periodo?
128300111122             WE22evntc  += WFES210ds.WE21ntc;
128400111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
128500111122             if  WFES210ds.WE21stamco = *blank;
128600111122               WE22evntcn += WFES210ds.WE21ntc;
128700111122             endif;
128800111122             // -?Num. totale clienti padre?
128900111122             if  WFES210ds.WE21ccu <> SAVccu;
129000111122               SAVccu = WFES210ds.WE21ccu;
129100111122               WE22evnksu += 1;
129200111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
129300111122               exsr  sr_Elab_5;
129400111122               if  wNTS = *zero;
129500111122                 WE22evnuns += 1;
129600111122               endif;
129700111122             endif;
129800111122             // -?Num. totale clienti in tab. "3C"?
129900111122             WE22evncli += 1;
130000111123             //  ?(scartati quelli bloccati)?
130100111122             IF  WFES210ds.WE21blo = *blank;
130200111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
130300111122               If  WFES210ds.WE21nts > *zero;
130400111122                 WE22evncls += 1;
130500111122               EndIf;
130600111122             ENDIF;
130700111123
130800111123           // -? EasySpeb WEB ?
130900111123           When  WFES210ds.WE21scb = c_SuppCB_ESWEB;
131000111123             // -?Num. totale spedizioni nel periodo?
131100111123             WE22ewnts  += WFES210ds.WE21nts;
131200111123             // -?Num. totale colli nel periodo?
131300111123             WE22ewntc  += WFES210ds.WE21ntc;
131400111123             // -?Num. totale spedizioni e colli senza stampanti in comodato?
131500111123             if  WFES210ds.WE21stamco = *blank;
131600111123               WE22ewntcn += WFES210ds.WE21ntc;
131700111123             endif;
131800111123             // -?Num. totale clienti padre?
131900111123             if  WFES210ds.WE21ccu <> SAVccu;
132000111123               SAVccu = WFES210ds.WE21ccu;
132100111123               WE22ewnksu += 1;
132200111123               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
132300111123               exsr  sr_Elab_5;
132400111123               if  wNTS = *zero;
132500111123                 WE22ewnuns += 1;
132600111123               endif;
132700111123             endif;
132800111123             // -?Num. totale clienti in tab. "3C"?
132900111123             WE22ewncli += 1;
133000111123             //  ?(scartati quelli bloccati)?
133100111123             IF  WFES210ds.WE21blo = *blank;
133200111123               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
133300111123               If  WFES210ds.WE21nts > *zero;
133400111123                 WE22ewncls += 1;
133500111123               EndIf;
133600111123             ENDIF;
133700111122
133800111122         EndSl;
133900110628
134000110628       ENDSR;
134100110628
134200110628       //--------------------------------------------------------------
134300110630       //?Chiusura cursore C3 (creazione WFES220F con totali x area).
134400110628       //--------------------------------------------------------------
134500110628       BEGSR  sr_CloseCursor_3;
134600110628
134700110628         // -?Scrittura ultimo record?
134800110628         if  WE22car <> *zero;
134900111123           exsr  sr_Wrt_WFES220;
135000110628         endif;
135100110628
135200110628         // -?Chiusura cursore?
135300110628         exec sql  close C3;
135400110628
135500110628       ENDSR;
135600111123
135700111123       //--------------------------------------------------------------
135800111123       //?Scrittura record in work-file WFES220F.                      ?
135900111123       //--------------------------------------------------------------
136000111123       BEGSR  sr_Wrt_WFES220;
136100111123
136200111123         // -? Totali AREA ?
136300111123         // ·?Num. totale spedizioni nel periodo?
136400111123         WE22tants  = WE22esnts + WE22evnts + WE22ewnts;
136500111123         // ·?Num. totale colli nel periodo?
136600111123         WE22tantc  = WE22esntc + WE22evntc + WE22ewntc;
136700111123         // ·?Num. totale spedizioni e colli senza stampanti in comodato?
136800111123         WE22tantcn = WE22esntcn + WE22evntcn + WE22ewntcn;
136900111123         // ·?Num. totale clienti padre?
137000111123         WE22tanksu = WE22esnksu + WE22evnksu + WE22ewnksu;
137100111123         // ·?"Raggruppamento" clienti SENZA spedizioni nel periodo?
137200111123         WE22tanuns = WE22esnuns + WE22evnuns + WE22ewnuns;
137300111123         // ·?Num. totale clienti in tab. "3C"?
137400111123         WE22tancli = WE22esncli + WE22evncli + WE22ewncli;
137500111123         // ·?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
137600111123         //  ?(scartati quelli bloccati)?
137700111123         WE22tancls = WE22esncls + WE22evncls + WE22ewncls;
137800111123
137900111123         //______________
138000111123         WRITE  WFES2200;
138100111123         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
138200111123
138300111123       ENDSR;
138400110628
138500110628       //--------------------------------------------------------------
138600110628       //?Verifica se padre & figli senza spedizioni nel periodo.      ?
138700110628       //--------------------------------------------------------------
138800110628       BEGSR  sr_Elab_5;
138900110628
139000110628         // -?Dichiarazione cursore?
139100110628         exec sql  declare C5  cursor for
139200110628                    select sum(WE21nts)
139300110628                      from WFES210F
139400110628                     where WE21ccu = :WFES210ds.WE21ccu
139500110628                       for read only;
139600110628
139700110628         // -?Apertura cursore?
139800110628         exec sql  open C5;
139900110628
140000110628         // -?Lettura cursore?
140100110628         exec sql  fetch  NEXT  from C5  into :wNTS;
140200110628
140300110628         select;
140400110628           when  SQLcode = 100;
140500110628             $EoF_5 = *on;
140600110628           when  SQLcode < *zero;
140700110628             $EoF_5 = *on;
140800110628             exsr  sr_PrintErr;
140900110628           other;
141000111122             //if  wNTS = *zero;
141100111122             //  WE22nksuns += 1;
141200111122             //endif;
141300110628         endsl;
141400110628
141500110628         // -?Chiusura cursore?
141600110628         exec sql  close C5;
141700110628
141800110628       ENDSR;
141900110628
142000110628       //--------------------------------------------------------------
142100110628       //?Ultimo aggiornamento (dati di EasySped & relativo flag).     ?
142200110628       //--------------------------------------------------------------
142300110628       BEGSR  sr_Upd_EsySp;
142400110628
142500110628         exsr  sr_OpenCursor_6;
142600110628
142700110628         clear wEsySp_ds;
142800110628         dou  $Eof_6 = *on;
142900110628           exsr  sr_ReadCursor_6;
143000110628         enddo;
143100110628
143200110628         exsr  sr_CloseCursor_6;
143300110628
143400110628       ENDSR;
143500110628
143600110628       //--------------------------------------------------------------
143700110628       //?Apertura cursore C6.                                         ?
143800110628       //--------------------------------------------------------------
143900110628       BEGSR  sr_OpenCursor_6;
144000110628
144100110628         // -?Dichiarazione cursore?
144200151023         exec sql  declare C6 insensitive cursor for
144300110628
144400151023                    select WE21ccu, WE21ccm, WE21scb, WE21flgu,
144500110628                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
144600110628                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg,
144700110628                           rrn(WFES210F)
144800110628                      from WFES210F
144900110628                     order by WE21ccu, WE21cver desc
145000110628
145100110628                       for fetch only;
145200110628
145300110628         // -?Apertura cursore?
145400110628         exec sql  open C6;
145500110628
145600110628       ENDSR;
145700110628
145800110628       //--------------------------------------------------------------
145900110628       //?Chiusura cursore C6.                                         ?
146000110628       //--------------------------------------------------------------
146100110628       BEGSR  sr_CloseCursor_6;
146200110628
146300110628         // -?Chiusura cursore?
146400110628         exec sql  close C6;
146500110628
146600110628       ENDSR;
146700110628
146800110628       //--------------------------------------------------------------
146900110628       //?Lettura cursore C6 (rec. WFES210F senza info EasySped).      ?
147000110628       //--------------------------------------------------------------
147100110628       BEGSR  sr_ReadCursor_6;
147200110628
147300110628         // -?Lettura cursore?
147400110628         exec sql  fetch  NEXT  from C6  into :wSQL_ds_WFES21;
147500110628
147600110628         select;
147700110628           when  SQLcode = 100;
147800110628             $Eof_6 = *on;
147900110628           when  SQLcode < *zero;
148000110628             $Eof_6 = *on;
148100110628           other;
148200110628             exsr  sr_Elab_6;
148300110628         endsl;
148400110628
148500110628       ENDSR;
148600110628
148700110628       //--------------------------------------------------------------
148800110630       //?Aggiornamento rec. WFES210F per info EasySped.               ?
148900110628       //--------------------------------------------------------------
149000110628       BEGSR  sr_Elab_6;
149100110628
149200110628         select;
149300110628
149400110628           // -?Record con la versione: salvo cod. cliente + dati della?
149500110628           //  ?versione?
149600111117           //  ?NO: la versione (anche se meno recente) del 2° cliente?
149700111117           //      ?della famiglia si sovrapporrebbe a quella del 1°...?
149800111117           //when  wSQL_ds_WFES21.sqlWESever <> *blank;
149900111117
150000111117           // -?1° Record con la versione: salvo cod. cliente + dati?
150100111117           //  ?della versione?
150200111117           //  ?Meglio tenere comunque la versione più recente (quella?
150300111117           //  ?nel 1° record letto via vedi SQL)!?
150400151023           //when  wSQL_ds_WFES21.sqlWESccu <> ESYWESccu;
150500151023           //  wEsySp_ds = wSQL_ds_WFES21;
150600110628
150700111117           // -?Record senza la versione: se unificante = al precedente?
150800110628           //  ?aggiorno il record con la versione salvata?
150900151023           //when  wSQL_ds_WFES21.sqlWESever = *blank  and
151000151023           //      wSQL_ds_WFES21.sqlWESccu  = ESYWESccu;
151100151023           //  chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
151200151023           //  if  %found(WFES210F);
151300151023           //    WE21flgu = 'U';
151400151023           //    WE21edat = ESYWESedat;
151500151023           //    WE21ever = ESYWESever;
151600151023           //    WE21cver = ESYWEScver;
151700151023           //    WE21typw = ESYWEStypw;
151800151023           //    WE21cdsc = ESYWEScdsc;
151900151023           //    WE21cvermx = ESYWEScvermax;
152000151023           //    WE21cdscmx = ESYWEScdscmax;
152100151023           //    WE21esweb  = ESYWESesweb;
152200151023           //    WE21cverag = ESYWEScveragg;
152300151023
152400151023           // Record senza la versione: reperisco da WFES210F il rcd dell'unificante (se il cliente
152500151023           // non è esso stesso l'unificante) e aggiorno il rcd in linea con i dati presi.
152600151023           // Qualora anche la versione dell'unificante fosse vuota, non faccio nulla.
152700151023           // E' dato per scontato che il cliente unificante abbia in tab. 3C se stesso come
152800151023           // unificante.
152900151023           when  wSQL_ds_WFES21.sqlWESscb  = 'ESYSP' and
153000151023                 wSQL_ds_WFES21.sqlWEScver = 0 and
153100151023                 wSQL_ds_WFES21.sqlWESccu <> wSQL_ds_WFES21.sqlWESccm;
153200151023             // leggo via SQL
153300151023             wCCM = wSQL_ds_WFES21.sqlWESccu;
153400151023             exsr sr_ReadCursor_7;
153500151023             if  $EoF_7 = *off;
153600151023               WE21flgu = 'U';
153700151023               WE21edat = ESYWESedat;
153800151023               WE21ever = ESYWESever;
153900151023               WE21cver = ESYWEScver;
154000151023               WE21typw = ESYWEStypw;
154100151023               WE21cdsc = ESYWEScdsc;
154200151023               WE21cvermx = ESYWEScvermax;
154300151023               WE21cdscmx = ESYWEScdscmax;
154400151023               WE21esweb  = ESYWESesweb;
154500151023               WE21cverag = ESYWEScveragg;
154600110628               //______________
154700110628               update WFES2100;
154800110628               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
154900110628             endif;
155000110628
155100110628         endsl;
155200151023
155300110628       ENDSR;
155400151023
155500151023       //--------------------------------------------------------------
155600151023       //?Lettura cursore C7 (1 solo rec. WFES210F avente CCM e CCU quelli passati).      ?
155700151023       //--------------------------------------------------------------
155800151023       BEGSR  sr_ReadCursor_7;
155900151023
156000151023         // -?Dichiarazione cursore?
156100151023         exec sql  declare C7 cursor for
156200151023
156300151023                    select WE21ccu, WE21ccm, WE21scb, WE21flgu,
156400151023                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
156500151023                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg
156600151023                      from WFES210F
156700151023                     where WE21ccm=:wCCM and WE21ccu=WE21ccm
156800151023                       for fetch only;
156900151023
157000151023         // -?Apertura cursore?
157100151023         exec sql  open C7;
157200151023
157300151023         // -?Lettura cursore?
157400151023         exec sql  fetch  next  from C7  into :wEsySp_ds;
157500151023
157600151023         select;
157700151023           when  SQLcode = 100;
157800151023             $Eof_7 = *on;
157900151023           when  SQLcode < *zero;
158000151023             $Eof_7 = *on;
158100151023           other;
158200151023             $Eof_7 = *off;
158300151023         endsl;
158400151023
158500151023         // -?Chiusura cursore?
158600151023         exec sql  close C7;
158700151023
158800151023       ENDSR;
158900110628
159000110628       //--------------------------------------------------------------
159100110628       //?Stampa segnalazione dell'errore rilevato.                    ?
159200110628       //--------------------------------------------------------------
159300110628       BEGSR  sr_PrintErr;
159400110628
159500110628         // -?Stampa del Dump?
159600110628         Dump(A);
159700110628
159800110628         // -?Stampa del Job-Log?
159900110628         Qcmd = 'DSPJOBLOG job(*) output(*print)';
160000110628         exsr  sr_ExecCmd;
160100110628
160200110628         // -?Chiusura del programma?
160300110628         exsr  sr_RoutEnd;
160400110628
160500110628       ENDSR;
160600110628
160700110628       //--------------------------------------------------------------
160800110628       //?Esecuzione del comando (già impostato).                      ?
160900110628       //--------------------------------------------------------------
161000110628       BEGSR  sr_ExecCmd;
161100110628
161200110628         clear Qcap0100;
161300110628         Qcabcsdh = *off;
161400110628         Qcapa    = *off;
161500110628         Qcacmdss = *off;
161600110628         Qcaerved = *allX'00';
161700110628
161800110628         clear Qusec;
161900110628         Qusbprv  = %size(Qusec);
162000110628
162100110628         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
162200110628                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
162300110628                           0 : 0 : Qusec);
162400110628
162500110628         //if  Qusei <> *blank;
162600110628         //  ...
162700110628         //endif;
162800110628
162900110628       ENDSR;
163000110628
163100110628       //--------------------------------------------------------------
163200110628       //?Operazioni finali.                                           ?
163300110628       //--------------------------------------------------------------
163400110628       BEGSR  sr_RoutEnd;
163500110628
163600110628         // -?Chiusura work-file?
163700110628         close(e)  WFES220F;
163800110628         close(e)  WFES210F;
163900110628
164000110628         // -?Chiusura applicazioni?
164100110628         reset  TIBS69ds;
164200110628         tibs69r( tibs69ds :
164300110628                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
164400110628
164500110628         // -?Uscita del *pgm?
164600110628         return;
164700110628
164800110628       ENDSR;
164900110628
165000110628      /end-free
