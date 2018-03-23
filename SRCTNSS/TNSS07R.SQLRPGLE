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
017600151023     d  sqlWESflgu                         inz    like(WE21flgu)
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
019500110628     d                                     inz
019600151023     d  ESYwesflgu                         like(wSQL_ds_WFES21.sqlWESflgu)
019700151023     d                                     inz
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
030700110628       exsr  sr_RoutInz;
030800110628
030900110628       // -?1° ciclo di elaborazione: creazione WFES210F (dettaglio)?
031000110628       //                            ?per clienti EasySped?
031100110628       exsr  sr_OpenCursor_1;
031200110628
031300110628       DoU  $Eof_1 = *on;
031400110628         exsr  sr_ReadCursor_1;
031500110628       EndDo;
031600110628
031700110628       exsr  sr_CloseCursor_1;
031800110628
031900110628       // -?2° ciclo di elaborazione: creazione WFES210F (dettaglio)?
032000110628       //                            ?per clienti EasySpedWeb?
032100110628       exsr  sr_OpenCursor_2;
032200110628
032300110628       DoU  $Eof_2 = *on;
032400110628         exsr  sr_ReadCursor_2;
032500110628       EndDo;
032600110628
032700110628       exsr  sr_CloseCursor_2;
032800110628
032900110628       // -?Aggiornamento dati EasySped/EasySpedWeb mancanti...?
033000110628       exsr  sr_Upd_EsySp;
033100110628
033200110628       // -?3° ciclo di elaborazione: creazione WFES220F (totale area)?
033300110628       exsr  sr_OpenCursor_3;
033400110628
033500110628       dou  $EoF_3 = *on;
033600110628         exsr  sr_ReadCursor_3;
033700110628       enddo;
033800110628
033900110628       exsr  sr_CloseCursor_3;
034000110628
034100110628       // -?Fine programma?
034200110628       exsr  sr_RoutEnd;
034300110628
034400110628       //--------------------------------------------------------------
034500110628       //?Operazioni iniziali.                                         ?
034600110628       //--------------------------------------------------------------
034700110628       BEGSR  sr_RoutInz;
034800110628
034900110628         *inLR = *on;
035000110628
035100110628         // -?Impostazione opzioni per SQL?
035200110628         exec sql  set  option  DynUsrPrf = *Owner,
035300110628                                CloSqlCsr = *EndMod;
035400110628
035500110628         // -?Reperimento dati job?
035600110628         exsr  sr_DatiJob;
035700110628
035800110628         // -?Impostazione campi "fissi"?
035900110628         k_TBLkut = 1;
036000110628
036100110628         // -?Reperimento data odierna in formato aaaammgg?
036200110628         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
036300110628         //wData_Iso = %date(wDate : *iso);
036400110628
036500110628         // -?Impostazione eventuali parametri ricevuti?
036600110628         if  SDSprm > *zero  and
036700110628             KPJBU <> *blank;
036800110628           TNSS07ds = KPJBU;
036900110628         else;
037000110628           reset  TNSS07ds;
037100110628         endif;
037200110628
037300110628         // -?Pulizia work-files del lavoro sottomesso e loro apertura?
037400110628         Qcmd = c_Cmd_ClrWF1;
037500110628         exsr  sr_ExecCmd;
037600110628         if  Qusei <> *blank;
037700110628           exsr  sr_PrintErr;
037800110628         endif;
037900110628         open  WFES210F;
038000110628         Qcmd = c_Cmd_ClrWF2;
038100110628         exsr  sr_ExecCmd;
038200110628         if  Qusei <> *blank;
038300110628           exsr  sr_PrintErr;
038400110628         endif;
038500110628         open  WFES220F;
038600110628
038700140416         //clear  xx;
038800140416         //clear  $Cli;
038900140416         //clear  $Padre;
039000140416         //clear  $StpComodato;
039100110628
039200110628         // -?Intabellamento clienti e relativa famiglia da "3C"?
039300140416         //k_TBLcod = '3C';
039400140416         //setll  %kds(k03tabel00 : 2)  TABEL;
039500140416         //reade  %kds(k03tabel00 : 2)  TABEL;
039600140416         //DoW  Not  %eof(TABEL00F);
039700140416         //  if  TBLflg = *blank;
039800140416         //    xx += 1;
039900140416         //    ds3C       = TBLuni;
040000140416         //    $Cli(xx)   = %trimr( TBLkey );
040100140416         //    $Padre(xx) = %editc( ds3C.§3Ccks : 'X' );
040200140416         //  endif;
040300140416         //reade  %kds(k03tabel00 : 2)  TABEL;
040400140416         //EndDo;
040500110628
040600110628         // -?Intabellamento stampanti in comodato per cliente da "3R"?
040700140416         //k_TBLcod = '3R';
040800140416         //For  yy = 1  To  xx;
040900140416         //  k_TBLkey = $Cli(yy);
041000140416         //  chain  %kds(k03tabel00)  TABEL;
041100140416         //  if  %found(TABEL00F)   and   TBLflg = *blank;
041200140416         //    ds3R = TBLuni;
041300140416         //    $StpComodato(yy) = ds3R.§3Rnot;
041400140416         //  endif;
041500140416         //EndFor;
041600110628
041700110628       ENDSR;
041800110628
041900110628       //--------------------------------------------------------------
042000110628       //?Reperimento Dati del job (Utente/Operativi).                 ?
042100110628       //--------------------------------------------------------------
042200110628       BEGSR  sr_DatiJob;
042300110628
042400110628         in(E) §AzUte;
042500110628         if NOT %error;
042600110628           in(E) §DatiUte;
042700110628         endif;
042800110628         if  %error  or  RSut = *blank;
042900110628           clear TIBS34ds;
043000110628           tibs34r ( tibs34ds );
043100110628           in §AzUte;
043200110628           in §DatiUte;
043300110628         endif;
043400110628
043500110628       ENDSR;
043600110628
043700110628       //--------------------------------------------------------------
043800110628       //?Apertura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
043900110628       //?  estrazione clienti EasySped.                               ?
044000110628       //--------------------------------------------------------------
044100110628       BEGSR  sr_OpenCursor_1;
044200110628
044300110628         // -?Preparazione stringa SQL?
044400110628         wSql = 'with +
044500110628
044600110628                 Tab_3C as +
044700110628                     ( select TBLkey, TBLuni +
044800110628                         from TABEL00F +
044900110628                        where TBLflg = '' '' +
045000110628                          and TBLcod = ''3C'' +
045100110628                          and substr(TBLuni,  1, 2) > ''00'' +
045200110628                          and substr(TBLuni, 44, 5) in +
045300110628                              (''' + c_SuppCB_ESYSP + ''', +
045400110628                               ''' + c_SuppCB_ESVAL + ''') ), +
045500110628
045600110628                 Tab_3CP as +
045700110628                     ( select TBEke1, TBEke2, TBEuni +
045800110628                         from TNTBE00F +
045900110628                        where TBEatb = '' '' +
046000110628                          and TBEcod = ''3CP'' +
046100110628                          and TBEsif = '' '' +
046200110629                          and TBElin = '' '' ), +
046300110628
046400110628                 Tab_3CE as +
046500110628                     ( select TBEke1, TBEuni +
046600110628                         from TNTBE00F +
046700110628                        where TBEatb = '' '' +
046800110628                          and TBEcod = ''3CE'' +
046900110628                          and TBEke2 = '' '' +
047000110628                          and TBEsif = '' '' +
047100110628                          and TBElin = '' '' ) +
047200110628
047300110630                 select Tab_3C.*, +
047400110630                        case when Tab_3CP.TBEke1 > '' '' +
047500110630                             then Tab_3CP.TBEke1 else '' '' end, +
047600110630                        case when Tab_3CP.TBEke2 > '' '' +
047700110630                             then Tab_3CP.TBEke2 else '' '' end, +
047800110630                        case when Tab_3CP.TBEuni > '' '' +
047900110630                             then Tab_3CP.TBEuni else '' '' end, +
048000110630                        case when Tab_3CE.TBEke1 > '' '' +
048100110630                             then Tab_3CE.TBEke1 else '' '' end, +
048200110630                        case when Tab_3CE.TBEuni > '' '' +
048300110630                             then Tab_3CE.TBEuni else '' '' end  +
048400110628
048500110628                   from Tab_3C left outer join Tab_3CP +
048600110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
048700110628                        substr(Tab_3CP.TBEke1, 1, 7) and +
048800110628                        substr(Tab_3C.TBLuni, 1, 2) = +
048900110628                        substr(Tab_3CP.TBEke2, 1, 2) +
049000110628
049100110628                               left outer join Tab_3CE +
049200110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
049300110630                        substr(Tab_3CE.TBEke1, 1, 7)';
049400110628
049500110628         // -?Dichiarazione cursore?
049600110628         exec sql  prepare S1  from :wSql;
049700110628         exec sql  declare C1  cursor for S1;
049800110628
049900110628         // -?Apertura del cursore?
050000110628         exec sql  open C1;
050100110628
050200110628         if  SQLcode < *zero;
050300110628           $EoF_1 = *on;
050400110628           exsr  sr_PrintErr;
050500110628         endif;
050600110628
050700110628       ENDSR;
050800110628
050900110628       //--------------------------------------------------------------
051000110628       //?Lettura cursore C1 (tab. "3C", "3CP" e "3CE") per            ?
051100110628       //?  estrazione clienti EasySped.                               ?
051200110628       //--------------------------------------------------------------
051300110628       BEGSR  sr_ReadCursor_1;
051400110628
051500110628         clear  ds3C;
051600110630         reset  d3CP;
051700110628         clear  d3CE;
051800110630         reset  wSgnClI;
051900110628         clear  wSQL_ds_3C;
052000110628         clear  wSQL_ds_3CP;
052100110628         clear  wSQL_ds_3CE;
052200110628
052300110628         // -?Lettura cursore?
052400110628         exec sql  fetch next  from C1  into :wSQL_ds_3C,
052500110628                                             :wSQL_ds_3CP,
052600110628                                             :wSQL_ds_3CE;
052700110628
052800110628         select;
052900110628           // -?Fine flusso?
053000110628           when  SQLcode = 100;
053100110628             $EoF_1 = *on;
053200110628           // -?Errore?
053300110628           when  SQLcode < *zero;
053400110628             exsr  sr_PrintErr;
053500110628             $EoF_1 = *on;
053600110628           // -?Elaborazione dati estratti?
053700110628           other;
053800110628             exsr  sr_Elab_1;
053900110628         endsl;
054000110628
054100110628       ENDSR;
054200110628
054300110628       //--------------------------------------------------------------
054400110628       //?Chiusura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
054500110628       //?  estrazione clienti EasySped.                               ?
054600110628       //--------------------------------------------------------------
054700110628       BEGSR  sr_CloseCursor_1;
054800110628
054900110628         // -?Chiusura del cursore?
055000110628         exec sql  close C1;
055100110628
055200110628       ENDSR;
055300110628
055400110628       //--------------------------------------------------------------
055500110628       //?Elaborazione singolo cliente estratto (EasySped).            ?
055600110628       //--------------------------------------------------------------
055700110628       BEGSR  sr_Elab_1;
055800110628
055900110628         // -?Valorizzazione tab. "3C"?
056000110628         ds3C = wSQL_ds_3C.sqlTBLuni;
056100110628
056200110628         // -?Valorizzazione tab. "3CP"?
056300110628         if  wSQL_ds_3CP.sqlTBEke1 > *zero  and
056400110628             wSQL_ds_3CP.sqlTBEke2 > *zero;
056500110628           Monitor;
056600110628             wSgnClI = %dec( %subst( wSQL_ds_3CP.sqlTBEke2 : 3 : 7 )
056700110628                             : 7 : 0 );
056800110628             On-Error  105;
056900110628               reset  wSgnClI;
057000110628               reset  d3CP;
057100110628           EndMon;
057200110628           if  wSQL_ds_3CP.sqlTBEuni > *zero;
057300110628             d3CP = wSQL_ds_3CP.sqlTBEuni;
057400110628           endif;
057500110628         endif;
057600110628
057700110628         // -?Valorizzazione tab. "3CE"?
057800110628         if  wSQL_ds_3CE.sqlTBEke1 > *blank;
057900110628           d3CE = wSQL_ds_3CE.sqlTBEuni;
058000110628         endif;
058100110628
058200110628         // -?Pulizia iniziale del record da registrare?
058300110628         clear  WFES2100;
058400110628
058500110628
058600110628         // -?Reperimento Distretto ed Area?
058700110628         //  ?(a "rottura" di filiale del cliente unificante)?
058800110628         k_ORGfil = %int( %subst( %editc( ds3C.§3Ccks : 'X' ) : 1 : 3 ) );
058900110628         if  k_ORGfil <> ORGfil;
059000110628           chain  %kds(k01azorg01)  AZORG;
059100110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
059200110628             clear  ORGfl3;
059300110628             clear  ORGcar;
059400110628             clear  ds17;
059500110628             clear  ds05;
059600110628           endif;
059700110628         endif;
059800110628
059900110628         // -?Decodifica distretto?
060000110628         if  ORGfl3 <> SAVcdi;
060100110628           SAVcdi = ORGfl3;
060200110628           clear  ds17;
060300110628           k_TBLkut = 1;
060400110628           k_TBLcod = '17';
060500110628           k_TBLkey = ORGfl3;
060600110628           chain  %kds(k03tabel00)  TABEL;
060700110628           if  %found(TABEL00F)  and  TBLflg = *blank;
060800110628             ds17 = TBLuni;
060900110628           endif;
061000110628         endif;
061100110628
061200110628         // -?Decodifica area?
061300110628         if  ORGcar <> SAVcar;
061400110628           SAVcar = ORGcar;
061500110628           clear  ds05;
061600110628           k_TBLkut = 1;
061700110628           k_TBLcod = '05';
061800110628           k_TBLkey = %editc( ORGcar : 'X' );
061900110628           chain  %kds(k03tabel00)  TABEL;
062000110628           if  %found(TABEL00F)  and  TBLflg = *blank;
062100110628             ds05 = TBLuni;
062200110628           endif;
062300110628         endif;
062400110628
062500110628         // -?Decodifica unificante?
062600110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
062700110628           clear  TIBS69ds;
062800110628           I69sif = knsif;
062900110628           I69kcc = DUTkci;
063000110628           I69kac = ds3C.§3Ccks;
063100110628           tibs69r( tibs69ds :
063200110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
063300110628           if  O69err = *on;
063400110628             clear  CNACOds_Uni;
063500110628           else;
063600110628             CNACOds_Uni = ds_CNACO;
063700141027           endif;
063800110628         EndIf;
063900110628
064000110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
064100110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
064200110628           clear  TIBS69ds;
064300110628           I69sif = knsif;
064400110628           I69kcc = DUTkci;
064500110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
064600110628           tibs69r( tibs69ds :
064700110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064800110628           if  O69err = *on;
064900110628             clear  ds_CNACO;
065000110628           endif;
065100110628         EndIf;
065200110628
065300140416         // -?Reperimento materiale in comodato file UNCMD00F ?
065400140416         exsr  sr_StampCom;
065500110628
065600110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
065700110628         // -?(reperimento dati da spedizioni associate)?
065800110628         $Eof_4 = *off;
065900110628         exsr  sr_OpenCursor_4;
066000110628
066100110628         DoU  $Eof_4;
066200110628           exsr  sr_ReadCursor_4;
066300110628         EndDo;
066400110628
066500110628         exsr  sr_CloseCursor_4;
066600110628
066700110628         // -?Scrittura del record a totale per cliente?
066800110629         wSupporto = ds3C.§3Ccba;
066900110628         exsr  sr_Wrt_WFES210;
067000110628
067100110628       ENDSR;
067200110628
067300110628       //--------------------------------------------------------------
067400110628       //?Apertura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
067500110628       //?  estrazione clienti EasySpedWeb.                            ?
067600110628       //--------------------------------------------------------------
067700110628       BEGSR  sr_OpenCursor_2;
067800110628
067900110628         // -?Preparazione stringa SQL?
068000110628         wSql = 'with +
068100110628
068200110628                 TIVSS as +
068300110628                     ( select VSSksu, VSSsun, VSSisv +
068400110628                         from TIVSS00F +
068500110628                        where VSSisv = ''EW'' and +
068600110628                             (VSSdsc = 0 or VSSdsc > ' +
068700110628                              %char(wDate) + ') ), +
068800110628
068900110628                 Tab_3EW as +
069000110628                     ( select TBEke1, TBEke2, TBEuni +
069100110628                         from TNTBE00F +
069200110628                        where TBEatb = '' '' and +
069300120814                              TBEcod = ''3EW'' and +
069400120814                              substr(TBEuni, 33, 1) = '' '' ), +
069500110628
069600110628                 Tab_3C as +
069700110628                     ( select TBLkey, TBLuni +
069800110628                         from TABEL00F +
069900110628                        where TBLflg = '' '' and +
070000110628                              TBLcod = ''3C'' and +
070100110628                              substr(TBLuni, 44, 5) = +
070200110630                              ''' + c_SuppCB_ESWEB + ''') +
070300110628
070400110628                 select * +
070500110628
070600110628                   from TIVSS inner join Tab_3EW +
070700110628                     on TIVSS.VSSksu = Tab_3EW.TBEke1  and +
070800110628                        TIVSS.VSSsun = Tab_3EW.TBEke2 +
070900110628
071000110628                              inner join Tab_3C +
071100110628                     on substr(TIVSS.VSSksu, 2, 7) = +
071200110628                        substr(Tab_3C.TBLuni, 79, 7)';
071300110628
071400110628         // -?Dichiarazione cursore?
071500110628         exec sql  prepare S2  from :wSql;
071600110628         exec sql  declare C2  cursor for S2;
071700110628
071800110628         // -?Apertura del cursore?
071900110628         exec sql  open C2;
072000110628
072100110628         if  SQLcode < *zero;
072200110628           $EoF_2 = *on;
072300110628           exsr  sr_PrintErr;
072400110628         endif;
072500110628
072600110628       ENDSR;
072700110628
072800110628       //--------------------------------------------------------------
072900110628       //?Lettura cursore C2 (file TIVSS, tab. "3EW" e tab. "3C") per  ?
073000110628       //?  estrazione clienti EasySpedWeb.                            ?
073100110628       //--------------------------------------------------------------
073200110628       BEGSR  sr_ReadCursor_2;
073300110628
073400110628         clear  d3EW;
073500110628         clear  ds3C;
073600110628         clear  wSQL_ds_TIVSS;
073700110628         clear  wSQL_ds_3EW;
073800110628         clear  wSQL_ds_3C;
073900110628
074000110628         // -?Lettura cursore?
074100110628         exec sql  fetch next  from C2  into :wSQL_ds_TIVSS,
074200110628                                             :wSQL_ds_3EW,
074300110628                                             :wSQL_ds_3C;
074400110628
074500110628         select;
074600110628           // -?Fine flusso?
074700110628           when  SQLcode = 100;
074800110628             $EoF_2 = *on;
074900110628           // -?Errore?
075000110628           when  SQLcode < *zero;
075100110628             exsr  sr_PrintErr;
075200110628             $EoF_2 = *on;
075300110628           // -?Elaborazione dati estratti?
075400110628           other;
075500110628             exsr  sr_Elab_2;
075600110628         endsl;
075700110628
075800110628       ENDSR;
075900110628
076000110628       //--------------------------------------------------------------
076100110628       //?Chiusura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
076200110628       //?  estrazione clienti EasySpedWeb.                            ?
076300110628       //--------------------------------------------------------------
076400110628       BEGSR  sr_CloseCursor_2;
076500110628
076600110628         // -?Chiusura del cursore?
076700110628         exec sql  close C2;
076800110628
076900110628       ENDSR;
077000110628
077100110628       //--------------------------------------------------------------
077200110629       //?Elaborazione singolo cliente estratto (EasySpedWeb).         ?
077300110628       //--------------------------------------------------------------
077400110628       BEGSR  sr_Elab_2;
077500110628
077600110628         d3EW = wSQL_ds_3EW.sqlTBEuni;
077700110628         ds3C = wSQL_ds_3C.sqlTBLuni;
077800110628
077900110630         // -?Pulizia iniziale del record da registrare?
078000110628         clear  WFES2100;
078100110628
078200110628         // -?Reperimento Area?
078300110628         //  ?(a "rottura" di filiale del cliente unificante)?
078400110628         k_ORGfil = %int( %subst( wSQL_ds_TIVSS.sqlVSSksu : 2 : 3 ) );
078500110628         if  k_ORGfil <> ORGfil;
078600110628           chain  %kds(k01azorg01)  AZORG;
078700110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
078800110629             clear  ORGfl3;
078900110628             clear  ORGcar;
079000110628           endif;
079100110628         endif;
079200110629
079300110629         // -?Decodifica distretto?
079400110629         if  ORGfl3 <> SAVcdi;
079500110629           SAVcdi = ORGfl3;
079600110629           clear  ds17;
079700110629           k_TBLkut = 1;
079800110629           k_TBLcod = '17';
079900110629           k_TBLkey = ORGfl3;
080000110629           chain  %kds(k03tabel00)  TABEL;
080100110629           if  %found(TABEL00F)  and  TBLflg = *blank;
080200110629             ds17 = TBLuni;
080300110629           endif;
080400110629         endif;
080500110628
080600110628         // -?Decodifica area?
080700110628         if  ORGcar <> SAVcar;
080800110628           SAVcar = ORGcar;
080900110628           clear  ds05;
081000110628           k_TBLkut = 1;
081100110628           k_TBLcod = '05';
081200110628           k_TBLkey = %editc( ORGcar : 'X' );
081300110628           chain  %kds(k03tabel00)  TABEL;
081400110628           if  %found(TABEL00F)  and  TBLflg = *blank;
081500110628             ds05 = TBLuni;
081600110628           endif;
081700110628         endif;
081800110628
081900110628         // -?Decodifica unificante?
082000110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
082100110628           clear  TIBS69ds;
082200110628           I69sif = knsif;
082300110628           I69kcc = DUTkci;
082400110628           I69kac = ds3C.§3Ccks;
082500110628           tibs69r( tibs69ds :
082600110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
082700110628           if  O69err = *on;
082800110628             clear  CNACOds_Uni;
082900110628           else;
083000110628             CNACOds_Uni = ds_CNACO;
083100110628           endif;
083200110628         EndIf;
083300110628
083400110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
083500110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
083600110628           clear  TIBS69ds;
083700110628           I69sif = knsif;
083800110628           I69kcc = DUTkci;
083900110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
084000110628           tibs69r( tibs69ds :
084100110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
084200110628           if  O69err = *on;
084300110628             clear  ds_CNACO;
084400110628           endif;
084500110628         EndIf;
084600110628
084700140416         // -?Reperimento materiale in comodato file UNCMD00F ?
084800140416         exsr  sr_StampCom ;
084900110628
085000110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
085100110628         // -?(reperimento dati da spedizioni associate)?
085200110628         $Eof_4 = *off;
085300110628         exsr  sr_OpenCursor_4;
085400110628
085500110628         DoU  $Eof_4;
085600110628           exsr  sr_ReadCursor_4;
085700110628         EndDo;
085800110628
085900110628         exsr  sr_CloseCursor_4;
086000110628
086100110628         // -?Scrittura del record a totale per cliente?
086200110629         wSupporto = ds3C.§3Ccba;
086300110628         exsr  sr_Wrt_WFES210;
086400110628
086500110628       ENDSR;
086600110628
086700110628       //--------------------------------------------------------------
086800110628       //?Apertura cursore C4 (totale spedizioni nel periodo x cliente)?
086900110628       //--------------------------------------------------------------
087000110628       BEGSR  sr_OpenCursor_4;
087100110628
087200110628         // -?Preparazione stringa SQL?
087300110628         //  ? N.B. ?- Seleziona le LdV del periodo richiesto?
087400110630         //  ?· Seleziona le LdV del cliente in esame da tab. "3C"?
087500110628         //  ?· Seleziona le LdV con la serie del cliente in esame?
087600110628         //  ?· Seleziona le LdV porto franco?
087700110628
087800110630         wSql_4 = 'select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
087900110630                          count(*), sum( TASncl ) +
088000110628                     from TITASP0F +
088100110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
088200110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
088300110628                          %editc( D07dti : 'X' ) + ' and ' +
088400110628                          %editc( D07dtf : 'X' ) +
088500110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
088600110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
088700110628                  ' group by TASccm +
088800110628
088900110628                   UNION +
089000110628
089100110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
089200110630                          count(*), sum( TASncl ) +
089300110628                     from TITAS10F +
089400110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
089500110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
089600110628                          %editc( D07dti : 'X' ) + ' and ' +
089700110628                          %editc( D07dtf : 'X' ) +
089800110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
089900110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
090000110628                  ' group by TASccm +
090100110628
090200110628                   UNION +
090300110628
090400110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
090500110630                          count(*), sum( TASncl ) +
090600110628                     from TITAS00F +
090700110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
090800110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
090900110628                          %editc( D07dti : 'X' ) + ' and ' +
091000110628                          %editc( D07dtf : 'X' ) +
091100110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
091200110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
091300110628                  ' group by TASccm +
091400110628
091500110628                      FOR FETCH ONLY';
091600110628
091700110628         // -?Dichiarazione cursore?
091800110628         exec sql  prepare S4  from :wSql_4;
091900110628         exec sql  declare C4  cursor for S4;
092000110628
092100110628         // -?Apertura del cursore?
092200110628         exec sql  open C4;
092300110628
092400110628         if  SQLcode < *zero;
092500110628           $EoF_4 = *on;
092600110628           exsr  sr_PrintErr;
092700110628         endif;
092800110628
092900110628       ENDSR;
093000110628
093100110628       //--------------------------------------------------------------
093200110628       //?Lettura cursore C4 (totale spedizioni nel periodo x cliente) ?
093300110628       //--------------------------------------------------------------
093400110628       BEGSR  sr_ReadCursor_4;
093500110628
093600110628         clear  wSQL_ds_TITAS;
093700110628
093800110628         // -?Lettura cursore?
093900110628         exec sql  fetch next  from C4  into :wSQL_ds_TITAS;
094000110628
094100110628         select;
094200110628           // -?Fine flusso?
094300110628           when  SQLcode = 100;
094400110628             $EoF_4 = *on;
094500110628           // -?Errore?
094600110628           when  SQLcode < *zero;
094700110628             $EoF_4 = *on;
094800110628             exsr  sr_PrintErr;
094900110628           // -?Elaborazione dati estratti?
095000110628           other;
095100110628             exsr  sr_Elab_4;
095200110628         endsl;
095300110628
095400110628       ENDSR;
095500110628
095600110628       //--------------------------------------------------------------
095700110628       //?Chiusura cursore C4 (totale spedizioni nel periodo x cliente)?
095800110628       //--------------------------------------------------------------
095900110628       BEGSR  sr_CloseCursor_4;
096000110628
096100110628         // -?Chiusura del cursore?
096200110628         exec sql  close C4;
096300110628
096400110628       ENDSR;
096500110628
096600110628       //--------------------------------------------------------------
096700110628       //?Elaborazione totale spedizioni nel periodo x CCM da TITAS33C ?
096800110628       //--------------------------------------------------------------
096900110628       BEGSR  sr_Elab_4;
097000110628
097100110628         // -?Conteggio numero spedizioni?
097200110628         WE21nts += wSQL_ds_TITAS.sqlTOTspe;
097300110628
097400110628         // -?Conteggio numero colli?
097500110628         WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
097600110628
097700110628         // -?Memorizzazione data ULTIMA spedizione?
097800110628         if  wSQL_ds_TITAS.sqlTASdts > WE21dus;
097900110628           WE21dus = wSQL_ds_TITAS.sqlTASdts;
098000110628         endif;
098100110628
098200110628       ENDSR;
098300110628
098400110628       //--------------------------------------------------------------
098500110628       //?Scrittura record in work-file WFES210F.                      ?
098600110628       //--------------------------------------------------------------
098700110628       BEGSR  sr_Wrt_WFES210;
098800110628
098900110628         // -?Dati nel trk del work-file WFES210F?
099000110628         // -?Parametri di lancio?
099100110628         WE21pdti  = D07dti;
099200110628         WE21pdtf  = D07dtf;
099300110628         WE21pcver = D07cver;
099400110628         WE21pcdsc = D07cdsc;
099500110628         // -?Distretto?
099600110628         WE21cdi = ORGfl3;
099700110628         WE21ddi = ds17.§17des;
099800110628         // -?Area?
099900110628         WE21car = ORGcar;
100000110628         WE21dar = ds05.§05des;
100100110628         // -?Cliente unificante?
100200110628         WE21ccu = ds3C.§3Ccks;
100300110628         WE21rsu = CNACOds_Uni.ACOrag;
100400110628         // -?Cliente mittente tab. "3C"?
100500110628         WE21ccm = %int( %triml( wSQL_ds_3C.sqlTBLkey ) );
100600140911         //WE21rsm = ds3C.§3Crag;
100700140911         WE21rsm = ACORag;
100800110628         WE21blo = ACOabl;
100900110628         // -?Supporto Cliente => BRT?
101000110628         WE21scb = wSupporto;
101100110628         if  wSupporto = c_SuppCB_ESWEB;
101200110628           // -?Dati dalla tab. "3EW"?
101300110628           WE21EWlnp = d3EW.§3EWlnp;
101400120321           WE21fls   = d3EW.§3EWfls;
101500110628           WE21nrs   = d3EW.§3EWnrs;
101600110628           WE21nsi   = d3EW.§3EWnsi;
101700110628           WE21nsf   = d3EW.§3EWnsf;
101800110628           WE21EWctm = d3EW.§3EWctm;
101900110628           WE21EWmad = d3EW.§3EWmad;
102000110628           WE21EWaew = d3EW.§3EWaew;
102100110628         else;
102200110628           // -?Dati dalla tab. "3C"?
102300120321           WE21fls   = ds3C.§3Cfls;
102400110628           WE21nrs   = ds3C.§3Cnrs;
102500110628           WE21nsi   = wSgnClI;
102600110628           WE21nsf   = d3CP.§3CPal;
102700110628         endif;
102800110630         // -?Totali (già calcolati in subr. "sr_Elab_4")?
102900110628         //WE21nts += wSQL_ds_TITAS.sqlTOTspe;
103000110628         //WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
103100110628         //WE21dus  = wSQL_ds_TITAS.sqlTASdts;
103200110706         // -?Dati EasySped (NON per "EasySped-Validate")?
103300110706         if  wSupporto <> c_SuppCB_ESVAL;
103400110706           WE21edat = d3CE.§3CEedat;
103500110706           WE21ever = d3CE.§3CEever;
103600110706           WE21cver = d3CE.§3CEcver;
103700110706           WE21cdsc = d3CE.§3CEcdsc;
103800110706           WE21typw = d3CE.§3CEtypweb;
103900110706           WE21cvermx = d3CE.§3CEcvermx;
104000110706           WE21cdscmx = d3CE.§3CEcdscmx;
104100110706         endif;
104200110630         // -?EasySpedWeb?
104300110706         select;
104400110706           when  wSupporto = c_SuppCB_ESWEB  or
104500110706                 wSupporto = c_SuppCB_ESVAL;
104600110706             WE21esweb = 'SI';
104700110706           when  WE21ever >= '4.0.3 ';
104800110706             WE21esweb = 'SI';
104900110706           other;
105000110706             WE21esweb = 'NO';
105100110706         endsl;
105200110630         // -?Cappario aggiornato?
105300110630         select;
105400110706           when  wSupporto = c_SuppCB_ESWEB;
105500110706             WE21cverag = 'SI';
105600110706           when  wSupporto = c_SuppCB_ESVAL;
105700110706             WE21cverag = 'NO';
105800110630           when  WE21cver   = *zero;
105900110630             WE21cverag = 'NO';
106000110630           when  WE21cver   = D07cver;
106100110630             WE21cverag = 'SI';
106200110630           when  WE21cverMx = D07cver;
106300110630             WE21cverag = 'SI';
106400120601           when  WE21cver   = (D07cver - 1)  and  WE21cdsc = D07cdsc;
106500120601             WE21cverag = 'SI';
106600110630           other;
106700110630             WE21cverag = 'NO';
106800110630         endsl;
106900140416         // -?Materiale in comodato?
107000140416         WE21stamco = $comodato  ;
107100141024         // -?Cod. e Nominativo commerciale
107200141027         WE21CMU = *zeros;
107300141027         WE21CMD = *blank;
107400141027         // se esiste un cod. unificante
107500141027         if WE21CCU <> *zeros;
107600141027           // reperisco il suo cod.agente
107700141024           clear  TIBS69ds;
107800141024           I69sif = knsif;
107900141024           I69kcc = DUTkci;
108000141024           I69kcp = WE21CCU;
108100141024           tibs69r( tibs69ds :
108200141024                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
108300141024           if  O69err = *on;
108400141024             clear  CNCLPds_Uni;
108500141024           else;
108600141024             CNCLPds_Uni = ds_CNCLP;
108700141024           endif;
108800141027           // reperisco il cod. commerciale unificante mediante l'agente del cliente
108900141027           k_CMMCOD = CLPAGE;
109000141027           chain  %kds(k01azcmm01)  AZCMM000;
109100141027           if  %found();
109200141027             WE21CMU = CMMUNI;
109300141027             k_CMMCOD = WE21CMU;
109400141027             chain  %kds(k01azcmm01)  AZCMM000;
109500141027             // reperisco la descrizione del cod. commerciale unificante
109600141027             if  %found();
109700141027               WE21CMD = CMMDES;
109800141027             endif;
109900141027           endif;
110000141024         EndIf;
110100110628
110200110628         //______________
110300110628         WRITE  WFES2100;
110400110628         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
110500110628
110600110628       ENDSR;
110700110628
110800110628       //--------------------------------------------------------------
110900110628       //?Reperimento stampanti in concordato da tab. "3R"             ?
111000110628       //--------------------------------------------------------------
111100140416       //BEGSR  sr_StampConc;
111200110628
111300140416       //  clear  ds3R;
111400110628
111500110628         // -?1° tentativo con il cliente in esame?
111600140416       //  clear  yy;
111700140416       //  yy = %lookup( %trimr( wSQL_ds_3C.sqlTBLkey ) : $Cli );
111800110628
111900110628         // -?2° tentativo con il padre del cliente in esame?
112000140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
112100140416       //    clear  yy;
112200140416       //    yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Cli );
112300140416       //  EndIf;
112400110628
112500110628         // -?3° tentativo con gli altri fratelli?
112600140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
112700140416       //    clear  yy;
112800140416       //    DoU  yy = *zero   or   $StpComodato(yy) <> *blank;
112900140416       //      yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Padre : yy + 1 );
113000140416       //    EndDo;
113100140416       //  EndIf;
113200110628
113300110628         // -?Imposta la stampante in ds3R (se reperita)?
113400140416       //  If  yy > *zero;
113500140416       //    ds3R.§3Rnot = $StpComodato(yy);
113600140416       //  EndIf;
113700140416       //--------------------------------------------------------------
113800140416       //?Verifica presenza comodati            ?
113900140416       //--------------------------------------------------------------
114000140416       BEGSR  sr_StampCom;
114100140415         clear $comodato;
114200140414         // -?Con il cliente in esame verifico legame "ST"
114300140414         //   Verifico se codice presente come padre
114400140414         clear tibs10ds;
114500140414         d10tle='ST';
114600140414         d10paf='F';
114700140415         d10cod=%dec(%trimr( wSQL_ds_3C.sqlTBLkey ):11:0);
114800140415         tibs10r(tibs10ds);
114900140415         if d10cop<=0;
115000140415         //   Se non è padre verifico se è figlio
115100140415            d10paf='P';
115200140415            tibs10r(tibs10ds);
115300140415            if d10cop<=0;
115400140415         //  Se non è nè padre nè figlio è cliente singolo
115500140415               d10cop=d10cod;
115600141016               skc(1)=%editc(d10cod:'X');
115700140415            endif;
115800140415         endif;
115900140416         // -?Verifico se il padre del legame ST è già stato analizzato per
116000140416         // -?i comodati, altrimenti lo analizzo e lo memorizzo in schiera
116100140416         // -?Schiera $padst (clienti padri legame ST)
116200140416         // -?Schiera $com   (indica se presente comodato S=Comodato)
116300140415         yy=%lookup(d10cop:$padst);
116400140415         if yy=0;
116500140415            yy=%lookup(0:$padst);
116600140415            $padst(yy)=d10cop      ;
116700140415         // Verifico se presente materiale in comodato nella famiglia e
116800140415         // memorizzo in schiera
116900140416            clear wcom;
117000140416            for i=1 to %elem(sKC);
117100140416               if skc(i)=*blanks or skc(i)=*zeros or wcom='S';
117200140415                  leave;
117300140415               endif;
117400140416               kcli=%dec(%subst(skc(I):5:7):7:0);
117500140415               exsr sr_comodato;
117600140416               if wcom = 'S';
117700140416                  $com(yy)='S';
117800140416               endif ;
117900140415            endfor;
118000140415         endif;
118100140415         if $com(yy)='S';
118200140415            $comodato='Materiale in comodato';
118300140415         endif;
118400110628       ENDSR;
118500110628
118600140416       //--------------------------------------------------------------
118700140416       //?Ricerca comodato per cliente                                 ?
118800140416       //--------------------------------------------------------------
118900140416       BEGSR  sr_Comodato;
119000140416       setll kcli uncmd01l;
119100140416       reade kcli uncmd01l;
119200140416       dow not %eof(uncmd01l) ;
119300140416          if cmdcfg<>'A' and cmdcfg<>'C';
119400140416             wcom='S';
119500140416             leave;
119600140416          endif;
119700140416          reade kcli uncmd01l;
119800140416       enddo;
119900140416       endsr;
120000110628       //--------------------------------------------------------------
120100110628       //?Apertura cursore C3 (creazione WFES220F con totali x area).  ?
120200110628       //--------------------------------------------------------------
120300110628       BEGSR  sr_OpenCursor_3;
120400110628
120500110628         // -?Dichiarazione cursore?
120600110628         exec sql  declare C3  cursor for
120700110628                   select *
120800110628                     from WFES210F
120900110628                    order by WE21car, WE21scb, WE21ccu, WE21ccm
121000110628                      for read only;
121100110628
121200110628         // -?Apertura cursore?
121300110628         exec sql  open C3;
121400110628
121500110628         clear  WFES2200;
121600110628
121700110628       ENDSR;
121800110628
121900110628       //--------------------------------------------------------------
122000110628       //?Lettura cursore C3 (creazione WFES220F con totali x area).
122100110628       //--------------------------------------------------------------
122200110628       BEGSR  sr_ReadCursor_3;
122300110628
122400110628         // -?Lettura cursore?
122500110628         exec sql  fetch  NEXT  from C3  into :WFES210ds;
122600110628
122700110628         select;
122800110628           when  SQLcode = 100;
122900110628             $EoF_3 = *on;
123000110628           when  SQLcode < *zero;
123100110628             $EoF_3 = *on;
123200110628             exsr  sr_PrintErr;
123300110628           other;
123400110628             exsr  sr_Elab_3;
123500110628         endsl;
123600110628
123700110628       ENDSR;
123800110628
123900110628       //--------------------------------------------------------------
124000110628       //?Creazione work-file WFES220F con totali per area.            ?
124100110628       //--------------------------------------------------------------
124200110628       BEGSR  sr_Elab_3;
124300110628
124400110628         // -?Rottura Area?
124500111122         IF  WFES210ds.WE21car <> WE22car;
124600110628
124700110628           // -?Scrittura record con i totali dell'area precedente?
124800110628           if  WE22car <> *zero;
124900111123             exsr  sr_Wrt_WFES220;
125000110628           endif;
125100110628
125200110628           // -?Impostazione dati nuova area?
125300110628           clear  WFES2200;
125400110628           WE22pdti  = WFES210ds.WE21pdti;
125500110628           WE22pdtf  = WFES210ds.WE21pdtf;
125600110628           WE22pcver = WFES210ds.WE21pcver;
125700110628           WE22pcdsc = WFES210ds.WE21pcdsc;
125800110628           WE22car   = WFES210ds.WE21car;
125900110628           WE22dar   = WFES210ds.WE21dar;
126000110628
126100110628         ENDIF;
126200110628
126300110628
126400110628         // -?Incremento totali dell'area in elaborazione?
126500111122         Select;
126600111122
126700111122           // -? EasySpeb ?
126800111123           When  WFES210ds.WE21scb = c_SuppCB_ESYSP;
126900111122             // -?Num. totale spedizioni nel periodo?
127000111122             WE22esnts  += WFES210ds.WE21nts;
127100111122             // -?Num. totale colli nel periodo?
127200111122             WE22esntc  += WFES210ds.WE21ntc;
127300111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
127400111122             if  WFES210ds.WE21stamco = *blank;
127500111122               WE22esntcn += WFES210ds.WE21ntc;
127600111122             endif;
127700111122             // -?Num. totale clienti padre?
127800111122             if  WFES210ds.WE21ccu <> SAVccu;
127900111122               SAVccu = WFES210ds.WE21ccu;
128000111122               WE22esnksu += 1;
128100111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
128200111122               exsr  sr_Elab_5;
128300111122               if  wNTS = *zero;
128400111122                 WE22esnuns += 1;
128500111122               endif;
128600111122             endif;
128700111122             // -?Num. totale clienti in tab. "3C"?
128800111122             WE22esncli += 1;
128900111123             //  ?(scartati quelli bloccati)?
129000111122             IF  WFES210ds.WE21blo = *blank;
129100111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
129200111122               If  WFES210ds.WE21nts > *zero;
129300111122                 WE22esncls += 1;
129400111122                 // ·?...ma SENZA cappario aggiornato?
129500111122                 if  WFES210ds.WE21cverag = 'NO';
129600111122                   WE22esncca += 1;
129700111122                 endif;
129800111122                 // ·?...ma SENZA EasySpedWeb?
129900111122                 if  WFES210ds.WE21esweb  = 'NO';
130000111122                   WE22esnclw += 1;
130100111122                 endif;
130200111122               EndIf;
130300111122             ENDIF;
130400111122
130500111122           // -? EasySpeb Validate ?
130600111123           When  WFES210ds.WE21scb = c_SuppCB_ESVAL;
130700111122             // -?Num. totale spedizioni nel periodo?
130800111122             WE22evnts  += WFES210ds.WE21nts;
130900111122             // -?Num. totale colli nel periodo?
131000111122             WE22evntc  += WFES210ds.WE21ntc;
131100111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
131200111122             if  WFES210ds.WE21stamco = *blank;
131300111122               WE22evntcn += WFES210ds.WE21ntc;
131400111122             endif;
131500111122             // -?Num. totale clienti padre?
131600111122             if  WFES210ds.WE21ccu <> SAVccu;
131700111122               SAVccu = WFES210ds.WE21ccu;
131800111122               WE22evnksu += 1;
131900111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
132000111122               exsr  sr_Elab_5;
132100111122               if  wNTS = *zero;
132200111122                 WE22evnuns += 1;
132300111122               endif;
132400111122             endif;
132500111122             // -?Num. totale clienti in tab. "3C"?
132600111122             WE22evncli += 1;
132700111123             //  ?(scartati quelli bloccati)?
132800111122             IF  WFES210ds.WE21blo = *blank;
132900111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
133000111122               If  WFES210ds.WE21nts > *zero;
133100111122                 WE22evncls += 1;
133200111122               EndIf;
133300111122             ENDIF;
133400111123
133500111123           // -? EasySpeb WEB ?
133600111123           When  WFES210ds.WE21scb = c_SuppCB_ESWEB;
133700111123             // -?Num. totale spedizioni nel periodo?
133800111123             WE22ewnts  += WFES210ds.WE21nts;
133900111123             // -?Num. totale colli nel periodo?
134000111123             WE22ewntc  += WFES210ds.WE21ntc;
134100111123             // -?Num. totale spedizioni e colli senza stampanti in comodato?
134200111123             if  WFES210ds.WE21stamco = *blank;
134300111123               WE22ewntcn += WFES210ds.WE21ntc;
134400111123             endif;
134500111123             // -?Num. totale clienti padre?
134600111123             if  WFES210ds.WE21ccu <> SAVccu;
134700111123               SAVccu = WFES210ds.WE21ccu;
134800111123               WE22ewnksu += 1;
134900111123               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
135000111123               exsr  sr_Elab_5;
135100111123               if  wNTS = *zero;
135200111123                 WE22ewnuns += 1;
135300111123               endif;
135400111123             endif;
135500111123             // -?Num. totale clienti in tab. "3C"?
135600111123             WE22ewncli += 1;
135700111123             //  ?(scartati quelli bloccati)?
135800111123             IF  WFES210ds.WE21blo = *blank;
135900111123               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
136000111123               If  WFES210ds.WE21nts > *zero;
136100111123                 WE22ewncls += 1;
136200111123               EndIf;
136300111123             ENDIF;
136400111122
136500111122         EndSl;
136600110628
136700110628       ENDSR;
136800110628
136900110628       //--------------------------------------------------------------
137000110630       //?Chiusura cursore C3 (creazione WFES220F con totali x area).
137100110628       //--------------------------------------------------------------
137200110628       BEGSR  sr_CloseCursor_3;
137300110628
137400110628         // -?Scrittura ultimo record?
137500110628         if  WE22car <> *zero;
137600111123           exsr  sr_Wrt_WFES220;
137700110628         endif;
137800110628
137900110628         // -?Chiusura cursore?
138000110628         exec sql  close C3;
138100110628
138200110628       ENDSR;
138300111123
138400111123       //--------------------------------------------------------------
138500111123       //?Scrittura record in work-file WFES220F.                      ?
138600111123       //--------------------------------------------------------------
138700111123       BEGSR  sr_Wrt_WFES220;
138800111123
138900111123         // -? Totali AREA ?
139000111123         // ·?Num. totale spedizioni nel periodo?
139100111123         WE22tants  = WE22esnts + WE22evnts + WE22ewnts;
139200111123         // ·?Num. totale colli nel periodo?
139300111123         WE22tantc  = WE22esntc + WE22evntc + WE22ewntc;
139400111123         // ·?Num. totale spedizioni e colli senza stampanti in comodato?
139500111123         WE22tantcn = WE22esntcn + WE22evntcn + WE22ewntcn;
139600111123         // ·?Num. totale clienti padre?
139700111123         WE22tanksu = WE22esnksu + WE22evnksu + WE22ewnksu;
139800111123         // ·?"Raggruppamento" clienti SENZA spedizioni nel periodo?
139900111123         WE22tanuns = WE22esnuns + WE22evnuns + WE22ewnuns;
140000111123         // ·?Num. totale clienti in tab. "3C"?
140100111123         WE22tancli = WE22esncli + WE22evncli + WE22ewncli;
140200111123         // ·?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
140300111123         //  ?(scartati quelli bloccati)?
140400111123         WE22tancls = WE22esncls + WE22evncls + WE22ewncls;
140500111123
140600111123         //______________
140700111123         WRITE  WFES2200;
140800111123         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
140900111123
141000111123       ENDSR;
141100110628
141200110628       //--------------------------------------------------------------
141300110628       //?Verifica se padre & figli senza spedizioni nel periodo.      ?
141400110628       //--------------------------------------------------------------
141500110628       BEGSR  sr_Elab_5;
141600110628
141700110628         // -?Dichiarazione cursore?
141800110628         exec sql  declare C5  cursor for
141900110628                    select sum(WE21nts)
142000110628                      from WFES210F
142100110628                     where WE21ccu = :WFES210ds.WE21ccu
142200110628                       for read only;
142300110628
142400110628         // -?Apertura cursore?
142500110628         exec sql  open C5;
142600110628
142700110628         // -?Lettura cursore?
142800110628         exec sql  fetch  NEXT  from C5  into :wNTS;
142900110628
143000110628         select;
143100110628           when  SQLcode = 100;
143200110628             $EoF_5 = *on;
143300110628           when  SQLcode < *zero;
143400110628             $EoF_5 = *on;
143500110628             exsr  sr_PrintErr;
143600110628           other;
143700111122             //if  wNTS = *zero;
143800111122             //  WE22nksuns += 1;
143900111122             //endif;
144000110628         endsl;
144100110628
144200110628         // -?Chiusura cursore?
144300110628         exec sql  close C5;
144400110628
144500110628       ENDSR;
144600110628
144700110628       //--------------------------------------------------------------
144800110628       //?Ultimo aggiornamento (dati di EasySped & relativo flag).     ?
144900110628       //--------------------------------------------------------------
145000110628       BEGSR  sr_Upd_EsySp;
145100110628
145200110628         exsr  sr_OpenCursor_6;
145300110628
145400110628         clear wEsySp_ds;
145500110628         dou  $Eof_6 = *on;
145600110628           exsr  sr_ReadCursor_6;
145700110628         enddo;
145800110628
145900110628         exsr  sr_CloseCursor_6;
146000110628
146100110628       ENDSR;
146200110628
146300110628       //--------------------------------------------------------------
146400110628       //?Apertura cursore C6.                                         ?
146500110628       //--------------------------------------------------------------
146600110628       BEGSR  sr_OpenCursor_6;
146700110628
146800110628         // -?Dichiarazione cursore?
146900151023         exec sql  declare C6 insensitive cursor for
147000110628
147100151023                    select WE21ccu, WE21ccm, WE21scb, WE21flgu,
147200110628                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
147300110628                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg,
147400110628                           rrn(WFES210F)
147500110628                      from WFES210F
147600110628                     order by WE21ccu, WE21cver desc
147700110628
147800110628                       for fetch only;
147900110628
148000110628         // -?Apertura cursore?
148100110628         exec sql  open C6;
148200110628
148300110628       ENDSR;
148400110628
148500110628       //--------------------------------------------------------------
148600110628       //?Chiusura cursore C6.                                         ?
148700110628       //--------------------------------------------------------------
148800110628       BEGSR  sr_CloseCursor_6;
148900110628
149000110628         // -?Chiusura cursore?
149100110628         exec sql  close C6;
149200110628
149300110628       ENDSR;
149400110628
149500110628       //--------------------------------------------------------------
149600110628       //?Lettura cursore C6 (rec. WFES210F senza info EasySped).      ?
149700110628       //--------------------------------------------------------------
149800110628       BEGSR  sr_ReadCursor_6;
149900110628
150000110628         // -?Lettura cursore?
150100110628         exec sql  fetch  NEXT  from C6  into :wSQL_ds_WFES21;
150200110628
150300110628         select;
150400110628           when  SQLcode = 100;
150500110628             $Eof_6 = *on;
150600110628           when  SQLcode < *zero;
150700110628             $Eof_6 = *on;
150800110628           other;
150900110628             exsr  sr_Elab_6;
151000110628         endsl;
151100110628
151200110628       ENDSR;
151300110628
151400110628       //--------------------------------------------------------------
151500110630       //?Aggiornamento rec. WFES210F per info EasySped.               ?
151600110628       //--------------------------------------------------------------
151700110628       BEGSR  sr_Elab_6;
151800110628
151900110628         select;
152000110628
152100110628           // -?Record con la versione: salvo cod. cliente + dati della?
152200110628           //  ?versione?
152300111117           //  ?NO: la versione (anche se meno recente) del 2° cliente?
152400111117           //      ?della famiglia si sovrapporrebbe a quella del 1°...?
152500111117           //when  wSQL_ds_WFES21.sqlWESever <> *blank;
152600111117
152700111117           // -?1° Record con la versione: salvo cod. cliente + dati?
152800111117           //  ?della versione?
152900111117           //  ?Meglio tenere comunque la versione più recente (quella?
153000111117           //  ?nel 1° record letto via vedi SQL)!?
153100151023           //when  wSQL_ds_WFES21.sqlWESccu <> ESYWESccu;
153200151023           //  wEsySp_ds = wSQL_ds_WFES21;
153300110628
153400111117           // -?Record senza la versione: se unificante = al precedente?
153500110628           //  ?aggiorno il record con la versione salvata?
153600151023           //when  wSQL_ds_WFES21.sqlWESever = *blank  and
153700151023           //      wSQL_ds_WFES21.sqlWESccu  = ESYWESccu;
153800151023           //  chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
153900151023           //  if  %found(WFES210F);
154000151023           //    WE21flgu = 'U';
154100151023           //    WE21edat = ESYWESedat;
154200151023           //    WE21ever = ESYWESever;
154300151023           //    WE21cver = ESYWEScver;
154400151023           //    WE21typw = ESYWEStypw;
154500151023           //    WE21cdsc = ESYWEScdsc;
154600151023           //    WE21cvermx = ESYWEScvermax;
154700151023           //    WE21cdscmx = ESYWEScdscmax;
154800151023           //    WE21esweb  = ESYWESesweb;
154900151023           //    WE21cverag = ESYWEScveragg;
155000151023
155100151023           // Record di EasySped (ESYSP) senza la versione:
155200151023           // reperisco da WFES210F il rcd dell'unificante (se il cliente non è esso stesso
155300151023           // l'unificante) e aggiorno il rcd in linea con i dati dell'unificante.
155400151023           // Qualora anche la versione dell'unificante fosse vuota, non faccio nulla.
155500151023           // E' dato per scontato che il cliente unificante abbia in tab. 3C se stesso come
155600151023           // unificante.
155700151023           when  wSQL_ds_WFES21.sqlWESscb  = 'ESYSP' and
155800151023                 wSQL_ds_WFES21.sqlWEScver = 0 and
155900151023                 wSQL_ds_WFES21.sqlWESccu <> wSQL_ds_WFES21.sqlWESccm;
156000151023             // leggo via SQL
156100151023             wCCM = wSQL_ds_WFES21.sqlWESccu;
156200151023             exsr sr_ReadCursor_7;
156300151109             if  $EoF_7 = *off
156400151109             and ESYWEScver<> 0;
156500151109               chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
156600151023               WE21flgu = 'U';
156700151023               WE21edat = ESYWESedat;
156800151023               WE21ever = ESYWESever;
156900151023               WE21cver = ESYWEScver;
157000151023               WE21typw = ESYWEStypw;
157100151023               WE21cdsc = ESYWEScdsc;
157200151023               WE21cvermx = ESYWEScvermax;
157300151023               WE21cdscmx = ESYWEScdscmax;
157400151023               WE21esweb  = ESYWESesweb;
157500151023               WE21cverag = ESYWEScveragg;
157600110628               //______________
157700110628               update WFES2100;
157800110628               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
157900110628             endif;
158000110628
158100110628         endsl;
158200151023
158300110628       ENDSR;
158400151023
158500151023       //--------------------------------------------------------------
158600151023       //?Lettura cursore C7 (1 solo rec. WFES210F avente CCM e CCU quelli passati).      ?
158700151023       //--------------------------------------------------------------
158800151023       BEGSR  sr_ReadCursor_7;
158900151023
159000151023         // -?Dichiarazione cursore?
159100151023         exec sql  declare C7 cursor for
159200151023
159300151023                    select WE21ccu, WE21ccm, WE21scb, WE21flgu,
159400151023                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
159500151023                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg
159600151023                      from WFES210F
159700151023                     where WE21ccm=:wCCM and WE21ccu=WE21ccm
159800151023                       for fetch only;
159900151023
160000151023         // -?Apertura cursore?
160100151023         exec sql  open C7;
160200151023
160300151023         // -?Lettura cursore?
160400151023         exec sql  fetch  next  from C7  into :wEsySp_ds;
160500151023
160600151023         select;
160700151023           when  SQLcode = 100;
160800151023             $Eof_7 = *on;
160900151023           when  SQLcode < *zero;
161000151023             $Eof_7 = *on;
161100151023           when  SQLcode >= *zero and ESYwescver > 0;
161200151023             $Eof_7 = *off;
161300151023         endsl;
161400151023
161500151023         // -?Chiusura cursore?
161600151023         exec sql  close C7;
161700151023
161800151023       ENDSR;
161900110628
162000110628       //--------------------------------------------------------------
162100110628       //?Stampa segnalazione dell'errore rilevato.                    ?
162200110628       //--------------------------------------------------------------
162300110628       BEGSR  sr_PrintErr;
162400110628
162500110628         // -?Stampa del Dump?
162600110628         Dump(A);
162700110628
162800110628         // -?Stampa del Job-Log?
162900110628         Qcmd = 'DSPJOBLOG job(*) output(*print)';
163000110628         exsr  sr_ExecCmd;
163100110628
163200110628         // -?Chiusura del programma?
163300110628         exsr  sr_RoutEnd;
163400110628
163500110628       ENDSR;
163600110628
163700110628       //--------------------------------------------------------------
163800110628       //?Esecuzione del comando (già impostato).                      ?
163900110628       //--------------------------------------------------------------
164000110628       BEGSR  sr_ExecCmd;
164100110628
164200110628         clear Qcap0100;
164300110628         Qcabcsdh = *off;
164400110628         Qcapa    = *off;
164500110628         Qcacmdss = *off;
164600110628         Qcaerved = *allX'00';
164700110628
164800110628         clear Qusec;
164900110628         Qusbprv  = %size(Qusec);
165000110628
165100110628         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
165200110628                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
165300110628                           0 : 0 : Qusec);
165400110628
165500110628         //if  Qusei <> *blank;
165600110628         //  ...
165700110628         //endif;
165800110628
165900110628       ENDSR;
166000110628
166100110628       //--------------------------------------------------------------
166200110628       //?Operazioni finali.                                           ?
166300110628       //--------------------------------------------------------------
166400110628       BEGSR  sr_RoutEnd;
166500110628
166600110628         // -?Chiusura work-file?
166700110628         close(e)  WFES220F;
166800110628         close(e)  WFES210F;
166900110628
167000110628         // -?Chiusura applicazioni?
167100110628         reset  TIBS69ds;
167200110628         tibs69r( tibs69ds :
167300110628                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
167400110628
167500110628         // -?Uscita del *pgm?
167600110628         return;
167700110628
167800110628       ENDSR;
167900110628
168000110628      /end-free
