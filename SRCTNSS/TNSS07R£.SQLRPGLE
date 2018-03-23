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
017400110628     d  sqlWESflgu                         inz    like(WE21flgu)
017500110628     d  sqlWESedat                         inz    like(WE21edat)
017600110628     d  sqlWESever                         inz    like(WE21ever)
017700110628     d  sqlWEScver                         inz    like(WE21cver)
017800110628     d  sqlWEScdsc                         inz    like(WE21cdsc)
017900110628     d  sqlWEStypw                         inz    like(WE21typw)
018000110628     d  sqlWEScverMax                      inz    like(WE21cverMx)
018100110628     d  sqlWEScdscMax                      inz    like(WE21cdscMx)
018200110628     d  sqlWESesweb                        inz    like(WE21esweb)
018300110628     d  sqlWEScverAgg                      inz    like(WE21cverag)
018400110628     d  SQLrrn                        8  0 inz
018500110628
018600110628       // -?Salvataggio dati (del padre) estratti in wSQL_WFES21?
018700110628     d wEsySp_ds       ds                  inz
018800110628     d  ESYwesccu                          like(wSQL_ds_WFES21.sqlWESccu)
018900110628     d                                     inz
019000110628     d  ESYwesflgu                         like(wSQL_ds_WFES21.sqlWESflgu)
019100110628     d                                     inz
019200110628     d  ESYwesedat                         like(wSQL_ds_WFES21.sqlWESedat)
019300110628     d                                     inz
019400110628     d  ESYwesever                         like(wSQL_ds_WFES21.sqlWESever)
019500110628     d                                     inz
019600110628     d  ESYwescver                         like(wSQL_ds_WFES21.sqlWEScver)
019700110628     d                                     inz
019800110628     d  ESYwescdsc                         like(wSQL_ds_WFES21.sqlWEScdsc)
019900110628     d                                     inz
020000110628     d  ESYwestypw                         like(wSQL_ds_WFES21.sqlWEStypw)
020100110628     d                                     inz
020200110628     d  ESYwescverMax                      like(wSQL_ds_WFES21.sqlWEScverMax)
020300110628     d                                     inz
020400110628     d  ESYwescdscMax                      like(wSQL_ds_WFES21.sqlWEScdscMax)
020500110628     d                                     inz
020600110628     d  ESYwesesweb                        like(wSQL_ds_WFES21.sqlWESesweb)
020700110628     d                                     inz
020800110628     d  ESYwescverAgg                      like(wSQL_ds_WFES21.sqlWEScverAgg)
020900110628     d                                     inz
021000110628
021100110628       // -?Dati estratti via SQL:?
021200110628       // - -?dal file WFES210F x totalizzazione in WFES220F?
021300110628     d WFES210ds     e ds                  extname(WFES210F)
021400110628     d                                     inz         qualified
021500110628
021600140416     d tibs10ds      e ds
021700140416     d skc                           11    DIM(500) overlay(d10skc)
021800110628       //--------------------------------------------------------------
021900110628       //?Definizione variabili globali.                               ?
022000110628       //--------------------------------------------------------------
022100110628
022200110628       // -?Flags booleani?
022300110628     d $EoF_1          s               n   inz
022400110628     d $EoF_2          s               n   inz
022500110628     d $EoF_3          s               n   inz
022600110628     d $EoF_4          s               n   inz
022700110628     d $EoF_5          s               n   inz
022800110628     d $EoF_6          s               n   inz
022900110628
023000110628       // -?Stringhe SQL da eseguire?
023100110628     d wSql            s           5000    inz  varying
023200110628     d wSql_4          s           5000    inz  varying
023300110628
023400110628       // -?Indici di schiera?
023500140416     d*xx              s              5  0 inz
023600110628     d yy              s              5  0 inz
023700140416     d i               s              5  0 inz
023800110628
023900110628       // -?Campi di comodo?
024000110628     d wDate           s              8  0 inz
024100110628     d wNTS            s              9  0 inz
024200110630     d SAVcdi          s                   inz(*loval) like(ORGfl3)
024300110628     d SAVcar          s                   inz(*loval) like(WE21car)
024400110628     d SAVccu          s                   inz(*loval) like(WE21ccu)
024500110628     d wSgnClI         s                   inz(1)      like(WE21nsi)
024600110630     d wSupporto       s                   inz         like(WE21scb)
024700140416     d wcom            s              1    inz
024800140416     d $COMODATO       s             21    inz
024900140416     d kcli            s                   like(cmdksc)
025000110628
025100110628       //--------------------------------------------------------------
025200110628       //?Definizione prototipi procedure.                             ?
025300110628       //--------------------------------------------------------------
025400110628
025500110628       // -?Reperimento dati utente?
025600110628     d TIBS34ds      e ds                  inz
025700110628      /copy gaitrasrc/srcProtoPr,TIBS34R
025800110628
025900110628       // -?Controllo/Decodifica cliente?
026000110628      /copy gaitrasrc/srcProtoPI,TIBS69R
026100110628      /copy gaitrasrc/srcProtoPR,TIBS69R
026200140416
026300140416       // -?ricerca cliente unificante
026400140416      /copy gaitrasrc/srcProtoPR,TIBS10R
026500110628
026600110628       // -?Parametri API QCAPCMD (Process Commands)?
026700110628     d Qcmd            s           2048    inz  varying
026800110628      /copy qSysInc/qRpgleSrc,QCAPCMD
026900110628       // -?API QCAPCMD (Process Commands)?
027000110628      /copy gaitrasrc/srcProtoPR,QCAPCMD
027100110628
027200110628       // -?Parametri gestione errori API.?
027300110628      /copy qsysinc/qrpglesrc,QUSEC
027400110628
027500110628       //--------------------------------------------------------------
027600110628       //?Definizione key-list.                                        ?
027700110628       //--------------------------------------------------------------
027800110628
027900110628       // -?File TABEL00F?
028000110628     d k03tabel00    e ds                  extname(TABEL00F : *key)
028100110628     d                                     prefix(k_)   inz
028200110628       // -?File AZORG01L?
028300110628     d k01azorg01    e ds                  extname(AZORG01L : *key)
028400110628     d                                     prefix(k_)   inz
028500141027       // -?File AZCMM01L?
028600141027     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
028700141027     d                                     prefix(k_)   inz
028800110628
028900110628       //--------------------------------------------------------------
029000110628       //?M A I N - L I N E                                            ?
029100110628       //--------------------------------------------------------------
029200110628
029300110628     c     *Entry        plist
029400110628     c                   parm                    KPJBA
029500110628
029600110628      /free
029700110628
029800110628       // -?Operazioni iniziali?
029900110628       exsr  sr_RoutInz;
030000110628
030100110628       // -?1° ciclo di elaborazione: creazione WFES210F (dettaglio)?
030200110628       //                            ?per clienti EasySped?
030300110628       exsr  sr_OpenCursor_1;
030400110628
030500110628       DoU  $Eof_1 = *on;
030600110628         exsr  sr_ReadCursor_1;
030700110628       EndDo;
030800110628
030900110628       exsr  sr_CloseCursor_1;
031000110628
031100110628       // -?2° ciclo di elaborazione: creazione WFES210F (dettaglio)?
031200110628       //                            ?per clienti EasySpedWeb?
031300110628       exsr  sr_OpenCursor_2;
031400110628
031500110628       DoU  $Eof_2 = *on;
031600110628         exsr  sr_ReadCursor_2;
031700110628       EndDo;
031800110628
031900110628       exsr  sr_CloseCursor_2;
032000110628
032100110628       // -?Aggiornamento dati EasySped/EasySpedWeb mancanti...?
032200110628       exsr  sr_Upd_EsySp;
032300110628
032400110628       // -?3° ciclo di elaborazione: creazione WFES220F (totale area)?
032500110628       exsr  sr_OpenCursor_3;
032600110628
032700110628       dou  $EoF_3 = *on;
032800110628         exsr  sr_ReadCursor_3;
032900110628       enddo;
033000110628
033100110628       exsr  sr_CloseCursor_3;
033200110628
033300110628       // -?Fine programma?
033400110628       exsr  sr_RoutEnd;
033500110628
033600110628       //--------------------------------------------------------------
033700110628       //?Operazioni iniziali.                                         ?
033800110628       //--------------------------------------------------------------
033900110628       BEGSR  sr_RoutInz;
034000110628
034100110628         *inLR = *on;
034200110628
034300110628         // -?Impostazione opzioni per SQL?
034400110628         exec sql  set  option  DynUsrPrf = *Owner,
034500110628                                CloSqlCsr = *EndMod;
034600110628
034700110628         // -?Reperimento dati job?
034800110628         exsr  sr_DatiJob;
034900110628
035000110628         // -?Impostazione campi "fissi"?
035100110628         k_TBLkut = 1;
035200110628
035300110628         // -?Reperimento data odierna in formato aaaammgg?
035400110628         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
035500110628         //wData_Iso = %date(wDate : *iso);
035600110628
035700110628         // -?Impostazione eventuali parametri ricevuti?
035800110628         if  SDSprm > *zero  and
035900110628             KPJBU <> *blank;
036000110628           TNSS07ds = KPJBU;
036100110628         else;
036200110628           reset  TNSS07ds;
036300110628         endif;
036400110628
036500110628         // -?Pulizia work-files del lavoro sottomesso e loro apertura?
036600110628         Qcmd = c_Cmd_ClrWF1;
036700110628         exsr  sr_ExecCmd;
036800110628         if  Qusei <> *blank;
036900110628           exsr  sr_PrintErr;
037000110628         endif;
037100110628         open  WFES210F;
037200110628         Qcmd = c_Cmd_ClrWF2;
037300110628         exsr  sr_ExecCmd;
037400110628         if  Qusei <> *blank;
037500110628           exsr  sr_PrintErr;
037600110628         endif;
037700110628         open  WFES220F;
037800110628
037900140416         //clear  xx;
038000140416         //clear  $Cli;
038100140416         //clear  $Padre;
038200140416         //clear  $StpComodato;
038300110628
038400110628         // -?Intabellamento clienti e relativa famiglia da "3C"?
038500140416         //k_TBLcod = '3C';
038600140416         //setll  %kds(k03tabel00 : 2)  TABEL;
038700140416         //reade  %kds(k03tabel00 : 2)  TABEL;
038800140416         //DoW  Not  %eof(TABEL00F);
038900140416         //  if  TBLflg = *blank;
039000140416         //    xx += 1;
039100140416         //    ds3C       = TBLuni;
039200140416         //    $Cli(xx)   = %trimr( TBLkey );
039300140416         //    $Padre(xx) = %editc( ds3C.§3Ccks : 'X' );
039400140416         //  endif;
039500140416         //reade  %kds(k03tabel00 : 2)  TABEL;
039600140416         //EndDo;
039700110628
039800110628         // -?Intabellamento stampanti in comodato per cliente da "3R"?
039900140416         //k_TBLcod = '3R';
040000140416         //For  yy = 1  To  xx;
040100140416         //  k_TBLkey = $Cli(yy);
040200140416         //  chain  %kds(k03tabel00)  TABEL;
040300140416         //  if  %found(TABEL00F)   and   TBLflg = *blank;
040400140416         //    ds3R = TBLuni;
040500140416         //    $StpComodato(yy) = ds3R.§3Rnot;
040600140416         //  endif;
040700140416         //EndFor;
040800110628
040900110628       ENDSR;
041000110628
041100110628       //--------------------------------------------------------------
041200110628       //?Reperimento Dati del job (Utente/Operativi).                 ?
041300110628       //--------------------------------------------------------------
041400110628       BEGSR  sr_DatiJob;
041500110628
041600110628         in(E) §AzUte;
041700110628         if NOT %error;
041800110628           in(E) §DatiUte;
041900110628         endif;
042000110628         if  %error  or  RSut = *blank;
042100110628           clear TIBS34ds;
042200110628           tibs34r ( tibs34ds );
042300110628           in §AzUte;
042400110628           in §DatiUte;
042500110628         endif;
042600110628
042700110628       ENDSR;
042800110628
042900110628       //--------------------------------------------------------------
043000110628       //?Apertura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
043100110628       //?  estrazione clienti EasySped.                               ?
043200110628       //--------------------------------------------------------------
043300110628       BEGSR  sr_OpenCursor_1;
043400110628
043500110628         // -?Preparazione stringa SQL?
043600110628         wSql = 'with +
043700110628
043800110628                 Tab_3C as +
043900110628                     ( select TBLkey, TBLuni +
044000110628                         from TABEL00F +
044100110628                        where TBLflg = '' '' +
044200110628                          and TBLcod = ''3C'' +
044300110628                          and substr(TBLuni,  1, 2) > ''00'' +
044400110628                          and substr(TBLuni, 44, 5) in +
044500110628                              (''' + c_SuppCB_ESYSP + ''', +
044600110628                               ''' + c_SuppCB_ESVAL + ''') ), +
044700110628
044800110628                 Tab_3CP as +
044900110628                     ( select TBEke1, TBEke2, TBEuni +
045000110628                         from TNTBE00F +
045100110628                        where TBEatb = '' '' +
045200110628                          and TBEcod = ''3CP'' +
045300110628                          and TBEsif = '' '' +
045400110629                          and TBElin = '' '' ), +
045500110628
045600110628                 Tab_3CE as +
045700110628                     ( select TBEke1, TBEuni +
045800110628                         from TNTBE00F +
045900110628                        where TBEatb = '' '' +
046000110628                          and TBEcod = ''3CE'' +
046100110628                          and TBEke2 = '' '' +
046200110628                          and TBEsif = '' '' +
046300110628                          and TBElin = '' '' ) +
046400110628
046500110630                 select Tab_3C.*, +
046600110630                        case when Tab_3CP.TBEke1 > '' '' +
046700110630                             then Tab_3CP.TBEke1 else '' '' end, +
046800110630                        case when Tab_3CP.TBEke2 > '' '' +
046900110630                             then Tab_3CP.TBEke2 else '' '' end, +
047000110630                        case when Tab_3CP.TBEuni > '' '' +
047100110630                             then Tab_3CP.TBEuni else '' '' end, +
047200110630                        case when Tab_3CE.TBEke1 > '' '' +
047300110630                             then Tab_3CE.TBEke1 else '' '' end, +
047400110630                        case when Tab_3CE.TBEuni > '' '' +
047500110630                             then Tab_3CE.TBEuni else '' '' end  +
047600110628
047700110628                   from Tab_3C left outer join Tab_3CP +
047800110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
047900110628                        substr(Tab_3CP.TBEke1, 1, 7) and +
048000110628                        substr(Tab_3C.TBLuni, 1, 2) = +
048100110628                        substr(Tab_3CP.TBEke2, 1, 2) +
048200110628
048300110628                               left outer join Tab_3CE +
048400110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
048500110630                        substr(Tab_3CE.TBEke1, 1, 7)';
048600110628
048700110628         // -?Dichiarazione cursore?
048800110628         exec sql  prepare S1  from :wSql;
048900110628         exec sql  declare C1  cursor for S1;
049000110628
049100110628         // -?Apertura del cursore?
049200110628         exec sql  open C1;
049300110628
049400110628         if  SQLcode < *zero;
049500110628           $EoF_1 = *on;
049600110628           exsr  sr_PrintErr;
049700110628         endif;
049800110628
049900110628       ENDSR;
050000110628
050100110628       //--------------------------------------------------------------
050200110628       //?Lettura cursore C1 (tab. "3C", "3CP" e "3CE") per            ?
050300110628       //?  estrazione clienti EasySped.                               ?
050400110628       //--------------------------------------------------------------
050500110628       BEGSR  sr_ReadCursor_1;
050600110628
050700110628         clear  ds3C;
050800110630         reset  d3CP;
050900110628         clear  d3CE;
051000110630         reset  wSgnClI;
051100110628         clear  wSQL_ds_3C;
051200110628         clear  wSQL_ds_3CP;
051300110628         clear  wSQL_ds_3CE;
051400110628
051500110628         // -?Lettura cursore?
051600110628         exec sql  fetch next  from C1  into :wSQL_ds_3C,
051700110628                                             :wSQL_ds_3CP,
051800110628                                             :wSQL_ds_3CE;
051900110628
052000110628         select;
052100110628           // -?Fine flusso?
052200110628           when  SQLcode = 100;
052300110628             $EoF_1 = *on;
052400110628           // -?Errore?
052500110628           when  SQLcode < *zero;
052600110628             exsr  sr_PrintErr;
052700110628             $EoF_1 = *on;
052800110628           // -?Elaborazione dati estratti?
052900110628           other;
053000110628             exsr  sr_Elab_1;
053100110628         endsl;
053200110628
053300110628       ENDSR;
053400110628
053500110628       //--------------------------------------------------------------
053600110628       //?Chiusura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
053700110628       //?  estrazione clienti EasySped.                               ?
053800110628       //--------------------------------------------------------------
053900110628       BEGSR  sr_CloseCursor_1;
054000110628
054100110628         // -?Chiusura del cursore?
054200110628         exec sql  close C1;
054300110628
054400110628       ENDSR;
054500110628
054600110628       //--------------------------------------------------------------
054700110628       //?Elaborazione singolo cliente estratto (EasySped).            ?
054800110628       //--------------------------------------------------------------
054900110628       BEGSR  sr_Elab_1;
055000110628
055100110628         // -?Valorizzazione tab. "3C"?
055200110628         ds3C = wSQL_ds_3C.sqlTBLuni;
055300110628
055400110628         // -?Valorizzazione tab. "3CP"?
055500110628         if  wSQL_ds_3CP.sqlTBEke1 > *zero  and
055600110628             wSQL_ds_3CP.sqlTBEke2 > *zero;
055700110628           Monitor;
055800110628             wSgnClI = %dec( %subst( wSQL_ds_3CP.sqlTBEke2 : 3 : 7 )
055900110628                             : 7 : 0 );
056000110628             On-Error  105;
056100110628               reset  wSgnClI;
056200110628               reset  d3CP;
056300110628           EndMon;
056400110628           if  wSQL_ds_3CP.sqlTBEuni > *zero;
056500110628             d3CP = wSQL_ds_3CP.sqlTBEuni;
056600110628           endif;
056700110628         endif;
056800110628
056900110628         // -?Valorizzazione tab. "3CE"?
057000110628         if  wSQL_ds_3CE.sqlTBEke1 > *blank;
057100110628           d3CE = wSQL_ds_3CE.sqlTBEuni;
057200110628         endif;
057300110628
057400110628         // -?Pulizia iniziale del record da registrare?
057500110628         clear  WFES2100;
057600110628
057700110628
057800110628         // -?Reperimento Distretto ed Area?
057900110628         //  ?(a "rottura" di filiale del cliente unificante)?
058000110628         k_ORGfil = %int( %subst( %editc( ds3C.§3Ccks : 'X' ) : 1 : 3 ) );
058100110628         if  k_ORGfil <> ORGfil;
058200110628           chain  %kds(k01azorg01)  AZORG;
058300110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
058400110628             clear  ORGfl3;
058500110628             clear  ORGcar;
058600110628             clear  ds17;
058700110628             clear  ds05;
058800110628           endif;
058900110628         endif;
059000110628
059100110628         // -?Decodifica distretto?
059200110628         if  ORGfl3 <> SAVcdi;
059300110628           SAVcdi = ORGfl3;
059400110628           clear  ds17;
059500110628           k_TBLkut = 1;
059600110628           k_TBLcod = '17';
059700110628           k_TBLkey = ORGfl3;
059800110628           chain  %kds(k03tabel00)  TABEL;
059900110628           if  %found(TABEL00F)  and  TBLflg = *blank;
060000110628             ds17 = TBLuni;
060100110628           endif;
060200110628         endif;
060300110628
060400110628         // -?Decodifica area?
060500110628         if  ORGcar <> SAVcar;
060600110628           SAVcar = ORGcar;
060700110628           clear  ds05;
060800110628           k_TBLkut = 1;
060900110628           k_TBLcod = '05';
061000110628           k_TBLkey = %editc( ORGcar : 'X' );
061100110628           chain  %kds(k03tabel00)  TABEL;
061200110628           if  %found(TABEL00F)  and  TBLflg = *blank;
061300110628             ds05 = TBLuni;
061400110628           endif;
061500110628         endif;
061600110628
061700110628         // -?Decodifica unificante?
061800110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
061900110628           clear  TIBS69ds;
062000110628           I69sif = knsif;
062100110628           I69kcc = DUTkci;
062200110628           I69kac = ds3C.§3Ccks;
062300110628           tibs69r( tibs69ds :
062400110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062500110628           if  O69err = *on;
062600110628             clear  CNACOds_Uni;
062700110628           else;
062800110628             CNACOds_Uni = ds_CNACO;
062900141027           endif;
063000110628         EndIf;
063100110628
063200110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
063300110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
063400110628           clear  TIBS69ds;
063500110628           I69sif = knsif;
063600110628           I69kcc = DUTkci;
063700110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
063800110628           tibs69r( tibs69ds :
063900110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064000110628           if  O69err = *on;
064100110628             clear  ds_CNACO;
064200110628           endif;
064300110628         EndIf;
064400110628
064500140416         // -?Reperimento materiale in comodato file UNCMD00F ?
064600140416         exsr  sr_StampCom;
064700110628
064800110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
064900110628         // -?(reperimento dati da spedizioni associate)?
065000110628         $Eof_4 = *off;
065100110628         exsr  sr_OpenCursor_4;
065200110628
065300110628         DoU  $Eof_4;
065400110628           exsr  sr_ReadCursor_4;
065500110628         EndDo;
065600110628
065700110628         exsr  sr_CloseCursor_4;
065800110628
065900110628         // -?Scrittura del record a totale per cliente?
066000110629         wSupporto = ds3C.§3Ccba;
066100110628         exsr  sr_Wrt_WFES210;
066200110628
066300110628       ENDSR;
066400110628
066500110628       //--------------------------------------------------------------
066600110628       //?Apertura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
066700110628       //?  estrazione clienti EasySpedWeb.                            ?
066800110628       //--------------------------------------------------------------
066900110628       BEGSR  sr_OpenCursor_2;
067000110628
067100110628         // -?Preparazione stringa SQL?
067200110628         wSql = 'with +
067300110628
067400110628                 TIVSS as +
067500110628                     ( select VSSksu, VSSsun, VSSisv +
067600110628                         from TIVSS00F +
067700110628                        where VSSisv = ''EW'' and +
067800110628                             (VSSdsc = 0 or VSSdsc > ' +
067900110628                              %char(wDate) + ') ), +
068000110628
068100110628                 Tab_3EW as +
068200110628                     ( select TBEke1, TBEke2, TBEuni +
068300110628                         from TNTBE00F +
068400110628                        where TBEatb = '' '' and +
068500120814                              TBEcod = ''3EW'' and +
068600120814                              substr(TBEuni, 33, 1) = '' '' ), +
068700110628
068800110628                 Tab_3C as +
068900110628                     ( select TBLkey, TBLuni +
069000110628                         from TABEL00F +
069100110628                        where TBLflg = '' '' and +
069200110628                              TBLcod = ''3C'' and +
069300110628                              substr(TBLuni, 44, 5) = +
069400110630                              ''' + c_SuppCB_ESWEB + ''') +
069500110628
069600110628                 select * +
069700110628
069800110628                   from TIVSS inner join Tab_3EW +
069900110628                     on TIVSS.VSSksu = Tab_3EW.TBEke1  and +
070000110628                        TIVSS.VSSsun = Tab_3EW.TBEke2 +
070100110628
070200110628                              inner join Tab_3C +
070300110628                     on substr(TIVSS.VSSksu, 2, 7) = +
070400110628                        substr(Tab_3C.TBLuni, 79, 7)';
070500110628
070600110628         // -?Dichiarazione cursore?
070700110628         exec sql  prepare S2  from :wSql;
070800110628         exec sql  declare C2  cursor for S2;
070900110628
071000110628         // -?Apertura del cursore?
071100110628         exec sql  open C2;
071200110628
071300110628         if  SQLcode < *zero;
071400110628           $EoF_2 = *on;
071500110628           exsr  sr_PrintErr;
071600110628         endif;
071700110628
071800110628       ENDSR;
071900110628
072000110628       //--------------------------------------------------------------
072100110628       //?Lettura cursore C2 (file TIVSS, tab. "3EW" e tab. "3C") per  ?
072200110628       //?  estrazione clienti EasySpedWeb.                            ?
072300110628       //--------------------------------------------------------------
072400110628       BEGSR  sr_ReadCursor_2;
072500110628
072600110628         clear  d3EW;
072700110628         clear  ds3C;
072800110628         clear  wSQL_ds_TIVSS;
072900110628         clear  wSQL_ds_3EW;
073000110628         clear  wSQL_ds_3C;
073100110628
073200110628         // -?Lettura cursore?
073300110628         exec sql  fetch next  from C2  into :wSQL_ds_TIVSS,
073400110628                                             :wSQL_ds_3EW,
073500110628                                             :wSQL_ds_3C;
073600110628
073700110628         select;
073800110628           // -?Fine flusso?
073900110628           when  SQLcode = 100;
074000110628             $EoF_2 = *on;
074100110628           // -?Errore?
074200110628           when  SQLcode < *zero;
074300110628             exsr  sr_PrintErr;
074400110628             $EoF_2 = *on;
074500110628           // -?Elaborazione dati estratti?
074600110628           other;
074700110628             exsr  sr_Elab_2;
074800110628         endsl;
074900110628
075000110628       ENDSR;
075100110628
075200110628       //--------------------------------------------------------------
075300110628       //?Chiusura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
075400110628       //?  estrazione clienti EasySpedWeb.                            ?
075500110628       //--------------------------------------------------------------
075600110628       BEGSR  sr_CloseCursor_2;
075700110628
075800110628         // -?Chiusura del cursore?
075900110628         exec sql  close C2;
076000110628
076100110628       ENDSR;
076200110628
076300110628       //--------------------------------------------------------------
076400110629       //?Elaborazione singolo cliente estratto (EasySpedWeb).         ?
076500110628       //--------------------------------------------------------------
076600110628       BEGSR  sr_Elab_2;
076700110628
076800110628         d3EW = wSQL_ds_3EW.sqlTBEuni;
076900110628         ds3C = wSQL_ds_3C.sqlTBLuni;
077000110628
077100110630         // -?Pulizia iniziale del record da registrare?
077200110628         clear  WFES2100;
077300110628
077400110628         // -?Reperimento Area?
077500110628         //  ?(a "rottura" di filiale del cliente unificante)?
077600110628         k_ORGfil = %int( %subst( wSQL_ds_TIVSS.sqlVSSksu : 2 : 3 ) );
077700110628         if  k_ORGfil <> ORGfil;
077800110628           chain  %kds(k01azorg01)  AZORG;
077900110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
078000110629             clear  ORGfl3;
078100110628             clear  ORGcar;
078200110628           endif;
078300110628         endif;
078400110629
078500110629         // -?Decodifica distretto?
078600110629         if  ORGfl3 <> SAVcdi;
078700110629           SAVcdi = ORGfl3;
078800110629           clear  ds17;
078900110629           k_TBLkut = 1;
079000110629           k_TBLcod = '17';
079100110629           k_TBLkey = ORGfl3;
079200110629           chain  %kds(k03tabel00)  TABEL;
079300110629           if  %found(TABEL00F)  and  TBLflg = *blank;
079400110629             ds17 = TBLuni;
079500110629           endif;
079600110629         endif;
079700110628
079800110628         // -?Decodifica area?
079900110628         if  ORGcar <> SAVcar;
080000110628           SAVcar = ORGcar;
080100110628           clear  ds05;
080200110628           k_TBLkut = 1;
080300110628           k_TBLcod = '05';
080400110628           k_TBLkey = %editc( ORGcar : 'X' );
080500110628           chain  %kds(k03tabel00)  TABEL;
080600110628           if  %found(TABEL00F)  and  TBLflg = *blank;
080700110628             ds05 = TBLuni;
080800110628           endif;
080900110628         endif;
081000110628
081100110628         // -?Decodifica unificante?
081200110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
081300110628           clear  TIBS69ds;
081400110628           I69sif = knsif;
081500110628           I69kcc = DUTkci;
081600110628           I69kac = ds3C.§3Ccks;
081700110628           tibs69r( tibs69ds :
081800110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
081900110628           if  O69err = *on;
082000110628             clear  CNACOds_Uni;
082100110628           else;
082200110628             CNACOds_Uni = ds_CNACO;
082300110628           endif;
082400110628         EndIf;
082500110628
082600110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
082700110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
082800110628           clear  TIBS69ds;
082900110628           I69sif = knsif;
083000110628           I69kcc = DUTkci;
083100110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
083200110628           tibs69r( tibs69ds :
083300110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
083400110628           if  O69err = *on;
083500110628             clear  ds_CNACO;
083600110628           endif;
083700110628         EndIf;
083800110628
083900140416         // -?Reperimento materiale in comodato file UNCMD00F ?
084000140416         exsr  sr_StampCom ;
084100110628
084200110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
084300110628         // -?(reperimento dati da spedizioni associate)?
084400110628         $Eof_4 = *off;
084500110628         exsr  sr_OpenCursor_4;
084600110628
084700110628         DoU  $Eof_4;
084800110628           exsr  sr_ReadCursor_4;
084900110628         EndDo;
085000110628
085100110628         exsr  sr_CloseCursor_4;
085200110628
085300110628         // -?Scrittura del record a totale per cliente?
085400110629         wSupporto = ds3C.§3Ccba;
085500110628         exsr  sr_Wrt_WFES210;
085600110628
085700110628       ENDSR;
085800110628
085900110628       //--------------------------------------------------------------
086000110628       //?Apertura cursore C4 (totale spedizioni nel periodo x cliente)?
086100110628       //--------------------------------------------------------------
086200110628       BEGSR  sr_OpenCursor_4;
086300110628
086400110628         // -?Preparazione stringa SQL?
086500110628         //  ? N.B. ?- Seleziona le LdV del periodo richiesto?
086600110630         //  ?· Seleziona le LdV del cliente in esame da tab. "3C"?
086700110628         //  ?· Seleziona le LdV con la serie del cliente in esame?
086800110628         //  ?· Seleziona le LdV porto franco?
086900110628
087000110630         wSql_4 = 'select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
087100110630                          count(*), sum( TASncl ) +
087200110628                     from TITASP0F +
087300110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
087400110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
087500110628                          %editc( D07dti : 'X' ) + ' and ' +
087600110628                          %editc( D07dtf : 'X' ) +
087700110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
087800110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
087900110628                  ' group by TASccm +
088000110628
088100110628                   UNION +
088200110628
088300110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
088400110630                          count(*), sum( TASncl ) +
088500110628                     from TITAS10F +
088600110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
088700110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
088800110628                          %editc( D07dti : 'X' ) + ' and ' +
088900110628                          %editc( D07dtf : 'X' ) +
089000110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
089100110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
089200110628                  ' group by TASccm +
089300110628
089400110628                   UNION +
089500110628
089600110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
089700110630                          count(*), sum( TASncl ) +
089800110628                     from TITAS00F +
089900110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
090000110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
090100110628                          %editc( D07dti : 'X' ) + ' and ' +
090200110628                          %editc( D07dtf : 'X' ) +
090300110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
090400110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
090500110628                  ' group by TASccm +
090600110628
090700110628                      FOR FETCH ONLY';
090800110628
090900110628         // -?Dichiarazione cursore?
091000110628         exec sql  prepare S4  from :wSql_4;
091100110628         exec sql  declare C4  cursor for S4;
091200110628
091300110628         // -?Apertura del cursore?
091400110628         exec sql  open C4;
091500110628
091600110628         if  SQLcode < *zero;
091700110628           $EoF_4 = *on;
091800110628           exsr  sr_PrintErr;
091900110628         endif;
092000110628
092100110628       ENDSR;
092200110628
092300110628       //--------------------------------------------------------------
092400110628       //?Lettura cursore C4 (totale spedizioni nel periodo x cliente) ?
092500110628       //--------------------------------------------------------------
092600110628       BEGSR  sr_ReadCursor_4;
092700110628
092800110628         clear  wSQL_ds_TITAS;
092900110628
093000110628         // -?Lettura cursore?
093100110628         exec sql  fetch next  from C4  into :wSQL_ds_TITAS;
093200110628
093300110628         select;
093400110628           // -?Fine flusso?
093500110628           when  SQLcode = 100;
093600110628             $EoF_4 = *on;
093700110628           // -?Errore?
093800110628           when  SQLcode < *zero;
093900110628             $EoF_4 = *on;
094000110628             exsr  sr_PrintErr;
094100110628           // -?Elaborazione dati estratti?
094200110628           other;
094300110628             exsr  sr_Elab_4;
094400110628         endsl;
094500110628
094600110628       ENDSR;
094700110628
094800110628       //--------------------------------------------------------------
094900110628       //?Chiusura cursore C4 (totale spedizioni nel periodo x cliente)?
095000110628       //--------------------------------------------------------------
095100110628       BEGSR  sr_CloseCursor_4;
095200110628
095300110628         // -?Chiusura del cursore?
095400110628         exec sql  close C4;
095500110628
095600110628       ENDSR;
095700110628
095800110628       //--------------------------------------------------------------
095900110628       //?Elaborazione totale spedizioni nel periodo x CCM da TITAS33C ?
096000110628       //--------------------------------------------------------------
096100110628       BEGSR  sr_Elab_4;
096200110628
096300110628         // -?Conteggio numero spedizioni?
096400110628         WE21nts += wSQL_ds_TITAS.sqlTOTspe;
096500110628
096600110628         // -?Conteggio numero colli?
096700110628         WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
096800110628
096900110628         // -?Memorizzazione data ULTIMA spedizione?
097000110628         if  wSQL_ds_TITAS.sqlTASdts > WE21dus;
097100110628           WE21dus = wSQL_ds_TITAS.sqlTASdts;
097200110628         endif;
097300110628
097400110628       ENDSR;
097500110628
097600110628       //--------------------------------------------------------------
097700110628       //?Scrittura record in work-file WFES210F.                      ?
097800110628       //--------------------------------------------------------------
097900110628       BEGSR  sr_Wrt_WFES210;
098000110628
098100110628         // -?Dati nel trk del work-file WFES210F?
098200110628         // -?Parametri di lancio?
098300110628         WE21pdti  = D07dti;
098400110628         WE21pdtf  = D07dtf;
098500110628         WE21pcver = D07cver;
098600110628         WE21pcdsc = D07cdsc;
098700110628         // -?Distretto?
098800110628         WE21cdi = ORGfl3;
098900110628         WE21ddi = ds17.§17des;
099000110628         // -?Area?
099100110628         WE21car = ORGcar;
099200110628         WE21dar = ds05.§05des;
099300110628         // -?Cliente unificante?
099400110628         WE21ccu = ds3C.§3Ccks;
099500110628         WE21rsu = CNACOds_Uni.ACOrag;
099600110628         // -?Cliente mittente tab. "3C"?
099700110628         WE21ccm = %int( %triml( wSQL_ds_3C.sqlTBLkey ) );
099800140911         //WE21rsm = ds3C.§3Crag;
099900140911         WE21rsm = ACORag;
100000110628         WE21blo = ACOabl;
100100110628         // -?Supporto Cliente => BRT?
100200110628         WE21scb = wSupporto;
100300110628         if  wSupporto = c_SuppCB_ESWEB;
100400110628           // -?Dati dalla tab. "3EW"?
100500110628           WE21EWlnp = d3EW.§3EWlnp;
100600120321           WE21fls   = d3EW.§3EWfls;
100700110628           WE21nrs   = d3EW.§3EWnrs;
100800110628           WE21nsi   = d3EW.§3EWnsi;
100900110628           WE21nsf   = d3EW.§3EWnsf;
101000110628           WE21EWctm = d3EW.§3EWctm;
101100110628           WE21EWmad = d3EW.§3EWmad;
101200110628           WE21EWaew = d3EW.§3EWaew;
101300110628         else;
101400110628           // -?Dati dalla tab. "3C"?
101500120321           WE21fls   = ds3C.§3Cfls;
101600110628           WE21nrs   = ds3C.§3Cnrs;
101700110628           WE21nsi   = wSgnClI;
101800110628           WE21nsf   = d3CP.§3CPal;
101900110628         endif;
102000110630         // -?Totali (già calcolati in subr. "sr_Elab_4")?
102100110628         //WE21nts += wSQL_ds_TITAS.sqlTOTspe;
102200110628         //WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
102300110628         //WE21dus  = wSQL_ds_TITAS.sqlTASdts;
102400110706         // -?Dati EasySped (NON per "EasySped-Validate")?
102500110706         if  wSupporto <> c_SuppCB_ESVAL;
102600110706           WE21edat = d3CE.§3CEedat;
102700110706           WE21ever = d3CE.§3CEever;
102800110706           WE21cver = d3CE.§3CEcver;
102900110706           WE21cdsc = d3CE.§3CEcdsc;
103000110706           WE21typw = d3CE.§3CEtypweb;
103100110706           WE21cvermx = d3CE.§3CEcvermx;
103200110706           WE21cdscmx = d3CE.§3CEcdscmx;
103300110706         endif;
103400110630         // -?EasySpedWeb?
103500110706         select;
103600110706           when  wSupporto = c_SuppCB_ESWEB  or
103700110706                 wSupporto = c_SuppCB_ESVAL;
103800110706             WE21esweb = 'SI';
103900110706           when  WE21ever >= '4.0.3 ';
104000110706             WE21esweb = 'SI';
104100110706           other;
104200110706             WE21esweb = 'NO';
104300110706         endsl;
104400110630         // -?Cappario aggiornato?
104500110630         select;
104600110706           when  wSupporto = c_SuppCB_ESWEB;
104700110706             WE21cverag = 'SI';
104800110706           when  wSupporto = c_SuppCB_ESVAL;
104900110706             WE21cverag = 'NO';
105000110630           when  WE21cver   = *zero;
105100110630             WE21cverag = 'NO';
105200110630           when  WE21cver   = D07cver;
105300110630             WE21cverag = 'SI';
105400110630           when  WE21cverMx = D07cver;
105500110630             WE21cverag = 'SI';
105600120601           when  WE21cver   = (D07cver - 1)  and  WE21cdsc = D07cdsc;
105700120601             WE21cverag = 'SI';
105800110630           other;
105900110630             WE21cverag = 'NO';
106000110630         endsl;
106100140416         // -?Materiale in comodato?
106200140416         WE21stamco = $comodato  ;
106300141024         // -?Cod. e Nominativo commerciale
106400141027         WE21CMU = *zeros;
106500141027         WE21CMD = *blank;
106600141027         // se esiste un cod. unificante
106700141027         if WE21CCU <> *zeros;
106800141027           // reperisco il suo cod.agente
106900141024           clear  TIBS69ds;
107000141024           I69sif = knsif;
107100141024           I69kcc = DUTkci;
107200141024           I69kcp = WE21CCU;
107300141024           tibs69r( tibs69ds :
107400141024                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
107500141024           if  O69err = *on;
107600141024             clear  CNCLPds_Uni;
107700141024           else;
107800141024             CNCLPds_Uni = ds_CNCLP;
107900141024           endif;
108000141027           // reperisco il cod. commerciale unificante mediante l'agente del cliente
108100141027           k_CMMCOD = CLPAGE;
108200141027           chain  %kds(k01azcmm01)  AZCMM000;
108300141027           if  %found();
108400141027             WE21CMU = CMMUNI;
108500141027             k_CMMCOD = WE21CMU;
108600141027             chain  %kds(k01azcmm01)  AZCMM000;
108700141027             // reperisco la descrizione del cod. commerciale unificante
108800141027             if  %found();
108900141027               WE21CMD = CMMDES;
109000141027             endif;
109100141027           endif;
109200141024         EndIf;
109300110628
109400110628         //______________
109500110628         WRITE  WFES2100;
109600110628         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109700110628
109800110628       ENDSR;
109900110628
110000110628       //--------------------------------------------------------------
110100110628       //?Reperimento stampanti in concordato da tab. "3R"             ?
110200110628       //--------------------------------------------------------------
110300140416       //BEGSR  sr_StampConc;
110400110628
110500140416       //  clear  ds3R;
110600110628
110700110628         // -?1° tentativo con il cliente in esame?
110800140416       //  clear  yy;
110900140416       //  yy = %lookup( %trimr( wSQL_ds_3C.sqlTBLkey ) : $Cli );
111000110628
111100110628         // -?2° tentativo con il padre del cliente in esame?
111200140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
111300140416       //    clear  yy;
111400140416       //    yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Cli );
111500140416       //  EndIf;
111600110628
111700110628         // -?3° tentativo con gli altri fratelli?
111800140416       //  If  yy = *zero   or   $StpComodato(yy) = *blank;
111900140416       //    clear  yy;
112000140416       //    DoU  yy = *zero   or   $StpComodato(yy) <> *blank;
112100140416       //      yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Padre : yy + 1 );
112200140416       //    EndDo;
112300140416       //  EndIf;
112400110628
112500110628         // -?Imposta la stampante in ds3R (se reperita)?
112600140416       //  If  yy > *zero;
112700140416       //    ds3R.§3Rnot = $StpComodato(yy);
112800140416       //  EndIf;
112900140416       //--------------------------------------------------------------
113000140416       //?Verifica presenza comodati            ?
113100140416       //--------------------------------------------------------------
113200140416       BEGSR  sr_StampCom;
113300140415         clear $comodato;
113400140414         // -?Con il cliente in esame verifico legame "ST"
113500140414         //   Verifico se codice presente come padre
113600140414         clear tibs10ds;
113700140414         d10tle='ST';
113800140414         d10paf='F';
113900140415         d10cod=%dec(%trimr( wSQL_ds_3C.sqlTBLkey ):11:0);
114000140415         tibs10r(tibs10ds);
114100140415         if d10cop<=0;
114200140415         //   Se non è padre verifico se è figlio
114300140415            d10paf='P';
114400140415            tibs10r(tibs10ds);
114500140415            if d10cop<=0;
114600140415         //  Se non è nè padre nè figlio è cliente singolo
114700140415               d10cop=d10cod;
114800141016               skc(1)=%editc(d10cod:'X');
114900140415            endif;
115000140415         endif;
115100140416         // -?Verifico se il padre del legame ST è già stato analizzato per
115200140416         // -?i comodati, altrimenti lo analizzo e lo memorizzo in schiera
115300140416         // -?Schiera $padst (clienti padri legame ST)
115400140416         // -?Schiera $com   (indica se presente comodato S=Comodato)
115500140415         yy=%lookup(d10cop:$padst);
115600140415         if yy=0;
115700140415            yy=%lookup(0:$padst);
115800140415            $padst(yy)=d10cop      ;
115900140415         // Verifico se presente materiale in comodato nella famiglia e
116000140415         // memorizzo in schiera
116100140416            clear wcom;
116200140416            for i=1 to %elem(sKC);
116300140416               if skc(i)=*blanks or skc(i)=*zeros or wcom='S';
116400140415                  leave;
116500140415               endif;
116600140416               kcli=%dec(%subst(skc(I):5:7):7:0);
116700140415               exsr sr_comodato;
116800140416               if wcom = 'S';
116900140416                  $com(yy)='S';
117000140416               endif ;
117100140415            endfor;
117200140415         endif;
117300140415         if $com(yy)='S';
117400140415            $comodato='Materiale in comodato';
117500140415         endif;
117600110628       ENDSR;
117700110628
117800140416       //--------------------------------------------------------------
117900140416       //?Ricerca comodato per cliente                                 ?
118000140416       //--------------------------------------------------------------
118100140416       BEGSR  sr_Comodato;
118200140416       setll kcli uncmd01l;
118300140416       reade kcli uncmd01l;
118400140416       dow not %eof(uncmd01l) ;
118500140416          if cmdcfg<>'A' and cmdcfg<>'C';
118600140416             wcom='S';
118700140416             leave;
118800140416          endif;
118900140416          reade kcli uncmd01l;
119000140416       enddo;
119100140416       endsr;
119200110628       //--------------------------------------------------------------
119300110628       //?Apertura cursore C3 (creazione WFES220F con totali x area).  ?
119400110628       //--------------------------------------------------------------
119500110628       BEGSR  sr_OpenCursor_3;
119600110628
119700110628         // -?Dichiarazione cursore?
119800110628         exec sql  declare C3  cursor for
119900110628                   select *
120000110628                     from WFES210F
120100110628                    order by WE21car, WE21scb, WE21ccu, WE21ccm
120200110628                      for read only;
120300110628
120400110628         // -?Apertura cursore?
120500110628         exec sql  open C3;
120600110628
120700110628         clear  WFES2200;
120800110628
120900110628       ENDSR;
121000110628
121100110628       //--------------------------------------------------------------
121200110628       //?Lettura cursore C3 (creazione WFES220F con totali x area).
121300110628       //--------------------------------------------------------------
121400110628       BEGSR  sr_ReadCursor_3;
121500110628
121600110628         // -?Lettura cursore?
121700110628         exec sql  fetch  NEXT  from C3  into :WFES210ds;
121800110628
121900110628         select;
122000110628           when  SQLcode = 100;
122100110628             $EoF_3 = *on;
122200110628           when  SQLcode < *zero;
122300110628             $EoF_3 = *on;
122400110628             exsr  sr_PrintErr;
122500110628           other;
122600110628             exsr  sr_Elab_3;
122700110628         endsl;
122800110628
122900110628       ENDSR;
123000110628
123100110628       //--------------------------------------------------------------
123200110628       //?Creazione work-file WFES220F con totali per area.            ?
123300110628       //--------------------------------------------------------------
123400110628       BEGSR  sr_Elab_3;
123500110628
123600110628         // -?Rottura Area?
123700111122         IF  WFES210ds.WE21car <> WE22car;
123800110628
123900110628           // -?Scrittura record con i totali dell'area precedente?
124000110628           if  WE22car <> *zero;
124100111123             exsr  sr_Wrt_WFES220;
124200110628           endif;
124300110628
124400110628           // -?Impostazione dati nuova area?
124500110628           clear  WFES2200;
124600110628           WE22pdti  = WFES210ds.WE21pdti;
124700110628           WE22pdtf  = WFES210ds.WE21pdtf;
124800110628           WE22pcver = WFES210ds.WE21pcver;
124900110628           WE22pcdsc = WFES210ds.WE21pcdsc;
125000110628           WE22car   = WFES210ds.WE21car;
125100110628           WE22dar   = WFES210ds.WE21dar;
125200110628
125300110628         ENDIF;
125400110628
125500110628
125600110628         // -?Incremento totali dell'area in elaborazione?
125700111122         Select;
125800111122
125900111122           // -? EasySpeb ?
126000111123           When  WFES210ds.WE21scb = c_SuppCB_ESYSP;
126100111122             // -?Num. totale spedizioni nel periodo?
126200111122             WE22esnts  += WFES210ds.WE21nts;
126300111122             // -?Num. totale colli nel periodo?
126400111122             WE22esntc  += WFES210ds.WE21ntc;
126500111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
126600111122             if  WFES210ds.WE21stamco = *blank;
126700111122               WE22esntcn += WFES210ds.WE21ntc;
126800111122             endif;
126900111122             // -?Num. totale clienti padre?
127000111122             if  WFES210ds.WE21ccu <> SAVccu;
127100111122               SAVccu = WFES210ds.WE21ccu;
127200111122               WE22esnksu += 1;
127300111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
127400111122               exsr  sr_Elab_5;
127500111122               if  wNTS = *zero;
127600111122                 WE22esnuns += 1;
127700111122               endif;
127800111122             endif;
127900111122             // -?Num. totale clienti in tab. "3C"?
128000111122             WE22esncli += 1;
128100111123             //  ?(scartati quelli bloccati)?
128200111122             IF  WFES210ds.WE21blo = *blank;
128300111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
128400111122               If  WFES210ds.WE21nts > *zero;
128500111122                 WE22esncls += 1;
128600111122                 // ·?...ma SENZA cappario aggiornato?
128700111122                 if  WFES210ds.WE21cverag = 'NO';
128800111122                   WE22esncca += 1;
128900111122                 endif;
129000111122                 // ·?...ma SENZA EasySpedWeb?
129100111122                 if  WFES210ds.WE21esweb  = 'NO';
129200111122                   WE22esnclw += 1;
129300111122                 endif;
129400111122               EndIf;
129500111122             ENDIF;
129600111122
129700111122           // -? EasySpeb Validate ?
129800111123           When  WFES210ds.WE21scb = c_SuppCB_ESVAL;
129900111122             // -?Num. totale spedizioni nel periodo?
130000111122             WE22evnts  += WFES210ds.WE21nts;
130100111122             // -?Num. totale colli nel periodo?
130200111122             WE22evntc  += WFES210ds.WE21ntc;
130300111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
130400111122             if  WFES210ds.WE21stamco = *blank;
130500111122               WE22evntcn += WFES210ds.WE21ntc;
130600111122             endif;
130700111122             // -?Num. totale clienti padre?
130800111122             if  WFES210ds.WE21ccu <> SAVccu;
130900111122               SAVccu = WFES210ds.WE21ccu;
131000111122               WE22evnksu += 1;
131100111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
131200111122               exsr  sr_Elab_5;
131300111122               if  wNTS = *zero;
131400111122                 WE22evnuns += 1;
131500111122               endif;
131600111122             endif;
131700111122             // -?Num. totale clienti in tab. "3C"?
131800111122             WE22evncli += 1;
131900111123             //  ?(scartati quelli bloccati)?
132000111122             IF  WFES210ds.WE21blo = *blank;
132100111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
132200111122               If  WFES210ds.WE21nts > *zero;
132300111122                 WE22evncls += 1;
132400111122               EndIf;
132500111122             ENDIF;
132600111123
132700111123           // -? EasySpeb WEB ?
132800111123           When  WFES210ds.WE21scb = c_SuppCB_ESWEB;
132900111123             // -?Num. totale spedizioni nel periodo?
133000111123             WE22ewnts  += WFES210ds.WE21nts;
133100111123             // -?Num. totale colli nel periodo?
133200111123             WE22ewntc  += WFES210ds.WE21ntc;
133300111123             // -?Num. totale spedizioni e colli senza stampanti in comodato?
133400111123             if  WFES210ds.WE21stamco = *blank;
133500111123               WE22ewntcn += WFES210ds.WE21ntc;
133600111123             endif;
133700111123             // -?Num. totale clienti padre?
133800111123             if  WFES210ds.WE21ccu <> SAVccu;
133900111123               SAVccu = WFES210ds.WE21ccu;
134000111123               WE22ewnksu += 1;
134100111123               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
134200111123               exsr  sr_Elab_5;
134300111123               if  wNTS = *zero;
134400111123                 WE22ewnuns += 1;
134500111123               endif;
134600111123             endif;
134700111123             // -?Num. totale clienti in tab. "3C"?
134800111123             WE22ewncli += 1;
134900111123             //  ?(scartati quelli bloccati)?
135000111123             IF  WFES210ds.WE21blo = *blank;
135100111123               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
135200111123               If  WFES210ds.WE21nts > *zero;
135300111123                 WE22ewncls += 1;
135400111123               EndIf;
135500111123             ENDIF;
135600111122
135700111122         EndSl;
135800110628
135900110628       ENDSR;
136000110628
136100110628       //--------------------------------------------------------------
136200110630       //?Chiusura cursore C3 (creazione WFES220F con totali x area).
136300110628       //--------------------------------------------------------------
136400110628       BEGSR  sr_CloseCursor_3;
136500110628
136600110628         // -?Scrittura ultimo record?
136700110628         if  WE22car <> *zero;
136800111123           exsr  sr_Wrt_WFES220;
136900110628         endif;
137000110628
137100110628         // -?Chiusura cursore?
137200110628         exec sql  close C3;
137300110628
137400110628       ENDSR;
137500111123
137600111123       //--------------------------------------------------------------
137700111123       //?Scrittura record in work-file WFES220F.                      ?
137800111123       //--------------------------------------------------------------
137900111123       BEGSR  sr_Wrt_WFES220;
138000111123
138100111123         // -? Totali AREA ?
138200111123         // ·?Num. totale spedizioni nel periodo?
138300111123         WE22tants  = WE22esnts + WE22evnts + WE22ewnts;
138400111123         // ·?Num. totale colli nel periodo?
138500111123         WE22tantc  = WE22esntc + WE22evntc + WE22ewntc;
138600111123         // ·?Num. totale spedizioni e colli senza stampanti in comodato?
138700111123         WE22tantcn = WE22esntcn + WE22evntcn + WE22ewntcn;
138800111123         // ·?Num. totale clienti padre?
138900111123         WE22tanksu = WE22esnksu + WE22evnksu + WE22ewnksu;
139000111123         // ·?"Raggruppamento" clienti SENZA spedizioni nel periodo?
139100111123         WE22tanuns = WE22esnuns + WE22evnuns + WE22ewnuns;
139200111123         // ·?Num. totale clienti in tab. "3C"?
139300111123         WE22tancli = WE22esncli + WE22evncli + WE22ewncli;
139400111123         // ·?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
139500111123         //  ?(scartati quelli bloccati)?
139600111123         WE22tancls = WE22esncls + WE22evncls + WE22ewncls;
139700111123
139800111123         //______________
139900111123         WRITE  WFES2200;
140000111123         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
140100111123
140200111123       ENDSR;
140300110628
140400110628       //--------------------------------------------------------------
140500110628       //?Verifica se padre & figli senza spedizioni nel periodo.      ?
140600110628       //--------------------------------------------------------------
140700110628       BEGSR  sr_Elab_5;
140800110628
140900110628         // -?Dichiarazione cursore?
141000110628         exec sql  declare C5  cursor for
141100110628                    select sum(WE21nts)
141200110628                      from WFES210F
141300110628                     where WE21ccu = :WFES210ds.WE21ccu
141400110628                       for read only;
141500110628
141600110628         // -?Apertura cursore?
141700110628         exec sql  open C5;
141800110628
141900110628         // -?Lettura cursore?
142000110628         exec sql  fetch  NEXT  from C5  into :wNTS;
142100110628
142200110628         select;
142300110628           when  SQLcode = 100;
142400110628             $EoF_5 = *on;
142500110628           when  SQLcode < *zero;
142600110628             $EoF_5 = *on;
142700110628             exsr  sr_PrintErr;
142800110628           other;
142900111122             //if  wNTS = *zero;
143000111122             //  WE22nksuns += 1;
143100111122             //endif;
143200110628         endsl;
143300110628
143400110628         // -?Chiusura cursore?
143500110628         exec sql  close C5;
143600110628
143700110628       ENDSR;
143800110628
143900110628       //--------------------------------------------------------------
144000110628       //?Ultimo aggiornamento (dati di EasySped & relativo flag).     ?
144100110628       //--------------------------------------------------------------
144200110628       BEGSR  sr_Upd_EsySp;
144300110628
144400110628         exsr  sr_OpenCursor_6;
144500110628
144600110628         clear wEsySp_ds;
144700110628         dou  $Eof_6 = *on;
144800110628           exsr  sr_ReadCursor_6;
144900110628         enddo;
145000110628
145100110628         exsr  sr_CloseCursor_6;
145200110628
145300110628       ENDSR;
145400110628
145500110628       //--------------------------------------------------------------
145600110628       //?Apertura cursore C6.                                         ?
145700110628       //--------------------------------------------------------------
145800110628       BEGSR  sr_OpenCursor_6;
145900110628
146000110628         // -?Dichiarazione cursore?
146100110628         exec sql  declare C6 cursor for
146200110628
146300110628                    select WE21ccu, WE21flgu,
146400110628                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
146500110628                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg,
146600110628                           rrn(WFES210F)
146700110628                      from WFES210F
146800110628                     order by WE21ccu, WE21cver desc
146900110628
147000110628                       for fetch only;
147100110628
147200110628         // -?Apertura cursore?
147300110628         exec sql  open C6;
147400110628
147500110628       ENDSR;
147600110628
147700110628       //--------------------------------------------------------------
147800110628       //?Chiusura cursore C6.                                         ?
147900110628       //--------------------------------------------------------------
148000110628       BEGSR  sr_CloseCursor_6;
148100110628
148200110628         // -?Chiusura cursore?
148300110628         exec sql  close C6;
148400110628
148500110628       ENDSR;
148600110628
148700110628       //--------------------------------------------------------------
148800110628       //?Lettura cursore C6 (rec. WFES210F senza info EasySped).      ?
148900110628       //--------------------------------------------------------------
149000110628       BEGSR  sr_ReadCursor_6;
149100110628
149200110628         // -?Lettura cursore?
149300110628         exec sql  fetch  NEXT  from C6  into :wSQL_ds_WFES21;
149400110628
149500110628         select;
149600110628           when  SQLcode = 100;
149700110628             $Eof_6 = *on;
149800110628           when  SQLcode < *zero;
149900110628             $Eof_6 = *on;
150000110628           other;
150100110628             exsr  sr_Elab_6;
150200110628         endsl;
150300110628
150400110628       ENDSR;
150500110628
150600110628       //--------------------------------------------------------------
150700110630       //?Aggiornamento rec. WFES210F per info EasySped.               ?
150800110628       //--------------------------------------------------------------
150900110628       BEGSR  sr_Elab_6;
151000110628
151100110628         select;
151200110628
151300110628           // -?Record con la versione: salvo cod. cliente + dati della?
151400110628           //  ?versione?
151500111117           //  ?NO: la versione (anche se meno recente) del 2° cliente?
151600111117           //      ?della famiglia si sovrapporrebbe a quella del 1°...?
151700111117           //when  wSQL_ds_WFES21.sqlWESever <> *blank;
151800111117
151900111117           // -?1° Record con la versione: salvo cod. cliente + dati?
152000111117           //  ?della versione?
152100111117           //  ?Meglio tenere comunque la versione più recente (quella?
152200111117           //  ?nel 1° record letto via vedi SQL)!?
152300111117           when  wSQL_ds_WFES21.sqlWESccu <> ESYWESccu;
152400110628             wEsySp_ds = wSQL_ds_WFES21;
152500110628
152600111117           // -?Record senza la versione: se unificante = al precedente?
152700110628           //  ?aggiorno il record con la versione salvata?
152800110628           when  wSQL_ds_WFES21.sqlWESever = *blank  and
152900110628                 wSQL_ds_WFES21.sqlWESccu  = ESYWESccu;
153000110628             chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
153100110628             if  %found(WFES210F);
153200110628               WE21flgu = 'U';
153300110628               WE21edat = ESYWESedat;
153400110628               WE21ever = ESYWESever;
153500110628               WE21cver = ESYWEScver;
153600110628               WE21typw = ESYWEStypw;
153700110628               WE21cdsc = ESYWEScdsc;
153800110628               WE21cvermx = ESYWEScvermax;
153900110628               WE21cdscmx = ESYWEScdscmax;
154000110628               WE21esweb  = ESYWESesweb;
154100110628               WE21cverag = ESYWEScveragg;
154200110628               //______________
154300110628               update WFES2100;
154400110628               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
154500110628             endif;
154600110628
154700110628         endsl;
154800110628
154900110628       ENDSR;
155000110628
155100110628       //--------------------------------------------------------------
155200110628       //?Stampa segnalazione dell'errore rilevato.                    ?
155300110628       //--------------------------------------------------------------
155400110628       BEGSR  sr_PrintErr;
155500110628
155600110628         // -?Stampa del Dump?
155700110628         Dump(A);
155800110628
155900110628         // -?Stampa del Job-Log?
156000110628         Qcmd = 'DSPJOBLOG job(*) output(*print)';
156100110628         exsr  sr_ExecCmd;
156200110628
156300110628         // -?Chiusura del programma?
156400110628         exsr  sr_RoutEnd;
156500110628
156600110628       ENDSR;
156700110628
156800110628       //--------------------------------------------------------------
156900110628       //?Esecuzione del comando (già impostato).                      ?
157000110628       //--------------------------------------------------------------
157100110628       BEGSR  sr_ExecCmd;
157200110628
157300110628         clear Qcap0100;
157400110628         Qcabcsdh = *off;
157500110628         Qcapa    = *off;
157600110628         Qcacmdss = *off;
157700110628         Qcaerved = *allX'00';
157800110628
157900110628         clear Qusec;
158000110628         Qusbprv  = %size(Qusec);
158100110628
158200110628         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
158300110628                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
158400110628                           0 : 0 : Qusec);
158500110628
158600110628         //if  Qusei <> *blank;
158700110628         //  ...
158800110628         //endif;
158900110628
159000110628       ENDSR;
159100110628
159200110628       //--------------------------------------------------------------
159300110628       //?Operazioni finali.                                           ?
159400110628       //--------------------------------------------------------------
159500110628       BEGSR  sr_RoutEnd;
159600110628
159700110628         // -?Chiusura work-file?
159800110628         close(e)  WFES220F;
159900110628         close(e)  WFES210F;
160000110628
160100110628         // -?Chiusura applicazioni?
160200110628         reset  TIBS69ds;
160300110628         tibs69r( tibs69ds :
160400110628                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
160500110628
160600110628         // -?Uscita del *pgm?
160700110628         return;
160800110628
160900110628       ENDSR;
161000110628
161100110628      /end-free
