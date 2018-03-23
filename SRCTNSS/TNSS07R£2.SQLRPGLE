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
002900110628
003000110628       // -?Work-File di dettaglio?
003100110628     fWFES210F  Uf A e             disk    usropn
003200110628
003300110628       // -?Work-File di totale?
003400110628     fWFES220F  O    e             disk    usropn
003500110628
003600110628       //--------------------------------------------------------------
003700110628       //?Definizione costanti.                                        ?
003800110628       //--------------------------------------------------------------
003900110628
004000110628       // -?Comandi per pulizia Work-Files?
004100110628     d c_Cmd_ClrWF1    c                   const('clrpfm file(*libl/+
004200110628     d                                                   WFES210F)')
004300110628     d c_Cmd_ClrWF2    c                   const('clrpfm file(*libl/+
004400110628     d                                                   WFES220F)')
004500110628
004600110628       // -?Supporti Cliente => BRT gestiti?
004700110628     d c_SuppCB_ESYSP  c                   const('ESYSP')
004800110628     d c_SuppCB_ESVAL  c                   const('ESVAL')
004900110628     d c_SuppCB_ESWEB  c                   const('ESWEB')
005000110628
005100110628       //--------------------------------------------------------------
005200110628       //?Definizione schiere.                                         ?
005300110628       //--------------------------------------------------------------
005400110628
005500130110     d c_MaxElem       c                   const(25000)
005600110628       // -?Clienti in "3C" con famiglia?
005700110628     d $Cli            s              7a   dim(c_MaxElem)    inz
005800110628       // -?Padri   in "3C" con famiglia?
005900110628     d $Padre          s              7a   dim(c_MaxElem)    inz
006000110628       // -?Stampanti in comodato (da tab. "3R") x ogni cliente in "3C"?
006100110628     d $StpComodato    s                   like(ds3R.§3Rnot)
006200110628     d                                     dim(c_MaxElem)  inz
006300110628
006400110628       //--------------------------------------------------------------
006500110628       //?Definizione aree dati.                                       ?
006600110628       //--------------------------------------------------------------
006700110628
006800110628       // -?Dati utente?
006900110628     d §AzUte        e ds                  extname(AZUTE00F)
007000110628     d                                     dtaara
007100110628     d §DatiUte      e ds                  extname(dDatiUte)
007200110628     d                                     dtaara
007300110628
007400110628       //--------------------------------------------------------------
007500110628       //?Definizione strutture dati.                                  ?
007600110628       //--------------------------------------------------------------
007700110628
007800110628       // -?Status ds?
007900110628     d Status         sds
008000110628     d  SDSpgm           *proc
008100110628     d  SDSprm           *parms
008200110628
008300110628       // -?Parametri ricevuti?
008400110628     d KPJBA         e ds
008500110628     d TNSS07ds      e ds                  inz
008600110628     d   D07dti      e                     inz(*loval)
008700110628     d   D07dtf      e                     inz(*hival)
008800110628
008900110628       // -?Tab. "3CE" = Versioni EasySped per cliente?
009000110628     d d3CE          e ds                  inz         qualified
009100110628       // -?Tab. "3CP" = Serie parziali assegnate ai clienti NON ESWEB?
009200110628     d d3CP          e ds                  inz         qualified
009300110628     d   §3CPal      e                     inz(*hival)
009400110628       // -?Tab. "3EW" = Dati assegnati alla postazione EasySpedWeb?
009500110628     d d3EW          e ds                  inz         qualified
009600110628
009700110628       // -?Tab. "3C"  = Invio dati -bollettazione-?
009800110628     d ds3C          e ds                  inz         qualified
009900110628       // -?Tab. "3R"  = Scheda clienti per scambio dati?
010000110628     d ds3R          e ds                  inz         qualified
010100110628       // -?Tab. "05"  = Aree?
010200110628     d ds05          e ds                  inz         qualified
010300110628       // -?Tab. "17"  = Distretti?
010400110628     d ds17          e ds                  inz         qualified
010500110628
010600110628       // -?Strutture dati "di comodo"?
010700110628       //  ?(per la definizione dei dati da estrarre)?
010800110628     d TIVSSds       e ds                  extname(TIVSS00F)
010900110628     d                                     inz         qualified
011000110628     d TNTBEds       e ds                  extname(TNTBE00F)
011100110628     d                                     inz         qualified
011200110628     d TITASds       e ds                  extname(TITAS00F)
011300110628     d                                     inz         qualified
011400110628     d CNACOds_Uni   e ds                  extname(CNACO00F)
011500110628     d                                     inz         qualified
011600110628
011700110628       // -?Dati estratti via SQL:?
011800110628       // - -?dal file TABEL00F per tab. "3C"?
011900110628     d wSQL_ds_3C      ds                  inz         qualified
012000110628     d   sqlTBLkey                         inz  like(TBLkey)
012100110628     d   sqlTBLuni                         inz  like(TBLuni)
012200110630       // - -?dal file TNTBE00F per tab. "3CP"?
012300110630     d wSQL_ds_3CP     ds                  inz         qualified
012400110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
012500110630     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
012600110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
012700110630       // - -?dal file TNTBE00F per tab. "3CE"?
012800110630     d wSQL_ds_3CE     ds                  inz         qualified
012900110630     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
013000110630     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
013100110628       // - -?dal file TNTBE00F per tab. "3EW"?
013200110628     d wSQL_ds_3EW     ds                  inz         qualified
013300110628     d   sqlTBEke1                         inz    like(TNTBEds.TBEke1)
013400110628     d   sqlTBEke2                         inz    like(TNTBEds.TBEke2)
013500110628     d   sqlTBEuni                         inz    like(TNTBEds.TBEuni)
013600110630       // - -?dal file TIVSS00F se VSSISV = "EW"?
013700110630     d wSQL_ds_TIVSS   ds                  inz         qualified
013800110630     d   sqlVSSksu                         inz    like(TIVSSds.VSSksu)
013900110630     d   sqlVSSsun                         inz    like(TIVSSds.VSSsun)
014000110630     d   sqlVSSisv                         inz    like(TIVSSds.VSSisv)
014100110630     d*//sqlVSSdsc                         inz    like(TIVSSds.VSSdsc)
014200110628
014300110628       // -?Dati estratti via SQL:?
014400110628       // - -?dal file TITAS33C x CCM/AAS/MGS?
014500110628     d wSQL_ds_TITAS   ds                  inz         qualified
014600110628     d   sqlTASccm                         inz    like(TITASds.TASccm)
014700110628     d*//sqlTASaas                         inz    like(TITASds.TASaas)
014800110628     d*//sqlTASmgs                         inz    like(TITASds.TASmgs)
014900110628     d*//sqlTASnrs                         inz    like(TITASds.TASnrs)
015000110628     d   sqlTASdts                    8  0 inz
015100110628     d   sqlTOTspe                    9  0 inz
015200110628     d   sqlTOTcol                    9  0 inz
015300110628     d   sqlMAXncd                    9  0 inz
015400110628     d   sqlMAXnca                    9  0 inz
015500110628
015600110628       // -?Dati estratti via SQL:?
015700110628       // - -?dal file WFES210F x aggiornamento info EasySped?
015800110628     d wSQL_ds_WFES21  ds                  inz         qualified
015900110628     d  sqlWESccu                          inz    like(WE21ccu)
016000110628     d  sqlWESflgu                         inz    like(WE21flgu)
016100110628     d  sqlWESedat                         inz    like(WE21edat)
016200110628     d  sqlWESever                         inz    like(WE21ever)
016300110628     d  sqlWEScver                         inz    like(WE21cver)
016400110628     d  sqlWEScdsc                         inz    like(WE21cdsc)
016500110628     d  sqlWEStypw                         inz    like(WE21typw)
016600110628     d  sqlWEScverMax                      inz    like(WE21cverMx)
016700110628     d  sqlWEScdscMax                      inz    like(WE21cdscMx)
016800110628     d  sqlWESesweb                        inz    like(WE21esweb)
016900110628     d  sqlWEScverAgg                      inz    like(WE21cverag)
017000110628     d  SQLrrn                        8  0 inz
017100110628
017200110628       // -?Salvataggio dati (del padre) estratti in wSQL_WFES21?
017300110628     d wEsySp_ds       ds                  inz
017400110628     d  ESYwesccu                          like(wSQL_ds_WFES21.sqlWESccu)
017500110628     d                                     inz
017600110628     d  ESYwesflgu                         like(wSQL_ds_WFES21.sqlWESflgu)
017700110628     d                                     inz
017800110628     d  ESYwesedat                         like(wSQL_ds_WFES21.sqlWESedat)
017900110628     d                                     inz
018000110628     d  ESYwesever                         like(wSQL_ds_WFES21.sqlWESever)
018100110628     d                                     inz
018200110628     d  ESYwescver                         like(wSQL_ds_WFES21.sqlWEScver)
018300110628     d                                     inz
018400110628     d  ESYwescdsc                         like(wSQL_ds_WFES21.sqlWEScdsc)
018500110628     d                                     inz
018600110628     d  ESYwestypw                         like(wSQL_ds_WFES21.sqlWEStypw)
018700110628     d                                     inz
018800110628     d  ESYwescverMax                      like(wSQL_ds_WFES21.sqlWEScverMax)
018900110628     d                                     inz
019000110628     d  ESYwescdscMax                      like(wSQL_ds_WFES21.sqlWEScdscMax)
019100110628     d                                     inz
019200110628     d  ESYwesesweb                        like(wSQL_ds_WFES21.sqlWESesweb)
019300110628     d                                     inz
019400110628     d  ESYwescverAgg                      like(wSQL_ds_WFES21.sqlWEScverAgg)
019500110628     d                                     inz
019600110628
019700110628       // -?Dati estratti via SQL:?
019800110628       // - -?dal file WFES210F x totalizzazione in WFES220F?
019900110628     d WFES210ds     e ds                  extname(WFES210F)
020000110628     d                                     inz         qualified
020100110628
020200110628       //--------------------------------------------------------------
020300110628       //?Definizione variabili globali.                               ?
020400110628       //--------------------------------------------------------------
020500110628
020600110628       // -?Flags booleani?
020700110628     d $EoF_1          s               n   inz
020800110628     d $EoF_2          s               n   inz
020900110628     d $EoF_3          s               n   inz
021000110628     d $EoF_4          s               n   inz
021100110628     d $EoF_5          s               n   inz
021200110628     d $EoF_6          s               n   inz
021300110628
021400110628       // -?Stringhe SQL da eseguire?
021500110628     d wSql            s           5000    inz  varying
021600110628     d wSql_4          s           5000    inz  varying
021700110628
021800110628       // -?Indici di schiera?
021900110628     d xx              s              5  0 inz
022000110628     d yy              s              5  0 inz
022100110628
022200110628       // -?Campi di comodo?
022300110628     d wDate           s              8  0 inz
022400110628     d wNTS            s              9  0 inz
022500110630     d SAVcdi          s                   inz(*loval) like(ORGfl3)
022600110628     d SAVcar          s                   inz(*loval) like(WE21car)
022700110628     d SAVccu          s                   inz(*loval) like(WE21ccu)
022800110628     d wSgnClI         s                   inz(1)      like(WE21nsi)
022900110630     d wSupporto       s                   inz         like(WE21scb)
023000110628
023100110628       //--------------------------------------------------------------
023200110628       //?Definizione prototipi procedure.                             ?
023300110628       //--------------------------------------------------------------
023400110628
023500110628       // -?Reperimento dati utente?
023600110628     d TIBS34ds      e ds                  inz
023700110628      /copy gaitrasrc/srcProtoPr,TIBS34R
023800110628
023900110628       // -?Controllo/Decodifica cliente?
024000110628      /copy gaitrasrc/srcProtoPI,TIBS69R
024100110628      /copy gaitrasrc/srcProtoPR,TIBS69R
024200110628
024300110628       // -?Parametri API QCAPCMD (Process Commands)?
024400110628     d Qcmd            s           2048    inz  varying
024500110628      /copy qSysInc/qRpgleSrc,QCAPCMD
024600110628       // -?API QCAPCMD (Process Commands)?
024700110628      /copy gaitrasrc/srcProtoPR,QCAPCMD
024800110628
024900110628       // -?Parametri gestione errori API.?
025000110628      /copy qsysinc/qrpglesrc,QUSEC
025100110628
025200110628       //--------------------------------------------------------------
025300110628       //?Definizione key-list.                                        ?
025400110628       //--------------------------------------------------------------
025500110628
025600110628       // -?File TABEL00F?
025700110628     d k03tabel00    e ds                  extname(TABEL00F : *key)
025800110628     d                                     prefix(k_)   inz
025900110628       // -?File AZORG01L?
026000110628     d k01azorg01    e ds                  extname(AZORG01L : *key)
026100110628     d                                     prefix(k_)   inz
026200110628
026300110628       //--------------------------------------------------------------
026400110628       //?M A I N - L I N E                                            ?
026500110628       //--------------------------------------------------------------
026600110628
026700110628     c     *Entry        plist
026800110628     c                   parm                    KPJBA
026900110628
027000110628      /free
027100110628
027200110628       // -?Operazioni iniziali?
027300110628       exsr  sr_RoutInz;
027400110628
027500110628       // -?1° ciclo di elaborazione: creazione WFES210F (dettaglio)?
027600110628       //                            ?per clienti EasySped?
027700110628       exsr  sr_OpenCursor_1;
027800110628
027900110628       DoU  $Eof_1 = *on;
028000110628         exsr  sr_ReadCursor_1;
028100110628       EndDo;
028200110628
028300110628       exsr  sr_CloseCursor_1;
028400110628
028500110628       // -?2° ciclo di elaborazione: creazione WFES210F (dettaglio)?
028600110628       //                            ?per clienti EasySpedWeb?
028700110628       exsr  sr_OpenCursor_2;
028800110628
028900110628       DoU  $Eof_2 = *on;
029000110628         exsr  sr_ReadCursor_2;
029100110628       EndDo;
029200110628
029300110628       exsr  sr_CloseCursor_2;
029400110628
029500110628       // -?Aggiornamento dati EasySped/EasySpedWeb mancanti...?
029600110628       exsr  sr_Upd_EsySp;
029700110628
029800110628       // -?3° ciclo di elaborazione: creazione WFES220F (totale area)?
029900110628       exsr  sr_OpenCursor_3;
030000110628
030100110628       dou  $EoF_3 = *on;
030200110628         exsr  sr_ReadCursor_3;
030300110628       enddo;
030400110628
030500110628       exsr  sr_CloseCursor_3;
030600110628
030700110628       // -?Fine programma?
030800110628       exsr  sr_RoutEnd;
030900110628
031000110628       //--------------------------------------------------------------
031100110628       //?Operazioni iniziali.                                         ?
031200110628       //--------------------------------------------------------------
031300110628       BEGSR  sr_RoutInz;
031400110628
031500110628         *inLR = *on;
031600110628
031700110628         // -?Impostazione opzioni per SQL?
031800110628         exec sql  set  option  DynUsrPrf = *Owner,
031900110628                                CloSqlCsr = *EndMod;
032000110628
032100110628         // -?Reperimento dati job?
032200110628         exsr  sr_DatiJob;
032300110628
032400110628         // -?Impostazione campi "fissi"?
032500110628         k_TBLkut = 1;
032600110628
032700110628         // -?Reperimento data odierna in formato aaaammgg?
032800110628         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
032900110628         //wData_Iso = %date(wDate : *iso);
033000110628
033100110628         // -?Impostazione eventuali parametri ricevuti?
033200110628         if  SDSprm > *zero  and
033300110628             KPJBU <> *blank;
033400110628           TNSS07ds = KPJBU;
033500110628         else;
033600110628           reset  TNSS07ds;
033700110628         endif;
033800110628
033900110628         // -?Pulizia work-files del lavoro sottomesso e loro apertura?
034000110628         Qcmd = c_Cmd_ClrWF1;
034100110628         exsr  sr_ExecCmd;
034200110628         if  Qusei <> *blank;
034300110628           exsr  sr_PrintErr;
034400110628         endif;
034500110628         open  WFES210F;
034600110628         Qcmd = c_Cmd_ClrWF2;
034700110628         exsr  sr_ExecCmd;
034800110628         if  Qusei <> *blank;
034900110628           exsr  sr_PrintErr;
035000110628         endif;
035100110628         open  WFES220F;
035200110628
035300110628         clear  xx;
035400110628         clear  $Cli;
035500110628         clear  $Padre;
035600110628         clear  $StpComodato;
035700110628
035800110628         // -?Intabellamento clienti e relativa famiglia da "3C"?
035900110628         k_TBLcod = '3C';
036000110628         setll  %kds(k03tabel00 : 2)  TABEL;
036100110628         reade  %kds(k03tabel00 : 2)  TABEL;
036200110628         DoW  Not  %eof(TABEL00F);
036300110628           if  TBLflg = *blank;
036400110628             xx += 1;
036500110628             ds3C       = TBLuni;
036600110628             $Cli(xx)   = %trimr( TBLkey );
036700110628             $Padre(xx) = %editc( ds3C.§3Ccks : 'X' );
036800110628           endif;
036900110628         reade  %kds(k03tabel00 : 2)  TABEL;
037000110628         EndDo;
037100110628
037200110628         // -?Intabellamento stampanti in comodato per cliente da "3R"?
037300110628         k_TBLcod = '3R';
037400110628         For  yy = 1  To  xx;
037500110628           k_TBLkey = $Cli(yy);
037600110628           chain  %kds(k03tabel00)  TABEL;
037700110628           if  %found(TABEL00F)   and   TBLflg = *blank;
037800110628             ds3R = TBLuni;
037900110628             $StpComodato(yy) = ds3R.§3Rnot;
038000110628           endif;
038100110628         EndFor;
038200110628
038300110628       ENDSR;
038400110628
038500110628       //--------------------------------------------------------------
038600110628       //?Reperimento Dati del job (Utente/Operativi).                 ?
038700110628       //--------------------------------------------------------------
038800110628       BEGSR  sr_DatiJob;
038900110628
039000110628         in(E) §AzUte;
039100110628         if NOT %error;
039200110628           in(E) §DatiUte;
039300110628         endif;
039400110628         if  %error  or  RSut = *blank;
039500110628           clear TIBS34ds;
039600110628           tibs34r ( tibs34ds );
039700110628           in §AzUte;
039800110628           in §DatiUte;
039900110628         endif;
040000110628
040100110628       ENDSR;
040200110628
040300110628       //--------------------------------------------------------------
040400110628       //?Apertura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
040500110628       //?  estrazione clienti EasySped.                               ?
040600110628       //--------------------------------------------------------------
040700110628       BEGSR  sr_OpenCursor_1;
040800110628
040900110628         // -?Preparazione stringa SQL?
041000110628         wSql = 'with +
041100110628
041200110628                 Tab_3C as +
041300110628                     ( select TBLkey, TBLuni +
041400110628                         from TABEL00F +
041500110628                        where TBLflg = '' '' +
041600110628                          and TBLcod = ''3C'' +
041700110628                          and substr(TBLuni,  1, 2) > ''00'' +
041800110628                          and substr(TBLuni, 44, 5) in +
041900110628                              (''' + c_SuppCB_ESYSP + ''', +
042000110628                               ''' + c_SuppCB_ESVAL + ''') ), +
042100110628
042200110628                 Tab_3CP as +
042300110628                     ( select TBEke1, TBEke2, TBEuni +
042400110628                         from TNTBE00F +
042500110628                        where TBEatb = '' '' +
042600110628                          and TBEcod = ''3CP'' +
042700110628                          and TBEsif = '' '' +
042800110629                          and TBElin = '' '' ), +
042900110628
043000110628                 Tab_3CE as +
043100110628                     ( select TBEke1, TBEuni +
043200110628                         from TNTBE00F +
043300110628                        where TBEatb = '' '' +
043400110628                          and TBEcod = ''3CE'' +
043500110628                          and TBEke2 = '' '' +
043600110628                          and TBEsif = '' '' +
043700110628                          and TBElin = '' '' ) +
043800110628
043900110630                 select Tab_3C.*, +
044000110630                        case when Tab_3CP.TBEke1 > '' '' +
044100110630                             then Tab_3CP.TBEke1 else '' '' end, +
044200110630                        case when Tab_3CP.TBEke2 > '' '' +
044300110630                             then Tab_3CP.TBEke2 else '' '' end, +
044400110630                        case when Tab_3CP.TBEuni > '' '' +
044500110630                             then Tab_3CP.TBEuni else '' '' end, +
044600110630                        case when Tab_3CE.TBEke1 > '' '' +
044700110630                             then Tab_3CE.TBEke1 else '' '' end, +
044800110630                        case when Tab_3CE.TBEuni > '' '' +
044900110630                             then Tab_3CE.TBEuni else '' '' end  +
045000110628
045100110628                   from Tab_3C left outer join Tab_3CP +
045200110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
045300110628                        substr(Tab_3CP.TBEke1, 1, 7) and +
045400110628                        substr(Tab_3C.TBLuni, 1, 2) = +
045500110628                        substr(Tab_3CP.TBEke2, 1, 2) +
045600110628
045700110628                               left outer join Tab_3CE +
045800110628                     on substr(Tab_3C.TBLkey, 1, 7) = +
045900110630                        substr(Tab_3CE.TBEke1, 1, 7)';
046000110628
046100110628         // -?Dichiarazione cursore?
046200110628         exec sql  prepare S1  from :wSql;
046300110628         exec sql  declare C1  cursor for S1;
046400110628
046500110628         // -?Apertura del cursore?
046600110628         exec sql  open C1;
046700110628
046800110628         if  SQLcode < *zero;
046900110628           $EoF_1 = *on;
047000110628           exsr  sr_PrintErr;
047100110628         endif;
047200110628
047300110628       ENDSR;
047400110628
047500110628       //--------------------------------------------------------------
047600110628       //?Lettura cursore C1 (tab. "3C", "3CP" e "3CE") per            ?
047700110628       //?  estrazione clienti EasySped.                               ?
047800110628       //--------------------------------------------------------------
047900110628       BEGSR  sr_ReadCursor_1;
048000110628
048100110628         clear  ds3C;
048200110630         reset  d3CP;
048300110628         clear  d3CE;
048400110630         reset  wSgnClI;
048500110628         clear  wSQL_ds_3C;
048600110628         clear  wSQL_ds_3CP;
048700110628         clear  wSQL_ds_3CE;
048800110628
048900110628         // -?Lettura cursore?
049000110628         exec sql  fetch next  from C1  into :wSQL_ds_3C,
049100110628                                             :wSQL_ds_3CP,
049200110628                                             :wSQL_ds_3CE;
049300110628
049400110628         select;
049500110628           // -?Fine flusso?
049600110628           when  SQLcode = 100;
049700110628             $EoF_1 = *on;
049800110628           // -?Errore?
049900110628           when  SQLcode < *zero;
050000110628             exsr  sr_PrintErr;
050100110628             $EoF_1 = *on;
050200110628           // -?Elaborazione dati estratti?
050300110628           other;
050400110628             exsr  sr_Elab_1;
050500110628         endsl;
050600110628
050700110628       ENDSR;
050800110628
050900110628       //--------------------------------------------------------------
051000110628       //?Chiusura cursore C1 (tab. "3C", "3CP" e "3CE") per           ?
051100110628       //?  estrazione clienti EasySped.                               ?
051200110628       //--------------------------------------------------------------
051300110628       BEGSR  sr_CloseCursor_1;
051400110628
051500110628         // -?Chiusura del cursore?
051600110628         exec sql  close C1;
051700110628
051800110628       ENDSR;
051900110628
052000110628       //--------------------------------------------------------------
052100110628       //?Elaborazione singolo cliente estratto (EasySped).            ?
052200110628       //--------------------------------------------------------------
052300110628       BEGSR  sr_Elab_1;
052400110628
052500110628         // -?Valorizzazione tab. "3C"?
052600110628         ds3C = wSQL_ds_3C.sqlTBLuni;
052700110628
052800110628         // -?Valorizzazione tab. "3CP"?
052900110628         if  wSQL_ds_3CP.sqlTBEke1 > *zero  and
053000110628             wSQL_ds_3CP.sqlTBEke2 > *zero;
053100110628           Monitor;
053200110628             wSgnClI = %dec( %subst( wSQL_ds_3CP.sqlTBEke2 : 3 : 7 )
053300110628                             : 7 : 0 );
053400110628             On-Error  105;
053500110628               reset  wSgnClI;
053600110628               reset  d3CP;
053700110628           EndMon;
053800110628           if  wSQL_ds_3CP.sqlTBEuni > *zero;
053900110628             d3CP = wSQL_ds_3CP.sqlTBEuni;
054000110628           endif;
054100110628         endif;
054200110628
054300110628         // -?Valorizzazione tab. "3CE"?
054400110628         if  wSQL_ds_3CE.sqlTBEke1 > *blank;
054500110628           d3CE = wSQL_ds_3CE.sqlTBEuni;
054600110628         endif;
054700110628
054800110628         // -?Pulizia iniziale del record da registrare?
054900110628         clear  WFES2100;
055000110628
055100110628
055200110628         // -?Reperimento Distretto ed Area?
055300110628         //  ?(a "rottura" di filiale del cliente unificante)?
055400110628         k_ORGfil = %int( %subst( %editc( ds3C.§3Ccks : 'X' ) : 1 : 3 ) );
055500110628         if  k_ORGfil <> ORGfil;
055600110628           chain  %kds(k01azorg01)  AZORG;
055700110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
055800110628             clear  ORGfl3;
055900110628             clear  ORGcar;
056000110628             clear  ds17;
056100110628             clear  ds05;
056200110628           endif;
056300110628         endif;
056400110628
056500110628         // -?Decodifica distretto?
056600110628         if  ORGfl3 <> SAVcdi;
056700110628           SAVcdi = ORGfl3;
056800110628           clear  ds17;
056900110628           k_TBLkut = 1;
057000110628           k_TBLcod = '17';
057100110628           k_TBLkey = ORGfl3;
057200110628           chain  %kds(k03tabel00)  TABEL;
057300110628           if  %found(TABEL00F)  and  TBLflg = *blank;
057400110628             ds17 = TBLuni;
057500110628           endif;
057600110628         endif;
057700110628
057800110628         // -?Decodifica area?
057900110628         if  ORGcar <> SAVcar;
058000110628           SAVcar = ORGcar;
058100110628           clear  ds05;
058200110628           k_TBLkut = 1;
058300110628           k_TBLcod = '05';
058400110628           k_TBLkey = %editc( ORGcar : 'X' );
058500110628           chain  %kds(k03tabel00)  TABEL;
058600110628           if  %found(TABEL00F)  and  TBLflg = *blank;
058700110628             ds05 = TBLuni;
058800110628           endif;
058900110628         endif;
059000110628
059100110628         // -?Decodifica unificante?
059200110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
059300110628           clear  TIBS69ds;
059400110628           I69sif = knsif;
059500110628           I69kcc = DUTkci;
059600110628           I69kac = ds3C.§3Ccks;
059700110628           tibs69r( tibs69ds :
059800110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
059900110628           if  O69err = *on;
060000110628             clear  CNACOds_Uni;
060100110628           else;
060200110628             CNACOds_Uni = ds_CNACO;
060300110628           endif;
060400110628         EndIf;
060500110628
060600110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
060700110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
060800110628           clear  TIBS69ds;
060900110628           I69sif = knsif;
061000110628           I69kcc = DUTkci;
061100110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
061200110628           tibs69r( tibs69ds :
061300110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
061400110628           if  O69err = *on;
061500110628             clear  ds_CNACO;
061600110628           endif;
061700110628         EndIf;
061800110628
061900110628         // -?Reperimento stampanti in concordato da tab. "3R"?
062000110628         exsr  sr_StampConc;
062100110628
062200110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
062300110628         // -?(reperimento dati da spedizioni associate)?
062400110628         $Eof_4 = *off;
062500110628         exsr  sr_OpenCursor_4;
062600110628
062700110628         DoU  $Eof_4;
062800110628           exsr  sr_ReadCursor_4;
062900110628         EndDo;
063000110628
063100110628         exsr  sr_CloseCursor_4;
063200110628
063300110628         // -?Scrittura del record a totale per cliente?
063400110629         wSupporto = ds3C.§3Ccba;
063500110628         exsr  sr_Wrt_WFES210;
063600110628
063700110628       ENDSR;
063800110628
063900110628       //--------------------------------------------------------------
064000110628       //?Apertura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
064100110628       //?  estrazione clienti EasySpedWeb.                            ?
064200110628       //--------------------------------------------------------------
064300110628       BEGSR  sr_OpenCursor_2;
064400110628
064500110628         // -?Preparazione stringa SQL?
064600110628         wSql = 'with +
064700110628
064800110628                 TIVSS as +
064900110628                     ( select VSSksu, VSSsun, VSSisv +
065000110628                         from TIVSS00F +
065100110628                        where VSSisv = ''EW'' and +
065200110628                             (VSSdsc = 0 or VSSdsc > ' +
065300110628                              %char(wDate) + ') ), +
065400110628
065500110628                 Tab_3EW as +
065600110628                     ( select TBEke1, TBEke2, TBEuni +
065700110628                         from TNTBE00F +
065800110628                        where TBEatb = '' '' and +
065900120814                              TBEcod = ''3EW'' and +
066000120814                              substr(TBEuni, 33, 1) = '' '' ), +
066100110628
066200110628                 Tab_3C as +
066300110628                     ( select TBLkey, TBLuni +
066400110628                         from TABEL00F +
066500110628                        where TBLflg = '' '' and +
066600110628                              TBLcod = ''3C'' and +
066700110628                              substr(TBLuni, 44, 5) = +
066800110630                              ''' + c_SuppCB_ESWEB + ''') +
066900110628
067000110628                 select * +
067100110628
067200110628                   from TIVSS inner join Tab_3EW +
067300110628                     on TIVSS.VSSksu = Tab_3EW.TBEke1  and +
067400110628                        TIVSS.VSSsun = Tab_3EW.TBEke2 +
067500110628
067600110628                              inner join Tab_3C +
067700110628                     on substr(TIVSS.VSSksu, 2, 7) = +
067800110628                        substr(Tab_3C.TBLuni, 79, 7)';
067900110628
068000110628         // -?Dichiarazione cursore?
068100110628         exec sql  prepare S2  from :wSql;
068200110628         exec sql  declare C2  cursor for S2;
068300110628
068400110628         // -?Apertura del cursore?
068500110628         exec sql  open C2;
068600110628
068700110628         if  SQLcode < *zero;
068800110628           $EoF_2 = *on;
068900110628           exsr  sr_PrintErr;
069000110628         endif;
069100110628
069200110628       ENDSR;
069300110628
069400110628       //--------------------------------------------------------------
069500110628       //?Lettura cursore C2 (file TIVSS, tab. "3EW" e tab. "3C") per  ?
069600110628       //?  estrazione clienti EasySpedWeb.                            ?
069700110628       //--------------------------------------------------------------
069800110628       BEGSR  sr_ReadCursor_2;
069900110628
070000110628         clear  d3EW;
070100110628         clear  ds3C;
070200110628         clear  wSQL_ds_TIVSS;
070300110628         clear  wSQL_ds_3EW;
070400110628         clear  wSQL_ds_3C;
070500110628
070600110628         // -?Lettura cursore?
070700110628         exec sql  fetch next  from C2  into :wSQL_ds_TIVSS,
070800110628                                             :wSQL_ds_3EW,
070900110628                                             :wSQL_ds_3C;
071000110628
071100110628         select;
071200110628           // -?Fine flusso?
071300110628           when  SQLcode = 100;
071400110628             $EoF_2 = *on;
071500110628           // -?Errore?
071600110628           when  SQLcode < *zero;
071700110628             exsr  sr_PrintErr;
071800110628             $EoF_2 = *on;
071900110628           // -?Elaborazione dati estratti?
072000110628           other;
072100110628             exsr  sr_Elab_2;
072200110628         endsl;
072300110628
072400110628       ENDSR;
072500110628
072600110628       //--------------------------------------------------------------
072700110628       //?Chiusura cursore C2 (file TIVSS e tab. "3C" e "3EW") per     ?
072800110628       //?  estrazione clienti EasySpedWeb.                            ?
072900110628       //--------------------------------------------------------------
073000110628       BEGSR  sr_CloseCursor_2;
073100110628
073200110628         // -?Chiusura del cursore?
073300110628         exec sql  close C2;
073400110628
073500110628       ENDSR;
073600110628
073700110628       //--------------------------------------------------------------
073800110629       //?Elaborazione singolo cliente estratto (EasySpedWeb).         ?
073900110628       //--------------------------------------------------------------
074000110628       BEGSR  sr_Elab_2;
074100110628
074200110628         d3EW = wSQL_ds_3EW.sqlTBEuni;
074300110628         ds3C = wSQL_ds_3C.sqlTBLuni;
074400110628
074500110630         // -?Pulizia iniziale del record da registrare?
074600110628         clear  WFES2100;
074700110628
074800110628         // -?Reperimento Area?
074900110628         //  ?(a "rottura" di filiale del cliente unificante)?
075000110628         k_ORGfil = %int( %subst( wSQL_ds_TIVSS.sqlVSSksu : 2 : 3 ) );
075100110628         if  k_ORGfil <> ORGfil;
075200110628           chain  %kds(k01azorg01)  AZORG;
075300110628           if  NOT  %found(AZORG01L)  or  ORGfva <> *blank;
075400110629             clear  ORGfl3;
075500110628             clear  ORGcar;
075600110628           endif;
075700110628         endif;
075800110629
075900110629         // -?Decodifica distretto?
076000110629         if  ORGfl3 <> SAVcdi;
076100110629           SAVcdi = ORGfl3;
076200110629           clear  ds17;
076300110629           k_TBLkut = 1;
076400110629           k_TBLcod = '17';
076500110629           k_TBLkey = ORGfl3;
076600110629           chain  %kds(k03tabel00)  TABEL;
076700110629           if  %found(TABEL00F)  and  TBLflg = *blank;
076800110629             ds17 = TBLuni;
076900110629           endif;
077000110629         endif;
077100110628
077200110628         // -?Decodifica area?
077300110628         if  ORGcar <> SAVcar;
077400110628           SAVcar = ORGcar;
077500110628           clear  ds05;
077600110628           k_TBLkut = 1;
077700110628           k_TBLcod = '05';
077800110628           k_TBLkey = %editc( ORGcar : 'X' );
077900110628           chain  %kds(k03tabel00)  TABEL;
078000110628           if  %found(TABEL00F)  and  TBLflg = *blank;
078100110628             ds05 = TBLuni;
078200110628           endif;
078300110628         endif;
078400110628
078500110628         // -?Decodifica unificante?
078600110628         If  CNACOds_Uni.ACOksc <> ds3C.§3Ccks;
078700110628           clear  TIBS69ds;
078800110628           I69sif = knsif;
078900110628           I69kcc = DUTkci;
079000110628           I69kac = ds3C.§3Ccks;
079100110628           tibs69r( tibs69ds :
079200110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
079300110628           if  O69err = *on;
079400110628             clear  CNACOds_Uni;
079500110628           else;
079600110628             CNACOds_Uni = ds_CNACO;
079700110628           endif;
079800110628         EndIf;
079900110628
080000110628         // -?Decodifica cliente per recuperarne il flag "bloccato"?
080100110628         If  ACOksc <> %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
080200110628           clear  TIBS69ds;
080300110628           I69sif = knsif;
080400110628           I69kcc = DUTkci;
080500110628           I69kac = %int( %trimr( wSQL_ds_3C.sqlTBLkey ) );
080600110628           tibs69r( tibs69ds :
080700110628                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
080800110628           if  O69err = *on;
080900110628             clear  ds_CNACO;
081000110628           endif;
081100110628         EndIf;
081200110628
081300110628         // -?Reperimento stampanti in concordato da tab. "3R"?
081400110628         exsr  sr_StampConc;
081500110628
081600110628         // -?Ciclo di elaborazione file TITAS33C x singolo cliente?
081700110628         // -?(reperimento dati da spedizioni associate)?
081800110628         $Eof_4 = *off;
081900110628         exsr  sr_OpenCursor_4;
082000110628
082100110628         DoU  $Eof_4;
082200110628           exsr  sr_ReadCursor_4;
082300110628         EndDo;
082400110628
082500110628         exsr  sr_CloseCursor_4;
082600110628
082700110628         // -?Scrittura del record a totale per cliente?
082800110629         wSupporto = ds3C.§3Ccba;
082900110628         exsr  sr_Wrt_WFES210;
083000110628
083100110628       ENDSR;
083200110628
083300110628       //--------------------------------------------------------------
083400110628       //?Apertura cursore C4 (totale spedizioni nel periodo x cliente)?
083500110628       //--------------------------------------------------------------
083600110628       BEGSR  sr_OpenCursor_4;
083700110628
083800110628         // -?Preparazione stringa SQL?
083900110628         //  ? N.B. ?- Seleziona le LdV del periodo richiesto?
084000110630         //  ?· Seleziona le LdV del cliente in esame da tab. "3C"?
084100110628         //  ?· Seleziona le LdV con la serie del cliente in esame?
084200110628         //  ?· Seleziona le LdV porto franco?
084300110628
084400110630         wSql_4 = 'select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
084500110630                          count(*), sum( TASncl ) +
084600110628                     from TITASP0F +
084700110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
084800110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
084900110628                          %editc( D07dti : 'X' ) + ' and ' +
085000110628                          %editc( D07dtf : 'X' ) +
085100110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
085200110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
085300110628                  ' group by TASccm +
085400110628
085500110628                   UNION +
085600110628
085700110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
085800110630                          count(*), sum( TASncl ) +
085900110628                     from TITAS10F +
086000110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
086100110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
086200110628                          %editc( D07dti : 'X' ) + ' and ' +
086300110628                          %editc( D07dtf : 'X' ) +
086400110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
086500110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
086600110628                  ' group by TASccm +
086700110628
086800110628                   UNION +
086900110628
087000110630                   select TASccm, max( ( TASaas * 10000 ) + TASmgs ), +
087100110630                          count(*), sum( TASncl ) +
087200110628                     from TITAS00F +
087300110630                    where ( TASncd > 0 or TASfns = ''S'' ) +
087400110630                      and ( ( TASaas * 10000 ) + TASmgs ) between ' +
087500110628                          %editc( D07dti : 'X' ) + ' and ' +
087600110628                          %editc( D07dtf : 'X' ) +
087700110628                    ' and TASccm = ' + %trim( wSQL_ds_3C.sqlTBLkey ) +
087800110630                    ' and TASnrs = ' + %editc( ds3C.§3Cnrs : '3' ) +
087900110628                  ' group by TASccm +
088000110628
088100110628                      FOR FETCH ONLY';
088200110628
088300110628         // -?Dichiarazione cursore?
088400110628         exec sql  prepare S4  from :wSql_4;
088500110628         exec sql  declare C4  cursor for S4;
088600110628
088700110628         // -?Apertura del cursore?
088800110628         exec sql  open C4;
088900110628
089000110628         if  SQLcode < *zero;
089100110628           $EoF_4 = *on;
089200110628           exsr  sr_PrintErr;
089300110628         endif;
089400110628
089500110628       ENDSR;
089600110628
089700110628       //--------------------------------------------------------------
089800110628       //?Lettura cursore C4 (totale spedizioni nel periodo x cliente) ?
089900110628       //--------------------------------------------------------------
090000110628       BEGSR  sr_ReadCursor_4;
090100110628
090200110628         clear  wSQL_ds_TITAS;
090300110628
090400110628         // -?Lettura cursore?
090500110628         exec sql  fetch next  from C4  into :wSQL_ds_TITAS;
090600110628
090700110628         select;
090800110628           // -?Fine flusso?
090900110628           when  SQLcode = 100;
091000110628             $EoF_4 = *on;
091100110628           // -?Errore?
091200110628           when  SQLcode < *zero;
091300110628             $EoF_4 = *on;
091400110628             exsr  sr_PrintErr;
091500110628           // -?Elaborazione dati estratti?
091600110628           other;
091700110628             exsr  sr_Elab_4;
091800110628         endsl;
091900110628
092000110628       ENDSR;
092100110628
092200110628       //--------------------------------------------------------------
092300110628       //?Chiusura cursore C4 (totale spedizioni nel periodo x cliente)?
092400110628       //--------------------------------------------------------------
092500110628       BEGSR  sr_CloseCursor_4;
092600110628
092700110628         // -?Chiusura del cursore?
092800110628         exec sql  close C4;
092900110628
093000110628       ENDSR;
093100110628
093200110628       //--------------------------------------------------------------
093300110628       //?Elaborazione totale spedizioni nel periodo x CCM da TITAS33C ?
093400110628       //--------------------------------------------------------------
093500110628       BEGSR  sr_Elab_4;
093600110628
093700110628         // -?Conteggio numero spedizioni?
093800110628         WE21nts += wSQL_ds_TITAS.sqlTOTspe;
093900110628
094000110628         // -?Conteggio numero colli?
094100110628         WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
094200110628
094300110628         // -?Memorizzazione data ULTIMA spedizione?
094400110628         if  wSQL_ds_TITAS.sqlTASdts > WE21dus;
094500110628           WE21dus = wSQL_ds_TITAS.sqlTASdts;
094600110628         endif;
094700110628
094800110628       ENDSR;
094900110628
095000110628       //--------------------------------------------------------------
095100110628       //?Scrittura record in work-file WFES210F.                      ?
095200110628       //--------------------------------------------------------------
095300110628       BEGSR  sr_Wrt_WFES210;
095400110628
095500110628         // -?Dati nel trk del work-file WFES210F?
095600110628         // -?Parametri di lancio?
095700110628         WE21pdti  = D07dti;
095800110628         WE21pdtf  = D07dtf;
095900110628         WE21pcver = D07cver;
096000110628         WE21pcdsc = D07cdsc;
096100110628         // -?Distretto?
096200110628         WE21cdi = ORGfl3;
096300110628         WE21ddi = ds17.§17des;
096400110628         // -?Area?
096500110628         WE21car = ORGcar;
096600110628         WE21dar = ds05.§05des;
096700110628         // -?Cliente unificante?
096800110628         WE21ccu = ds3C.§3Ccks;
096900110628         WE21rsu = CNACOds_Uni.ACOrag;
097000110628         // -?Cliente mittente tab. "3C"?
097100110628         WE21ccm = %int( %triml( wSQL_ds_3C.sqlTBLkey ) );
097200110628         WE21rsm = ds3C.§3Crag;
097300110628         WE21blo = ACOabl;
097400110628         // -?Supporto Cliente => BRT?
097500110628         WE21scb = wSupporto;
097600110628         if  wSupporto = c_SuppCB_ESWEB;
097700110628           // -?Dati dalla tab. "3EW"?
097800110628           WE21EWlnp = d3EW.§3EWlnp;
097900120321           WE21fls   = d3EW.§3EWfls;
098000110628           WE21nrs   = d3EW.§3EWnrs;
098100110628           WE21nsi   = d3EW.§3EWnsi;
098200110628           WE21nsf   = d3EW.§3EWnsf;
098300110628           WE21EWctm = d3EW.§3EWctm;
098400110628           WE21EWmad = d3EW.§3EWmad;
098500110628           WE21EWaew = d3EW.§3EWaew;
098600110628         else;
098700110628           // -?Dati dalla tab. "3C"?
098800120321           WE21fls   = ds3C.§3Cfls;
098900110628           WE21nrs   = ds3C.§3Cnrs;
099000110628           WE21nsi   = wSgnClI;
099100110628           WE21nsf   = d3CP.§3CPal;
099200110628         endif;
099300110630         // -?Totali (già calcolati in subr. "sr_Elab_4")?
099400110628         //WE21nts += wSQL_ds_TITAS.sqlTOTspe;
099500110628         //WE21ntc += wSQL_ds_TITAS.sqlTOTcol;
099600110628         //WE21dus  = wSQL_ds_TITAS.sqlTASdts;
099700110706         // -?Dati EasySped (NON per "EasySped-Validate")?
099800110706         if  wSupporto <> c_SuppCB_ESVAL;
099900110706           WE21edat = d3CE.§3CEedat;
100000110706           WE21ever = d3CE.§3CEever;
100100110706           WE21cver = d3CE.§3CEcver;
100200110706           WE21cdsc = d3CE.§3CEcdsc;
100300110706           WE21typw = d3CE.§3CEtypweb;
100400110706           WE21cvermx = d3CE.§3CEcvermx;
100500110706           WE21cdscmx = d3CE.§3CEcdscmx;
100600110706         endif;
100700110630         // -?EasySpedWeb?
100800110706         select;
100900110706           when  wSupporto = c_SuppCB_ESWEB  or
101000110706                 wSupporto = c_SuppCB_ESVAL;
101100110706             WE21esweb = 'SI';
101200110706           when  WE21ever >= '4.0.3 ';
101300110706             WE21esweb = 'SI';
101400110706           other;
101500110706             WE21esweb = 'NO';
101600110706         endsl;
101700110630         // -?Cappario aggiornato?
101800110630         select;
101900110706           when  wSupporto = c_SuppCB_ESWEB;
102000110706             WE21cverag = 'SI';
102100110706           when  wSupporto = c_SuppCB_ESVAL;
102200110706             WE21cverag = 'NO';
102300110630           when  WE21cver   = *zero;
102400110630             WE21cverag = 'NO';
102500110630           when  WE21cver   = D07cver;
102600110630             WE21cverag = 'SI';
102700110630           when  WE21cverMx = D07cver;
102800110630             WE21cverag = 'SI';
102900120601           when  WE21cver   = (D07cver - 1)  and  WE21cdsc = D07cdsc;
103000120601             WE21cverag = 'SI';
103100110630           other;
103200110630             WE21cverag = 'NO';
103300110630         endsl;
103400120531         // -?Stampanti in comodato?
103500110628         WE21stamco = ds3R.§3Rnot;
103600110628
103700110628         //______________
103800110628         WRITE  WFES2100;
103900110628         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
104000110628
104100110628       ENDSR;
104200110628
104300110628       //--------------------------------------------------------------
104400110628       //?Reperimento stampanti in concordato da tab. "3R"             ?
104500110628       //--------------------------------------------------------------
104600110628       BEGSR  sr_StampConc;
104700110628
104800110628         clear  ds3R;
104900110628
105000110628         // -?1° tentativo con il cliente in esame?
105100110628         clear  yy;
105200110628         yy = %lookup( %trimr( wSQL_ds_3C.sqlTBLkey ) : $Cli );
105300110628
105400110628         // -?2° tentativo con il padre del cliente in esame?
105500110628         If  yy = *zero   or   $StpComodato(yy) = *blank;
105600110628           clear  yy;
105700110628           yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Cli );
105800110628         EndIf;
105900110628
106000110628         // -?3° tentativo con gli altri fratelli?
106100110628         If  yy = *zero   or   $StpComodato(yy) = *blank;
106200110628           clear  yy;
106300110628           DoU  yy = *zero   or   $StpComodato(yy) <> *blank;
106400110628             yy = %lookup( %editc( ds3C.§3Ccks : 'X' ) : $Padre : yy + 1 );
106500110628           EndDo;
106600110628         EndIf;
106700110628
106800110628         // -?Imposta la stampante in ds3R (se reperita)?
106900110628         If  yy > *zero;
107000110628           ds3R.§3Rnot = $StpComodato(yy);
107100110628         EndIf;
107200110628
107300110628       ENDSR;
107400110628
107500110628       //--------------------------------------------------------------
107600110628       //?Apertura cursore C3 (creazione WFES220F con totali x area).  ?
107700110628       //--------------------------------------------------------------
107800110628       BEGSR  sr_OpenCursor_3;
107900110628
108000110628         // -?Dichiarazione cursore?
108100110628         exec sql  declare C3  cursor for
108200110628                   select *
108300110628                     from WFES210F
108400110628                    order by WE21car, WE21scb, WE21ccu, WE21ccm
108500110628                      for read only;
108600110628
108700110628         // -?Apertura cursore?
108800110628         exec sql  open C3;
108900110628
109000110628         clear  WFES2200;
109100110628
109200110628       ENDSR;
109300110628
109400110628       //--------------------------------------------------------------
109500110628       //?Lettura cursore C3 (creazione WFES220F con totali x area).
109600110628       //--------------------------------------------------------------
109700110628       BEGSR  sr_ReadCursor_3;
109800110628
109900110628         // -?Lettura cursore?
110000110628         exec sql  fetch  NEXT  from C3  into :WFES210ds;
110100110628
110200110628         select;
110300110628           when  SQLcode = 100;
110400110628             $EoF_3 = *on;
110500110628           when  SQLcode < *zero;
110600110628             $EoF_3 = *on;
110700110628             exsr  sr_PrintErr;
110800110628           other;
110900110628             exsr  sr_Elab_3;
111000110628         endsl;
111100110628
111200110628       ENDSR;
111300110628
111400110628       //--------------------------------------------------------------
111500110628       //?Creazione work-file WFES220F con totali per area.            ?
111600110628       //--------------------------------------------------------------
111700110628       BEGSR  sr_Elab_3;
111800110628
111900110628         // -?Rottura Area?
112000111122         IF  WFES210ds.WE21car <> WE22car;
112100110628
112200110628           // -?Scrittura record con i totali dell'area precedente?
112300110628           if  WE22car <> *zero;
112400111123             exsr  sr_Wrt_WFES220;
112500110628           endif;
112600110628
112700110628           // -?Impostazione dati nuova area?
112800110628           clear  WFES2200;
112900110628           WE22pdti  = WFES210ds.WE21pdti;
113000110628           WE22pdtf  = WFES210ds.WE21pdtf;
113100110628           WE22pcver = WFES210ds.WE21pcver;
113200110628           WE22pcdsc = WFES210ds.WE21pcdsc;
113300110628           WE22car   = WFES210ds.WE21car;
113400110628           WE22dar   = WFES210ds.WE21dar;
113500110628
113600110628         ENDIF;
113700110628
113800110628
113900110628         // -?Incremento totali dell'area in elaborazione?
114000111122         Select;
114100111122
114200111122           // -? EasySpeb ?
114300111123           When  WFES210ds.WE21scb = c_SuppCB_ESYSP;
114400111122             // -?Num. totale spedizioni nel periodo?
114500111122             WE22esnts  += WFES210ds.WE21nts;
114600111122             // -?Num. totale colli nel periodo?
114700111122             WE22esntc  += WFES210ds.WE21ntc;
114800111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
114900111122             if  WFES210ds.WE21stamco = *blank;
115000111122               WE22esntcn += WFES210ds.WE21ntc;
115100111122             endif;
115200111122             // -?Num. totale clienti padre?
115300111122             if  WFES210ds.WE21ccu <> SAVccu;
115400111122               SAVccu = WFES210ds.WE21ccu;
115500111122               WE22esnksu += 1;
115600111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
115700111122               exsr  sr_Elab_5;
115800111122               if  wNTS = *zero;
115900111122                 WE22esnuns += 1;
116000111122               endif;
116100111122             endif;
116200111122             // -?Num. totale clienti in tab. "3C"?
116300111122             WE22esncli += 1;
116400111123             //  ?(scartati quelli bloccati)?
116500111122             IF  WFES210ds.WE21blo = *blank;
116600111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
116700111122               If  WFES210ds.WE21nts > *zero;
116800111122                 WE22esncls += 1;
116900111122                 // ·?...ma SENZA cappario aggiornato?
117000111122                 if  WFES210ds.WE21cverag = 'NO';
117100111122                   WE22esncca += 1;
117200111122                 endif;
117300111122                 // ·?...ma SENZA EasySpedWeb?
117400111122                 if  WFES210ds.WE21esweb  = 'NO';
117500111122                   WE22esnclw += 1;
117600111122                 endif;
117700111122               EndIf;
117800111122             ENDIF;
117900111122
118000111122           // -? EasySpeb Validate ?
118100111123           When  WFES210ds.WE21scb = c_SuppCB_ESVAL;
118200111122             // -?Num. totale spedizioni nel periodo?
118300111122             WE22evnts  += WFES210ds.WE21nts;
118400111122             // -?Num. totale colli nel periodo?
118500111122             WE22evntc  += WFES210ds.WE21ntc;
118600111122             // -?Num. totale spedizioni e colli senza stampanti in comodato?
118700111122             if  WFES210ds.WE21stamco = *blank;
118800111122               WE22evntcn += WFES210ds.WE21ntc;
118900111122             endif;
119000111122             // -?Num. totale clienti padre?
119100111122             if  WFES210ds.WE21ccu <> SAVccu;
119200111122               SAVccu = WFES210ds.WE21ccu;
119300111122               WE22evnksu += 1;
119400111122               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
119500111122               exsr  sr_Elab_5;
119600111122               if  wNTS = *zero;
119700111122                 WE22evnuns += 1;
119800111122               endif;
119900111122             endif;
120000111122             // -?Num. totale clienti in tab. "3C"?
120100111122             WE22evncli += 1;
120200111123             //  ?(scartati quelli bloccati)?
120300111122             IF  WFES210ds.WE21blo = *blank;
120400111122               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
120500111122               If  WFES210ds.WE21nts > *zero;
120600111122                 WE22evncls += 1;
120700111122               EndIf;
120800111122             ENDIF;
120900111123
121000111123           // -? EasySpeb WEB ?
121100111123           When  WFES210ds.WE21scb = c_SuppCB_ESWEB;
121200111123             // -?Num. totale spedizioni nel periodo?
121300111123             WE22ewnts  += WFES210ds.WE21nts;
121400111123             // -?Num. totale colli nel periodo?
121500111123             WE22ewntc  += WFES210ds.WE21ntc;
121600111123             // -?Num. totale spedizioni e colli senza stampanti in comodato?
121700111123             if  WFES210ds.WE21stamco = *blank;
121800111123               WE22ewntcn += WFES210ds.WE21ntc;
121900111123             endif;
122000111123             // -?Num. totale clienti padre?
122100111123             if  WFES210ds.WE21ccu <> SAVccu;
122200111123               SAVccu = WFES210ds.WE21ccu;
122300111123               WE22ewnksu += 1;
122400111123               // -?"Raggruppamento" clienti SENZA spedizioni nel periodo?
122500111123               exsr  sr_Elab_5;
122600111123               if  wNTS = *zero;
122700111123                 WE22ewnuns += 1;
122800111123               endif;
122900111123             endif;
123000111123             // -?Num. totale clienti in tab. "3C"?
123100111123             WE22ewncli += 1;
123200111123             //  ?(scartati quelli bloccati)?
123300111123             IF  WFES210ds.WE21blo = *blank;
123400111123               // -?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
123500111123               If  WFES210ds.WE21nts > *zero;
123600111123                 WE22ewncls += 1;
123700111123               EndIf;
123800111123             ENDIF;
123900111122
124000111122         EndSl;
124100110628
124200110628       ENDSR;
124300110628
124400110628       //--------------------------------------------------------------
124500110630       //?Chiusura cursore C3 (creazione WFES220F con totali x area).
124600110628       //--------------------------------------------------------------
124700110628       BEGSR  sr_CloseCursor_3;
124800110628
124900110628         // -?Scrittura ultimo record?
125000110628         if  WE22car <> *zero;
125100111123           exsr  sr_Wrt_WFES220;
125200110628         endif;
125300110628
125400110628         // -?Chiusura cursore?
125500110628         exec sql  close C3;
125600110628
125700110628       ENDSR;
125800111123
125900111123       //--------------------------------------------------------------
126000111123       //?Scrittura record in work-file WFES220F.                      ?
126100111123       //--------------------------------------------------------------
126200111123       BEGSR  sr_Wrt_WFES220;
126300111123
126400111123         // -? Totali AREA ?
126500111123         // ·?Num. totale spedizioni nel periodo?
126600111123         WE22tants  = WE22esnts + WE22evnts + WE22ewnts;
126700111123         // ·?Num. totale colli nel periodo?
126800111123         WE22tantc  = WE22esntc + WE22evntc + WE22ewntc;
126900111123         // ·?Num. totale spedizioni e colli senza stampanti in comodato?
127000111123         WE22tantcn = WE22esntcn + WE22evntcn + WE22ewntcn;
127100111123         // ·?Num. totale clienti padre?
127200111123         WE22tanksu = WE22esnksu + WE22evnksu + WE22ewnksu;
127300111123         // ·?"Raggruppamento" clienti SENZA spedizioni nel periodo?
127400111123         WE22tanuns = WE22esnuns + WE22evnuns + WE22ewnuns;
127500111123         // ·?Num. totale clienti in tab. "3C"?
127600111123         WE22tancli = WE22esncli + WE22evncli + WE22ewncli;
127700111123         // ·?Num.tot. clienti in tab. "3C"  CON  spediz. nel periodo?
127800111123         //  ?(scartati quelli bloccati)?
127900111123         WE22tancls = WE22esncls + WE22evncls + WE22ewncls;
128000111123
128100111123         //______________
128200111123         WRITE  WFES2200;
128300111123         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
128400111123
128500111123       ENDSR;
128600110628
128700110628       //--------------------------------------------------------------
128800110628       //?Verifica se padre & figli senza spedizioni nel periodo.      ?
128900110628       //--------------------------------------------------------------
129000110628       BEGSR  sr_Elab_5;
129100110628
129200110628         // -?Dichiarazione cursore?
129300110628         exec sql  declare C5  cursor for
129400110628                    select sum(WE21nts)
129500110628                      from WFES210F
129600110628                     where WE21ccu = :WFES210ds.WE21ccu
129700110628                       for read only;
129800110628
129900110628         // -?Apertura cursore?
130000110628         exec sql  open C5;
130100110628
130200110628         // -?Lettura cursore?
130300110628         exec sql  fetch  NEXT  from C5  into :wNTS;
130400110628
130500110628         select;
130600110628           when  SQLcode = 100;
130700110628             $EoF_5 = *on;
130800110628           when  SQLcode < *zero;
130900110628             $EoF_5 = *on;
131000110628             exsr  sr_PrintErr;
131100110628           other;
131200111122             //if  wNTS = *zero;
131300111122             //  WE22nksuns += 1;
131400111122             //endif;
131500110628         endsl;
131600110628
131700110628         // -?Chiusura cursore?
131800110628         exec sql  close C5;
131900110628
132000110628       ENDSR;
132100110628
132200110628       //--------------------------------------------------------------
132300110628       //?Ultimo aggiornamento (dati di EasySped & relativo flag).     ?
132400110628       //--------------------------------------------------------------
132500110628       BEGSR  sr_Upd_EsySp;
132600110628
132700110628         exsr  sr_OpenCursor_6;
132800110628
132900110628         clear wEsySp_ds;
133000110628         dou  $Eof_6 = *on;
133100110628           exsr  sr_ReadCursor_6;
133200110628         enddo;
133300110628
133400110628         exsr  sr_CloseCursor_6;
133500110628
133600110628       ENDSR;
133700110628
133800110628       //--------------------------------------------------------------
133900110628       //?Apertura cursore C6.                                         ?
134000110628       //--------------------------------------------------------------
134100110628       BEGSR  sr_OpenCursor_6;
134200110628
134300110628         // -?Dichiarazione cursore?
134400110628         exec sql  declare C6 cursor for
134500110628
134600110628                    select WE21ccu, WE21flgu,
134700110628                           WE21edat, WE21ever, WE21cver, WE21cdsc, WE21typw,
134800110628                           WE21cverMx, WE21cdscMx, WE21esweb, WE21cverAg,
134900110628                           rrn(WFES210F)
135000110628                      from WFES210F
135100110628                     order by WE21ccu, WE21cver desc
135200110628
135300110628                       for fetch only;
135400110628
135500110628         // -?Apertura cursore?
135600110628         exec sql  open C6;
135700110628
135800110628       ENDSR;
135900110628
136000110628       //--------------------------------------------------------------
136100110628       //?Chiusura cursore C6.                                         ?
136200110628       //--------------------------------------------------------------
136300110628       BEGSR  sr_CloseCursor_6;
136400110628
136500110628         // -?Chiusura cursore?
136600110628         exec sql  close C6;
136700110628
136800110628       ENDSR;
136900110628
137000110628       //--------------------------------------------------------------
137100110628       //?Lettura cursore C6 (rec. WFES210F senza info EasySped).      ?
137200110628       //--------------------------------------------------------------
137300110628       BEGSR  sr_ReadCursor_6;
137400110628
137500110628         // -?Lettura cursore?
137600110628         exec sql  fetch  NEXT  from C6  into :wSQL_ds_WFES21;
137700110628
137800110628         select;
137900110628           when  SQLcode = 100;
138000110628             $Eof_6 = *on;
138100110628           when  SQLcode < *zero;
138200110628             $Eof_6 = *on;
138300110628           other;
138400110628             exsr  sr_Elab_6;
138500110628         endsl;
138600110628
138700110628       ENDSR;
138800110628
138900110628       //--------------------------------------------------------------
139000110630       //?Aggiornamento rec. WFES210F per info EasySped.               ?
139100110628       //--------------------------------------------------------------
139200110628       BEGSR  sr_Elab_6;
139300110628
139400110628         select;
139500110628
139600110628           // -?Record con la versione: salvo cod. cliente + dati della?
139700110628           //  ?versione?
139800111117           //  ?NO: la versione (anche se meno recente) del 2° cliente?
139900111117           //      ?della famiglia si sovrapporrebbe a quella del 1°...?
140000111117           //when  wSQL_ds_WFES21.sqlWESever <> *blank;
140100111117
140200111117           // -?1° Record con la versione: salvo cod. cliente + dati?
140300111117           //  ?della versione?
140400111117           //  ?Meglio tenere comunque la versione più recente (quella?
140500111117           //  ?nel 1° record letto via vedi SQL)!?
140600111117           when  wSQL_ds_WFES21.sqlWESccu <> ESYWESccu;
140700110628             wEsySp_ds = wSQL_ds_WFES21;
140800110628
140900111117           // -?Record senza la versione: se unificante = al precedente?
141000110628           //  ?aggiorno il record con la versione salvata?
141100110628           when  wSQL_ds_WFES21.sqlWESever = *blank  and
141200110628                 wSQL_ds_WFES21.sqlWESccu  = ESYWESccu;
141300110628             chain  wSQL_ds_WFES21.SQLrrn  WFES2100;
141400110628             if  %found(WFES210F);
141500110628               WE21flgu = 'U';
141600110628               WE21edat = ESYWESedat;
141700110628               WE21ever = ESYWESever;
141800110628               WE21cver = ESYWEScver;
141900110628               WE21typw = ESYWEStypw;
142000110628               WE21cdsc = ESYWEScdsc;
142100110628               WE21cvermx = ESYWEScvermax;
142200110628               WE21cdscmx = ESYWEScdscmax;
142300110628               WE21esweb  = ESYWESesweb;
142400110628               WE21cverag = ESYWEScveragg;
142500110628               //______________
142600110628               update WFES2100;
142700110628               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
142800110628             endif;
142900110628
143000110628         endsl;
143100110628
143200110628       ENDSR;
143300110628
143400110628       //--------------------------------------------------------------
143500110628       //?Stampa segnalazione dell'errore rilevato.                    ?
143600110628       //--------------------------------------------------------------
143700110628       BEGSR  sr_PrintErr;
143800110628
143900110628         // -?Stampa del Dump?
144000110628         Dump(A);
144100110628
144200110628         // -?Stampa del Job-Log?
144300110628         Qcmd = 'DSPJOBLOG job(*) output(*print)';
144400110628         exsr  sr_ExecCmd;
144500110628
144600110628         // -?Chiusura del programma?
144700110628         exsr  sr_RoutEnd;
144800110628
144900110628       ENDSR;
145000110628
145100110628       //--------------------------------------------------------------
145200110628       //?Esecuzione del comando (già impostato).                      ?
145300110628       //--------------------------------------------------------------
145400110628       BEGSR  sr_ExecCmd;
145500110628
145600110628         clear Qcap0100;
145700110628         Qcabcsdh = *off;
145800110628         Qcapa    = *off;
145900110628         Qcacmdss = *off;
146000110628         Qcaerved = *allX'00';
146100110628
146200110628         clear Qusec;
146300110628         Qusbprv  = %size(Qusec);
146400110628
146500110628         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
146600110628                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
146700110628                           0 : 0 : Qusec);
146800110628
146900110628         //if  Qusei <> *blank;
147000110628         //  ...
147100110628         //endif;
147200110628
147300110628       ENDSR;
147400110628
147500110628       //--------------------------------------------------------------
147600110628       //?Operazioni finali.                                           ?
147700110628       //--------------------------------------------------------------
147800110628       BEGSR  sr_RoutEnd;
147900110628
148000110628         // -?Chiusura work-file?
148100110628         close(e)  WFES220F;
148200110628         close(e)  WFES210F;
148300110628
148400110628         // -?Chiusura applicazioni?
148500110628         reset  TIBS69ds;
148600110628         tibs69r( tibs69ds :
148700110628                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
148800110628
148900110628         // -?Uscita del *pgm?
149000110628         return;
149100110628
149200110628       ENDSR;
149300110628
149400110628      /end-free
