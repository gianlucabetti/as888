000100121217       //==============================================================
000200121218       //?Scadenza abilitazione al servizio Strategi "EasySped WEB"    ?
000300121217       //==============================================================
000400121217
000500121217       //--------------------------------------------------------------
000600121217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121217       //--------------------------------------------------------------
000800121217
000900121217     /*PRM dbgview(*source)
001000121217     /*PRM commit(*none)
001100121217     /*END
001200121217
001300121217       //--------------------------------------------------------------
001400121217       //?Specifiche di controllo.                                     ?
001500121217       //--------------------------------------------------------------
001600121217
001700121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800121217     h dftactgrp(*no)
001900121217     h alwnull(*inputonly)
002000121217     h bnddir('UBBNDDIR')
002100121217
002200121217       //--------------------------------------------------------------
002300121217       //?Dichiarazione file.                                          ?
002400121217       //--------------------------------------------------------------
002500121217
002600121217       // -?Dati anagrafici clienti?
002700121217     fCNACO00F  if   e           k disk
002800121220
002900121220       // -?Tabella "3C"?
003000130212     fTABEL00F  if   e           k disk    extfile(wLibTABEL)  usropn
003100121220
003200121220       // -?Tabella "3EW"?
003300121220     fTNTBE01L  Uf   e           k disk    extfile(wLibTNTBE)  usropn
003400121217
003500121217       // -?Abilitazione clienti ai servizi internet?
003600121217     fTIVSS01L  Uf   e           k disk
003700130208
003800130208       // -?File di stampa per eventuale invio e-mail?
003900130208     fqSysPrt   o    f  132        printer usropn  oflind(*inOA)
004000121217
004100121217       // -?File di stampa per eventuale invio e-mail?
004200121218     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOF)
004300121217
004400121217       //--------------------------------------------------------------
004500121217       //?Definizione costanti.                                        ?
004600121217       //--------------------------------------------------------------
004700121217
004800121217       // -?Codice utente conti?
004900121217     d c_Kut           c                   const(1)
005000121220
005100121220       // -?Supporto Cliente=>BRT = EasySpedWEB?
005200121220     d c_EasySpedWEB   c                   const('ESWEB')
005300121217
005400121217       // -?Comandi di override al PrtF?
005500121217     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005600121217     d                                           file(PRTEMAIL) +
005700121217     d                                           pagesize(66 132) +
005800121217     d                                           lpi(6) cpi(10) +
005900121217     d                                           ovrscope(*actgrpdfn) +
006000121217     d                                           ')
006100121217     d c_CmdDltOvr     c                   const('DLTOVR +
006200121217     d                                            file(PRTEMAIL) +
006300121217     d                                            lvl(*actgrpdfn)')
006400121218
006500121218       // -?Costanti per la definizione delle schiere con i nomi?
006600121218       //  ?degli iSeries da elaborare e delle relative librerie?
006700121218     d c_NrSyst        c                   const(2)
006800121218     d c_NrLibr        c                   const(2)
006900121217
007000121217       //--------------------------------------------------------------
007100121217       //?Definizione schiere.                                         ?
007200121217       //--------------------------------------------------------------
007300121217
007400121218       // -?iSeries  &  Librerie con entrambi i file tabelle?
007500121218     d $iSystem        s                   like(currSysNetA)
007600121218     d                                     dim(c_NrSyst)
007700121218     d                                     ctdata   perrcd( 1)
007800121218     d $Libraries      s                   like(ds_Libr)
007900121218     d                                     dim(c_NrSyst)
008000121218     d                                     alt($iSystem)
008100121217
008200121217       //--------------------------------------------------------------
008300121217       //?Definizione aree dati.                                       ?
008400121217       //--------------------------------------------------------------
008500121217
008600121218       // -?Dati utente?
008700121218     d §AzUte        e ds                  extname(AZUTE00F)
008800121218     d                                     dtaara
008900121218     d §DatiUte      e ds                  extname(dDatiUte)
009000121218     d                                     dtaara
009100121217
009200121217       //--------------------------------------------------------------
009300121217       //?Definizione strutture dati.                                  ?
009400121217       //--------------------------------------------------------------
009500121217
009600121217       // -?Parametri in input?
009700121218     d KPJBA         e ds
009800121217
009900121217       // -?Tabelle usate:?
010000121217       // ·?Tab. "3EW" = Dati assegnati alla postazione EasyWEB?
010100121217     d d3EW          e ds                  inz  qualified
010200121217       // ·?Tab. "3C"  = Clienti che inviano bolle partenza?
010300121217     d ds3C          e ds                  inz  qualified
010400121217
010500121217       // -?Tab. "MRA" = Bart-Mailing x FISPE04R?
010600121217     d dMRAdan       e ds                  inz
010700121217     d   §MRAddes    e                     inz('ERRORE - FISPE041R')
010800121217     d   §MRAdreg    e                     inz('COR')
010900121217     d   §MRAdmitt   e                     inz('ced')
011000171006     d   §MRAddest   e                     inz('cedalert@brt.it')
011100121217     d   §MRAdidpro  e                     inz('2')
011200121217     d   §MRAdoutqi  e                     inz('EMAILIN')
011300121217
011400121217       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
011500121217     d TRTCM1ds      e ds                  inz
011600121218         //?·§CM1mitt = Indirizzo e-mail del mittente?
011700121217     d   §CM1mitt    e                     inz('ced')
011800121218         //?·§CM1dst  = Indirizzo e-mail del destinatario?
011900121217     d   §CM1dst     e                     inz('ced@brt.it')
012000121218         //?·§CM1tips = Tipo lettera via e-mail:?
012100121218         // ?"LET" = testo allegato in corpo con logo?
012200121218         //         ?(richiede righe libere iniziali per il logo)?
012300121218         // ?"COR" = testo integrato senza logo?
012400121218         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
012500121217     d   §CM1tips    e                     inz('COR')
012600121218         //?·§CM1po   = Filiale?
012700121217     d   §CM1po      e                     inz('046')
012800121218         //?·§CM1var  = Oggetto e-mail?
012900121217     d   §CM1var     e                     inz('*OBJM*+
013000121217     d                                     Nr MAX clienti estraibili -
013100121217     d                                     dalla s.p. di trasm. anagr-
013200121217     d                                     af. mittenti')
013300121218         //?·§CM1sts  = Stato?
013400121217     d   §CM1sts     e                     inz(*off)
013500121218         //?·§CM1sts  = Id processo?
013600121217     d   §CM1idp     e                     inz('2')
013700121221
013800121221       // -?Dati estratti via SQL (da TNTBE00F - "3EW")?
013900121221     d*// wSQL_dati       ds                  inz  qualified
014000121221     d*//   wSQLatb                           inz  like(TBEatb)
014100121221     d*//   wSQLksu                           inz  like(TBEke1)
014200121221     d*//   wSQLsun                           inz  like(TBEke2)
014300121221     d*//   wSQLuni                           inz  like(TBEuni)
014400121221       // -?Dati estratti via SQL (da TIVSS00F)?
014500121221     d wSQL_dati       ds                  inz  qualified
014600121221     d   wSQLsun                           inz  like(VSSsun)
014700121221     d   wSQLksu                           inz  like(VSSksu)
014800121221     d   wSQLisv                           inz  like(VSSisv)
014900121221     d   wSQLdsc                           inz  like(VSSdsc)
015000121221
015100121221       // -?Dati record principale della tabella "3EW"?
015200121221     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015300121221     d xTNTBE_3EWds  e ds                  inz  extname(TNTBE00F)
015400121221     d                                          prefix(X3EW : 3)
015500121218
015600121218       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
015700121218     d ds_Libr         ds                  inz
015800121218     d   $Libr                       10    dim(c_NrLibr) inz
015900121217
016000121217       // -?Status?
016100121217     d Psds           sds
016200121217     d   SDSpgm          *proc
016300121217     d*//JobName             244    253                                         Job name
016400121217     d*//JobUser             254    263                                         User name
016500121217     d*//JobNumber           264    269s 0                                      Job number
016600121217
016700121217       //--------------------------------------------------------------
016800121217       //?Definizione variabili.                                       ?
016900121217       //--------------------------------------------------------------
017000121217
017100121217       // -?Flags booleani?
017200130208     d $Test           s               n   inz
017300121217     d $EoF            s               n   inz
017400121221     d $ACOblocc       s               n   inz
017500121221     d $3CnoESW        s               n   inz
017600121221     d $no3EW          s               n   inz
017700121221     d $ScadEW         s               n   inz
017800121217
017900121217       // -?Indici di schiera?
018000121217     d xx              s              5  0 inz
018100121217     d yy              s              5  0 inz
018200121217     d ww              s              5  0 inz
018300121218
018400121218       // -?Nome esteso Libreria/File dei 2 file tabelle?
018500121220     d wLibTABEL       s             21a   inz
018600121218     d wLibTNTBE       s             21a   inz
018700121217
018800121217       // -?Campi di comodo?
018900121218     d w_iSystem       s              1  0 inz
019000121218     d w_SisInf        s              3  0 inz
019100121217     d wDate           s              8  0 inz
019200121218     d wErr            s             10i 0 inz
019300121218
019400121218       // -?Campi di stampa?
019500130208     d s_Flags_ds      ds                  inz
019600130208     d   S_ACOblocc    s              1    inz
019700130208     d   S_3CnoESW     s              1    inz
019800130208     d   S_No3EW       s              1    inz
019900130208     d S_Ksu           s                   inz  like(wSQL_dati.wSQLksu)
020000130208     d S_Sun           s                   inz  like(wSQL_dati.wSQLsun)
020100121218     d O_testo         s            132    inz
020200121217
020300121217       // -?Stringa SQL da eseguire?
020400121217     d wSQL            s           2048    inz  varying
020500121217
020600121217       //--------------------------------------------------------------
020700121217       //?Definizione prototipi procedure.                             ?
020800121217       //--------------------------------------------------------------
020900121218
021000121218       // -?Reperimento NETA sistema AS/400 corrente?
021100121218     d currSysNeta     s              8a   inz
021200121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
021300121218
021400121218       // -?Reperimento dati utente?
021500121218     d TIBS34ds      e ds
021600121218      /copy gaitrasrc/srcProtoPR,TIBS34R
021700121217
021800121217       // -?Reperimento legami in tab. "3C"?
021900121217      /copy gaitrasrc/srcProtoPI,UBLEG3C
022000121217      /copy gaitrasrc/srcProtoPR,UBLEG3C
022100121217
022200121217       // -?Verifica "blocco" clienti?
022300121217      /copy gaitrasrc/srcProtoPI,UBCHKABL
022400121217      /copy gaitrasrc/srcProtoPR,UBCHKABL
022500121217
022600121217       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
022700121217     d wRtnEsito       s             10i 0 inz
022800121217      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
022900121217
023000121217       // -?Parametri API QCAPCMD (Process Commands)?
023100121217     d Qcmd            s           2048    inz  varying
023200121217      /copy qSysInc/qRpgleSrc,QCAPCMD
023300121217       // -?API QCAPCMD (Process Commands)?
023400121217      /copy gaitrasrc/srcProtoPR,QCAPCMD
023500121217
023600121217       // -?Parametri gestione errori API.?
023700121217      /copy qsysinc/qrpglesrc,QUSEC
023800121217
023900121217       //--------------------------------------------------------------
024000121217       //?Definizione key-list.                                        ?
024100121217       //--------------------------------------------------------------
024200121217
024300121217       // -?File CNACO00F?
024400121217     d k03cnaco00    e ds                  extname(CNACO00F : *key)
024500121217     d                                     prefix(k_)   inz
024600121220
024700121220       // -?File TABEL00F?
024800121220     d k03tabel00    e ds                  extname(TABEL00F : *key)
024900121220     d                                     prefix(k_)   inz
025000121217
025100121217       // -?File TNTBE01L?
025200121217     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025300121217     d                                     prefix(k_)   inz
025400121220
025500121220       // -?File TIVSS01L?
025600121220     d k02tivss01    e ds                  extname(TIVSS01L : *key)
025700121220     d                                     prefix(k_)   inz
025800121217
025900121217       //--------------------------------------------------------------
026000121217       //?Definizione parametri procedura.                             ?
026100121217       //--------------------------------------------------------------
026200121217
026300121217     c     *Entry        plist
026400121217     c                   parm                    KPJBA
026500121217
026600121217      /free
026700121217
026800121217       //--------------------------------------------------------------
026900121217       //?M A I N - L I N E                                            ?
027000121217       //--------------------------------------------------------------
027100121217
027200121217       // -?Operazioni iniziali?
027300121217       exsr sr_RoutInz;
027400121217
027500121217       // -?Ciclo di reperimento clienti in tab. "3C"?
027600121218       doW  Not $EoF;
027700121217         exsr  sr_ReadCursor;
027800121218       enddo;
027900121217
028000121217       // -?Operazioni finali?
028100121217       exsr sr_RoutEnd;
028200121217
028300121217       //--------------------------------------------------------------
028400121217       //?Operazioni iniziali.                                         ?
028500121217       //--------------------------------------------------------------
028600121217       BEGSR  sr_RoutInz;
028700121217
028800121217         exec sql  set option  dynusrprf = *owner,
028900121217                               closqlcsr = *endmod;
029000121217
029100121217         *inLR = *on;
029200130208
029300130208         // -?Verifica SE richiamato per TEST?
029400130208         $Test = %subst( kpjbu : 1 : 4 ) = 'TEST';
029500121218
029600121218         // -?Verifica del sistema AS/400 corrente?
029700121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
029800121218           $EoF = *on;
029900121218           leavesr;
030000121218         endif;
030100121218
030200121218         // -?Impostazione elenco librerie in cui gestire le tabelle?
030300121218         //  ?(a seconda del sistema in cui si stà lavorando)?
030400121218         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
030500121218         if  w_iSystem = *zero;
030600121218           $EoF = *on;
030700121218           leavesr;
030800121218         endif;
030900121217
031000121217         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
031100121217         wDate = %dec( %date() );
031200121219
031300121219         // -?Reperimento dati job?
031400121219         exsr  sr_DatiJob;
031500121220
031600121221         // -?Apertura file TNTBE01L di sede (update)?
031700121221         w_SisInf = 1;
031800121221         exsr  sr_OpenFileTBE;
031900121221
032000121221         // -?Apertura file TABEL00F di sede (solo input)?
032100121221         wLibTABEL = %trimr( $Libr(1) ) + '/' + 'TABEL00F';
032200121221         open  TABEL00F;
032300121217
032400121217         // -?Reperimento dati tab. "MRA" (dati per invio e-mail)?
032500121221         clear  k05tntbe01;
032600121221         k_TBEcod = 'MRA';
032700121221         k_TBEke1 = SDSpgm;
032800121221         chain(N)  %kds(k05tntbe01)  TNTBE000;
032900121221         if  %found(TNTBE01L);
033000121221           dMRAdan = TBEuni;
033100121221         endif;
033200121221
033300121221         // -?Aggancio dati generali della tabella "3EW"?
033400121221         clear  k05tntbe01;
033500121221         k_TBEke1 = *zero;
033600121221         %subst( k_TBEke1 : %len(k_TBEke1) - 3 + 1 : 3 ) = '3EW';
033700121221         chain(N)  %kds(k05tntbe01)  TNTBE000;
033800121221         if  %found(TNTBE01L);
033900121221           xTNTBE_3EWds = TNTBEds;
034000121221           //dTBE01 = x3EWuni;
034100121221         endif;
034200121217
034300121217         // -?Preparazione stringa SQL?
034400121217         exsr  sr_PrepSQL;
034500121217
034600121217         // -?Apertura cursore SQL?
034700121217         exsr  sr_OpenCursor;
034800130208
034900130208         // -?Stampa 1ª testata?
035000130208         if  $Test;
035100130208           open  qSysPrt;
035200130208           except  PrtTXT;
035300130208           *inOA = *off;
035400130208         endif;
035500121217
035600121217       ENDSR;
035700121219
035800121219       //--------------------------------------------------------------
035900121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
036000121219       //--------------------------------------------------------------
036100121219       BEGSR  sr_DatiJob;
036200121219
036300121219         in(E) §AzUte;
036400121219         if NOT %error;
036500121219           in(E) §DatiUte;
036600121219         endif;
036700121219         if %error or RSut = *blanks;
036800121219           clear TIBS34ds;
036900121219           tibs34r ( tibs34ds );
037000121219           in §AzUte;
037100121219           in §DatiUte;
037200121219         endif;
037300121219
037400121219       ENDSR;
037500121217
037600121217       //--------------------------------------------------------------
037700121217       //?Preparazione stringa SQL                                     ?
037800121217       //--------------------------------------------------------------
037900121217       BEGSR  sr_PrepSQL;
038000121221
038100121221         //wSQL = 'select TBEatb, TBEke1, TBEke2, TBEuni +
038200121221         //          from TNTBE00F +
038300121221         //         where TBEcod = ''3EW'' +
038400121221         //         order by TBEke1, TBEke2 +
038500121221         //           for fetch only';
038600121218
038700121221         wSQL = 'select VSSsun, VSSksu, VSSisv, VSSdsc +
038800121221                   from TIVSS00F +
038900121221                  where VSSisv = ''EW'' +
039000130111                    and VSSdsc > ' + %editc( wDate : 'X' ) + ' +
039100121221                  order by VSSksu, VSSsun +
039200121221                    for fetch only';
039300121217
039400121217       ENDSR;
039500121217
039600121217       //--------------------------------------------------------------
039700121217       //?Apertura cursore.                                            ?
039800121217       //--------------------------------------------------------------
039900121217       BEGSR  sr_OpenCursor;
040000121217
040100121217         // -?Dichiarazione cursore?
040200121217         exec sql   prepare S1   from :wSQL;
040300121217         exec sql   declare C1   insensitive   cursor for S1;
040400121217
040500121217         // -?Apertura del cursore?
040600121217         exec sql   open C1;
040700121217
040800121217         if  SQLcode < *zero;
040900121217           $EoF = *on;
041000121217           wErr = 1;
041100121217           exsr  sr_SendEmail;
041200121218           //exsr  sr_RoutEnd;        (qui NON serve!)?
041300121217         endif;
041400121217
041500121217       ENDSR;
041600121217
041700121217       //--------------------------------------------------------------
041800121217       //?Lettura cursore.                                             ?
041900121217       //--------------------------------------------------------------
042000121217       BEGSR  sr_ReadCursor;
042100121217
042200121217         clear  wSQL_dati;
042300121217
042400121217         exec sql   fetch next   from C1   into :wSQL_dati;
042500121217
042600121217         select;
042700121217
042800121217           when  SQLcode < *zero;
042900121217             $EoF = *on;
043000121217             wErr = 2;
043100121217             exsr  sr_SendEmail;
043200121217
043300121217           when  SQLcode = 100;
043400121217             $EoF = *on;
043500121217
043600121217           other;
043700121220             exsr  sr_ElabUnif;
043800121217
043900121217         endsl;
044000121217
044100121217       ENDSR;
044200121217
044300121217       //--------------------------------------------------------------
044400121217       //?Chiusura cursore.                                            ?
044500121217       //--------------------------------------------------------------
044600121217       BEGSR  sr_CloseCursor;
044700121217
044800121217         // -?Chiusura del cursore?
044900121217         exec sql   close C1;
045000121217
045100121217       ENDSR;
045200121217
045300121217       //--------------------------------------------------------------
045400121220       //?Elaborazione singolo unificante da TIVSS x tab. "3EW" e "3C" ?
045500121217       //--------------------------------------------------------------
045600121220       BEGSR  sr_ElabUnif;
045700121218
045800121218         clear  d3EW;
045900121218         clear  ds3C;
046000121221         clear  $ScadEW;
046100121221         clear  $ACOblocc;
046200121221         clear  $3CnoESW;
046300121221         clear  $no3EW;
046400121221         //clear  $DisabVSS;
046500121221
046600121221         // -?Reimpostazione TNTBE di sede in "prima lettura"?
046700121221         w_SisInf = 1;
046800121221         exsr  sr_OpenFileTBE;
046900121217
047000121220         // -?Reperimento flag "Blocco" cliente unificante?
047100121217         k_ACOkut = c_Kut;
047200121220         k_ACOkcc = DUTkci;
047300121220         k_ACOksc = %int( %trim( wSQL_dati.wSQLksu ) );
047400121220         chain  %kds(k03cnaco00)  CNACO000;
047500121220
047600130121         // -?Reperimento tabella "3C" per cliente unificante?
047700121220         clear  k03tabel00;
047800121220         k_TBLkut = c_Kut;
047900121220         k_TBLcod = '3C';
048000121221         k_TBLkey = %subst( wSQL_dati.wSQLksu : 2 );
048100121221         chain  %kds(k03tabel00)  TABEL;
048200121220         if  %found(TABEL00F)  and  TBLflg = *blank;
048300121220           ds3C = TBLuni;
048400121220         endif;
048500121221
048600130121         // -?Reperimento tabella "3EW"?
048700121221         clear  k05tntbe01;
048800121221         k_TBEcod = '3EW';
048900121221         k_TBEke1 = wSQL_dati.wSQLksu;
049000121221         k_TBEke2 = wSQL_dati.wSQLsun;
049100121221         chain(n)  %kds(k05tntbe01)  TNTBE000;
049200121221         if  %found(TNTBE01L)  and  TBEatb = *blank;
049300121221           d3EW = TBEuni;
049400121221         endif;
049500121220
049600121221         // -?Reperimento abilitazione ad EasySpedWEB su Strategi?
049700121221         clear  k02tivss01;
049800121221         k_VSSsun = wSQL_dati.wSQLsun;
049900121221         k_VSSisv = wSQL_dati.wSQLisv;
050000121221         chain(n)  %kds(k02tivss01)  TIVSS000;
050100121221
050200121221
050300121221         // -?Cliente BLOCCATO in anagrafica?
050400121221         $ACOblocc = (Not  %found(CNACO00F)  or
050500121221                      %found(CNACO00F)  and  ACOabl <> *blank);
050600121221         // -?Clienti NON più EasySpedWEB in tab. "3C"?
050700121221         $3CnoESW  = (Not  %found(TABEL00F)  or
050800121221                      %found(TABEL00F)  and  (TBLflg <> *blank  or
050900121221                                             ds3C.§3Ccba <> c_EasySpedWEB));
051000121221         // -?Cliente non più in tab. "3EW" (pur essendo ancora?
051100121221         //  ?abilitato) - ????? (impossibile!!!)?
051200121221         $no3EW    = (Not  %found(TNTBE01L)  or
051300121221                      %found(TNTBE01L)  and  (TBEatb <> *blank  or
051400121221                                             d3EW.§3EWfaa <> *blank));
051500121221
051600121221         // -?Cliente NON più abilitato ad EasySpedWEB su Strategi?
051700121221         //  ?ma ancora in tab. "3C" e "3EW"?
051800121221         //  ?(IMPOSSIBILE vista l'estrazione sql)?
051900121221         //$DisabVSS = (%found(TIVSS01L)  and  VSSdsc <= wDate
052000121221         //                               and
052100121221         //             %found(TABEL00F)  and  TBLflg = *blank  and
052200121221         //                                    ds3C.§3Ccba = c_EasySpedWEB
052300121221         //                               and
052400121221         //             %found(TNTBE01L)  and  TBEatb = *blank  and
052500121221         //                                    d3EW.§3EWfaa = *blank;
052600121221
052700121218
052800121221         // -?Cliente "a posto"?
052900121221         if  Not $ACOblocc  and
053000121221             Not $3CnoESW   and
053100121221             Not $no3EW;
053200121218           leavesr;
053300121218         endif;
053400121217
053500121218         // -?Reperimento dei clienti bloccati nella famiglia?
053600121218         exsr  sr_ChkABL;
053700121218         if  Not $ScadEW;
053800121218           leavesr;
053900121218         endif;
054000121218
054100121218         // -?Scadenza servizio EasySped-WEB di Strategi.?                 ?
054200121218         exsr  sr_ScadEW_VSS_3EW;
054300121217
054400121217       ENDSR;
054500121218
054600121218       //--------------------------------------------------------------
054700121218       //?Reperimento flag di abilitazione per ciascun cliente "ESWEB" ?
054800121218       //?  nella famiglia.                                            ?
054900121218       //--------------------------------------------------------------
055000121218       BEGSR  sr_ChkABL;
055100121218
055200121218         //$ScadEW = *off;         (già fatto)?
055300121218
055400121218         // -?Reperimento famiglia completa del cliente in esame?
055500121218         clear  ubLeg3C_FlgPF;
055600121218         clear  ubLeg3C_Padre;
055700121218         clear  ubLeg3C_SKC;
055800121218         clear  ubLeg3C_SKEW;
055900121218
056000121218         If  wSQL_dati.wSQLksu > *zero;
056100121218
056200121218           if  ubLeg3C_Rtv ( %int( %trim( wSQL_dati.wSQLksu ) ) :
056300121218                             ubLeg3C_FlgPF :
056400121218                             ubLeg3C_Padre :
056500121218                             ubLeg3C_SKC :
056600121218                             ubLeg3C_SKEW ) < *zero;
056700121220             if  sch_SKC(1) = *zero;
056800121220               sch_SKC(1) = %int( %trim( wSQL_dati.wSQLksu ) );
056900121220             endif;
057000121218           endif;
057100121218
057200121218         EndIf;
057300121218
057400121218         // -?Impostazione schiera dei soli clienti della famiglia che?
057500121218         //  ?usano EasySped WEB?
057600121218         clear  yy;
057700121218         clear  ww;
057800121218         clear  sk_KSC;
057900121218         clear  sk_ABL;
058000121218
058100121218         For  yy = 1  To  %elem(sch_SKEW);
058200121218
058300121218           select;
058400121218
058500121218             when  sch_SKC(yy)  = *zero;
058600121218               leave;
058700121218
058800121218             when  sch_SKEW(yy) = *on;
058900121218               ww += 1;
059000121218               sk_KSC(ww) = sch_SKC(yy);
059100121218
059200121218           endsl;
059300121218
059400121218         EndFor;
059500121218
059600121218         // -?Reperimento dell'eventuale blocco di ciascuno dei clienti?
059700121218         //  ?qui sopra intabellati?
059800121218         if  ww > 1;
059900121218           ubChkABL_Chk ( ubChkABL_Ksc : ubChkABL_Abl );
060000121218         else;
060100121218           sk_ABL(1) = ACOabl;
060200121218         endif;
060300121218
060400121218         // -?Verifica se abilitazione ad EasySped-WEB da far scadere?
060500121218         $ScadEW = *on;
060600121218         For  yy = 1  To  ww;
060700121218
060800121218           select;
060900121218
061000121218             when  sk_KSC(yy) = *zero;
061100121218               leave;
061200121218
061300121218             // ·?? = Cliente NON trovato in anagrafica?
061400121218             // ·?A = Cliente annullato in anagrafica?
061500121218             // ·?8/* = Cliente bloccato?
061600121218             when  sk_ABL(yy) = *blank;
061700121218               $ScadEW = *off;
061800121218               leave;
061900121218
062000121218           endsl;
062100121218
062200121218         EndFor;
062300121218
062400121218       ENDSR;
062500121217
062600121217       //--------------------------------------------------------------
062700121217       //?Scadenza servizio EasySped-WEB di Strategi.                  ?
062800121217       //--------------------------------------------------------------
062900121218       BEGSR  sr_ScadEW_VSS_3EW;
063000121217
063100121218         // -?Forzatura scadenza servizio EasySped-WEB di Strategi?
063200130208         IF  wSQL_dati.wSQLdsc > wDate;
063300121221
063400130208           If  Not $Test;
063500130208
063600130208             clear  k02tivss01;
063700130208             k_VSSsun = wSQL_dati.wSQLsun;
063800130208             k_VSSisv = 'EW';
063900130208             chain  %kds(k02tivss01)  TIVSS000;
064000130208             if  %found(TIVSS01L)  and  VSSdsc > wDate;
064100130208               VSSdsc = wDate;
064200130208               //_______________
064300130208               update  TIVSS000;
064400130208               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064500130208             else;
064600130208               unlock  TIVSS01L;
064700130208             endif;
064800130208
064900130208           Else;
065000130208
065100130208             if  *inOA;
065200130208               except  PrtTXT;
065300130208               *inOA = *off;
065400130208             endif;
065500130208             clear  s_Flags_ds;
065600130208             s_Ksu = wSQL_dati.wSQLksu;
065700130208             s_Sun = wSQL_dati.wSQLsun;
065800130208             if  $ACOblocc;
065900130208               s_ACOblocc = 'S';
066000130208             endif;
066100130208             if  $3CnoESW;
066200130208               s_3CnoESW  = 'N';
066300130208             endif;
066400130208             if  $no3EW;
066500130208               s_No3EW    = 'N';
066600130208             endif;
066700130208             except  PrtVSS;
066800130208
066900130208           EndIf;
067000121221
067100130208         ENDIF;
067200121218
067300121221         // -?Forzatura annullamento applicativo in tab. "3EW"?
067400121221         IF  %found(TNTBE01L)  and  TBEatb = *blank
067500121221                               and  d3EW.§3EWfaa = *blank;
067600130208
067700130208           If  Not $Test;
067800121221
067900130208             clear  k05tntbe01;
068000130208             k_TBEcod = '3EW';
068100130208             k_TBEke1 = wSQL_dati.wSQLksu;
068200130208             k_TBEke2 = wSQL_dati.wSQLsun;
068300121221
068400130208             For  w_SisInf = 1  To  c_NrLibr;
068500130208
068600130208               exsr  sr_OpenFileTBE;
068700130208
068800130208               chain  %kds(k05tntbe01)  TNTBE000;
068900130208
069000130208               if  %found(TNTBE01L)  and  TBEatb = *blank;
069100130208                 d3EW = TBEuni;
069200130208                 if  d3EW.§3EWfaa = *blank;
069300130208                   d3EW.§3EWfaa = 'A';
069400130208                   d3EW.§3EWdaa = %editc( wDate : 'X' );
069500130208                   d3EW.§3EWuum = DUTute;
069600130208                   TBEuni = d3EW;
069700130208                   TBEapl = X3EWapl;
069800130208                   TBEftt = 'S';
069900130208                   clear  TBEflt;
070000130208                   if  w_SisInf = 1;
070100130208                     TBEftr = 'T';
070200130208                   else;
070300130208                     TBEftr = 'R';
070400130208                   endif;
070500130208                   TBEdtr = wDate;
070600130208                   //_______________
070700130208                   update  TNTBE000;
070800130208                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
070900130208                 else;
071000130208                   unlock  TNTBE01L;
071100130208                 endif;
071200130208               else;
071300130208                 unlock  TNTBE01L;
071400130208               endif;
071500130208
071600130208             EndFor;
071700121218
071800130208             // -?Aggiornamento flag di dato variato (§3EWUPD) in tab."3EW"?
071900130208             wRtnEsito = Ub§3EWupd_Upd ( wSQL_dati.wSQLksu :
072000130208                                         wSQL_dati.wSQLsun :
072100130208                                         'A' : *off );
072200130208             if wRtnEsito < *zero  and  wRtnEsito <> -4;
072300130208               //$EoF = *on;
072400130208               wErr = wRtnEsito;
072500130208               exsr  sr_SendEmail;
072600130208               //exsr  sr_RoutEnd;
072700130208             endif;
072800130208
072900130208           Else;
073000130208
073100130208             if  *inOA;
073200130208               except  PrtTXT;
073300130208               *inOA = *off;
073400130208             endif;
073500130208             clear  s_Flags_ds;
073600130208             s_Ksu = wSQL_dati.wSQLksu;
073700130208             s_Sun = wSQL_dati.wSQLsun;
073800130208             if  $ACOblocc;
073900130208               s_ACOblocc = 'S';
074000130208             endif;
074100130208             if  $3CnoESW;
074200130208               s_3CnoESW  = 'N';
074300130208             endif;
074400130208             if  $no3EW;
074500130208               s_No3EW    = 'N';
074600130208             endif;
074700130208             except  Prt3EW;
074800130208
074900130208           EndIF;
075000130208
075100130208         ENDIF;
075200121217
075300121217       ENDSR;
075400121218
075500121218       //--------------------------------------------------------------
075600121218       //?Apertura dei files tabelle nel sistema informativo impostato.?
075700121218       //--------------------------------------------------------------
075800121221       BEGSR  sr_OpenFileTBE;
075900121221
076000121221         ds_Libr   = $Libraries(w_iSystem);
076100121221         if  %subst(wLibTNTBE : 1 : 10) = $Libr(w_SisInf);
076200121221           leavesr;
076300121221         endif;
076400121218
076500121218         // -?Chiusura (eventuale) file tabelle?
076600121218         if  %open(TNTBE01L);
076700121218           close  TNTBE01L;
076800121218         endif;
076900121218
077000121218         // -?Apertura file tabelle?
077100121218         wLibTNTBE = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
077200121218         open  TNTBE01L;
077300121218
077400121218       ENDSR;
077500121217
077600121217       //--------------------------------------------------------------
077700121217       //?Invio e-mail di avviso errore.                               ?
077800121217       //--------------------------------------------------------------
077900121217       BEGSR  sr_SendEmail;
078000121217
078100121217         // -?Override al file di stampa ed apertura dello stesso?
078200121217         if  NOT  %open(PrtEMAIL);
078300121217           §CM1mitt = %trim(§MRAdmitt);
078400121217           §CM1dst  = %trim(§MRAddest);
078500121217           §CM1tips = §MRAdreg;
078600121217           §CM1var  = '*OBJM*' + §MRAddes;
078700121217           §CM1idp  = §MRAdidpro;
078800121217           Qcmd = c_CmdOvrPrtF
078900121217                + ' outq(' + %trim(§MRAdoutqi) + ')'
079000121217                + ' usrdfndta(''' + TRTCM1ds + ''')';
079100121217           exsr  sr_ExecCmd;
079200121217           //*inH1 = (Qusei <> *blank);
079300121217           open  PrtEMAIL;
079400121217         endif;
079500121217
079600121217         // -?Stampa del messaggio di errore?
079700121217         select;
079800121217           when  wErr = 1;
079900121217             O_testo = 'Errore nell''apertura del cursore SQL.';
080000121217           when  wErr = 2;
080100121217             O_testo = 'Errore nella lettura del cursore SQL.';
080200121217           when  wRtnEsito = -1;
080300121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
080400121217                        rilevato parametri errati per il cliente ' +
080500121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
080600121218                        %trimr(wSQL_dati.wSQLsun);
080700121217           when  wRtnEsito = -2;
080800121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
080900121217                        rilevato errori nel reperimento del nome +
081000121217                        del sistema per il cliente ' +
081100121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
081200121218                        %trimr(wSQL_dati.wSQLsun);
081300121217           when  wRtnEsito = -3;
081400121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
081500121217                        rilevato errori nell''apertura del file +
081600121217                        tabelle (tab. "3EW") per il cliente ' +
081700121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
081800121218                        %trimr(wSQL_dati.wSQLsun);
081900121217           when  wRtnEsito = -4;
082000130605             O_testo = 'Il programma di servizio UB§3EWUPD non ha +
082100130605                        trovato il cliente ' +
082200130605                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
082300130605                        %trimr(wSQL_dati.wSQLsun) +
082400130605                       ' in tab. "3EW".';
082500130605           when  wRtnEsito = -5;
082600121217             O_testo = 'Fallito aggiornamento del flag "Modificato" +
082700121217                        (§3EWUPD) in tab. "3EW" per il cliente ' +
082800121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
082900121218                        %trimr(wSQL_dati.wSQLsun);
083000121217           when  wRtnEsito = -6;
083100121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
083200121217                        rilevato errori nella chiusura del file +
083300121217                        tabelle (tab. "3EW"), key ' +
083400121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
083500121218                        %trimr(wSQL_dati.wSQLsun);
083600130605           when  wRtnEsito = -7;
083700130807             O_testo = 'Fallito aggiornam. del flag "Modificato" +
083800130807                        (§3EWUPD) in tab. "3EW" nel SIST.+
083900130807                        INFORM. di FIL. per il cliente ' +
084000130605                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
084100130605                        %trimr(wSQL_dati.wSQLsun);
084200121217         endsl;
084300121217         if  O_Testo <> *blank;
084400121217           except  PrtDet;
084500121217         endif;
084600121217
084700121217         // -?Chiusura del file di stampa e cancellazione override?
084800121218         //close  PrtEMAIL;        ?(conviene farlo a fine *pgm)?
084900121218         //Qcmd = c_CmdDltOvr;     ?(conviene farlo a fine *pgm)?
085000121218         //exsr  sr_ExecCmd;       ?(conviene farlo a fine *pgm)?
085100121217
085200121217       ENDSR;
085300121217
085400121217       //--------------------------------------------------------------
085500121217       //?Operazioni finali.                                           ?
085600121217       //--------------------------------------------------------------
085700121217       BEGSR  sr_RoutEnd;
085800130208
085900130208         // -?Stampa "Fine Lista"?
086000130208         if  $Test;
086100130208           except  PrtEND;
086200130208           close  qSysPrt;
086300130208         endif;
086400121217
086500121217         // -?Chiusura applicazioni precedentemente aperte?
086600121217         ubLeg3C_Close ();
086700121217         ubChkABL_Close ();
086800121217
086900121217         // -?Chiusura cursore SQL?
087000121217         exsr  sr_CloseCursor;
087100121218
087200121218         // -?Chiusura archivi?
087300121218         if  %open(TNTBE01L);
087400121218           close  TNTBE01L;
087500121218         endif;
087600121220         if  %open(TABEL00F);
087700121220           close  TABEL00F;
087800121220         endif;
087900121218
088000121218         // -?Chiusura del file di stampa e cancellazione override?
088100121218         if  %open(PrtEMAIL);
088200121218           close  PrtEMAIL;
088300121218           Qcmd = c_CmdDltOvr;
088400121218           exsr  sr_ExecCmd;
088500121218         endif;
088600121217
088700121217         // -?Chiusura pgm?
088800121217         return;
088900121217
089000121217       ENDSR;
089100121218
089200121218       //--------------------------------------------------------------
089300121218       //?Esecuzione del comando (già impostato).                      ?
089400121218       //--------------------------------------------------------------
089500121218       BEGSR  sr_ExecCmd;
089600121218
089700121218         clear Qcap0100;
089800121218         Qcabcsdh = *off;
089900121218         Qcapa    = *off;
090000121218         Qcacmdss = *off;
090100121218         Qcaerved = *allX'00';
090200121218
090300121218         clear Qusec;
090400121218         Qusbprv  = %size(Qusec);
090500121218
090600121218         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
090700121218                           %size(Qcap0100) : 'CPOP0100' : *omit :
090800121218                           0 : 0 : Qusec);
090900121218
091000121218         //if  Qusei <> *blank;
091100121218         //  ...;
091200121218         //endif;
091300121218
091400121218       ENDSR;
091500121217
091600121217      /end-free
091700130208
091800130208       //--------------------------------------------------------------
091900130208       //?Spool di stampa (alternativo all'aggiornamento).             ?
092000130208       //--------------------------------------------------------------
092100130208
092200130208     oqSysPrt   e            PrtTXT            2
092300130208     o                       RsUt
092400130208     o                                         +  3 'DISABILITAZIONE C-
092500130208     o                                              LIENTI AL SERVIZIO-
092600130208     o                                               STRATEGI'
092700130208     o                       SDSpgm            + 10
092800130208     o                       *date         y   +  5
092900130208     o                                         +  5 'Pag.'
093000130208     o                       Page1         z   +  0
093100130208     o          e            PrtTXT      0  0
093200130208     o                                         + 23 'DISABILITAZIONE C-
093300130208     o                                              LIENTI AL SERVIZIO-
093400130208     o                                               STRATEGI'
093500130208     o          e            PrtTXT      2
093600130208     o                                              'Unific. '
093700130208     o                                         +  2 '   S.U.N.'
093800130208     o                                         +  2 'ABL'
093900130208     o                                         +  2 'ESW'
094000130208     o                                         +  2 '3EW'
094100130208     o                                         +  2 'Segnalazione'
094200130208     o          e            PrtTXT      1
094300130208     o                                              '========'
094400130208     o                                         +  2 '========='
094500130208     o                                         +  2 '==='
094600130208     o                                         +  2 '==='
094700130208     o                                         +  2 '==='
094800130208     o                                         +  2 '============'
094900130208      *
095000130208     o          e            PrtVSS      1
095100130208     o                       s_ksu
095200130208     o                       s_sun             +  2
095300130208     o                       s_ACOblocc        +  3
095400130208     o                       s_3CnoESW         +  4
095500130208     o                       s_No3EW           +  4
095600130208     o                                         +  3 'Unificante disabi-
095700130208     o                                              litabile al serviz-
095800130208     o                                              io internet "EW"'
095900130208      *
096000130208     o          e            Prt3EW      1
096100130208     o                       s_ksu
096200130208     o                       s_sun             +  2
096300130208     o                       s_ACOblocc        +  3
096400130208     o                       s_3CnoESW         +  4
096500130208     o                       s_No3EW           +  4
096600130208     o                                         +  3 'Annullamento appl-
096700130208     o                                              icativo forzabile -
096800130208     o                                              in tab. "3EW"'
096900130208      *
097000130208     o          e            PrtEND      2
097100130208     o                                         + 24 '***  Fine Stampa  ***'
097200121217
097300121217       //--------------------------------------------------------------
097400121217       //?Spool di stampa (per e-mail).                                ?
097500121217       //--------------------------------------------------------------
097600121217
097700121217     oPrtEMAIL  e            PRTdet      1
097800121217     o                       O_testo
097900121218
098000121218       //--------------------------------------------------------------
098100121218       //?Definizione schiere a tempo di compilazione                  ?
098200121218       //--------------------------------------------------------------
098300121218
098400121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
098500121218SETRAS  GAITRAGRU FILTRAGRU
098600121218AS888   GAITRAGRPSFILTRAGRPF
