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
011000121217     d   §MRAddest   e                     inz('fabrizio.gurrieri+
011100121217     d                                          @brt.it; +
011200121217     d                                          stefano.merighi+
011300121217     d                                          @brt.it')
011400121217     d   §MRAdidpro  e                     inz('2')
011500121217     d   §MRAdoutqi  e                     inz('EMAILIN')
011600121217
011700121217       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
011800121217     d TRTCM1ds      e ds                  inz
011900121218         //?·§CM1mitt = Indirizzo e-mail del mittente?
012000121217     d   §CM1mitt    e                     inz('ced')
012100121218         //?·§CM1dst  = Indirizzo e-mail del destinatario?
012200121217     d   §CM1dst     e                     inz('ced@brt.it')
012300121218         //?·§CM1tips = Tipo lettera via e-mail:?
012400121218         // ?"LET" = testo allegato in corpo con logo?
012500121218         //         ?(richiede righe libere iniziali per il logo)?
012600121218         // ?"COR" = testo integrato senza logo?
012700121218         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
012800121217     d   §CM1tips    e                     inz('COR')
012900121218         //?·§CM1po   = Filiale?
013000121217     d   §CM1po      e                     inz('046')
013100121218         //?·§CM1var  = Oggetto e-mail?
013200121217     d   §CM1var     e                     inz('*OBJM*+
013300121217     d                                     Nr MAX clienti estraibili -
013400121217     d                                     dalla s.p. di trasm. anagr-
013500121217     d                                     af. mittenti')
013600121218         //?·§CM1sts  = Stato?
013700121217     d   §CM1sts     e                     inz(*off)
013800121218         //?·§CM1sts  = Id processo?
013900121217     d   §CM1idp     e                     inz('2')
014000121221
014100121221       // -?Dati estratti via SQL (da TNTBE00F - "3EW")?
014200121221     d*// wSQL_dati       ds                  inz  qualified
014300121221     d*//   wSQLatb                           inz  like(TBEatb)
014400121221     d*//   wSQLksu                           inz  like(TBEke1)
014500121221     d*//   wSQLsun                           inz  like(TBEke2)
014600121221     d*//   wSQLuni                           inz  like(TBEuni)
014700121221       // -?Dati estratti via SQL (da TIVSS00F)?
014800121221     d wSQL_dati       ds                  inz  qualified
014900121221     d   wSQLsun                           inz  like(VSSsun)
015000121221     d   wSQLksu                           inz  like(VSSksu)
015100121221     d   wSQLisv                           inz  like(VSSisv)
015200121221     d   wSQLdsc                           inz  like(VSSdsc)
015300121221
015400121221       // -?Dati record principale della tabella "3EW"?
015500121221     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015600121221     d xTNTBE_3EWds  e ds                  inz  extname(TNTBE00F)
015700121221     d                                          prefix(X3EW : 3)
015800121218
015900121218       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
016000121218     d ds_Libr         ds                  inz
016100121218     d   $Libr                       10    dim(c_NrLibr) inz
016200121217
016300121217       // -?Status?
016400121217     d Psds           sds
016500121217     d   SDSpgm          *proc
016600121217     d*//JobName             244    253                                         Job name
016700121217     d*//JobUser             254    263                                         User name
016800121217     d*//JobNumber           264    269s 0                                      Job number
016900121217
017000121217       //--------------------------------------------------------------
017100121217       //?Definizione variabili.                                       ?
017200121217       //--------------------------------------------------------------
017300121217
017400121217       // -?Flags booleani?
017500130208     d $Test           s               n   inz
017600121217     d $EoF            s               n   inz
017700121221     d $ACOblocc       s               n   inz
017800121221     d $3CnoESW        s               n   inz
017900121221     d $no3EW          s               n   inz
018000121221     d $ScadEW         s               n   inz
018100121217
018200121217       // -?Indici di schiera?
018300121217     d xx              s              5  0 inz
018400121217     d yy              s              5  0 inz
018500121217     d ww              s              5  0 inz
018600121218
018700121218       // -?Nome esteso Libreria/File dei 2 file tabelle?
018800121220     d wLibTABEL       s             21a   inz
018900121218     d wLibTNTBE       s             21a   inz
019000121217
019100121217       // -?Campi di comodo?
019200121218     d w_iSystem       s              1  0 inz
019300121218     d w_SisInf        s              3  0 inz
019400121217     d wDate           s              8  0 inz
019500121218     d wErr            s             10i 0 inz
019600121218
019700121218       // -?Campi di stampa?
019800130208     d s_Flags_ds      ds                  inz
019900130208     d   S_ACOblocc    s              1    inz
020000130208     d   S_3CnoESW     s              1    inz
020100130208     d   S_No3EW       s              1    inz
020200130208     d S_Ksu           s                   inz  like(wSQL_dati.wSQLksu)
020300130208     d S_Sun           s                   inz  like(wSQL_dati.wSQLsun)
020400121218     d O_testo         s            132    inz
020500121217
020600121217       // -?Stringa SQL da eseguire?
020700121217     d wSQL            s           2048    inz  varying
020800121217
020900121217       //--------------------------------------------------------------
021000121217       //?Definizione prototipi procedure.                             ?
021100121217       //--------------------------------------------------------------
021200121218
021300121218       // -?Reperimento NETA sistema AS/400 corrente?
021400121218     d currSysNeta     s              8a   inz
021500121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
021600121218
021700121218       // -?Reperimento dati utente?
021800121218     d TIBS34ds      e ds
021900121218      /copy gaitrasrc/srcProtoPR,TIBS34R
022000121217
022100121217       // -?Reperimento legami in tab. "3C"?
022200121217      /copy gaitrasrc/srcProtoPI,UBLEG3C
022300121217      /copy gaitrasrc/srcProtoPR,UBLEG3C
022400121217
022500121217       // -?Verifica "blocco" clienti?
022600121217      /copy gaitrasrc/srcProtoPI,UBCHKABL
022700121217      /copy gaitrasrc/srcProtoPR,UBCHKABL
022800121217
022900121217       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
023000121217     d wRtnEsito       s             10i 0 inz
023100121217      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
023200121217
023300121217       // -?Parametri API QCAPCMD (Process Commands)?
023400121217     d Qcmd            s           2048    inz  varying
023500121217      /copy qSysInc/qRpgleSrc,QCAPCMD
023600121217       // -?API QCAPCMD (Process Commands)?
023700121217      /copy gaitrasrc/srcProtoPR,QCAPCMD
023800121217
023900121217       // -?Parametri gestione errori API.?
024000121217      /copy qsysinc/qrpglesrc,QUSEC
024100121217
024200121217       //--------------------------------------------------------------
024300121217       //?Definizione key-list.                                        ?
024400121217       //--------------------------------------------------------------
024500121217
024600121217       // -?File CNACO00F?
024700121217     d k03cnaco00    e ds                  extname(CNACO00F : *key)
024800121217     d                                     prefix(k_)   inz
024900121220
025000121220       // -?File TABEL00F?
025100121220     d k03tabel00    e ds                  extname(TABEL00F : *key)
025200121220     d                                     prefix(k_)   inz
025300121217
025400121217       // -?File TNTBE01L?
025500121217     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025600121217     d                                     prefix(k_)   inz
025700121220
025800121220       // -?File TIVSS01L?
025900121220     d k02tivss01    e ds                  extname(TIVSS01L : *key)
026000121220     d                                     prefix(k_)   inz
026100121217
026200121217       //--------------------------------------------------------------
026300121217       //?Definizione parametri procedura.                             ?
026400121217       //--------------------------------------------------------------
026500121217
026600121217     c     *Entry        plist
026700121217     c                   parm                    KPJBA
026800121217
026900121217      /free
027000121217
027100121217       //--------------------------------------------------------------
027200121217       //?M A I N - L I N E                                            ?
027300121217       //--------------------------------------------------------------
027400121217
027500121217       // -?Operazioni iniziali?
027600121217       exsr sr_RoutInz;
027700121217
027800121217       // -?Ciclo di reperimento clienti in tab. "3C"?
027900121218       doW  Not $EoF;
028000121217         exsr  sr_ReadCursor;
028100121218       enddo;
028200121217
028300121217       // -?Operazioni finali?
028400121217       exsr sr_RoutEnd;
028500121217
028600121217       //--------------------------------------------------------------
028700121217       //?Operazioni iniziali.                                         ?
028800121217       //--------------------------------------------------------------
028900121217       BEGSR  sr_RoutInz;
029000121217
029100121217         exec sql  set option  dynusrprf = *owner,
029200121217                               closqlcsr = *endmod;
029300121217
029400121217         *inLR = *on;
029500130208
029600130208         // -?Verifica SE richiamato per TEST?
029700130208         $Test = %subst( kpjbu : 1 : 4 ) = 'TEST';
029800121218
029900121218         // -?Verifica del sistema AS/400 corrente?
030000121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
030100121218           $EoF = *on;
030200121218           leavesr;
030300121218         endif;
030400121218
030500121218         // -?Impostazione elenco librerie in cui gestire le tabelle?
030600121218         //  ?(a seconda del sistema in cui si stà lavorando)?
030700121218         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
030800121218         if  w_iSystem = *zero;
030900121218           $EoF = *on;
031000121218           leavesr;
031100121218         endif;
031200121217
031300121217         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
031400121217         wDate = %dec( %date() );
031500121219
031600121219         // -?Reperimento dati job?
031700121219         exsr  sr_DatiJob;
031800121220
031900121221         // -?Apertura file TNTBE01L di sede (update)?
032000121221         w_SisInf = 1;
032100121221         exsr  sr_OpenFileTBE;
032200121221
032300121221         // -?Apertura file TABEL00F di sede (solo input)?
032400121221         wLibTABEL = %trimr( $Libr(1) ) + '/' + 'TABEL00F';
032500121221         open  TABEL00F;
032600121217
032700121217         // -?Reperimento dati tab. "MRA" (dati per invio e-mail)?
032800121221         clear  k05tntbe01;
032900121221         k_TBEcod = 'MRA';
033000121221         k_TBEke1 = SDSpgm;
033100121221         chain(N)  %kds(k05tntbe01)  TNTBE000;
033200121221         if  %found(TNTBE01L);
033300121221           dMRAdan = TBEuni;
033400121221         endif;
033500121221
033600121221         // -?Aggancio dati generali della tabella "3EW"?
033700121221         clear  k05tntbe01;
033800121221         k_TBEke1 = *zero;
033900121221         %subst( k_TBEke1 : %len(k_TBEke1) - 3 + 1 : 3 ) = '3EW';
034000121221         chain(N)  %kds(k05tntbe01)  TNTBE000;
034100121221         if  %found(TNTBE01L);
034200121221           xTNTBE_3EWds = TNTBEds;
034300121221           //dTBE01 = x3EWuni;
034400121221         endif;
034500121217
034600121217         // -?Preparazione stringa SQL?
034700121217         exsr  sr_PrepSQL;
034800121217
034900121217         // -?Apertura cursore SQL?
035000121217         exsr  sr_OpenCursor;
035100130208
035200130208         // -?Stampa 1ª testata?
035300130208         if  $Test;
035400130208           open  qSysPrt;
035500130208           except  PrtTXT;
035600130208           *inOA = *off;
035700130208         endif;
035800121217
035900121217       ENDSR;
036000121219
036100121219       //--------------------------------------------------------------
036200121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
036300121219       //--------------------------------------------------------------
036400121219       BEGSR  sr_DatiJob;
036500121219
036600121219         in(E) §AzUte;
036700121219         if NOT %error;
036800121219           in(E) §DatiUte;
036900121219         endif;
037000121219         if %error or RSut = *blanks;
037100121219           clear TIBS34ds;
037200121219           tibs34r ( tibs34ds );
037300121219           in §AzUte;
037400121219           in §DatiUte;
037500121219         endif;
037600121219
037700121219       ENDSR;
037800121217
037900121217       //--------------------------------------------------------------
038000121217       //?Preparazione stringa SQL                                     ?
038100121217       //--------------------------------------------------------------
038200121217       BEGSR  sr_PrepSQL;
038300121221
038400121221         //wSQL = 'select TBEatb, TBEke1, TBEke2, TBEuni +
038500121221         //          from TNTBE00F +
038600121221         //         where TBEcod = ''3EW'' +
038700121221         //         order by TBEke1, TBEke2 +
038800121221         //           for fetch only';
038900121218
039000121221         wSQL = 'select VSSsun, VSSksu, VSSisv, VSSdsc +
039100121221                   from TIVSS00F +
039200121221                  where VSSisv = ''EW'' +
039300130111                    and VSSdsc > ' + %editc( wDate : 'X' ) + ' +
039400121221                  order by VSSksu, VSSsun +
039500121221                    for fetch only';
039600121217
039700121217       ENDSR;
039800121217
039900121217       //--------------------------------------------------------------
040000121217       //?Apertura cursore.                                            ?
040100121217       //--------------------------------------------------------------
040200121217       BEGSR  sr_OpenCursor;
040300121217
040400121217         // -?Dichiarazione cursore?
040500121217         exec sql   prepare S1   from :wSQL;
040600121217         exec sql   declare C1   insensitive   cursor for S1;
040700121217
040800121217         // -?Apertura del cursore?
040900121217         exec sql   open C1;
041000121217
041100121217         if  SQLcode < *zero;
041200121217           $EoF = *on;
041300121217           wErr = 1;
041400121217           exsr  sr_SendEmail;
041500121218           //exsr  sr_RoutEnd;        (qui NON serve!)?
041600121217         endif;
041700121217
041800121217       ENDSR;
041900121217
042000121217       //--------------------------------------------------------------
042100121217       //?Lettura cursore.                                             ?
042200121217       //--------------------------------------------------------------
042300121217       BEGSR  sr_ReadCursor;
042400121217
042500121217         clear  wSQL_dati;
042600121217
042700121217         exec sql   fetch next   from C1   into :wSQL_dati;
042800121217
042900121217         select;
043000121217
043100121217           when  SQLcode < *zero;
043200121217             $EoF = *on;
043300121217             wErr = 2;
043400121217             exsr  sr_SendEmail;
043500121217
043600121217           when  SQLcode = 100;
043700121217             $EoF = *on;
043800121217
043900121217           other;
044000121220             exsr  sr_ElabUnif;
044100121217
044200121217         endsl;
044300121217
044400121217       ENDSR;
044500121217
044600121217       //--------------------------------------------------------------
044700121217       //?Chiusura cursore.                                            ?
044800121217       //--------------------------------------------------------------
044900121217       BEGSR  sr_CloseCursor;
045000121217
045100121217         // -?Chiusura del cursore?
045200121217         exec sql   close C1;
045300121217
045400121217       ENDSR;
045500121217
045600121217       //--------------------------------------------------------------
045700121220       //?Elaborazione singolo unificante da TIVSS x tab. "3EW" e "3C" ?
045800121217       //--------------------------------------------------------------
045900121220       BEGSR  sr_ElabUnif;
046000121218
046100121218         clear  d3EW;
046200121218         clear  ds3C;
046300121221         clear  $ScadEW;
046400121221         clear  $ACOblocc;
046500121221         clear  $3CnoESW;
046600121221         clear  $no3EW;
046700121221         //clear  $DisabVSS;
046800121221
046900121221         // -?Reimpostazione TNTBE di sede in "prima lettura"?
047000121221         w_SisInf = 1;
047100121221         exsr  sr_OpenFileTBE;
047200121217
047300121220         // -?Reperimento flag "Blocco" cliente unificante?
047400121217         k_ACOkut = c_Kut;
047500121220         k_ACOkcc = DUTkci;
047600121220         k_ACOksc = %int( %trim( wSQL_dati.wSQLksu ) );
047700121220         chain  %kds(k03cnaco00)  CNACO000;
047800121220
047900130121         // -?Reperimento tabella "3C" per cliente unificante?
048000121220         clear  k03tabel00;
048100121220         k_TBLkut = c_Kut;
048200121220         k_TBLcod = '3C';
048300121221         k_TBLkey = %subst( wSQL_dati.wSQLksu : 2 );
048400121221         chain  %kds(k03tabel00)  TABEL;
048500121220         if  %found(TABEL00F)  and  TBLflg = *blank;
048600121220           ds3C = TBLuni;
048700121220         endif;
048800121221
048900130121         // -?Reperimento tabella "3EW"?
049000121221         clear  k05tntbe01;
049100121221         k_TBEcod = '3EW';
049200121221         k_TBEke1 = wSQL_dati.wSQLksu;
049300121221         k_TBEke2 = wSQL_dati.wSQLsun;
049400121221         chain(n)  %kds(k05tntbe01)  TNTBE000;
049500121221         if  %found(TNTBE01L)  and  TBEatb = *blank;
049600121221           d3EW = TBEuni;
049700121221         endif;
049800121220
049900121221         // -?Reperimento abilitazione ad EasySpedWEB su Strategi?
050000121221         clear  k02tivss01;
050100121221         k_VSSsun = wSQL_dati.wSQLsun;
050200121221         k_VSSisv = wSQL_dati.wSQLisv;
050300121221         chain(n)  %kds(k02tivss01)  TIVSS000;
050400121221
050500121221
050600121221         // -?Cliente BLOCCATO in anagrafica?
050700121221         $ACOblocc = (Not  %found(CNACO00F)  or
050800121221                      %found(CNACO00F)  and  ACOabl <> *blank);
050900121221         // -?Clienti NON più EasySpedWEB in tab. "3C"?
051000121221         $3CnoESW  = (Not  %found(TABEL00F)  or
051100121221                      %found(TABEL00F)  and  (TBLflg <> *blank  or
051200121221                                             ds3C.§3Ccba <> c_EasySpedWEB));
051300121221         // -?Cliente non più in tab. "3EW" (pur essendo ancora?
051400121221         //  ?abilitato) - ????? (impossibile!!!)?
051500121221         $no3EW    = (Not  %found(TNTBE01L)  or
051600121221                      %found(TNTBE01L)  and  (TBEatb <> *blank  or
051700121221                                             d3EW.§3EWfaa <> *blank));
051800121221
051900121221         // -?Cliente NON più abilitato ad EasySpedWEB su Strategi?
052000121221         //  ?ma ancora in tab. "3C" e "3EW"?
052100121221         //  ?(IMPOSSIBILE vista l'estrazione sql)?
052200121221         //$DisabVSS = (%found(TIVSS01L)  and  VSSdsc <= wDate
052300121221         //                               and
052400121221         //             %found(TABEL00F)  and  TBLflg = *blank  and
052500121221         //                                    ds3C.§3Ccba = c_EasySpedWEB
052600121221         //                               and
052700121221         //             %found(TNTBE01L)  and  TBEatb = *blank  and
052800121221         //                                    d3EW.§3EWfaa = *blank;
052900121221
053000121218
053100121221         // -?Cliente "a posto"?
053200121221         if  Not $ACOblocc  and
053300121221             Not $3CnoESW   and
053400121221             Not $no3EW;
053500121218           leavesr;
053600121218         endif;
053700121217
053800121218         // -?Reperimento dei clienti bloccati nella famiglia?
053900121218         exsr  sr_ChkABL;
054000121218         if  Not $ScadEW;
054100121218           leavesr;
054200121218         endif;
054300121218
054400121218         // -?Scadenza servizio EasySped-WEB di Strategi.?                 ?
054500121218         exsr  sr_ScadEW_VSS_3EW;
054600121217
054700121217       ENDSR;
054800121218
054900121218       //--------------------------------------------------------------
055000121218       //?Reperimento flag di abilitazione per ciascun cliente "ESWEB" ?
055100121218       //?  nella famiglia.                                            ?
055200121218       //--------------------------------------------------------------
055300121218       BEGSR  sr_ChkABL;
055400121218
055500121218         //$ScadEW = *off;         (già fatto)?
055600121218
055700121218         // -?Reperimento famiglia completa del cliente in esame?
055800121218         clear  ubLeg3C_FlgPF;
055900121218         clear  ubLeg3C_Padre;
056000121218         clear  ubLeg3C_SKC;
056100121218         clear  ubLeg3C_SKEW;
056200121218
056300121218         If  wSQL_dati.wSQLksu > *zero;
056400121218
056500121218           if  ubLeg3C_Rtv ( %int( %trim( wSQL_dati.wSQLksu ) ) :
056600121218                             ubLeg3C_FlgPF :
056700121218                             ubLeg3C_Padre :
056800121218                             ubLeg3C_SKC :
056900121218                             ubLeg3C_SKEW ) < *zero;
057000121220             if  sch_SKC(1) = *zero;
057100121220               sch_SKC(1) = %int( %trim( wSQL_dati.wSQLksu ) );
057200121220             endif;
057300121218           endif;
057400121218
057500121218         EndIf;
057600121218
057700121218         // -?Impostazione schiera dei soli clienti della famiglia che?
057800121218         //  ?usano EasySped WEB?
057900121218         clear  yy;
058000121218         clear  ww;
058100121218         clear  sk_KSC;
058200121218         clear  sk_ABL;
058300121218
058400121218         For  yy = 1  To  %elem(sch_SKEW);
058500121218
058600121218           select;
058700121218
058800121218             when  sch_SKC(yy)  = *zero;
058900121218               leave;
059000121218
059100121218             when  sch_SKEW(yy) = *on;
059200121218               ww += 1;
059300121218               sk_KSC(ww) = sch_SKC(yy);
059400121218
059500121218           endsl;
059600121218
059700121218         EndFor;
059800121218
059900121218         // -?Reperimento dell'eventuale blocco di ciascuno dei clienti?
060000121218         //  ?qui sopra intabellati?
060100121218         if  ww > 1;
060200121218           ubChkABL_Chk ( ubChkABL_Ksc : ubChkABL_Abl );
060300121218         else;
060400121218           sk_ABL(1) = ACOabl;
060500121218         endif;
060600121218
060700121218         // -?Verifica se abilitazione ad EasySped-WEB da far scadere?
060800121218         $ScadEW = *on;
060900121218         For  yy = 1  To  ww;
061000121218
061100121218           select;
061200121218
061300121218             when  sk_KSC(yy) = *zero;
061400121218               leave;
061500121218
061600121218             // ·?? = Cliente NON trovato in anagrafica?
061700121218             // ·?A = Cliente annullato in anagrafica?
061800121218             // ·?8/* = Cliente bloccato?
061900121218             when  sk_ABL(yy) = *blank;
062000121218               $ScadEW = *off;
062100121218               leave;
062200121218
062300121218           endsl;
062400121218
062500121218         EndFor;
062600121218
062700121218       ENDSR;
062800121217
062900121217       //--------------------------------------------------------------
063000121217       //?Scadenza servizio EasySped-WEB di Strategi.                  ?
063100121217       //--------------------------------------------------------------
063200121218       BEGSR  sr_ScadEW_VSS_3EW;
063300121217
063400121218         // -?Forzatura scadenza servizio EasySped-WEB di Strategi?
063500130208         IF  wSQL_dati.wSQLdsc > wDate;
063600121221
063700130208           If  Not $Test;
063800130208
063900130208             clear  k02tivss01;
064000130208             k_VSSsun = wSQL_dati.wSQLsun;
064100130208             k_VSSisv = 'EW';
064200130208             chain  %kds(k02tivss01)  TIVSS000;
064300130208             if  %found(TIVSS01L)  and  VSSdsc > wDate;
064400130208               VSSdsc = wDate;
064500130208               //_______________
064600130208               update  TIVSS000;
064700130208               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064800130208             else;
064900130208               unlock  TIVSS01L;
065000130208             endif;
065100130208
065200130208           Else;
065300130208
065400130208             if  *inOA;
065500130208               except  PrtTXT;
065600130208               *inOA = *off;
065700130208             endif;
065800130208             clear  s_Flags_ds;
065900130208             s_Ksu = wSQL_dati.wSQLksu;
066000130208             s_Sun = wSQL_dati.wSQLsun;
066100130208             if  $ACOblocc;
066200130208               s_ACOblocc = 'S';
066300130208             endif;
066400130208             if  $3CnoESW;
066500130208               s_3CnoESW  = 'N';
066600130208             endif;
066700130208             if  $no3EW;
066800130208               s_No3EW    = 'N';
066900130208             endif;
067000130208             except  PrtVSS;
067100130208
067200130208           EndIf;
067300121221
067400130208         ENDIF;
067500121218
067600121221         // -?Forzatura annullamento applicativo in tab. "3EW"?
067700121221         IF  %found(TNTBE01L)  and  TBEatb = *blank
067800121221                               and  d3EW.§3EWfaa = *blank;
067900130208
068000130208           If  Not $Test;
068100121221
068200130208             clear  k05tntbe01;
068300130208             k_TBEcod = '3EW';
068400130208             k_TBEke1 = wSQL_dati.wSQLksu;
068500130208             k_TBEke2 = wSQL_dati.wSQLsun;
068600121221
068700130208             For  w_SisInf = 1  To  c_NrLibr;
068800130208
068900130208               exsr  sr_OpenFileTBE;
069000130208
069100130208               chain  %kds(k05tntbe01)  TNTBE000;
069200130208
069300130208               if  %found(TNTBE01L)  and  TBEatb = *blank;
069400130208                 d3EW = TBEuni;
069500130208                 if  d3EW.§3EWfaa = *blank;
069600130208                   d3EW.§3EWfaa = 'A';
069700130208                   d3EW.§3EWdaa = %editc( wDate : 'X' );
069800130208                   d3EW.§3EWuum = DUTute;
069900130208                   TBEuni = d3EW;
070000130208                   TBEapl = X3EWapl;
070100130208                   TBEftt = 'S';
070200130208                   clear  TBEflt;
070300130208                   if  w_SisInf = 1;
070400130208                     TBEftr = 'T';
070500130208                   else;
070600130208                     TBEftr = 'R';
070700130208                   endif;
070800130208                   TBEdtr = wDate;
070900130208                   //_______________
071000130208                   update  TNTBE000;
071100130208                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071200130208                 else;
071300130208                   unlock  TNTBE01L;
071400130208                 endif;
071500130208               else;
071600130208                 unlock  TNTBE01L;
071700130208               endif;
071800130208
071900130208             EndFor;
072000121218
072100130208             // -?Aggiornamento flag di dato variato (§3EWUPD) in tab."3EW"?
072200130208             wRtnEsito = Ub§3EWupd_Upd ( wSQL_dati.wSQLksu :
072300130208                                         wSQL_dati.wSQLsun :
072400130208                                         'A' : *off );
072500130208             if wRtnEsito < *zero  and  wRtnEsito <> -4;
072600130208               //$EoF = *on;
072700130208               wErr = wRtnEsito;
072800130208               exsr  sr_SendEmail;
072900130208               //exsr  sr_RoutEnd;
073000130208             endif;
073100130208
073200130208           Else;
073300130208
073400130208             if  *inOA;
073500130208               except  PrtTXT;
073600130208               *inOA = *off;
073700130208             endif;
073800130208             clear  s_Flags_ds;
073900130208             s_Ksu = wSQL_dati.wSQLksu;
074000130208             s_Sun = wSQL_dati.wSQLsun;
074100130208             if  $ACOblocc;
074200130208               s_ACOblocc = 'S';
074300130208             endif;
074400130208             if  $3CnoESW;
074500130208               s_3CnoESW  = 'N';
074600130208             endif;
074700130208             if  $no3EW;
074800130208               s_No3EW    = 'N';
074900130208             endif;
075000130208             except  Prt3EW;
075100130208
075200130208           EndIF;
075300130208
075400130208         ENDIF;
075500121217
075600121217       ENDSR;
075700121218
075800121218       //--------------------------------------------------------------
075900121218       //?Apertura dei files tabelle nel sistema informativo impostato.?
076000121218       //--------------------------------------------------------------
076100121221       BEGSR  sr_OpenFileTBE;
076200121221
076300121221         ds_Libr   = $Libraries(w_iSystem);
076400121221         if  %subst(wLibTNTBE : 1 : 10) = $Libr(w_SisInf);
076500121221           leavesr;
076600121221         endif;
076700121218
076800121218         // -?Chiusura (eventuale) file tabelle?
076900121218         if  %open(TNTBE01L);
077000121218           close  TNTBE01L;
077100121218         endif;
077200121218
077300121218         // -?Apertura file tabelle?
077400121218         wLibTNTBE = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
077500121218         open  TNTBE01L;
077600121218
077700121218       ENDSR;
077800121217
077900121217       //--------------------------------------------------------------
078000121217       //?Invio e-mail di avviso errore.                               ?
078100121217       //--------------------------------------------------------------
078200121217       BEGSR  sr_SendEmail;
078300121217
078400121217         // -?Override al file di stampa ed apertura dello stesso?
078500121217         if  NOT  %open(PrtEMAIL);
078600121217           §CM1mitt = %trim(§MRAdmitt);
078700121217           §CM1dst  = %trim(§MRAddest);
078800121217           §CM1tips = §MRAdreg;
078900121217           §CM1var  = '*OBJM*' + §MRAddes;
079000121217           §CM1idp  = §MRAdidpro;
079100121217           Qcmd = c_CmdOvrPrtF
079200121217                + ' outq(' + %trim(§MRAdoutqi) + ')'
079300121217                + ' usrdfndta(''' + TRTCM1ds + ''')';
079400121217           exsr  sr_ExecCmd;
079500121217           //*inH1 = (Qusei <> *blank);
079600121217           open  PrtEMAIL;
079700121217         endif;
079800121217
079900121217         // -?Stampa del messaggio di errore?
080000121217         select;
080100121217           when  wErr = 1;
080200121217             O_testo = 'Errore nell''apertura del cursore SQL.';
080300121217           when  wErr = 2;
080400121217             O_testo = 'Errore nella lettura del cursore SQL.';
080500121217           when  wRtnEsito = -1;
080600121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
080700121217                        rilevato parametri errati per il cliente ' +
080800121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
080900121218                        %trimr(wSQL_dati.wSQLsun);
081000121217           when  wRtnEsito = -2;
081100121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
081200121217                        rilevato errori nel reperimento del nome +
081300121217                        del sistema per il cliente ' +
081400121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
081500121218                        %trimr(wSQL_dati.wSQLsun);
081600121217           when  wRtnEsito = -3;
081700121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
081800121217                        rilevato errori nell''apertura del file +
081900121217                        tabelle (tab. "3EW") per il cliente ' +
082000121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
082100121218                        %trimr(wSQL_dati.wSQLsun);
082200121217           when  wRtnEsito = -4;
082300130605             O_testo = 'Il programma di servizio UB§3EWUPD non ha +
082400130605                        trovato il cliente ' +
082500130605                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
082600130605                        %trimr(wSQL_dati.wSQLsun) +
082700130605                       ' in tab. "3EW".';
082800130605           when  wRtnEsito = -5;
082900121217             O_testo = 'Fallito aggiornamento del flag "Modificato" +
083000121217                        (§3EWUPD) in tab. "3EW" per il cliente ' +
083100121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
083200121218                        %trimr(wSQL_dati.wSQLsun);
083300121217           when  wRtnEsito = -6;
083400121217             O_testo = 'Il programma di servizio UB§3EWUPD ha +
083500121217                        rilevato errori nella chiusura del file +
083600121217                        tabelle (tab. "3EW"), key ' +
083700121218                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
083800121218                        %trimr(wSQL_dati.wSQLsun);
083900130605           when  wRtnEsito = -7;
084000130807             O_testo = 'Fallito aggiornam. del flag "Modificato" +
084100130807                        (§3EWUPD) in tab. "3EW" nel SIST.+
084200130807                        INFORM. di FIL. per il cliente ' +
084300130605                        %trimr(wSQL_dati.wSQLksu) + ' - ' +
084400130605                        %trimr(wSQL_dati.wSQLsun);
084500121217         endsl;
084600121217         if  O_Testo <> *blank;
084700121217           except  PrtDet;
084800121217         endif;
084900121217
085000121217         // -?Chiusura del file di stampa e cancellazione override?
085100121218         //close  PrtEMAIL;        ?(conviene farlo a fine *pgm)?
085200121218         //Qcmd = c_CmdDltOvr;     ?(conviene farlo a fine *pgm)?
085300121218         //exsr  sr_ExecCmd;       ?(conviene farlo a fine *pgm)?
085400121217
085500121217       ENDSR;
085600121217
085700121217       //--------------------------------------------------------------
085800121217       //?Operazioni finali.                                           ?
085900121217       //--------------------------------------------------------------
086000121217       BEGSR  sr_RoutEnd;
086100130208
086200130208         // -?Stampa "Fine Lista"?
086300130208         if  $Test;
086400130208           except  PrtEND;
086500130208           close  qSysPrt;
086600130208         endif;
086700121217
086800121217         // -?Chiusura applicazioni precedentemente aperte?
086900121217         ubLeg3C_Close ();
087000121217         ubChkABL_Close ();
087100121217
087200121217         // -?Chiusura cursore SQL?
087300121217         exsr  sr_CloseCursor;
087400121218
087500121218         // -?Chiusura archivi?
087600121218         if  %open(TNTBE01L);
087700121218           close  TNTBE01L;
087800121218         endif;
087900121220         if  %open(TABEL00F);
088000121220           close  TABEL00F;
088100121220         endif;
088200121218
088300121218         // -?Chiusura del file di stampa e cancellazione override?
088400121218         if  %open(PrtEMAIL);
088500121218           close  PrtEMAIL;
088600121218           Qcmd = c_CmdDltOvr;
088700121218           exsr  sr_ExecCmd;
088800121218         endif;
088900121217
089000121217         // -?Chiusura pgm?
089100121217         return;
089200121217
089300121217       ENDSR;
089400121218
089500121218       //--------------------------------------------------------------
089600121218       //?Esecuzione del comando (già impostato).                      ?
089700121218       //--------------------------------------------------------------
089800121218       BEGSR  sr_ExecCmd;
089900121218
090000121218         clear Qcap0100;
090100121218         Qcabcsdh = *off;
090200121218         Qcapa    = *off;
090300121218         Qcacmdss = *off;
090400121218         Qcaerved = *allX'00';
090500121218
090600121218         clear Qusec;
090700121218         Qusbprv  = %size(Qusec);
090800121218
090900121218         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
091000121218                           %size(Qcap0100) : 'CPOP0100' : *omit :
091100121218                           0 : 0 : Qusec);
091200121218
091300121218         //if  Qusei <> *blank;
091400121218         //  ...;
091500121218         //endif;
091600121218
091700121218       ENDSR;
091800121217
091900121217      /end-free
092000130208
092100130208       //--------------------------------------------------------------
092200130208       //?Spool di stampa (alternativo all'aggiornamento).             ?
092300130208       //--------------------------------------------------------------
092400130208
092500130208     oqSysPrt   e            PrtTXT            2
092600130208     o                       RsUt
092700130208     o                                         +  3 'DISABILITAZIONE C-
092800130208     o                                              LIENTI AL SERVIZIO-
092900130208     o                                               STRATEGI'
093000130208     o                       SDSpgm            + 10
093100130208     o                       *date         y   +  5
093200130208     o                                         +  5 'Pag.'
093300130208     o                       Page1         z   +  0
093400130208     o          e            PrtTXT      0  0
093500130208     o                                         + 23 'DISABILITAZIONE C-
093600130208     o                                              LIENTI AL SERVIZIO-
093700130208     o                                               STRATEGI'
093800130208     o          e            PrtTXT      2
093900130208     o                                              'Unific. '
094000130208     o                                         +  2 '   S.U.N.'
094100130208     o                                         +  2 'ABL'
094200130208     o                                         +  2 'ESW'
094300130208     o                                         +  2 '3EW'
094400130208     o                                         +  2 'Segnalazione'
094500130208     o          e            PrtTXT      1
094600130208     o                                              '========'
094700130208     o                                         +  2 '========='
094800130208     o                                         +  2 '==='
094900130208     o                                         +  2 '==='
095000130208     o                                         +  2 '==='
095100130208     o                                         +  2 '============'
095200130208      *
095300130208     o          e            PrtVSS      1
095400130208     o                       s_ksu
095500130208     o                       s_sun             +  2
095600130208     o                       s_ACOblocc        +  3
095700130208     o                       s_3CnoESW         +  4
095800130208     o                       s_No3EW           +  4
095900130208     o                                         +  3 'Unificante disabi-
096000130208     o                                              litabile al serviz-
096100130208     o                                              io internet "EW"'
096200130208      *
096300130208     o          e            Prt3EW      1
096400130208     o                       s_ksu
096500130208     o                       s_sun             +  2
096600130208     o                       s_ACOblocc        +  3
096700130208     o                       s_3CnoESW         +  4
096800130208     o                       s_No3EW           +  4
096900130208     o                                         +  3 'Annullamento appl-
097000130208     o                                              icativo forzabile -
097100130208     o                                              in tab. "3EW"'
097200130208      *
097300130208     o          e            PrtEND      2
097400130208     o                                         + 24 '***  Fine Stampa  ***'
097500121217
097600121217       //--------------------------------------------------------------
097700121217       //?Spool di stampa (per e-mail).                                ?
097800121217       //--------------------------------------------------------------
097900121217
098000121217     oPrtEMAIL  e            PRTdet      1
098100121217     o                       O_testo
098200121218
098300121218       //--------------------------------------------------------------
098400121218       //?Definizione schiere a tempo di compilazione                  ?
098500121218       //--------------------------------------------------------------
098600121218
098700121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
098800121218SETRAS  GAITRAGRU FILTRAGRU
098900121218AS888   GAITRAGRPSFILTRAGRPF
