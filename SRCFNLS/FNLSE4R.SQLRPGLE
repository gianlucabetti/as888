000100101206       //==============================================================
000200090430       //?STATISTICA NUMERO PAGINE DI DDT IN LDV PER CLIENTE           ?
000300101206       //==============================================================
000400090507       //? N.B.: ?Il conteggio dei record in FIAR8 avviene in maniera
000500090507       // piuttosto "chiodata":
000600130315       // ogni "Tipo Stampa DDT" prevede un suo limite di OverFlow,
000700090507       // pertanto certi conteggi non possono che essere "chiodati" di
000800090507       // conseguenza.
000900090507       //?Pertanto:?se/quando si dovesse prevedere un nuovo
001000090507       // "Tipo Stampa DDT" in tab. "TSD" occorrerà prevederlo anche
001100090507       // nei conteggi operati da questo programma!!!
001200090507       //?Stessa cosa?(ma questo è più difficle) se venisse modificato
001300090507       // (nel programma di stampa LdV) un limite di OverFlow, o se
001400090507       // venisse gestito un nuovo tipo record per un cliente...
001500090507       //--------------------------------------------------------------
001600101206
001700101206       //--------------------------------------------------------------
001800101206       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001900101206       //--------------------------------------------------------------
002000101206
002100101206     /*PRM dbgview(*source)
002200101206     /*CMD ovrdbf file(FIAR800F) tofile(FILTRAPRD/FIAR800F) +
002300101206     /*CMD        ovrscope(*calllvl)
002400101206     /*CMD ovrdbf file(FNBLP00F) tofile(FILTRAPRD/FNBLP00F) +
002500101206     /*CMD        ovrscope(*calllvl)
002600131220     /*CMD ovrdbf file(TIPDL00F) tofile(GAITRAAZP/TIPDL00F) +
002700131220     /*CMD        ovrscope(*calllvl)
002800101206     /*END dltovr file(FIAR800F) lvl(*)
002900101206     /*END dltovr file(FNBLP00F) lvl(*)
003000131220     /*END dltovr file(TIPDL00F) lvl(*)
003100101206     /*END
003200101206
003300101206       //--------------------------------------------------------------
003400101206       //?Specifiche di controllo.                                     ?
003500101206       //--------------------------------------------------------------
003600060327
003700060327     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003800060327     h alwnull(*inputonly)
003900101206     h dftactgrp(*no) bnddir('UBBNDDIR')
004000060327
004100090430       //--------------------------------------------------------------
004200090430       //?Dichiarazione file.                                          ?
004300090430       //--------------------------------------------------------------
004400090430
004500101206       // -?Organigramma?
004600060331     fAZORG01L  if   e           k disk
004700101206       // -?Tabelle?
004800090430     fTNTBE01L  if   e           k disk
004900090430
005000131220       // -?Statistica n° Pagine di Packing-List in LdV?
005100131220     fTIPDL00F  o    e             disk    extfile(wLibFileS)
005200131220     f                                     usropn
005300090507
005400140123       // -?File di stampa per eventuale invio e-mail?
005500140123     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
005600060327
005700090430       //--------------------------------------------------------------
005800090430       //?Definizione costanti.                                        ?
005900090430       //--------------------------------------------------------------
006000090430
006100101206       // -?Costanti per stringa SQL?
006200090430     d c_Union         c                   const('UNION')
006300090430     d c_OrderBy       c                   const('ORDER BY +
006400060330     d                                            BLPksc, +
006500060331     d                                            BLPrsm, +
006600060331     d                                            AR8tmd, +
006700060330     d                                            AR8aas, +
006800060330     d                                            AR8lnp, +
006900060330     d                                            AR8nrs, +
007000130927     d                                            AR8nsp')
007100090430     d c_ForFetch      c                   const('FOR FETCH ONLY')
007200101206
007300101206       // -?Comandi da eseguire?
007400140123     d c_CmdOvrPrtF    c                   const('OVRPRTF +
007500140123     d                                           file(PRTEMAIL) +
007600140123     d                                           pagesize(66 132) +
007700140123     d                                           lpi(6) cpi(10) +
007800140123     d                                           ovrscope(*actgrpdfn) +
007900140123     d                                           usrdta(''FNLSE4*ERR'') +
008000140123     d                                           ')
008100140123     d c_CmdDltOvr     c                   const('DLTOVR +
008200140123     d                                            file(PRTEMAIL) +
008300140123     d                                            lvl(*actgrpdfn)')
008400101206
008500101206       // -?Costanti per la definizione delle schiere con i nomi?
008600101206       //  ?degli iSeries da elaborare e delle relative librerie?
008700101206     d c_NrSyst        c                   const(2)
008800101206     d c_NrLibr        c                   const(2)
008900090430
009000090430       //--------------------------------------------------------------
009100090430       //?Definizione schiere.                                         ?
009200090430       //--------------------------------------------------------------
009300101206
009400101206       // -?iSeries & Librerie in cui gestire i files?
009500101206     d $iSystem        s                   like(currSysNetA)
009600101206     d                                     dim(c_NrSyst)
009700101206     d                                     ctdata   perrcd( 1)
009800101206     d $Libraries      s                   like(ds_Libr)
009900101206     d                                     dim(c_NrSyst)
010000101206     d                                     alt($iSystem)
010100090430
010200101206       // -?Stringhe SQL da eseguire?
010300090507     d $Sql            s             70    dim(25)  ctdata  perrcd(1)
010400090430
010500101206       // -?Totali per cliente?
010600090507     d $_ddt           s              7  0 dim(6)   inz
010700131007     d $_Loghi         s                   like(AR8tmd)
010800131007     d                                     dim(9)   inz
010900131007     d $_Sped§§        s              7  0 dim(9)   inz
011000131007     d $_nPdf§§        s              7  0 dim(9)   inz
011100090430
011200101206       // -?Schiera tab. "TSD" = Tipo Stampa DDT?
011300090506     d $TSD            s                   like(§AR8tsd) dim(015) inz
011400090506
011500101206       // -?Schiere tab. "AR8" = Tipo Modulo DDT x file FIAR8:?
011600101206       // - -?con "Tipo stampa DDT" = "S1" (Std senza logo)?
011700090506     d $AR8_S1         s                   like(AR8tmd)  dim(010) inz
011800131007      *// - -?con "Tipo stampa DDT" = "S2" (Std con logo)?
011900090506     d*// $AR8_S2         s                   like(AR8tmd)  dim(999999) inz
012000101206       // - -?con "Tipo stampa DDT" = "E1" (Esprinet)?
012100130121     d $AR8_E1         s                   like(AR8tmd)  dim(020) inz
012200101206       // - -?con "Tipo stampa DDT" = "K1" (Karnak)?
012300130121     d $AR8_K1         s                   like(AR8tmd)  dim(020) inz
012400101206       // - -?con "Tipo stampa DDT" = "00" (personalizzato - v. Arcte)?
012500090506     d $AR8_00         s                   like(AR8tmd)  dim(010) inz
012600101206       // - -?con "Tipo stampa DDT" = "L*" (Logistica, Sait o Brumar)?
012700130121     d $AR8_Lx         s                   like(AR8tmd)  dim(099) inz
012800130927       // - -?con "Tipo stampa DDT" = "§§" (PDF allegato)?
012900130927     d $AR8_§§         s                   like(AR8tmd)  dim(010) inz
013000090430
013100090430       //--------------------------------------------------------------
013200090430       //?Definizione aree dati.                                       ?
013300090430       //--------------------------------------------------------------
013400090430
013500101206       // -?Dati utente?
013600090430     d §AzUte        e ds                  extname(AZUTE00F)
013700090430     d                                     dtaara
013800090430     d §DatiUte      e ds                  extname(dDatiUte)
013900090430     d                                     dtaara
014000090430
014100090430       //--------------------------------------------------------------
014200090430       //?Definizione strutture dati.                                  ?
014300090430       //--------------------------------------------------------------
014400090430
014500101206       // -?Status ds?
014600090430     d Status         sds
014700131007     d   SDSpgm          *proc
014800131007     d   SDSprm          *parms
014900101206
015000101206       // -?Parametri ricevuti?
015100060327     d KPJBA         e ds
015200060330     d FNLSE4ds        ds                  inz
015300131007     d   DE4dti                       8  0 inz(*loval)
015400131007     d   DE4dtf                       8  0 inz(*hival)
015500090430
015600101206       // -?Ds di comodo per la definizione dei campi estratti via SQL?
015700131009     d FIAR8ds       e ds                  extname(FIAR800F)
015800131009     d                                     based(nullPtr)
015900131009     d FNBLPds       e ds                  extname(FNBLP00F)
016000131009     d                                     based(nullPtr)
016100090506
016200101206       // -?Tab. AR8 = Tipi Modulo DDT x file FIAR8?
016300090506     d dAR8          e ds                  inz
016400140123
016500140123       // -?Tab. MRA = Mailing - regole x invio da gestionale?
016600140123     d dMRAdan       e ds                  inz
016700140123     d   §MRADdes    e                     inz('ERR statist. Packing+
016800140123     d                                          -List')
016900140123     d   §MRADreg    e                     inz('COR')
017000140123     d   §MRADmitt   e                     inz('ced')
017100140123     d   §MRADdest   e                     inz('ced@brt.it; +
017200140123     d                                          stefano.merighi+
017300140123     d                                          @brt.it')
017400140123     d   §MRADidPro  e                     inz('2')
017500140123     d   §MRADoutqI  e                     inz('EMAILIN')
017600140123
017700140123       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
017800140123     d TRTCM1ds      e ds                  inz
017900140123         //?·§CM1mitt = Indirizzo e-mail del mittente?
018000140123     d   §CM1mitt    e                     inz('ced')
018100140123         //?·§CM1dst  = Indirizzo e-mail del destinatario?
018200140123     d   §CM1dst     e                     inz('ced@brt.it;+
018300140123     d                                          stefano.merighi+
018400140123     d                                          @brt.it')
018500140123         //?·§CM1tips = Tipo lettera via e-mail:?
018600140123         // ?"LET" = testo allegato in corpo con logo?
018700140123         //         ?(richiede righe libere iniziali per il logo)?
018800140123         // ?"COR" = testo integrato senza logo?
018900140123         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
019000140123     d   §CM1tips    e                     inz('COR')
019100140123         //?·§CM1po   = Filiale?
019200140123     d   §CM1po      e                     inz('046')
019300140123         //?·§CM1var  = Oggetto e-mail?
019400140123     d   §CM1var     e                     inz('*OBJM*+
019500140123     d                                     ERRORE in calcolo n° pag. -
019600140123     d                                     di Packing-List in LdV')
019700140123         //?·§CM1sts  = Stato?
019800140123     d   §CM1sts     e                     inz(*off)
019900140123         //?·§CM1sts  = Id processo?
020000140123     d   §CM1idp     e                     inz('2')
020100090430
020200101206       // -?Dati estratti via SQL?
020300060327     d wSQLds          ds                  inz
020400060327     d  SQLksc                             inz  like(BLPksc)
020500060327     d  SQLrsm                             inz  like(BLPrsm)
020600060331     d  SQLtmd                             inz  like(AR8tmd)
020700060327     d  SQLaas                             inz  like(AR8aas)
020800060327     d  SQLlnp                             inz  like(AR8lnp)
020900060327     d  SQLnrs                             inz  like(AR8nrs)
021000060327     d  SQLnsp                             inz  like(AR8nsp)
021100060327     d  SQLtrk                        2    inz
021200060327     d  SQLrec                        9  0 inz
021300090430
021400060328     d wSavSQLds       ds                  inz
021500090506     d  SavSQLksc                          inz  like(SQLksc)
021600090506     d  SavSQLrsm                          inz  like(SQLrsm)
021700090506     d  SavSQLtmd                          inz  like(SQLtmd)
021800090506     d  SavSQLaas                          inz  like(SQLaas)
021900090506     d  SavSQLlnp                          inz  like(SQLlnp)
022000090506     d  SavSQLnrs                          inz  like(SQLnrs)
022100090506     d  SavSQLnsp                          inz  like(SQLnsp)
022200090506     d  SavSQLtrk                          inz  like(SQLtrk)
022300090506     d  SavSQLrec                          inz  like(SQLrec)
022400060328     d  SavNrPag                           inz  like(wNrPag)
022500060328     d  SavXX                              inz  like(xx)
022600090507
022700101206       // -?Schiera per "scomposizione" stringa SQL in stampa?
022800090507     d ds_Temp         ds          5000    inz
022900090507     d   $Temp                 1   5000    inz  dim(50)
023000101206
023100101206       // -?Ridefinizione elenco librerie in elaborare le tabelle?
023200101206     d ds_Libr         ds                  inz
023300131007     d   $Libr                       10    dim(c_NrLibr) inz
023400090430
023500090430       //--------------------------------------------------------------
023600090430       //?Definizione variabili globali.                               ?
023700090430       //--------------------------------------------------------------
023800090430
023900101206       // -?Flags booleani?
024000090430     d $EoF            s               n   inz
024100131220     d $Schedul        s               n   inz
024200090430
024300101206       // -?Indici di schiera?
024400060327     d xx              s              3  0 inz
024500131007     d yy              s              3  0 inz
024600090506     d xS1             s              3  0 inz
024700090506     d*// xS2             s              3  0 inz
024800090506     d xE1             s              3  0 inz
024900090506     d xK1             s              3  0 inz
025000090506     d x00             s              3  0 inz
025100090506     d xLx             s              3  0 inz
025200130927     d x§§             s              3  0 inz
025300090430
025400131009       // -?Stringhe SQL da eseguire?
025500130927     d wSQL            s           7000    inz  varying
025600140123     d wSql0           s           1000    inz  varying
025700140123     d wSql2           s           1000    inz  varying
025800090430
025900101206       // -?Campi per controllo "rotture di livello"?
026000090507     d WWWfil          s                   inz  like(ORGfil)
026100101206
026200131220       // -?Nome esteso Libreria/File dei 2 file di output?
026300101206     d wLibFile        s             21a   inz
026400131220     d wLibFileS       s             21a   inz
026500110128
026600110128       // -?Totali per cliente?
026700110128     d wPagOltre5      s              9  0 inz
026800090430
026900101206       // -?Campi di comodo?
027000101206     d w_iSystem       s              1  0 inz
027100101206     d w_SisInf        s              3  0 inz
027200090507     d wTime           s              6  0 inz
027300090507     d wDate           s              8  0 inz
027400090507     d wDataLim        s               d   inz  datfmt(*iso)
027500090507     d wData_Iso       s               d   inz  datfmt(*iso)
027600060327     d wNrPag          s              9  0 inz
027700060327     d wResto          s              9  0 inz
027800140123     d wErr            s              3  0 inz
027900130927     d Sped§§          s              7  0 inz
028000131003     d nPDF§§          s              9  0 inz
028100131007     d wAR8tmd         s                   inz  like(AR8tmd)
028200090506
028300090506       //--------------------------------------------------------------
028400090506       //?Definizione prototipi procedure.                             ?
028500090506       //--------------------------------------------------------------
028600101206
028700101206       // -?Reperimento NETA sistema AS/400 corrente?
028800131220     d currSysNeta     s              8a   inz
028900101206      /copy gaitrasrc/srcProtoPR,UBRTVNETA
029000090506
029100101206       // -?Reperimento dati utente?
029200131220     d TIBS34ds      e ds                  inz
029300090506      /copy gaitrasrc/srcProtoPR,TIBS34R
029400130121
029500130121       // -?Controllo riempimento schiere?
029600130121     d TRUL0Sds      e ds                  qualified  inz
029700130121      /copy gaitrasrc/srcProtoPR,TRUL0SR
029800090507
029900140123       // -?Parametri API QCAPCMD (Process Commands)?
030000140123     d Qcmd            s           2048    inz  varying
030100140123      /copy qSysInc/qRpgleSrc,QCAPCMD
030200140123       // -?API QCAPCMD (Process Commands)?
030300140123      /copy gaitrasrc/srcProtoPR,QCAPCMD
030400140123
030500140123       // -?Parametri gestione errori API.?
030600140123      /copy qsysinc/qrpglesrc,QUSEC
030700090430
030800090430       //--------------------------------------------------------------
030900090430       //?Definizione key-list.                                        ?
031000090430       //--------------------------------------------------------------
031100090430
031200101206       // -?File TNTBE01L?
031300090430     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
031400090430     d                                     prefix(k_)   inz
031500060327
031600090430       //--------------------------------------------------------------
031700090430       //?Riepilogo indicatori utilizzati.                             ?
031800090430       //--------------------------------------------------------------
031900090430       //--------------------------------------------------------------
032000090430
032100090430       //--------------------------------------------------------------
032200090430       //?M A I N - L I N E                                            ?
032300090430       //--------------------------------------------------------------
032400090430
032500060327     c     *Entry        plist
032600060327     c                   parm                    KPJBA
032700090430
032800090430      /free
032900090430
033000101206       // -?Operazioni iniziali?
033100090430       exsr  sr_RoutInz;
033200090430
033300101206       // -?Preparazione stringa SQL?
033400090506       exsr  sr_PrepSQL;
033500090506
033600101206       // -?Ciclo di elaborazione?
033700090506       exsr  sr_OpenCursor;
033800090506
033900090506       dou  $EoF = *on;
034000090506         exsr  sr_ReadCursor;
034100090506       enddo;
034200090506
034300090506       exsr  sr_CloseCursor;
034400090430
034500101206       // -?Operazioni finali?
034600090430       exsr sr_RoutEnd;
034700090430
034800090430       //--------------------------------------------------------------
034900090430       //?Operazioni iniziali.                                         ?
035000090430       //--------------------------------------------------------------
035100090430       BEGSR  sr_RoutInz;
035200090430
035300090430         *inLR = *on;
035400090430
035500101206         // -?Impostazione opzioni per SQL?
035600090430         exec sql   set  option  DynUsrPrf = *Owner,
035700090430                                 CloSqlCsr = *EndMod;
035800090430
035900101206         // -?Reperimento dati job?
036000090430         exsr  sr_DatiJob;
036100090430
036200101206         // -?Reperimento data odierna in formato aaaammgg?
036300090430         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
036400090430         wData_Iso = %date(wDate : *iso);
036500090507
036600101206         // -?Reperimento orario?
036700090507         wTime = %dec( %time() );
036800101206
036900101206         // -?Verifica del sistema AS/400 corrente?
037000101206         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
037100101206           exsr  sr_RoutEnd;
037200101206         endif;
037300131220         // -?Reperimento librerie in stanno i files da gestire?
037400101206         //  ?(a seconda del sistema in cui si stà lavorando)?
037500101206         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
037600101206         if  w_iSystem = *zero;
037700101206           exsr  sr_RoutEnd;
037800101206         endif;
037900131220
038000131220         // -?Verifica se lavoro schedulato o sottomesso da menù?
038100131220         $Schedul = (SDSprm = *zero  or  KPJBU = *blank);
038200140115         if  Not $Schedul;
038300140115           exsr  sr_RoutEnd;
038400140115         endif;
038500131220
038600101206         // -?Impostazione libreria e nome esatto del work-file?
038700101206         ds_Libr  = $Libraries(w_iSystem);
038800140115         wLibFileS = %trimr( $Libr(1) ) + '/' + 'TIPDL00F';
038900131220
039000140115         // -?Apertura file?
039100140115         open  TIPDL00F;
039200090430
039300140115         // -?Gestione parametri (forzati)?
039400140115         exsr  sr_NoParms;
039500140123
039600140123         // -?Reperimento regole per mailing da tab. "MRA"?
039700140123         clear  k05tntbe01;
039800140123         k_TBEcod = 'MRA';
039900140123         k_TBEke1 = SDSpgm;
040000140123         chain  %kds(k05tntbe01)  TNTBE000;
040100140123         if  %found(TNTBE01L);
040200140123           dMRAdan = TBEuni;
040300140123         endif;
040400090430
040500101206         // -?Intabellamento dati da tab. "TSD" & "AR8"?
040600090430         exsr  sr_IntabAR8;
040700090430
040800090430       ENDSR;
040900090430
041000090430       //--------------------------------------------------------------
041100090430       //?Reperimento Dati del job (Utente/Operativi).                 ?
041200090430       //--------------------------------------------------------------
041300090430       BEGSR  sr_DatiJob;
041400090430
041500090430         in(E) §AzUte;
041600090430         if NOT %error;
041700090430           in(E) §DatiUte;
041800090430         endif;
041900090430         if %error or RSut = *blanks;
042000090430           clear TIBS34ds;
042100090430           tibs34r ( tibs34ds );
042200090430           in §AzUte;
042300090430           in §DatiUte;
042400090430         endif;
042500090430
042600090430       ENDSR;
042700090430
042800090430       //--------------------------------------------------------------
042900090430       //?Calcolo range di date da ultima estrazione ad oggi           ?
043000090430       //?(se non ricevuti parametri...)                               ?
043100090430       //--------------------------------------------------------------
043200090430       BEGSR  sr_NoParms;
043300090430
043400140123         // -?Preparazione SQL per reperimento ultima data di estrazione?
043500140123         clear  wSql0;
043600140123         wSql0 = 'select  coalesce( max(PDLdtf), 0 ) +
043700140123                    from ' + %trimr( $Libr(1) ) + '/TIPDL00F +
043800140123                     for fetch only';
043900090430
044000140123         // -?Dichiarazione cursore?
044100140123         exec sql   prepare S0   from :wSql0;
044200140123         exec sql   declare C0   cursor   for S0;
044300140123
044400140123         // -?Apertura del cursore?
044500140123         exec sql   open C0;
044600090430
044700140123         // -?Lettura cursore?
044800140123         clear  PDLdtf;
044900140123         If  SQLcode >= *zero;
045000140123
045100140123           exec sql   fetch   from C0   into :PDLdtf;
045200090430
045300140123           select;
045400140123             // -?Errore in elaborazione: si chiude il *pgm?
045500140123             when  SQLcode < *zero;
045600140123               $EoF = *on;
045700140123               wErr = 1;
045800140123               exsr  sr_PrintErrSQL;
045900140123             // -?WrkF vuoto: si chiude il *pgm?
046000140123             when  SQLcode = 100   or   PDLdtf = *zero;
046100140123               //wDataLim = wData_ISO - %days(7);
046200140123               $EoF = *on;
046300140123               wErr = 1;
046400140123               exsr  sr_PrintErrSQL;
046500140123             // -?Si riparte dall'ultima data fine periodo?
046600140123             other;
046700140123               wDataLim = %date( PDLdtf : *iso );
046800140123           endsl;
046900140123
047000140123         EndIf;
047100140123
047200140123         // -?Chiusura del cursore?
047300140123         exec sql   close C0;
047400090430
047500101206         // -?Impostazione date - parametri - per la settimana successiva?
047600101206         //  ?all'ultima elaborata?
047700101206         clear  FNLSE4ds;
047800090430         wDataLim += %days(1);
047900090506         DE4dti = %dec( wDataLim );
048000090430         wDataLim += %days(6);
048100090506         DE4dtf = %dec( wDataLim );
048200090430
048300101206         // -?Test "fattibilità": deve essere elaborabile almeno una?
048400101206         //  ?settimana...?
048500090430         if  wDataLim > wData_ISO;
048600090430           $EoF = *on;
048700090430           exsr  sr_RoutEnd;
048800090430         endif;
048900090430
049000090430       ENDSR;
049100090430
049200090430       //--------------------------------------------------------------
049300090430       //?Intabellamento dati da tab. "TSD" e "AR8"                    ?
049400090430       //--------------------------------------------------------------
049500090430       BEGSR  sr_IntabAR8;
049600090430
049700101206         // -?Intabellamento Tipi Stampa da tab. "TSD"?
049800140123         clear  k05tntbe01;
049900090430         k_TBEcod = 'TSD';
050000090430         setll  %kds(k05tntbe01)  TNTBE000;
050100090430         reade  %kds(k05tntbe01 : 1)  TNTBE000;
050200090430         DoW  Not %eof(TNTBE01L);
050300090430           if  TBEatb = *blank;
050400090430             xx += 1;
050500090506             $TSD(xx) = TBEke1;
050600090507             if  TBEke1 <> 'E1'   and
050700090507                 TBEke1 <> 'K1'   and
050800090507                 TBEke1 <> 'S1'   and
050900090507                 TBEke1 <> 'S2'   and
051000090507                 TBEke1 <> '00'   and
051100140123                 TBEke1 <> '§§'   and            //?PDF, no Pckg-Lst?
051200090507                 %subst(TBEke1 : 1 : 1) <> 'L';
051300140123               wErr = 2;
051400140123               exsr  sr_SendEmail;
051500090507             endif;
051600090430           endif;
051700090430           reade  %kds(k05tntbe01 : 1)  TNTBE000;
051800090430         EndDo;
051900090430
052000101206         // -?Intabellamento Tipi Modulo DDT da tab. "AR8"?
052100140123         clear  k05tntbe01;
052200090430         k_TBEcod = 'AR8';
052300090430         setll  %kds(k05tntbe01)  TNTBE000;
052400090430         reade  %kds(k05tntbe01 : 1)  TNTBE000;
052500090506
052600090430         DoW  Not %eof(TNTBE01L);
052700090506
052800090430           dAR8 = TBEuni;
052900090430           select;
053000090506
053100101206             // -?Rec. annullato?
053200090506             when  TBEatb <> *blank;
053300130927
053400130927             // -?PDF allegati?
053500130927             when  §AR8tsd = '§§';
053600130927               x§§ += 1;
053700130927               $AR8_§§(x§§) = TBEke1;
053800090506
053900101206             // -?Esprinet?
054000090430             when  §AR8tsd = 'E1';
054100090430               xE1 += 1;
054200090506               $AR8_E1(xE1) = TBEke1;
054300090506
054400101206             // -?Karnak?
054500090430             when  §AR8tsd = 'K1';
054600090430               xK1 += 1;
054700090506               $AR8_K1(xK1) = TBEke1;
054800090506
054900101206             // -?Logistica, Sait e Brumar?
055000090430             when  %subst(§AR8tsd : 1 : 1) = 'L';
055100090506               xLx += 1;
055200090506               $AR8_Lx(xLx) = TBEke1;
055300090506
055400101206             // -?Standard senza logo?
055500090430             when  §AR8tsd = 'S1';
055600090430               xS1 += 1;
055700090506               $AR8_S1(xS1) = TBEke1;
055800090506
055900101206             // -?Standard con logo?
056000090430             //   (NON memorizzati in schiera)
056100090430             when  §AR8tsd = 'S2';
056200090506
056300101206             // -?Modulo personalizzato (vedi Arcte)?
056400090430             when  §AR8tsd = '00';
056500090430               x00 += 1;
056600090506               $AR8_00(x00) = TBEke1;
056700090506
056800101206             // -?Tipo Modulo NON PREVISTO!!!?
056900090430             other;
057000140123               wErr = 3;
057100140123               exsr  sr_SendEmail;
057200090506
057300090430           endsl;
057400090506
057500090430           reade  %kds(k05tntbe01 : 1)  TNTBE000;
057600090506
057700090430         EndDo;
057800130121
057900130121         // -?Verifica "riempimento" schiere ed eventuale invio *msg?
058000130121         // - -?per "Tipo stampa DDT" = "S1" (Std senza logo)?
058100130121         clear  TRUL0Sds;
058200130121         TRUL0Sds.i0Sski  = '$AR8_S1';             // 15 char
058300130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
058400130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
058500130121         TRUL0Sds.i0Sele  = %elem($AR8_S1);
058600130121         TRUL0Sds.i0Spie  = xS1;
058700130121         TRUL0Sds.i0Spgm  = SDSpgm;
058800130121         TRUL0Sds.i0Ssif  = knsif;
058900130121         kpjbu = TRUL0Sds;
059000130121         trul0Sr ( kpjba );
059100130121         // - -?per "Tipo stampa DDT" = "E1" (Esprinet)?
059200130121         clear  TRUL0Sds;
059300130121         TRUL0Sds.i0Sski  = '$AR8_E1';             // 15 char
059400130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
059500130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
059600130121         TRUL0Sds.i0Sele  = %elem($AR8_E1);
059700130121         TRUL0Sds.i0Spie  = xE1;
059800130121         TRUL0Sds.i0Spgm  = SDSpgm;
059900130121         TRUL0Sds.i0Ssif  = knsif;
060000130121         kpjbu = TRUL0Sds;
060100130121         trul0Sr ( kpjba );
060200130121         // - -?per "Tipo stampa DDT" = "K1" (Karnak)?
060300130121         clear  TRUL0Sds;
060400130121         TRUL0Sds.i0Sski  = '$AR8_K1';             // 15 char
060500130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
060600130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
060700130121         TRUL0Sds.i0Sele  = %elem($AR8_K1);
060800130121         TRUL0Sds.i0Spie  = xK1;
060900130121         TRUL0Sds.i0Spgm  = SDSpgm;
061000130121         TRUL0Sds.i0Ssif  = knsif;
061100130121         kpjbu = TRUL0Sds;
061200130121         trul0Sr ( kpjba );
061300130121         // - -?per "Tipo stampa DDT" = "00" (personalizzato - v. Arcte)?
061400130121         clear  TRUL0Sds;
061500130121         TRUL0Sds.i0Sski  = '$AR8_00';             // 15 char
061600130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
061700130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
061800130121         TRUL0Sds.i0Sele  = %elem($AR8_00);
061900130121         TRUL0Sds.i0Spie  = x00;
062000130121         TRUL0Sds.i0Spgm  = SDSpgm;
062100130121         TRUL0Sds.i0Ssif  = knsif;
062200130121         kpjbu = TRUL0Sds;
062300130121         trul0Sr ( kpjba );
062400130121         // - -?per "Tipo stampa DDT" = "L*" (Logistica, Sait o Brumar)?
062500130121         clear  TRUL0Sds;
062600130121         TRUL0Sds.i0Sski  = '$AR8_LX';             // 15 char
062700130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
062800130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
062900130121         TRUL0Sds.i0Sele  = %elem($AR8_Lx);
063000130121         TRUL0Sds.i0Spie  = xLx;
063100130121         TRUL0Sds.i0Spgm  = SDSpgm;
063200130121         TRUL0Sds.i0Ssif  = knsif;
063300130121         kpjbu = TRUL0Sds;
063400130121         trul0Sr ( kpjba );
063500130927         // - -?per "Tipo stampa DDT" = "§§" (PDF allegato)?
063600130927         clear  TRUL0Sds;
063700130927         TRUL0Sds.i0Sski  = '$AR8_§§';             // 15 char
063800130927         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
063900130927         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
064000130927         TRUL0Sds.i0Sele  = %elem($AR8_§§);
064100130927         TRUL0Sds.i0Spie  = x§§;
064200130927         TRUL0Sds.i0Spgm  = SDSpgm;
064300130927         TRUL0Sds.i0Ssif  = knsif;
064400130927         kpjbu = TRUL0Sds;
064500130927         trul0Sr ( kpjba );
064600090430
064700090430       ENDSR;
064800090430
064900090506       //--------------------------------------------------------------
065000090506       //?Preparazione stringa SQL                                     ?
065100090506       //--------------------------------------------------------------
065200090506       BEGSR  sr_PrepSQL;
065300101206
065400101206         // -?Impostazione librerie specifiche per i file da gestire?
065500101206         //  ?nella stringa SQL?
065600101206         // - -?libreria del file FIAR800F?
065700101206         $Sql(9) = %replace( $Libr(2) : $Sql(9) :
065800101206                             %scan( '&LibFil1  ' : $Sql(9) ) );
065900101206         // - -?libreria del file FNBLP00F?
066000101206         $Sql(9) = %replace( $Libr(2) : $Sql(9) :
066100101206                             %scan( '&LibFil2  ' : $Sql(9) ) );
066200090506
066300101206         // -?Impostazione parametri ricevuti come parzializzazioni?
066400101206         //  ?nella stringa SQL?
066500101206         // - -?data dal?
066600090506         $Sql(17) = %replace( %char(DE4dti) : $Sql(17) :
066700090506                              %scan( '&LSE4dti' : $Sql(17) ) );
066800101206         // - -?data al?
066900090506         $Sql(17) = %replace( %char(DE4dtf) : $Sql(17) :
067000090506                              %scan( '&LSE4dtf' : $Sql(17) ) );
067100090506
067200090506         //? E S E M P I O : ?
067300090506         //
067400090506         // select AR8tmd,
067500090506         //        BLPksc, BLPrsm,
067600090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
067700090506         //        '* ', count(*)
067800090506         // from   FIAR800F   inner join   FNBLP00F
067900090506         // on     AR8aas = BLPaas
068000090506         //   and  AR8lnp = BLPlnp
068100090506         //   and  AR8nrs = BLPnrs
068200090506         //   and  AR8nsp = BLPnsp
068300090506         // where  AR8atb = ' '
068400090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
068500090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
068600090506         //   and  AR8tmd = 'STD1'
068700090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
068800090506         // UNION
068900090506         // select 'STD2' as AR8tmd,
069000090506         //        BLPksc, BLPrsm,
069100090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
069200090506         //        '* ', count(*)
069300090506         // from   FIAR800F   inner join   FNBLP00F
069400090506         // on     AR8aas = BLPaas
069500090506         //   and  AR8lnp = BLPlnp
069600090506         //   and  AR8nrs = BLPnrs
069700090506         //   and  AR8nsp = BLPnsp
069800090506         // where  AR8atb = ' '
069900090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
070000090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
070100090506         //   and  AR8tmd NOT in  ('STD1', 'KARN', 'EDK ', 'TITA',
070200090506         //                        'SAIT', 'ARCT', 'ESP1')
070300090506         //   and  AR8tmd NOT like 'LOG%'
070400090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
070500090506         // UNION
070600090506         // select AR8tmd,
070700090506         //        BLPksc, BLPrsm,
070800090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
070900090506         //        'C*', count(*)
071000090506         // from   FIAR800F   inner join   FNBLP00F
071100090506         // on     AR8aas = BLPaas
071200090506         //   and  AR8lnp = BLPlnp
071300090506         //   and  AR8nrs = BLPnrs
071400090506         //   and  AR8nsp = BLPnsp
071500090506         // where  AR8atb = ' '
071600090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
071700090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
071800090506         //   and (AR8tmd in  ('KARN', 'EDK ', 'TITA', 'SAIT', 'ARCT', 'ESP1')
071900090506         //    or  AR8tmd like 'LOG%')
072000090506         //   and  AR8trk in  ('C1', 'C2', 'C3', 'O1', 'O2')
072100090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
072200090506         // UNION
072300090506         // select AR8tmd,
072400090506         //        BLPksc, BLPrsm,
072500090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
072600090506         //        'Z1', count(*)
072700090506         // from   FIAR800F   inner join   FNBLP00F
072800090506         // on     AR8aas = BLPaas
072900090506         //   and  AR8lnp = BLPlnp
073000090506         //   and  AR8nrs = BLPnrs
073100090506         //   and  AR8nsp = BLPnsp
073200090506         // where  AR8atb = ' '
073300090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
073400090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
073500090506         //   and (AR8tmd in  ('SAIT')
073600090506         //    or  AR8tmd like 'LOG%')
073700090506         //   and  AR8trk in  ('Z1')
073800090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
073900090506         // UNION
074000090506         // select AR8tmd,
074100090506         //        BLPksc, BLPrsm,
074200090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
074300090506         //        'S*', count(*)
074400090506         // from   FIAR800F   inner join   FNBLP00F
074500090506         // on     AR8aas = BLPaas
074600090506         //   and  AR8lnp = BLPlnp
074700090506         //   and  AR8nrs = BLPnrs
074800090506         //   and  AR8nsp = BLPnsp
074900090506         // where  AR8atb = ' '
075000090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
075100090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
075200090506         //   and (AR8tmd in  ('SAIT')
075300090506         //    or  AR8tmd like 'LOG%')
075400090506         //   and  AR8trk in  ('S2', 'S3')
075500090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
075600090506         //
075700090506         // ORDER  BY  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
075800090506         // for    FETCH  only
075900090506         //
076000090506         //? N.B. ?
076100090506         // · Scarta    le LdV con BLPKSC = xxx9999
076200090506         // · Seleziona le LdV del periodo richiesto
076300090506         // · Seleziona le LdV con codice bolla = "1 ", "4 " o "5 "
076400090506
076500090506         clear  wSQL;
076600090506
076700130927         // »? 1 ?
076800101206         // -?DDT standard senza logo ("STD1"): trk T1, T2, C1?
076900090506         wSQL =       %trim($Sql( 1))
077000090506              + ' ' + %trim($Sql( 2))
077100090506              + ' ' + %trim($Sql( 4))
077200090506              + ' ' + %trim($Sql( 5))
077300090506              + ' ' + %trim($Sql( 9))
077400090506              + ' ' + %trim($Sql(10))
077500090506              + ' ' + %trim($Sql(11))
077600090506              + ' ' + %trim($Sql(12))
077700090506              + ' ' + %trim($Sql(13))
077800090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
077900090506              + ' ' + %trim($Sql(15))
078000090506              + ' ' + %trim($Sql(16))
078100090506              + ' ' + %trim($Sql(17))
078200090506              + ' ' + %trim($Sql(18))
078300090506              + ' ' + %trim($Sql(24))
078400090506              + ' ' + %trim($Sql(25))
078500090506
078600130927         // »? 2 ?
078700101206         // -?DDT standard  con  logo ("STD2"): trk T1, T2, C1?
078800090506              + ' ' + c_Union
078900090506              + ' ' + %trim($Sql( 1))
079000090506              + ' ' + %trim($Sql( 3))
079100090506              + ' ' + %trim($Sql( 4))
079200090506              + ' ' + %trim($Sql( 5))
079300090506              + ' ' + %trim($Sql( 9))
079400090506              + ' ' + %trim($Sql(10))
079500090506              + ' ' + %trim($Sql(11))
079600090506              + ' ' + %trim($Sql(12))
079700090506              + ' ' + %trim($Sql(13))
079800090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
079900090506              + ' ' + %trim($Sql(15))
080000090506              + ' ' + %trim($Sql(16))
080100090506              + ' ' + %trim($Sql(17))
080200090506              + ' ' + %trim($Sql(19));
080300101206         // -?Intabellamento loghi da scartare (NON di tipo "S2"):?
080400101206         // - -?di tipo "E1" (Esprinet)?
080500090506         For  xx = 1  To xE1;
080600090506           if  $AR8_E1(xx) <> *blank;
080700090506             if  xx = 1;
080800090507               wSQL += '''' + $AR8_E1(xx) + '''';
080900090506             else;
081000090507               wSQL += ', ''' + $AR8_E1(xx) + '''';
081100090506             endif;
081200090506           else;
081300090506             leave;
081400090506           endif;
081500090506         EndFor;
081600101206         // - -?di tipo "K1" (Karnak)?
081700090506         For  xx = 1  To xK1;
081800090506           if  $AR8_K1(xx) <> *blank;
081900090506             if  xx = 1   and   xE1 = *zero;
082000090507               wSQL += '''' + $AR8_K1(xx) + '''';
082100090506             else;
082200090507               wSQL += ', ''' + $AR8_K1(xx) + '''';
082300090506             endif;
082400090506           else;
082500090506             leave;
082600090506           endif;
082700090506         EndFor;
082800101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
082900090506         For  xx = 1  To xLx;
083000090506           if  $AR8_Lx(xx) <> *blank;
083100090506             if  xx = 1   and   xE1 = *zero
083200090506                          and   xK1 = *zero;
083300090507               wSQL += '''' + $AR8_Lx(xx) + '''';
083400090506             else;
083500090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
083600090506             endif;
083700090506           else;
083800090506             leave;
083900090506           endif;
084000090506         EndFor;
084100101206         // - -?di tipo "S1" (Std senza logo)?
084200090506         For  xx = 1  To xS1;
084300090506           if  $AR8_S1(xx) <> *blank;
084400090506             if  xx = 1   and   xE1 = *zero
084500090506                          and   xK1 = *zero
084600090506                          and   xLx = *zero;
084700090507               wSQL += '''' + $AR8_S1(xx) + '''';
084800090506             else;
084900090507               wSQL += ', ''' + $AR8_S1(xx) + '''';
085000090506             endif;
085100090506           else;
085200090506             leave;
085300090506           endif;
085400090506         EndFor;
085500101206         // - -?di tipo "00" (personalizzato - v. Arcte)?
085600090506         For  xx = 1  To x00;
085700090506           if  $AR8_00(xx) <> *blank;
085800090506             if  xx = 1   and   xE1 = *zero
085900090506                          and   xK1 = *zero
086000090506                          and   xLx = *zero
086100090506                          and   xS1 = *zero;
086200090507               wSQL += '''' + $AR8_00(xx) + '''';
086300090506             else;
086400090507               wSQL += ', ''' + $AR8_00(xx) + '''';
086500090506             endif;
086600090506           else;
086700090506             leave;
086800090506           endif;
086900090506         EndFor;
087000130927         // - -?di tipo "§§" (PDF allegato/i)?
087100130927         For  xx = 1  To x§§;
087200130927           if  $AR8_§§(xx) <> *blank;
087300130927             if  xx = 1   and   xE1 = *zero
087400130927                          and   xK1 = *zero
087500130927                          and   xLx = *zero
087600130927                          and   xS1 = *zero
087700130927                          and   x00 = *zero;
087800130927               wSQL += '''' + $AR8_§§(xx) + '''';
087900130927             else;
088000130927               wSQL += ', ''' + $AR8_§§(xx) + '''';
088100130927             endif;
088200130927           else;
088300130927             leave;
088400130927           endif;
088500130927         EndFor;
088600101206         // - -?"chiusura" stringa sql?
088700090507         wSQL += ')'
088800090506              + ' ' + %trim($Sql(24))
088900090506              + ' ' + %trim($Sql(25))
089000090506
089100130927         // »? 3 ?
089200101206         // -?Dettaglio in DDT Logistica o Sait      : trk C1, C2, C3, C4, C5?
089300101206         //               ?DDT Karnak, EDk o Titanedi: trk C1, C2, C3, O1, O2?
089400101206         //               ?DDT Arcte                 : trk C1, C2?
089500101206         //               ?DDT Esprinet              : trk C1, C2?
089600090506              + ' ' + c_Union
089700090506              + ' ' + %trim($Sql( 1))
089800090506              + ' ' + %trim($Sql( 2))
089900090506              + ' ' + %trim($Sql( 4))
090000090506              + ' ' + %trim($Sql( 6))
090100090506              + ' ' + %trim($Sql( 9))
090200090506              + ' ' + %trim($Sql(10))
090300090506              + ' ' + %trim($Sql(11))
090400090506              + ' ' + %trim($Sql(12))
090500090506              + ' ' + %trim($Sql(13))
090600090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
090700090506              + ' ' + %trim($Sql(15))
090800090506              + ' ' + %trim($Sql(16))
090900090506              + ' ' + %trim($Sql(17))
091000090506              + ' ' + %trim($Sql(20));
091100101206         // -?Intabellamento loghi da eleborare:?
091200101206         // - -?di tipo "E1" (Esprinet)?
091300090506         For  xx = 1  To xE1;
091400090506           if  $AR8_E1(xx) <> *blank;
091500090506             if  xx = 1;
091600090507               wSQL += '''' + $AR8_E1(xx) + '''';
091700090506             else;
091800090507               wSQL += ', ''' + $AR8_E1(xx) + '''';
091900090506             endif;
092000090506           else;
092100090506             leave;
092200090506           endif;
092300090506         EndFor;
092400101206         // - -?di tipo "K1" (Karnak)?
092500090506         For  xx = 1  To xK1;
092600090506           if  $AR8_K1(xx) <> *blank;
092700090506             if  xx = 1   and   xE1 = *zero;
092800090507               wSQL += '''' + $AR8_K1(xx) + '''';
092900090506             else;
093000090507               wSQL += ', ''' + $AR8_K1(xx) + '''';
093100090506             endif;
093200090506           else;
093300090506             leave;
093400090506           endif;
093500090506         EndFor;
093600101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
093700090506         For  xx = 1  To xLx;
093800090506           if  $AR8_Lx(xx) <> *blank;
093900090506             if  xx = 1   and   xE1 = *zero
094000090506                          and   xK1 = *zero;
094100090507               wSQL += '''' + $AR8_Lx(xx) + '''';
094200090506             else;
094300090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
094400090506             endif;
094500090506           else;
094600090506             leave;
094700090506           endif;
094800090506         EndFor;
094900101206         // - -?di tipo "00" (personalizzato - v. Arcte)?
095000090506         For  xx = 1  To x00;
095100090506           if  $AR8_00(xx) <> *blank;
095200090506             if  xx = 1   and   xE1 = *zero
095300090506                          and   xK1 = *zero
095400090506                          and   xLx = *zero
095500090506                          and   xS1 = *zero;
095600090507               wSQL += '''' + $AR8_00(xx) + '''';
095700090506             else;
095800090507               wSQL += ', ''' + $AR8_00(xx) + '''';
095900090506             endif;
096000090506           else;
096100090506             leave;
096200090506           endif;
096300090506         EndFor;
096400130927         // - -?di tipo "§§" (PDF allegato/i)?
096500130927         //    ?»»» NON COMPRESI QUI !!!! «««?
096600101206         // - -?"chiusura" stringa sql?
096700090507         wSQL += ')'
096800090506              + ' ' + %trim($Sql(21))
096900090506              + ' ' + %trim($Sql(24))
097000090506              + ' ' + %trim($Sql(25))
097100090506
097200130927         // »? 4 ?
097300101206         // -?Packing-List in DDT Logistica, Sait o Brumar: trk Z1?
097400090506              + ' ' + c_Union
097500090506              + ' ' + %trim($Sql( 1))
097600090506              + ' ' + %trim($Sql( 2))
097700090506              + ' ' + %trim($Sql( 4))
097800090506              + ' ' + %trim($Sql( 7))
097900090506              + ' ' + %trim($Sql( 9))
098000090506              + ' ' + %trim($Sql(10))
098100090506              + ' ' + %trim($Sql(11))
098200090506              + ' ' + %trim($Sql(12))
098300090506              + ' ' + %trim($Sql(13))
098400090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
098500090506              + ' ' + %trim($Sql(15))
098600090506              + ' ' + %trim($Sql(16))
098700090506              + ' ' + %trim($Sql(17))
098800090506              + ' ' + %trim($Sql(20));
098900101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
099000090506         For  xx = 1  To xLx;
099100090506           if  $AR8_Lx(xx) <> *blank;
099200090506             if  xx = 1;
099300090507               wSQL += '''' + $AR8_Lx(xx) + '''';
099400090506             else;
099500090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
099600090506             endif;
099700090506           else;
099800090506             leave;
099900090506           endif;
100000090506         EndFor;
100100101206         // - -?"chiusura" stringa sql?
100200090507         wSQL += ')'
100300090506              + ' ' + %trim($Sql(22))
100400090506              + ' ' + %trim($Sql(24))
100500090506              + ' ' + %trim($Sql(25))
100600090506
100700130927         // »? 5 ?
100800101206         // -?Fattura in DDT Logistica, Sait o Brumar: trk S2, S3?
100900090506              + ' ' + c_Union
101000090506              + ' ' + %trim($Sql( 1))
101100090506              + ' ' + %trim($Sql( 2))
101200090506              + ' ' + %trim($Sql( 4))
101300090506              + ' ' + %trim($Sql( 8))
101400090506              + ' ' + %trim($Sql( 9))
101500090506              + ' ' + %trim($Sql(10))
101600090506              + ' ' + %trim($Sql(11))
101700090506              + ' ' + %trim($Sql(12))
101800090506              + ' ' + %trim($Sql(13))
101900090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
102000090506              + ' ' + %trim($Sql(15))
102100090506              + ' ' + %trim($Sql(16))
102200090506              + ' ' + %trim($Sql(17))
102300090506              + ' ' + %trim($Sql(20));
102400101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
102500090506         For  xx = 1  To xLx;
102600090506           if  $AR8_Lx(xx) <> *blank;
102700090506             if  xx = 1;
102800090507               wSQL += '''' + $AR8_Lx(xx) + '''';
102900090506             else;
103000090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
103100090506             endif;
103200090506           else;
103300090506             leave;
103400090506           endif;
103500090506         EndFor;
103600101206         // - -?"chiusura" stringa sql?
103700090507         wSQL += ')'
103800090506              + ' ' + %trim($Sql(23))
103900090506              + ' ' + %trim($Sql(24))
104000090506              + ' ' + %trim($Sql(25))
104100130927
104200130927         // »? 6 ?
104300130927         // -?PDF allegati ("§PDF"): trk __?
104400130927              + ' ' + c_Union
104500130927              + ' ' + %trim($Sql( 1))
104600130927              + ' ' + %trim($Sql( 2))
104700130927              + ' ' + %trim($Sql( 4))
104800130927              + ' ' + %trim($Sql( 5))
104900130927              + ' ' + %trim($Sql( 9))
105000130927              + ' ' + %trim($Sql(10))
105100130927              + ' ' + %trim($Sql(11))
105200130927              + ' ' + %trim($Sql(12))
105300130927              + ' ' + %trim($Sql(13))
105400130927            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
105500130927              + ' ' + %trim($Sql(15))
105600130927              + ' ' + %trim($Sql(16))
105700130927              + ' ' + %trim($Sql(17))
105800130927              + ' ' + %trim($Sql(20));
105900130927         // - -?di tipo "§§" (PDF allegato/i)?
106000130927         For  xx = 1  To x§§;
106100130927           if  $AR8_§§(xx) <> *blank;
106200130927             if  xx = 1;
106300130927               wSQL += '''' + $AR8_§§(xx) + '''';
106400130927             else;
106500130927               wSQL += ', ''' + $AR8_§§(xx) + '''';
106600130927             endif;
106700130927           else;
106800130927             leave;
106900130927           endif;
107000130927         EndFor;
107100130927         // - -?"chiusura" stringa sql?
107200130927         wSQL += ')'
107300130927              + ' ' + %trim($Sql(24))
107400130927              + ' ' + %trim($Sql(25))
107500090506
107600130927         // »? 9 ?
107700101206         // -?Order by?
107800090506              + ' ' + c_OrderBy
107900090506
108000101206         // -?For fetch only?
108100090506              + ' ' + c_ForFetch;
108200090506
108300090506       ENDSR;
108400090506
108500090506       //--------------------------------------------------------------
108600090506       //?Apertura cursore.                                            ?
108700090506       //--------------------------------------------------------------
108800090506       BEGSR  sr_OpenCursor;
108900090506
109000101206         // -?Dichiarazione cursore?
109100090506         exec sql   prepare S1   from :wSQL;
109200090506         exec sql   declare C1   cursor for S1;
109300090506
109400101206         // -?Apertura del cursore?
109500090506         exec sql   open C1;
109600090507
109700090507         if  SQLcode < *zero;
109800090507           $EoF = *on;
109900140123           wErr = 4;
110000090507           exsr  sr_PrintErrSQL;
110100090507         endif;
110200090506
110300090506       ENDSR;
110400090506
110500090506       //--------------------------------------------------------------
110600090506       //?Chiusura cursore.                                            ?
110700090506       //--------------------------------------------------------------
110800090506       BEGSR  sr_CloseCursor;
110900090506
111000101206         // -?Registrazione totale ultimo cliente?
111100131220         exsr  sr_Wrt_TIPDL;
111200090506
111300101206         // -?Chiusura del cursore?
111400090506         exec sql   close C1;
111500090506
111600090506       ENDSR;
111700090506
111800090506       //--------------------------------------------------------------
111900090506       //?Lettura cursore.                                             ?
112000090506       //--------------------------------------------------------------
112100090506       BEGSR  sr_ReadCursor;
112200090506
112300090506         clear  wSQLds;
112400090506
112500090506         exec sql   fetch next   from C1   into :wSQLds;
112600090506
112700090506         select;
112800090507           when  SQLcode = 100;
112900090506             $EoF = *on;
113000090507           when  SQLcode < *zero;
113100140122             $EoF = *on;
113200140123             wErr = 4;
113300090507             exsr  sr_PrintErrSQL;
113400090506           other;
113500131007             if  SQLksc <> SavSQLksc   or   SQLtmd <> SavSQLtmd;
113600131007               exsr  sr_Write;
113700131007             endif;
113800131007             exsr  sr_Elab;
113900090506         endsl;
114000090506
114100090506       ENDSR;
114200131007
114300131007       //--------------------------------------------------------------
114400131007       //?Elaborazione singolo record da di totale                     ?
114500131007       //--------------------------------------------------------------
114600131007       BEGSR  sr_Elab;
114700131007
114800131007         // -?Reperimento tipologia di DDT?
114900131007         clear  xS1;
115000131007         clear  xE1;
115100131007         clear  xK1;
115200131007         clear  x00;
115300131007         clear  xLx;
115400131007         clear  x§§;
115500131007         xS1 = %lookup( SQLtmd : $AR8_S1 );
115600131007         xE1 = %lookup( SQLtmd : $AR8_E1 );
115700131007         xK1 = %lookup( SQLtmd : $AR8_K1 );
115800131007         x00 = %lookup( SQLtmd : $AR8_00 );
115900131007         xLx = %lookup( SQLtmd : $AR8_Lx );
116000131007         x§§ = %lookup( SQLtmd : $AR8_§§ );
116100131007
116200131007         //?Calcolo numero pagine per DDT in esame?
116300131007
116400131007         clear  wNrPag;
116500131007         clear  wResto;
116600131007
116700131007         SELECT;
116800131007
116900131007           // -?DDT standard con o senza logo (trk T1, T2, C1)?
117000131007           // - 42 righe x pagina
117100131007           WHEN  xS1                  > *zero   or
117200131007                (xE1+xK1+x00+xLx+x§§) = *zero;
117300131007             wNrPag = %div( SQlrec : 42 );
117400131007             wResto = %rem( SQLrec : 42 );
117500131007
117600131007           // -?DDT Logistica, Sait o Brumar?
117700131007           WHEN  xLx > *zero;
117800131007             select;
117900131007               // -?rec. di "corpo" (trk C1, C2, C3):?
118000131007               //  ?24 righe x pagina con   testata e con   piede?
118100131007               //  ?28 righe x pagina con   testata e senza piede?
118200131007               //  ?39 righe x pagina senza testata e senza piede?
118300131007               //  ?35 righe x pagina senza testata e con   piede?
118400131007               when  SQLtrk = 'C*'   and   SQLrec <= 24;
118500131007                 wNrPag  = 1;
118600131007               when  SQLtrk = 'C*'   and   SQLrec <= (28 + 35);
118700131007                 wNrPag  = 2;
118800131007               when  SQLtrk = 'C*'   and   SQLrec >  (28 + 35);
118900131007                 wResto  = SQLrec - (28 + 35);
119000131007                 wNrPag  = %div( wResto : 39 );
119100131007                 wResto  = %rem( wResto : 39 );
119200131007                 wNrPag += 2;
119300131007               // -?rec. di "packing-list" (trk Z1):?
119400131007               //  ?29 righe x pagina con   testata (NO piede)?
119500131007               //  ?40 righe x pagina senza testata (NO piede)?
119600131007               when  SQLtrk = 'Z1'   and   SQLrec <= 29;
119700131007                 wNrPag  = 1;
119800131007               when  SQLtrk = 'Z1'   and   SQLrec >  29;
119900131007                 wResto  = SQLrec - 29;
120000131007                 wNrPag  = %div( wResto : 40 );
120100131007                 wResto  = %rem( wResto : 40 );
120200131007                 wNrPag += 1;
120300131007               // -?rec. di "fattura" (trk S2, S3):?
120400131007               //  ?26 righe x pagina con   testata e con   piede?
120500131007               //  ?29 righe x pagina con   testata e senza piede?
120600131007               //  ?42 righe x pagina senza testata e senza piede?
120700131007               //  ?39 righe x pagina senza testata e con   piede?
120800131007               when  SQLtrk = 'S*'   and   SQLrec <= 26;
120900131007                 wNrPag  = 1;
121000131007               when  SQLtrk = 'S*'   and   SQLrec <= (29 + 39);
121100131007                 wNrPag  = 2;
121200131007               when  SQLtrk = 'S*'   and   SQLrec >  (29 + 39);
121300131007                 wResto  = SQLrec - (29 + 39);
121400131007                 wNrPag  = %div( wResto : 42 );
121500131007                 wResto  = %rem( wResto : 42 );
121600131007                 wNrPag += 2;
121700131007             endsl;
121800131007
121900131007           // -?DDT Karnak, EDk o Titanedi?
122000131007           //  ?19 righe x pagina (omaggi ESCLUSI)?
122100131007           WHEN  xK1 > *zero;
122200131007             if  SQLtrk = 'C*';
122300131007               wNrPag  = %div( SQLrec : 19 );
122400131007               wResto  = %rem( SQLrec : 19 );
122500131007             endif;
122600131007
122700131007           // -?DDT Arcte?
122800131007           WHEN  x00 > *zero;
122900131007             select;
123000131007               // -?rec. di "corpo" (trk C1, C2):?
123100131007               //  ?14 righe x pagina con   testata e con   piede?
123200131007               //  ?29 righe x pagina con   testata e senza piede?
123300131007               //  ?40 righe x pagina senza testata e senza piede?
123400131007               //  ?36 righe x pagina senza testata e con   piede?
123500131007               when  SQLtrk = 'C*'   and   SQLrec <= 25;
123600131007                 wNrPag  = 1;
123700131007               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 36);
123800131007                 wNrPag  = 2;
123900131007               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 36);
124000131007                 wResto  = SQLrec - (29 + 36);
124100131007                 wNrPag  = %div( wResto : 40 );
124200131007                 wResto  = %rem( wResto : 40 );
124300131007                 wNrPag += 2;
124400131007             endsl;
124500131007
124600131007           // -?DDT Esprinet?
124700131007           WHEN  xE1 > *zero;
124800131007             select;
124900131007               // -?rec. di "corpo" (trk C1, C2)?
125000131007               //  ?18 righe x pagina con   testata e con   piede?
125100131007               //  ?29 righe x pagina con   testata e senza piede?
125200131007               //  ?39 righe x pagina senza testata e senza piede?
125300131007               //  ?28 righe x pagina senza testata e con   piede?
125400131007               when  SQLtrk = 'C*'   and   SQLrec <= 18;
125500131007                 wNrPag  = 1;
125600131007               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 28);
125700131007                 wNrPag  = 2;
125800131007               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 28);
125900131007                 wResto  = SQLrec - (29 + 28);
126000131007                 wNrPag  = %div( wResto : 39 );
126100131007                 wResto  = %rem( wResto : 39 );
126200131007                 wNrPag += 2;
126300131007             endsl;
126400131007
126500131007           // -?PDF allegato/i?
126600131007           WHEN  x§§ > *zero;
126700131007             wNrPag += SQLrec;
126800131007             wResto  = *zero;
126900131007
127000131007         ENDSL;
127100131007
127200131007         if  wResto > *zero;
127300131007           wNrPag += 1;
127400131007         endif;
127500131007
127600131007         // -?Sistemazione dei totali delle sole righe di dettaglio  SE ?
127700131007         //  ?reperite righe di packing-list e/o fattura?
127800131007         if  xLx > *zero         and
127900131007            (SQLtrk = 'Z1'       or   SQLtrk = 'S*')      and
128000131007            (SQLtmd = SavSQLtmd  and  SQLksc = SavSQLksc  and
128100131007             SQLaas = SavSQLaas  and  SQLlnp = SavSQLlnp  and
128200131007             SQLnrs = SavSQLnrs  and  SQLnsp = SavSQLnsp);
128300131007           wNrPag += SavNrPag;
128400131007           $_ddt(SavXX) -= 1;
128500131007           if  SavXX = 6  and  SavNrPag >= 6;
128600131007             wPagOltre5 -= SavNrPag;
128700131007           endif;
128800131007         endif;
128900131007
129000131007         // -?Aggiornam. del totale DDT per n° pagine (relativo al cliente)?
129100131007         select;
129200131007           when  x§§ > *zero;
129300131007             exsr  sr_ChkPkgLst;
129400131007             clear  xx;
129500131007             Sped§§ += 1;
129600131007             nPDF§§ += wNrPag;
129700131007           when  wNrPag <= *zero;
129800131007             xx  = 1;
129900131007           when  wNrPag >= 6;
130000131007             xx  = 6;
130100131007             wPagOltre5 += wNrPag;
130200131007           other;
130300131007             xx  = wNrPag;
130400131007         endsl;
130500131007         if  x§§ = *zero;
130600131007           $_ddt(xx) += 1;
130700131007         endif;
130800131007
130900131007         // -?Memorizza i dati appena elaborati?
131000131007         //  ?(questo nr.pag. potrebbe essere da togliere da questo conteggio?
131100131007         //  ?e da aggiungere a quello successivo - se seguisse un trk "Z1"?
131200131007         //  ?con una packing-list nella stessa LdV, o se seguisse un trk?
131300131007         //  ?"S*" con una fattura nella stessa LdV)?
131400131007         clear  wSavSQLds;
131500131007         %subst( wSavSQLds : 1 : %len( wSQlds ) ) =  wSQLds;
131600131007         SavNrPag  =  wNrPag;
131700131007         SavXX     =  xx;
131800131007
131900131007       ENDSR;
132000090506
132100090506       //--------------------------------------------------------------
132200131220       //?Registrazione record nel file TIPL00F o nel wrkf WFPDL00F    ?
132300090506       //--------------------------------------------------------------
132400090506       BEGSR  sr_Write;
132500090506
132600131007         // -?Scrittura a rottura di cliente / tipo modulo DDT?
132700131007         IF  SavSQLtmd <> *blank   and   SavSQLksc <> *zero;
132800131007
132900131007           if  SavSQLksc <> SQLksc  or
133000131009               %lookup( SavSQLtmd : $AR8_§§ ) = *zero;
133100131220             exsr  sr_Wrt_TIPDL;
133200131007           endif;
133300131007
133400131007           clear  $_ddt;
133500131007           clear  wPagOltre5;
133600131007           if  SavSQLksc <> SQLksc;
133700131007             clear  Sped§§;
133800131007             clear  nPDF§§;
133900131007             clear  $_Loghi;
134000131007             clear  $_Sped§§;
134100131007             clear  $_nPdf§§;
134200131007           endif;
134300131007
134400131007         ENDIF;
134500131007
134600131007         SavSQLtmd = SQLtmd;
134700131007         SavSQLksc = SQLksc;
134800131007
134900131007         // -?decodifica nuovo P.O. (a rottura dello stesso)?
135000131007         WWWfil = %div( SQLksc : 10000 );
135100131007         if  WWWfil <> ORGfil;
135200131007           chain  (WWWfil)  AZORG;
135300131007           if  NOT  %found(AZORG01L)
135400131007               or   ORGfva <> *blank;
135500131007             clear  ORGfl3;
135600131007             clear  ORGcar;
135700131007           endif;
135800131007         endif;
135900090506
136000090506       ENDSR;
136100090506
136200090506       //--------------------------------------------------------------
136300131220       //?Scrittura del singolo record nel file TIPDL00F o nel WrkF    ?
136400090506       //--------------------------------------------------------------
136500131220       BEGSR  sr_Wrt_TIPDL;
136600090506
136700131007         // -?Nuovo Cliente  o  Nuovo Tipo Modulo?
136800140115         clear  TIPDL000;
136900090506
137000131220         PDLeut  = DUTute;
137100131220         PDLedt  = wDate;
137200131220         PDLdti  = DE4dti;
137300131220         PDLdtf  = DE4dtf;
137400131220         PDLcdi  = ORGfl3;
137500131220         PDLcar  = ORGcar;
137600131220         PDLfil  = ORGfil;
137700131220         PDLksc  = SavSQLksc;
137800131220         PDLrsm  = SavSQLrsm;
137900131220         PDLtmd  = SavSQLtmd;
138000131220         PDLn1p  = $_ddt(1);
138100131220         PDLn2p  = $_ddt(2);
138200131220         PDLn3p  = $_ddt(3);
138300131220         PDLn4p  = $_ddt(4);
138400131220         PDLn5p  = $_ddt(5);
138500131220         PDLnop  = $_ddt(6);
138600131220         PDLpo5  = wPagOltre5;
138700131007         // -?PDF allegati?
138800131220         //PDLpdfSp  = Sped§§;
138900131220         //PDLpdfNr  = nPDF§§;
139000131009         // -?Impostazione delle spedizioni con anche PDF e sottrazione?
139100131009         //  ?delle stesse dal totale di quelle cn PDF (per ottenere il?
139200131009         //  ?numero di quelle con SOLO PDF)?
139300131007         yy = %lookup( SavSQLtmd : $_Loghi );
139400131007         if  yy > *zero;
139500131220           PDLpdfSp  = $_Sped§§(yy);
139600131220           PDLpdfNr  = $_nPdf§§(yy);
139700131007           Sped§§   -= $_Sped§§(yy);
139800131007           nPDF§§   -= $_nPdf§§(yy);
139900131007         endif;
140000090506
140100140115         //_______________
140200140115         WRITE   TIPDL000;
140300140115         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
140400131007
140500131007         // -?PDF residui (NON abbinati ad altre packing-list)?
140600131007         IF  SavSQLksc <> SQLksc  and
140700131007             Sped§§     > *zero;
140800131007
140900131007           For  yy = 1  To  %elem($_Loghi);
141000131007
141100131007             if  $_Loghi(yy) = *blank;
141200131007               leave;
141300131007             endif;
141400131009             //if  %subst( $_Loghi(yy) : 1 : 1 ) <> '§'   or
141500131009             if  %lookup( $_Loghi(yy) : $AR8_§§ ) = *zero  or
141600131007                 $_Sped§§(yy) <= *zero;
141700131007               iter;
141800131007             endif;
141900131007
142000140115             clear  TIPDL000;
142100131007
142200131220             PDLeut  = DUTute;
142300131220             PDLedt  = wDate;
142400131220             PDLdti  = DE4dti;
142500131220             PDLdtf  = DE4dtf;
142600131220             PDLcdi  = ORGfl3;
142700131220             PDLcar  = ORGcar;
142800131220             PDLfil  = ORGfil;
142900131220             PDLksc  = SavSQLksc;
143000131220             PDLrsm  = SavSQLrsm;
143100131220             PDLtmd  = $_Loghi(yy);
143200131220             //PDLn1p  = *zero;
143300131220             //PDLn2p  = *zero;
143400131220             //PDLn3p  = *zero;
143500131220             //PDLn4p  = *zero;
143600131220             //PDLn5p  = *zero;
143700131220             //PDLnop  = *zero;
143800131220             //PDLpo5  = *zero;
143900131220             PDLpdfSp  = $_Sped§§(yy);
144000131220             PDLpdfNr  = $_nPdf§§(yy);
144100131007
144200140115             //_______________
144300140115             WRITE   TIPDL000;
144400140115             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
144500131007
144600131007           EndFor;
144700131007
144800131007         ENDIF;
144900090506
145000090506       ENDSR;
145100131007
145200131007       //--------------------------------------------------------------
145300131007       //?Verifica esistenza di packing-list oltre al PDF              ?
145400131007       //--------------------------------------------------------------
145500131007       BEGSR  sr_ChkPkgLst;
145600131007
145700131007         // -?Reperimento eventuale packing-list (oltre al PDF)?
145800131009         clear  wSql2;
145900131009         wSql2 = 'select AR8tmd +
146000131009                    from ' + %trimr( $Libr(2) ) + '/FIAR800F +
146100131009                   where AR8aas = ' + %trim( %editc( SQLaas : '3' ) ) + ' +
146200131009                     and AR8lnp = ' + %trim( %editc( SQLlnp : '3' ) ) + ' +
146300131009                     and AR8nrs = ' + %trim( %editc( SQLnrs : '3' ) ) + ' +
146400131009                     and AR8nsp = ' + %trim( %editc( SQLnsp : '3' ) ) + ' +
146500131009                     and AR8tmd NOT in (';
146600131009         For  xx = 1  To x§§;
146700131009           if  $AR8_§§(xx) <> *blank;
146800131009             if  xx = 1;
146900131009               wSql2 += '''' + $AR8_§§(xx) + '''';
147000131009             else;
147100131009               wSql2 += ', ''' + $AR8_§§(xx) + '''';
147200131009             endif;
147300131009           else;
147400131009             leave;
147500131009           endif;
147600131009         EndFor;
147700131009         wSql2 += ') ' +
147800131009                  c_ForFetch;
147900131009
148000131009         // -?Dichiarazione cursore?
148100131009         exec sql   prepare S2   from :wSql2;
148200131009         exec sql   declare C2   cursor for S2;
148300131009
148400131009         // -?Apertura del cursore?
148500131009         exec sql   open C2;
148600131009         if  SQLcode < *zero;
148700131009           $EoF = *on;
148800140123           wErr = 4;
148900131009           exsr  sr_PrintErrSQL;
149000131009         endif;
149100131009
149200131009         // -?Lettura del cursore?
149300131009         clear  wAR8tmd;
149400131009         exec sql   fetch next   from C2   into :wAR8tmd;
149500131007
149600131007         clear  yy;
149700131007         select;
149800131007           //// -?NON reperita packing-list (oltre al PDF)?
149900131007           ////  ?(si memorizzano questi PDF)?
150000131009           //when  SQLcode = 100;
150100131007           // -?Errore in elaborazione: si chiude il pgm.?
150200131009           when  SQLcode < *zero;
150300131007             $EoF = *on;
150400140123             wErr = 4;
150500140123             exsr  sr_PrintErrSQL;
150600131007           // -?Si conteggiano i PDF insieme alla packing-list?
150700131007           other;
150800131009             if  SQLcode = 100;
150900131007               yy = %lookup( SQLtmd : $_Loghi );
151000131007             else;
151100131007               yy = %lookup( wAR8tmd : $_Loghi );
151200131007             endif;
151300131007             if  yy = *zero;
151400131007               yy = %lookup( *blank : $_Loghi );
151500131007             endif;
151600131007             if  yy > *zero;
151700131009               if  SQLcode = 100;
151800131007                 $_Loghi(yy) = SQLtmd;
151900131007               else;
152000131007                 $_Loghi(yy) = wAR8tmd;
152100131007               endif;
152200131007               $_Sped§§(yy) += 1;
152300131007               $_nPdf§§(yy) += SQLrec;
152400131007             endif;
152500131007         endsl;
152600131007
152700131009         // -?Chiusura del cursore?
152800131009         exec sql   close   C2;
152900131007
153000131007       ENDSR;
153100090507
153200090507       //--------------------------------------------------------------
153300090507       //?Stampa segnalazione dell'errore rilevato via SQL             ?
153400090507       //--------------------------------------------------------------
153500090507       BEGSR  sr_PrintErrSQL;
153600090507
153700101206         // -?Stampa del Dump?
153800090507         Dump(A);
153900090507
154000101206         // -?Stampa del Job-Log?
154100090507         Qcmd = 'DSPJOBLOG job(*) output(*print)';
154200140123         exsr  sr_ExecCmd;
154300140123
154400140123         // -?Invio errore via e-mail?
154500140123         exsr  sr_SendEmail;
154600090507
154700101206         // -?Chiusura del programma?
154800090507         exsr  sr_RoutEnd;
154900090507
155000090507       ENDSR;
155100140123
155200140123       //--------------------------------------------------------------
155300140123       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
155400140123       //--------------------------------------------------------------
155500140123       BEGSR  sr_SendEmail;
155600140123
155700140123         // -?Override al file di stampa ed apertura dello stesso?
155800140123         if  NOT  %open(PrtEMAIL);
155900140123           §CM1mitt = %trim(§MRADmitt);
156000140123           §CM1dst  = %trim(§MRADdest);
156100140123           §CM1tips = §MRAdreg;
156200140123           §CM1var  = '*OBJM*' + §MRADdes;
156300140123           §CM1idp  = §MRADidPro;
156400140123           Qcmd = c_CmdOvrPrtF
156500140123                + ' outq(' + %trim(§MRADoutqI) + ')'
156600140123                + ' usrdfndta(''' + TRTCM1ds + ''')';
156700140123           exsr  sr_ExecCmd;
156800140123           open  PrtEMAIL;
156900140123           except  PRTtxt;
157000140123         endif;
157100140123
157200140123         // -?Preparazione e-Mail per il singolo errore rilevato?
157300140123         Select;
157400140123
157500140123           // -?Errore rilevato nel reperimento dell'ultima data?
157600140123           //  ?già estratta nel file TIPDL00F?
157700140123           When  wErr = 1;
157800140123             if  *inOF;
157900140123               except  PRTtxt;
158000140123               *inOF = *off;
158100140123             endif;
158200140123             except  PRTerrPDL;
158300140123
158400140123           // -?Avviso di Tipo Stampa DDT NON previsto?
158500140123           When  wErr = 2;
158600140123             if  *inOF;
158700140123               except  PRTtxt;
158800140123               *inOF = *off;
158900140123             endif;
159000140123             except  PRTerrTSD;
159100140123
159200140123           // -?Avviso di Tipo Modulo per Packing-List NON previsto?
159300140123           When  wErr = 3;
159400140123             if  *inOF;
159500140123               except  PRTtxt;
159600140123               *inOF = *off;
159700140123             endif;
159800140123             except  PRTerrAR8;
159900140123
160000140123           // -?Errore rilevato nell'estrazione dati via SQL dai?
160100140123           //  ?file FNBLP, FIAR8, ...?
160200140123           When  wErr = 4;
160300140123             clear  ds_Temp;
160400140123             ds_Temp = wSQL;
160500140123             if  *inOF;
160600140123               except  PRTtxt;
160700140123               *inOF = *off;
160800140123             endif;
160900140123             except  PRTerr1;
161000140123             For  xx = 1  To  %elem($Temp);
161100140123               if  $Temp(xx) = *blank;
161200140123                 leave;
161300140123               endif;
161400140123               if  *inOF;
161500140123                 except  PRTtxt;
161600140123                 *inOF = *off;
161700140123               endif;
161800140123               except  PRTerr2;
161900140123             EndFor;
162000140123
162100140123         EndSl;
162200140123
162300140123       ENDSR;
162400140123
162500140123       //--------------------------------------------------------------
162600140123       //?Esecuzione del comando (già impostato).                      ?
162700140123       //--------------------------------------------------------------
162800140123       BEGSR  sr_ExecCmd;
162900140123
163000140123         clear Qcap0100;
163100140123         Qcabcsdh = *off;
163200140123         Qcapa    = *off;
163300140123         Qcacmdss = *off;
163400140123         Qcaerved = *allX'00';
163500140123
163600140123         clear Qusec;
163700140123         Qusbprv  = %size(Qusec);
163800140123
163900140123         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
164000140123                           %size(Qcap0100) : 'CPOP0100' : *omit :
164100140123                           0 : 0 : Qusec);
164200140123
164300140123         //if  Qusei <> *blank;
164400140123         //  ...;
164500140123         //endif;
164600140123
164700140123       ENDSR;
164800090507
164900090507       //--------------------------------------------------------------
165000090507       //?Operazioni finali.                                           ?
165100090507       //--------------------------------------------------------------
165200090507       BEGSR  sr_RoutEnd;
165300090507
165400140123         // -?Chiusura del file di stampa e cancellazione override?
165500140123         if  %open(prtEMAIL);
165600140123           close  PrtEMAIL;
165700140123           Qcmd = c_CmdDltOvr;
165800140123           exsr  sr_ExecCmd;
165900090507         endif;
166000101206
166100140115         // -?Chiusura file?
166200140115         close  TIPDL00F;
166300090507
166400101206         // -?"Chiusura" pgm?
166500090507         return;
166600090507
166700090507       ENDSR;
166800090507
166900090507      /end-free
167000090507
167100090507       //--------------------------------------------------------------
167200090507       //?S P O O L - F I L E S                                        ?
167300090507       //--------------------------------------------------------------
167400090507
167500140123     oPrtEMAIL  e            PRTtxt            1
167600090507     o                       RSUT
167700140115     o                                        +   6 'LISTA ERRORI RILE-
167800140115     o                                              VATI IN FASE DI CA-
167900140115     o                                              LCOLO N° PAGINE DI-
168000140115     o                                               PACKING-LIST IN L-
168100140115     o                                              DV'
168200090507     o                       SDSpgm           +   6
168300090507     o                       wDate         y  +   3
168400090507     o          e            PRTtxt      1
168500090507     o                       KNSIF
168600090507     o                       KNMUS            +   1
168700140115     o                                        +   5 '------------------
168800090507     o                                              -------------------
168900090507     o                                              -------------------
169000090507     o                                              -------------------
169100140115     o                                              --'
169200090507     o                                        +   6 'Pag.'
169300090507     o                       Page          z  +   0
169400090507     o                       wTime            +   5 '  :  :  '
169500140122      *
169600140122     o          e            PRTerrPDL   2
169700140122     o                                              'Rilevato ERRORE n-
169800140122     o                                              el reperimento del-
169900140122     o                                              l''ultima data già-
170000140122     o                                               estratta.'
170100140122     o          e            PRTerrPDL   1
170200140123     o                                              'ANNULLATO il calc-
170300140123     o                                              olo del n° pagine -
170400140123     o                                              di packing-list in-
170500140123     o                                               LdV per cliente.'
170600140122      *
170700090507     o          e            PRTerrTSD   2
170800090507     o                                              'Reperito nuovo -
170900090507     o                                              "Tipo Stampa DDT" -
171000090507     o                                              in tab. "TSD":'
171100090507     o                       TBEke1           +   1
171200090507     o          e            PRTerrTSD   1
171300090507     o                                              'AVVISO: le righe -
171400090507     o                                              di tale tipo nelle-
171500090507     o                                               packing-list verr-
171600090507     o                                              anno contate come -
171700090507     o                                              "STANDARD CON LOGO-
171800090507     o                                              ".'
171900140122      *
172000090507     o          e            PRTerrAR8   2
172100090507     o                                              'Reperito nuovo -
172200090507     o                                              "Tipo Stampa DDT" -
172300090507     o                                              in tab. "AR8":'
172400090507     o                       §AR8tsd          +   1
172500090507     o                                        +   1 'per il logo'
172600090507     o                       TBEke1           +   1
172700090507     o          e            PRTerrAR8   1
172800090507     o                                              'AVVISO: le righe -
172900090507     o                                              della packing-list-
173000090507     o                                               relativa alla spe-
173100090507     o                                              dizione'
173200090507     o                       SQLlnp           +   1
173300090507     o                                        +   0 '/'
173400090507     o                       SQLnrs        z  +   0
173500090507     o                                        +   0 '/'
173600090507     o                       SQLnsp        z  +   0
173700090507     o                                        +   1 'anno'
173800090507     o                       SQLaas        z  +   1
173900090507     o          e            PRTerrAR8   1
174000090507     o                                        +   3 'sono contate come-
174100090507     o                                               "STANDARD CON LOG-
174200090507     o                                              O".'
174300140122      *
174400090507     o          e            PRTerr1     2
174500090507     o                                              'RILEVATO ERRORE: +
174600090507     o                                               SQLCODE'
174700090507     o                       SQLcode       k  +   1
174800090507     o                                        +   1 'SQLSTATE'
174900090507     o                       SQLstate         +   1
175000090507     o                                        +   1 'DURANTE L''ESECUZ-
175100090507     o                                              IONE DEL SEGUENTE -
175200090507     o                                              COMANDO SQL:'
175300090507     o          e            PRTerr1     0  1
175400090507     o                                              'RILEVATO ERRORE: +
175500090507     o                                               SQLCODE'
175600090507     o                       SQLcode       k  +   1
175700090507     o                                        +   1 'SQLSTATE'
175800090507     o                       SQLstate         +   1
175900090507     o          e            PRTerr2     1
176000090507     o                       $Temp(xx)        +   3
176100060327
176200090507       //--------------------------------------------------------------
176300090507       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
176400090507       //--------------------------------------------------------------
176500090507
176600101206** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie di Sede e di Fil.?
176700101206SETRAS  GAITRAAZM FILTRA201
176800101206AS888   GAITRAAZP FILTRAPRD
176900101206** -?$Sql:?Stringa SQL da eseguire?----------------------------------*
177000060331SELECT BLPksc, BLPrsm,                                                  1
177100060331       AR8tmd,                                                          2 a1
177200060331       'STD2' as AR8tmd,                                                3 a2
177300060330       AR8aas, AR8lnp, AR8nrs, AR8nsp,                                  4
177400060330       '* ', count(*)                                                   5 b1
177500060330       'C*', count(*)                                                   6 b2
177600060330       'Z1', count(*)                                                   7 b3
177700060330       'S*', count(*)                                                   8 b4
177800101206FROM   &LibFil1  /FIAR800F   INNER JOIN   &LibFil2  /FNBLP00F           9
177900060330ON     AR8aas = BLPaas                                                 10
178000060330  and  AR8lnp = BLPlnp                                                 11
178100060330  and  AR8nrs = BLPnrs                                                 12
178200060330  and  AR8nsp = BLPnsp                                                 13
178300090506  and  AR8atb = BLPatb                          --- NO ---   => libero 14
178400130927WHERE  AR8atb = ' '                                                    15
178500060330  and  substr(digits(BLPksc), 4, 4) <> '9999'                          16
178600060330  and  (BLPaas*10000+BLPmgs)  between  &LSE4dti and &LSE4dtf           17
178700060330  and  AR8tmd = 'STD1'                                                 18 c1
178800090507  and  AR8tmd NOT in (                                                 19 c2
178900090507  and  AR8tmd in (                                                     20 c3
179000090506  and (AR8trk like 'C%'  or  AR8trk like 'O%')                         21 d1
179100090512  and  AR8trk in ('Z1')                                                22 d2
179200090512  and  AR8trk in ('S2', 'S3')                                          23 d3
179300090507  and  BLPcbo in ('1 ', '4 ', '5 ')                                    24
179400090506GROUP  BY  BLPksc, BLPrsm, AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp      25
