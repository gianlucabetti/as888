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
002600101206     /*CMD ovrdbf file(WFPDL00F) tofile(GAITRAAZP/WFPDL00F) +
002700101206     /*CMD        ovrscope(*calllvl)
002800130927     /*CMD ovrdbf file(WFPDL10F) tofile(GAITRAAZP/WFPDL10F) +
002900130927     /*CMD        ovrscope(*calllvl)
003000101206     /*END dltovr file(FIAR800F) lvl(*)
003100101206     /*END dltovr file(FNBLP00F) lvl(*)
003200101206     /*END dltovr file(WFPDL00F) lvl(*)
003300130927     /*END dltovr file(WFPDL10F) lvl(*)
003400101206     /*END
003500101206
003600101206       //--------------------------------------------------------------
003700101206       //?Specifiche di controllo.                                     ?
003800101206       //--------------------------------------------------------------
003900060327
004000060327     h decedit('0,') datedit(*dmy/) option(*nodebugio)
004100060327     h alwnull(*inputonly)
004200101206     h dftactgrp(*no) bnddir('UBBNDDIR')
004300060327
004400090430       //--------------------------------------------------------------
004500090430       //?Dichiarazione file.                                          ?
004600090430       //--------------------------------------------------------------
004700090430
004800101206       // -?Organigramma?
004900060331     fAZORG01L  if   e           k disk
005000101206       // -?Tabelle?
005100090430     fTNTBE01L  if   e           k disk
005200090430
005300101206       // -?Work-File Statistica n° Pagine di DDT in LdV?
005400101206     fWFPDL00F  o    e             disk    extfile(wLibFile)
005500101206     f                                     usropn
005600090507
005700101206       // -?Stampa segnalazioni di errore?
005800090507     fQSYSPRT   o    f  132        printer usropn
005900090507     f                                     oflind(*inOF)
006000060327
006100090430       //--------------------------------------------------------------
006200090430       //?Definizione costanti.                                        ?
006300090430       //--------------------------------------------------------------
006400090430
006500101206       // -?Costanti per stringa SQL?
006600090430     d c_Union         c                   const('UNION')
006700090430     d c_OrderBy       c                   const('ORDER BY +
006800060330     d                                            BLPksc, +
006900060331     d                                            BLPrsm, +
007000060331     d                                            AR8tmd, +
007100060330     d                                            AR8aas, +
007200060330     d                                            AR8lnp, +
007300060330     d                                            AR8nrs, +
007400130927     d                                            AR8nsp')
007500090430     d c_ForFetch      c                   const('FOR FETCH ONLY')
007600101206
007700101206       // -?Comandi da eseguire?
007800090507     d c_OvrPrtF       c                   const('OVRPRTF +
007900101206     d                                            file(QSYSPRT) +
008000101206     d                                            usrdta(''FNLSE4+
008100090507     d                                                     *ERR'')')
008200090507     d c_DltOvr        c                   const('DLTOVR +
008300101206     d                                            file(QSYSPRT)')
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
015600101206       // -?Parametri per Reperimento dati utente?
015700060331     d TIBS34ds      e ds                  inz
015800090430
015900101206       // -?Ds di comodo per la definizione dei campi estratti via SQL?
016000131009     d FIAR8ds       e ds                  extname(FIAR800F)
016100131009     d                                     based(nullPtr)
016200131009     d FNBLPds       e ds                  extname(FNBLP00F)
016300131009     d                                     based(nullPtr)
016400090506
016500101206       // -?Tab. AR8 = Tipi Modulo DDT x file FIAR8?
016600090506     d dAR8          e ds                  inz
016700090430
016800101206       // -?Dati estratti via SQL?
016900060327     d wSQLds          ds                  inz
017000060327     d  SQLksc                             inz  like(BLPksc)
017100060327     d  SQLrsm                             inz  like(BLPrsm)
017200060331     d  SQLtmd                             inz  like(AR8tmd)
017300060327     d  SQLaas                             inz  like(AR8aas)
017400060327     d  SQLlnp                             inz  like(AR8lnp)
017500060327     d  SQLnrs                             inz  like(AR8nrs)
017600060327     d  SQLnsp                             inz  like(AR8nsp)
017700060327     d  SQLtrk                        2    inz
017800060327     d  SQLrec                        9  0 inz
017900090430
018000060328     d wSavSQLds       ds                  inz
018100090506     d  SavSQLksc                          inz  like(SQLksc)
018200090506     d  SavSQLrsm                          inz  like(SQLrsm)
018300090506     d  SavSQLtmd                          inz  like(SQLtmd)
018400090506     d  SavSQLaas                          inz  like(SQLaas)
018500090506     d  SavSQLlnp                          inz  like(SQLlnp)
018600090506     d  SavSQLnrs                          inz  like(SQLnrs)
018700090506     d  SavSQLnsp                          inz  like(SQLnsp)
018800090506     d  SavSQLtrk                          inz  like(SQLtrk)
018900090506     d  SavSQLrec                          inz  like(SQLrec)
019000060328     d  SavNrPag                           inz  like(wNrPag)
019100060328     d  SavXX                              inz  like(xx)
019200090507
019300101206       // -?Schiera per "scomposizione" stringa SQL in stampa?
019400090507     d ds_Temp         ds          5000    inz
019500090507     d   $Temp                 1   5000    inz  dim(50)
019600101206
019700101206       // -?Ridefinizione elenco librerie in elaborare le tabelle?
019800101206     d ds_Libr         ds                  inz
019900131007     d   $Libr                       10    dim(c_NrLibr) inz
020000090430
020100090430       //--------------------------------------------------------------
020200090430       //?Definizione variabili globali.                               ?
020300090430       //--------------------------------------------------------------
020400090430
020500101206       // -?Flags booleani?
020600090430     d $EoF            s               n   inz
020700090430     d $Err            s               n   inz
020800090430
020900101206       // -?Indici di schiera?
021000060327     d xx              s              3  0 inz
021100131007     d yy              s              3  0 inz
021200090506     d xS1             s              3  0 inz
021300090506     d*// xS2             s              3  0 inz
021400090506     d xE1             s              3  0 inz
021500090506     d xK1             s              3  0 inz
021600090506     d x00             s              3  0 inz
021700090506     d xLx             s              3  0 inz
021800130927     d x§§             s              3  0 inz
021900090430
022000131009       // -?Stringhe SQL da eseguire?
022100130927     d wSQL            s           7000    inz  varying
022200131009     d wSql2           s           1000    inz  varying
022300090430
022400101206       // -?Campi per controllo "rotture di livello"?
022500090507     d WWWfil          s                   inz  like(ORGfil)
022600090430
022700101206       // -?Campi per QCMDEXC?
022800090507     d Qcmd            s            128    inz  varying
022900101206
023000101206       // -?Nome del sistema?
023100101206     d currSysNeta     s              8a   inz
023200101206
023300101206       // -?Nome esteso Libreria/File dei 2 file tabella?
023400101206     d wLibFile        s             21a   inz
023500110128
023600110128       // -?Totali per cliente?
023700110128     d wPagOltre5      s              9  0 inz
023800090430
023900101206       // -?Campi di comodo?
024000101206     d w_iSystem       s              1  0 inz
024100101206     d w_SisInf        s              3  0 inz
024200090507     d wTime           s              6  0 inz
024300090507     d wDate           s              8  0 inz
024400090507     d wDataLim        s               d   inz  datfmt(*iso)
024500090507     d wData_Iso       s               d   inz  datfmt(*iso)
024600060327     d wNrPag          s              9  0 inz
024700060327     d wResto          s              9  0 inz
024800130927     d Sped§§          s              7  0 inz
024900131003     d nPDF§§          s              9  0 inz
025000131007     d wAR8tmd         s                   inz  like(AR8tmd)
025100090506
025200090506       //--------------------------------------------------------------
025300090506       //?Definizione prototipi procedure.                             ?
025400090506       //--------------------------------------------------------------
025500101206
025600101206       // -?Reperimento NETA sistema AS/400 corrente?
025700101206      /copy gaitrasrc/srcProtoPR,UBRTVNETA
025800090506
025900101206       // -?Reperimento dati utente?
026000090506      /copy gaitrasrc/srcProtoPR,TIBS34R
026100130121
026200130121       // -?Controllo riempimento schiere?
026300130121     d TRUL0Sds      e ds                  qualified  inz
026400130121      /copy gaitrasrc/srcProtoPR,TRUL0SR
026500090507
026600101206       // -?Execuzione comando di sistema?
026700090507      /copy gaitrasrc/srcProtoPR,QCMDEXC
026800090430
026900090430       //--------------------------------------------------------------
027000090430       //?Definizione key-list.                                        ?
027100090430       //--------------------------------------------------------------
027200090430
027300101206       // -?File TNTBE01L?
027400090430     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
027500090430     d                                     prefix(k_)   inz
027600060327
027700090430       //--------------------------------------------------------------
027800090430       //?Riepilogo indicatori utilizzati.                             ?
027900090430       //--------------------------------------------------------------
028000090430       //--------------------------------------------------------------
028100090430
028200090430       //--------------------------------------------------------------
028300090430       //?M A I N - L I N E                                            ?
028400090430       //--------------------------------------------------------------
028500090430
028600060327     c     *Entry        plist
028700060327     c                   parm                    KPJBA
028800090430
028900090430      /free
029000090430
029100101206       // -?Operazioni iniziali?
029200090430       exsr  sr_RoutInz;
029300090430
029400101206       // -?Preparazione stringa SQL?
029500090506       exsr  sr_PrepSQL;
029600090506
029700101206       // -?Ciclo di elaborazione?
029800090506       exsr  sr_OpenCursor;
029900090506
030000090506       dou  $EoF = *on;
030100090506         exsr  sr_ReadCursor;
030200090506       enddo;
030300090506
030400090506       exsr  sr_CloseCursor;
030500090430
030600101206       // -?Operazioni finali?
030700090430       exsr sr_RoutEnd;
030800090430
030900090430       //--------------------------------------------------------------
031000090430       //?Operazioni iniziali.                                         ?
031100090430       //--------------------------------------------------------------
031200090430       BEGSR  sr_RoutInz;
031300090430
031400090430         *inLR = *on;
031500090430
031600101206         // -?Impostazione opzioni per SQL?
031700090430         exec sql   set  option  DynUsrPrf = *Owner,
031800090430                                 CloSqlCsr = *EndMod;
031900090430
032000101206         // -?Reperimento dati job?
032100090430         exsr  sr_DatiJob;
032200090430
032300101206         // -?Reperimento data odierna in formato aaaammgg?
032400090430         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
032500090430         wData_Iso = %date(wDate : *iso);
032600090507
032700101206         // -?Reperimento orario?
032800090507         wTime = %dec( %time() );
032900101206
033000101206         // -?Verifica del sistema AS/400 corrente?
033100101206         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
033200101206           exsr  sr_RoutEnd;
033300101206         endif;
033400101206         // -?Impostazione elenco librerie in stanno i files da gestire?
033500101206         //  ?(a seconda del sistema in cui si stà lavorando)?
033600101206         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
033700101206         if  w_iSystem = *zero;
033800101206           exsr  sr_RoutEnd;
033900101206         endif;
034000101206         // -?Impostazione libreria e nome esatto del work-file?
034100101206         ds_Libr  = $Libraries(w_iSystem);
034200101206         if  SDSprm > *zero   and   KPJBU <> *blank;
034300101206           wLibFile = %trimr( $Libr(1) ) + '/' + 'WFPDL00F';
034400101206         else;
034500101206           wLibFile = %trimr( $Libr(1) ) + '/' + 'WFPDL10F';
034600101206         endif;
034700101206
034800101206         // -?Pulizia work-file del lavoro sottomesso (NON schedulato)?
034900101206         if  SDSprm > *zero   and   KPJBU <> *blank;
035000101206           Qcmd = 'CLRPFM file(' + %trimr($Libr(1)) + '/WFPDL00F)';
035100101206           ExecuteCommand ( Qcmd : %len(Qcmd) );
035200101206         endif;
035300090430
035400101206         // -?Gestione parametri (ricevuti oppure forzati)?
035500090430         if  SDSprm > *zero   and
035600090430             KPJBU <> *blank;
035700101206           FNLSE4ds = KPJBU;
035800090430         else;
035900090430           exsr  sr_NoParms;
036000090430         endif;
036100101206
036200101206         // -?Apertura work-file?
036300101206         open  WFPDL00F;
036400090430
036500101206         // -?Intabellamento dati da tab. "TSD" & "AR8"?
036600090430         exsr  sr_IntabAR8;
036700090430
036800090430       ENDSR;
036900090430
037000090430       //--------------------------------------------------------------
037100090430       //?Reperimento Dati del job (Utente/Operativi).                 ?
037200090430       //--------------------------------------------------------------
037300090430       BEGSR  sr_DatiJob;
037400090430
037500090430         in(E) §AzUte;
037600090430         if NOT %error;
037700090430           in(E) §DatiUte;
037800090430         endif;
037900090430         if %error or RSut = *blanks;
038000090430           clear TIBS34ds;
038100090430           tibs34r ( tibs34ds );
038200090430           in §AzUte;
038300090430           in §DatiUte;
038400090430         endif;
038500090430
038600090430       ENDSR;
038700090430
038800090430       //--------------------------------------------------------------
038900090430       //?Calcolo range di date da ultima estrazione ad oggi           ?
039000090430       //?(se non ricevuti parametri...)                               ?
039100090430       //--------------------------------------------------------------
039200090430       BEGSR  sr_NoParms;
039300090430
039400101206         // -?Reperimento ultima data di estrazione?
039500090430         exec sql   declare A1 cursor for
039600090518                    select  coalesce( max(WPDdtf), 0 )
039700090430                    from    WFPDL10F
039800090430                    for     fetch  only;
039900090430
040000090430         exec sql   open    A1;
040100090430
040200090430         clear  WPDdtf;
040300090518         exec sql   fetch   A1 into :WPDdtf;
040400090430
040500090430         select;
040600101206           // -?WrkF vuoto: gestisce l'ultima settimana?
040700090512           when  SQLcod = 100   or   WPDdtf = *zero;
040800090430             wDataLim = wData_ISO - %days(7);
040900101206           // -?Errore in elaborazione: si chiude il pgm.?
041000090430           when  SQLcod < *zero;
041100090430             $EoF = *on;
041200090430             $Err = *on;
041300101206           // -?Si riparte dall'ultima data fine periodo?
041400090430           other;
041500090430             wDataLim = %date( WPDdtf : *iso );
041600090430         endsl;
041700090430
041800090430         exec sql   close   A1;
041900090430
042000101206         // -?Se errore in elaborazione: si chiude il pgm.?
042100090430         if  $EoF = *on   or   $Err = *on;
042200090430           exsr  sr_RoutEnd;
042300090430         endif;
042400090430
042500101206         // -?Impostazione date - parametri - per la settimana successiva?
042600101206         //  ?all'ultima elaborata?
042700101206         clear  FNLSE4ds;
042800090430         wDataLim += %days(1);
042900090506         DE4dti = %dec( wDataLim );
043000090430         wDataLim += %days(6);
043100090506         DE4dtf = %dec( wDataLim );
043200090430
043300101206         // -?Test "fattibilità": deve essere elaborabile almeno una?
043400101206         //  ?settimana...?
043500090430         if  wDataLim > wData_ISO;
043600090430           $EoF = *on;
043700090430           exsr  sr_RoutEnd;
043800090430         endif;
043900090430
044000090430       ENDSR;
044100090430
044200090430       //--------------------------------------------------------------
044300090430       //?Intabellamento dati da tab. "TSD" e "AR8"                    ?
044400090430       //--------------------------------------------------------------
044500090430       BEGSR  sr_IntabAR8;
044600090430
044700101206         // -?Intabellamento Tipi Stampa da tab. "TSD"?
044800090430         k_TBEcod = 'TSD';
044900090430         clear  k_TBEke1;
045000090430         clear  k_TBEke2;
045100090430         clear  k_TBElin;
045200090430         clear  k_TBEsif;
045300090430         setll  %kds(k05tntbe01)  TNTBE000;
045400090430         reade  %kds(k05tntbe01 : 1)  TNTBE000;
045500090430         DoW  Not %eof(TNTBE01L);
045600090430           if  TBEatb = *blank;
045700090430             xx += 1;
045800090506             $TSD(xx) = TBEke1;
045900090507             if  TBEke1 <> 'E1'   and
046000090507                 TBEke1 <> 'K1'   and
046100090507                 TBEke1 <> 'S1'   and
046200090507                 TBEke1 <> 'S2'   and
046300090507                 TBEke1 <> '00'   and
046400130927                 TBEke1 <> '§§'   and            // PDF, no Pckg-Lst
046500090507                 %subst(TBEke1 : 1 : 1) <> 'L';
046600090507               if  Not %open(QSYSPRT);
046700090507                 ExecuteCommand ( c_OvrPrtF : %len(c_OvrPrtF) );
046800090507                 open   QSYSPRT;
046900090507                 except PRTtxt;
047000090507               endif;
047100090507               if  *inOF;
047200090507                 except PRTtxt;
047300090507                 *inOF = *off;
047400090507               endif;
047500090507               except PRTerrTSD;
047600090507             endif;
047700090430           endif;
047800090430           reade  %kds(k05tntbe01 : 1)  TNTBE000;
047900090430         EndDo;
048000090430
048100101206         // -?Intabellamento Tipi Modulo DDT da tab. "AR8"?
048200090430         k_TBEcod = 'AR8';
048300090430         clear  k_TBEke1;
048400090430         clear  k_TBEke2;
048500090430         clear  k_TBElin;
048600090430         clear  k_TBEsif;
048700090430         setll  %kds(k05tntbe01)  TNTBE000;
048800090430         reade  %kds(k05tntbe01 : 1)  TNTBE000;
048900090506
049000090430         DoW  Not %eof(TNTBE01L);
049100090506
049200090430           dAR8 = TBEuni;
049300090430           select;
049400090506
049500101206             // -?Rec. annullato?
049600090506             when  TBEatb <> *blank;
049700130927
049800130927             // -?PDF allegati?
049900130927             when  §AR8tsd = '§§';
050000130927               x§§ += 1;
050100130927               $AR8_§§(x§§) = TBEke1;
050200090506
050300101206             // -?Esprinet?
050400090430             when  §AR8tsd = 'E1';
050500090430               xE1 += 1;
050600090506               $AR8_E1(xE1) = TBEke1;
050700090506
050800101206             // -?Karnak?
050900090430             when  §AR8tsd = 'K1';
051000090430               xK1 += 1;
051100090506               $AR8_K1(xK1) = TBEke1;
051200090506
051300101206             // -?Logistica, Sait e Brumar?
051400090430             when  %subst(§AR8tsd : 1 : 1) = 'L';
051500090506               xLx += 1;
051600090506               $AR8_Lx(xLx) = TBEke1;
051700090506
051800101206             // -?Standard senza logo?
051900090430             when  §AR8tsd = 'S1';
052000090430               xS1 += 1;
052100090506               $AR8_S1(xS1) = TBEke1;
052200090506
052300101206             // -?Standard con logo?
052400090430             //   (NON memorizzati in schiera)
052500090430             when  §AR8tsd = 'S2';
052600090506
052700101206             // -?Modulo personalizzato (vedi Arcte)?
052800090430             when  §AR8tsd = '00';
052900090430               x00 += 1;
053000090506               $AR8_00(x00) = TBEke1;
053100090506
053200101206             // -?Tipo Modulo NON PREVISTO!!!?
053300090430             other;
053400090507               if  Not %open(QSYSPRT);
053500090507                 ExecuteCommand ( c_OvrPrtF : %len(c_OvrPrtF) );
053600090507                 open   QSYSPRT;
053700090507                 except PRTtxt;
053800090507               endif;
053900090507               if  *inOF;
054000090507                 except PRTtxt;
054100090507                 *inOF = *off;
054200090507               endif;
054300090507               except PRTerrAR8;
054400090506
054500090430           endsl;
054600090506
054700090430           reade  %kds(k05tntbe01 : 1)  TNTBE000;
054800090506
054900090430         EndDo;
055000130121
055100130121         // -?Verifica "riempimento" schiere ed eventuale invio *msg?
055200130121         // - -?per "Tipo stampa DDT" = "S1" (Std senza logo)?
055300130121         clear  TRUL0Sds;
055400130121         TRUL0Sds.i0Sski  = '$AR8_S1';             // 15 char
055500130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
055600130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
055700130121         TRUL0Sds.i0Sele  = %elem($AR8_S1);
055800130121         TRUL0Sds.i0Spie  = xS1;
055900130121         TRUL0Sds.i0Spgm  = SDSpgm;
056000130121         TRUL0Sds.i0Ssif  = knsif;
056100130121         kpjbu = TRUL0Sds;
056200130121         trul0Sr ( kpjba );
056300130121         // - -?per "Tipo stampa DDT" = "E1" (Esprinet)?
056400130121         clear  TRUL0Sds;
056500130121         TRUL0Sds.i0Sski  = '$AR8_E1';             // 15 char
056600130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
056700130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
056800130121         TRUL0Sds.i0Sele  = %elem($AR8_E1);
056900130121         TRUL0Sds.i0Spie  = xE1;
057000130121         TRUL0Sds.i0Spgm  = SDSpgm;
057100130121         TRUL0Sds.i0Ssif  = knsif;
057200130121         kpjbu = TRUL0Sds;
057300130121         trul0Sr ( kpjba );
057400130121         // - -?per "Tipo stampa DDT" = "K1" (Karnak)?
057500130121         clear  TRUL0Sds;
057600130121         TRUL0Sds.i0Sski  = '$AR8_K1';             // 15 char
057700130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
057800130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
057900130121         TRUL0Sds.i0Sele  = %elem($AR8_K1);
058000130121         TRUL0Sds.i0Spie  = xK1;
058100130121         TRUL0Sds.i0Spgm  = SDSpgm;
058200130121         TRUL0Sds.i0Ssif  = knsif;
058300130121         kpjbu = TRUL0Sds;
058400130121         trul0Sr ( kpjba );
058500130121         // - -?per "Tipo stampa DDT" = "00" (personalizzato - v. Arcte)?
058600130121         clear  TRUL0Sds;
058700130121         TRUL0Sds.i0Sski  = '$AR8_00';             // 15 char
058800130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
058900130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
059000130121         TRUL0Sds.i0Sele  = %elem($AR8_00);
059100130121         TRUL0Sds.i0Spie  = x00;
059200130121         TRUL0Sds.i0Spgm  = SDSpgm;
059300130121         TRUL0Sds.i0Ssif  = knsif;
059400130121         kpjbu = TRUL0Sds;
059500130121         trul0Sr ( kpjba );
059600130121         // - -?per "Tipo stampa DDT" = "L*" (Logistica, Sait o Brumar)?
059700130121         clear  TRUL0Sds;
059800130121         TRUL0Sds.i0Sski  = '$AR8_LX';             // 15 char
059900130121         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
060000130121         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
060100130121         TRUL0Sds.i0Sele  = %elem($AR8_Lx);
060200130121         TRUL0Sds.i0Spie  = xLx;
060300130121         TRUL0Sds.i0Spgm  = SDSpgm;
060400130121         TRUL0Sds.i0Ssif  = knsif;
060500130121         kpjbu = TRUL0Sds;
060600130121         trul0Sr ( kpjba );
060700130927         // - -?per "Tipo stampa DDT" = "§§" (PDF allegato)?
060800130927         clear  TRUL0Sds;
060900130927         TRUL0Sds.i0Sski  = '$AR8_§§';             // 15 char
061000130927         TRUL0Sds.i0Scod  = 'AR8  ';               //  5 char
061100130927         TRUL0Sds.i0Sfile = 'TNTBE00F  ';          // 10 char
061200130927         TRUL0Sds.i0Sele  = %elem($AR8_§§);
061300130927         TRUL0Sds.i0Spie  = x§§;
061400130927         TRUL0Sds.i0Spgm  = SDSpgm;
061500130927         TRUL0Sds.i0Ssif  = knsif;
061600130927         kpjbu = TRUL0Sds;
061700130927         trul0Sr ( kpjba );
061800090430
061900090430       ENDSR;
062000090430
062100090506       //--------------------------------------------------------------
062200090506       //?Preparazione stringa SQL                                     ?
062300090506       //--------------------------------------------------------------
062400090506       BEGSR  sr_PrepSQL;
062500101206
062600101206         // -?Impostazione librerie specifiche per i file da gestire?
062700101206         //  ?nella stringa SQL?
062800101206         // - -?libreria del file FIAR800F?
062900101206         $Sql(9) = %replace( $Libr(2) : $Sql(9) :
063000101206                             %scan( '&LibFil1  ' : $Sql(9) ) );
063100101206         // - -?libreria del file FNBLP00F?
063200101206         $Sql(9) = %replace( $Libr(2) : $Sql(9) :
063300101206                             %scan( '&LibFil2  ' : $Sql(9) ) );
063400090506
063500101206         // -?Impostazione parametri ricevuti come parzializzazioni?
063600101206         //  ?nella stringa SQL?
063700101206         // - -?data dal?
063800090506         $Sql(17) = %replace( %char(DE4dti) : $Sql(17) :
063900090506                              %scan( '&LSE4dti' : $Sql(17) ) );
064000101206         // - -?data al?
064100090506         $Sql(17) = %replace( %char(DE4dtf) : $Sql(17) :
064200090506                              %scan( '&LSE4dtf' : $Sql(17) ) );
064300090506
064400090506         //? E S E M P I O : ?
064500090506         //
064600090506         // select AR8tmd,
064700090506         //        BLPksc, BLPrsm,
064800090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
064900090506         //        '* ', count(*)
065000090506         // from   FIAR800F   inner join   FNBLP00F
065100090506         // on     AR8aas = BLPaas
065200090506         //   and  AR8lnp = BLPlnp
065300090506         //   and  AR8nrs = BLPnrs
065400090506         //   and  AR8nsp = BLPnsp
065500090506         // where  AR8atb = ' '
065600090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
065700090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
065800090506         //   and  AR8tmd = 'STD1'
065900090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
066000090506         // UNION
066100090506         // select 'STD2' as AR8tmd,
066200090506         //        BLPksc, BLPrsm,
066300090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
066400090506         //        '* ', count(*)
066500090506         // from   FIAR800F   inner join   FNBLP00F
066600090506         // on     AR8aas = BLPaas
066700090506         //   and  AR8lnp = BLPlnp
066800090506         //   and  AR8nrs = BLPnrs
066900090506         //   and  AR8nsp = BLPnsp
067000090506         // where  AR8atb = ' '
067100090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
067200090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
067300090506         //   and  AR8tmd NOT in  ('STD1', 'KARN', 'EDK ', 'TITA',
067400090506         //                        'SAIT', 'ARCT', 'ESP1')
067500090506         //   and  AR8tmd NOT like 'LOG%'
067600090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
067700090506         // UNION
067800090506         // select AR8tmd,
067900090506         //        BLPksc, BLPrsm,
068000090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
068100090506         //        'C*', count(*)
068200090506         // from   FIAR800F   inner join   FNBLP00F
068300090506         // on     AR8aas = BLPaas
068400090506         //   and  AR8lnp = BLPlnp
068500090506         //   and  AR8nrs = BLPnrs
068600090506         //   and  AR8nsp = BLPnsp
068700090506         // where  AR8atb = ' '
068800090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
068900090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
069000090506         //   and (AR8tmd in  ('KARN', 'EDK ', 'TITA', 'SAIT', 'ARCT', 'ESP1')
069100090506         //    or  AR8tmd like 'LOG%')
069200090506         //   and  AR8trk in  ('C1', 'C2', 'C3', 'O1', 'O2')
069300090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
069400090506         // UNION
069500090506         // select AR8tmd,
069600090506         //        BLPksc, BLPrsm,
069700090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
069800090506         //        'Z1', count(*)
069900090506         // from   FIAR800F   inner join   FNBLP00F
070000090506         // on     AR8aas = BLPaas
070100090506         //   and  AR8lnp = BLPlnp
070200090506         //   and  AR8nrs = BLPnrs
070300090506         //   and  AR8nsp = BLPnsp
070400090506         // where  AR8atb = ' '
070500090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
070600090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
070700090506         //   and (AR8tmd in  ('SAIT')
070800090506         //    or  AR8tmd like 'LOG%')
070900090506         //   and  AR8trk in  ('Z1')
071000090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
071100090506         // UNION
071200090506         // select AR8tmd,
071300090506         //        BLPksc, BLPrsm,
071400090506         //        AR8aas, AR8lnp, AR8nrs, AR8nsp,
071500090506         //        'S*', count(*)
071600090506         // from   FIAR800F   inner join   FNBLP00F
071700090506         // on     AR8aas = BLPaas
071800090506         //   and  AR8lnp = BLPlnp
071900090506         //   and  AR8nrs = BLPnrs
072000090506         //   and  AR8nsp = BLPnsp
072100090506         // where  AR8atb = ' '
072200090506         //   and  substr(digits(BLPksc), 4, 4) <> '9999'
072300090506         //   and  (BLPaas*10000+BLPmgs)  between  :DE4dti and :DE4dtf
072400090506         //   and (AR8tmd in  ('SAIT')
072500090506         //    or  AR8tmd like 'LOG%')
072600090506         //   and  AR8trk in  ('S2', 'S3')
072700090506         // group  by  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
072800090506         //
072900090506         // ORDER  BY  AR8tmd,BLPksc,BLPrsm,AR8aas,AR8lnp,AR8nrs,AR8nsp
073000090506         // for    FETCH  only
073100090506         //
073200090506         //? N.B. ?
073300090506         // · Scarta    le LdV con BLPKSC = xxx9999
073400090506         // · Seleziona le LdV del periodo richiesto
073500090506         // · Seleziona le LdV con codice bolla = "1 ", "4 " o "5 "
073600090506
073700090506         clear  wSQL;
073800090506
073900130927         // »? 1 ?
074000101206         // -?DDT standard senza logo ("STD1"): trk T1, T2, C1?
074100090506         wSQL =       %trim($Sql( 1))
074200090506              + ' ' + %trim($Sql( 2))
074300090506              + ' ' + %trim($Sql( 4))
074400090506              + ' ' + %trim($Sql( 5))
074500090506              + ' ' + %trim($Sql( 9))
074600090506              + ' ' + %trim($Sql(10))
074700090506              + ' ' + %trim($Sql(11))
074800090506              + ' ' + %trim($Sql(12))
074900090506              + ' ' + %trim($Sql(13))
075000090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
075100090506              + ' ' + %trim($Sql(15))
075200090506              + ' ' + %trim($Sql(16))
075300090506              + ' ' + %trim($Sql(17))
075400090506              + ' ' + %trim($Sql(18))
075500090506              + ' ' + %trim($Sql(24))
075600090506              + ' ' + %trim($Sql(25))
075700090506
075800130927         // »? 2 ?
075900101206         // -?DDT standard  con  logo ("STD2"): trk T1, T2, C1?
076000090506              + ' ' + c_Union
076100090506              + ' ' + %trim($Sql( 1))
076200090506              + ' ' + %trim($Sql( 3))
076300090506              + ' ' + %trim($Sql( 4))
076400090506              + ' ' + %trim($Sql( 5))
076500090506              + ' ' + %trim($Sql( 9))
076600090506              + ' ' + %trim($Sql(10))
076700090506              + ' ' + %trim($Sql(11))
076800090506              + ' ' + %trim($Sql(12))
076900090506              + ' ' + %trim($Sql(13))
077000090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
077100090506              + ' ' + %trim($Sql(15))
077200090506              + ' ' + %trim($Sql(16))
077300090506              + ' ' + %trim($Sql(17))
077400090506              + ' ' + %trim($Sql(19));
077500101206         // -?Intabellamento loghi da scartare (NON di tipo "S2"):?
077600101206         // - -?di tipo "E1" (Esprinet)?
077700090506         For  xx = 1  To xE1;
077800090506           if  $AR8_E1(xx) <> *blank;
077900090506             if  xx = 1;
078000090507               wSQL += '''' + $AR8_E1(xx) + '''';
078100090506             else;
078200090507               wSQL += ', ''' + $AR8_E1(xx) + '''';
078300090506             endif;
078400090506           else;
078500090506             leave;
078600090506           endif;
078700090506         EndFor;
078800101206         // - -?di tipo "K1" (Karnak)?
078900090506         For  xx = 1  To xK1;
079000090506           if  $AR8_K1(xx) <> *blank;
079100090506             if  xx = 1   and   xE1 = *zero;
079200090507               wSQL += '''' + $AR8_K1(xx) + '''';
079300090506             else;
079400090507               wSQL += ', ''' + $AR8_K1(xx) + '''';
079500090506             endif;
079600090506           else;
079700090506             leave;
079800090506           endif;
079900090506         EndFor;
080000101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
080100090506         For  xx = 1  To xLx;
080200090506           if  $AR8_Lx(xx) <> *blank;
080300090506             if  xx = 1   and   xE1 = *zero
080400090506                          and   xK1 = *zero;
080500090507               wSQL += '''' + $AR8_Lx(xx) + '''';
080600090506             else;
080700090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
080800090506             endif;
080900090506           else;
081000090506             leave;
081100090506           endif;
081200090506         EndFor;
081300101206         // - -?di tipo "S1" (Std senza logo)?
081400090506         For  xx = 1  To xS1;
081500090506           if  $AR8_S1(xx) <> *blank;
081600090506             if  xx = 1   and   xE1 = *zero
081700090506                          and   xK1 = *zero
081800090506                          and   xLx = *zero;
081900090507               wSQL += '''' + $AR8_S1(xx) + '''';
082000090506             else;
082100090507               wSQL += ', ''' + $AR8_S1(xx) + '''';
082200090506             endif;
082300090506           else;
082400090506             leave;
082500090506           endif;
082600090506         EndFor;
082700101206         // - -?di tipo "00" (personalizzato - v. Arcte)?
082800090506         For  xx = 1  To x00;
082900090506           if  $AR8_00(xx) <> *blank;
083000090506             if  xx = 1   and   xE1 = *zero
083100090506                          and   xK1 = *zero
083200090506                          and   xLx = *zero
083300090506                          and   xS1 = *zero;
083400090507               wSQL += '''' + $AR8_00(xx) + '''';
083500090506             else;
083600090507               wSQL += ', ''' + $AR8_00(xx) + '''';
083700090506             endif;
083800090506           else;
083900090506             leave;
084000090506           endif;
084100090506         EndFor;
084200130927         // - -?di tipo "§§" (PDF allegato/i)?
084300130927         For  xx = 1  To x§§;
084400130927           if  $AR8_§§(xx) <> *blank;
084500130927             if  xx = 1   and   xE1 = *zero
084600130927                          and   xK1 = *zero
084700130927                          and   xLx = *zero
084800130927                          and   xS1 = *zero
084900130927                          and   x00 = *zero;
085000130927               wSQL += '''' + $AR8_§§(xx) + '''';
085100130927             else;
085200130927               wSQL += ', ''' + $AR8_§§(xx) + '''';
085300130927             endif;
085400130927           else;
085500130927             leave;
085600130927           endif;
085700130927         EndFor;
085800101206         // - -?"chiusura" stringa sql?
085900090507         wSQL += ')'
086000090506              + ' ' + %trim($Sql(24))
086100090506              + ' ' + %trim($Sql(25))
086200090506
086300130927         // »? 3 ?
086400101206         // -?Dettaglio in DDT Logistica o Sait      : trk C1, C2, C3, C4, C5?
086500101206         //               ?DDT Karnak, EDk o Titanedi: trk C1, C2, C3, O1, O2?
086600101206         //               ?DDT Arcte                 : trk C1, C2?
086700101206         //               ?DDT Esprinet              : trk C1, C2?
086800090506              + ' ' + c_Union
086900090506              + ' ' + %trim($Sql( 1))
087000090506              + ' ' + %trim($Sql( 2))
087100090506              + ' ' + %trim($Sql( 4))
087200090506              + ' ' + %trim($Sql( 6))
087300090506              + ' ' + %trim($Sql( 9))
087400090506              + ' ' + %trim($Sql(10))
087500090506              + ' ' + %trim($Sql(11))
087600090506              + ' ' + %trim($Sql(12))
087700090506              + ' ' + %trim($Sql(13))
087800090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
087900090506              + ' ' + %trim($Sql(15))
088000090506              + ' ' + %trim($Sql(16))
088100090506              + ' ' + %trim($Sql(17))
088200090506              + ' ' + %trim($Sql(20));
088300101206         // -?Intabellamento loghi da eleborare:?
088400101206         // - -?di tipo "E1" (Esprinet)?
088500090506         For  xx = 1  To xE1;
088600090506           if  $AR8_E1(xx) <> *blank;
088700090506             if  xx = 1;
088800090507               wSQL += '''' + $AR8_E1(xx) + '''';
088900090506             else;
089000090507               wSQL += ', ''' + $AR8_E1(xx) + '''';
089100090506             endif;
089200090506           else;
089300090506             leave;
089400090506           endif;
089500090506         EndFor;
089600101206         // - -?di tipo "K1" (Karnak)?
089700090506         For  xx = 1  To xK1;
089800090506           if  $AR8_K1(xx) <> *blank;
089900090506             if  xx = 1   and   xE1 = *zero;
090000090507               wSQL += '''' + $AR8_K1(xx) + '''';
090100090506             else;
090200090507               wSQL += ', ''' + $AR8_K1(xx) + '''';
090300090506             endif;
090400090506           else;
090500090506             leave;
090600090506           endif;
090700090506         EndFor;
090800101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
090900090506         For  xx = 1  To xLx;
091000090506           if  $AR8_Lx(xx) <> *blank;
091100090506             if  xx = 1   and   xE1 = *zero
091200090506                          and   xK1 = *zero;
091300090507               wSQL += '''' + $AR8_Lx(xx) + '''';
091400090506             else;
091500090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
091600090506             endif;
091700090506           else;
091800090506             leave;
091900090506           endif;
092000090506         EndFor;
092100101206         // - -?di tipo "00" (personalizzato - v. Arcte)?
092200090506         For  xx = 1  To x00;
092300090506           if  $AR8_00(xx) <> *blank;
092400090506             if  xx = 1   and   xE1 = *zero
092500090506                          and   xK1 = *zero
092600090506                          and   xLx = *zero
092700090506                          and   xS1 = *zero;
092800090507               wSQL += '''' + $AR8_00(xx) + '''';
092900090506             else;
093000090507               wSQL += ', ''' + $AR8_00(xx) + '''';
093100090506             endif;
093200090506           else;
093300090506             leave;
093400090506           endif;
093500090506         EndFor;
093600130927         // - -?di tipo "§§" (PDF allegato/i)?
093700130927         //    ?»»» NON COMPRESI QUI !!!! «««?
093800101206         // - -?"chiusura" stringa sql?
093900090507         wSQL += ')'
094000090506              + ' ' + %trim($Sql(21))
094100090506              + ' ' + %trim($Sql(24))
094200090506              + ' ' + %trim($Sql(25))
094300090506
094400130927         // »? 4 ?
094500101206         // -?Packing-List in DDT Logistica, Sait o Brumar: trk Z1?
094600090506              + ' ' + c_Union
094700090506              + ' ' + %trim($Sql( 1))
094800090506              + ' ' + %trim($Sql( 2))
094900090506              + ' ' + %trim($Sql( 4))
095000090506              + ' ' + %trim($Sql( 7))
095100090506              + ' ' + %trim($Sql( 9))
095200090506              + ' ' + %trim($Sql(10))
095300090506              + ' ' + %trim($Sql(11))
095400090506              + ' ' + %trim($Sql(12))
095500090506              + ' ' + %trim($Sql(13))
095600090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
095700090506              + ' ' + %trim($Sql(15))
095800090506              + ' ' + %trim($Sql(16))
095900090506              + ' ' + %trim($Sql(17))
096000090506              + ' ' + %trim($Sql(20));
096100101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
096200090506         For  xx = 1  To xLx;
096300090506           if  $AR8_Lx(xx) <> *blank;
096400090506             if  xx = 1;
096500090507               wSQL += '''' + $AR8_Lx(xx) + '''';
096600090506             else;
096700090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
096800090506             endif;
096900090506           else;
097000090506             leave;
097100090506           endif;
097200090506         EndFor;
097300101206         // - -?"chiusura" stringa sql?
097400090507         wSQL += ')'
097500090506              + ' ' + %trim($Sql(22))
097600090506              + ' ' + %trim($Sql(24))
097700090506              + ' ' + %trim($Sql(25))
097800090506
097900130927         // »? 5 ?
098000101206         // -?Fattura in DDT Logistica, Sait o Brumar: trk S2, S3?
098100090506              + ' ' + c_Union
098200090506              + ' ' + %trim($Sql( 1))
098300090506              + ' ' + %trim($Sql( 2))
098400090506              + ' ' + %trim($Sql( 4))
098500090506              + ' ' + %trim($Sql( 8))
098600090506              + ' ' + %trim($Sql( 9))
098700090506              + ' ' + %trim($Sql(10))
098800090506              + ' ' + %trim($Sql(11))
098900090506              + ' ' + %trim($Sql(12))
099000090506              + ' ' + %trim($Sql(13))
099100090506            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
099200090506              + ' ' + %trim($Sql(15))
099300090506              + ' ' + %trim($Sql(16))
099400090506              + ' ' + %trim($Sql(17))
099500090506              + ' ' + %trim($Sql(20));
099600101206         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
099700090506         For  xx = 1  To xLx;
099800090506           if  $AR8_Lx(xx) <> *blank;
099900090506             if  xx = 1;
100000090507               wSQL += '''' + $AR8_Lx(xx) + '''';
100100090506             else;
100200090507               wSQL += ', ''' + $AR8_Lx(xx) + '''';
100300090506             endif;
100400090506           else;
100500090506             leave;
100600090506           endif;
100700090506         EndFor;
100800101206         // - -?"chiusura" stringa sql?
100900090507         wSQL += ')'
101000090506              + ' ' + %trim($Sql(23))
101100090506              + ' ' + %trim($Sql(24))
101200090506              + ' ' + %trim($Sql(25))
101300130927
101400130927         // »? 6 ?
101500130927         // -?PDF allegati ("§PDF"): trk __?
101600130927              + ' ' + c_Union
101700130927              + ' ' + %trim($Sql( 1))
101800130927              + ' ' + %trim($Sql( 2))
101900130927              + ' ' + %trim($Sql( 4))
102000130927              + ' ' + %trim($Sql( 5))
102100130927              + ' ' + %trim($Sql( 9))
102200130927              + ' ' + %trim($Sql(10))
102300130927              + ' ' + %trim($Sql(11))
102400130927              + ' ' + %trim($Sql(12))
102500130927              + ' ' + %trim($Sql(13))
102600130927            //+ ' ' + %trim($Sql(14))    (NO test flag ATB)
102700130927              + ' ' + %trim($Sql(15))
102800130927              + ' ' + %trim($Sql(16))
102900130927              + ' ' + %trim($Sql(17))
103000130927              + ' ' + %trim($Sql(20));
103100130927         // - -?di tipo "§§" (PDF allegato/i)?
103200130927         For  xx = 1  To x§§;
103300130927           if  $AR8_§§(xx) <> *blank;
103400130927             if  xx = 1;
103500130927               wSQL += '''' + $AR8_§§(xx) + '''';
103600130927             else;
103700130927               wSQL += ', ''' + $AR8_§§(xx) + '''';
103800130927             endif;
103900130927           else;
104000130927             leave;
104100130927           endif;
104200130927         EndFor;
104300130927         // - -?"chiusura" stringa sql?
104400130927         wSQL += ')'
104500130927              + ' ' + %trim($Sql(24))
104600130927              + ' ' + %trim($Sql(25))
104700090506
104800130927         // »? 9 ?
104900101206         // -?Order by?
105000090506              + ' ' + c_OrderBy
105100090506
105200101206         // -?For fetch only?
105300090506              + ' ' + c_ForFetch;
105400090506
105500090506       ENDSR;
105600090506
105700090506       //--------------------------------------------------------------
105800090506       //?Apertura cursore.                                            ?
105900090506       //--------------------------------------------------------------
106000090506       BEGSR  sr_OpenCursor;
106100090506
106200101206         // -?Dichiarazione cursore?
106300090506         exec sql   prepare S1   from :wSQL;
106400090506         exec sql   declare C1   cursor for S1;
106500090506
106600101206         // -?Apertura del cursore?
106700090506         exec sql   open C1;
106800090507
106900090507         if  SQLcode < *zero;
107000090507           $Err = *on;
107100090507           $EoF = *on;
107200090507           exsr  sr_PrintErrSQL;
107300090507         endif;
107400090506
107500090506       ENDSR;
107600090506
107700090506       //--------------------------------------------------------------
107800090506       //?Chiusura cursore.                                            ?
107900090506       //--------------------------------------------------------------
108000090506       BEGSR  sr_CloseCursor;
108100090506
108200101206         // -?Registrazione totale ultimo cliente?
108300090506         exsr  sr_Wrt_WFPDL;
108400090506
108500101206         // -?Chiusura del cursore?
108600090506         exec sql   close C1;
108700090506
108800090506       ENDSR;
108900090506
109000090506       //--------------------------------------------------------------
109100090506       //?Lettura cursore.                                             ?
109200090506       //--------------------------------------------------------------
109300090506       BEGSR  sr_ReadCursor;
109400090506
109500090506         clear  wSQLds;
109600090506
109700090506         exec sql   fetch next   from C1   into :wSQLds;
109800090506
109900090506         select;
110000090507           when  SQLcode = 100;
110100090506             $EoF = *on;
110200090507           when  SQLcode < *zero;
110300090506             $Err = *on;
110400090506             $Eof = *on;
110500090507             exsr  sr_PrintErrSQL;
110600090506           other;
110700131007             if  SQLksc <> SavSQLksc   or   SQLtmd <> SavSQLtmd;
110800131007               exsr  sr_Write;
110900131007             endif;
111000131007             exsr  sr_Elab;
111100090506         endsl;
111200090506
111300090506       ENDSR;
111400131007
111500131007       //--------------------------------------------------------------
111600131007       //?Elaborazione singolo record da di totale                     ?
111700131007       //--------------------------------------------------------------
111800131007       BEGSR  sr_Elab;
111900131007
112000131007         // -?Reperimento tipologia di DDT?
112100131007         clear  xS1;
112200131007         clear  xE1;
112300131007         clear  xK1;
112400131007         clear  x00;
112500131007         clear  xLx;
112600131007         clear  x§§;
112700131007         xS1 = %lookup( SQLtmd : $AR8_S1 );
112800131007         xE1 = %lookup( SQLtmd : $AR8_E1 );
112900131007         xK1 = %lookup( SQLtmd : $AR8_K1 );
113000131007         x00 = %lookup( SQLtmd : $AR8_00 );
113100131007         xLx = %lookup( SQLtmd : $AR8_Lx );
113200131007         x§§ = %lookup( SQLtmd : $AR8_§§ );
113300131007
113400131007         //?Calcolo numero pagine per DDT in esame?
113500131007
113600131007         clear  wNrPag;
113700131007         clear  wResto;
113800131007
113900131007         SELECT;
114000131007
114100131007           // -?DDT standard con o senza logo (trk T1, T2, C1)?
114200131007           // - 42 righe x pagina
114300131007           WHEN  xS1                  > *zero   or
114400131007                (xE1+xK1+x00+xLx+x§§) = *zero;
114500131007             wNrPag = %div( SQlrec : 42 );
114600131007             wResto = %rem( SQLrec : 42 );
114700131007
114800131007           // -?DDT Logistica, Sait o Brumar?
114900131007           WHEN  xLx > *zero;
115000131007             select;
115100131007               // -?rec. di "corpo" (trk C1, C2, C3):?
115200131007               //  ?24 righe x pagina con   testata e con   piede?
115300131007               //  ?28 righe x pagina con   testata e senza piede?
115400131007               //  ?39 righe x pagina senza testata e senza piede?
115500131007               //  ?35 righe x pagina senza testata e con   piede?
115600131007               when  SQLtrk = 'C*'   and   SQLrec <= 24;
115700131007                 wNrPag  = 1;
115800131007               when  SQLtrk = 'C*'   and   SQLrec <= (28 + 35);
115900131007                 wNrPag  = 2;
116000131007               when  SQLtrk = 'C*'   and   SQLrec >  (28 + 35);
116100131007                 wResto  = SQLrec - (28 + 35);
116200131007                 wNrPag  = %div( wResto : 39 );
116300131007                 wResto  = %rem( wResto : 39 );
116400131007                 wNrPag += 2;
116500131007               // -?rec. di "packing-list" (trk Z1):?
116600131007               //  ?29 righe x pagina con   testata (NO piede)?
116700131007               //  ?40 righe x pagina senza testata (NO piede)?
116800131007               when  SQLtrk = 'Z1'   and   SQLrec <= 29;
116900131007                 wNrPag  = 1;
117000131007               when  SQLtrk = 'Z1'   and   SQLrec >  29;
117100131007                 wResto  = SQLrec - 29;
117200131007                 wNrPag  = %div( wResto : 40 );
117300131007                 wResto  = %rem( wResto : 40 );
117400131007                 wNrPag += 1;
117500131007               // -?rec. di "fattura" (trk S2, S3):?
117600131007               //  ?26 righe x pagina con   testata e con   piede?
117700131007               //  ?29 righe x pagina con   testata e senza piede?
117800131007               //  ?42 righe x pagina senza testata e senza piede?
117900131007               //  ?39 righe x pagina senza testata e con   piede?
118000131007               when  SQLtrk = 'S*'   and   SQLrec <= 26;
118100131007                 wNrPag  = 1;
118200131007               when  SQLtrk = 'S*'   and   SQLrec <= (29 + 39);
118300131007                 wNrPag  = 2;
118400131007               when  SQLtrk = 'S*'   and   SQLrec >  (29 + 39);
118500131007                 wResto  = SQLrec - (29 + 39);
118600131007                 wNrPag  = %div( wResto : 42 );
118700131007                 wResto  = %rem( wResto : 42 );
118800131007                 wNrPag += 2;
118900131007             endsl;
119000131007
119100131007           // -?DDT Karnak, EDk o Titanedi?
119200131007           //  ?19 righe x pagina (omaggi ESCLUSI)?
119300131007           WHEN  xK1 > *zero;
119400131007             if  SQLtrk = 'C*';
119500131007               wNrPag  = %div( SQLrec : 19 );
119600131007               wResto  = %rem( SQLrec : 19 );
119700131007             endif;
119800131007
119900131007           // -?DDT Arcte?
120000131007           WHEN  x00 > *zero;
120100131007             select;
120200131007               // -?rec. di "corpo" (trk C1, C2):?
120300131007               //  ?14 righe x pagina con   testata e con   piede?
120400131007               //  ?29 righe x pagina con   testata e senza piede?
120500131007               //  ?40 righe x pagina senza testata e senza piede?
120600131007               //  ?36 righe x pagina senza testata e con   piede?
120700131007               when  SQLtrk = 'C*'   and   SQLrec <= 25;
120800131007                 wNrPag  = 1;
120900131007               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 36);
121000131007                 wNrPag  = 2;
121100131007               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 36);
121200131007                 wResto  = SQLrec - (29 + 36);
121300131007                 wNrPag  = %div( wResto : 40 );
121400131007                 wResto  = %rem( wResto : 40 );
121500131007                 wNrPag += 2;
121600131007             endsl;
121700131007
121800131007           // -?DDT Esprinet?
121900131007           WHEN  xE1 > *zero;
122000131007             select;
122100131007               // -?rec. di "corpo" (trk C1, C2)?
122200131007               //  ?18 righe x pagina con   testata e con   piede?
122300131007               //  ?29 righe x pagina con   testata e senza piede?
122400131007               //  ?39 righe x pagina senza testata e senza piede?
122500131007               //  ?28 righe x pagina senza testata e con   piede?
122600131007               when  SQLtrk = 'C*'   and   SQLrec <= 18;
122700131007                 wNrPag  = 1;
122800131007               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 28);
122900131007                 wNrPag  = 2;
123000131007               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 28);
123100131007                 wResto  = SQLrec - (29 + 28);
123200131007                 wNrPag  = %div( wResto : 39 );
123300131007                 wResto  = %rem( wResto : 39 );
123400131007                 wNrPag += 2;
123500131007             endsl;
123600131007
123700131007           // -?PDF allegato/i?
123800131007           WHEN  x§§ > *zero;
123900131007             wNrPag += SQLrec;
124000131007             wResto  = *zero;
124100131007
124200131007         ENDSL;
124300131007
124400131007         if  wResto > *zero;
124500131007           wNrPag += 1;
124600131007         endif;
124700131007
124800131007         // -?Sistemazione dei totali delle sole righe di dettaglio  SE ?
124900131007         //  ?reperite righe di packing-list e/o fattura?
125000131007         if  xLx > *zero         and
125100131007            (SQLtrk = 'Z1'       or   SQLtrk = 'S*')      and
125200131007            (SQLtmd = SavSQLtmd  and  SQLksc = SavSQLksc  and
125300131007             SQLaas = SavSQLaas  and  SQLlnp = SavSQLlnp  and
125400131007             SQLnrs = SavSQLnrs  and  SQLnsp = SavSQLnsp);
125500131007           wNrPag += SavNrPag;
125600131007           $_ddt(SavXX) -= 1;
125700131007           if  SavXX = 6  and  SavNrPag >= 6;
125800131007             wPagOltre5 -= SavNrPag;
125900131007           endif;
126000131007         endif;
126100131007
126200131007         // -?Aggiornam. del totale DDT per n° pagine (relativo al cliente)?
126300131007         select;
126400131007           when  x§§ > *zero;
126500131007             exsr  sr_ChkPkgLst;
126600131007             clear  xx;
126700131007             Sped§§ += 1;
126800131007             nPDF§§ += wNrPag;
126900131007           when  wNrPag <= *zero;
127000131007             xx  = 1;
127100131007           when  wNrPag >= 6;
127200131007             xx  = 6;
127300131007             wPagOltre5 += wNrPag;
127400131007           other;
127500131007             xx  = wNrPag;
127600131007         endsl;
127700131007         if  x§§ = *zero;
127800131007           $_ddt(xx) += 1;
127900131007         endif;
128000131007
128100131007         // -?Memorizza i dati appena elaborati?
128200131007         //  ?(questo nr.pag. potrebbe essere da togliere da questo conteggio?
128300131007         //  ?e da aggiungere a quello successivo - se seguisse un trk "Z1"?
128400131007         //  ?con una packing-list nella stessa LdV, o se seguisse un trk?
128500131007         //  ?"S*" con una fattura nella stessa LdV)?
128600131007         clear  wSavSQLds;
128700131007         %subst( wSavSQLds : 1 : %len( wSQlds ) ) =  wSQLds;
128800131007         SavNrPag  =  wNrPag;
128900131007         SavXX     =  xx;
129000131007
129100131007       ENDSR;
129200090506
129300090506       //--------------------------------------------------------------
129400090506       //?Registrazione record nel WorkFile WFPDL00F                   ?
129500090506       //--------------------------------------------------------------
129600090506       BEGSR  sr_Write;
129700090506
129800131007         // -?Scrittura a rottura di cliente / tipo modulo DDT?
129900131007         IF  SavSQLtmd <> *blank   and   SavSQLksc <> *zero;
130000131007
130100131007           if  SavSQLksc <> SQLksc  or
130200131009               %lookup( SavSQLtmd : $AR8_§§ ) = *zero;
130300131007             exsr  sr_Wrt_WFPDL;
130400131007           endif;
130500131007
130600131007           clear  $_ddt;
130700131007           clear  wPagOltre5;
130800131007           if  SavSQLksc <> SQLksc;
130900131007             clear  Sped§§;
131000131007             clear  nPDF§§;
131100131007             clear  $_Loghi;
131200131007             clear  $_Sped§§;
131300131007             clear  $_nPdf§§;
131400131007           endif;
131500131007
131600131007         ENDIF;
131700131007
131800131007         SavSQLtmd = SQLtmd;
131900131007         SavSQLksc = SQLksc;
132000131007
132100131007         // -?decodifica nuovo P.O. (a rottura dello stesso)?
132200131007         WWWfil = %div( SQLksc : 10000 );
132300131007         if  WWWfil <> ORGfil;
132400131007           chain  (WWWfil)  AZORG;
132500131007           if  NOT  %found(AZORG01L)
132600131007               or   ORGfva <> *blank;
132700131007             clear  ORGfl3;
132800131007             clear  ORGcar;
132900131007           endif;
133000131007         endif;
133100090506
133200090506       ENDSR;
133300090506
133400090506       //--------------------------------------------------------------
133500090506       //?Scrittura del singolo record nel WorkFile WFPDL00F           ?
133600090506       //--------------------------------------------------------------
133700090506       BEGSR  sr_Wrt_WFPDL;
133800090506
133900131007         // -?Nuovo Cliente  o  Nuovo Tipo Modulo?
134000090506         clear  WFPDL000;
134100090506
134200090506         WPDeut  = DUTute;
134300090506         WPDedt  = wDate;
134400090506         WPDdti  = DE4dti;
134500090506         WPDdtf  = DE4dtf;
134600090506         WPDcdi  = ORGfl3;
134700090506         WPDcar  = ORGcar;
134800090506         WPDfil  = ORGfil;
134900090506         WPDksc  = SavSQLksc;
135000090506         WPDrsm  = SavSQLrsm;
135100090506         WPDtmd  = SavSQLtmd;
135200090506         WPDn1p  = $_ddt(1);
135300090506         WPDn2p  = $_ddt(2);
135400090506         WPDn3p  = $_ddt(3);
135500090506         WPDn4p  = $_ddt(4);
135600090506         WPDn5p  = $_ddt(5);
135700090506         WPDnop  = $_ddt(6);
135800130927         WPDpo5  = wPagOltre5;
135900131007         // -?PDF allegati?
136000131007         //WPDpdfSp  = Sped§§;
136100131007         //WPDpdfNr  = nPDF§§;
136200131009         // -?Impostazione delle spedizioni con anche PDF e sottrazione?
136300131009         //  ?delle stesse dal totale di quelle cn PDF (per ottenere il?
136400131009         //  ?numero di quelle con SOLO PDF)?
136500131007         yy = %lookup( SavSQLtmd : $_Loghi );
136600131007         if  yy > *zero;
136700131007           WPDpdfSp  = $_Sped§§(yy);
136800131007           WPDpdfNr  = $_nPdf§§(yy);
136900131007           Sped§§   -= $_Sped§§(yy);
137000131007           nPDF§§   -= $_nPdf§§(yy);
137100131007         endif;
137200090506
137300090506         //_________________
137400090506         WRITE     WFPDL000;
137500090506         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
137600131007
137700131007         // -?PDF residui (NON abbinati ad altre packing-list)?
137800131007         IF  SavSQLksc <> SQLksc  and
137900131007             Sped§§     > *zero;
138000131007
138100131007           For  yy = 1  To  %elem($_Loghi);
138200131007
138300131007             if  $_Loghi(yy) = *blank;
138400131007               leave;
138500131007             endif;
138600131009             //if  %subst( $_Loghi(yy) : 1 : 1 ) <> '§'   or
138700131009             if  %lookup( $_Loghi(yy) : $AR8_§§ ) = *zero  or
138800131007                 $_Sped§§(yy) <= *zero;
138900131007               iter;
139000131007             endif;
139100131007
139200131007             clear  WFPDL000;
139300131007
139400131007             WPDeut  = DUTute;
139500131007             WPDedt  = wDate;
139600131007             WPDdti  = DE4dti;
139700131007             WPDdtf  = DE4dtf;
139800131007             WPDcdi  = ORGfl3;
139900131007             WPDcar  = ORGcar;
140000131007             WPDfil  = ORGfil;
140100131007             WPDksc  = SavSQLksc;
140200131007             WPDrsm  = SavSQLrsm;
140300131007             WPDtmd  = $_Loghi(yy);
140400131009             //WPDn1p  = *zero;
140500131009             //WPDn2p  = *zero;
140600131009             //WPDn3p  = *zero;
140700131009             //WPDn4p  = *zero;
140800131009             //WPDn5p  = *zero;
140900131009             //WPDnop  = *zero;
141000131009             //WPDpo5  = *zero;
141100131007             WPDpdfSp  = $_Sped§§(yy);
141200131007             WPDpdfNr  = $_nPdf§§(yy);
141300131007
141400131007             //_________________
141500131007             WRITE     WFPDL000;
141600131007             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
141700131007
141800131007           EndFor;
141900131007
142000131007         ENDIF;
142100090506
142200090506       ENDSR;
142300131007
142400131007       //--------------------------------------------------------------
142500131007       //?Verifica esistenza di packing-list oltre al PDF              ?
142600131007       //--------------------------------------------------------------
142700131007       BEGSR  sr_ChkPkgLst;
142800131007
142900131007         // -?Reperimento eventuale packing-list (oltre al PDF)?
143000131009         clear  wSql2;
143100131009         wSql2 = 'select AR8tmd +
143200131009                    from ' + %trimr( $Libr(2) ) + '/FIAR800F +
143300131009                   where AR8aas = ' + %trim( %editc( SQLaas : '3' ) ) + ' +
143400131009                     and AR8lnp = ' + %trim( %editc( SQLlnp : '3' ) ) + ' +
143500131009                     and AR8nrs = ' + %trim( %editc( SQLnrs : '3' ) ) + ' +
143600131009                     and AR8nsp = ' + %trim( %editc( SQLnsp : '3' ) ) + ' +
143700131009                     and AR8tmd NOT in (';
143800131009         For  xx = 1  To x§§;
143900131009           if  $AR8_§§(xx) <> *blank;
144000131009             if  xx = 1;
144100131009               wSql2 += '''' + $AR8_§§(xx) + '''';
144200131009             else;
144300131009               wSql2 += ', ''' + $AR8_§§(xx) + '''';
144400131009             endif;
144500131009           else;
144600131009             leave;
144700131009           endif;
144800131009         EndFor;
144900131009         wSql2 += ') ' +
145000131009                  c_ForFetch;
145100131009
145200131009         // -?Dichiarazione cursore?
145300131009         exec sql   prepare S2   from :wSql2;
145400131009         exec sql   declare C2   cursor for S2;
145500131009
145600131009         // -?Apertura del cursore?
145700131009         exec sql   open C2;
145800131009         if  SQLcode < *zero;
145900131009           $Err = *on;
146000131009           $EoF = *on;
146100131009           exsr  sr_PrintErrSQL;
146200131009         endif;
146300131009
146400131009         // -?Lettura del cursore?
146500131009         clear  wAR8tmd;
146600131009         exec sql   fetch next   from C2   into :wAR8tmd;
146700131007
146800131007         clear  yy;
146900131007         select;
147000131007           //// -?NON reperita packing-list (oltre al PDF)?
147100131007           ////  ?(si memorizzano questi PDF)?
147200131009           //when  SQLcode = 100;
147300131007           // -?Errore in elaborazione: si chiude il pgm.?
147400131009           when  SQLcode < *zero;
147500131007             $EoF = *on;
147600131007             $Err = *on;
147700131007           // -?Si conteggiano i PDF insieme alla packing-list?
147800131007           other;
147900131009             if  SQLcode = 100;
148000131007               yy = %lookup( SQLtmd : $_Loghi );
148100131007             else;
148200131007               yy = %lookup( wAR8tmd : $_Loghi );
148300131007             endif;
148400131007             if  yy = *zero;
148500131007               yy = %lookup( *blank : $_Loghi );
148600131007             endif;
148700131007             if  yy > *zero;
148800131009               if  SQLcode = 100;
148900131007                 $_Loghi(yy) = SQLtmd;
149000131007               else;
149100131007                 $_Loghi(yy) = wAR8tmd;
149200131007               endif;
149300131007               $_Sped§§(yy) += 1;
149400131007               $_nPdf§§(yy) += SQLrec;
149500131007             endif;
149600131007         endsl;
149700131007
149800131009         // -?Chiusura del cursore?
149900131009         exec sql   close   C2;
150000131007
150100131007       ENDSR;
150200090507
150300090507       //--------------------------------------------------------------
150400090507       //?Stampa segnalazione dell'errore rilevato via SQL             ?
150500090507       //--------------------------------------------------------------
150600090507       BEGSR  sr_PrintErrSQL;
150700090507
150800101206         // -?Stampa del Dump?
150900090507         Dump(A);
151000090507
151100101206         // -?Stampa del Job-Log?
151200090507         Qcmd = 'DSPJOBLOG job(*) output(*print)';
151300090507         ExecuteCommand ( Qcmd : %len(Qcmd) );
151400090507
151500101206         // -?Apertura del printer-file?
151600090507         if  Not %open(QSYSPRT);
151700090507           ExecuteCommand ( c_OvrPrtF : %len(c_OvrPrtF) );
151800090507           open  QSYSPRT;
151900090507         endif;
152000090507
152100101206         // -?Stampa dell'errore in QSYSPRT?
152200090507         clear  ds_Temp;
152300090507         ds_Temp = wSQL;
152400090507         if  *inOF;
152500090507           except  PRTtxt;
152600090507           *inOF = *off;
152700090507         endif;
152800090507         except  PRTerr1;
152900090507         *inOF = *off;
153000090507         For  xx = 1  To  %elem($Temp);
153100090507           if  $Temp(xx) = *blank;
153200090507             leave;
153300090507           endif;
153400090507           if  *inOF;
153500090507             except  PRTtxt;
153600090507             *inOF = *off;
153700090507           endif;
153800090507           except  PRTerr2;
153900090507         EndFor;
154000090507
154100101206         // -?Chiusura del programma?
154200090507         exsr  sr_RoutEnd;
154300090507
154400090507       ENDSR;
154500090507
154600090507       //--------------------------------------------------------------
154700090507       //?Operazioni finali.                                           ?
154800090507       //--------------------------------------------------------------
154900090507       BEGSR  sr_RoutEnd;
155000090507
155100101206         // -?Chiusura del printer-file?
155200090507         if  %open(QSYSPRT);
155300090507           close  QSYSPRT;
155400090507           ExecuteCommand ( c_DltOvr : %len(c_DltOvr) );
155500090507         endif;
155600101206
155700101206         // -?Chiusura work-file?
155800101206         close  WFPDL00F;
155900090507
156000101206         // -?"Chiusura" pgm?
156100090507         return;
156200090507
156300090507       ENDSR;
156400090507
156500090507      /end-free
156600090507
156700090507       //--------------------------------------------------------------
156800090507       //?S P O O L - F I L E S                                        ?
156900090507       //--------------------------------------------------------------
157000090507
157100090507     oQSYSPRT   e            PRTtxt            1
157200090507     o                       RSUT
157300090507     o                                        +   6 'LISTA ERRORI RILE-
157400090507     o                                              VATI IN FASE DI RI-
157500090507     o                                              CERCA NOMINATIVI I-
157600090507     o                                              N BOLLE SEDE PER G-
157700090507     o                                              .D.F.'
157800090507     o                       SDSpgm           +   6
157900090507     o                       wDate         y  +   3
158000090507     o          e            PRTtxt      1
158100090507     o                       KNSIF
158200090507     o                       KNMUS            +   1
158300090507     o                                        +   5 '------------------
158400090507     o                                              -------------------
158500090507     o                                              -------------------
158600090507     o                                              -------------------
158700090507     o                                              -----'
158800090507     o                                        +   6 'Pag.'
158900090507     o                       Page          z  +   0
159000090507     o                       wTime            +   5 '  :  :  '
159100090507     o          e            PRTerrTSD   2
159200090507     o                                              'Reperito nuovo -
159300090507     o                                              "Tipo Stampa DDT" -
159400090507     o                                              in tab. "TSD":'
159500090507     o                       TBEke1           +   1
159600090507     o          e            PRTerrTSD   1
159700090507     o                                              'AVVISO: le righe -
159800090507     o                                              di tale tipo nelle-
159900090507     o                                               packing-list verr-
160000090507     o                                              anno contate come -
160100090507     o                                              "STANDARD CON LOGO-
160200090507     o                                              ".'
160300090507     o          e            PRTerrAR8   2
160400090507     o                                              'Reperito nuovo -
160500090507     o                                              "Tipo Stampa DDT" -
160600090507     o                                              in tab. "AR8":'
160700090507     o                       §AR8tsd          +   1
160800090507     o                                        +   1 'per il logo'
160900090507     o                       TBEke1           +   1
161000090507     o          e            PRTerrAR8   1
161100090507     o                                              'AVVISO: le righe -
161200090507     o                                              della packing-list-
161300090507     o                                               relativa alla spe-
161400090507     o                                              dizione'
161500090507     o                       SQLlnp           +   1
161600090507     o                                        +   0 '/'
161700090507     o                       SQLnrs        z  +   0
161800090507     o                                        +   0 '/'
161900090507     o                       SQLnsp        z  +   0
162000090507     o                                        +   1 'anno'
162100090507     o                       SQLaas        z  +   1
162200090507     o          e            PRTerrAR8   1
162300090507     o                                        +   3 'sono contate come-
162400090507     o                                               "STANDARD CON LOG-
162500090507     o                                              O".'
162600090507     o          e            PRTerr1     2
162700090507     o                                              'RILEVATO ERRORE: +
162800090507     o                                               SQLCODE'
162900090507     o                       SQLcode       k  +   1
163000090507     o                                        +   1 'SQLSTATE'
163100090507     o                       SQLstate         +   1
163200090507     o                                        +   1 'DURANTE L''ESECUZ-
163300090507     o                                              IONE DEL SEGUENTE -
163400090507     o                                              COMANDO SQL:'
163500090507     o          e            PRTerr1     0  1
163600090507     o                                              'RILEVATO ERRORE: +
163700090507     o                                               SQLCODE'
163800090507     o                       SQLcode       k  +   1
163900090507     o                                        +   1 'SQLSTATE'
164000090507     o                       SQLstate         +   1
164100090507     o          e            PRTerr2     1
164200090507     o                       $Temp(xx)        +   3
164300060327
164400090507       //--------------------------------------------------------------
164500090507       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
164600090507       //--------------------------------------------------------------
164700090507
164800101206** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie di Sede e di Fil.?
164900101206SETRAS  GAITRAAZM FILTRA201
165000101206AS888   GAITRAAZP FILTRAPRD
165100101206** -?$Sql:?Stringa SQL da eseguire?----------------------------------*
165200060331SELECT BLPksc, BLPrsm,                                                  1
165300060331       AR8tmd,                                                          2 a1
165400060331       'STD2' as AR8tmd,                                                3 a2
165500060330       AR8aas, AR8lnp, AR8nrs, AR8nsp,                                  4
165600060330       '* ', count(*)                                                   5 b1
165700060330       'C*', count(*)                                                   6 b2
165800060330       'Z1', count(*)                                                   7 b3
165900060330       'S*', count(*)                                                   8 b4
166000101206FROM   &LibFil1  /FIAR800F   INNER JOIN   &LibFil2  /FNBLP00F           9
166100060330ON     AR8aas = BLPaas                                                 10
166200060330  and  AR8lnp = BLPlnp                                                 11
166300060330  and  AR8nrs = BLPnrs                                                 12
166400060330  and  AR8nsp = BLPnsp                                                 13
166500090506  and  AR8atb = BLPatb                          --- NO ---   => libero 14
166600130927WHERE  AR8atb = ' '                                                    15
166700060330  and  substr(digits(BLPksc), 4, 4) <> '9999'                          16
166800060330  and  (BLPaas*10000+BLPmgs)  between  &LSE4dti and &LSE4dtf           17
166900060330  and  AR8tmd = 'STD1'                                                 18 c1
167000090507  and  AR8tmd NOT in (                                                 19 c2
167100090507  and  AR8tmd in (                                                     20 c3
167200090506  and (AR8trk like 'C%'  or  AR8trk like 'O%')                         21 d1
167300090512  and  AR8trk in ('Z1')                                                22 d2
167400090512  and  AR8trk in ('S2', 'S3')                                          23 d3
167500090507  and  BLPcbo in ('1 ', '4 ', '5 ')                                    24
167600090506GROUP  BY  BLPksc, BLPrsm, AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp      25
