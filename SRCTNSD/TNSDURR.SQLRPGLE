000100071128     ***********************************************************************************************
000200071128     **
000300150422     ** Aggiornamento inventario PDA
000400071128     **
000500071128     ***********************************************************************************************
000600150507       // per complilare questo pgm si deve aggiungere in LIBL la FILTRAPRD
000700071128
000800150423
000900150423      //********************************************************************************************
001000150423      //?Specifiche di controllo.                                     ?
001100150423      //********************************************************************************************
001200150423
001300150423     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001400150805     h dftactgrp(*no) actgrp('PDA_AFARIA')
001500150423     h alwnull(*inputonly)
001600150423     h bnddir('UBBNDDIR')
001700150423       // per dire che i calcoli intermedi devono avere lo stesso num. decimali del risultato
001800150423     h expropts(*resdecpos)
001900141126
002000141126      //********************************************************************************************
002100141126      //
002200141126      // Definizione archivi                                       ?
002300141126      //
002400141126      //********************************************************************************************
002500150423     fUNANA07L  uf   e           k disk    usropn
002600150505
002700150423     fTILAP00F  uf   e             disk    usropn
002800150505
002900150505     fUNMOV05L  if   e           k disk    usropn
003000150423
003100150423     FPRTF198   O    f  198        PRINTER
003200150423     F                                     oflind(*inoa)
003300141126      //********************************************************************************************
003400141126      //
003500141126      // Definizione key-list globali                              ?
003600141126      //
003700141126      //********************************************************************************************
003800150422      // UNANA07L
003900150422     d k02UNANA07    e ds                  extname(UNANA07L : *key)
004000141126     d                                     prefix(k_)  inz
004100150422
004200150505      // UNMOV05L
004300150505     d k04UNMOV05    e ds                  extname(UNMOV05L : *key)
004400150505     d                                     prefix(k_)  inz
004500150505
004600150422      //********************************************************************************************
004700150422      //
004800150423      // Definizione data structure
004900150422      //
005000150422      //********************************************************************************************
005100150423     d TILAPDS       e ds                  inz  extname(TILAP00F)
005200150507     d UNANADS       e ds                  inz  extname(UNANA00F)
005300150507     d FIPLG00F      e ds                  inz
005400150507     d FIPSL00F      e ds                  inz
005500150423
005600150423       // -?Status ds?
005700150423     d Status         sds
005800150423     d  SDSpgm           *proc
005900150423     d  SDSprm           *parms
006000150423     d  SDSuser              254    263
006100150422
006200071128      //********************************************************************************************
006300071128      //
006400071128      // Definizione prototipi procedure.
006500071128      //
006600071128      //********************************************************************************************
006700141126     D/COPY GAITRASRC/SRCPROTOPR,UBCHKPWD
006800150507     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006900150507     d/copy gaitrasrc/srcProtoPr,ubRtvNetA
007000071128
007100071128      //********************************************************************************************
007200071128      //
007300150422      // Definizione variabili
007400071128      //
007500071128      //********************************************************************************************
007600150507
007700150505     D* parametri
007800150811     D KPJBA         E DS
007900150811     D*
008000150505     d Order           s              1
008100150507
008200150423     d* varibili per stampa
008300150423     d wLAPDATESTR     s             20
008400150505     d wLAPMATRICN     s             30
008500150423     d wLAPBRAND       s              9
008600150423     d wLAPUSER        s             10
008700150423     d wLAPDATCONN     s             20
008800150423     d wLAPULTIP       s             15
008900150508     d wUNATIP         s              2
009000150508     d wUNACOD         s              5
009100150508     d wUNAMAR         s             15
009200150508     d wUNAMAT         s             15
009300150520     d wUNAMAG         s              3
009400150520     d Motivo          S             70a
009500150507
009600150507     d* varibili varie
009700150507     d currSysNeta     s              8a   inz
009800150507     d wLib            s             10a   inz
009900150507     d wPLGDATORA      s                   inz  like(PLGDATORA)
010000150507     d wPLGPRFC        s                   inz  like(PLGPRFC)
010100150507     d wPSLIDDISP      s                   inz  like(PSLIDDISP)
010200150507     d wPSLDTORAR      s                   inz  like(PSLDTORAR)
010300150507     d wPSLINDIP       s                   inz  like(PSLINDIP)
010400150507     d wPSLPRFC        s                   inz  like(PSLPRFC)
010500150507     d Matricola       S             15a
010600150507
010700150423     d* Flags booleani
010800150423     d $EoF            s               n   inz
010900150424     d noAggUNANA      s               n   inz('0')
011000150505
011100150507     d* schiere dei motivi di non congruenza
011200150520     d MotiviSK        s             70    dim(13)
011300150423     d                                     ctdata   perrcd( 1)
011400150514     d NrMotSK         s              5s 0 dim(13)
011500150505     d TotMotivi       s              7s 0
011600150423
011700150423     D* varibili d wrk
011800150505     d wI              s              3s 0
011900150423     d wPwd            S             20a   inz
012000150423     d wDate           s               d
012100150507     d wDate80         s              8s 0
012200150423     d wHour           s               t
012300150813     d wHourEnd        s               t
012400141126     d wSQL            s           2048    inz  varying
012500150423     d dataTest        s             10    inz
012600150423     d nrRelRcd        s              9s 0
012700150604     d w1s0            s              1s 0
012800131017
012900131017     D* Costanti
013000150422     D numeri          c                   '1234567890'
013100150422     D up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
013200150422     D lo              c                   'abcdefghijklmnopqrstuvwxyz'
013300150505
013400150505     C     *ENTRY        PLIST
013500150811     C                   PARM                    Kpjba
013600150811
013700150811     C                   EVAL      Order = %subst(Kpjbu:1:1)
013800081003
013900131017      /free
014000150422
014100150422       //--------------------------------------------------------------
014200150422       //?M A I N - L I N E                                            ?
014300150422       //--------------------------------------------------------------
014400150422
014500150507       // - Operazioni iniziali
014600150422       exsr sr_RoutInz;
014700150422
014800150507       // - Ciclo di reperimento dati da TILAP + operazioni collegate
014900150422       doW  Not $EoF;
015000150422         exsr  sr_ReadCursor;
015100150422       enddo;
015200150422
015300150507       // - Ctrl le ricezioni da PDA ne le spunte da pistola
015400150507       exsr CtrlRicezSpunte;
015500150507
015600150507       // - Operazioni finali
015700150507       exsr sr_RoutEnd;
015800150422
015900150422       //--------------------------------------------------------------
016000150422       //?Operazioni iniziali.                                         ?
016100150422       //--------------------------------------------------------------
016200150422       BEGSR  sr_RoutInz;
016300131017
016400150422         // Inizializzazioni
016500150422
016600150507         // Inizializzazioni parametri esecuzione comandi sql
016700150507         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
016800150507
016900150422         // Reperimento data odierna (fmt aaaa/mm/gg)
017000150423         wDate = %date();
017100150507         wDate80 = %dec(%date() : *ISO);
017200150423         wHour = %time();
017300150507
017400150507         // - Verifica del sistema AS/400 corrente
017500150507         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
017600150507           exsr sr_RoutEnd;
017700150507         endif;
017800150507         // - Impostazione libreria
017900150507         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
018000150507           wLib = 'FILTRA201';
018100150507         else;
018200150507           wLib = 'FILTRAPRD';
018300150507         endif;
018400141126
018500150422         // estraggo LOG AFARIA
018600150423         wSQL = 'select tilap00f.* , rrn(tilap00f) +
018700150422         from TILAP00F +
018800150505         where LAPMATRICN <> ''CEDAFARIA'' and +
018900150505               LAPMATRICN not like ''CEDPDA%'' +
019000150505               order by +
019100150505               DATE(LAPDATCONN) desc, ';
019200150505         select;
019300150505         when Order = '1';
019400150505           wSQL = wSQL +
019500150505               'LAPBRAND, +
019600150505               LAPMATRICN';
019700150505         when Order = '2';
019800150505           wSQL = wSQL +
019900150505               'LAPUSER, +
020000150505               LAPMATRICN';
020100150505         endsl;
020200141126
020300150422         // Dichiarazione cursore
020400150422         exec sql   prepare S0   from :wSQL;
020500150422         exec sql   declare C0   cursor   for S0;
020600150422         // Apertura del cursore
020700150422         exec sql   open C0;
020800150423
020900150423         except testata;
021000150512         except testata1;
021100150422
021200150422       ENDSR;
021300150422
021400150422       //--------------------------------------------------------------
021500150422       //?Reperimento Dati da TILAP                                    ?
021600150422       //--------------------------------------------------------------
021700150422       BEGSR  sr_ReadCursor;
021800150422
021900150422         // Lettura cursore
022000150424         exec sql   fetch next   from C0   into :TILAPDS, :nrRelRcd ;
022100150423
022200150423         if *inOA = *on;
022300150423           except testata;
022400150512           except testata1;
022500150423         endif;
022600150423
022700150422         // leggo una riga alla volta
022800150422         select;
022900150422           // se ho un errore sql esco
023000150422           when  SQLcode < *zero;
023100150422             $EoF = *on;
023200150422             // Chiusura del cursore
023300150422             exec sql   close C0;
023400150422             // esco dalla routine
023500150422             leavesr;
023600150422           // se ho finito il file, esco
023700150422           when  SQLcode = 100;
023800150422             $EoF = *on;
023900150422             // Chiusura del cursore
024000150422             exec sql   close C0;
024100150422             // esco dalla routine
024200150422             leavesr;
024300150422           // negli altri casi, prosieguo
024400150422           other;
024500150422             exsr testRcdTILAP;
024600150422         endsl;
024700150422
024800150422       ENDSR;
024900150422
025000150422       //--------------------------------------------------------------
025100150422       //?Test sui dati del rcd di TILAP                               ?
025200150422       //--------------------------------------------------------------
025300150422       BEGSR  testRcdTILAP;
025400150422
025500150424         select;
025600150424           // se il dispositivo recuperato non è PDA o PISTOLA, stampare
025700150424           when %subst(%xlate(lo:up:LAPUSER):1:2) <> 'AU' and
025800150424             %subst(%xlate(lo:up:LAPUSER):1:2) <> 'PR';
025900150424             motivo = MotiviSK(1);
026000150505             NrMotSK(1) += 1;
026100150424             exsr stampaElenco;
026200150424           // se il brand del dispositivo recuperato non è Datalogic o Symbol, stampare
026300150424           when LAPBRAND <> 'Datalogic' and
026400150424                LAPBRAND <> 'Symbol';
026500150424             motivo = MotiviSK(8);
026600150505             NrMotSK(8) += 1;
026700150424             exsr stampaElenco;
026800150424           other;
026900150424             exsr leggoUNA07;
027000150424         endsl;
027100150422
027200150422       ENDSR;
027300150422
027400150422       //--------------------------------------------------------------
027500150422       //?leggo UNANA07L rispetto al brand                            ?
027600150422       //--------------------------------------------------------------
027700150422       BEGSR  leggoUNA07;
027800150422
027900150505         noAggUNANA = *off;
028000150505
028100150423         if not %open(UNANA07L);
028200150423           open UNANA07L;
028300150423         endif;
028400150505
028500150505         if not %open(UNMOV05L);
028600150505           open UNMOV05L;
028700150505         endif;
028800150423
028900150422         select;
029000150422           // PDA Datalogic
029100150422           when LAPBRAND = 'Datalogic' and
029200150424                %subst(%xlate(lo:up:LAPUSER):1:2) = 'AU';
029300150423             K_UNATIP = 74;
029400150422             K_UNAMAT = LAPMATRICN;
029500150514             chain(e)  %kds( k02UNANA07 )  UNANA000;
029600150423             if %found();
029700150423               exsr ultimeVerifiche;
029800150423             else;
029900150423               motivo = MotiviSK(2);
030000150505               NrMotSK(2) += 1;
030100150423               exsr stampaElenco;
030200150423               leavesr;
030300150423             endif;
030400150423
030500150422           // Pistola Datalogic
030600150422           when LAPBRAND = 'Datalogic' and
030700150424                %subst(%xlate(lo:up:LAPUSER):1:2) = 'PR';
030800150423             K_UNATIP = 77;
030900150422             K_UNAMAT = LAPMATRICN;
031000150514             chain(e)  %kds( k02UNANA07 )  UNANA000;
031100150423             if %found();
031200150423               exsr ultimeVerifiche;
031300150423             else;
031400150423               motivo = MotiviSK(3);
031500150505               NrMotSK(3) += 1;
031600150423               exsr stampaElenco;
031700150423               leavesr;
031800150423             endif;
031900150423
032000150422           // PDA Symbol
032100150422           when LAPBRAND = 'Symbol' and
032200150424                %subst(%xlate(lo:up:LAPUSER):1:2) = 'AU';
032300150423             K_UNATIP = 80;
032400150422             K_UNAMAT = LAPMATRICN;
032500150514             chain(e)  %kds( k02UNANA07 )  UNANA000;
032600150423             if %found();
032700150423               exsr ultimeVerifiche;
032800150423             else;
032900150505               // secondo tentativo
033000150505               K_UNAMAT = 'S' + LAPMATRICN;
033100150514               chain(e)  %kds( k02UNANA07 )  UNANA000;
033200150505               if %found();
033300150505                 exsr ultimeVerifiche;
033400150505               else;
033500150505                 // terzo tentativo (controllo se è in riparazione);
033600150505                 // UNANA non trovato quindi non va aggiornato;
033700150505                 noAggUNANA = *on;
033800150505                 K_UNMTIP = K_UNATIP;
033900150505                 K_UNMMAT = 'S' + LAPMATRICN;
034000150505                 K_UNMCAU = 'RIPA';
034100150514                 chain  %kds( k04UNMOV05:3 )  UNMOV000;
034200150505                 if %found();
034300150505                   // prima di passare alle ultime verifiche, controllo che la data riparazione
034400150505                   // sia superiore della data connessione
034500150505                   if UNMDTM >
034600150505                     %dec(%subst(%char(%dec(LAPDATCONN))
034700150505                      :1:8):8:0);
034800150505                       exsr ultimeVerifiche;
034900150505                   else;
035000150505                     motivo = MotiviSK(9);
035100150505                     // giorno
035200150505                     %subst(motivo:38:2) =
035300150506                     %subst(%char(UNMDTM):7:2);
035400150505                     // mese
035500150505                     %subst(motivo:41:2) =
035600150506                     %subst(%char(UNMDTM):5:2);
035700150505                     // anno
035800150505                     %subst(motivo:44:4) =
035900150506                     %subst(%char(UNMDTM):1:4);
036000150505                     NrMotSK(9) += 1;
036100150505                     exsr stampaElenco;
036200150505                     leavesr;
036300150505                   endif;
036400150505                 else;
036500150505                   // quarto tentativo (controllo se è in riparazione)
036600150505                   K_UNMTIP = K_UNATIP;
036700150505                   K_UNMMAT = LAPMATRICN;
036800150505                   K_UNMCAU = 'RIPA';
036900150505                   chain  %kds( k04UNMOV05:3 )  UNMOV000;
037000150505                   if %found();
037100150505                     // prima di passare alle ultime verifiche, controllo che la data riparazione
037200150505                     // sia superiore della data connessione
037300150505                     if UNMDTM >
037400150505                       %dec(%subst(%char(%dec(LAPDATCONN))
037500150505                        :1:8):8:0);
037600150505                         exsr ultimeVerifiche;
037700150505                     else;
037800150505                       motivo = MotiviSK(9);
037900150505                       // giorno
038000150505                       %subst(motivo:38:2) =
038100150506                       %subst(%char(UNMDTM):7:2);
038200150505                       // mese
038300150505                       %subst(motivo:41:2) =
038400150506                       %subst(%char(UNMDTM):5:2);
038500150505                       // anno
038600150505                       %subst(motivo:44:4) =
038700150506                       %subst(%char(UNMDTM):1:4);
038800150505                       NrMotSK(9) += 1;
038900150505                       exsr stampaElenco;
039000150505                       leavesr;
039100150505                     endif;
039200150505                   else;
039300150505                     motivo = MotiviSK(4);
039400150505                     NrMotSK(4) += 1;
039500150505                     exsr stampaElenco;
039600150505                     leavesr;
039700150505                   endif;
039800150505                 endif;
039900150505               endif;
040000150505             endif;
040100150423
040200150422           // Pistola Symbol
040300150422           when LAPBRAND = 'Symbol' and
040400150424                %subst(%xlate(lo:up:LAPUSER):1:2) = 'PR';
040500150423             K_UNATIP = 83;
040600150422             K_UNAMAT = LAPMATRICN;
040700150514             chain(e)  %kds( k02UNANA07 )  UNANA000;
040800150423             if %found();
040900150423               exsr ultimeVerifiche;
041000150423             else;
041100150505               // secondo tentativo
041200150505               K_UNAMAT = 'S' + LAPMATRICN;
041300150514               chain(e)  %kds( k02UNANA07 )  UNANA000;
041400150505               if %found();
041500150505                 exsr ultimeVerifiche;
041600150505               else;
041700150505                 // terzo tentativo (controllo se è in riparazione);
041800150505                 // UNANA non trovato quindi non va aggiornato;
041900150505                 noAggUNANA = *on;
042000150505                 K_UNMTIP = K_UNATIP;
042100150505                 K_UNMMAT = 'S' + LAPMATRICN;
042200150505                 K_UNMCAU = 'RIPA';
042300150505                 chain  %kds( k04UNMOV05:3 )  UNMOV000;
042400150505                 if %found();
042500150505                   // prima di passare alle ultime verifiche, controllo che la data riparazione
042600150505                   // sia superiore della data connessione
042700150505                   if UNMDTM >
042800150505                     %dec(%subst(%char(%dec(LAPDATCONN))
042900150505                      :1:8):8:0);
043000150505                       exsr ultimeVerifiche;
043100150505                   else;
043200150505                     motivo = MotiviSK(9);
043300150505                     // giorno
043400150505                     %subst(motivo:38:2) =
043500150506                     %subst(%char(UNMDTM):7:2);
043600150505                     // mese
043700150505                     %subst(motivo:41:2) =
043800150506                     %subst(%char(UNMDTM):5:2);
043900150505                     // anno
044000150505                     %subst(motivo:44:4) =
044100150506                     %subst(%char(UNMDTM):1:4);
044200150505                     NrMotSK(9) += 1;
044300150505                     exsr stampaElenco;
044400150505                     leavesr;
044500150505                   endif;
044600150505                 else;
044700150505                   // quarto tentativo (controllo se è in riparazione)
044800150505                   K_UNMTIP = K_UNATIP;
044900150505                   K_UNMMAT = LAPMATRICN;
045000150505                   K_UNMCAU = 'RIPA';
045100150505                   chain  %kds( k04UNMOV05:3 )  UNMOV000;
045200150505                   if %found();
045300150505                     // prima di passare alle ultime verifiche, controllo che la data riparazione
045400150505                     // sia superiore della data connessione
045500150505                     if UNMDTM >
045600150505                       %dec(%subst(%char(%dec(LAPDATCONN))
045700150505                        :1:8):8:0);
045800150505                         exsr ultimeVerifiche;
045900150505                     else;
046000150505                       motivo = MotiviSK(9);
046100150505                       // giorno
046200150505                       %subst(motivo:38:2) =
046300150506                       %subst(%char(UNMDTM):7:2);
046400150505                       // mese
046500150505                       %subst(motivo:41:2) =
046600150506                       %subst(%char(UNMDTM):5:2);
046700150505                       // anno
046800150505                       %subst(motivo:44:4) =
046900150506                       %subst(%char(UNMDTM):1:4);
047000150505                       NrMotSK(9) += 1;
047100150505                       exsr stampaElenco;
047200150505                       leavesr;
047300150505                     endif;
047400150505                   else;
047500150505                     motivo = MotiviSK(5);
047600150505                     NrMotSK(5) += 1;
047700150505                     exsr stampaElenco;
047800150505                     leavesr;
047900150505                   endif;
048000150505                 endif;
048100150505               endif;
048200150505             endif;
048300150423
048400150422         endsl;
048500150422
048600150422       ENDSR;
048700150422
048800150422       //--------------------------------------------------------------
048900150423       //?Ultime verifiche per decidere se aggiornare                ?
049000150422       //--------------------------------------------------------------
049100150423       BEGSR  ultimeVerifiche;
049200150422
049300150506         // se IP non inizia con 10. ne con 172. non aggiornare e stampare motivo = *
049400150506         if %subst(LAPULTIP:1:3) <> '10.' AND
049500150506            %subst(LAPULTIP:1:4) <> '172.';
049600150423           motivo = MotiviSK(6);
049700150505           NrMotSK(6) += 1;
049800150423           exsr stampaElenco;
049900150423           leavesr;
050000150423         endif;
050100150423
050200150423         // se Data non formalmente corretta non aggiornare e stampare motivo = *
050300150423         // se la data di arrivo è uguale alla data di partenza ma dovrebbe essere diversa, errore
050400150423         dataTest =
050500150504          UBFMTDATE_Convert(%subst(%char(LAPDATCONN):1:10)
050600150504                                              :'YYYY-MM-DD'
050700150504                                              :'YYYYMMDD');
050800150423         // se Data non formalmente corretta non aggiornare e stampare motivo = *
050900150504         if dataTest = %subst(%char(LAPDATCONN):1:10);
051000150423           motivo = MotiviSK(7);
051100150505           NrMotSK(7) += 1;
051200150423           exsr stampaElenco;
051300150423           leavesr;
051400150423         endif;
051500150423
051600150424         // se la data ultima rilevazione è >= data/ora connessione Afaria, non è un errore
051700150424         // ma devo solo cancellare il rcd di TILAP perché UNANA è già stato aggiornato per
051800150424         // una associazione più recente
051900150609         if UNADSL>0 and %timestamp(%date(UNADSL)) > LAPDatConn;
052000150424           noAggUNANA = *on;
052100150423         endif;
052200150423
052300150423         // se tutte le verifiche sono andate bene, aggiorno
052400150423         exsr aggiornaUNANA;
052500150423
052600150423       ENDSR;
052700150423
052800150423       //--------------------------------------------------------------
052900150423       //?Aggiorno UNAMA                                            ?
053000150423       //--------------------------------------------------------------
053100150423       BEGSR  aggiornaUNANA;
053200150423
053300150424         if noAggUNANA <> *on;
053400150507           monitor;
053500150424           UNAMTA = LAPULTIP;
053600150424           UNADSL = %dec(%subst(%char(%dec(LAPDATCONN))
053700150424                    :1:8):8:0);
053800150507           UNAMDR = 'AFARIA '+ %char(wDate80);
053900150506           UNAMVD = LAPUSER;
054000150604           // controllo e scrivo di chi è la SIM
054100150609           if LAPSIM > *blank;
054200150609           if %subst(LAPSIM:20:1) <> *blank;
054300150604             // se il 20° char è numerico, è una SIM TIM, altrimenti è VODAFONE
054400150604             monitor;
054500150604             w1s0 = %dec(%subst(LAPSIM:20:1):1:0);
054600150604             UNADES = 'SIM TIM ' + %trim(LAPSIM);
054700150604             on-error;
054800150604               UNADES = 'SIM VOD ' + %trim(LAPSIM);
054900150604             endmon;
055000150604           endif;
055100150609           endif;
055200150423
055300150424           update UNANA000;
055400150507           on-error;
055500150508             leavesr;
055600150507           endmon;
055700150424         endif;
055800150424         noAggUNANA = *off;
055900150423
056000150423         // se aggiorno UNANA posso eliminare il corrispondente rcd di TILAP
056100150423         if not %open(TILAP00F);
056200150423           open TILAP00F;
056300150423         endif;
056400150423         delete nrRelRcd TILAP00F;
056500150423
056600150423       ENDSR;
056700150423
056800150423       //--------------------------------------------------------------
056900150507       // Controllo ricezioni da PDA e spunte da pistole per decidere come aggiornare
057000150423       //--------------------------------------------------------------
057100150507       BEGSR  CtrlRicezSpunte;
057200150507
057300150507         $EoF = *off;
057400150505
057500150507         // - Chiusura precedente cursore SQL
057600150507         exsr  sr_CloseCursor;
057700150507
057800150507         // cursore per estrazione PDA e Pistole
057900150928         // in cui dichiaro che:
058000150928         // insensitive = faccio una foto dei dati in modo che l'update non me lo falsi
058100150507         wSQL = 'select unana00f.* , rrn(unana00f) +
058200150507         from UNANA00F +
058300150507         where UNATIP in (74, 77, 80, 83) and +
058400150507         UNAMDR <> ''AFARIA ';
058500150507         wSQL = wSQL + %char(wDate80) + '''';
058600150507         // Dichiarazione cursore
058700150507         exec sql   prepare S4   from :wSQL;
058800150928         exec sql   declare C4   insensitive cursor   for S4;
058900150507         // Apertura del cursore
059000150507         exec sql   open C4;
059100150508
059200150512         except testata;
059300150512         except testata2;
059400150507
059500150507         doW  Not $EoF;
059600150507           exsr  sr_ReadCursor_2;
059700150507         enddo;
059800150507
059900150507       ENDSR;
060000150507
060100150507       //--------------------------------------------------------------
060200150507       //?Reperimento Dati da UNANA00F                                ?
060300150507       //--------------------------------------------------------------
060400150507       BEGSR  sr_ReadCursor_2;
060500150507
060600150507         // Lettura cursore
060700150507         exec sql   fetch next   from C4   into :UNANADS, :nrRelRcd ;
060800150507
060900150507         // leggo una riga alla volta
061000150507         select;
061100150507           // se ho un errore sql esco
061200150507           when  SQLcode < *zero;
061300150507             $EoF = *on;
061400150507             // Chiusura del cursore
061500150507             exec sql   close C4;
061600150507             // esco dalla routine
061700150507             leavesr;
061800150507           // se ho finito il file, esco
061900150507           when  SQLcode = 100;
062000150507             $EoF = *on;
062100150507             // Chiusura del cursore
062200150507             exec sql   close C4;
062300150507             // esco dalla routine
062400150507             leavesr;
062500150507           // negli altri casi, prosieguo
062600150507           other;
062700150507             exsr testRcdPLG_PSL;
062800150507         endsl;
062900150507
063000150507       ENDSR;
063100150507
063200150507       //--------------------------------------------------------------
063300150507       // Controllo i valori letti con le spunte
063400150507       //--------------------------------------------------------------
063500150507       BEGSR  testRcdPLG_PSL;
063600150929
063700150929         // prendo in considerazione solo rcd non aggiornati o aggiornati con data più
063800150929         // vecchia di 3 gg
063900150929         if %subst(UNAMDR:8:8) <> *blank             AND
064000150929            %date(%dec(%subst(UNAMDR:8:8):8:0)) >=
064100150929            wDate - %days(3);
064200150929           leavesr;
064300150929         endif;
064400150929
064500150929         if *inOA = *on;
064600150929           except testata;
064700150929           except testata2;
064800150929         endif;
064900150507
065000150507         // se il device è di Symbol lo pulisco dell'eventuale 1° char = 'S'
065100150507         if UNATIP = 80 or UNATIP = 83;
065200150507           //memorizzo matricola senza il primo byte se questo è 'S'
065300150507           if %subst(UNAMAT:1:1)='S';
065400150507             matricola = %subst(UNAMAT:2:14) + ' ';
065500150507           else;
065600150507             matricola = UNAMAT;
065700150507           endif;
065800150520         else;
065900150520           matricola = UNAMAT;
066000150507         endif;
066100150507
066200150507         // se il device è un PDA
066300150507         if UNATIP = 80 or UNATIP = 74;
066400150507           exec sql
066500150629            select max(PLGDATORA), PLGPRFC
066600150629            into :wPLGDATORA, :wPLGPRFC
066700150629            FROM FIPLG00F WHERE PLGIDDISP in (:UNAMAT, :matricola)
066800150629            group by PLGPRFC;
066900150507
067000150507           // leggo l'unica riga che viene estratta
067100150507           select;
067200150507             // se ho un errore sql esco
067300150507             when  SQLcode < *zero;
067400150507               exsr sr_RoutEnd;
067500150507             // se ho finito il file (rcd non trovato), stampo anomalia
067600150507             when  SQLcode = 100;
067700150512               // se non ho trovato la spunta, stampo l'errore solo se il device non è appena
067800150512               // entrato a magazzino
067900150520               if UNAMAG <> 046 and
068000150520                  UNAMAG <> 199 and
068100150520                  UNAMAG <> 901 and
068200150520                  UNAMAG <> 903 and
068300150520                  UNAMAG <> 922 and
068400150520                  UNAMAG <> 923 and
068500150520                  UNAMAG <> 935 and
068600150520                  UNAMAG <> 950 and
068700150520                  UNAMAG <> 951 and
068800150520                  UNAMAG <> 989 and
068900150520                  UNAMAG <> 999;
069000150512                 motivo = MotiviSK(10);
069100150512                 // tipo
069200150512                 %subst(motivo:5:2) =
069300150512                 %editc(UNATIP:'X');
069400150512                 // matricola
069500150512                 %subst(motivo:10:15) =
069600150512                 UNAMAT;
069700150512                 NrMotSK(10) += 1;
069800150512                 exsr stampaElenco2;
069900150512               endif;
070000150507             // negli altri casi controlla la data
070100150507             other;
070200150507               // se la data della spunta è maggiore di quella di ultima rilevazione
070300150520               if UNADSL = 0
070400150507                  or
070500150507                  %dec(%subst(wPLGDATORA:1:8):8:0) >
070600150520                  UNADSL;
070700150507                 // aggiorno UNANA
070800150507                 wSQL = 'update UNANA00F set +
070900150514                         UNAMTA = '' '' , +
071000150507                         UNADSL = ' + %subst(wPLGDATORA:1:8) +
071100150514                        ', UNAMDR = ''SPUNTE ' + %char(wDate80) +
071200150514                        ''', UNAMVD = ''' + wPLGPRFC +
071300150514                        ''' where rrn(unana00f)=' + %char(nrRelRcd);
071400150507                 monitor;
071500150507                 exec sql   execute immediate  :wSQL;
071600150507                 on-error;
071700150508                   motivo = MotiviSK(12);
071800150508                   NrMotSK(12) += 1;
071900150508                   exsr stampaElenco2;
072000150507                 endmon;
072100150514                 select;
072200150514                   // se ho un errore sql stampo
072300150514                   when  SQLcode < *zero;
072400150514                     motivo = MotiviSK(12);
072500150514                     NrMotSK(12) += 1;
072600150514                     exsr stampaElenco2;
072700150514                   // se non trovo il rcd da updatare, stampo
072800150514                   when  SQLcode = 100;
072900150514                     motivo = MotiviSK(13);
073000150514                     NrMotSK(13) += 1;
073100150514                     exsr stampaElenco2;
073200150514                   // negli altri casi va bene così
073300150514                 endsl;
073400150507               // altrimenti
073500150507               else;
073600150507                 // stampo anomalia
073700150507                 motivo = MotiviSK(10);
073800150507                 // tipo
073900150507                 %subst(motivo:5:2) =
074000150507                 %editc(UNATIP:'X');
074100150507                 // matricola
074200150507                 %subst(motivo:10:15) =
074300150507                 UNAMAT;
074400150507                 NrMotSK(10) += 1;
074500150508                 exsr stampaElenco2;
074600150507               endif;
074700150507           endsl;
074800150507         endif;
074900150507
075000150507         // se il device è una Pistola
075100150507         if UNATIP = 77 or UNATIP = 83;
075200150507           exec sql
075300150507            SELECT PSLIDDISP, PSLDTORAR, PSLINDIP, PSLPRFC
075400150507            into :wPSLIDDISP, :wPSLDTORAR, :wPSLINDIP, :wPSLPRFC
075500150507            FROM  FILTRA201/FIPSL00F
075600150507            WHERE PSLIDDISP in (:UNAMAT, :matricola)
075700150520            order by PSLDTORAR desc
075800150507            fetch first 1 row only;
075900150507
076000150507           // leggo l'unica riga che viene estratta
076100150507           select;
076200150507             // se ho un errore sql esco
076300150507             when  SQLcode < *zero;
076400150507               exsr sr_RoutEnd;
076500150507             // se ho finito il file (rcd non trovato), stampo anomalia
076600150507             when  SQLcode = 100;
076700150512               // se non ho trovato la spunta, stampo l'errore solo se il device non è appena
076800150512               // entrato a magazzino
076900150520               if UNAMAG <> 046 and
077000150520                  UNAMAG <> 199 and
077100150520                  UNAMAG <> 901 and
077200150520                  UNAMAG <> 903 and
077300150520                  UNAMAG <> 922 and
077400150520                  UNAMAG <> 923 and
077500150520                  UNAMAG <> 935 and
077600150520                  UNAMAG <> 950 and
077700150520                  UNAMAG <> 951 and
077800150520                  UNAMAG <> 989 and
077900150520                  UNAMAG <> 999;
078000150512                 motivo = MotiviSK(11);
078100150512                 // tipo
078200150512                 %subst(motivo:9:2) =
078300150512                 %editc(UNATIP:'X');
078400150512                 // matricola
078500150512                 %subst(motivo:14:15) =
078600150512                 UNAMAT;
078700150512                 NrMotSK(11) += 1;
078800150512                 exsr stampaElenco2;
078900150512               endif;
079000150507             // negli altri casi controlla la data
079100150507             other;
079200150507               // se la data della spunta è maggiore di quella di ultima rilevazione
079300150520               if UNADSL = 0
079400150520                  or
079500150520                  %dec(%subst(wPSLDTORAR:1:8):8:0) >
079600150520                  UNADSL;
079700150507                 // aggiorno UNANA
079800150507                 wSQL = 'update UNANA00F set +
079900150514                         UNAMTA = ''' +
080000150519                         %char(%dec(%subst(wPSLINDIP:1:3):3:0)) + '.' +
080100150519                         %char(%dec(%subst(wPSLINDIP:4:3):3:0)) + '.' +
080200150519                         %char(%dec(%subst(wPSLINDIP:7:3):3:0)) + '.' +
080300150519                         %char(%dec(%subst(wPSLINDIP:10:3):3:0)) +
080400150514                        ''', UNADSL = ' + %subst(wPSLDTORAR:1:8) +
080500150514                        ', UNAMDR = ''SPUNTE ' + %char(wDate80) +
080600150514                        ''', UNAMVD = ''' + wPSLPRFC +
080700150514                        ''' where rrn(unana00f)=' + %char(nrRelRcd);
080800150507                 monitor;
080900150507                 exec sql   execute immediate  :wSQL;
081000150507                 on-error;
081100150508                   motivo = MotiviSK(12);
081200150508                   NrMotSK(12) += 1;
081300150508                   exsr stampaElenco2;
081400150507                 endmon;
081500150514                 select;
081600150514                   // se ho un errore sql stampo
081700150514                   when  SQLcode < *zero;
081800150514                     motivo = MotiviSK(12);
081900150514                     NrMotSK(12) += 1;
082000150514                     exsr stampaElenco2;
082100150514                   // se non trovo il rcd da updatare, stampo
082200150514                   when  SQLcode = 100;
082300150514                     motivo = MotiviSK(13);
082400150514                     NrMotSK(13) += 1;
082500150514                     exsr stampaElenco2;
082600150514                   // negli altri casi va bene così
082700150514                 endsl;
082800150507               // altrimenti
082900150507               else;
083000150507                 // stampo anomalia
083100150507                 motivo = MotiviSK(11);
083200150507                 // tipo
083300150507                 %subst(motivo:9:2) =
083400150507                 %editc(UNATIP:'X');
083500150507                 // matricola
083600150507                 %subst(motivo:14:15) =
083700150507                 UNAMAT;
083800150507                 NrMotSK(11) += 1;
083900150508                 exsr stampaElenco2;
084000150507               endif;
084100150507           endsl;
084200150507         endif;
084300150507
084400150507
084500150507
084600150507       ENDSR;
084700150507
084800150507       //--------------------------------------------------------------
084900150507       // Operazioni finali.
085000150507       //--------------------------------------------------------------
085100150507       BEGSR  sr_RoutEnd;
085200150507
085300150505         for wI = 1 to %elem(NrMotSK);
085400150505           TotMotivi = TotMotivi + NrMotSK(wI);
085500150505         endfor;
085600150505         if *inOA = *on;
085700150505           except testata;
085800150505         endif;
085900150505         // pulisco data
086000150505         %subst(MotiviSK(9):37:12) = *blank;
086100150813         wHourEnd = %time();
086200150505         except piede;
086300150423
086400150423         if %open(UNANA07L);
086500150423           close UNANA07L;
086600150423         endif;
086700150505
086800150505         if %open(UNMOV05L);
086900150505           close UNMOV05L;
087000150505         endif;
087100150423
087200150423         if %open(TILAP00F);
087300150423           close TILAP00F;
087400150423         endif;
087500150423
087600150423         // - Chiusura cursore SQL
087700150423         exsr  sr_CloseCursor;
087800150423
087900150423         // - Chiusura pgm
088000150423         *inlr = *on;
088100150423         return;
088200150423
088300150423       ENDSR;
088400150423
088500150423       //--------------------------------------------------------------
088600150508       //?Stampa elenco errori TILAP                                   ?
088700150423       //--------------------------------------------------------------
088800150423       BEGSR  stampaElenco;
088900150423
089000150423         wLAPDATESTR = %subst(%char(LAPDATESTR):1:20);
089100150505         wLAPMATRICN = LAPMATRICN;
089200150423         wLAPBRAND = LAPBRAND;
089300150423         wLAPUSER = LAPUSER;
089400150423         wLAPDATCONN = %subst(%char(LAPDATCONN):1:20);
089500150423         wLAPULTIP = LAPULTIP;
089600150423
089700150423         except dettaglio;
089800150423
089900150423       ENDSR;
090000150508
090100150508       //--------------------------------------------------------------
090200150508       //?Stampa elenco errori UNANA                                   ?
090300150508       //--------------------------------------------------------------
090400150508       BEGSR  stampaElenco2;
090500150508
090600150508         wUNATIP = %editc(UNATIP:'X');
090700150508         wUNACOD = %editc(UNACOD:'X');
090800150508         wUNAMAR = UNAMAR;
090900150508         wUNAMAT = UNAMAT;
091000150520         wUNAMAG = %editc(UNAMAG:'X');
091100150508
091200150508         except dettaglio2;
091300150508
091400150508       ENDSR;
091500150423
091600150423
091700150423       //--------------------------------------------------------------
091800150423       //?Chiusura cursore.                                            ?
091900150423       //--------------------------------------------------------------
092000150423       BEGSR  sr_CloseCursor;
092100150423
092200150507         // - Chiusura dei cursori
092300150507         exec sql   close C0;
092400150507         exec sql   close C4;
092500150422
092600150422       ENDSR;
092700150422
092800131017      /end-free
092900150423     OPRTF198   E            testata           2
093000150423     O                       SDSUSER
093100150423     O                                          109 '* Aggiornamento inventario'
093200150423     O                                          115 ' PDA *'
093300150423     O                       wDate              196
093400150423     O          E            testata     1
093500150423     O                       SDSPGM
093600150423     O                       wHour              196
093700150512     O          E            testata1    1
093800150423     O                                           16 'Data rilevazione'
093900150505     O                                           44 'Matricola normalizzata'
094000150423     O                                           59 'Brand'
094100150423     O                                           72 'Utente'
094200150423     O                                           98 'Data/ora conn.AFARIA'
094300150423     O                                          114 'IP conn.AFARIA'
094400150520     O                                          132 'Motivi'
094500150423     O          E            dettaglio   1
094600150423     O                       wLAPDATESTR         20
094700150505     O                       wLAPMATRICN         52
094800150423     O                       wLAPBRAND           63
094900150423     O                       wLAPUSER            76
095000150423     O                       wLAPDATCONN         98
095100150423     O                       wLAPULTIP          115
095200150423     O                       Motivo             196
095300150522     O          E            testata2          2
095400150522     O                                          150 'In questo giro vengono pre-
095500150522     O                                              si in considerazioni devic-
095600150522     O                                              e mai aggiornati o con aggi-
095700150522     O                                              ornamenti più vecchi d-
095800150522     O                                              i 4 gg da oggi.'
095900150522     O          E            testata2    1
096000150508     O                                            4 'Tipo'
096100150508     O                                           12 'Codice'
096200150508     O                                           19 'Marca'
096300150508     O                                           40 'Matricola'
096400150520     O                                           51 'Mag'
096500150520     O                                          132 'Motivi'
096600150508     O          E            dettaglio2  1
096700150508     O                       wUNATIP              3
096800150508     O                       wUNACOD             11
096900150508     O                       wUNAMAR             29
097000150508     O                       wUNAMAT             46
097100150520     O                       wUNAMAG             51
097200150508     O                       Motivo             196
097300150508     O          E            piede             2
097400150505     O                                              'Riepilogo errori'
097500150505     O          E            piede       1
097600150520     O                       MotiviSK(1)         72
097700150520     O                                           78 'nr.:'
097800150520     O                       NrMotSK(1)    z     84
097900150505     O          E            piede       1
098000150520     O                       MotiviSK(2)         72
098100150520     O                                           78 'nr.:'
098200150520     O                       NrMotSK(2)    z     84
098300150505     O          E            piede       1
098400150520     O                       MotiviSK(3)         72
098500150520     O                                           78 'nr.:'
098600150520     O                       NrMotSK(3)    z     84
098700150505     O          E            piede       1
098800150520     O                       MotiviSK(4)         72
098900150520     O                                           78 'nr.:'
099000150520     O                       NrMotSK(4)    z     84
099100150505     O          E            piede       1
099200150520     O                       MotiviSK(5)         72
099300150520     O                                           78 'nr.:'
099400150520     O                       NrMotSK(5)    z     84
099500150505     O          E            piede       1
099600150520     O                       MotiviSK(6)         72
099700150520     O                                           78 'nr.:'
099800150520     O                       NrMotSK(6)    z     84
099900150505     O          E            piede       1
100000150520     O                       MotiviSK(7)         72
100100150520     O                                           78 'nr.:'
100200150520     O                       NrMotSK(7)    z     84
100300150505     O          E            piede       1
100400150520     O                       MotiviSK(8)         72
100500150520     O                                           78 'nr.:'
100600150520     O                       NrMotSK(8)    z     84
100700150505     O          E            piede       1
100800150520     O                       MotiviSK(9)         72
100900150520     O                                           78 'nr.:'
101000150520     O                       NrMotSK(9)    z     84
101100150507     O          E            piede       1
101200150520     O                       MotiviSK(10)        72
101300150520     O                                           78 'nr.:'
101400150520     O                       NrMotSK(10)   z     84
101500150507     O          E            piede       1
101600150520     O                       MotiviSK(11)        72
101700150520     O                                           78 'nr.:'
101800150520     O                       NrMotSK(11)   z     84
101900150508     O          E            piede       1
102000150520     O                       MotiviSK(12)        72
102100150520     O                                           78 'nr.:'
102200150520     O                       NrMotSK(12)   z     84
102300150514     O          E            piede       1
102400150520     O                       MotiviSK(13)        72
102500150520     O                                           78 'nr.:'
102600150520     O                       NrMotSK(13)   z     84
102700150505     O          E            piede       1
102800150520     O                                           72 'Totale:'
102900150520     O                       TotMotivi     z     84
103000150813     O          E            piede       2
103100150813     O                                           26 'Inizio elaborazione alle:'
103200150813     O                       wHour               +1
103300150813     O                                           +2 'Fine elaborazione alle:'
103400150813     O                       wHourEnd            +1
103500150520** Motivi di stampa                                                   *
103600150520Dispositivo non PDA o Pistola                                         1
103700150520Non trovato rcd per PDA Datalogic                                     2
103800150520Non trovato rcd per Pistola Datalogic                                 3
103900150520Non trovato rcd per PDA Symbol                                        4
104000150520Non trovato rcd per Pistola Symbol                                    5
104100150520IP non inizia con 10. o 172.                                          6
104200150520Data errata                                                           7
104300150520Brand non Datalogic o Symbol                                          8
104400150520Conn.Afaria più recente data ripar. (xx/xx/xxxx)                      9
104500150520PDA xx / xxxxxxxxxxxxxxx no AFARIA no spunte > ult. rilevazione       10
104600150520Pistola xx / xxxxxxxxxxxxxxx no AFARIA no spunte > ult. rilevazione   11
104700150520Errore SQL aggiornamento UNANA00F                                     12
104800150520Non trovato rcd UNANA00F da agg. con SQL su rrn                       13
