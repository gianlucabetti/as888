000100121217       //==============================================================
000200121218       //?Scadenza abilitazione al servizio Strategi "EasySped WEB"    ?
000300121217       //==============================================================
000400141008
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
002100141029       // per dire che i calcoli intermedi devono avere lo stesso num. decimali del risultato
002200141029     h expropts(*resdecpos)
002300121217
002400121217       //--------------------------------------------------------------
002500121217       //?Dichiarazione file.                                          ?
002600121217       //--------------------------------------------------------------
002700121217
002800121217       //--------------------------------------------------------------
002900121217       //?Definizione costanti.                                        ?
003000121217       //--------------------------------------------------------------
003100121217
003200121217       // -?Codice utente conti?
003300121217     d c_Kut           c                   const(1)
003400121218
003500121218       // -?Costanti per la definizione delle schiere con i nomi?
003600121218       //  ?degli iSeries da elaborare e delle relative librerie?
003700140925     d c_NrSyst        c                   const(2)
003800140925     d c_NrLibr        c                   const(2)
003900121217
004000121217       //--------------------------------------------------------------
004100121217       //?Definizione schiere.                                         ?
004200121217       //--------------------------------------------------------------
004300121217
004400121218       // -?iSeries  &  Librerie con entrambi i file tabelle?
004500121218     d $iSystem        s                   like(currSysNetA)
004600121218     d                                     dim(c_NrSyst)
004700121218     d                                     ctdata   perrcd( 1)
004800121218     d $Libraries      s                   like(ds_Libr)
004900121218     d                                     dim(c_NrSyst)
005000121218     d                                     alt($iSystem)
005100121217
005200121217       //--------------------------------------------------------------
005300121217       //?Definizione aree dati.                                       ?
005400121217       //--------------------------------------------------------------
005500121217
005600121218       // -?Dati utente?
005700121218     d §AzUte        e ds                  extname(AZUTE00F)
005800121218     d                                     dtaara
005900121218     d §DatiUte      e ds                  extname(dDatiUte)
006000121218     d                                     dtaara
006100121217
006200121217       //--------------------------------------------------------------
006300121217       //?Definizione strutture dati.                                  ?
006400121217       //--------------------------------------------------------------
006500121217
006600121217       // -?Parametri in input?
006700121218     d KPJBA         e ds
006800121218
006900121218       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
007000140925     d ds_Libr         ds                  inz
007100121218     d   $Libr                       10    dim(c_NrLibr) inz
007200121217
007300121217       // -?Status?
007400121217     d Psds           sds
007500121217     d   SDSpgm          *proc
007600121217     d*//JobName             244    253                                         Job name
007700121217     d*//JobUser             254    263                                         User name
007800121217     d*//JobNumber           264    269s 0                                      Job number
007900121217
008000121217       //--------------------------------------------------------------
008100121217       //?Definizione variabili.                                       ?
008200121217       //--------------------------------------------------------------
008300121217
008400121217       // -?Flags booleani?
008500121217     d $EoF            s               n   inz
008600121217
008700121217       // -?Indici di schiera?
008800121217     d xx              s              5  0 inz
008900121217     d yy              s              5  0 inz
009000121217     d ww              s              5  0 inz
009100140925     d wX              s              5  0 inz
009200140925     d wY              s              5  0 inz
009300121218
009400121218       // -?Nome esteso Libreria/File dei 2 file tabelle?
009500121220     d wLibTABEL       s             21a   inz
009600121218     d wLibTNTBE       s             21a   inz
009700121217
009800121217       // -?Campi di comodo?
009900121218     d w_iSystem       s              1  0 inz
010000121218     d w_SisInf        s              3  0 inz
010100141028     d wDataDa         s              8  0 inz
010200141028     d wDataA          s              8  0 inz
010300141028     d wDay            s              8  0 inz
010400141028     d dateISO         s               d
010500121218     d wErr            s             10i 0 inz
010600141021     d dayNbr          s             11p 0
010700141021     D pMsgV           s           5000    inz varying
010800141028     d wVideo          s               n
010900141030     D wn20            s             20s 0
011000141030     D wn14            s             14s 0
011100141030     D wn14_2          s             14s 0
011200121218
011300141027       // -?Dati di  dialogo
011400141217     D UBMACTLDS       DS
011500141217     D  NrGg                          3s 0
011600141218     D  IndEMail                    126a
011700141027
011800121217       // -?Stringa SQL da eseguire?
011900121217     d wSQL            s           2048    inz  varying
012000141217     d wSQL_dati       ds                  inz  qualified
012100141217     d   wSQL_Now                    26a   inz
012200141218     d   wSQL_NrEMail                 9s 0 inz
012300121217
012400121217       //--------------------------------------------------------------
012500121217       //?Definizione prototipi procedure.                             ?
012600121217       //--------------------------------------------------------------
012700121218
012800121218       // -?Reperimento NETA sistema AS/400 corrente?
012900121218     d currSysNeta     s              8a   inz
013000121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
013100121218
013200121218       // -?Reperimento dati utente?
013300121218     d TIBS34ds      e ds
013400121218      /copy gaitrasrc/srcProtoPR,TIBS34R
013500121217
013600121217       // -?Parametri API QCAPCMD (Process Commands)?
013700121217     d Qcmd            s           2048    inz  varying
013800121217
013900121217       //--------------------------------------------------------------
014000121217       //?Definizione key-list.                                        ?
014100121217       //--------------------------------------------------------------
014200121217
014300121217
014400121217       //--------------------------------------------------------------
014500121217       //?Definizione parametri procedura.                             ?
014600121217       //--------------------------------------------------------------
014700121217
014800121217     c     *Entry        plist
014900121217     c                   parm                    KPJBA
015000121217
015100121217      /free
015200121217
015300121217       //--------------------------------------------------------------
015400121217       //?M A I N - L I N E                                            ?
015500121217       //--------------------------------------------------------------
015600121217
015700121217       // -?Operazioni iniziali?
015800121217       exsr sr_RoutInz;
015900121217
016000121217       // -?Ciclo di reperimento clienti in tab. "3C"?
016100121218       doW  Not $EoF;
016200121217         exsr  sr_ReadCursor;
016300121218       enddo;
016400140925
016500140925       // -?viene mandata una e-mail a chi è preposto?
016600141028       //  ?se non si deve passare il risultato al chiamante
016700141028       if wVideo = *off;
016800141028         exsr  sr_SendEmail;
016900141028       endif;
017000121217
017100121217       // -?Operazioni finali?
017200121217       exsr sr_RoutEnd;
017300121217
017400121217       //--------------------------------------------------------------
017500121217       //?Operazioni iniziali.                                         ?
017600121217       //--------------------------------------------------------------
017700121217       BEGSR  sr_RoutInz;
017800141217
017900141217         UBMACTLDS = kpjbu;
018000160420         // se NON è stato passato il nr. giorni (lavoro schedulato) imposto come default 3
018100141218         if NrGg = 0;
018200160420           NrGg = 3;
018300141218         endif;
018400121217
018500121217         exec sql  set option  dynusrprf = *owner,
018600121217                               closqlcsr = *endmod;
018700121218
018800121218         // -?Verifica del sistema AS/400 corrente?
018900121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019000121218           $EoF = *on;
019100121218           leavesr;
019200121218         endif;
019300121219
019400121219         // -?Reperimento dati job?
019500121219         exsr  sr_DatiJob;
019600140925
019700141028         // il risultato va inviato via email
019800141217         wVideo = *off;
019900141217         // se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
020000141217         if IndEMail <> *blank;
020100141217           peml = %trim(IndEMail);
020200141217         else;
020300170920           // a seconda del sistema AS/400 corrente imposto il ricevente
020400170920           if %subst (currSysNeta:1:6) = 'SETRAS';
020500170920             peml = 'cedalert@brt.it';
020600170920             pcceml = *blanks;
020700170920           else;
020800170920             peml = 'cedvas@brt.it';
020900170920             pcceml = 'luciano.carini@brt.it';
021000170920           endif;
021100141217         endif;
021200140925
021300141217         pogg = 'BARTMAIL - MISSING STATUS';
021400141217         pMsgV = *blank;
021500121217
021600121217         // -?Preparazione stringa SQL?
021700121217         exsr  sr_PrepSQL;
021800121217
021900121217         // -?Apertura cursore SQL?
022000121217         exsr  sr_OpenCursor;
022100140925
022200140925         clear  wX;
022300140925         clear  wY;
022400130208
022500140925       ENDSR;
022600121219
022700121219       //--------------------------------------------------------------
022800121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
022900121219       //--------------------------------------------------------------
023000121219       BEGSR  sr_DatiJob;
023100141028
023200121219         in(E) §AzUte;
023300121219         if NOT %error;
023400121219           in(E) §DatiUte;
023500121219         endif;
023600121219         if %error or RSut = *blanks;
023700121219           clear TIBS34ds;
023800121219           tibs34r ( tibs34ds );
023900121219           in §AzUte;
024000121219           in §DatiUte;
024100121219         endif;
024200140925
024300121219       ENDSR;
024400141028
024500141028       //--------------------------------------------------------------
024600141028       //?Preparazione stringa SQL                                     ?
024700141028       //--------------------------------------------------------------
024800141028       BEGSR  sr_PrepSQL;
024900141028
025000141217         wSQL = 'select char(now()) , count(*) +
025100141217                 from timal00f +
025200141217                 exception join +
025300141217                 timat00f +
025400150417                 on matprog = malprog and mattrc=''BM1'' and +
025500150422                    char(mataas)  = substr(maldaor , 1 , 4) concat +
025600150422                                    substr(maldaor , 6 , 2) +
025700141217                 where +
025800150216                 substr(MALDUEX, 31, 121) <> '' '' and MALATT = MALTOTATT +
025900161219                 and MALSTS not in (''2'', ''0'') +
026000150216                 and timestampdiff( 16, char(now() - timestamp(maldaor))) > ';
026100141217         wSQL =  wSQL + %char(NrGg);
026200141020
026300141020       ENDSR;
026400121217
026500121217       //--------------------------------------------------------------
026600121217       //?Apertura cursore.                                            ?
026700121217       //--------------------------------------------------------------
026800121217       BEGSR  sr_OpenCursor;
026900121217
027000121217         // -?Dichiarazione cursore?
027100121217         exec sql   prepare S1   from :wSQL;
027200141029         exec sql   declare C1  asensitive   cursor for S1;
027300121217
027400121217         // -?Apertura del cursore?
027500121217         exec sql   open C1;
027600121217
027700121217         if  SQLcode < *zero;
027800121217           $EoF = *on;
027900121217           wErr = 1;
028000121217           exsr  sr_SendEmail;
028100121217         endif;
028200121217
028300121217       ENDSR;
028400121217
028500121217       //--------------------------------------------------------------
028600121217       //?Lettura cursore.                                             ?
028700121217       //--------------------------------------------------------------
028800121217       BEGSR  sr_ReadCursor;
028900121217
029000121217         clear  wSQL_dati;
029100121217
029200121217         exec sql   fetch next   from C1   into :wSQL_dati;
029300141008
029400141008         //le righe successive servono per facilitare il debug
029500141008         //wY += 1;
029600141008         //if %rem(wY:1000) = 0;
029700141008         //  debug1000 = *on;
029800141008         //endif;
029900141008         //debug1000 = *off;
030000121217
030100141021         // il risultato non è un cursore ma un solo dato
030200141021
030300121217         select;
030400121217
030500121217           when  SQLcode < *zero;
030600121217             $EoF = *on;
030700121217             wErr = 2;
030800121217             exsr  sr_SendEmail;
030900121217
031000121217           when  SQLcode = 100;
031100121217             $EoF = *on;
031200121217
031300121217           other;
031400141028             // se mando il risultato via email
031500141028             if wVideo = *off;
031600141028               // preparo il msg
031700141217               pMsgV =
031800141028               ':/N' +
031900141217               'Ci sono ' +
032000141218               %editc(wSQL_dati.wSQL_NrEMail : '1') +
032100141217               ' e-mail spedite che non hanno un corrispettivo stato dopo ' +
032200141217               %editc(NrGg : '1') +
032300141217               ' giorni, rispetto alla data/ora di estrazione: ' +
032400141218               %subst(%trim(wSQL_dati.wSQL_Now) : 1 : 16) +
032500141217               ':/N' +
032600141217               ':/N' +
032700141217               ':/N' +
032800141217               'L''SQL con cui si sono estratti questi dati è:' +
032900141217               ':/N' +
033000141217               wSQL ;
033100141021
033200141028               // lo aggiungo nella email
033300141028               exsr AggEmail;
033400141028
033500141217             // se mando il risultato al chiamante (CASO NON CONTEMPLATO)
033600141028             else;
033700141028             endif;
033800141021
033900141021             // dico che è finito il ciclo perché ho già ricevuto tutti i dati
034000141021             $EoF = *on;
034100121217
034200121217         endsl;
034300121217
034400121217       ENDSR;
034500121217
034600121217       //--------------------------------------------------------------
034700121217       //?Chiusura cursore.                                            ?
034800121217       //--------------------------------------------------------------
034900121217       BEGSR  sr_CloseCursor;
035000121217
035100121217         // -?Chiusura del cursore?
035200121217         exec sql   close C1;
035300121217
035400121217       ENDSR;
035500121217
035600121217       //--------------------------------------------------------------
035700121217       //?Invio e-mail di avviso errore.                               ?
035800121217       //--------------------------------------------------------------
035900121217       BEGSR  sr_SendEmail;
036000140926
036100141021         pmsg = pMsgV;
036200141021         // se sono qui, qualcosa ho scritto
036300141021         exsr EmailInvia;
036400121217
036500121217       ENDSR;
036600121217
036700121217       //--------------------------------------------------------------
036800121217       //?Operazioni finali.                                           ?
036900121217       //--------------------------------------------------------------
037000121217       BEGSR  sr_RoutEnd;
037100121217
037200121217         // -?Chiusura cursore SQL?
037300121217         exsr  sr_CloseCursor;
037400141028
037500141028         // -?valorizza i parametri di output?
037600141217         kpjbu = UBMACTLDS;
037700121217
037800121217         // -?Chiusura pgm?
037900141029         *inlr = *on;
038000121217         return;
038100121217
038200121217       ENDSR;
038300121217
038400121217      /end-free
038500121218
038600140925     C*------------------------------------------------------------------------*
038700140925     C* AggEmail: aggiunge rcd all'e-mail
038800140925     C*------------------------------------------------------------------------*
038900141217     C     AggEmail      BEGSR
039000140925     C*
039100141021     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
039200140925     C*
039300140925     C                   ENDSR
039400140925     C*------------------------------------------------------------------------*
039500140925     C* EmailInvia - Invio e-mail di conferma forzatura
039600140925     C*------------------------------------------------------------------------*
039700140925     C     EmailInvia    BEGSR
039800140925     C*
039900140925     C* invio e-mail
040000140925     C                   call      'TIS701C1'
040100140925     C                   parm                    peml            253
040200140925     C                   parm                    pcceml          253
040300140925     C                   parm                    pogg             44
040400140925     C                   parm                    pmsg           5000
040500140925     C                   parm                    pesito            1
040600140925     C*
040700140925     C                   ENDSR
040800121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
040900121218SETRAS  GAITRAGRU FILTRAGRU
041000121218AS888   GAITRAGRPSFILTRAGRPF
