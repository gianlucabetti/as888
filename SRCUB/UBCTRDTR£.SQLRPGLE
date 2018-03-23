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
018000141218         // se NON è stato passato il nr. giorni (lavoro schedulato) imposto come default 2
018100141218         if NrGg = 0;
018200141218           NrGg = 2;
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
020300141217           // il default destinatario è sempre quello qualunque sistema sia9
020400141217           peml = 'fabrizio.gurrieri@brt.it';
020500141217           pcceml = 'luciano.carini@brt.it';
020600141217         endif;
020700140925
020800141217         pogg = 'BARTMAIL - MISSING STATUS';
020900141217         pMsgV = *blank;
021000121217
021100121217         // -?Preparazione stringa SQL?
021200121217         exsr  sr_PrepSQL;
021300121217
021400121217         // -?Apertura cursore SQL?
021500121217         exsr  sr_OpenCursor;
021600140925
021700140925         clear  wX;
021800140925         clear  wY;
021900130208
022000140925       ENDSR;
022100121219
022200121219       //--------------------------------------------------------------
022300121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
022400121219       //--------------------------------------------------------------
022500121219       BEGSR  sr_DatiJob;
022600141028
022700121219         in(E) §AzUte;
022800121219         if NOT %error;
022900121219           in(E) §DatiUte;
023000121219         endif;
023100121219         if %error or RSut = *blanks;
023200121219           clear TIBS34ds;
023300121219           tibs34r ( tibs34ds );
023400121219           in §AzUte;
023500121219           in §DatiUte;
023600121219         endif;
023700140925
023800121219       ENDSR;
023900141028
024000141028       //--------------------------------------------------------------
024100141028       //?Preparazione stringa SQL                                     ?
024200141028       //--------------------------------------------------------------
024300141028       BEGSR  sr_PrepSQL;
024400141028
024500141217         wSQL = 'select char(now()) , count(*) +
024600141217                 from timal00f +
024700141217                 exception join +
024800141217                 timat00f +
024900150417                 on matprog = malprog and mattrc=''BM1'' and +
025000150422                    char(mataas)  = substr(maldaor , 1 , 4) concat +
025100150422                                    substr(maldaor , 6 , 2) +
025200141217                 where +
025300150216                 substr(MALDUEX, 31, 121) <> '' '' and MALATT = MALTOTATT +
025400150216                 and timestampdiff( 16, char(now() - timestamp(maldaor))) > ';
025500141217         wSQL =  wSQL + %char(NrGg);
025600141020
025700141020       ENDSR;
025800121217
025900121217       //--------------------------------------------------------------
026000121217       //?Apertura cursore.                                            ?
026100121217       //--------------------------------------------------------------
026200121217       BEGSR  sr_OpenCursor;
026300121217
026400121217         // -?Dichiarazione cursore?
026500121217         exec sql   prepare S1   from :wSQL;
026600141029         exec sql   declare C1  asensitive   cursor for S1;
026700121217
026800121217         // -?Apertura del cursore?
026900121217         exec sql   open C1;
027000121217
027100121217         if  SQLcode < *zero;
027200121217           $EoF = *on;
027300121217           wErr = 1;
027400121217           exsr  sr_SendEmail;
027500121217         endif;
027600121217
027700121217       ENDSR;
027800121217
027900121217       //--------------------------------------------------------------
028000121217       //?Lettura cursore.                                             ?
028100121217       //--------------------------------------------------------------
028200121217       BEGSR  sr_ReadCursor;
028300121217
028400121217         clear  wSQL_dati;
028500121217
028600121217         exec sql   fetch next   from C1   into :wSQL_dati;
028700141008
028800141008         //le righe successive servono per facilitare il debug
028900141008         //wY += 1;
029000141008         //if %rem(wY:1000) = 0;
029100141008         //  debug1000 = *on;
029200141008         //endif;
029300141008         //debug1000 = *off;
029400121217
029500141021         // il risultato non è un cursore ma un solo dato
029600141021
029700121217         select;
029800121217
029900121217           when  SQLcode < *zero;
030000121217             $EoF = *on;
030100121217             wErr = 2;
030200121217             exsr  sr_SendEmail;
030300121217
030400121217           when  SQLcode = 100;
030500121217             $EoF = *on;
030600121217
030700121217           other;
030800141028             // se mando il risultato via email
030900141028             if wVideo = *off;
031000141028               // preparo il msg
031100141217               pMsgV =
031200141028               ':/N' +
031300141217               'Ci sono ' +
031400141218               %editc(wSQL_dati.wSQL_NrEMail : '1') +
031500141217               ' e-mail spedite che non hanno un corrispettivo stato dopo ' +
031600141217               %editc(NrGg : '1') +
031700141217               ' giorni, rispetto alla data/ora di estrazione: ' +
031800141218               %subst(%trim(wSQL_dati.wSQL_Now) : 1 : 16) +
031900141217               ':/N' +
032000141217               ':/N' +
032100141217               ':/N' +
032200141217               'L''SQL con cui si sono estratti questi dati è:' +
032300141217               ':/N' +
032400141217               wSQL ;
032500141021
032600141028               // lo aggiungo nella email
032700141028               exsr AggEmail;
032800141028
032900141217             // se mando il risultato al chiamante (CASO NON CONTEMPLATO)
033000141028             else;
033100141028             endif;
033200141021
033300141021             // dico che è finito il ciclo perché ho già ricevuto tutti i dati
033400141021             $EoF = *on;
033500121217
033600121217         endsl;
033700121217
033800121217       ENDSR;
033900121217
034000121217       //--------------------------------------------------------------
034100121217       //?Chiusura cursore.                                            ?
034200121217       //--------------------------------------------------------------
034300121217       BEGSR  sr_CloseCursor;
034400121217
034500121217         // -?Chiusura del cursore?
034600121217         exec sql   close C1;
034700121217
034800121217       ENDSR;
034900121217
035000121217       //--------------------------------------------------------------
035100121217       //?Invio e-mail di avviso errore.                               ?
035200121217       //--------------------------------------------------------------
035300121217       BEGSR  sr_SendEmail;
035400140926
035500141021         pmsg = pMsgV;
035600141021         // se sono qui, qualcosa ho scritto
035700141021         exsr EmailInvia;
035800121217
035900121217       ENDSR;
036000121217
036100121217       //--------------------------------------------------------------
036200121217       //?Operazioni finali.                                           ?
036300121217       //--------------------------------------------------------------
036400121217       BEGSR  sr_RoutEnd;
036500121217
036600121217         // -?Chiusura cursore SQL?
036700121217         exsr  sr_CloseCursor;
036800141028
036900141028         // -?valorizza i parametri di output?
037000141217         kpjbu = UBMACTLDS;
037100121217
037200121217         // -?Chiusura pgm?
037300141029         *inlr = *on;
037400121217         return;
037500121217
037600121217       ENDSR;
037700121217
037800121217      /end-free
037900121218
038000140925     C*------------------------------------------------------------------------*
038100140925     C* AggEmail: aggiunge rcd all'e-mail
038200140925     C*------------------------------------------------------------------------*
038300141217     C     AggEmail      BEGSR
038400140925     C*
038500141021     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
038600140925     C*
038700140925     C                   ENDSR
038800140925     C*------------------------------------------------------------------------*
038900140925     C* EmailInvia - Invio e-mail di conferma forzatura
039000140925     C*------------------------------------------------------------------------*
039100140925     C     EmailInvia    BEGSR
039200140925     C*
039300140925     C* invio e-mail
039400140925     C                   call      'TIS701C1'
039500140925     C                   parm                    peml            253
039600140925     C                   parm                    pcceml          253
039700140925     C                   parm                    pogg             44
039800140925     C                   parm                    pmsg           5000
039900140925     C                   parm                    pesito            1
040000140925     C*
040100140925     C                   ENDSR
040200121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
040300121218SETRAS  GAITRAGRU FILTRAGRU
040400121218AS888   GAITRAGRPSFILTRAGRPF
