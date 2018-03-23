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
011400141027     D UBSMSSTSDS      DS
011500141028     D  IndEMail                     50a
011600141027     D  DataDa                        8s 0
011700141027     D  DataA                         8s 0
011800141028     D  GgDa                          9
011900141028     D  GgA                           9
012000141027     D  NoInv                         7s 0
012100141027     D  Inv                           7s 0
012200141027     D  InvNP                         7s 0
012300141027     D  InvUnk                        7s 0
012400141027     D  InvFail                       7s 0
012500141027     D  InvNoEx                       7s 0
012600141027     D  InvOK                         7s 0
012700141027     D  InvMore                       7s 0
012800141028     D  InvIeri                       7s 0
012900141029     D  MedIeri                       6s 1
013000141028     D  InvOggi                       7s 0
013100141029     D  MedOggi                       6s 1
013200141030     D  InvOggi2Ore                   7s 0
013300141027
013400121217       // -?Stringa SQL da eseguire?
013500121217     d wSQL            s           2048    inz  varying
013600141021     d wSQL_dati       ds                  inz  qualified
013700141021     d   wSQL_NoInv                   7s 0 inz
013800141021     d   wSQL_Inv                     7s 0 inz
013900141121     d   wSQL_InvOK                   7s 0 inz
014000141121     d   wSQL_InvFail                 7s 0 inz
014100141121     d   wSQL_InvNoEx                 7s 0 inz
014200141121     d   wSQL_InvUnk                  7s 0 inz
014300141021     d   wSQL_InvNP                   7s 0 inz
014400141021     d   wSQL_InvMore                 7s 0 inz
014500141028     d   wSQL_InvOggi                 7s 0 inz
014600141028     d   wSQL_InvIeri                 7s 0 inz
014700141030     d   wSQL_Inv2Ore                 7s 0 inz
014800121217
014900121217       //--------------------------------------------------------------
015000121217       //?Definizione prototipi procedure.                             ?
015100121217       //--------------------------------------------------------------
015200121218
015300121218       // -?Reperimento NETA sistema AS/400 corrente?
015400121218     d currSysNeta     s              8a   inz
015500121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
015600121218
015700121218       // -?Reperimento dati utente?
015800121218     d TIBS34ds      e ds
015900121218      /copy gaitrasrc/srcProtoPR,TIBS34R
016000121217
016100121217       // -?Parametri API QCAPCMD (Process Commands)?
016200121217     d Qcmd            s           2048    inz  varying
016300121217
016400121217       //--------------------------------------------------------------
016500121217       //?Definizione key-list.                                        ?
016600121217       //--------------------------------------------------------------
016700121217
016800121217
016900121217       //--------------------------------------------------------------
017000121217       //?Definizione parametri procedura.                             ?
017100121217       //--------------------------------------------------------------
017200121217
017300121217     c     *Entry        plist
017400121217     c                   parm                    KPJBA
017500121217
017600121217      /free
017700121217
017800121217       //--------------------------------------------------------------
017900121217       //?M A I N - L I N E                                            ?
018000121217       //--------------------------------------------------------------
018100121217
018200121217       // -?Operazioni iniziali?
018300121217       exsr sr_RoutInz;
018400121217
018500121217       // -?Ciclo di reperimento clienti in tab. "3C"?
018600121218       doW  Not $EoF;
018700121217         exsr  sr_ReadCursor;
018800121218       enddo;
018900140925
019000140925       // -?viene mandata una e-mail a chi è preposto?
019100141028       //  ?se non si deve passare il risultato al chiamante
019200141028       if wVideo = *off;
019300141028         exsr  sr_SendEmail;
019400141028       endif;
019500121217
019600121217       // -?Operazioni finali?
019700121217       exsr sr_RoutEnd;
019800121217
019900121217       //--------------------------------------------------------------
020000121217       //?Operazioni iniziali.                                         ?
020100121217       //--------------------------------------------------------------
020200121217       BEGSR  sr_RoutInz;
020300121217
020400121217         exec sql  set option  dynusrprf = *owner,
020500121217                               closqlcsr = *endmod;
020600141030
020700141030         // data ora inizio elaborazione
020800141030         wn20 = %dec(%timestamp());
020900141030         wn14 = %dec(%subst(%editc(wn20:'X'):1:14):14:0);
021000141030         // data ora inizio elaborazione - 2 ore
021100141030         wn20 = %dec(%timestamp(wn20) - %hours(2));
021200141030         wn14_2 = %dec(%subst(%editc(wn20:'X'):1:14):14:0);
021300121218
021400121218         // -?Verifica del sistema AS/400 corrente?
021500121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
021600121218           $EoF = *on;
021700121218           leavesr;
021800121218         endif;
021900121219
022000121219         // -?Reperimento dati job?
022100121219         exsr  sr_DatiJob;
022200140925
022300141027         // se è stata passata la data A allora non va spedita l'email ma il risultato va a video
022400141027         if %subst(kpjbu : 59 : 8) <> *blank;
022500141028           wVideo = *on;
022600141027           UBSMSSTSDS = Kpjbu;
022700141028           // preparo le date da e a rispetto ai valori passati
022800141028           exsr sr_PrepDate;
022900141027
023000141028         // il risultato va inviato via email
023100141027         else;
023200141028           wVideo = *off;
023300141028           // preparo le date da e a rispetto alla data odierna
023400141028           exsr sr_PrepDate;
023500141027
023600141027           // se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
023700141027           if %subst( kpjbu : 1 : 50 ) <> *blank;
023800141027             peml = %trim(%subst( kpjbu : 1 : 50 ));
023900140925
024000141027           else;
024100140925
024200141027             // a seconda del sistema AS/400 corrente imposto il ricevente
024300141027             if %subst (currSysNeta:1:6) = 'SETRAS';
024400141027               peml = 'cedalert@brt.it';
024500141027               pcceml = *blanks;
024600141027             else;
024700141027               peml = 'cedvas@brt.it';
024800141027               pcceml = 'luciano.carini@brt.it';
024900141027             endif;
025000141027           endif;
025100140925
025200141027           pogg = 'BARTVAS - STATO INVII SMS';
025300141027           pMsgV =
025400141027            'Lista di controllo degli invii SMS da Lunedì ' +
025500141028            %subst(%editc(wDataDa : 'X'):7:2) + '/' +
025600141028            %subst(%editc(wDataDa : 'X'):5:2) + '/' +
025700141028            %subst(%editc(wDataDa : 'X'):1:4) +
025800141027            ' a ';
025900141027           select;
026000141027             when dayNbr = 0;
026100141027               pMsgV = pMsgV + 'Lunedì ';
026200141027             when dayNbr = 1;
026300141027               pMsgV = pMsgV + 'Martedì ';
026400141027             when dayNbr = 2;
026500141027               pMsgV = pMsgV + 'Mercoledì ';
026600141027             when dayNbr = 3;
026700141027               pMsgV = pMsgV + 'Giovedì ';
026800141027             when dayNbr = 4;
026900141027               pMsgV = pMsgV + 'Venerdì ';
027000141027             when dayNbr = 5;
027100141027               pMsgV = pMsgV + 'Sabato ';
027200141027             when dayNbr = 6;
027300141027               pMsgV = pMsgV + 'Domenica ';
027400141027           endsl;
027500141027           pMsgV = pMsgV +
027600141028            %subst(%editc(wDataA:'X'):7:2) + '/' +
027700141028            %subst(%editc(wDataA:'X'):5:2) + '/' +
027800141028            %subst(%editc(wDataA:'X'):1:4) +
027900141027            '.' +
028000141027            ' :/N';
028100141027         endif;
028200121217
028300121217         // -?Preparazione stringa SQL?
028400121217         exsr  sr_PrepSQL;
028500121217
028600121217         // -?Apertura cursore SQL?
028700121217         exsr  sr_OpenCursor;
028800140925
028900140925         clear  wX;
029000140925         clear  wY;
029100130208
029200140925       ENDSR;
029300121219
029400121219       //--------------------------------------------------------------
029500121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
029600121219       //--------------------------------------------------------------
029700121219       BEGSR  sr_DatiJob;
029800141028
029900121219         in(E) §AzUte;
030000121219         if NOT %error;
030100121219           in(E) §DatiUte;
030200121219         endif;
030300121219         if %error or RSut = *blanks;
030400121219           clear TIBS34ds;
030500121219           tibs34r ( tibs34ds );
030600121219           in §AzUte;
030700121219           in §DatiUte;
030800121219         endif;
030900140925
031000121219       ENDSR;
031100121217
031200121217       //--------------------------------------------------------------
031300141028       //?Prepararo date da e a da usare in SQL
031400121217       //--------------------------------------------------------------
031500141028       BEGSR  sr_PrepDate;
031600141028
031700141028         // pgm richiamato per mostrare i risultati a video
031800141028         if wVideo = *on;
031900141028
032000141029           // la data a è obblogataria quindi è stata passata
032100141028           wDataA = DataA;
032200141028           // recupero  il giorno della settimana
032300141028           dateISO = %date(DataA : *iso);
032400141028           // 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun
032500141028           exec sql  set :dayNbr = dayofweek_iso(:dateISO);
032600141028           // a me serve com Mon = 0 per cui sottraggo 1
032700141028           dayNbr = dayNbr - 1;
032800141028           select;
032900141028             when dayNbr = 0;
033000141028               GgA = 'Lunedì ';
033100141028             when dayNbr = 1;
033200141028               GgA = 'Martedì ';
033300141028             when dayNbr = 2;
033400141028               GgA = 'Mercoledì ';
033500141028             when dayNbr = 3;
033600141028               GgA = 'Giovedì ';
033700141028             when dayNbr = 4;
033800141028               GgA = 'Venerdì ';
033900141028             when dayNbr = 5;
034000141028               GgA = 'Sabato ';
034100141028             when dayNbr = 6;
034200141028               GgA = 'Domenica ';
034300141028           endsl;
034400141028
034500141028           // se la data da è stata passata uso quella
034600141028           if DataDa > 0;
034700141028             wDataDa = DataDa;
034800141028
034900141028           // se la data da NON è stata passata calcolo
035000141028           //  il Lunedì della settimana contenente la data a
035100141028           else;
035200141028             // trasformo in ISO la data a
035300141028             dateISO = %date(DataA : *iso);
035400141028             // calcolo che giorno della settimana è la data a
035500141028             // 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun
035600141028             exec sql  set :dayNbr = dayofweek_iso(:dateISO);
035700141028             // a me serve com Mon = 0 per cui sottraggo 1
035800141028             dayNbr = dayNbr - 1;
035900141028             wDataDa = %dec(%date(wDataA : *iso) - %days(dayNbr) : *iso);
036000141029             DataDa = wDataDa;
036100141028           endif;
036200141028
036300141028           // recupero  il giorno della settimana
036400141028           dateISO = %date(DataDa : *iso);
036500141028           // 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun
036600141028           exec sql  set :dayNbr = dayofweek_iso(:dateISO);
036700141028           // a me serve com Mon = 0 per cui sottraggo 1
036800141028           dayNbr = dayNbr - 1;
036900141028           select;
037000141028             when dayNbr = 0;
037100141028               GgDa = 'Lunedì ';
037200141028             when dayNbr = 1;
037300141028               GgDa = 'Martedì ';
037400141028             when dayNbr = 2;
037500141028               GgDa = 'Mercoledì ';
037600141028             when dayNbr = 3;
037700141028               GgDa = 'Giovedì ';
037800141028             when dayNbr = 4;
037900141028               GgDa = 'Venerdì ';
038000141028             when dayNbr = 5;
038100141028               GgDa = 'Sabato ';
038200141028             when dayNbr = 6;
038300141028               GgDa = 'Domenica ';
038400141028           endsl;
038500141028
038600141028         // pgm richiamato per mostrare i risultati su email
038700141028         else;
038800141028           // la data a è oggi
038900141028           wDataA = %dec(%date() : *iso);
039000141028           // la data da è il lunedì della settimana odierna
039100141028           // calcolo che giorno della settimana è oggi
039200141028           // 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun
039300141028           exec sql  set :dayNbr = dayofweek_iso(curdate());
039400141028           // a me serve com Mon = 0 per cui sottraggo 1
039500141028           dayNbr = dayNbr - 1;
039600141028           wDataDa = %dec(%date() - %days(dayNbr) : *iso);
039700141028         endif;
039800141028
039900141028       ENDSR;
040000141028
040100141028       //--------------------------------------------------------------
040200141028       //?Preparazione stringa SQL                                     ?
040300141028       //--------------------------------------------------------------
040400141028       BEGSR  sr_PrepSQL;
040500141028
040600141020         wSQL = 'with +
040700141020                 base as +
040800141021                  (select * from ';
040900141021                 if %subst (currSysNeta:1:6) = 'SETRAS';
041000141021                   wSQL = wSQL + 'filtra201/fiar500f ';
041100141021                 else;
041200141021                   wSQL = wSQL + 'fiar500f ';
041300141021                 endif;
041400141021                 wSQL = wSQL +
041500141021                   'where ar5trd= ''EMD'' +
041600141020                   and substr(ar5uni, 101, 1)= ''S'' ), +
041700141020                // da inviare
041800141020                 daInviare as +
041900141021                  (select * from base +
042000141020                   where substr(ar5uni, 102, 14)= '' '' ), +
042100141020                // inviato
042200141020                 inviato as +
042300141021                  (select * from base +
042400141020                   where substr(ar5uni, 102, 14)<>'' '' +
042500141021                   and dec(substr(ar5uni, 102, 8) , 8 , 0)  between ' +
042600141028                   %editc(wDataDa : 'X') +
042700141021                   ' and ' +
042800141028                   %editc(wDataA : 'X') + ' ), +
042900141020                // inviato: esito SMS non pervenuto
043000141021                 invNP as +
043100141021                  (select * from inviato +
043200141021                   where substr(ar5uni, 116, 1) = '' '' ), +
043300141021                // inviato: esito sconosciuto
043400141021                 invUnk as +
043500141021                  (select * from inviato +
043600141021                   where substr(ar5uni, 116, 1) = ''*'' ), +
043700141021                // inviato: fallito invio
043800141021                 invFailed as +
043900141021                  (select * from inviato +
044000141021                   where substr(ar5uni, 116, 1) = ''K'' ), +
044100141021                // inviato: nr. cellulare inesistente
044200141021                 invNoExist as +
044300141021                  (select * from inviato +
044400141021                   where substr(ar5uni, 116, 1) = ''N'' ), +
044500141021                // inviato: OK
044600141021                 invOK as +
044700141021                  (select * from inviato +
044800141021                   where substr(ar5uni, 116, 1) = ''O'' ), +
044900141021                // inviato: unexpected
045000141021                 invAltri as +
045100141021                  (select * from inviato +
045200141021                   where substr(ar5uni, 116, 1) not in  ('' '', +
045300141028                    ''*'',''K'',''N'',''O'')), +
045400141028                // inviati oggi
045500141028                 invoggi as +
045600141028                  (select * from base +
045700141028                   where substr(ar5uni, 102, 14)<>'' '' +
045800141028                   and dec(substr(ar5uni, 102, 8) , 8 , 0)  = ' +
045900141028                   %editc(%dec(%date() : *iso) : 'X') +
046000141028                   '), +
046100141030                // inviati ultime 2 ore oggi
046200141030                 invoggi2h as +
046300141030                  (SELECT * FROM tivin00s WHERE vinkey in ( +
046400141030                   SELECT VLRHDL FROM tivlr00f WHERE VLRKSC=''0BART001'' +
046500141030                   and VLRTIP=''SR'' +
046600141030                   and VLRDAT*1000000+VLRORA +
046700141030                   between ' + %editc(wn14_2:'X') + ' and ' +
046800141030                   %editc(wn14:'X') +
046900141030                   ') ), +
047000141028                // inviati ieri
047100141028                 invieri as +
047200141028                  (select * from base +
047300141028                   where substr(ar5uni, 102, 14)<>'' '' +
047400141028                   and dec(substr(ar5uni, 102, 8) , 8 , 0)  = ' +
047500141028                   %editc(%dec(%date() - %days(1) : *iso) : 'X') +
047600141028                   ') +
047700141021                // emetto totali
047800141022                select +
047900141022                (select count(*) from daInviare),  +
048000141022                (select count(*) from inviato), +
048100141029                (select count(*) from invOK), +
048200141029                (select count(*) from invFailed), +
048300141029                (select count(*) from invNoExist), +
048400141029                (select count(*) from invUnk), +
048500141022                (select count(*) from invNP), +
048600141028                (select count(*) from invAltri), +
048700141028                (select count(*) from invOggi), +
048800141030                (select count(*) from invIeri), +
048900141030                (select count(*) from invOggi2h) +
049000141022                // la sysibm/sysdummy1 è una tabella di sistema mono rcd e mono colonna
049100141022                // usata dall'IBM proprio per questi giochetti
049200141022                from sysibm/sysdummy1 +
049300141022                ';
049400141020
049500141020       ENDSR;
049600121217
049700121217       //--------------------------------------------------------------
049800121217       //?Apertura cursore.                                            ?
049900121217       //--------------------------------------------------------------
050000121217       BEGSR  sr_OpenCursor;
050100121217
050200121217         // -?Dichiarazione cursore?
050300121217         exec sql   prepare S1   from :wSQL;
050400141029         exec sql   declare C1  asensitive   cursor for S1;
050500121217
050600121217         // -?Apertura del cursore?
050700121217         exec sql   open C1;
050800121217
050900121217         if  SQLcode < *zero;
051000121217           $EoF = *on;
051100121217           wErr = 1;
051200121217           exsr  sr_SendEmail;
051300121217         endif;
051400121217
051500121217       ENDSR;
051600121217
051700121217       //--------------------------------------------------------------
051800121217       //?Lettura cursore.                                             ?
051900121217       //--------------------------------------------------------------
052000121217       BEGSR  sr_ReadCursor;
052100121217
052200121217         clear  wSQL_dati;
052300121217
052400121217         exec sql   fetch next   from C1   into :wSQL_dati;
052500141008
052600141008         //le righe successive servono per facilitare il debug
052700141008         //wY += 1;
052800141008         //if %rem(wY:1000) = 0;
052900141008         //  debug1000 = *on;
053000141008         //endif;
053100141008         //debug1000 = *off;
053200121217
053300141021         // il risultato non è un cursore ma un solo dato
053400141021
053500121217         select;
053600121217
053700121217           when  SQLcode < *zero;
053800121217             $EoF = *on;
053900121217             wErr = 2;
054000121217             exsr  sr_SendEmail;
054100121217
054200121217           when  SQLcode = 100;
054300121217             $EoF = *on;
054400121217
054500121217           other;
054600141028             // se mando il risultato via email
054700141028             if wVideo = *off;
054800141028               // preparo il msg
054900141028               pMsgV = pMsgV +
055000141028               ':/N' +
055100141028               'SMS ancora da inviare: ' +
055200141028               %editc(wSQL_dati.wSQL_NoInv : '1') +
055300141028               ':/N' +
055400141028               'SMS inviati: ' +
055500141028               %editc(wSQL_dati.wSQL_Inv : '1') +
055600141028               '  di cui:' +
055700141028               ':/N' +
055800141029               '  invio riuscito ..........: ' +
055900141029               %editc(wSQL_dati.wSQL_InvOK : '1') +
056000141029               ':/N' +
056100141029               '  invio SMS fallito .......: ' +
056200141029               %editc(wSQL_dati.wSQL_InvFail : '1') +
056300141029               ':/N' +
056400141029               '  nr. cellulare inesistente: ' +
056500141029               %editc(wSQL_dati.wSQL_InvNoEx : '1') +
056600141029               ':/N' +
056700141029               '  esito SMS sconosciuto ...: ' +
056800141029               %editc(wSQL_dati.wSQL_InvUnk : '1') +
056900141029               ':/N' +
057000141028               '  esito SMS non pervenuto .: ' +
057100141028               %editc(wSQL_dati.wSQL_InvNP : '1') +
057200141028               ':/N' +
057300141028               '  unexpected ..............: ' +
057400141028               %editc(wSQL_dati.wSQL_InvMore : '1') +
057500141030               ':/N' +
057600141028               'SMS inviati ieri ..........: ' +
057700141117               %editc(wSQL_dati.wSQL_InvIeri : '1');
057800141117               // se oogi ho spedito SMS (altrimenti si dividerebbe per 0)
057900141117               if wSQL_dati.wSQL_Inv > 0;
058000141117                 // se non è stato lanciato di lunedì (perché la differenza tra date darebbe 0)
058100141117                 if wDataA > wDataDa;
058200141117                   pMsgV = pMsgV +
058300141117                   %editc(%dec(wSQL_dati.wSQL_InvIeri * 100 / (
058400141029                            wSQL_dati.wSQL_Inv /
058500141029                            %diff(%date(wDataA  : *iso) :
058600141029                            %date(wDataDa : *iso) :
058700141029                            *days)) : 6 : 1)
058800141117                            : '1') + ' %';
058900141117                 else;
059000141117                   pMsgV = pMsgV +
059100141117                   ' 100 %';
059200141117                 endif;
059300141117               endif;
059400141117               pMsgV = pMsgV +
059500141028               ':/N' +
059600141028               'SMS inviati oggi ..........: ' +
059700141117               %editc(wSQL_dati.wSQL_InvOggi : '1');
059800141117               // se oogi ho spedito SMS (altrimenti si dividerebbe per 0)
059900141117               if wSQL_dati.wSQL_Inv > 0;
060000141117                 // se non è stato lanciato di lunedì (perché la differenza tra date darebbe 0)
060100141117                 if wDataA > wDataDa;
060200141117                   pMsgV = pMsgV +
060300141117                   %editc(%dec(wSQL_dati.wSQL_InvOggi * 100 / (
060400141029                            wSQL_dati.wSQL_Inv /
060500141029                            %diff(%date(wDataA  : *iso) :
060600141029                            %date(wDataDa : *iso) :
060700141029                            *days)) : 6 : 1)
060800141117                            : '1') + ' %';
060900141117                 else;
061000141117                   pMsgV = pMsgV +
061100141117                   ' 100 %';
061200141117                 endif;
061300141117               endif;
061400141117               pMsgV = pMsgV +
061500141030               ':/N' +
061600141030               'Esiti SMS ultime 2 ore oggi: ' +
061700141030               %editc(wSQL_dati.wSQL_Inv2Ore : '1') ;
061800141021
061900141028               // lo aggiungo nella email
062000141028               exsr AggEmail;
062100141028
062200141028             // se mando il risultato al chiamante
062300141028             else;
062400141028               DataDa       = wDataDa;
062500141028               DataA        = wDataA;
062600141028               NoInv        = wSQL_dati.wSQL_NoInv        ;
062700141028               Inv          = wSQL_dati.wSQL_Inv          ;
062800141028               InvNP        = wSQL_dati.wSQL_InvNP        ;
062900141028               InvUnk       = wSQL_dati.wSQL_InvUnk       ;
063000141028               InvFail      = wSQL_dati.wSQL_InvFail      ;
063100141028               InvNoEx      = wSQL_dati.wSQL_InvNoEx      ;
063200141028               InvOK        = wSQL_dati.wSQL_InvOK        ;
063300141028               InvMore      = wSQL_dati.wSQL_InvMore      ;
063400141028               InvOggi      = wSQL_dati.wSQL_invOggi ;
063500141029               // per calcolare la media percentuale l'espressione è:
063600141029               // inviati oggi : media inviati giornalieri = X : 100
063700141029               // quindi
063800141029               // media inviati giornalieri = inviati totali / gg. tra le due date da/a
063900141030               MedOggi = 0;
064000141030               if Inv > 0;
064100141117                 // se non è stato lanciato di lunedì (perché la differenza tra date darebbe 0)
064200141117                 if wDataA > wDataDa;
064300141117                   MedOggi = InvOggi * 100 / (Inv /
064400141029                            %diff(%date(DataA  : *iso) :
064500141029                            %date(DataDa : *iso) :
064600141029                            *days));
064700141117                 else;
064800141117                   MedOggi = 100;
064900141117                 endif;
065000141117               endif;
065100141028               InvIeri      = wSQL_dati.wSQL_invIeri ;
065200141030               MedIeri = 0;
065300141030               if Inv > 0;
065400141117                 // se non è stato lanciato di lunedì (perché la differenza tra date darebbe 0)
065500141117                 if wDataA > wDataDa;
065600141117                   MedIeri = InvIeri * 100 / (Inv /
065700141029                            %diff(%date(DataA  : *iso) :
065800141029                            %date(DataDa : *iso) :
065900141029                            *days));
066000141117                 else;
066100141117                   MedIeri = 100;
066200141117                 endif;
066300141030               endif;
066400141030               InvOggi2Ore  = wSQL_dati.wSQL_inv2Ore ;
066500141028             endif;
066600141021
066700141021             // dico che è finito il ciclo perché ho già ricevuto tutti i dati
066800141021             $EoF = *on;
066900121217
067000121217         endsl;
067100121217
067200121217       ENDSR;
067300121217
067400121217       //--------------------------------------------------------------
067500121217       //?Chiusura cursore.                                            ?
067600121217       //--------------------------------------------------------------
067700121217       BEGSR  sr_CloseCursor;
067800121217
067900121217         // -?Chiusura del cursore?
068000121217         exec sql   close C1;
068100121217
068200121217       ENDSR;
068300121217
068400121217       //--------------------------------------------------------------
068500121217       //?Invio e-mail di avviso errore.                               ?
068600121217       //--------------------------------------------------------------
068700121217       BEGSR  sr_SendEmail;
068800140926
068900141021         pmsg = pMsgV;
069000141021         // se sono qui, qualcosa ho scritto
069100141021         exsr EmailInvia;
069200121217
069300121217       ENDSR;
069400121217
069500121217       //--------------------------------------------------------------
069600121217       //?Operazioni finali.                                           ?
069700121217       //--------------------------------------------------------------
069800121217       BEGSR  sr_RoutEnd;
069900121217
070000121217         // -?Chiusura cursore SQL?
070100121217         exsr  sr_CloseCursor;
070200141028
070300141028         // -?valorizza i parametri di output?
070400141028         kpjbu = UBSMSSTSDS;
070500121217
070600121217         // -?Chiusura pgm?
070700141029         *inlr = *on;
070800121217         return;
070900121217
071000121217       ENDSR;
071100121217
071200121217      /end-free
071300121218
071400140925     C*------------------------------------------------------------------------*
071500140925     C* AggEmail: aggiunge rcd all'e-mail
071600140925     C*------------------------------------------------------------------------*
071700140925     C     AggEmail      BEGSR
071800140925     C*
071900141021     C* questa routine non fa niente perché pMsgV è già stato valorizzato prima
072000140925     C*
072100140925     C                   ENDSR
072200140925     C*------------------------------------------------------------------------*
072300140925     C* EmailInvia - Invio e-mail di conferma forzatura
072400140925     C*------------------------------------------------------------------------*
072500140925     C     EmailInvia    BEGSR
072600140925     C*
072700140925     C* invio e-mail
072800140925     C                   call      'TIS701C1'
072900140925     C                   parm                    peml            253
073000140925     C                   parm                    pcceml          253
073100140925     C                   parm                    pogg             44
073200140925     C                   parm                    pmsg           5000
073300140925     C                   parm                    pesito            1
073400140925     C*
073500140925     C                   ENDSR
073600121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
073700121218SETRAS  GAITRAGRU FILTRAGRU
073800121218AS888   GAITRAGRPSFILTRAGRPF
