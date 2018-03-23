000100100507       //==============================================================
000200101011       //?TNTAC1R * Crea WrkF per controllo attività
000300100507       //==============================================================
000400100507
000500100507       //--------------------------------------------------------------
000600100507       //?Specifiche di controllo.                                     ?
000700100507       //--------------------------------------------------------------
000800100507
000900100510     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000100507
001100100507       //--------------------------------------------------------------
001200100507       //?Dichiarazione file.                                          ?
001300100507       //--------------------------------------------------------------
001400101011
001500101011       // -?Organigramma?
001600101011     fAZORG01L  if   e           k disk
001700100507
001800100507       // -?Tabelle?
001900101011     fTABEL00F  if   e           k disk
002000100507
002100101011       // -?Elenco trattative
002200101011     fWFATT00F  o    e             disk    usropn
002300101011     f                                     extfile(wFLib)
002400101019
002500101019       // -?File video
002600101019     fTNTAC1D   cf   e             workstn
002700100507
002800100507       //--------------------------------------------------------------
002900100507       //?Definizione costanti.                                        ?
003000100507       //--------------------------------------------------------------
003100100507
003200100507       //--------------------------------------------------------------
003300100507       //?Definizione schiere.                                         ?
003400100507       //--------------------------------------------------------------
003500100507
003600100507       //--------------------------------------------------------------
003700100507       //?Definizione aree dati.                                       ?
003800100507       //--------------------------------------------------------------
003900100507
004000100507       // -?Dati utente?
004100100507     d §AzUte        e ds                  extname(AZUTE00F)
004200100507     d                                     dtaara
004300100507     d §DatiUte      e ds                  extname(dDatiUte)
004400100507     d                                     dtaara
004500100507
004600100507       //--------------------------------------------------------------
004700100507       //?Definizione strutture dati.                                  ?
004800100507       //--------------------------------------------------------------
004900100507
005000100507       // -?Status ds?
005100100507     d Status         sds
005200100507     d  SDSpgm           *proc
005300100507     d  SDSusr               254    263
005400101011
005500101011       // -?Parametri ricevuti
005600101011     d KPJBA         e ds
005700100507
005800100507       // -?Parametri per Reperimento dati utente?
005900100507     d TIBS34ds      e ds
006000100507
006100100510       // -?Tabella "01" = Commerciali?
006200100510     d ds01cmm       e ds                  extname(ds01)
006300100510     d                                     inz  prefix(cmm)
006400101019       // -?Tabella "01" = Commerciale Unificante
006500101019     d ds01uni       e ds                  extname(ds01)
006600101019     d                                     inz  prefix(uni)
006700100507
006800101019       // -?Tabella "05" = Codici Area commerciale unificante
006900101019     d ds05uni       e ds                  extname(ds05)
007000101019     d                                     inz  prefix(uni)
007100101019
007200101019       // -?Tabella "17" = Codici Distretto commerciale unificante
007300101019     d ds17uni       e ds                  extname(ds17)
007400101019     d                                     inz  prefix(uni)
007500100510
007600100510       // -?140ª descrizione dell'organigramma?
007700101019     d Og140uni      e ds                  extname(Og140)
007800101019     d                                     inz  prefix(uni)
007900101011
008000101011       // -?File attività
008100101011     d TIATC00F      e ds                  extname(TIATC00F)
008200101011
008300101011       // -?File trattative
008400101011     d TIVIS00F      e ds                  extname(TIVIS00F)
008500101019
008600101019       // -?Controllo validità delle date
008700101019     d Xgiolavds     e ds                  inz
008800100507
008900100507       //--------------------------------------------------------------
009000100507       //?Definizione variabili globali.                               ?
009100100507       //--------------------------------------------------------------
009200101011
009300101011       // -?Flags booleani
009400101019     d $End1           s               n   inz(*off)
009500101019     d $End2           s               n   inz(*off)
009600101019     d $End3           s               n   inz(*off)
009700101130     d $End4           s               n   inz(*off)
009800101019     d $Fine           s               n   inz(*off)
009900101011
010000101011       // -?Nome libreria del work-file WFATT00F?
010100101011     d wFLib           s             21    inz
010200101011
010300101011       // -?Campi per richiamo QCMDEXC
010400101011     d Qcmd            s            128    inz
010500100507
010600101019       // -?Codice filiale del commerciale unificante?
010700101019     d ORGfil_uni      s                   like(ORGfil)  inz
010800101019       // -?Codice distretto commerciale unificante?
010900101019     d ORGfl3_uni      s                   like(ORGfl3)  inz
011000101019       // -?Codice commerciale?
011100100510     d TBLkey_Cmm_01   s                   like(TBLkey)  inz
011200101019       // -?Codice commerciale unificante?
011300101019     d TBLkey_Uni_01   s                   like(TBLkey)  inz
011400101019       // -?Codice area del commerciale unificante?
011500101019     d TBLkey_Uni_05   s                   like(TBLkey)  inz
011600101019       // -?Codice distretto del commerciale unificante
011700101019     d TBLkey_Uni_17   s                   like(TBLkey)  inz
011800101011
011900101011       // -?Stringa SQL da eseguire
012000101011     d wSQL            s           2048    Varying        inz
012100101019
012200101019       // -?Data corrente?
012300101019     d wDate           s              8s 0 inz
012400101130
012500101130       // -?Campo di comodo
012600101130     d OLD_VISnrv      s                   like(VISnrv)
012700100507
012800100507       //--------------------------------------------------------------
012900100507       //?Definizione prototipi procedure e relativi parametri.        ?
013000100507       //--------------------------------------------------------------
013100101019
013200101019       // -?Aggiunge/Toglie gg/mm dalla data
013300101019     d Xgiolav         pr                  extpgm('XGIOLAV')
013400101019     d  xgiolavds                          likeds(xgiolavds)
013500101011
013600101011       // -?Esecuzione comando di sistema
013700101011      /copy gaitrasrc/srcProtoPR,QCMDEXC
013800100507
013900100507       // -?Reperimento dati utente?
014000100507      /copy gaitrasrc/srcProtoPR,TIBS34R
014100100507
014200100507       //--------------------------------------------------------------
014300100507       //?Definizione key-list.                                        ?
014400100507       //--------------------------------------------------------------
014500100507
014600100507       // -?File TABEL00F?
014700100507     d k03tabel00    e ds                  extname(TABEL00F : *key)
014800100507     d                                     inz  prefix(k_)
014900100507
015000100507       //--------------------------------------------------------------
015100100507       //?M A I N - L I N E                                            ?
015200100507       //--------------------------------------------------------------
015300101011
015400101011     c     *Entry        plist
015500101011     c                   parm                    KPJBA
015600100507
015700100507      /free
015800100507
015900100507       // -?Operazioni iniziali?
016000100507       exsr sr_RoutInz;
016100101019
016200101019       // -?Emetto il video?
016300101019       DOW $Fine = *off;
016400101019         exfmt  TAC1W01;
016500101019
016600101019         SELECT;
016700101019
016800101019           WHEN  *inkc;
016900101019             $fine = *on;
017000101019
017100101019           WHEN  *inkf;
017200101019             exsr Elabora;
017300101025
017400101025           WHEN  *inkh;
017500101025             exsr Visualizza;
017600101019
017700101019         ENDSL;
017800101019       ENDDO;
017900100507
018000100507       // -?Operazioni finali?
018100100507       exsr sr_RoutEnd;
018200100507
018300100507       //--------------------------------------------------------------
018400100507       //?Operazioni iniziali.                                         ?
018500100507       //--------------------------------------------------------------
018600100507       BEGSR  sr_RoutInz;
018700100507
018800100507         // -?Reperimento dati job?
018900100507         exsr  sr_DatiJob;
019000100507
019100100507         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
019200101019         wDate = %dec(%date());
019300100507
019400100507         // -?Impostazione campi fissi?
019500100507         k_TBLkut = 1;
019600101011
019700101011         exec sql set option dynusrprf = *owner,
019800101011                             closqlcsr = *endmod;
019900100507
020000100507       ENDSR;
020100100507
020200100507       //--------------------------------------------------------------
020300100507       //?Reperimento Dati del job (Utente/Operativi).                 ?
020400100507       //--------------------------------------------------------------
020500100507       BEGSR  sr_DatiJob;
020600100507
020700100507         in(E) §AzUte;
020800100507         if NOT %error;
020900100507           in(E) §DatiUte;
021000100507         endif;
021100100507         if %error or RSut = *blanks;
021200100507           clear TIBS34ds;
021300100507           tibs34r ( tibs34ds );
021400100507           in §AzUte;
021500100507           in §DatiUte;
021600100507         endif;
021700100507
021800100507       ENDSR;
021900101019
022000101019       //--------------------------------------------------------------
022100101019       //?Elaborazione.
022200101019       //--------------------------------------------------------------
022300101019       BEGSR  Elabora;
022400101025
022500101025       // -?Pulizia ed apertura WrkF?
022600101025         Qcmd = 'CLRPFM FILE(';
022700101025         IF  %subst(knsif : 7 : 1) = 'P';
022800101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
022900101025           wFLib = 'GAITRAAZP/WFATT00F';
023000101025         ELSE;
023100101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
023200101025           wFLib = 'GAITRAAZM/WFATT00F';
023300101025         ENDIF;
023400101025         Qcmd = %trimr(Qcmd) + '/WFATT00F)';
023500101025         ExecuteCommand ( Qcmd : %size(Qcmd));
023600101025         open  WFATT00F;
023700101019
023800101019       // -?Elaboro trattative aperte senza attività?
023900101019         exsr sr_ElaRcd_1;
024000101019
024100101019       // -?Elaboro attività aperte su trattative chiuse?
024200101019         exsr sr_ElaRcd_2;
024300101019
024400101019       // -?Elaboro appuntamenti non eseguiti entro 3 gg.lav.?
024500101019         exsr sr_ElaRcd_3;
024600101130
024700101130       // -?Elaboro trattative aperte senza info trattative?
024800101130         exsr sr_ElaRcd_4;
024900101019
025000101019         $fine = *on;
025100101019
025200101019       ENDSR;
025300101025
025400101025       //--------------------------------------------------------------
025500101025       //?Visualizza.
025600101025       //--------------------------------------------------------------
025700101025       BEGSR  Visualizza;
025800101025
025900101025         // -?Visualizzo il file?
026000101025         Qcmd = 'DSPDBF FILE(';
026100101025         IF  %subst(knsif : 7 : 1) = 'P';
026200101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
026300101025         ELSE;
026400101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
026500101025         ENDIF;
026600101025         Qcmd = %trimr(Qcmd) + '/WFATT00F) mbr(*first) Output(*)';
026700101025         callp(e) ExecuteCommand ( Qcmd : %size(Qcmd));
026800101025
026900101025       ENDSR;
027000101019
027100101019       //--------------------------------------------------------------
027200101019       //?Elaborazione rcd 1 - Trattative APERTE senza attività
027300101019       //--------------------------------------------------------------
027400101019       BEGSR  sr_ElaRcd_1;
027500101019
027600101019         wSQL = 'with ATT as (select ATCnrv from TIATC00F +
027700101019                 where ATCdco = 0 and ATCnrv > 0) +
027800101019                 select VIScmm, VISnrv, VISrag, VISdat from ';
027900101019
028000110803        // IF  %subst(knsif : 7 : 1) = 'P';
028100110803        //   wSQL += 'FILTRAGRPF/TIVIS00F ';
028200110803        // ELSE;
028300110803        //   wSQL += 'FILTRAGRU/TIVIS00F ';
028400110803        // ENDIF;
028500101019
028600110803         wSQL += 'TIVIS00F ';
028700101019         wSQL += 'exception join ATT on VISnrv = ATCnrv +
028800101019                  where VISffz = '' '' and VISdch = 0 +
028900101019                  order by VIScmm, VISnrv';
029000101019
029100101019         // -?Cerco trattative APERTE senza attività?
029200101019         exec sql
029300101019           prepare S1 from :wSQL;
029400101019         exec sql
029500101019          declare RCD1 cursor for S1;
029600101019
029700101019         exec sql
029800101019          open RCD1;
029900101019          IF sqlcode < 0;
030000101019            $End1 = *on;
030100101019          ENDIF;
030200101019
030300101019         DOW not $End1;
030400101019          exec sql
030500101019           fetch next from RCD1 into :VIScmm, :VISnrv,
030600101019                                     :VISrag, :VISdat;
030700101019           IF sqlcod = 100 or sqlcod < 0;
030800101019             $End1 = *on;
030900101019             leave;
031000101019           ENDIF;
031100101019
031200101019         // -?Scrivo rcd WrkF?
031300101019           exsr sr_WrtRcd_1;
031400101019
031500101019         ENDDO;
031600101019
031700101019         exec sql close RCD1;
031800101019
031900101019       ENDSR;
032000100507
032100100507       //--------------------------------------------------------------
032200101019       //?Elaborazione rcd 2 - Attività aperte su trattative CHIUSE     ?
032300100507       //--------------------------------------------------------------
032400101019       BEGSR  sr_ElaRcd_2;
032500101011
032600101011         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
032700101011                 from TIATC00F join ';
032800101011
032900110803         // IF  %subst(knsif : 7 : 1) = 'P';
033000110803         //  wSQL += 'FILTRAGRPF/TIVIS00F ';
033100110803         // ELSE;
033200110803         //  wSQL += 'FILTRAGRU/TIVIS00F ';
033300110803         // ENDIF;
033400101011
033500110803         wSQL += 'TIVIS00F ';
033600101011         wSQL += 'on ATCnrv = VISnrv where ATCdco = 0 and +
033700101011                  ATCnrv > 0 and VISdch > 0 +
033800101011                  order by VIScmm, VISnrv';
033900100510
034000101011         // -?Cerco attività aperte su trattative CHIUSE?
034100101011         exec sql
034200101019           prepare S2 from :wSQL;
034300101011         exec sql
034400101019          declare RCD2 cursor for S2;
034500101011
034600101011         exec sql
034700101019          open RCD2;
034800101011          IF sqlcode < 0;
034900101019            $End2 = *on;
035000101011          ENDIF;
035100101011
035200101019         DOW not $End2;
035300101011          exec sql
035400101019           fetch next from RCD2 into :VIScmm, :VISnrv,
035500101011                                     :VISrag, :VISdat;
035600101011           IF sqlcod = 100 or sqlcod < 0;
035700101019             $End2 = *on;
035800101011             leave;
035900101011           ENDIF;
036000101011
036100101011         // -?Scrivo rcd WrkF?
036200101019           exsr sr_WrtRcd_2;
036300101011
036400101011         ENDDO;
036500101011
036600101019         exec sql close RCD2;
036700100507
036800101011       ENDSR;
036900101019
037000101019       //--------------------------------------------------------------
037100101019       //?Elaborazione rcd 3 - Appuntamento non esitati da oltre 3 gg.  ?
037200101019       //--------------------------------------------------------------
037300101019       BEGSR  sr_ElaRcd_3;
037400101019
037500101019         wSQL = 'select ATCcmm, ATCnrv, ATCdad, VISrag, VISdat +
037600101019                 from TIATC00F join ';
037700101019
037800110803         // IF  %subst(knsif : 7 : 1) = 'P';
037900110803         //  wSQL += 'FILTRAGRPF/TIVIS00F ';
038000110803         // ELSE;
038100110803         //  wSQL += 'FILTRAGRU/TIVIS00F ';
038200110803         // ENDIF;
038300101019
038400110803         wSQL += 'TIVIS00F ';
038500101019         wSQL += 'on VISnrv = ATCnrv +
038600101019                  where ATCcad = ''91'' and ATCdco = 0 +
038700101019                  and ATCdad < ';
038800101019         wSQL += %editc(wdate:'X');
038900101019         wSQL += ' order by ATCcmm, ATCnrv';
039000101019
039100101019         // -?Cerco appuntamenti NON fatti?
039200101019         exec sql
039300101019           prepare S3 from :wSQL;
039400101019         exec sql
039500101019          declare RCD3 cursor for S3;
039600101019
039700101019         exec sql
039800101019          open RCD3;
039900101019          IF sqlcode < 0;
040000101019            $End3 = *on;
040100101019          ENDIF;
040200101019
040300101019         DOW not $End3;
040400101019          exec sql
040500101019           fetch next from RCD3 into :ATCcmm, :ATCnrv,
040600101019                                     :ATCdad, :VISrag,
040700101019                                     :VISdat;
040800101019           IF sqlcod = 100 or sqlcod < 0;
040900101019             $End3 = *on;
041000101019             leave;
041100101019           ENDIF;
041200101019
041300101019         // -?Controllo se sono passati 3 gg lavorativi?
041400101019           clear XGIOLAVDS;
041500101019           IXGLdata = wdate;
041600101019           IXGLsub  = 'S';
041700101019           IXGLgg   = 3;
041800101019           IXGLpa   = 'P';
041900101019           IXGLlav  = 'S';
042000101019           IXGLfil = %dec(%subst(%editc(ATCcmm:'X'):1:3):3:0);
042100101019           XGIOLAV (xgiolavds);
042200101019           IF  ATCdad < OXGLdata;
042300101019
042400101019           // -?Scrivo rcd WrkF?
042500101019             exsr sr_WrtRcd_3;
042600101019           ENDIF;
042700101019
042800101019         ENDDO;
042900101019
043000101019         exec sql close RCD3;
043100101019       ENDSR;
043200101130
043300101130       //--------------------------------------------------------------
043400101130       //?Elaborazione rcd - Trattative APERTE senza info trattative
043500101130       //--------------------------------------------------------------
043600101130       BEGSR  sr_ElaRcd_4;
043700101130
043800101130         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
043900101130                 from ';
044000101130
044100110803         // IF  %subst(knsif : 7 : 1) = 'P';
044200110803         //  wSQL += 'FILTRAGRPF/TIVIS00F ';
044300110803         // ELSE;
044400110803         //  wSQL += 'FILTRAGRU/TIVIS00F ';
044500110803         // ENDIF;
044600101130
044700110803         wSQL += 'TIVIS00F ';
044800101130         wSQL += 'join ';
044900101130
045000101130         IF  %subst(knsif : 7 : 1) = 'P';
045100101130           wSQL += 'FILTRAGRPF/TNOFM00F ';
045200101130         ELSE;
045300101130           wSQL += 'FILTRAGRU/TNOFM00F ';
045400101130         ENDIF;
045500101130
045600101130         wSQL += 'on VISnrv = TAMksc ';
045700101130
045800101130         wSQL += 'where VISdch = 0 and +
045900101130                  VISffz = '' '' and VISinfot = '' '' +
046000110802                  and VIStpv <> ''R'' +
046100101130                  order by VIScmm, VISnrv';
046200101130
046300101130         // -?Cerco attività aperte su trattative CHIUSE?
046400101130         exec sql
046500101130           prepare S4 from :wSQL;
046600101130         exec sql
046700101130          declare RCD4 cursor for S4;
046800101130
046900101130         exec sql
047000101130          open RCD4;
047100101130          IF sqlcode < 0;
047200101130            $End4 = *on;
047300101130          ENDIF;
047400101130
047500101130         DOW not $End4;
047600101130          exec sql
047700101130           fetch next from RCD4 into :VIScmm, :VISnrv,
047800101130                                    :VISrag, :VISdat;
047900101130           IF sqlcod = 100 or sqlcod < 0;
048000101130             $End4 = *on;
048100101130             leave;
048200101130           ENDIF;
048300101130
048400101130         // -?Scrivo rcd WrkF?
048500101130         //  ?a rottura di trattativa?
048600101130           IF  VISnrv <> OLD_VISnrv;
048700101130             exsr sr_WrtRcd_4;
048800101130             OLD_VISnrv = VISnrv;
048900101130           ENDIF;
049000101130
049100101130         ENDDO;
049200101130
049300101130         exec sql close RCD4;
049400101130
049500101130       ENDSR;
049600101019
049700101019       //--------------------------------------------------------------
049800101019       //?Scrittura rcd 1 - Attività aperte su trattative APERTE     ?
049900101019       //--------------------------------------------------------------
050000101019       BEGSR  sr_WrtRcd_1;
050100101019
050200101019         // -?Cerco descrizione commerciale e dati unificante?
050300101019         exsr sr_DatiCmm;
050400101019
050500101019         clear  WFATT000;
050600101019         WATute  = SDSusr;
050700101019         WATdta  = *date;
050800101019         WATtrc  = '1';
050900101019         WATnrv  = VISnrv;
051000101019         WATdat  = VISdat;
051100101019         WATrag  = VISrag;
051200101019         WATcmm  = VIScmm;
051300101019         WATcmmD = cmm§01age;
051400101019         WATcmu  = cmm§01rgf;
051500101019         WATcmuD = uni§01age;
051600101019         IF  uni§OGara > *blank;
051700101019           WATara  = %int( uni§OGara );
051800101019         ENDIF;
051900101019         WATaraD = uni§05des;
052000101019         IF  ORGfl3_uni > *blank;
052100101019           WATdis  =  ORGfl3_uni;
052200101019         ENDIF;
052300101019         WATdisD = uni§17des;
052400101019
052500101019         //______________
052600101019         write  WFATT000;
052700101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052800101019
052900101019       ENDSR;
053000101019
053100101011       //--------------------------------------------------------------
053200101019       //?Scrittura rcd 2 - Attività aperte su trattative CHIUSE     ?
053300101011       //--------------------------------------------------------------
053400101019       BEGSR  sr_WrtRcd_2;
053500101011
053600101019         // -?Cerco descrizione commerciale e dati unificante?
053700101011         exsr sr_DatiCmm;
053800100507
053900101011         clear  WFATT000;
054000101011         WATute  = SDSusr;
054100101011         WATdta  = *date;
054200101019         WATtrc  = '2';
054300101011         WATnrv  = VISnrv;
054400101011         WATdat  = VISdat;
054500101011         WATrag  = VISrag;
054600101011         WATcmm  = VIScmm;
054700101011         WATcmmD = cmm§01age;
054800101019         WATcmu  = cmm§01rgf;
054900101019         WATcmuD = uni§01age;
055000101019         IF  uni§OGara > *blank;
055100101019           WATara  = %int( uni§OGara );
055200101011         ENDIF;
055300101019         WATaraD = uni§05des;
055400101019         IF  ORGfl3_uni > *blank;
055500101019           WATdis  =  ORGfl3_uni;
055600101019         ENDIF;
055700101019         WATdisD = uni§17des;
055800100510
055900100510         //______________
056000101011         write  WFATT000;
056100100510         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056200100507
056300100507       ENDSR;
056400101019
056500101019       //--------------------------------------------------------------
056600101019       //?Scrittura rcd 3 - Appuntamenti NON fatti entro 3 gg. lav.  ?
056700101019       //--------------------------------------------------------------
056800101019       BEGSR  sr_WrtRcd_3;
056900101019
057000101019         // -?Cerco descrizione commerciale e dati unificante?
057100101019         VIScmm = ATCcmm;
057200101019         exsr sr_DatiCmm;
057300101019
057400101019         clear  WFATT000;
057500101019         WATute  = SDSusr;
057600101019         WATdta  = *date;
057700101019         WATtrc  = '3';
057800101019         WATnrv  = ATCnrv;
057900101019         WATdat  = VISdat;
058000101019         WATrag  = VISrag;
058100101019         WATdad  = ATCdad;
058200101019         WATcmm  = VIScmm;
058300101019         WATcmmD = cmm§01age;
058400101019         WATcmu  = cmm§01rgf;
058500101019         WATcmuD = uni§01age;
058600101019         IF  uni§OGara > *blank;
058700101019           WATara  = %int( uni§OGara );
058800101019         ENDIF;
058900101019         WATaraD = uni§05des;
059000101019         IF  ORGfl3_uni > *blank;
059100101019           WATdis  =  ORGfl3_uni;
059200101019         ENDIF;
059300101019         WATdisD = uni§17des;
059400101019
059500101019         //______________
059600101019         write  WFATT000;
059700101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059800101019
059900101019       ENDSR;
060000101130
060100101130       //--------------------------------------------------------------
060200101130       //?Scrittura rcd 3 - Trattative aperte senza info trattativa
060300101130       //--------------------------------------------------------------
060400101130       BEGSR  sr_WrtRcd_4;
060500101130
060600101130         // -?Cerco descrizione commerciale e dati unificante?
060700101130         exsr sr_DatiCmm;
060800101130
060900101130         clear  WFATT000;
061000101130         WATute  = SDSusr;
061100101130         WATdta  = *date;
061200101130         WATtrc  = '4';
061300101130         WATnrv  = VISnrv;
061400101130         WATdat  = VISdat;
061500101130         WATrag  = VISrag;
061600101130         WATcmm  = VIScmm;
061700101130         WATcmmD = cmm§01age;
061800101130         WATcmu  = cmm§01rgf;
061900101130         WATcmuD = uni§01age;
062000101130         IF  uni§OGara > *blank;
062100101130           WATara  = %int( uni§OGara );
062200101130         ENDIF;
062300101130         WATaraD = uni§05des;
062400101130         IF  ORGfl3_uni > *blank;
062500101130           WATdis  =  ORGfl3_uni;
062600101130         ENDIF;
062700101130         WATdisD = uni§17des;
062800101130
062900101130         //______________
063000101130         write  WFATT000;
063100101130         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063200101130
063300101130       ENDSR;
063400101011
063500101011       //--------------------------------------------------------------
063600101011       //?Cerco dati del commerciale
063700101011       //--------------------------------------------------------------
063800101011       BEGSR  sr_DatiCmm;
063900101011
064000101011         // -?Reperimento dati commerciale?
064100101011         IF  TBLkey_Cmm_01 <> %editc( VIScmm : 'X' );
064200101011           TBLkey_Cmm_01 = %editc( VIScmm : 'X' );
064300101011           clear  ds01cmm;
064400101011           k_TBLcod = '01';
064500101011           k_TBLkey = TBLkey_Cmm_01;
064600101011           chain  %kds(k03tabel00)  TABEL;
064700101011           IF  %found(TABEL00F)   and   TBLflg = *blank;
064800101011             ds01cmm = TBLuni;
064900101011           ENDIF;
065000101011         ENDIF;
065100101011
065200101019         // -?Reperimento area e distretto unificante?
065300101019         IF  ORGfil_uni <>
065400101019                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
065500101011
065600101019           ORGfil_uni =
065700101019                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
065800101019           clear  Og140uni;
065900101019           clear  ORGfl3_uni;
066000101019           chain  (ORGfil_uni)  AZORG;
066100101011
066200101011           IF  %found(AZORG01L);
066300101019             ORGfl3_uni = ORGfl3;
066400101019             Og140uni  = ORGde0;
066500101011           ENDIF;
066600101011         ENDIF;
066700101019
066800101019         // -?Decodifica unificante?
066900101019         If  TBLkey_Uni_01 <> %editc( cmm§01rgf : 'X' );
067000101019           TBLkey_Uni_01 = %editc( cmm§01rgf : 'X' );
067100101019           clear  ds01uni;
067200101019           k_TBLcod = '01';
067300101019           k_TBLkey = TBLkey_Uni_01;
067400101019           chain  %kds(k03tabel00)  TABEL;
067500101019           if  %found(TABEL00F)   and   TBLflg = *blank;
067600101019             ds01uni = TBLuni;
067700101019           endif;
067800101019         endif;
067900101011
068000101019         // -?Decodifica raggruppamento area unificante?
068100101019         IF  TBLkey_Uni_05 <> uni§OGara;
068200101019           TBLkey_Uni_05 = uni§OGara;
068300101019           clear  ds05uni;
068400101011           k_TBLcod = '05';
068500101019           k_TBLkey = TBLkey_Uni_05;
068600101011           chain  %kds(k03tabel00)  TABEL;
068700101011           IF  %found(TABEL00F)   and   TBLflg = *blank;
068800101019             ds05uni = TBLuni;
068900101011           ENDIF;
069000101011         ENDIF;
069100101019
069200101019         // -?Decodifica distretto unificante?
069300101019         IF  TBLkey_Uni_17 <> ORGfl3_uni;
069400101019           TBLkey_Uni_17 = ORGfl3_uni;
069500101019           clear  ds17uni;
069600101019           k_TBLcod = '17';
069700101019           k_TBLkey = TBLkey_Uni_17;
069800101019           chain  %kds(k03tabel00)  TABEL;
069900101019           IF  %found(TABEL00F)   and   TBLflg = *blank;
070000101019             ds17uni = TBLuni;
070100101019           ENDIF;
070200101019         ENDIF;
070300101011
070400101011       ENDSR;
070500100507
070600100507       //--------------------------------------------------------------
070700100507       //?Operazioni finali.                                           ?
070800100507       //--------------------------------------------------------------
070900100507       BEGSR  sr_RoutEnd;
071000101011
071100101011         *inLR = *on;
071200100507
071300100510         // -?Uscita dal *pgm?
071400100507         return;
071500100507
071600100507       ENDSR;
071700100507
071800100507      /end-free
