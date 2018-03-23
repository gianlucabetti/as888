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
028000101019         IF  %subst(knsif : 7 : 1) = 'P';
028100101019           wSQL += 'FILTRAGRPF/TIVIS00F ';
028200101019         ELSE;
028300101019           wSQL += 'FILTRAGRU/TIVIS00F ';
028400101019         ENDIF;
028500101019
028600101019         wSQL += 'exception join ATT on VISnrv = ATCnrv +
028700101019                  where VISffz = '' '' and VISdch = 0 +
028800101019                  order by VIScmm, VISnrv';
028900101019
029000101019         // -?Cerco trattative APERTE senza attività?
029100101019         exec sql
029200101019           prepare S1 from :wSQL;
029300101019         exec sql
029400101019          declare RCD1 cursor for S1;
029500101019
029600101019         exec sql
029700101019          open RCD1;
029800101019          IF sqlcode < 0;
029900101019            $End1 = *on;
030000101019          ENDIF;
030100101019
030200101019         DOW not $End1;
030300101019          exec sql
030400101019           fetch next from RCD1 into :VIScmm, :VISnrv,
030500101019                                     :VISrag, :VISdat;
030600101019           IF sqlcod = 100 or sqlcod < 0;
030700101019             $End1 = *on;
030800101019             leave;
030900101019           ENDIF;
031000101019
031100101019         // -?Scrivo rcd WrkF?
031200101019           exsr sr_WrtRcd_1;
031300101019
031400101019         ENDDO;
031500101019
031600101019         exec sql close RCD1;
031700101019
031800101019       ENDSR;
031900100507
032000100507       //--------------------------------------------------------------
032100101019       //?Elaborazione rcd 2 - Attività aperte su trattative CHIUSE     ?
032200100507       //--------------------------------------------------------------
032300101019       BEGSR  sr_ElaRcd_2;
032400101011
032500101011         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
032600101011                 from TIATC00F join ';
032700101011
032800101011         IF  %subst(knsif : 7 : 1) = 'P';
032900101011           wSQL += 'FILTRAGRPF/TIVIS00F ';
033000101011         ELSE;
033100101011           wSQL += 'FILTRAGRU/TIVIS00F ';
033200101011         ENDIF;
033300101011
033400101011         wSQL += 'on ATCnrv = VISnrv where ATCdco = 0 and +
033500101011                  ATCnrv > 0 and VISdch > 0 +
033600101011                  order by VIScmm, VISnrv';
033700100510
033800101011         // -?Cerco attività aperte su trattative CHIUSE?
033900101011         exec sql
034000101019           prepare S2 from :wSQL;
034100101011         exec sql
034200101019          declare RCD2 cursor for S2;
034300101011
034400101011         exec sql
034500101019          open RCD2;
034600101011          IF sqlcode < 0;
034700101019            $End2 = *on;
034800101011          ENDIF;
034900101011
035000101019         DOW not $End2;
035100101011          exec sql
035200101019           fetch next from RCD2 into :VIScmm, :VISnrv,
035300101011                                     :VISrag, :VISdat;
035400101011           IF sqlcod = 100 or sqlcod < 0;
035500101019             $End2 = *on;
035600101011             leave;
035700101011           ENDIF;
035800101011
035900101011         // -?Scrivo rcd WrkF?
036000101019           exsr sr_WrtRcd_2;
036100101011
036200101011         ENDDO;
036300101011
036400101019         exec sql close RCD2;
036500100507
036600101011       ENDSR;
036700101019
036800101019       //--------------------------------------------------------------
036900101019       //?Elaborazione rcd 3 - Appuntamento non esitati da oltre 3 gg.  ?
037000101019       //--------------------------------------------------------------
037100101019       BEGSR  sr_ElaRcd_3;
037200101019
037300101019         wSQL = 'select ATCcmm, ATCnrv, ATCdad, VISrag, VISdat +
037400101019                 from TIATC00F join ';
037500101019
037600101019         IF  %subst(knsif : 7 : 1) = 'P';
037700101019           wSQL += 'FILTRAGRPF/TIVIS00F ';
037800101019         ELSE;
037900101019           wSQL += 'FILTRAGRU/TIVIS00F ';
038000101019         ENDIF;
038100101019
038200101019         wSQL += 'on VISnrv = ATCnrv +
038300101019                  where ATCcad = ''91'' and ATCdco = 0 +
038400101019                  and ATCdad < ';
038500101019         wSQL += %editc(wdate:'X');
038600101019         wSQL += ' order by ATCcmm, ATCnrv';
038700101019
038800101019         // -?Cerco appuntamenti NON fatti?
038900101019         exec sql
039000101019           prepare S3 from :wSQL;
039100101019         exec sql
039200101019          declare RCD3 cursor for S3;
039300101019
039400101019         exec sql
039500101019          open RCD3;
039600101019          IF sqlcode < 0;
039700101019            $End3 = *on;
039800101019          ENDIF;
039900101019
040000101019         DOW not $End3;
040100101019          exec sql
040200101019           fetch next from RCD3 into :ATCcmm, :ATCnrv,
040300101019                                     :ATCdad, :VISrag,
040400101019                                     :VISdat;
040500101019           IF sqlcod = 100 or sqlcod < 0;
040600101019             $End3 = *on;
040700101019             leave;
040800101019           ENDIF;
040900101019
041000101019         // -?Controllo se sono passati 3 gg lavorativi?
041100101019           clear XGIOLAVDS;
041200101019           IXGLdata = wdate;
041300101019           IXGLsub  = 'S';
041400101019           IXGLgg   = 3;
041500101019           IXGLpa   = 'P';
041600101019           IXGLlav  = 'S';
041700101019           IXGLfil = %dec(%subst(%editc(ATCcmm:'X'):1:3):3:0);
041800101019           XGIOLAV (xgiolavds);
041900101019           IF  ATCdad < OXGLdata;
042000101019
042100101019           // -?Scrivo rcd WrkF?
042200101019             exsr sr_WrtRcd_3;
042300101019           ENDIF;
042400101019
042500101019         ENDDO;
042600101019
042700101019         exec sql close RCD3;
042800101019       ENDSR;
042900101130
043000101130       //--------------------------------------------------------------
043100101130       //?Elaborazione rcd - Trattative APERTE senza info trattative
043200101130       //--------------------------------------------------------------
043300101130       BEGSR  sr_ElaRcd_4;
043400101130
043500101130         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
043600101130                 from ';
043700101130
043800101130         IF  %subst(knsif : 7 : 1) = 'P';
043900101130           wSQL += 'FILTRAGRPF/TIVIS00F ';
044000101130         ELSE;
044100101130           wSQL += 'FILTRAGRU/TIVIS00F ';
044200101130         ENDIF;
044300101130
044400101130         wSQL += 'join ';
044500101130
044600101130         IF  %subst(knsif : 7 : 1) = 'P';
044700101130           wSQL += 'FILTRAGRPF/TNOFM00F ';
044800101130         ELSE;
044900101130           wSQL += 'FILTRAGRU/TNOFM00F ';
045000101130         ENDIF;
045100101130
045200101130         wSQL += 'on VISnrv = TAMksc ';
045300101130
045400101130         wSQL += 'where VISdch = 0 and +
045500101130                  VISffz = '' '' and VISinfot = '' '' +
045600101130                  order by VIScmm, VISnrv';
045700101130
045800101130         // -?Cerco attività aperte su trattative CHIUSE?
045900101130         exec sql
046000101130           prepare S4 from :wSQL;
046100101130         exec sql
046200101130          declare RCD4 cursor for S4;
046300101130
046400101130         exec sql
046500101130          open RCD4;
046600101130          IF sqlcode < 0;
046700101130            $End4 = *on;
046800101130          ENDIF;
046900101130
047000101130         DOW not $End4;
047100101130          exec sql
047200101130           fetch next from RCD4 into :VIScmm, :VISnrv,
047300101130                                    :VISrag, :VISdat;
047400101130           IF sqlcod = 100 or sqlcod < 0;
047500101130             $End4 = *on;
047600101130             leave;
047700101130           ENDIF;
047800101130
047900101130         // -?Scrivo rcd WrkF?
048000101130         //  ?a rottura di trattativa?
048100101130           IF  VISnrv <> OLD_VISnrv;
048200101130             exsr sr_WrtRcd_4;
048300101130             OLD_VISnrv = VISnrv;
048400101130           ENDIF;
048500101130
048600101130         ENDDO;
048700101130
048800101130         exec sql close RCD4;
048900101130
049000101130       ENDSR;
049100101019
049200101019       //--------------------------------------------------------------
049300101019       //?Scrittura rcd 1 - Attività aperte su trattative APERTE     ?
049400101019       //--------------------------------------------------------------
049500101019       BEGSR  sr_WrtRcd_1;
049600101019
049700101019         // -?Cerco descrizione commerciale e dati unificante?
049800101019         exsr sr_DatiCmm;
049900101019
050000101019         clear  WFATT000;
050100101019         WATute  = SDSusr;
050200101019         WATdta  = *date;
050300101019         WATtrc  = '1';
050400101019         WATnrv  = VISnrv;
050500101019         WATdat  = VISdat;
050600101019         WATrag  = VISrag;
050700101019         WATcmm  = VIScmm;
050800101019         WATcmmD = cmm§01age;
050900101019         WATcmu  = cmm§01rgf;
051000101019         WATcmuD = uni§01age;
051100101019         IF  uni§OGara > *blank;
051200101019           WATara  = %int( uni§OGara );
051300101019         ENDIF;
051400101019         WATaraD = uni§05des;
051500101019         IF  ORGfl3_uni > *blank;
051600101019           WATdis  =  ORGfl3_uni;
051700101019         ENDIF;
051800101019         WATdisD = uni§17des;
051900101019
052000101019         //______________
052100101019         write  WFATT000;
052200101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052300101019
052400101019       ENDSR;
052500101019
052600101011       //--------------------------------------------------------------
052700101019       //?Scrittura rcd 2 - Attività aperte su trattative CHIUSE     ?
052800101011       //--------------------------------------------------------------
052900101019       BEGSR  sr_WrtRcd_2;
053000101011
053100101019         // -?Cerco descrizione commerciale e dati unificante?
053200101011         exsr sr_DatiCmm;
053300100507
053400101011         clear  WFATT000;
053500101011         WATute  = SDSusr;
053600101011         WATdta  = *date;
053700101019         WATtrc  = '2';
053800101011         WATnrv  = VISnrv;
053900101011         WATdat  = VISdat;
054000101011         WATrag  = VISrag;
054100101011         WATcmm  = VIScmm;
054200101011         WATcmmD = cmm§01age;
054300101019         WATcmu  = cmm§01rgf;
054400101019         WATcmuD = uni§01age;
054500101019         IF  uni§OGara > *blank;
054600101019           WATara  = %int( uni§OGara );
054700101011         ENDIF;
054800101019         WATaraD = uni§05des;
054900101019         IF  ORGfl3_uni > *blank;
055000101019           WATdis  =  ORGfl3_uni;
055100101019         ENDIF;
055200101019         WATdisD = uni§17des;
055300100510
055400100510         //______________
055500101011         write  WFATT000;
055600100510         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
055700100507
055800100507       ENDSR;
055900101019
056000101019       //--------------------------------------------------------------
056100101019       //?Scrittura rcd 3 - Appuntamenti NON fatti entro 3 gg. lav.  ?
056200101019       //--------------------------------------------------------------
056300101019       BEGSR  sr_WrtRcd_3;
056400101019
056500101019         // -?Cerco descrizione commerciale e dati unificante?
056600101019         VIScmm = ATCcmm;
056700101019         exsr sr_DatiCmm;
056800101019
056900101019         clear  WFATT000;
057000101019         WATute  = SDSusr;
057100101019         WATdta  = *date;
057200101019         WATtrc  = '3';
057300101019         WATnrv  = ATCnrv;
057400101019         WATdat  = VISdat;
057500101019         WATrag  = VISrag;
057600101019         WATdad  = ATCdad;
057700101019         WATcmm  = VIScmm;
057800101019         WATcmmD = cmm§01age;
057900101019         WATcmu  = cmm§01rgf;
058000101019         WATcmuD = uni§01age;
058100101019         IF  uni§OGara > *blank;
058200101019           WATara  = %int( uni§OGara );
058300101019         ENDIF;
058400101019         WATaraD = uni§05des;
058500101019         IF  ORGfl3_uni > *blank;
058600101019           WATdis  =  ORGfl3_uni;
058700101019         ENDIF;
058800101019         WATdisD = uni§17des;
058900101019
059000101019         //______________
059100101019         write  WFATT000;
059200101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
059300101019
059400101019       ENDSR;
059500101130
059600101130       //--------------------------------------------------------------
059700101130       //?Scrittura rcd 3 - Trattative aperte senza info trattativa
059800101130       //--------------------------------------------------------------
059900101130       BEGSR  sr_WrtRcd_4;
060000101130
060100101130         // -?Cerco descrizione commerciale e dati unificante?
060200101130         exsr sr_DatiCmm;
060300101130
060400101130         clear  WFATT000;
060500101130         WATute  = SDSusr;
060600101130         WATdta  = *date;
060700101130         WATtrc  = '4';
060800101130         WATnrv  = VISnrv;
060900101130         WATdat  = VISdat;
061000101130         WATrag  = VISrag;
061100101130         WATcmm  = VIScmm;
061200101130         WATcmmD = cmm§01age;
061300101130         WATcmu  = cmm§01rgf;
061400101130         WATcmuD = uni§01age;
061500101130         IF  uni§OGara > *blank;
061600101130           WATara  = %int( uni§OGara );
061700101130         ENDIF;
061800101130         WATaraD = uni§05des;
061900101130         IF  ORGfl3_uni > *blank;
062000101130           WATdis  =  ORGfl3_uni;
062100101130         ENDIF;
062200101130         WATdisD = uni§17des;
062300101130
062400101130         //______________
062500101130         write  WFATT000;
062600101130         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062700101130
062800101130       ENDSR;
062900101011
063000101011       //--------------------------------------------------------------
063100101011       //?Cerco dati del commerciale
063200101011       //--------------------------------------------------------------
063300101011       BEGSR  sr_DatiCmm;
063400101011
063500101011         // -?Reperimento dati commerciale?
063600101011         IF  TBLkey_Cmm_01 <> %editc( VIScmm : 'X' );
063700101011           TBLkey_Cmm_01 = %editc( VIScmm : 'X' );
063800101011           clear  ds01cmm;
063900101011           k_TBLcod = '01';
064000101011           k_TBLkey = TBLkey_Cmm_01;
064100101011           chain  %kds(k03tabel00)  TABEL;
064200101011           IF  %found(TABEL00F)   and   TBLflg = *blank;
064300101011             ds01cmm = TBLuni;
064400101011           ENDIF;
064500101011         ENDIF;
064600101011
064700101019         // -?Reperimento area e distretto unificante?
064800101019         IF  ORGfil_uni <>
064900101019                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
065000101011
065100101019           ORGfil_uni =
065200101019                  %int( %subst( %editc( cmm§01rgf : 'X' ) : 1 : 3 ) );
065300101019           clear  Og140uni;
065400101019           clear  ORGfl3_uni;
065500101019           chain  (ORGfil_uni)  AZORG;
065600101011
065700101011           IF  %found(AZORG01L);
065800101019             ORGfl3_uni = ORGfl3;
065900101019             Og140uni  = ORGde0;
066000101011           ENDIF;
066100101011         ENDIF;
066200101019
066300101019         // -?Decodifica unificante?
066400101019         If  TBLkey_Uni_01 <> %editc( cmm§01rgf : 'X' );
066500101019           TBLkey_Uni_01 = %editc( cmm§01rgf : 'X' );
066600101019           clear  ds01uni;
066700101019           k_TBLcod = '01';
066800101019           k_TBLkey = TBLkey_Uni_01;
066900101019           chain  %kds(k03tabel00)  TABEL;
067000101019           if  %found(TABEL00F)   and   TBLflg = *blank;
067100101019             ds01uni = TBLuni;
067200101019           endif;
067300101019         endif;
067400101011
067500101019         // -?Decodifica raggruppamento area unificante?
067600101019         IF  TBLkey_Uni_05 <> uni§OGara;
067700101019           TBLkey_Uni_05 = uni§OGara;
067800101019           clear  ds05uni;
067900101011           k_TBLcod = '05';
068000101019           k_TBLkey = TBLkey_Uni_05;
068100101011           chain  %kds(k03tabel00)  TABEL;
068200101011           IF  %found(TABEL00F)   and   TBLflg = *blank;
068300101019             ds05uni = TBLuni;
068400101011           ENDIF;
068500101011         ENDIF;
068600101019
068700101019         // -?Decodifica distretto unificante?
068800101019         IF  TBLkey_Uni_17 <> ORGfl3_uni;
068900101019           TBLkey_Uni_17 = ORGfl3_uni;
069000101019           clear  ds17uni;
069100101019           k_TBLcod = '17';
069200101019           k_TBLkey = TBLkey_Uni_17;
069300101019           chain  %kds(k03tabel00)  TABEL;
069400101019           IF  %found(TABEL00F)   and   TBLflg = *blank;
069500101019             ds17uni = TBLuni;
069600101019           ENDIF;
069700101019         ENDIF;
069800101011
069900101011       ENDSR;
070000100507
070100100507       //--------------------------------------------------------------
070200100507       //?Operazioni finali.                                           ?
070300100507       //--------------------------------------------------------------
070400100507       BEGSR  sr_RoutEnd;
070500101011
070600101011         *inLR = *on;
070700100507
070800100510         // -?Uscita dal *pgm?
070900100507         return;
071000100507
071100100507       ENDSR;
071200100507
071300100507      /end-free
