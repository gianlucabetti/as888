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
002000130904
002100130904       // -?Anagrafica Commerciali?
002200130904     fAZCMM01L  if   e           k disk
002300100507
002400101011       // -?Elenco trattative
002500101011     fWFATT00F  o    e             disk    usropn
002600101011     f                                     extfile(wFLib)
002700101019
002800101019       // -?File video
002900101019     fTNTAC1D   cf   e             workstn
003000100507
003100100507       //--------------------------------------------------------------
003200100507       //?Definizione costanti.                                        ?
003300100507       //--------------------------------------------------------------
003400100507
003500100507       //--------------------------------------------------------------
003600100507       //?Definizione schiere.                                         ?
003700100507       //--------------------------------------------------------------
003800100507
003900100507       //--------------------------------------------------------------
004000100507       //?Definizione aree dati.                                       ?
004100100507       //--------------------------------------------------------------
004200100507
004300100507       // -?Dati utente?
004400100507     d §AzUte        e ds                  extname(AZUTE00F)
004500100507     d                                     dtaara
004600100507     d §DatiUte      e ds                  extname(dDatiUte)
004700100507     d                                     dtaara
004800100507
004900100507       //--------------------------------------------------------------
005000100507       //?Definizione strutture dati.                                  ?
005100100507       //--------------------------------------------------------------
005200100507
005300100507       // -?Status ds?
005400100507     d Status         sds
005500100507     d  SDSpgm           *proc
005600100507     d  SDSusr               254    263
005700101011
005800101011       // -?Parametri ricevuti
005900101011     d KPJBA         e ds
006000100507
006100100507       // -?Parametri per Reperimento dati utente?
006200100507     d TIBS34ds      e ds
006300100507
006400130904       // -?Salvataggio dati del Comm.le?
006500130904     d AZCMMds       e ds                  extname(AZCMM00F)
006600130904     d                                     inz  qualified
006700130904       // -?Salvataggio dati del Comm.le Unificante?
006800130904     d AZCMMds_Uni   e ds                  extname(AZCMM00F)
006900130904     d                                     inz  qualified
007000100507
007100101019       // -?Tabella "05" = Codici Area commerciale unificante
007200101019     d ds05uni       e ds                  extname(ds05)
007300101019     d                                     inz  prefix(uni)
007400101019
007500101019       // -?Tabella "17" = Codici Distretto commerciale unificante
007600101019     d ds17uni       e ds                  extname(ds17)
007700101019     d                                     inz  prefix(uni)
007800101011
007900101011       // -?File attività
008000101011     d TIATC00F      e ds                  extname(TIATC00F)
008100101011
008200101011       // -?File trattative
008300101011     d TIVIS00F      e ds                  extname(TIVIS00F)
008400101019
008500101019       // -?Controllo validità delle date
008600101019     d Xgiolavds     e ds                  inz
008700100507
008800100507       //--------------------------------------------------------------
008900100507       //?Definizione variabili globali.                               ?
009000100507       //--------------------------------------------------------------
009100101011
009200101011       // -?Flags booleani
009300101019     d $End1           s               n   inz(*off)
009400101019     d $End2           s               n   inz(*off)
009500101019     d $End3           s               n   inz(*off)
009600101130     d $End4           s               n   inz(*off)
009700101019     d $Fine           s               n   inz(*off)
009800101011
009900101011       // -?Nome libreria del work-file WFATT00F?
010000101011     d wFLib           s             21    inz
010100101011
010200101011       // -?Campi per richiamo QCMDEXC
010300101011     d Qcmd            s            128    inz
010400100507
010500150604       // -?Codice area del commerciale unificante?
010600150604     d ORGcar_uni      s                   like(ORGcar)  inz
010700101019       // -?Codice filiale del commerciale unificante?
010800101019     d ORGfil_uni      s                   like(ORGfil)  inz
010900101019       // -?Codice distretto commerciale unificante?
011000101019     d ORGfl3_uni      s                   like(ORGfl3)  inz
011100101019       // -?Codice area del commerciale unificante?
011200101019     d TBLkey_Uni_05   s                   like(TBLkey)  inz
011300101019       // -?Codice distretto del commerciale unificante
011400101019     d TBLkey_Uni_17   s                   like(TBLkey)  inz
011500101011
011600101011       // -?Stringa SQL da eseguire
011700101011     d wSQL            s           2048    Varying        inz
011800101019
011900101019       // -?Data corrente?
012000101019     d wDate           s              8s 0 inz
012100101130
012200101130       // -?Campo di comodo
012300101130     d OLD_VISnrv      s                   like(VISnrv)
012400100507
012500100507       //--------------------------------------------------------------
012600100507       //?Definizione prototipi procedure e relativi parametri.        ?
012700100507       //--------------------------------------------------------------
012800101019
012900101019       // -?Aggiunge/Toglie gg/mm dalla data
013000101019     d Xgiolav         pr                  extpgm('XGIOLAV')
013100101019     d  xgiolavds                          likeds(xgiolavds)
013200101011
013300101011       // -?Esecuzione comando di sistema
013400101011      /copy gaitrasrc/srcProtoPR,QCMDEXC
013500100507
013600100507       // -?Reperimento dati utente?
013700100507      /copy gaitrasrc/srcProtoPR,TIBS34R
013800100507
013900100507       //--------------------------------------------------------------
014000100507       //?Definizione key-list.                                        ?
014100100507       //--------------------------------------------------------------
014200100507
014300100507       // -?File TABEL00F?
014400100507     d k03tabel00    e ds                  extname(TABEL00F : *key)
014500100507     d                                     inz  prefix(k_)
014600100507
014700100507       //--------------------------------------------------------------
014800100507       //?M A I N - L I N E                                            ?
014900100507       //--------------------------------------------------------------
015000101011
015100101011     c     *Entry        plist
015200101011     c                   parm                    KPJBA
015300100507
015400100507      /free
015500100507
015600100507       // -?Operazioni iniziali?
015700100507       exsr sr_RoutInz;
015800101019
015900101019       // -?Emetto il video?
016000101019       DOW $Fine = *off;
016100101019         exfmt  TAC1W01;
016200101019
016300101019         SELECT;
016400101019
016500101019           WHEN  *inkc;
016600101019             $fine = *on;
016700101019
016800101019           WHEN  *inkf;
016900101019             exsr Elabora;
017000101025
017100101025           WHEN  *inkh;
017200101025             exsr Visualizza;
017300101019
017400101019         ENDSL;
017500101019       ENDDO;
017600100507
017700100507       // -?Operazioni finali?
017800100507       exsr sr_RoutEnd;
017900100507
018000100507       //--------------------------------------------------------------
018100100507       //?Operazioni iniziali.                                         ?
018200100507       //--------------------------------------------------------------
018300100507       BEGSR  sr_RoutInz;
018400100507
018500100507         // -?Reperimento dati job?
018600100507         exsr  sr_DatiJob;
018700100507
018800100507         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
018900101019         wDate = %dec(%date());
019000100507
019100100507         // -?Impostazione campi fissi?
019200100507         k_TBLkut = 1;
019300101011
019400101011         exec sql set option dynusrprf = *owner,
019500101011                             closqlcsr = *endmod;
019600100507
019700100507       ENDSR;
019800100507
019900100507       //--------------------------------------------------------------
020000100507       //?Reperimento Dati del job (Utente/Operativi).                 ?
020100100507       //--------------------------------------------------------------
020200100507       BEGSR  sr_DatiJob;
020300100507
020400100507         in(E) §AzUte;
020500100507         if NOT %error;
020600100507           in(E) §DatiUte;
020700100507         endif;
020800100507         if %error or RSut = *blanks;
020900100507           clear TIBS34ds;
021000100507           tibs34r ( tibs34ds );
021100100507           in §AzUte;
021200100507           in §DatiUte;
021300100507         endif;
021400100507
021500100507       ENDSR;
021600101019
021700101019       //--------------------------------------------------------------
021800101019       //?Elaborazione.
021900101019       //--------------------------------------------------------------
022000101019       BEGSR  Elabora;
022100101025
022200101025       // -?Pulizia ed apertura WrkF?
022300101025         Qcmd = 'CLRPFM FILE(';
022400101025         IF  %subst(knsif : 7 : 1) = 'P';
022500101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
022600101025           wFLib = 'GAITRAAZP/WFATT00F';
022700101025         ELSE;
022800101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
022900101025           wFLib = 'GAITRAAZM/WFATT00F';
023000101025         ENDIF;
023100101025         Qcmd = %trimr(Qcmd) + '/WFATT00F)';
023200101025         ExecuteCommand ( Qcmd : %size(Qcmd));
023300101025         open  WFATT00F;
023400101019
023500101019       // -?Elaboro trattative aperte senza attività?
023600101019         exsr sr_ElaRcd_1;
023700101019
023800101019       // -?Elaboro attività aperte su trattative chiuse?
023900101019         exsr sr_ElaRcd_2;
024000101019
024100101019       // -?Elaboro appuntamenti non eseguiti entro 3 gg.lav.?
024200101019         exsr sr_ElaRcd_3;
024300101130
024400101130       // -?Elaboro trattative aperte senza info trattative?
024500101130         exsr sr_ElaRcd_4;
024600101019
024700101019         $fine = *on;
024800101019
024900101019       ENDSR;
025000101025
025100101025       //--------------------------------------------------------------
025200101025       //?Visualizza.
025300101025       //--------------------------------------------------------------
025400101025       BEGSR  Visualizza;
025500101025
025600101025         // -?Visualizzo il file?
025700101025         Qcmd = 'DSPDBF FILE(';
025800101025         IF  %subst(knsif : 7 : 1) = 'P';
025900101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
026000101025         ELSE;
026100101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
026200101025         ENDIF;
026300101025         Qcmd = %trimr(Qcmd) + '/WFATT00F) mbr(*first) Output(*)';
026400101025         callp(e) ExecuteCommand ( Qcmd : %size(Qcmd));
026500101025
026600101025       ENDSR;
026700101019
026800101019       //--------------------------------------------------------------
026900101019       //?Elaborazione rcd 1 - Trattative APERTE senza attività
027000101019       //--------------------------------------------------------------
027100101019       BEGSR  sr_ElaRcd_1;
027200101019
027300101019         wSQL = 'with ATT as (select ATCnrv from TIATC00F +
027400101019                 where ATCdco = 0 and ATCnrv > 0) +
027500101019                 select VIScmm, VISnrv, VISrag, VISdat from ';
027600110803         wSQL += 'TIVIS00F ';
027700101019         wSQL += 'exception join ATT on VISnrv = ATCnrv +
027800101019                  where VISffz = '' '' and VISdch = 0 +
027900101019                  order by VIScmm, VISnrv';
028000101019
028100101019         // -?Cerco trattative APERTE senza attività?
028200101019         exec sql
028300101019           prepare S1 from :wSQL;
028400101019         exec sql
028500101019          declare RCD1 cursor for S1;
028600101019
028700101019         exec sql
028800101019          open RCD1;
028900101019          IF sqlcode < 0;
029000101019            $End1 = *on;
029100101019          ENDIF;
029200101019
029300101019         DOW not $End1;
029400101019          exec sql
029500101019           fetch next from RCD1 into :VIScmm, :VISnrv,
029600101019                                     :VISrag, :VISdat;
029700101019           IF sqlcod = 100 or sqlcod < 0;
029800101019             $End1 = *on;
029900101019             leave;
030000101019           ENDIF;
030100101019
030200101019         // -?Scrivo rcd WrkF?
030300101019           exsr sr_WrtRcd_1;
030400101019
030500101019         ENDDO;
030600101019
030700101019         exec sql close RCD1;
030800101019
030900101019       ENDSR;
031000100507
031100100507       //--------------------------------------------------------------
031200101019       //?Elaborazione rcd 2 - Attività aperte su trattative CHIUSE     ?
031300100507       //--------------------------------------------------------------
031400101019       BEGSR  sr_ElaRcd_2;
031500101011
031600101011         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
031700101011                 from TIATC00F join ';
031800110803         wSQL += 'TIVIS00F ';
031900101011         wSQL += 'on ATCnrv = VISnrv where ATCdco = 0 and +
032000101011                  ATCnrv > 0 and VISdch > 0 +
032100101011                  order by VIScmm, VISnrv';
032200100510
032300101011         // -?Cerco attività aperte su trattative CHIUSE?
032400101011         exec sql
032500101019           prepare S2 from :wSQL;
032600101011         exec sql
032700101019          declare RCD2 cursor for S2;
032800101011
032900101011         exec sql
033000101019          open RCD2;
033100101011          IF sqlcode < 0;
033200101019            $End2 = *on;
033300101011          ENDIF;
033400101011
033500101019         DOW not $End2;
033600101011          exec sql
033700101019           fetch next from RCD2 into :VIScmm, :VISnrv,
033800101011                                     :VISrag, :VISdat;
033900101011           IF sqlcod = 100 or sqlcod < 0;
034000101019             $End2 = *on;
034100101011             leave;
034200101011           ENDIF;
034300101011
034400101011         // -?Scrivo rcd WrkF?
034500101019           exsr sr_WrtRcd_2;
034600101011
034700101011         ENDDO;
034800101011
034900101019         exec sql close RCD2;
035000100507
035100101011       ENDSR;
035200101019
035300101019       //--------------------------------------------------------------
035400101019       //?Elaborazione rcd 3 - Appuntamento non esitati da oltre 3 gg.  ?
035500101019       //--------------------------------------------------------------
035600101019       BEGSR  sr_ElaRcd_3;
035700101019
035800101019         wSQL = 'select ATCcmm, ATCnrv, ATCdad, VISrag, VISdat +
035900101019                 from TIATC00F join ';
036000110803         wSQL += 'TIVIS00F ';
036100101019         wSQL += 'on VISnrv = ATCnrv +
036200101019                  where ATCcad = ''91'' and ATCdco = 0 +
036300101019                  and ATCdad < ';
036400101019         wSQL += %editc(wdate:'X');
036500101019         wSQL += ' order by ATCcmm, ATCnrv';
036600101019
036700101019         // -?Cerco appuntamenti NON fatti?
036800101019         exec sql
036900101019           prepare S3 from :wSQL;
037000101019         exec sql
037100101019          declare RCD3 cursor for S3;
037200101019
037300101019         exec sql
037400101019          open RCD3;
037500101019          IF sqlcode < 0;
037600101019            $End3 = *on;
037700101019          ENDIF;
037800101019
037900101019         DOW not $End3;
038000101019          exec sql
038100101019           fetch next from RCD3 into :ATCcmm, :ATCnrv,
038200101019                                     :ATCdad, :VISrag,
038300101019                                     :VISdat;
038400101019           IF sqlcod = 100 or sqlcod < 0;
038500101019             $End3 = *on;
038600101019             leave;
038700101019           ENDIF;
038800101019
038900101019         // -?Controllo se sono passati 3 gg lavorativi?
039000101019           clear XGIOLAVDS;
039100101019           IXGLdata = wdate;
039200101019           IXGLsub  = 'S';
039300101019           IXGLgg   = 3;
039400101019           IXGLpa   = 'P';
039500101019           IXGLlav  = 'S';
039600101019           IXGLfil = %dec(%subst(%editc(ATCcmm:'X'):1:3):3:0);
039700101019           XGIOLAV (xgiolavds);
039800101019           IF  ATCdad < OXGLdata;
039900101019
040000101019           // -?Scrivo rcd WrkF?
040100101019             exsr sr_WrtRcd_3;
040200101019           ENDIF;
040300101019
040400101019         ENDDO;
040500101019
040600101019         exec sql close RCD3;
040700101019       ENDSR;
040800101130
040900101130       //--------------------------------------------------------------
041000101130       //?Elaborazione rcd - Trattative APERTE senza info trattative
041100101130       //--------------------------------------------------------------
041200101130       BEGSR  sr_ElaRcd_4;
041300101130
041400101130         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
041500101130                 from ';
041600110803         wSQL += 'TIVIS00F ';
041700101130         wSQL += 'join ';
041800101130
041900101130         IF  %subst(knsif : 7 : 1) = 'P';
042000101130           wSQL += 'FILTRAGRPF/TNOFM00F ';
042100101130         ELSE;
042200101130           wSQL += 'FILTRAGRU/TNOFM00F ';
042300101130         ENDIF;
042400101130
042500101130         wSQL += 'on VISnrv = TAMksc ';
042600101130
042700101130         wSQL += 'where VISdch = 0 and +
042800101130                  VISffz = '' '' and VISinfot = '' '' +
042900110802                  and VIStpv <> ''R'' +
043000101130                  order by VIScmm, VISnrv';
043100101130
043200101130         // -?Cerco attività aperte su trattative CHIUSE?
043300101130         exec sql
043400101130           prepare S4 from :wSQL;
043500101130         exec sql
043600101130          declare RCD4 cursor for S4;
043700101130
043800101130         exec sql
043900101130          open RCD4;
044000101130          IF sqlcode < 0;
044100101130            $End4 = *on;
044200101130          ENDIF;
044300101130
044400101130         DOW not $End4;
044500101130          exec sql
044600101130           fetch next from RCD4 into :VIScmm, :VISnrv,
044700101130                                    :VISrag, :VISdat;
044800101130           IF sqlcod = 100 or sqlcod < 0;
044900101130             $End4 = *on;
045000101130             leave;
045100101130           ENDIF;
045200101130
045300101130         // -?Scrivo rcd WrkF?
045400101130         //  ?a rottura di trattativa?
045500101130           IF  VISnrv <> OLD_VISnrv;
045600101130             exsr sr_WrtRcd_4;
045700101130             OLD_VISnrv = VISnrv;
045800101130           ENDIF;
045900101130
046000101130         ENDDO;
046100101130
046200101130         exec sql close RCD4;
046300101130
046400101130       ENDSR;
046500101019
046600101019       //--------------------------------------------------------------
046700101019       //?Scrittura rcd 1 - Attività aperte su trattative APERTE     ?
046800101019       //--------------------------------------------------------------
046900101019       BEGSR  sr_WrtRcd_1;
047000101019
047100101019         // -?Cerco descrizione commerciale e dati unificante?
047200101019         exsr sr_DatiCmm;
047300101019
047400101019         clear  WFATT000;
047500101019         WATute  = SDSusr;
047600101019         WATdta  = *date;
047700101019         WATtrc  = '1';
047800101019         WATnrv  = VISnrv;
047900101019         WATdat  = VISdat;
048000101019         WATrag  = VISrag;
048100101019         WATcmm  = VIScmm;
048200130904         WATcmmD = AZCMMds.CMMdes;
048300130904         WATcmu  = AZCMMds.CMMuni;
048400130904         WATcmuD = AZCMMds_Uni.CMMdes;
048500150604         IF  ORGcar_uni > *zeros;
048600150604           WATara  = %int( ORGcar_uni );
048700101019         ENDIF;
048800101019         WATaraD = uni§05des;
048900101019         IF  ORGfl3_uni > *blank;
049000101019           WATdis  =  ORGfl3_uni;
049100101019         ENDIF;
049200101019         WATdisD = uni§17des;
049300101019
049400101019         //______________
049500101019         write  WFATT000;
049600101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049700101019
049800101019       ENDSR;
049900101019
050000101011       //--------------------------------------------------------------
050100101019       //?Scrittura rcd 2 - Attività aperte su trattative CHIUSE     ?
050200101011       //--------------------------------------------------------------
050300101019       BEGSR  sr_WrtRcd_2;
050400101011
050500101019         // -?Cerco descrizione commerciale e dati unificante?
050600101011         exsr sr_DatiCmm;
050700100507
050800101011         clear  WFATT000;
050900101011         WATute  = SDSusr;
051000101011         WATdta  = *date;
051100101019         WATtrc  = '2';
051200101011         WATnrv  = VISnrv;
051300101011         WATdat  = VISdat;
051400101011         WATrag  = VISrag;
051500101011         WATcmm  = VIScmm;
051600130904         WATcmmD = AZCMMds.CMMdes;
051700130904         WATcmu  = AZCMMds.CMMuni;
051800130904         WATcmuD = AZCMMds_Uni.CMMdes;
051900150604         IF  ORGcar_uni > *zeros;
052000150604           WATara  = %int( ORGcar_uni );
052100101011         ENDIF;
052200101019         WATaraD = uni§05des;
052300101019         IF  ORGfl3_uni > *blank;
052400101019           WATdis  =  ORGfl3_uni;
052500101019         ENDIF;
052600101019         WATdisD = uni§17des;
052700100510
052800100510         //______________
052900101011         write  WFATT000;
053000100510         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053100100507
053200100507       ENDSR;
053300101019
053400101019       //--------------------------------------------------------------
053500101019       //?Scrittura rcd 3 - Appuntamenti NON fatti entro 3 gg. lav.  ?
053600101019       //--------------------------------------------------------------
053700101019       BEGSR  sr_WrtRcd_3;
053800101019
053900101019         // -?Cerco descrizione commerciale e dati unificante?
054000101019         VIScmm = ATCcmm;
054100101019         exsr sr_DatiCmm;
054200101019
054300101019         clear  WFATT000;
054400101019         WATute  = SDSusr;
054500101019         WATdta  = *date;
054600101019         WATtrc  = '3';
054700101019         WATnrv  = ATCnrv;
054800101019         WATdat  = VISdat;
054900101019         WATrag  = VISrag;
055000101019         WATdad  = ATCdad;
055100101019         WATcmm  = VIScmm;
055200130904         WATcmmD = AZCMMds.CMMdes;
055300130904         WATcmu  = AZCMMds.CMMuni;
055400130904         WATcmuD = AZCMMds_Uni.CMMdes;
055500150604         IF  ORGcar_uni > *zeros;
055600150604           WATara  = %int( ORGcar_uni );
055700101019         ENDIF;
055800101019         WATaraD = uni§05des;
055900101019         IF  ORGfl3_uni > *blank;
056000101019           WATdis  =  ORGfl3_uni;
056100101019         ENDIF;
056200101019         WATdisD = uni§17des;
056300101019
056400101019         //______________
056500101019         write  WFATT000;
056600101019         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056700101019
056800101019       ENDSR;
056900101130
057000101130       //--------------------------------------------------------------
057100101130       //?Scrittura rcd 3 - Trattative aperte senza info trattativa
057200101130       //--------------------------------------------------------------
057300101130       BEGSR  sr_WrtRcd_4;
057400101130
057500101130         // -?Cerco descrizione commerciale e dati unificante?
057600101130         exsr sr_DatiCmm;
057700101130
057800101130         clear  WFATT000;
057900101130         WATute  = SDSusr;
058000101130         WATdta  = *date;
058100101130         WATtrc  = '4';
058200101130         WATnrv  = VISnrv;
058300101130         WATdat  = VISdat;
058400101130         WATrag  = VISrag;
058500101130         WATcmm  = VIScmm;
058600130904         WATcmmD = AZCMMds.CMMdes;
058700130904         WATcmu  = AZCMMds.CMMuni;
058800130904         WATcmuD = AZCMMds_Uni.CMMdes;
058900150604         IF  ORGcar_uni > *zeros;
059000150604           WATara  = %int( ORGcar_uni );
059100101130         ENDIF;
059200101130         WATaraD = uni§05des;
059300101130         IF  ORGfl3_uni > *blank;
059400101130           WATdis  =  ORGfl3_uni;
059500101130         ENDIF;
059600101130         WATdisD = uni§17des;
059700101130
059800101130         //______________
059900101130         write  WFATT000;
060000101130         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
060100101130
060200101130       ENDSR;
060300101011
060400101011       //--------------------------------------------------------------
060500101011       //?Cerco dati del commerciale
060600101011       //--------------------------------------------------------------
060700101011       BEGSR  sr_DatiCmm;
060800101011
060900101011         // -?Reperimento dati commerciale?
061000130904         IF  AZCMMds.CMMcod <> VIScmm;
061100130904           clear  AZCMMds;
061200130904           chain  (VIScmm)  AZCMM000;
061300130904           IF  %found(AZCMM01L);
061400130904             AZCMMds.CMMcod = CMMcod;
061500130904             AZCMMds.CMMdes = CMMdes;
061600130904             AZCMMds.CMMuni = CMMuni;
061700101011           ENDIF;
061800101011         ENDIF;
061900101011
062000101019         // -?Reperimento area e distretto unificante?
062100101019         IF  ORGfil_uni <>
062200130904                  %int( %subst( %editc( AZCMMds.CMMuni : 'X' ) : 1 : 3 ) );
062300101011
062400101019           ORGfil_uni =
062500130904                  %int( %subst( %editc( AZCMMds.CMMuni : 'X' ) : 1 : 3 ) );
062600150604           //clear  Og140uni;
062700101019           clear  ORGfl3_uni;
062800101019           chain  (ORGfil_uni)  AZORG;
062900101011
063000101011           IF  %found(AZORG01L);
063100101019             ORGfl3_uni = ORGfl3;
063200150604             ORGcar_uni = ORGcar;
063300101011           ENDIF;
063400101011         ENDIF;
063500101019
063600101019         // -?Decodifica unificante?
063700130904         If  AZCMMds_Uni.CMMcod <> AZCMMds.CMMuni;
063800130904           chain  (AZCMMds.CMMuni)  AZCMM000;
063900130904           if  %found(AZCMM01L);
064000130904             AZCMMds_Uni.CMMcod = CMMcod;
064100130904             AZCMMds_Uni.CMMdes = CMMdes;
064200130904             AZCMMds_Uni.CMMuni = CMMuni;
064300101019           endif;
064400101019         endif;
064500101011
064600101019         // -?Decodifica raggruppamento area unificante?
064700150604         IF  TBLkey_Uni_05 <> %editc(ORGcar_uni:'X');
064800150604           TBLkey_Uni_05 = %editc(ORGcar_uni:'X');
064900101019           clear  ds05uni;
065000101011           k_TBLcod = '05';
065100101019           k_TBLkey = TBLkey_Uni_05;
065200101011           chain  %kds(k03tabel00)  TABEL;
065300101011           IF  %found(TABEL00F)   and   TBLflg = *blank;
065400101019             ds05uni = TBLuni;
065500101011           ENDIF;
065600101011         ENDIF;
065700101019
065800101019         // -?Decodifica distretto unificante?
065900101019         IF  TBLkey_Uni_17 <> ORGfl3_uni;
066000101019           TBLkey_Uni_17 = ORGfl3_uni;
066100101019           clear  ds17uni;
066200101019           k_TBLcod = '17';
066300101019           k_TBLkey = TBLkey_Uni_17;
066400101019           chain  %kds(k03tabel00)  TABEL;
066500101019           IF  %found(TABEL00F)   and   TBLflg = *blank;
066600101019             ds17uni = TBLuni;
066700101019           ENDIF;
066800101019         ENDIF;
066900101011
067000101011       ENDSR;
067100100507
067200100507       //--------------------------------------------------------------
067300100507       //?Operazioni finali.                                           ?
067400100507       //--------------------------------------------------------------
067500100507       BEGSR  sr_RoutEnd;
067600101011
067700101011         *inLR = *on;
067800100507
067900100510         // -?Uscita dal *pgm?
068000100507         return;
068100100507
068200100507       ENDSR;
068300100507
068400100507      /end-free
