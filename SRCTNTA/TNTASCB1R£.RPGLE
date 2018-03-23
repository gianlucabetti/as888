000100160805       //==============================================================
000200160805       //?TNTAC2R * Crea WrkF per controllo attività
000300160805       //==============================================================
000400160805
000500160805       //--------------------------------------------------------------
000600160805       //?Specifiche di controllo.                                     ?
000700160805       //--------------------------------------------------------------
000800160805
000900160805     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000160805
001100160805       //--------------------------------------------------------------
001200160805       //?Dichiarazione file.                                          ?
001300160805       //--------------------------------------------------------------
001400160805
001500160805       // -?Organigramma?
001600160805     fAZORG01L  if   e           k disk
001700160805
001800160805       // -?Tabelle?
001900160805     fTABEL00F  if   e           k disk
002000160805
002100160805       // -?Anagrafica Commerciali?
002200160805     fAZCMM01L  if   e           k disk
002300160805
002400160805       // -?Elenco trattative
002500160805     fWFATT00F  o    e             disk    usropn
002600160805     f                                     extfile(wFLib)
002700160805
002800160805       //--------------------------------------------------------------
002900160805       //?Definizione costanti.                                        ?
003000160805       //--------------------------------------------------------------
003100160805
003200160805       //--------------------------------------------------------------
003300160805       //?Definizione schiere.                                         ?
003400160805       //--------------------------------------------------------------
003500160805
003600160805       //--------------------------------------------------------------
003700160805       //?Definizione aree dati.                                       ?
003800160805       //--------------------------------------------------------------
003900160805
004000160805       // -?Dati utente?
004100160805     d §AzUte        e ds                  extname(AZUTE00F)
004200160805     d                                     dtaara
004300160805     d §DatiUte      e ds                  extname(dDatiUte)
004400160805     d                                     dtaara
004500160805
004600160805       //--------------------------------------------------------------
004700160805       //?Definizione strutture dati.                                  ?
004800160805       //--------------------------------------------------------------
004900160805
005000160805       // -?Status ds?
005100160805     d Status         sds
005200160805     d  SDSpgm           *proc
005300160805     d  SDSusr               254    263
005400160805
005500160805       // -?Parametri ricevuti
005600160805     d KPJBA         e ds
005700160805
005800160805       // -?Parametri per Reperimento dati utente?
005900160805     d TIBS34ds      e ds
006000160805
006100160805       // -?Salvataggio dati del Comm.le?
006200160805     d AZCMMds       e ds                  extname(AZCMM00F)
006300160805     d                                     inz  qualified
006400160805       // -?Salvataggio dati del Comm.le Unificante?
006500160805     d AZCMMds_Uni   e ds                  extname(AZCMM00F)
006600160805     d                                     inz  qualified
006700160805
006800160805       // -?Tabella "05" = Codici Area commerciale unificante
006900160805     d ds05uni       e ds                  extname(ds05)
007000160805     d                                     inz  prefix(uni)
007100160805
007200160805       // -?Tabella "17" = Codici Distretto commerciale unificante
007300160805     d ds17uni       e ds                  extname(ds17)
007400160805     d                                     inz  prefix(uni)
007500160805
007600160805       // -?File attività
007700160805     d TIATC00F      e ds                  extname(TIATC00F)
007800160805
007900160805       // -?File trattative
008000160805     d TIVIS00F      e ds                  extname(TIVIS00F)
008100160805
008200160805       // -?Controllo validità delle date
008300160805     d Xgiolavds     e ds                  inz
008400160805
008500160805       //--------------------------------------------------------------
008600160805       //?Definizione variabili globali.                               ?
008700160805       //--------------------------------------------------------------
008800160805
008900160805       // -?Flags booleani
009000160805     d $End1           s               n   inz(*off)
009100160805     d $End2           s               n   inz(*off)
009200160805     d $End3           s               n   inz(*off)
009300160805     d $End4           s               n   inz(*off)
009400160805     d $Fine           s               n   inz(*off)
009500160805
009600160805       // -?Nome libreria del work-file WFATT00F?
009700160805     d wFLib           s             21    inz
009800160805
009900160805       // -?Campi per richiamo QCMDEXC
010000160805     d Qcmd            s            128    inz
010100160805
010200160805       // -?Codice area del commerciale unificante?
010300160805     d ORGcar_uni      s                   like(ORGcar)  inz
010400160805       // -?Codice filiale del commerciale unificante?
010500160805     d ORGfil_uni      s                   like(ORGfil)  inz
010600160805       // -?Codice distretto commerciale unificante?
010700160805     d ORGfl3_uni      s                   like(ORGfl3)  inz
010800160805       // -?Codice area del commerciale unificante?
010900160805     d TBLkey_Uni_05   s                   like(TBLkey)  inz
011000160805       // -?Codice distretto del commerciale unificante
011100160805     d TBLkey_Uni_17   s                   like(TBLkey)  inz
011200160805
011300160805       // -?Stringa SQL da eseguire
011400160805     d wSQL            s           2048    Varying        inz
011500160805
011600160805       // -?Data corrente?
011700160805     d wDate           s              8s 0 inz
011800160805
011900160805       // -?Campo di comodo
012000160805     d OLD_VISnrv      s                   like(VISnrv)
012100160805
012200160805       //--------------------------------------------------------------
012300160805       //?Definizione prototipi procedure e relativi parametri.        ?
012400160805       //--------------------------------------------------------------
012500160805
012600160805       // -?Aggiunge/Toglie gg/mm dalla data
012700160805     d Xgiolav         pr                  extpgm('XGIOLAV')
012800160805     d  xgiolavds                          likeds(xgiolavds)
012900160805
013000160805       // -?Esecuzione comando di sistema
013100160805      /copy gaitrasrc/srcProtoPR,QCMDEXC
013200160805
013300160805       // -?Reperimento dati utente?
013400160805      /copy gaitrasrc/srcProtoPR,TIBS34R
013500160805
013600160805       //--------------------------------------------------------------
013700160805       //?Definizione key-list.                                        ?
013800160805       //--------------------------------------------------------------
013900160805
014000160805       // -?File TABEL00F?
014100160805     d k03tabel00    e ds                  extname(TABEL00F : *key)
014200160805     d                                     inz  prefix(k_)
014300160805
014400160805       //--------------------------------------------------------------
014500160805       //?M A I N - L I N E                                            ?
014600160805       //--------------------------------------------------------------
014700160805
014800160805     c     *Entry        plist
014900160805     c                   parm                    KPJBA
015000160805
015100160805      /free
015200160805
015300160805       // -?Operazioni iniziali?
015400160805       exsr sr_RoutInz;
015500160805
015600160805       // -?Elabora?
015700160805       exsr Elabora;
015800160805
015900160805       // -?Operazioni finali?
016000160805       exsr sr_RoutEnd;
016100160805
016200160805       //--------------------------------------------------------------
016300160805       //?Operazioni iniziali.                                         ?
016400160805       //--------------------------------------------------------------
016500160805       BEGSR  sr_RoutInz;
016600160805
016700160805         // -?Reperimento dati job?
016800160805         exsr  sr_DatiJob;
016900160805
017000160805         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
017100160805         wDate = %dec(%date());
017200160805
017300160805         // -?Impostazione campi fissi?
017400160805         k_TBLkut = 1;
017500160805
017600160805         exec sql set option dynusrprf = *owner,
017700160805                             closqlcsr = *endmod;
017800160805
017900160805       ENDSR;
018000160805
018100160805       //--------------------------------------------------------------
018200160805       //?Reperimento Dati del job (Utente/Operativi).                 ?
018300160805       //--------------------------------------------------------------
018400160805       BEGSR  sr_DatiJob;
018500160805
018600160805         in(E) §AzUte;
018700160805         if NOT %error;
018800160805           in(E) §DatiUte;
018900160805         endif;
019000160805         if %error or RSut = *blanks;
019100160805           clear TIBS34ds;
019200160805           tibs34r ( tibs34ds );
019300160805           in §AzUte;
019400160805           in §DatiUte;
019500160805         endif;
019600160805
019700160805       ENDSR;
019800160805
019900160805       //--------------------------------------------------------------
020000160805       //?Elaborazione.
020100160805       //--------------------------------------------------------------
020200160805       BEGSR  Elabora;
020300160805
020400160805       // -?Pulizia ed apertura WrkF?
020500160805         Qcmd = 'CLRPFM FILE(';
020600160805         IF  %subst(knsif : 7 : 1) = 'P';
020700160805           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
020800160805           wFLib = 'GAITRAAZP/WFATT00F';
020900160805         ELSE;
021000160805           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
021100160805           wFLib = 'GAITRAAZM/WFATT00F';
021200160805         ENDIF;
021300160805         Qcmd = %trimr(Qcmd) + '/WFATT00F)';
021400160805         ExecuteCommand ( Qcmd : %size(Qcmd));
021500160805         open  WFATT00F;
021600160805
021700160805       // -?Elaboro trattative aperte senza attività?
021800160805         exsr sr_ElaRcd_1;
021900160805
022000160805       // -?Elaboro attività aperte su trattative chiuse?
022100160805         exsr sr_ElaRcd_2;
022200160805
022300160805       // -?Elaboro appuntamenti non eseguiti entro 3 gg.lav.?
022400160805         exsr sr_ElaRcd_3;
022500160805
022600160805       // -?Elaboro trattative aperte senza info trattative?
022700160805         exsr sr_ElaRcd_4;
022800160805
022900160805         $fine = *on;
023000160805
023100160805       ENDSR;
023200160805
023300160805       //--------------------------------------------------------------
023400160805       //?Elaborazione rcd 1 - Trattative APERTE senza attività
023500160805       //--------------------------------------------------------------
023600160805       BEGSR  sr_ElaRcd_1;
023700160805
023800160805         wSQL = 'with ATT as (select ATCnrv from TIATC00F +
023900160805                 where ATCdco = 0 and ATCnrv > 0) +
024000160805                 select VIScmm, VISnrv, VISrag, VISdat from ';
024100160805         wSQL += 'TIVIS00F ';
024200160805         wSQL += 'exception join ATT on VISnrv = ATCnrv +
024300160805                  where VISffz = '' '' and VISdch = 0 +
024400160805                  order by VIScmm, VISnrv';
024500160805
024600160805         // -?Cerco trattative APERTE senza attività?
024700160805         exec sql
024800160805           prepare S1 from :wSQL;
024900160805         exec sql
025000160805          declare RCD1 cursor for S1;
025100160805
025200160805         exec sql
025300160805          open RCD1;
025400160805          IF sqlcode < 0;
025500160805            $End1 = *on;
025600160805          ENDIF;
025700160805
025800160805         DOW not $End1;
025900160805          exec sql
026000160805           fetch next from RCD1 into :VIScmm, :VISnrv,
026100160805                                     :VISrag, :VISdat;
026200160805           IF sqlcod = 100 or sqlcod < 0;
026300160805             $End1 = *on;
026400160805             leave;
026500160805           ENDIF;
026600160805
026700160805         // -?Scrivo rcd WrkF?
026800160805           exsr sr_WrtRcd_1;
026900160805
027000160805         ENDDO;
027100160805
027200160805         exec sql close RCD1;
027300160805
027400160805       ENDSR;
027500160805
027600160805       //--------------------------------------------------------------
027700160805       //?Elaborazione rcd 2 - Attività aperte su trattative CHIUSE     ?
027800160805       //--------------------------------------------------------------
027900160805       BEGSR  sr_ElaRcd_2;
028000160805
028100160805         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
028200160805                 from TIATC00F join ';
028300160805         wSQL += 'TIVIS00F ';
028400160805         wSQL += 'on ATCnrv = VISnrv where ATCdco = 0 and +
028500160805                  ATCnrv > 0 and VISdch > 0 +
028600160805                  order by VIScmm, VISnrv';
028700160805
028800160805         // -?Cerco attività aperte su trattative CHIUSE?
028900160805         exec sql
029000160805           prepare S2 from :wSQL;
029100160805         exec sql
029200160805          declare RCD2 cursor for S2;
029300160805
029400160805         exec sql
029500160805          open RCD2;
029600160805          IF sqlcode < 0;
029700160805            $End2 = *on;
029800160805          ENDIF;
029900160805
030000160805         DOW not $End2;
030100160805          exec sql
030200160805           fetch next from RCD2 into :VIScmm, :VISnrv,
030300160805                                     :VISrag, :VISdat;
030400160805           IF sqlcod = 100 or sqlcod < 0;
030500160805             $End2 = *on;
030600160805             leave;
030700160805           ENDIF;
030800160805
030900160805         // -?Scrivo rcd WrkF?
031000160805           exsr sr_WrtRcd_2;
031100160805
031200160805         ENDDO;
031300160805
031400160805         exec sql close RCD2;
031500160805
031600160805       ENDSR;
031700160805
031800160805       //--------------------------------------------------------------
031900160805       //?Elaborazione rcd 3 - Appuntamento non esitati da oltre 3 gg.  ?
032000160805       //--------------------------------------------------------------
032100160805       BEGSR  sr_ElaRcd_3;
032200160805
032300160805         wSQL = 'select ATCcmm, ATCnrv, ATCdad, VISrag, VISdat +
032400160805                 from TIATC00F join ';
032500160805         wSQL += 'TIVIS00F ';
032600160805         wSQL += 'on VISnrv = ATCnrv +
032700160805                  where ATCcad = ''91'' and ATCdco = 0 +
032800160805                  and ATCdad < ';
032900160805         wSQL += %editc(wdate:'X');
033000160805         wSQL += ' order by ATCcmm, ATCnrv';
033100160805
033200160805         // -?Cerco appuntamenti NON fatti?
033300160805         exec sql
033400160805           prepare S3 from :wSQL;
033500160805         exec sql
033600160805          declare RCD3 cursor for S3;
033700160805
033800160805         exec sql
033900160805          open RCD3;
034000160805          IF sqlcode < 0;
034100160805            $End3 = *on;
034200160805          ENDIF;
034300160805
034400160805         DOW not $End3;
034500160805          exec sql
034600160805           fetch next from RCD3 into :ATCcmm, :ATCnrv,
034700160805                                     :ATCdad, :VISrag,
034800160805                                     :VISdat;
034900160805           IF sqlcod = 100 or sqlcod < 0;
035000160805             $End3 = *on;
035100160805             leave;
035200160805           ENDIF;
035300160805
035400160805         // -?Controllo se sono passati 3 gg lavorativi?
035500160805           clear XGIOLAVDS;
035600160805           IXGLdata = wdate;
035700160805           IXGLsub  = 'S';
035800160805           IXGLgg   = 3;
035900160805           IXGLpa   = 'P';
036000160805           IXGLlav  = 'S';
036100160805           IXGLfil = %dec(%subst(%editc(ATCcmm:'X'):1:3):3:0);
036200160805           XGIOLAV (xgiolavds);
036300160805           IF  ATCdad < OXGLdata;
036400160805
036500160805           // -?Scrivo rcd WrkF?
036600160805             exsr sr_WrtRcd_3;
036700160805           ENDIF;
036800160805
036900160805         ENDDO;
037000160805
037100160805         exec sql close RCD3;
037200160805       ENDSR;
037300160805
037400160805       //--------------------------------------------------------------
037500160805       //?Elaborazione rcd - Trattative APERTE senza info trattative
037600160805       //--------------------------------------------------------------
037700160805       BEGSR  sr_ElaRcd_4;
037800160805
037900160805         wSQL = 'select VIScmm, VISnrv, VISrag, VISdat +
038000160805                 from ';
038100160805         wSQL += 'TIVIS00F ';
038200160805         wSQL += 'join ';
038300160805
038400160805         IF  %subst(knsif : 7 : 1) = 'P';
038500160805           wSQL += 'FILTRAGRPF/TNOFM00F ';
038600160805         ELSE;
038700160805           wSQL += 'FILTRAGRU/TNOFM00F ';
038800160805         ENDIF;
038900160805
039000160805         wSQL += 'on VISnrv = TAMksc ';
039100160805
039200160805         wSQL += 'where VISdch = 0 and +
039300160805                  VISffz = '' '' and VISinfot = '' '' +
039400160805                  and VIStpv <> ''R'' +
039500160805                  order by VIScmm, VISnrv';
039600160805
039700160805         // -?Cerco attività aperte su trattative CHIUSE?
039800160805         exec sql
039900160805           prepare S4 from :wSQL;
040000160805         exec sql
040100160805          declare RCD4 cursor for S4;
040200160805
040300160805         exec sql
040400160805          open RCD4;
040500160805          IF sqlcode < 0;
040600160805            $End4 = *on;
040700160805          ENDIF;
040800160805
040900160805         DOW not $End4;
041000160805          exec sql
041100160805           fetch next from RCD4 into :VIScmm, :VISnrv,
041200160805                                    :VISrag, :VISdat;
041300160805           IF sqlcod = 100 or sqlcod < 0;
041400160805             $End4 = *on;
041500160805             leave;
041600160805           ENDIF;
041700160805
041800160805         // -?Scrivo rcd WrkF?
041900160805         //  ?a rottura di trattativa?
042000160805           IF  VISnrv <> OLD_VISnrv;
042100160805             exsr sr_WrtRcd_4;
042200160805             OLD_VISnrv = VISnrv;
042300160805           ENDIF;
042400160805
042500160805         ENDDO;
042600160805
042700160805         exec sql close RCD4;
042800160805
042900160805       ENDSR;
043000160805
043100160805       //--------------------------------------------------------------
043200160805       //?Scrittura rcd 1 - Attività aperte su trattative APERTE     ?
043300160805       //--------------------------------------------------------------
043400160805       BEGSR  sr_WrtRcd_1;
043500160805
043600160805         // -?Cerco descrizione commerciale e dati unificante?
043700160805         exsr sr_DatiCmm;
043800160805
043900160805         clear  WFATT000;
044000160805         WATute  = SDSusr;
044100160805         WATdta  = *date;
044200160805         WATtrc  = '1';
044300160805         WATnrv  = VISnrv;
044400160805         WATdat  = VISdat;
044500160805         WATrag  = VISrag;
044600160805         WATcmm  = VIScmm;
044700160805         WATcmmD = AZCMMds.CMMdes;
044800160805         WATcmu  = AZCMMds.CMMuni;
044900160805         WATcmuD = AZCMMds_Uni.CMMdes;
045000160805         IF  ORGcar_uni > *zeros;
045100160805           WATara  = %int( ORGcar_uni );
045200160805         ENDIF;
045300160805         WATaraD = uni§05des;
045400160805         IF  ORGfl3_uni > *blank;
045500160805           WATdis  =  ORGfl3_uni;
045600160805         ENDIF;
045700160805         WATdisD = uni§17des;
045800160805
045900160805         //______________
046000160805         write  WFATT000;
046100160805         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046200160805
046300160805       ENDSR;
046400160805
046500160805       //--------------------------------------------------------------
046600160805       //?Scrittura rcd 2 - Attività aperte su trattative CHIUSE     ?
046700160805       //--------------------------------------------------------------
046800160805       BEGSR  sr_WrtRcd_2;
046900160805
047000160805         // -?Cerco descrizione commerciale e dati unificante?
047100160805         exsr sr_DatiCmm;
047200160805
047300160805         clear  WFATT000;
047400160805         WATute  = SDSusr;
047500160805         WATdta  = *date;
047600160805         WATtrc  = '2';
047700160805         WATnrv  = VISnrv;
047800160805         WATdat  = VISdat;
047900160805         WATrag  = VISrag;
048000160805         WATcmm  = VIScmm;
048100160805         WATcmmD = AZCMMds.CMMdes;
048200160805         WATcmu  = AZCMMds.CMMuni;
048300160805         WATcmuD = AZCMMds_Uni.CMMdes;
048400160805         IF  ORGcar_uni > *zeros;
048500160805           WATara  = %int( ORGcar_uni );
048600160805         ENDIF;
048700160805         WATaraD = uni§05des;
048800160805         IF  ORGfl3_uni > *blank;
048900160805           WATdis  =  ORGfl3_uni;
049000160805         ENDIF;
049100160805         WATdisD = uni§17des;
049200160805
049300160805         //______________
049400160805         write  WFATT000;
049500160805         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049600160805
049700160805       ENDSR;
049800160805
049900160805       //--------------------------------------------------------------
050000160805       //?Scrittura rcd 3 - Appuntamenti NON fatti entro 3 gg. lav.  ?
050100160805       //--------------------------------------------------------------
050200160805       BEGSR  sr_WrtRcd_3;
050300160805
050400160805         // -?Cerco descrizione commerciale e dati unificante?
050500160805         VIScmm = ATCcmm;
050600160805         exsr sr_DatiCmm;
050700160805
050800160805         clear  WFATT000;
050900160805         WATute  = SDSusr;
051000160805         WATdta  = *date;
051100160805         WATtrc  = '3';
051200160805         WATnrv  = ATCnrv;
051300160805         WATdat  = VISdat;
051400160805         WATrag  = VISrag;
051500160805         WATdad  = ATCdad;
051600160805         WATcmm  = VIScmm;
051700160805         WATcmmD = AZCMMds.CMMdes;
051800160805         WATcmu  = AZCMMds.CMMuni;
051900160805         WATcmuD = AZCMMds_Uni.CMMdes;
052000160805         IF  ORGcar_uni > *zeros;
052100160805           WATara  = %int( ORGcar_uni );
052200160805         ENDIF;
052300160805         WATaraD = uni§05des;
052400160805         IF  ORGfl3_uni > *blank;
052500160805           WATdis  =  ORGfl3_uni;
052600160805         ENDIF;
052700160805         WATdisD = uni§17des;
052800160805
052900160805         //______________
053000160805         write  WFATT000;
053100160805         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053200160805
053300160805       ENDSR;
053400160805
053500160805       //--------------------------------------------------------------
053600160805       //?Scrittura rcd 3 - Trattative aperte senza info trattativa
053700160805       //--------------------------------------------------------------
053800160805       BEGSR  sr_WrtRcd_4;
053900160805
054000160805         // -?Cerco descrizione commerciale e dati unificante?
054100160805         exsr sr_DatiCmm;
054200160805
054300160805         clear  WFATT000;
054400160805         WATute  = SDSusr;
054500160805         WATdta  = *date;
054600160805         WATtrc  = '4';
054700160805         WATnrv  = VISnrv;
054800160805         WATdat  = VISdat;
054900160805         WATrag  = VISrag;
055000160805         WATcmm  = VIScmm;
055100160805         WATcmmD = AZCMMds.CMMdes;
055200160805         WATcmu  = AZCMMds.CMMuni;
055300160805         WATcmuD = AZCMMds_Uni.CMMdes;
055400160805         IF  ORGcar_uni > *zeros;
055500160805           WATara  = %int( ORGcar_uni );
055600160805         ENDIF;
055700160805         WATaraD = uni§05des;
055800160805         IF  ORGfl3_uni > *blank;
055900160805           WATdis  =  ORGfl3_uni;
056000160805         ENDIF;
056100160805         WATdisD = uni§17des;
056200160805
056300160805         //______________
056400160805         write  WFATT000;
056500160805         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056600160805
056700160805       ENDSR;
056800160805
056900160805       //--------------------------------------------------------------
057000160805       //?Cerco dati del commerciale
057100160805       //--------------------------------------------------------------
057200160805       BEGSR  sr_DatiCmm;
057300160805
057400160805         // -?Reperimento dati commerciale?
057500160805         IF  AZCMMds.CMMcod <> VIScmm;
057600160805           clear  AZCMMds;
057700160805           chain  (VIScmm)  AZCMM000;
057800160805           IF  %found(AZCMM01L);
057900160805             AZCMMds.CMMcod = CMMcod;
058000160805             AZCMMds.CMMdes = CMMdes;
058100160805             AZCMMds.CMMuni = CMMuni;
058200160805           ENDIF;
058300160805         ENDIF;
058400160805
058500160805         // -?Reperimento area e distretto unificante?
058600160805         IF  ORGfil_uni <>
058700160805                  %int( %subst( %editc( AZCMMds.CMMuni : 'X' ) : 1 : 3 ) );
058800160805
058900160805           ORGfil_uni =
059000160805                  %int( %subst( %editc( AZCMMds.CMMuni : 'X' ) : 1 : 3 ) );
059100160805           //clear  Og140uni;
059200160805           clear  ORGfl3_uni;
059300160805           chain  (ORGfil_uni)  AZORG;
059400160805
059500160805           IF  %found(AZORG01L);
059600160805             ORGfl3_uni = ORGfl3;
059700160805             ORGcar_uni = ORGcar;
059800160805           ENDIF;
059900160805         ENDIF;
060000160805
060100160805         // -?Decodifica unificante?
060200160805         If  AZCMMds_Uni.CMMcod <> AZCMMds.CMMuni;
060300160805           chain  (AZCMMds.CMMuni)  AZCMM000;
060400160805           if  %found(AZCMM01L);
060500160805             AZCMMds_Uni.CMMcod = CMMcod;
060600160805             AZCMMds_Uni.CMMdes = CMMdes;
060700160805             AZCMMds_Uni.CMMuni = CMMuni;
060800160805           endif;
060900160805         endif;
061000160805
061100160805         // -?Decodifica raggruppamento area unificante?
061200160805         IF  TBLkey_Uni_05 <> %editc(ORGcar_uni:'X');
061300160805           TBLkey_Uni_05 = %editc(ORGcar_uni:'X');
061400160805           clear  ds05uni;
061500160805           k_TBLcod = '05';
061600160805           k_TBLkey = TBLkey_Uni_05;
061700160805           chain  %kds(k03tabel00)  TABEL;
061800160805           IF  %found(TABEL00F)   and   TBLflg = *blank;
061900160805             ds05uni = TBLuni;
062000160805           ENDIF;
062100160805         ENDIF;
062200160805
062300160805         // -?Decodifica distretto unificante?
062400160805         IF  TBLkey_Uni_17 <> ORGfl3_uni;
062500160805           TBLkey_Uni_17 = ORGfl3_uni;
062600160805           clear  ds17uni;
062700160805           k_TBLcod = '17';
062800160805           k_TBLkey = TBLkey_Uni_17;
062900160805           chain  %kds(k03tabel00)  TABEL;
063000160805           IF  %found(TABEL00F)   and   TBLflg = *blank;
063100160805             ds17uni = TBLuni;
063200160805           ENDIF;
063300160805         ENDIF;
063400160805
063500160805       ENDSR;
063600160805
063700160805       //--------------------------------------------------------------
063800160805       //?Operazioni finali.                                           ?
063900160805       //--------------------------------------------------------------
064000160805       BEGSR  sr_RoutEnd;
064100160805
064200160805         *inLR = *on;
064300160805
064400160805         // -?Uscita dal *pgm?
064500160805         return;
064600160805
064700160805       ENDSR;
064800160805
064900160805      /end-free
