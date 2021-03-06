000100091228       //==============================================================
000200160809       // Verifica abilitazione cliente/tipo VAS
000300091228       //==============================================================
000400091228
000500091228
000600160202     **
000700160202     ** ISTRUZIONI PER LA COMPILAZIONE
000800160202     **
000900160809     ** 1. Creare il modulo UBKSUTIPR (Opz 15 PDM)
001000160809     ** 2. Creare/Aggiornare il programma di servizio UBKSUTIPR (CRTSRVPGM / UPDSRVPGM)
001100160601     **    con ACTGRP(*caller), la BNDDIR non � importante tanto non chiama niente
001200160809     ** 3. Creare/Aggiornare il programma UBKSUTIPR (CRTPGM / UPDPGM)
001300160601     **    con ACTGRP(*caller), la BNDDIR non � importante tanto non chiama niente
001400160202     ** 4. Cancellare il modulo.
001500160202     **
001600091228
001700091228       //--------------------------------------------------------------
001800121106       // Specifiche di controllo.
001900091228       //--------------------------------------------------------------
002000091223
002100091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200160601     h alwnull(*inputonly)
002300091223
002400091223       //--------------------------------------------------------------
002500121106       // Dichiarazione file.
002600091223       //--------------------------------------------------------------
002700160218
002800160531
002900091223       //--------------------------------------------------------------
003000121106       // Definizione costanti.
003100091223       //--------------------------------------------------------------
003200100302
003300091223
003400091223       //--------------------------------------------------------------
003500121106       // Definizione schiere.
003600091223       //--------------------------------------------------------------
003700091223
003800091223
003900091223       //--------------------------------------------------------------
004000121106       // Definizione aree dati.
004100091223       //--------------------------------------------------------------
004200091223
004300091223
004400140724       //--------------------------------------------------------------
004500140724       // Definizione strutture dati.
004600140724       //--------------------------------------------------------------
004700140724
004800121106       // - Status
004900160802     D Psds           sds
005000160802     D   SDSpgm          *proc
005100160802     D   JobName             244    253                                         Job name
005200160802     D   JobUser             254    263                                         User name
005300160802     D   JobNumber           264    269s 0                                      Job number
005400091223
005500160802
005600091223       //--------------------------------------------------------------
005700121106       // Definizione variabili.
005800091223       //--------------------------------------------------------------
005900091223
006000160802       // - Parametri di input
006100160809     D pIn_KSU         s              8a
006200160809     D pIn_TIP         s              2a
006300160804     D pIn_LANG        s              2a
006400160802
006500160802       // - Parametri di output
006600160802     D pOut_Esito      s              1s 0                                      0=Err 1=OK 2=KO
006700160809     D pOut_PGT        s             10a
006800160809     D pOut_PPT        s             50a
006900160804     D pOut_Msg        s            128a   varying
007000121106
007100121106       // Campi di comodo
007200160802     D wSQL            s            512a   inz  varying
007300160802     D wCount          s              1s 0 inz
007400160805     D wApi            c                   const('''')
007500091223
007600160802
007700091223       //--------------------------------------------------------------
007800160603       // Definizione costanti.
007900091223       //--------------------------------------------------------------
008000130307
008100160531
008200160603       //--------------------------------------------------------------
008300160603       // Definizione prototipi procedure.
008400160603       //--------------------------------------------------------------
008500160603
008600160603
008700091223       //--------------------------------------------------------------
008800121106       // Definizione key-list.
008900091223       //--------------------------------------------------------------
009000091223
009100091223
009200091223       //--------------------------------------------------------------
009300121107       // Definizione parametri procedura.
009400091223       //--------------------------------------------------------------
009500091223
009600091223     c     *Entry        plist
009700160809     c                   parm                    pIn_KSU
009800160809     c                   parm                    pIn_TIP
009900160804     c                   parm                    pIn_LANG
010000160802     c                   parm                    pOut_Esito
010100160809     c                   parm                    pOut_PGT
010200160809     c                   parm                    pOut_PPT
010300160804     c                   parm                    pOut_Msg
010400091223
010500130307      /free
010600091223
010700091223       //--------------------------------------------------------------
010800121106       // M A I N - L I N E
010900091223       //--------------------------------------------------------------
011000140806
011100160805       // Monitorizzo tutto (per necessit� massima solidit� stack chiamate)
011200160805       Monitor;
011300160805
011400130307       // Operazioni iniziali?
011500130307       exsr sr_RoutInz;
011600130307
011700160809       // Eseguo verifica abilitazione
011800160809       exsr ChkKsuTip;
011900091223
012000160804       // Operazioni finali
012100160804       exsr sr_RoutEnd;
012200160804
012300160805       // Fine monitoring
012400160805       On-Error;
012500160805       Endmon;
012600160805
012700160802       // Chiusura modulo
012800160802       *inlr = *on;
012900160802
013000160802
013100160601       //--------------------------------------------------------------
013200160601       //
013300160601       //--------------------------------------------------------------
013400160601       BEGSR  *inzsr;
013500160601
013600160601         // Inizializzazioni parametri esecuzione comandi sql
013700160802         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013800160601
013900160601       ENDSR;
014000091223
014100160802
014200091223       //--------------------------------------------------------------
014300121107       // Operazioni iniziali.
014400091223       //--------------------------------------------------------------
014500091223       BEGSR  sr_RoutInz;
014600160601
014700160802         // Inizializzo l'esito a errore
014800160802         pOut_Esito = *zeros;
014900160608
015000160608         // Controllo i parametri di input
015100160608         exsr sr_CtrlPIn;
015200160202
015300160202       ENDSR;
015400160202
015500160804
015600160804       //--------------------------------------------------------------
015700160804       // Operazioni finali.
015800160804       //--------------------------------------------------------------
015900160804       BEGSR  sr_RoutEnd;
016000160804
016100160804         // Imposto il messaggio di esito in lingua
016200160804         select;
016300160804            when pIn_LANG = 'IT';
016400160804                 select;
016500160804                    when pOut_Esito = *zeros;
016600160804                       pOut_Msg = 'Errore, verificare e ripetere la richiesta.';
016700160804                    when pOut_Esito = 1;
016800160809                       pOut_Msg = 'Abilitazione attiva.';
016900160804                    when pOut_Esito = 2;
017000160809                       pOut_Msg = 'No abilitazione.';
017100160804                 endsl;
017200160804
017300160804            when pIn_LANG = 'EN';
017400160804                 select;
017500160804                    when pOut_Esito = *zeros;
017600160804                       pOut_Msg = 'Error, check and submit request again.';
017700160804                    when pOut_Esito = 1;
017800160809                       pOut_Msg = 'Enabled.';
017900160804                    when pOut_Esito = 2;
018000160809                       pOut_Msg = 'Disabled.';
018100160804                 endsl;
018200160804         endsl;
018300160804
018400160804       ENDSR;
018500160802
018600160804
018700160202       //--------------------------------------------------------------
018800160202       // Controllo i parametri di input
018900160202       //--------------------------------------------------------------
019000160202       BEGSR  sr_CtrlPIn;
019100160804
019200160804         // Maiuscolizzo il parametro relatovo alla lingua
019300160804         select;
019400160804           when pIn_LANG = *blanks OR
019500160804                pIn_LANG = 'it'    OR
019600160804                pIn_LANG = 'It'    OR
019700160804                pIn_LANG = 'iT';
019800160804                pIN_LANG = 'IT';
019900160804           when pIn_LANG = 'en'    OR
020000160804                pIn_LANG = 'En'    OR
020100160804                pIn_LANG = 'eN';
020200160804                pIN_LANG = 'EN';
020300160804           other;
020400160804                pIN_LANG = 'EN';
020500160804         endsl;
020600091223
020700091223       ENDSR;
020800121106
020900160802
021000091223       //--------------------------------------------------------------
021100160217       // Esecuzione
021200091223       //--------------------------------------------------------------
021300160809       BEGSR  ChkKsuTip;
021400160531
021500160809         wSQL = 'select count(*), min(VIRPGT), min(VIRPPT) from tivir00f +
021600160809                 where +
021700160809                 VIRKSC = ' + wApi + pIn_KSU + wApi + ' and +
021800160809                 VIRTIP = ' + wApi + pIn_TIP + wApi + ' and +
021900160809                 (select DC_YYMD_DEC from DATECNV0F +
022000160809                  where DC_DATE = curdate()) +
022100160809                 between VIRDTI and VIRDTF +
022200160809                 for read only';
022300160802
022400160531         // Dichiarazione cursore
022500160802         exec sql   prepare S0   from :wSQL;
022600160531         exec sql   declare C0   cursor   for S0;
022700160802
022800160531         // Apro il cursore
022900160531         exec sql open C0;
023000160802
023100160531         // Leggo i dati
023200160531         exec sql
023300160809             fetch next from C0 into :wCount, :pOut_PGT, :pOut_PPT;
023400160802
023500160802         // Se 1 record trovato => login riuscito
023600160802         if sqlCode = 0;
023700160802            if wCount  = 1;
023800160802               pOut_Esito = 1;
023900160802            else;
024000160802               pOut_Esito = 2;
024100160802            endif;
024200160802         endif;
024300160802
024400160802         if pOut_Esito <> 1;
024500160809            clear pOut_PGT;
024600160809            clear pOut_PPT;
024700160802         endif;
024800160531
024900160531         // Quindi chiuso il cursore
025000160531         exec sql close C0;
025100160217
025200160217       ENDSR;
025300091223
025400160802
025500091223      /end-free
