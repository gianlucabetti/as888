000100140306       //--------------------------------------------------------------
000200140306       //?Specifiche di controllo.                                     ?
000300140306       //--------------------------------------------------------------
000400140314     h DFTACTGRP(*NO) ACTGRP(*CALLER)
000500140306
000600140306       //--------------------------------------------------------------
000700140306       //?Dichiarazione file.                                          ?
000800140306       //--------------------------------------------------------------
000900140710       // -?Video?
001000160301     fTIO7G31TSDcf   e             workstn
001100140710     f                                     indds(IndDspF)
001200140710     f                                     infds(InfDspF)
001300140306
001400140306       //--------------------------------------------------------------
001500140306       //?Definizione costanti.                                        ?
001600140306       //--------------------------------------------------------------
001700160301      /copy gaitrasrc/srcconst,TIO7G31R
001800160314      /copy gaitrasrc/srcconst,FIORC00R
001900140710
002000140710       // -?Tasti funzionali a video?
002100140710     d c_F01           c                   const(x'31')
002200140710     d c_F02           c                   const(x'32')
002300140710     d c_F03           c                   const(x'33')
002400140710     d c_F04           c                   const(x'34')
002500140710     d c_F05           c                   const(x'35')
002600140710     d c_F06           c                   const(x'36')
002700140710     d c_F07           c                   const(x'37')
002800140710     d c_F08           c                   const(x'38')
002900140710     d c_F09           c                   const(x'39')
003000140710     d c_F10           c                   const(x'3A')
003100140710     d c_F11           c                   const(x'3B')
003200140710     d c_F12           c                   const(x'3C')
003300140710     d c_F13           c                   const(x'B1')
003400140710     d c_F14           c                   const(x'B2')
003500140710     d c_F15           c                   const(x'B3')
003600140710     d c_F16           c                   const(x'B4')
003700140710     d c_F17           c                   const(x'B5')
003800140710     d c_F18           c                   const(x'B6')
003900140710     d c_F19           c                   const(x'B7')
004000140710     d c_F20           c                   const(x'B8')
004100140710     d c_F21           c                   const(x'B9')
004200140710     d c_F22           c                   const(x'BA')
004300140710     d c_F23           c                   const(x'BB')
004400140710     d c_F24           c                   const(x'BC')
004500140710     d c_Enter         c                   const(x'F1')
004600140710     d c_RollDown      c                   const(x'F4')
004700140710     d c_RollUp        c                   const(x'F5')
004800140306
004900140306       //--------------------------------------------------------------
005000140306       //?Definizione strutture dati.                                  ?
005100140306       //--------------------------------------------------------------
005200140710       // -?InfDS?
005300140710     d InfDspF         ds
005400140710     d  dsp_aid              369    369a
005500140710
005600140710       // -?Indicatori su DspF?
005700140710     d IndDspF         ds
005800140710
005900140306       // -?Dati INPUT
006000160301     d TIO1G3IDS     e ds                  QUALIFIED INZ(*EXTDFT)
006100160302     d  lang         e                     INZ(TIO7G31_ID_LINGUA_IT)
006200140306
006300140306       // -?Dati OUTPUT
006400160301     d TIO1G3ODS     e ds                  QUALIFIED INZ(*EXTDFT)
006500160314
006600160314       // -?Dati INPUT
006700160314     d FIORC0I0      e ds                  QUALIFIED INZ(*EXTDFT)
006800160314     d  idLingua     e                     INZ(FIORC00_ID_LINGUA_IT)
006900160314
007000160314       // -?Dati OUTPUT
007100160314     d FIORC0O0      e ds                  QUALIFIED INZ(*EXTDFT)
007200160314
007300160314       // -?Messaggi
007400160314     d FIORC0M0      e ds                  QUALIFIED INZ(*EXTDFT)
007500160314     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007600160314     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007700160314     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
007800160314     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
007900140306
008000140306       //--------------------------------------------------------------
008100140306       //?Definizione schiere.
008200140306       //--------------------------------------------------------------
008300140306
008400140306       //--------------------------------------------------------------
008500140306       //?Definizione campi.
008600140306       //--------------------------------------------------------------
008700140710
008800140710       // -?Flags booleani?
008900151009     d Fine            s               n   inz(*off)
009000151009     d wInzD02         s               n   inz(*on)
009100160301     d wInzD03         s               n   inz(*on)
009200140710
009300140710       // -?Campi associati al video
009400151009     d Video           s              2a   inz
009500140710
009600140710       // -?Comodo x campi dati
009700140710     d wData           s              8  0
009800160216
009900160216       // -?Indici di schiera
010000160216     d xx              s              4  0 inz
010100140306
010200140306       //--------------------------------------------------------------
010300140306       //?Definizione procedure.
010400140306       //--------------------------------------------------------------
010500140306
010600140306       //--------------------------------------------------------------
010700140306       //?Definizione prototipi.
010800140306       //--------------------------------------------------------------
010900160314      /copy gaitrasrc/srcprotopr,FIORC00R
011000160301      /copy gaitrasrc/srcprotopr,TIO7G31R
011100140306
011200140306       //--------------------------------------------------------------
011300140306       //?Definizione parametri programma.
011400140306       //--------------------------------------------------------------
011500140314     d prmRqsOpCode...
011600140314     d                 s             10I 0
011700140314     d prmRpyOpCode...
011800140314     d                 s             10I 0
011900140314     d prmRpyIdMsg...
012000140314     d                 s             10I 0
012100140314     d prmRqsFormato...
012200140314     d                 s             10A
012300140314     d prmRqsData...
012400140314     d                 s          32767A   varying
012500160216     d prmRqsSize...
012600140314     d                 s             10I 0
012700140314     d prmRpyFormato...
012800140314     d                 s             10A
012900140314     d prmRpyData...
013000140314     d                 s          32767A   varying
013100160216     d prmRpySize...
013200140314     d                 s             10I 0
013300160314
013400160314     d prmcRqsOpCode...
013500160314     d                 s             10I 0
013600160314     d prmcRpyOpCode...
013700160314     d                 s             10I 0
013800160314     d prmcRpyIdMsg...
013900160314     d                 s             10I 0
014000160314     d prmcRqsFormato...
014100160314     d                 s             10A
014200160314     d prmcRqsData...
014300160314     d                 s          32767A   varying
014400160314     d prmcRqsSize...
014500160314     d                 s             10I 0
014600160314     d prmcRpyFormato...
014700160314     d                 s             10A
014800160314     d prmcRpyData...
014900160314     d                 s          32767A   varying
015000160314     d prmcRpySize...
015100160314     d                 s             10I 0
015200160314     d prmcRpyMsgFormato...
015300160314     d                 s             10A
015400160314     d prmcRpyMsgData...
015500160314     d                 s          32767A   varying
015600160314     d prmcRpyMsgSize...
015700160314     d                 s             10I 0
015800140306
015900140306       //--------------------------------------------------------------
016000140306       //?MAIN.
016100140306       //--------------------------------------------------------------
016200140306
016300140306      /free
016400140306
016500140306       //?Operazioni iniziali
016600140306       exsr RoutInz;
016700140307
016800151009       //?Gestione Video
016900151009       DOW  Fine = *off;
017000151009         SELECT;
017100151009         WHEN  Video = 'D2';
017200151009           exsr GesD02;
017300160301         WHEN  Video = 'D3';
017400160301           exsr GesD03;
017500151009         OTHER;
017600151009           Fine = *on;
017700151009         ENDSL;
017800151009       ENDDO;
017900060612
018000140306       //?Operazioni finali
018100140306       exsr RoutEnd;
018200140306
018300140306       //--------------------------------------------------------------
018400140306       //?Operazioni iniziali.                                         ?
018500140306       //--------------------------------------------------------------
018600140306       BEGSR  RoutInz;
018700140306
018800160302         prmRqsOpCode = TIO7G31_RQSOPCODE_GETDETTAGLIOORM;
018900160301         prmRqsFormato = TIO1G3IDS.formato;
019000160301         prmRqsSize = %size(TIO1G3IDS);
019100160301         prmRpyFormato = TIO1G3ODS.formato;
019200160301         prmRpySize = %size(TIO1G3ODS);
019300151009
019400151009         Video = 'D2';
019500151009         wInzD02 = *on;
019600140306
019700140306       ENDSR;
019800151009
019900151009       //--------------------------------------------------------------
020000151009       //?Gestione videata D02.
020100151009       //--------------------------------------------------------------
020200151009       BEGSR GesD02;
020300151009
020400151009       //?Inizializzazione videata
020500151009         IF  wInzD02;
020600151009           exsr InzD02;
020700151009           wInzD02 = *off;
020800151009         ENDIF;
020900151009
021000151009       //?Emissione Testata
021100160301         write TI7G310T1;
021200151009
021300151009       //?Emissione videata
021400160301         exfmt TI7G310D2;
021500151009
021600151009         SELECT;
021700151009
021800151009       //?- F03=Fine
021900151009         WHEN  dsp_aid = c_F03;
022000151009           exsr F03D02;
022100160211
022200160211       //?- F06=Conferma
022300160211         WHEN  dsp_aid = c_F06;
022400160211           exsr F06D02;
022500151009
022600151009         ENDSL;
022700151009
022800151009       ENDSR;
022900151009
023000151009       //--------------------------------------------------------------
023100151009       //?Inizializzazione Videata D02.
023200151009       //--------------------------------------------------------------
023300151009       BEGSR InzD02;
023400151009
023500160302         iopcode = TIO7G31_RQSOPCODE_GETDETTAGLIOORM;
023600160301         iformato = TIO1G3IDS.formato;
023700160301         iversione = TIO1G3IDS.versione;
023800160302         ilingua = TIO1G3IDS.lang;
023900140307
024000151009       ENDSR;
024100151009
024200151009       //--------------------------------------------------------------
024300151009       //?Gestione tasto funzionale F03 da videata D02
024400151009       //?F03=Fine
024500151009       //--------------------------------------------------------------
024600151009       BEGSR F03D02;
024700151009
024800151009       //?Chiusura del programma
024900151009         Fine = *on;
025000151009
025100151009       ENDSR;
025200151009
025300151009       //--------------------------------------------------------------
025400151009       //?Gestione tasto funzionale F06 da videata D02
025500151009       //?F06=Conferma
025600151009       //--------------------------------------------------------------
025700151009       BEGSR F06D02;
025800151009
025900151009       //?Imposto i dati
026000160314         IF  orm = *zeros and npr = *blanks;
026100160211           leavesr;
026200151020         ENDIF;
026300160314
026400160314         IF  orm = *zeros;
026500160314         prmcRqsOpCode = FIORC00_RQSOPCODE_GET;
026600160314         prmcRqsFormato = FIORC0I0.formato;
026700160314         prmcRqsSize = %size(FIORC0I0);
026800160314         prmcRpyFormato = FIORC0O0.formato;
026900160314         prmcRpySize = %size(FIORC0O0);
027000160314         prmcRpyMsgFormato  = FIORC0M0.formato;
027100160314         prmcRpyMsgSize = %size(FIORC0M0);
027200160314         FIORC0I0.npr = npr;
027300160314       //?Richiamo FIORC00R
027400160314         FIORC00_GetOrm (prmcRqsOpCode:prmcRpyOpCode:prmcRpyIdMsg:
027500160314                         prmcRqsFormato:FIORC0I0:prmcRqsSize:
027600160314                         prmcRpyFormato:FIORC0O0:prmcRpySize:
027700160314                         prmcRpyMsgFormato:FIORC0M0:prmcRpyMsgSize);
027800160314         orm = (FIORC0O0.poe * 100000000000) +
027900160314               (FIORC0O0.nsr * 1000000000) +
028000160314               (FIORC0O0.nor * 100) +
028100160314                FIORC0O0.nrv;
028200160314         ENDIF;
028300160211
028400160301         TIO1G3IDS.orm = orm;
028500151009
028600160301       //?Richiamo TIO7G31R
028700160302         tio7G31_GetDettaglioOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
028800160302                                  prmRqsFormato:TIO1G3IDS:prmRqsSize:
028900160302                                  prmRpyFormato:TIO1G3ODS:prmRpySize);
029000160216
029100160216       //?Videata sucessiva
029200160301         Video = 'D3';
029300160301         wInzD03 = *on;
029400160216
029500160216       ENDSR;
029600160216
029700160216       //--------------------------------------------------------------
029800160301       //?Gestione videata D03.
029900160216       //--------------------------------------------------------------
030000160301       BEGSR GesD03;
030100160216
030200160216       //?Inizializzazione videata
030300160301         IF  wInzD03;
030400160301           exsr InzD03;
030500160301           wInzD03 = *off;
030600160216         ENDIF;
030700160301
030800160301       //?Emissione Testata
030900160301         write TI7G310T1;
031000160301
031100160301       //?Emissione videata
031200160301         exfmt TI7G310D3;
031300160216
031400160216         SELECT;
031500160216
031600160216       //?- F03=Fine
031700160216         WHEN  dsp_aid = c_F03;
031800160216           exsr F03D02;
031900160216
032000160216       //?- F12=Ritorno
032100160216         WHEN  dsp_aid = c_F12;
032200160301           exsr F12D03;
032300160216
032400160216         ENDSL;
032500160216
032600160216       ENDSR;
032700160216
032800160216       //--------------------------------------------------------------
032900160301       //?Inizializzazione videata D03.
033000160216       //--------------------------------------------------------------
033100160301       BEGSR InzD03;
033200151009
033300160216         oopcode = prmRpyOpCode;
033400160211         oformato = prmRpyFormato;
033500160301         oversione = TIO1G3ODS.versione;
033600160314         lang = TIO1G3IDS.lang;
033700160314         rsr = TIO1G3ODS.rsr;
033800160314         inr = TIO1G3ODS.inr;
033900160314         car = TIO1G3ODS.car;
034000160314         lor = TIO1G3ODS.lor;
034100160314         prr = TIO1G3ODS.prr;
034200160314         nar = TIO1G3ODS.nar;
034300160314         pag = TIO1G3ODS.pag;
034400160314         dar = TIO1G3ODS.dar;
034500160314         orr = TIO1G3ODS.orr;
034600160314         nam = TIO1G3ODS.nam;
034700160314         ncl = TIO1G3ODS.ncl;
034800160314         pkg = TIO1G3ODS.pkg;
034900160314         vlm = TIO1G3ODS.vlm;
035000160314         bnc = TIO1G3ODS.bnc;
035100160314         rer = TIO1G3ODS.rer;
035200160314         ter = TIO1G3ODS.ter;
035300160314         upd = TIO1G3ODS.upd;
035400160314         rsc = TIO1G3ODS.rsc;
035500160314         inc = TIO1G3ODS.inc;
035600160314         cac = TIO1G3ODS.cac;
035700160314         loc = TIO1G3ODS.loc;
035800160314         prc = TIO1G3ODS.prc;
035900160314         nac = TIO1G3ODS.nac;
036000160314         no1 = TIO1G3ODS.no1;
036100160314         no2 = TIO1G3ODS.no2;
036200160314         aas = TIO1G3ODS.aas;
036300160314         lnp = TIO1G3ODS.lnp;
036400160314         nrs = TIO1G3ODS.nrs;
036500160314         nsp = TIO1G3ODS.nsp;
036600160314         righefasi = TIO1G3ODS.righefasi;
036700160314         stat_n1 = TIO1G3ODS.stat_n1;
036800151009
036900151009       ENDSR;
037000160216
037100160216       //--------------------------------------------------------------
037200160301       //?Gestione tasto funzionale F12 da videata D03
037300160216       //?F12=Ritorno
037400160216       //--------------------------------------------------------------
037500160301       BEGSR F12D03;
037600160216
037700160216       //?Videata prima
037800160216         Video = 'D2';
037900160216
038000160216       ENDSR;
038100140306
038200140306       //--------------------------------------------------------------
038300140306       //?Operazioni finali.
038400140306       //--------------------------------------------------------------
038500140306       BEGSR  RoutEnd;
038600140311
038700140314         *inLR = *on;
038800140306
038900140306         return;
039000140306
039100140306       ENDSR;
