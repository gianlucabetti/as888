000100140306       //--------------------------------------------------------------
000200140306       //?Specifiche di controllo.                                     ?
000300140306       //--------------------------------------------------------------
000400140314     h DFTACTGRP(*NO) ACTGRP(*CALLER)
000500140306
000600140306       //--------------------------------------------------------------
000700140306       //?Dichiarazione file.                                          ?
000800140306       //--------------------------------------------------------------
000900140710
001000140710       // -?Video?
001100151009     fFIORA0TSTDcf   e             workstn
001200151009     f                                     sfile(ORA0S3:S03nrr)
001300151009     f                                     sfile(ORA0S5:S05nrr)
001400140710     f                                     indds(IndDspF)
001500140710     f                                     infds(InfDspF)
001600140306
001700140306       //--------------------------------------------------------------
001800140306       //?Definizione costanti.                                        ?
001900140306       //--------------------------------------------------------------
002000151009      /copy gaitrasrc/srcconst,FIORA00R
002100140710
002200140710       // -?Tasti funzionali a video?
002300140710     d c_F01           c                   const(x'31')
002400140710     d c_F02           c                   const(x'32')
002500140710     d c_F03           c                   const(x'33')
002600140710     d c_F04           c                   const(x'34')
002700140710     d c_F05           c                   const(x'35')
002800140710     d c_F06           c                   const(x'36')
002900140710     d c_F07           c                   const(x'37')
003000140710     d c_F08           c                   const(x'38')
003100140710     d c_F09           c                   const(x'39')
003200140710     d c_F10           c                   const(x'3A')
003300140710     d c_F11           c                   const(x'3B')
003400140710     d c_F12           c                   const(x'3C')
003500140710     d c_F13           c                   const(x'B1')
003600140710     d c_F14           c                   const(x'B2')
003700140710     d c_F15           c                   const(x'B3')
003800140710     d c_F16           c                   const(x'B4')
003900140710     d c_F17           c                   const(x'B5')
004000140710     d c_F18           c                   const(x'B6')
004100140710     d c_F19           c                   const(x'B7')
004200140710     d c_F20           c                   const(x'B8')
004300140710     d c_F21           c                   const(x'B9')
004400140710     d c_F22           c                   const(x'BA')
004500140710     d c_F23           c                   const(x'BB')
004600140710     d c_F24           c                   const(x'BC')
004700140710     d c_Enter         c                   const(x'F1')
004800140710     d c_RollDown      c                   const(x'F4')
004900140710     d c_RollUp        c                   const(x'F5')
005000140306
005100140306       //--------------------------------------------------------------
005200140306       //?Definizione strutture dati.                                  ?
005300140306       //--------------------------------------------------------------
005400140710       // -?InfDS?
005500140710     d InfDspF         ds
005600140710     d  dsp_aid              369    369a
005700140710
005800140710       // -?Indicatori su DspF?
005900140710     d IndDspF         ds
006000140710         // -?Indicatori di gestione subfile?
006100140710     d  SflDsp                        1n   overlay(IndDspF : 30)
006200140710     d  SflDspCtl                     1n   overlay(IndDspF : 31)
006300140710     d  SflEnd                        1n   overlay(IndDspF : 32)
006400140710
006500140306       // -?Dati INPUT
006600151009     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
006700151009     d  idRichiamo   e                     INZ(FIORA00_ID_RICHIAMO_INTERNET)
006800151009     d  idLingua     e                     INZ(FIORA00_ID_LINGUA_IT)
006900140306
007000140306       // -?Dati OUTPUT
007100151009     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
007200151012     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
007300151012     d                                     QUALIFIED INZ(*EXTDFT)
007400151020     d FIORB0OU      e ds                  EXTNAME(FIORB0O0)
007500151020     d                                     QUALIFIED INZ(*EXTDFT)
007600151009
007700151009       // -?Messaggi
007800151009     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
007900151009     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008000151009     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008100151009     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
008200151009     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
008300151009
008400151009       // -?Forzature
008500151009     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
008600151009     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
008700151009     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
008800151009     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
008900140306
009000140306       //--------------------------------------------------------------
009100140306       //?Definizione schiere.
009200140306       //--------------------------------------------------------------
009300140306
009400140306       //--------------------------------------------------------------
009500140306       //?Definizione campi.
009600140306       //--------------------------------------------------------------
009700140710
009800140710       // -?Flags booleani?
009900151009     d EndS03          s               n   inz(*off)
010000151009     d EndS05          s               n   inz(*off)
010100151009     d Fine            s               n   inz(*off)
010200151009     d wInzD02         s               n   inz(*on)
010300140710     d wInzS03         s               n   inz(*on)
010400151009     d wInzD04         s               n   inz(*on)
010500151009     d wInzS05         s               n   inz(*on)
010600151009     d wInzD06         s               n   inz(*on)
010700140710
010800140710       // -?Campi associati al video
010900151009     d Video           s              2a   inz
011000140710     d S03nrr          s              4  0 inz
011100151009     d S05nrr          s              4  0 inz
011200140710
011300140710       // -?Comodo x campi dati
011400140710     d wData           s              8  0
011500140710     d wData_eur       s               d   datfmt(*eur)
011600140710     d wData_iso       s               d   datfmt(*iso)
011700151012     d wCodice         s             10s 0
011800151020     d wKsu            s              8a
011900140710
012000140710       // -?Indici di schiera
012100140710     d xx              s              4  0 inz
012200140306
012300140306       //--------------------------------------------------------------
012400140306       //?Definizione procedure.
012500140306       //--------------------------------------------------------------
012600140306
012700140306       //--------------------------------------------------------------
012800140306       //?Definizione prototipi.
012900140306       //--------------------------------------------------------------
013000151009      /copy gaitrasrc/srcprotopr,FIORA00R
013100140306
013200140306       //--------------------------------------------------------------
013300140306       //?Definizione parametri programma.
013400140306       //--------------------------------------------------------------
013500140314     d prmRqsOpCode...
013600140314     d                 s             10I 0
013700140314     d prmRpyOpCode...
013800140314     d                 s             10I 0
013900140314     d prmRpyIdMsg...
014000140314     d                 s             10I 0
014100140314     d prmRqsFormato...
014200140314     d                 s             10A
014300140314     d prmRqsData...
014400140314     d                 s          32767A   varying
014500140314     d prmRqsDataSize...
014600140314     d                 s             10I 0
014700140314     d prmRpyFormato...
014800140314     d                 s             10A
014900140314     d prmRpyData...
015000140314     d                 s          32767A   varying
015100140314     d prmRpyDataSize...
015200140314     d                 s             10I 0
015300151009     d prmRpyFormMsg...
015400151009     d                 s             10A
015500151009     d prmRpyMessage...
015600151009     d                 s          32767A   varying
015700151009     d prmRpyMsgSize...
015800151009     d                 s             10I 0
015900151009     d prmRpyFormForz...
016000151009     d                 s             10A
016100151009     d prmRpyForzatu...
016200151009     d                 s          32767A   varying
016300151009     d prmRpyForSize...
016400151009     d                 s             10I 0
016500140306
016600140306       //--------------------------------------------------------------
016700140306       //?MAIN.
016800140306       //--------------------------------------------------------------
016900140306
017000140306      /free
017100140306
017200140306       //?Operazioni iniziali
017300140306       exsr RoutInz;
017400140307
017500151009       //?Gestione Video
017600151009       DOW  Fine = *off;
017700151009         SELECT;
017800151009         WHEN  Video = 'D2';
017900151009           exsr GesD02;
018000151009         WHEN  Video = 'S3';
018100151009           exsr GesS03;
018200151009         WHEN  Video = 'D4';
018300151009           exsr GesD04;
018400151009         WHEN  Video = 'S5';
018500151009           exsr GesS05;
018600151009         WHEN  Video = 'D6';
018700151009           exsr GesD06;
018800151009         OTHER;
018900151009           Fine = *on;
019000151009         ENDSL;
019100151009       ENDDO;
019200060612
019300140306       //?Operazioni finali
019400140306       exsr RoutEnd;
019500140306
019600140306       //--------------------------------------------------------------
019700140306       //?Operazioni iniziali.                                         ?
019800140306       //--------------------------------------------------------------
019900140306       BEGSR  RoutInz;
020000140306
020100151009         prmRqsOpCode = FIORA00_RQSOPCODE_CONTROL;
020200151009         prmRqsFormato = FIORA0I0.formato;
020300151009         prmRqsDataSize = %size(FIORA0I0);
020400151009         prmRpyFormato = FIORA0O0.formato;
020500151009         prmRpyDataSize = %size(FIORA0O0);
020600151009         prmRpyFormMsg  = FIORA0M0.formato;
020700151009         prmRpyMsgSize = %size(FIORA0M0);
020800151009         prmRpyFormForz = FIORA0F0.formato;
020900151009         prmRpyForSize = %size(FIORA0F0);
021000151009
021100151009         Video = 'D2';
021200151009         wInzD02 = *on;
021300140306
021400140306       ENDSR;
021500151009
021600151009       //--------------------------------------------------------------
021700151009       //?Gestione videata D02.
021800151009       //--------------------------------------------------------------
021900151009       BEGSR GesD02;
022000151009
022100151009       //?Inizializzazione videata
022200151009         IF  wInzD02;
022300151009           exsr InzD02;
022400151009           wInzD02 = *off;
022500151009         ENDIF;
022600151009
022700151009       //?Emissione Testata
022800151009         write  ORA0T1;
022900151009
023000151009       //?Emissione videata
023100151009         exfmt  ORA0D2;
023200151009
023300151009         SELECT;
023400151009
023500151009       //?- F03=Fine
023600151009         WHEN  dsp_aid = c_F03;
023700151009           exsr F03D02;
023800151009
023900151009       //?- F06=Conferma
024000151009         WHEN  dsp_aid = c_F06;
024100151009           exsr F06D02;
024200151009
024300151009         ENDSL;
024400151009
024500151009       ENDSR;
024600151009
024700151009       //--------------------------------------------------------------
024800151009       //?Inizializzazione Videata D02.
024900151009       //--------------------------------------------------------------
025000151009       BEGSR InzD02;
025100151009
025200151009         iopcode = FIORA00_RQSOPCODE_CONTROL;
025300151009         iformato = FIORA0I0.formato;
025400151009         iversione = FIORA0I0.versione;
025500140307
025600151009       ENDSR;
025700151009
025800151009       //--------------------------------------------------------------
025900151009       //?Gestione tasto funzionale F03 da videata D02
026000151009       //?F03=Fine
026100151009       //--------------------------------------------------------------
026200151009       BEGSR F03D02;
026300151009
026400151009       //?Chiusura del programma
026500151009         Fine = *on;
026600151009
026700151009       ENDSR;
026800151009
026900151009       //--------------------------------------------------------------
027000151009       //?Gestione tasto funzionale F06 da videata D02
027100151009       //?F06=Conferma
027200151009       //--------------------------------------------------------------
027300151009       BEGSR F06D02;
027400151009
027500151009       //?Imposto i dati
027600151020         IF  ksu <> *blanks;
027700151020           FIORA0I0.vaocor = %dec(%subst(ksu:2:7):7:0) * 1000;
027800151020         ENDIF;
027900151009         FIORA0I0.vaocra = icra;
028000151009         FIORA0I0.partitaiva = iiva;
028100151009         FIORA0I0.codfiscale = icf;
028200151009         FIORA0I0.vaorsr = irsr;
028300151009         FIORA0I0.vaoinr = iinr;
028400151009         FIORA0I0.vaolor = ilor;
028500151009         FIORA0I0.vaocar = icar;
028600151009         FIORA0I0.vaoprr = iprr;
028700151009         FIORA0I0.vaonar = inar;
028800151009         FIORA0I0.vaopag = ipag;
028900151009         FIORA0I0.contattot = icomm;
029000151009
029100151009       //?Richiamo FIORA00R
029200151012         FIORA00_Immetti (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
029300151009                          prmRqsFormato:FIORA0I0:prmRqsDataSize:
029400151009                          prmRpyFormato:FIORA0O0:prmRpyDataSize:
029500151009                          prmRpyFormMsg:FIORA0M0:prmRpyMsgSize:
029600151009                          prmRpyFormForz:FIORA0F0:prmRpyForSize);
029700151009
029800151009       //?Videata sucessiva
029900151009         Video = 'S3';
030000151009         wInzS03 = *on;
030100151009
030200151009       ENDSR;
030300151009
030400151009       //--------------------------------------------------------------
030500151009       //?Gestione videata S03.
030600151009       //--------------------------------------------------------------
030700151009       BEGSR GesS03;
030800151009
030900151009       //?Inizializzazione videata
031000151009         IF  wInzS03;
031100151009           exsr InzS03;
031200151009           wInzS03 = *off;
031300151009         ENDIF;
031400151009
031500151009       //?Visualizzazione del SFL (se ci sono dati)
031600151009         SflDsp = (S03nrr <= *zeros);
031700151009
031800151009       //?Emissione Testata e Piede con tasti funzionali abilitati
031900151009         write  ORA0T1;
032000151009         write  ORA0P3;
032100151009
032200151009       //?Emissione videata
032300151009         exfmt  ORA0C3;
032400151009
032500151009         SELECT;
032600151009
032700151009       //?- F03=Fine
032800151009         WHEN  dsp_aid = c_F03;
032900151009           exsr F03D02;
033000151009
033100151009       //?- F05=Avanti
033200151009         WHEN  dsp_aid = c_F05;
033300151009           exsr F05S03;
033400151009
033500151009       //?- F12=Ritorno
033600151009         WHEN  dsp_aid = c_F12;
033700151009           exsr F12S03;
033800151009
033900151009         ENDSL;
034000151009
034100151009       ENDSR;
034200151009
034300151009       //--------------------------------------------------------------
034400151009       //?Inizializzazione videata S03.
034500151009       //--------------------------------------------------------------
034600151009       BEGSR InzS03;
034700151009
034800151009         EndS03 = *off;
034900151009
035000151009       //?Pulizia subfile
035100151009         exsr PulS03;
035200151009
035300151009       //?Caricamento subfile
035400151009         exsr Ries03;
035500151009
035600151009         SflEnd = *on;
035700151009
035800151009       ENDSR;
035900151009
036000151009       //--------------------------------------------------------------
036100151009       //?Pulizia Subfile S03.
036200151009       //--------------------------------------------------------------
036300151009       BEGSR PulS03;
036400151009
036500151009       //?Pulizia subfile
036600151009         SflDsp    = *on;
036700151009         SflDspCtl = *on;
036800151009         write ORA0C3;
036900151009         SflDspCtl = *off;
037000151009         SflEnd    = *off;
037100151009
037200151009         clear S03nrr;
037300151009
037400151009       ENDSR;
037500151009
037600151009       //--------------------------------------------------------------
037700151009       //?Caricamento Subfile S03.
037800151009       //--------------------------------------------------------------
037900151009       BEGSR RieS03;
038000151009
038100151009         EndS03 = *off;
038200151020
038300151020       //?Se KSU cerco i dati
038400151020         IF  ksu <> *blanks;
038500151020           wKsu = ksu;
038600151020           reset FIORB0OU;
038700151020           CaricaKsu(FIORB0OU);
038800151020           ocomm = FIORB0OU.contatto;
038900151020         ENDIF;
039000151012
039100151012       //?se codificato cerco i dati del mittente
039200151012         IF  icra > 0;
039300151012         wCodice = icra;
039400151012         reset FIORB0OR;
039500151012         IsCodiceValido(FIORB0OR);
039600151012         orsr = FIORB0OR.nome;
039700151012         oinr = FIORB0OR.indirizzo;
039800151012         olor = FIORB0OR.localita;
039900151012         ocar = FIORB0OR.cap;
040000151012         oprr = FIORB0OR.provincia;
040100151012         onar = FIORB0OR.nazione;
040200151012         ENDIF;
040300151009
040400151009       //?Imposto i dati del control
040500151009         oopcode = prmRpyOpCode;
040600151009         oformato = prmRpyFormato;
040700151009         oversione = FIORA0O0.versione;
040800151009         oiva = FIORA0O0.partitaiva;
040900151009         ocf = FIORA0O0.codfiscale;
041000151012         IF  icra = 0;
041100151012         orsr = FIORA0O0.vaorsr;
041200151012         oinr = FIORA0O0.vaoinr;
041300151012         olor = FIORA0O0.vaolor;
041400151012         ocar = FIORA0O0.vaocar;
041500151012         oprr = FIORA0O0.vaoprr;
041600151012         onar = FIORA0O0.vaonar;
041700151012         ENDIF;
041800151009         ocomm = FIORA0O0.contattot;
041900151009         prepagato = FIORA0O0.prepagato;
042000151009         nrmsg = FIORA0M0.nrmsg;
042100151009         xx = 1;
042200151009       //?Leggo i messaggi
042300151009         IF  nrmsg > 0;
042400151009           FOR xx by 1 to nrmsg;
042500151012             clear  ORA0S3;
042600151012             IF  FIORA0M0.skIdCampo(xx) = 'CODFISCALE' or
042700151012                 FIORA0M0.skIdCampo(xx) = 'PARTITAIVA' or
042800151012                 FIORA0M0.skIdCampo(xx) = 'VAOCOR' or
042900151012                 FIORA0M0.skIdCampo(xx) = 'VAOCRA' or
043000151012                 FIORA0M0.skIdCampo(xx) = 'VAORSR' or
043100151012                 FIORA0M0.skIdCampo(xx) = 'VAOINR' or
043200151012                 FIORA0M0.skIdCampo(xx) = 'VAOCAR' or
043300151012                 FIORA0M0.skIdCampo(xx) = 'VAOLOR' or
043400151012                 FIORA0M0.skIdCampo(xx) = 'VAOPRR' or
043500151012                 FIORA0M0.skIdCampo(xx) = 'VAONAR' or
043600151012                 FIORA0M0.skIdCampo(xx) = 'VAOPAG' or
043700151012                 FIORA0M0.skIdCampo(xx) = 'CONTATTOT';
043800151012             riga = %trim(FIORA0M0.skIdCampo(xx)) +
043900151009                    %trim(FIORA0M0.skTextMsg(xx));
044000151009             S03nrr += 1;
044100151009             write ORA0S3;
044200151012             ENDIF;
044300151009           ENDFOR;
044400151009         ENDIF;
044500151009
044600151009       ENDSR;
044700151009
044800151009       //--------------------------------------------------------------
044900151009       //?Gestione tasto funzionale F05 da videata S03
045000151009       //?F05=Avanti
045100151009       //--------------------------------------------------------------
045200151009       BEGSR F05S03;
045300151009
045400151009       //?Videata sucessiva
045500151009         Video = 'D4';
045600151009         wInzD04 = *on;
045700151009
045800151009       ENDSR;
045900151009
046000151009       //--------------------------------------------------------------
046100151009       //?Gestione tasto funzionale F12 da videata S03
046200151009       //?F12=Ritorno
046300151009       //--------------------------------------------------------------
046400151009       BEGSR F12S03;
046500151009
046600151009       //?Videata prima
046700151009         Video = 'D2';
046800151009
046900151009       ENDSR;
047000151009
047100151009       //--------------------------------------------------------------
047200151009       //?Gestione videata D04.
047300151009       //--------------------------------------------------------------
047400151009       BEGSR GesD04;
047500151009
047600151009       //?Inizializzazione videata
047700151009         IF  wInzD04;
047800151009           exsr InzD04;
047900151009           wInzD04 = *off;
048000151009         ENDIF;
048100151009
048200151009       //?Emissione Testata
048300151009         write  ORA0T1;
048400151009
048500151009       //?Emissione videata
048600151009         exfmt  ORA0D4;
048700151009
048800151009         SELECT;
048900151009
049000151009       //?- F03=Fine
049100151009         WHEN  dsp_aid = c_F03;
049200151009           exsr F03D02;
049300151009
049400151009       //?- F06=Conferma
049500151009         WHEN  dsp_aid = c_F06;
049600151009           exsr F06D04;
049700151012
049800151012       //?- F12=Ritorno
049900151012         WHEN  dsp_aid = c_F12;
050000151012           exsr F12D04;
050100151009
050200151009         ENDSL;
050300151009
050400151009       ENDSR;
050500151009
050600151009       //--------------------------------------------------------------
050700151009       //?Inizializzazione Videata D04.
050800151009       //--------------------------------------------------------------
050900151009       BEGSR InzD04;
051000151009
051100151020         irer = FIORB0OR.referente;
051200151020         itel = FIORB0OR.telefono;
051300151020         inam = FIORB0OR.natmerce;
051400151020         iorr = FIORB0OR.oraritiro;
051500151020         imailconf = FIORB0OU.mailconf;
051600151020         ialertconf = FIORB0OU.alertconf;
051700160322         ismsconf = FIORB0OU.smsconf;
051800151009
051900151009       ENDSR;
052000151009
052100151009       //--------------------------------------------------------------
052200151009       //?Gestione tasto funzionale F06 da videata D04
052300151009       //?F06=Conferma
052400151009       //--------------------------------------------------------------
052500151009       BEGSR F06D04;
052600151009
052700151009       //?Imposto i dati
052800151009         FIORA0I0.vaodar = idar;
052900151009         FIORA0I0.vaoorr = iorr;
053000151009         FIORA0I0.vaoncl = incl;
053100151009         FIORA0I0.vaopkg = ipkg;
053200151009         FIORA0I0.vaovlm = ivlm;
053300151009         FIORA0I0.vaobnc = ibnc;
053400151009         FIORA0I0.vaorer = irer;
053500151012         FIORA0I0.vaoter = itel;
053600151009         FIORA0I0.vaonam = inam;
053700151009         FIORA0I0.vaorfa = irfa;
053800151009         FIORA0I0.vaono2 = ino2;
053900151009         FIORA0I0.dalleam = idalleam;
054000151009         FIORA0I0.alleam = ialleam;
054100151009         FIORA0I0.dallepm = idallepm;
054200151009         FIORA0I0.allepm = iallepm;
054300151009         FIORA0I0.unopiudest = idest;
054400151009         FIORA0I0.vaorsc = irsc;
054500151009         FIORA0I0.vaoinc = iinc;
054600151009         FIORA0I0.vaoloc = iloc;
054700151009         FIORA0I0.vaocac = icac;
054800151009         FIORA0I0.vaoprc = iprc;
054900151009         FIORA0I0.vaonac = inac;
055000151009         FIORA0I0.mail = imail;
055100151009         FIORA0I0.sms = isms;
055200151009         FIORA0I0.mailconf = imailconf;
055300151012         FIORA0I0.sceltantw = isceltantw;
055400151009         FIORA0I0.alertmail = ialertmail;
055500151009         FIORA0I0.alertsms = ialertsms;
055600151009         FIORA0I0.alertconf = ialertconf;
055700160322         FIORA0I0.smsconf = ismsconf;
055800151012         FIORA0I0.vaoffd = 'N';
055900151009
056000151009       //?Richiamo FIORA00R
056100151012         FIORA00_Immetti (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
056200151009                          prmRqsFormato:FIORA0I0:prmRqsDataSize:
056300151009                          prmRpyFormato:FIORA0O0:prmRpyDataSize:
056400151009                          prmRpyFormMsg:FIORA0M0:prmRpyMsgSize:
056500151009                          prmRpyFormForz:FIORA0F0:prmRpyForSize);
056600151009
056700151009       //?Videata sucessiva
056800151009         Video = 'S5';
056900151009         wInzS05 = *on;
057000151009
057100151009       ENDSR;
057200151012
057300151012       //--------------------------------------------------------------
057400151012       //?Gestione tasto funzionale F12 da videata D04
057500151012       //?F12=Ritorno
057600151012       //--------------------------------------------------------------
057700151012       BEGSR F12D04;
057800151012
057900151012       //?Videata precedente
058000151012         Video = 'S3';
058100151012
058200151012       ENDSR;
058300151009
058400151009       //--------------------------------------------------------------
058500151009       //?Gestione videata S05.
058600151009       //--------------------------------------------------------------
058700151009       BEGSR GesS05;
058800151009
058900151009       //?Inizializzazione videata
059000151009         IF  wInzS05;
059100151009           exsr InzS05;
059200151009           wInzS05 = *off;
059300151009         ENDIF;
059400151009
059500151009       //?Visualizzazione del SFL (se ci sono dati)
059600151009         SflDsp = (S05nrr <= *zeros);
059700151009
059800151009       //?Emissione Testata e Piede con tasti funzionali abilitati
059900151009         write  ORA0T1;
060000151009         write  ORA0P5;
060100151009
060200151009       //?Emissione videata
060300151009         exfmt  ORA0C5;
060400151009
060500151009         SELECT;
060600151009
060700151009       //?- F03=Fine
060800151009         WHEN  dsp_aid = c_F03;
060900151009           exsr F03D02;
061000151009
061100151009       //?- F05=Avanti
061200151009         WHEN  dsp_aid = c_F05;
061300151009           exsr F05S05;
061400151009
061500151009       //?- F12=Ritorno
061600151009         WHEN  dsp_aid = c_F12;
061700151009           exsr F12S05;
061800151009
061900151009         ENDSL;
062000151009
062100151009       ENDSR;
062200151009
062300151009       //--------------------------------------------------------------
062400151009       //?Inizializzazione videata S05.
062500151009       //--------------------------------------------------------------
062600151009       BEGSR InzS05;
062700151009
062800151009         EndS05 = *off;
062900151009
063000151009       //?Pulizia subfile
063100151009         exsr PulS05;
063200151009
063300151009       //?Caricamento subfile
063400151009         exsr Ries05;
063500151009
063600151009         SflEnd = *on;
063700151009
063800151009       ENDSR;
063900151009
064000151009       //--------------------------------------------------------------
064100151009       //?Pulizia Subfile S05.
064200151009       //--------------------------------------------------------------
064300151009       BEGSR PulS05;
064400151009
064500151009       //?Pulizia subfile
064600151009         SflDsp    = *on;
064700151009         SflDspCtl = *on;
064800151009         write ORA0C5;
064900151009         SflDspCtl = *off;
065000151009         SflEnd    = *off;
065100151009
065200151009         clear S05nrr;
065300151009
065400151009       ENDSR;
065500151009
065600151009       //--------------------------------------------------------------
065700151009       //?Caricamento Subfile S05.
065800151009       //--------------------------------------------------------------
065900151009       BEGSR RieS05;
066000151009
066100151009         EndS05 = *off;
066200151009
066300151009       //?Imposto i dati del control
066400151012         odar = FIORA0O0.vaodar;
066500151012         oorr = FIORA0O0.vaoorr;
066600151012         oncl = FIORA0I0.vaoncl;
066700151012         opkg = FIORA0I0.vaopkg;
066800151012         ovlm = FIORA0I0.vaovlm;
066900151012         obnc = FIORA0I0.vaobnc;
067000151012         orer = FIORA0O0.vaorer;
067100151012         otel = FIORA0O0.vaoter;
067200151012         onam = FIORA0O0.vaonam;
067300151012         orfa = FIORA0O0.vaorfa;
067400151012         ono2 = FIORA0O0.vaono2;
067500151012         odalleam = FIORA0I0.dalleam;
067600151012         oalleam = FIORA0I0.alleam;
067700151012         odallepm = FIORA0I0.dallepm;
067800151012         oallepm = FIORA0I0.allepm;
067900151012         odest = FIORA0O0.unopiudest;
068000151012         orsc = FIORA0O0.vaorsc;
068100151012         oinc = FIORA0O0.vaoinc;
068200151012         oloc = FIORA0O0.vaoloc;
068300151012         ocac = FIORA0O0.vaocac;
068400151012         oprc = FIORA0O0.vaoprc;
068500151012         onac = FIORA0O0.vaonac;
068600151012         omail= FIORA0O0.mail;
068700151012         osms = FIORA0O0.sms;
068800151012         omailconf = FIORA0O0.mailconf;
068900151012         osceltantw = FIORA0O0.sceltantw;
069000151012         oalertmail = FIORA0O0.alertmail;
069100151012         oalertsms = FIORA0O0.alertsms;
069200151012         oalertconf = FIORA0O0.alertconf;
069300160322         oalconfs   = FIORA0O0.alertconfs;
069400160322         osmsconf = FIORA0O0.smsconf;
069500151009         nrmsg = FIORA0M0.nrmsg;
069600151009         xx = 1;
069700151009       //?Leggo i messaggi
069800151009         IF  nrmsg > 0;
069900151009           FOR xx by 1 to nrmsg;
070000151009            clear  ORA0S5;
070100151009             riga = %trim(FIORA0M0.skIdCampo(xx)) +
070200151009                    %trim(FIORA0M0.skTextMsg(xx));
070300151012             S05nrr += 1;
070400151009             write ORA0S5;
070500151009           ENDFOR;
070600151009         ENDIF;
070700151009
070800151009       ENDSR;
070900151009
071000151009       //--------------------------------------------------------------
071100151009       //?Gestione tasto funzionale F05 da videata S05
071200151009       //?F05=Avanti
071300151009       //--------------------------------------------------------------
071400151009       BEGSR F05S05;
071500151009
071600151009       //?Videata sucessiva
071700151012         Video = 'D6';
071800151012         wInzD06 = *on;
071900151009
072000151009       ENDSR;
072100151009
072200151009       //--------------------------------------------------------------
072300151009       //?Gestione tasto funzionale F12 da videata S05
072400151009       //?F12=Ritorno
072500151009       //--------------------------------------------------------------
072600151009       BEGSR F12S05;
072700151009
072800151009       //?Videata prima
072900151009         Video = 'D4';
073000151009
073100151009       ENDSR;
073200151012
073300151012       //--------------------------------------------------------------
073400151012       //?Gestione videata D06.
073500151012       //--------------------------------------------------------------
073600151012       BEGSR GesD06;
073700151012
073800151012       //?Inizializzazione videata
073900151012         IF  wInzD06;
074000151012           exsr InzD06;
074100151012           wInzD06 = *off;
074200151012         ENDIF;
074300151012
074400151012       //?Emissione Testata
074500151012         write  ORA0T1;
074600151012
074700151012       //?Emissione videata
074800151012         exfmt  ORA0D6;
074900151012
075000151012         SELECT;
075100151012
075200151012       //?- F03=Fine
075300151012         WHEN  dsp_aid = c_F03;
075400151012           exsr F03D02;
075500151012
075600151012       //?- F12=Ritorno
075700151012         WHEN  dsp_aid = c_F12;
075800151012           exsr F12D06;
075900151012
076000151012         ENDSL;
076100151012
076200151012       ENDSR;
076300151012
076400151012       //--------------------------------------------------------------
076500151012       //?Inizializzazione Videata D06.
076600151012       //--------------------------------------------------------------
076700151012       BEGSR InzD06;
076800151012
076900151012       //?Pulizia videata
077000151012         clear ORA0D6;
077100160216
077200160216         prmRqsOpCode = FIORA00_RQSOPCODE_WRITENPR;
077300160216       //?Richiamo FIORA00R
077400160216         FIORA00_Immetti (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
077500160216                          prmRqsFormato:FIORA0I0:prmRqsDataSize:
077600160216                          prmRpyFormato:FIORA0O0:prmRpyDataSize:
077700160216                          prmRpyFormMsg:FIORA0M0:prmRpyMsgSize:
077800160216                          prmRpyFormForz:FIORA0F0:prmRpyForSize);
077900160216
078000151012         opoe = fiora0O0.vaopoe;
078100151012         opor = fiora0O0.vaopor;
078200151012         oorariti = fiora0O0.orariti;
078300151012         ooraritf = fiora0O0.oraritf;
078400151012         oritiroest = fiora0O0.ritiroest;
078500151012         oorainizio = fiora0O0.orainizio;
078600151012         oorafine = fiora0O0.orafine;
078700151012         ooracomm = fiora0O0.oracomm;
078800151012         ooranocomm = fiora0O0.oranocomm;
078900160216         onpr = FIORA0O0.npr;
079000160217         orfa = fiora0O0.vaorfa;
079100151012
079200151012       ENDSR;
079300151012
079400151012       //--------------------------------------------------------------
079500151012       //?Gestione tasto funzionale F12 da videata D06
079600151012       //?F12=Ritorno
079700151012       //--------------------------------------------------------------
079800151012       BEGSR F12D06;
079900151012
080000151012       //?Videata Precedente
080100151012         Video = 'S5';
080200151012
080300151012       ENDSR;
080400140306
080500140306       //--------------------------------------------------------------
080600140306       //?Operazioni finali.
080700140306       //--------------------------------------------------------------
080800140306       BEGSR  RoutEnd;
080900140311
081000140314         *inLR = *on;
081100140306
081200140306         return;
081300140306
081400140306       ENDSR;
081500151020
081600151020      *--------------------------------------------------
081700151020      * Procedure name: CaricaKsu
081800151020      * Purpose:        Recupera i dati del KSU
081900151020      * Returns:        ON - OFF
082000151020      *--------------------------------------------------
082100151020     p CaricaKsu       B
082200151020     d CaricaKsu       PI              n
082300151020     d  FIORB0O0                           likeds(FIORB0OU)
082400151020
082500151020      /copy gaitrasrc/srcconst,FIORB00R
082600151020     d isErrore        s               n   inz
082700151020     d RpyIdMsg        s             10i 0
082800151020     d RpyOpCode       s             10i 0
082900151020     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_LEGGI)
083000151020     d fiorb0I0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
083100151020      /copy gaitrasrc/srcprotopr,FIORB00R
083200151020
083300151020      /free
083400151020
083500151020       reset isErrore;
083600151020       reset RpyIdMsg;
083700151020       reset RpyOpCode;
083800151020       reset fiorb0I0;
083900151020
084000151020       fiorb0I0.codiceksu = wKsu;
084100151020       MONITOR;
084200151020         Fiorb00_Anagrafica( RqsOpCode
084300151020                           : RpyOpCode
084400151020                           : RpyIdMsg
084500151020                           : fiorb0I0.formato
084600151020                           : fiorb0I0
084700151020                           : %SIZE(fiorb0I0)
084800151020                           : fiorb0O0.formato
084900151020                           : fiorb0O0
085000151020                           : %SIZE(fiorb0O0)
085100151020                           );
085200151020         ON-ERROR;
085300151020           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
085400151020         ENDMON;
085500151020
085600151020       IF RpyOpCode < FIORB00_RPYOPCODE_DONE;
085700151020         isErrore = *on;
085800151020       ENDIF;
085900151020
086000151020       IF  isErrore;
086100151020         return *off;
086200151020       ENDIF;
086300151020
086400151020       RETURN *on;
086500151020
086600151020      /END-FREE
086700151020     p CaricaKsu       E
086800151012
086900151012      *--------------------------------------------------
087000151012      * Procedure name: IsCodiceValido
087100151012      * Purpose:        Controlla codice
087200151012      * Returns:        ON - OFF
087300151012      *--------------------------------------------------
087400151012     p IsCodiceValido  B
087500151012     d IsCodiceValido  PI              n
087600151012     d  FIORB0O1                           likeds(FIORB0OR)
087700151012
087800151012      /copy gaitrasrc/srcconst,FIORB00R
087900151012     d isErrore        s               n   inz
088000151012     d RpyIdMsg        s             10i 0
088100151012     d RpyOpCode       s             10i 0
088200151012     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
088300151012     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
088400151012      /copy gaitrasrc/srcprotopr,FIORB00R
088500151012
088600151012      /free
088700151012
088800151012       reset isErrore;
088900151012       reset RpyIdMsg;
089000151012       reset RpyOpCode;
089100151012       reset fiorb0I1;
089200151012
089300151012       fiorb0I1.codice = wCodice;
089400151012       MONITOR;
089500151012         Fiorb00_Anagrafica( RqsOpCode
089600151012                           : RpyOpCode
089700151012                           : RpyIdMsg
089800151012                           : fiorb0I1.formato
089900151012                           : fiorb0I1
090000151012                           : %SIZE(fiorb0I1)
090100151012                           : fiorb0O1.formato
090200151012                           : fiorb0O1
090300151012                           : %SIZE(fiorb0O1)
090400151012                           );
090500151012         ON-ERROR;
090600151012           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
090700151012         ENDMON;
090800151012
090900151012       IF RpyOpCode < FIORB00_RPYOPCODE_DONE;
091000151012         isErrore = *on;
091100151012       ENDIF;
091200151012
091300151012       IF  isErrore;
091400151012         return *off;
091500151012       ENDIF;
091600151012
091700151012       RETURN *on;
091800151012
091900151012      /END-FREE
092000151012     p IsCodiceValido  E
092100140306
092200140306      /end-free
