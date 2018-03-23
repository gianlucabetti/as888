000100131108       //==============================================================
000200140310       //?TEST - Richiamo pgm FIORB0R
000300131108       //==============================================================
000400131108
000500131108       //--------------------------------------------------------------
000600131108       //?Specifiche di controllo.                                     ?
000700131108       //--------------------------------------------------------------
000800131108
000900131108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000131108
001100131108       //--------------------------------------------------------------
001200131108       //?Dichiarazione file.                                          ?
001300131108       //--------------------------------------------------------------
001400131108
001500131108       // -?Video?
001600140310     fFIORB0TSTDcf   e             workstn
001700140310     f                                     sfile(ORB0S3:S03nrr)
001800131108     f                                     indds(IndDspF)
001900131108     f                                     infds(InfDspF)
002000131108
002100131108       //--------------------------------------------------------------
002200131108       //?Definizione costanti.                                        ?
002300131108       //--------------------------------------------------------------
002400140310      /copy gaitrasrc/srcconst,FIORB00R
002500131108
002600131108       // -?Costante per controllo "caratteri solo numerici"?
002700131108     d c_Digits        c                   const('0123456789')
002800131108
002900131108       // -?Tasti funzionali a video?
003000131108     d c_F01           c                   const(x'31')
003100131108     d c_F02           c                   const(x'32')
003200131108     d c_F03           c                   const(x'33')
003300131108     d c_F04           c                   const(x'34')
003400131108     d c_F05           c                   const(x'35')
003500131108     d c_F06           c                   const(x'36')
003600131108     d c_F07           c                   const(x'37')
003700131108     d c_F08           c                   const(x'38')
003800131108     d c_F09           c                   const(x'39')
003900131108     d c_F10           c                   const(x'3A')
004000131108     d c_F11           c                   const(x'3B')
004100131108     d c_F12           c                   const(x'3C')
004200131108     d c_F13           c                   const(x'B1')
004300131108     d c_F14           c                   const(x'B2')
004400131108     d c_F15           c                   const(x'B3')
004500131108     d c_F16           c                   const(x'B4')
004600131108     d c_F17           c                   const(x'B5')
004700131108     d c_F18           c                   const(x'B6')
004800131108     d c_F19           c                   const(x'B7')
004900131108     d c_F20           c                   const(x'B8')
005000131108     d c_F21           c                   const(x'B9')
005100131108     d c_F22           c                   const(x'BA')
005200131108     d c_F23           c                   const(x'BB')
005300131108     d c_F24           c                   const(x'BC')
005400131108     d c_Enter         c                   const(x'F1')
005500131108     d c_RollDown      c                   const(x'F4')
005600131108     d c_RollUp        c                   const(x'F5')
005700131108
005800131108       //--------------------------------------------------------------
005900131108       //?Definizione schiere.                                         ?
006000131108       //--------------------------------------------------------------
006100131108
006200131108       //--------------------------------------------------------------
006300131108       //?Definizione aree dati.                                       ?
006400131108       //--------------------------------------------------------------
006500131108
006600131108       // -?Dati utente?
006700131108     d §AzUte        e ds                  extname(AZUTE00F)
006800131108     d                                     dtaara
006900131108     d §DatiUte      e ds                  extname(dDatiUte)
007000131108     d                                     dtaara
007100131108
007200131108       //--------------------------------------------------------------
007300131108       //?Definizione strutture dati.                                  ?
007400131108       //--------------------------------------------------------------
007500131108
007600131108       // -?Status ds?
007700131108     d Status         sds
007800131108     d  SDSpgm           *proc
007900131108     d  SDSprm           *parms
008000131108     d  SDSusr               254    263
008100131108
008200131108       // -?InfDS?
008300131108     d InfDspF         ds
008400131108     d  dsp_aid              369    369a
008500131108
008600131108       // -?Indicatori su DspF?
008700131108     d IndDspF         ds
008800131217         // -?Indicatori di gestione subfile?
008900140310     d  SflDsp                        1n   overlay(IndDspF : 30)
009000140310     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009100140310     d  SflEnd                        1n   overlay(IndDspF : 32)
009200131108         // -?Emissione messaggio di errore?
009300131108     d   ErrMessage                    n   overlay(IndDspF : 28)
009400131108         // -?Riemissione videata?
009500131108     d   ErrGenerico                   n   overlay(IndDspF : 99)
009600131108
009700131108       // -?kpjba?
009800131129     d KPJBA         e ds
009900131217
010000131217       // -?Dati INPUT
010100140310     d FIORB0I0      e ds                  QUALIFIED INZ(*EXTDFT)
010200131217
010300131217       // -?Dati OUTPUT
010400140310     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
010500140310     d  skCodice                     10a   dim(10) overlay(Codice)
010600140310     d  skNome                       48a   dim(10) overlay(Nome)
010700140310     d  skIndirizzo                  35a   dim(10) overlay(Indirizzo)
010800140310     d  skCap                         9a   dim(10) overlay(Cap)
010900140310     d  skLocalita                   35a   dim(10) overlay(Localita)
011000140310     d  skProvincia                   2a   dim(10) overlay(Provincia)
011100140310     d  skNazione                     3a   dim(10) overlay(Nazione)
011200131108
011300131108       //--------------------------------------------------------------
011400131108       //?Definizione variabili globali.                               ?
011500131108       //--------------------------------------------------------------
011600131108
011700131108       // -?Flags booleani?
011800131216     d wFine           s               n   inz(*off)
011900131216     d wInzD01         s               n   inz(*on)
012000140310     d wInzD02         s               n   inz(*off)
012100140310     d wInzS03         s               n   inz(*off)
012200131217
012300131217       // -?Campi associati al video
012400131217     d wVideo          s              2    inz('D1')
012500140310     d S03nrr          s              4  0 inz
012600131129
012700131129       // -?Comodo x campi dati
012800131218     d wData           s              8  0
012900131129     d wData_eur       s               d   datfmt(*eur)
013000131129     d wData_iso       s               d   datfmt(*iso)
013100131217
013200131217       // -?Indici di schiera
013300131217     d xx              s              4  0 inz
013400131108
013500131108       //--------------------------------------------------------------
013600131108       //?Definizione procedure usate.                                 ?
013700131108       //--------------------------------------------------------------
013800131108
013900131108       // -?Reperimento dati utente?
014000131108     d TIBS34ds      e ds                  qualified
014100131219      /copy gaitrasrc/srcprotopr,TIBS34R
014200131216
014300131216       //--------------------------------------------------------------
014400131216       //?Definizione prototipi.
014500131216       //--------------------------------------------------------------
014600140310      /copy gaitrasrc/srcprotopr,FIORB00R
014700140710
014800140710       //--------------------------------------------------------------
014900140710       //?Definizione parametri programma.
015000140710       //--------------------------------------------------------------
015100140710     d prmRqsOpCode...
015200140710     d                 s             10I 0
015300140710     d prmRpyOpCode...
015400140710     d                 s             10I 0
015500140710     d prmRpyIdMsg...
015600140710     d                 s             10I 0
015700140710     d prmRqsFormato...
015800140710     d                 s             10A
015900140710     d prmRqsData...
016000140710     d                 s          32767A   varying
016100140710     d prmRqsDataSize...
016200140710     d                 s             10I 0
016300140710     d prmRpyFormato...
016400140710     d                 s             10A
016500140710     d prmRpyData...
016600140710     d                 s          32767A   varying
016700140710     d prmRpyDataSize...
016800140710     d                 s             10I 0
016900131108
017000131108       //--------------------------------------------------------------
017100131108       //?Definizione key-list.                                        ?
017200131108       //--------------------------------------------------------------
017300131108
017400131108       //--------------------------------------------------------------
017500131108       //?Riepilogo indicatori.                                        ?
017600131108       //--------------------------------------------------------------
017700131108
017800131108       //--------------------------------------------------------------
017900131108       //?M A I N - L I N E                                            ?
018000131108       //--------------------------------------------------------------
018100131129     c     *Entry        plist
018200131129     c                   parm                    KPJBA
018300131108
018400131108      /free
018500131108
018600131108       // -?Start?
018700131108       *inLR = *on;
018800131108
018900131108       // -?Elab?
019000131217       DOW  not wFine;
019100131217         SELECT;
019200131217           WHEN  wVideo = 'D1';
019300131217             exsr GesD01;
019400140310           WHEN  wVideo = 'D2';
019500140310             exsr GesD02;
019600140310           WHEN  wVideo = 'S3';
019700140310             exsr GesS03;
019800131217         ENDSL;
019900131217       ENDDO;
020000131108
020100131108       // -?End?
020200131108       return;
020300131108
020400131108       //--------------------------------------------------------------
020500131108       //?Operazioni iniziali.                                         ?
020600131108       //--------------------------------------------------------------
020700131217       BEGSR *InzSR;
020800131108
020900131108         // -?Reperimento dati job?
021000131217         exsr DatiJob;
021100131108
021200131108       ENDSR;
021300131108
021400131108       //--------------------------------------------------------------
021500131108       //?Reperimento Dati del job (Utente/Operativi).                 ?
021600131108       //--------------------------------------------------------------
021700131217       BEGSR DatiJob;
021800131108
021900131108         in(E) §AzUte;
022000131108         if NOT %error;
022100131108           in(E) §DatiUte;
022200131108         endif;
022300131108         if %error or RSut = *blanks;
022400131108           clear TIBS34ds;
022500131108           tibs34r(tibs34ds);
022600131108           in §AzUte;
022700131108           in §DatiUte;
022800131108         endif;
022900131108
023000131108       ENDSR;
023100131108
023200131108       //--------------------------------------------------------------
023300131108       //?Gestione videata D01                                         ?
023400131108       //--------------------------------------------------------------
023500131217       BEGSR GesD01;
023600131108
023700131108         // -?Inizializzazione videata?
023800131217         IF  wInzD01;
023900140310           clear ORB0D1;
024000140310           iformato = 'FIORB0I0';
024100131227           iversione = 'A';
024200131217           wInzD01 = *off;
024300131217         ENDIF;
024400131108
024500131108         // -?Emissione videata?
024600131217         IF  not ErrMessage;
024700140310           write ORB0T1;
024800131217         ENDIF;
024900140310         exfmt ORB0D1;
025000131108
025100131217         reset ErrGenerico;
025200131217         reset ErrMessage;
025300131108
025400131108         SELECT;
025500131108
025600131108           // -?F3=Fine?
025700131108           WHEN  dsp_aid = c_F03;
025800131216             wFine = *on;
025900131108
026000131108           // -?F6=Conferma?
026100131108           WHEN  dsp_aid = c_F06;
026200131217             exsr MoveDaVideo;
026300140311             exsr CallLEGGI;
026400131217             clear ErrGenerico;
026500131217             clear ErrMessage;
026600140310             wInzD02 = *on;
026700140310             wVideo  = 'D2';
026800131108
026900131108         ENDSL;
027000131108
027100131108       ENDSR;
027200131217
027300131217       //--------------------------------------------------------------
027400131217       //?Muovo i dati da video a FIORA0I0                        ?
027500131217       //--------------------------------------------------------------
027600131217       BEGSR MoveDaVideo;
027700131218
027800140310         fiorb0i0.codiceksu  = ksu;
027900140310         fiorb0i0.ordina     = ordina;
028000140310         fiorb0i0.escluditcr = frequenza;
028100131217
028200131217       ENDSR;
028300131108
028400131108       //--------------------------------------------------------------
028500140310       //?Richiamo FIORB00R                                       ?
028600131108       //--------------------------------------------------------------
028700140311       BEGSR CallLEGGI;
028800131129
028900140310         prmRqsOpCode = FIORB00_RQSOPCODE_LEGGI;
029000140310         prmRqsFormato = fiorb0i0.formato;
029100140710         prmRqsData    = fiorb0i0;
029200140310         prmRqsDataSize = %size(fiorb0i0);
029300140310         prmRpyFormato = fiorb0o0.formato;
029400140710         prmRpyData    = fiorb0o0;
029500140310         prmRpyDataSize = %size(fiorb0o0);
029600131108
029700140310         // -?Richiamo FIORB00R?
029800140310         fiorb00_Anagrafica (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
029900140710                          prmRqsFormato:FIORB0I0:prmRqsDataSize:
030000140710                          prmRpyFormato:FIORB0O0:prmRpyDataSize);
030100131217       ENDSR;
030200140310
030300140310       //--------------------------------------------------------------
030400140310       //?Gestione videata D02                                         ?
030500140310       //--------------------------------------------------------------
030600140310       BEGSR GesD02;
030700140310
030800140310         // -?Inizializzazione videata?
030900140310         IF  wInzD02;
031000140310           clear ORB0D2;
031100140310           opcode2 = opcode;
031200140310           oformato = fiorb0o0.formato;
031300140310           oversione = fiorb0o0.versione;
031400140310           ksu2 = ksu;
031500140310           ordina2 = ordina;
031600140310           frequenza2 = frequenza;
031700140310           clientitot = fiorb0o0.clientitot;
031800160322           alertconf = fiorb0o0.alertconf;
031900160322           mailconf = fiorb0o0.mailconf;
032000160322           smsconf = fiorb0o0.smsconf;
032100140310           wInzD02 = *off;
032200140310         ENDIF;
032300140310
032400140310         // -?Emissione videata?
032500140310         IF  not ErrMessage;
032600140310           write ORB0T1;
032700140310         ENDIF;
032800140310         exfmt ORB0D2;
032900140310
033000140310         reset ErrGenerico;
033100140310         reset ErrMessage;
033200140310
033300140310         SELECT;
033400140310
033500140310           // -?F3=Fine?
033600140310           WHEN  dsp_aid = c_F03;
033700140310             wFine = *on;
033800140310
033900140310           // -?F6=Conferma?
034000140310           WHEN  dsp_aid = c_F06;
034100140311             exsr CallCARICA;
034200140310             clear ErrGenerico;
034300140310             clear ErrMessage;
034400140310             wInzS03 = *on;
034500140310             wVideo  = 'S3';
034600140310
034700140310         ENDSL;
034800140310
034900140310       ENDSR;
035000140311
035100140311       //--------------------------------------------------------------
035200140311       //?Richiamo FIORB00R                                       ?
035300140311       //--------------------------------------------------------------
035400140311       BEGSR CallCARICA;
035500140311
035600140311         prmRqsOpCode = FIORB00_RQSOPCODE_CARICA;
035700140311         prmRqsFormato = fiorb0i0.formato;
035800140710         prmRqsData    = fiorb0i0;
035900140311         prmRqsDataSize = %size(fiorb0i0);
036000140311         prmRpyFormato = fiorb0o0.formato;
036100140710         prmRpyData    = fiorb0o0;
036200140311         prmRpyDataSize = %size(fiorb0o0);
036300140311
036400140311         // -?Richiamo FIORB00R?
036500140311         fiorb00_Anagrafica (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
036600140710                          prmRqsFormato:FIORB0I0:prmRqsDataSize:
036700140710                          prmRpyFormato:FIORB0O0:prmRpyDataSize);
036800140311
036900140311       ENDSR;
037000140310
037100131217       //--------------------------------------------------------------
037200140310       //?Gestione videata S03                                         ?
037300131217       //--------------------------------------------------------------
037400140310       BEGSR GesS03;
037500131217
037600131217         // -?Inizializzazione videata?
037700140310         IF  wInzS03;
037800140310           exsr InzS03;
037900140310           wInzS03 = *off;
038000131217         ENDIF;
038100131217
038200131217       //?Visualizzazione del SFL (se ci sono dati)
038300140310         SflDsp = (S03nrr <= *zeros);
038400131217
038500131217       //?Emissione Testata e Piede con tasti funzionali abilitati
038600140310         write ORB0T1;
038700140310         write ORB0P3;
038800131217
038900131217       //?Emissione videata
039000140310         exfmt ORB0C3;
039100131217         reset ErrMessage;
039200131217         reset ErrGenerico;
039300131217
039400131217         SELECT;
039500140311
039600140311           // -?F05=Avanti?
039700140311           WHEN  dsp_aid = c_F05;
039800140311             exsr CallCARICA;
039900140311             clear ErrGenerico;
040000140311             clear ErrMessage;
040100140311             wInzS03 = *on;
040200140311             wVideo  = 'S3';
040300131217
040400131217           // -?F12=Ritorno?
040500131217           WHEN  dsp_aid = c_F12;
040600140310             wVideo = 'D1';
040700140310             exsr GesD01;
040800131217
040900131217         ENDSL;
041000131217
041100131217       ENDSR;
041200131217
041300131217       //--------------------------------------------------------------
041400140310       //?Inizializzazione videata S03
041500131217       //--------------------------------------------------------------
041600140310       BEGSR InzS03;
041700131217
041800131217       //?Pulizia subfile
041900140310         exsr PulS03;
042000131217
042100131217       //?Carico i dati del control
042200140310         exsr CarC03;
042300131217
042400131217       //?Caricamento subfile
042500140310         exsr Ries03;
042600131217
042700140310         SflEnd = *on;
042800131217
042900131217       ENDSR;
043000131217
043100131217       //--------------------------------------------------------------
043200140310       //?Pulizia SFL03.
043300131217       //--------------------------------------------------------------
043400140310       BEGSR PulS03;
043500131217
043600131217       //?Pulizia subfile
043700140310         SflDsp    = *on;
043800140310         SflDspCtl = *on;
043900140310         write  ORB0C3;
044000140310         SflDspCtl = *off;
044100140310         SflEnd    = *off;
044200131217
044300140310         clear S03nrr;
044400131217
044500131217         ErrMessage  = *off;
044600131217         ErrGenerico = *off;
044700131217
044800131217       ENDSR;
044900131217
045000131217       //--------------------------------------------------------------
045100140310       //?Caricamento C03.
045200131217       //--------------------------------------------------------------
045300140310       BEGSR CarC03;
045400131217
045500140310         formato3 = fiorb0o0.formato;
045600140310         versione3 = fiorb0o0.versione;
045700140311         clientito3 = fiorb0o0.clientitot;
045800140310         nrrec = fiorb0o0.nrrec;
045900131217
046000131217       ENDSR;
046100131217
046200131217       //--------------------------------------------------------------
046300140310       //?Caricamento S03.
046400131217       //--------------------------------------------------------------
046500140310       BEGSR RieS03;
046600131217
046700131217       //?LOOP x nr. messaggi
046800140310         FOR xx = 1 to fiorb0o0.nrrec;
046900140310           clear riga;
047000140310           riga = %subst(fiorb0o0.sknome(xx):1:20) + ' ' +
047100140310                  %subst(fiorb0o0.skindirizzo(xx):1:20) + ' ' +
047200140310                  %subst(fiorb0o0.sklocalita(xx):1:20) + ' ' +
047300140310                  fiorb0o0.skprovincia(xx) + ' ' +
047400140310                  fiorb0o0.skcodice(xx);
047500140310           s03nrr += 1;
047600140310           write ORB0S3;
047700131217         ENDFOR;
047800131217
047900131217       ENDSR;
048000131108
048100131108      /end-free
