000100140218      //--------------------------------------------------------------
000200140218      //?TNTA39R - INTERROGAZIONE Prezzo Medio Fuel
000300140218      //--------------------------------------------------------------
000400140218
000500140218     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140218     h dftactgrp(*no) actgrp(*caller)
000700140218
000800140218      //---------------------------------------------------------------
000900140218      //?Dichiarazione file.
001000140218      //---------------------------------------------------------------
001100140218
001200140218      // - Video
001300140218     fTNTA39D   cf   e             workstn
001400140218     f                                     sfile(TA39S01 : S01nrr)
001500140218     f                                     indds(IndDspF)
001600140218     f                                     infds(InfDspF)
001700140218
001800140218      //---------------------------------------------------------------
001900140218      //?Definizione costanti.
002000140218      //---------------------------------------------------------------
002100140218
002200140218      // - Tasti funzionali a video
002300140218     d c_F01           c                   const(x'31')
002400140218     d c_F02           c                   const(x'32')
002500140218     d c_F03           c                   const(x'33')
002600140218     d c_F04           c                   const(x'34')
002700140218     d c_F05           c                   const(x'35')
002800140218     d c_F06           c                   const(x'36')
002900140218     d c_F07           c                   const(x'37')
003000140218     d c_F08           c                   const(x'38')
003100140218     d c_F09           c                   const(x'39')
003200140218     d c_F10           c                   const(x'3A')
003300140218     d c_F11           c                   const(x'3B')
003400140218     d c_F12           c                   const(x'3C')
003500140218     d c_F13           c                   const(x'B1')
003600140218     d c_F14           c                   const(x'B2')
003700140218     d c_F15           c                   const(x'B3')
003800140218     d c_F16           c                   const(x'B4')
003900140218     d c_F17           c                   const(x'B5')
004000140218     d c_F18           c                   const(x'B6')
004100140218     d c_F19           c                   const(x'B7')
004200140218     d c_F20           c                   const(x'B8')
004300140218     d c_F21           c                   const(x'B9')
004400140218     d c_F22           c                   const(x'BA')
004500140218     d c_F23           c                   const(x'BB')
004600140218     d c_F24           c                   const(x'BC')
004700140218     d c_Enter         c                   const(x'F1')
004800140218     d c_RollDown      c                   const(x'F4')
004900140218     d c_RollUp        c                   const(x'F5')
005000140218
005100140218     d Digits          c                   const('0123456789')
005200140218
005300140218      //---------------------------------------------------------------
005400140218      //?Definizione schiere.
005500140218      //---------------------------------------------------------------
005600140218
005700140218      // - Messaggi di errore
005800140218     d wMsg            s             78    dim(20) ctdata perrcd(1)
005900140218
006000140218      //---------------------------------------------------------------
006100140218      //?Definizione aree dati.
006200140218      //---------------------------------------------------------------
006300140218
006400140218      // - Dati utente
006500140218     d �AzUte        e ds                  extname(AZUTE00F)
006600140218     d                                     dtaara
006700140218     d �DatiUte      e ds                  extname(dDatiUte)
006800140218     d                                     dtaara
006900140218
007000140218      //---------------------------------------------------------------
007100140218      //?Definizione strutture dati.
007200140218      //---------------------------------------------------------------
007300140218
007400140218      // - Status
007500140218     d Psds           sds
007600140218     d   SDSpgm          *proc
007700140218
007800140218      // - InfDS
007900140218     d InfDspF         ds
008000140218     d  dsp_aid              369    369a
008100140218     d  sfl_rrn              376    377i 0
008200140218     d  min_nrr              378    379i 0
008300140218     d  num_rcds             380    381i 0
008400140218
008500140218      // - Indicatori su DspF
008600140218     d IndDspF         ds
008700140218        // - Indicatori di errore in videata
008800140218     d  ErrMessage                    1n   overlay(IndDspF : 28)
008900140218        // - Indicatori di gestione del subfile
009000140218     d  SflDsp                        1n   overlay(IndDspF : 30)
009100140218     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009200140218     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009300140218     d  SflEnd                        1n   overlay(IndDspF : 33)
009400140218        // - Indicatori di errore
009500140218     d  PosCurOpz                     1n   overlay(IndDspF : 50)
009600140218        // - Indicatori di errore generico
009700140218     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800140218
009900140218     d WindDspF        ds                  inz
010000140218     d   WindDspFa             1     49    inz(*zero)
010100140218     d   WindDspFb            50     99    inz(*zero)
010200140218
010300140218      // - Parametri ricevuti
010400140218     d KPJBA         e ds
010500140218     d TNTA39DS      e ds
010600140218
010700140218      // - File Prezzo medio Gasolio
010800140218     d TIPMG00F      e ds                  prefix(f)
010900140218
011000140218      //---------------------------------------------------------------
011100140218      //?Definizione variabili globali.
011200140218      //---------------------------------------------------------------
011300140218
011400140218      // - Flags booleani
011500140218     d wEndS01         s               n   inz(*off)
011600140218     d wFine           s               n   inz(*off)
011700140218     d wInzS01         s               n   inz(*on)
011800140218     d wRecOK          s               n   inz(*off)
011900140218
012000140218      // - Indici di schiera
012100140218     d xx              s              4  0 inz
012200140218
012300140218      // - Campi associati al video
012400140218     d wVideo          s              2    inz('S1')
012500140218     d S01nrr          s              4  0 inz
012600140218
012700140218      // - Campi di comodo data
012800140218     d wDate_EUR       s               d   datfmt(*eur)
012900140218     d wDate_ISO       s               d   datfmt(*iso)
013000140218
013100140218      // - Campi di comodo
013200140218     d wOggi           s              8  0 inz
013300140218
013400140218      //---------------------------------------------------------------
013500140218      //?Definizione procedure usate.
013600140218      //---------------------------------------------------------------
013700140218
013800140218      //---------------------------------------------------------------
013900140218      //?Prototipi.
014000140218      //---------------------------------------------------------------
014100140218
014200140218      //---------------------------------------------------------------
014300140218      //?Definizione key-list.
014400140218      //---------------------------------------------------------------
014500140218
014600140218      //---------------------------------------------------------------
014700140218      //?Riepilogo indicatori.
014800140218      //---------------------------------------------------------------
014900140218      // 28    : Emissione messaggio di errore a video
015000140218      // 30    : Condiziona SFLDSP
015100140218      // 31    : Condiziona SFLDSPCTL
015200140218      // 30+31 : Condiziona SFLCLR
015300140218      // 32    : Condiziona SFLNXTCHG
015400140218      // 33    : Condiziona SFLEND
015500140218      // 50    : Errore: Opzione
015600140218      // 99    : Generico di Errore
015700140218      //---------------------------------------------------------------
015800140218
015900140218      //---------------------------------------------------------------
016000140218      //?M A I N - L I N E
016100140218      //---------------------------------------------------------------
016200140218
016300140218     c     *Entry        plist
016400140218     c                   parm                    kpjba
016500140218     c                   parm                    tnta39ds
016600140218
016700140218      /free
016800140218
016900140218       //?Operazioni iniziali
017000140218       exsr RoutInz;
017100140218
017200140218       //?Gestione video
017300140218       DOW wFine = *off;
017400140218         SELECT;
017500140218           WHEN wVideo = 'S1';
017600140218             exsr GesS01;
017700140218           OTHER;
017800140218             wFine = *on;
017900140218         ENDSL;
018000140218       ENDDO;
018100140218
018200140218       //?Operazioni finali
018300140218       exsr RoutEnd;
018400140218
018500140218       //--------------------------------------------------------------
018600140218       //?Operazioni iniziali.
018700140218       //--------------------------------------------------------------
018800140218       BEGSR RoutInz;
018900140218
019000140218       //?Pulisco campi output
019100140218         clear OTA39sca;
019200140218         clear OTA39pmg;
019300140218         clear OTA39dtd;
019400140218         clear OTA39f03;
019500140218
019600140218         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019700140218
019800140218       ENDSR;
019900140218
020000140218       //--------------------------------------------------------------
020100140218       //?Gestione videata S01.
020200140218       //--------------------------------------------------------------
020300140218       BEGSR GesS01;
020400140218
020500140218       //?Inizializzazione videata
020600140218         IF  wInzS01;
020700140218           exsr InzS01;
020800140218           wInzS01 = *off;
020900140218         ENDIF;
021000140218
021100140218       //?Emissione Testata e Piede con tasti funzionali abilitati
021200140218         write  TA39T01;
021300140218         write  TA39P01;
021400140218
021500140218       //?Emissione videata
021600140218         exfmt  TA39C01;
021700140218         reset ErrMessage;
021800140218         reset ErrGenerico;
021900140218         clear VC1msg;
022000140218
022100140218         SELECT;
022200140218
022300140218       //?- F03=Fine
022400140218           WHEN  dsp_aid = c_F03;
022500140218             exsr F03S01;
022600140218
022700140218       //?Invio
022800140218           OTHER;
022900140224             exsr OpzS01;
023000140224             IF  ErrGenerico;
023100140224               leavesr;
023200140224             ENDIF;
023300140218
023400140218         ENDSL;
023500140218
023600140218       ENDSR;
023700140218
023800140218       //--------------------------------------------------------------
023900140218       //?Inizializzazione videata S01.
024000140218       //--------------------------------------------------------------
024100140218       BEGSR InzS01;
024200140218
024300140218         wEndS01 = *off;
024400140218
024500140218       //?Pulizia subfile
024600140218         exsr PulS01;
024700140218
024800140218       //?Caricamento subfile
024900140218         exsr Ries01;
025000140224
025100140224       //?Visualizzazione del SFL (se ci sono dati)
025200140224         SflDsp = (S01nrr <= *zeros);
025300140224
025400140224       //?Attivazione SFLEND
025500140224         SflEnd = *on;
025600140224
025700140224       //?Posizionamento cursore al 1� rcd della pagina
025800140224         IF  S01nrr > *zero;
025900140224           VC1rcd = 1;
026000140224         ELSE;
026100140224           clear VC1rcd;
026200140224         ENDIF;
026300140224
026400140224         VC1csr = VC1rcd;
026500140218
026600140218       ENDSR;
026700140218
026800140218       //--------------------------------------------------------------
026900140218       //?Pulizia SFL01.
027000140218       //--------------------------------------------------------------
027100140218       BEGSR PulS01;
027200140218
027300140218       //?Pulizia subfile
027400140218         SflDsp    = *on;
027500140218         SflDspCtl = *on;
027600140218         write  TA39C01;
027700140218         SflDspCtl = *off;
027800140218         SflEnd    = *off;
027900140218
028000140218         clear VC1rcd;
028100140218         clear VC1csr;
028200140218         clear S01nrr;
028300140218         clear VC1msg;
028400140218
028500140218         ErrMessage  = *off;
028600140218         ErrGenerico = *off;
028700140218
028800140218         WindDspF = IndDspF;
028900140218         reset WindDspFb;
029000140218         IndDspF  = WindDspF;
029100140218
029200140218       ENDSR ;
029300140218
029400140218       //--------------------------------------------------------------
029500140218       //?Caricamento S01.
029600140218       //--------------------------------------------------------------
029700140218       BEGSR RieS01;
029800140218
029900140218       //?Leggo il file del prezzo medio fuel
030000140218         exec sql
030100140218         DECLARE PMG CURSOR FOR
030200140218         WITH   A1 as ( select PMGsca, PMGpmg, PMGdtd
030300140218                          from TIPMG00F
030400140218                         order by PMGsca, PMGpmg desc, PMGdtd desc ),
030500140218                A2 as ( select PMGsca, max(PMGpmg) as PMGpmg
030600140218                          from A1
030700140218                         group by PMGsca
030800140218                         order by PMGsca )
030900140218         SELECT distinct A2.*, max(A1.PMGdtd)
031000140218           from A2 inner join A1
031100140218             on A2.PMGsca = A1.PMGsca
031200140218            and A2.PMGpmg = A1.PMGpmg
031300140218          group by A2.PMGsca, A2.PMGpmg
031400140218          order by A2.PMGsca;
031500140218
031600140218         exec sql OPEN PMG;
031700140218
031800140218         DOW not wEndS01;
031900140218
032000140218           exec sql FETCH next from PMG into :fPMGsca, :fPMGpmg,
032100140218                                             :fPMGdtd;
032200140218         //?fine file esco
032300140218           IF  sqlcod = 100 or sqlcod < 0;
032400140218             wEndS01 = *on;
032500140218             leave;
032600140218           ENDIF;
032700140218
032800140218           PosCurOpz = *off;
032900140218
033000140218        //?Imposto i campi del subfile
033100140218           VS1sca = fPMGsca;
033200140218           VS1pmg = fPMGpmg;
033300140218           VS1dtd = fPMGdtd;
033400140218
033500140218           S01nrr += 1;
033600140218           write  TA39S01;
033700140218
033800140218         ENDDO;
033900140218
034000140218         exec sql CLOSE PMG;
034100100622
034200100622       ENDSR;
034300100720
034400140218       //--------------------------------------------------------------
034500140218       //?Gestione tasto funzionale F03 da videata S01
034600140218       //?F03=Fine
034700140218       //--------------------------------------------------------------
034800140218       BEGSR F03S01;
034900140218
035000140218         // Chiusura del programma
035100140218         OTA39F03 = '1';
035200140218         wFine = *on;
035300140218
035400140218       ENDSR;
035500140218
035600140218       //--------------------------------------------------------------
035700140218       //?Gestione opzioni subfile
035800140218       //--------------------------------------------------------------
035900140218       BEGSR OpzS01;
036000140218
036100140218         readc TA39S01;
036200140218
036300140218         DOW  NOT  %eof(TNTA39D);
036400140218
036500140218           SflNxtChg = *off;
036600140218           WindDspF  = IndDspF;
036700140218           reset WindDspFb;
036800140218           IndDspF   = WindDspF;
036900140218           VC1rcd    = S01nrr;
037000140218
037100140218           PosCurOpz = *off;
037200140218
037300140218           SELECT;
037400140218
037500140218             //?- Nessuna opzione
037600140218             WHEN  VS1opz  = *blank;
037700140218
037800140218             //?- 1 = Scelta
037900140218             WHEN  VS1opz  = '1';
038000140218               //?passo i dati al chiamante ed esco dal pgm
038100140218               OTA39sca = VS1sca;
038200140218               OTA39pmg = VS1pmg;
038300140218               OTA39dtd = VS1dtd;
038400140218               wFine    = *on;
038500140218               leavesr;
038600140218
038700140218             //?- ? = Opzione NON valida
038800140218             OTHER;
038900140218               ErrMessage  = *on;
039000140218               ErrGenerico = *on;
039100140218               PosCurOpz   = *on;
039200140218               VC1msg = wMsg(01);
039300140218
039400140218           ENDSL;
039500140218
039600140218           //?Aggiornamento sfl
039700140218           SELECT;
039800140218             WHEN  ErrMessage;
039900140218               SflNxtChg = *on;
040000140218               VC1csr    = VC1rcd;
040100140218             WHEN  ErrGenerico;
040200140218               VC1csr    = VC1rcd;
040300140218               clear  VS1opz;
040400140218             OTHER;
040500140218               VC1csr    = VC1rcd;
040600140218               clear  VS1opz;
040700140218           ENDSL;
040800140218
040900140218           update TA39S01;
041000140218
041100140218           IF  ErrMessage or ErrGenerico;
041200140218             leave;
041300140218           ENDIF;
041400140218
041500140218           readc TA39S01;
041600140218
041700140218         ENDDO;
041800140218
041900140218       ENDSR;
042000140218
042100140218       //--------------------------------------------------------------
042200140218       //?Operazioni finali.
042300140218       //--------------------------------------------------------------
042400140218       BEGSR RoutEnd;
042500140218
042600140218         *inLR = *on;
042700140218         return;
042800140218
042900140218       ENDSR;
043000140218
043100140218      /end-free
043200140218
043300140218       //--------------------------------------------------------------
043400140218       //?Schiere a tempo di compilazione.
043500140218       //--------------------------------------------------------------
043600140218
043700140218** - $MSG -------------------------------------------------------------------*
043800140218Opzione errata                                                                 1
