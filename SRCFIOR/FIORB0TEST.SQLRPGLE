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
001100140710     fFIORB0TSTDcf   e             workstn
001200140710     f                                     sfile(ORB0S3:S03nrr)
001300140710     f                                     indds(IndDspF)
001400140710     f                                     infds(InfDspF)
001500140306
001600140306       //--------------------------------------------------------------
001700140306       //?Definizione costanti.                                        ?
001800140306       //--------------------------------------------------------------
001900140307      /copy gaitrasrc/srcconst,FIORB00R
002000140710
002100140710       // -?Tasti funzionali a video?
002200140710     d c_F01           c                   const(x'31')
002300140710     d c_F02           c                   const(x'32')
002400140710     d c_F03           c                   const(x'33')
002500140710     d c_F04           c                   const(x'34')
002600140710     d c_F05           c                   const(x'35')
002700140710     d c_F06           c                   const(x'36')
002800140710     d c_F07           c                   const(x'37')
002900140710     d c_F08           c                   const(x'38')
003000140710     d c_F09           c                   const(x'39')
003100140710     d c_F10           c                   const(x'3A')
003200140710     d c_F11           c                   const(x'3B')
003300140710     d c_F12           c                   const(x'3C')
003400140710     d c_F13           c                   const(x'B1')
003500140710     d c_F14           c                   const(x'B2')
003600140710     d c_F15           c                   const(x'B3')
003700140710     d c_F16           c                   const(x'B4')
003800140710     d c_F17           c                   const(x'B5')
003900140710     d c_F18           c                   const(x'B6')
004000140710     d c_F19           c                   const(x'B7')
004100140710     d c_F20           c                   const(x'B8')
004200140710     d c_F21           c                   const(x'B9')
004300140710     d c_F22           c                   const(x'BA')
004400140710     d c_F23           c                   const(x'BB')
004500140710     d c_F24           c                   const(x'BC')
004600140710     d c_Enter         c                   const(x'F1')
004700140710     d c_RollDown      c                   const(x'F4')
004800140710     d c_RollUp        c                   const(x'F5')
004900140306
005000140306       //--------------------------------------------------------------
005100140306       //?Definizione strutture dati.                                  ?
005200140306       //--------------------------------------------------------------
005300140710       // -?InfDS?
005400140710     d InfDspF         ds
005500140710     d  dsp_aid              369    369a
005600140710
005700140710       // -?Indicatori su DspF?
005800140710     d IndDspF         ds
005900140710         // -?Indicatori di gestione subfile?
006000140710     d  SflDsp                        1n   overlay(IndDspF : 30)
006100140710     d  SflDspCtl                     1n   overlay(IndDspF : 31)
006200140710     d  SflEnd                        1n   overlay(IndDspF : 32)
006300140710         // -?Emissione messaggio di errore?
006400140710     d   ErrMessage                    n   overlay(IndDspF : 28)
006500140710         // -?Riemissione videata?
006600140710     d   ErrGenerico                   n   overlay(IndDspF : 99)
006700140710
006800140306       // -?Dati INPUT
006900140307     d FIORB0I0      e ds                  QUALIFIED INZ(*EXTDFT)
007000140306
007100140306       // -?Dati OUTPUT
007200140307     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
007300140307     d  skCodice                     10a   dim(10) overlay(Codice)
007400140307     d  skNome                       48a   dim(10) overlay(Nome)
007500140307     d  skIndirizzo                  35a   dim(10) overlay(Indirizzo)
007600140307     d  skCap                         9a   dim(10) overlay(Cap)
007700140307     d  skLocalita                   35a   dim(10) overlay(Localita)
007800140307     d  skProvincia                   2a   dim(10) overlay(Provincia)
007900140307     d  skNazione                     3a   dim(10) overlay(Nazione)
008000140306
008100140306       //--------------------------------------------------------------
008200140306       //?Definizione schiere.
008300140306       //--------------------------------------------------------------
008400140306
008500140306       //--------------------------------------------------------------
008600140306       //?Definizione campi.
008700140306       //--------------------------------------------------------------
008800140710
008900140710       // -?Flags booleani?
009000140710     d wFine           s               n   inz(*off)
009100140710     d wInzS03         s               n   inz(*on)
009200140710
009300140710       // -?Campi associati al video
009400140710     d S03nrr          s              4  0 inz
009500140710
009600140710       // -?Comodo x campi dati
009700140710     d wData           s              8  0
009800140710     d wData_eur       s               d   datfmt(*eur)
009900140710     d wData_iso       s               d   datfmt(*iso)
010000140710
010100140710       // -?Indici di schiera
010200140710     d xx              s              4  0 inz
010300140306
010400140306       //--------------------------------------------------------------
010500140306       //?Definizione procedure.
010600140306       //--------------------------------------------------------------
010700140306
010800140306       //--------------------------------------------------------------
010900140306       //?Definizione prototipi.
011000140306       //--------------------------------------------------------------
011100140314      /copy gaitrasrc/srcprotopr,FIORB00R
011200140306
011300140306       //--------------------------------------------------------------
011400140306       //?Definizione parametri programma.
011500140306       //--------------------------------------------------------------
011600140314     d prmRqsOpCode...
011700140314     d                 s             10I 0
011800140314     d prmRpyOpCode...
011900140314     d                 s             10I 0
012000140314     d prmRpyIdMsg...
012100140314     d                 s             10I 0
012200140314     d prmRqsFormato...
012300140314     d                 s             10A
012400140314     d prmRqsData...
012500140314     d                 s          32767A   varying
012600140314     d prmRqsDataSize...
012700140314     d                 s             10I 0
012800140314     d prmRpyFormato...
012900140314     d                 s             10A
013000140314     d prmRpyData...
013100140314     d                 s          32767A   varying
013200140314     d prmRpyDataSize...
013300140314     d                 s             10I 0
013400140306
013500140306       //--------------------------------------------------------------
013600140306       //?MAIN.
013700140306       //--------------------------------------------------------------
013800140306
013900140306      /free
014000140306
014100140306       //?Operazioni iniziali
014200140306       exsr RoutInz;
014300140307
014400140307       //?Elabora
014500140312       exsr Elabora;
014600140710
014700140710       //?Emette il video coi dati di ritorno da FIORB00R
014800140710       exsr Video;
014900060612
015000140306       //?Operazioni finali
015100140306       exsr RoutEnd;
015200140306
015300140306       //--------------------------------------------------------------
015400140306       //?Operazioni iniziali.                                         ?
015500140306       //--------------------------------------------------------------
015600140306       BEGSR  RoutInz;
015700140306
015800140307         prmRpyOpCode = FIORB00_RPYOPCODE_DONE;
015900140307         prmRpyIdMsg  = FIORB00_ESITO_OK;
016000140306
016100140306       ENDSR;
016200140307
016300140307       //--------------------------------------------------------------
016400140307       //?Elabora.
016500140307       //--------------------------------------------------------------
016600140307       BEGSR  Elabora;
016700140312
016800140314         prmRqsOpCode = FIORB00_RQSOPCODE_LEGGI;
016900140314         prmRqsFormato = fiorb0i0.formato;
017000140314         prmRqsDataSize = %size(fiorb0i0);
017100140314         prmRpyFormato = fiorb0o0.formato;
017200140314         prmRpyDataSize = %size(fiorb0o0);
017300140314
017400140709         fiorb0i0.codiceksu  = '00321472';
017500140314         fiorb0i0.ordina     = fiorb00_ordina_loc;
017600140314         fiorb0i0.escluditcr = fiorb00_escludi_mai;
017700140314
017800140314         // -?Richiamo FIORB00R?
017900140314         fiorb00_Anagrafica (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
018000140314                          prmRqsFormato:FIORB0I0:prmRqsDataSize:
018100140314                          prmRpyFormato:FIORB0O0:prmRpyDataSize);
018200140307
018300140307       ENDSR;
018400140710
018500140710       //--------------------------------------------------------------
018600140710       //?Emissione video.
018700140710       //--------------------------------------------------------------
018800140710       BEGSR  Video;
018900140710
019000140710         // -?Inizializzazione videata?
019100140710         IF  wInzS03;
019200140710           exsr InzS03;
019300140710           wInzS03 = *off;
019400140710         ENDIF;
019500140710
019600140710       //?Visualizzazione del SFL (se ci sono dati)
019700140710         SflDsp = (S03nrr <= *zeros);
019800140710
019900140710       //?Emissione Testata e Piede con tasti funzionali abilitati
020000140710         write ORB0T1;
020100140710         write ORB0P3;
020200140710
020300140710       //?Emissione videata
020400140710         exfmt ORB0C3;
020500140710         reset ErrMessage;
020600140710         reset ErrGenerico;
020700140710
020800140710         SELECT;
020900140710
021000140710           // -?F12=Ritorno?
021100140710           WHEN  dsp_aid = c_F12;
021200140710             exsr RoutEnd;
021300140710
021400140710         ENDSL;
021500140710
021600140710       ENDSR;
021700140710
021800140710       //--------------------------------------------------------------
021900140710       //?Inizializzazione videata S03
022000140710       //--------------------------------------------------------------
022100140710       BEGSR InzS03;
022200140710
022300140710       //?Pulizia subfile
022400140710         exsr PulS03;
022500140710
022600140710       //?Carico i dati del control
022700140710         exsr CarC03;
022800140710
022900140710       //?Caricamento subfile
023000140710         exsr Ries03;
023100140710
023200140710         SflEnd = *on;
023300140710
023400140710       ENDSR;
023500140710
023600140710       //--------------------------------------------------------------
023700140710       //?Pulizia SFL03.
023800140710       //--------------------------------------------------------------
023900140710       BEGSR PulS03;
024000140710
024100140710       //?Pulizia subfile
024200140710         SflDsp    = *on;
024300140710         SflDspCtl = *on;
024400140710         write  ORB0C3;
024500140710         SflDspCtl = *off;
024600140710         SflEnd    = *off;
024700140710
024800140710         clear S03nrr;
024900140710
025000140710         ErrMessage  = *off;
025100140710         ErrGenerico = *off;
025200140710
025300140710       ENDSR;
025400140710
025500140710       //--------------------------------------------------------------
025600140710       //?Caricamento C03.
025700140710       //--------------------------------------------------------------
025800140710       BEGSR CarC03;
025900140710
026000140710         formato3 = fiorb0o0.formato;
026100140710         versione3 = fiorb0o0.versione;
026200140710         clientito3 = fiorb0o0.clientitot;
026300140710         nrrec = fiorb0o0.nrrec;
026400140710
026500140710       ENDSR;
026600140710
026700140710       //--------------------------------------------------------------
026800140710       //?Caricamento S03.
026900140710       //--------------------------------------------------------------
027000140710       BEGSR RieS03;
027100140710
027200140710       //?LOOP x nr. messaggi
027300140710         FOR xx = 1 to fiorb0o0.nrrec;
027400140710           clear riga;
027500140710           riga = %subst(fiorb0o0.sknome(xx):1:20) + ' ' +
027600140710                  %subst(fiorb0o0.skindirizzo(xx):1:20) + ' ' +
027700140710                  %subst(fiorb0o0.sklocalita(xx):1:20) + ' ' +
027800140710                  fiorb0o0.skprovincia(xx) + ' ' +
027900140710                  fiorb0o0.skcodice(xx);
028000140710           s03nrr += 1;
028100140710           write ORB0S3;
028200140710         ENDFOR;
028300140710
028400140710       ENDSR;
028500140306
028600140306       //--------------------------------------------------------------
028700140306       //?Operazioni finali.
028800140306       //--------------------------------------------------------------
028900140306       BEGSR  RoutEnd;
029000140311
029100140314         *inLR = *on;
029200140306
029300140306         return;
029400140306
029500140306       ENDSR;
029600140306
029700140306      /end-free
