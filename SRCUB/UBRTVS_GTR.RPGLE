000100090128      *PARMS dftactgrp(*no) actgrp('QILE') option(*noxref)
000200090128       //--------------------------------------------------------------
000300100622       //?TEST - RITORNO FILIALI ABILITATE
000400090128       //--------------------------------------------------------------
000500090128
000600090128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090128     h dftactgrp(*no) actgrp('QILE')
000800090128
000900090128       //--------------------------------------------------------------
001000090128       //?Dichiarazione file.
001100090128       //--------------------------------------------------------------
001200090128
001300090128       // - Video
001400111219     fubrtvs_gTDcf   e             workstn
001500090128     f                                     indds(IndDspF)
001600090128     f                                     infds(InfDspF)
001700090128
001800090128       //--------------------------------------------------------------
001900090128       //?Definizione costanti.
002000090128       //--------------------------------------------------------------
002100090128
002200090128       // - Tasti funzionali a video
002300090128     d c_F01           c                   const(x'31')
002400090128     d c_F02           c                   const(x'32')
002500090128     d c_F03           c                   const(x'33')
002600090128     d c_F04           c                   const(x'34')
002700090128     d c_F05           c                   const(x'35')
002800090128     d c_F06           c                   const(x'36')
002900090128     d c_F07           c                   const(x'37')
003000090128     d c_F08           c                   const(x'38')
003100090128     d c_F09           c                   const(x'39')
003200090128     d c_F10           c                   const(x'3A')
003300090128     d c_F11           c                   const(x'3B')
003400090128     d c_F12           c                   const(x'3C')
003500090128     d c_F13           c                   const(x'B1')
003600090128     d c_F14           c                   const(x'B2')
003700090128     d c_F15           c                   const(x'B3')
003800090128     d c_F16           c                   const(x'B4')
003900090128     d c_F17           c                   const(x'B5')
004000090128     d c_F18           c                   const(x'B6')
004100090128     d c_F19           c                   const(x'B7')
004200090128     d c_F20           c                   const(x'B8')
004300090128     d c_F21           c                   const(x'B9')
004400090128     d c_F22           c                   const(x'BA')
004500090128     d c_F23           c                   const(x'BB')
004600090128     d c_F24           c                   const(x'BC')
004700090128     d c_Enter         c                   const(x'F1')
004800090128     d c_RollDown      c                   const(x'F4')
004900090128     d c_RollUp        c                   const(x'F5')
005000090128
005100090128       //--------------------------------------------------------------
005200090128       //?Definizione schiere.
005300090128       //--------------------------------------------------------------
005400090128
005500090128
005600090128       //--------------------------------------------------------------
005700090128       //?Definizione aree dati.
005800090128       //--------------------------------------------------------------
005900090128
006000090128       // - Dati utente
006100090128     d �AzUte        e ds                  extname(AZUTE00F)
006200090128     d                                     dtaara
006300090128     d �DatiUte      e ds                  extname(dDatiUte)
006400090128     d                                     dtaara
006500090128
006600090128       //--------------------------------------------------------------
006700090128       //?Definizione strutture dati.
006800090128       //--------------------------------------------------------------
006900090128
007000090128       // - Status ds
007100090128     d Status         sds
007200090128     d  SDSpgm           *proc
007300090128     d  SDSprm           *parms
007400090128     d  SDSusr               254    263
007500090128
007600090128       // - InfDS
007700090128     d InfDspF         ds
007800090128     d  dsp_aid              369    369a
007900090128
008000090128       // - Indicatori su DspF
008100090128     d IndDspF         ds
008200090128        // - Indicatori di errore
008300090128     d  ErrMessage                    1n   overlay(IndDspF : 28)
008400090128     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008500090128
008600090128       // - Parametri x Controllo profilo utenti
008700111219     D TIBS34ds      e ds                  qualified
008800090128
008900111219     D     TITAS_DS  E DS                  extname(TITAS30C) qualified
009000111219     D     FNARB_DS  E DS                  extname(FNARB00F) qualified
009100111219     D     FNBLP_DS  E DS                  extname(FNBLP00F) qualified
009200100622
009300100622     d kpjba         e ds
009400090128
009500090128       //--------------------------------------------------------------
009600090128       //?Definizione variabili globali.
009700090128       //--------------------------------------------------------------
009800111219     D*-----------
009900111219     D* PARAMETRI Comuni
010000111219     D*-----------
010100111219     D pAnno           s                   like(VIDAnno)
010200111219     D pLineaPar       s                   like(VIDLinea)
010300111219     D pSerie          s                   like(VIDSerie)
010400111219     D pNumSped        s                   like(VIDNrSped)
010500111219     D*-----------
010600111219     D* PARAMETRI Spedizione
010700111219     D*-----------
010800111219     D pEsito          s              1A
010900090128
011000090128       // - Flags booleani
011100090128     d $Fine           s               n   inz(*off)
011200090128     d $InzD01         s               n   inz(*on)
011300090128
011400090128       //--------------------------------------------------------------
011500090128       //?Definizione procedure usate.
011600090128       //--------------------------------------------------------------
011700111219
011800111219     D UBRTVSPE        pr                  ExtPgm('UBRTVSPE')
011900111219     D*-----------
012000111219     D* PARAMETRI Comuni
012100111219     D*-----------
012200111219     D                                4p 0
012300111219     D                                3p 0
012400111219     D                                2p 0
012500111219     D                                7p 0
012600111219     D  FNARB_DS                           likeds(FNARB_DS)
012700111219     D  FNBLP_DS                           likeds(FNBLP_DS)
012800111219     D  TITAS_DS                           likeds(TITAS_DS)
012900111219     D                                1A
013000090128
013100090128       // - Reperimento dati utente
013200090304      /copy GAITRASRC/SRCPROTOPR,TIBS34R
013300111219     D*TIBS34R         pr                  extpgm('TIBS34R')
013400111219     D* TIBS34DS                           likeds(tibs34ds)
013500111219
013600090128       //--------------------------------------------------------------
013700090128       //?Definizione key-list.
013800090128       //--------------------------------------------------------------
013900090128
014000090128
014100090128       //--------------------------------------------------------------
014200090128       //?Riepilogo indicatori.
014300090128       //--------------------------------------------------------------
014400090128
014500090128
014600090128       //--------------------------------------------------------------
014700090128       //?M A I N - L I N E
014800090128       //--------------------------------------------------------------
014900090128
015000090128      /free
015100090128
015200090128       // - Start
015300090128       *inLR = *on;
015400090128
015500090128       // - Elab
015600090128       dow  $Fine = *off;
015700090128         exsr sr_GesD01;
015800090128       enddo;
015900090128
016000090128       // - End
016100090128       return;
016200090128
016300090128       //--------------------------------------------------------------
016400090128       //?Operazioni iniziali
016500090128       //--------------------------------------------------------------
016600090128       BEGSR *InzSR;
016700090128
016800090128         // Reperimento dati utente
016900090128         exsr sr_DatiJob;
017000090128
017100090128       ENDSR;
017200090128
017300090128       //--------------------------------------------------------------
017400090128       //?Reperimento Dati del job (Utente/Operativi).
017500090128       //--------------------------------------------------------------
017600090128       BEGSR sr_DatiJob;
017700090128
017800090128         in(E) �AzUte;
017900090128         if NOT %error;
018000090128           in(E) �DatiUte;
018100090128         endif;
018200090128         if %error or RSut = *blanks;
018300090128           clear TIBS34ds;
018400090128           tibs34r(tibs34ds);
018500090128           in �AzUte;
018600090128           in �DatiUte;
018700090128         endif;
018800090128
018900090128       ENDSR;
019000090128
019100090128       //--------------------------------------------------------------
019200090128       //?Gestione videata D01
019300090128       //--------------------------------------------------------------
019400090128       BEGSR sr_GesD01;
019500090128
019600090128         // Inizializzazione videata
019700090128         if  $InzD01 = *on;
019800111219           clear UBRTVS_GD1;
019900111219           clear UBRTVS_GD2;
020000090128           $InzD01  = *off;
020100090128         endif;
020200090128
020300090128         // emissione videata
020400090128         if  ErrMessage = *off;
020500111219           write UBRTVS_GT1;
020600111219           write UBRTVS_GZ1;
020700090128         endif;
020800090128
020900111219         exfmt UBRTVS_GD1;
021000090128
021100090128         reset ErrMessage;
021200090128         reset ErrGenerico;
021300090128         clear V1Dmsg;
021400090128
021500090128         SELECT;
021600090128
021700090128         // - F3=Fine
021800090128           WHEN  dsp_aid = c_F03;
021900090128             $Fine = *on;
022000090128
022100090128         // - Invio o F6=Conferma
022200090128           OTHER;
022300090128             exsr sr_CtrD01;
022400090128             select;
022500090129               // - rilevato errore
022600090128               when  ErrGenerico = *on;
022700090129               // - non premuto F6
022800090128               when  dsp_aid <> c_F06;
022900090129               // - override da gestire
023000090128               other;
023100111219                 exsr Call_UBRTVSPE;
023200090128             endsl;
023300090128
023400090128         ENDSL;
023500090128
023600090128       ENDSR;
023700090128
023800090128       //--------------------------------------------------------------
023900111219       //?Richiamo pgm
024000090128       //--------------------------------------------------------------
024100111219       BEGSR Call_UBRTVSPE;
024200090128
024300111219         UBRTVSPE(VIDAnno:VIDLinea:VIDSerie:VIDNrSped:
024400111219                  FNARB_DS:FNBLP_DS:TITAS_DS:pEsito);
024500111219         V2DEsito = pEsito;
024600111219         select;
024700111219           // arrivo
024800111219           when  V2DEsito = 'A';
024900111219             V2DDS = FNARB_DS;
025000111219           // partenza
025100111219           when  V2DEsito = 'P';
025200111219             V2DDS = FNBLP_DS;
025300111219           // sede
025400111219           when  V2DEsito = 'S';
025500111219             V2DDS = TITAS_DS;
025600111219         endsl;
025700111219
025800100622         dou  dsp_aid <> c_F03;
025900090212
026000111219           write  UBRTVS_GD1;
026100090212           write  Protect;
026200111219           exfmt  UBRTVS_GD2;
026300090212
026400090212           select;
026500090212             // - F3=Fine
026600090212             when  dsp_aid = c_F03;
026700090212               $Fine = *on;
026800090212           endsl;
026900090212
027000090212         enddo;
027100090128
027200090128       ENDSR;
027300090128
027400090128       //--------------------------------------------------------------
027500090128       //?Controllo dati in videata D01
027600090128       //--------------------------------------------------------------
027700090128       BEGSR sr_CtrD01;
027800090128
027900090128         IndDspF  = *zero;
028000090128
028100090128       ENDSR;
028200090128
028300090128      /end-free
