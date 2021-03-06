000100100210      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000200100210     /*PRM dftactgrp(*no) actgrp(QILE)
000300100210     /*END
000400100210
000500100210       //--------------------------------------------------------------
000600100210       //?TEST Gest. tab. "3EW" (Dati EasyWEB per "postazione")        ?
000700100210       //--------------------------------------------------------------
000800100210
000900100210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000100210     h dftactgrp(*no) actgrp('QILE')
001100100210
001200100210       //--------------------------------------------------------------
001300100210       //?Dichiarazione file.                                          ?
001400100210       //--------------------------------------------------------------
001500100210
001600100210       //?- Video?
001700100210     fTNTB79TSTDcf   e             workstn
001800100210     f                                     indds(IndDspF)
001900100210     f                                     infds(InfDspF)
002000100210
002100100210       //--------------------------------------------------------------
002200100210       //?Definizione costanti.                                        ?
002300100210       //--------------------------------------------------------------
002400100210
002500100210       //?- Tasti funzionali a video?
002600100210     d c_F01           c                   const(x'31')
002700100210     d c_F02           c                   const(x'32')
002800100210     d c_F03           c                   const(x'33')
002900100210     d c_F04           c                   const(x'34')
003000100210     d c_F05           c                   const(x'35')
003100100210     d c_F06           c                   const(x'36')
003200100210     d c_F07           c                   const(x'37')
003300100210     d c_F08           c                   const(x'38')
003400100210     d c_F09           c                   const(x'39')
003500100210     d c_F10           c                   const(x'3A')
003600100210     d c_F11           c                   const(x'3B')
003700100210     d c_F12           c                   const(x'3C')
003800100210     d c_F13           c                   const(x'B1')
003900100210     d c_F14           c                   const(x'B2')
004000100210     d c_F15           c                   const(x'B3')
004100100210     d c_F16           c                   const(x'B4')
004200100210     d c_F17           c                   const(x'B5')
004300100210     d c_F18           c                   const(x'B6')
004400100210     d c_F19           c                   const(x'B7')
004500100210     d c_F20           c                   const(x'B8')
004600100210     d c_F21           c                   const(x'B9')
004700100210     d c_F22           c                   const(x'BA')
004800100210     d c_F23           c                   const(x'BB')
004900100210     d c_F24           c                   const(x'BC')
005000100210     d c_Enter         c                   const(x'F1')
005100100210     d c_RollDown      c                   const(x'F4')
005200100210     d c_RollUp        c                   const(x'F5')
005300100210
005400100210       //--------------------------------------------------------------
005500100210       //?Definizione schiere.                                         ?
005600100210       //--------------------------------------------------------------
005700100210
005800100210
005900100210       //--------------------------------------------------------------
006000100210       //?Definizione aree dati.                                       ?
006100100210       //--------------------------------------------------------------
006200100210
006300100210       //?- Dati utente?
006400100210     d �AzUte        e ds                  extname(AZUTE00F)
006500100210     d                                     dtaara
006600100210     d �DatiUte      e ds                  extname(dDatiUte)
006700100210     d                                     dtaara
006800100210
006900100210       //--------------------------------------------------------------
007000100210       //?Definizione strutture dati.                                  ?
007100100210       //--------------------------------------------------------------
007200100210
007300100210       //?- Status ds?
007400100210     d Status         sds
007500100210     d  SDSpgm           *proc
007600100210     d  SDSprm           *parms
007700100210     d  SDSusr               254    263
007800100210
007900100210       //?- InfDS?
008000100210     d InfDspF         ds
008100100210     d  dsp_aid              369    369a                                        AID byte
008200100210
008300100210       //?- Indicatori su DspF?
008400100210     d IndDspF         ds
008500100210        //?- Indicatori di errore?
008600100210     d  ErrMessage                    1n   overlay(IndDspF : 28)
008700100210     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008800100210
008900100210       //?- Parametri ricevuti?
009000100210     d KPJBA         e ds
009100100210
009200100210       //?- Parametri x Controllo profilo utenti?
009300100210     d TIBS34ds      e ds
009400100210
009500100210       //?- Parametri x TRTB79R?
009600100210     d TNTB79ds      e ds
009700100210
009800100210       // -?Parametri per gestione clienti abilitati a STRATEGI?
009900100210     d TIS720ds      e ds                  inz
010000100210     d   S20op0      e                     inz('P01')
010100100210     d   S20f03      e                     inz(*off)
010200100210     d   S20f12      e                     inz(*off)
010300100210     d   S20err      e                     inz(*off)
010400100210
010500100210       //--------------------------------------------------------------
010600100210       //?Definizione variabili globali.                               ?
010700100210       //--------------------------------------------------------------
010800100210
010900100210       //?- Flags booleani?
011000100210     d $Fine           s               n   inz(*off)
011100100210     d $InzD01         s               n   inz(*on)
011200100210
011300100210       //--------------------------------------------------------------
011400100210       //?Definizione procedure usate.                                 ?
011500100210       //--------------------------------------------------------------
011600100210
011700100210       //?- Reperimento dati utente?
011800100210     d tibs34r         pr                  extpgm('TIBS34R')
011900100210     d   tibs34ds                          likeds(TIBS34ds)
012000100210
012100100210       //?- Gest. tab. "3EW" (Dati EasyWEB per "postazione")?
012200100210     d tntb79r         pr                  extpgm('TNTB79R')
012300100210     d   kpjba                             likeds(KPJBA)
012400100210     d   tntb79ds                          likeds(TNTB79ds)
012500100210
012600100210       // -?Gestione clienti STRATEGI?
012700100210      /copy gaitrasrc/srcProtoPR,TIS724R
012800100210
012900100210       //--------------------------------------------------------------
013000100210       //?Definizione key-list.                                        ?
013100100210       //--------------------------------------------------------------
013200100210
013300100210
013400100210       //--------------------------------------------------------------
013500100210       //?Riepilogo indicatori.                                        ?
013600100210       //--------------------------------------------------------------
013700100210
013800100210     c     *Entry        plist
013900100210     c                   parm                    KPJBA
014000100210
014100100210      /free
014200100210
014300100210       //?- Start?
014400100210       *inLR = *on;
014500100210
014600100210       //?- Elab?
014700100210       dow  $Fine = *off;
014800100210         exsr sr_GesD01;
014900100210       enddo;
015000100210
015100100210       //?- End?
015200100210       return;
015300100210
015400100210       //--------------------------------------------------------------
015500100210       //?Operazioni iniziali                                          ?
015600100210       //--------------------------------------------------------------
015700100210       BEGSR *InzSR;
015800100210
015900100210         //?- Reperimento dati utente?
016000100210         exsr sr_DatiJob;
016100100210
016200100210         //?- Impostazione nome programma in testata a video?
016300100210         V1Tpgm = SDSpgm;
016400100210
016500100210       ENDSR;
016600100210
016700100210       //--------------------------------------------------------------
016800100210       //?Reperimento Dati del job (Utente/Operativi).                 ?
016900100210       //--------------------------------------------------------------
017000100210       BEGSR sr_DatiJob;
017100100210
017200100210         in(E) �AzUte;
017300100210         if NOT %error;
017400100210           in(E) �DatiUte;
017500100210         endif;
017600100210         if %error or RSut = *blanks;
017700100210           clear TIBS34ds;
017800100210           tibs34r(tibs34ds);
017900100210           in �AzUte;
018000100210           in �DatiUte;
018100100210         endif;
018200100210
018300100210       ENDSR;
018400100210
018500100210       //--------------------------------------------------------------
018600100210       //?Gestione videata D01                                         ?
018700100210       //--------------------------------------------------------------
018800100210       BEGSR sr_GesD01;
018900100210
019000100210         //?- Inizializzazione videata?
019100100210         if  $InzD01 = *on;
019200100210           clear TB79D01;
019300100210           $InzD01  = *off;
019400100210         endif;
019500100210
019600100210         //?- Emissione videata?
019700100210         write TB79T01;
019800100210         write TB79P01;
019900100210         exfmt TB79D01;
020000100210
020100100210         reset ErrMessage;
020200100210         reset ErrGenerico;
020300100210         clear V1Dmsg;
020400100210
020500100210         SELECT;
020600100210
020700100210           //?- F3=Fine?
020800100210           WHEN  dsp_aid = c_F03;
020900100210             $Fine = *on;
021000100210
021100100210           // -?F4=Ricerca cliente STRATEGI?
021200100210           WHEN  dsp_aid = c_F04;
021300100210             exsr  sr_CallTIS724;
021400100210
021500100210           //?- Invio o F6=Conferma?
021600100210           OTHER;
021700100210             exsr sr_CtrD01;
021800100210             select;
021900100210               //?- rilevato errore?
022000100210               when  ErrGenerico = *on;
022100100210               //?- non premuto F6?
022200100210               when  dsp_aid <> c_F06;
022300100210               //?- elaborazione dati immessi?
022400100210               other;
022500100210                 exsr  Call_TNTB79R;
022600100210                 //reset $InzD01;
022700100210             endsl;
022800100210
022900100210         ENDSL;
023000100210
023100100210       ENDSR;
023200100210
023300100210       //--------------------------------------------------------------
023400100210       //?Chiamata alla gestione dei clienti abilitati ai servizi      ?
023500100210       //?  STRATEGI                                                   ?
023600100210       //--------------------------------------------------------------
023700100210       BEGSR  sr_CallTIS724;
023800100210
023900100210         reset  TIS720ds;
024000100210         kpjbu = TIS720ds;
024100100210
024200100210         tis724r ( kpjba );
024300100210
024400100210         TIS720ds = kpjbu;
024500100210         if  S20f03 = *off   and   S20f12 = *off;
024600100210           i3EWksu = S20ksc;
024700100210           i3EWsun = S20sun;
024800100210         endif;
024900100210
025000100210       ENDSR;
025100100210
025200100210       //--------------------------------------------------------------
025300100210       //?Richiamo *pgm TNTB79R                                        ?
025400100210       //--------------------------------------------------------------
025500100210       BEGSR Call_TNTB79R;
025600100210
025700100210         clear  o3EWuni;
025800100210         clear  o3EWfxx;
025900100210         clear  o3EWerr;
026000100210         clear  o3EWmsg;
026100100210
026200100210         tntb79r ( kpjba : tntb79ds );
026300100210
026400100210         write  Protect;
026500100210         exfmt  TB79D02;
026600100210
026700100210         //?- F3=Fine?
026800100210         if  dsp_aid = c_F03;
026900100210           $Fine = *on;
027000100210           leavesr;
027100100210         endif;
027200100210
027300100210       ENDSR;
027400100210
027500100210       //--------------------------------------------------------------
027600100210       //?Controllo dati in videata D01                                ?
027700100210       //--------------------------------------------------------------
027800100210       BEGSR sr_CtrD01;
027900100210
028000100210         %subst(IndDspF : 40) = *off;
028100100210
028200100210       ENDSR;
028300100210
028400100210      /end-free
