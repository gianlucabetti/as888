000100090521      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200090521       //--------------------------------------------------------------
000300090521       //?Gestione tabella "CCO" (Causali contatto marketing)          ?
000400090521       //--------------------------------------------------------------
000500090521
000600090521     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090521     h dftactgrp(*no) actgrp(*caller)
000800090521
000900090521       //--------------------------------------------------------------
001000090521       //?Dichiarazione file.                                          ?
001100090521       //--------------------------------------------------------------
001200090521
001300090521       // - Tabelle
001400090525     fTABEL00F  if   e           k disk
001500090521     fTNTBE01L  Uf A e           k disk
001600090521
001700090521       // - Video Gestione
001800090521     fTNTB75D   cf   e             workstn
001900090521     f                                     indds(IndDspF)
002000090521     f                                     infds(InfDspF)
002100090521
002200090521       //--------------------------------------------------------------
002300090521       //?Definizione costanti.                                        ?
002400090521       //--------------------------------------------------------------
002500090521
002600090521       // - Codice tabella in gestione
002700090521     d c_Tab           c                   const('CCO')
002800090525
002900090525       // - Codice utente in vecchie tabelle (TABEL00F)
003000090525     d c_Kut           c                   const(1)
003100090525
003200090525       // - Forzatura *blank
003300090525     d c_Blank         c                   const('BL')
003400090525     d c_Blank_Des     c                   const('Nessuno')
003500090525     d c_All           c                   const('99')
003600090525     d c_All_Des       c                   const('Qualsiasi')
003700090521
003800090521       // - Tasti funzionali a video
003900090521     d c_F01           c                   const(x'31')
004000090521     d c_F02           c                   const(x'32')
004100090521     d c_F03           c                   const(x'33')
004200090521     d c_F05           c                   const(x'35')
004300090521     d c_F06           c                   const(x'36')
004400090521     d c_F07           c                   const(x'37')
004500090521     d c_F08           c                   const(x'38')
004600090521     d c_F09           c                   const(x'39')
004700090521     d c_F10           c                   const(x'3A')
004800090521     d c_F12           c                   const(x'3C')
004900090521     d c_F13           c                   const(x'B1')
005000090521     d c_F14           c                   const(x'B2')
005100090521     d c_F15           c                   const(x'B3')
005200090521     d c_F16           c                   const(x'B4')
005300090521     d c_F17           c                   const(x'B5')
005400090521     d c_F18           c                   const(x'B6')
005500090521     d c_F19           c                   const(x'B7')
005600090521     d c_F20           c                   const(x'B8')
005700090521     d c_F21           c                   const(x'B9')
005800090521     d c_F22           c                   const(x'BA')
005900090521     d c_F23           c                   const(x'BB')
006000090521     d c_F24           c                   const(x'BC')
006100090521     d c_Enter         c                   const(x'F1')
006200090521     d c_RollDown      c                   const(x'F4')
006300090521     d c_RollUp        c                   const(x'F5')
006400100127
006500100127     d $_opz           c                   const(' 25G')
006600090521
006700090521       //--------------------------------------------------------------
006800090521       //?Definizione schiere.                                         ?
006900090521       //--------------------------------------------------------------
007000090521
007100090521       // - Messaggi di errore
007200100126     d $Msg            s             78    dim(20)  ctdata  perrcd(01)
007300090521
007400090521       //--------------------------------------------------------------
007500090521       //?Definizione aree dati.                                       ?
007600090521       //--------------------------------------------------------------
007700090521
007800090521       // - Dati utente
007900090521     d §AzUte        e ds                  extname(AZUTE00F)
008000090521     d                                     dtaara
008100090521     d §DatiUte      e ds                  extname(dDatiUte)
008200090521     d                                     dtaara
008300090521
008400090521       //--------------------------------------------------------------
008500090521       //?Definizione strutture dati.                                  ?
008600090521       //--------------------------------------------------------------
008700090521
008800090521       // - Status ds
008900090521     d Status         sds
009000090521     d  SDSpgm           *proc
009100090521
009200090521       // - InfDS
009300090521     d InfDspF         ds
009400090521     d   dsp_aid             369    369a
009500090521
009600090521       // - Indicatori su DspF
009700090521     d IndDspF         ds                  inz
009800090521         // - Abilitazione tasti funzionali
009900090521     d   F5Attivo                      n   overlay(IndDspF : 05)
010000090521     d   F6Attivo                      n   overlay(IndDspF : 06)
010100090521     d   F10Attivo                     n   overlay(IndDspF : 10)
010200090521     d   F16Attivo                     n   overlay(IndDspF : 16)
010300090521         // - Emissione messaggio di errore
010400090521     d   ErrMessage                    n   overlay(IndDspF : 28)
010500090521         // - Posizionamento cursore & segnalazione errore
010600101209     d   PosCurTPV6                    n   overlay(IndDspF : 40)
010700101209     d   PosCurTPV7                    n   overlay(IndDspF : 41)
010800101209     d   PosCurTPV8                    n   overlay(IndDspF : 42)
010900101209     d   PosCurTPV9                    n   overlay(IndDspF : 43)
011000101209     d   PosCurTPV0                    n   overlay(IndDspF : 44)
011100101209     d   PosCurCAE                     n   overlay(IndDspF : 45)
011200110429     d   PosCurCP1                     n   overlay(IndDspF : 46)
011300110429     d   PosCurCP2                     n   overlay(IndDspF : 47)
011400110429     d   PosCurCP3                     n   overlay(IndDspF : 48)
011500110429     d   PosCurCP4                     n   overlay(IndDspF : 49)
011600090521     d   PosCurCAM                     n   overlay(IndDspF : 50)
011700090521     d   PosCurDES                     n   overlay(IndDspF : 51)
011800090521     d   PosCurGIO                     n   overlay(IndDspF : 52)
011900100127     d   PosCurOPZ1                    n   overlay(IndDspF : 53)
012000100127     d   PosCurOPZ2                    n   overlay(IndDspF : 54)
012100100127     d   PosCurOPZ3                    n   overlay(IndDspF : 55)
012200100127     d   PosCurOPZ4                    n   overlay(IndDspF : 56)
012300100127     d   PosCurOPZ5                    n   overlay(IndDspF : 57)
012400100924     d   PosCurUPM                     n   overlay(IndDspF : 61)
012500100504     d   PosCurTPV1                    n   overlay(IndDspF : 62)
012600100504     d   PosCurTPV2                    n   overlay(IndDspF : 63)
012700100504     d   PosCurTPV3                    n   overlay(IndDspF : 64)
012800100504     d   PosCurTPV4                    n   overlay(IndDspF : 65)
012900100504     d   PosCurTPV5                    n   overlay(IndDspF : 66)
013000090521     d   PosCurSTP1                    n   overlay(IndDspF : 67)
013100090526     d   PosCurSTP2                    n   overlay(IndDspF : 68)
013200090526     d   PosCurSTP3                    n   overlay(IndDspF : 69)
013300090526     d   PosCurSTP4                    n   overlay(IndDspF : 70)
013400090526     d   PosCurSTP5                    n   overlay(IndDspF : 71)
013500090526     d   PosCurSTP6                    n   overlay(IndDspF : 72)
013600090526     d   PosCurSTP7                    n   overlay(IndDspF : 73)
013700090526     d   PosCurSTP8                    n   overlay(IndDspF : 74)
013800090526     d   PosCurSTP9                    n   overlay(IndDspF : 75)
013900090526     d   PosCurSTPA                    n   overlay(IndDspF : 76)
014000090526     d   PosCurSTC1                    n   overlay(IndDspF : 77)
014100090526     d   PosCurSTC2                    n   overlay(IndDspF : 78)
014200090526     d   PosCurSTC3                    n   overlay(IndDspF : 79)
014300090526     d   PosCurSTC4                    n   overlay(IndDspF : 80)
014400090526     d   PosCurSTC5                    n   overlay(IndDspF : 81)
014500090526     d   PosCurSTC6                    n   overlay(IndDspF : 82)
014600090526     d   PosCurSTC7                    n   overlay(IndDspF : 83)
014700090526     d   PosCurSTC8                    n   overlay(IndDspF : 84)
014800090526     d   PosCurSTC9                    n   overlay(IndDspF : 85)
014900090521     d   PosCurSTCA                    n   overlay(IndDspF : 86)
015000100126     d   PosCurATT                     n   overlay(IndDspF : 87)
015100090629     d   PosCurTAT                     n   overlay(IndDspF : 88)
015200090521         //   - Riemissione videata
015300090521     d   ErrGenerico                   n   overlay(IndDspF : 99)
015400090521
015500090521       // - Parametri ricevuti
015600090521     d KPJBA         e ds
015700090521
015800090521       // - Parametri per Reperimento dati utente
015900090521     d TIBS34ds      e ds
016000090521
016100090521       // - Parametri per Ricerca/controllo tabelle
016200090521     d TIBS02ds      e ds                  inz
016300090521     d   T02mod      e                     inz('R')
016400090521     d   T02cod      e                     inz(c_Tab)
016500090525
016600090525       // - Tab. CCO = Causali contatto marketing
016700090525     d dCCO          e ds                  inz
016800090525       // - Tab. ECO = Esiti contatto marketing
016900090525     d dECO          e ds                  inz
017000090525
017100090525       // - Tab. 1Q = Stato clienti potenziali
017200090525     d ds1Q          e ds                  inz
017300090521
017400090521       // - Parametri per Interrogazione/Selezione tab. "CCO"
017500090521     d TNTB74ds      e ds                  inz
017600090521
017700090521       // - Dati record principale della tabella
017800090521     d TNTBEds       e ds                  inz  extname(TNTBE00F)
017900090521     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
018000090521     d                                          prefix(TBX:3)
018100090521
018200100219       // - Ridefinizione dati a video
018300100219       // - - Tipi Attività abilitati
018400100219     d dsV1CTAA        ds            10    inz
018500100219     d  V1Caa1                             inz
018600100219     d  V1Caa2                             inz
018700100219     d  V1Caa3                             inz
018800100219     d  V1Caa4                             inz
018900100219     d  V1Caa5                             inz
019000100219     d  V1Caa6                             inz
019100100219     d  V1Caa7                             inz
019200100219     d  V1Caa8                             inz
019300100219     d  V1Caa9                             inz
019400100219     d  V1Ca10                             inz
019500100504       // - - Tipi Trattativa
019600101209     d dsV1CTPV        ds            10    inz
019700100504     d  V1Ctpv1                            inz
019800100504     d  V1Ctpv2                            inz
019900100504     d  V1Ctpv3                            inz
020000100504     d  V1Ctpv4                            inz
020100100504     d  V1Ctpv5                            inz
020200101209     d  V1Ctpv6                            inz
020300101209     d  V1Ctpv7                            inz
020400101209     d  V1Ctpv8                            inz
020500101209     d  V1Ctpv9                            inz
020600101209     d  V1Ctpv0                            inz
020700101209     d   $V1Ctpv               1     10    inz  dim(10)
020800100219
020900090521       // - Ridefinizione dati a video
021000090526       // - - Stati Precedenti Possibili
021100090521     d dsV1Cstp        ds            20    inz
021200090521     d  V1Cstp1                            inz
021300090521     d  V1Cstp2                            inz
021400090521     d  V1Cstp3                            inz
021500090521     d  V1Cstp4                            inz
021600090521     d  V1Cstp5                            inz
021700090521     d  V1Cstp6                            inz
021800090521     d  V1Cstp7                            inz
021900090521     d  V1Cstp8                            inz
022000090521     d  V1Cstp9                            inz
022100090521     d  V1CstpA                            inz
022200090521     d   $V1Cstp               1     20    inz  dim(10)
022300090526     d dsV1Dstp        ds           250    inz
022400090525     d  V1Dstp1                            inz
022500090525     d  V1Dstp2                            inz
022600090525     d  V1Dstp3                            inz
022700090525     d  V1Dstp4                            inz
022800090525     d  V1Dstp5                            inz
022900090525     d  V1Dstp6                            inz
023000090525     d  V1Dstp7                            inz
023100090525     d  V1Dstp8                            inz
023200090525     d  V1Dstp9                            inz
023300090525     d  V1DstpA                            inz
023400090526     d   $V1Dstp               1    250    inz  dim(10)
023500090526       // - - Stati Futuri Corrispondenti
023600090521     d dsV1Cstc        ds            20    inz
023700090521     d  V1Cstc1                            inz
023800090521     d  V1Cstc2                            inz
023900090521     d  V1Cstc3                            inz
024000090521     d  V1Cstc4                            inz
024100090521     d  V1Cstc5                            inz
024200090521     d  V1Cstc6                            inz
024300090521     d  V1Cstc7                            inz
024400090521     d  V1Cstc8                            inz
024500090521     d  V1Cstc9                            inz
024600090521     d  V1CstcA                            inz
024700090521     d   $V1Cstc               1     20    inz  dim(10)
024800090526     d dsV1Dstc        ds           250    inz
024900090525     d  V1Dstc1                            inz
025000090525     d  V1Dstc2                            inz
025100090525     d  V1Dstc3                            inz
025200090525     d  V1Dstc4                            inz
025300090525     d  V1Dstc5                            inz
025400090525     d  V1Dstc6                            inz
025500090525     d  V1Dstc7                            inz
025600090525     d  V1Dstc8                            inz
025700090525     d  V1Dstc9                            inz
025800090525     d  V1DstcA                            inz
025900090526     d   $V1Dstc               1    250    inz  dim(10)
026000110429       // - - Categoria Potenziale abilitate
026100110429     d dsV1Ccpo        ds             4    inz
026200110429     d  V1Ccp1                             inz
026300110429     d  V1Ccp2                             inz
026400110429     d  V1Ccp3                             inz
026500110429     d  V1Ccp4                             inz
026600110429     d   $V1Ccpo               1      4    inz  dim(4)
026700090521
026800090521       //--------------------------------------------------------------
026900090521       //?Definizione variabili globali.                               ?
027000090521       //--------------------------------------------------------------
027100090521
027200090521       // - Flags booleani
027300090521     d $Fine           s               n   inz
027400090521     d $InzD01         s               n   inz(*on)
027500090521     d $InzD02         s               n   inz(*on)
027600101209     d $InzD03         s               n   inz(*on)
027700090521     d $InzW01         s               n   inz(*on)
027800090521     d $Annullamento   s               n   inz
027900090521     d $Immissione     s               n   inz
028000090521     d $Ripristino     s               n   inz
028100090521
028200090521       // - Indici di schiera
028300090521     d xx              s              3  0 inz
028400090521
028500090521       // - Variabili per la gestione del video
028600090521     d $Video          s              2    inz('D1')
028700090525
028800090525       // - Flag di errore
028900090525     d $Err            s              1    inz(*off)
029000090521
029100090521       // - Campi di comodo
029200090521     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
029300110429     d savcod          s              1
029400090521
029500090525       // - Parametri per pgm interrogazione tabelle
029600090525      /copy gaitrasrc/srcProtoPI,X§TABER
029700090521
029800090521       //--------------------------------------------------------------
029900090521       //?Definizione prototipi procedure.                             ?
030000090521       //--------------------------------------------------------------
030100090521
030200090521       // - Reperimento dati utente
030300090521      /copy gaitrasrc/srcProtoPR,TIBS34R
030400090521
030500090525       // - Ricerca vecchie tabelle
030600090525      /copy gaitrasrc/srcProtoPR,X§TABER
030700090525
030800090525       // - Ricerca e controllo nuove tabelle
030900090521      /copy gaitrasrc/srcProtoPR,TIBS02R
031000090521
031100090525       // - Selezione singola causale attività
031200090521     d tntb74r         pr                  extpgm('TNTB74R')
031300090525     d   kpjba                             likeds(KPJBA)
031400090521     d   tntb74ds                          likeds(TNTB74ds)
031500090521
031600090521       //--------------------------------------------------------------
031700090521       //?Definizione key-list.                                        ?
031800090521       //--------------------------------------------------------------
031900090521
032000090525       // - File TABEL00F
032100090525     d k03tabel00    e ds                  extname(TABEL00F : *key)
032200090525     d                                     prefix(k_)   inz
032300090525
032400090521       // - File TNTBE01L
032500090521     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
032600090521     d                                     prefix(k_)   inz
032700090521
032800090521       //--------------------------------------------------------------
032900090521       //?Riepilogo indicatori utilizzati.                             ?
033000090521       //--------------------------------------------------------------
033100090521       //--------------------------------------------------------------
033200090521
033300090521       //--------------------------------------------------------------
033400090521       //?M A I N - L I N E                                            ?
033500090521       //--------------------------------------------------------------
033600090521
033700090521     c     *Entry        plist
033800090521     c                   parm                    KPJBA
033900090521
034000090521      /free
034100090521
034200090521       //?Operazioni iniziali?
034300090521       exsr sr_RoutInz;
034400090521
034500090521       //?Gestione video?
034600090521       DOW  $Fine = *off;
034700090521         select;
034800090521           // - Manutenzione singola tabella
034900090521           when $Video = 'D2';
035000090521             exsr sr_GesD02;
035100090526           when $Video = 'D3';
035200090526             exsr sr_GesD03;
035300101209           when $Video = 'D4';
035400101209             exsr sr_GesD04;
035500090521           // - Filtro di lancio
035600090521           when $Video = 'D1';
035700090521             exsr sr_GesD01;
035800090521           // - Richiesta dati per trasmissione record
035900090521           when $Video = 'W1';
036000090521             exsr sr_GesW01;
036100090521           other;
036200090521             $Fine = *on;
036300090521         endsl;
036400090521       ENDDO;
036500090521
036600090521       //?Operazioni finali?
036700090521       exsr sr_RoutEnd;
036800090521
036900090521       //--------------------------------------------------------------
037000090521       //?Operazioni iniziali.                                         ?
037100090521       //--------------------------------------------------------------
037200090521       BEGSR  sr_RoutInz;
037300090521
037400090521         *inLR = *on;
037500090521
037600090521         //?Reperimento dati job?
037700090521         exsr  sr_DatiJob;
037800090521
037900090521         //?Impostazione nome programma a video?
038000090521         V1Tpgm = SDSpgm;
038100090521
038200090521         //?Aggancio dati generali della tabella in esame?
038300090521         clear  k_TBEcod;
038400090521         k_TBEke1 = *zero;
038500090521         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
038600090521                = c_Tab;
038700090521         clear  k_TBEke2;
038800090521         clear  k_TBElin;
038900090521         k_TBEsif = KNSIF;
039000090521         chain(n)  %kds(k05tntbe01)  TNTBE000;
039100090521         if  not  %found(TNTBE01L);
039200090521           clear  k_TBEsif;
039300090521           chain(n)  %kds(k05tntbe01)  TNTBE000;
039400090521         endif;
039500090521         if  %found(TNTBE01L);
039600090521           xTNTBEds = TNTBEds;
039700090521         else;
039800090521           clear  xTNTBEds;
039900090521         endif;
040000090521
040100090521         //?Impostazione iniziale / pulizia dei campi chiave?
040200090521         k_TBEcod = c_Tab;
040300090521         clear  k_TBEke1;
040400090521         clear  k_TBEke2;
040500090521         clear  k_TBElin;
040600090521         clear  k_TBEsif;
040700090521
040800090521         //?1ª inizializzazione della videata D01?
040900090521         exsr  sr_InzD01;
041000090521         V1Ccam  = '?';
041100090521         $InzD01 = *off;
041200090521
041300090521       ENDSR;
041400090521
041500090521       //--------------------------------------------------------------
041600090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
041700090521       //--------------------------------------------------------------
041800090521       BEGSR  sr_DatiJob;
041900090521
042000090521         in(E) §AzUte;
042100090521         if NOT %error;
042200090521           in(E) §DatiUte;
042300090521         endif;
042400090521         if %error or RSut = *blanks;
042500090521           clear TIBS34ds;
042600090521           tibs34r ( tibs34ds );
042700090521           in §AzUte;
042800090521           in §DatiUte;
042900090521         endif;
043000090521
043100090521       ENDSR;
043200090521
043300090521       //--------------------------------------------------------------
043400090521       //?Gestione videata D01                                         ?
043500090521       //--------------------------------------------------------------
043600090521       BEGSR  sr_GesD01;
043700090521
043800090521         //?Inizializzazione videata?
043900090521         if  $InzD01 = *on;
044000090521           exsr  sr_InzD01;
044100090521           $InzD01 = *off;
044200090521         endif;
044300090521
044400090521         //?Emissione videata?
044500090521         write  TB75T01;
044600090521         write  TB75P01;
044700090521         exfmt  TB75D01;
044800090521
044900090521         reset  ErrMessage;
045000090521         reset  ErrGenerico;
045100090521         clear  V1Dmsg;
045200090521
045300090521         SELECT;
045400090521
045500090521           //?F3=Fine?
045600090521           WHEN  dsp_aid = c_F03;
045700090521             $Fine = *on;
045800090521
045900090521           //?Invio?
046000090521           OTHER;
046100090521             exsr sr_CtrD01;
046200090521             if  ErrGenerico = *off;
046300090521               $Video    = 'D2';
046400090521               $InzD02   = *on;
046500090521             endif;
046600090521
046700090521         ENDSL;
046800090521
046900090521       ENDSR;
047000090521
047100090521       //--------------------------------------------------------------
047200090521       //?Inizializzazione videata D01                                 ?
047300090521       //--------------------------------------------------------------
047400090521       BEGSR  sr_InzD01;
047500090521
047600090525         clear  TB75D01;
047700090521
047800090521       ENDSR;
047900090521
048000090521       //--------------------------------------------------------------
048100090521       //?Controllo dati immessi nella videata D01                     ?
048200090521       //--------------------------------------------------------------
048300090521       BEGSR  sr_CtrD01;
048400090521
048500090521         %subst(IndDspF : 28) = *off;
048600090521
048700090521         SELECT;
048800090521
048900090521           //?Ricerca codice Causale Attività?
049000090521           WHEN  %scan('?' : V1Ccam) > *zeros;
049100090521             clear  TNTB74ds;
049200101201             kpjbu = 'TNTB75R' ;
049300090521             tntb74r ( kpjba : tntb74ds );
049400090521             select;
049500090521               when  oCCOfxx <> *blank;
049600090521               when  oCCOerr = *on;
049700090521                 ErrMessage  = *on;
049800090521                 PosCurCAM   = *on;
049900090521                 V1Dmsg = oCCOmsg;
050000090521               other;
050100090521                 V1Ccam = %subst(oCCOke1 : 1 : %len(V1Ccam));
050200090521             endsl;
050300090521             ErrGenerico = *on;
050400090521             leavesr;
050500090521
050600090521           //?Controllo immissione codice causale?
050700090521           WHEN  V1Ccam = *blank;
050800090521             ErrMessage  = *on;
050900090521             ErrGenerico = *on;
051000090521             PosCurCAM   = *on;
051100090521             V1Dmsg = $Msg(01);
051200090521             leavesr;
051300090521
051400090521         ENDSL;
051500090521
051600090521       ENDSR;
051700090521
051800090521       //--------------------------------------------------------------
051900090521       //?Gestione videata D02                                         ?
052000090521       //--------------------------------------------------------------
052100090521       BEGSR  sr_GesD02;
052200090521
052300090521         //?Inizializzazione videata?
052400090521         if  $InzD02 = *on;
052500090521           exsr sr_InzD02;
052600090521           $InzD02 = *off;
052700090521         endif;
052800090521
052900090521         //?Emissione Testata e Piede con tasti funzionali abilitati?
053000090521         if  errGenerico = *off;
053100090521           write  TB75T01;
053200090521           write  TB75D01;
053300090521           write  Protect;
053400090521           write  TB75P02;
053500090521         endif;
053600090521
053700090521         //?Emissione videata?
053800090521         exfmt  TB75D02;
053900090521
054000090521         reset  ErrMessage;
054100090521         reset  ErrGenerico;
054200090521         clear  V1Dmsg;
054300090521         reset  $Annullamento;
054400090521
054500090521         SELECT;
054600090521
054700090521           //?F3=Fine?
054800090521           WHEN  dsp_aid = c_F03;
054900090521             unlock TNTBE01L;
055000090521             $Fine = *on;
055100090521
055200090521           //?F12=Ritorno?
055300090521           WHEN  dsp_aid = c_F12;
055400090521             unlock TNTBE01L;
055500090521             reset  $Video;
055600090521             clear  V1Topz;
055700090526
055800090526           //?RollUp=Videata successiva?
055900090526           WHEN  dsp_aid = c_RollUp;
056000090526             $Video  = 'D3';
056100090521
056200090521
056300090521           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
056400090521           OTHER;
056500090521             $Ripristino   = (dsp_aid = c_F05);
056600090521             $Annullamento = (dsp_aid = c_F16);
056700090521             if  Not $Annullamento;
056800090521               exsr  sr_CtrD02;
056900090526               if  ErrGenerico;
057000090526                 leavesr;
057100090526               endif;
057200090526               exsr  sr_CtrD03;
057300090526               if  ErrGenerico;
057400090526                 $Video  = 'D3';
057500090526                 leavesr;
057600090526               endif;
057700101209               exsr  sr_CtrD04;
057800101209               if  ErrGenerico;
057900101209                 $Video  = 'D4';
058000101209                 leavesr;
058100101209               endif;
058200090521             endif;
058300090526             select;
058400101210               when  dsp_aid = c_Enter  ;
058500090526                 $Video  = 'D3';
058600090526               when  dsp_aid = c_F05  or
058700090526                     dsp_aid = c_F06  or
058800090526                     dsp_aid = c_F16;
058900090526                 $Video  = 'W1';
059000090526                 reset  $InzW01;
059100090526             endsl;
059200090521
059300090521         ENDSL;
059400090521
059500090521       ENDSR;
059600090521
059700090521       //--------------------------------------------------------------
059800090521       //?Inizializzazione videata D02                                 ?
059900090521       //--------------------------------------------------------------
060000090521       BEGSR  sr_InzD02;
060100090521
060200090521         //?Spegnimento di TUTTI gli indicatori?
060300090521         IndDspF     = *off;
060400090521
060500090521         //?Pulizia videata?
060600090521         clear  TB75D02;
060700090526         clear  TB75D03;
060800101209         clear  TB75D04;
060900090521
061000090521         //?Reperimento dati della causale contatto marketing?
061100090521         reset  $Immissione;
061200090521         reset  $Ripristino;
061300090521         k_TBEke1 = V1Ccam;
061400090521         chain  %kds(k05tntbe01)  TNTBE000;
061500090521         select;
061600090521           when  Not %found(TNTBE01L);
061700090521             $Immissione = *on;
061800090521           when  TBEatb <> *blank;
061900090521             $Ripristino = *on;
062000090521         endsl;
062100090521
062200090521         //?Caricamento dati della causale contatto marketing?
062300090521         if  %found(TNTBE01L);
062400090521           exsr  sr_RieD02;
062500090525         endif;
062600090521
062700090521         //?Impostazione tipo manutenzione?
062800090521         select;
062900090521           when  $Immissione;
063000090521             V1Topz = 'IMMISSIONE';
063100090521           when  $Ripristino;
063200090521             V1Topz = 'RIPRISTINO';
063300090521           other;
063400090521             V1Topz = ' MODIFICA ';
063500090521         endsl;
063600090521
063700090521         //?Impostazione indicatori per abilitazione tasti funzionali?
063800090521         exsr  Set_Ind_D02;
063900090521
064000090521       ENDSR;
064100090521
064200090521       //--------------------------------------------------------------
064300090521       //?Caricamento singola riga di testo nella videata D02          ?
064400090521       //--------------------------------------------------------------
064500090521       BEGSR  sr_RieD02;
064600090521
064700090521         dCCO = TBEuni;
064800090521
064900090521         V1Cdes   = §CCOdes;
065000100126         V1Cord   = §CCOord;
065100090521         V1Cgio   = §CCOgio;
065200100126         V1Ctat   = §CCOtat;
065300100126         V1Catt   = §CCOatt;
065400110429         dsV1Ccpo = §CCOcpo;
065500100924         V1Cupm   = §CCOupm;
065600100126         V1Cuti   = §CCOuti;
065700100126         V1Cesi   = §CCOesi;
065800100126         V1Ceaf   = §CCOeaf;
065900100126         V1Copz1  = %subst(§CCOopz:1:1);
066000100126         V1Copz2  = %subst(§CCOopz:2:1);
066100100126         V1Copz3  = %subst(§CCOopz:3:1);
066200100126         V1Copz4  = %subst(§CCOopz:4:1);
066300100126         V1Copz5  = %subst(§CCOopz:5:1);
066400100219         V1Caa1   = %subst(§CCOtaa:1:1);
066500100219         V1Caa2   = %subst(§CCOtaa:2:1);
066600100219         V1Caa3   = %subst(§CCOtaa:3:1);
066700100219         V1Caa4   = %subst(§CCOtaa:4:1);
066800100219         V1Caa5   = %subst(§CCOtaa:5:1);
066900100219         V1Caa6   = %subst(§CCOtaa:6:1);
067000100219         V1Caa7   = %subst(§CCOtaa:7:1);
067100100219         V1Caa8   = %subst(§CCOtaa:8:1);
067200100219         V1Caa9   = %subst(§CCOtaa:9:1);
067300100219         V1Ca10   = %subst(§CCOtaa:10:1);
067400100126         V1Cnot   = §CCOnot;
067500100126         V1Csta   = §CCOsta;
067600100219         V1Coff   = §CCOoff;
067700100305         V1Ccht   = §CCOcht;
067800110429         Savcod   = §CCOcod;
067900100305         V1Cres   = §CCOres;
068000100305         V1Ccom   = §CCOcoi;
068100120622         V1Cdpc   = §CCOdpc;
068200100126
068300101209         // imposto dati video 3
068400100504         dsV1Ctpv = §CCOtpv;
068500100126         V1Capt   = §CCOapt;
068600100126         V1Ctra   = §CCOtra;
068700100126         V1Ceso   = §CCOeso;
068800101209         V1Chot   = §CCOhot;
068900101209         V1Ccae   = §CCOcae;
069000101209         V1Cann   = §CCOann;
069100101209         V1Cblc   = §CCOblc;
069200101214         V1Cccp   = §CCOccp;
069300110211         V1Cstl   = §CCOstl;
069400110211         V1Cpgmblc = §CCOpgmblc;
069500110405         V1Ccns   = §CCOcns;
069600110405         V1Cucns  = §CCOucns;
069700101210
069800101210         // imposto dati video 4
069900101210         dsV1Cstp = §CCOstp;
070000101210         dsV1Cstc = §CCOstc;
070100090526
070200090526         // Decodifiche
070300090526         exsr  sr_CtrD02;
070400090526         exsr  sr_CtrD03;
070500101209         exsr  sr_CtrD04;
070600090526         clear  ErrMessage;
070700090526         clear  ErrGenerico;
070800090526         clear  V1Dmsg;
070900090521
071000090521       ENDSR;
071100090521
071200090521       //--------------------------------------------------------------
071300090521       //?Settaggio indicatori nella videata D02 per abilitazione      ?
071400090521       //?  tasti funzionali (sul subfile control)                     ?
071500090521       //--------------------------------------------------------------
071600090521       BEGSR Set_Ind_D02;
071700090521
071800090521         //?F5=Ripristino?
071900090521         F5Attivo  = (NOT $Immissione  and  $Ripristino);
072000090521
072100090521         //?F6=Conferma?
072200090521         F6Attivo  = (NOT F5Attivo);
072300090521
072400090521         //?F16=Annullamento?
072500090521         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
072600090521
072700090521       ENDSR;
072800090521
072900090521       //--------------------------------------------------------------
073000090521       //?Controllo dati videata D02                                   ?
073100090521       //--------------------------------------------------------------
073200090521       BEGSR  sr_CtrD02;
073300090521
073400101209         %subst(IndDspF : 40) = *off;
073500090521
073600090521         select;
073700090521
073800090521           //?Descrizione obbligatoria?
073900090521           when  V1Cdes = *blank;
074000090521             ErrMessage  = *on;
074100090521             ErrGenerico = *on;
074200090521             PosCurDES   = *on;
074300090521             V1Dmsg = $Msg(02);
074400090521             leavesr;
074500090629
074600090521           //?Giorni proposti per prossimo contatto negativi?
074700090521           when  V1Cgio < *zero;
074800090521             ErrMessage  = *on;
074900090521             ErrGenerico = *on;
075000090521             PosCurGIO   = *on;
075100090521             V1Dmsg = $Msg(03);
075200090521             leavesr;
075300090521
075400090521         endsl;
075500090521
075600100504           //?Tipo contatto futuro obbligatorio?
075700090629           If    %scan('?' : V1Ctat) > *zero;
075800090629             errGenerico = *on;
075900090629             PosCurTat   = *on;
076000090629             clear  TIBS02ds;
076100090629             T02mod = 'R';
076200090629             T02cod = 'TTA';
076300090629             T02sif = KNSIF;
076400090629             TNTBE_RicercaControllo  (kpjba : tibs02ds);
076500090629             if  T02err = *blanks;
076600090629               v1ctat = T02ke1;
076700090629               errGenerico = *on;
076800090629               leavesr;
076900090629             else;
077000090629               errMessage  = *on;
077100090629               errGenerico = *on;
077200090629               V1Dmsg = T02msg;
077300090629               leavesr;
077400090629             endif;
077500090629           endif;
077600090629
077700090629         // Controllo tipo contatto
077800090629           If    v1ctat <> *blanks ;
077900090629                 clear  TIBS02ds;
078000090629                 T02mod = 'C';
078100090629                 T02cod = 'TTA';
078200090629                 T02ke1 = v1ctat;
078300090629                 T02sif = KNSIF;
078400090629                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
078500090629                 if  T02err <> *blanks;
078600090629                     errMessage  = *on;
078700090629                     errGenerico = *on;
078800090629                     PosCurTat   = *on;
078900090629                     V1Dmsg = $Msg(10);
079000090629                     leavesr;
079100090629                 endif;
079200090629           else ;
079300100924        // se segno operazione <> da blanks obbligatorio l'inserimento
079400100924                 if v1cupm <> ' ' ;
079500090630                    errMessage  = *on;
079600090630                    errGenerico = *on;
079700090630                    PosCurTat   = *on;
079800090630                    V1Dmsg = $Msg(10);
079900090630                    leavesr;
080000090630                 endif;
080100090629           endif;
080200090525
080300100126
080400100127           //?Tipo attività associata?
080500100126           If    %scan('?' : V1Catt) > *zero;
080600100126             errGenerico = *on;
080700100126             PosCurATT   = *on;
080800100126             clear  TIBS02ds;
080900100126             T02mod = 'R';
081000100126             T02cod = 'TTA';
081100100126             T02sif = KNSIF;
081200100126             TNTBE_RicercaControllo  (kpjba : tibs02ds);
081300100126             if  T02err <> *blanks;
081400100126               errMessage  = *on;
081500100126               errGenerico = *on;
081600100126               V1Dmsg = T02msg;
081700100126               leavesr;
081800100126             endif;
081900100126               v1catt = T02ke1;
082000100126               errGenerico = *on;
082100100126               leavesr;
082200100126           endif;
082300100126
082400100126         // Controllo tipo attività
082500100126           If    v1catt <> *blanks ;
082600100126                 clear  TIBS02ds;
082700100126                 T02mod = 'C';
082800100126                 T02cod = 'TTA';
082900100126                 T02ke1 = v1catt;
083000100126                 T02sif = KNSIF;
083100100126                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
083200100126                 if  T02err <> *blanks;
083300100126                     errMessage  = *on;
083400100126                     errGenerico = *on;
083500100126                     PosCurATT   = *on;
083600100126                     V1Dmsg = $Msg(09);
083700100126                     leavesr;
083800100126                 endif;
083900100126           endif;
084000100126
084100090526
084200100924        //?Segno operazione da fare alla data della prossima attività
084300100924           if    V1Cupm = '=' and V1CGio <> 0 ;
084400090525             errMessage  = *on;
084500090525             errGenerico = *on;
084600100924             PosCurUPM   = *on;
084700100924             V1Dmsg = $Msg(14);
084800090525             leavesr;
084900100126           endif;
085000100127
085100100924           if   (V1Cupm = '+' or V1Cupm = '-') and V1CGio = 0 ;
085200100924             errMessage  = *on;
085300100924             errGenerico = *on;
085400100924             PosCurUPM   = *on;
085500100924             V1Dmsg = $Msg(15);
085600100924             leavesr;
085700100924           endif;
085800100924
085900100924           if    V1Cupm = ' ' and V1CGio <> 0 ;
086000100924             errMessage  = *on;
086100100924             errGenerico = *on;
086200100924             PosCurUPM   = *on;
086300100924             V1Dmsg = $Msg(16);
086400100924             leavesr;
086500100924           endif;
086600100127
086700100127        //?Opzioni inibite?
086800100127           SELECT;
086900100127
087000100127           WHEN  %check($_opz:V1Copz1) > 0;
087100100127             errMessage  = *on;
087200100127             errGenerico = *on;
087300100127             PosCurOPZ1  = *on;
087400100127             V1Dmsg = $Msg(05);
087500100127             leavesr;
087600100127
087700100127           WHEN  %check($_opz:V1Copz2) > 0;
087800100127             errMessage  = *on;
087900100127             errGenerico = *on;
088000100127             PosCurOPZ2  = *on;
088100100127             V1Dmsg = $Msg(05);
088200100127             leavesr;
088300100127
088400100127           WHEN  %check($_opz:V1Copz3) > 0;
088500100127             errMessage  = *on;
088600100127             errGenerico = *on;
088700100127             PosCurOPZ3  = *on;
088800100127             V1Dmsg = $Msg(05);
088900100127             leavesr;
089000100127
089100100127           WHEN  %check($_opz:V1Copz4) > 0;
089200100127             errMessage  = *on;
089300100127             errGenerico = *on;
089400100127             PosCurOPZ4  = *on;
089500100127             V1Dmsg = $Msg(05);
089600100127             leavesr;
089700100127
089800100127           WHEN  %check($_opz:V1Copz5) > 0;
089900100127             errMessage  = *on;
090000100127             errGenerico = *on;
090100100127             PosCurOPZ5  = *on;
090200100127             V1Dmsg = $Msg(05);
090300100127             leavesr;
090400100127           ENDSL;
090500090526
090600090526
090700090526       ENDSR;
090800090526
090900090526       //--------------------------------------------------------------
091000090526       //?Gestione videata D03                                         ?
091100090526       //--------------------------------------------------------------
091200090526       BEGSR  sr_GesD03;
091300090526
091400090526         //?Inizializzazione videata?
091500090526         if  $InzD02 = *on;
091600090526           exsr sr_InzD02;
091700090526           $InzD02 = *off;
091800090526         endif;
091900090526
092000090526         //?Emissione Testata e Piede con tasti funzionali abilitati?
092100090526         if  errGenerico = *off;
092200090526           write  TB75T01;
092300090526           write  TB75D01;
092400090526           write  TB75D02;
092500090526           write  Protect;
092600090526           write  TB75P02;
092700090526         endif;
092800090526
092900090526         //?Emissione videata?
093000090526         exfmt  TB75D03;
093100090526
093200090526         reset  ErrMessage;
093300090526         reset  ErrGenerico;
093400090526         clear  V1Dmsg;
093500090526         reset  $Annullamento;
093600090526
093700090526         SELECT;
093800090526
093900090526           //?F3=Fine?
094000090526           WHEN  dsp_aid = c_F03;
094100090526             unlock TNTBE01L;
094200090526             $Fine = *on;
094300090526
094400090526           //?F12=Ritorno?
094500090526           WHEN  dsp_aid = c_F12;
094600090526             $Video  = 'D2';
094700090526
094800090526           //?RollDown=Videata precedente?
094900090526           WHEN  dsp_aid = c_RollDown;
095000090526             $Video  = 'D2';
095100090526
095200090526
095300090526           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
095400090526           OTHER;
095500090526             $Ripristino   = (dsp_aid = c_F05);
095600090526             $Annullamento = (dsp_aid = c_F16);
095700090526             if  Not $Annullamento;
095800090526               exsr  sr_CtrD03;
095900090526               if  ErrGenerico;
096000090526                 leavesr;
096100090526               endif;
096200090526               exsr  sr_CtrD02;
096300090526               if  ErrGenerico;
096400090526                 $Video  = 'D2';
096500090526                 leavesr;
096600090526               endif;
096700090526             endif;
096800101210             select;
096900101210               when  dsp_aid = c_Enter  ;
097000101210                 $Video  = 'D4';
097100101210               when  dsp_aid = c_F05  or
097200101210                     dsp_aid = c_F06  or
097300101210                     dsp_aid = c_F16;
097400090526               $Video  = 'W1';
097500090526               reset  $InzW01;
097600101210             endsl;
097700090526
097800090526         ENDSL;
097900090526
098000090526       ENDSR;
098100090526
098200090526       //--------------------------------------------------------------
098300090526       //?Controllo dati videata D03                                   ?
098400090526       //--------------------------------------------------------------
098500090526       BEGSR  sr_CtrD03;
098600090526
098700101209         %subst(IndDspF : 40) = *off;
098800100504
098900100504         // -?Tipi trattativa?
099000100504         If  dsV1Ctpv <> *blank;
099100100504
099200100504           if  V1Ctra <> 'S';
099300100504             errMessage  = *on;
099400100504             errGenerico = *on;
099500100504             %subst(IndDspF : 62 : 5) = *on;
099600100504             V1Dmsg = $Msg(12);
099700100504             leavesr;
099800100504           endif;
099900100504
100000101209           For  xx = 1  To  5;
100100100504             if  $V1Ctpv(xx) = *blank;
100200100504               iter;
100300100504             endif;
100400100504             clear  TIBS02ds;
100500100504             T02mod = 'C';
100600100504             T02cod = 'TTR';
100700100504             T02ke1 = $V1Ctpv(xx);
100800100504             TNTBE_RicercaControllo  (kpjba : tibs02ds);
100900100504             if  T02err <> *blanks;
101000100504               errMessage  = *on;
101100100504               errGenerico = *on;
101200100504               %subst(IndDspF : 61 + xx : 1) = *on;
101300100504               V1Dmsg = $Msg(13);
101400100504               leavesr;
101500100504             endif;
101600100504           EndFor;
101700101209
101800101209           For  xx = 6  To  %elem($V1Ctpv);
101900101209             if  $V1Ctpv(xx) = *blank;
102000101209               iter;
102100101209             endif;
102200101209             clear  TIBS02ds;
102300101209             T02mod = 'C';
102400101209             T02cod = 'TTR';
102500101209             T02ke1 = $V1Ctpv(xx);
102600101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
102700101209             if  T02err <> *blanks;
102800101209               errMessage  = *on;
102900101209               errGenerico = *on;
103000101210               %subst(IndDspF : 34 + xx : 1) = *on;
103100101209               V1Dmsg = $Msg(13);
103200101209               leavesr;
103300101209             endif;
103400101209           EndFor;
103500100504
103600100504         EndIf;
103700100504
103800110429         // -?Categoria potenziali abilitate
103900110429         If  dsV1Ccpo <> *blank;
104000110429
104100110429           For  xx = 1  To  4;
104200110429             if  $V1Ccpo(xx) = *blank;
104300110429               iter;
104400110429             endif;
104500110429             IF  %scan('?' : $V1Ccpo(xx)) > 0;
104600110429                 ErrGenerico = *on;
104700110429                 %subst(IndDspF : 45 + xx : 1) = *on;
104800110429                 clear TIBS02ds;
104900110429                 T02mod = 'R';
105000110429                 T02cod = 'CPO';
105100110429                 T02sif = KNSIF;
105200110429                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
105300110429                 IF  T02err = *blanks;
105400110429                     $V1Ccpo(xx) = t02ke1;
105500110429                     errGenerico = *on;
105600110429                     leavesr;
105700110429                 else;
105800110429                     errMessage  = *on;
105900110429                     errGenerico = *on;
106000110429                     V1Dmsg = T02msg;
106100110429                     leavesr;
106200110429                 endif;
106300110429             endif;
106400110429             clear  TIBS02ds;
106500110429             T02mod = 'C';
106600110429             T02cod = 'CPO';
106700110429             T02ke1 = $V1Ccpo(xx);
106800110429             TNTBE_RicercaControllo  (kpjba : tibs02ds);
106900110429             if  T02err <> *blanks;
107000110429               errMessage  = *on;
107100110429               errGenerico = *on;
107200110429               %subst(IndDspF : 45 + xx : 1) = *on;
107300110429               V1Dmsg = $Msg(18);
107400110429               leavesr;
107500110429             endif;
107600110429           EndFor;
107700110429
107800110429         EndIf;
107900091113
108000101209         //?Controllo se causale abilitata solo ad un'altra causale Es la 72 si usa solo con la 71
108100101209           If V1CCae <> *blanks  ;
108200101210             clear  TIBS02ds;
108300101209             T02mod = 'C';
108400101209             T02cod = 'CCO';
108500101209             T02ke1 = V1Ccae;
108600101209             TNTBE_RicercaControllo  (kpjba : tibs02ds);
108700101209             if  T02err <> *blanks;
108800101209               errMessage  = *on;
108900101209               errGenerico = *on;
109000101210               PosCurCae = *on;
109100101209               V1Dmsg = $Msg(17);
109200101210               clear v1dcae ;
109300101209               leavesr;
109400101210             else ;
109500101210               v1dcae = t02uni;
109600101209             endif;
109700101210           endif;
109800090526
109900090526       ENDSR;
110000101210
110100101210       //--------------------------------------------------------------
110200101210       //?Gestione videata D04                                         ?
110300101210       //--------------------------------------------------------------
110400101210       BEGSR  sr_GesD04;
110500101210
110600101210         //?Inizializzazione videata?
110700101210         if  $InzD02 = *on;
110800101210           exsr sr_InzD02;
110900101210           $InzD02 = *off;
111000101210         endif;
111100101210
111200101210         //?Emissione Testata e Piede con tasti funzionali abilitati?
111300101210         if  errGenerico = *off;
111400101210           write  TB75T01;
111500101210           write  TB75D01;
111600101210           write  TB75D02;
111700101210           write  Protect;
111800101210           write  TB75P02;
111900101210         endif;
112000101210
112100101210         //?Emissione videata?
112200101210         exfmt  TB75D04;
112300101210
112400101210         reset  ErrMessage;
112500101210         reset  ErrGenerico;
112600101210         clear  V1Dmsg;
112700101210         reset  $Annullamento;
112800101210
112900101210         SELECT;
113000101210
113100101210           //?F3=Fine?
113200101210           WHEN  dsp_aid = c_F03;
113300101210             unlock TNTBE01L;
113400101210             $Fine = *on;
113500101210
113600101210           //?F12=Ritorno?
113700101210           WHEN  dsp_aid = c_F12;
113800101210             $Video  = 'D3';
113900101210
114000101210           //?RollDown=Videata precedente?
114100101210           WHEN  dsp_aid = c_RollDown;
114200101210             $Video  = 'D3';
114300101210
114400101210
114500101210           //?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
114600101210           OTHER;
114700101210             $Ripristino   = (dsp_aid = c_F05);
114800101210             $Annullamento = (dsp_aid = c_F16);
114900101210             if  Not $Annullamento;
115000101210               exsr  sr_CtrD04;
115100101210               if  ErrGenerico;
115200101210                 leavesr;
115300101210               endif;
115400101210               exsr  sr_CtrD03;
115500101210               if  ErrGenerico;
115600101210                 $Video  = 'D3';
115700101210                 leavesr;
115800101210               endif;
115900101210               exsr  sr_CtrD02;
116000101210               if  ErrGenerico;
116100101210                 $Video  = 'D2';
116200101210                 leavesr;
116300101210               endif;
116400101210             endif;
116500101210             if  dsp_aid = c_F05  or
116600101210                 dsp_aid = c_F06  or
116700101210                 dsp_aid = c_F16;
116800101210               $Video  = 'W1';
116900101210               reset  $InzW01;
117000101210             endif;
117100101210
117200101210         ENDSL;
117300101210
117400101210       ENDSR;
117500101210
117600090521
117700101209       //--------------------------------------------------------------
117800101209       //?Controllo dati videata D04                                   ?
117900101209       //--------------------------------------------------------------
118000101209       BEGSR  sr_CtrD04;
118100101209
118200101209
118300101209         %subst(IndDspF : 40) = *off;
118400101209
118500101209
118600101209         For  xx = 1  To  %elem($V1Cstp);
118700101209
118800101209           //?Stati PRECEDENTI possibili?
118900101209           select;
119000101209
119100101209             // - stato precedente NON inserito
119200101209             when  $V1Cstp(xx) = *blank;
119300101209               clear  $V1Dstp(xx);
119400101209
119500101209             // - Nessuno stato precedente (*blank)
119600101209             when  $V1Cstp(xx) = c_Blank;
119700101209               $V1Dstp(xx) = c_Blank_Des;
119800101209
119900101209             // - Qualsiasi stato precedente ("99")
120000101209             when  $V1Cstp(xx) = c_All;
120100101209               $V1Dstp(xx) = c_All_Des;
120200101209
120300101209             // - interrogazione/selezione stato precedente
120400101209             when  %scan('?' : $V1Cstp(xx)) > *zero;
120500101209               clear  ds1Q;
120600101209               X§Tkut = c_Kut;
120700101209               X§Tcod = '1Q';
120800101209               clear  X§Tkey;
120900101209               clear  X§Tdes;
121000101209               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
121100101209               $V1Cstp(xx) = %subst(X§Tkey : 1 : %len(V1Cstp1));
121200101209               $V1Dstp(xx) = %subst(X§Tdes : 1 : %len(V1Dstp1));
121300101209               errGenerico = *on;
121400101209               %subst(IndDspF : 66 + xx : 1) = *on;
121500101209               leavesr;
121600101209
121700101209             // - controllo stato precedente
121800101209             when  $V1Cstp(xx) <> *blank ;
121900101209               clear  ds1Q;
122000101209               k_TBLkut = c_Kut;
122100101209               k_TBLcod = '1Q';
122200101209               k_TBLkey = $V1Cstp(xx);
122300101209               chain %kds(k03tabel00)  TABEL;
122400101209               if  %found(TABEL00F)   and   TBLflg = *blank;
122500101209                 ds1Q = TBLuni ;
122600101209                 $V1Dstp(xx) = §1Qdes;
122700101209               else;
122800101209                 errMessage  = *on;
122900101209                 errGenerico = *on;
123000101209                 %subst(IndDspF : 66 + xx : 1) = *on;
123100101209                 V1Dmsg = $Msg(06);
123200101209                 leavesr;
123300101209               endif;
123400101209
123500101209           endsl;
123600101209
123700101209           //?Corrispondenza codici?
123800101209           select;
123900101209             when  $V1Cstp(xx) =  *blank   and   $V1Cstc(xx) <> *blank;
124000101209               errMessage  = *on;
124100101209               errGenerico = *on;
124200101209               %subst(IndDspF : 66 + xx : 1) = *on;
124300101209               V1Dmsg = $Msg(07);
124400101209               leavesr;
124500101209             when  $V1Cstp(xx) <> *blank   and   $V1Cstc(xx) =  *blank;
124600101209               errMessage  = *on;
124700101209               errGenerico = *on;
124800101209               %subst(IndDspF : 76 + xx : 1) = *on;
124900101209               V1Dmsg = $Msg(08);
125000101209               leavesr;
125100101209           endsl;
125200101209
125300101209           //?Stati FUTURI corrispondenti?
125400101209           select;
125500101209
125600101209             // - stato corrispondente NON inserito
125700101209             when  $V1Cstc(xx) = *blank;
125800101209               clear  $V1Dstc(xx);
125900101209
126000101209             // - Nessuno stato corrispondente (*blank)
126100101209             when  $V1Cstc(xx) = c_Blank and $V1Cstp(xx) <> c_blank;
126200101209               errMessage  = *on;
126300101209               errGenerico = *on;
126400101209               %subst(IndDspF : 76 + xx : 1) = *on;
126500101209               V1Dmsg = $Msg(08);
126600101209               leavesr;
126700101209
126800101209             // - Qualsiasi stato corrispondente ("99")
126900101209             when  $V1Cstc(xx) = c_All;
127000101209               $V1Dstc(xx) = c_All_Des;
127100101209
127200101209             // - Nessuno stato successivo ('BL')
127300101209             when  $V1Cstc(xx) = c_Blank;
127400101209               $V1Dstc(xx) = c_Blank_Des;
127500101209
127600101209             // - interrogazione/selezione stato corrispondente
127700101209             when  %scan('?' : $V1Cstc(xx)) > *zero;
127800101209               clear  ds1Q;
127900101209               X§Tkut = c_Kut;
128000101209               X§Tcod = '1Q';
128100101209               clear  X§Tkey;
128200101209               clear  X§Tdes;
128300101209               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
128400101209               $V1Cstc(xx) = %subst(X§Tkey : 1 : %len(V1Cstc1));
128500101209               $V1Dstc(xx) = %subst(X§Tdes : 1 : %len(V1Cstc1));
128600101209               errGenerico = *on;
128700101209               %subst(IndDspF : 76 + xx : 1) = *on;
128800101209               leavesr;
128900101209
129000101209             // - controllo stato futuro
129100101209             when  $V1Cstc(xx) <> *blank;
129200101209               clear  ds1Q;
129300101209               k_TBLkut = c_Kut;
129400101209               k_TBLcod = '1Q';
129500101209               k_TBLkey = $V1Cstc(xx);
129600101209               chain %kds(k03tabel00)  TABEL;
129700101209               if  %found(TABEL00F)   and   TBLflg = *blank;
129800101209                 ds1Q = TBLuni ;
129900101209                 $V1Dstc(xx) = §1Qdes;
130000101209               else;
130100101209                 errMessage  = *on;
130200101209                 errGenerico = *on;
130300101209                 %subst(IndDspF : 76 + xx : 1) = *on;
130400101209                 V1Dmsg = $Msg(06);
130500101209                 leavesr;
130600101209               endif;
130700101209
130800101209           endsl;
130900101209
131000101209         EndFor;
131100101209
131200101209       ENDSR;
131300101209
131400090521       //--------------------------------------------------------------
131500090521       //?Gestione trasmissione aggiornamento                          ?
131600090521       //--------------------------------------------------------------
131700090521       BEGSR  sr_GesW01;
131800090521
131900090521         // Inizializzazione videata
132000090521         if $InzW01 = *on;
132100090521           exsr  sr_InzW01;
132200090521           $InzW01 = *off;
132300090521         endif;
132400090521
132500090521         // Emissione window
132600090521         if  TBXftt = 'S';
132700090525           exfmt  TB75W01;
132800090521           ErrMessage  = *off;
132900090521           ErrGenerico = *off;
133000090521           clear  V1Dmsg;
133100090526         else;
133200090526           dsp_aid = c_F06;
133300090521         endif;
133400090521
133500090521         select;
133600090521
133700090521           // F12=Ritorno (gestito solo se emesso W01)
133800090521           when  dsp_aid = c_F12;
133900090521             $Video = 'D2';
134000090521
134100090521           // F6=Conferma
134200090521           when  dsp_aid = c_F06;
134300090526             exsr  sr_UpdTNTBE;
134400090521             // Ritorno alla videata iniziale
134500090521             if  NOT  ErrGenerico;
134600090521               clear  V1Topz;
134700090521               $Video  = 'D1';
134800090521               $InzD01 = *on;
134900090521             endif;
135000090521         endsl;
135100090521
135200090521       ENDSR;
135300090521
135400090521       //--------------------------------------------------------------
135500090521       //?Inizializzazione videata W01                                 ?
135600090521       //--------------------------------------------------------------
135700090521       BEGSR  sr_InzW01;
135800090521
135900090525         clear TB75W01;
136000090521
136100090521         if  $Immissione;
136200090521
136300090521           // Se immissione
136400090521           W1ftt  = TBXftt;
136500090521
136600090521         else;
136700090521
136800090521           // Se NON immissione:
136900090521           // visualizza i dati relativi all'ultima trasmissione
137000090521           W1ftt  = TBEftt;
137100090521           W1flt  = TBEflt;
137200090521           W1ftr  = TBEftr;
137300090521           if TBEdtr <> *zero;
137400090521             wDate_EUR = %date(TBEdtr : *iso);
137500090521             W1dtr     = %dec(wDate_EUR);
137600090521           endif;
137700090521
137800090521         endif;
137900090521
138000090521       ENDSR;
138100090521
138200090521       //--------------------------------------------------------------
138300090521       //?Aggiornamento records TNTBE00F (tab. "CCO")                  ?
138400090521       //--------------------------------------------------------------
138500090526       BEGSR  sr_UpdTNTBE;
138600090521
138700090521         //?Aggancio record di TNTBE00F?
138800090521         k_TBEcod  = c_Tab;
138900090521         k_TBEke1  = V1Ccam;
139000090521         clear  k_TBEke2;
139100090521         clear  k_TBElin;
139200090521         clear  k_TBEsif;
139300090521         //k_TBElin  = TBXlin;
139400090521         //k_TBEsif  = TBXsif;
139500090521
139600090521         chain  %kds(K05tntbe01) TNTBE000;
139700090526
139800090526         exsr  sr_RieCCO;
139900090526         TBEuni = dCCO;
140000090521
140100090521         //?Aggiornamento tab. "CCO"?
140200090521         SELECT;
140300090521
140400090521           // -?F5=Ripristino?
140500090521           WHEN  $Ripristino;
140600090521             clear  TBEatb;
140700090521             TBEftt = W1ftt;
140800090521             clear  TBEftr;
140900090521             //_______________
141000090521             UPDATE  TNTBE000;
141100090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
141200090521
141300090521           // -?F16=Annullamento?
141400090521           WHEN  $Annullamento;
141500090521             TBEatb = 'A';
141600090521             TBEftt = W1ftt;
141700090521             clear  TBEftr;
141800090521             //_______________
141900090521             UPDATE  TNTBE000;
142000090521             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
142100090521
142200090521           // -?Immissione o Modifica?
142300090521           OTHER;
142400090521
142500090521             if  NOT %found(TNTBE01L);
142600090521               clear TNTBE000;
142700090521               TBEcod  = k_TBEcod;
142800090521               TBEke1  = k_TBEke1;
142900090521               TBEke2  = k_TBEke2;
143000090521               TBElin  = k_TBElin;
143100090521               TBEsif  = k_TBEsif;
143200091016               TBEuni  = dCCO;
143300090521             endif;
143400090521
143500090521             TBEapl = TBXapl;
143600090521             clear  TBEatb;
143700090521             TBEftt = W1ftt;
143800090521             clear  TBEflt;
143900090521             clear  TBEftr;
144000090521             //clear TBEdtr;
144100090521
144200090521             // - Aggiornamento record
144300090521             if  %found(TNTBE01L);
144400090521               //_____________
144500090521               UPDATE TNTBE000;
144600090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
144700090521             else;
144800090521               //_____________
144900090521               WRITE  TNTBE000;
145000090521               //¯¯¯¯¯¯¯¯¯¯¯¯¯
145100090521             endif;
145200090521
145300090521         ENDSL;
145400090521
145500090521       ENDSR;
145600090521
145700090521       //--------------------------------------------------------------
145800090521       //?Caricamento dati in tab. "CCO"                               ?
145900090521       //--------------------------------------------------------------
146000090521       BEGSR  sr_RieCCO;
146100090521
146200090521         clear  dCCO;
146300090521
146400090521         §CCOdes = V1Cdes;
146500100126         §CCOord = V1Cord;
146600090521         §CCOgio = V1Cgio;
146700100126         §CCOtat = V1Ctat;
146800100126         §CCOatt = V1Catt;
146900100924         §CCOupm = V1Cupm;
147000100126         §CCOuti = V1Cuti;
147100100126         §CCOesi = V1Cesi;
147200100126         §CCOeaf = V1Ceaf;
147300100126         %subst(§CCOopz:1:1) = V1Copz1;
147400100126         %subst(§CCOopz:2:1) = V1Copz2;
147500100126         %subst(§CCOopz:3:1) = V1Copz3;
147600100126         %subst(§CCOopz:4:1) = V1Copz4;
147700100126         %subst(§CCOopz:5:1) = V1Copz5;
147800100126         §CCOnot = V1Cnot;
147900100126         §CCOsta = V1Csta;
148000090521         §CCOstp = dsV1Cstp;
148100090521         §CCOstc = dsV1Cstc;
148200091113         §CCOtra = V1Ctra;
148300091113         §CCOapt = V1Capt;
148400100126         §CCOeso = V1Ceso;
148500100219         %subst(§CCOtaa:1:1) = V1Caa1;
148600100219         %subst(§CCOtaa:2:1) = V1Caa2;
148700100219         %subst(§CCOtaa:3:1) = V1Caa3;
148800100219         %subst(§CCOtaa:4:1) = V1Caa4;
148900100219         %subst(§CCOtaa:5:1) = V1Caa5;
149000100219         %subst(§CCOtaa:6:1) = V1Caa6;
149100100219         %subst(§CCOtaa:7:1) = V1Caa7;
149200100219         %subst(§CCOtaa:8:1) = V1Caa8;
149300100219         %subst(§CCOtaa:9:1) = V1Caa9;
149400100219         %subst(§CCOtaa:10:1) = V1Ca10;
149500100219         §CCOoff = V1Coff;
149600100305         §CCOcht = V1Ccht;
149700110429         §CCOcod = Savcod;
149800100305         §CCOres = V1Cres;
149900100305         §CCOcoi = V1Ccom;
150000100504         §CCOtpv = dsV1Ctpv;
150100110429         §CCOcpo = dsV1Ccpo;
150200101210         §CCOhot = V1Chot;
150300101210         §CCOcae = V1Ccae;
150400101210         §CCOann = V1Cann;
150500101210         §CCOblc = V1Cblc;
150600101214         §CCOccp = V1Cccp;
150700110211         §CCOstl = V1Cstl;
150800110211         §CCOpgmblc = V1Cpgmblc;
150900110405         §CCOcns = V1Ccns;
151000110405         §CCOucns = V1Cucns;
151100120622         §CCOdpc = V1Cdpc;
151200090521
151300090521       ENDSR;
151400090521
151500090521       //--------------------------------------------------------------
151600090521       //?Operazioni finali.                                           ?
151700090521       //--------------------------------------------------------------
151800090521       BEGSR  sr_RoutEnd;
151900090521
152000090521         //?Chiusura funzioni precedentemente aperte?
152100090521         clear  TIBS02ds;
152200090521         T02tla = 'C';
152300090521         TNTBE_RicercaControllo ( kpjba : tibs02ds );
152400090521
152500090521         //?Chiusura pgm?
152600090521         return;
152700090521
152800090521       ENDSR;
152900090521
153000090521      /end-free
153100090521
153200090521       //--------------------------------------------------------------
153300090521       //?Definizione schiere a tempo di compilazione                  ?
153400090521       //--------------------------------------------------------------
153500090521
153600090521** > $Msg -------------------------------------------------------------------*
153700091113Immettere la Causale Contatto Marketing                                         1
153800090521Descrizione obbligatoria                                                        2
153900090521Giorni proposti per prossimo contatto NON possono essere negativi               3
154000090525Valore NON previsto.  Unici valori previsti: _=No, L=Sì                         4
154100100127Opzione errata                                                                  5
154200090525Codice stato inesistente in tab. "1Q" (o annullato)                             6
154300090526Ogni stato SUCCESSIVO inserito richiede lo stato PRECEDENTE corrispondente      7
154400090526Ogni stato PRECEDENTE inserito richiede lo stato SUCCESSIVO corrispondente      8
154500100126Tipo attività associata errata                                                  9
154600090629Tipo contatto futuro errato                                                     10
154700091113Se causale crea Trattativa non può non essere legata ad una Trattativa          11
154800100504Richiesto "Causale legata a Trattativa" = "S" per indicare "Tipi Trattativa"    12
154900100504Tipo trattativa errato                                                          13
155000100924Impostare giorni prossimo contatto a zero se segno =                            14
155100100924Impostare giorni prossimo contatto > di zero se segno + o -                     15
155200100924Se impostati i giorni prossimo contatto obbligatorio il segno + o -             16
155300101209Causale errata                                                                  17
155400110429Categoria Potenziale errata                                                     18
