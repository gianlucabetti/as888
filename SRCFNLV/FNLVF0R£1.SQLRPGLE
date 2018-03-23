000100090708      //---------------------------------------------------------------
000200090708      //?TRMK82R - Agenda commerciali
000300090708      //---------------------------------------------------------------
000400091123      //?ATTENZIONE!!!!
000500091123      //?Colori fissi a video per tipo attività
000600091123      //?Sono previsti, oltre che nel subfile anche nella WIN x legenda
000700130828      //?Blue      per Telefonata?
000800130828      //?Green     per Ufficio?
000900130828      //?Pink      per Ferie
001000130828      //?Red       per Appuntamento?
001100130828      //?Turquoise per Altre attività?
001200130828      //?White     per Offerta?
001300130828      //Yellow    per ______________?
001400091125
001500091125      //?Se il pgm imposta "Altre Attività" si carica un tipo attività di WORK =
001600091125      //?per ora il 'W' non corrisponde a nessuna attività ATCTAT
001700130828
001800130828       //--------------------------------------------------------------
001900130828       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002000130828       //--------------------------------------------------------------
002100130828
002200130828     /*END
002300130828
002400130828       //--------------------------------------------------------------
002500130828       //?Specifiche di controllo.                                     ?
002600130828       //--------------------------------------------------------------
002700090708
002800090708     h decedit('0,') datedit(*ymd/) option(*nodebugio)
002900091229     h dftactgrp(*no) actgrp(*caller)
003000090708
003100090708      //---------------------------------------------------------------
003200090708      //?Dichiarazione file.
003300090708      //---------------------------------------------------------------
003400130828
003500090804     fcnaco00f  if   e           k disk
003600090709     ftncpo01l  if   e           k disk
003700091222     fazcln01l  if   e           k disk
003800130828      // -?Anagrafica Commerciali?
003900130828     fAZCMM01L  if   e           k disk
004000100427      // - Video
004100090805     ftrmk82d   cf   e             workstn
004200090722     f                                     sfile(mk82S02 : S02nrr)
004300090708     f                                     indds(IndDspF)
004400090708     f                                     infds(InfDspF)
004500090708
004600090708      //---------------------------------------------------------------
004700090511      //?Definizione costanti.
004800090511      //---------------------------------------------------------------
004900090708
005000090708      // - Tasti funzionali a video
005100090708     d c_F01           c                   const(x'31')
005200090708     d c_F02           c                   const(x'32')
005300090708     d c_F03           c                   const(x'33')
005400090723     d c_F04           c                   const(x'34')
005500090708     d c_F05           c                   const(x'35')
005600090708     d c_F06           c                   const(x'36')
005700090708     d c_F07           c                   const(x'37')
005800090708     d c_F08           c                   const(x'38')
005900090708     d c_F09           c                   const(x'39')
006000090708     d c_F10           c                   const(x'3A')
006100090810     d c_F11           c                   const(x'3B')
006200090708     d c_F12           c                   const(x'3C')
006300090708     d c_F13           c                   const(x'B1')
006400090708     d c_F14           c                   const(x'B2')
006500090708     d c_F15           c                   const(x'B3')
006600090708     d c_F16           c                   const(x'B4')
006700090708     d c_F17           c                   const(x'B5')
006800090708     d c_F18           c                   const(x'B6')
006900090708     d c_F19           c                   const(x'B7')
007000090708     d c_F20           c                   const(x'B8')
007100090708     d c_F21           c                   const(x'B9')
007200090708     d c_F22           c                   const(x'BA')
007300090708     d c_F23           c                   const(x'BB')
007400090708     d c_F24           c                   const(x'BC')
007500090708     d c_Enter         c                   const(x'F1')
007600090708     d c_RollDown      c                   const(x'F4')
007700090708     d c_RollUp        c                   const(x'F5')
007800091123
007900091123        // - Colori a video
008000091123     d Blue            c                   const(x'3A')
008100091123     d Green           c                   const(x'20')
008200091123     d Pink            c                   const(x'38')
008300091123     d Red             c                   const(x'28')
008400091123     d Turquoise       c                   const(x'30')
008500091123     d White           c                   const(x'22')
008600091123     d Yellow          c                   const(x'32')
008700090723
008800090723     d Digits          c                   const('0123456789')
008900090723
009000100304     d c_AltreA        c                   const('*  *******  ')
009100091125     d c_AttAltre      c                   const('W')
009200090511
009300090511      //---------------------------------------------------------------
009400090511      //?Definizione schiere.
009500090511      //---------------------------------------------------------------
009600090708
009700090708      // - Messaggi di errore
009800100427     d $Msg            s             78    dim(11) ctdata perrcd(1)
009900090804
010000090805      // - Agenti
010100100416     d $Age            s              7  0 dim(200)
010200090709
010300090709      // - Data completa
010400090709     d $Date           s              8  0 dim(07)
010500090721
010600090721      // - Dettaglio
010700090910     d $Det            ds
010800090910     d $DetLunA                      10    dim(%elem($Hour))
010900090910     d $DetLunB                      10    dim(%elem($Hour))
011000090910     d $DetMarA                      10    dim(%elem($Hour))
011100090910     d $DetMarB                      10    dim(%elem($Hour))
011200090910     d $DetMerA                      10    dim(%elem($Hour))
011300090910     d $DetMerB                      10    dim(%elem($Hour))
011400090910     d $DetGioA                      10    dim(%elem($Hour))
011500090910     d $DetGioB                      10    dim(%elem($Hour))
011600090910     d $DetVenA                      10    dim(%elem($Hour))
011700090910     d $DetVenB                      10    dim(%elem($Hour))
011800090910
011900090805     d $DetLunAt       s              1    dim(%elem($Hour))
012000090805     d $DetLunBt       s              1    dim(%elem($Hour))
012100090805     d $DetMarAt       s              1    dim(%elem($Hour))
012200090805     d $DetMarBt       s              1    dim(%elem($Hour))
012300090805     d $DetMerAt       s              1    dim(%elem($Hour))
012400090805     d $DetMerBt       s              1    dim(%elem($Hour))
012500090805     d $DetGioAt       s              1    dim(%elem($Hour))
012600090805     d $DetGioBt       s              1    dim(%elem($Hour))
012700090805     d $DetVenAt       s              1    dim(%elem($Hour))
012800090805     d $DetVenBt       s              1    dim(%elem($Hour))
012900090805
013000090805      // - Dettaglio di work
013100090805     d wDetA           s             10    dim(%elem($Hour))
013200090805     d wDetAt          s              1    dim(%elem($Hour))
013300090805     d wDetB           s             10    dim(%elem($Hour))
013400090805     d wDetBt          s              1    dim(%elem($Hour))
013500090709
013600090709      // - Ora
013700090807     d $Hour           s              6  0 dim(48)
013800031216
013900090708      // - Nome del giorno
014000090709     d $Name_Day       s              3    dim(07)
014100090708
014200090708      // - Nome del mese
014300090709     d $Name_Month     s              9    dim(07)
014400090709
014500090709      // - Numero del giorno
014600090709     d $Number_Day     s              2  0 dim(07)
014700091222
014800091222      // - Festività
014900091222     d $Holidays       s              1    dim(07)
015000090708
015100090511      //---------------------------------------------------------------
015200090511      //?Definizione aree dati.
015300090511      //---------------------------------------------------------------
015400090708
015500090708      // - Dati utente
015600090708     d §AzUte        e ds                  extname(AZUTE00F)
015700090708     d                                     dtaara
015800090708     d §DatiUte      e ds                  extname(dDatiUte)
015900090708     d                                     dtaara
016000090511
016100090511      //---------------------------------------------------------------
016200090511      //?Definizione strutture dati.
016300090511      //---------------------------------------------------------------
016400090708
016500090708      // - Status
016600090708     d Psds           sds
016700090708     d   SDSpgm          *proc
016800090708
016900090708      // - InfDS
017000090708     d InfDspF         ds
017100090708     d  dsp_aid              369    369a
017200090724     d  dsp_rig              370    370
017300090724     d  dsp_col              371    371
017400090708
017500090708      // - Indicatori su DspF
017600090708     d IndDspF         ds
017700090806        // - Abilitazione tasti funzionali
017800091229     d  F10Attivo                     1n   overlay(IndDspF : 10)
017900091217     d  F11Attivo                     1n   overlay(IndDspF : 11)
018000090721        // - Indicatori di gestione del subfile
018100090721     d  SflDsp_N                      1n   overlay(IndDspF : 30)
018200090721     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
018300090722     d  SflEnd                        1n   overlay(IndDspF : 32)
018400090708        // - Indicatori di errore
018500090708     d  ErrMessage                    1n   overlay(IndDspF : 28)
018600091123     d  PosCurDATA                    1n   overlay(IndDspF : 50)
018700091123     d  PosCurAGE                     1n   overlay(IndDspF : 51)
018800091123     d  PosCurATT                     1n   overlay(IndDspF : 52)
018900091123     d  PosCurORA                     1n   overlay(IndDspF : 53)
019000091217     d  NotDsp                        1n   overlay(IndDspf : 40)
019100090708     d  ErrGenerico                   1n   overlay(IndDspF : 99)
019200090724
019300090724      // Cursore
019400090724     d ds_cursor       ds
019500090724     d  ds_riga                1      2b 0 inz
019600090724     d  $riga                  2      2
019700090724     d  ds_colo                3      4b 0 inz
019800090724     d  $colo                  4      4
019900091222
020000091222      // - Mattino/Pomeriggio calendario
020100091222     d CLNmat          DS
020200091222     d  mat                    1     31    dim(31)
020300091222     d CLNpom          DS
020400091222     d  pom                    1     31    dim(31)
020500090724
020600090708      // - Controllo data
020700090708     d wlbdat          ds                  inz
020800090708     d  g02dat                 1      8  0
020900090708     d  g02inv                 9     16  0
021000090708     d  g02err                17     17
021100090708     d  g02tgi                18     22  0
021200090708
021300090708      // - Parametri ricevuti
021400090708     d KPJBA         e ds
021500090708     d TRMK82ds      e ds
021600090805     d  $NoAtt                41     50    dim(10)
021700090708
021800090708      // - Reperimento dati utente
021900090708     d TIBS34ds      e ds
022000090805
022100090805      // - Gestione tabelle: controllo e ricerca
022200090805     d TIBS02ds      e ds                  inz
022300090805     d   T02tla      e                     inz('L')
022400090805     d   T02cod      e                     inz('TTA')
022500100208
022600100208      // - controllo abilitazioni
022700100208     d TNTAA1DS      e ds                  inz
022800091228
022900091228      // - Inserimento attività
023000091228     d TRMK17DS      e ds
023100090806
023200090806      // - Interrogazione attività
023300090806     d TRMK83DS      e ds
023400100308
023500100308      // - Stampa agenda
023600100308     d TRMK86DS      e ds
023700100202
023800100202      // Abilitazioni utente
023900100202     d dUte01        e ds
024000090709
024100090709      // File attività
024200090709     d tiatc00f      e ds                  extname(TIATC00F)
024300130828
024400100305      // File trattativa
024500100305     d tivis00f      e ds                  extname(TIVIS00F)
024600090708
024700090708      // Campi di comodo per gestione indicatori a video
024800090708     d WindDspF        ds                  inz
024900091123     d   WindDspFa             1     49    inz(*zero)
025000091123     d   WindDspFb            50     99    inz(*zero)
025100090511
025200090511      //---------------------------------------------------------------
025300090511      //?Definizione variabili globali.
025400090511      //---------------------------------------------------------------
025500090708
025600090708      // - Flags booleani
025700090805     d $End            s               n   inz(*off)
025800090805     d $EndAge         s               n   inz(*off)
025900100202     d $ErrGrave       s               n   inz(*off)
026000090807     d $Fine           s               n   inz(*off)
026100090807     d $Hour1          s               n   inz(*off)
026200090807     d $Hour2          s               n   inz(*off)
026300090807     d $InzD01         s               n   inz(*on)
026400090807     d $InzS02         s               n   inz(*off)
026500090807     d $Minute30       s               n   inz(*off)
026600100305     d $RcdOk          s               n   inz(*off)
026700091111     d $Richiamato     s               n   inz(*off)
026800090807     d $Week           s               n   inz(*off)
026900090708
027000090708      // - Campi associati al video
027100090708     d $Video          s              2    inz('D1')
027200090722     d S02Nrr          s              4  0 inz
027300090804
027400090804       // - Stringa SQL da eseguire
027500090804     d wSQL            s           2048    Varying        inz
027600090708
027700090708      // - Campi di comodo
027800090805     d sav_atcdad      s                   like(atcdad)
027900090807     d sav_xx          s                   like(xx)
028000090721     d watcoii         s              6  0 inz
028100090721     d watcofi         s              6  0 inz
028200090724     d wColo           s              3s 0
028300090708     d wDay_Of_Week    s              2  0 inz
028400090721     d wdata           s              8  0
028500090721     d wDate_EUR       s               d   datfmt(*eur)
028600090721     d wDate_ISO       s               d   datfmt(*iso)
028700090721     d wDate_ISO_Par   s               d   datfmt(*iso)
028800090721     d wDesCpo         s             10
028900090721     d wHour           s              2  0 inz
029000090721     d wHour_Iso       s               t   inz(*loval)
029100090721     d wHour_Iso_F     s               t   inz
029200090721     d wHour_Iso_I     s               t   inz
029300100504     d wMin            s              2  0 inz
029400090708     d wName_Of_Day    s             10    inz
029500090708     d wName_Of_Month  s             10    inz
029600090807     d wRiga           s              3s 0
029700090807     d wTot_H          s              5  0 inz
029800100504     d wTot_M          s              5  0 inz
029900100504     d wTot_R          s              5  0 inz
030000090807     d wxxh1           s              2  0 inz(9)
030100090807     d wxxh2           s              2  0 inz(3)
030200090807     d wxxm            s              2  0 inz(17)
030300090804     d xx              s              3  0 inz
030400090804     d yy              s              3  0 inz
030500090805     d zz              s              3  0 inz
030600090511
030700090511      //---------------------------------------------------------------
030800090511      //?Definizione procedure esterne.
030900090511      //---------------------------------------------------------------
031000090708
031100090708      // - Reperimento dati utente
031200090708     d tibs34r         pr                  extpgm('TIBS34R')
031300090708     d  tibs34ds                           likeds(TIBS34ds)
031400090805
031500090805      // - Gestione tabelle
031600090805     d tibs02r         pr                  extpgm('TIBS02R')
031700090805     d  kpjba                              likeds(KPJBA)    const
031800090805     d  tibs02ds                           likeds(TIBS02ds)
031900091228
032000091228      // - Inserimento attività
032100091228     d trmk17r         pr                  extpgm('TRMK17R')
032200091228     d  kpjba                              likeds(KPJBA)
032300091228     d  trmk17ds                           likeds(TRMK17ds)
032400130828
032500130828       // -?Ricerca commerciali?
032600130828      /copy gaitrasrc/srcprotoPI,TRMK43R_1
032700130828      /copy gaitrasrc/srcprotoPR,TRMK43R
032800090806
032900090806      // - Interrogazione attività
033000090806     d trmk83r         pr                  extpgm('TRMK83R')
033100090806     d  kpjba                              likeds(KPJBA)
033200091111     d  trmk83ds                           likeds(TRMK83ds)
033300100308
033400100308      // - Stampa agenda
033500100308     d trmk86r         pr                  extpgm('TRMK86R')
033600100308     d  kpjba                              likeds(KPJBA)
033700100308     d  trmk86ds                           likeds(TRMK86ds)
033800090708
033900100208      /copy gaitrasrc/srcprotopr,tntaa1c
034000090708      /copy gaitrasrc/srcprotopr,xsrda8
034100090511
034200090511      //---------------------------------------------------------------
034300090511      //?Definizione key-list.
034400090511      //---------------------------------------------------------------
034500090804
034600090804       // - File CNACO00F
034700090804     d k03cnaco      e ds                  extname(CNACO00F:*key)
034800090804     d                                     prefix(k_)
034900090804     d                                     inz
035000091222
035100091222       // - File AZCLN01L
035200091222     d k04azcln      e ds                  extname(AZCLN01L:*key)
035300091222     d                                     prefix(k_)
035400091222     d                                     inz
035500090708
035600090708      //---------------------------------------------------------------
035700090708      //?Riepilogo indicatori.
035800090708      //---------------------------------------------------------------
035900090708
036000090708      // 28    : Emissione messaggio di errore a video
036100090807      // 30    : Condiziona SFLDSP
036200090807      // 31    : Condiziona SFLDSPCTL
036300090807      // 32    : Condiziona SFLEND
036400091123      // 50    : Posizionamento Cursore Data
036500091123      // 51    : Posizionamento Cursore Commerciale
036600091123      // 52    : Posizionamento Cursore Attività
036700091123      // 53    : Posizionamento Cursore Ore
036800100202      // 60    : Utente EDPMB visualizzo attività e ora in prima videata
036900090708      // 99    : Generico di Errore
037000090511
037100090511      //---------------------------------------------------------------
037200090708      //?M A I N - L I N E
037300090511      //---------------------------------------------------------------
037400090708
037500090708     c     *Entry        plist
037600090708     c                   parm                    KPJBA
037700090708
037800090708      /free
037900090708
038000090708       //?Operazioni iniziali
038100090708       exsr sr_RoutInz;
038200090708
038300090708       //?Gestione video
038400090722       DOW  $Fine = *off;
038500090722
038600090722         SELECT;
038700090722           WHEN  $Video = 'D1';
038800090708             exsr sr_GesD01;
038900090722           WHEN  $Video = 'S2';
039000090722             exsr sr_GesS02;
039100090722           OTHER;
039200090708             $Fine = *on;
039300090722         ENDSL;
039400090722
039500090708       ENDDO;
039600090708
039700090708       //?Operazioni finali
039800090708       exsr sr_RoutEnd;
039900090708
040000090708       //--------------------------------------------------------------
040100090708       //?Operazioni iniziali.
040200090708       //--------------------------------------------------------------
040300090708       BEGSR sr_RoutInz;
040400090708
040500090708         //?Ricezione parametri
040600091027         IF  %subst(kpjbu:1:1) <> *blanks;
040700090708           //?pgm richiamato
040800090708           TRMK82ds = kpjbu;
040900090722           $video = 'S2';
041000090722           $inzS02 = *on;
041100091111           $richiamato = *on;
041200090722         ELSE;
041300090708           //?pgm non richiamato
041400090708           clear TRMK82ds;
041500090708           $video = 'D1';
041600090708           $inzD01 = *on;
041700091111           $richiamato = *off;
041800090722         ENDIF;
041900091123
042000100202         //?Utente EDPMB
042100100419         NotDsp = *off;
042200100504         IF  %subst(knmus:1:5) = 'EDPMB';
042300100504            // %subst(knmus:1:5) = 'ROSS ';
042400100419           NotDsp = *on;
042500100419         ENDIF;
042600090708
042700090708         //?Reperimento dati job
042800090708         exsr sr_DatiJob;
042900100305
043000100305         //?Impostazione campi "fissi"
043100100305         T01pgm = SDSpgm;
043200100305         k_acokut = 1;
043300100305         k_acokcc = dutkci;
043400090708
043500090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
043600100208
043700100208         //?Controllo se utente abilitato alla funzione
043800100208         reset TNTAA1DS;
043900100208         ITAA1caut = '§utegtc';
044000100208         kpjbu     = TNTAA1DS;
044100100208         tntaa1c (kpjba);
044200100208         TNTAA1DS = kpjbu;
044300100208
044400100208         IF  OTAA1fabi = 'N';
044500100208           $ErrGrave   = *on;
044600100208           ErrMessage  = *on;
044700100208           ErrGenerico = *on;
044800100208           V1Dmsg      = $Msg(10);
044900100208           leavesr;
045000100208         ENDIF;
045100091229
045200091229         //?Abilito F10-Attività
045300110915         // anche se non sono in sede
045400110915         F10Attivo = (IMK82rich = ' ' and dutpou <> 046);
045500090805
045600091026         //?Se richiamato
045700091026         IF  $video = 'S2';
045800091026         //?Se data non passata imposto data del giorno
045900091111           IF IMK82Data = 0;
046000091111             IMK82Data = %dec(%date());
046100091026           ENDIF;
046200091026         //?Se Tempo di visualizzazione non passato imposto 1 ora
046300091111           IF IMK82Time = *blanks;
046400091111             IMK82Time = '1';
046500091026           ENDIF;
046600091026         //?Se impostato l'agente carico sk agenti
046700091111           IF  IMK82cAge > 0;
046800100203             exsr sr_CarAGE;
046900091026           ELSE;
047000091026         //?Se non impostato imposto tutto a 0
047100091026             clear $Age;
047200091026           ENDIF;
047300091026         //?Imposto i flag per tempo da visualizzare
047400091111           $Hour1 = (IMK82Time = '1');
047500091111           $Hour2 = (IMK82Time = '2');
047600091111           $Minute30 = (IMK82Time = '3');
047700090805         ENDIF;
047800090708
047900090708       ENDSR;
048000090708
048100090708       //--------------------------------------------------------------
048200090708       //?Reperimento Dati del job (Utente/Operativi).
048300090708       //--------------------------------------------------------------
048400090708       BEGSR sr_DatiJob;
048500090708
048600090708         in(E) §AzUte;
048700090722         IF  NOT %error;
048800090708           in(E) §DatiUte;
048900090722         ENDIF;
049000090722         IF  %error or RSut = *blanks;
049100090708           clear TIBS34ds;
049200090708           tibs34r(tibs34ds);
049300090708           in §AzUte;
049400090708           in §DatiUte;
049500090722         ENDIF;
049600090708
049700090708       ENDSR;
049800090805
049900090805       //--------------------------------------------------------------
050000090805       //?Carico Sk Agenti.
050100090805       //--------------------------------------------------------------
050200090805       BEGSR sr_CarAGE;
050300090805
050400090805         clear xx;
050500090805         clear $Age;
050600100309         $EndAge = *off;
050700090807
050800090805         exec sql
050900090805          declare AGE cursor for
051000130828          select CMMcod, CMMuni
051100130828          from AZCMM00F
051200130828          order by CMMcod
051300130828          for fetch only;
051400090805
051500090805         exec sql
051600090805           open AGE;
051700090805           IF sqlcode < 0;
051800090805             $EndAge = *on;
051900090805           ENDIF;
052000090805
052100090805         DOW not $EndAge;
052200090805           exec sql
052300130828             fetch next from AGE into :CMMcod, :CMMuni;
052400090805             IF sqlcod = 100 or sqlcod < 0;
052500090805               $EndAge = *on;
052600090805               leave;
052700090805             ENDIF;
052800130828             IF  CMMuni = IMK82cAGE;
052900090805               xx += 1;
053000130828               $Age(xx) = CMMcod;
053100090805             ENDIF;
053200090805         ENDDO;
053300090805
053400090805         exec sql close AGE;
053500090810
053600130828         //?Se non ho caricato niente memorizzo almeno il codice richiesto
053700090810         IF $Age(01) = 0;
053800091111           $Age(01) = IMK82cAGE;
053900090810         ENDIF;
054000090805
054100090805       ENDSR;
054200090708
054300090708       //--------------------------------------------------------------
054400090805       //?Gestione videata D01.
054500090708       //--------------------------------------------------------------
054600090708       BEGSR sr_GesD01;
054700090708
054800090708         //?Inizializzazione videata
054900090722         IF  $InzD01 = *on;
055000090708           exsr sr_InzD01;
055100090708           $InzD01 = *off;
055200090722         ENDIF;
055300090708
055400090708         //?Emissione videata
055500090708         write MK82T01;
055600090708         exfmt MK82D01;
055700090708         ErrMessage  = *off;
055800090708         ErrGenerico = *off;
055900090708         clear V1Dmsg;
056000090708
056100090708         SELECT;
056200090708
056300100202         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
056400100202           WHEN  $ErrGrave;
056500100202             $Fine  = *on;
056600100202
056700090708           //?F3=Fine
056800090722           WHEN  dsp_aid = c_F03;
056900090708             exsr sr_F03D01;
057000090708
057100090708           //?Enter
057200090708           OTHER;
057300090708             //?- Controllo dati
057400090708             exsr sr_CtrD01;
057500090722             IF  errGenerico;
057600090708               leavesr;
057700090722             ENDIF;
057800090722             $video = 'S2';
057900090722             $inzS02 = *on;
058000090708
058100090708         ENDSL;
058200090708
058300090708       ENDSR;
058400090708
058500090708       //--------------------------------------------------------------
058600090805       //?Inizializzazione videata D01.
058700090708       //--------------------------------------------------------------
058800090708       BEGSR sr_InzD01;
058900090708
059000090708         clear MK82D01;
059100090708
059200090708         //?imposto la data del giorno
059300090708          wdata = %dec(%date());
059400090708          wDate_ISO = %date(wdata:*iso);
059500090708          wDate_EUR = wDate_ISO;
059600090708          V1Data = %dec(wDate_EUR);
059700090907
059800090907          V1Hour1 = 'X';
059900090708
060000090708       ENDSR;
060100090708
060200090708       //--------------------------------------------------------------
060300090805       //?Gestione tasto funzionale F3 da videata D01.
060400090708       //?F3=Fine
060500090708       //--------------------------------------------------------------
060600090708       BEGSR sr_F03D01;
060700091111
060800090708         //?Chiusura del programma
060900090708         $Fine = *on;
061000090708
061100090708       ENDSR;
061200090708
061300090708       //--------------------------------------------------------------
061400090805       //?Controllo videata D01.
061500090708       //--------------------------------------------------------------
061600090708       BEGSR sr_CtrD01;
061700090708
061800090708         WindDspF = IndDspF;
061900090708         reset WindDspFb;
062000090708         IndDspF  = WindDspF;
062100090708
062200090708         //?Controllo data
062300090722         IF  V1Data    = *zeros;
062400090722           ErrMessage   = *on;
062500090722           ErrGenerico  = *on;
062600090722           PosCurDATA   = *on;
062700090722           V1Dmsg       = $Msg(01);
062800090722           leavesr;
062900090722         ENDIF;
063000090708
063100090722         clear wlbdat;
063200090722         g02dat = V1Data;
063300090722         xsrda8(wlbdat);
063400090722         IF  g02err    = '1';
063500090722           ErrMessage   = *on;
063600090722           ErrGenerico  = *on;
063700090722           PosCurDATA   = *on;
063800090722           V1Dmsg       = $Msg(02);
063900090722           leavesr;
064000090722         ENDIF;
064100090708
064200090722         V1Data = g02dat;
064300090722         wdata  = g02inv;
064400090708
064500090708         //?Controllo commerciale
064600090723         IF  V1cAGE   =  *blanks or V1cAGE = *zeros;
064700090722           errMessage  = *on;
064800090722           errGenerico = *on;
064900090722           PosCurAGE   = *on;
065000090722           V1Dmsg      = $Msg(03);
065100090722           leavesr;
065200090722         ENDIF;
065300090723
065400090723         IF  %scan('?' : v1cage) > 0;
065500090723            errGenerico = *on;
065600090723            PosCurAGE   = *on;
065700130828             clear  TRMK43ds;
065800130828             iMK43solU = 'S';
065900130828             iMK43fil  = DUTpou;
066000130828             TRMK43R (kpjba : TRMK43ds);
066100130828             if  oMK43err = *off  and  oMK43fxx = *blank;
066200130828               V1cAGE = %editc( oMK43cmm : 'X' );
066300130828               V1dAGE = oMK43des;
066400130828             endif;
066500090723            leavesr;
066600090723         ENDIF;
066700090723
066800090723         IF  %check(digits:V1cAGE) > 0;
066900090723           errMessage  = *on;
067000090723           errGenerico = *on;
067100090723           PosCurAGE   = *on;
067200090723           V1Dmsg      = $Msg(04);
067300090723           leavesr;
067400090723         ENDIF;
067500090708
067600130828         chain  (%int(V1Cage))  AZCMM000;
067700130828         IF  not %found( AZCMM01L );
067800090722           errMessage  = *on;
067900090722           errGenerico = *on;
068000090722           PosCurAGE   = *on;
068100090722           V1Dmsg      = $Msg(04);
068200090722           leavesr;
068300090722         ENDIF;
068400100202
068500100208         clear TNTAA1DS;
068600100208         ITAA1caut = '§utegtc';
068700100208         ITAA1cmm  = %dec(V1cAGE:7:0);
068800100208         kpjbu     = tntaa1ds;
068900100208         tntaa1c (kpjba);
069000100208         TNTAA1DS = kpjbu;
069100100208         IF  OTAA1fabi = 'N';
069200100202           ErrMessage  = *on;
069300100202           ErrGenerico = *on;
069400100202           PosCurAGE   = *on;
069500100202           V1Dmsg      = $Msg(04);
069600100202           leavesr;
069700100202         ENDIF;
069800090708
069900130828         V1dAGE = CMMdes;
070000110518
070100130828         IF  CMMpar <> *blanks;
070200110518           ErrMessage  = *on;
070300110518           ErrGenerico = *on;
070400110518           PosCurAGE   = *on;
070500110518           V1Dmsg      = $Msg(04);
070600110518           leavesr;
070700110518         ENDIF;
070800090804
070900130828         IF  %dec(V1cAGE:7:0) <> CMMuni;
071000090804           errMessage  = *on;
071100090804           errGenerico = *on;
071200090804           PosCurAGE   = *on;
071300090805           V1Dmsg      = $Msg(08);
071400090804           leavesr;
071500090804         ENDIF;
071600100202
071700090803         //?Attività da escludere
071800090805         IF  %scan('?' : V1Att01) > *zeros;
071900090805             errGenerico = *on;
072000091123             PosCurAtt   = *on;
072100090805             clear kpjbu;
072200090805             reset tibs02ds;
072300090805             T02mod = 'R';
072400090805             tibs02r(kpjba:tibs02ds);
072500090805             V1Att01 = t02ke1;
072600090805             leavesr;
072700090805           ENDIF;
072800090805         IF  %scan('?' : V1Att02) > *zeros;
072900090805             errGenerico = *on;
073000091123             PosCurAtt   = *on;
073100090805             clear kpjbu;
073200090805             reset tibs02ds;
073300090805             T02mod = 'R';
073400090805             tibs02r(kpjba:tibs02ds);
073500090805             V1Att02 = t02ke1;
073600090805             leavesr;
073700090805           ENDIF;
073800090805         IF  %scan('?' : V1Att03) > *zeros;
073900090805             errGenerico = *on;
074000091123             PosCurAtt   = *on;
074100090805             clear kpjbu;
074200090805             reset tibs02ds;
074300090805             T02mod = 'R';
074400090805             tibs02r(kpjba:tibs02ds);
074500090805             V1Att03 = t02ke1;
074600090805             leavesr;
074700090805           ENDIF;
074800090805         IF  %scan('?' : V1Att04) > *zeros;
074900090805             errGenerico = *on;
075000091123             PosCurAtt   = *on;
075100090805             clear kpjbu;
075200090805             reset tibs02ds;
075300090805             T02mod = 'R';
075400090805             tibs02r(kpjba:tibs02ds);
075500090805             V1Att04 = t02ke1;
075600090805             leavesr;
075700090805           ENDIF;
075800090805         IF  %scan('?' : V1Att05) > *zeros;
075900090805             errGenerico = *on;
076000091123             PosCurAtt   = *on;
076100090805             clear kpjbu;
076200090805             reset tibs02ds;
076300090805             T02mod = 'R';
076400090805             tibs02r(kpjba:tibs02ds);
076500090805             V1Att05 = t02ke1;
076600090805             leavesr;
076700090805           ENDIF;
076800090805         IF  %scan('?' : V1Att06) > *zeros;
076900090805             errGenerico = *on;
077000091123             PosCurAtt   = *on;
077100090805             clear kpjbu;
077200090805             reset tibs02ds;
077300090805             T02mod = 'R';
077400090805             tibs02r(kpjba:tibs02ds);
077500090805             V1Att06 = t02ke1;
077600090805             leavesr;
077700090805           ENDIF;
077800090805         IF  %scan('?' : V1Att07) > *zeros;
077900090805             errGenerico = *on;
078000091123             PosCurAtt   = *on;
078100090805             clear kpjbu;
078200090805             reset tibs02ds;
078300090805             T02mod = 'R';
078400090805             tibs02r(kpjba:tibs02ds);
078500090805             V1Att07 = t02ke1;
078600090805             leavesr;
078700090805           ENDIF;
078800090805         IF  %scan('?' : V1Att08) > *zeros;
078900090805             errGenerico = *on;
079000091123             PosCurAtt   = *on;
079100090805             clear kpjbu;
079200090805             reset tibs02ds;
079300090805             T02mod = 'R';
079400090805             tibs02r(kpjba:tibs02ds);
079500090805             V1Att08 = t02ke1;
079600090805             leavesr;
079700090805           ENDIF;
079800090805         IF  %scan('?' : V1Att09) > *zeros;
079900090805             errGenerico = *on;
080000091123             PosCurAtt   = *on;
080100090805             clear kpjbu;
080200090805             reset tibs02ds;
080300090805             T02mod = 'R';
080400090805             tibs02r(kpjba:tibs02ds);
080500090805             V1Att09 = t02ke1;
080600090805             leavesr;
080700090805           ENDIF;
080800090805         IF  %scan('?' : V1Att10) > *zeros;
080900090805             errGenerico = *on;
081000091123             PosCurAtt   = *on;
081100090805             clear kpjbu;
081200090805             reset tibs02ds;
081300090805             T02mod = 'R';
081400090805             tibs02r(kpjba:tibs02ds);
081500090805             V1Att10 = t02ke1;
081600090805             leavesr;
081700090805           ENDIF;
081800090805
081900090805         IF  V1Att01 <> *blanks;
082000090805           clear kpjbu;
082100090805           reset tibs02ds;
082200090805           T02mod = 'C';
082300090805           T02Ke1 = V1Att01;
082400090805           tibs02r(kpjba:tibs02ds);
082500090805           IF  T02Err <> *blanks;
082600090805             errMessage  = *on;
082700090805             errGenerico = *on;
082800091123             PosCurAtt   = *on;
082900090805             V1Dmsg      = $Msg(07);
083000090805             leavesr;
083100090805           ENDIF;
083200090803         ENDIF;
083300090805         IF  V1Att02 <> *blanks;
083400090805           clear kpjbu;
083500090805           reset tibs02ds;
083600090805           T02mod = 'C';
083700090805           T02Ke1 = V1Att02;
083800090805           tibs02r(kpjba:tibs02ds);
083900090805           IF  T02Err <> *blanks;
084000090805             errMessage  = *on;
084100090805             errGenerico = *on;
084200091123             PosCurAtt   = *on;
084300090805             V1Dmsg      = $Msg(07);
084400090805             leavesr;
084500090805           ENDIF;
084600090805         ENDIF;
084700090805         IF  V1Att03 <> *blanks;
084800090805           clear kpjbu;
084900090805           reset tibs02ds;
085000090805           T02mod = 'C';
085100090805           T02Ke1 = V1Att03;
085200090805           tibs02r(kpjba:tibs02ds);
085300090805           IF  T02Err <> *blanks;
085400090805             errMessage  = *on;
085500090805             errGenerico = *on;
085600091123             PosCurAtt   = *on;
085700090805             V1Dmsg      = $Msg(07);
085800090805             leavesr;
085900090805           ENDIF;
086000090805         ENDIF;
086100090805         IF  V1Att04 <> *blanks;
086200090805           clear kpjbu;
086300090805           reset tibs02ds;
086400090805           T02mod = 'C';
086500090805           T02Ke1 = V1Att04;
086600090805           tibs02r(kpjba:tibs02ds);
086700090805           IF  T02Err <> *blanks;
086800090805             errMessage  = *on;
086900090805             errGenerico = *on;
087000091123             PosCurAtt   = *on;
087100090805             V1Dmsg      = $Msg(07);
087200090805             leavesr;
087300090805           ENDIF;
087400090805         ENDIF;
087500090805         IF  V1Att05 <> *blanks;
087600090805           clear kpjbu;
087700090805           reset tibs02ds;
087800090805           T02mod = 'C';
087900090805           T02Ke1 = V1Att05;
088000090805           tibs02r(kpjba:tibs02ds);
088100090805           IF  T02Err <> *blanks;
088200090805             errMessage  = *on;
088300090805             errGenerico = *on;
088400091123             PosCurAtt   = *on;
088500090805             V1Dmsg      = $Msg(07);
088600090805             leavesr;
088700090805           ENDIF;
088800090805         ENDIF;
088900090805         IF  V1Att06 <> *blanks;
089000090805           clear kpjbu;
089100090805           reset tibs02ds;
089200090805           T02mod = 'C';
089300090805           T02Ke1 = V1Att06;
089400090805           tibs02r(kpjba:tibs02ds);
089500090805           IF  T02Err <> *blanks;
089600090805             errMessage  = *on;
089700090805             errGenerico = *on;
089800091123             PosCurAtt   = *on;
089900090805             V1Dmsg      = $Msg(07);
090000090805             leavesr;
090100090805           ENDIF;
090200090805         ENDIF;
090300090805         IF  V1Att07 <> *blanks;
090400090805           clear kpjbu;
090500090805           reset tibs02ds;
090600090805           T02mod = 'C';
090700090805           T02Ke1 = V1Att07;
090800090805           tibs02r(kpjba:tibs02ds);
090900090805           IF  T02Err <> *blanks;
091000090805             errMessage  = *on;
091100090805             errGenerico = *on;
091200091123             PosCurAtt   = *on;
091300090805             V1Dmsg      = $Msg(07);
091400090805             leavesr;
091500090805           ENDIF;
091600090805         ENDIF;
091700090805         IF  V1Att08 <> *blanks;
091800090805           clear kpjbu;
091900090805           reset tibs02ds;
092000090805           T02mod = 'C';
092100090805           T02Ke1 = V1Att08;
092200090805           tibs02r(kpjba:tibs02ds);
092300090805           IF  T02Err <> *blanks;
092400090805             errMessage  = *on;
092500090805             errGenerico = *on;
092600091123             PosCurAtt   = *on;
092700090805             V1Dmsg      = $Msg(07);
092800090805             leavesr;
092900090805           ENDIF;
093000090805         ENDIF;
093100090805         IF  V1Att09 <> *blanks;
093200090805           clear kpjbu;
093300090805           reset tibs02ds;
093400090805           T02mod = 'C';
093500090805           T02Ke1 = V1Att09;
093600090805           tibs02r(kpjba:tibs02ds);
093700090805           IF  T02Err <> *blanks;
093800090805             errMessage  = *on;
093900090805             errGenerico = *on;
094000091123             PosCurAtt   = *on;
094100090805             V1Dmsg      = $Msg(07);
094200090805             leavesr;
094300090805           ENDIF;
094400090805         ENDIF;
094500090805         IF  V1Att10 <> *blanks;
094600090805           clear kpjbu;
094700090805           reset tibs02ds;
094800090805           T02mod = 'C';
094900090805           T02Ke1 = V1Att10;
095000090805           tibs02r(kpjba:tibs02ds);
095100090805           IF  T02Err <> *blanks;
095200090805             errMessage  = *on;
095300090805             errGenerico = *on;
095400091123             PosCurAtt   = *on;
095500090805             V1Dmsg      = $Msg(07);
095600090805             leavesr;
095700090805           ENDIF;
095800090805         ENDIF;
095900090810
096000090810         //?Controllo che almeno 1 orario sia richiesto
096100090810         IF  V1Hour1 = *blanks and V1Hour2 = *blanks and
096200090810            V1Minute = *blanks;
096300090810             errMessage  = *on;
096400090810             errGenerico = *on;
096500090810             PosCurORA = *on;
096600090810             V1Dmsg      = $Msg(09);
096700090810             leavesr;
096800090810         ENDIF;
096900090708
097000090708         //?Imposto i campi da passare alla seconda videata
097100091111         IMK82Data = wdata;
097200091111         IMK82cAGE = %dec(V1cAGE:7:0);
097300090805
097400090805         //?Carico la sk attività da escludere
097500090805         $Noatt(01) = V1Att01;
097600090805         $Noatt(02) = V1Att02;
097700090805         $Noatt(03) = V1Att03;
097800090805         $Noatt(04) = V1Att04;
097900090805         $Noatt(05) = V1Att05;
098000090805         $Noatt(06) = V1Att06;
098100090805         $Noatt(07) = V1Att07;
098200090805         $Noatt(08) = V1Att08;
098300090805         $Noatt(09) = V1Att09;
098400090805         $Noatt(10) = V1Att10;
098500090805
098600090805         //?Carico la sk agenti
098700090805         exsr sr_CarAGE;
098800090807
098900090807         //?Imposto la visualizzazione
099000090807         $Hour1 = (V1Hour1 <> *blanks);
099100090807         $Hour2 = (V1Hour2 <> *blanks);
099200090807         $Minute30 = (V1Minute <> *blanks);
099300090708
099400090708       ENDSR;
099500090708
099600090708       //--------------------------------------------------------------
099700090805       //?Gestione videata S02.
099800090708       //--------------------------------------------------------------
099900090722       BEGSR sr_GesS02;
100000090708
100100090708         //?Inizializzazione videata
100200090722         IF  $InzS02 = *on;
100300090722           exsr sr_InzS02;
100400090722           $InzS02 = *off;
100500090722         ENDIF;
100600090708
100700090708         //?Emissione videata
100800090722         sfldsp_n = *off;
100900090722         V2hRcd = 1;
101000091217         F11Attivo = $Week;
101100100416         //?Posiziono il cursore sempre sul commerciale
101200100416         PosCurAGE = *on;
101300090708         write MK82T01;
101400090709         write MK82P02;
101500090722         exfmt MK82C02;
101600090708         ErrMessage  = *off;
101700090708         ErrGenerico = *off;
101800090708         clear V2Dmsg;
101900100204
102000100308         //?- Controllo subito il commerciale perchè se errato non devo fare ved
102100100308         IF  V2cAGE <> *zeros and V2cAGE <> *blanks and
102200100308             dsp_aid <> c_F12 and dsp_aid <> c_F01;
102300100308           exsr sr_ctrAGE;
102400100204           IF  errGenerico;
102500100204             leavesr;
102600100204           ENDIF;
102700100204         ENDIF;
102800090708
102900090708         SELECT;
103000090724
103100090724           //?F1=Help Legenda colori
103200090724           WHEN  dsp_aid = c_F01;
103300090724             exsr sr_F01S02;
103400090722
103500090723           //?F4=Settimana Precedente
103600090723           WHEN  dsp_aid = c_F04;
103700090723             exsr sr_F04S02;
103800090722
103900090723           //?F5=Settimana Successiva
104000090723           WHEN  dsp_aid = c_F05;
104100090723             exsr sr_F05S02;
104200100308
104300100308           //?F8=Stampa
104400100308           WHEN  dsp_aid = c_F08;
104500100308             exsr sr_F08S02;
104600090724
104700090724           //?F9=Dettaglio
104800090724           WHEN  dsp_aid = c_F09;
104900090724             exsr sr_F09S02;
105000091217
105100091217           //?F10=Attività
105200091217           WHEN  dsp_aid = c_F10;
105300091217             exsr sr_F10S02;
105400090806
105500091217           //?F11=Week-End
105600091217           WHEN  dsp_aid = c_F11;
105700091217             exsr sr_F11S02;
105800091111
105900091111           //?F12=Ritorno
106000091111           WHEN  dsp_aid = c_F12;
106100091111             exsr sr_F12S02;
106200090722
106300090722           //?RollUp - PAGEDOWN
106400090722           WHEN  dsp_aid = c_RollUp;
106500090722             exsr sr_RollUpS02;
106600090722
106700090722           //?RollDown - PAGEUP
106800090722           WHEN  dsp_aid = c_RollDown;
106900090722             exsr sr_RollDownS02;
107000090708
107100090708           //?Enter
107200100308           OTHER;
107300100308           //?- Controllo dati
107400100308             exsr sr_CtrS02;
107500100308             IF  errGenerico;
107600100308               leavesr;
107700100308             ENDIF;
107800090708
107900090708         ENDSL;
108000090708
108100090708       ENDSR;
108200090708
108300090708       //--------------------------------------------------------------
108400090805       //?Inizializzazione videata S02.
108500090708       //--------------------------------------------------------------
108600090722       BEGSR sr_InzS02;
108700091027
108800091027         clear sav_atcdad;
108900090708
109000090722         //?Pulizia subfile
109100090722         exsr Sr_PulSfl;
109200090708
109300090708         //?imposto i dati ricevuti
109400091230          IF  V2Data = 0;
109500091230            wDate_ISO = %date(IMK82Data:*iso);
109600091230            wDate_EUR = wDate_ISO;
109700091230            V2Data = %dec(wDate_EUR);
109800091230          ENDIF;
109900091230          IF  V2cAGE = *blanks;
110000091230            V2cAGE = %editc(IMK82cAGE:'X');
110100091230          ENDIF;
110200090907
110300091027         //?Se commerciale impostato
110400091105          IF  V2cAGE <> *blanks and V2cAGE <> *zeros;
110500130828            chain  (%int(V2Cage))  AZCMM000;
110600130828            IF  not %found( AZCMM01L );
110700091027              errMessage  = *on;
110800091027              errGenerico = *on;
110900091027              PosCurAGE   = *on;
111000091027              V2Dmsg      = $Msg(04);
111100100203              clear $AGE;
111200100331            ELSE;
111300130828              V2dAGE = CMMdes;
111400100331            ENDIF;
111500100331           //?se programma richiamato e il commerciale impostato non è un
111600100331           //?commerciale unificante mi carico il suo unificante
111700130828            IF  $Richiamato and CMMuni <> %dec(V2cAGE:7:0) and not ErrGenerico;
111800130828              V2cAge = %editc(CMMuni:'X');
111900100331              IMK82cAGE = %dec(V2cAGE:7:0);
112000100331              exsr sr_CarAGE;
112100100331            ENDIF;
112200100203         //?Controllo se utente abilitato all'agente
112300100331           IF  not ErrGenerico;
112400100331             clear TNTAA1DS;
112500100331             ITAA1caut = '§utegtc';
112600100331             ITAA1cmm  = %dec(V2cAGE:7:0);
112700100331             kpjbu     = tntaa1ds;
112800100331             tntaa1c (kpjba);
112900100331             TNTAA1DS = kpjbu;
113000100331             IF  OTAA1fabi = 'N';
113100100331                ErrMessage  = *on;
113200100331                ErrGenerico = *on;
113300100331                PosCurAGE   = *on;
113400100331                V2Dmsg      = $Msg(04);
113500100203           //?pulisco la sk $age così non viene caricato niente nel subfile
113600100331                clear $AGE;
113700100331              ENDIF;
113800100331            ENDIF;
113900100203         //?Se commerciale non impostato passo l'errore e non vado a fine
114000091027         //?della routine altrimenti non carica subfile vuoto
114100091027          ELSE;
114200091027            errMessage  = *on;
114300091027            errGenerico = *on;
114400091027            PosCurAGE   = *on;
114500091027            V2Dmsg      = $Msg(04);
114600091027          ENDIF;
114700090708
114800090709         //?Carico la settimana partendo dalla data richiesta
114900090805         exsr sr_CarDay;
115000090721
115100090721         //?Carico le ore
115200090805         exsr sr_CarHour;
115300090721
115400090722         //?Carico i dati nel control
115500090805         exsr sr_CarCTRL;
115600090708
115700090806         //?Carico le attività relative a 7 gg
115800090805         exsr sr_CarAttivita;
115900090910
116000091217         //?Se non ho caricato niente spengo indicatore F11=Week
116100090910         IF $Det = *blanks;
116200090910           $Week = *off;
116300090910         ENDIF;
116400090709
116500090722         //?Carico la prima pagina del subfile
116600090807         //?  parto sempre dalle h. 08:00
116700090807         SELECT;
116800090807           WHEN $Hour1;
116900090807             xx = wxxh1;
117000090807             sav_xx = wxxh1;
117100090807           WHEN $Hour2;
117200090807             xx = wxxh2;
117300090807             sav_xx = wxxh2;
117400090807           WHEN $Minute30;
117500090807             xx = wxxm;
117600090807             sav_xx = wxxm;
117700090807         ENDSL;
117800090807         exsr sr_CarS02;
117900090708
118000090708       ENDSR;
118100090708
118200090708       //--------------------------------------------------------------
118300090805       //?Carico le sk dei gg.
118400090708       //--------------------------------------------------------------
118500090805       BEGSR sr_CarDay;
118600090708
118700090709         clear $Name_Day;
118800090709         clear $Name_Month;
118900090709         clear $Number_Day;
119000090708         clear $Date;
119100091222         clear $Holidays;
119200090708
119300090708         //?devo sempre partire dal Lunedì
119400091111         wDate_ISO_Par= %date(IMK82Data);
119500090709         clear wDay_Of_Week;
119600090709         exec sql
119700090709           set :wDay_Of_Week = dayofweek_iso(:wDate_ISO_Par);
119800090709
119900090709         //?carico le sk dei gg da visualizzare
120000090709           xx = 1;
120100090805           FOR xx by 1 to %elem($Date);
120200090709             SELECT;
120300090709
120400090709               WHEN xx = wDay_Of_Week;
120500090709                 wDate_ISO = wDate_ISO_Par;
120600090709                 exsr Set_Name;
120700090709                 $Date(xx)       = %dec(wDate_ISO);
120800090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
120900090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
121000090709                 $Name_Month(xx) = wName_Of_Month;
121100091222                 exsr Calendario;
121200090709
121300090709               WHEN xx < wDay_Of_Week;
121400090709                 wDate_ISO = wDate_ISO_Par - %days(wDay_Of_Week - xx);
121500090709                 exsr Set_Name;
121600090709                 $Date(xx)       = %dec(wDate_ISO);
121700090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
121800090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
121900090709                 $Name_Month(xx) = wName_Of_Month;
122000091222                 exsr Calendario;
122100090709
122200090709               WHEN xx > wDay_Of_Week;
122300090709                 wDate_ISO = wDate_ISO_Par + %days(xx - wDay_Of_Week);
122400090709                 exsr Set_Name;
122500090709                 $Date(xx)       = %dec(wDate_ISO);
122600090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
122700090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
122800090709                 $Name_Month(xx) = wName_Of_Month;
122900091222                 exsr Calendario;
123000090709             ENDSL;
123100090709
123200090709           ENDFOR;
123300090708
123400090708       ENDSR;
123500090709
123600090709       //--------------------------------------------------------------
123700090805       //?Carico la sk delle ore.
123800090709       //--------------------------------------------------------------
123900090805       BEGSR sr_CarHour;
124000090709
124100090722         reset wHour_ISO;
124200090709         xx = 1;
124300090807         clear yy;
124400090805         FOR xx by 1 to %elem($Hour);
124500090709
124600090807           IF ($Hour1 and yy = 24) or
124700090807              ($Hour2 and yy = 12);
124800090807              leave;
124900090807           ENDIF;
125000090807
125100090807           yy += 1;
125200090807
125300090807           IF xx = 1;
125400090807             $Hour(xx) = %dec(wHour_ISO);
125500090807           ELSE;
125600090807             SELECT;
125700090807               WHEN  $Minute30;
125800090807                 wHour_ISO = wHour_ISO + %minutes(30);
125900090807               WHEN  $Hour1;
126000090807                 wHour_ISO = wHour_ISO + %hours(1);
126100090807               WHEN  $Hour2;
126200090807                 wHour_ISO = wHour_ISO + %hours(2);
126300090807             ENDSL;
126400090807             $Hour(xx) = %dec(wHour_ISO);
126500090807           ENDIF;
126600090709
126700090709         ENDFOR;
126800090709
126900090709       ENDSR;
127000090722
127100090722       //--------------------------------------------------------------
127200090805       //?Carico i dati nel CONTROL.
127300090722       //--------------------------------------------------------------
127400090805       BEGSR sr_CarCTRL;
127500090722
127600090722         //?Carico riga di testata per il subfile
127700091222         v2criga = '___________|___';
127800091222         IF  $Holidays(01) = '1';
127900091222           V2cRiga = %trim(V2cRiga) + Pink +
128000091222                   $Name_Day(01)    + ' ' +
128100091222                   %editc($Number_Day(01):'X') + ' ' +
128200091222                   $Name_Month(01) + Green +
128300091222                   '__|___';
128400091222         ELSE;
128500091222           V2cRiga = %trim(V2cRiga) + ' ' +
128600091222                   $Name_Day(01)    + ' ' +
128700091222                   %editc($Number_Day(01):'X') + ' ' +
128800091222                   $Name_Month(01) +
128900091222                   ' __|___';
129000091222         ENDIF;
129100091222         IF  $Holidays(02) = '1';
129200091222           V2cRiga = %trim(V2cRiga) + Pink +
129300091222                   $Name_Day(02)   + ' ' +
129400091222                   %editc($Number_Day(02):'X') + ' ' +
129500091222                   $Name_Month(02) + Green +
129600091222                   '__|___';
129700091222         ELSE;
129800091222           V2cRiga = %trim(V2cRiga) + ' ' +
129900090722                   $Name_Day(02)   + ' ' +
130000090722                   %editc($Number_Day(02):'X') + ' ' +
130100091222                   $Name_Month(02) +
130200091222                   ' __|___';
130300091222         ENDIF;
130400091222         IF  $Holidays(03) = '1';
130500091222           V2cRiga = %trim(V2cRiga) + Pink +
130600091222                   $Name_Day(03)   + ' ' +
130700091222                   %editc($Number_Day(03):'X') + ' ' +
130800091222                   $Name_Month(03) + Green +
130900091222                   '__|___';
131000091222         ELSE;
131100091222         V2cRiga = %trim(V2cRiga) + ' ' +
131200091222                   $Name_Day(03)   + ' ' +
131300091222                   %editc($Number_Day(03):'X') + ' ' +
131400091222                   $Name_Month(03) +
131500091222                   ' __|___';
131600091222         ENDIF;
131700091222         IF  $Holidays(04) = '1';
131800091222           V2cRiga = %trim(V2cRiga) + Pink +
131900091222                   $Name_Day(04)   + ' ' +
132000091222                   %editc($Number_Day(04):'X') + ' ' +
132100091222                   $Name_Month(04) + Green +
132200091222                   '__|___';
132300091222         ELSE;
132400091222           V2cRiga = %trim(V2cRiga) + ' ' +
132500090722                   $Name_Day(04)   + ' ' +
132600090722                   %editc($Number_Day(04):'X') + ' ' +
132700091222                   $Name_Month(04) +
132800091222                   ' __|___';
132900091222         ENDIF;
133000091222         IF  $Holidays(05) = '1';
133100091222           V2cRiga = %trim(V2cRiga) + Pink +
133200091222                   $Name_Day(05)   + ' ' +
133300091222                   %editc($Number_Day(05):'X') + ' ' +
133400091222                   $Name_Month(05) + Green +
133500091222                   '__' ;
133600091222         ELSE;
133700091222           V2cRiga = %trim(V2cRiga) + ' ' +
133800090722                   $Name_Day(05)   + ' ' +
133900090722                   %editc($Number_Day(05):'X') + ' ' +
134000091222                   $Name_Month(05) +
134100091222                   ' __' ;
134200091222         ENDIF;
134300090806
134400090806         //?Imposto i giorni Hidden
134500090806         V2hLun = $Date(01);
134600090806         V2hMar = $Date(02);
134700090806         V2hMer = $Date(03);
134800090806         V2hGio = $Date(04);
134900090806         V2hVen = $Date(05);
135000090806         V2hSab = $Date(06);
135100090806         V2hDom = $Date(07);
135200090722
135300090722       ENDSR;
135400090722
135500090722       //--------------------------------------------------------------
135600090806       //?Carico le attività di 7 gg.
135700090722       //--------------------------------------------------------------
135800090805       BEGSR sr_CarAttivita;
135900090805
136000090805         //?Pulisco le skiere
136100090805         clear $DetLunA;
136200090805         clear $DetLunB;
136300090805         clear $DetMarA;
136400090805         clear $DetMarB;
136500090805         clear $DetMerA;
136600090805         clear $DetMerB;
136700090805         clear $DetGioA;
136800090805         clear $DetGioB;
136900090805         clear $DetVenA;
137000090805         clear $DetVenB;
137100090805
137200090805         clear $DetLunAt;
137300090805         clear $DetLunBt;
137400090805         clear $DetMarAt;
137500090805         clear $DetMarBt;
137600090805         clear $DetMerAt;
137700090805         clear $DetMerBt;
137800090805         clear $DetGioAt;
137900090805         clear $DetGioBt;
138000090805         clear $DetVenAt;
138100090805         clear $DetVenBt;
138200090805
138300090805         $End = *off;
138400090805
138500090805         //?Imposto la stringa per SQL
138600090805         exsr  sr_PrepSQL;
138700090805
138800090805         //?Dichiarazione cursore
138900090805         exec sql
139000090805           prepare S1   from :wSQL;
139100090805         exec sql
139200090805           declare ATT  cursor for S1;
139300090805
139400090805         //?Apertura del cursore
139500090805         exec sql
139600090805           open ATT;
139700090722
139800090805         IF sqlcode < 0;
139900090805           $End = *on;
140000090805         ENDIF;
140100090722
140200090805         DOW not $End;
140300090722           exec sql
140400090805             fetch next from ATT into :atctat, :atccpo, :atcksc,
140500100305                                      :atcdad, :atcoii, :atcofi,
140600100525                                      :atchda, :atcnrv, :atccad,
140700100525                                      :atccac, :atcest;
140800090805           IF sqlcod = 100 or sqlcod < 0;
140900090805             $End = *on;
141000090805             leave;
141100090805           ENDIF;
141200090805
141300090805           IF atcdad <> sav_atcdad;
141400090805             clear wDetA;
141500090805             clear wDetAt;
141600090805             clear wDetB;
141700090805             clear wDetBt;
141800090805             sav_atcdad = atcdad;
141900090805           ENDIF;
142000100305
142100100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
142200100305           exsr sr_CtrRcdOk;
142300100305           IF  not $RcdOk;
142400100305             iter;
142500100305           ENDIF;
142600090805
142700090805         //?Carico i dati a dettaglio
142800090805           exsr sr_CarDati;
142900090805
143000090722         ENDDO;
143100090722
143200090805         exec sql
143300090805           close ATT;
143400090722
143500090722       ENDSR;
143600090805
143700090805       //--------------------------------------------------------------
143800090805       //?Imposto i nomi del giorno e del mese.
143900090805       //--------------------------------------------------------------
144000090805       BEGSR Set_Name;
144100090805
144200090805         //?Recupero il nome del giorno
144300090805         clear wName_Of_Day;
144400090805         exec sql
144500090805           set :wName_Of_Day = dayname(:wDate_ISO);
144600090805
144700090805         //?Recupero il nome del mese
144800090805         clear wName_Of_Month;
144900090805         exec sql
145000090805           set :wName_Of_Month = monthname(:wDate_ISO);
145100090805
145200090805       ENDSR;
145300091222
145400091222       //--------------------------------------------------------------
145500091222       //?Verifico se giorno di festa.
145600091222       //--------------------------------------------------------------
145700091222       BEGSR Calendario;
145800091222
145900091222         $Holidays(xx) = '0';
146000091222
146100091229         clear k_clntfa;
146200091229         k_clntfp = %dec(%subst(V2cage:1:3):3:0);
146300091222         k_clnann = %subdt(wDate_ISO:*years);
146400091222         k_clnmes = %subdt(wDate_ISO:*months);
146500091222         chain %kds(K04azcln) AZCLN01L;
146600091222         IF  %found(AZCLN01L);
146700091222           IF  mat($Number_Day(xx)) = 'F'  or
146800091222               pom($Number_Day(xx)) = 'F';
146900091222             $Holidays(xx) = '1';
147000091222           ENDIF;
147100091222         ENDIF;
147200091222
147300091222       ENDSR;
147400090805
147500090805       //--------------------------------------------------------------
147600090805       //?Preparazione stringa SQL.
147700090805       //--------------------------------------------------------------
147800090805       BEGSR sr_PrepSQL;
147900090805
148000090805         wSQL = 'select atctat, atccpo, atcksc, atcdad, atcoii, atcofi, +
148100100525                 atchda, atcnrv, atccad, atccac, atcest +
148200100525                  from TIATC00F +
148300090805                  where atccmm in(';
148400090805
148500090806         yy = 0;
148600090805         xx = 1;
148700090805         FOR xx by 1 to %elem($Age);
148800090805           IF $Age(xx) <> *zeros;
148900090806             IF yy > 0;
149000090806               wSQL += ', ';
149100090806             ELSE;
149200090806               yy = 1;
149300090806             ENDIF;
149400090806             wSQL += %editc($Age(xx):'X');
149500090805           ENDIF;
149600090805         ENDFOR;
149700090805
149800090805         wSQL += ') and atcdad between ' + %editc($Date(01):'X') +
149900090805                 ' and ' + %editc($Date(07):'X');
150000090805
150100091111         IF IMK82NoAtt <> *blanks;
150200090806         wSQL += ' and atctat not in(''';
150300090806           yy = 0;
150400090805           xx = 1;
150500090805           FOR xx by 1 to %elem($NoAtt);
150600090806             IF $NoAtt(xx) <> *blanks;
150700090806               IF yy > 0;
150800090806                 wSQL += ', ';
150900090806               ELSE;
151000090806                 yy = 1;
151100090806               ENDIF;
151200090806               wSQL +=  $NoAtt(xx) + '''';
151300090805             ENDIF;
151400090805           ENDFOR;
151500090805         wSQL += ')';
151600090805         ENDIF;
151700090805
151800090806         wSQL += ' order by atcdad, atctat +
151900090805                   for fetch only';
152000090805
152100090805       ENDSR;
152200100305
152300100305       //--------------------------------------------------------------
152400100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
152500100305       //--------------------------------------------------------------
152600100305       BEGSR Sr_CtrRcdOk;
152700100305
152800100305         $RcdOk = *on;
152900100416
153000100305         //?se attività legata a trattativa
153100100305         //?la trattativa NON deve essere fittizia
153200100305         IF  atcnrv > 0;
153300110624             exec sql
153400110803             select visffz into :visffz from tivis05l
153500110624             where visnrv = : atcnrv;
153600100305         //?non trovo il rcd corrispondente
153700100305           IF  sqlcod <> 0;
153800100305             leavesr;
153900100305           ENDIF;
154000100305         //?se trattativa fittizia non carico a video
154100100305           IF  visffz = 'S';
154200100305             $RcdOk = *off;
154300100305           ENDIF;
154400100305         ENDIF;
154500100416
154600100416         //?CHIODO
154700100705         //?MODIFICA CONTROLLO 05 Luglio 2010 ora controllo solo esito senza cau
154800100705         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
154900100705         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
155000100705           $RcdOk = *off;
155100100705         ENDIF;
155200100416         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
155300100416         //?non devo caricare in agenda
155400100705         // IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
155500100705         //   $RcdOk = *off;
155600100705         // ENDIF;
155700100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
155800100525         //?non devo caricare in agenda
155900100705         // IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
156000100705         //   $RcdOk = *off;
156100100705         // ENDIF;
156200100705         //?CHIODO
156300100521         //?Se attvità qualsiasi chiusa con attività 'AA'
156400100521         //?non devo caricare in agenda
156500100521         IF  ATCcac = 'AA';
156600100521           $RcdOk = *off;
156700100521         ENDIF;
156800100305
156900100305       ENDSR;
157000090709
157100090709       //--------------------------------------------------------------
157200090807       //?Carico i dati a video per i gg della settimana.
157300090709       //--------------------------------------------------------------
157400090805       BEGSR Sr_CarDati;
157500090806
157600090807         $Week = *off;
157700090709
157800090805         //?descrizione
157900090709         clear wDesCpo;
158000090804         SELECT;
158100090805         //?del cliente
158200090804           WHEN  atcksc <> *zeros;
158300090804             k_acoksc = atcksc;
158400090804             chain  %kds(k03cnaco) CNACO00F;
158500090804             wDesCpo = acorag;
158600090805         //?del potenziale
158700090804           WHEN  atccpo <> 0;
158800090804             chain  atccpo tncpo01l;
158900090804             wDesCpo = cporag;
159000090805           ENDSL;
159100090805         //?imposto fisso ferie
159200090805         IF atctat = 'F';
159300090722           wDesCpo = 'Ferie';
159400090805         ENDIF;
159500091123         //?imposto fisso ufficio
159600091123         IF atctat = 'U';
159700091123           wDesCpo = 'Ufficio';
159800091123         ENDIF;
159900090709
160000090805         //?ora inizio
160100090807         IF  atcoii = 0;
160200090807           watcoii = atchda;
160300090807         ELSE;
160400090807           watcoii = atcoii * 100;
160500090807         ENDIF;
160600090721         wHour_ISO_I = %time(watcoii);
160700100504         //?tengo buone solo le ore
160800100504         IF  not $Minute30;
160900100504           wHour = %subdt(wHour_ISO_I:*hours);
161000100504           watcoii = wHour * 10000;
161100100504         ENDIF;
161200090721
161300090805         //?ora fine
161400090723         IF  atcofi = 0;
161500090723           watcofi = watcoii;
161600090723         ELSE;
161700090723           watcofi = atcofi * 100;
161800090723         ENDIF;
161900090721         wHour_ISO_F = %time(watcofi);
162000100504         //?controllo se ho dei minuti
162100100504         IF  not $Minute30;
162200100504           wMin = %subdt(wHour_ISO_F:*minutes);
162300100504         ENDIF;
162400090709
162500090805         //?calcolo quante ore ho impegnate
162600090807         IF  not $Minute30;
162700100504           IF not $Hour2;
162800100504             wTot_M = %diff(wHour_ISO_F : wHour_ISO_I : *minutes);
162900100504             wTot_H = %div(wTot_M : 60);
163000100504             wTot_R = %rem(wTot_M : 60);
163100100504           //?ho del resto dal calcolo delle ore aggiungo 1 alle ore
163200100504           //?ho dei minuti nell'ora di fine impegno aggiungo 1 alle ore
163300100504             IF  wTot_R > 0 or wMin > 0;
163400100504               wTot_H += 1;
163500100504             ENDIF;
163600100504           ENDIF;
163700100504           IF $Hour2;
163800100504             wTot_H = %diff(wHour_ISO_F : wHour_ISO_I : *hours);
163900090807             wTot_H = wTot_H / 2;
164000090807           ENDIF;
164100090807         ENDIF;
164200090807         IF  $Minute30;
164300090807           wTot_H = %diff(wHour_ISO_F : wHour_ISO_I : *minutes);
164400090807           wTot_H = wTot_H / 30;
164500090807         ENDIF;
164600090805
164700090805         //?carico il dettaglio per giorno
164800090805         xx = %lookup(watcoii : $Hour);
164900090805         //?carico la prima ora di impegno
165000090805         IF xx > 0;
165100090805           zz = xx;
165200090805           exsr sr_CarDetGio;
165300090805         //?imposto le successive ore di impegno
165400090805           IF wTot_H > 1;
165500100504             wTot_H -= 1;
165600090805             yy = 1;
165700090805               FOR yy by 1 to wTot_H;
165800090805                 zz = yy+xx;
165900090805                 exsr sr_CarDetGio;
166000090805               ENDFOR;
166100090805           ENDIF;
166200090805         ENDIF;
166300090807         //?Se visualizzo ogni 2 ore e non trovo l'ora corrispondende nella sk
166400090807         //?provo sommando 1 ora all'ora inizio impegno
166500090807         IF xx = 0;
166600090807           wHour_ISO = %time(watcoii);
166700090807           wHour_ISO = wHour_ISO + %hours(1);
166800090807           watcoii = %dec(wHour_ISO);
166900090807           xx = %lookup(watcoii : $Hour);
167000090807         //?carico la prima ora di impegno
167100090807           IF xx > 0;
167200090807             zz = xx;
167300090807             exsr sr_CarDetGio;
167400090807         //?imposto le successive ore di impegno
167500090807             IF wTot_H > 1;
167600090807               yy = 1;
167700090807                 FOR yy by 1 to wTot_H;
167800090807                   zz = yy+xx;
167900090807                   exsr sr_CarDetGio;
168000090807                 ENDFOR;
168100090807             ENDIF;
168200090807           ENDIF;
168300090807         ENDIF;
168400090807         //?Imposto se c'è impegno nel week-end
168500090807         IF  atcdad = V2hSab or
168600090807            atcdad = V2hDom;
168700090807            $Week = *on;
168800090807         ENDIF;
168900090709
169000090709       ENDSR;
169100090805
169200090805       //--------------------------------------------------------------
169300090805       //?Carico il dettaglio per giorno.
169400090805       //--------------------------------------------------------------
169500090805       BEGSR Sr_CarDetGio;
169600090805
169700090805         SELECT;
169800090805           WHEN zz > 1 and
169900090805              wDetA(zz) = *blanks and
170000100416              %scan(%subst(wDetB(zz-1):3:7):%subst(wDesCpo:1:7)) > 0;
170100090805              wDetB(zz)  = '* ' + wDesCpo;
170200090805              wDetBt(zz) = atctat;
170300090805           WHEN wDetA(zz)  = *blanks;
170400090805              wDetA(zz)  = wDesCpo;
170500090805              wDetAt(zz) = atctat;
170600090805           OTHER;
170700090805           IF wDetB(zz) = *blanks;
170800090805              wDetB(zz)  = '* ' + wDesCpo;
170900090805              wDetBt(zz) = atctat;
171000090805           ELSE;
171100090805              wDetB(zz)  = c_AltreA;
171200091125              wDetBt(zz) = c_AttAltre;
171300090805           ENDIF;
171400090805         ENDSL;
171500090806
171600090805         SELECT;
171700090805         //?Lunedì
171800090806           WHEN  atcdad = V2hLun;
171900090805             $DetLunA(zz)  = wDetA(zz);
172000090805             $DetLunB(zz)  = wDetB(zz);
172100090805             $DetLunAt(zz) = wDetAt(zz);
172200090805             $DetLunBt(zz) = wDetBt(zz);
172300090805         //?Martedì
172400090806           WHEN  atcdad = V2hMar;
172500090805             $DetMarA(zz)  = wDetA(zz);
172600090805             $DetMarB(zz)  = wDetB(zz);
172700090805             $DetMarAt(zz) = wDetAt(zz);
172800090805             $DetMarBt(zz) = wDetBt(zz);
172900090805         //?Mercoledì
173000090806           WHEN  atcdad = V2hMer;
173100090805             $DetMerA(zz)  = wDetA(zz);
173200090805             $DetMerB(zz)  = wDetB(zz);
173300090805             $DetMerAt(zz) = wDetAt(zz);
173400090805             $DetMerBt(zz) = wDetBt(zz);
173500090805         //?Giovedì
173600090806           WHEN  atcdad = V2hGio;
173700090805             $DetGioA(zz)  = wDetA(zz);
173800090805             $DetGioB(zz)  = wDetB(zz);
173900090805             $DetGioAt(zz) = wDetAt(zz);
174000090805             $DetGioBt(zz) = wDetBt(zz);
174100090805         //?Venerdì
174200090806           WHEN  atcdad = V2hVen;
174300090805             $DetVenA(zz)  = wDetA(zz);
174400090805             $DetVenB(zz)  = wDetB(zz);
174500090805             $DetVenAt(zz) = wDetAt(zz);
174600090805             $DetVenBt(zz) = wDetBt(zz);
174700090805         ENDSL;
174800090805
174900090805       ENDSR;
175000090722
175100090722       //--------------------------------------------------------------
175200090805       //?Pulisco subfile.
175300090722       //--------------------------------------------------------------
175400090722       BEGSR sr_PulSfl;
175500090722
175600090722         SflDsp_N    = *on;
175700090722         SflDspCtl_N = *on;
175800090722         write  MK82C02;
175900090722         SflDspCtl_N = *off;
176000090722         SflEnd      = *off;
176100090722
176200090722         clear V2hRcd;
176300090722         clear V2Dmsg;
176400090722         errMessage  = *off;
176500090722         errGenerico = *off;
176600090722
176700090722       ENDSR;
176800090722
176900090722       //--------------------------------------------------------------
177000090807       //?Carico il subfile.
177100090722       //--------------------------------------------------------------
177200090807       BEGSR sr_CarS02;
177300090722
177400090722         clear S02Nrr;
177500090807         clear yy;
177600090807
177700090807         FOR xx by 1 to %elem($Hour);
177800090807
177900090807           IF yy = 10;
178000090807             leave;
178100090807           ENDIF;
178200090807
178300090807           yy += 1;
178400090723
178500090723           V2Ora = %dec(%subst(%editc($Hour(xx):'X'):1:4):4:0);
178600090723           V2LunA  = $DetLunA(xx);
178700090723           V2LunB  = $DetLunB(xx);
178800090723           V2hLunA = $DetLunAt(xx);
178900090723           V2hLunB = $DetLunBt(xx);
179000090723           V2MarA  = $DetMarA(xx);
179100090723           V2MarB  = $DetMarB(xx);
179200090723           V2hMarA = $DetMarAt(xx);
179300090723           V2hMarB = $DetMarBt(xx);
179400090723           V2MerA  = $DetMerA(xx);
179500090723           V2MerB  = $DetMerB(xx);
179600090723           V2hMerA = $DetMerAt(xx);
179700090723           V2hMerB = $DetMerBt(xx);
179800090723           V2GioA  = $DetGioA(xx);
179900090723           V2GioB  = $DetGioB(xx);
180000090723           V2hGioA = $DetGioAt(xx);
180100090723           V2hGioB = $DetGioBt(xx);
180200090723           V2VenA  = $DetVenA(xx);
180300090723           V2VenB  = $DetVenB(xx);
180400090723           V2hVenA = $DetVenAt(xx);
180500090723           V2hVenB = $DetVenBt(xx);
180600090723
180700090723           v2sriga = '___________|' +
180800090723                     '_______________________|' +
180900090723                     '_______________________|' +
181000090723                     '_______________________|' +
181100090723                     '_______________________|' +
181200090723                     '________________________';
181300090723
181400091123         //?Imposto i colori delle attività
181500091123         //?LUNEDI
181600091123           SELECT;
181700091123             WHEN V2hLunA = 'T';
181800091123               ColorLunA = Blue;
181900091123             WHEN V2hLunA = 'A';
182000091123               ColorLunA = Red;
182100091123             WHEN V2hLunA = 'F';
182200091123               ColorLunA = Pink;
182300091123             WHEN V2hLunA = 'O';
182400091123               ColorLunA = White;
182500091123             WHEN V2hLunA = 'U';
182600091125               ColorLunA = Green;
182700091125             WHEN V2hLunA = 'W';
182800091125               ColorLunA = Turquoise;
182900091123             OTHER;
183000091125               ColorLunA = Green;
183100091123           ENDSL;
183200091123           SELECT;
183300091123             WHEN V2hLunB = 'T';
183400091123               ColorLunB = Blue;
183500091123             WHEN V2hLunB = 'A';
183600091123               ColorLunB = Red;
183700091123             WHEN V2hLunB = 'F';
183800091123               ColorLunB = Pink;
183900091123             WHEN V2hLunB = 'O';
184000091123               ColorLunB = White;
184100091123             WHEN V2hLunB = 'U';
184200091125               ColorLunB = Green;
184300091125             WHEN V2hLunB = 'W';
184400091125               ColorLunB = Turquoise;
184500091123             OTHER;
184600091125               ColorLunB = Green;
184700091123           ENDSL;
184800091123         //?MARTEDI
184900091123           SELECT;
185000091123             WHEN V2hMarA = 'T';
185100091123               ColorMarA = Blue;
185200091123             WHEN V2hMarA = 'A';
185300091123               ColorMarA = Red;
185400091123             WHEN V2hMarA = 'F';
185500091123               ColorMarA = Pink;
185600091123             WHEN V2hMarA = 'O';
185700091123               ColorMarA = White;
185800091123             WHEN V2hMarA = 'U';
185900091125               ColorMarA = Green;
186000091125             WHEN V2hMarA = 'W';
186100091125               ColorMarA = Turquoise;
186200091123             OTHER;
186300091125               ColorMarA = Green;
186400091123           ENDSL;
186500091123           SELECT;
186600091123             WHEN V2hMarB = 'T';
186700091123               ColorMarB = Blue;
186800091123             WHEN V2hMarB = 'A';
186900091123               ColorMarB = Red;
187000091123             WHEN V2hMarB = 'F';
187100091123               ColorMarB = Pink;
187200091123             WHEN V2hMarB = 'O';
187300091123               ColorMarB = White;
187400091123             WHEN V2hMarB = 'U';
187500091125               ColorMarB = Green;
187600091125             WHEN V2hMarB = 'W';
187700091125               ColorMarB = Turquoise;
187800091123             OTHER;
187900091125               ColorMarB = Green;
188000091123           ENDSL;
188100091123         //?MERCOLEDI
188200091123           SELECT;
188300091123             WHEN V2hMerA = 'T';
188400091123               ColorMerA = Blue;
188500091123             WHEN V2hMerA = 'A';
188600091123               ColorMerA = Red;
188700091123             WHEN V2hMerA = 'F';
188800091123               ColorMerA = Pink;
188900091123             WHEN V2hMerA = 'O';
189000091123               ColorMerA = White;
189100091123             WHEN V2hMerA = 'U';
189200091125               ColorMerA = Green;
189300091125             WHEN V2hMerA = 'W';
189400091125               ColorMerA = Turquoise;
189500091123             OTHER;
189600091125               ColorMerA = Green;
189700091123           ENDSL;
189800091123           SELECT;
189900091123             WHEN V2hMerB = 'T';
190000091123               ColorMerB = Blue;
190100091123             WHEN V2hMerB = 'A';
190200091123               ColorMerB = Red;
190300091123             WHEN V2hMerB = 'F';
190400091123               ColorMerB = Pink;
190500091123             WHEN V2hMerB = 'O';
190600091123               ColorMerB = White;
190700091123             WHEN V2hMerB = 'U';
190800091125               ColorMerB = Green;
190900091125             WHEN V2hMerB = 'W';
191000091125               ColorMerB = Turquoise;
191100091123             OTHER;
191200091125               ColorMerB = Green;
191300091123           ENDSL;
191400091123         //?GIOVEDI
191500091123           SELECT;
191600091123             WHEN V2hGioA = 'T';
191700091123               ColorGioA = Blue;
191800091123             WHEN V2hGioA = 'A';
191900091123               ColorGioA = Red;
192000091123             WHEN V2hGioA = 'F';
192100091123               ColorGioA = Pink;
192200091123             WHEN V2hGioA = 'O';
192300091123               ColorGioA = White;
192400091123             WHEN V2hGioA = 'U';
192500091125               ColorGioA = Green;
192600091125             WHEN V2hGioA = 'W';
192700091125               ColorGioA = Turquoise;
192800091123             OTHER;
192900091125               ColorGioA = Green;
193000091123           ENDSL;
193100091123           SELECT;
193200091123             WHEN V2hGioB = 'T';
193300091123               ColorGioB = Blue;
193400091123             WHEN V2hGioB = 'A';
193500091123               ColorGioB = Red;
193600091123             WHEN V2hGioB = 'F';
193700091123               ColorGioB = Pink;
193800091123             WHEN V2hGioB = 'O';
193900091123               ColorGioB = White;
194000091123             WHEN V2hGioB = 'U';
194100091125               ColorGioB = Green;
194200091125             WHEN V2hGioB = 'W';
194300091125               ColorGioB = Turquoise;
194400091123             OTHER;
194500091125               ColorGioB = Green;
194600091123           ENDSL;
194700091123         //?VENERDI
194800091123           SELECT;
194900091123             WHEN V2hVenA = 'T';
195000091123               ColorVenA = Blue;
195100091123             WHEN V2hVenA = 'A';
195200091123               ColorVenA = Red;
195300091123             WHEN V2hVenA = 'F';
195400091123               ColorVenA = Pink;
195500091123             WHEN V2hVenA = 'O';
195600091123               ColorVenA = White;
195700091123             WHEN V2hVenA = 'U';
195800091125               ColorVenA = Green;
195900091125             WHEN V2hVenA = 'W';
196000091125               ColorVenA = Turquoise;
196100091123             OTHER;
196200091125               ColorVenA = Green;
196300091123           ENDSL;
196400091123           SELECT;
196500091123             WHEN V2hVenB = 'T';
196600091123               ColorVenB = Blue;
196700091123             WHEN V2hVenB = 'A';
196800091123               ColorVenB = Red;
196900091123             WHEN V2hVenB = 'F';
197000091123               ColorVenB = Pink;
197100091123             WHEN V2hVenB = 'O';
197200091123               ColorVenB = White;
197300091123             WHEN V2hVenB = 'U';
197400091125               ColorVenB = Green;
197500091125             WHEN V2hVenB = 'W';
197600091125               ColorVenB = Turquoise;
197700091123             OTHER;
197800091125               ColorVenB = Green;
197900091123           ENDSL;
198000090723
198100090722           S02Nrr += 1;
198200090722           write MK82S02;
198300090722
198400090722         ENDFOR;
198500090807
198600090807         //?Indicatore di fine file
198700090807         SELECT;
198800090807           WHEN $Hour1 and sav_xx = (wxxh1 +6);
198900090807             SflEnd = *on;
199000090807           WHEN $Hour2 and sav_xx = wxxh2;
199100090807             SflEnd = *on;
199200090807           WHEN $Minute30 and sav_xx > 38;
199300090807             SflEnd = *on;
199400090807         ENDSL;
199500090722
199600090722       ENDSR;
199700090724
199800090724       //--------------------------------------------------------------
199900090805       //?Gestione tasto funzionale F1 da videata S02.
200000090724       //?F1=Help Legenda Colori
200100090724       //--------------------------------------------------------------
200200090724       BEGSR sr_F01S02;
200300090724
200400090724        DOW  dsp_aid <> c_F12;
200500090724           exfmt MK82W02;
200600090724        ENDDO;
200700090724
200800090724       ENDSR;
200900090722
201000090722       //--------------------------------------------------------------
201100090805       //?Gestione tasto funzionale F4 da videata S02.
201200090723       //?F4=Settimana Precedente
201300090722       //--------------------------------------------------------------
201400090723       BEGSR sr_F04S02;
201500090722
201600090722         //?Imposto la data inizio settimana
201700090722         wDate_ISO= %date(V2hLun);
201800090722         wDate_ISO_Par= wDate_ISO - %days(7);
201900090722
202000090722         $video = 'S2';
202100090722         $inzS02 = *on;
202200090722         wdata = %dec(wDate_ISO_Par);
202300091111         IMK82Data = wdata;
202400091111         IMK82cAGE = %dec(V2cAGE:7:0);
202500090722
202600090722       ENDSR;
202700090722
202800090722       //--------------------------------------------------------------
202900090805       //?Gestione tasto funzionale F5 da videata S02.
203000090724       //?F5=Settimana Successiva
203100090722       //--------------------------------------------------------------
203200090723       BEGSR sr_F05S02;
203300090722
203400090722         //?Imposto la data inizio settimana
203500090722         wDate_ISO= %date(V2hLun);
203600090722         wDate_ISO_Par= wDate_ISO + %days(7);
203700090722
203800090722         $video = 'S2';
203900090722         $inzS02 = *on;
204000090722         wdata = %dec(wDate_ISO_Par);
204100091111         IMK82Data = wdata;
204200091111         IMK82cAGE = %dec(V2cAGE:7:0);
204300090722
204400090722       ENDSR;
204500100308
204600100308       //--------------------------------------------------------------
204700100308       //?Gestione tasto funzionale F8 da videata S02.
204800100308       //?F8=Stamp
204900100308       //--------------------------------------------------------------
205000100308       BEGSR sr_F08S02;
205100100308
205200100308         clear trmk86ds;
205300100308         IMK86cmm   = %dec(V2cAge:7:0);
205400100308         IMK86datai = V2hLun;
205500100308         IMK86dataf = V2hDom;
205600100308         trmk86r (kpjba:trmk86ds);
205700100308
205800100308       ENDSR;
205900090724
206000090724       //--------------------------------------------------------------
206100090805       //?Gestione tasto funzionale F9 da videata S02.
206200090724       //?F9=Dettaglio
206300090724       //--------------------------------------------------------------
206400090724       BEGSR sr_F09S02;
206500090724
206600090724         $Riga = dsp_rig;
206700090724         $Colo = dsp_col;
206800090724
206900090724         wRiga = ds_riga;
207000090724         wColo = ds_colo;
207100090806
207200090806         clear trmk83ds;
207300100120         IMK83rich = IMK82rich;
207400091105         imk83cmm = %dec(V2cAge:7:0);
207500090724
207600090724         //?in base a dove sono posizionata richiamo pgm int. attività
207700090724         SELECT;
207800090724
207900090724           WHEN  wColo >  13 and wColo <  37;
208000090806             imk83dati = V2hLun;
208100090724           WHEN  wColo >  37 and wColo <  61;
208200090806             imk83dati = V2hMar;
208300090724           WHEN  wColo >  61 and wColo <  85;
208400090806             imk83dati = V2hMer;
208500090724           WHEN  wColo >  85 and wColo <  109;
208600090806             imk83dati = V2hGio;
208700090724           WHEN  wColo > 109 and wColo <= 132;
208800090806             imk83dati = V2hVen;
208900090724           OTHER;
209000090724             ErrMessage   = *on;
209100090724             ErrGenerico  = *on;
209200090724             V2Dmsg       = $Msg(06);
209300090724             leavesr;
209400090724         ENDSL;
209500090724
209600090724         IF  wRiga < 5 or wRiga > 25;
209700090724           ErrMessage   = *on;
209800090724           ErrGenerico  = *on;
209900090724           V2Dmsg       = $Msg(06);
210000090724           leavesr;
210100090724         ENDIF;
210200090806
210300090806         imk83datf = imk83dati;
210400090806
210500091111         trmk83r (kpjba:trmk83ds);
210600100112
210700100112         //?ricarico la videata
210800100112         $InzS02 = *on;
210900090724
211000090724       ENDSR;
211100091217
211200091217       //--------------------------------------------------------------
211300091217       //?Gestione tasto funzionale F10 da videata S02.
211400091217       //?F10=Attività
211500091217       //--------------------------------------------------------------
211600091217       BEGSR sr_F10S02;
211700091217
211800091228         clear trmk17ds;
211900091229         IMK17cmm = %dec(V2cAge:7:0);
212000091228
212100091228         trmk17r (kpjba:trmk17ds);
212200091229
212300091229         //?ricarico la videata
212400091229         $InzS02 = *on;
212500091217
212600091217       ENDSR;
212700090806
212800090806       //--------------------------------------------------------------
212900091217       //?Gestione tasto funzionale F11 da videata S02.
213000091217       //?F11=Week-End
213100090806       //--------------------------------------------------------------
213200091217       BEGSR sr_F11S02;
213300090806
213400090806         clear trmk83ds;
213500090806         imk83dati = V2hSab;
213600090806         imk83datf = V2hDom;
213700091105         imk83cmm = %dec(V2cAge:7:0);
213800090806
213900091111         trmk83r (kpjba:trmk83ds);
214000090806
214100090806       ENDSR;
214200090722
214300091111       //--------------------------------------------------------------
214400091111       //?Gestione tasto funzionale F12 da videata S02.
214500091111       //?F12=Ritorno
214600091111       //--------------------------------------------------------------
214700091111       BEGSR sr_F12S02;
214800091111
214900091111         //?Se richiamato torno al chiamante
215000091111         IF  $Richiamato;
215100091111         //?Chiusura del programma
215200091111         $Fine = *on;
215300091111
215400091111         //?Se NON richiamato torno alla prima videata
215500091111         ELSE;
215600091111           $video = 'D1';
215700091111           $inzD01 = *on;
215800100120           clear V2cage;
215900100120           clear V2data;
216000091111         ENDIF;
216100091111
216200091111       ENDSR;
216300091111
216400090722       //--------------------------------------------------------------
216500090805       //?Gestione tasto funzionale RollUp videata S02.
216600090722       //--------------------------------------------------------------
216700090722       BEGSR sr_RollUpS02;
216800090722
216900090722         //?Carico le 10 ore successive
217000090722         SELECT;
217100090722
217200090807           WHEN  sav_xx = 1;
217300090807             exsr sr_PulSfl;
217400090807             IF $Hour1;
217500090807               xx = wxxh1;
217600090807             ENDIF;
217700090807             IF $Hour2;
217800090807               xx = wxxh2;
217900090807             ENDIF;
218000090807             IF $Minute30;
218100090807               xx = wxxm - 8;
218200090807             ENDIF;
218300090807             sav_xx = xx;
218400090807             exsr sr_CarS02;
218500090807           WHEN  $Hour1 and sav_xx = wxxh1;
218600090805             exsr sr_PulSfl;
218700090807             xx = wxxh1 + 6;
218800090807             sav_xx = xx;
218900090807             exsr sr_CarS02;
219000090807           WHEN  $Hour1 and sav_xx > wxxh1;
219100090807             V2Dmsg = $msg(05);
219200090807             ErrMessage  = *on;
219300090807             ErrGenerico = *on;
219400090807           WHEN  $Hour2 and sav_xx = wxxh2;
219500090722             V2Dmsg = $msg(05);
219600090722             ErrMessage  = *on;
219700090722             ErrGenerico = *on;
219800090807           WHEN  $Minute30 and sav_xx = wxxm;
219900090807             exsr sr_PulSfl;
220000090807             xx = wxxm + 8;
220100090807             sav_xx = xx;
220200090807             exsr sr_CarS02;
220300090807           WHEN  $Minute30 and sav_xx = (wxxm +8);
220400090807             exsr sr_PulSfl;
220500090807             xx = sav_xx + 8;
220600090807             sav_xx = xx;
220700090807             exsr sr_CarS02;
220800090807           WHEN  $Minute30 and sav_xx < wxxm;
220900090807             exsr sr_PulSfl;
221000090807             xx = sav_xx + 8;
221100090807             sav_xx = xx;
221200090807             exsr sr_CarS02;
221300090807           WHEN  $Minute30 and sav_xx > 38;
221400090807             V2Dmsg = $msg(05);
221500090807             ErrMessage  = *on;
221600090807             ErrGenerico = *on;
221700090807           WHEN  $Minute30 and sav_xx > (wxxm +8);
221800090807             exsr sr_PulSfl;
221900090807             xx = sav_xx + 8;
222000090807             sav_xx = xx;
222100090807             exsr sr_CarS02;
222200090722
222300090722         ENDSL;
222400090722
222500090722       ENDSR;
222600090722
222700090722       //--------------------------------------------------------------
222800090805       //?Gestione tasto funzionale RollDown videata S02.
222900090722       //--------------------------------------------------------------
223000090722       BEGSR sr_RollDownS02;
223100090722
223200090722         //?Carico le 10 ore precedenti
223300090722         SELECT;
223400090722
223500090807           WHEN  sav_xx = 1;
223600090807             V2Dmsg = $msg(05);
223700090807             ErrMessage  = *on;
223800090807             ErrGenerico = *on;
223900090807           WHEN  ($Hour1 and sav_xx = wxxh1) or
224000090807                 ($Hour2 and sav_xx = wxxh2);
224100090807             exsr sr_PulSfl;
224200090807             xx = 1;
224300090807             sav_xx = xx;
224400090807             exsr sr_CarS02;
224500090807           WHEN  $Hour1 and sav_xx > wxxh1;
224600090807             exsr sr_PulSfl;
224700090807             xx = wxxh1;
224800090807             sav_xx = xx;
224900090807             exsr sr_CarS02;
225000090807           WHEN  $Hour2 and sav_xx > wxxh2;
225100090807             exsr sr_PulSfl;
225200090807             xx = wxxh2;
225300090807             sav_xx = xx;
225400090807             exsr sr_CarS02;
225500090807           WHEN  $Minute30 and sav_xx = wxxm;
225600090807             exsr sr_PulSfl;
225700090807             xx = wxxm - 8;
225800090807             sav_xx = xx;
225900090807             exsr sr_CarS02;
226000090807           WHEN  $Minute30 and sav_xx < wxxm;
226100090807             exsr sr_PulSfl;
226200090807             xx = 1;
226300090807             sav_xx = xx;
226400090807             exsr sr_CarS02;
226500090807           WHEN  $Minute30 and sav_xx > wxxm;
226600090807             exsr sr_PulSfl;
226700090807             xx = sav_xx - 8;
226800090807             sav_xx = xx;
226900090807             exsr sr_CarS02;
227000090722
227100090722         ENDSL;
227200090722
227300090722       ENDSR;
227400090708
227500090708       //--------------------------------------------------------------
227600090805       //?Controllo videata S02.
227700090708       //--------------------------------------------------------------
227800090722       BEGSR sr_CtrS02;
227900090708
228000090708         WindDspF = IndDspF;
228100090708         reset WindDspFb;
228200090708         IndDspF  = WindDspF;
228300090708
228400090708         //?Controllo data
228500090805         IF   V2Data    = *zeros;
228600090805           ErrMessage   = *on;
228700090805           ErrGenerico  = *on;
228800090805           PosCurDATA   = *on;
228900090805           V2Dmsg       = $Msg(01);
229000090805           leavesr;
229100090805         ENDIF;
229200090708
229300090805         clear wlbdat;
229400090805         g02dat = V2Data;
229500090805         xsrda8(wlbdat);
229600090805         IF   g02err    = '1';
229700090805           ErrMessage   = *on;
229800090805           ErrGenerico  = *on;
229900090805           PosCurDATA   = *on;
230000090805           V2Dmsg       = $Msg(02);
230100090805           leavesr;
230200090805         ENDIF;
230300090708
230400090805         V2Data = g02dat;
230500090805         wdata  = g02inv;
230600090708
230700090708         //?Controllo commerciale
230800100308         exsr sr_ctrAGE;
230900100308         IF  ErrGenerico;
231000100308           leavesr;
231100100308         ENDIF;
231200090722
231300090805         $video = 'S2';
231400090805         $inzS02 = *on;
231500091111         IMK82Data = wdata;
231600091111         IMK82cAGE = %dec(V2cAGE:7:0);
231700090810
231800090810         //?Carico la sk agenti
231900090810         exsr sr_CarAGE;
232000090708
232100090708       ENDSR;
232200100308
232300100308       //--------------------------------------------------------------
232400100308       //?Controllo commerciale.
232500100308       //--------------------------------------------------------------
232600100308       BEGSR sr_ctrAGE;
232700100308
232800100308         IF  V2cAGE   =  *zeros or V2cAGE = *blanks;
232900100308           errMessage  = *on;
233000100308           errGenerico = *on;
233100100308           PosCurAGE   = *on;
233200100308           V2Dmsg      = $Msg(03);
233300100308           leavesr;
233400100308         ENDIF;
233500100308
233600100308         IF  %scan('?' : V2cAGE) > 0;
233700100308            errGenerico = *on;
233800100308            PosCurAGE   = *on;
233900130828            clear  TRMK43ds;
234000130828            iMK43solU = 'S';
234100130828            iMK43fil  = DUTpou;
234200130828            TRMK43R (kpjba : TRMK43ds);
234300130828            if  oMK43err = *off  and  oMK43fxx = *blank;
234400130828              V2cAGE = %editc( oMK43cmm : 'X' );
234500130828              V2dAGE = oMK43des;
234600130828            endif;
234700100308         ENDIF;
234800100308
234900100308         IF  %check(digits:V2cAGE) > 0;
235000100308           errMessage  = *on;
235100100308           errGenerico = *on;
235200100308           PosCurAGE   = *on;
235300100308           V2Dmsg      = $Msg(04);
235400100308           leavesr;
235500100308         ENDIF;
235600100428
235700100428         //?Se richiesto '?' sul commerciale ricarico la videata
235800100428         IF  errGenerico;
235900100428           IMK82cage = %dec(V2cAGE:7:0);
236000100428           clear V2cAGE;
236100100428           clear V2dAGE;
236200100428           $InzS02 = *on;
236300100428           //?Carico la sk agenti
236400100428           exsr sr_CarAGE;
236500130828           errGenerico = *on;
236600130828           leavesr;
236700100428         ENDIF;
236800100308
236900130828         chain  (%int(V2Cage))  AZCMM000;
237000130828         IF  not %found( AZCMM01L );
237100100308           errMessage  = *on;
237200100308           errGenerico = *on;
237300100308           PosCurAGE   = *on;
237400100308           V2Dmsg      = $Msg(04);
237500100308           leavesr;
237600100308         ENDIF;
237700100308
237800100308         clear TNTAA1DS;
237900100308         ITAA1caut = '§utegtc';
238000100308         ITAA1cmm  = %dec(V2cAGE:7:0);
238100100308         kpjbu     = tntaa1ds;
238200100308         tntaa1c (kpjba);
238300100308         TNTAA1DS = kpjbu;
238400100308         IF  OTAA1fabi = 'N';
238500100308           ErrMessage  = *on;
238600100308           ErrGenerico = *on;
238700100308           PosCurAGE   = *on;
238800100308           V2Dmsg      = $Msg(04);
238900100308           leavesr;
239000100308         ENDIF;
239100100308
239200130828         V2dAGE = CMMdes;
239300110518
239400130828         IF  CMMpar <> *blanks;
239500110518           ErrMessage  = *on;
239600110518           ErrGenerico = *on;
239700110518           PosCurAGE   = *on;
239800110518           V2Dmsg      = $Msg(04);
239900110518           leavesr;
240000110518         ENDIF;
240100100308
240200130828         IF  %dec(V2cAGE:7:0) <> CMMuni;
240300100308           errMessage  = *on;
240400100308           errGenerico = *on;
240500100308           PosCurAGE   = *on;
240600100308           V2Dmsg      = $Msg(08);
240700100308           leavesr;
240800100308         ENDIF;
240900100308
241000100308       ENDSR;
241100090708
241200090708       //--------------------------------------------------------------
241300090708       //?Operazioni finali.
241400090708       //--------------------------------------------------------------
241500090708       BEGSR sr_RoutEnd;
241600090708
241700090708         *inLR = *on;
241800090708         return;
241900090708
242000090708       ENDSR;
242100090708
242200090708      /end-free
242300090708
242400090708       //--------------------------------------------------------------
242500090708       //?Schiere a tempo di compilazione.
242600090708       //--------------------------------------------------------------
242700090708
242800090708** - $MSG -------------------------------------------------------------------*
242900090708Immettere la data                                                              0
243000090708Data errata                                                                    0
243100090708Immettere il commerciale                                                       0
243200090708Commerciale errato o non in gestione all'utente                                0
243300090803Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.    0
243400090803Cursore in area protetta dello schermo.                                        0
243500090803Attività da escludere non valida.                                              0
243600090804Richiedere solo commerciali UNIFICANTI!                                        0
243700090810Almeno un'ora deve essere inserita                                             0
243800100202Utente non abilitato alla funzione                                             1
243900110105
