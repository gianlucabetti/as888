000100090708      //---------------------------------------------------------------
000200090708      //?TRMK82R - Agenda commerciali
000300090708      //---------------------------------------------------------------
000400091123      //?ATTENZIONE!!!!
000500091123      //?Colori fissi a video per tipo attività
000600091123      //?Sono previsti, oltre che nel subfile anche nella WIN x legenda
000700100304      //?Blue      per Telefonata
000800100304      //?Green     per Ufficio
000900100304      //?Pink      per Ferie
001000100304      //?Red       per Appuntamento
001100100304      //?Turquoise per Altre attività
001200100304      //?White     per Offerta
001300100304      //?Yellow    per ______________
001400091125
001500091125      //?Se il pgm imposta "Altre Attività" si carica un tipo attività di WORK = 'W'
001600091125      //?per ora il 'W' non corrisponde a nessuna attività ATCTAT
001700090708
001800090708     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001900091229     h dftactgrp(*no) actgrp(*caller)
002000090708
002100090708      //---------------------------------------------------------------
002200090708      //?Dichiarazione file.
002300090708      //---------------------------------------------------------------
002400090708     ftabel00f  if   e           k disk
002500090804     fcnaco00f  if   e           k disk
002600090709     ftncpo01l  if   e           k disk
002700091222     fazcln01l  if   e           k disk
002800100427      // - Organigramma filiale/aziendale
002900100427     fAZORG01L  if   e           k disk
003000100427      // - Video
003100090805     ftrmk82d   cf   e             workstn
003200090722     f                                     sfile(mk82S02 : S02nrr)
003300090708     f                                     indds(IndDspF)
003400090708     f                                     infds(InfDspF)
003500090708
003600090708      //---------------------------------------------------------------
003700090511      //?Definizione costanti.
003800090511      //---------------------------------------------------------------
003900090708
004000090708      // - Tasti funzionali a video
004100090708     d c_F01           c                   const(x'31')
004200090708     d c_F02           c                   const(x'32')
004300090708     d c_F03           c                   const(x'33')
004400090723     d c_F04           c                   const(x'34')
004500090708     d c_F05           c                   const(x'35')
004600090708     d c_F06           c                   const(x'36')
004700090708     d c_F07           c                   const(x'37')
004800090708     d c_F08           c                   const(x'38')
004900090708     d c_F09           c                   const(x'39')
005000090708     d c_F10           c                   const(x'3A')
005100090810     d c_F11           c                   const(x'3B')
005200090708     d c_F12           c                   const(x'3C')
005300090708     d c_F13           c                   const(x'B1')
005400090708     d c_F14           c                   const(x'B2')
005500090708     d c_F15           c                   const(x'B3')
005600090708     d c_F16           c                   const(x'B4')
005700090708     d c_F17           c                   const(x'B5')
005800090708     d c_F18           c                   const(x'B6')
005900090708     d c_F19           c                   const(x'B7')
006000090708     d c_F20           c                   const(x'B8')
006100090708     d c_F21           c                   const(x'B9')
006200090708     d c_F22           c                   const(x'BA')
006300090708     d c_F23           c                   const(x'BB')
006400090708     d c_F24           c                   const(x'BC')
006500090708     d c_Enter         c                   const(x'F1')
006600090708     d c_RollDown      c                   const(x'F4')
006700090708     d c_RollUp        c                   const(x'F5')
006800091123
006900091123        // - Colori a video
007000091123     d Blue            c                   const(x'3A')
007100091123     d Green           c                   const(x'20')
007200091123     d Pink            c                   const(x'38')
007300091123     d Red             c                   const(x'28')
007400091123     d Turquoise       c                   const(x'30')
007500091123     d White           c                   const(x'22')
007600091123     d Yellow          c                   const(x'32')
007700090723
007800090723     d Digits          c                   const('0123456789')
007900090723
008000100304     d c_AltreA        c                   const('*  *******  ')
008100091125     d c_AttAltre      c                   const('W')
008200090511
008300090511      //---------------------------------------------------------------
008400090511      //?Definizione schiere.
008500090511      //---------------------------------------------------------------
008600090708
008700090708      // - Messaggi di errore
008800100427     d $Msg            s             78    dim(11) ctdata perrcd(1)
008900090804
009000090805      // - Agenti
009100100416     d $Age            s              7  0 dim(200)
009200090709
009300090709      // - Data completa
009400090709     d $Date           s              8  0 dim(07)
009500090721
009600090721      // - Dettaglio
009700090910     d $Det            ds
009800090910     d $DetLunA                      10    dim(%elem($Hour))
009900090910     d $DetLunB                      10    dim(%elem($Hour))
010000090910     d $DetMarA                      10    dim(%elem($Hour))
010100090910     d $DetMarB                      10    dim(%elem($Hour))
010200090910     d $DetMerA                      10    dim(%elem($Hour))
010300090910     d $DetMerB                      10    dim(%elem($Hour))
010400090910     d $DetGioA                      10    dim(%elem($Hour))
010500090910     d $DetGioB                      10    dim(%elem($Hour))
010600090910     d $DetVenA                      10    dim(%elem($Hour))
010700090910     d $DetVenB                      10    dim(%elem($Hour))
010800090910
010900090805     d $DetLunAt       s              1    dim(%elem($Hour))
011000090805     d $DetLunBt       s              1    dim(%elem($Hour))
011100090805     d $DetMarAt       s              1    dim(%elem($Hour))
011200090805     d $DetMarBt       s              1    dim(%elem($Hour))
011300090805     d $DetMerAt       s              1    dim(%elem($Hour))
011400090805     d $DetMerBt       s              1    dim(%elem($Hour))
011500090805     d $DetGioAt       s              1    dim(%elem($Hour))
011600090805     d $DetGioBt       s              1    dim(%elem($Hour))
011700090805     d $DetVenAt       s              1    dim(%elem($Hour))
011800090805     d $DetVenBt       s              1    dim(%elem($Hour))
011900090805
012000090805      // - Dettaglio di work
012100090805     d wDetA           s             10    dim(%elem($Hour))
012200090805     d wDetAt          s              1    dim(%elem($Hour))
012300090805     d wDetB           s             10    dim(%elem($Hour))
012400090805     d wDetBt          s              1    dim(%elem($Hour))
012500090709
012600090709      // - Ora
012700090807     d $Hour           s              6  0 dim(48)
012800031216
012900090708      // - Nome del giorno
013000090709     d $Name_Day       s              3    dim(07)
013100090708
013200090708      // - Nome del mese
013300090709     d $Name_Month     s              9    dim(07)
013400090709
013500090709      // - Numero del giorno
013600090709     d $Number_Day     s              2  0 dim(07)
013700091222
013800091222      // - Festività
013900091222     d $Holidays       s              1    dim(07)
014000090708
014100090511      //---------------------------------------------------------------
014200090511      //?Definizione aree dati.
014300090511      //---------------------------------------------------------------
014400090708
014500090708      // - Dati utente
014600090708     d §AzUte        e ds                  extname(AZUTE00F)
014700090708     d                                     dtaara
014800090708     d §DatiUte      e ds                  extname(dDatiUte)
014900090708     d                                     dtaara
015000090511
015100090511      //---------------------------------------------------------------
015200090511      //?Definizione strutture dati.
015300090511      //---------------------------------------------------------------
015400090708
015500090708      // - Status
015600090708     d Psds           sds
015700090708     d   SDSpgm          *proc
015800090708
015900090708      // - InfDS
016000090708     d InfDspF         ds
016100090708     d  dsp_aid              369    369a                                        AID byte
016200090724     d  dsp_rig              370    370
016300090724     d  dsp_col              371    371
016400090708
016500090708      // - Indicatori su DspF
016600090708     d IndDspF         ds
016700090806        // - Abilitazione tasti funzionali
016800091229     d  F10Attivo                     1n   overlay(IndDspF : 10)
016900091217     d  F11Attivo                     1n   overlay(IndDspF : 11)
017000090721        // - Indicatori di gestione del subfile
017100090721     d  SflDsp_N                      1n   overlay(IndDspF : 30)
017200090721     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
017300090722     d  SflEnd                        1n   overlay(IndDspF : 32)
017400090708        // - Indicatori di errore
017500090708     d  ErrMessage                    1n   overlay(IndDspF : 28)
017600091123     d  PosCurDATA                    1n   overlay(IndDspF : 50)
017700091123     d  PosCurAGE                     1n   overlay(IndDspF : 51)
017800091123     d  PosCurATT                     1n   overlay(IndDspF : 52)
017900091123     d  PosCurORA                     1n   overlay(IndDspF : 53)
018000091217     d  NotDsp                        1n   overlay(IndDspf : 40)
018100090708     d  ErrGenerico                   1n   overlay(IndDspF : 99)
018200090724
018300090724      // Cursore
018400090724     d ds_cursor       ds
018500090724     d  ds_riga                1      2b 0 inz
018600090724     d  $riga                  2      2
018700090724     d  ds_colo                3      4b 0 inz
018800090724     d  $colo                  4      4
018900091222
019000091222      // - Mattino/Pomeriggio calendario
019100091222     d CLNmat          DS
019200091222     d  mat                    1     31    dim(31)
019300091222     d CLNpom          DS
019400091222     d  pom                    1     31    dim(31)
019500090724
019600090708      // - Controllo data
019700090708     d wlbdat          ds                  inz
019800090708     d  g02dat                 1      8  0
019900090708     d  g02inv                 9     16  0
020000090708     d  g02err                17     17
020100090708     d  g02tgi                18     22  0
020200090708
020300090708      // - Parametri ricevuti
020400090708     d KPJBA         e ds
020500090708     d TRMK82ds      e ds
020600090805     d  $NoAtt                41     50    dim(10)
020700090708
020800090708      // - Reperimento dati utente
020900090708     d TIBS34ds      e ds
021000090805
021100090805      // - Gestione tabelle: controllo e ricerca
021200090805     d TIBS02ds      e ds                  inz
021300090805     d   T02tla      e                     inz('L')
021400090805     d   T02cod      e                     inz('TTA')
021500100208
021600100208      // - controllo abilitazioni
021700100208     d TNTAA1DS      e ds                  inz
021800091228
021900091228      // - Inserimento attività
022000091228     d TRMK17DS      e ds
022100090806
022200090806      // - Interrogazione attività
022300090806     d TRMK83DS      e ds
022400100308
022500100308      // - Stampa agenda
022600100308     d TRMK86DS      e ds
022700090708
022800090708      // Tabella agenti
022900090708     d ds01          e ds
023000100202
023100100202      // Abilitazioni utente
023200100202     d dUte01        e ds
023300090709
023400090709      // File attività
023500090709     d tiatc00f      e ds                  extname(TIATC00F)
023600100305
023700100305      // File trattativa
023800100305     d tivis00f      e ds                  extname(TIVIS00F)
023900090708
024000090708      // Campi di comodo per gestione indicatori a video
024100090708     d WindDspF        ds                  inz
024200091123     d   WindDspFa             1     49    inz(*zero)
024300091123     d   WindDspFb            50     99    inz(*zero)
024400090511
024500090511      //---------------------------------------------------------------
024600090511      //?Definizione variabili globali.
024700090511      //---------------------------------------------------------------
024800090708
024900090708      // - Flags booleani
025000090805     d $End            s               n   inz(*off)
025100090805     d $EndAge         s               n   inz(*off)
025200100202     d $ErrGrave       s               n   inz(*off)
025300090807     d $Fine           s               n   inz(*off)
025400090807     d $Hour1          s               n   inz(*off)
025500090807     d $Hour2          s               n   inz(*off)
025600090807     d $InzD01         s               n   inz(*on)
025700090807     d $InzS02         s               n   inz(*off)
025800090807     d $Minute30       s               n   inz(*off)
025900100305     d $RcdOk          s               n   inz(*off)
026000091111     d $Richiamato     s               n   inz(*off)
026100090807     d $Week           s               n   inz(*off)
026200090708
026300090708      // - Campi associati al video
026400090708     d $Video          s              2    inz('D1')
026500090722     d S02Nrr          s              4  0 inz
026600090804
026700090804       // - Stringa SQL da eseguire
026800090804     d wSQL            s           2048    Varying        inz
026900090708
027000090708      // - Campi di comodo
027100090805     d sav_atcdad      s                   like(atcdad)
027200090807     d sav_xx          s                   like(xx)
027300090723     d tb85Kfil        s              3
027400090723     d tb85Key         s              8
027500090723     d tb85Des1        s             25
027600090810     d tb85Uni         s              1    inz('S')
027700090721     d watcoii         s              6  0 inz
027800090721     d watcofi         s              6  0 inz
027900090724     d wColo           s              3s 0
028000090708     d wDay_Of_Week    s              2  0 inz
028100090721     d wdata           s              8  0
028200090721     d wDate_EUR       s               d   datfmt(*eur)
028300090721     d wDate_ISO       s               d   datfmt(*iso)
028400090721     d wDate_ISO_Par   s               d   datfmt(*iso)
028500100427     d wDate_SYS       s               d   datfmt(*iso)  inz(*sys)
028600090721     d wDesCpo         s             10
028700090721     d wHour           s              2  0 inz
028800090721     d wHour_Iso       s               t   inz(*loval)
028900090721     d wHour_Iso_F     s               t   inz
029000090721     d wHour_Iso_I     s               t   inz
029100100504     d wMin            s              2  0 inz
029200090708     d wName_Of_Day    s             10    inz
029300090708     d wName_Of_Month  s             10    inz
029400090807     d wRiga           s              3s 0
029500090807     d wTot_H          s              5  0 inz
029600100504     d wTot_M          s              5  0 inz
029700100504     d wTot_R          s              5  0 inz
029800090807     d wxxh1           s              2  0 inz(9)
029900090807     d wxxh2           s              2  0 inz(3)
030000090807     d wxxm            s              2  0 inz(17)
030100090804     d xx              s              3  0 inz
030200090804     d yy              s              3  0 inz
030300090805     d zz              s              3  0 inz
030400090511
030500090511      //---------------------------------------------------------------
030600090511      //?Definizione procedure esterne.
030700090511      //---------------------------------------------------------------
030800090708
030900090708      // - Reperimento dati utente
031000090708     d tibs34r         pr                  extpgm('TIBS34R')
031100090708     d  tibs34ds                           likeds(TIBS34ds)
031200090805
031300090805      // - Gestione tabelle
031400090805     d tibs02r         pr                  extpgm('TIBS02R')
031500090805     d  kpjba                              likeds(KPJBA)    const
031600090805     d  tibs02ds                           likeds(TIBS02ds)
031700091228
031800091228      // - Inserimento attività
031900091228     d trmk17r         pr                  extpgm('TRMK17R')
032000091228     d  kpjba                              likeds(KPJBA)
032100091228     d  trmk17ds                           likeds(TRMK17ds)
032200090806
032300090806      // - Interrogazione attività
032400090806     d trmk83r         pr                  extpgm('TRMK83R')
032500090806     d  kpjba                              likeds(KPJBA)
032600091111     d  trmk83ds                           likeds(TRMK83ds)
032700100308
032800100308      // - Stampa agenda
032900100308     d trmk86r         pr                  extpgm('TRMK86R')
033000100308     d  kpjba                              likeds(KPJBA)
033100100308     d  trmk86ds                           likeds(TRMK86ds)
033200090708
033300100208      /copy gaitrasrc/srcprotopr,tntaa1c
033400090723      /copy gaitrasrc/srcprotopr,trtb85r
033500090708      /copy gaitrasrc/srcprotopr,xsrda8
033600090511
033700090511      //---------------------------------------------------------------
033800090511      //?Definizione key-list.
033900090511      //---------------------------------------------------------------
034000090804
034100090804       // - File CNACO00F
034200090804     d k03cnaco      e ds                  extname(CNACO00F:*key)
034300090804     d                                     prefix(k_)
034400090804     d                                     inz
034500090708
034600090708       // - File TABEL00F
034700090708     d k03tabel      e ds                  extname(TABEL00F:*key)
034800090708     d                                     prefix(k_)
034900090708     d                                     inz
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
051000090805          select tblkey, tbluni
051100090807          from tabel00f where tblkut = '1' and tblcod = '01' and
051200090807          tblflg = ' ' order by tblkut, tblcod, tblkey;
051300090805
051400090805         exec sql
051500090805           open AGE;
051600090805           IF sqlcode < 0;
051700090805             $EndAge = *on;
051800090805           ENDIF;
051900090805
052000090805         DOW not $EndAge;
052100090805           exec sql
052200090805             fetch next from AGE into :tblkey, :tbluni;
052300090805             IF sqlcod = 100 or sqlcod < 0;
052400090805               $EndAge = *on;
052500090805               leave;
052600090805             ENDIF;
052700090805             ds01 = Tbluni;
052800091111             IF  §01rgf = IMK82cAGE;
052900090805               xx += 1;
053000090805               $Age(xx) = %dec(%subst(tblkey:1:7):7:0);
053100090805             ENDIF;
053200090805         ENDDO;
053300090805
053400090805         exec sql close AGE;
053500090810
053600090810         //?Se non ho cricato niente memorizzo almeno il codice richiesto
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
065700100428            tb85Kfil = %editc(DUTpou:'X');
065800090723            clear tb85Key ;
065900090723            clear tb85Des1;
066000090810            trtb85r (kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni);
066100090723            V1cAGE = tb85Key ;
066200090723            V1dAGE = tb85Des1 ;
066300090723            leavesr;
066400090723         ENDIF;
066500090723
066600090723         IF  %check(digits:V1cAGE) > 0;
066700090723           errMessage  = *on;
066800090723           errGenerico = *on;
066900090723           PosCurAGE   = *on;
067000090723           V1Dmsg      = $Msg(04);
067100090723           leavesr;
067200090723         ENDIF;
067300090708
067400090722         clear ds01;
067500090722         k_tblkut = 1;
067600090722         k_tblcod = '01';
067700090723         k_tblkey = V1cAGE;
067800090722         chain  %kds(K03tabel) TABEL00F;
067900090722         IF  not %found( TABEL00F ) or TBLflg <> *blanks;
068000090722           errMessage  = *on;
068100090722           errGenerico = *on;
068200090722           PosCurAGE   = *on;
068300090722           V1Dmsg      = $Msg(04);
068400090722           leavesr;
068500090722         ENDIF;
068600100202
068700100208         clear TNTAA1DS;
068800100208         ITAA1caut = '§utegtc';
068900100208         ITAA1cmm  = %dec(V1cAGE:7:0);
069000100208         kpjbu     = tntaa1ds;
069100100208         tntaa1c (kpjba);
069200100208         TNTAA1DS = kpjbu;
069300100208         IF  OTAA1fabi = 'N';
069400100202           ErrMessage  = *on;
069500100202           ErrGenerico = *on;
069600100202           PosCurAGE   = *on;
069700100202           V1Dmsg      = $Msg(04);
069800100202           leavesr;
069900100202         ENDIF;
070000090708
070100090722         ds01 = TBLuni;
070200090722         V1dAGE = §01age;
070300110518
070400110518         IF  §01par <> *blanks;
070500110518           ErrMessage  = *on;
070600110518           ErrGenerico = *on;
070700110518           PosCurAGE   = *on;
070800110518           V1Dmsg      = $Msg(04);
070900110518           leavesr;
071000110518         ENDIF;
071100090804
071200090805         IF  %dec(V1cAGE:7:0) <> §01rgf;
071300090804           errMessage  = *on;
071400090804           errGenerico = *on;
071500090804           PosCurAGE   = *on;
071600090805           V1Dmsg      = $Msg(08);
071700090804           leavesr;
071800090804         ENDIF;
071900100202
072000090803         //?Attività da escludere
072100090805         IF  %scan('?' : V1Att01) > *zeros;
072200090805             errGenerico = *on;
072300091123             PosCurAtt   = *on;
072400090805             clear kpjbu;
072500090805             reset tibs02ds;
072600090805             T02mod = 'R';
072700090805             tibs02r(kpjba:tibs02ds);
072800090805             V1Att01 = t02ke1;
072900090805             leavesr;
073000090805           ENDIF;
073100090805         IF  %scan('?' : V1Att02) > *zeros;
073200090805             errGenerico = *on;
073300091123             PosCurAtt   = *on;
073400090805             clear kpjbu;
073500090805             reset tibs02ds;
073600090805             T02mod = 'R';
073700090805             tibs02r(kpjba:tibs02ds);
073800090805             V1Att02 = t02ke1;
073900090805             leavesr;
074000090805           ENDIF;
074100090805         IF  %scan('?' : V1Att03) > *zeros;
074200090805             errGenerico = *on;
074300091123             PosCurAtt   = *on;
074400090805             clear kpjbu;
074500090805             reset tibs02ds;
074600090805             T02mod = 'R';
074700090805             tibs02r(kpjba:tibs02ds);
074800090805             V1Att03 = t02ke1;
074900090805             leavesr;
075000090805           ENDIF;
075100090805         IF  %scan('?' : V1Att04) > *zeros;
075200090805             errGenerico = *on;
075300091123             PosCurAtt   = *on;
075400090805             clear kpjbu;
075500090805             reset tibs02ds;
075600090805             T02mod = 'R';
075700090805             tibs02r(kpjba:tibs02ds);
075800090805             V1Att04 = t02ke1;
075900090805             leavesr;
076000090805           ENDIF;
076100090805         IF  %scan('?' : V1Att05) > *zeros;
076200090805             errGenerico = *on;
076300091123             PosCurAtt   = *on;
076400090805             clear kpjbu;
076500090805             reset tibs02ds;
076600090805             T02mod = 'R';
076700090805             tibs02r(kpjba:tibs02ds);
076800090805             V1Att05 = t02ke1;
076900090805             leavesr;
077000090805           ENDIF;
077100090805         IF  %scan('?' : V1Att06) > *zeros;
077200090805             errGenerico = *on;
077300091123             PosCurAtt   = *on;
077400090805             clear kpjbu;
077500090805             reset tibs02ds;
077600090805             T02mod = 'R';
077700090805             tibs02r(kpjba:tibs02ds);
077800090805             V1Att06 = t02ke1;
077900090805             leavesr;
078000090805           ENDIF;
078100090805         IF  %scan('?' : V1Att07) > *zeros;
078200090805             errGenerico = *on;
078300091123             PosCurAtt   = *on;
078400090805             clear kpjbu;
078500090805             reset tibs02ds;
078600090805             T02mod = 'R';
078700090805             tibs02r(kpjba:tibs02ds);
078800090805             V1Att07 = t02ke1;
078900090805             leavesr;
079000090805           ENDIF;
079100090805         IF  %scan('?' : V1Att08) > *zeros;
079200090805             errGenerico = *on;
079300091123             PosCurAtt   = *on;
079400090805             clear kpjbu;
079500090805             reset tibs02ds;
079600090805             T02mod = 'R';
079700090805             tibs02r(kpjba:tibs02ds);
079800090805             V1Att08 = t02ke1;
079900090805             leavesr;
080000090805           ENDIF;
080100090805         IF  %scan('?' : V1Att09) > *zeros;
080200090805             errGenerico = *on;
080300091123             PosCurAtt   = *on;
080400090805             clear kpjbu;
080500090805             reset tibs02ds;
080600090805             T02mod = 'R';
080700090805             tibs02r(kpjba:tibs02ds);
080800090805             V1Att09 = t02ke1;
080900090805             leavesr;
081000090805           ENDIF;
081100090805         IF  %scan('?' : V1Att10) > *zeros;
081200090805             errGenerico = *on;
081300091123             PosCurAtt   = *on;
081400090805             clear kpjbu;
081500090805             reset tibs02ds;
081600090805             T02mod = 'R';
081700090805             tibs02r(kpjba:tibs02ds);
081800090805             V1Att10 = t02ke1;
081900090805             leavesr;
082000090805           ENDIF;
082100090805
082200090805         IF  V1Att01 <> *blanks;
082300090805           clear kpjbu;
082400090805           reset tibs02ds;
082500090805           T02mod = 'C';
082600090805           T02Ke1 = V1Att01;
082700090805           tibs02r(kpjba:tibs02ds);
082800090805           IF  T02Err <> *blanks;
082900090805             errMessage  = *on;
083000090805             errGenerico = *on;
083100091123             PosCurAtt   = *on;
083200090805             V1Dmsg      = $Msg(07);
083300090805             leavesr;
083400090805           ENDIF;
083500090803         ENDIF;
083600090805         IF  V1Att02 <> *blanks;
083700090805           clear kpjbu;
083800090805           reset tibs02ds;
083900090805           T02mod = 'C';
084000090805           T02Ke1 = V1Att02;
084100090805           tibs02r(kpjba:tibs02ds);
084200090805           IF  T02Err <> *blanks;
084300090805             errMessage  = *on;
084400090805             errGenerico = *on;
084500091123             PosCurAtt   = *on;
084600090805             V1Dmsg      = $Msg(07);
084700090805             leavesr;
084800090805           ENDIF;
084900090805         ENDIF;
085000090805         IF  V1Att03 <> *blanks;
085100090805           clear kpjbu;
085200090805           reset tibs02ds;
085300090805           T02mod = 'C';
085400090805           T02Ke1 = V1Att03;
085500090805           tibs02r(kpjba:tibs02ds);
085600090805           IF  T02Err <> *blanks;
085700090805             errMessage  = *on;
085800090805             errGenerico = *on;
085900091123             PosCurAtt   = *on;
086000090805             V1Dmsg      = $Msg(07);
086100090805             leavesr;
086200090805           ENDIF;
086300090805         ENDIF;
086400090805         IF  V1Att04 <> *blanks;
086500090805           clear kpjbu;
086600090805           reset tibs02ds;
086700090805           T02mod = 'C';
086800090805           T02Ke1 = V1Att04;
086900090805           tibs02r(kpjba:tibs02ds);
087000090805           IF  T02Err <> *blanks;
087100090805             errMessage  = *on;
087200090805             errGenerico = *on;
087300091123             PosCurAtt   = *on;
087400090805             V1Dmsg      = $Msg(07);
087500090805             leavesr;
087600090805           ENDIF;
087700090805         ENDIF;
087800090805         IF  V1Att05 <> *blanks;
087900090805           clear kpjbu;
088000090805           reset tibs02ds;
088100090805           T02mod = 'C';
088200090805           T02Ke1 = V1Att05;
088300090805           tibs02r(kpjba:tibs02ds);
088400090805           IF  T02Err <> *blanks;
088500090805             errMessage  = *on;
088600090805             errGenerico = *on;
088700091123             PosCurAtt   = *on;
088800090805             V1Dmsg      = $Msg(07);
088900090805             leavesr;
089000090805           ENDIF;
089100090805         ENDIF;
089200090805         IF  V1Att06 <> *blanks;
089300090805           clear kpjbu;
089400090805           reset tibs02ds;
089500090805           T02mod = 'C';
089600090805           T02Ke1 = V1Att06;
089700090805           tibs02r(kpjba:tibs02ds);
089800090805           IF  T02Err <> *blanks;
089900090805             errMessage  = *on;
090000090805             errGenerico = *on;
090100091123             PosCurAtt   = *on;
090200090805             V1Dmsg      = $Msg(07);
090300090805             leavesr;
090400090805           ENDIF;
090500090805         ENDIF;
090600090805         IF  V1Att07 <> *blanks;
090700090805           clear kpjbu;
090800090805           reset tibs02ds;
090900090805           T02mod = 'C';
091000090805           T02Ke1 = V1Att07;
091100090805           tibs02r(kpjba:tibs02ds);
091200090805           IF  T02Err <> *blanks;
091300090805             errMessage  = *on;
091400090805             errGenerico = *on;
091500091123             PosCurAtt   = *on;
091600090805             V1Dmsg      = $Msg(07);
091700090805             leavesr;
091800090805           ENDIF;
091900090805         ENDIF;
092000090805         IF  V1Att08 <> *blanks;
092100090805           clear kpjbu;
092200090805           reset tibs02ds;
092300090805           T02mod = 'C';
092400090805           T02Ke1 = V1Att08;
092500090805           tibs02r(kpjba:tibs02ds);
092600090805           IF  T02Err <> *blanks;
092700090805             errMessage  = *on;
092800090805             errGenerico = *on;
092900091123             PosCurAtt   = *on;
093000090805             V1Dmsg      = $Msg(07);
093100090805             leavesr;
093200090805           ENDIF;
093300090805         ENDIF;
093400090805         IF  V1Att09 <> *blanks;
093500090805           clear kpjbu;
093600090805           reset tibs02ds;
093700090805           T02mod = 'C';
093800090805           T02Ke1 = V1Att09;
093900090805           tibs02r(kpjba:tibs02ds);
094000090805           IF  T02Err <> *blanks;
094100090805             errMessage  = *on;
094200090805             errGenerico = *on;
094300091123             PosCurAtt   = *on;
094400090805             V1Dmsg      = $Msg(07);
094500090805             leavesr;
094600090805           ENDIF;
094700090805         ENDIF;
094800090805         IF  V1Att10 <> *blanks;
094900090805           clear kpjbu;
095000090805           reset tibs02ds;
095100090805           T02mod = 'C';
095200090805           T02Ke1 = V1Att10;
095300090805           tibs02r(kpjba:tibs02ds);
095400090805           IF  T02Err <> *blanks;
095500090805             errMessage  = *on;
095600090805             errGenerico = *on;
095700091123             PosCurAtt   = *on;
095800090805             V1Dmsg      = $Msg(07);
095900090805             leavesr;
096000090805           ENDIF;
096100090805         ENDIF;
096200090810
096300090810         //?Controllo che almeno 1 orario sia richiesto
096400090810         IF  V1Hour1 = *blanks and V1Hour2 = *blanks and
096500090810            V1Minute = *blanks;
096600090810             errMessage  = *on;
096700090810             errGenerico = *on;
096800090810             PosCurORA = *on;
096900090810             V1Dmsg      = $Msg(09);
097000090810             leavesr;
097100090810         ENDIF;
097200090708
097300090708         //?Imposto i campi da passare alla seconda videata
097400091111         IMK82Data = wdata;
097500091111         IMK82cAGE = %dec(V1cAGE:7:0);
097600090805
097700090805         //?Carico la sk attività da escludere
097800090805         $Noatt(01) = V1Att01;
097900090805         $Noatt(02) = V1Att02;
098000090805         $Noatt(03) = V1Att03;
098100090805         $Noatt(04) = V1Att04;
098200090805         $Noatt(05) = V1Att05;
098300090805         $Noatt(06) = V1Att06;
098400090805         $Noatt(07) = V1Att07;
098500090805         $Noatt(08) = V1Att08;
098600090805         $Noatt(09) = V1Att09;
098700090805         $Noatt(10) = V1Att10;
098800090805
098900090805         //?Carico la sk agenti
099000090805         exsr sr_CarAGE;
099100090807
099200090807         //?Imposto la visualizzazione
099300090807         $Hour1 = (V1Hour1 <> *blanks);
099400090807         $Hour2 = (V1Hour2 <> *blanks);
099500090807         $Minute30 = (V1Minute <> *blanks);
099600090708
099700090708       ENDSR;
099800090708
099900090708       //--------------------------------------------------------------
100000090805       //?Gestione videata S02.
100100090708       //--------------------------------------------------------------
100200090722       BEGSR sr_GesS02;
100300090708
100400090708         //?Inizializzazione videata
100500090722         IF  $InzS02 = *on;
100600090722           exsr sr_InzS02;
100700090722           $InzS02 = *off;
100800090722         ENDIF;
100900090708
101000090708         //?Emissione videata
101100090722         sfldsp_n = *off;
101200090722         V2hRcd = 1;
101300091217         F11Attivo = $Week;
101400100416         //?Posiziono il cursore sempre sul commerciale
101500100416         PosCurAGE = *on;
101600090708         write MK82T01;
101700090709         write MK82P02;
101800090722         exfmt MK82C02;
101900090708         ErrMessage  = *off;
102000090708         ErrGenerico = *off;
102100090708         clear V2Dmsg;
102200100204
102300100308         //?- Controllo subito il commerciale perchè se errato non devo fare vedere niente
102400100308         IF  V2cAGE <> *zeros and V2cAGE <> *blanks and
102500100308             dsp_aid <> c_F12 and dsp_aid <> c_F01;
102600100308           exsr sr_ctrAGE;
102700100204           IF  errGenerico;
102800100204             leavesr;
102900100204           ENDIF;
103000100204         ENDIF;
103100090708
103200090708         SELECT;
103300090724
103400090724           //?F1=Help Legenda colori
103500090724           WHEN  dsp_aid = c_F01;
103600090724             exsr sr_F01S02;
103700090722
103800090723           //?F4=Settimana Precedente
103900090723           WHEN  dsp_aid = c_F04;
104000090723             exsr sr_F04S02;
104100090722
104200090723           //?F5=Settimana Successiva
104300090723           WHEN  dsp_aid = c_F05;
104400090723             exsr sr_F05S02;
104500100308
104600100308           //?F8=Stampa
104700100308           WHEN  dsp_aid = c_F08;
104800100308             exsr sr_F08S02;
104900090724
105000090724           //?F9=Dettaglio
105100090724           WHEN  dsp_aid = c_F09;
105200090724             exsr sr_F09S02;
105300091217
105400091217           //?F10=Attività
105500091217           WHEN  dsp_aid = c_F10;
105600091217             exsr sr_F10S02;
105700090806
105800091217           //?F11=Week-End
105900091217           WHEN  dsp_aid = c_F11;
106000091217             exsr sr_F11S02;
106100091111
106200091111           //?F12=Ritorno
106300091111           WHEN  dsp_aid = c_F12;
106400091111             exsr sr_F12S02;
106500090722
106600090722           //?RollUp - PAGEDOWN
106700090722           WHEN  dsp_aid = c_RollUp;
106800090722             exsr sr_RollUpS02;
106900090722
107000090722           //?RollDown - PAGEUP
107100090722           WHEN  dsp_aid = c_RollDown;
107200090722             exsr sr_RollDownS02;
107300090708
107400090708           //?Enter
107500100308           OTHER;
107600100308           //?- Controllo dati
107700100308             exsr sr_CtrS02;
107800100308             IF  errGenerico;
107900100308               leavesr;
108000100308             ENDIF;
108100090708
108200090708         ENDSL;
108300090708
108400090708       ENDSR;
108500090708
108600090708       //--------------------------------------------------------------
108700090805       //?Inizializzazione videata S02.
108800090708       //--------------------------------------------------------------
108900090722       BEGSR sr_InzS02;
109000091027
109100091027         clear sav_atcdad;
109200090708
109300090722         //?Pulizia subfile
109400090722         exsr Sr_PulSfl;
109500090708
109600090708         //?imposto i dati ricevuti
109700091230          IF  V2Data = 0;
109800091230            wDate_ISO = %date(IMK82Data:*iso);
109900091230            wDate_EUR = wDate_ISO;
110000091230            V2Data = %dec(wDate_EUR);
110100091230          ENDIF;
110200091230          IF  V2cAGE = *blanks;
110300091230            V2cAGE = %editc(IMK82cAGE:'X');
110400091230          ENDIF;
110500090907
110600091027         //?Se commerciale impostato
110700091105          IF  V2cAGE <> *blanks and V2cAGE <> *zeros;
110800091027            clear ds01;
110900091027            k_tblkut = 1;
111000091027            k_tblcod = '01';
111100091105            k_tblkey = V2cAGE;
111200091027            chain  %kds(K03tabel) TABEL00F;
111300091027            IF  not %found( TABEL00F ) or TBLflg <> *blanks;
111400091027              errMessage  = *on;
111500091027              errGenerico = *on;
111600091027              PosCurAGE   = *on;
111700091027              V2Dmsg      = $Msg(04);
111800100203              clear $AGE;
111900100331            ELSE;
112000100331              ds01 = TBLuni;
112100100331              V2dAGE = §01age;
112200100331            ENDIF;
112300100331           //?se programma richiamato e il commerciale impostato non è un
112400100331           //?commerciale unificante mi carico il suo unificante
112500100331            IF  $Richiamato and §01rgf <> %dec(V2cAGE:7:0) and not ErrGenerico;
112600100331              V2cAge = %editc(§01rgf:'X');
112700100331              IMK82cAGE = %dec(V2cAGE:7:0);
112800100331              exsr sr_CarAGE;
112900100331            ENDIF;
113000100203         //?Controllo se utente abilitato all'agente
113100100331           IF  not ErrGenerico;
113200100331             clear TNTAA1DS;
113300100331             ITAA1caut = '§utegtc';
113400100331             ITAA1cmm  = %dec(V2cAGE:7:0);
113500100331             kpjbu     = tntaa1ds;
113600100331             tntaa1c (kpjba);
113700100331             TNTAA1DS = kpjbu;
113800100331             IF  OTAA1fabi = 'N';
113900100331                ErrMessage  = *on;
114000100331                ErrGenerico = *on;
114100100331                PosCurAGE   = *on;
114200100331                V2Dmsg      = $Msg(04);
114300100203           //?pulisco la sk $age così non viene caricato niente nel subfile
114400100331                clear $AGE;
114500100331              ENDIF;
114600100331            ENDIF;
114700100203         //?Se commerciale non impostato passo l'errore e non vado a fine
114800091027         //?della routine altrimenti non carica subfile vuoto
114900091027          ELSE;
115000091027            errMessage  = *on;
115100091027            errGenerico = *on;
115200091027            PosCurAGE   = *on;
115300091027            V2Dmsg      = $Msg(04);
115400091027          ENDIF;
115500090708
115600090709         //?Carico la settimana partendo dalla data richiesta
115700090805         exsr sr_CarDay;
115800090721
115900090721         //?Carico le ore
116000090805         exsr sr_CarHour;
116100090721
116200090722         //?Carico i dati nel control
116300090805         exsr sr_CarCTRL;
116400090708
116500090806         //?Carico le attività relative a 7 gg
116600090805         exsr sr_CarAttivita;
116700090910
116800091217         //?Se non ho caricato niente spengo indicatore F11=Week
116900090910         IF $Det = *blanks;
117000090910           $Week = *off;
117100090910         ENDIF;
117200090709
117300090722         //?Carico la prima pagina del subfile
117400090807         //?  parto sempre dalle h. 08:00
117500090807         SELECT;
117600090807           WHEN $Hour1;
117700090807             xx = wxxh1;
117800090807             sav_xx = wxxh1;
117900090807           WHEN $Hour2;
118000090807             xx = wxxh2;
118100090807             sav_xx = wxxh2;
118200090807           WHEN $Minute30;
118300090807             xx = wxxm;
118400090807             sav_xx = wxxm;
118500090807         ENDSL;
118600090807         exsr sr_CarS02;
118700090708
118800090708       ENDSR;
118900090708
119000090708       //--------------------------------------------------------------
119100090805       //?Carico le sk dei gg.
119200090708       //--------------------------------------------------------------
119300090805       BEGSR sr_CarDay;
119400090708
119500090709         clear $Name_Day;
119600090709         clear $Name_Month;
119700090709         clear $Number_Day;
119800090708         clear $Date;
119900091222         clear $Holidays;
120000090708
120100090708         //?devo sempre partire dal Lunedì
120200091111         wDate_ISO_Par= %date(IMK82Data);
120300090709         clear wDay_Of_Week;
120400090709         exec sql
120500090709           set :wDay_Of_Week = dayofweek_iso(:wDate_ISO_Par);
120600090709
120700090709         //?carico le sk dei gg da visualizzare
120800090709           xx = 1;
120900090805           FOR xx by 1 to %elem($Date);
121000090709             SELECT;
121100090709
121200090709               WHEN xx = wDay_Of_Week;
121300090709                 wDate_ISO = wDate_ISO_Par;
121400090709                 exsr Set_Name;
121500090709                 $Date(xx)       = %dec(wDate_ISO);
121600090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
121700090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
121800090709                 $Name_Month(xx) = wName_Of_Month;
121900091222                 exsr Calendario;
122000090709
122100090709               WHEN xx < wDay_Of_Week;
122200090709                 wDate_ISO = wDate_ISO_Par - %days(wDay_Of_Week - xx);
122300090709                 exsr Set_Name;
122400090709                 $Date(xx)       = %dec(wDate_ISO);
122500090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
122600090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
122700090709                 $Name_Month(xx) = wName_Of_Month;
122800091222                 exsr Calendario;
122900090709
123000090709               WHEN xx > wDay_Of_Week;
123100090709                 wDate_ISO = wDate_ISO_Par + %days(xx - wDay_Of_Week);
123200090709                 exsr Set_Name;
123300090709                 $Date(xx)       = %dec(wDate_ISO);
123400090709                 $Number_Day(xx) = %subdt(wDate_ISO:*days);
123500090709                 $Name_Day(xx)   = %subst(wName_Of_Day:1:3);
123600090709                 $Name_Month(xx) = wName_Of_Month;
123700091222                 exsr Calendario;
123800090709             ENDSL;
123900090709
124000090709           ENDFOR;
124100090708
124200090708       ENDSR;
124300090709
124400090709       //--------------------------------------------------------------
124500090805       //?Carico la sk delle ore.
124600090709       //--------------------------------------------------------------
124700090805       BEGSR sr_CarHour;
124800090709
124900090722         reset wHour_ISO;
125000090709         xx = 1;
125100090807         clear yy;
125200090805         FOR xx by 1 to %elem($Hour);
125300090709
125400090807           IF ($Hour1 and yy = 24) or
125500090807              ($Hour2 and yy = 12);
125600090807              leave;
125700090807           ENDIF;
125800090807
125900090807           yy += 1;
126000090807
126100090807           IF xx = 1;
126200090807             $Hour(xx) = %dec(wHour_ISO);
126300090807           ELSE;
126400090807             SELECT;
126500090807               WHEN  $Minute30;
126600090807                 wHour_ISO = wHour_ISO + %minutes(30);
126700090807               WHEN  $Hour1;
126800090807                 wHour_ISO = wHour_ISO + %hours(1);
126900090807               WHEN  $Hour2;
127000090807                 wHour_ISO = wHour_ISO + %hours(2);
127100090807             ENDSL;
127200090807             $Hour(xx) = %dec(wHour_ISO);
127300090807           ENDIF;
127400090709
127500090709         ENDFOR;
127600090709
127700090709       ENDSR;
127800090722
127900090722       //--------------------------------------------------------------
128000090805       //?Carico i dati nel CONTROL.
128100090722       //--------------------------------------------------------------
128200090805       BEGSR sr_CarCTRL;
128300090722
128400090722         //?Carico riga di testata per il subfile
128500091222         v2criga = '___________|___';
128600091222         IF  $Holidays(01) = '1';
128700091222           V2cRiga = %trim(V2cRiga) + Pink +
128800091222                   $Name_Day(01)    + ' ' +
128900091222                   %editc($Number_Day(01):'X') + ' ' +
129000091222                   $Name_Month(01) + Green +
129100091222                   '__|___';
129200091222         ELSE;
129300091222           V2cRiga = %trim(V2cRiga) + ' ' +
129400091222                   $Name_Day(01)    + ' ' +
129500091222                   %editc($Number_Day(01):'X') + ' ' +
129600091222                   $Name_Month(01) +
129700091222                   ' __|___';
129800091222         ENDIF;
129900091222         IF  $Holidays(02) = '1';
130000091222           V2cRiga = %trim(V2cRiga) + Pink +
130100091222                   $Name_Day(02)   + ' ' +
130200091222                   %editc($Number_Day(02):'X') + ' ' +
130300091222                   $Name_Month(02) + Green +
130400091222                   '__|___';
130500091222         ELSE;
130600091222           V2cRiga = %trim(V2cRiga) + ' ' +
130700090722                   $Name_Day(02)   + ' ' +
130800090722                   %editc($Number_Day(02):'X') + ' ' +
130900091222                   $Name_Month(02) +
131000091222                   ' __|___';
131100091222         ENDIF;
131200091222         IF  $Holidays(03) = '1';
131300091222           V2cRiga = %trim(V2cRiga) + Pink +
131400091222                   $Name_Day(03)   + ' ' +
131500091222                   %editc($Number_Day(03):'X') + ' ' +
131600091222                   $Name_Month(03) + Green +
131700091222                   '__|___';
131800091222         ELSE;
131900091222         V2cRiga = %trim(V2cRiga) + ' ' +
132000091222                   $Name_Day(03)   + ' ' +
132100091222                   %editc($Number_Day(03):'X') + ' ' +
132200091222                   $Name_Month(03) +
132300091222                   ' __|___';
132400091222         ENDIF;
132500091222         IF  $Holidays(04) = '1';
132600091222           V2cRiga = %trim(V2cRiga) + Pink +
132700091222                   $Name_Day(04)   + ' ' +
132800091222                   %editc($Number_Day(04):'X') + ' ' +
132900091222                   $Name_Month(04) + Green +
133000091222                   '__|___';
133100091222         ELSE;
133200091222           V2cRiga = %trim(V2cRiga) + ' ' +
133300090722                   $Name_Day(04)   + ' ' +
133400090722                   %editc($Number_Day(04):'X') + ' ' +
133500091222                   $Name_Month(04) +
133600091222                   ' __|___';
133700091222         ENDIF;
133800091222         IF  $Holidays(05) = '1';
133900091222           V2cRiga = %trim(V2cRiga) + Pink +
134000091222                   $Name_Day(05)   + ' ' +
134100091222                   %editc($Number_Day(05):'X') + ' ' +
134200091222                   $Name_Month(05) + Green +
134300091222                   '__' ;
134400091222         ELSE;
134500091222           V2cRiga = %trim(V2cRiga) + ' ' +
134600090722                   $Name_Day(05)   + ' ' +
134700090722                   %editc($Number_Day(05):'X') + ' ' +
134800091222                   $Name_Month(05) +
134900091222                   ' __' ;
135000091222         ENDIF;
135100090806
135200090806         //?Imposto i giorni Hidden
135300090806         V2hLun = $Date(01);
135400090806         V2hMar = $Date(02);
135500090806         V2hMer = $Date(03);
135600090806         V2hGio = $Date(04);
135700090806         V2hVen = $Date(05);
135800090806         V2hSab = $Date(06);
135900090806         V2hDom = $Date(07);
136000090722
136100090722       ENDSR;
136200090722
136300090722       //--------------------------------------------------------------
136400090806       //?Carico le attività di 7 gg.
136500090722       //--------------------------------------------------------------
136600090805       BEGSR sr_CarAttivita;
136700090805
136800090805         //?Pulisco le skiere
136900090805         clear $DetLunA;
137000090805         clear $DetLunB;
137100090805         clear $DetMarA;
137200090805         clear $DetMarB;
137300090805         clear $DetMerA;
137400090805         clear $DetMerB;
137500090805         clear $DetGioA;
137600090805         clear $DetGioB;
137700090805         clear $DetVenA;
137800090805         clear $DetVenB;
137900090805
138000090805         clear $DetLunAt;
138100090805         clear $DetLunBt;
138200090805         clear $DetMarAt;
138300090805         clear $DetMarBt;
138400090805         clear $DetMerAt;
138500090805         clear $DetMerBt;
138600090805         clear $DetGioAt;
138700090805         clear $DetGioBt;
138800090805         clear $DetVenAt;
138900090805         clear $DetVenBt;
139000090805
139100090805         $End = *off;
139200090805
139300090805         //?Imposto la stringa per SQL
139400090805         exsr  sr_PrepSQL;
139500090805
139600090805         //?Dichiarazione cursore
139700090805         exec sql
139800090805           prepare S1   from :wSQL;
139900090805         exec sql
140000090805           declare ATT  cursor for S1;
140100090805
140200090805         //?Apertura del cursore
140300090805         exec sql
140400090805           open ATT;
140500090722
140600090805         IF sqlcode < 0;
140700090805           $End = *on;
140800090805         ENDIF;
140900090722
141000090805         DOW not $End;
141100090722           exec sql
141200090805             fetch next from ATT into :atctat, :atccpo, :atcksc,
141300100305                                      :atcdad, :atcoii, :atcofi,
141400100525                                      :atchda, :atcnrv, :atccad,
141500100525                                      :atccac, :atcest;
141600090805           IF sqlcod = 100 or sqlcod < 0;
141700090805             $End = *on;
141800090805             leave;
141900090805           ENDIF;
142000090805
142100090805           IF atcdad <> sav_atcdad;
142200090805             clear wDetA;
142300090805             clear wDetAt;
142400090805             clear wDetB;
142500090805             clear wDetBt;
142600090805             sav_atcdad = atcdad;
142700090805           ENDIF;
142800100305
142900100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
143000100305           exsr sr_CtrRcdOk;
143100100305           IF  not $RcdOk;
143200100305             iter;
143300100305           ENDIF;
143400090805
143500090805         //?Carico i dati a dettaglio
143600090805           exsr sr_CarDati;
143700090805
143800090722         ENDDO;
143900090722
144000090805         exec sql
144100090805           close ATT;
144200090722
144300090722       ENDSR;
144400090805
144500090805       //--------------------------------------------------------------
144600090805       //?Imposto i nomi del giorno e del mese.
144700090805       //--------------------------------------------------------------
144800090805       BEGSR Set_Name;
144900090805
145000090805         //?Recupero il nome del giorno
145100090805         clear wName_Of_Day;
145200090805         exec sql
145300090805           set :wName_Of_Day = dayname(:wDate_ISO);
145400090805
145500090805         //?Recupero il nome del mese
145600090805         clear wName_Of_Month;
145700090805         exec sql
145800090805           set :wName_Of_Month = monthname(:wDate_ISO);
145900090805
146000090805       ENDSR;
146100091222
146200091222       //--------------------------------------------------------------
146300091222       //?Verifico se giorno di festa.
146400091222       //--------------------------------------------------------------
146500091222       BEGSR Calendario;
146600091222
146700091222         $Holidays(xx) = '0';
146800091222
146900091229         clear k_clntfa;
147000091229         k_clntfp = %dec(%subst(V2cage:1:3):3:0);
147100091222         k_clnann = %subdt(wDate_ISO:*years);
147200091222         k_clnmes = %subdt(wDate_ISO:*months);
147300091222         chain %kds(K04azcln) AZCLN01L;
147400091222         IF  %found(AZCLN01L);
147500091222           IF  mat($Number_Day(xx)) = 'F'  or
147600091222               pom($Number_Day(xx)) = 'F';
147700091222             $Holidays(xx) = '1';
147800091222           ENDIF;
147900091222         ENDIF;
148000091222
148100091222       ENDSR;
148200090805
148300090805       //--------------------------------------------------------------
148400090805       //?Preparazione stringa SQL.
148500090805       //--------------------------------------------------------------
148600090805       BEGSR sr_PrepSQL;
148700090805
148800090805         wSQL = 'select atctat, atccpo, atcksc, atcdad, atcoii, atcofi, +
148900100525                 atchda, atcnrv, atccad, atccac, atcest +
149000100525                  from TIATC00F +
149100090805                  where atccmm in(';
149200090805
149300090806         yy = 0;
149400090805         xx = 1;
149500090805         FOR xx by 1 to %elem($Age);
149600090805           IF $Age(xx) <> *zeros;
149700090806             IF yy > 0;
149800090806               wSQL += ', ';
149900090806             ELSE;
150000090806               yy = 1;
150100090806             ENDIF;
150200090806             wSQL += %editc($Age(xx):'X');
150300090805           ENDIF;
150400090805         ENDFOR;
150500090805
150600090805         wSQL += ') and atcdad between ' + %editc($Date(01):'X') +
150700090805                 ' and ' + %editc($Date(07):'X');
150800090805
150900091111         IF IMK82NoAtt <> *blanks;
151000090806         wSQL += ' and atctat not in(''';
151100090806           yy = 0;
151200090805           xx = 1;
151300090805           FOR xx by 1 to %elem($NoAtt);
151400090806             IF $NoAtt(xx) <> *blanks;
151500090806               IF yy > 0;
151600090806                 wSQL += ', ';
151700090806               ELSE;
151800090806                 yy = 1;
151900090806               ENDIF;
152000090806               wSQL +=  $NoAtt(xx) + '''';
152100090805             ENDIF;
152200090805           ENDFOR;
152300090805         wSQL += ')';
152400090805         ENDIF;
152500090805
152600090806         wSQL += ' order by atcdad, atctat +
152700090805                   for fetch only';
152800090805
152900090805       ENDSR;
153000100305
153100100305       //--------------------------------------------------------------
153200100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
153300100305       //--------------------------------------------------------------
153400100305       BEGSR Sr_CtrRcdOk;
153500100305
153600100305         $RcdOk = *on;
153700100416
153800100305         //?se attività legata a trattativa
153900100305         //?la trattativa NON deve essere fittizia
154000100305         IF  atcnrv > 0;
154100110624             exec sql
154200110803             select visffz into :visffz from tivis05l
154300110624             where visnrv = : atcnrv;
154400100305         //?non trovo il rcd corrispondente
154500100305           IF  sqlcod <> 0;
154600100305             leavesr;
154700100305           ENDIF;
154800100305         //?se trattativa fittizia non carico a video
154900100305           IF  visffz = 'S';
155000100305             $RcdOk = *off;
155100100305           ENDIF;
155200100305         ENDIF;
155300100416
155400100416         //?CHIODO
155500100705         //?MODIFICA CONTROLLO 05 Luglio 2010 ora controllo solo esito senza causale
155600100705         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
155700100705         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
155800100705           $RcdOk = *off;
155900100705         ENDIF;
156000100416         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
156100100416         //?non devo caricare in agenda
156200100705         // IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
156300100705         //   $RcdOk = *off;
156400100705         // ENDIF;
156500100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
156600100525         //?non devo caricare in agenda
156700100705         // IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
156800100705         //   $RcdOk = *off;
156900100705         // ENDIF;
157000100705         //?CHIODO
157100100521         //?Se attvità qualsiasi chiusa con attività 'AA'
157200100521         //?non devo caricare in agenda
157300100521         IF  ATCcac = 'AA';
157400100521           $RcdOk = *off;
157500100521         ENDIF;
157600100305
157700100305       ENDSR;
157800090709
157900090709       //--------------------------------------------------------------
158000090807       //?Carico i dati a video per i gg della settimana.
158100090709       //--------------------------------------------------------------
158200090805       BEGSR Sr_CarDati;
158300090806
158400090807         $Week = *off;
158500090709
158600090805         //?descrizione
158700090709         clear wDesCpo;
158800090804         SELECT;
158900090805         //?del cliente
159000090804           WHEN  atcksc <> *zeros;
159100090804             k_acoksc = atcksc;
159200090804             chain  %kds(k03cnaco) CNACO00F;
159300090804             wDesCpo = acorag;
159400090805         //?del potenziale
159500090804           WHEN  atccpo <> 0;
159600090804             chain  atccpo tncpo01l;
159700090804             wDesCpo = cporag;
159800090805           ENDSL;
159900090805         //?imposto fisso ferie
160000090805         IF atctat = 'F';
160100090722           wDesCpo = 'Ferie';
160200090805         ENDIF;
160300091123         //?imposto fisso ufficio
160400091123         IF atctat = 'U';
160500091123           wDesCpo = 'Ufficio';
160600091123         ENDIF;
160700090709
160800090805         //?ora inizio
160900090807         IF  atcoii = 0;
161000090807           watcoii = atchda;
161100090807         ELSE;
161200090807           watcoii = atcoii * 100;
161300090807         ENDIF;
161400090721         wHour_ISO_I = %time(watcoii);
161500100504         //?tengo buone solo le ore
161600100504         IF  not $Minute30;
161700100504           wHour = %subdt(wHour_ISO_I:*hours);
161800100504           watcoii = wHour * 10000;
161900100504         ENDIF;
162000090721
162100090805         //?ora fine
162200090723         IF  atcofi = 0;
162300090723           watcofi = watcoii;
162400090723         ELSE;
162500090723           watcofi = atcofi * 100;
162600090723         ENDIF;
162700090721         wHour_ISO_F = %time(watcofi);
162800100504         //?controllo se ho dei minuti
162900100504         IF  not $Minute30;
163000100504           wMin = %subdt(wHour_ISO_F:*minutes);
163100100504         ENDIF;
163200090709
163300090805         //?calcolo quante ore ho impegnate
163400090807         IF  not $Minute30;
163500100504           IF not $Hour2;
163600100504             wTot_M = %diff(wHour_ISO_F : wHour_ISO_I : *minutes);
163700100504             wTot_H = %div(wTot_M : 60);
163800100504             wTot_R = %rem(wTot_M : 60);
163900100504           //?ho del resto dal calcolo delle ore aggiungo 1 alle ore
164000100504           //?ho dei minuti nell'ora di fine impegno aggiungo 1 alle ore
164100100504             IF  wTot_R > 0 or wMin > 0;
164200100504               wTot_H += 1;
164300100504             ENDIF;
164400100504           ENDIF;
164500100504           IF $Hour2;
164600100504             wTot_H = %diff(wHour_ISO_F : wHour_ISO_I : *hours);
164700090807             wTot_H = wTot_H / 2;
164800090807           ENDIF;
164900090807         ENDIF;
165000090807         IF  $Minute30;
165100090807           wTot_H = %diff(wHour_ISO_F : wHour_ISO_I : *minutes);
165200090807           wTot_H = wTot_H / 30;
165300090807         ENDIF;
165400090805
165500090805         //?carico il dettaglio per giorno
165600090805         xx = %lookup(watcoii : $Hour);
165700090805         //?carico la prima ora di impegno
165800090805         IF xx > 0;
165900090805           zz = xx;
166000090805           exsr sr_CarDetGio;
166100090805         //?imposto le successive ore di impegno
166200090805           IF wTot_H > 1;
166300100504             wTot_H -= 1;
166400090805             yy = 1;
166500090805               FOR yy by 1 to wTot_H;
166600090805                 zz = yy+xx;
166700090805                 exsr sr_CarDetGio;
166800090805               ENDFOR;
166900090805           ENDIF;
167000090805         ENDIF;
167100090807         //?Se visualizzo ogni 2 ore e non trovo l'ora corrispondende nella sk
167200090807         //?provo sommando 1 ora all'ora inizio impegno
167300090807         IF xx = 0;
167400090807           wHour_ISO = %time(watcoii);
167500090807           wHour_ISO = wHour_ISO + %hours(1);
167600090807           watcoii = %dec(wHour_ISO);
167700090807           xx = %lookup(watcoii : $Hour);
167800090807         //?carico la prima ora di impegno
167900090807           IF xx > 0;
168000090807             zz = xx;
168100090807             exsr sr_CarDetGio;
168200090807         //?imposto le successive ore di impegno
168300090807             IF wTot_H > 1;
168400090807               yy = 1;
168500090807                 FOR yy by 1 to wTot_H;
168600090807                   zz = yy+xx;
168700090807                   exsr sr_CarDetGio;
168800090807                 ENDFOR;
168900090807             ENDIF;
169000090807           ENDIF;
169100090807         ENDIF;
169200090807         //?Imposto se c'è impegno nel week-end
169300090807         IF  atcdad = V2hSab or
169400090807            atcdad = V2hDom;
169500090807            $Week = *on;
169600090807         ENDIF;
169700090709
169800090709       ENDSR;
169900090805
170000090805       //--------------------------------------------------------------
170100090805       //?Carico il dettaglio per giorno.
170200090805       //--------------------------------------------------------------
170300090805       BEGSR Sr_CarDetGio;
170400090805
170500090805         SELECT;
170600090805           WHEN zz > 1 and
170700090805              wDetA(zz) = *blanks and
170800100416              %scan(%subst(wDetB(zz-1):3:7):%subst(wDesCpo:1:7)) > 0;
170900090805              wDetB(zz)  = '* ' + wDesCpo;
171000090805              wDetBt(zz) = atctat;
171100090805           WHEN wDetA(zz)  = *blanks;
171200090805              wDetA(zz)  = wDesCpo;
171300090805              wDetAt(zz) = atctat;
171400090805           OTHER;
171500090805           IF wDetB(zz) = *blanks;
171600090805              wDetB(zz)  = '* ' + wDesCpo;
171700090805              wDetBt(zz) = atctat;
171800090805           ELSE;
171900090805              wDetB(zz)  = c_AltreA;
172000091125              wDetBt(zz) = c_AttAltre;
172100090805           ENDIF;
172200090805         ENDSL;
172300090806
172400090805         SELECT;
172500090805         //?Lunedì
172600090806           WHEN  atcdad = V2hLun;
172700090805             $DetLunA(zz)  = wDetA(zz);
172800090805             $DetLunB(zz)  = wDetB(zz);
172900090805             $DetLunAt(zz) = wDetAt(zz);
173000090805             $DetLunBt(zz) = wDetBt(zz);
173100090805         //?Martedì
173200090806           WHEN  atcdad = V2hMar;
173300090805             $DetMarA(zz)  = wDetA(zz);
173400090805             $DetMarB(zz)  = wDetB(zz);
173500090805             $DetMarAt(zz) = wDetAt(zz);
173600090805             $DetMarBt(zz) = wDetBt(zz);
173700090805         //?Mercoledì
173800090806           WHEN  atcdad = V2hMer;
173900090805             $DetMerA(zz)  = wDetA(zz);
174000090805             $DetMerB(zz)  = wDetB(zz);
174100090805             $DetMerAt(zz) = wDetAt(zz);
174200090805             $DetMerBt(zz) = wDetBt(zz);
174300090805         //?Giovedì
174400090806           WHEN  atcdad = V2hGio;
174500090805             $DetGioA(zz)  = wDetA(zz);
174600090805             $DetGioB(zz)  = wDetB(zz);
174700090805             $DetGioAt(zz) = wDetAt(zz);
174800090805             $DetGioBt(zz) = wDetBt(zz);
174900090805         //?Venerdì
175000090806           WHEN  atcdad = V2hVen;
175100090805             $DetVenA(zz)  = wDetA(zz);
175200090805             $DetVenB(zz)  = wDetB(zz);
175300090805             $DetVenAt(zz) = wDetAt(zz);
175400090805             $DetVenBt(zz) = wDetBt(zz);
175500090805         ENDSL;
175600090805
175700090805       ENDSR;
175800090722
175900090722       //--------------------------------------------------------------
176000090805       //?Pulisco subfile.
176100090722       //--------------------------------------------------------------
176200090722       BEGSR sr_PulSfl;
176300090722
176400090722         SflDsp_N    = *on;
176500090722         SflDspCtl_N = *on;
176600090722         write  MK82C02;
176700090722         SflDspCtl_N = *off;
176800090722         SflEnd      = *off;
176900090722
177000090722         clear V2hRcd;
177100090722         clear V2Dmsg;
177200090722         errMessage  = *off;
177300090722         errGenerico = *off;
177400090722
177500090722       ENDSR;
177600090722
177700090722       //--------------------------------------------------------------
177800090807       //?Carico il subfile.
177900090722       //--------------------------------------------------------------
178000090807       BEGSR sr_CarS02;
178100090722
178200090722         clear S02Nrr;
178300090807         clear yy;
178400090807
178500090807         FOR xx by 1 to %elem($Hour);
178600090807
178700090807           IF yy = 10;
178800090807             leave;
178900090807           ENDIF;
179000090807
179100090807           yy += 1;
179200090723
179300090723           V2Ora = %dec(%subst(%editc($Hour(xx):'X'):1:4):4:0);
179400090723           V2LunA  = $DetLunA(xx);
179500090723           V2LunB  = $DetLunB(xx);
179600090723           V2hLunA = $DetLunAt(xx);
179700090723           V2hLunB = $DetLunBt(xx);
179800090723           V2MarA  = $DetMarA(xx);
179900090723           V2MarB  = $DetMarB(xx);
180000090723           V2hMarA = $DetMarAt(xx);
180100090723           V2hMarB = $DetMarBt(xx);
180200090723           V2MerA  = $DetMerA(xx);
180300090723           V2MerB  = $DetMerB(xx);
180400090723           V2hMerA = $DetMerAt(xx);
180500090723           V2hMerB = $DetMerBt(xx);
180600090723           V2GioA  = $DetGioA(xx);
180700090723           V2GioB  = $DetGioB(xx);
180800090723           V2hGioA = $DetGioAt(xx);
180900090723           V2hGioB = $DetGioBt(xx);
181000090723           V2VenA  = $DetVenA(xx);
181100090723           V2VenB  = $DetVenB(xx);
181200090723           V2hVenA = $DetVenAt(xx);
181300090723           V2hVenB = $DetVenBt(xx);
181400090723
181500090723           v2sriga = '___________|' +
181600090723                     '_______________________|' +
181700090723                     '_______________________|' +
181800090723                     '_______________________|' +
181900090723                     '_______________________|' +
182000090723                     '________________________';
182100090723
182200091123         //?Imposto i colori delle attività
182300091123         //?LUNEDI
182400091123           SELECT;
182500091123             WHEN V2hLunA = 'T';
182600091123               ColorLunA = Blue;
182700091123             WHEN V2hLunA = 'A';
182800091123               ColorLunA = Red;
182900091123             WHEN V2hLunA = 'F';
183000091123               ColorLunA = Pink;
183100091123             WHEN V2hLunA = 'O';
183200091123               ColorLunA = White;
183300091123             WHEN V2hLunA = 'U';
183400091125               ColorLunA = Green;
183500091125             WHEN V2hLunA = 'W';
183600091125               ColorLunA = Turquoise;
183700091123             OTHER;
183800091125               ColorLunA = Green;
183900091123           ENDSL;
184000091123           SELECT;
184100091123             WHEN V2hLunB = 'T';
184200091123               ColorLunB = Blue;
184300091123             WHEN V2hLunB = 'A';
184400091123               ColorLunB = Red;
184500091123             WHEN V2hLunB = 'F';
184600091123               ColorLunB = Pink;
184700091123             WHEN V2hLunB = 'O';
184800091123               ColorLunB = White;
184900091123             WHEN V2hLunB = 'U';
185000091125               ColorLunB = Green;
185100091125             WHEN V2hLunB = 'W';
185200091125               ColorLunB = Turquoise;
185300091123             OTHER;
185400091125               ColorLunB = Green;
185500091123           ENDSL;
185600091123         //?MARTEDI
185700091123           SELECT;
185800091123             WHEN V2hMarA = 'T';
185900091123               ColorMarA = Blue;
186000091123             WHEN V2hMarA = 'A';
186100091123               ColorMarA = Red;
186200091123             WHEN V2hMarA = 'F';
186300091123               ColorMarA = Pink;
186400091123             WHEN V2hMarA = 'O';
186500091123               ColorMarA = White;
186600091123             WHEN V2hMarA = 'U';
186700091125               ColorMarA = Green;
186800091125             WHEN V2hMarA = 'W';
186900091125               ColorMarA = Turquoise;
187000091123             OTHER;
187100091125               ColorMarA = Green;
187200091123           ENDSL;
187300091123           SELECT;
187400091123             WHEN V2hMarB = 'T';
187500091123               ColorMarB = Blue;
187600091123             WHEN V2hMarB = 'A';
187700091123               ColorMarB = Red;
187800091123             WHEN V2hMarB = 'F';
187900091123               ColorMarB = Pink;
188000091123             WHEN V2hMarB = 'O';
188100091123               ColorMarB = White;
188200091123             WHEN V2hMarB = 'U';
188300091125               ColorMarB = Green;
188400091125             WHEN V2hMarB = 'W';
188500091125               ColorMarB = Turquoise;
188600091123             OTHER;
188700091125               ColorMarB = Green;
188800091123           ENDSL;
188900091123         //?MERCOLEDI
189000091123           SELECT;
189100091123             WHEN V2hMerA = 'T';
189200091123               ColorMerA = Blue;
189300091123             WHEN V2hMerA = 'A';
189400091123               ColorMerA = Red;
189500091123             WHEN V2hMerA = 'F';
189600091123               ColorMerA = Pink;
189700091123             WHEN V2hMerA = 'O';
189800091123               ColorMerA = White;
189900091123             WHEN V2hMerA = 'U';
190000091125               ColorMerA = Green;
190100091125             WHEN V2hMerA = 'W';
190200091125               ColorMerA = Turquoise;
190300091123             OTHER;
190400091125               ColorMerA = Green;
190500091123           ENDSL;
190600091123           SELECT;
190700091123             WHEN V2hMerB = 'T';
190800091123               ColorMerB = Blue;
190900091123             WHEN V2hMerB = 'A';
191000091123               ColorMerB = Red;
191100091123             WHEN V2hMerB = 'F';
191200091123               ColorMerB = Pink;
191300091123             WHEN V2hMerB = 'O';
191400091123               ColorMerB = White;
191500091123             WHEN V2hMerB = 'U';
191600091125               ColorMerB = Green;
191700091125             WHEN V2hMerB = 'W';
191800091125               ColorMerB = Turquoise;
191900091123             OTHER;
192000091125               ColorMerB = Green;
192100091123           ENDSL;
192200091123         //?GIOVEDI
192300091123           SELECT;
192400091123             WHEN V2hGioA = 'T';
192500091123               ColorGioA = Blue;
192600091123             WHEN V2hGioA = 'A';
192700091123               ColorGioA = Red;
192800091123             WHEN V2hGioA = 'F';
192900091123               ColorGioA = Pink;
193000091123             WHEN V2hGioA = 'O';
193100091123               ColorGioA = White;
193200091123             WHEN V2hGioA = 'U';
193300091125               ColorGioA = Green;
193400091125             WHEN V2hGioA = 'W';
193500091125               ColorGioA = Turquoise;
193600091123             OTHER;
193700091125               ColorGioA = Green;
193800091123           ENDSL;
193900091123           SELECT;
194000091123             WHEN V2hGioB = 'T';
194100091123               ColorGioB = Blue;
194200091123             WHEN V2hGioB = 'A';
194300091123               ColorGioB = Red;
194400091123             WHEN V2hGioB = 'F';
194500091123               ColorGioB = Pink;
194600091123             WHEN V2hGioB = 'O';
194700091123               ColorGioB = White;
194800091123             WHEN V2hGioB = 'U';
194900091125               ColorGioB = Green;
195000091125             WHEN V2hGioB = 'W';
195100091125               ColorGioB = Turquoise;
195200091123             OTHER;
195300091125               ColorGioB = Green;
195400091123           ENDSL;
195500091123         //?VENERDI
195600091123           SELECT;
195700091123             WHEN V2hVenA = 'T';
195800091123               ColorVenA = Blue;
195900091123             WHEN V2hVenA = 'A';
196000091123               ColorVenA = Red;
196100091123             WHEN V2hVenA = 'F';
196200091123               ColorVenA = Pink;
196300091123             WHEN V2hVenA = 'O';
196400091123               ColorVenA = White;
196500091123             WHEN V2hVenA = 'U';
196600091125               ColorVenA = Green;
196700091125             WHEN V2hVenA = 'W';
196800091125               ColorVenA = Turquoise;
196900091123             OTHER;
197000091125               ColorVenA = Green;
197100091123           ENDSL;
197200091123           SELECT;
197300091123             WHEN V2hVenB = 'T';
197400091123               ColorVenB = Blue;
197500091123             WHEN V2hVenB = 'A';
197600091123               ColorVenB = Red;
197700091123             WHEN V2hVenB = 'F';
197800091123               ColorVenB = Pink;
197900091123             WHEN V2hVenB = 'O';
198000091123               ColorVenB = White;
198100091123             WHEN V2hVenB = 'U';
198200091125               ColorVenB = Green;
198300091125             WHEN V2hVenB = 'W';
198400091125               ColorVenB = Turquoise;
198500091123             OTHER;
198600091125               ColorVenB = Green;
198700091123           ENDSL;
198800090723
198900090722           S02Nrr += 1;
199000090722           write MK82S02;
199100090722
199200090722         ENDFOR;
199300090807
199400090807         //?Indicatore di fine file
199500090807         SELECT;
199600090807           WHEN $Hour1 and sav_xx = (wxxh1 +6);
199700090807             SflEnd = *on;
199800090807           WHEN $Hour2 and sav_xx = wxxh2;
199900090807             SflEnd = *on;
200000090807           WHEN $Minute30 and sav_xx > 38;
200100090807             SflEnd = *on;
200200090807         ENDSL;
200300090722
200400090722       ENDSR;
200500090724
200600090724       //--------------------------------------------------------------
200700090805       //?Gestione tasto funzionale F1 da videata S02.
200800090724       //?F1=Help Legenda Colori
200900090724       //--------------------------------------------------------------
201000090724       BEGSR sr_F01S02;
201100090724
201200090724        DOW  dsp_aid <> c_F12;
201300090724           exfmt MK82W02;
201400090724        ENDDO;
201500090724
201600090724       ENDSR;
201700090722
201800090722       //--------------------------------------------------------------
201900090805       //?Gestione tasto funzionale F4 da videata S02.
202000090723       //?F4=Settimana Precedente
202100090722       //--------------------------------------------------------------
202200090723       BEGSR sr_F04S02;
202300090722
202400090722         //?Imposto la data inizio settimana
202500090722         wDate_ISO= %date(V2hLun);
202600090722         wDate_ISO_Par= wDate_ISO - %days(7);
202700090722
202800090722         $video = 'S2';
202900090722         $inzS02 = *on;
203000090722         wdata = %dec(wDate_ISO_Par);
203100091111         IMK82Data = wdata;
203200091111         IMK82cAGE = %dec(V2cAGE:7:0);
203300090722
203400090722       ENDSR;
203500090722
203600090722       //--------------------------------------------------------------
203700090805       //?Gestione tasto funzionale F5 da videata S02.
203800090724       //?F5=Settimana Successiva
203900090722       //--------------------------------------------------------------
204000090723       BEGSR sr_F05S02;
204100090722
204200090722         //?Imposto la data inizio settimana
204300090722         wDate_ISO= %date(V2hLun);
204400090722         wDate_ISO_Par= wDate_ISO + %days(7);
204500090722
204600090722         $video = 'S2';
204700090722         $inzS02 = *on;
204800090722         wdata = %dec(wDate_ISO_Par);
204900091111         IMK82Data = wdata;
205000091111         IMK82cAGE = %dec(V2cAGE:7:0);
205100090722
205200090722       ENDSR;
205300100308
205400100308       //--------------------------------------------------------------
205500100308       //?Gestione tasto funzionale F8 da videata S02.
205600100308       //?F8=Stamp
205700100308       //--------------------------------------------------------------
205800100308       BEGSR sr_F08S02;
205900100308
206000100308         clear trmk86ds;
206100100308         IMK86cmm   = %dec(V2cAge:7:0);
206200100308         IMK86datai = V2hLun;
206300100308         IMK86dataf = V2hDom;
206400100308         trmk86r (kpjba:trmk86ds);
206500100308
206600100308       ENDSR;
206700090724
206800090724       //--------------------------------------------------------------
206900090805       //?Gestione tasto funzionale F9 da videata S02.
207000090724       //?F9=Dettaglio
207100090724       //--------------------------------------------------------------
207200090724       BEGSR sr_F09S02;
207300090724
207400090724         $Riga = dsp_rig;
207500090724         $Colo = dsp_col;
207600090724
207700090724         wRiga = ds_riga;
207800090724         wColo = ds_colo;
207900090806
208000090806         clear trmk83ds;
208100100120         IMK83rich = IMK82rich;
208200091105         imk83cmm = %dec(V2cAge:7:0);
208300090724
208400090724         //?in base a dove sono posizionata richiamo pgm int. attività
208500090724         SELECT;
208600090724
208700090724           WHEN  wColo >  13 and wColo <  37;
208800090806             imk83dati = V2hLun;
208900090724           WHEN  wColo >  37 and wColo <  61;
209000090806             imk83dati = V2hMar;
209100090724           WHEN  wColo >  61 and wColo <  85;
209200090806             imk83dati = V2hMer;
209300090724           WHEN  wColo >  85 and wColo <  109;
209400090806             imk83dati = V2hGio;
209500090724           WHEN  wColo > 109 and wColo <= 132;
209600090806             imk83dati = V2hVen;
209700090724           OTHER;
209800090724             ErrMessage   = *on;
209900090724             ErrGenerico  = *on;
210000090724             V2Dmsg       = $Msg(06);
210100090724             leavesr;
210200090724         ENDSL;
210300090724
210400090724         IF  wRiga < 5 or wRiga > 25;
210500090724           ErrMessage   = *on;
210600090724           ErrGenerico  = *on;
210700090724           V2Dmsg       = $Msg(06);
210800090724           leavesr;
210900090724         ENDIF;
211000090806
211100090806         imk83datf = imk83dati;
211200090806
211300091111         trmk83r (kpjba:trmk83ds);
211400100112
211500100112         //?ricarico la videata
211600100112         $InzS02 = *on;
211700090724
211800090724       ENDSR;
211900091217
212000091217       //--------------------------------------------------------------
212100091217       //?Gestione tasto funzionale F10 da videata S02.
212200091217       //?F10=Attività
212300091217       //--------------------------------------------------------------
212400091217       BEGSR sr_F10S02;
212500091217
212600091228         clear trmk17ds;
212700091229         IMK17cmm = %dec(V2cAge:7:0);
212800091228
212900091228         trmk17r (kpjba:trmk17ds);
213000091229
213100091229         //?ricarico la videata
213200091229         $InzS02 = *on;
213300091217
213400091217       ENDSR;
213500090806
213600090806       //--------------------------------------------------------------
213700091217       //?Gestione tasto funzionale F11 da videata S02.
213800091217       //?F11=Week-End
213900090806       //--------------------------------------------------------------
214000091217       BEGSR sr_F11S02;
214100090806
214200090806         clear trmk83ds;
214300090806         imk83dati = V2hSab;
214400090806         imk83datf = V2hDom;
214500091105         imk83cmm = %dec(V2cAge:7:0);
214600090806
214700091111         trmk83r (kpjba:trmk83ds);
214800090806
214900090806       ENDSR;
215000090722
215100091111       //--------------------------------------------------------------
215200091111       //?Gestione tasto funzionale F12 da videata S02.
215300091111       //?F12=Ritorno
215400091111       //--------------------------------------------------------------
215500091111       BEGSR sr_F12S02;
215600091111
215700091111         //?Se richiamato torno al chiamante
215800091111         IF  $Richiamato;
215900091111         //?Chiusura del programma
216000091111         $Fine = *on;
216100091111
216200091111         //?Se NON richiamato torno alla prima videata
216300091111         ELSE;
216400091111           $video = 'D1';
216500091111           $inzD01 = *on;
216600100120           clear V2cage;
216700100120           clear V2data;
216800091111         ENDIF;
216900091111
217000091111       ENDSR;
217100091111
217200090722       //--------------------------------------------------------------
217300090805       //?Gestione tasto funzionale RollUp videata S02.
217400090722       //--------------------------------------------------------------
217500090722       BEGSR sr_RollUpS02;
217600090722
217700090722         //?Carico le 10 ore successive
217800090722         SELECT;
217900090722
218000090807           WHEN  sav_xx = 1;
218100090807             exsr sr_PulSfl;
218200090807             IF $Hour1;
218300090807               xx = wxxh1;
218400090807             ENDIF;
218500090807             IF $Hour2;
218600090807               xx = wxxh2;
218700090807             ENDIF;
218800090807             IF $Minute30;
218900090807               xx = wxxm - 8;
219000090807             ENDIF;
219100090807             sav_xx = xx;
219200090807             exsr sr_CarS02;
219300090807           WHEN  $Hour1 and sav_xx = wxxh1;
219400090805             exsr sr_PulSfl;
219500090807             xx = wxxh1 + 6;
219600090807             sav_xx = xx;
219700090807             exsr sr_CarS02;
219800090807           WHEN  $Hour1 and sav_xx > wxxh1;
219900090807             V2Dmsg = $msg(05);
220000090807             ErrMessage  = *on;
220100090807             ErrGenerico = *on;
220200090807           WHEN  $Hour2 and sav_xx = wxxh2;
220300090722             V2Dmsg = $msg(05);
220400090722             ErrMessage  = *on;
220500090722             ErrGenerico = *on;
220600090807           WHEN  $Minute30 and sav_xx = wxxm;
220700090807             exsr sr_PulSfl;
220800090807             xx = wxxm + 8;
220900090807             sav_xx = xx;
221000090807             exsr sr_CarS02;
221100090807           WHEN  $Minute30 and sav_xx = (wxxm +8);
221200090807             exsr sr_PulSfl;
221300090807             xx = sav_xx + 8;
221400090807             sav_xx = xx;
221500090807             exsr sr_CarS02;
221600090807           WHEN  $Minute30 and sav_xx < wxxm;
221700090807             exsr sr_PulSfl;
221800090807             xx = sav_xx + 8;
221900090807             sav_xx = xx;
222000090807             exsr sr_CarS02;
222100090807           WHEN  $Minute30 and sav_xx > 38;
222200090807             V2Dmsg = $msg(05);
222300090807             ErrMessage  = *on;
222400090807             ErrGenerico = *on;
222500090807           WHEN  $Minute30 and sav_xx > (wxxm +8);
222600090807             exsr sr_PulSfl;
222700090807             xx = sav_xx + 8;
222800090807             sav_xx = xx;
222900090807             exsr sr_CarS02;
223000090722
223100090722         ENDSL;
223200090722
223300090722       ENDSR;
223400090722
223500090722       //--------------------------------------------------------------
223600090805       //?Gestione tasto funzionale RollDown videata S02.
223700090722       //--------------------------------------------------------------
223800090722       BEGSR sr_RollDownS02;
223900090722
224000090722         //?Carico le 10 ore precedenti
224100090722         SELECT;
224200090722
224300090807           WHEN  sav_xx = 1;
224400090807             V2Dmsg = $msg(05);
224500090807             ErrMessage  = *on;
224600090807             ErrGenerico = *on;
224700090807           WHEN  ($Hour1 and sav_xx = wxxh1) or
224800090807                 ($Hour2 and sav_xx = wxxh2);
224900090807             exsr sr_PulSfl;
225000090807             xx = 1;
225100090807             sav_xx = xx;
225200090807             exsr sr_CarS02;
225300090807           WHEN  $Hour1 and sav_xx > wxxh1;
225400090807             exsr sr_PulSfl;
225500090807             xx = wxxh1;
225600090807             sav_xx = xx;
225700090807             exsr sr_CarS02;
225800090807           WHEN  $Hour2 and sav_xx > wxxh2;
225900090807             exsr sr_PulSfl;
226000090807             xx = wxxh2;
226100090807             sav_xx = xx;
226200090807             exsr sr_CarS02;
226300090807           WHEN  $Minute30 and sav_xx = wxxm;
226400090807             exsr sr_PulSfl;
226500090807             xx = wxxm - 8;
226600090807             sav_xx = xx;
226700090807             exsr sr_CarS02;
226800090807           WHEN  $Minute30 and sav_xx < wxxm;
226900090807             exsr sr_PulSfl;
227000090807             xx = 1;
227100090807             sav_xx = xx;
227200090807             exsr sr_CarS02;
227300090807           WHEN  $Minute30 and sav_xx > wxxm;
227400090807             exsr sr_PulSfl;
227500090807             xx = sav_xx - 8;
227600090807             sav_xx = xx;
227700090807             exsr sr_CarS02;
227800090722
227900090722         ENDSL;
228000090722
228100090722       ENDSR;
228200090708
228300090708       //--------------------------------------------------------------
228400090805       //?Controllo videata S02.
228500090708       //--------------------------------------------------------------
228600090722       BEGSR sr_CtrS02;
228700090708
228800090708         WindDspF = IndDspF;
228900090708         reset WindDspFb;
229000090708         IndDspF  = WindDspF;
229100090708
229200090708         //?Controllo data
229300090805         IF   V2Data    = *zeros;
229400090805           ErrMessage   = *on;
229500090805           ErrGenerico  = *on;
229600090805           PosCurDATA   = *on;
229700090805           V2Dmsg       = $Msg(01);
229800090805           leavesr;
229900090805         ENDIF;
230000090708
230100090805         clear wlbdat;
230200090805         g02dat = V2Data;
230300090805         xsrda8(wlbdat);
230400090805         IF   g02err    = '1';
230500090805           ErrMessage   = *on;
230600090805           ErrGenerico  = *on;
230700090805           PosCurDATA   = *on;
230800090805           V2Dmsg       = $Msg(02);
230900090805           leavesr;
231000090805         ENDIF;
231100090708
231200090805         V2Data = g02dat;
231300090805         wdata  = g02inv;
231400090708
231500090708         //?Controllo commerciale
231600100308         exsr sr_ctrAGE;
231700100308         IF  ErrGenerico;
231800100308           leavesr;
231900100308         ENDIF;
232000090722
232100090805         $video = 'S2';
232200090805         $inzS02 = *on;
232300091111         IMK82Data = wdata;
232400091111         IMK82cAGE = %dec(V2cAGE:7:0);
232500090810
232600090810         //?Carico la sk agenti
232700090810         exsr sr_CarAGE;
232800090708
232900090708       ENDSR;
233000100308
233100100308       //--------------------------------------------------------------
233200100308       //?Controllo commerciale.
233300100308       //--------------------------------------------------------------
233400100308       BEGSR sr_ctrAGE;
233500100308
233600100308         IF  V2cAGE   =  *zeros or V2cAGE = *blanks;
233700100308           errMessage  = *on;
233800100308           errGenerico = *on;
233900100308           PosCurAGE   = *on;
234000100308           V2Dmsg      = $Msg(03);
234100100308           leavesr;
234200100308         ENDIF;
234300100308
234400100308         IF  %scan('?' : V2cAGE) > 0;
234500100308            errGenerico = *on;
234600100308            PosCurAGE   = *on;
234700100428            tb85Kfil = %editc(DUTpou:'X');
234800100308            clear tb85Key ;
234900100308            clear tb85Des1;
235000100308            trtb85r (kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni);
235100100308            V2cAGE = tb85Key ;
235200100308            V2dAGE = tb85Des1 ;
235300100308         ENDIF;
235400100308
235500100308         IF  %check(digits:V2cAGE) > 0;
235600100308           errMessage  = *on;
235700100308           errGenerico = *on;
235800100308           PosCurAGE   = *on;
235900100308           V2Dmsg      = $Msg(04);
236000100308           leavesr;
236100100308         ENDIF;
236200100428
236300100428         //?Se richiesto '?' sul commerciale ricarico la videata
236400100428         IF  errGenerico;
236500100428           IMK82cage = %dec(V2cAGE:7:0);
236600100428           clear V2cAGE;
236700100428           clear V2dAGE;
236800100428           $InzS02 = *on;
236900100428           //?Carico la sk agenti
237000100428           exsr sr_CarAGE;
237100100428         ENDIF;
237200100308
237300100308         clear ds01;
237400100308         k_tblkut = 1;
237500100308         k_tblcod = '01';
237600100308         k_tblkey = V2cAGE;
237700100308         chain  %kds(K03tabel) TABEL00F;
237800100308         IF  not %found( TABEL00F ) or TBLflg <> *blanks;
237900100308           errMessage  = *on;
238000100308           errGenerico = *on;
238100100308           PosCurAGE   = *on;
238200100308           V2Dmsg      = $Msg(04);
238300100308           leavesr;
238400100308         ENDIF;
238500100308
238600100308         clear TNTAA1DS;
238700100308         ITAA1caut = '§utegtc';
238800100308         ITAA1cmm  = %dec(V2cAGE:7:0);
238900100308         kpjbu     = tntaa1ds;
239000100308         tntaa1c (kpjba);
239100100308         TNTAA1DS = kpjbu;
239200100308         IF  OTAA1fabi = 'N';
239300100308           ErrMessage  = *on;
239400100308           ErrGenerico = *on;
239500100308           PosCurAGE   = *on;
239600100308           V2Dmsg      = $Msg(04);
239700100308           leavesr;
239800100308         ENDIF;
239900100308
240000100308         ds01 = TBLuni;
240100100308         V2dAGE = §01age;
240200110518
240300110518         IF  §01par <> *blanks;
240400110518           ErrMessage  = *on;
240500110518           ErrGenerico = *on;
240600110518           PosCurAGE   = *on;
240700110518           V2Dmsg      = $Msg(04);
240800110518           leavesr;
240900110518         ENDIF;
241000100308
241100100308         IF  %dec(V2cAGE:7:0) <> §01rgf;
241200100308           errMessage  = *on;
241300100308           errGenerico = *on;
241400100308           PosCurAGE   = *on;
241500100308           V2Dmsg      = $Msg(08);
241600100308           leavesr;
241700100308         ENDIF;
241800100308
241900100308       ENDSR;
242000090708
242100090708       //--------------------------------------------------------------
242200090708       //?Operazioni finali.
242300090708       //--------------------------------------------------------------
242400090708       BEGSR sr_RoutEnd;
242500090708
242600090708         *inLR = *on;
242700090708         return;
242800090708
242900090708       ENDSR;
243000090708
243100090708      /end-free
243200090708
243300090708       //--------------------------------------------------------------
243400090708       //?Schiere a tempo di compilazione.
243500090708       //--------------------------------------------------------------
243600090708
243700090708** - $MSG -------------------------------------------------------------------*
243800090708Immettere la data                                                              01
243900090708Data errata                                                                    02
244000090708Immettere il commerciale                                                       03
244100090708Commerciale errato o non in gestione all'utente                                04
244200090803Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.    05
244300090803Cursore in area protetta dello schermo.                                        06
244400090803Attività da escludere non valida.                                              07
244500090804Richiedere solo commerciali UNIFICANTI!                                        08
244600090810Almeno un'ora deve essere inserita                                             09
244700100202Utente non abilitato alla funzione                                             10
244800110105
