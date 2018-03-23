000100130225       //==============================================================
000200130222       //?TNTE58R * Lancio Simulazione Delta Tariffa/Offerta
000300130225       //==============================================================
000400121025
000500121025       //--------------------------------------------------------------
000600121025       //?Specifiche di controllo.                                     ?
000700121025       //--------------------------------------------------------------
000800121025
000900121025     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000121025
001100121025       //--------------------------------------------------------------
001200121025       //?Dichiarazione file.                                          ?
001300121025       //--------------------------------------------------------------
001400130225
001500130225       // -?File Organigramma
001600130225     fAZORG01L  if   e           k disk
001700121025
001800121025       // -?File Tabella
001900130222     fTABEL00F  if   e           k disk
002000130225
002100130225       // -?File Trattative
002200130225     fTIVIS05L  if   e           k disk
002300130225
002400130225       // -?File Offerte
002500130225     fTIVOF11L  if   e           k disk
002600130225     fTNOFM00L  if   e           k disk    rename(TNTAM000:TNOFM000)
002700130226     f                                     usropn extfile(wFLib)
002800130226     fTIOFD01L  if   e           k disk    rename(TITAD000:TIOFD000)
002900130226     f                                     usropn extfile(wFLib)
003000130225
003100130225       // -?File Tariffe
003200130225     fTNTAM00L  if   e           k disk
003300130226     fTITAD04L  if   e           k disk
003400121025
003500121025       // -?File video
003600130222     fTNTE58D   cf   e             workstn
003700121107     f                                     indds(IndDspF)
003800121107     f                                     infds(InfDspF)
003900121025
004000121025       //--------------------------------------------------------------
004100121025       //?Definizione costanti.                                        ?
004200121025       //--------------------------------------------------------------
004300121025
004400130225       // -?Tasti funzionali a video
004500121024     d c_F01           c                   const(x'31')
004600121024     d c_F02           c                   const(x'32')
004700121024     d c_F03           c                   const(x'33')
004800121024     d c_F04           c                   const(x'34')
004900121024     d c_F05           c                   const(x'35')
005000121024     d c_F06           c                   const(x'36')
005100121024     d c_F07           c                   const(x'37')
005200121024     d c_F08           c                   const(x'38')
005300121024     d c_F09           c                   const(x'39')
005400121024     d c_F10           c                   const(x'3A')
005500121024     d c_F11           c                   const(x'3B')
005600121024     d c_F12           c                   const(x'3C')
005700121024     d c_F13           c                   const(x'B1')
005800121024     d c_F14           c                   const(x'B2')
005900121024     d c_F15           c                   const(x'B3')
006000121024     d c_F16           c                   const(x'B4')
006100121024     d c_F17           c                   const(x'B5')
006200121024     d c_F18           c                   const(x'B6')
006300121024     d c_F19           c                   const(x'B7')
006400121024     d c_F20           c                   const(x'B8')
006500121024     d c_F21           c                   const(x'B9')
006600121024     d c_F22           c                   const(x'BA')
006700121024     d c_F23           c                   const(x'BB')
006800121024     d c_F24           c                   const(x'BC')
006900121024     d c_Enter         c                   const(x'F1')
007000121024     d c_RollDown      c                   const(x'F4')
007100121024     d c_RollUp        c                   const(x'F5')
007200121024
007300121024     d Digits          c                   const('0123456789')
007400121025
007500121025       //--------------------------------------------------------------
007600121025       //?Definizione schiere.                                         ?
007700121025       //--------------------------------------------------------------
007800130225
007900130225       // -?Schiere per gestione codici cliente di ritorno da XCLIR
008000130225     d ksa             s              4    dim(30)
008100130225     d ksc             s              7  0 dim(30)
008200121025
008300121024      // -?Messaggi di errore
008400130225     d skMsg           s             78    dim(30) ctdata perrcd(1)
008500121025
008600121025       //--------------------------------------------------------------
008700121025       //?Definizione aree dati.                                       ?
008800121025       //--------------------------------------------------------------
008900121025
009000121025       // -?Dati utente?
009100121025     d §AzUte        e ds                  extname(AZUTE00F)
009200121025     d                                     dtaara
009300121025     d §DatiUte      e ds                  extname(dDatiUte)
009400121025     d                                     dtaara
009500121025
009600121025       //--------------------------------------------------------------
009700121025       //?Definizione strutture dati.                                  ?
009800121025       //--------------------------------------------------------------
009900121025
010000121025       // -?Status ds?
010100121025     d Status         sds
010200121025     d  SDSpgm           *proc
010300121025     d  SDSusr               254    263
010400121024
010500130225       // -?InfDS
010600121024     d InfDspF         ds
010700121024     d  dsp_aid              369    369a
010800121024
010900130225       // -?Indicatori su DspF
011000121024     d IndDspF         ds
011100130225       // -?Indicatori di Errore
011200121024     d  ErrMessage                    1n   overlay(IndDspF : 28)
011300130222     d  PosCurKsc01                   1n   overlay(IndDspF : 50)
011400130222     d  PosCurKsc02                   1n   overlay(IndDspF : 51)
011500130222     d  PosCurKsc03                   1n   overlay(IndDspF : 52)
011600130222     d  PosCurKsc04                   1n   overlay(IndDspF : 53)
011700130222     d  PosCurKsc05                   1n   overlay(IndDspF : 54)
011800130222     d  PosCurTar01                   1n   overlay(IndDspF : 55)
011900130222     d  PosCurTar02                   1n   overlay(IndDspF : 56)
012000130222     d  PosCurTar03                   1n   overlay(IndDspF : 57)
012100130222     d  PosCurTar04                   1n   overlay(IndDspF : 58)
012200130222     d  PosCurTar05                   1n   overlay(IndDspF : 59)
012300130222     d  PosCurLnp01                   1n   overlay(IndDspF : 60)
012400130222     d  PosCurLnp02                   1n   overlay(IndDspF : 61)
012500130222     d  PosCurLnp03                   1n   overlay(IndDspF : 62)
012600130222     d  PosCurLnp04                   1n   overlay(IndDspF : 63)
012700130222     d  PosCurLnp05                   1n   overlay(IndDspF : 64)
012800130222     d  PosCurDspDal                  1n   overlay(IndDspF : 65)
012900130222     d  PosCurDspAl                   1n   overlay(IndDspF : 66)
013000130222     d  PosCurCts01                   1n   overlay(IndDspF : 67)
013100130222     d  PosCurCts02                   1n   overlay(IndDspF : 68)
013200130222     d  PosCurCts03                   1n   overlay(IndDspF : 69)
013300130222     d  PosCurCts04                   1n   overlay(IndDspF : 70)
013400130222     d  PosCurCts05                   1n   overlay(IndDspF : 71)
013500130222     d  PosCurCts06                   1n   overlay(IndDspF : 72)
013600130222     d  PosCurCts07                   1n   overlay(IndDspF : 73)
013700130222     d  PosCurCts08                   1n   overlay(IndDspF : 74)
013800130222     d  PosCurCts09                   1n   overlay(IndDspF : 75)
013900130222     d  PosCurCts10                   1n   overlay(IndDspF : 76)
014000130222     d  PosCurPkgDa                   1n   overlay(IndDspF : 77)
014100130222     d  PosCurPkgA                    1n   overlay(IndDspF : 78)
014200130222     d  PosCurNclDa                   1n   overlay(IndDspF : 79)
014300130222     d  PosCurNclA                    1n   overlay(IndDspF : 80)
014400130222     d  PosCurKsc                     1n   overlay(IndDspF : 81)
014500130222     d  PosCurCtr                     1n   overlay(IndDspF : 82)
014600130222     d  PosCurPrg                     1n   overlay(IndDspF : 83)
014700130222     d  PosCurLnp                     1n   overlay(IndDspF : 84)
014800130225       // -?Indicatori di errore generico
014900121024     d  ErrGenerico                   1n   overlay(IndDspF : 99)
015000121024
015100130225       // -?DS indicatori Dspf
015200121024     d WindDspF        ds                  inz
015300121024     d   WindDspFa             1     49    inz(*zero)
015400121024     d   WindDspFb            50     99    inz(*zero)
015500121024
015600130225       // -?DS controllo data
015700121024     d wlbdat          ds                  inz
015800121024     d  g02dat                 1      8  0
015900121024     d  g02inv                 9     16  0
016000121024     d  g02err                17     17
016100121024     d  g02tgi                18     22  0
016200130225
016300130225       // -?DS per gestione codici cliente di ritorno da XCLIR
016400130225     d                 ds
016500130225     d dsksn                   1      4p 0
016600130225     d dsksa                   1      4
016700130225
016800130225       // -?Parametri per il pgm chiamato
016900130225     d param           ds                  inz
017000130225     d  parks1                 1      7  0
017100130225     d  parks2                 8     14  0
017200130225     d  parks3                15     21  0
017300130225     d  parks4                22     28  0
017400130225     d  parks5                29     35  0
017500130225     d  parta1                36     38  0
017600130225     d  parta2                39     41  0
017700130225     d  parta3                42     44  0
017800130225     d  parta4                45     47  0
017900130225     d  parta5                48     50  0
018000130228     d  parln1                51     53  0
018100130228     d  parln2                54     56  0
018200130228     d  parln3                57     59  0
018300130228     d  parln4                60     62  0
018400130228     d  parln5                63     65  0
018500130228     d  pards1                66     73  0
018600130228     d  pards2                74     81  0
018700130228     d  parct1                82     83
018800130228     d  parct2                84     85
018900130228     d  parct3                86     87
019000130228     d  parct4                88     89
019100130228     d  parct5                90     91
019200130228     d  parct6                92     93
019300130228     d  parct7                94     95
019400130228     d  parct8                96     97
019500130228     d  parct9                98     99
019600130228     d  parct10              100    101
019700130228     d  parkgd               102    108  1
019800130228     d  parkga               109    115  1
019900130228     d  parcod               116    121  0
020000130228     d  parcoa               122    127  0
020100130228     d  partaof              128    128
020200130228     d  parksc               129    135  0
020300130228     d  parctr               136    138  0
020400130228     d  parprg               139    141  0
020500130228     d  parlnp               142    144  0
020600130228     d  pardtcal             145    145
020700130228     d  pardet               146    146
020800130228     d  parnrv               147    153  0
020900130228     d  parddt               154    161  0
021000130225
021100130225       // -?Parametri per il pgm TNTA36R
021200130225     d parmta36        ds                  inz
021300130225     d  ta36ksc                6     12  0
021400130225     d  ta36ctr               13     15
021500130225     d  ta36flg               16     16
021600130225     d  ta36vpr               17     17
021700130225     d  ta36kcc               18     21  0
021800130225     d  ta36prg               37     39
021900121025
022000121025       // -?Parametri ricevuti
022100121025     d KPJBA         e ds
022200100507
022300121025       // -?Parametri per Reperimento dati utente?
022400121025     d TIBS34ds      e ds
022500130225
022600130225       // -?Parametri per Reperimento S.Informativo
022700130225     d TIBS55ds      e ds
022800130222
022900130222       // -?Abilitazioni utente
023000130222     d TNTAA1DS      e ds                  inz
023100130225
023200130225       // -?Tabella Codici Tassazione
023300130225     d TRTB09DS      e ds
023400130222
023500130222       // -?P.O. abilitati all'utente
023600130222     d TRUL31DS      e ds
023700130222     d POG                    10    759    DIM(250)
023800130225
023900130225       // -?Ricerca clienti
024000130225     d xCLIrds       e ds
024100130225
024200130225       // -?Campo TAMFLO
024300130225     d dsTA01        e ds
024400130225
024500130225       // -?Organigramma
024600130225     d OG143         e ds
024700130225
024800130225       // -?DS Tabella CT - Codici Tassazione
024900130225     d dsCT          e ds
025000130225
025100130225       // -?DS Tabella TR - Codici Tariffa
025200130225     d dsTR          e ds
025300170526
025400170526       // -?DS Tabella $3 - Date trasferimento bolle di sede
025500170526     d ds$3          e ds                  inz
025600121025
025700121025       //--------------------------------------------------------------
025800121025       //?Definizione variabili globali.                               ?
025900121025       //--------------------------------------------------------------
026000121025
026100121025       // -?Flags booleani
026200121024     d wEnd            s               n   inz(*off)
026300121024     d wErrGrave       s               n   inz(*off)
026400121024     d wFine           s               n   inz(*off)
026500130222     d wInzD01         s               n   inz(*on)
026600121024
026700121024       // -?Campi Data
026800130222     d wData_ISO       s               d   datfmt(*iso)
026900130222     d wData_EUR       s               d   datfmt(*eur)
027000121024     d wOggi           s              8s 0 inz
027100130225
027200130225       // -?Campi di Comodo
027300130225     d sav_KNSIF       s                   like(KNSIF)
027400130225     d wCTS            s                   like(V01cts01)
027500130225     d wKSC            s                   like(V01ksc01)
027600130225     d wKSCd           s                   like(V01kscd01)
027700130225     d wLNP            s                   like(V01lnp01)
027800130225     d wLNPd           s                   like(V01lnpd01)
027900130225     d wDspDal         s                   like(V01dspdal)
028000130225     d wDspAl          s                   like(V01dspal)
028100130226
028200130226       // -?Nome libreria
028300130226     d wLib            s             10    inz
028400130226
028500130226       // -?Nome libreria + file dei file tariffe di cartello
028600130226     d wFLib           s             21    inz
028700121025
028800121025       //--------------------------------------------------------------
028900121025       //?Definizione procedure esterne.                     ?
029000121025       //--------------------------------------------------------------
029100121025
029200121025       // -?Sottomissione lavoro batch
029300121025     d bch10           pr                  extpgm('BCH10')
029400130225     d  kpjba                              likeds(KPJBA)
029500130225
029600130225       // -?Reperimento S.Informativo
029700130225     d tibs55r         pr                  extpgm('TIBS55R')
029800130225     d  tibs55ds                           likeds(tibs55ds)
029900130225
030000130225       // -?Ricerca offerte
030100130225     d tnta36c         pr                  extpgm('TNTA36C')
030200130225     d  kpjba                              likeds(kpjba)
030300130225
030400130225       // -?Ricerca tariffe
030500130225     d tnta36r         pr                  extpgm('TNTA36R')
030600130225     d  kpjba                              likeds(kpjba)
030700130225
030800130225       // -?Tabella Codici Tassazione
030900130225     d trtb09r         pr                  extpgm('TRTB09R')
031000130225     d  kpjba                              likeds(kpjba)
031100130225
031200130225       // -?Ricerca cliente
031300130225     d xclir           pr                  extpgm('XCLIR')
031400130225     d  xclirds                            likeds(XCLIRDS)
031500121025
031600121024       //--------------------------------------------------------------
031700121024       //?Definizione prototipi.
031800121024       //--------------------------------------------------------------
031900121024
032000121025      /copy gaitrasrc/srcprotopr,tibs34r
032100130225      /copy gaitrasrc/srcprotopi,tibs69r
032200130225      /copy gaitrasrc/srcprotopr,tibs69r
032300130222      /copy gaitrasrc/srcprotopr,tntaa1c
032400130222      /copy gaitrasrc/srcprotopr,trul31r
032500121024      /copy gaitrasrc/srcprotopr,xsrda8
032600121025
032700121025       //--------------------------------------------------------------
032800121025       //?Definizione key-list.                                        ?
032900121025       //--------------------------------------------------------------
033000170526       // - File TABEL00F
033100170526     d k03tabel      e ds                  extname(TABEL00F:*key)
033200170526     d                                     prefix(k_)
033300121025
033400121025       //--------------------------------------------------------------
033500121025       //?M A I N - L I N E                                            ?
033600121025       //--------------------------------------------------------------
033700121025
033800121025     c     *Entry        plist
033900121025     c                   parm                    KPJBA
034000121025
034100121025      /free
034200121025
034300121024       //?Operazioni iniziali?
034400121025       exsr RoutInz;
034500121025
034600121024       //?Gestione vidata?
034700121024       DOW wFine = *off;
034800130222         exsr GesD01;
034900121025       ENDDO;
035000121025
035100121024       //?Operazioni finali?
035200121025       exsr RoutEnd;
035300121025
035400121025       //--------------------------------------------------------------
035500121025       //?Operazioni iniziali.                                         ?
035600121025       //--------------------------------------------------------------
035700121024       BEGSR  RoutInz;
035800121025
035900121024       //?Reperimento dati job?
036000121024         exsr  DatiJob;
036100130222
036200130222       //?Reperimento Autorizzazione utente Tariffe
036300130222         reset TNTAA1DS;
036400130222         ITAA1caut = '§utegtc';
036500130222         kpjbu     = TNTAA1DS;
036600130222         tntaa1c (kpjba);
036700130222         TNTAA1DS = kpjbu;
036800130222
036900130222         IF  OTAA1fabi = 'N';
037000130222           wErrGrave   = *on;
037100130222           ErrMessage  = *on;
037200130222           ErrGenerico = *on;
037300130222           V01msg = skMsg(01);
037400130222           leavesr;
037500130222         ENDIF;
037600130222
037700130222       //?Reperimento P.O. abilitati all'utente
037800130222         clear TRUL31DS;
037900130222         I31abi = OTAA1cabi;
038000130222         I31cdi = DUTdis;
038100130222         I31car = DUTare;
038200130222         I31cpo = DUTpou;
038300130222         callp trul31r (kpjba : trul31ds);
038400130222         IF  O31pog <= *zeros;
038500130222           wErrGrave   = *on;
038600130222           ErrMessage  = *on;
038700130222           ErrGenerico = *on;
038800130222           V01msg = skMsg(01);
038900130222           leavesr;
039000130222         ENDIF;
039100130222
039200130222       //?Impostazione campi "fissi"
039300130222         V01pgm = SDSpgm;
039400130222         TBLkut = 1;
039500121025
039600121025       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
039700121024         wOggi = %dec(%date());
039800130226
039900130226       //?Determino la libreria
040000130226         IF  %subst(knsif : 7 : 1) = 'P';
040100130226           wLib = 'FILTRAGRPF';
040200130226         ELSE;
040300130226           wLib = 'FILTRAGRU';
040400130226         ENDIF;
040500130226
040600130226       //?Apro i file
040700130226         wFLib = %trim(wLib) + '/TNOFM00L';
040800130226         open  TNOFM00L;
040900130226         wFLib = %trim(wLib) + '/TIOFD01L';
041000130226         open  TIOFD01L;
041100170526
041200170526       //?Recupero dati da tabella $3
041300170526         k_TBLkut = 1;
041400170526         clear ds$3;
041500170526         k_TBLcod = '$3';
041600170526         k_TBLkey = '1';
041700170526         chain %kds(K03tabel) TABEL00F;
041800170526         IF  %found(TABEL00F);
041900170526           ds$3 = TBLuni;
042000170526         ENDIF;
042100170526         wDspDal = §$3dp0;
042200121025
042300121025       ENDSR;
042400121025
042500121025       //--------------------------------------------------------------
042600121025       //?Reperimento Dati del job (Utente/Operativi).                 ?
042700121025       //--------------------------------------------------------------
042800121024       BEGSR  DatiJob;
042900121025
043000121025         in(E) §AzUte;
043100121025         IF  NOT %error;
043200121025           in(E) §DatiUte;
043300121025         ENDIF;
043400121025         IF  %error or RSut = *blanks;
043500121025           clear TIBS34DS;
043600121025           tibs34r ( TIBS34DS );
043700121025           in §AzUte;
043800121025           in §DatiUte;
043900121025         ENDIF;
044000121025
044100121025       ENDSR;
044200121024
044300121024       //--------------------------------------------------------------
044400130225       //?Gestione videata.
044500121024       //--------------------------------------------------------------
044600130225       BEGSR  GesD01;
044700121024
044800121024       //?Inizializzazione videata
044900130222         IF  wInzD01;
045000130222           exsr Inzd01;
045100130222           wInzD01 = *off;
045200121024         ENDIF;
045300121024
045400121024       //?Emissione videata
045500130222         write TE58T01;
045600130222         exfmt TE58D01;
045700121024         ErrMessage   = *off;
045800121024         ErrGenerico  = *off;
045900130222         clear V01msg;
046000121024
046100121024         SELECT;
046200121024
046300121024       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
046400121025           WHEN  wErrGrave;
046500121025             wFine = *on;
046600121024
046700121024       //?F3=Fine
046800121024           WHEN  dsp_aid = c_F03;
046900130222             exsr F03D01;
047000121024
047100121024       //?F6=Conferma
047200121024           WHEN  dsp_aid = c_F06;
047300130222             exsr CtrD01;
047400121024             IF  ErrGenerico;
047500121024               leavesr;
047600121024             ENDIF;
047700130222             exsr F06D01;
047800121024
047900121024       //?Enter
048000121024           OTHER;
048100130222             exsr CtrD01;
048200121024             IF  ErrGenerico;
048300121024               leavesr;
048400121024             ENDIF;
048500121024
048600121024         ENDSL;
048700130226
048800121024       ENDSR;
048900121024
049000121024       //--------------------------------------------------------------
049100121024       //?Inizializzazione videata.
049200121024       //--------------------------------------------------------------
049300130222       BEGSR InzD01;
049400121024
049500130222         clear TE58D01;
049600130222
049700130222       //?Codici tariffa tutti a 999
049800130225         V01tar01 = *all'9';
049900130225         V01tar02 = *all'9';
050000130225         V01tar03 = *all'9';
050100130225         V01tar04 = *all'9';
050200130225         V01tar05 = *all'9';
050300130222
050400130222       //?LNP tutte a 999
050500130225         V01lnp01 = *all'9';
050600130225         V01lnp02 = *all'9';
050700130225         V01lnp03 = *all'9';
050800130225         V01lnp04 = *all'9';
050900130225         V01lnp05 = *all'9';
051000130222
051100130222       //?Data Spedizione DAL
051200130222         wData_ISO = %date(wOggi:*iso);
051300130313       //?Oggi - 2 mesi
051400130313         wData_EUR = wData_ISO - %months(2);
051500130313         wData_ISO = wData_EUR;
051600130225         V01dspdal = %dec(wData_EUR);
051700140521
051800130222       //?Data Spedizione AL oggi - 1 giorno
051900130225         wData_ISO = %date(wOggi:*iso);
052000130222         wData_EUR = wData_ISO - %days(1);
052100130225         wData_ISO = wData_EUR;
052200130225         V01dspal = %dec(wData_EUR);
052300130225         wDspAl = %dec(wData_ISO);
052400130222
052500130222       //?Imposto T=Tariffa
052600130225         V01taof = 'T';
052700121205
052800130222       //?Imposto S=Spedizione in Data Calcolo
052900130225         V01dtacal = 'S';
053000130222
053100130222       //?Imposto N=Stampa dettaglio
053200130225         V01stpdet = 'N';
053300121024
053400121024       ENDSR;
053500121024
053600121024       //--------------------------------------------------------------
053700121024       //?Gestione tasto funzionale F3.
053800121024       //?F3=Fine
053900121024       //--------------------------------------------------------------
054000130225       BEGSR F03D01;
054100121024
054200121024       //?Chiusura del programma
054300121024         wFine = *on;
054400121024
054500121024       ENDSR;
054600121024
054700121024       //--------------------------------------------------------------
054800121024       //?Gestione tasto funzionale F6 da videata D01
054900121024       //?F6=Conferma
055000121024       //--------------------------------------------------------------
055100130225       BEGSR F06D01;
055200121205
055300130226       //?Richiamo pgm per cercare il S.Informativo nel quale sottomettere il lavoro
055400130225         clear TIBS55DS;
055500130225         I50tla = 'L';
055600130225         I50ppo = DUTpou;
055700130225         I50apo = 046;
055800130225         tibs55r (TIBS55DS);
055900121205
056000130225       //?Salvo il S.Informativo originale
056100130225         sav_KNSIF = KNSIF;
056200130225
056300130225       //?Imposto kpjba
056400130225         KNSIF = O50asi;
056500130225         KCOAZ = 'TE59';
056600130226         kpjbu = param;
056700130225         BCH10 (kpjba);
056800130225
056900130225       //?Reimposto il S.Informativo originale
057000130225         KNSIF = sav_KNSIF;
057100121025
057200121025       //?Chiusura del programma
057300121025         wFine = *on;
057400121024
057500121024       ENDSR;
057600121024
057700121024       //--------------------------------------------------------------
057800121024       //?Controllo videata.
057900121024       //--------------------------------------------------------------
058000130225       BEGSR CtrD01;
058100121024
058200121024         WindDspF = IndDspF;
058300121024         reset WindDspFb;
058400121024         IndDspF  = WindDspF;
058500130225
058600130225       //?CLIENTI
058700130225         clear V01kscd01;
058800130225         clear V01kscd02;
058900130225         clear V01kscd03;
059000130225         clear V01kscd04;
059100130225         clear V01kscd05;
059200130225
059300130225       //?Primo -- obbligatorio
059400130225         IF  V01ksc01 = *blanks;
059500130225           ErrMessage  = *on;
059600130225           ErrGenerico = *on;
059700130225           PosCurKsc01 = *on;
059800130225           V01msg      = skMsg(02);
059900130225           leavesr;
060000130225         ENDIF;
060100130225
060200130225         wKSC = V01ksc01;
060300130225         exsr  ctrKSC;
060400130225         V01ksc01  = wKSC;
060500130225         V01kscd01 = wKSCd;
060600130225         IF  ErrGenerico;
060700130225           PosCurKsc01 = *on;
060800130225           leavesr;
060900130225         ENDIF;
061000130225         parks1    = %dec(wKSC:7:0);
061100130225
061200130225       //?Secondo
061300130225         IF  V01ksc02 <> *blanks and V01ksc02 <> *zeros;
061400130225           wKSC = V01ksc02;
061500130225           exsr  ctrKSC;
061600130225           V01ksc02  = wKSC;
061700130225           V01kscd02 = wKSCd;
061800130225           IF  ErrGenerico;
061900130225             PosCurKsc02 = *on;
062000130225             leavesr;
062100130225           ENDIF;
062200130225           parks2    = %dec(wKSC:7:0);
062300130225         ENDIF;
062400121024
062500130225       //?Terzo
062600130225         IF  V01ksc03 <> *blanks and V01ksc03 <> *zeros;
062700130225           wKSC = V01ksc03;
062800130225           exsr  ctrKSC;
062900130225           V01ksc03  = wKSC;
063000130225           V01kscd03 = wKSCd;
063100130225           IF  ErrGenerico;
063200130225             PosCurKsc03 = *on;
063300130225             leavesr;
063400130225           ENDIF;
063500130225           parks3    = %dec(wKSC:7:0);
063600130225         ENDIF;
063700130225
063800130225       //?Quarto
063900130225         IF  V01ksc04 <> *blanks and V01ksc04 <> *zeros;
064000130225           wKSC = V01ksc04;
064100130225           exsr  ctrKSC;
064200130225           V01ksc04  = wKSC;
064300130225           V01kscd04 = wKSCd;
064400130225           IF  ErrGenerico;
064500130225             PosCurKsc04 = *on;
064600130225             leavesr;
064700130225           ENDIF;
064800130225           parks4    = %dec(wKSC:7:0);
064900130225         ENDIF;
065000130225
065100130225       //?Quinto
065200130225         IF  V01ksc05 <> *blanks and V01ksc05 <> *zeros;
065300130225           wKSC = V01ksc05;
065400130225           exsr  ctrKSC;
065500130225           IF  ErrGenerico;
065600130225             PosCurKsc05 = *on;
065700130225             leavesr;
065800130225           ENDIF;
065900130225           V01ksc05  = wKSC;
066000130225           V01kscd05 = wKSCd;
066100130225           parks5    = %dec(wKSC:7:0);
066200130225         ENDIF;
066300130225
066400130225       //?TARIFFE
066500130225       //?Prima
066600130225         IF  V01tar01 <> 999;
066700130225           TAMksc = %int(V01ksc01);
066800130225           chain (TAMksc:V01tar01) TNTAM00L;
066900130225           IF  not %found(TNTAM00L);
067000130225             ErrMessage  = *on;
067100130225             ErrGenerico = *on;
067200130225             PosCurTar01 = *on;
067300130225             V01msg      = skMsg(03);
067400130225             leavesr;
067500130225           ENDIF;
067600130225         ENDIF;
067700130225         parta1 = V01tar01;
067800130225
067900130225       //?Seconda
068000130225         IF  V01tar02 <> 999;
068100130225           TAMksc = %int(V01ksc02);
068200130225           chain (TAMksc:V01tar02) TNTAM00L;
068300130225           IF  not %found(TNTAM00L);
068400130225             ErrMessage  = *on;
068500130225             ErrGenerico = *on;
068600130225             PosCurTar02 = *on;
068700130225             V01msg      = skMsg(03);
068800130225             leavesr;
068900130225           ENDIF;
069000130225         ENDIF;
069100130225         parta2 = V01tar02;
069200130225
069300130225       //?Terza
069400130225         IF  V01tar03 <> 999;
069500130225           TAMksc = %int(V01ksc03);
069600130225           chain (TAMksc:V01tar03) TNTAM00L;
069700130225           IF  not %found(TNTAM00L);
069800130225             ErrMessage  = *on;
069900130225             ErrGenerico = *on;
070000130225             PosCurTar03 = *on;
070100130225             V01msg      = skMsg(03);
070200130225             leavesr;
070300130225           ENDIF;
070400130225         ENDIF;
070500130225         parta3 = V01tar03;
070600130225
070700130225       //?Quarta
070800130225         IF  V01tar04 <> 999;
070900130225           TAMksc = %int(V01ksc04);
071000130225           chain (TAMksc:V01tar04) TNTAM00L;
071100130225           IF  not %found(TNTAM00L);
071200130225             ErrMessage  = *on;
071300130225             ErrGenerico = *on;
071400130225             PosCurTar04 = *on;
071500130225             V01msg      = skMsg(03);
071600130225             leavesr;
071700130225           ENDIF;
071800130225         ENDIF;
071900130225         parta4 = V01tar04;
072000130225
072100130225       //?Quinta
072200130225         IF  V01tar05 <> 999;
072300130225           TAMksc = %int(V01ksc05);
072400130225           chain (TAMksc:V01tar05) TNTAM00L;
072500130225           IF  not %found(TNTAM00L);
072600130225             ErrMessage  = *on;
072700130225             ErrGenerico = *on;
072800130225             PosCurTar05 = *on;
072900130225             V01msg      = skMsg(03);
073000130225             leavesr;
073100130225           ENDIF;
073200130225         ENDIF;
073300130225         parta5 = V01tar05;
073400130225
073500130225       //?LINEA DI PARTENZA
073600130225         clear V01lnpd01;
073700130225         clear V01lnpd02;
073800130225         clear V01lnpd03;
073900130225         clear V01lnpd04;
074000130225         clear V01lnpd05;
074100130225
074200130225       //?Prima
074300130225         IF  V01lnp01 <> 999;
074400130225           wLNP = V01lnp01;
074500130225           exsr ctrLNP;
074600130225           V01lnp01  = wLNP;
074700130225           V01lnpd01 = wLNPd;
074800130225           IF  ErrGenerico;
074900130225             PosCurLnp01 = *on;
075000130225             leavesr;
075100130225           ENDIF;
075200130228           parln1 = V01lnp01;
075300130225         ENDIF;
075400130225
075500130225       //?Seconda
075600130225         IF  V01lnp02 <> 999;
075700130225           wLNP = V01lnp02;
075800130225           exsr ctrLNP;
075900130225           V01lnp02  = wLNP;
076000130225           V01lnpd02 = wLNPd;
076100130225           IF  ErrGenerico;
076200130225             PosCurLnp02 = *on;
076300130225             leavesr;
076400130225           ENDIF;
076500130228           parln2 = V01lnp02;
076600130225         ENDIF;
076700130225
076800130225       //?Terza
076900130225         IF  V01lnp03 <> 999;
077000130225           wLNP = V01lnp03;
077100130225           exsr ctrLNP;
077200130225           V01lnp03  = wLNP;
077300130225           V01lnpd03 = wLNPd;
077400130225           IF  ErrGenerico;
077500130225             PosCurLnp03 = *on;
077600130225             leavesr;
077700130225           ENDIF;
077800130228           parln3 = V01lnp03;
077900130225         ENDIF;
078000130225
078100130225       //?Quarta
078200130225         IF  V01lnp04 <> 999;
078300130225           wLNP = V01lnp04;
078400130225           exsr ctrLNP;
078500130225           V01lnp04  = wLNP;
078600130225           V01lnpd04 = wLNPd;
078700130225           IF  ErrGenerico;
078800130225             PosCurLnp04 = *on;
078900130225             leavesr;
079000130225           ENDIF;
079100130228           parln4 = V01lnp04;
079200130225         ENDIF;
079300130225
079400130225       //?Quinta
079500130225         IF  V01lnp05 <> 999;
079600130225           wLNP = V01lnp05;
079700130225           exsr ctrLNP;
079800130225           V01lnp05  = wLNP;
079900130225           V01lnpd05 = wLNPd;
080000130225           IF  ErrGenerico;
080100130225             PosCurLnp05 = *on;
080200130225             leavesr;
080300130225           ENDIF;
080400130228           parln5 = V01lnp05;
080500130225         ENDIF;
080600130225
080700130225       //?Data spedizione dal
080800130225         clear wlbdat;
080900130225         g02dat = V01dspdal;
081000130225         xsrda8(wlbdat);
081100130225         IF  g02err    = '1';
081200130225           ErrMessage   = *on;
081300130225           ErrGenerico  = *on;
081400130225           PosCurDspDal = *on;
081500130225           V01msg       = skMsg(06);
081600130225           leavesr;
081700130225         ENDIF;
081800130225         V01dspdal = g02dat;
081900170526         IF  g02inv <= wDspDal;
082000130225           ErrMessage   = *on;
082100130225           ErrGenerico  = *on;
082200130225           PosCurDspDal = *on;
082300130225           V01msg       = skMsg(07);
082400130225           leavesr;
082500130225         ENDIF;
082600130225         pards1 = g02inv;
082700130225
082800130225       //?Data spedizione al
082900130225         clear wlbdat;
083000130225         g02dat = V01dspal;
083100130225         xsrda8(wlbdat);
083200130225         IF  g02err    = '1';
083300130225           ErrMessage   = *on;
083400130225           ErrGenerico  = *on;
083500130225           PosCurDspAl  = *on;
083600130225           V01msg       = skMsg(06);
083700130225           leavesr;
083800130225         ENDIF;
083900130225         V01dspal = g02dat;
084000130225         IF  g02inv > wDspAl;
084100130225           ErrMessage   = *on;
084200130225           ErrGenerico  = *on;
084300130225           PosCurDspAl = *on;
084400130225           V01msg       = skMsg(08);
084500130225           leavesr;
084600130225         ENDIF;
084700130225         pards2 = g02inv;
084800130225
084900130225       //?Devono esserci tutte e 2 le date
085000130225         IF  V01dspdal > 0 and V01dspal = 0;
085100130225           ErrMessage   = *on;
085200130225           ErrGenerico  = *on;
085300130225           PosCurDspAl  = *on;
085400130225           V01msg       = skMsg(06);
085500130225           leavesr;
085600130225         ENDIF;
085700130225         IF  V01dspal > 0 and V01dspdal = 0;
085800130225           ErrMessage   = *on;
085900130225           ErrGenerico  = *on;
086000130225           PosCurDspDal = *on;
086100130225           V01msg       = skMsg(06);
086200130225           leavesr;
086300130225         ENDIF;
086400130225
086500130225       //?Congruenza tra date
086600130225         IF  wDspAl < wDspDal;
086700130225           ErrMessage   = *on;
086800130225           ErrGenerico  = *on;
086900130225           PosCurDspAl  = *on;
087000130225           V01msg       = skMsg(09);
087100130225           leavesr;
087200130225         ENDIF;
087300170526         IF  pards2 < pards1;
087400170526           ErrMessage   = *on;
087500170526           ErrGenerico  = *on;
087600170526           PosCurDspAl  = *on;
087700170526           V01msg       = skMsg(09);
087800170526           leavesr;
087900170526         ENDIF;
088000130225
088100130225       //?CODICI TASSAZIONE
088200130225       //?Primo
088300130225         IF  V01cts01 <> *blanks;
088400130225           wCTS = V01cts01;
088500130225           exsr ctrCTS;
088600130225           V01cts01 = wCTS;
088700130225           parct1   = wCTS;
088800130225           IF  ErrGenerico;
088900130225             PosCurCts01 = *on;
089000130225             leavesr;
089100130225           ENDIF;
089200130225         ENDIF;
089300130225
089400130225       //?Secondo
089500130225         IF  V01cts02 <> *blanks;
089600130225           wCTS = V01cts02;
089700130225           exsr ctrCTS;
089800130225           V01cts02 = wCTS;
089900130225           parct2   = wCTS;
090000130225           IF  ErrGenerico;
090100130225             PosCurCts02 = *on;
090200130225             leavesr;
090300130225           ENDIF;
090400130225         ENDIF;
090500130225
090600130225       //?Terzo
090700130225         IF  V01cts03 <> *blanks;
090800130225           wCTS = V01cts03;
090900130225           exsr ctrCTS;
091000130225           V01cts03 = wCTS;
091100130225           parct3   = wCTS;
091200130225           IF  ErrGenerico;
091300130225             PosCurCts03 = *on;
091400130225             leavesr;
091500130225           ENDIF;
091600130225         ENDIF;
091700130225
091800130225       //?Quarto
091900130225         IF  V01cts04 <> *blanks;
092000130225           wCTS = V01cts04;
092100130225           exsr ctrCTS;
092200130225           V01cts04 = wCTS;
092300130225           parct4   = wCTS;
092400130225           IF  ErrGenerico;
092500130225             PosCurCts04 = *on;
092600130225             leavesr;
092700130225           ENDIF;
092800130225         ENDIF;
092900130225
093000130225       //?Quinto
093100130225         IF  V01cts05 <> *blanks;
093200130225           wCTS = V01cts05;
093300130225           exsr ctrCTS;
093400130225           V01cts05 = wCTS;
093500130225           parct5   = wCTS;
093600130225           IF  ErrGenerico;
093700130225             PosCurCts05 = *on;
093800130225             leavesr;
093900130225           ENDIF;
094000130225         ENDIF;
094100130225
094200130225       //?Sesto
094300130225         IF  V01cts06 <> *blanks;
094400130225           wCTS = V01cts06;
094500130225           exsr ctrCTS;
094600130225           V01cts06 = wCTS;
094700130225           parct6   = wCTS;
094800130225           IF  ErrGenerico;
094900130225             PosCurCts06 = *on;
095000130225             leavesr;
095100130225           ENDIF;
095200130225         ENDIF;
095300130225
095400130225       //?Settimo
095500130225         IF  V01cts07 <> *blanks;
095600130225           wCTS = V01cts07;
095700130225           exsr ctrCTS;
095800130225           V01cts07 = wCTS;
095900130225           parct7   = wCTS;
096000130225           IF  ErrGenerico;
096100130225             PosCurCts07 = *on;
096200130225             leavesr;
096300130225           ENDIF;
096400130225         ENDIF;
096500130225
096600130225       //?Ottavo
096700130225         IF  V01cts08 <> *blanks;
096800130225           wCTS = V01cts08;
096900130225           exsr ctrCTS;
097000130225           V01cts08 = wCTS;
097100130225           parct8   = wCTS;
097200130225           IF  ErrGenerico;
097300130225             PosCurCts08 = *on;
097400130225             leavesr;
097500130225           ENDIF;
097600130225         ENDIF;
097700130225
097800130225       //?Nono
097900130225         IF  V01cts09 <> *blanks;
098000130225           wCTS = V01cts09;
098100130225           exsr ctrCTS;
098200130225           V01cts09 = wCTS;
098300130225           parct9   = wCTS;
098400130225           IF  ErrGenerico;
098500130225             PosCurCts09 = *on;
098600130225             leavesr;
098700130225           ENDIF;
098800130225         ENDIF;
098900130225
099000130225       //?Decimo
099100130225         IF  V01cts10 <> *blanks;
099200130225           wCTS = V01cts10;
099300130225           exsr ctrCTS;
099400130225           V01cts10 = wCTS;
099500130225           parct10  = wCTS;
099600130225           IF  ErrGenerico;
099700130225             PosCurCts10 = *on;
099800130225             leavesr;
099900130225           ENDIF;
100000130225         ENDIF;
100100130225
100200130225       //?PESO
100300130225       //?Devono esserci tutti e 2 i pesi
100400130225         IF  V01pkgda > 0 and V01pkga = 0;
100500130225           ErrMessage  = *on;
100600130225           ErrGenerico = *on;
100700130225           PosCurPkgA  = *on;
100800130225           V01msg      = skMsg(11);
100900130225           leavesr;
101000130225         ENDIF;
101100130225         IF  V01pkga > 0 and V01pkgda = 0;
101200130225           ErrMessage  = *on;
101300130225           ErrGenerico = *on;
101400130225           PosCurPkgDa = *on;
101500130225           V01msg      = skMsg(11);
101600130225           leavesr;
101700130225         ENDIF;
101800130225
101900130225       //?Congruenza tra i pesi
102000130225         IF  V01pkga < V01pkgda;
102100130225           ErrMessage  = *on;
102200130225           ErrGenerico = *on;
102300130225           PosCurPkgA  = *on;
102400130225           V01msg      = skMsg(12);
102500130225           leavesr;
102600130225         ENDIF;
102700130225         parkgd = V01pkgda;
102800130225         parkga = V01pkga;
102900130225
103000130225       //?COLLI
103100130225       //?Devono esserci tutti e 2 i colli
103200130225         IF  V01nclda > 0 and V01ncla = 0;
103300130225           ErrMessage  = *on;
103400130225           ErrGenerico = *on;
103500130225           PosCurNclA  = *on;
103600130225           V01msg      = skMsg(13);
103700130225           leavesr;
103800130225         ENDIF;
103900130225         IF  V01ncla > 0 and V01nclda = 0;
104000130225           ErrMessage  = *on;
104100130225           ErrGenerico = *on;
104200130225           PosCurNclDa = *on;
104300130225           V01msg      = skMsg(12);
104400130225           leavesr;
104500130225         ENDIF;
104600130225
104700130225       //?Congruenza tra i colli
104800130225         IF  V01ncla < V01nclda;
104900130225           ErrMessage  = *on;
105000130225           ErrGenerico = *on;
105100130225           PosCurNclA  = *on;
105200130225           V01msg      = skMsg(14);
105300130225           leavesr;
105400130225         ENDIF;
105500130225         parcod = V01nclda;
105600130225         parcoa = V01ncla;
105700130225
105800130225       //?TARIFFA/OFFERTA
105900130225         clear V01destar;
106000130225
106100130225       //?Cliente/Trattativa obbligatorio
106200130225         IF  V01ksc = *blanks;
106300130225           ErrMessage  = *on;
106400130225           ErrGenerico = *on;
106500130225           PosCurKsc   = *on;
106600130225           V01msg      = skMsg(15);
106700130225           leavesr;
106800130225         ENDIF;
106900130225
107000130225       //?Se Tariffa
107100130225         IF  V01taof = 'T';
107200130225         //?controllo KSC
107300130225           wKSC = V01ksc;
107400130225           exsr  ctrKSC;
107500130225           V01ksc  = wKSC;
107600130225           V01destar = wKSCd;
107700130225           IF  ErrGenerico;
107800130225             PosCurKsc = *on;
107900130225             leavesr;
108000130225           ENDIF;
108100130225         //?deve esistere almeno una tariffa
108200130225           TAMksc = %int(V01ksc);
108300130225           chain TAMksc TNTAM00L;
108400130225           IF  not %found(TNTAM00L);
108500130225             ErrMessage  = *on;
108600130225             ErrGenerico = *on;
108700130225             PosCurKsc   = *on;
108800130225             V01msg      = skMsg(03);
108900130225             leavesr;
109000130225           ENDIF;
109100130225         ENDIF;
109200130225
109300130225       //?Se Offerta
109400130225         IF  V01taof = 'O';
109500130225         //?controllo Trattativa
109600130225           VISnrv = %int(V01ksc);
109700130225           chain VISnrv TIVIS05L;
109800130225           IF  not %found(TIVIS05L);
109900130225             ErrMessage  = *on;
110000130225             ErrGenerico = *on;
110100130225             PosCurKsc   = *on;
110200130225             V01msg      = skMsg(16);
110300130225             leavesr;
110400130225           ENDIF;
110500130313           V01destar = VISrag;
110600130225         //?utente abilitato alla trattativa
110700130225           clear TNTAA1DS;
110800130225           ITAA1caut = '§utecli';
110900130225           ITAA1nrv  = VISnrv;
111000130225           kpjbu     = tntaa1ds;
111100130225           tntaa1c (kpjba);
111200130225           TNTAA1DS = kpjbu;
111300130225           IF  OTAA1fabi = 'N';
111400130313             PosCurKsc   = *on;
111500130225             ErrMessage  = *on;
111600130225             ErrGenerico = *on;
111700130225             V01msg      = skMsg(16);
111800130225           leavesr;
111900130225         ENDIF;
112000130225         //?deve esistere almeno una offerta
112100130225           chain VISnrv TIVOF11L;
112200130225           IF  not %found(TIVOF11L);
112300130225             ErrMessage  = *on;
112400130225             ErrGenerico = *on;
112500130225             PosCurKsc   = *on;
112600130225             V01msg      = skMsg(17);
112700130225             leavesr;
112800130225           ENDIF;
112900130225         ENDIF;
113000130225         partaof = V01taof;
113100130225         parksc  = %dec(V01ksc:7:0);
113200130226         parnrv  = parksc;
113300130225
113400130225       //?CODICE TARIFFA
113500130225       //?obbligatorio
113600130313         IF  V01ctr = *blanks and %scan('?' : V01prg) = 0;
113700130225           ErrMessage  = *on;
113800130225           ErrGenerico = *on;
113900130225           PosCurCtr   = *on;
114000130225           V01msg      = skMsg(18);
114100130225           leavesr;
114200130225         ENDIF;
114300130225       //?valido
114400130313         IF  V01ctr <> *blanks;
114500130313           exsr ctrCTR;
114600130313           IF  ErrGenerico;
114700130313             PosCurCtr = *on;
114800130313             leavesr;
114900130313           ENDIF;
115000130313           parctr = %dec(V01ctr:3:0);
115100130313         ENDIF;
115200130225
115300130225       //?PROGRESSIVO TARIFFA
115400130313         IF  %scan('?' : V01prg) > 0;
115500130313           exsr RicercaTAR;
115600130313           leavesr;
115700130313         ENDIF;
115800130226       //?Solo valori numerici
115900130226         IF  %check(digits:V01prg) > 0;
116000130226           ErrMessage  = *on;
116100130226           ErrGenerico = *on;
116200130226           PosCurPrg   = *on;
116300130226           V01msg      = skMsg(23);
116400130226           leavesr;
116500130226         ENDIF;
116600130225       //?Deve esistere
116700130225         TAMksc = %dec(V01ksc:7:0);
116800130225         TAMctr = %dec(V01ctr:3:0);
116900130226         TAMprg = %dec(V01prg:3:0);
117000130225         IF  V01taof = 'T';
117100130225           chain (TAMksc:TAMctr:TAMprg) TNTAM00L;
117200130225         ELSE;
117300130225           chain (TAMksc:TAMctr:TAMprg) TNOFM00L;
117400130225         ENDIF;
117500130225         IF  not %found;
117600130225           ErrMessage  = *on;
117700130225           ErrGenerico = *on;
117800130226           PosCurPrg   = *on;
117900130225           V01msg      = skMsg(20);
118000130225           leavesr;
118100130225         ENDIF;
118200130225       //?Non accetto tariffe FedEx
118300130225         dsTA01 = TAMflo;
118400130225         IF  §TAfed = 'S';
118500130225           ErrMessage  = *on;
118600130225           ErrGenerico = *on;
118700130225           PosCurPrg = *on;
118800130225           V01msg      = skMsg(21);
118900130225           leavesr;
119000130225         ENDIF;
119100130226         parprg = %dec(V01prg:3:0);
119200130228         parddt = TAMddt;
119300130225
119400130225       //?LINEA DI PARTENZA TARIFFA
119500130225         clear V01lnpd;
119600130225         IF  V01lnp <> *zeros;
119700130225           wLNP = V01lnp;
119800130225           exsr ctrLNP;
119900130225           V01lnp  = wLNP;
120000130225           V01lnpd = wLNPd;
120100130225           IF  ErrGenerico;
120200130225             PosCurLnp = *on;
120300130225             leavesr;
120400130225           ENDIF;
120500130226         //?deve esistere in tariffa
120600130226           IF  V01taof = 'T';
120700130226             chain (TAMksc:TAMctr:TAMprg:V01lnp) TITAD04L;
120800130226           ELSE;
120900130226             chain (TAMksc:TAMctr:TAMprg:V01lnp) TIOFD01L;
121000130226           ENDIF;
121100130226           IF  not %found;
121200130226             ErrMessage  = *on;
121300130226             ErrGenerico = *on;
121400130226             PosCurLnp   = *on;
121500130226             V01msg      = skMsg(22);
121600130226             leavesr;
121700130226           ENDIF;
121800130225         ENDIF;
121900130225         parlnp = V01lnp;
122000130225
122100130225         pardtcal = V01dtacal;
122200130225         pardet   = V01stpdet;
122300130225
122400121024       ENDSR;
122500130225
122600130225       //--------------------------------------------------------------
122700130225       //?Controllo cliente.
122800130225       //--------------------------------------------------------------
122900130225       BEGSR  ctrKSC;
123000130225
123100130225       //?Ricerca del cliente
123200130225         IF  %scan('?' : wKSC) > 0;
123300130225           ErrGenerico = *on;
123400130225           clear xCLIrds;
123500130225           DXCaut = OTAA1cabi;
123600130225           DXCcap = DUTkci;
123700130225           xclir (xCLIrds);
123800130225           ksa = DXCksc;
123900130225           dsksa = ksa(1);
124000130225           ksc(1) = dsksn;
124100130225           wKSC = %editc(ksc(1):'X');
124200130225         ENDIF;
124300130225
124400130225       //?Solo valori numerici
124500130225         IF  %check(digits:wKSC) > 0;
124600130225           ErrMessage  = *on;
124700130225           ErrGenerico = *on;
124800130225           V01msg      = skMsg(02);
124900130225           leavesr;
125000130225         ENDIF;
125100130225
125200130225       //?Cliente valido
125300130225         clear tibs69ds;
125400130225         I69kac = %dec(wKSC:7:0);
125500130225         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
125600130225         IF  O69err <> *blanks;
125700130225           ErrMessage  = *on;
125800130225           ErrGenerico = *on;
125900130225           V01msg      = skMsg(02);
126000130225           leavesr;
126100130225         ENDIF;
126200130225         wKSCd = ACOrag;
126300130225
126400130225       //?Finisco qua i controlli se sto verificando la tariffa ed è una cartello
126500130225         IF  wKSC = V01ksc and %subst(V01ksc:1:4) = '8888';
126600130225           leavesr;
126700130225         ENDIF;
126800130225
126900130225       //?Utente abilitato al cliente
127000130225         clear TNTAA1DS;
127100130225         ITAA1caut = '§utecli';
127200130225         ITAA1ksc  = %dec(wKSC:7:0);
127300130225         kpjbu     = tntaa1ds;
127400130225         tntaa1c (kpjba);
127500130225         TNTAA1DS = kpjbu;
127600130225         IF  OTAA1fabi = 'N';
127700130225           ErrMessage  = *on;
127800130225           ErrGenerico = *on;
127900130225           V01msg      = skMsg(02);
128000130225           leavesr;
128100130225         ENDIF;
128200130225
128300130225       ENDSR;
128400130225
128500130225       //--------------------------------------------------------------
128600130225       //?Controllo linea di partenza.
128700130225       //--------------------------------------------------------------
128800130225       BEGSR  ctrLNP;
128900130225
129000130225       //?Linea di partenza valida
129100130225         chain wLNP AZORG01L;
129200130225         IF  not %found(AZORG01L) or
129300130225            (ORGfva <> *blanks and ORGfag = 'V');
129400130225           ErrMessage  = *on;
129500130225           ErrGenerico = *on;
129600130225           V01msg      = skMsg(04);
129700130225           leavesr;
129800130225         ENDIF;
129900130225         wLNPd = ORGdes;
130000130225         OG143 = ORGde3;
130100130225
130200130225       //?NO Linea di partenza FedEx
130300130225         IF  §OGntw = 'FED';
130400130225           ErrMessage  = *on;
130500130225           ErrGenerico = *on;
130600130225           V01msg      = skMsg(05);
130700130225           leavesr;
130800130225         ENDIF;
130900130225
131000130225       ENDSR;
131100130225
131200130225       //--------------------------------------------------------------
131300130225       //?Controllo codice tassazione.
131400130225       //--------------------------------------------------------------
131500130225       BEGSR  ctrCTS;
131600130225
131700130225         TBLcod = 'CT';
131800130225
131900130225       //?Ricerca
132000130225         IF  %scan('?' : wCTS) > 0;
132100130225           ErrGenerico = *on;
132200130225           clear TRTB09DS;
132300130225           TB09fun = '1';
132400130225           TB09ord = '1';
132500130225           kpjbu = TRTB09DS;
132600130225           trtb09r (kpjba);
132700130225           TRTB09DS = kpjbu;
132800130225           wCTS = TB09cod;
132900130225         ENDIF;
133000130225
133100130225       //?Verifico che esista in tabella
133200130225         TBLkey = wCTS;
133300130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
133400130225         IF  not %found(TABEL00F) or TBLflg <> *blanks;
133500130225           ErrMessage  = *on;
133600130225           ErrGenerico = *on;
133700130225           V01msg      = skMsg(10);
133800130225           leavesr;
133900130225         ENDIF;
134000130225         dsCT = TBLuni;
134100130225
134200130225       //?Codice tassazione utilizzabile
134300130225         IF  §CTutc = 'N';
134400130225           ErrMessage  = *on;
134500130225           ErrGenerico = *on;
134600130225           V01msg      = skMsg(10);
134700130225           leavesr;
134800130225         ENDIF;
134900130225
135000130225       ENDSR;
135100130225
135200130225       //--------------------------------------------------------------
135300130225       //?Controllo codice tariffa.
135400130225       //--------------------------------------------------------------
135500130225       BEGSR  ctrCTR;
135600130225
135700130225       //?Ricerca
135800130225         IF  %scan('?' : V01ctr) > 0;
135900130313           exsr RicercaTAR;
136000130225           leavesr;
136100130225         ENDIF;
136200130225
136300130225       //?Solo valori numerici
136400130225         IF  %check(digits:V01ctr) > 0;
136500130225           ErrMessage  = *on;
136600130225           ErrGenerico = *on;
136700130225           V01msg      = skMsg(18);
136800130225           leavesr;
136900130225         ENDIF;
137000130225
137100130225       //?Verifico che sia utilizzabile
137200130225         TBLcod = 'TR';
137300130225         TBLkey = %subst(V01ctr:1:1);
137400130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
137500130225         IF  %found(TABEL00F);
137600130225           dsTR = TBLuni;
137700130225         //?Codice tassazione utilizzabile
137800130225           IF  §TRute = 'N';
137900130225             ErrMessage  = *on;
138000130225             ErrGenerico = *on;
138100130225             V01msg      = skMsg(19);
138200130225             leavesr;
138300130225           ENDIF;
138400130225         ENDIF;
138500130225
138600130225       ENDSR;
138700130313
138800130313       //--------------------------------------------------------------
138900130313       //?Ricerca Tariffa.
139000130313       //--------------------------------------------------------------
139100130313       BEGSR  RicercaTAR;
139200130313
139300130313         ErrGenerico = *on;
139400130313         clear parmta36;
139500130313         ta36ksc = %dec(V01ksc:7:0);
139600130313         IF  V01taof = 'T';
139700130313           ta36flg = '1';
139800130313         ELSE;
139900130313           ta36flg = '3';
140000130313         ENDIF;
140100130313         ta36vpr = '1';
140200130313         ta36kcc = DUTkci;
140300130313         kpjbu = parmta36;
140400130313         IF  V01taof = 'T';
140500130313           tnta36r (kpjba);
140600130313         ELSE;
140700130313           tnta36c (kpjba);
140800130313         ENDIF;
140900130313         parmta36 = kpjbu;
141000130313         V01ctr = ta36ctr;
141100130313         V01prg = ta36prg;
141200130313
141300130313       ENDSR;
141400121025
141500121025       //--------------------------------------------------------------
141600121025       //?Operazioni finali.                                           ?
141700121025       //--------------------------------------------------------------
141800121024       BEGSR  RoutEnd;
141900121025
142000121025         *inLR = *on;
142100100507
142200121025       //?Uscita dal pgm
142300121025         return;
142400121025
142500121025       ENDSR;
142600121025
142700121025      /end-free
142800121024
142900121024       //--------------------------------------------------------------
143000121024       //?Schiere a tempo di compilazione.
143100121024       //--------------------------------------------------------------
143200121024
143300130222** - skMSG ------------------------------------------------------------------*
143400130222Utente non abilitato alla funzione                                             01
143500130225Cliente errato o non in gestione all'utente                                    02
143600130225Tariffa inesistente                                                            03
143700130225Linea di partenza errata o non in gestione all'utente                          04
143800130225Linea di partenza FedEx non ammessa                                            05
143900130225Data errata                                                                    06
144000130225Data DAL inferiore alla prima data consentita da elaborare                     07
144100130225Data AL non può essere maggiore della data corrente                            08
144200130225La data AL deve essere maggiore o uguale della data DAL                        09
144300130225Codice tassazione errato o non accettabile                                     10
144400130225Peso spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                 11
144500130225Peso spedizioni a.. minore da..                                                12
144600130225Colli spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                13
144700130225Colli spedizioni a.. minore da..                                               14
144800130225Immettere cliente/trattativa                                                   15
144900130225Trattativa errata o non in gestione all'utente                                 16
145000130225Offerta inesistente                                                            17
145100130225Codice tariffa errato o non trovato                                            18
145200130225Tipo tariffa non gestibile dal C.A.T.                                          19
145300130225Tariffa/Offerta errata o inesistente                                           20
145400130225Tariffa FedEx non ammessa                                                      21
145500130226Linea di Partenza non trovata nella tariffa/offerta richiesta                  22
145600130226Progressivo errato                                                             23
