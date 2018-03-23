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
025300121025
025400121025       //--------------------------------------------------------------
025500121025       //?Definizione variabili globali.                               ?
025600121025       //--------------------------------------------------------------
025700121025
025800121025       // -?Flags booleani
025900121024     d wEnd            s               n   inz(*off)
026000121024     d wErrGrave       s               n   inz(*off)
026100121024     d wFine           s               n   inz(*off)
026200130222     d wInzD01         s               n   inz(*on)
026300121024
026400121024       // -?Campi Data
026500130222     d wData_ISO       s               d   datfmt(*iso)
026600130222     d wData_EUR       s               d   datfmt(*eur)
026700121024     d wOggi           s              8s 0 inz
026800130225
026900130225       // -?Campi di Comodo
027000130225     d sav_KNSIF       s                   like(KNSIF)
027100130225     d wCTS            s                   like(V01cts01)
027200130225     d wKSC            s                   like(V01ksc01)
027300130225     d wKSCd           s                   like(V01kscd01)
027400130225     d wLNP            s                   like(V01lnp01)
027500130225     d wLNPd           s                   like(V01lnpd01)
027600130225     d wDspDal         s                   like(V01dspdal)
027700130225     d wDspAl          s                   like(V01dspal)
027800130226
027900130226       // -?Nome libreria
028000130226     d wLib            s             10    inz
028100130226
028200130226       // -?Nome libreria + file dei file tariffe di cartello
028300130226     d wFLib           s             21    inz
028400121025
028500121025       //--------------------------------------------------------------
028600121025       //?Definizione procedure esterne.                     ?
028700121025       //--------------------------------------------------------------
028800121025
028900121025       // -?Sottomissione lavoro batch
029000121025     d bch10           pr                  extpgm('BCH10')
029100130225     d  kpjba                              likeds(KPJBA)
029200130225
029300130225       // -?Reperimento S.Informativo
029400130225     d tibs55r         pr                  extpgm('TIBS55R')
029500130225     d  tibs55ds                           likeds(tibs55ds)
029600130225
029700130225       // -?Ricerca offerte
029800130225     d tnta36c         pr                  extpgm('TNTA36C')
029900130225     d  kpjba                              likeds(kpjba)
030000130225
030100130225       // -?Ricerca tariffe
030200130225     d tnta36r         pr                  extpgm('TNTA36R')
030300130225     d  kpjba                              likeds(kpjba)
030400130225
030500130225       // -?Tabella Codici Tassazione
030600130225     d trtb09r         pr                  extpgm('TRTB09R')
030700130225     d  kpjba                              likeds(kpjba)
030800130225
030900130225       // -?Ricerca cliente
031000130225     d xclir           pr                  extpgm('XCLIR')
031100130225     d  xclirds                            likeds(XCLIRDS)
031200121025
031300121024       //--------------------------------------------------------------
031400121024       //?Definizione prototipi.
031500121024       //--------------------------------------------------------------
031600121024
031700121025      /copy gaitrasrc/srcprotopr,tibs34r
031800130225      /copy gaitrasrc/srcprotopi,tibs69r
031900130225      /copy gaitrasrc/srcprotopr,tibs69r
032000130222      /copy gaitrasrc/srcprotopr,tntaa1c
032100130222      /copy gaitrasrc/srcprotopr,trul31r
032200121024      /copy gaitrasrc/srcprotopr,xsrda8
032300121025
032400121025       //--------------------------------------------------------------
032500121025       //?Definizione key-list.                                        ?
032600121025       //--------------------------------------------------------------
032700121025
032800121025       //--------------------------------------------------------------
032900121025       //?M A I N - L I N E                                            ?
033000121025       //--------------------------------------------------------------
033100121025
033200121025     c     *Entry        plist
033300121025     c                   parm                    KPJBA
033400121025
033500121025      /free
033600121025
033700121024       //?Operazioni iniziali?
033800121025       exsr RoutInz;
033900121025
034000121024       //?Gestione vidata?
034100121024       DOW wFine = *off;
034200130222         exsr GesD01;
034300121025       ENDDO;
034400121025
034500121024       //?Operazioni finali?
034600121025       exsr RoutEnd;
034700121025
034800121025       //--------------------------------------------------------------
034900121025       //?Operazioni iniziali.                                         ?
035000121025       //--------------------------------------------------------------
035100121024       BEGSR  RoutInz;
035200121025
035300121024       //?Reperimento dati job?
035400121024         exsr  DatiJob;
035500130222
035600130222       //?Reperimento Autorizzazione utente Tariffe
035700130222         reset TNTAA1DS;
035800130222         ITAA1caut = '§utegtc';
035900130222         kpjbu     = TNTAA1DS;
036000130222         tntaa1c (kpjba);
036100130222         TNTAA1DS = kpjbu;
036200130222
036300130222         IF  OTAA1fabi = 'N';
036400130222           wErrGrave   = *on;
036500130222           ErrMessage  = *on;
036600130222           ErrGenerico = *on;
036700130222           V01msg = skMsg(01);
036800130222           leavesr;
036900130222         ENDIF;
037000130222
037100130222       //?Reperimento P.O. abilitati all'utente
037200130222         clear TRUL31DS;
037300130222         I31abi = OTAA1cabi;
037400130222         I31cdi = DUTdis;
037500130222         I31car = DUTare;
037600130222         I31cpo = DUTpou;
037700130222         callp trul31r (kpjba : trul31ds);
037800130222         IF  O31pog <= *zeros;
037900130222           wErrGrave   = *on;
038000130222           ErrMessage  = *on;
038100130222           ErrGenerico = *on;
038200130222           V01msg = skMsg(01);
038300130222           leavesr;
038400130222         ENDIF;
038500130222
038600130222       //?Impostazione campi "fissi"
038700130222         V01pgm = SDSpgm;
038800130222         TBLkut = 1;
038900121025
039000121025       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
039100121024         wOggi = %dec(%date());
039200130226
039300130226       //?Determino la libreria
039400130226         IF  %subst(knsif : 7 : 1) = 'P';
039500130226           wLib = 'FILTRAGRPF';
039600130226         ELSE;
039700130226           wLib = 'FILTRAGRU';
039800130226         ENDIF;
039900130226
040000130226       //?Apro i file
040100130226         wFLib = %trim(wLib) + '/TNOFM00L';
040200130226         open  TNOFM00L;
040300130226         wFLib = %trim(wLib) + '/TIOFD01L';
040400130226         open  TIOFD01L;
040500121025
040600121025       ENDSR;
040700121025
040800121025       //--------------------------------------------------------------
040900121025       //?Reperimento Dati del job (Utente/Operativi).                 ?
041000121025       //--------------------------------------------------------------
041100121024       BEGSR  DatiJob;
041200121025
041300121025         in(E) §AzUte;
041400121025         IF  NOT %error;
041500121025           in(E) §DatiUte;
041600121025         ENDIF;
041700121025         IF  %error or RSut = *blanks;
041800121025           clear TIBS34DS;
041900121025           tibs34r ( TIBS34DS );
042000121025           in §AzUte;
042100121025           in §DatiUte;
042200121025         ENDIF;
042300121025
042400121025       ENDSR;
042500121024
042600121024       //--------------------------------------------------------------
042700130225       //?Gestione videata.
042800121024       //--------------------------------------------------------------
042900130225       BEGSR  GesD01;
043000121024
043100121024       //?Inizializzazione videata
043200130222         IF  wInzD01;
043300130222           exsr Inzd01;
043400130222           wInzD01 = *off;
043500121024         ENDIF;
043600121024
043700121024       //?Emissione videata
043800130222         write TE58T01;
043900130222         exfmt TE58D01;
044000121024         ErrMessage   = *off;
044100121024         ErrGenerico  = *off;
044200130222         clear V01msg;
044300121024
044400121024         SELECT;
044500121024
044600121024       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
044700121025           WHEN  wErrGrave;
044800121025             wFine = *on;
044900121024
045000121024       //?F3=Fine
045100121024           WHEN  dsp_aid = c_F03;
045200130222             exsr F03D01;
045300121024
045400121024       //?F6=Conferma
045500121024           WHEN  dsp_aid = c_F06;
045600130222             exsr CtrD01;
045700121024             IF  ErrGenerico;
045800121024               leavesr;
045900121024             ENDIF;
046000130222             exsr F06D01;
046100121024
046200121024       //?Enter
046300121024           OTHER;
046400130222             exsr CtrD01;
046500121024             IF  ErrGenerico;
046600121024               leavesr;
046700121024             ENDIF;
046800121024
046900121024         ENDSL;
047000130226
047100121024       ENDSR;
047200121024
047300121024       //--------------------------------------------------------------
047400121024       //?Inizializzazione videata.
047500121024       //--------------------------------------------------------------
047600130222       BEGSR InzD01;
047700121024
047800130222         clear TE58D01;
047900130222
048000130222       //?Codici tariffa tutti a 999
048100130225         V01tar01 = *all'9';
048200130225         V01tar02 = *all'9';
048300130225         V01tar03 = *all'9';
048400130225         V01tar04 = *all'9';
048500130225         V01tar05 = *all'9';
048600130222
048700130222       //?LNP tutte a 999
048800130225         V01lnp01 = *all'9';
048900130225         V01lnp02 = *all'9';
049000130225         V01lnp03 = *all'9';
049100130225         V01lnp04 = *all'9';
049200130225         V01lnp05 = *all'9';
049300130222
049400130222       //?Data Spedizione DAL
049500130222         wData_ISO = %date(wOggi:*iso);
049600130313       //?Oggi - 2 mesi
049601140521       // se utente non EDP 2 mesi indiettro massimo altrimenti 14
049602140521       If %subst(Knmus:1:3) <> 'EDP' ;
049700130313         wData_EUR = wData_ISO - %months(2);
049800130313         wData_ISO = wData_EUR;
049900130225         V01dspdal = %dec(wData_EUR);
050000130225         wDspDal = %dec(wData_ISO);
050100140521       Else ;
050101140521       //?Oggi - 14 mesi per utenti EDP
050102140521         wData_EUR = wData_ISO - %months(14);
050103140521         wData_ISO = wData_EUR;
050104140521         V01dspdal = %dec(wData_EUR);
050105140521         wDspDal = %dec(wData_ISO);
050106140521       Endif ;
050107140521
050200130222       //?Data Spedizione AL oggi - 1 giorno
050300130225         wData_ISO = %date(wOggi:*iso);
050400130222         wData_EUR = wData_ISO - %days(1);
050500130225         wData_ISO = wData_EUR;
050600130225         V01dspal = %dec(wData_EUR);
050700130225         wDspAl = %dec(wData_ISO);
050800130222
050900130222       //?Imposto T=Tariffa
051000130225         V01taof = 'T';
051100121205
051200130222       //?Imposto S=Spedizione in Data Calcolo
051300130225         V01dtacal = 'S';
051400130222
051500130222       //?Imposto N=Stampa dettaglio
051600130225         V01stpdet = 'N';
051700121024
051800121024       ENDSR;
051900121024
052000121024       //--------------------------------------------------------------
052100121024       //?Gestione tasto funzionale F3.
052200121024       //?F3=Fine
052300121024       //--------------------------------------------------------------
052400130225       BEGSR F03D01;
052500121024
052600121024       //?Chiusura del programma
052700121024         wFine = *on;
052800121024
052900121024       ENDSR;
053000121024
053100121024       //--------------------------------------------------------------
053200121024       //?Gestione tasto funzionale F6 da videata D01
053300121024       //?F6=Conferma
053400121024       //--------------------------------------------------------------
053500130225       BEGSR F06D01;
053600121205
053700130226       //?Richiamo pgm per cercare il S.Informativo nel quale sottomettere il lavoro
053800130225         clear TIBS55DS;
053900130225         I50tla = 'L';
054000130225         I50ppo = DUTpou;
054100130225         I50apo = 046;
054200130225         tibs55r (TIBS55DS);
054300121205
054400130225       //?Salvo il S.Informativo originale
054500130225         sav_KNSIF = KNSIF;
054600130225
054700130225       //?Imposto kpjba
054800130225         KNSIF = O50asi;
054900130225         KCOAZ = 'TE59';
055000130226         kpjbu = param;
055100130225         BCH10 (kpjba);
055200130225
055300130225       //?Reimposto il S.Informativo originale
055400130225         KNSIF = sav_KNSIF;
055500121025
055600121025       //?Chiusura del programma
055700121025         wFine = *on;
055800121024
055900121024       ENDSR;
056000121024
056100121024       //--------------------------------------------------------------
056200121024       //?Controllo videata.
056300121024       //--------------------------------------------------------------
056400130225       BEGSR CtrD01;
056500121024
056600121024         WindDspF = IndDspF;
056700121024         reset WindDspFb;
056800121024         IndDspF  = WindDspF;
056900130225
057000130225       //?CLIENTI
057100130225         clear V01kscd01;
057200130225         clear V01kscd02;
057300130225         clear V01kscd03;
057400130225         clear V01kscd04;
057500130225         clear V01kscd05;
057600130225
057700130225       //?Primo -- obbligatorio
057800130225         IF  V01ksc01 = *blanks;
057900130225           ErrMessage  = *on;
058000130225           ErrGenerico = *on;
058100130225           PosCurKsc01 = *on;
058200130225           V01msg      = skMsg(02);
058300130225           leavesr;
058400130225         ENDIF;
058500130225
058600130225         wKSC = V01ksc01;
058700130225         exsr  ctrKSC;
058800130225         V01ksc01  = wKSC;
058900130225         V01kscd01 = wKSCd;
059000130225         IF  ErrGenerico;
059100130225           PosCurKsc01 = *on;
059200130225           leavesr;
059300130225         ENDIF;
059400130225         parks1    = %dec(wKSC:7:0);
059500130225
059600130225       //?Secondo
059700130225         IF  V01ksc02 <> *blanks and V01ksc02 <> *zeros;
059800130225           wKSC = V01ksc02;
059900130225           exsr  ctrKSC;
060000130225           V01ksc02  = wKSC;
060100130225           V01kscd02 = wKSCd;
060200130225           IF  ErrGenerico;
060300130225             PosCurKsc02 = *on;
060400130225             leavesr;
060500130225           ENDIF;
060600130225           parks2    = %dec(wKSC:7:0);
060700130225         ENDIF;
060800121024
060900130225       //?Terzo
061000130225         IF  V01ksc03 <> *blanks and V01ksc03 <> *zeros;
061100130225           wKSC = V01ksc03;
061200130225           exsr  ctrKSC;
061300130225           V01ksc03  = wKSC;
061400130225           V01kscd03 = wKSCd;
061500130225           IF  ErrGenerico;
061600130225             PosCurKsc03 = *on;
061700130225             leavesr;
061800130225           ENDIF;
061900130225           parks3    = %dec(wKSC:7:0);
062000130225         ENDIF;
062100130225
062200130225       //?Quarto
062300130225         IF  V01ksc04 <> *blanks and V01ksc04 <> *zeros;
062400130225           wKSC = V01ksc04;
062500130225           exsr  ctrKSC;
062600130225           V01ksc04  = wKSC;
062700130225           V01kscd04 = wKSCd;
062800130225           IF  ErrGenerico;
062900130225             PosCurKsc04 = *on;
063000130225             leavesr;
063100130225           ENDIF;
063200130225           parks4    = %dec(wKSC:7:0);
063300130225         ENDIF;
063400130225
063500130225       //?Quinto
063600130225         IF  V01ksc05 <> *blanks and V01ksc05 <> *zeros;
063700130225           wKSC = V01ksc05;
063800130225           exsr  ctrKSC;
063900130225           IF  ErrGenerico;
064000130225             PosCurKsc05 = *on;
064100130225             leavesr;
064200130225           ENDIF;
064300130225           V01ksc05  = wKSC;
064400130225           V01kscd05 = wKSCd;
064500130225           parks5    = %dec(wKSC:7:0);
064600130225         ENDIF;
064700130225
064800130225       //?TARIFFE
064900130225       //?Prima
065000130225         IF  V01tar01 <> 999;
065100130225           TAMksc = %int(V01ksc01);
065200130225           chain (TAMksc:V01tar01) TNTAM00L;
065300130225           IF  not %found(TNTAM00L);
065400130225             ErrMessage  = *on;
065500130225             ErrGenerico = *on;
065600130225             PosCurTar01 = *on;
065700130225             V01msg      = skMsg(03);
065800130225             leavesr;
065900130225           ENDIF;
066000130225         ENDIF;
066100130225         parta1 = V01tar01;
066200130225
066300130225       //?Seconda
066400130225         IF  V01tar02 <> 999;
066500130225           TAMksc = %int(V01ksc02);
066600130225           chain (TAMksc:V01tar02) TNTAM00L;
066700130225           IF  not %found(TNTAM00L);
066800130225             ErrMessage  = *on;
066900130225             ErrGenerico = *on;
067000130225             PosCurTar02 = *on;
067100130225             V01msg      = skMsg(03);
067200130225             leavesr;
067300130225           ENDIF;
067400130225         ENDIF;
067500130225         parta2 = V01tar02;
067600130225
067700130225       //?Terza
067800130225         IF  V01tar03 <> 999;
067900130225           TAMksc = %int(V01ksc03);
068000130225           chain (TAMksc:V01tar03) TNTAM00L;
068100130225           IF  not %found(TNTAM00L);
068200130225             ErrMessage  = *on;
068300130225             ErrGenerico = *on;
068400130225             PosCurTar03 = *on;
068500130225             V01msg      = skMsg(03);
068600130225             leavesr;
068700130225           ENDIF;
068800130225         ENDIF;
068900130225         parta3 = V01tar03;
069000130225
069100130225       //?Quarta
069200130225         IF  V01tar04 <> 999;
069300130225           TAMksc = %int(V01ksc04);
069400130225           chain (TAMksc:V01tar04) TNTAM00L;
069500130225           IF  not %found(TNTAM00L);
069600130225             ErrMessage  = *on;
069700130225             ErrGenerico = *on;
069800130225             PosCurTar04 = *on;
069900130225             V01msg      = skMsg(03);
070000130225             leavesr;
070100130225           ENDIF;
070200130225         ENDIF;
070300130225         parta4 = V01tar04;
070400130225
070500130225       //?Quinta
070600130225         IF  V01tar05 <> 999;
070700130225           TAMksc = %int(V01ksc05);
070800130225           chain (TAMksc:V01tar05) TNTAM00L;
070900130225           IF  not %found(TNTAM00L);
071000130225             ErrMessage  = *on;
071100130225             ErrGenerico = *on;
071200130225             PosCurTar05 = *on;
071300130225             V01msg      = skMsg(03);
071400130225             leavesr;
071500130225           ENDIF;
071600130225         ENDIF;
071700130225         parta5 = V01tar05;
071800130225
071900130225       //?LINEA DI PARTENZA
072000130225         clear V01lnpd01;
072100130225         clear V01lnpd02;
072200130225         clear V01lnpd03;
072300130225         clear V01lnpd04;
072400130225         clear V01lnpd05;
072500130225
072600130225       //?Prima
072700130225         IF  V01lnp01 <> 999;
072800130225           wLNP = V01lnp01;
072900130225           exsr ctrLNP;
073000130225           V01lnp01  = wLNP;
073100130225           V01lnpd01 = wLNPd;
073200130225           IF  ErrGenerico;
073300130225             PosCurLnp01 = *on;
073400130225             leavesr;
073500130225           ENDIF;
073600130228           parln1 = V01lnp01;
073700130225         ENDIF;
073800130225
073900130225       //?Seconda
074000130225         IF  V01lnp02 <> 999;
074100130225           wLNP = V01lnp02;
074200130225           exsr ctrLNP;
074300130225           V01lnp02  = wLNP;
074400130225           V01lnpd02 = wLNPd;
074500130225           IF  ErrGenerico;
074600130225             PosCurLnp02 = *on;
074700130225             leavesr;
074800130225           ENDIF;
074900130228           parln2 = V01lnp02;
075000130225         ENDIF;
075100130225
075200130225       //?Terza
075300130225         IF  V01lnp03 <> 999;
075400130225           wLNP = V01lnp03;
075500130225           exsr ctrLNP;
075600130225           V01lnp03  = wLNP;
075700130225           V01lnpd03 = wLNPd;
075800130225           IF  ErrGenerico;
075900130225             PosCurLnp03 = *on;
076000130225             leavesr;
076100130225           ENDIF;
076200130228           parln3 = V01lnp03;
076300130225         ENDIF;
076400130225
076500130225       //?Quarta
076600130225         IF  V01lnp04 <> 999;
076700130225           wLNP = V01lnp04;
076800130225           exsr ctrLNP;
076900130225           V01lnp04  = wLNP;
077000130225           V01lnpd04 = wLNPd;
077100130225           IF  ErrGenerico;
077200130225             PosCurLnp04 = *on;
077300130225             leavesr;
077400130225           ENDIF;
077500130228           parln4 = V01lnp04;
077600130225         ENDIF;
077700130225
077800130225       //?Quinta
077900130225         IF  V01lnp05 <> 999;
078000130225           wLNP = V01lnp05;
078100130225           exsr ctrLNP;
078200130225           V01lnp05  = wLNP;
078300130225           V01lnpd05 = wLNPd;
078400130225           IF  ErrGenerico;
078500130225             PosCurLnp05 = *on;
078600130225             leavesr;
078700130225           ENDIF;
078800130228           parln5 = V01lnp05;
078900130225         ENDIF;
079000130225
079100130225       //?Data spedizione dal
079200130225         clear wlbdat;
079300130225         g02dat = V01dspdal;
079400130225         xsrda8(wlbdat);
079500130225         IF  g02err    = '1';
079600130225           ErrMessage   = *on;
079700130225           ErrGenerico  = *on;
079800130225           PosCurDspDal = *on;
079900130225           V01msg       = skMsg(06);
080000130225           leavesr;
080100130225         ENDIF;
080200130225         V01dspdal = g02dat;
080300130225         IF  g02inv < wDspDal;
080400130225           ErrMessage   = *on;
080500130225           ErrGenerico  = *on;
080600130225           PosCurDspDal = *on;
080700130225           V01msg       = skMsg(07);
080800130225           leavesr;
080900130225         ENDIF;
081000130225         pards1 = g02inv;
081100130225
081200130225       //?Data spedizione al
081300130225         clear wlbdat;
081400130225         g02dat = V01dspal;
081500130225         xsrda8(wlbdat);
081600130225         IF  g02err    = '1';
081700130225           ErrMessage   = *on;
081800130225           ErrGenerico  = *on;
081900130225           PosCurDspAl  = *on;
082000130225           V01msg       = skMsg(06);
082100130225           leavesr;
082200130225         ENDIF;
082300130225         V01dspal = g02dat;
082400130225         IF  g02inv > wDspAl;
082500130225           ErrMessage   = *on;
082600130225           ErrGenerico  = *on;
082700130225           PosCurDspAl = *on;
082800130225           V01msg       = skMsg(08);
082900130225           leavesr;
083000130225         ENDIF;
083100130225         pards2 = g02inv;
083200130225
083300130225       //?Devono esserci tutte e 2 le date
083400130225         IF  V01dspdal > 0 and V01dspal = 0;
083500130225           ErrMessage   = *on;
083600130225           ErrGenerico  = *on;
083700130225           PosCurDspAl  = *on;
083800130225           V01msg       = skMsg(06);
083900130225           leavesr;
084000130225         ENDIF;
084100130225         IF  V01dspal > 0 and V01dspdal = 0;
084200130225           ErrMessage   = *on;
084300130225           ErrGenerico  = *on;
084400130225           PosCurDspDal = *on;
084500130225           V01msg       = skMsg(06);
084600130225           leavesr;
084700130225         ENDIF;
084800130225
084900130225       //?Congruenza tra date
085000130225         IF  wDspAl < wDspDal;
085100130225           ErrMessage   = *on;
085200130225           ErrGenerico  = *on;
085300130225           PosCurDspAl  = *on;
085400130225           V01msg       = skMsg(09);
085500130225           leavesr;
085600130225         ENDIF;
085700130225
085800130225       //?CODICI TASSAZIONE
085900130225       //?Primo
086000130225         IF  V01cts01 <> *blanks;
086100130225           wCTS = V01cts01;
086200130225           exsr ctrCTS;
086300130225           V01cts01 = wCTS;
086400130225           parct1   = wCTS;
086500130225           IF  ErrGenerico;
086600130225             PosCurCts01 = *on;
086700130225             leavesr;
086800130225           ENDIF;
086900130225         ENDIF;
087000130225
087100130225       //?Secondo
087200130225         IF  V01cts02 <> *blanks;
087300130225           wCTS = V01cts02;
087400130225           exsr ctrCTS;
087500130225           V01cts02 = wCTS;
087600130225           parct2   = wCTS;
087700130225           IF  ErrGenerico;
087800130225             PosCurCts02 = *on;
087900130225             leavesr;
088000130225           ENDIF;
088100130225         ENDIF;
088200130225
088300130225       //?Terzo
088400130225         IF  V01cts03 <> *blanks;
088500130225           wCTS = V01cts03;
088600130225           exsr ctrCTS;
088700130225           V01cts03 = wCTS;
088800130225           parct3   = wCTS;
088900130225           IF  ErrGenerico;
089000130225             PosCurCts03 = *on;
089100130225             leavesr;
089200130225           ENDIF;
089300130225         ENDIF;
089400130225
089500130225       //?Quarto
089600130225         IF  V01cts04 <> *blanks;
089700130225           wCTS = V01cts04;
089800130225           exsr ctrCTS;
089900130225           V01cts04 = wCTS;
090000130225           parct4   = wCTS;
090100130225           IF  ErrGenerico;
090200130225             PosCurCts04 = *on;
090300130225             leavesr;
090400130225           ENDIF;
090500130225         ENDIF;
090600130225
090700130225       //?Quinto
090800130225         IF  V01cts05 <> *blanks;
090900130225           wCTS = V01cts05;
091000130225           exsr ctrCTS;
091100130225           V01cts05 = wCTS;
091200130225           parct5   = wCTS;
091300130225           IF  ErrGenerico;
091400130225             PosCurCts05 = *on;
091500130225             leavesr;
091600130225           ENDIF;
091700130225         ENDIF;
091800130225
091900130225       //?Sesto
092000130225         IF  V01cts06 <> *blanks;
092100130225           wCTS = V01cts06;
092200130225           exsr ctrCTS;
092300130225           V01cts06 = wCTS;
092400130225           parct6   = wCTS;
092500130225           IF  ErrGenerico;
092600130225             PosCurCts06 = *on;
092700130225             leavesr;
092800130225           ENDIF;
092900130225         ENDIF;
093000130225
093100130225       //?Settimo
093200130225         IF  V01cts07 <> *blanks;
093300130225           wCTS = V01cts07;
093400130225           exsr ctrCTS;
093500130225           V01cts07 = wCTS;
093600130225           parct7   = wCTS;
093700130225           IF  ErrGenerico;
093800130225             PosCurCts07 = *on;
093900130225             leavesr;
094000130225           ENDIF;
094100130225         ENDIF;
094200130225
094300130225       //?Ottavo
094400130225         IF  V01cts08 <> *blanks;
094500130225           wCTS = V01cts08;
094600130225           exsr ctrCTS;
094700130225           V01cts08 = wCTS;
094800130225           parct8   = wCTS;
094900130225           IF  ErrGenerico;
095000130225             PosCurCts08 = *on;
095100130225             leavesr;
095200130225           ENDIF;
095300130225         ENDIF;
095400130225
095500130225       //?Nono
095600130225         IF  V01cts09 <> *blanks;
095700130225           wCTS = V01cts09;
095800130225           exsr ctrCTS;
095900130225           V01cts09 = wCTS;
096000130225           parct9   = wCTS;
096100130225           IF  ErrGenerico;
096200130225             PosCurCts09 = *on;
096300130225             leavesr;
096400130225           ENDIF;
096500130225         ENDIF;
096600130225
096700130225       //?Decimo
096800130225         IF  V01cts10 <> *blanks;
096900130225           wCTS = V01cts10;
097000130225           exsr ctrCTS;
097100130225           V01cts10 = wCTS;
097200130225           parct10  = wCTS;
097300130225           IF  ErrGenerico;
097400130225             PosCurCts10 = *on;
097500130225             leavesr;
097600130225           ENDIF;
097700130225         ENDIF;
097800130225
097900130225       //?PESO
098000130225       //?Devono esserci tutti e 2 i pesi
098100130225         IF  V01pkgda > 0 and V01pkga = 0;
098200130225           ErrMessage  = *on;
098300130225           ErrGenerico = *on;
098400130225           PosCurPkgA  = *on;
098500130225           V01msg      = skMsg(11);
098600130225           leavesr;
098700130225         ENDIF;
098800130225         IF  V01pkga > 0 and V01pkgda = 0;
098900130225           ErrMessage  = *on;
099000130225           ErrGenerico = *on;
099100130225           PosCurPkgDa = *on;
099200130225           V01msg      = skMsg(11);
099300130225           leavesr;
099400130225         ENDIF;
099500130225
099600130225       //?Congruenza tra i pesi
099700130225         IF  V01pkga < V01pkgda;
099800130225           ErrMessage  = *on;
099900130225           ErrGenerico = *on;
100000130225           PosCurPkgA  = *on;
100100130225           V01msg      = skMsg(12);
100200130225           leavesr;
100300130225         ENDIF;
100400130225         parkgd = V01pkgda;
100500130225         parkga = V01pkga;
100600130225
100700130225       //?COLLI
100800130225       //?Devono esserci tutti e 2 i colli
100900130225         IF  V01nclda > 0 and V01ncla = 0;
101000130225           ErrMessage  = *on;
101100130225           ErrGenerico = *on;
101200130225           PosCurNclA  = *on;
101300130225           V01msg      = skMsg(13);
101400130225           leavesr;
101500130225         ENDIF;
101600130225         IF  V01ncla > 0 and V01nclda = 0;
101700130225           ErrMessage  = *on;
101800130225           ErrGenerico = *on;
101900130225           PosCurNclDa = *on;
102000130225           V01msg      = skMsg(12);
102100130225           leavesr;
102200130225         ENDIF;
102300130225
102400130225       //?Congruenza tra i colli
102500130225         IF  V01ncla < V01nclda;
102600130225           ErrMessage  = *on;
102700130225           ErrGenerico = *on;
102800130225           PosCurNclA  = *on;
102900130225           V01msg      = skMsg(14);
103000130225           leavesr;
103100130225         ENDIF;
103200130225         parcod = V01nclda;
103300130225         parcoa = V01ncla;
103400130225
103500130225       //?TARIFFA/OFFERTA
103600130225         clear V01destar;
103700130225
103800130225       //?Cliente/Trattativa obbligatorio
103900130225         IF  V01ksc = *blanks;
104000130225           ErrMessage  = *on;
104100130225           ErrGenerico = *on;
104200130225           PosCurKsc   = *on;
104300130225           V01msg      = skMsg(15);
104400130225           leavesr;
104500130225         ENDIF;
104600130225
104700130225       //?Se Tariffa
104800130225         IF  V01taof = 'T';
104900130225         //?controllo KSC
105000130225           wKSC = V01ksc;
105100130225           exsr  ctrKSC;
105200130225           V01ksc  = wKSC;
105300130225           V01destar = wKSCd;
105400130225           IF  ErrGenerico;
105500130225             PosCurKsc = *on;
105600130225             leavesr;
105700130225           ENDIF;
105800130225         //?deve esistere almeno una tariffa
105900130225           TAMksc = %int(V01ksc);
106000130225           chain TAMksc TNTAM00L;
106100130225           IF  not %found(TNTAM00L);
106200130225             ErrMessage  = *on;
106300130225             ErrGenerico = *on;
106400130225             PosCurKsc   = *on;
106500130225             V01msg      = skMsg(03);
106600130225             leavesr;
106700130225           ENDIF;
106800130225         ENDIF;
106900130225
107000130225       //?Se Offerta
107100130225         IF  V01taof = 'O';
107200130225         //?controllo Trattativa
107300130225           VISnrv = %int(V01ksc);
107400130225           chain VISnrv TIVIS05L;
107500130225           IF  not %found(TIVIS05L);
107600130225             ErrMessage  = *on;
107700130225             ErrGenerico = *on;
107800130225             PosCurKsc   = *on;
107900130225             V01msg      = skMsg(16);
108000130225             leavesr;
108100130225           ENDIF;
108200130313           V01destar = VISrag;
108300130225         //?utente abilitato alla trattativa
108400130225           clear TNTAA1DS;
108500130225           ITAA1caut = '§utecli';
108600130225           ITAA1nrv  = VISnrv;
108700130225           kpjbu     = tntaa1ds;
108800130225           tntaa1c (kpjba);
108900130225           TNTAA1DS = kpjbu;
109000130225           IF  OTAA1fabi = 'N';
109100130313             PosCurKsc   = *on;
109200130225             ErrMessage  = *on;
109300130225             ErrGenerico = *on;
109400130225             V01msg      = skMsg(16);
109500130225           leavesr;
109600130225         ENDIF;
109700130225         //?deve esistere almeno una offerta
109800130225           chain VISnrv TIVOF11L;
109900130225           IF  not %found(TIVOF11L);
110000130225             ErrMessage  = *on;
110100130225             ErrGenerico = *on;
110200130225             PosCurKsc   = *on;
110300130225             V01msg      = skMsg(17);
110400130225             leavesr;
110500130225           ENDIF;
110600130225         ENDIF;
110700130225         partaof = V01taof;
110800130225         parksc  = %dec(V01ksc:7:0);
110900130226         parnrv  = parksc;
111000130225
111100130225       //?CODICE TARIFFA
111200130225       //?obbligatorio
111300130313         IF  V01ctr = *blanks and %scan('?' : V01prg) = 0;
111400130225           ErrMessage  = *on;
111500130225           ErrGenerico = *on;
111600130225           PosCurCtr   = *on;
111700130225           V01msg      = skMsg(18);
111800130225           leavesr;
111900130225         ENDIF;
112000130225       //?valido
112100130313         IF  V01ctr <> *blanks;
112200130313           exsr ctrCTR;
112300130313           IF  ErrGenerico;
112400130313             PosCurCtr = *on;
112500130313             leavesr;
112600130313           ENDIF;
112700130313           parctr = %dec(V01ctr:3:0);
112800130313         ENDIF;
112900130225
113000130225       //?PROGRESSIVO TARIFFA
113100130313         IF  %scan('?' : V01prg) > 0;
113200130313           exsr RicercaTAR;
113300130313           leavesr;
113400130313         ENDIF;
113500130226       //?Solo valori numerici
113600130226         IF  %check(digits:V01prg) > 0;
113700130226           ErrMessage  = *on;
113800130226           ErrGenerico = *on;
113900130226           PosCurPrg   = *on;
114000130226           V01msg      = skMsg(23);
114100130226           leavesr;
114200130226         ENDIF;
114300130225       //?Deve esistere
114400130225         TAMksc = %dec(V01ksc:7:0);
114500130225         TAMctr = %dec(V01ctr:3:0);
114600130226         TAMprg = %dec(V01prg:3:0);
114700130225         IF  V01taof = 'T';
114800130225           chain (TAMksc:TAMctr:TAMprg) TNTAM00L;
114900130225         ELSE;
115000130225           chain (TAMksc:TAMctr:TAMprg) TNOFM00L;
115100130225         ENDIF;
115200130225         IF  not %found;
115300130225           ErrMessage  = *on;
115400130225           ErrGenerico = *on;
115500130226           PosCurPrg   = *on;
115600130225           V01msg      = skMsg(20);
115700130225           leavesr;
115800130225         ENDIF;
115900130225       //?Non accetto tariffe FedEx
116000130225         dsTA01 = TAMflo;
116100130225         IF  §TAfed = 'S';
116200130225           ErrMessage  = *on;
116300130225           ErrGenerico = *on;
116400130225           PosCurPrg = *on;
116500130225           V01msg      = skMsg(21);
116600130225           leavesr;
116700130225         ENDIF;
116800130226         parprg = %dec(V01prg:3:0);
116900130228         parddt = TAMddt;
117000130225
117100130225       //?LINEA DI PARTENZA TARIFFA
117200130225         clear V01lnpd;
117300130225         IF  V01lnp <> *zeros;
117400130225           wLNP = V01lnp;
117500130225           exsr ctrLNP;
117600130225           V01lnp  = wLNP;
117700130225           V01lnpd = wLNPd;
117800130225           IF  ErrGenerico;
117900130225             PosCurLnp = *on;
118000130225             leavesr;
118100130225           ENDIF;
118200130226         //?deve esistere in tariffa
118300130226           IF  V01taof = 'T';
118400130226             chain (TAMksc:TAMctr:TAMprg:V01lnp) TITAD04L;
118500130226           ELSE;
118600130226             chain (TAMksc:TAMctr:TAMprg:V01lnp) TIOFD01L;
118700130226           ENDIF;
118800130226           IF  not %found;
118900130226             ErrMessage  = *on;
119000130226             ErrGenerico = *on;
119100130226             PosCurLnp   = *on;
119200130226             V01msg      = skMsg(22);
119300130226             leavesr;
119400130226           ENDIF;
119500130225         ENDIF;
119600130225         parlnp = V01lnp;
119700130225
119800130225         pardtcal = V01dtacal;
119900130225         pardet   = V01stpdet;
120000130225
120100121024       ENDSR;
120200130225
120300130225       //--------------------------------------------------------------
120400130225       //?Controllo cliente.
120500130225       //--------------------------------------------------------------
120600130225       BEGSR  ctrKSC;
120700130225
120800130225       //?Ricerca del cliente
120900130225         IF  %scan('?' : wKSC) > 0;
121000130225           ErrGenerico = *on;
121100130225           clear xCLIrds;
121200130225           DXCaut = OTAA1cabi;
121300130225           DXCcap = DUTkci;
121400130225           xclir (xCLIrds);
121500130225           ksa = DXCksc;
121600130225           dsksa = ksa(1);
121700130225           ksc(1) = dsksn;
121800130225           wKSC = %editc(ksc(1):'X');
121900130225         ENDIF;
122000130225
122100130225       //?Solo valori numerici
122200130225         IF  %check(digits:wKSC) > 0;
122300130225           ErrMessage  = *on;
122400130225           ErrGenerico = *on;
122500130225           V01msg      = skMsg(02);
122600130225           leavesr;
122700130225         ENDIF;
122800130225
122900130225       //?Cliente valido
123000130225         clear tibs69ds;
123100130225         I69kac = %dec(wKSC:7:0);
123200130225         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
123300130225         IF  O69err <> *blanks;
123400130225           ErrMessage  = *on;
123500130225           ErrGenerico = *on;
123600130225           V01msg      = skMsg(02);
123700130225           leavesr;
123800130225         ENDIF;
123900130225         wKSCd = ACOrag;
124000130225
124100130225       //?Finisco qua i controlli se sto verificando la tariffa ed è una cartello
124200130225         IF  wKSC = V01ksc and %subst(V01ksc:1:4) = '8888';
124300130225           leavesr;
124400130225         ENDIF;
124500130225
124600130225       //?Utente abilitato al cliente
124700130225         clear TNTAA1DS;
124800130225         ITAA1caut = '§utecli';
124900130225         ITAA1ksc  = %dec(wKSC:7:0);
125000130225         kpjbu     = tntaa1ds;
125100130225         tntaa1c (kpjba);
125200130225         TNTAA1DS = kpjbu;
125300130225         IF  OTAA1fabi = 'N';
125400130225           ErrMessage  = *on;
125500130225           ErrGenerico = *on;
125600130225           V01msg      = skMsg(02);
125700130225           leavesr;
125800130225         ENDIF;
125900130225
126000130225       ENDSR;
126100130225
126200130225       //--------------------------------------------------------------
126300130225       //?Controllo linea di partenza.
126400130225       //--------------------------------------------------------------
126500130225       BEGSR  ctrLNP;
126600130225
126700130225       //?Linea di partenza valida
126800130225         chain wLNP AZORG01L;
126900130225         IF  not %found(AZORG01L) or
127000130225            (ORGfva <> *blanks and ORGfag = 'V');
127100130225           ErrMessage  = *on;
127200130225           ErrGenerico = *on;
127300130225           V01msg      = skMsg(04);
127400130225           leavesr;
127500130225         ENDIF;
127600130225         wLNPd = ORGdes;
127700130225         OG143 = ORGde3;
127800130225
127900130225       //?NO Linea di partenza FedEx
128000130225         IF  §OGntw = 'FED';
128100130225           ErrMessage  = *on;
128200130225           ErrGenerico = *on;
128300130225           V01msg      = skMsg(05);
128400130225           leavesr;
128500130225         ENDIF;
128600130225
128700130225       ENDSR;
128800130225
128900130225       //--------------------------------------------------------------
129000130225       //?Controllo codice tassazione.
129100130225       //--------------------------------------------------------------
129200130225       BEGSR  ctrCTS;
129300130225
129400130225         TBLcod = 'CT';
129500130225
129600130225       //?Ricerca
129700130225         IF  %scan('?' : wCTS) > 0;
129800130225           ErrGenerico = *on;
129900130225           clear TRTB09DS;
130000130225           TB09fun = '1';
130100130225           TB09ord = '1';
130200130225           kpjbu = TRTB09DS;
130300130225           trtb09r (kpjba);
130400130225           TRTB09DS = kpjbu;
130500130225           wCTS = TB09cod;
130600130225         ENDIF;
130700130225
130800130225       //?Verifico che esista in tabella
130900130225         TBLkey = wCTS;
131000130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
131100130225         IF  not %found(TABEL00F) or TBLflg <> *blanks;
131200130225           ErrMessage  = *on;
131300130225           ErrGenerico = *on;
131400130225           V01msg      = skMsg(10);
131500130225           leavesr;
131600130225         ENDIF;
131700130225         dsCT = TBLuni;
131800130225
131900130225       //?Codice tassazione utilizzabile
132000130225         IF  §CTutc = 'N';
132100130225           ErrMessage  = *on;
132200130225           ErrGenerico = *on;
132300130225           V01msg      = skMsg(10);
132400130225           leavesr;
132500130225         ENDIF;
132600130225
132700130225       ENDSR;
132800130225
132900130225       //--------------------------------------------------------------
133000130225       //?Controllo codice tariffa.
133100130225       //--------------------------------------------------------------
133200130225       BEGSR  ctrCTR;
133300130225
133400130225       //?Ricerca
133500130225         IF  %scan('?' : V01ctr) > 0;
133600130313           exsr RicercaTAR;
133700130225           leavesr;
133800130225         ENDIF;
133900130225
134000130225       //?Solo valori numerici
134100130225         IF  %check(digits:V01ctr) > 0;
134200130225           ErrMessage  = *on;
134300130225           ErrGenerico = *on;
134400130225           V01msg      = skMsg(18);
134500130225           leavesr;
134600130225         ENDIF;
134700130225
134800130225       //?Verifico che sia utilizzabile
134900130225         TBLcod = 'TR';
135000130225         TBLkey = %subst(V01ctr:1:1);
135100130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
135200130225         IF  %found(TABEL00F);
135300130225           dsTR = TBLuni;
135400130225         //?Codice tassazione utilizzabile
135500130225           IF  §TRute = 'N';
135600130225             ErrMessage  = *on;
135700130225             ErrGenerico = *on;
135800130225             V01msg      = skMsg(19);
135900130225             leavesr;
136000130225           ENDIF;
136100130225         ENDIF;
136200130225
136300130225       ENDSR;
136400130313
136500130313       //--------------------------------------------------------------
136600130313       //?Ricerca Tariffa.
136700130313       //--------------------------------------------------------------
136800130313       BEGSR  RicercaTAR;
136900130313
137000130313         ErrGenerico = *on;
137100130313         clear parmta36;
137200130313         ta36ksc = %dec(V01ksc:7:0);
137300130313         IF  V01taof = 'T';
137400130313           ta36flg = '1';
137500130313         ELSE;
137600130313           ta36flg = '3';
137700130313         ENDIF;
137800130313         ta36vpr = '1';
137900130313         ta36kcc = DUTkci;
138000130313         kpjbu = parmta36;
138100130313         IF  V01taof = 'T';
138200130313           tnta36r (kpjba);
138300130313         ELSE;
138400130313           tnta36c (kpjba);
138500130313         ENDIF;
138600130313         parmta36 = kpjbu;
138700130313         V01ctr = ta36ctr;
138800130313         V01prg = ta36prg;
138900130313
139000130313       ENDSR;
139100121025
139200121025       //--------------------------------------------------------------
139300121025       //?Operazioni finali.                                           ?
139400121025       //--------------------------------------------------------------
139500121024       BEGSR  RoutEnd;
139600121025
139700121025         *inLR = *on;
139800100507
139900121025       //?Uscita dal pgm
140000121025         return;
140100121025
140200121025       ENDSR;
140300121025
140400121025      /end-free
140500121024
140600121024       //--------------------------------------------------------------
140700121024       //?Schiere a tempo di compilazione.
140800121024       //--------------------------------------------------------------
140900121024
141000130222** - skMSG ------------------------------------------------------------------*
141100130222Utente non abilitato alla funzione                                             01
141200130225Cliente errato o non in gestione all'utente                                    02
141300130225Tariffa inesistente                                                            03
141400130225Linea di partenza errata o non in gestione all'utente                          04
141500130225Linea di partenza FedEx non ammessa                                            05
141600130225Data errata                                                                    06
141700130225Data DAL inferiore alla prima data consentita da elaborare                     07
141800130225Data AL non può essere maggiore della data corrente                            08
141900130225La data AL deve essere maggiore o uguale della data DAL                        09
142000130225Codice tassazione errato o non accettabile                                     10
142100130225Peso spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                 11
142200130225Peso spedizioni a.. minore da..                                                12
142300130225Colli spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                13
142400130225Colli spedizioni a.. minore da..                                               14
142500130225Immettere cliente/trattativa                                                   15
142600130225Trattativa errata o non in gestione all'utente                                 16
142700130225Offerta inesistente                                                            17
142800130225Codice tariffa errato o non trovato                                            18
142900130225Tipo tariffa non gestibile dal C.A.T.                                          19
143000130225Tariffa/Offerta errata o inesistente                                           20
143100130225Tariffa FedEx non ammessa                                                      21
143200130226Linea di Partenza non trovata nella tariffa/offerta richiesta                  22
143300130226Progressivo errato                                                             23
