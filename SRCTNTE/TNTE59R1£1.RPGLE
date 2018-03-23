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
002700130225
002800130225       // -?File Tariffe
002900130225     fTNTAM00L  if   e           k disk
003000121025
003100121025       // -?File video
003200130222     fTNTE58D   cf   e             workstn
003300121107     f                                     indds(IndDspF)
003400121107     f                                     infds(InfDspF)
003500121025
003600121025       //--------------------------------------------------------------
003700121025       //?Definizione costanti.                                        ?
003800121025       //--------------------------------------------------------------
003900121025
004000130225       // -?Tasti funzionali a video
004100121024     d c_F01           c                   const(x'31')
004200121024     d c_F02           c                   const(x'32')
004300121024     d c_F03           c                   const(x'33')
004400121024     d c_F04           c                   const(x'34')
004500121024     d c_F05           c                   const(x'35')
004600121024     d c_F06           c                   const(x'36')
004700121024     d c_F07           c                   const(x'37')
004800121024     d c_F08           c                   const(x'38')
004900121024     d c_F09           c                   const(x'39')
005000121024     d c_F10           c                   const(x'3A')
005100121024     d c_F11           c                   const(x'3B')
005200121024     d c_F12           c                   const(x'3C')
005300121024     d c_F13           c                   const(x'B1')
005400121024     d c_F14           c                   const(x'B2')
005500121024     d c_F15           c                   const(x'B3')
005600121024     d c_F16           c                   const(x'B4')
005700121024     d c_F17           c                   const(x'B5')
005800121024     d c_F18           c                   const(x'B6')
005900121024     d c_F19           c                   const(x'B7')
006000121024     d c_F20           c                   const(x'B8')
006100121024     d c_F21           c                   const(x'B9')
006200121024     d c_F22           c                   const(x'BA')
006300121024     d c_F23           c                   const(x'BB')
006400121024     d c_F24           c                   const(x'BC')
006500121024     d c_Enter         c                   const(x'F1')
006600121024     d c_RollDown      c                   const(x'F4')
006700121024     d c_RollUp        c                   const(x'F5')
006800121024
006900121024     d Digits          c                   const('0123456789')
007000121025
007100121025       //--------------------------------------------------------------
007200121025       //?Definizione schiere.                                         ?
007300121025       //--------------------------------------------------------------
007400130225
007500130225       // -?Schiere per gestione codici cliente di ritorno da XCLIR
007600130225     d ksa             s              4    dim(30)
007700130225     d ksc             s              7  0 dim(30)
007800121025
007900121024      // -?Messaggi di errore
008000130225     d skMsg           s             78    dim(30) ctdata perrcd(1)
008100121025
008200121025       //--------------------------------------------------------------
008300121025       //?Definizione aree dati.                                       ?
008400121025       //--------------------------------------------------------------
008500121025
008600121025       // -?Dati utente?
008700121025     d §AzUte        e ds                  extname(AZUTE00F)
008800121025     d                                     dtaara
008900121025     d §DatiUte      e ds                  extname(dDatiUte)
009000121025     d                                     dtaara
009100121025
009200121025       //--------------------------------------------------------------
009300121025       //?Definizione strutture dati.                                  ?
009400121025       //--------------------------------------------------------------
009500121025
009600121025       // -?Status ds?
009700121025     d Status         sds
009800121025     d  SDSpgm           *proc
009900121025     d  SDSusr               254    263
010000121024
010100130225       // -?InfDS
010200121024     d InfDspF         ds
010300121024     d  dsp_aid              369    369a
010400121024
010500130225       // -?Indicatori su DspF
010600121024     d IndDspF         ds
010700130225       // -?Indicatori di Errore
010800121024     d  ErrMessage                    1n   overlay(IndDspF : 28)
010900130222     d  PosCurKsc01                   1n   overlay(IndDspF : 50)
011000130222     d  PosCurKsc02                   1n   overlay(IndDspF : 51)
011100130222     d  PosCurKsc03                   1n   overlay(IndDspF : 52)
011200130222     d  PosCurKsc04                   1n   overlay(IndDspF : 53)
011300130222     d  PosCurKsc05                   1n   overlay(IndDspF : 54)
011400130222     d  PosCurTar01                   1n   overlay(IndDspF : 55)
011500130222     d  PosCurTar02                   1n   overlay(IndDspF : 56)
011600130222     d  PosCurTar03                   1n   overlay(IndDspF : 57)
011700130222     d  PosCurTar04                   1n   overlay(IndDspF : 58)
011800130222     d  PosCurTar05                   1n   overlay(IndDspF : 59)
011900130222     d  PosCurLnp01                   1n   overlay(IndDspF : 60)
012000130222     d  PosCurLnp02                   1n   overlay(IndDspF : 61)
012100130222     d  PosCurLnp03                   1n   overlay(IndDspF : 62)
012200130222     d  PosCurLnp04                   1n   overlay(IndDspF : 63)
012300130222     d  PosCurLnp05                   1n   overlay(IndDspF : 64)
012400130222     d  PosCurDspDal                  1n   overlay(IndDspF : 65)
012500130222     d  PosCurDspAl                   1n   overlay(IndDspF : 66)
012600130222     d  PosCurCts01                   1n   overlay(IndDspF : 67)
012700130222     d  PosCurCts02                   1n   overlay(IndDspF : 68)
012800130222     d  PosCurCts03                   1n   overlay(IndDspF : 69)
012900130222     d  PosCurCts04                   1n   overlay(IndDspF : 70)
013000130222     d  PosCurCts05                   1n   overlay(IndDspF : 71)
013100130222     d  PosCurCts06                   1n   overlay(IndDspF : 72)
013200130222     d  PosCurCts07                   1n   overlay(IndDspF : 73)
013300130222     d  PosCurCts08                   1n   overlay(IndDspF : 74)
013400130222     d  PosCurCts09                   1n   overlay(IndDspF : 75)
013500130222     d  PosCurCts10                   1n   overlay(IndDspF : 76)
013600130222     d  PosCurPkgDa                   1n   overlay(IndDspF : 77)
013700130222     d  PosCurPkgA                    1n   overlay(IndDspF : 78)
013800130222     d  PosCurNclDa                   1n   overlay(IndDspF : 79)
013900130222     d  PosCurNclA                    1n   overlay(IndDspF : 80)
014000130222     d  PosCurKsc                     1n   overlay(IndDspF : 81)
014100130222     d  PosCurCtr                     1n   overlay(IndDspF : 82)
014200130222     d  PosCurPrg                     1n   overlay(IndDspF : 83)
014300130222     d  PosCurLnp                     1n   overlay(IndDspF : 84)
014400130225       // -?Indicatori di errore generico
014500121024     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014600121024
014700130225       // -?DS indicatori Dspf
014800121024     d WindDspF        ds                  inz
014900121024     d   WindDspFa             1     49    inz(*zero)
015000121024     d   WindDspFb            50     99    inz(*zero)
015100121024
015200130225       // -?DS controllo data
015300121024     d wlbdat          ds                  inz
015400121024     d  g02dat                 1      8  0
015500121024     d  g02inv                 9     16  0
015600121024     d  g02err                17     17
015700121024     d  g02tgi                18     22  0
015800130225
015900130225       // -?DS per gestione codici cliente di ritorno da XCLIR
016000130225     d                 ds
016100130225     d dsksn                   1      4p 0
016200130225     d dsksa                   1      4
016300130225
016400130225       // -?Parametri per il pgm chiamato
016500130225     d param           ds                  inz
016600130225     d  parks1                 1      7  0
016700130225     d  parks2                 8     14  0
016800130225     d  parks3                15     21  0
016900130225     d  parks4                22     28  0
017000130225     d  parks5                29     35  0
017100130225     d  parta1                36     38  0
017200130225     d  parta2                39     41  0
017300130225     d  parta3                42     44  0
017400130225     d  parta4                45     47  0
017500130225     d  parta5                48     50  0
017600130225     d  parln1                50     52  0
017700130225     d  parln2                53     55  0
017800130225     d  parln3                56     58  0
017900130225     d  parln4                59     61  0
018000130225     d  parln5                62     64  0
018100130225     d  pards1                65     72  0
018200130225     d  pards2                73     80  0
018300130225     d  parct1                81     82
018400130225     d  parct2                83     84
018500130225     d  parct3                85     86
018600130225     d  parct4                87     88
018700130225     d  parct5                89     90
018800130225     d  parct6                91     92
018900130225     d  parct7                93     94
019000130225     d  parct8                95     96
019100130225     d  parct9                97     98
019200130225     d  parct10               99    100
019300130225     d  parkgd               101    107  1
019400130225     d  parkga               108    114  1
019500130225     d  parcod               115    120  0
019600130225     d  parcoa               121    126  0
019700130225     d  partaof              127    127
019800130225     d  parksc               128    134  0
019900130225     d  parctr               135    137  0
020000130226     d  parprg               138    140  0
020100130226     d  parlnp               141    143  0
020200130226     d  pardtcal             144    144
020300130226     d  pardet               145    145
020400130225
020500130225       // -?Parametri per il pgm TNTA36R
020600130225     d parmta36        ds                  inz
020700130225     d  ta36ksc                6     12  0
020800130225     d  ta36ctr               13     15
020900130225     d  ta36flg               16     16
021000130225     d  ta36vpr               17     17
021100130225     d  ta36kcc               18     21  0
021200130225     d  ta36prg               37     39
021300121025
021400121025       // -?Parametri ricevuti
021500121025     d KPJBA         e ds
021600100507
021700121025       // -?Parametri per Reperimento dati utente?
021800121025     d TIBS34ds      e ds
021900130225
022000130225       // -?Parametri per Reperimento S.Informativo
022100130225     d TIBS55ds      e ds
022200130222
022300130222       // -?Abilitazioni utente
022400130222     d TNTAA1DS      e ds                  inz
022500130225
022600130225       // -?Tabella Codici Tassazione
022700130225     d TRTB09DS      e ds
022800130222
022900130222       // -?P.O. abilitati all'utente
023000130222     d TRUL31DS      e ds
023100130222     d POG                    10    759    DIM(250)
023200130222     d TRUL31DS2     e ds
023300130225
023400130225       // -?Ricerca clienti
023500130225     d xCLIrds       e ds
023600130225
023700130225       // -?Campo TAMFLO
023800130225     d dsTA01        e ds
023900130225
024000130225       // -?Organigramma
024100130225     d OG143         e ds
024200130225
024300130225       // -?DS Tabella CT - Codici Tassazione
024400130225     d dsCT          e ds
024500130225
024600130225       // -?DS Tabella TR - Codici Tariffa
024700130225     d dsTR          e ds
024800121025
024900121025       //--------------------------------------------------------------
025000121025       //?Definizione variabili globali.                               ?
025100121025       //--------------------------------------------------------------
025200121025
025300121025       // -?Flags booleani
025400121024     d wEnd            s               n   inz(*off)
025500121024     d wErrGrave       s               n   inz(*off)
025600121024     d wFine           s               n   inz(*off)
025700130222     d wInzD01         s               n   inz(*on)
025800121024
025900121024       // -?Campi Data
026000130222     d wData_ISO       s               d   datfmt(*iso)
026100130222     d wData_EUR       s               d   datfmt(*eur)
026200121024     d wOggi           s              8s 0 inz
026300130225
026400130225       // -?Campi di Comodo
026500130225     d sav_KNSIF       s                   like(KNSIF)
026600130225     d wCTS            s                   like(V01cts01)
026700130225     d wKSC            s                   like(V01ksc01)
026800130225     d wKSCd           s                   like(V01kscd01)
026900130225     d wLNP            s                   like(V01lnp01)
027000130225     d wLNPd           s                   like(V01lnpd01)
027100130225     d wDspDal         s                   like(V01dspdal)
027200130225     d wDspAl          s                   like(V01dspal)
027300121025
027400121025       //--------------------------------------------------------------
027500121025       //?Definizione procedure esterne.                     ?
027600121025       //--------------------------------------------------------------
027700121025
027800121025       // -?Sottomissione lavoro batch
027900121025     d bch10           pr                  extpgm('BCH10')
028000130225     d  kpjba                              likeds(KPJBA)
028100130225
028200130225       // -?Reperimento S.Informativo
028300130225     d tibs55r         pr                  extpgm('TIBS55R')
028400130225     d  tibs55ds                           likeds(tibs55ds)
028500130225
028600130225       // -?Ricerca offerte
028700130225     d tnta36c         pr                  extpgm('TNTA36C')
028800130225     d  kpjba                              likeds(kpjba)
028900130225
029000130225       // -?Ricerca tariffe
029100130225     d tnta36r         pr                  extpgm('TNTA36R')
029200130225     d  kpjba                              likeds(kpjba)
029300130225
029400130225       // -?Tabella Codici Tassazione
029500130225     d trtb09r         pr                  extpgm('TRTB09R')
029600130225     d  kpjba                              likeds(kpjba)
029700130225
029800130225       // -?Ricerca cliente
029900130225     d xclir           pr                  extpgm('XCLIR')
030000130225     d  xclirds                            likeds(XCLIRDS)
030100121025
030200121024       //--------------------------------------------------------------
030300121024       //?Definizione prototipi.
030400121024       //--------------------------------------------------------------
030500121024
030600121025      /copy gaitrasrc/srcprotopr,tibs34r
030700130225      /copy gaitrasrc/srcprotopi,tibs69r
030800130225      /copy gaitrasrc/srcprotopr,tibs69r
030900130222      /copy gaitrasrc/srcprotopr,tntaa1c
031000130222      /copy gaitrasrc/srcprotopr,trul31r
031100121024      /copy gaitrasrc/srcprotopr,xsrda8
031200121025
031300121025       //--------------------------------------------------------------
031400121025       //?Definizione key-list.                                        ?
031500121025       //--------------------------------------------------------------
031600121025
031700121025       //--------------------------------------------------------------
031800121025       //?M A I N - L I N E                                            ?
031900121025       //--------------------------------------------------------------
032000121025
032100121025     c     *Entry        plist
032200121025     c                   parm                    KPJBA
032300121025
032400121025      /free
032500121025
032600121024       //?Operazioni iniziali?
032700121025       exsr RoutInz;
032800121025
032900121024       //?Gestione vidata?
033000121024       DOW wFine = *off;
033100130222         exsr GesD01;
033200121025       ENDDO;
033300121025
033400121024       //?Operazioni finali?
033500121025       exsr RoutEnd;
033600121025
033700121025       //--------------------------------------------------------------
033800121025       //?Operazioni iniziali.                                         ?
033900121025       //--------------------------------------------------------------
034000121024       BEGSR  RoutInz;
034100121025
034200121024       //?Reperimento dati job?
034300121024         exsr  DatiJob;
034400130222
034500130222       //?Reperimento Autorizzazione utente Tariffe
034600130222         reset TNTAA1DS;
034700130222         ITAA1caut = '§utegtc';
034800130222         kpjbu     = TNTAA1DS;
034900130222         tntaa1c (kpjba);
035000130222         TNTAA1DS = kpjbu;
035100130222
035200130222         IF  OTAA1fabi = 'N';
035300130222           wErrGrave   = *on;
035400130222           ErrMessage  = *on;
035500130222           ErrGenerico = *on;
035600130222           V01msg = skMsg(01);
035700130222           leavesr;
035800130222         ENDIF;
035900130222
036000130222       //?Reperimento P.O. abilitati all'utente
036100130222         clear TRUL31DS;
036200130222         I31abi = OTAA1cabi;
036300130222         I31cdi = DUTdis;
036400130222         I31car = DUTare;
036500130222         I31cpo = DUTpou;
036600130222         callp trul31r (kpjba : trul31ds);
036700130222         IF  O31pog <= *zeros;
036800130222           wErrGrave   = *on;
036900130222           ErrMessage  = *on;
037000130222           ErrGenerico = *on;
037100130222           V01msg = skMsg(01);
037200130222           leavesr;
037300130222         ENDIF;
037400130222
037500130222       //?Impostazione campi "fissi"
037600130222         V01pgm = SDSpgm;
037700130222         TBLkut = 1;
037800121025
037900121025       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
038000121024         wOggi = %dec(%date());
038100121025
038200121025       ENDSR;
038300121025
038400121025       //--------------------------------------------------------------
038500121025       //?Reperimento Dati del job (Utente/Operativi).                 ?
038600121025       //--------------------------------------------------------------
038700121024       BEGSR  DatiJob;
038800121025
038900121025         in(E) §AzUte;
039000121025         IF  NOT %error;
039100121025           in(E) §DatiUte;
039200121025         ENDIF;
039300121025         IF  %error or RSut = *blanks;
039400121025           clear TIBS34DS;
039500121025           tibs34r ( TIBS34DS );
039600121025           in §AzUte;
039700121025           in §DatiUte;
039800121025         ENDIF;
039900121025
040000121025       ENDSR;
040100121024
040200121024       //--------------------------------------------------------------
040300130225       //?Gestione videata.
040400121024       //--------------------------------------------------------------
040500130225       BEGSR  GesD01;
040600121024
040700121024       //?Inizializzazione videata
040800130222         IF  wInzD01;
040900130222           exsr Inzd01;
041000130222           wInzD01 = *off;
041100121024         ENDIF;
041200121024
041300121024       //?Emissione videata
041400130222         write TE58T01;
041500130222         exfmt TE58D01;
041600121024         ErrMessage   = *off;
041700121024         ErrGenerico  = *off;
041800130222         clear V01msg;
041900121024
042000121024         SELECT;
042100121024
042200121024       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
042300121025           WHEN  wErrGrave;
042400121025             wFine = *on;
042500121024
042600121024       //?F3=Fine
042700121024           WHEN  dsp_aid = c_F03;
042800130222             exsr F03D01;
042900121024
043000121024       //?F6=Conferma
043100121024           WHEN  dsp_aid = c_F06;
043200130222             exsr CtrD01;
043300121024             IF  ErrGenerico;
043400121024               leavesr;
043500121024             ENDIF;
043600130222             exsr F06D01;
043700121024
043800121024       //?Enter
043900121024           OTHER;
044000130222             exsr CtrD01;
044100121024             IF  ErrGenerico;
044200121024               leavesr;
044300121024             ENDIF;
044400121024
044500121024         ENDSL;
044600121024       ENDSR;
044700121024
044800121024       //--------------------------------------------------------------
044900121024       //?Inizializzazione videata.
045000121024       //--------------------------------------------------------------
045100130222       BEGSR InzD01;
045200121024
045300130222         clear TE58D01;
045400130222
045500130222       //?Codici tariffa tutti a 999
045600130225         V01tar01 = *all'9';
045700130225         V01tar02 = *all'9';
045800130225         V01tar03 = *all'9';
045900130225         V01tar04 = *all'9';
046000130225         V01tar05 = *all'9';
046100130222
046200130222       //?LNP tutte a 999
046300130225         V01lnp01 = *all'9';
046400130225         V01lnp02 = *all'9';
046500130225         V01lnp03 = *all'9';
046600130225         V01lnp04 = *all'9';
046700130225         V01lnp05 = *all'9';
046800130222
046900130222       //?Data Spedizione DAL
047000130222         wData_ISO = %date(wOggi:*iso);
047100130222       //?Oggi - 6 mesi se distretto
047200130222         IF  O31dig <> *blanks;
047300130222           wData_EUR = wData_ISO - %months(6);
047400130222         ELSE;
047500130222       //?Oggi - 2 mesi se NON distretto
047600130222           wData_EUR = wData_ISO - %months(2);
047700130225           wData_ISO = wData_EUR;
047800130222         ENDIF;
047900130225         V01dspdal = %dec(wData_EUR);
048000130225         wDspDal = %dec(wData_ISO);
048100130222
048200130222       //?Data Spedizione AL oggi - 1 giorno
048300130225         wData_ISO = %date(wOggi:*iso);
048400130222         wData_EUR = wData_ISO - %days(1);
048500130225         wData_ISO = wData_EUR;
048600130225         V01dspal = %dec(wData_EUR);
048700130225         wDspAl = %dec(wData_ISO);
048800130222
048900130222       //?Imposto T=Tariffa
049000130225         V01taof = 'T';
049100121205
049200130222       //?Imposto S=Spedizione in Data Calcolo
049300130225         V01dtacal = 'S';
049400130222
049500130222       //?Imposto N=Stampa dettaglio
049600130225         V01stpdet = 'N';
049700121024
049800121024       ENDSR;
049900121024
050000121024       //--------------------------------------------------------------
050100121024       //?Gestione tasto funzionale F3.
050200121024       //?F3=Fine
050300121024       //--------------------------------------------------------------
050400130225       BEGSR F03D01;
050500121024
050600121024       //?Chiusura del programma
050700121024         wFine = *on;
050800121024
050900121024       ENDSR;
051000121024
051100121024       //--------------------------------------------------------------
051200121024       //?Gestione tasto funzionale F6 da videata D01
051300121024       //?F6=Conferma
051400121024       //--------------------------------------------------------------
051500130225       BEGSR F06D01;
051600121205
051700130226       //?Richiamo pgm per cercare il S.Informativo nel quale sottomettere il lavoro
051800130225         clear TIBS55DS;
051900130225         I50tla = 'L';
052000130225         I50ppo = DUTpou;
052100130225         I50apo = 046;
052200130225         tibs55r (TIBS55DS);
052300121205
052400130225       //?Salvo il S.Informativo originale
052500130225         sav_KNSIF = KNSIF;
052600130225
052700130225       //?Imposto kpjba
052800130225         KNSIF = O50asi;
052900130225         KCOAZ = 'TE59';
053000130225         BCH10 (kpjba);
053100130225
053200130225       //?Reimposto il S.Informativo originale
053300130225         KNSIF = sav_KNSIF;
053400121025
053500121025       //?Chiusura del programma
053600121025         wFine = *on;
053700121024
053800121024       ENDSR;
053900121024
054000121024       //--------------------------------------------------------------
054100121024       //?Controllo videata.
054200121024       //--------------------------------------------------------------
054300130225       BEGSR CtrD01;
054400121024
054500121024         WindDspF = IndDspF;
054600121024         reset WindDspFb;
054700121024         IndDspF  = WindDspF;
054800130225
054900130225       //?CLIENTI
055000130225         clear V01kscd01;
055100130225         clear V01kscd02;
055200130225         clear V01kscd03;
055300130225         clear V01kscd04;
055400130225         clear V01kscd05;
055500130225
055600130225       //?Primo -- obbligatorio
055700130225         IF  V01ksc01 = *blanks;
055800130225           ErrMessage  = *on;
055900130225           ErrGenerico = *on;
056000130225           PosCurKsc01 = *on;
056100130225           V01msg      = skMsg(02);
056200130225           leavesr;
056300130225         ENDIF;
056400130225
056500130225         wKSC = V01ksc01;
056600130225         exsr  ctrKSC;
056700130225         V01ksc01  = wKSC;
056800130225         V01kscd01 = wKSCd;
056900130225         IF  ErrGenerico;
057000130225           PosCurKsc01 = *on;
057100130225           leavesr;
057200130225         ENDIF;
057300130225         parks1    = %dec(wKSC:7:0);
057400130225
057500130225       //?Secondo
057600130225         IF  V01ksc02 <> *blanks and V01ksc02 <> *zeros;
057700130225           wKSC = V01ksc02;
057800130225           exsr  ctrKSC;
057900130225           V01ksc02  = wKSC;
058000130225           V01kscd02 = wKSCd;
058100130225           IF  ErrGenerico;
058200130225             PosCurKsc02 = *on;
058300130225             leavesr;
058400130225           ENDIF;
058500130225           parks2    = %dec(wKSC:7:0);
058600130225         ENDIF;
058700121024
058800130225       //?Terzo
058900130225         IF  V01ksc03 <> *blanks and V01ksc03 <> *zeros;
059000130225           wKSC = V01ksc03;
059100130225           exsr  ctrKSC;
059200130225           V01ksc03  = wKSC;
059300130225           V01kscd03 = wKSCd;
059400130225           IF  ErrGenerico;
059500130225             PosCurKsc03 = *on;
059600130225             leavesr;
059700130225           ENDIF;
059800130225           parks3    = %dec(wKSC:7:0);
059900130225         ENDIF;
060000130225
060100130225       //?Quarto
060200130225         IF  V01ksc04 <> *blanks and V01ksc04 <> *zeros;
060300130225           wKSC = V01ksc04;
060400130225           exsr  ctrKSC;
060500130225           V01ksc04  = wKSC;
060600130225           V01kscd04 = wKSCd;
060700130225           IF  ErrGenerico;
060800130225             PosCurKsc04 = *on;
060900130225             leavesr;
061000130225           ENDIF;
061100130225           parks4    = %dec(wKSC:7:0);
061200130225         ENDIF;
061300130225
061400130225       //?Quinto
061500130225         IF  V01ksc05 <> *blanks and V01ksc05 <> *zeros;
061600130225           wKSC = V01ksc05;
061700130225           exsr  ctrKSC;
061800130225           IF  ErrGenerico;
061900130225             PosCurKsc05 = *on;
062000130225             leavesr;
062100130225           ENDIF;
062200130225           V01ksc05  = wKSC;
062300130225           V01kscd05 = wKSCd;
062400130225           parks5    = %dec(wKSC:7:0);
062500130225         ENDIF;
062600130225
062700130225       //?TARIFFE
062800130225       //?Prima
062900130225         IF  V01tar01 <> 999;
063000130225           TAMksc = %int(V01ksc01);
063100130225           chain (TAMksc:V01tar01) TNTAM00L;
063200130225           IF  not %found(TNTAM00L);
063300130225             ErrMessage  = *on;
063400130225             ErrGenerico = *on;
063500130225             PosCurTar01 = *on;
063600130225             V01msg      = skMsg(03);
063700130225             leavesr;
063800130225           ENDIF;
063900130225         ENDIF;
064000130225         parta1 = V01tar01;
064100130225
064200130225       //?Seconda
064300130225         IF  V01tar02 <> 999;
064400130225           TAMksc = %int(V01ksc02);
064500130225           chain (TAMksc:V01tar02) TNTAM00L;
064600130225           IF  not %found(TNTAM00L);
064700130225             ErrMessage  = *on;
064800130225             ErrGenerico = *on;
064900130225             PosCurTar02 = *on;
065000130225             V01msg      = skMsg(03);
065100130225             leavesr;
065200130225           ENDIF;
065300130225         ENDIF;
065400130225         parta2 = V01tar02;
065500130225
065600130225       //?Terza
065700130225         IF  V01tar03 <> 999;
065800130225           TAMksc = %int(V01ksc03);
065900130225           chain (TAMksc:V01tar03) TNTAM00L;
066000130225           IF  not %found(TNTAM00L);
066100130225             ErrMessage  = *on;
066200130225             ErrGenerico = *on;
066300130225             PosCurTar03 = *on;
066400130225             V01msg      = skMsg(03);
066500130225             leavesr;
066600130225           ENDIF;
066700130225         ENDIF;
066800130225         parta3 = V01tar03;
066900130225
067000130225       //?Quarta
067100130225         IF  V01tar04 <> 999;
067200130225           TAMksc = %int(V01ksc04);
067300130225           chain (TAMksc:V01tar04) TNTAM00L;
067400130225           IF  not %found(TNTAM00L);
067500130225             ErrMessage  = *on;
067600130225             ErrGenerico = *on;
067700130225             PosCurTar04 = *on;
067800130225             V01msg      = skMsg(03);
067900130225             leavesr;
068000130225           ENDIF;
068100130225         ENDIF;
068200130225         parta4 = V01tar04;
068300130225
068400130225       //?Quinta
068500130225         IF  V01tar05 <> 999;
068600130225           TAMksc = %int(V01ksc05);
068700130225           chain (TAMksc:V01tar05) TNTAM00L;
068800130225           IF  not %found(TNTAM00L);
068900130225             ErrMessage  = *on;
069000130225             ErrGenerico = *on;
069100130225             PosCurTar05 = *on;
069200130225             V01msg      = skMsg(03);
069300130225             leavesr;
069400130225           ENDIF;
069500130225         ENDIF;
069600130225         parta5 = V01tar05;
069700130225
069800130225       //?LINEA DI PARTENZA
069900130225         clear V01lnpd01;
070000130225         clear V01lnpd02;
070100130225         clear V01lnpd03;
070200130225         clear V01lnpd04;
070300130225         clear V01lnpd05;
070400130225
070500130225       //?Prima
070600130225         IF  V01lnp01 <> 999;
070700130225           wLNP = V01lnp01;
070800130225           exsr ctrLNP;
070900130225           V01lnp01  = wLNP;
071000130225           V01lnpd01 = wLNPd;
071100130225           IF  ErrGenerico;
071200130225             PosCurLnp01 = *on;
071300130225             leavesr;
071400130225           ENDIF;
071500130225         ENDIF;
071600130225         parln1 = V01lnp01;
071700130225
071800130225       //?Seconda
071900130225         IF  V01lnp02 <> 999;
072000130225           wLNP = V01lnp02;
072100130225           exsr ctrLNP;
072200130225           V01lnp02  = wLNP;
072300130225           V01lnpd02 = wLNPd;
072400130225           IF  ErrGenerico;
072500130225             PosCurLnp02 = *on;
072600130225             leavesr;
072700130225           ENDIF;
072800130225         ENDIF;
072900130225         parln2 = V01lnp02;
073000130225
073100130225       //?Terza
073200130225         IF  V01lnp03 <> 999;
073300130225           wLNP = V01lnp03;
073400130225           exsr ctrLNP;
073500130225           V01lnp03  = wLNP;
073600130225           V01lnpd03 = wLNPd;
073700130225           IF  ErrGenerico;
073800130225             PosCurLnp03 = *on;
073900130225             leavesr;
074000130225           ENDIF;
074100130225         ENDIF;
074200130225         parln3 = V01lnp03;
074300130225
074400130225       //?Quarta
074500130225         IF  V01lnp04 <> 999;
074600130225           wLNP = V01lnp04;
074700130225           exsr ctrLNP;
074800130225           V01lnp04  = wLNP;
074900130225           V01lnpd04 = wLNPd;
075000130225           IF  ErrGenerico;
075100130225             PosCurLnp04 = *on;
075200130225             leavesr;
075300130225           ENDIF;
075400130225         ENDIF;
075500130225         parln4 = V01lnp04;
075600130225
075700130225       //?Quinta
075800130225         IF  V01lnp05 <> 999;
075900130225           wLNP = V01lnp05;
076000130225           exsr ctrLNP;
076100130225           V01lnp05  = wLNP;
076200130225           V01lnpd05 = wLNPd;
076300130225           IF  ErrGenerico;
076400130225             PosCurLnp05 = *on;
076500130225             leavesr;
076600130225           ENDIF;
076700130225         ENDIF;
076800130225         parln5 = V01lnp05;
076900130225
077000130225       //?Data spedizione dal
077100130225         clear wlbdat;
077200130225         g02dat = V01dspdal;
077300130225         xsrda8(wlbdat);
077400130225         IF  g02err    = '1';
077500130225           ErrMessage   = *on;
077600130225           ErrGenerico  = *on;
077700130225           PosCurDspDal = *on;
077800130225           V01msg       = skMsg(06);
077900130225           leavesr;
078000130225         ENDIF;
078100130225         V01dspdal = g02dat;
078200130225         IF  g02inv < wDspDal;
078300130225           ErrMessage   = *on;
078400130225           ErrGenerico  = *on;
078500130225           PosCurDspDal = *on;
078600130225           V01msg       = skMsg(07);
078700130225           leavesr;
078800130225         ENDIF;
078900130225         pards1 = g02inv;
079000130225
079100130225       //?Data spedizione al
079200130225         clear wlbdat;
079300130225         g02dat = V01dspal;
079400130225         xsrda8(wlbdat);
079500130225         IF  g02err    = '1';
079600130225           ErrMessage   = *on;
079700130225           ErrGenerico  = *on;
079800130225           PosCurDspAl  = *on;
079900130225           V01msg       = skMsg(06);
080000130225           leavesr;
080100130225         ENDIF;
080200130225         V01dspal = g02dat;
080300130225         IF  g02inv > wDspAl;
080400130225           ErrMessage   = *on;
080500130225           ErrGenerico  = *on;
080600130225           PosCurDspAl = *on;
080700130225           V01msg       = skMsg(08);
080800130225           leavesr;
080900130225         ENDIF;
081000130225         pards2 = g02inv;
081100130225
081200130225       //?Devono esserci tutte e 2 le date
081300130225         IF  V01dspdal > 0 and V01dspal = 0;
081400130225           ErrMessage   = *on;
081500130225           ErrGenerico  = *on;
081600130225           PosCurDspAl  = *on;
081700130225           V01msg       = skMsg(06);
081800130225           leavesr;
081900130225         ENDIF;
082000130225         IF  V01dspal > 0 and V01dspdal = 0;
082100130225           ErrMessage   = *on;
082200130225           ErrGenerico  = *on;
082300130225           PosCurDspDal = *on;
082400130225           V01msg       = skMsg(06);
082500130225           leavesr;
082600130225         ENDIF;
082700130225
082800130225       //?Congruenza tra date
082900130225         IF  wDspAl < wDspDal;
083000130225           ErrMessage   = *on;
083100130225           ErrGenerico  = *on;
083200130225           PosCurDspAl  = *on;
083300130225           V01msg       = skMsg(09);
083400130225           leavesr;
083500130225         ENDIF;
083600130225
083700130225       //?CODICI TASSAZIONE
083800130225       //?Primo
083900130225         IF  V01cts01 <> *blanks;
084000130225           wCTS = V01cts01;
084100130225           exsr ctrCTS;
084200130225           V01cts01 = wCTS;
084300130225           parct1   = wCTS;
084400130225           IF  ErrGenerico;
084500130225             PosCurCts01 = *on;
084600130225             leavesr;
084700130225           ENDIF;
084800130225         ENDIF;
084900130225
085000130225       //?Secondo
085100130225         IF  V01cts02 <> *blanks;
085200130225           wCTS = V01cts02;
085300130225           exsr ctrCTS;
085400130225           V01cts02 = wCTS;
085500130225           parct2   = wCTS;
085600130225           IF  ErrGenerico;
085700130225             PosCurCts02 = *on;
085800130225             leavesr;
085900130225           ENDIF;
086000130225         ENDIF;
086100130225
086200130225       //?Terzo
086300130225         IF  V01cts03 <> *blanks;
086400130225           wCTS = V01cts03;
086500130225           exsr ctrCTS;
086600130225           V01cts03 = wCTS;
086700130225           parct3   = wCTS;
086800130225           IF  ErrGenerico;
086900130225             PosCurCts03 = *on;
087000130225             leavesr;
087100130225           ENDIF;
087200130225         ENDIF;
087300130225
087400130225       //?Quarto
087500130225         IF  V01cts04 <> *blanks;
087600130225           wCTS = V01cts04;
087700130225           exsr ctrCTS;
087800130225           V01cts04 = wCTS;
087900130225           parct4   = wCTS;
088000130225           IF  ErrGenerico;
088100130225             PosCurCts04 = *on;
088200130225             leavesr;
088300130225           ENDIF;
088400130225         ENDIF;
088500130225
088600130225       //?Quinto
088700130225         IF  V01cts05 <> *blanks;
088800130225           wCTS = V01cts05;
088900130225           exsr ctrCTS;
089000130225           V01cts05 = wCTS;
089100130225           parct5   = wCTS;
089200130225           IF  ErrGenerico;
089300130225             PosCurCts05 = *on;
089400130225             leavesr;
089500130225           ENDIF;
089600130225         ENDIF;
089700130225
089800130225       //?Sesto
089900130225         IF  V01cts06 <> *blanks;
090000130225           wCTS = V01cts06;
090100130225           exsr ctrCTS;
090200130225           V01cts06 = wCTS;
090300130225           parct6   = wCTS;
090400130225           IF  ErrGenerico;
090500130225             PosCurCts06 = *on;
090600130225             leavesr;
090700130225           ENDIF;
090800130225         ENDIF;
090900130225
091000130225       //?Settimo
091100130225         IF  V01cts07 <> *blanks;
091200130225           wCTS = V01cts07;
091300130225           exsr ctrCTS;
091400130225           V01cts07 = wCTS;
091500130225           parct7   = wCTS;
091600130225           IF  ErrGenerico;
091700130225             PosCurCts07 = *on;
091800130225             leavesr;
091900130225           ENDIF;
092000130225         ENDIF;
092100130225
092200130225       //?Ottavo
092300130225         IF  V01cts08 <> *blanks;
092400130225           wCTS = V01cts08;
092500130225           exsr ctrCTS;
092600130225           V01cts08 = wCTS;
092700130225           parct8   = wCTS;
092800130225           IF  ErrGenerico;
092900130225             PosCurCts08 = *on;
093000130225             leavesr;
093100130225           ENDIF;
093200130225         ENDIF;
093300130225
093400130225       //?Nono
093500130225         IF  V01cts09 <> *blanks;
093600130225           wCTS = V01cts09;
093700130225           exsr ctrCTS;
093800130225           V01cts09 = wCTS;
093900130225           parct9   = wCTS;
094000130225           IF  ErrGenerico;
094100130225             PosCurCts09 = *on;
094200130225             leavesr;
094300130225           ENDIF;
094400130225         ENDIF;
094500130225
094600130225       //?Decimo
094700130225         IF  V01cts10 <> *blanks;
094800130225           wCTS = V01cts10;
094900130225           exsr ctrCTS;
095000130225           V01cts10 = wCTS;
095100130225           parct10  = wCTS;
095200130225           IF  ErrGenerico;
095300130225             PosCurCts10 = *on;
095400130225             leavesr;
095500130225           ENDIF;
095600130225         ENDIF;
095700130225
095800130225       //?PESO
095900130225       //?Devono esserci tutti e 2 i pesi
096000130225         IF  V01pkgda > 0 and V01pkga = 0;
096100130225           ErrMessage  = *on;
096200130225           ErrGenerico = *on;
096300130225           PosCurPkgA  = *on;
096400130225           V01msg      = skMsg(11);
096500130225           leavesr;
096600130225         ENDIF;
096700130225         IF  V01pkga > 0 and V01pkgda = 0;
096800130225           ErrMessage  = *on;
096900130225           ErrGenerico = *on;
097000130225           PosCurPkgDa = *on;
097100130225           V01msg      = skMsg(11);
097200130225           leavesr;
097300130225         ENDIF;
097400130225
097500130225       //?Congruenza tra i pesi
097600130225         IF  V01pkga < V01pkgda;
097700130225           ErrMessage  = *on;
097800130225           ErrGenerico = *on;
097900130225           PosCurPkgA  = *on;
098000130225           V01msg      = skMsg(12);
098100130225           leavesr;
098200130225         ENDIF;
098300130225         parkgd = V01pkgda;
098400130225         parkga = V01pkga;
098500130225
098600130225       //?COLLI
098700130225       //?Devono esserci tutti e 2 i colli
098800130225         IF  V01nclda > 0 and V01ncla = 0;
098900130225           ErrMessage  = *on;
099000130225           ErrGenerico = *on;
099100130225           PosCurNclA  = *on;
099200130225           V01msg      = skMsg(13);
099300130225           leavesr;
099400130225         ENDIF;
099500130225         IF  V01ncla > 0 and V01nclda = 0;
099600130225           ErrMessage  = *on;
099700130225           ErrGenerico = *on;
099800130225           PosCurNclDa = *on;
099900130225           V01msg      = skMsg(12);
100000130225           leavesr;
100100130225         ENDIF;
100200130225
100300130225       //?Congruenza tra i colli
100400130225         IF  V01ncla < V01nclda;
100500130225           ErrMessage  = *on;
100600130225           ErrGenerico = *on;
100700130225           PosCurNclA  = *on;
100800130225           V01msg      = skMsg(14);
100900130225           leavesr;
101000130225         ENDIF;
101100130225         parcod = V01nclda;
101200130225         parcoa = V01ncla;
101300130225
101400130225       //?TARIFFA/OFFERTA
101500130225         clear V01destar;
101600130225
101700130225       //?Cliente/Trattativa obbligatorio
101800130225         IF  V01ksc = *blanks;
101900130225           ErrMessage  = *on;
102000130225           ErrGenerico = *on;
102100130225           PosCurKsc   = *on;
102200130225           V01msg      = skMsg(15);
102300130225           leavesr;
102400130225         ENDIF;
102500130225
102600130225       //?Se Tariffa
102700130225         IF  V01taof = 'T';
102800130225         //?controllo KSC
102900130225           wKSC = V01ksc;
103000130225           exsr  ctrKSC;
103100130225           V01ksc  = wKSC;
103200130225           V01destar = wKSCd;
103300130225           IF  ErrGenerico;
103400130225             PosCurKsc = *on;
103500130225             leavesr;
103600130225           ENDIF;
103700130225         //?deve esistere almeno una tariffa
103800130225           TAMksc = %int(V01ksc);
103900130225           chain TAMksc TNTAM00L;
104000130225           IF  not %found(TNTAM00L);
104100130225             ErrMessage  = *on;
104200130225             ErrGenerico = *on;
104300130225             PosCurKsc   = *on;
104400130225             V01msg      = skMsg(03);
104500130225             leavesr;
104600130225           ENDIF;
104700130225         ENDIF;
104800130225
104900130225       //?Se Offerta
105000130225         IF  V01taof = 'O';
105100130225         //?controllo Trattativa
105200130225           VISnrv = %int(V01ksc);
105300130225           chain VISnrv TIVIS05L;
105400130225           IF  not %found(TIVIS05L);
105500130225             ErrMessage  = *on;
105600130225             ErrGenerico = *on;
105700130225             PosCurKsc   = *on;
105800130225             V01msg      = skMsg(16);
105900130225             leavesr;
106000130225           ENDIF;
106100130225         //?utente abilitato alla trattativa
106200130225           clear TNTAA1DS;
106300130225           ITAA1caut = '§utecli';
106400130225           ITAA1nrv  = VISnrv;
106500130225           kpjbu     = tntaa1ds;
106600130225           tntaa1c (kpjba);
106700130225           TNTAA1DS = kpjbu;
106800130225           IF  OTAA1fabi = 'N';
106900130225             ErrMessage  = *on;
107000130225             ErrGenerico = *on;
107100130225             V01msg      = skMsg(16);
107200130225           leavesr;
107300130225         ENDIF;
107400130225         //?deve esistere almeno una offerta
107500130225           chain VISnrv TIVOF11L;
107600130225           IF  not %found(TIVOF11L);
107700130225             ErrMessage  = *on;
107800130225             ErrGenerico = *on;
107900130225             PosCurKsc   = *on;
108000130225             V01msg      = skMsg(17);
108100130225             leavesr;
108200130225           ENDIF;
108300130225         ENDIF;
108400130225         partaof = V01taof;
108500130225         parksc  = %dec(V01ksc:7:0);
108600130225
108700130225       //?CODICE TARIFFA
108800130225       //?obbligatorio
108900130225         IF  V01ctr = *blanks;
109000130225           ErrMessage  = *on;
109100130225           ErrGenerico = *on;
109200130225           PosCurCtr   = *on;
109300130225           V01msg      = skMsg(18);
109400130225           leavesr;
109500130225         ENDIF;
109600130225       //?valido
109700130225         exsr ctrCTR;
109800130225         IF  ErrGenerico;
109900130225           PosCurCtr = *on;
110000130225           leavesr;
110100130225         ENDIF;
110200130225         parctr = %dec(V01ctr:3:0);
110300130225
110400130225       //?PROGRESSIVO TARIFFA
110500130225       //?Deve esistere
110600130225         TAMksc = %dec(V01ksc:7:0);
110700130225         TAMctr = %dec(V01ctr:3:0);
110800130225         TAMprg = V01prg;
110900130225         IF  V01taof = 'T';
111000130225           chain (TAMksc:TAMctr:TAMprg) TNTAM00L;
111100130225         ELSE;
111200130225           chain (TAMksc:TAMctr:TAMprg) TNOFM00L;
111300130225         ENDIF;
111400130225         IF  not %found;
111500130225           ErrMessage  = *on;
111600130225           ErrGenerico = *on;
111700130225           PosCurPrg = *on;
111800130225           V01msg      = skMsg(20);
111900130225           leavesr;
112000130225         ENDIF;
112100130225       //?Non accetto tariffe FedEx
112200130225         dsTA01 = TAMflo;
112300130225         IF  §TAfed = 'S';
112400130225           ErrMessage  = *on;
112500130225           ErrGenerico = *on;
112600130225           PosCurPrg = *on;
112700130225           V01msg      = skMsg(21);
112800130225           leavesr;
112900130225         ENDIF;
113000130225         parprg = V01prg;
113100130225
113200130225       //?LINEA DI PARTENZA TARIFFA
113300130225         clear V01lnpd;
113400130225         IF  V01lnp <> *zeros;
113500130225           wLNP = V01lnp;
113600130225           exsr ctrLNP;
113700130225           V01lnp  = wLNP;
113800130225           V01lnpd = wLNPd;
113900130225           IF  ErrGenerico;
114000130225             PosCurLnp = *on;
114100130225             leavesr;
114200130225           ENDIF;
114300130225         ENDIF;
114400130225         parlnp = V01lnp;
114500130225
114600130225         pardtcal = V01dtacal;
114700130225         pardet   = V01stpdet;
114800130225
114900121024       ENDSR;
115000130225
115100130225       //--------------------------------------------------------------
115200130225       //?Controllo cliente.
115300130225       //--------------------------------------------------------------
115400130225       BEGSR  ctrKSC;
115500130225
115600130225       //?Ricerca del cliente
115700130225         IF  %scan('?' : wKSC) > 0;
115800130225           ErrGenerico = *on;
115900130225           clear xCLIrds;
116000130225           DXCaut = OTAA1cabi;
116100130225           DXCcap = DUTkci;
116200130225           xclir (xCLIrds);
116300130225           ksa = DXCksc;
116400130225           dsksa = ksa(1);
116500130225           ksc(1) = dsksn;
116600130225           wKSC = %editc(ksc(1):'X');
116700130225         ENDIF;
116800130225
116900130225       //?Solo valori numerici
117000130225         IF  %check(digits:wKSC) > 0;
117100130225           ErrMessage  = *on;
117200130225           ErrGenerico = *on;
117300130225           V01msg      = skMsg(02);
117400130225           leavesr;
117500130225         ENDIF;
117600130225
117700130225       //?Cliente valido
117800130225         clear tibs69ds;
117900130225         I69kac = %dec(wKSC:7:0);
118000130225         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
118100130225         IF  O69err <> *blanks;
118200130225           ErrMessage  = *on;
118300130225           ErrGenerico = *on;
118400130225           V01msg      = skMsg(02);
118500130225           leavesr;
118600130225         ENDIF;
118700130225         wKSCd = ACOrag;
118800130225
118900130225       //?Finisco qua i controlli se sto verificando la tariffa ed è una cartello
119000130225         IF  wKSC = V01ksc and %subst(V01ksc:1:4) = '8888';
119100130225           leavesr;
119200130225         ENDIF;
119300130225
119400130225       //?Utente abilitato al cliente
119500130225         clear TNTAA1DS;
119600130225         ITAA1caut = '§utecli';
119700130225         ITAA1ksc  = %dec(wKSC:7:0);
119800130225         kpjbu     = tntaa1ds;
119900130225         tntaa1c (kpjba);
120000130225         TNTAA1DS = kpjbu;
120100130225         IF  OTAA1fabi = 'N';
120200130225           ErrMessage  = *on;
120300130225           ErrGenerico = *on;
120400130225           V01msg      = skMsg(02);
120500130225           leavesr;
120600130225         ENDIF;
120700130225
120800130225       ENDSR;
120900130225
121000130225       //--------------------------------------------------------------
121100130225       //?Controllo linea di partenza.
121200130225       //--------------------------------------------------------------
121300130225       BEGSR  ctrLNP;
121400130225
121500130225       //?Linea di partenza valida
121600130225         chain wLNP AZORG01L;
121700130225         IF  not %found(AZORG01L) or
121800130225            (ORGfva <> *blanks and ORGfag = 'V');
121900130225           ErrMessage  = *on;
122000130225           ErrGenerico = *on;
122100130225           V01msg      = skMsg(04);
122200130225           leavesr;
122300130225         ENDIF;
122400130225         wLNPd = ORGdes;
122500130225         OG143 = ORGde3;
122600130225
122700130225       //?NO Linea di partenza FedEx
122800130225         IF  §OGntw = 'FED';
122900130225           ErrMessage  = *on;
123000130225           ErrGenerico = *on;
123100130225           V01msg      = skMsg(05);
123200130225           leavesr;
123300130225         ENDIF;
123400130225
123500130225       ENDSR;
123600130225
123700130225       //--------------------------------------------------------------
123800130225       //?Controllo codice tassazione.
123900130225       //--------------------------------------------------------------
124000130225       BEGSR  ctrCTS;
124100130225
124200130225         TBLcod = 'CT';
124300130225
124400130225       //?Ricerca
124500130225         IF  %scan('?' : wCTS) > 0;
124600130225           ErrGenerico = *on;
124700130225           clear TRTB09DS;
124800130225           TB09fun = '1';
124900130225           TB09ord = '1';
125000130225           kpjbu = TRTB09DS;
125100130225           trtb09r (kpjba);
125200130225           TRTB09DS = kpjbu;
125300130225           wCTS = TB09cod;
125400130225         ENDIF;
125500130225
125600130225       //?Verifico che esista in tabella
125700130225         TBLkey = wCTS;
125800130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
125900130225         IF  not %found(TABEL00F) or TBLflg <> *blanks;
126000130225           ErrMessage  = *on;
126100130225           ErrGenerico = *on;
126200130225           V01msg      = skMsg(10);
126300130225           leavesr;
126400130225         ENDIF;
126500130225         dsCT = TBLuni;
126600130225
126700130225       //?Codice tassazione utilizzabile
126800130225         IF  §CTutc = 'N';
126900130225           ErrMessage  = *on;
127000130225           ErrGenerico = *on;
127100130225           V01msg      = skMsg(10);
127200130225           leavesr;
127300130225         ENDIF;
127400130225
127500130225       ENDSR;
127600130225
127700130225       //--------------------------------------------------------------
127800130225       //?Controllo codice tariffa.
127900130225       //--------------------------------------------------------------
128000130225       BEGSR  ctrCTR;
128100130225
128200130225       //?Ricerca
128300130225         IF  %scan('?' : V01ctr) > 0;
128400130225           ErrGenerico = *on;
128500130225           clear parmta36;
128600130225           ta36ksc = %dec(V01ksc:7:0);
128700130225           IF  V01taof = 'T';
128800130225             ta36flg = '1';
128900130225           ELSE;
129000130225             ta36flg = '3';
129100130225           ENDIF;
129200130225           ta36vpr = '1';
129300130225           ta36kcc = DUTkci;
129400130225           kpjbu = parmta36;
129500130225           IF  V01taof = 'T';
129600130225             tnta36r (kpjba);
129700130225           ELSE;
129800130225             tnta36c (kpjba);
129900130225           ENDIF;
130000130225           parmta36 = kpjbu;
130100130225           V01ctr = ta36ctr;
130200130225           V01prg = %int(ta36prg);
130300130225           leavesr;
130400130225         ENDIF;
130500130225
130600130225       //?Solo valori numerici
130700130225         IF  %check(digits:V01ctr) > 0;
130800130225           ErrMessage  = *on;
130900130225           ErrGenerico = *on;
131000130225           V01msg      = skMsg(18);
131100130225           leavesr;
131200130225         ENDIF;
131300130225
131400130225       //?Verifico che sia utilizzabile
131500130225         TBLcod = 'TR';
131600130225         TBLkey = %subst(V01ctr:1:1);
131700130225         chain  (TBLkut:TBLcod:TBLkey) TABEL00F;
131800130225         IF  %found(TABEL00F);
131900130225           dsTR = TBLuni;
132000130225         //?Codice tassazione utilizzabile
132100130225           IF  §TRute = 'N';
132200130225             ErrMessage  = *on;
132300130225             ErrGenerico = *on;
132400130225             V01msg      = skMsg(19);
132500130225             leavesr;
132600130225           ENDIF;
132700130225         ENDIF;
132800130225
132900130225       ENDSR;
133000121025
133100121025       //--------------------------------------------------------------
133200121025       //?Operazioni finali.                                           ?
133300121025       //--------------------------------------------------------------
133400121024       BEGSR  RoutEnd;
133500121025
133600121025         *inLR = *on;
133700100507
133800121025       //?Uscita dal pgm
133900121025         return;
134000121025
134100121025       ENDSR;
134200121025
134300121025      /end-free
134400121024
134500121024       //--------------------------------------------------------------
134600121024       //?Schiere a tempo di compilazione.
134700121024       //--------------------------------------------------------------
134800121024
134900130222** - skMSG ------------------------------------------------------------------*
135000130222Utente non abilitato alla funzione                                             01
135100130225Cliente errato o non in gestione all'utente                                    02
135200130225Tariffa inesistente                                                            03
135300130225Linea di partenza errata o non in gestione all'utente                          04
135400130225Linea di partenza FedEx non ammessa                                            05
135500130225Data errata                                                                    06
135600130225Data DAL inferiore alla prima data consentita da elaborare                     07
135700130225Data AL non può essere maggiore della data corrente                            08
135800130225La data AL deve essere maggiore o uguale della data DAL                        09
135900130225Codice tassazione errato o non accettabile                                     10
136000130225Peso spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                 11
136100130225Peso spedizioni a.. minore da..                                                12
136200130225Colli spedizioni da.. a.. devono essere entrambi = 0 oppure > 0                13
136300130225Colli spedizioni a.. minore da..                                               14
136400130225Immettere cliente/trattativa                                                   15
136500130225Trattativa errata o non in gestione all'utente                                 16
136600130225Offerta inesistente                                                            17
136700130225Codice tariffa errato o non trovato                                            18
136800130225Tipo tariffa non gestibile dal C.A.T.                                          19
136900130225Tariffa/Offerta errata o inesistente                                           20
137000130225Tariffa FedEx non ammessa                                                      21
