000100080206      //--------------------------------------------------------------
000200120221      //?TNSB48R - Gestione Manca Tariffa
000300080206      //--------------------------------------------------------------
000400080206
000500091124     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000120223
001100120223       // -?File Organigramma
001200120223     fAZORG01L  if   e           k disk
001300120130
001400120130       // -?File Tabelle
001500120130     fTABEL00F  if   e           k disk
001600110706
001700110706       // -?File video
001800120221     fTNSB48D   cf   e             workstn
001900120221     f                                     sfile(SB48S02 : S02nrr)
002000080208     f                                     indds(IndDspF)
002100080206     f                                     infds(InfDspF)
002200080206
002300080206      //---------------------------------------------------------------
002400090406      //?Definizione costanti.
002500080206      //---------------------------------------------------------------
002600080207
002700110706       // -?Tasti funzionali a video
002800080207     d c_F01           c                   const(x'31')
002900080207     d c_F02           c                   const(x'32')
003000080207     d c_F03           c                   const(x'33')
003100090406     d c_F04           c                   const(x'34')
003200080207     d c_F05           c                   const(x'35')
003300080207     d c_F06           c                   const(x'36')
003400080207     d c_F07           c                   const(x'37')
003500080207     d c_F08           c                   const(x'38')
003600080207     d c_F09           c                   const(x'39')
003700080207     d c_F10           c                   const(x'3A')
003800090303     d c_F11           c                   const(x'3B')
003900080207     d c_F12           c                   const(x'3C')
004000080207     d c_F13           c                   const(x'B1')
004100080207     d c_F14           c                   const(x'B2')
004200080207     d c_F15           c                   const(x'B3')
004300080207     d c_F16           c                   const(x'B4')
004400080207     d c_F17           c                   const(x'B5')
004500080207     d c_F18           c                   const(x'B6')
004600080207     d c_F19           c                   const(x'B7')
004700080207     d c_F20           c                   const(x'B8')
004800080207     d c_F21           c                   const(x'B9')
004900080207     d c_F22           c                   const(x'BA')
005000080207     d c_F23           c                   const(x'BB')
005100080207     d c_F24           c                   const(x'BC')
005200080207     d c_Enter         c                   const(x'F1')
005300080207     d c_RollDown      c                   const(x'F4')
005400080207     d c_RollUp        c                   const(x'F5')
005500091124
005600110706       // -?Costante per controllo "caratteri solo numerici"?
005700110706     d c_Digits        c                   const('0123456789')
005800080206
005900080206      //---------------------------------------------------------------
006000080206      //?Definizione schiere.
006100080206      //---------------------------------------------------------------
006200120223
006300120223       // -?Schiere per gestione codici cliente di ritorno da XCLIR
006400120223     d ksa             s              4    dim(30)
006500120223     d ksc             s              7  0 dim(30)
006600080206
006700110706       // -?Messaggi di errore
006800120223     d skMsg           s             78    dim(20) ctdata perrcd(1)
006900080206
007000080206      //---------------------------------------------------------------
007100080206      //?Definizione aree dati.
007200080206      //---------------------------------------------------------------
007300080206
007400110706       // -?Dati utente
007500080206     d §AzUte        e ds                  extname(AZUTE00F)
007600080206     d                                     dtaara
007700080206     d §DatiUte      e ds                  extname(dDatiUte)
007800080206     d                                     dtaara
007900080206
008000080206      //---------------------------------------------------------------
008100080206      //?Definizione strutture dati.
008200080206      //---------------------------------------------------------------
008300080206
008400110706       // -?Status
008500080206     d Psds           sds
008600080206     d   SDSpgm          *proc
008700080206
008800110706       // -?InfDS
008900080206     d InfDspF         ds
009000080207     d  dsp_aid              369    369a
009100120127     d  sfl_rrn              376    377i 0
009200120127     d  min_nrr              378    379i 0
009300120127     d  num_rcds             380    381i 0
009400080206
009500110706       // -?Indicatori su DspF
009600080208     d IndDspF         ds
009700120221       // -?Indicatori di ordinamento
009800120221     d  ORD_xksc                      1n   overlay(IndDspF : 25)
009900120221     d  ORD_xccm                      1n   overlay(IndDspF : 26)
010000120127       // -?Indicatori di errore
010100120127     d  ErrMessage                    1n   overlay(IndDspF : 28)
010200120127       // -?Indicatori di gestione subfile
010300120130     d  SflDsp                        1n   overlay(IndDspF : 30)
010400120130     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010500120127     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010600120127     d  SflEnd                        1n   overlay(IndDspF : 33)
010700120127       // -?Altri Indicatori
010800120221     d  PosCurFIL                     1n   overlay(IndDspF : 50)
010900120221     d  PosCurCCM                     1n   overlay(IndDspF : 51)
011000120221     d  PosCurKSC                     1n   overlay(IndDspF : 52)
011100120222     d  PosCurPWD                     1n   overlay(IndDspF : 53)
011200120223     d  PosCurOPZ                     1n   overlay(IndDspF : 54)
011300090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011400090407
011500090407     d WindDspF        ds                  inz
011600090407     d   WindDspFa             1     49    inz(*zero)
011700090407     d   WindDspFb            50     99    inz(*zero)
011800120223
011900120224       // -?ds x ricerca clienti
012000120223     d                 ds
012100120223     d dsksn                   1      4p 0
012200120223     d dsksa                   1      4
012300080206
012400110706       // -?Paremetri ricevuti
012500080206     d KPJBA         e ds
012600120326
012700120326      // -?Scrittura R.A. per OK utente su errore IMP
012800120326     d FIDNA3ds      e ds                  inz
012900120130
013000120130      // -?Ricerca/Controllo tabelle
013100120130     d TIBS02ds      e ds                  inz
013200080206
013300120229      // -?Reperimento dati utente
013400080206     d TIBS34ds      e ds
013500120224
013600120224      // -?Gestione bolle di sede
013700120224     d TNSB01ds      e ds                  inz
013800120224
013900120224      // -?Lancio controllo manca tariffa
014000120224     d TNSB44ds      e ds                  inz
014100120314
014200120314      // -?Passaggio dati al pgm gestione tariffe
014300120314     d TNSB48ds      e ds                  inz
014400120229
014500120229      // -?Interrogazione bolle di sede
014600120229     d TNSB51ds      e ds                  inz
014700120221
014800120221       // -?Controllo abilitazioni utente - Gestione tariffe
014900120221     d TNTAA1DS      e ds                  inz
015000120130
015100120221       // -?Reperimento filiali gestitbili dall'utente
015200120221     d TRUL31ds      e ds
015300120223     d pog                    10    759    dim(250)
015400120223
015500120223       // -?Ricerca clienti
015600120223     d xCLIrds       e ds
015700120223
015800120223       // -?DS Tabella MTC - Password tariffe
015900120223     d dMTC          e ds
016000120221
016100120130       // -?DS Tabella 01 - Codice Commerciale
016200120130     d ds01          e ds
016300120223
016400120223       // -?File TFMTC00F - ds  x sql
016500120309     d TFMTCds       e ds                  extname(TFMTC00F)
016600090629
016700080206      //---------------------------------------------------------------
016800080206      //?Definizione variabili globali.
016900080206      //---------------------------------------------------------------
017000080206
017100110706       // -?Flags booleani
017200120221     d wEoF            s               n   inz(*off)
017300120221     d wErrGrave       s               n   inz(*off)
017400120130     d wFine           s               n   inz(*off)
017500120326     d wFineW01        s               n   inz(*off)
017600120130     d wInzD01         s               n   inz(*on)
017700120130     d wInzS02         s               n   inz(*off)
017800120306     d wTassaPrima     s               n   inz(*off)
017900080206
018000110706       // -?Campi associati al video
018100120130     d wVideo          s              2    inz('D1')
018200090601     d dsp_mod         s             10I 0
018300120127     d S02nrr          s              4  0 inz
018400110706
018500110706       // -?Campi di comodo
018600120223     d sav_Pwd         s                   like(V01pwd)
018700120306     d sav_S02nrr      s                   like(S02nrr)
018800120222     d wGiorni         s              3s 0 inz
018900120223     d wUteAbi         s                   like(OTAA1cabi)
019000120221     d w003a           s              3a   inz
019100120307     d w0070           s              7s 0 inz
019200120130
019300120130       // -?Campi di comodo data
019400120223     d wData           s              8s 0
019500120130     d wDataEUR        s               d   datfmt(*eur)
019600120130     d wDataISO        s               d   datfmt(*iso)
019700120222     d wDataScad       s              8s 0
019800120221     d wOggi           s              8s 0
019900120221
020000120221       // -?Campi da passare al pgm x la ricerca del commerciale
020100120221     d TB85kfil        s              3a
020200120221     d TB85key         s              8a
020300120221     d TB85des1        s             25a
020400120412     d tb85Uni         s              1    inz('S')
020500120302
020600120302       // -?Campo da passare al pgm x la gestione/interrogazione bolle
020700120302     d ParTnsb         s              1a
020800120321
020900120321       // -?Campi da passare al pgm x interrogazione clienti
021000120321     d Par01           s              2a
021100120321     d Parrag          s             48a
021200120223
021300120222       // -?Indici di schiera
021400120222     d xx              s              4  0 inz
021500120222     d yy              s              4  0 inz
021600090806
021700110706       // -?Stringa SQL da eseguire
021800090806     d wSQL            s           2048    Varying        inz
021900120221
022000120221      // ----------------------------------------------------------------------
022100120221      //?Costanti per ordinamento subfile
022200120221      // ----------------------------------------------------------------------
022300120221     d MaxKey          c                   4
022400120221     d Ascendente      c                   1
022500120221     d Discendente     c                   2
022600120221     d Carattere       c                   6
022700120221     d Numerico        c                   8
022800120221     d Put             c                   1
022900120221     d EndPut          c                   2
023000120221     d Get             c                   3
023100120221      // ----------------------------------------------------------------------
023200120221      // Campi utili:
023300120221      //     nrr        - Numero relativo di record del Subfile
023400120221      //     SizeList   - Dimensione della lista
023500120221      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
023600120221      // ----------------------------------------------------------------------
023700120221     d NotUsed         s             16A
023800120221     d ReturnSize      s              9B 0
023900120221     d SizeList        s              9B 0
024000120221     d RrnLast         s              5I 0
024100120221     d WrkSort         s              1  0 inz(0)
024200120221      // ----------------------------------------------------------------------
024300120221      // Data Structures
024400120221      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
024500120221      //     QLGSCB     - The sort request block for the QLGSORT API
024600120221      //     QLGSCB00   - The sort request block for the QLGSRTIO API
024700120221      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
024800120221      //     QUSEC      - Error structure for the QLGSORT API
024900120221      // ----------------------------------------------------------------------
025000120221     d sflrcd          ds
025100120223     d  s02ksc
025200120223     d  s02ccm
025300120223     d  s02dsp
025400120223     d  s02lnp
025500120223     d  s02lna
025600120223     d  s02ctr
025700120223     d  s02tsp
025800120223     d  s02opz
025900120223     d  s02rag
026000120223     d  s02dccm
026100120224     d  s02err
026200120228     d  s02aas
026300120228     d  s02nrs
026400120228     d  s02nsp
026500120228     d  s02tbl
026600120228     d  s02pro
026700120228     d  s02cts
026800120305     d  s02ncl
026900120305     d  s02pkf
027000120305     d  s02vlf
027100120326     d  s02imp
027200120326     d  s02imv
027300120221     d  selected                      1A
027400120221
027500120221      /COPY QSYSINC/QRPGLESRC,QLGSORT
027600120221     d QLGKL                         16    DIM(MaxKey)
027700120221     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
027800120221     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
027900120221     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
028000120221     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
028100120221
028200120221      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
028300120221      /COPY QSYSINC/QRPGLESRC,QUSEC
028400090508
028500090508      //---------------------------------------------------------------
028600090807      //?Definizione procedure esterne.
028700090508      //---------------------------------------------------------------
028800120221
028900120221       // -?Api x ordinamento subfile
029000120221     d Qlgsort_pr      pr                  extpgm('QLGSORT')
029100120221     d  pr_QLGSCB                          like(Qlgscb)
029200120221     d  pr_NotUsed1                        like(NotUsed)
029300120221     d  pr_NotUsed2                        like(NotUsed)
029400120221     d  pr_SizeList                        like(SizeList)
029500120221     d  pr_ReturnSize                      like(ReturnSize)
029600120221     d  pr_QUSEC                           like(Qusec)
029700120221     d                                     options(*varsize)
029800120221
029900120221     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
030000120221     d  pr_QLGSCB00                        like(QLGSCB00)
030100120221     d  pr_SflRcd                          like(SflRcd)
030200120221     d  pr_NotUsed1                        like(NotUsed)
030300120221     d  pr_SizeList                        like(SizeList)
030400120221     d  pr_NotUsed2                        like(NotUsed)
030500120221     d  pr_QUSEC                           like(Qusec)
030600120221     d                                     options(*varsize)
030700120221
030800120221     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
030900120221     d  pr_QLGSCB00                        like(QLGSCB00)
031000120221     d  pr_NotUsed1                        like(NotUsed)
031100120221     d  pr_SflRcd                          like(SflRcd)
031200120221     d  pr_SizeList                        like(SizeList)
031300120221     d  pr_NotUsed2                        like(NotUsed)
031400120221     d  pr_QUSEC                           like(Qusec)
031500120221     d                                     options(*varsize)
031600120326
031700120326       // -?Richiamo Scrittura R.A.
031800120326     d fidna3r         pr                  extpgm('FIDNA3R')
031900120326     d  kpjba                              likeds(KPJBA)
032000120326     d  fidna3ds                           likeds(FIDNA3DS)
032100120224
032200120224       // -?Richiamo Controllo Manca Tariffe
032300120224     d tnsb45r         pr                  extpgm('TNSB45R')
032400120224     d  kpjba                              likeds(KPJBA)
032500120229
032600120229       // -?Richiamo Interrogazione bolle di Sede
032700120229     d tnsb51c         pr                  extpgm('TNSB51C')
032800120229     d  kpjba                              likeds(KPJBA)
032900120302     d  partnsb                       1a
033000120224
033100120224       // -?Richiamo Gestione bolle di Sede
033200120224     d tnsb52r         pr                  extpgm('TNSB52R')
033300120224     d  kpjba                              likeds(KPJBA)
033400120224
033500120224       // -?Richiamo Gestione Tariffe
033600120224     d tnta01r         pr                  extpgm('TNTA01R')
033700120224     d  kpjba                              likeds(KPJBA)
033800120314     d  tnsb48ds                           likeds(TNSB48DS)
033900120321
034000120321       // -?Richiamo Interrogazione Tariffe
034100120321     d tnta02r         pr                  extpgm('TNTA02R')
034200120321     d  kpjba                              likeds(KPJBA)
034300120321     d  tnsb48ds                           likeds(TNSB48DS)
034400110506
034500120221       // -?Reperimento filiali gestitbili dall'utente
034600120221     d trul31r         pr                  extpgm('TRUL31R')
034700120221     d  kpjba                              likeds(KPJBA)
034800120221     d  trul31ds                           likeds(TRUL31DS)
034900120223
035000120223       // -?Ricerca cliente
035100120223     d xclir           pr                  extpgm('XCLIR')
035200120223     d  xclirds                            likeds(XCLIRDS)
035300120321     d  par01                         2
035400120321     d  parrag                       48
035500110706
035600110706      //---------------------------------------------------------------
035700110706      //?Definizione prototipi.
035800110706      //---------------------------------------------------------------
035900120130
036000120130       // -?Ricerc/Controllo tabelle
036100120130      /copy gaitrasrc/srcprotopr,tibs02r
036200110706
036300110706       // -?Reperimento dati utente
036400110706      /copy gaitrasrc/srcprotopr,tibs34r
036500120130
036600120130       // -?Reperimento dati anagrafici cliente codificato
036700120222      /copy gaitrasrc/srcprotopi,tibs69r
036800120222      /copy gaitrasrc/srcprotopr,tibs69r
036900120221
037000120221       // -?Controllo abilitazioni utente
037100120221      /copy gaitrasrc/srcprotopr,tntaa1c
037200120221
037300120221       // -?Ricerca commerciale
037400120221      /copy gaitrasrc/srcprotopr,trtb85r
037500090807
037600080206      //---------------------------------------------------------------
037700080206      //?Definizione key-list.
037800080206      //---------------------------------------------------------------
037900090807
038000120130       // -?File TABEL00F
038100120130     d k03tabel      e ds                  extname(TABEL00F:*key)
038200120130     d                                     prefix(k_)
038300080206
038400080206      //---------------------------------------------------------------
038500080206      //?Riepilogo indicatori.
038600080206      //---------------------------------------------------------------
038700090807
038800090807
038900080206      //---------------------------------------------------------------
039000080206
039100080206      //---------------------------------------------------------------
039200080206      //?M A I N - L I N E
039300080206      //---------------------------------------------------------------
039400080206
039500080206     c     *Entry        plist
039600080206     c                   parm                    KPJBA
039700080206
039800080206      /free
039900080206
040000090807       //?Operazioni iniziali
040100080206       exsr RoutInz;
040200080206
040300090807       //?Gestione video
040400120326       DOW  wFine = *off;
040500090807
040600080206         select;
040700120130           when wVideo = 'D1';
040800110706             exsr GesD01;
040900120130           when wVideo = 'S2';
041000120130             exsr GesS02;
041100090428           other;
041200120130             wFine = *on;
041300080206         endsl;
041400090807
041500080206       ENDDO;
041600080206
041700090807       //?Operazioni finali
041800080206       exsr RoutEnd;
041900080206
042000080206       //--------------------------------------------------------------
042100080206       //?Operazioni iniziali.
042200080206       //--------------------------------------------------------------
042300080206       BEGSR RoutInz;
042400120309
042500120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
042600080206
042700120130         wVideo = 'D1';
042800120130         wInzD01 = *on;
042900090807
043000110706       //?Reperimento dati job
043100090807         exsr DatiJob;
043200120221
043300120221       //?Controllo se utente autorizzato alla funzione
043400120221         reset TNTAA1DS;
043500120221         ITAA1caut = '§utegtc';
043600120221         kpjbu     = TNTAA1DS;
043700120221         tntaa1c (kpjba);
043800120221         TNTAA1DS = kpjbu;
043900120221
044000120221         IF  OTAA1fabi = 'N';
044100120221           wErrGrave   = *on;
044200120221           ErrMessage  = *on;
044300120221           ErrGenerico = *on;
044400120221           V01msg = skMsg(01);
044500120221           leavesr;
044600120221         ENDIF;
044700120223
044800120223         wUteAbi = OTAA1cabi;
044900120221
045000120221         clear TRUL31DS;
045100120221         I31abi = OTAA1cabi;
045200120221         I31cdi = DUTdis;
045300120221         I31car = DUTare;
045400120221         I31cpo = DUTpou;
045500120221         trul31r (kpjba : trul31ds);
045600120221         IF  O31pog <= *zeros;
045700120221           wErrGrave   = *on;
045800120221           ErrMessage  = *on;
045900120221           ErrGenerico = *on;
046000120223           V01msg = skMsg(01);
046100120221           leavesr;
046200120221         ENDIF;
046300120309
046400090806
046500110706       //?Impostazione campi "fissi"
046600110706         V1Tpgm = SDSpgm;
046700120130         k_TBLkut = 1;
046800120221
046900120221       //?Data del giorno
047000120221         wOggi = %dec(%date());
047100120223
047200120223       //?Cerco la tabella gg. validità password
047300120223         clear TIBS02DS;
047400120223         clear dMTC;
047500120223         T02mod = 'C';
047600120223         T02cod = 'MTC';
047700120223         T02sif = knsif;
047800120223         T02ke1 = 'PSW';
047900120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
048000120223         IF  T02err <> *blanks;
048100120223           wErrGrave   = *on;
048200120223           ErrMessage  = *on;
048300120223           ErrGenerico = *on;
048400120223           PosCurPWD   = *on;
048500120223           V01msg      = skMsg(06);
048600120223           leavesr;
048700120223         ENDIF;
048800120223         wGiorni = %dec(T02uni:3:0);
048900120222
049000120222       //?Cerco la password memorizzata per filiale (utente)
049100120222         clear TIBS02DS;
049200120222         clear dMTC;
049300120222         T02mod = 'C';
049400120222         T02cod = 'MTC';
049500120222         T02sif = knsif;
049600120223         T02ke1 = %editc(DUTpou:'X');
049700120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
049800120222         IF  T02err <> *blanks;
049900120222           wErrGrave   = *on;
050000120222           ErrMessage  = *on;
050100120222           ErrGenerico = *on;
050200120222           V01msg = skMsg(02);
050300120222           leavesr;
050400120222         ENDIF;
050500120222         dMTC = T02uni;
050600120223
050700120223         clear sav_Pwd;
050800090714
050900080206       ENDSR;
051000080206
051100080206       //--------------------------------------------------------------
051200080206       //?Reperimento Dati del job (Utente/Operativi).
051300080206       //--------------------------------------------------------------
051400080206       BEGSR DatiJob;
051500080206
051600080206         in(E) §AzUte;
051700110706         IF NOT %error;
051800080206           in(E) §DatiUte;
051900110706         ENDIF;
052000110706         IF %error or RSut = *blanks;
052100080206           clear TIBS34ds;
052200080206           tibs34r(tibs34ds);
052300080206           in §AzUte;
052400080206           in §DatiUte;
052500110706         ENDIF;
052600080206
052700080206       ENDSR;
052800080206
052900090421       //--------------------------------------------------------------
053000110706       //?Gestione videata D01
053100080206       //--------------------------------------------------------------
053200110706       BEGSR GesD01;
053300080207
053400110706       //?Inizializzazione videata
053500120130         IF  wInzD01 = *on;
053600110706            exsr InzD01;
053700120130            wInzD01  = *off;
053800110706         ENDIF;
053900080207
054000110706       //?Emissione Testata
054100120221         write  SB48T01;
054200080207
054300110706       //?Emissione videata
054400120221         exfmt  SB48D01;
054500110706         reset ErrMessage;
054600110706         reset ErrGenerico;
054700120127         clear V01msg;
054800080207
054900080207         SELECT;
055000120227
055100120227         //?- Errore grave esco dal pgm
055200120227           WHEN  wErrGrave;
055300120227             wFine = *on;
055400110706
055500110706         //?- F03=Fine
055600110706           WHEN  dsp_aid = c_F03;
055700110706             exsr F03D01;
055800080207
055900090807         //?Invio
056000080207           OTHER;
056100110706         //?Eseguo i controlli
056200110706             exsr CtrD01;
056300110706             IF  ErrGenerico;
056400080207               leavesr;
056500110706             ENDIF;
056600120127         //?Se tutto OK vado nella seconda videata
056700120130           wVideo = 'S2';
056800120130           wInzS02 = *on;
056900080207
057000080207         ENDSL;
057100080207
057200080207       ENDSR;
057300080207
057400080207       //--------------------------------------------------------------
057500110706       //?Inizializzazione videata D01.
057600080207       //--------------------------------------------------------------
057700110706       BEGSR InzD01;
057800080207
057900120221         clear V01fil;
058000120302         V01dfil = '(0 = Tutte)';
058100120221         clear V01ccm;
058200120221         clear V01dccm;
058300120221         clear V01ksc;
058400120221         clear V01rag;
058500120223
058600120223         V01pwd = sav_Pwd;
058700090508
058800080207       ENDSR;
058900080207
059000091111       //--------------------------------------------------------------
059100110706       //?Gestione tasto funzionale F03 da videata D01.
059200110706       //?F3=Fine
059300091111       //--------------------------------------------------------------
059400110706       BEGSR F03D01;
059500091111
059600110706       //?Chiusura del programma
059700120130         wFine = *on;
059800091111
059900091111       ENDSR;
060000120130
060100120130       //--------------------------------------------------------------
060200120130       //?Controllo dati videata D01.
060300120130       //--------------------------------------------------------------
060400120130       BEGSR CtrD01;
060500120130
060600120130         WindDspF  = IndDspF;
060700120130         reset WindDspFb;
060800120130         IndDspF   = WindDspF;
060900120130
061000120221       //?Controllo la filiale
061100120221         IF  V01fil > 0;
061200120221           chain V01fil AZORG01L;
061300120223           IF not %found(AZORG01L);
061400120221             ErrMessage  = *on;
061500120221             ErrGenerico = *on;
061600120221             PosCurFIL   = *on;
061700120222             V01msg      = skMsg(03);
061800120221             leavesr;
061900120221           ENDIF;
062000120221         //?Verifico se l'utente è abilitato alla filiale
062100120221           w003a = %editc(V01fil:'X');
062200120221           IF  %lookup(w003a:pog) = 0;
062300120221             ErrMessage  = *on;
062400120221             ErrGenerico = *on;
062500120221             PosCurFIL   = *on;
062600120222             V01msg      = skMsg(03);
062700120221             leavesr;
062800120221           ENDIF;
062900120130         ENDIF;
063000120221
063100120221       //?Ricerca del commerciale
063200120221         IF  %scan('?' : V01ccm) > 0;
063300120221           ErrGenerico = *on;
063400120222           PosCurCCM   = *on;
063500120221           TB85kfil    = %editc(DUTpou:'X');
063600120221           clear TB85key;
063700120221           clear TB85des1;
063800120412           trtb85r (kpjba : TB85kfil : TB85key : TB85des1 : tb85Uni);
063900120221           V01ccm  = TB85key;
064000120221           V01dccm = TB85des1;
064100120221           leavesr;
064200120221         ENDIF;
064300120221
064400120221       //?Controllo il commerciale
064500120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
064600120221
064700120221         //?Solo valori numerici
064800120223           IF  %check(c_digits:V01ccm) > 0;
064900120221             ErrMessage  = *on;
065000120221             ErrGenerico = *on;
065100120222             PosCurCCM   = *on;
065200120222             V01msg      = skMsg(04);
065300120221             leavesr;
065400120221           ENDIF;
065500120221
065600120221         //?Controllo se utente abilitato al commerciale
065700120221           clear TNTAA1DS;
065800120221           ITAA1caut = '§utegtc';
065900120221           ITAA1cmm  = %dec(V01ccm:7:0);
066000120221           kpjbu     = tntaa1ds;
066100120221           tntaa1c (kpjba);
066200120221           TNTAA1DS = kpjbu;
066300120221           IF  OTAA1fabi = 'N';
066400120221             ErrMessage  = *on;
066500120221             ErrGenerico = *on;
066600120222             PosCurCCM   = *on;
066700120222             V01msg      = skMsg(04);
066800120221             leavesr;
066900120221           ENDIF;
067000120221
067100120221         //?Controllo se commerciale valido
067200120221           clear  ds01;
067300120223           k_TBLcod = '01';
067400120223           k_TBLkey = V01ccm;
067500120223           chain  %kds(K03tabel) TABEL00F;
067600120221           IF  not %found(TABEL00F) or TBLflg <> *blanks;
067700120221             ErrMessage  = *on;
067800120221             ErrGenerico = *on;
067900120222             PosCurCCM   = *on;
068000120222             V01msg      = skMsg(04);
068100120221             leavesr;
068200120221           ENDIF;
068300120221
068400120221           ds01    = TBLuni;
068500120221           V01dccm = §01age;
068600120221
068700120221         //?Commerciale senza particolarità
068800120221           IF  §01par <> *blanks;
068900120221             ErrMessage  = *on;
069000120221             ErrGenerico = *on;
069100120222             PosCurCCM   = *on;
069200120222             V01msg      = skMsg(04);
069300120221             leavesr;
069400120221           ENDIF;
069500120221
069600120221         //?Commerciale "valido" data inizio e fine attività
069700120221           IF  §01dtdec > %editc(wOggi:'X') or
069800120221               §01dtfin < %editc(wOggi:'X');
069900120221             ErrMessage = *on;
070000120221             ErrGenerico = *on;
070100120222             PosCurCCM   = *on;
070200120222             V01msg      = skMsg(04);
070300120221             leavesr;
070400120221           ENDIF;
070500120412
070600120412         //?Deve essere un commerciale Unificante
070700120412           IF  %dec(V01ccm:7:0) <> §01rgf;
070800120412             ErrMessage = *on;
070900120412             ErrGenerico = *on;
071000120412             PosCurCCM   = *on;
071100120412             V01msg      = skMsg(11);
071200120412             leavesr;
071300120412           ENDIF;
071400120221         ENDIF;
071500120223
071600120223       //?Ricerca del cliente
071700120223         IF  %scan('?' : V01ksc) > 0;
071800120223           ErrGenerico = *on;
071900120223           PosCurKSC   = *on;
072000120223           clear xCLIrds;
072100120321           clear par01;
072200120321           clear parrag;
072300120223           DXCaut = wUteAbi;
072400120223           DXCcap = DUTkci;
072500120321           xclir (xCLIrds:par01:parrag);
072600120223           ksa = DXCksc;
072700120223           dsksa = ksa(1);
072800120223           ksc(1) = dsksn;
072900120223           V01ksc = %editc(ksc(1):'X');
073000120223         ENDIF;
073100120223
073200120223       //?Controllo il cleinte
073300120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
073400120223
073500120223         //?Solo valori numerici
073600120223           IF  %check(c_digits:V01ksc) > 0;
073700120223             ErrMessage  = *on;
073800120223             ErrGenerico = *on;
073900120223             PosCurKSC   = *on;
074000120223             V01msg      = skMsg(09);
074100120223             leavesr;
074200120223           ENDIF;
074300120223
074400120223         //?Controllo se utente abilitato al cliente
074500120223           clear TNTAA1DS;
074600120223           ITAA1caut = '§utegtc';
074700120223           ITAA1ksc  = %dec(V01ksc:7:0);
074800120223           kpjbu     = tntaa1ds;
074900120223           tntaa1c (kpjba);
075000120223           TNTAA1DS = kpjbu;
075100120223           IF  OTAA1fabi = 'N';
075200120223             ErrMessage  = *on;
075300120223             ErrGenerico = *on;
075400120223             PosCurKSC   = *on;
075500120223             V01msg      = skMsg(09);
075600120223             leavesr;
075700120223           ENDIF;
075800120223
075900120223         //?Controllo se cliente valido
076000120223           clear tibs69ds;
076100120223           I69kac = %dec(V01ksc:7:0);
076200120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
076300120223           IF  O69err <> *blanks;
076400120223             ErrMessage  = *on;
076500120223             ErrGenerico = *on;
076600120223             PosCurKSC   = *on;
076700120223             V01msg      = skMsg(09);
076800120223             leavesr;
076900120223           ENDIF;
077000120223           V01rag = ACOrag;
077100120223         ENDIF;
077200120221
077300120222       //?Password obbligatoria
077400120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
077500120222           ErrMessage = *on;
077600120222           ErrGenerico = *on;
077700120222           PosCurPWD   = *on;
077800120222           V01msg      = skMsg(05);
077900120222           leavesr;
078000120222         ENDIF;
078100120222
078200120222       //?Controllo se password OK
078300120222         IF  V01pwd <> §MTCpwd;
078400120222           ErrMessage = *on;
078500120222           ErrGenerico = *on;
078600120222           PosCurPWD   = *on;
078700120222           V01msg      = skMsg(05);
078800120222           clear V01pwd;
078900120222           leavesr;
079000120222         ENDIF;
079100120222
079200120222       //?Data immissione/modifica password
079300120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
079400120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
079500120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
079600120223         wDataISO = %date(wData);
079700120223
079800120223       //?Calcolo la data scadenza password
079900120223         wDataISO += %days(wgiorni);
080000120223         wDataScad = %dec(wDataISO);
080100120223
080200120223       //?Controllo se la password è scaduta
080300120222         IF wOggi > wDataScad;
080400120222           ErrMessage  = *on;
080500120222           ErrGenerico = *on;
080600120222           PosCurPWD   = *on;
080700120222           V01msg      = skMsg(07);
080800120222           leavesr;
080900120222         ENDIF;
081000120223
081100120223         sav_Pwd = V01pwd;
081200120130
081300120130       ENDSR;
081400120130
081500120130       //--------------------------------------------------------------
081600120130       //?Gestione videata S02
081700120130       //--------------------------------------------------------------
081800120130       BEGSR GesS02;
081900120306
082000120306       //?Se non l'ho ancora fatto tasso tutto
082100120306       //?(capita solo appena entro nel pgm)
082200120306         IF  not wTassaPrima;
082300120309           exsr F05S02;
082400120306           wTassaPrima = *on;
082500120306         ENDIF;
082600120130
082700120130       //?Inizializzazione videata
082800120130         IF  wInzS02 = *on;
082900120130            exsr InzS02;
083000120130            wInzS02  = *off;
083100120130         ENDIF;
083200120130
083300120130       //?Emissione Testata + Piede
083400120221         write  SB48T01;
083500120221         write  SB48P02;
083600120130
083700120130       //?Emissione subfile
083800120221         exfmt  SB48C02;
083900120130         reset ErrMessage;
084000120130         reset ErrGenerico;
084100120130         clear V02msg;
084200120130
084300120130         SELECT;
084400120130
084500120130         //?- F03=Fine
084600120130           WHEN  dsp_aid = c_F03;
084700120130             exsr F03D01;
084800120130
084900120222         //?- F05=Ritassa
085000120222           WHEN  dsp_aid = c_F05;
085100120222             exsr F05S02;
085200120222
085300120228         //?- F08=Ordina
085400120222           WHEN  dsp_aid = c_F08;
085500120229           //?Se era ordina per cliente diventa ordina per commerciale
085600120229             IF  Ord_xksc;
085700120229               Ord_xksc = *off;
085800120229               Ord_Xccm = *on;
085900120229               wF08 = 'F8=Ord.x Cliente';
086000120229           //?Se era ordina per cliente diventa ordina per commerciale
086100120229             ELSE;
086200120229               Ord_xksc = *on;
086300120229               Ord_Xccm = *off;
086400120229               wF08 = 'F8=Ord.x Commerciale';
086500120229             ENDIF;
086600120222             exsr F08S02;
086700120130
086800120130         //?- F12=Ritorno
086900120130           WHEN  dsp_aid = c_F12;
087000120130             exsr F12S02;
087100120130
087200120130         //?Invio
087300120130           OTHER;
087400120222         //?Opzioni
087500120223             IF  S02nrr > *zeros;
087600120223               exsr OpzS02 ;
087700120223             ENDIF;
087800120130             IF  ErrGenerico;
087900120130               leavesr;
088000120130             ENDIF;
088100120130
088200120130         ENDSL;
088300120130
088400120130       ENDSR;
088500120130
088600120130       //--------------------------------------------------------------
088700120130       //?Inizializzazione videata S02.
088800120130       //--------------------------------------------------------------
088900120130       BEGSR InzS02;
089000120130
089100120130       //?Pulizia subfile
089200120130         exsr PulS02;
089300120222
089400120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
089500120222         exsr PrepSql;
089600120130
089700120130       //?Caricamento dati
089800120130         exsr CarS02;
089900120130
090000120130       //?Visualizzazione del Subfile
090100120130         SflDsp = (S02nrr <= *zeros);
090200120130
090300120130       //?Attivazione SFLEND
090400120130         SflEnd = *on;
090500120130
090600120130       //?Impaginazione Subfile
090700120130       //?Forzo sempre il primo rcd
090800120130         IF  S02nrr > *zero;
090900120130           C02rcd = 1;
091000120130         ELSE;
091100120130           clear C02rcd;
091200120130         ENDIF;
091300120130
091400120130         C02csr = C02rcd;
091500120130
091600120130       ENDSR;
091700120130
091800120130       //--------------------------------------------------------------
091900120130       //?Pulizia Subfile S02.
092000120130       //--------------------------------------------------------------
092100120130       BEGSR PulS02;
092200120130
092300120130       //?Pulizia subfile
092400120130         SflDsp    = *on;
092500120130         SflDspCtl = *on;
092600120221         write  SB48C02;
092700120130         SflDspCtl = *off;
092800120130         SflEnd    = *off;
092900120130
093000120130         clear C02rcd;
093100120130         clear C02csr;
093200120130         clear S02nrr;
093300120130         clear V02msg;
093400120130         ErrMessage  = *off;
093500120130         ErrGenerico = *off;
093600120130
093700120130         WindDspF = IndDspF;
093800120130         reset WindDspFb;
093900120130         IndDspF  = WindDspF;
094000120130
094100120130       ENDSR;
094200120222
094300120222       //--------------------------------------------------------------
094400120222       //?Preparazione stringa SQL.
094500120222       //--------------------------------------------------------------
094600120222       BEGSR PrepSql;
094700120307
094800120307         clear w0070;
094900120222
095000120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
095100120222
095200120307       //?Se nessuna richiesta a video
095300120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
095400120307            (V01ccm = *blanks or V01ccm = *zeros);
095500120222       //?Carico le bolle con filiale cliente = sk POG
095600120222           wSQL += ' and MTCpoc in(';
095700120222           yy = 0;
095800120222           xx = 1;
095900120222           FOR xx by 1 to %elem(pog);
096000120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
096100120222               IF yy > 0;
096200120222                 wSQL += ', ';
096300120222               ELSE;
096400120222               yy = 1;
096500120222               ENDIF;
096600120222               wSQL += '''';
096700120222               wSQL += pog(xx);
096800120222               wSQL += '''';
096900120222             ENDIF;
097000120222           ENDFOR;
097100120222           wSQL += ')';
097200120222         ELSE;
097300120307         //?Se richiesta filiale a video
097400120307           IF  V01fil > 0;
097500120307         //?Carico le bolle con filiale cliente = filiale richiesta
097600120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
097700120307           ENDIF;
097800120307         //?Se richiesto commerciale a video
097900120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
098000120307              w0070 = %dec(V01ccm:7:0);
098100120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
098200120307           ENDIF;
098300120307         //?Se richiesto cliente a video
098400120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
098500120307              w0070 = %dec(V01ksc:7:0);
098600120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
098700120307           ENDIF;
098800120222         ENDIF;
098900120309
099000120309         wSQL += ' for FETCH ONLY';
099100120222
099200120222       ENDSR;
099300120130
099400120130       //--------------------------------------------------------------
099500120130       //?Caricamento Subfile S02.
099600120130       //--------------------------------------------------------------
099700120130       BEGSR CarS02;
099800120312
099900120312         clear sav_s02nrr;
100000120130
100100120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
100200120222         exec sql prepare A1 from :wSQL;
100300120222         exec sql declare MTC cursor for A1;
100400120222         exec sql open MTC;
100500120222
100600120222         IF  sqlcode < 0;
100700120222           wEoF = *on;
100800120222         ENDIF;
100900120222
101000120222         DOW  not wEoF;
101100120309           exec sql fetch next from MTC into :TFMTCds;
101200120222           IF  sqlcod = 100 or sqlcod < 0;
101300120222             wEoF = *on;
101400120222             leave;
101500120222           ENDIF;
101600120222
101700120222         //?Carico il subfile
101800120222           exsr RieS02;
101900120222         //?Scrico il subfile
102000120222           S02nrr += 1;
102100120222           write SB48S02;
102200120306           sav_S02nrr = S02nrr;
102300120229           IF  S02nrr = 9998;
102400120229             leave;
102500120229           ENDIF;
102600120222
102700120222         ENDDO;
102800120222
102900120222         exec sql close MTC;
103000120130         wEoF = *off;
103100120222
103200120222         //?Dopo aver caricato il subfile lo ordino per cliente
103300120229         wF08 = 'F8=Ord.x Commerciale';
103400120228         Ord_xksc = *on;
103500120228         Ord_Xccm = *off;
103600120228         exsr F08S02;
103700120130
103800120130       ENDSR;
103900120130
104000120130       //--------------------------------------------------------------
104100120130       //?Carico i dati nel subfile S02.
104200120130       //--------------------------------------------------------------
104300120130       BEGSR RieS02;
104400120130
104500120221         clear  SB48S02;
104600120130
104700120228         S02aas = MTCaas;
104800120228         S02nrs = MTCnrs;
104900120228         S02nsp = MTCnsp;
105000120228         S02tbl = MTCtbl;
105100120222
105200120222         S02ksc = MTCksc;
105300120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
105400120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
105500120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
105600120222         S02ctr = MTCctr;
105700120222         S02ccm = MTCage;
105800120222         S02err = MTCerr;
105900120223         S02lnp = MTClnp;
106000120223         S02lna = MTClna;
106100120223         S02tsp = MTCtsp;
106200120228         S02pro = MTCpro;
106300120228         S02cts = MTCcts;
106400120305         S02ncl = MTCncl;
106500120305         S02pkf = MTCpkf;
106600120305         S02vlf = MTCvlf;
106700120326
106800120326         S02imp = MTCimp;
106900120326         S02imv = MTCimv;
107000120130
107100120222       //?Decodifico il cliente
107200120222         clear tibs69ds;
107300120222         I69kac = S02ksc;
107400120222         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
107500120222         IF  O69err = *blanks;
107600120222           S02rag = ACOrag;
107700120222         ENDIF;
107800120222
107900120222       //?Decodifico il commerciale
108000120222         clear ds01;
108100120223         k_TBLcod = '01';
108200120223         k_TBLkey = %editc(S02ccm:'X');
108300120223         chain  %kds(K03tabel) TABEL00F;
108400120222         IF  %found(TABEL00F) and TBLflg = *blanks;
108500120222           ds01 = TBLuni;
108600120222         ENDIF;
108700120223         S02dccm = §01age;
108800120130
108900120130       ENDSR;
109000120130
109100120130       //--------------------------------------------------------------
109200120222       //?Gestione delle opzioni immesse nella videata S02.
109300120130       //--------------------------------------------------------------
109400120222       BEGSR OpzS02;
109500120130
109600120223         readc SB48S02;
109700120223
109800120223         DOW  not %eof(TNSB48D);
109900120223
110000120223           SflNxtChg = *off;
110100120223
110200120223           WindDspF = IndDspF;
110300120223           reset WindDspFb;
110400120223           IndDspF  = WindDspF;
110500120223
110600120223           C02rcd = S02nrr;
110700120223
110800120223           SELECT;
110900120223
111000120223         //?Nessuna opzione immessa
111100120223             WHEN  S02opz  = *blanks;
111200120223
111300120223         //?Precedente segnalazione di errore
111400120223             WHEN  S02opz  = 'E';
111500120223               clear S02opz;
111600120223
111700120223         //?B = Manutenzione bolla di sede
111800120223             WHEN  S02opz  = 'B';
111900120223               exsr Call_TNSB52;
112000120223
112100120223         //?T = Manutenzione tariffa
112200120223             WHEN  S02opz  = 'T';
112300120223               exsr Call_TNTA01;
112400120326
112500120326         //?V = OK utente
112600120326             WHEN  S02opz  = 'V' and S02imp = 'IMP';
112700120326               exsr Call_visto;
112800120321
112900120321         //?3 = Interrogazione tariffa
113000120321             WHEN  S02opz  = '3';
113100120321               exsr Call_TNTA02;
113200120321
113300120321         //?4 = Interrogazione clienti
113400120321             WHEN  S02opz  = '4';
113500120321               exsr Call_XCLIR;
113600120223
113700120223         //?5 = Visualizza bolla di sede
113800120223             WHEN  S02opz  = '5';
113900120223               exsr Call_TNSB51;
114000120223
114100120223         //?Opzione non valida
114200120223             OTHER;
114300120223               ErrMessage  = *on;
114400120223               ErrGenerico = *on;
114500120223               PosCurOPZ   = *on;
114600120223               V02msg = skMsg(08);
114700120223
114800120223           ENDSL;
114900120223
115000120223         //?Aggiorno il subfile
115100120223           SELECT;
115200120223             WHEN  ErrMessage;
115300120223               SflNxtChg = *on;
115400120223               C02csr    = C02rcd;
115500120223             WHEN  ErrGenerico;
115600120223               C02csr = C02rcd;
115700120223               clear S02opz;
115800120223             OTHER;
115900120223               C02csr = C02rcd;
116000120223               clear S02opz;
116100120223           ENDSL;
116200120223
116300120223           update SB48S02;
116400120223
116500120223           IF  ErrMessage or ErrGenerico;
116600120223             leave;
116700120223           ENDIF;
116800120223
116900120223           readc SB48S02;
117000120223
117100120223         ENDDO;
117200120130
117300120223       ENDSR;
117400120222
117500120222       //--------------------------------------------------------------
117600120222       //?Gestione tasto funzionale F05 da videata S02.
117700120222       //?F05=Ritassa
117800120222       //--------------------------------------------------------------
117900120222       BEGSR F05S02;
118000120224
118100120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
118200120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
118300120224            (V01ccm = *blanks or V01ccm = *zeros);
118400120224           xx = 1;
118500120224           FOR xx by 1 to %elem(pog);
118600120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
118700120224               clear TNSB44DS;
118800120224               D44fil = %dec(pog(xx):3:0);
118900120224               D44sb48 = 'S';
119000120224               kpjbu = TNSB44DS;
119100120224               tnsb45r (kpjba);
119200120224             ENDIF;
119300120224           ENDFOR;
119400120224
119500120224         ELSE;
119600120224
119700120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
119800120224           clear TNSB44DS;
119900120224           D44fil = V01fil;
120000120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
120100120224             D44ccm = %dec(V01ccm:7:0);
120200120224           ENDIF;
120300120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
120400120224             D44cli = %dec(V01ksc:7:0);
120500120224           ENDIF;
120600120224           D44sb48 = 'S';
120700120224           kpjbu = TNSB44DS;
120800120224           tnsb45r (kpjba);
120900120224
121000120224         ENDIF;
121100120224
121200120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
121300120224         wVideo = 'S2';
121400120224         wInzS02 = *on;
121500120222
121600120223       ENDSR;
121700120222
121800120222       //--------------------------------------------------------------
121900120222       //?Gestione tasto funzionale F08 da videata S02.
122000120228       //?F08=Ordinamento
122100120222       //--------------------------------------------------------------
122200120222       BEGSR F08S02;
122300120222
122400120306         rrnlast = sav_S02nrr;
122500120222
122600120222       //?Inizializza i campi chiave x l'ordinamento.
122700120222         clear QLGSCB;
122800120222         clear QLGSCB00;
122900120222
123000120418       //?2 campi chiave
123100120418         QLGNBRK = 2;
123200120222
123300120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
123400120228         IF  Ord_xksc;
123500120228           QLGSP = 1;
123600120228           QLGSS = %SIZE(S02ksc);
123700120228         ENDIF;
123800120418       //?imposto la posizione del Commerciale sul subfile e la lunghezza
123900120228         IF  Ord_xccm;
124000120228           QLGSP = 1 + %size(S02ksc);
124100120228           QLGSS = %SIZE(S02ccm);
124200120228         ENDIF;
124300120418
124400120222         QLGDT = Numerico  ;
124500120222         QLGSO = Ascendente;
124600120222         QLGKL(1) = QLGSKL;
124700120418
124800120418       //?imposto la posizione della Data sul subfile e la lunghezza
124900120418         IF  Ord_xksc;
125000120418           QLGSP = 1 + %size(S02ksc) + %size(S02ccm);
125100120418           QLGSS = %SIZE(S02dsp);
125200120418         ENDIF;
125300120418       //?imposto la posizione del Cliente sul subfile e la lunghezza
125400120418         IF  Ord_xccm;
125500120418           QLGSP = 1;
125600120418           QLGSS = %SIZE(S02ksc);
125700120418         ENDIF;
125800120418         QLGDT = Numerico  ;
125900120418         QLGSO = Ascendente;
126000120418         QLGKL(2) = QLGSKL;
126100120222
126200120222       //?Load other sort parameters.
126300120222         QLGLB = 80 + 16 * MaxKey;
126400120222         QLGRL = %SIZE(sflrcd) - 1;
126500120222         QLGRT = 8;
126600120222         QLGOKL = 80;
126700120222         QLGLKE = 16;
126800120222         QLGLSS = 290;
126900120222
127000120222       //?Initialize Sort I/O API fields.
127100120222         QLGRL00 = QLGRL;
127200120222         QLGRC00 = 1;
127300120222         clear QUSEI;
127400120222         QUSBPRV = %SIZE(QUSEC);
127500120222
127600120222       //?First step - Initialize the sort routine.
127700120222         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
127800120222
127900120222       //?Next step - Write records to I/O routine.
128000120222         QLGRT00 = Put;
128100120222         FOR xx = 1 to rrnlast;
128200120222           chain xx SB48S02;
128300120222
128400120222       //?solo le righe con Selected = 'Y' sono riordinate,
128500120222       //?quindi per fare un ordinamento di tutte le righe
128600120222       //?metto 'Y' sempre.
128700120222           selected  = 'Y';
128800120222           clear QUSEI;
128900120222           QUSBPRV = %SIZE(QUSEC);
129000120222           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
129100120222         ENDFOR;
129200120222
129300120222       //?Next step - Signal end of input, clear subfile for reload.
129400120222         QLGRT00 = EndPut;
129500120222         clear QUSEI;
129600120222         QUSBPRV = %SIZE(QUSEC);
129700120222         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
129800120222
129900120222       //?Pulizia SFL
130000120222         exsr PulS02;
130100120222
130200120222       //?Final step - Write the records back to the subfile.
130300120222         QLGRT00 = Get;
130400120222         FOR xx = 1 to rrnlast;
130500120222           clear QUSEI;
130600120222           QUSBPRV = %SIZE(QUSEC);
130700120222           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
130800120222           S02nrr = xx;
130900120222           write SB48S02;
131000120222         ENDFOR;
131100120222
131200120222       //?Visualizzazione del SFL (se ci sono dati)
131300120223         SflDsp = (S02nrr <= *zeros);
131400120222
131500120222       //?Attivazione del SFLEND
131600120222         SflEnd = *on;
131700120222
131800120222       //?Posizionamento cursore al 1° rcd della pagina
131900120222         IF  S02nrr > *zero;
132000120222           C02rcd = 1;
132100120222         ELSE;
132200120222           clear C02rcd;
132300120222         ENDIF;
132400120222
132500120222         C02csr = C02rcd;
132600120222
132700120222       ENDSR;
132800120222
132900120222       //--------------------------------------------------------------
133000120222       //?Gestione tasto funzionale F12 da videata S02.
133100120222       //?F12=Ritorno
133200120222       //--------------------------------------------------------------
133300120222       BEGSR F12S02;
133400120222
133500120222         wVideo = 'D1';
133600120222         wInzD01 = *on;
133700120222
133800120222       ENDSR;
133900120326
134000120326       //--------------------------------------------------------------
134100120326       //?Richiamo pgm Interrogazione bolle di sede.
134200120326       //--------------------------------------------------------------
134300120326       BEGSR Call_TNSB51;
134400120326
134500120326         clear TNSB51DS;
134600120326         I51op0 = 'B00';
134700120326         I51aas = S02aas;
134800120326         I51lnp = S02lnp;
134900120326         I51nrs = S02nrs;
135000120326         I51nsp = S02nsp;
135100120326         I51tbl = S02tbl;
135200120326         kpjbu = TNSB51DS;
135300120326         ParTnsb = '5';
135400120326         tnsb51c (kpjba:ParTnsb);
135500120326         TNSB51DS = kpjbu;
135600120326         IF  O51err = '1';
135700120326           ErrMessage  = *on;
135800120326           ErrGenerico = *on;
135900120326           PosCurOPZ   = *on;
136000120326           V02msg = O51msg;
136100120326         ENDIF;
136200120326
136300120326       ENDSR;
136400090807
136500120223       //--------------------------------------------------------------
136600120223       //?Richiamo pgm Gestione bolle di sede.
136700120223       //--------------------------------------------------------------
136800120223       BEGSR Call_TNSB52;
136900120223
137000120224         clear TNSB01DS;
137100120327         D01op0 = 'T02';
137200120228         D01aas = S02aas;
137300120224         D01lnp = S02lnp;
137400120228         D01nrs = S02nrs;
137500120228         D01nsp = S02nsp;
137600120228         D01tbl = S02tbl;
137700120224         kpjbu = TNSB01DS;
137800120302         ParTnsb = '2';
137900120302         tnsb51c (kpjba:ParTnsb);
138000120301         TNSB01DS = kpjbu;
138100120301         IF  D01err = '1';
138200120301           ErrMessage  = *on;
138300120301           ErrGenerico = *on;
138400120301           PosCurOPZ   = *on;
138500120301           V02msg = D01msg;
138600120301         ENDIF;
138700120223
138800120223       ENDSR;
138900120223
139000120223       //--------------------------------------------------------------
139100120223       //?Richiamo pgm Gestione Tariffa.
139200120223       //--------------------------------------------------------------
139300120223       BEGSR Call_TNTA01;
139400120223
139500120314         clear TNSB48DS;
139600120314         ISB48ksc  = S02ksc;
139700120314         ISB48rag  = S02rag;
139800120314         ISB48dsp  = S02dsp;
139900120314         ISB48tbl  = S02tbl;
140000120314         ISB48lnp  = S02lnp;
140100120314         ISB48lna  = S02lna;
140200120314         ISB48pro  = S02pro;
140300120314         ISB48cts  = S02cts;
140400120314         ISB48ctr  = S02ctr;
140500120314         ISB48ncl  = S02ncl;
140600120314         ISB48pkf  = S02pkf;
140700120314         ISB48vlf  = S02vlf;
140800120314         ISB48err  = S02err;
140900120314         tnta01r (kpjba:TNSB48DS);
141000120314         IF  OSB48err <> *blanks;
141100120228           ErrMessage  = *on;
141200120228           ErrGenerico = *on;
141300120228           PosCurOPZ   = *on;
141400120228           V02msg = skMsg(08);
141500120228         ENDIF;
141600120223
141700120223       ENDSR;
141800120321
141900120321       //--------------------------------------------------------------
142000120321       //?Richiamo pgm Interrogazione Tariffa.
142100120321       //--------------------------------------------------------------
142200120321       BEGSR Call_TNTA02;
142300120321
142400120321         clear TNSB48DS;
142500120321         ISB48ksc  = S02ksc;
142600120321         ISB48rag  = S02rag;
142700120321         ISB48dsp  = S02dsp;
142800120321         ISB48tbl  = S02tbl;
142900120321         ISB48lnp  = S02lnp;
143000120321         ISB48lna  = S02lna;
143100120321         ISB48pro  = S02pro;
143200120321         ISB48cts  = S02cts;
143300120321         ISB48ctr  = S02ctr;
143400120321         ISB48ncl  = S02ncl;
143500120321         ISB48pkf  = S02pkf;
143600120321         ISB48vlf  = S02vlf;
143700120321         ISB48err  = S02err;
143800120321         tnta02r (kpjba:TNSB48DS);
143900120321         IF  OSB48err <> *blanks;
144000120321           ErrMessage  = *on;
144100120321           ErrGenerico = *on;
144200120321           PosCurOPZ   = *on;
144300120321           V02msg = skMsg(08);
144400120321         ENDIF;
144500120321
144600120321       ENDSR;
144700120326
144800120326       //--------------------------------------------------------------
144900120326       //?Visto: OK dell'utente.
145000120326       //--------------------------------------------------------------
145100120326       BEGSR Call_visto;
145200120326
145300120326         wFineW01 = *off;
145400120326         W01aas = S02aas;
145500120326         W01lnp = S02lnp;
145600120326         W01nrs = S02nrs;
145700120326         W01nsp = S02nsp;
145800120326         W01tbl = S02tbl;
145900120326         W01ksc = S02ksc;
146000120326         W01rag = S02rag;
146100120326         W01imv = S02imv;
146200120326         W01err = S02err;
146300120326
146400120326         DOW  not wFineW01;
146500120326           exfmt  SB48W01;
146600120326           reset ErrMessage;
146700120326           reset ErrGenerico;
146800120326           clear W01msg;
146900120326         //?- F12=Ritorno
147000120326           IF  dsp_aid = c_F12;
147100120326             wFineW01 = *on;
147200120326           ENDIF;
147300120326         //?- F06=Conferma
147400120326           IF  dsp_aid = c_F06;
147500120326             exsr  F06W01;
147600120326             IF  not ErrGenerico;
147700120326               wFineW01 = *on;
147800120326             ENDIF;
147900120326           ENDIF;
148000120326         ENDDO;
148100120326
148200120326       ENDSR;
148300120321
148400120321       //--------------------------------------------------------------
148500120321       //?Richiamo pgm Interrogazione Clienti.
148600120321       //--------------------------------------------------------------
148700120321       BEGSR Call_XCLIR;
148800120321
148900120321         clear xCLIrds;
149000120321         clear par01;
149100120321         parrag = S02rag;
149200120321         DXCop0 = 'OPZ';
149300120321         DXCaut = wUteAbi;
149400120321         DXCcap = DUTkci;
149500120321         xclir (xCLIrds:par01:parrag);
149600120321
149700120321       ENDSR;
149800120326
149900120326       //--------------------------------------------------------------
150000120326       //?Gestione tasto funzionale F06 da videata W01.
150100120326       //?F6=Conferma.
150200120326       //--------------------------------------------------------------
150300120326       BEGSR F06W01;
150400120326
150500120326       //?Ok utente --> Aggiorna il manca tariffa da M a V
150600120329          exec sql
150700120329          UPDATE TFMTC00F
150800120329          SET MTCtrc = 'V'
150900120329          WHERE MTCaas = :W01aas and MTClnp = :W01lnp and
151000120329                MTCnrs = :W01nrs and MTCnsp = :W01nsp and
151100120329                MTCtbl = :W01tbl and MTCtrc = 'M';
151200120326
151300120326       //?OK utente --> Scrive rcd richiesta assistenza
151400120326       //?              richiama FIDNA3R
151500120326          exsr Call_FIDNA3;
151600120326
151700120326       ENDSR;
151800120326
151900120326       //--------------------------------------------------------------
152000120326       //?Richiamo pgm scrittura R.A.
152100120326       //--------------------------------------------------------------
152200120326       BEGSR Call_FIDNA3;
152300120326
152400120326         clear FIDNA3DS;
152500120326         IA3mad = ' 30';
152600120326         IA3tor = 'S';
152700120326         IA3ogg = %editc(W01lnp:'X') + %editc(W01nrs:'X') +
152800120326                  %editc(W01nsp:'X') + %editc(W01aas:'X');
152900120326         IA3no1 = 'Manca tariffa: ' + W01err;
153000120326         IA3no2 = 'Vistato dall''utente';
153100120326         fidna3r (kpjba:FIDNA3DS);
153200120326
153300120326       ENDSR;
153400120223
153500080206       //--------------------------------------------------------------
153600080206       //?Operazioni finali.
153700080206       //--------------------------------------------------------------
153800080206       BEGSR RoutEnd;
153900090521
154000080206         *inLR = *on;
154100080206         return;
154200080206
154300080206       ENDSR;
154400080206
154500080206      /end-free
154600090807
154700080206       //--------------------------------------------------------------
154800080206       //?Schiere a tempo di compilazione.
154900080206       //--------------------------------------------------------------
155000080206
155100120222** - skMSG ------------------------------------------------------------------*
155200120221Utente non abilitato alla funzione                                            01
155300120222Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    02
155400120222Filiale errata o non in gestione all'utente                                   03
155500120222Codice commerciale errato o non in gestione all'utente                        04
155600120222Password errata o non immessa                                                 05
155700120222Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              06
155800120222Password scaduta                                                              07
155900120223Opzione errata                                                                08
156000120223Codice cliente errato o non in gestione all'utente                            09
156100120326Un altro utente sta vistando questo errore!!!                                 10
156200120412Richiedere solo commerciali UNIFICANTI!                                       11
