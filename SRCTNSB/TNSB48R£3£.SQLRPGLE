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
043500120221         ITAA1tipo = 'N';
043600120221         ITAA1caut = '§utegtc';
043700120221         kpjbu     = TNTAA1DS;
043800120221         tntaa1c (kpjba);
043900120221         TNTAA1DS = kpjbu;
044000120221
044100120221         IF  OTAA1fabi = 'N';
044200120221           wErrGrave   = *on;
044300120221           ErrMessage  = *on;
044400120221           ErrGenerico = *on;
044500120221           V01msg = skMsg(01);
044600120221           leavesr;
044700120221         ENDIF;
044800120223
044900120223         wUteAbi = OTAA1cabi;
045000120221
045100120221         clear TRUL31DS;
045200120221         I31abi = OTAA1cabi;
045300120221         I31cdi = DUTdis;
045400120221         I31car = DUTare;
045500120221         I31cpo = DUTpou;
045600120221         trul31r (kpjba : trul31ds);
045700120221         IF  O31pog <= *zeros;
045800120221           wErrGrave   = *on;
045900120221           ErrMessage  = *on;
046000120221           ErrGenerico = *on;
046100120223           V01msg = skMsg(01);
046200120221           leavesr;
046300120221         ENDIF;
046400120309
046500120309       //?Se utente non EDP controllo la filiale, per ora abilitiamo solo 005 e 028
046600120309         IF  %subst(knmus:1:3) <> 'EDP' and
046700120309             DUTpou <> 005 and DUTpou <> 028;
046800120309           wErrGrave   = *on;
046900120309           ErrMessage  = *on;
047000120309           ErrGenerico = *on;
047100120309           V01msg = skMsg(01);
047200120309           leavesr;
047300120309         ENDIF;
047400090806
047500110706       //?Impostazione campi "fissi"
047600110706         V1Tpgm = SDSpgm;
047700120130         k_TBLkut = 1;
047800120221
047900120221       //?Data del giorno
048000120221         wOggi = %dec(%date());
048100120223
048200120223       //?Cerco la tabella gg. validità password
048300120223         clear TIBS02DS;
048400120223         clear dMTC;
048500120223         T02mod = 'C';
048600120223         T02cod = 'MTC';
048700120223         T02sif = knsif;
048800120223         T02ke1 = 'PSW';
048900120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
049000120223         IF  T02err <> *blanks;
049100120223           wErrGrave   = *on;
049200120223           ErrMessage  = *on;
049300120223           ErrGenerico = *on;
049400120223           PosCurPWD   = *on;
049500120223           V01msg      = skMsg(06);
049600120223           leavesr;
049700120223         ENDIF;
049800120223         wGiorni = %dec(T02uni:3:0);
049900120222
050000120222       //?Cerco la password memorizzata per filiale (utente)
050100120222         clear TIBS02DS;
050200120222         clear dMTC;
050300120222         T02mod = 'C';
050400120222         T02cod = 'MTC';
050500120222         T02sif = knsif;
050600120223         T02ke1 = %editc(DUTpou:'X');
050700120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
050800120222         IF  T02err <> *blanks;
050900120222           wErrGrave   = *on;
051000120222           ErrMessage  = *on;
051100120222           ErrGenerico = *on;
051200120222           V01msg = skMsg(02);
051300120222           leavesr;
051400120222         ENDIF;
051500120222         dMTC = T02uni;
051600120223
051700120223         clear sav_Pwd;
051800090714
051900080206       ENDSR;
052000080206
052100080206       //--------------------------------------------------------------
052200080206       //?Reperimento Dati del job (Utente/Operativi).
052300080206       //--------------------------------------------------------------
052400080206       BEGSR DatiJob;
052500080206
052600080206         in(E) §AzUte;
052700110706         IF NOT %error;
052800080206           in(E) §DatiUte;
052900110706         ENDIF;
053000110706         IF %error or RSut = *blanks;
053100080206           clear TIBS34ds;
053200080206           tibs34r(tibs34ds);
053300080206           in §AzUte;
053400080206           in §DatiUte;
053500110706         ENDIF;
053600080206
053700080206       ENDSR;
053800080206
053900090421       //--------------------------------------------------------------
054000110706       //?Gestione videata D01
054100080206       //--------------------------------------------------------------
054200110706       BEGSR GesD01;
054300080207
054400110706       //?Inizializzazione videata
054500120130         IF  wInzD01 = *on;
054600110706            exsr InzD01;
054700120130            wInzD01  = *off;
054800110706         ENDIF;
054900080207
055000110706       //?Emissione Testata
055100120221         write  SB48T01;
055200080207
055300110706       //?Emissione videata
055400120221         exfmt  SB48D01;
055500110706         reset ErrMessage;
055600110706         reset ErrGenerico;
055700120127         clear V01msg;
055800080207
055900080207         SELECT;
056000120227
056100120227         //?- Errore grave esco dal pgm
056200120227           WHEN  wErrGrave;
056300120227             wFine = *on;
056400110706
056500110706         //?- F03=Fine
056600110706           WHEN  dsp_aid = c_F03;
056700110706             exsr F03D01;
056800080207
056900090807         //?Invio
057000080207           OTHER;
057100110706         //?Eseguo i controlli
057200110706             exsr CtrD01;
057300110706             IF  ErrGenerico;
057400080207               leavesr;
057500110706             ENDIF;
057600120127         //?Se tutto OK vado nella seconda videata
057700120130           wVideo = 'S2';
057800120130           wInzS02 = *on;
057900080207
058000080207         ENDSL;
058100080207
058200080207       ENDSR;
058300080207
058400080207       //--------------------------------------------------------------
058500110706       //?Inizializzazione videata D01.
058600080207       //--------------------------------------------------------------
058700110706       BEGSR InzD01;
058800080207
058900120221         clear V01fil;
059000120302         V01dfil = '(0 = Tutte)';
059100120221         clear V01ccm;
059200120221         clear V01dccm;
059300120221         clear V01ksc;
059400120221         clear V01rag;
059500120223
059600120223         V01pwd = sav_Pwd;
059700090508
059800080207       ENDSR;
059900080207
060000091111       //--------------------------------------------------------------
060100110706       //?Gestione tasto funzionale F03 da videata D01.
060200110706       //?F3=Fine
060300091111       //--------------------------------------------------------------
060400110706       BEGSR F03D01;
060500091111
060600110706       //?Chiusura del programma
060700120130         wFine = *on;
060800091111
060900091111       ENDSR;
061000120130
061100120130       //--------------------------------------------------------------
061200120130       //?Controllo dati videata D01.
061300120130       //--------------------------------------------------------------
061400120130       BEGSR CtrD01;
061500120130
061600120130         WindDspF  = IndDspF;
061700120130         reset WindDspFb;
061800120130         IndDspF   = WindDspF;
061900120130
062000120221       //?Controllo la filiale
062100120221         IF  V01fil > 0;
062200120221           chain V01fil AZORG01L;
062300120223           IF not %found(AZORG01L);
062400120221             ErrMessage  = *on;
062500120221             ErrGenerico = *on;
062600120221             PosCurFIL   = *on;
062700120222             V01msg      = skMsg(03);
062800120221             leavesr;
062900120221           ENDIF;
063000120221         //?Verifico se l'utente è abilitato alla filiale
063100120221           w003a = %editc(V01fil:'X');
063200120221           IF  %lookup(w003a:pog) = 0;
063300120221             ErrMessage  = *on;
063400120221             ErrGenerico = *on;
063500120221             PosCurFIL   = *on;
063600120222             V01msg      = skMsg(03);
063700120221             leavesr;
063800120221           ENDIF;
063900120130         ENDIF;
064000120221
064100120221       //?Ricerca del commerciale
064200120221         IF  %scan('?' : V01ccm) > 0;
064300120221           ErrGenerico = *on;
064400120222           PosCurCCM   = *on;
064500120221           TB85kfil    = %editc(DUTpou:'X');
064600120221           clear TB85key;
064700120221           clear TB85des1;
064800120412           trtb85r (kpjba : TB85kfil : TB85key : TB85des1 : tb85Uni);
064900120221           V01ccm  = TB85key;
065000120221           V01dccm = TB85des1;
065100120221           leavesr;
065200120221         ENDIF;
065300120221
065400120221       //?Controllo il commerciale
065500120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
065600120221
065700120221         //?Solo valori numerici
065800120223           IF  %check(c_digits:V01ccm) > 0;
065900120221             ErrMessage  = *on;
066000120221             ErrGenerico = *on;
066100120222             PosCurCCM   = *on;
066200120222             V01msg      = skMsg(04);
066300120221             leavesr;
066400120221           ENDIF;
066500120221
066600120221         //?Controllo se utente abilitato al commerciale
066700120221           clear TNTAA1DS;
066800120221           ITAA1tipo = 'N';
066900120221           ITAA1caut = '§utegtc';
067000120221           ITAA1cmm  = %dec(V01ccm:7:0);
067100120221           kpjbu     = tntaa1ds;
067200120221           tntaa1c (kpjba);
067300120221           TNTAA1DS = kpjbu;
067400120221           IF  OTAA1fabi = 'N';
067500120221             ErrMessage  = *on;
067600120221             ErrGenerico = *on;
067700120222             PosCurCCM   = *on;
067800120222             V01msg      = skMsg(04);
067900120221             leavesr;
068000120221           ENDIF;
068100120221
068200120221         //?Controllo se commerciale valido
068300120221           clear  ds01;
068400120223           k_TBLcod = '01';
068500120223           k_TBLkey = V01ccm;
068600120223           chain  %kds(K03tabel) TABEL00F;
068700120221           IF  not %found(TABEL00F) or TBLflg <> *blanks;
068800120221             ErrMessage  = *on;
068900120221             ErrGenerico = *on;
069000120222             PosCurCCM   = *on;
069100120222             V01msg      = skMsg(04);
069200120221             leavesr;
069300120221           ENDIF;
069400120221
069500120221           ds01    = TBLuni;
069600120221           V01dccm = §01age;
069700120221
069800120221         //?Commerciale senza particolarità
069900120221           IF  §01par <> *blanks;
070000120221             ErrMessage  = *on;
070100120221             ErrGenerico = *on;
070200120222             PosCurCCM   = *on;
070300120222             V01msg      = skMsg(04);
070400120221             leavesr;
070500120221           ENDIF;
070600120221
070700120221         //?Commerciale "valido" data inizio e fine attività
070800120221           IF  §01dtdec > %editc(wOggi:'X') or
070900120221               §01dtfin < %editc(wOggi:'X');
071000120221             ErrMessage = *on;
071100120221             ErrGenerico = *on;
071200120222             PosCurCCM   = *on;
071300120222             V01msg      = skMsg(04);
071400120221             leavesr;
071500120221           ENDIF;
071600120412
071700120412         //?Deve essere un commerciale Unificante
071800120412           IF  %dec(V01ccm:7:0) <> §01rgf;
071900120412             ErrMessage = *on;
072000120412             ErrGenerico = *on;
072100120412             PosCurCCM   = *on;
072200120412             V01msg      = skMsg(11);
072300120412             leavesr;
072400120412           ENDIF;
072500120221         ENDIF;
072600120223
072700120223       //?Ricerca del cliente
072800120223         IF  %scan('?' : V01ksc) > 0;
072900120223           ErrGenerico = *on;
073000120223           PosCurKSC   = *on;
073100120223           clear xCLIrds;
073200120321           clear par01;
073300120321           clear parrag;
073400120223           DXCaut = wUteAbi;
073500120223           DXCcap = DUTkci;
073600120321           xclir (xCLIrds:par01:parrag);
073700120223           ksa = DXCksc;
073800120223           dsksa = ksa(1);
073900120223           ksc(1) = dsksn;
074000120223           V01ksc = %editc(ksc(1):'X');
074100120223         ENDIF;
074200120223
074300120223       //?Controllo il cleinte
074400120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
074500120223
074600120223         //?Solo valori numerici
074700120223           IF  %check(c_digits:V01ksc) > 0;
074800120223             ErrMessage  = *on;
074900120223             ErrGenerico = *on;
075000120223             PosCurKSC   = *on;
075100120223             V01msg      = skMsg(09);
075200120223             leavesr;
075300120223           ENDIF;
075400120223
075500120223         //?Controllo se utente abilitato al cliente
075600120223           clear TNTAA1DS;
075700120223           ITAA1tipo = 'N';
075800120223           ITAA1caut = '§utegtc';
075900120223           ITAA1ksc  = %dec(V01ksc:7:0);
076000120223           kpjbu     = tntaa1ds;
076100120223           tntaa1c (kpjba);
076200120223           TNTAA1DS = kpjbu;
076300120223           IF  OTAA1fabi = 'N';
076400120223             ErrMessage  = *on;
076500120223             ErrGenerico = *on;
076600120223             PosCurKSC   = *on;
076700120223             V01msg      = skMsg(09);
076800120223             leavesr;
076900120223           ENDIF;
077000120223
077100120223         //?Controllo se cliente valido
077200120223           clear tibs69ds;
077300120223           I69kac = %dec(V01ksc:7:0);
077400120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
077500120223           IF  O69err <> *blanks;
077600120223             ErrMessage  = *on;
077700120223             ErrGenerico = *on;
077800120223             PosCurKSC   = *on;
077900120223             V01msg      = skMsg(09);
078000120223             leavesr;
078100120223           ENDIF;
078200120223           V01rag = ACOrag;
078300120223         ENDIF;
078400120221
078500120222       //?Password obbligatoria
078600120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
078700120222           ErrMessage = *on;
078800120222           ErrGenerico = *on;
078900120222           PosCurPWD   = *on;
079000120222           V01msg      = skMsg(05);
079100120222           leavesr;
079200120222         ENDIF;
079300120222
079400120222       //?Controllo se password OK
079500120222         IF  V01pwd <> §MTCpwd;
079600120222           ErrMessage = *on;
079700120222           ErrGenerico = *on;
079800120222           PosCurPWD   = *on;
079900120222           V01msg      = skMsg(05);
080000120222           clear V01pwd;
080100120222           leavesr;
080200120222         ENDIF;
080300120222
080400120222       //?Data immissione/modifica password
080500120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
080600120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
080700120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
080800120223         wDataISO = %date(wData);
080900120223
081000120223       //?Calcolo la data scadenza password
081100120223         wDataISO += %days(wgiorni);
081200120223         wDataScad = %dec(wDataISO);
081300120223
081400120223       //?Controllo se la password è scaduta
081500120222         IF wOggi > wDataScad;
081600120222           ErrMessage  = *on;
081700120222           ErrGenerico = *on;
081800120222           PosCurPWD   = *on;
081900120222           V01msg      = skMsg(07);
082000120222           leavesr;
082100120222         ENDIF;
082200120223
082300120223         sav_Pwd = V01pwd;
082400120130
082500120130       ENDSR;
082600120130
082700120130       //--------------------------------------------------------------
082800120130       //?Gestione videata S02
082900120130       //--------------------------------------------------------------
083000120130       BEGSR GesS02;
083100120306
083200120306       //?Se non l'ho ancora fatto tasso tutto
083300120306       //?(capita solo appena entro nel pgm)
083400120306         IF  not wTassaPrima;
083500120309           exsr F05S02;
083600120306           wTassaPrima = *on;
083700120306         ENDIF;
083800120130
083900120130       //?Inizializzazione videata
084000120130         IF  wInzS02 = *on;
084100120130            exsr InzS02;
084200120130            wInzS02  = *off;
084300120130         ENDIF;
084400120130
084500120130       //?Emissione Testata + Piede
084600120221         write  SB48T01;
084700120221         write  SB48P02;
084800120130
084900120130       //?Emissione subfile
085000120221         exfmt  SB48C02;
085100120130         reset ErrMessage;
085200120130         reset ErrGenerico;
085300120130         clear V02msg;
085400120130
085500120130         SELECT;
085600120130
085700120130         //?- F03=Fine
085800120130           WHEN  dsp_aid = c_F03;
085900120130             exsr F03D01;
086000120130
086100120222         //?- F05=Ritassa
086200120222           WHEN  dsp_aid = c_F05;
086300120222             exsr F05S02;
086400120222
086500120228         //?- F08=Ordina
086600120222           WHEN  dsp_aid = c_F08;
086700120229           //?Se era ordina per cliente diventa ordina per commerciale
086800120229             IF  Ord_xksc;
086900120229               Ord_xksc = *off;
087000120229               Ord_Xccm = *on;
087100120229               wF08 = 'F8=Ord.x Cliente';
087200120229           //?Se era ordina per cliente diventa ordina per commerciale
087300120229             ELSE;
087400120229               Ord_xksc = *on;
087500120229               Ord_Xccm = *off;
087600120229               wF08 = 'F8=Ord.x Commerciale';
087700120229             ENDIF;
087800120222             exsr F08S02;
087900120130
088000120130         //?- F12=Ritorno
088100120130           WHEN  dsp_aid = c_F12;
088200120130             exsr F12S02;
088300120130
088400120130         //?Invio
088500120130           OTHER;
088600120222         //?Opzioni
088700120223             IF  S02nrr > *zeros;
088800120223               exsr OpzS02 ;
088900120223             ENDIF;
089000120130             IF  ErrGenerico;
089100120130               leavesr;
089200120130             ENDIF;
089300120130
089400120130         ENDSL;
089500120130
089600120130       ENDSR;
089700120130
089800120130       //--------------------------------------------------------------
089900120130       //?Inizializzazione videata S02.
090000120130       //--------------------------------------------------------------
090100120130       BEGSR InzS02;
090200120130
090300120130       //?Pulizia subfile
090400120130         exsr PulS02;
090500120222
090600120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
090700120222         exsr PrepSql;
090800120130
090900120130       //?Caricamento dati
091000120130         exsr CarS02;
091100120130
091200120130       //?Visualizzazione del Subfile
091300120130         SflDsp = (S02nrr <= *zeros);
091400120130
091500120130       //?Attivazione SFLEND
091600120130         SflEnd = *on;
091700120130
091800120130       //?Impaginazione Subfile
091900120130       //?Forzo sempre il primo rcd
092000120130         IF  S02nrr > *zero;
092100120130           C02rcd = 1;
092200120130         ELSE;
092300120130           clear C02rcd;
092400120130         ENDIF;
092500120130
092600120130         C02csr = C02rcd;
092700120130
092800120130       ENDSR;
092900120130
093000120130       //--------------------------------------------------------------
093100120130       //?Pulizia Subfile S02.
093200120130       //--------------------------------------------------------------
093300120130       BEGSR PulS02;
093400120130
093500120130       //?Pulizia subfile
093600120130         SflDsp    = *on;
093700120130         SflDspCtl = *on;
093800120221         write  SB48C02;
093900120130         SflDspCtl = *off;
094000120130         SflEnd    = *off;
094100120130
094200120130         clear C02rcd;
094300120130         clear C02csr;
094400120130         clear S02nrr;
094500120130         clear V02msg;
094600120130         ErrMessage  = *off;
094700120130         ErrGenerico = *off;
094800120130
094900120130         WindDspF = IndDspF;
095000120130         reset WindDspFb;
095100120130         IndDspF  = WindDspF;
095200120130
095300120130       ENDSR;
095400120222
095500120222       //--------------------------------------------------------------
095600120222       //?Preparazione stringa SQL.
095700120222       //--------------------------------------------------------------
095800120222       BEGSR PrepSql;
095900120307
096000120307         clear w0070;
096100120222
096200120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
096300120222
096400120307       //?Se nessuna richiesta a video
096500120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
096600120307            (V01ccm = *blanks or V01ccm = *zeros);
096700120222       //?Carico le bolle con filiale cliente = sk POG
096800120222           wSQL += ' and MTCpoc in(';
096900120222           yy = 0;
097000120222           xx = 1;
097100120222           FOR xx by 1 to %elem(pog);
097200120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
097300120222               IF yy > 0;
097400120222                 wSQL += ', ';
097500120222               ELSE;
097600120222               yy = 1;
097700120222               ENDIF;
097800120222               wSQL += '''';
097900120222               wSQL += pog(xx);
098000120222               wSQL += '''';
098100120222             ENDIF;
098200120222           ENDFOR;
098300120222           wSQL += ')';
098400120222         ELSE;
098500120307         //?Se richiesta filiale a video
098600120307           IF  V01fil > 0;
098700120307         //?Carico le bolle con filiale cliente = filiale richiesta
098800120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
098900120307           ENDIF;
099000120307         //?Se richiesto commerciale a video
099100120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
099200120307              w0070 = %dec(V01ccm:7:0);
099300120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
099400120307           ENDIF;
099500120307         //?Se richiesto cliente a video
099600120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
099700120307              w0070 = %dec(V01ksc:7:0);
099800120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
099900120307           ENDIF;
100000120222         ENDIF;
100100120309
100200120309         wSQL += ' for FETCH ONLY';
100300120222
100400120222       ENDSR;
100500120130
100600120130       //--------------------------------------------------------------
100700120130       //?Caricamento Subfile S02.
100800120130       //--------------------------------------------------------------
100900120130       BEGSR CarS02;
101000120312
101100120312         clear sav_s02nrr;
101200120130
101300120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
101400120222         exec sql prepare A1 from :wSQL;
101500120222         exec sql declare MTC cursor for A1;
101600120222         exec sql open MTC;
101700120222
101800120222         IF  sqlcode < 0;
101900120222           wEoF = *on;
102000120222         ENDIF;
102100120222
102200120222         DOW  not wEoF;
102300120309           exec sql fetch next from MTC into :TFMTCds;
102400120222           IF  sqlcod = 100 or sqlcod < 0;
102500120222             wEoF = *on;
102600120222             leave;
102700120222           ENDIF;
102800120222
102900120222         //?Carico il subfile
103000120222           exsr RieS02;
103100120222         //?Scrico il subfile
103200120222           S02nrr += 1;
103300120222           write SB48S02;
103400120306           sav_S02nrr = S02nrr;
103500120229           IF  S02nrr = 9998;
103600120229             leave;
103700120229           ENDIF;
103800120222
103900120222         ENDDO;
104000120222
104100120222         exec sql close MTC;
104200120130         wEoF = *off;
104300120222
104400120222         //?Dopo aver caricato il subfile lo ordino per cliente
104500120229         wF08 = 'F8=Ord.x Commerciale';
104600120228         Ord_xksc = *on;
104700120228         Ord_Xccm = *off;
104800120228         exsr F08S02;
104900120130
105000120130       ENDSR;
105100120130
105200120130       //--------------------------------------------------------------
105300120130       //?Carico i dati nel subfile S02.
105400120130       //--------------------------------------------------------------
105500120130       BEGSR RieS02;
105600120130
105700120221         clear  SB48S02;
105800120130
105900120228         S02aas = MTCaas;
106000120228         S02nrs = MTCnrs;
106100120228         S02nsp = MTCnsp;
106200120228         S02tbl = MTCtbl;
106300120222
106400120222         S02ksc = MTCksc;
106500120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
106600120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
106700120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
106800120222         S02ctr = MTCctr;
106900120222         S02ccm = MTCage;
107000120222         S02err = MTCerr;
107100120223         S02lnp = MTClnp;
107200120223         S02lna = MTClna;
107300120223         S02tsp = MTCtsp;
107400120228         S02pro = MTCpro;
107500120228         S02cts = MTCcts;
107600120305         S02ncl = MTCncl;
107700120305         S02pkf = MTCpkf;
107800120305         S02vlf = MTCvlf;
107900120326
108000120326         S02imp = MTCimp;
108100120326         S02imv = MTCimv;
108200120130
108300120222       //?Decodifico il cliente
108400120222         clear tibs69ds;
108500120222         I69kac = S02ksc;
108600120222         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
108700120222         IF  O69err = *blanks;
108800120222           S02rag = ACOrag;
108900120222         ENDIF;
109000120222
109100120222       //?Decodifico il commerciale
109200120222         clear ds01;
109300120223         k_TBLcod = '01';
109400120223         k_TBLkey = %editc(S02ccm:'X');
109500120223         chain  %kds(K03tabel) TABEL00F;
109600120222         IF  %found(TABEL00F) and TBLflg = *blanks;
109700120222           ds01 = TBLuni;
109800120222         ENDIF;
109900120223         S02dccm = §01age;
110000120130
110100120130       ENDSR;
110200120130
110300120130       //--------------------------------------------------------------
110400120222       //?Gestione delle opzioni immesse nella videata S02.
110500120130       //--------------------------------------------------------------
110600120222       BEGSR OpzS02;
110700120130
110800120223         readc SB48S02;
110900120223
111000120223         DOW  not %eof(TNSB48D);
111100120223
111200120223           SflNxtChg = *off;
111300120223
111400120223           WindDspF = IndDspF;
111500120223           reset WindDspFb;
111600120223           IndDspF  = WindDspF;
111700120223
111800120223           C02rcd = S02nrr;
111900120223
112000120223           SELECT;
112100120223
112200120223         //?Nessuna opzione immessa
112300120223             WHEN  S02opz  = *blanks;
112400120223
112500120223         //?Precedente segnalazione di errore
112600120223             WHEN  S02opz  = 'E';
112700120223               clear S02opz;
112800120223
112900120223         //?B = Manutenzione bolla di sede
113000120223             WHEN  S02opz  = 'B';
113100120223               exsr Call_TNSB52;
113200120223
113300120223         //?T = Manutenzione tariffa
113400120223             WHEN  S02opz  = 'T';
113500120223               exsr Call_TNTA01;
113600120326
113700120326         //?V = OK utente
113800120326             WHEN  S02opz  = 'V' and S02imp = 'IMP';
113900120326               exsr Call_visto;
114000120321
114100120321         //?3 = Interrogazione tariffa
114200120321             WHEN  S02opz  = '3';
114300120321               exsr Call_TNTA02;
114400120321
114500120321         //?4 = Interrogazione clienti
114600120321             WHEN  S02opz  = '4';
114700120321               exsr Call_XCLIR;
114800120223
114900120223         //?5 = Visualizza bolla di sede
115000120223             WHEN  S02opz  = '5';
115100120223               exsr Call_TNSB51;
115200120223
115300120223         //?Opzione non valida
115400120223             OTHER;
115500120223               ErrMessage  = *on;
115600120223               ErrGenerico = *on;
115700120223               PosCurOPZ   = *on;
115800120223               V02msg = skMsg(08);
115900120223
116000120223           ENDSL;
116100120223
116200120223         //?Aggiorno il subfile
116300120223           SELECT;
116400120223             WHEN  ErrMessage;
116500120223               SflNxtChg = *on;
116600120223               C02csr    = C02rcd;
116700120223             WHEN  ErrGenerico;
116800120223               C02csr = C02rcd;
116900120223               clear S02opz;
117000120223             OTHER;
117100120223               C02csr = C02rcd;
117200120223               clear S02opz;
117300120223           ENDSL;
117400120223
117500120223           update SB48S02;
117600120223
117700120223           IF  ErrMessage or ErrGenerico;
117800120223             leave;
117900120223           ENDIF;
118000120223
118100120223           readc SB48S02;
118200120223
118300120223         ENDDO;
118400120130
118500120223       ENDSR;
118600120222
118700120222       //--------------------------------------------------------------
118800120222       //?Gestione tasto funzionale F05 da videata S02.
118900120222       //?F05=Ritassa
119000120222       //--------------------------------------------------------------
119100120222       BEGSR F05S02;
119200120224
119300120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
119400120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
119500120224            (V01ccm = *blanks or V01ccm = *zeros);
119600120224           xx = 1;
119700120224           FOR xx by 1 to %elem(pog);
119800120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
119900120224               clear TNSB44DS;
120000120224               D44fil = %dec(pog(xx):3:0);
120100120224               D44sb48 = 'S';
120200120224               kpjbu = TNSB44DS;
120300120224               tnsb45r (kpjba);
120400120224             ENDIF;
120500120224           ENDFOR;
120600120224
120700120224         ELSE;
120800120224
120900120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
121000120224           clear TNSB44DS;
121100120224           D44fil = V01fil;
121200120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
121300120224             D44ccm = %dec(V01ccm:7:0);
121400120224           ENDIF;
121500120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
121600120224             D44cli = %dec(V01ksc:7:0);
121700120224           ENDIF;
121800120224           D44sb48 = 'S';
121900120224           kpjbu = TNSB44DS;
122000120224           tnsb45r (kpjba);
122100120224
122200120224         ENDIF;
122300120224
122400120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
122500120224         wVideo = 'S2';
122600120224         wInzS02 = *on;
122700120222
122800120223       ENDSR;
122900120222
123000120222       //--------------------------------------------------------------
123100120222       //?Gestione tasto funzionale F08 da videata S02.
123200120228       //?F08=Ordinamento
123300120222       //--------------------------------------------------------------
123400120222       BEGSR F08S02;
123500120222
123600120306         rrnlast = sav_S02nrr;
123700120222
123800120222       //?Inizializza i campi chiave x l'ordinamento.
123900120222         clear QLGSCB;
124000120222         clear QLGSCB00;
124100120222
124200120418       //?2 campi chiave
124300120418         QLGNBRK = 2;
124400120222
124500120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
124600120228         IF  Ord_xksc;
124700120228           QLGSP = 1;
124800120228           QLGSS = %SIZE(S02ksc);
124900120228         ENDIF;
125000120418       //?imposto la posizione del Commerciale sul subfile e la lunghezza
125100120228         IF  Ord_xccm;
125200120228           QLGSP = 1 + %size(S02ksc);
125300120228           QLGSS = %SIZE(S02ccm);
125400120228         ENDIF;
125500120418
125600120222         QLGDT = Numerico  ;
125700120222         QLGSO = Ascendente;
125800120222         QLGKL(1) = QLGSKL;
125900120418
126000120418       //?imposto la posizione della Data sul subfile e la lunghezza
126100120418         IF  Ord_xksc;
126200120418           QLGSP = 1 + %size(S02ksc) + %size(S02ccm);
126300120418           QLGSS = %SIZE(S02dsp);
126400120418         ENDIF;
126500120418       //?imposto la posizione del Cliente sul subfile e la lunghezza
126600120418         IF  Ord_xccm;
126700120418           QLGSP = 1;
126800120418           QLGSS = %SIZE(S02ksc);
126900120418         ENDIF;
127000120418         QLGDT = Numerico  ;
127100120418         QLGSO = Ascendente;
127200120418         QLGKL(2) = QLGSKL;
127300120222
127400120222       //?Load other sort parameters.
127500120222         QLGLB = 80 + 16 * MaxKey;
127600120222         QLGRL = %SIZE(sflrcd) - 1;
127700120222         QLGRT = 8;
127800120222         QLGOKL = 80;
127900120222         QLGLKE = 16;
128000120222         QLGLSS = 290;
128100120222
128200120222       //?Initialize Sort I/O API fields.
128300120222         QLGRL00 = QLGRL;
128400120222         QLGRC00 = 1;
128500120222         clear QUSEI;
128600120222         QUSBPRV = %SIZE(QUSEC);
128700120222
128800120222       //?First step - Initialize the sort routine.
128900120222         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
129000120222
129100120222       //?Next step - Write records to I/O routine.
129200120222         QLGRT00 = Put;
129300120222         FOR xx = 1 to rrnlast;
129400120222           chain xx SB48S02;
129500120222
129600120222       //?solo le righe con Selected = 'Y' sono riordinate,
129700120222       //?quindi per fare un ordinamento di tutte le righe
129800120222       //?metto 'Y' sempre.
129900120222           selected  = 'Y';
130000120222           clear QUSEI;
130100120222           QUSBPRV = %SIZE(QUSEC);
130200120222           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
130300120222         ENDFOR;
130400120222
130500120222       //?Next step - Signal end of input, clear subfile for reload.
130600120222         QLGRT00 = EndPut;
130700120222         clear QUSEI;
130800120222         QUSBPRV = %SIZE(QUSEC);
130900120222         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
131000120222
131100120222       //?Pulizia SFL
131200120222         exsr PulS02;
131300120222
131400120222       //?Final step - Write the records back to the subfile.
131500120222         QLGRT00 = Get;
131600120222         FOR xx = 1 to rrnlast;
131700120222           clear QUSEI;
131800120222           QUSBPRV = %SIZE(QUSEC);
131900120222           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
132000120222           S02nrr = xx;
132100120222           write SB48S02;
132200120222         ENDFOR;
132300120222
132400120222       //?Visualizzazione del SFL (se ci sono dati)
132500120223         SflDsp = (S02nrr <= *zeros);
132600120222
132700120222       //?Attivazione del SFLEND
132800120222         SflEnd = *on;
132900120222
133000120222       //?Posizionamento cursore al 1° rcd della pagina
133100120222         IF  S02nrr > *zero;
133200120222           C02rcd = 1;
133300120222         ELSE;
133400120222           clear C02rcd;
133500120222         ENDIF;
133600120222
133700120222         C02csr = C02rcd;
133800120222
133900120222       ENDSR;
134000120222
134100120222       //--------------------------------------------------------------
134200120222       //?Gestione tasto funzionale F12 da videata S02.
134300120222       //?F12=Ritorno
134400120222       //--------------------------------------------------------------
134500120222       BEGSR F12S02;
134600120222
134700120222         wVideo = 'D1';
134800120222         wInzD01 = *on;
134900120222
135000120222       ENDSR;
135100120326
135200120326       //--------------------------------------------------------------
135300120326       //?Richiamo pgm Interrogazione bolle di sede.
135400120326       //--------------------------------------------------------------
135500120326       BEGSR Call_TNSB51;
135600120326
135700120326         clear TNSB51DS;
135800120326         I51op0 = 'B00';
135900120326         I51aas = S02aas;
136000120326         I51lnp = S02lnp;
136100120326         I51nrs = S02nrs;
136200120326         I51nsp = S02nsp;
136300120326         I51tbl = S02tbl;
136400120326         kpjbu = TNSB51DS;
136500120326         ParTnsb = '5';
136600120326         tnsb51c (kpjba:ParTnsb);
136700120326         TNSB51DS = kpjbu;
136800120326         IF  O51err = '1';
136900120326           ErrMessage  = *on;
137000120326           ErrGenerico = *on;
137100120326           PosCurOPZ   = *on;
137200120326           V02msg = O51msg;
137300120326         ENDIF;
137400120326
137500120326       ENDSR;
137600090807
137700120223       //--------------------------------------------------------------
137800120223       //?Richiamo pgm Gestione bolle di sede.
137900120223       //--------------------------------------------------------------
138000120223       BEGSR Call_TNSB52;
138100120223
138200120224         clear TNSB01DS;
138300120327         D01op0 = 'T02';
138400120228         D01aas = S02aas;
138500120224         D01lnp = S02lnp;
138600120228         D01nrs = S02nrs;
138700120228         D01nsp = S02nsp;
138800120228         D01tbl = S02tbl;
138900120224         kpjbu = TNSB01DS;
139000120302         ParTnsb = '2';
139100120302         tnsb51c (kpjba:ParTnsb);
139200120301         TNSB01DS = kpjbu;
139300120301         IF  D01err = '1';
139400120301           ErrMessage  = *on;
139500120301           ErrGenerico = *on;
139600120301           PosCurOPZ   = *on;
139700120301           V02msg = D01msg;
139800120301         ENDIF;
139900120223
140000120223       ENDSR;
140100120223
140200120223       //--------------------------------------------------------------
140300120223       //?Richiamo pgm Gestione Tariffa.
140400120223       //--------------------------------------------------------------
140500120223       BEGSR Call_TNTA01;
140600120223
140700120314         clear TNSB48DS;
140800120314         ISB48ksc  = S02ksc;
140900120314         ISB48rag  = S02rag;
141000120314         ISB48dsp  = S02dsp;
141100120314         ISB48tbl  = S02tbl;
141200120314         ISB48lnp  = S02lnp;
141300120314         ISB48lna  = S02lna;
141400120314         ISB48pro  = S02pro;
141500120314         ISB48cts  = S02cts;
141600120314         ISB48ctr  = S02ctr;
141700120314         ISB48ncl  = S02ncl;
141800120314         ISB48pkf  = S02pkf;
141900120314         ISB48vlf  = S02vlf;
142000120314         ISB48err  = S02err;
142100120314         tnta01r (kpjba:TNSB48DS);
142200120314         IF  OSB48err <> *blanks;
142300120228           ErrMessage  = *on;
142400120228           ErrGenerico = *on;
142500120228           PosCurOPZ   = *on;
142600120228           V02msg = skMsg(08);
142700120228         ENDIF;
142800120223
142900120223       ENDSR;
143000120321
143100120321       //--------------------------------------------------------------
143200120321       //?Richiamo pgm Interrogazione Tariffa.
143300120321       //--------------------------------------------------------------
143400120321       BEGSR Call_TNTA02;
143500120321
143600120321         clear TNSB48DS;
143700120321         ISB48ksc  = S02ksc;
143800120321         ISB48rag  = S02rag;
143900120321         ISB48dsp  = S02dsp;
144000120321         ISB48tbl  = S02tbl;
144100120321         ISB48lnp  = S02lnp;
144200120321         ISB48lna  = S02lna;
144300120321         ISB48pro  = S02pro;
144400120321         ISB48cts  = S02cts;
144500120321         ISB48ctr  = S02ctr;
144600120321         ISB48ncl  = S02ncl;
144700120321         ISB48pkf  = S02pkf;
144800120321         ISB48vlf  = S02vlf;
144900120321         ISB48err  = S02err;
145000120321         tnta02r (kpjba:TNSB48DS);
145100120321         IF  OSB48err <> *blanks;
145200120321           ErrMessage  = *on;
145300120321           ErrGenerico = *on;
145400120321           PosCurOPZ   = *on;
145500120321           V02msg = skMsg(08);
145600120321         ENDIF;
145700120321
145800120321       ENDSR;
145900120326
146000120326       //--------------------------------------------------------------
146100120326       //?Visto: OK dell'utente.
146200120326       //--------------------------------------------------------------
146300120326       BEGSR Call_visto;
146400120326
146500120326         wFineW01 = *off;
146600120326         W01aas = S02aas;
146700120326         W01lnp = S02lnp;
146800120326         W01nrs = S02nrs;
146900120326         W01nsp = S02nsp;
147000120326         W01tbl = S02tbl;
147100120326         W01ksc = S02ksc;
147200120326         W01rag = S02rag;
147300120326         W01imv = S02imv;
147400120326         W01err = S02err;
147500120326
147600120326         DOW  not wFineW01;
147700120326           exfmt  SB48W01;
147800120326           reset ErrMessage;
147900120326           reset ErrGenerico;
148000120326           clear W01msg;
148100120326         //?- F12=Ritorno
148200120326           IF  dsp_aid = c_F12;
148300120326             wFineW01 = *on;
148400120326           ENDIF;
148500120326         //?- F06=Conferma
148600120326           IF  dsp_aid = c_F06;
148700120326             exsr  F06W01;
148800120326             IF  not ErrGenerico;
148900120326               wFineW01 = *on;
149000120326             ENDIF;
149100120326           ENDIF;
149200120326         ENDDO;
149300120326
149400120326       ENDSR;
149500120321
149600120321       //--------------------------------------------------------------
149700120321       //?Richiamo pgm Interrogazione Clienti.
149800120321       //--------------------------------------------------------------
149900120321       BEGSR Call_XCLIR;
150000120321
150100120321         clear xCLIrds;
150200120321         clear par01;
150300120321         parrag = S02rag;
150400120321         DXCop0 = 'OPZ';
150500120321         DXCaut = wUteAbi;
150600120321         DXCcap = DUTkci;
150700120321         xclir (xCLIrds:par01:parrag);
150800120321
150900120321       ENDSR;
151000120326
151100120326       //--------------------------------------------------------------
151200120326       //?Gestione tasto funzionale F06 da videata W01.
151300120326       //?F6=Conferma.
151400120326       //--------------------------------------------------------------
151500120326       BEGSR F06W01;
151600120326
151700120326       //?Ok utente --> Aggiorna il manca tariffa da M a V
151800120329          exec sql
151900120329          UPDATE TFMTC00F
152000120329          SET MTCtrc = 'V'
152100120329          WHERE MTCaas = :W01aas and MTClnp = :W01lnp and
152200120329                MTCnrs = :W01nrs and MTCnsp = :W01nsp and
152300120329                MTCtbl = :W01tbl and MTCtrc = 'M';
152400120326
152500120326       //?OK utente --> Scrive rcd richiesta assistenza
152600120326       //?              richiama FIDNA3R
152700120326          exsr Call_FIDNA3;
152800120326
152900120326       ENDSR;
153000120326
153100120326       //--------------------------------------------------------------
153200120326       //?Richiamo pgm scrittura R.A.
153300120326       //--------------------------------------------------------------
153400120326       BEGSR Call_FIDNA3;
153500120326
153600120326         clear FIDNA3DS;
153700120326         IA3mad = ' 30';
153800120326         IA3tor = 'S';
153900120326         IA3ogg = %editc(W01lnp:'X') + %editc(W01nrs:'X') +
154000120326                  %editc(W01nsp:'X') + %editc(W01aas:'X');
154100120326         IA3no1 = 'Manca tariffa: ' + W01err;
154200120326         IA3no2 = 'Vistato dall''utente';
154300120326         fidna3r (kpjba:FIDNA3DS);
154400120326
154500120326       ENDSR;
154600120223
154700080206       //--------------------------------------------------------------
154800080206       //?Operazioni finali.
154900080206       //--------------------------------------------------------------
155000080206       BEGSR RoutEnd;
155100090521
155200080206         *inLR = *on;
155300080206         return;
155400080206
155500080206       ENDSR;
155600080206
155700080206      /end-free
155800090807
155900080206       //--------------------------------------------------------------
156000080206       //?Schiere a tempo di compilazione.
156100080206       //--------------------------------------------------------------
156200080206
156300120222** - skMSG ------------------------------------------------------------------*
156400120221Utente non abilitato alla funzione                                            01
156500120222Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    02
156600120222Filiale errata o non in gestione all'utente                                   03
156700120222Codice commerciale errato o non in gestione all'utente                        04
156800120222Password errata o non immessa                                                 05
156900120222Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              06
157000120222Password scaduta                                                              07
157100120223Opzione errata                                                                08
157200120223Codice cliente errato o non in gestione all'utente                            09
157300120326Un altro utente sta vistando questo errore!!!                                 10
157400120412Richiedere solo commerciali UNIFICANTI!                                       11
