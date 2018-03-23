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
020400120302
020500120302       // -?Campo da passare al pgm x la gestione/interrogazione bolle
020600120302     d ParTnsb         s              1a
020700120321
020800120321       // -?Campi da passare al pgm x interrogazione clienti
020900120321     d Par01           s              2a
021000120321     d Parrag          s             48a
021100120223
021200120222       // -?Indici di schiera
021300120222     d xx              s              4  0 inz
021400120222     d yy              s              4  0 inz
021500090806
021600110706       // -?Stringa SQL da eseguire
021700090806     d wSQL            s           2048    Varying        inz
021800120221
021900120221      // ----------------------------------------------------------------------
022000120221      //?Costanti per ordinamento subfile
022100120221      // ----------------------------------------------------------------------
022200120221     d MaxKey          c                   4
022300120221     d Ascendente      c                   1
022400120221     d Discendente     c                   2
022500120221     d Carattere       c                   6
022600120221     d Numerico        c                   8
022700120221     d Put             c                   1
022800120221     d EndPut          c                   2
022900120221     d Get             c                   3
023000120221      // ----------------------------------------------------------------------
023100120221      // Campi utili:
023200120221      //     nrr        - Numero relativo di record del Subfile
023300120221      //     SizeList   - Dimensione della lista
023400120221      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
023500120221      // ----------------------------------------------------------------------
023600120221     d NotUsed         s             16A
023700120221     d ReturnSize      s              9B 0
023800120221     d SizeList        s              9B 0
023900120221     d RrnLast         s              5I 0
024000120221     d WrkSort         s              1  0 inz(0)
024100120221      // ----------------------------------------------------------------------
024200120221      // Data Structures
024300120221      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
024400120221      //     QLGSCB     - The sort request block for the QLGSORT API
024500120221      //     QLGSCB00   - The sort request block for the QLGSRTIO API
024600120221      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
024700120221      //     QUSEC      - Error structure for the QLGSORT API
024800120221      // ----------------------------------------------------------------------
024900120221     d sflrcd          ds
025000120223     d  s02ksc
025100120223     d  s02ccm
025200120223     d  s02dsp
025300120223     d  s02lnp
025400120223     d  s02lna
025500120223     d  s02ctr
025600120223     d  s02tsp
025700120223     d  s02opz
025800120223     d  s02rag
025900120223     d  s02dccm
026000120224     d  s02err
026100120228     d  s02aas
026200120228     d  s02nrs
026300120228     d  s02nsp
026400120228     d  s02tbl
026500120228     d  s02pro
026600120228     d  s02cts
026700120305     d  s02ncl
026800120305     d  s02pkf
026900120305     d  s02vlf
027000120326     d  s02imp
027100120326     d  s02imv
027200120221     d  selected                      1A
027300120221
027400120221      /COPY QSYSINC/QRPGLESRC,QLGSORT
027500120221     d QLGKL                         16    DIM(MaxKey)
027600120221     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
027700120221     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
027800120221     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
027900120221     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
028000120221
028100120221      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
028200120221      /COPY QSYSINC/QRPGLESRC,QUSEC
028300090508
028400090508      //---------------------------------------------------------------
028500090807      //?Definizione procedure esterne.
028600090508      //---------------------------------------------------------------
028700120221
028800120221       // -?Api x ordinamento subfile
028900120221     d Qlgsort_pr      pr                  extpgm('QLGSORT')
029000120221     d  pr_QLGSCB                          like(Qlgscb)
029100120221     d  pr_NotUsed1                        like(NotUsed)
029200120221     d  pr_NotUsed2                        like(NotUsed)
029300120221     d  pr_SizeList                        like(SizeList)
029400120221     d  pr_ReturnSize                      like(ReturnSize)
029500120221     d  pr_QUSEC                           like(Qusec)
029600120221     d                                     options(*varsize)
029700120221
029800120221     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
029900120221     d  pr_QLGSCB00                        like(QLGSCB00)
030000120221     d  pr_SflRcd                          like(SflRcd)
030100120221     d  pr_NotUsed1                        like(NotUsed)
030200120221     d  pr_SizeList                        like(SizeList)
030300120221     d  pr_NotUsed2                        like(NotUsed)
030400120221     d  pr_QUSEC                           like(Qusec)
030500120221     d                                     options(*varsize)
030600120221
030700120221     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
030800120221     d  pr_QLGSCB00                        like(QLGSCB00)
030900120221     d  pr_NotUsed1                        like(NotUsed)
031000120221     d  pr_SflRcd                          like(SflRcd)
031100120221     d  pr_SizeList                        like(SizeList)
031200120221     d  pr_NotUsed2                        like(NotUsed)
031300120221     d  pr_QUSEC                           like(Qusec)
031400120221     d                                     options(*varsize)
031500120326
031600120326       // -?Richiamo Scrittura R.A.
031700120326     d fidna3r         pr                  extpgm('FIDNA3R')
031800120326     d  kpjba                              likeds(KPJBA)
031900120326     d  fidna3ds                           likeds(FIDNA3DS)
032000120224
032100120224       // -?Richiamo Controllo Manca Tariffe
032200120224     d tnsb45r         pr                  extpgm('TNSB45R')
032300120224     d  kpjba                              likeds(KPJBA)
032400120229
032500120229       // -?Richiamo Interrogazione bolle di Sede
032600120229     d tnsb51c         pr                  extpgm('TNSB51C')
032700120229     d  kpjba                              likeds(KPJBA)
032800120302     d  partnsb                       1a
032900120224
033000120224       // -?Richiamo Gestione bolle di Sede
033100120224     d tnsb52r         pr                  extpgm('TNSB52R')
033200120224     d  kpjba                              likeds(KPJBA)
033300120224
033400120224       // -?Richiamo Gestione Tariffe
033500120224     d tnta01r         pr                  extpgm('TNTA01R')
033600120224     d  kpjba                              likeds(KPJBA)
033700120314     d  tnsb48ds                           likeds(TNSB48DS)
033800120321
033900120321       // -?Richiamo Interrogazione Tariffe
034000120321     d tnta02r         pr                  extpgm('TNTA02R')
034100120321     d  kpjba                              likeds(KPJBA)
034200120321     d  tnsb48ds                           likeds(TNSB48DS)
034300110506
034400120221       // -?Reperimento filiali gestitbili dall'utente
034500120221     d trul31r         pr                  extpgm('TRUL31R')
034600120221     d  kpjba                              likeds(KPJBA)
034700120221     d  trul31ds                           likeds(TRUL31DS)
034800120223
034900120223       // -?Ricerca cliente
035000120223     d xclir           pr                  extpgm('XCLIR')
035100120223     d  xclirds                            likeds(XCLIRDS)
035200120321     d  par01                         2
035300120321     d  parrag                       48
035400110706
035500110706      //---------------------------------------------------------------
035600110706      //?Definizione prototipi.
035700110706      //---------------------------------------------------------------
035800120130
035900120130       // -?Ricerc/Controllo tabelle
036000120130      /copy gaitrasrc/srcprotopr,tibs02r
036100110706
036200110706       // -?Reperimento dati utente
036300110706      /copy gaitrasrc/srcprotopr,tibs34r
036400120130
036500120130       // -?Reperimento dati anagrafici cliente codificato
036600120222      /copy gaitrasrc/srcprotopi,tibs69r
036700120222      /copy gaitrasrc/srcprotopr,tibs69r
036800120221
036900120221       // -?Controllo abilitazioni utente
037000120221      /copy gaitrasrc/srcprotopr,tntaa1c
037100120221
037200120221       // -?Ricerca commerciale
037300120221      /copy gaitrasrc/srcprotopr,trtb85r
037400090807
037500080206      //---------------------------------------------------------------
037600080206      //?Definizione key-list.
037700080206      //---------------------------------------------------------------
037800090807
037900120130       // -?File TABEL00F
038000120130     d k03tabel      e ds                  extname(TABEL00F:*key)
038100120130     d                                     prefix(k_)
038200080206
038300080206      //---------------------------------------------------------------
038400080206      //?Riepilogo indicatori.
038500080206      //---------------------------------------------------------------
038600090807
038700090807
038800080206      //---------------------------------------------------------------
038900080206
039000080206      //---------------------------------------------------------------
039100080206      //?M A I N - L I N E
039200080206      //---------------------------------------------------------------
039300080206
039400080206     c     *Entry        plist
039500080206     c                   parm                    KPJBA
039600080206
039700080206      /free
039800080206
039900090807       //?Operazioni iniziali
040000080206       exsr RoutInz;
040100080206
040200090807       //?Gestione video
040300120326       DOW  wFine = *off;
040400090807
040500080206         select;
040600120130           when wVideo = 'D1';
040700110706             exsr GesD01;
040800120130           when wVideo = 'S2';
040900120130             exsr GesS02;
041000090428           other;
041100120130             wFine = *on;
041200080206         endsl;
041300090807
041400080206       ENDDO;
041500080206
041600090807       //?Operazioni finali
041700080206       exsr RoutEnd;
041800080206
041900080206       //--------------------------------------------------------------
042000080206       //?Operazioni iniziali.
042100080206       //--------------------------------------------------------------
042200080206       BEGSR RoutInz;
042300120309
042400120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
042500080206
042600120130         wVideo = 'D1';
042700120130         wInzD01 = *on;
042800090807
042900110706       //?Reperimento dati job
043000090807         exsr DatiJob;
043100120221
043200120221       //?Controllo se utente autorizzato alla funzione
043300120221         reset TNTAA1DS;
043400120221         ITAA1tipo = 'N';
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
046400120309       //?Se utente non EDP controllo la filiale, per ora abilitiamo solo 005 e 028
046500120309         IF  %subst(knmus:1:3) <> 'EDP' and
046600120309             DUTpou <> 005 and DUTpou <> 028;
046700120309           wErrGrave   = *on;
046800120309           ErrMessage  = *on;
046900120309           ErrGenerico = *on;
047000120309           V01msg = skMsg(01);
047100120309           leavesr;
047200120309         ENDIF;
047300090806
047400110706       //?Impostazione campi "fissi"
047500110706         V1Tpgm = SDSpgm;
047600120130         k_TBLkut = 1;
047700120221
047800120221       //?Data del giorno
047900120221         wOggi = %dec(%date());
048000120223
048100120223       //?Cerco la tabella gg. validità password
048200120223         clear TIBS02DS;
048300120223         clear dMTC;
048400120223         T02mod = 'C';
048500120223         T02cod = 'MTC';
048600120223         T02sif = knsif;
048700120223         T02ke1 = 'PSW';
048800120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
048900120223         IF  T02err <> *blanks;
049000120223           wErrGrave   = *on;
049100120223           ErrMessage  = *on;
049200120223           ErrGenerico = *on;
049300120223           PosCurPWD   = *on;
049400120223           V01msg      = skMsg(06);
049500120223           leavesr;
049600120223         ENDIF;
049700120223         wGiorni = %dec(T02uni:3:0);
049800120222
049900120222       //?Cerco la password memorizzata per filiale (utente)
050000120222         clear TIBS02DS;
050100120222         clear dMTC;
050200120222         T02mod = 'C';
050300120222         T02cod = 'MTC';
050400120222         T02sif = knsif;
050500120223         T02ke1 = %editc(DUTpou:'X');
050600120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
050700120222         IF  T02err <> *blanks;
050800120222           wErrGrave   = *on;
050900120222           ErrMessage  = *on;
051000120222           ErrGenerico = *on;
051100120222           V01msg = skMsg(02);
051200120222           leavesr;
051300120222         ENDIF;
051400120222         dMTC = T02uni;
051500120223
051600120223         clear sav_Pwd;
051700090714
051800080206       ENDSR;
051900080206
052000080206       //--------------------------------------------------------------
052100080206       //?Reperimento Dati del job (Utente/Operativi).
052200080206       //--------------------------------------------------------------
052300080206       BEGSR DatiJob;
052400080206
052500080206         in(E) §AzUte;
052600110706         IF NOT %error;
052700080206           in(E) §DatiUte;
052800110706         ENDIF;
052900110706         IF %error or RSut = *blanks;
053000080206           clear TIBS34ds;
053100080206           tibs34r(tibs34ds);
053200080206           in §AzUte;
053300080206           in §DatiUte;
053400110706         ENDIF;
053500080206
053600080206       ENDSR;
053700080206
053800090421       //--------------------------------------------------------------
053900110706       //?Gestione videata D01
054000080206       //--------------------------------------------------------------
054100110706       BEGSR GesD01;
054200080207
054300110706       //?Inizializzazione videata
054400120130         IF  wInzD01 = *on;
054500110706            exsr InzD01;
054600120130            wInzD01  = *off;
054700110706         ENDIF;
054800080207
054900110706       //?Emissione Testata
055000120221         write  SB48T01;
055100080207
055200110706       //?Emissione videata
055300120221         exfmt  SB48D01;
055400110706         reset ErrMessage;
055500110706         reset ErrGenerico;
055600120127         clear V01msg;
055700080207
055800080207         SELECT;
055900120227
056000120227         //?- Errore grave esco dal pgm
056100120227           WHEN  wErrGrave;
056200120227             wFine = *on;
056300110706
056400110706         //?- F03=Fine
056500110706           WHEN  dsp_aid = c_F03;
056600110706             exsr F03D01;
056700080207
056800090807         //?Invio
056900080207           OTHER;
057000110706         //?Eseguo i controlli
057100110706             exsr CtrD01;
057200110706             IF  ErrGenerico;
057300080207               leavesr;
057400110706             ENDIF;
057500120127         //?Se tutto OK vado nella seconda videata
057600120130           wVideo = 'S2';
057700120130           wInzS02 = *on;
057800080207
057900080207         ENDSL;
058000080207
058100080207       ENDSR;
058200080207
058300080207       //--------------------------------------------------------------
058400110706       //?Inizializzazione videata D01.
058500080207       //--------------------------------------------------------------
058600110706       BEGSR InzD01;
058700080207
058800120221         clear V01fil;
058900120302         V01dfil = '(0 = Tutte)';
059000120221         clear V01ccm;
059100120221         clear V01dccm;
059200120221         clear V01ksc;
059300120221         clear V01rag;
059400120223
059500120223         V01pwd = sav_Pwd;
059600090508
059700080207       ENDSR;
059800080207
059900091111       //--------------------------------------------------------------
060000110706       //?Gestione tasto funzionale F03 da videata D01.
060100110706       //?F3=Fine
060200091111       //--------------------------------------------------------------
060300110706       BEGSR F03D01;
060400091111
060500110706       //?Chiusura del programma
060600120130         wFine = *on;
060700091111
060800091111       ENDSR;
060900120130
061000120130       //--------------------------------------------------------------
061100120130       //?Controllo dati videata D01.
061200120130       //--------------------------------------------------------------
061300120130       BEGSR CtrD01;
061400120130
061500120130         WindDspF  = IndDspF;
061600120130         reset WindDspFb;
061700120130         IndDspF   = WindDspF;
061800120130
061900120221       //?Controllo la filiale
062000120221         IF  V01fil > 0;
062100120221           chain V01fil AZORG01L;
062200120223           IF not %found(AZORG01L);
062300120221             ErrMessage  = *on;
062400120221             ErrGenerico = *on;
062500120221             PosCurFIL   = *on;
062600120222             V01msg      = skMsg(03);
062700120221             leavesr;
062800120221           ENDIF;
062900120221         //?Verifico se l'utente è abilitato alla filiale
063000120221           w003a = %editc(V01fil:'X');
063100120221           IF  %lookup(w003a:pog) = 0;
063200120221             ErrMessage  = *on;
063300120221             ErrGenerico = *on;
063400120221             PosCurFIL   = *on;
063500120222             V01msg      = skMsg(03);
063600120221             leavesr;
063700120221           ENDIF;
063800120130         ENDIF;
063900120221
064000120221       //?Ricerca del commerciale
064100120221         IF  %scan('?' : V01ccm) > 0;
064200120221           ErrGenerico = *on;
064300120222           PosCurCCM   = *on;
064400120221           TB85kfil    = %editc(DUTpou:'X');
064500120221           clear TB85key;
064600120221           clear TB85des1;
064700120221           trtb85r (kpjba : TB85kfil : TB85key : TB85des1);
064800120221           V01ccm  = TB85key;
064900120221           V01dccm = TB85des1;
065000120221           leavesr;
065100120221         ENDIF;
065200120221
065300120221       //?Controllo il commerciale
065400120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
065500120221
065600120221         //?Solo valori numerici
065700120223           IF  %check(c_digits:V01ccm) > 0;
065800120221             ErrMessage  = *on;
065900120221             ErrGenerico = *on;
066000120222             PosCurCCM   = *on;
066100120222             V01msg      = skMsg(04);
066200120221             leavesr;
066300120221           ENDIF;
066400120221
066500120221         //?Controllo se utente abilitato al commerciale
066600120221           clear TNTAA1DS;
066700120221           ITAA1tipo = 'N';
066800120221           ITAA1caut = '§utegtc';
066900120221           ITAA1cmm  = %dec(V01ccm:7:0);
067000120221           kpjbu     = tntaa1ds;
067100120221           tntaa1c (kpjba);
067200120221           TNTAA1DS = kpjbu;
067300120221           IF  OTAA1fabi = 'N';
067400120221             ErrMessage  = *on;
067500120221             ErrGenerico = *on;
067600120222             PosCurCCM   = *on;
067700120222             V01msg      = skMsg(04);
067800120221             leavesr;
067900120221           ENDIF;
068000120221
068100120221         //?Controllo se commerciale valido
068200120221           clear  ds01;
068300120223           k_TBLcod = '01';
068400120223           k_TBLkey = V01ccm;
068500120223           chain  %kds(K03tabel) TABEL00F;
068600120221           IF  not %found(TABEL00F) or TBLflg <> *blanks;
068700120221             ErrMessage  = *on;
068800120221             ErrGenerico = *on;
068900120222             PosCurCCM   = *on;
069000120222             V01msg      = skMsg(04);
069100120221             leavesr;
069200120221           ENDIF;
069300120221
069400120221           ds01    = TBLuni;
069500120221           V01dccm = §01age;
069600120221
069700120221         //?Commerciale senza particolarità
069800120221           IF  §01par <> *blanks;
069900120221             ErrMessage  = *on;
070000120221             ErrGenerico = *on;
070100120222             PosCurCCM   = *on;
070200120222             V01msg      = skMsg(04);
070300120221             leavesr;
070400120221           ENDIF;
070500120221
070600120221         //?Commerciale "valido" data inizio e fine attività
070700120221           IF  §01dtdec > %editc(wOggi:'X') or
070800120221               §01dtfin < %editc(wOggi:'X');
070900120221             ErrMessage = *on;
071000120221             ErrGenerico = *on;
071100120222             PosCurCCM   = *on;
071200120222             V01msg      = skMsg(04);
071300120221             leavesr;
071400120221           ENDIF;
071500120221         ENDIF;
071600120223
071700120223       //?Ricerca del cliente
071800120223         IF  %scan('?' : V01ksc) > 0;
071900120223           ErrGenerico = *on;
072000120223           PosCurKSC   = *on;
072100120223           clear xCLIrds;
072200120321           clear par01;
072300120321           clear parrag;
072400120223           DXCaut = wUteAbi;
072500120223           DXCcap = DUTkci;
072600120321           xclir (xCLIrds:par01:parrag);
072700120223           ksa = DXCksc;
072800120223           dsksa = ksa(1);
072900120223           ksc(1) = dsksn;
073000120223           V01ksc = %editc(ksc(1):'X');
073100120223         ENDIF;
073200120223
073300120223       //?Controllo il cleinte
073400120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
073500120223
073600120223         //?Solo valori numerici
073700120223           IF  %check(c_digits:V01ksc) > 0;
073800120223             ErrMessage  = *on;
073900120223             ErrGenerico = *on;
074000120223             PosCurKSC   = *on;
074100120223             V01msg      = skMsg(09);
074200120223             leavesr;
074300120223           ENDIF;
074400120223
074500120223         //?Controllo se utente abilitato al cliente
074600120223           clear TNTAA1DS;
074700120223           ITAA1tipo = 'N';
074800120223           ITAA1caut = '§utegtc';
074900120223           ITAA1ksc  = %dec(V01ksc:7:0);
075000120223           kpjbu     = tntaa1ds;
075100120223           tntaa1c (kpjba);
075200120223           TNTAA1DS = kpjbu;
075300120223           IF  OTAA1fabi = 'N';
075400120223             ErrMessage  = *on;
075500120223             ErrGenerico = *on;
075600120223             PosCurKSC   = *on;
075700120223             V01msg      = skMsg(09);
075800120223             leavesr;
075900120223           ENDIF;
076000120223
076100120223         //?Controllo se cliente valido
076200120223           clear tibs69ds;
076300120223           I69kac = %dec(V01ksc:7:0);
076400120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
076500120223           IF  O69err <> *blanks;
076600120223             ErrMessage  = *on;
076700120223             ErrGenerico = *on;
076800120223             PosCurKSC   = *on;
076900120223             V01msg      = skMsg(09);
077000120223             leavesr;
077100120223           ENDIF;
077200120223           V01rag = ACOrag;
077300120223         ENDIF;
077400120221
077500120222       //?Password obbligatoria
077600120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
077700120222           ErrMessage = *on;
077800120222           ErrGenerico = *on;
077900120222           PosCurPWD   = *on;
078000120222           V01msg      = skMsg(05);
078100120222           leavesr;
078200120222         ENDIF;
078300120222
078400120222       //?Controllo se password OK
078500120222         IF  V01pwd <> §MTCpwd;
078600120222           ErrMessage = *on;
078700120222           ErrGenerico = *on;
078800120222           PosCurPWD   = *on;
078900120222           V01msg      = skMsg(05);
079000120222           clear V01pwd;
079100120222           leavesr;
079200120222         ENDIF;
079300120222
079400120222       //?Data immissione/modifica password
079500120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
079600120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
079700120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
079800120223         wDataISO = %date(wData);
079900120223
080000120223       //?Calcolo la data scadenza password
080100120223         wDataISO += %days(wgiorni);
080200120223         wDataScad = %dec(wDataISO);
080300120223
080400120223       //?Controllo se la password è scaduta
080500120222         IF wOggi > wDataScad;
080600120222           ErrMessage  = *on;
080700120222           ErrGenerico = *on;
080800120222           PosCurPWD   = *on;
080900120222           V01msg      = skMsg(07);
081000120222           leavesr;
081100120222         ENDIF;
081200120223
081300120223         sav_Pwd = V01pwd;
081400120130
081500120130       ENDSR;
081600120130
081700120130       //--------------------------------------------------------------
081800120130       //?Gestione videata S02
081900120130       //--------------------------------------------------------------
082000120130       BEGSR GesS02;
082100120306
082200120306       //?Se non l'ho ancora fatto tasso tutto
082300120306       //?(capita solo appena entro nel pgm)
082400120306         IF  not wTassaPrima;
082500120309           exsr F05S02;
082600120306           wTassaPrima = *on;
082700120306         ENDIF;
082800120130
082900120130       //?Inizializzazione videata
083000120130         IF  wInzS02 = *on;
083100120130            exsr InzS02;
083200120130            wInzS02  = *off;
083300120130         ENDIF;
083400120130
083500120130       //?Emissione Testata + Piede
083600120221         write  SB48T01;
083700120221         write  SB48P02;
083800120130
083900120130       //?Emissione subfile
084000120221         exfmt  SB48C02;
084100120130         reset ErrMessage;
084200120130         reset ErrGenerico;
084300120130         clear V02msg;
084400120130
084500120130         SELECT;
084600120130
084700120130         //?- F03=Fine
084800120130           WHEN  dsp_aid = c_F03;
084900120130             exsr F03D01;
085000120130
085100120222         //?- F05=Ritassa
085200120222           WHEN  dsp_aid = c_F05;
085300120222             exsr F05S02;
085400120222
085500120228         //?- F08=Ordina
085600120222           WHEN  dsp_aid = c_F08;
085700120229           //?Se era ordina per cliente diventa ordina per commerciale
085800120229             IF  Ord_xksc;
085900120229               Ord_xksc = *off;
086000120229               Ord_Xccm = *on;
086100120229               wF08 = 'F8=Ord.x Cliente';
086200120229           //?Se era ordina per cliente diventa ordina per commerciale
086300120229             ELSE;
086400120229               Ord_xksc = *on;
086500120229               Ord_Xccm = *off;
086600120229               wF08 = 'F8=Ord.x Commerciale';
086700120229             ENDIF;
086800120222             exsr F08S02;
086900120130
087000120130         //?- F12=Ritorno
087100120130           WHEN  dsp_aid = c_F12;
087200120130             exsr F12S02;
087300120130
087400120130         //?Invio
087500120130           OTHER;
087600120222         //?Opzioni
087700120223             IF  S02nrr > *zeros;
087800120223               exsr OpzS02 ;
087900120223             ENDIF;
088000120130             IF  ErrGenerico;
088100120130               leavesr;
088200120130             ENDIF;
088300120130
088400120130         ENDSL;
088500120130
088600120130       ENDSR;
088700120130
088800120130       //--------------------------------------------------------------
088900120130       //?Inizializzazione videata S02.
089000120130       //--------------------------------------------------------------
089100120130       BEGSR InzS02;
089200120130
089300120130       //?Pulizia subfile
089400120130         exsr PulS02;
089500120222
089600120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
089700120222         exsr PrepSql;
089800120130
089900120130       //?Caricamento dati
090000120130         exsr CarS02;
090100120130
090200120130       //?Visualizzazione del Subfile
090300120130         SflDsp = (S02nrr <= *zeros);
090400120130
090500120130       //?Attivazione SFLEND
090600120130         SflEnd = *on;
090700120130
090800120130       //?Impaginazione Subfile
090900120130       //?Forzo sempre il primo rcd
091000120130         IF  S02nrr > *zero;
091100120130           C02rcd = 1;
091200120130         ELSE;
091300120130           clear C02rcd;
091400120130         ENDIF;
091500120130
091600120130         C02csr = C02rcd;
091700120130
091800120130       ENDSR;
091900120130
092000120130       //--------------------------------------------------------------
092100120130       //?Pulizia Subfile S02.
092200120130       //--------------------------------------------------------------
092300120130       BEGSR PulS02;
092400120130
092500120130       //?Pulizia subfile
092600120130         SflDsp    = *on;
092700120130         SflDspCtl = *on;
092800120221         write  SB48C02;
092900120130         SflDspCtl = *off;
093000120130         SflEnd    = *off;
093100120130
093200120130         clear C02rcd;
093300120130         clear C02csr;
093400120130         clear S02nrr;
093500120130         clear V02msg;
093600120130         ErrMessage  = *off;
093700120130         ErrGenerico = *off;
093800120130
093900120130         WindDspF = IndDspF;
094000120130         reset WindDspFb;
094100120130         IndDspF  = WindDspF;
094200120130
094300120130       ENDSR;
094400120222
094500120222       //--------------------------------------------------------------
094600120222       //?Preparazione stringa SQL.
094700120222       //--------------------------------------------------------------
094800120222       BEGSR PrepSql;
094900120307
095000120307         clear w0070;
095100120222
095200120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
095300120222
095400120307       //?Se nessuna richiesta a video
095500120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
095600120307            (V01ccm = *blanks or V01ccm = *zeros);
095700120222       //?Carico le bolle con filiale cliente = sk POG
095800120222           wSQL += ' and MTCpoc in(';
095900120222           yy = 0;
096000120222           xx = 1;
096100120222           FOR xx by 1 to %elem(pog);
096200120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
096300120222               IF yy > 0;
096400120222                 wSQL += ', ';
096500120222               ELSE;
096600120222               yy = 1;
096700120222               ENDIF;
096800120222               wSQL += '''';
096900120222               wSQL += pog(xx);
097000120222               wSQL += '''';
097100120222             ENDIF;
097200120222           ENDFOR;
097300120222           wSQL += ')';
097400120222         ELSE;
097500120307         //?Se richiesta filiale a video
097600120307           IF  V01fil > 0;
097700120307         //?Carico le bolle con filiale cliente = filiale richiesta
097800120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
097900120307           ENDIF;
098000120307         //?Se richiesto commerciale a video
098100120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
098200120307              w0070 = %dec(V01ccm:7:0);
098300120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
098400120307           ENDIF;
098500120307         //?Se richiesto cliente a video
098600120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
098700120307              w0070 = %dec(V01ksc:7:0);
098800120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
098900120307           ENDIF;
099000120222         ENDIF;
099100120309
099200120309         wSQL += ' for FETCH ONLY';
099300120222
099400120222       ENDSR;
099500120130
099600120130       //--------------------------------------------------------------
099700120130       //?Caricamento Subfile S02.
099800120130       //--------------------------------------------------------------
099900120130       BEGSR CarS02;
100000120312
100100120312         clear sav_s02nrr;
100200120130
100300120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
100400120222         exec sql prepare A1 from :wSQL;
100500120222         exec sql declare MTC cursor for A1;
100600120222         exec sql open MTC;
100700120222
100800120222         IF  sqlcode < 0;
100900120222           wEoF = *on;
101000120222         ENDIF;
101100120222
101200120222         DOW  not wEoF;
101300120309           exec sql fetch next from MTC into :TFMTCds;
101400120222           IF  sqlcod = 100 or sqlcod < 0;
101500120222             wEoF = *on;
101600120222             leave;
101700120222           ENDIF;
101800120222
101900120222         //?Carico il subfile
102000120222           exsr RieS02;
102100120222         //?Scrico il subfile
102200120222           S02nrr += 1;
102300120222           write SB48S02;
102400120306           sav_S02nrr = S02nrr;
102500120229           IF  S02nrr = 9998;
102600120229             leave;
102700120229           ENDIF;
102800120222
102900120222         ENDDO;
103000120222
103100120222         exec sql close MTC;
103200120130         wEoF = *off;
103300120222
103400120222         //?Dopo aver caricato il subfile lo ordino per cliente
103500120229         wF08 = 'F8=Ord.x Commerciale';
103600120228         Ord_xksc = *on;
103700120228         Ord_Xccm = *off;
103800120228         exsr F08S02;
103900120130
104000120130       ENDSR;
104100120130
104200120130       //--------------------------------------------------------------
104300120130       //?Carico i dati nel subfile S02.
104400120130       //--------------------------------------------------------------
104500120130       BEGSR RieS02;
104600120130
104700120221         clear  SB48S02;
104800120130
104900120228         S02aas = MTCaas;
105000120228         S02nrs = MTCnrs;
105100120228         S02nsp = MTCnsp;
105200120228         S02tbl = MTCtbl;
105300120222
105400120222         S02ksc = MTCksc;
105500120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
105600120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
105700120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
105800120222         S02ctr = MTCctr;
105900120222         S02ccm = MTCage;
106000120222         S02err = MTCerr;
106100120223         S02lnp = MTClnp;
106200120223         S02lna = MTClna;
106300120223         S02tsp = MTCtsp;
106400120228         S02pro = MTCpro;
106500120228         S02cts = MTCcts;
106600120305         S02ncl = MTCncl;
106700120305         S02pkf = MTCpkf;
106800120305         S02vlf = MTCvlf;
106900120326
107000120326         S02imp = MTCimp;
107100120326         S02imv = MTCimv;
107200120130
107300120222       //?Decodifico il cliente
107400120222         clear tibs69ds;
107500120222         I69kac = S02ksc;
107600120222         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
107700120222         IF  O69err = *blanks;
107800120222           S02rag = ACOrag;
107900120222         ENDIF;
108000120222
108100120222       //?Decodifico il commerciale
108200120222         clear ds01;
108300120223         k_TBLcod = '01';
108400120223         k_TBLkey = %editc(S02ccm:'X');
108500120223         chain  %kds(K03tabel) TABEL00F;
108600120222         IF  %found(TABEL00F) and TBLflg = *blanks;
108700120222           ds01 = TBLuni;
108800120222         ENDIF;
108900120223         S02dccm = §01age;
109000120130
109100120130       ENDSR;
109200120130
109300120130       //--------------------------------------------------------------
109400120222       //?Gestione delle opzioni immesse nella videata S02.
109500120130       //--------------------------------------------------------------
109600120222       BEGSR OpzS02;
109700120130
109800120223         readc SB48S02;
109900120223
110000120223         DOW  not %eof(TNSB48D);
110100120223
110200120223           SflNxtChg = *off;
110300120223
110400120223           WindDspF = IndDspF;
110500120223           reset WindDspFb;
110600120223           IndDspF  = WindDspF;
110700120223
110800120223           C02rcd = S02nrr;
110900120223
111000120223           SELECT;
111100120223
111200120223         //?Nessuna opzione immessa
111300120223             WHEN  S02opz  = *blanks;
111400120223
111500120223         //?Precedente segnalazione di errore
111600120223             WHEN  S02opz  = 'E';
111700120223               clear S02opz;
111800120223
111900120223         //?B = Manutenzione bolla di sede
112000120223             WHEN  S02opz  = 'B';
112100120223               exsr Call_TNSB52;
112200120223
112300120223         //?T = Manutenzione tariffa
112400120223             WHEN  S02opz  = 'T';
112500120223               exsr Call_TNTA01;
112600120326
112700120326         //?V = OK utente
112800120326             WHEN  S02opz  = 'V' and S02imp = 'IMP';
112900120326               exsr Call_visto;
113000120321
113100120321         //?3 = Interrogazione tariffa
113200120321             WHEN  S02opz  = '3';
113300120321               exsr Call_TNTA02;
113400120321
113500120321         //?4 = Interrogazione clienti
113600120321             WHEN  S02opz  = '4';
113700120321               exsr Call_XCLIR;
113800120223
113900120223         //?5 = Visualizza bolla di sede
114000120223             WHEN  S02opz  = '5';
114100120223               exsr Call_TNSB51;
114200120223
114300120223         //?Opzione non valida
114400120223             OTHER;
114500120223               ErrMessage  = *on;
114600120223               ErrGenerico = *on;
114700120223               PosCurOPZ   = *on;
114800120223               V02msg = skMsg(08);
114900120223
115000120223           ENDSL;
115100120223
115200120223         //?Aggiorno il subfile
115300120223           SELECT;
115400120223             WHEN  ErrMessage;
115500120223               SflNxtChg = *on;
115600120223               C02csr    = C02rcd;
115700120223             WHEN  ErrGenerico;
115800120223               C02csr = C02rcd;
115900120223               clear S02opz;
116000120223             OTHER;
116100120223               C02csr = C02rcd;
116200120223               clear S02opz;
116300120223           ENDSL;
116400120223
116500120223           update SB48S02;
116600120223
116700120223           IF  ErrMessage or ErrGenerico;
116800120223             leave;
116900120223           ENDIF;
117000120223
117100120223           readc SB48S02;
117200120223
117300120223         ENDDO;
117400120130
117500120223       ENDSR;
117600120222
117700120222       //--------------------------------------------------------------
117800120222       //?Gestione tasto funzionale F05 da videata S02.
117900120222       //?F05=Ritassa
118000120222       //--------------------------------------------------------------
118100120222       BEGSR F05S02;
118200120224
118300120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
118400120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
118500120224            (V01ccm = *blanks or V01ccm = *zeros);
118600120224           xx = 1;
118700120224           FOR xx by 1 to %elem(pog);
118800120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
118900120224               clear TNSB44DS;
119000120224               D44fil = %dec(pog(xx):3:0);
119100120224               D44sb48 = 'S';
119200120224               kpjbu = TNSB44DS;
119300120224               tnsb45r (kpjba);
119400120224             ENDIF;
119500120224           ENDFOR;
119600120224
119700120224         ELSE;
119800120224
119900120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
120000120224           clear TNSB44DS;
120100120224           D44fil = V01fil;
120200120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
120300120224             D44ccm = %dec(V01ccm:7:0);
120400120224           ENDIF;
120500120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
120600120224             D44cli = %dec(V01ksc:7:0);
120700120224           ENDIF;
120800120224           D44sb48 = 'S';
120900120224           kpjbu = TNSB44DS;
121000120224           tnsb45r (kpjba);
121100120224
121200120224         ENDIF;
121300120224
121400120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
121500120224         wVideo = 'S2';
121600120224         wInzS02 = *on;
121700120222
121800120223       ENDSR;
121900120222
122000120222       //--------------------------------------------------------------
122100120222       //?Gestione tasto funzionale F08 da videata S02.
122200120228       //?F08=Ordinamento
122300120222       //--------------------------------------------------------------
122400120222       BEGSR F08S02;
122500120222
122600120306         rrnlast = sav_S02nrr;
122700120222
122800120222       //?Inizializza i campi chiave x l'ordinamento.
122900120222         clear QLGSCB;
123000120222         clear QLGSCB00;
123100120222
123200120228       //?1 campo chiave
123300120222         QLGNBRK = 1;
123400120222
123500120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
123600120228         IF  Ord_xksc;
123700120228           QLGSP = 1;
123800120228           QLGSS = %SIZE(S02ksc);
123900120228         ENDIF;
124000120228         IF  Ord_xccm;
124100120228           QLGSP = 1 + %size(S02ksc);
124200120228           QLGSS = %SIZE(S02ccm);
124300120228         ENDIF;
124400120222         QLGDT = Numerico  ;
124500120222         QLGSO = Ascendente;
124600120222         QLGKL(1) = QLGSKL;
124700120222
124800120222       //?Load other sort parameters.
124900120222         QLGLB = 80 + 16 * MaxKey;
125000120222         QLGRL = %SIZE(sflrcd) - 1;
125100120222         QLGRT = 8;
125200120222         QLGOKL = 80;
125300120222         QLGLKE = 16;
125400120222         QLGLSS = 290;
125500120222
125600120222       //?Initialize Sort I/O API fields.
125700120222         QLGRL00 = QLGRL;
125800120222         QLGRC00 = 1;
125900120222         clear QUSEI;
126000120222         QUSBPRV = %SIZE(QUSEC);
126100120222
126200120222       //?First step - Initialize the sort routine.
126300120222         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
126400120222
126500120222       //?Next step - Write records to I/O routine.
126600120222         QLGRT00 = Put;
126700120222         FOR xx = 1 to rrnlast;
126800120222           chain xx SB48S02;
126900120222
127000120222       //?solo le righe con Selected = 'Y' sono riordinate,
127100120222       //?quindi per fare un ordinamento di tutte le righe
127200120222       //?metto 'Y' sempre.
127300120222           selected  = 'Y';
127400120222           clear QUSEI;
127500120222           QUSBPRV = %SIZE(QUSEC);
127600120222           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
127700120222         ENDFOR;
127800120222
127900120222       //?Next step - Signal end of input, clear subfile for reload.
128000120222         QLGRT00 = EndPut;
128100120222         clear QUSEI;
128200120222         QUSBPRV = %SIZE(QUSEC);
128300120222         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
128400120222
128500120222       //?Pulizia SFL
128600120222         exsr PulS02;
128700120222
128800120222       //?Final step - Write the records back to the subfile.
128900120222         QLGRT00 = Get;
129000120222         FOR xx = 1 to rrnlast;
129100120222           clear QUSEI;
129200120222           QUSBPRV = %SIZE(QUSEC);
129300120222           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
129400120222           S02nrr = xx;
129500120222           write SB48S02;
129600120222         ENDFOR;
129700120222
129800120222       //?Visualizzazione del SFL (se ci sono dati)
129900120223         SflDsp = (S02nrr <= *zeros);
130000120222
130100120222       //?Attivazione del SFLEND
130200120222         SflEnd = *on;
130300120222
130400120222       //?Posizionamento cursore al 1° rcd della pagina
130500120222         IF  S02nrr > *zero;
130600120222           C02rcd = 1;
130700120222         ELSE;
130800120222           clear C02rcd;
130900120222         ENDIF;
131000120222
131100120222         C02csr = C02rcd;
131200120222
131300120222       ENDSR;
131400120222
131500120222       //--------------------------------------------------------------
131600120222       //?Gestione tasto funzionale F12 da videata S02.
131700120222       //?F12=Ritorno
131800120222       //--------------------------------------------------------------
131900120222       BEGSR F12S02;
132000120222
132100120222         wVideo = 'D1';
132200120222         wInzD01 = *on;
132300120222
132400120222       ENDSR;
132500120326
132600120326       //--------------------------------------------------------------
132700120326       //?Richiamo pgm Interrogazione bolle di sede.
132800120326       //--------------------------------------------------------------
132900120326       BEGSR Call_TNSB51;
133000120326
133100120326         clear TNSB51DS;
133200120326         I51op0 = 'B00';
133300120326         I51aas = S02aas;
133400120326         I51lnp = S02lnp;
133500120326         I51nrs = S02nrs;
133600120326         I51nsp = S02nsp;
133700120326         I51tbl = S02tbl;
133800120326         kpjbu = TNSB51DS;
133900120326         ParTnsb = '5';
134000120326         tnsb51c (kpjba:ParTnsb);
134100120326         TNSB51DS = kpjbu;
134200120326         IF  O51err = '1';
134300120326           ErrMessage  = *on;
134400120326           ErrGenerico = *on;
134500120326           PosCurOPZ   = *on;
134600120326           V02msg = O51msg;
134700120326         ENDIF;
134800120326
134900120326       ENDSR;
135000090807
135100120223       //--------------------------------------------------------------
135200120223       //?Richiamo pgm Gestione bolle di sede.
135300120223       //--------------------------------------------------------------
135400120223       BEGSR Call_TNSB52;
135500120223
135600120224         clear TNSB01DS;
135700120327         D01op0 = 'T02';
135800120228         D01aas = S02aas;
135900120224         D01lnp = S02lnp;
136000120228         D01nrs = S02nrs;
136100120228         D01nsp = S02nsp;
136200120228         D01tbl = S02tbl;
136300120224         kpjbu = TNSB01DS;
136400120302         ParTnsb = '2';
136500120302         tnsb51c (kpjba:ParTnsb);
136600120301         TNSB01DS = kpjbu;
136700120301         IF  D01err = '1';
136800120301           ErrMessage  = *on;
136900120301           ErrGenerico = *on;
137000120301           PosCurOPZ   = *on;
137100120301           V02msg = D01msg;
137200120301         ENDIF;
137300120223
137400120223       ENDSR;
137500120223
137600120223       //--------------------------------------------------------------
137700120223       //?Richiamo pgm Gestione Tariffa.
137800120223       //--------------------------------------------------------------
137900120223       BEGSR Call_TNTA01;
138000120223
138100120314         clear TNSB48DS;
138200120314         ISB48ksc  = S02ksc;
138300120314         ISB48rag  = S02rag;
138400120314         ISB48dsp  = S02dsp;
138500120314         ISB48tbl  = S02tbl;
138600120314         ISB48lnp  = S02lnp;
138700120314         ISB48lna  = S02lna;
138800120314         ISB48pro  = S02pro;
138900120314         ISB48cts  = S02cts;
139000120314         ISB48ctr  = S02ctr;
139100120314         ISB48ncl  = S02ncl;
139200120314         ISB48pkf  = S02pkf;
139300120314         ISB48vlf  = S02vlf;
139400120314         ISB48err  = S02err;
139500120314         tnta01r (kpjba:TNSB48DS);
139600120314         IF  OSB48err <> *blanks;
139700120228           ErrMessage  = *on;
139800120228           ErrGenerico = *on;
139900120228           PosCurOPZ   = *on;
140000120228           V02msg = skMsg(08);
140100120228         ENDIF;
140200120223
140300120223       ENDSR;
140400120321
140500120321       //--------------------------------------------------------------
140600120321       //?Richiamo pgm Interrogazione Tariffa.
140700120321       //--------------------------------------------------------------
140800120321       BEGSR Call_TNTA02;
140900120321
141000120321         clear TNSB48DS;
141100120321         ISB48ksc  = S02ksc;
141200120321         ISB48rag  = S02rag;
141300120321         ISB48dsp  = S02dsp;
141400120321         ISB48tbl  = S02tbl;
141500120321         ISB48lnp  = S02lnp;
141600120321         ISB48lna  = S02lna;
141700120321         ISB48pro  = S02pro;
141800120321         ISB48cts  = S02cts;
141900120321         ISB48ctr  = S02ctr;
142000120321         ISB48ncl  = S02ncl;
142100120321         ISB48pkf  = S02pkf;
142200120321         ISB48vlf  = S02vlf;
142300120321         ISB48err  = S02err;
142400120321         tnta02r (kpjba:TNSB48DS);
142500120321         IF  OSB48err <> *blanks;
142600120321           ErrMessage  = *on;
142700120321           ErrGenerico = *on;
142800120321           PosCurOPZ   = *on;
142900120321           V02msg = skMsg(08);
143000120321         ENDIF;
143100120321
143200120321       ENDSR;
143300120326
143400120326       //--------------------------------------------------------------
143500120326       //?Visto: OK dell'utente.
143600120326       //--------------------------------------------------------------
143700120326       BEGSR Call_visto;
143800120326
143900120326         wFineW01 = *off;
144000120326         W01aas = S02aas;
144100120326         W01lnp = S02lnp;
144200120326         W01nrs = S02nrs;
144300120326         W01nsp = S02nsp;
144400120326         W01tbl = S02tbl;
144500120326         W01ksc = S02ksc;
144600120326         W01rag = S02rag;
144700120326         W01imv = S02imv;
144800120326         W01err = S02err;
144900120326
145000120326         DOW  not wFineW01;
145100120326           exfmt  SB48W01;
145200120326           reset ErrMessage;
145300120326           reset ErrGenerico;
145400120326           clear W01msg;
145500120326         //?- F12=Ritorno
145600120326           IF  dsp_aid = c_F12;
145700120326             wFineW01 = *on;
145800120326           ENDIF;
145900120326         //?- F06=Conferma
146000120326           IF  dsp_aid = c_F06;
146100120326             exsr  F06W01;
146200120326             IF  not ErrGenerico;
146300120326               wFineW01 = *on;
146400120326             ENDIF;
146500120326           ENDIF;
146600120326         ENDDO;
146700120326
146800120326       ENDSR;
146900120321
147000120321       //--------------------------------------------------------------
147100120321       //?Richiamo pgm Interrogazione Clienti.
147200120321       //--------------------------------------------------------------
147300120321       BEGSR Call_XCLIR;
147400120321
147500120321         clear xCLIrds;
147600120321         clear par01;
147700120321         parrag = S02rag;
147800120321         DXCop0 = 'OPZ';
147900120321         DXCaut = wUteAbi;
148000120321         DXCcap = DUTkci;
148100120321         xclir (xCLIrds:par01:parrag);
148200120321
148300120321       ENDSR;
148400120326
148500120326       //--------------------------------------------------------------
148600120326       //?Gestione tasto funzionale F06 da videata W01.
148700120326       //?F6=Conferma.
148800120326       //--------------------------------------------------------------
148900120326       BEGSR F06W01;
149000120326
149100120326       //?Ok utente --> Aggiorna il manca tariffa da M a V
149200120329          exec sql
149300120329          UPDATE TFMTC00F
149400120329          SET MTCtrc = 'V'
149500120329          WHERE MTCaas = :W01aas and MTClnp = :W01lnp and
149600120329                MTCnrs = :W01nrs and MTCnsp = :W01nsp and
149700120329                MTCtbl = :W01tbl and MTCtrc = 'M';
149800120326
149900120326       //?OK utente --> Scrive rcd richiesta assistenza
150000120326       //?              richiama FIDNA3R
150100120326          exsr Call_FIDNA3;
150200120326
150300120326       ENDSR;
150400120326
150500120326       //--------------------------------------------------------------
150600120326       //?Richiamo pgm scrittura R.A.
150700120326       //--------------------------------------------------------------
150800120326       BEGSR Call_FIDNA3;
150900120326
151000120326         clear FIDNA3DS;
151100120326         IA3mad = ' 30';
151200120326         IA3tor = 'S';
151300120326         IA3ogg = %editc(W01lnp:'X') + %editc(W01nrs:'X') +
151400120326                  %editc(W01nsp:'X') + %editc(W01aas:'X');
151500120326         IA3no1 = 'Manca tariffa: ' + W01err;
151600120326         IA3no2 = 'Vistato dall''utente';
151700120326         fidna3r (kpjba:FIDNA3DS);
151800120326
151900120326       ENDSR;
152000120223
152100080206       //--------------------------------------------------------------
152200080206       //?Operazioni finali.
152300080206       //--------------------------------------------------------------
152400080206       BEGSR RoutEnd;
152500090521
152600080206         *inLR = *on;
152700080206         return;
152800080206
152900080206       ENDSR;
153000080206
153100080206      /end-free
153200090807
153300080206       //--------------------------------------------------------------
153400080206       //?Schiere a tempo di compilazione.
153500080206       //--------------------------------------------------------------
153600080206
153700120222** - skMSG ------------------------------------------------------------------*
153800120221Utente non abilitato alla funzione                                            01
153900120222Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    02
154000120222Filiale errata o non in gestione all'utente                                   03
154100120222Codice commerciale errato o non in gestione all'utente                        04
154200120222Password errata o non immessa                                                 05
154300120222Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              06
154400120222Password scaduta                                                              07
154500120223Opzione errata                                                                08
154600120223Codice cliente errato o non in gestione all'utente                            09
154700120326Un altro utente sta vistando questo errore!!!                                 10
