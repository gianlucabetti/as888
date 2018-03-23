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
001400130801       // -?Anagrafica commerciali
001500130801     fAZCMM01L  if   e           k disk
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
009100140508     d*/sfl_rrn              376    377i 0
009200140508     d*/min_nrr              378    379i 0
009300140508     d*/num_rcds             380    381i 0
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
012800140508     d FIDNA6ds      e ds                  inz
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
015100130801       // -?Reperimento filiali gestibili dall'utente
015200120221     d TRUL31ds      e ds
015300120223     d pog                    10    759    dim(250)
015400120223
015500120223       // -?Ricerca clienti
015600120223     d xCLIrds       e ds
015700120223
015800120223       // -?DS Tabella MTC - Password tariffe
015900120223     d dMTC          e ds
016000120223
016100120223       // -?File TFMTC00F - ds  x sql
016200120309     d TFMTCds       e ds                  extname(TFMTC00F)
016300090629
016400080206      //---------------------------------------------------------------
016500080206      //?Definizione variabili globali.
016600080206      //---------------------------------------------------------------
016700080206
016800110706       // -?Flags booleani
016900120221     d wEoF            s               n   inz(*off)
017000120221     d wErrGrave       s               n   inz(*off)
017100120130     d wFine           s               n   inz(*off)
017200120326     d wFineW01        s               n   inz(*off)
017300120130     d wInzD01         s               n   inz(*on)
017400120130     d wInzS02         s               n   inz(*off)
017500120306     d wTassaPrima     s               n   inz(*off)
017600080206
017700110706       // -?Campi associati al video
017800120130     d wVideo          s              2    inz('D1')
017900140508     d*// dsp_mod         s             10I 0
018000120127     d S02nrr          s              4  0 inz
018100110706
018200110706       // -?Campi di comodo
018300120223     d sav_Pwd         s                   like(V01pwd)
018400120306     d sav_S02nrr      s                   like(S02nrr)
018500120222     d wGiorni         s              3s 0 inz
018600120223     d wUteAbi         s                   like(OTAA1cabi)
018700120221     d w003a           s              3a   inz
018800120307     d w0070           s              7s 0 inz
018900120130
019000120130       // -?Campi di comodo data
019100120223     d wData           s              8s 0
019200140508     d*// wDataEUR        s               d   datfmt(*eur)
019300120130     d wDataISO        s               d   datfmt(*iso)
019400120222     d wDataScad       s              8s 0
019500120221     d wOggi           s              8s 0
019600120302
019700120302       // -?Campo da passare al pgm x la gestione/interrogazione bolle
019800120302     d ParTnsb         s              1a
019900120321
020000120321       // -?Campi da passare al pgm x interrogazione clienti
020100120321     d Par01           s              2a
020200120321     d Parrag          s             48a
020300120223
020400120222       // -?Indici di schiera
020500120222     d xx              s              4  0 inz
020600120222     d yy              s              4  0 inz
020700090806
020800110706       // -?Stringa SQL da eseguire
020900090806     d wSQL            s           2048    Varying        inz
021000120221
021100120221      // ----------------------------------------------------------------------
021200120221      //?Costanti per ordinamento subfile
021300120221      // ----------------------------------------------------------------------
021400120221     d MaxKey          c                   4
021500120221     d Ascendente      c                   1
021600120221     d Discendente     c                   2
021700120221     d Carattere       c                   6
021800120221     d Numerico        c                   8
021900120221     d Put             c                   1
022000120221     d EndPut          c                   2
022100120221     d Get             c                   3
022200120221      // ----------------------------------------------------------------------
022300120221      // Campi utili:
022400120221      //     nrr        - Numero relativo di record del Subfile
022500120221      //     SizeList   - Dimensione della lista
022600120221      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
022700120221      // ----------------------------------------------------------------------
022800120221     d NotUsed         s             16A
022900120221     d ReturnSize      s              9B 0
023000120221     d SizeList        s              9B 0
023100120221     d RrnLast         s              5I 0
023200140508     d*// WrkSort         s              1  0 inz(0)
023300120221      // ----------------------------------------------------------------------
023400120221      // Data Structures
023500120221      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
023600120221      //     QLGSCB     - The sort request block for the QLGSORT API
023700120221      //     QLGSCB00   - The sort request block for the QLGSRTIO API
023800120221      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
023900120221      //     QUSEC      - Error structure for the QLGSORT API
024000120221      // ----------------------------------------------------------------------
024100120221     d sflrcd          ds
024200120223     d  s02ksc
024300120223     d  s02ccm
024400120223     d  s02dsp
024500120223     d  s02lnp
024600120223     d  s02lna
024700120223     d  s02ctr
024800120223     d  s02tsp
024900120223     d  s02opz
025000120223     d  s02rag
025100120223     d  s02dccm
025200120224     d  s02err
025300120228     d  s02aas
025400120228     d  s02nrs
025500120228     d  s02nsp
025600120228     d  s02tbl
025700120228     d  s02pro
025800120228     d  s02cts
025900120305     d  s02ncl
026000120305     d  s02pkf
026100120305     d  s02vlf
026200120326     d  s02imp
026300120326     d  s02imv
026400120221     d  selected                      1A
026500120221
026600120221      /COPY QSYSINC/QRPGLESRC,QLGSORT
026700120221     d QLGKL                         16    DIM(MaxKey)
026800120221     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
026900120221     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
027000120221     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
027100120221     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
027200120221
027300120221      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
027400120221      /COPY QSYSINC/QRPGLESRC,QUSEC
027500090508
027600090508      //---------------------------------------------------------------
027700090807      //?Definizione procedure esterne.
027800090508      //---------------------------------------------------------------
027900120221
028000120221       // -?Api x ordinamento subfile
028100120221     d Qlgsort_pr      pr                  extpgm('QLGSORT')
028200120221     d  pr_QLGSCB                          like(Qlgscb)
028300120221     d  pr_NotUsed1                        like(NotUsed)
028400120221     d  pr_NotUsed2                        like(NotUsed)
028500120221     d  pr_SizeList                        like(SizeList)
028600120221     d  pr_ReturnSize                      like(ReturnSize)
028700120221     d  pr_QUSEC                           like(Qusec)
028800120221     d                                     options(*varsize)
028900120221
029000120221     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
029100120221     d  pr_QLGSCB00                        like(QLGSCB00)
029200120221     d  pr_SflRcd                          like(SflRcd)
029300120221     d  pr_NotUsed1                        like(NotUsed)
029400120221     d  pr_SizeList                        like(SizeList)
029500120221     d  pr_NotUsed2                        like(NotUsed)
029600120221     d  pr_QUSEC                           like(Qusec)
029700120221     d                                     options(*varsize)
029800120221
029900120221     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
030000120221     d  pr_QLGSCB00                        like(QLGSCB00)
030100120221     d  pr_NotUsed1                        like(NotUsed)
030200120221     d  pr_SflRcd                          like(SflRcd)
030300120221     d  pr_SizeList                        like(SizeList)
030400120221     d  pr_NotUsed2                        like(NotUsed)
030500120221     d  pr_QUSEC                           like(Qusec)
030600120221     d                                     options(*varsize)
030700120326
030800120326       // -?Richiamo Scrittura R.A.
030900140508     d fidnA6r         pr                  extpgm('FIDNA6R')
031000140508     d  fidnA6ds                           likeds(FIDNA6DS)
031100120224
031200120224       // -?Richiamo Controllo Manca Tariffe
031300120224     d tnsb45r         pr                  extpgm('TNSB45R')
031400120224     d  kpjba                              likeds(KPJBA)
031500120229
031600120229       // -?Richiamo Interrogazione bolle di Sede
031700120229     d tnsb51c         pr                  extpgm('TNSB51C')
031800120229     d  kpjba                              likeds(KPJBA)
031900120302     d  partnsb                       1a
032000120224
032100120224       // -?Richiamo Gestione bolle di Sede
032200140508     d*// tnsb52r         pr                  extpgm('TNSB52R')
032300140508     d*//  kpjba                              likeds(KPJBA)
032400120224
032500120224       // -?Richiamo Gestione Tariffe
032600120224     d tnta01r         pr                  extpgm('TNTA01R')
032700120224     d  kpjba                              likeds(KPJBA)
032800120314     d  tnsb48ds                           likeds(TNSB48DS)
032900120321
033000120321       // -?Richiamo Interrogazione Tariffe
033100120321     d tnta02r         pr                  extpgm('TNTA02R')
033200120321     d  kpjba                              likeds(KPJBA)
033300120321     d  tnsb48ds                           likeds(TNSB48DS)
033400110506
033500120221       // -?Reperimento filiali gestitbili dall'utente
033600120221     d trul31r         pr                  extpgm('TRUL31R')
033700120221     d  kpjba                              likeds(KPJBA)
033800120221     d  trul31ds                           likeds(TRUL31DS)
033900120223
034000120223       // -?Ricerca cliente
034100120223     d xclir           pr                  extpgm('XCLIR')
034200120223     d  xclirds                            likeds(XCLIRDS)
034300120321     d  par01                         2
034400120321     d  parrag                       48
034500110706
034600110706      //---------------------------------------------------------------
034700110706      //?Definizione prototipi.
034800110706      //---------------------------------------------------------------
034900120130
035000120130       // -?Ricerc/Controllo tabelle
035100120130      /copy gaitrasrc/srcprotopr,tibs02r
035200110706
035300110706       // -?Reperimento dati utente
035400110706      /copy gaitrasrc/srcprotopr,tibs34r
035500120130
035600120130       // -?Reperimento dati anagrafici cliente codificato
035700120222      /copy gaitrasrc/srcprotopi,tibs69r
035800120222      /copy gaitrasrc/srcprotopr,tibs69r
035900120221
036000120221       // -?Controllo abilitazioni utente
036100120221      /copy gaitrasrc/srcprotopr,tntaa1c
036200120221
036300120221       // -?Ricerca commerciale
036400130801      /copy gaitrasrc/srcprotoPI,TRMK43R_1
036500130801      /copy gaitrasrc/srcprotoPR,TRMK43R
036600090807
036700080206      //---------------------------------------------------------------
036800080206      //?Definizione key-list.
036900080206      //---------------------------------------------------------------
037000130801
037100130801       // -?File AZCMM01L
037200130801     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
037300130801     d                                     prefix(k_)
037400080206
037500080206      //---------------------------------------------------------------
037600080206      //?Riepilogo indicatori.
037700080206      //---------------------------------------------------------------
037800080206      //---------------------------------------------------------------
037900080206
038000080206      //---------------------------------------------------------------
038100080206      //?M A I N - L I N E
038200080206      //---------------------------------------------------------------
038300080206
038400080206     c     *Entry        plist
038500080206     c                   parm                    KPJBA
038600080206
038700080206      /free
038800080206
038900090807       //?Operazioni iniziali
039000080206       exsr RoutInz;
039100080206
039200090807       //?Gestione video
039300120326       DOW  wFine = *off;
039400090807
039500080206         select;
039600120130           when wVideo = 'D1';
039700110706             exsr GesD01;
039800120130           when wVideo = 'S2';
039900120130             exsr GesS02;
040000090428           other;
040100120130             wFine = *on;
040200080206         endsl;
040300090807
040400080206       ENDDO;
040500080206
040600090807       //?Operazioni finali
040700080206       exsr RoutEnd;
040800080206
040900080206       //--------------------------------------------------------------
041000080206       //?Operazioni iniziali.
041100080206       //--------------------------------------------------------------
041200080206       BEGSR RoutInz;
041300120309
041400120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
041500080206
041600120130         wVideo = 'D1';
041700120130         wInzD01 = *on;
041800090807
041900110706       //?Reperimento dati job
042000090807         exsr DatiJob;
042100120221
042200120221       //?Controllo se utente autorizzato alla funzione
042300120221         reset TNTAA1DS;
042400120221         ITAA1caut = '§utegtc';
042500120221         kpjbu     = TNTAA1DS;
042600120221         tntaa1c (kpjba);
042700120221         TNTAA1DS = kpjbu;
042800120221
042900120221         IF  OTAA1fabi = 'N';
043000120221           wErrGrave   = *on;
043100120221           ErrMessage  = *on;
043200120221           ErrGenerico = *on;
043300120221           V01msg = skMsg(01);
043400120221           leavesr;
043500120221         ENDIF;
043600120223
043700120223         wUteAbi = OTAA1cabi;
043800120221
043900120221         clear TRUL31DS;
044000120221         I31abi = OTAA1cabi;
044100120221         I31cdi = DUTdis;
044200120221         I31car = DUTare;
044300120221         I31cpo = DUTpou;
044400120221         trul31r (kpjba : trul31ds);
044500120221         IF  O31pog <= *zeros;
044600120221           wErrGrave   = *on;
044700120221           ErrMessage  = *on;
044800120221           ErrGenerico = *on;
044900120223           V01msg = skMsg(01);
045000120221           leavesr;
045100120221         ENDIF;
045200120309
045300090806
045400110706       //?Impostazione campi "fissi"
045500110706         V1Tpgm = SDSpgm;
045600120221
045700120221       //?Data del giorno
045800120221         wOggi = %dec(%date());
045900120223
046000120223       //?Cerco la tabella gg. validità password
046100120223         clear TIBS02DS;
046200120223         clear dMTC;
046300120223         T02mod = 'C';
046400120223         T02cod = 'MTC';
046500120223         T02sif = knsif;
046600120223         T02ke1 = 'PSW';
046700120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
046800120223         IF  T02err <> *blanks;
046900120223           wErrGrave   = *on;
047000120223           ErrMessage  = *on;
047100120223           ErrGenerico = *on;
047200120223           PosCurPWD   = *on;
047300120223           V01msg      = skMsg(06);
047400120223           leavesr;
047500120223         ENDIF;
047600120223         wGiorni = %dec(T02uni:3:0);
047700120222
047800120222       //?Cerco la password memorizzata per filiale (utente)
047900120222         clear TIBS02DS;
048000120222         clear dMTC;
048100120222         T02mod = 'C';
048200120222         T02cod = 'MTC';
048300120222         T02sif = knsif;
048400120223         T02ke1 = %editc(DUTpou:'X');
048500120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
048600120222         IF  T02err <> *blanks;
048700120222           wErrGrave   = *on;
048800120222           ErrMessage  = *on;
048900120222           ErrGenerico = *on;
049000120222           V01msg = skMsg(02);
049100120222           leavesr;
049200120222         ENDIF;
049300120222         dMTC = T02uni;
049400120223
049500120223         clear sav_Pwd;
049600090714
049700080206       ENDSR;
049800080206
049900080206       //--------------------------------------------------------------
050000080206       //?Reperimento Dati del job (Utente/Operativi).
050100080206       //--------------------------------------------------------------
050200080206       BEGSR DatiJob;
050300080206
050400080206         in(E) §AzUte;
050500110706         IF NOT %error;
050600080206           in(E) §DatiUte;
050700110706         ENDIF;
050800110706         IF %error or RSut = *blanks;
050900080206           clear TIBS34ds;
051000080206           tibs34r(tibs34ds);
051100080206           in §AzUte;
051200080206           in §DatiUte;
051300110706         ENDIF;
051400080206
051500080206       ENDSR;
051600080206
051700090421       //--------------------------------------------------------------
051800110706       //?Gestione videata D01
051900080206       //--------------------------------------------------------------
052000110706       BEGSR GesD01;
052100080207
052200110706       //?Inizializzazione videata
052300120130         IF  wInzD01 = *on;
052400110706            exsr InzD01;
052500120130            wInzD01  = *off;
052600110706         ENDIF;
052700080207
052800110706       //?Emissione Testata
052900120221         write  SB48T01;
053000080207
053100110706       //?Emissione videata
053200120221         exfmt  SB48D01;
053300110706         reset ErrMessage;
053400110706         reset ErrGenerico;
053500120127         clear V01msg;
053600080207
053700080207         SELECT;
053800120227
053900120227         //?- Errore grave esco dal pgm
054000120227           WHEN  wErrGrave;
054100120227             wFine = *on;
054200110706
054300110706         //?- F03=Fine
054400110706           WHEN  dsp_aid = c_F03;
054500110706             exsr F03D01;
054600080207
054700090807         //?Invio
054800080207           OTHER;
054900110706         //?Eseguo i controlli
055000110706             exsr CtrD01;
055100110706             IF  ErrGenerico;
055200080207               leavesr;
055300110706             ENDIF;
055400120127         //?Se tutto OK vado nella seconda videata
055500120130           wVideo = 'S2';
055600120130           wInzS02 = *on;
055700080207
055800080207         ENDSL;
055900080207
056000080207       ENDSR;
056100080207
056200080207       //--------------------------------------------------------------
056300110706       //?Inizializzazione videata D01.
056400080207       //--------------------------------------------------------------
056500110706       BEGSR InzD01;
056600080207
056700120221         clear V01fil;
056800120302         V01dfil = '(0 = Tutte)';
056900120221         clear V01ccm;
057000120221         clear V01dccm;
057100120221         clear V01ksc;
057200120221         clear V01rag;
057300120223
057400120223         V01pwd = sav_Pwd;
057500090508
057600080207       ENDSR;
057700080207
057800091111       //--------------------------------------------------------------
057900110706       //?Gestione tasto funzionale F03 da videata D01.
058000110706       //?F3=Fine
058100091111       //--------------------------------------------------------------
058200110706       BEGSR F03D01;
058300091111
058400110706       //?Chiusura del programma
058500120130         wFine = *on;
058600091111
058700091111       ENDSR;
058800120130
058900120130       //--------------------------------------------------------------
059000120130       //?Controllo dati videata D01.
059100120130       //--------------------------------------------------------------
059200120130       BEGSR CtrD01;
059300120130
059400120130         WindDspF  = IndDspF;
059500120130         reset WindDspFb;
059600120130         IndDspF   = WindDspF;
059700120130
059800120221       //?Controllo la filiale
059900120221         IF  V01fil > 0;
060000120221           chain V01fil AZORG01L;
060100120223           IF not %found(AZORG01L);
060200120221             ErrMessage  = *on;
060300120221             ErrGenerico = *on;
060400120221             PosCurFIL   = *on;
060500120222             V01msg      = skMsg(03);
060600120221             leavesr;
060700120221           ENDIF;
060800120221         //?Verifico se l'utente è abilitato alla filiale
060900120221           w003a = %editc(V01fil:'X');
061000120221           IF  %lookup(w003a:pog) = 0;
061100120221             ErrMessage  = *on;
061200120221             ErrGenerico = *on;
061300120221             PosCurFIL   = *on;
061400120222             V01msg      = skMsg(03);
061500120221             leavesr;
061600120221           ENDIF;
061700120130         ENDIF;
061800120221
061900130801       //?Ricerca del commerciale unificante
062000120221         IF  %scan('?' : V01ccm) > 0;
062100120221           ErrGenerico = *on;
062200120222           PosCurCCM   = *on;
062300130801           clear  TRMK43ds;
062400130801           iMK43solU = 'S';
062500130801           iMK43fil  = DUTpou;
062600130801           TRMK43R (kpjba : TRMK43ds);
062700130801           if  oMK43err = *off  and  oMK43fxx = *blank;
062800130801             V01ccm  = %editc( oMK43cmm : 'X' );
062900130801             V01dccm = oMK43des;
063000130801           endif;
063100120221           leavesr;
063200120221         ENDIF;
063300120221
063400120221       //?Controllo il commerciale
063500120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
063600120221
063700120221         //?Solo valori numerici
063800120223           IF  %check(c_digits:V01ccm) > 0;
063900120221             ErrMessage  = *on;
064000120221             ErrGenerico = *on;
064100120222             PosCurCCM   = *on;
064200120222             V01msg      = skMsg(04);
064300120221             leavesr;
064400120221           ENDIF;
064500120221
064600120221         //?Controllo se utente abilitato al commerciale
064700120221           clear TNTAA1DS;
064800120221           ITAA1caut = '§utegtc';
064900120221           ITAA1cmm  = %dec(V01ccm:7:0);
065000120221           kpjbu     = tntaa1ds;
065100120221           tntaa1c (kpjba);
065200120221           TNTAA1DS = kpjbu;
065300120221           IF  OTAA1fabi = 'N';
065400120221             ErrMessage  = *on;
065500120221             ErrGenerico = *on;
065600120222             PosCurCCM   = *on;
065700120222             V01msg      = skMsg(04);
065800120221             leavesr;
065900120221           ENDIF;
066000120221
066100120221         //?Controllo se commerciale valido
066200130801           k_CMMcod = %int(V01ccm);
066300130801           chain  %kds(k_azcmm01) AZCMM000;
066400130801           IF  not %found(AZCMM01L);
066500120221             ErrMessage  = *on;
066600120221             ErrGenerico = *on;
066700120222             PosCurCCM   = *on;
066800120222             V01msg      = skMsg(04);
066900120221             leavesr;
067000120221           ENDIF;
067100120221
067200130801           V01dccm = CMMdes;
067300120221
067400120221         //?Commerciale senza particolarità
067500130801           IF  CMMpar <> *blanks;
067600120221             ErrMessage  = *on;
067700120221             ErrGenerico = *on;
067800120222             PosCurCCM   = *on;
067900120222             V01msg      = skMsg(04);
068000120221             leavesr;
068100120221           ENDIF;
068200120221
068300120221         //?Commerciale "valido" data inizio e fine attività
068400130801           IF  CMMdtIni > wOggi  or
068500130801               CMMdtFin < wOggi;
068600120221             ErrMessage = *on;
068700120221             ErrGenerico = *on;
068800120222             PosCurCCM   = *on;
068900120222             V01msg      = skMsg(04);
069000120221             leavesr;
069100120221           ENDIF;
069200120412
069300120412         //?Deve essere un commerciale Unificante
069400130801           IF  %dec(V01ccm:7:0) <> CMMuni;
069500120412             ErrMessage = *on;
069600120412             ErrGenerico = *on;
069700120412             PosCurCCM   = *on;
069800120412             V01msg      = skMsg(11);
069900120412             leavesr;
070000120412           ENDIF;
070100120221         ENDIF;
070200120223
070300120223       //?Ricerca del cliente
070400120223         IF  %scan('?' : V01ksc) > 0;
070500120223           ErrGenerico = *on;
070600120223           PosCurKSC   = *on;
070700120223           clear xCLIrds;
070800120321           clear par01;
070900120321           clear parrag;
071000120223           DXCaut = wUteAbi;
071100120223           DXCcap = DUTkci;
071200120321           xclir (xCLIrds:par01:parrag);
071300120223           ksa = DXCksc;
071400120223           dsksa = ksa(1);
071500120223           ksc(1) = dsksn;
071600120223           V01ksc = %editc(ksc(1):'X');
071700120223         ENDIF;
071800120223
071900130801       //?Controllo il cliente
072000120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
072100120223
072200120223         //?Solo valori numerici
072300120223           IF  %check(c_digits:V01ksc) > 0;
072400120223             ErrMessage  = *on;
072500120223             ErrGenerico = *on;
072600120223             PosCurKSC   = *on;
072700120223             V01msg      = skMsg(09);
072800120223             leavesr;
072900120223           ENDIF;
073000120223
073100120223         //?Controllo se utente abilitato al cliente
073200120223           clear TNTAA1DS;
073300120223           ITAA1caut = '§utegtc';
073400120223           ITAA1ksc  = %dec(V01ksc:7:0);
073500120223           kpjbu     = tntaa1ds;
073600120223           tntaa1c (kpjba);
073700120223           TNTAA1DS = kpjbu;
073800120223           IF  OTAA1fabi = 'N';
073900120223             ErrMessage  = *on;
074000120223             ErrGenerico = *on;
074100120223             PosCurKSC   = *on;
074200120223             V01msg      = skMsg(09);
074300120223             leavesr;
074400120223           ENDIF;
074500120223
074600120223         //?Controllo se cliente valido
074700120223           clear tibs69ds;
074800120223           I69kac = %dec(V01ksc:7:0);
074900120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
075000120223           IF  O69err <> *blanks;
075100120223             ErrMessage  = *on;
075200120223             ErrGenerico = *on;
075300120223             PosCurKSC   = *on;
075400120223             V01msg      = skMsg(09);
075500120223             leavesr;
075600120223           ENDIF;
075700120223           V01rag = ACOrag;
075800120223         ENDIF;
075900120221
076000120222       //?Password obbligatoria
076100120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
076200120222           ErrMessage = *on;
076300120222           ErrGenerico = *on;
076400120222           PosCurPWD   = *on;
076500120222           V01msg      = skMsg(05);
076600120222           leavesr;
076700120222         ENDIF;
076800120222
076900120222       //?Controllo se password OK
077000120222         IF  V01pwd <> §MTCpwd;
077100120222           ErrMessage = *on;
077200120222           ErrGenerico = *on;
077300120222           PosCurPWD   = *on;
077400120222           V01msg      = skMsg(05);
077500120222           clear V01pwd;
077600120222           leavesr;
077700120222         ENDIF;
077800120222
077900120222       //?Data immissione/modifica password
078000120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
078100120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
078200120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
078300120223         wDataISO = %date(wData);
078400120223
078500120223       //?Calcolo la data scadenza password
078600120223         wDataISO += %days(wgiorni);
078700120223         wDataScad = %dec(wDataISO);
078800120223
078900120223       //?Controllo se la password è scaduta
079000120222         IF wOggi > wDataScad;
079100120222           ErrMessage  = *on;
079200120222           ErrGenerico = *on;
079300120222           PosCurPWD   = *on;
079400120222           V01msg      = skMsg(07);
079500120222           leavesr;
079600120222         ENDIF;
079700120223
079800120223         sav_Pwd = V01pwd;
079900120130
080000120130       ENDSR;
080100120130
080200120130       //--------------------------------------------------------------
080300120130       //?Gestione videata S02
080400120130       //--------------------------------------------------------------
080500120130       BEGSR GesS02;
080600120306
080700120306       //?Se non l'ho ancora fatto tasso tutto
080800120306       //?(capita solo appena entro nel pgm)
080900120306         IF  not wTassaPrima;
081000120309           exsr F05S02;
081100120306           wTassaPrima = *on;
081200120306         ENDIF;
081300120130
081400120130       //?Inizializzazione videata
081500120130         IF  wInzS02 = *on;
081600120130            exsr InzS02;
081700120130            wInzS02  = *off;
081800120130         ENDIF;
081900120130
082000120130       //?Emissione Testata + Piede
082100120221         write  SB48T01;
082200120221         write  SB48P02;
082300120130
082400120130       //?Emissione subfile
082500120221         exfmt  SB48C02;
082600120130         reset ErrMessage;
082700120130         reset ErrGenerico;
082800120130         clear V02msg;
082900120130
083000120130         SELECT;
083100120130
083200120130         //?- F03=Fine
083300120130           WHEN  dsp_aid = c_F03;
083400120130             exsr F03D01;
083500120130
083600120222         //?- F05=Ritassa
083700120222           WHEN  dsp_aid = c_F05;
083800120222             exsr F05S02;
083900120222
084000120228         //?- F08=Ordina
084100120222           WHEN  dsp_aid = c_F08;
084200120229           //?Se era ordina per cliente diventa ordina per commerciale
084300120229             IF  Ord_xksc;
084400120229               Ord_xksc = *off;
084500120229               Ord_Xccm = *on;
084600120229               wF08 = 'F8=Ord.x Cliente';
084700120229           //?Se era ordina per cliente diventa ordina per commerciale
084800120229             ELSE;
084900120229               Ord_xksc = *on;
085000120229               Ord_Xccm = *off;
085100120229               wF08 = 'F8=Ord.x Commerciale';
085200120229             ENDIF;
085300120222             exsr F08S02;
085400120130
085500120130         //?- F12=Ritorno
085600120130           WHEN  dsp_aid = c_F12;
085700120130             exsr F12S02;
085800120130
085900120130         //?Invio
086000120130           OTHER;
086100120222         //?Opzioni
086200120223             IF  S02nrr > *zeros;
086300120223               exsr OpzS02 ;
086400120223             ENDIF;
086500120130             IF  ErrGenerico;
086600120130               leavesr;
086700120130             ENDIF;
086800120130
086900120130         ENDSL;
087000120130
087100120130       ENDSR;
087200120130
087300120130       //--------------------------------------------------------------
087400120130       //?Inizializzazione videata S02.
087500120130       //--------------------------------------------------------------
087600120130       BEGSR InzS02;
087700120130
087800120130       //?Pulizia subfile
087900120130         exsr PulS02;
088000120222
088100120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
088200120222         exsr PrepSql;
088300120130
088400120130       //?Caricamento dati
088500120130         exsr CarS02;
088600120130
088700120130       //?Visualizzazione del Subfile
088800120130         SflDsp = (S02nrr <= *zeros);
088900120130
089000120130       //?Attivazione SFLEND
089100120130         SflEnd = *on;
089200120130
089300120130       //?Impaginazione Subfile
089400120130       //?Forzo sempre il primo rcd
089500120130         IF  S02nrr > *zero;
089600120130           C02rcd = 1;
089700120130         ELSE;
089800120130           clear C02rcd;
089900120130         ENDIF;
090000120130
090100120130         C02csr = C02rcd;
090200120130
090300120130       ENDSR;
090400120130
090500120130       //--------------------------------------------------------------
090600120130       //?Pulizia Subfile S02.
090700120130       //--------------------------------------------------------------
090800120130       BEGSR PulS02;
090900120130
091000120130       //?Pulizia subfile
091100120130         SflDsp    = *on;
091200120130         SflDspCtl = *on;
091300120221         write  SB48C02;
091400120130         SflDspCtl = *off;
091500120130         SflEnd    = *off;
091600120130
091700120130         clear C02rcd;
091800120130         clear C02csr;
091900120130         clear S02nrr;
092000120130         clear V02msg;
092100120130         ErrMessage  = *off;
092200120130         ErrGenerico = *off;
092300120130
092400120130         WindDspF = IndDspF;
092500120130         reset WindDspFb;
092600120130         IndDspF  = WindDspF;
092700120130
092800120130       ENDSR;
092900120222
093000120222       //--------------------------------------------------------------
093100120222       //?Preparazione stringa SQL.
093200120222       //--------------------------------------------------------------
093300120222       BEGSR PrepSql;
093400120307
093500120307         clear w0070;
093600120222
093700120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
093800120222
093900120307       //?Se nessuna richiesta a video
094000120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
094100120307            (V01ccm = *blanks or V01ccm = *zeros);
094200120222       //?Carico le bolle con filiale cliente = sk POG
094300120222           wSQL += ' and MTCpoc in(';
094400120222           yy = 0;
094500120222           xx = 1;
094600120222           FOR xx by 1 to %elem(pog);
094700120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
094800120222               IF yy > 0;
094900120222                 wSQL += ', ';
095000120222               ELSE;
095100120222               yy = 1;
095200120222               ENDIF;
095300120222               wSQL += '''';
095400120222               wSQL += pog(xx);
095500120222               wSQL += '''';
095600120222             ENDIF;
095700120222           ENDFOR;
095800120222           wSQL += ')';
095900120222         ELSE;
096000120307         //?Se richiesta filiale a video
096100120307           IF  V01fil > 0;
096200120307         //?Carico le bolle con filiale cliente = filiale richiesta
096300120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
096400120307           ENDIF;
096500120307         //?Se richiesto commerciale a video
096600120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
096700120307              w0070 = %dec(V01ccm:7:0);
096800120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
096900120307           ENDIF;
097000120307         //?Se richiesto cliente a video
097100120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
097200120307              w0070 = %dec(V01ksc:7:0);
097300120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
097400120307           ENDIF;
097500120222         ENDIF;
097600120309
097700120309         wSQL += ' for FETCH ONLY';
097800120222
097900120222       ENDSR;
098000120130
098100120130       //--------------------------------------------------------------
098200120130       //?Caricamento Subfile S02.
098300120130       //--------------------------------------------------------------
098400120130       BEGSR CarS02;
098500120312
098600120312         clear sav_s02nrr;
098700120130
098800120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
098900120222         exec sql prepare A1 from :wSQL;
099000120222         exec sql declare MTC cursor for A1;
099100120222         exec sql open MTC;
099200120222
099300120222         IF  sqlcode < 0;
099400120222           wEoF = *on;
099500120222         ENDIF;
099600120222
099700120222         DOW  not wEoF;
099800120309           exec sql fetch next from MTC into :TFMTCds;
099900120222           IF  sqlcod = 100 or sqlcod < 0;
100000120222             wEoF = *on;
100100120222             leave;
100200120222           ENDIF;
100300120222
100400120222         //?Carico il subfile
100500120222           exsr RieS02;
100600120222         //?Scrico il subfile
100700120222           S02nrr += 1;
100800120222           write SB48S02;
100900120306           sav_S02nrr = S02nrr;
101000120229           IF  S02nrr = 9998;
101100120229             leave;
101200120229           ENDIF;
101300120222
101400120222         ENDDO;
101500120222
101600120222         exec sql close MTC;
101700120130         wEoF = *off;
101800120222
101900120222         //?Dopo aver caricato il subfile lo ordino per cliente
102000120229         wF08 = 'F8=Ord.x Commerciale';
102100120228         Ord_xksc = *on;
102200120228         Ord_Xccm = *off;
102300120228         exsr F08S02;
102400120130
102500120130       ENDSR;
102600120130
102700120130       //--------------------------------------------------------------
102800120130       //?Carico i dati nel subfile S02.
102900120130       //--------------------------------------------------------------
103000120130       BEGSR RieS02;
103100120130
103200120221         clear  SB48S02;
103300120130
103400120228         S02aas = MTCaas;
103500120228         S02nrs = MTCnrs;
103600120228         S02nsp = MTCnsp;
103700120228         S02tbl = MTCtbl;
103800120222
103900120222         S02ksc = MTCksc;
104000120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
104100120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
104200120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
104300120222         S02ctr = MTCctr;
104400120222         S02ccm = MTCage;
104500120222         S02err = MTCerr;
104600120223         S02lnp = MTClnp;
104700120223         S02lna = MTClna;
104800120223         S02tsp = MTCtsp;
104900120228         S02pro = MTCpro;
105000120228         S02cts = MTCcts;
105100120305         S02ncl = MTCncl;
105200120305         S02pkf = MTCpkf;
105300120305         S02vlf = MTCvlf;
105400120326
105500120326         S02imp = MTCimp;
105600120326         S02imv = MTCimv;
105700120130
105800120222       //?Decodifico il cliente
105900120222         clear tibs69ds;
106000120222         I69kac = S02ksc;
106100120222         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
106200120222         IF  O69err = *blanks;
106300120222           S02rag = ACOrag;
106400120222         ENDIF;
106500120222
106600120222       //?Decodifico il commerciale
106700130801         clear  S02dccm;
106800130801         k_CMMcod = S02ccm;
106900130801         chain  %kds(k_azcmm01) AZCMM000;
107000130801         IF  %found(AZCMM01L);
107100130801           S02dccm = CMMdes;
107200130801         ENDIF;
107300120130
107400120130       ENDSR;
107500120130
107600120130       //--------------------------------------------------------------
107700120222       //?Gestione delle opzioni immesse nella videata S02.
107800120130       //--------------------------------------------------------------
107900120222       BEGSR OpzS02;
108000120130
108100120223         readc SB48S02;
108200120223
108300120223         DOW  not %eof(TNSB48D);
108400120223
108500120223           SflNxtChg = *off;
108600120223
108700120223           WindDspF = IndDspF;
108800120223           reset WindDspFb;
108900120223           IndDspF  = WindDspF;
109000120223
109100120223           C02rcd = S02nrr;
109200120223
109300120223           SELECT;
109400120223
109500120223         //?Nessuna opzione immessa
109600120223             WHEN  S02opz  = *blanks;
109700120223
109800120223         //?Precedente segnalazione di errore
109900120223             WHEN  S02opz  = 'E';
110000120223               clear S02opz;
110100120223
110200120223         //?B = Manutenzione bolla di sede
110300120223             WHEN  S02opz  = 'B';
110400120223               exsr Call_TNSB52;
110500120223
110600120223         //?T = Manutenzione tariffa
110700120223             WHEN  S02opz  = 'T';
110800120223               exsr Call_TNTA01;
110900120326
111000120326         //?V = OK utente
111100120326             WHEN  S02opz  = 'V' and S02imp = 'IMP';
111200120326               exsr Call_visto;
111300120321
111400120321         //?3 = Interrogazione tariffa
111500120321             WHEN  S02opz  = '3';
111600120321               exsr Call_TNTA02;
111700120321
111800120321         //?4 = Interrogazione clienti
111900120321             WHEN  S02opz  = '4';
112000120321               exsr Call_XCLIR;
112100120223
112200120223         //?5 = Visualizza bolla di sede
112300120223             WHEN  S02opz  = '5';
112400120223               exsr Call_TNSB51;
112500120223
112600120223         //?Opzione non valida
112700120223             OTHER;
112800120223               ErrMessage  = *on;
112900120223               ErrGenerico = *on;
113000120223               PosCurOPZ   = *on;
113100120223               V02msg = skMsg(08);
113200120223
113300120223           ENDSL;
113400120223
113500120223         //?Aggiorno il subfile
113600120223           SELECT;
113700120223             WHEN  ErrMessage;
113800120223               SflNxtChg = *on;
113900120223               C02csr    = C02rcd;
114000120223             WHEN  ErrGenerico;
114100120223               C02csr = C02rcd;
114200120223               clear S02opz;
114300120223             OTHER;
114400120223               C02csr = C02rcd;
114500120223               clear S02opz;
114600120223           ENDSL;
114700120223
114800120223           update SB48S02;
114900120223
115000120223           IF  ErrMessage or ErrGenerico;
115100120223             leave;
115200120223           ENDIF;
115300120223
115400120223           readc SB48S02;
115500120223
115600120223         ENDDO;
115700120130
115800120223       ENDSR;
115900120222
116000120222       //--------------------------------------------------------------
116100120222       //?Gestione tasto funzionale F05 da videata S02.
116200120222       //?F05=Ritassa
116300120222       //--------------------------------------------------------------
116400120222       BEGSR F05S02;
116500120224
116600120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
116700120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
116800120224            (V01ccm = *blanks or V01ccm = *zeros);
116900120224           xx = 1;
117000120224           FOR xx by 1 to %elem(pog);
117100120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
117200120224               clear TNSB44DS;
117300120224               D44fil = %dec(pog(xx):3:0);
117400120224               D44sb48 = 'S';
117500120224               kpjbu = TNSB44DS;
117600120224               tnsb45r (kpjba);
117700120224             ENDIF;
117800120224           ENDFOR;
117900120224
118000120224         ELSE;
118100120224
118200120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
118300120224           clear TNSB44DS;
118400120224           D44fil = V01fil;
118500120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
118600120224             D44ccm = %dec(V01ccm:7:0);
118700120224           ENDIF;
118800120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
118900120224             D44cli = %dec(V01ksc:7:0);
119000120224           ENDIF;
119100120224           D44sb48 = 'S';
119200120224           kpjbu = TNSB44DS;
119300120224           tnsb45r (kpjba);
119400120224
119500120224         ENDIF;
119600120224
119700120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
119800120224         wVideo = 'S2';
119900120224         wInzS02 = *on;
120000120222
120100120223       ENDSR;
120200120222
120300120222       //--------------------------------------------------------------
120400120222       //?Gestione tasto funzionale F08 da videata S02.
120500120228       //?F08=Ordinamento
120600120222       //--------------------------------------------------------------
120700120222       BEGSR F08S02;
120800120222
120900120306         rrnlast = sav_S02nrr;
121000120222
121100120222       //?Inizializza i campi chiave x l'ordinamento.
121200120222         clear QLGSCB;
121300120222         clear QLGSCB00;
121400120222
121500120418       //?2 campi chiave
121600120418         QLGNBRK = 2;
121700120222
121800120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
121900120228         IF  Ord_xksc;
122000120228           QLGSP = 1;
122100120228           QLGSS = %SIZE(S02ksc);
122200120228         ENDIF;
122300120418       //?imposto la posizione del Commerciale sul subfile e la lunghezza
122400120228         IF  Ord_xccm;
122500120228           QLGSP = 1 + %size(S02ksc);
122600120228           QLGSS = %SIZE(S02ccm);
122700120228         ENDIF;
122800120418
122900120222         QLGDT = Numerico  ;
123000120222         QLGSO = Ascendente;
123100120222         QLGKL(1) = QLGSKL;
123200120418
123300120418       //?imposto la posizione della Data sul subfile e la lunghezza
123400120418         IF  Ord_xksc;
123500120418           QLGSP = 1 + %size(S02ksc) + %size(S02ccm);
123600120418           QLGSS = %SIZE(S02dsp);
123700120418         ENDIF;
123800120418       //?imposto la posizione del Cliente sul subfile e la lunghezza
123900120418         IF  Ord_xccm;
124000120418           QLGSP = 1;
124100120418           QLGSS = %SIZE(S02ksc);
124200120418         ENDIF;
124300120418         QLGDT = Numerico  ;
124400120418         QLGSO = Ascendente;
124500120418         QLGKL(2) = QLGSKL;
124600120222
124700120222       //?Load other sort parameters.
124800120222         QLGLB = 80 + 16 * MaxKey;
124900120222         QLGRL = %SIZE(sflrcd) - 1;
125000120222         QLGRT = 8;
125100120222         QLGOKL = 80;
125200120222         QLGLKE = 16;
125300120222         QLGLSS = 290;
125400120222
125500120222       //?Initialize Sort I/O API fields.
125600120222         QLGRL00 = QLGRL;
125700120222         QLGRC00 = 1;
125800120222         clear QUSEI;
125900120222         QUSBPRV = %SIZE(QUSEC);
126000120222
126100120222       //?First step - Initialize the sort routine.
126200120222         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
126300120222
126400120222       //?Next step - Write records to I/O routine.
126500120222         QLGRT00 = Put;
126600120222         FOR xx = 1 to rrnlast;
126700120222           chain xx SB48S02;
126800120222
126900120222       //?solo le righe con Selected = 'Y' sono riordinate,
127000120222       //?quindi per fare un ordinamento di tutte le righe
127100120222       //?metto 'Y' sempre.
127200120222           selected  = 'Y';
127300120222           clear QUSEI;
127400120222           QUSBPRV = %SIZE(QUSEC);
127500120222           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
127600120222         ENDFOR;
127700120222
127800120222       //?Next step - Signal end of input, clear subfile for reload.
127900120222         QLGRT00 = EndPut;
128000120222         clear QUSEI;
128100120222         QUSBPRV = %SIZE(QUSEC);
128200120222         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
128300120222
128400120222       //?Pulizia SFL
128500120222         exsr PulS02;
128600120222
128700120222       //?Final step - Write the records back to the subfile.
128800120222         QLGRT00 = Get;
128900120222         FOR xx = 1 to rrnlast;
129000120222           clear QUSEI;
129100120222           QUSBPRV = %SIZE(QUSEC);
129200120222           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
129300120222           S02nrr = xx;
129400120222           write SB48S02;
129500120222         ENDFOR;
129600120222
129700120222       //?Visualizzazione del SFL (se ci sono dati)
129800120223         SflDsp = (S02nrr <= *zeros);
129900120222
130000120222       //?Attivazione del SFLEND
130100120222         SflEnd = *on;
130200120222
130300120222       //?Posizionamento cursore al 1° rcd della pagina
130400120222         IF  S02nrr > *zero;
130500120222           C02rcd = 1;
130600120222         ELSE;
130700120222           clear C02rcd;
130800120222         ENDIF;
130900120222
131000120222         C02csr = C02rcd;
131100120222
131200120222       ENDSR;
131300120222
131400120222       //--------------------------------------------------------------
131500120222       //?Gestione tasto funzionale F12 da videata S02.
131600120222       //?F12=Ritorno
131700120222       //--------------------------------------------------------------
131800120222       BEGSR F12S02;
131900120222
132000120222         wVideo = 'D1';
132100120222         wInzD01 = *on;
132200120222
132300120222       ENDSR;
132400120326
132500120326       //--------------------------------------------------------------
132600120326       //?Richiamo pgm Interrogazione bolle di sede.
132700120326       //--------------------------------------------------------------
132800120326       BEGSR Call_TNSB51;
132900120326
133000120326         clear TNSB51DS;
133100120326         I51op0 = 'B00';
133200120326         I51aas = S02aas;
133300120326         I51lnp = S02lnp;
133400120326         I51nrs = S02nrs;
133500120326         I51nsp = S02nsp;
133600120326         I51tbl = S02tbl;
133700120326         kpjbu = TNSB51DS;
133800120326         ParTnsb = '5';
133900120326         tnsb51c (kpjba:ParTnsb);
134000120326         TNSB51DS = kpjbu;
134100120326         IF  O51err = '1';
134200120326           ErrMessage  = *on;
134300120326           ErrGenerico = *on;
134400120326           PosCurOPZ   = *on;
134500120326           V02msg = O51msg;
134600120326         ENDIF;
134700120326
134800120326       ENDSR;
134900090807
135000120223       //--------------------------------------------------------------
135100120223       //?Richiamo pgm Gestione bolle di sede.
135200120223       //--------------------------------------------------------------
135300120223       BEGSR Call_TNSB52;
135400120223
135500120224         clear TNSB01DS;
135600120327         D01op0 = 'T02';
135700120228         D01aas = S02aas;
135800120224         D01lnp = S02lnp;
135900120228         D01nrs = S02nrs;
136000120228         D01nsp = S02nsp;
136100120228         D01tbl = S02tbl;
136200120224         kpjbu = TNSB01DS;
136300120302         ParTnsb = '2';
136400120302         tnsb51c (kpjba:ParTnsb);
136500120301         TNSB01DS = kpjbu;
136600120301         IF  D01err = '1';
136700120301           ErrMessage  = *on;
136800120301           ErrGenerico = *on;
136900120301           PosCurOPZ   = *on;
137000120301           V02msg = D01msg;
137100120301         ENDIF;
137200120223
137300120223       ENDSR;
137400120223
137500120223       //--------------------------------------------------------------
137600120223       //?Richiamo pgm Gestione Tariffa.
137700120223       //--------------------------------------------------------------
137800120223       BEGSR Call_TNTA01;
137900120223
138000120314         clear TNSB48DS;
138100120314         ISB48ksc  = S02ksc;
138200120314         ISB48rag  = S02rag;
138300120314         ISB48dsp  = S02dsp;
138400120314         ISB48tbl  = S02tbl;
138500120314         ISB48lnp  = S02lnp;
138600120314         ISB48lna  = S02lna;
138700120314         ISB48pro  = S02pro;
138800120314         ISB48cts  = S02cts;
138900120314         ISB48ctr  = S02ctr;
139000120314         ISB48ncl  = S02ncl;
139100120314         ISB48pkf  = S02pkf;
139200120314         ISB48vlf  = S02vlf;
139300120314         ISB48err  = S02err;
139400120314         tnta01r (kpjba:TNSB48DS);
139500120314         IF  OSB48err <> *blanks;
139600120228           ErrMessage  = *on;
139700120228           ErrGenerico = *on;
139800120228           PosCurOPZ   = *on;
139900120228           V02msg = skMsg(08);
140000120228         ENDIF;
140100120223
140200120223       ENDSR;
140300120321
140400120321       //--------------------------------------------------------------
140500120321       //?Richiamo pgm Interrogazione Tariffa.
140600120321       //--------------------------------------------------------------
140700120321       BEGSR Call_TNTA02;
140800120321
140900120321         clear TNSB48DS;
141000120321         ISB48ksc  = S02ksc;
141100120321         ISB48rag  = S02rag;
141200120321         ISB48dsp  = S02dsp;
141300120321         ISB48tbl  = S02tbl;
141400120321         ISB48lnp  = S02lnp;
141500120321         ISB48lna  = S02lna;
141600120321         ISB48pro  = S02pro;
141700120321         ISB48cts  = S02cts;
141800120321         ISB48ctr  = S02ctr;
141900120321         ISB48ncl  = S02ncl;
142000120321         ISB48pkf  = S02pkf;
142100120321         ISB48vlf  = S02vlf;
142200120321         ISB48err  = S02err;
142300120321         tnta02r (kpjba:TNSB48DS);
142400120321         IF  OSB48err <> *blanks;
142500120321           ErrMessage  = *on;
142600120321           ErrGenerico = *on;
142700120321           PosCurOPZ   = *on;
142800120321           V02msg = skMsg(08);
142900120321         ENDIF;
143000120321
143100120321       ENDSR;
143200120326
143300120326       //--------------------------------------------------------------
143400120326       //?Visto: OK dell'utente.
143500120326       //--------------------------------------------------------------
143600120326       BEGSR Call_visto;
143700120326
143800120326         wFineW01 = *off;
143900120326         W01aas = S02aas;
144000120326         W01lnp = S02lnp;
144100120326         W01nrs = S02nrs;
144200120326         W01nsp = S02nsp;
144300120326         W01tbl = S02tbl;
144400120326         W01ksc = S02ksc;
144500120326         W01rag = S02rag;
144600120326         W01imv = S02imv;
144700120326         W01err = S02err;
144800120326
144900120326         DOW  not wFineW01;
145000120326           exfmt  SB48W01;
145100120326           reset ErrMessage;
145200120326           reset ErrGenerico;
145300120326           clear W01msg;
145400120326         //?- F12=Ritorno
145500120326           IF  dsp_aid = c_F12;
145600120326             wFineW01 = *on;
145700120326           ENDIF;
145800120326         //?- F06=Conferma
145900120326           IF  dsp_aid = c_F06;
146000120326             exsr  F06W01;
146100120326             IF  not ErrGenerico;
146200120326               wFineW01 = *on;
146300120326             ENDIF;
146400120326           ENDIF;
146500120326         ENDDO;
146600120326
146700120326       ENDSR;
146800120321
146900120321       //--------------------------------------------------------------
147000120321       //?Richiamo pgm Interrogazione Clienti.
147100120321       //--------------------------------------------------------------
147200120321       BEGSR Call_XCLIR;
147300120321
147400120321         clear xCLIrds;
147500120321         clear par01;
147600120321         parrag = S02rag;
147700120321         DXCop0 = 'OPZ';
147800120321         DXCaut = wUteAbi;
147900120321         DXCcap = DUTkci;
148000120321         xclir (xCLIrds:par01:parrag);
148100120321
148200120321       ENDSR;
148300120326
148400120326       //--------------------------------------------------------------
148500120326       //?Gestione tasto funzionale F06 da videata W01.
148600120326       //?F6=Conferma.
148700120326       //--------------------------------------------------------------
148800120326       BEGSR F06W01;
148900120326
149000120326       //?Ok utente --> Aggiorna il manca tariffa da M a V
149100120329          exec sql
149200120329          UPDATE TFMTC00F
149300120329          SET MTCtrc = 'V'
149400120329          WHERE MTCaas = :W01aas and MTClnp = :W01lnp and
149500120329                MTCnrs = :W01nrs and MTCnsp = :W01nsp and
149600120329                MTCtbl = :W01tbl and MTCtrc = 'M';
149700120326
149800120326       //?OK utente --> Scrive rcd richiesta assistenza
149900140508       //?              richiama FIDNA6R
150000140508          exsr Call_FIDNA6;
150100120326
150200120326       ENDSR;
150300120326
150400120326       //--------------------------------------------------------------
150500120326       //?Richiamo pgm scrittura R.A.
150600120326       //--------------------------------------------------------------
150700140508       BEGSR Call_FIDNA6;
150800120326
150900140508         clear FIDNA6DS;
151000140508         IDA06mad = ' 30';
151100140508         IDA06tor = 'S';
151200140508         IDA06ogg = %editc(W01lnp:'X') + %editc(W01nrs:'X') +
151300140508                    %editc(W01nsp:'X') + %editc(W01aas:'X');
151400140508         IDA06no1 = 'Manca tariffa: ' + W01err;
151500140508         IDA06no2 = 'Vistato dall''utente';
151600140508         iDA06ute = DUTute;
151700140508         iDA06fil = DUTpou;
151800140508
151900140508         fidnA6r (FIDNA6DS);
152000120326
152100120326       ENDSR;
152200120223
152300080206       //--------------------------------------------------------------
152400080206       //?Operazioni finali.
152500080206       //--------------------------------------------------------------
152600080206       BEGSR RoutEnd;
152700090521
152800080206         *inLR = *on;
152900080206         return;
153000080206
153100080206       ENDSR;
153200080206
153300080206      /end-free
153400090807
153500080206       //--------------------------------------------------------------
153600080206       //?Schiere a tempo di compilazione.
153700080206       //--------------------------------------------------------------
153800080206
153900120222** - skMSG ------------------------------------------------------------------*
154000120221Utente non abilitato alla funzione                                            01
154100120222Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    02
154200120222Filiale errata o non in gestione all'utente                                   03
154300120222Codice commerciale errato o non in gestione all'utente                        04
154400120222Password errata o non immessa                                                 05
154500120222Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              06
154600120222Password scaduta                                                              07
154700120223Opzione errata                                                                08
154800120223Codice cliente errato o non in gestione all'utente                            09
154900120326Un altro utente sta vistando questo errore!!!                                 10
155000120412Richiedere solo commerciali UNIFICANTI!                                       11
