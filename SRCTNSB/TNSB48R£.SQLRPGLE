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
017900090601     d dsp_mod         s             10I 0
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
019200120130     d wDataEUR        s               d   datfmt(*eur)
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
023200120221     d WrkSort         s              1  0 inz(0)
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
030900120326     d fidna3r         pr                  extpgm('FIDNA3R')
031000120326     d  kpjba                              likeds(KPJBA)
031100120326     d  fidna3ds                           likeds(FIDNA3DS)
031200120224
031300120224       // -?Richiamo Controllo Manca Tariffe
031400120224     d tnsb45r         pr                  extpgm('TNSB45R')
031500120224     d  kpjba                              likeds(KPJBA)
031600120229
031700120229       // -?Richiamo Interrogazione bolle di Sede
031800120229     d tnsb51c         pr                  extpgm('TNSB51C')
031900120229     d  kpjba                              likeds(KPJBA)
032000120302     d  partnsb                       1a
032100120224
032200120224       // -?Richiamo Gestione bolle di Sede
032300120224     d tnsb52r         pr                  extpgm('TNSB52R')
032400120224     d  kpjba                              likeds(KPJBA)
032500120224
032600120224       // -?Richiamo Gestione Tariffe
032700120224     d tnta01r         pr                  extpgm('TNTA01R')
032800120224     d  kpjba                              likeds(KPJBA)
032900120314     d  tnsb48ds                           likeds(TNSB48DS)
033000120321
033100120321       // -?Richiamo Interrogazione Tariffe
033200120321     d tnta02r         pr                  extpgm('TNTA02R')
033300120321     d  kpjba                              likeds(KPJBA)
033400120321     d  tnsb48ds                           likeds(TNSB48DS)
033500110506
033600120221       // -?Reperimento filiali gestitbili dall'utente
033700120221     d trul31r         pr                  extpgm('TRUL31R')
033800120221     d  kpjba                              likeds(KPJBA)
033900120221     d  trul31ds                           likeds(TRUL31DS)
034000120223
034100120223       // -?Ricerca cliente
034200120223     d xclir           pr                  extpgm('XCLIR')
034300120223     d  xclirds                            likeds(XCLIRDS)
034400120321     d  par01                         2
034500120321     d  parrag                       48
034600110706
034700110706      //---------------------------------------------------------------
034800110706      //?Definizione prototipi.
034900110706      //---------------------------------------------------------------
035000120130
035100120130       // -?Ricerc/Controllo tabelle
035200120130      /copy gaitrasrc/srcprotopr,tibs02r
035300110706
035400110706       // -?Reperimento dati utente
035500110706      /copy gaitrasrc/srcprotopr,tibs34r
035600120130
035700120130       // -?Reperimento dati anagrafici cliente codificato
035800120222      /copy gaitrasrc/srcprotopi,tibs69r
035900120222      /copy gaitrasrc/srcprotopr,tibs69r
036000120221
036100120221       // -?Controllo abilitazioni utente
036200120221      /copy gaitrasrc/srcprotopr,tntaa1c
036300120221
036400120221       // -?Ricerca commerciale
036500130801      /copy gaitrasrc/srcprotoPI,TRMK43R_1
036600130801      /copy gaitrasrc/srcprotoPR,TRMK43R
036700090807
036800080206      //---------------------------------------------------------------
036900080206      //?Definizione key-list.
037000080206      //---------------------------------------------------------------
037100130801
037200130801       // -?File AZCMM01L
037300130801     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
037400130801     d                                     prefix(k_)
037500080206
037600080206      //---------------------------------------------------------------
037700080206      //?Riepilogo indicatori.
037800080206      //---------------------------------------------------------------
037900080206      //---------------------------------------------------------------
038000080206
038100080206      //---------------------------------------------------------------
038200080206      //?M A I N - L I N E
038300080206      //---------------------------------------------------------------
038400080206
038500080206     c     *Entry        plist
038600080206     c                   parm                    KPJBA
038700080206
038800080206      /free
038900080206
039000090807       //?Operazioni iniziali
039100080206       exsr RoutInz;
039200080206
039300090807       //?Gestione video
039400120326       DOW  wFine = *off;
039500090807
039600080206         select;
039700120130           when wVideo = 'D1';
039800110706             exsr GesD01;
039900120130           when wVideo = 'S2';
040000120130             exsr GesS02;
040100090428           other;
040200120130             wFine = *on;
040300080206         endsl;
040400090807
040500080206       ENDDO;
040600080206
040700090807       //?Operazioni finali
040800080206       exsr RoutEnd;
040900080206
041000080206       //--------------------------------------------------------------
041100080206       //?Operazioni iniziali.
041200080206       //--------------------------------------------------------------
041300080206       BEGSR RoutInz;
041400120309
041500120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
041600080206
041700120130         wVideo = 'D1';
041800120130         wInzD01 = *on;
041900090807
042000110706       //?Reperimento dati job
042100090807         exsr DatiJob;
042200120221
042300120221       //?Controllo se utente autorizzato alla funzione
042400120221         reset TNTAA1DS;
042500120221         ITAA1caut = '§utegtc';
042600120221         kpjbu     = TNTAA1DS;
042700120221         tntaa1c (kpjba);
042800120221         TNTAA1DS = kpjbu;
042900120221
043000120221         IF  OTAA1fabi = 'N';
043100120221           wErrGrave   = *on;
043200120221           ErrMessage  = *on;
043300120221           ErrGenerico = *on;
043400120221           V01msg = skMsg(01);
043500120221           leavesr;
043600120221         ENDIF;
043700120223
043800120223         wUteAbi = OTAA1cabi;
043900120221
044000120221         clear TRUL31DS;
044100120221         I31abi = OTAA1cabi;
044200120221         I31cdi = DUTdis;
044300120221         I31car = DUTare;
044400120221         I31cpo = DUTpou;
044500120221         trul31r (kpjba : trul31ds);
044600120221         IF  O31pog <= *zeros;
044700120221           wErrGrave   = *on;
044800120221           ErrMessage  = *on;
044900120221           ErrGenerico = *on;
045000120223           V01msg = skMsg(01);
045100120221           leavesr;
045200120221         ENDIF;
045300120309
045400090806
045500110706       //?Impostazione campi "fissi"
045600110706         V1Tpgm = SDSpgm;
045700120221
045800120221       //?Data del giorno
045900120221         wOggi = %dec(%date());
046000120223
046100120223       //?Cerco la tabella gg. validità password
046200120223         clear TIBS02DS;
046300120223         clear dMTC;
046400120223         T02mod = 'C';
046500120223         T02cod = 'MTC';
046600120223         T02sif = knsif;
046700120223         T02ke1 = 'PSW';
046800120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
046900120223         IF  T02err <> *blanks;
047000120223           wErrGrave   = *on;
047100120223           ErrMessage  = *on;
047200120223           ErrGenerico = *on;
047300120223           PosCurPWD   = *on;
047400120223           V01msg      = skMsg(06);
047500120223           leavesr;
047600120223         ENDIF;
047700120223         wGiorni = %dec(T02uni:3:0);
047800120222
047900120222       //?Cerco la password memorizzata per filiale (utente)
048000120222         clear TIBS02DS;
048100120222         clear dMTC;
048200120222         T02mod = 'C';
048300120222         T02cod = 'MTC';
048400120222         T02sif = knsif;
048500120223         T02ke1 = %editc(DUTpou:'X');
048600120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
048700120222         IF  T02err <> *blanks;
048800120222           wErrGrave   = *on;
048900120222           ErrMessage  = *on;
049000120222           ErrGenerico = *on;
049100120222           V01msg = skMsg(02);
049200120222           leavesr;
049300120222         ENDIF;
049400120222         dMTC = T02uni;
049500120223
049600120223         clear sav_Pwd;
049700090714
049800080206       ENDSR;
049900080206
050000080206       //--------------------------------------------------------------
050100080206       //?Reperimento Dati del job (Utente/Operativi).
050200080206       //--------------------------------------------------------------
050300080206       BEGSR DatiJob;
050400080206
050500080206         in(E) §AzUte;
050600110706         IF NOT %error;
050700080206           in(E) §DatiUte;
050800110706         ENDIF;
050900110706         IF %error or RSut = *blanks;
051000080206           clear TIBS34ds;
051100080206           tibs34r(tibs34ds);
051200080206           in §AzUte;
051300080206           in §DatiUte;
051400110706         ENDIF;
051500080206
051600080206       ENDSR;
051700080206
051800090421       //--------------------------------------------------------------
051900110706       //?Gestione videata D01
052000080206       //--------------------------------------------------------------
052100110706       BEGSR GesD01;
052200080207
052300110706       //?Inizializzazione videata
052400120130         IF  wInzD01 = *on;
052500110706            exsr InzD01;
052600120130            wInzD01  = *off;
052700110706         ENDIF;
052800080207
052900110706       //?Emissione Testata
053000120221         write  SB48T01;
053100080207
053200110706       //?Emissione videata
053300120221         exfmt  SB48D01;
053400110706         reset ErrMessage;
053500110706         reset ErrGenerico;
053600120127         clear V01msg;
053700080207
053800080207         SELECT;
053900120227
054000120227         //?- Errore grave esco dal pgm
054100120227           WHEN  wErrGrave;
054200120227             wFine = *on;
054300110706
054400110706         //?- F03=Fine
054500110706           WHEN  dsp_aid = c_F03;
054600110706             exsr F03D01;
054700080207
054800090807         //?Invio
054900080207           OTHER;
055000110706         //?Eseguo i controlli
055100110706             exsr CtrD01;
055200110706             IF  ErrGenerico;
055300080207               leavesr;
055400110706             ENDIF;
055500120127         //?Se tutto OK vado nella seconda videata
055600120130           wVideo = 'S2';
055700120130           wInzS02 = *on;
055800080207
055900080207         ENDSL;
056000080207
056100080207       ENDSR;
056200080207
056300080207       //--------------------------------------------------------------
056400110706       //?Inizializzazione videata D01.
056500080207       //--------------------------------------------------------------
056600110706       BEGSR InzD01;
056700080207
056800120221         clear V01fil;
056900120302         V01dfil = '(0 = Tutte)';
057000120221         clear V01ccm;
057100120221         clear V01dccm;
057200120221         clear V01ksc;
057300120221         clear V01rag;
057400120223
057500120223         V01pwd = sav_Pwd;
057600090508
057700080207       ENDSR;
057800080207
057900091111       //--------------------------------------------------------------
058000110706       //?Gestione tasto funzionale F03 da videata D01.
058100110706       //?F3=Fine
058200091111       //--------------------------------------------------------------
058300110706       BEGSR F03D01;
058400091111
058500110706       //?Chiusura del programma
058600120130         wFine = *on;
058700091111
058800091111       ENDSR;
058900120130
059000120130       //--------------------------------------------------------------
059100120130       //?Controllo dati videata D01.
059200120130       //--------------------------------------------------------------
059300120130       BEGSR CtrD01;
059400120130
059500120130         WindDspF  = IndDspF;
059600120130         reset WindDspFb;
059700120130         IndDspF   = WindDspF;
059800120130
059900120221       //?Controllo la filiale
060000120221         IF  V01fil > 0;
060100120221           chain V01fil AZORG01L;
060200120223           IF not %found(AZORG01L);
060300120221             ErrMessage  = *on;
060400120221             ErrGenerico = *on;
060500120221             PosCurFIL   = *on;
060600120222             V01msg      = skMsg(03);
060700120221             leavesr;
060800120221           ENDIF;
060900120221         //?Verifico se l'utente è abilitato alla filiale
061000120221           w003a = %editc(V01fil:'X');
061100120221           IF  %lookup(w003a:pog) = 0;
061200120221             ErrMessage  = *on;
061300120221             ErrGenerico = *on;
061400120221             PosCurFIL   = *on;
061500120222             V01msg      = skMsg(03);
061600120221             leavesr;
061700120221           ENDIF;
061800120130         ENDIF;
061900120221
062000130801       //?Ricerca del commerciale unificante
062100120221         IF  %scan('?' : V01ccm) > 0;
062200120221           ErrGenerico = *on;
062300120222           PosCurCCM   = *on;
062400130801           clear  TRMK43ds;
062500130801           iMK43solU = 'S';
062600130801           iMK43fil  = DUTpou;
062700130801           TRMK43R (kpjba : TRMK43ds);
062800130801           if  oMK43err = *off  and  oMK43fxx = *blank;
062900130801             V01ccm  = %editc( oMK43cmm : 'X' );
063000130801             V01dccm = oMK43des;
063100130801           endif;
063200120221           leavesr;
063300120221         ENDIF;
063400120221
063500120221       //?Controllo il commerciale
063600120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
063700120221
063800120221         //?Solo valori numerici
063900120223           IF  %check(c_digits:V01ccm) > 0;
064000120221             ErrMessage  = *on;
064100120221             ErrGenerico = *on;
064200120222             PosCurCCM   = *on;
064300120222             V01msg      = skMsg(04);
064400120221             leavesr;
064500120221           ENDIF;
064600120221
064700120221         //?Controllo se utente abilitato al commerciale
064800120221           clear TNTAA1DS;
064900120221           ITAA1caut = '§utegtc';
065000120221           ITAA1cmm  = %dec(V01ccm:7:0);
065100120221           kpjbu     = tntaa1ds;
065200120221           tntaa1c (kpjba);
065300120221           TNTAA1DS = kpjbu;
065400120221           IF  OTAA1fabi = 'N';
065500120221             ErrMessage  = *on;
065600120221             ErrGenerico = *on;
065700120222             PosCurCCM   = *on;
065800120222             V01msg      = skMsg(04);
065900120221             leavesr;
066000120221           ENDIF;
066100120221
066200120221         //?Controllo se commerciale valido
066300130801           k_CMMcod = %int(V01ccm);
066400130801           chain  %kds(k_azcmm01) AZCMM000;
066500130801           IF  not %found(AZCMM01L);
066600120221             ErrMessage  = *on;
066700120221             ErrGenerico = *on;
066800120222             PosCurCCM   = *on;
066900120222             V01msg      = skMsg(04);
067000120221             leavesr;
067100120221           ENDIF;
067200120221
067300130801           V01dccm = CMMdes;
067400120221
067500120221         //?Commerciale senza particolarità
067600130801           IF  CMMpar <> *blanks;
067700120221             ErrMessage  = *on;
067800120221             ErrGenerico = *on;
067900120222             PosCurCCM   = *on;
068000120222             V01msg      = skMsg(04);
068100120221             leavesr;
068200120221           ENDIF;
068300120221
068400120221         //?Commerciale "valido" data inizio e fine attività
068500130801           IF  CMMdtIni > wOggi  or
068600130801               CMMdtFin < wOggi;
068700120221             ErrMessage = *on;
068800120221             ErrGenerico = *on;
068900120222             PosCurCCM   = *on;
069000120222             V01msg      = skMsg(04);
069100120221             leavesr;
069200120221           ENDIF;
069300120412
069400120412         //?Deve essere un commerciale Unificante
069500130801           IF  %dec(V01ccm:7:0) <> CMMuni;
069600120412             ErrMessage = *on;
069700120412             ErrGenerico = *on;
069800120412             PosCurCCM   = *on;
069900120412             V01msg      = skMsg(11);
070000120412             leavesr;
070100120412           ENDIF;
070200120221         ENDIF;
070300120223
070400120223       //?Ricerca del cliente
070500120223         IF  %scan('?' : V01ksc) > 0;
070600120223           ErrGenerico = *on;
070700120223           PosCurKSC   = *on;
070800120223           clear xCLIrds;
070900120321           clear par01;
071000120321           clear parrag;
071100120223           DXCaut = wUteAbi;
071200120223           DXCcap = DUTkci;
071300120321           xclir (xCLIrds:par01:parrag);
071400120223           ksa = DXCksc;
071500120223           dsksa = ksa(1);
071600120223           ksc(1) = dsksn;
071700120223           V01ksc = %editc(ksc(1):'X');
071800120223         ENDIF;
071900120223
072000130801       //?Controllo il cliente
072100120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
072200120223
072300120223         //?Solo valori numerici
072400120223           IF  %check(c_digits:V01ksc) > 0;
072500120223             ErrMessage  = *on;
072600120223             ErrGenerico = *on;
072700120223             PosCurKSC   = *on;
072800120223             V01msg      = skMsg(09);
072900120223             leavesr;
073000120223           ENDIF;
073100120223
073200120223         //?Controllo se utente abilitato al cliente
073300120223           clear TNTAA1DS;
073400120223           ITAA1caut = '§utegtc';
073500120223           ITAA1ksc  = %dec(V01ksc:7:0);
073600120223           kpjbu     = tntaa1ds;
073700120223           tntaa1c (kpjba);
073800120223           TNTAA1DS = kpjbu;
073900120223           IF  OTAA1fabi = 'N';
074000120223             ErrMessage  = *on;
074100120223             ErrGenerico = *on;
074200120223             PosCurKSC   = *on;
074300120223             V01msg      = skMsg(09);
074400120223             leavesr;
074500120223           ENDIF;
074600120223
074700120223         //?Controllo se cliente valido
074800120223           clear tibs69ds;
074900120223           I69kac = %dec(V01ksc:7:0);
075000120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
075100120223           IF  O69err <> *blanks;
075200120223             ErrMessage  = *on;
075300120223             ErrGenerico = *on;
075400120223             PosCurKSC   = *on;
075500120223             V01msg      = skMsg(09);
075600120223             leavesr;
075700120223           ENDIF;
075800120223           V01rag = ACOrag;
075900120223         ENDIF;
076000120221
076100120222       //?Password obbligatoria
076200120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
076300120222           ErrMessage = *on;
076400120222           ErrGenerico = *on;
076500120222           PosCurPWD   = *on;
076600120222           V01msg      = skMsg(05);
076700120222           leavesr;
076800120222         ENDIF;
076900120222
077000120222       //?Controllo se password OK
077100120222         IF  V01pwd <> §MTCpwd;
077200120222           ErrMessage = *on;
077300120222           ErrGenerico = *on;
077400120222           PosCurPWD   = *on;
077500120222           V01msg      = skMsg(05);
077600120222           clear V01pwd;
077700120222           leavesr;
077800120222         ENDIF;
077900120222
078000120222       //?Data immissione/modifica password
078100120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
078200120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
078300120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
078400120223         wDataISO = %date(wData);
078500120223
078600120223       //?Calcolo la data scadenza password
078700120223         wDataISO += %days(wgiorni);
078800120223         wDataScad = %dec(wDataISO);
078900120223
079000120223       //?Controllo se la password è scaduta
079100120222         IF wOggi > wDataScad;
079200120222           ErrMessage  = *on;
079300120222           ErrGenerico = *on;
079400120222           PosCurPWD   = *on;
079500120222           V01msg      = skMsg(07);
079600120222           leavesr;
079700120222         ENDIF;
079800120223
079900120223         sav_Pwd = V01pwd;
080000120130
080100120130       ENDSR;
080200120130
080300120130       //--------------------------------------------------------------
080400120130       //?Gestione videata S02
080500120130       //--------------------------------------------------------------
080600120130       BEGSR GesS02;
080700120306
080800120306       //?Se non l'ho ancora fatto tasso tutto
080900120306       //?(capita solo appena entro nel pgm)
081000120306         IF  not wTassaPrima;
081100120309           exsr F05S02;
081200120306           wTassaPrima = *on;
081300120306         ENDIF;
081400120130
081500120130       //?Inizializzazione videata
081600120130         IF  wInzS02 = *on;
081700120130            exsr InzS02;
081800120130            wInzS02  = *off;
081900120130         ENDIF;
082000120130
082100120130       //?Emissione Testata + Piede
082200120221         write  SB48T01;
082300120221         write  SB48P02;
082400120130
082500120130       //?Emissione subfile
082600120221         exfmt  SB48C02;
082700120130         reset ErrMessage;
082800120130         reset ErrGenerico;
082900120130         clear V02msg;
083000120130
083100120130         SELECT;
083200120130
083300120130         //?- F03=Fine
083400120130           WHEN  dsp_aid = c_F03;
083500120130             exsr F03D01;
083600120130
083700120222         //?- F05=Ritassa
083800120222           WHEN  dsp_aid = c_F05;
083900120222             exsr F05S02;
084000120222
084100120228         //?- F08=Ordina
084200120222           WHEN  dsp_aid = c_F08;
084300120229           //?Se era ordina per cliente diventa ordina per commerciale
084400120229             IF  Ord_xksc;
084500120229               Ord_xksc = *off;
084600120229               Ord_Xccm = *on;
084700120229               wF08 = 'F8=Ord.x Cliente';
084800120229           //?Se era ordina per cliente diventa ordina per commerciale
084900120229             ELSE;
085000120229               Ord_xksc = *on;
085100120229               Ord_Xccm = *off;
085200120229               wF08 = 'F8=Ord.x Commerciale';
085300120229             ENDIF;
085400120222             exsr F08S02;
085500120130
085600120130         //?- F12=Ritorno
085700120130           WHEN  dsp_aid = c_F12;
085800120130             exsr F12S02;
085900120130
086000120130         //?Invio
086100120130           OTHER;
086200120222         //?Opzioni
086300120223             IF  S02nrr > *zeros;
086400120223               exsr OpzS02 ;
086500120223             ENDIF;
086600120130             IF  ErrGenerico;
086700120130               leavesr;
086800120130             ENDIF;
086900120130
087000120130         ENDSL;
087100120130
087200120130       ENDSR;
087300120130
087400120130       //--------------------------------------------------------------
087500120130       //?Inizializzazione videata S02.
087600120130       //--------------------------------------------------------------
087700120130       BEGSR InzS02;
087800120130
087900120130       //?Pulizia subfile
088000120130         exsr PulS02;
088100120222
088200120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
088300120222         exsr PrepSql;
088400120130
088500120130       //?Caricamento dati
088600120130         exsr CarS02;
088700120130
088800120130       //?Visualizzazione del Subfile
088900120130         SflDsp = (S02nrr <= *zeros);
089000120130
089100120130       //?Attivazione SFLEND
089200120130         SflEnd = *on;
089300120130
089400120130       //?Impaginazione Subfile
089500120130       //?Forzo sempre il primo rcd
089600120130         IF  S02nrr > *zero;
089700120130           C02rcd = 1;
089800120130         ELSE;
089900120130           clear C02rcd;
090000120130         ENDIF;
090100120130
090200120130         C02csr = C02rcd;
090300120130
090400120130       ENDSR;
090500120130
090600120130       //--------------------------------------------------------------
090700120130       //?Pulizia Subfile S02.
090800120130       //--------------------------------------------------------------
090900120130       BEGSR PulS02;
091000120130
091100120130       //?Pulizia subfile
091200120130         SflDsp    = *on;
091300120130         SflDspCtl = *on;
091400120221         write  SB48C02;
091500120130         SflDspCtl = *off;
091600120130         SflEnd    = *off;
091700120130
091800120130         clear C02rcd;
091900120130         clear C02csr;
092000120130         clear S02nrr;
092100120130         clear V02msg;
092200120130         ErrMessage  = *off;
092300120130         ErrGenerico = *off;
092400120130
092500120130         WindDspF = IndDspF;
092600120130         reset WindDspFb;
092700120130         IndDspF  = WindDspF;
092800120130
092900120130       ENDSR;
093000120222
093100120222       //--------------------------------------------------------------
093200120222       //?Preparazione stringa SQL.
093300120222       //--------------------------------------------------------------
093400120222       BEGSR PrepSql;
093500120307
093600120307         clear w0070;
093700120222
093800120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
093900120222
094000120307       //?Se nessuna richiesta a video
094100120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
094200120307            (V01ccm = *blanks or V01ccm = *zeros);
094300120222       //?Carico le bolle con filiale cliente = sk POG
094400120222           wSQL += ' and MTCpoc in(';
094500120222           yy = 0;
094600120222           xx = 1;
094700120222           FOR xx by 1 to %elem(pog);
094800120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
094900120222               IF yy > 0;
095000120222                 wSQL += ', ';
095100120222               ELSE;
095200120222               yy = 1;
095300120222               ENDIF;
095400120222               wSQL += '''';
095500120222               wSQL += pog(xx);
095600120222               wSQL += '''';
095700120222             ENDIF;
095800120222           ENDFOR;
095900120222           wSQL += ')';
096000120222         ELSE;
096100120307         //?Se richiesta filiale a video
096200120307           IF  V01fil > 0;
096300120307         //?Carico le bolle con filiale cliente = filiale richiesta
096400120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
096500120307           ENDIF;
096600120307         //?Se richiesto commerciale a video
096700120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
096800120307              w0070 = %dec(V01ccm:7:0);
096900120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
097000120307           ENDIF;
097100120307         //?Se richiesto cliente a video
097200120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
097300120307              w0070 = %dec(V01ksc:7:0);
097400120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
097500120307           ENDIF;
097600120222         ENDIF;
097700120309
097800120309         wSQL += ' for FETCH ONLY';
097900120222
098000120222       ENDSR;
098100120130
098200120130       //--------------------------------------------------------------
098300120130       //?Caricamento Subfile S02.
098400120130       //--------------------------------------------------------------
098500120130       BEGSR CarS02;
098600120312
098700120312         clear sav_s02nrr;
098800120130
098900120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
099000120222         exec sql prepare A1 from :wSQL;
099100120222         exec sql declare MTC cursor for A1;
099200120222         exec sql open MTC;
099300120222
099400120222         IF  sqlcode < 0;
099500120222           wEoF = *on;
099600120222         ENDIF;
099700120222
099800120222         DOW  not wEoF;
099900120309           exec sql fetch next from MTC into :TFMTCds;
100000120222           IF  sqlcod = 100 or sqlcod < 0;
100100120222             wEoF = *on;
100200120222             leave;
100300120222           ENDIF;
100400120222
100500120222         //?Carico il subfile
100600120222           exsr RieS02;
100700120222         //?Scrico il subfile
100800120222           S02nrr += 1;
100900120222           write SB48S02;
101000120306           sav_S02nrr = S02nrr;
101100120229           IF  S02nrr = 9998;
101200120229             leave;
101300120229           ENDIF;
101400120222
101500120222         ENDDO;
101600120222
101700120222         exec sql close MTC;
101800120130         wEoF = *off;
101900120222
102000120222         //?Dopo aver caricato il subfile lo ordino per cliente
102100120229         wF08 = 'F8=Ord.x Commerciale';
102200120228         Ord_xksc = *on;
102300120228         Ord_Xccm = *off;
102400120228         exsr F08S02;
102500120130
102600120130       ENDSR;
102700120130
102800120130       //--------------------------------------------------------------
102900120130       //?Carico i dati nel subfile S02.
103000120130       //--------------------------------------------------------------
103100120130       BEGSR RieS02;
103200120130
103300120221         clear  SB48S02;
103400120130
103500120228         S02aas = MTCaas;
103600120228         S02nrs = MTCnrs;
103700120228         S02nsp = MTCnsp;
103800120228         S02tbl = MTCtbl;
103900120222
104000120222         S02ksc = MTCksc;
104100120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
104200120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
104300120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
104400120222         S02ctr = MTCctr;
104500120222         S02ccm = MTCage;
104600120222         S02err = MTCerr;
104700120223         S02lnp = MTClnp;
104800120223         S02lna = MTClna;
104900120223         S02tsp = MTCtsp;
105000120228         S02pro = MTCpro;
105100120228         S02cts = MTCcts;
105200120305         S02ncl = MTCncl;
105300120305         S02pkf = MTCpkf;
105400120305         S02vlf = MTCvlf;
105500120326
105600120326         S02imp = MTCimp;
105700120326         S02imv = MTCimv;
105800120130
105900120222       //?Decodifico il cliente
106000120222         clear tibs69ds;
106100120222         I69kac = S02ksc;
106200120222         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
106300120222         IF  O69err = *blanks;
106400120222           S02rag = ACOrag;
106500120222         ENDIF;
106600120222
106700120222       //?Decodifico il commerciale
106800130801         clear  S02dccm;
106900130801         k_CMMcod = S02ccm;
107000130801         chain  %kds(k_azcmm01) AZCMM000;
107100130801         IF  %found(AZCMM01L);
107200130801           S02dccm = CMMdes;
107300130801         ENDIF;
107400120130
107500120130       ENDSR;
107600120130
107700120130       //--------------------------------------------------------------
107800120222       //?Gestione delle opzioni immesse nella videata S02.
107900120130       //--------------------------------------------------------------
108000120222       BEGSR OpzS02;
108100120130
108200120223         readc SB48S02;
108300120223
108400120223         DOW  not %eof(TNSB48D);
108500120223
108600120223           SflNxtChg = *off;
108700120223
108800120223           WindDspF = IndDspF;
108900120223           reset WindDspFb;
109000120223           IndDspF  = WindDspF;
109100120223
109200120223           C02rcd = S02nrr;
109300120223
109400120223           SELECT;
109500120223
109600120223         //?Nessuna opzione immessa
109700120223             WHEN  S02opz  = *blanks;
109800120223
109900120223         //?Precedente segnalazione di errore
110000120223             WHEN  S02opz  = 'E';
110100120223               clear S02opz;
110200120223
110300120223         //?B = Manutenzione bolla di sede
110400120223             WHEN  S02opz  = 'B';
110500120223               exsr Call_TNSB52;
110600120223
110700120223         //?T = Manutenzione tariffa
110800120223             WHEN  S02opz  = 'T';
110900120223               exsr Call_TNTA01;
111000120326
111100120326         //?V = OK utente
111200120326             WHEN  S02opz  = 'V' and S02imp = 'IMP';
111300120326               exsr Call_visto;
111400120321
111500120321         //?3 = Interrogazione tariffa
111600120321             WHEN  S02opz  = '3';
111700120321               exsr Call_TNTA02;
111800120321
111900120321         //?4 = Interrogazione clienti
112000120321             WHEN  S02opz  = '4';
112100120321               exsr Call_XCLIR;
112200120223
112300120223         //?5 = Visualizza bolla di sede
112400120223             WHEN  S02opz  = '5';
112500120223               exsr Call_TNSB51;
112600120223
112700120223         //?Opzione non valida
112800120223             OTHER;
112900120223               ErrMessage  = *on;
113000120223               ErrGenerico = *on;
113100120223               PosCurOPZ   = *on;
113200120223               V02msg = skMsg(08);
113300120223
113400120223           ENDSL;
113500120223
113600120223         //?Aggiorno il subfile
113700120223           SELECT;
113800120223             WHEN  ErrMessage;
113900120223               SflNxtChg = *on;
114000120223               C02csr    = C02rcd;
114100120223             WHEN  ErrGenerico;
114200120223               C02csr = C02rcd;
114300120223               clear S02opz;
114400120223             OTHER;
114500120223               C02csr = C02rcd;
114600120223               clear S02opz;
114700120223           ENDSL;
114800120223
114900120223           update SB48S02;
115000120223
115100120223           IF  ErrMessage or ErrGenerico;
115200120223             leave;
115300120223           ENDIF;
115400120223
115500120223           readc SB48S02;
115600120223
115700120223         ENDDO;
115800120130
115900120223       ENDSR;
116000120222
116100120222       //--------------------------------------------------------------
116200120222       //?Gestione tasto funzionale F05 da videata S02.
116300120222       //?F05=Ritassa
116400120222       //--------------------------------------------------------------
116500120222       BEGSR F05S02;
116600120224
116700120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
116800120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
116900120224            (V01ccm = *blanks or V01ccm = *zeros);
117000120224           xx = 1;
117100120224           FOR xx by 1 to %elem(pog);
117200120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
117300120224               clear TNSB44DS;
117400120224               D44fil = %dec(pog(xx):3:0);
117500120224               D44sb48 = 'S';
117600120224               kpjbu = TNSB44DS;
117700120224               tnsb45r (kpjba);
117800120224             ENDIF;
117900120224           ENDFOR;
118000120224
118100120224         ELSE;
118200120224
118300120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
118400120224           clear TNSB44DS;
118500120224           D44fil = V01fil;
118600120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
118700120224             D44ccm = %dec(V01ccm:7:0);
118800120224           ENDIF;
118900120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
119000120224             D44cli = %dec(V01ksc:7:0);
119100120224           ENDIF;
119200120224           D44sb48 = 'S';
119300120224           kpjbu = TNSB44DS;
119400120224           tnsb45r (kpjba);
119500120224
119600120224         ENDIF;
119700120224
119800120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
119900120224         wVideo = 'S2';
120000120224         wInzS02 = *on;
120100120222
120200120223       ENDSR;
120300120222
120400120222       //--------------------------------------------------------------
120500120222       //?Gestione tasto funzionale F08 da videata S02.
120600120228       //?F08=Ordinamento
120700120222       //--------------------------------------------------------------
120800120222       BEGSR F08S02;
120900120222
121000120306         rrnlast = sav_S02nrr;
121100120222
121200120222       //?Inizializza i campi chiave x l'ordinamento.
121300120222         clear QLGSCB;
121400120222         clear QLGSCB00;
121500120222
121600120418       //?2 campi chiave
121700120418         QLGNBRK = 2;
121800120222
121900120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
122000120228         IF  Ord_xksc;
122100120228           QLGSP = 1;
122200120228           QLGSS = %SIZE(S02ksc);
122300120228         ENDIF;
122400120418       //?imposto la posizione del Commerciale sul subfile e la lunghezza
122500120228         IF  Ord_xccm;
122600120228           QLGSP = 1 + %size(S02ksc);
122700120228           QLGSS = %SIZE(S02ccm);
122800120228         ENDIF;
122900120418
123000120222         QLGDT = Numerico  ;
123100120222         QLGSO = Ascendente;
123200120222         QLGKL(1) = QLGSKL;
123300120418
123400120418       //?imposto la posizione della Data sul subfile e la lunghezza
123500120418         IF  Ord_xksc;
123600120418           QLGSP = 1 + %size(S02ksc) + %size(S02ccm);
123700120418           QLGSS = %SIZE(S02dsp);
123800120418         ENDIF;
123900120418       //?imposto la posizione del Cliente sul subfile e la lunghezza
124000120418         IF  Ord_xccm;
124100120418           QLGSP = 1;
124200120418           QLGSS = %SIZE(S02ksc);
124300120418         ENDIF;
124400120418         QLGDT = Numerico  ;
124500120418         QLGSO = Ascendente;
124600120418         QLGKL(2) = QLGSKL;
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
154800120412Richiedere solo commerciali UNIFICANTI!                                       11
