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
012600120130
012700120130      // -?Ricerca/Controllo tabelle
012800120130     d TIBS02ds      e ds                  inz
012900080206
013000120229      // -?Reperimento dati utente
013100080206     d TIBS34ds      e ds
013200120224
013300120224      // -?Gestione bolle di sede
013400120224     d TNSB01ds      e ds                  inz
013500120224
013600120224      // -?Lancio controllo manca tariffa
013700120224     d TNSB44ds      e ds                  inz
013800120314
013900120314      // -?Passaggio dati al pgm gestione tariffe
014000120314     d TNSB48ds      e ds                  inz
014100120229
014200120229      // -?Interrogazione bolle di sede
014300120229     d TNSB51ds      e ds                  inz
014400120221
014500120221       // -?Controllo abilitazioni utente - Gestione tariffe
014600120221     d TNTAA1DS      e ds                  inz
014700120130
014800120221       // -?Reperimento filiali gestitbili dall'utente
014900120221     d TRUL31ds      e ds
015000120223     d pog                    10    759    dim(250)
015100120223
015200120223       // -?Ricerca clienti
015300120223     d xCLIrds       e ds
015400120223
015500120223       // -?DS Tabella MTC - Password tariffe
015600120223     d dMTC          e ds
015700120221
015800120130       // -?DS Tabella 01 - Codice Commerciale
015900120130     d ds01          e ds
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
017200120130     d wInzD01         s               n   inz(*on)
017300120130     d wInzS02         s               n   inz(*off)
017400120306     d wTassaPrima     s               n   inz(*off)
017500080206
017600110706       // -?Campi associati al video
017700120130     d wVideo          s              2    inz('D1')
017800090601     d dsp_mod         s             10I 0
017900120127     d S02nrr          s              4  0 inz
018000110706
018100110706       // -?Campi di comodo
018200120223     d sav_Pwd         s                   like(V01pwd)
018300120306     d sav_S02nrr      s                   like(S02nrr)
018400120222     d wGiorni         s              3s 0 inz
018500120223     d wUteAbi         s                   like(OTAA1cabi)
018600120221     d w003a           s              3a   inz
018700120307     d w0070           s              7s 0 inz
018800120130
018900120130       // -?Campi di comodo data
019000120223     d wData           s              8s 0
019100120130     d wDataEUR        s               d   datfmt(*eur)
019200120130     d wDataISO        s               d   datfmt(*iso)
019300120222     d wDataScad       s              8s 0
019400120221     d wOggi           s              8s 0
019500120221
019600120221       // -?Campi da passare al pgm x la ricerca del commerciale
019700120221     d TB85kfil        s              3a
019800120221     d TB85key         s              8a
019900120221     d TB85des1        s             25a
020000120302
020100120302       // -?Campo da passare al pgm x la gestione/interrogazione bolle
020200120302     d ParTnsb         s              1a
020300120321
020400120321       // -?Campi da passare al pgm x interrogazione clienti
020500120321     d Par01           s              2a
020600120321     d Parrag          s             48a
020700120223
020800120222       // -?Indici di schiera
020900120222     d xx              s              4  0 inz
021000120222     d yy              s              4  0 inz
021100090806
021200110706       // -?Stringa SQL da eseguire
021300090806     d wSQL            s           2048    Varying        inz
021400120221
021500120221      // ----------------------------------------------------------------------
021600120221      //?Costanti per ordinamento subfile
021700120221      // ----------------------------------------------------------------------
021800120221     d MaxKey          c                   4
021900120221     d Ascendente      c                   1
022000120221     d Discendente     c                   2
022100120221     d Carattere       c                   6
022200120221     d Numerico        c                   8
022300120221     d Put             c                   1
022400120221     d EndPut          c                   2
022500120221     d Get             c                   3
022600120221      // ----------------------------------------------------------------------
022700120221      // Campi utili:
022800120221      //     nrr        - Numero relativo di record del Subfile
022900120221      //     SizeList   - Dimensione della lista
023000120221      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
023100120221      // ----------------------------------------------------------------------
023200120221     d NotUsed         s             16A
023300120221     d ReturnSize      s              9B 0
023400120221     d SizeList        s              9B 0
023500120221     d RrnLast         s              5I 0
023600120221     d WrkSort         s              1  0 inz(0)
023700120221      // ----------------------------------------------------------------------
023800120221      // Data Structures
023900120221      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
024000120221      //     QLGSCB     - The sort request block for the QLGSORT API
024100120221      //     QLGSCB00   - The sort request block for the QLGSRTIO API
024200120221      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
024300120221      //     QUSEC      - Error structure for the QLGSORT API
024400120221      // ----------------------------------------------------------------------
024500120221     d sflrcd          ds
024600120223     d  s02ksc
024700120223     d  s02ccm
024800120223     d  s02dsp
024900120223     d  s02lnp
025000120223     d  s02lna
025100120223     d  s02ctr
025200120223     d  s02tsp
025300120223     d  s02opz
025400120223     d  s02rag
025500120223     d  s02dccm
025600120224     d  s02err
025700120228     d  s02aas
025800120228     d  s02nrs
025900120228     d  s02nsp
026000120228     d  s02tbl
026100120228     d  s02pro
026200120228     d  s02cts
026300120305     d  s02ncl
026400120305     d  s02pkf
026500120305     d  s02vlf
026600120221     d  selected                      1A
026700120221
026800120221      /COPY QSYSINC/QRPGLESRC,QLGSORT
026900120221     d QLGKL                         16    DIM(MaxKey)
027000120221     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
027100120221     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
027200120221     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
027300120221     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
027400120221
027500120221      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
027600120221      /COPY QSYSINC/QRPGLESRC,QUSEC
027700090508
027800090508      //---------------------------------------------------------------
027900090807      //?Definizione procedure esterne.
028000090508      //---------------------------------------------------------------
028100120221
028200120221       // -?Api x ordinamento subfile
028300120221     d Qlgsort_pr      pr                  extpgm('QLGSORT')
028400120221     d  pr_QLGSCB                          like(Qlgscb)
028500120221     d  pr_NotUsed1                        like(NotUsed)
028600120221     d  pr_NotUsed2                        like(NotUsed)
028700120221     d  pr_SizeList                        like(SizeList)
028800120221     d  pr_ReturnSize                      like(ReturnSize)
028900120221     d  pr_QUSEC                           like(Qusec)
029000120221     d                                     options(*varsize)
029100120221
029200120221     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
029300120221     d  pr_QLGSCB00                        like(QLGSCB00)
029400120221     d  pr_SflRcd                          like(SflRcd)
029500120221     d  pr_NotUsed1                        like(NotUsed)
029600120221     d  pr_SizeList                        like(SizeList)
029700120221     d  pr_NotUsed2                        like(NotUsed)
029800120221     d  pr_QUSEC                           like(Qusec)
029900120221     d                                     options(*varsize)
030000120221
030100120221     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
030200120221     d  pr_QLGSCB00                        like(QLGSCB00)
030300120221     d  pr_NotUsed1                        like(NotUsed)
030400120221     d  pr_SflRcd                          like(SflRcd)
030500120221     d  pr_SizeList                        like(SizeList)
030600120221     d  pr_NotUsed2                        like(NotUsed)
030700120221     d  pr_QUSEC                           like(Qusec)
030800120221     d                                     options(*varsize)
030900120224
031000120224       // -?Richiamo Controllo Manca Tariffe
031100120224     d tnsb45r         pr                  extpgm('TNSB45R')
031200120224     d  kpjba                              likeds(KPJBA)
031300120229
031400120229       // -?Richiamo Interrogazione bolle di Sede
031500120229     d tnsb51c         pr                  extpgm('TNSB51C')
031600120229     d  kpjba                              likeds(KPJBA)
031700120302     d  partnsb                       1a
031800120224
031900120224       // -?Richiamo Gestione bolle di Sede
032000120224     d tnsb52r         pr                  extpgm('TNSB52R')
032100120224     d  kpjba                              likeds(KPJBA)
032200120224
032300120224       // -?Richiamo Gestione Tariffe
032400120224     d tnta01r         pr                  extpgm('TNTA01R')
032500120224     d  kpjba                              likeds(KPJBA)
032600120314     d  tnsb48ds                           likeds(TNSB48DS)
032700120321
032800120321       // -?Richiamo Interrogazione Tariffe
032900120321     d tnta02r         pr                  extpgm('TNTA02R')
033000120321     d  kpjba                              likeds(KPJBA)
033100120321     d  tnsb48ds                           likeds(TNSB48DS)
033200110506
033300120221       // -?Reperimento filiali gestitbili dall'utente
033400120221     d trul31r         pr                  extpgm('TRUL31R')
033500120221     d  kpjba                              likeds(KPJBA)
033600120221     d  trul31ds                           likeds(TRUL31DS)
033700120223
033800120223       // -?Ricerca cliente
033900120223     d xclir           pr                  extpgm('XCLIR')
034000120223     d  xclirds                            likeds(XCLIRDS)
034100120321     d  par01                         2
034200120321     d  parrag                       48
034300110706
034400110706      //---------------------------------------------------------------
034500110706      //?Definizione prototipi.
034600110706      //---------------------------------------------------------------
034700120130
034800120130       // -?Ricerc/Controllo tabelle
034900120130      /copy gaitrasrc/srcprotopr,tibs02r
035000110706
035100110706       // -?Reperimento dati utente
035200110706      /copy gaitrasrc/srcprotopr,tibs34r
035300120130
035400120130       // -?Reperimento dati anagrafici cliente codificato
035500120222      /copy gaitrasrc/srcprotopi,tibs69r
035600120222      /copy gaitrasrc/srcprotopr,tibs69r
035700120221
035800120221       // -?Controllo abilitazioni utente
035900120221      /copy gaitrasrc/srcprotopr,tntaa1c
036000120221
036100120221       // -?Ricerca commerciale
036200120221      /copy gaitrasrc/srcprotopr,trtb85r
036300090807
036400080206      //---------------------------------------------------------------
036500080206      //?Definizione key-list.
036600080206      //---------------------------------------------------------------
036700090807
036800120130       // -?File TABEL00F
036900120130     d k03tabel      e ds                  extname(TABEL00F:*key)
037000120130     d                                     prefix(k_)
037100080206
037200080206      //---------------------------------------------------------------
037300080206      //?Riepilogo indicatori.
037400080206      //---------------------------------------------------------------
037500090807
037600090807
037700080206      //---------------------------------------------------------------
037800080206
037900080206      //---------------------------------------------------------------
038000080206      //?M A I N - L I N E
038100080206      //---------------------------------------------------------------
038200080206
038300080206     c     *Entry        plist
038400080206     c                   parm                    KPJBA
038500080206
038600080206      /free
038700080206
038800090807       //?Operazioni iniziali
038900080206       exsr RoutInz;
039000080206
039100090807       //?Gestione video
039200120130       DOW wFine = *off;
039300090807
039400080206         select;
039500120130           when wVideo = 'D1';
039600110706             exsr GesD01;
039700120130           when wVideo = 'S2';
039800120130             exsr GesS02;
039900090428           other;
040000120130             wFine = *on;
040100080206         endsl;
040200090807
040300080206       ENDDO;
040400080206
040500090807       //?Operazioni finali
040600080206       exsr RoutEnd;
040700080206
040800080206       //--------------------------------------------------------------
040900080206       //?Operazioni iniziali.
041000080206       //--------------------------------------------------------------
041100080206       BEGSR RoutInz;
041200120309
041300120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
041400080206
041500120130         wVideo = 'D1';
041600120130         wInzD01 = *on;
041700090807
041800110706       //?Reperimento dati job
041900090807         exsr DatiJob;
042000120221
042100120221       //?Controllo se utente autorizzato alla funzione
042200120221         reset TNTAA1DS;
042300120221         ITAA1tipo = 'N';
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
045300120309       //?Se utente non EDP controllo la filiale, per ora abilitiamo solo 005 e 028
045400120309         IF  %subst(knmus:1:3) <> 'EDP' and
045500120309             DUTpou <> 005 and DUTpou <> 028;
045600120309           wErrGrave   = *on;
045700120309           ErrMessage  = *on;
045800120309           ErrGenerico = *on;
045900120309           V01msg = skMsg(01);
046000120309           leavesr;
046100120309         ENDIF;
046200090806
046300110706       //?Impostazione campi "fissi"
046400110706         V1Tpgm = SDSpgm;
046500120130         k_TBLkut = 1;
046600120221
046700120221       //?Data del giorno
046800120221         wOggi = %dec(%date());
046900120223
047000120223       //?Cerco la tabella gg. validità password
047100120223         clear TIBS02DS;
047200120223         clear dMTC;
047300120223         T02mod = 'C';
047400120223         T02cod = 'MTC';
047500120223         T02sif = knsif;
047600120223         T02ke1 = 'PSW';
047700120223         TNTBE_RicercaControllo (kpjba : tibs02ds);
047800120223         IF  T02err <> *blanks;
047900120223           wErrGrave   = *on;
048000120223           ErrMessage  = *on;
048100120223           ErrGenerico = *on;
048200120223           PosCurPWD   = *on;
048300120223           V01msg      = skMsg(06);
048400120223           leavesr;
048500120223         ENDIF;
048600120223         wGiorni = %dec(T02uni:3:0);
048700120222
048800120222       //?Cerco la password memorizzata per filiale (utente)
048900120222         clear TIBS02DS;
049000120222         clear dMTC;
049100120222         T02mod = 'C';
049200120222         T02cod = 'MTC';
049300120222         T02sif = knsif;
049400120223         T02ke1 = %editc(DUTpou:'X');
049500120222         TNTBE_RicercaControllo (kpjba : tibs02ds);
049600120222         IF  T02err <> *blanks;
049700120222           wErrGrave   = *on;
049800120222           ErrMessage  = *on;
049900120222           ErrGenerico = *on;
050000120222           V01msg = skMsg(02);
050100120222           leavesr;
050200120222         ENDIF;
050300120222         dMTC = T02uni;
050400120223
050500120223         clear sav_Pwd;
050600090714
050700080206       ENDSR;
050800080206
050900080206       //--------------------------------------------------------------
051000080206       //?Reperimento Dati del job (Utente/Operativi).
051100080206       //--------------------------------------------------------------
051200080206       BEGSR DatiJob;
051300080206
051400080206         in(E) §AzUte;
051500110706         IF NOT %error;
051600080206           in(E) §DatiUte;
051700110706         ENDIF;
051800110706         IF %error or RSut = *blanks;
051900080206           clear TIBS34ds;
052000080206           tibs34r(tibs34ds);
052100080206           in §AzUte;
052200080206           in §DatiUte;
052300110706         ENDIF;
052400080206
052500080206       ENDSR;
052600080206
052700090421       //--------------------------------------------------------------
052800110706       //?Gestione videata D01
052900080206       //--------------------------------------------------------------
053000110706       BEGSR GesD01;
053100080207
053200110706       //?Inizializzazione videata
053300120130         IF  wInzD01 = *on;
053400110706            exsr InzD01;
053500120130            wInzD01  = *off;
053600110706         ENDIF;
053700080207
053800110706       //?Emissione Testata
053900120221         write  SB48T01;
054000080207
054100110706       //?Emissione videata
054200120221         exfmt  SB48D01;
054300110706         reset ErrMessage;
054400110706         reset ErrGenerico;
054500120127         clear V01msg;
054600080207
054700080207         SELECT;
054800120227
054900120227         //?- Errore grave esco dal pgm
055000120227           WHEN  wErrGrave;
055100120227             wFine = *on;
055200110706
055300110706         //?- F03=Fine
055400110706           WHEN  dsp_aid = c_F03;
055500110706             exsr F03D01;
055600080207
055700090807         //?Invio
055800080207           OTHER;
055900110706         //?Eseguo i controlli
056000110706             exsr CtrD01;
056100110706             IF  ErrGenerico;
056200080207               leavesr;
056300110706             ENDIF;
056400120127         //?Se tutto OK vado nella seconda videata
056500120130           wVideo = 'S2';
056600120130           wInzS02 = *on;
056700080207
056800080207         ENDSL;
056900080207
057000080207       ENDSR;
057100080207
057200080207       //--------------------------------------------------------------
057300110706       //?Inizializzazione videata D01.
057400080207       //--------------------------------------------------------------
057500110706       BEGSR InzD01;
057600080207
057700120221         clear V01fil;
057800120302         V01dfil = '(0 = Tutte)';
057900120221         clear V01ccm;
058000120221         clear V01dccm;
058100120221         clear V01ksc;
058200120221         clear V01rag;
058300120223
058400120223         V01pwd = sav_Pwd;
058500090508
058600080207       ENDSR;
058700080207
058800091111       //--------------------------------------------------------------
058900110706       //?Gestione tasto funzionale F03 da videata D01.
059000110706       //?F3=Fine
059100091111       //--------------------------------------------------------------
059200110706       BEGSR F03D01;
059300091111
059400110706       //?Chiusura del programma
059500120130         wFine = *on;
059600091111
059700091111       ENDSR;
059800120130
059900120130       //--------------------------------------------------------------
060000120130       //?Controllo dati videata D01.
060100120130       //--------------------------------------------------------------
060200120130       BEGSR CtrD01;
060300120130
060400120130         WindDspF  = IndDspF;
060500120130         reset WindDspFb;
060600120130         IndDspF   = WindDspF;
060700120130
060800120221       //?Controllo la filiale
060900120221         IF  V01fil > 0;
061000120221           chain V01fil AZORG01L;
061100120223           IF not %found(AZORG01L);
061200120221             ErrMessage  = *on;
061300120221             ErrGenerico = *on;
061400120221             PosCurFIL   = *on;
061500120222             V01msg      = skMsg(03);
061600120221             leavesr;
061700120221           ENDIF;
061800120221         //?Verifico se l'utente è abilitato alla filiale
061900120221           w003a = %editc(V01fil:'X');
062000120221           IF  %lookup(w003a:pog) = 0;
062100120221             ErrMessage  = *on;
062200120221             ErrGenerico = *on;
062300120221             PosCurFIL   = *on;
062400120222             V01msg      = skMsg(03);
062500120221             leavesr;
062600120221           ENDIF;
062700120130         ENDIF;
062800120221
062900120221       //?Ricerca del commerciale
063000120221         IF  %scan('?' : V01ccm) > 0;
063100120221           ErrGenerico = *on;
063200120222           PosCurCCM   = *on;
063300120221           TB85kfil    = %editc(DUTpou:'X');
063400120221           clear TB85key;
063500120221           clear TB85des1;
063600120221           trtb85r (kpjba : TB85kfil : TB85key : TB85des1);
063700120221           V01ccm  = TB85key;
063800120221           V01dccm = TB85des1;
063900120221           leavesr;
064000120221         ENDIF;
064100120221
064200120221       //?Controllo il commerciale
064300120221         IF  V01ccm <> *blanks and V01ccm <> *zeros;
064400120221
064500120221         //?Solo valori numerici
064600120223           IF  %check(c_digits:V01ccm) > 0;
064700120221             ErrMessage  = *on;
064800120221             ErrGenerico = *on;
064900120222             PosCurCCM   = *on;
065000120222             V01msg      = skMsg(04);
065100120221             leavesr;
065200120221           ENDIF;
065300120221
065400120221         //?Controllo se utente abilitato al commerciale
065500120221           clear TNTAA1DS;
065600120221           ITAA1tipo = 'N';
065700120221           ITAA1caut = '§utegtc';
065800120221           ITAA1cmm  = %dec(V01ccm:7:0);
065900120221           kpjbu     = tntaa1ds;
066000120221           tntaa1c (kpjba);
066100120221           TNTAA1DS = kpjbu;
066200120221           IF  OTAA1fabi = 'N';
066300120221             ErrMessage  = *on;
066400120221             ErrGenerico = *on;
066500120222             PosCurCCM   = *on;
066600120222             V01msg      = skMsg(04);
066700120221             leavesr;
066800120221           ENDIF;
066900120221
067000120221         //?Controllo se commerciale valido
067100120221           clear  ds01;
067200120223           k_TBLcod = '01';
067300120223           k_TBLkey = V01ccm;
067400120223           chain  %kds(K03tabel) TABEL00F;
067500120221           IF  not %found(TABEL00F) or TBLflg <> *blanks;
067600120221             ErrMessage  = *on;
067700120221             ErrGenerico = *on;
067800120222             PosCurCCM   = *on;
067900120222             V01msg      = skMsg(04);
068000120221             leavesr;
068100120221           ENDIF;
068200120221
068300120221           ds01    = TBLuni;
068400120221           V01dccm = §01age;
068500120221
068600120221         //?Commerciale senza particolarità
068700120221           IF  §01par <> *blanks;
068800120221             ErrMessage  = *on;
068900120221             ErrGenerico = *on;
069000120222             PosCurCCM   = *on;
069100120222             V01msg      = skMsg(04);
069200120221             leavesr;
069300120221           ENDIF;
069400120221
069500120221         //?Commerciale "valido" data inizio e fine attività
069600120221           IF  §01dtdec > %editc(wOggi:'X') or
069700120221               §01dtfin < %editc(wOggi:'X');
069800120221             ErrMessage = *on;
069900120221             ErrGenerico = *on;
070000120222             PosCurCCM   = *on;
070100120222             V01msg      = skMsg(04);
070200120221             leavesr;
070300120221           ENDIF;
070400120221         ENDIF;
070500120223
070600120223       //?Ricerca del cliente
070700120223         IF  %scan('?' : V01ksc) > 0;
070800120223           ErrGenerico = *on;
070900120223           PosCurKSC   = *on;
071000120223           clear xCLIrds;
071100120321           clear par01;
071200120321           clear parrag;
071300120223           DXCaut = wUteAbi;
071400120223           DXCcap = DUTkci;
071500120321           xclir (xCLIrds:par01:parrag);
071600120223           ksa = DXCksc;
071700120223           dsksa = ksa(1);
071800120223           ksc(1) = dsksn;
071900120223           V01ksc = %editc(ksc(1):'X');
072000120223         ENDIF;
072100120223
072200120223       //?Controllo il cleinte
072300120223         IF  V01ksc <> *blanks and V01ksc <> *zeros;
072400120223
072500120223         //?Solo valori numerici
072600120223           IF  %check(c_digits:V01ksc) > 0;
072700120223             ErrMessage  = *on;
072800120223             ErrGenerico = *on;
072900120223             PosCurKSC   = *on;
073000120223             V01msg      = skMsg(09);
073100120223             leavesr;
073200120223           ENDIF;
073300120223
073400120223         //?Controllo se utente abilitato al cliente
073500120223           clear TNTAA1DS;
073600120223           ITAA1tipo = 'N';
073700120223           ITAA1caut = '§utegtc';
073800120223           ITAA1ksc  = %dec(V01ksc:7:0);
073900120223           kpjbu     = tntaa1ds;
074000120223           tntaa1c (kpjba);
074100120223           TNTAA1DS = kpjbu;
074200120223           IF  OTAA1fabi = 'N';
074300120223             ErrMessage  = *on;
074400120223             ErrGenerico = *on;
074500120223             PosCurKSC   = *on;
074600120223             V01msg      = skMsg(09);
074700120223             leavesr;
074800120223           ENDIF;
074900120223
075000120223         //?Controllo se cliente valido
075100120223           clear tibs69ds;
075200120223           I69kac = %dec(V01ksc:7:0);
075300120223           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
075400120223           IF  O69err <> *blanks;
075500120223             ErrMessage  = *on;
075600120223             ErrGenerico = *on;
075700120223             PosCurKSC   = *on;
075800120223             V01msg      = skMsg(09);
075900120223             leavesr;
076000120223           ENDIF;
076100120223           V01rag = ACOrag;
076200120223         ENDIF;
076300120221
076400120222       //?Password obbligatoria
076500120223         IF  V01pwd = *blanks and sav_Pwd = *blanks;
076600120222           ErrMessage = *on;
076700120222           ErrGenerico = *on;
076800120222           PosCurPWD   = *on;
076900120222           V01msg      = skMsg(05);
077000120222           leavesr;
077100120222         ENDIF;
077200120222
077300120222       //?Controllo se password OK
077400120222         IF  V01pwd <> §MTCpwd;
077500120222           ErrMessage = *on;
077600120222           ErrGenerico = *on;
077700120222           PosCurPWD   = *on;
077800120222           V01msg      = skMsg(05);
077900120222           clear V01pwd;
078000120222           leavesr;
078100120222         ENDIF;
078200120222
078300120222       //?Data immissione/modifica password
078400120223         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
078500120223         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
078600120223         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
078700120223         wDataISO = %date(wData);
078800120223
078900120223       //?Calcolo la data scadenza password
079000120223         wDataISO += %days(wgiorni);
079100120223         wDataScad = %dec(wDataISO);
079200120223
079300120223       //?Controllo se la password è scaduta
079400120222         IF wOggi > wDataScad;
079500120222           ErrMessage  = *on;
079600120222           ErrGenerico = *on;
079700120222           PosCurPWD   = *on;
079800120222           V01msg      = skMsg(07);
079900120222           leavesr;
080000120222         ENDIF;
080100120223
080200120223         sav_Pwd = V01pwd;
080300120130
080400120130       ENDSR;
080500120130
080600120130       //--------------------------------------------------------------
080700120130       //?Gestione videata S02
080800120130       //--------------------------------------------------------------
080900120130       BEGSR GesS02;
081000120306
081100120306       //?Se non l'ho ancora fatto tasso tutto
081200120306       //?(capita solo appena entro nel pgm)
081300120306         IF  not wTassaPrima;
081400120309           exsr F05S02;
081500120306           wTassaPrima = *on;
081600120306         ENDIF;
081700120130
081800120130       //?Inizializzazione videata
081900120130         IF  wInzS02 = *on;
082000120130            exsr InzS02;
082100120130            wInzS02  = *off;
082200120130         ENDIF;
082300120130
082400120130       //?Emissione Testata + Piede
082500120221         write  SB48T01;
082600120221         write  SB48P02;
082700120130
082800120130       //?Emissione subfile
082900120221         exfmt  SB48C02;
083000120130         reset ErrMessage;
083100120130         reset ErrGenerico;
083200120130         clear V02msg;
083300120130
083400120130         SELECT;
083500120130
083600120130         //?- F03=Fine
083700120130           WHEN  dsp_aid = c_F03;
083800120130             exsr F03D01;
083900120130
084000120222         //?- F05=Ritassa
084100120222           WHEN  dsp_aid = c_F05;
084200120222             exsr F05S02;
084300120222
084400120228         //?- F08=Ordina
084500120222           WHEN  dsp_aid = c_F08;
084600120229           //?Se era ordina per cliente diventa ordina per commerciale
084700120229             IF  Ord_xksc;
084800120229               Ord_xksc = *off;
084900120229               Ord_Xccm = *on;
085000120229               wF08 = 'F8=Ord.x Cliente';
085100120229           //?Se era ordina per cliente diventa ordina per commerciale
085200120229             ELSE;
085300120229               Ord_xksc = *on;
085400120229               Ord_Xccm = *off;
085500120229               wF08 = 'F8=Ord.x Commerciale';
085600120229             ENDIF;
085700120222             exsr F08S02;
085800120130
085900120130         //?- F12=Ritorno
086000120130           WHEN  dsp_aid = c_F12;
086100120130             exsr F12S02;
086200120130
086300120130         //?Invio
086400120130           OTHER;
086500120222         //?Opzioni
086600120223             IF  S02nrr > *zeros;
086700120223               exsr OpzS02 ;
086800120223             ENDIF;
086900120130             IF  ErrGenerico;
087000120130               leavesr;
087100120130             ENDIF;
087200120130
087300120130         ENDSL;
087400120130
087500120130       ENDSR;
087600120130
087700120130       //--------------------------------------------------------------
087800120130       //?Inizializzazione videata S02.
087900120130       //--------------------------------------------------------------
088000120130       BEGSR InzS02;
088100120130
088200120130       //?Pulizia subfile
088300120130         exsr PulS02;
088400120222
088500120222       //?Preparo la stringa SQL per leggere al meglio il file TFMTC
088600120222         exsr PrepSql;
088700120130
088800120130       //?Caricamento dati
088900120130         exsr CarS02;
089000120130
089100120130       //?Visualizzazione del Subfile
089200120130         SflDsp = (S02nrr <= *zeros);
089300120130
089400120130       //?Attivazione SFLEND
089500120130         SflEnd = *on;
089600120130
089700120130       //?Impaginazione Subfile
089800120130       //?Forzo sempre il primo rcd
089900120130         IF  S02nrr > *zero;
090000120130           C02rcd = 1;
090100120130         ELSE;
090200120130           clear C02rcd;
090300120130         ENDIF;
090400120130
090500120130         C02csr = C02rcd;
090600120130
090700120130       ENDSR;
090800120130
090900120130       //--------------------------------------------------------------
091000120130       //?Pulizia Subfile S02.
091100120130       //--------------------------------------------------------------
091200120130       BEGSR PulS02;
091300120130
091400120130       //?Pulizia subfile
091500120130         SflDsp    = *on;
091600120130         SflDspCtl = *on;
091700120221         write  SB48C02;
091800120130         SflDspCtl = *off;
091900120130         SflEnd    = *off;
092000120130
092100120130         clear C02rcd;
092200120130         clear C02csr;
092300120130         clear S02nrr;
092400120130         clear V02msg;
092500120130         ErrMessage  = *off;
092600120130         ErrGenerico = *off;
092700120130
092800120130         WindDspF = IndDspF;
092900120130         reset WindDspFb;
093000120130         IndDspF  = WindDspF;
093100120130
093200120130       ENDSR;
093300120222
093400120222       //--------------------------------------------------------------
093500120222       //?Preparazione stringa SQL.
093600120222       //--------------------------------------------------------------
093700120222       BEGSR PrepSql;
093800120307
093900120307         clear w0070;
094000120222
094100120223         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
094200120222
094300120307       //?Se nessuna richiesta a video
094400120307         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
094500120307            (V01ccm = *blanks or V01ccm = *zeros);
094600120222       //?Carico le bolle con filiale cliente = sk POG
094700120222           wSQL += ' and MTCpoc in(';
094800120222           yy = 0;
094900120222           xx = 1;
095000120222           FOR xx by 1 to %elem(pog);
095100120222             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
095200120222               IF yy > 0;
095300120222                 wSQL += ', ';
095400120222               ELSE;
095500120222               yy = 1;
095600120222               ENDIF;
095700120222               wSQL += '''';
095800120222               wSQL += pog(xx);
095900120222               wSQL += '''';
096000120222             ENDIF;
096100120222           ENDFOR;
096200120222           wSQL += ')';
096300120222         ELSE;
096400120307         //?Se richiesta filiale a video
096500120307           IF  V01fil > 0;
096600120307         //?Carico le bolle con filiale cliente = filiale richiesta
096700120307             wSQL += ' and MTCpoc = ' + %editc(V01fil:'X');
096800120307           ENDIF;
096900120307         //?Se richiesto commerciale a video
097000120307           IF  V01ccm <> *zeros and V01ccm <> *blanks;
097100120307              w0070 = %dec(V01ccm:7:0);
097200120307             wSQL += ' and MTCage = ' + %editc(w0070:'X');
097300120307           ENDIF;
097400120307         //?Se richiesto cliente a video
097500120307           IF  V01ksc <> *zeros and V01ksc <> *blanks;
097600120307              w0070 = %dec(V01ksc:7:0);
097700120312             wSQL += ' and MTCksc = ' + %editc(w0070:'X');
097800120307           ENDIF;
097900120222         ENDIF;
098000120309
098100120309         wSQL += ' for FETCH ONLY';
098200120222
098300120222       ENDSR;
098400120130
098500120130       //--------------------------------------------------------------
098600120130       //?Caricamento Subfile S02.
098700120130       //--------------------------------------------------------------
098800120130       BEGSR CarS02;
098900120312
099000120312         clear sav_s02nrr;
099100120130
099200120222       //?Leggo il file del Manca Tariffa solo rcd 'M'
099300120222         exec sql prepare A1 from :wSQL;
099400120222         exec sql declare MTC cursor for A1;
099500120222         exec sql open MTC;
099600120222
099700120222         IF  sqlcode < 0;
099800120222           wEoF = *on;
099900120222         ENDIF;
100000120222
100100120222         DOW  not wEoF;
100200120309           exec sql fetch next from MTC into :TFMTCds;
100300120222           IF  sqlcod = 100 or sqlcod < 0;
100400120222             wEoF = *on;
100500120222             leave;
100600120222           ENDIF;
100700120222
100800120222         //?Carico il subfile
100900120222           exsr RieS02;
101000120222         //?Scrico il subfile
101100120222           S02nrr += 1;
101200120222           write SB48S02;
101300120306           sav_S02nrr = S02nrr;
101400120229           IF  S02nrr = 9998;
101500120229             leave;
101600120229           ENDIF;
101700120222
101800120222         ENDDO;
101900120222
102000120222         exec sql close MTC;
102100120130         wEoF = *off;
102200120222
102300120222         //?Dopo aver caricato il subfile lo ordino per cliente
102400120229         wF08 = 'F8=Ord.x Commerciale';
102500120228         Ord_xksc = *on;
102600120228         Ord_Xccm = *off;
102700120228         exsr F08S02;
102800120130
102900120130       ENDSR;
103000120130
103100120130       //--------------------------------------------------------------
103200120130       //?Carico i dati nel subfile S02.
103300120130       //--------------------------------------------------------------
103400120130       BEGSR RieS02;
103500120130
103600120221         clear  SB48S02;
103700120130
103800120228         S02aas = MTCaas;
103900120228         S02nrs = MTCnrs;
104000120228         S02nsp = MTCnsp;
104100120228         S02tbl = MTCtbl;
104200120222
104300120222         S02ksc = MTCksc;
104400120305         S02dsp = %dec(%subst(%editc(MTCdsp:'X'):7:2):2:0) * 1000000;
104500120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):5:2):2:0) * 10000;
104600120305         S02dsp += %dec(%subst(%editc(MTCdsp:'X'):1:4):4:0);
104700120222         S02ctr = MTCctr;
104800120222         S02ccm = MTCage;
104900120222         S02err = MTCerr;
105000120223         S02lnp = MTClnp;
105100120223         S02lna = MTClna;
105200120223         S02tsp = MTCtsp;
105300120228         S02pro = MTCpro;
105400120228         S02cts = MTCcts;
105500120305         S02ncl = MTCncl;
105600120305         S02pkf = MTCpkf;
105700120305         S02vlf = MTCvlf;
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
106800120222         clear ds01;
106900120223         k_TBLcod = '01';
107000120223         k_TBLkey = %editc(S02ccm:'X');
107100120223         chain  %kds(K03tabel) TABEL00F;
107200120222         IF  %found(TABEL00F) and TBLflg = *blanks;
107300120222           ds01 = TBLuni;
107400120222         ENDIF;
107500120223         S02dccm = §01age;
107600120130
107700120130       ENDSR;
107800120130
107900120130       //--------------------------------------------------------------
108000120222       //?Gestione delle opzioni immesse nella videata S02.
108100120130       //--------------------------------------------------------------
108200120222       BEGSR OpzS02;
108300120130
108400120223         readc SB48S02;
108500120223
108600120223         DOW  not %eof(TNSB48D);
108700120223
108800120223           SflNxtChg = *off;
108900120223
109000120223           WindDspF = IndDspF;
109100120223           reset WindDspFb;
109200120223           IndDspF  = WindDspF;
109300120223
109400120223           C02rcd = S02nrr;
109500120223
109600120223           SELECT;
109700120223
109800120223         //?Nessuna opzione immessa
109900120223             WHEN  S02opz  = *blanks;
110000120223
110100120223         //?Precedente segnalazione di errore
110200120223             WHEN  S02opz  = 'E';
110300120223               clear S02opz;
110400120223
110500120223         //?B = Manutenzione bolla di sede
110600120223             WHEN  S02opz  = 'B';
110700120223               exsr Call_TNSB52;
110800120223
110900120223         //?T = Manutenzione tariffa
111000120223             WHEN  S02opz  = 'T';
111100120223               exsr Call_TNTA01;
111200120321
111300120321         //?3 = Interrogazione tariffa
111400120321             WHEN  S02opz  = '3';
111500120321               exsr Call_TNTA02;
111600120321
111700120321         //?4 = Interrogazione clienti
111800120321             WHEN  S02opz  = '4';
111900120321               exsr Call_XCLIR;
112000120223
112100120223         //?5 = Visualizza bolla di sede
112200120223             WHEN  S02opz  = '5';
112300120223               exsr Call_TNSB51;
112400120223
112500120223         //?Opzione non valida
112600120223             OTHER;
112700120223               ErrMessage  = *on;
112800120223               ErrGenerico = *on;
112900120223               PosCurOPZ   = *on;
113000120223               V02msg = skMsg(08);
113100120223
113200120223           ENDSL;
113300120223
113400120223         //?Aggiorno il subfile
113500120223           SELECT;
113600120223             WHEN  ErrMessage;
113700120223               SflNxtChg = *on;
113800120223               C02csr    = C02rcd;
113900120223             WHEN  ErrGenerico;
114000120223               C02csr = C02rcd;
114100120223               clear S02opz;
114200120223             OTHER;
114300120223               C02csr = C02rcd;
114400120223               clear S02opz;
114500120223           ENDSL;
114600120223
114700120223           update SB48S02;
114800120223
114900120223           IF  ErrMessage or ErrGenerico;
115000120223             leave;
115100120223           ENDIF;
115200120223
115300120223           readc SB48S02;
115400120223
115500120223         ENDDO;
115600120130
115700120223       ENDSR;
115800120222
115900120222       //--------------------------------------------------------------
116000120222       //?Gestione tasto funzionale F05 da videata S02.
116100120222       //?F05=Ritassa
116200120222       //--------------------------------------------------------------
116300120222       BEGSR F05S02;
116400120224
116500120224       //?Richiamo il TNSB45R per ogni filiale gestita dall'utente
116600120224         IF  V01fil = 0 and (V01ksc = *blanks or V01ksc = *zeros) and
116700120224            (V01ccm = *blanks or V01ccm = *zeros);
116800120224           xx = 1;
116900120224           FOR xx by 1 to %elem(pog);
117000120224             IF  pog(xx) <> *zeros and pog(xx) <> *blanks;
117100120224               clear TNSB44DS;
117200120224               D44fil = %dec(pog(xx):3:0);
117300120224               D44sb48 = 'S';
117400120224               kpjbu = TNSB44DS;
117500120224               tnsb45r (kpjba);
117600120224             ENDIF;
117700120224           ENDFOR;
117800120224
117900120224         ELSE;
118000120224
118100120224       //?Richiamo il TNSB45R con i dati delle parzializzazioni
118200120224           clear TNSB44DS;
118300120224           D44fil = V01fil;
118400120224           IF  V01ccm <> *blanks and V01ccm <> *zeros;
118500120224             D44ccm = %dec(V01ccm:7:0);
118600120224           ENDIF;
118700120224           IF  V01ksc <> *blanks and V01ksc <> *zeros;
118800120224             D44cli = %dec(V01ksc:7:0);
118900120224           ENDIF;
119000120224           D44sb48 = 'S';
119100120224           kpjbu = TNSB44DS;
119200120224           tnsb45r (kpjba);
119300120224
119400120224         ENDIF;
119500120224
119600120224       //?Dopo aver rielaborato tutto devo ricaricare il subfile
119700120224         wVideo = 'S2';
119800120224         wInzS02 = *on;
119900120222
120000120223       ENDSR;
120100120222
120200120222       //--------------------------------------------------------------
120300120222       //?Gestione tasto funzionale F08 da videata S02.
120400120228       //?F08=Ordinamento
120500120222       //--------------------------------------------------------------
120600120222       BEGSR F08S02;
120700120222
120800120306         rrnlast = sav_S02nrr;
120900120222
121000120222       //?Inizializza i campi chiave x l'ordinamento.
121100120222         clear QLGSCB;
121200120222         clear QLGSCB00;
121300120222
121400120228       //?1 campo chiave
121500120222         QLGNBRK = 1;
121600120222
121700120222       //?imposto la posizione del Cliente sul subfile e la lunghezza
121800120228         IF  Ord_xksc;
121900120228           QLGSP = 1;
122000120228           QLGSS = %SIZE(S02ksc);
122100120228         ENDIF;
122200120228         IF  Ord_xccm;
122300120228           QLGSP = 1 + %size(S02ksc);
122400120228           QLGSS = %SIZE(S02ccm);
122500120228         ENDIF;
122600120222         QLGDT = Numerico  ;
122700120222         QLGSO = Ascendente;
122800120222         QLGKL(1) = QLGSKL;
122900120222
123000120222       //?Load other sort parameters.
123100120222         QLGLB = 80 + 16 * MaxKey;
123200120222         QLGRL = %SIZE(sflrcd) - 1;
123300120222         QLGRT = 8;
123400120222         QLGOKL = 80;
123500120222         QLGLKE = 16;
123600120222         QLGLSS = 290;
123700120222
123800120222       //?Initialize Sort I/O API fields.
123900120222         QLGRL00 = QLGRL;
124000120222         QLGRC00 = 1;
124100120222         clear QUSEI;
124200120222         QUSBPRV = %SIZE(QUSEC);
124300120222
124400120222       //?First step - Initialize the sort routine.
124500120222         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
124600120222
124700120222       //?Next step - Write records to I/O routine.
124800120222         QLGRT00 = Put;
124900120222         FOR xx = 1 to rrnlast;
125000120222           chain xx SB48S02;
125100120222
125200120222       //?solo le righe con Selected = 'Y' sono riordinate,
125300120222       //?quindi per fare un ordinamento di tutte le righe
125400120222       //?metto 'Y' sempre.
125500120222           selected  = 'Y';
125600120222           clear QUSEI;
125700120222           QUSBPRV = %SIZE(QUSEC);
125800120222           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
125900120222         ENDFOR;
126000120222
126100120222       //?Next step - Signal end of input, clear subfile for reload.
126200120222         QLGRT00 = EndPut;
126300120222         clear QUSEI;
126400120222         QUSBPRV = %SIZE(QUSEC);
126500120222         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
126600120222
126700120222       //?Pulizia SFL
126800120222         exsr PulS02;
126900120222
127000120222       //?Final step - Write the records back to the subfile.
127100120222         QLGRT00 = Get;
127200120222         FOR xx = 1 to rrnlast;
127300120222           clear QUSEI;
127400120222           QUSBPRV = %SIZE(QUSEC);
127500120222           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
127600120222           S02nrr = xx;
127700120222           write SB48S02;
127800120222         ENDFOR;
127900120222
128000120222       //?Visualizzazione del SFL (se ci sono dati)
128100120223         SflDsp = (S02nrr <= *zeros);
128200120222
128300120222       //?Attivazione del SFLEND
128400120222         SflEnd = *on;
128500120222
128600120222       //?Posizionamento cursore al 1° rcd della pagina
128700120222         IF  S02nrr > *zero;
128800120222           C02rcd = 1;
128900120222         ELSE;
129000120222           clear C02rcd;
129100120222         ENDIF;
129200120222
129300120222         C02csr = C02rcd;
129400120222
129500120222       ENDSR;
129600120222
129700120222       //--------------------------------------------------------------
129800120222       //?Gestione tasto funzionale F12 da videata S02.
129900120222       //?F12=Ritorno
130000120222       //--------------------------------------------------------------
130100120222       BEGSR F12S02;
130200120222
130300120222         wVideo = 'D1';
130400120222         wInzD01 = *on;
130500120222
130600120222       ENDSR;
130700090807
130800120223       //--------------------------------------------------------------
130900120223       //?Richiamo pgm Gestione bolle di sede.
131000120223       //--------------------------------------------------------------
131100120223       BEGSR Call_TNSB52;
131200120223
131300120224         clear TNSB01DS;
131400120224         D01op0 = ' 02';
131500120228         D01aas = S02aas;
131600120224         D01lnp = S02lnp;
131700120228         D01nrs = S02nrs;
131800120228         D01nsp = S02nsp;
131900120228         D01tbl = S02tbl;
132000120224         kpjbu = TNSB01DS;
132100120302         ParTnsb = '2';
132200120302         tnsb51c (kpjba:ParTnsb);
132300120301         TNSB01DS = kpjbu;
132400120301         IF  D01err = '1';
132500120301           ErrMessage  = *on;
132600120301           ErrGenerico = *on;
132700120301           PosCurOPZ   = *on;
132800120301           V02msg = D01msg;
132900120301         ENDIF;
133000120223
133100120223       ENDSR;
133200120223
133300120223       //--------------------------------------------------------------
133400120223       //?Richiamo pgm Gestione Tariffa.
133500120223       //--------------------------------------------------------------
133600120223       BEGSR Call_TNTA01;
133700120223
133800120314         clear TNSB48DS;
133900120314         ISB48ksc  = S02ksc;
134000120314         ISB48rag  = S02rag;
134100120314         ISB48dsp  = S02dsp;
134200120314         ISB48tbl  = S02tbl;
134300120314         ISB48lnp  = S02lnp;
134400120314         ISB48lna  = S02lna;
134500120314         ISB48pro  = S02pro;
134600120314         ISB48cts  = S02cts;
134700120314         ISB48ctr  = S02ctr;
134800120314         ISB48ncl  = S02ncl;
134900120314         ISB48pkf  = S02pkf;
135000120314         ISB48vlf  = S02vlf;
135100120314         ISB48err  = S02err;
135200120314         tnta01r (kpjba:TNSB48DS);
135300120314         IF  OSB48err <> *blanks;
135400120228           ErrMessage  = *on;
135500120228           ErrGenerico = *on;
135600120228           PosCurOPZ   = *on;
135700120228           V02msg = skMsg(08);
135800120228         ENDIF;
135900120223
136000120223       ENDSR;
136100120321
136200120321       //--------------------------------------------------------------
136300120321       //?Richiamo pgm Interrogazione Tariffa.
136400120321       //--------------------------------------------------------------
136500120321       BEGSR Call_TNTA02;
136600120321
136700120321         clear TNSB48DS;
136800120321         ISB48ksc  = S02ksc;
136900120321         ISB48rag  = S02rag;
137000120321         ISB48dsp  = S02dsp;
137100120321         ISB48tbl  = S02tbl;
137200120321         ISB48lnp  = S02lnp;
137300120321         ISB48lna  = S02lna;
137400120321         ISB48pro  = S02pro;
137500120321         ISB48cts  = S02cts;
137600120321         ISB48ctr  = S02ctr;
137700120321         ISB48ncl  = S02ncl;
137800120321         ISB48pkf  = S02pkf;
137900120321         ISB48vlf  = S02vlf;
138000120321         ISB48err  = S02err;
138100120321         tnta02r (kpjba:TNSB48DS);
138200120321         IF  OSB48err <> *blanks;
138300120321           ErrMessage  = *on;
138400120321           ErrGenerico = *on;
138500120321           PosCurOPZ   = *on;
138600120321           V02msg = skMsg(08);
138700120321         ENDIF;
138800120321
138900120321       ENDSR;
139000120321
139100120321       //--------------------------------------------------------------
139200120321       //?Richiamo pgm Interrogazione Clienti.
139300120321       //--------------------------------------------------------------
139400120321       BEGSR Call_XCLIR;
139500120321
139600120321         clear xCLIrds;
139700120321         clear par01;
139800120321         parrag = S02rag;
139900120321         DXCop0 = 'OPZ';
140000120321         DXCaut = wUteAbi;
140100120321         DXCcap = DUTkci;
140200120321         xclir (xCLIrds:par01:parrag);
140300120321
140400120321       ENDSR;
140500120223
140600120223       //--------------------------------------------------------------
140700120223       //?Richiamo pgm Interrogazione bolle di sede.
140800120223       //--------------------------------------------------------------
140900120223       BEGSR Call_TNSB51;
141000120223
141100120229         clear TNSB51DS;
141200120229         I51op0 = 'B00';
141300120229         I51aas = S02aas;
141400120229         I51lnp = S02lnp;
141500120229         I51nrs = S02nrs;
141600120229         I51nsp = S02nsp;
141700120229         I51tbl = S02tbl;
141800120229         kpjbu = TNSB51DS;
141900120302         ParTnsb = '5';
142000120302         tnsb51c (kpjba:ParTnsb);
142100120301         TNSB51DS = kpjbu;
142200120301         IF  O51err = '1';
142300120301           ErrMessage  = *on;
142400120301           ErrGenerico = *on;
142500120301           PosCurOPZ   = *on;
142600120301           V02msg = O51msg;
142700120301         ENDIF;
142800120223
142900120223       ENDSR;
143000120223
143100080206       //--------------------------------------------------------------
143200080206       //?Operazioni finali.
143300080206       //--------------------------------------------------------------
143400080206       BEGSR RoutEnd;
143500090521
143600080206         *inLR = *on;
143700080206         return;
143800080206
143900080206       ENDSR;
144000080206
144100080206      /end-free
144200090807
144300080206       //--------------------------------------------------------------
144400080206       //?Schiere a tempo di compilazione.
144500080206       //--------------------------------------------------------------
144600080206
144700120222** - skMSG ------------------------------------------------------------------*
144800120221Utente non abilitato alla funzione                                            01
144900120222Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    02
145000120222Filiale errata o non in gestione all'utente                                   03
145100120222Codice commerciale errato o non in gestione all'utente                        04
145200120222Password errata o non immessa                                                 05
145300120222Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              06
145400120222Password scaduta                                                              07
145500120223Opzione errata                                                                08
145600120223Codice cliente errato o non in gestione all'utente                            09
