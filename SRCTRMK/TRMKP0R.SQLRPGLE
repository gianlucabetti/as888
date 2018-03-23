000100170405      //--------------------------------------------------------------
000200170405      //?TRMKP0R - Manutenzione Straordinaria Tariffe
000300170405      //--------------------------------------------------------------
000400170405     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170405     h dftactgrp(*no) actgrp(*caller)
000600170405
000700170405      //---------------------------------------------------------------
000800170405      //?Dichiarazione file.
000900170405      //---------------------------------------------------------------
001000170405      // - Organigramma
001100170405     fAZORG01L  if   e           k disk
001200170405
001300170405      // - Anagrafica Commerciali
001400170405     fAZCMM01L  if   e           k disk
001500170406
001600170406      // - Anagrafica Clienti
001700170406     fCNACO00F  if   e           k disk
001800170406     fCNCLP01L  if   e           k disk
001900170405
002000170405      // - Tabelle
002100170405     fTABEL00F  if   e           k disk
002200170406
002300170406      // - Info Commerciali
002400170406     fTICPI01L  if   e           k disk
002500170405
002600170405      // - File Tariffe
002700170406     fTNTAM04L  uf   e           k disk
002800170405
002900170405      // - Video
003000170405     fTRMKP0D   cf   e             workstn
003100170405     f                                     sfile(MKP0S03 : S03nrr)
003200170405     f                                     indds(IndDspF)
003300170405     f                                     infds(InfDspF)
003400170405
003500170405      //---------------------------------------------------------------
003600170405      //?Definizione costanti.
003700170405      //---------------------------------------------------------------
003800170405      // - Tasti funzionali a video
003900170405     d c_F01           c                   const(x'31')
004000170405     d c_F02           c                   const(x'32')
004100170405     d c_F03           c                   const(x'33')
004200170405     d c_F04           c                   const(x'34')
004300170405     d c_F05           c                   const(x'35')
004400170405     d c_F06           c                   const(x'36')
004500170405     d c_F07           c                   const(x'37')
004600170405     d c_F08           c                   const(x'38')
004700170405     d c_F09           c                   const(x'39')
004800170405     d c_F10           c                   const(x'3A')
004900170405     d c_F11           c                   const(x'3B')
005000170405     d c_F12           c                   const(x'3C')
005100170405     d c_F13           c                   const(x'B1')
005200170405     d c_F14           c                   const(x'B2')
005300170405     d c_F15           c                   const(x'B3')
005400170405     d c_F16           c                   const(x'B4')
005500170405     d c_F17           c                   const(x'B5')
005600170405     d c_F18           c                   const(x'B6')
005700170405     d c_F19           c                   const(x'B7')
005800170405     d c_F20           c                   const(x'B8')
005900170405     d c_F21           c                   const(x'B9')
006000170405     d c_F22           c                   const(x'BA')
006100170405     d c_F23           c                   const(x'BB')
006200170405     d c_F24           c                   const(x'BC')
006300170405     d c_Enter         c                   const(x'F1')
006400170405     d c_RollDown      c                   const(x'F4')
006500170405     d c_RollUp        c                   const(x'F5')
006600170405
006700170405     d Digits          c                   const('0123456789')
006800170405
006900170405      //---------------------------------------------------------------
007000170405      //?Definizione schiere.
007100170405      //---------------------------------------------------------------
007200170405      // - Sk Commerciali
007300170405     d skAge           s              7  0 dim(3500)
007400170405
007500170405      // - Sk Cod.Importanza clienti da tabella IC
007600170405     d skIC            s              1    dim(10)
007700170405     d skIC_ord        s              1  0 dim(10)
007800170405
007900170405      // - Sk Cod.Importanza clienti immessi come selezione
008000170405     d skCLV           s              1    dim(5)
008100170405
008200170405      // - Messaggi di errore
008300170405     d Msg             s             78    dim(30) ctdata perrcd(1)
008400170405
008500170405      //---------------------------------------------------------------
008600170405      //?Definizione aree dati.
008700170405      //---------------------------------------------------------------
008800170405      // - Dati utente
008900170405     d §AzUte        e ds                  extname(AZUTE00F)
009000170405     d                                     dtaara
009100170405     d §DatiUte      e ds                  extname(dDatiUte)
009200170405     d                                     dtaara
009300170405
009400170405      //---------------------------------------------------------------
009500170405      //?Definizione strutture dati.
009600170405      //---------------------------------------------------------------
009700170405      // - Status
009800170405     d Psds           sds
009900170405     d   SDSpgm          *proc
010000170405
010100170405      // - InfDS
010200170405     d InfDspF         ds
010300170405     d  dsp_aid              369    369a
010400170405     d  sfl_rrn              376    377i 0
010500170405     d  min_nrr              378    379i 0
010600170405     d  num_rcds             380    381i 0
010700170405
010800170405      // - Indicatori su DspF
010900170405     d IndDspF         ds
011000170405        // - Indicatori di errore in videata
011100170405     d  ErrMessage                    1n   overlay(IndDspF : 28)
011200170405        // - Indicatori di gestione del subfile
011300170405     d  SflDsp                        1n   overlay(IndDspF : 30)
011400170405     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011500170405     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011600170405     d  SflEnd                        1n   overlay(IndDspF : 33)
011700170405        // - Indicatori di errore
011800170405     d  PosCurFIL                     1n   overlay(IndDspF : 50)
011900170405     d  PosCurCMM                     1n   overlay(IndDspF : 51)
012000170405     d  PosCurKSC                     1n   overlay(IndDspF : 52)
012100170405     d  PosCurKSU                     1n   overlay(IndDspF : 53)
012200170405     d  PosCurCLV1                    1n   overlay(IndDspF : 54)
012300170405     d  PosCurCLV2                    1n   overlay(IndDspF : 55)
012400170405     d  PosCurCLV3                    1n   overlay(IndDspF : 56)
012500170405     d  PosCurCLV4                    1n   overlay(IndDspF : 57)
012600170405     d  PosCurCLV5                    1n   overlay(IndDspF : 58)
012700170405     d  PosCurPWD                     1n   overlay(IndDspF : 59)
012800170405     d  PosCurOPZ                     1n   overlay(IndDspF : 60)
012900170410     d  PosCurPERC                    1n   overlay(IndDspF : 61)
013000170405        // - Indicatori di errore generico
013100170405     d  ErrGenerico                   1n   overlay(IndDspF : 99)
013200170406
013300170406      // - Parametri per Interrogazione tariffe
013400170406     d TA02R2ds        ds                  inz
013500170406     d TA02ksc                 2      8  0
013600170406     d TA02prg                 9     11  0
013700170406     d TA02ccc                12     15  0
013800170406     d TA02rgs                16     63
013900170406     d TA02ctr                64     66  0
014000170405
014100170405      // - Indicatore di errore dal 50 al 99
014200170405     d WindDspF        ds                  inz
014300170405     d  WindDspFa              1     49    inz(*zero)
014400170405     d  WindDspFb             50     99    inz(*zero)
014500170405
014600170405      // - Parametri ricevuti
014700170405     d KPJBA         e ds
014800170405
014900170405      // - Ricerca/Controllo tabelle
015000170405     d TIBS02DS      e ds                  inz
015100170405
015200170405      // - Ricerca Unificante Padre?
015300170405     d TIBS10DS      e ds                  inz
015400170405     d  sk_D10ksc             21   5520  0 inz  dim(500)
015500170405
015600170405      // - Reperimento dati utente
015700170405     d TIBS34DS      e ds
015800170405
015900170405      // - Reperimento dati Anagrafica Clienti
016000170405      /copy gaitrasrc/srcprotopi,TIBS69R
016100170406
016200170406      // - Statistica cliente
016300170406     d TISE60DS      e ds
016400170406     d  skKSA                 50    161    dim(28)
016500170406
016600170406      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
016700170406      // - devo definirla se non va in errore per via dei prototipi
016800170406     d TNSB48DS      e ds
016900170406
017000170406      // - Controllo abilitazioni
017100170406     d TNTAA1DS      e ds                  inz
017200170406
017300170406      // - Ricerca Anagrafica Clienti
017400170406     d TNTAI1DS      e ds                  inz
017500170406
017600170406      // - Int. Anagrafica Clienti
017700170406     d TNTAI2DS      e ds                  inz
017800170405
017900170405      // - Reperimento filiali gestite dall'utente
018000170405     d TRUL31DS      e ds
018100170405     d  POG                   10    759    DIM(250)
018200170405
018300170405      // - Tabella IC - Importanza Clienti
018400170405     d dsIC          e ds
018500170405
018600170405      // - DS Tabella MTC - Password tariffe
018700170405     d dMTC          e ds
018800170406
018900170406      // - DS per passaggio clienti PACKED per int. delta
019000170406     d                 ds
019100170406     d  dsKSC                  1      4p 0
019200170406     d  dsKSA                  1      4
019300170405
019400170405      //---------------------------------------------------------------
019500170405      //?Definizione variabili globali.
019600170405      //---------------------------------------------------------------
019700170405      // - Flags booleani
019800170405     d EndS03          s               n   inz(*off)
019900170405     d ErrGrave        s               n   inz(*off)
020000170405     d Fine            s               n   inz(*off)
020100170405     d RecOK           s               n   inz(*off)
020200170405     d TroppiRcd       s               n   inz(*off)
020300170405     d wEoF            s               n   inz(*off)
020400170406     d wEoFclp         s               n   inz(*off)
020500170405     d wEnd            s               n   inz(*off)
020600170405     d wInzD02         s               n   inz(*off)
020700170405     d wInzS03         s               n   inz(*off)
020800170406     d wInzD04         s               n   inz(*off)
020900170406     d wMaxNrr         s               n   inz(*off)
021000170405
021100170405      // - Indici di schiera
021200170405     d xx              s              4s 0 inz
021300170405
021400170405      // - Campi associati al video
021500170405     d Video           s              2a   inz('D2')
021600170405     d S03nrr          s              4s 0 inz
021700170405     d sav_S03nrr      s              4s 0 inz
021800170405
021900170405       // - Stringa SQL da eseguire
022000170405     d wSQL            s           4096    Varying        inz
022100170405
022200170405      // - Campi di comodo data
022300170405     d Data_EUR        s               d   datfmt(*eur)
022400170405     d Data_ISO        s               d   datfmt(*iso)
022500170405     d Oggi            s              8s 0 inz
022600170405     d wData           s              8s 0
022700170406     d wDataScadPwd    s              8s 0
022800170405
022900170405      // - Campi di comodo
023000170406     d conta           s              2s 0 inz
023100170406     d sav_ctr         s                   like(TAMctr)
023200170406     d sav_pwd         s                   like(V02pwd)
023300170405     d wabi            s                   like(OTAA1cabi) inz
023400170406     d wcmm            s              7s 0 inz
023500170405     d wfil            s              3s 0 inz
023600170405     d wksc            s              7s 0 inz
023700170406     d wksu            s              7s 0 inz
023800170405     d wGiorni         s              3s 0 inz
023900170405     d w001a           s              1a   inz
024000170405     d w0030           s              3s 0 inz
024100170405     d w0070           s              7s 0 inz
024200170406     d w0070i          s              7s 0 inz
024300170406     d w0070f          s              7s 0 inz
024400170405     d w050a           s             50a   inz
024500170406
024600170406      // - Campi x ricerca tabelle TABEL
024700170406     d idTabella       s              2
024800170406     d Ordinamento     s              1
024900170406     d idElemento      s              8
025000170406     d TastoUscita     s              1
025100170405
025200170405       // - Parametri per ricerca Filiale
025300170405     d pinFIL          s              3
025400170405     d pinFAG          s              1
025500170405     d pinDES          s             25
025600170405     d pinDIT          s              3
025700170405
025800170405      // - Parametri per ricerca Commerciale Unificante
025900170405      /copy gaitrasrc/srcprotopi,TRMK43R_1
026000170405
026100170405      // ----------------------------------------------------------------------
026200170405      //?DATI per ordinamento subfile
026300170405      // ----------------------------------------------------------------------
026400170405     d MaxKey          c                   6
026500170405     d Ascendente      c                   1
026600170405     d Discendente     c                   2
026700170405     d Carattere       c                   6
026800170405     d Numerico        c                   8
026900170405     d NumericoSegno   c                   2
027000170405     d Put             c                   1
027100170405     d EndPut          c                   2
027200170405     d Get             c                   3
027300170405      *************************************************************************
027400170405      * Campi utili:
027500170405      *     nrr        - Numero relativo di record del Subfile
027600170405      *     SizeList   - Dimensione della lista
027700170405      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
027800170405      *************************************************************************
027900170405     d NotUsed         s             16A
028000170405     d ReturnSize      s              9B 0
028100170406     d SizeList        s             10I 0
028200170405     d RrnLast         s              5I 0
028300170405     d WrkSort         s              1  0 inz(0)
028400170405      *************************************************************************
028500170405      * Data Structures
028600170405      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
028700170405      *     QLGSCB     - The sort request block for the QLGSORT API
028800170405      *     QLGSCB00   - The sort request block for the QLGSRTIO API
028900170405      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
029000170405      *     QUSEC      - Error structure for the QLGSORT API
029100170405      *************************************************************************
029200170405     d sflrcd          ds
029300170405     d  VS3cmm
029400170405     d  VS3ksu
029500170405     d  VS3ksc
029600170405     d  VS3ctr
029700170405     d  VS3prg
029800170405     d  VS3opz
029900170405     d  VS3clv
030000170405     d  VS3blc
030100170405     d  VS3kscd
030200170406     d  VH3kscd
030300170405     d  VS3cmmd
030400170405     d  VS3ddt
030500170405     d  VS3dst
030600170405     d  VS3des
030700170405     d  VS3bap
030800170405     d  VS3ecm
030900170405     d  VS3gva
031000170405     d  VS3ksud
031100170405     d  selected                      1A
031200170405
031300170405      /COPY QSYSINC/QRPGLESRC,QLGSORT
031400170405     d QLGKL                         16    DIM(MaxKey)
031500170405     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
031600170405     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
031700170405     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
031800170405     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
031900170405
032000170405      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
032100170405      /COPY QSYSINC/QRPGLESRC,QUSEC
032200170405
032300170405      //---------------------------------------------------------------
032400170405      //?Definizione procedure usate.
032500170405      //---------------------------------------------------------------
032600170405      // - Ordinamento Subfile
032700170405     d Qlgsort_pr      pr                  extpgm('QLGSORT')
032800170405     d  pr_QLGSCB                          like(Qlgscb)
032900170405     d  pr_NotUsed1                        like(NotUsed)
033000170405     d  pr_NotUsed2                        like(NotUsed)
033100170405     d  pr_SizeList                        like(SizeList)
033200170405     d  pr_ReturnSize                      like(ReturnSize)
033300170405     d  pr_QUSEC                           like(Qusec)
033400170405     d                                     options(*varsize)
033500170405
033600170405     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
033700170405     d  pr_QLGSCB00                        like(QLGSCB00)
033800170405     d  pr_SflRcd                          like(SflRcd)
033900170405     d  pr_NotUsed1                        like(NotUsed)
034000170405     d  pr_SizeList                        like(SizeList)
034100170405     d  pr_NotUsed2                        like(NotUsed)
034200170405     d  pr_QUSEC                           like(Qusec)
034300170405     d                                     options(*varsize)
034400170405
034500170405     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
034600170405     d  pr_QLGSCB00                        like(QLGSCB00)
034700170405     d  pr_NotUsed1                        like(NotUsed)
034800170405     d  pr_SflRcd                          like(SflRcd)
034900170405     d  pr_SizeList                        like(SizeList)
035000170405     d  pr_NotUsed2                        like(NotUsed)
035100170405     d  pr_QUSEC                           like(Qusec)
035200170405     d                                     options(*varsize)
035300170406
035400170406       // - Delta cliente unificante
035500170406     d TISE61R         pr                  extpgm('TISE61R')
035600170406     d  kpjba                              likeds(kpjba)
035700170406
035800170406       // - Ricerca anagrafica clienti
035900170406     d TNTAI1R         pr                  extpgm('TNTAI1R')
036000170406     d  kpjba                              likeds(kpjba)
036100170406     d  tntai1ds                           likeds(tntai1ds)
036200170406
036300170406       // - Int. anagrafica clienti
036400170406     d TNTAI2R         pr                  extpgm('TNTAI2R')
036500170406     d  kpjba                              likeds(kpjba)
036600170406     d  tntai2ds                           likeds(tntai2ds)
036700170405
036800170405       // - Caricamento Filiali in gestione
036900170405     d TRUL31R         pr                  extpgm('TRUL31R')
037000170405     d  kpjba                              likeds(kpjba)
037100170405     d  trul31ds                           likeds(trul31ds)
037200170405
037300170405      //---------------------------------------------------------------
037400170405      //?Definizione Prototipi.
037500170405      //---------------------------------------------------------------
037600170405      /copy gaitrasrc/srcprotopr,TIBS02R
037700170405      /copy gaitrasrc/srcprotopr,TIBS10R
037800170405      /copy gaitrasrc/srcprotopr,TIBS34R
037900170405      /copy gaitrasrc/srcprotopr,TIBS69R
038000170406      /copy gaitrasrc/srcprotopr,TNTA02R2
038100170405      /copy gaitrasrc/srcprotopr,TNSD24R
038200170405      /copy gaitrasrc/srcprotopr,TNTAA1C
038300170405      /copy gaitrasrc/srcprotopr,TRMK43R
038400170406      /copy gaitrasrc/srcprotopr,TRUL19R
038500170405
038600170405      //---------------------------------------------------------------
038700170405      //?Definizione key-list.
038800170405      //---------------------------------------------------------------
038900170406      // - File TABEL00F
039000170405     d k03tabel      e ds                  extname(TABEL00F:*key)
039100170405     d                                     prefix(k_)
039200170405
039300170405      //---------------------------------------------------------------
039400170405      //?M A I N - L I N E
039500170405      //---------------------------------------------------------------
039600170405     c     *Entry        plist
039700170405     c                   parm                    kpjba
039800170405
039900170405      /free
040000170405
040100170405       //?Operazioni iniziali
040200170405       exsr RoutInz;
040300170405
040400170405       //?Gestione video
040500170405       DOW  Fine = *off;
040600170405         SELECT;
040700170405           WHEN  Video = 'D2';
040800170405             exsr GesD02;
040900170405           WHEN  Video = 'S3';
041000170405             exsr GesS03;
041100170406           WHEN  Video = 'D4';
041200170406             exsr GesD04;
041300170405           OTHER;
041400170405             Fine = *on;
041500170405         ENDSL;
041600170405       ENDDO;
041700170405
041800170405       //?Operazioni finali
041900170405       exsr RoutEnd;
042000170405
042100170405       //--------------------------------------------------------------
042200170405       //?Operazioni iniziali.
042300170405       //--------------------------------------------------------------
042400170405       BEGSR RoutInz;
042500170405
042600170405         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
042700170405
042800170405       //?Impostazione campi "fissi"
042900170405         V01pgm = SDSpgm;
043000170405         k_TBLkut = 1;
043100170405         Video = 'D2';
043200170405         wInzD02 = *on;
043300170405
043400170405       //?Imposto oggi
043500170405         Oggi = %dec(%date());
043600170405
043700170405       //?Reperimento dati job
043800170405         exsr DatiJob;
043900170405
044000170405       //?Controllo abilitazione utente
044100170405         reset TNTAA1DS;
044200170405         ITAA1caut = '§utegtc';
044300170405         clear kpjbu;
044400170405         kpjbu = TNTAA1DS;
044500170405         tntaa1c (kpjba);
044600170405         TNTAA1DS = kpjbu;
044700170405
044800170405         IF  OTAA1fabi = 'N';
044900170405           ErrGrave = *on;
045000170405           leavesr;
045100170405         ENDIF;
045200170405         wabi = OTAA1cabi;
045300170405
045400170405       //?Carico le filiali abilitate all'utente
045500170405         clear TRUL31DS;
045600170405         I31abi = wabi;
045700170405         I31cdi = DUTdis;
045800170405         I31car = DUTare;
045900170405         I31cpo = DUTpou;
046000170405         TRUL31R (kpjba:trul31ds);
046100170405         IF O31pog <= *zeros;
046200170405           ErrGrave = *on;
046300170405           leavesr;
046400170405         ENDIF;
046500170405
046600170405       //?Carico sk Importanza Clienti
046700170405         clear xx;
046800170405         k_TBLcod = 'IC';
046900170405         clear k_TBLkey;
047000170405         setll %kds(K03tabel) TABEL00F;
047100170405         reade %kds(K03tabel : 2) TABEL00F;
047200170405         DOW  not %eof(TABEL00F);
047300170405           IF  TBLflg = *blanks;
047400170405             xx += 1;
047500170405             skIC(xx) = %subst(TBLkey:1:1);
047600170405             dsIC = TBLuni;
047700170405             skIC_ord(xx) = §SICor;
047800170405           ENDIF;
047900170405           reade %kds(K03tabel : 2) TABEL00F;
048000170405         ENDDO;
048100170405
048200170405       //?Cerco la tabella gg. validità password
048300170405         clear TIBS02DS;
048400170405         clear dMTC;
048500170405         T02mod = 'C';
048600170405         T02cod = 'MTC';
048700170405         T02sif = knsif;
048800170405         T02ke1 = 'PSW';
048900170405         TNTBE_RicercaControllo (kpjba : tibs02ds);
049000170405         IF  T02err <> *blanks;
049100170405           ErrGrave   = *on;
049200170405           ErrMessage  = *on;
049300170405           ErrGenerico = *on;
049400170405           PosCurPWD   = *on;
049500170406           V02msg      = Msg(11);
049600170405           leavesr;
049700170405         ENDIF;
049800170405         wGiorni = %dec(T02uni:3:0);
049900170405
050000170405       //?Cerco la password memorizzata per filiale (utente)
050100170405         clear TIBS02DS;
050200170405         clear dMTC;
050300170405         T02mod = 'C';
050400170405         T02cod = 'MTC';
050500170405         T02sif = knsif;
050600170405         T02ke1 = %editc(DUTpou:'X');
050700170405         TNTBE_RicercaControllo (kpjba : tibs02ds);
050800170405         IF  T02err <> *blanks;
050900170405           ErrGrave   = *on;
051000170405           ErrMessage  = *on;
051100170405           ErrGenerico = *on;
051200170406           V02msg = Msg(12);
051300170405           leavesr;
051400170405         ENDIF;
051500170405         dMTC = T02uni;
051600170405
051700170406         clear sav_pwd;
051800170405
051900170405       ENDSR;
052000170405
052100170405       //--------------------------------------------------------------
052200170405       //?Reperimento Dati del job (Utente/Operativi).
052300170405       //--------------------------------------------------------------
052400170405       BEGSR DatiJob;
052500170405
052600170405         in(E) §AzUte;
052700170405         IF  NOT %error;
052800170405           in(E) §DatiUte;
052900170405         ENDIF;
053000170405         IF  %error or RSut = *blanks;
053100170405           clear TIBS34ds;
053200170405           tibs34r(tibs34ds);
053300170405           in §AzUte;
053400170405           in §DatiUte;
053500170405         ENDIF;
053600170405
053700170405       ENDSR;
053800170405
053900170405       //--------------------------------------------------------------
054000170405       //?Gestione videata D02.
054100170405       //--------------------------------------------------------------
054200170405       BEGSR GesD02;
054300170405
054400170405       //?Inizializzazione videata
054500170405         IF  wInzD02;
054600170405           exsr InzD02;
054700170405           wInzD02 = *off;
054800170405         ENDIF;
054900170405
055000170405       //?Se errore grave emetto messaggio poi esco
055100170405         IF  ErrGrave;
055200170405           ErrMessage  = *on;
055300170405           ErrGenerico = *on;
055400170405           V02msg = Msg(01);
055500170405         ENDIF;
055600170405
055700170405       //?Emissione Testata
055800170405         write MKP0T01;
055900170405
056000170405       //?Emissione videata
056100170405         exfmt MKP0D02;
056200170405         reset ErrMessage;
056300170405         reset ErrGenerico;
056400170405         clear V02msg;
056500170405
056600170405         SELECT;
056700170405
056800170405       //?- Errore Grave esco
056900170405           WHEN  ErrGrave;
057000170405             Fine = *on;
057100170405
057200170405       //?- F03=Fine
057300170405           WHEN  dsp_aid = c_F03;
057400170405             exsr F03D02;
057500170405
057600170405       //?Invio
057700170405           OTHER;
057800170405             exsr ContrD02;
057900170405             IF  ErrGenerico;
058000170405               leavesr;
058100170405             ENDIF;
058200170405         //?Videata sucessiva
058300170405             Video = 'S3';
058400170405             wInzS03 = *on;
058500170405
058600170405         ENDSL;
058700170405
058800170405       ENDSR;
058900170405
059000170405       //--------------------------------------------------------------
059100170405       //?Inizializzazione Videata D02.
059200170405       //--------------------------------------------------------------
059300170405       BEGSR InzD02;
059400170405
059500170405       //?Pulizia videata
059600170405         clear MKP0D02;
059700170405
059800170405       //?Se errore grave non imposto più niente
059900170405         IF  ErrGrave;
060000170405           leavesr;
060100170405         ENDIF;
060200170405
060300170405       //?Imposto i default
060400170405         V02escludi = 'S';
060500170405
060600170405       //?Password
060700170406         V02pwd = sav_pwd;
060800170405
060900170405       ENDSR;
061000170405
061100170405       //--------------------------------------------------------------
061200170405       //?Gestione tasto funzionale F03 da videata D02
061300170405       //?F03=Fine
061400170405       //--------------------------------------------------------------
061500170405       BEGSR F03D02;
061600170405
061700170405       //?Chiusura del programma
061800170405         Fine = *on;
061900170405
062000170405       ENDSR;
062100170405
062200170405       //--------------------------------------------------------------
062300170405       //?Controlla Videata D02.
062400170405       //--------------------------------------------------------------
062500170405       BEGSR ContrD02;
062600170405
062700170405         WindDspF = IndDspF;
062800170405         reset WindDspFb;
062900170405         IndDspF  = WindDspF;
063000170406
063100170406       //?Almeno 1 tra Filiale - Commerciale - Cliente
063200170406       //?deve essere inserito
063300170406         IF  V02fil = *blanks and V02cmm = *blanks and
063400170406             V02ksu = *blanks and V02ksc = *blanks;
063500170406           ErrMessage  = *on;
063600170406           ErrGenerico = *on;
063700170406           PosCurFIL   = *on;
063800170406           V02msg = Msg(02);
063900170406           leavesr;
064000170406         ENDIF;
064100170405
064200170405       //?Filiale
064300170405         exsr Filiale;
064400170405         IF  ErrGenerico;
064500170405           leavesr;
064600170405         ENDIF;
064700170405         IF  V02fil <> *blanks;
064800170405           wfil = %int(V02fil);
064900170405         ENDIF;
065000170405
065100170405       //?Commerciale Unificante
065200170405         exsr Commerciale;
065300170405         IF  ErrGenerico;
065400170405           leavesr;
065500170405         ENDIF;
065600170405         IF  V02cmm <> *blanks;
065700170405           wcmm = %int(V02cmm);
065800170405         ENDIF;
065900170405
066000170405       //?Cliente
066100170405         exsr Cliente;
066200170405         IF  ErrGenerico;
066300170405           leavesr;
066400170405         ENDIF;
066500170405         IF  V02ksc <> *blanks;
066600170405           wksc = %int(V02ksc);
066700170405         ENDIF;
066800170405
066900170405       //?Cliente Unificante
067000170405         exsr Unificante;
067100170405         IF  ErrGenerico;
067200170405           leavesr;
067300170405         ENDIF;
067400170406         IF  V02ksu <> *blanks;
067500170406           wksu = %int(V02ksu);
067600170406         ENDIF;
067700170405
067800170405       //?Cliente e Unificante sono il alternativa
067900170405         IF  V02ksc <> *blanks and V02ksu <> *blanks;
068000170405           ErrMessage  = *on;
068100170405           ErrGenerico = *on;
068200170405           PosCurKSC   = *on;
068300170406           V02msg = Msg(07);
068400170405           leavesr;
068500170405         ENDIF;
068600170405
068700170405       //?Importanza Cliente
068800170405         w001a = V02clv1;
068900170405         clear w050a;
069000170405         exsr ImpCliente;
069100170405         V02clv1 = w001a;
069200170405         IF  ErrGenerico;
069300170405           PosCurCLV1 = *on;
069400170405           leavesr;
069500170405         ENDIF;
069600170405         w001a = V02clv2;
069700170405         clear w050a;
069800170405         exsr ImpCliente;
069900170405         V02clv2 = w001a;
070000170405         IF  ErrGenerico;
070100170405           PosCurCLV2 = *on;
070200170405           leavesr;
070300170405         ENDIF;
070400170405         w001a = V02clv3;
070500170405         clear w050a;
070600170405         exsr ImpCliente;
070700170405         V02clv3 = w001a;
070800170405         IF  ErrGenerico;
070900170405           PosCurCLV3 = *on;
071000170405           leavesr;
071100170405         ENDIF;
071200170405         w001a = V02clv4;
071300170405         clear w050a;
071400170405         exsr ImpCliente;
071500170405         V02clv4 = w001a;
071600170405         IF  ErrGenerico;
071700170405           PosCurCLV4 = *on;
071800170405           leavesr;
071900170405         ENDIF;
072000170405         w001a = V02clv5;
072100170405         clear w050a;
072200170405         exsr ImpCliente;
072300170405         V02clv5 = w001a;
072400170405         IF  ErrGenerico;
072500170405           PosCurCLV5 = *on;
072600170405           leavesr;
072700170405         ENDIF;
072800170410
072900170410       //?Controllo % sped. E-Commerce
073000170410         IF  V02ecom <> 'S' and V02perc > 0;
073100170410           ErrMessage  = *on;
073200170410           ErrGenerico = *on;
073300170410           PosCurPERC  = *on;
073400170410           V02msg = Msg(15);
073500170410         ENDIF;
073600170405
073700170405       //?Controllo Password
073800170405         exsr Password;
073900170405         IF  ErrGenerico;
074000170405           leavesr;
074100170405         ENDIF;
074200170405
074300170405       //?Carico in SK i codici richiesti
074400170405         clear skCLV;
074500170405         skCLV(1) = V02clv1;
074600170405         skCLV(2) = V02clv2;
074700170405         skCLV(3) = V02clv3;
074800170405         skCLV(4) = V02clv4;
074900170405         skCLV(5) = V02clv5;
075000170405
075100170405       ENDSR;
075200170405
075300170405       //--------------------------------------------------------------
075400170405       //?Controllo Filiale.
075500170405       //--------------------------------------------------------------
075600170405       BEGSR Filiale;
075700170405
075800170405         clear V02fild;
075900170405
076000170405         IF  V02fil = *blanks;
076100170405           leavesr;
076200170405         ENDIF;
076300170405
076400170405       //?Ricerca Filale
076500170405         IF  %scan('?' : V02fil) > 0;
076600170405           clear pinFIL;
076700170405           clear pinFAG;
076800170405           clear pinDES;
076900170405           clear pinDIT;
077000170405           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
077100170405           IF pinFIL > *zeros;
077200170405             V02fil  = pinFIL;
077300170405             V02fild = pinDES;
077400170405           ELSE;
077500170405             clear V02fil;
077600170405           ENDIF;
077700170405           ErrGenerico = *on;
077800170405           PosCurFIL   = *on;
077900170405         ENDIF;
078000170405         IF  V02fil = *blanks;
078100170405           leavesr;
078200170405         ENDIF;
078300170405       //?Accetto solo dati numerici
078400170405         xx = 1;
078500170405         FOR xx by 1 to %len(V02fil);
078600170405           IF  %subst(V02fil:xx:1) <> *blanks and
078700170405               %check(Digits:%subst(V02fil:xx:1)) > *zeros;
078800170405             ErrMessage  = *on;
078900170405             ErrGenerico = *on;
079000170405             PosCurFIL   = *on;
079100170406             V02msg = Msg(03);
079200170405             leavesr;
079300170405           ENDIF;
079400170405         ENDFOR;
079500170405         w0030 = %dec(V02fil:3:0);
079600170405       //?Deve esistere la Filiale
079700170405         chain w0030 AZORG01L;
079800170405         IF  not %found(AZORG01L) or ORGfva <> *blanks;
079900170405           ErrMessage  = *on;
080000170405           ErrGenerico = *on;
080100170405           PosCurFIL   = *on;
080200170406           V02msg = Msg(03);
080300170405           leavesr;
080400170405         ENDIF;
080500170405       //?Decodifico filiale
080600170405         V02fild = ORGdes;
080700170405       //?Deve essere una filiale in carico all'utente
080800170405         IF  %lookup(%editc(w0030:'X'):POG) = 0;
080900170405           ErrGenerico = *on;
081000170405           ErrMessage  = *on;
081100170405           PosCurFIL   = *on;
081200170406           V02msg = %trim(Msg(03)) + '. Non in gestione';
081300170405           leavesr;
081400170405         ENDIF;
081500170407
081600170407       //?Deve essere una filiale Italia
081700170407         IF  not IsItalia (w0030);
081800170407           ErrGenerico = *on;
081900170407           ErrMessage  = *on;
082000170407           PosCurFIL   = *on;
082100170407           V02msg = %trim(Msg(03)) + '. Filiale Estera';
082200170407           leavesr;
082300170407         ENDIF;
082400170405
082500170405       ENDSR;
082600170405
082700170405       //--------------------------------------------------------------
082800170405       //?Controllo Commerciale.
082900170405       //--------------------------------------------------------------
083000170405       BEGSR Commerciale;
083100170405
083200170405         clear V02cmmd;
083300170405
083400170405         IF  V02cmm = *blanks;
083500170405           leavesr;
083600170405         ENDIF;
083700170405
083800170405       //?Ricerca Commerciale
083900170405         IF  %scan('?' : V02cmm) > 0;
084000170405           clear TRMK43DS;
084100170405           IMK43solu = 'S';
084200170405           IMK43fil = DUTpou;
084300170405           trmk43r (kpjba : TRMK43DS);
084400170405           IF  OMK43err = *off and OMK43fxx = *blanks;
084500170405             V02cmm  = %editc(OMK43cmm:'X');
084600170405             V02cmmd = OMK43des;
084700170405           ENDIF;
084800170405           IF  OMK43fxx <> *blanks;
084900170405             clear V02cmm;
085000170405           ENDIF;
085100170405           ErrGenerico = *on;
085200170405           PosCurCMM   = *on;
085300170405         ENDIF;
085400170405         IF  V02cmm = *blanks;
085500170405           leavesr;
085600170405         ENDIF;
085700170405       //?Accetto solo dati numerici
085800170405         xx = 1;
085900170405         FOR xx by 1 to %len(V02cmm);
086000170405           IF  %subst(V02cmm:xx:1) <> *blanks and
086100170405               %check(Digits:%subst(V02cmm:xx:1)) > *zeros;
086200170405             ErrMessage  = *on;
086300170405             ErrGenerico = *on;
086400170405             PosCurCMM   = *on;
086500170406             V02msg = Msg(04);
086600170405             leavesr;
086700170405           ENDIF;
086800170405         ENDFOR;
086900170405         w0070 = %dec(V02cmm:7:0);
087000170405       //?Deve esistere il Commerciale
087100170405         chain (w0070) AZCMM01L;
087200170405         IF  not %found(AZCMM01L);
087300170405           ErrMessage  = *on;
087400170405           ErrGenerico = *on;
087500170405           PosCurCMM   = *on;
087600170406           V02msg = Msg(04);
087700170405           leavesr;
087800170405         ENDIF;
087900170405       //?Decodifico Commerciale
088000170405         V02cmmd = CMMdes;
088100170405       //?Deve essere un Commerciale in carico all'utente
088200170405         clear TNTAA1DS;
088300170405         ITAA1caut = '§utegtc';
088400170405         ITAA1cmm  = w0070;
088500170405         kpjbu = TNTAA1DS;
088600170405         tntaa1c (kpjba);
088700170405         TNTAA1DS = kpjbu;
088800170405         IF  OTAA1fabi = 'N';
088900170405           ErrGenerico = *on;
089000170405           ErrMessage  = *on;
089100170405           PosCurCMM   = *on;
089200170406           V02msg = %trim(Msg(04)) + '. Non in gestione';
089300170405           leavesr;
089400170405         ENDIF;
089500170405       //?Deve essere un Commerciale Unificante
089600170405         IF  w0070 <> CMMuni;
089700170405           ErrGenerico = *on;
089800170405           ErrMessage  = *on;
089900170405           PosCurCMM   = *on;
090000170406           V02msg = %trim(Msg(04)) + '. Non è un Commerciale Unificante';
090100170405           leavesr;
090200170405         ENDIF;
090300170411
090400170411       //?Deve essere congruente con la filiale inserita
090500170413         IF  wfil > *zeros and
090600170413             %dec(%subst(%editc(w0070:'X'):1:3):3:0) <> wfil;
090700170411           ErrGenerico = *on;
090800170411           ErrMessage  = *on;
090900170411           PosCurCMM   = *on;
091000170411           V02msg = %trim(Msg(04)) +
091100170411           '. Non congruente con la filiale inserita';
091200170411           leavesr;
091300170411         ENDIF;
091400170405
091500170405       //?Carico in sk tutti i codici del Commerciale Unificante
091600170405         exsr CarskAGE;
091700170405
091800170405       ENDSR;
091900170405
092000170405       //--------------------------------------------------------------
092100170405       //?Controllo Cliente.
092200170405       //--------------------------------------------------------------
092300170405       BEGSR Cliente;
092400170405
092500170405         clear V02kscd;
092600170405
092700170405         IF  V02ksc = *blanks;
092800170405           leavesr;
092900170405         ENDIF;
093000170405
093100170405       //?Ricerca Cliente
093200170405         IF  %scan('?' : V02ksc) > 0;
093300170405           clear TNTAI1DS;
093400170405           ITAI1ric = 'S';
093500170405           ITAI1abi = wabi;
093600170405           ITAI1aut = '*C';
093700170405           tntai1r (kpjba : TNTAI1DS);
093800170405           ErrGenerico = *on;
093900170405           PosCurKSC   = *on;
094000170405           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
094100170405               OTAI1err <> *blanks;
094200170405             clear V02ksc;
094300170405           ELSE;
094400170405             V02ksc = %editc(OTAI1ksc:'X');
094500170405           ENDIF;
094600170405           IF  OTAI1err <> *blanks;
094700170405             ErrMessage  = *on;
094800170405             V02msg = OTAI1msg;
094900170405           ENDIF;
095000170405         ENDIF;
095100170405         IF  V02ksc = *blanks;
095200170405           leavesr;
095300170405         ENDIF;
095400170405       //?Accetto solo dati numerici
095500170405         xx = 1;
095600170405         FOR xx by 1 to %len(V02ksc);
095700170405           IF  %subst(V02ksc:xx:1) <> *blanks and
095800170405               %check(Digits:%subst(V02ksc:xx:1)) > *zeros;
095900170405             ErrMessage  = *on;
096000170405             ErrGenerico = *on;
096100170405             PosCurKSC   = *on;
096200170406             V02msg = Msg(05);
096300170405             leavesr;
096400170405           ENDIF;
096500170405         ENDFOR;
096600170405         w0070 = %dec(V02ksc:7:0);
096700170405       //?Deve esistere il Cliente
096800170405         clear TIBS69DS;
096900170405         clear ACOrag;
097000170405         I69kac = w0070;
097100170406         I69kcp = w0070;
097200170405         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
097300170405         IF  O69err <> *blanks;
097400170405           ErrMessage  = *on;
097500170405           ErrGenerico = *on;
097600170405           PosCurKSC   = *on;
097700170406           V02msg      = Msg(05);
097800170405         ENDIF;
097900170405       //?Decodifico Cliente
098000170405         V02kscd = ACOrag;
098100170405       //?Deve essere un Cliente in carico all'utente
098200170405         clear TNTAA1DS;
098300170405         ITAA1caut = '§utegtc';
098400170405         ITAA1ksc  = w0070;
098500170405         kpjbu = TNTAA1DS;
098600170405         tntaa1c (kpjba);
098700170405         TNTAA1DS = kpjbu;
098800170405         IF  OTAA1fabi = 'N';
098900170405           ErrGenerico = *on;
099000170405           ErrMessage  = *on;
099100170405           PosCurKSC   = *on;
099200170406           V02msg = %trim(Msg(05)) + '. Non in gestione';
099300170405           leavesr;
099400170405         ENDIF;
099500170407
099600170407       //?Deve essere un cliente Italia
099700170407         w0030 = %dec(%subst(%editc(W0070:'X'):1:3):3:0);
099800170407         IF  not IsItalia (w0030);
099900170407           ErrGenerico = *on;
100000170407           ErrMessage  = *on;
100100170407           PosCurKSC   = *on;
100200170407           V02msg = %trim(Msg(05)) + '. Cliente Estero';
100300170407           leavesr;
100400170407         ENDIF;
100500170405
100600170405       ENDSR;
100700170405
100800170405       //--------------------------------------------------------------
100900170405       //?Controllo Unificante.
101000170405       //--------------------------------------------------------------
101100170405       BEGSR Unificante;
101200170405
101300170405         clear V02ksud;
101400170405
101500170406         IF  V02ksu = *blanks;
101600170405           leavesr;
101700170405         ENDIF;
101800170405
101900170405       //?Ricerca Unificante
102000170405         IF  %scan('?' : V02ksu) > 0;
102100170405           clear TNTAI1DS;
102200170405           ITAI1ric = 'S';
102300170405           ITAI1abi = wabi;
102400170405           ITAI1aut = '*C';
102500170405           tntai1r (kpjba : TNTAI1DS);
102600170405           ErrGenerico = *on;
102700170405           PosCurKSU   = *on;
102800170405           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
102900170405               OTAI1err <> *blanks;
103000170405             clear V02ksu;
103100170405           ELSE;
103200170405             V02ksu = %editc(OTAI1ksc:'X');
103300170405           ENDIF;
103400170405           IF  OTAI1err <> *blanks;
103500170405             ErrMessage  = *on;
103600170405             V02msg = OTAI1msg;
103700170405           ENDIF;
103800170405         ENDIF;
103900170405         IF  V02ksu = *blanks;
104000170405           leavesr;
104100170405         ENDIF;
104200170405       //?Accetto solo dati numerici
104300170405         xx = 1;
104400170405         FOR xx by 1 to %len(V02ksu);
104500170405           IF  %subst(V02ksu:xx:1) <> *blanks and
104600170405               %check(Digits:%subst(V02ksu:xx:1)) > *zeros;
104700170405             ErrMessage  = *on;
104800170405             ErrGenerico = *on;
104900170405             PosCurKSU   = *on;
105000170406             V02msg = Msg(05);
105100170405             leavesr;
105200170405           ENDIF;
105300170405         ENDFOR;
105400170405         w0070 = %dec(V02ksu:7:0);
105500170405       //?Deve esistere il Cliente Unificante
105600170405         clear TIBS69DS;
105700170405         clear ACOrag;
105800170405         I69kac = w0070;
105900170405         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
106000170405         IF  O69err <> *blanks;
106100170405           ErrMessage  = *on;
106200170405           ErrGenerico = *on;
106300170405           PosCurKSU   = *on;
106400170406           V02msg      = Msg(05);
106500170405         ENDIF;
106600170405       //?Decodifico Cliente
106700170405         V02ksud = ACOrag;
106800170405       //?Deve essere un Cliente in carico all'utente
106900170405         clear TNTAA1DS;
107000170405         ITAA1caut = '§utegtc';
107100170405         ITAA1ksc  = w0070;
107200170405         kpjbu = TNTAA1DS;
107300170405         tntaa1c (kpjba);
107400170405         TNTAA1DS = kpjbu;
107500170405         IF  OTAA1fabi = 'N';
107600170405           ErrGenerico = *on;
107700170405           ErrMessage  = *on;
107800170405           PosCurKSU   = *on;
107900170406           V02msg = %trim(Msg(05)) + '. Non in gestione';
108000170405           leavesr;
108100170405         ENDIF;
108200170405
108300170405         wksc = w0070;
108400170405       //?Come prima cosa verifico se il codice inserito è padre
108500170405         clear TIBS10DS;
108600170405         D10tle = 'ST';
108700170405         D10paf = 'F';
108800170405         D10cod = w0070;
108900170405         TIBS10R (TIBS10DS);
109000170405       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
109100170405         IF  D10err <> *blanks;
109200170405         //?Quindi provo a cercare come figlio
109300170405           clear TIBS10DS;
109400170405           D10tle = 'ST';
109500170405           D10paf = 'P';
109600170405           D10cod = w0070;
109700170405           TIBS10R (TIBS10DS);
109800170405         //?Se ritorna errore vuol dire che è padre di se stesso
109900170405         //?quindi carico il codice inserito com primo elemento della sk
110000170405           IF  D10err <> *blanks;
110100170406             w0070 = %dec(V02ksu:7:0);
110200170405             sk_D10ksc(01) = w0070;
110300170405             leavesr;
110400170405           ENDIF;
110500170405         ENDIF;
110600170405       //?Carico i dati del padre
110700170405         IF  D10cop > 0;
110800170405           w0070 = D10cop;
110900170405         //?se variato avviso l'utente
111000170405           IF  wksc <> w0070;
111100170405             ErrGenerico = *on;
111200170405             ErrMessage  = *on;
111300170405             PosCurKSU   = *on;
111400170406             V02msg = Msg(08);
111500170406             %subst(V02msg:24:7) = %editc(wksc:'X');
111600170405           ENDIF;
111700170405           clear TIBS69DS;
111800170405           clear ACOrag;
111900170405           I69kac = w0070;
112000170405           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
112100170405         //?Decodifico Cliente
112200170405           V02ksud = ACOrag;
112300170405           V02ksu  = %editc(ACOksc:'X');
112400170405         ENDIF;
112500170405
112600170405       ENDSR;
112700170405
112800170405       //--------------------------------------------------------------
112900170405       //?Controllo Importanza Cliente.
113000170405       //--------------------------------------------------------------
113100170405       BEGSR ImpCliente;
113200170405
113300170405         IF  w001a = *blanks;
113400170405           leavesr;
113500170405         ENDIF;
113600170405
113700170405         IF  %scan('?' : w001a) > 0;
113800170405           ErrGenerico = *on;
113900170405           idTabella = 'IC';
114000170405           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
114100170405                          tastoUscita);
114200170405           w001a = idElemento;
114300170405         ENDIF;
114400170405         IF  %lookup(w001a : skIC) = 0;
114500170405           ErrMessage  = *on;
114600170405           ErrGenerico = *on;
114700170406           V02msg = Msg(10);
114800170405           leavesr;
114900170405         ENDIF;
115000170405
115100170405       ENDSR;
115200170405
115300170405       //--------------------------------------------------------------
115400170405       //?Controllo Password
115500170405       //--------------------------------------------------------------
115600170405       BEGSR Password;
115700170405
115800170405       //?Password obbligatoria
115900170406         IF  V02pwd = *blanks and sav_pwd = *blanks;
116000170405           ErrMessage = *on;
116100170405           ErrGenerico = *on;
116200170405           PosCurPWD   = *on;
116300170406           V02msg      = Msg(13);
116400170405           leavesr;
116500170405         ENDIF;
116600170405
116700170405       //?Controllo se password OK
116800170405         IF  V02pwd <> §MTCpwd;
116900170405           ErrMessage = *on;
117000170405           ErrGenerico = *on;
117100170405           PosCurPWD   = *on;
117200170406           V02msg      = Msg(13);
117300170405           clear V02pwd;
117400170405           leavesr;
117500170405         ENDIF;
117600170405
117700170405       //?Data immissione/modifica password
117800170405         wData = %dec(%subst(%editc(§MTCdta:'X'):5:4):4:0) * 10000;
117900170405         wData += %dec(%subst(%editc(§MTCdta:'X'):3:2):2:0) * 100;
118000170405         wData += %dec(%subst(%editc(§MTCdta:'X'):1:2):2:0);
118100170405         Data_ISO = %date(wData);
118200170405
118300170405       //?Calcolo la data scadenza password
118400170405         Data_ISO += %days(wgiorni);
118500170406         wDataScadPwd = %dec(Data_ISO);
118600170405
118700170405       //?Controllo se la password è scaduta
118800170406         IF Oggi > wDataScadPwd;
118900170405           ErrMessage  = *on;
119000170405           ErrGenerico = *on;
119100170405           PosCurPWD   = *on;
119200170406           V02msg      = Msg(14);
119300170405           leavesr;
119400170405         ENDIF;
119500170405
119600170406         sav_pwd = V02pwd;
119700170405
119800170405       ENDSR;
119900170405
120000170405       //--------------------------------------------------------------
120100170405       //?Gestione videata S03.
120200170405       //--------------------------------------------------------------
120300170405       BEGSR GesS03;
120400170405
120500170405       //?Inizializzazione videata
120600170405         IF  wInzS03;
120700170405           exsr InzS03;
120800170405           wInzS03 = *off;
120900170406           IF  wMaxNrr;
121000170406             leavesr;
121100170406           ENDIF;
121200170405         ENDIF;
121300170405
121400170405       //?Visualizzazione del SFL (se ci sono dati)
121500170405         SflDsp = (S03nrr <= *zeros);
121600170405
121700170405       //?Posizionamento cursore
121800170405         V03rcd = V03csr;
121900170405
122000170405       //?Emissione Testata e Piede con tasti funzionali abilitati
122100170405         write MKP0T01;
122200170405         write MKP0P03;
122300170405
122400170405       //?Emissione videata
122500170405         exfmt MKP0C03;
122600170405         reset ErrMessage;
122700170405         reset ErrGenerico;
122800170405         clear V03msg;
122900170405
123000170405         SELECT;
123100170405
123200170405       //?- F03=Fine
123300170405           WHEN  dsp_aid = c_F03;
123400170405             exsr F03D02;
123500170406
123600170406       //?- F06=Conferma
123700170406           WHEN  dsp_aid = c_F06;
123800170410             exsr OpzS03;
123900170410             IF  ErrGenerico;
124000170410               leavesr;
124100170410             ENDIF;
124200170406             exsr F06S03;
124300170405
124400170405       //?- F12=Ritorno
124500170405           WHEN  dsp_aid = c_F12;
124600170405             exsr F12S03;
124700170405
124800170405       //?Invio
124900170405           OTHER;
125000170405             IF  V03csr > *zero;
125100170405               exsr OpzS03;
125200170405               IF  ErrGenerico;
125300170405                 leavesr;
125400170405               ENDIF;
125500170405             ENDIF;
125600170405
125700170405         ENDSL;
125800170405
125900170405       ENDSR;
126000170405
126100170405       //--------------------------------------------------------------
126200170405       //?Inizializzazione videata S03.
126300170405       //--------------------------------------------------------------
126400170405       BEGSR InzS03;
126500170405
126600170405         EndS03 = *off;
126700170405
126800170405       //?Pulizia subfile
126900170405         exsr PulS03;
127000170405
127100170405       //?Caricamento subfile
127200170405         exsr Ries03;
127300170405
127400170405         rrnlast = S03nrr;
127500170405
127600170405         SflEnd = *on;
127700170405
127800170405       //?Imposto il posizionamento al primo rcd
127900170405         IF  S03nrr > 0;
128000170405           V03rcd = 1;
128100170405         ELSE;
128200170405           clear V03rcd;
128300170405         ENDIF;
128400170405
128500170405         V03csr = V03rcd;
128600170405
128700170405       //?Imposto il n. rcd del subfile solo se il nrr savlato
128800170405       //?è <= all'ultimo nrr del subfile appena caricato
128900170405       //?se no imposto l'ultimo nrr
129000170405         IF  sav_S03nrr > 0;
129100170405           IF  sav_S03nrr <= S03nrr;
129200170405             V03csr = sav_S03nrr;
129300170405             clear sav_S03nrr;
129400170405           ELSE;
129500170405             V03csr = S03nrr;
129600170405           ENDIF;
129700170405         ENDIF;
129800170405
129900170405       ENDSR;
130000170405
130100170405       //--------------------------------------------------------------
130200170405       //?Pulizia Subfile S03.
130300170405       //--------------------------------------------------------------
130400170405       BEGSR PulS03;
130500170405
130600170405       //?Pulizia subfile
130700170405         SflDsp    = *on;
130800170405         SflDspCtl = *on;
130900170405         write MKP0C03;
131000170405         SflDspCtl = *off;
131100170405         SflEnd    = *off;
131200170405
131300170405         clear V03rcd;
131400170405         clear V03csr;
131500170405         clear S03nrr;
131600170405         clear V03msg;
131700170405
131800170405         ErrMessage  = *off;
131900170405         ErrGenerico = *off;
132000170405
132100170405         WindDspF = IndDspF;
132200170405         reset WindDspFb;
132300170405         IndDspF  = WindDspF;
132400170405
132500170405       ENDSR;
132600170405
132700170405       //--------------------------------------------------------------
132800170405       //?Caricamento Subfile S03.
132900170405       //--------------------------------------------------------------
133000170405       BEGSR RieS03;
133100170405
133200170405         EndS03 = *off;
133300170406         wMaxNrr = *off;
133400170405
133500170405       //?Leggo in base alle selezioni fatte
133600170405         SELECT;
133700170405       //?Selezione per Filiale leggo
133800170405       //?dall'Anagrafico Commerciali
133900170405       //?dall'Anagrafico Clienti
134000170405       //?dalle Tariffe
134100170405         WHEN  V02fil <> *blanks;
134200170405           exsr Leggo_FIL;
134300170405       //?Selezione per Commerciale Unificante leggo
134400170405       //?dall'Anagrafico Commerciali
134500170405       //?dall'Anagrafico Clienti
134600170405       //?dalle Tariffe
134700170405         WHEN  V02cmm <> *blanks;
134800170405           exsr Leggo_CMM;
134900170405       //?Selezione per Cliente Unificante carico i figli e leggo
135000170405       //?dalle Tariffe
135100170406         WHEN  V02ksu <> *blanks;
135200170405           exsr Leggo_KSU;
135300170405       //?Selezione per Cliente leggo dalle Tariffe
135400170405         WHEN  V02ksc <> *blanks;
135500170407         //?Controllo ulteriori selezioni
135600170407           exsr ContrRecord;
135700170407           IF  RecOK;
135800170407             exsr Leggo_Tariffa;
135900170407           ENDIF;
136000170405         ENDSL;
136100170406
136200170406       //?Se ho superato il numero massimo di rcd da caricare esco
136300170406         IF  wMaxNrr;
136400170406           Video = 'D4';
136500170406           leavesr;
136600170406         ENDIF;
136700170406
136800170406         rrnlast = S03nrr;
136900170406
137000170406       //?Faccio l'ordinamento del subfile
137100170406         exsr Ordina;
137200170405
137300170405       ENDSR;
137400170405
137500170405       //--------------------------------------------------------------
137600170405       //?Selezione per Filiale.
137700170405       //--------------------------------------------------------------
137800170405       BEGSR Leggo_FIL;
137900170405
138000170405         wEoF = *off;
138100170405
138200170406         w0070i = wfil * 10000;
138300170406         w0070f = wfil * 10000 + 9999;
138400170407
138500170407       //?Se richiesto 1 solo commerciale seleziono solo quello
138600170407         IF  V02cmm <> *blanks;
138700170407           w0070i = wcmm;
138800170407           w0070f = wcmm;
138900170407         ENDIF;
139000170405
139100170405       //?Leggo TUTTI i commerciali unificanti della Filiale
139200170405         exec sql
139300170405         DECLARE COMMFIL cursor for
139400170405         SELECT CMMcod, CMMdes, CMMuni from AZCMM00F
139500170405         WHERE  CMMuni between :w0070i and :w0070f and
139600170405                CMMatb = ' ' and CMMpar = ' '
139700170405         ORDER BY CMMuni, CMMcod;
139800170405
139900170405         exec sql open COMMFIL;
140000170405
140100170405         IF  sqlcode < 0;
140200170405           wEoF = *on;
140300170405           exec sql close COMMFIL;
140400170405           leavesr;
140500170405         ENDIF;
140600170405
140700170405         DOW  not wEoF;
140800170405           exec sql
140900170405           fetch next from COMMFIL into :CMMcod, :CMMdes, :CMMuni;
141000170405           IF  sqlcod = 100 or sqlcod < 0;
141100170405             wEoF = *on;
141200170405             leave;
141300170405           ENDIF;
141400170405
141500170405         //?Per ogni Commerciale cerco i clienti dall'Anagrafica clienti
141600170405           exsr Leggo_Anagrafica;
141700170405
141800170405         ENDDO;
141900170405
142000170405         exec sql close COMMFIL;
142100170405
142200170405       ENDSR;
142300170405
142400170405       //--------------------------------------------------------------
142500170405       //?Selezione per Commerciale.
142600170405       //--------------------------------------------------------------
142700170405       BEGSR Leggo_CMM;
142800170405
142900170405         wEoF = *off;
143000170405
143100170405       //?Leggo il commerciale unificante
143200170405         exec sql
143300170405         DECLARE COMM cursor for
143400170405         SELECT CMMcod, CMMdes, CMMuni from AZCMM00F
143500170405         WHERE  CMMuni = :wcmm and
143600170405                CMMatb = ' ' and CMMpar = ' '
143700170405         ORDER BY CMMuni, CMMcod;
143800170405
143900170405         exec sql open COMM;
144000170405
144100170405         IF  sqlcode < 0;
144200170405           wEoF = *on;
144300170405           exec sql close COMM;
144400170405           leavesr;
144500170405         ENDIF;
144600170405
144700170405         DOW  not wEoF;
144800170405           exec sql
144900170405           fetch next from COMM into :CMMcod, :CMMdes, :CMMuni;
145000170405           IF  sqlcod = 100 or sqlcod < 0;
145100170405             wEoF = *on;
145200170405             leave;
145300170405           ENDIF;
145400170405
145500170405         //?Per ogni Commerciale cerco i clienti dall'Anagrafica clienti
145600170405           exsr Leggo_Anagrafica;
145700170405
145800170405         ENDDO;
145900170405
146000170405         exec sql close COMM;
146100170405
146200170405       ENDSR;
146300170405
146400170405       //--------------------------------------------------------------
146500170405       //?Selezione per Cliente Unificante.
146600170405       //--------------------------------------------------------------
146700170405       BEGSR Leggo_KSU;
146800170405
146900170405       //?Per ogni Cliente leggo la tariffa
147000170405         xx = 1;
147100170405         FOR xx by 1 to %elem(sk_D10ksc);
147200170405           IF  sk_D10ksc(xx) <> *zeros;
147300170405             wksc = sk_D10ksc(xx);
147400170407         //?Recupero i dati dell'anagrafica
147500170407             I69kac = wksc;
147600170407             I69kcp = wksc;
147700170407             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
147800170407         //?Controllo ulteriori selezioni
147900170407             exsr ContrRecord;
148000170407             IF  not RecOK;
148100170407               iter;
148200170407             ENDIF;
148300170405             exsr Leggo_Tariffa;
148400170405           ENDIF;
148500170405         ENDFOR;
148600170405
148700170405       ENDSR;
148800170405
148900170405       //--------------------------------------------------------------
149000170405       //?Leggo Anagrafiche Clienti per commerciale.
149100170405       //--------------------------------------------------------------
149200170405       BEGSR Leggo_Anagrafica;
149300170406
149400170406         wEoFclp = *off;
149500170405
149600170406         setll (1:DUTkci:CMMcod) CNCLP01L;
149700170406         DOW  not wEoFclp;
149800170406           reade (1:DUTkci:CMMcod) CNCLP01L;
149900170406           IF  %eof(CNCLP01L);
150000170406             wEoFclp = *on;
150100170406             leave;
150200170406           ENDIF;
150300170405         //?Escludo i codici esteri
150400170405           w0030 = %dec(%subst(%editc(CLPksc:'X'):1:3):3:0);
150500170407           IF  not IsItalia (w0030);
150600170406             iter;
150700170406           ENDIF;
150800170406
150900170406         //?Aggancio CNACO
151000170406           chain (CLPkut:CLPkcc:CLPksc) CNACO00F;
151100170411
151200170411         //?Controllo ulteriori selezioni
151300170411           exsr ContrRecordAna;
151400170411           IF  not RecOK;
151500170411             iter;
151600170411           ENDIF;
151700170406
151800170406         //?Controllo ulteriori selezioni
151900170406           exsr ContrRecord;
152000170406           IF  not RecOK;
152100170406             iter;
152200170406           ENDIF;
152300170406
152400170406         //?Se cliente OK leggo le tariffe
152500170406           wksc = CLPksc;
152600170406           exsr Leggo_Tariffa;
152700170405
152800170405         ENDDO;
152900170405
153000170405       ENDSR;
153100170411
153200170411       //--------------------------------------------------------------
153300170411       //?Controllo se rcd ok per le selezioni fatte su cliente.
153400170411       //--------------------------------------------------------------
153500170411       BEGSR ContrRecordAna;
153600170411
153700170411         RecOK = *on;
153800170411
153900170411       //?Se richiesto 1 solo cliente leggo solo quello
154000170411         IF  V02ksc <> *blanks and wksc <> CLPksc;
154100170411             RecOk = *off;
154200170411             leavesr;
154300170411         ENDIF;
154400170411
154500170411       //?Se richiesto 1 solo cliente Unificante leggo solo quello
154600170411         IF  V02ksu <> *blanks;
154700170411           xx = 1;
154800170411           FOR xx by 1 to %elem(sk_D10ksc);
154900170411             IF  sk_D10ksc(xx) <> *zeros;
155000170411               wksc = sk_D10ksc(xx);
155100170411               IF  wksc <> CLPksc;
155200170411                 RecOk = *off;
155300170411                 leavesr;
155400170411               ENDIF;
155500170411             ENDIF;
155600170411           ENDFOR;
155700170411         ENDIF;
155800170411
155900170411       ENDSR;
156000170405
156100170405       //--------------------------------------------------------------
156200170405       //?Controllo se rcd ok per le selezioni fatte.
156300170405       //--------------------------------------------------------------
156400170405       BEGSR ContrRecord;
156500170405
156600170405         RecOK = *on;
156700170405
156800170405       //?Selezione per Importanza cliente
156900170406         IF  V02clv1 <> *blanks or V02clv2 <> *blanks or
157000170406             V02clv3 <> *blanks or V02clv4 <> *blanks or
157100170406             V02clv5 <> *blanks;
157200170405           IF  CLPclv = *blanks;
157300170405             RecOk = *off;
157400170405             leavesr;
157500170405           ENDIF;
157600170405           IF  %lookup (CLPclv:skCLV) = 0;
157700170405             RecOk = *off;
157800170405             leavesr;
157900170405           ENDIF;
158000170405         ENDIF;
158100170405
158200170406       //?confronto la % Sped. E-Commerce
158300170406         IF  V02ecom = 'S';
158400170406           chain (ACOlib:'ECM') TICPI01L;
158500170406           IF  %found(TICPI01L) and CPIatb = *blanks and
158600170406               CPIval > V02perc;
158700170406           ELSE;
158800170405             RecOK = *off;
158900170405             leavesr;
159000170405           ENDIF;
159100170405         ENDIF;
159200170410         IF  V02ecom = 'N';
159300170410           chain (ACOlib:'ECM') TICPI01L;
159400170410           IF  %found(TICPI01L) and CPIatb = *blanks;
159500170410             RecOK = *off;
159600170410             leavesr;
159700170410           ENDIF;
159800170410         ENDIF;
159900170405
160000170405       ENDSR;
160100170406
160200170406       //--------------------------------------------------------------
160300170406       //?Leggo le Tariffe.
160400170406       //--------------------------------------------------------------
160500170406       BEGSR Leggo_Tariffa;
160600170406
160700170406         EndS03 = *off;
160800170406         sav_ctr = 999;
160900170406
161000170406         setll wksc TNTAM04L;
161100170406         DOW  not EndS03;
161200170406
161300170406           reade (wksc) TNTAM04L;
161400170406         //?Fine file
161500170406           IF  %eof(TNTAM04L);
161600170406             leavesr;
161700170406           ENDIF;
161800170406
161900170406         //?Carico le tariffe in corso o future
162000170406           IF  sav_ctr <> TAMctr;
162100170406             clear conta;
162200170406
162300170406           //?Data Scadenza > Oggi --> Carico
162400170406             IF  TAMdst > Oggi;
162500170406               exsr CarS03;
162600170406               conta += 1;
162700170406             ENDIF;
162800170406
162900170407           //?Data Decorrenza <= Oggi
163000170407           //?Se ho già caricato almeno una tariffa non devo più caricarne
163100170407             IF  TAMddt <= Oggi and conta > 0;
163200170407               sav_ctr = TAMctr;
163300170407               iter;
163400170407             ENDIF;
163500170407           //?Se non ho caricato ancora nessuna tariffa devo caricare
163600170407           //?questa anche se scaduta
163700170407             IF  TAMddt <= Oggi and conta = 0;
163800170407               exsr CarS03;
163900170407               sav_ctr = TAMctr;
164000170406             ENDIF;
164100170406
164200170406           ENDIF;
164300170406
164400170406         ENDDO;
164500170406
164600170406       ENDSR;
164700170405
164800170405       //--------------------------------------------------------------
164900170405       //?Carico i dati nel Subfile S03.
165000170405       //--------------------------------------------------------------
165100170405       BEGSR CarS03;
165200170407
165300170407       //?Se richiesta esclusione controllo la particolarità varie
165400170407         IF  V02escludi = 'S' and TAMgva = 'P';
165500170407           leavesr;
165600170407         ENDIF;
165700170405
165800170407         SflNxtChg = *off;
165900170405         clear MKP0S03;
166000170405         PosCurOPZ = *off;
166100170405
166200170406       //?Dati relativi al cliente
166300170406         VS3ksc = wksc;
166400170406         clear TIBS69DS;
166500170406         clear ACOrag;
166600170406         I69kac = wksc;
166700170406         I69kcp = wksc;
166800170406         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
166900170406         VS3kscd = ACOrag;
167000170406         VH3kscd = ACOrag;
167100170406         VS3clv = CLPclv;
167200170406         IF  ACOabl <> *blanks;
167300170406           VS3blc = 'B';
167400170406         ENDIF;
167500170406
167600170406       //?Dati commerciale
167700170406         IF  V02ksc <> *blanks or V02ksu <> *blanks;
167800170406           chain CLPage AZCMM01L;
167900170406           IF  not %found(AZCMM01L);
168000170406             clear CMMcod;
168100170406             clear CMMdes;
168200170406           ENDIF;
168300170406         ENDIF;
168400170410         VS3cmm = CMMuni;
168500170406         VS3cmmd = CMMdes;
168600170406
168700170406       //?Dati Tariffa
168800170406         VS3ctr = TAMctr;
168900170406         VS3prg = TAMprg;
169000170406         Data_ISO = %date(TAMddt);
169100170406         Data_EUR = Data_ISO;
169200170406         VS3ddt = %dec(Data_EUR);
169300170406         Data_ISO = %date(TAMdst);
169400170406         Data_EUR = Data_ISO;
169500170406         VS3dst = %dec(Data_EUR);
169600170406         VS3des = TAMdcv;
169700170406         VS3bap = TAMbap;
169800170406         VS3gva = TAMgva;
169900170406
170000170406       //?Info Commerciale E-Commerce
170100170406         chain (ACOlib:'ECM') TICPI01L;
170200170406         IF  %found(TICPI01L) and CPIatb = *blanks;
170300170406           Vs3ecm = CPIval;
170400170406         ENDIF;
170500170406
170600170406       //?Dati Cliente Unificante
170700170406         SELECT;
170800170406         WHEN  V02ksu <> *blanks;
170900170406           VS3ksu = wksu;
171000170406           VS3ksud = V02ksud;
171100170406         OTHER;
171200170406         //?Cerco il codice padre
171300170406           clear TIBS10DS;
171400170406           D10tle = 'ST';
171500170406           D10paf = 'P';
171600170406           D10cod = wksc;
171700170406           TIBS10R (TIBS10DS);
171800170406         //?Se torna errore vuol dire che NON è figlio di nessuno
171900170406         //?quindi imposto lui come unificante
172000170406           IF  D10err <> *blanks;
172100170406             VS3ksu = wksc;
172200170406             VS3ksud = ACOrag;
172300170406         //?Se non torna errore cerco la ragione sociale dell'unificane
172400170406           ELSE;
172500170406             clear TIBS69DS;
172600170406             clear ACOrag;
172700170406             I69kac = D10cop;
172800170406             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
172900170406             VS3ksu = D10cop;
173000170406             VS3ksud = ACOrag;
173100170406           ENDIF;
173200170406         ENDSL;
173300170405
173400170405         S03nrr += 1;
173500170405
173600170405       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
173700170405         IF  S03nrr >= 9999;
173800170406           wMaxNrr = *on;
173900170405           EndS03 = *on;
174000170405           leavesr;
174100170405         ENDIF;
174200170405
174300170405         write MKP0S03;
174400170405
174500170405       ENDSR;
174600170406
174700170406       //--------------------------------------------------------------
174800170406       //?Gestione tasto funzionale F06 da videata S03
174900170406       //?F06=Ritorno
175000170406       //--------------------------------------------------------------
175100170406       BEGSR F06S03;
175200170406
175300170406         readc MKP0S03;
175400170406
175500170406         DOW  not %eof(TRMKP0D);
175600170406           SflNxtChg = *off;
175700170406           V03rcd    = S03nrr;
175800170406           PosCurOPZ = *off;
175900170410
176000170410         //?Solo per opzione 'P'
176100170410           IF  VS3opz = 'P';
176200170406
176300170410           //?Non posso aggiornare se c'è già una particolarità
176400170410             IF  VS3gva <> *blanks;
176500170410               ErrGenerico = *on;
176600170410               ErrMessage  = *on;
176700170410               PosCurOPZ   = *on;
176800170410               VS3opz = 'E';
176900170410               V03msg = %trim(Msg(06)) + '. Presente altra particolarità';
177000170410             ENDIF;
177100170410           //?Se non ho errore eseguo l'opzione immessa
177200170410             IF  not ErrMessage and VS3opz <> *blanks;
177300170410               chain (VS3ksc:VS3ctr:VS3prg) TNTAM04L;
177400170410               IF  not %found(TNTAM04L);
177500170410                 ErrMessage  = *on;
177600170410                 ErrGenerico = *on;
177700170410                 PosCurOPZ   = *on;
177800170410                 VS3opz = 'E';
177900170410               ENDIF;
178000170410               IF  %found(TNTAM04L);
178100170410                 TAMgva = 'P';
178200170410                 TAMduv = Oggi;
178300170410                 update TNTAM000;
178400170410                 PosCurOPZ = *off;
178500170410                 clear VS3opz;
178600170410               ENDIF;
178700170410             ENDIF;
178800170410           ENDIF;
178900170410
179000170410         //?Errore per altra opzione
179100170410           IF  VS3opz <> 'P' and VS3opz <> *blanks;
179200170410             ErrGenerico = *on;
179300170410             ErrMessage  = *on;
179400170410             PosCurOPZ   = *on;
179500170410             VS3opz = 'E';
179600170410             V03msg = %trim(Msg(06)) + '. Per F6 ammessa solo opzione "P"';
179700170410           ENDIF;
179800170406
179900170406           //?Aggiornamento sfl
180000170406           V03csr = V03rcd;
180100170406           update MKP0S03;
180200170406           readc MKP0S03;
180300170406         ENDDO;
180400170406
180500170406       //?Se non ci sono stati errori ricarico
180600170406         IF  not ErrMessage;
180700170406           Video = 'S3';
180800170406           wInzS03 = *on;
180900170406         ENDIF;
181000170406
181100170406       ENDSR;
181200170405
181300170405       //--------------------------------------------------------------
181400170405       //?Gestione tasto funzionale F12 da videata S03
181500170405       //?F12=Ritorno
181600170405       //--------------------------------------------------------------
181700170405       BEGSR F12S03;
181800170405
181900170405       //?Ritorno alle selezioni
182000170405         Video = 'D2';
182100170405         clear sav_s03nrr;
182200170405
182300170405       ENDSR;
182400170405
182500170405       //--------------------------------------------------------------
182600170406       //?Ordinamento Subfile S03.
182700170405       //--------------------------------------------------------------
182800170406       BEGSR Ordina;
182900170405
183000170405       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
183100170405       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
183200170405       // il campo è usato per selezionare i records dando un ordine a quelli
183300170405       // selezionati davanti ai non selezionati.
183400170405         clear QLGSCB;
183500170405         clear QLGSCB00;
183600170405
183700170406       // 5 campi chiave x Commerciale - KSU - KSC - TAR - PRG
183800170406         QLGNBRK = 5;
183900170405
184000170405       // imposto la posizione del CMM sul subfile e la lunghezza
184100170405       // l'ordinamento è su un campo numerico e deve essere ascendente
184200170405         QLGSP = 1;
184300170406         QLGSS = %SIZE(VS3cmm);
184400170405         QLGDT = Numerico;
184500170405         QLGSO = Ascendente;
184600170405         QLGKL(1) = QLGSKL;
184700170405
184800170405       // imposto la posizione del KSU sul subfile e la lunghezza
184900170405       // l'ordinamento è su un campo numerico e deve essere ascendente
185000170406         QLGSP = 1 + %size(VS3cmm);
185100170406         QLGSS = %SIZE(VS3ksu);
185200170405         QLGDT = Numerico;
185300170405         QLGSO = Ascendente;
185400170405         QLGKL(2) = QLGSKL;
185500170405
185600170405       // imposto la posizione del KSC sul subfile e la lunghezza
185700170405       // l'ordinamento è su un campo numerico e deve essere ascendente
185800170406         QLGSP = 1 + %size(VS3cmm) + %size(VS3ksu);
185900170405         QLGSS = %SIZE(VS3ksc);
186000170405         QLGDT = Numerico;
186100170405         QLGSO = Ascendente;
186200170405         QLGKL(3) = QLGSKL;
186300170405
186400170405       // imposto la posizione del CTR sul subfile e la lunghezza
186500170405       // l'ordinamento è su un campo numerico e deve essere ascendente
186600170406         QLGSP = 1 + %size(VS3cmm) + %size(VS3ksu) +
186700170405                     %size(VS3ksc);
186800170405         QLGSS = %SIZE(VS3ctr);
186900170405         QLGDT = Numerico;
187000170405         QLGSO = Ascendente;
187100170405         QLGKL(4) = QLGSKL;
187200170405
187300170405       // imposto la posizione del PRG sul subfile e la lunghezza
187400170405       // l'ordinamento è su un campo numerico e deve essere ascendente
187500170406         QLGSP = 1 + %size(VS3cmm) + %size(VS3ksu) +
187600170405                     %size(VS3ksc) + %size(VS3ctr);
187700170405         QLGSS = %SIZE(VS3prg);
187800170405         QLGDT = Numerico;
187900170405         QLGSO = Ascendente;
188000170405         QLGKL(5) = QLGSKL;
188100170405
188200170405       // Load other sort parameters.
188300170405         QLGLB = 80 + 16 * MaxKey;
188400170405         QLGRL = %SIZE(sflrcd) - 1;
188500170405         QLGRT = 8;
188600170405         QLGOKL = 80;
188700170405         QLGLKE = 16;
188800170405         QLGLSS = 290;
188900170405
189000170405       // Initialize Sort I/O API fields.
189100170405         QLGRL00 = QLGRL;
189200170405         QLGRC00 = 1;
189300170405         clear QUSEI;
189400170405         QUSBPRV = %SIZE(QUSEC);
189500170405
189600170405       // First step - Initialize the sort routine.
189700170405         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
189800170405
189900170405       // Next step - Write records to I/O routine.
190000170405         QLGRT00 = Put;
190100170405         FOR  xx = 1 to rrnlast;
190200170405           chain xx MKP0S03;
190300170405
190400170405       // solo le righe con Selected = 'Y' sono riordinate,
190500170405       // quindi per fare un ordinamento di tutte le righe
190600170405       // metto 'Y' sempre.
190700170405           selected  = 'Y';
190800170405           clear QUSEI;
190900170405           QUSBPRV = %SIZE(QUSEC);
191000170405           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
191100170405         ENDFOR;
191200170405
191300170405       // Next step - Signal end of input, clear subfile for reload.
191400170405         QLGRT00 = EndPut;
191500170405         clear QUSEI;
191600170405         QUSBPRV = %SIZE(QUSEC);
191700170405         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
191800170405       // pulizia SFL
191900170405         exsr PulS03;
192000170405
192100170405       // Final step - Write the records back to the subfile.
192200170405         QLGRT00 = Get;
192300170405         FOR  xx = 1 to rrnlast;
192400170405           clear QUSEI;
192500170405           QUSBPRV = %SIZE(QUSEC);
192600170405           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
192700170405           s03nrr = xx;
192800170405           write MKP0S03;
192900170405         ENDFOR;
193000170405
193100170405         rrnlast = S03nrr;
193200170405         IF  S03nrr > *zeros;
193300170405           V03rcd = 1;
193400170405           V03csr = 1;
193500170405         ELSE;
193600170405           clear V03rcd;
193700170405         ENDIF;
193800170405
193900170405       ENDSR;
194000170405
194100170405
194200170405       //--------------------------------------------------------------
194300170405       //?Gestione opzioni Subfile S03.
194400170405       //--------------------------------------------------------------
194500170405       BEGSR OpzS03;
194600170405
194700170405         readc MKP0S03;
194800170405
194900170405         DOW  NOT %eof(TRMKP0D);
195000170405
195100170405           SflNxtChg = *off;
195200170405           WindDspF = IndDspF;
195300170405           reset WindDspFb;
195400170405           IndDspF = WindDspF;
195500170405           V03rcd  = S03nrr;
195600170405
195700170405           PosCurOPZ = *off;
195800170405           sav_S03nrr = S03nrr;
195900170405
196000170405         //?- Verifico se il cliente è gestibile dall'utente
196100170405           IF  VS3opz <> *blank;
196200170405             clear TNTAA1DS;
196300170405             ITAA1caut = '§utegtc';
196400170405             ITAA1ksc = VS3ksc;
196500170405             kpjbu = TNTAA1DS;
196600170405             tntaa1c (kpjba);
196700170405             TNTAA1DS = kpjbu;
196800170405             IF  OTAA1fabi = 'N';
196900170405               ErrGenerico = *on;
197000170405               ErrMessage  = *on;
197100170405               PosCurOPZ   = *on;
197200170406               V03msg = %trim(Msg(05)) + '. Non in gestione';
197300170405             ENDIF;
197400170405           ENDIF;
197500170405
197600170405         //?- Se ok il cliente
197700170410           IF  not ErrMessage and
197800170410           //?- Non accetto F6 su opzione diversa da 'P'
197900170410               VS3opz <> *blanks and VS3opz <> 'P' and
198000170410               dsp_aid = c_F06;
198100170410             ErrGenerico = *on;
198200170410             ErrMessage  = *on;
198300170410             PosCurOPZ   = *on;
198400170410             V03msg = %trim(Msg(06)) + '. Per F6 ammessa solo opzione "P"';
198500170410           ENDIF;
198600170410
198700170410         //?- Se ok il tutto
198800170410           IF  not ErrMessage;
198900170405
199000170405             SELECT;
199100170405           //?- Nessuna opzione
199200170405             WHEN  VS3opz = *blank;
199300170406
199400170406           //?- I = Interrogazione Clienti
199500170406             WHEN  VS3opz = 'I';
199600170406               exsr OpzioneI;
199700170406
199800170406           //?- P = Aggiorna Tariffa
199900170406             WHEN  VS3opz = 'P';
200000170406               exsr OpzioneP;
200100170406
200200170406           //?- S = Statistica
200300170406             WHEN  VS3opz = 'S';
200400170406               exsr OpzioneS;
200500170405
200600170405           //?- T = Tariffa
200700170405             WHEN  VS3opz = 'T';
200800170405               exsr OpzioneT;
200900170405
201000170405             OTHER;
201100170405               ErrMessage  = *on;
201200170405               ErrGenerico = *on;
201300170405               PosCurOPZ   = *on;
201400170406               V03msg = Msg(06);
201500170405             ENDSL;
201600170405           ENDIF;
201700170405
201800170407         //?Aggiornamento sfl
201900170407           SflNxtChg = *on;
202000170407           V03csr = V03rcd;
202100170411           IF  not ErrMessage and dsp_aid <> c_F06;
202200170407             clear VS3opz;
202300170407           ENDIF;
202400170405           update MKP0S03;
202500170405
202600170405           IF  ErrMessage or ErrGenerico;
202700170405             leave;
202800170405           ENDIF;
202900170405
203000170405           readc MKP0S03;
203100170405
203200170405         ENDDO;
203300170405
203400170405       ENDSR;
203500170406
203600170406       //--------------------------------------------------------------
203700170406       //?Opzione "I" videata S03.
203800170406       //--------------------------------------------------------------
203900170406       BEGSR OpzioneI;
204000170406
204100170406         clear TNTAI2DS;
204200170406         ITAI2ksc = VS3ksc;
204300170406         tntai2r (kpjba:TNTAI2DS);
204400170406
204500170406       ENDSR;
204600170406
204700170406       //--------------------------------------------------------------
204800170406       //?Opzione "P" videata S03.
204900170406       //--------------------------------------------------------------
205000170406       BEGSR OpzioneP;
205100170406
205200170406       //?- Se c'è già una particolarità errore
205300170406         IF  VS3gva <> *blanks;
205400170406           ErrGenerico = *on;
205500170406           ErrMessage  = *on;
205600170406           PosCurOPZ   = *on;
205700170406           V03msg = %trim(Msg(06)) + '. Presente altra particolarità';
205800170406           leavesr;
205900170406         ENDIF;
206000170411
206100170411       //?- Se opzione 'P' devo fare F6
206200170411         IF  dsp_aid <> c_F06;
206300170411           ErrGenerico = *on;
206400170411           ErrMessage  = *on;
206500170411           PosCurOPZ   = *on;
206600170411           V03msg = 'Per opzione "P" fare F6';
206700170411           leavesr;
206800170411         ENDIF;
206900170406
207000170406       ENDSR;
207100170406
207200170406       //--------------------------------------------------------------
207300170406       //?Opzione "S" videata S03.
207400170406       //--------------------------------------------------------------
207500170406       BEGSR OpzioneS;
207600170406
207700170406       //?- Verifico se l'opzione è utilizzabile dall'utente
207800170406         clear TNTAA1DS;
207900170406         ITAA1caut = '§uteist';
208000170406         ITAA1ksc = VS3ksc;
208100170406         kpjbu = TNTAA1DS;
208200170406         tntaa1c (kpjba);
208300170406         TNTAA1DS = kpjbu;
208400170406         IF  OTAA1fabi = 'N';
208500170406           ErrGenerico = *on;
208600170406           ErrMessage  = *on;
208700170406           PosCurOPZ   = *on;
208800170406           V03msg = %trim(Msg(05)) + '. Non in gestione';
208900170406           leavesr;
209000170406         ENDIF;
209100170406
209200170406         clear TISE60DS;
209300170406         D60op0 = 'VIS';
209400170406         D60op1 = 'F06';
209500170406         D60sce = '1';
209600170406         dsKSC = VS3ksc;
209700170406         skKSA(1) = dsKSA;
209800170406         kpjbu = TISE60DS;
209900170406         tise61r (kpjba);
210000170406
210100170406       ENDSR;
210200170405
210300170405       //--------------------------------------------------------------
210400170405       //?Opzione "T" videata S03.
210500170405       //--------------------------------------------------------------
210600170405       BEGSR OpzioneT;
210700170405
210800170405       //?Verifico che la tariffa non sia annullata
210900170406         chain (VS3ksc:VS3ctr:VS3prg) TNTAM04L;
211000170406         IF  not %found(TNTAM04L);
211100170405           ErrMessage  = *on;
211200170405           ErrGenerico = *on;
211300170405           PosCurOPZ   = *on;
211400170406           V03msg = Msg(09);
211500170405           leavesr;
211600170405         ENDIF;
211700170405
211800170405         clear TA02R2ds;
211900170405         TA02ksc = VS3ksc;
212000170405         TA02ctr = VS3ctr;
212100170405         TA02prg = VS3prg;
212200170405         TA02ccc = DUTkci;
212300170406         TA02rgs = VS3kscd;
212400170405         clear kpjbu;
212500170405         kpjbu = TA02R2ds;
212600170405         tnta02r2 (kpjba);
212700170405
212800170405       ENDSR;
212900170406
213000170406       //--------------------------------------------------------------
213100170406       //?Gestione videata D04.
213200170406       //--------------------------------------------------------------
213300170406       BEGSR GesD04;
213400170406
213500170406       //?Emissione videata
213600170406         write MKP0T01;
213700170406         exfmt MKP0D04;
213800170406         reset ErrMessage;
213900170406         reset ErrGenerico;
214000170406
214100170406         SELECT;
214200170406
214300170406       //?- F03=Fine
214400170406           WHEN  dsp_aid = c_F03;
214500170406             exsr F03D02;
214600170406
214700170406       //?- F12=Ritorno
214800170406           WHEN  dsp_aid = c_F12;
214900170406             exsr F12S03;
215000170406
215100170406         ENDSL;
215200170406
215300170406       ENDSR;
215400170405
215500170405       //--------------------------------------------------------------
215600170405       //?Carico sk Commerciali
215700170405       //--------------------------------------------------------------
215800170405       BEGSR CarskAGE;
215900170405
216000170405         wEnd = *off;
216100170405         clear xx;
216200170405
216300170405         exec sql declare WRKAGE cursor for
216400170405                  SELECT CMMcod from AZCMM00F
216500170405                  WHERE CMMuni = :w0070;
216600170405         exec sql open WRKAGE;
216700170405
216800170405         IF  sqlcode < 0;
216900170405           exec sql close WRKAGE;
217000170405           leavesr;
217100170405         ENDIF;
217200170405
217300170405         DOW  not wEnd;
217400170405           exec sql
217500170405           fetch next from WRKAGE into :CMMcod;
217600170405           IF  sqlcod = 100 or sqlcod < 0;
217700170405             leave;
217800170405           ENDIF;
217900170405
218000170405         //?Carico i commerciali in sk
218100170405           xx += 1;
218200170405           skAge(xx) = CMMcod;
218300170405
218400170405         ENDDO;
218500170405
218600170405         exec sql close WRKAGE;
218700170405
218800170405       ENDSR;
218900170405
219000170405       //--------------------------------------------------------------
219100170405       //?Operazioni finali.
219200170405       //--------------------------------------------------------------
219300170405       BEGSR RoutEnd;
219400170405
219500170405         *inLR = *on;
219600170405         return;
219700170405
219800170405       ENDSR;
219900170405
220000170405      /end-free
220100170405
220200170405      *--------------------------------------------------
220300170407      * Procedure name: IsItalia
220400170405      * Purpose:        Ritorna se Cliente Italia
220500170405      * Returns:        On - OFF
220600170405      * Parameter:      Filiale
220700170405      *--------------------------------------------------
220800170407     p IsItalia        B
220900170407     d IsItalia        PI              n
221000170405     d  Filiale                       3s 0 CONST
221100170405
221200170405      // ds per campo ORGde3
221300170405     d OG143         e ds                  QUALIFIED INZ
221400170405
221500170405      /free
221600170405
221700170405       IF  filiale = *zeros;
221800170405         RETURN *off;
221900170405       ENDIF;
222000170405
222100170405       chain (filiale) AZORG01L;
222200170405       IF  not %found(AZORG01L);
222300170405         RETURN *off;
222400170405       ENDIF;
222500170405       IF  ORGfva <> *blanks;
222600170405         RETURN *off;
222700170405       ENDIF;
222800170405       IF  ORGfag <> 'F' and ORGfag <> 'A';
222900170405         RETURN *off;
223000170405       ENDIF;
223100170405
223200170405       OG143 = ORGde3;
223300170406       IF  og143.§OGntw = 'DPD' or og143.§OGntw = 'EEX' or
223400170405           og143.§OGntw = 'FED';
223500170405         RETURN *off;
223600170405       ENDIF;
223700170405
223800170405       RETURN *on;
223900170405
224000170405      /end-free
224100170407     P IsItalia        E
224200170405
224300170405       //--------------------------------------------------------------
224400170405       //?Schiere a tempo di compilazione.
224500170405       //--------------------------------------------------------------
224600170405** -- MSG -------------------------------------------------------------------*
224700170405Utente non abilitato alla Funzione.                                            1
224800170406Almeno 1 tra Filiale - Commerciale - Cliente deve essere inserito              2
224900170406Filiale Errata                                                                 3
225000170406Commerciale Unificante Errato                                                  4
225100170406Codice Cliente Errato                                                          5
225200170406Opzione errata                                                                 6
225300170406Immettere il Cliente o il Cliente Unificante                                   7
225400170406Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante        8
225500170406Tariffa non trovata                                                            9
225600170406Codice Importanza Clienti Errato                                              10
225700170406Manca tabella di controllo scadenza password. TEL CED SEDE!!!!!!              11
225800170406Manca tabella PASSWORD. TEL CED SEDE!!!!!!                                    12
225900170406Password errata o non immessa                                                 13
226000170406Password scaduta                                                              14
226100170410Impostare la percentuale SOLO per E-Commerce "Sì"                             15
