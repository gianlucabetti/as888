000100141028      //--------------------------------------------------------------
000200150112      //?TRKC01R - GESTIONE CAMPAGNE COMMERCIALI
000300141028      //--------------------------------------------------------------
000400141028
000500141028     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141029     h dftactgrp(*no) actgrp(*caller)
000700141028
000800141028      //---------------------------------------------------------------
000900141028      //?Dichiarazione file.
001000141028      //---------------------------------------------------------------
001100141024
001200141024      // - Organigramma
001300141024     fAZORG01L  if   e           k disk
001400141029     fAZORG02L  if   e           k disk    rename(AZORG:AZORG02)
001500141029
001600141029      // - Anagrafica Commerciali
001700141029     fAZCMM01L  if   e           k disk
001800141024
001900141024      // - Tabelle
002000141024     fTABEL00F  if   e           k disk
002100141028
002200141027      // - File Fasi avanzamento Campagna
002300141024     fTICMF01L  if   e           k disk
002400141114
002500141114      // - File anagrafiche Campagne
002600141114     fTICMP01L  if   e           k disk
002700141028
002800141024      // - Video Gestione Campagne
002900150112     fTRKC01D   cf   e             workstn
003000150112     f                                     sfile(KC01S03 : S03nrr)
003100150116     f                                     sfile(KC01SW6 : S06nrr)
003200150128     f                                     sfile(KC01SW8 : S08nrr)
003300150302     f                                     sfile(KC01SW9 : S09nrr)
003400141028     f                                     indds(IndDspF)
003500141028     f                                     infds(InfDspF)
003600141028
003700141028      //---------------------------------------------------------------
003800141028      //?Definizione costanti.
003900141028      //---------------------------------------------------------------
004000141028
004100141028      // - Tasti funzionali a video
004200141028     d c_F01           c                   const(x'31')
004300141028     d c_F02           c                   const(x'32')
004400141028     d c_F03           c                   const(x'33')
004500141028     d c_F04           c                   const(x'34')
004600141028     d c_F05           c                   const(x'35')
004700141028     d c_F06           c                   const(x'36')
004800141028     d c_F07           c                   const(x'37')
004900141028     d c_F08           c                   const(x'38')
005000141028     d c_F09           c                   const(x'39')
005100141028     d c_F10           c                   const(x'3A')
005200141028     d c_F11           c                   const(x'3B')
005300141028     d c_F12           c                   const(x'3C')
005400141028     d c_F13           c                   const(x'B1')
005500141028     d c_F14           c                   const(x'B2')
005600141028     d c_F15           c                   const(x'B3')
005700141028     d c_F16           c                   const(x'B4')
005800141028     d c_F17           c                   const(x'B5')
005900141028     d c_F18           c                   const(x'B6')
006000141028     d c_F19           c                   const(x'B7')
006100141028     d c_F20           c                   const(x'B8')
006200141028     d c_F21           c                   const(x'B9')
006300141028     d c_F22           c                   const(x'BA')
006400141028     d c_F23           c                   const(x'BB')
006500141028     d c_F24           c                   const(x'BC')
006600141028     d c_Enter         c                   const(x'F1')
006700141028     d c_RollDown      c                   const(x'F4')
006800141028     d c_RollUp        c                   const(x'F5')
006900141028
007000141028     d Digits          c                   const('0123456789')
007100141029
007200141029     d FaseObj         c                   const(' 10')
007300141111     d FaseObjProp     c                   const(' 20')
007400141111     d FaseObjFine     c                   const(' 30')
007500150220     d FaseObjTf       c                   const(' TF')
007600141113     d FaseObjTtr      c                   const(' TR')
007700141113     d FaseObjCf       c                   const(' CF')
007800141114     d FaseChiudi      c                   const(' 90')
007900150128
008000150128     d Causale         c                   const('NA')
008100141030
008200141030     d visHI           c                   const(X'22')
008300141030     d visRI           c                   const(X'21')
008400141112
008500141117     d ObjAll          c                   const('Tutti')
008600150128     d ObjInizio       c                   const('Inizio')
008700141117     d ObjProposto     c                   const('Proposto')
008800141126     d ObjFinale       c                   const('Finale')
008900150127
009000150127     d ValoriFormula   c                   const('><=')
009100141030
009200141028      //---------------------------------------------------------------
009300141028      //?Definizione schiere.
009400141028      //---------------------------------------------------------------
009500141027
009600141027      // - Sk Aree
009700141104     d skAree          s              3a   dim(999) inz
009800141029     d skDesArea       s                   like(ORGdes) dim(999) inz
009900141027
010000141027      // - Sk Distretti
010100141105     d skDistretti     s                   like(ORGfl3) dim(99) inz
010200141029     d skDesDist       s                   like(§17des) dim(99) inz
010300141024
010400141024      // - Sk Cod.Importanza clienti da tabella IC
010500141024     d skIC            s              1    dim(10)
010600141117     d skIC_ord        s              1  0 dim(10)
010700141103
010800141103      // - Sk Clienti per interrogazione Delta
010900141103     d skKSC           s              7  0 dim(28)
011000150116
011100150116      // - Sk Formule
011200150116     d Formula         s              2a   dim(06) ctdata perrcd(1)
011300150121     d DesForm         s             20a   dim(06) alt(Formula)
011400150128
011500150128      // - Sk Obiettivo/Andamento
011600150128     d Obiettivo       s              1a   dim(05) ctdata perrcd(1)
011700150128     d DesObj          s             35a   dim(05) alt(Obiettivo)
011800150302
011900150302      // - Sk Trattative
012000150302     d Trattative      s              2a   dim(10) ctdata perrcd(1)
012100150302     d DesTtr          s             30a   dim(10) alt(Trattative)
012200141028
012300141028      // - Messaggi di errore
012400141124     d Msg             s             78    dim(30) ctdata perrcd(1)
012500141028
012600141028      //---------------------------------------------------------------
012700141028      //?Definizione aree dati.
012800141028      //---------------------------------------------------------------
012900141028
013000141028      // - Dati utente
013100141028     d §AzUte        e ds                  extname(AZUTE00F)
013200141028     d                                     dtaara
013300141028     d §DatiUte      e ds                  extname(dDatiUte)
013400141028     d                                     dtaara
013500141028
013600141028      //---------------------------------------------------------------
013700141028      //?Definizione strutture dati.
013800141028      //---------------------------------------------------------------
013900141028
014000141028      // - Status
014100141028     d Psds           sds
014200141028     d   SDSpgm          *proc
014300141028
014400141028      // - InfDS
014500141028     d InfDspF         ds
014600141028     d  dsp_aid              369    369a
014700141028     d  sfl_rrn              376    377i 0
014800141028     d  min_nrr              378    379i 0
014900141028     d  num_rcds             380    381i 0
015000141028
015100141028      // - Indicatori su DspF
015200141028     d IndDspF         ds
015300141111        // - Indicatori di Abilitazione Tasti
015400141111     d  AbilitaF10                    1n   overlay(IndDspF : 10)
015500141127     d  AbilitaF13                    1n   overlay(IndDspF : 13)
015600141024        // - Indicatori di Visualizzazione Campi
015700141024     d  VisArea                       1n   overlay(IndDspF : 26)
015800141024     d  VisDistretto                  1n   overlay(IndDspF : 27)
015900141028        // - Indicatori di errore in videata
016000141028     d  ErrMessage                    1n   overlay(IndDspF : 28)
016100141028        // - Indicatori di gestione del subfile
016200141029     d  SflDsp                        1n   overlay(IndDspF : 30)
016300141029     d  SflDspCtl                     1n   overlay(IndDspF : 31)
016400141028     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016500141028     d  SflEnd                        1n   overlay(IndDspF : 33)
016600141030        // - Altri indicatori di visualizzazione campi
016700141111     d  VisOrdRAG                     1n   overlay(IndDspf : 35)
016800141030     d  VisOrdCLV                     1n   overlay(IndDspf : 36)
016900141121     d  VisEsito                      1n   overlay(IndDspf : 37)
017000141028        // - Indicatori di errore
017100141024     d  PosCurNCM                     1n   overlay(IndDspF : 50)
017200141027     d  PosCurCDI                     1n   overlay(IndDspF : 51)
017300141027     d  PosCurCAR                     1n   overlay(IndDspF : 52)
017400141027     d  PosCurFIL                     1n   overlay(IndDspF : 53)
017500141027     d  PosCurCMM                     1n   overlay(IndDspF : 54)
017600141107     d  PosCurKSU                     1n   overlay(IndDspF : 55)
017700141028     d  PosCurCLV1                    1n   overlay(IndDspF : 56)
017800141028     d  PosCurCLV2                    1n   overlay(IndDspF : 57)
017900141028     d  PosCurCLV3                    1n   overlay(IndDspF : 58)
018000141028     d  PosCurCLV4                    1n   overlay(IndDspF : 59)
018100141028     d  PosCurCLV5                    1n   overlay(IndDspF : 60)
018200141028     d  PosCurDELda                   1n   overlay(IndDspF : 61)
018300141028     d  PosCurDELa                    1n   overlay(IndDspF : 62)
018400141028     d  PosCurOPZ                     1n   overlay(IndDspF : 65)
018500141127     d  PosCurOBJ1                    1n   overlay(IndDspF : 66)
018600141127     d  PosCurOBJ2                    1n   overlay(IndDspF : 67)
018700150119     d  PosCurFORM                    1n   overlay(IndDspF : 68)
018800150213     d  PosCurOBJ3                    1n   overlay(IndDspF : 69)
018900150213     d  PosCurOBJ3DA                  1n   overlay(IndDspF : 70)
019000150302     d  PosCurTTR                     1n   overlay(IndDspF : 71)
019100141028        // - Indicatori di errore generico
019200141028     d  ErrGenerico                   1n   overlay(IndDspF : 99)
019300141028
019400141028     d WindDspF        ds                  inz
019500141028     d  WindDspFa              1     49    inz(*zero)
019600141028     d  WindDspFb             50     99    inz(*zero)
019700141028
019800141028      // - Parametri ricevuti
019900141028     d KPJBA         e ds
020000141105
020100150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
020200150112     d TRKC02DS      e ds
020300141107
020400150112      // - Richiamo pgm TRKC03R - Storico Fasi
020500150112     d TRKC03DS      e ds
020600141111
020700150112      // - Richiamo pgm TRKC06R - Aggiunta Cliente in campagna
020800150112     d TRKC06DS      e ds
020900141114
021000150112      // - Richiamo pgm TRKC07R - Variazione Obiettivo
021100150112     d TRKC07DS      e ds
021200150127
021300150127      // - Richiamo pgm TRKC10R - Gestione Note
021400150127     d TRKC10DS      e ds
021500141127
021600141127      // - Scrittura fase
021700150112     d TRKC71DS      e ds                  inz
021800141114
021900150115      // - Verifica quale obiettivo variare
022000150112     d TRKC72DS      e ds                  inz
022100150218
022200150218      // - Dati da Visualizzare o Parzializzazioni Trattative
022300150218     d TRKC76DS      e ds                  inz
022400141114
022500141114      // - File Clienti in Campagna Commerciale
022600141114     d TICMC00F      e ds                  extname(TICMC00F)
022700141028
022800141028      // - File INFO Clienti in Campagna Commerciale
022900141028     d TICMI00F      e ds                  extname(TICMI00F)
023000150115
023100150115      // - Ricerca/Controllo tabelle
023200150115     d TIBS02DS      e ds                  inz
023300141028
023400141028      // - Reperimento dati utente
023500141027     d TIBS34DS      e ds
023600141027
023700141027      // - Reperimento dati Anagrafica Clienti
023800141027      /copy gaitrasrc/srcprotopi,TIBS69R
023900141103
024000141103      // - Delta cliente unificante
024100141103     d TISE60DS      e ds
024200141103     d  skKSA                 50    161    dim(28)
024300141024
024400141024      // - Controllo abilitazioni
024500141024     d TNTAA1DS      e ds                  inz
024600141027
024700141027      // - Ricerca Anagrafica Clienti
024800141027     d TNTAI1DS      e ds                  inz
024900141103
025000141103      // - Int. Anagrafica Clienti
025100141103     d TNTAI2DS      e ds                  inz
025200141103
025300141103      // - Gestione trattative commerciali
025400141103     d TNTA88DS      e ds                  inz
025500141024
025600141024      // - Reperimento filiali gestite dall'utente
025700141028     d TRUL31DS      e ds
025800141029     d  POG                   10    759    DIM(250)
025900141027     d TRUL31DS2     e ds
026000141029     d  DIS                    1     20    DIM(20)
026100141029     d  CAR                   22    171    DIM(50)
026200150115
026300150115      // - Tabella ECM - Causali chiusura campagna
026400150115     d dECM          e ds
026500141027
026600141027      // - Tabella 05 - Aree
026700141027     d ds05          e ds
026800141027
026900141027      // - Tabella 17 - Distretti
027000141027     d ds17          e ds
027100141117
027200141117      // - Tabella IC - Importanza Clienti
027300141117     d dsIC          e ds
027400141103
027500141103      // - DS per passaggio clienti PACKED per int. delta
027600141103     d                 ds
027700141126     d  dsKSC                  1      4p 0
027800141103     d  dsKSA                  1      4
027900141028
028000141028      //---------------------------------------------------------------
028100141028      //?Definizione variabili globali.
028200141028      //---------------------------------------------------------------
028300141028
028400141028      // - Flags booleani
028500141024     d EndS03          s               n   inz(*off)
028600141024     d ErrGrave        s               n   inz(*off)
028700141024     d Fine            s               n   inz(*off)
028800141119     d Primo           s               n   inz(*off)
028900141124     d RecOK           s               n   inz(*off)
029000150116     d TroppiRcd       s               n   inz(*off)
029100141028     d wEnd            s               n   inz(*off)
029200141113     d wclv            s               n   inz(*off)
029300141029     d wInzD02         s               n   inz(*off)
029400141030     d wInzD04         s               n   inz(*off)
029500141118     d wInzD05         s               n   inz(*off)
029600150128     d wInzD07         s               n   inz(*off)
029700141029     d wInzS03         s               n   inz(*off)
029800150116     d wInzS06         s               n   inz(*off)
029900150128     d wInzS08         s               n   inz(*off)
030000150302     d wInzS09         s               n   inz(*off)
030100141103     d wMaxNrr         s               n   inz(*off)
030200141103     d wokTtr          s               n   inz(*off)
030300150218     d wTRKC76         s               n   inz(*off)
030400141028
030500141028      // - Indici di schiera
030600141029     d xx              s              4s 0 inz
030700141028
030800141028      // - Campi associati al video
030900141029     d Video           s              2a   inz('D2')
031000141029     d S03nrr          s              4s 0 inz
031100150116     d S06nrr          s              4s 0 inz
031200150128     d S08nrr          s              4s 0 inz
031300150302     d S09nrr          s              4s 0 inz
031400141112     d sav_S03nrr      s              4s 0 inz
031500141028
031600141028       // - Stringa SQL da eseguire
031700141028     d wSQL            s           2048    Varying        inz
031800141028
031900141028      // - Campi di comodo data
032000141024     d Data_EUR        s               d   datfmt(*eur)
032100141024     d Data_ISO        s               d   datfmt(*iso)
032200150203     d wData           s              8s 0 inz
032300141028
032400141028      // - Campi di comodo
032500141029     d Oggi            s              8s 0 inz
032600150128     d savformula      s                   like(V02formula) inz
032700141027     d wabi            s                   like(OTAA1cabi) inz
032800141112     d wdela           s                   like(CMIpde) inz
032900141112     d wdelda          s                   like(CMIpde) inz
033000150220     d wfase           s                   like(CMFacm) inz
033100150303     d wflgp           s                   like(IKC76flgp) inz
033200150128     d wfobj           s                   like(V02obj) inz
033300150128     d wfobj1          s                   like(V02obj1) inz
033400150128     d wfobj2          s                   like(V02obj2) inz
033500150213     d wfobj3          s                   like(V02obj3) inz
033600150128     d wfformula       s                   like(V02formula) inz
033700141029     d wimp            s             13s 2 inz
033800141030     d wmsg            s             78a   inz
033900150213     d wobj3da         s                   like(CMFpea) inz
034000150213     d wobj3a          s                   like(CMFpea) inz
034100150128     d wora            s                   like(CMFhfc) inz
034200150203     d wperc           s                   like(CMFpea) inz
034300150303     d wtpric          s                   like(IKC76ric) inz
034400150128     d wvalobj1        s                   like(CMFpea) inz
034500150128     d wvalobj2        s                   like(CMFpea) inz
034600150213     d wvalobj3        s                   like(CMFpea) inz
034700141030     d w001a           s              1a   inz
034800150302     d w002a           s              2a   inz
034900150218     d w0020           s              2s 0 inz
035000141030     d w003a           s              3a   inz
035100141029     d w0030           s              3s 0 inz
035200141030     d w007a           s              7a   inz
035300141029     d w0070           s              7s 0 inz
035400141030     d w050a           s             50a   inz
035500141024
035600141024      // - Campi x ricerca tabelle TABEL
035700141024     d idTabella       s              2
035800141024     d Ordinamento     s              1
035900141024     d idElemento      s              8
036000141024     d TastoUscita     s              1
036100141027
036200141027       // - Parametri per ricerca Filiale
036300141027     d pinFIL          s              3
036400141027     d pinFAG          s              1
036500141027     d pinDES          s             25
036600141027     d pinDIT          s              3
036700141027
036800141027      // - Parametri per ricerca Commerciale Unificante
036900141029      /copy gaitrasrc/srcprotopi,TRMK43R_1
037000141028
037100141028      // ----------------------------------------------------------------------
037200141027      //?DATI per ordinamento subfile
037300141028      // ----------------------------------------------------------------------
037400141028     d MaxKey          c                   5
037500141028     d Ascendente      c                   1
037600141028     d Discendente     c                   2
037700141028     d Carattere       c                   6
037800141028     d Numerico        c                   8
037900141028     d Put             c                   1
038000141028     d EndPut          c                   2
038100141028     d Get             c                   3
038200141028      *************************************************************************
038300141028      * Campi utili:
038400141028      *     nrr        - Numero relativo di record del Subfile
038500141028      *     SizeList   - Dimensione della lista
038600141028      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
038700141028      *************************************************************************
038800141028     d NotUsed         s             16A
038900141028     d ReturnSize      s              9B 0
039000141028     d SizeList        s              9B 0
039100141028     d RrnLast         s              5I 0
039200141028     d WrkSort         s              1  0 inz(0)
039300141028      *************************************************************************
039400141028      * Data Structures
039500141028      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
039600141028      *     QLGSCB     - The sort request block for the QLGSORT API
039700141028      *     QLGSCB00   - The sort request block for the QLGSRTIO API
039800141028      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
039900141028      *     QUSEC      - Error structure for the QLGSORT API
040000141028      *************************************************************************
040100141028     d sflrcd          ds
040200141103     d  VS3clv
040300141117     d  VS3ord
040400141107     d  VS3ksu
040500150116     d  VS3tipocl
040600141103     d  VS3rag
040700141103     d  VS3istat
040800141103     d  VS3del
040900141103     d  VS3sped
041000141103     d  VS3ric
041100141103     d  VS3pkgm
041200141103     d  VS3obj
041300141111     d  VS3objprop
041400141103     d  VS3objfine
041500141103     d  VS3objttr
041600141103     d  VS3objcf
041700141103     d  VS3cmmd
041800141121     d  VS3esitotr
041900141126     d  VS3decotr
042000150220     d  VS3forza
042100141103     d  VS3nrv
042200141107     d  VS3ksc
042300141107     d  VS3cpo
042400141117     d  VS3ufe
042500150115     d  VS3cch
042600150115     d  VS3cchaut
042700141127     d  VS3oprop
042800141028     d  selected                      1A
042900141028
043000141028      /COPY QSYSINC/QRPGLESRC,QLGSORT
043100141028     d QLGKL                         16    DIM(MaxKey)
043200141028     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
043300141028     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
043400141028     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
043500141028     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
043600141028
043700141028      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
043800141028      /COPY QSYSINC/QRPGLESRC,QUSEC
043900141028
044000141028      //---------------------------------------------------------------
044100141028      //?Definizione procedure usate.
044200141028      //---------------------------------------------------------------
044300141028
044400141024      // - Ordinamento Subfile
044500141028     d Qlgsort_pr      pr                  extpgm('QLGSORT')
044600141028     d  pr_QLGSCB                          like(Qlgscb)
044700141028     d  pr_NotUsed1                        like(NotUsed)
044800141028     d  pr_NotUsed2                        like(NotUsed)
044900141028     d  pr_SizeList                        like(SizeList)
045000141028     d  pr_ReturnSize                      like(ReturnSize)
045100141028     d  pr_QUSEC                           like(Qusec)
045200141028     d                                     options(*varsize)
045300141028
045400141028     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
045500141028     d  pr_QLGSCB00                        like(QLGSCB00)
045600141028     d  pr_SflRcd                          like(SflRcd)
045700141028     d  pr_NotUsed1                        like(NotUsed)
045800141028     d  pr_SizeList                        like(SizeList)
045900141028     d  pr_NotUsed2                        like(NotUsed)
046000141028     d  pr_QUSEC                           like(Qusec)
046100141028     d                                     options(*varsize)
046200141028
046300141028     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
046400141028     d  pr_QLGSCB00                        like(QLGSCB00)
046500141028     d  pr_NotUsed1                        like(NotUsed)
046600141028     d  pr_SflRcd                          like(SflRcd)
046700141028     d  pr_SizeList                        like(SizeList)
046800141028     d  pr_NotUsed2                        like(NotUsed)
046900141028     d  pr_QUSEC                           like(Qusec)
047000141028     d                                     options(*varsize)
047100141105
047200141105       // - Interrogazione anagrafica campagne
047300150112     d TRKC02R         pr                  extpgm('TRKC02R')
047400141105     d  kpjba                              likeds(kpjba)
047500150112     d  trkc02ds                           likeds(TRKC02DS)
047600141107
047700141107       // - Storico Fasi
047800150112     d TRKC03R         pr                  extpgm('TRKC03R')
047900141107     d  kpjba                              likeds(kpjba)
048000150112     d  trkc03ds                           likeds(TRKC03DS)
048100141111
048200141111       // - Aggiunta Cliente in campagna
048300150112     d TRKC06R         pr                  extpgm('TRKC06R')
048400141111     d  kpjba                              likeds(kpjba)
048500150112     d  trkc06ds                           likeds(TRKC06DS)
048600141114
048700141114       // - Variazione Obiettivo
048800150112     d TRKC07R         pr                  extpgm('TRKC07R')
048900141114     d  kpjba                              likeds(kpjba)
049000150112     d  trkc07ds                           likeds(TRKC07DS)
049100150127
049200150127       // - Gestione Note
049300150127     d TRKC10R         pr                  extpgm('TRKC10R')
049400150127     d  kpjba                              likeds(kpjba)
049500150127     d  trkc10ds                           likeds(TRKC10DS)
049600141127
049700141127      // - Pgm Scrive fase se cliente in Campagna
049800150112     d TRKC71R         pr                  extpgm('TRKC71R')
049900141127     d  kpjba                              likeds(kpjba)
050000150112     d  trkc71ds                           likeds(TRKC71DS)
050100141114
050200141114      // - Verifica quale obiettivo variare
050300150112     d TRKC72R         pr                  extpgm('TRKC72R')
050400141114     d  kpjba                              likeds(kpjba)
050500150112     d  trkc72ds                           likeds(TRKC72DS)
050600150218
050700150218      // - Dati da Visualizzare o Parzializzazioni Trattative
050800150218     d TRKC76R         pr                  extpgm('TRKC76R')
050900150218     d  kpjba                              likeds(kpjba)
051000150218     d  trkc76ds                           likeds(TRKC76DS)
051100141103
051200141103       // - Delta cliente unificante
051300141103     d TISE61R         pr                  extpgm('TISE61R')
051400141103     d  kpjba                              likeds(kpjba)
051500141027
051600141103       // - Ricerca anagrafica clienti
051700141027     d TNTAI1R         pr                  extpgm('TNTAI1R')
051800141027     d  kpjba                              likeds(kpjba)
051900141027     d  tntai1ds                           likeds(tntai1ds)
052000141103
052100141103       // - Int. anagrafica clienti
052200141103     d TNTAI2R         pr                  extpgm('TNTAI2R')
052300141103     d  kpjba                              likeds(kpjba)
052400141103     d  tntai2ds                           likeds(tntai2ds)
052500141029
052600141029       // - Caricamento Filiali in gestione
052700141029     d TRUL31R         pr                  extpgm('TRUL31R')
052800141029     d  kpjba                              likeds(kpjba)
052900141029     d  trul31ds                           likeds(trul31ds)
053000141029     d  trul31ds2                          likeds(trul31ds2)
053100141028
053200141028      //---------------------------------------------------------------
053300141024      //?Definizione Prototipi.
053400141028      //---------------------------------------------------------------
053500141024      /copy gaitrasrc/srcprotopr,QsnQryModS
053600150115      /copy gaitrasrc/srcprotopr,TIBS02R
053700141024      /copy gaitrasrc/srcprotopr,TIBS34R
053800141027      /copy gaitrasrc/srcprotopr,TIBS69R
053900141027      /copy gaitrasrc/srcprotopr,TNSD24R
054000141024      /copy gaitrasrc/srcprotopr,TNTAA1C
054100141103      /copy gaitrasrc/srcprotopr,TNTA88R
054200141027      /copy gaitrasrc/srcprotopr,TRMK43R
054300141029      /copy gaitrasrc/srcprotopr,TRUL19R
054400141028
054500141028      //---------------------------------------------------------------
054600141028      //?Definizione key-list.
054700141028      //---------------------------------------------------------------
054800141028       // - File TABEL00F
054900141028     d k03tabel      e ds                  extname(TABEL00F:*key)
055000141028     d                                     prefix(k_)
055100141028
055200141028      //---------------------------------------------------------------
055300141028      //?M A I N - L I N E
055400141028      //---------------------------------------------------------------
055500141028
055600141028     c     *Entry        plist
055700141028     c                   parm                    kpjba
055800141028
055900141028      /free
056000141028
056100141028       //?Operazioni iniziali
056200141028       exsr RoutInz;
056300141028
056400141028       //?Gestione video
056500141024       DOW  Fine = *off;
056600141028         SELECT;
056700141024           WHEN  Video = 'D2';
056800141024             exsr GesD02;
056900141024           WHEN  Video = 'S3';
057000141024             exsr GesS03;
057100141030           WHEN  Video = 'D4';
057200141030             exsr GesD04;
057300141118           WHEN  Video = 'D5';
057400141118             exsr GesD05;
057500150116           WHEN  Video = 'S6';
057600150116             exsr GesS06;
057700150121           WHEN  Video = 'D7';
057800150121             exsr GesD07;
057900150128           WHEN  Video = 'S8';
058000150128             exsr GesS08;
058100150302           WHEN  Video = 'S9';
058200150302             exsr GesS09;
058300141028           OTHER;
058400141024             Fine = *on;
058500141028         ENDSL;
058600141028       ENDDO;
058700141028
058800141028       //?Operazioni finali
058900141028       exsr RoutEnd;
059000141028
059100141028       //--------------------------------------------------------------
059200141028       //?Operazioni iniziali.
059300141028       //--------------------------------------------------------------
059400141028       BEGSR RoutInz;
059500141028
059600141028         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
059700141028
059800141028       //?Impostazione campi "fissi"
059900141024         V01pgm = SDSpgm;
060000141029         k_TBLkut = 1;
060100141029         Video = 'D2';
060200141029         wInzD02 = *on;
060300150121         VisOrdRAG = *on;
060400150121         VisOrdCLV = *off;
060500141029
060600141029       //?Imposto oggi
060700141029         Oggi = %dec(%date());
060800141028
060900141028       //?Reperimento dati job
061000141028         exsr DatiJob;
061100141024
061200141024       //?Controllo abilitazione utente
061300141024         reset TNTAA1DS;
061400141024         ITAA1caut = '§utegtc';
061500141024         clear kpjbu;
061600141024         kpjbu = TNTAA1DS;
061700141024         tntaa1c (kpjba);
061800141024         TNTAA1DS = kpjbu;
061900141024
062000141024         IF  OTAA1fabi = 'N';
062100141024           ErrGrave = *on;
062200141024           leavesr;
062300141024         ENDIF;
062400141027         wabi = OTAA1cabi;
062500141024
062600141027       //?Carico le filiali abilitate all'utente
062700141024         clear TRUL31DS;
062800141027         clear TRUL31DS2;
062900141027         I31abi = wabi;
063000141024         I31cdi = DUTdis;
063100141024         I31car = DUTare;
063200141024         I31cpo = DUTpou;
063300141027         TRUL31R (kpjba:trul31ds:trul31ds2);
063400141024         IF O31pog <= *zeros;
063500141024           ErrGrave = *on;
063600141024           leavesr;
063700141024         ENDIF;
063800141027
063900141027       //?Carico sk Distretti - 17
064000141027         clear xx;
064100141027         k_TBLcod = '17';
064200141027         clear k_TBLkey;
064300141027         setll %kds(K03tabel) TABEL00F;
064400141027         reade %kds(K03tabel : 2) TABEL00F;
064500141027         DOW  not %eof(TABEL00F);
064600141027           IF  TBLflg = *blanks;
064700141027             ds17 = TBLuni;
064800141027             xx += 1;
064900141027             skDistretti(xx) = %subst(TBLkey:1:1);
065000141027             skDesDist(xx) = §17des;
065100141027           ENDIF;
065200141027           reade %kds(K03tabel : 2) TABEL00F;
065300141027         ENDDO;
065400141027
065500141027       //?Carico sk Aree - 05
065600141027         clear xx;
065700141027         k_TBLcod = '05';
065800141027         clear k_TBLkey;
065900141027         setll %kds(K03tabel) TABEL00F;
066000141027         reade %kds(K03tabel : 2) TABEL00F;
066100141027         DOW  not %eof(TABEL00F);
066200141027           IF  TBLflg = *blanks;
066300141027             ds05 = TBLuni;
066400141027             xx += 1;
066500141104             skAree(xx) = %subst(TBLkey:1:3);
066600141027             skDesArea(xx) = §05des;
066700141027           ENDIF;
066800141027           reade %kds(K03tabel : 2) TABEL00F;
066900141027         ENDDO;
067000141024
067100141024       //?Carico sk Importanza Clienti
067200141024         clear xx;
067300141029         k_TBLcod = 'IC';
067400141027         clear k_TBLkey;
067500141027         setll %kds(K03tabel) TABEL00F;
067600141027         reade %kds(K03tabel : 2) TABEL00F;
067700141024         DOW  not %eof(TABEL00F);
067800141024           IF  TBLflg = *blanks;
067900141024             xx += 1;
068000141024             skIC(xx) = %subst(TBLkey:1:1);
068100141117             dsIC = TBLuni;
068200141117             skIC_ord(xx) = §SICor;
068300141024           ENDIF;
068400141027           reade %kds(K03tabel : 2) TABEL00F;
068500141024         ENDDO;
068600141028
068700141028       ENDSR;
068800141028
068900141028       //--------------------------------------------------------------
069000141028       //?Reperimento Dati del job (Utente/Operativi).
069100141028       //--------------------------------------------------------------
069200141028       BEGSR DatiJob;
069300141028
069400141028         in(E) §AzUte;
069500141024         IF  NOT %error;
069600141028           in(E) §DatiUte;
069700141024         ENDIF;
069800141024         IF  %error or RSut = *blanks;
069900141028           clear TIBS34ds;
070000141028           tibs34r(tibs34ds);
070100141028           in §AzUte;
070200141028           in §DatiUte;
070300141024         ENDIF;
070400141028
070500141028       ENDSR;
070600141024
070700141024       //--------------------------------------------------------------
070800141024       //?Gestione videata D02.
070900141024       //--------------------------------------------------------------
071000141024       BEGSR GesD02;
071100141024
071200141024       //?Inizializzazione videata
071300141029         IF  wInzD02;
071400141029           exsr InzD02;
071500141029           wInzD02 = *off;
071600141024         ENDIF;
071700141024
071800141024       //?Se errore grave emetto messaggio poi esco
071900141024         IF  ErrGrave;
072000141024           ErrMessage  = *on;
072100141024           ErrGenerico = *on;
072200141024           V02msg = Msg(01);
072300141024         ENDIF;
072400141024
072500141030       //?Emissione Testata
072600150112         write  KC01T01;
072700141024
072800141024       //?Emissione videata
072900150112         exfmt  KC01D02;
073000141024         reset ErrMessage;
073100141024         reset ErrGenerico;
073200141024         clear V02msg;
073300141024
073400141024         SELECT;
073500141024
073600141024       //?- Errore Grave esco
073700141024           WHEN  ErrGrave;
073800141024             Fine = *on;
073900150116
074000150116       //?- F01=Formula
074100150116           WHEN  dsp_aid = c_F01;
074200150116             exsr F01D02;
074300150128
074400150128       //?- F02=Obiettivo/Andamento
074500150128           WHEN  dsp_aid = c_F02;
074600150128             exsr F02D02;
074700141024
074800141024       //?- F03=Fine
074900141024           WHEN  dsp_aid = c_F03;
075000141024             exsr F03D02;
075100150302
075200150302       //?- F04=Scelta Trattative
075300150302           WHEN  dsp_aid = c_F04;
075400150302             exsr F04D02;
075500141024
075600141024       //?Invio
075700141024           OTHER;
075800141024             exsr ContrD02;
075900141028             IF  ErrGenerico;
076000141028               leavesr;
076100141028             ENDIF;
076200141028         //?Videata sucessiva
076300141028             Video = 'S3';
076400141029             wInzS03 = *on;
076500150212             wInzD04 = *on;
076600141024
076700141024         ENDSL;
076800141024
076900141024       ENDSR;
077000141024
077100141024       //--------------------------------------------------------------
077200141029       //?Inizializzazione Videata D02.
077300141024       //--------------------------------------------------------------
077400141029       BEGSR InzD02;
077500141027
077600141027         VisDistretto = *off;
077700141027         VisArea      = *off;
077800141202
077900141202         clear V01des;
078000141024
078100141024       //?Pulizia videata
078200150112         clear KC01D02;
078300141024
078400141024       //?Imposto di dflt il "+" nelle % delta
078500141024         V02deldas = '+';
078600141024         V02delas = '+';
078700150127
078800150213       //?Imposto di dflt 'E' escludi
078900150213         V02cli = 'E';
079000150213
079100150213       //?Imposto di dflt il "+" nelle % Obiettivo/Andamento
079200150213         V02obj3das = '+';
079300150213         V02obj3as = '+';
079400141024
079500141024       //?Se errore grave non imposto più niente
079600141024         IF  ErrGrave;
079700141024           leavesr;
079800141024         ENDIF;
079900141027
080000141027         SELECT;
080100141027       //?Abilitazione AZ-Azienda
080200141027       //?vedo tutte le selezioni
080300141027         WHEN  wabi = 'AZ';
080400141027           VisDistretto = *on;
080500141027           VisArea      = *on;
080600141024
080700141027       //?Abilitazione DI-Distretto
080800141027       //?NON vedo la selezione per Distretto
080900141027         WHEN  wabi = 'DI';
081000141027           VisArea      = *on;
081100141027
081200141027       //?Abilitazione TP-Terminal Partenza o PO=Filiale
081300141027       //?NON vedo la selezione per Distretto e per Area
081400141029         WHEN  wabi = 'TP' or wabi = 'PO';
081500141027         ENDSL;
081600141024
081700141024       ENDSR;
081800150116
081900150116       //--------------------------------------------------------------
082000150116       //?Gestione tasto funzionale F01 da videata D02
082100150116       //?F01=Formula
082200150116       //--------------------------------------------------------------
082300150116       BEGSR F01D02;
082400150116
082500150119         savformula = V02formula;
082600150116         Video = 'S6';
082700150116         wInzS06 = *on;
082800150116
082900150116       ENDSR;
083000150128
083100150128       //--------------------------------------------------------------
083200150128       //?Gestione tasto funzionale F02 da videata D02
083300150128       //?F02=Obiettivo/Andamento
083400150128       //--------------------------------------------------------------
083500150128       BEGSR F02D02;
083600150216
083700150216         IF  H2nmfl <> 'V02OBJ1' and H2nmfl <> 'V02OBJ2' and
083800150216             H2nmfl <> 'V02OBJ3';
083900150216           ErrMessage  = *on;
084000150216           ErrGenerico = *on;
084100150216           PosCurOBJ1  = *on;
084200150216           V02msg = Msg(22);
084300150216           leavesr;
084400150216         ENDIF;
084500150128
084600150128         Video = 'S8';
084700150128         wInzS08 = *on;
084800150128
084900150128       ENDSR;
085000141029
085100141029       //--------------------------------------------------------------
085200141029       //?Gestione tasto funzionale F03 da videata D02
085300141029       //?F03=Fine
085400141029       //--------------------------------------------------------------
085500141029       BEGSR F03D02;
085600141029
085700141029       //?Chiusura del programma
085800141029         Fine = *on;
085900141029
086000141029       ENDSR;
086100150302
086200150302       //--------------------------------------------------------------
086300150302       //?Gestione tasto funzionale F04 da videata D02
086400150302       //?F04=Scelta Trattative
086500150302       //--------------------------------------------------------------
086600150302       BEGSR F04D02;
086700150302
086800150302         Video = 'S9';
086900150302         wInzS09 = *on;
087000150302
087100150302       ENDSR;
087200141024
087300141024       //--------------------------------------------------------------
087400141024       //?Controlla Videata D02.
087500141024       //--------------------------------------------------------------
087600141024       BEGSR ContrD02;
087700141024
087800141024         WindDspF = IndDspF;
087900141024         reset WindDspFb;
088000141024         IndDspF  = WindDspF;
088100141119
088200141119         clear wdelda;
088300141119         clear wdela;
088400141024
088500141027       //?Campagna Commerciale Obbligatoria
088600141027         clear V02des;
088700141024         IF  V02ncm = *blanks;
088800141024           ErrMessage  = *on;
088900141024           ErrGenerico = *on;
089000141024           PosCurNCM   = *on;
089100141024           V02msg = Msg(02);
089200141024           leavesr;
089300141024         ENDIF;
089400141027       //?Ricerca Campagna Commerciale
089500141024         IF  %scan('?':V02ncm) > 0;
089600150112           clear TRKC02DS;
089700150112           IKC02ric = 'R';
089800150112           trkc02r (kpjba:trkc02ds);
089900141024           ErrGenerico = *on;
090000141024           PosCurNCM   = *on;
090100150112           V02ncm = %editc(OKC02ncm:'X');
090200141024         ENDIF;
090300141027       //?Accetto solo dati numerici
090400141029         xx = 1;
090500141029         FOR xx by 1 to %len(V02ncm);
090600141029           IF  %subst(V02ncm:xx:1) <> *blanks and
090700141029               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
090800141029             ErrMessage  = *on;
090900141029             ErrGenerico = *on;
091000141029             PosCurNCM   = *on;
091100141029             V02msg = Msg(02);
091200141029             leavesr;
091300141029           ENDIF;
091400141029         ENDFOR;
091500141027       //?Deve essere una Campagna Commerciale valida
091600141024         CMPncm = %dec(V02ncm:7:0);
091700141027         chain CMPncm TICMP01L;
091800141027         IF  not %found(TICMP01L);
091900141027           ErrMessage  = *on;
092000141027           ErrGenerico = *on;
092100141027           PosCurNCM   = *on;
092200141027           V02msg = Msg(02);
092300141027           leavesr;
092400141027         ENDIF;
092500141027       //?Decodifico Campagna Commerciale
092600141027         V02des = CMPdes;
092700150505       ////?Campagna Commerciale già finita
092800150505         //IF  CMPdfc < Oggi;
092900150505         //  ErrMessage  = *on;
093000150505         //  ErrGenerico = *on;
093100150505         //  PosCurNCM   = *on;
093200150505         //  V02msg = %trim(Msg(02)) + '. Già chiusa';
093300150505         //  leavesr;
093400150505         //ENDIF;
093500141027       //?In carico all'Utente
093600141029         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
093700141027           ErrMessage  = *on;
093800141027           ErrGenerico = *on;
093900141027           PosCurNCM   = *on;
094000141027           V02msg = %trim(Msg(02)) + '. Non in gestione';
094100141027           leavesr;
094200141027         ENDIF;
094300141027
094400141027       //?Distretto
094500141103         w001a = V02cdi;
094600141030         clear w050a;
094700141030         exsr Distretto;
094800141103         V02cdi = w001a;
094900141030         V02cdid = w050a;
095000141030         V02msg = wmsg;
095100141030         IF  ErrGenerico;
095200141030           leavesr;
095300141030         ENDIF;
095400141027
095500141027       //?Area
095600141103         w003a = V02car;
095700141030         clear w050a;
095800141030         exsr Area;
095900141103         V02car = w003a;
096000141030         V02card = w050a;
096100141030         V02msg = wmsg;
096200141030         IF  ErrGenerico;
096300141030           leavesr;
096400141030         ENDIF;
096500141030       //?Se presente anche il distretto devono essere congruenti
096600141030         IF  V02cdi <> *blanks and V02car <> *blanks;
096700141030           chain (V02cdi:w0030) AZORG02L;
096800141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
096900141030             ErrGenerico = *on;
097000141030             ErrMessage  = *on;
097100141030             PosCurCAR   = *on;
097200141030             V02msg = %trim(Msg(04)) + '. Non congruente con il distretto';
097300141030             leavesr;
097400141030           ENDIF;
097500141030         ENDIF;
097600141027
097700141027       //?Filiale Commerciale
097800141103         w003a = V02fil;
097900141030         clear w050a;
098000141030         exsr Filiale;
098100141103         V02fil = w003a;
098200141030         V02fild = w050a;
098300141030         V02msg = wmsg;
098400141030         IF  ErrGenerico;
098500141030           leavesr;
098600141030         ENDIF;
098700141027
098800141027       //?Commerciale Unificante
098900141103         w007a = V02cmm;
099000141030         clear w050a;
099100141030         exsr Commerciale;
099200141103         V02cmm = w007a;
099300141030         V02cmmd = w050a;
099400141030         V02msg = wmsg;
099500141030         IF  ErrGenerico;
099600141030           leavesr;
099700141030         ENDIF;
099800141027
099900141027       //?Cliente
100000141107         w007a = V02ksu;
100100141030         clear w050a;
100200141030         exsr Cliente;
100300141107         V02ksu = w007a;
100400141030         V02rag = w050a;
100500141030         V02msg = wmsg;
100600141030         IF  ErrGenerico;
100700141030           leavesr;
100800141030         ENDIF;
100900141024
101000141024       //?Importanza Cliente
101100141030         w001a = V02clv1;
101200141030         clear w050a;
101300141030         exsr ImpCliente;
101400141030         V02clv1 = w001a;
101500141030         V02msg = wmsg;
101600141030         IF  ErrGenerico;
101700141030           PosCurCLV1 = *on;
101800141030           leavesr;
101900141030         ENDIF;
102000141030         w001a = V02clv2;
102100141030         clear w050a;
102200141030         exsr ImpCliente;
102300141030         V02clv2 = w001a;
102400141030         V02msg = wmsg;
102500141030         IF  ErrGenerico;
102600141030           PosCurCLV2 = *on;
102700141030           leavesr;
102800141030         ENDIF;
102900141030         w001a = V02clv3;
103000141030         clear w050a;
103100141030         exsr ImpCliente;
103200141030         V02clv3 = w001a;
103300141030         V02msg = wmsg;
103400141030         IF  ErrGenerico;
103500141030           PosCurCLV3 = *on;
103600141030           leavesr;
103700141030         ENDIF;
103800141030         w001a = V02clv4;
103900141030         clear w050a;
104000141030         exsr ImpCliente;
104100141030         V02clv4 = w001a;
104200141030         V02msg = wmsg;
104300141030         IF  ErrGenerico;
104400141030           PosCurCLV4 = *on;
104500141030           leavesr;
104600141030         ENDIF;
104700141030         w001a = V02clv5;
104800141030         clear w050a;
104900141030         exsr ImpCliente;
105000141030         V02clv5 = w001a;
105100141030         V02msg = wmsg;
105200141030         IF  ErrGenerico;
105300141030           PosCurCLV5 = *on;
105400141030           leavesr;
105500141030         ENDIF;
105600141028
105700141028       //?Delta
105800141029         IF  V02delda > *zeros or V02dela <> *zeros;
105900141028         //?Obbligatori tutti e 2 se immesso uno dei due
106000141028           IF  V02delda > *zeros and V02dela = *zeros;
106100141028             ErrMessage  = *on;
106200141028             ErrGenerico = *on;
106300141028             PosCurDELA  = *on;
106400150216             V02msg      = Msg(09);
106500141028             leavesr;
106600141028           ENDIF;
106700141028           IF  V02delda = *zeros and V02dela > *zeros;
106800141028             ErrMessage  = *on;
106900141028             ErrGenerico = *on;
107000141028             PosCurDELda = *on;
107100150216             V02msg      = Msg(09);
107200141028             leavesr;
107300141028           ENDIF;
107400141028         //?Controllo congruenza tra "DAL" e "AL"
107500141028           wdelda = V02delda;
107600141028           wdela  = V02dela;
107700141029           IF  V02deldas = '-';
107800141028             wdelda = wdelda * -1;
107900141028           ENDIF;
108000141029           IF  V02delas = '-';
108100141028             wdela = wdela * -1;
108200141028           ENDIF;
108300141028           IF  wdelda > wdela;
108400141028             ErrMessage  = *on;
108500141028             ErrGenerico = *on;
108600141028             PosCurDELda = *on;
108700150216             V02msg      = Msg(10);
108800141028             leavesr;
108900141028           ENDIF;
109000141028         ENDIF;
109100141121
109200141121       //?Obiettivi
109300150119         wfobj  = V02obj;
109400141127         wfobj1 = V02obj1;
109500141127         wfobj2 = V02obj2;
109600150119         wfformula = V02formula;
109700150213         wfobj3 = V02obj3;
109800150213         wobj3da = V02obj3da;
109900150213         wobj3a  = V02obj3a;
110000150213         IF  V02obj3das = '-';
110100150213           wobj3da = wobj3da * -1;
110200150213         ENDIF;
110300150213         IF  V02obj3as = '-';
110400150213           wobj3a = wobj3a * -1;
110500150213         ENDIF;
110600150128         exsr ContrObj;
110700141127         V02obj1 = wfobj1;
110800141127         V02obj2 = wfobj2;
110900150119         V02formula = wfformula;
111000150213         V02obj3 = wfobj3;
111100150213         V02obj3da = %abs(wobj3da);
111200150213         V02obj3a  = %abs(wobj3a);
111300141127         V02msg = wmsg;
111400141127         IF  ErrGenerico;
111500141127           leavesr;
111600141127         ENDIF;
111700150302
111800150302       //?Trattative
111900150302         w002a = V02ttr;
112000150302         clear w050a;
112100150302         exsr SceltaTtr;
112200150302         V02ttr = w002a;
112300150302         V02msg = wmsg;
112400150302         IF  ErrGenerico;
112500150302           leavesr;
112600150302         ENDIF;
112700141024
112800141024       ENDSR;
112900141028
113000141028       //--------------------------------------------------------------
113100141028       //?Gestione videata S03.
113200141028       //--------------------------------------------------------------
113300141028       BEGSR GesS03;
113400141028
113500141028       //?Inizializzazione videata
113600141029         IF  wInzS03;
113700141028           exsr InzS03;
113800141029           wInzS03 = *off;
113900141118           IF  wMaxNrr;
114000141118             leavesr;
114100141118           ENDIF;
114200141028         ENDIF;
114300141028
114400141028       //?Visualizzazione del SFL (se ci sono dati)
114500141117         SflDsp = (S03nrr <= *zeros);
114600141104
114700141104       //?Posizionamento cursore
114800141112         V03rcd = V03csr;
114900141111
115000141111       //?Se utente di sede abilito anche l'aggiunta di nuovi clienti
115100141111       //?con F10
115200150507       //?(SE Campagna NON scaduta)?
115300150505         AbilitaF10 = (DUTpou = 046  and  CMPdfc >= Oggi);
115400141127
115500150119       //?Se utente di sede e richiesto
115600150128       //?Obiettivo Inizio = Obiettivo Proposto
115700150507       //?e SE Campagna NON scaduta?
115800141127       //?abilito F13 per conferma obiettivo proposto
115900150212         AbilitaF13 = (DUTpou = 046 and V04obj = 'P' and
116000150507                       CMPdfc >= Oggi  and
116100150216                      ((V04formula = Formula(1) and
116200150216                       ((V04obj1 = 'I' and V04obj2 = 'P') or
116300150216                        (V04obj1 = 'P' and V04obj2 = 'I'))) or
116400150216                       (V04formula = Formula(2) and
116500150216                        V04obj1 = 'P' and V04obj2 = 'I')));
116600141028
116700141028       //?Emissione Testata e Piede con tasti funzionali abilitati
116800150112         write  KC01T01;
116900150112         write  KC01P03;
117000141028
117100141028       //?Emissione videata
117200150112         exfmt  KC01C03;
117300141028         reset ErrMessage;
117400141028         reset ErrGenerico;
117500141028         clear V03msg;
117600141028
117700141028         SELECT;
117800141030
117900141030       //?- F01=Guida Fasi
118000141030           WHEN  dsp_aid = c_F01;
118100141030             exsr F01S03;
118200141028
118300141028       //?- F03=Fine
118400141028           WHEN  dsp_aid = c_F03;
118500141029             exsr F03D02;
118600141030
118700141030       //?- F05=Altre parzializzazioni
118800141030           WHEN  dsp_aid = c_F05;
118900141030             exsr F05S03;
119000141030
119100141030       //?- F08=Ord.X Imp.Cliente
119200141030           WHEN  dsp_aid = c_F08;
119300141030             exsr F08S03;
119400141111
119500141111       //?- F10=Aggiunta Cliente
119600141111           WHEN  dsp_aid = c_F10;
119700141111             exsr F10S03;
119800141028
119900141028       //?- F12=Ritorno
120000141028           WHEN  dsp_aid = c_F12;
120100141028             exsr F12S03;
120200141127
120300141127       //?- F13=Conferma Obiettivo Proposto
120400141127           WHEN  dsp_aid = c_F13;
120500141127             exsr F13S03;
120600141028
120700141028       //?Invio
120800141028           OTHER;
120900141028             IF  V03csr > *zero;
121000141028               exsr OpzS03;
121100141028               IF  ErrGenerico;
121200141028                 leavesr;
121300141028               ENDIF;
121400141028             ENDIF;
121500141028
121600141028         ENDSL;
121700141028
121800141028       ENDSR;
121900141028
122000141028       //--------------------------------------------------------------
122100141028       //?Inizializzazione videata S03.
122200141028       //--------------------------------------------------------------
122300141029       BEGSR InzS03;
122400141028
122500141028         EndS03 = *off;
122600141112         clear V03tot;
122700150116         TroppiRcd = *off;
122800141202
122900141202       //?Imposto la descrizione della campagna
123000150121         V01des = CMPdes;
123100150212
123200150212       //?Periodo Confronto Fatturazione
123300150212         V03aammcf = %subst(CMPflo:5:2) + '/' + %subst(CMPflo:1:4);
123400141028
123500141028       //?Pulizia subfile
123600141028         exsr PulS03;
123700150212
123800150212       //?Imposto i dati per videata delle parzializzazioni
123900150212       //?e per caricamento subfile
124000150212         IF  wInzD04;
124100150212           exsr InzD04;
124200150212           wInzD04 = *off;
124300150212         ENDIF;
124400141028
124500141028       //?Caricamento subfile
124600141028         exsr Ries03;
124700141118
124800141118       //?Se ho superato il numero massimo di rcd da caricare esco
124900141118         IF  wMaxNrr;
125000141124           clear V03tot;
125100141118           Video = 'D5';
125200141118           wInzD05 = *on;
125300141118           leavesr;
125400141118         ENDIF;
125500141112
125600141117         rrnlast = S03nrr;
125700141112
125800141112       //?Faccio l'ordinamento
125900141112         SELECT;
126000141112       //?se ordina x RAG devo ordinare per CLV
126100141112         WHEN  VisOrdRAG;
126200141112           exsr Ordina_x_CLV;
126300141112       //?se ordina x CLV devo ordinare per RAG
126400141112         WHEN  VisOrdCLV;
126500141112           exsr Ordina_x_RAG;
126600141112         ENDSL;
126700141112
126800141112         SflEnd = *on;
126900141112
127000141112       //?Imposto il posizionamento al primo rcd
127100141112         IF  S03nrr > 0;
127200141112           V03rcd = 1;
127300141112         ELSE;
127400141112           clear V03rcd;
127500141112         ENDIF;
127600141112
127700141112         V03csr = V03rcd;
127800141117
127900150213       //?Imposto il n. rcd del subfile solo se il nrr savlato
128000150213       //?è <= all'ultimo nrr del subfile appena caricato
128100150213       //?se no imposto l'ultimo nrr
128200150213         IF  sav_S03nrr > 0;
128300150213           IF  sav_S03nrr <= rrnlast;
128400150213             V03csr = sav_S03nrr;
128500150213             clear sav_S03nrr;
128600150213           ELSE;
128700150213             V03csr = rrnlast;
128800150213           ENDIF;
128900141117         ENDIF;
129000141028
129100141028       ENDSR;
129200141028
129300141028       //--------------------------------------------------------------
129400141028       //?Pulizia Subfile S03.
129500141028       //--------------------------------------------------------------
129600141028       BEGSR PulS03;
129700141028
129800141028       //?Pulizia subfile
129900141028         SflDsp    = *on;
130000141028         SflDspCtl = *on;
130100150112         write KC01C03;
130200141028         SflDspCtl = *off;
130300141028         SflEnd    = *off;
130400141028
130500141112         clear V03rcd;
130600141028         clear V03csr;
130700141028         clear S03nrr;
130800141028         clear V03msg;
130900141028
131000141028         ErrMessage  = *off;
131100141028         ErrGenerico = *off;
131200141028
131300141028         WindDspF = IndDspF;
131400141028         reset WindDspFb;
131500141028         IndDspF  = WindDspF;
131600141028
131700141104       ENDSR;
131800141028
131900141028       //--------------------------------------------------------------
132000141028       //?Caricamento Subfile S03.
132100141028       //--------------------------------------------------------------
132200141028       BEGSR RieS03;
132300141028
132400141028         EndS03 = *off;
132500141103         wMaxNrr = *off;
132600141028
132700141028       //?Preparo Stringa Sql
132800141113         exsr PreparaSQL;
132900141028
133000141028       //?Dichiarazione cursore
133100141028         exec sql
133200141028           prepare S1   from :wSQL;
133300141028         exec sql
133400141124           declare WRK cursor for S1;
133500141124          // declare WRK  insensitive no scroll cursor for S1;
133600141028
133700141028         //?Apertura del cursore
133800141028         exec sql
133900141028           open WRK;
134000141028
134100141028         IF sqlcode < 0;
134200141028           EndS03 = *on;
134300141113           exec sql close WRK;
134400141113           leavesr;
134500141028         ENDIF;
134600141028
134700141028         DOW  not EndS03;
134800141028           exec sql
134900150115             fetch next from WRK into :TICMI00F, :CMCufe, :CMCcch;
135000141028           IF sqlcod = 100 or sqlcod < 0;
135100141028             EndS03 = *on;
135200141028             leave;
135300141028           ENDIF;
135400150303
135500150303         //?Controllo se ok il rcd per la selezione sulle trattative
135600150303           IF  V04ttr <> *blanks and V04ttr <> 'NO' and V04ttr <> 'SI';
135700150303             exsr CTRttr;
135800150303             IF  not RecOK;
135900150303               iter;
136000150303             ENDIF;
136100150303           ENDIF;
136200141124
136300141124         //?Controllo se ok il rcd
136400141124         //?per il "di cui" obiettivo
136500150212           IF  V04obj1 <> *blanks and V04obj2 <> *blanks;
136600141124             exsr CTRrec;
136700141124             IF  not RecOK;
136800141124               iter;
136900141124             ENDIF;
137000141124           ENDIF;
137100150213         //?per la parzializzazione dal al Obiettivo/Andamento
137200150213           IF  V04obj3 <> *blanks;
137300150213             exsr CTRrec1;
137400150213             IF  not RecOK;
137500150213               iter;
137600150213             ENDIF;
137700150213           ENDIF;
137800141028
137900141028         //?Carico i dati nel subfile
138000141028           exsr CarS03;
138100141028
138200141028         ENDDO;
138300141028
138400141028         exec sql
138500141028           close WRK;
138600141124
138700141124         V03tot = S03nrr;
138800141028
138900141028       ENDSR;
139000141028
139100141028       //--------------------------------------------------------------
139200141028       //?Preparo la Stringa SQL.
139300141028       //--------------------------------------------------------------
139400141028       BEGSR PreparaSQL;
139500141113
139600150303       //?Scelta trattative
139700141113         SELECT;
139800150303       //?Con trattativa  (fase TR o TF)
139900150303         WHEN  V04ttr <> 'NO' and V04ttr <> *blanks;
140000141113           wSQL = 'with SEL001 as ' +
140100141113                  '(select max (digits(CMFdfa) concat '+
140200141113                  'digits(CMFhfc)) as time, ' +
140300141113                  'CMFncm, CMFksu, CMFksc, CMFcpo ' +
140400141113                  'from TICMF00F ' +
140500150223                  'where CMFacm in('+ '''' + FaseObjTtr + '''' +
140600150223                  ', ' + '''' + FaseObjTf + '''' + ')' +
140700150212                  ' and CMFncm = ' + %editc(VH4ncm:'X') +
140800141113                  ' group by CMFncm, CMFksu, CMFksc, CMFcpo)' +
140900150115                  'select TICMI00F.*, CMCufe, CMCcch from SEL001 ' +
141000141113                  'join TICMI00F on ' +
141100141113                  'CMFncm = CMIncm and CMFksu = CMIksu and ' +
141200141113                  'CMFksc = CMIksc and CMFcpo = CMIcpo ' +
141300141113                  'join TICMC00F on ' +
141400141113                  'CMIncm = CMCncm and CMIksu = CMCksu and ' +
141500150303                  'CMIksc = CMCksc and CMIcpo = CMCcpo ' +
141600150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
141700141113           exsr AggSelSQL;
141800150303
141900150303       //?Nessuna trattativa
142000150303         WHEN  V04ttr = 'NO';
142100150303           wSQL = 'with SEL001 as ' +
142200150303                  '(select TICMI00F.* from TICMI00F '+
142300150303                  'exception join TICMF00F on ' +
142400150303                  'CMIncm = CMFncm and CMIksu = CMFksu and ' +
142500150303                  'CMIksc = CMFksc and CMIcpo = CMFcpo and ' +
142600150303                  'CMFacm in(' + '''' + FaseObjTtr + '''' +
142700150303                  ', ' + '''' + FaseObjTf + '''' + ') ' +
142800150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
142900150303           exsr AggSelSQL;
143000150303           wSQL += ') ';
143100150303           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
143200150303                   'from SEL001, TICMC00F ' +
143300150303                   'where ' +
143400150303                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
143500150303                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
143600141113
143700141113         OTHER;
143800150303       //?"  " = Nessuna scelta
143900141113           wSQL = 'with SEL001 as ' +
144000141113                  '(select TICMI00F.* from TICMI00F ' +
144100150212                  ' where CMIncm = ' + %editc(VH4ncm:'X');
144200141113           exsr AggSelSQL;
144300141113           wSQL += ') ';
144400150115           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
144500141113                   'from SEL001, TICMC00F ' +
144600141113                   'where ' +
144700141113                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
144800141113                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
144900141113         ENDSL;
145000150119
145100150213       //?Clienti Bloccati
145200150213         IF  V04cli = 'E';
145300150119           wSQL += ' and CMCcch = ' + '''   ''';
145400150119         ENDIF;
145500150213         IF  V04cli = 'I';
145600150213           wSQL += ' and CMCcch <> ' + '''   ''';
145700150213         ENDIF;
145800150213         IF  V04cli = *blanks;
145900150213         ENDIF;
146000150119
146100150119       //?Clienti Nuovi
146200150212         IF  V04ncli = 'S';
146300150119           wSQL += ' and CMIcln = ' + '''N''' +
146400150119                   ' and CMCcch = ' + '''   ''';
146500150119         ENDIF;
146600141113
146700141113         SELECT;
146800150128       //?Richiesta parzializzazione obiettivo Inizio
146900150212         WHEN  V04obj = 'I';
147000141113           wSQL += ' and CMCufe = ' + '''' + FaseObj + '''';
147100141113       //?Richiesta parzializzazione obiettivo Proposto
147200150212         WHEN  V04obj = 'P';
147300141113           wSQL += ' and CMCufe = ' + '''' + FaseObjProp + '''';
147400141113       //?Richiesta parzializzazione obiettivo Finale
147500150212         WHEN  V04obj = 'F';
147600141113           wSQL += ' and CMCufe = ' + '''' + FaseObjFine + '''';
147700141113         ENDSL;
147800141028
147900141028         wSQL += ' for fetch only';
148000141028
148100141028       ENDSR;
148200141113
148300141113       //--------------------------------------------------------------
148400141113       //?Aggiungo le selezioni alla stringa SQL.
148500141113       //--------------------------------------------------------------
148600141113       BEGSR AggSelSQL;
148700141113
148800141113         wclv = *off;
148900141119
149000141119       //?Se non sono richiesti Distretto/Area/Filiale DEVO caricare solo i clienti
149100141119       //?abilitati all'utente
149200150212         IF  V04cdi = *blanks and VH4car = *zeros and VH4fil = *zeros;
149300141119           xx = 1;
149400141119           Primo = *off;
149500141119           wSQL += ' and CMIfcm in(';
149600141119           FOR  xx by 1 to %elem(POG);
149700141119             IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
149800141119               IF  Primo;
149900141119                 wsql += ', ';
150000141119               ELSE;
150100141119                 Primo = *on;
150200141119               ENDIF;
150300141119               wsql += POG(xx);
150400141119             ENDIF;
150500141119           ENDFOR;
150600141119           wsql += ')';
150700141119         ENDIF;
150800141113
150900141113       //?Distretto
151000150212         IF  V04cdi <> *blanks;
151100150212           wSQL += ' and CMIdcm = ' + '''' + V04cdi + '''';
151200141113         ENDIF;
151300141113       //?Area
151400150212         IF  VH4car > *zeros;
151500150212           wSQL += ' and CMIacm = ' + %editc(VH4car:'X');
151600141113         ENDIF;
151700141113       //?Filiale
151800150212         IF  VH4fil > *zeros;
151900150212           wSQL += ' and CMIfcm = ' + %editc(VH4fil:'X');
152000141113         ENDIF;
152100141113       //?Commerciale
152200150212         IF  VH4cmm > *zeros;
152300150212           wSQL += ' and CMIcmm = ' + %editc(VH4cmm:'X');
152400141113         ENDIF;
152500141113       //?Cliente
152600150212         IF  VH4ksu > *zeros;
152700150212           wSQL += ' and CMIksu = ' + %editc(VH4ksu:'X');
152800141113         ENDIF;
152900141113       //?Importanza Cliente
153000150212         IF  V04clv1 <> *blanks;
153100150212           wSQL += ' and CMIclv in(' + '''' + V04clv1 + '''';
153200141113           wclv = *on;
153300141113         ENDIF;
153400150212         IF  V04clv2 <> *blanks and wclv;
153500150212           wSQL += ',' + '''' + V04clv2 + '''';
153600141113           wclv = *on;
153700141113         ENDIF;
153800150212         IF  V04clv2 <> *blanks and not wclv;
153900150212           wSQL += ' and CMIclv in(' + '''' + V04clv2 + '''';
154000141113           wclv = *on;
154100141113         ENDIF;
154200150212         IF  V04clv3 <> *blanks and wclv;
154300150212           wSQL += ',' + '''' + V04clv3 + '''';
154400141113           wclv = *on;
154500141113         ENDIF;
154600150212         IF  V04clv3 <> *blanks and not wclv;
154700150212           wSQL += ' and CMIclv in(' + '''' + V04clv3 + '''';
154800141113           wclv = *on;
154900141113         ENDIF;
155000150212         IF  V04clv4 <> *blanks and wclv;
155100150212           wSQL += ',' + '''' + V04clv4 + '''';
155200141113           wclv = *on;
155300141113         ENDIF;
155400150212         IF  V04clv4 <> *blanks and not wclv;
155500150212           wSQL += ' and CMIclv in(' + '''' + V04clv4 + '''';
155600141113           wclv = *on;
155700141113         ENDIF;
155800150212         IF  V04clv5 <> *blanks and wclv;
155900150212           wSQL += ',' + '''' + V04clv5 + '''';
156000141113           wclv = *on;
156100141113         ENDIF;
156200150212         IF  V04clv5 <> *blanks and not wclv;
156300150212           wSQL += ' and CMIclv in(' + '''' + V04clv5 + '''';
156400141113           wclv = *on;
156500141113         ENDIF;
156600141113         IF  wclv;
156700141113           wSQL += ')';
156800141113         ENDIF;
156900141113       //?Delta
157000150212         IF  v04delda <> *zeros and V04dela <> *zeros;
157100141119           wSQL += ' and CMIpde between ' + %editc(wdelda:'O') +
157200141119                   ' and ' + %editc(wdela:'O');
157300141113         ENDIF;
157400141113
157500141113       ENDSR;
157600150303
157700150303       //--------------------------------------------------------------
157800150303       //?Controllo se OK il record per selezione trattative.
157900150303       //--------------------------------------------------------------
158000150303       BEGSR CTRttr;
158100150303
158200150303         RecOk = *off;
158300150303         clear wfase;
158400150303         wflgp = V04ttr;
158500150303         wtpric = 'P';
158600150303
158700150303         exsr CallTRKC76;
158800150303         IF  OKC76err <> *blanks;
158900150303           leavesr;
159000150303         ENDIF;
159100150303
159200150303         RecOK = *on;
159300150303
159400150303       ENDSR;
159500141124
159600141124       //--------------------------------------------------------------
159700141124       //?Controllo se OK il record.
159800141124       //--------------------------------------------------------------
159900141124       BEGSR CTRrec;
160000141124
160100141124         RecOk = *off;
160200150128         clear wvalobj1;
160300150128         clear wvalobj2;
160400141124
160500141124       //?Cerco il "di cui" 1
160600141124         SELECT;
160700150128       //?Inizio
160800150212         WHEN  V04obj1 = 'I';
160900150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
161000150218           IF  not %found(TICMF01L);
161100141126             leavesr;
161200141126           ENDIF;
161300150218           wvalobj1 = CMFpea;
161400141124       //?Proposto
161500150212         WHEN  V04obj1 = 'P';
161600150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
161700150218           IF  not %found(TICMF01L);
161800141126             leavesr;
161900141126           ENDIF;
162000150218           wvalobj1 = CMFpea;
162100141124       //?Finale
162200150212         WHEN  V04obj1 = 'F';
162300150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
162400150218           IF  not %found(TICMF01L);
162500141126             leavesr;
162600141126           ENDIF;
162700150218           wvalobj1 = CMFpea;
162800150119       //?Trattativa
162900150212         WHEN  V04obj1 = 'T';
163000150223           wfase = FaseObjTtr;
163100150303           wtpric = 'F';
163200150223           exsr CallTRKC76;
163300150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
163400150223             leavesr;
163500150223           ENDIF;
163600150223           wvalobj1 = OKC76pea;
163700150119       //?Confronto Fatturazione
163800150212         WHEN  V04obj1 = 'C';
163900150223           wfase = FaseObjCf;
164000150303           wtpric = 'F';
164100150223           exsr CallTRKC76;
164200150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
164300150223             leavesr;
164400150223           ENDIF;
164500150223           wvalobj1 = OKC76pea;
164600141124         ENDSL;
164700141124
164800141124       //?Cerco il "di cui" 2
164900141124         SELECT;
165000150128       //?Inizio
165100150212         WHEN  V04obj2 = 'I';
165200150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
165300150218           IF  not %found(TICMF01L);
165400141126             leavesr;
165500141126           ENDIF;
165600150218           wvalobj2 = CMFpea;
165700141124       //?Proposto
165800150212         WHEN  V04obj2 = 'P';
165900150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
166000150218           IF  not %found(TICMF01L);
166100141126             leavesr;
166200141126           ENDIF;
166300150218           wvalobj2 = CMFpea;
166400141124       //?Finale
166500150212         WHEN  V04obj2 = 'F';
166600150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
166700150218           IF  not %found(TICMF01L);
166800141126             leavesr;
166900141126           ENDIF;
167000150218           wvalobj2 = CMFpea;
167100150119       //?Trattativa
167200150212         WHEN  V04obj2 = 'T';
167300150223           wfase = FaseObjTtr;
167400150303           wtpric = 'F';
167500150223           exsr CallTRKC76;
167600150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
167700150223             leavesr;
167800150223           ENDIF;
167900150223           wvalobj2 = OKC76pea;
168000150119       //?Confronto Fatturazione
168100150212         WHEN  V04obj2 = 'C';
168200150223           wfase = FaseObjCf;
168300150303           wtpric = 'F';
168400150223           exsr CallTRKC76;
168500150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
168600150223             leavesr;
168700150223           ENDIF;
168800150223           wvalobj2 = OKC76pea;
168900141124         ENDSL;
169000141124
169100141127       //?controllo i 2 obiettivi
169200141127         SELECT;
169300150212         WHEN  V04formula = Formula(1) and wvalobj1 = wvalobj2;
169400141127           RecOK = *on;
169500150212         WHEN  V04formula = Formula(2) and wvalobj1 > wvalobj2;
169600141127           RecOK = *on;
169700150212         WHEN  V04formula = Formula(3) and wvalobj1 < wvalobj2;
169800141127           RecOK = *on;
169900150212         WHEN  V04formula = Formula(4) and wvalobj1 >= wvalobj2;
170000150119           RecOK = *on;
170100150212         WHEN  V04formula = Formula(5) and wvalobj1 <= wvalobj2;
170200141127           RecOK = *on;
170300150212         WHEN  V04formula = Formula(6) and wvalobj1 <> wvalobj2;
170400141127           RecOK = *on;
170500141127         ENDSL;
170600141124
170700141124       ENDSR;
170800150213
170900150213       //--------------------------------------------------------------
171000150213       //?Controllo se OK il record.
171100150213       //--------------------------------------------------------------
171200150213       BEGSR CTRrec1;
171300150213
171400150213         RecOk = *off;
171500150213         clear wvalobj3;
171600150213
171700150213       //?Cerco Obiettivo/Andamento richiesto
171800150213         SELECT;
171900150213       //?Inizio
172000150213         WHEN  V04obj3 = 'I';
172100150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
172200150218           IF  not %found(TICMF01L);
172300150213             leavesr;
172400150213           ENDIF;
172500150218           wvalobj3 = CMFpea;
172600150213       //?Proposto
172700150213         WHEN  V04obj3 = 'P';
172800150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
172900150218           IF  not %found(TICMF01L);
173000150213             leavesr;
173100150213           ENDIF;
173200150218           wvalobj3 = CMFpea;
173300150213       //?Finale
173400150213         WHEN  V04obj3 = 'F';
173500150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
173600150218           IF  not %found(TICMF01L);
173700150213             leavesr;
173800150213           ENDIF;
173900150218           wvalobj3 = CMFpea;
174000150213       //?Trattativa
174100150213         WHEN  V04obj3 = 'T';
174200150223           wfase = FaseObjTtr;
174300150303           wtpric = 'F';
174400150223           exsr CallTRKC76;
174500150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
174600150223             leavesr;
174700150213           ENDIF;
174800150223           wvalobj3 = OKC76pea;
174900150213       //?Confronto Fatturazione
175000150213         WHEN  V04obj3 = 'C';
175100150223           wfase = FaseObjCf;
175200150303           wtpric = 'F';
175300150223           exsr CallTRKC76;
175400150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
175500150223             leavesr;
175600150213           ENDIF;
175700150223           wvalobj3 = OKC76pea;
175800150213         ENDSL;
175900150213
176000150213       //?confronto con il "dal" - "al" richiesto
176100150213         IF  wvalobj3 >= wobj3da and wvalobj3 <= wobj3a;
176200150213           RecOK = *on;
176300150213         ENDIF;
176400150213
176500150213       ENDSR;
176600141028
176700141028       //--------------------------------------------------------------
176800141028       //?Carico i dati nel Subfile S03.
176900141028       //--------------------------------------------------------------
177000141028       BEGSR CarS03;
177100141028
177200150112         clear  KC01S03;
177300141127         PosCurOPZ = *off;
177400141028
177500141029       //?Imposto i campi da TICMI
177600141103         VS3clv = CMIclv;
177700141107         VS3ksu = CMIksu;
177800141107         VS3ksc = CMIksc;
177900141107         VS3cpo = CMIcpo;
178000150115       //?Imposto i campi da TICMC
178100141114         VS3ufe = CMCufe;
178200150115         VS3cch = CMCcch;
178300150115         clear VS3cchaut;
178400150115       //?Imposto se causale automatica
178500150115         IF  VS3cch <> *blanks;
178600150115           clear TIBS02DS;
178700150115           clear dECM;
178800150115           T02mod = 'C';
178900150115           T02cod = 'ECM';
179000150115           T02ke1 = VS3cch;
179100150115           T02sif = KNSIF;
179200150115           TNTBE_RicercaControllo  (kpjba : tibs02ds);
179300150115           dECM = T02uni;
179400150115           IF  §ECMaut <> *blanks;
179500150115             VS3cchaut = 'S';
179600150115           ENDIF;
179700150115         ENDIF;
179800150116         clear VS3tipocl;
179900141028         IF  CMIcln = 'N';
180000150116           VS3tipocl = '*';
180100141028         ENDIF;
180200150116         IF  CMCcch <> *blanks;
180300150213           VS3tipocl = 'B';
180400150116         ENDIF;
180500141028         clear TIBS69DS;
180600141028         clear ACOrag;
180700141028         I69kac = CMIksu;
180800141028         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
180900141103         VS3rag = ACOrag;
181000141103         VS3istat = CMIist;
181100141103         VS3del = CMIpde;
181200141103         VS3sped = CMInsp;
181300141103         VS3ric = CMIric;
181400141103         VS3pkgm = CMIpme;
181500141029
181600141029       //?Imposto i campi da TICMF
181700150128       //?Obiettivo inizio
181800141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
181900141113         IF  %found(TICMF01L);
182000141125           VS3obj = %editc(CMFpea:'J');
182100141113         ENDIF;
182200141111       //?Obiettivo Proposto
182300141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
182400141113         IF  %found(TICMF01L);
182500141125           VS3objprop = %editc(CMFpea:'J');
182600141127           VS3oprop = CMFpea;
182700141113         ENDIF;
182800141029       //?Obiettivo Finale
182900141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
183000141113         IF  %found(TICMF01L);
183100141125           VS3objfine = %editc(CMFpea:'J');
183200141113         ENDIF;
183300150220       //?Aumento da Trattativa
183400150220         wfase = FaseObjTtr;
183500150303         wtpric = 'F';
183600150220         exsr CallTRKC76;
183700150220         IF  OKC76err = *blanks;
183800150220           VS3esitotr = OKC76flag;
183900150223           VS3objttr = OKC76peaa;
184000150224           IF  OKC76anno > 0;
184100150224             VS3decotr = (OKC76mese * 10000) + OKC76anno;
184200150224           ENDIF;
184300150220           VS3nrv = OKC76nrv;
184400150225           VS3forza = OKC76forza;
184500150220         ENDIF;
184600141029       //?% Confronto Fatturazione
184700150220         wfase = FaseObjCf;
184800150303         wtpric = 'F';
184900150220         exsr CallTRKC76;
185000150220         IF  OKC76err = *blanks;
185100150223           VS3objcf = OKC76peaa;
185200150220         ENDIF;
185300141029
185400141029       //?Imposto descrizione del commerciale
185500141029         clear CMMdes;
185600141029         chain CMIcmm AZCMM01L;
185700141029         IF  %found(AZCMM01L);
185800141103           VS3cmmd = CMMdes;
185900141029         ENDIF;
186000141117
186100141117       //?Imposto ordinamento importanza cliente
186200141117         clear xx;
186300141117         xx = %lookup(VS3clv:skIC);
186400141117         IF  xx > 0;
186500141117           VS3ord = skIC_ord(xx);
186600141117         ELSE;
186700141117           clear VS3ord;
186800141117         ENDIF;
186900141028
187000141029         S03nrr += 1;
187100141124
187200141124       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
187300141124         IF  S03nrr >= 9999;
187400141124           wMaxNrr = *on;
187500141124           EndS03 = *on;
187600141124           leavesr;
187700141124         ENDIF;
187800141124
187900150112         write  KC01S03;
188000141028
188100141028       ENDSR;
188200141030
188300141030       //--------------------------------------------------------------
188400141030       //?Gestione tasto funzionale F01 da videata S03
188500141111       //?F01=Campagna
188600141030       //--------------------------------------------------------------
188700141030       BEGSR F01S03;
188800141112
188900150112         clear TRKC02DS;
189000150112         IKC02ric = 'I';
189100150212         IKC02ncm = VH4ncm;
189200150112         trkc02r (kpjba:trkc02ds);
189300141030
189400141030       ENDSR;
189500141030
189600141030       //--------------------------------------------------------------
189700141030       //?Gestione tasto funzionale F05 da videata S03
189800141030       //?F05=Altre Parzializzazioni
189900141030       //--------------------------------------------------------------
190000141030       BEGSR F05S03;
190100141030
190200141030       //?Videata per nuove parzializzazioni
190300141030         Video = 'D4';
190400141030
190500141030       ENDSR;
190600141030
190700141030       //--------------------------------------------------------------
190800141030       //?Gestione tasto funzionale F08 da videata S03
190900141030       //?F08=Ord. X Imp.Cliente
191000141030       //--------------------------------------------------------------
191100141030       BEGSR F08S03;
191200141030
191300141111       //?F8 visualizzato richiede ordinamento x RAG
191400141111         IF  VisOrdRAG;
191500141111           exsr Ordina_x_RAG;
191600141030           leavesr;
191700141030         ENDIF;
191800141030
191900141030       //?F8 visualizzato richiede ordinamento x CLV
192000141030         IF  VisOrdCLV;
192100141030           exsr Ordina_x_CLV;
192200141030           leavesr;
192300141030         ENDIF;
192400141030
192500141030       ENDSR;
192600141111
192700141111       //--------------------------------------------------------------
192800141111       //?Gestione tasto funzionale F10 da videata S03
192900141111       //?F10=Aggiunta Cliente
193000141111       //--------------------------------------------------------------
193100141111       BEGSR F10S03;
193200141111
193300150112         clear TRKC06DS;
193400150212         IKC06ncm = VH4ncm;
193500150112         trkc06r (kpjba:TRKC06DS);
193600141111
193700141111       //?Ricarico il subfile
193800141111         wInzS03 = *on;
193900141111
194000141111       ENDSR;
194100141028
194200141028       //--------------------------------------------------------------
194300141028       //?Gestione tasto funzionale F12 da videata S03
194400141028       //?F12=Ritorno
194500141028       //--------------------------------------------------------------
194600141028       BEGSR F12S03;
194700141028
194800141028       //?Ritorno alle selezioni
194900141028         Video = 'D2';
195000141029         wInzD02 = *on;
195100150225         clear sav_s03nrr;
195200150223
195300150223       //?Se richiamato chiudo i file per il pgm TRKC76R
195400150223         IF  wTRKC76;
195500150223           clear TRKC76DS;
195600150223           IKC76ric = 'C';
195700150223           trkc76r (kpjba:TRKC76DS);
195800150223         ENDIF;
195900141028
196000141028       ENDSR;
196100141127
196200141127       //--------------------------------------------------------------
196300141127       //?Gestione tasto funzionale F13 da videata S03
196400141127       //?F13=Conferma Obiettivo proposto.
196500141127       //--------------------------------------------------------------
196600141127       BEGSR F13S03;
196700141127
196800150121         Video = 'D7';
196900150121         wInzD07 = *on;
197000141127
197100141127       ENDSR;
197200141030
197300141030       //--------------------------------------------------------------
197400141111       //?Ordinamento x RAG Subfile S03.
197500141030       //--------------------------------------------------------------
197600141111       BEGSR Ordina_x_RAG;
197700141030
197800141030         VisOrdCLV = *on;
197900141111         VisOrdRAG = *off;
198000141030
198100141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
198200141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
198300141030       // il campo è usato per selezionare i records dando un ordine a quelli
198400141030       // selezionati davanti ai non selezionati.
198500141030         clear QLGSCB;
198600141030         clear QLGSCB00;
198700141030
198800141118       // 2 campi chiave x Ragione Sociale - Codice (KSU)
198900141030         QLGNBRK = 1;
199000141030
199100141111       // imposto la posizione del RAG sul subfile e la lunghezza
199200141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
199300141117         QLGSP = 1 + %size(VS3clv) + %size(VS3ord)
199400150116                   + %size(VS3ksu) + %size(VS3tipocl);
199500141111         QLGSS = %SIZE(VS3rag);
199600141118         QLGDT = Carattere;
199700141030         QLGSO = Ascendente;
199800141030         QLGKL(1) = QLGSKL;
199900141118
200000141118       // imposto la posizione del KSU sul subfile e la lunghezza
200100141118       // l'ordinamento è su un campo numerico e deve essere ascendente
200200141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord);
200300141118         QLGSS = %SIZE(VS3ksu);
200400141118         QLGDT = Numerico;
200500141118         QLGSO = Ascendente;
200600141118         QLGKL(2) = QLGSKL;
200700141030
200800141030       // Load other sort parameters.
200900141030         QLGLB = 80 + 16 * MaxKey;
201000141030         QLGRL = %SIZE(sflrcd) - 1;
201100141030         QLGRT = 8;
201200141030         QLGOKL = 80;
201300141030         QLGLKE = 16;
201400141030         QLGLSS = 290;
201500141030
201600141030       // Initialize Sort I/O API fields.
201700141030         QLGRL00 = QLGRL;
201800141030         QLGRC00 = 1;
201900141030         clear QUSEI;
202000141030         QUSBPRV = %SIZE(QUSEC);
202100141030
202200141030       // First step - Initialize the sort routine.
202300141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
202400141030
202500141030       // Next step - Write records to I/O routine.
202600141030         QLGRT00 = Put;
202700141103         FOR  xx = 1 to rrnlast;
202800150112           chain xx KC01S03;
202900141030
203000141030       // solo le righe con Selected = 'Y' sono riordinate,
203100141030       // quindi per fare un ordinamento di tutte le righe
203200141030       // metto 'Y' sempre.
203300141103           selected  = 'Y';
203400141103           clear QUSEI;
203500141103           QUSBPRV = %SIZE(QUSEC);
203600141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
203700141103         ENDFOR;
203800141030
203900141030       // Next step - Signal end of input, clear subfile for reload.
204000141030         QLGRT00 = EndPut;
204100141030         clear QUSEI;
204200141030         QUSBPRV = %SIZE(QUSEC);
204300141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
204400141030       // pulizia SFL
204500141030         exsr PulS03;
204600141030
204700141030       // Final step - Write the records back to the subfile.
204800141030         QLGRT00 = Get;
204900141103         FOR  xx = 1 to rrnlast;
205000141103           clear QUSEI;
205100141103           QUSBPRV = %SIZE(QUSEC);
205200141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
205300141111           attrrag = visHI;
205400141103           clear attrclv;
205500150121           IF  VS3objcf <> *blanks;
205600141103             attrobjcf = visRI;
205700141103           ELSE;
205800141103             clear attrobjcf;
205900141103           ENDIF;
206000141121           IF  VS3esitotr <> *blanks;
206100141121             Visesito = *on;
206200141121           ELSE;
206300141121             Visesito = *off;
206400141121           ENDIF;
206500141103           s03nrr = xx;
206600150112           write KC01S03;
206700141103         ENDFOR;
206800141112
206900141117         rrnlast = S03nrr;
207000141112         IF  S03nrr > *zeros;
207100141112           V03rcd = 1;
207200141112           V03csr = 1;
207300141112         ELSE;
207400141112           clear V03rcd;
207500141112         ENDIF;
207600141030
207700141030       ENDSR;
207800141030
207900141030       //--------------------------------------------------------------
208000141030       //?Ordinamento x CLV Subfile S03.
208100141030       //--------------------------------------------------------------
208200141030       BEGSR Ordina_x_CLV;
208300141030
208400141030         VisOrdCLV = *off;
208500141111         VisOrdRAG = *on;
208600141030
208700141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
208800141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
208900141030       // il campo è usato per selezionare i records dando un ordine a quelli
209000141030       // selezionati davanti ai non selezionati.
209100141030         clear QLGSCB;
209200141030         clear QLGSCB00;
209300141030
209400150115       // 3 campi chiave x Importanza cliente, Fatturato e RAG
209500150115         QLGNBRK = 3;
209600141030
209700141118       // imposto la posizione del ORD CLV sul subfile e la lunghezza
209800141118       // l'ordinamento è su un campo numerico e deve essere discendente
209900141117         QLGSP = 1 + %size(VS3clv);
210000141117         QLGSS = %SIZE(VS3ord);
210100141117         QLGDT = Numerico;
210200141117         QLGSO = Discendente;
210300141030         QLGKL(1) = QLGSKL;
210400150115
210500150115       // imposto la posizione del Fatturato sul subfile e la lunghezza
210600150115       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
210700150115         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
210800150116                   + %size(VS3ksu) + %size(VS3tipocl)
210900150115                   + %size(VS3rag) + %size(VS3istat)
211000150115                   + %size(VS3del) + %size(VS3sped);
211100150115         QLGSS = %SIZE(VS3ric);
211200150115         QLGDT = Numerico;
211300150115         QLGSO = Discendente;
211400150115         QLGKL(2) = QLGSKL;
211500141118
211600141118       // imposto la posizione del RAG sul subfile e la lunghezza
211700141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
211800141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
211900150116                   + %size(VS3ksu) + %size(VS3tipocl);
212000141118         QLGSS = %SIZE(VS3rag);
212100141118         QLGDT = Carattere;
212200141118         QLGSO = Ascendente;
212300150115         QLGKL(3) = QLGSKL;
212400141030
212500141030       // Load other sort parameters.
212600141030         QLGLB = 80 + 16 * MaxKey;
212700141030         QLGRL = %SIZE(sflrcd) - 1;
212800141030         QLGRT = 8;
212900141030         QLGOKL = 80;
213000141030         QLGLKE = 16;
213100141030         QLGLSS = 290;
213200141030
213300141030       // Initialize Sort I/O API fields.
213400141030         QLGRL00 = QLGRL;
213500141030         QLGRC00 = 1;
213600141030         clear QUSEI;
213700141030         QUSBPRV = %SIZE(QUSEC);
213800141030
213900141030       // First step - Initialize the sort routine.
214000141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
214100141030
214200141030       // Next step - Write records to I/O routine.
214300141030         QLGRT00 = Put;
214400141103         FOR  xx = 1 to rrnlast;
214500150112           chain xx KC01S03;
214600141030
214700141030       // solo le righe con Selected = 'Y' sono riordinate,
214800141030       // quindi per fare un ordinamento di tutte le righe
214900141030       // metto 'Y' sempre.
215000141103           selected  = 'Y';
215100141103           clear QUSEI;
215200141103           QUSBPRV = %SIZE(QUSEC);
215300141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
215400141103         ENDFOR;
215500141030
215600141030       // Next step - Signal end of input, clear subfile for reload.
215700141030         QLGRT00 = EndPut;
215800141030         clear QUSEI;
215900141030         QUSBPRV = %SIZE(QUSEC);
216000141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
216100141030       // pulizia SFL
216200141030         exsr PulS03;
216300141030
216400141030       // Final step - Write the records back to the subfile.
216500141030         QLGRT00 = Get;
216600141103         FOR  xx = 1 to rrnlast;
216700141103           clear QUSEI;
216800141103           QUSBPRV = %SIZE(QUSEC);
216900141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
217000141111           clear attrrag;
217100141103           attrclv = visHI;
217200150121           IF  VS3objcf <> *blanks;
217300141103             attrobjcf = visRI;
217400141103           ELSE;
217500141103             clear attrobjcf;
217600141103           ENDIF;
217700141121           IF  VS3esitotr <> *blanks;
217800141121             Visesito = *on;
217900141121           ELSE;
218000141121             Visesito = *off;
218100141121           ENDIF;
218200141103           s03nrr = xx;
218300150112           write KC01S03;
218400141103         ENDFOR;
218500141112
218600141117         rrnlast = S03nrr;
218700141112         IF  S03nrr > *zeros;
218800141112           V03rcd = 1;
218900141112           V03csr = 1;
219000141112         ELSE;
219100141112           clear V03rcd;
219200141112         ENDIF;
219300141030
219400141030       ENDSR;
219500141028
219600141028       //--------------------------------------------------------------
219700141028       //?Gestione opzioni Subfile S03.
219800141028       //--------------------------------------------------------------
219900141028       BEGSR OpzS03;
220000141028
220100150112         readc KC01S03;
220200141028
220300150112         DOW  NOT  %eof(TRKC01D);
220400141028
220500141028           SflNxtChg = *off;
220600141028           WindDspF  = IndDspF;
220700141028           reset WindDspFb;
220800141028           IndDspF   = WindDspF;
220900141028           V03rcd    = S03nrr;
221000141028
221100141127           PosCurOPZ = *off;
221200141119           sav_S03nrr = S03nrr;
221300141119
221400141119         //?- Verifico se il cliente è gestibile dall'utente
221500141119           IF  VS3opz <> *blank;
221600141119             clear TNTAA1DS;
221700141119             ITAA1caut = '§utegtc';
221800141119             ITAA1ksc = VS3ksu;
221900141119             kpjbu = TNTAA1DS;
222000141119             tntaa1c (kpjba);
222100150217             TNTAA1DS = kpjbu;
222200141119             IF  OTAA1fabi = 'N';
222300141119               ErrGenerico = *on;
222400141119               ErrMessage  = *on;
222500141127               PosCurOPZ   = *on;
222600150216               V03msg = %trim(Msg(07)) + '. Non in gestione';
222700141119             ENDIF;
222800141119           ENDIF;
222900141028
223000141119         //?- Se ok il cliente
223100141119           IF  not ErrMessage;
223200141119
223300141119             SELECT;
223400141119           //?- Nessuna opzione
223500141119             WHEN  VS3opz = *blank;
223600141028
223700141119           //?- 2 = Modifica
223800150115             WHEN  VS3opz = '2' and VS3cch = *blanks;
223900150127             //?anche in caso di periodo fuori dal range ma utente di sede
224000150115               IF  Oggi < CMPdigo or Oggi > CMPdfgo;
224100150216                 IF  DUTpou = 046;
224200150127                   exsr Opzione2;
224300150127                 ELSE;
224400150115                 ErrMessage  = *on;
224500150115                 ErrGenerico = *on;
224600150115                 PosCurOPZ   = *on;
224700150216                 V03msg = Msg(12);
224800150306                 %subst(V03msg:34:10) =
224900150306                 %subst(%editc(CMPdigo:'X'):7:2) + '/' +
225000150306                 %subst(%editc(CMPdigo:'X'):5:2) + '/' +
225100150306                 %subst(%editc(CMPdigo:'X'):1:4);
225200150306                 %subst(V03msg:48:10) =
225300150306                 %subst(%editc(CMPdfgo:'X'):7:2) + '/' +
225400150306                 %subst(%editc(CMPdfgo:'X'):5:2) + '/' +
225500150306                 %subst(%editc(CMPdfgo:'X'):1:4);
225600150127                 ENDIF;
225700150115               ELSE;
225800150115                 exsr Opzione2;
225900150115               ENDIF;
226000141028
226100150116           //?- 5 = Statistica
226200150116             WHEN  VS3opz = '5';
226300150116               exsr Opzione5;
226400141028
226500141119           //?- I = Interrogazione Clienti
226600141119             WHEN  VS3opz = 'I';
226700141119               exsr OpzioneI;
226800141028
226900141119           //?- T = Gestione Trattativa
227000141119             WHEN  VS3opz = 'T' and VS3nrv > 0;
227100141119               exsr OpzioneT;
227200141028
227300141119           //?- S = Storico Fasi
227400141119             WHEN  VS3opz = 'S';
227500141119               exsr OpzioneS;
227600150127
227700150127           //?- N = Note
227800150127             WHEN  VS3opz = 'N';
227900150127               exsr OpzioneN;
228000150220
228100150220           //?- F = Trattativa Forzata
228200150220             WHEN  VS3opz = 'F';
228300150220               exsr OpzioneF;
228400141028
228500141119             OTHER;
228600141119               ErrMessage  = *on;
228700141119               ErrGenerico = *on;
228800141127               PosCurOPZ   = *on;
228900150216               V03msg = Msg(11);
229000141119             ENDSL;
229100141119           ENDIF;
229200141028
229300141028           //?Aggiornamento sfl
229400141028           SELECT;
229500141028             WHEN  ErrMessage;
229600141028               SflNxtChg = *on;
229700141028               V03csr    = V03rcd;
229800141028             WHEN  ErrGenerico;
229900141028               V03csr = V03rcd;
230000141103               clear VS3opz;
230100141028             OTHER;
230200141028               V03csr = V03rcd;
230300141103               clear VS3opz;
230400141028           ENDSL;
230500150128
230600150128           IF  VS3esitotr <> *blanks;
230700150128             Visesito = *on;
230800150128           ELSE;
230900150128             Visesito = *off;
231000150128           ENDIF;
231100141028
231200150112           update KC01S03;
231300141028
231400141028           IF  ErrMessage or ErrGenerico;
231500141028             leave;
231600141028           ENDIF;
231700141028
231800150112           readc KC01S03;
231900141028
232000141028         ENDDO;
232100141118
232200141118       //?Ricarico il subfile
232300141119         IF  not ErrGenerico and not ErrMessage;
232400141119           wInzS03 = *on;
232500141119         ENDIF;
232600141028
232700141028       ENDSR;
232800141113
232900141113       //--------------------------------------------------------------
233000141113       //?Opzione "2" videata S03.
233100141113       //--------------------------------------------------------------
233200141113       BEGSR Opzione2;
233300141113
233400141114       //?Richiamo pgm x sapere quale obiettivo posso variare
233500150112         clear TRKC72DS;
233600150212         IKC72ncm = VH4ncm;
233700150112         IKC72ksu = VS3ksu;
233800150112         IKC72ksc = VS3ksc;
233900150112         IKC72cpo = VS3cpo;
234000150112         IKC72acm = VS3ufe;
234100150127       //?Se utente di sede e non siamo più nel periodo possibile
234200150127       //?per la modifica obiettivi e l'ultima fase è la 10 forzo come
234300150127       //?ultima fase la 20, in questo modo il pgm di ma la possibilità
234400150127       //?di inserire la 30
234500150127         IF  IKC72acm = FaseObj and DUTpou = 046 and
234600150127            (Oggi < CMPdigo or Oggi > CMPdfgo);
234700150127           IKC72acm = FaseObjProp;
234800150127         ENDIF;
234900150112         trkc72r (kpjba:TRKC72DS);
235000150112         IF  OKC72err <> *blanks;
235100141114           ErrMessage  = *on;
235200141114           ErrGenerico = *on;
235300141127           PosCurOPZ   = *on;
235400150112           V03msg = OKC72msg;
235500141114           leavesr;
235600141114         ENDIF;
235700150115
235800150115       //?Se tutto OK
235900150115       //?richiamo pgm per la variazione obiettivo
236000150115         clear TRKC07DS;
236100150115         IKC07ric = '2';
236200150212         IKC07ncm = VH4ncm;
236300150112         IKC07ksu = VS3ksu;
236400150112         IKC07ksc = VS3ksc;
236500150112         IKC07cpo = VS3cpo;
236600150112         IKC07acm = OKC72acm;
236700150112         trkc07r (kpjba:TRKC07DS);
236800141113
236900141113       ENDSR;
237000150115
237100150115       //--------------------------------------------------------------
237200150115       //?Opzione "R" videata S03.
237300150115       //--------------------------------------------------------------
237400150115       BEGSR OpzioneR;
237500150115
237600150115       //?richiamo il pgm per ripristinare il cliente in campagna
237700150115         clear TRKC07DS;
237800150115         IKC07ric = 'R';
237900150212         IKC07ncm = VH4ncm;
238000150115         IKC07ksu = VS3ksu;
238100150115         IKC07ksc = VS3ksc;
238200150115         IKC07cpo = VS3cpo;
238300150115         IKC07acm = VS3ufe;
238400150115         trkc07r (kpjba:TRKC07DS);
238500150115
238600150115       ENDSR;
238700141103
238800141103       //--------------------------------------------------------------
238900150116       //?Opzione "5" videata S03.
239000141103       //--------------------------------------------------------------
239100150116       BEGSR Opzione5;
239200150217
239300150217       //?- Verifico se l'opzione è utilizzabile dall'utente
239400150217          clear TNTAA1DS;
239500150217          ITAA1caut = '§uteist';
239600150217          ITAA1ksc = VS3ksu;
239700150217          kpjbu = TNTAA1DS;
239800150217          tntaa1c (kpjba);
239900150217          TNTAA1DS = kpjbu;
240000150217          IF  OTAA1fabi = 'N';
240100150217            ErrGenerico = *on;
240200150217            ErrMessage  = *on;
240300150217            PosCurOPZ   = *on;
240400150217            V03msg = %trim(Msg(07)) + '. Non in gestione';
240500150217            leavesr;
240600150217          ENDIF;
240700141103
240800141103         clear TISE60DS;
240900141103         D60op0 = 'VIS';
241000141103         D60op1 = 'F06';
241100141103         D60sce = '3';
241200141107         dsKSC = VS3ksu;
241300141103         skKSA(1) = dsKSA;
241400141103         kpjbu = TISE60DS;
241500141103         tise61r (kpjba);
241600141103
241700141103       ENDSR;
241800141103
241900141103       //--------------------------------------------------------------
242000141103       //?Opzione "I" videata S03.
242100141103       //--------------------------------------------------------------
242200141103       BEGSR OpzioneI;
242300141103
242400141103         clear TNTAI2DS;
242500150122       //?se non è utente di sede richiamo il pgm in modo da poter gestire
242600150122       //?le eventuali attività ancora da eseguire + crearne delle nuove
242700150122         IF  DUTpou <> 046;
242800150122           ITAI2ric = 'A';
242900150122         ENDIF;
243000141107         ITAI2ksc = VS3ksu;
243100141103         tntai2r (kpjba:TNTAI2DS);
243200141103
243300141103       ENDSR;
243400141103
243500141103       //--------------------------------------------------------------
243600141103       //?Opzione "T" videata S03.
243700141103       //--------------------------------------------------------------
243800141103       BEGSR OpzioneT;
243900141103
244000141103         clear TNTA88DS;
244100141103         ITA88fle = '5';
244200141103         ITA88nrv = VS3nrv;
244300141103         tnta88r (kpjba:TNTA88DS);
244400141103
244500141103       ENDSR;
244600141107
244700141107       //--------------------------------------------------------------
244800141107       //?Opzione "S" videata S03.
244900141107       //--------------------------------------------------------------
245000141107       BEGSR OpzioneS;
245100141107
245200150112         clear TRKC03DS;
245300150212         IKC03ncm = VH4ncm;
245400150112         IKC03ksu = VS3ksu;
245500150112         IKC03ksc = VS3ksc;
245600150112         IKC03cpo = VS3cpo;
245700150112         trkc03r (kpjba:TRKC03DS);
245800141107
245900141107       ENDSR;
246000141113
246100141113       //--------------------------------------------------------------
246200141113       //?Opzione "E" videata S03.
246300141113       //--------------------------------------------------------------
246400141113       BEGSR OpzioneE;
246500141114
246600141114       //?richiamo pgm per escludere il cliente dalla campagna
246700150112         clear TRKC07DS;
246800150112         IKC07ric = '2';
246900150212         IKC07ncm = VH4ncm;
247000150112         IKC07ksu = VS3ksu;
247100150112         IKC07ksc = VS3ksc;
247200150112         IKC07cpo = VS3cpo;
247300150112         IKC07acm = FaseChiudi;
247400150112         trkc07r (kpjba:TRKC07DS);
247500141113
247600141113       ENDSR;
247700150127
247800150127       //--------------------------------------------------------------
247900150127       //?Opzione "N" videata S03.
248000150127       //--------------------------------------------------------------
248100150127       BEGSR OpzioneN;
248200150127
248300150127       //?richiamo pgm per gestione Note
248400150127         clear TRKC10DS;
248500150127         IKC10tla = 'L';
248600150212         IKC10ncm = VH4ncm;
248700150127         IKC10ksu = VS3ksu;
248800150127         IKC10ksc = VS3ksc;
248900150127         IKC10cpo = VS3cpo;
249000150127         IKC10acm = VS3ufe;
249100150127         trkc10r (kpjba:TRKC10DS);
249200150127
249300150127       ENDSR;
249400150220
249500150220       //--------------------------------------------------------------
249600150220       //?Opzione "F" videata S03.
249700150220       //--------------------------------------------------------------
249800150220       BEGSR OpzioneF;
249900150225
250000150225       //?Posso fare la fase TF solo nel periodo di inserimento trattative
250100150225         IF  Oggi < CMPdit or Oggi > CMPdft;
250200150225           ErrMessage  = *on;
250300150225           ErrGenerico = *on;
250400150225           PosCurOPZ   = *on;
250500150225           V03msg = Msg(23);
250600150306           %subst(V03msg:39:10) =
250700150306           %subst(%editc(CMPdit:'X'):7:2) + '/' +
250800150306           %subst(%editc(CMPdit:'X'):5:2) + '/' +
250900150306           %subst(%editc(CMPdit:'X'):1:4);
251000150306           %subst(V03msg:53:10) =
251100150306           %subst(%editc(CMPdft:'X'):7:2) + '/' +
251200150306           %subst(%editc(CMPdft:'X'):5:2) + '/' +
251300150306           %subst(%editc(CMPdft:'X'):1:4);
251400150225           leavesr;
251500150225         ENDIF;
251600150220
251700150220       //?Posso immettere la Trattativa Forzata se non presente NESSUNA fase TR
251800150220         chain (VH4ncm:VS3ksu:VS3ksc:VS3cpo:FaseObjTtr) TICMF01L;
251900150220         IF  %found(TICMF01L);
252000150220           ErrMessage  = *on;
252100150220           ErrGenerico = *on;
252200150220           PosCurOPZ   = *on;
252300150220           V03msg = %trim(Msg(11)) + '. Già presente una Trattativa';
252400150220           leavesr;
252500150220         ENDIF;
252600150220
252700150220       //?richiamo pgm per immissione Trattativa Forzata
252800150220         clear TRKC07DS;
252900150220         IKC07ric = '2';
253000150220         IKC07ncm = VH4ncm;
253100150220         IKC07ksu = VS3ksu;
253200150220         IKC07ksc = VS3ksc;
253300150220         IKC07cpo = VS3cpo;
253400150220         IKC07acm = FaseObjTf;
253500150220         trkc07r (kpjba:TRKC07DS);
253600150220
253700150220       ENDSR;
253800141030
253900141030       //--------------------------------------------------------------
254000141030       //?Gestione videata D04.
254100141030       //--------------------------------------------------------------
254200141030       BEGSR GesD04;
254300141030
254400141030       //?Inizializzazione videata
254500141030         IF  wInzD04;
254600141030           exsr InzD04;
254700141030           wInzD04 = *off;
254800141030         ENDIF;
254900141030
255000141030       //?Emissione videata
255100150112         exfmt  KC01W04;
255200141030         reset ErrMessage;
255300141030         reset ErrGenerico;
255400141030         clear V04msg;
255500141030
255600141030         SELECT;
255700150119
255800150119       //?- F01=Formula
255900150119           WHEN  dsp_aid = c_F01;
256000150119             exsr F01D04;
256100150128
256200150128       //?- F02=Obiettivo/Andamento
256300150128           WHEN  dsp_aid = c_F02;
256400150128             exsr F02D04;
256500150302
256600150302       //?- F04=Scelta Trattative
256700150302           WHEN  dsp_aid = c_F04;
256800150302             exsr F04D04;
256900141103
257000141103       //?- F06=Conferma
257100141103           WHEN  dsp_aid = c_F06;
257200141104             exsr ContrD04;
257300141104             IF  ErrGenerico;
257400141104               leavesr;
257500141104             ENDIF;
257600141103             exsr F06D04;
257700141030
257800141030       //?- F12=Ritorno
257900141030           WHEN  dsp_aid = c_F12;
258000141030             exsr F12D04;
258100141030
258200141030       //?Invio
258300141030           OTHER;
258400141030             exsr ContrD04;
258500141030             IF  ErrGenerico;
258600141030               leavesr;
258700141030             ENDIF;
258800141030
258900141030         ENDSL;
259000141030
259100141030       ENDSR;
259200141030
259300141030       //--------------------------------------------------------------
259400141030       //?Inizializzazione Videata D04.
259500141030       //--------------------------------------------------------------
259600141030       BEGSR InzD04;
259700141030
259800141030       //?Pulizia videata
259900150112         clear KC01W04;
260000150212
260100150212         V04ncm = V02ncm;
260200150212         V04des = V02des;
260300150212         VH4ncm = %dec(V02ncm:7:0);
260400141030
260500150212         V04cdi = V02cdi;
260600150212         V04cdid = V02cdid;
260700150216         IF  V02car <> *blanks;
260800150212           V04car = V02car;
260900150212           V04card = V02card;
261000150212           VH4car = %dec(V02car:3:0);
261100141030         ENDIF;
261200150216         IF  V02fil <> *blanks;
261300150212           V04fil = V02fil;
261400150212           V04fild = V02fild;
261500150212           VH4fil = %dec(V02fil:3:0);
261600141030         ENDIF;
261700150216         IF  V02cmm <> *blanks;
261800150212           V04cmm = V02cmm;
261900150212           V04cmmd = V02cmmd;
262000150212           VH4cmm = %dec(V02cmm:7:0);
262100141030         ENDIF;
262200150216         IF  V02ksu <> *blanks;
262300150212           V04ksu = V02ksu;
262400150212           V04rag = V02rag;
262500150212           VH4ksu = %dec(V02ksu:7:0);
262600141030         ENDIF;
262700150212         V04clv1 = V02clv1;
262800150212         V04clv2 = V02clv2;
262900150212         V04clv3 = V02clv3;
263000150212         V04clv4 = V02clv4;
263100150212         V04clv5 = V02clv5;
263200150212         V04deldas = V02deldas;
263300150212         V04delda = V02delda;
263400150212         V04delas = V02delas;
263500150212         V04dela = V02dela;
263600150212         V04cli = V02cli;
263700150212         V04ncli = V02ncli;
263800150212         V04ttr = V02ttr;
263900150212         V04obj = V02obj;
264000150212         V04obj1 = V02obj1;
264100150212         V04obj2 = V02obj2;
264200150212         V04formula = V02formula;
264300150213         V04obj3 = V02obj3;
264400150213         V04obj3das = V02obj3das;
264500150213         V04obj3da = V02obj3da;
264600150213         V04obj3as = V02obj3as;
264700150213         V04obj3a = V02obj3a;
264800150223
264900150223       //?Se richiamato chiudo i file per il pgm TRKC76R
265000150223         IF  wTRKC76;
265100150223           clear TRKC76DS;
265200150223           IKC76ric = 'C';
265300150223           trkc76r (kpjba:TRKC76DS);
265400150223         ENDIF;
265500141030
265600141030       ENDSR;
265700141030
265800141030       //--------------------------------------------------------------
265900141030       //?Controllo Videata D04.
266000141030       //--------------------------------------------------------------
266100141030       BEGSR ContrD04;
266200141030
266300141030         WindDspF = IndDspF;
266400141030         reset WindDspFb;
266500141030         IndDspF  = WindDspF;
266600141119
266700141119         clear wdelda;
266800141119         clear wdela;
266900150212
267000150212       //?Campagna Commerciale Obbligatoria
267100150212         clear V04des;
267200150305         clear VH4ncm;
267300150212         IF  V04ncm = *blanks;
267400150212           ErrMessage  = *on;
267500150212           ErrGenerico = *on;
267600150212           PosCurNCM   = *on;
267700150212           V04msg = Msg(02);
267800150212           leavesr;
267900150212         ENDIF;
268000150212       //?Ricerca Campagna Commerciale
268100150212         IF  %scan('?':V04ncm) > 0;
268200150212           clear TRKC02DS;
268300150212           IKC02ric = 'R';
268400150212           trkc02r (kpjba:trkc02ds);
268500150212           ErrGenerico = *on;
268600150212           PosCurNCM   = *on;
268700150212           V04ncm = %editc(OKC02ncm:'X');
268800150212         ENDIF;
268900150212       //?Accetto solo dati numerici
269000150212         xx = 1;
269100150212         FOR xx by 1 to %len(V04ncm);
269200150212           IF  %subst(V04ncm:xx:1) <> *blanks and
269300150212               %check(Digits:%subst(V04ncm:xx:1)) > *zeros;
269400150212             ErrMessage  = *on;
269500150212             ErrGenerico = *on;
269600150212             PosCurNCM   = *on;
269700150212             V04msg = Msg(02);
269800150212             leavesr;
269900150212           ENDIF;
270000150212         ENDFOR;
270100150212       //?Deve essere una Campagna Commerciale valida
270200150212         CMPncm = %dec(V04ncm:7:0);
270300150212         chain CMPncm TICMP01L;
270400150212         IF  not %found(TICMP01L);
270500150212           ErrMessage  = *on;
270600150212           ErrGenerico = *on;
270700150212           PosCurNCM   = *on;
270800150212           V04msg = Msg(02);
270900150212           leavesr;
271000150212         ENDIF;
271100150212       //?Decodifico Campagna Commerciale
271200150212         V04des = CMPdes;
271300150505       ////?Campagna Commerciale già finita
271400150505         //IF  CMPdfc < Oggi;
271500150505         //  ErrMessage  = *on;
271600150505         //  ErrGenerico = *on;
271700150505         //  PosCurNCM   = *on;
271800150505         //  V04msg = %trim(Msg(02)) + '. Già chiusa';
271900150505         //  leavesr;
272000150505         //ENDIF;
272100150212       //?In carico all'Utente
272200150212         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
272300150212           ErrMessage  = *on;
272400150212           ErrGenerico = *on;
272500150212           PosCurNCM   = *on;
272600150212           V04msg = %trim(Msg(02)) + '. Non in gestione';
272700150212           leavesr;
272800150212         ENDIF;
272900150212         VH4ncm = %dec(V04ncm:7:0);
273000141030
273100141030       //?Distretto
273200141030         w001a = V04cdi;
273300141030         clear w050a;
273400141030         exsr Distretto;
273500141030         V04cdi = w001a;
273600141030         v04cdid = w050a;
273700141030         V04msg = wmsg;
273800141030         IF  Errgenerico;
273900141030           leavesr;
274000141030         ENDIF;
274100141030
274200141030       //?Area
274300150305         clear VH4car;
274400141030         w003a = V04car;
274500141030         clear w050a;
274600141030         exsr Area;
274700141030         V04car = w003a;
274800141030         V04card = w050a;
274900141030         V04msg = wmsg;
275000141030         IF  ErrGenerico;
275100141030           leavesr;
275200141030         ENDIF;
275300141030       //?Se presente anche il distretto devono essere congruenti
275400141030         IF  V04cdi <> *blanks and V04car <> *blanks;
275500150219           chain (V04cdi:w0030) AZORG02L;
275600141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
275700141030             ErrGenerico = *on;
275800141030             ErrMessage  = *on;
275900141030             PosCurCAR   = *on;
276000141030             V04msg = %trim(Msg(04)) + '. Non congruente con il distretto';
276100141030             leavesr;
276200141030           ENDIF;
276300141030         ENDIF;
276400150212         IF  V04car <> *blanks;
276500150212           VH4car = %dec(V04car:3:0);
276600150212         ENDIF;
276700141030
276800141030       //?Filiale Commerciale
276900150305         clear VH4fil;
277000141030         w003a = V04fil;
277100141030         clear w050a;
277200141030         exsr Filiale;
277300141030         V04fil = w003a;
277400141030         V04fild = w050a;
277500141030         V04msg = wmsg;
277600141030         IF  ErrGenerico;
277700141030           leavesr;
277800141030         ENDIF;
277900150212         IF  V04fil <> *blanks;
278000150212           VH4fil = %dec(V04fil:3:0);
278100150212         ENDIF;
278200141030
278300141030       //?Commerciale Unificante
278400150305         clear VH4cmm;
278500141030         w007a = V04cmm;
278600141030         clear w050a;
278700141030         exsr Commerciale;
278800141030         V04cmm = w007a;
278900141030         V04cmmd = w050a;
279000141103         V04msg = wmsg;
279100141030         IF  ErrGenerico;
279200141030           leavesr;
279300141030         ENDIF;
279400150212         IF  V04cmm <> *blanks;
279500150212           VH4cmm = %dec(V04cmm:7:0);
279600150212         ENDIF;
279700141030
279800141030       //?Cliente
279900150305         clear VH4ksu;
280000141107         w007a = V04ksu;
280100141030         clear w050a;
280200141030         exsr Cliente;
280300141107         V04ksu = w007a;
280400141030         V04rag = w050a;
280500141030         V04msg = wmsg;
280600141030         IF  ErrGenerico;
280700141030           leavesr;
280800141030         ENDIF;
280900150212         IF  V04ksu <> *blanks;
281000150212           VH4ksu = %dec(V04ksu:7:0);
281100150212         ENDIF;
281200141030
281300141030       //?Importanza Cliente
281400141030         w001a = V04clv1;
281500141030         clear w050a;
281600141030         exsr ImpCliente;
281700150219         V04clv1 = w001a;
281800141103         V04msg = wmsg;
281900141030         IF  ErrGenerico;
282000141030           PosCurCLV1 = *on;
282100141030           leavesr;
282200141030         ENDIF;
282300141030         w001a = V04clv2;
282400141030         clear w050a;
282500141030         exsr ImpCliente;
282600141030         V04clv2 = w001a;
282700141030         V04msg = wmsg;
282800141030         IF  ErrGenerico;
282900141030           PosCurCLV2 = *on;
283000141030           leavesr;
283100141030         ENDIF;
283200141030         w001a = V04clv3;
283300141030         clear w050a;
283400141030         exsr ImpCliente;
283500141030         V04clv3 = w001a;
283600141030         V04msg = wmsg;
283700141030         IF  ErrGenerico;
283800141030           PosCurCLV3 = *on;
283900141030           leavesr;
284000141030         ENDIF;
284100141030         w001a = V04clv4;
284200141030         clear w050a;
284300141030         exsr ImpCliente;
284400141030         V04clv4 = w001a;
284500141030         V04msg = wmsg;
284600141030         IF  ErrGenerico;
284700141030           PosCurCLV4 = *on;
284800141030           leavesr;
284900141030         ENDIF;
285000141030         w001a = V04clv5;
285100141030         clear w050a;
285200141030         exsr ImpCliente;
285300141030         V04clv5 = w001a;
285400141030         V04msg = wmsg;
285500141030         IF  ErrGenerico;
285600141030           PosCurCLV5 = *on;
285700141030           leavesr;
285800141030         ENDIF;
285900141030
286000141030       //?Delta
286100141030         IF  V04delda > *zeros or V04dela <> *zeros;
286200141030         //?Obbligatori tutti e 2 se immesso uno dei due
286300141030           IF  V04delda > *zeros and V04dela = *zeros;
286400141030             ErrMessage  = *on;
286500141030             ErrGenerico = *on;
286600141030             PosCurDELA  = *on;
286700150216             V04msg      = Msg(09);
286800141030             leavesr;
286900141030           ENDIF;
287000141030           IF  V04delda = *zeros and V04dela > *zeros;
287100141030             ErrMessage  = *on;
287200141030             ErrGenerico = *on;
287300141030             PosCurDELda = *on;
287400150216             V04msg      = Msg(09);
287500141030             leavesr;
287600141030           ENDIF;
287700141030         //?Controllo congruenza tra "DAL" e "AL"
287800141104           wdelda = V04delda;
287900141104           wdela  = V04dela;
288000141030           IF  V04deldas = '-';
288100141030             wdelda = wdelda * -1;
288200141030           ENDIF;
288300141030           IF  V04delas = '-';
288400141030             wdela = wdela * -1;
288500141030           ENDIF;
288600141030           IF  wdelda > wdela;
288700141030             ErrMessage  = *on;
288800141030             ErrGenerico = *on;
288900141030             PosCurDELda = *on;
289000150216             V04msg      = Msg(10);
289100141030             leavesr;
289200141030           ENDIF;
289300141030         ENDIF;
289400141121
289500141121       //?Obiettivi
289600141127         wfobj  = V04obj;
289700141127         wfobj1 = V04obj1;
289800141127         wfobj2 = V04obj2;
289900150119         wfformula = V04formula;
290000150213         wfobj3 = V04obj3;
290100150213         wobj3da = V04obj3da;
290200150213         wobj3a  = V04obj3a;
290300150219         IF  V04obj3das = '-';
290400150213           wobj3da = wobj3da * -1;
290500150213         ENDIF;
290600150219         IF  V04obj3as = '-';
290700150213           wobj3a = wobj3a * -1;
290800150213         ENDIF;
290900150128         exsr ContrObj;
291000141127         V04obj1 = wfobj1;
291100141127         V04obj2 = wfobj2;
291200150119         V04formula = wfformula;
291300150213         V04obj3 = wfobj3;
291400150213         V04obj3da = %abs(wobj3da);
291500150213         V04obj3a  = %abs(wobj3a);
291600141127         V04msg = wmsg;
291700141127         IF  ErrGenerico;
291800141127           leavesr;
291900141127         ENDIF;
292000150302
292100150302       //?Trattative
292200150302         w002a = V04ttr;
292300150302         clear w050a;
292400150302         exsr SceltaTtr;
292500150302         V04ttr = w002a;
292600150302         V04msg = wmsg;
292700150302         IF  ErrGenerico;
292800150302           leavesr;
292900150302         ENDIF;
293000141030
293100141030       ENDSR;
293200150119
293300150119       //--------------------------------------------------------------
293400150119       //?Gestione tasto funzionale F01 da videata D04
293500150119       //?F01=Formula
293600150119       //--------------------------------------------------------------
293700150119       BEGSR F01D04;
293800150119
293900150119         savformula = V04formula;
294000150119         Video = 'S6';
294100150119         wInzS06 = *on;
294200150119
294300150119       ENDSR;
294400150128
294500150128       //--------------------------------------------------------------
294600150128       //?Gestione tasto funzionale F02 da videata D04
294700150128       //?F02=Obiettivo/Andamento
294800150128       //--------------------------------------------------------------
294900150128       BEGSR F02D04;
295000150216
295100150216         IF  H2nmfl <> 'V04OBJ1' and H2nmfl <> 'V04OBJ2' and
295200150216             H2nmfl <> 'V04OBJ3';
295300150216           ErrMessage  = *on;
295400150216           ErrGenerico = *on;
295500150216           PosCurOBJ1  = *on;
295600150216           V04msg = Msg(22);
295700150216           leavesr;
295800150216         ENDIF;
295900150128
296000150128         Video = 'S8';
296100150128         wInzS08 = *on;
296200150128
296300150128       ENDSR;
296400150302
296500150302       //--------------------------------------------------------------
296600150302       //?Gestione tasto funzionale F04 da videata D04
296700150302       //?F04=Scelta Trattative
296800150302       //--------------------------------------------------------------
296900150302       BEGSR F04D04;
297000150302
297100150302         Video = 'S9';
297200150302         wInzS09 = *on;
297300150302
297400150302       ENDSR;
297500141103
297600141103       //--------------------------------------------------------------
297700141103       //?Gestione tasto funzionale F06 da videata D04
297800141103       //?F06=Conferma
297900141103       //--------------------------------------------------------------
298000141103       BEGSR F06D04;
298100141103
298200141103       //?Devo ricaricare il subfile con le nuove parzializzazioni
298300141103         Video = 'S3';
298400141112         wInzS03 = *on;
298500141103
298600141103       ENDSR;
298700141030
298800141030       //--------------------------------------------------------------
298900141030       //?Gestione tasto funzionale F12 da videata D04
299000141030       //?F12=Ritorno
299100141030       //--------------------------------------------------------------
299200141030       BEGSR F12D04;
299300150116
299400150116       //?Se arrivo qua dalla videata con messaggio di troppi rcd
299500150116       //?devo ricaricare il subfile
299600150116         IF  TroppiRcd;
299700150116           wInzs03 = *on;
299800150116         ENDIF;
299900141030
300000141030       //?Ritorno alle selezioni
300100141030         Video = 'S3';
300200141104       //?Forzo il posizionamento del cursore al 1° rcd del subfile
300300141104         clear V03rcd;
300400141030
300500141030       ENDSR;
300600141118
300700141118       //--------------------------------------------------------------
300800141118       //?Gestione videata D05.
300900141118       //--------------------------------------------------------------
301000141118       BEGSR GesD05;
301100150116
301200150116         TroppiRcd = *on;
301300141118
301400141118       //?Inizializzazione videata
301500141118         IF  wInzD05;
301600141118           exsr InzD05;
301700141118           wInzD05 = *off;
301800141118         ENDIF;
301900141118
302000141118       //?Emissione videata
302100150112         write  KC01T01;
302200150112         exfmt  KC01D05;
302300141118         reset ErrMessage;
302400141118         reset ErrGenerico;
302500141118
302600141118         SELECT;
302700141118
302800141118       //?- F03=Fine
302900141118           WHEN  dsp_aid = c_F03;
303000141118             exsr F03D02;
303100141118
303200141118       //?- F05=Altre parzializzazioni
303300141118           WHEN  dsp_aid = c_F05;
303400141118             exsr F05S03;
303500141118
303600141118       //?- F12=Ritorno
303700141118           WHEN  dsp_aid = c_F12;
303800141118             exsr F12S03;
303900141118
304000141118         ENDSL;
304100141118
304200141118       ENDSR;
304300141118
304400141118       //--------------------------------------------------------------
304500141118       //?Inizializzazione Videata D05.
304600141118       //--------------------------------------------------------------
304700141118       BEGSR InzD05;
304800141118
304900141118       ENDSR;
305000150116
305100150116       //--------------------------------------------------------------
305200150116       //?Gestione videata S06.
305300150116       //--------------------------------------------------------------
305400150116       BEGSR GesS06;
305500150116
305600150116       //?Inizializzazione videata
305700150116         IF  wInzS06;
305800150116           exsr InzS06;
305900150116           wInzS06 = *off;
306000150116         ENDIF;
306100150119
306200150119       //?Visualizzazione del SFL
306300150119         SflDsp = (S06nrr <= *zeros);
306400150116
306500150116       //?Emissione videata
306600150119         exfmt  KC01CW6;
306700150116         reset ErrMessage;
306800150116         reset ErrGenerico;
306900150116
307000150116       //?- Enter Controllo ed esco
307100150116         exsr OpzS06;
307200150116         IF  ErrGenerico;
307300150116           leavesr;
307400150116         ENDIF;
307500150116
307600150116      //?Videata sucessiva
307700150216         IF H2nmrc = 'KC01D02';
307800150216           Video = 'D2';
307900150216           V02formula = savformula;
308000150216         ENDIF;
308100150216         IF H2nmrc = 'KC01W04';
308200150216           Video = 'D4';
308300150216           V04formula = savformula;
308400150216         ENDIF;
308500150116
308600150116       ENDSR;
308700150116
308800150116       //--------------------------------------------------------------
308900150116       //?Inizializzazione Videata S06.
309000150116       //--------------------------------------------------------------
309100150116       BEGSR InzS06;
309200150116
309300150116       //?Pulizia subfile
309400150116         exsr PulS06;
309500150116
309600150116       //?Caricamento subfile
309700150116         exsr Ries06;
309800150116
309900150116         SflEnd = *on;
310000150116
310100150116       //?Imposto il posizionamento al primo rcd
310200150116         IF  S06nrr > 0;
310300150119           V06rcd = 1;
310400150116         ELSE;
310500150119           clear V06rcd;
310600150116         ENDIF;
310700150116
310800150119         V06csr = V06rcd;
310900150116
311000150116       ENDSR;
311100150116
311200150116       //--------------------------------------------------------------
311300150116       //?Pulizia Subfile S06.
311400150116       //--------------------------------------------------------------
311500150116       BEGSR PulS06;
311600150116
311700150116       //?Pulizia subfile
311800150116         SflDsp    = *on;
311900150116         SflDspCtl = *on;
312000150116         write KC01CW6;
312100150116         SflDspCtl = *off;
312200150116         SflEnd    = *off;
312300150116
312400150119         clear V06rcd;
312500150119         clear V06csr;
312600150116         clear S06nrr;
312700150119         clear V06msg;
312800150116
312900150116         ErrMessage  = *off;
313000150116         ErrGenerico = *off;
313100150116
313200150116         WindDspF = IndDspF;
313300150116         reset WindDspFb;
313400150116         IndDspF  = WindDspF;
313500150116
313600150116       ENDSR;
313700150116
313800150116       //--------------------------------------------------------------
313900150116       //?Caricamento Subfile S06.
314000150116       //--------------------------------------------------------------
314100150116       BEGSR RieS06;
314200150116
314300150116         xx = 1;
314400150116         FOR xx by 1 to 6;
314500150116           clear  KC01SW6;
314600150116           PosCurOPZ = *off;
314700150119           V06formula = Formula(xx);
314800150119           V06des = Formula(xx) + DesForm(xx);
314900150116           S06nrr += 1;
315000150116           write  KC01SW6;
315100150116         ENDFOR;
315200150116
315300150116       ENDSR;
315400150116
315500150116       //--------------------------------------------------------------
315600150116       //?Gestione opzioni Subfile S06.
315700150116       //--------------------------------------------------------------
315800150116       BEGSR OpzS06;
315900150116
316000150116         xx = 1;
316100150116         FOR xx by 1 to 6;
316200150119           S06nrr = xx;
316300150119           chain S06nrr KC01SW6;
316400150116           IF  not %found;
316500150116             leave;
316600150116           ENDIF;
316700150116
316800150116           SELECT;
316900150116         //?- Nessuna opzione
317000150119           WHEN  V06opz = *blank;
317100150116
317200150116         //?- 1 = Scelta
317300150119           WHEN  V06opz = '1';
317400150119             savformula = V06formula;
317500150121             PosCurOBJ2 = *on;
317600150116             leave;
317700150116
317800150116           OTHER;
317900150116             ErrMessage  = *on;
318000150116             ErrGenerico = *on;
318100150116             PosCurOPZ   = *on;
318200150216             V06msg = Msg(11);
318300150116             update KC01SW6;
318400150116             leave;
318500150116           ENDSL;
318600150116
318700150116         ENDFOR;
318800150116
318900150116       ENDSR;
319000150121
319100150121       //--------------------------------------------------------------
319200150121       //?Gestione videata D07.
319300150121       //--------------------------------------------------------------
319400150121       BEGSR GesD07;
319500150121
319600150121       //?Inizializzazione videata
319700150121         IF  wInzD07;
319800150121           exsr InzD07;
319900150121           wInzD07 = *off;
320000150121         ENDIF;
320100150121
320200150121       //?Emissione videata
320300150121         exfmt  KC01W07;
320400150121         reset ErrMessage;
320500150121         reset ErrGenerico;
320600150121
320700150121         SELECT;
320800150121
320900150121       //?- F06=Conferma
321000150121           WHEN  dsp_aid = c_F06;
321100150121             exsr F06D07;
321200150121
321300150121         ENDSL;
321400150121
321500150121       ENDSR;
321600150121
321700150121       //--------------------------------------------------------------
321800150121       //?Inizializzazione Videata D07.
321900150121       //--------------------------------------------------------------
322000150121       BEGSR InzD07;
322100150121
322200150121         clear KC01W07;
322300150121         V07sino = 'SI';
322400150121
322500150121       ENDSR;
322600150121
322700150121       //--------------------------------------------------------------
322800150121       //?Gestione tasto funzionale F06 da videata D07
322900150121       //?F06=Conferma.
323000150121       //--------------------------------------------------------------
323100150121       BEGSR F06D07;
323200150121
323300150121         Video = 'S3';
323400150121
323500150121         IF  V07sino = 'NO';
323600150121           leavesr;
323700150121         ENDIF;
323800150121
323900150121         FOR  xx = 1 to rrnlast;
324000150121           chain xx KC01S03;
324100150121         //?Scrivo la fase
324200150121           clear TRKC71DS;
324300150212           IKC71ncm = %dec(V04ncm:7:0);
324400150121           IKC71ksu = VS3ksu;
324500150121           IKC71ksc = VS3ksc;
324600150121           IKC71cpo = VS3cpo;
324700150121           IKC71acm = FaseObjFine;
324800150121           IKC71pea = VS3oprop;
324900150122           IKC71aut = 'A';
325000150128           IKC71dfa = %dec(%date());
325100150128           IKC71hfc = %dec(%time());
325200150121           TRKC71R (kpjba:TRKC71DS);
325300150121         ENDFOR;
325400150121
325500150121       //?Ricarico il subfile
325600150121         wInzS03 = *on;
325700150121
325800150121       ENDSR;
325900150128
326000150128       //--------------------------------------------------------------
326100150128       //?Gestione videata S08.
326200150128       //--------------------------------------------------------------
326300150128       BEGSR GesS08;
326400150128
326500150128       //?Inizializzazione videata
326600150128         IF  wInzS08;
326700150128           exsr InzS08;
326800150128           wInzS08 = *off;
326900150128         ENDIF;
327000150128
327100150128       //?Visualizzazione del SFL
327200150128         SflDsp = (S08nrr <= *zeros);
327300150128
327400150128       //?Emissione videata
327500150128         exfmt  KC01CW8;
327600150128         reset ErrMessage;
327700150128         reset ErrGenerico;
327800150128
327900150128       //?- Enter Controllo ed esco
328000150128         exsr OpzS08;
328100150128         IF  ErrGenerico;
328200150128           leavesr;
328300150128         ENDIF;
328400150128
328500150128      //?Videata sucessiva
328600150216         IF H2nmrc = 'KC01D02';
328700150216           Video = 'D2';
328800150216         ENDIF;
328900150216         IF H2nmrc = 'KC01W04';
329000150216           Video = 'D4';
329100150216         ENDIF;
329200150128
329300150128       ENDSR;
329400150128
329500150128       //--------------------------------------------------------------
329600150128       //?Inizializzazione Videata S08.
329700150128       //--------------------------------------------------------------
329800150128       BEGSR InzS08;
329900150128
330000150128       //?Pulizia subfile
330100150128         exsr PulS08;
330200150128
330300150128       //?Caricamento subfile
330400150128         exsr Ries08;
330500150128
330600150128         SflEnd = *on;
330700150128
330800150128       //?Imposto il posizionamento al primo rcd
330900150128         IF  S08nrr > 0;
331000150128           V08rcd = 1;
331100150128         ELSE;
331200150128           clear V08rcd;
331300150128         ENDIF;
331400150128
331500150128         V08csr = V08rcd;
331600150128
331700150128       ENDSR;
331800150128
331900150128       //--------------------------------------------------------------
332000150128       //?Pulizia Subfile S08.
332100150128       //--------------------------------------------------------------
332200150128       BEGSR PulS08;
332300150128
332400150128       //?Pulizia subfile
332500150128         SflDsp    = *on;
332600150128         SflDspCtl = *on;
332700150128         write KC01CW8;
332800150128         SflDspCtl = *off;
332900150128         SflEnd    = *off;
333000150128
333100150128         clear V08rcd;
333200150128         clear V08csr;
333300150128         clear S08nrr;
333400150128         clear V08msg;
333500150128
333600150128         ErrMessage  = *off;
333700150128         ErrGenerico = *off;
333800150128
333900150128         WindDspF = IndDspF;
334000150128         reset WindDspFb;
334100150128         IndDspF  = WindDspF;
334200150128
334300150128       ENDSR;
334400150128
334500150128       //--------------------------------------------------------------
334600150128       //?Caricamento Subfile S08.
334700150128       //--------------------------------------------------------------
334800150128       BEGSR RieS08;
334900150128
335000150128         xx = 1;
335100150128         FOR xx by 1 to 5;
335200150128           clear  KC01SW8;
335300150128           PosCurOPZ = *off;
335400150128           V08obj = Obiettivo(xx);
335500150128           V08des = Obiettivo(xx) + ' ' + DesObj(xx);
335600150128           S08nrr += 1;
335700150128           write  KC01SW8;
335800150128         ENDFOR;
335900150128
336000150128       ENDSR;
336100150128
336200150128       //--------------------------------------------------------------
336300150128       //?Gestione opzioni Subfile S08.
336400150128       //--------------------------------------------------------------
336500150128       BEGSR OpzS08;
336600150128
336700150128         xx = 1;
336800150128         FOR xx by 1 to 5;
336900150128           S08nrr = xx;
337000150128           chain S08nrr KC01SW8;
337100150128           IF  not %found;
337200150128             leave;
337300150128           ENDIF;
337400150128
337500150128           SELECT;
337600150128         //?- Nessuna opzione
337700150128           WHEN  V08opz = *blank;
337800150128
337900150128         //?- 1 = Scelta
338000150128           WHEN  V08opz = '1';
338100150216             exsr  Scelta08;
338200150216             leave;
338300150128
338400150128           OTHER;
338500150128             ErrMessage  = *on;
338600150128             ErrGenerico = *on;
338700150128             PosCurOPZ   = *on;
338800150216             V08msg = Msg(11);
338900150128             update KC01SW8;
339000150128             leave;
339100150128           ENDSL;
339200150128
339300150128         ENDFOR;
339400150128
339500150128       ENDSR;
339600150216
339700150216       //--------------------------------------------------------------
339800150216       //?Ritorno la scelta fatta da videata D08.
339900150216       //--------------------------------------------------------------
340000150216       BEGSR Scelta08;
340100150216
340200150216         SELECT;
340300150216         WHEN  H2nmfl = 'V02OBJ1';
340400150216           V02obj1 = V08obj;
340500150216           POScurOBJ1 = *on;
340600150216         WHEN  H2nmfl = 'V02OBJ2';
340700150216           V02obj2 = V08obj;
340800150216           POScurOBJ2 = *on;
340900150216         WHEN  H2nmfl = 'V02OBJ3';
341000150216           V02obj3 = V08obj;
341100150216           POScurOBJ3 = *on;
341200150216         WHEN  H2nmfl = 'V04OBJ1';
341300150216           V04obj1 = V08obj;
341400150216           POScurOBJ1 = *on;
341500150216         WHEN  H2nmfl = 'V04OBJ2';
341600150216           V04obj2 = V08obj;
341700150216           POScurOBJ2 = *on;
341800150216         WHEN  H2nmfl = 'V04OBJ3';
341900150216           V04obj3 = V08obj;
342000150216           POScurOBJ3 = *on;
342100150216         ENDSL;
342200150216
342300150216       ENDSR;
342400150302
342500150302       //--------------------------------------------------------------
342600150302       //?Gestione videata S09.
342700150302       //--------------------------------------------------------------
342800150302       BEGSR GesS09;
342900150302
343000150302       //?Inizializzazione videata
343100150302         IF  wInzS09;
343200150302           exsr InzS09;
343300150302           wInzS09 = *off;
343400150302         ENDIF;
343500150302
343600150302       //?Visualizzazione del SFL
343700150302         SflDsp = (S09nrr <= *zeros);
343800150302
343900150302       //?Emissione videata
344000150302         exfmt  KC01CW9;
344100150302         reset ErrMessage;
344200150302         reset ErrGenerico;
344300150302
344400150302       //?- Enter Controllo ed esco
344500150302         exsr OpzS09;
344600150302         IF  ErrGenerico;
344700150302           leavesr;
344800150302         ENDIF;
344900150302
345000150302      //?Videata sucessiva
345100150302         IF H2nmrc = 'KC01D02';
345200150302           Video = 'D2';
345300150302         ENDIF;
345400150302         IF H2nmrc = 'KC01W04';
345500150302           Video = 'D4';
345600150302         ENDIF;
345700150302
345800150302       ENDSR;
345900150302
346000150302       //--------------------------------------------------------------
346100150302       //?Inizializzazione Videata S09.
346200150302       //--------------------------------------------------------------
346300150302       BEGSR InzS09;
346400150302
346500150302       //?Pulizia subfile
346600150302         exsr PulS09;
346700150302
346800150302       //?Caricamento subfile
346900150302         exsr Ries09;
347000150302
347100150302         SflEnd = *on;
347200150302
347300150302       //?Imposto il posizionamento al primo rcd
347400150302         IF  S09nrr > 0;
347500150302           V09rcd = 1;
347600150302         ELSE;
347700150302           clear V09rcd;
347800150302         ENDIF;
347900150302
348000150302         V09csr = V09rcd;
348100150302
348200150302       ENDSR;
348300150302
348400150302       //--------------------------------------------------------------
348500150302       //?Pulizia Subfile S09.
348600150302       //--------------------------------------------------------------
348700150302       BEGSR PulS09;
348800150302
348900150302       //?Pulizia subfile
349000150302         SflDsp    = *on;
349100150302         SflDspCtl = *on;
349200150302         write KC01CW9;
349300150302         SflDspCtl = *off;
349400150302         SflEnd    = *off;
349500150302
349600150302         clear V09rcd;
349700150302         clear V09csr;
349800150302         clear S09nrr;
349900150302         clear V09msg;
350000150302
350100150302         ErrMessage  = *off;
350200150302         ErrGenerico = *off;
350300150302
350400150302         WindDspF = IndDspF;
350500150302         reset WindDspFb;
350600150302         IndDspF  = WindDspF;
350700150302
350800150302       ENDSR;
350900150302
351000150302       //--------------------------------------------------------------
351100150302       //?Caricamento Subfile S09.
351200150302       //--------------------------------------------------------------
351300150302       BEGSR RieS09;
351400150302
351500150302         xx = 1;
351600150302         FOR xx by 1 to 10;
351700150302           clear  KC01SW9;
351800150302           PosCurOPZ = *off;
351900150302           V09ttr = Trattative(xx);
352000150302           V09des = Trattative(xx) + ' ' + DesTtr(xx);
352100150302           S09nrr += 1;
352200150302           write  KC01SW9;
352300150302         ENDFOR;
352400150302
352500150302       ENDSR;
352600150302
352700150302       //--------------------------------------------------------------
352800150302       //?Gestione opzioni Subfile S09.
352900150302       //--------------------------------------------------------------
353000150302       BEGSR OpzS09;
353100150302
353200150302         xx = 1;
353300150302         FOR xx by 1 to 10;
353400150302           S09nrr = xx;
353500150302           chain S09nrr KC01SW9;
353600150302           IF  not %found;
353700150302             leave;
353800150302           ENDIF;
353900150302
354000150302           SELECT;
354100150302         //?- Nessuna opzione
354200150302           WHEN  V09opz = *blank;
354300150302
354400150302         //?- 1 = Scelta
354500150302           WHEN  V09opz = '1';
354600150302             exsr  Scelta09;
354700150302             leave;
354800150302
354900150302           OTHER;
355000150302             ErrMessage  = *on;
355100150302             ErrGenerico = *on;
355200150302             PosCurOPZ   = *on;
355300150302             V09msg = Msg(11);
355400150302             update KC01SW9;
355500150302             leave;
355600150302           ENDSL;
355700150302
355800150302         ENDFOR;
355900150302
356000150302       ENDSR;
356100150302
356200150302       //--------------------------------------------------------------
356300150302       //?Ritorno la scelta fatta da videata D09.
356400150302       //--------------------------------------------------------------
356500150302       BEGSR Scelta09;
356600150302
356700150304         IF H2nmrc = 'KC01D02';
356800150304           V02ttr = V09ttr;
356900150304         ENDIF;
357000150304         IF H2nmrc = 'KC01W04';
357100150304           V04ttr = V09ttr;
357200150304         ENDIF;
357300150302         POScurTTR = *on;
357400150302
357500150302       ENDSR;
357600141030
357700141030       //--------------------------------------------------------------
357800141030       //?Controllo Distretto.
357900141030       //--------------------------------------------------------------
358000141030       BEGSR Distretto;
358100141030
358200141030         IF  w001a = *blanks;
358300141030           leavesr;
358400141030         ENDIF;
358500141030
358600141030       //?Ricerca Distretto
358700141030         IF  %scan('?' : w001a) > 0;
358800141030           ErrGenerico = *on;
358900141030           PosCurCDI   = *on;
359000141030           idTabella = '17';
359100141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
359200141030                            tastoUscita);
359300141030           w001a = idElemento;
359400141030         ENDIF;
359500141030       //?Deve essere un Distretto valido
359600141030         clear xx;
359700141030         xx = %lookup(w001a : skDistretti);
359800141030         IF  xx = 0;
359900141030           ErrMessage  = *on;
360000141030           ErrGenerico = *on;
360100141030           PosCurCDI   = *on;
360200141030           wmsg = Msg(03);
360300141030           leavesr;
360400141030         ENDIF;
360500141030       //?Decodifico distretto
360600141030         w050a = skDesDist(xx);
360700141030       //?In carico all'Utente
360800141030         IF  %lookup (w001a:DIS) = 0;
360900141030           ErrMessage  = *on;
361000141030           ErrGenerico = *on;
361100141030           PosCurCDI   = *on;
361200141030           wmsg = %trim(Msg(03)) + '. Non in gestione';
361300141030           leavesr;
361400141030         ENDIF;
361500141030
361600141030       ENDSR;
361700141030
361800141030       //--------------------------------------------------------------
361900141030       //?Controllo Area.
362000141030       //--------------------------------------------------------------
362100141030       BEGSR Area;
362200141030
362300141030         IF  w003a = *blanks;
362400141030           leavesr;
362500141030         ENDIF;
362600141030
362700141030       //?Ricerca Area
362800141030         IF  %scan('?' : w003a) > 0;
362900141030           ErrGenerico = *on;
363000141030           PosCurCAR   = *on;
363100141104           idTabella = '05';
363200141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
363300141030                          tastoUscita);
363400141030           w003a = idElemento;
363500141030         ENDIF;
363600141104         IF  w003a = *blanks;
363700141104           leavesr;
363800141104         ENDIF;
363900141030       //?Accetto solo dati numerici
364000141030         xx = 1;
364100141030         FOR xx by 1 to %len(w003a);
364200141030           IF  %subst(w003a:xx:1) <> *blanks and
364300141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
364400141030             ErrMessage  = *on;
364500141030             ErrGenerico = *on;
364600141030             PosCurCAR   = *on;
364700141030             wmsg = Msg(04);
364800141030             leavesr;
364900141030           ENDIF;
365000141030         ENDFOR;
365100141030       //?Deve essere un'Area valida
365200141030         clear xx;
365300141030         w0030 = %dec(w003a:3:0);
365400141030         xx = %lookup(%editc(w0030:'X'):skAree);
365500141030         IF  xx = 0;
365600141030           ErrMessage  = *on;
365700141030           ErrGenerico = *on;
365800141030           PosCurCAR   = *on;
365900141030           wmsg = Msg(04);
366000141030           leavesr;
366100141030         ENDIF;
366200141030       //?Decodifico area
366300141030         w050a = skDesArea(xx);
366400141030       //?Deve essere un'area in carico all'utente
366500141030         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
366600141030           ErrGenerico = *on;
366700141030           ErrMessage  = *on;
366800141030           PosCurCAR   = *on;
366900141030           wmsg = %trim(Msg(04)) + '. Non in gestione';
367000141030           leavesr;
367100141030         ENDIF;
367200141030
367300141030       ENDSR;
367400141030
367500141030       //--------------------------------------------------------------
367600141030       //?Controllo Filiale.
367700141030       //--------------------------------------------------------------
367800141030       BEGSR Filiale;
367900141030
368000141030         IF  w003a = *blanks;
368100141030           leavesr;
368200141030         ENDIF;
368300141030
368400141030       //?Ricerca Filale
368500141030         IF  %scan('?' : w003a) > 0;
368600141030           clear pinFIL;
368700141030           clear pinFAG;
368800141030           clear pinDES;
368900141030           clear pinDIT;
369000141030           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
369100141030           IF pinFIL > *zeros;
369200141030             w003a = pinFIL;
369300141030             w050a = pinDES;
369400141030           ENDIF;
369500141030           ErrGenerico = *on;
369600141030           PosCurFIL   = *on;
369700141030         ENDIF;
369800141030       //?Accetto solo dati numerici
369900141030         xx = 1;
370000141030         FOR xx by 1 to %len(w003a);
370100141030           IF  %subst(w003a:xx:1) <> *blanks and
370200141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
370300141030             ErrMessage  = *on;
370400141030             ErrGenerico = *on;
370500141030             PosCurFIL   = *on;
370600141030             wmsg = Msg(05);
370700141030             leavesr;
370800141030           ENDIF;
370900141030         ENDFOR;
371000141030         w0030 = %dec(w003a:3:0);
371100141030       //?Deve esistere la Filiale
371200141030         chain w0030 AZORG01L;
371300141030         IF  not %found(AZORG01L) or ORGfva <> *blanks;
371400141030           ErrMessage  = *on;
371500141030           ErrGenerico = *on;
371600141030           PosCurFIL   = *on;
371700141030           wmsg = Msg(05);
371800141030           leavesr;
371900141030         ENDIF;
372000141030       //?Decodifico filiale
372100141030         w050a = ORGdes;
372200141030       //?Deve essere una filiale in carico all'utente
372300141030         IF  %lookup(%editc(w0030:'X'):POG) = 0;
372400141030           ErrGenerico = *on;
372500141030           ErrMessage  = *on;
372600141030           PosCurFIL   = *on;
372700141030           wmsg = %trim(Msg(05)) + '. Non in gestione';
372800141030           leavesr;
372900141030         ENDIF;
373000141104       //?Se presente anche il distretto o l'area devono essere congruenti
373100141104         IF  V02cdi <> *blanks and V02fil <> *blanks;
373200141105           IF  V02cdi <> ORGfl3;
373300141104             ErrGenerico = *on;
373400141104             ErrMessage  = *on;
373500141104             PosCurFIL   = *on;
373600141104             wmsg = %trim(Msg(05)) + '. Non congruente con il distretto';
373700141104             leavesr;
373800141104           ENDIF;
373900141104           IF  V02car <> %editc(ORGcar:'X');
374000141104             ErrGenerico = *on;
374100141104             ErrMessage  = *on;
374200141104             PosCurFIL   = *on;
374300141104             wmsg = %trim(Msg(05)) + '. Non congruente con l''area';
374400141104             leavesr;
374500141104           ENDIF;
374600141104         ENDIF;
374700141030
374800141030       ENDSR;
374900141030
375000141030       //--------------------------------------------------------------
375100141030       //?Controllo Commerciale.
375200141030       //--------------------------------------------------------------
375300141030       BEGSR Commerciale;
375400141030
375500141030         IF  w007a = *blanks;
375600141030           leavesr;
375700141030         ENDIF;
375800141030
375900141030       //?Ricerca Commerciale
376000141030         IF  %scan('?' : w007a) > 0;
376100141030           clear TRMK43DS;
376200141030           IMK43solu = 'S';
376300141030           IMK43fil = DUTpou;
376400141030           trmk43r (kpjba : TRMK43DS);
376500141030           IF  OMK43err = *off and OMK43fxx = *blanks;
376600141030             w007a = %editc(OMK43cmm:'X');
376700141030             w050a = OMK43des;
376800141030           ENDIF;
376900141030           ErrGenerico = *on;
377000141030           PosCurCMM   = *on;
377100141030         ENDIF;
377200141030       //?Accetto solo dati numerici
377300141030         xx = 1;
377400141030         FOR xx by 1 to %len(w007a);
377500141030           IF  %subst(w007a:xx:1) <> *blanks and
377600141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
377700141030             ErrMessage  = *on;
377800141030             ErrGenerico = *on;
377900141030             PosCurCMM   = *on;
378000150216             wmsg = Msg(06);
378100141030             leavesr;
378200141030           ENDIF;
378300141030         ENDFOR;
378400141030         w0070 = %dec(w007a:7:0);
378500141030       //?Deve esistere il Commerciale
378600141030         chain (w0070) AZCMM01L;
378700141030         IF  not %found(AZCMM01L);
378800141030           ErrMessage  = *on;
378900141030           ErrGenerico = *on;
379000141030           PosCurCMM   = *on;
379100150216           wmsg = Msg(06);
379200141030           leavesr;
379300141030         ENDIF;
379400141030       //?Decodifico Commerciale
379500141030         w050a = CMMdes;
379600141030       //?Deve essere un Commerciale in carico all'utente
379700141030         clear TNTAA1DS;
379800141119         ITAA1caut = '§utegtc';
379900141030         ITAA1cmm  = w0070;
380000141030         kpjbu = TNTAA1DS;
380100141030         tntaa1c (kpjba);
380200150217         TNTAA1DS = kpjbu;
380300141030         IF  OTAA1fabi = 'N';
380400141030           ErrGenerico = *on;
380500141030           ErrMessage  = *on;
380600141030           PosCurCMM   = *on;
380700150216           wmsg = %trim(Msg(06)) + '. Non in gestione';
380800141030           leavesr;
380900141030         ENDIF;
381000141030       //?Deve essere un Commerciale Unificante
381100141030         IF  w0070 <> CMMuni;
381200141030           ErrGenerico = *on;
381300141030           ErrMessage  = *on;
381400141030           PosCurCMM   = *on;
381500150216           wmsg = %trim(Msg(06)) + '. Non è un Commerciale Unificante';
381600141030           leavesr;
381700141030         ENDIF;
381800141030
381900141030       ENDSR;
382000141030
382100141030       //--------------------------------------------------------------
382200141030       //?Controllo Cliente.
382300141030       //--------------------------------------------------------------
382400141030       BEGSR Cliente;
382500141030
382600141030         IF  w007a = *blanks;
382700141030           leavesr;
382800141030         ENDIF;
382900141030
383000141030       //?Ricerca Cliente
383100141030         IF  %scan('?' : w007a) > 0;
383200141030           clear TNTAI1DS;
383300141030           ITAI1ric = 'S';
383400141030           ITAI1abi = wabi;
383500141112           ITAI1aut = '*C';
383600141030           tntai1r (kpjba : TNTAI1DS);
383700141030           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
383800141030               OTAI1err <> *blanks;
383900141107             clear V02ksu;
384000141030           ELSE;
384100141030             w007a = %editc(OTAI1ksc:'X');
384200141030           ENDIF;
384300141030           ErrGenerico = *on;
384400141107           PosCurKSU   = *on;
384500141030         ENDIF;
384600141030       //?Accetto solo dati numerici
384700141030         xx = 1;
384800141030         FOR xx by 1 to %len(w007a);
384900141030           IF  %subst(w007a:xx:1) <> *blanks and
385000141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
385100141030             ErrMessage  = *on;
385200141030             ErrGenerico = *on;
385300141107             PosCurKSU   = *on;
385400150216             wmsg = Msg(07);
385500141030             leavesr;
385600141030           ENDIF;
385700141030         ENDFOR;
385800141030         w0070 = %dec(w007a:7:0);
385900141030       //?Deve esistere il Cliente
386000141030         clear TIBS69DS;
386100141030         clear ACOrag;
386200141030         I69kac = %dec(w007a:7:0);
386300141030         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
386400141030         IF  O69err <> *blanks;
386500141030           ErrMessage  = *on;
386600141030           ErrGenerico = *on;
386700141107           PosCurKSU   = *on;
386800150216           wmsg      = Msg(07);
386900141030         ENDIF;
387000141030       //?Decodifico Cliente
387100141030         w050a = ACOrag;
387200141030       //?Deve essere un Cliente in carico all'utente
387300141030         clear TNTAA1DS;
387400141119         ITAA1caut = '§utegtc';
387500141030         ITAA1ksc  = %dec(w007a:7:0);
387600141030         kpjbu = TNTAA1DS;
387700141030         tntaa1c (kpjba);
387800150217         TNTAA1DS = kpjbu;
387900141030         IF  OTAA1fabi = 'N';
388000141030           ErrGenerico = *on;
388100141030           ErrMessage  = *on;
388200141107           PosCurKSU   = *on;
388300150216           wmsg = %trim(Msg(07)) + '. Non in gestione';
388400141030           leavesr;
388500141030         ENDIF;
388600141030
388700141030       ENDSR;
388800141030
388900141030       //--------------------------------------------------------------
389000141030       //?Controllo Importanza Cliente.
389100141030       //--------------------------------------------------------------
389200141030       BEGSR ImpCliente;
389300141030
389400141030         IF  w001a = *blanks;
389500141030           leavesr;
389600141030         ENDIF;
389700141030
389800141030         IF  %scan('?' : w001a) > 0;
389900141030           ErrGenerico = *on;
390000141030           idTabella = 'IC';
390100141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
390200141030                          tastoUscita);
390300141030           w001a = idElemento;
390400141030         ENDIF;
390500141030         IF  %lookup(w001a : skIC) = 0;
390600141030           ErrMessage  = *on;
390700141030           ErrGenerico = *on;
390800141030           PosCurCLV1  = *on;
390900150216           wmsg = Msg(08);
391000141030           leavesr;
391100141030         ENDIF;
391200141030
391300141030       ENDSR;
391400141127
391500141127       //--------------------------------------------------------------
391600141127       //?Controllo Obiettivi.
391700141127       //--------------------------------------------------------------
391800150128       BEGSR ContrObj;
391900141127
392000150128       //?Se Obiettivo inizio NO "di cui"
392100150119         IF  wfobj = 'I' and
392200150119            (wfobj1 = 'P' or wfobj1 = 'F' or
392300150119             wfobj2 = 'P' or wfobj2 = 'F');
392400141127           ErrMessage  = *on;
392500141127           ErrGenerico = *on;
392600150216           wmsg = Msg(14);
392700141127           IF  wfobj1 <> *blanks;
392800141127             PosCurOBJ1  = *on;
392900141127             leavesr;
393000141127           ENDIF;
393100141127           IF  wfobj2 <> *blanks;
393200141127             PosCurOBJ2  = *on;
393300141127             leavesr;
393400141127           ENDIF;
393500141127         ENDIF;
393600141127       //?Se Obiettivo proposto NO "di cui" Finale
393700141127         IF  wfobj = 'P' and (wfobj1 = 'F' or wfobj2 = 'F');
393800141127           ErrMessage  = *on;
393900141127           ErrGenerico = *on;
394000150216           wmsg = Msg(15);
394100141127           IF  wfobj1 = 'F';
394200141127             PosCurOBJ1  = *on;
394300141127             leavesr;
394400141127           ENDIF;
394500141127           IF  wfobj2 = 'F';
394600141127             PosCurOBJ2  = *on;
394700141127             leavesr;
394800141127           ENDIF;
394900141127         ENDIF;
395000150128       //?Deve essere un obiettivo valido
395100150128         IF  wfobj1 <> *blanks and %lookup (wfobj1:Obiettivo) = 0;
395200150128           ErrMessage  = *on;
395300150128           ErrGenerico = *on;
395400150128           PosCurOBJ1  = *on;
395500150216           wmsg = Msg(13);
395600150128           leavesr;
395700150128         ENDIF;
395800150128         IF  wfobj2 <> *blanks and %lookup (wfobj2:Obiettivo) = 0;
395900150128           ErrMessage  = *on;
396000150128           ErrGenerico = *on;
396100150128           PosCurOBJ2  = *on;
396200150216           wmsg = Msg(13);
396300150128           leavesr;
396400150128         ENDIF;
396500150128
396600141127       //?Non posso accettare 2 obiettivi uguali
396700141127         IF  wfobj1 = wfobj2 and (wfobj1 <> *blanks or
396800141127                                  wfobj2 <> *blanks);
396900141127           ErrMessage  = *on;
397000141127           ErrGenerico = *on;
397100141127           PosCurOBJ1  = *on;
397200150216           wmsg = Msg(16);
397300141127           leavesr;
397400141127         ENDIF;
397500141127       //?Devono essere tutti e 2 impostati
397600141127         IF  (wfobj1 = *blanks and wfobj2 <> *blanks) or
397700141127             (wfobj1 <> *blanks and wfobj2 = *blanks);
397800141127           ErrMessage  = *on;
397900141127           ErrGenerico = *on;
398000150216           wmsg = Msg(17);
398100141127           IF  wfobj1 = *blanks;
398200141127             PosCurOBJ1  = *on;
398300141127             leavesr;
398400141127           ENDIF;
398500141127           IF  wfobj2 = *blanks;
398600141127             PosCurOBJ2  = *on;
398700141127             leavesr;
398800141127           ENDIF;
398900141127         ENDIF;
399000150127
399100150127       //?Se il primo byte del campo formula è vuoto controllo se è
399200150127       //?uno dei campi validi e sistemo il campo
399300150127         IF  wfformula <> *blanks and %subst(wfformula:1:1) = *blanks
399400150127             and %check(ValoriFormula:%subst(wfformula:2:1)) = *zeros;
399500150127           %subst(wfformula:1:1) = %subst(wfformula:2:1);
399600150127           %subst(wfformula:2:1) = *blanks;
399700150127         ENDIF;
399800141127
399900150119       //?La formula deve essere una formula valida
400000150119         IF  %lookup (wfformula:Formula) = 0 and wfobj1 <> *blanks;
400100141127           ErrMessage  = *on;
400200141127           ErrGenerico = *on;
400300150119           PosCurFORM  = *on;
400400150216           wmsg = Msg(18);
400500141127           leavesr;
400600141127         ENDIF;
400700141128
400800150121       //?Se ci sono gli obiettivi devo avere anche la formula
400900141128         IF  wfobj1 <> *blanks and wfobj2 <> *blanks and
401000150119             wfformula = *blanks;
401100141128           ErrMessage  = *on;
401200141128           ErrGenerico = *on;
401300150119           PosCurFORM  = *on;
401400150216           wmsg = Msg(18);
401500141128           leavesr;
401600141128         ENDIF;
401700150121
401800150121       //?Se ho la formula devo avere anche gli obiettivi
401900150121         IF  wfobj1 = *blanks and wfobj2 = *blanks and
402000150121             wfformula <> *blanks;
402100150121           ErrMessage  = *on;
402200150121           ErrGenerico = *on;
402300150122           PosCurOBJ1  = *on;
402400150216           wmsg = Msg(19);
402500150121           leavesr;
402600150121         ENDIF;
402700150213
402800150213       //?Deve essere un obiettivo valido
402900150213         IF  wfobj3 <> *blanks and %lookup (wfobj3:Obiettivo) = 0;
403000150213           ErrMessage  = *on;
403100150213           ErrGenerico = *on;
403200150213           PosCurOBJ3  = *on;
403300150216           wmsg = Msg(13);
403400150213           leavesr;
403500150213         ENDIF;
403600150213       //?Almeno 1 dei 2 valori deve essere presente
403700150213         IF  wfobj3 <> *blanks and wobj3da = 0 and wobj3a = 0;
403800150216           //ErrMessage  = *on;
403900150216           //ErrGenerico = *on;
404000150216           //PosCurOBJ3DA = *on;
404100150216           //wmsg = Msg(20);
404200150216           //leavesr;
404300150216         ENDIF;
404400150213       //?Se impostate % serve il flag obiettivo
404500150213         IF  wfobj3 = *blanks and (wobj3da <> 0 or wobj3a <> 0);
404600150213           ErrMessage  = *on;
404700150213           ErrGenerico = *on;
404800150213           PosCurOBJ3  = *on;
404900150216           wmsg = Msg(13);
405000150213           leavesr;
405100150213         ENDIF;
405200150213         IF  wobj3da > wobj3a;
405300150213           ErrMessage  = *on;
405400150213           ErrGenerico = *on;
405500150213           PosCurOBJ3DA = *on;
405600150216           wmsg = Msg(21);
405700150213           leavesr;
405800150213         ENDIF;
405900141127
406000141127       ENDSR;
406100150302
406200150302       //--------------------------------------------------------------
406300150302       //?Controllo Scelta Trattative.
406400150302       //--------------------------------------------------------------
406500150302       BEGSR SceltaTtr;
406600150302
406700150302         IF  w002a = *blanks;
406800150302           leavesr;
406900150302         ENDIF;
407000150302
407100150302       //?Deve essere una Scelta valida
407200150302         clear xx;
407300150302         xx = %lookup(w002a : Trattative);
407400150302         IF  xx = 0;
407500150302           ErrMessage  = *on;
407600150302           ErrGenerico = *on;
407700150302           PosCurTTR   = *on;
407800150302           wmsg = Msg(24);
407900150302           leavesr;
408000150302         ENDIF;
408100150302
408200150302       ENDSR;
408300141121
408400141121       //--------------------------------------------------------------
408500150218       //?Richiamo pgm TRKC76R.
408600141121       //--------------------------------------------------------------
408700150218       BEGSR CallTRKC76;
408800141121
408900150218         wTRKC76 = *on;
409000141121
409100150218         clear TRKC76DS;
409200150303         IKC76ric = wtpric;
409300150218         IKC76ncm = CMIncm;
409400150218         IKC76ksu = CMIksu;
409500150218         IKC76ksc = CMIksc;
409600150218         IKC76cpo = CMIcpo;
409700150303         IKC76flgp = wflgp;
409800150220         IKC76acm = wFase;
409900150218         IKC76istat = CMIist;
410000150218
410100150218         trkc76r (kpjba:TRKC76DS);
410200150203
410300150203       ENDSR;
410400141028
410500141028       //--------------------------------------------------------------
410600141028       //?Operazioni finali.
410700141028       //--------------------------------------------------------------
410800141028       BEGSR RoutEnd;
410900150218
411000150218       //?Se richiamato chiudo i file per il pgm TRKC76R
411100150218         IF  wTRKC76;
411200150218           clear TRKC76DS;
411300150218           IKC76ric = 'C';
411400150218           trkc76r (kpjba:TRKC76DS);
411500150218         ENDIF;
411600141028
411700141028         *inLR = *on;
411800141028         return;
411900141028
412000141028       ENDSR;
412100141028
412200141028      /end-free
412300141028
412400141028       //--------------------------------------------------------------
412500141028       //?Schiere a tempo di compilazione.
412600141028       //--------------------------------------------------------------
412700150116
412800150116** -- Formule / DesForm -----------------------------------------------------*
412900150116=  : Uguale
413000150116>  : Maggiore
413100150116<  : Minore
413200150116>= : Maggiore Uguale
413300150116<= : Minore Uguale
413400150116<> : Diverso
413500150128** -- Obiettivo / DesObj ----------------------------------------------------*
413600150128I= Obiettivo Inizio
413700150128P= Obiettivo Proposto
413800150128F= Obiettivo Finale
413900150128T= Andamento Trattativa
414000150128C= Andamento Confronto Fatturazione
414100150302** -- Trattative / DesTtr----------------------------------------------------*
414200150302A = Avviate
414300150302AO= Avviate con Offerta
414400150302AN= Avviate senza Offerta
414500150302C = Chiuse
414600150302CN= Chiuse No Positive
414700150302CP= Chiuse Positive
414800150302SI= Tutte
414900150302T = Trattative non Forzate
415000150302F = Trattative Forzate
415100150302NO= Non avviate
415200141028** -- MSG -------------------------------------------------------------------*
415300141024Utente non abilitato alla Funzione.                                            1
415400141024Campagna Commerciale errata                                                    2
415500141027Distretto Errato                                                               3
415600141027Area Errata                                                                    4
415700141027Filiale Errata                                                                 5
415800150216Commerciale Unificante Errato                                                  6
415900150216Codice Cliente Errato                                                          7
416000150216Codice Importanza Clienti Errato                                               8
416100150216Immettere il Delta                                                             9
416200150216Delta previsto "DAL" incongruente con Delta previsto "AL"                     10
416300150216Opzione errata                                                                11
416400150216La modifica è possibile solo dal            al                                12
416500150216Impostare un Obiettivo/Andamento valido (F2)                                  13
416600150216Se richiesto Obiettivo Inizio non richiedere il "di cui"                      14
416700150216Non è possibile il "di cui" Finale se richiesto Obiettivo Proposto            15
416800150216Non è possibile il confronto tra 2 obiettivi uguali                           16
416900150216Impostare tutti e 2 gli obiettivi/andamento da confrontare                    17
417000150216Impostare una formula valida (F1=Legenda Formula)                             18
417100150216Impostare gli obiettivi/andamento                                             19
417200150216Immettere la % Obiettivo/Andamento                                            20
417300150216Obiettivo/Andamento "DAL" incongruente con Obiettivo/Andamento "AL"           21
417400150216Per poter fare F2 posizionarsi su obiettivo/andamento da inserire             22
417500150225Trattativa Forzata possibile solo dal            al                           23
417600150302Scelta trattativa non valida                                                  24
