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
080900150611         WHEN  wabi = 'DI' or car(2) > *zeros;
081000150611           VisArea = *on;
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
100900150611
101000150611       //?Se utente NON di sede a abilitato a più di un'area e no
101100150611       //?abilitazione 'AZ' o 'DI'
101200150611       //?richiedo obbligatoria l'area
101300150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
101400150611             V02car = *blanks and V02cmm = *blanks and
101500150611             V02ksu = *blanks and car(2) > *zeros;
101600150611           ErrMessage  = *on;
101700150611           ErrGenerico = *on;
101800150611           PosCurCAR   = *on;
101900150611           V02msg = Msg(25);
102000150611           leavesr;
102100150611         ENDIF;
102200141024
102300141024       //?Importanza Cliente
102400141030         w001a = V02clv1;
102500141030         clear w050a;
102600141030         exsr ImpCliente;
102700141030         V02clv1 = w001a;
102800141030         V02msg = wmsg;
102900141030         IF  ErrGenerico;
103000141030           PosCurCLV1 = *on;
103100141030           leavesr;
103200141030         ENDIF;
103300141030         w001a = V02clv2;
103400141030         clear w050a;
103500141030         exsr ImpCliente;
103600141030         V02clv2 = w001a;
103700141030         V02msg = wmsg;
103800141030         IF  ErrGenerico;
103900141030           PosCurCLV2 = *on;
104000141030           leavesr;
104100141030         ENDIF;
104200141030         w001a = V02clv3;
104300141030         clear w050a;
104400141030         exsr ImpCliente;
104500141030         V02clv3 = w001a;
104600141030         V02msg = wmsg;
104700141030         IF  ErrGenerico;
104800141030           PosCurCLV3 = *on;
104900141030           leavesr;
105000141030         ENDIF;
105100141030         w001a = V02clv4;
105200141030         clear w050a;
105300141030         exsr ImpCliente;
105400141030         V02clv4 = w001a;
105500141030         V02msg = wmsg;
105600141030         IF  ErrGenerico;
105700141030           PosCurCLV4 = *on;
105800141030           leavesr;
105900141030         ENDIF;
106000141030         w001a = V02clv5;
106100141030         clear w050a;
106200141030         exsr ImpCliente;
106300141030         V02clv5 = w001a;
106400141030         V02msg = wmsg;
106500141030         IF  ErrGenerico;
106600141030           PosCurCLV5 = *on;
106700141030           leavesr;
106800141030         ENDIF;
106900141028
107000141028       //?Delta
107100141029         IF  V02delda > *zeros or V02dela <> *zeros;
107200141028         //?Obbligatori tutti e 2 se immesso uno dei due
107300141028           IF  V02delda > *zeros and V02dela = *zeros;
107400141028             ErrMessage  = *on;
107500141028             ErrGenerico = *on;
107600141028             PosCurDELA  = *on;
107700150216             V02msg      = Msg(09);
107800141028             leavesr;
107900141028           ENDIF;
108000141028           IF  V02delda = *zeros and V02dela > *zeros;
108100141028             ErrMessage  = *on;
108200141028             ErrGenerico = *on;
108300141028             PosCurDELda = *on;
108400150216             V02msg      = Msg(09);
108500141028             leavesr;
108600141028           ENDIF;
108700141028         //?Controllo congruenza tra "DAL" e "AL"
108800141028           wdelda = V02delda;
108900141028           wdela  = V02dela;
109000141029           IF  V02deldas = '-';
109100141028             wdelda = wdelda * -1;
109200141028           ENDIF;
109300141029           IF  V02delas = '-';
109400141028             wdela = wdela * -1;
109500141028           ENDIF;
109600141028           IF  wdelda > wdela;
109700141028             ErrMessage  = *on;
109800141028             ErrGenerico = *on;
109900141028             PosCurDELda = *on;
110000150216             V02msg      = Msg(10);
110100141028             leavesr;
110200141028           ENDIF;
110300141028         ENDIF;
110400141121
110500141121       //?Obiettivi
110600150119         wfobj  = V02obj;
110700141127         wfobj1 = V02obj1;
110800141127         wfobj2 = V02obj2;
110900150119         wfformula = V02formula;
111000150213         wfobj3 = V02obj3;
111100150213         wobj3da = V02obj3da;
111200150213         wobj3a  = V02obj3a;
111300150213         IF  V02obj3das = '-';
111400150213           wobj3da = wobj3da * -1;
111500150213         ENDIF;
111600150213         IF  V02obj3as = '-';
111700150213           wobj3a = wobj3a * -1;
111800150213         ENDIF;
111900150128         exsr ContrObj;
112000141127         V02obj1 = wfobj1;
112100141127         V02obj2 = wfobj2;
112200150119         V02formula = wfformula;
112300150213         V02obj3 = wfobj3;
112400150213         V02obj3da = %abs(wobj3da);
112500150213         V02obj3a  = %abs(wobj3a);
112600141127         V02msg = wmsg;
112700141127         IF  ErrGenerico;
112800141127           leavesr;
112900141127         ENDIF;
113000150302
113100150302       //?Trattative
113200150302         w002a = V02ttr;
113300150302         clear w050a;
113400150302         exsr SceltaTtr;
113500150302         V02ttr = w002a;
113600150302         V02msg = wmsg;
113700150302         IF  ErrGenerico;
113800150302           leavesr;
113900150302         ENDIF;
114000141024
114100141024       ENDSR;
114200141028
114300141028       //--------------------------------------------------------------
114400141028       //?Gestione videata S03.
114500141028       //--------------------------------------------------------------
114600141028       BEGSR GesS03;
114700141028
114800141028       //?Inizializzazione videata
114900141029         IF  wInzS03;
115000141028           exsr InzS03;
115100141029           wInzS03 = *off;
115200141118           IF  wMaxNrr;
115300141118             leavesr;
115400141118           ENDIF;
115500141028         ENDIF;
115600141028
115700141028       //?Visualizzazione del SFL (se ci sono dati)
115800141117         SflDsp = (S03nrr <= *zeros);
115900141104
116000141104       //?Posizionamento cursore
116100141112         V03rcd = V03csr;
116200141111
116300141111       //?Se utente di sede abilito anche l'aggiunta di nuovi clienti
116400141111       //?con F10
116500150507       //?(SE Campagna NON scaduta)?
116600150505         AbilitaF10 = (DUTpou = 046  and  CMPdfc >= Oggi);
116700141127
116800150119       //?Se utente di sede e richiesto
116900150128       //?Obiettivo Inizio = Obiettivo Proposto
117000150507       //?e SE Campagna NON scaduta?
117100141127       //?abilito F13 per conferma obiettivo proposto
117200150212         AbilitaF13 = (DUTpou = 046 and V04obj = 'P' and
117300150507                       CMPdfc >= Oggi  and
117400150216                      ((V04formula = Formula(1) and
117500150216                       ((V04obj1 = 'I' and V04obj2 = 'P') or
117600150216                        (V04obj1 = 'P' and V04obj2 = 'I'))) or
117700150216                       (V04formula = Formula(2) and
117800150216                        V04obj1 = 'P' and V04obj2 = 'I')));
117900141028
118000141028       //?Emissione Testata e Piede con tasti funzionali abilitati
118100150112         write  KC01T01;
118200150112         write  KC01P03;
118300141028
118400141028       //?Emissione videata
118500150112         exfmt  KC01C03;
118600141028         reset ErrMessage;
118700141028         reset ErrGenerico;
118800141028         clear V03msg;
118900141028
119000141028         SELECT;
119100141030
119200141030       //?- F01=Guida Fasi
119300141030           WHEN  dsp_aid = c_F01;
119400141030             exsr F01S03;
119500141028
119600141028       //?- F03=Fine
119700141028           WHEN  dsp_aid = c_F03;
119800141029             exsr F03D02;
119900141030
120000141030       //?- F05=Altre parzializzazioni
120100141030           WHEN  dsp_aid = c_F05;
120200141030             exsr F05S03;
120300141030
120400141030       //?- F08=Ord.X Imp.Cliente
120500141030           WHEN  dsp_aid = c_F08;
120600141030             exsr F08S03;
120700141111
120800141111       //?- F10=Aggiunta Cliente
120900141111           WHEN  dsp_aid = c_F10;
121000141111             exsr F10S03;
121100141028
121200141028       //?- F12=Ritorno
121300141028           WHEN  dsp_aid = c_F12;
121400141028             exsr F12S03;
121500141127
121600141127       //?- F13=Conferma Obiettivo Proposto
121700141127           WHEN  dsp_aid = c_F13;
121800141127             exsr F13S03;
121900141028
122000141028       //?Invio
122100141028           OTHER;
122200141028             IF  V03csr > *zero;
122300141028               exsr OpzS03;
122400141028               IF  ErrGenerico;
122500141028                 leavesr;
122600141028               ENDIF;
122700141028             ENDIF;
122800141028
122900141028         ENDSL;
123000141028
123100141028       ENDSR;
123200141028
123300141028       //--------------------------------------------------------------
123400141028       //?Inizializzazione videata S03.
123500141028       //--------------------------------------------------------------
123600141029       BEGSR InzS03;
123700141028
123800141028         EndS03 = *off;
123900141112         clear V03tot;
124000150116         TroppiRcd = *off;
124100141202
124200141202       //?Imposto la descrizione della campagna
124300150121         V01des = CMPdes;
124400150212
124500150212       //?Periodo Confronto Fatturazione
124600150212         V03aammcf = %subst(CMPflo:5:2) + '/' + %subst(CMPflo:1:4);
124700141028
124800141028       //?Pulizia subfile
124900141028         exsr PulS03;
125000150212
125100150212       //?Imposto i dati per videata delle parzializzazioni
125200150212       //?e per caricamento subfile
125300150212         IF  wInzD04;
125400150212           exsr InzD04;
125500150212           wInzD04 = *off;
125600150212         ENDIF;
125700141028
125800141028       //?Caricamento subfile
125900141028         exsr Ries03;
126000141118
126100141118       //?Se ho superato il numero massimo di rcd da caricare esco
126200141118         IF  wMaxNrr;
126300141124           clear V03tot;
126400141118           Video = 'D5';
126500141118           wInzD05 = *on;
126600141118           leavesr;
126700141118         ENDIF;
126800141112
126900141117         rrnlast = S03nrr;
127000141112
127100141112       //?Faccio l'ordinamento
127200141112         SELECT;
127300141112       //?se ordina x RAG devo ordinare per CLV
127400141112         WHEN  VisOrdRAG;
127500141112           exsr Ordina_x_CLV;
127600141112       //?se ordina x CLV devo ordinare per RAG
127700141112         WHEN  VisOrdCLV;
127800141112           exsr Ordina_x_RAG;
127900141112         ENDSL;
128000141112
128100141112         SflEnd = *on;
128200141112
128300141112       //?Imposto il posizionamento al primo rcd
128400141112         IF  S03nrr > 0;
128500141112           V03rcd = 1;
128600141112         ELSE;
128700141112           clear V03rcd;
128800141112         ENDIF;
128900141112
129000141112         V03csr = V03rcd;
129100141117
129200150213       //?Imposto il n. rcd del subfile solo se il nrr savlato
129300150213       //?è <= all'ultimo nrr del subfile appena caricato
129400150213       //?se no imposto l'ultimo nrr
129500150213         IF  sav_S03nrr > 0;
129600150213           IF  sav_S03nrr <= rrnlast;
129700150213             V03csr = sav_S03nrr;
129800150213             clear sav_S03nrr;
129900150213           ELSE;
130000150213             V03csr = rrnlast;
130100150213           ENDIF;
130200141117         ENDIF;
130300141028
130400141028       ENDSR;
130500141028
130600141028       //--------------------------------------------------------------
130700141028       //?Pulizia Subfile S03.
130800141028       //--------------------------------------------------------------
130900141028       BEGSR PulS03;
131000141028
131100141028       //?Pulizia subfile
131200141028         SflDsp    = *on;
131300141028         SflDspCtl = *on;
131400150112         write KC01C03;
131500141028         SflDspCtl = *off;
131600141028         SflEnd    = *off;
131700141028
131800141112         clear V03rcd;
131900141028         clear V03csr;
132000141028         clear S03nrr;
132100141028         clear V03msg;
132200141028
132300141028         ErrMessage  = *off;
132400141028         ErrGenerico = *off;
132500141028
132600141028         WindDspF = IndDspF;
132700141028         reset WindDspFb;
132800141028         IndDspF  = WindDspF;
132900141028
133000141104       ENDSR;
133100141028
133200141028       //--------------------------------------------------------------
133300141028       //?Caricamento Subfile S03.
133400141028       //--------------------------------------------------------------
133500141028       BEGSR RieS03;
133600141028
133700141028         EndS03 = *off;
133800141103         wMaxNrr = *off;
133900141028
134000141028       //?Preparo Stringa Sql
134100141113         exsr PreparaSQL;
134200141028
134300141028       //?Dichiarazione cursore
134400141028         exec sql
134500141028           prepare S1   from :wSQL;
134600141028         exec sql
134700141124           declare WRK cursor for S1;
134800141124          // declare WRK  insensitive no scroll cursor for S1;
134900141028
135000141028         //?Apertura del cursore
135100141028         exec sql
135200141028           open WRK;
135300141028
135400141028         IF sqlcode < 0;
135500141028           EndS03 = *on;
135600141113           exec sql close WRK;
135700141113           leavesr;
135800141028         ENDIF;
135900141028
136000141028         DOW  not EndS03;
136100141028           exec sql
136200150115             fetch next from WRK into :TICMI00F, :CMCufe, :CMCcch;
136300141028           IF sqlcod = 100 or sqlcod < 0;
136400141028             EndS03 = *on;
136500141028             leave;
136600141028           ENDIF;
136700150303
136800150303         //?Controllo se ok il rcd per la selezione sulle trattative
136900150303           IF  V04ttr <> *blanks and V04ttr <> 'NO' and V04ttr <> 'SI';
137000150303             exsr CTRttr;
137100150303             IF  not RecOK;
137200150303               iter;
137300150303             ENDIF;
137400150303           ENDIF;
137500141124
137600141124         //?Controllo se ok il rcd
137700141124         //?per il "di cui" obiettivo
137800150212           IF  V04obj1 <> *blanks and V04obj2 <> *blanks;
137900141124             exsr CTRrec;
138000141124             IF  not RecOK;
138100141124               iter;
138200141124             ENDIF;
138300141124           ENDIF;
138400150213         //?per la parzializzazione dal al Obiettivo/Andamento
138500150213           IF  V04obj3 <> *blanks;
138600150213             exsr CTRrec1;
138700150213             IF  not RecOK;
138800150213               iter;
138900150213             ENDIF;
139000150213           ENDIF;
139100141028
139200141028         //?Carico i dati nel subfile
139300141028           exsr CarS03;
139400141028
139500141028         ENDDO;
139600141028
139700141028         exec sql
139800141028           close WRK;
139900141124
140000141124         V03tot = S03nrr;
140100141028
140200141028       ENDSR;
140300141028
140400141028       //--------------------------------------------------------------
140500141028       //?Preparo la Stringa SQL.
140600141028       //--------------------------------------------------------------
140700141028       BEGSR PreparaSQL;
140800141113
140900150303       //?Scelta trattative
141000141113         SELECT;
141100150303       //?Con trattativa  (fase TR o TF)
141200150303         WHEN  V04ttr <> 'NO' and V04ttr <> *blanks;
141300141113           wSQL = 'with SEL001 as ' +
141400141113                  '(select max (digits(CMFdfa) concat '+
141500141113                  'digits(CMFhfc)) as time, ' +
141600141113                  'CMFncm, CMFksu, CMFksc, CMFcpo ' +
141700141113                  'from TICMF00F ' +
141800150223                  'where CMFacm in('+ '''' + FaseObjTtr + '''' +
141900150223                  ', ' + '''' + FaseObjTf + '''' + ')' +
142000150212                  ' and CMFncm = ' + %editc(VH4ncm:'X') +
142100141113                  ' group by CMFncm, CMFksu, CMFksc, CMFcpo)' +
142200150115                  'select TICMI00F.*, CMCufe, CMCcch from SEL001 ' +
142300141113                  'join TICMI00F on ' +
142400141113                  'CMFncm = CMIncm and CMFksu = CMIksu and ' +
142500141113                  'CMFksc = CMIksc and CMFcpo = CMIcpo ' +
142600141113                  'join TICMC00F on ' +
142700141113                  'CMIncm = CMCncm and CMIksu = CMCksu and ' +
142800150303                  'CMIksc = CMCksc and CMIcpo = CMCcpo ' +
142900150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
143000141113           exsr AggSelSQL;
143100150303
143200150303       //?Nessuna trattativa
143300150303         WHEN  V04ttr = 'NO';
143400150303           wSQL = 'with SEL001 as ' +
143500150303                  '(select TICMI00F.* from TICMI00F '+
143600150303                  'exception join TICMF00F on ' +
143700150303                  'CMIncm = CMFncm and CMIksu = CMFksu and ' +
143800150303                  'CMIksc = CMFksc and CMIcpo = CMFcpo and ' +
143900150303                  'CMFacm in(' + '''' + FaseObjTtr + '''' +
144000150303                  ', ' + '''' + FaseObjTf + '''' + ') ' +
144100150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
144200150303           exsr AggSelSQL;
144300150303           wSQL += ') ';
144400150303           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
144500150303                   'from SEL001, TICMC00F ' +
144600150303                   'where ' +
144700150303                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
144800150303                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
144900141113
145000141113         OTHER;
145100150303       //?"  " = Nessuna scelta
145200141113           wSQL = 'with SEL001 as ' +
145300141113                  '(select TICMI00F.* from TICMI00F ' +
145400150212                  ' where CMIncm = ' + %editc(VH4ncm:'X');
145500141113           exsr AggSelSQL;
145600141113           wSQL += ') ';
145700150115           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
145800141113                   'from SEL001, TICMC00F ' +
145900141113                   'where ' +
146000141113                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
146100141113                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
146200141113         ENDSL;
146300150119
146400150213       //?Clienti Bloccati
146500150213         IF  V04cli = 'E';
146600150119           wSQL += ' and CMCcch = ' + '''   ''';
146700150119         ENDIF;
146800150213         IF  V04cli = 'I';
146900150213           wSQL += ' and CMCcch <> ' + '''   ''';
147000150213         ENDIF;
147100150213         IF  V04cli = *blanks;
147200150213         ENDIF;
147300150119
147400150119       //?Clienti Nuovi
147500150212         IF  V04ncli = 'S';
147600150119           wSQL += ' and CMIcln = ' + '''N''' +
147700150119                   ' and CMCcch = ' + '''   ''';
147800150119         ENDIF;
147900141113
148000141113         SELECT;
148100150128       //?Richiesta parzializzazione obiettivo Inizio
148200150212         WHEN  V04obj = 'I';
148300141113           wSQL += ' and CMCufe = ' + '''' + FaseObj + '''';
148400141113       //?Richiesta parzializzazione obiettivo Proposto
148500150212         WHEN  V04obj = 'P';
148600141113           wSQL += ' and CMCufe = ' + '''' + FaseObjProp + '''';
148700141113       //?Richiesta parzializzazione obiettivo Finale
148800150212         WHEN  V04obj = 'F';
148900141113           wSQL += ' and CMCufe = ' + '''' + FaseObjFine + '''';
149000141113         ENDSL;
149100141028
149200141028         wSQL += ' for fetch only';
149300141028
149400141028       ENDSR;
149500141113
149600141113       //--------------------------------------------------------------
149700141113       //?Aggiungo le selezioni alla stringa SQL.
149800141113       //--------------------------------------------------------------
149900141113       BEGSR AggSelSQL;
150000141113
150100141113         wclv = *off;
150200141119
150300141119       //?Se non sono richiesti Distretto/Area/Filiale DEVO caricare solo i clienti
150400141119       //?abilitati all'utente
150500150212         IF  V04cdi = *blanks and VH4car = *zeros and VH4fil = *zeros;
150600141119           xx = 1;
150700141119           Primo = *off;
150800141119           wSQL += ' and CMIfcm in(';
150900141119           FOR  xx by 1 to %elem(POG);
151000141119             IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
151100141119               IF  Primo;
151200141119                 wsql += ', ';
151300141119               ELSE;
151400141119                 Primo = *on;
151500141119               ENDIF;
151600141119               wsql += POG(xx);
151700141119             ENDIF;
151800141119           ENDFOR;
151900141119           wsql += ')';
152000141119         ENDIF;
152100141113
152200141113       //?Distretto
152300150212         IF  V04cdi <> *blanks;
152400150212           wSQL += ' and CMIdcm = ' + '''' + V04cdi + '''';
152500141113         ENDIF;
152600141113       //?Area
152700150212         IF  VH4car > *zeros;
152800150212           wSQL += ' and CMIacm = ' + %editc(VH4car:'X');
152900141113         ENDIF;
153000141113       //?Filiale
153100150212         IF  VH4fil > *zeros;
153200150212           wSQL += ' and CMIfcm = ' + %editc(VH4fil:'X');
153300141113         ENDIF;
153400141113       //?Commerciale
153500150212         IF  VH4cmm > *zeros;
153600150212           wSQL += ' and CMIcmm = ' + %editc(VH4cmm:'X');
153700141113         ENDIF;
153800141113       //?Cliente
153900150212         IF  VH4ksu > *zeros;
154000150212           wSQL += ' and CMIksu = ' + %editc(VH4ksu:'X');
154100141113         ENDIF;
154200141113       //?Importanza Cliente
154300150212         IF  V04clv1 <> *blanks;
154400150212           wSQL += ' and CMIclv in(' + '''' + V04clv1 + '''';
154500141113           wclv = *on;
154600141113         ENDIF;
154700150212         IF  V04clv2 <> *blanks and wclv;
154800150212           wSQL += ',' + '''' + V04clv2 + '''';
154900141113           wclv = *on;
155000141113         ENDIF;
155100150212         IF  V04clv2 <> *blanks and not wclv;
155200150212           wSQL += ' and CMIclv in(' + '''' + V04clv2 + '''';
155300141113           wclv = *on;
155400141113         ENDIF;
155500150212         IF  V04clv3 <> *blanks and wclv;
155600150212           wSQL += ',' + '''' + V04clv3 + '''';
155700141113           wclv = *on;
155800141113         ENDIF;
155900150212         IF  V04clv3 <> *blanks and not wclv;
156000150212           wSQL += ' and CMIclv in(' + '''' + V04clv3 + '''';
156100141113           wclv = *on;
156200141113         ENDIF;
156300150212         IF  V04clv4 <> *blanks and wclv;
156400150212           wSQL += ',' + '''' + V04clv4 + '''';
156500141113           wclv = *on;
156600141113         ENDIF;
156700150212         IF  V04clv4 <> *blanks and not wclv;
156800150212           wSQL += ' and CMIclv in(' + '''' + V04clv4 + '''';
156900141113           wclv = *on;
157000141113         ENDIF;
157100150212         IF  V04clv5 <> *blanks and wclv;
157200150212           wSQL += ',' + '''' + V04clv5 + '''';
157300141113           wclv = *on;
157400141113         ENDIF;
157500150212         IF  V04clv5 <> *blanks and not wclv;
157600150212           wSQL += ' and CMIclv in(' + '''' + V04clv5 + '''';
157700141113           wclv = *on;
157800141113         ENDIF;
157900141113         IF  wclv;
158000141113           wSQL += ')';
158100141113         ENDIF;
158200141113       //?Delta
158300150212         IF  v04delda <> *zeros and V04dela <> *zeros;
158400141119           wSQL += ' and CMIpde between ' + %editc(wdelda:'O') +
158500141119                   ' and ' + %editc(wdela:'O');
158600141113         ENDIF;
158700141113
158800141113       ENDSR;
158900150303
159000150303       //--------------------------------------------------------------
159100150303       //?Controllo se OK il record per selezione trattative.
159200150303       //--------------------------------------------------------------
159300150303       BEGSR CTRttr;
159400150303
159500150303         RecOk = *off;
159600150303         clear wfase;
159700150303         wflgp = V04ttr;
159800150303         wtpric = 'P';
159900150303
160000150303         exsr CallTRKC76;
160100150303         IF  OKC76err <> *blanks;
160200150303           leavesr;
160300150303         ENDIF;
160400150303
160500150303         RecOK = *on;
160600150303
160700150303       ENDSR;
160800141124
160900141124       //--------------------------------------------------------------
161000141124       //?Controllo se OK il record.
161100141124       //--------------------------------------------------------------
161200141124       BEGSR CTRrec;
161300141124
161400141124         RecOk = *off;
161500150128         clear wvalobj1;
161600150128         clear wvalobj2;
161700141124
161800141124       //?Cerco il "di cui" 1
161900141124         SELECT;
162000150128       //?Inizio
162100150212         WHEN  V04obj1 = 'I';
162200150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
162300150218           IF  not %found(TICMF01L);
162400141126             leavesr;
162500141126           ENDIF;
162600150218           wvalobj1 = CMFpea;
162700141124       //?Proposto
162800150212         WHEN  V04obj1 = 'P';
162900150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
163000150218           IF  not %found(TICMF01L);
163100141126             leavesr;
163200141126           ENDIF;
163300150218           wvalobj1 = CMFpea;
163400141124       //?Finale
163500150212         WHEN  V04obj1 = 'F';
163600150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
163700150218           IF  not %found(TICMF01L);
163800141126             leavesr;
163900141126           ENDIF;
164000150218           wvalobj1 = CMFpea;
164100150119       //?Trattativa
164200150212         WHEN  V04obj1 = 'T';
164300150223           wfase = FaseObjTtr;
164400150303           wtpric = 'F';
164500150223           exsr CallTRKC76;
164600150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
164700150223             leavesr;
164800150223           ENDIF;
164900150223           wvalobj1 = OKC76pea;
165000150119       //?Confronto Fatturazione
165100150212         WHEN  V04obj1 = 'C';
165200150223           wfase = FaseObjCf;
165300150303           wtpric = 'F';
165400150223           exsr CallTRKC76;
165500150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
165600150223             leavesr;
165700150223           ENDIF;
165800150223           wvalobj1 = OKC76pea;
165900141124         ENDSL;
166000141124
166100141124       //?Cerco il "di cui" 2
166200141124         SELECT;
166300150128       //?Inizio
166400150212         WHEN  V04obj2 = 'I';
166500150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
166600150218           IF  not %found(TICMF01L);
166700141126             leavesr;
166800141126           ENDIF;
166900150218           wvalobj2 = CMFpea;
167000141124       //?Proposto
167100150212         WHEN  V04obj2 = 'P';
167200150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
167300150218           IF  not %found(TICMF01L);
167400141126             leavesr;
167500141126           ENDIF;
167600150218           wvalobj2 = CMFpea;
167700141124       //?Finale
167800150212         WHEN  V04obj2 = 'F';
167900150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
168000150218           IF  not %found(TICMF01L);
168100141126             leavesr;
168200141126           ENDIF;
168300150218           wvalobj2 = CMFpea;
168400150119       //?Trattativa
168500150212         WHEN  V04obj2 = 'T';
168600150223           wfase = FaseObjTtr;
168700150303           wtpric = 'F';
168800150223           exsr CallTRKC76;
168900150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
169000150223             leavesr;
169100150223           ENDIF;
169200150223           wvalobj2 = OKC76pea;
169300150119       //?Confronto Fatturazione
169400150212         WHEN  V04obj2 = 'C';
169500150223           wfase = FaseObjCf;
169600150303           wtpric = 'F';
169700150223           exsr CallTRKC76;
169800150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
169900150223             leavesr;
170000150223           ENDIF;
170100150223           wvalobj2 = OKC76pea;
170200141124         ENDSL;
170300141124
170400141127       //?controllo i 2 obiettivi
170500141127         SELECT;
170600150212         WHEN  V04formula = Formula(1) and wvalobj1 = wvalobj2;
170700141127           RecOK = *on;
170800150212         WHEN  V04formula = Formula(2) and wvalobj1 > wvalobj2;
170900141127           RecOK = *on;
171000150212         WHEN  V04formula = Formula(3) and wvalobj1 < wvalobj2;
171100141127           RecOK = *on;
171200150212         WHEN  V04formula = Formula(4) and wvalobj1 >= wvalobj2;
171300150119           RecOK = *on;
171400150212         WHEN  V04formula = Formula(5) and wvalobj1 <= wvalobj2;
171500141127           RecOK = *on;
171600150212         WHEN  V04formula = Formula(6) and wvalobj1 <> wvalobj2;
171700141127           RecOK = *on;
171800141127         ENDSL;
171900141124
172000141124       ENDSR;
172100150213
172200150213       //--------------------------------------------------------------
172300150213       //?Controllo se OK il record.
172400150213       //--------------------------------------------------------------
172500150213       BEGSR CTRrec1;
172600150213
172700150213         RecOk = *off;
172800150213         clear wvalobj3;
172900150213
173000150213       //?Cerco Obiettivo/Andamento richiesto
173100150213         SELECT;
173200150213       //?Inizio
173300150213         WHEN  V04obj3 = 'I';
173400150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
173500150218           IF  not %found(TICMF01L);
173600150213             leavesr;
173700150213           ENDIF;
173800150218           wvalobj3 = CMFpea;
173900150213       //?Proposto
174000150213         WHEN  V04obj3 = 'P';
174100150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
174200150218           IF  not %found(TICMF01L);
174300150213             leavesr;
174400150213           ENDIF;
174500150218           wvalobj3 = CMFpea;
174600150213       //?Finale
174700150213         WHEN  V04obj3 = 'F';
174800150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
174900150218           IF  not %found(TICMF01L);
175000150213             leavesr;
175100150213           ENDIF;
175200150218           wvalobj3 = CMFpea;
175300150213       //?Trattativa
175400150213         WHEN  V04obj3 = 'T';
175500150223           wfase = FaseObjTtr;
175600150303           wtpric = 'F';
175700150223           exsr CallTRKC76;
175800150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
175900150223             leavesr;
176000150213           ENDIF;
176100150223           wvalobj3 = OKC76pea;
176200150213       //?Confronto Fatturazione
176300150213         WHEN  V04obj3 = 'C';
176400150223           wfase = FaseObjCf;
176500150303           wtpric = 'F';
176600150223           exsr CallTRKC76;
176700150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
176800150223             leavesr;
176900150213           ENDIF;
177000150223           wvalobj3 = OKC76pea;
177100150213         ENDSL;
177200150213
177300150213       //?confronto con il "dal" - "al" richiesto
177400150213         IF  wvalobj3 >= wobj3da and wvalobj3 <= wobj3a;
177500150213           RecOK = *on;
177600150213         ENDIF;
177700150213
177800150213       ENDSR;
177900141028
178000141028       //--------------------------------------------------------------
178100141028       //?Carico i dati nel Subfile S03.
178200141028       //--------------------------------------------------------------
178300141028       BEGSR CarS03;
178400141028
178500150112         clear  KC01S03;
178600141127         PosCurOPZ = *off;
178700141028
178800141029       //?Imposto i campi da TICMI
178900141103         VS3clv = CMIclv;
179000141107         VS3ksu = CMIksu;
179100141107         VS3ksc = CMIksc;
179200141107         VS3cpo = CMIcpo;
179300150115       //?Imposto i campi da TICMC
179400141114         VS3ufe = CMCufe;
179500150115         VS3cch = CMCcch;
179600150115         clear VS3cchaut;
179700150115       //?Imposto se causale automatica
179800150115         IF  VS3cch <> *blanks;
179900150115           clear TIBS02DS;
180000150115           clear dECM;
180100150115           T02mod = 'C';
180200150115           T02cod = 'ECM';
180300150115           T02ke1 = VS3cch;
180400150115           T02sif = KNSIF;
180500150115           TNTBE_RicercaControllo  (kpjba : tibs02ds);
180600150115           dECM = T02uni;
180700150115           IF  §ECMaut <> *blanks;
180800150115             VS3cchaut = 'S';
180900150115           ENDIF;
181000150115         ENDIF;
181100150116         clear VS3tipocl;
181200141028         IF  CMIcln = 'N';
181300150116           VS3tipocl = '*';
181400141028         ENDIF;
181500150116         IF  CMCcch <> *blanks;
181600150213           VS3tipocl = 'B';
181700150116         ENDIF;
181800141028         clear TIBS69DS;
181900141028         clear ACOrag;
182000141028         I69kac = CMIksu;
182100141028         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
182200141103         VS3rag = ACOrag;
182300141103         VS3istat = CMIist;
182400141103         VS3del = CMIpde;
182500141103         VS3sped = CMInsp;
182600141103         VS3ric = CMIric;
182700141103         VS3pkgm = CMIpme;
182800141029
182900141029       //?Imposto i campi da TICMF
183000150128       //?Obiettivo inizio
183100141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
183200141113         IF  %found(TICMF01L);
183300141125           VS3obj = %editc(CMFpea:'J');
183400141113         ENDIF;
183500141111       //?Obiettivo Proposto
183600141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
183700141113         IF  %found(TICMF01L);
183800141125           VS3objprop = %editc(CMFpea:'J');
183900141127           VS3oprop = CMFpea;
184000141113         ENDIF;
184100141029       //?Obiettivo Finale
184200141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
184300141113         IF  %found(TICMF01L);
184400141125           VS3objfine = %editc(CMFpea:'J');
184500141113         ENDIF;
184600150220       //?Aumento da Trattativa
184700150220         wfase = FaseObjTtr;
184800150303         wtpric = 'F';
184900150220         exsr CallTRKC76;
185000150220         IF  OKC76err = *blanks;
185100150220           VS3esitotr = OKC76flag;
185200150223           VS3objttr = OKC76peaa;
185300150224           IF  OKC76anno > 0;
185400150224             VS3decotr = (OKC76mese * 10000) + OKC76anno;
185500150224           ENDIF;
185600150220           VS3nrv = OKC76nrv;
185700150225           VS3forza = OKC76forza;
185800150220         ENDIF;
185900141029       //?% Confronto Fatturazione
186000150220         wfase = FaseObjCf;
186100150303         wtpric = 'F';
186200150220         exsr CallTRKC76;
186300150220         IF  OKC76err = *blanks;
186400150223           VS3objcf = OKC76peaa;
186500150220         ENDIF;
186600141029
186700141029       //?Imposto descrizione del commerciale
186800141029         clear CMMdes;
186900141029         chain CMIcmm AZCMM01L;
187000141029         IF  %found(AZCMM01L);
187100141103           VS3cmmd = CMMdes;
187200141029         ENDIF;
187300141117
187400141117       //?Imposto ordinamento importanza cliente
187500141117         clear xx;
187600141117         xx = %lookup(VS3clv:skIC);
187700141117         IF  xx > 0;
187800141117           VS3ord = skIC_ord(xx);
187900141117         ELSE;
188000141117           clear VS3ord;
188100141117         ENDIF;
188200141028
188300141029         S03nrr += 1;
188400141124
188500141124       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
188600141124         IF  S03nrr >= 9999;
188700141124           wMaxNrr = *on;
188800141124           EndS03 = *on;
188900141124           leavesr;
189000141124         ENDIF;
189100141124
189200150112         write  KC01S03;
189300141028
189400141028       ENDSR;
189500141030
189600141030       //--------------------------------------------------------------
189700141030       //?Gestione tasto funzionale F01 da videata S03
189800141111       //?F01=Campagna
189900141030       //--------------------------------------------------------------
190000141030       BEGSR F01S03;
190100141112
190200150112         clear TRKC02DS;
190300150112         IKC02ric = 'I';
190400150212         IKC02ncm = VH4ncm;
190500150112         trkc02r (kpjba:trkc02ds);
190600141030
190700141030       ENDSR;
190800141030
190900141030       //--------------------------------------------------------------
191000141030       //?Gestione tasto funzionale F05 da videata S03
191100141030       //?F05=Altre Parzializzazioni
191200141030       //--------------------------------------------------------------
191300141030       BEGSR F05S03;
191400141030
191500141030       //?Videata per nuove parzializzazioni
191600141030         Video = 'D4';
191700141030
191800141030       ENDSR;
191900141030
192000141030       //--------------------------------------------------------------
192100141030       //?Gestione tasto funzionale F08 da videata S03
192200141030       //?F08=Ord. X Imp.Cliente
192300141030       //--------------------------------------------------------------
192400141030       BEGSR F08S03;
192500141030
192600141111       //?F8 visualizzato richiede ordinamento x RAG
192700141111         IF  VisOrdRAG;
192800141111           exsr Ordina_x_RAG;
192900141030           leavesr;
193000141030         ENDIF;
193100141030
193200141030       //?F8 visualizzato richiede ordinamento x CLV
193300141030         IF  VisOrdCLV;
193400141030           exsr Ordina_x_CLV;
193500141030           leavesr;
193600141030         ENDIF;
193700141030
193800141030       ENDSR;
193900141111
194000141111       //--------------------------------------------------------------
194100141111       //?Gestione tasto funzionale F10 da videata S03
194200141111       //?F10=Aggiunta Cliente
194300141111       //--------------------------------------------------------------
194400141111       BEGSR F10S03;
194500141111
194600150112         clear TRKC06DS;
194700150212         IKC06ncm = VH4ncm;
194800150112         trkc06r (kpjba:TRKC06DS);
194900141111
195000141111       //?Ricarico il subfile
195100141111         wInzS03 = *on;
195200141111
195300141111       ENDSR;
195400141028
195500141028       //--------------------------------------------------------------
195600141028       //?Gestione tasto funzionale F12 da videata S03
195700141028       //?F12=Ritorno
195800141028       //--------------------------------------------------------------
195900141028       BEGSR F12S03;
196000141028
196100141028       //?Ritorno alle selezioni
196200141028         Video = 'D2';
196300141029         wInzD02 = *on;
196400150225         clear sav_s03nrr;
196500150223
196600150223       //?Se richiamato chiudo i file per il pgm TRKC76R
196700150223         IF  wTRKC76;
196800150223           clear TRKC76DS;
196900150223           IKC76ric = 'C';
197000150223           trkc76r (kpjba:TRKC76DS);
197100150223         ENDIF;
197200141028
197300141028       ENDSR;
197400141127
197500141127       //--------------------------------------------------------------
197600141127       //?Gestione tasto funzionale F13 da videata S03
197700141127       //?F13=Conferma Obiettivo proposto.
197800141127       //--------------------------------------------------------------
197900141127       BEGSR F13S03;
198000141127
198100150121         Video = 'D7';
198200150121         wInzD07 = *on;
198300141127
198400141127       ENDSR;
198500141030
198600141030       //--------------------------------------------------------------
198700141111       //?Ordinamento x RAG Subfile S03.
198800141030       //--------------------------------------------------------------
198900141111       BEGSR Ordina_x_RAG;
199000141030
199100141030         VisOrdCLV = *on;
199200141111         VisOrdRAG = *off;
199300141030
199400141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
199500141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
199600141030       // il campo è usato per selezionare i records dando un ordine a quelli
199700141030       // selezionati davanti ai non selezionati.
199800141030         clear QLGSCB;
199900141030         clear QLGSCB00;
200000141030
200100141118       // 2 campi chiave x Ragione Sociale - Codice (KSU)
200200141030         QLGNBRK = 1;
200300141030
200400141111       // imposto la posizione del RAG sul subfile e la lunghezza
200500141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
200600141117         QLGSP = 1 + %size(VS3clv) + %size(VS3ord)
200700150116                   + %size(VS3ksu) + %size(VS3tipocl);
200800141111         QLGSS = %SIZE(VS3rag);
200900141118         QLGDT = Carattere;
201000141030         QLGSO = Ascendente;
201100141030         QLGKL(1) = QLGSKL;
201200141118
201300141118       // imposto la posizione del KSU sul subfile e la lunghezza
201400141118       // l'ordinamento è su un campo numerico e deve essere ascendente
201500141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord);
201600141118         QLGSS = %SIZE(VS3ksu);
201700141118         QLGDT = Numerico;
201800141118         QLGSO = Ascendente;
201900141118         QLGKL(2) = QLGSKL;
202000141030
202100141030       // Load other sort parameters.
202200141030         QLGLB = 80 + 16 * MaxKey;
202300141030         QLGRL = %SIZE(sflrcd) - 1;
202400141030         QLGRT = 8;
202500141030         QLGOKL = 80;
202600141030         QLGLKE = 16;
202700141030         QLGLSS = 290;
202800141030
202900141030       // Initialize Sort I/O API fields.
203000141030         QLGRL00 = QLGRL;
203100141030         QLGRC00 = 1;
203200141030         clear QUSEI;
203300141030         QUSBPRV = %SIZE(QUSEC);
203400141030
203500141030       // First step - Initialize the sort routine.
203600141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
203700141030
203800141030       // Next step - Write records to I/O routine.
203900141030         QLGRT00 = Put;
204000141103         FOR  xx = 1 to rrnlast;
204100150112           chain xx KC01S03;
204200141030
204300141030       // solo le righe con Selected = 'Y' sono riordinate,
204400141030       // quindi per fare un ordinamento di tutte le righe
204500141030       // metto 'Y' sempre.
204600141103           selected  = 'Y';
204700141103           clear QUSEI;
204800141103           QUSBPRV = %SIZE(QUSEC);
204900141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
205000141103         ENDFOR;
205100141030
205200141030       // Next step - Signal end of input, clear subfile for reload.
205300141030         QLGRT00 = EndPut;
205400141030         clear QUSEI;
205500141030         QUSBPRV = %SIZE(QUSEC);
205600141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
205700141030       // pulizia SFL
205800141030         exsr PulS03;
205900141030
206000141030       // Final step - Write the records back to the subfile.
206100141030         QLGRT00 = Get;
206200141103         FOR  xx = 1 to rrnlast;
206300141103           clear QUSEI;
206400141103           QUSBPRV = %SIZE(QUSEC);
206500141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
206600141111           attrrag = visHI;
206700141103           clear attrclv;
206800150121           IF  VS3objcf <> *blanks;
206900141103             attrobjcf = visRI;
207000141103           ELSE;
207100141103             clear attrobjcf;
207200141103           ENDIF;
207300141121           IF  VS3esitotr <> *blanks;
207400141121             Visesito = *on;
207500141121           ELSE;
207600141121             Visesito = *off;
207700141121           ENDIF;
207800141103           s03nrr = xx;
207900150112           write KC01S03;
208000141103         ENDFOR;
208100141112
208200141117         rrnlast = S03nrr;
208300141112         IF  S03nrr > *zeros;
208400141112           V03rcd = 1;
208500141112           V03csr = 1;
208600141112         ELSE;
208700141112           clear V03rcd;
208800141112         ENDIF;
208900141030
209000141030       ENDSR;
209100141030
209200141030       //--------------------------------------------------------------
209300141030       //?Ordinamento x CLV Subfile S03.
209400141030       //--------------------------------------------------------------
209500141030       BEGSR Ordina_x_CLV;
209600141030
209700141030         VisOrdCLV = *off;
209800141111         VisOrdRAG = *on;
209900141030
210000141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
210100141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
210200141030       // il campo è usato per selezionare i records dando un ordine a quelli
210300141030       // selezionati davanti ai non selezionati.
210400141030         clear QLGSCB;
210500141030         clear QLGSCB00;
210600141030
210700150115       // 3 campi chiave x Importanza cliente, Fatturato e RAG
210800150115         QLGNBRK = 3;
210900141030
211000141118       // imposto la posizione del ORD CLV sul subfile e la lunghezza
211100141118       // l'ordinamento è su un campo numerico e deve essere discendente
211200141117         QLGSP = 1 + %size(VS3clv);
211300141117         QLGSS = %SIZE(VS3ord);
211400141117         QLGDT = Numerico;
211500141117         QLGSO = Discendente;
211600141030         QLGKL(1) = QLGSKL;
211700150115
211800150115       // imposto la posizione del Fatturato sul subfile e la lunghezza
211900150115       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
212000150115         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
212100150116                   + %size(VS3ksu) + %size(VS3tipocl)
212200150115                   + %size(VS3rag) + %size(VS3istat)
212300150115                   + %size(VS3del) + %size(VS3sped);
212400150115         QLGSS = %SIZE(VS3ric);
212500150115         QLGDT = Numerico;
212600150115         QLGSO = Discendente;
212700150115         QLGKL(2) = QLGSKL;
212800141118
212900141118       // imposto la posizione del RAG sul subfile e la lunghezza
213000141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
213100141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
213200150116                   + %size(VS3ksu) + %size(VS3tipocl);
213300141118         QLGSS = %SIZE(VS3rag);
213400141118         QLGDT = Carattere;
213500141118         QLGSO = Ascendente;
213600150115         QLGKL(3) = QLGSKL;
213700141030
213800141030       // Load other sort parameters.
213900141030         QLGLB = 80 + 16 * MaxKey;
214000141030         QLGRL = %SIZE(sflrcd) - 1;
214100141030         QLGRT = 8;
214200141030         QLGOKL = 80;
214300141030         QLGLKE = 16;
214400141030         QLGLSS = 290;
214500141030
214600141030       // Initialize Sort I/O API fields.
214700141030         QLGRL00 = QLGRL;
214800141030         QLGRC00 = 1;
214900141030         clear QUSEI;
215000141030         QUSBPRV = %SIZE(QUSEC);
215100141030
215200141030       // First step - Initialize the sort routine.
215300141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
215400141030
215500141030       // Next step - Write records to I/O routine.
215600141030         QLGRT00 = Put;
215700141103         FOR  xx = 1 to rrnlast;
215800150112           chain xx KC01S03;
215900141030
216000141030       // solo le righe con Selected = 'Y' sono riordinate,
216100141030       // quindi per fare un ordinamento di tutte le righe
216200141030       // metto 'Y' sempre.
216300141103           selected  = 'Y';
216400141103           clear QUSEI;
216500141103           QUSBPRV = %SIZE(QUSEC);
216600141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
216700141103         ENDFOR;
216800141030
216900141030       // Next step - Signal end of input, clear subfile for reload.
217000141030         QLGRT00 = EndPut;
217100141030         clear QUSEI;
217200141030         QUSBPRV = %SIZE(QUSEC);
217300141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
217400141030       // pulizia SFL
217500141030         exsr PulS03;
217600141030
217700141030       // Final step - Write the records back to the subfile.
217800141030         QLGRT00 = Get;
217900141103         FOR  xx = 1 to rrnlast;
218000141103           clear QUSEI;
218100141103           QUSBPRV = %SIZE(QUSEC);
218200141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
218300141111           clear attrrag;
218400141103           attrclv = visHI;
218500150121           IF  VS3objcf <> *blanks;
218600141103             attrobjcf = visRI;
218700141103           ELSE;
218800141103             clear attrobjcf;
218900141103           ENDIF;
219000141121           IF  VS3esitotr <> *blanks;
219100141121             Visesito = *on;
219200141121           ELSE;
219300141121             Visesito = *off;
219400141121           ENDIF;
219500141103           s03nrr = xx;
219600150112           write KC01S03;
219700141103         ENDFOR;
219800141112
219900141117         rrnlast = S03nrr;
220000141112         IF  S03nrr > *zeros;
220100141112           V03rcd = 1;
220200141112           V03csr = 1;
220300141112         ELSE;
220400141112           clear V03rcd;
220500141112         ENDIF;
220600141030
220700141030       ENDSR;
220800141028
220900141028       //--------------------------------------------------------------
221000141028       //?Gestione opzioni Subfile S03.
221100141028       //--------------------------------------------------------------
221200141028       BEGSR OpzS03;
221300141028
221400150112         readc KC01S03;
221500141028
221600150112         DOW  NOT  %eof(TRKC01D);
221700141028
221800141028           SflNxtChg = *off;
221900141028           WindDspF  = IndDspF;
222000141028           reset WindDspFb;
222100141028           IndDspF   = WindDspF;
222200141028           V03rcd    = S03nrr;
222300141028
222400141127           PosCurOPZ = *off;
222500141119           sav_S03nrr = S03nrr;
222600141119
222700141119         //?- Verifico se il cliente è gestibile dall'utente
222800141119           IF  VS3opz <> *blank;
222900141119             clear TNTAA1DS;
223000141119             ITAA1caut = '§utegtc';
223100141119             ITAA1ksc = VS3ksu;
223200141119             kpjbu = TNTAA1DS;
223300141119             tntaa1c (kpjba);
223400150217             TNTAA1DS = kpjbu;
223500141119             IF  OTAA1fabi = 'N';
223600141119               ErrGenerico = *on;
223700141119               ErrMessage  = *on;
223800141127               PosCurOPZ   = *on;
223900150216               V03msg = %trim(Msg(07)) + '. Non in gestione';
224000141119             ENDIF;
224100141119           ENDIF;
224200141028
224300141119         //?- Se ok il cliente
224400141119           IF  not ErrMessage;
224500141119
224600141119             SELECT;
224700141119           //?- Nessuna opzione
224800141119             WHEN  VS3opz = *blank;
224900141028
225000141119           //?- 2 = Modifica
225100150115             WHEN  VS3opz = '2' and VS3cch = *blanks;
225200150127             //?anche in caso di periodo fuori dal range ma utente di sede
225300150115               IF  Oggi < CMPdigo or Oggi > CMPdfgo;
225400150216                 IF  DUTpou = 046;
225500150127                   exsr Opzione2;
225600150127                 ELSE;
225700150115                 ErrMessage  = *on;
225800150115                 ErrGenerico = *on;
225900150115                 PosCurOPZ   = *on;
226000150216                 V03msg = Msg(12);
226100150306                 %subst(V03msg:34:10) =
226200150306                 %subst(%editc(CMPdigo:'X'):7:2) + '/' +
226300150306                 %subst(%editc(CMPdigo:'X'):5:2) + '/' +
226400150306                 %subst(%editc(CMPdigo:'X'):1:4);
226500150306                 %subst(V03msg:48:10) =
226600150306                 %subst(%editc(CMPdfgo:'X'):7:2) + '/' +
226700150306                 %subst(%editc(CMPdfgo:'X'):5:2) + '/' +
226800150306                 %subst(%editc(CMPdfgo:'X'):1:4);
226900150127                 ENDIF;
227000150115               ELSE;
227100150115                 exsr Opzione2;
227200150115               ENDIF;
227300141028
227400150116           //?- 5 = Statistica
227500150116             WHEN  VS3opz = '5';
227600150116               exsr Opzione5;
227700141028
227800141119           //?- I = Interrogazione Clienti
227900141119             WHEN  VS3opz = 'I';
228000141119               exsr OpzioneI;
228100141028
228200141119           //?- T = Gestione Trattativa
228300141119             WHEN  VS3opz = 'T' and VS3nrv > 0;
228400141119               exsr OpzioneT;
228500141028
228600141119           //?- S = Storico Fasi
228700141119             WHEN  VS3opz = 'S';
228800141119               exsr OpzioneS;
228900150127
229000150127           //?- N = Note
229100150127             WHEN  VS3opz = 'N';
229200150127               exsr OpzioneN;
229300150220
229400150220           //?- F = Trattativa Forzata
229500150220             WHEN  VS3opz = 'F';
229600150220               exsr OpzioneF;
229700141028
229800141119             OTHER;
229900141119               ErrMessage  = *on;
230000141119               ErrGenerico = *on;
230100141127               PosCurOPZ   = *on;
230200150216               V03msg = Msg(11);
230300141119             ENDSL;
230400141119           ENDIF;
230500141028
230600141028           //?Aggiornamento sfl
230700141028           SELECT;
230800141028             WHEN  ErrMessage;
230900141028               SflNxtChg = *on;
231000141028               V03csr    = V03rcd;
231100141028             WHEN  ErrGenerico;
231200141028               V03csr = V03rcd;
231300141103               clear VS3opz;
231400141028             OTHER;
231500141028               V03csr = V03rcd;
231600141103               clear VS3opz;
231700141028           ENDSL;
231800150128
231900150128           IF  VS3esitotr <> *blanks;
232000150128             Visesito = *on;
232100150128           ELSE;
232200150128             Visesito = *off;
232300150128           ENDIF;
232400141028
232500150112           update KC01S03;
232600141028
232700141028           IF  ErrMessage or ErrGenerico;
232800141028             leave;
232900141028           ENDIF;
233000141028
233100150112           readc KC01S03;
233200141028
233300141028         ENDDO;
233400141118
233500141118       //?Ricarico il subfile
233600141119         IF  not ErrGenerico and not ErrMessage;
233700141119           wInzS03 = *on;
233800141119         ENDIF;
233900141028
234000141028       ENDSR;
234100141113
234200141113       //--------------------------------------------------------------
234300141113       //?Opzione "2" videata S03.
234400141113       //--------------------------------------------------------------
234500141113       BEGSR Opzione2;
234600141113
234700141114       //?Richiamo pgm x sapere quale obiettivo posso variare
234800150112         clear TRKC72DS;
234900150212         IKC72ncm = VH4ncm;
235000150112         IKC72ksu = VS3ksu;
235100150112         IKC72ksc = VS3ksc;
235200150112         IKC72cpo = VS3cpo;
235300150112         IKC72acm = VS3ufe;
235400150127       //?Se utente di sede e non siamo più nel periodo possibile
235500150127       //?per la modifica obiettivi e l'ultima fase è la 10 forzo come
235600150127       //?ultima fase la 20, in questo modo il pgm di ma la possibilità
235700150127       //?di inserire la 30
235800150127         IF  IKC72acm = FaseObj and DUTpou = 046 and
235900150127            (Oggi < CMPdigo or Oggi > CMPdfgo);
236000150127           IKC72acm = FaseObjProp;
236100150127         ENDIF;
236200150112         trkc72r (kpjba:TRKC72DS);
236300150112         IF  OKC72err <> *blanks;
236400141114           ErrMessage  = *on;
236500141114           ErrGenerico = *on;
236600141127           PosCurOPZ   = *on;
236700150112           V03msg = OKC72msg;
236800141114           leavesr;
236900141114         ENDIF;
237000150115
237100150115       //?Se tutto OK
237200150115       //?richiamo pgm per la variazione obiettivo
237300150115         clear TRKC07DS;
237400150115         IKC07ric = '2';
237500150212         IKC07ncm = VH4ncm;
237600150112         IKC07ksu = VS3ksu;
237700150112         IKC07ksc = VS3ksc;
237800150112         IKC07cpo = VS3cpo;
237900150112         IKC07acm = OKC72acm;
238000150112         trkc07r (kpjba:TRKC07DS);
238100141113
238200141113       ENDSR;
238300150115
238400150115       //--------------------------------------------------------------
238500150115       //?Opzione "R" videata S03.
238600150115       //--------------------------------------------------------------
238700150115       BEGSR OpzioneR;
238800150115
238900150115       //?richiamo il pgm per ripristinare il cliente in campagna
239000150115         clear TRKC07DS;
239100150115         IKC07ric = 'R';
239200150212         IKC07ncm = VH4ncm;
239300150115         IKC07ksu = VS3ksu;
239400150115         IKC07ksc = VS3ksc;
239500150115         IKC07cpo = VS3cpo;
239600150115         IKC07acm = VS3ufe;
239700150115         trkc07r (kpjba:TRKC07DS);
239800150115
239900150115       ENDSR;
240000141103
240100141103       //--------------------------------------------------------------
240200150116       //?Opzione "5" videata S03.
240300141103       //--------------------------------------------------------------
240400150116       BEGSR Opzione5;
240500150217
240600150217       //?- Verifico se l'opzione è utilizzabile dall'utente
240700150217          clear TNTAA1DS;
240800150217          ITAA1caut = '§uteist';
240900150217          ITAA1ksc = VS3ksu;
241000150217          kpjbu = TNTAA1DS;
241100150217          tntaa1c (kpjba);
241200150217          TNTAA1DS = kpjbu;
241300150217          IF  OTAA1fabi = 'N';
241400150217            ErrGenerico = *on;
241500150217            ErrMessage  = *on;
241600150217            PosCurOPZ   = *on;
241700150217            V03msg = %trim(Msg(07)) + '. Non in gestione';
241800150217            leavesr;
241900150217          ENDIF;
242000141103
242100141103         clear TISE60DS;
242200141103         D60op0 = 'VIS';
242300141103         D60op1 = 'F06';
242400141103         D60sce = '3';
242500141107         dsKSC = VS3ksu;
242600141103         skKSA(1) = dsKSA;
242700141103         kpjbu = TISE60DS;
242800141103         tise61r (kpjba);
242900141103
243000141103       ENDSR;
243100141103
243200141103       //--------------------------------------------------------------
243300141103       //?Opzione "I" videata S03.
243400141103       //--------------------------------------------------------------
243500141103       BEGSR OpzioneI;
243600141103
243700141103         clear TNTAI2DS;
243800150122       //?se non è utente di sede richiamo il pgm in modo da poter gestire
243900150122       //?le eventuali attività ancora da eseguire + crearne delle nuove
244000150122         IF  DUTpou <> 046;
244100150122           ITAI2ric = 'A';
244200150122         ENDIF;
244300141107         ITAI2ksc = VS3ksu;
244400141103         tntai2r (kpjba:TNTAI2DS);
244500141103
244600141103       ENDSR;
244700141103
244800141103       //--------------------------------------------------------------
244900141103       //?Opzione "T" videata S03.
245000141103       //--------------------------------------------------------------
245100141103       BEGSR OpzioneT;
245200141103
245300141103         clear TNTA88DS;
245400141103         ITA88fle = '5';
245500141103         ITA88nrv = VS3nrv;
245600141103         tnta88r (kpjba:TNTA88DS);
245700141103
245800141103       ENDSR;
245900141107
246000141107       //--------------------------------------------------------------
246100141107       //?Opzione "S" videata S03.
246200141107       //--------------------------------------------------------------
246300141107       BEGSR OpzioneS;
246400141107
246500150112         clear TRKC03DS;
246600150212         IKC03ncm = VH4ncm;
246700150112         IKC03ksu = VS3ksu;
246800150112         IKC03ksc = VS3ksc;
246900150112         IKC03cpo = VS3cpo;
247000150112         trkc03r (kpjba:TRKC03DS);
247100141107
247200141107       ENDSR;
247300141113
247400141113       //--------------------------------------------------------------
247500141113       //?Opzione "E" videata S03.
247600141113       //--------------------------------------------------------------
247700141113       BEGSR OpzioneE;
247800141114
247900141114       //?richiamo pgm per escludere il cliente dalla campagna
248000150112         clear TRKC07DS;
248100150112         IKC07ric = '2';
248200150212         IKC07ncm = VH4ncm;
248300150112         IKC07ksu = VS3ksu;
248400150112         IKC07ksc = VS3ksc;
248500150112         IKC07cpo = VS3cpo;
248600150112         IKC07acm = FaseChiudi;
248700150112         trkc07r (kpjba:TRKC07DS);
248800141113
248900141113       ENDSR;
249000150127
249100150127       //--------------------------------------------------------------
249200150127       //?Opzione "N" videata S03.
249300150127       //--------------------------------------------------------------
249400150127       BEGSR OpzioneN;
249500150127
249600150127       //?richiamo pgm per gestione Note
249700150127         clear TRKC10DS;
249800150127         IKC10tla = 'L';
249900150212         IKC10ncm = VH4ncm;
250000150127         IKC10ksu = VS3ksu;
250100150127         IKC10ksc = VS3ksc;
250200150127         IKC10cpo = VS3cpo;
250300150127         IKC10acm = VS3ufe;
250400150127         trkc10r (kpjba:TRKC10DS);
250500150127
250600150127       ENDSR;
250700150220
250800150220       //--------------------------------------------------------------
250900150220       //?Opzione "F" videata S03.
251000150220       //--------------------------------------------------------------
251100150220       BEGSR OpzioneF;
251200150225
251300150225       //?Posso fare la fase TF solo nel periodo di inserimento trattative
251400150225         IF  Oggi < CMPdit or Oggi > CMPdft;
251500150225           ErrMessage  = *on;
251600150225           ErrGenerico = *on;
251700150225           PosCurOPZ   = *on;
251800150225           V03msg = Msg(23);
251900150306           %subst(V03msg:39:10) =
252000150306           %subst(%editc(CMPdit:'X'):7:2) + '/' +
252100150306           %subst(%editc(CMPdit:'X'):5:2) + '/' +
252200150306           %subst(%editc(CMPdit:'X'):1:4);
252300150306           %subst(V03msg:53:10) =
252400150306           %subst(%editc(CMPdft:'X'):7:2) + '/' +
252500150306           %subst(%editc(CMPdft:'X'):5:2) + '/' +
252600150306           %subst(%editc(CMPdft:'X'):1:4);
252700150225           leavesr;
252800150225         ENDIF;
252900150220
253000150220       //?Posso immettere la Trattativa Forzata se non presente NESSUNA fase TR
253100150220         chain (VH4ncm:VS3ksu:VS3ksc:VS3cpo:FaseObjTtr) TICMF01L;
253200150220         IF  %found(TICMF01L);
253300150220           ErrMessage  = *on;
253400150220           ErrGenerico = *on;
253500150220           PosCurOPZ   = *on;
253600150220           V03msg = %trim(Msg(11)) + '. Già presente una Trattativa';
253700150220           leavesr;
253800150220         ENDIF;
253900150220
254000150220       //?richiamo pgm per immissione Trattativa Forzata
254100150220         clear TRKC07DS;
254200150220         IKC07ric = '2';
254300150220         IKC07ncm = VH4ncm;
254400150220         IKC07ksu = VS3ksu;
254500150220         IKC07ksc = VS3ksc;
254600150220         IKC07cpo = VS3cpo;
254700150220         IKC07acm = FaseObjTf;
254800150220         trkc07r (kpjba:TRKC07DS);
254900150220
255000150220       ENDSR;
255100141030
255200141030       //--------------------------------------------------------------
255300141030       //?Gestione videata D04.
255400141030       //--------------------------------------------------------------
255500141030       BEGSR GesD04;
255600141030
255700141030       //?Inizializzazione videata
255800141030         IF  wInzD04;
255900141030           exsr InzD04;
256000141030           wInzD04 = *off;
256100141030         ENDIF;
256200141030
256300141030       //?Emissione videata
256400150112         exfmt  KC01W04;
256500141030         reset ErrMessage;
256600141030         reset ErrGenerico;
256700141030         clear V04msg;
256800141030
256900141030         SELECT;
257000150119
257100150119       //?- F01=Formula
257200150119           WHEN  dsp_aid = c_F01;
257300150119             exsr F01D04;
257400150128
257500150128       //?- F02=Obiettivo/Andamento
257600150128           WHEN  dsp_aid = c_F02;
257700150128             exsr F02D04;
257800150302
257900150302       //?- F04=Scelta Trattative
258000150302           WHEN  dsp_aid = c_F04;
258100150302             exsr F04D04;
258200141103
258300141103       //?- F06=Conferma
258400141103           WHEN  dsp_aid = c_F06;
258500141104             exsr ContrD04;
258600141104             IF  ErrGenerico;
258700141104               leavesr;
258800141104             ENDIF;
258900141103             exsr F06D04;
259000141030
259100141030       //?- F12=Ritorno
259200141030           WHEN  dsp_aid = c_F12;
259300141030             exsr F12D04;
259400141030
259500141030       //?Invio
259600141030           OTHER;
259700141030             exsr ContrD04;
259800141030             IF  ErrGenerico;
259900141030               leavesr;
260000141030             ENDIF;
260100141030
260200141030         ENDSL;
260300141030
260400141030       ENDSR;
260500141030
260600141030       //--------------------------------------------------------------
260700141030       //?Inizializzazione Videata D04.
260800141030       //--------------------------------------------------------------
260900141030       BEGSR InzD04;
261000141030
261100141030       //?Pulizia videata
261200150112         clear KC01W04;
261300150212
261400150212         V04ncm = V02ncm;
261500150212         V04des = V02des;
261600150212         VH4ncm = %dec(V02ncm:7:0);
261700141030
261800150212         V04cdi = V02cdi;
261900150212         V04cdid = V02cdid;
262000150216         IF  V02car <> *blanks;
262100150212           V04car = V02car;
262200150212           V04card = V02card;
262300150212           VH4car = %dec(V02car:3:0);
262400141030         ENDIF;
262500150216         IF  V02fil <> *blanks;
262600150212           V04fil = V02fil;
262700150212           V04fild = V02fild;
262800150212           VH4fil = %dec(V02fil:3:0);
262900141030         ENDIF;
263000150216         IF  V02cmm <> *blanks;
263100150212           V04cmm = V02cmm;
263200150212           V04cmmd = V02cmmd;
263300150212           VH4cmm = %dec(V02cmm:7:0);
263400141030         ENDIF;
263500150216         IF  V02ksu <> *blanks;
263600150212           V04ksu = V02ksu;
263700150212           V04rag = V02rag;
263800150212           VH4ksu = %dec(V02ksu:7:0);
263900141030         ENDIF;
264000150212         V04clv1 = V02clv1;
264100150212         V04clv2 = V02clv2;
264200150212         V04clv3 = V02clv3;
264300150212         V04clv4 = V02clv4;
264400150212         V04clv5 = V02clv5;
264500150212         V04deldas = V02deldas;
264600150212         V04delda = V02delda;
264700150212         V04delas = V02delas;
264800150212         V04dela = V02dela;
264900150212         V04cli = V02cli;
265000150212         V04ncli = V02ncli;
265100150212         V04ttr = V02ttr;
265200150212         V04obj = V02obj;
265300150212         V04obj1 = V02obj1;
265400150212         V04obj2 = V02obj2;
265500150212         V04formula = V02formula;
265600150213         V04obj3 = V02obj3;
265700150213         V04obj3das = V02obj3das;
265800150213         V04obj3da = V02obj3da;
265900150213         V04obj3as = V02obj3as;
266000150213         V04obj3a = V02obj3a;
266100150223
266200150223       //?Se richiamato chiudo i file per il pgm TRKC76R
266300150223         IF  wTRKC76;
266400150223           clear TRKC76DS;
266500150223           IKC76ric = 'C';
266600150223           trkc76r (kpjba:TRKC76DS);
266700150223         ENDIF;
266800141030
266900141030       ENDSR;
267000141030
267100141030       //--------------------------------------------------------------
267200141030       //?Controllo Videata D04.
267300141030       //--------------------------------------------------------------
267400141030       BEGSR ContrD04;
267500141030
267600141030         WindDspF = IndDspF;
267700141030         reset WindDspFb;
267800141030         IndDspF  = WindDspF;
267900141119
268000141119         clear wdelda;
268100141119         clear wdela;
268200150212
268300150212       //?Campagna Commerciale Obbligatoria
268400150212         clear V04des;
268500150305         clear VH4ncm;
268600150212         IF  V04ncm = *blanks;
268700150212           ErrMessage  = *on;
268800150212           ErrGenerico = *on;
268900150212           PosCurNCM   = *on;
269000150212           V04msg = Msg(02);
269100150212           leavesr;
269200150212         ENDIF;
269300150212       //?Ricerca Campagna Commerciale
269400150212         IF  %scan('?':V04ncm) > 0;
269500150212           clear TRKC02DS;
269600150212           IKC02ric = 'R';
269700150212           trkc02r (kpjba:trkc02ds);
269800150212           ErrGenerico = *on;
269900150212           PosCurNCM   = *on;
270000150212           V04ncm = %editc(OKC02ncm:'X');
270100150212         ENDIF;
270200150212       //?Accetto solo dati numerici
270300150212         xx = 1;
270400150212         FOR xx by 1 to %len(V04ncm);
270500150212           IF  %subst(V04ncm:xx:1) <> *blanks and
270600150212               %check(Digits:%subst(V04ncm:xx:1)) > *zeros;
270700150212             ErrMessage  = *on;
270800150212             ErrGenerico = *on;
270900150212             PosCurNCM   = *on;
271000150212             V04msg = Msg(02);
271100150212             leavesr;
271200150212           ENDIF;
271300150212         ENDFOR;
271400150212       //?Deve essere una Campagna Commerciale valida
271500150212         CMPncm = %dec(V04ncm:7:0);
271600150212         chain CMPncm TICMP01L;
271700150212         IF  not %found(TICMP01L);
271800150212           ErrMessage  = *on;
271900150212           ErrGenerico = *on;
272000150212           PosCurNCM   = *on;
272100150212           V04msg = Msg(02);
272200150212           leavesr;
272300150212         ENDIF;
272400150212       //?Decodifico Campagna Commerciale
272500150212         V04des = CMPdes;
272600150505       ////?Campagna Commerciale già finita
272700150505         //IF  CMPdfc < Oggi;
272800150505         //  ErrMessage  = *on;
272900150505         //  ErrGenerico = *on;
273000150505         //  PosCurNCM   = *on;
273100150505         //  V04msg = %trim(Msg(02)) + '. Già chiusa';
273200150505         //  leavesr;
273300150505         //ENDIF;
273400150212       //?In carico all'Utente
273500150212         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
273600150212           ErrMessage  = *on;
273700150212           ErrGenerico = *on;
273800150212           PosCurNCM   = *on;
273900150212           V04msg = %trim(Msg(02)) + '. Non in gestione';
274000150212           leavesr;
274100150212         ENDIF;
274200150212         VH4ncm = %dec(V04ncm:7:0);
274300141030
274400141030       //?Distretto
274500141030         w001a = V04cdi;
274600141030         clear w050a;
274700141030         exsr Distretto;
274800141030         V04cdi = w001a;
274900141030         v04cdid = w050a;
275000141030         V04msg = wmsg;
275100141030         IF  Errgenerico;
275200141030           leavesr;
275300141030         ENDIF;
275400141030
275500141030       //?Area
275600150305         clear VH4car;
275700141030         w003a = V04car;
275800141030         clear w050a;
275900141030         exsr Area;
276000141030         V04car = w003a;
276100141030         V04card = w050a;
276200141030         V04msg = wmsg;
276300141030         IF  ErrGenerico;
276400141030           leavesr;
276500141030         ENDIF;
276600141030       //?Se presente anche il distretto devono essere congruenti
276700141030         IF  V04cdi <> *blanks and V04car <> *blanks;
276800150219           chain (V04cdi:w0030) AZORG02L;
276900141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
277000141030             ErrGenerico = *on;
277100141030             ErrMessage  = *on;
277200141030             PosCurCAR   = *on;
277300141030             V04msg = %trim(Msg(04)) + '. Non congruente con il distretto';
277400141030             leavesr;
277500141030           ENDIF;
277600141030         ENDIF;
277700150212         IF  V04car <> *blanks;
277800150212           VH4car = %dec(V04car:3:0);
277900150212         ENDIF;
278000141030
278100141030       //?Filiale Commerciale
278200150305         clear VH4fil;
278300141030         w003a = V04fil;
278400141030         clear w050a;
278500141030         exsr Filiale;
278600141030         V04fil = w003a;
278700141030         V04fild = w050a;
278800141030         V04msg = wmsg;
278900141030         IF  ErrGenerico;
279000141030           leavesr;
279100141030         ENDIF;
279200150212         IF  V04fil <> *blanks;
279300150212           VH4fil = %dec(V04fil:3:0);
279400150212         ENDIF;
279500141030
279600141030       //?Commerciale Unificante
279700150305         clear VH4cmm;
279800141030         w007a = V04cmm;
279900141030         clear w050a;
280000141030         exsr Commerciale;
280100141030         V04cmm = w007a;
280200141030         V04cmmd = w050a;
280300141103         V04msg = wmsg;
280400141030         IF  ErrGenerico;
280500141030           leavesr;
280600141030         ENDIF;
280700150212         IF  V04cmm <> *blanks;
280800150212           VH4cmm = %dec(V04cmm:7:0);
280900150212         ENDIF;
281000141030
281100141030       //?Cliente
281200150305         clear VH4ksu;
281300141107         w007a = V04ksu;
281400141030         clear w050a;
281500141030         exsr Cliente;
281600141107         V04ksu = w007a;
281700141030         V04rag = w050a;
281800141030         V04msg = wmsg;
281900141030         IF  ErrGenerico;
282000141030           leavesr;
282100141030         ENDIF;
282200150212         IF  V04ksu <> *blanks;
282300150212           VH4ksu = %dec(V04ksu:7:0);
282400150212         ENDIF;
282500150611
282600150611       //?Se utente NON di sede a abilitato a più di un'area e no
282700150611       //?abilitazione 'AZ' o 'DI'
282800150611       //?richiedo obbligatoria l'area
282900150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
283000150611             V04car = *blanks and V04cmm = *blanks and
283100150611             V04ksu = *blanks and car(2) > *zeros;
283200150611           ErrMessage  = *on;
283300150611           ErrGenerico = *on;
283400150611           PosCurCAR   = *on;
283500150611           V04msg = Msg(25);
283600150611           leavesr;
283700150611         ENDIF;
283800141030
283900141030       //?Importanza Cliente
284000141030         w001a = V04clv1;
284100141030         clear w050a;
284200141030         exsr ImpCliente;
284300150219         V04clv1 = w001a;
284400141103         V04msg = wmsg;
284500141030         IF  ErrGenerico;
284600141030           PosCurCLV1 = *on;
284700141030           leavesr;
284800141030         ENDIF;
284900141030         w001a = V04clv2;
285000141030         clear w050a;
285100141030         exsr ImpCliente;
285200141030         V04clv2 = w001a;
285300141030         V04msg = wmsg;
285400141030         IF  ErrGenerico;
285500141030           PosCurCLV2 = *on;
285600141030           leavesr;
285700141030         ENDIF;
285800141030         w001a = V04clv3;
285900141030         clear w050a;
286000141030         exsr ImpCliente;
286100141030         V04clv3 = w001a;
286200141030         V04msg = wmsg;
286300141030         IF  ErrGenerico;
286400141030           PosCurCLV3 = *on;
286500141030           leavesr;
286600141030         ENDIF;
286700141030         w001a = V04clv4;
286800141030         clear w050a;
286900141030         exsr ImpCliente;
287000141030         V04clv4 = w001a;
287100141030         V04msg = wmsg;
287200141030         IF  ErrGenerico;
287300141030           PosCurCLV4 = *on;
287400141030           leavesr;
287500141030         ENDIF;
287600141030         w001a = V04clv5;
287700141030         clear w050a;
287800141030         exsr ImpCliente;
287900141030         V04clv5 = w001a;
288000141030         V04msg = wmsg;
288100141030         IF  ErrGenerico;
288200141030           PosCurCLV5 = *on;
288300141030           leavesr;
288400141030         ENDIF;
288500141030
288600141030       //?Delta
288700141030         IF  V04delda > *zeros or V04dela <> *zeros;
288800141030         //?Obbligatori tutti e 2 se immesso uno dei due
288900141030           IF  V04delda > *zeros and V04dela = *zeros;
289000141030             ErrMessage  = *on;
289100141030             ErrGenerico = *on;
289200141030             PosCurDELA  = *on;
289300150216             V04msg      = Msg(09);
289400141030             leavesr;
289500141030           ENDIF;
289600141030           IF  V04delda = *zeros and V04dela > *zeros;
289700141030             ErrMessage  = *on;
289800141030             ErrGenerico = *on;
289900141030             PosCurDELda = *on;
290000150216             V04msg      = Msg(09);
290100141030             leavesr;
290200141030           ENDIF;
290300141030         //?Controllo congruenza tra "DAL" e "AL"
290400141104           wdelda = V04delda;
290500141104           wdela  = V04dela;
290600141030           IF  V04deldas = '-';
290700141030             wdelda = wdelda * -1;
290800141030           ENDIF;
290900141030           IF  V04delas = '-';
291000141030             wdela = wdela * -1;
291100141030           ENDIF;
291200141030           IF  wdelda > wdela;
291300141030             ErrMessage  = *on;
291400141030             ErrGenerico = *on;
291500141030             PosCurDELda = *on;
291600150216             V04msg      = Msg(10);
291700141030             leavesr;
291800141030           ENDIF;
291900141030         ENDIF;
292000141121
292100141121       //?Obiettivi
292200141127         wfobj  = V04obj;
292300141127         wfobj1 = V04obj1;
292400141127         wfobj2 = V04obj2;
292500150119         wfformula = V04formula;
292600150213         wfobj3 = V04obj3;
292700150213         wobj3da = V04obj3da;
292800150213         wobj3a  = V04obj3a;
292900150219         IF  V04obj3das = '-';
293000150213           wobj3da = wobj3da * -1;
293100150213         ENDIF;
293200150219         IF  V04obj3as = '-';
293300150213           wobj3a = wobj3a * -1;
293400150213         ENDIF;
293500150128         exsr ContrObj;
293600141127         V04obj1 = wfobj1;
293700141127         V04obj2 = wfobj2;
293800150119         V04formula = wfformula;
293900150213         V04obj3 = wfobj3;
294000150213         V04obj3da = %abs(wobj3da);
294100150213         V04obj3a  = %abs(wobj3a);
294200141127         V04msg = wmsg;
294300141127         IF  ErrGenerico;
294400141127           leavesr;
294500141127         ENDIF;
294600150302
294700150302       //?Trattative
294800150302         w002a = V04ttr;
294900150302         clear w050a;
295000150302         exsr SceltaTtr;
295100150302         V04ttr = w002a;
295200150302         V04msg = wmsg;
295300150302         IF  ErrGenerico;
295400150302           leavesr;
295500150302         ENDIF;
295600141030
295700141030       ENDSR;
295800150119
295900150119       //--------------------------------------------------------------
296000150119       //?Gestione tasto funzionale F01 da videata D04
296100150119       //?F01=Formula
296200150119       //--------------------------------------------------------------
296300150119       BEGSR F01D04;
296400150119
296500150119         savformula = V04formula;
296600150119         Video = 'S6';
296700150119         wInzS06 = *on;
296800150119
296900150119       ENDSR;
297000150128
297100150128       //--------------------------------------------------------------
297200150128       //?Gestione tasto funzionale F02 da videata D04
297300150128       //?F02=Obiettivo/Andamento
297400150128       //--------------------------------------------------------------
297500150128       BEGSR F02D04;
297600150216
297700150216         IF  H2nmfl <> 'V04OBJ1' and H2nmfl <> 'V04OBJ2' and
297800150216             H2nmfl <> 'V04OBJ3';
297900150216           ErrMessage  = *on;
298000150216           ErrGenerico = *on;
298100150216           PosCurOBJ1  = *on;
298200150216           V04msg = Msg(22);
298300150216           leavesr;
298400150216         ENDIF;
298500150128
298600150128         Video = 'S8';
298700150128         wInzS08 = *on;
298800150128
298900150128       ENDSR;
299000150302
299100150302       //--------------------------------------------------------------
299200150302       //?Gestione tasto funzionale F04 da videata D04
299300150302       //?F04=Scelta Trattative
299400150302       //--------------------------------------------------------------
299500150302       BEGSR F04D04;
299600150302
299700150302         Video = 'S9';
299800150302         wInzS09 = *on;
299900150302
300000150302       ENDSR;
300100141103
300200141103       //--------------------------------------------------------------
300300141103       //?Gestione tasto funzionale F06 da videata D04
300400141103       //?F06=Conferma
300500141103       //--------------------------------------------------------------
300600141103       BEGSR F06D04;
300700141103
300800141103       //?Devo ricaricare il subfile con le nuove parzializzazioni
300900141103         Video = 'S3';
301000141112         wInzS03 = *on;
301100141103
301200141103       ENDSR;
301300141030
301400141030       //--------------------------------------------------------------
301500141030       //?Gestione tasto funzionale F12 da videata D04
301600141030       //?F12=Ritorno
301700141030       //--------------------------------------------------------------
301800141030       BEGSR F12D04;
301900150116
302000150116       //?Se arrivo qua dalla videata con messaggio di troppi rcd
302100150116       //?devo ricaricare il subfile
302200150116         IF  TroppiRcd;
302300150116           wInzs03 = *on;
302400150116         ENDIF;
302500141030
302600141030       //?Ritorno alle selezioni
302700141030         Video = 'S3';
302800141104       //?Forzo il posizionamento del cursore al 1° rcd del subfile
302900141104         clear V03rcd;
303000141030
303100141030       ENDSR;
303200141118
303300141118       //--------------------------------------------------------------
303400141118       //?Gestione videata D05.
303500141118       //--------------------------------------------------------------
303600141118       BEGSR GesD05;
303700150116
303800150116         TroppiRcd = *on;
303900141118
304000141118       //?Inizializzazione videata
304100141118         IF  wInzD05;
304200141118           exsr InzD05;
304300141118           wInzD05 = *off;
304400141118         ENDIF;
304500141118
304600141118       //?Emissione videata
304700150112         write  KC01T01;
304800150112         exfmt  KC01D05;
304900141118         reset ErrMessage;
305000141118         reset ErrGenerico;
305100141118
305200141118         SELECT;
305300141118
305400141118       //?- F03=Fine
305500141118           WHEN  dsp_aid = c_F03;
305600141118             exsr F03D02;
305700141118
305800141118       //?- F05=Altre parzializzazioni
305900141118           WHEN  dsp_aid = c_F05;
306000141118             exsr F05S03;
306100141118
306200141118       //?- F12=Ritorno
306300141118           WHEN  dsp_aid = c_F12;
306400141118             exsr F12S03;
306500141118
306600141118         ENDSL;
306700141118
306800141118       ENDSR;
306900141118
307000141118       //--------------------------------------------------------------
307100141118       //?Inizializzazione Videata D05.
307200141118       //--------------------------------------------------------------
307300141118       BEGSR InzD05;
307400141118
307500141118       ENDSR;
307600150116
307700150116       //--------------------------------------------------------------
307800150116       //?Gestione videata S06.
307900150116       //--------------------------------------------------------------
308000150116       BEGSR GesS06;
308100150116
308200150116       //?Inizializzazione videata
308300150116         IF  wInzS06;
308400150116           exsr InzS06;
308500150116           wInzS06 = *off;
308600150116         ENDIF;
308700150119
308800150119       //?Visualizzazione del SFL
308900150119         SflDsp = (S06nrr <= *zeros);
309000150116
309100150116       //?Emissione videata
309200150119         exfmt  KC01CW6;
309300150116         reset ErrMessage;
309400150116         reset ErrGenerico;
309500150116
309600150116       //?- Enter Controllo ed esco
309700150116         exsr OpzS06;
309800150116         IF  ErrGenerico;
309900150116           leavesr;
310000150116         ENDIF;
310100150116
310200150116      //?Videata sucessiva
310300150216         IF H2nmrc = 'KC01D02';
310400150216           Video = 'D2';
310500150216           V02formula = savformula;
310600150216         ENDIF;
310700150216         IF H2nmrc = 'KC01W04';
310800150216           Video = 'D4';
310900150216           V04formula = savformula;
311000150216         ENDIF;
311100150116
311200150116       ENDSR;
311300150116
311400150116       //--------------------------------------------------------------
311500150116       //?Inizializzazione Videata S06.
311600150116       //--------------------------------------------------------------
311700150116       BEGSR InzS06;
311800150116
311900150116       //?Pulizia subfile
312000150116         exsr PulS06;
312100150116
312200150116       //?Caricamento subfile
312300150116         exsr Ries06;
312400150116
312500150116         SflEnd = *on;
312600150116
312700150116       //?Imposto il posizionamento al primo rcd
312800150116         IF  S06nrr > 0;
312900150119           V06rcd = 1;
313000150116         ELSE;
313100150119           clear V06rcd;
313200150116         ENDIF;
313300150116
313400150119         V06csr = V06rcd;
313500150116
313600150116       ENDSR;
313700150116
313800150116       //--------------------------------------------------------------
313900150116       //?Pulizia Subfile S06.
314000150116       //--------------------------------------------------------------
314100150116       BEGSR PulS06;
314200150116
314300150116       //?Pulizia subfile
314400150116         SflDsp    = *on;
314500150116         SflDspCtl = *on;
314600150116         write KC01CW6;
314700150116         SflDspCtl = *off;
314800150116         SflEnd    = *off;
314900150116
315000150119         clear V06rcd;
315100150119         clear V06csr;
315200150116         clear S06nrr;
315300150119         clear V06msg;
315400150116
315500150116         ErrMessage  = *off;
315600150116         ErrGenerico = *off;
315700150116
315800150116         WindDspF = IndDspF;
315900150116         reset WindDspFb;
316000150116         IndDspF  = WindDspF;
316100150116
316200150116       ENDSR;
316300150116
316400150116       //--------------------------------------------------------------
316500150116       //?Caricamento Subfile S06.
316600150116       //--------------------------------------------------------------
316700150116       BEGSR RieS06;
316800150116
316900150116         xx = 1;
317000150116         FOR xx by 1 to 6;
317100150116           clear  KC01SW6;
317200150116           PosCurOPZ = *off;
317300150119           V06formula = Formula(xx);
317400150119           V06des = Formula(xx) + DesForm(xx);
317500150116           S06nrr += 1;
317600150116           write  KC01SW6;
317700150116         ENDFOR;
317800150116
317900150116       ENDSR;
318000150116
318100150116       //--------------------------------------------------------------
318200150116       //?Gestione opzioni Subfile S06.
318300150116       //--------------------------------------------------------------
318400150116       BEGSR OpzS06;
318500150116
318600150116         xx = 1;
318700150116         FOR xx by 1 to 6;
318800150119           S06nrr = xx;
318900150119           chain S06nrr KC01SW6;
319000150116           IF  not %found;
319100150116             leave;
319200150116           ENDIF;
319300150116
319400150116           SELECT;
319500150116         //?- Nessuna opzione
319600150119           WHEN  V06opz = *blank;
319700150116
319800150116         //?- 1 = Scelta
319900150119           WHEN  V06opz = '1';
320000150119             savformula = V06formula;
320100150121             PosCurOBJ2 = *on;
320200150116             leave;
320300150116
320400150116           OTHER;
320500150116             ErrMessage  = *on;
320600150116             ErrGenerico = *on;
320700150116             PosCurOPZ   = *on;
320800150216             V06msg = Msg(11);
320900150116             update KC01SW6;
321000150116             leave;
321100150116           ENDSL;
321200150116
321300150116         ENDFOR;
321400150116
321500150116       ENDSR;
321600150121
321700150121       //--------------------------------------------------------------
321800150121       //?Gestione videata D07.
321900150121       //--------------------------------------------------------------
322000150121       BEGSR GesD07;
322100150121
322200150121       //?Inizializzazione videata
322300150121         IF  wInzD07;
322400150121           exsr InzD07;
322500150121           wInzD07 = *off;
322600150121         ENDIF;
322700150121
322800150121       //?Emissione videata
322900150121         exfmt  KC01W07;
323000150121         reset ErrMessage;
323100150121         reset ErrGenerico;
323200150121
323300150121         SELECT;
323400150121
323500150121       //?- F06=Conferma
323600150121           WHEN  dsp_aid = c_F06;
323700150121             exsr F06D07;
323800150121
323900150121         ENDSL;
324000150121
324100150121       ENDSR;
324200150121
324300150121       //--------------------------------------------------------------
324400150121       //?Inizializzazione Videata D07.
324500150121       //--------------------------------------------------------------
324600150121       BEGSR InzD07;
324700150121
324800150121         clear KC01W07;
324900150121         V07sino = 'SI';
325000150121
325100150121       ENDSR;
325200150121
325300150121       //--------------------------------------------------------------
325400150121       //?Gestione tasto funzionale F06 da videata D07
325500150121       //?F06=Conferma.
325600150121       //--------------------------------------------------------------
325700150121       BEGSR F06D07;
325800150121
325900150121         Video = 'S3';
326000150121
326100150121         IF  V07sino = 'NO';
326200150121           leavesr;
326300150121         ENDIF;
326400150121
326500150121         FOR  xx = 1 to rrnlast;
326600150121           chain xx KC01S03;
326700150121         //?Scrivo la fase
326800150121           clear TRKC71DS;
326900150212           IKC71ncm = %dec(V04ncm:7:0);
327000150121           IKC71ksu = VS3ksu;
327100150121           IKC71ksc = VS3ksc;
327200150121           IKC71cpo = VS3cpo;
327300150121           IKC71acm = FaseObjFine;
327400150121           IKC71pea = VS3oprop;
327500150122           IKC71aut = 'A';
327600150128           IKC71dfa = %dec(%date());
327700150128           IKC71hfc = %dec(%time());
327800150121           TRKC71R (kpjba:TRKC71DS);
327900150121         ENDFOR;
328000150121
328100150121       //?Ricarico il subfile
328200150121         wInzS03 = *on;
328300150121
328400150121       ENDSR;
328500150128
328600150128       //--------------------------------------------------------------
328700150128       //?Gestione videata S08.
328800150128       //--------------------------------------------------------------
328900150128       BEGSR GesS08;
329000150128
329100150128       //?Inizializzazione videata
329200150128         IF  wInzS08;
329300150128           exsr InzS08;
329400150128           wInzS08 = *off;
329500150128         ENDIF;
329600150128
329700150128       //?Visualizzazione del SFL
329800150128         SflDsp = (S08nrr <= *zeros);
329900150128
330000150128       //?Emissione videata
330100150128         exfmt  KC01CW8;
330200150128         reset ErrMessage;
330300150128         reset ErrGenerico;
330400150128
330500150128       //?- Enter Controllo ed esco
330600150128         exsr OpzS08;
330700150128         IF  ErrGenerico;
330800150128           leavesr;
330900150128         ENDIF;
331000150128
331100150128      //?Videata sucessiva
331200150216         IF H2nmrc = 'KC01D02';
331300150216           Video = 'D2';
331400150216         ENDIF;
331500150216         IF H2nmrc = 'KC01W04';
331600150216           Video = 'D4';
331700150216         ENDIF;
331800150128
331900150128       ENDSR;
332000150128
332100150128       //--------------------------------------------------------------
332200150128       //?Inizializzazione Videata S08.
332300150128       //--------------------------------------------------------------
332400150128       BEGSR InzS08;
332500150128
332600150128       //?Pulizia subfile
332700150128         exsr PulS08;
332800150128
332900150128       //?Caricamento subfile
333000150128         exsr Ries08;
333100150128
333200150128         SflEnd = *on;
333300150128
333400150128       //?Imposto il posizionamento al primo rcd
333500150128         IF  S08nrr > 0;
333600150128           V08rcd = 1;
333700150128         ELSE;
333800150128           clear V08rcd;
333900150128         ENDIF;
334000150128
334100150128         V08csr = V08rcd;
334200150128
334300150128       ENDSR;
334400150128
334500150128       //--------------------------------------------------------------
334600150128       //?Pulizia Subfile S08.
334700150128       //--------------------------------------------------------------
334800150128       BEGSR PulS08;
334900150128
335000150128       //?Pulizia subfile
335100150128         SflDsp    = *on;
335200150128         SflDspCtl = *on;
335300150128         write KC01CW8;
335400150128         SflDspCtl = *off;
335500150128         SflEnd    = *off;
335600150128
335700150128         clear V08rcd;
335800150128         clear V08csr;
335900150128         clear S08nrr;
336000150128         clear V08msg;
336100150128
336200150128         ErrMessage  = *off;
336300150128         ErrGenerico = *off;
336400150128
336500150128         WindDspF = IndDspF;
336600150128         reset WindDspFb;
336700150128         IndDspF  = WindDspF;
336800150128
336900150128       ENDSR;
337000150128
337100150128       //--------------------------------------------------------------
337200150128       //?Caricamento Subfile S08.
337300150128       //--------------------------------------------------------------
337400150128       BEGSR RieS08;
337500150128
337600150128         xx = 1;
337700150128         FOR xx by 1 to 5;
337800150128           clear  KC01SW8;
337900150128           PosCurOPZ = *off;
338000150128           V08obj = Obiettivo(xx);
338100150128           V08des = Obiettivo(xx) + ' ' + DesObj(xx);
338200150128           S08nrr += 1;
338300150128           write  KC01SW8;
338400150128         ENDFOR;
338500150128
338600150128       ENDSR;
338700150128
338800150128       //--------------------------------------------------------------
338900150128       //?Gestione opzioni Subfile S08.
339000150128       //--------------------------------------------------------------
339100150128       BEGSR OpzS08;
339200150128
339300150128         xx = 1;
339400150128         FOR xx by 1 to 5;
339500150128           S08nrr = xx;
339600150128           chain S08nrr KC01SW8;
339700150128           IF  not %found;
339800150128             leave;
339900150128           ENDIF;
340000150128
340100150128           SELECT;
340200150128         //?- Nessuna opzione
340300150128           WHEN  V08opz = *blank;
340400150128
340500150128         //?- 1 = Scelta
340600150128           WHEN  V08opz = '1';
340700150216             exsr  Scelta08;
340800150216             leave;
340900150128
341000150128           OTHER;
341100150128             ErrMessage  = *on;
341200150128             ErrGenerico = *on;
341300150128             PosCurOPZ   = *on;
341400150216             V08msg = Msg(11);
341500150128             update KC01SW8;
341600150128             leave;
341700150128           ENDSL;
341800150128
341900150128         ENDFOR;
342000150128
342100150128       ENDSR;
342200150216
342300150216       //--------------------------------------------------------------
342400150216       //?Ritorno la scelta fatta da videata D08.
342500150216       //--------------------------------------------------------------
342600150216       BEGSR Scelta08;
342700150216
342800150216         SELECT;
342900150216         WHEN  H2nmfl = 'V02OBJ1';
343000150216           V02obj1 = V08obj;
343100150216           POScurOBJ1 = *on;
343200150216         WHEN  H2nmfl = 'V02OBJ2';
343300150216           V02obj2 = V08obj;
343400150216           POScurOBJ2 = *on;
343500150216         WHEN  H2nmfl = 'V02OBJ3';
343600150216           V02obj3 = V08obj;
343700150216           POScurOBJ3 = *on;
343800150216         WHEN  H2nmfl = 'V04OBJ1';
343900150216           V04obj1 = V08obj;
344000150216           POScurOBJ1 = *on;
344100150216         WHEN  H2nmfl = 'V04OBJ2';
344200150216           V04obj2 = V08obj;
344300150216           POScurOBJ2 = *on;
344400150216         WHEN  H2nmfl = 'V04OBJ3';
344500150216           V04obj3 = V08obj;
344600150216           POScurOBJ3 = *on;
344700150216         ENDSL;
344800150216
344900150216       ENDSR;
345000150302
345100150302       //--------------------------------------------------------------
345200150302       //?Gestione videata S09.
345300150302       //--------------------------------------------------------------
345400150302       BEGSR GesS09;
345500150302
345600150302       //?Inizializzazione videata
345700150302         IF  wInzS09;
345800150302           exsr InzS09;
345900150302           wInzS09 = *off;
346000150302         ENDIF;
346100150302
346200150302       //?Visualizzazione del SFL
346300150302         SflDsp = (S09nrr <= *zeros);
346400150302
346500150302       //?Emissione videata
346600150302         exfmt  KC01CW9;
346700150302         reset ErrMessage;
346800150302         reset ErrGenerico;
346900150302
347000150302       //?- Enter Controllo ed esco
347100150302         exsr OpzS09;
347200150302         IF  ErrGenerico;
347300150302           leavesr;
347400150302         ENDIF;
347500150302
347600150302      //?Videata sucessiva
347700150302         IF H2nmrc = 'KC01D02';
347800150302           Video = 'D2';
347900150302         ENDIF;
348000150302         IF H2nmrc = 'KC01W04';
348100150302           Video = 'D4';
348200150302         ENDIF;
348300150302
348400150302       ENDSR;
348500150302
348600150302       //--------------------------------------------------------------
348700150302       //?Inizializzazione Videata S09.
348800150302       //--------------------------------------------------------------
348900150302       BEGSR InzS09;
349000150302
349100150302       //?Pulizia subfile
349200150302         exsr PulS09;
349300150302
349400150302       //?Caricamento subfile
349500150302         exsr Ries09;
349600150302
349700150302         SflEnd = *on;
349800150302
349900150302       //?Imposto il posizionamento al primo rcd
350000150302         IF  S09nrr > 0;
350100150302           V09rcd = 1;
350200150302         ELSE;
350300150302           clear V09rcd;
350400150302         ENDIF;
350500150302
350600150302         V09csr = V09rcd;
350700150302
350800150302       ENDSR;
350900150302
351000150302       //--------------------------------------------------------------
351100150302       //?Pulizia Subfile S09.
351200150302       //--------------------------------------------------------------
351300150302       BEGSR PulS09;
351400150302
351500150302       //?Pulizia subfile
351600150302         SflDsp    = *on;
351700150302         SflDspCtl = *on;
351800150302         write KC01CW9;
351900150302         SflDspCtl = *off;
352000150302         SflEnd    = *off;
352100150302
352200150302         clear V09rcd;
352300150302         clear V09csr;
352400150302         clear S09nrr;
352500150302         clear V09msg;
352600150302
352700150302         ErrMessage  = *off;
352800150302         ErrGenerico = *off;
352900150302
353000150302         WindDspF = IndDspF;
353100150302         reset WindDspFb;
353200150302         IndDspF  = WindDspF;
353300150302
353400150302       ENDSR;
353500150302
353600150302       //--------------------------------------------------------------
353700150302       //?Caricamento Subfile S09.
353800150302       //--------------------------------------------------------------
353900150302       BEGSR RieS09;
354000150302
354100150302         xx = 1;
354200150302         FOR xx by 1 to 10;
354300150302           clear  KC01SW9;
354400150302           PosCurOPZ = *off;
354500150302           V09ttr = Trattative(xx);
354600150302           V09des = Trattative(xx) + ' ' + DesTtr(xx);
354700150302           S09nrr += 1;
354800150302           write  KC01SW9;
354900150302         ENDFOR;
355000150302
355100150302       ENDSR;
355200150302
355300150302       //--------------------------------------------------------------
355400150302       //?Gestione opzioni Subfile S09.
355500150302       //--------------------------------------------------------------
355600150302       BEGSR OpzS09;
355700150302
355800150302         xx = 1;
355900150302         FOR xx by 1 to 10;
356000150302           S09nrr = xx;
356100150302           chain S09nrr KC01SW9;
356200150302           IF  not %found;
356300150302             leave;
356400150302           ENDIF;
356500150302
356600150302           SELECT;
356700150302         //?- Nessuna opzione
356800150302           WHEN  V09opz = *blank;
356900150302
357000150302         //?- 1 = Scelta
357100150302           WHEN  V09opz = '1';
357200150302             exsr  Scelta09;
357300150302             leave;
357400150302
357500150302           OTHER;
357600150302             ErrMessage  = *on;
357700150302             ErrGenerico = *on;
357800150302             PosCurOPZ   = *on;
357900150302             V09msg = Msg(11);
358000150302             update KC01SW9;
358100150302             leave;
358200150302           ENDSL;
358300150302
358400150302         ENDFOR;
358500150302
358600150302       ENDSR;
358700150302
358800150302       //--------------------------------------------------------------
358900150302       //?Ritorno la scelta fatta da videata D09.
359000150302       //--------------------------------------------------------------
359100150302       BEGSR Scelta09;
359200150302
359300150304         IF H2nmrc = 'KC01D02';
359400150304           V02ttr = V09ttr;
359500150304         ENDIF;
359600150304         IF H2nmrc = 'KC01W04';
359700150304           V04ttr = V09ttr;
359800150304         ENDIF;
359900150302         POScurTTR = *on;
360000150302
360100150302       ENDSR;
360200141030
360300141030       //--------------------------------------------------------------
360400141030       //?Controllo Distretto.
360500141030       //--------------------------------------------------------------
360600141030       BEGSR Distretto;
360700141030
360800141030         IF  w001a = *blanks;
360900141030           leavesr;
361000141030         ENDIF;
361100141030
361200141030       //?Ricerca Distretto
361300141030         IF  %scan('?' : w001a) > 0;
361400141030           ErrGenerico = *on;
361500141030           PosCurCDI   = *on;
361600141030           idTabella = '17';
361700141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
361800141030                            tastoUscita);
361900141030           w001a = idElemento;
362000141030         ENDIF;
362100141030       //?Deve essere un Distretto valido
362200141030         clear xx;
362300141030         xx = %lookup(w001a : skDistretti);
362400141030         IF  xx = 0;
362500141030           ErrMessage  = *on;
362600141030           ErrGenerico = *on;
362700141030           PosCurCDI   = *on;
362800141030           wmsg = Msg(03);
362900141030           leavesr;
363000141030         ENDIF;
363100141030       //?Decodifico distretto
363200141030         w050a = skDesDist(xx);
363300141030       //?In carico all'Utente
363400141030         IF  %lookup (w001a:DIS) = 0;
363500141030           ErrMessage  = *on;
363600141030           ErrGenerico = *on;
363700141030           PosCurCDI   = *on;
363800141030           wmsg = %trim(Msg(03)) + '. Non in gestione';
363900141030           leavesr;
364000141030         ENDIF;
364100141030
364200141030       ENDSR;
364300141030
364400141030       //--------------------------------------------------------------
364500141030       //?Controllo Area.
364600141030       //--------------------------------------------------------------
364700141030       BEGSR Area;
364800141030
364900141030         IF  w003a = *blanks;
365000141030           leavesr;
365100141030         ENDIF;
365200141030
365300141030       //?Ricerca Area
365400141030         IF  %scan('?' : w003a) > 0;
365500141030           ErrGenerico = *on;
365600141030           PosCurCAR   = *on;
365700141104           idTabella = '05';
365800141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
365900141030                          tastoUscita);
366000141030           w003a = idElemento;
366100141030         ENDIF;
366200141104         IF  w003a = *blanks;
366300141104           leavesr;
366400141104         ENDIF;
366500141030       //?Accetto solo dati numerici
366600141030         xx = 1;
366700141030         FOR xx by 1 to %len(w003a);
366800141030           IF  %subst(w003a:xx:1) <> *blanks and
366900141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
367000141030             ErrMessage  = *on;
367100141030             ErrGenerico = *on;
367200141030             PosCurCAR   = *on;
367300141030             wmsg = Msg(04);
367400141030             leavesr;
367500141030           ENDIF;
367600141030         ENDFOR;
367700141030       //?Deve essere un'Area valida
367800141030         clear xx;
367900141030         w0030 = %dec(w003a:3:0);
368000141030         xx = %lookup(%editc(w0030:'X'):skAree);
368100141030         IF  xx = 0;
368200141030           ErrMessage  = *on;
368300141030           ErrGenerico = *on;
368400141030           PosCurCAR   = *on;
368500141030           wmsg = Msg(04);
368600141030           leavesr;
368700141030         ENDIF;
368800141030       //?Decodifico area
368900141030         w050a = skDesArea(xx);
369000141030       //?Deve essere un'area in carico all'utente
369100141030         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
369200141030           ErrGenerico = *on;
369300141030           ErrMessage  = *on;
369400141030           PosCurCAR   = *on;
369500141030           wmsg = %trim(Msg(04)) + '. Non in gestione';
369600141030           leavesr;
369700141030         ENDIF;
369800141030
369900141030       ENDSR;
370000141030
370100141030       //--------------------------------------------------------------
370200141030       //?Controllo Filiale.
370300141030       //--------------------------------------------------------------
370400141030       BEGSR Filiale;
370500141030
370600141030         IF  w003a = *blanks;
370700141030           leavesr;
370800141030         ENDIF;
370900141030
371000141030       //?Ricerca Filale
371100141030         IF  %scan('?' : w003a) > 0;
371200141030           clear pinFIL;
371300141030           clear pinFAG;
371400141030           clear pinDES;
371500141030           clear pinDIT;
371600141030           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
371700141030           IF pinFIL > *zeros;
371800141030             w003a = pinFIL;
371900141030             w050a = pinDES;
372000141030           ENDIF;
372100141030           ErrGenerico = *on;
372200141030           PosCurFIL   = *on;
372300141030         ENDIF;
372400141030       //?Accetto solo dati numerici
372500141030         xx = 1;
372600141030         FOR xx by 1 to %len(w003a);
372700141030           IF  %subst(w003a:xx:1) <> *blanks and
372800141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
372900141030             ErrMessage  = *on;
373000141030             ErrGenerico = *on;
373100141030             PosCurFIL   = *on;
373200141030             wmsg = Msg(05);
373300141030             leavesr;
373400141030           ENDIF;
373500141030         ENDFOR;
373600141030         w0030 = %dec(w003a:3:0);
373700141030       //?Deve esistere la Filiale
373800141030         chain w0030 AZORG01L;
373900141030         IF  not %found(AZORG01L) or ORGfva <> *blanks;
374000141030           ErrMessage  = *on;
374100141030           ErrGenerico = *on;
374200141030           PosCurFIL   = *on;
374300141030           wmsg = Msg(05);
374400141030           leavesr;
374500141030         ENDIF;
374600141030       //?Decodifico filiale
374700141030         w050a = ORGdes;
374800141030       //?Deve essere una filiale in carico all'utente
374900141030         IF  %lookup(%editc(w0030:'X'):POG) = 0;
375000141030           ErrGenerico = *on;
375100141030           ErrMessage  = *on;
375200141030           PosCurFIL   = *on;
375300141030           wmsg = %trim(Msg(05)) + '. Non in gestione';
375400141030           leavesr;
375500141030         ENDIF;
375600141104       //?Se presente anche il distretto o l'area devono essere congruenti
375700141104         IF  V02cdi <> *blanks and V02fil <> *blanks;
375800141105           IF  V02cdi <> ORGfl3;
375900141104             ErrGenerico = *on;
376000141104             ErrMessage  = *on;
376100141104             PosCurFIL   = *on;
376200141104             wmsg = %trim(Msg(05)) + '. Non congruente con il distretto';
376300141104             leavesr;
376400141104           ENDIF;
376500141104           IF  V02car <> %editc(ORGcar:'X');
376600141104             ErrGenerico = *on;
376700141104             ErrMessage  = *on;
376800141104             PosCurFIL   = *on;
376900141104             wmsg = %trim(Msg(05)) + '. Non congruente con l''area';
377000141104             leavesr;
377100141104           ENDIF;
377200141104         ENDIF;
377300141030
377400141030       ENDSR;
377500141030
377600141030       //--------------------------------------------------------------
377700141030       //?Controllo Commerciale.
377800141030       //--------------------------------------------------------------
377900141030       BEGSR Commerciale;
378000141030
378100141030         IF  w007a = *blanks;
378200141030           leavesr;
378300141030         ENDIF;
378400141030
378500141030       //?Ricerca Commerciale
378600141030         IF  %scan('?' : w007a) > 0;
378700141030           clear TRMK43DS;
378800141030           IMK43solu = 'S';
378900141030           IMK43fil = DUTpou;
379000141030           trmk43r (kpjba : TRMK43DS);
379100141030           IF  OMK43err = *off and OMK43fxx = *blanks;
379200141030             w007a = %editc(OMK43cmm:'X');
379300141030             w050a = OMK43des;
379400141030           ENDIF;
379500141030           ErrGenerico = *on;
379600141030           PosCurCMM   = *on;
379700141030         ENDIF;
379800141030       //?Accetto solo dati numerici
379900141030         xx = 1;
380000141030         FOR xx by 1 to %len(w007a);
380100141030           IF  %subst(w007a:xx:1) <> *blanks and
380200141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
380300141030             ErrMessage  = *on;
380400141030             ErrGenerico = *on;
380500141030             PosCurCMM   = *on;
380600150216             wmsg = Msg(06);
380700141030             leavesr;
380800141030           ENDIF;
380900141030         ENDFOR;
381000141030         w0070 = %dec(w007a:7:0);
381100141030       //?Deve esistere il Commerciale
381200141030         chain (w0070) AZCMM01L;
381300141030         IF  not %found(AZCMM01L);
381400141030           ErrMessage  = *on;
381500141030           ErrGenerico = *on;
381600141030           PosCurCMM   = *on;
381700150216           wmsg = Msg(06);
381800141030           leavesr;
381900141030         ENDIF;
382000141030       //?Decodifico Commerciale
382100141030         w050a = CMMdes;
382200141030       //?Deve essere un Commerciale in carico all'utente
382300141030         clear TNTAA1DS;
382400141119         ITAA1caut = '§utegtc';
382500141030         ITAA1cmm  = w0070;
382600141030         kpjbu = TNTAA1DS;
382700141030         tntaa1c (kpjba);
382800150217         TNTAA1DS = kpjbu;
382900141030         IF  OTAA1fabi = 'N';
383000141030           ErrGenerico = *on;
383100141030           ErrMessage  = *on;
383200141030           PosCurCMM   = *on;
383300150216           wmsg = %trim(Msg(06)) + '. Non in gestione';
383400141030           leavesr;
383500141030         ENDIF;
383600141030       //?Deve essere un Commerciale Unificante
383700141030         IF  w0070 <> CMMuni;
383800141030           ErrGenerico = *on;
383900141030           ErrMessage  = *on;
384000141030           PosCurCMM   = *on;
384100150216           wmsg = %trim(Msg(06)) + '. Non è un Commerciale Unificante';
384200141030           leavesr;
384300141030         ENDIF;
384400141030
384500141030       ENDSR;
384600141030
384700141030       //--------------------------------------------------------------
384800141030       //?Controllo Cliente.
384900141030       //--------------------------------------------------------------
385000141030       BEGSR Cliente;
385100141030
385200141030         IF  w007a = *blanks;
385300141030           leavesr;
385400141030         ENDIF;
385500141030
385600141030       //?Ricerca Cliente
385700141030         IF  %scan('?' : w007a) > 0;
385800141030           clear TNTAI1DS;
385900141030           ITAI1ric = 'S';
386000141030           ITAI1abi = wabi;
386100141112           ITAI1aut = '*C';
386200141030           tntai1r (kpjba : TNTAI1DS);
386300141030           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
386400141030               OTAI1err <> *blanks;
386500141107             clear V02ksu;
386600141030           ELSE;
386700141030             w007a = %editc(OTAI1ksc:'X');
386800141030           ENDIF;
386900141030           ErrGenerico = *on;
387000141107           PosCurKSU   = *on;
387100141030         ENDIF;
387200141030       //?Accetto solo dati numerici
387300141030         xx = 1;
387400141030         FOR xx by 1 to %len(w007a);
387500141030           IF  %subst(w007a:xx:1) <> *blanks and
387600141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
387700141030             ErrMessage  = *on;
387800141030             ErrGenerico = *on;
387900141107             PosCurKSU   = *on;
388000150216             wmsg = Msg(07);
388100141030             leavesr;
388200141030           ENDIF;
388300141030         ENDFOR;
388400141030         w0070 = %dec(w007a:7:0);
388500141030       //?Deve esistere il Cliente
388600141030         clear TIBS69DS;
388700141030         clear ACOrag;
388800141030         I69kac = %dec(w007a:7:0);
388900141030         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
389000141030         IF  O69err <> *blanks;
389100141030           ErrMessage  = *on;
389200141030           ErrGenerico = *on;
389300141107           PosCurKSU   = *on;
389400150216           wmsg      = Msg(07);
389500141030         ENDIF;
389600141030       //?Decodifico Cliente
389700141030         w050a = ACOrag;
389800141030       //?Deve essere un Cliente in carico all'utente
389900141030         clear TNTAA1DS;
390000141119         ITAA1caut = '§utegtc';
390100141030         ITAA1ksc  = %dec(w007a:7:0);
390200141030         kpjbu = TNTAA1DS;
390300141030         tntaa1c (kpjba);
390400150217         TNTAA1DS = kpjbu;
390500141030         IF  OTAA1fabi = 'N';
390600141030           ErrGenerico = *on;
390700141030           ErrMessage  = *on;
390800141107           PosCurKSU   = *on;
390900150216           wmsg = %trim(Msg(07)) + '. Non in gestione';
391000141030           leavesr;
391100141030         ENDIF;
391200141030
391300141030       ENDSR;
391400141030
391500141030       //--------------------------------------------------------------
391600141030       //?Controllo Importanza Cliente.
391700141030       //--------------------------------------------------------------
391800141030       BEGSR ImpCliente;
391900141030
392000141030         IF  w001a = *blanks;
392100141030           leavesr;
392200141030         ENDIF;
392300141030
392400141030         IF  %scan('?' : w001a) > 0;
392500141030           ErrGenerico = *on;
392600141030           idTabella = 'IC';
392700141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
392800141030                          tastoUscita);
392900141030           w001a = idElemento;
393000141030         ENDIF;
393100141030         IF  %lookup(w001a : skIC) = 0;
393200141030           ErrMessage  = *on;
393300141030           ErrGenerico = *on;
393400141030           PosCurCLV1  = *on;
393500150216           wmsg = Msg(08);
393600141030           leavesr;
393700141030         ENDIF;
393800141030
393900141030       ENDSR;
394000141127
394100141127       //--------------------------------------------------------------
394200141127       //?Controllo Obiettivi.
394300141127       //--------------------------------------------------------------
394400150128       BEGSR ContrObj;
394500141127
394600150128       //?Se Obiettivo inizio NO "di cui"
394700150119         IF  wfobj = 'I' and
394800150119            (wfobj1 = 'P' or wfobj1 = 'F' or
394900150119             wfobj2 = 'P' or wfobj2 = 'F');
395000141127           ErrMessage  = *on;
395100141127           ErrGenerico = *on;
395200150216           wmsg = Msg(14);
395300141127           IF  wfobj1 <> *blanks;
395400141127             PosCurOBJ1  = *on;
395500141127             leavesr;
395600141127           ENDIF;
395700141127           IF  wfobj2 <> *blanks;
395800141127             PosCurOBJ2  = *on;
395900141127             leavesr;
396000141127           ENDIF;
396100141127         ENDIF;
396200141127       //?Se Obiettivo proposto NO "di cui" Finale
396300141127         IF  wfobj = 'P' and (wfobj1 = 'F' or wfobj2 = 'F');
396400141127           ErrMessage  = *on;
396500141127           ErrGenerico = *on;
396600150216           wmsg = Msg(15);
396700141127           IF  wfobj1 = 'F';
396800141127             PosCurOBJ1  = *on;
396900141127             leavesr;
397000141127           ENDIF;
397100141127           IF  wfobj2 = 'F';
397200141127             PosCurOBJ2  = *on;
397300141127             leavesr;
397400141127           ENDIF;
397500141127         ENDIF;
397600150128       //?Deve essere un obiettivo valido
397700150128         IF  wfobj1 <> *blanks and %lookup (wfobj1:Obiettivo) = 0;
397800150128           ErrMessage  = *on;
397900150128           ErrGenerico = *on;
398000150128           PosCurOBJ1  = *on;
398100150216           wmsg = Msg(13);
398200150128           leavesr;
398300150128         ENDIF;
398400150128         IF  wfobj2 <> *blanks and %lookup (wfobj2:Obiettivo) = 0;
398500150128           ErrMessage  = *on;
398600150128           ErrGenerico = *on;
398700150128           PosCurOBJ2  = *on;
398800150216           wmsg = Msg(13);
398900150128           leavesr;
399000150128         ENDIF;
399100150128
399200141127       //?Non posso accettare 2 obiettivi uguali
399300141127         IF  wfobj1 = wfobj2 and (wfobj1 <> *blanks or
399400141127                                  wfobj2 <> *blanks);
399500141127           ErrMessage  = *on;
399600141127           ErrGenerico = *on;
399700141127           PosCurOBJ1  = *on;
399800150216           wmsg = Msg(16);
399900141127           leavesr;
400000141127         ENDIF;
400100141127       //?Devono essere tutti e 2 impostati
400200141127         IF  (wfobj1 = *blanks and wfobj2 <> *blanks) or
400300141127             (wfobj1 <> *blanks and wfobj2 = *blanks);
400400141127           ErrMessage  = *on;
400500141127           ErrGenerico = *on;
400600150216           wmsg = Msg(17);
400700141127           IF  wfobj1 = *blanks;
400800141127             PosCurOBJ1  = *on;
400900141127             leavesr;
401000141127           ENDIF;
401100141127           IF  wfobj2 = *blanks;
401200141127             PosCurOBJ2  = *on;
401300141127             leavesr;
401400141127           ENDIF;
401500141127         ENDIF;
401600150127
401700150127       //?Se il primo byte del campo formula è vuoto controllo se è
401800150127       //?uno dei campi validi e sistemo il campo
401900150127         IF  wfformula <> *blanks and %subst(wfformula:1:1) = *blanks
402000150127             and %check(ValoriFormula:%subst(wfformula:2:1)) = *zeros;
402100150127           %subst(wfformula:1:1) = %subst(wfformula:2:1);
402200150127           %subst(wfformula:2:1) = *blanks;
402300150127         ENDIF;
402400141127
402500150119       //?La formula deve essere una formula valida
402600150119         IF  %lookup (wfformula:Formula) = 0 and wfobj1 <> *blanks;
402700141127           ErrMessage  = *on;
402800141127           ErrGenerico = *on;
402900150119           PosCurFORM  = *on;
403000150216           wmsg = Msg(18);
403100141127           leavesr;
403200141127         ENDIF;
403300141128
403400150121       //?Se ci sono gli obiettivi devo avere anche la formula
403500141128         IF  wfobj1 <> *blanks and wfobj2 <> *blanks and
403600150119             wfformula = *blanks;
403700141128           ErrMessage  = *on;
403800141128           ErrGenerico = *on;
403900150119           PosCurFORM  = *on;
404000150216           wmsg = Msg(18);
404100141128           leavesr;
404200141128         ENDIF;
404300150121
404400150121       //?Se ho la formula devo avere anche gli obiettivi
404500150121         IF  wfobj1 = *blanks and wfobj2 = *blanks and
404600150121             wfformula <> *blanks;
404700150121           ErrMessage  = *on;
404800150121           ErrGenerico = *on;
404900150122           PosCurOBJ1  = *on;
405000150216           wmsg = Msg(19);
405100150121           leavesr;
405200150121         ENDIF;
405300150213
405400150213       //?Deve essere un obiettivo valido
405500150213         IF  wfobj3 <> *blanks and %lookup (wfobj3:Obiettivo) = 0;
405600150213           ErrMessage  = *on;
405700150213           ErrGenerico = *on;
405800150213           PosCurOBJ3  = *on;
405900150216           wmsg = Msg(13);
406000150213           leavesr;
406100150213         ENDIF;
406200150213       //?Almeno 1 dei 2 valori deve essere presente
406300150213         IF  wfobj3 <> *blanks and wobj3da = 0 and wobj3a = 0;
406400150216           //ErrMessage  = *on;
406500150216           //ErrGenerico = *on;
406600150216           //PosCurOBJ3DA = *on;
406700150216           //wmsg = Msg(20);
406800150216           //leavesr;
406900150216         ENDIF;
407000150213       //?Se impostate % serve il flag obiettivo
407100150213         IF  wfobj3 = *blanks and (wobj3da <> 0 or wobj3a <> 0);
407200150213           ErrMessage  = *on;
407300150213           ErrGenerico = *on;
407400150213           PosCurOBJ3  = *on;
407500150216           wmsg = Msg(13);
407600150213           leavesr;
407700150213         ENDIF;
407800150213         IF  wobj3da > wobj3a;
407900150213           ErrMessage  = *on;
408000150213           ErrGenerico = *on;
408100150213           PosCurOBJ3DA = *on;
408200150216           wmsg = Msg(21);
408300150213           leavesr;
408400150213         ENDIF;
408500141127
408600141127       ENDSR;
408700150302
408800150302       //--------------------------------------------------------------
408900150302       //?Controllo Scelta Trattative.
409000150302       //--------------------------------------------------------------
409100150302       BEGSR SceltaTtr;
409200150302
409300150302         IF  w002a = *blanks;
409400150302           leavesr;
409500150302         ENDIF;
409600150302
409700150302       //?Deve essere una Scelta valida
409800150302         clear xx;
409900150302         xx = %lookup(w002a : Trattative);
410000150302         IF  xx = 0;
410100150302           ErrMessage  = *on;
410200150302           ErrGenerico = *on;
410300150302           PosCurTTR   = *on;
410400150302           wmsg = Msg(24);
410500150302           leavesr;
410600150302         ENDIF;
410700150302
410800150302       ENDSR;
410900141121
411000141121       //--------------------------------------------------------------
411100150218       //?Richiamo pgm TRKC76R.
411200141121       //--------------------------------------------------------------
411300150218       BEGSR CallTRKC76;
411400141121
411500150218         wTRKC76 = *on;
411600141121
411700150218         clear TRKC76DS;
411800150303         IKC76ric = wtpric;
411900150218         IKC76ncm = CMIncm;
412000150218         IKC76ksu = CMIksu;
412100150218         IKC76ksc = CMIksc;
412200150218         IKC76cpo = CMIcpo;
412300150303         IKC76flgp = wflgp;
412400150220         IKC76acm = wFase;
412500150218         IKC76istat = CMIist;
412600150218
412700150218         trkc76r (kpjba:TRKC76DS);
412800150203
412900150203       ENDSR;
413000141028
413100141028       //--------------------------------------------------------------
413200141028       //?Operazioni finali.
413300141028       //--------------------------------------------------------------
413400141028       BEGSR RoutEnd;
413500150218
413600150218       //?Se richiamato chiudo i file per il pgm TRKC76R
413700150218         IF  wTRKC76;
413800150218           clear TRKC76DS;
413900150218           IKC76ric = 'C';
414000150218           trkc76r (kpjba:TRKC76DS);
414100150218         ENDIF;
414200141028
414300141028         *inLR = *on;
414400141028         return;
414500141028
414600141028       ENDSR;
414700141028
414800141028      /end-free
414900141028
415000141028       //--------------------------------------------------------------
415100141028       //?Schiere a tempo di compilazione.
415200141028       //--------------------------------------------------------------
415300150116
415400150116** -- Formule / DesForm -----------------------------------------------------*
415500150116=  : Uguale
415600150116>  : Maggiore
415700150116<  : Minore
415800150116>= : Maggiore Uguale
415900150116<= : Minore Uguale
416000150116<> : Diverso
416100150128** -- Obiettivo / DesObj ----------------------------------------------------*
416200150128I= Obiettivo Inizio
416300150128P= Obiettivo Proposto
416400150128F= Obiettivo Finale
416500150128T= Andamento Trattativa
416600150128C= Andamento Confronto Fatturazione
416700150302** -- Trattative / DesTtr----------------------------------------------------*
416800150302A = Avviate
416900150302AO= Avviate con Offerta
417000150302AN= Avviate senza Offerta
417100150302C = Chiuse
417200150302CN= Chiuse No Positive
417300150302CP= Chiuse Positive
417400150302SI= Tutte
417500150302T = Trattative non Forzate
417600150302F = Trattative Forzate
417700150302NO= Non avviate
417800141028** -- MSG -------------------------------------------------------------------*
417900141024Utente non abilitato alla Funzione.                                            1
418000141024Campagna Commerciale errata                                                    2
418100141027Distretto Errato                                                               3
418200141027Area Errata                                                                    4
418300141027Filiale Errata                                                                 5
418400150216Commerciale Unificante Errato                                                  6
418500150216Codice Cliente Errato                                                          7
418600150216Codice Importanza Clienti Errato                                               8
418700150216Immettere il Delta                                                             9
418800150216Delta previsto "DAL" incongruente con Delta previsto "AL"                     10
418900150216Opzione errata                                                                11
419000150216La modifica è possibile solo dal            al                                12
419100150216Impostare un Obiettivo/Andamento valido (F2)                                  13
419200150216Se richiesto Obiettivo Inizio non richiedere il "di cui"                      14
419300150216Non è possibile il "di cui" Finale se richiesto Obiettivo Proposto            15
419400150216Non è possibile il confronto tra 2 obiettivi uguali                           16
419500150216Impostare tutti e 2 gli obiettivi/andamento da confrontare                    17
419600150216Impostare una formula valida (F1=Legenda Formula)                             18
419700150216Impostare gli obiettivi/andamento                                             19
419800150216Immettere la % Obiettivo/Andamento                                            20
419900150216Obiettivo/Andamento "DAL" incongruente con Obiettivo/Andamento "AL"           21
420000150216Per poter fare F2 posizionarsi su obiettivo/andamento da inserire             22
420100150225Trattativa Forzata possibile solo dal            al                           23
420200150302Scelta trattativa non valida                                                  24
420300150611Impostare l'aera da elaborare                                                 25
