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
008400170302     d visRIRED        c                   const(X'29')
008500141112
008600141117     d ObjAll          c                   const('Tutti')
008700150128     d ObjInizio       c                   const('Inizio')
008800141117     d ObjProposto     c                   const('Proposto')
008900141126     d ObjFinale       c                   const('Finale')
009000150127
009100150127     d ValoriFormula   c                   const('><=')
009200141030
009300141028      //---------------------------------------------------------------
009400141028      //?Definizione schiere.
009500141028      //---------------------------------------------------------------
009600141027
009700141027      // - Sk Aree
009800141104     d skAree          s              3a   dim(999) inz
009900141029     d skDesArea       s                   like(ORGdes) dim(999) inz
010000141027
010100141027      // - Sk Distretti
010200141105     d skDistretti     s                   like(ORGfl3) dim(99) inz
010300141029     d skDesDist       s                   like(§17des) dim(99) inz
010400141024
010500141024      // - Sk Cod.Importanza clienti da tabella IC
010600141024     d skIC            s              1    dim(10)
010700141117     d skIC_ord        s              1  0 dim(10)
010800141103
010900141103      // - Sk Clienti per interrogazione Delta
011000141103     d skKSC           s              7  0 dim(28)
011100150116
011200150116      // - Sk Formule
011300150116     d Formula         s              2a   dim(06) ctdata perrcd(1)
011400150121     d DesForm         s             20a   dim(06) alt(Formula)
011500150128
011600150128      // - Sk Obiettivo/Andamento
011700150128     d Obiettivo       s              1a   dim(05) ctdata perrcd(1)
011800150128     d DesObj          s             35a   dim(05) alt(Obiettivo)
011900150302
012000150302      // - Sk Trattative
012100150302     d Trattative      s              2a   dim(10) ctdata perrcd(1)
012200150302     d DesTtr          s             30a   dim(10) alt(Trattative)
012300141028
012400141028      // - Messaggi di errore
012500141124     d Msg             s             78    dim(30) ctdata perrcd(1)
012600141028
012700141028      //---------------------------------------------------------------
012800141028      //?Definizione aree dati.
012900141028      //---------------------------------------------------------------
013000141028
013100141028      // - Dati utente
013200141028     d §AzUte        e ds                  extname(AZUTE00F)
013300141028     d                                     dtaara
013400141028     d §DatiUte      e ds                  extname(dDatiUte)
013500141028     d                                     dtaara
013600141028
013700141028      //---------------------------------------------------------------
013800141028      //?Definizione strutture dati.
013900141028      //---------------------------------------------------------------
014000141028
014100141028      // - Status
014200141028     d Psds           sds
014300141028     d   SDSpgm          *proc
014400141028
014500141028      // - InfDS
014600141028     d InfDspF         ds
014700141028     d  dsp_aid              369    369a
014800141028     d  sfl_rrn              376    377i 0
014900141028     d  min_nrr              378    379i 0
015000141028     d  num_rcds             380    381i 0
015100141028
015200141028      // - Indicatori su DspF
015300141028     d IndDspF         ds
015400141111        // - Indicatori di Abilitazione Tasti
015500141111     d  AbilitaF10                    1n   overlay(IndDspF : 10)
015600141127     d  AbilitaF13                    1n   overlay(IndDspF : 13)
015700141024        // - Indicatori di Visualizzazione Campi
015800141024     d  VisArea                       1n   overlay(IndDspF : 26)
015900141024     d  VisDistretto                  1n   overlay(IndDspF : 27)
016000141028        // - Indicatori di errore in videata
016100141028     d  ErrMessage                    1n   overlay(IndDspF : 28)
016200141028        // - Indicatori di gestione del subfile
016300141029     d  SflDsp                        1n   overlay(IndDspF : 30)
016400141029     d  SflDspCtl                     1n   overlay(IndDspF : 31)
016500141028     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016600141028     d  SflEnd                        1n   overlay(IndDspF : 33)
016700141030        // - Altri indicatori di visualizzazione campi
016800141111     d  VisOrdRAG                     1n   overlay(IndDspf : 35)
016900141030     d  VisOrdCLV                     1n   overlay(IndDspf : 36)
017000141121     d  VisEsito                      1n   overlay(IndDspf : 37)
017100170303     d  VisDelta                      1n   overlay(IndDspf : 38)
017200141028        // - Indicatori di errore
017300141024     d  PosCurNCM                     1n   overlay(IndDspF : 50)
017400141027     d  PosCurCDI                     1n   overlay(IndDspF : 51)
017500141027     d  PosCurCAR                     1n   overlay(IndDspF : 52)
017600141027     d  PosCurFIL                     1n   overlay(IndDspF : 53)
017700141027     d  PosCurCMM                     1n   overlay(IndDspF : 54)
017800141107     d  PosCurKSU                     1n   overlay(IndDspF : 55)
017900141028     d  PosCurCLV1                    1n   overlay(IndDspF : 56)
018000141028     d  PosCurCLV2                    1n   overlay(IndDspF : 57)
018100141028     d  PosCurCLV3                    1n   overlay(IndDspF : 58)
018200141028     d  PosCurCLV4                    1n   overlay(IndDspF : 59)
018300141028     d  PosCurCLV5                    1n   overlay(IndDspF : 60)
018400141028     d  PosCurDELda                   1n   overlay(IndDspF : 61)
018500141028     d  PosCurDELa                    1n   overlay(IndDspF : 62)
018600141028     d  PosCurOPZ                     1n   overlay(IndDspF : 65)
018700141127     d  PosCurOBJ1                    1n   overlay(IndDspF : 66)
018800141127     d  PosCurOBJ2                    1n   overlay(IndDspF : 67)
018900150119     d  PosCurFORM                    1n   overlay(IndDspF : 68)
019000150213     d  PosCurOBJ3                    1n   overlay(IndDspF : 69)
019100150213     d  PosCurOBJ3DA                  1n   overlay(IndDspF : 70)
019200150302     d  PosCurTTR                     1n   overlay(IndDspF : 71)
019300141028        // - Indicatori di errore generico
019400141028     d  ErrGenerico                   1n   overlay(IndDspF : 99)
019500141028
019600141028     d WindDspF        ds                  inz
019700141028     d  WindDspFa              1     49    inz(*zero)
019800141028     d  WindDspFb             50     99    inz(*zero)
019900141028
020000141028      // - Parametri ricevuti
020100141028     d KPJBA         e ds
020200170303
020300170303      // - Ricerca Unificante Padre?
020400170303     d TIBS10DS      e ds                  inz
020500141105
020600150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
020700150112     d TRKC02DS      e ds
020800141107
020900150112      // - Richiamo pgm TRKC03R - Storico Fasi
021000150112     d TRKC03DS      e ds
021100141111
021200150112      // - Richiamo pgm TRKC06R - Aggiunta Cliente in campagna
021300150112     d TRKC06DS      e ds
021400141114
021500150112      // - Richiamo pgm TRKC07R - Variazione Obiettivo
021600150112     d TRKC07DS      e ds
021700150127
021800150127      // - Richiamo pgm TRKC10R - Gestione Note
021900150127     d TRKC10DS      e ds
022000141127
022100141127      // - Scrittura fase
022200150112     d TRKC71DS      e ds                  inz
022300141114
022400150115      // - Verifica quale obiettivo variare
022500150112     d TRKC72DS      e ds                  inz
022600150218
022700150218      // - Dati da Visualizzare o Parzializzazioni Trattative
022800150218     d TRKC76DS      e ds                  inz
022900170302
023000170302      // - Calcolo delta e spedizioni
023100170302     d TRKC78DS      e ds                  inz
023200141114
023300141114      // - File Clienti in Campagna Commerciale
023400141114     d TICMC00F      e ds                  extname(TICMC00F)
023500141028
023600141028      // - File INFO Clienti in Campagna Commerciale
023700141028     d TICMI00F      e ds                  extname(TICMI00F)
023800150115
023900150115      // - Ricerca/Controllo tabelle
024000150115     d TIBS02DS      e ds                  inz
024100141028
024200141028      // - Reperimento dati utente
024300141027     d TIBS34DS      e ds
024400141027
024500141027      // - Reperimento dati Anagrafica Clienti
024600141027      /copy gaitrasrc/srcprotopi,TIBS69R
024700141103
024800141103      // - Delta cliente unificante
024900141103     d TISE60DS      e ds
025000141103     d  skKSA                 50    161    dim(28)
025100141024
025200141024      // - Controllo abilitazioni
025300141024     d TNTAA1DS      e ds                  inz
025400141027
025500141027      // - Ricerca Anagrafica Clienti
025600141027     d TNTAI1DS      e ds                  inz
025700141103
025800141103      // - Int. Anagrafica Clienti
025900141103     d TNTAI2DS      e ds                  inz
026000141103
026100141103      // - Gestione trattative commerciali
026200141103     d TNTA88DS      e ds                  inz
026300141024
026400141024      // - Reperimento filiali gestite dall'utente
026500141028     d TRUL31DS      e ds
026600141029     d  POG                   10    759    DIM(250)
026700141027     d TRUL31DS2     e ds
026800141029     d  DIS                    1     20    DIM(20)
026900141029     d  CAR                   22    171    DIM(50)
027000150115
027100150115      // - Tabella ECM - Causali chiusura campagna
027200150115     d dECM          e ds
027300141027
027400141027      // - Tabella 05 - Aree
027500141027     d ds05          e ds
027600141027
027700141027      // - Tabella 17 - Distretti
027800141027     d ds17          e ds
027900170302
028000170302      // - Tabella 5S - Statistiche
028100170302     d ds5S          e ds
028200141117
028300141117      // - Tabella IC - Importanza Clienti
028400141117     d dsIC          e ds
028500141103
028600141103      // - DS per passaggio clienti PACKED per int. delta
028700141103     d                 ds
028800141126     d  dsKSC                  1      4p 0
028900141103     d  dsKSA                  1      4
029000141028
029100141028      //---------------------------------------------------------------
029200141028      //?Definizione variabili globali.
029300141028      //---------------------------------------------------------------
029400141028
029500141028      // - Flags booleani
029600141024     d EndS03          s               n   inz(*off)
029700141024     d ErrGrave        s               n   inz(*off)
029800141024     d Fine            s               n   inz(*off)
029900141119     d Primo           s               n   inz(*off)
030000141124     d RecOK           s               n   inz(*off)
030100170303     d Ricarica        s               n   inz(*off)
030200150116     d TroppiRcd       s               n   inz(*off)
030300141028     d wEnd            s               n   inz(*off)
030400141113     d wclv            s               n   inz(*off)
030500141029     d wInzD02         s               n   inz(*off)
030600141030     d wInzD04         s               n   inz(*off)
030700141118     d wInzD05         s               n   inz(*off)
030800150128     d wInzD07         s               n   inz(*off)
030900141029     d wInzS03         s               n   inz(*off)
031000150116     d wInzS06         s               n   inz(*off)
031100150128     d wInzS08         s               n   inz(*off)
031200150302     d wInzS09         s               n   inz(*off)
031300141103     d wMaxNrr         s               n   inz(*off)
031400141103     d wokTtr          s               n   inz(*off)
031500150218     d wTRKC76         s               n   inz(*off)
031600141028
031700141028      // - Indici di schiera
031800141029     d xx              s              4s 0 inz
031900141028
032000141028      // - Campi associati al video
032100141029     d Video           s              2a   inz('D2')
032200141029     d S03nrr          s              4s 0 inz
032300150116     d S06nrr          s              4s 0 inz
032400150128     d S08nrr          s              4s 0 inz
032500150302     d S09nrr          s              4s 0 inz
032600141112     d sav_S03nrr      s              4s 0 inz
032700141028
032800141028       // - Stringa SQL da eseguire
032900141028     d wSQL            s           2048    Varying        inz
033000141028
033100141028      // - Campi di comodo data
033200141024     d Data_EUR        s               d   datfmt(*eur)
033300141024     d Data_ISO        s               d   datfmt(*iso)
033400150203     d wData           s              8s 0 inz
033500141028
033600141028      // - Campi di comodo
033700141029     d Oggi            s              8s 0 inz
033800150128     d savformula      s                   like(V02formula) inz
033900170303     d savokobj        s                   like(VS3okobj) inz
034000170303     d savokobjp       s                   like(VS3okobjp) inz
034100170303     d savokobjf       s                   like(VS3okobjf) inz
034200170303     d savokdelta      s                   like(VS3okdelta) inz
034300141027     d wabi            s                   like(OTAA1cabi) inz
034400170302     d wanno           s              4s 0 inz
034500141112     d wdela           s                   like(CMIpde) inz
034600141112     d wdelda          s                   like(CMIpde) inz
034700150220     d wfase           s                   like(CMFacm) inz
034800150303     d wflgp           s                   like(IKC76flgp) inz
034900150128     d wfobj           s                   like(V02obj) inz
035000150128     d wfobj1          s                   like(V02obj1) inz
035100150128     d wfobj2          s                   like(V02obj2) inz
035200150213     d wfobj3          s                   like(V02obj3) inz
035300150128     d wfformula       s                   like(V02formula) inz
035400141029     d wimp            s             13s 2 inz
035500170303     d wksu            s                   like(VS3ksu) inz
035600170302     d wmese           s              2s 0 inz
035700141030     d wmsg            s             78a   inz
035800150213     d wobj3da         s                   like(CMFpea) inz
035900150213     d wobj3a          s                   like(CMFpea) inz
036000150128     d wora            s                   like(CMFhfc) inz
036100150203     d wperc           s                   like(CMFpea) inz
036200150303     d wtpric          s                   like(IKC76ric) inz
036300150128     d wvalobj1        s                   like(CMFpea) inz
036400150128     d wvalobj2        s                   like(CMFpea) inz
036500150213     d wvalobj3        s                   like(CMFpea) inz
036600141030     d w001a           s              1a   inz
036700150302     d w002a           s              2a   inz
036800150218     d w0020           s              2s 0 inz
036900141030     d w003a           s              3a   inz
037000141029     d w0030           s              3s 0 inz
037100141030     d w007a           s              7a   inz
037200141029     d w0070           s              7s 0 inz
037300141030     d w050a           s             50a   inz
037400141024
037500141024      // - Campi x ricerca tabelle TABEL
037600141024     d idTabella       s              2
037700141024     d Ordinamento     s              1
037800141024     d idElemento      s              8
037900141024     d TastoUscita     s              1
038000141027
038100141027       // - Parametri per ricerca Filiale
038200141027     d pinFIL          s              3
038300141027     d pinFAG          s              1
038400141027     d pinDES          s             25
038500141027     d pinDIT          s              3
038600141027
038700141027      // - Parametri per ricerca Commerciale Unificante
038800141029      /copy gaitrasrc/srcprotopi,TRMK43R_1
038900141028
039000141028      // ----------------------------------------------------------------------
039100141027      //?DATI per ordinamento subfile
039200141028      // ----------------------------------------------------------------------
039300141028     d MaxKey          c                   5
039400141028     d Ascendente      c                   1
039500141028     d Discendente     c                   2
039600141028     d Carattere       c                   6
039700141028     d Numerico        c                   8
039800141028     d Put             c                   1
039900141028     d EndPut          c                   2
040000141028     d Get             c                   3
040100141028      *************************************************************************
040200141028      * Campi utili:
040300141028      *     nrr        - Numero relativo di record del Subfile
040400141028      *     SizeList   - Dimensione della lista
040500141028      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
040600141028      *************************************************************************
040700141028     d NotUsed         s             16A
040800141028     d ReturnSize      s              9B 0
040900170215     d*// SizeList        s              9B 0
041000170215     d SizeList        s             10I 0
041100141028     d RrnLast         s              5I 0
041200141028     d WrkSort         s              1  0 inz(0)
041300141028      *************************************************************************
041400141028      * Data Structures
041500141028      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
041600141028      *     QLGSCB     - The sort request block for the QLGSORT API
041700141028      *     QLGSCB00   - The sort request block for the QLGSRTIO API
041800141028      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
041900141028      *     QUSEC      - Error structure for the QLGSORT API
042000141028      *************************************************************************
042100141028     d sflrcd          ds
042200141103     d  VS3clv
042300141117     d  VS3ord
042400141107     d  VS3ksu
042500150116     d  VS3tipocl
042600141103     d  VS3rag
042700141103     d  VS3istat
042800141103     d  VS3del
042900141103     d  VS3sped
043000141103     d  VS3ric
043100141103     d  VS3pkgm
043200141103     d  VS3obj
043300170303     d  VS3alfa
043400141103     d  VS3objttr
043500141103     d  VS3objcf
043600141103     d  VS3cmmd
043700141121     d  VS3esitotr
043800141126     d  VS3decotr
043900150220     d  VS3forza
044000141103     d  VS3nrv
044100141107     d  VS3ksc
044200141107     d  VS3cpo
044300141117     d  VS3ufe
044400150115     d  VS3cch
044500150115     d  VS3cchaut
044600170303     d  VS3hobj
044700170303     d  VS3hobjpro
044800170302     d  VS3hobjfin
044900170302     d  VS3hobjcf
045000170303     d  VS3hdelta
045100170303     d  VS3hnsped
045200170303     d  VS3okobj
045300170303     d  VS3okobjp
045400170303     d  VS3okobjf
045500170303     d  VS3okdelta
045600141028     d  selected                      1A
045700141028
045800141028      /COPY QSYSINC/QRPGLESRC,QLGSORT
045900141028     d QLGKL                         16    DIM(MaxKey)
046000141028     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
046100141028     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
046200141028     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
046300141028     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
046400141028
046500141028      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
046600141028      /COPY QSYSINC/QRPGLESRC,QUSEC
046700141028
046800141028      //---------------------------------------------------------------
046900141028      //?Definizione procedure usate.
047000141028      //---------------------------------------------------------------
047100141028
047200141024      // - Ordinamento Subfile
047300141028     d Qlgsort_pr      pr                  extpgm('QLGSORT')
047400141028     d  pr_QLGSCB                          like(Qlgscb)
047500141028     d  pr_NotUsed1                        like(NotUsed)
047600141028     d  pr_NotUsed2                        like(NotUsed)
047700141028     d  pr_SizeList                        like(SizeList)
047800141028     d  pr_ReturnSize                      like(ReturnSize)
047900141028     d  pr_QUSEC                           like(Qusec)
048000141028     d                                     options(*varsize)
048100141028
048200141028     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
048300141028     d  pr_QLGSCB00                        like(QLGSCB00)
048400141028     d  pr_SflRcd                          like(SflRcd)
048500141028     d  pr_NotUsed1                        like(NotUsed)
048600141028     d  pr_SizeList                        like(SizeList)
048700141028     d  pr_NotUsed2                        like(NotUsed)
048800141028     d  pr_QUSEC                           like(Qusec)
048900141028     d                                     options(*varsize)
049000141028
049100141028     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
049200141028     d  pr_QLGSCB00                        like(QLGSCB00)
049300141028     d  pr_NotUsed1                        like(NotUsed)
049400141028     d  pr_SflRcd                          like(SflRcd)
049500170215     d  pr_SizeList                        like(SizeList)
049600141028     d  pr_NotUsed2                        like(NotUsed)
049700141028     d  pr_QUSEC                           like(Qusec)
049800141028     d                                     options(*varsize)
049900141105
050000141105       // - Interrogazione anagrafica campagne
050100150112     d TRKC02R         pr                  extpgm('TRKC02R')
050200141105     d  kpjba                              likeds(kpjba)
050300150112     d  trkc02ds                           likeds(TRKC02DS)
050400141107
050500141107       // - Storico Fasi
050600150112     d TRKC03R         pr                  extpgm('TRKC03R')
050700141107     d  kpjba                              likeds(kpjba)
050800150112     d  trkc03ds                           likeds(TRKC03DS)
050900141111
051000141111       // - Aggiunta Cliente in campagna
051100150112     d TRKC06R         pr                  extpgm('TRKC06R')
051200141111     d  kpjba                              likeds(kpjba)
051300150112     d  trkc06ds                           likeds(TRKC06DS)
051400141114
051500141114       // - Variazione Obiettivo
051600150112     d TRKC07R         pr                  extpgm('TRKC07R')
051700141114     d  kpjba                              likeds(kpjba)
051800150112     d  trkc07ds                           likeds(TRKC07DS)
051900150127
052000150127       // - Gestione Note
052100150127     d TRKC10R         pr                  extpgm('TRKC10R')
052200150127     d  kpjba                              likeds(kpjba)
052300150127     d  trkc10ds                           likeds(TRKC10DS)
052400141127
052500141127      // - Pgm Scrive fase se cliente in Campagna
052600150112     d TRKC71R         pr                  extpgm('TRKC71R')
052700141127     d  kpjba                              likeds(kpjba)
052800150112     d  trkc71ds                           likeds(TRKC71DS)
052900141114
053000141114      // - Verifica quale obiettivo variare
053100150112     d TRKC72R         pr                  extpgm('TRKC72R')
053200141114     d  kpjba                              likeds(kpjba)
053300150112     d  trkc72ds                           likeds(TRKC72DS)
053400150218
053500150218      // - Dati da Visualizzare o Parzializzazioni Trattative
053600150218     d TRKC76R         pr                  extpgm('TRKC76R')
053700150218     d  kpjba                              likeds(kpjba)
053800150218     d  trkc76ds                           likeds(TRKC76DS)
053900170302
054000170302      // - Calcolo delta e spedizioni
054100170302     d TRKC78R         pr                  extpgm('TRKC78R')
054200170302     d  kpjba                              likeds(kpjba)
054300170302     d  trkc78ds                           likeds(TRKC78DS)
054400141103
054500141103       // - Delta cliente unificante
054600141103     d TISE61R         pr                  extpgm('TISE61R')
054700141103     d  kpjba                              likeds(kpjba)
054800141027
054900141103       // - Ricerca anagrafica clienti
055000141027     d TNTAI1R         pr                  extpgm('TNTAI1R')
055100141027     d  kpjba                              likeds(kpjba)
055200141027     d  tntai1ds                           likeds(tntai1ds)
055300141103
055400141103       // - Int. anagrafica clienti
055500141103     d TNTAI2R         pr                  extpgm('TNTAI2R')
055600141103     d  kpjba                              likeds(kpjba)
055700141103     d  tntai2ds                           likeds(tntai2ds)
055800141029
055900141029       // - Caricamento Filiali in gestione
056000141029     d TRUL31R         pr                  extpgm('TRUL31R')
056100141029     d  kpjba                              likeds(kpjba)
056200141029     d  trul31ds                           likeds(trul31ds)
056300141029     d  trul31ds2                          likeds(trul31ds2)
056400141028
056500141028      //---------------------------------------------------------------
056600141024      //?Definizione Prototipi.
056700141028      //---------------------------------------------------------------
056800141024      /copy gaitrasrc/srcprotopr,QsnQryModS
056900150115      /copy gaitrasrc/srcprotopr,TIBS02R
057000170303      /copy gaitrasrc/srcprotopr,TIBS10R
057100141024      /copy gaitrasrc/srcprotopr,TIBS34R
057200141027      /copy gaitrasrc/srcprotopr,TIBS69R
057300141027      /copy gaitrasrc/srcprotopr,TNSD24R
057400141024      /copy gaitrasrc/srcprotopr,TNTAA1C
057500141103      /copy gaitrasrc/srcprotopr,TNTA88R
057600141027      /copy gaitrasrc/srcprotopr,TRMK43R
057700141029      /copy gaitrasrc/srcprotopr,TRUL19R
057800141028
057900141028      //---------------------------------------------------------------
058000141028      //?Definizione key-list.
058100141028      //---------------------------------------------------------------
058200141028       // - File TABEL00F
058300141028     d k03tabel      e ds                  extname(TABEL00F:*key)
058400141028     d                                     prefix(k_)
058500141028
058600141028      //---------------------------------------------------------------
058700141028      //?M A I N - L I N E
058800141028      //---------------------------------------------------------------
058900141028
059000141028     c     *Entry        plist
059100141028     c                   parm                    kpjba
059200141028
059300141028      /free
059400141028
059500141028       //?Operazioni iniziali
059600141028       exsr RoutInz;
059700141028
059800141028       //?Gestione video
059900141024       DOW  Fine = *off;
060000141028         SELECT;
060100141024           WHEN  Video = 'D2';
060200141024             exsr GesD02;
060300141024           WHEN  Video = 'S3';
060400141024             exsr GesS03;
060500141030           WHEN  Video = 'D4';
060600141030             exsr GesD04;
060700141118           WHEN  Video = 'D5';
060800141118             exsr GesD05;
060900150116           WHEN  Video = 'S6';
061000150116             exsr GesS06;
061100150121           WHEN  Video = 'D7';
061200150121             exsr GesD07;
061300150128           WHEN  Video = 'S8';
061400150128             exsr GesS08;
061500150302           WHEN  Video = 'S9';
061600150302             exsr GesS09;
061700141028           OTHER;
061800141024             Fine = *on;
061900141028         ENDSL;
062000141028       ENDDO;
062100141028
062200141028       //?Operazioni finali
062300141028       exsr RoutEnd;
062400141028
062500141028       //--------------------------------------------------------------
062600141028       //?Operazioni iniziali.
062700141028       //--------------------------------------------------------------
062800141028       BEGSR RoutInz;
062900141028
063000141028         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
063100141028
063200141028       //?Impostazione campi "fissi"
063300141024         V01pgm = SDSpgm;
063400141029         k_TBLkut = 1;
063500141029         Video = 'D2';
063600141029         wInzD02 = *on;
063700150121         VisOrdRAG = *on;
063800150121         VisOrdCLV = *off;
063900141029
064000141029       //?Imposto oggi
064100141029         Oggi = %dec(%date());
064200141028
064300141028       //?Reperimento dati job
064400141028         exsr DatiJob;
064500141024
064600141024       //?Controllo abilitazione utente
064700141024         reset TNTAA1DS;
064800141024         ITAA1caut = '§utegtc';
064900141024         clear kpjbu;
065000141024         kpjbu = TNTAA1DS;
065100141024         tntaa1c (kpjba);
065200141024         TNTAA1DS = kpjbu;
065300141024
065400141024         IF  OTAA1fabi = 'N';
065500141024           ErrGrave = *on;
065600141024           leavesr;
065700141024         ENDIF;
065800141027         wabi = OTAA1cabi;
065900141024
066000141027       //?Carico le filiali abilitate all'utente
066100141024         clear TRUL31DS;
066200141027         clear TRUL31DS2;
066300141027         I31abi = wabi;
066400141024         I31cdi = DUTdis;
066500141024         I31car = DUTare;
066600141024         I31cpo = DUTpou;
066700141027         TRUL31R (kpjba:trul31ds:trul31ds2);
066800141024         IF O31pog <= *zeros;
066900141024           ErrGrave = *on;
067000141024           leavesr;
067100141024         ENDIF;
067200141027
067300141027       //?Carico sk Distretti - 17
067400141027         clear xx;
067500141027         k_TBLcod = '17';
067600141027         clear k_TBLkey;
067700141027         setll %kds(K03tabel) TABEL00F;
067800141027         reade %kds(K03tabel : 2) TABEL00F;
067900141027         DOW  not %eof(TABEL00F);
068000141027           IF  TBLflg = *blanks;
068100141027             ds17 = TBLuni;
068200141027             xx += 1;
068300141027             skDistretti(xx) = %subst(TBLkey:1:1);
068400141027             skDesDist(xx) = §17des;
068500141027           ENDIF;
068600141027           reade %kds(K03tabel : 2) TABEL00F;
068700141027         ENDDO;
068800141027
068900141027       //?Carico sk Aree - 05
069000141027         clear xx;
069100141027         k_TBLcod = '05';
069200141027         clear k_TBLkey;
069300141027         setll %kds(K03tabel) TABEL00F;
069400141027         reade %kds(K03tabel : 2) TABEL00F;
069500141027         DOW  not %eof(TABEL00F);
069600141027           IF  TBLflg = *blanks;
069700141027             ds05 = TBLuni;
069800141027             xx += 1;
069900141104             skAree(xx) = %subst(TBLkey:1:3);
070000141027             skDesArea(xx) = §05des;
070100141027           ENDIF;
070200141027           reade %kds(K03tabel : 2) TABEL00F;
070300141027         ENDDO;
070400141024
070500141024       //?Carico sk Importanza Clienti
070600141024         clear xx;
070700141029         k_TBLcod = 'IC';
070800141027         clear k_TBLkey;
070900141027         setll %kds(K03tabel) TABEL00F;
071000141027         reade %kds(K03tabel : 2) TABEL00F;
071100141024         DOW  not %eof(TABEL00F);
071200141024           IF  TBLflg = *blanks;
071300141024             xx += 1;
071400141024             skIC(xx) = %subst(TBLkey:1:1);
071500141117             dsIC = TBLuni;
071600141117             skIC_ord(xx) = §SICor;
071700141024           ENDIF;
071800141027           reade %kds(K03tabel : 2) TABEL00F;
071900141024         ENDDO;
072000170302
072100170302       //?Dalla tabella "5S" recupero le date aggiornamento saldi
072200170302         k_TBLkut = 1;
072300170302         k_TBLcod = '5S';
072400170302         clear k_TBLkey;
072500170302         clear ds5S;
072600170302         chain %kds(K03tabel) TABEL00F;
072700170302         IF  %found(TABEL00F) and TBLflg = *blanks;
072800170302           ds5S = TBLuni;
072900170302         ENDIF;
073000170302
073100170302       //?Imposto Anno e Mese per calcolo saldi clienti
073200170302         Data_ISO = %date(§5Sdac);
073300170302       //?Devo elaborare il mese precedente alla data ultimo aggiornamento saldi
073400170302         wdata = %dec(Data_ISO - %months(1));
073500170302         Data_ISO = %date(wdata);
073600170302         wanno = %subdt(Data_ISO:*years);
073700170302         wmese = %subdt(Data_ISO:*months);
073800141028
073900141028       ENDSR;
074000141028
074100141028       //--------------------------------------------------------------
074200141028       //?Reperimento Dati del job (Utente/Operativi).
074300141028       //--------------------------------------------------------------
074400141028       BEGSR DatiJob;
074500141028
074600141028         in(E) §AzUte;
074700141024         IF  NOT %error;
074800141028           in(E) §DatiUte;
074900141024         ENDIF;
075000141024         IF  %error or RSut = *blanks;
075100141028           clear TIBS34ds;
075200141028           tibs34r(tibs34ds);
075300141028           in §AzUte;
075400141028           in §DatiUte;
075500141024         ENDIF;
075600141028
075700141028       ENDSR;
075800141024
075900141024       //--------------------------------------------------------------
076000141024       //?Gestione videata D02.
076100141024       //--------------------------------------------------------------
076200141024       BEGSR GesD02;
076300141024
076400141024       //?Inizializzazione videata
076500141029         IF  wInzD02;
076600141029           exsr InzD02;
076700141029           wInzD02 = *off;
076800141024         ENDIF;
076900141024
077000141024       //?Se errore grave emetto messaggio poi esco
077100141024         IF  ErrGrave;
077200141024           ErrMessage  = *on;
077300141024           ErrGenerico = *on;
077400141024           V02msg = Msg(01);
077500141024         ENDIF;
077600141024
077700141030       //?Emissione Testata
077800150112         write  KC01T01;
077900141024
078000141024       //?Emissione videata
078100150112         exfmt  KC01D02;
078200141024         reset ErrMessage;
078300141024         reset ErrGenerico;
078400141024         clear V02msg;
078500141024
078600141024         SELECT;
078700141024
078800141024       //?- Errore Grave esco
078900141024           WHEN  ErrGrave;
079000141024             Fine = *on;
079100150116
079200150116       //?- F01=Formula
079300150116           WHEN  dsp_aid = c_F01;
079400150116             exsr F01D02;
079500150128
079600150128       //?- F02=Obiettivo/Andamento
079700150128           WHEN  dsp_aid = c_F02;
079800150128             exsr F02D02;
079900141024
080000141024       //?- F03=Fine
080100141024           WHEN  dsp_aid = c_F03;
080200141024             exsr F03D02;
080300150302
080400150302       //?- F04=Scelta Trattative
080500150302           WHEN  dsp_aid = c_F04;
080600150302             exsr F04D02;
080700141024
080800141024       //?Invio
080900141024           OTHER;
081000141024             exsr ContrD02;
081100141028             IF  ErrGenerico;
081200141028               leavesr;
081300141028             ENDIF;
081400141028         //?Videata sucessiva
081500141028             Video = 'S3';
081600141029             wInzS03 = *on;
081700150212             wInzD04 = *on;
081800141024
081900141024         ENDSL;
082000141024
082100141024       ENDSR;
082200141024
082300141024       //--------------------------------------------------------------
082400141029       //?Inizializzazione Videata D02.
082500141024       //--------------------------------------------------------------
082600141029       BEGSR InzD02;
082700141027
082800141027         VisDistretto = *off;
082900141027         VisArea      = *off;
083000141202
083100141202         clear V01des;
083200141024
083300141024       //?Pulizia videata
083400150112         clear KC01D02;
083500141024
083600141024       //?Imposto di dflt il "+" nelle % delta
083700141024         V02deldas = '+';
083800141024         V02delas = '+';
083900150127
084000170207       //*·//?Imposto di dflt 'E' escludi
084100170207       //*·  V02cli = 'E';
084200150213
084300150213       //?Imposto di dflt il "+" nelle % Obiettivo/Andamento
084400150213         V02obj3das = '+';
084500150213         V02obj3as = '+';
084600141024
084700141024       //?Se errore grave non imposto più niente
084800141024         IF  ErrGrave;
084900141024           leavesr;
085000141024         ENDIF;
085100141027
085200141027         SELECT;
085300141027       //?Abilitazione AZ-Azienda
085400141027       //?vedo tutte le selezioni
085500141027         WHEN  wabi = 'AZ';
085600141027           VisDistretto = *on;
085700141027           VisArea      = *on;
085800141024
085900141027       //?Abilitazione DI-Distretto
086000141027       //?NON vedo la selezione per Distretto
086100150611         WHEN  wabi = 'DI' or car(2) > *zeros;
086200150611           VisArea = *on;
086300141027
086400141027       //?Abilitazione TP-Terminal Partenza o PO=Filiale
086500141027       //?NON vedo la selezione per Distretto e per Area
086600141029         WHEN  wabi = 'TP' or wabi = 'PO';
086700141027         ENDSL;
086800141024
086900141024       ENDSR;
087000150116
087100150116       //--------------------------------------------------------------
087200150116       //?Gestione tasto funzionale F01 da videata D02
087300150116       //?F01=Formula
087400150116       //--------------------------------------------------------------
087500150116       BEGSR F01D02;
087600150116
087700150119         savformula = V02formula;
087800150116         Video = 'S6';
087900150116         wInzS06 = *on;
088000150116
088100150116       ENDSR;
088200150128
088300150128       //--------------------------------------------------------------
088400150128       //?Gestione tasto funzionale F02 da videata D02
088500150128       //?F02=Obiettivo/Andamento
088600150128       //--------------------------------------------------------------
088700150128       BEGSR F02D02;
088800150216
088900150216         IF  H2nmfl <> 'V02OBJ1' and H2nmfl <> 'V02OBJ2' and
089000150216             H2nmfl <> 'V02OBJ3';
089100150216           ErrMessage  = *on;
089200150216           ErrGenerico = *on;
089300150216           PosCurOBJ1  = *on;
089400150216           V02msg = Msg(22);
089500150216           leavesr;
089600150216         ENDIF;
089700150128
089800150128         Video = 'S8';
089900150128         wInzS08 = *on;
090000150128
090100150128       ENDSR;
090200141029
090300141029       //--------------------------------------------------------------
090400141029       //?Gestione tasto funzionale F03 da videata D02
090500141029       //?F03=Fine
090600141029       //--------------------------------------------------------------
090700141029       BEGSR F03D02;
090800141029
090900141029       //?Chiusura del programma
091000141029         Fine = *on;
091100141029
091200141029       ENDSR;
091300150302
091400150302       //--------------------------------------------------------------
091500150302       //?Gestione tasto funzionale F04 da videata D02
091600150302       //?F04=Scelta Trattative
091700150302       //--------------------------------------------------------------
091800150302       BEGSR F04D02;
091900150302
092000150302         Video = 'S9';
092100150302         wInzS09 = *on;
092200150302
092300150302       ENDSR;
092400141024
092500141024       //--------------------------------------------------------------
092600141024       //?Controlla Videata D02.
092700141024       //--------------------------------------------------------------
092800141024       BEGSR ContrD02;
092900141024
093000141024         WindDspF = IndDspF;
093100141024         reset WindDspFb;
093200141024         IndDspF  = WindDspF;
093300141119
093400141119         clear wdelda;
093500141119         clear wdela;
093600141024
093700141027       //?Campagna Commerciale Obbligatoria
093800141027         clear V02des;
093900141024         IF  V02ncm = *blanks;
094000141024           ErrMessage  = *on;
094100141024           ErrGenerico = *on;
094200141024           PosCurNCM   = *on;
094300141024           V02msg = Msg(02);
094400141024           leavesr;
094500141024         ENDIF;
094600141027       //?Ricerca Campagna Commerciale
094700141024         IF  %scan('?':V02ncm) > 0;
094800150112           clear TRKC02DS;
094900150112           IKC02ric = 'R';
095000150112           trkc02r (kpjba:trkc02ds);
095100141024           ErrGenerico = *on;
095200141024           PosCurNCM   = *on;
095300150112           V02ncm = %editc(OKC02ncm:'X');
095400141024         ENDIF;
095500141027       //?Accetto solo dati numerici
095600141029         xx = 1;
095700141029         FOR xx by 1 to %len(V02ncm);
095800141029           IF  %subst(V02ncm:xx:1) <> *blanks and
095900141029               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
096000141029             ErrMessage  = *on;
096100141029             ErrGenerico = *on;
096200141029             PosCurNCM   = *on;
096300141029             V02msg = Msg(02);
096400141029             leavesr;
096500141029           ENDIF;
096600141029         ENDFOR;
096700141027       //?Deve essere una Campagna Commerciale valida
096800141024         CMPncm = %dec(V02ncm:7:0);
096900141027         chain CMPncm TICMP01L;
097000141027         IF  not %found(TICMP01L);
097100141027           ErrMessage  = *on;
097200141027           ErrGenerico = *on;
097300141027           PosCurNCM   = *on;
097400141027           V02msg = Msg(02);
097500141027           leavesr;
097600141027         ENDIF;
097700141027       //?Decodifico Campagna Commerciale
097800141027         V02des = CMPdes;
097900150505       ////?Campagna Commerciale già finita
098000150505         //IF  CMPdfc < Oggi;
098100150505         //  ErrMessage  = *on;
098200150505         //  ErrGenerico = *on;
098300150505         //  PosCurNCM   = *on;
098400150505         //  V02msg = %trim(Msg(02)) + '. Già chiusa';
098500150505         //  leavesr;
098600150505         //ENDIF;
098700141027       //?In carico all'Utente
098800141029         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
098900141027           ErrMessage  = *on;
099000141027           ErrGenerico = *on;
099100141027           PosCurNCM   = *on;
099200141027           V02msg = %trim(Msg(02)) + '. Non in gestione';
099300141027           leavesr;
099400141027         ENDIF;
099500170303       //?Controllo la gestione degli obiettivi per visualizzare
099600170303       //?obiettivi o % delta
099700170303         IF  Oggi >= CMPdigo and Oggi <= CMPdfgo;
099800170303           VisDelta     = *off;
099900170303         ELSE;
100000170303           VisDelta     = *on;
100100170303         ENDIF;
100200141027
100300141027       //?Distretto
100400141103         w001a = V02cdi;
100500141030         clear w050a;
100600141030         exsr Distretto;
100700141103         V02cdi = w001a;
100800141030         V02cdid = w050a;
100900141030         V02msg = wmsg;
101000141030         IF  ErrGenerico;
101100141030           leavesr;
101200141030         ENDIF;
101300141027
101400141027       //?Area
101500141103         w003a = V02car;
101600141030         clear w050a;
101700141030         exsr Area;
101800141103         V02car = w003a;
101900141030         V02card = w050a;
102000141030         V02msg = wmsg;
102100141030         IF  ErrGenerico;
102200141030           leavesr;
102300141030         ENDIF;
102400141030       //?Se presente anche il distretto devono essere congruenti
102500141030         IF  V02cdi <> *blanks and V02car <> *blanks;
102600141030           chain (V02cdi:w0030) AZORG02L;
102700141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
102800141030             ErrGenerico = *on;
102900141030             ErrMessage  = *on;
103000141030             PosCurCAR   = *on;
103100141030             V02msg = %trim(Msg(04)) + '. Non congruente con il distretto';
103200141030             leavesr;
103300141030           ENDIF;
103400141030         ENDIF;
103500141027
103600141027       //?Filiale Commerciale
103700141103         w003a = V02fil;
103800141030         clear w050a;
103900141030         exsr Filiale;
104000141103         V02fil = w003a;
104100141030         V02fild = w050a;
104200141030         V02msg = wmsg;
104300141030         IF  ErrGenerico;
104400141030           leavesr;
104500141030         ENDIF;
104600141027
104700141027       //?Commerciale Unificante
104800141103         w007a = V02cmm;
104900141030         clear w050a;
105000141030         exsr Commerciale;
105100141103         V02cmm = w007a;
105200141030         V02cmmd = w050a;
105300141030         V02msg = wmsg;
105400141030         IF  ErrGenerico;
105500141030           leavesr;
105600141030         ENDIF;
105700141027
105800141027       //?Cliente
105900141107         w007a = V02ksu;
106000141030         clear w050a;
106100141030         exsr Cliente;
106200141107         V02ksu = w007a;
106300141030         V02rag = w050a;
106400141030         V02msg = wmsg;
106500141030         IF  ErrGenerico;
106600141030           leavesr;
106700141030         ENDIF;
106800150611
106900150611       //?Se utente NON di sede a abilitato a più di un'area e no
107000150611       //?abilitazione 'AZ' o 'DI'
107100150611       //?richiedo obbligatoria l'area
107200150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
107300150611             V02car = *blanks and V02cmm = *blanks and
107400150611             V02ksu = *blanks and car(2) > *zeros;
107500150611           ErrMessage  = *on;
107600150611           ErrGenerico = *on;
107700150611           PosCurCAR   = *on;
107800150611           V02msg = Msg(25);
107900150611           leavesr;
108000150611         ENDIF;
108100141024
108200141024       //?Importanza Cliente
108300141030         w001a = V02clv1;
108400141030         clear w050a;
108500141030         exsr ImpCliente;
108600141030         V02clv1 = w001a;
108700141030         V02msg = wmsg;
108800141030         IF  ErrGenerico;
108900141030           PosCurCLV1 = *on;
109000141030           leavesr;
109100141030         ENDIF;
109200141030         w001a = V02clv2;
109300141030         clear w050a;
109400141030         exsr ImpCliente;
109500141030         V02clv2 = w001a;
109600141030         V02msg = wmsg;
109700141030         IF  ErrGenerico;
109800141030           PosCurCLV2 = *on;
109900141030           leavesr;
110000141030         ENDIF;
110100141030         w001a = V02clv3;
110200141030         clear w050a;
110300141030         exsr ImpCliente;
110400141030         V02clv3 = w001a;
110500141030         V02msg = wmsg;
110600141030         IF  ErrGenerico;
110700141030           PosCurCLV3 = *on;
110800141030           leavesr;
110900141030         ENDIF;
111000141030         w001a = V02clv4;
111100141030         clear w050a;
111200141030         exsr ImpCliente;
111300141030         V02clv4 = w001a;
111400141030         V02msg = wmsg;
111500141030         IF  ErrGenerico;
111600141030           PosCurCLV4 = *on;
111700141030           leavesr;
111800141030         ENDIF;
111900141030         w001a = V02clv5;
112000141030         clear w050a;
112100141030         exsr ImpCliente;
112200141030         V02clv5 = w001a;
112300141030         V02msg = wmsg;
112400141030         IF  ErrGenerico;
112500141030           PosCurCLV5 = *on;
112600141030           leavesr;
112700141030         ENDIF;
112800141028
112900141028       //?Delta
113000141029         IF  V02delda > *zeros or V02dela <> *zeros;
113100141028         //?Obbligatori tutti e 2 se immesso uno dei due
113200141028           IF  V02delda > *zeros and V02dela = *zeros;
113300141028             ErrMessage  = *on;
113400141028             ErrGenerico = *on;
113500141028             PosCurDELA  = *on;
113600150216             V02msg      = Msg(09);
113700141028             leavesr;
113800141028           ENDIF;
113900141028           IF  V02delda = *zeros and V02dela > *zeros;
114000141028             ErrMessage  = *on;
114100141028             ErrGenerico = *on;
114200141028             PosCurDELda = *on;
114300150216             V02msg      = Msg(09);
114400141028             leavesr;
114500141028           ENDIF;
114600141028         //?Controllo congruenza tra "DAL" e "AL"
114700141028           wdelda = V02delda;
114800141028           wdela  = V02dela;
114900141029           IF  V02deldas = '-';
115000141028             wdelda = wdelda * -1;
115100141028           ENDIF;
115200141029           IF  V02delas = '-';
115300141028             wdela = wdela * -1;
115400141028           ENDIF;
115500141028           IF  wdelda > wdela;
115600141028             ErrMessage  = *on;
115700141028             ErrGenerico = *on;
115800141028             PosCurDELda = *on;
115900150216             V02msg      = Msg(10);
116000141028             leavesr;
116100141028           ENDIF;
116200141028         ENDIF;
116300141121
116400141121       //?Obiettivi
116500150119         wfobj  = V02obj;
116600141127         wfobj1 = V02obj1;
116700141127         wfobj2 = V02obj2;
116800150119         wfformula = V02formula;
116900150213         wfobj3 = V02obj3;
117000150213         wobj3da = V02obj3da;
117100150213         wobj3a  = V02obj3a;
117200150213         IF  V02obj3das = '-';
117300150213           wobj3da = wobj3da * -1;
117400150213         ENDIF;
117500150213         IF  V02obj3as = '-';
117600150213           wobj3a = wobj3a * -1;
117700150213         ENDIF;
117800150128         exsr ContrObj;
117900141127         V02obj1 = wfobj1;
118000141127         V02obj2 = wfobj2;
118100150119         V02formula = wfformula;
118200150213         V02obj3 = wfobj3;
118300150213         V02obj3da = %abs(wobj3da);
118400150213         V02obj3a  = %abs(wobj3a);
118500141127         V02msg = wmsg;
118600141127         IF  ErrGenerico;
118700141127           leavesr;
118800141127         ENDIF;
118900150302
119000150302       //?Trattative
119100150302         w002a = V02ttr;
119200150302         clear w050a;
119300150302         exsr SceltaTtr;
119400150302         V02ttr = w002a;
119500150302         V02msg = wmsg;
119600150302         IF  ErrGenerico;
119700150302           leavesr;
119800150302         ENDIF;
119900141024
120000141024       ENDSR;
120100141028
120200141028       //--------------------------------------------------------------
120300141028       //?Gestione videata S03.
120400141028       //--------------------------------------------------------------
120500141028       BEGSR GesS03;
120600141028
120700141028       //?Inizializzazione videata
120800141029         IF  wInzS03;
120900141028           exsr InzS03;
121000141029           wInzS03 = *off;
121100141118           IF  wMaxNrr;
121200141118             leavesr;
121300141118           ENDIF;
121400141028         ENDIF;
121500141028
121600141028       //?Visualizzazione del SFL (se ci sono dati)
121700141117         SflDsp = (S03nrr <= *zeros);
121800141104
121900141104       //?Posizionamento cursore
122000141112         V03rcd = V03csr;
122100141111
122200141111       //?Se utente di sede abilito anche l'aggiunta di nuovi clienti
122300141111       //?con F10
122400150507       //?(SE Campagna NON scaduta)?
122500150505         AbilitaF10 = (DUTpou = 046  and  CMPdfc >= Oggi);
122600141127
122700150119       //?Se utente di sede e richiesto
122800150128       //?Obiettivo Inizio = Obiettivo Proposto
122900150507       //?e SE Campagna NON scaduta?
123000141127       //?abilito F13 per conferma obiettivo proposto
123100150212         AbilitaF13 = (DUTpou = 046 and V04obj = 'P' and
123200150507                       CMPdfc >= Oggi  and
123300150216                      ((V04formula = Formula(1) and
123400150216                       ((V04obj1 = 'I' and V04obj2 = 'P') or
123500150216                        (V04obj1 = 'P' and V04obj2 = 'I'))) or
123600150216                       (V04formula = Formula(2) and
123700150216                        V04obj1 = 'P' and V04obj2 = 'I')));
123800141028
123900141028       //?Emissione Testata e Piede con tasti funzionali abilitati
124000150112         write  KC01T01;
124100150112         write  KC01P03;
124200141028
124300141028       //?Emissione videata
124400150112         exfmt  KC01C03;
124500141028         reset ErrMessage;
124600141028         reset ErrGenerico;
124700141028         clear V03msg;
124800141028
124900141028         SELECT;
125000141030
125100141030       //?- F01=Guida Fasi
125200141030           WHEN  dsp_aid = c_F01;
125300141030             exsr F01S03;
125400141028
125500141028       //?- F03=Fine
125600141028           WHEN  dsp_aid = c_F03;
125700141029             exsr F03D02;
125800141030
125900141030       //?- F05=Altre parzializzazioni
126000141030           WHEN  dsp_aid = c_F05;
126100141030             exsr F05S03;
126200141030
126300141030       //?- F08=Ord.X Imp.Cliente
126400141030           WHEN  dsp_aid = c_F08;
126500141030             exsr F08S03;
126600141111
126700141111       //?- F10=Aggiunta Cliente
126800141111           WHEN  dsp_aid = c_F10;
126900141111             exsr F10S03;
127000141028
127100141028       //?- F12=Ritorno
127200141028           WHEN  dsp_aid = c_F12;
127300141028             exsr F12S03;
127400141127
127500141127       //?- F13=Conferma Obiettivo Proposto
127600141127           WHEN  dsp_aid = c_F13;
127700141127             exsr F13S03;
127800141028
127900141028       //?Invio
128000141028           OTHER;
128100141028             IF  V03csr > *zero;
128200141028               exsr OpzS03;
128300141028               IF  ErrGenerico;
128400141028                 leavesr;
128500141028               ENDIF;
128600141028             ENDIF;
128700141028
128800141028         ENDSL;
128900141028
129000141028       ENDSR;
129100141028
129200141028       //--------------------------------------------------------------
129300141028       //?Inizializzazione videata S03.
129400141028       //--------------------------------------------------------------
129500141029       BEGSR InzS03;
129600141028
129700141028         EndS03 = *off;
129800141112         clear V03tot;
129900150116         TroppiRcd = *off;
130000141202
130100141202       //?Imposto la descrizione della campagna
130200150121         V01des = CMPdes;
130300150212
130400150212       //?Periodo Confronto Fatturazione
130500150212         V03aammcf = %subst(CMPflo:5:2) + '/' + %subst(CMPflo:1:4);
130600170302
130700170302       //?Periodo Statistica
130800170302         V03aammdel = %editc(wmese:'X') + '/' + %editc(wanno:'X');
130900141028
131000141028       //?Pulizia subfile
131100141028         exsr PulS03;
131200150212
131300150212       //?Imposto i dati per videata delle parzializzazioni
131400150212       //?e per caricamento subfile
131500150212         IF  wInzD04;
131600150212           exsr InzD04;
131700150212           wInzD04 = *off;
131800150212         ENDIF;
131900141028
132000141028       //?Caricamento subfile
132100141028         exsr Ries03;
132200141118
132300141118       //?Se ho superato il numero massimo di rcd da caricare esco
132400141118         IF  wMaxNrr;
132500141124           clear V03tot;
132600141118           Video = 'D5';
132700141118           wInzD05 = *on;
132800141118           leavesr;
132900141118         ENDIF;
133000141112
133100141117         rrnlast = S03nrr;
133200141112
133300141112       //?Faccio l'ordinamento
133400141112         SELECT;
133500141112       //?se ordina x RAG devo ordinare per CLV
133600141112         WHEN  VisOrdRAG;
133700141112           exsr Ordina_x_CLV;
133800141112       //?se ordina x CLV devo ordinare per RAG
133900141112         WHEN  VisOrdCLV;
134000141112           exsr Ordina_x_RAG;
134100141112         ENDSL;
134200141112
134300141112         SflEnd = *on;
134400141112
134500141112       //?Imposto il posizionamento al primo rcd
134600141112         IF  S03nrr > 0;
134700141112           V03rcd = 1;
134800141112         ELSE;
134900141112           clear V03rcd;
135000141112         ENDIF;
135100141112
135200141112         V03csr = V03rcd;
135300141117
135400150213       //?Imposto il n. rcd del subfile solo se il nrr savlato
135500150213       //?è <= all'ultimo nrr del subfile appena caricato
135600150213       //?se no imposto l'ultimo nrr
135700150213         IF  sav_S03nrr > 0;
135800150213           IF  sav_S03nrr <= rrnlast;
135900150213             V03csr = sav_S03nrr;
136000150213             clear sav_S03nrr;
136100150213           ELSE;
136200150213             V03csr = rrnlast;
136300150213           ENDIF;
136400141117         ENDIF;
136500141028
136600141028       ENDSR;
136700141028
136800141028       //--------------------------------------------------------------
136900141028       //?Pulizia Subfile S03.
137000141028       //--------------------------------------------------------------
137100141028       BEGSR PulS03;
137200141028
137300141028       //?Pulizia subfile
137400141028         SflDsp    = *on;
137500141028         SflDspCtl = *on;
137600150112         write KC01C03;
137700141028         SflDspCtl = *off;
137800141028         SflEnd    = *off;
137900141028
138000141112         clear V03rcd;
138100141028         clear V03csr;
138200141028         clear S03nrr;
138300141028         clear V03msg;
138400141028
138500141028         ErrMessage  = *off;
138600141028         ErrGenerico = *off;
138700141028
138800141028         WindDspF = IndDspF;
138900141028         reset WindDspFb;
139000141028         IndDspF  = WindDspF;
139100141028
139200141104       ENDSR;
139300141028
139400141028       //--------------------------------------------------------------
139500141028       //?Caricamento Subfile S03.
139600141028       //--------------------------------------------------------------
139700141028       BEGSR RieS03;
139800141028
139900141028         EndS03 = *off;
140000141103         wMaxNrr = *off;
140100141028
140200141028       //?Preparo Stringa Sql
140300141113         exsr PreparaSQL;
140400141028
140500141028       //?Dichiarazione cursore
140600141028         exec sql
140700141028           prepare S1   from :wSQL;
140800141028         exec sql
140900141124           declare WRK cursor for S1;
141000141028
141100141028         //?Apertura del cursore
141200141028         exec sql
141300141028           open WRK;
141400141028
141500141028         IF sqlcode < 0;
141600141028           EndS03 = *on;
141700141113           exec sql close WRK;
141800141113           leavesr;
141900141028         ENDIF;
142000141028
142100141028         DOW  not EndS03;
142200141028           exec sql
142300150115             fetch next from WRK into :TICMI00F, :CMCufe, :CMCcch;
142400141028           IF sqlcod = 100 or sqlcod < 0;
142500141028             EndS03 = *on;
142600141028             leave;
142700141028           ENDIF;
142800150303
142900150303         //?Controllo se ok il rcd per la selezione sulle trattative
143000150303           IF  V04ttr <> *blanks and V04ttr <> 'NO' and V04ttr <> 'SI';
143100150303             exsr CTRttr;
143200150303             IF  not RecOK;
143300150303               iter;
143400150303             ENDIF;
143500150303           ENDIF;
143600141124
143700141124         //?Controllo se ok il rcd
143800141124         //?per il "di cui" obiettivo
143900150212           IF  V04obj1 <> *blanks and V04obj2 <> *blanks;
144000141124             exsr CTRrec;
144100141124             IF  not RecOK;
144200141124               iter;
144300141124             ENDIF;
144400141124           ENDIF;
144500150213         //?per la parzializzazione dal al Obiettivo/Andamento
144600150213           IF  V04obj3 <> *blanks;
144700150213             exsr CTRrec1;
144800150213             IF  not RecOK;
144900150213               iter;
145000150213             ENDIF;
145100150213           ENDIF;
145200141028
145300141028         //?Carico i dati nel subfile
145400141028           exsr CarS03;
145500141028
145600141028         ENDDO;
145700141028
145800141028         exec sql
145900141028           close WRK;
146000141124
146100141124         V03tot = S03nrr;
146200141028
146300141028       ENDSR;
146400141028
146500141028       //--------------------------------------------------------------
146600141028       //?Preparo la Stringa SQL.
146700141028       //--------------------------------------------------------------
146800141028       BEGSR PreparaSQL;
146900141113
147000150303       //?Scelta trattative
147100141113         SELECT;
147200150303       //?Con trattativa  (fase TR o TF)
147300150303         WHEN  V04ttr <> 'NO' and V04ttr <> *blanks;
147400141113           wSQL = 'with SEL001 as ' +
147500141113                  '(select max (digits(CMFdfa) concat '+
147600141113                  'digits(CMFhfc)) as time, ' +
147700141113                  'CMFncm, CMFksu, CMFksc, CMFcpo ' +
147800141113                  'from TICMF00F ' +
147900150223                  'where CMFacm in('+ '''' + FaseObjTtr + '''' +
148000150223                  ', ' + '''' + FaseObjTf + '''' + ')' +
148100150212                  ' and CMFncm = ' + %editc(VH4ncm:'X') +
148200141113                  ' group by CMFncm, CMFksu, CMFksc, CMFcpo)' +
148300150115                  'select TICMI00F.*, CMCufe, CMCcch from SEL001 ' +
148400141113                  'join TICMI00F on ' +
148500141113                  'CMFncm = CMIncm and CMFksu = CMIksu and ' +
148600141113                  'CMFksc = CMIksc and CMFcpo = CMIcpo ' +
148700141113                  'join TICMC00F on ' +
148800141113                  'CMIncm = CMCncm and CMIksu = CMCksu and ' +
148900150303                  'CMIksc = CMCksc and CMIcpo = CMCcpo ' +
149000150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
149100141113           exsr AggSelSQL;
149200150303
149300150303       //?Nessuna trattativa
149400150303         WHEN  V04ttr = 'NO';
149500150303           wSQL = 'with SEL001 as ' +
149600150303                  '(select TICMI00F.* from TICMI00F '+
149700150303                  'exception join TICMF00F on ' +
149800150303                  'CMIncm = CMFncm and CMIksu = CMFksu and ' +
149900150303                  'CMIksc = CMFksc and CMIcpo = CMFcpo and ' +
150000150303                  'CMFacm in(' + '''' + FaseObjTtr + '''' +
150100150303                  ', ' + '''' + FaseObjTf + '''' + ') ' +
150200150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
150300150303           exsr AggSelSQL;
150400150303           wSQL += ') ';
150500150303           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
150600150303                   'from SEL001, TICMC00F ' +
150700150303                   'where ' +
150800150303                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
150900150303                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
151000141113
151100141113         OTHER;
151200150303       //?"  " = Nessuna scelta
151300141113           wSQL = 'with SEL001 as ' +
151400141113                  '(select TICMI00F.* from TICMI00F ' +
151500150212                  ' where CMIncm = ' + %editc(VH4ncm:'X');
151600141113           exsr AggSelSQL;
151700141113           wSQL += ') ';
151800150115           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
151900141113                   'from SEL001, TICMC00F ' +
152000141113                   'where ' +
152100141113                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
152200141113                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
152300141113         ENDSL;
152400150119
152500150213       //?Clienti Bloccati
152600150213         IF  V04cli = 'E';
152700150119           wSQL += ' and CMCcch = ' + '''   ''';
152800150119         ENDIF;
152900150213         IF  V04cli = 'I';
153000150213           wSQL += ' and CMCcch <> ' + '''   ''';
153100150213         ENDIF;
153200150213         IF  V04cli = *blanks;
153300150213         ENDIF;
153400150119
153500150119       //?Clienti Nuovi
153600150212         IF  V04ncli = 'S';
153700150119           wSQL += ' and CMIcln = ' + '''N''' +
153800150119                   ' and CMCcch = ' + '''   ''';
153900150119         ENDIF;
154000141113
154100141113         SELECT;
154200150128       //?Richiesta parzializzazione obiettivo Inizio
154300150212         WHEN  V04obj = 'I';
154400141113           wSQL += ' and CMCufe = ' + '''' + FaseObj + '''';
154500141113       //?Richiesta parzializzazione obiettivo Proposto
154600150212         WHEN  V04obj = 'P';
154700141113           wSQL += ' and CMCufe = ' + '''' + FaseObjProp + '''';
154800141113       //?Richiesta parzializzazione obiettivo Finale
154900150212         WHEN  V04obj = 'F';
155000141113           wSQL += ' and CMCufe = ' + '''' + FaseObjFine + '''';
155100141113         ENDSL;
155200141028
155300141028         wSQL += ' for fetch only';
155400141028
155500141028       ENDSR;
155600141113
155700141113       //--------------------------------------------------------------
155800141113       //?Aggiungo le selezioni alla stringa SQL.
155900141113       //--------------------------------------------------------------
156000141113       BEGSR AggSelSQL;
156100141113
156200141113         wclv = *off;
156300141119
156400141119       //?Se non sono richiesti Distretto/Area/Filiale DEVO caricare solo i clienti
156500141119       //?abilitati all'utente
156600150212         IF  V04cdi = *blanks and VH4car = *zeros and VH4fil = *zeros;
156700141119           xx = 1;
156800141119           Primo = *off;
156900141119           wSQL += ' and CMIfcm in(';
157000141119           FOR  xx by 1 to %elem(POG);
157100141119             IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
157200141119               IF  Primo;
157300141119                 wsql += ', ';
157400141119               ELSE;
157500141119                 Primo = *on;
157600141119               ENDIF;
157700141119               wsql += POG(xx);
157800141119             ENDIF;
157900141119           ENDFOR;
158000141119           wsql += ')';
158100141119         ENDIF;
158200141113
158300141113       //?Distretto
158400150212         IF  V04cdi <> *blanks;
158500150212           wSQL += ' and CMIdcm = ' + '''' + V04cdi + '''';
158600141113         ENDIF;
158700141113       //?Area
158800150212         IF  VH4car > *zeros;
158900150212           wSQL += ' and CMIacm = ' + %editc(VH4car:'X');
159000141113         ENDIF;
159100141113       //?Filiale
159200150212         IF  VH4fil > *zeros;
159300150212           wSQL += ' and CMIfcm = ' + %editc(VH4fil:'X');
159400141113         ENDIF;
159500141113       //?Commerciale
159600150212         IF  VH4cmm > *zeros;
159700150212           wSQL += ' and CMIcmm = ' + %editc(VH4cmm:'X');
159800141113         ENDIF;
159900141113       //?Cliente
160000150212         IF  VH4ksu > *zeros;
160100150212           wSQL += ' and CMIksu = ' + %editc(VH4ksu:'X');
160200141113         ENDIF;
160300141113       //?Importanza Cliente
160400150212         IF  V04clv1 <> *blanks;
160500150212           wSQL += ' and CMIclv in(' + '''' + V04clv1 + '''';
160600141113           wclv = *on;
160700141113         ENDIF;
160800150212         IF  V04clv2 <> *blanks and wclv;
160900150212           wSQL += ',' + '''' + V04clv2 + '''';
161000141113           wclv = *on;
161100141113         ENDIF;
161200150212         IF  V04clv2 <> *blanks and not wclv;
161300150212           wSQL += ' and CMIclv in(' + '''' + V04clv2 + '''';
161400141113           wclv = *on;
161500141113         ENDIF;
161600150212         IF  V04clv3 <> *blanks and wclv;
161700150212           wSQL += ',' + '''' + V04clv3 + '''';
161800141113           wclv = *on;
161900141113         ENDIF;
162000150212         IF  V04clv3 <> *blanks and not wclv;
162100150212           wSQL += ' and CMIclv in(' + '''' + V04clv3 + '''';
162200141113           wclv = *on;
162300141113         ENDIF;
162400150212         IF  V04clv4 <> *blanks and wclv;
162500150212           wSQL += ',' + '''' + V04clv4 + '''';
162600141113           wclv = *on;
162700141113         ENDIF;
162800150212         IF  V04clv4 <> *blanks and not wclv;
162900150212           wSQL += ' and CMIclv in(' + '''' + V04clv4 + '''';
163000141113           wclv = *on;
163100141113         ENDIF;
163200150212         IF  V04clv5 <> *blanks and wclv;
163300150212           wSQL += ',' + '''' + V04clv5 + '''';
163400141113           wclv = *on;
163500141113         ENDIF;
163600150212         IF  V04clv5 <> *blanks and not wclv;
163700150212           wSQL += ' and CMIclv in(' + '''' + V04clv5 + '''';
163800141113           wclv = *on;
163900141113         ENDIF;
164000141113         IF  wclv;
164100141113           wSQL += ')';
164200141113         ENDIF;
164300141113       //?Delta
164400150212         IF  v04delda <> *zeros and V04dela <> *zeros;
164500141119           wSQL += ' and CMIpde between ' + %editc(wdelda:'O') +
164600141119                   ' and ' + %editc(wdela:'O');
164700141113         ENDIF;
164800141113
164900141113       ENDSR;
165000150303
165100150303       //--------------------------------------------------------------
165200150303       //?Controllo se OK il record per selezione trattative.
165300150303       //--------------------------------------------------------------
165400150303       BEGSR CTRttr;
165500150303
165600150303         RecOk = *off;
165700150303         clear wfase;
165800150303         wflgp = V04ttr;
165900150303         wtpric = 'P';
166000150303
166100150303         exsr CallTRKC76;
166200150303         IF  OKC76err <> *blanks;
166300150303           leavesr;
166400150303         ENDIF;
166500150303
166600150303         RecOK = *on;
166700150303
166800150303       ENDSR;
166900141124
167000141124       //--------------------------------------------------------------
167100141124       //?Controllo se OK il record.
167200141124       //--------------------------------------------------------------
167300141124       BEGSR CTRrec;
167400141124
167500141124         RecOk = *off;
167600150128         clear wvalobj1;
167700150128         clear wvalobj2;
167800141124
167900141124       //?Cerco il "di cui" 1
168000141124         SELECT;
168100150128       //?Inizio
168200150212         WHEN  V04obj1 = 'I';
168300150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
168400150218           IF  not %found(TICMF01L);
168500141126             leavesr;
168600141126           ENDIF;
168700150218           wvalobj1 = CMFpea;
168800141124       //?Proposto
168900150212         WHEN  V04obj1 = 'P';
169000150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
169100150218           IF  not %found(TICMF01L);
169200141126             leavesr;
169300141126           ENDIF;
169400150218           wvalobj1 = CMFpea;
169500141124       //?Finale
169600150212         WHEN  V04obj1 = 'F';
169700150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
169800150218           IF  not %found(TICMF01L);
169900141126             leavesr;
170000141126           ENDIF;
170100150218           wvalobj1 = CMFpea;
170200150119       //?Trattativa
170300150212         WHEN  V04obj1 = 'T';
170400150223           wfase = FaseObjTtr;
170500150303           wtpric = 'F';
170600150223           exsr CallTRKC76;
170700150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
170800150223             leavesr;
170900150223           ENDIF;
171000150223           wvalobj1 = OKC76pea;
171100150119       //?Confronto Fatturazione
171200150212         WHEN  V04obj1 = 'C';
171300150223           wfase = FaseObjCf;
171400150303           wtpric = 'F';
171500150223           exsr CallTRKC76;
171600150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
171700150223             leavesr;
171800150223           ENDIF;
171900150223           wvalobj1 = OKC76pea;
172000141124         ENDSL;
172100141124
172200141124       //?Cerco il "di cui" 2
172300141124         SELECT;
172400150128       //?Inizio
172500150212         WHEN  V04obj2 = 'I';
172600150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
172700150218           IF  not %found(TICMF01L);
172800141126             leavesr;
172900141126           ENDIF;
173000150218           wvalobj2 = CMFpea;
173100141124       //?Proposto
173200150212         WHEN  V04obj2 = 'P';
173300150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
173400150218           IF  not %found(TICMF01L);
173500141126             leavesr;
173600141126           ENDIF;
173700150218           wvalobj2 = CMFpea;
173800141124       //?Finale
173900150212         WHEN  V04obj2 = 'F';
174000150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
174100150218           IF  not %found(TICMF01L);
174200141126             leavesr;
174300141126           ENDIF;
174400150218           wvalobj2 = CMFpea;
174500150119       //?Trattativa
174600150212         WHEN  V04obj2 = 'T';
174700150223           wfase = FaseObjTtr;
174800150303           wtpric = 'F';
174900150223           exsr CallTRKC76;
175000150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
175100150223             leavesr;
175200150223           ENDIF;
175300150223           wvalobj2 = OKC76pea;
175400150119       //?Confronto Fatturazione
175500150212         WHEN  V04obj2 = 'C';
175600150223           wfase = FaseObjCf;
175700150303           wtpric = 'F';
175800150223           exsr CallTRKC76;
175900150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
176000150223             leavesr;
176100150223           ENDIF;
176200150223           wvalobj2 = OKC76pea;
176300141124         ENDSL;
176400141124
176500141127       //?controllo i 2 obiettivi
176600141127         SELECT;
176700150212         WHEN  V04formula = Formula(1) and wvalobj1 = wvalobj2;
176800141127           RecOK = *on;
176900150212         WHEN  V04formula = Formula(2) and wvalobj1 > wvalobj2;
177000141127           RecOK = *on;
177100150212         WHEN  V04formula = Formula(3) and wvalobj1 < wvalobj2;
177200141127           RecOK = *on;
177300150212         WHEN  V04formula = Formula(4) and wvalobj1 >= wvalobj2;
177400150119           RecOK = *on;
177500150212         WHEN  V04formula = Formula(5) and wvalobj1 <= wvalobj2;
177600141127           RecOK = *on;
177700150212         WHEN  V04formula = Formula(6) and wvalobj1 <> wvalobj2;
177800141127           RecOK = *on;
177900141127         ENDSL;
178000141124
178100141124       ENDSR;
178200150213
178300150213       //--------------------------------------------------------------
178400150213       //?Controllo se OK il record.
178500150213       //--------------------------------------------------------------
178600150213       BEGSR CTRrec1;
178700150213
178800150213         RecOk = *off;
178900150213         clear wvalobj3;
179000150213
179100150213       //?Cerco Obiettivo/Andamento richiesto
179200150213         SELECT;
179300150213       //?Inizio
179400150213         WHEN  V04obj3 = 'I';
179500150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
179600150218           IF  not %found(TICMF01L);
179700150213             leavesr;
179800150213           ENDIF;
179900150218           wvalobj3 = CMFpea;
180000150213       //?Proposto
180100150213         WHEN  V04obj3 = 'P';
180200150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
180300150218           IF  not %found(TICMF01L);
180400150213             leavesr;
180500150213           ENDIF;
180600150218           wvalobj3 = CMFpea;
180700150213       //?Finale
180800150213         WHEN  V04obj3 = 'F';
180900150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
181000150218           IF  not %found(TICMF01L);
181100150213             leavesr;
181200150213           ENDIF;
181300150218           wvalobj3 = CMFpea;
181400150213       //?Trattativa
181500150213         WHEN  V04obj3 = 'T';
181600150223           wfase = FaseObjTtr;
181700150303           wtpric = 'F';
181800150223           exsr CallTRKC76;
181900150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
182000150223             leavesr;
182100150213           ENDIF;
182200150223           wvalobj3 = OKC76pea;
182300150213       //?Confronto Fatturazione
182400150213         WHEN  V04obj3 = 'C';
182500150223           wfase = FaseObjCf;
182600150303           wtpric = 'F';
182700150223           exsr CallTRKC76;
182800150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
182900150223             leavesr;
183000150213           ENDIF;
183100150223           wvalobj3 = OKC76pea;
183200150213         ENDSL;
183300150213
183400150213       //?confronto con il "dal" - "al" richiesto
183500150213         IF  wvalobj3 >= wobj3da and wvalobj3 <= wobj3a;
183600150213           RecOK = *on;
183700150213         ENDIF;
183800150213
183900150213       ENDSR;
184000141028
184100141028       //--------------------------------------------------------------
184200141028       //?Carico i dati nel Subfile S03.
184300141028       //--------------------------------------------------------------
184400141028       BEGSR CarS03;
184500141028
184600150112         clear  KC01S03;
184700141127         PosCurOPZ = *off;
184800141028
184900141029       //?Imposto i campi da TICMI
185000141103         VS3clv = CMIclv;
185100141107         VS3ksu = CMIksu;
185200141107         VS3ksc = CMIksc;
185300141107         VS3cpo = CMIcpo;
185400150115       //?Imposto i campi da TICMC
185500141114         VS3ufe = CMCufe;
185600150115         VS3cch = CMCcch;
185700150115         clear VS3cchaut;
185800150115       //?Imposto se causale automatica
185900150115         IF  VS3cch <> *blanks;
186000150115           clear TIBS02DS;
186100150115           clear dECM;
186200150115           T02mod = 'C';
186300150115           T02cod = 'ECM';
186400150115           T02ke1 = VS3cch;
186500150115           T02sif = KNSIF;
186600150115           TNTBE_RicercaControllo  (kpjba : tibs02ds);
186700150115           dECM = T02uni;
186800150115           IF  §ECMaut <> *blanks;
186900150115             VS3cchaut = 'S';
187000150115           ENDIF;
187100150115         ENDIF;
187200150116         clear VS3tipocl;
187300141028         IF  CMIcln = 'N';
187400150116           VS3tipocl = '*';
187500141028         ENDIF;
187600150116         IF  CMCcch <> *blanks;
187700150213           VS3tipocl = 'B';
187800150116         ENDIF;
187900141028         clear TIBS69DS;
188000141028         clear ACOrag;
188100141028         I69kac = CMIksu;
188200141028         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
188300141103         VS3rag = ACOrag;
188400141103         VS3istat = CMIist;
188500141103         VS3del = CMIpde;
188600141103         VS3sped = CMInsp;
188700141103         VS3ric = CMIric;
188800141103         VS3pkgm = CMIpme;
188900141029
189000141029       //?Imposto i campi da TICMF
189100150128       //?Obiettivo inizio
189200141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
189300141113         IF  %found(TICMF01L);
189400170303           VS3hobj = CMFpea;
189500170303           VS3okobj = 'S';
189600141113         ENDIF;
189700141111       //?Obiettivo Proposto
189800141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
189900141113         IF  %found(TICMF01L);
190000170303           VS3hobjpro = CMFpea;
190100170303           VS3okobjp = 'S';
190200141113         ENDIF;
190300141029       //?Obiettivo Finale
190400141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
190500141113         IF  %found(TICMF01L);
190600170302           VS3hobjfin = CMFpea;
190700170303           VS3okobjf = 'S';
190800141113         ENDIF;
190900150220       //?Aumento da Trattativa
191000150220         wfase = FaseObjTtr;
191100150303         wtpric = 'F';
191200150220         exsr CallTRKC76;
191300150220         IF  OKC76err = *blanks;
191400150220           VS3esitotr = OKC76flag;
191500150223           VS3objttr = OKC76peaa;
191600150224           IF  OKC76anno > 0;
191700150224             VS3decotr = (OKC76mese * 10000) + OKC76anno;
191800150224           ENDIF;
191900150220           VS3nrv = OKC76nrv;
192000150225           VS3forza = OKC76forza;
192100150220         ENDIF;
192200141029       //?% Confronto Fatturazione
192300150220         wfase = FaseObjCf;
192400150303         wtpric = 'F';
192500150220         exsr CallTRKC76;
192600150220         IF  OKC76err = *blanks;
192700150223           VS3objcf = OKC76peaa;
192800170302           VS3hobjcf = OKC76pea;
192900150220         ENDIF;
193000141029
193100141029       //?Imposto descrizione del commerciale
193200141029         clear CMMdes;
193300141029         chain CMIcmm AZCMM01L;
193400141029         IF  %found(AZCMM01L);
193500141103           VS3cmmd = CMMdes;
193600141029         ENDIF;
193700141117
193800141117       //?Imposto ordinamento importanza cliente
193900141117         clear xx;
194000141117         xx = %lookup(VS3clv:skIC);
194100141117         IF  xx > 0;
194200141117           VS3ord = skIC_ord(xx);
194300141117         ELSE;
194400141117           clear VS3ord;
194500141117         ENDIF;
194600170302
194700170302       //?Calcolo delta e spedizioni
194800170302         exsr CallTRKC78;
194900170309         IF  OKC78err = *blanks;
195000170303           VS3hdelta = OKC78delta;
195100170303           VS3hnsped = OKC78sped;
195200170303           VS3okdelta = 'S';
195300170302         ENDIF;
195400141028
195500141029         S03nrr += 1;
195600141124
195700141124       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
195800141124         IF  S03nrr >= 9999;
195900141124           wMaxNrr = *on;
196000141124           EndS03 = *on;
196100141124           leavesr;
196200141124         ENDIF;
196300141124
196400150112         write  KC01S03;
196500141028
196600141028       ENDSR;
196700141030
196800141030       //--------------------------------------------------------------
196900141030       //?Gestione tasto funzionale F01 da videata S03
197000141111       //?F01=Campagna
197100141030       //--------------------------------------------------------------
197200141030       BEGSR F01S03;
197300141112
197400150112         clear TRKC02DS;
197500150112         IKC02ric = 'I';
197600150212         IKC02ncm = VH4ncm;
197700150112         trkc02r (kpjba:trkc02ds);
197800141030
197900141030       ENDSR;
198000141030
198100141030       //--------------------------------------------------------------
198200141030       //?Gestione tasto funzionale F05 da videata S03
198300141030       //?F05=Altre Parzializzazioni
198400141030       //--------------------------------------------------------------
198500141030       BEGSR F05S03;
198600141030
198700141030       //?Videata per nuove parzializzazioni
198800141030         Video = 'D4';
198900141030
199000141030       ENDSR;
199100141030
199200141030       //--------------------------------------------------------------
199300141030       //?Gestione tasto funzionale F08 da videata S03
199400141030       //?F08=Ord. X Imp.Cliente
199500141030       //--------------------------------------------------------------
199600141030       BEGSR F08S03;
199700141030
199800141111       //?F8 visualizzato richiede ordinamento x RAG
199900141111         IF  VisOrdRAG;
200000141111           exsr Ordina_x_RAG;
200100141030           leavesr;
200200141030         ENDIF;
200300141030
200400141030       //?F8 visualizzato richiede ordinamento x CLV
200500141030         IF  VisOrdCLV;
200600141030           exsr Ordina_x_CLV;
200700141030           leavesr;
200800141030         ENDIF;
200900141030
201000141030       ENDSR;
201100141111
201200141111       //--------------------------------------------------------------
201300141111       //?Gestione tasto funzionale F10 da videata S03
201400141111       //?F10=Aggiunta Cliente
201500141111       //--------------------------------------------------------------
201600141111       BEGSR F10S03;
201700141111
201800150112         clear TRKC06DS;
201900150212         IKC06ncm = VH4ncm;
202000150112         trkc06r (kpjba:TRKC06DS);
202100141111
202200141111       //?Ricarico il subfile
202300141111         wInzS03 = *on;
202400141111
202500141111       ENDSR;
202600141028
202700141028       //--------------------------------------------------------------
202800141028       //?Gestione tasto funzionale F12 da videata S03
202900141028       //?F12=Ritorno
203000141028       //--------------------------------------------------------------
203100141028       BEGSR F12S03;
203200141028
203300141028       //?Ritorno alle selezioni
203400141028         Video = 'D2';
203500141029         wInzD02 = *on;
203600150225         clear sav_s03nrr;
203700150223
203800150223       //?Se richiamato chiudo i file per il pgm TRKC76R
203900150223         IF  wTRKC76;
204000150223           clear TRKC76DS;
204100150223           IKC76ric = 'C';
204200150223           trkc76r (kpjba:TRKC76DS);
204300150223         ENDIF;
204400141028
204500141028       ENDSR;
204600141127
204700141127       //--------------------------------------------------------------
204800141127       //?Gestione tasto funzionale F13 da videata S03
204900141127       //?F13=Conferma Obiettivo proposto.
205000141127       //--------------------------------------------------------------
205100141127       BEGSR F13S03;
205200141127
205300150121         Video = 'D7';
205400150121         wInzD07 = *on;
205500141127
205600141127       ENDSR;
205700141030
205800141030       //--------------------------------------------------------------
205900141111       //?Ordinamento x RAG Subfile S03.
206000141030       //--------------------------------------------------------------
206100141111       BEGSR Ordina_x_RAG;
206200141030
206300141030         VisOrdCLV = *on;
206400141111         VisOrdRAG = *off;
206500141030
206600141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
206700141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
206800141030       // il campo è usato per selezionare i records dando un ordine a quelli
206900141030       // selezionati davanti ai non selezionati.
207000141030         clear QLGSCB;
207100141030         clear QLGSCB00;
207200141030
207300141118       // 2 campi chiave x Ragione Sociale - Codice (KSU)
207400141030         QLGNBRK = 1;
207500141030
207600141111       // imposto la posizione del RAG sul subfile e la lunghezza
207700141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
207800141117         QLGSP = 1 + %size(VS3clv) + %size(VS3ord)
207900150116                   + %size(VS3ksu) + %size(VS3tipocl);
208000141111         QLGSS = %SIZE(VS3rag);
208100141118         QLGDT = Carattere;
208200141030         QLGSO = Ascendente;
208300141030         QLGKL(1) = QLGSKL;
208400141118
208500141118       // imposto la posizione del KSU sul subfile e la lunghezza
208600141118       // l'ordinamento è su un campo numerico e deve essere ascendente
208700141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord);
208800141118         QLGSS = %SIZE(VS3ksu);
208900141118         QLGDT = Numerico;
209000141118         QLGSO = Ascendente;
209100141118         QLGKL(2) = QLGSKL;
209200141030
209300141030       // Load other sort parameters.
209400141030         QLGLB = 80 + 16 * MaxKey;
209500141030         QLGRL = %SIZE(sflrcd) - 1;
209600141030         QLGRT = 8;
209700141030         QLGOKL = 80;
209800141030         QLGLKE = 16;
209900141030         QLGLSS = 290;
210000141030
210100141030       // Initialize Sort I/O API fields.
210200141030         QLGRL00 = QLGRL;
210300141030         QLGRC00 = 1;
210400141030         clear QUSEI;
210500141030         QUSBPRV = %SIZE(QUSEC);
210600141030
210700141030       // First step - Initialize the sort routine.
210800141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
210900141030
211000141030       // Next step - Write records to I/O routine.
211100141030         QLGRT00 = Put;
211200141103         FOR  xx = 1 to rrnlast;
211300150112           chain xx KC01S03;
211400170303
211500170303           savokobj = VS3okobj;
211600170303           savokobjp = VS3okobjp;
211700170303           savokobjf = VS3okobjf;
211800170303           savokdelta = VS3okdelta;
211900141030
212000141030       // solo le righe con Selected = 'Y' sono riordinate,
212100141030       // quindi per fare un ordinamento di tutte le righe
212200141030       // metto 'Y' sempre.
212300141103           selected  = 'Y';
212400141103           clear QUSEI;
212500141103           QUSBPRV = %SIZE(QUSEC);
212600141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
212700141103         ENDFOR;
212800141030
212900141030       // Next step - Signal end of input, clear subfile for reload.
213000141030         QLGRT00 = EndPut;
213100141030         clear QUSEI;
213200141030         QUSBPRV = %SIZE(QUSEC);
213300141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
213400141030       // pulizia SFL
213500141030         exsr PulS03;
213600141030
213700141030       // Final step - Write the records back to the subfile.
213800141030         QLGRT00 = Get;
213900141103         FOR  xx = 1 to rrnlast;
214000141103           clear QUSEI;
214100141103           QUSBPRV = %SIZE(QUSEC);
214200141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
214300170303
214400170303       //?Imposto il subfile da emettere
214500170303           VS3okobj = savokobj;
214600170303           VS3okobjp = savokobjp;
214700170303           VS3okobjf = savokobjf;
214800170303           VS3okdelta = savokdelta;
214900141111           attrrag = visHI;
215000141103           clear attrclv;
215100170303           clear attrobj;
215200170302           clear attrobjcf;
215300170303           IF  not VisDelta;
215400170303             attrobj = visRIRED;
215500170303           ENDIF;
215600170303           IF  VS3objcf <> *blanks;
215700141103             attrobjcf = visRI;
215800141103           ENDIF;
215900170303           IF  VS3hobjcf < VS3hobjfin and VS3objcf <> *blanks;
216000170303             attrobjcf = visRIRED;
216100170303           ENDIF;
216200141121           IF  VS3esitotr <> *blanks;
216300141121             Visesito = *on;
216400141121           ELSE;
216500141121             Visesito = *off;
216600141121           ENDIF;
216700170303
216800170303       //?Imposto a video le colonne obiettivi/delta
216900170303         IF  not VisDelta;
217000170303           IF  VS3okobj = 'S';
217100170303             VS3obj = %editc(VS3hobj:'J');
217200170303           ENDIF;
217300170303           IF  VS3okobjp = 'S';
217400170303             VS3alfa = ' ' + %editc(VS3hobjpro:'J');
217500170303           ENDIF;
217600170303           IF  VS3okobjf = 'S';
217700170303             %subst(VS3alfa:11:7) = %editc(VS3hobjfin:'J');
217800170303           ENDIF;
217900170303         ELSE;
218000170303           IF  VS3okdelta = 'S';
218100170303             VS3obj = %editc(VS3hobjfin:'J');
218200170303             VS3alfa = %editc(VS3hdelta:'M') + ' ' +
218300170303                       %editc(VS3hnsped:'2');
218400170303           ENDIF;
218500170303         ENDIF;
218600170303
218700141103           s03nrr = xx;
218800150112           write KC01S03;
218900141103         ENDFOR;
219000141112
219100141117         rrnlast = S03nrr;
219200141112         IF  S03nrr > *zeros;
219300141112           V03rcd = 1;
219400141112           V03csr = 1;
219500141112         ELSE;
219600141112           clear V03rcd;
219700141112         ENDIF;
219800141030
219900141030       ENDSR;
220000141030
220100141030       //--------------------------------------------------------------
220200141030       //?Ordinamento x CLV Subfile S03.
220300141030       //--------------------------------------------------------------
220400141030       BEGSR Ordina_x_CLV;
220500141030
220600141030         VisOrdCLV = *off;
220700141111         VisOrdRAG = *on;
220800141030
220900141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
221000141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
221100141030       // il campo è usato per selezionare i records dando un ordine a quelli
221200141030       // selezionati davanti ai non selezionati.
221300141030         clear QLGSCB;
221400141030         clear QLGSCB00;
221500141030
221600150115       // 3 campi chiave x Importanza cliente, Fatturato e RAG
221700150115         QLGNBRK = 3;
221800141030
221900141118       // imposto la posizione del ORD CLV sul subfile e la lunghezza
222000141118       // l'ordinamento è su un campo numerico e deve essere discendente
222100141117         QLGSP = 1 + %size(VS3clv);
222200141117         QLGSS = %SIZE(VS3ord);
222300141117         QLGDT = Numerico;
222400141117         QLGSO = Discendente;
222500141030         QLGKL(1) = QLGSKL;
222600150115
222700150115       // imposto la posizione del Fatturato sul subfile e la lunghezza
222800150115       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
222900150115         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
223000150116                   + %size(VS3ksu) + %size(VS3tipocl)
223100150115                   + %size(VS3rag) + %size(VS3istat)
223200150115                   + %size(VS3del) + %size(VS3sped);
223300150115         QLGSS = %SIZE(VS3ric);
223400150115         QLGDT = Numerico;
223500150115         QLGSO = Discendente;
223600150115         QLGKL(2) = QLGSKL;
223700141118
223800141118       // imposto la posizione del RAG sul subfile e la lunghezza
223900141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
224000141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
224100150116                   + %size(VS3ksu) + %size(VS3tipocl);
224200141118         QLGSS = %SIZE(VS3rag);
224300141118         QLGDT = Carattere;
224400141118         QLGSO = Ascendente;
224500150115         QLGKL(3) = QLGSKL;
224600141030
224700141030       // Load other sort parameters.
224800141030         QLGLB = 80 + 16 * MaxKey;
224900141030         QLGRL = %SIZE(sflrcd) - 1;
225000141030         QLGRT = 8;
225100141030         QLGOKL = 80;
225200141030         QLGLKE = 16;
225300141030         QLGLSS = 290;
225400141030
225500141030       // Initialize Sort I/O API fields.
225600141030         QLGRL00 = QLGRL;
225700141030         QLGRC00 = 1;
225800141030         clear QUSEI;
225900141030         QUSBPRV = %SIZE(QUSEC);
226000141030
226100141030       // First step - Initialize the sort routine.
226200141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
226300141030
226400141030       // Next step - Write records to I/O routine.
226500141030         QLGRT00 = Put;
226600141103         FOR  xx = 1 to rrnlast;
226700150112           chain xx KC01S03;
226800170303           savokobj = VS3okobj;
226900170303           savokobjp = VS3okobjp;
227000170303           savokobjf = VS3okobjf;
227100170303           savokdelta = VS3okdelta;
227200141030
227300141030       // solo le righe con Selected = 'Y' sono riordinate,
227400141030       // quindi per fare un ordinamento di tutte le righe
227500141030       // metto 'Y' sempre.
227600141103           selected  = 'Y';
227700141103           clear QUSEI;
227800141103           QUSBPRV = %SIZE(QUSEC);
227900141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
228000141103         ENDFOR;
228100141030
228200141030       // Next step - Signal end of input, clear subfile for reload.
228300141030         QLGRT00 = EndPut;
228400141030         clear QUSEI;
228500141030         QUSBPRV = %SIZE(QUSEC);
228600141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
228700141030       // pulizia SFL
228800141030         exsr PulS03;
228900141030
229000141030       // Final step - Write the records back to the subfile.
229100141030         QLGRT00 = Get;
229200141103         FOR  xx = 1 to rrnlast;
229300141103           clear QUSEI;
229400141103           QUSBPRV = %SIZE(QUSEC);
229500141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
229600170303
229700170303       //?Imposto il subfile da emettere
229800170303           VS3okobj = savokobj;
229900170303           VS3okobjp = savokobjp;
230000170303           VS3okobjf = savokobjf;
230100170303           VS3okdelta = savokdelta;
230200141111           clear attrrag;
230300170303           clear attrobj;
230400170303           clear attrobjcf;
230500141103           attrclv = visHI;
230600170303           IF  not VisDelta;
230700170303             attrobj = visRIRED;
230800170303           ENDIF;
230900170303           IF  VS3objcf <> *blanks;
231000141103             attrobjcf = visRI;
231100141103           ENDIF;
231200170303           IF  VS3hobjcf < VS3hobjfin and VS3objcf <> *blanks;
231300170303             attrobjcf = visRIRED;
231400170303           ENDIF;
231500141121           IF  VS3esitotr <> *blanks;
231600141121             Visesito = *on;
231700141121           ELSE;
231800141121             Visesito = *off;
231900141121           ENDIF;
232000170303
232100170303       //?Imposto a video le colonne obiettivi/delta
232200170303         IF  not VisDelta;
232300170303           IF  VS3okobj = 'S';
232400170303             VS3obj = %editc(VS3hobj:'J');
232500170303           ENDIF;
232600170303           IF  VS3okobjp = 'S';
232700170303             VS3alfa = ' ' + %editc(VS3hobjpro:'J');
232800170303           ENDIF;
232900170303           IF  VS3okobjf = 'S';
233000170303             %subst(VS3alfa:11:7) = %editc(VS3hobjfin:'J');
233100170303           ENDIF;
233200170303         ELSE;
233300170303           IF  VS3okdelta = 'S';
233400170303             VS3obj = %editc(VS3hobjfin:'J');
233500170303             VS3alfa = %editc(VS3hdelta:'M') + ' ' +
233600170303                       %editc(VS3hnsped:'2');
233700170303           ENDIF;
233800170303         ENDIF;
233900170303
234000141103           s03nrr = xx;
234100150112           write KC01S03;
234200141103         ENDFOR;
234300141112
234400141117         rrnlast = S03nrr;
234500141112         IF  S03nrr > *zeros;
234600141112           V03rcd = 1;
234700141112           V03csr = 1;
234800141112         ELSE;
234900141112           clear V03rcd;
235000141112         ENDIF;
235100141030
235200141030       ENDSR;
235300141028
235400141028       //--------------------------------------------------------------
235500141028       //?Gestione opzioni Subfile S03.
235600141028       //--------------------------------------------------------------
235700141028       BEGSR OpzS03;
235800170303
235900170303         Ricarica = *off;
236000141028
236100150112         readc KC01S03;
236200141028
236300150112         DOW  NOT  %eof(TRKC01D);
236400141028
236500141028           SflNxtChg = *off;
236600141028           WindDspF  = IndDspF;
236700141028           reset WindDspFb;
236800141028           IndDspF   = WindDspF;
236900141028           V03rcd    = S03nrr;
237000141028
237100141127           PosCurOPZ = *off;
237200141119           sav_S03nrr = S03nrr;
237300141119
237400141119         //?- Verifico se il cliente è gestibile dall'utente
237500141119           IF  VS3opz <> *blank;
237600141119             clear TNTAA1DS;
237700141119             ITAA1caut = '§utegtc';
237800141119             ITAA1ksc = VS3ksu;
237900141119             kpjbu = TNTAA1DS;
238000141119             tntaa1c (kpjba);
238100150217             TNTAA1DS = kpjbu;
238200141119             IF  OTAA1fabi = 'N';
238300141119               ErrGenerico = *on;
238400141119               ErrMessage  = *on;
238500141127               PosCurOPZ   = *on;
238600150216               V03msg = %trim(Msg(07)) + '. Non in gestione';
238700141119             ENDIF;
238800141119           ENDIF;
238900141028
239000141119         //?- Se ok il cliente
239100141119           IF  not ErrMessage;
239200141119
239300141119             SELECT;
239400141119           //?- Nessuna opzione
239500141119             WHEN  VS3opz = *blank;
239600141028
239700141119           //?- 2 = Modifica
239800150115             WHEN  VS3opz = '2' and VS3cch = *blanks;
239900150127             //?anche in caso di periodo fuori dal range ma utente di sede
240000150115               IF  Oggi < CMPdigo or Oggi > CMPdfgo;
240100150216                 IF  DUTpou = 046;
240200150127                   exsr Opzione2;
240300150127                 ELSE;
240400150115                 ErrMessage  = *on;
240500150115                 ErrGenerico = *on;
240600150115                 PosCurOPZ   = *on;
240700150216                 V03msg = Msg(12);
240800150306                 %subst(V03msg:34:10) =
240900150306                 %subst(%editc(CMPdigo:'X'):7:2) + '/' +
241000150306                 %subst(%editc(CMPdigo:'X'):5:2) + '/' +
241100150306                 %subst(%editc(CMPdigo:'X'):1:4);
241200150306                 %subst(V03msg:48:10) =
241300150306                 %subst(%editc(CMPdfgo:'X'):7:2) + '/' +
241400150306                 %subst(%editc(CMPdfgo:'X'):5:2) + '/' +
241500150306                 %subst(%editc(CMPdfgo:'X'):1:4);
241600150127                 ENDIF;
241700150115               ELSE;
241800150115                 exsr Opzione2;
241900150115               ENDIF;
242000141028
242100150116           //?- 5 = Statistica
242200150116             WHEN  VS3opz = '5';
242300150116               exsr Opzione5;
242400141028
242500141119           //?- I = Interrogazione Clienti
242600141119             WHEN  VS3opz = 'I';
242700141119               exsr OpzioneI;
242800141028
242900141119           //?- T = Gestione Trattativa
243000141119             WHEN  VS3opz = 'T' and VS3nrv > 0;
243100141119               exsr OpzioneT;
243200141028
243300141119           //?- S = Storico Fasi
243400141119             WHEN  VS3opz = 'S';
243500141119               exsr OpzioneS;
243600150127
243700150127           //?- N = Note
243800150127             WHEN  VS3opz = 'N';
243900150127               exsr OpzioneN;
244000150220
244100150220           //?- F = Trattativa Forzata
244200150220             WHEN  VS3opz = 'F';
244300150220               exsr OpzioneF;
244400141028
244500141119             OTHER;
244600141119               ErrMessage  = *on;
244700141119               ErrGenerico = *on;
244800141127               PosCurOPZ   = *on;
244900150216               V03msg = Msg(11);
245000141119             ENDSL;
245100141119           ENDIF;
245200141028
245300141028           //?Aggiornamento sfl
245400141028           SELECT;
245500141028             WHEN  ErrMessage;
245600141028               SflNxtChg = *on;
245700141028               V03csr    = V03rcd;
245800141028             WHEN  ErrGenerico;
245900141028               V03csr = V03rcd;
246000141103               clear VS3opz;
246100141028             OTHER;
246200141028               V03csr = V03rcd;
246300141103               clear VS3opz;
246400141028           ENDSL;
246500150128
246600150128           IF  VS3esitotr <> *blanks;
246700150128             Visesito = *on;
246800150128           ELSE;
246900150128             Visesito = *off;
247000150128           ENDIF;
247100141028
247200150112           update KC01S03;
247300141028
247400141028           IF  ErrMessage or ErrGenerico;
247500141028             leave;
247600141028           ENDIF;
247700141028
247800150112           readc KC01S03;
247900141028
248000141028         ENDDO;
248100141118
248200141118       //?Ricarico il subfile
248300170303         IF  not ErrGenerico and not ErrMessage and Ricarica;
248400141119           wInzS03 = *on;
248500141119         ENDIF;
248600141028
248700141028       ENDSR;
248800141113
248900141113       //--------------------------------------------------------------
249000141113       //?Opzione "2" videata S03.
249100141113       //--------------------------------------------------------------
249200141113       BEGSR Opzione2;
249300141113
249400141114       //?Richiamo pgm x sapere quale obiettivo posso variare
249500150112         clear TRKC72DS;
249600150212         IKC72ncm = VH4ncm;
249700150112         IKC72ksu = VS3ksu;
249800150112         IKC72ksc = VS3ksc;
249900150112         IKC72cpo = VS3cpo;
250000150112         IKC72acm = VS3ufe;
250100150127       //?Se utente di sede e non siamo più nel periodo possibile
250200150127       //?per la modifica obiettivi e l'ultima fase è la 10 forzo come
250300150127       //?ultima fase la 20, in questo modo il pgm di ma la possibilità
250400150127       //?di inserire la 30
250500150127         IF  IKC72acm = FaseObj and DUTpou = 046 and
250600150127            (Oggi < CMPdigo or Oggi > CMPdfgo);
250700150127           IKC72acm = FaseObjProp;
250800150127         ENDIF;
250900150112         trkc72r (kpjba:TRKC72DS);
251000150112         IF  OKC72err <> *blanks;
251100141114           ErrMessage  = *on;
251200141114           ErrGenerico = *on;
251300141127           PosCurOPZ   = *on;
251400150112           V03msg = OKC72msg;
251500141114           leavesr;
251600141114         ENDIF;
251700150115
251800150115       //?Se tutto OK
251900150115       //?richiamo pgm per la variazione obiettivo
252000150115         clear TRKC07DS;
252100150115         IKC07ric = '2';
252200150212         IKC07ncm = VH4ncm;
252300150112         IKC07ksu = VS3ksu;
252400150112         IKC07ksc = VS3ksc;
252500150112         IKC07cpo = VS3cpo;
252600150112         IKC07acm = OKC72acm;
252700150112         trkc07r (kpjba:TRKC07DS);
252800170303
252900170303         IF  OKC07f12 <> 'S';
253000170303           Ricarica = *on;
253100170303         ENDIF;
253200141113
253300141113       ENDSR;
253400150115
253500150115       //--------------------------------------------------------------
253600150115       //?Opzione "R" videata S03.
253700150115       //--------------------------------------------------------------
253800150115       BEGSR OpzioneR;
253900150115
254000150115       //?richiamo il pgm per ripristinare il cliente in campagna
254100150115         clear TRKC07DS;
254200150115         IKC07ric = 'R';
254300150212         IKC07ncm = VH4ncm;
254400150115         IKC07ksu = VS3ksu;
254500150115         IKC07ksc = VS3ksc;
254600150115         IKC07cpo = VS3cpo;
254700150115         IKC07acm = VS3ufe;
254800150115         trkc07r (kpjba:TRKC07DS);
254900150115
255000150115       ENDSR;
255100141103
255200141103       //--------------------------------------------------------------
255300150116       //?Opzione "5" videata S03.
255400141103       //--------------------------------------------------------------
255500150116       BEGSR Opzione5;
255600150217
255700150217       //?- Verifico se l'opzione è utilizzabile dall'utente
255800170303         clear TNTAA1DS;
255900170303         ITAA1caut = '§uteist';
256000170303         ITAA1ksc = VS3ksu;
256100170303         kpjbu = TNTAA1DS;
256200170303         tntaa1c (kpjba);
256300170303         TNTAA1DS = kpjbu;
256400170303         IF  OTAA1fabi = 'N';
256500170303           ErrGenerico = *on;
256600170303           ErrMessage  = *on;
256700170303           PosCurOPZ   = *on;
256800170303           V03msg = %trim(Msg(07)) + '. Non in gestione';
256900170303           leavesr;
257000170303         ENDIF;
257100170303
257200170303       //?Controllo se ora è figlio in questo caso devo passare il padre
257300170303         wksu = VS3ksu;
257400170303         clear TIBS10DS;
257500170303         D10tle = 'ST';
257600170303         D10paf = 'P';
257700170303         D10cod = VS3ksu;
257800170303         TIBS10R (TIBS10DS);
257900170303         IF  D10err = *blanks and D10cop > *zeros;
258000170303           wksu = D10cop;
258100170303         ENDIF;
258200141103
258300141103         clear TISE60DS;
258400141103         D60op0 = 'VIS';
258500141103         D60op1 = 'F06';
258600141103         D60sce = '3';
258700170303         dsKSC = wksu;
258800141103         skKSA(1) = dsKSA;
258900141103         kpjbu = TISE60DS;
259000141103         tise61r (kpjba);
259100141103
259200141103       ENDSR;
259300141103
259400141103       //--------------------------------------------------------------
259500141103       //?Opzione "I" videata S03.
259600141103       //--------------------------------------------------------------
259700141103       BEGSR OpzioneI;
259800141103
259900141103         clear TNTAI2DS;
260000150122       //?se non è utente di sede richiamo il pgm in modo da poter gestire
260100150122       //?le eventuali attività ancora da eseguire + crearne delle nuove
260200150122         IF  DUTpou <> 046;
260300150122           ITAI2ric = 'A';
260400150122         ENDIF;
260500141107         ITAI2ksc = VS3ksu;
260600141103         tntai2r (kpjba:TNTAI2DS);
260700141103
260800141103       ENDSR;
260900141103
261000141103       //--------------------------------------------------------------
261100141103       //?Opzione "T" videata S03.
261200141103       //--------------------------------------------------------------
261300141103       BEGSR OpzioneT;
261400141103
261500141103         clear TNTA88DS;
261600141103         ITA88fle = '5';
261700141103         ITA88nrv = VS3nrv;
261800141103         tnta88r (kpjba:TNTA88DS);
261900141103
262000141103       ENDSR;
262100141107
262200141107       //--------------------------------------------------------------
262300141107       //?Opzione "S" videata S03.
262400141107       //--------------------------------------------------------------
262500141107       BEGSR OpzioneS;
262600141107
262700150112         clear TRKC03DS;
262800150212         IKC03ncm = VH4ncm;
262900150112         IKC03ksu = VS3ksu;
263000150112         IKC03ksc = VS3ksc;
263100150112         IKC03cpo = VS3cpo;
263200150112         trkc03r (kpjba:TRKC03DS);
263300141107
263400141107       ENDSR;
263500141113
263600141113       //--------------------------------------------------------------
263700141113       //?Opzione "E" videata S03.
263800141113       //--------------------------------------------------------------
263900141113       BEGSR OpzioneE;
264000141114
264100141114       //?richiamo pgm per escludere il cliente dalla campagna
264200150112         clear TRKC07DS;
264300150112         IKC07ric = '2';
264400150212         IKC07ncm = VH4ncm;
264500150112         IKC07ksu = VS3ksu;
264600150112         IKC07ksc = VS3ksc;
264700150112         IKC07cpo = VS3cpo;
264800150112         IKC07acm = FaseChiudi;
264900150112         trkc07r (kpjba:TRKC07DS);
265000141113
265100141113       ENDSR;
265200150127
265300150127       //--------------------------------------------------------------
265400150127       //?Opzione "N" videata S03.
265500150127       //--------------------------------------------------------------
265600150127       BEGSR OpzioneN;
265700150127
265800150127       //?richiamo pgm per gestione Note
265900150127         clear TRKC10DS;
266000150127         IKC10tla = 'L';
266100150212         IKC10ncm = VH4ncm;
266200150127         IKC10ksu = VS3ksu;
266300150127         IKC10ksc = VS3ksc;
266400150127         IKC10cpo = VS3cpo;
266500150127         IKC10acm = VS3ufe;
266600150127         trkc10r (kpjba:TRKC10DS);
266700150127
266800150127       ENDSR;
266900150220
267000150220       //--------------------------------------------------------------
267100150220       //?Opzione "F" videata S03.
267200150220       //--------------------------------------------------------------
267300150220       BEGSR OpzioneF;
267400150225
267500150225       //?Posso fare la fase TF solo nel periodo di inserimento trattative
267600150225         IF  Oggi < CMPdit or Oggi > CMPdft;
267700150225           ErrMessage  = *on;
267800150225           ErrGenerico = *on;
267900150225           PosCurOPZ   = *on;
268000150225           V03msg = Msg(23);
268100150306           %subst(V03msg:39:10) =
268200150306           %subst(%editc(CMPdit:'X'):7:2) + '/' +
268300150306           %subst(%editc(CMPdit:'X'):5:2) + '/' +
268400150306           %subst(%editc(CMPdit:'X'):1:4);
268500150306           %subst(V03msg:53:10) =
268600150306           %subst(%editc(CMPdft:'X'):7:2) + '/' +
268700150306           %subst(%editc(CMPdft:'X'):5:2) + '/' +
268800150306           %subst(%editc(CMPdft:'X'):1:4);
268900150225           leavesr;
269000150225         ENDIF;
269100150220
269200150220       //?Posso immettere la Trattativa Forzata se non presente NESSUNA fase TR
269300150220         chain (VH4ncm:VS3ksu:VS3ksc:VS3cpo:FaseObjTtr) TICMF01L;
269400150220         IF  %found(TICMF01L);
269500150220           ErrMessage  = *on;
269600150220           ErrGenerico = *on;
269700150220           PosCurOPZ   = *on;
269800150220           V03msg = %trim(Msg(11)) + '. Già presente una Trattativa';
269900150220           leavesr;
270000150220         ENDIF;
270100150220
270200150220       //?richiamo pgm per immissione Trattativa Forzata
270300150220         clear TRKC07DS;
270400150220         IKC07ric = '2';
270500150220         IKC07ncm = VH4ncm;
270600150220         IKC07ksu = VS3ksu;
270700150220         IKC07ksc = VS3ksc;
270800150220         IKC07cpo = VS3cpo;
270900150220         IKC07acm = FaseObjTf;
271000150220         trkc07r (kpjba:TRKC07DS);
271100170303
271200170303         IF  OKC07f12 <> 'S';
271300170303           Ricarica = *on;
271400170303         ENDIF;
271500150220
271600150220       ENDSR;
271700141030
271800141030       //--------------------------------------------------------------
271900141030       //?Gestione videata D04.
272000141030       //--------------------------------------------------------------
272100141030       BEGSR GesD04;
272200141030
272300141030       //?Inizializzazione videata
272400141030         IF  wInzD04;
272500141030           exsr InzD04;
272600141030           wInzD04 = *off;
272700141030         ENDIF;
272800141030
272900141030       //?Emissione videata
273000150112         exfmt  KC01W04;
273100141030         reset ErrMessage;
273200141030         reset ErrGenerico;
273300141030         clear V04msg;
273400141030
273500141030         SELECT;
273600150119
273700150119       //?- F01=Formula
273800150119           WHEN  dsp_aid = c_F01;
273900150119             exsr F01D04;
274000150128
274100150128       //?- F02=Obiettivo/Andamento
274200150128           WHEN  dsp_aid = c_F02;
274300150128             exsr F02D04;
274400150302
274500150302       //?- F04=Scelta Trattative
274600150302           WHEN  dsp_aid = c_F04;
274700150302             exsr F04D04;
274800141103
274900141103       //?- F06=Conferma
275000141103           WHEN  dsp_aid = c_F06;
275100141104             exsr ContrD04;
275200141104             IF  ErrGenerico;
275300141104               leavesr;
275400141104             ENDIF;
275500141103             exsr F06D04;
275600141030
275700141030       //?- F12=Ritorno
275800141030           WHEN  dsp_aid = c_F12;
275900141030             exsr F12D04;
276000141030
276100141030       //?Invio
276200141030           OTHER;
276300141030             exsr ContrD04;
276400141030             IF  ErrGenerico;
276500141030               leavesr;
276600141030             ENDIF;
276700141030
276800141030         ENDSL;
276900141030
277000141030       ENDSR;
277100141030
277200141030       //--------------------------------------------------------------
277300141030       //?Inizializzazione Videata D04.
277400141030       //--------------------------------------------------------------
277500141030       BEGSR InzD04;
277600141030
277700141030       //?Pulizia videata
277800150112         clear KC01W04;
277900150212
278000150212         V04ncm = V02ncm;
278100150212         V04des = V02des;
278200150212         VH4ncm = %dec(V02ncm:7:0);
278300141030
278400150212         V04cdi = V02cdi;
278500150212         V04cdid = V02cdid;
278600150216         IF  V02car <> *blanks;
278700150212           V04car = V02car;
278800150212           V04card = V02card;
278900150212           VH4car = %dec(V02car:3:0);
279000141030         ENDIF;
279100150216         IF  V02fil <> *blanks;
279200150212           V04fil = V02fil;
279300150212           V04fild = V02fild;
279400150212           VH4fil = %dec(V02fil:3:0);
279500141030         ENDIF;
279600150216         IF  V02cmm <> *blanks;
279700150212           V04cmm = V02cmm;
279800150212           V04cmmd = V02cmmd;
279900150212           VH4cmm = %dec(V02cmm:7:0);
280000141030         ENDIF;
280100150216         IF  V02ksu <> *blanks;
280200150212           V04ksu = V02ksu;
280300150212           V04rag = V02rag;
280400150212           VH4ksu = %dec(V02ksu:7:0);
280500141030         ENDIF;
280600150212         V04clv1 = V02clv1;
280700150212         V04clv2 = V02clv2;
280800150212         V04clv3 = V02clv3;
280900150212         V04clv4 = V02clv4;
281000150212         V04clv5 = V02clv5;
281100150212         V04deldas = V02deldas;
281200150212         V04delda = V02delda;
281300150212         V04delas = V02delas;
281400150212         V04dela = V02dela;
281500150212         V04cli = V02cli;
281600150212         V04ncli = V02ncli;
281700150212         V04ttr = V02ttr;
281800150212         V04obj = V02obj;
281900150212         V04obj1 = V02obj1;
282000150212         V04obj2 = V02obj2;
282100150212         V04formula = V02formula;
282200150213         V04obj3 = V02obj3;
282300150213         V04obj3das = V02obj3das;
282400150213         V04obj3da = V02obj3da;
282500150213         V04obj3as = V02obj3as;
282600150213         V04obj3a = V02obj3a;
282700150223
282800150223       //?Se richiamato chiudo i file per il pgm TRKC76R
282900150223         IF  wTRKC76;
283000150223           clear TRKC76DS;
283100150223           IKC76ric = 'C';
283200150223           trkc76r (kpjba:TRKC76DS);
283300150223         ENDIF;
283400141030
283500141030       ENDSR;
283600141030
283700141030       //--------------------------------------------------------------
283800141030       //?Controllo Videata D04.
283900141030       //--------------------------------------------------------------
284000141030       BEGSR ContrD04;
284100141030
284200141030         WindDspF = IndDspF;
284300141030         reset WindDspFb;
284400141030         IndDspF  = WindDspF;
284500141119
284600141119         clear wdelda;
284700141119         clear wdela;
284800150212
284900150212       //?Campagna Commerciale Obbligatoria
285000150212         clear V04des;
285100150305         clear VH4ncm;
285200150212         IF  V04ncm = *blanks;
285300150212           ErrMessage  = *on;
285400150212           ErrGenerico = *on;
285500150212           PosCurNCM   = *on;
285600150212           V04msg = Msg(02);
285700150212           leavesr;
285800150212         ENDIF;
285900150212       //?Ricerca Campagna Commerciale
286000150212         IF  %scan('?':V04ncm) > 0;
286100150212           clear TRKC02DS;
286200150212           IKC02ric = 'R';
286300150212           trkc02r (kpjba:trkc02ds);
286400150212           ErrGenerico = *on;
286500150212           PosCurNCM   = *on;
286600150212           V04ncm = %editc(OKC02ncm:'X');
286700150212         ENDIF;
286800150212       //?Accetto solo dati numerici
286900150212         xx = 1;
287000150212         FOR xx by 1 to %len(V04ncm);
287100150212           IF  %subst(V04ncm:xx:1) <> *blanks and
287200150212               %check(Digits:%subst(V04ncm:xx:1)) > *zeros;
287300150212             ErrMessage  = *on;
287400150212             ErrGenerico = *on;
287500150212             PosCurNCM   = *on;
287600150212             V04msg = Msg(02);
287700150212             leavesr;
287800150212           ENDIF;
287900150212         ENDFOR;
288000150212       //?Deve essere una Campagna Commerciale valida
288100150212         CMPncm = %dec(V04ncm:7:0);
288200150212         chain CMPncm TICMP01L;
288300150212         IF  not %found(TICMP01L);
288400150212           ErrMessage  = *on;
288500150212           ErrGenerico = *on;
288600150212           PosCurNCM   = *on;
288700150212           V04msg = Msg(02);
288800150212           leavesr;
288900150212         ENDIF;
289000150212       //?Decodifico Campagna Commerciale
289100150212         V04des = CMPdes;
289200150505       ////?Campagna Commerciale già finita
289300150505         //IF  CMPdfc < Oggi;
289400150505         //  ErrMessage  = *on;
289500150505         //  ErrGenerico = *on;
289600150505         //  PosCurNCM   = *on;
289700150505         //  V04msg = %trim(Msg(02)) + '. Già chiusa';
289800150505         //  leavesr;
289900150505         //ENDIF;
290000150212       //?In carico all'Utente
290100150212         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
290200150212           ErrMessage  = *on;
290300150212           ErrGenerico = *on;
290400150212           PosCurNCM   = *on;
290500150212           V04msg = %trim(Msg(02)) + '. Non in gestione';
290600150212           leavesr;
290700150212         ENDIF;
290800150212         VH4ncm = %dec(V04ncm:7:0);
290900141030
291000141030       //?Distretto
291100141030         w001a = V04cdi;
291200141030         clear w050a;
291300141030         exsr Distretto;
291400141030         V04cdi = w001a;
291500141030         v04cdid = w050a;
291600141030         V04msg = wmsg;
291700141030         IF  Errgenerico;
291800141030           leavesr;
291900141030         ENDIF;
292000141030
292100141030       //?Area
292200150305         clear VH4car;
292300141030         w003a = V04car;
292400141030         clear w050a;
292500141030         exsr Area;
292600141030         V04car = w003a;
292700141030         V04card = w050a;
292800141030         V04msg = wmsg;
292900141030         IF  ErrGenerico;
293000141030           leavesr;
293100141030         ENDIF;
293200141030       //?Se presente anche il distretto devono essere congruenti
293300141030         IF  V04cdi <> *blanks and V04car <> *blanks;
293400150219           chain (V04cdi:w0030) AZORG02L;
293500141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
293600141030             ErrGenerico = *on;
293700141030             ErrMessage  = *on;
293800141030             PosCurCAR   = *on;
293900141030             V04msg = %trim(Msg(04)) + '. Non congruente con il distretto';
294000141030             leavesr;
294100141030           ENDIF;
294200141030         ENDIF;
294300150212         IF  V04car <> *blanks;
294400150212           VH4car = %dec(V04car:3:0);
294500150212         ENDIF;
294600141030
294700141030       //?Filiale Commerciale
294800150305         clear VH4fil;
294900141030         w003a = V04fil;
295000141030         clear w050a;
295100141030         exsr Filiale;
295200141030         V04fil = w003a;
295300141030         V04fild = w050a;
295400141030         V04msg = wmsg;
295500141030         IF  ErrGenerico;
295600141030           leavesr;
295700141030         ENDIF;
295800150212         IF  V04fil <> *blanks;
295900150212           VH4fil = %dec(V04fil:3:0);
296000150212         ENDIF;
296100141030
296200141030       //?Commerciale Unificante
296300150305         clear VH4cmm;
296400141030         w007a = V04cmm;
296500141030         clear w050a;
296600141030         exsr Commerciale;
296700141030         V04cmm = w007a;
296800141030         V04cmmd = w050a;
296900141103         V04msg = wmsg;
297000141030         IF  ErrGenerico;
297100141030           leavesr;
297200141030         ENDIF;
297300150212         IF  V04cmm <> *blanks;
297400150212           VH4cmm = %dec(V04cmm:7:0);
297500150212         ENDIF;
297600141030
297700141030       //?Cliente
297800150305         clear VH4ksu;
297900141107         w007a = V04ksu;
298000141030         clear w050a;
298100141030         exsr Cliente;
298200141107         V04ksu = w007a;
298300141030         V04rag = w050a;
298400141030         V04msg = wmsg;
298500141030         IF  ErrGenerico;
298600141030           leavesr;
298700141030         ENDIF;
298800150212         IF  V04ksu <> *blanks;
298900150212           VH4ksu = %dec(V04ksu:7:0);
299000150212         ENDIF;
299100150611
299200150611       //?Se utente NON di sede a abilitato a più di un'area e no
299300150611       //?abilitazione 'AZ' o 'DI'
299400150611       //?richiedo obbligatoria l'area
299500150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
299600150611             V04car = *blanks and V04cmm = *blanks and
299700150611             V04ksu = *blanks and car(2) > *zeros;
299800150611           ErrMessage  = *on;
299900150611           ErrGenerico = *on;
300000150611           PosCurCAR   = *on;
300100150611           V04msg = Msg(25);
300200150611           leavesr;
300300150611         ENDIF;
300400141030
300500141030       //?Importanza Cliente
300600141030         w001a = V04clv1;
300700141030         clear w050a;
300800141030         exsr ImpCliente;
300900150219         V04clv1 = w001a;
301000141103         V04msg = wmsg;
301100141030         IF  ErrGenerico;
301200141030           PosCurCLV1 = *on;
301300141030           leavesr;
301400141030         ENDIF;
301500141030         w001a = V04clv2;
301600141030         clear w050a;
301700141030         exsr ImpCliente;
301800141030         V04clv2 = w001a;
301900141030         V04msg = wmsg;
302000141030         IF  ErrGenerico;
302100141030           PosCurCLV2 = *on;
302200141030           leavesr;
302300141030         ENDIF;
302400141030         w001a = V04clv3;
302500141030         clear w050a;
302600141030         exsr ImpCliente;
302700141030         V04clv3 = w001a;
302800141030         V04msg = wmsg;
302900141030         IF  ErrGenerico;
303000141030           PosCurCLV3 = *on;
303100141030           leavesr;
303200141030         ENDIF;
303300141030         w001a = V04clv4;
303400141030         clear w050a;
303500141030         exsr ImpCliente;
303600141030         V04clv4 = w001a;
303700141030         V04msg = wmsg;
303800141030         IF  ErrGenerico;
303900141030           PosCurCLV4 = *on;
304000141030           leavesr;
304100141030         ENDIF;
304200141030         w001a = V04clv5;
304300141030         clear w050a;
304400141030         exsr ImpCliente;
304500141030         V04clv5 = w001a;
304600141030         V04msg = wmsg;
304700141030         IF  ErrGenerico;
304800141030           PosCurCLV5 = *on;
304900141030           leavesr;
305000141030         ENDIF;
305100141030
305200141030       //?Delta
305300141030         IF  V04delda > *zeros or V04dela <> *zeros;
305400141030         //?Obbligatori tutti e 2 se immesso uno dei due
305500141030           IF  V04delda > *zeros and V04dela = *zeros;
305600141030             ErrMessage  = *on;
305700141030             ErrGenerico = *on;
305800141030             PosCurDELA  = *on;
305900150216             V04msg      = Msg(09);
306000141030             leavesr;
306100141030           ENDIF;
306200141030           IF  V04delda = *zeros and V04dela > *zeros;
306300141030             ErrMessage  = *on;
306400141030             ErrGenerico = *on;
306500141030             PosCurDELda = *on;
306600150216             V04msg      = Msg(09);
306700141030             leavesr;
306800141030           ENDIF;
306900141030         //?Controllo congruenza tra "DAL" e "AL"
307000141104           wdelda = V04delda;
307100141104           wdela  = V04dela;
307200141030           IF  V04deldas = '-';
307300141030             wdelda = wdelda * -1;
307400141030           ENDIF;
307500141030           IF  V04delas = '-';
307600141030             wdela = wdela * -1;
307700141030           ENDIF;
307800141030           IF  wdelda > wdela;
307900141030             ErrMessage  = *on;
308000141030             ErrGenerico = *on;
308100141030             PosCurDELda = *on;
308200150216             V04msg      = Msg(10);
308300141030             leavesr;
308400141030           ENDIF;
308500141030         ENDIF;
308600141121
308700141121       //?Obiettivi
308800141127         wfobj  = V04obj;
308900141127         wfobj1 = V04obj1;
309000141127         wfobj2 = V04obj2;
309100150119         wfformula = V04formula;
309200150213         wfobj3 = V04obj3;
309300150213         wobj3da = V04obj3da;
309400150213         wobj3a  = V04obj3a;
309500150219         IF  V04obj3das = '-';
309600150213           wobj3da = wobj3da * -1;
309700150213         ENDIF;
309800150219         IF  V04obj3as = '-';
309900150213           wobj3a = wobj3a * -1;
310000150213         ENDIF;
310100150128         exsr ContrObj;
310200141127         V04obj1 = wfobj1;
310300141127         V04obj2 = wfobj2;
310400150119         V04formula = wfformula;
310500150213         V04obj3 = wfobj3;
310600150213         V04obj3da = %abs(wobj3da);
310700150213         V04obj3a  = %abs(wobj3a);
310800141127         V04msg = wmsg;
310900141127         IF  ErrGenerico;
311000141127           leavesr;
311100141127         ENDIF;
311200150302
311300150302       //?Trattative
311400150302         w002a = V04ttr;
311500150302         clear w050a;
311600150302         exsr SceltaTtr;
311700150302         V04ttr = w002a;
311800150302         V04msg = wmsg;
311900150302         IF  ErrGenerico;
312000150302           leavesr;
312100150302         ENDIF;
312200141030
312300141030       ENDSR;
312400150119
312500150119       //--------------------------------------------------------------
312600150119       //?Gestione tasto funzionale F01 da videata D04
312700150119       //?F01=Formula
312800150119       //--------------------------------------------------------------
312900150119       BEGSR F01D04;
313000150119
313100150119         savformula = V04formula;
313200150119         Video = 'S6';
313300150119         wInzS06 = *on;
313400150119
313500150119       ENDSR;
313600150128
313700150128       //--------------------------------------------------------------
313800150128       //?Gestione tasto funzionale F02 da videata D04
313900150128       //?F02=Obiettivo/Andamento
314000150128       //--------------------------------------------------------------
314100150128       BEGSR F02D04;
314200150216
314300150216         IF  H2nmfl <> 'V04OBJ1' and H2nmfl <> 'V04OBJ2' and
314400150216             H2nmfl <> 'V04OBJ3';
314500150216           ErrMessage  = *on;
314600150216           ErrGenerico = *on;
314700150216           PosCurOBJ1  = *on;
314800150216           V04msg = Msg(22);
314900150216           leavesr;
315000150216         ENDIF;
315100150128
315200150128         Video = 'S8';
315300150128         wInzS08 = *on;
315400150128
315500150128       ENDSR;
315600150302
315700150302       //--------------------------------------------------------------
315800150302       //?Gestione tasto funzionale F04 da videata D04
315900150302       //?F04=Scelta Trattative
316000150302       //--------------------------------------------------------------
316100150302       BEGSR F04D04;
316200150302
316300150302         Video = 'S9';
316400150302         wInzS09 = *on;
316500150302
316600150302       ENDSR;
316700141103
316800141103       //--------------------------------------------------------------
316900141103       //?Gestione tasto funzionale F06 da videata D04
317000141103       //?F06=Conferma
317100141103       //--------------------------------------------------------------
317200141103       BEGSR F06D04;
317300141103
317400141103       //?Devo ricaricare il subfile con le nuove parzializzazioni
317500141103         Video = 'S3';
317600141112         wInzS03 = *on;
317700170306         clear sav_s03nrr;
317800141103
317900141103       ENDSR;
318000141030
318100141030       //--------------------------------------------------------------
318200141030       //?Gestione tasto funzionale F12 da videata D04
318300141030       //?F12=Ritorno
318400141030       //--------------------------------------------------------------
318500141030       BEGSR F12D04;
318600150116
318700150116       //?Se arrivo qua dalla videata con messaggio di troppi rcd
318800150116       //?devo ricaricare il subfile
318900150116         IF  TroppiRcd;
319000150116           wInzs03 = *on;
319100150116         ENDIF;
319200141030
319300141030       //?Ritorno alle selezioni
319400141030         Video = 'S3';
319500141104       //?Forzo il posizionamento del cursore al 1° rcd del subfile
319600141104         clear V03rcd;
319700141030
319800141030       ENDSR;
319900141118
320000141118       //--------------------------------------------------------------
320100141118       //?Gestione videata D05.
320200141118       //--------------------------------------------------------------
320300141118       BEGSR GesD05;
320400150116
320500150116         TroppiRcd = *on;
320600141118
320700141118       //?Inizializzazione videata
320800141118         IF  wInzD05;
320900141118           exsr InzD05;
321000141118           wInzD05 = *off;
321100141118         ENDIF;
321200141118
321300141118       //?Emissione videata
321400150112         write  KC01T01;
321500150112         exfmt  KC01D05;
321600141118         reset ErrMessage;
321700141118         reset ErrGenerico;
321800141118
321900141118         SELECT;
322000141118
322100141118       //?- F03=Fine
322200141118           WHEN  dsp_aid = c_F03;
322300141118             exsr F03D02;
322400141118
322500141118       //?- F05=Altre parzializzazioni
322600141118           WHEN  dsp_aid = c_F05;
322700141118             exsr F05S03;
322800141118
322900141118       //?- F12=Ritorno
323000141118           WHEN  dsp_aid = c_F12;
323100141118             exsr F12S03;
323200141118
323300141118         ENDSL;
323400141118
323500141118       ENDSR;
323600141118
323700141118       //--------------------------------------------------------------
323800141118       //?Inizializzazione Videata D05.
323900141118       //--------------------------------------------------------------
324000141118       BEGSR InzD05;
324100141118
324200141118       ENDSR;
324300150116
324400150116       //--------------------------------------------------------------
324500150116       //?Gestione videata S06.
324600150116       //--------------------------------------------------------------
324700150116       BEGSR GesS06;
324800150116
324900150116       //?Inizializzazione videata
325000150116         IF  wInzS06;
325100150116           exsr InzS06;
325200150116           wInzS06 = *off;
325300150116         ENDIF;
325400150119
325500150119       //?Visualizzazione del SFL
325600150119         SflDsp = (S06nrr <= *zeros);
325700150116
325800150116       //?Emissione videata
325900150119         exfmt  KC01CW6;
326000150116         reset ErrMessage;
326100150116         reset ErrGenerico;
326200150116
326300150116       //?- Enter Controllo ed esco
326400150116         exsr OpzS06;
326500150116         IF  ErrGenerico;
326600150116           leavesr;
326700150116         ENDIF;
326800150116
326900150116      //?Videata sucessiva
327000150216         IF H2nmrc = 'KC01D02';
327100150216           Video = 'D2';
327200150216           V02formula = savformula;
327300150216         ENDIF;
327400150216         IF H2nmrc = 'KC01W04';
327500150216           Video = 'D4';
327600150216           V04formula = savformula;
327700150216         ENDIF;
327800150116
327900150116       ENDSR;
328000150116
328100150116       //--------------------------------------------------------------
328200150116       //?Inizializzazione Videata S06.
328300150116       //--------------------------------------------------------------
328400150116       BEGSR InzS06;
328500150116
328600150116       //?Pulizia subfile
328700150116         exsr PulS06;
328800150116
328900150116       //?Caricamento subfile
329000150116         exsr Ries06;
329100150116
329200150116         SflEnd = *on;
329300150116
329400150116       //?Imposto il posizionamento al primo rcd
329500150116         IF  S06nrr > 0;
329600150119           V06rcd = 1;
329700150116         ELSE;
329800150119           clear V06rcd;
329900150116         ENDIF;
330000150116
330100150119         V06csr = V06rcd;
330200150116
330300150116       ENDSR;
330400150116
330500150116       //--------------------------------------------------------------
330600150116       //?Pulizia Subfile S06.
330700150116       //--------------------------------------------------------------
330800150116       BEGSR PulS06;
330900150116
331000150116       //?Pulizia subfile
331100150116         SflDsp    = *on;
331200150116         SflDspCtl = *on;
331300150116         write KC01CW6;
331400150116         SflDspCtl = *off;
331500150116         SflEnd    = *off;
331600150116
331700150119         clear V06rcd;
331800150119         clear V06csr;
331900150116         clear S06nrr;
332000150119         clear V06msg;
332100150116
332200150116         ErrMessage  = *off;
332300150116         ErrGenerico = *off;
332400150116
332500150116         WindDspF = IndDspF;
332600150116         reset WindDspFb;
332700150116         IndDspF  = WindDspF;
332800150116
332900150116       ENDSR;
333000150116
333100150116       //--------------------------------------------------------------
333200150116       //?Caricamento Subfile S06.
333300150116       //--------------------------------------------------------------
333400150116       BEGSR RieS06;
333500150116
333600150116         xx = 1;
333700150116         FOR xx by 1 to 6;
333800150116           clear  KC01SW6;
333900150116           PosCurOPZ = *off;
334000150119           V06formula = Formula(xx);
334100150119           V06des = Formula(xx) + DesForm(xx);
334200150116           S06nrr += 1;
334300150116           write  KC01SW6;
334400150116         ENDFOR;
334500150116
334600150116       ENDSR;
334700150116
334800150116       //--------------------------------------------------------------
334900150116       //?Gestione opzioni Subfile S06.
335000150116       //--------------------------------------------------------------
335100150116       BEGSR OpzS06;
335200150116
335300150116         xx = 1;
335400150116         FOR xx by 1 to 6;
335500150119           S06nrr = xx;
335600150119           chain S06nrr KC01SW6;
335700150116           IF  not %found;
335800150116             leave;
335900150116           ENDIF;
336000150116
336100150116           SELECT;
336200150116         //?- Nessuna opzione
336300150119           WHEN  V06opz = *blank;
336400150116
336500150116         //?- 1 = Scelta
336600150119           WHEN  V06opz = '1';
336700150119             savformula = V06formula;
336800150121             PosCurOBJ2 = *on;
336900150116             leave;
337000150116
337100150116           OTHER;
337200150116             ErrMessage  = *on;
337300150116             ErrGenerico = *on;
337400150116             PosCurOPZ   = *on;
337500150216             V06msg = Msg(11);
337600150116             update KC01SW6;
337700150116             leave;
337800150116           ENDSL;
337900150116
338000150116         ENDFOR;
338100150116
338200150116       ENDSR;
338300150121
338400150121       //--------------------------------------------------------------
338500150121       //?Gestione videata D07.
338600150121       //--------------------------------------------------------------
338700150121       BEGSR GesD07;
338800150121
338900150121       //?Inizializzazione videata
339000150121         IF  wInzD07;
339100150121           exsr InzD07;
339200150121           wInzD07 = *off;
339300150121         ENDIF;
339400150121
339500150121       //?Emissione videata
339600150121         exfmt  KC01W07;
339700150121         reset ErrMessage;
339800150121         reset ErrGenerico;
339900150121
340000150121         SELECT;
340100150121
340200150121       //?- F06=Conferma
340300150121           WHEN  dsp_aid = c_F06;
340400150121             exsr F06D07;
340500150121
340600150121         ENDSL;
340700150121
340800150121       ENDSR;
340900150121
341000150121       //--------------------------------------------------------------
341100150121       //?Inizializzazione Videata D07.
341200150121       //--------------------------------------------------------------
341300150121       BEGSR InzD07;
341400150121
341500150121         clear KC01W07;
341600150121         V07sino = 'SI';
341700150121
341800150121       ENDSR;
341900150121
342000150121       //--------------------------------------------------------------
342100150121       //?Gestione tasto funzionale F06 da videata D07
342200150121       //?F06=Conferma.
342300150121       //--------------------------------------------------------------
342400150121       BEGSR F06D07;
342500150121
342600150121         Video = 'S3';
342700150121
342800150121         IF  V07sino = 'NO';
342900150121           leavesr;
343000150121         ENDIF;
343100150121
343200150121         FOR  xx = 1 to rrnlast;
343300150121           chain xx KC01S03;
343400150121         //?Scrivo la fase
343500150121           clear TRKC71DS;
343600150212           IKC71ncm = %dec(V04ncm:7:0);
343700150121           IKC71ksu = VS3ksu;
343800150121           IKC71ksc = VS3ksc;
343900150121           IKC71cpo = VS3cpo;
344000150121           IKC71acm = FaseObjFine;
344100170303           IKC71pea = VS3hobjpro;
344200150122           IKC71aut = 'A';
344300150128           IKC71dfa = %dec(%date());
344400150128           IKC71hfc = %dec(%time());
344500150121           TRKC71R (kpjba:TRKC71DS);
344600150121         ENDFOR;
344700150121
344800150121       //?Ricarico il subfile
344900150121         wInzS03 = *on;
345000150121
345100150121       ENDSR;
345200150128
345300150128       //--------------------------------------------------------------
345400150128       //?Gestione videata S08.
345500150128       //--------------------------------------------------------------
345600150128       BEGSR GesS08;
345700150128
345800150128       //?Inizializzazione videata
345900150128         IF  wInzS08;
346000150128           exsr InzS08;
346100150128           wInzS08 = *off;
346200150128         ENDIF;
346300150128
346400150128       //?Visualizzazione del SFL
346500150128         SflDsp = (S08nrr <= *zeros);
346600150128
346700150128       //?Emissione videata
346800150128         exfmt  KC01CW8;
346900150128         reset ErrMessage;
347000150128         reset ErrGenerico;
347100150128
347200150128       //?- Enter Controllo ed esco
347300150128         exsr OpzS08;
347400150128         IF  ErrGenerico;
347500150128           leavesr;
347600150128         ENDIF;
347700150128
347800150128      //?Videata sucessiva
347900150216         IF H2nmrc = 'KC01D02';
348000150216           Video = 'D2';
348100150216         ENDIF;
348200150216         IF H2nmrc = 'KC01W04';
348300150216           Video = 'D4';
348400150216         ENDIF;
348500150128
348600150128       ENDSR;
348700150128
348800150128       //--------------------------------------------------------------
348900150128       //?Inizializzazione Videata S08.
349000150128       //--------------------------------------------------------------
349100150128       BEGSR InzS08;
349200150128
349300150128       //?Pulizia subfile
349400150128         exsr PulS08;
349500150128
349600150128       //?Caricamento subfile
349700150128         exsr Ries08;
349800150128
349900150128         SflEnd = *on;
350000150128
350100150128       //?Imposto il posizionamento al primo rcd
350200150128         IF  S08nrr > 0;
350300150128           V08rcd = 1;
350400150128         ELSE;
350500150128           clear V08rcd;
350600150128         ENDIF;
350700150128
350800150128         V08csr = V08rcd;
350900150128
351000150128       ENDSR;
351100150128
351200150128       //--------------------------------------------------------------
351300150128       //?Pulizia Subfile S08.
351400150128       //--------------------------------------------------------------
351500150128       BEGSR PulS08;
351600150128
351700150128       //?Pulizia subfile
351800150128         SflDsp    = *on;
351900150128         SflDspCtl = *on;
352000150128         write KC01CW8;
352100150128         SflDspCtl = *off;
352200150128         SflEnd    = *off;
352300150128
352400150128         clear V08rcd;
352500150128         clear V08csr;
352600150128         clear S08nrr;
352700150128         clear V08msg;
352800150128
352900150128         ErrMessage  = *off;
353000150128         ErrGenerico = *off;
353100150128
353200150128         WindDspF = IndDspF;
353300150128         reset WindDspFb;
353400150128         IndDspF  = WindDspF;
353500150128
353600150128       ENDSR;
353700150128
353800150128       //--------------------------------------------------------------
353900150128       //?Caricamento Subfile S08.
354000150128       //--------------------------------------------------------------
354100150128       BEGSR RieS08;
354200150128
354300150128         xx = 1;
354400150128         FOR xx by 1 to 5;
354500150128           clear  KC01SW8;
354600150128           PosCurOPZ = *off;
354700150128           V08obj = Obiettivo(xx);
354800150128           V08des = Obiettivo(xx) + ' ' + DesObj(xx);
354900150128           S08nrr += 1;
355000150128           write  KC01SW8;
355100150128         ENDFOR;
355200150128
355300150128       ENDSR;
355400150128
355500150128       //--------------------------------------------------------------
355600150128       //?Gestione opzioni Subfile S08.
355700150128       //--------------------------------------------------------------
355800150128       BEGSR OpzS08;
355900150128
356000150128         xx = 1;
356100150128         FOR xx by 1 to 5;
356200150128           S08nrr = xx;
356300150128           chain S08nrr KC01SW8;
356400150128           IF  not %found;
356500150128             leave;
356600150128           ENDIF;
356700150128
356800150128           SELECT;
356900150128         //?- Nessuna opzione
357000150128           WHEN  V08opz = *blank;
357100150128
357200150128         //?- 1 = Scelta
357300150128           WHEN  V08opz = '1';
357400150216             exsr  Scelta08;
357500150216             leave;
357600150128
357700150128           OTHER;
357800150128             ErrMessage  = *on;
357900150128             ErrGenerico = *on;
358000150128             PosCurOPZ   = *on;
358100150216             V08msg = Msg(11);
358200150128             update KC01SW8;
358300150128             leave;
358400150128           ENDSL;
358500150128
358600150128         ENDFOR;
358700150128
358800150128       ENDSR;
358900150216
359000150216       //--------------------------------------------------------------
359100150216       //?Ritorno la scelta fatta da videata D08.
359200150216       //--------------------------------------------------------------
359300150216       BEGSR Scelta08;
359400150216
359500150216         SELECT;
359600150216         WHEN  H2nmfl = 'V02OBJ1';
359700150216           V02obj1 = V08obj;
359800150216           POScurOBJ1 = *on;
359900150216         WHEN  H2nmfl = 'V02OBJ2';
360000150216           V02obj2 = V08obj;
360100150216           POScurOBJ2 = *on;
360200150216         WHEN  H2nmfl = 'V02OBJ3';
360300150216           V02obj3 = V08obj;
360400150216           POScurOBJ3 = *on;
360500150216         WHEN  H2nmfl = 'V04OBJ1';
360600150216           V04obj1 = V08obj;
360700150216           POScurOBJ1 = *on;
360800150216         WHEN  H2nmfl = 'V04OBJ2';
360900150216           V04obj2 = V08obj;
361000150216           POScurOBJ2 = *on;
361100150216         WHEN  H2nmfl = 'V04OBJ3';
361200150216           V04obj3 = V08obj;
361300150216           POScurOBJ3 = *on;
361400150216         ENDSL;
361500150216
361600150216       ENDSR;
361700150302
361800150302       //--------------------------------------------------------------
361900150302       //?Gestione videata S09.
362000150302       //--------------------------------------------------------------
362100150302       BEGSR GesS09;
362200150302
362300150302       //?Inizializzazione videata
362400150302         IF  wInzS09;
362500150302           exsr InzS09;
362600150302           wInzS09 = *off;
362700150302         ENDIF;
362800150302
362900150302       //?Visualizzazione del SFL
363000150302         SflDsp = (S09nrr <= *zeros);
363100150302
363200150302       //?Emissione videata
363300150302         exfmt  KC01CW9;
363400150302         reset ErrMessage;
363500150302         reset ErrGenerico;
363600150302
363700150302       //?- Enter Controllo ed esco
363800150302         exsr OpzS09;
363900150302         IF  ErrGenerico;
364000150302           leavesr;
364100150302         ENDIF;
364200150302
364300150302      //?Videata sucessiva
364400150302         IF H2nmrc = 'KC01D02';
364500150302           Video = 'D2';
364600150302         ENDIF;
364700150302         IF H2nmrc = 'KC01W04';
364800150302           Video = 'D4';
364900150302         ENDIF;
365000150302
365100150302       ENDSR;
365200150302
365300150302       //--------------------------------------------------------------
365400150302       //?Inizializzazione Videata S09.
365500150302       //--------------------------------------------------------------
365600150302       BEGSR InzS09;
365700150302
365800150302       //?Pulizia subfile
365900150302         exsr PulS09;
366000150302
366100150302       //?Caricamento subfile
366200150302         exsr Ries09;
366300150302
366400150302         SflEnd = *on;
366500150302
366600150302       //?Imposto il posizionamento al primo rcd
366700150302         IF  S09nrr > 0;
366800150302           V09rcd = 1;
366900150302         ELSE;
367000150302           clear V09rcd;
367100150302         ENDIF;
367200150302
367300150302         V09csr = V09rcd;
367400150302
367500150302       ENDSR;
367600150302
367700150302       //--------------------------------------------------------------
367800150302       //?Pulizia Subfile S09.
367900150302       //--------------------------------------------------------------
368000150302       BEGSR PulS09;
368100150302
368200150302       //?Pulizia subfile
368300150302         SflDsp    = *on;
368400150302         SflDspCtl = *on;
368500150302         write KC01CW9;
368600150302         SflDspCtl = *off;
368700150302         SflEnd    = *off;
368800150302
368900150302         clear V09rcd;
369000150302         clear V09csr;
369100150302         clear S09nrr;
369200150302         clear V09msg;
369300150302
369400150302         ErrMessage  = *off;
369500150302         ErrGenerico = *off;
369600150302
369700150302         WindDspF = IndDspF;
369800150302         reset WindDspFb;
369900150302         IndDspF  = WindDspF;
370000150302
370100150302       ENDSR;
370200150302
370300150302       //--------------------------------------------------------------
370400150302       //?Caricamento Subfile S09.
370500150302       //--------------------------------------------------------------
370600150302       BEGSR RieS09;
370700150302
370800150302         xx = 1;
370900150302         FOR xx by 1 to 10;
371000150302           clear  KC01SW9;
371100150302           PosCurOPZ = *off;
371200150302           V09ttr = Trattative(xx);
371300150302           V09des = Trattative(xx) + ' ' + DesTtr(xx);
371400150302           S09nrr += 1;
371500150302           write  KC01SW9;
371600150302         ENDFOR;
371700150302
371800150302       ENDSR;
371900150302
372000150302       //--------------------------------------------------------------
372100150302       //?Gestione opzioni Subfile S09.
372200150302       //--------------------------------------------------------------
372300150302       BEGSR OpzS09;
372400150302
372500150302         xx = 1;
372600150302         FOR xx by 1 to 10;
372700150302           S09nrr = xx;
372800150302           chain S09nrr KC01SW9;
372900150302           IF  not %found;
373000150302             leave;
373100150302           ENDIF;
373200150302
373300150302           SELECT;
373400150302         //?- Nessuna opzione
373500150302           WHEN  V09opz = *blank;
373600150302
373700150302         //?- 1 = Scelta
373800150302           WHEN  V09opz = '1';
373900150302             exsr  Scelta09;
374000150302             leave;
374100150302
374200150302           OTHER;
374300150302             ErrMessage  = *on;
374400150302             ErrGenerico = *on;
374500150302             PosCurOPZ   = *on;
374600150302             V09msg = Msg(11);
374700150302             update KC01SW9;
374800150302             leave;
374900150302           ENDSL;
375000150302
375100150302         ENDFOR;
375200150302
375300150302       ENDSR;
375400150302
375500150302       //--------------------------------------------------------------
375600150302       //?Ritorno la scelta fatta da videata D09.
375700150302       //--------------------------------------------------------------
375800150302       BEGSR Scelta09;
375900150302
376000150304         IF H2nmrc = 'KC01D02';
376100150304           V02ttr = V09ttr;
376200150304         ENDIF;
376300150304         IF H2nmrc = 'KC01W04';
376400150304           V04ttr = V09ttr;
376500150304         ENDIF;
376600150302         POScurTTR = *on;
376700150302
376800150302       ENDSR;
376900141030
377000141030       //--------------------------------------------------------------
377100141030       //?Controllo Distretto.
377200141030       //--------------------------------------------------------------
377300141030       BEGSR Distretto;
377400141030
377500141030         IF  w001a = *blanks;
377600141030           leavesr;
377700141030         ENDIF;
377800141030
377900141030       //?Ricerca Distretto
378000141030         IF  %scan('?' : w001a) > 0;
378100141030           ErrGenerico = *on;
378200141030           PosCurCDI   = *on;
378300141030           idTabella = '17';
378400141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
378500141030                            tastoUscita);
378600141030           w001a = idElemento;
378700141030         ENDIF;
378800141030       //?Deve essere un Distretto valido
378900141030         clear xx;
379000141030         xx = %lookup(w001a : skDistretti);
379100141030         IF  xx = 0;
379200141030           ErrMessage  = *on;
379300141030           ErrGenerico = *on;
379400141030           PosCurCDI   = *on;
379500141030           wmsg = Msg(03);
379600141030           leavesr;
379700141030         ENDIF;
379800141030       //?Decodifico distretto
379900141030         w050a = skDesDist(xx);
380000141030       //?In carico all'Utente
380100141030         IF  %lookup (w001a:DIS) = 0;
380200141030           ErrMessage  = *on;
380300141030           ErrGenerico = *on;
380400141030           PosCurCDI   = *on;
380500141030           wmsg = %trim(Msg(03)) + '. Non in gestione';
380600141030           leavesr;
380700141030         ENDIF;
380800141030
380900141030       ENDSR;
381000141030
381100141030       //--------------------------------------------------------------
381200141030       //?Controllo Area.
381300141030       //--------------------------------------------------------------
381400141030       BEGSR Area;
381500141030
381600141030         IF  w003a = *blanks;
381700141030           leavesr;
381800141030         ENDIF;
381900141030
382000141030       //?Ricerca Area
382100141030         IF  %scan('?' : w003a) > 0;
382200141030           ErrGenerico = *on;
382300141030           PosCurCAR   = *on;
382400141104           idTabella = '05';
382500141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
382600141030                          tastoUscita);
382700141030           w003a = idElemento;
382800141030         ENDIF;
382900141104         IF  w003a = *blanks;
383000141104           leavesr;
383100141104         ENDIF;
383200141030       //?Accetto solo dati numerici
383300141030         xx = 1;
383400141030         FOR xx by 1 to %len(w003a);
383500141030           IF  %subst(w003a:xx:1) <> *blanks and
383600141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
383700141030             ErrMessage  = *on;
383800141030             ErrGenerico = *on;
383900141030             PosCurCAR   = *on;
384000141030             wmsg = Msg(04);
384100141030             leavesr;
384200141030           ENDIF;
384300141030         ENDFOR;
384400141030       //?Deve essere un'Area valida
384500141030         clear xx;
384600141030         w0030 = %dec(w003a:3:0);
384700141030         xx = %lookup(%editc(w0030:'X'):skAree);
384800141030         IF  xx = 0;
384900141030           ErrMessage  = *on;
385000141030           ErrGenerico = *on;
385100141030           PosCurCAR   = *on;
385200141030           wmsg = Msg(04);
385300141030           leavesr;
385400141030         ENDIF;
385500141030       //?Decodifico area
385600141030         w050a = skDesArea(xx);
385700141030       //?Deve essere un'area in carico all'utente
385800141030         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
385900141030           ErrGenerico = *on;
386000141030           ErrMessage  = *on;
386100141030           PosCurCAR   = *on;
386200141030           wmsg = %trim(Msg(04)) + '. Non in gestione';
386300141030           leavesr;
386400141030         ENDIF;
386500141030
386600141030       ENDSR;
386700141030
386800141030       //--------------------------------------------------------------
386900141030       //?Controllo Filiale.
387000141030       //--------------------------------------------------------------
387100141030       BEGSR Filiale;
387200141030
387300141030         IF  w003a = *blanks;
387400141030           leavesr;
387500141030         ENDIF;
387600141030
387700141030       //?Ricerca Filale
387800141030         IF  %scan('?' : w003a) > 0;
387900141030           clear pinFIL;
388000141030           clear pinFAG;
388100141030           clear pinDES;
388200141030           clear pinDIT;
388300141030           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
388400141030           IF pinFIL > *zeros;
388500141030             w003a = pinFIL;
388600141030             w050a = pinDES;
388700141030           ENDIF;
388800141030           ErrGenerico = *on;
388900141030           PosCurFIL   = *on;
389000141030         ENDIF;
389100141030       //?Accetto solo dati numerici
389200141030         xx = 1;
389300141030         FOR xx by 1 to %len(w003a);
389400141030           IF  %subst(w003a:xx:1) <> *blanks and
389500141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
389600141030             ErrMessage  = *on;
389700141030             ErrGenerico = *on;
389800141030             PosCurFIL   = *on;
389900141030             wmsg = Msg(05);
390000141030             leavesr;
390100141030           ENDIF;
390200141030         ENDFOR;
390300141030         w0030 = %dec(w003a:3:0);
390400141030       //?Deve esistere la Filiale
390500141030         chain w0030 AZORG01L;
390600141030         IF  not %found(AZORG01L) or ORGfva <> *blanks;
390700141030           ErrMessage  = *on;
390800141030           ErrGenerico = *on;
390900141030           PosCurFIL   = *on;
391000141030           wmsg = Msg(05);
391100141030           leavesr;
391200141030         ENDIF;
391300141030       //?Decodifico filiale
391400141030         w050a = ORGdes;
391500141030       //?Deve essere una filiale in carico all'utente
391600141030         IF  %lookup(%editc(w0030:'X'):POG) = 0;
391700141030           ErrGenerico = *on;
391800141030           ErrMessage  = *on;
391900141030           PosCurFIL   = *on;
392000141030           wmsg = %trim(Msg(05)) + '. Non in gestione';
392100141030           leavesr;
392200141030         ENDIF;
392300141104       //?Se presente anche il distretto o l'area devono essere congruenti
392400141104         IF  V02cdi <> *blanks and V02fil <> *blanks;
392500141105           IF  V02cdi <> ORGfl3;
392600141104             ErrGenerico = *on;
392700141104             ErrMessage  = *on;
392800141104             PosCurFIL   = *on;
392900141104             wmsg = %trim(Msg(05)) + '. Non congruente con il distretto';
393000141104             leavesr;
393100141104           ENDIF;
393200141104           IF  V02car <> %editc(ORGcar:'X');
393300141104             ErrGenerico = *on;
393400141104             ErrMessage  = *on;
393500141104             PosCurFIL   = *on;
393600141104             wmsg = %trim(Msg(05)) + '. Non congruente con l''area';
393700141104             leavesr;
393800141104           ENDIF;
393900141104         ENDIF;
394000141030
394100141030       ENDSR;
394200141030
394300141030       //--------------------------------------------------------------
394400141030       //?Controllo Commerciale.
394500141030       //--------------------------------------------------------------
394600141030       BEGSR Commerciale;
394700141030
394800141030         IF  w007a = *blanks;
394900141030           leavesr;
395000141030         ENDIF;
395100141030
395200141030       //?Ricerca Commerciale
395300141030         IF  %scan('?' : w007a) > 0;
395400141030           clear TRMK43DS;
395500141030           IMK43solu = 'S';
395600141030           IMK43fil = DUTpou;
395700141030           trmk43r (kpjba : TRMK43DS);
395800141030           IF  OMK43err = *off and OMK43fxx = *blanks;
395900141030             w007a = %editc(OMK43cmm:'X');
396000141030             w050a = OMK43des;
396100141030           ENDIF;
396200141030           ErrGenerico = *on;
396300141030           PosCurCMM   = *on;
396400141030         ENDIF;
396500141030       //?Accetto solo dati numerici
396600141030         xx = 1;
396700141030         FOR xx by 1 to %len(w007a);
396800141030           IF  %subst(w007a:xx:1) <> *blanks and
396900141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
397000141030             ErrMessage  = *on;
397100141030             ErrGenerico = *on;
397200141030             PosCurCMM   = *on;
397300150216             wmsg = Msg(06);
397400141030             leavesr;
397500141030           ENDIF;
397600141030         ENDFOR;
397700141030         w0070 = %dec(w007a:7:0);
397800141030       //?Deve esistere il Commerciale
397900141030         chain (w0070) AZCMM01L;
398000141030         IF  not %found(AZCMM01L);
398100141030           ErrMessage  = *on;
398200141030           ErrGenerico = *on;
398300141030           PosCurCMM   = *on;
398400150216           wmsg = Msg(06);
398500141030           leavesr;
398600141030         ENDIF;
398700141030       //?Decodifico Commerciale
398800141030         w050a = CMMdes;
398900141030       //?Deve essere un Commerciale in carico all'utente
399000141030         clear TNTAA1DS;
399100141119         ITAA1caut = '§utegtc';
399200141030         ITAA1cmm  = w0070;
399300141030         kpjbu = TNTAA1DS;
399400141030         tntaa1c (kpjba);
399500150217         TNTAA1DS = kpjbu;
399600141030         IF  OTAA1fabi = 'N';
399700141030           ErrGenerico = *on;
399800141030           ErrMessage  = *on;
399900141030           PosCurCMM   = *on;
400000150216           wmsg = %trim(Msg(06)) + '. Non in gestione';
400100141030           leavesr;
400200141030         ENDIF;
400300141030       //?Deve essere un Commerciale Unificante
400400141030         IF  w0070 <> CMMuni;
400500141030           ErrGenerico = *on;
400600141030           ErrMessage  = *on;
400700141030           PosCurCMM   = *on;
400800150216           wmsg = %trim(Msg(06)) + '. Non è un Commerciale Unificante';
400900141030           leavesr;
401000141030         ENDIF;
401100141030
401200141030       ENDSR;
401300141030
401400141030       //--------------------------------------------------------------
401500141030       //?Controllo Cliente.
401600141030       //--------------------------------------------------------------
401700141030       BEGSR Cliente;
401800141030
401900141030         IF  w007a = *blanks;
402000141030           leavesr;
402100141030         ENDIF;
402200141030
402300141030       //?Ricerca Cliente
402400141030         IF  %scan('?' : w007a) > 0;
402500141030           clear TNTAI1DS;
402600141030           ITAI1ric = 'S';
402700141030           ITAI1abi = wabi;
402800141112           ITAI1aut = '*C';
402900141030           tntai1r (kpjba : TNTAI1DS);
403000141030           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
403100141030               OTAI1err <> *blanks;
403200141107             clear V02ksu;
403300141030           ELSE;
403400141030             w007a = %editc(OTAI1ksc:'X');
403500141030           ENDIF;
403600141030           ErrGenerico = *on;
403700141107           PosCurKSU   = *on;
403800141030         ENDIF;
403900141030       //?Accetto solo dati numerici
404000141030         xx = 1;
404100141030         FOR xx by 1 to %len(w007a);
404200141030           IF  %subst(w007a:xx:1) <> *blanks and
404300141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
404400141030             ErrMessage  = *on;
404500141030             ErrGenerico = *on;
404600141107             PosCurKSU   = *on;
404700150216             wmsg = Msg(07);
404800141030             leavesr;
404900141030           ENDIF;
405000141030         ENDFOR;
405100141030         w0070 = %dec(w007a:7:0);
405200141030       //?Deve esistere il Cliente
405300141030         clear TIBS69DS;
405400141030         clear ACOrag;
405500141030         I69kac = %dec(w007a:7:0);
405600141030         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
405700141030         IF  O69err <> *blanks;
405800141030           ErrMessage  = *on;
405900141030           ErrGenerico = *on;
406000141107           PosCurKSU   = *on;
406100150216           wmsg      = Msg(07);
406200141030         ENDIF;
406300141030       //?Decodifico Cliente
406400141030         w050a = ACOrag;
406500141030       //?Deve essere un Cliente in carico all'utente
406600141030         clear TNTAA1DS;
406700141119         ITAA1caut = '§utegtc';
406800141030         ITAA1ksc  = %dec(w007a:7:0);
406900141030         kpjbu = TNTAA1DS;
407000141030         tntaa1c (kpjba);
407100150217         TNTAA1DS = kpjbu;
407200141030         IF  OTAA1fabi = 'N';
407300141030           ErrGenerico = *on;
407400141030           ErrMessage  = *on;
407500141107           PosCurKSU   = *on;
407600150216           wmsg = %trim(Msg(07)) + '. Non in gestione';
407700141030           leavesr;
407800141030         ENDIF;
407900141030
408000141030       ENDSR;
408100141030
408200141030       //--------------------------------------------------------------
408300141030       //?Controllo Importanza Cliente.
408400141030       //--------------------------------------------------------------
408500141030       BEGSR ImpCliente;
408600141030
408700141030         IF  w001a = *blanks;
408800141030           leavesr;
408900141030         ENDIF;
409000141030
409100141030         IF  %scan('?' : w001a) > 0;
409200141030           ErrGenerico = *on;
409300141030           idTabella = 'IC';
409400141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
409500141030                          tastoUscita);
409600141030           w001a = idElemento;
409700141030         ENDIF;
409800141030         IF  %lookup(w001a : skIC) = 0;
409900141030           ErrMessage  = *on;
410000141030           ErrGenerico = *on;
410100141030           PosCurCLV1  = *on;
410200150216           wmsg = Msg(08);
410300141030           leavesr;
410400141030         ENDIF;
410500141030
410600141030       ENDSR;
410700141127
410800141127       //--------------------------------------------------------------
410900141127       //?Controllo Obiettivi.
411000141127       //--------------------------------------------------------------
411100150128       BEGSR ContrObj;
411200141127
411300150128       //?Se Obiettivo inizio NO "di cui"
411400150119         IF  wfobj = 'I' and
411500150119            (wfobj1 = 'P' or wfobj1 = 'F' or
411600150119             wfobj2 = 'P' or wfobj2 = 'F');
411700141127           ErrMessage  = *on;
411800141127           ErrGenerico = *on;
411900150216           wmsg = Msg(14);
412000141127           IF  wfobj1 <> *blanks;
412100141127             PosCurOBJ1  = *on;
412200141127             leavesr;
412300141127           ENDIF;
412400141127           IF  wfobj2 <> *blanks;
412500141127             PosCurOBJ2  = *on;
412600141127             leavesr;
412700141127           ENDIF;
412800141127         ENDIF;
412900141127       //?Se Obiettivo proposto NO "di cui" Finale
413000141127         IF  wfobj = 'P' and (wfobj1 = 'F' or wfobj2 = 'F');
413100141127           ErrMessage  = *on;
413200141127           ErrGenerico = *on;
413300150216           wmsg = Msg(15);
413400141127           IF  wfobj1 = 'F';
413500141127             PosCurOBJ1  = *on;
413600141127             leavesr;
413700141127           ENDIF;
413800141127           IF  wfobj2 = 'F';
413900141127             PosCurOBJ2  = *on;
414000141127             leavesr;
414100141127           ENDIF;
414200141127         ENDIF;
414300150128       //?Deve essere un obiettivo valido
414400150128         IF  wfobj1 <> *blanks and %lookup (wfobj1:Obiettivo) = 0;
414500150128           ErrMessage  = *on;
414600150128           ErrGenerico = *on;
414700150128           PosCurOBJ1  = *on;
414800150216           wmsg = Msg(13);
414900150128           leavesr;
415000150128         ENDIF;
415100150128         IF  wfobj2 <> *blanks and %lookup (wfobj2:Obiettivo) = 0;
415200150128           ErrMessage  = *on;
415300150128           ErrGenerico = *on;
415400150128           PosCurOBJ2  = *on;
415500150216           wmsg = Msg(13);
415600150128           leavesr;
415700150128         ENDIF;
415800150128
415900141127       //?Non posso accettare 2 obiettivi uguali
416000141127         IF  wfobj1 = wfobj2 and (wfobj1 <> *blanks or
416100141127                                  wfobj2 <> *blanks);
416200141127           ErrMessage  = *on;
416300141127           ErrGenerico = *on;
416400141127           PosCurOBJ1  = *on;
416500150216           wmsg = Msg(16);
416600141127           leavesr;
416700141127         ENDIF;
416800141127       //?Devono essere tutti e 2 impostati
416900141127         IF  (wfobj1 = *blanks and wfobj2 <> *blanks) or
417000141127             (wfobj1 <> *blanks and wfobj2 = *blanks);
417100141127           ErrMessage  = *on;
417200141127           ErrGenerico = *on;
417300150216           wmsg = Msg(17);
417400141127           IF  wfobj1 = *blanks;
417500141127             PosCurOBJ1  = *on;
417600141127             leavesr;
417700141127           ENDIF;
417800141127           IF  wfobj2 = *blanks;
417900141127             PosCurOBJ2  = *on;
418000141127             leavesr;
418100141127           ENDIF;
418200141127         ENDIF;
418300150127
418400150127       //?Se il primo byte del campo formula è vuoto controllo se è
418500150127       //?uno dei campi validi e sistemo il campo
418600150127         IF  wfformula <> *blanks and %subst(wfformula:1:1) = *blanks
418700150127             and %check(ValoriFormula:%subst(wfformula:2:1)) = *zeros;
418800150127           %subst(wfformula:1:1) = %subst(wfformula:2:1);
418900150127           %subst(wfformula:2:1) = *blanks;
419000150127         ENDIF;
419100141127
419200150119       //?La formula deve essere una formula valida
419300150119         IF  %lookup (wfformula:Formula) = 0 and wfobj1 <> *blanks;
419400141127           ErrMessage  = *on;
419500141127           ErrGenerico = *on;
419600150119           PosCurFORM  = *on;
419700150216           wmsg = Msg(18);
419800141127           leavesr;
419900141127         ENDIF;
420000141128
420100150121       //?Se ci sono gli obiettivi devo avere anche la formula
420200141128         IF  wfobj1 <> *blanks and wfobj2 <> *blanks and
420300150119             wfformula = *blanks;
420400141128           ErrMessage  = *on;
420500141128           ErrGenerico = *on;
420600150119           PosCurFORM  = *on;
420700150216           wmsg = Msg(18);
420800141128           leavesr;
420900141128         ENDIF;
421000150121
421100150121       //?Se ho la formula devo avere anche gli obiettivi
421200150121         IF  wfobj1 = *blanks and wfobj2 = *blanks and
421300150121             wfformula <> *blanks;
421400150121           ErrMessage  = *on;
421500150121           ErrGenerico = *on;
421600150122           PosCurOBJ1  = *on;
421700150216           wmsg = Msg(19);
421800150121           leavesr;
421900150121         ENDIF;
422000150213
422100150213       //?Deve essere un obiettivo valido
422200150213         IF  wfobj3 <> *blanks and %lookup (wfobj3:Obiettivo) = 0;
422300150213           ErrMessage  = *on;
422400150213           ErrGenerico = *on;
422500150213           PosCurOBJ3  = *on;
422600150216           wmsg = Msg(13);
422700150213           leavesr;
422800150213         ENDIF;
422900150213       //?Almeno 1 dei 2 valori deve essere presente
423000150213         IF  wfobj3 <> *blanks and wobj3da = 0 and wobj3a = 0;
423100150216           //ErrMessage  = *on;
423200150216           //ErrGenerico = *on;
423300150216           //PosCurOBJ3DA = *on;
423400150216           //wmsg = Msg(20);
423500150216           //leavesr;
423600150216         ENDIF;
423700150213       //?Se impostate % serve il flag obiettivo
423800150213         IF  wfobj3 = *blanks and (wobj3da <> 0 or wobj3a <> 0);
423900150213           ErrMessage  = *on;
424000150213           ErrGenerico = *on;
424100150213           PosCurOBJ3  = *on;
424200150216           wmsg = Msg(13);
424300150213           leavesr;
424400150213         ENDIF;
424500150213         IF  wobj3da > wobj3a;
424600150213           ErrMessage  = *on;
424700150213           ErrGenerico = *on;
424800150213           PosCurOBJ3DA = *on;
424900150216           wmsg = Msg(21);
425000150213           leavesr;
425100150213         ENDIF;
425200141127
425300141127       ENDSR;
425400150302
425500150302       //--------------------------------------------------------------
425600150302       //?Controllo Scelta Trattative.
425700150302       //--------------------------------------------------------------
425800150302       BEGSR SceltaTtr;
425900150302
426000150302         IF  w002a = *blanks;
426100150302           leavesr;
426200150302         ENDIF;
426300150302
426400150302       //?Deve essere una Scelta valida
426500150302         clear xx;
426600150302         xx = %lookup(w002a : Trattative);
426700150302         IF  xx = 0;
426800150302           ErrMessage  = *on;
426900150302           ErrGenerico = *on;
427000150302           PosCurTTR   = *on;
427100150302           wmsg = Msg(24);
427200150302           leavesr;
427300150302         ENDIF;
427400150302
427500150302       ENDSR;
427600141121
427700141121       //--------------------------------------------------------------
427800150218       //?Richiamo pgm TRKC76R.
427900141121       //--------------------------------------------------------------
428000150218       BEGSR CallTRKC76;
428100141121
428200150218         wTRKC76 = *on;
428300141121
428400150218         clear TRKC76DS;
428500150303         IKC76ric = wtpric;
428600150218         IKC76ncm = CMIncm;
428700150218         IKC76ksu = CMIksu;
428800150218         IKC76ksc = CMIksc;
428900150218         IKC76cpo = CMIcpo;
429000150303         IKC76flgp = wflgp;
429100150220         IKC76acm = wFase;
429200150218         IKC76istat = CMIist;
429300150218
429400150218         trkc76r (kpjba:TRKC76DS);
429500150203
429600150203       ENDSR;
429700170303
429800170303       //--------------------------------------------------------------
429900170303       //?Richiamo pgm TRKC78R.
430000170303       //--------------------------------------------------------------
430100170303       BEGSR CallTRKC78;
430200170303
430300170303         clear TRKC78DS;
430400170303         IKC78ncm = CMIncm;
430500170303         IKC78ksu = CMIksu;
430600170303         IKC78ksc = CMIksc;
430700170303         IKC78cpo = CMIcpo;
430800170303
430900170303         trkc78r (kpjba:TRKC78DS);
431000170303
431100170303       ENDSR;
431200141028
431300141028       //--------------------------------------------------------------
431400141028       //?Operazioni finali.
431500141028       //--------------------------------------------------------------
431600141028       BEGSR RoutEnd;
431700150218
431800150218       //?Se richiamato chiudo i file per il pgm TRKC76R
431900150218         IF  wTRKC76;
432000150218           clear TRKC76DS;
432100150218           IKC76ric = 'C';
432200150218           trkc76r (kpjba:TRKC76DS);
432300150218         ENDIF;
432400141028
432500141028         *inLR = *on;
432600141028         return;
432700141028
432800141028       ENDSR;
432900141028
433000141028      /end-free
433100141028
433200141028       //--------------------------------------------------------------
433300141028       //?Schiere a tempo di compilazione.
433400141028       //--------------------------------------------------------------
433500150116
433600150116** -- Formule / DesForm -----------------------------------------------------*
433700150116=  : Uguale
433800150116>  : Maggiore
433900150116<  : Minore
434000150116>= : Maggiore Uguale
434100150116<= : Minore Uguale
434200150116<> : Diverso
434300150128** -- Obiettivo / DesObj ----------------------------------------------------*
434400150128I= Obiettivo Inizio
434500150128P= Obiettivo Proposto
434600150128F= Obiettivo Finale
434700150128T= Andamento Trattativa
434800150128C= Andamento Confronto Fatturazione
434900150302** -- Trattative / DesTtr----------------------------------------------------*
435000150302A = Avviate
435100150302AO= Avviate con Offerta
435200150302AN= Avviate senza Offerta
435300150302C = Chiuse
435400150302CN= Chiuse No Positive
435500150302CP= Chiuse Positive
435600150302SI= Tutte
435700150302T = Trattative non Forzate
435800150302F = Trattative Forzate
435900150302NO= Non avviate
436000141028** -- MSG -------------------------------------------------------------------*
436100141024Utente non abilitato alla Funzione.                                            1
436200141024Campagna Commerciale errata                                                    2
436300141027Distretto Errato                                                               3
436400141027Area Errata                                                                    4
436500141027Filiale Errata                                                                 5
436600150216Commerciale Unificante Errato                                                  6
436700150216Codice Cliente Errato                                                          7
436800150216Codice Importanza Clienti Errato                                               8
436900150216Immettere il Delta                                                             9
437000150216Delta previsto "DAL" incongruente con Delta previsto "AL"                     10
437100150216Opzione errata                                                                11
437200150216La modifica è possibile solo dal            al                                12
437300150216Impostare un Obiettivo/Andamento valido (F2)                                  13
437400150216Se richiesto Obiettivo Inizio non richiedere il "di cui"                      14
437500150216Non è possibile il "di cui" Finale se richiesto Obiettivo Proposto            15
437600150216Non è possibile il confronto tra 2 obiettivi uguali                           16
437700150216Impostare tutti e 2 gli obiettivi/andamento da confrontare                    17
437800150216Impostare una formula valida (F1=Legenda Formula)                             18
437900150216Impostare gli obiettivi/andamento                                             19
438000150216Immettere la % Obiettivo/Andamento                                            20
438100150216Obiettivo/Andamento "DAL" incongruente con Obiettivo/Andamento "AL"           21
438200150216Per poter fare F2 posizionarsi su obiettivo/andamento da inserire             22
438300150225Trattativa Forzata possibile solo dal            al                           23
438400150302Scelta trattativa non valida                                                  24
438500150611Impostare l'aera da elaborare                                                 25
