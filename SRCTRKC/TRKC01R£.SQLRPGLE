000100170207     /*END
000200141028      //--------------------------------------------------------------
000300150112      //?TRKC01R - GESTIONE CAMPAGNE COMMERCIALI
000400141028      //--------------------------------------------------------------
000500141028
000600141028     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700141029     h dftactgrp(*no) actgrp(*caller)
000800141028
000900141028      //---------------------------------------------------------------
001000141028      //?Dichiarazione file.
001100141028      //---------------------------------------------------------------
001200141024
001300141024      // - Organigramma
001400141024     fAZORG01L  if   e           k disk
001500141029     fAZORG02L  if   e           k disk    rename(AZORG:AZORG02)
001600141029
001700141029      // - Anagrafica Commerciali
001800141029     fAZCMM01L  if   e           k disk
001900141024
002000141024      // - Tabelle
002100141024     fTABEL00F  if   e           k disk
002200141028
002300141027      // - File Fasi avanzamento Campagna
002400141024     fTICMF01L  if   e           k disk
002500141114
002600141114      // - File anagrafiche Campagne
002700141114     fTICMP01L  if   e           k disk
002800141028
002900141024      // - Video Gestione Campagne
003000150112     fTRKC01D   cf   e             workstn
003100150112     f                                     sfile(KC01S03 : S03nrr)
003200150116     f                                     sfile(KC01SW6 : S06nrr)
003300150128     f                                     sfile(KC01SW8 : S08nrr)
003400150302     f                                     sfile(KC01SW9 : S09nrr)
003500141028     f                                     indds(IndDspF)
003600141028     f                                     infds(InfDspF)
003700141028
003800141028      //---------------------------------------------------------------
003900141028      //?Definizione costanti.
004000141028      //---------------------------------------------------------------
004100141028
004200141028      // - Tasti funzionali a video
004300141028     d c_F01           c                   const(x'31')
004400141028     d c_F02           c                   const(x'32')
004500141028     d c_F03           c                   const(x'33')
004600141028     d c_F04           c                   const(x'34')
004700141028     d c_F05           c                   const(x'35')
004800141028     d c_F06           c                   const(x'36')
004900141028     d c_F07           c                   const(x'37')
005000141028     d c_F08           c                   const(x'38')
005100141028     d c_F09           c                   const(x'39')
005200141028     d c_F10           c                   const(x'3A')
005300141028     d c_F11           c                   const(x'3B')
005400141028     d c_F12           c                   const(x'3C')
005500141028     d c_F13           c                   const(x'B1')
005600141028     d c_F14           c                   const(x'B2')
005700141028     d c_F15           c                   const(x'B3')
005800141028     d c_F16           c                   const(x'B4')
005900141028     d c_F17           c                   const(x'B5')
006000141028     d c_F18           c                   const(x'B6')
006100141028     d c_F19           c                   const(x'B7')
006200141028     d c_F20           c                   const(x'B8')
006300141028     d c_F21           c                   const(x'B9')
006400141028     d c_F22           c                   const(x'BA')
006500141028     d c_F23           c                   const(x'BB')
006600141028     d c_F24           c                   const(x'BC')
006700141028     d c_Enter         c                   const(x'F1')
006800141028     d c_RollDown      c                   const(x'F4')
006900141028     d c_RollUp        c                   const(x'F5')
007000141028
007100141028     d Digits          c                   const('0123456789')
007200141029
007300141029     d FaseObj         c                   const(' 10')
007400141111     d FaseObjProp     c                   const(' 20')
007500141111     d FaseObjFine     c                   const(' 30')
007600150220     d FaseObjTf       c                   const(' TF')
007700141113     d FaseObjTtr      c                   const(' TR')
007800141113     d FaseObjCf       c                   const(' CF')
007900141114     d FaseChiudi      c                   const(' 90')
008000150128
008100150128     d Causale         c                   const('NA')
008200141030
008300141030     d visHI           c                   const(X'22')
008400141030     d visRI           c                   const(X'21')
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
017100141028        // - Indicatori di errore
017200141024     d  PosCurNCM                     1n   overlay(IndDspF : 50)
017300141027     d  PosCurCDI                     1n   overlay(IndDspF : 51)
017400141027     d  PosCurCAR                     1n   overlay(IndDspF : 52)
017500141027     d  PosCurFIL                     1n   overlay(IndDspF : 53)
017600141027     d  PosCurCMM                     1n   overlay(IndDspF : 54)
017700141107     d  PosCurKSU                     1n   overlay(IndDspF : 55)
017800141028     d  PosCurCLV1                    1n   overlay(IndDspF : 56)
017900141028     d  PosCurCLV2                    1n   overlay(IndDspF : 57)
018000141028     d  PosCurCLV3                    1n   overlay(IndDspF : 58)
018100141028     d  PosCurCLV4                    1n   overlay(IndDspF : 59)
018200141028     d  PosCurCLV5                    1n   overlay(IndDspF : 60)
018300141028     d  PosCurDELda                   1n   overlay(IndDspF : 61)
018400141028     d  PosCurDELa                    1n   overlay(IndDspF : 62)
018500141028     d  PosCurOPZ                     1n   overlay(IndDspF : 65)
018600141127     d  PosCurOBJ1                    1n   overlay(IndDspF : 66)
018700141127     d  PosCurOBJ2                    1n   overlay(IndDspF : 67)
018800150119     d  PosCurFORM                    1n   overlay(IndDspF : 68)
018900150213     d  PosCurOBJ3                    1n   overlay(IndDspF : 69)
019000150213     d  PosCurOBJ3DA                  1n   overlay(IndDspF : 70)
019100150302     d  PosCurTTR                     1n   overlay(IndDspF : 71)
019200141028        // - Indicatori di errore generico
019300141028     d  ErrGenerico                   1n   overlay(IndDspF : 99)
019400141028
019500141028     d WindDspF        ds                  inz
019600141028     d  WindDspFa              1     49    inz(*zero)
019700141028     d  WindDspFb             50     99    inz(*zero)
019800141028
019900141028      // - Parametri ricevuti
020000141028     d KPJBA         e ds
020100141105
020200150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
020300150112     d TRKC02DS      e ds
020400141107
020500150112      // - Richiamo pgm TRKC03R - Storico Fasi
020600150112     d TRKC03DS      e ds
020700141111
020800150112      // - Richiamo pgm TRKC06R - Aggiunta Cliente in campagna
020900150112     d TRKC06DS      e ds
021000141114
021100150112      // - Richiamo pgm TRKC07R - Variazione Obiettivo
021200150112     d TRKC07DS      e ds
021300150127
021400150127      // - Richiamo pgm TRKC10R - Gestione Note
021500150127     d TRKC10DS      e ds
021600141127
021700141127      // - Scrittura fase
021800150112     d TRKC71DS      e ds                  inz
021900141114
022000150115      // - Verifica quale obiettivo variare
022100150112     d TRKC72DS      e ds                  inz
022200150218
022300150218      // - Dati da Visualizzare o Parzializzazioni Trattative
022400150218     d TRKC76DS      e ds                  inz
022500141114
022600141114      // - File Clienti in Campagna Commerciale
022700141114     d TICMC00F      e ds                  extname(TICMC00F)
022800141028
022900141028      // - File INFO Clienti in Campagna Commerciale
023000141028     d TICMI00F      e ds                  extname(TICMI00F)
023100150115
023200150115      // - Ricerca/Controllo tabelle
023300150115     d TIBS02DS      e ds                  inz
023400141028
023500141028      // - Reperimento dati utente
023600141027     d TIBS34DS      e ds
023700141027
023800141027      // - Reperimento dati Anagrafica Clienti
023900141027      /copy gaitrasrc/srcprotopi,TIBS69R
024000141103
024100141103      // - Delta cliente unificante
024200141103     d TISE60DS      e ds
024300141103     d  skKSA                 50    161    dim(28)
024400141024
024500141024      // - Controllo abilitazioni
024600141024     d TNTAA1DS      e ds                  inz
024700141027
024800141027      // - Ricerca Anagrafica Clienti
024900141027     d TNTAI1DS      e ds                  inz
025000141103
025100141103      // - Int. Anagrafica Clienti
025200141103     d TNTAI2DS      e ds                  inz
025300141103
025400141103      // - Gestione trattative commerciali
025500141103     d TNTA88DS      e ds                  inz
025600141024
025700141024      // - Reperimento filiali gestite dall'utente
025800141028     d TRUL31DS      e ds
025900141029     d  POG                   10    759    DIM(250)
026000141027     d TRUL31DS2     e ds
026100141029     d  DIS                    1     20    DIM(20)
026200141029     d  CAR                   22    171    DIM(50)
026300150115
026400150115      // - Tabella ECM - Causali chiusura campagna
026500150115     d dECM          e ds
026600141027
026700141027      // - Tabella 05 - Aree
026800141027     d ds05          e ds
026900141027
027000141027      // - Tabella 17 - Distretti
027100141027     d ds17          e ds
027200141117
027300141117      // - Tabella IC - Importanza Clienti
027400141117     d dsIC          e ds
027500141103
027600141103      // - DS per passaggio clienti PACKED per int. delta
027700141103     d                 ds
027800141126     d  dsKSC                  1      4p 0
027900141103     d  dsKSA                  1      4
028000141028
028100141028      //---------------------------------------------------------------
028200141028      //?Definizione variabili globali.
028300141028      //---------------------------------------------------------------
028400141028
028500141028      // - Flags booleani
028600141024     d EndS03          s               n   inz(*off)
028700141024     d ErrGrave        s               n   inz(*off)
028800141024     d Fine            s               n   inz(*off)
028900141119     d Primo           s               n   inz(*off)
029000141124     d RecOK           s               n   inz(*off)
029100150116     d TroppiRcd       s               n   inz(*off)
029200141028     d wEnd            s               n   inz(*off)
029300141113     d wclv            s               n   inz(*off)
029400141029     d wInzD02         s               n   inz(*off)
029500141030     d wInzD04         s               n   inz(*off)
029600141118     d wInzD05         s               n   inz(*off)
029700150128     d wInzD07         s               n   inz(*off)
029800141029     d wInzS03         s               n   inz(*off)
029900150116     d wInzS06         s               n   inz(*off)
030000150128     d wInzS08         s               n   inz(*off)
030100150302     d wInzS09         s               n   inz(*off)
030200141103     d wMaxNrr         s               n   inz(*off)
030300141103     d wokTtr          s               n   inz(*off)
030400150218     d wTRKC76         s               n   inz(*off)
030500141028
030600141028      // - Indici di schiera
030700141029     d xx              s              4s 0 inz
030800141028
030900141028      // - Campi associati al video
031000141029     d Video           s              2a   inz('D2')
031100141029     d S03nrr          s              4s 0 inz
031200150116     d S06nrr          s              4s 0 inz
031300150128     d S08nrr          s              4s 0 inz
031400150302     d S09nrr          s              4s 0 inz
031500141112     d sav_S03nrr      s              4s 0 inz
031600141028
031700141028       // - Stringa SQL da eseguire
031800141028     d wSQL            s           2048    Varying        inz
031900141028
032000141028      // - Campi di comodo data
032100141024     d Data_EUR        s               d   datfmt(*eur)
032200141024     d Data_ISO        s               d   datfmt(*iso)
032300150203     d wData           s              8s 0 inz
032400141028
032500141028      // - Campi di comodo
032600141029     d Oggi            s              8s 0 inz
032700150128     d savformula      s                   like(V02formula) inz
032800141027     d wabi            s                   like(OTAA1cabi) inz
032900141112     d wdela           s                   like(CMIpde) inz
033000141112     d wdelda          s                   like(CMIpde) inz
033100150220     d wfase           s                   like(CMFacm) inz
033200150303     d wflgp           s                   like(IKC76flgp) inz
033300150128     d wfobj           s                   like(V02obj) inz
033400150128     d wfobj1          s                   like(V02obj1) inz
033500150128     d wfobj2          s                   like(V02obj2) inz
033600150213     d wfobj3          s                   like(V02obj3) inz
033700150128     d wfformula       s                   like(V02formula) inz
033800141029     d wimp            s             13s 2 inz
033900141030     d wmsg            s             78a   inz
034000150213     d wobj3da         s                   like(CMFpea) inz
034100150213     d wobj3a          s                   like(CMFpea) inz
034200150128     d wora            s                   like(CMFhfc) inz
034300150203     d wperc           s                   like(CMFpea) inz
034400150303     d wtpric          s                   like(IKC76ric) inz
034500150128     d wvalobj1        s                   like(CMFpea) inz
034600150128     d wvalobj2        s                   like(CMFpea) inz
034700150213     d wvalobj3        s                   like(CMFpea) inz
034800141030     d w001a           s              1a   inz
034900150302     d w002a           s              2a   inz
035000150218     d w0020           s              2s 0 inz
035100141030     d w003a           s              3a   inz
035200141029     d w0030           s              3s 0 inz
035300141030     d w007a           s              7a   inz
035400141029     d w0070           s              7s 0 inz
035500141030     d w050a           s             50a   inz
035600141024
035700141024      // - Campi x ricerca tabelle TABEL
035800141024     d idTabella       s              2
035900141024     d Ordinamento     s              1
036000141024     d idElemento      s              8
036100141024     d TastoUscita     s              1
036200141027
036300141027       // - Parametri per ricerca Filiale
036400141027     d pinFIL          s              3
036500141027     d pinFAG          s              1
036600141027     d pinDES          s             25
036700141027     d pinDIT          s              3
036800141027
036900141027      // - Parametri per ricerca Commerciale Unificante
037000141029      /copy gaitrasrc/srcprotopi,TRMK43R_1
037100141028
037200141028      // ----------------------------------------------------------------------
037300141027      //?DATI per ordinamento subfile
037400141028      // ----------------------------------------------------------------------
037500141028     d MaxKey          c                   5
037600141028     d Ascendente      c                   1
037700141028     d Discendente     c                   2
037800141028     d Carattere       c                   6
037900141028     d Numerico        c                   8
038000141028     d Put             c                   1
038100141028     d EndPut          c                   2
038200141028     d Get             c                   3
038300141028      *************************************************************************
038400141028      * Campi utili:
038500141028      *     nrr        - Numero relativo di record del Subfile
038600141028      *     SizeList   - Dimensione della lista
038700141028      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
038800141028      *************************************************************************
038900141028     d NotUsed         s             16A
039000141028     d ReturnSize      s              9B 0
039100170215     d*// SizeList        s              9B 0
039200170215     d SizeList        s             10I 0
039300141028     d RrnLast         s              5I 0
039400141028     d WrkSort         s              1  0 inz(0)
039500141028      *************************************************************************
039600141028      * Data Structures
039700141028      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
039800141028      *     QLGSCB     - The sort request block for the QLGSORT API
039900141028      *     QLGSCB00   - The sort request block for the QLGSRTIO API
040000141028      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
040100141028      *     QUSEC      - Error structure for the QLGSORT API
040200141028      *************************************************************************
040300141028     d sflrcd          ds
040400141103     d  VS3clv
040500141117     d  VS3ord
040600141107     d  VS3ksu
040700150116     d  VS3tipocl
040800141103     d  VS3rag
040900141103     d  VS3istat
041000141103     d  VS3del
041100141103     d  VS3sped
041200141103     d  VS3ric
041300141103     d  VS3pkgm
041400141103     d  VS3obj
041500141111     d  VS3objprop
041600141103     d  VS3objfine
041700141103     d  VS3objttr
041800141103     d  VS3objcf
041900141103     d  VS3cmmd
042000141121     d  VS3esitotr
042100141126     d  VS3decotr
042200150220     d  VS3forza
042300141103     d  VS3nrv
042400141107     d  VS3ksc
042500141107     d  VS3cpo
042600141117     d  VS3ufe
042700150115     d  VS3cch
042800150115     d  VS3cchaut
042900141127     d  VS3oprop
043000141028     d  selected                      1A
043100141028
043200141028      /COPY QSYSINC/QRPGLESRC,QLGSORT
043300141028     d QLGKL                         16    DIM(MaxKey)
043400141028     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
043500141028     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
043600141028     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
043700141028     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
043800141028
043900141028      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
044000141028      /COPY QSYSINC/QRPGLESRC,QUSEC
044100141028
044200141028      //---------------------------------------------------------------
044300141028      //?Definizione procedure usate.
044400141028      //---------------------------------------------------------------
044500141028
044600141024      // - Ordinamento Subfile
044700141028     d Qlgsort_pr      pr                  extpgm('QLGSORT')
044800141028     d  pr_QLGSCB                          like(Qlgscb)
044900141028     d  pr_NotUsed1                        like(NotUsed)
045000141028     d  pr_NotUsed2                        like(NotUsed)
045100141028     d  pr_SizeList                        like(SizeList)
045200141028     d  pr_ReturnSize                      like(ReturnSize)
045300141028     d  pr_QUSEC                           like(Qusec)
045400141028     d                                     options(*varsize)
045500141028
045600141028     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
045700141028     d  pr_QLGSCB00                        like(QLGSCB00)
045800141028     d  pr_SflRcd                          like(SflRcd)
045900141028     d  pr_NotUsed1                        like(NotUsed)
046000141028     d  pr_SizeList                        like(SizeList)
046100141028     d  pr_NotUsed2                        like(NotUsed)
046200141028     d  pr_QUSEC                           like(Qusec)
046300141028     d                                     options(*varsize)
046400141028
046500141028     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
046600141028     d  pr_QLGSCB00                        like(QLGSCB00)
046700141028     d  pr_NotUsed1                        like(NotUsed)
046800141028     d  pr_SflRcd                          like(SflRcd)
046900170215     d  pr_SizeList                        like(SizeList)
047000141028     d  pr_NotUsed2                        like(NotUsed)
047100141028     d  pr_QUSEC                           like(Qusec)
047200141028     d                                     options(*varsize)
047300141105
047400141105       // - Interrogazione anagrafica campagne
047500150112     d TRKC02R         pr                  extpgm('TRKC02R')
047600141105     d  kpjba                              likeds(kpjba)
047700150112     d  trkc02ds                           likeds(TRKC02DS)
047800141107
047900141107       // - Storico Fasi
048000150112     d TRKC03R         pr                  extpgm('TRKC03R')
048100141107     d  kpjba                              likeds(kpjba)
048200150112     d  trkc03ds                           likeds(TRKC03DS)
048300141111
048400141111       // - Aggiunta Cliente in campagna
048500150112     d TRKC06R         pr                  extpgm('TRKC06R')
048600141111     d  kpjba                              likeds(kpjba)
048700150112     d  trkc06ds                           likeds(TRKC06DS)
048800141114
048900141114       // - Variazione Obiettivo
049000150112     d TRKC07R         pr                  extpgm('TRKC07R')
049100141114     d  kpjba                              likeds(kpjba)
049200150112     d  trkc07ds                           likeds(TRKC07DS)
049300150127
049400150127       // - Gestione Note
049500150127     d TRKC10R         pr                  extpgm('TRKC10R')
049600150127     d  kpjba                              likeds(kpjba)
049700150127     d  trkc10ds                           likeds(TRKC10DS)
049800141127
049900141127      // - Pgm Scrive fase se cliente in Campagna
050000150112     d TRKC71R         pr                  extpgm('TRKC71R')
050100141127     d  kpjba                              likeds(kpjba)
050200150112     d  trkc71ds                           likeds(TRKC71DS)
050300141114
050400141114      // - Verifica quale obiettivo variare
050500150112     d TRKC72R         pr                  extpgm('TRKC72R')
050600141114     d  kpjba                              likeds(kpjba)
050700150112     d  trkc72ds                           likeds(TRKC72DS)
050800150218
050900150218      // - Dati da Visualizzare o Parzializzazioni Trattative
051000150218     d TRKC76R         pr                  extpgm('TRKC76R')
051100150218     d  kpjba                              likeds(kpjba)
051200150218     d  trkc76ds                           likeds(TRKC76DS)
051300141103
051400141103       // - Delta cliente unificante
051500141103     d TISE61R         pr                  extpgm('TISE61R')
051600141103     d  kpjba                              likeds(kpjba)
051700141027
051800141103       // - Ricerca anagrafica clienti
051900141027     d TNTAI1R         pr                  extpgm('TNTAI1R')
052000141027     d  kpjba                              likeds(kpjba)
052100141027     d  tntai1ds                           likeds(tntai1ds)
052200141103
052300141103       // - Int. anagrafica clienti
052400141103     d TNTAI2R         pr                  extpgm('TNTAI2R')
052500141103     d  kpjba                              likeds(kpjba)
052600141103     d  tntai2ds                           likeds(tntai2ds)
052700141029
052800141029       // - Caricamento Filiali in gestione
052900141029     d TRUL31R         pr                  extpgm('TRUL31R')
053000141029     d  kpjba                              likeds(kpjba)
053100141029     d  trul31ds                           likeds(trul31ds)
053200141029     d  trul31ds2                          likeds(trul31ds2)
053300141028
053400141028      //---------------------------------------------------------------
053500141024      //?Definizione Prototipi.
053600141028      //---------------------------------------------------------------
053700141024      /copy gaitrasrc/srcprotopr,QsnQryModS
053800150115      /copy gaitrasrc/srcprotopr,TIBS02R
053900141024      /copy gaitrasrc/srcprotopr,TIBS34R
054000141027      /copy gaitrasrc/srcprotopr,TIBS69R
054100141027      /copy gaitrasrc/srcprotopr,TNSD24R
054200141024      /copy gaitrasrc/srcprotopr,TNTAA1C
054300141103      /copy gaitrasrc/srcprotopr,TNTA88R
054400141027      /copy gaitrasrc/srcprotopr,TRMK43R
054500141029      /copy gaitrasrc/srcprotopr,TRUL19R
054600141028
054700141028      //---------------------------------------------------------------
054800141028      //?Definizione key-list.
054900141028      //---------------------------------------------------------------
055000141028       // - File TABEL00F
055100141028     d k03tabel      e ds                  extname(TABEL00F:*key)
055200141028     d                                     prefix(k_)
055300141028
055400141028      //---------------------------------------------------------------
055500141028      //?M A I N - L I N E
055600141028      //---------------------------------------------------------------
055700141028
055800141028     c     *Entry        plist
055900141028     c                   parm                    kpjba
056000141028
056100141028      /free
056200141028
056300141028       //?Operazioni iniziali
056400141028       exsr RoutInz;
056500141028
056600141028       //?Gestione video
056700141024       DOW  Fine = *off;
056800141028         SELECT;
056900141024           WHEN  Video = 'D2';
057000141024             exsr GesD02;
057100141024           WHEN  Video = 'S3';
057200141024             exsr GesS03;
057300141030           WHEN  Video = 'D4';
057400141030             exsr GesD04;
057500141118           WHEN  Video = 'D5';
057600141118             exsr GesD05;
057700150116           WHEN  Video = 'S6';
057800150116             exsr GesS06;
057900150121           WHEN  Video = 'D7';
058000150121             exsr GesD07;
058100150128           WHEN  Video = 'S8';
058200150128             exsr GesS08;
058300150302           WHEN  Video = 'S9';
058400150302             exsr GesS09;
058500141028           OTHER;
058600141024             Fine = *on;
058700141028         ENDSL;
058800141028       ENDDO;
058900141028
059000141028       //?Operazioni finali
059100141028       exsr RoutEnd;
059200141028
059300141028       //--------------------------------------------------------------
059400141028       //?Operazioni iniziali.
059500141028       //--------------------------------------------------------------
059600141028       BEGSR RoutInz;
059700141028
059800141028         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
059900141028
060000141028       //?Impostazione campi "fissi"
060100141024         V01pgm = SDSpgm;
060200141029         k_TBLkut = 1;
060300141029         Video = 'D2';
060400141029         wInzD02 = *on;
060500150121         VisOrdRAG = *on;
060600150121         VisOrdCLV = *off;
060700141029
060800141029       //?Imposto oggi
060900141029         Oggi = %dec(%date());
061000141028
061100141028       //?Reperimento dati job
061200141028         exsr DatiJob;
061300141024
061400141024       //?Controllo abilitazione utente
061500141024         reset TNTAA1DS;
061600141024         ITAA1caut = '§utegtc';
061700141024         clear kpjbu;
061800141024         kpjbu = TNTAA1DS;
061900141024         tntaa1c (kpjba);
062000141024         TNTAA1DS = kpjbu;
062100141024
062200141024         IF  OTAA1fabi = 'N';
062300141024           ErrGrave = *on;
062400141024           leavesr;
062500141024         ENDIF;
062600141027         wabi = OTAA1cabi;
062700141024
062800141027       //?Carico le filiali abilitate all'utente
062900141024         clear TRUL31DS;
063000141027         clear TRUL31DS2;
063100141027         I31abi = wabi;
063200141024         I31cdi = DUTdis;
063300141024         I31car = DUTare;
063400141024         I31cpo = DUTpou;
063500141027         TRUL31R (kpjba:trul31ds:trul31ds2);
063600141024         IF O31pog <= *zeros;
063700141024           ErrGrave = *on;
063800141024           leavesr;
063900141024         ENDIF;
064000141027
064100141027       //?Carico sk Distretti - 17
064200141027         clear xx;
064300141027         k_TBLcod = '17';
064400141027         clear k_TBLkey;
064500141027         setll %kds(K03tabel) TABEL00F;
064600141027         reade %kds(K03tabel : 2) TABEL00F;
064700141027         DOW  not %eof(TABEL00F);
064800141027           IF  TBLflg = *blanks;
064900141027             ds17 = TBLuni;
065000141027             xx += 1;
065100141027             skDistretti(xx) = %subst(TBLkey:1:1);
065200141027             skDesDist(xx) = §17des;
065300141027           ENDIF;
065400141027           reade %kds(K03tabel : 2) TABEL00F;
065500141027         ENDDO;
065600141027
065700141027       //?Carico sk Aree - 05
065800141027         clear xx;
065900141027         k_TBLcod = '05';
066000141027         clear k_TBLkey;
066100141027         setll %kds(K03tabel) TABEL00F;
066200141027         reade %kds(K03tabel : 2) TABEL00F;
066300141027         DOW  not %eof(TABEL00F);
066400141027           IF  TBLflg = *blanks;
066500141027             ds05 = TBLuni;
066600141027             xx += 1;
066700141104             skAree(xx) = %subst(TBLkey:1:3);
066800141027             skDesArea(xx) = §05des;
066900141027           ENDIF;
067000141027           reade %kds(K03tabel : 2) TABEL00F;
067100141027         ENDDO;
067200141024
067300141024       //?Carico sk Importanza Clienti
067400141024         clear xx;
067500141029         k_TBLcod = 'IC';
067600141027         clear k_TBLkey;
067700141027         setll %kds(K03tabel) TABEL00F;
067800141027         reade %kds(K03tabel : 2) TABEL00F;
067900141024         DOW  not %eof(TABEL00F);
068000141024           IF  TBLflg = *blanks;
068100141024             xx += 1;
068200141024             skIC(xx) = %subst(TBLkey:1:1);
068300141117             dsIC = TBLuni;
068400141117             skIC_ord(xx) = §SICor;
068500141024           ENDIF;
068600141027           reade %kds(K03tabel : 2) TABEL00F;
068700141024         ENDDO;
068800141028
068900141028       ENDSR;
069000141028
069100141028       //--------------------------------------------------------------
069200141028       //?Reperimento Dati del job (Utente/Operativi).
069300141028       //--------------------------------------------------------------
069400141028       BEGSR DatiJob;
069500141028
069600141028         in(E) §AzUte;
069700141024         IF  NOT %error;
069800141028           in(E) §DatiUte;
069900141024         ENDIF;
070000141024         IF  %error or RSut = *blanks;
070100141028           clear TIBS34ds;
070200141028           tibs34r(tibs34ds);
070300141028           in §AzUte;
070400141028           in §DatiUte;
070500141024         ENDIF;
070600141028
070700141028       ENDSR;
070800141024
070900141024       //--------------------------------------------------------------
071000141024       //?Gestione videata D02.
071100141024       //--------------------------------------------------------------
071200141024       BEGSR GesD02;
071300141024
071400141024       //?Inizializzazione videata
071500141029         IF  wInzD02;
071600141029           exsr InzD02;
071700141029           wInzD02 = *off;
071800141024         ENDIF;
071900141024
072000141024       //?Se errore grave emetto messaggio poi esco
072100141024         IF  ErrGrave;
072200141024           ErrMessage  = *on;
072300141024           ErrGenerico = *on;
072400141024           V02msg = Msg(01);
072500141024         ENDIF;
072600141024
072700141030       //?Emissione Testata
072800150112         write  KC01T01;
072900141024
073000141024       //?Emissione videata
073100150112         exfmt  KC01D02;
073200141024         reset ErrMessage;
073300141024         reset ErrGenerico;
073400141024         clear V02msg;
073500141024
073600141024         SELECT;
073700141024
073800141024       //?- Errore Grave esco
073900141024           WHEN  ErrGrave;
074000141024             Fine = *on;
074100150116
074200150116       //?- F01=Formula
074300150116           WHEN  dsp_aid = c_F01;
074400150116             exsr F01D02;
074500150128
074600150128       //?- F02=Obiettivo/Andamento
074700150128           WHEN  dsp_aid = c_F02;
074800150128             exsr F02D02;
074900141024
075000141024       //?- F03=Fine
075100141024           WHEN  dsp_aid = c_F03;
075200141024             exsr F03D02;
075300150302
075400150302       //?- F04=Scelta Trattative
075500150302           WHEN  dsp_aid = c_F04;
075600150302             exsr F04D02;
075700141024
075800141024       //?Invio
075900141024           OTHER;
076000141024             exsr ContrD02;
076100141028             IF  ErrGenerico;
076200141028               leavesr;
076300141028             ENDIF;
076400141028         //?Videata sucessiva
076500141028             Video = 'S3';
076600141029             wInzS03 = *on;
076700150212             wInzD04 = *on;
076800141024
076900141024         ENDSL;
077000141024
077100141024       ENDSR;
077200141024
077300141024       //--------------------------------------------------------------
077400141029       //?Inizializzazione Videata D02.
077500141024       //--------------------------------------------------------------
077600141029       BEGSR InzD02;
077700141027
077800141027         VisDistretto = *off;
077900141027         VisArea      = *off;
078000141202
078100141202         clear V01des;
078200141024
078300141024       //?Pulizia videata
078400150112         clear KC01D02;
078500141024
078600141024       //?Imposto di dflt il "+" nelle % delta
078700141024         V02deldas = '+';
078800141024         V02delas = '+';
078900150127
079000170207       //*·//?Imposto di dflt 'E' escludi
079100170207       //*·  V02cli = 'E';
079200150213
079300150213       //?Imposto di dflt il "+" nelle % Obiettivo/Andamento
079400150213         V02obj3das = '+';
079500150213         V02obj3as = '+';
079600141024
079700141024       //?Se errore grave non imposto più niente
079800141024         IF  ErrGrave;
079900141024           leavesr;
080000141024         ENDIF;
080100141027
080200141027         SELECT;
080300141027       //?Abilitazione AZ-Azienda
080400141027       //?vedo tutte le selezioni
080500141027         WHEN  wabi = 'AZ';
080600141027           VisDistretto = *on;
080700141027           VisArea      = *on;
080800141024
080900141027       //?Abilitazione DI-Distretto
081000141027       //?NON vedo la selezione per Distretto
081100150611         WHEN  wabi = 'DI' or car(2) > *zeros;
081200150611           VisArea = *on;
081300141027
081400141027       //?Abilitazione TP-Terminal Partenza o PO=Filiale
081500141027       //?NON vedo la selezione per Distretto e per Area
081600141029         WHEN  wabi = 'TP' or wabi = 'PO';
081700141027         ENDSL;
081800141024
081900141024       ENDSR;
082000150116
082100150116       //--------------------------------------------------------------
082200150116       //?Gestione tasto funzionale F01 da videata D02
082300150116       //?F01=Formula
082400150116       //--------------------------------------------------------------
082500150116       BEGSR F01D02;
082600150116
082700150119         savformula = V02formula;
082800150116         Video = 'S6';
082900150116         wInzS06 = *on;
083000150116
083100150116       ENDSR;
083200150128
083300150128       //--------------------------------------------------------------
083400150128       //?Gestione tasto funzionale F02 da videata D02
083500150128       //?F02=Obiettivo/Andamento
083600150128       //--------------------------------------------------------------
083700150128       BEGSR F02D02;
083800150216
083900150216         IF  H2nmfl <> 'V02OBJ1' and H2nmfl <> 'V02OBJ2' and
084000150216             H2nmfl <> 'V02OBJ3';
084100150216           ErrMessage  = *on;
084200150216           ErrGenerico = *on;
084300150216           PosCurOBJ1  = *on;
084400150216           V02msg = Msg(22);
084500150216           leavesr;
084600150216         ENDIF;
084700150128
084800150128         Video = 'S8';
084900150128         wInzS08 = *on;
085000150128
085100150128       ENDSR;
085200141029
085300141029       //--------------------------------------------------------------
085400141029       //?Gestione tasto funzionale F03 da videata D02
085500141029       //?F03=Fine
085600141029       //--------------------------------------------------------------
085700141029       BEGSR F03D02;
085800141029
085900141029       //?Chiusura del programma
086000141029         Fine = *on;
086100141029
086200141029       ENDSR;
086300150302
086400150302       //--------------------------------------------------------------
086500150302       //?Gestione tasto funzionale F04 da videata D02
086600150302       //?F04=Scelta Trattative
086700150302       //--------------------------------------------------------------
086800150302       BEGSR F04D02;
086900150302
087000150302         Video = 'S9';
087100150302         wInzS09 = *on;
087200150302
087300150302       ENDSR;
087400141024
087500141024       //--------------------------------------------------------------
087600141024       //?Controlla Videata D02.
087700141024       //--------------------------------------------------------------
087800141024       BEGSR ContrD02;
087900141024
088000141024         WindDspF = IndDspF;
088100141024         reset WindDspFb;
088200141024         IndDspF  = WindDspF;
088300141119
088400141119         clear wdelda;
088500141119         clear wdela;
088600141024
088700141027       //?Campagna Commerciale Obbligatoria
088800141027         clear V02des;
088900141024         IF  V02ncm = *blanks;
089000141024           ErrMessage  = *on;
089100141024           ErrGenerico = *on;
089200141024           PosCurNCM   = *on;
089300141024           V02msg = Msg(02);
089400141024           leavesr;
089500141024         ENDIF;
089600141027       //?Ricerca Campagna Commerciale
089700141024         IF  %scan('?':V02ncm) > 0;
089800150112           clear TRKC02DS;
089900150112           IKC02ric = 'R';
090000150112           trkc02r (kpjba:trkc02ds);
090100141024           ErrGenerico = *on;
090200141024           PosCurNCM   = *on;
090300150112           V02ncm = %editc(OKC02ncm:'X');
090400141024         ENDIF;
090500141027       //?Accetto solo dati numerici
090600141029         xx = 1;
090700141029         FOR xx by 1 to %len(V02ncm);
090800141029           IF  %subst(V02ncm:xx:1) <> *blanks and
090900141029               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
091000141029             ErrMessage  = *on;
091100141029             ErrGenerico = *on;
091200141029             PosCurNCM   = *on;
091300141029             V02msg = Msg(02);
091400141029             leavesr;
091500141029           ENDIF;
091600141029         ENDFOR;
091700141027       //?Deve essere una Campagna Commerciale valida
091800141024         CMPncm = %dec(V02ncm:7:0);
091900141027         chain CMPncm TICMP01L;
092000141027         IF  not %found(TICMP01L);
092100141027           ErrMessage  = *on;
092200141027           ErrGenerico = *on;
092300141027           PosCurNCM   = *on;
092400141027           V02msg = Msg(02);
092500141027           leavesr;
092600141027         ENDIF;
092700141027       //?Decodifico Campagna Commerciale
092800141027         V02des = CMPdes;
092900150505       ////?Campagna Commerciale già finita
093000150505         //IF  CMPdfc < Oggi;
093100150505         //  ErrMessage  = *on;
093200150505         //  ErrGenerico = *on;
093300150505         //  PosCurNCM   = *on;
093400150505         //  V02msg = %trim(Msg(02)) + '. Già chiusa';
093500150505         //  leavesr;
093600150505         //ENDIF;
093700141027       //?In carico all'Utente
093800141029         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
093900141027           ErrMessage  = *on;
094000141027           ErrGenerico = *on;
094100141027           PosCurNCM   = *on;
094200141027           V02msg = %trim(Msg(02)) + '. Non in gestione';
094300141027           leavesr;
094400141027         ENDIF;
094500141027
094600141027       //?Distretto
094700141103         w001a = V02cdi;
094800141030         clear w050a;
094900141030         exsr Distretto;
095000141103         V02cdi = w001a;
095100141030         V02cdid = w050a;
095200141030         V02msg = wmsg;
095300141030         IF  ErrGenerico;
095400141030           leavesr;
095500141030         ENDIF;
095600141027
095700141027       //?Area
095800141103         w003a = V02car;
095900141030         clear w050a;
096000141030         exsr Area;
096100141103         V02car = w003a;
096200141030         V02card = w050a;
096300141030         V02msg = wmsg;
096400141030         IF  ErrGenerico;
096500141030           leavesr;
096600141030         ENDIF;
096700141030       //?Se presente anche il distretto devono essere congruenti
096800141030         IF  V02cdi <> *blanks and V02car <> *blanks;
096900141030           chain (V02cdi:w0030) AZORG02L;
097000141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
097100141030             ErrGenerico = *on;
097200141030             ErrMessage  = *on;
097300141030             PosCurCAR   = *on;
097400141030             V02msg = %trim(Msg(04)) + '. Non congruente con il distretto';
097500141030             leavesr;
097600141030           ENDIF;
097700141030         ENDIF;
097800141027
097900141027       //?Filiale Commerciale
098000141103         w003a = V02fil;
098100141030         clear w050a;
098200141030         exsr Filiale;
098300141103         V02fil = w003a;
098400141030         V02fild = w050a;
098500141030         V02msg = wmsg;
098600141030         IF  ErrGenerico;
098700141030           leavesr;
098800141030         ENDIF;
098900141027
099000141027       //?Commerciale Unificante
099100141103         w007a = V02cmm;
099200141030         clear w050a;
099300141030         exsr Commerciale;
099400141103         V02cmm = w007a;
099500141030         V02cmmd = w050a;
099600141030         V02msg = wmsg;
099700141030         IF  ErrGenerico;
099800141030           leavesr;
099900141030         ENDIF;
100000141027
100100141027       //?Cliente
100200141107         w007a = V02ksu;
100300141030         clear w050a;
100400141030         exsr Cliente;
100500141107         V02ksu = w007a;
100600141030         V02rag = w050a;
100700141030         V02msg = wmsg;
100800141030         IF  ErrGenerico;
100900141030           leavesr;
101000141030         ENDIF;
101100150611
101200150611       //?Se utente NON di sede a abilitato a più di un'area e no
101300150611       //?abilitazione 'AZ' o 'DI'
101400150611       //?richiedo obbligatoria l'area
101500150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
101600150611             V02car = *blanks and V02cmm = *blanks and
101700150611             V02ksu = *blanks and car(2) > *zeros;
101800150611           ErrMessage  = *on;
101900150611           ErrGenerico = *on;
102000150611           PosCurCAR   = *on;
102100150611           V02msg = Msg(25);
102200150611           leavesr;
102300150611         ENDIF;
102400141024
102500141024       //?Importanza Cliente
102600141030         w001a = V02clv1;
102700141030         clear w050a;
102800141030         exsr ImpCliente;
102900141030         V02clv1 = w001a;
103000141030         V02msg = wmsg;
103100141030         IF  ErrGenerico;
103200141030           PosCurCLV1 = *on;
103300141030           leavesr;
103400141030         ENDIF;
103500141030         w001a = V02clv2;
103600141030         clear w050a;
103700141030         exsr ImpCliente;
103800141030         V02clv2 = w001a;
103900141030         V02msg = wmsg;
104000141030         IF  ErrGenerico;
104100141030           PosCurCLV2 = *on;
104200141030           leavesr;
104300141030         ENDIF;
104400141030         w001a = V02clv3;
104500141030         clear w050a;
104600141030         exsr ImpCliente;
104700141030         V02clv3 = w001a;
104800141030         V02msg = wmsg;
104900141030         IF  ErrGenerico;
105000141030           PosCurCLV3 = *on;
105100141030           leavesr;
105200141030         ENDIF;
105300141030         w001a = V02clv4;
105400141030         clear w050a;
105500141030         exsr ImpCliente;
105600141030         V02clv4 = w001a;
105700141030         V02msg = wmsg;
105800141030         IF  ErrGenerico;
105900141030           PosCurCLV4 = *on;
106000141030           leavesr;
106100141030         ENDIF;
106200141030         w001a = V02clv5;
106300141030         clear w050a;
106400141030         exsr ImpCliente;
106500141030         V02clv5 = w001a;
106600141030         V02msg = wmsg;
106700141030         IF  ErrGenerico;
106800141030           PosCurCLV5 = *on;
106900141030           leavesr;
107000141030         ENDIF;
107100141028
107200141028       //?Delta
107300141029         IF  V02delda > *zeros or V02dela <> *zeros;
107400141028         //?Obbligatori tutti e 2 se immesso uno dei due
107500141028           IF  V02delda > *zeros and V02dela = *zeros;
107600141028             ErrMessage  = *on;
107700141028             ErrGenerico = *on;
107800141028             PosCurDELA  = *on;
107900150216             V02msg      = Msg(09);
108000141028             leavesr;
108100141028           ENDIF;
108200141028           IF  V02delda = *zeros and V02dela > *zeros;
108300141028             ErrMessage  = *on;
108400141028             ErrGenerico = *on;
108500141028             PosCurDELda = *on;
108600150216             V02msg      = Msg(09);
108700141028             leavesr;
108800141028           ENDIF;
108900141028         //?Controllo congruenza tra "DAL" e "AL"
109000141028           wdelda = V02delda;
109100141028           wdela  = V02dela;
109200141029           IF  V02deldas = '-';
109300141028             wdelda = wdelda * -1;
109400141028           ENDIF;
109500141029           IF  V02delas = '-';
109600141028             wdela = wdela * -1;
109700141028           ENDIF;
109800141028           IF  wdelda > wdela;
109900141028             ErrMessage  = *on;
110000141028             ErrGenerico = *on;
110100141028             PosCurDELda = *on;
110200150216             V02msg      = Msg(10);
110300141028             leavesr;
110400141028           ENDIF;
110500141028         ENDIF;
110600141121
110700141121       //?Obiettivi
110800150119         wfobj  = V02obj;
110900141127         wfobj1 = V02obj1;
111000141127         wfobj2 = V02obj2;
111100150119         wfformula = V02formula;
111200150213         wfobj3 = V02obj3;
111300150213         wobj3da = V02obj3da;
111400150213         wobj3a  = V02obj3a;
111500150213         IF  V02obj3das = '-';
111600150213           wobj3da = wobj3da * -1;
111700150213         ENDIF;
111800150213         IF  V02obj3as = '-';
111900150213           wobj3a = wobj3a * -1;
112000150213         ENDIF;
112100150128         exsr ContrObj;
112200141127         V02obj1 = wfobj1;
112300141127         V02obj2 = wfobj2;
112400150119         V02formula = wfformula;
112500150213         V02obj3 = wfobj3;
112600150213         V02obj3da = %abs(wobj3da);
112700150213         V02obj3a  = %abs(wobj3a);
112800141127         V02msg = wmsg;
112900141127         IF  ErrGenerico;
113000141127           leavesr;
113100141127         ENDIF;
113200150302
113300150302       //?Trattative
113400150302         w002a = V02ttr;
113500150302         clear w050a;
113600150302         exsr SceltaTtr;
113700150302         V02ttr = w002a;
113800150302         V02msg = wmsg;
113900150302         IF  ErrGenerico;
114000150302           leavesr;
114100150302         ENDIF;
114200141024
114300141024       ENDSR;
114400141028
114500141028       //--------------------------------------------------------------
114600141028       //?Gestione videata S03.
114700141028       //--------------------------------------------------------------
114800141028       BEGSR GesS03;
114900141028
115000141028       //?Inizializzazione videata
115100141029         IF  wInzS03;
115200141028           exsr InzS03;
115300141029           wInzS03 = *off;
115400141118           IF  wMaxNrr;
115500141118             leavesr;
115600141118           ENDIF;
115700141028         ENDIF;
115800141028
115900141028       //?Visualizzazione del SFL (se ci sono dati)
116000141117         SflDsp = (S03nrr <= *zeros);
116100141104
116200141104       //?Posizionamento cursore
116300141112         V03rcd = V03csr;
116400141111
116500141111       //?Se utente di sede abilito anche l'aggiunta di nuovi clienti
116600141111       //?con F10
116700150507       //?(SE Campagna NON scaduta)?
116800150505         AbilitaF10 = (DUTpou = 046  and  CMPdfc >= Oggi);
116900141127
117000150119       //?Se utente di sede e richiesto
117100150128       //?Obiettivo Inizio = Obiettivo Proposto
117200150507       //?e SE Campagna NON scaduta?
117300141127       //?abilito F13 per conferma obiettivo proposto
117400150212         AbilitaF13 = (DUTpou = 046 and V04obj = 'P' and
117500150507                       CMPdfc >= Oggi  and
117600150216                      ((V04formula = Formula(1) and
117700150216                       ((V04obj1 = 'I' and V04obj2 = 'P') or
117800150216                        (V04obj1 = 'P' and V04obj2 = 'I'))) or
117900150216                       (V04formula = Formula(2) and
118000150216                        V04obj1 = 'P' and V04obj2 = 'I')));
118100141028
118200141028       //?Emissione Testata e Piede con tasti funzionali abilitati
118300150112         write  KC01T01;
118400150112         write  KC01P03;
118500141028
118600141028       //?Emissione videata
118700150112         exfmt  KC01C03;
118800141028         reset ErrMessage;
118900141028         reset ErrGenerico;
119000141028         clear V03msg;
119100141028
119200141028         SELECT;
119300141030
119400141030       //?- F01=Guida Fasi
119500141030           WHEN  dsp_aid = c_F01;
119600141030             exsr F01S03;
119700141028
119800141028       //?- F03=Fine
119900141028           WHEN  dsp_aid = c_F03;
120000141029             exsr F03D02;
120100141030
120200141030       //?- F05=Altre parzializzazioni
120300141030           WHEN  dsp_aid = c_F05;
120400141030             exsr F05S03;
120500141030
120600141030       //?- F08=Ord.X Imp.Cliente
120700141030           WHEN  dsp_aid = c_F08;
120800141030             exsr F08S03;
120900141111
121000141111       //?- F10=Aggiunta Cliente
121100141111           WHEN  dsp_aid = c_F10;
121200141111             exsr F10S03;
121300141028
121400141028       //?- F12=Ritorno
121500141028           WHEN  dsp_aid = c_F12;
121600141028             exsr F12S03;
121700141127
121800141127       //?- F13=Conferma Obiettivo Proposto
121900141127           WHEN  dsp_aid = c_F13;
122000141127             exsr F13S03;
122100141028
122200141028       //?Invio
122300141028           OTHER;
122400141028             IF  V03csr > *zero;
122500141028               exsr OpzS03;
122600141028               IF  ErrGenerico;
122700141028                 leavesr;
122800141028               ENDIF;
122900141028             ENDIF;
123000141028
123100141028         ENDSL;
123200141028
123300141028       ENDSR;
123400141028
123500141028       //--------------------------------------------------------------
123600141028       //?Inizializzazione videata S03.
123700141028       //--------------------------------------------------------------
123800141029       BEGSR InzS03;
123900141028
124000141028         EndS03 = *off;
124100141112         clear V03tot;
124200150116         TroppiRcd = *off;
124300141202
124400141202       //?Imposto la descrizione della campagna
124500150121         V01des = CMPdes;
124600150212
124700150212       //?Periodo Confronto Fatturazione
124800150212         V03aammcf = %subst(CMPflo:5:2) + '/' + %subst(CMPflo:1:4);
124900141028
125000141028       //?Pulizia subfile
125100141028         exsr PulS03;
125200150212
125300150212       //?Imposto i dati per videata delle parzializzazioni
125400150212       //?e per caricamento subfile
125500150212         IF  wInzD04;
125600150212           exsr InzD04;
125700150212           wInzD04 = *off;
125800150212         ENDIF;
125900141028
126000141028       //?Caricamento subfile
126100141028         exsr Ries03;
126200141118
126300141118       //?Se ho superato il numero massimo di rcd da caricare esco
126400141118         IF  wMaxNrr;
126500141124           clear V03tot;
126600141118           Video = 'D5';
126700141118           wInzD05 = *on;
126800141118           leavesr;
126900141118         ENDIF;
127000141112
127100141117         rrnlast = S03nrr;
127200141112
127300141112       //?Faccio l'ordinamento
127400141112         SELECT;
127500141112       //?se ordina x RAG devo ordinare per CLV
127600141112         WHEN  VisOrdRAG;
127700141112           exsr Ordina_x_CLV;
127800141112       //?se ordina x CLV devo ordinare per RAG
127900141112         WHEN  VisOrdCLV;
128000141112           exsr Ordina_x_RAG;
128100141112         ENDSL;
128200141112
128300141112         SflEnd = *on;
128400141112
128500141112       //?Imposto il posizionamento al primo rcd
128600141112         IF  S03nrr > 0;
128700141112           V03rcd = 1;
128800141112         ELSE;
128900141112           clear V03rcd;
129000141112         ENDIF;
129100141112
129200141112         V03csr = V03rcd;
129300141117
129400150213       //?Imposto il n. rcd del subfile solo se il nrr savlato
129500150213       //?è <= all'ultimo nrr del subfile appena caricato
129600150213       //?se no imposto l'ultimo nrr
129700150213         IF  sav_S03nrr > 0;
129800150213           IF  sav_S03nrr <= rrnlast;
129900150213             V03csr = sav_S03nrr;
130000150213             clear sav_S03nrr;
130100150213           ELSE;
130200150213             V03csr = rrnlast;
130300150213           ENDIF;
130400141117         ENDIF;
130500141028
130600141028       ENDSR;
130700141028
130800141028       //--------------------------------------------------------------
130900141028       //?Pulizia Subfile S03.
131000141028       //--------------------------------------------------------------
131100141028       BEGSR PulS03;
131200141028
131300141028       //?Pulizia subfile
131400141028         SflDsp    = *on;
131500141028         SflDspCtl = *on;
131600150112         write KC01C03;
131700141028         SflDspCtl = *off;
131800141028         SflEnd    = *off;
131900141028
132000141112         clear V03rcd;
132100141028         clear V03csr;
132200141028         clear S03nrr;
132300141028         clear V03msg;
132400141028
132500141028         ErrMessage  = *off;
132600141028         ErrGenerico = *off;
132700141028
132800141028         WindDspF = IndDspF;
132900141028         reset WindDspFb;
133000141028         IndDspF  = WindDspF;
133100141028
133200141104       ENDSR;
133300141028
133400141028       //--------------------------------------------------------------
133500141028       //?Caricamento Subfile S03.
133600141028       //--------------------------------------------------------------
133700141028       BEGSR RieS03;
133800141028
133900141028         EndS03 = *off;
134000141103         wMaxNrr = *off;
134100141028
134200141028       //?Preparo Stringa Sql
134300141113         exsr PreparaSQL;
134400141028
134500141028       //?Dichiarazione cursore
134600141028         exec sql
134700141028           prepare S1   from :wSQL;
134800141028         exec sql
134900141124           declare WRK cursor for S1;
135000141124          // declare WRK  insensitive no scroll cursor for S1;
135100141028
135200141028         //?Apertura del cursore
135300141028         exec sql
135400141028           open WRK;
135500141028
135600141028         IF sqlcode < 0;
135700141028           EndS03 = *on;
135800141113           exec sql close WRK;
135900141113           leavesr;
136000141028         ENDIF;
136100141028
136200141028         DOW  not EndS03;
136300141028           exec sql
136400150115             fetch next from WRK into :TICMI00F, :CMCufe, :CMCcch;
136500141028           IF sqlcod = 100 or sqlcod < 0;
136600141028             EndS03 = *on;
136700141028             leave;
136800141028           ENDIF;
136900150303
137000150303         //?Controllo se ok il rcd per la selezione sulle trattative
137100150303           IF  V04ttr <> *blanks and V04ttr <> 'NO' and V04ttr <> 'SI';
137200150303             exsr CTRttr;
137300150303             IF  not RecOK;
137400150303               iter;
137500150303             ENDIF;
137600150303           ENDIF;
137700141124
137800141124         //?Controllo se ok il rcd
137900141124         //?per il "di cui" obiettivo
138000150212           IF  V04obj1 <> *blanks and V04obj2 <> *blanks;
138100141124             exsr CTRrec;
138200141124             IF  not RecOK;
138300141124               iter;
138400141124             ENDIF;
138500141124           ENDIF;
138600150213         //?per la parzializzazione dal al Obiettivo/Andamento
138700150213           IF  V04obj3 <> *blanks;
138800150213             exsr CTRrec1;
138900150213             IF  not RecOK;
139000150213               iter;
139100150213             ENDIF;
139200150213           ENDIF;
139300141028
139400141028         //?Carico i dati nel subfile
139500141028           exsr CarS03;
139600141028
139700141028         ENDDO;
139800141028
139900141028         exec sql
140000141028           close WRK;
140100141124
140200141124         V03tot = S03nrr;
140300141028
140400141028       ENDSR;
140500141028
140600141028       //--------------------------------------------------------------
140700141028       //?Preparo la Stringa SQL.
140800141028       //--------------------------------------------------------------
140900141028       BEGSR PreparaSQL;
141000141113
141100150303       //?Scelta trattative
141200141113         SELECT;
141300150303       //?Con trattativa  (fase TR o TF)
141400150303         WHEN  V04ttr <> 'NO' and V04ttr <> *blanks;
141500141113           wSQL = 'with SEL001 as ' +
141600141113                  '(select max (digits(CMFdfa) concat '+
141700141113                  'digits(CMFhfc)) as time, ' +
141800141113                  'CMFncm, CMFksu, CMFksc, CMFcpo ' +
141900141113                  'from TICMF00F ' +
142000150223                  'where CMFacm in('+ '''' + FaseObjTtr + '''' +
142100150223                  ', ' + '''' + FaseObjTf + '''' + ')' +
142200150212                  ' and CMFncm = ' + %editc(VH4ncm:'X') +
142300141113                  ' group by CMFncm, CMFksu, CMFksc, CMFcpo)' +
142400150115                  'select TICMI00F.*, CMCufe, CMCcch from SEL001 ' +
142500141113                  'join TICMI00F on ' +
142600141113                  'CMFncm = CMIncm and CMFksu = CMIksu and ' +
142700141113                  'CMFksc = CMIksc and CMFcpo = CMIcpo ' +
142800141113                  'join TICMC00F on ' +
142900141113                  'CMIncm = CMCncm and CMIksu = CMCksu and ' +
143000150303                  'CMIksc = CMCksc and CMIcpo = CMCcpo ' +
143100150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
143200141113           exsr AggSelSQL;
143300150303
143400150303       //?Nessuna trattativa
143500150303         WHEN  V04ttr = 'NO';
143600150303           wSQL = 'with SEL001 as ' +
143700150303                  '(select TICMI00F.* from TICMI00F '+
143800150303                  'exception join TICMF00F on ' +
143900150303                  'CMIncm = CMFncm and CMIksu = CMFksu and ' +
144000150303                  'CMIksc = CMFksc and CMIcpo = CMFcpo and ' +
144100150303                  'CMFacm in(' + '''' + FaseObjTtr + '''' +
144200150303                  ', ' + '''' + FaseObjTf + '''' + ') ' +
144300150303                  'where CMIncm = ' + %editc(VH4ncm:'X');
144400150303           exsr AggSelSQL;
144500150303           wSQL += ') ';
144600150303           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
144700150303                   'from SEL001, TICMC00F ' +
144800150303                   'where ' +
144900150303                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
145000150303                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
145100141113
145200141113         OTHER;
145300150303       //?"  " = Nessuna scelta
145400141113           wSQL = 'with SEL001 as ' +
145500141113                  '(select TICMI00F.* from TICMI00F ' +
145600150212                  ' where CMIncm = ' + %editc(VH4ncm:'X');
145700141113           exsr AggSelSQL;
145800141113           wSQL += ') ';
145900150115           wSQL += 'select SEL001.*, CMCufe, CMCcch ' +
146000141113                   'from SEL001, TICMC00F ' +
146100141113                   'where ' +
146200141113                   'CMIncm = CMCncm and CMIksu = CMCksu and ' +
146300141113                   'CMIksc = CMCksc and CMIcpo = CMCcpo';
146400141113         ENDSL;
146500150119
146600150213       //?Clienti Bloccati
146700150213         IF  V04cli = 'E';
146800150119           wSQL += ' and CMCcch = ' + '''   ''';
146900150119         ENDIF;
147000150213         IF  V04cli = 'I';
147100150213           wSQL += ' and CMCcch <> ' + '''   ''';
147200150213         ENDIF;
147300150213         IF  V04cli = *blanks;
147400150213         ENDIF;
147500150119
147600150119       //?Clienti Nuovi
147700150212         IF  V04ncli = 'S';
147800150119           wSQL += ' and CMIcln = ' + '''N''' +
147900150119                   ' and CMCcch = ' + '''   ''';
148000150119         ENDIF;
148100141113
148200141113         SELECT;
148300150128       //?Richiesta parzializzazione obiettivo Inizio
148400150212         WHEN  V04obj = 'I';
148500141113           wSQL += ' and CMCufe = ' + '''' + FaseObj + '''';
148600141113       //?Richiesta parzializzazione obiettivo Proposto
148700150212         WHEN  V04obj = 'P';
148800141113           wSQL += ' and CMCufe = ' + '''' + FaseObjProp + '''';
148900141113       //?Richiesta parzializzazione obiettivo Finale
149000150212         WHEN  V04obj = 'F';
149100141113           wSQL += ' and CMCufe = ' + '''' + FaseObjFine + '''';
149200141113         ENDSL;
149300141028
149400141028         wSQL += ' for fetch only';
149500141028
149600141028       ENDSR;
149700141113
149800141113       //--------------------------------------------------------------
149900141113       //?Aggiungo le selezioni alla stringa SQL.
150000141113       //--------------------------------------------------------------
150100141113       BEGSR AggSelSQL;
150200141113
150300141113         wclv = *off;
150400141119
150500141119       //?Se non sono richiesti Distretto/Area/Filiale DEVO caricare solo i clienti
150600141119       //?abilitati all'utente
150700150212         IF  V04cdi = *blanks and VH4car = *zeros and VH4fil = *zeros;
150800141119           xx = 1;
150900141119           Primo = *off;
151000141119           wSQL += ' and CMIfcm in(';
151100141119           FOR  xx by 1 to %elem(POG);
151200141119             IF  POG(xx) <> *blanks and POG(xx) <> *zeros;
151300141119               IF  Primo;
151400141119                 wsql += ', ';
151500141119               ELSE;
151600141119                 Primo = *on;
151700141119               ENDIF;
151800141119               wsql += POG(xx);
151900141119             ENDIF;
152000141119           ENDFOR;
152100141119           wsql += ')';
152200141119         ENDIF;
152300141113
152400141113       //?Distretto
152500150212         IF  V04cdi <> *blanks;
152600150212           wSQL += ' and CMIdcm = ' + '''' + V04cdi + '''';
152700141113         ENDIF;
152800141113       //?Area
152900150212         IF  VH4car > *zeros;
153000150212           wSQL += ' and CMIacm = ' + %editc(VH4car:'X');
153100141113         ENDIF;
153200141113       //?Filiale
153300150212         IF  VH4fil > *zeros;
153400150212           wSQL += ' and CMIfcm = ' + %editc(VH4fil:'X');
153500141113         ENDIF;
153600141113       //?Commerciale
153700150212         IF  VH4cmm > *zeros;
153800150212           wSQL += ' and CMIcmm = ' + %editc(VH4cmm:'X');
153900141113         ENDIF;
154000141113       //?Cliente
154100150212         IF  VH4ksu > *zeros;
154200150212           wSQL += ' and CMIksu = ' + %editc(VH4ksu:'X');
154300141113         ENDIF;
154400141113       //?Importanza Cliente
154500150212         IF  V04clv1 <> *blanks;
154600150212           wSQL += ' and CMIclv in(' + '''' + V04clv1 + '''';
154700141113           wclv = *on;
154800141113         ENDIF;
154900150212         IF  V04clv2 <> *blanks and wclv;
155000150212           wSQL += ',' + '''' + V04clv2 + '''';
155100141113           wclv = *on;
155200141113         ENDIF;
155300150212         IF  V04clv2 <> *blanks and not wclv;
155400150212           wSQL += ' and CMIclv in(' + '''' + V04clv2 + '''';
155500141113           wclv = *on;
155600141113         ENDIF;
155700150212         IF  V04clv3 <> *blanks and wclv;
155800150212           wSQL += ',' + '''' + V04clv3 + '''';
155900141113           wclv = *on;
156000141113         ENDIF;
156100150212         IF  V04clv3 <> *blanks and not wclv;
156200150212           wSQL += ' and CMIclv in(' + '''' + V04clv3 + '''';
156300141113           wclv = *on;
156400141113         ENDIF;
156500150212         IF  V04clv4 <> *blanks and wclv;
156600150212           wSQL += ',' + '''' + V04clv4 + '''';
156700141113           wclv = *on;
156800141113         ENDIF;
156900150212         IF  V04clv4 <> *blanks and not wclv;
157000150212           wSQL += ' and CMIclv in(' + '''' + V04clv4 + '''';
157100141113           wclv = *on;
157200141113         ENDIF;
157300150212         IF  V04clv5 <> *blanks and wclv;
157400150212           wSQL += ',' + '''' + V04clv5 + '''';
157500141113           wclv = *on;
157600141113         ENDIF;
157700150212         IF  V04clv5 <> *blanks and not wclv;
157800150212           wSQL += ' and CMIclv in(' + '''' + V04clv5 + '''';
157900141113           wclv = *on;
158000141113         ENDIF;
158100141113         IF  wclv;
158200141113           wSQL += ')';
158300141113         ENDIF;
158400141113       //?Delta
158500150212         IF  v04delda <> *zeros and V04dela <> *zeros;
158600141119           wSQL += ' and CMIpde between ' + %editc(wdelda:'O') +
158700141119                   ' and ' + %editc(wdela:'O');
158800141113         ENDIF;
158900141113
159000141113       ENDSR;
159100150303
159200150303       //--------------------------------------------------------------
159300150303       //?Controllo se OK il record per selezione trattative.
159400150303       //--------------------------------------------------------------
159500150303       BEGSR CTRttr;
159600150303
159700150303         RecOk = *off;
159800150303         clear wfase;
159900150303         wflgp = V04ttr;
160000150303         wtpric = 'P';
160100150303
160200150303         exsr CallTRKC76;
160300150303         IF  OKC76err <> *blanks;
160400150303           leavesr;
160500150303         ENDIF;
160600150303
160700150303         RecOK = *on;
160800150303
160900150303       ENDSR;
161000141124
161100141124       //--------------------------------------------------------------
161200141124       //?Controllo se OK il record.
161300141124       //--------------------------------------------------------------
161400141124       BEGSR CTRrec;
161500141124
161600141124         RecOk = *off;
161700150128         clear wvalobj1;
161800150128         clear wvalobj2;
161900141124
162000141124       //?Cerco il "di cui" 1
162100141124         SELECT;
162200150128       //?Inizio
162300150212         WHEN  V04obj1 = 'I';
162400150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
162500150218           IF  not %found(TICMF01L);
162600141126             leavesr;
162700141126           ENDIF;
162800150218           wvalobj1 = CMFpea;
162900141124       //?Proposto
163000150212         WHEN  V04obj1 = 'P';
163100150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
163200150218           IF  not %found(TICMF01L);
163300141126             leavesr;
163400141126           ENDIF;
163500150218           wvalobj1 = CMFpea;
163600141124       //?Finale
163700150212         WHEN  V04obj1 = 'F';
163800150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
163900150218           IF  not %found(TICMF01L);
164000141126             leavesr;
164100141126           ENDIF;
164200150218           wvalobj1 = CMFpea;
164300150119       //?Trattativa
164400150212         WHEN  V04obj1 = 'T';
164500150223           wfase = FaseObjTtr;
164600150303           wtpric = 'F';
164700150223           exsr CallTRKC76;
164800150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
164900150223             leavesr;
165000150223           ENDIF;
165100150223           wvalobj1 = OKC76pea;
165200150119       //?Confronto Fatturazione
165300150212         WHEN  V04obj1 = 'C';
165400150223           wfase = FaseObjCf;
165500150303           wtpric = 'F';
165600150223           exsr CallTRKC76;
165700150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
165800150223             leavesr;
165900150223           ENDIF;
166000150223           wvalobj1 = OKC76pea;
166100141124         ENDSL;
166200141124
166300141124       //?Cerco il "di cui" 2
166400141124         SELECT;
166500150128       //?Inizio
166600150212         WHEN  V04obj2 = 'I';
166700150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
166800150218           IF  not %found(TICMF01L);
166900141126             leavesr;
167000141126           ENDIF;
167100150218           wvalobj2 = CMFpea;
167200141124       //?Proposto
167300150212         WHEN  V04obj2 = 'P';
167400150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
167500150218           IF  not %found(TICMF01L);
167600141126             leavesr;
167700141126           ENDIF;
167800150218           wvalobj2 = CMFpea;
167900141124       //?Finale
168000150212         WHEN  V04obj2 = 'F';
168100150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
168200150218           IF  not %found(TICMF01L);
168300141126             leavesr;
168400141126           ENDIF;
168500150218           wvalobj2 = CMFpea;
168600150119       //?Trattativa
168700150212         WHEN  V04obj2 = 'T';
168800150223           wfase = FaseObjTtr;
168900150303           wtpric = 'F';
169000150223           exsr CallTRKC76;
169100150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
169200150223             leavesr;
169300150223           ENDIF;
169400150223           wvalobj2 = OKC76pea;
169500150119       //?Confronto Fatturazione
169600150212         WHEN  V04obj2 = 'C';
169700150223           wfase = FaseObjCf;
169800150303           wtpric = 'F';
169900150223           exsr CallTRKC76;
170000150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
170100150223             leavesr;
170200150223           ENDIF;
170300150223           wvalobj2 = OKC76pea;
170400141124         ENDSL;
170500141124
170600141127       //?controllo i 2 obiettivi
170700141127         SELECT;
170800150212         WHEN  V04formula = Formula(1) and wvalobj1 = wvalobj2;
170900141127           RecOK = *on;
171000150212         WHEN  V04formula = Formula(2) and wvalobj1 > wvalobj2;
171100141127           RecOK = *on;
171200150212         WHEN  V04formula = Formula(3) and wvalobj1 < wvalobj2;
171300141127           RecOK = *on;
171400150212         WHEN  V04formula = Formula(4) and wvalobj1 >= wvalobj2;
171500150119           RecOK = *on;
171600150212         WHEN  V04formula = Formula(5) and wvalobj1 <= wvalobj2;
171700141127           RecOK = *on;
171800150212         WHEN  V04formula = Formula(6) and wvalobj1 <> wvalobj2;
171900141127           RecOK = *on;
172000141127         ENDSL;
172100141124
172200141124       ENDSR;
172300150213
172400150213       //--------------------------------------------------------------
172500150213       //?Controllo se OK il record.
172600150213       //--------------------------------------------------------------
172700150213       BEGSR CTRrec1;
172800150213
172900150213         RecOk = *off;
173000150213         clear wvalobj3;
173100150213
173200150213       //?Cerco Obiettivo/Andamento richiesto
173300150213         SELECT;
173400150213       //?Inizio
173500150213         WHEN  V04obj3 = 'I';
173600150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
173700150218           IF  not %found(TICMF01L);
173800150213             leavesr;
173900150213           ENDIF;
174000150218           wvalobj3 = CMFpea;
174100150213       //?Proposto
174200150213         WHEN  V04obj3 = 'P';
174300150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
174400150218           IF  not %found(TICMF01L);
174500150213             leavesr;
174600150213           ENDIF;
174700150218           wvalobj3 = CMFpea;
174800150213       //?Finale
174900150213         WHEN  V04obj3 = 'F';
175000150218           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
175100150218           IF  not %found(TICMF01L);
175200150213             leavesr;
175300150213           ENDIF;
175400150218           wvalobj3 = CMFpea;
175500150213       //?Trattativa
175600150213         WHEN  V04obj3 = 'T';
175700150223           wfase = FaseObjTtr;
175800150303           wtpric = 'F';
175900150223           exsr CallTRKC76;
176000150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
176100150223             leavesr;
176200150213           ENDIF;
176300150223           wvalobj3 = OKC76pea;
176400150213       //?Confronto Fatturazione
176500150213         WHEN  V04obj3 = 'C';
176600150223           wfase = FaseObjCf;
176700150303           wtpric = 'F';
176800150223           exsr CallTRKC76;
176900150223           IF  OKC76err <> *blanks or OKC76peaa = *blanks;
177000150223             leavesr;
177100150213           ENDIF;
177200150223           wvalobj3 = OKC76pea;
177300150213         ENDSL;
177400150213
177500150213       //?confronto con il "dal" - "al" richiesto
177600150213         IF  wvalobj3 >= wobj3da and wvalobj3 <= wobj3a;
177700150213           RecOK = *on;
177800150213         ENDIF;
177900150213
178000150213       ENDSR;
178100141028
178200141028       //--------------------------------------------------------------
178300141028       //?Carico i dati nel Subfile S03.
178400141028       //--------------------------------------------------------------
178500141028       BEGSR CarS03;
178600141028
178700150112         clear  KC01S03;
178800141127         PosCurOPZ = *off;
178900141028
179000141029       //?Imposto i campi da TICMI
179100141103         VS3clv = CMIclv;
179200141107         VS3ksu = CMIksu;
179300141107         VS3ksc = CMIksc;
179400141107         VS3cpo = CMIcpo;
179500150115       //?Imposto i campi da TICMC
179600141114         VS3ufe = CMCufe;
179700150115         VS3cch = CMCcch;
179800150115         clear VS3cchaut;
179900150115       //?Imposto se causale automatica
180000150115         IF  VS3cch <> *blanks;
180100150115           clear TIBS02DS;
180200150115           clear dECM;
180300150115           T02mod = 'C';
180400150115           T02cod = 'ECM';
180500150115           T02ke1 = VS3cch;
180600150115           T02sif = KNSIF;
180700150115           TNTBE_RicercaControllo  (kpjba : tibs02ds);
180800150115           dECM = T02uni;
180900150115           IF  §ECMaut <> *blanks;
181000150115             VS3cchaut = 'S';
181100150115           ENDIF;
181200150115         ENDIF;
181300150116         clear VS3tipocl;
181400141028         IF  CMIcln = 'N';
181500150116           VS3tipocl = '*';
181600141028         ENDIF;
181700150116         IF  CMCcch <> *blanks;
181800150213           VS3tipocl = 'B';
181900150116         ENDIF;
182000141028         clear TIBS69DS;
182100141028         clear ACOrag;
182200141028         I69kac = CMIksu;
182300141028         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
182400141103         VS3rag = ACOrag;
182500141103         VS3istat = CMIist;
182600141103         VS3del = CMIpde;
182700141103         VS3sped = CMInsp;
182800141103         VS3ric = CMIric;
182900141103         VS3pkgm = CMIpme;
183000141029
183100141029       //?Imposto i campi da TICMF
183200150128       //?Obiettivo inizio
183300141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
183400141113         IF  %found(TICMF01L);
183500141125           VS3obj = %editc(CMFpea:'J');
183600141113         ENDIF;
183700141111       //?Obiettivo Proposto
183800141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
183900141113         IF  %found(TICMF01L);
184000141125           VS3objprop = %editc(CMFpea:'J');
184100141127           VS3oprop = CMFpea;
184200141113         ENDIF;
184300141029       //?Obiettivo Finale
184400141113         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
184500141113         IF  %found(TICMF01L);
184600141125           VS3objfine = %editc(CMFpea:'J');
184700141113         ENDIF;
184800150220       //?Aumento da Trattativa
184900150220         wfase = FaseObjTtr;
185000150303         wtpric = 'F';
185100150220         exsr CallTRKC76;
185200150220         IF  OKC76err = *blanks;
185300150220           VS3esitotr = OKC76flag;
185400150223           VS3objttr = OKC76peaa;
185500150224           IF  OKC76anno > 0;
185600150224             VS3decotr = (OKC76mese * 10000) + OKC76anno;
185700150224           ENDIF;
185800150220           VS3nrv = OKC76nrv;
185900150225           VS3forza = OKC76forza;
186000150220         ENDIF;
186100141029       //?% Confronto Fatturazione
186200150220         wfase = FaseObjCf;
186300150303         wtpric = 'F';
186400150220         exsr CallTRKC76;
186500150220         IF  OKC76err = *blanks;
186600150223           VS3objcf = OKC76peaa;
186700150220         ENDIF;
186800141029
186900141029       //?Imposto descrizione del commerciale
187000141029         clear CMMdes;
187100141029         chain CMIcmm AZCMM01L;
187200141029         IF  %found(AZCMM01L);
187300141103           VS3cmmd = CMMdes;
187400141029         ENDIF;
187500141117
187600141117       //?Imposto ordinamento importanza cliente
187700141117         clear xx;
187800141117         xx = %lookup(VS3clv:skIC);
187900141117         IF  xx > 0;
188000141117           VS3ord = skIC_ord(xx);
188100141117         ELSE;
188200141117           clear VS3ord;
188300141117         ENDIF;
188400141028
188500141029         S03nrr += 1;
188600141124
188700141124       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
188800141124         IF  S03nrr >= 9999;
188900141124           wMaxNrr = *on;
189000141124           EndS03 = *on;
189100141124           leavesr;
189200141124         ENDIF;
189300141124
189400150112         write  KC01S03;
189500141028
189600141028       ENDSR;
189700141030
189800141030       //--------------------------------------------------------------
189900141030       //?Gestione tasto funzionale F01 da videata S03
190000141111       //?F01=Campagna
190100141030       //--------------------------------------------------------------
190200141030       BEGSR F01S03;
190300141112
190400150112         clear TRKC02DS;
190500150112         IKC02ric = 'I';
190600150212         IKC02ncm = VH4ncm;
190700150112         trkc02r (kpjba:trkc02ds);
190800141030
190900141030       ENDSR;
191000141030
191100141030       //--------------------------------------------------------------
191200141030       //?Gestione tasto funzionale F05 da videata S03
191300141030       //?F05=Altre Parzializzazioni
191400141030       //--------------------------------------------------------------
191500141030       BEGSR F05S03;
191600141030
191700141030       //?Videata per nuove parzializzazioni
191800141030         Video = 'D4';
191900141030
192000141030       ENDSR;
192100141030
192200141030       //--------------------------------------------------------------
192300141030       //?Gestione tasto funzionale F08 da videata S03
192400141030       //?F08=Ord. X Imp.Cliente
192500141030       //--------------------------------------------------------------
192600141030       BEGSR F08S03;
192700141030
192800141111       //?F8 visualizzato richiede ordinamento x RAG
192900141111         IF  VisOrdRAG;
193000141111           exsr Ordina_x_RAG;
193100141030           leavesr;
193200141030         ENDIF;
193300141030
193400141030       //?F8 visualizzato richiede ordinamento x CLV
193500141030         IF  VisOrdCLV;
193600141030           exsr Ordina_x_CLV;
193700141030           leavesr;
193800141030         ENDIF;
193900141030
194000141030       ENDSR;
194100141111
194200141111       //--------------------------------------------------------------
194300141111       //?Gestione tasto funzionale F10 da videata S03
194400141111       //?F10=Aggiunta Cliente
194500141111       //--------------------------------------------------------------
194600141111       BEGSR F10S03;
194700141111
194800150112         clear TRKC06DS;
194900150212         IKC06ncm = VH4ncm;
195000150112         trkc06r (kpjba:TRKC06DS);
195100141111
195200141111       //?Ricarico il subfile
195300141111         wInzS03 = *on;
195400141111
195500141111       ENDSR;
195600141028
195700141028       //--------------------------------------------------------------
195800141028       //?Gestione tasto funzionale F12 da videata S03
195900141028       //?F12=Ritorno
196000141028       //--------------------------------------------------------------
196100141028       BEGSR F12S03;
196200141028
196300141028       //?Ritorno alle selezioni
196400141028         Video = 'D2';
196500141029         wInzD02 = *on;
196600150225         clear sav_s03nrr;
196700150223
196800150223       //?Se richiamato chiudo i file per il pgm TRKC76R
196900150223         IF  wTRKC76;
197000150223           clear TRKC76DS;
197100150223           IKC76ric = 'C';
197200150223           trkc76r (kpjba:TRKC76DS);
197300150223         ENDIF;
197400141028
197500141028       ENDSR;
197600141127
197700141127       //--------------------------------------------------------------
197800141127       //?Gestione tasto funzionale F13 da videata S03
197900141127       //?F13=Conferma Obiettivo proposto.
198000141127       //--------------------------------------------------------------
198100141127       BEGSR F13S03;
198200141127
198300150121         Video = 'D7';
198400150121         wInzD07 = *on;
198500141127
198600141127       ENDSR;
198700141030
198800141030       //--------------------------------------------------------------
198900141111       //?Ordinamento x RAG Subfile S03.
199000141030       //--------------------------------------------------------------
199100141111       BEGSR Ordina_x_RAG;
199200141030
199300141030         VisOrdCLV = *on;
199400141111         VisOrdRAG = *off;
199500141030
199600141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
199700141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
199800141030       // il campo è usato per selezionare i records dando un ordine a quelli
199900141030       // selezionati davanti ai non selezionati.
200000141030         clear QLGSCB;
200100141030         clear QLGSCB00;
200200141030
200300141118       // 2 campi chiave x Ragione Sociale - Codice (KSU)
200400141030         QLGNBRK = 1;
200500141030
200600141111       // imposto la posizione del RAG sul subfile e la lunghezza
200700141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
200800141117         QLGSP = 1 + %size(VS3clv) + %size(VS3ord)
200900150116                   + %size(VS3ksu) + %size(VS3tipocl);
201000141111         QLGSS = %SIZE(VS3rag);
201100141118         QLGDT = Carattere;
201200141030         QLGSO = Ascendente;
201300141030         QLGKL(1) = QLGSKL;
201400141118
201500141118       // imposto la posizione del KSU sul subfile e la lunghezza
201600141118       // l'ordinamento è su un campo numerico e deve essere ascendente
201700141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord);
201800141118         QLGSS = %SIZE(VS3ksu);
201900141118         QLGDT = Numerico;
202000141118         QLGSO = Ascendente;
202100141118         QLGKL(2) = QLGSKL;
202200141030
202300141030       // Load other sort parameters.
202400141030         QLGLB = 80 + 16 * MaxKey;
202500141030         QLGRL = %SIZE(sflrcd) - 1;
202600141030         QLGRT = 8;
202700141030         QLGOKL = 80;
202800141030         QLGLKE = 16;
202900141030         QLGLSS = 290;
203000141030
203100141030       // Initialize Sort I/O API fields.
203200141030         QLGRL00 = QLGRL;
203300141030         QLGRC00 = 1;
203400141030         clear QUSEI;
203500141030         QUSBPRV = %SIZE(QUSEC);
203600141030
203700141030       // First step - Initialize the sort routine.
203800141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
203900141030
204000141030       // Next step - Write records to I/O routine.
204100141030         QLGRT00 = Put;
204200141103         FOR  xx = 1 to rrnlast;
204300150112           chain xx KC01S03;
204400141030
204500141030       // solo le righe con Selected = 'Y' sono riordinate,
204600141030       // quindi per fare un ordinamento di tutte le righe
204700141030       // metto 'Y' sempre.
204800141103           selected  = 'Y';
204900141103           clear QUSEI;
205000141103           QUSBPRV = %SIZE(QUSEC);
205100141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
205200141103         ENDFOR;
205300141030
205400141030       // Next step - Signal end of input, clear subfile for reload.
205500141030         QLGRT00 = EndPut;
205600141030         clear QUSEI;
205700141030         QUSBPRV = %SIZE(QUSEC);
205800141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
205900141030       // pulizia SFL
206000141030         exsr PulS03;
206100141030
206200141030       // Final step - Write the records back to the subfile.
206300141030         QLGRT00 = Get;
206400141103         FOR  xx = 1 to rrnlast;
206500141103           clear QUSEI;
206600141103           QUSBPRV = %SIZE(QUSEC);
206700141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
206800141111           attrrag = visHI;
206900141103           clear attrclv;
207000150121           IF  VS3objcf <> *blanks;
207100141103             attrobjcf = visRI;
207200141103           ELSE;
207300141103             clear attrobjcf;
207400141103           ENDIF;
207500141121           IF  VS3esitotr <> *blanks;
207600141121             Visesito = *on;
207700141121           ELSE;
207800141121             Visesito = *off;
207900141121           ENDIF;
208000141103           s03nrr = xx;
208100150112           write KC01S03;
208200141103         ENDFOR;
208300141112
208400141117         rrnlast = S03nrr;
208500141112         IF  S03nrr > *zeros;
208600141112           V03rcd = 1;
208700141112           V03csr = 1;
208800141112         ELSE;
208900141112           clear V03rcd;
209000141112         ENDIF;
209100141030
209200141030       ENDSR;
209300141030
209400141030       //--------------------------------------------------------------
209500141030       //?Ordinamento x CLV Subfile S03.
209600141030       //--------------------------------------------------------------
209700141030       BEGSR Ordina_x_CLV;
209800141030
209900141030         VisOrdCLV = *off;
210000141111         VisOrdRAG = *on;
210100141030
210200141030       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
210300141030       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
210400141030       // il campo è usato per selezionare i records dando un ordine a quelli
210500141030       // selezionati davanti ai non selezionati.
210600141030         clear QLGSCB;
210700141030         clear QLGSCB00;
210800141030
210900150115       // 3 campi chiave x Importanza cliente, Fatturato e RAG
211000150115         QLGNBRK = 3;
211100141030
211200141118       // imposto la posizione del ORD CLV sul subfile e la lunghezza
211300141118       // l'ordinamento è su un campo numerico e deve essere discendente
211400141117         QLGSP = 1 + %size(VS3clv);
211500141117         QLGSS = %SIZE(VS3ord);
211600141117         QLGDT = Numerico;
211700141117         QLGSO = Discendente;
211800141030         QLGKL(1) = QLGSKL;
211900150115
212000150115       // imposto la posizione del Fatturato sul subfile e la lunghezza
212100150115       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
212200150115         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
212300150116                   + %size(VS3ksu) + %size(VS3tipocl)
212400150115                   + %size(VS3rag) + %size(VS3istat)
212500150115                   + %size(VS3del) + %size(VS3sped);
212600150115         QLGSS = %SIZE(VS3ric);
212700150115         QLGDT = Numerico;
212800150115         QLGSO = Discendente;
212900150115         QLGKL(2) = QLGSKL;
213000141118
213100141118       // imposto la posizione del RAG sul subfile e la lunghezza
213200141118       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
213300141118         QLGSP = 1 + %size(VS3clv) + %size(Vs3ord)
213400150116                   + %size(VS3ksu) + %size(VS3tipocl);
213500141118         QLGSS = %SIZE(VS3rag);
213600141118         QLGDT = Carattere;
213700141118         QLGSO = Ascendente;
213800150115         QLGKL(3) = QLGSKL;
213900141030
214000141030       // Load other sort parameters.
214100141030         QLGLB = 80 + 16 * MaxKey;
214200141030         QLGRL = %SIZE(sflrcd) - 1;
214300141030         QLGRT = 8;
214400141030         QLGOKL = 80;
214500141030         QLGLKE = 16;
214600141030         QLGLSS = 290;
214700141030
214800141030       // Initialize Sort I/O API fields.
214900141030         QLGRL00 = QLGRL;
215000141030         QLGRC00 = 1;
215100141030         clear QUSEI;
215200141030         QUSBPRV = %SIZE(QUSEC);
215300141030
215400141030       // First step - Initialize the sort routine.
215500141030         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
215600141030
215700141030       // Next step - Write records to I/O routine.
215800141030         QLGRT00 = Put;
215900141103         FOR  xx = 1 to rrnlast;
216000150112           chain xx KC01S03;
216100141030
216200141030       // solo le righe con Selected = 'Y' sono riordinate,
216300141030       // quindi per fare un ordinamento di tutte le righe
216400141030       // metto 'Y' sempre.
216500141103           selected  = 'Y';
216600141103           clear QUSEI;
216700141103           QUSBPRV = %SIZE(QUSEC);
216800141103           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
216900141103         ENDFOR;
217000141030
217100141030       // Next step - Signal end of input, clear subfile for reload.
217200141030         QLGRT00 = EndPut;
217300141030         clear QUSEI;
217400141030         QUSBPRV = %SIZE(QUSEC);
217500141030         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
217600141030       // pulizia SFL
217700141030         exsr PulS03;
217800141030
217900141030       // Final step - Write the records back to the subfile.
218000141030         QLGRT00 = Get;
218100141103         FOR  xx = 1 to rrnlast;
218200141103           clear QUSEI;
218300141103           QUSBPRV = %SIZE(QUSEC);
218400141103           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
218500141111           clear attrrag;
218600141103           attrclv = visHI;
218700150121           IF  VS3objcf <> *blanks;
218800141103             attrobjcf = visRI;
218900141103           ELSE;
219000141103             clear attrobjcf;
219100141103           ENDIF;
219200141121           IF  VS3esitotr <> *blanks;
219300141121             Visesito = *on;
219400141121           ELSE;
219500141121             Visesito = *off;
219600141121           ENDIF;
219700141103           s03nrr = xx;
219800150112           write KC01S03;
219900141103         ENDFOR;
220000141112
220100141117         rrnlast = S03nrr;
220200141112         IF  S03nrr > *zeros;
220300141112           V03rcd = 1;
220400141112           V03csr = 1;
220500141112         ELSE;
220600141112           clear V03rcd;
220700141112         ENDIF;
220800141030
220900141030       ENDSR;
221000141028
221100141028       //--------------------------------------------------------------
221200141028       //?Gestione opzioni Subfile S03.
221300141028       //--------------------------------------------------------------
221400141028       BEGSR OpzS03;
221500141028
221600150112         readc KC01S03;
221700141028
221800150112         DOW  NOT  %eof(TRKC01D);
221900141028
222000141028           SflNxtChg = *off;
222100141028           WindDspF  = IndDspF;
222200141028           reset WindDspFb;
222300141028           IndDspF   = WindDspF;
222400141028           V03rcd    = S03nrr;
222500141028
222600141127           PosCurOPZ = *off;
222700141119           sav_S03nrr = S03nrr;
222800141119
222900141119         //?- Verifico se il cliente è gestibile dall'utente
223000141119           IF  VS3opz <> *blank;
223100141119             clear TNTAA1DS;
223200141119             ITAA1caut = '§utegtc';
223300141119             ITAA1ksc = VS3ksu;
223400141119             kpjbu = TNTAA1DS;
223500141119             tntaa1c (kpjba);
223600150217             TNTAA1DS = kpjbu;
223700141119             IF  OTAA1fabi = 'N';
223800141119               ErrGenerico = *on;
223900141119               ErrMessage  = *on;
224000141127               PosCurOPZ   = *on;
224100150216               V03msg = %trim(Msg(07)) + '. Non in gestione';
224200141119             ENDIF;
224300141119           ENDIF;
224400141028
224500141119         //?- Se ok il cliente
224600141119           IF  not ErrMessage;
224700141119
224800141119             SELECT;
224900141119           //?- Nessuna opzione
225000141119             WHEN  VS3opz = *blank;
225100141028
225200141119           //?- 2 = Modifica
225300150115             WHEN  VS3opz = '2' and VS3cch = *blanks;
225400150127             //?anche in caso di periodo fuori dal range ma utente di sede
225500150115               IF  Oggi < CMPdigo or Oggi > CMPdfgo;
225600150216                 IF  DUTpou = 046;
225700150127                   exsr Opzione2;
225800150127                 ELSE;
225900150115                 ErrMessage  = *on;
226000150115                 ErrGenerico = *on;
226100150115                 PosCurOPZ   = *on;
226200150216                 V03msg = Msg(12);
226300150306                 %subst(V03msg:34:10) =
226400150306                 %subst(%editc(CMPdigo:'X'):7:2) + '/' +
226500150306                 %subst(%editc(CMPdigo:'X'):5:2) + '/' +
226600150306                 %subst(%editc(CMPdigo:'X'):1:4);
226700150306                 %subst(V03msg:48:10) =
226800150306                 %subst(%editc(CMPdfgo:'X'):7:2) + '/' +
226900150306                 %subst(%editc(CMPdfgo:'X'):5:2) + '/' +
227000150306                 %subst(%editc(CMPdfgo:'X'):1:4);
227100150127                 ENDIF;
227200150115               ELSE;
227300150115                 exsr Opzione2;
227400150115               ENDIF;
227500141028
227600150116           //?- 5 = Statistica
227700150116             WHEN  VS3opz = '5';
227800150116               exsr Opzione5;
227900141028
228000141119           //?- I = Interrogazione Clienti
228100141119             WHEN  VS3opz = 'I';
228200141119               exsr OpzioneI;
228300141028
228400141119           //?- T = Gestione Trattativa
228500141119             WHEN  VS3opz = 'T' and VS3nrv > 0;
228600141119               exsr OpzioneT;
228700141028
228800141119           //?- S = Storico Fasi
228900141119             WHEN  VS3opz = 'S';
229000141119               exsr OpzioneS;
229100150127
229200150127           //?- N = Note
229300150127             WHEN  VS3opz = 'N';
229400150127               exsr OpzioneN;
229500150220
229600150220           //?- F = Trattativa Forzata
229700150220             WHEN  VS3opz = 'F';
229800150220               exsr OpzioneF;
229900141028
230000141119             OTHER;
230100141119               ErrMessage  = *on;
230200141119               ErrGenerico = *on;
230300141127               PosCurOPZ   = *on;
230400150216               V03msg = Msg(11);
230500141119             ENDSL;
230600141119           ENDIF;
230700141028
230800141028           //?Aggiornamento sfl
230900141028           SELECT;
231000141028             WHEN  ErrMessage;
231100141028               SflNxtChg = *on;
231200141028               V03csr    = V03rcd;
231300141028             WHEN  ErrGenerico;
231400141028               V03csr = V03rcd;
231500141103               clear VS3opz;
231600141028             OTHER;
231700141028               V03csr = V03rcd;
231800141103               clear VS3opz;
231900141028           ENDSL;
232000150128
232100150128           IF  VS3esitotr <> *blanks;
232200150128             Visesito = *on;
232300150128           ELSE;
232400150128             Visesito = *off;
232500150128           ENDIF;
232600141028
232700150112           update KC01S03;
232800141028
232900141028           IF  ErrMessage or ErrGenerico;
233000141028             leave;
233100141028           ENDIF;
233200141028
233300150112           readc KC01S03;
233400141028
233500141028         ENDDO;
233600141118
233700141118       //?Ricarico il subfile
233800141119         IF  not ErrGenerico and not ErrMessage;
233900141119           wInzS03 = *on;
234000141119         ENDIF;
234100141028
234200141028       ENDSR;
234300141113
234400141113       //--------------------------------------------------------------
234500141113       //?Opzione "2" videata S03.
234600141113       //--------------------------------------------------------------
234700141113       BEGSR Opzione2;
234800141113
234900141114       //?Richiamo pgm x sapere quale obiettivo posso variare
235000150112         clear TRKC72DS;
235100150212         IKC72ncm = VH4ncm;
235200150112         IKC72ksu = VS3ksu;
235300150112         IKC72ksc = VS3ksc;
235400150112         IKC72cpo = VS3cpo;
235500150112         IKC72acm = VS3ufe;
235600150127       //?Se utente di sede e non siamo più nel periodo possibile
235700150127       //?per la modifica obiettivi e l'ultima fase è la 10 forzo come
235800150127       //?ultima fase la 20, in questo modo il pgm di ma la possibilità
235900150127       //?di inserire la 30
236000150127         IF  IKC72acm = FaseObj and DUTpou = 046 and
236100150127            (Oggi < CMPdigo or Oggi > CMPdfgo);
236200150127           IKC72acm = FaseObjProp;
236300150127         ENDIF;
236400150112         trkc72r (kpjba:TRKC72DS);
236500150112         IF  OKC72err <> *blanks;
236600141114           ErrMessage  = *on;
236700141114           ErrGenerico = *on;
236800141127           PosCurOPZ   = *on;
236900150112           V03msg = OKC72msg;
237000141114           leavesr;
237100141114         ENDIF;
237200150115
237300150115       //?Se tutto OK
237400150115       //?richiamo pgm per la variazione obiettivo
237500150115         clear TRKC07DS;
237600150115         IKC07ric = '2';
237700150212         IKC07ncm = VH4ncm;
237800150112         IKC07ksu = VS3ksu;
237900150112         IKC07ksc = VS3ksc;
238000150112         IKC07cpo = VS3cpo;
238100150112         IKC07acm = OKC72acm;
238200150112         trkc07r (kpjba:TRKC07DS);
238300141113
238400141113       ENDSR;
238500150115
238600150115       //--------------------------------------------------------------
238700150115       //?Opzione "R" videata S03.
238800150115       //--------------------------------------------------------------
238900150115       BEGSR OpzioneR;
239000150115
239100150115       //?richiamo il pgm per ripristinare il cliente in campagna
239200150115         clear TRKC07DS;
239300150115         IKC07ric = 'R';
239400150212         IKC07ncm = VH4ncm;
239500150115         IKC07ksu = VS3ksu;
239600150115         IKC07ksc = VS3ksc;
239700150115         IKC07cpo = VS3cpo;
239800150115         IKC07acm = VS3ufe;
239900150115         trkc07r (kpjba:TRKC07DS);
240000150115
240100150115       ENDSR;
240200141103
240300141103       //--------------------------------------------------------------
240400150116       //?Opzione "5" videata S03.
240500141103       //--------------------------------------------------------------
240600150116       BEGSR Opzione5;
240700150217
240800150217       //?- Verifico se l'opzione è utilizzabile dall'utente
240900150217          clear TNTAA1DS;
241000150217          ITAA1caut = '§uteist';
241100150217          ITAA1ksc = VS3ksu;
241200150217          kpjbu = TNTAA1DS;
241300150217          tntaa1c (kpjba);
241400150217          TNTAA1DS = kpjbu;
241500150217          IF  OTAA1fabi = 'N';
241600150217            ErrGenerico = *on;
241700150217            ErrMessage  = *on;
241800150217            PosCurOPZ   = *on;
241900150217            V03msg = %trim(Msg(07)) + '. Non in gestione';
242000150217            leavesr;
242100150217          ENDIF;
242200141103
242300141103         clear TISE60DS;
242400141103         D60op0 = 'VIS';
242500141103         D60op1 = 'F06';
242600141103         D60sce = '3';
242700141107         dsKSC = VS3ksu;
242800141103         skKSA(1) = dsKSA;
242900141103         kpjbu = TISE60DS;
243000141103         tise61r (kpjba);
243100141103
243200141103       ENDSR;
243300141103
243400141103       //--------------------------------------------------------------
243500141103       //?Opzione "I" videata S03.
243600141103       //--------------------------------------------------------------
243700141103       BEGSR OpzioneI;
243800141103
243900141103         clear TNTAI2DS;
244000150122       //?se non è utente di sede richiamo il pgm in modo da poter gestire
244100150122       //?le eventuali attività ancora da eseguire + crearne delle nuove
244200150122         IF  DUTpou <> 046;
244300150122           ITAI2ric = 'A';
244400150122         ENDIF;
244500141107         ITAI2ksc = VS3ksu;
244600141103         tntai2r (kpjba:TNTAI2DS);
244700141103
244800141103       ENDSR;
244900141103
245000141103       //--------------------------------------------------------------
245100141103       //?Opzione "T" videata S03.
245200141103       //--------------------------------------------------------------
245300141103       BEGSR OpzioneT;
245400141103
245500141103         clear TNTA88DS;
245600141103         ITA88fle = '5';
245700141103         ITA88nrv = VS3nrv;
245800141103         tnta88r (kpjba:TNTA88DS);
245900141103
246000141103       ENDSR;
246100141107
246200141107       //--------------------------------------------------------------
246300141107       //?Opzione "S" videata S03.
246400141107       //--------------------------------------------------------------
246500141107       BEGSR OpzioneS;
246600141107
246700150112         clear TRKC03DS;
246800150212         IKC03ncm = VH4ncm;
246900150112         IKC03ksu = VS3ksu;
247000150112         IKC03ksc = VS3ksc;
247100150112         IKC03cpo = VS3cpo;
247200150112         trkc03r (kpjba:TRKC03DS);
247300141107
247400141107       ENDSR;
247500141113
247600141113       //--------------------------------------------------------------
247700141113       //?Opzione "E" videata S03.
247800141113       //--------------------------------------------------------------
247900141113       BEGSR OpzioneE;
248000141114
248100141114       //?richiamo pgm per escludere il cliente dalla campagna
248200150112         clear TRKC07DS;
248300150112         IKC07ric = '2';
248400150212         IKC07ncm = VH4ncm;
248500150112         IKC07ksu = VS3ksu;
248600150112         IKC07ksc = VS3ksc;
248700150112         IKC07cpo = VS3cpo;
248800150112         IKC07acm = FaseChiudi;
248900150112         trkc07r (kpjba:TRKC07DS);
249000141113
249100141113       ENDSR;
249200150127
249300150127       //--------------------------------------------------------------
249400150127       //?Opzione "N" videata S03.
249500150127       //--------------------------------------------------------------
249600150127       BEGSR OpzioneN;
249700150127
249800150127       //?richiamo pgm per gestione Note
249900150127         clear TRKC10DS;
250000150127         IKC10tla = 'L';
250100150212         IKC10ncm = VH4ncm;
250200150127         IKC10ksu = VS3ksu;
250300150127         IKC10ksc = VS3ksc;
250400150127         IKC10cpo = VS3cpo;
250500150127         IKC10acm = VS3ufe;
250600150127         trkc10r (kpjba:TRKC10DS);
250700150127
250800150127       ENDSR;
250900150220
251000150220       //--------------------------------------------------------------
251100150220       //?Opzione "F" videata S03.
251200150220       //--------------------------------------------------------------
251300150220       BEGSR OpzioneF;
251400150225
251500150225       //?Posso fare la fase TF solo nel periodo di inserimento trattative
251600150225         IF  Oggi < CMPdit or Oggi > CMPdft;
251700150225           ErrMessage  = *on;
251800150225           ErrGenerico = *on;
251900150225           PosCurOPZ   = *on;
252000150225           V03msg = Msg(23);
252100150306           %subst(V03msg:39:10) =
252200150306           %subst(%editc(CMPdit:'X'):7:2) + '/' +
252300150306           %subst(%editc(CMPdit:'X'):5:2) + '/' +
252400150306           %subst(%editc(CMPdit:'X'):1:4);
252500150306           %subst(V03msg:53:10) =
252600150306           %subst(%editc(CMPdft:'X'):7:2) + '/' +
252700150306           %subst(%editc(CMPdft:'X'):5:2) + '/' +
252800150306           %subst(%editc(CMPdft:'X'):1:4);
252900150225           leavesr;
253000150225         ENDIF;
253100150220
253200150220       //?Posso immettere la Trattativa Forzata se non presente NESSUNA fase TR
253300150220         chain (VH4ncm:VS3ksu:VS3ksc:VS3cpo:FaseObjTtr) TICMF01L;
253400150220         IF  %found(TICMF01L);
253500150220           ErrMessage  = *on;
253600150220           ErrGenerico = *on;
253700150220           PosCurOPZ   = *on;
253800150220           V03msg = %trim(Msg(11)) + '. Già presente una Trattativa';
253900150220           leavesr;
254000150220         ENDIF;
254100150220
254200150220       //?richiamo pgm per immissione Trattativa Forzata
254300150220         clear TRKC07DS;
254400150220         IKC07ric = '2';
254500150220         IKC07ncm = VH4ncm;
254600150220         IKC07ksu = VS3ksu;
254700150220         IKC07ksc = VS3ksc;
254800150220         IKC07cpo = VS3cpo;
254900150220         IKC07acm = FaseObjTf;
255000150220         trkc07r (kpjba:TRKC07DS);
255100150220
255200150220       ENDSR;
255300141030
255400141030       //--------------------------------------------------------------
255500141030       //?Gestione videata D04.
255600141030       //--------------------------------------------------------------
255700141030       BEGSR GesD04;
255800141030
255900141030       //?Inizializzazione videata
256000141030         IF  wInzD04;
256100141030           exsr InzD04;
256200141030           wInzD04 = *off;
256300141030         ENDIF;
256400141030
256500141030       //?Emissione videata
256600150112         exfmt  KC01W04;
256700141030         reset ErrMessage;
256800141030         reset ErrGenerico;
256900141030         clear V04msg;
257000141030
257100141030         SELECT;
257200150119
257300150119       //?- F01=Formula
257400150119           WHEN  dsp_aid = c_F01;
257500150119             exsr F01D04;
257600150128
257700150128       //?- F02=Obiettivo/Andamento
257800150128           WHEN  dsp_aid = c_F02;
257900150128             exsr F02D04;
258000150302
258100150302       //?- F04=Scelta Trattative
258200150302           WHEN  dsp_aid = c_F04;
258300150302             exsr F04D04;
258400141103
258500141103       //?- F06=Conferma
258600141103           WHEN  dsp_aid = c_F06;
258700141104             exsr ContrD04;
258800141104             IF  ErrGenerico;
258900141104               leavesr;
259000141104             ENDIF;
259100141103             exsr F06D04;
259200141030
259300141030       //?- F12=Ritorno
259400141030           WHEN  dsp_aid = c_F12;
259500141030             exsr F12D04;
259600141030
259700141030       //?Invio
259800141030           OTHER;
259900141030             exsr ContrD04;
260000141030             IF  ErrGenerico;
260100141030               leavesr;
260200141030             ENDIF;
260300141030
260400141030         ENDSL;
260500141030
260600141030       ENDSR;
260700141030
260800141030       //--------------------------------------------------------------
260900141030       //?Inizializzazione Videata D04.
261000141030       //--------------------------------------------------------------
261100141030       BEGSR InzD04;
261200141030
261300141030       //?Pulizia videata
261400150112         clear KC01W04;
261500150212
261600150212         V04ncm = V02ncm;
261700150212         V04des = V02des;
261800150212         VH4ncm = %dec(V02ncm:7:0);
261900141030
262000150212         V04cdi = V02cdi;
262100150212         V04cdid = V02cdid;
262200150216         IF  V02car <> *blanks;
262300150212           V04car = V02car;
262400150212           V04card = V02card;
262500150212           VH4car = %dec(V02car:3:0);
262600141030         ENDIF;
262700150216         IF  V02fil <> *blanks;
262800150212           V04fil = V02fil;
262900150212           V04fild = V02fild;
263000150212           VH4fil = %dec(V02fil:3:0);
263100141030         ENDIF;
263200150216         IF  V02cmm <> *blanks;
263300150212           V04cmm = V02cmm;
263400150212           V04cmmd = V02cmmd;
263500150212           VH4cmm = %dec(V02cmm:7:0);
263600141030         ENDIF;
263700150216         IF  V02ksu <> *blanks;
263800150212           V04ksu = V02ksu;
263900150212           V04rag = V02rag;
264000150212           VH4ksu = %dec(V02ksu:7:0);
264100141030         ENDIF;
264200150212         V04clv1 = V02clv1;
264300150212         V04clv2 = V02clv2;
264400150212         V04clv3 = V02clv3;
264500150212         V04clv4 = V02clv4;
264600150212         V04clv5 = V02clv5;
264700150212         V04deldas = V02deldas;
264800150212         V04delda = V02delda;
264900150212         V04delas = V02delas;
265000150212         V04dela = V02dela;
265100150212         V04cli = V02cli;
265200150212         V04ncli = V02ncli;
265300150212         V04ttr = V02ttr;
265400150212         V04obj = V02obj;
265500150212         V04obj1 = V02obj1;
265600150212         V04obj2 = V02obj2;
265700150212         V04formula = V02formula;
265800150213         V04obj3 = V02obj3;
265900150213         V04obj3das = V02obj3das;
266000150213         V04obj3da = V02obj3da;
266100150213         V04obj3as = V02obj3as;
266200150213         V04obj3a = V02obj3a;
266300150223
266400150223       //?Se richiamato chiudo i file per il pgm TRKC76R
266500150223         IF  wTRKC76;
266600150223           clear TRKC76DS;
266700150223           IKC76ric = 'C';
266800150223           trkc76r (kpjba:TRKC76DS);
266900150223         ENDIF;
267000141030
267100141030       ENDSR;
267200141030
267300141030       //--------------------------------------------------------------
267400141030       //?Controllo Videata D04.
267500141030       //--------------------------------------------------------------
267600141030       BEGSR ContrD04;
267700141030
267800141030         WindDspF = IndDspF;
267900141030         reset WindDspFb;
268000141030         IndDspF  = WindDspF;
268100141119
268200141119         clear wdelda;
268300141119         clear wdela;
268400150212
268500150212       //?Campagna Commerciale Obbligatoria
268600150212         clear V04des;
268700150305         clear VH4ncm;
268800150212         IF  V04ncm = *blanks;
268900150212           ErrMessage  = *on;
269000150212           ErrGenerico = *on;
269100150212           PosCurNCM   = *on;
269200150212           V04msg = Msg(02);
269300150212           leavesr;
269400150212         ENDIF;
269500150212       //?Ricerca Campagna Commerciale
269600150212         IF  %scan('?':V04ncm) > 0;
269700150212           clear TRKC02DS;
269800150212           IKC02ric = 'R';
269900150212           trkc02r (kpjba:trkc02ds);
270000150212           ErrGenerico = *on;
270100150212           PosCurNCM   = *on;
270200150212           V04ncm = %editc(OKC02ncm:'X');
270300150212         ENDIF;
270400150212       //?Accetto solo dati numerici
270500150212         xx = 1;
270600150212         FOR xx by 1 to %len(V04ncm);
270700150212           IF  %subst(V04ncm:xx:1) <> *blanks and
270800150212               %check(Digits:%subst(V04ncm:xx:1)) > *zeros;
270900150212             ErrMessage  = *on;
271000150212             ErrGenerico = *on;
271100150212             PosCurNCM   = *on;
271200150212             V04msg = Msg(02);
271300150212             leavesr;
271400150212           ENDIF;
271500150212         ENDFOR;
271600150212       //?Deve essere una Campagna Commerciale valida
271700150212         CMPncm = %dec(V04ncm:7:0);
271800150212         chain CMPncm TICMP01L;
271900150212         IF  not %found(TICMP01L);
272000150212           ErrMessage  = *on;
272100150212           ErrGenerico = *on;
272200150212           PosCurNCM   = *on;
272300150212           V04msg = Msg(02);
272400150212           leavesr;
272500150212         ENDIF;
272600150212       //?Decodifico Campagna Commerciale
272700150212         V04des = CMPdes;
272800150505       ////?Campagna Commerciale già finita
272900150505         //IF  CMPdfc < Oggi;
273000150505         //  ErrMessage  = *on;
273100150505         //  ErrGenerico = *on;
273200150505         //  PosCurNCM   = *on;
273300150505         //  V04msg = %trim(Msg(02)) + '. Già chiusa';
273400150505         //  leavesr;
273500150505         //ENDIF;
273600150212       //?In carico all'Utente
273700150212         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
273800150212           ErrMessage  = *on;
273900150212           ErrGenerico = *on;
274000150212           PosCurNCM   = *on;
274100150212           V04msg = %trim(Msg(02)) + '. Non in gestione';
274200150212           leavesr;
274300150212         ENDIF;
274400150212         VH4ncm = %dec(V04ncm:7:0);
274500141030
274600141030       //?Distretto
274700141030         w001a = V04cdi;
274800141030         clear w050a;
274900141030         exsr Distretto;
275000141030         V04cdi = w001a;
275100141030         v04cdid = w050a;
275200141030         V04msg = wmsg;
275300141030         IF  Errgenerico;
275400141030           leavesr;
275500141030         ENDIF;
275600141030
275700141030       //?Area
275800150305         clear VH4car;
275900141030         w003a = V04car;
276000141030         clear w050a;
276100141030         exsr Area;
276200141030         V04car = w003a;
276300141030         V04card = w050a;
276400141030         V04msg = wmsg;
276500141030         IF  ErrGenerico;
276600141030           leavesr;
276700141030         ENDIF;
276800141030       //?Se presente anche il distretto devono essere congruenti
276900141030         IF  V04cdi <> *blanks and V04car <> *blanks;
277000150219           chain (V04cdi:w0030) AZORG02L;
277100141030           IF  not %found(AZORG02L) or ORGfva <> *blanks;
277200141030             ErrGenerico = *on;
277300141030             ErrMessage  = *on;
277400141030             PosCurCAR   = *on;
277500141030             V04msg = %trim(Msg(04)) + '. Non congruente con il distretto';
277600141030             leavesr;
277700141030           ENDIF;
277800141030         ENDIF;
277900150212         IF  V04car <> *blanks;
278000150212           VH4car = %dec(V04car:3:0);
278100150212         ENDIF;
278200141030
278300141030       //?Filiale Commerciale
278400150305         clear VH4fil;
278500141030         w003a = V04fil;
278600141030         clear w050a;
278700141030         exsr Filiale;
278800141030         V04fil = w003a;
278900141030         V04fild = w050a;
279000141030         V04msg = wmsg;
279100141030         IF  ErrGenerico;
279200141030           leavesr;
279300141030         ENDIF;
279400150212         IF  V04fil <> *blanks;
279500150212           VH4fil = %dec(V04fil:3:0);
279600150212         ENDIF;
279700141030
279800141030       //?Commerciale Unificante
279900150305         clear VH4cmm;
280000141030         w007a = V04cmm;
280100141030         clear w050a;
280200141030         exsr Commerciale;
280300141030         V04cmm = w007a;
280400141030         V04cmmd = w050a;
280500141103         V04msg = wmsg;
280600141030         IF  ErrGenerico;
280700141030           leavesr;
280800141030         ENDIF;
280900150212         IF  V04cmm <> *blanks;
281000150212           VH4cmm = %dec(V04cmm:7:0);
281100150212         ENDIF;
281200141030
281300141030       //?Cliente
281400150305         clear VH4ksu;
281500141107         w007a = V04ksu;
281600141030         clear w050a;
281700141030         exsr Cliente;
281800141107         V04ksu = w007a;
281900141030         V04rag = w050a;
282000141030         V04msg = wmsg;
282100141030         IF  ErrGenerico;
282200141030           leavesr;
282300141030         ENDIF;
282400150212         IF  V04ksu <> *blanks;
282500150212           VH4ksu = %dec(V04ksu:7:0);
282600150212         ENDIF;
282700150611
282800150611       //?Se utente NON di sede a abilitato a più di un'area e no
282900150611       //?abilitazione 'AZ' o 'DI'
283000150611       //?richiedo obbligatoria l'area
283100150611         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
283200150611             V04car = *blanks and V04cmm = *blanks and
283300150611             V04ksu = *blanks and car(2) > *zeros;
283400150611           ErrMessage  = *on;
283500150611           ErrGenerico = *on;
283600150611           PosCurCAR   = *on;
283700150611           V04msg = Msg(25);
283800150611           leavesr;
283900150611         ENDIF;
284000141030
284100141030       //?Importanza Cliente
284200141030         w001a = V04clv1;
284300141030         clear w050a;
284400141030         exsr ImpCliente;
284500150219         V04clv1 = w001a;
284600141103         V04msg = wmsg;
284700141030         IF  ErrGenerico;
284800141030           PosCurCLV1 = *on;
284900141030           leavesr;
285000141030         ENDIF;
285100141030         w001a = V04clv2;
285200141030         clear w050a;
285300141030         exsr ImpCliente;
285400141030         V04clv2 = w001a;
285500141030         V04msg = wmsg;
285600141030         IF  ErrGenerico;
285700141030           PosCurCLV2 = *on;
285800141030           leavesr;
285900141030         ENDIF;
286000141030         w001a = V04clv3;
286100141030         clear w050a;
286200141030         exsr ImpCliente;
286300141030         V04clv3 = w001a;
286400141030         V04msg = wmsg;
286500141030         IF  ErrGenerico;
286600141030           PosCurCLV3 = *on;
286700141030           leavesr;
286800141030         ENDIF;
286900141030         w001a = V04clv4;
287000141030         clear w050a;
287100141030         exsr ImpCliente;
287200141030         V04clv4 = w001a;
287300141030         V04msg = wmsg;
287400141030         IF  ErrGenerico;
287500141030           PosCurCLV4 = *on;
287600141030           leavesr;
287700141030         ENDIF;
287800141030         w001a = V04clv5;
287900141030         clear w050a;
288000141030         exsr ImpCliente;
288100141030         V04clv5 = w001a;
288200141030         V04msg = wmsg;
288300141030         IF  ErrGenerico;
288400141030           PosCurCLV5 = *on;
288500141030           leavesr;
288600141030         ENDIF;
288700141030
288800141030       //?Delta
288900141030         IF  V04delda > *zeros or V04dela <> *zeros;
289000141030         //?Obbligatori tutti e 2 se immesso uno dei due
289100141030           IF  V04delda > *zeros and V04dela = *zeros;
289200141030             ErrMessage  = *on;
289300141030             ErrGenerico = *on;
289400141030             PosCurDELA  = *on;
289500150216             V04msg      = Msg(09);
289600141030             leavesr;
289700141030           ENDIF;
289800141030           IF  V04delda = *zeros and V04dela > *zeros;
289900141030             ErrMessage  = *on;
290000141030             ErrGenerico = *on;
290100141030             PosCurDELda = *on;
290200150216             V04msg      = Msg(09);
290300141030             leavesr;
290400141030           ENDIF;
290500141030         //?Controllo congruenza tra "DAL" e "AL"
290600141104           wdelda = V04delda;
290700141104           wdela  = V04dela;
290800141030           IF  V04deldas = '-';
290900141030             wdelda = wdelda * -1;
291000141030           ENDIF;
291100141030           IF  V04delas = '-';
291200141030             wdela = wdela * -1;
291300141030           ENDIF;
291400141030           IF  wdelda > wdela;
291500141030             ErrMessage  = *on;
291600141030             ErrGenerico = *on;
291700141030             PosCurDELda = *on;
291800150216             V04msg      = Msg(10);
291900141030             leavesr;
292000141030           ENDIF;
292100141030         ENDIF;
292200141121
292300141121       //?Obiettivi
292400141127         wfobj  = V04obj;
292500141127         wfobj1 = V04obj1;
292600141127         wfobj2 = V04obj2;
292700150119         wfformula = V04formula;
292800150213         wfobj3 = V04obj3;
292900150213         wobj3da = V04obj3da;
293000150213         wobj3a  = V04obj3a;
293100150219         IF  V04obj3das = '-';
293200150213           wobj3da = wobj3da * -1;
293300150213         ENDIF;
293400150219         IF  V04obj3as = '-';
293500150213           wobj3a = wobj3a * -1;
293600150213         ENDIF;
293700150128         exsr ContrObj;
293800141127         V04obj1 = wfobj1;
293900141127         V04obj2 = wfobj2;
294000150119         V04formula = wfformula;
294100150213         V04obj3 = wfobj3;
294200150213         V04obj3da = %abs(wobj3da);
294300150213         V04obj3a  = %abs(wobj3a);
294400141127         V04msg = wmsg;
294500141127         IF  ErrGenerico;
294600141127           leavesr;
294700141127         ENDIF;
294800150302
294900150302       //?Trattative
295000150302         w002a = V04ttr;
295100150302         clear w050a;
295200150302         exsr SceltaTtr;
295300150302         V04ttr = w002a;
295400150302         V04msg = wmsg;
295500150302         IF  ErrGenerico;
295600150302           leavesr;
295700150302         ENDIF;
295800141030
295900141030       ENDSR;
296000150119
296100150119       //--------------------------------------------------------------
296200150119       //?Gestione tasto funzionale F01 da videata D04
296300150119       //?F01=Formula
296400150119       //--------------------------------------------------------------
296500150119       BEGSR F01D04;
296600150119
296700150119         savformula = V04formula;
296800150119         Video = 'S6';
296900150119         wInzS06 = *on;
297000150119
297100150119       ENDSR;
297200150128
297300150128       //--------------------------------------------------------------
297400150128       //?Gestione tasto funzionale F02 da videata D04
297500150128       //?F02=Obiettivo/Andamento
297600150128       //--------------------------------------------------------------
297700150128       BEGSR F02D04;
297800150216
297900150216         IF  H2nmfl <> 'V04OBJ1' and H2nmfl <> 'V04OBJ2' and
298000150216             H2nmfl <> 'V04OBJ3';
298100150216           ErrMessage  = *on;
298200150216           ErrGenerico = *on;
298300150216           PosCurOBJ1  = *on;
298400150216           V04msg = Msg(22);
298500150216           leavesr;
298600150216         ENDIF;
298700150128
298800150128         Video = 'S8';
298900150128         wInzS08 = *on;
299000150128
299100150128       ENDSR;
299200150302
299300150302       //--------------------------------------------------------------
299400150302       //?Gestione tasto funzionale F04 da videata D04
299500150302       //?F04=Scelta Trattative
299600150302       //--------------------------------------------------------------
299700150302       BEGSR F04D04;
299800150302
299900150302         Video = 'S9';
300000150302         wInzS09 = *on;
300100150302
300200150302       ENDSR;
300300141103
300400141103       //--------------------------------------------------------------
300500141103       //?Gestione tasto funzionale F06 da videata D04
300600141103       //?F06=Conferma
300700141103       //--------------------------------------------------------------
300800141103       BEGSR F06D04;
300900141103
301000141103       //?Devo ricaricare il subfile con le nuove parzializzazioni
301100141103         Video = 'S3';
301200141112         wInzS03 = *on;
301300141103
301400141103       ENDSR;
301500141030
301600141030       //--------------------------------------------------------------
301700141030       //?Gestione tasto funzionale F12 da videata D04
301800141030       //?F12=Ritorno
301900141030       //--------------------------------------------------------------
302000141030       BEGSR F12D04;
302100150116
302200150116       //?Se arrivo qua dalla videata con messaggio di troppi rcd
302300150116       //?devo ricaricare il subfile
302400150116         IF  TroppiRcd;
302500150116           wInzs03 = *on;
302600150116         ENDIF;
302700141030
302800141030       //?Ritorno alle selezioni
302900141030         Video = 'S3';
303000141104       //?Forzo il posizionamento del cursore al 1° rcd del subfile
303100141104         clear V03rcd;
303200141030
303300141030       ENDSR;
303400141118
303500141118       //--------------------------------------------------------------
303600141118       //?Gestione videata D05.
303700141118       //--------------------------------------------------------------
303800141118       BEGSR GesD05;
303900150116
304000150116         TroppiRcd = *on;
304100141118
304200141118       //?Inizializzazione videata
304300141118         IF  wInzD05;
304400141118           exsr InzD05;
304500141118           wInzD05 = *off;
304600141118         ENDIF;
304700141118
304800141118       //?Emissione videata
304900150112         write  KC01T01;
305000150112         exfmt  KC01D05;
305100141118         reset ErrMessage;
305200141118         reset ErrGenerico;
305300141118
305400141118         SELECT;
305500141118
305600141118       //?- F03=Fine
305700141118           WHEN  dsp_aid = c_F03;
305800141118             exsr F03D02;
305900141118
306000141118       //?- F05=Altre parzializzazioni
306100141118           WHEN  dsp_aid = c_F05;
306200141118             exsr F05S03;
306300141118
306400141118       //?- F12=Ritorno
306500141118           WHEN  dsp_aid = c_F12;
306600141118             exsr F12S03;
306700141118
306800141118         ENDSL;
306900141118
307000141118       ENDSR;
307100141118
307200141118       //--------------------------------------------------------------
307300141118       //?Inizializzazione Videata D05.
307400141118       //--------------------------------------------------------------
307500141118       BEGSR InzD05;
307600141118
307700141118       ENDSR;
307800150116
307900150116       //--------------------------------------------------------------
308000150116       //?Gestione videata S06.
308100150116       //--------------------------------------------------------------
308200150116       BEGSR GesS06;
308300150116
308400150116       //?Inizializzazione videata
308500150116         IF  wInzS06;
308600150116           exsr InzS06;
308700150116           wInzS06 = *off;
308800150116         ENDIF;
308900150119
309000150119       //?Visualizzazione del SFL
309100150119         SflDsp = (S06nrr <= *zeros);
309200150116
309300150116       //?Emissione videata
309400150119         exfmt  KC01CW6;
309500150116         reset ErrMessage;
309600150116         reset ErrGenerico;
309700150116
309800150116       //?- Enter Controllo ed esco
309900150116         exsr OpzS06;
310000150116         IF  ErrGenerico;
310100150116           leavesr;
310200150116         ENDIF;
310300150116
310400150116      //?Videata sucessiva
310500150216         IF H2nmrc = 'KC01D02';
310600150216           Video = 'D2';
310700150216           V02formula = savformula;
310800150216         ENDIF;
310900150216         IF H2nmrc = 'KC01W04';
311000150216           Video = 'D4';
311100150216           V04formula = savformula;
311200150216         ENDIF;
311300150116
311400150116       ENDSR;
311500150116
311600150116       //--------------------------------------------------------------
311700150116       //?Inizializzazione Videata S06.
311800150116       //--------------------------------------------------------------
311900150116       BEGSR InzS06;
312000150116
312100150116       //?Pulizia subfile
312200150116         exsr PulS06;
312300150116
312400150116       //?Caricamento subfile
312500150116         exsr Ries06;
312600150116
312700150116         SflEnd = *on;
312800150116
312900150116       //?Imposto il posizionamento al primo rcd
313000150116         IF  S06nrr > 0;
313100150119           V06rcd = 1;
313200150116         ELSE;
313300150119           clear V06rcd;
313400150116         ENDIF;
313500150116
313600150119         V06csr = V06rcd;
313700150116
313800150116       ENDSR;
313900150116
314000150116       //--------------------------------------------------------------
314100150116       //?Pulizia Subfile S06.
314200150116       //--------------------------------------------------------------
314300150116       BEGSR PulS06;
314400150116
314500150116       //?Pulizia subfile
314600150116         SflDsp    = *on;
314700150116         SflDspCtl = *on;
314800150116         write KC01CW6;
314900150116         SflDspCtl = *off;
315000150116         SflEnd    = *off;
315100150116
315200150119         clear V06rcd;
315300150119         clear V06csr;
315400150116         clear S06nrr;
315500150119         clear V06msg;
315600150116
315700150116         ErrMessage  = *off;
315800150116         ErrGenerico = *off;
315900150116
316000150116         WindDspF = IndDspF;
316100150116         reset WindDspFb;
316200150116         IndDspF  = WindDspF;
316300150116
316400150116       ENDSR;
316500150116
316600150116       //--------------------------------------------------------------
316700150116       //?Caricamento Subfile S06.
316800150116       //--------------------------------------------------------------
316900150116       BEGSR RieS06;
317000150116
317100150116         xx = 1;
317200150116         FOR xx by 1 to 6;
317300150116           clear  KC01SW6;
317400150116           PosCurOPZ = *off;
317500150119           V06formula = Formula(xx);
317600150119           V06des = Formula(xx) + DesForm(xx);
317700150116           S06nrr += 1;
317800150116           write  KC01SW6;
317900150116         ENDFOR;
318000150116
318100150116       ENDSR;
318200150116
318300150116       //--------------------------------------------------------------
318400150116       //?Gestione opzioni Subfile S06.
318500150116       //--------------------------------------------------------------
318600150116       BEGSR OpzS06;
318700150116
318800150116         xx = 1;
318900150116         FOR xx by 1 to 6;
319000150119           S06nrr = xx;
319100150119           chain S06nrr KC01SW6;
319200150116           IF  not %found;
319300150116             leave;
319400150116           ENDIF;
319500150116
319600150116           SELECT;
319700150116         //?- Nessuna opzione
319800150119           WHEN  V06opz = *blank;
319900150116
320000150116         //?- 1 = Scelta
320100150119           WHEN  V06opz = '1';
320200150119             savformula = V06formula;
320300150121             PosCurOBJ2 = *on;
320400150116             leave;
320500150116
320600150116           OTHER;
320700150116             ErrMessage  = *on;
320800150116             ErrGenerico = *on;
320900150116             PosCurOPZ   = *on;
321000150216             V06msg = Msg(11);
321100150116             update KC01SW6;
321200150116             leave;
321300150116           ENDSL;
321400150116
321500150116         ENDFOR;
321600150116
321700150116       ENDSR;
321800150121
321900150121       //--------------------------------------------------------------
322000150121       //?Gestione videata D07.
322100150121       //--------------------------------------------------------------
322200150121       BEGSR GesD07;
322300150121
322400150121       //?Inizializzazione videata
322500150121         IF  wInzD07;
322600150121           exsr InzD07;
322700150121           wInzD07 = *off;
322800150121         ENDIF;
322900150121
323000150121       //?Emissione videata
323100150121         exfmt  KC01W07;
323200150121         reset ErrMessage;
323300150121         reset ErrGenerico;
323400150121
323500150121         SELECT;
323600150121
323700150121       //?- F06=Conferma
323800150121           WHEN  dsp_aid = c_F06;
323900150121             exsr F06D07;
324000150121
324100150121         ENDSL;
324200150121
324300150121       ENDSR;
324400150121
324500150121       //--------------------------------------------------------------
324600150121       //?Inizializzazione Videata D07.
324700150121       //--------------------------------------------------------------
324800150121       BEGSR InzD07;
324900150121
325000150121         clear KC01W07;
325100150121         V07sino = 'SI';
325200150121
325300150121       ENDSR;
325400150121
325500150121       //--------------------------------------------------------------
325600150121       //?Gestione tasto funzionale F06 da videata D07
325700150121       //?F06=Conferma.
325800150121       //--------------------------------------------------------------
325900150121       BEGSR F06D07;
326000150121
326100150121         Video = 'S3';
326200150121
326300150121         IF  V07sino = 'NO';
326400150121           leavesr;
326500150121         ENDIF;
326600150121
326700150121         FOR  xx = 1 to rrnlast;
326800150121           chain xx KC01S03;
326900150121         //?Scrivo la fase
327000150121           clear TRKC71DS;
327100150212           IKC71ncm = %dec(V04ncm:7:0);
327200150121           IKC71ksu = VS3ksu;
327300150121           IKC71ksc = VS3ksc;
327400150121           IKC71cpo = VS3cpo;
327500150121           IKC71acm = FaseObjFine;
327600150121           IKC71pea = VS3oprop;
327700150122           IKC71aut = 'A';
327800150128           IKC71dfa = %dec(%date());
327900150128           IKC71hfc = %dec(%time());
328000150121           TRKC71R (kpjba:TRKC71DS);
328100150121         ENDFOR;
328200150121
328300150121       //?Ricarico il subfile
328400150121         wInzS03 = *on;
328500150121
328600150121       ENDSR;
328700150128
328800150128       //--------------------------------------------------------------
328900150128       //?Gestione videata S08.
329000150128       //--------------------------------------------------------------
329100150128       BEGSR GesS08;
329200150128
329300150128       //?Inizializzazione videata
329400150128         IF  wInzS08;
329500150128           exsr InzS08;
329600150128           wInzS08 = *off;
329700150128         ENDIF;
329800150128
329900150128       //?Visualizzazione del SFL
330000150128         SflDsp = (S08nrr <= *zeros);
330100150128
330200150128       //?Emissione videata
330300150128         exfmt  KC01CW8;
330400150128         reset ErrMessage;
330500150128         reset ErrGenerico;
330600150128
330700150128       //?- Enter Controllo ed esco
330800150128         exsr OpzS08;
330900150128         IF  ErrGenerico;
331000150128           leavesr;
331100150128         ENDIF;
331200150128
331300150128      //?Videata sucessiva
331400150216         IF H2nmrc = 'KC01D02';
331500150216           Video = 'D2';
331600150216         ENDIF;
331700150216         IF H2nmrc = 'KC01W04';
331800150216           Video = 'D4';
331900150216         ENDIF;
332000150128
332100150128       ENDSR;
332200150128
332300150128       //--------------------------------------------------------------
332400150128       //?Inizializzazione Videata S08.
332500150128       //--------------------------------------------------------------
332600150128       BEGSR InzS08;
332700150128
332800150128       //?Pulizia subfile
332900150128         exsr PulS08;
333000150128
333100150128       //?Caricamento subfile
333200150128         exsr Ries08;
333300150128
333400150128         SflEnd = *on;
333500150128
333600150128       //?Imposto il posizionamento al primo rcd
333700150128         IF  S08nrr > 0;
333800150128           V08rcd = 1;
333900150128         ELSE;
334000150128           clear V08rcd;
334100150128         ENDIF;
334200150128
334300150128         V08csr = V08rcd;
334400150128
334500150128       ENDSR;
334600150128
334700150128       //--------------------------------------------------------------
334800150128       //?Pulizia Subfile S08.
334900150128       //--------------------------------------------------------------
335000150128       BEGSR PulS08;
335100150128
335200150128       //?Pulizia subfile
335300150128         SflDsp    = *on;
335400150128         SflDspCtl = *on;
335500150128         write KC01CW8;
335600150128         SflDspCtl = *off;
335700150128         SflEnd    = *off;
335800150128
335900150128         clear V08rcd;
336000150128         clear V08csr;
336100150128         clear S08nrr;
336200150128         clear V08msg;
336300150128
336400150128         ErrMessage  = *off;
336500150128         ErrGenerico = *off;
336600150128
336700150128         WindDspF = IndDspF;
336800150128         reset WindDspFb;
336900150128         IndDspF  = WindDspF;
337000150128
337100150128       ENDSR;
337200150128
337300150128       //--------------------------------------------------------------
337400150128       //?Caricamento Subfile S08.
337500150128       //--------------------------------------------------------------
337600150128       BEGSR RieS08;
337700150128
337800150128         xx = 1;
337900150128         FOR xx by 1 to 5;
338000150128           clear  KC01SW8;
338100150128           PosCurOPZ = *off;
338200150128           V08obj = Obiettivo(xx);
338300150128           V08des = Obiettivo(xx) + ' ' + DesObj(xx);
338400150128           S08nrr += 1;
338500150128           write  KC01SW8;
338600150128         ENDFOR;
338700150128
338800150128       ENDSR;
338900150128
339000150128       //--------------------------------------------------------------
339100150128       //?Gestione opzioni Subfile S08.
339200150128       //--------------------------------------------------------------
339300150128       BEGSR OpzS08;
339400150128
339500150128         xx = 1;
339600150128         FOR xx by 1 to 5;
339700150128           S08nrr = xx;
339800150128           chain S08nrr KC01SW8;
339900150128           IF  not %found;
340000150128             leave;
340100150128           ENDIF;
340200150128
340300150128           SELECT;
340400150128         //?- Nessuna opzione
340500150128           WHEN  V08opz = *blank;
340600150128
340700150128         //?- 1 = Scelta
340800150128           WHEN  V08opz = '1';
340900150216             exsr  Scelta08;
341000150216             leave;
341100150128
341200150128           OTHER;
341300150128             ErrMessage  = *on;
341400150128             ErrGenerico = *on;
341500150128             PosCurOPZ   = *on;
341600150216             V08msg = Msg(11);
341700150128             update KC01SW8;
341800150128             leave;
341900150128           ENDSL;
342000150128
342100150128         ENDFOR;
342200150128
342300150128       ENDSR;
342400150216
342500150216       //--------------------------------------------------------------
342600150216       //?Ritorno la scelta fatta da videata D08.
342700150216       //--------------------------------------------------------------
342800150216       BEGSR Scelta08;
342900150216
343000150216         SELECT;
343100150216         WHEN  H2nmfl = 'V02OBJ1';
343200150216           V02obj1 = V08obj;
343300150216           POScurOBJ1 = *on;
343400150216         WHEN  H2nmfl = 'V02OBJ2';
343500150216           V02obj2 = V08obj;
343600150216           POScurOBJ2 = *on;
343700150216         WHEN  H2nmfl = 'V02OBJ3';
343800150216           V02obj3 = V08obj;
343900150216           POScurOBJ3 = *on;
344000150216         WHEN  H2nmfl = 'V04OBJ1';
344100150216           V04obj1 = V08obj;
344200150216           POScurOBJ1 = *on;
344300150216         WHEN  H2nmfl = 'V04OBJ2';
344400150216           V04obj2 = V08obj;
344500150216           POScurOBJ2 = *on;
344600150216         WHEN  H2nmfl = 'V04OBJ3';
344700150216           V04obj3 = V08obj;
344800150216           POScurOBJ3 = *on;
344900150216         ENDSL;
345000150216
345100150216       ENDSR;
345200150302
345300150302       //--------------------------------------------------------------
345400150302       //?Gestione videata S09.
345500150302       //--------------------------------------------------------------
345600150302       BEGSR GesS09;
345700150302
345800150302       //?Inizializzazione videata
345900150302         IF  wInzS09;
346000150302           exsr InzS09;
346100150302           wInzS09 = *off;
346200150302         ENDIF;
346300150302
346400150302       //?Visualizzazione del SFL
346500150302         SflDsp = (S09nrr <= *zeros);
346600150302
346700150302       //?Emissione videata
346800150302         exfmt  KC01CW9;
346900150302         reset ErrMessage;
347000150302         reset ErrGenerico;
347100150302
347200150302       //?- Enter Controllo ed esco
347300150302         exsr OpzS09;
347400150302         IF  ErrGenerico;
347500150302           leavesr;
347600150302         ENDIF;
347700150302
347800150302      //?Videata sucessiva
347900150302         IF H2nmrc = 'KC01D02';
348000150302           Video = 'D2';
348100150302         ENDIF;
348200150302         IF H2nmrc = 'KC01W04';
348300150302           Video = 'D4';
348400150302         ENDIF;
348500150302
348600150302       ENDSR;
348700150302
348800150302       //--------------------------------------------------------------
348900150302       //?Inizializzazione Videata S09.
349000150302       //--------------------------------------------------------------
349100150302       BEGSR InzS09;
349200150302
349300150302       //?Pulizia subfile
349400150302         exsr PulS09;
349500150302
349600150302       //?Caricamento subfile
349700150302         exsr Ries09;
349800150302
349900150302         SflEnd = *on;
350000150302
350100150302       //?Imposto il posizionamento al primo rcd
350200150302         IF  S09nrr > 0;
350300150302           V09rcd = 1;
350400150302         ELSE;
350500150302           clear V09rcd;
350600150302         ENDIF;
350700150302
350800150302         V09csr = V09rcd;
350900150302
351000150302       ENDSR;
351100150302
351200150302       //--------------------------------------------------------------
351300150302       //?Pulizia Subfile S09.
351400150302       //--------------------------------------------------------------
351500150302       BEGSR PulS09;
351600150302
351700150302       //?Pulizia subfile
351800150302         SflDsp    = *on;
351900150302         SflDspCtl = *on;
352000150302         write KC01CW9;
352100150302         SflDspCtl = *off;
352200150302         SflEnd    = *off;
352300150302
352400150302         clear V09rcd;
352500150302         clear V09csr;
352600150302         clear S09nrr;
352700150302         clear V09msg;
352800150302
352900150302         ErrMessage  = *off;
353000150302         ErrGenerico = *off;
353100150302
353200150302         WindDspF = IndDspF;
353300150302         reset WindDspFb;
353400150302         IndDspF  = WindDspF;
353500150302
353600150302       ENDSR;
353700150302
353800150302       //--------------------------------------------------------------
353900150302       //?Caricamento Subfile S09.
354000150302       //--------------------------------------------------------------
354100150302       BEGSR RieS09;
354200150302
354300150302         xx = 1;
354400150302         FOR xx by 1 to 10;
354500150302           clear  KC01SW9;
354600150302           PosCurOPZ = *off;
354700150302           V09ttr = Trattative(xx);
354800150302           V09des = Trattative(xx) + ' ' + DesTtr(xx);
354900150302           S09nrr += 1;
355000150302           write  KC01SW9;
355100150302         ENDFOR;
355200150302
355300150302       ENDSR;
355400150302
355500150302       //--------------------------------------------------------------
355600150302       //?Gestione opzioni Subfile S09.
355700150302       //--------------------------------------------------------------
355800150302       BEGSR OpzS09;
355900150302
356000150302         xx = 1;
356100150302         FOR xx by 1 to 10;
356200150302           S09nrr = xx;
356300150302           chain S09nrr KC01SW9;
356400150302           IF  not %found;
356500150302             leave;
356600150302           ENDIF;
356700150302
356800150302           SELECT;
356900150302         //?- Nessuna opzione
357000150302           WHEN  V09opz = *blank;
357100150302
357200150302         //?- 1 = Scelta
357300150302           WHEN  V09opz = '1';
357400150302             exsr  Scelta09;
357500150302             leave;
357600150302
357700150302           OTHER;
357800150302             ErrMessage  = *on;
357900150302             ErrGenerico = *on;
358000150302             PosCurOPZ   = *on;
358100150302             V09msg = Msg(11);
358200150302             update KC01SW9;
358300150302             leave;
358400150302           ENDSL;
358500150302
358600150302         ENDFOR;
358700150302
358800150302       ENDSR;
358900150302
359000150302       //--------------------------------------------------------------
359100150302       //?Ritorno la scelta fatta da videata D09.
359200150302       //--------------------------------------------------------------
359300150302       BEGSR Scelta09;
359400150302
359500150304         IF H2nmrc = 'KC01D02';
359600150304           V02ttr = V09ttr;
359700150304         ENDIF;
359800150304         IF H2nmrc = 'KC01W04';
359900150304           V04ttr = V09ttr;
360000150304         ENDIF;
360100150302         POScurTTR = *on;
360200150302
360300150302       ENDSR;
360400141030
360500141030       //--------------------------------------------------------------
360600141030       //?Controllo Distretto.
360700141030       //--------------------------------------------------------------
360800141030       BEGSR Distretto;
360900141030
361000141030         IF  w001a = *blanks;
361100141030           leavesr;
361200141030         ENDIF;
361300141030
361400141030       //?Ricerca Distretto
361500141030         IF  %scan('?' : w001a) > 0;
361600141030           ErrGenerico = *on;
361700141030           PosCurCDI   = *on;
361800141030           idTabella = '17';
361900141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
362000141030                            tastoUscita);
362100141030           w001a = idElemento;
362200141030         ENDIF;
362300141030       //?Deve essere un Distretto valido
362400141030         clear xx;
362500141030         xx = %lookup(w001a : skDistretti);
362600141030         IF  xx = 0;
362700141030           ErrMessage  = *on;
362800141030           ErrGenerico = *on;
362900141030           PosCurCDI   = *on;
363000141030           wmsg = Msg(03);
363100141030           leavesr;
363200141030         ENDIF;
363300141030       //?Decodifico distretto
363400141030         w050a = skDesDist(xx);
363500141030       //?In carico all'Utente
363600141030         IF  %lookup (w001a:DIS) = 0;
363700141030           ErrMessage  = *on;
363800141030           ErrGenerico = *on;
363900141030           PosCurCDI   = *on;
364000141030           wmsg = %trim(Msg(03)) + '. Non in gestione';
364100141030           leavesr;
364200141030         ENDIF;
364300141030
364400141030       ENDSR;
364500141030
364600141030       //--------------------------------------------------------------
364700141030       //?Controllo Area.
364800141030       //--------------------------------------------------------------
364900141030       BEGSR Area;
365000141030
365100141030         IF  w003a = *blanks;
365200141030           leavesr;
365300141030         ENDIF;
365400141030
365500141030       //?Ricerca Area
365600141030         IF  %scan('?' : w003a) > 0;
365700141030           ErrGenerico = *on;
365800141030           PosCurCAR   = *on;
365900141104           idTabella = '05';
366000141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
366100141030                          tastoUscita);
366200141030           w003a = idElemento;
366300141030         ENDIF;
366400141104         IF  w003a = *blanks;
366500141104           leavesr;
366600141104         ENDIF;
366700141030       //?Accetto solo dati numerici
366800141030         xx = 1;
366900141030         FOR xx by 1 to %len(w003a);
367000141030           IF  %subst(w003a:xx:1) <> *blanks and
367100141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
367200141030             ErrMessage  = *on;
367300141030             ErrGenerico = *on;
367400141030             PosCurCAR   = *on;
367500141030             wmsg = Msg(04);
367600141030             leavesr;
367700141030           ENDIF;
367800141030         ENDFOR;
367900141030       //?Deve essere un'Area valida
368000141030         clear xx;
368100141030         w0030 = %dec(w003a:3:0);
368200141030         xx = %lookup(%editc(w0030:'X'):skAree);
368300141030         IF  xx = 0;
368400141030           ErrMessage  = *on;
368500141030           ErrGenerico = *on;
368600141030           PosCurCAR   = *on;
368700141030           wmsg = Msg(04);
368800141030           leavesr;
368900141030         ENDIF;
369000141030       //?Decodifico area
369100141030         w050a = skDesArea(xx);
369200141030       //?Deve essere un'area in carico all'utente
369300141030         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
369400141030           ErrGenerico = *on;
369500141030           ErrMessage  = *on;
369600141030           PosCurCAR   = *on;
369700141030           wmsg = %trim(Msg(04)) + '. Non in gestione';
369800141030           leavesr;
369900141030         ENDIF;
370000141030
370100141030       ENDSR;
370200141030
370300141030       //--------------------------------------------------------------
370400141030       //?Controllo Filiale.
370500141030       //--------------------------------------------------------------
370600141030       BEGSR Filiale;
370700141030
370800141030         IF  w003a = *blanks;
370900141030           leavesr;
371000141030         ENDIF;
371100141030
371200141030       //?Ricerca Filale
371300141030         IF  %scan('?' : w003a) > 0;
371400141030           clear pinFIL;
371500141030           clear pinFAG;
371600141030           clear pinDES;
371700141030           clear pinDIT;
371800141030           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
371900141030           IF pinFIL > *zeros;
372000141030             w003a = pinFIL;
372100141030             w050a = pinDES;
372200141030           ENDIF;
372300141030           ErrGenerico = *on;
372400141030           PosCurFIL   = *on;
372500141030         ENDIF;
372600141030       //?Accetto solo dati numerici
372700141030         xx = 1;
372800141030         FOR xx by 1 to %len(w003a);
372900141030           IF  %subst(w003a:xx:1) <> *blanks and
373000141030               %check(Digits:%subst(w003a:xx:1)) > *zeros;
373100141030             ErrMessage  = *on;
373200141030             ErrGenerico = *on;
373300141030             PosCurFIL   = *on;
373400141030             wmsg = Msg(05);
373500141030             leavesr;
373600141030           ENDIF;
373700141030         ENDFOR;
373800141030         w0030 = %dec(w003a:3:0);
373900141030       //?Deve esistere la Filiale
374000141030         chain w0030 AZORG01L;
374100141030         IF  not %found(AZORG01L) or ORGfva <> *blanks;
374200141030           ErrMessage  = *on;
374300141030           ErrGenerico = *on;
374400141030           PosCurFIL   = *on;
374500141030           wmsg = Msg(05);
374600141030           leavesr;
374700141030         ENDIF;
374800141030       //?Decodifico filiale
374900141030         w050a = ORGdes;
375000141030       //?Deve essere una filiale in carico all'utente
375100141030         IF  %lookup(%editc(w0030:'X'):POG) = 0;
375200141030           ErrGenerico = *on;
375300141030           ErrMessage  = *on;
375400141030           PosCurFIL   = *on;
375500141030           wmsg = %trim(Msg(05)) + '. Non in gestione';
375600141030           leavesr;
375700141030         ENDIF;
375800141104       //?Se presente anche il distretto o l'area devono essere congruenti
375900141104         IF  V02cdi <> *blanks and V02fil <> *blanks;
376000141105           IF  V02cdi <> ORGfl3;
376100141104             ErrGenerico = *on;
376200141104             ErrMessage  = *on;
376300141104             PosCurFIL   = *on;
376400141104             wmsg = %trim(Msg(05)) + '. Non congruente con il distretto';
376500141104             leavesr;
376600141104           ENDIF;
376700141104           IF  V02car <> %editc(ORGcar:'X');
376800141104             ErrGenerico = *on;
376900141104             ErrMessage  = *on;
377000141104             PosCurFIL   = *on;
377100141104             wmsg = %trim(Msg(05)) + '. Non congruente con l''area';
377200141104             leavesr;
377300141104           ENDIF;
377400141104         ENDIF;
377500141030
377600141030       ENDSR;
377700141030
377800141030       //--------------------------------------------------------------
377900141030       //?Controllo Commerciale.
378000141030       //--------------------------------------------------------------
378100141030       BEGSR Commerciale;
378200141030
378300141030         IF  w007a = *blanks;
378400141030           leavesr;
378500141030         ENDIF;
378600141030
378700141030       //?Ricerca Commerciale
378800141030         IF  %scan('?' : w007a) > 0;
378900141030           clear TRMK43DS;
379000141030           IMK43solu = 'S';
379100141030           IMK43fil = DUTpou;
379200141030           trmk43r (kpjba : TRMK43DS);
379300141030           IF  OMK43err = *off and OMK43fxx = *blanks;
379400141030             w007a = %editc(OMK43cmm:'X');
379500141030             w050a = OMK43des;
379600141030           ENDIF;
379700141030           ErrGenerico = *on;
379800141030           PosCurCMM   = *on;
379900141030         ENDIF;
380000141030       //?Accetto solo dati numerici
380100141030         xx = 1;
380200141030         FOR xx by 1 to %len(w007a);
380300141030           IF  %subst(w007a:xx:1) <> *blanks and
380400141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
380500141030             ErrMessage  = *on;
380600141030             ErrGenerico = *on;
380700141030             PosCurCMM   = *on;
380800150216             wmsg = Msg(06);
380900141030             leavesr;
381000141030           ENDIF;
381100141030         ENDFOR;
381200141030         w0070 = %dec(w007a:7:0);
381300141030       //?Deve esistere il Commerciale
381400141030         chain (w0070) AZCMM01L;
381500141030         IF  not %found(AZCMM01L);
381600141030           ErrMessage  = *on;
381700141030           ErrGenerico = *on;
381800141030           PosCurCMM   = *on;
381900150216           wmsg = Msg(06);
382000141030           leavesr;
382100141030         ENDIF;
382200141030       //?Decodifico Commerciale
382300141030         w050a = CMMdes;
382400141030       //?Deve essere un Commerciale in carico all'utente
382500141030         clear TNTAA1DS;
382600141119         ITAA1caut = '§utegtc';
382700141030         ITAA1cmm  = w0070;
382800141030         kpjbu = TNTAA1DS;
382900141030         tntaa1c (kpjba);
383000150217         TNTAA1DS = kpjbu;
383100141030         IF  OTAA1fabi = 'N';
383200141030           ErrGenerico = *on;
383300141030           ErrMessage  = *on;
383400141030           PosCurCMM   = *on;
383500150216           wmsg = %trim(Msg(06)) + '. Non in gestione';
383600141030           leavesr;
383700141030         ENDIF;
383800141030       //?Deve essere un Commerciale Unificante
383900141030         IF  w0070 <> CMMuni;
384000141030           ErrGenerico = *on;
384100141030           ErrMessage  = *on;
384200141030           PosCurCMM   = *on;
384300150216           wmsg = %trim(Msg(06)) + '. Non è un Commerciale Unificante';
384400141030           leavesr;
384500141030         ENDIF;
384600141030
384700141030       ENDSR;
384800141030
384900141030       //--------------------------------------------------------------
385000141030       //?Controllo Cliente.
385100141030       //--------------------------------------------------------------
385200141030       BEGSR Cliente;
385300141030
385400141030         IF  w007a = *blanks;
385500141030           leavesr;
385600141030         ENDIF;
385700141030
385800141030       //?Ricerca Cliente
385900141030         IF  %scan('?' : w007a) > 0;
386000141030           clear TNTAI1DS;
386100141030           ITAI1ric = 'S';
386200141030           ITAI1abi = wabi;
386300141112           ITAI1aut = '*C';
386400141030           tntai1r (kpjba : TNTAI1DS);
386500141030           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
386600141030               OTAI1err <> *blanks;
386700141107             clear V02ksu;
386800141030           ELSE;
386900141030             w007a = %editc(OTAI1ksc:'X');
387000141030           ENDIF;
387100141030           ErrGenerico = *on;
387200141107           PosCurKSU   = *on;
387300141030         ENDIF;
387400141030       //?Accetto solo dati numerici
387500141030         xx = 1;
387600141030         FOR xx by 1 to %len(w007a);
387700141030           IF  %subst(w007a:xx:1) <> *blanks and
387800141030               %check(Digits:%subst(w007a:xx:1)) > *zeros;
387900141030             ErrMessage  = *on;
388000141030             ErrGenerico = *on;
388100141107             PosCurKSU   = *on;
388200150216             wmsg = Msg(07);
388300141030             leavesr;
388400141030           ENDIF;
388500141030         ENDFOR;
388600141030         w0070 = %dec(w007a:7:0);
388700141030       //?Deve esistere il Cliente
388800141030         clear TIBS69DS;
388900141030         clear ACOrag;
389000141030         I69kac = %dec(w007a:7:0);
389100141030         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
389200141030         IF  O69err <> *blanks;
389300141030           ErrMessage  = *on;
389400141030           ErrGenerico = *on;
389500141107           PosCurKSU   = *on;
389600150216           wmsg      = Msg(07);
389700141030         ENDIF;
389800141030       //?Decodifico Cliente
389900141030         w050a = ACOrag;
390000141030       //?Deve essere un Cliente in carico all'utente
390100141030         clear TNTAA1DS;
390200141119         ITAA1caut = '§utegtc';
390300141030         ITAA1ksc  = %dec(w007a:7:0);
390400141030         kpjbu = TNTAA1DS;
390500141030         tntaa1c (kpjba);
390600150217         TNTAA1DS = kpjbu;
390700141030         IF  OTAA1fabi = 'N';
390800141030           ErrGenerico = *on;
390900141030           ErrMessage  = *on;
391000141107           PosCurKSU   = *on;
391100150216           wmsg = %trim(Msg(07)) + '. Non in gestione';
391200141030           leavesr;
391300141030         ENDIF;
391400141030
391500141030       ENDSR;
391600141030
391700141030       //--------------------------------------------------------------
391800141030       //?Controllo Importanza Cliente.
391900141030       //--------------------------------------------------------------
392000141030       BEGSR ImpCliente;
392100141030
392200141030         IF  w001a = *blanks;
392300141030           leavesr;
392400141030         ENDIF;
392500141030
392600141030         IF  %scan('?' : w001a) > 0;
392700141030           ErrGenerico = *on;
392800141030           idTabella = 'IC';
392900141030           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
393000141030                          tastoUscita);
393100141030           w001a = idElemento;
393200141030         ENDIF;
393300141030         IF  %lookup(w001a : skIC) = 0;
393400141030           ErrMessage  = *on;
393500141030           ErrGenerico = *on;
393600141030           PosCurCLV1  = *on;
393700150216           wmsg = Msg(08);
393800141030           leavesr;
393900141030         ENDIF;
394000141030
394100141030       ENDSR;
394200141127
394300141127       //--------------------------------------------------------------
394400141127       //?Controllo Obiettivi.
394500141127       //--------------------------------------------------------------
394600150128       BEGSR ContrObj;
394700141127
394800150128       //?Se Obiettivo inizio NO "di cui"
394900150119         IF  wfobj = 'I' and
395000150119            (wfobj1 = 'P' or wfobj1 = 'F' or
395100150119             wfobj2 = 'P' or wfobj2 = 'F');
395200141127           ErrMessage  = *on;
395300141127           ErrGenerico = *on;
395400150216           wmsg = Msg(14);
395500141127           IF  wfobj1 <> *blanks;
395600141127             PosCurOBJ1  = *on;
395700141127             leavesr;
395800141127           ENDIF;
395900141127           IF  wfobj2 <> *blanks;
396000141127             PosCurOBJ2  = *on;
396100141127             leavesr;
396200141127           ENDIF;
396300141127         ENDIF;
396400141127       //?Se Obiettivo proposto NO "di cui" Finale
396500141127         IF  wfobj = 'P' and (wfobj1 = 'F' or wfobj2 = 'F');
396600141127           ErrMessage  = *on;
396700141127           ErrGenerico = *on;
396800150216           wmsg = Msg(15);
396900141127           IF  wfobj1 = 'F';
397000141127             PosCurOBJ1  = *on;
397100141127             leavesr;
397200141127           ENDIF;
397300141127           IF  wfobj2 = 'F';
397400141127             PosCurOBJ2  = *on;
397500141127             leavesr;
397600141127           ENDIF;
397700141127         ENDIF;
397800150128       //?Deve essere un obiettivo valido
397900150128         IF  wfobj1 <> *blanks and %lookup (wfobj1:Obiettivo) = 0;
398000150128           ErrMessage  = *on;
398100150128           ErrGenerico = *on;
398200150128           PosCurOBJ1  = *on;
398300150216           wmsg = Msg(13);
398400150128           leavesr;
398500150128         ENDIF;
398600150128         IF  wfobj2 <> *blanks and %lookup (wfobj2:Obiettivo) = 0;
398700150128           ErrMessage  = *on;
398800150128           ErrGenerico = *on;
398900150128           PosCurOBJ2  = *on;
399000150216           wmsg = Msg(13);
399100150128           leavesr;
399200150128         ENDIF;
399300150128
399400141127       //?Non posso accettare 2 obiettivi uguali
399500141127         IF  wfobj1 = wfobj2 and (wfobj1 <> *blanks or
399600141127                                  wfobj2 <> *blanks);
399700141127           ErrMessage  = *on;
399800141127           ErrGenerico = *on;
399900141127           PosCurOBJ1  = *on;
400000150216           wmsg = Msg(16);
400100141127           leavesr;
400200141127         ENDIF;
400300141127       //?Devono essere tutti e 2 impostati
400400141127         IF  (wfobj1 = *blanks and wfobj2 <> *blanks) or
400500141127             (wfobj1 <> *blanks and wfobj2 = *blanks);
400600141127           ErrMessage  = *on;
400700141127           ErrGenerico = *on;
400800150216           wmsg = Msg(17);
400900141127           IF  wfobj1 = *blanks;
401000141127             PosCurOBJ1  = *on;
401100141127             leavesr;
401200141127           ENDIF;
401300141127           IF  wfobj2 = *blanks;
401400141127             PosCurOBJ2  = *on;
401500141127             leavesr;
401600141127           ENDIF;
401700141127         ENDIF;
401800150127
401900150127       //?Se il primo byte del campo formula è vuoto controllo se è
402000150127       //?uno dei campi validi e sistemo il campo
402100150127         IF  wfformula <> *blanks and %subst(wfformula:1:1) = *blanks
402200150127             and %check(ValoriFormula:%subst(wfformula:2:1)) = *zeros;
402300150127           %subst(wfformula:1:1) = %subst(wfformula:2:1);
402400150127           %subst(wfformula:2:1) = *blanks;
402500150127         ENDIF;
402600141127
402700150119       //?La formula deve essere una formula valida
402800150119         IF  %lookup (wfformula:Formula) = 0 and wfobj1 <> *blanks;
402900141127           ErrMessage  = *on;
403000141127           ErrGenerico = *on;
403100150119           PosCurFORM  = *on;
403200150216           wmsg = Msg(18);
403300141127           leavesr;
403400141127         ENDIF;
403500141128
403600150121       //?Se ci sono gli obiettivi devo avere anche la formula
403700141128         IF  wfobj1 <> *blanks and wfobj2 <> *blanks and
403800150119             wfformula = *blanks;
403900141128           ErrMessage  = *on;
404000141128           ErrGenerico = *on;
404100150119           PosCurFORM  = *on;
404200150216           wmsg = Msg(18);
404300141128           leavesr;
404400141128         ENDIF;
404500150121
404600150121       //?Se ho la formula devo avere anche gli obiettivi
404700150121         IF  wfobj1 = *blanks and wfobj2 = *blanks and
404800150121             wfformula <> *blanks;
404900150121           ErrMessage  = *on;
405000150121           ErrGenerico = *on;
405100150122           PosCurOBJ1  = *on;
405200150216           wmsg = Msg(19);
405300150121           leavesr;
405400150121         ENDIF;
405500150213
405600150213       //?Deve essere un obiettivo valido
405700150213         IF  wfobj3 <> *blanks and %lookup (wfobj3:Obiettivo) = 0;
405800150213           ErrMessage  = *on;
405900150213           ErrGenerico = *on;
406000150213           PosCurOBJ3  = *on;
406100150216           wmsg = Msg(13);
406200150213           leavesr;
406300150213         ENDIF;
406400150213       //?Almeno 1 dei 2 valori deve essere presente
406500150213         IF  wfobj3 <> *blanks and wobj3da = 0 and wobj3a = 0;
406600150216           //ErrMessage  = *on;
406700150216           //ErrGenerico = *on;
406800150216           //PosCurOBJ3DA = *on;
406900150216           //wmsg = Msg(20);
407000150216           //leavesr;
407100150216         ENDIF;
407200150213       //?Se impostate % serve il flag obiettivo
407300150213         IF  wfobj3 = *blanks and (wobj3da <> 0 or wobj3a <> 0);
407400150213           ErrMessage  = *on;
407500150213           ErrGenerico = *on;
407600150213           PosCurOBJ3  = *on;
407700150216           wmsg = Msg(13);
407800150213           leavesr;
407900150213         ENDIF;
408000150213         IF  wobj3da > wobj3a;
408100150213           ErrMessage  = *on;
408200150213           ErrGenerico = *on;
408300150213           PosCurOBJ3DA = *on;
408400150216           wmsg = Msg(21);
408500150213           leavesr;
408600150213         ENDIF;
408700141127
408800141127       ENDSR;
408900150302
409000150302       //--------------------------------------------------------------
409100150302       //?Controllo Scelta Trattative.
409200150302       //--------------------------------------------------------------
409300150302       BEGSR SceltaTtr;
409400150302
409500150302         IF  w002a = *blanks;
409600150302           leavesr;
409700150302         ENDIF;
409800150302
409900150302       //?Deve essere una Scelta valida
410000150302         clear xx;
410100150302         xx = %lookup(w002a : Trattative);
410200150302         IF  xx = 0;
410300150302           ErrMessage  = *on;
410400150302           ErrGenerico = *on;
410500150302           PosCurTTR   = *on;
410600150302           wmsg = Msg(24);
410700150302           leavesr;
410800150302         ENDIF;
410900150302
411000150302       ENDSR;
411100141121
411200141121       //--------------------------------------------------------------
411300150218       //?Richiamo pgm TRKC76R.
411400141121       //--------------------------------------------------------------
411500150218       BEGSR CallTRKC76;
411600141121
411700150218         wTRKC76 = *on;
411800141121
411900150218         clear TRKC76DS;
412000150303         IKC76ric = wtpric;
412100150218         IKC76ncm = CMIncm;
412200150218         IKC76ksu = CMIksu;
412300150218         IKC76ksc = CMIksc;
412400150218         IKC76cpo = CMIcpo;
412500150303         IKC76flgp = wflgp;
412600150220         IKC76acm = wFase;
412700150218         IKC76istat = CMIist;
412800150218
412900150218         trkc76r (kpjba:TRKC76DS);
413000150203
413100150203       ENDSR;
413200141028
413300141028       //--------------------------------------------------------------
413400141028       //?Operazioni finali.
413500141028       //--------------------------------------------------------------
413600141028       BEGSR RoutEnd;
413700150218
413800150218       //?Se richiamato chiudo i file per il pgm TRKC76R
413900150218         IF  wTRKC76;
414000150218           clear TRKC76DS;
414100150218           IKC76ric = 'C';
414200150218           trkc76r (kpjba:TRKC76DS);
414300150218         ENDIF;
414400141028
414500141028         *inLR = *on;
414600141028         return;
414700141028
414800141028       ENDSR;
414900141028
415000141028      /end-free
415100141028
415200141028       //--------------------------------------------------------------
415300141028       //?Schiere a tempo di compilazione.
415400141028       //--------------------------------------------------------------
415500150116
415600150116** -- Formule / DesForm -----------------------------------------------------*
415700150116=  : Uguale
415800150116>  : Maggiore
415900150116<  : Minore
416000150116>= : Maggiore Uguale
416100150116<= : Minore Uguale
416200150116<> : Diverso
416300150128** -- Obiettivo / DesObj ----------------------------------------------------*
416400150128I= Obiettivo Inizio
416500150128P= Obiettivo Proposto
416600150128F= Obiettivo Finale
416700150128T= Andamento Trattativa
416800150128C= Andamento Confronto Fatturazione
416900150302** -- Trattative / DesTtr----------------------------------------------------*
417000150302A = Avviate
417100150302AO= Avviate con Offerta
417200150302AN= Avviate senza Offerta
417300150302C = Chiuse
417400150302CN= Chiuse No Positive
417500150302CP= Chiuse Positive
417600150302SI= Tutte
417700150302T = Trattative non Forzate
417800150302F = Trattative Forzate
417900150302NO= Non avviate
418000141028** -- MSG -------------------------------------------------------------------*
418100141024Utente non abilitato alla Funzione.                                            1
418200141024Campagna Commerciale errata                                                    2
418300141027Distretto Errato                                                               3
418400141027Area Errata                                                                    4
418500141027Filiale Errata                                                                 5
418600150216Commerciale Unificante Errato                                                  6
418700150216Codice Cliente Errato                                                          7
418800150216Codice Importanza Clienti Errato                                               8
418900150216Immettere il Delta                                                             9
419000150216Delta previsto "DAL" incongruente con Delta previsto "AL"                     10
419100150216Opzione errata                                                                11
419200150216La modifica è possibile solo dal            al                                12
419300150216Impostare un Obiettivo/Andamento valido (F2)                                  13
419400150216Se richiesto Obiettivo Inizio non richiedere il "di cui"                      14
419500150216Non è possibile il "di cui" Finale se richiesto Obiettivo Proposto            15
419600150216Non è possibile il confronto tra 2 obiettivi uguali                           16
419700150216Impostare tutti e 2 gli obiettivi/andamento da confrontare                    17
419800150216Impostare una formula valida (F1=Legenda Formula)                             18
419900150216Impostare gli obiettivi/andamento                                             19
420000150216Immettere la % Obiettivo/Andamento                                            20
420100150216Obiettivo/Andamento "DAL" incongruente con Obiettivo/Andamento "AL"           21
420200150216Per poter fare F2 posizionarsi su obiettivo/andamento da inserire             22
420300150225Trattativa Forzata possibile solo dal            al                           23
420400150302Scelta trattativa non valida                                                  24
420500150611Impostare l'aera da elaborare                                                 25
