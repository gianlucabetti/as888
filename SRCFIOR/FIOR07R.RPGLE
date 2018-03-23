000100090223     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200140320     h DFTACTGRP(*NO) ACTGRP(*CALLER)
000300001006
000400001006      *****************************************************************
000500001006      *                                                               *
000600001016      *       GESTIONE ORDINI RITIRO MERCE                            *
000700001006      *                                                               *
000800001006      *****************************************************************
000900001006
001000001010      ****************************************************************
001100001010      *  RIEPILOGO INDICATORI
001200001010      ****************************************************************
001300140304      * 01 - Visualizzazione Storico Variazione
001400001031      * 02 - Visualizza causale mancato ritiro nel subfile fasi
001500001031      * 03 - Visualizza analisi proposta nel subfile proposte
001600001031      * 04 - Visualizza ORM fisso
001700010219      * 05 - Inversione di fondo nel campo distinta
001800010226      * 06 - Inversione di fondo nel campo autotrasportatore
001900010309      * 07 - Visualizzazione cuasale
002000040220      * 08 - Abilita tasto funzionale F1=Aggiungi in reclami
002100040220      * 09 - Abilita tasto funzionale F16=Visualizza reclami
002200040908      * 10 - Visualizza distinta nel subfile fasi
002300050502      * 11 - ORM con ricevuta di ritiro
002400010226      * 12 - SONO IN SEDE
002500061017      * 13 - P.O. gestione DPD visualizzo F9=Sollecito
002600061023      * 14 - P.O. emissione o P.O. ritiro DPD F2=Dati DPD
002700140304      * 15 - Visualizzazione proposta di variazione
002800070924      * 16 - Visualizzo 'STAMPATO'
002900081125      * 17 - Visualizza 'No addebito' nel subfile fasi
003000100222      * 18 - ORM con preavviso MAIL
003100061003      * 20 - subfile
003200061003      * 21 - subfile
003300001010      * 28 - ERRORE GENERICO DSPF
003400001010      * 30 - Comodo
003500010322      * 40 - Visualizza tasto x Variazioni
003600010322      * 41 - Visualizza tasto x Proposte
003700010322      * 42 - Visualizza tasto x Note
003800090211      * 43 - Visualizza tasto x Info Appuntamento
003900131125      * 44 - Visualizza tasto x Orari Servizio
004000131211      * 46 - HI tasto x Altri Dati
004100140121      * 48 - HI tasto x Note AUT
004200180222      * 50 - Abilito tasto F8=Int.Bolle
004300061003      * 99 - errore QCMDEXEC
004400001010      ****************************************************************
004500001010
004600001030     FFNORM01L  IF   E           K DISK
004700070911     ffnorg01l  if   e           k disk
004800001030     FFNORV01L  IF   E           K DISK
004900131029     fFNORVE1L  if   e           k disk
005000001031     FFNORF01L  IF   E           K DISK
005100001031     FFNORP01L  IF   E           K DISK
005200131029     fFNORPE1L  if   e           k disk
005300061017     FFNORN02L  IF a E           K DISK
005400090211     ffnora01l  if   e           k disk
005500140409     ffnore01l  if   e           k disk
005600140121     fFNORT01L  if   e           k disk
005700140303     fFNORPT1L  if   e           k disk
005800001011     FAZORG01L  IF   E           K DISK
005900010119     FTABEL00F  IF   E           K DISK
006000120612     Ffidst01l  IF   E           K DISK    usropn
006100180314     Ffiar405l  IF   E           K DISK    usropn
006200021202     FFIAPD01L  IF   E           K DISK
006300150526     fFITGD32L  if   e           k disk
006400001030     FFIOR07D   CF   E             WORKSTN  sfile(fior07s:nrr)
006500001031     F                                      sfile(fior07sf:nrrf)
006600001031     F                                      sfile(fior07sp:nrrp)
006700090223     F                                      sfile(fior07aps:nrra)
006800001010
006900001010      *------------------------------------------------------------------------*
007000001011     D kpoe            S                   LIKE(V1cpoe)
007100001016     D knsr            S                   LIKE(V1nsr)
007200001006     D knor            S                   LIKE(V1nor)
007300001016     D knrv            S                   LIKE(V1nrv)
007400001103     D kdtv            S                   LIKE(ORPdtv)
007500001103     D korv            S                   LIKE(ORPorv)
007600010305     D kdfo            S                   LIKE(ORMdfo)
007700010305     D kofo            S                   LIKE(ORMofo)
007800010305     D kfao            S                   LIKE(ORMfao)
007900090216     d kfar            s                   LIKE(ORMfao)
008000001030
008100120612     D knpg            S                   LIKE(DSTnpg) inz(4)
008200001030     D knfv            S                   LIKE(DSTnfv)
008300001030     D kfgs            S                   LIKE(DSTfgs)
008400021202
008500021202     D kpdpdr          S                   LIKE(APDpdr)
008600040908     D kpdtip          S                   LIKE(APDtip) inz('A')
008700001026
008800001026     D kazorg          S                   LIKE(ORGfil)
008900010119
009000010119     D kcod            S                   LIKE(TBLcod)
009100010119     D kkey            S                   LIKE(TBLkey)
009200010119     D kdes            S             30
009300001030
009400001030     D §doggi          S                   LIKE(v1dar)
009500001025     D §kpjbu          S                   LIKE(KPJBU)
009600151001     d wabi            s                   like(UTEaut)
009700151001     d w0030           s                   like(ORGfil)
009800041001
009900041001     d wlib            s             10
010000041001     d comman          s             80
010100041001     d lenght          s             15  5 inz(80)
010200001009
010300001006     D W0140           S             14  0
010400001006     D Wdtgio          S              8  0
010500001006     D dateu           S              8  0
010600001009
010700001006     D DATA_oggi       S               D   DATFMT(*eur)
010800061017
010900061017     d wprg            s                   like(ornprg)
011000061017     d xx              s              3  0
011100090210     d rigatf1         s              1    dim(78)
011200090210     d rigatf2         s              1    dim(62)
011300090210     d rigatf3         s              1    dim(62)
011400090210     d $loop           s              1  0
011500090210     d $rig            s              1  0
011600090211     d $h              s              3  0
011700090210     d $j              s              3  0
011800090210     d $k              s              3  0
011900090211     d $x              s              3  0
012000090210     d $y              s              3  0
012100090210     d $z              s              3  0
012200131028
012300131028     d wEndF05         s               n
012400140206
012500140206     D network         S                   LIKE(ORGdit)
012600001006
012700001010      *  titolo videata (lunghezza massima 34)
012800010223     D TIT_A           C                   CONST('***   INTERROGAZIONE O.R.M.  -
012900001010     D                                       ***')
013000001031
013100001031      *   S C H I E R E
013200180314     d cmdt            s             60    dim(2) ctdata perrcd(1)
013300061017     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
013400061017     d skpodpd         s              3  0 dim(250)                             p.o. DPD
013500151001     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
013600131028     d tf01            s              1    dim(25) ctdata perrcd(25)
013700090211     d tf02            s              1    dim(25) ctdata perrcd(25)
013800180222     d tf08            s              1    dim(25) ctdata perrcd(25)
013900090211     d tf09            s              1    dim(25) ctdata perrcd(25)
014000130913     d tf14            s              1    dim(25) ctdata perrcd(25)
014100131120     d tf15            s              1    dim(25) ctdata perrcd(25)
014200131028     d tf16            s              1    dim(25) ctdata perrcd(25)
014300090211     d tf19            s              1    dim(25) ctdata perrcd(25)
014400090211     d tf20            s              1    dim(25) ctdata perrcd(25)
014500090211     d tf22            s              1    dim(25) ctdata perrcd(25)
014600090210     d $tf             s              1    dim(25)
014700001006
014800001010      *   D S   I N T E R N E / E S T E R N E
014900001009
015000001009     D WLBDAT          DS                  INZ
015100001009     D  G02DAT                 1      8  0
015200001009     D  G02INV                 9     16  0
015300001009     D  G02ERR                17     17
015400001009     D  G02TGI                18     22  0
015500001020
015600001023     D                 DS
015700001023     D  v1cra1                 1      3  0
015800001023     D  v1cra2                 4      7  0
015900001023     D  v1cra3                 8     10  0
016000001023     D  v1ccra                 1     10  0
016100001020
016200001023     D                 DS
016300001023     D  v1cor1                 1      3  0
016400001023     D  v1cor2                 4      7  0
016500001023     D  v1cor3                 8     10  0
016600001023     D  v1ccor                 1     10  0
016700001020
016800001023     D                 DS
016900001023     D  v1crc1                 1      3  0
017000001023     D  v1crc2                 4      7  0
017100001023     D  v1crc3                 8     10  0
017200001023     D  v1ccrc                 1     10  0
017300001031
017400001031     D                 DS
017500001031     D  v2dtvg                 1      2
017600001031     D  v2dtvm                 3      4
017700001031     D  v2dtva                 5      8
017800001031     D  v2dtv                  1      8  0
017900001031
018000001031     D                 DS
018100001031     D  v2orvo                 1      2
018200001031     D  v2orvm                 3      4
018300001031     D  v2orvs                 5      6
018400001031     D  v2orv                  1      6  0
018500001031
018600001031     D                 DS
018700001031     D  v4dtvg                 1      2
018800001031     D  v4dtvm                 3      4
018900001031     D  v4dtva                 5      8
019000001031     D  v4dtv                  1      8  0
019100001031
019200001031     D                 DS
019300001031     D  v4orvo                 1      2
019400001031     D  v4orvm                 3      4
019500001031     D  v4orvs                 5      6
019600001031     D  v4orv                  1      6  0
019700061017
019800061017     d                 ds
019900061017     d  ds_giorno              1      2  0
020000061017     d  ds_mese                3      4  0
020100061017     d  ds_anno                5      8  0
020200061017     d  ds_data                1      8  0
020300001009
020400001009     D Parm01          DS
020500001016     D  ppoe                          3  0
020600001016     D  pnor                          7  0
020700001016     D  pnsr                          2  0
020800001016     D  pnrv                          2  0
020900001016     D  psce                          1
021000001018     D  pfgs                          3  0
021100010205     D  ppor                          3  0
021200010205     D  pdtr                          8  0
021300010205     D  pmdb                         10
021400010205     D  pprb                         10
021500010205     D  pdts                          8  0
021600010205     D  prmp                          1
021700010205     D  pbrc                          1
021800010205     D  pref                          2
021900010205     D  pmio                          1
022000010205      * Da int.proposte
022100010109     D  pdtv                          8  0
022200010109     D  porv                          6  0
022300010109     D  putv                         10
022400010109     D  pric                          1
022500040220      *
022600040220      *?definizione oggetto reclamo come O.R.M.?
022700040220     d ds_OggORM       ds
022800040220     d  ORMpoe
022900040220     d  ORMnsr
023000040220     d  ORMnor
023100040220     d  ORMnrv
023200180222
023300180222       // -?Parametri per ricerca elenco programmi richiamati
023400180222     d dsStack         ds
023500180222     d  skStack                      10    Dim(100)
023600180222
023700180222      // - Campi per procedura esterna elenco programmi richiamati
023800180222     d  pStack         s           1000
023900180222     d  pStartPoint    s              3  0
024000180222     d  pEntries       s              3  0
024100180222
024200040908     d dFar          e ds
024300010301     D DCMR          E DS
024400001010     D TIBS02DS      E DS
024500090202     d ds5P          e ds                  inz
024600090202     d   §5Ppgm      e                     inz('FIOR18R   ')
024700001030     D FIOR06DS      E DS
024800061017     d fior16ds      e ds                  inz
024900030606     d Dorm01        e ds
025000081125     d dorf01        e ds
025100060515     d fnlv55ds      e ds
025200061003     d og143         e ds
025300061017     d trul90ds      e ds
025400061019     d fieu55ds      e ds
025500070911     d fidg09ds      e ds
025600171025     d tisid4ds      e ds                  inz
025700171025     d truldepds     e ds                  inz
025800171025     d truldepds1    e ds                  inz
025900090211     d ds3idp        e ds
026000140206     d TISI95DS      e ds
026100151001     d dUTE01        e ds
026200001010
026300001010      *
026400001010     D KPJBA         E DS
026500040220     D FIDNA1ds      e ds
026600040226      *
026700040226     d FIDNA2ds      e ds                  inz
026800040226     d  IA2mod       e                     inz('M')
026900040226     d  IA2tor       e                     inz('O')
027000130913
027100130913      * - Parametri x Interrogazione orari servizio
027200140317     d TRULORSds     e ds
027300090202
027400090202      * - Parametri x Controllo profilo utenti
027500090202     d TIBS34ds      e ds
027600090202      * - Ds di riferimento al file esterno AZUTE00F
027700090202     d AzuteDs       e ds                  extname(AZUTE00F)
027800090202      * - Ds per dati organigramma
027900090202     d dDatiUte      e ds
028000140110
028100140110      // ds per note AUT
028200140110     d FIORT1DS      e ds
028300131028
028400131028      // ds per rcd 'O' FNORE - orari apertura
028500131028     d dOREorari     e ds
028600140429
028700140429      // ds per rcd 'G' FNORE - generale
028800140429     d dOREgen       e ds
028900140429
029000140429      // ds per rcd 'S' FNORE - SMS
029100140429     d dOREsms       e ds
029200161108
029300161108      // ds per rcd 'DT' FNORE - Data Pronta Merce
029400161108     d dOREdt        e ds
029500161115
029600161115      // ds per campo IOREflo - TRULORSDS
029700161115     d dIORE01       e ds
029800140318
029900140318      // ds per recupero ora presunta ritiro da PDA
030000140318     d FIOR01DS      e ds
030100151001
030200151001      // Carico filiali gestite dall'utente
030300151001     d TRUL31DS      e ds
030400180222
030500180222      // Interrogazione bolle unica di filiale
030600180222     d FNLRU6DS      e ds
030700090202
030800001010      *
030900001010     D                SDS
031000040220     d  SDSprm           *parms
031100001010     D  VTCPGM                 1     10
031200090210
031300090210      *   C O S T A N T I
031400090210     d cf2413          c                   const('F24=AlTasti(1/3)')
031500090210     d cf2423          c                   const('F24=AlTasti(2/3)')
031600090210     d cf2433          c                   const('F24=AlTasti(3/3)')
031700090210     d cf2412          c                   const('F24=AlTasti(1/2)')
031800090210     d cf2422          c                   const('F24=AlTasti(2/2)')
031900140110
032000130913      //---------------------------------------------------------------
032100130913      //?Definizione procedure utilizzate.
032200130913      //---------------------------------------------------------------
032300140110
032400140110      // - Gestione note AUT
032500140110     d fiort1r         pr                  extpgm('FIORT1R')
032600140110     d  kpjba                              likeds(KPJBA)
032700140110     d  fiort1ds                           likeds(FIORT1DS)
032800140320
032900140320      // - Gestione note AUT x proposte
033000140320     d fiort1c         pr                  extpgm('FIORT1C')
033100140320     d  kpjba                              likeds(KPJBA)
033200140320     d  fiort1ds                           likeds(FIORT1DS)
033300130913
033400130913      // -Interrogazione orari di servizio
033500140317     d trulorsr        pr                  extpgm('TRULORSR')
033600130913     d  kpjba                              likeds(KPJBA)
033700140317     d  trulorsds                          likeds(TRULORSDS)
033800140206
033900140206      // - Cappario
034000140206     d tisi95r         pr                  extpgm('TISI95R')
034100140206     d  tisi95ds                           likeds(TISI95DS)
034200140318
034300140318      // - Recupero ora presunta ritiro
034400140318     d fior01r         pr                  extpgm('FIOR01R')
034500140318     d  fior01ds                           likeds(FIOR01DS)
034600131125
034700180222       // - pgm Int.Bolle Unica di filiale
034800180222     d FNLRU6R         pr                  extpgm('FNLRU6R')
034900180222     d  kpjba                              likeds(KPJBA)
035000180222     d  fnlru6ds                           likeds(FNLRU6DS)
035100180222
035200180222       // - Pgm per ricerca elenco Programmi richiamati
035300180222     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
035400180222     d  pStack                     1000
035500180222     d  pStartPoint                   3  0
035600180222     d  pEntries                      3  0
035700180222
035800131125      //---------------------------------------------------------------
035900131125      //?Definizione prototipi utilizzati.
036000131125      //---------------------------------------------------------------
036100131125
036200131125      /copy gaitrasrc/srcprotopr,tibs02r
036300161020      /copy gaitrasrc/srcprotopr,XSRDA8
036400131028
036500131028      //---------------------------------------------------------------
036600131028      //?Definizione key-list.
036700131028      //---------------------------------------------------------------
036800131028
036900131028       // - File FNORE01L
037000131029     d k05fnore      e ds                  extname(FNORE01L:*key)
037100131028     d                                     prefix(k_)
037200131028     d                                     inz
037300131029
037400131029       // - File FNORVE1L
037500131029     d k07fnorve     e ds                  extname(FNORVE1L:*key)
037600131029     d                                     prefix(k_)
037700131029     d                                     inz
037800131029
037900131029       // - File FNORPE1L
038000131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
038100131029     d                                     prefix(k_)
038200131029     d                                     inz
038300001010
038400001006      *------------------------------------------------------------------------*
038500001103
038600001030      * Carica i dati dal file FNORM
038700001030     C     kfnor         chain     fnorm01L
038800001115     C                   if        %found(fnorm01l)
038900001115      * Annullato
039000001115     C                   if        ORMatb <> *blanks
039100001115     C                   movel     'ANNULLATO'   v1ann
039200001115     C                   else
039300001115     C                   clear                   v1ann
039400001115     C                   endif
039500001103      * Se è richiamato con 'P' deve solo gestire le proposte
039600001103     C                   if        psce = 'P'
039700001103     C                   goto      richiama
039800001103     C                   endif
039900010315
040000010109     C                   else
040100010109     C                   if        psce = 'P' and pric = 'P'
040200010109     C                   movel     'E'           psce
040300010109     C                   movel     parm01        kpjbu
040400010109     C                   goto      richiama
040500010109     C                   endif
040600001009     C                   endif
040700040220
040800040220      * Abilito eventuali tasti funzionali
040900140304     c                   exsr      Abil_Fxx
041000001006
041100001006      * Emissione 1° Videata
041200001010
041300001006     C                   do        *hival
041400120612     c                   eval      *in01 = *off
041500140303     c                   eval      *in15 = *off
041600010315     C                   exsr      Sr_Carvid
041700010315     C                   exsr      Sr_Decodifica
041800090211     c                   ExSr      Sr_Tastifun
041900001010
042000061017     c     for01         tag
042100001031     C                   eval      v1mod = 'VISUALIZZAZIONE'
042200001031
042300001103     C                   setoff                                       010203
042400140304     c                   setoff                                       1715
042500040908     c                   Eval      *In10 = *Off
042600001030     C                   exfmt     fior071
042700001031     C                   setoff                                       28
042800001011
042900001011      * F12=Ritorno
043000001023     C                   if        *inkl = *on
043100001023     C                   leave
043200001023     C                   endif
043300040220
043400040220      *
043500040220      *?F1 - Aggiunta bolla in reclamo?
043600040226if  1c                   if        *inKA  = *on
043700040220     c                   clear                   FIDNA1ds
043800040220     c                   eval      IA1ins = 'S'
043900040220     c                   eval      IA1tor = 'O'
044000040220     c                   eval      IA1ogg = ds_OggORM
044100040226     c                   leave
044200040220e   1c                   endif
044300040226      *
044400040226      *?F16 - Gestione reclami?
044500040226if  2c                   if        *inKQ  = *on
044600040226     c                   reset                   FIDNA2ds
044700040226if  3c                   if        SDSprm > 1
044800040226     c                   eval      IA2mod = 'I'
044900040226e   3c                   endif
045000040226     c                   eval      IA2ogg = ds_OggORM
045100040226     c                   call      'FIDNA2R'
045200040226     c                   parm                    KPJBA
045300040226     c                   parm                    FIDNA2ds
045400040226if  3c                   if        OA2f03 = 'S'
045500040226     c                   leave
045600040226e   3c                   endif
045700040226e   2c                   endif
045800130913
045900180222       // F8=Int.Bolle
046000180222       IF  *inkh;
046100180222         exsr F08D01;
046200180222         iter;
046300180222       ENDIF;
046400180222
046500130913       //?F14=Interrogazione oraro di servizio
046600130913       IF  *inkn;
046700130913         exsr F14D01;
046800130913         iter;
046900130913       ENDIF;
047000131028
047100131028       //?F5=Altri dati
047200131028       IF  *inke;
047300131028         exsr F05D01;
047400131028         iter;
047500131028       ENDIF;
047600140110
047700140110       //?F17=Note AUT
047800140110       IF  *inkr;
047900140110         exsr F17D01;
048000140110         iter;
048100140110       ENDIF;
048200130913
048300001030
048400010402      * F20=Fasi
048500010402     C                   if        *inku = *on
048600001031     C                   exsr      Sr_Fasi
048700001030     C                   endif
048800120612
048900131120      * F15=Variazioni
049000131120     C                   if        *inkp = *on
049100001102     C                   exsr      Sr_Variazioni
049200001030     C                   endif
049300001011
049400001025      * F18=Note
049500001025     C                   if        *inks = *on
049600001025     C                   exsr      Sr_Note
049700001025     C                   endif
049800001020      * F19=Proposte
049900001030     C                   if        *inkt = *on
050000001031     C                   exsr      Sr_Proposte
050100001020     C                   endif
050200061017      * F9=Sollecito
050300061017     c                   if        *inki
050400061017     c                   exsr      sr_sollecito
050500061017     c                   endif
050600061023      * F2=Dati DPD
050700061023     c                   if        *inkb
050800061019     c                   exsr      sr_datidpd
050900061019     c                   endif
051000090211      * F22=Info appuntamento
051100090211     c                   if        *inkw
051200090211     c                   exsr      sr_ormapp
051300090211     c                   endif
051400090210      * F24=Altri tasti  ?
051500090210     c                   if        *inky and $loop > 1
051600090210     c                   exsr      sr_f24
051700090210     c                   goto      for01
051800090210     c                   endif
051900001010
052000001006     C                   enddo
052100001023
052200001103     C     richiama      tag
052300001103
052400010109     C                   if        psce = 'P'
052500010109     C                   if        pric = *blanks
052600001103     C                   exsr      Sr_Proposte
052700010109     C                   endif
052800010109
052900010109     C                   if        pric = 'P'
053000010109     C                   exsr      Sr_Cardatiprop
053100010109     C                   exsr      Sr_Visproposte
053200010109     C                   endif
053300001103     C                   endif
053400001010
053500001006     C                   eval      *inlr = *on
053600001009      **********************************************************************
053700001023      * CARICA DATI NELLA VIDEATA DAL FILE
053800001009      **********************************************************************
053900001009     C     Sr_Carvid     begsr
054000001009
054100001129     C                   movel     ORMtco        v1ctco
054200001016     C                   movel     ORMtor        v1ctor
054300001023      * Inversione data emissione ORM
054400001016     C                   clear                   wlbdat
054500001017     C                   z-add     ORMdao        G02inv
054600001017     C                   movel     '3'           G02err
054700001016     C                   call      'XSRDA8'
054800001016     C                   parm                    wlbdat
054900001025     C                   z-add     G02dat        v1dao
055000001215     C                   movel     ORMoao        v1oao
055100001016      * Dati Ordinante
055200001025     C                   z-add     ORMcor        v1ccor
055300001025     C                   movel     ORMrso        v1rso
055400001025     C                   movel     ORMino        v1ino
055500001025     C                   movel     ORMcao        v1cao
055600001025     C                   movel     ORMloo        v1loo
055700001025     C                   movel     ORMpro        v1pro
055800001025     C                   movel     ORMnao        v1nao
055900001016      * Dati Ritiro a
056000001025     C                   z-add     ORMcra        v1ccra
056100001025     C                   movel     ORMrsr        v1rsr
056200001025     C                   movel     ORMinr        v1inr
056300001025     C                   movel     ORMcar        v1car
056400001025     C                   movel     ORMlor        v1lor
056500001025     C                   movel     ORMprr        v1prr
056600001025     C                   movel     ORMnar        v1nar
056700001016     C                   movel     ORMrer        v1rer
056800001016     C                   movel     ORMter        v1ter
056900001016      * Dati Consegna a
057000001025     C                   z-add     ORMcrc        v1ccrc
057100001016     C                   movel     ORMrsc        v1rsc
057200001016     C                   movel     ORMinc        v1inc
057300001016     C                   movel     ORMloc        v1loc
057400001016     C                   movel     ORMcac        v1cac
057500001016     C                   movel     ORMprc        v1prc
057600001016     C                   movel     ORMnac        v1nac
057700001016
057800010315     C                   if        ORMffd = 'S'
057900010315     C                   eval      v1ffd = 'SI'
058000010315     C                   else
058100010315     C                   eval      v1ffd = 'NO'
058200010315     C                   endif
058300010315
058400001016     C                   movel     ORMnam        v1nam
058500001023      * Inversione data ritiro
058600001017     C                   z-add     ORMdar        G02inv
058700001017     C                   movel     '3'           G02err
058800001016     C                   call      'XSRDA8'
058900001016     C                   parm                    wlbdat
059000001025     C                   z-add     G02dat        v1dar
059100001016     C                   movel     ORMorr        v1orr
059200001016
059300001025     C                   z-add     ORMncl        v1ncl
059400001025     C                   z-add     ORMpkg        v1pkg
059500001025     C                   z-add     ORMvlm        v1vlm
059600001025     C                   z-add     ORMbnc        v1bnc
059700001025     C                   z-add     ORMblc        v1blc
059800001025     C                   z-add     ORMatt        v1att
059900001025     C                   z-add     ORMmtc        v1mtc
060000001016
060100010315     C                   if        ORMpag = 'M'
060200010315     C                   eval      v1pag = 'Mitt.'
060300010315     C                   endif
060400010315     C                   if        ORMpag = 'D'
060500010315     C                   eval      v1pag = 'Dest.'
060600010315     C                   endif
060700010315     C                   if        ORMpag = 'O'
060800010315     C                   eval      v1pag = 'Ord. '
060900010315     C                   endif
061000010315
061100001025     C                   z-add     ORMksc        v1ksc
061200001020     C                   movel     ORMctr        v1ctr
061300010315
061400010315     C                   if        ORMddt = 'S'
061500010315     C                   eval      v1ddt = 'SI'
061600010315     C                   endif
061700010315     C                   if        ORMddt = 'N'
061800010315     C                   eval      v1ddt = 'NO'
061900010315     C                   endif
062000010315
062100001016     C                   movel     ORMpor        v1cpor
062200001016     C                   movel     ORMpoc        v1cpoc
062300001016     C                   movel     ORMno1        v1not1
062400001016     C                   movel     ORMno2        v1not2
062500001025     C                   z-add     ORMfao        v1fao
062600001025     C                   z-add     ORMpos        v1cpos
062700001025     C                   z-add     ORMors        v1ors
062800001027     C                   movel     ORMrfa        v1rfa
062900001221     C                   move      ORMsto        v1sto
063000001030     C                   z-add     ORMndc        v1ndc
063100001030      * Inversione data distinta
063200001030     C                   clear                   wlbdat
063300001030     C                   z-add     ORMddc        G02inv
063400001030     C                   movel     '3'           G02err
063500001030     C                   call      'XSRDA8'
063600001030     C                   parm                    wlbdat
063700001030     C                   z-add     G02dat        v1ddc
063800010226
063900010226      * Cerca codice autotrasportatore
064000071108     c                   if        ormndc > 0
064100001030     C                   z-add     ORMndc        knfv
064200001030     C                   z-add     ORMpor        kfgs
064300120612     C     kfidst        chain     fidst01l
064400060515      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
064500120612     c                   if        not %found(fidst01l)
064600060515     c                   clear                   fnlv55ds
064700060515     c                   eval      d55tpt = '6'
064800060515     c                   move      ormpor        d55lin
064900060515     c                   eval      d55drf = ormddc
065000060515     c                   call      'FNLV55R'
065100060515     c                   parm                    fnlv55ds
065200060515     c                   if        d55err = *blanks and d55tfa <> d55lin
065300060515     c                   move      d55tfa        kfgs
065400120612     c     kfidst        chain     fidst01l
065500060515     c                   endif
065600060515     c                   endif
065700120612     C                   if        %found(fidst01l)
065800001030     C                   eval      v1cpdc = DSTpdr
065900001030     C                   endif
066000071108     c                   else
066100071108     c                   clear                   v1cpdc
066200071108     c                   endif
066300071108
066400010226      * Autotrasportatore evidenziato a video
066500010226     C                   eval      *in06 = (v1cpdc > 0)
066600010226
066700010219      * Distinta evidenziata a video
066800010219     C                   eval      *in05 = (v1ndc > 0)
066900070924
067000070924      * se Stampato faccio vedere la scritta
067100070924     c                   eval      v1dst = ormdst
067200070924     c                   eval      *in16 = (v1dst > 0)
067300010119
067400010315     C                   if        ORMspi = 'S'
067500010315     C                   eval      v1spi = 'SI'
067600010315     C                   else
067700010315     C                   eval      v1spi = 'NO'
067800010315     C                   endif
067900010309      * Causale
068000010305     C                   exsr      Sr_Carcau
068100010322
068200010322     C                   eval      *in40 = *off
068300010322     C                   eval      *in41 = *off
068400010322     C                   eval      *in42 = *off
068500090211     C                   eval      *in43 = *off
068600131211     C                   eval      *in46 = *off
068700140121     C                   eval      *in48 = *off
068800131211
068900010322      * Controllo se ci sono Variazioni
069000010322     C     kfnor         setll     fnorv01l
069100010322     C                   do        *hival
069200010322     C     kfnor         reade     fnorv01l
069300010322     C                   if        %eof
069400010322     C                   leave
069500010322     C                   endif
069600010322     C                   eval      *in40 = *on
069700010322     C                   leave
069800010322     C                   enddo
069900010322      * Controllo se ci sono Proposte
070000010322     C     kfnor         setll     fnorp01l
070100010322     C                   do        *hival
070200010322     C     kfnor         reade     fnorp01l
070300010322     C                   if        %eof
070400010322     C                   leave
070500010322     C                   endif
070600010322     C                   eval      *in41 = *on
070700010322     C                   leave
070800010322     C                   enddo
070900010322      * Controllo se ci sono Note
071000061017     C     kfnor         setll     fnorn02l
071100010322     C                   do        *hival
071200061017     C     kfnor         reade     fnorn02l
071300010322     C                   if        %eof
071400010322     C                   leave
071500010322     C                   endif
071600010322     C                   eval      *in42 = *on
071700010322     C                   leave
071800010322     C                   enddo
071900090211
072000090211      * controllo se inserite info appuntamento
072100090211     c     kfnor         setll     fnora01l
072200090211     c                   do        *hival
072300090211     c     kfnor         reade     fnora01l
072400090211     c                   if        %eof
072500090211     c                   leave
072600090211     c                   endif
072700090211     c                   eval      *in43 = *on
072800090211     c                   leave
072900090211     c                   enddo
073000030606
073100030606     C                   eval      dorm01 = ORMflo
073200030606
073300030606      * ORM commissionato
073400030606     c                   If        §orCom = 'S'
073500030606     c                   Eval      V1dCom = 'COMMISSIONATO'
073600030606     c                   Else
073700030606     c                   Clear                   V1dCom
073800030606     c                   EndIf
073900050502
074000050502      * ORM con Ricevuta di Ritiro
074100050502     c                   Eval      *In11 = (§orsrm = 'S')
074200100225
074300100225      /free
074400140429       //?Imposto costante se ORM con preavviso (CEVA)
074500100225       *in18 = *off;
074600100225       IF  §ormpre <> *blanks;
074700100225         *in18 = *on;
074800100225         IF  §ormpre = 'M';
074900100225         v1este = 'Preavviso: '+
075000100225                  'Mail';
075100100225         ENDIF;
075200100225       ENDIF;
075300100225      /end-free
075400061017
075500061017     c                   eval      v1cfcs = ormfcs
075600070911      * giro
075700070911     c                   clear                   v1ccgi
075800070911     c     kfnor         chain     fnorg01l
075900070911     c                   if        %found(fnorg01l)
076000070911     c                   eval      v1ccgi = orgcgi
076100070911     c                   endif
076200131028
076300131028      /free
076400131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
076500131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
076600131211           *in46 = *on;
076700131211         ENDIF;
076800160513
076900160513         k_OREpoe = ORMpoe;
077000160513         k_OREnsr = ORMnsr;
077100160513         k_OREnor = ORMnor;
077200160513         k_OREnrv = ORMnrv;
077300160513
077400160513       //?Recupero dati da estensione FNORE
077500160513       //?Rcd 'G ' --> Generale
077600160513         k_OREtrc = 'G';
077700160513         clear dOREgen;
077800160513         chain  %kds(k05fnore) FNORE01L;
077900160513         IF  %found(FNORE01L);
078000160513           dOREgen = OREdati;
078100160513         ENDIF;
078200160513
078300131028       //?Recupero dati da estensione FNORE
078400131028       //?Rcd 'O ' --> Orari apertura
078500131028         k_OREtrc = 'O ';
078600131028         clear dOREorari;
078700131028       //?Aggancio il rcd
078800140429         chain  %kds(k05fnore) FNORE01L;
078900131028         IF  %found(FNORE01L);
079000131028           dOREorari = OREdati;
079100131028         ENDIF;
079200131028         v1OraAMda = §OREoramda;
079300131028         v1OraAMa  = §OREorama;
079400131028         v1OraPMda = §OREorapda;
079500131028         v1OraPMa  = §OREorapa;
079600161020
079700161020       //?Rcd 'DT' --> Data Pronta Merce
079800161020         k_OREtrc = 'DT';
079900161108         clear dOREdt;
080000161020         chain  %kds(k05fnore) FNORE01L;
080100161020         IF  %found(FNORE01L);
080200161108           dOREdt = OREdati;
080300161020         ENDIF;
080400161108         IF  §OREdpm > *zeros;
080500161020           clear wlbdat;
080600161108           G02inv = §OREdpm;
080700161020           G02err = '3';
080800161020           xsrda8 (wlbdat);
080900161020           V1dpm = G02dat;
081000161020         ENDIF;
081100140429
081200140429       //?Imposto costante se ORM con Alert via Mail/SMS
081300160513         IF  §OREfimo = 'S' or §OREfiso = 'S';
081400160513           IF  not *in18;
081500160513             V1este = 'Preavviso: Mail/SMS';
081600160513             *in18 = *on;
081700160513           ENDIF;
081800160513         ENDIF;
081900160224
082000160224       //?Recupero NPR
082100160224         k_OREtrc = 'NP';
082200160224         clear OREdati;
082300160224       //?Aggancio il rcd
082400160224         chain  %kds(k05fnore) FNORE01L;
082500160224         IF  %found(FNORE01L);
082600160224           V1npr = OREdati;
082700160224         ENDIF;
082800140121
082900140121       //?Controllo se ci sono Note AUT
083000140121         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
083100140121         IF  %found(FNORT01L);
083200140121           *in48 = *on;
083300140121         ENDIF;
083400131211
083500131028      /end-free
083600001009
083700001009     C                   endsr
083800001030      **********************************************************************
083900001030      * DECODIFICHE
084000001030      **********************************************************************
084100001030     C     Sr_Decodifica begsr
084200001030
084300001030      * Fase
084400001129     C                   clear                   v1dfao
084500001030     C                   clear                   TIBS02DS
084600001030     C                   movel     'C'           T02mod
084700001030     C                   movel     knsif         t02sif
084800001030     C                   movel     'FAR'         t02cod
084900001030     C                   movel(p)  v1fao         T02ke1
085000001030     C                   call      'TIBS02R'
085100001030     C                   parm                    KPJBA
085200001030     C                   parm                    TIBS02DS
085300040908     c                   Eval      dFar = T02Uni
085400040908     c                   Eval      V1dfao = d§fardes
085500001030
085600001030      * Tipo ordine assegnazione
085700001129     C                   clear                   v1dtco
085800001030     C                   clear                   TIBS02DS
085900001030     C                   movel     'C'           T02mod
086000001030     C                   movel     knsif         t02sif
086100001129     C                   movel     'TCO'         t02cod
086200001129     C                   movel(p)  v1ctco        T02ke1
086300001030     C                   call      'TIBS02R'
086400001030     C                   parm                    KPJBA
086500001030     C                   parm                    TIBS02DS
086600001129     C                   movel     t02uni        v1dtco
086700001030
086800001030      * P.O. ritiro
086900061003     c                   eval      *in22 = *off
087000061003     c                   clear                   v1dfcs
087100061003     c                   clear                   og143
087200001129     C                   clear                   v1dpor
087300140206     c                   clear                   network
087400001030     C                   movel     v1cpor        kazorg
087500001030     C     kazorg        chain     azorg01L
087600001030     C                   if        %found(azorg01l)
087700001030     C                   movel     ORGdes        v1dpor
087800140206     C                   movel     orgdit        network
087900061003     c                   eval      og143 = orgde3
088000001030     C                   endif
088100061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
088200061003      * europolitan invece ormai è obsoleta
088300061003     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
088400061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
088500061003      * partener se caricata e la visualizzo al posto della zona
088600061017     c                   if        v1cfcs <> *blanks
088700061003     c                   clear                   tibs02ds
088800061003     c                   eval      t02mod = 'C'
088900061003     c                   eval      t02sif = knsif
089000061003     c                   eval      t02cod = 'MIP'
089100061017     c                   eval      t02ke1 = v1cfcs
089200061003     c                   call      'TIBS02R'
089300061003     c                   parm                    kpjba
089400061003     c                   parm                    tibs02ds
089500061003     c                   eval      v1dfcs = t02uni
089600061003     c                   eval      *in22 = *on
089700061003     c                   endif
089800061003     c                   endif
089900070911
090000070911      * Giro
090100070911     c                   clear                   v1dcgi
090200070911     c                   clear                   fidg09ds
090300070911     c                   eval      d09iop0 = '001'
090400071127     c                   eval      d09ifgs = v1cpor
090500070911     c                   eval      d09idat = *date
090600070911     c                   eval      d09icgi = v1ccgi
090700070911     c                   eval      d09itug = 'R'
090800080318     c                   movel     kpjbu         §kpjbu
090900070911     c                   eval      kpjbu = fidg09ds
091000070911     c                   call      'FIDG09R'
091100070911     c                   parm                    kpjba
091200070911     c                   eval      fidg09ds = kpjbu
091300080318     c                   movel     §kpjbu        kpjbu
091400070911     c                   if        d09ocgi <> ' '
091500070911     c                   eval      v1dcgi = d09odes
091600070911     c                   endif
091700001030
091800001030      * P.O. Consegna
091900001129     C                   clear                   v1dpoc
092000001030     C                   movel     v1cpoc        kazorg
092100001030     C     kazorg        chain     azorg01L
092200001030     C                   if        %found(azorg01l)
092300001030     C                   movel     ORGdes        v1dpoc
092400001030     C                   endif
092500001030
092600001030      * Tipo ordine
092700001129     C                   clear                   v1dtor
092800001030     C                   clear                   TIBS02DS
092900001030     C                   movel     'C'           T02mod
093000001030     C                   movel     knsif         t02sif
093100001030     C                   movel     'TOR'         t02cod
093200001030     C                   movel(p)  v1ctor        T02ke1
093300001030     C                   call      'TIBS02R'
093400001030     C                   parm                    KPJBA
093500001030     C                   parm                    TIBS02DS
093600001030     C                   movel     t02uni        v1dtor
093700001103
093800010205      * Priorità
093900010205     C                   clear                   v1dsto
094000010205     C                   if        v1sto <> *zeros
094100001103     C                   clear                   TIBS02DS
094200001103     C                   movel     'C'           T02mod
094300001103     C                   movel     knsif         t02sif
094400001103     C                   movel     'STO'         t02cod
094500131029     C                   eval      t02ke1 = %editc(ORMsto:'X')
094600001103     C                   call      'TIBS02R'
094700001103     C                   parm                    KPJBA
094800001103     C                   parm                    TIBS02DS
094900010205     C                   movel     t02uni        v1dsto
095000010205     C                   endif
095100001030
095200010226      * Autotrasportatore
095300001129     C                   clear                   v1dpdc
095400021202     C                   movel     v1cpdc        kpdpdr
095500021202     C     kfapd         chain     fiapd01L
095600021202     C                   if        %found(fiapd01l)
095700001030     C                   movel     APDrsc        v1dpdc
095800001030     C                   endif
095900001031      * ORM fisso
096000001031     C                   eval      *in04 = (v1ors <> *zeros)
096100161108
096200161108      /free
096300161108       //?Se non ho la data pronta merce imposto la data ritiro
096400161108         IF  V1dpm = 0;
096500161108           V1dpm = V1dar;
096600161108         ENDIF;
096700161108      /end-free
096800001030
096900001030     C                   endsr
097000010305      **********************************************************************
097100010305      * Cerca causale chiusura e decodifica
097200010305      **********************************************************************
097300010305     C     Sr_Carcau     begsr
097400010305
097500010305     C                   eval      kpoe = ORMpoe
097600010305     C                   eval      knsr = ORMnsr
097700010305     C                   eval      knor = ORMnor
097800010305     C                   eval      knrv = ORMnrv
097900010305     C                   eval      kdfo = ORMdfo
098000010305     C                   eval      kofo = ORMofo
098100010305     C                   eval      kfao = ORMfao
098200110418     C**!!!kfnorf        chain     fnorf01L
098300110418     C**!!!              if        %found(fnorf01l)
098400110418     c     kfnorf        setll     fnorf01l
098500110418     c                   do        *hival
098600110418     c     kfnorf        reade     fnorf01l
098700110418     c                   if        %eof(fnorf01l)
098800110418     c                   leave
098900110418     c                   endif
099000010305     C                   eval      v1cau = orfcar
099100110418     C                   enddo
099200010305
099300010305     C                   clear                   dcmr
099400010305     C                   clear                   TIBS02DS
099500010305     C                   movel     'C'           T02mod
099600010305     C                   movel     knsif         t02sif
099700010305     C                   movel     'CMR'         t02cod
099800010305     C                   movel(p)  v1cau         T02ke1
099900010305     C                   call      'TIBS02R'
100000010305     C                   parm                    KPJBA
100100010305     C                   parm                    TIBS02DS
100200010305     C                   movel     t02uni        dcmr
100300010305     C                   movel     d§cmrdes      v1dcau
100400010309
100500010309     C                   eval      *in07 = (v1cau <> *blanks)
100600010305
100700010305     C                   endsr
100800001031      **********************************************************************
100900081125      * F20 - Fasi
101000001031      **********************************************************************
101100001031     C     Sr_Fasi       BEGSR
101200001031
101300001031     C                   z-add     1             recsff
101400001031      * Pulisco subfile
101500001031     C                   z-add     *zeros        nrrf              4 0
101600001031     C                   eval      *in21 = *off
101700001031     C                   write     fior07cf
101800001031     C                   eval      *in20 = *on
101900001031     C                   eval      *in21 = *on
102000001031
102100001031      * Carico il subfile in base alle scelte fatte
102200001031     C     kfnor         setll     fnorf01l
102300001031     C                   do        *hival
102400001031     C     kfnor         reade     fnorf01l
102500001031     C                   if        %eof
102600001031     C                   leave
102700001031     C                   endif
102800001031
102900001031     C                   z-add     v1cpoe        v3cpoe
103000001031     C                   z-add     v1nsr         v3nsr
103100001031     C                   z-add     v1nor         v3nor
103200001031     C                   z-add     v1nrv         v3nrv
103300081125
103400081125     c                   eval      dorf01 = orfflo
103500001031
103600001031     C                   exsr      Sr_CarFasi
103700001031     C                   enddo
103800001031      * Se non ho caricato emetto una videata di segnalazione.
103900001031     C                   if        nrrf = *zeros
104000001031     C                   movel     msg(2)        v1cmsg
104100001031     C                   seton                                        28
104200001031     C                   goto      endfasi
104300001031     C                   endif
104400001031
104500001031      * Se ho caricato emetto il subfile
104600001031     C                   if        nrrf > *zeros
104700001102     C                   do        *hival
104800001031     C                   write     fior073
104900001031     C                   exfmt     fior07cf
105000001031      * F12=Ritorno
105100001102     C   kl              leave
105200001102     C                   enddo
105300001102     C                   endif
105400001031
105500001031     C     endfasi       endsr
105600001031      **********************************************************************
105700001031      * CARICO SUBFILE FASI
105800001031      **********************************************************************
105900001031     C     Sr_CarFasi    begsr
106000001031
106100001031      * Inversione data modifica
106200001031     C                   z-add     ORFdae        G02inv
106300001031     C                   movel     '3'           G02err
106400001031     C                   call      'XSRDA8'
106500001031     C                   parm                    wlbdat
106600001031     C                   movel     G02dat        v3dae
106700001031
106800001031     C                   movel     ORFore        v3ore
106900001031     C                   eval      v3cpog = ORFpog
107000001031     C                   eval      v3ute  = ORFpue
107100001031     C                   eval      v3far  = ORFfar
107200001031      * Decodifica fase
107300001031     C                   clear                   TIBS02DS
107400001031     C                   movel     'C'           T02mod
107500001031     C                   movel     knsif         t02sif
107600001031     C                   movel     'FAR'         t02cod
107700001031     C                   movel(p)  v3far         T02ke1
107800001031     C                   call      'TIBS02R'
107900001031     C                   parm                    KPJBA
108000001031     C                   parm                    TIBS02DS
108100040908     C**!!!              movel     t02uni        v3dfar
108200040908     c                   Eval      dFar = T02Uni
108300040908     c                   Eval      V3dfar = d§fardes
108400001031
108500001031     C                   if        ORFcar <> *blanks
108600001031     C                   seton                                        02
108700001031     C                   eval      v3car  = ORFcar
108800001031      * Decodifica causale
108900010301     C                   clear                   dcmr
109000001031     C                   clear                   TIBS02DS
109100001031     C                   movel     'C'           T02mod
109200001031     C                   movel     knsif         t02sif
109300001128     C                   movel     'CMR'         t02cod
109400001031     C                   movel(p)  v3car         T02ke1
109500001031     C                   call      'TIBS02R'
109600001031     C                   parm                    KPJBA
109700001031     C                   parm                    TIBS02DS
109800010301     C                   movel     t02uni        dcmr
109900010301     C                   movel     d§cmrdes      v3dcar
110000001107     C                   else
110100001107     C                   setoff                                       02
110200001107     C                   clear                   v3car
110300001107     C                   clear                   v3dcar
110400001031     C                   endif
110500081125      * visualizzo se 'No addebito'
110600081125     c                   eval      *in17 = *off
110700081125     c                   if        §orfadd = 'S'
110800081125     c                   eval      *in17 = *on
110900081125     c                   endif
111000040908
111100040908      * Se ci sono carico anche i dati della distinta
111200040908if  1c                   If        OrfNdc > *Zeros
111300040908     c                   Eval      *In10 = *On
111400040908     c                   Eval      v3ndc = OrfNdc
111500040908      * giro la data della distinta
111600040908     c                   Clear                   wlbdat
111700040908     c                   Z-add     OrfDdc        G02inv
111800040908     c                   Movel     '3'           G02err
111900040908     c                   Call      'XSRDA8'
112000040908     c                   Parm                    wlbdat
112100040908     c                   Z-add     G02dat        v3ddc
112200040915
112300040908      * cerco il codice dell'autotrasportatore
112400040908     c                   Z-add     OrfNdc        knfv
112500040908     c                   Z-add     OrfFgs        kfgs
112600120612     c     kfidst        Chain     fidst01l
112700060522      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
112800120612     c                   if        not %found(fidst01l)
112900060522     c                   clear                   fnlv55ds
113000060522     c                   eval      d55tpt = '6'
113100060522     c                   move      orffgs        d55lin
113200060522     c                   eval      d55drf = ormddc
113300060522     c                   call      'FNLV55R'
113400060522     c                   parm                    fnlv55ds
113500060522     c                   if        d55err = *blanks and d55tfa <> d55lin
113600060522     c                   move      d55tfa        kfgs
113700120612     c     kfidst        chain     fidst01l
113800060522     c                   endif
113900060522     c                   endif
114000120612     c                   If        %Found(fidst01l)
114100040908     c                   Eval      v3cpdr = DstPdr
114200040908     c                   EndIf
114300040908      * decodifico l'autotrasportatore
114400040908     c                   Clear                   v3dpdr
114500040908     c                   Movel     v3cpdr        kpdpdr
114600040908     c     kfapd         Chain     Fiapd01L
114700040908     c                   If        %Found(Fiapd01l)
114800040908     c                   Eval      v3dpdr = ApdRsc
114900040908     c                   EndIf
115000041013   x1c                   Else
115100041013     c                   Eval      *In10 = *Off
115200041013e   1c                   EndIf
115300001031
115400001031     C                   add       1             nrrf
115500001031     C                   write     fior07sf
115600001031
115700001031     C                   endsr
115800001030      **********************************************************************
115900131120      * F15 - Variazioni
116000001030      **********************************************************************
116100001102     C     Sr_Variazioni BEGSR
116200001030
116300001031     C                   z-add     1             recsf
116400001030      * Pulisco subfile
116500001030     C                   z-add     *zeros        nrr               4 0
116600001030     C                   eval      *in21 = *off
116700001030     C                   write     fior07c
116800001030     C                   eval      *in20 = *on
116900001030     C                   eval      *in21 = *on
117000001030
117100001030      * Carico il subfile in base alle scelte fatte
117200001030     C     kfnor         setll     fnorv01l
117300001030     C                   do        *hival
117400001030     C     kfnor         reade     fnorv01l
117500001030     C                   if        %eof
117600001030     C                   leave
117700001030     C                   endif
117800001030
117900001031     C                   z-add     v1cpoe        v2cpoe
118000001031     C                   z-add     v1nsr         v2nsr
118100001031     C                   z-add     v1nor         v2nor
118200001031     C                   z-add     v1nrv         v2nrv
118300001030
118400001102     C                   exsr      Sr_CarVariaz
118500001030     C                   enddo
118600001030      * Se non ho caricato emetto una videata di segnalazione.
118700001031     C                   if        nrr = *zeros
118800001031     C                   movel     msg(1)        v1cmsg
118900001031     C                   seton                                        28
119000001102     C                   goto      endvariaz
119100001031     C                   endif
119200001030
119300001030      * Se ho caricato emetto il subfile
119400001030     C                   if        nrr > *zeros
119500001031     C                   do        *hival
119600001030     C                   write     fior072
119700001030     C                   exfmt     fior07c
119800001030      * F12=Ritorno
119900001102     C   kl              goto      endvariaz
120000001030
120100001031      * Legge i record scelti
120200001030     C                   do        *hival
120300001030     C                   readc     fior07s
120400001030     C                   if        %eof
120500001030     C                   leave
120600001030     C                   endif
120700001030     C                   if        v2scel = '5'
120800001030     C                   z-add     nrr           recsf
120900001102     C                   exsr      Sr_VisVariaz
121000001030     C                   endif
121100001031     C                   clear                   v2scel
121200001030     C                   update    fior07s
121300001030     C                   enddo
121400001030
121500001030     C                   enddo
121600001031     C                   endif
121700001030
121800001102     C     endvariaz     endsr
121900001030      **********************************************************************
122000001102      * CARICO SUBFILE VARIAZIONI
122100001030      **********************************************************************
122200001102     C     Sr_CarVariaz  begsr
122300001030
122400001030     C                   clear                   v2scel
122500001030     C                   eval      v2cpor = ORVpor
122600001030
122700001030      * Inversione data modifica
122800001103     C                   z-add     ORVdtv        v2dtvd
122900001030     C                   z-add     ORVdtv        G02inv
123000001030     C                   movel     '3'           G02err
123100001030     C                   call      'XSRDA8'
123200001030     C                   parm                    wlbdat
123300001030     C                   movel     G02dat        v2dtv
123400001030
123500001030     C                   movel     ORVorv        v2orv
123600001030     C                   eval      v2utv  = ORVutv
123700001031     C                   eval      v2fao  = ORVfao
123800001031      * Decodifica
123900001031     C                   clear                   TIBS02DS
124000001031     C                   movel     'C'           T02mod
124100001031     C                   movel     knsif         t02sif
124200001031     C                   movel     'FAR'         t02cod
124300001031     C                   movel(p)  v2fao         T02ke1
124400001031     C                   call      'TIBS02R'
124500001031     C                   parm                    KPJBA
124600001031     C                   parm                    TIBS02DS
124700001031     C                   movel     t02uni        v2dfao
124800001030
124900001030     C                   add       1             nrr
125000001030     C                   write     fior07s
125100001030
125200001030     C                   endsr
125300001030      **********************************************************************
125400001102      * VISUALIZZO DATI DEL FILE VARIAZIONI
125500001030      **********************************************************************
125600001102     C     Sr_VisVariaz  begsr
125700001031
125800001031     C                   seton                                        01
125900001030
126000001031     C                   eval      v1mod1 = 'Var.il '
126100001031     C                             + v2dtvg
126200001031     C                             +'/'
126300001031     C                             + v2dtvm
126400001031     C                             + '/'
126500001031     C                             + v2dtva
126600001031     C                             + ' alle '
126700001031     C                             + v2orvo
126800001031     C                             + ':'
126900001031     C                             + v2orvm
127000001031     C                             + ':'
127100001031     C                             + v2orvs
127200001031     C                             + ' da '
127300001031     C                             + v2utv
127400001031
127500001103     C                   eval      kdtv = v2dtvd
127600001103     C                   eval      korv = v2orv
127700001103     C     kforp         chain     fnorv01l
127800001103     C                   if        %found(fnorv01l)
127900001103
128000001102     C                   exsr      Sr_CarVidVar
128100001031     C                   exsr      Sr_Decodifica
128200120612     c                   ExSr      Sr_Tastifun
128300001103     C                   endif
128400001030
128500001102     C                   do        *hival
128600001031     C                   exfmt     fior071
128700001102
128800001102      * F12=Ritorno
128900001102     C   kl              leave
129000131029      /free
129100131029       //?F5=Altri dati
129200131029       IF  *inke;
129300131029         exsr F05D01;
129400131029         iter;
129500131029       ENDIF;
129600131029      /end-free
129700001102
129800001102     C                   enddo
129900001031
130000001030     C                   endsr
130100001031      **********************************************************************
130200001102      * CARICA DATI NELLA VIDEATA DAL FILE VARIAZIONI
130300001031      **********************************************************************
130400001102     C     Sr_CarVidVar  begsr
130500001031
130600001129     C                   movel     ORMtco        v1ctco
130700001031     C                   movel     ORVtor        v1ctor
130800001031      * Inversione data emissione ORM
130900001031     C                   clear                   wlbdat
131000001031     C                   z-add     ORMdao        G02inv
131100001031     C                   movel     '3'           G02err
131200001031     C                   call      'XSRDA8'
131300001031     C                   parm                    wlbdat
131400001031     C                   z-add     G02dat        v1dao
131500010109     C                   movel     ORMoao        v1oao
131600001031      * Dati Ordinante
131700001031     C                   z-add     ORVcor        v1ccor
131800001031     C                   movel     ORVrso        v1rso
131900001031     C                   movel     ORVino        v1ino
132000001031     C                   movel     ORVcao        v1cao
132100001031     C                   movel     ORVloo        v1loo
132200001031     C                   movel     ORVpro        v1pro
132300001031     C                   movel     ORVnao        v1nao
132400001031      * Dati Ritiro a
132500001031     C                   z-add     ORVcra        v1ccra
132600001031     C                   movel     ORVrsr        v1rsr
132700001031     C                   movel     ORVinr        v1inr
132800001031     C                   movel     ORVcar        v1car
132900001031     C                   movel     ORVlor        v1lor
133000001031     C                   movel     ORVprr        v1prr
133100001031     C                   movel     ORVnar        v1nar
133200001031     C                   movel     ORVrer        v1rer
133300001031     C                   movel     ORVter        v1ter
133400001031      * Dati Consegna a
133500001031     C                   z-add     ORVcrc        v1ccrc
133600001031     C                   movel     ORVrsc        v1rsc
133700001031     C                   movel     ORVinc        v1inc
133800001031     C                   movel     ORVloc        v1loc
133900001031     C                   movel     ORVcac        v1cac
134000001031     C                   movel     ORVprc        v1prc
134100001031     C                   movel     ORVnac        v1nac
134200001031
134300010315     C                   if        ORVffd = 'S'
134400010315     C                   eval      v1ffd = 'SI'
134500010315     C                   else
134600010315     C                   eval      v1ffd = 'NO'
134700010315     C                   endif
134800010315
134900001031     C                   movel     ORVnam        v1nam
135000001031      * Inversione data ritiro
135100001031     C                   z-add     ORVdar        G02inv
135200001031     C                   movel     '3'           G02err
135300001031     C                   call      'XSRDA8'
135400001031     C                   parm                    wlbdat
135500001031     C                   z-add     G02dat        v1dar
135600001031     C                   movel     ORVorr        v1orr
135700001031
135800001031     C                   z-add     ORVncl        v1ncl
135900001031     C                   z-add     ORVpkg        v1pkg
136000001031     C                   z-add     ORVvlm        v1vlm
136100001031     C                   z-add     ORVbnc        v1bnc
136200001031     C                   z-add     ORVblc        v1blc
136300001031     C                   z-add     ORVatt        v1att
136400001031     C                   z-add     ORVmtc        v1mtc
136500001031
136600010315     C                   if        ORVpag = 'M'
136700010315     C                   eval      v1pag = 'Mitt.'
136800010315     C                   endif
136900010315     C                   if        ORVpag = 'D'
137000010315     C                   eval      v1pag = 'Dest.'
137100010315     C                   endif
137200010315     C                   if        ORVpag = 'O'
137300010315     C                   eval      v1pag = 'Ord. '
137400010315     C                   endif
137500010315
137600001031     C                   z-add     ORVksc        v1ksc
137700001031     C                   movel     ORVctr        v1ctr
137800010315
137900010315     C                   if        ORVddt = 'S'
138000010315     C                   eval      v1ddt = 'SI'
138100010315     C                   endif
138200010315     C                   if        ORVddt = 'N'
138300010315     C                   eval      v1ddt = 'NO'
138400010315     C                   endif
138500010315
138600001031     C                   movel     ORVpor        v1cpor
138700001031     C                   movel     ORVpoc        v1cpoc
138800001031     C                   movel     ORVno1        v1not1
138900001031     C                   movel     ORVno2        v1not2
139000001031     C                   z-add     ORVfao        v1fao
139100001031     C                   z-add     ORMpos        v1cpos
139200001031     C                   z-add     ORMors        v1ors
139300001031     C                   movel     ORVrfa        v1rfa
139400001221     C                   move      ORVsto        v1sto
139500001031     C                   z-add     ORVndc        v1ndc
139600001031      * Inversione data distinta
139700001031     C                   clear                   wlbdat
139800001031     C                   z-add     ORVddc        G02inv
139900001031     C                   movel     '3'           G02err
140000001031     C                   call      'XSRDA8'
140100001031     C                   parm                    wlbdat
140200001031     C                   z-add     G02dat        v1ddc
140300010226
140400010226      * Cerca codice autotrasportatore
140500071129     c                   clear                   v1cpdc
140600001031     C                   z-add     ORVndc        knfv
140700001031     C                   z-add     ORVpor        kfgs
140800120612     C     kfidst        chain     fidst01l
140900120612     C                   if        %found(fidst01l) and DSTatb = *blanks
141000001031     C                   eval      v1cpdc = DSTpdr
141100001031     C                   endif
141200010119
141300010315     C                   if        ORVspi = 'S'
141400010315     C                   eval      v1spi = 'SI'
141500010315     C                   else
141600010315     C                   eval      v1spi = 'NO'
141700010315     C                   endif
141800030627
141900030627     C                   eval      dorm01 = ORvflo
142000030627
142100030627      * ORM commissionato
142200030627     c                   If        §orCom = 'S'
142300030627     c                   Eval      V1dCom = 'COMMISSIONATO'
142400030627     c                   Else
142500030627     c                   Clear                   V1dCom
142600030627     c                   EndIf
142700131211
142800131211     c                   eval      *in46 = *off
142900131029
143000131029      /free
143100131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
143200131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
143300131211           *in46 = *on;
143400131211         ENDIF;
143500131029       //?Recupero dati da estensione FNORVE
143600131029       //?Rcd 'O ' --> Orari apertura
143700131029         k_ORVEpoe = ORVpoe;
143800131029         k_ORVEnsr = ORVnsr;
143900131029         k_ORVEnor = ORVnor;
144000131029         k_ORVEnrv = ORVnrv;
144100131029         k_ORVEdtv = ORVdtv;
144200131029         k_ORVEorv = ORVorv;
144300131029         k_ORVEtrc = 'O ';
144400131029         clear dOREorari;
144500131029       //?Aggancio il rcd
144600171106         chain %kds(k07fnorve) FNORVE1L;
144700131029         IF  %found(FNORVE1L);
144800131029           dOREorari = ORVEdati;
144900131029         ENDIF;
145000131029         v1OraAMda = §OREoramda;
145100131029         v1OraAMa  = §OREorama;
145200131029         v1OraPMda = §OREorapda;
145300131029         v1OraPMa  = §OREorapa;
145400170607
145500170607       //?Rcd 'DT' --> Data Pronta Merce
145600170607         k_ORVEtrc = 'DT';
145700170607         clear dOREdt;
145800170607       //?Aggancio il rcd
145900171106         chain %kds(k07fnorve) FNORVE1L;
146000170607         IF  %found(FNORVE1L);
146100170607           dOREdt = ORVEdati;
146200170607         ENDIF;
146300170607         IF  §OREdpm > *zeros;
146400170607           clear wlbdat;
146500170607           G02inv = §OREdpm;
146600170607           G02err = '3';
146700170607           xsrda8 (wlbdat);
146800170607           V1dpm = G02dat;
146900170607         ENDIF;
147000160513
147100131029      /end-free
147200001031
147300001031     C                   endsr
147400001025      **********************************************************************
147500001025      * F18 - Note
147600001025      **********************************************************************
147700001025     C     Sr_Note       BEGSR
147800001025
147900001025     C                   clear                   fior06DS
148000001025     C                   z-add     v1dao         i06dta
148100001026     C                   z-add     v1cpoe        i06poe
148200001026     C                   z-add     v1nor         i06nor
148300001026     C                   z-add     v1nsr         i06nsr
148400001026     C                   z-add     v1nrv         i06nrv
148500001026     C                   movel     v1fao         i06far
148600001026     C                   eval      i06dai = §doggi
148700001026     C                   TIME                    W0140
148800001026     C                   movel     w0140         I06ori
148900001026     C                   movel     'I'           i06flm
149000061017     c   ki              movel     'V'           i06flm
149100001026
149200001025     C                   CALL      'FIOR06R'
149300001025     C                   PARM                    kpjba
149400001027     C                   parm                    fior06ds
149500001025
149600001025     C                   endsr
149700001031      **********************************************************************
149800001031      * F19 - Proposte
149900001031      **********************************************************************
150000001031     C     Sr_Proposte   BEGSR
150100001031
150200001031     C                   z-add     1             recsfp
150300001031      * Pulisco subfile
150400001031     C                   z-add     *zeros        nrrp              4 0
150500001031     C                   eval      *in21 = *off
150600001031     C                   write     fior07cp
150700001031     C                   eval      *in20 = *on
150800001031     C                   eval      *in21 = *on
150900001031
151000001031      * Carico il subfile in base alle scelte fatte
151100001031     C     kfnor         setll     fnorp01l
151200001031     C                   do        *hival
151300001031     C     kfnor         reade     fnorp01l
151400001031     C                   if        %eof
151500001031     C                   leave
151600001031     C                   endif
151700001031
151800001031     C                   z-add     v1cpoe        v4cpoe
151900001031     C                   z-add     v1nsr         v4nsr
152000001031     C                   z-add     v1nor         v4nor
152100001031     C                   z-add     v1nrv         v4nrv
152200001031
152300001031     C                   exsr      Sr_CarProposte
152400001031     C                   enddo
152500001031      * Se non ho caricato emetto una videata di segnalazione.
152600001106     C                   if        nrrp = *zeros and psce <> 'P'
152700001103     C                   movel     msg(3)        v1cmsg
152800001031     C                   seton                                        28
152900001031     C                   goto      endproposte
153000001031     C                   endif
153100001103
153200001103     C                   if        nrrp = *zeros and psce = 'P'
153300001103     C                   do        *hival
153400001103     C                   exfmt     fior075
153500001103      * F12=Ritorno
153600001103     C   kl              leave
153700001103     C                   enddo
153800001103     C                   endif
153900001031
154000001031      * Se ho caricato emetto il subfile
154100001031     C                   if        nrrp > *zeros
154200001031     C                   do        *hival
154300001031     C                   write     fior074
154400001031     C                   exfmt     fior07cp
154500001031      * F12=Ritorno
154600001031     C   kl              goto      endproposte
154700001031
154800001031      * Legge i record scelti
154900001031     C                   do        *hival
155000001031     C                   readc     fior07sp
155100001031     C                   if        %eof
155200001031     C                   leave
155300001031     C                   endif
155400001031     C                   if        v4scel = '5'
155500001031     C                   z-add     nrrp          recsfp
155600001031     C                   exsr      Sr_VisProposte
155700001031     C                   endif
155800001031     C                   clear                   v4scel
155900001031     C                   update    fior07sp
156000001031     C                   enddo
156100001031
156200001031     C                   enddo
156300001031     C                   endif
156400001031
156500001031     C     endproposte   endsr
156600001031      **********************************************************************
156700001031      * CARICO SUBFILE PROPOSTE
156800001031      **********************************************************************
156900001031     C     Sr_CarPropostebegsr
157000001031
157100001031     C                   clear                   v4scel
157200001031     C                   eval      v4cpor = ORPpoe
157300001031
157400001031      * Inversione data modifica
157500001103     C                   z-add     ORPdtv        v4dtvd
157600001031     C                   z-add     ORPdtv        G02inv
157700001031     C                   movel     '3'           G02err
157800001031     C                   call      'XSRDA8'
157900001031     C                   parm                    wlbdat
158000001031     C                   movel     G02dat        v4dtv
158100001031
158200001031     C                   movel     ORPorv        v4orv
158300001031     C                   eval      v4utv  = ORPutv
158400001031     C                   eval      v4fao  = ORPfao
158500001031      * Decodifica
158600001031     C                   clear                   TIBS02DS
158700001031     C                   movel     'C'           T02mod
158800001031     C                   movel     knsif         t02sif
158900001031     C                   movel     'FAR'         t02cod
159000001031     C                   movel(p)  v4fao         T02ke1
159100001031     C                   call      'TIBS02R'
159200001031     C                   parm                    KPJBA
159300001031     C                   parm                    TIBS02DS
159400001031     C                   movel     t02uni        v4dfao
159500001031      * Analisi Proposta
159600001031     C                   if        ORPfev <> *blanks
159700001031     C                   seton                                        03
159800001031     C                   if        ORPfev = 'E'
159900001031     C                   eval      v4sta = 'EFFETTUATA'
160000001107     C                   eval      v4sta1= 'EFF'
160100001031     C                   endif
160200001031     C                   if        ORPfev = 'R'
160300001031     C                   eval      v4sta = 'RIFIUTATA'
160400001107     C                   eval      v4sta1= 'RIF'
160500001031     C                   endif
160600001031      * Inversione data analisi proposta
160700001031     C                   z-add     ORPdte        G02inv
160800001031     C                   movel     '3'           G02err
160900001031     C                   call      'XSRDA8'
161000001031     C                   parm                    wlbdat
161100001031     C                   movel     G02dat        v4dte
161200001031     C                   movel     ORPore        v4ore
161300001031     C                   eval      v4ute  = ORPute
161400001107     C                   else
161500001107     C                   setoff                                       03
161600001107     C                   clear                   v4sta
161700001107     C                   clear                   v4sta1
161800001107     C                   clear                   v4dte
161900001107     C                   clear                   v4ore
162000001107     C                   clear                   v4ute
162100001031     C                   endif
162200001031
162300001031     C                   add       1             nrrp
162400001031     C                   write     fior07sp
162500001031
162600001031     C                   endsr
162700001031      **********************************************************************
162800001031      * VISUALIZZO DATI DEL FILE PROPOSTE
162900001031      **********************************************************************
163000001031     C     Sr_VisPropostebegsr
163100001031
163200140303     c                   eval      *in15 = *on
163300001031
163400001031     C                   eval      v1mod1 = 'Prop. del '
163500001031     C                             + v4dtvg
163600001031     C                             +'/'
163700001031     C                             + v4dtvm
163800001031     C                             + '/'
163900001031     C                             + v4dtva
164000001031     C                             + ' alle '
164100001031     C                             + v4orvo
164200001031     C                             + ':'
164300001031     C                             + v4orvm
164400001031     C                             + ':'
164500001031     C                             + v4orvs
164600001031     C                             + ' da '
164700001031     C                             + v4utv
164800001031
164900001103     C                   eval      kdtv = v4dtvd
165000001103     C                   eval      korv = v4orv
165100001103     C     kforp         chain     fnorp01l
165200001103     C                   if        %found(fnorp01l)
165300001103
165400001031     C                   exsr      Sr_CarVidPro
165500001031     C                   exsr      Sr_Decodifica
165600120612     c                   ExSr      Sr_Tastifun
165700001103     C                   endif
165800001031
165900001102     C                   do        *hival
166000001031     C                   exfmt     fior071
166100001102
166200001102      * F12=Ritorno
166300001102     C   kl              leave
166400131029      /free
166500131029       //?F5=Altri dati
166600131029       IF  *inke;
166700131029         exsr F05D01;
166800131029         iter;
166900131029       ENDIF;
167000140303
167100140303       //?F17=Note AUT
167200140303       IF  *inkr;
167300140303         exsr F17D01;
167400140303         iter;
167500140303       ENDIF;
167600131029      /end-free
167700001102
167800001102     C                   enddo
167900001031
168000001031     C                   endsr
168100001031      **********************************************************************
168200001031      * CARICA DATI NELLA VIDEATA DAL FILE PROPOSTE
168300001031      **********************************************************************
168400001031     C     Sr_CarVidPro  begsr
168500001031
168600001129     C                   movel     ORMtco        v1ctco
168700001031     C                   movel     ORPtor        v1ctor
168800001031      * Inversione data emissione ORM
168900001031     C                   clear                   wlbdat
169000001031     C                   z-add     ORMdao        G02inv
169100001031     C                   movel     '3'           G02err
169200001031     C                   call      'XSRDA8'
169300001031     C                   parm                    wlbdat
169400001031     C                   z-add     G02dat        v1dao
169500010109     C                   movel     ORMoao        v1oao
169600001031      * Dati Ordinante
169700001031     C                   z-add     ORPcor        v1ccor
169800001031     C                   movel     ORPrso        v1rso
169900001031     C                   movel     ORPino        v1ino
170000001031     C                   movel     ORPcao        v1cao
170100001031     C                   movel     ORPloo        v1loo
170200001031     C                   movel     ORPpro        v1pro
170300001031     C                   movel     ORPnao        v1nao
170400001031      * Dati Ritiro a
170500001031     C                   z-add     ORPcra        v1ccra
170600001031     C                   movel     ORPrsr        v1rsr
170700001031     C                   movel     ORPinr        v1inr
170800001031     C                   movel     ORPcar        v1car
170900001031     C                   movel     ORPlor        v1lor
171000001031     C                   movel     ORPprr        v1prr
171100001031     C                   movel     ORPnar        v1nar
171200001031     C                   movel     ORPrer        v1rer
171300001031     C                   movel     ORPter        v1ter
171400001031      * Dati Consegna a
171500001031     C                   z-add     ORPcrc        v1ccrc
171600001031     C                   movel     ORPrsc        v1rsc
171700001031     C                   movel     ORPinc        v1inc
171800001031     C                   movel     ORPloc        v1loc
171900001031     C                   movel     ORPcac        v1cac
172000001031     C                   movel     ORPprc        v1prc
172100001031     C                   movel     ORPnac        v1nac
172200001031
172300010315     C                   if        ORPffd = 'S'
172400010315     C                   eval      v1ffd = 'SI'
172500010315     C                   else
172600010315     C                   eval      v1ffd = 'NO'
172700010315     C                   endif
172800010315
172900001031     C                   movel     ORPnam        v1nam
173000001031      * Inversione data ritiro
173100001031     C                   z-add     ORPdar        G02inv
173200001031     C                   movel     '3'           G02err
173300001031     C                   call      'XSRDA8'
173400001031     C                   parm                    wlbdat
173500001031     C                   z-add     G02dat        v1dar
173600001031     C                   movel     ORPorr        v1orr
173700001031
173800001031     C                   z-add     ORPncl        v1ncl
173900001031     C                   z-add     ORPpkg        v1pkg
174000001031     C                   z-add     ORPvlm        v1vlm
174100001031     C                   z-add     ORPbnc        v1bnc
174200001031     C                   z-add     ORPblc        v1blc
174300001031     C                   z-add     ORPatt        v1att
174400001031     C                   z-add     ORPmtc        v1mtc
174500010315
174600010315     C                   if        ORPpag = 'M'
174700010315     C                   eval      v1pag = 'Mitt.'
174800010315     C                   endif
174900010315     C                   if        ORPpag = 'D'
175000010315     C                   eval      v1pag = 'Dest.'
175100010315     C                   endif
175200010315     C                   if        ORPpag = 'O'
175300010315     C                   eval      v1pag = 'Ord. '
175400010315     C                   endif
175500001031
175600001031     C                   z-add     ORPksc        v1ksc
175700001031     C                   movel     ORPctr        v1ctr
175800010315
175900010315     C                   if        ORPddt = 'S'
176000010315     C                   eval      v1ddt = 'SI'
176100010315     C                   endif
176200010315     C                   if        ORPddt = 'N'
176300010315     C                   eval      v1ddt = 'NO'
176400010315     C                   endif
176500010315
176600001031     C                   movel     ORPpor        v1cpor
176700001031     C                   movel     ORPpoc        v1cpoc
176800001031     C                   movel     ORPno1        v1not1
176900001031     C                   movel     ORPno2        v1not2
177000001031     C                   z-add     ORPfao        v1fao
177100001031     C                   z-add     ORMpos        v1cpos
177200001031     C                   z-add     ORMors        v1ors
177300001031     C                   movel     ORPrfa        v1rfa
177400001221     C                   move      ORPsto        v1sto
177500001031     C                   z-add     ORPndc        v1ndc
177600001031      * Inversione data distinta
177700001031     C                   clear                   wlbdat
177800001031     C                   z-add     ORPddc        G02inv
177900001031     C                   movel     '3'           G02err
178000001031     C                   call      'XSRDA8'
178100001031     C                   parm                    wlbdat
178200001031     C                   z-add     G02dat        v1ddc
178300010226
178400010226      * Cerca codice autotrasportatore
178500071129     c                   clear                   v1cpdc
178600001031     C                   z-add     ORPndc        knfv
178700001031     C                   z-add     ORPpor        kfgs
178800120612     C     kfidst        chain     fidst01l
178900120612     C                   if        %found(fidst01l) and DSTatb = *blanks
179000001031     C                   eval      v1cpdc = DSTpdr
179100001031     C                   endif
179200010226
179300010315     C                   if        ORPspi = 'S'
179400010315     C                   eval      v1spi = 'SI'
179500010315     C                   else
179600010315     C                   eval      v1spi = 'NO'
179700010315     C                   endif
179800030627
179900030627     C                   eval      dorm01 = ORpflo
180000030627
180100030627      * ORM commissionato
180200030627     c                   If        §orCom = 'S'
180300030627     c                   Eval      V1dCom = 'COMMISSIONATO'
180400030627     c                   Else
180500030627     c                   Clear                   V1dCom
180600030627     c                   EndIf
180700131211
180800131211     c                   eval      *in46 = *off
180900140303     c                   eval      *in48 = *off
181000131029
181100131029      /free
181200131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
181300131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
181400131211           *in46 = *on;
181500131211         ENDIF;
181600131029       //?Recupero dati da estensione FNORPE
181700131029       //?Rcd 'O ' --> Orari apertura
181800131029         k_ORPEpoe = ORPpoe;
181900131029         k_ORPEnsr = ORPnsr;
182000131029         k_ORPEnor = ORPnor;
182100131029         k_ORPEnrv = ORPnrv;
182200131029         k_ORPEdtv = ORPdtv;
182300131029         k_ORPEorv = ORPorv;
182400131029         k_ORPEtrc = 'O ';
182500131029         clear dOREorari;
182600131029       //?Aggancio il rcd
182700171106         chain %kds(k07fnorpe) FNORPE1L;
182800131029         IF  %found(FNORPE1L);
182900131029           dOREorari = ORPEdati;
183000131029         ENDIF;
183100131029         v1OraAMda = §OREoramda;
183200131029         v1OraAMa  = §OREorama;
183300131029         v1OraPMda = §OREorapda;
183400131029         v1OraPMa  = §OREorapa;
183500161114       //?Rcd 'DT' --> Data Pronta Merce
183600161114         k_ORPEtrc = 'DT';
183700161114         clear dOREdt;
183800161114       //?Aggancio il rcd
183900171106         chain %kds(k07fnorpe) FNORPE1L;
184000161114         IF  %found(FNORPE1L);
184100161114           dOREdt = ORPEdati;
184200161114         ENDIF;
184300161114         IF  §OREdpm > *zeros;
184400161114           clear wlbdat;
184500161114           G02inv = §OREdpm;
184600161114           G02err = '3';
184700161114           xsrda8 (wlbdat);
184800161114           V1dpm = G02dat;
184900161114         ENDIF;
185000140303
185100140303       //?Controllo se ci sono proposte Note AUT
185200140303         chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
185300140303         IF  %found(FNORPT1L);
185400140303           *in48 = *on;
185500140303         ENDIF;
185600131029      /end-free
185700001031
185800001031     C                   endsr
185900010109      **********************************************************************
186000010109      * Carico dati dalla kpjba per programma richiamato da interrogazione
186100010109      * proposte
186200010109      **********************************************************************
186300010109     C     Sr_Cardatipropbegsr
186400010109
186500010109     C                   z-add     ppoe          v4cpoe
186600010109     C                   z-add     pnsr          v4nsr
186700010109     C                   z-add     pnor          v4nor
186800010109     C                   z-add     pnrv          v4nrv
186900010109     C                   z-add     pdtv          v4dtv
187000010109     C                   z-add     porv          v4orv
187100010109      * Inverto la data
187200010109     C                   z-add     v4dtv         G02DAT
187300010109     C                   MOVEL     *BLANK        G02ERR
187400010109     C                   CALL      'XSRDA8'
187500010109     C                   PARM                    WLBDAT
187600010109     C                   Z-ADD     G02INV        v4dtvd
187700010109
187800010109     C                   movel     putv          v4utv
187900010109
188000010109     C                   endsr
188100061017      *--------------------------------------------------------------------*
188200061017      * F9 - Sollecito
188300061017      *--------------------------------------------------------------------*
188400061017     c     sr_sollecito  begsr
188500061017
188600061017      * il sollecito è possibile se l'ORM è automatizzato
188700061017      * p.o. emissione DPD
188800061017     c     v1cpoe        lookup    skpodpd                                30
188900061017     c                   if        *in30 and v1ctco <> 'F'
189000061017     c                   eval      *in28 = *on
189100061017     c                   eval      v1cmsg = msg(05)
189200061017     c                   goto      for01
189300061017     c                   endif
189400061017      * p.o. ritiro DPD
189500061017     c     v1cpor        lookup    skpodpd                                30
189600061017     c                   if        *in30 and v1cfcs <> 'F'
189700061017     c                   eval      *in28 = *on
189800061017     c                   eval      v1cmsg = msg(05)
189900061017     c                   goto      for01
190000061017     c                   endif
190100061017      * se orm in fase 900 o 999 non posso sollecitare
190200061017     c                   if        v1fao > 600
190300061017     c                   eval      *in28 = *on
190400061017     c                   eval      v1cmsg = msg(06)
190500061017     c                   goto      for01
190600061017     c                   endif
190700061018      * il sollecito è possibile se l'ORM è già stato stampato
190800080505     c     v1cpor        lookup    skpodpd                                30
190900080505     c                   if        *in30 and v1cfcs = 'F' and
191000080505     c                             v1dst = *zeros
191100061018     c                   eval      *in28 = *on
191200061018     c                   eval      v1cmsg = msg(04)
191300061018     c                   goto      for01
191400061018     c                   endif
191500061017
191600061017      * p.o. ritiro DPD
191700061017     c     v1cpor        lookup    skpodpd                                30
191800061017      * devo richiamare il pgm di stampa per mandare il sollecito
191900061017      * via fax o mail
192000061017     c                   if        *in30
192100090211      * prima di mandare il fax o la mail verifico se il depot è associato ad un cod.cliente
192200090211     c                   exsr      sr_depot
192300090211      * se non associato errore e non si fa il sollecito
192400090211     c                   if        odepksc = *zeros
192500090211     c                   eval      *in28 = *on
192600090211     c                   eval      v1cmsg = msg(07)
192700171027     c                   eval      %subst(v1cmsg:32:7) = OSID4DDPT
192800090211     c                   goto      for01
192900090211     c                   endif
193000061017     c                   exsr      sr_stampa
193100061017     c                   endif
193200061017
193300061017      * scrivo in automatico la nota per avvenuto sollecito
193400061017     c                   eval      ornpoe = v1cpoe
193500061017     c                   eval      ornnsr = v1nsr
193600061017     c                   eval      ornnor = v1nor
193700061017     c                   eval      ornnrv = v1nrv
193800061017     c                   eval      ornfar = v1fao
193900061017     c                   clear                   wprg
194000061017      * cerco ultimo progressivo relativo alla stessa fase
194100061017     c     kfnorn        setll     fnorn02l
194200061017     c                   do        *hival
194300061017     c     kfnorn        reade     fnorn02l
194400061017     c                   if        %eof(fnorn02l)
194500061017     c                   leave
194600061017     c                   endif
194700061017     c                   eval      wprg = ornprg
194800061017     c                   enddo
194900061017      * scrivo la nota
195000061017     c                   move      data_oggi     ds_data
195100061017     c                   add       1             wprg
195200061017     c                   clear                   fnorn000
195300061017     c                   eval      ornpoe = v1cpoe
195400061017     c                   eval      ornnsr = v1nsr
195500061017     c                   eval      ornnor = v1nor
195600061017     c                   eval      ornnrv = v1nrv
195700061017     c                   eval      ornfar = v1fao
195800061017     c                   eval      orndai = dateu
195900061017     c                   time                    ornori
196000061017     c                   eval      ornprg = wprg
196100061017     c                   eval      ornpue = knmus
196200150309     c                   eval      ornpon = DUTpou
196300061017     c                   eval      ornsns = 'I'
196400061017     c                   eval      ornnot = '** SOLLECITATO IL ' +
196500061017     c                             %editc(ds_giorno:'1') + '/' +
196600061017     c                             %editc(ds_mese:'1') + '/' +
196700061017     c                             %editc(ds_anno:'Z') + ' **'
196800061017     c                   eval      ornftr = 'S'
196900061017     c                   eval      orndtr = dateu
197000061017     c                   write     fnorn000
197100061017
197200061017      * richiamo il pgm di gestione note
197300061017     c                   exsr      sr_note
197400061017
197500061017     c                   endsr
197600090211
197700090211      *--------------------------------------------------------------------*
197800090211      * Controllo il depot
197900090211      *--------------------------------------------------------------------*
198000090211     c     sr_depot      begsr
198100090211
198200171025     c                   clear                   tisid4ds
198300171106     c                   eval      isid4dri = dateu
198400171025     c                   eval      isid4nzd = ormnar
198500171025     c                   eval      isid4cad = ormcar
198600090211     c                   if        ormpkg > §3ilmp
198700171025     c                   eval      isid4sorc = 101
198800090211     c                   else
198900171025     c                   eval      isid4sorc = 136
199000090211     c                   endif
199100171025     c                   eval      isid4lnp = ormpoe
199200171025     c                   call      'TISID4R'
199300171025     c                   parm                    tisid4ds
199400090211      * con il depot richiamo pgm per avere il codice cliente
199500090211      * cliente
199600090211     c                   clear                   truldepds
199700171025     c                   clear                   truldepds1
199800171106     c                   eval      idepdpc = osid4dstr
199900171106     c                   eval      idepdep7 = OSID4DDPT
200000171106     c                   eval      idepdrf = dateu
200100171027     c                   call      'TRULDEPR'
200200090211     c                   parm                    truldepds
200300171027     c                   parm                    truldepds1
200400090211
200500090211     c                   endsr
200600061017
200700061017      *--------------------------------------------------------------------*
200800061017      * Richiama il pgm di stampa per sollecito
200900061017      *--------------------------------------------------------------------*
201000061017     c     sr_stampa     begsr
201100061017
201200061017     c                   clear                   fior16ds
201300061017     c                   clear                   trul90ds
201400061017     c                   movel     'S'           d90rso
201500061017     c                   call      'TRUL90R'
201600061017     c                   parm                    kpjba
201700061017     c                   Parm                    trul90ds
201800061017      * F3=Fine
201900061017     c                   if        d90f3 = '1'
202000061017     c                   goto      endsrstampa
202100061017     c                   endif
202200090202      /free
202300090202         reset ds5P;
202400090202         if  %subst(knmus : 1 : 3) = 'EDP';
202500090202           TBLkey = 'O' + 'EDP';
202600090202         else;
202700090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
202800090202         endif;
202900090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
203000090202         if  %found( TABEL00F );
203100090202           ds5P = TBLuni;
203200090202         endif;
203300090202      /end-free
203400061017     c                   movel     d90mdo        r16mdo
203500061017     c                   movel     d90pro        r16oqo
203600061017     c                   eval      r16tla = *blanks
203700061017      * è sempre sollecito imposto il nuovo tipo
203800061017     c                   eval      r16ris ='X'
203900061017     c                   eval      r16poe = v1cpoe
204000061017     c                   eval      r16nsr = v1nsr
204100061017     c                   eval      r16nor = v1nor
204200061017     c                   eval      r16nrv = v1nrv
204300061017     c                   eval      r16por = v1cpor
204400061017     c                   eval      r16dst = ormdar
204500061017     c                   movel     kpjbu         §kpjbu
204600061017     c                   movel(p)  fior16ds      kpjbu
204700090202     c                   call      §5Ppgm
204800061017     c                   parm                    kpjba
204900080318     c                   movel     kpjbu         fior16ds
205000061017     c                   movel     §kpjbu        kpjbu
205100061017
205200061017     c     endsrstampa   endsr
205300061019
205400061019      *--------------------------------------------------------------------*
205500061023      * F2 - Dati DPD
205600061019      *--------------------------------------------------------------------*
205700061019     c     sr_datidpd    begsr
205800080430
205900090202      * Salvataggio KPJBU
206000090202     c                   eval      §kpjbu = kpjbu
206100090202
206200080430     c                   clear                   fieu55ds
206300080422
206400080430      * se ORM commissionato da DPD passo il n. ORM DPD
206500080430     c     ormpoe        lookup    skpodpd                                30
206600080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
206700080430     c                   eval      i55dep = %subst(ormrfa:4:4)
206800080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
206900081015     c                   eval      i55ie = 'I'
207000080430     c                   eval      kpjbu = fieu55ds
207100080430     c                   call      'FIEU55C'
207200080430     c                   parm                    kpjba
207300080430     c                   endif
207400080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
207500080430     c     ormpor        lookup    skpodpd                                30
207600080430     c                   if        *in30
207700061019     c                   eval      i55poe = v1cpoe
207800061019     c                   eval      i55nrs = v1nsr
207900061019     c                   eval      i55nro = v1nor
208000061019     c                   eval      i55nrv = v1nrv
208100081015     c                   eval      i55ie = 'E'
208200061019     c                   eval      kpjbu = fieu55ds
208300061206     c                   call      'FIEU55C'
208400061019     c                   parm                    kpjba
208500080430     c                   endif
208600090202
208700090202      * Ripristino KPJBU
208800090202     c                   eval      kpjbu = §kpjbu
208900061019
209000061019     c                   endsr
209100090211
209200090211      *--------------------------------------------------------------------*
209300090211      * Window per dati appuntamento ORM
209400090211      *--------------------------------------------------------------------*
209500090211     c     sr_ormapp     begsr
209600090223
209700090223      * pulisco il subfile
209800090223     c                   clear                   nrra              4 0
209900090223     c                   eval      *in21 = *off
210000090223     c                   write     fior07apc
210100090223     c                   eval      *in20 = *on
210200090223     c                   eval      *in21 = *on
210300090223
210400090223      * carico il subfile
210500090216      * metto fissa la fase 100, per ora ho l'appuntamento solo su questa fase
210600090225     c                   eval      *in31 = *off
210700090216     c                   eval      kfar = 100
210800090223     c     kfnora        setll     fnora01l
210900090223     c                   do        *hival
211000090223     c     kfnora        reade     fnora01l
211100090223     c                   if        %eof(fnora01l)
211200090225     c                   eval      *in31 = *on
211300090223     c                   leave
211400090223     c                   endif
211500090211     c                   eval      wappref = oraref
211600090211     c                   eval      wappnota1 = %subst(oranote:1:35)
211700090211     c                   eval      wappnota2 = %subst(oranote:36:35)
211800090223     c                   eval      wappora = oraora
211900090223     c                   eval      wappute = oraute
212000090223     c                   eval      g02inv = oradata
212100090223     c                   eval      g02err = '3'
212200090223     c                   call      'XSRDA8'
212300090223     c                   parm                    wlbdat
212400090223     c                   eval      wappdata = g02dat
212500090223     c                   add       1             nrra
212600090223     c                   write     fior07aps
212700090223     c                   enddo
212800090211
212900090223      * se non ho caricato nessun appuntamento emetto una segnalazione
213000090223     c                   if        nrra = *zeros
213100090223     c                   movel     msg(8)        v1cmsg
213200090223     c                   eval      *in28 = *on
213300090223     c                   leavesr
213400090223     c                   endif
213500090223
213600090223      * se ho caricato emetto il subfile
213700090223     c                   if        nrra > *zeros
213800090225     c                   eval      recsfa = 1
213900090223     c                   do        *hival
214000090223     c                   write     fior07apz
214100090223     c                   exfmt     fior07apc
214200090223      * f12-ritorno
214300090223     c                   if        *inkl
214400090223     c                   leave
214500090223     c                   endif
214600090211     c                   enddo
214700090223     c                   endif
214800090211
214900090211     c                   endsr
215000130913
215100130913      /free
215200180222       //--------------------------------------------------------------------
215300180222       //?F8 - Int.Bolle.
215400180222       //--------------------------------------------------------------------
215500180222       BEGSR F08D01;
215600180222
215700180222       //?Richiamo pgm int.bolle unica
215800180222         clear FNLRU6DS;
215900180222         ILRU6sel = 'S';
216000180222         ILRU6poe = ORMpoe;
216100180222         ILRU6nsr = ORMnsr;
216200180222         ILRU6nor = ORMnor;
216300180222         ILRU6nrv = ORMnrv;
216400180222         fnlru6r (kpjba:fnlru6ds);
216500180222
216600180222       ENDSR;
216700131028       //--------------------------------------------------------------------
216800131028       //?F5 - Altri dati.
216900131028       //--------------------------------------------------------------------
217000131028       BEGSR F05D01;
217100131028
217200131028         wEndF05 = *off;
217300131028
217400131028       //?emetto la videata relativa agli altri dati
217500131028         write fior071;
217600131028       //?emetto la window dove richiedo gli altri dati
217700131028         DOW not wEndF05;
217800131028           exfmt fior0712;
217900131028       //?F12-Ritorno
218000131028           IF  *inkl;
218100131028             wEndF05 = *on;
218200131028           ENDIF;
218300131028         ENDDO;
218400131028
218500131028       ENDSR;
218600140110
218700130913       //--------------------------------------------------------------------
218800130913       //?F14 - Interrogazione orari di servizio.
218900130913       //--------------------------------------------------------------------
219000130913       BEGSR F14D01;
219100140206
219200140317       //?Devo trovare la pickup per passarla al trulorsr
219300140206         clear TISI95DS;
219400140206         I95tcn = '3';
219500140206         I95cap = ORMcar;
219600140206         I95nar = ORMnar;
219700140206         I95ind = ORMinr;
219800140206         I95loc = ORMlor;
219900140206         I95prv = ORMprr;
220000140206       //?Peso in base al ntwk fil.ritiro
220100140206         IF  network = 'SDI';
220200140206           I95lkg = 1000;
220300140206         ELSE;
220400140206           clear I95lkg;
220500140206         ENDIF;
220600140206         tisi95r (tisi95ds);
220700140317
220800140317       //?Cerco eventuale ora presunta ritiro
220900140318         clear fior01ds;
221000140317         IF  ORMndc > 0 and ORMfao < 600;
221100140318           IOR01fgs = ORMpor;
221200140318           IOR01ndc = ORMndc;
221300140318           IOR01poe = ORMpoe;
221400140318           IOR01nsr = ORMnsr;
221500140318           IOR01nor = ORMnor;
221600140318           IOR01nrv = ORMnrv;
221700140318           fior01r (fior01ds);
221800140317         ENDIF;
221900130913
222000140317         clear trulorsds;
222100130913         IOREfil = ORMpor;
222200130913         IOREcap = ORMcar;
222300130913         IOREloc = ORMlor;
222400130913         IOREdta = ORMdar;
222500130913         IOREtser = 'R';
222600140206         IOREgf2 = O95gf2;
222700140318         IOREorp = OOR01ora;
222800161115
222900161115       //?Se peso inferiore a 5 kg passo flag
223000161115         clear dIORE01;
223100161115         IF  ORMrmp <> *blanks;
223200161115           §IOREr_mnx = 'S';
223300161115         ENDIF;
223400161115         IOREflo = dIORE01;
223500130913
223600140317         trulorsr (kpjba:trulorsds);
223700130913
223800130913       ENDSR;
223900140110
224000140110       //--------------------------------------------------------------------
224100140110       //?F17 - Note AUT.
224200140110       //--------------------------------------------------------------------
224300140110       BEGSR F17D01;
224400140121
224500140121         *in48 = *off;
224600140110
224700140110         clear fiort1ds;
224800140303         IORT1mod = 'G';
224900140110         IORT1poe = ORMpoe;
225000140110         IORT1nsr = ORMnsr;
225100140110         IORT1nor = ORMnor;
225200140110         IORT1nrv = ORMnrv;
225300140110         IORT1pgm = 'FIOR07R';
225400140303         IF  *in15;
225500140303           IORT1mod = 'I';
225600140303           IORT1dim = V4Dtvd;
225700140303           IORT1oim = V4orv;
225800140303         ENDIF;
225900140114         §kpjbu = kpjbu;
226000140110
226100140320         IF  psce = 'P' and *in15;
226200140320           fiort1c (kpjba:fiort1ds);
226300140320         ELSE;
226400140110         fiort1r (kpjba:fiort1ds);
226500140320         ENDIF;
226600140114
226700140114         kpjbu = §kpjbu;
226800140121
226900140121       //?Controllo se sono state immesse Note AUT
227000140304         IF  not *in15;
227100140304           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
227200140304           IF  %found(FNORT01L);
227300140304             *in48 = *on;
227400140304           ENDIF;
227500140304         ENDIF;
227600140304       //?Controllo se ci sono proposte Note AUT
227700140304         IF  *in15;
227800140304           chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
227900140304           IF  %found(FNORPT1L);
228000140304             *in48 = *on;
228100140304           ENDIF;
228200140304         ENDIF;
228300140110
228400140110       ENDSR;
228500130913
228600130913      /end-free
228700130913
228800090210
228900090210      *------------------------------------------------------------------------*
229000090210      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
229100090210      *------------------------------------------------------------------------*
229200090210     c     sr_f24        begsr
229300090210
229400090210     c                   Select
229500090210
229600090210     c                   When      $loop = 2 and $rig =2  or
229700090210     c                             $loop = 3 and $rig =3
229800090210     c                   Movea     rigatf1       vzfd01
229900090210     c                   Z-add     1             $rig
230000090210
230100090210     c                   If        $loop = 2
230200090210     c                   Eval      vzfd02= cf2412
230300090210     c                   Else
230400090210     c                   Eval      vzfd02= cf2413
230500090210     c                   EndIf
230600090210
230700090210     c                   When      $loop = 2 and $rig =1 or
230800090210     c                             $loop = 3 and $rig =1
230900090210     c                   Movea     rigatf2       vzfd01
231000090210     c                   Z-add     2             $rig
231100090210
231200090210     c                   If        $loop = 2
231300090210     c                   Eval      vzfd02 = cf2422
231400090210     c                   Else
231500090210     c                   Eval      vzfd02 = cf2423
231600090210     c                   EndIf
231700090210
231800090210     c                   When      $loop = 3 and $rig =2
231900090210     c                   Movea     rigatf3       vzfd01
232000090210     c                   Z-add     3             $rig
232100090210     c                   Eval      vzfd02 = cf2433
232200090210     c                   EndSl
232300090210
232400090210     c                   EndSr
232500090210
232600090210      *------------------------------------------------------------------------*
232700090210      *  Imposto descrizione tasti funzione
232800090210      *------------------------------------------------------------------------*
232900090210     c     Sr_Tastifun   BegSr
233000140304
233100140304      * Abilito eventuali tasti funzionali
233200140304     c                   exsr      Abil_Fxx
233300090210
233400090210      * Conta i caratteri riempiti dalle RigaTf1
233500090210     c                   Clear                   $z
233600090210      * Conta i caratteri riempiti dalle RigaTf2
233700090210     c                   Clear                   $k
233800090210      * Conta i caratteri riempiti dalle RigaTf3
233900090210     c                   Clear                   $j
234000090210      * Conta la posizione del campone da cui partire per impostare
234100090210      * la descrizione del tasto funzione
234200090210     c                   Clear                   $y
234300090210
234400090210      * Stringhe che contengono le descrizioni dei tasti funzione
234500090210     c                   Reset                   rigatf1
234600090210     c                   Reset                   rigatf2
234700090210     c                   Reset                   rigatf3
234800090217
234900131028      * F1 - Aggiungi Richiesta Assistenza
235000131028     c                   if        *in08
235100131028     c                   reset                   $tf
235200131028     c                   movea     tf01          $tf
235300131028     c                   exsr      rie_$tf
235400131028     c                   endif
235500090217      * F2 - Dati DPD
235600090217     c                   if        not *in01 and *in14 and
235700140304     c                             not *in12 and not *in15
235800090217     c                   reset                   $tf
235900090217     c                   movea     tf02          $tf
236000090217     c                   exsr      rie_$tf
236100090217     c                   endif
236200180222       // F8 - Int.Bolle
236300180222     c                   if        not *in01 and not *in15
236400180222     c                             and *in50
236500180222     c                   reset                   $tf
236600180222     c                   movea     tf08          $tf
236700180222     c                   exsr      rie_$tf
236800180222     c                   endif
236900090217      * F9 - Sollecito
237000090217     c                   if        not *in01 and *in13
237100140304     c                             and not *in15
237200090217     c                   reset                   $tf
237300090217     c                   movea     tf09          $tf
237400090217     c                   exsr      rie_$tf
237500090217     c                   endif
237600130913      * F14 - Orari servizio
237700131108     c                   if        not *in01
237800131125     c                             and *in44
237900140304     c                             and not *in15
238000130913     c                   reset                   $tf
238100130913     c                   movea     tf14          $tf
238200130913     c                   exsr      rie_$tf
238300130913     c                   endif
238400131120      * F15 - Variazioni
238500131120     c                   if        not *in01 and *in40
238600140304     c                             and not *in15
238700131120     c                   reset                   $tf
238800131120     c                   movea     tf15          $tf
238900131120     c                   exsr      rie_$tf
239000131120     c                   endif
239100131028      * F16 - Gestione Richieste assistenza
239200131108     c                   if        not *in01 and *in09
239300140304     c                             and not *in15
239400131028     c                   reset                   $tf
239500131028     c                   movea     tf16          $tf
239600131028     c                   exsr      rie_$tf
239700131028     c                   endif
239800090217      * F19 - Proposte Variazioni
239900090217     c                   if        not *in01 and *in41
240000140304     c                             and not *in15
240100090217     c                   reset                   $tf
240200090217     c                   movea     tf19          $tf
240300090217     c                   exsr      rie_$tf
240400090217     c                   endif
240500090217      * F22 - Info Appuntamento
240600090217     c                   if        not *in01 and *in43
240700140304     c                             and not *in15
240800090217     c                   reset                   $tf
240900090217     c                   movea     tf22          $tf
241000090217     c                   exsr      rie_$tf
241100090217     c                   endif
241200090211      * F20 - Fasi
241300090211     c                   if        not *in01
241400140304     c                             and not *in15
241500090210     c                   reset                   $tf
241600090211     c                   movea     tf20          $tf
241700090210     c                   exsr      rie_$tf
241800090210     c                   endif
241900090210
242000090210      * Pulisco la stringa
242100090210     c                   Eval      $h = 1
242200090210     c                   For       $h by 1 to 62
242300090210     c                   If        rigatf1($h) = '#'
242400090210     c                   Clear                   rigatf1($h)
242500090210     c                   EndIf
242600090210     c                   EndFor
242700090210     c                   Eval      $h = 1
242800090210     c                   For       $h by 1 to 62
242900090210     c                   If        rigatf2($h) = '#'
243000090210     c                   Clear                   rigatf2($h)
243100090210     c                   EndIf
243200090210     c                   EndFor
243300090210     c                   Eval      $h = 1
243400090210     c                   For       $h by 1 to 62
243500090210     c                   If        rigatf3($h) = '#'
243600090210     c                   Clear                   rigatf3($h)
243700090210     c                   EndIf
243800090210     c                   EndFor
243900090210
244000090210      * Imposto la descrizione del tasto funzione F24 e segnalo
244100090210      * quante righe ho riempito e quale devo emettere
244200090210     c                   Select
244300090210
244400090210     c                   When      $k <> *Zeros and $y <> *Zeros
244500090210     c                   Eval      $loop = 3
244600090210     c                   Eval      vzfd02 = cf2413
244700090210
244800090210     c                   When      $k <> *Zeros and $y = *Zeros
244900090210     c                   Eval      $loop = 2
245000090210     c                   Eval      vzfd02 = cf2412
245100090210
245200090210     c                   Other
245300090210
245400090210     c                   Eval      $loop = 1
245500090210     c                   Clear                   vzfd02
245600090210     c                   EndSl
245700090210
245800090210      * Prima riga di tasti funzione
245900090210     c                   Movea     rigatf1       vzfd01
246000090210     c                   Eval      $rig = 1
246100090210
246200090210     c                   EndSr
246300090210
246400090210      *------------------------------------------------------------------------*
246500090210      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
246600090210      *------------------------------------------------------------------------*
246700090210     c     Rie_$tf       BegSr
246800090210      *
246900090210     c                   Eval      $x = 1
247000090210     c     '#'           Lookup    $tf($x)                                30
247100090210     c                   Add       $x            $z
247200090210     c                   If        $Z <= 63
247300090210     c                   Eval      $j = $Z - $x + 1
247400090210     c                   Movea     $tf           rigatf1($j)
247500090210     c                   Else
247600090210     c                   Add       $x            $k
247700090210     c                   If        $k <= 63
247800090210     c                   Eval      $j = $K - $x + 1
247900090210     c                   Movea     $tf           rigatf2($j)
248000090210     c                   Else
248100090210     c                   Add       $x            $y
248200090210     c                   If        $y <= 63
248300090210     c                   Eval      $j = $y - $x + 1
248400090210     c                   Movea     $tf           rigatf3($j)
248500090210     c                   EndIf
248600090210     c                   EndIf
248700090210     c                   EndIf
248800090210
248900090210     c                   EndSr
249000061017
249100001010      *****************************************************************
249200001010      * ROUTINE INIZIALE
249300001010      *****************************************************************
249400001010     C     *INZSR        BEGSR
249500001009
249600001010     C     *ENTRY        PLIST
249700001010     C                   PARM                    KPJBA
249800040220     c                   parm                    FIDNA1ds
249900001011
250000001011     C                   movel     TIT_A         VTCtit
250100001009
250200001026     C                   movel(p)  kpjbu         parm01
250300001016
250400090202     C                   Z-ADD     1             CODUT             1 0
250500090202     c     *dtaara       define    §azute        AZUTEds
250600090202     c     *dtaara       define    §datiute      dDATIUTE
250700090202     c                   in(E)     *dtaara
250800090202     c                   if        %ERROR or RSUT = *blanks
250900090202     c                   clear                   Tibs34Ds
251000090202     c                   call      'TIBS34R'
251100090202     c                   parm                    Tibs34Ds
251200090202     c                   in        *dtaara
251300090202     c                   endif
251400151001
251500151001      * abilitazione utente
251600151001     c                   clear                   wabi
251700151001     c                   eval      dUTE01 = UTEfaf
251800151001     c                   SELECT
251900151001      * carica abilitazioni del profilo
252000151001     c                   WHEN      §UTEorm <> *blanks
252100151001     c                   eval      wabi = §UTEorm
252200151001     c                   WHEN      UTEaut <> *blanks
252300151001     c                   eval      wabi = UTEaut
252400151001      * se non c'è l'abilitazione
252500151001      * --> se 1° livello, abilitazioni al terminal
252600151001      *     se 2° livello, abilitazioni al punto operativo
252700151001      *     se sede no abilitazioni
252800151001     c                   OTHER
252900151001     c                   IF        DUTlpo = '1'
253000151001     c                   eval      wabi   = 'TP'
253100151001     c                   ENDIF
253200151001     c                   IF        DUTlpo = '2'
253300151001     c                   eval      wabi   = 'PO'
253400151001     c                   ENDIF
253500151001     c                   ENDSL
253600151001
253700151001      * Reperimento delle filiali gestibili dall'utente
253800151001     c                   clear                   TRUL31DS
253900151001     c                   eval      I31abi = wabi
254000151001     c                   eval      I31cdi = DUTdis
254100151001     c                   eval      I31car = DUTare
254200151001     c                   eval      I31cpo = DUTpou
254300151001     c                   call      'TRUL31R'
254400151001     c                   parm                    KPJBA
254500151001     c                   parm                    TRUL31DS
254600151001     c                   IF        O31pog > *zeros
254700151001     c                   movea     O31pog        SKpog
254800151001     c                   ENDIF
254900041001
255000050112      * Se ambiente prova FILTRAPRD
255100050112     c                   If        %subst(knsif:7:1) = 'P'
255200050112     c                   Eval      wlib = 'FILTRAPRD '
255300050112     c                   Else
255400050112      * Se ambiente buono FILTRA201
255500050112     c                   Eval      wlib = 'FILTRA201 '
255600041001     c                   EndIf
255700010226
255800150309      * Da sede
255900150309      * apro il file delle distinte dell'ambiente di filiale
256000180314      * apro anche il file estensione bolle FIAR
256100010226
256200010226     C                   if        simfel = 0
256300010226     C                   eval      *in12 = *on
256400041001     c                   Clear                   comman
256500041001     c                   Movel(p)  cmdt(1)       comman
256600041001     c                   Eval      %Subst(comman:30:10) = wlib
256700041001     c                   Eval      comman =
256800120612     c                             %trim(comman) + '/FIDST01L)'
256900041001     c                   Call      'QCMDEXC'                            99
257000041001     c                   Parm                    comman
257100041001     c                   Parm                    lenght
257200120612     c  n99              Open      fidst01l
257300180314
257400180314     c                   Clear                   comman
257500180314     c                   Movel(p)  cmdt(2)       comman
257600180314     c                   Eval      %Subst(comman:30:10) = wlib
257700180314     c                   Eval      comman =
257800180314     c                             %trim(comman) + '/FAR4051L)'
257900180314     c                   Call      'QCMDEXC'                            99
258000180314     c                   Parm                    comman
258100180314     c                   Parm                    lenght
258200180314     c  n99              Open      fiar405l
258300041001
258400010226     C                   else
258500010226     C                   eval      *in12 = *off
258600120612     C                   open      fidst01l
258700180314     C                   open      fiar405l
258800010226     C                   endif
258900061017
259000061206      * carico sk dei p.o. DPD
259100061017     c                   clear                   xx
259200061017     c     *loval        setll     azorg01l
259300061017     c                   do        *hival
259400061017     c                   read      azorg01l
259500061017     c                   if        %eof(azorg01l)
259600061017     c                   leave
259700061017     c                   endif
259800061017     c                   if        orgfva <> *blanks  or
259900061017     c                             (orgfag <> 'F' and orgfag <> 'A')
260000061017     c                   iter
260100061017     c                   endif
260200061017     c                   eval      og143 = orgde3
260300061017     c                   if        §ogntw = 'DPD'
260400061017     c                   add       1             xx
260500061017     c                   eval      skpodpd(xx) = orgfil
260600061017     c                   endif
260700061017     c                   enddo
260800090211
260900090211      * carico tabella DPD
261000090211     c                   clear                   ds3idp
261100090211     c                   movel     '3I'          tblcod
261200090211     c                   movel(p)  'DPD'         tblkey
261300090211     c     ktab          chain     tabel00f
261400090211     c                   if        %found(tabel00f) and tblflg = *blanks
261500090211     c                   eval      ds3idp = tbluni
261600090211     c                   endif
261700001009
261800001023      * reperisce data e ora
261900001010     C                   TIME                    W0140
262000001010      * UDATE IN GGMMAAAA
262100001010     C                   MOVE      W0140         WDTGIO
262200001010      * UDATE IN AAAAMMGG
262300001010     C                   Z-ADD     WDTGIO        G02DAT
262400001010     C                   MOVEL     *BLANK        G02ERR
262500001010     C                   CALL      'XSRDA8'
262600001010     C                   PARM                    WLBDAT
262700001025     C                   Z-ADD     G02INV        DATEU
262800001010     C     *iso          MOVEL     DATEU         DATA_oggi
262900001010
263000001025     C                   z-add     dateu         §doggi
263100001016
263200001009     C     Kfnor         klist
263300001011     C                   kfld                    kpoe
263400001016     C                   kfld                    knsr
263500001009     C                   kfld                    knor
263600001016     C                   kfld                    knrv
263700001103
263800001103     C     Kforp         klist
263900001103     C                   kfld                    kpoe
264000001103     C                   kfld                    knsr
264100001103     C                   kfld                    knor
264200001103     C                   kfld                    knrv
264300001103     C                   kfld                    kdtv
264400001103     C                   kfld                    korv
264500010305
264600010305     C     Kfnorf        klist
264700010305     C                   kfld                    kpoe
264800010305     C                   kfld                    knsr
264900010305     C                   kfld                    knor
265000010305     C                   kfld                    knrv
265100010305     C                   kfld                    kdfo
265200010305     C                   kfld                    kofo
265300010305     C                   kfld                    kfao
265400090216
265500090216     c     kfnora        klist
265600090216     c                   kfld                    kpoe
265700090216     c                   kfld                    knsr
265800090216     c                   kfld                    knor
265900090216     c                   kfld                    knrv
266000090216     c                   kfld                    kfar
266100010119
266200010119     C     Ktab          klist
266300010119     C                   kfld                    codut
266400010119     C                   kfld                    kcod
266500010119     C                   kfld                    kkey
266600001030
266700120612     C     Kfidst        klist
266800120612     c                   kfld                    knpg
266900001030     C                   kfld                    knfv
267000001030     C                   kfld                    kfgs
267100021202
267200021202     C     Kfapd         klist
267300021202     C                   kfld                    kpdtip
267400021202     C                   kfld                    kpdpdr
267500061017
267600061017     c     kfnorn        klist
267700061017     c                   kfld                    ornpoe
267800061017     c                   kfld                    ornnsr
267900061017     c                   kfld                    ornnor
268000061017     c                   kfld                    ornnrv
268100061017     c                   kfld                    ornfar
268200061017
268300150526      * File FITGD32L
268400150526     c     K02TGD32      klist
268500040220     c                   kfld                    TGDtor
268600040220     c                   kfld                    TGDogg
268700001009
268800001011     C                   eval      kpoe = ppoe
268900001016     C                   eval      knsr = pnsr
269000001011     C                   eval      knor = pnor
269100001016     C                   eval      knrv = pnrv
269200001011
269300001025     C                   eval      v1cpoe = ppoe
269400001016     C                   eval      v1nsr  = pnsr
269500001025     C                   eval      v1nor  = pnor
269600001025     C                   eval      v1nrv  = pnrv
269700001115
269800001115     C                   movel     *zeros        v1ccra
269900001115     C                   movel     *zeros        v1ccor
270000001115     C                   movel     *zeros        v1ccrc
270100001006
270200001006     C                   endsr
270300040220      *****************************************************************
270400040220      * Abilitazione tasti funzionali
270500040220      *****************************************************************
270600040220     c     Abil_Fxx      begsr
270700040220
270800140304      * abilito i tasti funzione seguenti solo se non sono in
270900140304      * Interrogazione Storico Variazioni o Interrogazione Proposte Variazione
271000140304     c                   IF        not *in01 and not *in15
271100140304
271200040220      *
271300040220      *?Abilito F1=Aggiunta in Reclami?
271400040220      * (se richiamato da pgm. FIDNA1R...)
271500040303     c                   eval      *in08  =    (SDSprm >  1
271600040309     c                                      and IA1ins = ' ')
271700040220      *
271800040220      *?Abilito F16=Visualizzazione Reclami?
271900040220      * (se rilevo dei reclami già immessi)
272000040220     c                   eval      TGDtor  = 'O'
272100040220     c                   eval      TGDogg  = ds_OggORM
272200150526     c     K02TGD32      setll     FITGD000
272300150526     c                   eval      *in09   = (%equal(FITGD32L)   and
272400040303     c                                           (SDSprm =  1
272500040303     c                                        or (SDSprm >  1
272600040303     c                                        and IA1ins <> 'N')))
272700070509
272800151002       //?Abilito F9=Sollecito
272900151002       //?Se non è utente di sede
273000151002       //?se utente INT
273100151002       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
273200151002         IF  %subst(knmus:1:3) = 'INT' and DUTpou <> 046;
273300151002           xx = 1;
273400151002           FOR xx by 1 to %elem(skpog);
273500151002             IF  skpog(xx) <> *zeros and
273600151002                 %lookup(%dec(skpog(xx):3:0):skpoDPD) > 0;
273700151002               *in13 = *on;
273800151002               leave;
273900151002             ENDIF;
274000151002           ENDFOR;
274100151002         ENDIF;
274200180222
274300180314       //?Abilito F8=Int.Bolle
274400180309       //Se utente di sede non abilito la chiamata all'interrogazione bolle di filiale
274500180222       //Se tra i pgm richiamati NON c'è già l'interrogazione bolle abilito F8=Int.Bolle
274600180314       //ma solo se ho almeno una bolla legata a questo ORM
274700180222         *in50 = *on;
274800180309         IF DUTpou = 046;
274900180309           *in50 = *off;
275000180309         ELSE;
275100180309           XRTVSTKR (pStack:pStartPoint:pEntries);
275200180309           dsStack = pStack;
275300180309           IF  %lookup('FNLRU6R   ':skStack) > 0 or
275400180309               %lookup('FNLRU6R1  ':skStack) > 0;
275500180309             *in50 = *off;
275600180309           ENDIF;
275700180314           IF *in50;
275800180314             AR4trc = 'M';
275900180314             AR4n14 = %editc(ORMpoe:'X') + %editc(ORMnsr:'X') +
276000180314                      %editc(ORMnor:'X') + %editc(ORMnrv:'X');
276100180314             chain (AR4trc:AR4n14) FIAR405L;
276200180314             IF not %found(FIAR405L);
276300180314               *in50 = *off;
276400180314             ENDIF;
276500180314           ENDIF;
276600180309         ENDIF;
276700061019
276800061023      *?Abilito F2=Dati DPD
276900080430      * se non provengo già dal pgm fieu55r
277000061024     c                   if        psce <> 'D'
277100061019     c     ormpoe        lookup    skpodpd                                30
277200061019     c  n30ormpor        lookup    skpodpd                                30
277300140304     c                   eval      *in14 = (*in30 and not *in12)
277400061024     c                   endif
277500140304     c                   ENDIF
277600140121
277700140121      /free
277800140121         *in44 = *off;
277900140526       //?F14 - Orari servizio
278000140304       //?Abilito solo se NON sono in
278100140304       //?Interrogazione Storico Variazioni
278200140304       //?Interrogazione Proposte Variazioni
278300140304         IF  not *in01 and not *in15;
278400140317           *in44 = *on;
278500140304         ENDIF;
278600140304
278700140121      /end-free
278800040220
278900040220     c                   EndSR
279000001010      *---------------------------------------------------------------------------------------------
279100041001** cmdt
279200120612OVRDBF FILE(FIDST01L) TOFILE(
279300180314OVRDBF FILE(FIAR405L) TOFILE(
279400001031** MSG  Lungh. 78                                                            *
279500001102Non esistono variazioni per l'ORM selezionato                                       1
279600001102Non esistono fasi per l'ORM selezionato                                             2
279700001102Non esistono proposte per l'ORM selezionato                                         3
279800061017L'ORM non è stato stampato impossibile sollecitare                                  4
279900061017Impossibile fare sollecito per ORM non automatizzato                                5
280000061017Impossibile fare sollecito per ORM già fatto o chiuso                               6
280100171114Impossibile sollecitare. Depot xxxxxxx non associato ad un cliente                  7
280200090223Non esistono appuntamenti per l'ORM selezionato                                     8
280300131028** TF01
280400131211F1=Agg.Rich.  #
280500090211** TF02
280600131211F2=Dati DPD  #
280700180222** TF08
280800180222F8=Int.Bolle  #
280900180222** TF09
281000131211F9=Sollecito  #
281100130913** TF14
281200131211F14=OrariSr  #
281300131120** TF15
281400131211F15=Variazioni  #
281500131028** TF16
281600131211F16=Gest.Rich.  #
281700140110** TF19
281800131211F19=Proposte Var.  #
281900090217** TF20
282000131211F20=Fasi  #
282100090217** TF22
282200131211F22=Info App.  #
