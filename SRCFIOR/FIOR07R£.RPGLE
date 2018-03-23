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
004200061003      * 99 - errore QCMDEXEC
004300001010      ****************************************************************
004400001010
004500001030     FFNORM01L  IF   E           K DISK
004600070911     ffnorg01l  if   e           k disk
004700001030     FFNORV01L  IF   E           K DISK
004800131029     fFNORVE1L  if   e           k disk
004900001031     FFNORF01L  IF   E           K DISK
005000001031     FFNORP01L  IF   E           K DISK
005100131029     fFNORPE1L  if   e           k disk
005200061017     FFNORN02L  IF a E           K DISK
005300090211     ffnora01l  if   e           k disk
005400140409     ffnore01l  if   e           k disk
005500140121     fFNORT01L  if   e           k disk
005600140303     fFNORPT1L  if   e           k disk
005700001011     FAZORG01L  IF   E           K DISK
005800010119     FTABEL00F  IF   E           K DISK
005900120612     Ffidst01l  IF   E           K DISK    usropn
006000021202     FFIAPD01L  IF   E           K DISK
006100150526     fFITGD32L  if   e           k disk
006200001030     FFIOR07D   CF   E             WORKSTN  sfile(fior07s:nrr)
006300001031     F                                      sfile(fior07sf:nrrf)
006400001031     F                                      sfile(fior07sp:nrrp)
006500090223     F                                      sfile(fior07aps:nrra)
006600001010
006700001010      *------------------------------------------------------------------------*
006800001011     D kpoe            S                   LIKE(V1cpoe)
006900001016     D knsr            S                   LIKE(V1nsr)
007000001006     D knor            S                   LIKE(V1nor)
007100001016     D knrv            S                   LIKE(V1nrv)
007200001103     D kdtv            S                   LIKE(ORPdtv)
007300001103     D korv            S                   LIKE(ORPorv)
007400010305     D kdfo            S                   LIKE(ORMdfo)
007500010305     D kofo            S                   LIKE(ORMofo)
007600010305     D kfao            S                   LIKE(ORMfao)
007700090216     d kfar            s                   LIKE(ORMfao)
007800001030
007900120612     D knpg            S                   LIKE(DSTnpg) inz(4)
008000001030     D knfv            S                   LIKE(DSTnfv)
008100001030     D kfgs            S                   LIKE(DSTfgs)
008200021202
008300021202     D kpdpdr          S                   LIKE(APDpdr)
008400040908     D kpdtip          S                   LIKE(APDtip) inz('A')
008500001026
008600001026     D kazorg          S                   LIKE(ORGfil)
008700010119
008800010119     D kcod            S                   LIKE(TBLcod)
008900010119     D kkey            S                   LIKE(TBLkey)
009000010119     D kdes            S             30
009100001030
009200001030     D §doggi          S                   LIKE(v1dar)
009300001025     D §kpjbu          S                   LIKE(KPJBU)
009400151001     d wabi            s                   like(UTEaut)
009500151001     d w0030           s                   like(ORGfil)
009600041001
009700041001     d wlib            s             10
009800041001     d comman          s             80
009900041001     d lenght          s             15  5 inz(80)
010000001009
010100001006     D W0140           S             14  0
010200001006     D Wdtgio          S              8  0
010300001006     D dateu           S              8  0
010400001009
010500001006     D DATA_oggi       S               D   DATFMT(*eur)
010600061017
010700061017     d wprg            s                   like(ornprg)
010800061017     d xx              s              3  0
010900090210     d rigatf1         s              1    dim(78)
011000090210     d rigatf2         s              1    dim(62)
011100090210     d rigatf3         s              1    dim(62)
011200090210     d $loop           s              1  0
011300090210     d $rig            s              1  0
011400090211     d $h              s              3  0
011500090210     d $j              s              3  0
011600090210     d $k              s              3  0
011700090211     d $x              s              3  0
011800090210     d $y              s              3  0
011900090210     d $z              s              3  0
012000131028
012100131028     d wEndF05         s               n
012200140206
012300140206     D network         S                   LIKE(ORGdit)
012400001006
012500001010      *  titolo videata (lunghezza massima 34)
012600010223     D TIT_A           C                   CONST('***   INTERROGAZIONE O.R.M.  -
012700001010     D                                       ***')
012800001031
012900001031      *   S C H I E R E
013000041001     d cmdt            s             60    dim(1) ctdata perrcd(1)
013100061017     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
013200061017     d skpodpd         s              3  0 dim(250)                             p.o. DPD
013300151001     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
013400131028     d tf01            s              1    dim(25) ctdata perrcd(25)
013500090211     d tf02            s              1    dim(25) ctdata perrcd(25)
013600090211     d tf09            s              1    dim(25) ctdata perrcd(25)
013700130913     d tf14            s              1    dim(25) ctdata perrcd(25)
013800131120     d tf15            s              1    dim(25) ctdata perrcd(25)
013900131028     d tf16            s              1    dim(25) ctdata perrcd(25)
014000090211     d tf19            s              1    dim(25) ctdata perrcd(25)
014100090211     d tf20            s              1    dim(25) ctdata perrcd(25)
014200090211     d tf22            s              1    dim(25) ctdata perrcd(25)
014300090210     d $tf             s              1    dim(25)
014400001006
014500001010      *   D S   I N T E R N E / E S T E R N E
014600001009
014700001009     D WLBDAT          DS                  INZ
014800001009     D  G02DAT                 1      8  0
014900001009     D  G02INV                 9     16  0
015000001009     D  G02ERR                17     17
015100001009     D  G02TGI                18     22  0
015200001020
015300001023     D                 DS
015400001023     D  v1cra1                 1      3  0
015500001023     D  v1cra2                 4      7  0
015600001023     D  v1cra3                 8     10  0
015700001023     D  v1ccra                 1     10  0
015800001020
015900001023     D                 DS
016000001023     D  v1cor1                 1      3  0
016100001023     D  v1cor2                 4      7  0
016200001023     D  v1cor3                 8     10  0
016300001023     D  v1ccor                 1     10  0
016400001020
016500001023     D                 DS
016600001023     D  v1crc1                 1      3  0
016700001023     D  v1crc2                 4      7  0
016800001023     D  v1crc3                 8     10  0
016900001023     D  v1ccrc                 1     10  0
017000001031
017100001031     D                 DS
017200001031     D  v2dtvg                 1      2
017300001031     D  v2dtvm                 3      4
017400001031     D  v2dtva                 5      8
017500001031     D  v2dtv                  1      8  0
017600001031
017700001031     D                 DS
017800001031     D  v2orvo                 1      2
017900001031     D  v2orvm                 3      4
018000001031     D  v2orvs                 5      6
018100001031     D  v2orv                  1      6  0
018200001031
018300001031     D                 DS
018400001031     D  v4dtvg                 1      2
018500001031     D  v4dtvm                 3      4
018600001031     D  v4dtva                 5      8
018700001031     D  v4dtv                  1      8  0
018800001031
018900001031     D                 DS
019000001031     D  v4orvo                 1      2
019100001031     D  v4orvm                 3      4
019200001031     D  v4orvs                 5      6
019300001031     D  v4orv                  1      6  0
019400061017
019500061017     d                 ds
019600061017     d  ds_giorno              1      2  0
019700061017     d  ds_mese                3      4  0
019800061017     d  ds_anno                5      8  0
019900061017     d  ds_data                1      8  0
020000001009
020100001009     D Parm01          DS
020200001016     D  ppoe                          3  0
020300001016     D  pnor                          7  0
020400001016     D  pnsr                          2  0
020500001016     D  pnrv                          2  0
020600001016     D  psce                          1
020700001018     D  pfgs                          3  0
020800010205     D  ppor                          3  0
020900010205     D  pdtr                          8  0
021000010205     D  pmdb                         10
021100010205     D  pprb                         10
021200010205     D  pdts                          8  0
021300010205     D  prmp                          1
021400010205     D  pbrc                          1
021500010205     D  pref                          2
021600010205     D  pmio                          1
021700010205      * Da int.proposte
021800010109     D  pdtv                          8  0
021900010109     D  porv                          6  0
022000010109     D  putv                         10
022100010109     D  pric                          1
022200040220      *
022300040220      *?definizione oggetto reclamo come O.R.M.?
022400040220     d ds_OggORM       ds
022500040220     d  ORMpoe
022600040220     d  ORMnsr
022700040220     d  ORMnor
022800040220     d  ORMnrv
022900001009
023000040908     d dFar          e ds
023100010301     D DCMR          E DS
023200001010     D TIBS02DS      E DS
023300090202     d ds5P          e ds                  inz
023400090202     d   §5Ppgm      e                     inz('FIOR18R   ')
023500001030     D FIOR06DS      E DS
023600061017     d fior16ds      e ds                  inz
023700030606     d Dorm01        e ds
023800081125     d dorf01        e ds
023900060515     d fnlv55ds      e ds
024000061003     d og143         e ds
024100061017     d trul90ds      e ds
024200061019     d fieu55ds      e ds
024300070911     d fidg09ds      e ds
024400171025     d tisid4ds      e ds                  inz
024500171025     d truldepds     e ds                  inz
024600171025     d truldepds1    e ds                  inz
024700090211     d ds3idp        e ds
024800140206     d TISI95DS      e ds
024900151001     d dUTE01        e ds
025000001010
025100001010      *
025200001010     D KPJBA         E DS
025300040220     D FIDNA1ds      e ds
025400040226      *
025500040226     d FIDNA2ds      e ds                  inz
025600040226     d  IA2mod       e                     inz('M')
025700040226     d  IA2tor       e                     inz('O')
025800130913
025900130913      * - Parametri x Interrogazione orari servizio
026000140317     d TRULORSds     e ds
026100090202
026200090202      * - Parametri x Controllo profilo utenti
026300090202     d TIBS34ds      e ds
026400090202      * - Ds di riferimento al file esterno AZUTE00F
026500090202     d AzuteDs       e ds                  extname(AZUTE00F)
026600090202      * - Ds per dati organigramma
026700090202     d dDatiUte      e ds
026800140110
026900140110      // ds per note AUT
027000140110     d FIORT1DS      e ds
027100131028
027200131028      // ds per rcd 'O' FNORE - orari apertura
027300131028     d dOREorari     e ds
027400140429
027500140429      // ds per rcd 'G' FNORE - generale
027600140429     d dOREgen       e ds
027700140429
027800140429      // ds per rcd 'S' FNORE - SMS
027900140429     d dOREsms       e ds
028000161108
028100161108      // ds per rcd 'DT' FNORE - Data Pronta Merce
028200161108     d dOREdt        e ds
028300161115
028400161115      // ds per campo IOREflo - TRULORSDS
028500161115     d dIORE01       e ds
028600140318
028700140318      // ds per recupero ora presunta ritiro da PDA
028800140318     d FIOR01DS      e ds
028900151001
029000151001      // Carico filiali gestite dall'utente
029100151001     d TRUL31DS      e ds
029200090202
029300001010      *
029400001010     D                SDS
029500040220     d  SDSprm           *parms
029600001010     D  VTCPGM                 1     10
029700090210
029800090210      *   C O S T A N T I
029900090210     d cf2413          c                   const('F24=AlTasti(1/3)')
030000090210     d cf2423          c                   const('F24=AlTasti(2/3)')
030100090210     d cf2433          c                   const('F24=AlTasti(3/3)')
030200090210     d cf2412          c                   const('F24=AlTasti(1/2)')
030300090210     d cf2422          c                   const('F24=AlTasti(2/2)')
030400140110
030500130913      //---------------------------------------------------------------
030600130913      //?Definizione procedure utilizzate.
030700130913      //---------------------------------------------------------------
030800140110
030900140110      // - Gestione note AUT
031000140110     d fiort1r         pr                  extpgm('FIORT1R')
031100140110     d  kpjba                              likeds(KPJBA)
031200140110     d  fiort1ds                           likeds(FIORT1DS)
031300140320
031400140320      // - Gestione note AUT x proposte
031500140320     d fiort1c         pr                  extpgm('FIORT1C')
031600140320     d  kpjba                              likeds(KPJBA)
031700140320     d  fiort1ds                           likeds(FIORT1DS)
031800130913
031900130913      // -Interrogazione orari di servizio
032000140317     d trulorsr        pr                  extpgm('TRULORSR')
032100130913     d  kpjba                              likeds(KPJBA)
032200140317     d  trulorsds                          likeds(TRULORSDS)
032300140206
032400140206      // - Cappario
032500140206     d tisi95r         pr                  extpgm('TISI95R')
032600140206     d  tisi95ds                           likeds(TISI95DS)
032700140318
032800140318      // - Recupero ora presunta ritiro
032900140318     d fior01r         pr                  extpgm('FIOR01R')
033000140318     d  fior01ds                           likeds(FIOR01DS)
033100131125
033200131125      //---------------------------------------------------------------
033300131125      //?Definizione prototipi utilizzati.
033400131125      //---------------------------------------------------------------
033500131125
033600131125      /copy gaitrasrc/srcprotopr,tibs02r
033700161020      /copy gaitrasrc/srcprotopr,XSRDA8
033800131028
033900131028      //---------------------------------------------------------------
034000131028      //?Definizione key-list.
034100131028      //---------------------------------------------------------------
034200131028
034300131028       // - File FNORE01L
034400131029     d k05fnore      e ds                  extname(FNORE01L:*key)
034500131028     d                                     prefix(k_)
034600131028     d                                     inz
034700131029
034800131029       // - File FNORVE1L
034900131029     d k07fnorve     e ds                  extname(FNORVE1L:*key)
035000131029     d                                     prefix(k_)
035100131029     d                                     inz
035200131029
035300131029       // - File FNORPE1L
035400131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
035500131029     d                                     prefix(k_)
035600131029     d                                     inz
035700001010
035800001006      *------------------------------------------------------------------------*
035900001103
036000001030      * Carica i dati dal file FNORM
036100001030     C     kfnor         chain     fnorm01L
036200001115     C                   if        %found(fnorm01l)
036300001115      * Annullato
036400001115     C                   if        ORMatb <> *blanks
036500001115     C                   movel     'ANNULLATO'   v1ann
036600001115     C                   else
036700001115     C                   clear                   v1ann
036800001115     C                   endif
036900001103      * Se è richiamato con 'P' deve solo gestire le proposte
037000001103     C                   if        psce = 'P'
037100001103     C                   goto      richiama
037200001103     C                   endif
037300010315
037400010109     C                   else
037500010109     C                   if        psce = 'P' and pric = 'P'
037600010109     C                   movel     'E'           psce
037700010109     C                   movel     parm01        kpjbu
037800010109     C                   goto      richiama
037900010109     C                   endif
038000001009     C                   endif
038100040220
038200040220      * Abilito eventuali tasti funzionali
038300140304     c                   exsr      Abil_Fxx
038400001006
038500001006      * Emissione 1° Videata
038600001010
038700001006     C                   do        *hival
038800120612     c                   eval      *in01 = *off
038900140303     c                   eval      *in15 = *off
039000010315     C                   exsr      Sr_Carvid
039100010315     C                   exsr      Sr_Decodifica
039200090211     c                   ExSr      Sr_Tastifun
039300001010
039400061017     c     for01         tag
039500001031     C                   eval      v1mod = 'VISUALIZZAZIONE'
039600001031
039700001103     C                   setoff                                       010203
039800140304     c                   setoff                                       1715
039900040908     c                   Eval      *In10 = *Off
040000001030     C                   exfmt     fior071
040100001031     C                   setoff                                       28
040200001011
040300001011      * F12=Ritorno
040400001023     C                   if        *inkl = *on
040500001023     C                   leave
040600001023     C                   endif
040700040220
040800040220      *
040900040220      *?F1 - Aggiunta bolla in reclamo?
041000040226if  1c                   if        *inKA  = *on
041100040220     c                   clear                   FIDNA1ds
041200040220     c                   eval      IA1ins = 'S'
041300040220     c                   eval      IA1tor = 'O'
041400040220     c                   eval      IA1ogg = ds_OggORM
041500040226     c                   leave
041600040220e   1c                   endif
041700040226      *
041800040226      *?F16 - Gestione reclami?
041900040226if  2c                   if        *inKQ  = *on
042000040226     c                   reset                   FIDNA2ds
042100040226if  3c                   if        SDSprm > 1
042200040226     c                   eval      IA2mod = 'I'
042300040226e   3c                   endif
042400040226     c                   eval      IA2ogg = ds_OggORM
042500040226     c                   call      'FIDNA2R'
042600040226     c                   parm                    KPJBA
042700040226     c                   parm                    FIDNA2ds
042800040226if  3c                   if        OA2f03 = 'S'
042900040226     c                   leave
043000040226e   3c                   endif
043100040226e   2c                   endif
043200130913
043300130913      /free
043400130913
043500130913       //?F14=Interrogazione oraro di servizio
043600130913       IF  *inkn;
043700130913         exsr F14D01;
043800130913         iter;
043900130913       ENDIF;
044000131028
044100131028       //?F5=Altri dati
044200131028       IF  *inke;
044300131028         exsr F05D01;
044400131028         iter;
044500131028       ENDIF;
044600140110
044700140110       //?F17=Note AUT
044800140110       IF  *inkr;
044900140110         exsr F17D01;
045000140110         iter;
045100140110       ENDIF;
045200130913
045300130913      /end-free
045400001030
045500010402      * F20=Fasi
045600010402     C                   if        *inku = *on
045700001031     C                   exsr      Sr_Fasi
045800001030     C                   endif
045900120612
046000131120      * F15=Variazioni
046100131120     C                   if        *inkp = *on
046200001102     C                   exsr      Sr_Variazioni
046300001030     C                   endif
046400001011
046500001025      * F18=Note
046600001025     C                   if        *inks = *on
046700001025     C                   exsr      Sr_Note
046800001025     C                   endif
046900001020      * F19=Proposte
047000001030     C                   if        *inkt = *on
047100001031     C                   exsr      Sr_Proposte
047200001020     C                   endif
047300061017      * F9=Sollecito
047400061017     c                   if        *inki
047500061017     c                   exsr      sr_sollecito
047600061017     c                   endif
047700061023      * F2=Dati DPD
047800061023     c                   if        *inkb
047900061019     c                   exsr      sr_datidpd
048000061019     c                   endif
048100090211      * F22=Info appuntamento
048200090211     c                   if        *inkw
048300090211     c                   exsr      sr_ormapp
048400090211     c                   endif
048500090210      * F24=Altri tasti  ?
048600090210     c                   if        *inky and $loop > 1
048700090210     c                   exsr      sr_f24
048800090210     c                   goto      for01
048900090210     c                   endif
049000001010
049100001006     C                   enddo
049200001023
049300001103     C     richiama      tag
049400001103
049500010109     C                   if        psce = 'P'
049600010109     C                   if        pric = *blanks
049700001103     C                   exsr      Sr_Proposte
049800010109     C                   endif
049900010109
050000010109     C                   if        pric = 'P'
050100010109     C                   exsr      Sr_Cardatiprop
050200010109     C                   exsr      Sr_Visproposte
050300010109     C                   endif
050400001103     C                   endif
050500001010
050600001006     C                   eval      *inlr = *on
050700001009      **********************************************************************
050800001023      * CARICA DATI NELLA VIDEATA DAL FILE
050900001009      **********************************************************************
051000001009     C     Sr_Carvid     begsr
051100001009
051200001129     C                   movel     ORMtco        v1ctco
051300001016     C                   movel     ORMtor        v1ctor
051400001023      * Inversione data emissione ORM
051500001016     C                   clear                   wlbdat
051600001017     C                   z-add     ORMdao        G02inv
051700001017     C                   movel     '3'           G02err
051800001016     C                   call      'XSRDA8'
051900001016     C                   parm                    wlbdat
052000001025     C                   z-add     G02dat        v1dao
052100001215     C                   movel     ORMoao        v1oao
052200001016      * Dati Ordinante
052300001025     C                   z-add     ORMcor        v1ccor
052400001025     C                   movel     ORMrso        v1rso
052500001025     C                   movel     ORMino        v1ino
052600001025     C                   movel     ORMcao        v1cao
052700001025     C                   movel     ORMloo        v1loo
052800001025     C                   movel     ORMpro        v1pro
052900001025     C                   movel     ORMnao        v1nao
053000001016      * Dati Ritiro a
053100001025     C                   z-add     ORMcra        v1ccra
053200001025     C                   movel     ORMrsr        v1rsr
053300001025     C                   movel     ORMinr        v1inr
053400001025     C                   movel     ORMcar        v1car
053500001025     C                   movel     ORMlor        v1lor
053600001025     C                   movel     ORMprr        v1prr
053700001025     C                   movel     ORMnar        v1nar
053800001016     C                   movel     ORMrer        v1rer
053900001016     C                   movel     ORMter        v1ter
054000001016      * Dati Consegna a
054100001025     C                   z-add     ORMcrc        v1ccrc
054200001016     C                   movel     ORMrsc        v1rsc
054300001016     C                   movel     ORMinc        v1inc
054400001016     C                   movel     ORMloc        v1loc
054500001016     C                   movel     ORMcac        v1cac
054600001016     C                   movel     ORMprc        v1prc
054700001016     C                   movel     ORMnac        v1nac
054800001016
054900010315     C                   if        ORMffd = 'S'
055000010315     C                   eval      v1ffd = 'SI'
055100010315     C                   else
055200010315     C                   eval      v1ffd = 'NO'
055300010315     C                   endif
055400010315
055500001016     C                   movel     ORMnam        v1nam
055600001023      * Inversione data ritiro
055700001017     C                   z-add     ORMdar        G02inv
055800001017     C                   movel     '3'           G02err
055900001016     C                   call      'XSRDA8'
056000001016     C                   parm                    wlbdat
056100001025     C                   z-add     G02dat        v1dar
056200001016     C                   movel     ORMorr        v1orr
056300001016
056400001025     C                   z-add     ORMncl        v1ncl
056500001025     C                   z-add     ORMpkg        v1pkg
056600001025     C                   z-add     ORMvlm        v1vlm
056700001025     C                   z-add     ORMbnc        v1bnc
056800001025     C                   z-add     ORMblc        v1blc
056900001025     C                   z-add     ORMatt        v1att
057000001025     C                   z-add     ORMmtc        v1mtc
057100001016
057200010315     C                   if        ORMpag = 'M'
057300010315     C                   eval      v1pag = 'Mitt.'
057400010315     C                   endif
057500010315     C                   if        ORMpag = 'D'
057600010315     C                   eval      v1pag = 'Dest.'
057700010315     C                   endif
057800010315     C                   if        ORMpag = 'O'
057900010315     C                   eval      v1pag = 'Ord. '
058000010315     C                   endif
058100010315
058200001025     C                   z-add     ORMksc        v1ksc
058300001020     C                   movel     ORMctr        v1ctr
058400010315
058500010315     C                   if        ORMddt = 'S'
058600010315     C                   eval      v1ddt = 'SI'
058700010315     C                   endif
058800010315     C                   if        ORMddt = 'N'
058900010315     C                   eval      v1ddt = 'NO'
059000010315     C                   endif
059100010315
059200001016     C                   movel     ORMpor        v1cpor
059300001016     C                   movel     ORMpoc        v1cpoc
059400001016     C                   movel     ORMno1        v1not1
059500001016     C                   movel     ORMno2        v1not2
059600001025     C                   z-add     ORMfao        v1fao
059700001025     C                   z-add     ORMpos        v1cpos
059800001025     C                   z-add     ORMors        v1ors
059900001027     C                   movel     ORMrfa        v1rfa
060000001221     C                   move      ORMsto        v1sto
060100001030     C                   z-add     ORMndc        v1ndc
060200001030      * Inversione data distinta
060300001030     C                   clear                   wlbdat
060400001030     C                   z-add     ORMddc        G02inv
060500001030     C                   movel     '3'           G02err
060600001030     C                   call      'XSRDA8'
060700001030     C                   parm                    wlbdat
060800001030     C                   z-add     G02dat        v1ddc
060900010226
061000010226      * Cerca codice autotrasportatore
061100071108     c                   if        ormndc > 0
061200001030     C                   z-add     ORMndc        knfv
061300001030     C                   z-add     ORMpor        kfgs
061400120612     C     kfidst        chain     fidst01l
061500060515      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
061600120612     c                   if        not %found(fidst01l)
061700060515     c                   clear                   fnlv55ds
061800060515     c                   eval      d55tpt = '6'
061900060515     c                   move      ormpor        d55lin
062000060515     c                   eval      d55drf = ormddc
062100060515     c                   call      'FNLV55R'
062200060515     c                   parm                    fnlv55ds
062300060515     c                   if        d55err = *blanks and d55tfa <> d55lin
062400060515     c                   move      d55tfa        kfgs
062500120612     c     kfidst        chain     fidst01l
062600060515     c                   endif
062700060515     c                   endif
062800120612     C                   if        %found(fidst01l)
062900001030     C                   eval      v1cpdc = DSTpdr
063000001030     C                   endif
063100071108     c                   else
063200071108     c                   clear                   v1cpdc
063300071108     c                   endif
063400071108
063500010226      * Autotrasportatore evidenziato a video
063600010226     C                   eval      *in06 = (v1cpdc > 0)
063700010226
063800010219      * Distinta evidenziata a video
063900010219     C                   eval      *in05 = (v1ndc > 0)
064000070924
064100070924      * se Stampato faccio vedere la scritta
064200070924     c                   eval      v1dst = ormdst
064300070924     c                   eval      *in16 = (v1dst > 0)
064400010119
064500010315     C                   if        ORMspi = 'S'
064600010315     C                   eval      v1spi = 'SI'
064700010315     C                   else
064800010315     C                   eval      v1spi = 'NO'
064900010315     C                   endif
065000010309      * Causale
065100010305     C                   exsr      Sr_Carcau
065200010322
065300010322     C                   eval      *in40 = *off
065400010322     C                   eval      *in41 = *off
065500010322     C                   eval      *in42 = *off
065600090211     C                   eval      *in43 = *off
065700131211     C                   eval      *in46 = *off
065800140121     C                   eval      *in48 = *off
065900131211
066000010322      * Controllo se ci sono Variazioni
066100010322     C     kfnor         setll     fnorv01l
066200010322     C                   do        *hival
066300010322     C     kfnor         reade     fnorv01l
066400010322     C                   if        %eof
066500010322     C                   leave
066600010322     C                   endif
066700010322     C                   eval      *in40 = *on
066800010322     C                   leave
066900010322     C                   enddo
067000010322      * Controllo se ci sono Proposte
067100010322     C     kfnor         setll     fnorp01l
067200010322     C                   do        *hival
067300010322     C     kfnor         reade     fnorp01l
067400010322     C                   if        %eof
067500010322     C                   leave
067600010322     C                   endif
067700010322     C                   eval      *in41 = *on
067800010322     C                   leave
067900010322     C                   enddo
068000010322      * Controllo se ci sono Note
068100061017     C     kfnor         setll     fnorn02l
068200010322     C                   do        *hival
068300061017     C     kfnor         reade     fnorn02l
068400010322     C                   if        %eof
068500010322     C                   leave
068600010322     C                   endif
068700010322     C                   eval      *in42 = *on
068800010322     C                   leave
068900010322     C                   enddo
069000090211
069100090211      * controllo se inserite info appuntamento
069200090211     c     kfnor         setll     fnora01l
069300090211     c                   do        *hival
069400090211     c     kfnor         reade     fnora01l
069500090211     c                   if        %eof
069600090211     c                   leave
069700090211     c                   endif
069800090211     c                   eval      *in43 = *on
069900090211     c                   leave
070000090211     c                   enddo
070100030606
070200030606     C                   eval      dorm01 = ORMflo
070300030606
070400030606      * ORM commissionato
070500030606     c                   If        §orCom = 'S'
070600030606     c                   Eval      V1dCom = 'COMMISSIONATO'
070700030606     c                   Else
070800030606     c                   Clear                   V1dCom
070900030606     c                   EndIf
071000050502
071100050502      * ORM con Ricevuta di Ritiro
071200050502     c                   Eval      *In11 = (§orsrm = 'S')
071300100225
071400100225      /free
071500140429       //?Imposto costante se ORM con preavviso (CEVA)
071600100225       *in18 = *off;
071700100225       IF  §ormpre <> *blanks;
071800100225         *in18 = *on;
071900100225         IF  §ormpre = 'M';
072000100225         v1este = 'Preavviso: '+
072100100225                  'Mail';
072200100225         ENDIF;
072300100225       ENDIF;
072400100225      /end-free
072500061017
072600061017     c                   eval      v1cfcs = ormfcs
072700070911      * giro
072800070911     c                   clear                   v1ccgi
072900070911     c     kfnor         chain     fnorg01l
073000070911     c                   if        %found(fnorg01l)
073100070911     c                   eval      v1ccgi = orgcgi
073200070911     c                   endif
073300131028
073400131028      /free
073500131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
073600131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
073700131211           *in46 = *on;
073800131211         ENDIF;
073900160513
074000160513         k_OREpoe = ORMpoe;
074100160513         k_OREnsr = ORMnsr;
074200160513         k_OREnor = ORMnor;
074300160513         k_OREnrv = ORMnrv;
074400160513
074500160513       //?Recupero dati da estensione FNORE
074600160513       //?Rcd 'G ' --> Generale
074700160513         k_OREtrc = 'G';
074800160513         clear dOREgen;
074900160513         chain  %kds(k05fnore) FNORE01L;
075000160513         IF  %found(FNORE01L);
075100160513           dOREgen = OREdati;
075200160513         ENDIF;
075300160513
075400131028       //?Recupero dati da estensione FNORE
075500131028       //?Rcd 'O ' --> Orari apertura
075600131028         k_OREtrc = 'O ';
075700131028         clear dOREorari;
075800131028       //?Aggancio il rcd
075900140429         chain  %kds(k05fnore) FNORE01L;
076000131028         IF  %found(FNORE01L);
076100131028           dOREorari = OREdati;
076200131028         ENDIF;
076300131028         v1OraAMda = §OREoramda;
076400131028         v1OraAMa  = §OREorama;
076500131028         v1OraPMda = §OREorapda;
076600131028         v1OraPMa  = §OREorapa;
076700161020
076800161020       //?Rcd 'DT' --> Data Pronta Merce
076900161020         k_OREtrc = 'DT';
077000161108         clear dOREdt;
077100161020         chain  %kds(k05fnore) FNORE01L;
077200161020         IF  %found(FNORE01L);
077300161108           dOREdt = OREdati;
077400161020         ENDIF;
077500161108         IF  §OREdpm > *zeros;
077600161020           clear wlbdat;
077700161108           G02inv = §OREdpm;
077800161020           G02err = '3';
077900161020           xsrda8 (wlbdat);
078000161020           V1dpm = G02dat;
078100161020         ENDIF;
078200140429
078300140429       //?Imposto costante se ORM con Alert via Mail/SMS
078400160513         IF  §OREfimo = 'S' or §OREfiso = 'S';
078500160513           IF  not *in18;
078600160513             V1este = 'Preavviso: Mail/SMS';
078700160513             *in18 = *on;
078800160513           ENDIF;
078900160513         ENDIF;
079000160224
079100160224       //?Recupero NPR
079200160224         k_OREtrc = 'NP';
079300160224         clear OREdati;
079400160224       //?Aggancio il rcd
079500160224         chain  %kds(k05fnore) FNORE01L;
079600160224         IF  %found(FNORE01L);
079700160224           V1npr = OREdati;
079800160224         ENDIF;
079900140121
080000140121       //?Controllo se ci sono Note AUT
080100140121         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
080200140121         IF  %found(FNORT01L);
080300140121           *in48 = *on;
080400140121         ENDIF;
080500131211
080600131028      /end-free
080700001009
080800001009     C                   endsr
080900001030      **********************************************************************
081000001030      * DECODIFICHE
081100001030      **********************************************************************
081200001030     C     Sr_Decodifica begsr
081300001030
081400001030      * Fase
081500001129     C                   clear                   v1dfao
081600001030     C                   clear                   TIBS02DS
081700001030     C                   movel     'C'           T02mod
081800001030     C                   movel     knsif         t02sif
081900001030     C                   movel     'FAR'         t02cod
082000001030     C                   movel(p)  v1fao         T02ke1
082100001030     C                   call      'TIBS02R'
082200001030     C                   parm                    KPJBA
082300001030     C                   parm                    TIBS02DS
082400040908     c                   Eval      dFar = T02Uni
082500040908     c                   Eval      V1dfao = d§fardes
082600001030
082700001030      * Tipo ordine assegnazione
082800001129     C                   clear                   v1dtco
082900001030     C                   clear                   TIBS02DS
083000001030     C                   movel     'C'           T02mod
083100001030     C                   movel     knsif         t02sif
083200001129     C                   movel     'TCO'         t02cod
083300001129     C                   movel(p)  v1ctco        T02ke1
083400001030     C                   call      'TIBS02R'
083500001030     C                   parm                    KPJBA
083600001030     C                   parm                    TIBS02DS
083700001129     C                   movel     t02uni        v1dtco
083800001030
083900001030      * P.O. ritiro
084000061003     c                   eval      *in22 = *off
084100061003     c                   clear                   v1dfcs
084200061003     c                   clear                   og143
084300001129     C                   clear                   v1dpor
084400140206     c                   clear                   network
084500001030     C                   movel     v1cpor        kazorg
084600001030     C     kazorg        chain     azorg01L
084700001030     C                   if        %found(azorg01l)
084800001030     C                   movel     ORGdes        v1dpor
084900140206     C                   movel     orgdit        network
085000061003     c                   eval      og143 = orgde3
085100001030     C                   endif
085200061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
085300061003      * europolitan invece ormai è obsoleta
085400061003     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
085500061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
085600061003      * partener se caricata e la visualizzo al posto della zona
085700061017     c                   if        v1cfcs <> *blanks
085800061003     c                   clear                   tibs02ds
085900061003     c                   eval      t02mod = 'C'
086000061003     c                   eval      t02sif = knsif
086100061003     c                   eval      t02cod = 'MIP'
086200061017     c                   eval      t02ke1 = v1cfcs
086300061003     c                   call      'TIBS02R'
086400061003     c                   parm                    kpjba
086500061003     c                   parm                    tibs02ds
086600061003     c                   eval      v1dfcs = t02uni
086700061003     c                   eval      *in22 = *on
086800061003     c                   endif
086900061003     c                   endif
087000070911
087100070911      * Giro
087200070911     c                   clear                   v1dcgi
087300070911     c                   clear                   fidg09ds
087400070911     c                   eval      d09iop0 = '001'
087500071127     c                   eval      d09ifgs = v1cpor
087600070911     c                   eval      d09idat = *date
087700070911     c                   eval      d09icgi = v1ccgi
087800070911     c                   eval      d09itug = 'R'
087900080318     c                   movel     kpjbu         §kpjbu
088000070911     c                   eval      kpjbu = fidg09ds
088100070911     c                   call      'FIDG09R'
088200070911     c                   parm                    kpjba
088300070911     c                   eval      fidg09ds = kpjbu
088400080318     c                   movel     §kpjbu        kpjbu
088500070911     c                   if        d09ocgi <> ' '
088600070911     c                   eval      v1dcgi = d09odes
088700070911     c                   endif
088800001030
088900001030      * P.O. Consegna
089000001129     C                   clear                   v1dpoc
089100001030     C                   movel     v1cpoc        kazorg
089200001030     C     kazorg        chain     azorg01L
089300001030     C                   if        %found(azorg01l)
089400001030     C                   movel     ORGdes        v1dpoc
089500001030     C                   endif
089600001030
089700001030      * Tipo ordine
089800001129     C                   clear                   v1dtor
089900001030     C                   clear                   TIBS02DS
090000001030     C                   movel     'C'           T02mod
090100001030     C                   movel     knsif         t02sif
090200001030     C                   movel     'TOR'         t02cod
090300001030     C                   movel(p)  v1ctor        T02ke1
090400001030     C                   call      'TIBS02R'
090500001030     C                   parm                    KPJBA
090600001030     C                   parm                    TIBS02DS
090700001030     C                   movel     t02uni        v1dtor
090800001103
090900010205      * Priorità
091000010205     C                   clear                   v1dsto
091100010205     C                   if        v1sto <> *zeros
091200001103     C                   clear                   TIBS02DS
091300001103     C                   movel     'C'           T02mod
091400001103     C                   movel     knsif         t02sif
091500001103     C                   movel     'STO'         t02cod
091600131029     C                   eval      t02ke1 = %editc(ORMsto:'X')
091700001103     C                   call      'TIBS02R'
091800001103     C                   parm                    KPJBA
091900001103     C                   parm                    TIBS02DS
092000010205     C                   movel     t02uni        v1dsto
092100010205     C                   endif
092200001030
092300010226      * Autotrasportatore
092400001129     C                   clear                   v1dpdc
092500021202     C                   movel     v1cpdc        kpdpdr
092600021202     C     kfapd         chain     fiapd01L
092700021202     C                   if        %found(fiapd01l)
092800001030     C                   movel     APDrsc        v1dpdc
092900001030     C                   endif
093000001031      * ORM fisso
093100001031     C                   eval      *in04 = (v1ors <> *zeros)
093200161108
093300161108      /free
093400161108       //?Se non ho la data pronta merce imposto la data ritiro
093500161108         IF  V1dpm = 0;
093600161108           V1dpm = V1dar;
093700161108         ENDIF;
093800161108      /end-free
093900001030
094000001030     C                   endsr
094100010305      **********************************************************************
094200010305      * Cerca causale chiusura e decodifica
094300010305      **********************************************************************
094400010305     C     Sr_Carcau     begsr
094500010305
094600010305     C                   eval      kpoe = ORMpoe
094700010305     C                   eval      knsr = ORMnsr
094800010305     C                   eval      knor = ORMnor
094900010305     C                   eval      knrv = ORMnrv
095000010305     C                   eval      kdfo = ORMdfo
095100010305     C                   eval      kofo = ORMofo
095200010305     C                   eval      kfao = ORMfao
095300110418     C**!!!kfnorf        chain     fnorf01L
095400110418     C**!!!              if        %found(fnorf01l)
095500110418     c     kfnorf        setll     fnorf01l
095600110418     c                   do        *hival
095700110418     c     kfnorf        reade     fnorf01l
095800110418     c                   if        %eof(fnorf01l)
095900110418     c                   leave
096000110418     c                   endif
096100010305     C                   eval      v1cau = orfcar
096200110418     C                   enddo
096300010305
096400010305     C                   clear                   dcmr
096500010305     C                   clear                   TIBS02DS
096600010305     C                   movel     'C'           T02mod
096700010305     C                   movel     knsif         t02sif
096800010305     C                   movel     'CMR'         t02cod
096900010305     C                   movel(p)  v1cau         T02ke1
097000010305     C                   call      'TIBS02R'
097100010305     C                   parm                    KPJBA
097200010305     C                   parm                    TIBS02DS
097300010305     C                   movel     t02uni        dcmr
097400010305     C                   movel     d§cmrdes      v1dcau
097500010309
097600010309     C                   eval      *in07 = (v1cau <> *blanks)
097700010305
097800010305     C                   endsr
097900001031      **********************************************************************
098000081125      * F20 - Fasi
098100001031      **********************************************************************
098200001031     C     Sr_Fasi       BEGSR
098300001031
098400001031     C                   z-add     1             recsff
098500001031      * Pulisco subfile
098600001031     C                   z-add     *zeros        nrrf              4 0
098700001031     C                   eval      *in21 = *off
098800001031     C                   write     fior07cf
098900001031     C                   eval      *in20 = *on
099000001031     C                   eval      *in21 = *on
099100001031
099200001031      * Carico il subfile in base alle scelte fatte
099300001031     C     kfnor         setll     fnorf01l
099400001031     C                   do        *hival
099500001031     C     kfnor         reade     fnorf01l
099600001031     C                   if        %eof
099700001031     C                   leave
099800001031     C                   endif
099900001031
100000001031     C                   z-add     v1cpoe        v3cpoe
100100001031     C                   z-add     v1nsr         v3nsr
100200001031     C                   z-add     v1nor         v3nor
100300001031     C                   z-add     v1nrv         v3nrv
100400081125
100500081125     c                   eval      dorf01 = orfflo
100600001031
100700001031     C                   exsr      Sr_CarFasi
100800001031     C                   enddo
100900001031      * Se non ho caricato emetto una videata di segnalazione.
101000001031     C                   if        nrrf = *zeros
101100001031     C                   movel     msg(2)        v1cmsg
101200001031     C                   seton                                        28
101300001031     C                   goto      endfasi
101400001031     C                   endif
101500001031
101600001031      * Se ho caricato emetto il subfile
101700001031     C                   if        nrrf > *zeros
101800001102     C                   do        *hival
101900001031     C                   write     fior073
102000001031     C                   exfmt     fior07cf
102100001031      * F12=Ritorno
102200001102     C   kl              leave
102300001102     C                   enddo
102400001102     C                   endif
102500001031
102600001031     C     endfasi       endsr
102700001031      **********************************************************************
102800001031      * CARICO SUBFILE FASI
102900001031      **********************************************************************
103000001031     C     Sr_CarFasi    begsr
103100001031
103200001031      * Inversione data modifica
103300001031     C                   z-add     ORFdae        G02inv
103400001031     C                   movel     '3'           G02err
103500001031     C                   call      'XSRDA8'
103600001031     C                   parm                    wlbdat
103700001031     C                   movel     G02dat        v3dae
103800001031
103900001031     C                   movel     ORFore        v3ore
104000001031     C                   eval      v3cpog = ORFpog
104100001031     C                   eval      v3ute  = ORFpue
104200001031     C                   eval      v3far  = ORFfar
104300001031      * Decodifica fase
104400001031     C                   clear                   TIBS02DS
104500001031     C                   movel     'C'           T02mod
104600001031     C                   movel     knsif         t02sif
104700001031     C                   movel     'FAR'         t02cod
104800001031     C                   movel(p)  v3far         T02ke1
104900001031     C                   call      'TIBS02R'
105000001031     C                   parm                    KPJBA
105100001031     C                   parm                    TIBS02DS
105200040908     C**!!!              movel     t02uni        v3dfar
105300040908     c                   Eval      dFar = T02Uni
105400040908     c                   Eval      V3dfar = d§fardes
105500001031
105600001031     C                   if        ORFcar <> *blanks
105700001031     C                   seton                                        02
105800001031     C                   eval      v3car  = ORFcar
105900001031      * Decodifica causale
106000010301     C                   clear                   dcmr
106100001031     C                   clear                   TIBS02DS
106200001031     C                   movel     'C'           T02mod
106300001031     C                   movel     knsif         t02sif
106400001128     C                   movel     'CMR'         t02cod
106500001031     C                   movel(p)  v3car         T02ke1
106600001031     C                   call      'TIBS02R'
106700001031     C                   parm                    KPJBA
106800001031     C                   parm                    TIBS02DS
106900010301     C                   movel     t02uni        dcmr
107000010301     C                   movel     d§cmrdes      v3dcar
107100001107     C                   else
107200001107     C                   setoff                                       02
107300001107     C                   clear                   v3car
107400001107     C                   clear                   v3dcar
107500001031     C                   endif
107600081125      * visualizzo se 'No addebito'
107700081125     c                   eval      *in17 = *off
107800081125     c                   if        §orfadd = 'S'
107900081125     c                   eval      *in17 = *on
108000081125     c                   endif
108100040908
108200040908      * Se ci sono carico anche i dati della distinta
108300040908if  1c                   If        OrfNdc > *Zeros
108400040908     c                   Eval      *In10 = *On
108500040908     c                   Eval      v3ndc = OrfNdc
108600040908      * giro la data della distinta
108700040908     c                   Clear                   wlbdat
108800040908     c                   Z-add     OrfDdc        G02inv
108900040908     c                   Movel     '3'           G02err
109000040908     c                   Call      'XSRDA8'
109100040908     c                   Parm                    wlbdat
109200040908     c                   Z-add     G02dat        v3ddc
109300040915
109400040908      * cerco il codice dell'autotrasportatore
109500040908     c                   Z-add     OrfNdc        knfv
109600040908     c                   Z-add     OrfFgs        kfgs
109700120612     c     kfidst        Chain     fidst01l
109800060522      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
109900120612     c                   if        not %found(fidst01l)
110000060522     c                   clear                   fnlv55ds
110100060522     c                   eval      d55tpt = '6'
110200060522     c                   move      orffgs        d55lin
110300060522     c                   eval      d55drf = ormddc
110400060522     c                   call      'FNLV55R'
110500060522     c                   parm                    fnlv55ds
110600060522     c                   if        d55err = *blanks and d55tfa <> d55lin
110700060522     c                   move      d55tfa        kfgs
110800120612     c     kfidst        chain     fidst01l
110900060522     c                   endif
111000060522     c                   endif
111100120612     c                   If        %Found(fidst01l)
111200040908     c                   Eval      v3cpdr = DstPdr
111300040908     c                   EndIf
111400040908      * decodifico l'autotrasportatore
111500040908     c                   Clear                   v3dpdr
111600040908     c                   Movel     v3cpdr        kpdpdr
111700040908     c     kfapd         Chain     Fiapd01L
111800040908     c                   If        %Found(Fiapd01l)
111900040908     c                   Eval      v3dpdr = ApdRsc
112000040908     c                   EndIf
112100041013   x1c                   Else
112200041013     c                   Eval      *In10 = *Off
112300041013e   1c                   EndIf
112400001031
112500001031     C                   add       1             nrrf
112600001031     C                   write     fior07sf
112700001031
112800001031     C                   endsr
112900001030      **********************************************************************
113000131120      * F15 - Variazioni
113100001030      **********************************************************************
113200001102     C     Sr_Variazioni BEGSR
113300001030
113400001031     C                   z-add     1             recsf
113500001030      * Pulisco subfile
113600001030     C                   z-add     *zeros        nrr               4 0
113700001030     C                   eval      *in21 = *off
113800001030     C                   write     fior07c
113900001030     C                   eval      *in20 = *on
114000001030     C                   eval      *in21 = *on
114100001030
114200001030      * Carico il subfile in base alle scelte fatte
114300001030     C     kfnor         setll     fnorv01l
114400001030     C                   do        *hival
114500001030     C     kfnor         reade     fnorv01l
114600001030     C                   if        %eof
114700001030     C                   leave
114800001030     C                   endif
114900001030
115000001031     C                   z-add     v1cpoe        v2cpoe
115100001031     C                   z-add     v1nsr         v2nsr
115200001031     C                   z-add     v1nor         v2nor
115300001031     C                   z-add     v1nrv         v2nrv
115400001030
115500001102     C                   exsr      Sr_CarVariaz
115600001030     C                   enddo
115700001030      * Se non ho caricato emetto una videata di segnalazione.
115800001031     C                   if        nrr = *zeros
115900001031     C                   movel     msg(1)        v1cmsg
116000001031     C                   seton                                        28
116100001102     C                   goto      endvariaz
116200001031     C                   endif
116300001030
116400001030      * Se ho caricato emetto il subfile
116500001030     C                   if        nrr > *zeros
116600001031     C                   do        *hival
116700001030     C                   write     fior072
116800001030     C                   exfmt     fior07c
116900001030      * F12=Ritorno
117000001102     C   kl              goto      endvariaz
117100001030
117200001031      * Legge i record scelti
117300001030     C                   do        *hival
117400001030     C                   readc     fior07s
117500001030     C                   if        %eof
117600001030     C                   leave
117700001030     C                   endif
117800001030     C                   if        v2scel = '5'
117900001030     C                   z-add     nrr           recsf
118000001102     C                   exsr      Sr_VisVariaz
118100001030     C                   endif
118200001031     C                   clear                   v2scel
118300001030     C                   update    fior07s
118400001030     C                   enddo
118500001030
118600001030     C                   enddo
118700001031     C                   endif
118800001030
118900001102     C     endvariaz     endsr
119000001030      **********************************************************************
119100001102      * CARICO SUBFILE VARIAZIONI
119200001030      **********************************************************************
119300001102     C     Sr_CarVariaz  begsr
119400001030
119500001030     C                   clear                   v2scel
119600001030     C                   eval      v2cpor = ORVpor
119700001030
119800001030      * Inversione data modifica
119900001103     C                   z-add     ORVdtv        v2dtvd
120000001030     C                   z-add     ORVdtv        G02inv
120100001030     C                   movel     '3'           G02err
120200001030     C                   call      'XSRDA8'
120300001030     C                   parm                    wlbdat
120400001030     C                   movel     G02dat        v2dtv
120500001030
120600001030     C                   movel     ORVorv        v2orv
120700001030     C                   eval      v2utv  = ORVutv
120800001031     C                   eval      v2fao  = ORVfao
120900001031      * Decodifica
121000001031     C                   clear                   TIBS02DS
121100001031     C                   movel     'C'           T02mod
121200001031     C                   movel     knsif         t02sif
121300001031     C                   movel     'FAR'         t02cod
121400001031     C                   movel(p)  v2fao         T02ke1
121500001031     C                   call      'TIBS02R'
121600001031     C                   parm                    KPJBA
121700001031     C                   parm                    TIBS02DS
121800001031     C                   movel     t02uni        v2dfao
121900001030
122000001030     C                   add       1             nrr
122100001030     C                   write     fior07s
122200001030
122300001030     C                   endsr
122400001030      **********************************************************************
122500001102      * VISUALIZZO DATI DEL FILE VARIAZIONI
122600001030      **********************************************************************
122700001102     C     Sr_VisVariaz  begsr
122800001031
122900001031     C                   seton                                        01
123000001030
123100001031     C                   eval      v1mod1 = 'Var.il '
123200001031     C                             + v2dtvg
123300001031     C                             +'/'
123400001031     C                             + v2dtvm
123500001031     C                             + '/'
123600001031     C                             + v2dtva
123700001031     C                             + ' alle '
123800001031     C                             + v2orvo
123900001031     C                             + ':'
124000001031     C                             + v2orvm
124100001031     C                             + ':'
124200001031     C                             + v2orvs
124300001031     C                             + ' da '
124400001031     C                             + v2utv
124500001031
124600001103     C                   eval      kdtv = v2dtvd
124700001103     C                   eval      korv = v2orv
124800001103     C     kforp         chain     fnorv01l
124900001103     C                   if        %found(fnorv01l)
125000001103
125100001102     C                   exsr      Sr_CarVidVar
125200001031     C                   exsr      Sr_Decodifica
125300120612     c                   ExSr      Sr_Tastifun
125400001103     C                   endif
125500001030
125600001102     C                   do        *hival
125700001031     C                   exfmt     fior071
125800001102
125900001102      * F12=Ritorno
126000001102     C   kl              leave
126100131029      /free
126200131029       //?F5=Altri dati
126300131029       IF  *inke;
126400131029         exsr F05D01;
126500131029         iter;
126600131029       ENDIF;
126700131029      /end-free
126800001102
126900001102     C                   enddo
127000001031
127100001030     C                   endsr
127200001031      **********************************************************************
127300001102      * CARICA DATI NELLA VIDEATA DAL FILE VARIAZIONI
127400001031      **********************************************************************
127500001102     C     Sr_CarVidVar  begsr
127600001031
127700001129     C                   movel     ORMtco        v1ctco
127800001031     C                   movel     ORVtor        v1ctor
127900001031      * Inversione data emissione ORM
128000001031     C                   clear                   wlbdat
128100001031     C                   z-add     ORMdao        G02inv
128200001031     C                   movel     '3'           G02err
128300001031     C                   call      'XSRDA8'
128400001031     C                   parm                    wlbdat
128500001031     C                   z-add     G02dat        v1dao
128600010109     C                   movel     ORMoao        v1oao
128700001031      * Dati Ordinante
128800001031     C                   z-add     ORVcor        v1ccor
128900001031     C                   movel     ORVrso        v1rso
129000001031     C                   movel     ORVino        v1ino
129100001031     C                   movel     ORVcao        v1cao
129200001031     C                   movel     ORVloo        v1loo
129300001031     C                   movel     ORVpro        v1pro
129400001031     C                   movel     ORVnao        v1nao
129500001031      * Dati Ritiro a
129600001031     C                   z-add     ORVcra        v1ccra
129700001031     C                   movel     ORVrsr        v1rsr
129800001031     C                   movel     ORVinr        v1inr
129900001031     C                   movel     ORVcar        v1car
130000001031     C                   movel     ORVlor        v1lor
130100001031     C                   movel     ORVprr        v1prr
130200001031     C                   movel     ORVnar        v1nar
130300001031     C                   movel     ORVrer        v1rer
130400001031     C                   movel     ORVter        v1ter
130500001031      * Dati Consegna a
130600001031     C                   z-add     ORVcrc        v1ccrc
130700001031     C                   movel     ORVrsc        v1rsc
130800001031     C                   movel     ORVinc        v1inc
130900001031     C                   movel     ORVloc        v1loc
131000001031     C                   movel     ORVcac        v1cac
131100001031     C                   movel     ORVprc        v1prc
131200001031     C                   movel     ORVnac        v1nac
131300001031
131400010315     C                   if        ORVffd = 'S'
131500010315     C                   eval      v1ffd = 'SI'
131600010315     C                   else
131700010315     C                   eval      v1ffd = 'NO'
131800010315     C                   endif
131900010315
132000001031     C                   movel     ORVnam        v1nam
132100001031      * Inversione data ritiro
132200001031     C                   z-add     ORVdar        G02inv
132300001031     C                   movel     '3'           G02err
132400001031     C                   call      'XSRDA8'
132500001031     C                   parm                    wlbdat
132600001031     C                   z-add     G02dat        v1dar
132700001031     C                   movel     ORVorr        v1orr
132800001031
132900001031     C                   z-add     ORVncl        v1ncl
133000001031     C                   z-add     ORVpkg        v1pkg
133100001031     C                   z-add     ORVvlm        v1vlm
133200001031     C                   z-add     ORVbnc        v1bnc
133300001031     C                   z-add     ORVblc        v1blc
133400001031     C                   z-add     ORVatt        v1att
133500001031     C                   z-add     ORVmtc        v1mtc
133600001031
133700010315     C                   if        ORVpag = 'M'
133800010315     C                   eval      v1pag = 'Mitt.'
133900010315     C                   endif
134000010315     C                   if        ORVpag = 'D'
134100010315     C                   eval      v1pag = 'Dest.'
134200010315     C                   endif
134300010315     C                   if        ORVpag = 'O'
134400010315     C                   eval      v1pag = 'Ord. '
134500010315     C                   endif
134600010315
134700001031     C                   z-add     ORVksc        v1ksc
134800001031     C                   movel     ORVctr        v1ctr
134900010315
135000010315     C                   if        ORVddt = 'S'
135100010315     C                   eval      v1ddt = 'SI'
135200010315     C                   endif
135300010315     C                   if        ORVddt = 'N'
135400010315     C                   eval      v1ddt = 'NO'
135500010315     C                   endif
135600010315
135700001031     C                   movel     ORVpor        v1cpor
135800001031     C                   movel     ORVpoc        v1cpoc
135900001031     C                   movel     ORVno1        v1not1
136000001031     C                   movel     ORVno2        v1not2
136100001031     C                   z-add     ORVfao        v1fao
136200001031     C                   z-add     ORMpos        v1cpos
136300001031     C                   z-add     ORMors        v1ors
136400001031     C                   movel     ORVrfa        v1rfa
136500001221     C                   move      ORVsto        v1sto
136600001031     C                   z-add     ORVndc        v1ndc
136700001031      * Inversione data distinta
136800001031     C                   clear                   wlbdat
136900001031     C                   z-add     ORVddc        G02inv
137000001031     C                   movel     '3'           G02err
137100001031     C                   call      'XSRDA8'
137200001031     C                   parm                    wlbdat
137300001031     C                   z-add     G02dat        v1ddc
137400010226
137500010226      * Cerca codice autotrasportatore
137600071129     c                   clear                   v1cpdc
137700001031     C                   z-add     ORVndc        knfv
137800001031     C                   z-add     ORVpor        kfgs
137900120612     C     kfidst        chain     fidst01l
138000120612     C                   if        %found(fidst01l) and DSTatb = *blanks
138100001031     C                   eval      v1cpdc = DSTpdr
138200001031     C                   endif
138300010119
138400010315     C                   if        ORVspi = 'S'
138500010315     C                   eval      v1spi = 'SI'
138600010315     C                   else
138700010315     C                   eval      v1spi = 'NO'
138800010315     C                   endif
138900030627
139000030627     C                   eval      dorm01 = ORvflo
139100030627
139200030627      * ORM commissionato
139300030627     c                   If        §orCom = 'S'
139400030627     c                   Eval      V1dCom = 'COMMISSIONATO'
139500030627     c                   Else
139600030627     c                   Clear                   V1dCom
139700030627     c                   EndIf
139800131211
139900131211     c                   eval      *in46 = *off
140000131029
140100131029      /free
140200131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
140300131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
140400131211           *in46 = *on;
140500131211         ENDIF;
140600131029       //?Recupero dati da estensione FNORVE
140700131029       //?Rcd 'O ' --> Orari apertura
140800131029         k_ORVEpoe = ORVpoe;
140900131029         k_ORVEnsr = ORVnsr;
141000131029         k_ORVEnor = ORVnor;
141100131029         k_ORVEnrv = ORVnrv;
141200131029         k_ORVEdtv = ORVdtv;
141300131029         k_ORVEorv = ORVorv;
141400131029         k_ORVEtrc = 'O ';
141500131029         clear dOREorari;
141600131029       //?Aggancio il rcd
141700171106         chain %kds(k07fnorve) FNORVE1L;
141800131029         IF  %found(FNORVE1L);
141900131029           dOREorari = ORVEdati;
142000131029         ENDIF;
142100131029         v1OraAMda = §OREoramda;
142200131029         v1OraAMa  = §OREorama;
142300131029         v1OraPMda = §OREorapda;
142400131029         v1OraPMa  = §OREorapa;
142500170607
142600170607       //?Rcd 'DT' --> Data Pronta Merce
142700170607         k_ORVEtrc = 'DT';
142800170607         clear dOREdt;
142900170607       //?Aggancio il rcd
143000171106         chain %kds(k07fnorve) FNORVE1L;
143100170607         IF  %found(FNORVE1L);
143200170607           dOREdt = ORVEdati;
143300170607         ENDIF;
143400170607         IF  §OREdpm > *zeros;
143500170607           clear wlbdat;
143600170607           G02inv = §OREdpm;
143700170607           G02err = '3';
143800170607           xsrda8 (wlbdat);
143900170607           V1dpm = G02dat;
144000170607         ENDIF;
144100160513
144200131029      /end-free
144300001031
144400001031     C                   endsr
144500001025      **********************************************************************
144600001025      * F18 - Note
144700001025      **********************************************************************
144800001025     C     Sr_Note       BEGSR
144900001025
145000001025     C                   clear                   fior06DS
145100001025     C                   z-add     v1dao         i06dta
145200001026     C                   z-add     v1cpoe        i06poe
145300001026     C                   z-add     v1nor         i06nor
145400001026     C                   z-add     v1nsr         i06nsr
145500001026     C                   z-add     v1nrv         i06nrv
145600001026     C                   movel     v1fao         i06far
145700001026     C                   eval      i06dai = §doggi
145800001026     C                   TIME                    W0140
145900001026     C                   movel     w0140         I06ori
146000001026     C                   movel     'I'           i06flm
146100061017     c   ki              movel     'V'           i06flm
146200001026
146300001025     C                   CALL      'FIOR06R'
146400001025     C                   PARM                    kpjba
146500001027     C                   parm                    fior06ds
146600001025
146700001025     C                   endsr
146800001031      **********************************************************************
146900001031      * F19 - Proposte
147000001031      **********************************************************************
147100001031     C     Sr_Proposte   BEGSR
147200001031
147300001031     C                   z-add     1             recsfp
147400001031      * Pulisco subfile
147500001031     C                   z-add     *zeros        nrrp              4 0
147600001031     C                   eval      *in21 = *off
147700001031     C                   write     fior07cp
147800001031     C                   eval      *in20 = *on
147900001031     C                   eval      *in21 = *on
148000001031
148100001031      * Carico il subfile in base alle scelte fatte
148200001031     C     kfnor         setll     fnorp01l
148300001031     C                   do        *hival
148400001031     C     kfnor         reade     fnorp01l
148500001031     C                   if        %eof
148600001031     C                   leave
148700001031     C                   endif
148800001031
148900001031     C                   z-add     v1cpoe        v4cpoe
149000001031     C                   z-add     v1nsr         v4nsr
149100001031     C                   z-add     v1nor         v4nor
149200001031     C                   z-add     v1nrv         v4nrv
149300001031
149400001031     C                   exsr      Sr_CarProposte
149500001031     C                   enddo
149600001031      * Se non ho caricato emetto una videata di segnalazione.
149700001106     C                   if        nrrp = *zeros and psce <> 'P'
149800001103     C                   movel     msg(3)        v1cmsg
149900001031     C                   seton                                        28
150000001031     C                   goto      endproposte
150100001031     C                   endif
150200001103
150300001103     C                   if        nrrp = *zeros and psce = 'P'
150400001103     C                   do        *hival
150500001103     C                   exfmt     fior075
150600001103      * F12=Ritorno
150700001103     C   kl              leave
150800001103     C                   enddo
150900001103     C                   endif
151000001031
151100001031      * Se ho caricato emetto il subfile
151200001031     C                   if        nrrp > *zeros
151300001031     C                   do        *hival
151400001031     C                   write     fior074
151500001031     C                   exfmt     fior07cp
151600001031      * F12=Ritorno
151700001031     C   kl              goto      endproposte
151800001031
151900001031      * Legge i record scelti
152000001031     C                   do        *hival
152100001031     C                   readc     fior07sp
152200001031     C                   if        %eof
152300001031     C                   leave
152400001031     C                   endif
152500001031     C                   if        v4scel = '5'
152600001031     C                   z-add     nrrp          recsfp
152700001031     C                   exsr      Sr_VisProposte
152800001031     C                   endif
152900001031     C                   clear                   v4scel
153000001031     C                   update    fior07sp
153100001031     C                   enddo
153200001031
153300001031     C                   enddo
153400001031     C                   endif
153500001031
153600001031     C     endproposte   endsr
153700001031      **********************************************************************
153800001031      * CARICO SUBFILE PROPOSTE
153900001031      **********************************************************************
154000001031     C     Sr_CarPropostebegsr
154100001031
154200001031     C                   clear                   v4scel
154300001031     C                   eval      v4cpor = ORPpoe
154400001031
154500001031      * Inversione data modifica
154600001103     C                   z-add     ORPdtv        v4dtvd
154700001031     C                   z-add     ORPdtv        G02inv
154800001031     C                   movel     '3'           G02err
154900001031     C                   call      'XSRDA8'
155000001031     C                   parm                    wlbdat
155100001031     C                   movel     G02dat        v4dtv
155200001031
155300001031     C                   movel     ORPorv        v4orv
155400001031     C                   eval      v4utv  = ORPutv
155500001031     C                   eval      v4fao  = ORPfao
155600001031      * Decodifica
155700001031     C                   clear                   TIBS02DS
155800001031     C                   movel     'C'           T02mod
155900001031     C                   movel     knsif         t02sif
156000001031     C                   movel     'FAR'         t02cod
156100001031     C                   movel(p)  v4fao         T02ke1
156200001031     C                   call      'TIBS02R'
156300001031     C                   parm                    KPJBA
156400001031     C                   parm                    TIBS02DS
156500001031     C                   movel     t02uni        v4dfao
156600001031      * Analisi Proposta
156700001031     C                   if        ORPfev <> *blanks
156800001031     C                   seton                                        03
156900001031     C                   if        ORPfev = 'E'
157000001031     C                   eval      v4sta = 'EFFETTUATA'
157100001107     C                   eval      v4sta1= 'EFF'
157200001031     C                   endif
157300001031     C                   if        ORPfev = 'R'
157400001031     C                   eval      v4sta = 'RIFIUTATA'
157500001107     C                   eval      v4sta1= 'RIF'
157600001031     C                   endif
157700001031      * Inversione data analisi proposta
157800001031     C                   z-add     ORPdte        G02inv
157900001031     C                   movel     '3'           G02err
158000001031     C                   call      'XSRDA8'
158100001031     C                   parm                    wlbdat
158200001031     C                   movel     G02dat        v4dte
158300001031     C                   movel     ORPore        v4ore
158400001031     C                   eval      v4ute  = ORPute
158500001107     C                   else
158600001107     C                   setoff                                       03
158700001107     C                   clear                   v4sta
158800001107     C                   clear                   v4sta1
158900001107     C                   clear                   v4dte
159000001107     C                   clear                   v4ore
159100001107     C                   clear                   v4ute
159200001031     C                   endif
159300001031
159400001031     C                   add       1             nrrp
159500001031     C                   write     fior07sp
159600001031
159700001031     C                   endsr
159800001031      **********************************************************************
159900001031      * VISUALIZZO DATI DEL FILE PROPOSTE
160000001031      **********************************************************************
160100001031     C     Sr_VisPropostebegsr
160200001031
160300140303     c                   eval      *in15 = *on
160400001031
160500001031     C                   eval      v1mod1 = 'Prop. del '
160600001031     C                             + v4dtvg
160700001031     C                             +'/'
160800001031     C                             + v4dtvm
160900001031     C                             + '/'
161000001031     C                             + v4dtva
161100001031     C                             + ' alle '
161200001031     C                             + v4orvo
161300001031     C                             + ':'
161400001031     C                             + v4orvm
161500001031     C                             + ':'
161600001031     C                             + v4orvs
161700001031     C                             + ' da '
161800001031     C                             + v4utv
161900001031
162000001103     C                   eval      kdtv = v4dtvd
162100001103     C                   eval      korv = v4orv
162200001103     C     kforp         chain     fnorp01l
162300001103     C                   if        %found(fnorp01l)
162400001103
162500001031     C                   exsr      Sr_CarVidPro
162600001031     C                   exsr      Sr_Decodifica
162700120612     c                   ExSr      Sr_Tastifun
162800001103     C                   endif
162900001031
163000001102     C                   do        *hival
163100001031     C                   exfmt     fior071
163200001102
163300001102      * F12=Ritorno
163400001102     C   kl              leave
163500131029      /free
163600131029       //?F5=Altri dati
163700131029       IF  *inke;
163800131029         exsr F05D01;
163900131029         iter;
164000131029       ENDIF;
164100140303
164200140303       //?F17=Note AUT
164300140303       IF  *inkr;
164400140303         exsr F17D01;
164500140303         iter;
164600140303       ENDIF;
164700131029      /end-free
164800001102
164900001102     C                   enddo
165000001031
165100001031     C                   endsr
165200001031      **********************************************************************
165300001031      * CARICA DATI NELLA VIDEATA DAL FILE PROPOSTE
165400001031      **********************************************************************
165500001031     C     Sr_CarVidPro  begsr
165600001031
165700001129     C                   movel     ORMtco        v1ctco
165800001031     C                   movel     ORPtor        v1ctor
165900001031      * Inversione data emissione ORM
166000001031     C                   clear                   wlbdat
166100001031     C                   z-add     ORMdao        G02inv
166200001031     C                   movel     '3'           G02err
166300001031     C                   call      'XSRDA8'
166400001031     C                   parm                    wlbdat
166500001031     C                   z-add     G02dat        v1dao
166600010109     C                   movel     ORMoao        v1oao
166700001031      * Dati Ordinante
166800001031     C                   z-add     ORPcor        v1ccor
166900001031     C                   movel     ORPrso        v1rso
167000001031     C                   movel     ORPino        v1ino
167100001031     C                   movel     ORPcao        v1cao
167200001031     C                   movel     ORPloo        v1loo
167300001031     C                   movel     ORPpro        v1pro
167400001031     C                   movel     ORPnao        v1nao
167500001031      * Dati Ritiro a
167600001031     C                   z-add     ORPcra        v1ccra
167700001031     C                   movel     ORPrsr        v1rsr
167800001031     C                   movel     ORPinr        v1inr
167900001031     C                   movel     ORPcar        v1car
168000001031     C                   movel     ORPlor        v1lor
168100001031     C                   movel     ORPprr        v1prr
168200001031     C                   movel     ORPnar        v1nar
168300001031     C                   movel     ORPrer        v1rer
168400001031     C                   movel     ORPter        v1ter
168500001031      * Dati Consegna a
168600001031     C                   z-add     ORPcrc        v1ccrc
168700001031     C                   movel     ORPrsc        v1rsc
168800001031     C                   movel     ORPinc        v1inc
168900001031     C                   movel     ORPloc        v1loc
169000001031     C                   movel     ORPcac        v1cac
169100001031     C                   movel     ORPprc        v1prc
169200001031     C                   movel     ORPnac        v1nac
169300001031
169400010315     C                   if        ORPffd = 'S'
169500010315     C                   eval      v1ffd = 'SI'
169600010315     C                   else
169700010315     C                   eval      v1ffd = 'NO'
169800010315     C                   endif
169900010315
170000001031     C                   movel     ORPnam        v1nam
170100001031      * Inversione data ritiro
170200001031     C                   z-add     ORPdar        G02inv
170300001031     C                   movel     '3'           G02err
170400001031     C                   call      'XSRDA8'
170500001031     C                   parm                    wlbdat
170600001031     C                   z-add     G02dat        v1dar
170700001031     C                   movel     ORPorr        v1orr
170800001031
170900001031     C                   z-add     ORPncl        v1ncl
171000001031     C                   z-add     ORPpkg        v1pkg
171100001031     C                   z-add     ORPvlm        v1vlm
171200001031     C                   z-add     ORPbnc        v1bnc
171300001031     C                   z-add     ORPblc        v1blc
171400001031     C                   z-add     ORPatt        v1att
171500001031     C                   z-add     ORPmtc        v1mtc
171600010315
171700010315     C                   if        ORPpag = 'M'
171800010315     C                   eval      v1pag = 'Mitt.'
171900010315     C                   endif
172000010315     C                   if        ORPpag = 'D'
172100010315     C                   eval      v1pag = 'Dest.'
172200010315     C                   endif
172300010315     C                   if        ORPpag = 'O'
172400010315     C                   eval      v1pag = 'Ord. '
172500010315     C                   endif
172600001031
172700001031     C                   z-add     ORPksc        v1ksc
172800001031     C                   movel     ORPctr        v1ctr
172900010315
173000010315     C                   if        ORPddt = 'S'
173100010315     C                   eval      v1ddt = 'SI'
173200010315     C                   endif
173300010315     C                   if        ORPddt = 'N'
173400010315     C                   eval      v1ddt = 'NO'
173500010315     C                   endif
173600010315
173700001031     C                   movel     ORPpor        v1cpor
173800001031     C                   movel     ORPpoc        v1cpoc
173900001031     C                   movel     ORPno1        v1not1
174000001031     C                   movel     ORPno2        v1not2
174100001031     C                   z-add     ORPfao        v1fao
174200001031     C                   z-add     ORMpos        v1cpos
174300001031     C                   z-add     ORMors        v1ors
174400001031     C                   movel     ORPrfa        v1rfa
174500001221     C                   move      ORPsto        v1sto
174600001031     C                   z-add     ORPndc        v1ndc
174700001031      * Inversione data distinta
174800001031     C                   clear                   wlbdat
174900001031     C                   z-add     ORPddc        G02inv
175000001031     C                   movel     '3'           G02err
175100001031     C                   call      'XSRDA8'
175200001031     C                   parm                    wlbdat
175300001031     C                   z-add     G02dat        v1ddc
175400010226
175500010226      * Cerca codice autotrasportatore
175600071129     c                   clear                   v1cpdc
175700001031     C                   z-add     ORPndc        knfv
175800001031     C                   z-add     ORPpor        kfgs
175900120612     C     kfidst        chain     fidst01l
176000120612     C                   if        %found(fidst01l) and DSTatb = *blanks
176100001031     C                   eval      v1cpdc = DSTpdr
176200001031     C                   endif
176300010226
176400010315     C                   if        ORPspi = 'S'
176500010315     C                   eval      v1spi = 'SI'
176600010315     C                   else
176700010315     C                   eval      v1spi = 'NO'
176800010315     C                   endif
176900030627
177000030627     C                   eval      dorm01 = ORpflo
177100030627
177200030627      * ORM commissionato
177300030627     c                   If        §orCom = 'S'
177400030627     c                   Eval      V1dCom = 'COMMISSIONATO'
177500030627     c                   Else
177600030627     c                   Clear                   V1dCom
177700030627     c                   EndIf
177800131211
177900131211     c                   eval      *in46 = *off
178000140303     c                   eval      *in48 = *off
178100131029
178200131029      /free
178300131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
178400131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
178500131211           *in46 = *on;
178600131211         ENDIF;
178700131029       //?Recupero dati da estensione FNORPE
178800131029       //?Rcd 'O ' --> Orari apertura
178900131029         k_ORPEpoe = ORPpoe;
179000131029         k_ORPEnsr = ORPnsr;
179100131029         k_ORPEnor = ORPnor;
179200131029         k_ORPEnrv = ORPnrv;
179300131029         k_ORPEdtv = ORPdtv;
179400131029         k_ORPEorv = ORPorv;
179500131029         k_ORPEtrc = 'O ';
179600131029         clear dOREorari;
179700131029       //?Aggancio il rcd
179800171106         chain %kds(k07fnorpe) FNORPE1L;
179900131029         IF  %found(FNORPE1L);
180000131029           dOREorari = ORPEdati;
180100131029         ENDIF;
180200131029         v1OraAMda = §OREoramda;
180300131029         v1OraAMa  = §OREorama;
180400131029         v1OraPMda = §OREorapda;
180500131029         v1OraPMa  = §OREorapa;
180600161114       //?Rcd 'DT' --> Data Pronta Merce
180700161114         k_ORPEtrc = 'DT';
180800161114         clear dOREdt;
180900161114       //?Aggancio il rcd
181000171106         chain %kds(k07fnorpe) FNORPE1L;
181100161114         IF  %found(FNORPE1L);
181200161114           dOREdt = ORPEdati;
181300161114         ENDIF;
181400161114         IF  §OREdpm > *zeros;
181500161114           clear wlbdat;
181600161114           G02inv = §OREdpm;
181700161114           G02err = '3';
181800161114           xsrda8 (wlbdat);
181900161114           V1dpm = G02dat;
182000161114         ENDIF;
182100140303
182200140303       //?Controllo se ci sono proposte Note AUT
182300140303         chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
182400140303         IF  %found(FNORPT1L);
182500140303           *in48 = *on;
182600140303         ENDIF;
182700131029      /end-free
182800001031
182900001031     C                   endsr
183000010109      **********************************************************************
183100010109      * Carico dati dalla kpjba per programma richiamato da interrogazione
183200010109      * proposte
183300010109      **********************************************************************
183400010109     C     Sr_Cardatipropbegsr
183500010109
183600010109     C                   z-add     ppoe          v4cpoe
183700010109     C                   z-add     pnsr          v4nsr
183800010109     C                   z-add     pnor          v4nor
183900010109     C                   z-add     pnrv          v4nrv
184000010109     C                   z-add     pdtv          v4dtv
184100010109     C                   z-add     porv          v4orv
184200010109      * Inverto la data
184300010109     C                   z-add     v4dtv         G02DAT
184400010109     C                   MOVEL     *BLANK        G02ERR
184500010109     C                   CALL      'XSRDA8'
184600010109     C                   PARM                    WLBDAT
184700010109     C                   Z-ADD     G02INV        v4dtvd
184800010109
184900010109     C                   movel     putv          v4utv
185000010109
185100010109     C                   endsr
185200061017      *--------------------------------------------------------------------*
185300061017      * F9 - Sollecito
185400061017      *--------------------------------------------------------------------*
185500061017     c     sr_sollecito  begsr
185600061017
185700061017      * il sollecito è possibile se l'ORM è automatizzato
185800061017      * p.o. emissione DPD
185900061017     c     v1cpoe        lookup    skpodpd                                30
186000061017     c                   if        *in30 and v1ctco <> 'F'
186100061017     c                   eval      *in28 = *on
186200061017     c                   eval      v1cmsg = msg(05)
186300061017     c                   goto      for01
186400061017     c                   endif
186500061017      * p.o. ritiro DPD
186600061017     c     v1cpor        lookup    skpodpd                                30
186700061017     c                   if        *in30 and v1cfcs <> 'F'
186800061017     c                   eval      *in28 = *on
186900061017     c                   eval      v1cmsg = msg(05)
187000061017     c                   goto      for01
187100061017     c                   endif
187200061017      * se orm in fase 900 o 999 non posso sollecitare
187300061017     c                   if        v1fao > 600
187400061017     c                   eval      *in28 = *on
187500061017     c                   eval      v1cmsg = msg(06)
187600061017     c                   goto      for01
187700061017     c                   endif
187800061018      * il sollecito è possibile se l'ORM è già stato stampato
187900080505     c     v1cpor        lookup    skpodpd                                30
188000080505     c                   if        *in30 and v1cfcs = 'F' and
188100080505     c                             v1dst = *zeros
188200061018     c                   eval      *in28 = *on
188300061018     c                   eval      v1cmsg = msg(04)
188400061018     c                   goto      for01
188500061018     c                   endif
188600061017
188700061017      * p.o. ritiro DPD
188800061017     c     v1cpor        lookup    skpodpd                                30
188900061017      * devo richiamare il pgm di stampa per mandare il sollecito
189000061017      * via fax o mail
189100061017     c                   if        *in30
189200090211      * prima di mandare il fax o la mail verifico se il depot è associato ad un cod.cliente
189300090211     c                   exsr      sr_depot
189400090211      * se non associato errore e non si fa il sollecito
189500090211     c                   if        odepksc = *zeros
189600090211     c                   eval      *in28 = *on
189700090211     c                   eval      v1cmsg = msg(07)
189800171027     c                   eval      %subst(v1cmsg:32:7) = OSID4DDPT
189900090211     c                   goto      for01
190000090211     c                   endif
190100061017     c                   exsr      sr_stampa
190200061017     c                   endif
190300061017
190400061017      * scrivo in automatico la nota per avvenuto sollecito
190500061017     c                   eval      ornpoe = v1cpoe
190600061017     c                   eval      ornnsr = v1nsr
190700061017     c                   eval      ornnor = v1nor
190800061017     c                   eval      ornnrv = v1nrv
190900061017     c                   eval      ornfar = v1fao
191000061017     c                   clear                   wprg
191100061017      * cerco ultimo progressivo relativo alla stessa fase
191200061017     c     kfnorn        setll     fnorn02l
191300061017     c                   do        *hival
191400061017     c     kfnorn        reade     fnorn02l
191500061017     c                   if        %eof(fnorn02l)
191600061017     c                   leave
191700061017     c                   endif
191800061017     c                   eval      wprg = ornprg
191900061017     c                   enddo
192000061017      * scrivo la nota
192100061017     c                   move      data_oggi     ds_data
192200061017     c                   add       1             wprg
192300061017     c                   clear                   fnorn000
192400061017     c                   eval      ornpoe = v1cpoe
192500061017     c                   eval      ornnsr = v1nsr
192600061017     c                   eval      ornnor = v1nor
192700061017     c                   eval      ornnrv = v1nrv
192800061017     c                   eval      ornfar = v1fao
192900061017     c                   eval      orndai = dateu
193000061017     c                   time                    ornori
193100061017     c                   eval      ornprg = wprg
193200061017     c                   eval      ornpue = knmus
193300150309     c                   eval      ornpon = DUTpou
193400061017     c                   eval      ornsns = 'I'
193500061017     c                   eval      ornnot = '** SOLLECITATO IL ' +
193600061017     c                             %editc(ds_giorno:'1') + '/' +
193700061017     c                             %editc(ds_mese:'1') + '/' +
193800061017     c                             %editc(ds_anno:'Z') + ' **'
193900061017     c                   eval      ornftr = 'S'
194000061017     c                   eval      orndtr = dateu
194100061017     c                   write     fnorn000
194200061017
194300061017      * richiamo il pgm di gestione note
194400061017     c                   exsr      sr_note
194500061017
194600061017     c                   endsr
194700090211
194800090211      *--------------------------------------------------------------------*
194900090211      * Controllo il depot
195000090211      *--------------------------------------------------------------------*
195100090211     c     sr_depot      begsr
195200090211
195300171025     c                   clear                   tisid4ds
195400171106     c                   eval      isid4dri = dateu
195500171025     c                   eval      isid4nzd = ormnar
195600171025     c                   eval      isid4cad = ormcar
195700090211     c                   if        ormpkg > §3ilmp
195800171025     c                   eval      isid4sorc = 101
195900090211     c                   else
196000171025     c                   eval      isid4sorc = 136
196100090211     c                   endif
196200171025     c                   eval      isid4lnp = ormpoe
196300171025     c                   call      'TISID4R'
196400171025     c                   parm                    tisid4ds
196500090211      * con il depot richiamo pgm per avere il codice cliente
196600090211      * cliente
196700090211     c                   clear                   truldepds
196800171025     c                   clear                   truldepds1
196900171106     c                   eval      idepdpc = osid4dstr
197000171106     c                   eval      idepdep7 = OSID4DDPT
197100171106     c                   eval      idepdrf = dateu
197200171027     c                   call      'TRULDEPR'
197300090211     c                   parm                    truldepds
197400171027     c                   parm                    truldepds1
197500090211
197600090211     c                   endsr
197700061017
197800061017      *--------------------------------------------------------------------*
197900061017      * Richiama il pgm di stampa per sollecito
198000061017      *--------------------------------------------------------------------*
198100061017     c     sr_stampa     begsr
198200061017
198300061017     c                   clear                   fior16ds
198400061017     c                   clear                   trul90ds
198500061017     c                   movel     'S'           d90rso
198600061017     c                   call      'TRUL90R'
198700061017     c                   parm                    kpjba
198800061017     c                   Parm                    trul90ds
198900061017      * F3=Fine
199000061017     c                   if        d90f3 = '1'
199100061017     c                   goto      endsrstampa
199200061017     c                   endif
199300090202      /free
199400090202         reset ds5P;
199500090202         if  %subst(knmus : 1 : 3) = 'EDP';
199600090202           TBLkey = 'O' + 'EDP';
199700090202         else;
199800090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
199900090202         endif;
200000090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
200100090202         if  %found( TABEL00F );
200200090202           ds5P = TBLuni;
200300090202         endif;
200400090202      /end-free
200500061017     c                   movel     d90mdo        r16mdo
200600061017     c                   movel     d90pro        r16oqo
200700061017     c                   eval      r16tla = *blanks
200800061017      * è sempre sollecito imposto il nuovo tipo
200900061017     c                   eval      r16ris ='X'
201000061017     c                   eval      r16poe = v1cpoe
201100061017     c                   eval      r16nsr = v1nsr
201200061017     c                   eval      r16nor = v1nor
201300061017     c                   eval      r16nrv = v1nrv
201400061017     c                   eval      r16por = v1cpor
201500061017     c                   eval      r16dst = ormdar
201600061017     c                   movel     kpjbu         §kpjbu
201700061017     c                   movel(p)  fior16ds      kpjbu
201800090202     c                   call      §5Ppgm
201900061017     c                   parm                    kpjba
202000080318     c                   movel     kpjbu         fior16ds
202100061017     c                   movel     §kpjbu        kpjbu
202200061017
202300061017     c     endsrstampa   endsr
202400061019
202500061019      *--------------------------------------------------------------------*
202600061023      * F2 - Dati DPD
202700061019      *--------------------------------------------------------------------*
202800061019     c     sr_datidpd    begsr
202900080430
203000090202      * Salvataggio KPJBU
203100090202     c                   eval      §kpjbu = kpjbu
203200090202
203300080430     c                   clear                   fieu55ds
203400080422
203500080430      * se ORM commissionato da DPD passo il n. ORM DPD
203600080430     c     ormpoe        lookup    skpodpd                                30
203700080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
203800080430     c                   eval      i55dep = %subst(ormrfa:4:4)
203900080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
204000081015     c                   eval      i55ie = 'I'
204100080430     c                   eval      kpjbu = fieu55ds
204200080430     c                   call      'FIEU55C'
204300080430     c                   parm                    kpjba
204400080430     c                   endif
204500080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
204600080430     c     ormpor        lookup    skpodpd                                30
204700080430     c                   if        *in30
204800061019     c                   eval      i55poe = v1cpoe
204900061019     c                   eval      i55nrs = v1nsr
205000061019     c                   eval      i55nro = v1nor
205100061019     c                   eval      i55nrv = v1nrv
205200081015     c                   eval      i55ie = 'E'
205300061019     c                   eval      kpjbu = fieu55ds
205400061206     c                   call      'FIEU55C'
205500061019     c                   parm                    kpjba
205600080430     c                   endif
205700090202
205800090202      * Ripristino KPJBU
205900090202     c                   eval      kpjbu = §kpjbu
206000061019
206100061019     c                   endsr
206200090211
206300090211      *--------------------------------------------------------------------*
206400090211      * Window per dati appuntamento ORM
206500090211      *--------------------------------------------------------------------*
206600090211     c     sr_ormapp     begsr
206700090223
206800090223      * pulisco il subfile
206900090223     c                   clear                   nrra              4 0
207000090223     c                   eval      *in21 = *off
207100090223     c                   write     fior07apc
207200090223     c                   eval      *in20 = *on
207300090223     c                   eval      *in21 = *on
207400090223
207500090223      * carico il subfile
207600090216      * metto fissa la fase 100, per ora ho l'appuntamento solo su questa fase
207700090225     c                   eval      *in31 = *off
207800090216     c                   eval      kfar = 100
207900090223     c     kfnora        setll     fnora01l
208000090223     c                   do        *hival
208100090223     c     kfnora        reade     fnora01l
208200090223     c                   if        %eof(fnora01l)
208300090225     c                   eval      *in31 = *on
208400090223     c                   leave
208500090223     c                   endif
208600090211     c                   eval      wappref = oraref
208700090211     c                   eval      wappnota1 = %subst(oranote:1:35)
208800090211     c                   eval      wappnota2 = %subst(oranote:36:35)
208900090223     c                   eval      wappora = oraora
209000090223     c                   eval      wappute = oraute
209100090223     c                   eval      g02inv = oradata
209200090223     c                   eval      g02err = '3'
209300090223     c                   call      'XSRDA8'
209400090223     c                   parm                    wlbdat
209500090223     c                   eval      wappdata = g02dat
209600090223     c                   add       1             nrra
209700090223     c                   write     fior07aps
209800090223     c                   enddo
209900090211
210000090223      * se non ho caricato nessun appuntamento emetto una segnalazione
210100090223     c                   if        nrra = *zeros
210200090223     c                   movel     msg(8)        v1cmsg
210300090223     c                   eval      *in28 = *on
210400090223     c                   leavesr
210500090223     c                   endif
210600090223
210700090223      * se ho caricato emetto il subfile
210800090223     c                   if        nrra > *zeros
210900090225     c                   eval      recsfa = 1
211000090223     c                   do        *hival
211100090223     c                   write     fior07apz
211200090223     c                   exfmt     fior07apc
211300090223      * f12-ritorno
211400090223     c                   if        *inkl
211500090223     c                   leave
211600090223     c                   endif
211700090211     c                   enddo
211800090223     c                   endif
211900090211
212000090211     c                   endsr
212100130913
212200130913      /free
212300131028       //--------------------------------------------------------------------
212400131028       //?F5 - Altri dati.
212500131028       //--------------------------------------------------------------------
212600131028       BEGSR F05D01;
212700131028
212800131028         wEndF05 = *off;
212900131028
213000131028       //?emetto la videata relativa agli altri dati
213100131028         write fior071;
213200131028       //?emetto la window dove richiedo gli altri dati
213300131028         DOW not wEndF05;
213400131028           exfmt fior0712;
213500131028       //?F12-Ritorno
213600131028           IF  *inkl;
213700131028             wEndF05 = *on;
213800131028           ENDIF;
213900131028         ENDDO;
214000131028
214100131028       ENDSR;
214200140110
214300130913       //--------------------------------------------------------------------
214400130913       //?F14 - Interrogazione orari di servizio.
214500130913       //--------------------------------------------------------------------
214600130913       BEGSR F14D01;
214700140206
214800140317       //?Devo trovare la pickup per passarla al trulorsr
214900140206         clear TISI95DS;
215000140206         I95tcn = '3';
215100140206         I95cap = ORMcar;
215200140206         I95nar = ORMnar;
215300140206         I95ind = ORMinr;
215400140206         I95loc = ORMlor;
215500140206         I95prv = ORMprr;
215600140206       //?Peso in base al ntwk fil.ritiro
215700140206         IF  network = 'SDI';
215800140206           I95lkg = 1000;
215900140206         ELSE;
216000140206           clear I95lkg;
216100140206         ENDIF;
216200140206         tisi95r (tisi95ds);
216300140317
216400140317       //?Cerco eventuale ora presunta ritiro
216500140318         clear fior01ds;
216600140317         IF  ORMndc > 0 and ORMfao < 600;
216700140318           IOR01fgs = ORMpor;
216800140318           IOR01ndc = ORMndc;
216900140318           IOR01poe = ORMpoe;
217000140318           IOR01nsr = ORMnsr;
217100140318           IOR01nor = ORMnor;
217200140318           IOR01nrv = ORMnrv;
217300140318           fior01r (fior01ds);
217400140317         ENDIF;
217500130913
217600140317         clear trulorsds;
217700130913         IOREfil = ORMpor;
217800130913         IOREcap = ORMcar;
217900130913         IOREloc = ORMlor;
218000130913         IOREdta = ORMdar;
218100130913         IOREtser = 'R';
218200140206         IOREgf2 = O95gf2;
218300140318         IOREorp = OOR01ora;
218400161115
218500161115       //?Se peso inferiore a 5 kg passo flag
218600161115         clear dIORE01;
218700161115         IF  ORMrmp <> *blanks;
218800161115           §IOREr_mnx = 'S';
218900161115         ENDIF;
219000161115         IOREflo = dIORE01;
219100130913
219200140317         trulorsr (kpjba:trulorsds);
219300130913
219400130913       ENDSR;
219500140110
219600140110       //--------------------------------------------------------------------
219700140110       //?F17 - Note AUT.
219800140110       //--------------------------------------------------------------------
219900140110       BEGSR F17D01;
220000140121
220100140121         *in48 = *off;
220200140110
220300140110         clear fiort1ds;
220400140303         IORT1mod = 'G';
220500140110         IORT1poe = ORMpoe;
220600140110         IORT1nsr = ORMnsr;
220700140110         IORT1nor = ORMnor;
220800140110         IORT1nrv = ORMnrv;
220900140110         IORT1pgm = 'FIOR07R';
221000140303         IF  *in15;
221100140303           IORT1mod = 'I';
221200140303           IORT1dim = V4Dtvd;
221300140303           IORT1oim = V4orv;
221400140303         ENDIF;
221500140114         §kpjbu = kpjbu;
221600140110
221700140320         IF  psce = 'P' and *in15;
221800140320           fiort1c (kpjba:fiort1ds);
221900140320         ELSE;
222000140110         fiort1r (kpjba:fiort1ds);
222100140320         ENDIF;
222200140114
222300140114         kpjbu = §kpjbu;
222400140121
222500140121       //?Controllo se sono state immesse Note AUT
222600140304         IF  not *in15;
222700140304           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
222800140304           IF  %found(FNORT01L);
222900140304             *in48 = *on;
223000140304           ENDIF;
223100140304         ENDIF;
223200140304       //?Controllo se ci sono proposte Note AUT
223300140304         IF  *in15;
223400140304           chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
223500140304           IF  %found(FNORPT1L);
223600140304             *in48 = *on;
223700140304           ENDIF;
223800140304         ENDIF;
223900140110
224000140110       ENDSR;
224100130913
224200130913      /end-free
224300130913
224400090210
224500090210      *------------------------------------------------------------------------*
224600090210      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
224700090210      *------------------------------------------------------------------------*
224800090210     c     sr_f24        begsr
224900090210
225000090210     c                   Select
225100090210
225200090210     c                   When      $loop = 2 and $rig =2  or
225300090210     c                             $loop = 3 and $rig =3
225400090210     c                   Movea     rigatf1       vzfd01
225500090210     c                   Z-add     1             $rig
225600090210
225700090210     c                   If        $loop = 2
225800090210     c                   Eval      vzfd02= cf2412
225900090210     c                   Else
226000090210     c                   Eval      vzfd02= cf2413
226100090210     c                   EndIf
226200090210
226300090210     c                   When      $loop = 2 and $rig =1 or
226400090210     c                             $loop = 3 and $rig =1
226500090210     c                   Movea     rigatf2       vzfd01
226600090210     c                   Z-add     2             $rig
226700090210
226800090210     c                   If        $loop = 2
226900090210     c                   Eval      vzfd02 = cf2422
227000090210     c                   Else
227100090210     c                   Eval      vzfd02 = cf2423
227200090210     c                   EndIf
227300090210
227400090210     c                   When      $loop = 3 and $rig =2
227500090210     c                   Movea     rigatf3       vzfd01
227600090210     c                   Z-add     3             $rig
227700090210     c                   Eval      vzfd02 = cf2433
227800090210     c                   EndSl
227900090210
228000090210     c                   EndSr
228100090210
228200090210      *------------------------------------------------------------------------*
228300090210      *  Imposto descrizione tasti funzione
228400090210      *------------------------------------------------------------------------*
228500090210     c     Sr_Tastifun   BegSr
228600140304
228700140304      * Abilito eventuali tasti funzionali
228800140304     c                   exsr      Abil_Fxx
228900090210
229000090210      * Conta i caratteri riempiti dalle RigaTf1
229100090210     c                   Clear                   $z
229200090210      * Conta i caratteri riempiti dalle RigaTf2
229300090210     c                   Clear                   $k
229400090210      * Conta i caratteri riempiti dalle RigaTf3
229500090210     c                   Clear                   $j
229600090210      * Conta la posizione del campone da cui partire per impostare
229700090210      * la descrizione del tasto funzione
229800090210     c                   Clear                   $y
229900090210
230000090210      * Stringhe che contengono le descrizioni dei tasti funzione
230100090210     c                   Reset                   rigatf1
230200090210     c                   Reset                   rigatf2
230300090210     c                   Reset                   rigatf3
230400090217
230500131028      * F1 - Aggiungi Richiesta Assistenza
230600131028     c                   if        *in08
230700131028     c                   reset                   $tf
230800131028     c                   movea     tf01          $tf
230900131028     c                   exsr      rie_$tf
231000131028     c                   endif
231100090217      * F2 - Dati DPD
231200090217     c                   if        not *in01 and *in14 and
231300140304     c                             not *in12 and not *in15
231400090217     c                   reset                   $tf
231500090217     c                   movea     tf02          $tf
231600090217     c                   exsr      rie_$tf
231700090217     c                   endif
231800090217      * F9 - Sollecito
231900090217     c                   if        not *in01 and *in13
232000140304     c                             and not *in15
232100090217     c                   reset                   $tf
232200090217     c                   movea     tf09          $tf
232300090217     c                   exsr      rie_$tf
232400090217     c                   endif
232500130913      * F14 - Orari servizio
232600131108     c                   if        not *in01
232700131125     c                             and *in44
232800140304     c                             and not *in15
232900130913     c                   reset                   $tf
233000130913     c                   movea     tf14          $tf
233100130913     c                   exsr      rie_$tf
233200130913     c                   endif
233300131120      * F15 - Variazioni
233400131120     c                   if        not *in01 and *in40
233500140304     c                             and not *in15
233600131120     c                   reset                   $tf
233700131120     c                   movea     tf15          $tf
233800131120     c                   exsr      rie_$tf
233900131120     c                   endif
234000131028      * F16 - Gestione Richieste assistenza
234100131108     c                   if        not *in01 and *in09
234200140304     c                             and not *in15
234300131028     c                   reset                   $tf
234400131028     c                   movea     tf16          $tf
234500131028     c                   exsr      rie_$tf
234600131028     c                   endif
234700090217      * F19 - Proposte Variazioni
234800090217     c                   if        not *in01 and *in41
234900140304     c                             and not *in15
235000090217     c                   reset                   $tf
235100090217     c                   movea     tf19          $tf
235200090217     c                   exsr      rie_$tf
235300090217     c                   endif
235400090217      * F22 - Info Appuntamento
235500090217     c                   if        not *in01 and *in43
235600140304     c                             and not *in15
235700090217     c                   reset                   $tf
235800090217     c                   movea     tf22          $tf
235900090217     c                   exsr      rie_$tf
236000090217     c                   endif
236100090211      * F20 - Fasi
236200090211     c                   if        not *in01
236300140304     c                             and not *in15
236400090210     c                   reset                   $tf
236500090211     c                   movea     tf20          $tf
236600090210     c                   exsr      rie_$tf
236700090210     c                   endif
236800090210
236900090210      * Pulisco la stringa
237000090210     c                   Eval      $h = 1
237100090210     c                   For       $h by 1 to 62
237200090210     c                   If        rigatf1($h) = '#'
237300090210     c                   Clear                   rigatf1($h)
237400090210     c                   EndIf
237500090210     c                   EndFor
237600090210     c                   Eval      $h = 1
237700090210     c                   For       $h by 1 to 62
237800090210     c                   If        rigatf2($h) = '#'
237900090210     c                   Clear                   rigatf2($h)
238000090210     c                   EndIf
238100090210     c                   EndFor
238200090210     c                   Eval      $h = 1
238300090210     c                   For       $h by 1 to 62
238400090210     c                   If        rigatf3($h) = '#'
238500090210     c                   Clear                   rigatf3($h)
238600090210     c                   EndIf
238700090210     c                   EndFor
238800090210
238900090210      * Imposto la descrizione del tasto funzione F24 e segnalo
239000090210      * quante righe ho riempito e quale devo emettere
239100090210     c                   Select
239200090210
239300090210     c                   When      $k <> *Zeros and $y <> *Zeros
239400090210     c                   Eval      $loop = 3
239500090210     c                   Eval      vzfd02 = cf2413
239600090210
239700090210     c                   When      $k <> *Zeros and $y = *Zeros
239800090210     c                   Eval      $loop = 2
239900090210     c                   Eval      vzfd02 = cf2412
240000090210
240100090210     c                   Other
240200090210
240300090210     c                   Eval      $loop = 1
240400090210     c                   Clear                   vzfd02
240500090210     c                   EndSl
240600090210
240700090210      * Prima riga di tasti funzione
240800090210     c                   Movea     rigatf1       vzfd01
240900090210     c                   Eval      $rig = 1
241000090210
241100090210     c                   EndSr
241200090210
241300090210      *------------------------------------------------------------------------*
241400090210      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
241500090210      *------------------------------------------------------------------------*
241600090210     c     Rie_$tf       BegSr
241700090210      *
241800090210     c                   Eval      $x = 1
241900090210     c     '#'           Lookup    $tf($x)                                30
242000090210     c                   Add       $x            $z
242100090210     c                   If        $Z <= 63
242200090210     c                   Eval      $j = $Z - $x + 1
242300090210     c                   Movea     $tf           rigatf1($j)
242400090210     c                   Else
242500090210     c                   Add       $x            $k
242600090210     c                   If        $k <= 63
242700090210     c                   Eval      $j = $K - $x + 1
242800090210     c                   Movea     $tf           rigatf2($j)
242900090210     c                   Else
243000090210     c                   Add       $x            $y
243100090210     c                   If        $y <= 63
243200090210     c                   Eval      $j = $y - $x + 1
243300090210     c                   Movea     $tf           rigatf3($j)
243400090210     c                   EndIf
243500090210     c                   EndIf
243600090210     c                   EndIf
243700090210
243800090210     c                   EndSr
243900061017
244000001010      *****************************************************************
244100001010      * ROUTINE INIZIALE
244200001010      *****************************************************************
244300001010     C     *INZSR        BEGSR
244400001009
244500001010     C     *ENTRY        PLIST
244600001010     C                   PARM                    KPJBA
244700040220     c                   parm                    FIDNA1ds
244800001011
244900001011     C                   movel     TIT_A         VTCtit
245000001009
245100001026     C                   movel(p)  kpjbu         parm01
245200001016
245300090202     C                   Z-ADD     1             CODUT             1 0
245400090202     c     *dtaara       define    §azute        AZUTEds
245500090202     c     *dtaara       define    §datiute      dDATIUTE
245600090202     c                   in(E)     *dtaara
245700090202     c                   if        %ERROR or RSUT = *blanks
245800090202     c                   clear                   Tibs34Ds
245900090202     c                   call      'TIBS34R'
246000090202     c                   parm                    Tibs34Ds
246100090202     c                   in        *dtaara
246200090202     c                   endif
246300151001
246400151001      * abilitazione utente
246500151001     c                   clear                   wabi
246600151001     c                   eval      dUTE01 = UTEfaf
246700151001     c                   SELECT
246800151001      * carica abilitazioni del profilo
246900151001     c                   WHEN      §UTEorm <> *blanks
247000151001     c                   eval      wabi = §UTEorm
247100151001     c                   WHEN      UTEaut <> *blanks
247200151001     c                   eval      wabi = UTEaut
247300151001      * se non c'è l'abilitazione
247400151001      * --> se 1° livello, abilitazioni al terminal
247500151001      *     se 2° livello, abilitazioni al punto operativo
247600151001      *     se sede no abilitazioni
247700151001     c                   OTHER
247800151001     c                   IF        DUTlpo = '1'
247900151001     c                   eval      wabi   = 'TP'
248000151001     c                   ENDIF
248100151001     c                   IF        DUTlpo = '2'
248200151001     c                   eval      wabi   = 'PO'
248300151001     c                   ENDIF
248400151001     c                   ENDSL
248500151001
248600151001      * Reperimento delle filiali gestibili dall'utente
248700151001     c                   clear                   TRUL31DS
248800151001     c                   eval      I31abi = wabi
248900151001     c                   eval      I31cdi = DUTdis
249000151001     c                   eval      I31car = DUTare
249100151001     c                   eval      I31cpo = DUTpou
249200151001     c                   call      'TRUL31R'
249300151001     c                   parm                    KPJBA
249400151001     c                   parm                    TRUL31DS
249500151001     c                   IF        O31pog > *zeros
249600151001     c                   movea     O31pog        SKpog
249700151001     c                   ENDIF
249800041001
249900050112      * Se ambiente prova FILTRAPRD
250000050112     c                   If        %subst(knsif:7:1) = 'P'
250100050112     c                   Eval      wlib = 'FILTRAPRD '
250200050112     c                   Else
250300050112      * Se ambiente buono FILTRA201
250400050112     c                   Eval      wlib = 'FILTRA201 '
250500041001     c                   EndIf
250600010226
250700150309      * Da sede
250800150309      * apro il file delle distinte dell'ambiente di filiale
250900010226
251000010226     C                   if        simfel = 0
251100010226     C                   eval      *in12 = *on
251200041001     c                   Clear                   comman
251300041001     c                   Movel(p)  cmdt(1)       comman
251400041001     c                   Eval      %Subst(comman:30:10) = wlib
251500041001     c                   Eval      comman =
251600120612     c                             %trim(comman) + '/FIDST01L)'
251700041001     c                   Call      'QCMDEXC'                            99
251800041001     c                   Parm                    comman
251900041001     c                   Parm                    lenght
252000120612     c  n99              Open      fidst01l
252100041001
252200010226     C                   else
252300010226     C                   eval      *in12 = *off
252400120612     C                   open      fidst01l
252500010226     C                   endif
252600061017
252700061206      * carico sk dei p.o. DPD
252800061017     c                   clear                   xx
252900061017     c     *loval        setll     azorg01l
253000061017     c                   do        *hival
253100061017     c                   read      azorg01l
253200061017     c                   if        %eof(azorg01l)
253300061017     c                   leave
253400061017     c                   endif
253500061017     c                   if        orgfva <> *blanks  or
253600061017     c                             (orgfag <> 'F' and orgfag <> 'A')
253700061017     c                   iter
253800061017     c                   endif
253900061017     c                   eval      og143 = orgde3
254000061017     c                   if        §ogntw = 'DPD'
254100061017     c                   add       1             xx
254200061017     c                   eval      skpodpd(xx) = orgfil
254300061017     c                   endif
254400061017     c                   enddo
254500090211
254600090211      * carico tabella DPD
254700090211     c                   clear                   ds3idp
254800090211     c                   movel     '3I'          tblcod
254900090211     c                   movel(p)  'DPD'         tblkey
255000090211     c     ktab          chain     tabel00f
255100090211     c                   if        %found(tabel00f) and tblflg = *blanks
255200090211     c                   eval      ds3idp = tbluni
255300090211     c                   endif
255400001009
255500001023      * reperisce data e ora
255600001010     C                   TIME                    W0140
255700001010      * UDATE IN GGMMAAAA
255800001010     C                   MOVE      W0140         WDTGIO
255900001010      * UDATE IN AAAAMMGG
256000001010     C                   Z-ADD     WDTGIO        G02DAT
256100001010     C                   MOVEL     *BLANK        G02ERR
256200001010     C                   CALL      'XSRDA8'
256300001010     C                   PARM                    WLBDAT
256400001025     C                   Z-ADD     G02INV        DATEU
256500001010     C     *iso          MOVEL     DATEU         DATA_oggi
256600001010
256700001025     C                   z-add     dateu         §doggi
256800001016
256900001009     C     Kfnor         klist
257000001011     C                   kfld                    kpoe
257100001016     C                   kfld                    knsr
257200001009     C                   kfld                    knor
257300001016     C                   kfld                    knrv
257400001103
257500001103     C     Kforp         klist
257600001103     C                   kfld                    kpoe
257700001103     C                   kfld                    knsr
257800001103     C                   kfld                    knor
257900001103     C                   kfld                    knrv
258000001103     C                   kfld                    kdtv
258100001103     C                   kfld                    korv
258200010305
258300010305     C     Kfnorf        klist
258400010305     C                   kfld                    kpoe
258500010305     C                   kfld                    knsr
258600010305     C                   kfld                    knor
258700010305     C                   kfld                    knrv
258800010305     C                   kfld                    kdfo
258900010305     C                   kfld                    kofo
259000010305     C                   kfld                    kfao
259100090216
259200090216     c     kfnora        klist
259300090216     c                   kfld                    kpoe
259400090216     c                   kfld                    knsr
259500090216     c                   kfld                    knor
259600090216     c                   kfld                    knrv
259700090216     c                   kfld                    kfar
259800010119
259900010119     C     Ktab          klist
260000010119     C                   kfld                    codut
260100010119     C                   kfld                    kcod
260200010119     C                   kfld                    kkey
260300001030
260400120612     C     Kfidst        klist
260500120612     c                   kfld                    knpg
260600001030     C                   kfld                    knfv
260700001030     C                   kfld                    kfgs
260800021202
260900021202     C     Kfapd         klist
261000021202     C                   kfld                    kpdtip
261100021202     C                   kfld                    kpdpdr
261200061017
261300061017     c     kfnorn        klist
261400061017     c                   kfld                    ornpoe
261500061017     c                   kfld                    ornnsr
261600061017     c                   kfld                    ornnor
261700061017     c                   kfld                    ornnrv
261800061017     c                   kfld                    ornfar
261900061017
262000150526      * File FITGD32L
262100150526     c     K02TGD32      klist
262200040220     c                   kfld                    TGDtor
262300040220     c                   kfld                    TGDogg
262400001009
262500001011     C                   eval      kpoe = ppoe
262600001016     C                   eval      knsr = pnsr
262700001011     C                   eval      knor = pnor
262800001016     C                   eval      knrv = pnrv
262900001011
263000001025     C                   eval      v1cpoe = ppoe
263100001016     C                   eval      v1nsr  = pnsr
263200001025     C                   eval      v1nor  = pnor
263300001025     C                   eval      v1nrv  = pnrv
263400001115
263500001115     C                   movel     *zeros        v1ccra
263600001115     C                   movel     *zeros        v1ccor
263700001115     C                   movel     *zeros        v1ccrc
263800001006
263900001006     C                   endsr
264000040220      *****************************************************************
264100040220      * Abilitazione tasti funzionali
264200040220      *****************************************************************
264300040220     c     Abil_Fxx      begsr
264400040220
264500140304      * abilito i tasti funzione seguenti solo se non sono in
264600140304      * Interrogazione Storico Variazioni o Interrogazione Proposte Variazione
264700140304     c                   IF        not *in01 and not *in15
264800140304
264900040220      *
265000040220      *?Abilito F1=Aggiunta in Reclami?
265100040220      * (se richiamato da pgm. FIDNA1R...)
265200040303     c                   eval      *in08  =    (SDSprm >  1
265300040309     c                                      and IA1ins = ' ')
265400040220      *
265500040220      *?Abilito F16=Visualizzazione Reclami?
265600040220      * (se rilevo dei reclami già immessi)
265700040220     c                   eval      TGDtor  = 'O'
265800040220     c                   eval      TGDogg  = ds_OggORM
265900150526     c     K02TGD32      setll     FITGD000
266000150526     c                   eval      *in09   = (%equal(FITGD32L)   and
266100040303     c                                           (SDSprm =  1
266200040303     c                                        or (SDSprm >  1
266300040303     c                                        and IA1ins <> 'N')))
266400070509
266500151002      /free
266600151002       //?Abilito F9=Sollecito
266700151002       //?Se non è utente di sede
266800151002       //?se utente INT
266900151002       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
267000151002         IF  %subst(knmus:1:3) = 'INT' and DUTpou <> 046;
267100151002           xx = 1;
267200151002           FOR xx by 1 to %elem(skpog);
267300151002             IF  skpog(xx) <> *zeros and
267400151002                 %lookup(%dec(skpog(xx):3:0):skpoDPD) > 0;
267500151002               *in13 = *on;
267600151002               leave;
267700151002             ENDIF;
267800151002           ENDFOR;
267900151002         ENDIF;
268000151002      /end-free
268100061019
268200061023      *?Abilito F2=Dati DPD
268300080430      * se non provengo già dal pgm fieu55r
268400061024     c                   if        psce <> 'D'
268500061019     c     ormpoe        lookup    skpodpd                                30
268600061019     c  n30ormpor        lookup    skpodpd                                30
268700140304     c                   eval      *in14 = (*in30 and not *in12)
268800061024     c                   endif
268900140304     c                   ENDIF
269000140121
269100140121      /free
269200140121         *in44 = *off;
269300140526       //?F14 - Orari servizio
269400140304       //?Abilito solo se NON sono in
269500140304       //?Interrogazione Storico Variazioni
269600140304       //?Interrogazione Proposte Variazioni
269700140304         IF  not *in01 and not *in15;
269800140317           *in44 = *on;
269900140304         ENDIF;
270000140304
270100140121      /end-free
270200040220
270300040220     c                   EndSR
270400001010      *---------------------------------------------------------------------------------------------
270500041001** cmdt
270600120612OVRDBF FILE(FIDST01L) TOFILE(
270700001031** MSG  Lungh. 78                                                            *
270800001102Non esistono variazioni per l'ORM selezionato                                       1
270900001102Non esistono fasi per l'ORM selezionato                                             2
271000001102Non esistono proposte per l'ORM selezionato                                         3
271100061017L'ORM non è stato stampato impossibile sollecitare                                  4
271200061017Impossibile fare sollecito per ORM non automatizzato                                5
271300061017Impossibile fare sollecito per ORM già fatto o chiuso                               6
271400171114Impossibile sollecitare. Depot xxxxxxx non associato ad un cliente                  7
271500090223Non esistono appuntamenti per l'ORM selezionato                                     8
271600131028** TF01
271700131211F1=Agg.Rich.  #
271800090211** TF02
271900131211F2=Dati DPD  #
272000090211** TF09
272100131211F9=Sollecito  #
272200130913** TF14
272300131211F14=OrariSr  #
272400131120** TF15
272500131211F15=Variazioni  #
272600131028** TF16
272700131211F16=Gest.Rich.  #
272800140110** TF19
272900131211F19=Proposte Var.  #
273000090217** TF20
273100131211F20=Fasi  #
273200090217** TF22
273300131211F22=Info App.  #
