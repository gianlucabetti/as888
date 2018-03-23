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
024400090211     d tisie3ds      e ds                  inz
024500090211     d truldepds     e ds
024600090211     d ds3idp        e ds
024700140206     d TISI95DS      e ds
024800151001     d dUTE01        e ds
024900001010
025000001010      *
025100001010     D KPJBA         E DS
025200040220     D FIDNA1ds      e ds
025300040226      *
025400040226     d FIDNA2ds      e ds                  inz
025500040226     d  IA2mod       e                     inz('M')
025600040226     d  IA2tor       e                     inz('O')
025700130913
025800130913      * - Parametri x Interrogazione orari servizio
025900140317     d TRULORSds     e ds
026000090202
026100090202      * - Parametri x Controllo profilo utenti
026200090202     d TIBS34ds      e ds
026300090202      * - Ds di riferimento al file esterno AZUTE00F
026400090202     d AzuteDs       e ds                  extname(AZUTE00F)
026500090202      * - Ds per dati organigramma
026600090202     d dDatiUte      e ds
026700140110
026800140110      // ds per note AUT
026900140110     d FIORT1DS      e ds
027000131028
027100131028      // ds per rcd 'O' FNORE - orari apertura
027200131028     d dOREorari     e ds
027300140429
027400140429      // ds per rcd 'G' FNORE - generale
027500140429     d dOREgen       e ds
027600140429
027700140429      // ds per rcd 'S' FNORE - SMS
027800140429     d dOREsms       e ds
027900140318
028000140318      // ds per recupero ora presunta ritiro da PDA
028100140318     d FIOR01DS      e ds
028200151001
028300151001      // Carico filiali gestite dall'utente
028400151001     d TRUL31DS      e ds
028500090202
028600001010      *
028700001010     D                SDS
028800040220     d  SDSprm           *parms
028900001010     D  VTCPGM                 1     10
029000090210
029100090210      *   C O S T A N T I
029200090210     d cf2413          c                   const('F24=AlTasti(1/3)')
029300090210     d cf2423          c                   const('F24=AlTasti(2/3)')
029400090210     d cf2433          c                   const('F24=AlTasti(3/3)')
029500090210     d cf2412          c                   const('F24=AlTasti(1/2)')
029600090210     d cf2422          c                   const('F24=AlTasti(2/2)')
029700140110
029800130913      //---------------------------------------------------------------
029900130913      //?Definizione procedure utilizzate.
030000130913      //---------------------------------------------------------------
030100140110
030200140110      // - Gestione note AUT
030300140110     d fiort1r         pr                  extpgm('FIORT1R')
030400140110     d  kpjba                              likeds(KPJBA)
030500140110     d  fiort1ds                           likeds(FIORT1DS)
030600140320
030700140320      // - Gestione note AUT x proposte
030800140320     d fiort1c         pr                  extpgm('FIORT1C')
030900140320     d  kpjba                              likeds(KPJBA)
031000140320     d  fiort1ds                           likeds(FIORT1DS)
031100130913
031200130913      // -Interrogazione orari di servizio
031300140317     d trulorsr        pr                  extpgm('TRULORSR')
031400130913     d  kpjba                              likeds(KPJBA)
031500140317     d  trulorsds                          likeds(TRULORSDS)
031600140206
031700140206      // - Cappario
031800140206     d tisi95r         pr                  extpgm('TISI95R')
031900140206     d  tisi95ds                           likeds(TISI95DS)
032000140318
032100140318      // - Recupero ora presunta ritiro
032200140318     d fior01r         pr                  extpgm('FIOR01R')
032300140318     d  fior01ds                           likeds(FIOR01DS)
032400131125
032500131125      //---------------------------------------------------------------
032600131125      //?Definizione prototipi utilizzati.
032700131125      //---------------------------------------------------------------
032800131125
032900131125      /copy gaitrasrc/srcprotopr,tibs02r
033000131028
033100131028      //---------------------------------------------------------------
033200131028      //?Definizione key-list.
033300131028      //---------------------------------------------------------------
033400131028
033500131028       // - File FNORE01L
033600131029     d k05fnore      e ds                  extname(FNORE01L:*key)
033700131028     d                                     prefix(k_)
033800131028     d                                     inz
033900131029
034000131029       // - File FNORVE1L
034100131029     d k07fnorve     e ds                  extname(FNORVE1L:*key)
034200131029     d                                     prefix(k_)
034300131029     d                                     inz
034400131029
034500131029       // - File FNORPE1L
034600131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
034700131029     d                                     prefix(k_)
034800131029     d                                     inz
034900001010
035000001006      *------------------------------------------------------------------------*
035100001103
035200001030      * Carica i dati dal file FNORM
035300001030     C     kfnor         chain     fnorm01L
035400001115     C                   if        %found(fnorm01l)
035500001115      * Annullato
035600001115     C                   if        ORMatb <> *blanks
035700001115     C                   movel     'ANNULLATO'   v1ann
035800001115     C                   else
035900001115     C                   clear                   v1ann
036000001115     C                   endif
036100001103      * Se è richiamato con 'P' deve solo gestire le proposte
036200001103     C                   if        psce = 'P'
036300001103     C                   goto      richiama
036400001103     C                   endif
036500010315
036600010109     C                   else
036700010109     C                   if        psce = 'P' and pric = 'P'
036800010109     C                   movel     'E'           psce
036900010109     C                   movel     parm01        kpjbu
037000010109     C                   goto      richiama
037100010109     C                   endif
037200001009     C                   endif
037300040220
037400040220      * Abilito eventuali tasti funzionali
037500140304     c                   exsr      Abil_Fxx
037600001006
037700001006      * Emissione 1° Videata
037800001010
037900001006     C                   do        *hival
038000120612     c                   eval      *in01 = *off
038100140303     c                   eval      *in15 = *off
038200010315     C                   exsr      Sr_Carvid
038300010315     C                   exsr      Sr_Decodifica
038400090211     c                   ExSr      Sr_Tastifun
038500001010
038600061017     c     for01         tag
038700001031     C                   eval      v1mod = 'VISUALIZZAZIONE'
038800001031
038900001103     C                   setoff                                       010203
039000140304     c                   setoff                                       1715
039100040908     c                   Eval      *In10 = *Off
039200001030     C                   exfmt     fior071
039300001031     C                   setoff                                       28
039400001011
039500001011      * F12=Ritorno
039600001023     C                   if        *inkl = *on
039700001023     C                   leave
039800001023     C                   endif
039900040220
040000040220      *
040100040220      *?F1 - Aggiunta bolla in reclamo?
040200040226if  1c                   if        *inKA  = *on
040300040220     c                   clear                   FIDNA1ds
040400040220     c                   eval      IA1ins = 'S'
040500040220     c                   eval      IA1tor = 'O'
040600040220     c                   eval      IA1ogg = ds_OggORM
040700040226     c                   leave
040800040220e   1c                   endif
040900040226      *
041000040226      *?F16 - Gestione reclami?
041100040226if  2c                   if        *inKQ  = *on
041200040226     c                   reset                   FIDNA2ds
041300040226if  3c                   if        SDSprm > 1
041400040226     c                   eval      IA2mod = 'I'
041500040226e   3c                   endif
041600040226     c                   eval      IA2ogg = ds_OggORM
041700040226     c                   call      'FIDNA2R'
041800040226     c                   parm                    KPJBA
041900040226     c                   parm                    FIDNA2ds
042000040226if  3c                   if        OA2f03 = 'S'
042100040226     c                   leave
042200040226e   3c                   endif
042300040226e   2c                   endif
042400130913
042500130913      /free
042600130913
042700130913       //?F14=Interrogazione oraro di servizio
042800130913       IF  *inkn;
042900130913         exsr F14D01;
043000130913         iter;
043100130913       ENDIF;
043200131028
043300131028       //?F5=Altri dati
043400131028       IF  *inke;
043500131028         exsr F05D01;
043600131028         iter;
043700131028       ENDIF;
043800140110
043900140110       //?F17=Note AUT
044000140110       IF  *inkr;
044100140110         exsr F17D01;
044200140110         iter;
044300140110       ENDIF;
044400130913
044500130913      /end-free
044600001030
044700010402      * F20=Fasi
044800010402     C                   if        *inku = *on
044900001031     C                   exsr      Sr_Fasi
045000001030     C                   endif
045100120612
045200131120      * F15=Variazioni
045300131120     C                   if        *inkp = *on
045400001102     C                   exsr      Sr_Variazioni
045500001030     C                   endif
045600001011
045700001025      * F18=Note
045800001025     C                   if        *inks = *on
045900001025     C                   exsr      Sr_Note
046000001025     C                   endif
046100001020      * F19=Proposte
046200001030     C                   if        *inkt = *on
046300001031     C                   exsr      Sr_Proposte
046400001020     C                   endif
046500061017      * F9=Sollecito
046600061017     c                   if        *inki
046700061017     c                   exsr      sr_sollecito
046800061017     c                   endif
046900061023      * F2=Dati DPD
047000061023     c                   if        *inkb
047100061019     c                   exsr      sr_datidpd
047200061019     c                   endif
047300090211      * F22=Info appuntamento
047400090211     c                   if        *inkw
047500090211     c                   exsr      sr_ormapp
047600090211     c                   endif
047700090210      * F24=Altri tasti  ?
047800090210     c                   if        *inky and $loop > 1
047900090210     c                   exsr      sr_f24
048000090210     c                   goto      for01
048100090210     c                   endif
048200001010
048300001006     C                   enddo
048400001023
048500001103     C     richiama      tag
048600001103
048700010109     C                   if        psce = 'P'
048800010109     C                   if        pric = *blanks
048900001103     C                   exsr      Sr_Proposte
049000010109     C                   endif
049100010109
049200010109     C                   if        pric = 'P'
049300010109     C                   exsr      Sr_Cardatiprop
049400010109     C                   exsr      Sr_Visproposte
049500010109     C                   endif
049600001103     C                   endif
049700001010
049800001006     C                   eval      *inlr = *on
049900001009      **********************************************************************
050000001023      * CARICA DATI NELLA VIDEATA DAL FILE
050100001009      **********************************************************************
050200001009     C     Sr_Carvid     begsr
050300001009
050400001129     C                   movel     ORMtco        v1ctco
050500001016     C                   movel     ORMtor        v1ctor
050600001023      * Inversione data emissione ORM
050700001016     C                   clear                   wlbdat
050800001017     C                   z-add     ORMdao        G02inv
050900001017     C                   movel     '3'           G02err
051000001016     C                   call      'XSRDA8'
051100001016     C                   parm                    wlbdat
051200001025     C                   z-add     G02dat        v1dao
051300001215     C                   movel     ORMoao        v1oao
051400001016      * Dati Ordinante
051500001025     C                   z-add     ORMcor        v1ccor
051600001025     C                   movel     ORMrso        v1rso
051700001025     C                   movel     ORMino        v1ino
051800001025     C                   movel     ORMcao        v1cao
051900001025     C                   movel     ORMloo        v1loo
052000001025     C                   movel     ORMpro        v1pro
052100001025     C                   movel     ORMnao        v1nao
052200001016      * Dati Ritiro a
052300001025     C                   z-add     ORMcra        v1ccra
052400001025     C                   movel     ORMrsr        v1rsr
052500001025     C                   movel     ORMinr        v1inr
052600001025     C                   movel     ORMcar        v1car
052700001025     C                   movel     ORMlor        v1lor
052800001025     C                   movel     ORMprr        v1prr
052900001025     C                   movel     ORMnar        v1nar
053000001016     C                   movel     ORMrer        v1rer
053100001016     C                   movel     ORMter        v1ter
053200001016      * Dati Consegna a
053300001025     C                   z-add     ORMcrc        v1ccrc
053400001016     C                   movel     ORMrsc        v1rsc
053500001016     C                   movel     ORMinc        v1inc
053600001016     C                   movel     ORMloc        v1loc
053700001016     C                   movel     ORMcac        v1cac
053800001016     C                   movel     ORMprc        v1prc
053900001016     C                   movel     ORMnac        v1nac
054000001016
054100010315     C                   if        ORMffd = 'S'
054200010315     C                   eval      v1ffd = 'SI'
054300010315     C                   else
054400010315     C                   eval      v1ffd = 'NO'
054500010315     C                   endif
054600010315
054700001016     C                   movel     ORMnam        v1nam
054800001023      * Inversione data ritiro
054900001017     C                   z-add     ORMdar        G02inv
055000001017     C                   movel     '3'           G02err
055100001016     C                   call      'XSRDA8'
055200001016     C                   parm                    wlbdat
055300001025     C                   z-add     G02dat        v1dar
055400001016     C                   movel     ORMorr        v1orr
055500001016
055600001025     C                   z-add     ORMncl        v1ncl
055700001025     C                   z-add     ORMpkg        v1pkg
055800001025     C                   z-add     ORMvlm        v1vlm
055900001025     C                   z-add     ORMbnc        v1bnc
056000001025     C                   z-add     ORMblc        v1blc
056100001025     C                   z-add     ORMatt        v1att
056200001025     C                   z-add     ORMmtc        v1mtc
056300001016
056400010315     C                   if        ORMpag = 'M'
056500010315     C                   eval      v1pag = 'Mitt.'
056600010315     C                   endif
056700010315     C                   if        ORMpag = 'D'
056800010315     C                   eval      v1pag = 'Dest.'
056900010315     C                   endif
057000010315     C                   if        ORMpag = 'O'
057100010315     C                   eval      v1pag = 'Ord. '
057200010315     C                   endif
057300010315
057400001025     C                   z-add     ORMksc        v1ksc
057500001020     C                   movel     ORMctr        v1ctr
057600010315
057700010315     C                   if        ORMddt = 'S'
057800010315     C                   eval      v1ddt = 'SI'
057900010315     C                   endif
058000010315     C                   if        ORMddt = 'N'
058100010315     C                   eval      v1ddt = 'NO'
058200010315     C                   endif
058300010315
058400001016     C                   movel     ORMpor        v1cpor
058500001016     C                   movel     ORMpoc        v1cpoc
058600001016     C                   movel     ORMno1        v1not1
058700001016     C                   movel     ORMno2        v1not2
058800001025     C                   z-add     ORMfao        v1fao
058900001025     C                   z-add     ORMpos        v1cpos
059000001025     C                   z-add     ORMors        v1ors
059100001027     C                   movel     ORMrfa        v1rfa
059200001221     C                   move      ORMsto        v1sto
059300001030     C                   z-add     ORMndc        v1ndc
059400001030      * Inversione data distinta
059500001030     C                   clear                   wlbdat
059600001030     C                   z-add     ORMddc        G02inv
059700001030     C                   movel     '3'           G02err
059800001030     C                   call      'XSRDA8'
059900001030     C                   parm                    wlbdat
060000001030     C                   z-add     G02dat        v1ddc
060100010226
060200010226      * Cerca codice autotrasportatore
060300071108     c                   if        ormndc > 0
060400001030     C                   z-add     ORMndc        knfv
060500001030     C                   z-add     ORMpor        kfgs
060600120612     C     kfidst        chain     fidst01l
060700060515      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
060800120612     c                   if        not %found(fidst01l)
060900060515     c                   clear                   fnlv55ds
061000060515     c                   eval      d55tpt = '6'
061100060515     c                   move      ormpor        d55lin
061200060515     c                   eval      d55drf = ormddc
061300060515     c                   call      'FNLV55R'
061400060515     c                   parm                    fnlv55ds
061500060515     c                   if        d55err = *blanks and d55tfa <> d55lin
061600060515     c                   move      d55tfa        kfgs
061700120612     c     kfidst        chain     fidst01l
061800060515     c                   endif
061900060515     c                   endif
062000120612     C                   if        %found(fidst01l)
062100001030     C                   eval      v1cpdc = DSTpdr
062200001030     C                   endif
062300071108     c                   else
062400071108     c                   clear                   v1cpdc
062500071108     c                   endif
062600071108
062700010226      * Autotrasportatore evidenziato a video
062800010226     C                   eval      *in06 = (v1cpdc > 0)
062900010226
063000010219      * Distinta evidenziata a video
063100010219     C                   eval      *in05 = (v1ndc > 0)
063200070924
063300070924      * se Stampato faccio vedere la scritta
063400070924     c                   eval      v1dst = ormdst
063500070924     c                   eval      *in16 = (v1dst > 0)
063600010119
063700010315     C                   if        ORMspi = 'S'
063800010315     C                   eval      v1spi = 'SI'
063900010315     C                   else
064000010315     C                   eval      v1spi = 'NO'
064100010315     C                   endif
064200010309      * Causale
064300010305     C                   exsr      Sr_Carcau
064400010322
064500010322     C                   eval      *in40 = *off
064600010322     C                   eval      *in41 = *off
064700010322     C                   eval      *in42 = *off
064800090211     C                   eval      *in43 = *off
064900131211     C                   eval      *in46 = *off
065000140121     C                   eval      *in48 = *off
065100131211
065200010322      * Controllo se ci sono Variazioni
065300010322     C     kfnor         setll     fnorv01l
065400010322     C                   do        *hival
065500010322     C     kfnor         reade     fnorv01l
065600010322     C                   if        %eof
065700010322     C                   leave
065800010322     C                   endif
065900010322     C                   eval      *in40 = *on
066000010322     C                   leave
066100010322     C                   enddo
066200010322      * Controllo se ci sono Proposte
066300010322     C     kfnor         setll     fnorp01l
066400010322     C                   do        *hival
066500010322     C     kfnor         reade     fnorp01l
066600010322     C                   if        %eof
066700010322     C                   leave
066800010322     C                   endif
066900010322     C                   eval      *in41 = *on
067000010322     C                   leave
067100010322     C                   enddo
067200010322      * Controllo se ci sono Note
067300061017     C     kfnor         setll     fnorn02l
067400010322     C                   do        *hival
067500061017     C     kfnor         reade     fnorn02l
067600010322     C                   if        %eof
067700010322     C                   leave
067800010322     C                   endif
067900010322     C                   eval      *in42 = *on
068000010322     C                   leave
068100010322     C                   enddo
068200090211
068300090211      * controllo se inserite info appuntamento
068400090211     c     kfnor         setll     fnora01l
068500090211     c                   do        *hival
068600090211     c     kfnor         reade     fnora01l
068700090211     c                   if        %eof
068800090211     c                   leave
068900090211     c                   endif
069000090211     c                   eval      *in43 = *on
069100090211     c                   leave
069200090211     c                   enddo
069300030606
069400030606     C                   eval      dorm01 = ORMflo
069500030606
069600030606      * ORM commissionato
069700030606     c                   If        §orCom = 'S'
069800030606     c                   Eval      V1dCom = 'COMMISSIONATO'
069900030606     c                   Else
070000030606     c                   Clear                   V1dCom
070100030606     c                   EndIf
070200050502
070300050502      * ORM con Ricevuta di Ritiro
070400050502     c                   Eval      *In11 = (§orsrm = 'S')
070500100225
070600100225      /free
070700140429       //?Imposto costante se ORM con preavviso (CEVA)
070800100225       *in18 = *off;
070900100225       IF  §ormpre <> *blanks;
071000100225         *in18 = *on;
071100100225         IF  §ormpre = 'M';
071200100225         v1este = 'Preavviso: '+
071300100225                  'Mail';
071400100225         ENDIF;
071500100225       ENDIF;
071600100225      /end-free
071700061017
071800061017     c                   eval      v1cfcs = ormfcs
071900070911      * giro
072000070911     c                   clear                   v1ccgi
072100070911     c     kfnor         chain     fnorg01l
072200070911     c                   if        %found(fnorg01l)
072300070911     c                   eval      v1ccgi = orgcgi
072400070911     c                   endif
072500131028
072600131028      /free
072700131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
072800131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
072900131211           *in46 = *on;
073000131211         ENDIF;
073100160513
073200160513         k_OREpoe = ORMpoe;
073300160513         k_OREnsr = ORMnsr;
073400160513         k_OREnor = ORMnor;
073500160513         k_OREnrv = ORMnrv;
073600160513
073700160513       //?Recupero dati da estensione FNORE
073800160513       //?Rcd 'G ' --> Generale
073900160513         k_OREtrc = 'G';
074000160513         clear dOREgen;
074100160513         chain  %kds(k05fnore) FNORE01L;
074200160513         IF  %found(FNORE01L);
074300160513           dOREgen = OREdati;
074400160513         ENDIF;
074500160513
074600131028       //?Recupero dati da estensione FNORE
074700131028       //?Rcd 'O ' --> Orari apertura
074800131028         k_OREtrc = 'O ';
074900131028         clear dOREorari;
075000131028       //?Aggancio il rcd
075100140429         chain  %kds(k05fnore) FNORE01L;
075200131028         IF  %found(FNORE01L);
075300131028           dOREorari = OREdati;
075400131028         ENDIF;
075500131028         v1OraAMda = §OREoramda;
075600131028         v1OraAMa  = §OREorama;
075700131028         v1OraPMda = §OREorapda;
075800131028         v1OraPMa  = §OREorapa;
075900140429
076000140429       //?Imposto costante se ORM con Alert via Mail/SMS
076100160513         IF  §OREfimo = 'S' or §OREfiso = 'S';
076200160513           IF  not *in18;
076300160513             V1este = 'Preavviso: Mail/SMS';
076400160513             *in18 = *on;
076500160513           ENDIF;
076600160513         ENDIF;
076700160224
076800160224       //?Recupero NPR
076900160224         k_OREtrc = 'NP';
077000160224         clear OREdati;
077100160224       //?Aggancio il rcd
077200160224         chain  %kds(k05fnore) FNORE01L;
077300160224         IF  %found(FNORE01L);
077400160224           V1npr = OREdati;
077500160224         ENDIF;
077600140121
077700140121       //?Controllo se ci sono Note AUT
077800140121         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
077900140121         IF  %found(FNORT01L);
078000140121           *in48 = *on;
078100140121         ENDIF;
078200131211
078300131028      /end-free
078400001009
078500001009     C                   endsr
078600001030      **********************************************************************
078700001030      * DECODIFICHE
078800001030      **********************************************************************
078900001030     C     Sr_Decodifica begsr
079000001030
079100001030      * Fase
079200001129     C                   clear                   v1dfao
079300001030     C                   clear                   TIBS02DS
079400001030     C                   movel     'C'           T02mod
079500001030     C                   movel     knsif         t02sif
079600001030     C                   movel     'FAR'         t02cod
079700001030     C                   movel(p)  v1fao         T02ke1
079800001030     C                   call      'TIBS02R'
079900001030     C                   parm                    KPJBA
080000001030     C                   parm                    TIBS02DS
080100040908     c                   Eval      dFar = T02Uni
080200040908     c                   Eval      V1dfao = d§fardes
080300001030
080400001030      * Tipo ordine assegnazione
080500001129     C                   clear                   v1dtco
080600001030     C                   clear                   TIBS02DS
080700001030     C                   movel     'C'           T02mod
080800001030     C                   movel     knsif         t02sif
080900001129     C                   movel     'TCO'         t02cod
081000001129     C                   movel(p)  v1ctco        T02ke1
081100001030     C                   call      'TIBS02R'
081200001030     C                   parm                    KPJBA
081300001030     C                   parm                    TIBS02DS
081400001129     C                   movel     t02uni        v1dtco
081500001030
081600001030      * P.O. ritiro
081700061003     c                   eval      *in22 = *off
081800061003     c                   clear                   v1dfcs
081900061003     c                   clear                   og143
082000001129     C                   clear                   v1dpor
082100140206     c                   clear                   network
082200001030     C                   movel     v1cpor        kazorg
082300001030     C     kazorg        chain     azorg01L
082400001030     C                   if        %found(azorg01l)
082500001030     C                   movel     ORGdes        v1dpor
082600140206     C                   movel     orgdit        network
082700061003     c                   eval      og143 = orgde3
082800001030     C                   endif
082900061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
083000061003      * europolitan invece ormai è obsoleta
083100061003     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
083200061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
083300061003      * partener se caricata e la visualizzo al posto della zona
083400061017     c                   if        v1cfcs <> *blanks
083500061003     c                   clear                   tibs02ds
083600061003     c                   eval      t02mod = 'C'
083700061003     c                   eval      t02sif = knsif
083800061003     c                   eval      t02cod = 'MIP'
083900061017     c                   eval      t02ke1 = v1cfcs
084000061003     c                   call      'TIBS02R'
084100061003     c                   parm                    kpjba
084200061003     c                   parm                    tibs02ds
084300061003     c                   eval      v1dfcs = t02uni
084400061003     c                   eval      *in22 = *on
084500061003     c                   endif
084600061003     c                   endif
084700070911
084800070911      * Giro
084900070911     c                   clear                   v1dcgi
085000070911     c                   clear                   fidg09ds
085100070911     c                   eval      d09iop0 = '001'
085200071127     c                   eval      d09ifgs = v1cpor
085300070911     c                   eval      d09idat = *date
085400070911     c                   eval      d09icgi = v1ccgi
085500070911     c                   eval      d09itug = 'R'
085600080318     c                   movel     kpjbu         §kpjbu
085700070911     c                   eval      kpjbu = fidg09ds
085800070911     c                   call      'FIDG09R'
085900070911     c                   parm                    kpjba
086000070911     c                   eval      fidg09ds = kpjbu
086100080318     c                   movel     §kpjbu        kpjbu
086200070911     c                   if        d09ocgi <> ' '
086300070911     c                   eval      v1dcgi = d09odes
086400070911     c                   endif
086500001030
086600001030      * P.O. Consegna
086700001129     C                   clear                   v1dpoc
086800001030     C                   movel     v1cpoc        kazorg
086900001030     C     kazorg        chain     azorg01L
087000001030     C                   if        %found(azorg01l)
087100001030     C                   movel     ORGdes        v1dpoc
087200001030     C                   endif
087300001030
087400001030      * Tipo ordine
087500001129     C                   clear                   v1dtor
087600001030     C                   clear                   TIBS02DS
087700001030     C                   movel     'C'           T02mod
087800001030     C                   movel     knsif         t02sif
087900001030     C                   movel     'TOR'         t02cod
088000001030     C                   movel(p)  v1ctor        T02ke1
088100001030     C                   call      'TIBS02R'
088200001030     C                   parm                    KPJBA
088300001030     C                   parm                    TIBS02DS
088400001030     C                   movel     t02uni        v1dtor
088500001103
088600010205      * Priorità
088700010205     C                   clear                   v1dsto
088800010205     C                   if        v1sto <> *zeros
088900001103     C                   clear                   TIBS02DS
089000001103     C                   movel     'C'           T02mod
089100001103     C                   movel     knsif         t02sif
089200001103     C                   movel     'STO'         t02cod
089300131029     C                   eval      t02ke1 = %editc(ORMsto:'X')
089400001103     C                   call      'TIBS02R'
089500001103     C                   parm                    KPJBA
089600001103     C                   parm                    TIBS02DS
089700010205     C                   movel     t02uni        v1dsto
089800010205     C                   endif
089900001030
090000010226      * Autotrasportatore
090100001129     C                   clear                   v1dpdc
090200021202     C                   movel     v1cpdc        kpdpdr
090300021202     C     kfapd         chain     fiapd01L
090400021202     C                   if        %found(fiapd01l)
090500001030     C                   movel     APDrsc        v1dpdc
090600001030     C                   endif
090700001031      * ORM fisso
090800001031     C                   eval      *in04 = (v1ors <> *zeros)
090900001030
091000001030     C                   endsr
091100010305      **********************************************************************
091200010305      * Cerca causale chiusura e decodifica
091300010305      **********************************************************************
091400010305     C     Sr_Carcau     begsr
091500010305
091600010305     C                   eval      kpoe = ORMpoe
091700010305     C                   eval      knsr = ORMnsr
091800010305     C                   eval      knor = ORMnor
091900010305     C                   eval      knrv = ORMnrv
092000010305     C                   eval      kdfo = ORMdfo
092100010305     C                   eval      kofo = ORMofo
092200010305     C                   eval      kfao = ORMfao
092300110418     C**!!!kfnorf        chain     fnorf01L
092400110418     C**!!!              if        %found(fnorf01l)
092500110418     c     kfnorf        setll     fnorf01l
092600110418     c                   do        *hival
092700110418     c     kfnorf        reade     fnorf01l
092800110418     c                   if        %eof(fnorf01l)
092900110418     c                   leave
093000110418     c                   endif
093100010305     C                   eval      v1cau = orfcar
093200110418     C                   enddo
093300010305
093400010305     C                   clear                   dcmr
093500010305     C                   clear                   TIBS02DS
093600010305     C                   movel     'C'           T02mod
093700010305     C                   movel     knsif         t02sif
093800010305     C                   movel     'CMR'         t02cod
093900010305     C                   movel(p)  v1cau         T02ke1
094000010305     C                   call      'TIBS02R'
094100010305     C                   parm                    KPJBA
094200010305     C                   parm                    TIBS02DS
094300010305     C                   movel     t02uni        dcmr
094400010305     C                   movel     d§cmrdes      v1dcau
094500010309
094600010309     C                   eval      *in07 = (v1cau <> *blanks)
094700010305
094800010305     C                   endsr
094900001031      **********************************************************************
095000081125      * F20 - Fasi
095100001031      **********************************************************************
095200001031     C     Sr_Fasi       BEGSR
095300001031
095400001031     C                   z-add     1             recsff
095500001031      * Pulisco subfile
095600001031     C                   z-add     *zeros        nrrf              4 0
095700001031     C                   eval      *in21 = *off
095800001031     C                   write     fior07cf
095900001031     C                   eval      *in20 = *on
096000001031     C                   eval      *in21 = *on
096100001031
096200001031      * Carico il subfile in base alle scelte fatte
096300001031     C     kfnor         setll     fnorf01l
096400001031     C                   do        *hival
096500001031     C     kfnor         reade     fnorf01l
096600001031     C                   if        %eof
096700001031     C                   leave
096800001031     C                   endif
096900001031
097000001031     C                   z-add     v1cpoe        v3cpoe
097100001031     C                   z-add     v1nsr         v3nsr
097200001031     C                   z-add     v1nor         v3nor
097300001031     C                   z-add     v1nrv         v3nrv
097400081125
097500081125     c                   eval      dorf01 = orfflo
097600001031
097700001031     C                   exsr      Sr_CarFasi
097800001031     C                   enddo
097900001031      * Se non ho caricato emetto una videata di segnalazione.
098000001031     C                   if        nrrf = *zeros
098100001031     C                   movel     msg(2)        v1cmsg
098200001031     C                   seton                                        28
098300001031     C                   goto      endfasi
098400001031     C                   endif
098500001031
098600001031      * Se ho caricato emetto il subfile
098700001031     C                   if        nrrf > *zeros
098800001102     C                   do        *hival
098900001031     C                   write     fior073
099000001031     C                   exfmt     fior07cf
099100001031      * F12=Ritorno
099200001102     C   kl              leave
099300001102     C                   enddo
099400001102     C                   endif
099500001031
099600001031     C     endfasi       endsr
099700001031      **********************************************************************
099800001031      * CARICO SUBFILE FASI
099900001031      **********************************************************************
100000001031     C     Sr_CarFasi    begsr
100100001031
100200001031      * Inversione data modifica
100300001031     C                   z-add     ORFdae        G02inv
100400001031     C                   movel     '3'           G02err
100500001031     C                   call      'XSRDA8'
100600001031     C                   parm                    wlbdat
100700001031     C                   movel     G02dat        v3dae
100800001031
100900001031     C                   movel     ORFore        v3ore
101000001031     C                   eval      v3cpog = ORFpog
101100001031     C                   eval      v3ute  = ORFpue
101200001031     C                   eval      v3far  = ORFfar
101300001031      * Decodifica fase
101400001031     C                   clear                   TIBS02DS
101500001031     C                   movel     'C'           T02mod
101600001031     C                   movel     knsif         t02sif
101700001031     C                   movel     'FAR'         t02cod
101800001031     C                   movel(p)  v3far         T02ke1
101900001031     C                   call      'TIBS02R'
102000001031     C                   parm                    KPJBA
102100001031     C                   parm                    TIBS02DS
102200040908     C**!!!              movel     t02uni        v3dfar
102300040908     c                   Eval      dFar = T02Uni
102400040908     c                   Eval      V3dfar = d§fardes
102500001031
102600001031     C                   if        ORFcar <> *blanks
102700001031     C                   seton                                        02
102800001031     C                   eval      v3car  = ORFcar
102900001031      * Decodifica causale
103000010301     C                   clear                   dcmr
103100001031     C                   clear                   TIBS02DS
103200001031     C                   movel     'C'           T02mod
103300001031     C                   movel     knsif         t02sif
103400001128     C                   movel     'CMR'         t02cod
103500001031     C                   movel(p)  v3car         T02ke1
103600001031     C                   call      'TIBS02R'
103700001031     C                   parm                    KPJBA
103800001031     C                   parm                    TIBS02DS
103900010301     C                   movel     t02uni        dcmr
104000010301     C                   movel     d§cmrdes      v3dcar
104100001107     C                   else
104200001107     C                   setoff                                       02
104300001107     C                   clear                   v3car
104400001107     C                   clear                   v3dcar
104500001031     C                   endif
104600081125      * visualizzo se 'No addebito'
104700081125     c                   eval      *in17 = *off
104800081125     c                   if        §orfadd = 'S'
104900081125     c                   eval      *in17 = *on
105000081125     c                   endif
105100040908
105200040908      * Se ci sono carico anche i dati della distinta
105300040908if  1c                   If        OrfNdc > *Zeros
105400040908     c                   Eval      *In10 = *On
105500040908     c                   Eval      v3ndc = OrfNdc
105600040908      * giro la data della distinta
105700040908     c                   Clear                   wlbdat
105800040908     c                   Z-add     OrfDdc        G02inv
105900040908     c                   Movel     '3'           G02err
106000040908     c                   Call      'XSRDA8'
106100040908     c                   Parm                    wlbdat
106200040908     c                   Z-add     G02dat        v3ddc
106300040915
106400040908      * cerco il codice dell'autotrasportatore
106500040908     c                   Z-add     OrfNdc        knfv
106600040908     c                   Z-add     OrfFgs        kfgs
106700120612     c     kfidst        Chain     fidst01l
106800060522      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
106900120612     c                   if        not %found(fidst01l)
107000060522     c                   clear                   fnlv55ds
107100060522     c                   eval      d55tpt = '6'
107200060522     c                   move      orffgs        d55lin
107300060522     c                   eval      d55drf = ormddc
107400060522     c                   call      'FNLV55R'
107500060522     c                   parm                    fnlv55ds
107600060522     c                   if        d55err = *blanks and d55tfa <> d55lin
107700060522     c                   move      d55tfa        kfgs
107800120612     c     kfidst        chain     fidst01l
107900060522     c                   endif
108000060522     c                   endif
108100120612     c                   If        %Found(fidst01l)
108200040908     c                   Eval      v3cpdr = DstPdr
108300040908     c                   EndIf
108400040908      * decodifico l'autotrasportatore
108500040908     c                   Clear                   v3dpdr
108600040908     c                   Movel     v3cpdr        kpdpdr
108700040908     c     kfapd         Chain     Fiapd01L
108800040908     c                   If        %Found(Fiapd01l)
108900040908     c                   Eval      v3dpdr = ApdRsc
109000040908     c                   EndIf
109100041013   x1c                   Else
109200041013     c                   Eval      *In10 = *Off
109300041013e   1c                   EndIf
109400001031
109500001031     C                   add       1             nrrf
109600001031     C                   write     fior07sf
109700001031
109800001031     C                   endsr
109900001030      **********************************************************************
110000131120      * F15 - Variazioni
110100001030      **********************************************************************
110200001102     C     Sr_Variazioni BEGSR
110300001030
110400001031     C                   z-add     1             recsf
110500001030      * Pulisco subfile
110600001030     C                   z-add     *zeros        nrr               4 0
110700001030     C                   eval      *in21 = *off
110800001030     C                   write     fior07c
110900001030     C                   eval      *in20 = *on
111000001030     C                   eval      *in21 = *on
111100001030
111200001030      * Carico il subfile in base alle scelte fatte
111300001030     C     kfnor         setll     fnorv01l
111400001030     C                   do        *hival
111500001030     C     kfnor         reade     fnorv01l
111600001030     C                   if        %eof
111700001030     C                   leave
111800001030     C                   endif
111900001030
112000001031     C                   z-add     v1cpoe        v2cpoe
112100001031     C                   z-add     v1nsr         v2nsr
112200001031     C                   z-add     v1nor         v2nor
112300001031     C                   z-add     v1nrv         v2nrv
112400001030
112500001102     C                   exsr      Sr_CarVariaz
112600001030     C                   enddo
112700001030      * Se non ho caricato emetto una videata di segnalazione.
112800001031     C                   if        nrr = *zeros
112900001031     C                   movel     msg(1)        v1cmsg
113000001031     C                   seton                                        28
113100001102     C                   goto      endvariaz
113200001031     C                   endif
113300001030
113400001030      * Se ho caricato emetto il subfile
113500001030     C                   if        nrr > *zeros
113600001031     C                   do        *hival
113700001030     C                   write     fior072
113800001030     C                   exfmt     fior07c
113900001030      * F12=Ritorno
114000001102     C   kl              goto      endvariaz
114100001030
114200001031      * Legge i record scelti
114300001030     C                   do        *hival
114400001030     C                   readc     fior07s
114500001030     C                   if        %eof
114600001030     C                   leave
114700001030     C                   endif
114800001030     C                   if        v2scel = '5'
114900001030     C                   z-add     nrr           recsf
115000001102     C                   exsr      Sr_VisVariaz
115100001030     C                   endif
115200001031     C                   clear                   v2scel
115300001030     C                   update    fior07s
115400001030     C                   enddo
115500001030
115600001030     C                   enddo
115700001031     C                   endif
115800001030
115900001102     C     endvariaz     endsr
116000001030      **********************************************************************
116100001102      * CARICO SUBFILE VARIAZIONI
116200001030      **********************************************************************
116300001102     C     Sr_CarVariaz  begsr
116400001030
116500001030     C                   clear                   v2scel
116600001030     C                   eval      v2cpor = ORVpor
116700001030
116800001030      * Inversione data modifica
116900001103     C                   z-add     ORVdtv        v2dtvd
117000001030     C                   z-add     ORVdtv        G02inv
117100001030     C                   movel     '3'           G02err
117200001030     C                   call      'XSRDA8'
117300001030     C                   parm                    wlbdat
117400001030     C                   movel     G02dat        v2dtv
117500001030
117600001030     C                   movel     ORVorv        v2orv
117700001030     C                   eval      v2utv  = ORVutv
117800001031     C                   eval      v2fao  = ORVfao
117900001031      * Decodifica
118000001031     C                   clear                   TIBS02DS
118100001031     C                   movel     'C'           T02mod
118200001031     C                   movel     knsif         t02sif
118300001031     C                   movel     'FAR'         t02cod
118400001031     C                   movel(p)  v2fao         T02ke1
118500001031     C                   call      'TIBS02R'
118600001031     C                   parm                    KPJBA
118700001031     C                   parm                    TIBS02DS
118800001031     C                   movel     t02uni        v2dfao
118900001030
119000001030     C                   add       1             nrr
119100001030     C                   write     fior07s
119200001030
119300001030     C                   endsr
119400001030      **********************************************************************
119500001102      * VISUALIZZO DATI DEL FILE VARIAZIONI
119600001030      **********************************************************************
119700001102     C     Sr_VisVariaz  begsr
119800001031
119900001031     C                   seton                                        01
120000001030
120100001031     C                   eval      v1mod1 = 'Var.il '
120200001031     C                             + v2dtvg
120300001031     C                             +'/'
120400001031     C                             + v2dtvm
120500001031     C                             + '/'
120600001031     C                             + v2dtva
120700001031     C                             + ' alle '
120800001031     C                             + v2orvo
120900001031     C                             + ':'
121000001031     C                             + v2orvm
121100001031     C                             + ':'
121200001031     C                             + v2orvs
121300001031     C                             + ' da '
121400001031     C                             + v2utv
121500001031
121600001103     C                   eval      kdtv = v2dtvd
121700001103     C                   eval      korv = v2orv
121800001103     C     kforp         chain     fnorv01l
121900001103     C                   if        %found(fnorv01l)
122000001103
122100001102     C                   exsr      Sr_CarVidVar
122200001031     C                   exsr      Sr_Decodifica
122300120612     c                   ExSr      Sr_Tastifun
122400001103     C                   endif
122500001030
122600001102     C                   do        *hival
122700001031     C                   exfmt     fior071
122800001102
122900001102      * F12=Ritorno
123000001102     C   kl              leave
123100131029      /free
123200131029       //?F5=Altri dati
123300131029       IF  *inke;
123400131029         exsr F05D01;
123500131029         iter;
123600131029       ENDIF;
123700131029      /end-free
123800001102
123900001102     C                   enddo
124000001031
124100001030     C                   endsr
124200001031      **********************************************************************
124300001102      * CARICA DATI NELLA VIDEATA DAL FILE VARIAZIONI
124400001031      **********************************************************************
124500001102     C     Sr_CarVidVar  begsr
124600001031
124700001129     C                   movel     ORMtco        v1ctco
124800001031     C                   movel     ORVtor        v1ctor
124900001031      * Inversione data emissione ORM
125000001031     C                   clear                   wlbdat
125100001031     C                   z-add     ORMdao        G02inv
125200001031     C                   movel     '3'           G02err
125300001031     C                   call      'XSRDA8'
125400001031     C                   parm                    wlbdat
125500001031     C                   z-add     G02dat        v1dao
125600010109     C                   movel     ORMoao        v1oao
125700001031      * Dati Ordinante
125800001031     C                   z-add     ORVcor        v1ccor
125900001031     C                   movel     ORVrso        v1rso
126000001031     C                   movel     ORVino        v1ino
126100001031     C                   movel     ORVcao        v1cao
126200001031     C                   movel     ORVloo        v1loo
126300001031     C                   movel     ORVpro        v1pro
126400001031     C                   movel     ORVnao        v1nao
126500001031      * Dati Ritiro a
126600001031     C                   z-add     ORVcra        v1ccra
126700001031     C                   movel     ORVrsr        v1rsr
126800001031     C                   movel     ORVinr        v1inr
126900001031     C                   movel     ORVcar        v1car
127000001031     C                   movel     ORVlor        v1lor
127100001031     C                   movel     ORVprr        v1prr
127200001031     C                   movel     ORVnar        v1nar
127300001031     C                   movel     ORVrer        v1rer
127400001031     C                   movel     ORVter        v1ter
127500001031      * Dati Consegna a
127600001031     C                   z-add     ORVcrc        v1ccrc
127700001031     C                   movel     ORVrsc        v1rsc
127800001031     C                   movel     ORVinc        v1inc
127900001031     C                   movel     ORVloc        v1loc
128000001031     C                   movel     ORVcac        v1cac
128100001031     C                   movel     ORVprc        v1prc
128200001031     C                   movel     ORVnac        v1nac
128300001031
128400010315     C                   if        ORVffd = 'S'
128500010315     C                   eval      v1ffd = 'SI'
128600010315     C                   else
128700010315     C                   eval      v1ffd = 'NO'
128800010315     C                   endif
128900010315
129000001031     C                   movel     ORVnam        v1nam
129100001031      * Inversione data ritiro
129200001031     C                   z-add     ORVdar        G02inv
129300001031     C                   movel     '3'           G02err
129400001031     C                   call      'XSRDA8'
129500001031     C                   parm                    wlbdat
129600001031     C                   z-add     G02dat        v1dar
129700001031     C                   movel     ORVorr        v1orr
129800001031
129900001031     C                   z-add     ORVncl        v1ncl
130000001031     C                   z-add     ORVpkg        v1pkg
130100001031     C                   z-add     ORVvlm        v1vlm
130200001031     C                   z-add     ORVbnc        v1bnc
130300001031     C                   z-add     ORVblc        v1blc
130400001031     C                   z-add     ORVatt        v1att
130500001031     C                   z-add     ORVmtc        v1mtc
130600001031
130700010315     C                   if        ORVpag = 'M'
130800010315     C                   eval      v1pag = 'Mitt.'
130900010315     C                   endif
131000010315     C                   if        ORVpag = 'D'
131100010315     C                   eval      v1pag = 'Dest.'
131200010315     C                   endif
131300010315     C                   if        ORVpag = 'O'
131400010315     C                   eval      v1pag = 'Ord. '
131500010315     C                   endif
131600010315
131700001031     C                   z-add     ORVksc        v1ksc
131800001031     C                   movel     ORVctr        v1ctr
131900010315
132000010315     C                   if        ORVddt = 'S'
132100010315     C                   eval      v1ddt = 'SI'
132200010315     C                   endif
132300010315     C                   if        ORVddt = 'N'
132400010315     C                   eval      v1ddt = 'NO'
132500010315     C                   endif
132600010315
132700001031     C                   movel     ORVpor        v1cpor
132800001031     C                   movel     ORVpoc        v1cpoc
132900001031     C                   movel     ORVno1        v1not1
133000001031     C                   movel     ORVno2        v1not2
133100001031     C                   z-add     ORVfao        v1fao
133200001031     C                   z-add     ORMpos        v1cpos
133300001031     C                   z-add     ORMors        v1ors
133400001031     C                   movel     ORVrfa        v1rfa
133500001221     C                   move      ORVsto        v1sto
133600001031     C                   z-add     ORVndc        v1ndc
133700001031      * Inversione data distinta
133800001031     C                   clear                   wlbdat
133900001031     C                   z-add     ORVddc        G02inv
134000001031     C                   movel     '3'           G02err
134100001031     C                   call      'XSRDA8'
134200001031     C                   parm                    wlbdat
134300001031     C                   z-add     G02dat        v1ddc
134400010226
134500010226      * Cerca codice autotrasportatore
134600071129     c                   clear                   v1cpdc
134700001031     C                   z-add     ORVndc        knfv
134800001031     C                   z-add     ORVpor        kfgs
134900120612     C     kfidst        chain     fidst01l
135000120612     C                   if        %found(fidst01l) and DSTatb = *blanks
135100001031     C                   eval      v1cpdc = DSTpdr
135200001031     C                   endif
135300010119
135400010315     C                   if        ORVspi = 'S'
135500010315     C                   eval      v1spi = 'SI'
135600010315     C                   else
135700010315     C                   eval      v1spi = 'NO'
135800010315     C                   endif
135900030627
136000030627     C                   eval      dorm01 = ORvflo
136100030627
136200030627      * ORM commissionato
136300030627     c                   If        §orCom = 'S'
136400030627     c                   Eval      V1dCom = 'COMMISSIONATO'
136500030627     c                   Else
136600030627     c                   Clear                   V1dCom
136700030627     c                   EndIf
136800131211
136900131211     c                   eval      *in46 = *off
137000131029
137100131029      /free
137200131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
137300131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
137400131211           *in46 = *on;
137500131211         ENDIF;
137600131029       //?Recupero dati da estensione FNORVE
137700131029       //?Rcd 'O ' --> Orari apertura
137800131029         k_ORVEpoe = ORVpoe;
137900131029         k_ORVEnsr = ORVnsr;
138000131029         k_ORVEnor = ORVnor;
138100131029         k_ORVEnrv = ORVnrv;
138200131029         k_ORVEdtv = ORVdtv;
138300131029         k_ORVEorv = ORVorv;
138400131029         k_ORVEtrc = 'O ';
138500131029         clear dOREorari;
138600131029       //?Aggancio il rcd
138700131029         chain(n)  %kds(k07fnorve) FNORVE1L;
138800131029         IF  %found(FNORVE1L);
138900131029           dOREorari = ORVEdati;
139000131029         ENDIF;
139100131029         v1OraAMda = §OREoramda;
139200131029         v1OraAMa  = §OREorama;
139300131029         v1OraPMda = §OREorapda;
139400131029         v1OraPMa  = §OREorapa;
139500160513
139600131029      /end-free
139700001031
139800001031     C                   endsr
139900001025      **********************************************************************
140000001025      * F18 - Note
140100001025      **********************************************************************
140200001025     C     Sr_Note       BEGSR
140300001025
140400001025     C                   clear                   fior06DS
140500001025     C                   z-add     v1dao         i06dta
140600001026     C                   z-add     v1cpoe        i06poe
140700001026     C                   z-add     v1nor         i06nor
140800001026     C                   z-add     v1nsr         i06nsr
140900001026     C                   z-add     v1nrv         i06nrv
141000001026     C                   movel     v1fao         i06far
141100001026     C                   eval      i06dai = §doggi
141200001026     C                   TIME                    W0140
141300001026     C                   movel     w0140         I06ori
141400001026     C                   movel     'I'           i06flm
141500061017     c   ki              movel     'V'           i06flm
141600001026
141700001025     C                   CALL      'FIOR06R'
141800001025     C                   PARM                    kpjba
141900001027     C                   parm                    fior06ds
142000001025
142100001025     C                   endsr
142200001031      **********************************************************************
142300001031      * F19 - Proposte
142400001031      **********************************************************************
142500001031     C     Sr_Proposte   BEGSR
142600001031
142700001031     C                   z-add     1             recsfp
142800001031      * Pulisco subfile
142900001031     C                   z-add     *zeros        nrrp              4 0
143000001031     C                   eval      *in21 = *off
143100001031     C                   write     fior07cp
143200001031     C                   eval      *in20 = *on
143300001031     C                   eval      *in21 = *on
143400001031
143500001031      * Carico il subfile in base alle scelte fatte
143600001031     C     kfnor         setll     fnorp01l
143700001031     C                   do        *hival
143800001031     C     kfnor         reade     fnorp01l
143900001031     C                   if        %eof
144000001031     C                   leave
144100001031     C                   endif
144200001031
144300001031     C                   z-add     v1cpoe        v4cpoe
144400001031     C                   z-add     v1nsr         v4nsr
144500001031     C                   z-add     v1nor         v4nor
144600001031     C                   z-add     v1nrv         v4nrv
144700001031
144800001031     C                   exsr      Sr_CarProposte
144900001031     C                   enddo
145000001031      * Se non ho caricato emetto una videata di segnalazione.
145100001106     C                   if        nrrp = *zeros and psce <> 'P'
145200001103     C                   movel     msg(3)        v1cmsg
145300001031     C                   seton                                        28
145400001031     C                   goto      endproposte
145500001031     C                   endif
145600001103
145700001103     C                   if        nrrp = *zeros and psce = 'P'
145800001103     C                   do        *hival
145900001103     C                   exfmt     fior075
146000001103      * F12=Ritorno
146100001103     C   kl              leave
146200001103     C                   enddo
146300001103     C                   endif
146400001031
146500001031      * Se ho caricato emetto il subfile
146600001031     C                   if        nrrp > *zeros
146700001031     C                   do        *hival
146800001031     C                   write     fior074
146900001031     C                   exfmt     fior07cp
147000001031      * F12=Ritorno
147100001031     C   kl              goto      endproposte
147200001031
147300001031      * Legge i record scelti
147400001031     C                   do        *hival
147500001031     C                   readc     fior07sp
147600001031     C                   if        %eof
147700001031     C                   leave
147800001031     C                   endif
147900001031     C                   if        v4scel = '5'
148000001031     C                   z-add     nrrp          recsfp
148100001031     C                   exsr      Sr_VisProposte
148200001031     C                   endif
148300001031     C                   clear                   v4scel
148400001031     C                   update    fior07sp
148500001031     C                   enddo
148600001031
148700001031     C                   enddo
148800001031     C                   endif
148900001031
149000001031     C     endproposte   endsr
149100001031      **********************************************************************
149200001031      * CARICO SUBFILE PROPOSTE
149300001031      **********************************************************************
149400001031     C     Sr_CarPropostebegsr
149500001031
149600001031     C                   clear                   v4scel
149700001031     C                   eval      v4cpor = ORPpoe
149800001031
149900001031      * Inversione data modifica
150000001103     C                   z-add     ORPdtv        v4dtvd
150100001031     C                   z-add     ORPdtv        G02inv
150200001031     C                   movel     '3'           G02err
150300001031     C                   call      'XSRDA8'
150400001031     C                   parm                    wlbdat
150500001031     C                   movel     G02dat        v4dtv
150600001031
150700001031     C                   movel     ORPorv        v4orv
150800001031     C                   eval      v4utv  = ORPutv
150900001031     C                   eval      v4fao  = ORPfao
151000001031      * Decodifica
151100001031     C                   clear                   TIBS02DS
151200001031     C                   movel     'C'           T02mod
151300001031     C                   movel     knsif         t02sif
151400001031     C                   movel     'FAR'         t02cod
151500001031     C                   movel(p)  v4fao         T02ke1
151600001031     C                   call      'TIBS02R'
151700001031     C                   parm                    KPJBA
151800001031     C                   parm                    TIBS02DS
151900001031     C                   movel     t02uni        v4dfao
152000001031      * Analisi Proposta
152100001031     C                   if        ORPfev <> *blanks
152200001031     C                   seton                                        03
152300001031     C                   if        ORPfev = 'E'
152400001031     C                   eval      v4sta = 'EFFETTUATA'
152500001107     C                   eval      v4sta1= 'EFF'
152600001031     C                   endif
152700001031     C                   if        ORPfev = 'R'
152800001031     C                   eval      v4sta = 'RIFIUTATA'
152900001107     C                   eval      v4sta1= 'RIF'
153000001031     C                   endif
153100001031      * Inversione data analisi proposta
153200001031     C                   z-add     ORPdte        G02inv
153300001031     C                   movel     '3'           G02err
153400001031     C                   call      'XSRDA8'
153500001031     C                   parm                    wlbdat
153600001031     C                   movel     G02dat        v4dte
153700001031     C                   movel     ORPore        v4ore
153800001031     C                   eval      v4ute  = ORPute
153900001107     C                   else
154000001107     C                   setoff                                       03
154100001107     C                   clear                   v4sta
154200001107     C                   clear                   v4sta1
154300001107     C                   clear                   v4dte
154400001107     C                   clear                   v4ore
154500001107     C                   clear                   v4ute
154600001031     C                   endif
154700001031
154800001031     C                   add       1             nrrp
154900001031     C                   write     fior07sp
155000001031
155100001031     C                   endsr
155200001031      **********************************************************************
155300001031      * VISUALIZZO DATI DEL FILE PROPOSTE
155400001031      **********************************************************************
155500001031     C     Sr_VisPropostebegsr
155600001031
155700140303     c                   eval      *in15 = *on
155800001031
155900001031     C                   eval      v1mod1 = 'Prop. del '
156000001031     C                             + v4dtvg
156100001031     C                             +'/'
156200001031     C                             + v4dtvm
156300001031     C                             + '/'
156400001031     C                             + v4dtva
156500001031     C                             + ' alle '
156600001031     C                             + v4orvo
156700001031     C                             + ':'
156800001031     C                             + v4orvm
156900001031     C                             + ':'
157000001031     C                             + v4orvs
157100001031     C                             + ' da '
157200001031     C                             + v4utv
157300001031
157400001103     C                   eval      kdtv = v4dtvd
157500001103     C                   eval      korv = v4orv
157600001103     C     kforp         chain     fnorp01l
157700001103     C                   if        %found(fnorp01l)
157800001103
157900001031     C                   exsr      Sr_CarVidPro
158000001031     C                   exsr      Sr_Decodifica
158100120612     c                   ExSr      Sr_Tastifun
158200001103     C                   endif
158300001031
158400001102     C                   do        *hival
158500001031     C                   exfmt     fior071
158600001102
158700001102      * F12=Ritorno
158800001102     C   kl              leave
158900131029      /free
159000131029       //?F5=Altri dati
159100131029       IF  *inke;
159200131029         exsr F05D01;
159300131029         iter;
159400131029       ENDIF;
159500140303
159600140303       //?F17=Note AUT
159700140303       IF  *inkr;
159800140303         exsr F17D01;
159900140303         iter;
160000140303       ENDIF;
160100131029      /end-free
160200001102
160300001102     C                   enddo
160400001031
160500001031     C                   endsr
160600001031      **********************************************************************
160700001031      * CARICA DATI NELLA VIDEATA DAL FILE PROPOSTE
160800001031      **********************************************************************
160900001031     C     Sr_CarVidPro  begsr
161000001031
161100001129     C                   movel     ORMtco        v1ctco
161200001031     C                   movel     ORPtor        v1ctor
161300001031      * Inversione data emissione ORM
161400001031     C                   clear                   wlbdat
161500001031     C                   z-add     ORMdao        G02inv
161600001031     C                   movel     '3'           G02err
161700001031     C                   call      'XSRDA8'
161800001031     C                   parm                    wlbdat
161900001031     C                   z-add     G02dat        v1dao
162000010109     C                   movel     ORMoao        v1oao
162100001031      * Dati Ordinante
162200001031     C                   z-add     ORPcor        v1ccor
162300001031     C                   movel     ORPrso        v1rso
162400001031     C                   movel     ORPino        v1ino
162500001031     C                   movel     ORPcao        v1cao
162600001031     C                   movel     ORPloo        v1loo
162700001031     C                   movel     ORPpro        v1pro
162800001031     C                   movel     ORPnao        v1nao
162900001031      * Dati Ritiro a
163000001031     C                   z-add     ORPcra        v1ccra
163100001031     C                   movel     ORPrsr        v1rsr
163200001031     C                   movel     ORPinr        v1inr
163300001031     C                   movel     ORPcar        v1car
163400001031     C                   movel     ORPlor        v1lor
163500001031     C                   movel     ORPprr        v1prr
163600001031     C                   movel     ORPnar        v1nar
163700001031     C                   movel     ORPrer        v1rer
163800001031     C                   movel     ORPter        v1ter
163900001031      * Dati Consegna a
164000001031     C                   z-add     ORPcrc        v1ccrc
164100001031     C                   movel     ORPrsc        v1rsc
164200001031     C                   movel     ORPinc        v1inc
164300001031     C                   movel     ORPloc        v1loc
164400001031     C                   movel     ORPcac        v1cac
164500001031     C                   movel     ORPprc        v1prc
164600001031     C                   movel     ORPnac        v1nac
164700001031
164800010315     C                   if        ORPffd = 'S'
164900010315     C                   eval      v1ffd = 'SI'
165000010315     C                   else
165100010315     C                   eval      v1ffd = 'NO'
165200010315     C                   endif
165300010315
165400001031     C                   movel     ORPnam        v1nam
165500001031      * Inversione data ritiro
165600001031     C                   z-add     ORPdar        G02inv
165700001031     C                   movel     '3'           G02err
165800001031     C                   call      'XSRDA8'
165900001031     C                   parm                    wlbdat
166000001031     C                   z-add     G02dat        v1dar
166100001031     C                   movel     ORPorr        v1orr
166200001031
166300001031     C                   z-add     ORPncl        v1ncl
166400001031     C                   z-add     ORPpkg        v1pkg
166500001031     C                   z-add     ORPvlm        v1vlm
166600001031     C                   z-add     ORPbnc        v1bnc
166700001031     C                   z-add     ORPblc        v1blc
166800001031     C                   z-add     ORPatt        v1att
166900001031     C                   z-add     ORPmtc        v1mtc
167000010315
167100010315     C                   if        ORPpag = 'M'
167200010315     C                   eval      v1pag = 'Mitt.'
167300010315     C                   endif
167400010315     C                   if        ORPpag = 'D'
167500010315     C                   eval      v1pag = 'Dest.'
167600010315     C                   endif
167700010315     C                   if        ORPpag = 'O'
167800010315     C                   eval      v1pag = 'Ord. '
167900010315     C                   endif
168000001031
168100001031     C                   z-add     ORPksc        v1ksc
168200001031     C                   movel     ORPctr        v1ctr
168300010315
168400010315     C                   if        ORPddt = 'S'
168500010315     C                   eval      v1ddt = 'SI'
168600010315     C                   endif
168700010315     C                   if        ORPddt = 'N'
168800010315     C                   eval      v1ddt = 'NO'
168900010315     C                   endif
169000010315
169100001031     C                   movel     ORPpor        v1cpor
169200001031     C                   movel     ORPpoc        v1cpoc
169300001031     C                   movel     ORPno1        v1not1
169400001031     C                   movel     ORPno2        v1not2
169500001031     C                   z-add     ORPfao        v1fao
169600001031     C                   z-add     ORMpos        v1cpos
169700001031     C                   z-add     ORMors        v1ors
169800001031     C                   movel     ORPrfa        v1rfa
169900001221     C                   move      ORPsto        v1sto
170000001031     C                   z-add     ORPndc        v1ndc
170100001031      * Inversione data distinta
170200001031     C                   clear                   wlbdat
170300001031     C                   z-add     ORPddc        G02inv
170400001031     C                   movel     '3'           G02err
170500001031     C                   call      'XSRDA8'
170600001031     C                   parm                    wlbdat
170700001031     C                   z-add     G02dat        v1ddc
170800010226
170900010226      * Cerca codice autotrasportatore
171000071129     c                   clear                   v1cpdc
171100001031     C                   z-add     ORPndc        knfv
171200001031     C                   z-add     ORPpor        kfgs
171300120612     C     kfidst        chain     fidst01l
171400120612     C                   if        %found(fidst01l) and DSTatb = *blanks
171500001031     C                   eval      v1cpdc = DSTpdr
171600001031     C                   endif
171700010226
171800010315     C                   if        ORPspi = 'S'
171900010315     C                   eval      v1spi = 'SI'
172000010315     C                   else
172100010315     C                   eval      v1spi = 'NO'
172200010315     C                   endif
172300030627
172400030627     C                   eval      dorm01 = ORpflo
172500030627
172600030627      * ORM commissionato
172700030627     c                   If        §orCom = 'S'
172800030627     c                   Eval      V1dCom = 'COMMISSIONATO'
172900030627     c                   Else
173000030627     c                   Clear                   V1dCom
173100030627     c                   EndIf
173200131211
173300131211     c                   eval      *in46 = *off
173400140303     c                   eval      *in48 = *off
173500131029
173600131029      /free
173700131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
173800131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
173900131211           *in46 = *on;
174000131211         ENDIF;
174100131029       //?Recupero dati da estensione FNORPE
174200131029       //?Rcd 'O ' --> Orari apertura
174300131029         k_ORPEpoe = ORPpoe;
174400131029         k_ORPEnsr = ORPnsr;
174500131029         k_ORPEnor = ORPnor;
174600131029         k_ORPEnrv = ORPnrv;
174700131029         k_ORPEdtv = ORPdtv;
174800131029         k_ORPEorv = ORPorv;
174900131029         k_ORPEtrc = 'O ';
175000131029         clear dOREorari;
175100131029       //?Aggancio il rcd
175200131029         chain(n)  %kds(k07fnorpe) FNORPE1L;
175300131029         IF  %found(FNORPE1L);
175400131029           dOREorari = ORPEdati;
175500131029         ENDIF;
175600131029         v1OraAMda = §OREoramda;
175700131029         v1OraAMa  = §OREorama;
175800131029         v1OraPMda = §OREorapda;
175900131029         v1OraPMa  = §OREorapa;
176000140303
176100140303       //?Controllo se ci sono proposte Note AUT
176200140303         chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
176300140303         IF  %found(FNORPT1L);
176400140303           *in48 = *on;
176500140303         ENDIF;
176600131029      /end-free
176700001031
176800001031     C                   endsr
176900010109      **********************************************************************
177000010109      * Carico dati dalla kpjba per programma richiamato da interrogazione
177100010109      * proposte
177200010109      **********************************************************************
177300010109     C     Sr_Cardatipropbegsr
177400010109
177500010109     C                   z-add     ppoe          v4cpoe
177600010109     C                   z-add     pnsr          v4nsr
177700010109     C                   z-add     pnor          v4nor
177800010109     C                   z-add     pnrv          v4nrv
177900010109     C                   z-add     pdtv          v4dtv
178000010109     C                   z-add     porv          v4orv
178100010109      * Inverto la data
178200010109     C                   z-add     v4dtv         G02DAT
178300010109     C                   MOVEL     *BLANK        G02ERR
178400010109     C                   CALL      'XSRDA8'
178500010109     C                   PARM                    WLBDAT
178600010109     C                   Z-ADD     G02INV        v4dtvd
178700010109
178800010109     C                   movel     putv          v4utv
178900010109
179000010109     C                   endsr
179100061017      *--------------------------------------------------------------------*
179200061017      * F9 - Sollecito
179300061017      *--------------------------------------------------------------------*
179400061017     c     sr_sollecito  begsr
179500061017
179600061017      * il sollecito è possibile se l'ORM è automatizzato
179700061017      * p.o. emissione DPD
179800061017     c     v1cpoe        lookup    skpodpd                                30
179900061017     c                   if        *in30 and v1ctco <> 'F'
180000061017     c                   eval      *in28 = *on
180100061017     c                   eval      v1cmsg = msg(05)
180200061017     c                   goto      for01
180300061017     c                   endif
180400061017      * p.o. ritiro DPD
180500061017     c     v1cpor        lookup    skpodpd                                30
180600061017     c                   if        *in30 and v1cfcs <> 'F'
180700061017     c                   eval      *in28 = *on
180800061017     c                   eval      v1cmsg = msg(05)
180900061017     c                   goto      for01
181000061017     c                   endif
181100061017      * se orm in fase 900 o 999 non posso sollecitare
181200061017     c                   if        v1fao > 600
181300061017     c                   eval      *in28 = *on
181400061017     c                   eval      v1cmsg = msg(06)
181500061017     c                   goto      for01
181600061017     c                   endif
181700061018      * il sollecito è possibile se l'ORM è già stato stampato
181800080505     c     v1cpor        lookup    skpodpd                                30
181900080505     c                   if        *in30 and v1cfcs = 'F' and
182000080505     c                             v1dst = *zeros
182100061018     c                   eval      *in28 = *on
182200061018     c                   eval      v1cmsg = msg(04)
182300061018     c                   goto      for01
182400061018     c                   endif
182500061017
182600061017      * p.o. ritiro DPD
182700061017     c     v1cpor        lookup    skpodpd                                30
182800061017      * devo richiamare il pgm di stampa per mandare il sollecito
182900061017      * via fax o mail
183000061017     c                   if        *in30
183100090211      * prima di mandare il fax o la mail verifico se il depot è associato ad un cod.cliente
183200090211     c                   exsr      sr_depot
183300090211      * se non associato errore e non si fa il sollecito
183400090211     c                   if        odepksc = *zeros
183500090211     c                   eval      *in28 = *on
183600090211     c                   eval      v1cmsg = msg(07)
183700090217     c                   eval      %subst(v1cmsg:32:4) = osie3ddep
183800090211     c                   goto      for01
183900090211     c                   endif
184000061017     c                   exsr      sr_stampa
184100061017     c                   endif
184200061017
184300061017      * scrivo in automatico la nota per avvenuto sollecito
184400061017     c                   eval      ornpoe = v1cpoe
184500061017     c                   eval      ornnsr = v1nsr
184600061017     c                   eval      ornnor = v1nor
184700061017     c                   eval      ornnrv = v1nrv
184800061017     c                   eval      ornfar = v1fao
184900061017     c                   clear                   wprg
185000061017      * cerco ultimo progressivo relativo alla stessa fase
185100061017     c     kfnorn        setll     fnorn02l
185200061017     c                   do        *hival
185300061017     c     kfnorn        reade     fnorn02l
185400061017     c                   if        %eof(fnorn02l)
185500061017     c                   leave
185600061017     c                   endif
185700061017     c                   eval      wprg = ornprg
185800061017     c                   enddo
185900061017      * scrivo la nota
186000061017     c                   move      data_oggi     ds_data
186100061017     c                   add       1             wprg
186200061017     c                   clear                   fnorn000
186300061017     c                   eval      ornpoe = v1cpoe
186400061017     c                   eval      ornnsr = v1nsr
186500061017     c                   eval      ornnor = v1nor
186600061017     c                   eval      ornnrv = v1nrv
186700061017     c                   eval      ornfar = v1fao
186800061017     c                   eval      orndai = dateu
186900061017     c                   time                    ornori
187000061017     c                   eval      ornprg = wprg
187100061017     c                   eval      ornpue = knmus
187200150309     c                   eval      ornpon = DUTpou
187300061017     c                   eval      ornsns = 'I'
187400061017     c                   eval      ornnot = '** SOLLECITATO IL ' +
187500061017     c                             %editc(ds_giorno:'1') + '/' +
187600061017     c                             %editc(ds_mese:'1') + '/' +
187700061017     c                             %editc(ds_anno:'Z') + ' **'
187800061017     c                   eval      ornftr = 'S'
187900061017     c                   eval      orndtr = dateu
188000061017     c                   write     fnorn000
188100061017
188200061017      * richiamo il pgm di gestione note
188300061017     c                   exsr      sr_note
188400061017
188500061017     c                   endsr
188600090211
188700090211      *--------------------------------------------------------------------*
188800090211      * Controllo il depot
188900090211      *--------------------------------------------------------------------*
189000090211     c     sr_depot      begsr
189100090211
189200090211     c                   clear                   tisie3ds
189300090211     c                   eval      isie3dri = dateu
189400090211     c                   eval      isie3dsp = ormdar
189500090211     c                   time                    isie3hsp
189600090211     c                   eval      isie3nzd = ormnar
189700090211     c                   eval      isie3cad = ormcar
189800090211     c                   if        ormpkg > §3ilmp
189900090211     c                   eval      isie3srv = 101
190000090211     c                   else
190100090211     c                   eval      isie3srv = 136
190200090211     c                   endif
190300090211     c                   eval      isie3lnp = ormpoe
190400090211     c                   call      'TISIE3R'
190500090211     c                   parm                    tisie3ds
190600090211      * con il depot richiamo pgm per avere il codice cliente
190700090211      * cliente
190800090211     c                   clear                   truldepds
190900090211     c                   eval      idepdpc = osie3ddep
191000090211     c                   eval      idepdrf = ormdar
191100090211     c                   call      'TRULDEPR'
191200090211     c                   parm                    truldepds
191300090211
191400090211     c                   endsr
191500061017
191600061017      *--------------------------------------------------------------------*
191700061017      * Richiama il pgm di stampa per sollecito
191800061017      *--------------------------------------------------------------------*
191900061017     c     sr_stampa     begsr
192000061017
192100061017     c                   clear                   fior16ds
192200061017     c                   clear                   trul90ds
192300061017     c                   movel     'S'           d90rso
192400061017     c                   call      'TRUL90R'
192500061017     c                   parm                    kpjba
192600061017     c                   Parm                    trul90ds
192700061017      * F3=Fine
192800061017     c                   if        d90f3 = '1'
192900061017     c                   goto      endsrstampa
193000061017     c                   endif
193100090202      /free
193200090202         reset ds5P;
193300090202         if  %subst(knmus : 1 : 3) = 'EDP';
193400090202           TBLkey = 'O' + 'EDP';
193500090202         else;
193600090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
193700090202         endif;
193800090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
193900090202         if  %found( TABEL00F );
194000090202           ds5P = TBLuni;
194100090202         endif;
194200090202      /end-free
194300061017     c                   movel     d90mdo        r16mdo
194400061017     c                   movel     d90pro        r16oqo
194500061017     c                   eval      r16tla = *blanks
194600061017      * è sempre sollecito imposto il nuovo tipo
194700061017     c                   eval      r16ris ='X'
194800061017     c                   eval      r16poe = v1cpoe
194900061017     c                   eval      r16nsr = v1nsr
195000061017     c                   eval      r16nor = v1nor
195100061017     c                   eval      r16nrv = v1nrv
195200061017     c                   eval      r16por = v1cpor
195300061017     c                   eval      r16dst = ormdar
195400061017     c                   movel     kpjbu         §kpjbu
195500061017     c                   movel(p)  fior16ds      kpjbu
195600090202     c                   call      §5Ppgm
195700061017     c                   parm                    kpjba
195800080318     c                   movel     kpjbu         fior16ds
195900061017     c                   movel     §kpjbu        kpjbu
196000061017
196100061017     c     endsrstampa   endsr
196200061019
196300061019      *--------------------------------------------------------------------*
196400061023      * F2 - Dati DPD
196500061019      *--------------------------------------------------------------------*
196600061019     c     sr_datidpd    begsr
196700080430
196800090202      * Salvataggio KPJBU
196900090202     c                   eval      §kpjbu = kpjbu
197000090202
197100080430     c                   clear                   fieu55ds
197200080422
197300080430      * se ORM commissionato da DPD passo il n. ORM DPD
197400080430     c     ormpoe        lookup    skpodpd                                30
197500080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
197600080430     c                   eval      i55dep = %subst(ormrfa:4:4)
197700080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
197800081015     c                   eval      i55ie = 'I'
197900080430     c                   eval      kpjbu = fieu55ds
198000080430     c                   call      'FIEU55C'
198100080430     c                   parm                    kpjba
198200080430     c                   endif
198300080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
198400080430     c     ormpor        lookup    skpodpd                                30
198500080430     c                   if        *in30
198600061019     c                   eval      i55poe = v1cpoe
198700061019     c                   eval      i55nrs = v1nsr
198800061019     c                   eval      i55nro = v1nor
198900061019     c                   eval      i55nrv = v1nrv
199000081015     c                   eval      i55ie = 'E'
199100061019     c                   eval      kpjbu = fieu55ds
199200061206     c                   call      'FIEU55C'
199300061019     c                   parm                    kpjba
199400080430     c                   endif
199500090202
199600090202      * Ripristino KPJBU
199700090202     c                   eval      kpjbu = §kpjbu
199800061019
199900061019     c                   endsr
200000090211
200100090211      *--------------------------------------------------------------------*
200200090211      * Window per dati appuntamento ORM
200300090211      *--------------------------------------------------------------------*
200400090211     c     sr_ormapp     begsr
200500090223
200600090223      * pulisco il subfile
200700090223     c                   clear                   nrra              4 0
200800090223     c                   eval      *in21 = *off
200900090223     c                   write     fior07apc
201000090223     c                   eval      *in20 = *on
201100090223     c                   eval      *in21 = *on
201200090223
201300090223      * carico il subfile
201400090216      * metto fissa la fase 100, per ora ho l'appuntamento solo su questa fase
201500090225     c                   eval      *in31 = *off
201600090216     c                   eval      kfar = 100
201700090223     c     kfnora        setll     fnora01l
201800090223     c                   do        *hival
201900090223     c     kfnora        reade     fnora01l
202000090223     c                   if        %eof(fnora01l)
202100090225     c                   eval      *in31 = *on
202200090223     c                   leave
202300090223     c                   endif
202400090211     c                   eval      wappref = oraref
202500090211     c                   eval      wappnota1 = %subst(oranote:1:35)
202600090211     c                   eval      wappnota2 = %subst(oranote:36:35)
202700090223     c                   eval      wappora = oraora
202800090223     c                   eval      wappute = oraute
202900090223     c                   eval      g02inv = oradata
203000090223     c                   eval      g02err = '3'
203100090223     c                   call      'XSRDA8'
203200090223     c                   parm                    wlbdat
203300090223     c                   eval      wappdata = g02dat
203400090223     c                   add       1             nrra
203500090223     c                   write     fior07aps
203600090223     c                   enddo
203700090211
203800090223      * se non ho caricato nessun appuntamento emetto una segnalazione
203900090223     c                   if        nrra = *zeros
204000090223     c                   movel     msg(8)        v1cmsg
204100090223     c                   eval      *in28 = *on
204200090223     c                   leavesr
204300090223     c                   endif
204400090223
204500090223      * se ho caricato emetto il subfile
204600090223     c                   if        nrra > *zeros
204700090225     c                   eval      recsfa = 1
204800090223     c                   do        *hival
204900090223     c                   write     fior07apz
205000090223     c                   exfmt     fior07apc
205100090223      * f12-ritorno
205200090223     c                   if        *inkl
205300090223     c                   leave
205400090223     c                   endif
205500090211     c                   enddo
205600090223     c                   endif
205700090211
205800090211     c                   endsr
205900130913
206000130913      /free
206100131028       //--------------------------------------------------------------------
206200131028       //?F5 - Altri dati.
206300131028       //--------------------------------------------------------------------
206400131028       BEGSR F05D01;
206500131028
206600131028         wEndF05 = *off;
206700131028
206800131028       //?emetto la videata relativa agli altri dati
206900131028         write fior071;
207000131028       //?emetto la window dove richiedo gli altri dati
207100131028         DOW not wEndF05;
207200131028           exfmt fior0712;
207300131028       //?F12-Ritorno
207400131028           IF  *inkl;
207500131028             wEndF05 = *on;
207600131028           ENDIF;
207700131028         ENDDO;
207800131028
207900131028       ENDSR;
208000140110
208100130913       //--------------------------------------------------------------------
208200130913       //?F14 - Interrogazione orari di servizio.
208300130913       //--------------------------------------------------------------------
208400130913       BEGSR F14D01;
208500140206
208600140317       //?Devo trovare la pickup per passarla al trulorsr
208700140206         clear TISI95DS;
208800140206         I95tcn = '3';
208900140206         I95cap = ORMcar;
209000140206         I95nar = ORMnar;
209100140206         I95ind = ORMinr;
209200140206         I95loc = ORMlor;
209300140206         I95prv = ORMprr;
209400140206       //?Peso in base al ntwk fil.ritiro
209500140206         IF  network = 'SDI';
209600140206           I95lkg = 1000;
209700140206         ELSE;
209800140206           clear I95lkg;
209900140206         ENDIF;
210000140206         tisi95r (tisi95ds);
210100140317
210200140317       //?Cerco eventuale ora presunta ritiro
210300140318         clear fior01ds;
210400140317         IF  ORMndc > 0 and ORMfao < 600;
210500140318           IOR01fgs = ORMpor;
210600140318           IOR01ndc = ORMndc;
210700140318           IOR01poe = ORMpoe;
210800140318           IOR01nsr = ORMnsr;
210900140318           IOR01nor = ORMnor;
211000140318           IOR01nrv = ORMnrv;
211100140318           fior01r (fior01ds);
211200140317         ENDIF;
211300130913
211400140317         clear trulorsds;
211500130913         IOREfil = ORMpor;
211600130913         IOREcap = ORMcar;
211700130913         IOREloc = ORMlor;
211800130913         IOREdta = ORMdar;
211900130913         IOREtser = 'R';
212000140206         IOREgf2 = O95gf2;
212100140318         IOREorp = OOR01ora;
212200130913
212300140317         trulorsr (kpjba:trulorsds);
212400130913
212500130913       ENDSR;
212600140110
212700140110       //--------------------------------------------------------------------
212800140110       //?F17 - Note AUT.
212900140110       //--------------------------------------------------------------------
213000140110       BEGSR F17D01;
213100140121
213200140121         *in48 = *off;
213300140110
213400140110         clear fiort1ds;
213500140303         IORT1mod = 'G';
213600140110         IORT1poe = ORMpoe;
213700140110         IORT1nsr = ORMnsr;
213800140110         IORT1nor = ORMnor;
213900140110         IORT1nrv = ORMnrv;
214000140110         IORT1pgm = 'FIOR07R';
214100140303         IF  *in15;
214200140303           IORT1mod = 'I';
214300140303           IORT1dim = V4Dtvd;
214400140303           IORT1oim = V4orv;
214500140303         ENDIF;
214600140114         §kpjbu = kpjbu;
214700140110
214800140320         IF  psce = 'P' and *in15;
214900140320           fiort1c (kpjba:fiort1ds);
215000140320         ELSE;
215100140110         fiort1r (kpjba:fiort1ds);
215200140320         ENDIF;
215300140114
215400140114         kpjbu = §kpjbu;
215500140121
215600140121       //?Controllo se sono state immesse Note AUT
215700140304         IF  not *in15;
215800140304           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
215900140304           IF  %found(FNORT01L);
216000140304             *in48 = *on;
216100140304           ENDIF;
216200140304         ENDIF;
216300140304       //?Controllo se ci sono proposte Note AUT
216400140304         IF  *in15;
216500140304           chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
216600140304           IF  %found(FNORPT1L);
216700140304             *in48 = *on;
216800140304           ENDIF;
216900140304         ENDIF;
217000140110
217100140110       ENDSR;
217200130913
217300130913      /end-free
217400130913
217500090210
217600090210      *------------------------------------------------------------------------*
217700090210      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
217800090210      *------------------------------------------------------------------------*
217900090210     c     sr_f24        begsr
218000090210
218100090210     c                   Select
218200090210
218300090210     c                   When      $loop = 2 and $rig =2  or
218400090210     c                             $loop = 3 and $rig =3
218500090210     c                   Movea     rigatf1       vzfd01
218600090210     c                   Z-add     1             $rig
218700090210
218800090210     c                   If        $loop = 2
218900090210     c                   Eval      vzfd02= cf2412
219000090210     c                   Else
219100090210     c                   Eval      vzfd02= cf2413
219200090210     c                   EndIf
219300090210
219400090210     c                   When      $loop = 2 and $rig =1 or
219500090210     c                             $loop = 3 and $rig =1
219600090210     c                   Movea     rigatf2       vzfd01
219700090210     c                   Z-add     2             $rig
219800090210
219900090210     c                   If        $loop = 2
220000090210     c                   Eval      vzfd02 = cf2422
220100090210     c                   Else
220200090210     c                   Eval      vzfd02 = cf2423
220300090210     c                   EndIf
220400090210
220500090210     c                   When      $loop = 3 and $rig =2
220600090210     c                   Movea     rigatf3       vzfd01
220700090210     c                   Z-add     3             $rig
220800090210     c                   Eval      vzfd02 = cf2433
220900090210     c                   EndSl
221000090210
221100090210     c                   EndSr
221200090210
221300090210      *------------------------------------------------------------------------*
221400090210      *  Imposto descrizione tasti funzione
221500090210      *------------------------------------------------------------------------*
221600090210     c     Sr_Tastifun   BegSr
221700140304
221800140304      * Abilito eventuali tasti funzionali
221900140304     c                   exsr      Abil_Fxx
222000090210
222100090210      * Conta i caratteri riempiti dalle RigaTf1
222200090210     c                   Clear                   $z
222300090210      * Conta i caratteri riempiti dalle RigaTf2
222400090210     c                   Clear                   $k
222500090210      * Conta i caratteri riempiti dalle RigaTf3
222600090210     c                   Clear                   $j
222700090210      * Conta la posizione del campone da cui partire per impostare
222800090210      * la descrizione del tasto funzione
222900090210     c                   Clear                   $y
223000090210
223100090210      * Stringhe che contengono le descrizioni dei tasti funzione
223200090210     c                   Reset                   rigatf1
223300090210     c                   Reset                   rigatf2
223400090210     c                   Reset                   rigatf3
223500090217
223600131028      * F1 - Aggiungi Richiesta Assistenza
223700131028     c                   if        *in08
223800131028     c                   reset                   $tf
223900131028     c                   movea     tf01          $tf
224000131028     c                   exsr      rie_$tf
224100131028     c                   endif
224200090217      * F2 - Dati DPD
224300090217     c                   if        not *in01 and *in14 and
224400140304     c                             not *in12 and not *in15
224500090217     c                   reset                   $tf
224600090217     c                   movea     tf02          $tf
224700090217     c                   exsr      rie_$tf
224800090217     c                   endif
224900090217      * F9 - Sollecito
225000090217     c                   if        not *in01 and *in13
225100140304     c                             and not *in15
225200090217     c                   reset                   $tf
225300090217     c                   movea     tf09          $tf
225400090217     c                   exsr      rie_$tf
225500090217     c                   endif
225600130913      * F14 - Orari servizio
225700131108     c                   if        not *in01
225800131125     c                             and *in44
225900140304     c                             and not *in15
226000130913     c                   reset                   $tf
226100130913     c                   movea     tf14          $tf
226200130913     c                   exsr      rie_$tf
226300130913     c                   endif
226400131120      * F15 - Variazioni
226500131120     c                   if        not *in01 and *in40
226600140304     c                             and not *in15
226700131120     c                   reset                   $tf
226800131120     c                   movea     tf15          $tf
226900131120     c                   exsr      rie_$tf
227000131120     c                   endif
227100131028      * F16 - Gestione Richieste assistenza
227200131108     c                   if        not *in01 and *in09
227300140304     c                             and not *in15
227400131028     c                   reset                   $tf
227500131028     c                   movea     tf16          $tf
227600131028     c                   exsr      rie_$tf
227700131028     c                   endif
227800090217      * F19 - Proposte Variazioni
227900090217     c                   if        not *in01 and *in41
228000140304     c                             and not *in15
228100090217     c                   reset                   $tf
228200090217     c                   movea     tf19          $tf
228300090217     c                   exsr      rie_$tf
228400090217     c                   endif
228500090217      * F22 - Info Appuntamento
228600090217     c                   if        not *in01 and *in43
228700140304     c                             and not *in15
228800090217     c                   reset                   $tf
228900090217     c                   movea     tf22          $tf
229000090217     c                   exsr      rie_$tf
229100090217     c                   endif
229200090211      * F20 - Fasi
229300090211     c                   if        not *in01
229400140304     c                             and not *in15
229500090210     c                   reset                   $tf
229600090211     c                   movea     tf20          $tf
229700090210     c                   exsr      rie_$tf
229800090210     c                   endif
229900090210
230000090210      * Pulisco la stringa
230100090210     c                   Eval      $h = 1
230200090210     c                   For       $h by 1 to 62
230300090210     c                   If        rigatf1($h) = '#'
230400090210     c                   Clear                   rigatf1($h)
230500090210     c                   EndIf
230600090210     c                   EndFor
230700090210     c                   Eval      $h = 1
230800090210     c                   For       $h by 1 to 62
230900090210     c                   If        rigatf2($h) = '#'
231000090210     c                   Clear                   rigatf2($h)
231100090210     c                   EndIf
231200090210     c                   EndFor
231300090210     c                   Eval      $h = 1
231400090210     c                   For       $h by 1 to 62
231500090210     c                   If        rigatf3($h) = '#'
231600090210     c                   Clear                   rigatf3($h)
231700090210     c                   EndIf
231800090210     c                   EndFor
231900090210
232000090210      * Imposto la descrizione del tasto funzione F24 e segnalo
232100090210      * quante righe ho riempito e quale devo emettere
232200090210     c                   Select
232300090210
232400090210     c                   When      $k <> *Zeros and $y <> *Zeros
232500090210     c                   Eval      $loop = 3
232600090210     c                   Eval      vzfd02 = cf2413
232700090210
232800090210     c                   When      $k <> *Zeros and $y = *Zeros
232900090210     c                   Eval      $loop = 2
233000090210     c                   Eval      vzfd02 = cf2412
233100090210
233200090210     c                   Other
233300090210
233400090210     c                   Eval      $loop = 1
233500090210     c                   Clear                   vzfd02
233600090210     c                   EndSl
233700090210
233800090210      * Prima riga di tasti funzione
233900090210     c                   Movea     rigatf1       vzfd01
234000090210     c                   Eval      $rig = 1
234100090210
234200090210     c                   EndSr
234300090210
234400090210      *------------------------------------------------------------------------*
234500090210      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
234600090210      *------------------------------------------------------------------------*
234700090210     c     Rie_$tf       BegSr
234800090210      *
234900090210     c                   Eval      $x = 1
235000090210     c     '#'           Lookup    $tf($x)                                30
235100090210     c                   Add       $x            $z
235200090210     c                   If        $Z <= 63
235300090210     c                   Eval      $j = $Z - $x + 1
235400090210     c                   Movea     $tf           rigatf1($j)
235500090210     c                   Else
235600090210     c                   Add       $x            $k
235700090210     c                   If        $k <= 63
235800090210     c                   Eval      $j = $K - $x + 1
235900090210     c                   Movea     $tf           rigatf2($j)
236000090210     c                   Else
236100090210     c                   Add       $x            $y
236200090210     c                   If        $y <= 63
236300090210     c                   Eval      $j = $y - $x + 1
236400090210     c                   Movea     $tf           rigatf3($j)
236500090210     c                   EndIf
236600090210     c                   EndIf
236700090210     c                   EndIf
236800090210
236900090210     c                   EndSr
237000061017
237100001010      *****************************************************************
237200001010      * ROUTINE INIZIALE
237300001010      *****************************************************************
237400001010     C     *INZSR        BEGSR
237500001009
237600001010     C     *ENTRY        PLIST
237700001010     C                   PARM                    KPJBA
237800040220     c                   parm                    FIDNA1ds
237900001011
238000001011     C                   movel     TIT_A         VTCtit
238100001009
238200001026     C                   movel(p)  kpjbu         parm01
238300001016
238400090202     C                   Z-ADD     1             CODUT             1 0
238500090202     c     *dtaara       define    §azute        AZUTEds
238600090202     c     *dtaara       define    §datiute      dDATIUTE
238700090202     c                   in(E)     *dtaara
238800090202     c                   if        %ERROR or RSUT = *blanks
238900090202     c                   clear                   Tibs34Ds
239000090202     c                   call      'TIBS34R'
239100090202     c                   parm                    Tibs34Ds
239200090202     c                   in        *dtaara
239300090202     c                   endif
239400151001
239500151001      * abilitazione utente
239600151001     c                   clear                   wabi
239700151001     c                   eval      dUTE01 = UTEfaf
239800151001     c                   SELECT
239900151001      * carica abilitazioni del profilo
240000151001     c                   WHEN      §UTEorm <> *blanks
240100151001     c                   eval      wabi = §UTEorm
240200151001     c                   WHEN      UTEaut <> *blanks
240300151001     c                   eval      wabi = UTEaut
240400151001      * se non c'è l'abilitazione
240500151001      * --> se 1° livello, abilitazioni al terminal
240600151001      *     se 2° livello, abilitazioni al punto operativo
240700151001      *     se sede no abilitazioni
240800151001     c                   OTHER
240900151001     c                   IF        DUTlpo = '1'
241000151001     c                   eval      wabi   = 'TP'
241100151001     c                   ENDIF
241200151001     c                   IF        DUTlpo = '2'
241300151001     c                   eval      wabi   = 'PO'
241400151001     c                   ENDIF
241500151001     c                   ENDSL
241600151001
241700151001      * Reperimento delle filiali gestibili dall'utente
241800151001     c                   clear                   TRUL31DS
241900151001     c                   eval      I31abi = wabi
242000151001     c                   eval      I31cdi = DUTdis
242100151001     c                   eval      I31car = DUTare
242200151001     c                   eval      I31cpo = DUTpou
242300151001     c                   call      'TRUL31R'
242400151001     c                   parm                    KPJBA
242500151001     c                   parm                    TRUL31DS
242600151001     c                   IF        O31pog > *zeros
242700151001     c                   movea     O31pog        SKpog
242800151001     c                   ENDIF
242900041001
243000050112      * Se ambiente prova FILTRAPRD
243100050112     c                   If        %subst(knsif:7:1) = 'P'
243200050112     c                   Eval      wlib = 'FILTRAPRD '
243300050112     c                   Else
243400050112      * Se ambiente buono FILTRA201
243500050112     c                   Eval      wlib = 'FILTRA201 '
243600041001     c                   EndIf
243700010226
243800150309      * Da sede
243900150309      * apro il file delle distinte dell'ambiente di filiale
244000010226
244100010226     C                   if        simfel = 0
244200010226     C                   eval      *in12 = *on
244300041001     c                   Clear                   comman
244400041001     c                   Movel(p)  cmdt(1)       comman
244500041001     c                   Eval      %Subst(comman:30:10) = wlib
244600041001     c                   Eval      comman =
244700120612     c                             %trim(comman) + '/FIDST01L)'
244800041001     c                   Call      'QCMDEXC'                            99
244900041001     c                   Parm                    comman
245000041001     c                   Parm                    lenght
245100120612     c  n99              Open      fidst01l
245200041001
245300010226     C                   else
245400010226     C                   eval      *in12 = *off
245500120612     C                   open      fidst01l
245600010226     C                   endif
245700061017
245800061206      * carico sk dei p.o. DPD
245900061017     c                   clear                   xx
246000061017     c     *loval        setll     azorg01l
246100061017     c                   do        *hival
246200061017     c                   read      azorg01l
246300061017     c                   if        %eof(azorg01l)
246400061017     c                   leave
246500061017     c                   endif
246600061017     c                   if        orgfva <> *blanks  or
246700061017     c                             (orgfag <> 'F' and orgfag <> 'A')
246800061017     c                   iter
246900061017     c                   endif
247000061017     c                   eval      og143 = orgde3
247100061017     c                   if        §ogntw = 'DPD'
247200061017     c                   add       1             xx
247300061017     c                   eval      skpodpd(xx) = orgfil
247400061017     c                   endif
247500061017     c                   enddo
247600090211
247700090211      * carico tabella DPD
247800090211     c                   clear                   ds3idp
247900090211     c                   movel     '3I'          tblcod
248000090211     c                   movel(p)  'DPD'         tblkey
248100090211     c     ktab          chain     tabel00f
248200090211     c                   if        %found(tabel00f) and tblflg = *blanks
248300090211     c                   eval      ds3idp = tbluni
248400090211     c                   endif
248500001009
248600001023      * reperisce data e ora
248700001010     C                   TIME                    W0140
248800001010      * UDATE IN GGMMAAAA
248900001010     C                   MOVE      W0140         WDTGIO
249000001010      * UDATE IN AAAAMMGG
249100001010     C                   Z-ADD     WDTGIO        G02DAT
249200001010     C                   MOVEL     *BLANK        G02ERR
249300001010     C                   CALL      'XSRDA8'
249400001010     C                   PARM                    WLBDAT
249500001025     C                   Z-ADD     G02INV        DATEU
249600001010     C     *iso          MOVEL     DATEU         DATA_oggi
249700001010
249800001025     C                   z-add     dateu         §doggi
249900001016
250000001009     C     Kfnor         klist
250100001011     C                   kfld                    kpoe
250200001016     C                   kfld                    knsr
250300001009     C                   kfld                    knor
250400001016     C                   kfld                    knrv
250500001103
250600001103     C     Kforp         klist
250700001103     C                   kfld                    kpoe
250800001103     C                   kfld                    knsr
250900001103     C                   kfld                    knor
251000001103     C                   kfld                    knrv
251100001103     C                   kfld                    kdtv
251200001103     C                   kfld                    korv
251300010305
251400010305     C     Kfnorf        klist
251500010305     C                   kfld                    kpoe
251600010305     C                   kfld                    knsr
251700010305     C                   kfld                    knor
251800010305     C                   kfld                    knrv
251900010305     C                   kfld                    kdfo
252000010305     C                   kfld                    kofo
252100010305     C                   kfld                    kfao
252200090216
252300090216     c     kfnora        klist
252400090216     c                   kfld                    kpoe
252500090216     c                   kfld                    knsr
252600090216     c                   kfld                    knor
252700090216     c                   kfld                    knrv
252800090216     c                   kfld                    kfar
252900010119
253000010119     C     Ktab          klist
253100010119     C                   kfld                    codut
253200010119     C                   kfld                    kcod
253300010119     C                   kfld                    kkey
253400001030
253500120612     C     Kfidst        klist
253600120612     c                   kfld                    knpg
253700001030     C                   kfld                    knfv
253800001030     C                   kfld                    kfgs
253900021202
254000021202     C     Kfapd         klist
254100021202     C                   kfld                    kpdtip
254200021202     C                   kfld                    kpdpdr
254300061017
254400061017     c     kfnorn        klist
254500061017     c                   kfld                    ornpoe
254600061017     c                   kfld                    ornnsr
254700061017     c                   kfld                    ornnor
254800061017     c                   kfld                    ornnrv
254900061017     c                   kfld                    ornfar
255000061017
255100150526      * File FITGD32L
255200150526     c     K02TGD32      klist
255300040220     c                   kfld                    TGDtor
255400040220     c                   kfld                    TGDogg
255500001009
255600001011     C                   eval      kpoe = ppoe
255700001016     C                   eval      knsr = pnsr
255800001011     C                   eval      knor = pnor
255900001016     C                   eval      knrv = pnrv
256000001011
256100001025     C                   eval      v1cpoe = ppoe
256200001016     C                   eval      v1nsr  = pnsr
256300001025     C                   eval      v1nor  = pnor
256400001025     C                   eval      v1nrv  = pnrv
256500001115
256600001115     C                   movel     *zeros        v1ccra
256700001115     C                   movel     *zeros        v1ccor
256800001115     C                   movel     *zeros        v1ccrc
256900001006
257000001006     C                   endsr
257100040220      *****************************************************************
257200040220      * Abilitazione tasti funzionali
257300040220      *****************************************************************
257400040220     c     Abil_Fxx      begsr
257500040220
257600140304      * abilito i tasti funzione seguenti solo se non sono in
257700140304      * Interrogazione Storico Variazioni o Interrogazione Proposte Variazione
257800140304     c                   IF        not *in01 and not *in15
257900140304
258000040220      *
258100040220      *?Abilito F1=Aggiunta in Reclami?
258200040220      * (se richiamato da pgm. FIDNA1R...)
258300040303     c                   eval      *in08  =    (SDSprm >  1
258400040309     c                                      and IA1ins = ' ')
258500040220      *
258600040220      *?Abilito F16=Visualizzazione Reclami?
258700040220      * (se rilevo dei reclami già immessi)
258800040220     c                   eval      TGDtor  = 'O'
258900040220     c                   eval      TGDogg  = ds_OggORM
259000150526     c     K02TGD32      setll     FITGD000
259100150526     c                   eval      *in09   = (%equal(FITGD32L)   and
259200040303     c                                           (SDSprm =  1
259300040303     c                                        or (SDSprm >  1
259400040303     c                                        and IA1ins <> 'N')))
259500070509
259600151002      /free
259700151002       //?Abilito F9=Sollecito
259800151002       //?Se non è utente di sede
259900151002       //?se utente INT
260000151002       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
260100151002         IF  %subst(knmus:1:3) = 'INT' and DUTpou <> 046;
260200151002           xx = 1;
260300151002           FOR xx by 1 to %elem(skpog);
260400151002             IF  skpog(xx) <> *zeros and
260500151002                 %lookup(%dec(skpog(xx):3:0):skpoDPD) > 0;
260600151002               *in13 = *on;
260700151002               leave;
260800151002             ENDIF;
260900151002           ENDFOR;
261000151002         ENDIF;
261100151002      /end-free
261200061019
261300061023      *?Abilito F2=Dati DPD
261400080430      * se non provengo già dal pgm fieu55r
261500061024     c                   if        psce <> 'D'
261600061019     c     ormpoe        lookup    skpodpd                                30
261700061019     c  n30ormpor        lookup    skpodpd                                30
261800140304     c                   eval      *in14 = (*in30 and not *in12)
261900061024     c                   endif
262000140304     c                   ENDIF
262100140121
262200140121      /free
262300140121         *in44 = *off;
262400140526       //?F14 - Orari servizio
262500140304       //?Abilito solo se NON sono in
262600140304       //?Interrogazione Storico Variazioni
262700140304       //?Interrogazione Proposte Variazioni
262800140304         IF  not *in01 and not *in15;
262900140317           *in44 = *on;
263000140304         ENDIF;
263100140304
263200140121      /end-free
263300040220
263400040220     c                   EndSR
263500001010      *---------------------------------------------------------------------------------------------
263600041001** cmdt
263700120612OVRDBF FILE(FIDST01L) TOFILE(
263800001031** MSG  Lungh. 78                                                            *
263900001102Non esistono variazioni per l'ORM selezionato                                       1
264000001102Non esistono fasi per l'ORM selezionato                                             2
264100001102Non esistono proposte per l'ORM selezionato                                         3
264200061017L'ORM non è stato stampato impossibile sollecitare                                  4
264300061017Impossibile fare sollecito per ORM non automatizzato                                5
264400061017Impossibile fare sollecito per ORM già fatto o chiuso                               6
264500090217Impossibile sollecitare. Depot xxxx non associato ad un cliente                     7
264600090223Non esistono appuntamenti per l'ORM selezionato                                     8
264700131028** TF01
264800131211F1=Agg.Rich.  #
264900090211** TF02
265000131211F2=Dati DPD  #
265100090211** TF09
265200131211F9=Sollecito  #
265300130913** TF14
265400131211F14=OrariSr  #
265500131120** TF15
265600131211F15=Variazioni  #
265700131028** TF16
265800131211F16=Gest.Rich.  #
265900140110** TF19
266000131211F19=Proposte Var.  #
266100090217** TF20
266200131211F20=Fasi  #
266300090217** TF22
266400131211F22=Info App.  #
