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
004000131211      * 45 - HI tasto x Alert
004100131211      * 46 - HI tasto x Altri Dati
004200140121      * 48 - HI tasto x Note AUT
004300160407      * 49 - Visualizza tasto x Conferma Prenotazione Ritiro
004400061003      * 99 - errore QCMDEXEC
004500001010      ****************************************************************
004600001010
004700001030     FFNORM01L  IF   E           K DISK
004800070911     ffnorg01l  if   e           k disk
004900001030     FFNORV01L  IF   E           K DISK
005000131029     fFNORVE1L  if   e           k disk
005100001031     FFNORF01L  IF   E           K DISK
005200001031     FFNORP01L  IF   E           K DISK
005300131029     fFNORPE1L  if   e           k disk
005400061017     FFNORN02L  IF a E           K DISK
005500090211     ffnora01l  if   e           k disk
005600140409     ffnore01l  if   e           k disk
005700140121     fFNORT01L  if   e           k disk
005800140303     fFNORPT1L  if   e           k disk
005900001011     FAZORG01L  IF   E           K DISK
006000010119     FTABEL00F  IF   E           K DISK
006100120612     Ffidst01l  IF   E           K DISK    usropn
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
012300131028     d wEndF04         s               n
012400131028     d wEndF05         s               n
012500160407     d wEndF13         s               n
012600140206
012700140206     D network         S                   LIKE(ORGdit)
012800001006
012900001010      *  titolo videata (lunghezza massima 34)
013000010223     D TIT_A           C                   CONST('***   INTERROGAZIONE O.R.M.  -
013100001010     D                                       ***')
013200001031
013300001031      *   S C H I E R E
013400041001     d cmdt            s             60    dim(1) ctdata perrcd(1)
013500061017     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
013600061017     d skpodpd         s              3  0 dim(250)                             p.o. DPD
013700151001     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
013800131028     d tf01            s              1    dim(25) ctdata perrcd(25)
013900090211     d tf02            s              1    dim(25) ctdata perrcd(25)
014000090211     d tf09            s              1    dim(25) ctdata perrcd(25)
014100160407     d tf13            s              1    dim(25) ctdata perrcd(25)
014200130913     d tf14            s              1    dim(25) ctdata perrcd(25)
014300131120     d tf15            s              1    dim(25) ctdata perrcd(25)
014400131028     d tf16            s              1    dim(25) ctdata perrcd(25)
014500090211     d tf19            s              1    dim(25) ctdata perrcd(25)
014600090211     d tf20            s              1    dim(25) ctdata perrcd(25)
014700090211     d tf22            s              1    dim(25) ctdata perrcd(25)
014800090210     d $tf             s              1    dim(25)
014900001006
015000001010      *   D S   I N T E R N E / E S T E R N E
015100001009
015200001009     D WLBDAT          DS                  INZ
015300001009     D  G02DAT                 1      8  0
015400001009     D  G02INV                 9     16  0
015500001009     D  G02ERR                17     17
015600001009     D  G02TGI                18     22  0
015700001020
015800001023     D                 DS
015900001023     D  v1cra1                 1      3  0
016000001023     D  v1cra2                 4      7  0
016100001023     D  v1cra3                 8     10  0
016200001023     D  v1ccra                 1     10  0
016300001020
016400001023     D                 DS
016500001023     D  v1cor1                 1      3  0
016600001023     D  v1cor2                 4      7  0
016700001023     D  v1cor3                 8     10  0
016800001023     D  v1ccor                 1     10  0
016900001020
017000001023     D                 DS
017100001023     D  v1crc1                 1      3  0
017200001023     D  v1crc2                 4      7  0
017300001023     D  v1crc3                 8     10  0
017400001023     D  v1ccrc                 1     10  0
017500001031
017600001031     D                 DS
017700001031     D  v2dtvg                 1      2
017800001031     D  v2dtvm                 3      4
017900001031     D  v2dtva                 5      8
018000001031     D  v2dtv                  1      8  0
018100001031
018200001031     D                 DS
018300001031     D  v2orvo                 1      2
018400001031     D  v2orvm                 3      4
018500001031     D  v2orvs                 5      6
018600001031     D  v2orv                  1      6  0
018700001031
018800001031     D                 DS
018900001031     D  v4dtvg                 1      2
019000001031     D  v4dtvm                 3      4
019100001031     D  v4dtva                 5      8
019200001031     D  v4dtv                  1      8  0
019300001031
019400001031     D                 DS
019500001031     D  v4orvo                 1      2
019600001031     D  v4orvm                 3      4
019700001031     D  v4orvs                 5      6
019800001031     D  v4orv                  1      6  0
019900061017
020000061017     d                 ds
020100061017     d  ds_giorno              1      2  0
020200061017     d  ds_mese                3      4  0
020300061017     d  ds_anno                5      8  0
020400061017     d  ds_data                1      8  0
020500001009
020600001009     D Parm01          DS
020700001016     D  ppoe                          3  0
020800001016     D  pnor                          7  0
020900001016     D  pnsr                          2  0
021000001016     D  pnrv                          2  0
021100001016     D  psce                          1
021200001018     D  pfgs                          3  0
021300010205     D  ppor                          3  0
021400010205     D  pdtr                          8  0
021500010205     D  pmdb                         10
021600010205     D  pprb                         10
021700010205     D  pdts                          8  0
021800010205     D  prmp                          1
021900010205     D  pbrc                          1
022000010205     D  pref                          2
022100010205     D  pmio                          1
022200010205      * Da int.proposte
022300010109     D  pdtv                          8  0
022400010109     D  porv                          6  0
022500010109     D  putv                         10
022600010109     D  pric                          1
022700040220      *
022800040220      *?definizione oggetto reclamo come O.R.M.?
022900040220     d ds_OggORM       ds
023000040220     d  ORMpoe
023100040220     d  ORMnsr
023200040220     d  ORMnor
023300040220     d  ORMnrv
023400001009
023500040908     d dFar          e ds
023600010301     D DCMR          E DS
023700001010     D TIBS02DS      E DS
023800090202     d ds5P          e ds                  inz
023900090202     d   §5Ppgm      e                     inz('FIOR18R   ')
024000001030     D FIOR06DS      E DS
024100061017     d fior16ds      e ds                  inz
024200030606     d Dorm01        e ds
024300081125     d dorf01        e ds
024400060515     d fnlv55ds      e ds
024500061003     d og143         e ds
024600061017     d trul90ds      e ds
024700061019     d fieu55ds      e ds
024800070911     d fidg09ds      e ds
024900090211     d tisie3ds      e ds                  inz
025000090211     d truldepds     e ds
025100090211     d ds3idp        e ds
025200140206     d TISI95DS      e ds
025300151001     d dUTE01        e ds
025400001010
025500001010      *
025600001010     D KPJBA         E DS
025700040220     D FIDNA1ds      e ds
025800040226      *
025900040226     d FIDNA2ds      e ds                  inz
026000040226     d  IA2mod       e                     inz('M')
026100040226     d  IA2tor       e                     inz('O')
026200130913
026300130913      * - Parametri x Interrogazione orari servizio
026400140317     d TRULORSds     e ds
026500090202
026600090202      * - Parametri x Controllo profilo utenti
026700090202     d TIBS34ds      e ds
026800090202      * - Ds di riferimento al file esterno AZUTE00F
026900090202     d AzuteDs       e ds                  extname(AZUTE00F)
027000090202      * - Ds per dati organigramma
027100090202     d dDatiUte      e ds
027200140110
027300140110      // ds per note AUT
027400140110     d FIORT1DS      e ds
027500131028
027600131028      // ds per rcd 'O' FNORE - orari apertura
027700131028     d dOREorari     e ds
027800140429
027900140429      // ds per rcd 'G' FNORE - generale
028000140429     d dOREgen       e ds
028100140429
028200140429      // ds per rcd 'S' FNORE - SMS
028300140429     d dOREsms       e ds
028400140318
028500140318      // ds per recupero ora presunta ritiro da PDA
028600140318     d FIOR01DS      e ds
028700151001
028800151001      // Carico filiali gestite dall'utente
028900151001     d TRUL31DS      e ds
029000090202
029100001010      *
029200001010     D                SDS
029300040220     d  SDSprm           *parms
029400001010     D  VTCPGM                 1     10
029500090210
029600090210      *   C O S T A N T I
029700090210     d cf2413          c                   const('F24=AlTasti(1/3)')
029800090210     d cf2423          c                   const('F24=AlTasti(2/3)')
029900090210     d cf2433          c                   const('F24=AlTasti(3/3)')
030000090210     d cf2412          c                   const('F24=AlTasti(1/2)')
030100090210     d cf2422          c                   const('F24=AlTasti(2/2)')
030200140110
030300130913      //---------------------------------------------------------------
030400130913      //?Definizione procedure utilizzate.
030500130913      //---------------------------------------------------------------
030600140110
030700140110      // - Gestione note AUT
030800140110     d fiort1r         pr                  extpgm('FIORT1R')
030900140110     d  kpjba                              likeds(KPJBA)
031000140110     d  fiort1ds                           likeds(FIORT1DS)
031100140320
031200140320      // - Gestione note AUT x proposte
031300140320     d fiort1c         pr                  extpgm('FIORT1C')
031400140320     d  kpjba                              likeds(KPJBA)
031500140320     d  fiort1ds                           likeds(FIORT1DS)
031600130913
031700130913      // -Interrogazione orari di servizio
031800140317     d trulorsr        pr                  extpgm('TRULORSR')
031900130913     d  kpjba                              likeds(KPJBA)
032000140317     d  trulorsds                          likeds(TRULORSDS)
032100140206
032200140206      // - Cappario
032300140206     d tisi95r         pr                  extpgm('TISI95R')
032400140206     d  tisi95ds                           likeds(TISI95DS)
032500140318
032600140318      // - Recupero ora presunta ritiro
032700140318     d fior01r         pr                  extpgm('FIOR01R')
032800140318     d  fior01ds                           likeds(FIOR01DS)
032900131125
033000131125      //---------------------------------------------------------------
033100131125      //?Definizione prototipi utilizzati.
033200131125      //---------------------------------------------------------------
033300131125
033400131125      /copy gaitrasrc/srcprotopr,tibs02r
033500131028
033600131028      //---------------------------------------------------------------
033700131028      //?Definizione key-list.
033800131028      //---------------------------------------------------------------
033900131028
034000131028       // - File FNORE01L
034100131029     d k05fnore      e ds                  extname(FNORE01L:*key)
034200131028     d                                     prefix(k_)
034300131028     d                                     inz
034400131029
034500131029       // - File FNORVE1L
034600131029     d k07fnorve     e ds                  extname(FNORVE1L:*key)
034700131029     d                                     prefix(k_)
034800131029     d                                     inz
034900131029
035000131029       // - File FNORPE1L
035100131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
035200131029     d                                     prefix(k_)
035300131029     d                                     inz
035400001010
035500001006      *------------------------------------------------------------------------*
035600001103
035700001030      * Carica i dati dal file FNORM
035800001030     C     kfnor         chain     fnorm01L
035900001115     C                   if        %found(fnorm01l)
036000001115      * Annullato
036100001115     C                   if        ORMatb <> *blanks
036200001115     C                   movel     'ANNULLATO'   v1ann
036300001115     C                   else
036400001115     C                   clear                   v1ann
036500001115     C                   endif
036600001103      * Se è richiamato con 'P' deve solo gestire le proposte
036700001103     C                   if        psce = 'P'
036800001103     C                   goto      richiama
036900001103     C                   endif
037000010315
037100010109     C                   else
037200010109     C                   if        psce = 'P' and pric = 'P'
037300010109     C                   movel     'E'           psce
037400010109     C                   movel     parm01        kpjbu
037500010109     C                   goto      richiama
037600010109     C                   endif
037700001009     C                   endif
037800040220
037900040220      * Abilito eventuali tasti funzionali
038000140304     c                   exsr      Abil_Fxx
038100001006
038200001006      * Emissione 1° Videata
038300001010
038400001006     C                   do        *hival
038500120612     c                   eval      *in01 = *off
038600140303     c                   eval      *in15 = *off
038700010315     C                   exsr      Sr_Carvid
038800010315     C                   exsr      Sr_Decodifica
038900090211     c                   ExSr      Sr_Tastifun
039000001010
039100061017     c     for01         tag
039200001031     C                   eval      v1mod = 'VISUALIZZAZIONE'
039300001031
039400001103     C                   setoff                                       010203
039500140304     c                   setoff                                       1715
039600040908     c                   Eval      *In10 = *Off
039700001030     C                   exfmt     fior071
039800001031     C                   setoff                                       28
039900001011
040000001011      * F12=Ritorno
040100001023     C                   if        *inkl = *on
040200001023     C                   leave
040300001023     C                   endif
040400040220
040500040220      *
040600040220      *?F1 - Aggiunta bolla in reclamo?
040700040226if  1c                   if        *inKA  = *on
040800040220     c                   clear                   FIDNA1ds
040900040220     c                   eval      IA1ins = 'S'
041000040220     c                   eval      IA1tor = 'O'
041100040220     c                   eval      IA1ogg = ds_OggORM
041200040226     c                   leave
041300040220e   1c                   endif
041400040226      *
041500040226      *?F16 - Gestione reclami?
041600040226if  2c                   if        *inKQ  = *on
041700040226     c                   reset                   FIDNA2ds
041800040226if  3c                   if        SDSprm > 1
041900040226     c                   eval      IA2mod = 'I'
042000040226e   3c                   endif
042100040226     c                   eval      IA2ogg = ds_OggORM
042200040226     c                   call      'FIDNA2R'
042300040226     c                   parm                    KPJBA
042400040226     c                   parm                    FIDNA2ds
042500040226if  3c                   if        OA2f03 = 'S'
042600040226     c                   leave
042700040226e   3c                   endif
042800040226e   2c                   endif
042900130913
043000130913      /free
043100130913
043200130913       //?F14=Interrogazione oraro di servizio
043300130913       IF  *inkn;
043400130913         exsr F14D01;
043500130913         iter;
043600130913       ENDIF;
043700131028
043800131028       //?F4=Dati Avviso Ritiro
043900131028       IF  *inkd;
044000131028         exsr F04D01;
044100131028         iter;
044200131028       ENDIF;
044300131028
044400131028       //?F5=Altri dati
044500131028       IF  *inke;
044600131028         exsr F05D01;
044700131028         iter;
044800131028       ENDIF;
044900160407
045000160407       //?F13=Dati Conferma Prenotazione Ritiro
045100160407       IF  *inkm;
045200160407         exsr F13D01;
045300160407         iter;
045400160407       ENDIF;
045500140110
045600140110       //?F17=Note AUT
045700140110       IF  *inkr;
045800140110         exsr F17D01;
045900140110         iter;
046000140110       ENDIF;
046100130913
046200130913      /end-free
046300001030
046400010402      * F20=Fasi
046500010402     C                   if        *inku = *on
046600001031     C                   exsr      Sr_Fasi
046700001030     C                   endif
046800120612
046900131120      * F15=Variazioni
047000131120     C                   if        *inkp = *on
047100001102     C                   exsr      Sr_Variazioni
047200001030     C                   endif
047300001011
047400001025      * F18=Note
047500001025     C                   if        *inks = *on
047600001025     C                   exsr      Sr_Note
047700001025     C                   endif
047800001020      * F19=Proposte
047900001030     C                   if        *inkt = *on
048000001031     C                   exsr      Sr_Proposte
048100001020     C                   endif
048200061017      * F9=Sollecito
048300061017     c                   if        *inki
048400061017     c                   exsr      sr_sollecito
048500061017     c                   endif
048600061023      * F2=Dati DPD
048700061023     c                   if        *inkb
048800061019     c                   exsr      sr_datidpd
048900061019     c                   endif
049000090211      * F22=Info appuntamento
049100090211     c                   if        *inkw
049200090211     c                   exsr      sr_ormapp
049300090211     c                   endif
049400090210      * F24=Altri tasti  ?
049500090210     c                   if        *inky and $loop > 1
049600090210     c                   exsr      sr_f24
049700090210     c                   goto      for01
049800090210     c                   endif
049900001010
050000001006     C                   enddo
050100001023
050200001103     C     richiama      tag
050300001103
050400010109     C                   if        psce = 'P'
050500010109     C                   if        pric = *blanks
050600001103     C                   exsr      Sr_Proposte
050700010109     C                   endif
050800010109
050900010109     C                   if        pric = 'P'
051000010109     C                   exsr      Sr_Cardatiprop
051100010109     C                   exsr      Sr_Visproposte
051200010109     C                   endif
051300001103     C                   endif
051400001010
051500001006     C                   eval      *inlr = *on
051600001009      **********************************************************************
051700001023      * CARICA DATI NELLA VIDEATA DAL FILE
051800001009      **********************************************************************
051900001009     C     Sr_Carvid     begsr
052000001009
052100001129     C                   movel     ORMtco        v1ctco
052200001016     C                   movel     ORMtor        v1ctor
052300001023      * Inversione data emissione ORM
052400001016     C                   clear                   wlbdat
052500001017     C                   z-add     ORMdao        G02inv
052600001017     C                   movel     '3'           G02err
052700001016     C                   call      'XSRDA8'
052800001016     C                   parm                    wlbdat
052900001025     C                   z-add     G02dat        v1dao
053000001215     C                   movel     ORMoao        v1oao
053100001016      * Dati Ordinante
053200001025     C                   z-add     ORMcor        v1ccor
053300001025     C                   movel     ORMrso        v1rso
053400001025     C                   movel     ORMino        v1ino
053500001025     C                   movel     ORMcao        v1cao
053600001025     C                   movel     ORMloo        v1loo
053700001025     C                   movel     ORMpro        v1pro
053800001025     C                   movel     ORMnao        v1nao
053900001016      * Dati Ritiro a
054000001025     C                   z-add     ORMcra        v1ccra
054100001025     C                   movel     ORMrsr        v1rsr
054200001025     C                   movel     ORMinr        v1inr
054300001025     C                   movel     ORMcar        v1car
054400001025     C                   movel     ORMlor        v1lor
054500001025     C                   movel     ORMprr        v1prr
054600001025     C                   movel     ORMnar        v1nar
054700001016     C                   movel     ORMrer        v1rer
054800001016     C                   movel     ORMter        v1ter
054900001016      * Dati Consegna a
055000001025     C                   z-add     ORMcrc        v1ccrc
055100001016     C                   movel     ORMrsc        v1rsc
055200001016     C                   movel     ORMinc        v1inc
055300001016     C                   movel     ORMloc        v1loc
055400001016     C                   movel     ORMcac        v1cac
055500001016     C                   movel     ORMprc        v1prc
055600001016     C                   movel     ORMnac        v1nac
055700001016
055800010315     C                   if        ORMffd = 'S'
055900010315     C                   eval      v1ffd = 'SI'
056000010315     C                   else
056100010315     C                   eval      v1ffd = 'NO'
056200010315     C                   endif
056300010315
056400001016     C                   movel     ORMnam        v1nam
056500001023      * Inversione data ritiro
056600001017     C                   z-add     ORMdar        G02inv
056700001017     C                   movel     '3'           G02err
056800001016     C                   call      'XSRDA8'
056900001016     C                   parm                    wlbdat
057000001025     C                   z-add     G02dat        v1dar
057100001016     C                   movel     ORMorr        v1orr
057200001016
057300001025     C                   z-add     ORMncl        v1ncl
057400001025     C                   z-add     ORMpkg        v1pkg
057500001025     C                   z-add     ORMvlm        v1vlm
057600001025     C                   z-add     ORMbnc        v1bnc
057700001025     C                   z-add     ORMblc        v1blc
057800001025     C                   z-add     ORMatt        v1att
057900001025     C                   z-add     ORMmtc        v1mtc
058000001016
058100010315     C                   if        ORMpag = 'M'
058200010315     C                   eval      v1pag = 'Mitt.'
058300010315     C                   endif
058400010315     C                   if        ORMpag = 'D'
058500010315     C                   eval      v1pag = 'Dest.'
058600010315     C                   endif
058700010315     C                   if        ORMpag = 'O'
058800010315     C                   eval      v1pag = 'Ord. '
058900010315     C                   endif
059000010315
059100001025     C                   z-add     ORMksc        v1ksc
059200001020     C                   movel     ORMctr        v1ctr
059300010315
059400010315     C                   if        ORMddt = 'S'
059500010315     C                   eval      v1ddt = 'SI'
059600010315     C                   endif
059700010315     C                   if        ORMddt = 'N'
059800010315     C                   eval      v1ddt = 'NO'
059900010315     C                   endif
060000010315
060100001016     C                   movel     ORMpor        v1cpor
060200001016     C                   movel     ORMpoc        v1cpoc
060300001016     C                   movel     ORMno1        v1not1
060400001016     C                   movel     ORMno2        v1not2
060500001025     C                   z-add     ORMfao        v1fao
060600001025     C                   z-add     ORMpos        v1cpos
060700001025     C                   z-add     ORMors        v1ors
060800001027     C                   movel     ORMrfa        v1rfa
060900001221     C                   move      ORMsto        v1sto
061000001030     C                   z-add     ORMndc        v1ndc
061100001030      * Inversione data distinta
061200001030     C                   clear                   wlbdat
061300001030     C                   z-add     ORMddc        G02inv
061400001030     C                   movel     '3'           G02err
061500001030     C                   call      'XSRDA8'
061600001030     C                   parm                    wlbdat
061700001030     C                   z-add     G02dat        v1ddc
061800010226
061900010226      * Cerca codice autotrasportatore
062000071108     c                   if        ormndc > 0
062100001030     C                   z-add     ORMndc        knfv
062200001030     C                   z-add     ORMpor        kfgs
062300120612     C     kfidst        chain     fidst01l
062400060515      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
062500120612     c                   if        not %found(fidst01l)
062600060515     c                   clear                   fnlv55ds
062700060515     c                   eval      d55tpt = '6'
062800060515     c                   move      ormpor        d55lin
062900060515     c                   eval      d55drf = ormddc
063000060515     c                   call      'FNLV55R'
063100060515     c                   parm                    fnlv55ds
063200060515     c                   if        d55err = *blanks and d55tfa <> d55lin
063300060515     c                   move      d55tfa        kfgs
063400120612     c     kfidst        chain     fidst01l
063500060515     c                   endif
063600060515     c                   endif
063700120612     C                   if        %found(fidst01l)
063800001030     C                   eval      v1cpdc = DSTpdr
063900001030     C                   endif
064000071108     c                   else
064100071108     c                   clear                   v1cpdc
064200071108     c                   endif
064300071108
064400010226      * Autotrasportatore evidenziato a video
064500010226     C                   eval      *in06 = (v1cpdc > 0)
064600010226
064700010219      * Distinta evidenziata a video
064800010219     C                   eval      *in05 = (v1ndc > 0)
064900070924
065000070924      * se Stampato faccio vedere la scritta
065100070924     c                   eval      v1dst = ormdst
065200070924     c                   eval      *in16 = (v1dst > 0)
065300010119
065400010315     C                   if        ORMspi = 'S'
065500010315     C                   eval      v1spi = 'SI'
065600010315     C                   else
065700010315     C                   eval      v1spi = 'NO'
065800010315     C                   endif
065900010309      * Causale
066000010305     C                   exsr      Sr_Carcau
066100010322
066200010322     C                   eval      *in40 = *off
066300010322     C                   eval      *in41 = *off
066400010322     C                   eval      *in42 = *off
066500090211     C                   eval      *in43 = *off
066600131211     C                   eval      *in45 = *off
066700131211     C                   eval      *in46 = *off
066800140121     C                   eval      *in48 = *off
066900160407     C                   eval      *in49 = *off
067000131211
067100010322      * Controllo se ci sono Variazioni
067200010322     C     kfnor         setll     fnorv01l
067300010322     C                   do        *hival
067400010322     C     kfnor         reade     fnorv01l
067500010322     C                   if        %eof
067600010322     C                   leave
067700010322     C                   endif
067800010322     C                   eval      *in40 = *on
067900010322     C                   leave
068000010322     C                   enddo
068100010322      * Controllo se ci sono Proposte
068200010322     C     kfnor         setll     fnorp01l
068300010322     C                   do        *hival
068400010322     C     kfnor         reade     fnorp01l
068500010322     C                   if        %eof
068600010322     C                   leave
068700010322     C                   endif
068800010322     C                   eval      *in41 = *on
068900010322     C                   leave
069000010322     C                   enddo
069100010322      * Controllo se ci sono Note
069200061017     C     kfnor         setll     fnorn02l
069300010322     C                   do        *hival
069400061017     C     kfnor         reade     fnorn02l
069500010322     C                   if        %eof
069600010322     C                   leave
069700010322     C                   endif
069800010322     C                   eval      *in42 = *on
069900010322     C                   leave
070000010322     C                   enddo
070100090211
070200090211      * controllo se inserite info appuntamento
070300090211     c     kfnor         setll     fnora01l
070400090211     c                   do        *hival
070500090211     c     kfnor         reade     fnora01l
070600090211     c                   if        %eof
070700090211     c                   leave
070800090211     c                   endif
070900090211     c                   eval      *in43 = *on
071000090211     c                   leave
071100090211     c                   enddo
071200030606
071300030606     C                   eval      dorm01 = ORMflo
071400030606
071500030606      * ORM commissionato
071600030606     c                   If        §orCom = 'S'
071700030606     c                   Eval      V1dCom = 'COMMISSIONATO'
071800030606     c                   Else
071900030606     c                   Clear                   V1dCom
072000030606     c                   EndIf
072100050502
072200050502      * ORM con Ricevuta di Ritiro
072300050502     c                   Eval      *In11 = (§orsrm = 'S')
072400100225
072500100225      /free
072600140429       //?Imposto costante se ORM con preavviso (CEVA)
072700100225       *in18 = *off;
072800100225       IF  §ormpre <> *blanks;
072900100225         *in18 = *on;
073000100225         IF  §ormpre = 'M';
073100100225         v1este = 'Preavviso: '+
073200100225                  'Mail';
073300100225         ENDIF;
073400100225       ENDIF;
073500100225      /end-free
073600061017
073700061017     c                   eval      v1cfcs = ormfcs
073800070911      * giro
073900070911     c                   clear                   v1ccgi
074000070911     c     kfnor         chain     fnorg01l
074100070911     c                   if        %found(fnorg01l)
074200070911     c                   eval      v1ccgi = orgcgi
074300070911     c                   endif
074400131028
074500131028      /free
074600131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
074700131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
074800131211           *in46 = *on;
074900131211         ENDIF;
075000131028       //?Recupero dati da estensione FNORE
075100131028       //?Rcd 'O ' --> Orari apertura
075200131028         k_OREpoe = ORMpoe;
075300131028         k_OREnsr = ORMnsr;
075400131028         k_OREnor = ORMnor;
075500131028         k_OREnrv = ORMnrv;
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
076700140429
076800140429         //?Rcd 'S ' --> SMS per invio alert
076900140429           k_OREtrc = 'S ';
077000140429           clear dOREsms;
077100140429         //?Aggancio il rcd
077200140429           chain  %kds(k05fnore) FNORE01L;
077300140429           IF  %found(FNORE01L);
077400140429             dOREsms = OREdati;
077500140429           ENDIF;
077600140429           w03sms = §OREsms;
077700140429         //?Rcd 'MA' --> Mail per invio alert
077800140429           k_OREtrc = 'MA';
077900140429           clear w03mail;
078000140429         //?Aggancio il rcd
078100140429           chain  %kds(k05fnore) FNORE01L;
078200140429           IF  %found(FNORE01L);
078300140429             w03mail = OREdati;
078400140429           ENDIF;
078500160311
078600160407           IF  W03mail <> *blanks or W03sms <> *blanks;
078700140429             *in45 = *on;
078800140429           ENDIF;
078900140429
079000140429       //?Imposto costante se ORM con Alert via Mail/SMS
079100140515       //?se non è Preavviso CEVA
079200140515         IF  not *in18;
079300140515           k_OREtrc = 'G';
079400140515           clear dOREgen;
079500140515           chain  %kds(k05fnore) FNORE01L;
079600140515           IF  %found(FNORE01L);
079700140515             dOREgen = OREdati;
079800140515           ENDIF;
079900140515           IF  §OREfimo = 'S' or §OREfiso = 'S';
080000140515             *in18 = *on;
080100140515             V1este = 'Preavviso: Mail/SMS';
080200140515           ENDIF;
080300140515         ENDIF;
080400160407
080500160407         IF  §OREfmco = 'S' or §OREfsco = 'S';
080600160407           *in49 = *on;
080700160407         //?Rcd 'SC' --> SMS per Conferma Prenotazione Ritiro
080800160407           k_OREtrc = 'SC';
080900160407           clear w04sms;
081000160407         //?Aggancio il rcd
081100160407           chain  %kds(k05fnore) FNORE01L;
081200160407           IF  %found(FNORE01L);
081300160407             w04sms = OREdati;
081400160407           ENDIF;
081500160407         //?Rcd 'MC' --> Mail per Conferma Prenotazione Ritiro
081600160407           k_OREtrc = 'MC';
081700160407           clear w04mail;
081800160407         //?Aggancio il rcd
081900160407           chain  %kds(k05fnore) FNORE01L;
082000160407           IF  %found(FNORE01L);
082100160407             w04mail = OREdati;
082200160407           ENDIF;
082300160407         ENDIF;
082400160224
082500160224       //?Recupero NPR
082600160224         k_OREtrc = 'NP';
082700160224         clear OREdati;
082800160224       //?Aggancio il rcd
082900160224         chain  %kds(k05fnore) FNORE01L;
083000160224         IF  %found(FNORE01L);
083100160224           V1npr = OREdati;
083200160224         ENDIF;
083300140121
083400140121       //?Controllo se ci sono Note AUT
083500140121         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
083600140121         IF  %found(FNORT01L);
083700140121           *in48 = *on;
083800140121         ENDIF;
083900131211
084000131028      /end-free
084100001009
084200001009     C                   endsr
084300001030      **********************************************************************
084400001030      * DECODIFICHE
084500001030      **********************************************************************
084600001030     C     Sr_Decodifica begsr
084700001030
084800001030      * Fase
084900001129     C                   clear                   v1dfao
085000001030     C                   clear                   TIBS02DS
085100001030     C                   movel     'C'           T02mod
085200001030     C                   movel     knsif         t02sif
085300001030     C                   movel     'FAR'         t02cod
085400001030     C                   movel(p)  v1fao         T02ke1
085500001030     C                   call      'TIBS02R'
085600001030     C                   parm                    KPJBA
085700001030     C                   parm                    TIBS02DS
085800040908     c                   Eval      dFar = T02Uni
085900040908     c                   Eval      V1dfao = d§fardes
086000001030
086100001030      * Tipo ordine assegnazione
086200001129     C                   clear                   v1dtco
086300001030     C                   clear                   TIBS02DS
086400001030     C                   movel     'C'           T02mod
086500001030     C                   movel     knsif         t02sif
086600001129     C                   movel     'TCO'         t02cod
086700001129     C                   movel(p)  v1ctco        T02ke1
086800001030     C                   call      'TIBS02R'
086900001030     C                   parm                    KPJBA
087000001030     C                   parm                    TIBS02DS
087100001129     C                   movel     t02uni        v1dtco
087200001030
087300001030      * P.O. ritiro
087400061003     c                   eval      *in22 = *off
087500061003     c                   clear                   v1dfcs
087600061003     c                   clear                   og143
087700001129     C                   clear                   v1dpor
087800140206     c                   clear                   network
087900001030     C                   movel     v1cpor        kazorg
088000001030     C     kazorg        chain     azorg01L
088100001030     C                   if        %found(azorg01l)
088200001030     C                   movel     ORGdes        v1dpor
088300140206     C                   movel     orgdit        network
088400061003     c                   eval      og143 = orgde3
088500001030     C                   endif
088600061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
088700061003      * europolitan invece ormai è obsoleta
088800061003     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
088900061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
089000061003      * partener se caricata e la visualizzo al posto della zona
089100061017     c                   if        v1cfcs <> *blanks
089200061003     c                   clear                   tibs02ds
089300061003     c                   eval      t02mod = 'C'
089400061003     c                   eval      t02sif = knsif
089500061003     c                   eval      t02cod = 'MIP'
089600061017     c                   eval      t02ke1 = v1cfcs
089700061003     c                   call      'TIBS02R'
089800061003     c                   parm                    kpjba
089900061003     c                   parm                    tibs02ds
090000061003     c                   eval      v1dfcs = t02uni
090100061003     c                   eval      *in22 = *on
090200061003     c                   endif
090300061003     c                   endif
090400070911
090500070911      * Giro
090600070911     c                   clear                   v1dcgi
090700070911     c                   clear                   fidg09ds
090800070911     c                   eval      d09iop0 = '001'
090900071127     c                   eval      d09ifgs = v1cpor
091000070911     c                   eval      d09idat = *date
091100070911     c                   eval      d09icgi = v1ccgi
091200070911     c                   eval      d09itug = 'R'
091300080318     c                   movel     kpjbu         §kpjbu
091400070911     c                   eval      kpjbu = fidg09ds
091500070911     c                   call      'FIDG09R'
091600070911     c                   parm                    kpjba
091700070911     c                   eval      fidg09ds = kpjbu
091800080318     c                   movel     §kpjbu        kpjbu
091900070911     c                   if        d09ocgi <> ' '
092000070911     c                   eval      v1dcgi = d09odes
092100070911     c                   endif
092200001030
092300001030      * P.O. Consegna
092400001129     C                   clear                   v1dpoc
092500001030     C                   movel     v1cpoc        kazorg
092600001030     C     kazorg        chain     azorg01L
092700001030     C                   if        %found(azorg01l)
092800001030     C                   movel     ORGdes        v1dpoc
092900001030     C                   endif
093000001030
093100001030      * Tipo ordine
093200001129     C                   clear                   v1dtor
093300001030     C                   clear                   TIBS02DS
093400001030     C                   movel     'C'           T02mod
093500001030     C                   movel     knsif         t02sif
093600001030     C                   movel     'TOR'         t02cod
093700001030     C                   movel(p)  v1ctor        T02ke1
093800001030     C                   call      'TIBS02R'
093900001030     C                   parm                    KPJBA
094000001030     C                   parm                    TIBS02DS
094100001030     C                   movel     t02uni        v1dtor
094200001103
094300010205      * Priorità
094400010205     C                   clear                   v1dsto
094500010205     C                   if        v1sto <> *zeros
094600001103     C                   clear                   TIBS02DS
094700001103     C                   movel     'C'           T02mod
094800001103     C                   movel     knsif         t02sif
094900001103     C                   movel     'STO'         t02cod
095000131029     C                   eval      t02ke1 = %editc(ORMsto:'X')
095100001103     C                   call      'TIBS02R'
095200001103     C                   parm                    KPJBA
095300001103     C                   parm                    TIBS02DS
095400010205     C                   movel     t02uni        v1dsto
095500010205     C                   endif
095600001030
095700010226      * Autotrasportatore
095800001129     C                   clear                   v1dpdc
095900021202     C                   movel     v1cpdc        kpdpdr
096000021202     C     kfapd         chain     fiapd01L
096100021202     C                   if        %found(fiapd01l)
096200001030     C                   movel     APDrsc        v1dpdc
096300001030     C                   endif
096400001031      * ORM fisso
096500001031     C                   eval      *in04 = (v1ors <> *zeros)
096600001030
096700001030     C                   endsr
096800010305      **********************************************************************
096900010305      * Cerca causale chiusura e decodifica
097000010305      **********************************************************************
097100010305     C     Sr_Carcau     begsr
097200010305
097300010305     C                   eval      kpoe = ORMpoe
097400010305     C                   eval      knsr = ORMnsr
097500010305     C                   eval      knor = ORMnor
097600010305     C                   eval      knrv = ORMnrv
097700010305     C                   eval      kdfo = ORMdfo
097800010305     C                   eval      kofo = ORMofo
097900010305     C                   eval      kfao = ORMfao
098000110418     C**!!!kfnorf        chain     fnorf01L
098100110418     C**!!!              if        %found(fnorf01l)
098200110418     c     kfnorf        setll     fnorf01l
098300110418     c                   do        *hival
098400110418     c     kfnorf        reade     fnorf01l
098500110418     c                   if        %eof(fnorf01l)
098600110418     c                   leave
098700110418     c                   endif
098800010305     C                   eval      v1cau = orfcar
098900110418     C                   enddo
099000010305
099100010305     C                   clear                   dcmr
099200010305     C                   clear                   TIBS02DS
099300010305     C                   movel     'C'           T02mod
099400010305     C                   movel     knsif         t02sif
099500010305     C                   movel     'CMR'         t02cod
099600010305     C                   movel(p)  v1cau         T02ke1
099700010305     C                   call      'TIBS02R'
099800010305     C                   parm                    KPJBA
099900010305     C                   parm                    TIBS02DS
100000010305     C                   movel     t02uni        dcmr
100100010305     C                   movel     d§cmrdes      v1dcau
100200010309
100300010309     C                   eval      *in07 = (v1cau <> *blanks)
100400010305
100500010305     C                   endsr
100600001031      **********************************************************************
100700081125      * F20 - Fasi
100800001031      **********************************************************************
100900001031     C     Sr_Fasi       BEGSR
101000001031
101100001031     C                   z-add     1             recsff
101200001031      * Pulisco subfile
101300001031     C                   z-add     *zeros        nrrf              4 0
101400001031     C                   eval      *in21 = *off
101500001031     C                   write     fior07cf
101600001031     C                   eval      *in20 = *on
101700001031     C                   eval      *in21 = *on
101800001031
101900001031      * Carico il subfile in base alle scelte fatte
102000001031     C     kfnor         setll     fnorf01l
102100001031     C                   do        *hival
102200001031     C     kfnor         reade     fnorf01l
102300001031     C                   if        %eof
102400001031     C                   leave
102500001031     C                   endif
102600001031
102700001031     C                   z-add     v1cpoe        v3cpoe
102800001031     C                   z-add     v1nsr         v3nsr
102900001031     C                   z-add     v1nor         v3nor
103000001031     C                   z-add     v1nrv         v3nrv
103100081125
103200081125     c                   eval      dorf01 = orfflo
103300001031
103400001031     C                   exsr      Sr_CarFasi
103500001031     C                   enddo
103600001031      * Se non ho caricato emetto una videata di segnalazione.
103700001031     C                   if        nrrf = *zeros
103800001031     C                   movel     msg(2)        v1cmsg
103900001031     C                   seton                                        28
104000001031     C                   goto      endfasi
104100001031     C                   endif
104200001031
104300001031      * Se ho caricato emetto il subfile
104400001031     C                   if        nrrf > *zeros
104500001102     C                   do        *hival
104600001031     C                   write     fior073
104700001031     C                   exfmt     fior07cf
104800001031      * F12=Ritorno
104900001102     C   kl              leave
105000001102     C                   enddo
105100001102     C                   endif
105200001031
105300001031     C     endfasi       endsr
105400001031      **********************************************************************
105500001031      * CARICO SUBFILE FASI
105600001031      **********************************************************************
105700001031     C     Sr_CarFasi    begsr
105800001031
105900001031      * Inversione data modifica
106000001031     C                   z-add     ORFdae        G02inv
106100001031     C                   movel     '3'           G02err
106200001031     C                   call      'XSRDA8'
106300001031     C                   parm                    wlbdat
106400001031     C                   movel     G02dat        v3dae
106500001031
106600001031     C                   movel     ORFore        v3ore
106700001031     C                   eval      v3cpog = ORFpog
106800001031     C                   eval      v3ute  = ORFpue
106900001031     C                   eval      v3far  = ORFfar
107000001031      * Decodifica fase
107100001031     C                   clear                   TIBS02DS
107200001031     C                   movel     'C'           T02mod
107300001031     C                   movel     knsif         t02sif
107400001031     C                   movel     'FAR'         t02cod
107500001031     C                   movel(p)  v3far         T02ke1
107600001031     C                   call      'TIBS02R'
107700001031     C                   parm                    KPJBA
107800001031     C                   parm                    TIBS02DS
107900040908     C**!!!              movel     t02uni        v3dfar
108000040908     c                   Eval      dFar = T02Uni
108100040908     c                   Eval      V3dfar = d§fardes
108200001031
108300001031     C                   if        ORFcar <> *blanks
108400001031     C                   seton                                        02
108500001031     C                   eval      v3car  = ORFcar
108600001031      * Decodifica causale
108700010301     C                   clear                   dcmr
108800001031     C                   clear                   TIBS02DS
108900001031     C                   movel     'C'           T02mod
109000001031     C                   movel     knsif         t02sif
109100001128     C                   movel     'CMR'         t02cod
109200001031     C                   movel(p)  v3car         T02ke1
109300001031     C                   call      'TIBS02R'
109400001031     C                   parm                    KPJBA
109500001031     C                   parm                    TIBS02DS
109600010301     C                   movel     t02uni        dcmr
109700010301     C                   movel     d§cmrdes      v3dcar
109800001107     C                   else
109900001107     C                   setoff                                       02
110000001107     C                   clear                   v3car
110100001107     C                   clear                   v3dcar
110200001031     C                   endif
110300081125      * visualizzo se 'No addebito'
110400081125     c                   eval      *in17 = *off
110500081125     c                   if        §orfadd = 'S'
110600081125     c                   eval      *in17 = *on
110700081125     c                   endif
110800040908
110900040908      * Se ci sono carico anche i dati della distinta
111000040908if  1c                   If        OrfNdc > *Zeros
111100040908     c                   Eval      *In10 = *On
111200040908     c                   Eval      v3ndc = OrfNdc
111300040908      * giro la data della distinta
111400040908     c                   Clear                   wlbdat
111500040908     c                   Z-add     OrfDdc        G02inv
111600040908     c                   Movel     '3'           G02err
111700040908     c                   Call      'XSRDA8'
111800040908     c                   Parm                    wlbdat
111900040908     c                   Z-add     G02dat        v3ddc
112000040915
112100040908      * cerco il codice dell'autotrasportatore
112200040908     c                   Z-add     OrfNdc        knfv
112300040908     c                   Z-add     OrfFgs        kfgs
112400120612     c     kfidst        Chain     fidst01l
112500060522      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
112600120612     c                   if        not %found(fidst01l)
112700060522     c                   clear                   fnlv55ds
112800060522     c                   eval      d55tpt = '6'
112900060522     c                   move      orffgs        d55lin
113000060522     c                   eval      d55drf = ormddc
113100060522     c                   call      'FNLV55R'
113200060522     c                   parm                    fnlv55ds
113300060522     c                   if        d55err = *blanks and d55tfa <> d55lin
113400060522     c                   move      d55tfa        kfgs
113500120612     c     kfidst        chain     fidst01l
113600060522     c                   endif
113700060522     c                   endif
113800120612     c                   If        %Found(fidst01l)
113900040908     c                   Eval      v3cpdr = DstPdr
114000040908     c                   EndIf
114100040908      * decodifico l'autotrasportatore
114200040908     c                   Clear                   v3dpdr
114300040908     c                   Movel     v3cpdr        kpdpdr
114400040908     c     kfapd         Chain     Fiapd01L
114500040908     c                   If        %Found(Fiapd01l)
114600040908     c                   Eval      v3dpdr = ApdRsc
114700040908     c                   EndIf
114800041013   x1c                   Else
114900041013     c                   Eval      *In10 = *Off
115000041013e   1c                   EndIf
115100001031
115200001031     C                   add       1             nrrf
115300001031     C                   write     fior07sf
115400001031
115500001031     C                   endsr
115600001030      **********************************************************************
115700131120      * F15 - Variazioni
115800001030      **********************************************************************
115900001102     C     Sr_Variazioni BEGSR
116000001030
116100001031     C                   z-add     1             recsf
116200001030      * Pulisco subfile
116300001030     C                   z-add     *zeros        nrr               4 0
116400001030     C                   eval      *in21 = *off
116500001030     C                   write     fior07c
116600001030     C                   eval      *in20 = *on
116700001030     C                   eval      *in21 = *on
116800001030
116900001030      * Carico il subfile in base alle scelte fatte
117000001030     C     kfnor         setll     fnorv01l
117100001030     C                   do        *hival
117200001030     C     kfnor         reade     fnorv01l
117300001030     C                   if        %eof
117400001030     C                   leave
117500001030     C                   endif
117600001030
117700001031     C                   z-add     v1cpoe        v2cpoe
117800001031     C                   z-add     v1nsr         v2nsr
117900001031     C                   z-add     v1nor         v2nor
118000001031     C                   z-add     v1nrv         v2nrv
118100001030
118200001102     C                   exsr      Sr_CarVariaz
118300001030     C                   enddo
118400001030      * Se non ho caricato emetto una videata di segnalazione.
118500001031     C                   if        nrr = *zeros
118600001031     C                   movel     msg(1)        v1cmsg
118700001031     C                   seton                                        28
118800001102     C                   goto      endvariaz
118900001031     C                   endif
119000001030
119100001030      * Se ho caricato emetto il subfile
119200001030     C                   if        nrr > *zeros
119300001031     C                   do        *hival
119400001030     C                   write     fior072
119500001030     C                   exfmt     fior07c
119600001030      * F12=Ritorno
119700001102     C   kl              goto      endvariaz
119800001030
119900001031      * Legge i record scelti
120000001030     C                   do        *hival
120100001030     C                   readc     fior07s
120200001030     C                   if        %eof
120300001030     C                   leave
120400001030     C                   endif
120500001030     C                   if        v2scel = '5'
120600001030     C                   z-add     nrr           recsf
120700001102     C                   exsr      Sr_VisVariaz
120800001030     C                   endif
120900001031     C                   clear                   v2scel
121000001030     C                   update    fior07s
121100001030     C                   enddo
121200001030
121300001030     C                   enddo
121400001031     C                   endif
121500001030
121600001102     C     endvariaz     endsr
121700001030      **********************************************************************
121800001102      * CARICO SUBFILE VARIAZIONI
121900001030      **********************************************************************
122000001102     C     Sr_CarVariaz  begsr
122100001030
122200001030     C                   clear                   v2scel
122300001030     C                   eval      v2cpor = ORVpor
122400001030
122500001030      * Inversione data modifica
122600001103     C                   z-add     ORVdtv        v2dtvd
122700001030     C                   z-add     ORVdtv        G02inv
122800001030     C                   movel     '3'           G02err
122900001030     C                   call      'XSRDA8'
123000001030     C                   parm                    wlbdat
123100001030     C                   movel     G02dat        v2dtv
123200001030
123300001030     C                   movel     ORVorv        v2orv
123400001030     C                   eval      v2utv  = ORVutv
123500001031     C                   eval      v2fao  = ORVfao
123600001031      * Decodifica
123700001031     C                   clear                   TIBS02DS
123800001031     C                   movel     'C'           T02mod
123900001031     C                   movel     knsif         t02sif
124000001031     C                   movel     'FAR'         t02cod
124100001031     C                   movel(p)  v2fao         T02ke1
124200001031     C                   call      'TIBS02R'
124300001031     C                   parm                    KPJBA
124400001031     C                   parm                    TIBS02DS
124500001031     C                   movel     t02uni        v2dfao
124600001030
124700001030     C                   add       1             nrr
124800001030     C                   write     fior07s
124900001030
125000001030     C                   endsr
125100001030      **********************************************************************
125200001102      * VISUALIZZO DATI DEL FILE VARIAZIONI
125300001030      **********************************************************************
125400001102     C     Sr_VisVariaz  begsr
125500001031
125600001031     C                   seton                                        01
125700001030
125800001031     C                   eval      v1mod1 = 'Var.il '
125900001031     C                             + v2dtvg
126000001031     C                             +'/'
126100001031     C                             + v2dtvm
126200001031     C                             + '/'
126300001031     C                             + v2dtva
126400001031     C                             + ' alle '
126500001031     C                             + v2orvo
126600001031     C                             + ':'
126700001031     C                             + v2orvm
126800001031     C                             + ':'
126900001031     C                             + v2orvs
127000001031     C                             + ' da '
127100001031     C                             + v2utv
127200001031
127300001103     C                   eval      kdtv = v2dtvd
127400001103     C                   eval      korv = v2orv
127500001103     C     kforp         chain     fnorv01l
127600001103     C                   if        %found(fnorv01l)
127700001103
127800001102     C                   exsr      Sr_CarVidVar
127900001031     C                   exsr      Sr_Decodifica
128000120612     c                   ExSr      Sr_Tastifun
128100001103     C                   endif
128200001030
128300001102     C                   do        *hival
128400001031     C                   exfmt     fior071
128500001102
128600001102      * F12=Ritorno
128700001102     C   kl              leave
128800131029      /free
128900131029       //?F4=Dati Avviso Ritiro
129000131029       IF  *inkd;
129100131029         exsr F04D01;
129200131029         iter;
129300131029       ENDIF;
129400131029
129500131029       //?F5=Altri dati
129600131029       IF  *inke;
129700131029         exsr F05D01;
129800131029         iter;
129900131029       ENDIF;
130000131029      /end-free
130100001102
130200001102     C                   enddo
130300001031
130400001030     C                   endsr
130500001031      **********************************************************************
130600001102      * CARICA DATI NELLA VIDEATA DAL FILE VARIAZIONI
130700001031      **********************************************************************
130800001102     C     Sr_CarVidVar  begsr
130900001031
131000001129     C                   movel     ORMtco        v1ctco
131100001031     C                   movel     ORVtor        v1ctor
131200001031      * Inversione data emissione ORM
131300001031     C                   clear                   wlbdat
131400001031     C                   z-add     ORMdao        G02inv
131500001031     C                   movel     '3'           G02err
131600001031     C                   call      'XSRDA8'
131700001031     C                   parm                    wlbdat
131800001031     C                   z-add     G02dat        v1dao
131900010109     C                   movel     ORMoao        v1oao
132000001031      * Dati Ordinante
132100001031     C                   z-add     ORVcor        v1ccor
132200001031     C                   movel     ORVrso        v1rso
132300001031     C                   movel     ORVino        v1ino
132400001031     C                   movel     ORVcao        v1cao
132500001031     C                   movel     ORVloo        v1loo
132600001031     C                   movel     ORVpro        v1pro
132700001031     C                   movel     ORVnao        v1nao
132800001031      * Dati Ritiro a
132900001031     C                   z-add     ORVcra        v1ccra
133000001031     C                   movel     ORVrsr        v1rsr
133100001031     C                   movel     ORVinr        v1inr
133200001031     C                   movel     ORVcar        v1car
133300001031     C                   movel     ORVlor        v1lor
133400001031     C                   movel     ORVprr        v1prr
133500001031     C                   movel     ORVnar        v1nar
133600001031     C                   movel     ORVrer        v1rer
133700001031     C                   movel     ORVter        v1ter
133800001031      * Dati Consegna a
133900001031     C                   z-add     ORVcrc        v1ccrc
134000001031     C                   movel     ORVrsc        v1rsc
134100001031     C                   movel     ORVinc        v1inc
134200001031     C                   movel     ORVloc        v1loc
134300001031     C                   movel     ORVcac        v1cac
134400001031     C                   movel     ORVprc        v1prc
134500001031     C                   movel     ORVnac        v1nac
134600001031
134700010315     C                   if        ORVffd = 'S'
134800010315     C                   eval      v1ffd = 'SI'
134900010315     C                   else
135000010315     C                   eval      v1ffd = 'NO'
135100010315     C                   endif
135200010315
135300001031     C                   movel     ORVnam        v1nam
135400001031      * Inversione data ritiro
135500001031     C                   z-add     ORVdar        G02inv
135600001031     C                   movel     '3'           G02err
135700001031     C                   call      'XSRDA8'
135800001031     C                   parm                    wlbdat
135900001031     C                   z-add     G02dat        v1dar
136000001031     C                   movel     ORVorr        v1orr
136100001031
136200001031     C                   z-add     ORVncl        v1ncl
136300001031     C                   z-add     ORVpkg        v1pkg
136400001031     C                   z-add     ORVvlm        v1vlm
136500001031     C                   z-add     ORVbnc        v1bnc
136600001031     C                   z-add     ORVblc        v1blc
136700001031     C                   z-add     ORVatt        v1att
136800001031     C                   z-add     ORVmtc        v1mtc
136900001031
137000010315     C                   if        ORVpag = 'M'
137100010315     C                   eval      v1pag = 'Mitt.'
137200010315     C                   endif
137300010315     C                   if        ORVpag = 'D'
137400010315     C                   eval      v1pag = 'Dest.'
137500010315     C                   endif
137600010315     C                   if        ORVpag = 'O'
137700010315     C                   eval      v1pag = 'Ord. '
137800010315     C                   endif
137900010315
138000001031     C                   z-add     ORVksc        v1ksc
138100001031     C                   movel     ORVctr        v1ctr
138200010315
138300010315     C                   if        ORVddt = 'S'
138400010315     C                   eval      v1ddt = 'SI'
138500010315     C                   endif
138600010315     C                   if        ORVddt = 'N'
138700010315     C                   eval      v1ddt = 'NO'
138800010315     C                   endif
138900010315
139000001031     C                   movel     ORVpor        v1cpor
139100001031     C                   movel     ORVpoc        v1cpoc
139200001031     C                   movel     ORVno1        v1not1
139300001031     C                   movel     ORVno2        v1not2
139400001031     C                   z-add     ORVfao        v1fao
139500001031     C                   z-add     ORMpos        v1cpos
139600001031     C                   z-add     ORMors        v1ors
139700001031     C                   movel     ORVrfa        v1rfa
139800001221     C                   move      ORVsto        v1sto
139900001031     C                   z-add     ORVndc        v1ndc
140000001031      * Inversione data distinta
140100001031     C                   clear                   wlbdat
140200001031     C                   z-add     ORVddc        G02inv
140300001031     C                   movel     '3'           G02err
140400001031     C                   call      'XSRDA8'
140500001031     C                   parm                    wlbdat
140600001031     C                   z-add     G02dat        v1ddc
140700010226
140800010226      * Cerca codice autotrasportatore
140900071129     c                   clear                   v1cpdc
141000001031     C                   z-add     ORVndc        knfv
141100001031     C                   z-add     ORVpor        kfgs
141200120612     C     kfidst        chain     fidst01l
141300120612     C                   if        %found(fidst01l) and DSTatb = *blanks
141400001031     C                   eval      v1cpdc = DSTpdr
141500001031     C                   endif
141600010119
141700010315     C                   if        ORVspi = 'S'
141800010315     C                   eval      v1spi = 'SI'
141900010315     C                   else
142000010315     C                   eval      v1spi = 'NO'
142100010315     C                   endif
142200030627
142300030627     C                   eval      dorm01 = ORvflo
142400030627
142500030627      * ORM commissionato
142600030627     c                   If        §orCom = 'S'
142700030627     c                   Eval      V1dCom = 'COMMISSIONATO'
142800030627     c                   Else
142900030627     c                   Clear                   V1dCom
143000030627     c                   EndIf
143100131211
143200131211     c                   eval      *in45 = *off
143300131211     c                   eval      *in46 = *off
143400131029
143500131029      /free
143600131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
143700131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
143800131211           *in46 = *on;
143900131211         ENDIF;
144000131029       //?Recupero dati da estensione FNORVE
144100131029       //?Rcd 'O ' --> Orari apertura
144200131029         k_ORVEpoe = ORVpoe;
144300131029         k_ORVEnsr = ORVnsr;
144400131029         k_ORVEnor = ORVnor;
144500131029         k_ORVEnrv = ORVnrv;
144600131029         k_ORVEdtv = ORVdtv;
144700131029         k_ORVEorv = ORVorv;
144800131029         k_ORVEtrc = 'O ';
144900131029         clear dOREorari;
145000131029       //?Aggancio il rcd
145100131029         chain(n)  %kds(k07fnorve) FNORVE1L;
145200131029         IF  %found(FNORVE1L);
145300131029           dOREorari = ORVEdati;
145400131029         ENDIF;
145500131029         v1OraAMda = §OREoramda;
145600131029         v1OraAMa  = §OREorama;
145700131029         v1OraPMda = §OREorapda;
145800131029         v1OraPMa  = §OREorapa;
145900131029       //?Rcd 'S ' --> SMS per invio alert
146000131029         k_ORVEtrc = 'S ';
146100160311         clear dOREsms;
146200131029         clear w03sms;
146300131029       //?Aggancio il rcd
146400131029         chain(n)  %kds(k07fnorve) FNORVE1L;
146500131029         IF  %found(FNORVE1L);
146600160311           dOREsms = ORVEdati;
146700131029         ENDIF;
146800160311         w03sms = §OREsms;
146900131029       //?Rcd 'MA' --> Mail per invio alert
147000131029         k_ORVEtrc = 'MA';
147100131029         clear w03mail;
147200131029       //?Aggancio il rcd
147300131029         chain(n)  %kds(k07fnorve) FNORVE1L;
147400131029         IF  %found(FNORVE1L);
147500131029           w03mail = ORVEdati;
147600131029         ENDIF;
147700160311
147800160407         IF  W03mail <> *blanks or W03sms <> *blanks;
147900131211           *in45 = *on;
148000131211         ENDIF;
148100131029      /end-free
148200001031
148300001031     C                   endsr
148400001025      **********************************************************************
148500001025      * F18 - Note
148600001025      **********************************************************************
148700001025     C     Sr_Note       BEGSR
148800001025
148900001025     C                   clear                   fior06DS
149000001025     C                   z-add     v1dao         i06dta
149100001026     C                   z-add     v1cpoe        i06poe
149200001026     C                   z-add     v1nor         i06nor
149300001026     C                   z-add     v1nsr         i06nsr
149400001026     C                   z-add     v1nrv         i06nrv
149500001026     C                   movel     v1fao         i06far
149600001026     C                   eval      i06dai = §doggi
149700001026     C                   TIME                    W0140
149800001026     C                   movel     w0140         I06ori
149900001026     C                   movel     'I'           i06flm
150000061017     c   ki              movel     'V'           i06flm
150100001026
150200001025     C                   CALL      'FIOR06R'
150300001025     C                   PARM                    kpjba
150400001027     C                   parm                    fior06ds
150500001025
150600001025     C                   endsr
150700001031      **********************************************************************
150800001031      * F19 - Proposte
150900001031      **********************************************************************
151000001031     C     Sr_Proposte   BEGSR
151100001031
151200001031     C                   z-add     1             recsfp
151300001031      * Pulisco subfile
151400001031     C                   z-add     *zeros        nrrp              4 0
151500001031     C                   eval      *in21 = *off
151600001031     C                   write     fior07cp
151700001031     C                   eval      *in20 = *on
151800001031     C                   eval      *in21 = *on
151900001031
152000001031      * Carico il subfile in base alle scelte fatte
152100001031     C     kfnor         setll     fnorp01l
152200001031     C                   do        *hival
152300001031     C     kfnor         reade     fnorp01l
152400001031     C                   if        %eof
152500001031     C                   leave
152600001031     C                   endif
152700001031
152800001031     C                   z-add     v1cpoe        v4cpoe
152900001031     C                   z-add     v1nsr         v4nsr
153000001031     C                   z-add     v1nor         v4nor
153100001031     C                   z-add     v1nrv         v4nrv
153200001031
153300001031     C                   exsr      Sr_CarProposte
153400001031     C                   enddo
153500001031      * Se non ho caricato emetto una videata di segnalazione.
153600001106     C                   if        nrrp = *zeros and psce <> 'P'
153700001103     C                   movel     msg(3)        v1cmsg
153800001031     C                   seton                                        28
153900001031     C                   goto      endproposte
154000001031     C                   endif
154100001103
154200001103     C                   if        nrrp = *zeros and psce = 'P'
154300001103     C                   do        *hival
154400001103     C                   exfmt     fior075
154500001103      * F12=Ritorno
154600001103     C   kl              leave
154700001103     C                   enddo
154800001103     C                   endif
154900001031
155000001031      * Se ho caricato emetto il subfile
155100001031     C                   if        nrrp > *zeros
155200001031     C                   do        *hival
155300001031     C                   write     fior074
155400001031     C                   exfmt     fior07cp
155500001031      * F12=Ritorno
155600001031     C   kl              goto      endproposte
155700001031
155800001031      * Legge i record scelti
155900001031     C                   do        *hival
156000001031     C                   readc     fior07sp
156100001031     C                   if        %eof
156200001031     C                   leave
156300001031     C                   endif
156400001031     C                   if        v4scel = '5'
156500001031     C                   z-add     nrrp          recsfp
156600001031     C                   exsr      Sr_VisProposte
156700001031     C                   endif
156800001031     C                   clear                   v4scel
156900001031     C                   update    fior07sp
157000001031     C                   enddo
157100001031
157200001031     C                   enddo
157300001031     C                   endif
157400001031
157500001031     C     endproposte   endsr
157600001031      **********************************************************************
157700001031      * CARICO SUBFILE PROPOSTE
157800001031      **********************************************************************
157900001031     C     Sr_CarPropostebegsr
158000001031
158100001031     C                   clear                   v4scel
158200001031     C                   eval      v4cpor = ORPpoe
158300001031
158400001031      * Inversione data modifica
158500001103     C                   z-add     ORPdtv        v4dtvd
158600001031     C                   z-add     ORPdtv        G02inv
158700001031     C                   movel     '3'           G02err
158800001031     C                   call      'XSRDA8'
158900001031     C                   parm                    wlbdat
159000001031     C                   movel     G02dat        v4dtv
159100001031
159200001031     C                   movel     ORPorv        v4orv
159300001031     C                   eval      v4utv  = ORPutv
159400001031     C                   eval      v4fao  = ORPfao
159500001031      * Decodifica
159600001031     C                   clear                   TIBS02DS
159700001031     C                   movel     'C'           T02mod
159800001031     C                   movel     knsif         t02sif
159900001031     C                   movel     'FAR'         t02cod
160000001031     C                   movel(p)  v4fao         T02ke1
160100001031     C                   call      'TIBS02R'
160200001031     C                   parm                    KPJBA
160300001031     C                   parm                    TIBS02DS
160400001031     C                   movel     t02uni        v4dfao
160500001031      * Analisi Proposta
160600001031     C                   if        ORPfev <> *blanks
160700001031     C                   seton                                        03
160800001031     C                   if        ORPfev = 'E'
160900001031     C                   eval      v4sta = 'EFFETTUATA'
161000001107     C                   eval      v4sta1= 'EFF'
161100001031     C                   endif
161200001031     C                   if        ORPfev = 'R'
161300001031     C                   eval      v4sta = 'RIFIUTATA'
161400001107     C                   eval      v4sta1= 'RIF'
161500001031     C                   endif
161600001031      * Inversione data analisi proposta
161700001031     C                   z-add     ORPdte        G02inv
161800001031     C                   movel     '3'           G02err
161900001031     C                   call      'XSRDA8'
162000001031     C                   parm                    wlbdat
162100001031     C                   movel     G02dat        v4dte
162200001031     C                   movel     ORPore        v4ore
162300001031     C                   eval      v4ute  = ORPute
162400001107     C                   else
162500001107     C                   setoff                                       03
162600001107     C                   clear                   v4sta
162700001107     C                   clear                   v4sta1
162800001107     C                   clear                   v4dte
162900001107     C                   clear                   v4ore
163000001107     C                   clear                   v4ute
163100001031     C                   endif
163200001031
163300001031     C                   add       1             nrrp
163400001031     C                   write     fior07sp
163500001031
163600001031     C                   endsr
163700001031      **********************************************************************
163800001031      * VISUALIZZO DATI DEL FILE PROPOSTE
163900001031      **********************************************************************
164000001031     C     Sr_VisPropostebegsr
164100001031
164200140303     c                   eval      *in15 = *on
164300001031
164400001031     C                   eval      v1mod1 = 'Prop. del '
164500001031     C                             + v4dtvg
164600001031     C                             +'/'
164700001031     C                             + v4dtvm
164800001031     C                             + '/'
164900001031     C                             + v4dtva
165000001031     C                             + ' alle '
165100001031     C                             + v4orvo
165200001031     C                             + ':'
165300001031     C                             + v4orvm
165400001031     C                             + ':'
165500001031     C                             + v4orvs
165600001031     C                             + ' da '
165700001031     C                             + v4utv
165800001031
165900001103     C                   eval      kdtv = v4dtvd
166000001103     C                   eval      korv = v4orv
166100001103     C     kforp         chain     fnorp01l
166200001103     C                   if        %found(fnorp01l)
166300001103
166400001031     C                   exsr      Sr_CarVidPro
166500001031     C                   exsr      Sr_Decodifica
166600120612     c                   ExSr      Sr_Tastifun
166700001103     C                   endif
166800001031
166900001102     C                   do        *hival
167000001031     C                   exfmt     fior071
167100001102
167200001102      * F12=Ritorno
167300001102     C   kl              leave
167400131029      /free
167500131029       //?F4=Dati Avviso Ritiro
167600131029       IF  *inkd;
167700131029         exsr F04D01;
167800131029         iter;
167900131029       ENDIF;
168000131029
168100131029       //?F5=Altri dati
168200131029       IF  *inke;
168300131029         exsr F05D01;
168400131029         iter;
168500131029       ENDIF;
168600140303
168700140303       //?F17=Note AUT
168800140303       IF  *inkr;
168900140303         exsr F17D01;
169000140303         iter;
169100140303       ENDIF;
169200131029      /end-free
169300001102
169400001102     C                   enddo
169500001031
169600001031     C                   endsr
169700001031      **********************************************************************
169800001031      * CARICA DATI NELLA VIDEATA DAL FILE PROPOSTE
169900001031      **********************************************************************
170000001031     C     Sr_CarVidPro  begsr
170100001031
170200001129     C                   movel     ORMtco        v1ctco
170300001031     C                   movel     ORPtor        v1ctor
170400001031      * Inversione data emissione ORM
170500001031     C                   clear                   wlbdat
170600001031     C                   z-add     ORMdao        G02inv
170700001031     C                   movel     '3'           G02err
170800001031     C                   call      'XSRDA8'
170900001031     C                   parm                    wlbdat
171000001031     C                   z-add     G02dat        v1dao
171100010109     C                   movel     ORMoao        v1oao
171200001031      * Dati Ordinante
171300001031     C                   z-add     ORPcor        v1ccor
171400001031     C                   movel     ORPrso        v1rso
171500001031     C                   movel     ORPino        v1ino
171600001031     C                   movel     ORPcao        v1cao
171700001031     C                   movel     ORPloo        v1loo
171800001031     C                   movel     ORPpro        v1pro
171900001031     C                   movel     ORPnao        v1nao
172000001031      * Dati Ritiro a
172100001031     C                   z-add     ORPcra        v1ccra
172200001031     C                   movel     ORPrsr        v1rsr
172300001031     C                   movel     ORPinr        v1inr
172400001031     C                   movel     ORPcar        v1car
172500001031     C                   movel     ORPlor        v1lor
172600001031     C                   movel     ORPprr        v1prr
172700001031     C                   movel     ORPnar        v1nar
172800001031     C                   movel     ORPrer        v1rer
172900001031     C                   movel     ORPter        v1ter
173000001031      * Dati Consegna a
173100001031     C                   z-add     ORPcrc        v1ccrc
173200001031     C                   movel     ORPrsc        v1rsc
173300001031     C                   movel     ORPinc        v1inc
173400001031     C                   movel     ORPloc        v1loc
173500001031     C                   movel     ORPcac        v1cac
173600001031     C                   movel     ORPprc        v1prc
173700001031     C                   movel     ORPnac        v1nac
173800001031
173900010315     C                   if        ORPffd = 'S'
174000010315     C                   eval      v1ffd = 'SI'
174100010315     C                   else
174200010315     C                   eval      v1ffd = 'NO'
174300010315     C                   endif
174400010315
174500001031     C                   movel     ORPnam        v1nam
174600001031      * Inversione data ritiro
174700001031     C                   z-add     ORPdar        G02inv
174800001031     C                   movel     '3'           G02err
174900001031     C                   call      'XSRDA8'
175000001031     C                   parm                    wlbdat
175100001031     C                   z-add     G02dat        v1dar
175200001031     C                   movel     ORPorr        v1orr
175300001031
175400001031     C                   z-add     ORPncl        v1ncl
175500001031     C                   z-add     ORPpkg        v1pkg
175600001031     C                   z-add     ORPvlm        v1vlm
175700001031     C                   z-add     ORPbnc        v1bnc
175800001031     C                   z-add     ORPblc        v1blc
175900001031     C                   z-add     ORPatt        v1att
176000001031     C                   z-add     ORPmtc        v1mtc
176100010315
176200010315     C                   if        ORPpag = 'M'
176300010315     C                   eval      v1pag = 'Mitt.'
176400010315     C                   endif
176500010315     C                   if        ORPpag = 'D'
176600010315     C                   eval      v1pag = 'Dest.'
176700010315     C                   endif
176800010315     C                   if        ORPpag = 'O'
176900010315     C                   eval      v1pag = 'Ord. '
177000010315     C                   endif
177100001031
177200001031     C                   z-add     ORPksc        v1ksc
177300001031     C                   movel     ORPctr        v1ctr
177400010315
177500010315     C                   if        ORPddt = 'S'
177600010315     C                   eval      v1ddt = 'SI'
177700010315     C                   endif
177800010315     C                   if        ORPddt = 'N'
177900010315     C                   eval      v1ddt = 'NO'
178000010315     C                   endif
178100010315
178200001031     C                   movel     ORPpor        v1cpor
178300001031     C                   movel     ORPpoc        v1cpoc
178400001031     C                   movel     ORPno1        v1not1
178500001031     C                   movel     ORPno2        v1not2
178600001031     C                   z-add     ORPfao        v1fao
178700001031     C                   z-add     ORMpos        v1cpos
178800001031     C                   z-add     ORMors        v1ors
178900001031     C                   movel     ORPrfa        v1rfa
179000001221     C                   move      ORPsto        v1sto
179100001031     C                   z-add     ORPndc        v1ndc
179200001031      * Inversione data distinta
179300001031     C                   clear                   wlbdat
179400001031     C                   z-add     ORPddc        G02inv
179500001031     C                   movel     '3'           G02err
179600001031     C                   call      'XSRDA8'
179700001031     C                   parm                    wlbdat
179800001031     C                   z-add     G02dat        v1ddc
179900010226
180000010226      * Cerca codice autotrasportatore
180100071129     c                   clear                   v1cpdc
180200001031     C                   z-add     ORPndc        knfv
180300001031     C                   z-add     ORPpor        kfgs
180400120612     C     kfidst        chain     fidst01l
180500120612     C                   if        %found(fidst01l) and DSTatb = *blanks
180600001031     C                   eval      v1cpdc = DSTpdr
180700001031     C                   endif
180800010226
180900010315     C                   if        ORPspi = 'S'
181000010315     C                   eval      v1spi = 'SI'
181100010315     C                   else
181200010315     C                   eval      v1spi = 'NO'
181300010315     C                   endif
181400030627
181500030627     C                   eval      dorm01 = ORpflo
181600030627
181700030627      * ORM commissionato
181800030627     c                   If        §orCom = 'S'
181900030627     c                   Eval      V1dCom = 'COMMISSIONATO'
182000030627     c                   Else
182100030627     c                   Clear                   V1dCom
182200030627     c                   EndIf
182300131211
182400131211     c                   eval      *in45 = *off
182500131211     c                   eval      *in46 = *off
182600140303     c                   eval      *in48 = *off
182700131029
182800131029      /free
182900131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
183000131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
183100131211           *in46 = *on;
183200131211         ENDIF;
183300131029       //?Recupero dati da estensione FNORPE
183400131029       //?Rcd 'O ' --> Orari apertura
183500131029         k_ORPEpoe = ORPpoe;
183600131029         k_ORPEnsr = ORPnsr;
183700131029         k_ORPEnor = ORPnor;
183800131029         k_ORPEnrv = ORPnrv;
183900131029         k_ORPEdtv = ORPdtv;
184000131029         k_ORPEorv = ORPorv;
184100131029         k_ORPEtrc = 'O ';
184200131029         clear dOREorari;
184300131029       //?Aggancio il rcd
184400131029         chain(n)  %kds(k07fnorpe) FNORPE1L;
184500131029         IF  %found(FNORPE1L);
184600131029           dOREorari = ORPEdati;
184700131029         ENDIF;
184800131029         v1OraAMda = §OREoramda;
184900131029         v1OraAMa  = §OREorama;
185000131029         v1OraPMda = §OREorapda;
185100131029         v1OraPMa  = §OREorapa;
185200131029       //?Rcd 'S ' --> SMS per invio alert
185300131029         k_ORPEtrc = 'S ';
185400160311         clear dOREsms;
185500131029         clear W03sms;
185600131029       //?Aggancio il rcd
185700131029         chain(n)  %kds(k07fnorpe) FNORPE1L;
185800131029         IF  %found(FNORPE1L);
185900160311           dOREsms = ORPEdati;
186000131029         ENDIF;
186100160311         w03sms = §OREsms;
186200131029       //?Rcd 'MA' --> Mail per invio alert
186300131029         k_ORPEtrc = 'MA';
186400131029         clear W03mail;
186500131029       //?Aggancio il rcd
186600131029         chain(n)  %kds(k07fnorpe) FNORPE1L;
186700131029         IF  %found(FNORPE1L);
186800131029           w03mail = ORPEdati;
186900131029         ENDIF;
187000160311
187100160407         IF  W03mail <> *blanks or W03sms <> *blanks;
187200131211           *in45 = *on;
187300131211         ENDIF;
187400140303
187500140303       //?Controllo se ci sono proposte Note AUT
187600140303         chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
187700140303         IF  %found(FNORPT1L);
187800140303           *in48 = *on;
187900140303         ENDIF;
188000131029      /end-free
188100001031
188200001031     C                   endsr
188300010109      **********************************************************************
188400010109      * Carico dati dalla kpjba per programma richiamato da interrogazione
188500010109      * proposte
188600010109      **********************************************************************
188700010109     C     Sr_Cardatipropbegsr
188800010109
188900010109     C                   z-add     ppoe          v4cpoe
189000010109     C                   z-add     pnsr          v4nsr
189100010109     C                   z-add     pnor          v4nor
189200010109     C                   z-add     pnrv          v4nrv
189300010109     C                   z-add     pdtv          v4dtv
189400010109     C                   z-add     porv          v4orv
189500010109      * Inverto la data
189600010109     C                   z-add     v4dtv         G02DAT
189700010109     C                   MOVEL     *BLANK        G02ERR
189800010109     C                   CALL      'XSRDA8'
189900010109     C                   PARM                    WLBDAT
190000010109     C                   Z-ADD     G02INV        v4dtvd
190100010109
190200010109     C                   movel     putv          v4utv
190300010109
190400010109     C                   endsr
190500061017      *--------------------------------------------------------------------*
190600061017      * F9 - Sollecito
190700061017      *--------------------------------------------------------------------*
190800061017     c     sr_sollecito  begsr
190900061017
191000061017      * il sollecito è possibile se l'ORM è automatizzato
191100061017      * p.o. emissione DPD
191200061017     c     v1cpoe        lookup    skpodpd                                30
191300061017     c                   if        *in30 and v1ctco <> 'F'
191400061017     c                   eval      *in28 = *on
191500061017     c                   eval      v1cmsg = msg(05)
191600061017     c                   goto      for01
191700061017     c                   endif
191800061017      * p.o. ritiro DPD
191900061017     c     v1cpor        lookup    skpodpd                                30
192000061017     c                   if        *in30 and v1cfcs <> 'F'
192100061017     c                   eval      *in28 = *on
192200061017     c                   eval      v1cmsg = msg(05)
192300061017     c                   goto      for01
192400061017     c                   endif
192500061017      * se orm in fase 900 o 999 non posso sollecitare
192600061017     c                   if        v1fao > 600
192700061017     c                   eval      *in28 = *on
192800061017     c                   eval      v1cmsg = msg(06)
192900061017     c                   goto      for01
193000061017     c                   endif
193100061018      * il sollecito è possibile se l'ORM è già stato stampato
193200080505     c     v1cpor        lookup    skpodpd                                30
193300080505     c                   if        *in30 and v1cfcs = 'F' and
193400080505     c                             v1dst = *zeros
193500061018     c                   eval      *in28 = *on
193600061018     c                   eval      v1cmsg = msg(04)
193700061018     c                   goto      for01
193800061018     c                   endif
193900061017
194000061017      * p.o. ritiro DPD
194100061017     c     v1cpor        lookup    skpodpd                                30
194200061017      * devo richiamare il pgm di stampa per mandare il sollecito
194300061017      * via fax o mail
194400061017     c                   if        *in30
194500090211      * prima di mandare il fax o la mail verifico se il depot è associato ad un cod.cliente
194600090211     c                   exsr      sr_depot
194700090211      * se non associato errore e non si fa il sollecito
194800090211     c                   if        odepksc = *zeros
194900090211     c                   eval      *in28 = *on
195000090211     c                   eval      v1cmsg = msg(07)
195100090217     c                   eval      %subst(v1cmsg:32:4) = osie3ddep
195200090211     c                   goto      for01
195300090211     c                   endif
195400061017     c                   exsr      sr_stampa
195500061017     c                   endif
195600061017
195700061017      * scrivo in automatico la nota per avvenuto sollecito
195800061017     c                   eval      ornpoe = v1cpoe
195900061017     c                   eval      ornnsr = v1nsr
196000061017     c                   eval      ornnor = v1nor
196100061017     c                   eval      ornnrv = v1nrv
196200061017     c                   eval      ornfar = v1fao
196300061017     c                   clear                   wprg
196400061017      * cerco ultimo progressivo relativo alla stessa fase
196500061017     c     kfnorn        setll     fnorn02l
196600061017     c                   do        *hival
196700061017     c     kfnorn        reade     fnorn02l
196800061017     c                   if        %eof(fnorn02l)
196900061017     c                   leave
197000061017     c                   endif
197100061017     c                   eval      wprg = ornprg
197200061017     c                   enddo
197300061017      * scrivo la nota
197400061017     c                   move      data_oggi     ds_data
197500061017     c                   add       1             wprg
197600061017     c                   clear                   fnorn000
197700061017     c                   eval      ornpoe = v1cpoe
197800061017     c                   eval      ornnsr = v1nsr
197900061017     c                   eval      ornnor = v1nor
198000061017     c                   eval      ornnrv = v1nrv
198100061017     c                   eval      ornfar = v1fao
198200061017     c                   eval      orndai = dateu
198300061017     c                   time                    ornori
198400061017     c                   eval      ornprg = wprg
198500061017     c                   eval      ornpue = knmus
198600150309     c                   eval      ornpon = DUTpou
198700061017     c                   eval      ornsns = 'I'
198800061017     c                   eval      ornnot = '** SOLLECITATO IL ' +
198900061017     c                             %editc(ds_giorno:'1') + '/' +
199000061017     c                             %editc(ds_mese:'1') + '/' +
199100061017     c                             %editc(ds_anno:'Z') + ' **'
199200061017     c                   eval      ornftr = 'S'
199300061017     c                   eval      orndtr = dateu
199400061017     c                   write     fnorn000
199500061017
199600061017      * richiamo il pgm di gestione note
199700061017     c                   exsr      sr_note
199800061017
199900061017     c                   endsr
200000090211
200100090211      *--------------------------------------------------------------------*
200200090211      * Controllo il depot
200300090211      *--------------------------------------------------------------------*
200400090211     c     sr_depot      begsr
200500090211
200600090211     c                   clear                   tisie3ds
200700090211     c                   eval      isie3dri = dateu
200800090211     c                   eval      isie3dsp = ormdar
200900090211     c                   time                    isie3hsp
201000090211     c                   eval      isie3nzd = ormnar
201100090211     c                   eval      isie3cad = ormcar
201200090211     c                   if        ormpkg > §3ilmp
201300090211     c                   eval      isie3srv = 101
201400090211     c                   else
201500090211     c                   eval      isie3srv = 136
201600090211     c                   endif
201700090211     c                   eval      isie3lnp = ormpoe
201800090211     c                   call      'TISIE3R'
201900090211     c                   parm                    tisie3ds
202000090211      * con il depot richiamo pgm per avere il codice cliente
202100090211      * cliente
202200090211     c                   clear                   truldepds
202300090211     c                   eval      idepdpc = osie3ddep
202400090211     c                   eval      idepdrf = ormdar
202500090211     c                   call      'TRULDEPR'
202600090211     c                   parm                    truldepds
202700090211
202800090211     c                   endsr
202900061017
203000061017      *--------------------------------------------------------------------*
203100061017      * Richiama il pgm di stampa per sollecito
203200061017      *--------------------------------------------------------------------*
203300061017     c     sr_stampa     begsr
203400061017
203500061017     c                   clear                   fior16ds
203600061017     c                   clear                   trul90ds
203700061017     c                   movel     'S'           d90rso
203800061017     c                   call      'TRUL90R'
203900061017     c                   parm                    kpjba
204000061017     c                   Parm                    trul90ds
204100061017      * F3=Fine
204200061017     c                   if        d90f3 = '1'
204300061017     c                   goto      endsrstampa
204400061017     c                   endif
204500090202      /free
204600090202         reset ds5P;
204700090202         if  %subst(knmus : 1 : 3) = 'EDP';
204800090202           TBLkey = 'O' + 'EDP';
204900090202         else;
205000090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
205100090202         endif;
205200090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
205300090202         if  %found( TABEL00F );
205400090202           ds5P = TBLuni;
205500090202         endif;
205600090202      /end-free
205700061017     c                   movel     d90mdo        r16mdo
205800061017     c                   movel     d90pro        r16oqo
205900061017     c                   eval      r16tla = *blanks
206000061017      * è sempre sollecito imposto il nuovo tipo
206100061017     c                   eval      r16ris ='X'
206200061017     c                   eval      r16poe = v1cpoe
206300061017     c                   eval      r16nsr = v1nsr
206400061017     c                   eval      r16nor = v1nor
206500061017     c                   eval      r16nrv = v1nrv
206600061017     c                   eval      r16por = v1cpor
206700061017     c                   eval      r16dst = ormdar
206800061017     c                   movel     kpjbu         §kpjbu
206900061017     c                   movel(p)  fior16ds      kpjbu
207000090202     c                   call      §5Ppgm
207100061017     c                   parm                    kpjba
207200080318     c                   movel     kpjbu         fior16ds
207300061017     c                   movel     §kpjbu        kpjbu
207400061017
207500061017     c     endsrstampa   endsr
207600061019
207700061019      *--------------------------------------------------------------------*
207800061023      * F2 - Dati DPD
207900061019      *--------------------------------------------------------------------*
208000061019     c     sr_datidpd    begsr
208100080430
208200090202      * Salvataggio KPJBU
208300090202     c                   eval      §kpjbu = kpjbu
208400090202
208500080430     c                   clear                   fieu55ds
208600080422
208700080430      * se ORM commissionato da DPD passo il n. ORM DPD
208800080430     c     ormpoe        lookup    skpodpd                                30
208900080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
209000080430     c                   eval      i55dep = %subst(ormrfa:4:4)
209100080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
209200081015     c                   eval      i55ie = 'I'
209300080430     c                   eval      kpjbu = fieu55ds
209400080430     c                   call      'FIEU55C'
209500080430     c                   parm                    kpjba
209600080430     c                   endif
209700080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
209800080430     c     ormpor        lookup    skpodpd                                30
209900080430     c                   if        *in30
210000061019     c                   eval      i55poe = v1cpoe
210100061019     c                   eval      i55nrs = v1nsr
210200061019     c                   eval      i55nro = v1nor
210300061019     c                   eval      i55nrv = v1nrv
210400081015     c                   eval      i55ie = 'E'
210500061019     c                   eval      kpjbu = fieu55ds
210600061206     c                   call      'FIEU55C'
210700061019     c                   parm                    kpjba
210800080430     c                   endif
210900090202
211000090202      * Ripristino KPJBU
211100090202     c                   eval      kpjbu = §kpjbu
211200061019
211300061019     c                   endsr
211400090211
211500090211      *--------------------------------------------------------------------*
211600090211      * Window per dati appuntamento ORM
211700090211      *--------------------------------------------------------------------*
211800090211     c     sr_ormapp     begsr
211900090223
212000090223      * pulisco il subfile
212100090223     c                   clear                   nrra              4 0
212200090223     c                   eval      *in21 = *off
212300090223     c                   write     fior07apc
212400090223     c                   eval      *in20 = *on
212500090223     c                   eval      *in21 = *on
212600090223
212700090223      * carico il subfile
212800090216      * metto fissa la fase 100, per ora ho l'appuntamento solo su questa fase
212900090225     c                   eval      *in31 = *off
213000090216     c                   eval      kfar = 100
213100090223     c     kfnora        setll     fnora01l
213200090223     c                   do        *hival
213300090223     c     kfnora        reade     fnora01l
213400090223     c                   if        %eof(fnora01l)
213500090225     c                   eval      *in31 = *on
213600090223     c                   leave
213700090223     c                   endif
213800090211     c                   eval      wappref = oraref
213900090211     c                   eval      wappnota1 = %subst(oranote:1:35)
214000090211     c                   eval      wappnota2 = %subst(oranote:36:35)
214100090223     c                   eval      wappora = oraora
214200090223     c                   eval      wappute = oraute
214300090223     c                   eval      g02inv = oradata
214400090223     c                   eval      g02err = '3'
214500090223     c                   call      'XSRDA8'
214600090223     c                   parm                    wlbdat
214700090223     c                   eval      wappdata = g02dat
214800090223     c                   add       1             nrra
214900090223     c                   write     fior07aps
215000090223     c                   enddo
215100090211
215200090223      * se non ho caricato nessun appuntamento emetto una segnalazione
215300090223     c                   if        nrra = *zeros
215400090223     c                   movel     msg(8)        v1cmsg
215500090223     c                   eval      *in28 = *on
215600090223     c                   leavesr
215700090223     c                   endif
215800090223
215900090223      * se ho caricato emetto il subfile
216000090223     c                   if        nrra > *zeros
216100090225     c                   eval      recsfa = 1
216200090223     c                   do        *hival
216300090223     c                   write     fior07apz
216400090223     c                   exfmt     fior07apc
216500090223      * f12-ritorno
216600090223     c                   if        *inkl
216700090223     c                   leave
216800090223     c                   endif
216900090211     c                   enddo
217000090223     c                   endif
217100090211
217200090211     c                   endsr
217300130913
217400130913      /free
217500131028       //--------------------------------------------------------------------
217600131028       //?F4 - Alert.
217700131028       //--------------------------------------------------------------------
217800131028       BEGSR F04D01;
217900131028
218000131028         wEndF04 = *off;
218100131028
218200131028       //?emetto la videata relativa alla MAIL e SMS
218300131028         write fior071;
218400131028       //?emetto la window
218500131028         DOW not wEndF04;
218600131028           exfmt fior0713;
218700131028       //?F12-Ritorno
218800131028           IF  *inkl;
218900131028             wEndF04 = *on;
219000131028           ENDIF;
219100131028         ENDDO;
219200131028
219300131028       ENDSR;
219400131028
219500131028       //--------------------------------------------------------------------
219600131028       //?F5 - Altri dati.
219700131028       //--------------------------------------------------------------------
219800131028       BEGSR F05D01;
219900131028
220000131028         wEndF05 = *off;
220100131028
220200131028       //?emetto la videata relativa agli altri dati
220300131028         write fior071;
220400131028       //?emetto la window dove richiedo gli altri dati
220500131028         DOW not wEndF05;
220600131028           exfmt fior0712;
220700131028       //?F12-Ritorno
220800131028           IF  *inkl;
220900131028             wEndF05 = *on;
221000131028           ENDIF;
221100131028         ENDDO;
221200131028
221300131028       ENDSR;
221400160407
221500160407       //--------------------------------------------------------------------
221600160407       //?F13 - Conferma Prenotazione Ritiro.
221700160407       //--------------------------------------------------------------------
221800160407       BEGSR F13D01;
221900160407
222000160407         wEndF13 = *off;
222100160407
222200160407       //?emetto la videata relativa alla MAIL e SMS
222300160407         write fior071;
222400160407       //?emetto la window
222500160407         DOW not wEndF13;
222600160407           exfmt fior0714;
222700160407       //?F12-Ritorno
222800160407           IF  *inkl;
222900160407             wEndF13 = *on;
223000160407           ENDIF;
223100160407         ENDDO;
223200160407
223300160407       ENDSR;
223400140110
223500130913       //--------------------------------------------------------------------
223600130913       //?F14 - Interrogazione orari di servizio.
223700130913       //--------------------------------------------------------------------
223800130913       BEGSR F14D01;
223900140206
224000140317       //?Devo trovare la pickup per passarla al trulorsr
224100140206         clear TISI95DS;
224200140206         I95tcn = '3';
224300140206         I95cap = ORMcar;
224400140206         I95nar = ORMnar;
224500140206         I95ind = ORMinr;
224600140206         I95loc = ORMlor;
224700140206         I95prv = ORMprr;
224800140206       //?Peso in base al ntwk fil.ritiro
224900140206         IF  network = 'SDI';
225000140206           I95lkg = 1000;
225100140206         ELSE;
225200140206           clear I95lkg;
225300140206         ENDIF;
225400140206         tisi95r (tisi95ds);
225500140317
225600140317       //?Cerco eventuale ora presunta ritiro
225700140318         clear fior01ds;
225800140317         IF  ORMndc > 0 and ORMfao < 600;
225900140318           IOR01fgs = ORMpor;
226000140318           IOR01ndc = ORMndc;
226100140318           IOR01poe = ORMpoe;
226200140318           IOR01nsr = ORMnsr;
226300140318           IOR01nor = ORMnor;
226400140318           IOR01nrv = ORMnrv;
226500140318           fior01r (fior01ds);
226600140317         ENDIF;
226700130913
226800140317         clear trulorsds;
226900130913         IOREfil = ORMpor;
227000130913         IOREcap = ORMcar;
227100130913         IOREloc = ORMlor;
227200130913         IOREdta = ORMdar;
227300130913         IOREtser = 'R';
227400140206         IOREgf2 = O95gf2;
227500140318         IOREorp = OOR01ora;
227600130913
227700140317         trulorsr (kpjba:trulorsds);
227800130913
227900130913       ENDSR;
228000140110
228100140110       //--------------------------------------------------------------------
228200140110       //?F17 - Note AUT.
228300140110       //--------------------------------------------------------------------
228400140110       BEGSR F17D01;
228500140121
228600140121         *in48 = *off;
228700140110
228800140110         clear fiort1ds;
228900140303         IORT1mod = 'G';
229000140110         IORT1poe = ORMpoe;
229100140110         IORT1nsr = ORMnsr;
229200140110         IORT1nor = ORMnor;
229300140110         IORT1nrv = ORMnrv;
229400140110         IORT1pgm = 'FIOR07R';
229500140303         IF  *in15;
229600140303           IORT1mod = 'I';
229700140303           IORT1dim = V4Dtvd;
229800140303           IORT1oim = V4orv;
229900140303         ENDIF;
230000140114         §kpjbu = kpjbu;
230100140110
230200140320         IF  psce = 'P' and *in15;
230300140320           fiort1c (kpjba:fiort1ds);
230400140320         ELSE;
230500140110         fiort1r (kpjba:fiort1ds);
230600140320         ENDIF;
230700140114
230800140114         kpjbu = §kpjbu;
230900140121
231000140121       //?Controllo se sono state immesse Note AUT
231100140304         IF  not *in15;
231200140304           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
231300140304           IF  %found(FNORT01L);
231400140304             *in48 = *on;
231500140304           ENDIF;
231600140304         ENDIF;
231700140304       //?Controllo se ci sono proposte Note AUT
231800140304         IF  *in15;
231900140304           chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
232000140304           IF  %found(FNORPT1L);
232100140304             *in48 = *on;
232200140304           ENDIF;
232300140304         ENDIF;
232400140110
232500140110       ENDSR;
232600130913
232700130913      /end-free
232800130913
232900090210
233000090210      *------------------------------------------------------------------------*
233100090210      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
233200090210      *------------------------------------------------------------------------*
233300090210     c     sr_f24        begsr
233400090210
233500090210     c                   Select
233600090210
233700090210     c                   When      $loop = 2 and $rig =2  or
233800090210     c                             $loop = 3 and $rig =3
233900090210     c                   Movea     rigatf1       vzfd01
234000090210     c                   Z-add     1             $rig
234100090210
234200090210     c                   If        $loop = 2
234300090210     c                   Eval      vzfd02= cf2412
234400090210     c                   Else
234500090210     c                   Eval      vzfd02= cf2413
234600090210     c                   EndIf
234700090210
234800090210     c                   When      $loop = 2 and $rig =1 or
234900090210     c                             $loop = 3 and $rig =1
235000090210     c                   Movea     rigatf2       vzfd01
235100090210     c                   Z-add     2             $rig
235200090210
235300090210     c                   If        $loop = 2
235400090210     c                   Eval      vzfd02 = cf2422
235500090210     c                   Else
235600090210     c                   Eval      vzfd02 = cf2423
235700090210     c                   EndIf
235800090210
235900090210     c                   When      $loop = 3 and $rig =2
236000090210     c                   Movea     rigatf3       vzfd01
236100090210     c                   Z-add     3             $rig
236200090210     c                   Eval      vzfd02 = cf2433
236300090210     c                   EndSl
236400090210
236500090210     c                   EndSr
236600090210
236700090210      *------------------------------------------------------------------------*
236800090210      *  Imposto descrizione tasti funzione
236900090210      *------------------------------------------------------------------------*
237000090210     c     Sr_Tastifun   BegSr
237100140304
237200140304      * Abilito eventuali tasti funzionali
237300140304     c                   exsr      Abil_Fxx
237400090210
237500090210      * Conta i caratteri riempiti dalle RigaTf1
237600090210     c                   Clear                   $z
237700090210      * Conta i caratteri riempiti dalle RigaTf2
237800090210     c                   Clear                   $k
237900090210      * Conta i caratteri riempiti dalle RigaTf3
238000090210     c                   Clear                   $j
238100090210      * Conta la posizione del campone da cui partire per impostare
238200090210      * la descrizione del tasto funzione
238300090210     c                   Clear                   $y
238400090210
238500090210      * Stringhe che contengono le descrizioni dei tasti funzione
238600090210     c                   Reset                   rigatf1
238700090210     c                   Reset                   rigatf2
238800090210     c                   Reset                   rigatf3
238900090217
239000131028      * F1 - Aggiungi Richiesta Assistenza
239100131028     c                   if        *in08
239200131028     c                   reset                   $tf
239300131028     c                   movea     tf01          $tf
239400131028     c                   exsr      rie_$tf
239500131028     c                   endif
239600090217      * F2 - Dati DPD
239700090217     c                   if        not *in01 and *in14 and
239800140304     c                             not *in12 and not *in15
239900090217     c                   reset                   $tf
240000090217     c                   movea     tf02          $tf
240100090217     c                   exsr      rie_$tf
240200090217     c                   endif
240300090217      * F9 - Sollecito
240400090217     c                   if        not *in01 and *in13
240500140304     c                             and not *in15
240600090217     c                   reset                   $tf
240700090217     c                   movea     tf09          $tf
240800090217     c                   exsr      rie_$tf
240900090217     c                   endif
241000160407      * F13 - Conferma Prenotazione
241100160407     c                   if        not *in01
241200160407     c                             and *in49
241300160407     c                   reset                   $tf
241400160407     c                   movea     tf13          $tf
241500160407     c                   exsr      rie_$tf
241600160407     c                   endif
241700130913      * F14 - Orari servizio
241800131108     c                   if        not *in01
241900131125     c                             and *in44
242000140304     c                             and not *in15
242100130913     c                   reset                   $tf
242200130913     c                   movea     tf14          $tf
242300130913     c                   exsr      rie_$tf
242400130913     c                   endif
242500131120      * F15 - Variazioni
242600131120     c                   if        not *in01 and *in40
242700140304     c                             and not *in15
242800131120     c                   reset                   $tf
242900131120     c                   movea     tf15          $tf
243000131120     c                   exsr      rie_$tf
243100131120     c                   endif
243200131028      * F16 - Gestione Richieste assistenza
243300131108     c                   if        not *in01 and *in09
243400140304     c                             and not *in15
243500131028     c                   reset                   $tf
243600131028     c                   movea     tf16          $tf
243700131028     c                   exsr      rie_$tf
243800131028     c                   endif
243900090217      * F19 - Proposte Variazioni
244000090217     c                   if        not *in01 and *in41
244100140304     c                             and not *in15
244200090217     c                   reset                   $tf
244300090217     c                   movea     tf19          $tf
244400090217     c                   exsr      rie_$tf
244500090217     c                   endif
244600090217      * F22 - Info Appuntamento
244700090217     c                   if        not *in01 and *in43
244800140304     c                             and not *in15
244900090217     c                   reset                   $tf
245000090217     c                   movea     tf22          $tf
245100090217     c                   exsr      rie_$tf
245200090217     c                   endif
245300090211      * F20 - Fasi
245400090211     c                   if        not *in01
245500140304     c                             and not *in15
245600090210     c                   reset                   $tf
245700090211     c                   movea     tf20          $tf
245800090210     c                   exsr      rie_$tf
245900090210     c                   endif
246000090210
246100090210      * Pulisco la stringa
246200090210     c                   Eval      $h = 1
246300090210     c                   For       $h by 1 to 62
246400090210     c                   If        rigatf1($h) = '#'
246500090210     c                   Clear                   rigatf1($h)
246600090210     c                   EndIf
246700090210     c                   EndFor
246800090210     c                   Eval      $h = 1
246900090210     c                   For       $h by 1 to 62
247000090210     c                   If        rigatf2($h) = '#'
247100090210     c                   Clear                   rigatf2($h)
247200090210     c                   EndIf
247300090210     c                   EndFor
247400090210     c                   Eval      $h = 1
247500090210     c                   For       $h by 1 to 62
247600090210     c                   If        rigatf3($h) = '#'
247700090210     c                   Clear                   rigatf3($h)
247800090210     c                   EndIf
247900090210     c                   EndFor
248000090210
248100090210      * Imposto la descrizione del tasto funzione F24 e segnalo
248200090210      * quante righe ho riempito e quale devo emettere
248300090210     c                   Select
248400090210
248500090210     c                   When      $k <> *Zeros and $y <> *Zeros
248600090210     c                   Eval      $loop = 3
248700090210     c                   Eval      vzfd02 = cf2413
248800090210
248900090210     c                   When      $k <> *Zeros and $y = *Zeros
249000090210     c                   Eval      $loop = 2
249100090210     c                   Eval      vzfd02 = cf2412
249200090210
249300090210     c                   Other
249400090210
249500090210     c                   Eval      $loop = 1
249600090210     c                   Clear                   vzfd02
249700090210     c                   EndSl
249800090210
249900090210      * Prima riga di tasti funzione
250000090210     c                   Movea     rigatf1       vzfd01
250100090210     c                   Eval      $rig = 1
250200090210
250300090210     c                   EndSr
250400090210
250500090210      *------------------------------------------------------------------------*
250600090210      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
250700090210      *------------------------------------------------------------------------*
250800090210     c     Rie_$tf       BegSr
250900090210      *
251000090210     c                   Eval      $x = 1
251100090210     c     '#'           Lookup    $tf($x)                                30
251200090210     c                   Add       $x            $z
251300090210     c                   If        $Z <= 63
251400090210     c                   Eval      $j = $Z - $x + 1
251500090210     c                   Movea     $tf           rigatf1($j)
251600090210     c                   Else
251700090210     c                   Add       $x            $k
251800090210     c                   If        $k <= 63
251900090210     c                   Eval      $j = $K - $x + 1
252000090210     c                   Movea     $tf           rigatf2($j)
252100090210     c                   Else
252200090210     c                   Add       $x            $y
252300090210     c                   If        $y <= 63
252400090210     c                   Eval      $j = $y - $x + 1
252500090210     c                   Movea     $tf           rigatf3($j)
252600090210     c                   EndIf
252700090210     c                   EndIf
252800090210     c                   EndIf
252900090210
253000090210     c                   EndSr
253100061017
253200001010      *****************************************************************
253300001010      * ROUTINE INIZIALE
253400001010      *****************************************************************
253500001010     C     *INZSR        BEGSR
253600001009
253700001010     C     *ENTRY        PLIST
253800001010     C                   PARM                    KPJBA
253900040220     c                   parm                    FIDNA1ds
254000001011
254100001011     C                   movel     TIT_A         VTCtit
254200001009
254300001026     C                   movel(p)  kpjbu         parm01
254400001016
254500090202     C                   Z-ADD     1             CODUT             1 0
254600090202     c     *dtaara       define    §azute        AZUTEds
254700090202     c     *dtaara       define    §datiute      dDATIUTE
254800090202     c                   in(E)     *dtaara
254900090202     c                   if        %ERROR or RSUT = *blanks
255000090202     c                   clear                   Tibs34Ds
255100090202     c                   call      'TIBS34R'
255200090202     c                   parm                    Tibs34Ds
255300090202     c                   in        *dtaara
255400090202     c                   endif
255500151001
255600151001      * abilitazione utente
255700151001     c                   clear                   wabi
255800151001     c                   eval      dUTE01 = UTEfaf
255900151001     c                   SELECT
256000151001      * carica abilitazioni del profilo
256100151001     c                   WHEN      §UTEorm <> *blanks
256200151001     c                   eval      wabi = §UTEorm
256300151001     c                   WHEN      UTEaut <> *blanks
256400151001     c                   eval      wabi = UTEaut
256500151001      * se non c'è l'abilitazione
256600151001      * --> se 1° livello, abilitazioni al terminal
256700151001      *     se 2° livello, abilitazioni al punto operativo
256800151001      *     se sede no abilitazioni
256900151001     c                   OTHER
257000151001     c                   IF        DUTlpo = '1'
257100151001     c                   eval      wabi   = 'TP'
257200151001     c                   ENDIF
257300151001     c                   IF        DUTlpo = '2'
257400151001     c                   eval      wabi   = 'PO'
257500151001     c                   ENDIF
257600151001     c                   ENDSL
257700151001
257800151001      * Reperimento delle filiali gestibili dall'utente
257900151001     c                   clear                   TRUL31DS
258000151001     c                   eval      I31abi = wabi
258100151001     c                   eval      I31cdi = DUTdis
258200151001     c                   eval      I31car = DUTare
258300151001     c                   eval      I31cpo = DUTpou
258400151001     c                   call      'TRUL31R'
258500151001     c                   parm                    KPJBA
258600151001     c                   parm                    TRUL31DS
258700151001     c                   IF        O31pog > *zeros
258800151001     c                   movea     O31pog        SKpog
258900151001     c                   ENDIF
259000041001
259100050112      * Se ambiente prova FILTRAPRD
259200050112     c                   If        %subst(knsif:7:1) = 'P'
259300050112     c                   Eval      wlib = 'FILTRAPRD '
259400050112     c                   Else
259500050112      * Se ambiente buono FILTRA201
259600050112     c                   Eval      wlib = 'FILTRA201 '
259700041001     c                   EndIf
259800010226
259900150309      * Da sede
260000150309      * apro il file delle distinte dell'ambiente di filiale
260100010226
260200010226     C                   if        simfel = 0
260300010226     C                   eval      *in12 = *on
260400041001     c                   Clear                   comman
260500041001     c                   Movel(p)  cmdt(1)       comman
260600041001     c                   Eval      %Subst(comman:30:10) = wlib
260700041001     c                   Eval      comman =
260800120612     c                             %trim(comman) + '/FIDST01L)'
260900041001     c                   Call      'QCMDEXC'                            99
261000041001     c                   Parm                    comman
261100041001     c                   Parm                    lenght
261200120612     c  n99              Open      fidst01l
261300041001
261400010226     C                   else
261500010226     C                   eval      *in12 = *off
261600120612     C                   open      fidst01l
261700010226     C                   endif
261800061017
261900061206      * carico sk dei p.o. DPD
262000061017     c                   clear                   xx
262100061017     c     *loval        setll     azorg01l
262200061017     c                   do        *hival
262300061017     c                   read      azorg01l
262400061017     c                   if        %eof(azorg01l)
262500061017     c                   leave
262600061017     c                   endif
262700061017     c                   if        orgfva <> *blanks  or
262800061017     c                             (orgfag <> 'F' and orgfag <> 'A')
262900061017     c                   iter
263000061017     c                   endif
263100061017     c                   eval      og143 = orgde3
263200061017     c                   if        §ogntw = 'DPD'
263300061017     c                   add       1             xx
263400061017     c                   eval      skpodpd(xx) = orgfil
263500061017     c                   endif
263600061017     c                   enddo
263700090211
263800090211      * carico tabella DPD
263900090211     c                   clear                   ds3idp
264000090211     c                   movel     '3I'          tblcod
264100090211     c                   movel(p)  'DPD'         tblkey
264200090211     c     ktab          chain     tabel00f
264300090211     c                   if        %found(tabel00f) and tblflg = *blanks
264400090211     c                   eval      ds3idp = tbluni
264500090211     c                   endif
264600001009
264700001023      * reperisce data e ora
264800001010     C                   TIME                    W0140
264900001010      * UDATE IN GGMMAAAA
265000001010     C                   MOVE      W0140         WDTGIO
265100001010      * UDATE IN AAAAMMGG
265200001010     C                   Z-ADD     WDTGIO        G02DAT
265300001010     C                   MOVEL     *BLANK        G02ERR
265400001010     C                   CALL      'XSRDA8'
265500001010     C                   PARM                    WLBDAT
265600001025     C                   Z-ADD     G02INV        DATEU
265700001010     C     *iso          MOVEL     DATEU         DATA_oggi
265800001010
265900001025     C                   z-add     dateu         §doggi
266000001016
266100001009     C     Kfnor         klist
266200001011     C                   kfld                    kpoe
266300001016     C                   kfld                    knsr
266400001009     C                   kfld                    knor
266500001016     C                   kfld                    knrv
266600001103
266700001103     C     Kforp         klist
266800001103     C                   kfld                    kpoe
266900001103     C                   kfld                    knsr
267000001103     C                   kfld                    knor
267100001103     C                   kfld                    knrv
267200001103     C                   kfld                    kdtv
267300001103     C                   kfld                    korv
267400010305
267500010305     C     Kfnorf        klist
267600010305     C                   kfld                    kpoe
267700010305     C                   kfld                    knsr
267800010305     C                   kfld                    knor
267900010305     C                   kfld                    knrv
268000010305     C                   kfld                    kdfo
268100010305     C                   kfld                    kofo
268200010305     C                   kfld                    kfao
268300090216
268400090216     c     kfnora        klist
268500090216     c                   kfld                    kpoe
268600090216     c                   kfld                    knsr
268700090216     c                   kfld                    knor
268800090216     c                   kfld                    knrv
268900090216     c                   kfld                    kfar
269000010119
269100010119     C     Ktab          klist
269200010119     C                   kfld                    codut
269300010119     C                   kfld                    kcod
269400010119     C                   kfld                    kkey
269500001030
269600120612     C     Kfidst        klist
269700120612     c                   kfld                    knpg
269800001030     C                   kfld                    knfv
269900001030     C                   kfld                    kfgs
270000021202
270100021202     C     Kfapd         klist
270200021202     C                   kfld                    kpdtip
270300021202     C                   kfld                    kpdpdr
270400061017
270500061017     c     kfnorn        klist
270600061017     c                   kfld                    ornpoe
270700061017     c                   kfld                    ornnsr
270800061017     c                   kfld                    ornnor
270900061017     c                   kfld                    ornnrv
271000061017     c                   kfld                    ornfar
271100061017
271200150526      * File FITGD32L
271300150526     c     K02TGD32      klist
271400040220     c                   kfld                    TGDtor
271500040220     c                   kfld                    TGDogg
271600001009
271700001011     C                   eval      kpoe = ppoe
271800001016     C                   eval      knsr = pnsr
271900001011     C                   eval      knor = pnor
272000001016     C                   eval      knrv = pnrv
272100001011
272200001025     C                   eval      v1cpoe = ppoe
272300001016     C                   eval      v1nsr  = pnsr
272400001025     C                   eval      v1nor  = pnor
272500001025     C                   eval      v1nrv  = pnrv
272600001115
272700001115     C                   movel     *zeros        v1ccra
272800001115     C                   movel     *zeros        v1ccor
272900001115     C                   movel     *zeros        v1ccrc
273000001006
273100001006     C                   endsr
273200040220      *****************************************************************
273300040220      * Abilitazione tasti funzionali
273400040220      *****************************************************************
273500040220     c     Abil_Fxx      begsr
273600040220
273700140304      * abilito i tasti funzione seguenti solo se non sono in
273800140304      * Interrogazione Storico Variazioni o Interrogazione Proposte Variazione
273900140304     c                   IF        not *in01 and not *in15
274000140304
274100040220      *
274200040220      *?Abilito F1=Aggiunta in Reclami?
274300040220      * (se richiamato da pgm. FIDNA1R...)
274400040303     c                   eval      *in08  =    (SDSprm >  1
274500040309     c                                      and IA1ins = ' ')
274600040220      *
274700040220      *?Abilito F16=Visualizzazione Reclami?
274800040220      * (se rilevo dei reclami già immessi)
274900040220     c                   eval      TGDtor  = 'O'
275000040220     c                   eval      TGDogg  = ds_OggORM
275100150526     c     K02TGD32      setll     FITGD000
275200150526     c                   eval      *in09   = (%equal(FITGD32L)   and
275300040303     c                                           (SDSprm =  1
275400040303     c                                        or (SDSprm >  1
275500040303     c                                        and IA1ins <> 'N')))
275600070509
275700151002      /free
275800151002       //?Abilito F9=Sollecito
275900151002       //?Se non è utente di sede
276000151002       //?se utente INT
276100151002       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
276200151002         IF  %subst(knmus:1:3) = 'INT' and DUTpou <> 046;
276300151002           xx = 1;
276400151002           FOR xx by 1 to %elem(skpog);
276500151002             IF  skpog(xx) <> *zeros and
276600151002                 %lookup(%dec(skpog(xx):3:0):skpoDPD) > 0;
276700151002               *in13 = *on;
276800151002               leave;
276900151002             ENDIF;
277000151002           ENDFOR;
277100151002         ENDIF;
277200151002      /end-free
277300061019
277400061023      *?Abilito F2=Dati DPD
277500080430      * se non provengo già dal pgm fieu55r
277600061024     c                   if        psce <> 'D'
277700061019     c     ormpoe        lookup    skpodpd                                30
277800061019     c  n30ormpor        lookup    skpodpd                                30
277900140304     c                   eval      *in14 = (*in30 and not *in12)
278000061024     c                   endif
278100140304     c                   ENDIF
278200140121
278300140121      /free
278400140121         *in44 = *off;
278500140526       //?F14 - Orari servizio
278600140304       //?Abilito solo se NON sono in
278700140304       //?Interrogazione Storico Variazioni
278800140304       //?Interrogazione Proposte Variazioni
278900140304         IF  not *in01 and not *in15;
279000140317           *in44 = *on;
279100140304         ENDIF;
279200140304
279300140121      /end-free
279400040220
279500040220     c                   EndSR
279600001010      *---------------------------------------------------------------------------------------------
279700041001** cmdt
279800120612OVRDBF FILE(FIDST01L) TOFILE(
279900001031** MSG  Lungh. 78                                                            *
280000001102Non esistono variazioni per l'ORM selezionato                                       1
280100001102Non esistono fasi per l'ORM selezionato                                             2
280200001102Non esistono proposte per l'ORM selezionato                                         3
280300061017L'ORM non è stato stampato impossibile sollecitare                                  4
280400061017Impossibile fare sollecito per ORM non automatizzato                                5
280500061017Impossibile fare sollecito per ORM già fatto o chiuso                               6
280600090217Impossibile sollecitare. Depot xxxx non associato ad un cliente                     7
280700090223Non esistono appuntamenti per l'ORM selezionato                                     8
280800131028** TF01
280900131211F1=Agg.Rich.  #
281000090211** TF02
281100131211F2=Dati DPD  #
281200090211** TF09
281300131211F9=Sollecito  #
281400160407** TF13
281500160407F13=Conf.Prenotazione  #
281600130913** TF14
281700131211F14=OrariSr  #
281800131120** TF15
281900131211F15=Variazioni  #
282000131028** TF16
282100131211F16=Gest.Rich.  #
282200140110** TF19
282300131211F19=Proposte Var.  #
282400090217** TF20
282500131211F20=Fasi  #
282600090217** TF22
282700131211F22=Info App.  #
