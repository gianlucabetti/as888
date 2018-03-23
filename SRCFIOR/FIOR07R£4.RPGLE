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
024501171025     d truldepds1    e ds                  inz
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
027900161108
028000161108      // ds per rcd 'DT' FNORE - Data Pronta Merce
028100161108     d dOREdt        e ds
028200161115
028300161115      // ds per campo IOREflo - TRULORSDS
028400161115     d dIORE01       e ds
028500140318
028600140318      // ds per recupero ora presunta ritiro da PDA
028700140318     d FIOR01DS      e ds
028800151001
028900151001      // Carico filiali gestite dall'utente
029000151001     d TRUL31DS      e ds
029100090202
029200001010      *
029300001010     D                SDS
029400040220     d  SDSprm           *parms
029500001010     D  VTCPGM                 1     10
029600090210
029700090210      *   C O S T A N T I
029800090210     d cf2413          c                   const('F24=AlTasti(1/3)')
029900090210     d cf2423          c                   const('F24=AlTasti(2/3)')
030000090210     d cf2433          c                   const('F24=AlTasti(3/3)')
030100090210     d cf2412          c                   const('F24=AlTasti(1/2)')
030200090210     d cf2422          c                   const('F24=AlTasti(2/2)')
030300140110
030400130913      //---------------------------------------------------------------
030500130913      //?Definizione procedure utilizzate.
030600130913      //---------------------------------------------------------------
030700140110
030800140110      // - Gestione note AUT
030900140110     d fiort1r         pr                  extpgm('FIORT1R')
031000140110     d  kpjba                              likeds(KPJBA)
031100140110     d  fiort1ds                           likeds(FIORT1DS)
031200140320
031300140320      // - Gestione note AUT x proposte
031400140320     d fiort1c         pr                  extpgm('FIORT1C')
031500140320     d  kpjba                              likeds(KPJBA)
031600140320     d  fiort1ds                           likeds(FIORT1DS)
031700130913
031800130913      // -Interrogazione orari di servizio
031900140317     d trulorsr        pr                  extpgm('TRULORSR')
032000130913     d  kpjba                              likeds(KPJBA)
032100140317     d  trulorsds                          likeds(TRULORSDS)
032200140206
032300140206      // - Cappario
032400140206     d tisi95r         pr                  extpgm('TISI95R')
032500140206     d  tisi95ds                           likeds(TISI95DS)
032600140318
032700140318      // - Recupero ora presunta ritiro
032800140318     d fior01r         pr                  extpgm('FIOR01R')
032900140318     d  fior01ds                           likeds(FIOR01DS)
033000131125
033100131125      //---------------------------------------------------------------
033200131125      //?Definizione prototipi utilizzati.
033300131125      //---------------------------------------------------------------
033400131125
033500131125      /copy gaitrasrc/srcprotopr,tibs02r
033600161020      /copy gaitrasrc/srcprotopr,XSRDA8
033700131028
033800131028      //---------------------------------------------------------------
033900131028      //?Definizione key-list.
034000131028      //---------------------------------------------------------------
034100131028
034200131028       // - File FNORE01L
034300131029     d k05fnore      e ds                  extname(FNORE01L:*key)
034400131028     d                                     prefix(k_)
034500131028     d                                     inz
034600131029
034700131029       // - File FNORVE1L
034800131029     d k07fnorve     e ds                  extname(FNORVE1L:*key)
034900131029     d                                     prefix(k_)
035000131029     d                                     inz
035100131029
035200131029       // - File FNORPE1L
035300131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
035400131029     d                                     prefix(k_)
035500131029     d                                     inz
035600001010
035700001006      *------------------------------------------------------------------------*
035800001103
035900001030      * Carica i dati dal file FNORM
036000001030     C     kfnor         chain     fnorm01L
036100001115     C                   if        %found(fnorm01l)
036200001115      * Annullato
036300001115     C                   if        ORMatb <> *blanks
036400001115     C                   movel     'ANNULLATO'   v1ann
036500001115     C                   else
036600001115     C                   clear                   v1ann
036700001115     C                   endif
036800001103      * Se è richiamato con 'P' deve solo gestire le proposte
036900001103     C                   if        psce = 'P'
037000001103     C                   goto      richiama
037100001103     C                   endif
037200010315
037300010109     C                   else
037400010109     C                   if        psce = 'P' and pric = 'P'
037500010109     C                   movel     'E'           psce
037600010109     C                   movel     parm01        kpjbu
037700010109     C                   goto      richiama
037800010109     C                   endif
037900001009     C                   endif
038000040220
038100040220      * Abilito eventuali tasti funzionali
038200140304     c                   exsr      Abil_Fxx
038300001006
038400001006      * Emissione 1° Videata
038500001010
038600001006     C                   do        *hival
038700120612     c                   eval      *in01 = *off
038800140303     c                   eval      *in15 = *off
038900010315     C                   exsr      Sr_Carvid
039000010315     C                   exsr      Sr_Decodifica
039100090211     c                   ExSr      Sr_Tastifun
039200001010
039300061017     c     for01         tag
039400001031     C                   eval      v1mod = 'VISUALIZZAZIONE'
039500001031
039600001103     C                   setoff                                       010203
039700140304     c                   setoff                                       1715
039800040908     c                   Eval      *In10 = *Off
039900001030     C                   exfmt     fior071
040000001031     C                   setoff                                       28
040100001011
040200001011      * F12=Ritorno
040300001023     C                   if        *inkl = *on
040400001023     C                   leave
040500001023     C                   endif
040600040220
040700040220      *
040800040220      *?F1 - Aggiunta bolla in reclamo?
040900040226if  1c                   if        *inKA  = *on
041000040220     c                   clear                   FIDNA1ds
041100040220     c                   eval      IA1ins = 'S'
041200040220     c                   eval      IA1tor = 'O'
041300040220     c                   eval      IA1ogg = ds_OggORM
041400040226     c                   leave
041500040220e   1c                   endif
041600040226      *
041700040226      *?F16 - Gestione reclami?
041800040226if  2c                   if        *inKQ  = *on
041900040226     c                   reset                   FIDNA2ds
042000040226if  3c                   if        SDSprm > 1
042100040226     c                   eval      IA2mod = 'I'
042200040226e   3c                   endif
042300040226     c                   eval      IA2ogg = ds_OggORM
042400040226     c                   call      'FIDNA2R'
042500040226     c                   parm                    KPJBA
042600040226     c                   parm                    FIDNA2ds
042700040226if  3c                   if        OA2f03 = 'S'
042800040226     c                   leave
042900040226e   3c                   endif
043000040226e   2c                   endif
043100130913
043200130913      /free
043300130913
043400130913       //?F14=Interrogazione oraro di servizio
043500130913       IF  *inkn;
043600130913         exsr F14D01;
043700130913         iter;
043800130913       ENDIF;
043900131028
044000131028       //?F5=Altri dati
044100131028       IF  *inke;
044200131028         exsr F05D01;
044300131028         iter;
044400131028       ENDIF;
044500140110
044600140110       //?F17=Note AUT
044700140110       IF  *inkr;
044800140110         exsr F17D01;
044900140110         iter;
045000140110       ENDIF;
045100130913
045200130913      /end-free
045300001030
045400010402      * F20=Fasi
045500010402     C                   if        *inku = *on
045600001031     C                   exsr      Sr_Fasi
045700001030     C                   endif
045800120612
045900131120      * F15=Variazioni
046000131120     C                   if        *inkp = *on
046100001102     C                   exsr      Sr_Variazioni
046200001030     C                   endif
046300001011
046400001025      * F18=Note
046500001025     C                   if        *inks = *on
046600001025     C                   exsr      Sr_Note
046700001025     C                   endif
046800001020      * F19=Proposte
046900001030     C                   if        *inkt = *on
047000001031     C                   exsr      Sr_Proposte
047100001020     C                   endif
047200061017      * F9=Sollecito
047300061017     c                   if        *inki
047400061017     c                   exsr      sr_sollecito
047500061017     c                   endif
047600061023      * F2=Dati DPD
047700061023     c                   if        *inkb
047800061019     c                   exsr      sr_datidpd
047900061019     c                   endif
048000090211      * F22=Info appuntamento
048100090211     c                   if        *inkw
048200090211     c                   exsr      sr_ormapp
048300090211     c                   endif
048400090210      * F24=Altri tasti  ?
048500090210     c                   if        *inky and $loop > 1
048600090210     c                   exsr      sr_f24
048700090210     c                   goto      for01
048800090210     c                   endif
048900001010
049000001006     C                   enddo
049100001023
049200001103     C     richiama      tag
049300001103
049400010109     C                   if        psce = 'P'
049500010109     C                   if        pric = *blanks
049600001103     C                   exsr      Sr_Proposte
049700010109     C                   endif
049800010109
049900010109     C                   if        pric = 'P'
050000010109     C                   exsr      Sr_Cardatiprop
050100010109     C                   exsr      Sr_Visproposte
050200010109     C                   endif
050300001103     C                   endif
050400001010
050500001006     C                   eval      *inlr = *on
050600001009      **********************************************************************
050700001023      * CARICA DATI NELLA VIDEATA DAL FILE
050800001009      **********************************************************************
050900001009     C     Sr_Carvid     begsr
051000001009
051100001129     C                   movel     ORMtco        v1ctco
051200001016     C                   movel     ORMtor        v1ctor
051300001023      * Inversione data emissione ORM
051400001016     C                   clear                   wlbdat
051500001017     C                   z-add     ORMdao        G02inv
051600001017     C                   movel     '3'           G02err
051700001016     C                   call      'XSRDA8'
051800001016     C                   parm                    wlbdat
051900001025     C                   z-add     G02dat        v1dao
052000001215     C                   movel     ORMoao        v1oao
052100001016      * Dati Ordinante
052200001025     C                   z-add     ORMcor        v1ccor
052300001025     C                   movel     ORMrso        v1rso
052400001025     C                   movel     ORMino        v1ino
052500001025     C                   movel     ORMcao        v1cao
052600001025     C                   movel     ORMloo        v1loo
052700001025     C                   movel     ORMpro        v1pro
052800001025     C                   movel     ORMnao        v1nao
052900001016      * Dati Ritiro a
053000001025     C                   z-add     ORMcra        v1ccra
053100001025     C                   movel     ORMrsr        v1rsr
053200001025     C                   movel     ORMinr        v1inr
053300001025     C                   movel     ORMcar        v1car
053400001025     C                   movel     ORMlor        v1lor
053500001025     C                   movel     ORMprr        v1prr
053600001025     C                   movel     ORMnar        v1nar
053700001016     C                   movel     ORMrer        v1rer
053800001016     C                   movel     ORMter        v1ter
053900001016      * Dati Consegna a
054000001025     C                   z-add     ORMcrc        v1ccrc
054100001016     C                   movel     ORMrsc        v1rsc
054200001016     C                   movel     ORMinc        v1inc
054300001016     C                   movel     ORMloc        v1loc
054400001016     C                   movel     ORMcac        v1cac
054500001016     C                   movel     ORMprc        v1prc
054600001016     C                   movel     ORMnac        v1nac
054700001016
054800010315     C                   if        ORMffd = 'S'
054900010315     C                   eval      v1ffd = 'SI'
055000010315     C                   else
055100010315     C                   eval      v1ffd = 'NO'
055200010315     C                   endif
055300010315
055400001016     C                   movel     ORMnam        v1nam
055500001023      * Inversione data ritiro
055600001017     C                   z-add     ORMdar        G02inv
055700001017     C                   movel     '3'           G02err
055800001016     C                   call      'XSRDA8'
055900001016     C                   parm                    wlbdat
056000001025     C                   z-add     G02dat        v1dar
056100001016     C                   movel     ORMorr        v1orr
056200001016
056300001025     C                   z-add     ORMncl        v1ncl
056400001025     C                   z-add     ORMpkg        v1pkg
056500001025     C                   z-add     ORMvlm        v1vlm
056600001025     C                   z-add     ORMbnc        v1bnc
056700001025     C                   z-add     ORMblc        v1blc
056800001025     C                   z-add     ORMatt        v1att
056900001025     C                   z-add     ORMmtc        v1mtc
057000001016
057100010315     C                   if        ORMpag = 'M'
057200010315     C                   eval      v1pag = 'Mitt.'
057300010315     C                   endif
057400010315     C                   if        ORMpag = 'D'
057500010315     C                   eval      v1pag = 'Dest.'
057600010315     C                   endif
057700010315     C                   if        ORMpag = 'O'
057800010315     C                   eval      v1pag = 'Ord. '
057900010315     C                   endif
058000010315
058100001025     C                   z-add     ORMksc        v1ksc
058200001020     C                   movel     ORMctr        v1ctr
058300010315
058400010315     C                   if        ORMddt = 'S'
058500010315     C                   eval      v1ddt = 'SI'
058600010315     C                   endif
058700010315     C                   if        ORMddt = 'N'
058800010315     C                   eval      v1ddt = 'NO'
058900010315     C                   endif
059000010315
059100001016     C                   movel     ORMpor        v1cpor
059200001016     C                   movel     ORMpoc        v1cpoc
059300001016     C                   movel     ORMno1        v1not1
059400001016     C                   movel     ORMno2        v1not2
059500001025     C                   z-add     ORMfao        v1fao
059600001025     C                   z-add     ORMpos        v1cpos
059700001025     C                   z-add     ORMors        v1ors
059800001027     C                   movel     ORMrfa        v1rfa
059900001221     C                   move      ORMsto        v1sto
060000001030     C                   z-add     ORMndc        v1ndc
060100001030      * Inversione data distinta
060200001030     C                   clear                   wlbdat
060300001030     C                   z-add     ORMddc        G02inv
060400001030     C                   movel     '3'           G02err
060500001030     C                   call      'XSRDA8'
060600001030     C                   parm                    wlbdat
060700001030     C                   z-add     G02dat        v1ddc
060800010226
060900010226      * Cerca codice autotrasportatore
061000071108     c                   if        ormndc > 0
061100001030     C                   z-add     ORMndc        knfv
061200001030     C                   z-add     ORMpor        kfgs
061300120612     C     kfidst        chain     fidst01l
061400060515      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
061500120612     c                   if        not %found(fidst01l)
061600060515     c                   clear                   fnlv55ds
061700060515     c                   eval      d55tpt = '6'
061800060515     c                   move      ormpor        d55lin
061900060515     c                   eval      d55drf = ormddc
062000060515     c                   call      'FNLV55R'
062100060515     c                   parm                    fnlv55ds
062200060515     c                   if        d55err = *blanks and d55tfa <> d55lin
062300060515     c                   move      d55tfa        kfgs
062400120612     c     kfidst        chain     fidst01l
062500060515     c                   endif
062600060515     c                   endif
062700120612     C                   if        %found(fidst01l)
062800001030     C                   eval      v1cpdc = DSTpdr
062900001030     C                   endif
063000071108     c                   else
063100071108     c                   clear                   v1cpdc
063200071108     c                   endif
063300071108
063400010226      * Autotrasportatore evidenziato a video
063500010226     C                   eval      *in06 = (v1cpdc > 0)
063600010226
063700010219      * Distinta evidenziata a video
063800010219     C                   eval      *in05 = (v1ndc > 0)
063900070924
064000070924      * se Stampato faccio vedere la scritta
064100070924     c                   eval      v1dst = ormdst
064200070924     c                   eval      *in16 = (v1dst > 0)
064300010119
064400010315     C                   if        ORMspi = 'S'
064500010315     C                   eval      v1spi = 'SI'
064600010315     C                   else
064700010315     C                   eval      v1spi = 'NO'
064800010315     C                   endif
064900010309      * Causale
065000010305     C                   exsr      Sr_Carcau
065100010322
065200010322     C                   eval      *in40 = *off
065300010322     C                   eval      *in41 = *off
065400010322     C                   eval      *in42 = *off
065500090211     C                   eval      *in43 = *off
065600131211     C                   eval      *in46 = *off
065700140121     C                   eval      *in48 = *off
065800131211
065900010322      * Controllo se ci sono Variazioni
066000010322     C     kfnor         setll     fnorv01l
066100010322     C                   do        *hival
066200010322     C     kfnor         reade     fnorv01l
066300010322     C                   if        %eof
066400010322     C                   leave
066500010322     C                   endif
066600010322     C                   eval      *in40 = *on
066700010322     C                   leave
066800010322     C                   enddo
066900010322      * Controllo se ci sono Proposte
067000010322     C     kfnor         setll     fnorp01l
067100010322     C                   do        *hival
067200010322     C     kfnor         reade     fnorp01l
067300010322     C                   if        %eof
067400010322     C                   leave
067500010322     C                   endif
067600010322     C                   eval      *in41 = *on
067700010322     C                   leave
067800010322     C                   enddo
067900010322      * Controllo se ci sono Note
068000061017     C     kfnor         setll     fnorn02l
068100010322     C                   do        *hival
068200061017     C     kfnor         reade     fnorn02l
068300010322     C                   if        %eof
068400010322     C                   leave
068500010322     C                   endif
068600010322     C                   eval      *in42 = *on
068700010322     C                   leave
068800010322     C                   enddo
068900090211
069000090211      * controllo se inserite info appuntamento
069100090211     c     kfnor         setll     fnora01l
069200090211     c                   do        *hival
069300090211     c     kfnor         reade     fnora01l
069400090211     c                   if        %eof
069500090211     c                   leave
069600090211     c                   endif
069700090211     c                   eval      *in43 = *on
069800090211     c                   leave
069900090211     c                   enddo
070000030606
070100030606     C                   eval      dorm01 = ORMflo
070200030606
070300030606      * ORM commissionato
070400030606     c                   If        §orCom = 'S'
070500030606     c                   Eval      V1dCom = 'COMMISSIONATO'
070600030606     c                   Else
070700030606     c                   Clear                   V1dCom
070800030606     c                   EndIf
070900050502
071000050502      * ORM con Ricevuta di Ritiro
071100050502     c                   Eval      *In11 = (§orsrm = 'S')
071200100225
071300100225      /free
071400140429       //?Imposto costante se ORM con preavviso (CEVA)
071500100225       *in18 = *off;
071600100225       IF  §ormpre <> *blanks;
071700100225         *in18 = *on;
071800100225         IF  §ormpre = 'M';
071900100225         v1este = 'Preavviso: '+
072000100225                  'Mail';
072100100225         ENDIF;
072200100225       ENDIF;
072300100225      /end-free
072400061017
072500061017     c                   eval      v1cfcs = ormfcs
072600070911      * giro
072700070911     c                   clear                   v1ccgi
072800070911     c     kfnor         chain     fnorg01l
072900070911     c                   if        %found(fnorg01l)
073000070911     c                   eval      v1ccgi = orgcgi
073100070911     c                   endif
073200131028
073300131028      /free
073400131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
073500131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
073600131211           *in46 = *on;
073700131211         ENDIF;
073800160513
073900160513         k_OREpoe = ORMpoe;
074000160513         k_OREnsr = ORMnsr;
074100160513         k_OREnor = ORMnor;
074200160513         k_OREnrv = ORMnrv;
074300160513
074400160513       //?Recupero dati da estensione FNORE
074500160513       //?Rcd 'G ' --> Generale
074600160513         k_OREtrc = 'G';
074700160513         clear dOREgen;
074800160513         chain  %kds(k05fnore) FNORE01L;
074900160513         IF  %found(FNORE01L);
075000160513           dOREgen = OREdati;
075100160513         ENDIF;
075200160513
075300131028       //?Recupero dati da estensione FNORE
075400131028       //?Rcd 'O ' --> Orari apertura
075500131028         k_OREtrc = 'O ';
075600131028         clear dOREorari;
075700131028       //?Aggancio il rcd
075800140429         chain  %kds(k05fnore) FNORE01L;
075900131028         IF  %found(FNORE01L);
076000131028           dOREorari = OREdati;
076100131028         ENDIF;
076200131028         v1OraAMda = §OREoramda;
076300131028         v1OraAMa  = §OREorama;
076400131028         v1OraPMda = §OREorapda;
076500131028         v1OraPMa  = §OREorapa;
076600161020
076700161020       //?Rcd 'DT' --> Data Pronta Merce
076800161020         k_OREtrc = 'DT';
076900161108         clear dOREdt;
077000161020         chain  %kds(k05fnore) FNORE01L;
077100161020         IF  %found(FNORE01L);
077200161108           dOREdt = OREdati;
077300161020         ENDIF;
077400161108         IF  §OREdpm > *zeros;
077500161020           clear wlbdat;
077600161108           G02inv = §OREdpm;
077700161020           G02err = '3';
077800161020           xsrda8 (wlbdat);
077900161020           V1dpm = G02dat;
078000161020         ENDIF;
078100140429
078200140429       //?Imposto costante se ORM con Alert via Mail/SMS
078300160513         IF  §OREfimo = 'S' or §OREfiso = 'S';
078400160513           IF  not *in18;
078500160513             V1este = 'Preavviso: Mail/SMS';
078600160513             *in18 = *on;
078700160513           ENDIF;
078800160513         ENDIF;
078900160224
079000160224       //?Recupero NPR
079100160224         k_OREtrc = 'NP';
079200160224         clear OREdati;
079300160224       //?Aggancio il rcd
079400160224         chain  %kds(k05fnore) FNORE01L;
079500160224         IF  %found(FNORE01L);
079600160224           V1npr = OREdati;
079700160224         ENDIF;
079800140121
079900140121       //?Controllo se ci sono Note AUT
080000140121         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
080100140121         IF  %found(FNORT01L);
080200140121           *in48 = *on;
080300140121         ENDIF;
080400131211
080500131028      /end-free
080600001009
080700001009     C                   endsr
080800001030      **********************************************************************
080900001030      * DECODIFICHE
081000001030      **********************************************************************
081100001030     C     Sr_Decodifica begsr
081200001030
081300001030      * Fase
081400001129     C                   clear                   v1dfao
081500001030     C                   clear                   TIBS02DS
081600001030     C                   movel     'C'           T02mod
081700001030     C                   movel     knsif         t02sif
081800001030     C                   movel     'FAR'         t02cod
081900001030     C                   movel(p)  v1fao         T02ke1
082000001030     C                   call      'TIBS02R'
082100001030     C                   parm                    KPJBA
082200001030     C                   parm                    TIBS02DS
082300040908     c                   Eval      dFar = T02Uni
082400040908     c                   Eval      V1dfao = d§fardes
082500001030
082600001030      * Tipo ordine assegnazione
082700001129     C                   clear                   v1dtco
082800001030     C                   clear                   TIBS02DS
082900001030     C                   movel     'C'           T02mod
083000001030     C                   movel     knsif         t02sif
083100001129     C                   movel     'TCO'         t02cod
083200001129     C                   movel(p)  v1ctco        T02ke1
083300001030     C                   call      'TIBS02R'
083400001030     C                   parm                    KPJBA
083500001030     C                   parm                    TIBS02DS
083600001129     C                   movel     t02uni        v1dtco
083700001030
083800001030      * P.O. ritiro
083900061003     c                   eval      *in22 = *off
084000061003     c                   clear                   v1dfcs
084100061003     c                   clear                   og143
084200001129     C                   clear                   v1dpor
084300140206     c                   clear                   network
084400001030     C                   movel     v1cpor        kazorg
084500001030     C     kazorg        chain     azorg01L
084600001030     C                   if        %found(azorg01l)
084700001030     C                   movel     ORGdes        v1dpor
084800140206     C                   movel     orgdit        network
084900061003     c                   eval      og143 = orgde3
085000001030     C                   endif
085100061003      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
085200061003      * europolitan invece ormai è obsoleta
085300061003     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
085400061003      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
085500061003      * partener se caricata e la visualizzo al posto della zona
085600061017     c                   if        v1cfcs <> *blanks
085700061003     c                   clear                   tibs02ds
085800061003     c                   eval      t02mod = 'C'
085900061003     c                   eval      t02sif = knsif
086000061003     c                   eval      t02cod = 'MIP'
086100061017     c                   eval      t02ke1 = v1cfcs
086200061003     c                   call      'TIBS02R'
086300061003     c                   parm                    kpjba
086400061003     c                   parm                    tibs02ds
086500061003     c                   eval      v1dfcs = t02uni
086600061003     c                   eval      *in22 = *on
086700061003     c                   endif
086800061003     c                   endif
086900070911
087000070911      * Giro
087100070911     c                   clear                   v1dcgi
087200070911     c                   clear                   fidg09ds
087300070911     c                   eval      d09iop0 = '001'
087400071127     c                   eval      d09ifgs = v1cpor
087500070911     c                   eval      d09idat = *date
087600070911     c                   eval      d09icgi = v1ccgi
087700070911     c                   eval      d09itug = 'R'
087800080318     c                   movel     kpjbu         §kpjbu
087900070911     c                   eval      kpjbu = fidg09ds
088000070911     c                   call      'FIDG09R'
088100070911     c                   parm                    kpjba
088200070911     c                   eval      fidg09ds = kpjbu
088300080318     c                   movel     §kpjbu        kpjbu
088400070911     c                   if        d09ocgi <> ' '
088500070911     c                   eval      v1dcgi = d09odes
088600070911     c                   endif
088700001030
088800001030      * P.O. Consegna
088900001129     C                   clear                   v1dpoc
089000001030     C                   movel     v1cpoc        kazorg
089100001030     C     kazorg        chain     azorg01L
089200001030     C                   if        %found(azorg01l)
089300001030     C                   movel     ORGdes        v1dpoc
089400001030     C                   endif
089500001030
089600001030      * Tipo ordine
089700001129     C                   clear                   v1dtor
089800001030     C                   clear                   TIBS02DS
089900001030     C                   movel     'C'           T02mod
090000001030     C                   movel     knsif         t02sif
090100001030     C                   movel     'TOR'         t02cod
090200001030     C                   movel(p)  v1ctor        T02ke1
090300001030     C                   call      'TIBS02R'
090400001030     C                   parm                    KPJBA
090500001030     C                   parm                    TIBS02DS
090600001030     C                   movel     t02uni        v1dtor
090700001103
090800010205      * Priorità
090900010205     C                   clear                   v1dsto
091000010205     C                   if        v1sto <> *zeros
091100001103     C                   clear                   TIBS02DS
091200001103     C                   movel     'C'           T02mod
091300001103     C                   movel     knsif         t02sif
091400001103     C                   movel     'STO'         t02cod
091500131029     C                   eval      t02ke1 = %editc(ORMsto:'X')
091600001103     C                   call      'TIBS02R'
091700001103     C                   parm                    KPJBA
091800001103     C                   parm                    TIBS02DS
091900010205     C                   movel     t02uni        v1dsto
092000010205     C                   endif
092100001030
092200010226      * Autotrasportatore
092300001129     C                   clear                   v1dpdc
092400021202     C                   movel     v1cpdc        kpdpdr
092500021202     C     kfapd         chain     fiapd01L
092600021202     C                   if        %found(fiapd01l)
092700001030     C                   movel     APDrsc        v1dpdc
092800001030     C                   endif
092900001031      * ORM fisso
093000001031     C                   eval      *in04 = (v1ors <> *zeros)
093100161108
093200161108      /free
093300161108       //?Se non ho la data pronta merce imposto la data ritiro
093400161108         IF  V1dpm = 0;
093500161108           V1dpm = V1dar;
093600161108         ENDIF;
093700161108      /end-free
093800001030
093900001030     C                   endsr
094000010305      **********************************************************************
094100010305      * Cerca causale chiusura e decodifica
094200010305      **********************************************************************
094300010305     C     Sr_Carcau     begsr
094400010305
094500010305     C                   eval      kpoe = ORMpoe
094600010305     C                   eval      knsr = ORMnsr
094700010305     C                   eval      knor = ORMnor
094800010305     C                   eval      knrv = ORMnrv
094900010305     C                   eval      kdfo = ORMdfo
095000010305     C                   eval      kofo = ORMofo
095100010305     C                   eval      kfao = ORMfao
095200110418     C**!!!kfnorf        chain     fnorf01L
095300110418     C**!!!              if        %found(fnorf01l)
095400110418     c     kfnorf        setll     fnorf01l
095500110418     c                   do        *hival
095600110418     c     kfnorf        reade     fnorf01l
095700110418     c                   if        %eof(fnorf01l)
095800110418     c                   leave
095900110418     c                   endif
096000010305     C                   eval      v1cau = orfcar
096100110418     C                   enddo
096200010305
096300010305     C                   clear                   dcmr
096400010305     C                   clear                   TIBS02DS
096500010305     C                   movel     'C'           T02mod
096600010305     C                   movel     knsif         t02sif
096700010305     C                   movel     'CMR'         t02cod
096800010305     C                   movel(p)  v1cau         T02ke1
096900010305     C                   call      'TIBS02R'
097000010305     C                   parm                    KPJBA
097100010305     C                   parm                    TIBS02DS
097200010305     C                   movel     t02uni        dcmr
097300010305     C                   movel     d§cmrdes      v1dcau
097400010309
097500010309     C                   eval      *in07 = (v1cau <> *blanks)
097600010305
097700010305     C                   endsr
097800001031      **********************************************************************
097900081125      * F20 - Fasi
098000001031      **********************************************************************
098100001031     C     Sr_Fasi       BEGSR
098200001031
098300001031     C                   z-add     1             recsff
098400001031      * Pulisco subfile
098500001031     C                   z-add     *zeros        nrrf              4 0
098600001031     C                   eval      *in21 = *off
098700001031     C                   write     fior07cf
098800001031     C                   eval      *in20 = *on
098900001031     C                   eval      *in21 = *on
099000001031
099100001031      * Carico il subfile in base alle scelte fatte
099200001031     C     kfnor         setll     fnorf01l
099300001031     C                   do        *hival
099400001031     C     kfnor         reade     fnorf01l
099500001031     C                   if        %eof
099600001031     C                   leave
099700001031     C                   endif
099800001031
099900001031     C                   z-add     v1cpoe        v3cpoe
100000001031     C                   z-add     v1nsr         v3nsr
100100001031     C                   z-add     v1nor         v3nor
100200001031     C                   z-add     v1nrv         v3nrv
100300081125
100400081125     c                   eval      dorf01 = orfflo
100500001031
100600001031     C                   exsr      Sr_CarFasi
100700001031     C                   enddo
100800001031      * Se non ho caricato emetto una videata di segnalazione.
100900001031     C                   if        nrrf = *zeros
101000001031     C                   movel     msg(2)        v1cmsg
101100001031     C                   seton                                        28
101200001031     C                   goto      endfasi
101300001031     C                   endif
101400001031
101500001031      * Se ho caricato emetto il subfile
101600001031     C                   if        nrrf > *zeros
101700001102     C                   do        *hival
101800001031     C                   write     fior073
101900001031     C                   exfmt     fior07cf
102000001031      * F12=Ritorno
102100001102     C   kl              leave
102200001102     C                   enddo
102300001102     C                   endif
102400001031
102500001031     C     endfasi       endsr
102600001031      **********************************************************************
102700001031      * CARICO SUBFILE FASI
102800001031      **********************************************************************
102900001031     C     Sr_CarFasi    begsr
103000001031
103100001031      * Inversione data modifica
103200001031     C                   z-add     ORFdae        G02inv
103300001031     C                   movel     '3'           G02err
103400001031     C                   call      'XSRDA8'
103500001031     C                   parm                    wlbdat
103600001031     C                   movel     G02dat        v3dae
103700001031
103800001031     C                   movel     ORFore        v3ore
103900001031     C                   eval      v3cpog = ORFpog
104000001031     C                   eval      v3ute  = ORFpue
104100001031     C                   eval      v3far  = ORFfar
104200001031      * Decodifica fase
104300001031     C                   clear                   TIBS02DS
104400001031     C                   movel     'C'           T02mod
104500001031     C                   movel     knsif         t02sif
104600001031     C                   movel     'FAR'         t02cod
104700001031     C                   movel(p)  v3far         T02ke1
104800001031     C                   call      'TIBS02R'
104900001031     C                   parm                    KPJBA
105000001031     C                   parm                    TIBS02DS
105100040908     C**!!!              movel     t02uni        v3dfar
105200040908     c                   Eval      dFar = T02Uni
105300040908     c                   Eval      V3dfar = d§fardes
105400001031
105500001031     C                   if        ORFcar <> *blanks
105600001031     C                   seton                                        02
105700001031     C                   eval      v3car  = ORFcar
105800001031      * Decodifica causale
105900010301     C                   clear                   dcmr
106000001031     C                   clear                   TIBS02DS
106100001031     C                   movel     'C'           T02mod
106200001031     C                   movel     knsif         t02sif
106300001128     C                   movel     'CMR'         t02cod
106400001031     C                   movel(p)  v3car         T02ke1
106500001031     C                   call      'TIBS02R'
106600001031     C                   parm                    KPJBA
106700001031     C                   parm                    TIBS02DS
106800010301     C                   movel     t02uni        dcmr
106900010301     C                   movel     d§cmrdes      v3dcar
107000001107     C                   else
107100001107     C                   setoff                                       02
107200001107     C                   clear                   v3car
107300001107     C                   clear                   v3dcar
107400001031     C                   endif
107500081125      * visualizzo se 'No addebito'
107600081125     c                   eval      *in17 = *off
107700081125     c                   if        §orfadd = 'S'
107800081125     c                   eval      *in17 = *on
107900081125     c                   endif
108000040908
108100040908      * Se ci sono carico anche i dati della distinta
108200040908if  1c                   If        OrfNdc > *Zeros
108300040908     c                   Eval      *In10 = *On
108400040908     c                   Eval      v3ndc = OrfNdc
108500040908      * giro la data della distinta
108600040908     c                   Clear                   wlbdat
108700040908     c                   Z-add     OrfDdc        G02inv
108800040908     c                   Movel     '3'           G02err
108900040908     c                   Call      'XSRDA8'
109000040908     c                   Parm                    wlbdat
109100040908     c                   Z-add     G02dat        v3ddc
109200040915
109300040908      * cerco il codice dell'autotrasportatore
109400040908     c                   Z-add     OrfNdc        knfv
109500040908     c                   Z-add     OrfFgs        kfgs
109600120612     c     kfidst        Chain     fidst01l
109700060522      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
109800120612     c                   if        not %found(fidst01l)
109900060522     c                   clear                   fnlv55ds
110000060522     c                   eval      d55tpt = '6'
110100060522     c                   move      orffgs        d55lin
110200060522     c                   eval      d55drf = ormddc
110300060522     c                   call      'FNLV55R'
110400060522     c                   parm                    fnlv55ds
110500060522     c                   if        d55err = *blanks and d55tfa <> d55lin
110600060522     c                   move      d55tfa        kfgs
110700120612     c     kfidst        chain     fidst01l
110800060522     c                   endif
110900060522     c                   endif
111000120612     c                   If        %Found(fidst01l)
111100040908     c                   Eval      v3cpdr = DstPdr
111200040908     c                   EndIf
111300040908      * decodifico l'autotrasportatore
111400040908     c                   Clear                   v3dpdr
111500040908     c                   Movel     v3cpdr        kpdpdr
111600040908     c     kfapd         Chain     Fiapd01L
111700040908     c                   If        %Found(Fiapd01l)
111800040908     c                   Eval      v3dpdr = ApdRsc
111900040908     c                   EndIf
112000041013   x1c                   Else
112100041013     c                   Eval      *In10 = *Off
112200041013e   1c                   EndIf
112300001031
112400001031     C                   add       1             nrrf
112500001031     C                   write     fior07sf
112600001031
112700001031     C                   endsr
112800001030      **********************************************************************
112900131120      * F15 - Variazioni
113000001030      **********************************************************************
113100001102     C     Sr_Variazioni BEGSR
113200001030
113300001031     C                   z-add     1             recsf
113400001030      * Pulisco subfile
113500001030     C                   z-add     *zeros        nrr               4 0
113600001030     C                   eval      *in21 = *off
113700001030     C                   write     fior07c
113800001030     C                   eval      *in20 = *on
113900001030     C                   eval      *in21 = *on
114000001030
114100001030      * Carico il subfile in base alle scelte fatte
114200001030     C     kfnor         setll     fnorv01l
114300001030     C                   do        *hival
114400001030     C     kfnor         reade     fnorv01l
114500001030     C                   if        %eof
114600001030     C                   leave
114700001030     C                   endif
114800001030
114900001031     C                   z-add     v1cpoe        v2cpoe
115000001031     C                   z-add     v1nsr         v2nsr
115100001031     C                   z-add     v1nor         v2nor
115200001031     C                   z-add     v1nrv         v2nrv
115300001030
115400001102     C                   exsr      Sr_CarVariaz
115500001030     C                   enddo
115600001030      * Se non ho caricato emetto una videata di segnalazione.
115700001031     C                   if        nrr = *zeros
115800001031     C                   movel     msg(1)        v1cmsg
115900001031     C                   seton                                        28
116000001102     C                   goto      endvariaz
116100001031     C                   endif
116200001030
116300001030      * Se ho caricato emetto il subfile
116400001030     C                   if        nrr > *zeros
116500001031     C                   do        *hival
116600001030     C                   write     fior072
116700001030     C                   exfmt     fior07c
116800001030      * F12=Ritorno
116900001102     C   kl              goto      endvariaz
117000001030
117100001031      * Legge i record scelti
117200001030     C                   do        *hival
117300001030     C                   readc     fior07s
117400001030     C                   if        %eof
117500001030     C                   leave
117600001030     C                   endif
117700001030     C                   if        v2scel = '5'
117800001030     C                   z-add     nrr           recsf
117900001102     C                   exsr      Sr_VisVariaz
118000001030     C                   endif
118100001031     C                   clear                   v2scel
118200001030     C                   update    fior07s
118300001030     C                   enddo
118400001030
118500001030     C                   enddo
118600001031     C                   endif
118700001030
118800001102     C     endvariaz     endsr
118900001030      **********************************************************************
119000001102      * CARICO SUBFILE VARIAZIONI
119100001030      **********************************************************************
119200001102     C     Sr_CarVariaz  begsr
119300001030
119400001030     C                   clear                   v2scel
119500001030     C                   eval      v2cpor = ORVpor
119600001030
119700001030      * Inversione data modifica
119800001103     C                   z-add     ORVdtv        v2dtvd
119900001030     C                   z-add     ORVdtv        G02inv
120000001030     C                   movel     '3'           G02err
120100001030     C                   call      'XSRDA8'
120200001030     C                   parm                    wlbdat
120300001030     C                   movel     G02dat        v2dtv
120400001030
120500001030     C                   movel     ORVorv        v2orv
120600001030     C                   eval      v2utv  = ORVutv
120700001031     C                   eval      v2fao  = ORVfao
120800001031      * Decodifica
120900001031     C                   clear                   TIBS02DS
121000001031     C                   movel     'C'           T02mod
121100001031     C                   movel     knsif         t02sif
121200001031     C                   movel     'FAR'         t02cod
121300001031     C                   movel(p)  v2fao         T02ke1
121400001031     C                   call      'TIBS02R'
121500001031     C                   parm                    KPJBA
121600001031     C                   parm                    TIBS02DS
121700001031     C                   movel     t02uni        v2dfao
121800001030
121900001030     C                   add       1             nrr
122000001030     C                   write     fior07s
122100001030
122200001030     C                   endsr
122300001030      **********************************************************************
122400001102      * VISUALIZZO DATI DEL FILE VARIAZIONI
122500001030      **********************************************************************
122600001102     C     Sr_VisVariaz  begsr
122700001031
122800001031     C                   seton                                        01
122900001030
123000001031     C                   eval      v1mod1 = 'Var.il '
123100001031     C                             + v2dtvg
123200001031     C                             +'/'
123300001031     C                             + v2dtvm
123400001031     C                             + '/'
123500001031     C                             + v2dtva
123600001031     C                             + ' alle '
123700001031     C                             + v2orvo
123800001031     C                             + ':'
123900001031     C                             + v2orvm
124000001031     C                             + ':'
124100001031     C                             + v2orvs
124200001031     C                             + ' da '
124300001031     C                             + v2utv
124400001031
124500001103     C                   eval      kdtv = v2dtvd
124600001103     C                   eval      korv = v2orv
124700001103     C     kforp         chain     fnorv01l
124800001103     C                   if        %found(fnorv01l)
124900001103
125000001102     C                   exsr      Sr_CarVidVar
125100001031     C                   exsr      Sr_Decodifica
125200120612     c                   ExSr      Sr_Tastifun
125300001103     C                   endif
125400001030
125500001102     C                   do        *hival
125600001031     C                   exfmt     fior071
125700001102
125800001102      * F12=Ritorno
125900001102     C   kl              leave
126000131029      /free
126100131029       //?F5=Altri dati
126200131029       IF  *inke;
126300131029         exsr F05D01;
126400131029         iter;
126500131029       ENDIF;
126600131029      /end-free
126700001102
126800001102     C                   enddo
126900001031
127000001030     C                   endsr
127100001031      **********************************************************************
127200001102      * CARICA DATI NELLA VIDEATA DAL FILE VARIAZIONI
127300001031      **********************************************************************
127400001102     C     Sr_CarVidVar  begsr
127500001031
127600001129     C                   movel     ORMtco        v1ctco
127700001031     C                   movel     ORVtor        v1ctor
127800001031      * Inversione data emissione ORM
127900001031     C                   clear                   wlbdat
128000001031     C                   z-add     ORMdao        G02inv
128100001031     C                   movel     '3'           G02err
128200001031     C                   call      'XSRDA8'
128300001031     C                   parm                    wlbdat
128400001031     C                   z-add     G02dat        v1dao
128500010109     C                   movel     ORMoao        v1oao
128600001031      * Dati Ordinante
128700001031     C                   z-add     ORVcor        v1ccor
128800001031     C                   movel     ORVrso        v1rso
128900001031     C                   movel     ORVino        v1ino
129000001031     C                   movel     ORVcao        v1cao
129100001031     C                   movel     ORVloo        v1loo
129200001031     C                   movel     ORVpro        v1pro
129300001031     C                   movel     ORVnao        v1nao
129400001031      * Dati Ritiro a
129500001031     C                   z-add     ORVcra        v1ccra
129600001031     C                   movel     ORVrsr        v1rsr
129700001031     C                   movel     ORVinr        v1inr
129800001031     C                   movel     ORVcar        v1car
129900001031     C                   movel     ORVlor        v1lor
130000001031     C                   movel     ORVprr        v1prr
130100001031     C                   movel     ORVnar        v1nar
130200001031     C                   movel     ORVrer        v1rer
130300001031     C                   movel     ORVter        v1ter
130400001031      * Dati Consegna a
130500001031     C                   z-add     ORVcrc        v1ccrc
130600001031     C                   movel     ORVrsc        v1rsc
130700001031     C                   movel     ORVinc        v1inc
130800001031     C                   movel     ORVloc        v1loc
130900001031     C                   movel     ORVcac        v1cac
131000001031     C                   movel     ORVprc        v1prc
131100001031     C                   movel     ORVnac        v1nac
131200001031
131300010315     C                   if        ORVffd = 'S'
131400010315     C                   eval      v1ffd = 'SI'
131500010315     C                   else
131600010315     C                   eval      v1ffd = 'NO'
131700010315     C                   endif
131800010315
131900001031     C                   movel     ORVnam        v1nam
132000001031      * Inversione data ritiro
132100001031     C                   z-add     ORVdar        G02inv
132200001031     C                   movel     '3'           G02err
132300001031     C                   call      'XSRDA8'
132400001031     C                   parm                    wlbdat
132500001031     C                   z-add     G02dat        v1dar
132600001031     C                   movel     ORVorr        v1orr
132700001031
132800001031     C                   z-add     ORVncl        v1ncl
132900001031     C                   z-add     ORVpkg        v1pkg
133000001031     C                   z-add     ORVvlm        v1vlm
133100001031     C                   z-add     ORVbnc        v1bnc
133200001031     C                   z-add     ORVblc        v1blc
133300001031     C                   z-add     ORVatt        v1att
133400001031     C                   z-add     ORVmtc        v1mtc
133500001031
133600010315     C                   if        ORVpag = 'M'
133700010315     C                   eval      v1pag = 'Mitt.'
133800010315     C                   endif
133900010315     C                   if        ORVpag = 'D'
134000010315     C                   eval      v1pag = 'Dest.'
134100010315     C                   endif
134200010315     C                   if        ORVpag = 'O'
134300010315     C                   eval      v1pag = 'Ord. '
134400010315     C                   endif
134500010315
134600001031     C                   z-add     ORVksc        v1ksc
134700001031     C                   movel     ORVctr        v1ctr
134800010315
134900010315     C                   if        ORVddt = 'S'
135000010315     C                   eval      v1ddt = 'SI'
135100010315     C                   endif
135200010315     C                   if        ORVddt = 'N'
135300010315     C                   eval      v1ddt = 'NO'
135400010315     C                   endif
135500010315
135600001031     C                   movel     ORVpor        v1cpor
135700001031     C                   movel     ORVpoc        v1cpoc
135800001031     C                   movel     ORVno1        v1not1
135900001031     C                   movel     ORVno2        v1not2
136000001031     C                   z-add     ORVfao        v1fao
136100001031     C                   z-add     ORMpos        v1cpos
136200001031     C                   z-add     ORMors        v1ors
136300001031     C                   movel     ORVrfa        v1rfa
136400001221     C                   move      ORVsto        v1sto
136500001031     C                   z-add     ORVndc        v1ndc
136600001031      * Inversione data distinta
136700001031     C                   clear                   wlbdat
136800001031     C                   z-add     ORVddc        G02inv
136900001031     C                   movel     '3'           G02err
137000001031     C                   call      'XSRDA8'
137100001031     C                   parm                    wlbdat
137200001031     C                   z-add     G02dat        v1ddc
137300010226
137400010226      * Cerca codice autotrasportatore
137500071129     c                   clear                   v1cpdc
137600001031     C                   z-add     ORVndc        knfv
137700001031     C                   z-add     ORVpor        kfgs
137800120612     C     kfidst        chain     fidst01l
137900120612     C                   if        %found(fidst01l) and DSTatb = *blanks
138000001031     C                   eval      v1cpdc = DSTpdr
138100001031     C                   endif
138200010119
138300010315     C                   if        ORVspi = 'S'
138400010315     C                   eval      v1spi = 'SI'
138500010315     C                   else
138600010315     C                   eval      v1spi = 'NO'
138700010315     C                   endif
138800030627
138900030627     C                   eval      dorm01 = ORvflo
139000030627
139100030627      * ORM commissionato
139200030627     c                   If        §orCom = 'S'
139300030627     c                   Eval      V1dCom = 'COMMISSIONATO'
139400030627     c                   Else
139500030627     c                   Clear                   V1dCom
139600030627     c                   EndIf
139700131211
139800131211     c                   eval      *in46 = *off
139900131029
140000131029      /free
140100131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
140200131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
140300131211           *in46 = *on;
140400131211         ENDIF;
140500131029       //?Recupero dati da estensione FNORVE
140600131029       //?Rcd 'O ' --> Orari apertura
140700131029         k_ORVEpoe = ORVpoe;
140800131029         k_ORVEnsr = ORVnsr;
140900131029         k_ORVEnor = ORVnor;
141000131029         k_ORVEnrv = ORVnrv;
141100131029         k_ORVEdtv = ORVdtv;
141200131029         k_ORVEorv = ORVorv;
141300131029         k_ORVEtrc = 'O ';
141400131029         clear dOREorari;
141500131029       //?Aggancio il rcd
141600131029         chain(n)  %kds(k07fnorve) FNORVE1L;
141700131029         IF  %found(FNORVE1L);
141800131029           dOREorari = ORVEdati;
141900131029         ENDIF;
142000131029         v1OraAMda = §OREoramda;
142100131029         v1OraAMa  = §OREorama;
142200131029         v1OraPMda = §OREorapda;
142300131029         v1OraPMa  = §OREorapa;
142400170607
142500170607       //?Rcd 'DT' --> Data Pronta Merce
142600170607         k_ORVEtrc = 'DT';
142700170607         clear dOREdt;
142800170607       //?Aggancio il rcd
142900170607         chain(n)  %kds(k07fnorve) FNORVE1L;
143000170607         IF  %found(FNORVE1L);
143100170607           dOREdt = ORVEdati;
143200170607         ENDIF;
143300170607         IF  §OREdpm > *zeros;
143400170607           clear wlbdat;
143500170607           G02inv = §OREdpm;
143600170607           G02err = '3';
143700170607           xsrda8 (wlbdat);
143800170607           V1dpm = G02dat;
143900170607         ENDIF;
144000160513
144100131029      /end-free
144200001031
144300001031     C                   endsr
144400001025      **********************************************************************
144500001025      * F18 - Note
144600001025      **********************************************************************
144700001025     C     Sr_Note       BEGSR
144800001025
144900001025     C                   clear                   fior06DS
145000001025     C                   z-add     v1dao         i06dta
145100001026     C                   z-add     v1cpoe        i06poe
145200001026     C                   z-add     v1nor         i06nor
145300001026     C                   z-add     v1nsr         i06nsr
145400001026     C                   z-add     v1nrv         i06nrv
145500001026     C                   movel     v1fao         i06far
145600001026     C                   eval      i06dai = §doggi
145700001026     C                   TIME                    W0140
145800001026     C                   movel     w0140         I06ori
145900001026     C                   movel     'I'           i06flm
146000061017     c   ki              movel     'V'           i06flm
146100001026
146200001025     C                   CALL      'FIOR06R'
146300001025     C                   PARM                    kpjba
146400001027     C                   parm                    fior06ds
146500001025
146600001025     C                   endsr
146700001031      **********************************************************************
146800001031      * F19 - Proposte
146900001031      **********************************************************************
147000001031     C     Sr_Proposte   BEGSR
147100001031
147200001031     C                   z-add     1             recsfp
147300001031      * Pulisco subfile
147400001031     C                   z-add     *zeros        nrrp              4 0
147500001031     C                   eval      *in21 = *off
147600001031     C                   write     fior07cp
147700001031     C                   eval      *in20 = *on
147800001031     C                   eval      *in21 = *on
147900001031
148000001031      * Carico il subfile in base alle scelte fatte
148100001031     C     kfnor         setll     fnorp01l
148200001031     C                   do        *hival
148300001031     C     kfnor         reade     fnorp01l
148400001031     C                   if        %eof
148500001031     C                   leave
148600001031     C                   endif
148700001031
148800001031     C                   z-add     v1cpoe        v4cpoe
148900001031     C                   z-add     v1nsr         v4nsr
149000001031     C                   z-add     v1nor         v4nor
149100001031     C                   z-add     v1nrv         v4nrv
149200001031
149300001031     C                   exsr      Sr_CarProposte
149400001031     C                   enddo
149500001031      * Se non ho caricato emetto una videata di segnalazione.
149600001106     C                   if        nrrp = *zeros and psce <> 'P'
149700001103     C                   movel     msg(3)        v1cmsg
149800001031     C                   seton                                        28
149900001031     C                   goto      endproposte
150000001031     C                   endif
150100001103
150200001103     C                   if        nrrp = *zeros and psce = 'P'
150300001103     C                   do        *hival
150400001103     C                   exfmt     fior075
150500001103      * F12=Ritorno
150600001103     C   kl              leave
150700001103     C                   enddo
150800001103     C                   endif
150900001031
151000001031      * Se ho caricato emetto il subfile
151100001031     C                   if        nrrp > *zeros
151200001031     C                   do        *hival
151300001031     C                   write     fior074
151400001031     C                   exfmt     fior07cp
151500001031      * F12=Ritorno
151600001031     C   kl              goto      endproposte
151700001031
151800001031      * Legge i record scelti
151900001031     C                   do        *hival
152000001031     C                   readc     fior07sp
152100001031     C                   if        %eof
152200001031     C                   leave
152300001031     C                   endif
152400001031     C                   if        v4scel = '5'
152500001031     C                   z-add     nrrp          recsfp
152600001031     C                   exsr      Sr_VisProposte
152700001031     C                   endif
152800001031     C                   clear                   v4scel
152900001031     C                   update    fior07sp
153000001031     C                   enddo
153100001031
153200001031     C                   enddo
153300001031     C                   endif
153400001031
153500001031     C     endproposte   endsr
153600001031      **********************************************************************
153700001031      * CARICO SUBFILE PROPOSTE
153800001031      **********************************************************************
153900001031     C     Sr_CarPropostebegsr
154000001031
154100001031     C                   clear                   v4scel
154200001031     C                   eval      v4cpor = ORPpoe
154300001031
154400001031      * Inversione data modifica
154500001103     C                   z-add     ORPdtv        v4dtvd
154600001031     C                   z-add     ORPdtv        G02inv
154700001031     C                   movel     '3'           G02err
154800001031     C                   call      'XSRDA8'
154900001031     C                   parm                    wlbdat
155000001031     C                   movel     G02dat        v4dtv
155100001031
155200001031     C                   movel     ORPorv        v4orv
155300001031     C                   eval      v4utv  = ORPutv
155400001031     C                   eval      v4fao  = ORPfao
155500001031      * Decodifica
155600001031     C                   clear                   TIBS02DS
155700001031     C                   movel     'C'           T02mod
155800001031     C                   movel     knsif         t02sif
155900001031     C                   movel     'FAR'         t02cod
156000001031     C                   movel(p)  v4fao         T02ke1
156100001031     C                   call      'TIBS02R'
156200001031     C                   parm                    KPJBA
156300001031     C                   parm                    TIBS02DS
156400001031     C                   movel     t02uni        v4dfao
156500001031      * Analisi Proposta
156600001031     C                   if        ORPfev <> *blanks
156700001031     C                   seton                                        03
156800001031     C                   if        ORPfev = 'E'
156900001031     C                   eval      v4sta = 'EFFETTUATA'
157000001107     C                   eval      v4sta1= 'EFF'
157100001031     C                   endif
157200001031     C                   if        ORPfev = 'R'
157300001031     C                   eval      v4sta = 'RIFIUTATA'
157400001107     C                   eval      v4sta1= 'RIF'
157500001031     C                   endif
157600001031      * Inversione data analisi proposta
157700001031     C                   z-add     ORPdte        G02inv
157800001031     C                   movel     '3'           G02err
157900001031     C                   call      'XSRDA8'
158000001031     C                   parm                    wlbdat
158100001031     C                   movel     G02dat        v4dte
158200001031     C                   movel     ORPore        v4ore
158300001031     C                   eval      v4ute  = ORPute
158400001107     C                   else
158500001107     C                   setoff                                       03
158600001107     C                   clear                   v4sta
158700001107     C                   clear                   v4sta1
158800001107     C                   clear                   v4dte
158900001107     C                   clear                   v4ore
159000001107     C                   clear                   v4ute
159100001031     C                   endif
159200001031
159300001031     C                   add       1             nrrp
159400001031     C                   write     fior07sp
159500001031
159600001031     C                   endsr
159700001031      **********************************************************************
159800001031      * VISUALIZZO DATI DEL FILE PROPOSTE
159900001031      **********************************************************************
160000001031     C     Sr_VisPropostebegsr
160100001031
160200140303     c                   eval      *in15 = *on
160300001031
160400001031     C                   eval      v1mod1 = 'Prop. del '
160500001031     C                             + v4dtvg
160600001031     C                             +'/'
160700001031     C                             + v4dtvm
160800001031     C                             + '/'
160900001031     C                             + v4dtva
161000001031     C                             + ' alle '
161100001031     C                             + v4orvo
161200001031     C                             + ':'
161300001031     C                             + v4orvm
161400001031     C                             + ':'
161500001031     C                             + v4orvs
161600001031     C                             + ' da '
161700001031     C                             + v4utv
161800001031
161900001103     C                   eval      kdtv = v4dtvd
162000001103     C                   eval      korv = v4orv
162100001103     C     kforp         chain     fnorp01l
162200001103     C                   if        %found(fnorp01l)
162300001103
162400001031     C                   exsr      Sr_CarVidPro
162500001031     C                   exsr      Sr_Decodifica
162600120612     c                   ExSr      Sr_Tastifun
162700001103     C                   endif
162800001031
162900001102     C                   do        *hival
163000001031     C                   exfmt     fior071
163100001102
163200001102      * F12=Ritorno
163300001102     C   kl              leave
163400131029      /free
163500131029       //?F5=Altri dati
163600131029       IF  *inke;
163700131029         exsr F05D01;
163800131029         iter;
163900131029       ENDIF;
164000140303
164100140303       //?F17=Note AUT
164200140303       IF  *inkr;
164300140303         exsr F17D01;
164400140303         iter;
164500140303       ENDIF;
164600131029      /end-free
164700001102
164800001102     C                   enddo
164900001031
165000001031     C                   endsr
165100001031      **********************************************************************
165200001031      * CARICA DATI NELLA VIDEATA DAL FILE PROPOSTE
165300001031      **********************************************************************
165400001031     C     Sr_CarVidPro  begsr
165500001031
165600001129     C                   movel     ORMtco        v1ctco
165700001031     C                   movel     ORPtor        v1ctor
165800001031      * Inversione data emissione ORM
165900001031     C                   clear                   wlbdat
166000001031     C                   z-add     ORMdao        G02inv
166100001031     C                   movel     '3'           G02err
166200001031     C                   call      'XSRDA8'
166300001031     C                   parm                    wlbdat
166400001031     C                   z-add     G02dat        v1dao
166500010109     C                   movel     ORMoao        v1oao
166600001031      * Dati Ordinante
166700001031     C                   z-add     ORPcor        v1ccor
166800001031     C                   movel     ORPrso        v1rso
166900001031     C                   movel     ORPino        v1ino
167000001031     C                   movel     ORPcao        v1cao
167100001031     C                   movel     ORPloo        v1loo
167200001031     C                   movel     ORPpro        v1pro
167300001031     C                   movel     ORPnao        v1nao
167400001031      * Dati Ritiro a
167500001031     C                   z-add     ORPcra        v1ccra
167600001031     C                   movel     ORPrsr        v1rsr
167700001031     C                   movel     ORPinr        v1inr
167800001031     C                   movel     ORPcar        v1car
167900001031     C                   movel     ORPlor        v1lor
168000001031     C                   movel     ORPprr        v1prr
168100001031     C                   movel     ORPnar        v1nar
168200001031     C                   movel     ORPrer        v1rer
168300001031     C                   movel     ORPter        v1ter
168400001031      * Dati Consegna a
168500001031     C                   z-add     ORPcrc        v1ccrc
168600001031     C                   movel     ORPrsc        v1rsc
168700001031     C                   movel     ORPinc        v1inc
168800001031     C                   movel     ORPloc        v1loc
168900001031     C                   movel     ORPcac        v1cac
169000001031     C                   movel     ORPprc        v1prc
169100001031     C                   movel     ORPnac        v1nac
169200001031
169300010315     C                   if        ORPffd = 'S'
169400010315     C                   eval      v1ffd = 'SI'
169500010315     C                   else
169600010315     C                   eval      v1ffd = 'NO'
169700010315     C                   endif
169800010315
169900001031     C                   movel     ORPnam        v1nam
170000001031      * Inversione data ritiro
170100001031     C                   z-add     ORPdar        G02inv
170200001031     C                   movel     '3'           G02err
170300001031     C                   call      'XSRDA8'
170400001031     C                   parm                    wlbdat
170500001031     C                   z-add     G02dat        v1dar
170600001031     C                   movel     ORPorr        v1orr
170700001031
170800001031     C                   z-add     ORPncl        v1ncl
170900001031     C                   z-add     ORPpkg        v1pkg
171000001031     C                   z-add     ORPvlm        v1vlm
171100001031     C                   z-add     ORPbnc        v1bnc
171200001031     C                   z-add     ORPblc        v1blc
171300001031     C                   z-add     ORPatt        v1att
171400001031     C                   z-add     ORPmtc        v1mtc
171500010315
171600010315     C                   if        ORPpag = 'M'
171700010315     C                   eval      v1pag = 'Mitt.'
171800010315     C                   endif
171900010315     C                   if        ORPpag = 'D'
172000010315     C                   eval      v1pag = 'Dest.'
172100010315     C                   endif
172200010315     C                   if        ORPpag = 'O'
172300010315     C                   eval      v1pag = 'Ord. '
172400010315     C                   endif
172500001031
172600001031     C                   z-add     ORPksc        v1ksc
172700001031     C                   movel     ORPctr        v1ctr
172800010315
172900010315     C                   if        ORPddt = 'S'
173000010315     C                   eval      v1ddt = 'SI'
173100010315     C                   endif
173200010315     C                   if        ORPddt = 'N'
173300010315     C                   eval      v1ddt = 'NO'
173400010315     C                   endif
173500010315
173600001031     C                   movel     ORPpor        v1cpor
173700001031     C                   movel     ORPpoc        v1cpoc
173800001031     C                   movel     ORPno1        v1not1
173900001031     C                   movel     ORPno2        v1not2
174000001031     C                   z-add     ORPfao        v1fao
174100001031     C                   z-add     ORMpos        v1cpos
174200001031     C                   z-add     ORMors        v1ors
174300001031     C                   movel     ORPrfa        v1rfa
174400001221     C                   move      ORPsto        v1sto
174500001031     C                   z-add     ORPndc        v1ndc
174600001031      * Inversione data distinta
174700001031     C                   clear                   wlbdat
174800001031     C                   z-add     ORPddc        G02inv
174900001031     C                   movel     '3'           G02err
175000001031     C                   call      'XSRDA8'
175100001031     C                   parm                    wlbdat
175200001031     C                   z-add     G02dat        v1ddc
175300010226
175400010226      * Cerca codice autotrasportatore
175500071129     c                   clear                   v1cpdc
175600001031     C                   z-add     ORPndc        knfv
175700001031     C                   z-add     ORPpor        kfgs
175800120612     C     kfidst        chain     fidst01l
175900120612     C                   if        %found(fidst01l) and DSTatb = *blanks
176000001031     C                   eval      v1cpdc = DSTpdr
176100001031     C                   endif
176200010226
176300010315     C                   if        ORPspi = 'S'
176400010315     C                   eval      v1spi = 'SI'
176500010315     C                   else
176600010315     C                   eval      v1spi = 'NO'
176700010315     C                   endif
176800030627
176900030627     C                   eval      dorm01 = ORpflo
177000030627
177100030627      * ORM commissionato
177200030627     c                   If        §orCom = 'S'
177300030627     c                   Eval      V1dCom = 'COMMISSIONATO'
177400030627     c                   Else
177500030627     c                   Clear                   V1dCom
177600030627     c                   EndIf
177700131211
177800131211     c                   eval      *in46 = *off
177900140303     c                   eval      *in48 = *off
178000131029
178100131029      /free
178200131216         IF  V1Ccgi <> *blanks or V1blc > 0 or V1att > 0 or
178300131216             V1mtc > 0 or V1sto > 0 or V1ffd <> 'NO';
178400131211           *in46 = *on;
178500131211         ENDIF;
178600131029       //?Recupero dati da estensione FNORPE
178700131029       //?Rcd 'O ' --> Orari apertura
178800131029         k_ORPEpoe = ORPpoe;
178900131029         k_ORPEnsr = ORPnsr;
179000131029         k_ORPEnor = ORPnor;
179100131029         k_ORPEnrv = ORPnrv;
179200131029         k_ORPEdtv = ORPdtv;
179300131029         k_ORPEorv = ORPorv;
179400131029         k_ORPEtrc = 'O ';
179500131029         clear dOREorari;
179600131029       //?Aggancio il rcd
179700131029         chain(n)  %kds(k07fnorpe) FNORPE1L;
179800131029         IF  %found(FNORPE1L);
179900131029           dOREorari = ORPEdati;
180000131029         ENDIF;
180100131029         v1OraAMda = §OREoramda;
180200131029         v1OraAMa  = §OREorama;
180300131029         v1OraPMda = §OREorapda;
180400131029         v1OraPMa  = §OREorapa;
180500161114       //?Rcd 'DT' --> Data Pronta Merce
180600161114         k_ORPEtrc = 'DT';
180700161114         clear dOREdt;
180800161114       //?Aggancio il rcd
180900161114         chain(n)  %kds(k07fnorpe) FNORPE1L;
181000161114         IF  %found(FNORPE1L);
181100161114           dOREdt = ORPEdati;
181200161114         ENDIF;
181300161114         IF  §OREdpm > *zeros;
181400161114           clear wlbdat;
181500161114           G02inv = §OREdpm;
181600161114           G02err = '3';
181700161114           xsrda8 (wlbdat);
181800161114           V1dpm = G02dat;
181900161114         ENDIF;
182000140303
182100140303       //?Controllo se ci sono proposte Note AUT
182200140303         chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
182300140303         IF  %found(FNORPT1L);
182400140303           *in48 = *on;
182500140303         ENDIF;
182600131029      /end-free
182700001031
182800001031     C                   endsr
182900010109      **********************************************************************
183000010109      * Carico dati dalla kpjba per programma richiamato da interrogazione
183100010109      * proposte
183200010109      **********************************************************************
183300010109     C     Sr_Cardatipropbegsr
183400010109
183500010109     C                   z-add     ppoe          v4cpoe
183600010109     C                   z-add     pnsr          v4nsr
183700010109     C                   z-add     pnor          v4nor
183800010109     C                   z-add     pnrv          v4nrv
183900010109     C                   z-add     pdtv          v4dtv
184000010109     C                   z-add     porv          v4orv
184100010109      * Inverto la data
184200010109     C                   z-add     v4dtv         G02DAT
184300010109     C                   MOVEL     *BLANK        G02ERR
184400010109     C                   CALL      'XSRDA8'
184500010109     C                   PARM                    WLBDAT
184600010109     C                   Z-ADD     G02INV        v4dtvd
184700010109
184800010109     C                   movel     putv          v4utv
184900010109
185000010109     C                   endsr
185100061017      *--------------------------------------------------------------------*
185200061017      * F9 - Sollecito
185300061017      *--------------------------------------------------------------------*
185400061017     c     sr_sollecito  begsr
185500061017
185600061017      * il sollecito è possibile se l'ORM è automatizzato
185700061017      * p.o. emissione DPD
185800061017     c     v1cpoe        lookup    skpodpd                                30
185900061017     c                   if        *in30 and v1ctco <> 'F'
186000061017     c                   eval      *in28 = *on
186100061017     c                   eval      v1cmsg = msg(05)
186200061017     c                   goto      for01
186300061017     c                   endif
186400061017      * p.o. ritiro DPD
186500061017     c     v1cpor        lookup    skpodpd                                30
186600061017     c                   if        *in30 and v1cfcs <> 'F'
186700061017     c                   eval      *in28 = *on
186800061017     c                   eval      v1cmsg = msg(05)
186900061017     c                   goto      for01
187000061017     c                   endif
187100061017      * se orm in fase 900 o 999 non posso sollecitare
187200061017     c                   if        v1fao > 600
187300061017     c                   eval      *in28 = *on
187400061017     c                   eval      v1cmsg = msg(06)
187500061017     c                   goto      for01
187600061017     c                   endif
187700061018      * il sollecito è possibile se l'ORM è già stato stampato
187800080505     c     v1cpor        lookup    skpodpd                                30
187900080505     c                   if        *in30 and v1cfcs = 'F' and
188000080505     c                             v1dst = *zeros
188100061018     c                   eval      *in28 = *on
188200061018     c                   eval      v1cmsg = msg(04)
188300061018     c                   goto      for01
188400061018     c                   endif
188500061017
188600061017      * p.o. ritiro DPD
188700061017     c     v1cpor        lookup    skpodpd                                30
188800061017      * devo richiamare il pgm di stampa per mandare il sollecito
188900061017      * via fax o mail
189000061017     c                   if        *in30
189100090211      * prima di mandare il fax o la mail verifico se il depot è associato ad un cod.cliente
189200090211     c                   exsr      sr_depot
189300090211      * se non associato errore e non si fa il sollecito
189400090211     c                   if        odepksc = *zeros
189500090211     c                   eval      *in28 = *on
189600090211     c                   eval      v1cmsg = msg(07)
189700171027     c                   eval      %subst(v1cmsg:32:7) = OSID4DDPT
189800090211     c                   goto      for01
189900090211     c                   endif
190000061017     c                   exsr      sr_stampa
190100061017     c                   endif
190200061017
190300061017      * scrivo in automatico la nota per avvenuto sollecito
190400061017     c                   eval      ornpoe = v1cpoe
190500061017     c                   eval      ornnsr = v1nsr
190600061017     c                   eval      ornnor = v1nor
190700061017     c                   eval      ornnrv = v1nrv
190800061017     c                   eval      ornfar = v1fao
190900061017     c                   clear                   wprg
191000061017      * cerco ultimo progressivo relativo alla stessa fase
191100061017     c     kfnorn        setll     fnorn02l
191200061017     c                   do        *hival
191300061017     c     kfnorn        reade     fnorn02l
191400061017     c                   if        %eof(fnorn02l)
191500061017     c                   leave
191600061017     c                   endif
191700061017     c                   eval      wprg = ornprg
191800061017     c                   enddo
191900061017      * scrivo la nota
192000061017     c                   move      data_oggi     ds_data
192100061017     c                   add       1             wprg
192200061017     c                   clear                   fnorn000
192300061017     c                   eval      ornpoe = v1cpoe
192400061017     c                   eval      ornnsr = v1nsr
192500061017     c                   eval      ornnor = v1nor
192600061017     c                   eval      ornnrv = v1nrv
192700061017     c                   eval      ornfar = v1fao
192800061017     c                   eval      orndai = dateu
192900061017     c                   time                    ornori
193000061017     c                   eval      ornprg = wprg
193100061017     c                   eval      ornpue = knmus
193200150309     c                   eval      ornpon = DUTpou
193300061017     c                   eval      ornsns = 'I'
193400061017     c                   eval      ornnot = '** SOLLECITATO IL ' +
193500061017     c                             %editc(ds_giorno:'1') + '/' +
193600061017     c                             %editc(ds_mese:'1') + '/' +
193700061017     c                             %editc(ds_anno:'Z') + ' **'
193800061017     c                   eval      ornftr = 'S'
193900061017     c                   eval      orndtr = dateu
194000061017     c                   write     fnorn000
194100061017
194200061017      * richiamo il pgm di gestione note
194300061017     c                   exsr      sr_note
194400061017
194500061017     c                   endsr
194600090211
194700090211      *--------------------------------------------------------------------*
194800090211      * Controllo il depot
194900090211      *--------------------------------------------------------------------*
195000090211     c     sr_depot      begsr
195100090211
195200171025     c                   clear                   tisid4ds
195300171025     c                   eval      isid4dri = ormdar
195600171025     c                   eval      isid4nzd = ormnar
195700171025     c                   eval      isid4cad = ormcar
195800090211     c                   if        ormpkg > §3ilmp
195900171025     c                   eval      isid4sorc = 101
196000090211     c                   else
196100171025     c                   eval      isid4sorc = 136
196200090211     c                   endif
196300171025     c                   eval      isid4lnp = ormpoe
196400171025     c                   call      'TISID4R'
196500171025     c                   parm                    tisid4ds
196600090211      * con il depot richiamo pgm per avere il codice cliente
196700090211      * cliente
196800090211     c                   clear                   truldepds
196801171025     c                   clear                   truldepds1
196900171027     c                   eval      idepdp7 = OSID4DDPT
197000171027     c                   eval      idepdrf = ormdar
197100171027     c                   call      'TRULDEPR'
197200090211     c                   parm                    truldepds
197201171027     c                   parm                    truldepds1
197300090211
197400090211     c                   endsr
197500061017
197600061017      *--------------------------------------------------------------------*
197700061017      * Richiama il pgm di stampa per sollecito
197800061017      *--------------------------------------------------------------------*
197900061017     c     sr_stampa     begsr
198000061017
198100061017     c                   clear                   fior16ds
198200061017     c                   clear                   trul90ds
198300061017     c                   movel     'S'           d90rso
198400061017     c                   call      'TRUL90R'
198500061017     c                   parm                    kpjba
198600061017     c                   Parm                    trul90ds
198700061017      * F3=Fine
198800061017     c                   if        d90f3 = '1'
198900061017     c                   goto      endsrstampa
199000061017     c                   endif
199100090202      /free
199200090202         reset ds5P;
199300090202         if  %subst(knmus : 1 : 3) = 'EDP';
199400090202           TBLkey = 'O' + 'EDP';
199500090202         else;
199600090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
199700090202         endif;
199800090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
199900090202         if  %found( TABEL00F );
200000090202           ds5P = TBLuni;
200100090202         endif;
200200090202      /end-free
200300061017     c                   movel     d90mdo        r16mdo
200400061017     c                   movel     d90pro        r16oqo
200500061017     c                   eval      r16tla = *blanks
200600061017      * è sempre sollecito imposto il nuovo tipo
200700061017     c                   eval      r16ris ='X'
200800061017     c                   eval      r16poe = v1cpoe
200900061017     c                   eval      r16nsr = v1nsr
201000061017     c                   eval      r16nor = v1nor
201100061017     c                   eval      r16nrv = v1nrv
201200061017     c                   eval      r16por = v1cpor
201300061017     c                   eval      r16dst = ormdar
201400061017     c                   movel     kpjbu         §kpjbu
201500061017     c                   movel(p)  fior16ds      kpjbu
201600090202     c                   call      §5Ppgm
201700061017     c                   parm                    kpjba
201800080318     c                   movel     kpjbu         fior16ds
201900061017     c                   movel     §kpjbu        kpjbu
202000061017
202100061017     c     endsrstampa   endsr
202200061019
202300061019      *--------------------------------------------------------------------*
202400061023      * F2 - Dati DPD
202500061019      *--------------------------------------------------------------------*
202600061019     c     sr_datidpd    begsr
202700080430
202800090202      * Salvataggio KPJBU
202900090202     c                   eval      §kpjbu = kpjbu
203000090202
203100080430     c                   clear                   fieu55ds
203200080422
203300080430      * se ORM commissionato da DPD passo il n. ORM DPD
203400080430     c     ormpoe        lookup    skpodpd                                30
203500080430     c                   if        *in30 and %subst(ormrfa:1:3) = 'DPD'
203600080430     c                   eval      i55dep = %subst(ormrfa:4:4)
203700080430     c                   eval      i55nor = %dec(%subst(ormrfa:8:6):6:0)
203800081015     c                   eval      i55ie = 'I'
203900080430     c                   eval      kpjbu = fieu55ds
204000080430     c                   call      'FIEU55C'
204100080430     c                   parm                    kpjba
204200080430     c                   endif
204300080430      * se ORM commissionato a DPD passo il n. ORM Bartolini
204400080430     c     ormpor        lookup    skpodpd                                30
204500080430     c                   if        *in30
204600061019     c                   eval      i55poe = v1cpoe
204700061019     c                   eval      i55nrs = v1nsr
204800061019     c                   eval      i55nro = v1nor
204900061019     c                   eval      i55nrv = v1nrv
205000081015     c                   eval      i55ie = 'E'
205100061019     c                   eval      kpjbu = fieu55ds
205200061206     c                   call      'FIEU55C'
205300061019     c                   parm                    kpjba
205400080430     c                   endif
205500090202
205600090202      * Ripristino KPJBU
205700090202     c                   eval      kpjbu = §kpjbu
205800061019
205900061019     c                   endsr
206000090211
206100090211      *--------------------------------------------------------------------*
206200090211      * Window per dati appuntamento ORM
206300090211      *--------------------------------------------------------------------*
206400090211     c     sr_ormapp     begsr
206500090223
206600090223      * pulisco il subfile
206700090223     c                   clear                   nrra              4 0
206800090223     c                   eval      *in21 = *off
206900090223     c                   write     fior07apc
207000090223     c                   eval      *in20 = *on
207100090223     c                   eval      *in21 = *on
207200090223
207300090223      * carico il subfile
207400090216      * metto fissa la fase 100, per ora ho l'appuntamento solo su questa fase
207500090225     c                   eval      *in31 = *off
207600090216     c                   eval      kfar = 100
207700090223     c     kfnora        setll     fnora01l
207800090223     c                   do        *hival
207900090223     c     kfnora        reade     fnora01l
208000090223     c                   if        %eof(fnora01l)
208100090225     c                   eval      *in31 = *on
208200090223     c                   leave
208300090223     c                   endif
208400090211     c                   eval      wappref = oraref
208500090211     c                   eval      wappnota1 = %subst(oranote:1:35)
208600090211     c                   eval      wappnota2 = %subst(oranote:36:35)
208700090223     c                   eval      wappora = oraora
208800090223     c                   eval      wappute = oraute
208900090223     c                   eval      g02inv = oradata
209000090223     c                   eval      g02err = '3'
209100090223     c                   call      'XSRDA8'
209200090223     c                   parm                    wlbdat
209300090223     c                   eval      wappdata = g02dat
209400090223     c                   add       1             nrra
209500090223     c                   write     fior07aps
209600090223     c                   enddo
209700090211
209800090223      * se non ho caricato nessun appuntamento emetto una segnalazione
209900090223     c                   if        nrra = *zeros
210000090223     c                   movel     msg(8)        v1cmsg
210100090223     c                   eval      *in28 = *on
210200090223     c                   leavesr
210300090223     c                   endif
210400090223
210500090223      * se ho caricato emetto il subfile
210600090223     c                   if        nrra > *zeros
210700090225     c                   eval      recsfa = 1
210800090223     c                   do        *hival
210900090223     c                   write     fior07apz
211000090223     c                   exfmt     fior07apc
211100090223      * f12-ritorno
211200090223     c                   if        *inkl
211300090223     c                   leave
211400090223     c                   endif
211500090211     c                   enddo
211600090223     c                   endif
211700090211
211800090211     c                   endsr
211900130913
212000130913      /free
212100131028       //--------------------------------------------------------------------
212200131028       //?F5 - Altri dati.
212300131028       //--------------------------------------------------------------------
212400131028       BEGSR F05D01;
212500131028
212600131028         wEndF05 = *off;
212700131028
212800131028       //?emetto la videata relativa agli altri dati
212900131028         write fior071;
213000131028       //?emetto la window dove richiedo gli altri dati
213100131028         DOW not wEndF05;
213200131028           exfmt fior0712;
213300131028       //?F12-Ritorno
213400131028           IF  *inkl;
213500131028             wEndF05 = *on;
213600131028           ENDIF;
213700131028         ENDDO;
213800131028
213900131028       ENDSR;
214000140110
214100130913       //--------------------------------------------------------------------
214200130913       //?F14 - Interrogazione orari di servizio.
214300130913       //--------------------------------------------------------------------
214400130913       BEGSR F14D01;
214500140206
214600140317       //?Devo trovare la pickup per passarla al trulorsr
214700140206         clear TISI95DS;
214800140206         I95tcn = '3';
214900140206         I95cap = ORMcar;
215000140206         I95nar = ORMnar;
215100140206         I95ind = ORMinr;
215200140206         I95loc = ORMlor;
215300140206         I95prv = ORMprr;
215400140206       //?Peso in base al ntwk fil.ritiro
215500140206         IF  network = 'SDI';
215600140206           I95lkg = 1000;
215700140206         ELSE;
215800140206           clear I95lkg;
215900140206         ENDIF;
216000140206         tisi95r (tisi95ds);
216100140317
216200140317       //?Cerco eventuale ora presunta ritiro
216300140318         clear fior01ds;
216400140317         IF  ORMndc > 0 and ORMfao < 600;
216500140318           IOR01fgs = ORMpor;
216600140318           IOR01ndc = ORMndc;
216700140318           IOR01poe = ORMpoe;
216800140318           IOR01nsr = ORMnsr;
216900140318           IOR01nor = ORMnor;
217000140318           IOR01nrv = ORMnrv;
217100140318           fior01r (fior01ds);
217200140317         ENDIF;
217300130913
217400140317         clear trulorsds;
217500130913         IOREfil = ORMpor;
217600130913         IOREcap = ORMcar;
217700130913         IOREloc = ORMlor;
217800130913         IOREdta = ORMdar;
217900130913         IOREtser = 'R';
218000140206         IOREgf2 = O95gf2;
218100140318         IOREorp = OOR01ora;
218200161115
218300161115       //?Se peso inferiore a 5 kg passo flag
218400161115         clear dIORE01;
218500161115         IF  ORMrmp <> *blanks;
218600161115           §IOREr_mnx = 'S';
218700161115         ENDIF;
218800161115         IOREflo = dIORE01;
218900130913
219000140317         trulorsr (kpjba:trulorsds);
219100130913
219200130913       ENDSR;
219300140110
219400140110       //--------------------------------------------------------------------
219500140110       //?F17 - Note AUT.
219600140110       //--------------------------------------------------------------------
219700140110       BEGSR F17D01;
219800140121
219900140121         *in48 = *off;
220000140110
220100140110         clear fiort1ds;
220200140303         IORT1mod = 'G';
220300140110         IORT1poe = ORMpoe;
220400140110         IORT1nsr = ORMnsr;
220500140110         IORT1nor = ORMnor;
220600140110         IORT1nrv = ORMnrv;
220700140110         IORT1pgm = 'FIOR07R';
220800140303         IF  *in15;
220900140303           IORT1mod = 'I';
221000140303           IORT1dim = V4Dtvd;
221100140303           IORT1oim = V4orv;
221200140303         ENDIF;
221300140114         §kpjbu = kpjbu;
221400140110
221500140320         IF  psce = 'P' and *in15;
221600140320           fiort1c (kpjba:fiort1ds);
221700140320         ELSE;
221800140110         fiort1r (kpjba:fiort1ds);
221900140320         ENDIF;
222000140114
222100140114         kpjbu = §kpjbu;
222200140121
222300140121       //?Controllo se sono state immesse Note AUT
222400140304         IF  not *in15;
222500140304           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORT01L;
222600140304           IF  %found(FNORT01L);
222700140304             *in48 = *on;
222800140304           ENDIF;
222900140304         ENDIF;
223000140304       //?Controllo se ci sono proposte Note AUT
223100140304         IF  *in15;
223200140304           chain (ORPpoe:ORPnsr:ORPnor:ORPnrv:ORPdtv:ORPorv) FNORPT1L;
223300140304           IF  %found(FNORPT1L);
223400140304             *in48 = *on;
223500140304           ENDIF;
223600140304         ENDIF;
223700140110
223800140110       ENDSR;
223900130913
224000130913      /end-free
224100130913
224200090210
224300090210      *------------------------------------------------------------------------*
224400090210      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
224500090210      *------------------------------------------------------------------------*
224600090210     c     sr_f24        begsr
224700090210
224800090210     c                   Select
224900090210
225000090210     c                   When      $loop = 2 and $rig =2  or
225100090210     c                             $loop = 3 and $rig =3
225200090210     c                   Movea     rigatf1       vzfd01
225300090210     c                   Z-add     1             $rig
225400090210
225500090210     c                   If        $loop = 2
225600090210     c                   Eval      vzfd02= cf2412
225700090210     c                   Else
225800090210     c                   Eval      vzfd02= cf2413
225900090210     c                   EndIf
226000090210
226100090210     c                   When      $loop = 2 and $rig =1 or
226200090210     c                             $loop = 3 and $rig =1
226300090210     c                   Movea     rigatf2       vzfd01
226400090210     c                   Z-add     2             $rig
226500090210
226600090210     c                   If        $loop = 2
226700090210     c                   Eval      vzfd02 = cf2422
226800090210     c                   Else
226900090210     c                   Eval      vzfd02 = cf2423
227000090210     c                   EndIf
227100090210
227200090210     c                   When      $loop = 3 and $rig =2
227300090210     c                   Movea     rigatf3       vzfd01
227400090210     c                   Z-add     3             $rig
227500090210     c                   Eval      vzfd02 = cf2433
227600090210     c                   EndSl
227700090210
227800090210     c                   EndSr
227900090210
228000090210      *------------------------------------------------------------------------*
228100090210      *  Imposto descrizione tasti funzione
228200090210      *------------------------------------------------------------------------*
228300090210     c     Sr_Tastifun   BegSr
228400140304
228500140304      * Abilito eventuali tasti funzionali
228600140304     c                   exsr      Abil_Fxx
228700090210
228800090210      * Conta i caratteri riempiti dalle RigaTf1
228900090210     c                   Clear                   $z
229000090210      * Conta i caratteri riempiti dalle RigaTf2
229100090210     c                   Clear                   $k
229200090210      * Conta i caratteri riempiti dalle RigaTf3
229300090210     c                   Clear                   $j
229400090210      * Conta la posizione del campone da cui partire per impostare
229500090210      * la descrizione del tasto funzione
229600090210     c                   Clear                   $y
229700090210
229800090210      * Stringhe che contengono le descrizioni dei tasti funzione
229900090210     c                   Reset                   rigatf1
230000090210     c                   Reset                   rigatf2
230100090210     c                   Reset                   rigatf3
230200090217
230300131028      * F1 - Aggiungi Richiesta Assistenza
230400131028     c                   if        *in08
230500131028     c                   reset                   $tf
230600131028     c                   movea     tf01          $tf
230700131028     c                   exsr      rie_$tf
230800131028     c                   endif
230900090217      * F2 - Dati DPD
231000090217     c                   if        not *in01 and *in14 and
231100140304     c                             not *in12 and not *in15
231200090217     c                   reset                   $tf
231300090217     c                   movea     tf02          $tf
231400090217     c                   exsr      rie_$tf
231500090217     c                   endif
231600090217      * F9 - Sollecito
231700090217     c                   if        not *in01 and *in13
231800140304     c                             and not *in15
231900090217     c                   reset                   $tf
232000090217     c                   movea     tf09          $tf
232100090217     c                   exsr      rie_$tf
232200090217     c                   endif
232300130913      * F14 - Orari servizio
232400131108     c                   if        not *in01
232500131125     c                             and *in44
232600140304     c                             and not *in15
232700130913     c                   reset                   $tf
232800130913     c                   movea     tf14          $tf
232900130913     c                   exsr      rie_$tf
233000130913     c                   endif
233100131120      * F15 - Variazioni
233200131120     c                   if        not *in01 and *in40
233300140304     c                             and not *in15
233400131120     c                   reset                   $tf
233500131120     c                   movea     tf15          $tf
233600131120     c                   exsr      rie_$tf
233700131120     c                   endif
233800131028      * F16 - Gestione Richieste assistenza
233900131108     c                   if        not *in01 and *in09
234000140304     c                             and not *in15
234100131028     c                   reset                   $tf
234200131028     c                   movea     tf16          $tf
234300131028     c                   exsr      rie_$tf
234400131028     c                   endif
234500090217      * F19 - Proposte Variazioni
234600090217     c                   if        not *in01 and *in41
234700140304     c                             and not *in15
234800090217     c                   reset                   $tf
234900090217     c                   movea     tf19          $tf
235000090217     c                   exsr      rie_$tf
235100090217     c                   endif
235200090217      * F22 - Info Appuntamento
235300090217     c                   if        not *in01 and *in43
235400140304     c                             and not *in15
235500090217     c                   reset                   $tf
235600090217     c                   movea     tf22          $tf
235700090217     c                   exsr      rie_$tf
235800090217     c                   endif
235900090211      * F20 - Fasi
236000090211     c                   if        not *in01
236100140304     c                             and not *in15
236200090210     c                   reset                   $tf
236300090211     c                   movea     tf20          $tf
236400090210     c                   exsr      rie_$tf
236500090210     c                   endif
236600090210
236700090210      * Pulisco la stringa
236800090210     c                   Eval      $h = 1
236900090210     c                   For       $h by 1 to 62
237000090210     c                   If        rigatf1($h) = '#'
237100090210     c                   Clear                   rigatf1($h)
237200090210     c                   EndIf
237300090210     c                   EndFor
237400090210     c                   Eval      $h = 1
237500090210     c                   For       $h by 1 to 62
237600090210     c                   If        rigatf2($h) = '#'
237700090210     c                   Clear                   rigatf2($h)
237800090210     c                   EndIf
237900090210     c                   EndFor
238000090210     c                   Eval      $h = 1
238100090210     c                   For       $h by 1 to 62
238200090210     c                   If        rigatf3($h) = '#'
238300090210     c                   Clear                   rigatf3($h)
238400090210     c                   EndIf
238500090210     c                   EndFor
238600090210
238700090210      * Imposto la descrizione del tasto funzione F24 e segnalo
238800090210      * quante righe ho riempito e quale devo emettere
238900090210     c                   Select
239000090210
239100090210     c                   When      $k <> *Zeros and $y <> *Zeros
239200090210     c                   Eval      $loop = 3
239300090210     c                   Eval      vzfd02 = cf2413
239400090210
239500090210     c                   When      $k <> *Zeros and $y = *Zeros
239600090210     c                   Eval      $loop = 2
239700090210     c                   Eval      vzfd02 = cf2412
239800090210
239900090210     c                   Other
240000090210
240100090210     c                   Eval      $loop = 1
240200090210     c                   Clear                   vzfd02
240300090210     c                   EndSl
240400090210
240500090210      * Prima riga di tasti funzione
240600090210     c                   Movea     rigatf1       vzfd01
240700090210     c                   Eval      $rig = 1
240800090210
240900090210     c                   EndSr
241000090210
241100090210      *------------------------------------------------------------------------*
241200090210      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
241300090210      *------------------------------------------------------------------------*
241400090210     c     Rie_$tf       BegSr
241500090210      *
241600090210     c                   Eval      $x = 1
241700090210     c     '#'           Lookup    $tf($x)                                30
241800090210     c                   Add       $x            $z
241900090210     c                   If        $Z <= 63
242000090210     c                   Eval      $j = $Z - $x + 1
242100090210     c                   Movea     $tf           rigatf1($j)
242200090210     c                   Else
242300090210     c                   Add       $x            $k
242400090210     c                   If        $k <= 63
242500090210     c                   Eval      $j = $K - $x + 1
242600090210     c                   Movea     $tf           rigatf2($j)
242700090210     c                   Else
242800090210     c                   Add       $x            $y
242900090210     c                   If        $y <= 63
243000090210     c                   Eval      $j = $y - $x + 1
243100090210     c                   Movea     $tf           rigatf3($j)
243200090210     c                   EndIf
243300090210     c                   EndIf
243400090210     c                   EndIf
243500090210
243600090210     c                   EndSr
243700061017
243800001010      *****************************************************************
243900001010      * ROUTINE INIZIALE
244000001010      *****************************************************************
244100001010     C     *INZSR        BEGSR
244200001009
244300001010     C     *ENTRY        PLIST
244400001010     C                   PARM                    KPJBA
244500040220     c                   parm                    FIDNA1ds
244600001011
244700001011     C                   movel     TIT_A         VTCtit
244800001009
244900001026     C                   movel(p)  kpjbu         parm01
245000001016
245100090202     C                   Z-ADD     1             CODUT             1 0
245200090202     c     *dtaara       define    §azute        AZUTEds
245300090202     c     *dtaara       define    §datiute      dDATIUTE
245400090202     c                   in(E)     *dtaara
245500090202     c                   if        %ERROR or RSUT = *blanks
245600090202     c                   clear                   Tibs34Ds
245700090202     c                   call      'TIBS34R'
245800090202     c                   parm                    Tibs34Ds
245900090202     c                   in        *dtaara
246000090202     c                   endif
246100151001
246200151001      * abilitazione utente
246300151001     c                   clear                   wabi
246400151001     c                   eval      dUTE01 = UTEfaf
246500151001     c                   SELECT
246600151001      * carica abilitazioni del profilo
246700151001     c                   WHEN      §UTEorm <> *blanks
246800151001     c                   eval      wabi = §UTEorm
246900151001     c                   WHEN      UTEaut <> *blanks
247000151001     c                   eval      wabi = UTEaut
247100151001      * se non c'è l'abilitazione
247200151001      * --> se 1° livello, abilitazioni al terminal
247300151001      *     se 2° livello, abilitazioni al punto operativo
247400151001      *     se sede no abilitazioni
247500151001     c                   OTHER
247600151001     c                   IF        DUTlpo = '1'
247700151001     c                   eval      wabi   = 'TP'
247800151001     c                   ENDIF
247900151001     c                   IF        DUTlpo = '2'
248000151001     c                   eval      wabi   = 'PO'
248100151001     c                   ENDIF
248200151001     c                   ENDSL
248300151001
248400151001      * Reperimento delle filiali gestibili dall'utente
248500151001     c                   clear                   TRUL31DS
248600151001     c                   eval      I31abi = wabi
248700151001     c                   eval      I31cdi = DUTdis
248800151001     c                   eval      I31car = DUTare
248900151001     c                   eval      I31cpo = DUTpou
249000151001     c                   call      'TRUL31R'
249100151001     c                   parm                    KPJBA
249200151001     c                   parm                    TRUL31DS
249300151001     c                   IF        O31pog > *zeros
249400151001     c                   movea     O31pog        SKpog
249500151001     c                   ENDIF
249600041001
249700050112      * Se ambiente prova FILTRAPRD
249800050112     c                   If        %subst(knsif:7:1) = 'P'
249900050112     c                   Eval      wlib = 'FILTRAPRD '
250000050112     c                   Else
250100050112      * Se ambiente buono FILTRA201
250200050112     c                   Eval      wlib = 'FILTRA201 '
250300041001     c                   EndIf
250400010226
250500150309      * Da sede
250600150309      * apro il file delle distinte dell'ambiente di filiale
250700010226
250800010226     C                   if        simfel = 0
250900010226     C                   eval      *in12 = *on
251000041001     c                   Clear                   comman
251100041001     c                   Movel(p)  cmdt(1)       comman
251200041001     c                   Eval      %Subst(comman:30:10) = wlib
251300041001     c                   Eval      comman =
251400120612     c                             %trim(comman) + '/FIDST01L)'
251500041001     c                   Call      'QCMDEXC'                            99
251600041001     c                   Parm                    comman
251700041001     c                   Parm                    lenght
251800120612     c  n99              Open      fidst01l
251900041001
252000010226     C                   else
252100010226     C                   eval      *in12 = *off
252200120612     C                   open      fidst01l
252300010226     C                   endif
252400061017
252500061206      * carico sk dei p.o. DPD
252600061017     c                   clear                   xx
252700061017     c     *loval        setll     azorg01l
252800061017     c                   do        *hival
252900061017     c                   read      azorg01l
253000061017     c                   if        %eof(azorg01l)
253100061017     c                   leave
253200061017     c                   endif
253300061017     c                   if        orgfva <> *blanks  or
253400061017     c                             (orgfag <> 'F' and orgfag <> 'A')
253500061017     c                   iter
253600061017     c                   endif
253700061017     c                   eval      og143 = orgde3
253800061017     c                   if        §ogntw = 'DPD'
253900061017     c                   add       1             xx
254000061017     c                   eval      skpodpd(xx) = orgfil
254100061017     c                   endif
254200061017     c                   enddo
254300090211
254400090211      * carico tabella DPD
254500090211     c                   clear                   ds3idp
254600090211     c                   movel     '3I'          tblcod
254700090211     c                   movel(p)  'DPD'         tblkey
254800090211     c     ktab          chain     tabel00f
254900090211     c                   if        %found(tabel00f) and tblflg = *blanks
255000090211     c                   eval      ds3idp = tbluni
255100090211     c                   endif
255200001009
255300001023      * reperisce data e ora
255400001010     C                   TIME                    W0140
255500001010      * UDATE IN GGMMAAAA
255600001010     C                   MOVE      W0140         WDTGIO
255700001010      * UDATE IN AAAAMMGG
255800001010     C                   Z-ADD     WDTGIO        G02DAT
255900001010     C                   MOVEL     *BLANK        G02ERR
256000001010     C                   CALL      'XSRDA8'
256100001010     C                   PARM                    WLBDAT
256200001025     C                   Z-ADD     G02INV        DATEU
256300001010     C     *iso          MOVEL     DATEU         DATA_oggi
256400001010
256500001025     C                   z-add     dateu         §doggi
256600001016
256700001009     C     Kfnor         klist
256800001011     C                   kfld                    kpoe
256900001016     C                   kfld                    knsr
257000001009     C                   kfld                    knor
257100001016     C                   kfld                    knrv
257200001103
257300001103     C     Kforp         klist
257400001103     C                   kfld                    kpoe
257500001103     C                   kfld                    knsr
257600001103     C                   kfld                    knor
257700001103     C                   kfld                    knrv
257800001103     C                   kfld                    kdtv
257900001103     C                   kfld                    korv
258000010305
258100010305     C     Kfnorf        klist
258200010305     C                   kfld                    kpoe
258300010305     C                   kfld                    knsr
258400010305     C                   kfld                    knor
258500010305     C                   kfld                    knrv
258600010305     C                   kfld                    kdfo
258700010305     C                   kfld                    kofo
258800010305     C                   kfld                    kfao
258900090216
259000090216     c     kfnora        klist
259100090216     c                   kfld                    kpoe
259200090216     c                   kfld                    knsr
259300090216     c                   kfld                    knor
259400090216     c                   kfld                    knrv
259500090216     c                   kfld                    kfar
259600010119
259700010119     C     Ktab          klist
259800010119     C                   kfld                    codut
259900010119     C                   kfld                    kcod
260000010119     C                   kfld                    kkey
260100001030
260200120612     C     Kfidst        klist
260300120612     c                   kfld                    knpg
260400001030     C                   kfld                    knfv
260500001030     C                   kfld                    kfgs
260600021202
260700021202     C     Kfapd         klist
260800021202     C                   kfld                    kpdtip
260900021202     C                   kfld                    kpdpdr
261000061017
261100061017     c     kfnorn        klist
261200061017     c                   kfld                    ornpoe
261300061017     c                   kfld                    ornnsr
261400061017     c                   kfld                    ornnor
261500061017     c                   kfld                    ornnrv
261600061017     c                   kfld                    ornfar
261700061017
261800150526      * File FITGD32L
261900150526     c     K02TGD32      klist
262000040220     c                   kfld                    TGDtor
262100040220     c                   kfld                    TGDogg
262200001009
262300001011     C                   eval      kpoe = ppoe
262400001016     C                   eval      knsr = pnsr
262500001011     C                   eval      knor = pnor
262600001016     C                   eval      knrv = pnrv
262700001011
262800001025     C                   eval      v1cpoe = ppoe
262900001016     C                   eval      v1nsr  = pnsr
263000001025     C                   eval      v1nor  = pnor
263100001025     C                   eval      v1nrv  = pnrv
263200001115
263300001115     C                   movel     *zeros        v1ccra
263400001115     C                   movel     *zeros        v1ccor
263500001115     C                   movel     *zeros        v1ccrc
263600001006
263700001006     C                   endsr
263800040220      *****************************************************************
263900040220      * Abilitazione tasti funzionali
264000040220      *****************************************************************
264100040220     c     Abil_Fxx      begsr
264200040220
264300140304      * abilito i tasti funzione seguenti solo se non sono in
264400140304      * Interrogazione Storico Variazioni o Interrogazione Proposte Variazione
264500140304     c                   IF        not *in01 and not *in15
264600140304
264700040220      *
264800040220      *?Abilito F1=Aggiunta in Reclami?
264900040220      * (se richiamato da pgm. FIDNA1R...)
265000040303     c                   eval      *in08  =    (SDSprm >  1
265100040309     c                                      and IA1ins = ' ')
265200040220      *
265300040220      *?Abilito F16=Visualizzazione Reclami?
265400040220      * (se rilevo dei reclami già immessi)
265500040220     c                   eval      TGDtor  = 'O'
265600040220     c                   eval      TGDogg  = ds_OggORM
265700150526     c     K02TGD32      setll     FITGD000
265800150526     c                   eval      *in09   = (%equal(FITGD32L)   and
265900040303     c                                           (SDSprm =  1
266000040303     c                                        or (SDSprm >  1
266100040303     c                                        and IA1ins <> 'N')))
266200070509
266300151002      /free
266400151002       //?Abilito F9=Sollecito
266500151002       //?Se non è utente di sede
266600151002       //?se utente INT
266700151002       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
266800151002         IF  %subst(knmus:1:3) = 'INT' and DUTpou <> 046;
266900151002           xx = 1;
267000151002           FOR xx by 1 to %elem(skpog);
267100151002             IF  skpog(xx) <> *zeros and
267200151002                 %lookup(%dec(skpog(xx):3:0):skpoDPD) > 0;
267300151002               *in13 = *on;
267400151002               leave;
267500151002             ENDIF;
267600151002           ENDFOR;
267700151002         ENDIF;
267800151002      /end-free
267900061019
268000061023      *?Abilito F2=Dati DPD
268100080430      * se non provengo già dal pgm fieu55r
268200061024     c                   if        psce <> 'D'
268300061019     c     ormpoe        lookup    skpodpd                                30
268400061019     c  n30ormpor        lookup    skpodpd                                30
268500140304     c                   eval      *in14 = (*in30 and not *in12)
268600061024     c                   endif
268700140304     c                   ENDIF
268800140121
268900140121      /free
269000140121         *in44 = *off;
269100140526       //?F14 - Orari servizio
269200140304       //?Abilito solo se NON sono in
269300140304       //?Interrogazione Storico Variazioni
269400140304       //?Interrogazione Proposte Variazioni
269500140304         IF  not *in01 and not *in15;
269600140317           *in44 = *on;
269700140304         ENDIF;
269800140304
269900140121      /end-free
270000040220
270100040220     c                   EndSR
270200001010      *---------------------------------------------------------------------------------------------
270300041001** cmdt
270400120612OVRDBF FILE(FIDST01L) TOFILE(
270500001031** MSG  Lungh. 78                                                            *
270600001102Non esistono variazioni per l'ORM selezionato                                       1
270700001102Non esistono fasi per l'ORM selezionato                                             2
270800001102Non esistono proposte per l'ORM selezionato                                         3
270900061017L'ORM non è stato stampato impossibile sollecitare                                  4
271000061017Impossibile fare sollecito per ORM non automatizzato                                5
271100061017Impossibile fare sollecito per ORM già fatto o chiuso                               6
271200090217Impossibile sollecitare. Depot xxxx non associato ad un cliente                     7
271300090223Non esistono appuntamenti per l'ORM selezionato                                     8
271400131028** TF01
271500131211F1=Agg.Rich.  #
271600090211** TF02
271700131211F2=Dati DPD  #
271800090211** TF09
271900131211F9=Sollecito  #
272000130913** TF14
272100131211F14=OrariSr  #
272200131120** TF15
272300131211F15=Variazioni  #
272400131028** TF16
272500131211F16=Gest.Rich.  #
272600140110** TF19
272700131211F19=Proposte Var.  #
272800090217** TF20
272900131211F20=Fasi  #
273000090217** TF22
273100131211F22=Info App.  #
